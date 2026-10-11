###############################################################################
## lean-tools.R
##
## Optional formal verification with Lean 4. The Lean library `SaomNK` ships in
## inst/lean; these helpers locate it, check that a Lean toolchain is usable,
## build it, and audit the axioms of what was built. Nothing here is needed to
## simulate or estimate a model, and every function degrades to an informative
## message when Lean is not installed.
###############################################################################

## The three axioms every Lean/Mathlib proof may use. Anything else (sorryAx,
## Lean.ofReduceBool from native_decide, a user axiom) is reported.
.LEAN_STANDARD_AXIOMS <- c("propext", "Classical.choice", "Quot.sound")

.lean_exe <- function(name) {
  ext <- if (.Platform$OS.type == "windows") ".exe" else ""
  hit <- Sys.which(name)
  if (nzchar(hit)) return(unname(hit))
  home <- Sys.getenv("ELAN_HOME", file.path(path.expand("~"), ".elan"))
  cand <- c(file.path(home, "bin", paste0(name, ext)),
            file.path(Sys.getenv("USERPROFILE"), ".elan", "bin", paste0(name, ext)))
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (length(cand)) normalizePath(cand[1], winslash = "/") else ""
}

#' Locate the Lean project shipped with searchnet
#'
#' Resolves the directory holding the `SaomNK` Lean library (the package's
#' `inst/lean`). Resolution order: the option `searchnet.lean_home`, the
#' environment variable `SEARCHNET_LEAN_HOME`, the installed package's `lean/`
#' directory, then a search upward from the working directory for
#' `inst/lean/lakefile.toml` (a source checkout).
#'
#' An installed package directory may not be writable, and `lake build` writes
#' its output next to the sources. [lean_setup()] can copy the project to a
#' writable location and point `searchnet.lean_home` at it.
#'
#' @return Path to the Lean project, or `NA_character_` if none is found.
#' @seealso [lean_available()], [lean_setup()]
#' @examples
#' lean_home()
#' @export
lean_home <- function() {
  ok <- function(d) !is.null(d) && length(d) == 1L && !is.na(d) && nzchar(d) &&
    file.exists(file.path(d, "lakefile.toml"))
  cand <- list(getOption("searchnet.lean_home"),
               Sys.getenv("SEARCHNET_LEAN_HOME", ""),
               tryCatch(system.file("lean", package = "searchnet"), error = function(e) ""))
  for (d in cand) if (ok(d)) return(normalizePath(d, winslash = "/"))
  d <- normalizePath(getwd(), winslash = "/")
  for (k in 0:6) {
    if (ok(file.path(d, "inst", "lean"))) return(normalizePath(file.path(d, "inst", "lean"), winslash = "/"))
    if (ok(d)) return(d)
    up <- dirname(d)
    if (identical(up, d)) break
    d <- up
  }
  NA_character_
}

#' Is a usable Lean toolchain available?
#'
#' Returns `TRUE` when `lake` (from elan) can be found and the Lean project is
#' present. With `check_build = TRUE` it also requires that the `SaomNK`
#' library has been built (its `.olean` files exist), which is what
#' [lean_check()] needs to check an exported model without first building the
#' library.
#'
#' Lean is optional. Without it, every `lean_*` function that needs it returns
#' early with a message saying what to install.
#'
#' @param check_build Logical; also require a built library.
#' @return Logical scalar, with attribute `reason` when `FALSE`.
#' @export
#' @examples
#' lean_available()
lean_available <- function(check_build = FALSE) {
  no <- function(why) structure(FALSE, reason = why)
  if (!nzchar(.lean_exe("lake")))
    return(no("lake not found: install elan (https://github.com/leanprover/elan)"))
  home <- lean_home()
  if (is.na(home)) return(no("Lean project (inst/lean) not found; see lean_home()"))
  if (check_build) {
    olean <- file.path(home, ".lake", "build", "lib", "lean", "SaomNK.olean")
    if (!file.exists(olean)) return(no("SaomNK not built; run lean_setup() or `lake build`"))
  }
  TRUE
}

.lean_skip_message <- function(what) {
  a <- lean_available()
  message(what, " skipped: ", attr(a, "reason") %||% "Lean is not available", ".")
  invisible(NULL)
}

## Run a command in `wd`, with elan's bin directory on PATH. Uses processx when
## installed (timeouts, separate streams), system2 otherwise.
.lean_run <- function(cmd, args, wd, timeout = Inf) {
  exe <- .lean_exe(cmd)
  if (!nzchar(exe)) stop(cmd, " not found", call. = FALSE)
  binpath <- dirname(exe)
  sep <- if (.Platform$OS.type == "windows") ";" else ":"
  env_path <- paste(binpath, Sys.getenv("PATH"), sep = sep)
  t0 <- Sys.time()
  if (requireNamespace("processx", quietly = TRUE)) {
    res <- processx::run(exe, args, wd = wd, error_on_status = FALSE,
                         timeout = if (is.finite(timeout)) timeout else NULL,
                         env = c("current", PATH = env_path))
    out <- paste(res$stdout, res$stderr, sep = "\n")
    status <- res$status
    if (isTRUE(res$timeout)) status <- NA_integer_
  } else {
    old <- setwd(wd); on.exit(setwd(old), add = TRUE)
    old_path <- Sys.getenv("PATH"); Sys.setenv(PATH = env_path)
    on.exit(Sys.setenv(PATH = old_path), add = TRUE)
    out <- suppressWarnings(system2(exe, args, stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status") %||% 0L
    out <- paste(out, collapse = "\n")
  }
  list(status = status, output = out,
       seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

#' Set up the Lean library: print, and on confirmation run, the build steps
#'
#' Prints the commands that build the `SaomNK` Lean library and runs nothing
#' unless the session is interactive and you confirm. Mathlib is large (about
#' 7 GB unpacked, more on file systems with large clusters), so when a built
#' Mathlib checkout at the pinned revision already exists on the machine, pass
#' its `.lake/packages` directory as `packages_dir`: `lean_setup()` then writes
#' `.lake/package-overrides.json` in the Lean project, pointing every
#' dependency at that checkout as a path dependency, and no second copy is
#' downloaded. That file is machine-specific and ignored by git.
#'
#' With an override in place, do not run `lake update` or `lake exe cache get`
#' in the project: Lake runs no git command on a path dependency, but those
#' commands would try to move the pins.
#'
#' @param packages_dir Optional path to an existing `.lake/packages` directory
#'   built with the same Lean toolchain and Mathlib revision (see the project's
#'   `lake-manifest.json`).
#' @param copy_to Optional writable directory to copy the Lean project to
#'   (useful when the installed package is read-only). On success the option
#'   `searchnet.lean_home` is set to it for this session.
#' @param run Logical; ask before running the commands. Default:
#'   `interactive()`. When `FALSE`, only prints.
#' @return Invisibly, the character vector of commands.
#' @examples
#' ## Print the build commands without running them
#' if (!is.na(lean_home())) lean_setup(run = FALSE)
#' @export
lean_setup <- function(packages_dir = NULL, copy_to = NULL, run = interactive()) {
  home <- lean_home()
  if (is.na(home)) stop("Lean project not found; see ?lean_home", call. = FALSE)
  if (!is.null(copy_to)) {
    dir.create(copy_to, recursive = TRUE, showWarnings = FALSE)
    srcs <- list.files(home, recursive = TRUE, all.files = FALSE, full.names = FALSE)
    srcs <- srcs[!grepl("^\\.lake/", srcs)]
    for (f in srcs) {
      dir.create(dirname(file.path(copy_to, f)), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(home, f), file.path(copy_to, f), overwrite = TRUE)
    }
    home <- normalizePath(copy_to, winslash = "/")
    options(searchnet.lean_home = home)
    message("Copied the Lean project to ", home, " (option searchnet.lean_home set).")
  }
  cmds <- character(0)
  if (!is.null(packages_dir)) {
    packages_dir <- normalizePath(packages_dir, winslash = "/", mustWork = TRUE)
    cmds <- c(cmds, sprintf("# write %s/.lake/package-overrides.json -> %s", home, packages_dir))
  } else {
    cmds <- c(cmds, "lake exe cache get   # downloads prebuilt Mathlib (~0.5 GB, ~7 GB unpacked)")
  }
  cmds <- c(cmds, "lake build           # builds SaomNK (minutes with the Mathlib cache)")
  cat("Lean project: ", home, "\n", "Commands, run in that directory:\n",
      paste0("  ", cmds, collapse = "\n"), "\n", sep = "")
  if (!nzchar(.lean_exe("lake"))) {
    cat("lake was not found. Install elan first: https://github.com/leanprover/elan\n")
    return(invisible(cmds))
  }
  if (!isTRUE(run)) return(invisible(cmds))
  if (!isTRUE(utils::askYesNo("Run these commands now?", default = FALSE)))
    return(invisible(cmds))
  if (!is.null(packages_dir)) .lean_write_overrides(home, packages_dir)
  else print(.lean_run("lake", c("exe", "cache", "get"), home)$status)
  res <- .lean_run("lake", "build", home)
  cat(utils::tail(strsplit(res$output, "\n")[[1]], 5), sep = "\n")
  invisible(cmds)
}

## Write .lake/package-overrides.json pointing every package listed in the
## manifest at `packages_dir/<name>` as a path dependency.
.lean_write_overrides <- function(home, packages_dir) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required", call. = FALSE)
  man <- jsonlite::fromJSON(file.path(home, "lake-manifest.json"), simplifyVector = FALSE)
  pk <- lapply(man$packages, function(p) {
    d <- file.path(packages_dir, p$name)
    if (!dir.exists(d)) stop("missing package directory: ", d, call. = FALSE)
    list(type = "path", name = p$name, scope = p$scope, inherited = p$inherited,
         manifestFile = p$manifestFile, configFile = p$configFile,
         dir = normalizePath(d, winslash = "/"))
  })
  dir.create(file.path(home, ".lake"), showWarnings = FALSE)
  out <- file.path(home, ".lake", "package-overrides.json")
  writeLines(jsonlite::toJSON(list(schemaVersion = man$version, packages = pk),
                              auto_unbox = TRUE, pretty = TRUE), out)
  invisible(out)
}

## Parse Lean output: errors, `sorry` warnings, and `#print axioms` lines.
.lean_parse <- function(output, file = NA_character_) {
  lines <- strsplit(output, "\r?\n")[[1]]
  ax_re <- "^'([^']+)' (depends on axioms: \\[([^]]*)\\]|does not depend on any axioms)"
  hits <- regmatches(lines, regexec(ax_re, lines))
  rows <- lapply(hits[lengths(hits) > 0], function(h) {
    ax <- if (nzchar(h[4])) trimws(strsplit(h[4], ",")[[1]]) else character(0)
    status <- if ("sorryAx" %in% ax) "sorry"
      else if (any(grepl("^Lean\\.ofReduce", ax))) "native"
      else if (all(ax %in% .LEAN_STANDARD_AXIOMS)) "ok" else "axiom"
    data.frame(file = file, decl = h[2], status = status,
               axioms = paste(ax, collapse = ", "), stringsAsFactors = FALSE)
  })
  df <- if (length(rows)) do.call(rbind, rows) else
    data.frame(file = character(0), decl = character(0), status = character(0),
               axioms = character(0), stringsAsFactors = FALSE)
  errs <- grep("(^|: )error(:| )", lines, value = TRUE)
  attr(df, "errors") <- errs
  df
}

#' Check exported models (or the library) with Lean
#'
#' Type-checks every `Instance_*.lean` file in `dir` against the built
#' `SaomNK` library, and audits the axioms of every declaration in them (the
#' generated files end with `#print axioms` lines). With `dir = NULL`, builds
#' the library itself (`lake build`) and runs the library-wide audit file
#' `Axioms.lean`.
#'
#' A declaration passes (`status == "ok"`) when it depends on no axioms beyond
#' `propext`, `Classical.choice` and `Quot.sound`. `"sorry"` marks an
#' incomplete proof, `"native"` a fact proved by `native_decide` (trusting the
#' compiler, `Lean.ofReduceBool`), `"axiom"` anything else. A file that fails
#' to compile is reported with `status == "error"` and its error lines.
#'
#' @param dir Directory holding exported `Instance_*.lean` files, or `NULL` to
#'   build and audit the library.
#' @param files Optional explicit file paths (overrides `dir`).
#' @param timeout Seconds per file (default 1800).
#' @return A data.frame with columns `file`, `decl`, `status`, `axioms`, and
#'   attributes `errors` (character) and `seconds`.
#' @examples
#' \donttest{
#' ## Needs elan/lake and a built SaomNK library; otherwise it
#' ## returns NULL with a message saying what is missing.
#' mod <- list(M = 2, N = 3, E = diag(3),
#'             theta = c(density = -0.5, inPop = 0.2))
#' f <- lean_export_model(mod, dir = tempdir())
#' res <- lean_check(files = f)
#' }
#' @export
lean_check <- function(dir = NULL, files = NULL, timeout = 1800) {
  if (!isTRUE(lean_available())) return(.lean_skip_message("lean_check()"))
  home <- lean_home()
  if (is.null(dir) && is.null(files)) {
    b <- .lean_run("lake", "build", home, timeout = timeout)
    if (!identical(b$status, 0L)) {
      df <- .lean_parse(b$output, "SaomNK")
      attr(df, "errors") <- c(attr(df, "errors"), "lake build failed")
      return(df)
    }
    files <- file.path(home, "Axioms.lean")
  } else if (is.null(files)) {
    files <- list.files(dir, pattern = "^Instance_.*\\.lean$", full.names = TRUE)
    if (!length(files)) stop("no Instance_*.lean files in ", dir, call. = FALSE)
  }
  if (!isTRUE(lean_available(check_build = TRUE)))
    return(.lean_skip_message("lean_check() (library not built)"))
  out <- list(); errs <- character(0); secs <- 0
  for (f in files) {
    r <- .lean_run("lake", c("env", "lean", normalizePath(f, winslash = "/")), home,
                   timeout = timeout)
    secs <- secs + r$seconds
    df <- .lean_parse(r$output, basename(f))
    if (!identical(r$status, 0L)) {
      e <- attr(df, "errors")
      if (!length(e)) e <- if (is.na(r$status)) "timeout" else "lean exited with an error"
      errs <- c(errs, paste0(basename(f), ": ", e))
      df <- rbind(df, data.frame(file = basename(f), decl = NA_character_, status = "error",
                                 axioms = NA_character_, stringsAsFactors = FALSE))
    }
    out[[f]] <- df
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  attr(res, "errors") <- errs
  attr(res, "seconds") <- secs
  res
}

#' Registry of formal results
#'
#' Reads `registry.yml` from the Lean project: one entry per machine-checked
#' declaration, with its module, a plain statement, the step identifiers of the
#' package's proof table it underwrites, the testthat files that exercise the
#' same claim numerically, and the R functions whose behavior it concerns.
#'
#' @return A data.frame with columns `decl`, `module`, `kind`, `statement`,
#'   `proof_table`, `tests`, `r_functions` (list columns collapsed to
#'   `"; "`-separated strings).
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE) && !is.na(lean_home())) {
#'   reg <- lean_registry()
#'   head(reg[, c("decl", "module", "proof_table")])
#' }
#' @export
lean_registry <- function() {
  if (!requireNamespace("yaml", quietly = TRUE))
    stop("lean_registry() needs the 'yaml' package", call. = FALSE)
  home <- lean_home()
  if (is.na(home)) stop("Lean project not found; see ?lean_home", call. = FALSE)
  reg <- yaml::read_yaml(file.path(home, "registry.yml"))
  col <- function(x) if (is.null(x)) NA_character_ else paste(unlist(x), collapse = "; ")
  do.call(rbind, lapply(reg$results, function(e) data.frame(
    decl = e$decl, module = e$module, kind = col(e$kind), statement = col(e$statement),
    proof_table = col(e$proof_table), tests = col(e$tests),
    r_functions = col(e$r_functions), stringsAsFactors = FALSE)))
}

#' Expect that Lean verified a declaration
#'
#' A testthat expectation for checked results: `result` is the data.frame
#' returned by [lean_check()]; the expectation passes when `decl` appears in
#' it with status `"ok"` (only the standard axioms).
#'
#' @param result Output of [lean_check()].
#' @param decl Declaration name, fully qualified or a unique suffix.
#' @examples
#' ## A lean_check() result, written out by hand for illustration
#' res <- data.frame(file = "Instance_demo.lean",
#'                   decl = "SaomNK.Demo.exact_potential",
#'                   status = "ok", axioms = "propext, Quot.sound")
#' if (requireNamespace("testthat", quietly = TRUE))
#'   testthat::test_that("the exported potential is verified",
#'                       expect_lean_theorem(res, "exact_potential"))
#' @export
expect_lean_theorem <- function(result, decl) {
  if (!requireNamespace("testthat", quietly = TRUE)) stop("testthat is required", call. = FALSE)
  hit <- result[!is.na(result$decl) &
                  (result$decl == decl | endsWith(result$decl, paste0(".", decl))), , drop = FALSE]
  testthat::expect(nrow(hit) >= 1L, sprintf("declaration '%s' not found in Lean output", decl))
  if (nrow(hit) >= 1L)
    testthat::expect(all(hit$status == "ok"),
                     sprintf("'%s' has status %s (axioms: %s)", decl,
                             paste(hit$status, collapse = ","), paste(hit$axioms, collapse = " | ")))
  invisible(result)
}

#' List the declarations of the Lean library
#'
#' Scans the `.lean` sources of the `SaomNK` library and returns every
#' `theorem` and `lemma` with its fully qualified name (namespaces resolved).
#' Used to generate the library-wide axiom audit (`Axioms.lean`) and to count
#' results.
#'
#' @param home Lean project directory (default [lean_home()]).
#' @param kinds Declaration keywords to include (default theorems and lemmas;
#'   add `"def"` for definitions).
#' @return A data.frame with columns `decl`, `kind`, `module`.
#' @examples
#' if (!is.na(lean_home())) {
#'   d <- lean_declarations()
#'   table(d$kind)
#' }
#' @export
lean_declarations <- function(home = lean_home(), kinds = c("theorem", "lemma")) {
  src <- list.files(file.path(home, "SaomNK"), pattern = "\\.lean$", recursive = TRUE,
                    full.names = TRUE)
  rows <- list()
  for (f in sort(src)) {
    mod <- paste0("SaomNK.", gsub("/", ".", sub("\\.lean$", "",
                  sub(paste0("^", file.path(home, "SaomNK"), "/"), "", f, fixed = FALSE))))
    ns <- character(0)
    for (ln in readLines(f, warn = FALSE, encoding = "UTF-8")) {
      if (grepl("^namespace ", ln)) ns <- c(ns, sub("^namespace ([^ ]+).*$", "\\1", ln))
      else if (grepl("^end [A-Za-z]", ln) && length(ns) &&
               sub("^end ([^ ]+).*$", "\\1", ln) == ns[length(ns)]) ns <- ns[-length(ns)]
      m <- regmatches(ln, regexec("^(@\\[[^]]*\\] )?(theorem|lemma|def) ([^ (:{]+)", ln))[[1]]
      if (length(m) && m[3] %in% kinds) {
        name <- m[4]
        full <- if (length(ns)) paste(c(ns, name), collapse = ".") else name
        rows[[length(rows) + 1L]] <- data.frame(decl = full, kind = m[3], module = mod,
                                                stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, rows)
}

## Write the library-wide axiom audit file from the sources.
.lean_write_axioms_file <- function(home = lean_home()) {
  d <- lean_declarations(home)
  body <- c("-- Axiom audit of every theorem and lemma in the SaomNK library.",
            "-- Generated by searchnet:::.lean_write_axioms_file(); do not edit by hand.",
            "-- Run after `lake build`:  lake env lean Axioms.lean",
            "-- Expected: every line reports a subset of [propext, Classical.choice, Quot.sound].",
            "import SaomNK", "",
            paste0("#print axioms ", d$decl))
  writeLines(body, file.path(home, "Axioms.lean"), useBytes = TRUE)
  invisible(nrow(d))
}
