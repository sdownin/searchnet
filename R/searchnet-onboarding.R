#' @title Onboarding helpers: setup check, batch classroom submission, presets
#' @description
#' Functions that take a workshop participant or instructor from a fresh
#' install to a first result: \code{\link{searchnet_check_setup}} diagnoses
#' the installation, \code{\link{searchnet_classroom_submit_batch}} submits a
#' whole round of student decisions from a form-export CSV, and
#' \code{\link{searchnet_validate_preset}} checks a custom classroom preset.
#' @name searchnet-onboarding
NULL


# ---------------------------------------------------------------------------- #
#  searchnet_check_setup
# ---------------------------------------------------------------------------- #

## Parse a DESCRIPTION dependency field into a data frame (package, minimum).
.parse_dep_field <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(data.frame(package = character(), minimum = character(),
                      stringsAsFactors = FALSE))
  }
  parts <- trimws(strsplit(gsub("[[:space:]]+", " ", x), ",")[[1]])
  parts <- parts[nzchar(parts)]
  pkg <- trimws(sub("[[:space:]]*\\(.*$", "", parts))
  minv <- ifelse(grepl(">=", parts),
                 trimws(sub("^.*>=[[:space:]]*([^)[:space:]]+).*$", "\\1", parts)),
                 NA_character_)
  data.frame(package = pkg, minimum = minv, stringsAsFactors = FALSE)
}

## Installed version of a package as a string, or NA when it is not installed.
.installed_version <- function(pkg) {
  tryCatch(as.character(utils::packageVersion(pkg)),
           error = function(e) NA_character_)
}

## The base packages that ship with every R installation.
.base_pkgs <- c("stats", "tools", "utils", "grDevices", "graphics", "grid",
                "methods", "parallel", "splines", "tcltk", "compiler")


## The smoke run: a tiny seeded simulation and its {K}-4 plot, drawn to a PNG
## in tempdir() that is deleted afterwards. Returns TRUE or throws.
.setup_smoke_run <- function() {
  env <- saomnk_env(M = 4, N = 6, density = 0.3, seed = 42,
                    name = "setup_check")
  model <- saomnk_model(density = -0.5, popularity = 0.2,
                        influence_matrix = saomnk_block_diagonal(6, 2),
                        influence_weight = 0.3)
  utils::capture.output(suppressMessages(
    saomnk_run(env, model, steps_per_actor = 10, seed = 12345)))
  png_file <- tempfile("searchnet_check_", fileext = ".png")
  on.exit(unlink(png_file), add = TRUE)
  grDevices::png(png_file, width = 800, height = 600)
  dev <- grDevices::dev.cur()
  on.exit(if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev),
          add = TRUE, after = FALSE)
  p <- suppressMessages(saomnk_plot_k4(env))
  if (inherits(p, c("ggplot", "gtable", "grob"))) print(p)
  grDevices::dev.off(dev)
  if (!file.exists(png_file) || file.size(png_file) == 0) {
    stop("the {K}-4 plot produced no image")
  }
  TRUE
}


#' Check That searchnet Is Ready to Use
#'
#' Diagnoses a searchnet installation in one call, for workshop participants
#' and instructors on a fresh machine. It reports the R version, the searchnet
#' version, whether RSiena and the other required packages (Imports) are
#' installed at the required versions, which optional packages (Suggests) used
#' by the vignettes and the classroom module are present, whether the session
#' temporary directory is writable, and whether the classroom presets can be
#' found. With \code{run_smoke = TRUE} it also runs a tiny seeded SAOM-NK
#' simulation and draws its \{K\}-4 plot to a file in \code{tempdir()}, which
#' exercises the whole pipeline from environment to figure.
#'
#' Nothing is written outside \code{tempdir()}, and the caller's random number
#' state is restored afterwards.
#'
#' @param run_smoke Logical. Run the seeded smoke simulation (about 10 to 30
#'   seconds)? Default \code{TRUE}.
#' @param verbose Logical. Print the report? Default \code{TRUE}.
#' @return Invisibly, a list of class \code{"searchnet_setup_check"} with
#'   elements \code{ok} (logical: no check failed), \code{checks} (a data
#'   frame with columns \code{check}, \code{status} (\code{"ok"},
#'   \code{"warn"}, or \code{"fail"}), \code{detail}, and \code{fix}),
#'   \code{packages} (a data frame of required and optional packages with
#'   installed and minimum versions), \code{smoke} (a list with \code{ran},
#'   \code{ok}, \code{seconds}, and \code{error}), and \code{r_version} and
#'   \code{searchnet_version}.
#' @export
#' @examples
#' \donttest{
#' res <- searchnet_check_setup(run_smoke = FALSE)
#' res$ok
#' }
searchnet_check_setup <- function(run_smoke = TRUE, verbose = TRUE) {
  stopifnot(is.logical(run_smoke), length(run_smoke) == 1L, !is.na(run_smoke))
  checks <- list()
  add <- function(check, status, detail, fix = "") {
    checks[[length(checks) + 1L]] <<- data.frame(
      check = check, status = status, detail = detail, fix = fix,
      stringsAsFactors = FALSE)
  }

  # ---- R ----
  r_ver <- as.character(getRversion())
  if (getRversion() >= "4.1.0") {
    add("R version", "ok", r_ver)
  } else {
    add("R version", "fail", paste0(r_ver, " (searchnet needs >= 4.1.0)"),
        "Install a current R from https://CRAN.R-project.org")
  }

  # ---- searchnet ----
  sn_ver <- tryCatch(as.character(getNamespaceVersion("searchnet")),
                     error = function(e) NA_character_)
  add("searchnet version", if (is.na(sn_ver)) "fail" else "ok",
      if (is.na(sn_ver)) "not loaded" else sn_ver,
      if (is.na(sn_ver)) 'Install with install_github("sdownin/searchnet") from the remotes package' else "")

  # ---- Dependencies ----
  desc_file <- system.file("DESCRIPTION", package = "searchnet")
  desc <- if (nzchar(desc_file)) {
    tryCatch(read.dcf(desc_file, fields = c("Imports", "Suggests")),
             error = function(e) NULL)
  } else NULL
  imports  <- .parse_dep_field(if (is.null(desc)) NA else desc[1, "Imports"])
  suggests <- .parse_dep_field(if (is.null(desc)) NA else desc[1, "Suggests"])
  imports  <- imports[!imports$package %in% .base_pkgs, , drop = FALSE]
  ## The optional packages the vignettes and the classroom debrief reach for.
  key_suggests <- c("rmarkdown", "patchwork", "shiny", "future.apply", "withr")
  suggests <- suggests[suggests$package %in% key_suggests, , drop = FALSE]

  pkgs <- rbind(
    cbind(imports, type = rep("Imports", nrow(imports)),
          stringsAsFactors = FALSE),
    cbind(suggests, type = rep("Suggests", nrow(suggests)),
          stringsAsFactors = FALSE))
  pkgs$installed <- vapply(pkgs$package, .installed_version, character(1))
  pkgs$meets_minimum <- mapply(function(inst, minv) {
    if (is.na(inst)) return(FALSE)
    if (is.na(minv)) return(TRUE)
    utils::compareVersion(inst, minv) >= 0
  }, pkgs$installed, pkgs$minimum)
  rownames(pkgs) <- NULL

  install_fix <- function(p) {
    paste0("install.packages(c(", paste0('"', p, '"', collapse = ", "), "))")
  }

  ## RSiena gets its own line: it is the estimation engine and the most
  ## common install problem.
  rs <- pkgs[pkgs$package == "RSiena", , drop = FALSE]
  if (nrow(rs) == 1L) {
    if (is.na(rs$installed)) {
      add("RSiena", "fail", "not installed", install_fix("RSiena"))
    } else if (!rs$meets_minimum) {
      add("RSiena", "fail",
          paste0(rs$installed, " (needs >= ", rs$minimum, ")"),
          install_fix("RSiena"))
    } else {
      add("RSiena", "ok", rs$installed)
    }
  }
  imp <- pkgs[pkgs$type == "Imports" & pkgs$package != "RSiena", , drop = FALSE]
  bad <- imp[!imp$meets_minimum, , drop = FALSE]
  if (nrow(bad) == 0L) {
    key <- imp[imp$package %in% c("R6", "Matrix", "igraph", "ggplot2",
                                  "jsonlite"), , drop = FALSE]
    add("Required packages (Imports)", "ok",
        paste0("all ", nrow(imp), " installed; ",
               paste0(key$package, " ", key$installed, collapse = ", ")))
  } else {
    add("Required packages (Imports)", "fail",
        paste0("missing or too old: ",
               paste0(bad$package, ifelse(is.na(bad$installed), "",
                                          paste0(" ", bad$installed)),
                      collapse = ", ")),
        install_fix(bad$package))
  }
  sug <- pkgs[pkgs$type == "Suggests", , drop = FALSE]
  miss <- sug$package[is.na(sug$installed)]
  if (length(miss) == 0L) {
    add("Optional packages (Suggests)", "ok",
        paste0(sug$package, " ", sug$installed, collapse = ", "))
  } else {
    add("Optional packages (Suggests)", "warn",
        paste0("not installed: ", paste(miss, collapse = ", "),
               " (needed only for vignettes, the app, or parallel runs)"),
        install_fix(miss))
  }

  # ---- tempdir ----
  tmp_ok <- tryCatch({
    f <- tempfile("searchnet_check_", fileext = ".txt")
    writeLines("ok", f)
    res <- identical(readLines(f), "ok")
    unlink(f)
    res
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (tmp_ok) {
    add("Writable tempdir", "ok", tempdir())
  } else {
    add("Writable tempdir", "fail", paste0(tempdir(), " is not writable"),
        "Set TMPDIR (or TMP on Windows) to a writable folder and restart R")
  }

  # ---- Classroom presets ----
  presets <- c("airline", "tech", "pharma")
  found <- vapply(presets, function(p) {
    tryCatch({ .load_teaching_preset(p); TRUE }, error = function(e) FALSE)
  }, logical(1))
  if (all(found)) {
    add("Classroom presets", "ok", paste(presets, collapse = ", "))
  } else {
    add("Classroom presets", "fail",
        paste0("not found: ", paste(presets[!found], collapse = ", ")),
        'Reinstall with install_github("sdownin/searchnet", force = TRUE) from the remotes package')
  }

  # ---- Smoke run ----
  smoke <- list(ran = FALSE, ok = NA, seconds = NA_real_, error = NA_character_)
  if (run_smoke) {
    has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (has_seed) get(".Random.seed", envir = globalenv()) else NULL
    t0 <- proc.time()[["elapsed"]]
    res <- tryCatch(.setup_smoke_run(), error = function(e) conditionMessage(e))
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
    smoke$ran <- TRUE
    smoke$seconds <- round(proc.time()[["elapsed"]] - t0, 1)
    smoke$ok <- isTRUE(res)
    if (smoke$ok) {
      add("Smoke run (simulate + plot)", "ok",
          paste0("4 actors x 6 components, seeded; ", smoke$seconds, " s"))
    } else {
      smoke$error <- as.character(res)
      add("Smoke run (simulate + plot)", "fail", smoke$error,
          paste0("Fix any failed check above first; otherwise reinstall ",
                 "RSiena and searchnet and report the message at ",
                 "https://github.com/sdownin/searchnet/issues"))
    }
  }

  checks <- do.call(rbind, checks)
  out <- structure(list(
    ok                = !any(checks$status == "fail"),
    checks            = checks,
    packages          = pkgs,
    smoke             = smoke,
    r_version         = r_ver,
    searchnet_version = sn_ver
  ), class = "searchnet_setup_check")
  if (verbose) print(out)
  invisible(out)
}


#' @export
print.searchnet_setup_check <- function(x, ...) {
  tag <- c(ok = "[OK]  ", warn = "[WARN]", fail = "[FAIL]")
  cat("searchnet setup check\n")
  for (i in seq_len(nrow(x$checks))) {
    r <- x$checks[i, ]
    cat(tag[[r$status]], " ", r$check, ": ", r$detail, "\n", sep = "")
  }
  probs <- x$checks[x$checks$status != "ok", , drop = FALSE]
  cat("\n")
  if (nrow(probs) == 0L) {
    cat("All checks passed. You are ready to run SAOM-NK simulations.\n")
  } else {
    n_fail <- sum(probs$status == "fail")
    cat(if (n_fail > 0) paste0(n_fail, " problem(s) to fix")
        else "No blocking problems",
        if (any(probs$status == "warn"))
          paste0(", ", sum(probs$status == "warn"), " optional warning(s)"),
        ":\n", sep = "")
    for (i in seq_len(nrow(probs))) {
      if (nzchar(probs$fix[i])) {
        cat("  - ", probs$check[i], ": ", probs$fix[i], "\n", sep = "")
      }
    }
  }
  if (isTRUE(x$smoke$ran) && isTRUE(x$smoke$ok)) {
    cat("Next: the Quick Start in the README draws your first plot.\n")
  }
  invisible(x)
}


# ---------------------------------------------------------------------------- #
#  searchnet_classroom_submit_batch
# ---------------------------------------------------------------------------- #

## Parse one decision cell ("3; 7", "3,7", "3 7", "", NA) into integers.
## Returns NULL for an empty cell and NA (with attr "bad") when malformed.
.parse_decision_cell <- function(x) {
  if (length(x) == 0L || is.na(x)) return(NULL)
  s <- trimws(as.character(x))
  if (!nzchar(s) || toupper(s) %in% c("NA", "NONE", "-")) return(NULL)
  tok <- strsplit(s, "[;,|[:space:]]+")[[1]]
  tok <- tok[nzchar(tok)]
  if (length(tok) == 0L) return(NULL)
  if (!all(grepl("^[0-9]+$", tok))) {
    return(structure(NA, bad = paste0("'", s, "' is not a list of activity ",
                                      "numbers")))
  }
  as.integer(tok)
}


#' Submit a Round of Student Decisions from a CSV or Data Frame
#'
#' Reads the decisions for the current round from a form export (for example
#' a Google Forms or Qualtrics CSV), validates every row, and records the
#' valid ones by calling \code{\link{searchnet_classroom_submit}} for each, so
#' the result is exactly what submitting the rows one at a time would give.
#'
#' @section Input format:
#' One row per student per round, with these columns (names configurable):
#' \describe{
#'   \item{\code{student_id}}{The roster ID, such as \code{"student_3"}. A bare
#'     number such as \code{3} is read as \code{"student_3"}.}
#'   \item{\code{round}}{The round the decision is for. It must equal the round
#'     about to be played, \code{classroom$current_round + 1}.}
#'   \item{\code{adds}}{Activity numbers (column indices, \code{1} to
#'     \code{N}) to add, separated by semicolons, commas, bars, or spaces,
#'     for example \code{"3;7"}. Empty means none.}
#'   \item{\code{drops}}{Activity numbers to drop, in the same format.}
#' }
#' Other columns, such as a form timestamp or e-mail address, are ignored.
#'
#' @section Validation:
#' A row is invalid when its student is not on the roster, its round is not
#' the current one, a decision cell is not a list of whole numbers, an
#' activity number is outside \code{1} to \code{N}, or its student appears in
#' more than one row of the batch (all of that student's rows are rejected,
#' because which one was meant is ambiguous). Adds of activities already held
#' and drops of activities not held are not errors; as in
#' \code{searchnet_classroom_submit()}, they are ignored with a warning.
#'
#' @param classroom A \code{searchnet_classroom} object.
#' @param file_or_df Path to a CSV file, or a data frame with the columns
#'   described below.
#' @param atomic Logical. If \code{TRUE}, any invalid row stops the call with
#'   a report and nothing is submitted. If \code{FALSE} (default), valid rows
#'   are submitted and invalid rows are reported.
#' @param student_col,round_col,adds_col,drops_col Column names in the input.
#' @param quiet Logical. Suppress the per-student messages from
#'   \code{searchnet_classroom_submit()}? Default \code{TRUE}; a one-line
#'   summary is always printed.
#' @return The updated classroom object, invisibly, with attribute
#'   \code{"batch_report"}: a data frame with one row per input row and
#'   columns \code{row}, \code{student_id}, \code{round}, \code{status}
#'   (\code{"submitted"}, \code{"invalid"}, or \code{"error"}), and
#'   \code{problem}.
#' @export
#' @examples
#' \dontrun{
#' session <- searchnet_classroom_init(n_students = 3, industry = "airline",
#'                                     seed = 1)
#' decisions <- data.frame(student_id = c("student_1", "student_2", "student_3"),
#'                         round = 1, adds = c("3;7", "5", ""),
#'                         drops = c("1", "", "2"))
#' session <- searchnet_classroom_submit_batch(session, decisions)
#' attr(session, "batch_report")
#' }
searchnet_classroom_submit_batch <- function(classroom, file_or_df,
                                             atomic = FALSE,
                                             student_col = "student_id",
                                             round_col = "round",
                                             adds_col = "adds",
                                             drops_col = "drops",
                                             quiet = TRUE) {
  stopifnot(inherits(classroom, "searchnet_classroom"))
  stopifnot(is.logical(atomic), length(atomic) == 1L, !is.na(atomic))

  # ---- Read ----
  if (is.character(file_or_df) && length(file_or_df) == 1L) {
    if (!file.exists(file_or_df)) {
      stop("File not found: ", file_or_df, call. = FALSE)
    }
    df <- utils::read.csv(file_or_df, stringsAsFactors = FALSE,
                          colClasses = "character", check.names = FALSE,
                          na.strings = c("", "NA"), strip.white = TRUE,
                          fileEncoding = "UTF-8-BOM")
  } else if (is.data.frame(file_or_df)) {
    df <- as.data.frame(file_or_df, stringsAsFactors = FALSE)
  } else {
    stop("file_or_df must be a CSV path or a data frame.", call. = FALSE)
  }
  needed <- c(student_col, round_col, adds_col, drops_col)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0L) {
    stop("Missing column(s): ", paste(missing_cols, collapse = ", "),
         ". Found: ", paste(names(df), collapse = ", "), ". ",
         "Rename them or pass student_col / round_col / adds_col / drops_col.",
         call. = FALSE)
  }
  if (nrow(df) == 0L) stop("No rows to submit.", call. = FALSE)
  if (classroom$current_round >= classroom$n_rounds) {
    stop("Game is over. All ", classroom$n_rounds, " rounds completed.",
         call. = FALSE)
  }

  roster   <- classroom$student_roster$student_id
  expected <- classroom$current_round + 1L
  N        <- classroom$N

  # ---- Validate each row ----
  sid_raw <- trimws(as.character(df[[student_col]]))
  sid <- sid_raw
  bare <- !is.na(sid_raw) & !sid_raw %in% roster & grepl("^[0-9]+$", sid_raw)
  sid[bare] <- paste0("student_", as.integer(sid_raw[bare]))
  rnd <- suppressWarnings(as.numeric(as.character(df[[round_col]])))
  problems <- vector("list", nrow(df))
  parsed <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    p <- character()
    if (is.na(sid[i]) || !nzchar(sid[i])) {
      p <- c(p, "missing student id")
    } else if (!sid[i] %in% roster) {
      p <- c(p, paste0("unknown student '", sid_raw[i], "'"))
    }
    if (is.na(rnd[i])) {
      p <- c(p, paste0("round '", df[[round_col]][i], "' is not a number"))
    } else if (rnd[i] != expected) {
      p <- c(p, paste0("round ", rnd[i], " is not the current round (",
                       expected, ")"))
    }
    dec <- list(adds = NULL, drops = NULL)
    for (what in c("adds", "drops")) {
      col <- if (what == "adds") adds_col else drops_col
      v <- .parse_decision_cell(df[[col]][i])
      if (!is.null(v) && length(v) == 1L && is.na(v)) {
        p <- c(p, paste0(what, ": ", attr(v, "bad")))
      } else if (!is.null(v)) {
        oor <- v[v < 1L | v > N]
        if (length(oor) > 0L) {
          p <- c(p, paste0(what, ": activity ", paste(oor, collapse = ", "),
                           " outside 1-", N))
        }
        dec[[what]] <- v
      }
    }
    problems[[i]] <- p
    parsed[[i]] <- dec
  }
  dup <- sid %in% sid[duplicated(sid)] & sid %in% roster
  for (i in which(dup)) {
    problems[[i]] <- c(problems[[i]],
                       paste0("student '", sid[i], "' appears in more than ",
                              "one row"))
  }

  report <- data.frame(
    row        = seq_len(nrow(df)),
    student_id = sid,
    round      = rnd,
    status     = ifelse(lengths(problems) > 0L, "invalid", "pending"),
    problem    = vapply(problems, paste, character(1), collapse = "; "),
    stringsAsFactors = FALSE)

  bad_rows <- report[report$status == "invalid", , drop = FALSE]
  if (atomic && nrow(bad_rows) > 0L) {
    stop("Batch rejected (atomic = TRUE); nothing was submitted. ",
         nrow(bad_rows), " invalid row(s):\n",
         paste0("  row ", bad_rows$row, ": ", bad_rows$problem,
                collapse = "\n"),
         call. = FALSE)
  }

  # ---- Submit ----
  original <- classroom
  for (i in which(report$status == "pending")) {
    res <- tryCatch({
      call_submit <- function() {
        searchnet_classroom_submit(classroom, sid[i],
                                   adds = parsed[[i]]$adds,
                                   drops = parsed[[i]]$drops)
      }
      if (quiet) suppressMessages(call_submit()) else call_submit()
    }, error = function(e) e)
    if (inherits(res, "error")) {
      if (atomic) {
        stop("Batch rejected (atomic = TRUE); nothing was submitted. ",
             "Row ", i, ": ", conditionMessage(res), call. = FALSE)
      }
      report$status[i]  <- "error"
      report$problem[i] <- conditionMessage(res)
    } else {
      classroom <- res
      report$status[i] <- "submitted"
    }
  }
  if (atomic && !all(report$status == "submitted")) classroom <- original

  n_ok  <- sum(report$status == "submitted")
  n_bad <- nrow(report) - n_ok
  message("Batch for round ", expected, ": ", n_ok, " submitted, ", n_bad,
          " rejected. ", length(classroom$pending), " student(s) still pending",
          if (length(classroom$pending) > 0L)
            paste0(": ", paste(classroom$pending, collapse = ", ")), ".")
  if (n_bad > 0L) {
    rej <- report[report$status != "submitted", , drop = FALSE]
    message(paste0("  row ", rej$row, ": ", rej$problem, collapse = "\n"))
  }
  attr(classroom, "batch_report") <- report
  invisible(classroom)
}


# ---------------------------------------------------------------------------- #
#  searchnet_validate_preset
# ---------------------------------------------------------------------------- #

## Fields of the classroom preset schema (inst/teaching/presets/*.json).
.preset_fields <- c("industry", "N", "activity_names", "epistasis", "blocks",
                    "density", "density_param", "popularity",
                    "influence_weight", "epistasis_weight",
                    "difficulty_settings", "description",
                    "learning_objectives")


#' Validate a Classroom Preset
#'
#' Checks a custom classroom preset against the schema the shipped presets
#' (\code{airline}, \code{tech}, \code{pharma}) follow, and reports every
#' problem at once. \code{searchnet_classroom_init(industry = "custom")}
#' calls it on \code{custom_params}. The schema is documented in the
#' Instructor Guide, \code{system.file("teaching", "INSTRUCTOR_GUIDE.md",
#' package = "searchnet")}.
#'
#' Required: \code{N} (whole number, at least 2), \code{activity_names}
#' (distinct non-empty strings, at most \code{N}; fewer are padded with
#' generic names), \code{density} (initial tie density in \eqn{[0, 1]}), and
#' \code{influence_weight} (a number; the older name \code{epistasis_weight}
#' is accepted). Optional: \code{industry}, \code{description} (strings),
#' \code{learning_objectives} (strings), \code{epistasis} (only
#' \code{"modular"} is implemented), \code{blocks} (whole number from 1 to
#' \code{N}), \code{density_param} and \code{popularity} (numbers), and
#' \code{difficulty_settings}, which, if given, must define \code{intro},
#' \code{intermediate}, and \code{advanced}, each with
#' \code{steps_per_round} (whole number, at least 1), \code{n_AI} (whole
#' number, at least 1), and \code{shock_probability} (in \eqn{[0, 1]}).
#' Unknown fields produce a warning, not an error.
#'
#' @param path_or_list Path to a preset JSON file, or a named list.
#' @param quiet Logical. Suppress the confirmation message? Default
#'   \code{FALSE}.
#' @return Invisibly, the preset as a list (with \code{activity_names} as a
#'   character vector). Stops with a list of every problem if it is invalid.
#' @export
#' @examples
#' p <- system.file("teaching", "presets", "airline_preset.json",
#'                  package = "searchnet")
#' if (nzchar(p)) searchnet_validate_preset(p)
#' searchnet_validate_preset(list(N = 4, activity_names = c("A", "B", "C", "D"),
#'                                density = 0.3, influence_weight = 0.4))
searchnet_validate_preset <- function(path_or_list, quiet = FALSE) {
  src <- "preset"
  if (is.character(path_or_list) && length(path_or_list) == 1L) {
    src <- path_or_list
    if (!file.exists(path_or_list)) {
      stop("Preset file not found: ", path_or_list, call. = FALSE)
    }
    p <- tryCatch(jsonlite::fromJSON(path_or_list, simplifyVector = FALSE),
                  error = function(e) {
                    stop("Preset file is not valid JSON: ", path_or_list,
                         "\n  ", conditionMessage(e), call. = FALSE)
                  })
  } else if (is.list(path_or_list)) {
    p <- path_or_list
  } else {
    stop("path_or_list must be a path to a JSON file or a named list.",
         call. = FALSE)
  }
  if (length(p) == 0L || is.null(names(p)) || any(!nzchar(names(p)))) {
    stop("A preset must be a JSON object (named list) with named fields.",
         call. = FALSE)
  }

  errs <- character()
  warns <- character()
  is_num1 <- function(x) {
    is.numeric(unlist(x)) && length(unlist(x)) == 1L &&
      is.finite(unlist(x))
  }
  is_whole1 <- function(x) is_num1(x) && unlist(x) == round(unlist(x))
  is_str1 <- function(x) {
    is.character(unlist(x)) && length(unlist(x)) == 1L && !is.na(unlist(x))
  }

  # ---- Required ----
  N <- NULL
  if (is.null(p$N)) {
    errs <- c(errs, "N is required (the number of activities).")
  } else if (!is_whole1(p$N) || unlist(p$N) < 2) {
    errs <- c(errs, "N must be a single whole number of at least 2.")
  } else {
    N <- as.integer(unlist(p$N))
  }

  if (is.null(p$activity_names)) {
    errs <- c(errs, "activity_names is required (a list of strings).")
  } else {
    an <- unlist(p$activity_names)
    if (!is.character(an) || length(an) == 0L || anyNA(an) ||
        any(!nzchar(trimws(an)))) {
      errs <- c(errs, "activity_names must be a non-empty list of non-empty strings.")
    } else {
      if (anyDuplicated(an)) {
        errs <- c(errs, paste0("activity_names has duplicates: ",
                               paste(unique(an[duplicated(an)]), collapse = ", "),
                               "."))
      }
      if (!is.null(N) && length(an) > N) {
        errs <- c(errs, paste0("activity_names has ", length(an),
                               " entries but N is ", N, "."))
      } else if (!is.null(N) && length(an) < N) {
        warns <- c(warns, paste0("activity_names has ", length(an),
                                 " entries for N = ", N,
                                 "; the rest will be named Activity_k."))
      }
      p$activity_names <- an
    }
  }

  if (is.null(p$density)) {
    errs <- c(errs, "density is required (initial tie density in [0, 1]).")
  } else if (!is_num1(p$density) || unlist(p$density) < 0 ||
             unlist(p$density) > 1) {
    errs <- c(errs, "density must be a single number in [0, 1].")
  }

  iw <- p$influence_weight %||% p$epistasis_weight
  if (is.null(iw)) {
    errs <- c(errs, paste0("influence_weight is required (the older name ",
                           "epistasis_weight is also accepted)."))
  } else if (!is_num1(iw)) {
    errs <- c(errs, "influence_weight must be a single number.")
  }

  # ---- Optional ----
  for (f in c("industry", "description", "epistasis")) {
    if (!is.null(p[[f]]) && !is_str1(p[[f]])) {
      errs <- c(errs, paste0(f, " must be a single string."))
    }
  }
  if (is_str1(p$epistasis) && !identical(unlist(p$epistasis), "modular")) {
    errs <- c(errs, paste0("epistasis '", unlist(p$epistasis),
                           "' is not implemented; only \"modular\" ",
                           "(block-diagonal) is."))
  }
  if (!is.null(p$blocks)) {
    if (!is_whole1(p$blocks) || unlist(p$blocks) < 1 ||
        (!is.null(N) && unlist(p$blocks) > N)) {
      errs <- c(errs, paste0("blocks must be a whole number from 1 to N",
                             if (!is.null(N)) paste0(" (", N, ")"), "."))
    }
  }
  for (f in c("density_param", "popularity")) {
    if (!is.null(p[[f]]) && !is_num1(p[[f]])) {
      errs <- c(errs, paste0(f, " must be a single number."))
    }
  }
  if (!is.null(p$learning_objectives)) {
    lo <- unlist(p$learning_objectives)
    if (!is.character(lo)) {
      errs <- c(errs, "learning_objectives must be a list of strings.")
    }
  }
  if (!is.null(p$difficulty_settings)) {
    ds <- p$difficulty_settings
    levels <- c("intro", "intermediate", "advanced")
    if (!is.list(ds) || is.null(names(ds))) {
      errs <- c(errs, paste0("difficulty_settings must be an object with ",
                             "intro, intermediate, and advanced."))
    } else {
      miss <- setdiff(levels, names(ds))
      if (length(miss) > 0L) {
        errs <- c(errs, paste0("difficulty_settings is missing: ",
                               paste(miss, collapse = ", "), "."))
      }
      extra <- setdiff(names(ds), levels)
      if (length(extra) > 0L) {
        warns <- c(warns, paste0("difficulty_settings has unknown level(s): ",
                                 paste(extra, collapse = ", "), "."))
      }
      for (lv in intersect(levels, names(ds))) {
        s <- ds[[lv]]
        pre <- paste0("difficulty_settings$", lv, "$")
        if (!is_whole1(s$steps_per_round) || unlist(s$steps_per_round) < 1) {
          errs <- c(errs, paste0(pre, "steps_per_round must be a whole ",
                                 "number of at least 1."))
        }
        if (!is_whole1(s$n_AI) || unlist(s$n_AI) < 1) {
          errs <- c(errs, paste0(pre, "n_AI must be a whole number of at ",
                                 "least 1."))
        }
        if (!is_num1(s$shock_probability) ||
            unlist(s$shock_probability) < 0 ||
            unlist(s$shock_probability) > 1) {
          errs <- c(errs, paste0(pre, "shock_probability must be a number ",
                                 "in [0, 1]."))
        }
      }
    }
  }

  unknown <- setdiff(names(p), .preset_fields)
  if (length(unknown) > 0L) {
    warns <- c(warns, paste0("unknown field(s) ignored: ",
                             paste(unknown, collapse = ", "), "."))
  }

  if (length(errs) > 0L) {
    stop("Invalid classroom preset (", src, "): ", length(errs),
         " problem(s)\n", paste0("  - ", errs, collapse = "\n"),
         call. = FALSE)
  }
  for (w in warns) warning("Preset: ", w, call. = FALSE)
  if (!quiet) message("Preset OK: ", src)
  invisible(p)
}
