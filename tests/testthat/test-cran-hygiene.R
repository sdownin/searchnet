## test-cran-hygiene.R
## Regression guards for the `R CMD check --as-cran` findings cleared during
## JSS/CRAN preparation. Each of these was a real check finding once; the point
## of the file is that re-introducing one fails here rather than at submission,
## when a full --as-cran run is the only thing that would notice.
##
## These tests read the package SOURCE tree. Under `R CMD check` the sources are
## not shipped alongside the tests, so every test skips there and runs only from
## a working copy (devtools::test(), or testthat from tests/testthat).

pkg_root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
r_dir <- file.path(pkg_root, "R")

skip_without_sources <- function() {
  skip_if_not(dir.exists(r_dir), "package source tree not available")
}

r_files <- function() list.files(r_dir, pattern = "[.]R$", full.names = TRUE)

read_src <- function(f) {
  con <- file(f, encoding = "UTF-8")
  on.exit(close(con))
  readLines(con, warn = FALSE)
}

# ===========================================================================
# 1. Non-ASCII characters in R code
# ===========================================================================
# `R CMD check` raises a WARNING for non-ASCII bytes in R sources. It tolerates
# them in comments, but the house style is ASCII throughout (and no em-dashes),
# so this guard is stricter than the check itself and covers plain comments too.
#
# Roxygen lines (#') are excluded ONLY because they are generated and owned
# alongside man/, not because they are exempt from the rule. Widen this to every
# line once the roxygen sources are ASCII-clean.
test_that("R code and plain comments contain no non-ASCII characters", {
  skip_without_sources()
  offenders <- character(0)
  for (f in r_files()) {
    lines <- read_src(f)
    keep <- !grepl("^\\s*#'", lines)
    bad <- which(is.na(iconv(lines, "UTF-8", "ASCII")) & keep)
    if (length(bad)) {
      offenders <- c(offenders,
                     sprintf("%s:%d", basename(f), bad))
    }
  }
  expect_equal(offenders, character(0))
})

# ===========================================================================
# 2. Partial argument matches
# ===========================================================================
# `ggsave(file = )` partially matches the `filename` formal. R resolves it, but
# --as-cran reports every site, and a future ggplot2 formal named `file` would
# silently change which argument is filled.
#
# This walks the parse tree rather than grepping lines. A line-based check on
# "ggsave(file =" misses the wrapped form
#
#   ggsave(
#     file = file.path(...)
#
# which is how six of the sites in this package were actually written, and how
# they survived the first sweep.
test_that("ggsave() is called with 'filename', never the partial match 'file'", {
  skip_without_sources()

  offenders <- character(0)
  walk <- function(e, where) {
    if (is.call(e)) {
      fn <- e[[1]]
      nm <- if (is.name(fn)) as.character(fn) else
            if (is.call(fn) && identical(as.character(fn[[1]]), "::"))
              as.character(fn[[3]]) else ""
      if (identical(nm, "ggsave") && "file" %in% names(e)) {
        offenders <<- c(offenders, where)
      }
    }
    if (is.call(e) || is.pairlist(e)) {
      ## Index straight into the list. An empty symbol (the gap left by a
      ## trailing comma in a call) may be TESTED in place, but binding it to a
      ## variable first raises "argument is missing", so never do that here.
      parts <- as.list(e)
      for (i in seq_along(parts)) {
        if (is.call(parts[[i]]) || is.pairlist(parts[[i]])) walk(parts[[i]], where)
      }
    }
  }

  for (f in r_files()) {
    exprs <- parse(f, keep.source = FALSE)
    for (e in exprs) walk(e, basename(f))
  }
  expect_equal(unique(offenders), character(0))
})

# ===========================================================================
# 3. Every namespace referenced with :: is declared in DESCRIPTION
# ===========================================================================
# This is the guard that would have caught ggrepel, ggExtra and ggridges. An
# undeclared package is not merely a check WARNING: the call fails at run time
# for any user who does not happen to have the package installed.
test_that("every '::' namespace is declared in DESCRIPTION", {
  skip_without_sources()

  d <- read.dcf(file.path(pkg_root, "DESCRIPTION"))
  declared <- character(0)
  for (fld in c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")) {
    if (fld %in% colnames(d) && !is.na(d[1, fld])) {
      parts <- strsplit(d[1, fld], ",")[[1]]
      parts <- trimws(sub("\\(.*", "", parts))
      declared <- c(declared, parts[nzchar(parts)])
    }
  }
  ## base is always available and is never declared.
  declared <- unique(c(declared, "base", "searchnet"))

  used <- character(0)
  for (f in r_files()) {
    lines <- read_src(f)
    lines <- lines[!grepl("^\\s*#", lines)]          # drop comments and roxygen
    m <- regmatches(lines, gregexpr("[A-Za-z][A-Za-z0-9._]*(?=:::?[A-Za-z._])",
                                    lines, perl = TRUE))
    used <- c(used, unlist(m))
  }
  used <- sort(unique(used))

  expect_equal(setdiff(used, declared), character(0))
})

# ===========================================================================
# 4. globals.R declares NSE column names only
# ===========================================================================
# utils::globalVariables() silences a diagnostic and verifies nothing, so an
# actually-unbound object listed there hides a run-time error forever.
# '.saomnk_dir' was exactly that case: undefined everywhere, flagged by the
# check, and fixed at its site in R/utils.R rather than declared here.
test_that("globals.R exists and declares a non-trivial set of names", {
  skip_without_sources()
  g <- file.path(r_dir, "globals.R")
  expect_true(file.exists(g))

  e <- new.env()
  code <- parse(g)
  expect_length(code, 1L)

  names_declared <- eval(code[[1]][[2]], envir = e)
  expect_type(names_declared, "character")
  expect_gt(length(names_declared), 100L)
  expect_false(any(duplicated(names_declared)))
})

test_that("globals.R does not declare known non-NSE objects", {
  skip_without_sources()
  g <- file.path(r_dir, "globals.R")
  skip_if_not(file.exists(g))
  names_declared <- eval(parse(g)[[1]][[2]])
  expect_false(".saomnk_dir" %in% names_declared)
})

test_that("searchnet_proof() no longer references the undefined .saomnk_dir", {
  skip_without_sources()
  src <- unlist(lapply(r_files(), read_src))
  src <- src[!grepl("^\\s*#", src)]
  expect_false(any(grepl(".saomnk_dir", src, fixed = TRUE)))
})
