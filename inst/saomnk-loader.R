##
## searchnet Loader
##
## Source this file to load the searchnet framework in a standalone R session
## (without installing the package). It sources EVERY .R file in the package's
## R/ directory, with the R6 class hierarchy loaded first.
##
## Usage:
##   source('path/to/SaoMNK/inst/saomnk-loader.R')
##   env <- SaomNkRSienaBiEnv$new(list(M=3, N=6, BI_PROB=0.5, rand_seed=123, name='test'))
##
## Or, if the location cannot be auto-detected (e.g. some Rscript invocations),
## set `.saomnk_dir` first -- it may point at either the package root or R/:
##   .saomnk_dir <- 'path/to/SaoMNK/R'
##   source(file.path(.saomnk_dir, '..', 'inst', 'saomnk-loader.R'))
##
## Options:
##   options(saomnk.loader.quiet  = TRUE)  # suppress the summary message
##   options(saomnk.loader.strict = TRUE)  # stop() on the first file that fails
##

## ---------------------------------------------------------------------------
## 1. Locate the R/ directory
## ---------------------------------------------------------------------------
## `dirname(sys.frame(1)$ofile)` is NULL under Rscript and in some IDE contexts,
## which previously fell back silently to getwd() and then failed to find the
## sources. Try several strategies and validate the result before using it.

.saomnk_find_r_dir <- function(hint = NULL) {
  ## A directory is the R/ dir iff it contains the base R6 class definition.
  .valid <- function(d) {
    !is.null(d) && length(d) == 1L && !is.na(d) && nzchar(d) &&
      file.exists(file.path(d, 'saomnk-base.R'))
  }
  ## For a candidate that is the package ROOT (or inst/), R/ sits alongside:
  ## <root>/R, or <root>/../R when called from inst/.
  .expand <- function(d) {
    if (is.null(d) || length(d) != 1L || is.na(d) || !nzchar(d)) return(character(0))
    c(d, file.path(d, 'R'), file.path(d, '..', 'R'), file.path(d, '..', '..', 'R'))
  }

  cands <- character(0)

  ## (a) explicit hint from the caller
  if (!is.null(hint)) cands <- c(cands, .expand(hint))

  ## (b) this file's own location, via the `ofile` of any calling source() frame
  for (.i in rev(seq_len(sys.nframe()))) {
    .of <- tryCatch(sys.frame(.i)$ofile, error = function(e) NULL)
    if (!is.null(.of) && length(.of) == 1L && nzchar(.of))
      cands <- c(cands, .expand(dirname(normalizePath(.of, mustWork = FALSE))))
  }

  ## (c) Rscript --file=, then the working directory
  .args <- commandArgs(trailingOnly = FALSE)
  .fa <- grep('^--file=', .args, value = TRUE)
  if (length(.fa))
    cands <- c(cands, .expand(dirname(normalizePath(sub('^--file=', '', .fa[1]),
                                                    mustWork = FALSE))))
  cands <- c(cands, .expand(getwd()))

  for (.c in cands) if (.valid(.c)) return(normalizePath(.c, mustWork = FALSE))
  NULL
}

.saomnk_dir <- .saomnk_find_r_dir(
  hint = if (exists('.saomnk_dir', envir = globalenv()))
           get('.saomnk_dir', envir = globalenv()) else NULL
)

if (is.null(.saomnk_dir)) {
  stop("saomnk-loader.R: could not locate the package R/ directory. ",
       "Set it explicitly before sourcing, e.g.\n",
       "  .saomnk_dir <- 'path/to/SaoMNK/R'", call. = FALSE)
}

## ---------------------------------------------------------------------------
## 2. Dependencies
## ---------------------------------------------------------------------------
## Hard dependencies: the package cannot load without these.
for (.pkg in c('R6', 'igraph', 'RSiena', 'ggplot2', 'plyr', 'dplyr', 'tidyr',
               'Matrix', 'network', 'reshape2')) {
  suppressPackageStartupMessages(library(.pkg, character.only = TRUE))
}
## Soft dependencies: only some plot/report modules need these. A missing one
## degrades specific functions rather than blocking the whole load.
.saomnk_soft_missing <- character(0)
for (.pkg in c('cowplot', 'ggraph', 'ggpubr', 'grid', 'gridExtra', 'texreg', 'uuid')) {
  if (!suppressWarnings(suppressPackageStartupMessages(
        require(.pkg, character.only = TRUE, quietly = TRUE))))
    .saomnk_soft_missing <- c(.saomnk_soft_missing, .pkg)
}

## ---------------------------------------------------------------------------
## 3. Source every .R file in R/
## ---------------------------------------------------------------------------
## Load order only matters for the R6 hierarchy (a class must exist before the
## class that inherits from it). Everything else defines free functions, so it
## is loaded alphabetically for determinism.

.saomnk_core <- c('utils.R', 'saomnk-base.R', 'saomnk-class.R')

.saomnk_all <- sort(basename(Sys.glob(file.path(.saomnk_dir, '*.R'))))
## Never source the loader itself, should a copy sit in R/.
.saomnk_all <- setdiff(.saomnk_all, 'saomnk-loader.R')

.saomnk_order <- c(intersect(.saomnk_core, .saomnk_all),
                   setdiff(.saomnk_all, .saomnk_core))

.saomnk_failed <- list()
for (.f in .saomnk_order) {
  .p <- file.path(.saomnk_dir, .f)
  .err <- tryCatch({ source(.p, local = FALSE); NULL },
                   error = function(e) conditionMessage(e))
  if (!is.null(.err)) {
    if (isTRUE(getOption('saomnk.loader.strict', FALSE)))
      stop(sprintf("saomnk-loader.R: failed to source '%s': %s", .f, .err), call. = FALSE)
    .saomnk_failed[[.f]] <- .err
  }
}

## ---------------------------------------------------------------------------
## 4. Report
## ---------------------------------------------------------------------------
if (!isTRUE(getOption('saomnk.loader.quiet', FALSE))) {
  message(sprintf("searchnet loaded: %d/%d R files from %s",
                  length(.saomnk_order) - length(.saomnk_failed),
                  length(.saomnk_order), .saomnk_dir))
  if (length(.saomnk_soft_missing))
    message("  optional packages not installed (some plots/reports unavailable): ",
            paste(.saomnk_soft_missing, collapse = ', '))
}
if (length(.saomnk_failed))
  for (.f in names(.saomnk_failed))
    warning(sprintf("saomnk-loader.R: '%s' did not load: %s", .f, .saomnk_failed[[.f]]),
            call. = FALSE)

## Keep `.saomnk_failed` available for diagnostics; drop the rest.
suppressWarnings(rm(list = intersect(
  c('.saomnk_dir', '.saomnk_core', '.saomnk_all', '.saomnk_order', '.f', '.p', '.err',
    '.pkg', '.i', '.of', '.args', '.fa', '.c', '.saomnk_find_r_dir',
    '.saomnk_soft_missing'),
  ls(all.names = TRUE)), envir = environment()))

## Return the class generator (backward-compat with `searchnet <- source(...)$value`)
SaomNkRSienaBiEnv
