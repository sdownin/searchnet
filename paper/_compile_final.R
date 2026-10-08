## Superseded 2026-10-07 by paper/build_submission.R, which this stub runs.
## The old script setwd() to a fixed D:/ path and sourced R/saomnk-loader.R,
## which no longer exists. Arguments (e.g. --fast) are passed through when run
## with Rscript.
local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  here <- if (length(f)) dirname(normalizePath(f[1])) else {
    o <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
    if (is.null(o)) "." else dirname(normalizePath(o))
  }
  source(file.path(here, "build_submission.R"), chdir = FALSE)
})
