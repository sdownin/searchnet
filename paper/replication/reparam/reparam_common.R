# =============================================================================
# reparam_common.R
#
# Shared helpers for the re-parameterization searches of the paper's simulated
# illustrations under searchnet 0.11.0 (state-carrying SAOM paths). The
# criteria, grids, seeds and selection rule were fixed in a pre-registration
# committed before any search ran (the private file
# 2026-10-07_PREREG_illustration_parameters.md, kept with the author's drafts).
#
# Every quantity computed here is a REGIME property of a run (how full the
# network is, whether actors sit at a boundary, how much the chain moved). None
# is the pattern the paper's text claims; those are computed only at the
# selected point, by claims_at_selected.R.
# =============================================================================

suppressPackageStartupMessages(library(searchnet))
stopifnot(as.character(utils::packageVersion("searchnet")) == "0.11.0")

reparam_dir <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) dirname(normalizePath(f[1], mustWork = FALSE)) else "."
})
results_dir <- file.path(reparam_dir, "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

## Regime statistics of one finished run.
regime_stats <- function(env) {
  B   <- env$bipartite_matrix
  M   <- nrow(B); N <- ncol(B)
  arr <- env$bi_env_arr
  B0  <- env$bipartite_matrix_init
  tog <- sum(abs(arr[, , 1] - B0))
  if (dim(arr)[3] > 1)
    tog <- tog + sum(abs(arr[, , -1, drop = FALSE] -
                         arr[, , -dim(arr)[3], drop = FALSE]))
  deg <- rowSums(B)
  data.frame(
    density_final     = sum(B) / (M * N),
    n_boundary        = sum(deg == 0 | deg == N),
    toggles_per_actor = tog / M,
    ministeps         = sum(env$path_segments$n_ministeps),
    seg1_ministeps    = env$path_segments$n_ministeps[1],
    seg2_ministeps    = if (nrow(env$path_segments) > 1)
                          env$path_segments$n_ministeps[2] else NA_integer_
  )
}

## Selection rule (pre-registered): smallest Euclidean distance from the
## original coefficient vector in raw units, among admissible points; ties
## (within 1e-9) broken by fewer coordinates changed, then by the less
## negative density coefficient.
select_point <- function(grid, par_cols, original, density_col = "density") {
  adm <- grid[grid$admissible %in% TRUE, , drop = FALSE]
  if (!nrow(adm)) return(NULL)
  X <- as.matrix(adm[, par_cols, drop = FALSE])
  o <- matrix(original, nrow(X), length(original), byrow = TRUE)
  adm$distance  <- sqrt(rowSums((X - o)^2))
  adm$n_changed <- rowSums(abs(X - o) > 1e-12)
  adm$dist_r    <- round(adm$distance, 9)
  adm <- adm[order(adm$dist_r, adm$n_changed, -adm[[density_col]]), ]
  adm[1, , drop = FALSE]
}

session_line <- function() {
  sprintf("searchnet %s | RSiena %s | %s | %s %s | %s",
          utils::packageVersion("searchnet"), utils::packageVersion("RSiena"),
          R.version.string, Sys.info()[["sysname"]], Sys.info()[["release"]],
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}
