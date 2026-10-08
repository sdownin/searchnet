#!/usr/bin/env Rscript
# =============================================================================
# search_ergodicity.R
#
# Registered screen for the ergodicity sweep's density coefficient
# (manuscript chunk `ergodicity`; appendix chunks `thm4-convergence` and
# `thm4-endpoints`). For each density_par on the grid, a screening sweep at
# the longest registered run length (240) with 4 replicates per arm records
# ONLY the pooled mean final density of the 8 runs (both arms together). The
# per-arm means, the gap and the TOST verdict are deliberately not extracted.
#
# Output: results/grid_ergodicity.csv and the selected point.
# =============================================================================

.sd <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  dirname(normalizePath(f[1], mustWork = FALSE))
})
source(file.path(.sd, "reparam_common.R"))
cat(session_line(), "\n")

grid <- data.frame(density_par = c(-0.5, -0.75, -1.0, -1.25, -1.5, -1.75,
                                   -2.0, -2.5, -3.0, -3.5, -4.0))

rows <- vector("list", nrow(grid))
for (g in seq_len(nrow(grid))) {
  d <- grid$density_par[g]
  t0 <- proc.time()[["elapsed"]]
  sw <- searchnet_ergodicity_sweep(
    M = 12, N = 15, start_densities = c(0.1, 0.8), run_lengths = 240,
    replicates = 4, density_par = d, pop_par = 0.15, epistasis_par = 0.2,
    equivalence_margin = 0.05, seed = 42
  )
  pooled <- mean(sw$runs$final_density)
  rm(sw)
  ok <- pooled >= 0.25 && pooled <= 0.65
  rows[[g]] <- data.frame(density_par = d, pooled_mean_density = pooled,
                          n_runs = 8, admissible = ok,
                          runtime_s = round(proc.time()[["elapsed"]] - t0, 2))
  cat(sprintf("  density_par=%5.2f  pooled=%.3f  admissible=%s  (%.1fs)\n",
              d, pooled, ok, rows[[g]]$runtime_s))
}

res <- do.call(rbind, rows)
write.csv(res, file.path(results_dir, "grid_ergodicity.csv"), row.names = FALSE)

res$density <- res$density_par
sel <- select_point(res, "density_par", -0.5, density_col = "density")
if (is.null(sel)) {
  cat("NO ADMISSIBLE POINT on the registered grid; original kept.\n")
} else {
  cat(sprintf("SELECTED: density_par = %.2f (distance %.3f)\n",
              sel$density_par, sel$distance))
  write.csv(sel[, c("density_par", "distance")],
            file.path(results_dir, "selected_ergodicity.csv"), row.names = FALSE)
}
