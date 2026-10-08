#!/usr/bin/env Rscript
# =============================================================================
# search_endogenous.R
#
# Registered search for the endogenous-landscape illustration (manuscript
# chunk `endogenous-setup`; reproduce_all.R Illustration 2): 12 actors, 12
# components, block-diagonal W (4 blocks, weight 0.02), strategies egoX and
# inPopX as in the paper (default weight 0.2), 30 expected opportunities per
# actor, empty start (env seed 1234).
#
# Grid: density x popularity x scope. Output: results/grid_endogenous.csv and
# the selected point.
# =============================================================================

.sd <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  dirname(normalizePath(f[1], mustWork = FALSE))
})
source(file.path(.sd, "reparam_common.R"))
cat(session_line(), "\n")

M <- 12; N <- 12; SPA <- 30
W <- saomnk_block_diagonal(12, 4)
SEEDS <- c(12345, 12346, 12347)

grid <- expand.grid(density = c(-0.3, -0.5, -0.75, -1.0, -1.25, -1.5, -2.0,
                                -2.5, -3.0),
                    popularity = c(0.2, 0.1, 0.05),
                    scope = c(0.1, 0.05, 0.025))

rows <- vector("list", nrow(grid))
for (g in seq_len(nrow(grid))) {
  d <- grid$density[g]; p <- grid$popularity[g]; sc <- grid$scope[g]
  t0 <- proc.time()[["elapsed"]]
  row <- data.frame(density = d, popularity = p, scope = sc)
  mod <- saomnk_model(
    density = d, popularity = p, scope = sc,
    influence_matrix = W, influence_weight = 0.02,
    strategies = list(
      egoX   = rep(c(-1, 0, 1), length.out = 12),
      inPopX = rep(c( 1, 0,-1), length.out = 12)
    )
  )
  ok <- TRUE
  for (k in seq_along(SEEDS)) {
    e <- saomnk_env(M = M, N = N, density = 0, seed = 1234)
    saomnk_run(e, mod, steps_per_actor = SPA, seed = SEEDS[k])
    s <- regime_stats(e)
    pass <- s$density_final >= 0.15 && s$density_final <= 0.60 &&
            s$n_boundary <= 2 && s$toggles_per_actor >= 3 &&
            s$ministeps >= 0.5 * M * SPA
    ok <- ok && pass
    row[[sprintf("s%d_density", k)]]   <- s$density_final
    row[[sprintf("s%d_boundary", k)]]  <- s$n_boundary
    row[[sprintf("s%d_toggles", k)]]   <- s$toggles_per_actor
    row[[sprintf("s%d_ministeps", k)]] <- s$ministeps
    row[[sprintf("s%d_pass", k)]]      <- pass
  }
  row$admissible <- ok
  row$runtime_s  <- round(proc.time()[["elapsed"]] - t0, 2)
  rows[[g]] <- row
  cat(sprintf("  d=%5.2f p=%4.2f s=%5.3f  admissible=%s  (%.1fs)\n",
              d, p, sc, ok, row$runtime_s))
}

res <- do.call(rbind, rows)
write.csv(res, file.path(results_dir, "grid_endogenous.csv"), row.names = FALSE)

sel <- select_point(res, c("density", "popularity", "scope"), c(-0.3, 0.2, 0.1))
if (is.null(sel)) {
  cat("NO ADMISSIBLE POINT on the registered grid; original kept.\n")
} else {
  cat(sprintf("SELECTED: density = %.2f, popularity = %.2f, scope = %.3f (distance %.3f)\n",
              sel$density, sel$popularity, sel$scope, sel$distance))
  write.csv(sel[, c("density", "popularity", "scope", "distance")],
            file.path(results_dir, "selected_endogenous.csv"), row.names = FALSE)
}
