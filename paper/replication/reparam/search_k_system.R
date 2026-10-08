#!/usr/bin/env Rscript
# =============================================================================
# search_k_system.R
#
# Registered search for the shared baseline of the two {K}-system figures
# (fig_k_coupling_sim.png and fig_k_shock_relocation.png, produced by
# paper/replication/make_k_system_figures.R): 20 actors, 24 components,
# block-diagonal W (4 blocks), 40 expected opportunities per actor, empty start.
#
# Grid: density x popularity; scope 0.05 and influence weight 0.05 fixed.
# Shock arms keep the original shock sizes: density d -> d - 1.5 and
# popularity p -> p + 1.05.
#
# Output: results/grid_k_system.csv (one row per grid point; regime statistics
# per seed and arm, admissibility flags, runtime) and the selected point.
# =============================================================================

.sd <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  dirname(normalizePath(f[1], mustWork = FALSE))
})
source(file.path(.sd, "reparam_common.R"))
cat(session_line(), "\n")

M <- 20; N <- 24; SPA <- 40
W <- saomnk_block_diagonal(N, 4)
SEEDS_A <- c(20260909, 20260910, 20260911)
SEEDS_B <- c(77001, 77002, 77003)
D_SHOCK <- -1.5; P_SHOCK <- 1.05

grid <- expand.grid(density = c(-5.0, -4.5, -4.0, -3.5, -3.0, -2.5, -2.0,
                                -1.5, -1.0, -0.5),
                    popularity = c(0.15, 0.10, 0.05))

model_at <- function(d, p) {
  saomnk_model(density = d, popularity = p, scope = 0.05,
               influence_matrix = W, influence_weight = 0.05)
}

run_one <- function(d, p, seed, shocks = NULL) {
  set.seed(seed)
  e <- saomnk_env(M = M, N = N, density = 0, seed = seed)
  saomnk_run(e, model_at(d, p), steps_per_actor = SPA, seed = seed,
             shocks = shocks)
  regime_stats(e)
}

rows <- vector("list", nrow(grid))
for (g in seq_len(nrow(grid))) {
  d <- grid$density[g]; p <- grid$popularity[g]
  t0 <- proc.time()[["elapsed"]]
  row <- data.frame(density = d, popularity = p)

  ## --- Coupling runs -------------------------------------------------------
  okA <- TRUE
  for (k in seq_along(SEEDS_A)) {
    s <- run_one(d, p, SEEDS_A[k])
    pass <- s$density_final >= 0.15 && s$density_final <= 0.60 &&
            s$n_boundary <= 2 && s$toggles_per_actor >= 3 &&
            s$ministeps >= 0.5 * M * SPA
    okA <- okA && pass
    row[[sprintf("A%d_density", k)]]  <- s$density_final
    row[[sprintf("A%d_boundary", k)]] <- s$n_boundary
    row[[sprintf("A%d_toggles", k)]]  <- s$toggles_per_actor
    row[[sprintf("A%d_ministeps", k)]] <- s$ministeps
    row[[sprintf("A%d_pass", k)]]     <- pass
  }
  row$coupling_admissible <- okA

  ## --- Relocation runs, only where the coupling criteria hold --------------
  okB <- NA
  for (k in seq_along(SEEDS_B)) {
    for (arm in c("control", "sparsity", "popularity")) {
      row[[sprintf("B%d_%s_density", k, arm)]] <- NA_real_
    }
    row[[sprintf("B%d_pass", k)]] <- NA
  }
  if (okA) {
    okB <- TRUE
    for (k in seq_along(SEEDS_B)) {
      sd_ <- SEEDS_B[k]
      sc <- run_one(d, p, sd_)
      ss <- run_one(d, p, sd_, list(saomnk_shock("density", d, 1),
                                    saomnk_shock("density", d + D_SHOCK, 1)))
      sp <- run_one(d, p, sd_, list(saomnk_shock("popularity", p, 1),
                                    saomnk_shock("popularity", p + P_SHOCK, 1)))
      seg_ok <- all(c(ss$seg1_ministeps, ss$seg2_ministeps,
                      sp$seg1_ministeps, sp$seg2_ministeps) >= 100)
      pass <- sc$density_final >= 0.15 && sc$density_final <= 0.60 &&
              ss$density_final >= 0.02 && sp$density_final <= 0.90 && seg_ok
      okB <- okB && pass
      row[[sprintf("B%d_control_density", k)]]    <- sc$density_final
      row[[sprintf("B%d_sparsity_density", k)]]   <- ss$density_final
      row[[sprintf("B%d_popularity_density", k)]] <- sp$density_final
      row[[sprintf("B%d_pass", k)]] <- pass
    }
  }
  row$relocation_admissible <- okB
  row$admissible <- okA && isTRUE(okB)
  row$runtime_s  <- round(proc.time()[["elapsed"]] - t0, 2)
  rows[[g]] <- row
  cat(sprintf("  d=%5.2f p=%4.2f  coupling=%s relocation=%s  (%.1fs)\n",
              d, p, okA, okB, row$runtime_s))
}

res <- do.call(rbind, rows)
write.csv(res, file.path(results_dir, "grid_k_system.csv"), row.names = FALSE)

sel <- select_point(res, c("density", "popularity"), c(-5.0, 0.15))
if (is.null(sel)) {
  cat("NO ADMISSIBLE POINT on the registered grid; original kept.\n")
} else {
  cat(sprintf("SELECTED: density = %.2f, popularity = %.2f (distance %.3f)\n",
              sel$density, sel$popularity, sel$distance))
  write.csv(sel[, c("density", "popularity", "distance")],
            file.path(results_dir, "selected_k_system.csv"), row.names = FALSE)
}
