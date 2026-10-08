#!/usr/bin/env Rscript
# =============================================================================
# search_thm2.R
#
# Registered search for the online appendix's Theorem 2 worked example (chunk
# `thm2-examples`): M = 4, N = 6, XWX 0.3 on block-diagonal (6, 2), env seed
# 99, 25 expected opportunities per actor, empty start.
#
# Grid: density x inPop x outAct. Output: results/grid_thm2.csv and the
# selected point.
# =============================================================================

.sd <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  dirname(normalizePath(f[1], mustWork = FALSE))
})
source(file.path(.sd, "reparam_common.R"))
cat(session_line(), "\n")

DV <- "self$bipartite_rsienaDV"
M <- 4; N <- 6; SPA <- 25
SEEDS <- c(12345, 12346, 12347)

grid <- expand.grid(density = c(-0.5, -0.75, -1.0, -1.25, -1.5, -1.75, -2.0,
                                -2.25, -2.5, -2.75, -3.0),
                    inPop = c(0.3, 0.2, 0.1),
                    outAct = c(0.1, 0.05))

sm_at <- function(d, ip, oa) {
  list(dv_bipartite = list(
    name = DV,
    effects = list(
      list(effect = "density", parameter = d,  dv_name = DV, fix = TRUE),
      list(effect = "inPop",   parameter = ip, dv_name = DV, fix = TRUE),
      list(effect = "outAct",  parameter = oa, dv_name = DV, fix = TRUE)
    ),
    coDyadCovars = list(
      list(effect = "XWX", parameter = 0.3, dv_name = DV, fix = TRUE,
           nodeSet = c("COMPONENTS", "COMPONENTS"),
           interaction1 = "self$component_1_coDyadCovar",
           x = create_block_diag(6, 2))
    )
  ))
}

rows <- vector("list", nrow(grid))
for (g in seq_len(nrow(grid))) {
  d <- grid$density[g]; ip <- grid$inPop[g]; oa <- grid$outAct[g]
  t0 <- proc.time()[["elapsed"]]
  row <- data.frame(density = d, inPop = ip, outAct = oa)
  ok <- TRUE
  for (k in seq_along(SEEDS)) {
    e <- SaomNkRSienaBiEnv$new(list(M = M, N = N, BI_PROB = 0, rand_seed = 99,
                                    name = "_thm2_search_"))
    utils::capture.output(
      e$search_rsiena(sm_at(d, ip, oa), iterations_per_actor = SPA,
                      run_seed = SEEDS[k]))
    s <- regime_stats(e)
    pass <- s$density_final >= 0.15 && s$density_final <= 0.60 &&
            s$n_boundary <= 1 && s$toggles_per_actor >= 3 &&
            s$ministeps >= 50
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
  cat(sprintf("  d=%5.2f inPop=%4.2f outAct=%4.2f  admissible=%s  (%.1fs)\n",
              d, ip, oa, ok, row$runtime_s))
}

res <- do.call(rbind, rows)
write.csv(res, file.path(results_dir, "grid_thm2.csv"), row.names = FALSE)

sel <- select_point(res, c("density", "inPop", "outAct"), c(-0.5, 0.3, 0.1))
if (is.null(sel)) {
  cat("NO ADMISSIBLE POINT on the registered grid; original kept.\n")
} else {
  cat(sprintf("SELECTED: density = %.2f, inPop = %.2f, outAct = %.2f (distance %.3f)\n",
              sel$density, sel$inPop, sel$outAct, sel$distance))
  write.csv(sel[, c("density", "inPop", "outAct", "distance")],
            file.path(results_dir, "selected_thm2.csv"), row.names = FALSE)
}
