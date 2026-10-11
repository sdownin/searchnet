#!/usr/bin/env Rscript
## Regenerate inst/extdata/searchnet_floor_power.rds, the precomputed power
## and identification study of an ESTIMATED actor floor shown in
## vignettes/searchnet-degree-bounds.Rmd.
##
## Design, fixed here before the run (the vignette reports whatever it returns):
##   * planted process: density -1, basic rate 3 per period, N = 8 components,
##     3 waves, wave 1 a Bernoulli draw at density 0.2 with every actor brought
##     to at least one component (seeded), and an actor floor of 1 that is
##     NOT a constraint: outIso planted at 0, -1, -2 or -4;
##   * the number of observed last-tie drops is moved by the number of actors,
##     M in {15, 30, 60, 120}, and by the planted value itself;
##   * every panel is estimated with outIso free (starting value -2) and
##     density, by unconditional method of moments, n3 = 500, nsub = 4,
##     nbrNodes = 1, seeds derived from one base seed per cell;
##   * 100 replications per cell; a panel with no observed drop to degree 0 is
##     filed as non-identified and not estimated.
## Reading, stated in advance: the result is the MDE of outIso by the observed
## count of last-tie drops (searchnet_floor_power(), pooled over planted
## values), with bias and 95% coverage by planted value and count. A bin is
## called adequate when it has at least 30 converged replications, |bias| at
## most 0.25 and coverage at least 0.90.
##
## Usage (from the package root):
##   Rscript tools/make_floor_power_vignette_data.R

pkgload::load_all(".", quiet = TRUE)

planted <- c(0, -1, -2, -4)
Ms      <- c(15L, 30L, 60L, 120L)
base_seed <- 20261011L
design <- list(effects_spec = "density", N = 8L, waves = 3L, reps = 100L,
               nbrNodes = 1L, n3 = 500L, nsub = 4L, init_density = 0.2,
               rate = 3, density = -1, floor_start = -2)

b <- searchnet_degree_bounds(actor = c(min = 1), estimate_floor = TRUE,
                             floor_start = design$floor_start)
runs <- list()
cells <- expand.grid(outIso = planted, M = Ms)
t0 <- proc.time()[["elapsed"]]
for (k in seq_len(nrow(cells))) {
  th <- cells$outIso[k]; M <- cells$M[k]
  rec <- searchnet_recovery(
    design$effects_spec,
    theta_true = c(rate = design$rate, density = design$density, outIso = th),
    reps = design$reps, M = M, N = design$N, waves = design$waves,
    nbrNodes = design$nbrNodes, seed = base_seed + k, n3 = design$n3,
    nsub = design$nsub, init_density = design$init_density, degree_bounds = b)
  rec$cell <- list(outIso = th, M = M)
  runs[[k]] <- rec
  message(sprintf("cell %d/%d: outIso %g, M %d: %s; median drops %g; %.0f s",
                  k, nrow(cells), th, M, .recovery_status_line(rec),
                  stats::median(rec$reps$floor_drops, na.rm = TRUE), rec$elapsed))
}
elapsed <- proc.time()[["elapsed"]] - t0

by_count  <- searchnet_floor_power(runs, by_true = FALSE)
by_true   <- searchnet_floor_power(runs, by_true = TRUE)
by_cell <- do.call(rbind, lapply(runs, function(r) {
  t <- r$table[r$table$effect == "outIso", ]
  data.frame(outIso = r$cell$outIso, M = r$cell$M,
             median_drops = stats::median(r$reps$floor_drops),
             n_non_identified = r$status_counts[["non_identified"]],
             n_not_converged = r$status_counts[["not_converged"]],
             n_error = r$status_counts[["error_simulation"]] +
               r$status_counts[["error_estimation"]],
             n_converged = r$n_converged,
             bias = t$bias, coverage = t$coverage, rejection = t$rejection,
             mde = t$mde, seconds = r$elapsed)
}))

out <- list(by_count = by_count, by_true = by_true, by_cell = by_cell,
            runs = runs, design = c(design, list(planted = planted, M = Ms,
                                                 base_seed = base_seed)),
            elapsed = elapsed,
            rsiena = as.character(utils::packageVersion("RSiena")),
            generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"))
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
saveRDS(out, "inst/extdata/searchnet_floor_power.rds", version = 2)

print(by_cell)
print(by_count)
print(by_true)
cat(sprintf("\nelapsed %.1f s\n", elapsed))
