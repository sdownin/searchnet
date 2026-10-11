#!/usr/bin/env Rscript
## Regenerate inst/extdata/searchnet_degree_bounds_recovery.rds, the
## precomputed results shown in vignettes/searchnet-degree-bounds.Rmd.
##
## One planted data-generating process with an actor floor of 1 (no actor may
## drop its last component), estimated twice on the SAME simulated panels:
## WITH the bound (fixed outIso penalty, method of moments) and WITHOUT it.
## The design, the criterion and the seed are fixed here before the run; the
## vignette reports whatever they return.
##
## Usage (from the package root):
##   Rscript tools/make_degree_bounds_vignette_data.R

pkgload::load_all(".", quiet = TRUE)

design <- list(
  effects_spec = c("density", "inPop"),
  theta_true = c(rate = 4, density = -1.8, inPop = 0.08),
  M = 30L, N = 10L, waves = 3L, reps = 100L,
  nbrNodes = 1L, seed = 20261010L,
  n3 = 500L, nsub = 4L, init_density = 0.12,
  degree_bounds = c(min = 1),
  criterion = list(max_abs_bias = 0.10, min_coverage = 0.90,
                   min_converged_share = 0.90),
  keep_panels = TRUE
)

with_bounds    <- do.call(searchnet_recovery, c(design, list(estimate_with_bounds = TRUE)))
without_bounds <- do.call(searchnet_recovery, c(design, list(estimate_with_bounds = FALSE)))

## How often the floor binds in the simulated data: the share of actors at
## degree 1 (the floor) and the mean actor degree, per wave, over replications.
## Identical in both arms (the panels are the same seeded draws).
stopifnot(identical(with_bounds$panels, without_bounds$panels))
P <- with_bounds$panels
floor_binding <- data.frame(
  wave = seq_len(design$waves),
  share_at_floor = rowMeans(sapply(P, function(p)
    vapply(p, function(B) mean(rowSums(B) == 1), numeric(1)))),
  share_isolated = rowMeans(sapply(P, function(p)
    vapply(p, function(B) mean(rowSums(B) == 0), numeric(1)))),
  mean_degree = rowMeans(sapply(P, function(p)
    vapply(p, function(B) mean(rowSums(B)), numeric(1)))))

## The panels are not stored: they are reproducible from the seed.
with_bounds$panels <- NULL
without_bounds$panels <- NULL

out <- list(with_bounds = with_bounds, without_bounds = without_bounds,
            floor_binding = floor_binding,
            design = design[setdiff(names(design), "keep_panels")],
            generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"))
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
saveRDS(out, "inst/extdata/searchnet_degree_bounds_recovery.rds", version = 2)

print(with_bounds)
print(without_bounds)
print(floor_binding)
cat(sprintf("\nelapsed: with bounds %.1f s, without bounds %.1f s\n",
            with_bounds$elapsed, without_bounds$elapsed))
