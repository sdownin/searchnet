#!/usr/bin/env Rscript
## Regenerate inst/extdata/searchnet_recovery_vignette.rds, the precomputed
## results shown in vignettes/searchnet-recovery.Rmd.
##
## The runs take a few minutes, so the vignette does not evaluate them at build
## time; it loads this file. Settings and criteria are fixed here, before the
## run, and the vignette reports whatever they return.
##
## Usage (from the package root):
##   Rscript tools/make_recovery_vignette_data.R

pkgload::load_all(".", quiet = TRUE)

design <- list(
  effects_spec = c("density", "inPop"),
  M = 30L, N = 12L, waves = 3L, reps = 100L,
  nbrNodes = 1L, seed = 20261010L,
  n3 = 500L, nsub = 4L, init_density = 0.2
)
theta <- c(rate = 5, density = -1.5, inPop = 0.10)

recovery <- do.call(searchnet_recovery, c(design, list(
  theta_true = theta, focal = "inPop",
  criterion = list(max_abs_bias = 0.03, min_coverage = 0.90,
                   min_converged_share = 0.90))))

null_run <- do.call(searchnet_recovery, c(design, list(
  theta_true = theta, focal = "inPop", null = TRUE,
  criterion = list(max_size = 0.10, min_converged_share = 0.90))))

out <- list(recovery = recovery, null = null_run,
            generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"))
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
saveRDS(out, "inst/extdata/searchnet_recovery_vignette.rds", version = 2)

print(recovery)
print(null_run)
cat(sprintf("\nelapsed: recovery %.1f s, null %.1f s\n",
            recovery$elapsed, null_run$elapsed))
