#!/usr/bin/env Rscript
## Regenerate inst/extdata/searchnet_composition_recovery.rds, the precomputed
## results shown in the "Actors who enter or leave" section of
## vignettes/searchnet-observed-data.Rmd.
##
## One planted design with leavers, estimated twice on the SAME panels (same
## seed): once with RSiena's composition change, once the naive way (absent
## actors read as rows of zeros, so a leaver looks like an actor dropping all
## its ties). Settings and criteria are fixed here, before the run, and the
## vignette reports whatever they return.
##
## Usage (from the package root):
##   Rscript tools/make_composition_vignette_data.R

pkgload::load_all(".", quiet = TRUE)

design <- list(
  effects_spec = c("density", "inPop"),
  theta_true   = c(rate = 5, density = -1.5, inPop = 0.10),
  M = 30L, N = 12L, waves = 3L, reps = 100L,
  nbrNodes = 1L, seed = 20261011L,
  n3 = 500L, nsub = 4L, init_density = 0.2,
  composition = list(leave = 0.2, enter = 0),
  criterion = list(max_abs_bias = c(density = 0.10, inPop = 0.03),
                   min_coverage = 0.90, min_converged_share = 0.90)
)

with_comp <- do.call(searchnet_recovery,
                     c(design, list(estimate_with_composition = TRUE)))
naive     <- do.call(searchnet_recovery,
                     c(design, list(estimate_with_composition = FALSE)))

out <- list(with_composition = with_comp, naive = naive, design = design,
            generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
            rsiena = as.character(utils::packageVersion("RSiena")))
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
saveRDS(out, "inst/extdata/searchnet_composition_recovery.rds", version = 2)

print(with_comp)
print(naive)
cat(sprintf("\nelapsed: with composition %.1f s, naive %.1f s\n",
            with_comp$elapsed, naive$elapsed))
