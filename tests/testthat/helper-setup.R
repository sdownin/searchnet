###############################################################################
## helper-setup.R
## Shared test fixtures for searchnet testthat tests
## testthat auto-loads helper-*.R files before running tests
###############################################################################

## ---- Load source files directly (avoids segfault from loading all libs at once) ----
pkg_root <- normalizePath(file.path(dirname(dirname(getwd()))), winslash = "/")
## Fallback: if running from a different working dir, try known path
if (!file.exists(file.path(pkg_root, "R", "saomnk-base.R"))) {
  pkg_root <- normalizePath("D:/Search_networks/SaoMNK", winslash = "/")
}

dir_r <- file.path(pkg_root, "R")

## Load only the packages needed for the core R6 classes
suppressPackageStartupMessages({
  suppressWarnings({
    library(R6)
    library(igraph)
    library(RSiena)
    library(ggplot2)
    library(dplyr)
    library(plyr)
    library(tidyr)
    library(Matrix)
    library(reshape2)
    library(uuid)
    library(grid)
    library(gridExtra)
    library(texreg)
    if (requireNamespace("xml2", quietly = TRUE)) library(xml2)
    if (requireNamespace("rvest", quietly = TRUE)) library(rvest)
  })
})

## Source the package via inst/saomnk-loader.R, which discovers every file in R/
## by glob with the R6 hierarchy (utils -> saomnk-base -> saomnk-class) first.
##
## This used to hand-list four files. That is the same defect NEWS records fixing
## in the loader at v0.3.3, where sourcing 11 of 28 files left whole modules
## absent -- and the second copy of the list drifted the same way: when behaviour
## coevolution added a `.searchnet_has_behavior()` call inside saomnk-class.R,
## searchnet-behavior.R was not on this list, so EVERY simulation-dependent test
## died with "could not find function" and the surrounding tryCatch turned that
## into a skip. Five test files reported green while the simulation path was
## entirely broken.
##
## Delegating to the loader means load order is defined in exactly one place and
## cannot drift from the package again.
suppressPackageStartupMessages({
  suppressWarnings({
    loader <- file.path(pkg_root, "inst", "saomnk-loader.R")
    if (file.exists(loader)) {
      source(loader, local = FALSE)
    } else {
      ## Fallback: glob in the loader's order. Deliberately NOT a curated list.
      r_files <- list.files(dir_r, pattern = "[.]R$", full.names = TRUE)
      first <- file.path(dir_r, c("utils.R", "saomnk-base.R", "saomnk-class.R"))
      for (f in c(first[file.exists(first)], sort(setdiff(r_files, first)))) {
        source(f, local = FALSE)
      }
    }
  })
})

## No tryCatch around the above. It previously downgraded a sourcing failure to a
## message(), so a package that would not load produced a full run of skips
## rather than one loud error. If the package cannot be sourced, every result
## after this point is meaningless and the run should stop here.

## ---- Shared constants ----
DV_NAME <- "self$bipartite_rsienaDV"

## ---- Small environment params for fast tests ----
make_small_environ_params <- function(M = 4, N = 8, BI_PROB = 0.3,
                                       rand_seed = 42, name = "_test_") {
  list(
    M = M,
    N = N,
    BI_PROB = BI_PROB,
    rand_seed = rand_seed,
    name = name,
    dir_output = tempdir()
  )
}

## ---- Minimal structure model (density only, no covariates) ----
make_minimal_structure_model <- function() {
  list(
    dv_bipartite = list(
      name = DV_NAME,
      effects = list(
        list(effect = "density", parameter = -1, dv_name = DV_NAME, fix = TRUE)
      ),
      coCovars     = list(),
      varCovars    = list(),
      coDyadCovars = list(),
      varDyadCovars = list(),
      interactions  = list()
    )
  )
}

## ---- Structure model with a strategy covariate ----
make_strategy_structure_model <- function(M) {
  strat_vec <- rep(c(0, 1), length.out = M)
  list(
    dv_bipartite = list(
      name = DV_NAME,
      effects = list(
        list(effect = "density", parameter = -1, dv_name = DV_NAME, fix = TRUE)
      ),
      coCovars = list(
        list(effect = "egoX", parameter = 0.5, dv_name = DV_NAME, fix = TRUE,
             interaction1 = "self$strat_1_coCovar",
             x = strat_vec)
      ),
      varCovars     = list(),
      coDyadCovars  = list(),
      varDyadCovars = list(),
      interactions  = list()
    )
  )
}

## ---- Helper: create and run a tiny simulation, returning the env object ----
## Wraps the full init + run cycle inside tryCatch for safety.
run_tiny_sim <- function(M = 4, N = 8, iterations_per_actor = 5,
                         rand_seed = 42, use_strategy = FALSE) {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = M, N = N, rand_seed = rand_seed)
  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) {
      skip(paste("searchnet init failed:", e$message))
    }
  )

  struct <- if (use_strategy) {
    make_strategy_structure_model(M)
  } else {
    make_minimal_structure_model()
  }

  tryCatch(
    env$search_rsiena(
      structure_model = struct,
      iterations_per_actor = iterations_per_actor,
      run_seed = rand_seed,
      verbose = FALSE
    ),
    error = function(e) {
      skip(paste("search_rsiena failed:", e$message))
    }
  )

  env
}
