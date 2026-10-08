###############################################################################
## test-chain-golden.R
## Golden outputs of ministep-chain processing.
##
## Written 2026-10-07 BEFORE the chain-processing performance work on the
## perf/chain-processing branch, from the code as it stood at dev aa00ad0.
## Every object the chain-processing path produces (the state array, the
## ministep frame with segment ids, the change log, the statistics, utility
## and K-4 tables, the final matrix, the path segments and the provenance
## minus its timestamp and version strings) is compared with identical()
## against tests/testthat/fixtures/chain_golden.rds. An optimization of that
## path must keep these identical, not merely close.
##
## Regenerate the fixture ONLY when a change to the path's results is
## intended and documented:  SEARCHNET_WRITE_CHAIN_GOLDEN=true, then run this
## file once.
##
## Regenerated 2026-10-08 (branch fix/k-degree-alignment) for an intended
## change: K_AA and K_CC exclude the node itself (every non-isolated node's
## value drops by 1), the NEW/OLD subsets no longer swap K_CA and K_CC, and
## K_CC_df carries its NEW/OLD strategy. Before rewriting, every object was
## compared with the old fixture: only the K tables (and K4_df / the
## multiwave K frames built from them) differed; the state arrays, change
## logs, statistics and utilities were identical.
###############################################################################

.golden_file <- function() test_path("fixtures", "chain_golden.rds")

## data.table carries an external-pointer attribute that never compares equal
## across objects (or after a save/load), so it is dropped before comparison.
## Everything else, including classes, factor levels and names, is kept.
.golden_strip <- function(x) {
  if (is.list(x) && !is.null(attr(x, ".internal.selfref")))
    attr(x, ".internal.selfref") <- NULL
  if (is.list(x) && !is.data.frame(x) && length(x))
    x[] <- lapply(x, .golden_strip)
  x
}

.golden_env_outputs <- function(env) {
  prov <- unclass(env$provenance)
  prov$timestamp <- NULL
  prov$searchnet_version <- NULL
  prov$RSiena_version <- NULL
  prov$R_version <- NULL
  out <- list(
    bi_env_arr         = env$bi_env_arr,
    bi_env_arr_initial = env$bi_env_arr_initial,
    bi_env_changes     = env$bi_env_changes,
    chain_stats        = env$chain_stats,
    actor_stats_df     = env$actor_stats_df,
    actor_util_df      = env$actor_util_df,
    actor_util_diff_df = env$actor_util_diff_df,
    K_AA_df = env$K_AA_df, K_AC_df = env$K_AC_df,
    K_CA_df = env$K_CA_df, K_CC_df = env$K_CC_df,
    K_AA_NEW_df = env$K_AA_NEW_df, K_AC_NEW_df = env$K_AC_NEW_df,
    K_CA_NEW_df = env$K_CA_NEW_df, K_CC_NEW_df = env$K_CC_NEW_df,
    K_AA_OLD_df = env$K_AA_OLD_df, K_AC_OLD_df = env$K_AC_OLD_df,
    K_CA_OLD_df = env$K_CA_OLD_df, K_CC_OLD_df = env$K_CC_OLD_df,
    K4_df              = env$get_K4_df(),
    final_matrix       = env$bipartite_matrix,
    path_start_matrix  = env$path_start_matrix,
    path_segments      = env$path_segments,
    theta_shocks_steps = lapply(env$theta_shocks, function(s) s$chain_step_ids),
    provenance         = prov
  )
  .golden_strip(out)
}

.golden_compute <- function() {
  res <- list()

  ## 1. Density only, empty start (every component NEW).
  e <- saomnk_env(M = 4, N = 6, seed = 11)
  invisible(capture.output(
    saomnk_run(e, saomnk_model(density = -0.5), steps_per_actor = 30, seed = 11)))
  res$density_only <- .golden_env_outputs(e)

  ## 2. Density + inPop + XWX, non-empty start (NEW and OLD components).
  e <- saomnk_env(M = 5, N = 6, density = 0.3, seed = 12)
  m <- saomnk_model(density = -0.5, popularity = 0.2,
                    influence_matrix = saomnk_block_diagonal(6, 2),
                    influence_weight = 0.3)
  invisible(capture.output(saomnk_run(e, m, steps_per_actor = 30, seed = 12)))
  res$inpop_xwx <- .golden_env_outputs(e)

  ## 3. egoX actor covariate.
  e <- saomnk_env(M = 5, N = 7, density = 0.2, seed = 13)
  m <- saomnk_model(density = -0.7, strategies = list(egoX = c(-1, 0, 1, -1, 1)),
                    influence_matrix = saomnk_block_diagonal(7, 2))
  invisible(capture.output(saomnk_run(e, m, steps_per_actor = 25, seed = 13)))
  res$egox <- .golden_env_outputs(e)

  ## 4. Two segments: a theta shock halfway through.
  e <- saomnk_env(M = 4, N = 6, density = 0.25, seed = 14)
  m <- saomnk_model(density = -0.5, popularity = 0.2)
  shocks <- list(saomnk_shock("density", parameter = -0.5, portion = 1),
                 saomnk_shock("density", parameter = -2.0, portion = 1))
  invisible(capture.output(
    saomnk_run(e, m, steps_per_actor = 20, seed = 14, shocks = shocks)))
  res$shock_two_segments <- .golden_env_outputs(e)

  ## 5. restart = FALSE: continue from the state model 1's settings reached.
  e <- saomnk_env(M = 4, N = 6, density = 0.2, seed = 15)
  m <- saomnk_model(density = -0.5, popularity = 0.1)
  invisible(capture.output(saomnk_run(e, m, steps_per_actor = 15, seed = 15)))
  invisible(capture.output(
    saomnk_run(e, m, steps_per_actor = 15, seed = 16, restart = FALSE)))
  res$restart_false <- .golden_env_outputs(e)

  ## 6. Multiwave, 2 waves, with an egoX covariate.
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = 4, N = 8, rand_seed = 220))
  invisible(capture.output({
    env$search_rsiena_multiwave_run(structure_model = make_strategy_structure_model(4),
                                    waves = 2, iterations_per_actor = 5, run_seed = 7)
    env$search_rsiena_multiwave_process_results()
  }))
  mw <- list(
    actor_wave_stats     = env$actor_wave_stats,
    actor_wave_util      = env$actor_wave_util,
    actor_wave_util_diff = env$actor_wave_util_diff,
    K_wave_A  = env$K_wave_A,  K_wave_B1 = env$K_wave_B1,
    K_wave_B2 = env$K_wave_B2, K_wave_C  = env$K_wave_C,
    bipartite_matrix_waves = env$bipartite_matrix_waves,
    last_wave = .golden_env_outputs(env)[c("bi_env_arr", "bi_env_changes",
                                           "chain_stats", "actor_util_df",
                                           "final_matrix")]
  )
  res$multiwave <- .golden_strip(mw)
  res
}

test_that("chain processing reproduces its golden outputs exactly", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  ## The fixture was produced under RSiena 1.5.0; another RSiena version may
  ## simulate a different chain, which is not a chain-processing change.
  skip_if(as.character(utils::packageVersion("RSiena")) != "1.5.0",
          "golden chain outputs were recorded under RSiena 1.5.0")

  got <- .golden_compute()

  if (identical(Sys.getenv("SEARCHNET_WRITE_CHAIN_GOLDEN"), "true")) {
    dir.create(dirname(.golden_file()), showWarnings = FALSE, recursive = TRUE)
    saveRDS(got, .golden_file(), version = 3)
    skip("golden chain outputs written")
  }
  skip_if_not(file.exists(.golden_file()), "golden fixture missing")
  want <- readRDS(.golden_file())

  expect_identical(names(got), names(want))
  for (model in names(want)) {
    expect_identical(names(got[[model]]), names(want[[model]]), label = model)
    for (obj in names(want[[model]])) {
      expect_identical(got[[model]][[obj]], want[[model]][[obj]],
                       label = sprintf("%s$%s", model, obj))
    }
  }
})
