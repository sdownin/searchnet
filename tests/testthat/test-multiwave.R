###############################################################################
## test-multiwave.R
## search_rsiena_multiwave_run(): argument names and wave-to-wave state
###############################################################################

## Regression (2026-10-07): the simulation vignette called
##   search_rsiena_multiwave_run(n_waves = 2, iterations_per_actor = 20,
##                               run_seed = 12345)
## using the argument names of search_rsiena(), and the multiwave route
## (waves, iterations, rand_seed) failed with "unused arguments". The
## search_rsiena() names are now accepted as well.
test_that("a 2-wave tiny multiwave runs with either argument naming", {
  skip_if_not_installed("RSiena")
  struct <- make_strategy_structure_model(4)

  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = 4, N = 8, rand_seed = 220))
  env$search_rsiena_multiwave_run(structure_model = struct, waves = 2,
                                  iterations_per_actor = 3, run_seed = 7)
  expect_length(env$rsiena_model_waves, 2L)
  expect_length(env$bipartite_matrix_waves, 2L)
  ## Wave 2 starts where wave 1 ended.
  expect_equal(unname(env$rsiena_model_waves[[2]]$searchnet_start),
               unname(matrix(as.numeric(env$bipartite_matrix_waves[[1]]), 4, 8)))

  ## Same run under the canonical names gives the same path.
  env2 <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = 4, N = 8, rand_seed = 220))
  env2$search_rsiena_multiwave_run(structure_model = struct, waves = 2,
                                   iterations = 12, rand_seed = 7)
  expect_equal(env2$bipartite_matrix_waves, env$bipartite_matrix_waves)

  ## Supplying both names of one argument is an error, not a silent choice.
  expect_error(
    env$search_rsiena_multiwave_run(structure_model = struct, waves = 1,
                                    iterations = 12, iterations_per_actor = 3),
    "not both")
  expect_error(
    env$search_rsiena_multiwave_run(structure_model = struct, waves = 1,
                                    rand_seed = 1, run_seed = 2),
    "not both")
})
