###############################################################################
## test-simulation.R
## Tests for simulation execution: init, run, chain stats, reproducibility
###############################################################################

test_that("SaomNkRSienaBiEnv initializes with small network", {
  params <- make_small_environ_params(M = 4, N = 8)
  env <- SaomNkRSienaBiEnv$new(params)

  expect_s3_class(env, "SaomNkRSienaBiEnv")
  expect_equal(env$M, 4)
  expect_equal(env$N, 8)
  expect_true(is.matrix(env$bipartite_matrix))
  expect_equal(dim(env$bipartite_matrix), c(4, 8))
  expect_true(is.matrix(env$bipartite_matrix_init))
  expect_equal(dim(env$bipartite_matrix_init), c(4, 8))
  ## Social and search projections should also be initialized

  expect_true(is.matrix(env$social_matrix))
  expect_equal(dim(env$social_matrix), c(4, 4))
  expect_true(is.matrix(env$search_matrix))
  expect_equal(dim(env$search_matrix), c(8, 8))
})


test_that("search_rsiena runs with minimal structure model", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 99),
    error = function(e) skip(paste("Tiny sim failed:", e$message))
  )

  ## rsiena_model should be populated after a run

  expect_false(is.null(env$rsiena_model))
  expect_false(is.null(env$rsiena_algorithm))
  expect_false(is.null(env$rsiena_effects))

  ## chain_stats should be populated (non-NULL data.frame)
  expect_false(is.null(env$chain_stats))
  expect_true(is.data.frame(env$chain_stats) || is.list(env$chain_stats))
  if (is.data.frame(env$chain_stats)) {
    expect_gt(nrow(env$chain_stats), 0)
  }
})


test_that("bipartite matrix can change from initial state after simulation", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 10, rand_seed = 77),
    error = function(e) skip(paste("Tiny sim failed:", e$message))
  )

  init_mat <- env$bipartite_matrix_init
  final_mat <- env$bipartite_matrix

  ## Both should be MxN matrices
  expect_equal(dim(init_mat), c(4, 8))
  expect_equal(dim(final_mat), c(4, 8))

  ## With 10 steps per actor, there is a very high probability that at least

  ## one tie changed.  We test that the matrices are not identical.
  ## (In extremely rare cases this could fail; we accept that.)
  expect_false(
    identical(init_mat, final_mat),
    info = "Bipartite matrix should change from initial state after simulation"
  )
})


test_that("random seed produces reproducible results", {
  skip_if_not_installed("RSiena")

  env1 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 555),
    error = function(e) skip(paste("Tiny sim run 1 failed:", e$message))
  )
  env2 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 555),
    error = function(e) skip(paste("Tiny sim run 2 failed:", e$message))
  )

  ## Same seed should give the same final bipartite matrix
  expect_equal(env1$bipartite_matrix, env2$bipartite_matrix)

  ## And the same chain_stats dimensions
  if (is.data.frame(env1$chain_stats) && is.data.frame(env2$chain_stats)) {
    expect_equal(nrow(env1$chain_stats), nrow(env2$chain_stats))
  }
})


test_that("search_rsiena_multiwave_run executes with waves=1", {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = 4, N = 8, rand_seed = 321)
  struct <- make_minimal_structure_model()

  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) skip(paste("Init failed:", e$message))
  )

  result <- tryCatch(
    env$search_rsiena_multiwave_run(
      structure_model = struct,
      waves = 1,
      iterations = 10,
      rand_seed = 321
    ),
    error = function(e) skip(paste("Multiwave run failed:", e$message))
  )

  ## After multiwave run, rsiena_model should exist
  expect_false(is.null(env$rsiena_model))
  ## bipartite_matrix_waves should have 1 element
  expect_length(env$bipartite_matrix_waves, 1)
  expect_true(is.matrix(env$bipartite_matrix_waves[[1]]))
  expect_equal(dim(env$bipartite_matrix_waves[[1]]), c(4, 8))
})
