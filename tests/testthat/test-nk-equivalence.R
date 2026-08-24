###############################################################################
## test-nk-equivalence.R
## Tests for verify_nk_equivalence method (Theorem 1 reduction)
###############################################################################

test_that("verify_nk_equivalence with N=4 identity E: max difference < 1e-10", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("psych")

  M <- 1
  N <- 4
  ## No simulation is needed here: compute_fitness_landscape() and
  ## verify_nk_equivalence() read only the constructed environment, and the
  ## simulation path is exactly what cannot exist at M = 1 (RSiena refuses
  ## single-actor bipartite data). Constructing directly is what lets the
  ## Theorem 1 reduction actually be tested at M = 1 instead of skipped.
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = M, N = N,
                                                         rand_seed = 600))

  ## Set influence matrix to identity (K=0 in NK terms)
  env$component_1_coDyadCovar <- diag(N)
  env$search_matrix <- diag(N)

  ## Compute fitness landscape
  tryCatch(
    env$compute_fitness_landscape(
      n_landscapes = 1,
      component_coCovar = NULL,
      normalize_int_mat = FALSE,
      project_int_mat = FALSE,
      component_value_sd = 0.1,
      verbose = FALSE
    ),
    error = function(e) stop(paste("compute_fitness_landscape failed:", e$message))
  )

  result <- tryCatch(
    env$verify_nk_equivalence(max_N = 12, landscape_id = 1),
    error = function(e) stop(paste("verify_nk_equivalence failed:", e$message))
  )

  expect_true(is.data.frame(result))
  expect_true(max(result$difference) < 1e-10,
              info = sprintf("Max difference should be < 1e-10 (got %.2e)",
                             max(result$difference)))
})


test_that("verify_nk_equivalence with N=4 block-diagonal E: max difference < 1e-10", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("psych")

  M <- 1
  N <- 4
  ## No simulation is needed here: compute_fitness_landscape() and
  ## verify_nk_equivalence() read only the constructed environment, and the
  ## simulation path is exactly what cannot exist at M = 1 (RSiena refuses
  ## single-actor bipartite data). Constructing directly is what lets the
  ## Theorem 1 reduction actually be tested at M = 1 instead of skipped.
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = M, N = N,
                                                         rand_seed = 601))

  ## Block-diagonal epistasis: dimensions 1-2 interact, 3-4 interact
  E_block <- diag(N)
  E_block[1, 2] <- 1; E_block[2, 1] <- 1
  E_block[3, 4] <- 1; E_block[4, 3] <- 1
  env$component_1_coDyadCovar <- E_block
  env$search_matrix <- E_block

  tryCatch(
    env$compute_fitness_landscape(
      n_landscapes = 1,
      component_coCovar = NULL,
      normalize_int_mat = FALSE,
      project_int_mat = FALSE,
      component_value_sd = 0.1,
      verbose = FALSE
    ),
    error = function(e) stop(paste("compute_fitness_landscape failed:", e$message))
  )

  result <- tryCatch(
    env$verify_nk_equivalence(max_N = 12, landscape_id = 1),
    error = function(e) stop(paste("verify_nk_equivalence failed:", e$message))
  )

  expect_true(is.data.frame(result))
  expect_true(max(result$difference) < 1e-10,
              info = sprintf("Max difference should be < 1e-10 with block-diagonal E (got %.2e)",
                             max(result$difference)))
})


test_that("verify_nk_equivalence errors when N > max_N", {
  skip_if_not_installed("RSiena")

  M <- 1
  N <- 6
  ## No simulation is needed here: compute_fitness_landscape() and
  ## verify_nk_equivalence() read only the constructed environment, and the
  ## simulation path is exactly what cannot exist at M = 1 (RSiena refuses
  ## single-actor bipartite data). Constructing directly is what lets the
  ## Theorem 1 reduction actually be tested at M = 1 instead of skipped.
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = M, N = N,
                                                         rand_seed = 602))

  ## Provide a fitness landscape so the error is about max_N, not missing landscape
  tryCatch(
    env$compute_fitness_landscape(
      n_landscapes = 1,
      component_coCovar = NULL,
      normalize_int_mat = TRUE,
      project_int_mat = FALSE,
      component_value_sd = 0.1,
      verbose = FALSE
    ),
    error = function(e) stop(paste("compute_fitness_landscape failed:", e$message))
  )

  ## max_N = 4 but N = 6, should error
  expect_error(
    env$verify_nk_equivalence(max_N = 4),
    regexp = "too large"
  )
})


test_that("verify_nk_equivalence returns data frame with correct columns", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("psych")

  M <- 1
  N <- 4
  ## No simulation is needed here: compute_fitness_landscape() and
  ## verify_nk_equivalence() read only the constructed environment, and the
  ## simulation path is exactly what cannot exist at M = 1 (RSiena refuses
  ## single-actor bipartite data). Constructing directly is what lets the
  ## Theorem 1 reduction actually be tested at M = 1 instead of skipped.
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = M, N = N,
                                                         rand_seed = 603))

  env$component_1_coDyadCovar <- diag(N)
  env$search_matrix <- diag(N)

  tryCatch(
    env$compute_fitness_landscape(
      n_landscapes = 1,
      component_coCovar = NULL,
      normalize_int_mat = FALSE,
      project_int_mat = FALSE,
      component_value_sd = 0.1,
      verbose = FALSE
    ),
    error = function(e) stop(paste("compute_fitness_landscape failed:", e$message))
  )

  result <- tryCatch(
    env$verify_nk_equivalence(max_N = 12, landscape_id = 1),
    error = function(e) stop(paste("verify_nk_equivalence failed:", e$message))
  )

  expect_true(is.data.frame(result))
  expected_cols <- c("config_id", "nk_fitness", "saomnk_utility", "difference")
  for (col in expected_cols) {
    expect_true(col %in% names(result),
                info = sprintf("Column '%s' must be present in result", col))
  }
  ## Should have 2^N rows
  expect_equal(nrow(result), 2^N,
               info = "Result should have 2^N rows")
})


test_that("M=1 simulation is refused with an actionable message", {
  skip_if_not_installed("RSiena")

  ## This test previously tried to run search_rsiena() at M=1 to check the NK
  ## greedy property. That is not something the package can do: RSiena's
  ## sienaDataCreate() does not support single-actor bipartite networks, and
  ## search_rsiena() refuses M < 2 deliberately. The failure was invisible
  ## because the error was caught and turned into a skip.
  ##
  ## What IS worth asserting is the refusal itself: it is a documented boundary
  ## of the package, and a silent change to it (crashing instead, or quietly
  ## accepting M=1 and producing nonsense) would be a real regression.
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = 1, N = 4,
                                                         rand_seed = 604))
  expect_error(
    env$search_rsiena(structure_model = make_minimal_structure_model(),
                      iterations_per_actor = 10, run_seed = 604, verbose = FALSE),
    regexp = "M >= 2|single-actor"
  )
})

## NOT YET COVERED: the NK greedy property (utility non-decreasing under
## theta = 0 and high beta) at M = 1. The test above cannot carry that claim,
## because the simulation path it needs does not exist for a single actor. The
## package's own error message names the route -- compute_fitness_landscape()
## and verify_nk_equivalence() remain available at M = 1 -- so the claim should
## be re-tested against the landscape rather than a ministep chain. Left as an
## explicit gap rather than deleted, so it is not mistaken for covered ground.
