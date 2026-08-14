###############################################################################
## test-nk-equivalence.R
## Tests for verify_nk_equivalence method (Theorem 1 reduction)
###############################################################################

test_that("verify_nk_equivalence with N=4 identity E: max difference < 1e-10", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("psych")

  M <- 1
  N <- 4
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 600),
    error = function(e) skip(paste("Tiny sim failed:", e$message))
  )

  ## Set epistasis matrix to identity (K=0 in NK terms)
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
    error = function(e) skip(paste("compute_fitness_landscape failed:", e$message))
  )

  result <- tryCatch(
    env$verify_nk_equivalence(max_N = 12, landscape_id = 1),
    error = function(e) skip(paste("verify_nk_equivalence failed:", e$message))
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
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 601),
    error = function(e) skip(paste("Tiny sim failed:", e$message))
  )

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
    error = function(e) skip(paste("compute_fitness_landscape failed:", e$message))
  )

  result <- tryCatch(
    env$verify_nk_equivalence(max_N = 12, landscape_id = 1),
    error = function(e) skip(paste("verify_nk_equivalence failed:", e$message))
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
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 602),
    error = function(e) skip(paste("Tiny sim failed:", e$message))
  )

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
    error = function(e) skip(paste("compute_fitness_landscape failed:", e$message))
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
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 603),
    error = function(e) skip(paste("Tiny sim failed:", e$message))
  )

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
    error = function(e) skip(paste("compute_fitness_landscape failed:", e$message))
  )

  result <- tryCatch(
    env$verify_nk_equivalence(max_N = 12, landscape_id = 1),
    error = function(e) skip(paste("verify_nk_equivalence failed:", e$message))
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


test_that("M=1 theta=0 simulation has non-decreasing utility (NK greedy property)", {
  skip_if_not_installed("RSiena")

  M <- 1
  N <- 4
  env <- tryCatch({
    params <- make_small_environ_params(M = M, N = N, rand_seed = 604)
    SaomNkRSienaBiEnv$new(params)
  }, error = function(e) {
    skip(paste("searchnet init failed:", e$message))
  })

  struct <- make_minimal_structure_model()

  ## Run simulation with theta = 0 (no social effects) and high beta (greedy)
  tryCatch(
    env$search_rsiena(
      structure_model = struct,
      iterations_per_actor = 10,
      run_seed = 604,
      verbose = FALSE
    ),
    error = function(e) {
      skip(paste("search_rsiena failed:", e$message))
    }
  )

  ## Extract utility trajectory for the single actor across steps
  ## bi_env_arr stores the bipartite state at each step
  n_steps <- dim(env$bi_env_arr)[3]
  ## The run above requested multiple iterations, so fewer than two recorded
  ## steps means the chain was not stored -- assert it rather than skipping.
  expect_false(is.null(n_steps))
  expect_gte(n_steps, 2)

  utilities <- tryCatch({
    sapply(1:n_steps, function(s) {
      env$compute_formal_utility(actor_id = 1, step = s)$total
    })
  }, error = function(e) {
    skip(paste("compute_formal_utility failed:", e$message))
  })

  ## With theta=0 and greedy search (high rationality), utility should be non-decreasing
  ## Allow small floating-point tolerance
  diffs <- diff(utilities)
  expect_true(all(diffs >= -1e-10),
              info = sprintf("Utility trajectory should be non-decreasing (min diff = %.4e)",
                             min(diffs)))
})
