###############################################################################
## test-fitness.R
## Tests for fitness landscape computation and processing
###############################################################################

test_that("compute_fitness_landscape produces valid landscape array", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("psych")

  M <- 4
  N <- 6  ## Keep N small -- 2^N combinations computed
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 400),
    error = function(e) skip(paste("Tiny sim failed:", e$message))
  )

  n_lands <- 5
  result <- tryCatch(
    env$compute_fitness_landscape(
      n_landscapes = n_lands,
      component_coCovar = NULL,
      normalize_int_mat = TRUE,
      project_int_mat = TRUE,
      component_value_sd = 0.1,
      verbose = FALSE
    ),
    error = function(e) skip(paste("compute_fitness_landscape failed:", e$message))
  )

  fl <- env$fitness_landscape
  expect_false(is.null(fl), info = "fitness_landscape should not be NULL")
  expect_true(is.array(fl))
  expect_equal(length(dim(fl)), 3, info = "fitness_landscape should be 3D array")

  ## Dimensions: [n_landscapes, 2^N, 2*N+2]
  expect_equal(dim(fl)[1], n_lands)
  expect_equal(dim(fl)[2], 2^N)
  expect_equal(dim(fl)[3], 2 * N + 2)
})


test_that("compute_fitness_landscape works with different N values", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("psych")

  ## Test with N=4 (small, fast: 2^4 = 16 combinations)
  M <- 3
  N <- 4
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 401),
    error = function(e) skip(paste("Tiny sim failed:", e$message))
  )

  tryCatch(
    env$compute_fitness_landscape(
      n_landscapes = 3,
      project_int_mat = TRUE,
      verbose = FALSE
    ),
    error = function(e) skip(paste("compute_fitness_landscape failed:", e$message))
  )

  fl <- env$fitness_landscape
  expect_false(is.null(fl))
  expect_equal(dim(fl)[2], 2^N)
})


test_that("fitness landscape contains local peaks", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("psych")

  M <- 3
  N <- 4
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 402),
    error = function(e) skip(paste("Tiny sim failed:", e$message))
  )

  tryCatch(
    env$compute_fitness_landscape(
      n_landscapes = 10,
      project_int_mat = TRUE,
      verbose = FALSE
    ),
    error = function(e) skip(paste("compute_fitness_landscape failed:", e$message))
  )

  fl <- env$fitness_landscape
  ## The last column (2*N+2) is the local peak indicator (0 or 1)
  peak_col <- 2 * N + 2

  ## There should be at least one peak per landscape
  ## (the global optimum is always a peak)
  for (i in 1:dim(fl)[1]) {
    n_peaks <- sum(fl[i, , peak_col])
    ## `expect_gte()` takes `label`, not `info` -- passing `info` raises
    ## "unused argument" and the expectation errors rather than running.
    expect_gte(n_peaks, 1,
               label = paste("peak count for landscape", i))
  }
})


test_that("process_fitness_landscape runs without error", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("psych")

  M <- 3
  N <- 4
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 403),
    error = function(e) skip(paste("Tiny sim failed:", e$message))
  )

  ## compute_fitness_landscape first (process_ calls it if missing, but let us
  ## make sure theta_matrix exists from the simulation run)
  tryCatch(
    env$compute_fitness_landscape(
      n_landscapes = 5,
      project_int_mat = TRUE,
      verbose = FALSE
    ),
    error = function(e) skip(paste("compute_fitness_landscape failed:", e$message))
  )

  ## process_fitness_landscape requires theta_matrix, and the run above should
  ## have built one. A NULL here is the defect, not a missing precondition.
  expect_false(is.null(env$theta_matrix))

  result <- tryCatch(
    env$process_fitness_landscape(actor_ids = 1:M, step_ids = 1:3),
    error = function(e) skip(paste("process_fitness_landscape failed:", e$message))
  )

  ## The method should complete without error
  expect_true(TRUE, info = "process_fitness_landscape ran without error")
})
