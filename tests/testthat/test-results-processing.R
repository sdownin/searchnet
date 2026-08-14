###############################################################################
## test-results-processing.R
## Tests for output data structures after simulation
###############################################################################

test_that("actor_util_df is a data frame with expected columns after simulation", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 101),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  util_df <- env$actor_util_df

  expect_false(is.null(util_df), info = "actor_util_df should not be NULL after simulation")
  expect_true(is.data.frame(util_df))
  expect_gt(nrow(util_df), 0)

  ## Expect key columns to exist
  expected_cols <- c("actor_id", "chain_step_id", "utility")
  for (col in expected_cols) {
    expect_true(
      col %in% names(util_df),
      info = paste("actor_util_df should contain column:", col)
    )
  }
})


test_that("K4 statistics exist with correct structure after simulation", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 102),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  ## K_AA, K_AC, K_CA, K_CC should be data frames (or at least not NULL)
  k_fields <- c("K_AA_df", "K_AC_df", "K_CA_df", "K_CC_df")

  for (field in k_fields) {
    val <- env[[field]]
    ## At minimum they should not be NULL after a processed simulation
    expect_false(
      is.null(val),
      info = paste(field, "should not be NULL after simulation")
    )
    if (is.data.frame(val)) {
      expect_gt(nrow(val), 0)
    }
  }
})


test_that("chain_stats is populated after simulation", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 103),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  cs <- env$chain_stats
  expect_false(is.null(cs), info = "chain_stats should not be NULL")

  if (is.data.frame(cs)) {
    expect_gt(nrow(cs), 0)
  } else if (is.list(cs)) {
    expect_gt(length(cs), 0)
  }
})


test_that("get_bipartite_matrix_from_rsiena_model returns MxN matrix", {
  skip_if_not_installed("RSiena")

  M <- 4
  N <- 8
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 104),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  bi_mat <- tryCatch(
    env$get_bipartite_matrix_from_rsiena_model(),
    error = function(e) stop(paste("get_bipartite_matrix failed:", e$message))
  )

  expect_true(is.matrix(bi_mat))
  expect_equal(dim(bi_mat), c(M, N))
  ## Values should be 0 or 1 (binary bipartite ties)
  expect_true(all(bi_mat %in% c(0, 1)))
})


test_that("bi_env_arr is a 3D array after simulation", {
  skip_if_not_installed("RSiena")

  M <- 4
  N <- 8
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 105),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  arr <- env$bi_env_arr
  expect_false(is.null(arr), info = "bi_env_arr should not be NULL after simulation")
  expect_true(is.array(arr))
  expect_equal(length(dim(arr)), 3, info = "bi_env_arr should be a 3D array")
  ## First two dims should be M x N
  expect_equal(dim(arr)[1], M)
  expect_equal(dim(arr)[2], N)
  ## Third dimension should be > 0 (number of chain steps recorded)
  expect_gt(dim(arr)[3], 0)
})


test_that("theta_matrix is populated after simulation", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 106),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  tm <- env$theta_matrix
  expect_false(is.null(tm), info = "theta_matrix should not be NULL after simulation")
  expect_true(is.matrix(tm))
  ## Rows = iterations, Columns = number of parameters
  expect_gt(nrow(tm), 0)
  expect_gt(ncol(tm), 0)
})
