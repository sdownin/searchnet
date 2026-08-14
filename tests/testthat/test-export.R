###############################################################################
## test-export.R
## Tests for CSV export functions in searchnet-export.R
##
## Philosophy: "Users don't have to trust us -- they just trust the tests."
## These tests verify that export functions produce correctly structured files
## that downstream tools (Python/manim) can consume, and that pre-simulation
## calls fail informatively.
###############################################################################

## ---- Source the export layer ----
tryCatch(
  source(file.path(dir_r, "searchnet-export.R"), local = FALSE),
  error = function(e) {
    message("Could not source searchnet-export.R: ", e$message)
  }
)


# ===========================================================================
# 0. Internal helpers
# ===========================================================================
test_that(".defactor converts factor columns to character", {
  df <- data.frame(
    a = factor(c("x", "y")),
    b = c(1, 2),
    stringsAsFactors = TRUE
  )
  result <- .defactor(df)
  expect_true(is.character(result$a))
  expect_true(is.numeric(result$b))
})

test_that(".validate_env rejects non-SaomNkRSienaBiEnv object", {
  expect_error(.validate_env("not_an_env"), "must be a SaomNkRSienaBiEnv")
})

test_that(".validate_env gives informative error for missing fields", {
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params())
  expect_error(
    .validate_env(env, "K_AC_df"),
    "K_AC_df.*NULL"
  )
})


# ===========================================================================
# 1. searchnet_export_k4() -- K-4 degree trajectories
# ===========================================================================
test_that("searchnet_export_k4() creates a CSV file", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 300),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  ## A populated K_AC_df is the postcondition of the run above, so assert it
  ## rather than skipping: an empty one means the stats pass produced nothing.
  expect_false(is.null(env$K_AC_df))

  tmpfile <- tempfile(fileext = ".csv")
  on.exit(unlink(c(tmpfile, sub("\\.csv$", "_summary.csv", tmpfile))),
          add = TRUE)

  result <- tryCatch(
    searchnet_export_k4(env, file = tmpfile),
    error = function(e) stop(paste("export_k4 failed:", e$message))
  )

  expect_true(file.exists(tmpfile))
  ## Read and check structure
  df <- utils::read.csv(tmpfile, stringsAsFactors = FALSE)
  expect_true(nrow(df) > 0)
  expect_true(all(c("step", "K_type", "value") %in% names(df)))

  ## K_type should contain the four degree types
  k_types <- unique(df$K_type)
  expect_true(all(c("K_AC", "K_CA", "K_AA", "K_CC") %in% k_types))
})

test_that("searchnet_export_k4() also creates summary CSV", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 301),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )
  expect_false(is.null(env$K_AC_df))

  tmpfile <- tempfile(fileext = ".csv")
  summary_file <- sub("\\.csv$", "_summary.csv", tmpfile)
  on.exit(unlink(c(tmpfile, summary_file)), add = TRUE)

  tryCatch(
    searchnet_export_k4(env, file = tmpfile),
    error = function(e) stop(paste("export_k4 failed:", e$message))
  )

  expect_true(file.exists(summary_file))
  sdf <- utils::read.csv(summary_file, stringsAsFactors = FALSE)
  expect_true(all(c("step", "K_type", "mean_value", "sd_value") %in% names(sdf)))
})

test_that("searchnet_export_k4() errors before simulation", {
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params())
  expect_error(
    searchnet_export_k4(env),
    "NULL"
  )
})


# ===========================================================================
# 2. searchnet_export_snapshots() -- bipartite matrix snapshots
# ===========================================================================
test_that("searchnet_export_snapshots() creates CSV with correct columns", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 310),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )
  expect_false(is.null(env$bi_env_arr))

  tmpfile <- tempfile(fileext = ".csv")
  on.exit(unlink(tmpfile), add = TRUE)

  tryCatch(
    searchnet_export_snapshots(env, file = tmpfile),
    error = function(e) stop(paste("export_snapshots failed:", e$message))
  )

  expect_true(file.exists(tmpfile))
  df <- utils::read.csv(tmpfile, stringsAsFactors = FALSE)
  expect_true(all(c("step", "actor_id", "component_id", "tie") %in% names(df)))
  ## Ties should be 0 or 1
  expect_true(all(df$tie %in% c(0, 1)))
})

test_that("searchnet_export_snapshots() sparse mode omits zero-ties", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 311),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )
  expect_false(is.null(env$bi_env_arr))

  tmpfile <- tempfile(fileext = ".csv")
  on.exit(unlink(tmpfile), add = TRUE)

  tryCatch(
    searchnet_export_snapshots(env, file = tmpfile, sparse = TRUE),
    error = function(e) stop(paste("export failed:", e$message))
  )

  df <- utils::read.csv(tmpfile, stringsAsFactors = FALSE)
  ## In sparse mode, all rows should have tie == 1
  if (nrow(df) > 0) {
    expect_true(all(df$tie == 1))
  }
})

test_that("searchnet_export_snapshots() errors before simulation", {
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params())
  expect_error(searchnet_export_snapshots(env))
})


# ===========================================================================
# 3. searchnet_export_utility() -- actor utility trajectories
# ===========================================================================
test_that("searchnet_export_utility() creates a CSV", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 320),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )
  expect_false(is.null(env$actor_util_df))

  tmpfile <- tempfile(fileext = ".csv")
  summary_file <- sub("\\.csv$", "_summary.csv", tmpfile)
  on.exit(unlink(c(tmpfile, summary_file)), add = TRUE)

  tryCatch(
    searchnet_export_utility(env, file = tmpfile),
    error = function(e) stop(paste("export_utility failed:", e$message))
  )

  expect_true(file.exists(tmpfile))
  df <- utils::read.csv(tmpfile, stringsAsFactors = FALSE)
  expect_true(all(c("step", "actor_id", "utility") %in% names(df)))
  expect_true(nrow(df) > 0)
})

test_that("searchnet_export_utility() errors before simulation", {
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params())
  expect_error(
    searchnet_export_utility(env),
    "NULL"
  )
})


# ===========================================================================
# 4. searchnet_export_all() -- full export bundle
# ===========================================================================
test_that("searchnet_export_all() creates directory and multiple files", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 330),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )
  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_AC_df))
  expect_false(is.null(env$actor_util_df))

  tmpdir <- file.path(tempdir(), paste0("export_test_", Sys.getpid()))
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  result <- tryCatch(
    searchnet_export_all(env, dir = tmpdir),
    error = function(e) stop(paste("export_all failed:", e$message))
  )

  expect_true(dir.exists(tmpdir))
  expect_true(is.list(result))

  ## At least metadata.csv should exist
  expect_true(file.exists(result$metadata))

  ## metadata should contain M, N, n_chain_steps
  meta <- utils::read.csv(result$metadata, stringsAsFactors = FALSE)
  expect_true(all(c("parameter", "value") %in% names(meta)))
  param_names <- meta$parameter
  expect_true("M" %in% param_names)
  expect_true("N" %in% param_names)
})


# ===========================================================================
# 5. Exported CSVs are readable and have expected structure
# ===========================================================================
test_that("exported CSVs round-trip through read.csv correctly", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 340),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )
  expect_false(is.null(env$bi_env_arr))

  tmpfile <- tempfile(fileext = ".csv")
  on.exit(unlink(tmpfile), add = TRUE)

  tryCatch(
    searchnet_export_snapshots(env, steps = 1, file = tmpfile, sparse = FALSE),
    error = function(e) stop(paste("export failed:", e$message))
  )

  df <- utils::read.csv(tmpfile, stringsAsFactors = FALSE)
  ## For M=4, N=8, non-sparse, step 1 should have exactly 4*8=32 rows
  expect_equal(nrow(df), 4 * 8)
  expect_true(is.integer(df$step) || is.numeric(df$step))
  expect_true(is.integer(df$actor_id) || is.numeric(df$actor_id))
  expect_true(is.integer(df$component_id) || is.numeric(df$component_id))
})
