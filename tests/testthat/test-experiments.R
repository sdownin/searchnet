###############################################################################
## test-experiments.R
## Tests for SaoMNKexperiments class
###############################################################################

## Load the experiments class file
tryCatch({
  experiments_file <- file.path(
    normalizePath("D:/Search_networks/SaoMNK", winslash = "/"),
    "R", "SAOM_NK_R6_experiments.R"
  )
  if (file.exists(experiments_file)) {
    source(experiments_file, local = FALSE)
  }
}, error = function(e) {
  message("Could not source experiments file: ", e$message)
})


test_that("SaoMNKexperiments initializes correctly", {
  skip_if(!exists("SaoMNKexperiments"),
          message = "SaoMNKexperiments class not available")

  M <- 3
  N <- 5
  params <- make_small_environ_params(M = M, N = N, rand_seed = 300)
  struct <- make_minimal_structure_model()
  steps <- 5

  exp <- SaoMNKexperiments$new(
    name = "test_experiment",
    n = 2,
    environ_params = params,
    structure_model = struct,
    steps_per_actor = steps,
    target_markets = 4:5,
    conf_level = 0.95,
    verbose_run = FALSE,
    rand_seed = 300
  )

  expect_s3_class(exp, "SaoMNKexperiments")
  expect_equal(exp$experiment_name, "test_experiment")
  expect_equal(exp$n_simulations, 2L)
})


test_that("batch_seeds are generated correctly", {
  skip_if(!exists("SaoMNKexperiments"),
          message = "SaoMNKexperiments class not available")

  M <- 3
  N <- 5
  params <- make_small_environ_params(M = M, N = N, rand_seed = 301)
  struct <- make_minimal_structure_model()

  exp <- SaoMNKexperiments$new(
    name = "seed_test",
    n = 10,
    environ_params = params,
    structure_model = struct,
    steps_per_actor = 3,
    rand_seed = 301
  )

  seeds <- exp$get_batch_seeds()
  expect_true(is.numeric(seeds))
  expect_length(seeds, 10)
  ## All seeds should be unique

  expect_equal(length(unique(seeds)), 10)
  ## Seeds should be positive integers in [1, 9999999]
  expect_true(all(seeds >= 1 & seeds <= 9999999))
})


test_that("batch_seeds are reproducible with same rand_seed", {
  skip_if(!exists("SaoMNKexperiments"),
          message = "SaoMNKexperiments class not available")

  M <- 3
  N <- 5
  params <- make_small_environ_params(M = M, N = N, rand_seed = 302)
  struct <- make_minimal_structure_model()

  exp1 <- SaoMNKexperiments$new(
    name = "repro_1", n = 5,
    environ_params = params, structure_model = struct,
    steps_per_actor = 3, rand_seed = 999
  )
  exp2 <- SaoMNKexperiments$new(
    name = "repro_2", n = 5,
    environ_params = params, structure_model = struct,
    steps_per_actor = 3, rand_seed = 999
  )

  expect_equal(exp1$get_batch_seeds(), exp2$get_batch_seeds())
})


test_that("result containers are initialized empty", {
  skip_if(!exists("SaoMNKexperiments"),
          message = "SaoMNKexperiments class not available")

  M <- 3
  N <- 5
  params <- make_small_environ_params(M = M, N = N, rand_seed = 303)
  struct <- make_minimal_structure_model()

  exp <- SaoMNKexperiments$new(
    name = "container_test", n = 2,
    environ_params = params, structure_model = struct,
    steps_per_actor = 3, rand_seed = 303
  )

  ## Before running simulations, results should be empty
  expect_true(is.list(exp$simulation_results))
  expect_length(exp$simulation_results, 0)
  expect_null(exp$aggregated_util_df)
  expect_null(exp$aggregated_entry_df)
  expect_null(exp$survival_analysis_data)
  expect_null(exp$market_entry_plot)
})


test_that("SaoMNKexperiments input validation works", {
  skip_if(!exists("SaoMNKexperiments"),
          message = "SaoMNKexperiments class not available")

  params <- make_small_environ_params(M = 3, N = 5)
  struct <- make_minimal_structure_model()

  ## n must be positive
  expect_error(
    SaoMNKexperiments$new(
      name = "bad_n", n = -1,
      environ_params = params, structure_model = struct,
      steps_per_actor = 3
    )
  )

  ## environ_params cannot be NULL
  expect_error(
    SaoMNKexperiments$new(
      name = "null_params", n = 2,
      environ_params = NULL, structure_model = struct,
      steps_per_actor = 3
    )
  )

  ## structure_model cannot be NULL
  expect_error(
    SaoMNKexperiments$new(
      name = "null_struct", n = 2,
      environ_params = params, structure_model = NULL,
      steps_per_actor = 3
    )
  )
})


test_that("process_results errors without running simulations first", {
  skip_if(!exists("SaoMNKexperiments"),
          message = "SaoMNKexperiments class not available")

  params <- make_small_environ_params(M = 3, N = 5)
  struct <- make_minimal_structure_model()

  exp <- SaoMNKexperiments$new(
    name = "no_run", n = 2,
    environ_params = params, structure_model = struct,
    steps_per_actor = 3
  )

  expect_error(exp$process_results(), "No simulation results")
})
