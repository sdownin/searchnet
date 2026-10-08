###############################################################################
## test-shocks.R
## Tests for intervention/shock analysis (theta_shocks, preprocess, utility)
###############################################################################

test_that("preprocess_theta_shocks correctly structures shock schedule", {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = 4, N = 8, rand_seed = 200)
  struct <- make_strategy_structure_model(params$M)

  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) stop(paste("Init failed:", e$message))
  )

  ## Must set config_structure_model before preprocessing shocks
  env$config_structure_model <- struct

  ## Define theta shocks: two phases (baseline off, then on)
  theta_shocks <- list(
    list(
      effect = "egoX",
      parameter = 0,
      portion = 1,
      shock_on = 0,
      label = "baseline"
    ),
    list(
      effect = "egoX",
      parameter = 1.5,
      portion = 1,
      shock_on = 1,
      label = "treatment"
    )
  )

  iterations <- 20
  processed <- tryCatch(
    env$preprocess_theta_shocks(theta_shocks, iterations),
    error = function(e) stop(paste("preprocess_theta_shocks failed:", e$message))
  )

  expect_true(is.list(processed))
  expect_length(processed, 2)

  ## Each shock should now have chain_step_ids
  expect_false(is.null(processed[[1]]$chain_step_ids))
  expect_false(is.null(processed[[2]]$chain_step_ids))

  ## chain_step_ids should partition the iterations range
  all_ids <- sort(c(processed[[1]]$chain_step_ids, processed[[2]]$chain_step_ids))
  expect_equal(all_ids, 1:iterations)

  ## Each shock should have effect_level set
  expect_false(is.null(processed[[1]]$effect_level))
  expect_false(is.null(processed[[2]]$effect_level))
})


test_that("preprocess_theta_shocks handles effect_level input", {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = 4, N = 8, rand_seed = 201)
  struct <- make_strategy_structure_model(params$M)

  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) stop(paste("Init failed:", e$message))
  )
  env$config_structure_model <- struct

  ## Provide effect_level directly instead of effect
  theta_shocks <- list(
    list(
      effect_level = "egoX",
      parameter = 0,
      portion = 1,
      shock_on = 0,
      label = "baseline"
    ),
    list(
      effect_level = "egoX",
      parameter = 2.0,
      portion = 1,
      shock_on = 1,
      label = "shock"
    )
  )

  processed <- tryCatch(
    env$preprocess_theta_shocks(theta_shocks, 30),
    error = function(e) stop(paste("preprocess_theta_shocks failed:", e$message))
  )

  expect_true(is.list(processed))
  expect_length(processed, 2)
  ## Should have both effect and effect_level populated
  expect_false(is.null(processed[[1]]$effect))
  expect_false(is.null(processed[[1]]$effect_level))
})


test_that("search_rsiena with theta_shocks runs without error", {
  skip_if_not_installed("RSiena")

  M <- 4
  N <- 8
  params <- make_small_environ_params(M = M, N = N, rand_seed = 202)
  struct <- make_strategy_structure_model(M)

  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) stop(paste("Init failed:", e$message))
  )

  theta_shocks <- list(
    list(
      effect = "egoX",
      parameter = 0,
      portion = 1,
      shock_on = 0,
      label = "pre"
    ),
    list(
      effect = "egoX",
      parameter = 1.0,
      portion = 1,
      shock_on = 1,
      label = "post"
    )
  )

  result <- tryCatch(
    env$search_rsiena(
      structure_model = struct,
      iterations_per_actor = 5,
      theta_shocks = theta_shocks,
      run_seed = 202,
      verbose = FALSE
    ),
    error = function(e) stop(paste("search_rsiena with shocks failed:", e$message))
  )

  ## Shocks should be stored on the object
  expect_false(is.null(env$theta_shocks))
  expect_true(is.list(env$theta_shocks))
  expect_length(env$theta_shocks, 2)

  ## theta_matrix should reflect shock schedule
  expect_false(is.null(env$theta_matrix))
  expect_true(is.matrix(env$theta_matrix))
})


test_that("search_rsiena with NULL theta_shocks is a no-op for shocks", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 203),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  ## theta_shocks should remain NULL when not provided
  expect_null(env$theta_shocks)
  ## But the model should still run fine
  expect_false(is.null(env$rsiena_model))
})


test_that("compute_utility_shocks runs without error when shocks are set", {
  skip_if_not_installed("RSiena")

  M <- 4
  N <- 8
  params <- make_small_environ_params(M = M, N = N, rand_seed = 204)
  struct <- make_strategy_structure_model(M)

  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) stop(paste("Init failed:", e$message))
  )

  theta_shocks <- list(
    list(
      effect = "egoX",
      parameter = 0,
      portion = 1,
      shock_on = 0,
      label = "control"
    ),
    list(
      effect = "egoX",
      parameter = 1.0,
      portion = 1,
      shock_on = 1,
      label = "treatment"
    )
  )

  tryCatch(
    env$search_rsiena(
      structure_model = struct,
      iterations_per_actor = 5,
      theta_shocks = theta_shocks,
      run_seed = 204,
      verbose = FALSE
    ),
    error = function(e) stop(paste("search_rsiena with shocks failed:", e$message))
  )

  ## compute_utility_shocks should run without error
  result <- tryCatch(
    env$compute_utility_shocks(verbose = FALSE),
    error = function(e) {
      ## Was a skip on the guess that failure "is acceptable if" the tiny model
      ## lacks required structure. Unverified, and it swallowed every other
      ## cause with it. Name and check the precondition, or let it fail.
      stop(paste("compute_utility_shocks failed:", e$message))
    }
  )

  ## If it succeeds, the return should be invisible(self) or NULL
  ## The key assertion is that it ran without error
  expect_true(TRUE)
})


## Regression (2026-10-07): the simulation vignette passed
##   list(effect_level = "density_dv_bipartite", new_parameter = -2, portion = 1)
## and the engine ran it as an unshocked model: the effect_level matched no
## theta column and `new_parameter` is not a field the engine reads, and
## shock_theta_matrix() skipped both without a word.
test_that("a malformed theta_shocks spec errors instead of being ignored", {
  skip_if_not_installed("RSiena")
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = 4, N = 8, rand_seed = 210))
  struct <- make_strategy_structure_model(4)

  bad_level <- list(list(effect_level = "density_dv_bipartite", parameter = -2, portion = 1),
                    list(effect_level = "density_dv_bipartite", parameter = -2, portion = 1))
  expect_error(
    env$search_rsiena(struct, iterations_per_actor = 4, run_seed = 1,
                      theta_shocks = bad_level),
    "matches no theta column")

  bad_field <- list(list(effect_level = "density", parameter = -1, portion = 1),
                    list(effect_level = "density", new_parameter = -2, portion = 1))
  expect_error(
    env$search_rsiena(struct, iterations_per_actor = 4, run_seed = 1,
                      theta_shocks = bad_field),
    "new_parameter")

  one_seg <- list(list(effect_level = "density", parameter = -2, portion = 1))
  expect_warning(
    env$search_rsiena(struct, iterations_per_actor = 4, run_seed = 1,
                      theta_shocks = one_seg),
    "one entry")
})

test_that("a well-formed two-segment shock reaches the theta grid and the path", {
  skip_if_not_installed("RSiena")
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = 4, N = 8, rand_seed = 211))
  struct <- make_strategy_structure_model(4)
  shocks <- list(list(effect_level = "density", parameter = -1, portion = 1),
                 list(effect_level = "density", parameter = -2, portion = 1))
  env$search_rsiena(struct, iterations_per_actor = 6, run_seed = 1,
                    theta_shocks = shocks)
  dens <- env$theta_matrix[, "density"]
  n <- length(dens)
  expect_true(all(dens[seq_len(n / 2)] == -1))
  expect_true(all(dens[(n / 2 + 1):n] == -2))
  ## The break is a segment boundary of the genuine path.
  expect_gte(nrow(env$path_segments), 2L)
})
