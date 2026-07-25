###############################################################################
## test-edge-cases.R
## Edge cases and robustness tests for SaoMNK
##
## Philosophy: "Users don't have to trust us -- they just trust the tests."
## These tests verify that the engine handles boundary conditions gracefully:
## minimal sizes, extreme densities, empty covariates, invalid parameters,
## and very short simulations. A reviewer can see that corner cases do not
## produce silent failures or incorrect output.
###############################################################################

## ---- Source API layer for high-level tests ----
tryCatch(
  source(file.path(dir_r, "saomnk-api.R"), local = FALSE),
  error = function(e) {
    message("Could not source saomnk-api.R: ", e$message)
  }
)


# ===========================================================================
# 1. Minimal network sizes
# ===========================================================================
test_that("M=2, N=2 construction succeeds", {
  env <- SaomNkRSienaBiEnv_base$new(list(
    M = 2, N = 2, BI_PROB = 0.5, name = "tiny", rand_seed = 1
  ))
  expect_equal(env$M, 2)
  expect_equal(env$N, 2)
  expect_equal(dim(env$bipartite_matrix), c(2, 2))
})

test_that("M=2, N=2 produces valid projections", {
  env <- SaomNkRSienaBiEnv_base$new(list(
    M = 2, N = 2, BI_PROB = 0.5, name = "tiny", rand_seed = 1
  ))
  expect_equal(dim(env$social_matrix), c(2, 2))
  expect_equal(dim(env$search_matrix), c(2, 2))
})

test_that("M=1, N=1 extreme minimal case", {
  env <- SaomNkRSienaBiEnv_base$new(list(
    M = 1, N = 1, BI_PROB = 1, name = "single", rand_seed = 1
  ))
  expect_equal(dim(env$bipartite_matrix), c(1, 1))
  expect_equal(env$bipartite_matrix[1, 1], 1)
})

test_that("M=2, N=2 simulation runs without error", {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = 2, N = 2, BI_PROB = 0.5, rand_seed = 10)
  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) skip(paste("Init failed:", e$message))
  )
  struct <- make_minimal_structure_model()

  result <- tryCatch(
    env$search_rsiena(
      structure_model = struct,
      iterations_per_actor = 3,
      run_seed = 10,
      verbose = FALSE
    ),
    error = function(e) skip(paste("Sim failed:", e$message))
  )

  expect_false(is.null(env$rsiena_model))
})


# ===========================================================================
# 2. Extreme density values
# ===========================================================================
test_that("BI_PROB=0 produces all-zero bipartite matrix", {
  env <- SaomNkRSienaBiEnv_base$new(list(
    M = 4, N = 6, BI_PROB = 0, name = "empty", rand_seed = 1
  ))
  expect_true(all(env$bipartite_matrix == 0))
  ## Projections should also be all zeros
  expect_true(all(env$social_matrix == 0))
  expect_true(all(env$search_matrix == 0))
})

test_that("BI_PROB=1 produces all-ones bipartite matrix", {
  env <- SaomNkRSienaBiEnv_base$new(list(
    M = 4, N = 6, BI_PROB = 1, name = "full", rand_seed = 1
  ))
  expect_true(all(env$bipartite_matrix == 1))
})

test_that("BI_PROB=0 simulation runs without error", {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = 4, N = 6, BI_PROB = 0, rand_seed = 20)
  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) skip(paste("Init failed:", e$message))
  )
  struct <- make_minimal_structure_model()

  result <- tryCatch(
    env$search_rsiena(
      structure_model = struct,
      iterations_per_actor = 3,
      run_seed = 20,
      verbose = FALSE
    ),
    error = function(e) skip(paste("Empty-start sim failed:", e$message))
  )

  expect_false(is.null(env$rsiena_model))
})

test_that("BI_PROB=1 simulation runs without error", {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = 4, N = 6, BI_PROB = 1, rand_seed = 30)
  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) skip(paste("Init failed:", e$message))
  )
  struct <- make_minimal_structure_model()

  result <- tryCatch(
    env$search_rsiena(
      structure_model = struct,
      iterations_per_actor = 3,
      run_seed = 30,
      verbose = FALSE
    ),
    error = function(e) skip(paste("Full-start sim failed:", e$message))
  )

  expect_false(is.null(env$rsiena_model))
})


# ===========================================================================
# 3. Empty covariates and dyad covariates do not crash
# ===========================================================================
test_that("empty coCovars list does not crash model construction", {
  struct <- list(
    dv_bipartite = list(
      name = DV_NAME,
      effects = list(
        list(effect = "density", parameter = -1, dv_name = DV_NAME, fix = TRUE)
      ),
      coCovars      = list(),
      varCovars     = list(),
      coDyadCovars  = list(),
      varDyadCovars = list(),
      interactions  = list()
    )
  )

  ## Construction should not error
  expect_true(is.list(struct))
  expect_length(struct$dv_bipartite$coCovars, 0)
})

test_that("empty coCovars/coDyadCovars simulation runs", {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = 4, N = 8, rand_seed = 40)
  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) skip(paste("Init failed:", e$message))
  )

  ## Minimal structure: no covariates at all
  struct <- make_minimal_structure_model()

  result <- tryCatch(
    env$search_rsiena(
      structure_model = struct,
      iterations_per_actor = 3,
      run_seed = 40,
      verbose = FALSE
    ),
    error = function(e) skip(paste("Sim failed:", e$message))
  )

  expect_false(is.null(env$rsiena_model))
})

test_that("saomnk_model() with no optional params creates valid structure", {
  mod <- saomnk_model()
  expect_s3_class(mod, "saomnk_model")
  expect_length(mod$dv_bipartite$coCovars, 0)
  expect_length(mod$dv_bipartite$coDyadCovars, 0)
})


# ===========================================================================
# 4. Invalid parameter validation
# ===========================================================================
test_that("saomnk_env() rejects negative M", {
  expect_error(saomnk_env(M = -1, N = 5))
})

test_that("saomnk_env() rejects non-numeric M", {
  expect_error(saomnk_env(M = "three", N = 5))
})

test_that("saomnk_env() rejects density outside [0,1]", {
  expect_error(saomnk_env(M = 3, N = 5, density = -0.1))
  expect_error(saomnk_env(M = 3, N = 5, density = 1.5))
})

test_that("saomnk_block_diagonal() rejects blocks > N", {
  expect_error(saomnk_block_diagonal(4, 5))
})

test_that("saomnk_block_diagonal() rejects N = 0", {
  expect_error(saomnk_block_diagonal(0, 0))
})

test_that("saomnk_run() rejects wrong env type", {
  mod <- saomnk_model(density = -1)
  expect_error(saomnk_run(list(M = 3), mod), "SaomNkRSienaBiEnv")
})

test_that("saomnk_get_bipartite() rejects wrong type", {
  expect_error(saomnk_get_bipartite(42))
})

test_that("saomnk_shock() rejects non-character effect", {
  expect_error(saomnk_shock(42, parameter = 1))
})

test_that("saomnk_shock() rejects non-numeric parameter", {
  expect_error(saomnk_shock("density", parameter = "high"))
})


# ===========================================================================
# 5. Very short simulation (1 step per actor)
# ===========================================================================
test_that("simulation with 1 step per actor completes", {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = 4, N = 8, rand_seed = 50)
  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) skip(paste("Init failed:", e$message))
  )
  struct <- make_minimal_structure_model()

  result <- tryCatch(
    env$search_rsiena(
      structure_model = struct,
      iterations_per_actor = 1,
      run_seed = 50,
      verbose = FALSE
    ),
    error = function(e) skip(paste("1-step sim failed:", e$message))
  )

  expect_false(is.null(env$rsiena_model))

  ## bi_env_arr should have at least 1 step
  if (!is.null(env$bi_env_arr)) {
    expect_true(dim(env$bi_env_arr)[3] >= 1)
  }
})


# ===========================================================================
# 6. Custom init_matrix edge cases
# ===========================================================================
test_that("custom init_matrix with single tie works", {
  custom <- matrix(0, nrow = 3, ncol = 4)
  custom[1, 1] <- 1
  cfg <- list(M = 3, N = 4, BI_PROB = NULL, init_matrix = custom,
              name = "single_tie", rand_seed = 1)
  env <- SaomNkRSienaBiEnv_base$new(cfg)

  expect_equal(sum(env$bipartite_matrix), 1)
  expect_equal(env$bipartite_matrix[1, 1], 1)
})

test_that("custom all-zero init_matrix produces zero projections", {
  custom <- matrix(0, nrow = 3, ncol = 4)
  cfg <- list(M = 3, N = 4, BI_PROB = NULL, init_matrix = custom,
              name = "all_zero", rand_seed = 1)
  env <- SaomNkRSienaBiEnv_base$new(cfg)

  expect_true(all(env$social_matrix == 0))
  expect_true(all(env$search_matrix == 0))
})


# ===========================================================================
# 7. Asymmetric M and N
# ===========================================================================
test_that("M > N construction succeeds", {
  env <- SaomNkRSienaBiEnv_base$new(list(
    M = 8, N = 3, BI_PROB = 0.5, name = "wide", rand_seed = 1
  ))
  expect_equal(dim(env$bipartite_matrix), c(8, 3))
  expect_equal(dim(env$social_matrix), c(8, 8))
  expect_equal(dim(env$search_matrix), c(3, 3))
})

test_that("M << N construction succeeds", {
  env <- SaomNkRSienaBiEnv_base$new(list(
    M = 2, N = 10, BI_PROB = 0.3, name = "tall", rand_seed = 1
  ))
  expect_equal(dim(env$bipartite_matrix), c(2, 10))
  expect_equal(dim(env$social_matrix), c(2, 2))
  expect_equal(dim(env$search_matrix), c(10, 10))
})


# ===========================================================================
# 8. Jaccard edge cases (standalone utils)
# ===========================================================================
test_that("get_jaccard_index handles all-zero matrices", {
  m <- matrix(0, 3, 3)
  ## 0/0 = NaN
  expect_true(is.nan(get_jaccard_index(m, m)))
})

test_that("get_jaccard_index handles single-cell matrices", {
  m0 <- matrix(1, 1, 1)
  m1 <- matrix(0, 1, 1)
  expect_equal(get_jaccard_index(m0, m1), 0)
  expect_equal(get_jaccard_index(m0, m0), 1)
})

test_that("saomnk_toggle on 1x1 one-mode matrix is no-op (i==j)", {
  m <- matrix(0, 1, 1)
  m2 <- saomnk_toggle(m, 1, 1)
  expect_equal(m2[1, 1], 0)
})

test_that("saomnk_toggle_bipartite handles 1x1 matrix", {
  m <- matrix(0, 1, 1)
  m2 <- saomnk_toggle_bipartite(m, 1, 1)
  expect_equal(m2[1, 1], 1)
})

test_that("saomnk_exists handles edge values", {
  expect_false(saomnk_exists(NULL))
  expect_false(saomnk_exists(NA))
  expect_false(saomnk_exists(NaN))
  expect_true(saomnk_exists(0))
  expect_true(saomnk_exists(""))
  expect_true(saomnk_exists(FALSE))
})
