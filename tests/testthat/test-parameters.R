## test-parameters.R
## Tests for theta matrix construction, shock preprocessing, and shock application.
## Uses small networks and short iteration counts for speed.

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------
setup_env_with_effects <- function(M = 4, N = 6, seed = 42) {
  env <- SaomNkRSienaBiEnv$new(list(
    M = M, N = N, BI_PROB = 0.5,
    name = "param_test", rand_seed = seed
  ))

  sm <- list(
    dv_bipartite = list(
      name = "self$bipartite_rsienaDV",
      rates   = list(),
      effects = list(
        list(dv_name = "self$bipartite_rsienaDV",
             effect = "density", parameter = -1,
             interaction1 = "", interaction2 = ""),
        list(dv_name = "self$bipartite_rsienaDV",
             effect = "inPop", parameter = 0.5,
             interaction1 = "", interaction2 = "")
      ),
      coCovars      = list(),
      varCovars     = list(),
      coDyadCovars  = list(),
      varDyadCovars = list(),
      interactions  = list()
    )
  )

  # Wire up RSiena objects
  array_bi <- array(c(env$bipartite_matrix, env$bipartite_matrix),
                    dim = c(env$M, env$N, 2))
  env$bipartite_rsienaDV <- RSiena::sienaDependent(
    array_bi, type = "bipartite",
    nodeSet = c("ACTORS", "COMPONENTS"), allowOnly = FALSE
  )
  env$rsiena_data    <- env$get_rsiena_data_from_structure_model(sm)
  env$rsiena_effects <- RSiena::getEffects(env$rsiena_data)
  env$config_structure_model <- sm
  env$add_rsiena_effects(sm)

  list(env = env, sm = sm)
}

# ===========================================================================
# 1. get_theta_matrix: shape
# ===========================================================================
test_that("get_theta_matrix returns matrix with correct shape", {
  res <- setup_env_with_effects()
  env <- res$env

  input_effs <- data.frame(
    effect = c("density", "inPop"),
    stringsAsFactors = FALSE
  )
  iterations <- 20
  theta_mat <- env$get_theta_matrix(input_effs, iterations)

  expect_true(is.matrix(theta_mat))
  expect_equal(nrow(theta_mat), iterations)
  # At least 2 columns for our two effects
  expect_true(ncol(theta_mat) >= 2)
})

test_that("get_theta_matrix columns are named", {
  res <- setup_env_with_effects()
  theta_mat <- res$env$get_theta_matrix(
    data.frame(effect = c("density", "inPop"), stringsAsFactors = FALSE),
    10
  )
  expect_true(!is.null(colnames(theta_mat)))
  expect_true(all(nchar(colnames(theta_mat)) > 0))
})

test_that("get_theta_matrix rows are constant when no shocks", {
  res <- setup_env_with_effects()
  theta_mat <- res$env$get_theta_matrix(
    data.frame(effect = c("density", "inPop"), stringsAsFactors = FALSE),
    15
  )
  # Each column should have the same value in every row
  for (j in seq_len(ncol(theta_mat))) {
    expect_equal(length(unique(theta_mat[, j])), 1,
                 info = paste("Column", colnames(theta_mat)[j], "should be constant"))
  }
})

# ===========================================================================
# 2. preprocess_theta_shocks: valid input
# ===========================================================================
test_that("preprocess_theta_shocks assigns chain_step_ids", {
  res <- setup_env_with_effects()
  env <- res$env

  shocks <- list(
    list(effect = "density", parameter = -2, portion = 1),
    list(effect = "density", parameter = -0.5, portion = 1)
  )
  processed <- env$preprocess_theta_shocks(shocks, iterations = 20)

  expect_true(!is.null(processed[[1]]$chain_step_ids))
  expect_true(!is.null(processed[[2]]$chain_step_ids))
  # Step IDs should span 1:20
  all_ids <- sort(c(processed[[1]]$chain_step_ids, processed[[2]]$chain_step_ids))
  expect_equal(all_ids, 1:20)
})

test_that("preprocess_theta_shocks assigns effect_level", {
  res <- setup_env_with_effects()
  shocks <- list(
    list(effect = "density", parameter = -2, portion = 1)
  )
  processed <- res$env$preprocess_theta_shocks(shocks, iterations = 10)
  expect_true(!is.null(processed[[1]]$effect_level))
})

# ===========================================================================
# 3. preprocess_theta_shocks: error on empty list
# ===========================================================================
test_that("preprocess_theta_shocks errors on empty list", {
  res <- setup_env_with_effects()
  expect_error(res$env$preprocess_theta_shocks(list(), 10),
               "empty")
})

# ===========================================================================
# 4. preprocess_theta_shocks: unequal portions
# ===========================================================================
test_that("preprocess_theta_shocks handles unequal portions", {
  res <- setup_env_with_effects()
  shocks <- list(
    list(effect = "density", parameter = -2, portion = 1),
    list(effect = "density", parameter = -0.5, portion = 3)
  )
  processed <- res$env$preprocess_theta_shocks(shocks, iterations = 40)

  # portion=1 gets 10 steps, portion=3 gets 30 steps
  expect_equal(length(processed[[1]]$chain_step_ids), 10)
  expect_equal(length(processed[[2]]$chain_step_ids), 30)
})

# ===========================================================================
# 5. shock_theta_matrix: shocks applied to correct positions
# ===========================================================================
test_that("shock_theta_matrix modifies correct rows and columns", {
  res <- setup_env_with_effects()
  env <- res$env

  input_effs <- data.frame(effect = c("density", "inPop"),
                           stringsAsFactors = FALSE)
  theta_mat <- env$get_theta_matrix(input_effs, 20)

  shocks <- list(
    list(effect = "density", parameter = -3, portion = 1),
    list(effect = "density", parameter = 0,  portion = 1)
  )
  shocks <- env$preprocess_theta_shocks(shocks, 20)
  theta_shocked <- env$shock_theta_matrix(theta_mat, shocks)

  # First half (rows 1:10): density column should be -3
  density_col <- which(colnames(theta_shocked) == "density")
  if (length(density_col)) {
    expect_true(all(theta_shocked[1:10, density_col] == -3))
    # Second half (rows 11:20): density column should be 0
    expect_true(all(theta_shocked[11:20, density_col] == 0))
  }
})

test_that("shock_theta_matrix does not modify unshocked columns", {
  res <- setup_env_with_effects()
  env <- res$env

  input_effs <- data.frame(effect = c("density", "inPop"),
                           stringsAsFactors = FALSE)
  theta_mat <- env$get_theta_matrix(input_effs, 20)
  original_inpop <- theta_mat[, "inPop"]

  shocks <- list(
    list(effect = "density", parameter = -3, portion = 1),
    list(effect = "density", parameter = 0,  portion = 1)
  )
  shocks <- env$preprocess_theta_shocks(shocks, 20)
  theta_shocked <- env$shock_theta_matrix(theta_mat, shocks)

  # inPop column should be unchanged
  expect_equal(theta_shocked[, "inPop"], original_inpop)
})

# ===========================================================================
# 6. shock_theta_matrix errors without preprocessing
# ===========================================================================
test_that("shock_theta_matrix errors if effect_level not set", {
  res <- setup_env_with_effects()
  env <- res$env

  input_effs <- data.frame(effect = c("density", "inPop"),
                           stringsAsFactors = FALSE)
  theta_mat <- env$get_theta_matrix(input_effs, 10)

  # Raw shocks without preprocessing -- no effect_level set
  bad_shocks <- list(
    list(effect = "density", parameter = -3, portion = 1)
  )
  expect_error(env$shock_theta_matrix(theta_mat, bad_shocks),
               "effect_level")
})

# ===========================================================================
# 7. get_rsiena_effects_theta_df column checks
# ===========================================================================
test_that("theta_df has effect_key and effect_level columns", {
  res <- setup_env_with_effects()
  theta_df <- res$env$get_rsiena_effects_theta_df()
  expect_true("effect_key" %in% names(theta_df))
  expect_true("effect_level" %in% names(theta_df))
  expect_true("shortName" %in% names(theta_df))
  expect_true("parm" %in% names(theta_df))
})

test_that("theta_df excludes rate effects with no_rates=TRUE", {
  res <- setup_env_with_effects()
  theta_df <- res$env$get_rsiena_effects_theta_df(no_rates = TRUE)
  expect_false(any(grepl("rate", theta_df$shortName, ignore.case = TRUE)))
})

test_that("theta_df includes rate effects with no_rates=FALSE", {
  res <- setup_env_with_effects()
  theta_df <- res$env$get_rsiena_effects_theta_df(no_rates = FALSE)
  expect_true(any(grepl("Rate", theta_df$shortName)))
})
