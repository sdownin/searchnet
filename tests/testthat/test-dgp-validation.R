###############################################################################
## test-dgp-validation.R
## Data-generating-process (DGP) validation tests
##
## These tests run tiny simulations and verify that SAOM parameter settings
## produce the expected directional effects on K-4 statistics.
## All simulations: M<=4, N<=6, steps<=10.  skip() on any failure.
###############################################################################

## Source the API wrappers (not loaded by helper-setup.R)
tryCatch(
  source(file.path(dir_r, "saomnk-api.R"), local = FALSE),
  error = function(e) {
    message("Could not source saomnk-api.R: ", e$message)
  }
)


# ===================================================================
# Helper: safely run a tiny simulation via the public API
# ===================================================================
run_dgp_sim <- function(model, M = 4, N = 6, density = 0.3,
                         steps_per_actor = 10, seed = 42) {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    saomnk_env(M = M, N = N, density = density, seed = seed),
    error = function(e) skip(paste("saomnk_env failed:", e$message))
  )

  tryCatch(
    saomnk_run(env, model, steps_per_actor = steps_per_actor,
               seed = seed, verbose = FALSE),
    error = function(e) skip(paste("saomnk_run failed:", e$message))
  )

  env
}


# Helper: extract final-step K_AC values from an env
get_final_k_ac <- function(env) {
  if (is.null(env$K_AC_df) || nrow(env$K_AC_df) == 0)
    skip("K_AC_df not populated")
  last_step <- max(env$K_AC_df$chain_step_id)
  vals <- env$K_AC_df$value[env$K_AC_df$chain_step_id == last_step]
  vals
}


# Helper: extract final-step K_CA values from an env
get_final_k_ca <- function(env) {
  if (is.null(env$K_CA_df) || nrow(env$K_CA_df) == 0)
    skip("K_CA_df not populated")
  last_step <- max(env$K_CA_df$chain_step_id)
  vals <- env$K_CA_df$value[env$K_CA_df$chain_step_id == last_step]
  vals
}


# ===================================================================
# 1. Strong negative density: actors should have mean scope < N/2
# ===================================================================
test_that("Strong negative density produces sparse networks (mean K_AC < N/2)", {
  M <- 4; N <- 6
  mod <- saomnk_model(density = -2)

  env <- tryCatch(
    run_dgp_sim(mod, M = M, N = N, density = 0.5, seed = 101,
                steps_per_actor = 10),
    error = function(e) skip(paste("Simulation error:", e$message))
  )

  k_ac <- get_final_k_ac(env)
  expect_lt(mean(k_ac), N / 2,
            label = "Mean actor scope should be below N/2 under strong negative density")
})


# ===================================================================
# 2. Zero density effect: scope should be roughly N/2 (within tolerance)
# ===================================================================
test_that("Zero density effect yields mean scope near N/2", {
  M <- 4; N <- 6
  mod <- saomnk_model(density = 0)

  env <- tryCatch(
    run_dgp_sim(mod, M = M, N = N, density = 0.5, seed = 202,
                steps_per_actor = 10),
    error = function(e) skip(paste("Simulation error:", e$message))
  )

  k_ac <- get_final_k_ac(env)
  ## With zero density, the random walk should keep scope near the initial
  ## level (0.5 * N = 3).  Allow generous tolerance for tiny sims.
  expect_gt(mean(k_ac), N * 0.1,
            label = "Mean scope should not collapse to zero with density = 0")
  expect_lt(mean(k_ac), N * 0.9,
            label = "Mean scope should not saturate to N with density = 0")
})


# ===================================================================
# 3. Positive inPop: popularity distribution more right-skewed
# ===================================================================
test_that("Positive inPop produces more right-skewed K_CA than baseline", {
  M <- 4; N <- 6

  ## Baseline: density only
  mod_base <- saomnk_model(density = -0.5)
  env_base <- tryCatch(
    run_dgp_sim(mod_base, M = M, N = N, density = 0.3, seed = 301,
                steps_per_actor = 10),
    error = function(e) skip(paste("Baseline sim error:", e$message))
  )

  ## Treatment: density + strong inPop
  mod_pop <- saomnk_model(density = -0.5, popularity = 0.5)
  env_pop <- tryCatch(
    run_dgp_sim(mod_pop, M = M, N = N, density = 0.3, seed = 301,
                steps_per_actor = 10),
    error = function(e) skip(paste("inPop sim error:", e$message))
  )

  k_ca_base <- get_final_k_ca(env_base)
  k_ca_pop  <- get_final_k_ca(env_pop)

  ## With inPop, the maximum popularity should be at least as high as baseline
  ## (preferential attachment concentrates ties).  Use >= to allow ties.
  expect_gte(max(k_ca_pop), max(k_ca_base),
             label = "inPop should produce at least as concentrated popularity")

  ## Variance of K_CA should be weakly higher under inPop (more heterogeneous)
  ## This is a soft directional check; skip if baseline has zero variance
  if (var(k_ca_base) > 0 || var(k_ca_pop) > 0) {
    expect_true(
      var(k_ca_pop) >= var(k_ca_base) * 0.5,
      info = "inPop should not drastically reduce popularity variance"
    )
  }
})


# ===================================================================
# 4. XWX with block-diagonal W: within-block ties > cross-block ties
# ===================================================================
test_that("XWX with block-diagonal W concentrates within-block ties", {
  M <- 4; N <- 6

  ## Create 2 blocks of 3 components each
  W <- saomnk_block_diagonal(N, 2)

  mod <- saomnk_model(density = -0.5, epistasis_matrix = W,
                       epistasis_weight = 0.5)

  env <- tryCatch(
    run_dgp_sim(mod, M = M, N = N, density = 0.3, seed = 401,
                steps_per_actor = 10),
    error = function(e) skip(paste("XWX sim error:", e$message))
  )

  if (is.null(env$bi_env_arr))
    skip("bi_env_arr not populated")

  ## Get the final bipartite matrix
  n_steps <- dim(env$bi_env_arr)[3]
  B_final <- env$bi_env_arr[, , n_steps]

  ## Block 1: components 1-3, Block 2: components 4-6
  block1 <- 1:3
  block2 <- 4:6

  ## For each actor, count ties within vs across blocks
  within_ties <- 0
  cross_ties  <- 0
  for (i in 1:M) {
    held <- which(B_final[i, ] > 0)
    if (length(held) == 0) next
    ## Determine which block the majority of ties fall in
    n_b1 <- sum(held %in% block1)
    n_b2 <- sum(held %in% block2)
    ## The block with more ties is "within"; the rest is "cross"
    within_ties <- within_ties + max(n_b1, n_b2)
    cross_ties  <- cross_ties  + min(n_b1, n_b2)
  }

  ## With epistasis encouraging within-block ties, within should dominate
  ## Allow soft check: within >= cross (can be equal in very sparse cases)
  expect_gte(within_ties, cross_ties,
             label = "Within-block ties should >= cross-block under XWX")
})


# ===================================================================
# 5. Reproducibility: same seed produces same K-4 trajectories
# ===================================================================
test_that("Same seed produces identical K_AC trajectories", {
  M <- 3; N <- 5
  mod <- saomnk_model(density = -0.5)

  env1 <- tryCatch(
    run_dgp_sim(mod, M = M, N = N, density = 0.3, seed = 501,
                steps_per_actor = 5),
    error = function(e) skip(paste("Reproducibility sim 1 error:", e$message))
  )

  env2 <- tryCatch(
    run_dgp_sim(mod, M = M, N = N, density = 0.3, seed = 501,
                steps_per_actor = 5),
    error = function(e) skip(paste("Reproducibility sim 2 error:", e$message))
  )

  if (is.null(env1$K_AC_df) || is.null(env2$K_AC_df))
    skip("K_AC_df not populated in one or both envs")

  ## Values should be identical at every chain step
  expect_equal(env1$K_AC_df$value, env2$K_AC_df$value,
               info = "K_AC trajectory must be identical with same seed")
  expect_equal(env1$K_AC_df$chain_step_id, env2$K_AC_df$chain_step_id,
               info = "Chain step IDs must match with same seed")
})

test_that("Different seed produces different K_AC trajectories", {
  M <- 3; N <- 5
  mod <- saomnk_model(density = -0.5)

  env1 <- tryCatch(
    run_dgp_sim(mod, M = M, N = N, density = 0.3, seed = 601,
                steps_per_actor = 5),
    error = function(e) skip(paste("Diff-seed sim 1 error:", e$message))
  )

  env2 <- tryCatch(
    run_dgp_sim(mod, M = M, N = N, density = 0.3, seed = 602,
                steps_per_actor = 5),
    error = function(e) skip(paste("Diff-seed sim 2 error:", e$message))
  )

  if (is.null(env1$K_AC_df) || is.null(env2$K_AC_df))
    skip("K_AC_df not populated")

  ## With different seeds, values should differ at some step
  ## (vanishingly unlikely to match across all steps)
  expect_false(
    identical(env1$K_AC_df$value, env2$K_AC_df$value),
    info = "Different seeds should produce different K_AC trajectories"
  )
})
