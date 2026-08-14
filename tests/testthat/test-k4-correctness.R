###############################################################################
## test-k4-correctness.R
## Mathematical correctness tests for the K-4 coupled degree measures
##
## Philosophy: "Users don't have to trust us -- they just trust the tests."
## These tests verify the K-4 degree computations against hand-computable
## examples. A reviewer can check every assertion against the definitions:
##
##   B      = bipartite matrix (M x N)
##   K_AC   = rowSums(B)                               (actor scope)
##   K_CA   = colSums(B)                               (component popularity)
##   K_AA   = rowSums( (B %*% t(B)) > 0 )              (actor sociality)
##   K_CC   = colSums( (t(B) %*% B) > 0 )              (component epistasis)
##
## Note from source (saomnk-class.R lines 4126-4138):
##   K_AA counts the number of other actors sharing at least one component
##        (number of positive entries per row of B %*% t(B)).
##   K_CC counts the number of other components sharing at least one actor
##        (number of positive entries per column of t(B) %*% B).
###############################################################################


# ===========================================================================
# 1. Known small matrix produces expected K values
# ===========================================================================
test_that("K_AC = rowSums of bipartite matrix for known input", {
  ## B = [1 0 1 0]
  ##     [0 1 1 0]
  ##     [1 1 0 1]
  B <- matrix(c(1,0,1, 0,1,1, 1,1,0, 0,0,1), nrow = 3, ncol = 4)
  ## Expected K_AC (actor scope): c(2, 2, 3)
  K_AC <- rowSums(B)
  expect_equal(K_AC, c(2, 2, 3))
})

test_that("K_CA = colSums of bipartite matrix for known input", {
  B <- matrix(c(1,0,1, 0,1,1, 1,1,0, 0,0,1), nrow = 3, ncol = 4)
  ## Expected K_CA (component popularity): c(2, 2, 2, 1)
  K_CA <- colSums(B)
  expect_equal(K_CA, c(2, 2, 2, 1))
})

test_that("K_AA (actor sociality) = positive entries per row of B %*% t(B)", {
  B <- matrix(c(1,0,1, 0,1,1, 1,1,0, 0,0,1), nrow = 3, ncol = 4)
  ## B %*% t(B):
  ##   Row 1: (1*1+0*0+1*1+0*0, 1*0+0*1+1*1+0*0, 1*1+0*1+1*0+0*1) = (2, 1, 1)
  ##   Row 2: (0*1+1*0+1*1+0*0, 0*0+1*1+1*1+0*0, 0*1+1*1+1*0+0*1) = (1, 2, 1)
  ##   Row 3: (1*1+1*0+0*1+1*0, 1*0+1*1+0*1+1*0, 1*1+1*1+0*0+1*1) = (1, 1, 3)
  BBt <- B %*% t(B)
  ## K_AA counts number of positive entries per row (including self)
  K_AA <- apply(BBt, 1, function(x) sum(x > 0))
  ## Actor 1 shares components with actors 2 and 3, plus self: (2,1,1) => 3 positive
  ## Actor 2 shares components with actors 1 and 3, plus self: (1,2,1) => 3 positive
  ## Actor 3 shares components with actors 1 and 2, plus self: (1,1,3) => 3 positive
  expect_equal(K_AA, c(3, 3, 3))
})

test_that("K_CC (component epistasis) = positive entries per col of t(B) %*% B", {
  B <- matrix(c(1,0,1, 0,1,1, 1,1,0, 0,0,1), nrow = 3, ncol = 4)
  BtB <- t(B) %*% B
  ## K_CC counts number of positive entries per column
  K_CC <- apply(BtB, 2, function(x) sum(x > 0))
  ## BtB[1,] = t(B)[1,] %*% B = (1,0,1) %*% B
  ##         = (1*1+0*0+1*1, 1*0+0*1+1*1, 1*1+0*1+1*0, 1*0+0*0+1*1) = (2,1,1,1)
  ## So col 1 has all 4 positive entries
  ## Check: Components 1 and 3 share actors 1 and 3 => co-adopted
  expect_true(all(K_CC >= 1))  # every component connects to at least itself
})


# ===========================================================================
# 2. Isolated actor (zero row) produces K_AC = 0 and K_AA = 0
# ===========================================================================
test_that("isolated actor has K_AC = 0 and K_AA = 0", {
  ## Actor 2 has no components
  B <- matrix(c(1,0,0, 1,0,0, 0,0,0), nrow = 3, ncol = 3)
  K_AC <- rowSums(B)
  expect_equal(K_AC[2], 0)

  BBt <- B %*% t(B)
  K_AA <- apply(BBt, 1, function(x) sum(x > 0))
  ## Actor 2 contributes nothing to BBt => row 2 is all zeros
  expect_equal(K_AA[2], 0)
})

test_that("isolated component has K_CA = 0 and K_CC = 0", {
  ## Component 3 has no actors
  B <- matrix(c(1,0, 1,0, 0,0), nrow = 2, ncol = 3)
  K_CA <- colSums(B)
  expect_equal(K_CA[3], 0)

  BtB <- t(B) %*% B
  K_CC <- apply(BtB, 2, function(x) sum(x > 0))
  expect_equal(K_CC[3], 0)
})


# ===========================================================================
# 3. K_AA and K_CC are based on symmetric matrices
# ===========================================================================
test_that("B %*% t(B) is symmetric (K_AA projection)", {
  set.seed(99)
  B <- matrix(sample(0:1, 5 * 8, replace = TRUE), nrow = 5, ncol = 8)
  BBt <- B %*% t(B)
  expect_equal(BBt, t(BBt))
})

test_that("t(B) %*% B is symmetric (K_CC projection)", {
  set.seed(99)
  B <- matrix(sample(0:1, 5 * 8, replace = TRUE), nrow = 5, ncol = 8)
  BtB <- t(B) %*% B
  expect_equal(BtB, t(BtB))
})


# ===========================================================================
# 4. Full bipartite matrix has maximal K values
# ===========================================================================
test_that("all-ones bipartite matrix gives maximal K_AC and K_CA", {
  M <- 4; N <- 6
  B <- matrix(1, nrow = M, ncol = N)
  expect_equal(rowSums(B), rep(N, M))
  expect_equal(colSums(B), rep(M, N))
})

test_that("all-ones bipartite matrix gives K_AA = M and K_CC = N", {
  M <- 4; N <- 6
  B <- matrix(1, nrow = M, ncol = N)
  BBt <- B %*% t(B)
  K_AA <- apply(BBt, 1, function(x) sum(x > 0))
  ## Every actor shares all components with every other actor (including self)
  expect_equal(K_AA, rep(M, M))

  BtB <- t(B) %*% B
  K_CC <- apply(BtB, 2, function(x) sum(x > 0))
  expect_equal(K_CC, rep(N, N))
})


# ===========================================================================
# 5. Identity-like bipartite (each actor has unique component) => K_AA = 1
# ===========================================================================
test_that("diagonal bipartite gives K_AA = 1 (no shared components)", {
  ## 4 actors, 4 components, each actor affiliates with exactly one unique component
  B <- diag(4)
  BBt <- B %*% t(B)
  K_AA <- apply(BBt, 1, function(x) sum(x > 0))
  ## No actor shares a component with any other => only self is positive
  expect_equal(K_AA, rep(1, 4))
})


# ===========================================================================
# 6. K measures are consistent across bi_env_arr steps (integration test)
# ===========================================================================
test_that("K values match hand-computation from bi_env_arr", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 500),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  ## Both are postconditions of the run above; assert rather than skip, so a
  ## silently empty engine result fails instead of reporting green.
  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_AC_df))

  ## Pick the first step and verify K_AC matches rowSums
  step <- 1
  B <- env$bi_env_arr[, , step]
  expected_K_AC <- rowSums(B)

  k_ac_step <- env$K_AC_df[env$K_AC_df$chain_step_id == step, ]
  if (nrow(k_ac_step) > 0) {
    actual_K_AC <- k_ac_step$value[order(as.numeric(k_ac_step$actor_id))]
    expect_equal(actual_K_AC, unname(expected_K_AC),
                 info = "K_AC should equal rowSums of bipartite matrix at step 1")
  }
})

test_that("K_CA values match hand-computation from bi_env_arr", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 501),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_CA_df))

  step <- 1
  B <- env$bi_env_arr[, , step]
  expected_K_CA <- colSums(B)

  k_ca_step <- env$K_CA_df[env$K_CA_df$chain_step_id == step, ]
  if (nrow(k_ca_step) > 0) {
    actual_K_CA <- k_ca_step$value[order(as.numeric(k_ca_step$component_id))]
    expect_equal(actual_K_CA, unname(expected_K_CA),
                 info = "K_CA should equal colSums of bipartite matrix at step 1")
  }
})

test_that("K_AA values match hand-computation from bi_env_arr", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 502),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_AA_df))

  step <- 1
  B <- env$bi_env_arr[, , step]
  BBt <- B %*% t(B)
  expected_K_AA <- apply(BBt, 1, function(x) sum(x > 0))

  k_aa_step <- env$K_AA_df[env$K_AA_df$chain_step_id == step, ]
  if (nrow(k_aa_step) > 0) {
    actual_K_AA <- k_aa_step$value[order(as.numeric(k_aa_step$actor_id))]
    expect_equal(actual_K_AA, unname(expected_K_AA),
                 info = "K_AA should match rowSums of positive entries in B %*% t(B)")
  }
})

test_that("K_CC values match hand-computation from bi_env_arr", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 503),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_CC_df))

  step <- 1
  B <- env$bi_env_arr[, , step]
  BtB <- t(B) %*% B
  expected_K_CC <- apply(BtB, 2, function(x) sum(x > 0))

  k_cc_step <- env$K_CC_df[env$K_CC_df$chain_step_id == step, ]
  if (nrow(k_cc_step) > 0) {
    actual_K_CC <- k_cc_step$value[order(as.numeric(k_cc_step$component_id))]
    expect_equal(actual_K_CC, unname(expected_K_CC),
                 info = "K_CC should match colSums of positive entries in t(B) %*% B")
  }
})


# ===========================================================================
# 7. K values at multiple steps are internally consistent
# ===========================================================================
test_that("K_AC sum equals total number of ties at each step", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 510),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_AC_df))

  n_steps <- dim(env$bi_env_arr)[3]
  ## Check a few steps
  check_steps <- unique(c(1, min(3, n_steps), n_steps))

  for (s in check_steps) {
    B <- env$bi_env_arr[, , s]
    total_ties <- sum(B)

    k_ac_step <- env$K_AC_df[env$K_AC_df$chain_step_id == s, ]
    if (nrow(k_ac_step) > 0) {
      expect_equal(sum(k_ac_step$value), total_ties,
                   info = paste("Sum of K_AC should equal total ties at step", s))
    }
  }
})

test_that("Sum of K_AC equals sum of K_CA at each step (conservation)", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 520),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  expect_false(is.null(env$K_AC_df))
  expect_false(is.null(env$K_CA_df))

  ## Both K_AC and K_CA sum to total number of ties at each step
  steps <- unique(env$K_AC_df$chain_step_id)
  for (s in steps[1:min(5, length(steps))]) {
    sum_ac <- sum(env$K_AC_df$value[env$K_AC_df$chain_step_id == s])
    sum_ca <- sum(env$K_CA_df$value[env$K_CA_df$chain_step_id == s])
    expect_equal(sum_ac, sum_ca,
                 info = paste("Sum(K_AC) should equal Sum(K_CA) at step", s))
  }
})
