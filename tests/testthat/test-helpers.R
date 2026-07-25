## test-helpers.R
## Tests for utility / helper methods on SaomNkRSienaBiEnv_base

# ---------------------------------------------------------------------------
# Shared fixture: small environment
# ---------------------------------------------------------------------------
make_env <- function(M = 4, N = 6, BI_PROB = 0.5, seed = 123) {
  SaomNkRSienaBiEnv_base$new(list(
    M = M, N = N, BI_PROB = BI_PROB,
    name = "helper_test", rand_seed = seed
  ))
}

# ===========================================================================
# 1. get_jaccard_index
# ===========================================================================
test_that("jaccard of identical matrices equals 1", {
  env <- make_env()
  m <- matrix(c(1, 0, 1, 0, 1, 1), nrow = 2, ncol = 3)
  expect_equal(env$get_jaccard_index(m, m), 1)
})

test_that("jaccard of disjoint matrices equals 0", {
  env <- make_env()
  m0 <- matrix(c(1, 0, 1, 0), nrow = 2, ncol = 2)
  m1 <- matrix(c(0, 1, 0, 1), nrow = 2, ncol = 2)
  expect_equal(env$get_jaccard_index(m0, m1), 0)
})

test_that("jaccard of partial overlap is between 0 and 1", {
  env <- make_env()
  m0 <- matrix(c(1, 1, 0, 0), nrow = 2, ncol = 2)
  m1 <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  j <- env$get_jaccard_index(m0, m1)
  expect_true(j > 0 && j < 1)
})

test_that("jaccard of known example is correct", {
  # m0: {(1,1),(1,2)} = 2 ties
  # m1: {(1,1),(2,2)} = 2 ties
  # maintain = m0 AND m1 = {(1,1)} = 1
  # change = cells that differ = 2  (dropped (1,2), added (2,2))
  # J = 1 / (1 + 2) = 1/3
  env <- make_env()
  m0 <- matrix(c(1, 0, 1, 0), nrow = 2, ncol = 2)
  m1 <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  expect_equal(env$get_jaccard_index(m0, m1), 1 / 3)
})

test_that("jaccard of two all-zero matrices equals NaN (0/0)", {

  env <- make_env()
  m <- matrix(0, 2, 2)
  # maintain=0, change=0 => 0/0 = NaN
  expect_true(is.nan(env$get_jaccard_index(m, m)))
})

# ===========================================================================
# 2. exists()
# ===========================================================================
test_that("exists returns FALSE for NULL", {
  env <- make_env()
  expect_false(env$exists(NULL))
})

test_that("exists returns FALSE for NA", {
  env <- make_env()
  expect_false(env$exists(NA))
})

test_that("exists returns FALSE for NaN", {
  env <- make_env()
  expect_false(env$exists(NaN))
})

test_that("exists returns TRUE for a valid number", {
  env <- make_env()
  expect_true(env$exists(42))
})

test_that("exists returns TRUE for zero", {
  env <- make_env()
  expect_true(env$exists(0))
})

test_that("exists returns TRUE for a non-empty string", {
  env <- make_env()
  expect_true(env$exists("hello"))
})

# ===========================================================================
# 3. toggle (one-mode)
# ===========================================================================
test_that("toggle flips 0 to 1", {
  env <- make_env()
  m <- matrix(0, 3, 3)
  m2 <- env$toggle(m, 1, 2)
  expect_equal(m2[1, 2], 1)
})

test_that("toggle flips 1 to 0", {
  env <- make_env()
  m <- matrix(0, 3, 3)
  m[2, 3] <- 1
  m2 <- env$toggle(m, 2, 3)
  expect_equal(m2[2, 3], 0)
})

test_that("toggle does NOT flip diagonal (i == j)", {
  env <- make_env()
  m <- matrix(0, 3, 3)
  m2 <- env$toggle(m, 2, 2)
  expect_equal(m2[2, 2], 0)
})

test_that("toggle is self-inverse (double toggle restores)", {
  env <- make_env()
  m <- matrix(0, 3, 3)
  m2 <- env$toggle(env$toggle(m, 1, 3), 1, 3)
  expect_equal(m2[1, 3], 0)
})

test_that("toggle only modifies the target cell", {
  env <- make_env()
  m <- matrix(0, 3, 3)
  m_orig <- m
  m2 <- env$toggle(m, 1, 2)
  m2[1, 2] <- 0  # undo the expected change
  expect_identical(m2, m_orig)
})

# ===========================================================================
# 4. toggleBiMat (bipartite)
# ===========================================================================
test_that("toggleBiMat flips 0 to 1 in bipartite matrix", {
  env <- make_env()
  m <- matrix(0, nrow = 3, ncol = 5)
  m2 <- env$toggleBiMat(m, 2, 4)
  expect_equal(m2[2, 4], 1)
})

test_that("toggleBiMat flips 1 to 0 in bipartite matrix", {
  env <- make_env()
  m <- matrix(0, nrow = 3, ncol = 5)
  m[1, 3] <- 1
  m2 <- env$toggleBiMat(m, 1, 3)
  expect_equal(m2[1, 3], 0)
})

test_that("toggleBiMat allows diagonal (i == j)", {
  env <- make_env()
  m <- matrix(0, nrow = 3, ncol = 5)
  m2 <- env$toggleBiMat(m, 2, 2)
  expect_equal(m2[2, 2], 1)
})

test_that("toggleBiMat handles out-of-bounds gracefully", {
  env <- make_env()
  m <- matrix(0, nrow = 2, ncol = 3)
  # i out of bounds: should return unchanged matrix
  m2 <- env$toggleBiMat(m, 5, 1)
  expect_identical(m2, m)
  # j out of bounds
  m3 <- env$toggleBiMat(m, 1, 10)
  expect_identical(m3, m)
})

test_that("toggleBiMat is self-inverse", {
  env <- make_env()
  m <- matrix(0, nrow = 3, ncol = 4)
  m2 <- env$toggleBiMat(env$toggleBiMat(m, 1, 2), 1, 2)
  expect_equal(m2[1, 2], 0)
})

# ===========================================================================
# 5. increment_sim_iter
# ===========================================================================
test_that("increment_sim_iter increments by 1 by default", {
  env <- make_env()
  expect_equal(env$ITERATION, 0)
  env$increment_sim_iter()
  expect_equal(env$ITERATION, 1)
})

test_that("increment_sim_iter increments by custom value", {
  env <- make_env()
  env$increment_sim_iter(5)
  expect_equal(env$ITERATION, 5)
})

test_that("increment_sim_iter accumulates across calls", {
  env <- make_env()
  env$increment_sim_iter(3)
  env$increment_sim_iter(2)
  expect_equal(env$ITERATION, 5)
})
