###############################################################################
## test-canonical-networks.R
## Canonical network samples -- pure matrix-math K-4 verification
##
## These tests require NO simulation.  Every expected value is derivable by
## hand from the definitions:
##
##   K_AC  = rowSums(B > 0)                     actor scope
##   K_CA  = colSums(B > 0)                     component popularity
##   S     = (B>0) %*% t(B>0); diag(S) <- 0
##   K_AA  = rowSums(S > 0)                     actor sociality (excl. self)
##   E     = t(B>0) %*% (B>0); diag(E) <- 0
##   K_CC  = rowSums(E > 0)                     component epistasis (excl. self)
##
## Conservation law:  sum(K_AC) == sum(K_CA) == total ties
###############################################################################


# ===================================================================
# Helper: compute K-4 from a raw bipartite matrix (excluding self)
# ===================================================================
compute_k4_from_matrix <- function(B) {
  B_bin <- (B > 0) * 1L
  K_AC <- rowSums(B_bin)
  K_CA <- colSums(B_bin)
  S <- B_bin %*% t(B_bin)
  diag(S) <- 0
  K_AA <- rowSums(S > 0)
  E <- t(B_bin) %*% B_bin
  diag(E) <- 0
  K_CC <- rowSums(E > 0)
  list(K_AC = K_AC, K_CA = K_CA, K_AA = K_AA, K_CC = K_CC)
}


# ===================================================================
# 1. Complete bipartite B = matrix(1, M, N)
# ===================================================================
test_that("Complete bipartite: K_AC = N for all actors", {
  M <- 4; N <- 6
  B <- matrix(1, nrow = M, ncol = N)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_AC, rep(N, M))
})

test_that("Complete bipartite: K_CA = M for all components", {
  M <- 4; N <- 6
  B <- matrix(1, nrow = M, ncol = N)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_CA, rep(M, N))
})

test_that("Complete bipartite: K_AA = M-1 (every pair shares all components)", {
  M <- 4; N <- 6
  B <- matrix(1, nrow = M, ncol = N)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_AA, rep(M - 1L, M))
})

test_that("Complete bipartite: K_CC = N-1 (every pair shares all actors)", {
  M <- 4; N <- 6
  B <- matrix(1, nrow = M, ncol = N)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_CC, rep(N - 1L, N))
})


# ===================================================================
# 2. Identity bipartite B = diag(N) with M = N
# ===================================================================
test_that("Identity bipartite: K_AC = 1 for all actors", {
  N <- 5
  B <- diag(N)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_AC, rep(1L, N))
})

test_that("Identity bipartite: K_CA = 1 for all components", {
  N <- 5
  B <- diag(N)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_CA, rep(1L, N))
})

test_that("Identity bipartite: K_AA = 0 (no shared components)", {
  N <- 5
  B <- diag(N)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_AA, rep(0L, N))
})

test_that("Identity bipartite: K_CC = 0 (no shared actors)", {
  N <- 5
  B <- diag(N)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_CC, rep(0L, N))
})


# ===================================================================
# 3. Empty bipartite B = matrix(0, M, N)
# ===================================================================
test_that("Empty bipartite: all K values are 0", {
  M <- 3; N <- 4
  B <- matrix(0, nrow = M, ncol = N)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_AC, rep(0L, M))
  expect_equal(k4$K_CA, rep(0L, N))
  expect_equal(k4$K_AA, rep(0L, M))
  expect_equal(k4$K_CC, rep(0L, N))
})


# ===================================================================
# 4. Single-row bipartite M = 1, b = (1,1,0,0)
# ===================================================================
test_that("Single-row bipartite: K_AC = 2, K_CA = {1,1,0,0}", {
  B <- matrix(c(1, 1, 0, 0), nrow = 1, ncol = 4)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_AC, 2L)
  expect_equal(k4$K_CA, c(1L, 1L, 0L, 0L))
})

test_that("Single-row bipartite: K_AA = 0 (only 1 actor)", {
  B <- matrix(c(1, 1, 0, 0), nrow = 1, ncol = 4)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(k4$K_AA, 0L)
})

test_that("Single-row bipartite: components sharing 1 actor have K_CC = 1", {
  B <- matrix(c(1, 1, 0, 0), nrow = 1, ncol = 4)
  k4 <- compute_k4_from_matrix(B)

  ## Components 1 and 2 share actor 1, so K_CC = 1 for each
  ## Components 3 and 4 are unheld, K_CC = 0
  expect_equal(k4$K_CC, c(1L, 1L, 0L, 0L))
})


# ===================================================================
# 5. Projection symmetry: B %*% t(B) and t(B) %*% B are symmetric
# ===================================================================
test_that("Actor projection B*B' is symmetric for random bipartite", {
  set.seed(314)
  B <- matrix(sample(0:1, 5 * 8, replace = TRUE), nrow = 5, ncol = 8)
  BBt <- B %*% t(B)
  expect_equal(BBt, t(BBt))
})

test_that("Component projection B'*B is symmetric for random bipartite", {
  set.seed(314)
  B <- matrix(sample(0:1, 5 * 8, replace = TRUE), nrow = 5, ncol = 8)
  BtB <- t(B) %*% B
  expect_equal(BtB, t(BtB))
})

test_that("Projection symmetry holds for non-square bipartite", {
  set.seed(271)
  B <- matrix(sample(0:1, 3 * 10, replace = TRUE), nrow = 3, ncol = 10)
  expect_equal(B %*% t(B), t(B %*% t(B)))
  expect_equal(t(B) %*% B, t(t(B) %*% B))
})


# ===================================================================
# 6. Conservation: sum(K_AC) == sum(K_CA) == total ties
# ===================================================================
test_that("Conservation law: sum(K_AC) == sum(K_CA) == total ties -- complete", {
  M <- 4; N <- 6
  B <- matrix(1, nrow = M, ncol = N)
  k4 <- compute_k4_from_matrix(B)

  total_ties <- sum(B > 0)
  expect_equal(sum(k4$K_AC), total_ties)
  expect_equal(sum(k4$K_CA), total_ties)
  expect_equal(sum(k4$K_AC), sum(k4$K_CA))
})

test_that("Conservation law: sum(K_AC) == sum(K_CA) == total ties -- identity", {
  N <- 5
  B <- diag(N)
  k4 <- compute_k4_from_matrix(B)

  total_ties <- sum(B > 0)
  expect_equal(sum(k4$K_AC), total_ties)
  expect_equal(sum(k4$K_CA), total_ties)
})

test_that("Conservation law: sum(K_AC) == sum(K_CA) == total ties -- empty", {
  B <- matrix(0, nrow = 3, ncol = 4)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(sum(k4$K_AC), 0L)
  expect_equal(sum(k4$K_CA), 0L)
})

test_that("Conservation law: sum(K_AC) == sum(K_CA) -- random matrix", {
  set.seed(628)
  B <- matrix(sample(0:1, 6 * 9, replace = TRUE), nrow = 6, ncol = 9)
  k4 <- compute_k4_from_matrix(B)

  total_ties <- sum(B > 0)
  expect_equal(sum(k4$K_AC), total_ties)
  expect_equal(sum(k4$K_CA), total_ties)
})

test_that("Conservation law: single-row bipartite", {
  B <- matrix(c(1, 1, 0, 0), nrow = 1, ncol = 4)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(sum(k4$K_AC), sum(k4$K_CA))
  expect_equal(sum(k4$K_AC), 2L)
})


# ===================================================================
# 7. Helper function matches explicit hand-computation
# ===================================================================
test_that("Helper matches hand-computed K-4 for a 3x4 matrix", {
  ## B = [1 0 1 0]
  ##     [0 1 1 0]
  ##     [1 1 0 1]
  B <- matrix(c(1, 0, 1,   # col 1
                0, 1, 1,   # col 2
                1, 1, 0,   # col 3
                0, 0, 1),  # col 4
              nrow = 3, ncol = 4)

  k4 <- compute_k4_from_matrix(B)

  ## K_AC: actor 1 has 2, actor 2 has 2, actor 3 has 3
  expect_equal(k4$K_AC, c(2, 2, 3))

  ## K_CA: col sums = c(2, 2, 2, 1)
  expect_equal(k4$K_CA, c(2, 2, 2, 1))

  ## S = B %*% t(B) with diag zeroed:
  ##   [-, 1, 1]   actor 1 shares with 2 (comp 3) and 3 (comp 1) => K_AA=2
  ##   [1, -, 1]   actor 2 shares with 1 (comp 3) and 3 (comp 2) => K_AA=2
  ##   [1, 1, -]   actor 3 shares with 1 (comp 1) and 2 (comp 2) => K_AA=2
  expect_equal(k4$K_AA, c(2, 2, 2))

  ## E = t(B) %*% B with diag zeroed:
  ## Comp 1 (actors 1,3) shares with comp 2 (via 3), comp 3 (via 1), comp 4 (via 3) => K_CC=3
  ## Comp 2 (actors 2,3) shares with comp 1 (via 3), comp 3 (via 2), comp 4 (via 3) => K_CC=3
  ## Comp 3 (actors 1,2) shares with comp 1 (via 1), comp 2 (via 2), comp 4 (no)    => K_CC=2
  ## Comp 4 (actor 3)    shares with comp 1 (via 3), comp 2 (via 3), comp 3 (no)    => K_CC=2
  expect_equal(k4$K_CC, c(3, 3, 2, 2))
})


# ===================================================================
# 8. Stress: larger random matrices still satisfy conservation
# ===================================================================
test_that("Conservation holds for M=10, N=15 random bipartite", {
  set.seed(999)
  B <- matrix(sample(0:1, 10 * 15, replace = TRUE, prob = c(0.6, 0.4)),
              nrow = 10, ncol = 15)
  k4 <- compute_k4_from_matrix(B)

  expect_equal(sum(k4$K_AC), sum(k4$K_CA))
  expect_equal(sum(k4$K_AC), sum(B > 0))
})

test_that("K_AA and K_CC are non-negative integers", {
  set.seed(777)
  B <- matrix(sample(0:1, 8 * 12, replace = TRUE), nrow = 8, ncol = 12)
  k4 <- compute_k4_from_matrix(B)

  expect_true(all(k4$K_AA >= 0))
  expect_true(all(k4$K_CC >= 0))
  expect_true(all(k4$K_AA == as.integer(k4$K_AA)))
  expect_true(all(k4$K_CC == as.integer(k4$K_CC)))
})

test_that("K_AA <= M-1 and K_CC <= N-1 (upper bounds excl. self)", {
  set.seed(555)
  M <- 6; N <- 10
  B <- matrix(sample(0:1, M * N, replace = TRUE), nrow = M, ncol = N)
  k4 <- compute_k4_from_matrix(B)

  expect_true(all(k4$K_AA <= M - 1))
  expect_true(all(k4$K_CC <= N - 1))
})
