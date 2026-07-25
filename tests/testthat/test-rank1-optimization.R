###############################################################################
## test-rank1-optimization.R
## Rank-1 incremental update optimization tests
##
## These tests verify that the optimized rank-1 incremental updates
## produce identical results to full matrix recomputation.
##
## No simulation required -- pure matrix algebra verification.
###############################################################################

context("Rank-1 projection update optimization")

# These tests verify that the optimized rank-1 incremental updates
# produce identical results to full matrix recomputation.

test_that("rank-1 social projection update is correct", {
  # Create a small bipartite matrix
  set.seed(42)
  M <- 5; N <- 8
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.6, 0.4)), M, N)

  # Full computation
  S_full <- B %*% t(B)
  diag(S_full) <- 0

  # Toggle dyad (2,3) and recompute
  delta <- if (B[2,3] == 0) 1L else -1L
  B[2,3] <- B[2,3] + delta
  S_updated <- B %*% t(B)
  diag(S_updated) <- 0

  # Rank-1 update
  S_rank1 <- S_full
  S_rank1[2, ] <- S_rank1[2, ] + delta * B[, 3]
  S_rank1[, 2] <- S_rank1[, 2] + delta * B[, 3]
  S_rank1[2, 2] <- 0

  expect_equal(S_rank1, S_updated)
})

test_that("rank-1 epistasis projection update is correct", {
  set.seed(42)
  M <- 5; N <- 8
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.6, 0.4)), M, N)

  E_full <- t(B) %*% B
  diag(E_full) <- 0

  delta <- if (B[3,5] == 0) 1L else -1L
  B[3,5] <- B[3,5] + delta
  E_updated <- t(B) %*% B
  diag(E_updated) <- 0

  E_rank1 <- E_full
  E_rank1[5, ] <- E_rank1[5, ] + delta * B[3, ]
  E_rank1[, 5] <- E_rank1[, 5] + delta * B[3, ]
  E_rank1[5, 5] <- 0

  expect_equal(E_rank1, E_updated)
})

test_that("incremental rowSums/colSums are correct", {
  set.seed(42)
  M <- 5; N <- 8
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.6, 0.4)), M, N)
  rs <- rowSums(B)
  cs <- colSums(B)

  # Toggle (2,3)
  delta <- if (B[2,3] == 0) 1L else -1L
  B[2,3] <- B[2,3] + delta
  rs[2] <- rs[2] + delta
  cs[3] <- cs[3] + delta

  expect_equal(rs, rowSums(B))
  expect_equal(cs, colSums(B))
})

test_that("rank-1 updates work across multiple sequential toggles", {
  set.seed(123)
  M <- 8; N <- 12
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.7, 0.3)), M, N)

  S <- B %*% t(B); diag(S) <- 0
  E <- t(B) %*% B; diag(E) <- 0
  rs <- rowSums(B)
  cs <- colSums(B)

  # Apply 20 random toggles via rank-1 updates
  for (k in 1:20) {
    i <- sample(M, 1)
    j <- sample(N, 1)
    delta <- if (B[i,j] == 0) 1L else -1L
    B[i,j] <- B[i,j] + delta

    S[i, ] <- S[i, ] + delta * B[, j]
    S[, i] <- S[, i] + delta * B[, j]
    S[i, i] <- 0

    E[j, ] <- E[j, ] + delta * B[i, ]
    E[, j] <- E[, j] + delta * B[i, ]
    E[j, j] <- 0

    rs[i] <- rs[i] + delta
    cs[j] <- cs[j] + delta
  }

  # Verify against full recomputation
  S_check <- B %*% t(B); diag(S_check) <- 0
  E_check <- t(B) %*% B; diag(E_check) <- 0

  expect_equal(S, S_check)
  expect_equal(E, E_check)
  expect_equal(rs, rowSums(B))
  expect_equal(cs, colSums(B))
})
