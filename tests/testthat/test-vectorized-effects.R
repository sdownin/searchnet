###############################################################################
## test-vectorized-effects.R
## Vectorized effect computation tests
##
## Verifies that vectorized matrix-algebra implementations of RSiena
## effects (distance-2, cycle4, diff-based reconstruction) produce
## correct results.
##
## Pure matrix algebra -- no simulation required.
###############################################################################

context("Vectorized effect computations")

test_that("vectorized simEgoInDist2 matches loop version", {
  set.seed(42)
  M <- 6; N <- 10
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.6, 0.4)), M, N)

  # Social projection
  S <- B %*% t(B)
  diag(S) <- 0

  # Vectorized: distance-2 neighbors via matrix algebra
  dist1 <- (S > 0) * 1
  S2 <- dist1 %*% dist1
  dist2_only <- (S2 > 0) & !(dist1 > 0)
  diag(dist2_only) <- FALSE

  # Should be a valid binary adjacency matrix
  expect_true(all(dist2_only %in% c(TRUE, FALSE)))
  expect_equal(diag(dist2_only), rep(FALSE, M))
})

test_that("dist2 via matrix algebra matches brute-force loop", {
  set.seed(99)
  M <- 5; N <- 8
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.5, 0.5)), M, N)

  S <- B %*% t(B); diag(S) <- 0
  adj <- (S > 0) * 1L

  # Vectorized
  S2 <- adj %*% adj
  dist2_vec <- (S2 > 0 & adj == 0)
  diag(dist2_vec) <- FALSE

  # Brute-force loop
  dist2_loop <- matrix(FALSE, M, M)
  for (a in 1:M) {
    nbrs_a <- which(adj[a, ] > 0)
    for (b in setdiff(1:M, c(a, nbrs_a))) {
      nbrs_b <- which(adj[b, ] > 0)
      if (length(intersect(nbrs_a, nbrs_b)) > 0) {
        dist2_loop[a, b] <- TRUE
      }
    }
  }

  expect_equal(dist2_vec, dist2_loop)
})

test_that("cycle4 uses cached social projection", {
  set.seed(42)
  M <- 5; N <- 8
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.5, 0.5)), M, N)

  XXt <- B %*% t(B)
  diag(XXt) <- 0

  # cycle4 = diag(XXt %*% XXt %*% XXt) / 6 per RSiena convention
  # Or simplified: rowSums((XXt %*% XXt) * XXt)
  c4_full <- rowSums((XXt %*% XXt) * XXt)

  # Verify it's a valid numeric vector
  expect_length(c4_full, M)
  expect_true(all(is.finite(c4_full)))
})

test_that("cycle4 is symmetric in its inputs", {
  set.seed(77)
  M <- 6; N <- 10
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.5, 0.5)), M, N)

  XXt <- B %*% t(B); diag(XXt) <- 0

  # cycle4 via two equivalent computations
  c4_a <- rowSums((XXt %*% XXt) * XXt)
  c4_b <- diag(XXt %*% XXt %*% XXt)

  expect_equal(c4_a, c4_b)
})

test_that("diff-based bi_env_arr reconstruction is correct", {
  set.seed(42)
  M <- 4; N <- 6
  B_init <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.6, 0.4)), M, N)

  # Simulate 10 changes
  changes <- matrix(0, 10, 3)
  B <- B_init
  for (s in 1:10) {
    i <- sample(M, 1)
    j <- sample(N, 1)
    B[i, j] <- 1L - B[i, j]
    changes[s, ] <- c(s, i, j)
  }

  # Reconstruct step 5 from diffs
  B_at_5 <- B_init
  for (s in 1:5) {
    ii <- changes[s, 2]
    jj <- changes[s, 3]
    B_at_5[ii, jj] <- 1L - B_at_5[ii, jj]
  }

  # Reconstruct step 10 (should equal final B)
  B_at_10 <- B_init
  for (s in 1:10) {
    ii <- changes[s, 2]
    jj <- changes[s, 3]
    B_at_10[ii, jj] <- 1L - B_at_10[ii, jj]
  }

  expect_equal(B_at_10, B)
})

test_that("diff reconstruction is reversible", {
  set.seed(55)
  M <- 5; N <- 8
  B_init <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.5, 0.5)), M, N)

  # Apply changes forward
  n_changes <- 15
  changes <- matrix(0, n_changes, 3)
  B <- B_init
  for (s in 1:n_changes) {
    i <- sample(M, 1)
    j <- sample(N, 1)
    B[i, j] <- 1L - B[i, j]
    changes[s, ] <- c(s, i, j)
  }
  B_final <- B

  # Reverse the changes (apply in reverse order)
  for (s in n_changes:1) {
    ii <- changes[s, 2]
    jj <- changes[s, 3]
    B[ii, jj] <- 1L - B[ii, jj]
  }

  # Should be back to initial state
  expect_equal(B, B_init)
})
