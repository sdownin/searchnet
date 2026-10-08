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

## The two distance-2 tests below check matrix algebra for distance-2 reach in
## the actor projection. Until 2026-10-04 the simEgoInDist2 column of
## get_struct_mod_stats_mat_from_bi_mat() was built on it, and the first test
## was named after simEgoInDist2; RSiena's simEgoInDist2 is a different
## statistic and is now pinned in test-structural-stats-vs-rsiena.R.
test_that("distance-2 reach in the actor projection is a binary adjacency", {
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

## The two cycle4 tests that stood here until 2026-09-15 compared
## rowSums((XXt %*% XXt) * XXt) with diag(XXt^3), i.e. the formula with itself,
## and neither is RSiena's statistic. They are replaced by a brute-force count;
## agreement with RSiena's own siena07 target is pinned in
## test-structural-stats-vs-rsiena.R.

test_that("cycle4 actor statistic equals half the four-cycles through the actor", {
  set.seed(42)
  for (trial in 1:20) {
    M <- sample(3:7, 1); N <- sample(3:8, 1)
    B <- matrix(sample(0:1, M*N, replace=TRUE), M, N)

    ## Implementation formula (R/saomnk-base.R)
    ov <- B %*% t(B); diag(ov) <- 0
    s_impl <- rowSums(choose(ov, 2)) / 2

    ## Brute force: a four-cycle through actor i is an unordered pair of
    ## components {j, l} held by i and by some other actor k.
    s_brute <- numeric(M)
    for (i in seq_len(M)) {
      for (k in setdiff(seq_len(M), i)) {
        shared <- which(B[i, ] == 1 & B[k, ] == 1)
        s_brute[i] <- s_brute[i] + choose(length(shared), 2)
      }
    }
    expect_equal(s_impl, s_brute / 2)

    ## Network level: each four-cycle has two actors, so the actor statistics
    ## sum to the number of distinct four-cycles.
    n_cycles <- 0
    for (a in 1:(M - 1)) for (b in (a + 1):M)
      n_cycles <- n_cycles + choose(sum(B[a, ] * B[b, ]), 2)
    expect_equal(sum(s_impl), n_cycles)
  }
})

test_that("cycle4 is zero when no two actors share two components", {
  B <- diag(4)                       # every actor holds one distinct component
  ov <- B %*% t(B); diag(ov) <- 0
  expect_equal(rowSums(choose(ov, 2)) / 2, rep(0, 4))
  ## The pre-2026-09-15 formula kept the diagonal and reported degenerate walks.
  S <- B %*% t(B)
  expect_true(any(rowSums((S %*% S) * S) / 2 != 0))
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
