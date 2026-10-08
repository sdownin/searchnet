# Independent check of the per-tie ministep adoption law F_N and of the
# first-order rescaling kappa_N = 2N/(N+1) used by
# verify_brock_durlauf_reduction() (NEWS 0.11.0, PROOF_TABLE L16).
#
# The reference here does not use the birth-death reduction. It builds the
# ministep Markov chain on the full row state space {0,1}^N: from row x the
# actor toggles tie j with probability proportional to exp(+delta) (add) or
# exp(-delta) (drop), or makes no change with probability proportional to 1.
# The stationary law is the leading left eigenvector of that 2^N x 2^N matrix.

.full_row_chain_adoption <- function(delta, N) {
  S <- as.matrix(expand.grid(rep(list(0:1), N)))
  n_s <- nrow(S)
  key <- function(x) sum(x * 2^(seq_len(N) - 1)) + 1
  P <- matrix(0, n_s, n_s)
  for (s in seq_len(n_s)) {
    x <- S[s, ]
    w <- c(1, ifelse(x == 1, exp(-delta), exp(delta)))
    w <- w / sum(w)
    P[s, s] <- P[s, s] + w[1]
    for (j in seq_len(N)) {
      y <- x
      y[j] <- 1 - y[j]
      P[s, key(y)] <- P[s, key(y)] + w[j + 1]
    }
  }
  ev <- eigen(t(P))
  pi_s <- Re(ev$vectors[, which.min(abs(ev$values - 1))])
  pi_s <- pi_s / sum(pi_s)
  k <- rowSums(S)
  list(F = sum(pi_s * k) / N,
       pi_k = vapply(0:N, function(kk) sum(pi_s[k == kk]), numeric(1)))
}

.pi_k_closed_form <- function(delta, N) {
  k <- 0:N
  w <- choose(N, k) * exp(2 * delta * k) *
    (1 + (N - k) * exp(delta) + k * exp(-delta))
  w / sum(w)
}

test_that("pi_k closed form matches the full-row ministep chain, N = 1..6", {
  for (N in 1:6) {
    for (delta in c(-1.3, -0.2, 0, 0.4, 1.1)) {
      ref <- .full_row_chain_adoption(delta, N)
      expect_equal(ref$pi_k, .pi_k_closed_form(delta, N), tolerance = 1e-8,
                   info = sprintf("N = %d, delta = %g", N, delta))
      expect_equal(searchnet:::.saomnk_ministep_adoption(delta, N), ref$F,
                   tolerance = 1e-8,
                   info = sprintf("N = %d, delta = %g", N, delta))
    }
  }
})

test_that("F_N equals the binary logit exactly at N = 1 only", {
  d <- seq(-3, 3, by = 0.5)
  F1 <- vapply(d, function(x) .full_row_chain_adoption(x, 1)$F, numeric(1))
  expect_equal(F1, stats::plogis(d), tolerance = 1e-10)
  F3 <- .full_row_chain_adoption(1, 3)$F
  expect_gt(abs(F3 - stats::plogis(1)), 0.05)
})

test_that("kappa_N = F_N'(0) / sigma'(0) = 2N/(N+1), N = 1..6", {
  h <- 1e-4
  for (N in 1:6) {
    dF <- (.full_row_chain_adoption(h, N)$F -
             .full_row_chain_adoption(-h, N)$F) / (2 * h)
    expect_equal(.full_row_chain_adoption(0, N)$F, 0.5, tolerance = 1e-10)
    expect_equal(dF / 0.25, 2 * N / (N + 1), tolerance = 1e-6,
                 info = sprintf("N = %d", N))
  }
})

test_that("sigma(kappa_N delta) is first order only: error is O(delta^3)", {
  for (N in 2:6) {
    kap <- 2 * N / (N + 1)
    e1 <- abs(.full_row_chain_adoption(0.1, N)$F - stats::plogis(kap * 0.1))
    e2 <- abs(.full_row_chain_adoption(0.2, N)$F - stats::plogis(kap * 0.2))
    expect_lt(e1, 1e-3)
    ## doubling delta multiplies a cubic error by about 8
    expect_gt(e2 / e1, 6)
    expect_lt(e2 / e1, 10)
  }
})
