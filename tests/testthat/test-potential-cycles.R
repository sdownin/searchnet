###############################################################################
## test-potential-cycles.R
##
## Two checks on the potential-game results recorded in inst/proofs/PROOF_TABLE.md
## (Part F, Lemma F3 and Property 4's revision-rule qualifier).
##
## (a) Exact potential, checked on four-move cycles. A utility u_i is an exact
##     potential game on the bipartite state B iff, for every pair of actors
##     i != k and cells (i,j), (k,l), the utility changes around the cycle
##       add (i,j), add (k,l), drop (i,j), drop (k,l)
##     sum to zero, each move scored by the moving actor's own utility. The
##     statistics come from the package's own post-hoc statistic function
##     (get_struct_mod_stats_mat_from_bi_mat in R/saomnk-base.R), checked
##     against independent reference implementations of PROOF_TABLE.md B4.
##     An ego-covariate-weighted coupling term is the negative control: it
##     must FAIL the cycle test.
##
## (b) Revision rule. With single-flip binary-logit (Glauber) revision the
##     stationary law is the Gibbs measure exp(beta * Phi), so its mass on the
##     unique maximizer of Phi tends to 1 as beta grows. Under the multinomial
##     ministep (one logit over all N single flips plus "no change", RSiena's
##     rule) that is not true in general. Computed exactly on the 64-state
##     chain at M = 2, N = 3.
##
## Both parts are pure linear algebra on tiny state spaces: no RSiena call.
###############################################################################

## ---- helpers ---------------------------------------------------------------

## Call the package's statistic function without building an RSiena model:
## the method only needs `self$get_bipartite_effects_theta_df()`,
## `self$get_cov_data()`, `self$M` and `self$N`.
.engine_stats <- function(B, effects, W = NULL) {
  f <- SaomNkRSienaBiEnv_base$public_methods$get_struct_mod_stats_mat_from_bi_mat
  theta_df <- data.frame(shortName = effects, initialValue = 1,
                         effect_level = effects, stringsAsFactors = FALSE)
  fake_self <- list(
    M = nrow(B), N = ncol(B),
    get_bipartite_effects_theta_df = function() theta_df,
    get_cov_data = function(item) W
  )
  environment(f) <- list2env(list(self = fake_self), parent = asNamespace("searchnet"))
  out <- f(B)
  colnames(out) <- effects
  out
}

## Reference statistics, as defined in PROOF_TABLE.md B4.
.ref_cycle4 <- function(B) {          # half the four-cycles through actor i
  O <- B %*% t(B); diag(O) <- 0
  0.5 * rowSums(O * (O - 1) / 2)
}
.ref_xwx_own <- function(B, W) rowSums((B %*% W) * B)   # b_i' W b_i, any W
.n_four_cycles <- function(B) {
  O <- B %*% t(B)
  sum(choose(O[upper.tri(O)], 2))
}

## Sum of the movers' utility changes around add(i,j), add(k,l), drop(i,j),
## drop(k,l). `util(B)` returns the M-vector of actor utilities.
.cycle_sum <- function(util, B, i, j, k, l) {
  set <- function(B, a, b, v) { B[a, b] <- v; B }
  B1 <- set(B, i, j, 1); B2 <- set(B1, k, l, 1)
  B3 <- set(B2, i, j, 0); B4 <- set(B3, k, l, 0)
  (util(B1)[i] - util(B)[i]) + (util(B2)[k] - util(B1)[k]) +
    (util(B3)[i] - util(B2)[i]) + (util(B4)[k] - util(B3)[k])
}

## Max |cycle sum| over random states and cycles.
.max_cycle_violation <- function(util, M, N, n = 100, seed = 1) {
  set.seed(seed)
  worst <- 0
  for (r in seq_len(n)) {
    B <- matrix(rbinom(M * N, 1, 0.5), M, N)
    ik <- sample(M, 2); j <- sample(N, 1); l <- sample(N, 1)
    B[ik[1], j] <- 0; B[ik[2], l] <- 0
    worst <- max(worst, abs(.cycle_sum(util, B, ik[1], j, ik[2], l)))
  }
  worst
}

M_t <- 5; N_t <- 6
set.seed(11)
W_asym <- matrix(runif(N_t * N_t), N_t, N_t); diag(W_asym) <- 0  # NOT symmetric
theta  <- c(density = -0.7, inPop = 0.15, cycle4 = 0.3, XWX = 0.4)

## ---- (a) exact potential on four-move cycles --------------------------------

test_that("engine density and inPop statistics pass the four-move cycle test", {
  util <- function(B) {
    s <- .engine_stats(B, c("density", "inPop"))
    c(s %*% theta[c("density", "inPop")])
  }
  expect_lt(.max_cycle_violation(util, M_t, N_t), 1e-10)
})

test_that("engine cycle4 and XWX columns match the PROOF_TABLE definitions", {
  ## Before the 2026-09-15 engine repair, cycle4 was diag((BB')^3)/2 and XWX
  ## was rowSums(B W B'); neither was a potential statistic for every W.
  set.seed(13)
  for (r in 1:20) {
    B <- matrix(rbinom(M_t * N_t, 1, 0.5), M_t, N_t)
    s <- .engine_stats(B, c("cycle4", "XWX"), W = W_asym)
    expect_equal(unname(s[, "cycle4"]), unname(.ref_cycle4(B)))
    expect_equal(unname(s[, "XWX"]), unname(.ref_xwx_own(B, W_asym)))
  }
})

test_that("engine density + inPop + cycle4 + XWX (asymmetric W) is an exact potential game", {
  eff <- c("density", "inPop", "cycle4", "XWX")
  util <- function(B) c(.engine_stats(B, eff, W = W_asym) %*% theta[eff])
  expect_lt(.max_cycle_violation(util, M_t, N_t), 1e-10)

  ## The potential itself: Rosenthal term for inPop (RSiena's
  ## sum_j x_ij x_+j), one half the number of four-cycles for the
  ## half-normalized cycle4 statistic, own-row sums for density and XWX.
  ## It must reproduce every unilateral change.
  Phi <- function(B) {
    n <- colSums(B)
    theta[["density"]] * sum(B) +
      theta[["inPop"]] * sum(n * (n + 1) / 2) +
      theta[["cycle4"]] * 0.5 * .n_four_cycles(B) +
      theta[["XWX"]] * sum(.ref_xwx_own(B, W_asym))
  }
  set.seed(12)
  worst <- 0
  for (r in 1:100) {
    B <- matrix(rbinom(M_t * N_t, 1, 0.5), M_t, N_t)
    i <- sample(M_t, 1); j <- sample(N_t, 1)
    B2 <- B; B2[i, j] <- 1 - B2[i, j]
    worst <- max(worst, abs((util(B2)[i] - util(B)[i]) - (Phi(B2) - Phi(B))))
  }
  expect_lt(worst, 1e-10)
})

test_that("negative control: an ego-covariate-weighted coupling term fails the cycle test", {
  z <- seq(-1, 1, length.out = M_t)        # non-constant actor covariate
  util <- function(B) {
    s <- .engine_stats(B, c("density", "inPop"))
    c(s %*% theta[c("density", "inPop")]) + z * s[, "inPop"]
  }
  expect_gt(.max_cycle_violation(util, M_t, N_t), 0.1)
})


## ---- (b) single-flip versus multinomial revision ----------------------------

## Stationary distribution by the Grassmann-Taksar-Heyman algorithm, which is
## stable when transition probabilities span many orders of magnitude.
.gth <- function(P) {
  n <- nrow(P); A <- P; diag(A) <- 0
  for (k in n:2) {
    lo <- seq_len(k - 1)
    A[lo, k] <- A[lo, k] / sum(A[k, lo])
    A[lo, lo] <- A[lo, lo] + outer(A[lo, k], A[k, lo])
  }
  p <- numeric(n); p[1] <- 1
  for (k in 2:n) {
    lo <- seq_len(k - 1)
    p[k] <- sum(p[lo] * A[lo, k])
  }
  p / sum(p)
}

test_that("Gibbs concentration holds for single-flip logit but fails for the multinomial ministep (M=2, N=3)", {
  M <- 2; N <- 3; S <- 2^(M * N)
  ## State s <-> B with cell (i, j) at bit (i - 1) * N + j (expand.grid order).
  states <- as.matrix(expand.grid(rep(list(0:1), M * N)))
  idx <- function(v) sum(v * 2^(0:(M * N - 1))) + 1
  nbr <- lapply(seq_len(S), function(s) lapply(seq_len(M), function(i)
    vapply(seq_len(N), function(j) {
      v <- states[s, ]; p <- (i - 1) * N + j; v[p] <- 1 - v[p]; idx(v)
    }, numeric(1))))

  ## Identical-interest game u_i = Phi (an exact potential game for any Phi).
  ## Fixed values with a unique maximizer.
  Phi <- c(68, 169, 178, 152, 72, 110, 36, 9, 171, 24, 179, 172, 63, 106, 140,
           124, 78, 65, 162, 14, 59, 172, 157, 107, 147, 3, 95, 89, 20, 157,
           10, 27, 134, 8, 193, 143, 39, 197, 184, 42, 11, 148, 136, 82, 20,
           144, 40, 45, 128, 195, 191, 53, 109, 39, 114, 5, 7, 158, 157, 92,
           47, 51, 7, 148) / 20
  expect_equal(sum(Phi == max(Phi)), 1L)
  star <- which.max(Phi)

  ## Single-flip (Glauber): pick actor and cell uniformly, accept the flip
  ## with the binary logit against no change.
  P_glauber <- function(beta) {
    P <- matrix(0, S, S)
    for (s in seq_len(S)) for (i in seq_len(M)) for (t in nbr[[s]][[i]]) {
      a <- 1 / (1 + exp(-beta * (Phi[t] - Phi[s])))
      P[s, t] <- P[s, t] + a / (M * N)
      P[s, s] <- P[s, s] + (1 - a) / (M * N)
    }
    P
  }
  ## Multinomial ministep: pick actor uniformly, one logit over its N flips
  ## and no change.
  P_multinomial <- function(beta) {
    P <- matrix(0, S, S)
    for (s in seq_len(S)) for (i in seq_len(M)) {
      opts <- c(s, nbr[[s]][[i]])
      w <- exp(beta * (Phi[opts] - max(Phi[opts]))); w <- w / sum(w)
      for (o in seq_along(opts)) P[s, opts[o]] <- P[s, opts[o]] + w[o] / M
    }
    P
  }

  betas <- c(2, 5, 10, 20)
  gl <- vapply(betas, function(b) .gth(P_glauber(b))[star], numeric(1))
  mn <- vapply(betas, function(b) .gth(P_multinomial(b))[star], numeric(1))

  ## Single flip: stationary law is exactly Gibbs, and its mass on the
  ## maximizer rises toward 1.
  gibbs <- function(b) { g <- exp(b * (Phi - max(Phi))); g / sum(g) }
  expect_equal(.gth(P_glauber(5)), gibbs(5), tolerance = 1e-10)
  expect_true(all(diff(gl) > 0))
  expect_gt(gl[length(gl)], 0.8)

  ## Multinomial: the chain is not Gibbs, and the mass on the maximizer falls
  ## instead of rising (to about 6e-16 at beta = 20).
  expect_gt(max(abs(.gth(P_multinomial(5)) - gibbs(5))), 0.1)
  expect_true(all(diff(mn) < 0))
  expect_lt(mn[length(mn)], 1e-6)
})
