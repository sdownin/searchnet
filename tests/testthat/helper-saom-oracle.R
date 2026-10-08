###############################################################################
## helper-saom-oracle.R
##
## An independent continuous-time bipartite SAOM forward simulator, used ONLY
## as an oracle in tests (gate G5 of the 2026-10-06 pre-registration). It is
## a port of a reference simulator written outside this package and validated
## there against RSiena 1.5.0's logged choice probabilities. It shares no code
## with search_rsiena(): it never calls RSiena, and every change statistic is
## written out below.
##
## The law (Snijders 2001). Each actor i holds an exponential clock with rate
## lambda_i. At a tick it toggles one tie (actor i, component j) or keeps its
## portfolio, by multinomial logit over the change in its evaluation function;
## the "keep" option has utility 0. Effects:
##   density  s_i = sum_j x_ij                      change: sg_j
##   inPop    s_i = sum_j x_ij x_+j                 change: sg_j (x_+j - x_ij + 1)
##   XWX      s_i = sum_{j != h} x_ij x_ih w_hj     change: sg_j ((W + W') x_i - 2 diag(W) x_i)_j
##   egoX     s_i = v_i sum_j x_ij                  change: sg_j v_i
## with sg_j = +1 to add a tie and -1 to drop it, and W as supplied to RSiena
## (RSiena's XWX does not center W). `v` is the covariate as RSiena applied it.
###############################################################################

.oracle_change_stats <- function(B, i, eff, W = NULL, v = NULL) {
  x  <- B[i, ]
  sg <- 1 - 2 * x
  out <- matrix(0, length(x), length(eff))
  colnames(out) <- eff
  for (k in seq_along(eff)) {
    out[, k] <- switch(eff[k],
      density = sg,
      inPop   = sg * (colSums(B) - x + 1),
      XWX     = sg * (as.numeric((W + t(W)) %*% x) - 2 * diag(W) * x),
      egoX    = sg * v[i],
      stop("oracle: effect not implemented: ", eff[k]))
  }
  out
}

## Log choice probabilities over (toggle 1..N, keep) for ego i in state B.
.oracle_log_probs <- function(B, i, theta, W = NULL, v = NULL) {
  u <- c(as.numeric(.oracle_change_stats(B, i, names(theta), W, v) %*% theta), 0)
  u - max(u) - log(sum(exp(u - max(u))))
}

## One period of length `t_end` from B0 with per-actor rates `lambda`.
## Gillespie form: the next opportunity comes after Exp(sum lambda), and it
## belongs to actor i with probability lambda_i / sum lambda, which is the
## same law as independent per-actor clocks.
.oracle_simulate <- function(B0, lambda, theta, W = NULL, v = NULL, t_end = 1) {
  B <- B0
  N <- ncol(B0)
  tot <- sum(lambda)
  t <- 0
  n_opp <- 0L
  repeat {
    t <- t + stats::rexp(1, tot)
    if (t > t_end) break
    i <- sample.int(length(lambda), 1L, prob = lambda)
    n_opp <- n_opp + 1L
    lp <- .oracle_log_probs(B, i, theta, W, v)
    j <- sample.int(N + 1L, 1L, prob = exp(lp))
    if (j <= N) B[i, j] <- 1 - B[i, j]
  }
  list(B = B, n_opp = n_opp)
}

## End-of-run statistics compared in G5.
.oracle_end_stats <- function(B, B0, W) {
  c(ties    = sum(B),
    inPop   = sum(colSums(B)^2),
    XWX     = sum(vapply(seq_len(nrow(B)), function(i) {
                x <- B[i, ]
                as.numeric(t(x) %*% W %*% x) - sum(diag(W) * x)
              }, numeric(1))),
    hamming = sum(abs(B - B0)))
}
