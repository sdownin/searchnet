#' @title Similarity-Weighted Imitation on Bipartite Networks (K_CA)
#' @description
#' The K_CA channel of the \{K\} interdependence system: the pull an actor feels
#' toward components already held by performance-similar peers.
#'
#' @section Why this is a statistic and not yet an effect:
#' RSiena's effect set for a bipartite dependent variable contains 34 short
#' names and none of them is a similarity effect; the nearest are `inPop_ego`
#' and `outAct_ego`, which are degree-based. `simEgoInDist2` exists for one-mode
#' networks only. searchnet's simulation path delegates to `siena07()`, so an
#' effect that RSiena cannot express cannot enter the evaluation function
#' through it.
#'
#' What is provided here is therefore the STATISTIC, computed natively on any
#' bipartite state, matching the definition used by the CD4 procedural engines
#' so the two are directly comparable. It makes the K_CA channel measurable in
#' searchnet, where it was previously absent entirely. It does NOT make K_CA
#' simulable through `saomnk_run()`, and callers must not assume it does.
#' Closing that gap needs either a C-level RSiena effect or a searchnet-native
#' simulation loop, and both are larger than this.
#'
#' @name searchnet-imitation
#' @importFrom stats setNames
NULL


#' Similarity-weighted imitation statistic for a bipartite state
#'
#' For actor \code{i} and component \code{j}, let the co-holders of \code{j} be
#' the other actors currently holding it. The statistic is the centered
#' performance similarity between \code{i} and the mean performance of those
#' co-holders, summed over the components \code{i} holds:
#'
#' \deqn{s_i(z) = \sum_j z_{ij} [ sim(v_i, \check{v}_j) - \overline{sim} ]}
#'
#' with \eqn{sim(v_i, \check{v}_j) = (\Delta - |v_i - \check{v}_j|) / \Delta}
#' and \eqn{\Delta} the range of performance across actors.
#'
#' Components with no co-holders contribute nothing: they are invisible rather
#' than unattractive, which is the behaviour the CD4 engines implement and is
#' what distinguishes imitation from popularity. Centering follows the RSiena
#' convention that evaluation effects carry no intercept.
#'
#' @param bi_mat Binary actor-by-component matrix (M x N).
#' @param performance Numeric vector of length M, one performance value per
#'   actor.
#' @param per_component If \code{TRUE}, return the M x N matrix of centered
#'   similarities rather than the actor-level sum. Useful for diagnosing which
#'   components carry the pull.
#'
#' @return Numeric vector of length M, or an M x N matrix if
#'   \code{per_component = TRUE}.
#'
#' @details
#' Degenerate cases are handled explicitly rather than by falling through to
#' \code{NaN}. When every actor has identical performance the range is zero and
#' every similarity is defined to be 1, since all actors are maximally similar;
#' the centering then makes the statistic zero, which is correct because no
#' component is more attractive than another on this channel. When a component
#' has no co-holders its similarity is not zero but absent, and it is excluded
#' from the mean used for centering; treating it as a zero would drag the
#' center down and make held-but-unpopular components look repellent.
#'
#' @examples
#' set.seed(1)
#' B <- matrix(rbinom(40, 1, 0.3), nrow = 5)
#' saomnk_sim_ego_indist2(B, performance = runif(5))
#'
#' @export
saomnk_sim_ego_indist2 <- function(bi_mat, performance,
                                   per_component = FALSE) {

  bi_mat <- as.matrix(bi_mat)
  storage.mode(bi_mat) <- "numeric"
  M <- nrow(bi_mat)
  N <- ncol(bi_mat)

  if (length(performance) != M)
    stop(sprintf("performance has length %d but bi_mat has %d actors",
                 length(performance), M))
  if (anyNA(performance))
    stop("performance contains NA")

  rng <- diff(range(performance))
  sim_mat <- matrix(NA_real_, M, N)

  for (i in seq_len(M)) {
    others <- setdiff(seq_len(M), i)
    if (!length(others)) next
    holds <- bi_mat[others, , drop = FALSE]
    n_co <- colSums(holds)
    has_co <- n_co > 0
    if (!any(has_co)) next

    co_perf_sum <- as.numeric(performance[others] %*% holds)
    avg_co <- rep(NA_real_, N)
    avg_co[has_co] <- co_perf_sum[has_co] / n_co[has_co]

    s <- rep(NA_real_, N)
    if (rng > 0) {
      s[has_co] <- (rng - abs(performance[i] - avg_co[has_co])) / rng
    } else {
      # every actor identical: maximal similarity everywhere, which centers to
      # zero below. Defining this as 1 rather than 0 keeps the uncentered
      # quantity on its natural [0, 1] scale.
      s[has_co] <- 1
    }
    # center over components that HAVE co-holders only
    s[has_co] <- s[has_co] - mean(s[has_co])
    sim_mat[i, ] <- s
  }

  if (per_component) return(sim_mat)

  vapply(seq_len(M), function(i) {
    s <- sim_mat[i, ]
    held <- bi_mat[i, ] == 1 & !is.na(s)
    if (!any(held)) 0 else sum(s[held])
  }, numeric(1))
}


#' Imitation statistic for a searchnet environment
#'
#' Convenience wrapper that pulls the bipartite state and a performance vector
#' out of a \code{SaomNkRSienaBiEnv} and applies
#' \code{\link{saomnk_sim_ego_indist2}}.
#'
#' @param env A \code{SaomNkRSienaBiEnv}.
#' @param performance Optional numeric vector of length M. When omitted, actor
#'   scope (row sums of the bipartite matrix) is used as the performance proxy,
#'   and a message says so, because scope is a poor stand-in for fitness and a
#'   silent default here would be the kind of substitution that makes results
#'   hard to trace.
#' @param step Optional step index into the environment's state array; the
#'   current state is used when omitted.
#' @param ... Passed to \code{\link{saomnk_sim_ego_indist2}}.
#'
#' @return Numeric vector of length M, or a matrix, per \code{per_component}.
#' @export
saomnk_env_imitation <- function(env, performance = NULL, step = NULL, ...) {

  if (!inherits(env, "SaomNkRSienaBiEnv"))
    stop("env must be a SaomNkRSienaBiEnv")

  bi <- if (is.null(step)) env$bipartite_matrix else env$bi_env_arr[, , step]
  bi <- as.matrix(bi)

  if (is.null(performance)) {
    performance <- rowSums(bi)
    message("saomnk_env_imitation(): no performance supplied; using actor ",
            "scope as a proxy. Supply `performance` for a fitness-based ",
            "statistic.")
  }

  saomnk_sim_ego_indist2(bi, performance, ...)
}
