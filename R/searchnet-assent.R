#' @title Two-Sided Tie Formation: Initiative and Confirmation
#' @description
#' SaoMNK's bipartite dependent variable is formed by unilateral ministeps: an
#' actor decides to add or drop a component tie and the tie exists. Many
#' substantive settings are two-sided. The actor may only *propose*, and the
#' tie exists only if the component side confirms:
#'
#' \itemize{
#'   \item an alliance exists only because the prospective partner assented;
#'   \item a job move exists only because the hiring firm made an offer;
#'   \item a funding tie exists only because the investor chose to invest;
#'   \item a category membership exists only because a gatekeeper admitted it.
#' }
#'
#' This module adds that confirmation step. It corresponds to what Snijders
#' calls "unilateral initiative with reciprocal confirmation" in non-directed
#' SAOMs, applied here to the bipartite case, and it is what distinguishes a
#' \emph{negotiated} tie from a \emph{declared} one.
#'
#' Why this matters across theory domains: the confirmation step is a screening
#' device. When the confirming side screens on an actor attribute, the resulting
#' network carries information about that attribute which the proposal network
#' does not. Two networks with identical density and identical degree
#' distributions can therefore differ completely in what an observer may infer
#' from a tie. Any theory in which ties function as signals, certifications, or
#' endorsements needs this distinction, and it cannot be expressed by the
#' objective function alone, because the objective function represents the
#' proposer's preferences rather than the counterparty's veto.
#'
#' @section Relationship to the rest of the package:
#' \code{\link{saomnk_run}} is unchanged and still produces the proposal
#' network. \code{saomnk_confirm} applies the confirmation step to an
#' environment after a run, storing both networks so the pair can be compared.
#' \code{saomnk_run_two_sided} wraps the two into a multi-wave loop.
#'
#' @name searchnet-assent
NULL

#' Specify a confirmation (assent) rule
#'
#' Builds the rule by which the component side confirms or declines an actor's
#' proposal. Three ways to specify it, in increasing order of generality.
#'
#' @param prob Either a single probability applied to every proposal, an
#'   M-length vector of per-actor confirmation probabilities, or a full
#'   \eqn{M \times N} matrix of dyad-specific probabilities. When a matrix is
#'   supplied the remaining arguments are ignored.
#' @param actor_attribute Optional M-length numeric or logical vector. Together
#'   with \code{rate_high} and \code{rate_low} this expresses screening: actors
#'   scoring high on the attribute are confirmed at \code{rate_high}, others at
#'   \code{rate_low}.
#' @param rate_high,rate_low Confirmation probabilities for actors above and
#'   below \code{threshold} on \code{actor_attribute}.
#' @param threshold Cut applied to \code{actor_attribute}. Default 0.5, which
#'   splits a 0/1 indicator.
#' @param component_selectivity Optional N-length vector in \[0, 1). Higher
#'   values make that component more demanding: its confirmation probability is
#'   multiplied by \code{1 - component_selectivity[j]}. Use this to make
#'   prestigious or scarce components harder to attach to.
#' @param name Optional label carried into diagnostics.
#'
#' @return A list of class \code{saomnk_assent}.
#'
#' @examples
#' \dontrun{
#' # every proposal confirmed with probability .5
#' a1 <- saomnk_assent(prob = 0.5)
#'
#' # screening: high-quality actors confirmed at .62, others at .16,
#' # and the most prestigious components are the most demanding
#' a2 <- saomnk_assent(actor_attribute = quality, rate_high = 0.62,
#'                     rate_low = 0.16,
#'                     component_selectivity = 0.45 * prestige)
#'
#' # fully general: supply the M x N probability matrix directly
#' a3 <- saomnk_assent(prob = P_matrix)
#' }
#' @export
saomnk_assent <- function(prob = NULL,
                          actor_attribute = NULL,
                          rate_high = 0.6,
                          rate_low = 0.2,
                          threshold = 0.5,
                          component_selectivity = NULL,
                          name = NULL) {
  if (is.null(prob) && is.null(actor_attribute))
    stop("supply either `prob` or `actor_attribute`")
  if (!is.null(component_selectivity) &&
      (any(component_selectivity < 0) || any(component_selectivity >= 1)))
    stop("`component_selectivity` must lie in [0, 1)")
  structure(list(prob = prob,
                 actor_attribute = actor_attribute,
                 rate_high = rate_high,
                 rate_low = rate_low,
                 threshold = threshold,
                 component_selectivity = component_selectivity,
                 name = name),
            class = "saomnk_assent")
}

#' Build the M x N confirmation-probability matrix
#' @param assent A \code{saomnk_assent} object.
#' @param M,N Dimensions of the bipartite network.
#' @return An \eqn{M \times N} matrix of probabilities in \[0, 1\].
#' @keywords internal
.assent_matrix <- function(assent, M, N) {
  if (!is.null(assent$prob) && is.matrix(assent$prob)) {
    if (!identical(dim(assent$prob), c(M, N)))
      stop(sprintf("`prob` matrix is %dx%d but the network is %dx%d",
                   nrow(assent$prob), ncol(assent$prob), M, N))
    P <- assent$prob
  } else if (!is.null(assent$prob)) {
    base <- if (length(assent$prob) == 1L) rep(assent$prob, M) else assent$prob
    if (length(base) != M) stop("`prob` must have length 1 or M")
    P <- matrix(base, nrow = M, ncol = N)
  } else {
    a <- assent$actor_attribute
    if (length(a) != M) stop("`actor_attribute` must have length M")
    base <- ifelse(a > assent$threshold, assent$rate_high, assent$rate_low)
    P <- matrix(base, nrow = M, ncol = N)
  }
  if (!is.null(assent$component_selectivity)) {
    if (length(assent$component_selectivity) != N)
      stop("`component_selectivity` must have length N")
    P <- P * matrix(1 - assent$component_selectivity, nrow = M, ncol = N,
                    byrow = TRUE)
  }
  pmin(pmax(P, 0), 1)
}

#' Apply the confirmation step to a simulated environment
#'
#' Treats the environment's current bipartite matrix as a matrix of
#' \emph{proposals} and applies the confirmation rule to it. Both networks are
#' retained: the proposal network is stored in \code{env$proposal_matrix} and
#' the environment's \code{bipartite_matrix} becomes the confirmed network, so
#' downstream projections and plots operate on the realised ties.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after \code{\link{saomnk_run}}.
#' @param assent A \code{saomnk_assent} object.
#' @param seed Optional integer seed for the confirmation draws.
#' @return A list with the \code{proposal} matrix, the \code{confirmed} matrix,
#'   and \code{diagnostics}. The environment's \code{bipartite_matrix} is
#'   replaced in place by the confirmed network so that downstream projections
#'   and plots operate on realised ties. The proposal network is returned rather
#'   than attached to the environment, because the R6 environment is locked and
#'   will not accept new bindings.
#' @export
saomnk_confirm <- function(env, assent, seed = NULL) {
  stopifnot(inherits(assent, "saomnk_assent"))
  m <- env$bipartite_matrix
  if (is.null(m)) stop("no bipartite matrix on this environment; run it first")
  Prop <- matrix(as.numeric(as.matrix(m) > 0), nrow = nrow(m))
  M <- nrow(Prop); N <- ncol(Prop)
  P <- .assent_matrix(assent, M, N)
  if (!is.null(seed)) set.seed(seed)
  Conf <- Prop * matrix(stats::rbinom(M * N, 1, as.vector(P)), nrow = M)

  env$bipartite_matrix <- Conf

  a <- assent$actor_attribute
  rate_by_group <- if (!is.null(a)) {
    hi <- a > assent$threshold
    c(high = if (sum(Prop[hi, , drop = FALSE]) > 0)
        sum(Conf[hi, , drop = FALSE]) / sum(Prop[hi, , drop = FALSE]) else NA_real_,
      low  = if (sum(Prop[!hi, , drop = FALSE]) > 0)
        sum(Conf[!hi, , drop = FALSE]) / sum(Prop[!hi, , drop = FALSE]) else NA_real_)
  } else NULL

  diagnostics <- list(
    name = assent$name,
    proposals = sum(Prop),
    confirmed = sum(Conf),
    confirmation_rate = if (sum(Prop) > 0) sum(Conf) / sum(Prop) else NA_real_,
    confirmation_rate_by_group = rate_by_group,
    # How much information the confirmation step injects: the correlation
    # between the screened attribute and degree, before and after. If the
    # confirming side screens, the confirmed network carries a signal the
    # proposal network does not.
    attribute_degree_cor = if (!is.null(a)) c(
      proposal  = suppressWarnings(stats::cor(a, rowSums(Prop))),
      confirmed = suppressWarnings(stats::cor(a, rowSums(Conf)))) else NULL
  )
  list(proposal = Prop, confirmed = Conf, diagnostics = diagnostics)
}

#' Run a two-sided, multi-wave bipartite simulation
#'
#' Each wave runs \code{\link{saomnk_run}} to generate proposals and then
#' applies \code{\link{saomnk_confirm}}. Set \code{assent = NULL} to run the
#' one-sided (declared-tie) case, which is the comparison that isolates what
#' the confirmation step contributes.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object.
#' @param model A \code{saomnk_model} specification.
#' @param assent A \code{saomnk_assent} object, or NULL for one-sided ties.
#' @param waves Number of waves.
#' @param steps_per_actor Micro-steps per actor per wave.
#' @param seed Base seed; wave \code{w} uses \code{seed + w}.
#' @param verbose Print per-wave diagnostics.
#' @return A list with the per-wave confirmed and proposal matrices, the
#'   four coupled degree processes for each, and the assent diagnostics.
#' @export
saomnk_run_two_sided <- function(env, model, assent = NULL, waves = 5,
                                 steps_per_actor = 6, seed = 1,
                                 verbose = FALSE) {
  confirmed <- proposals <- vector("list", waves)
  diags <- vector("list", waves)
  kdeg <- vector("list", waves)

  for (w in seq_len(waves)) {
    saomnk_run(env, model, steps_per_actor = steps_per_actor, seed = seed + w)
    if (!is.null(assent)) {
      cf <- saomnk_confirm(env, assent, seed = seed + 1000 * w)
      proposals[[w]] <- cf$proposal
      diags[[w]] <- cf$diagnostics
    } else {
      m <- env$bipartite_matrix
      proposals[[w]] <- matrix(as.numeric(as.matrix(m) > 0), nrow = nrow(m))
    }
    A <- matrix(as.numeric(as.matrix(env$bipartite_matrix) > 0),
                nrow = nrow(env$bipartite_matrix))
    confirmed[[w]] <- A
    kdeg[[w]] <- .two_sided_degrees(A, w)
    if (verbose) {
      d <- diags[[w]]
      cat(sprintf("wave %d: proposals %d -> confirmed %d (%.0f%%)\n", w,
                  if (is.null(d)) sum(A) else d$proposals, sum(A),
                  100 * (if (is.null(d)) 1 else d$confirmation_rate)))
    }
  }
  list(confirmed = confirmed, proposals = proposals,
       degrees = do.call(rbind, kdeg),
       assent_diagnostics = diags)
}

#' Four coupled degree processes for a bipartite matrix
#'
#' Reports the same quantities used elsewhere in the package and in the
#' employee-mobility application, so results remain comparable across domains.
#' @param A An \eqn{M \times N} binary incidence matrix.
#' @param wave Wave index recorded in the output.
#' @return A one-row data frame.
#' @keywords internal
.two_sided_degrees <- function(A, wave = NA_integer_) {
  AA <- A %*% t(A); diag(AA) <- 0      # actor-actor co-affiliation
  CC <- t(A) %*% A; diag(CC) <- 0      # component-component co-attachment
  g <- function(x) { x <- sort(x[is.finite(x)]); n <- length(x)
    if (n < 2 || mean(x) == 0) return(NA_real_)
    sum((2 * seq_len(n) - n - 1) * x) / (n^2 * mean(x)) }
  data.frame(wave = wave,
             K_AC = mean(rowSums(A)),          # actor scope
             K_CA = mean(colSums(A)),          # component popularity
             K_AA = mean(rowSums(AA > 0)),     # actor co-affiliation
             K_CC = mean(rowSums(CC > 0)),     # component co-attachment
             gini_K_CA = g(colSums(A)),
             density = mean(A))
}
