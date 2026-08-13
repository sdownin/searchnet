#' @title Pre-Estimation Screens and Post-Estimation Diagnostics
#' @description
#' Four diagnostics for bipartite stochastic actor-oriented models, two of
#' which run on the observed data before anything is fitted and two of which
#' read a fitted model.
#'
#' \describe{
#'   \item{\code{\link{boundary_screen}}}{Which degree-threshold effects are
#'     structurally unestimable on this panel, from the observed array alone.}
#'   \item{\code{\link{scope_confound_screen}}}{Whether a coupling matrix
#'     carries pairwise structure or merely re-measures actor scope, and what
#'     row-normalization and banding do about it.}
#'   \item{\code{\link{gof_battery}}}{Five goodness-of-fit statistics with a
#'     verdict for each, rather than one pass/fail on two degree
#'     distributions.}
#'   \item{\code{\link{rate_ladder}}}{Convergence and fit across the rate
#'     functions RSiena offers, with the simulated floor cells reported beside
#'     them so the mechanism is visible.}
#' }
#'
#' All four report every row they compute, passing or not. None of them
#' selects a specification; they say what a specification is up against.
#'
#' @name searchnet-screens
#' @importFrom stats cor quantile sd
NULL


# ---------------------------------------------------------------------------- #
#  Internal helpers
# ---------------------------------------------------------------------------- #

## Coerce the several things a caller may hold to a plain M x N x T numeric
## array: a 3-d array, an RSiena sienaDependent (which IS a 3-d array carrying
## attributes), or a list of M x N matrices, one per wave.
.searchnet_incidence_array <- function(x, arg = "x") {
  if (is.list(x) && !is.array(x)) {
    if (!length(x))
      stop(sprintf("`%s` is an empty list; supply one matrix per wave.", arg))
    dims <- lapply(x, dim)
    if (any(vapply(dims, is.null, logical(1))) ||
        length(unique(vapply(dims, paste, character(1), collapse = "x"))) != 1L)
      stop(sprintf("`%s` must be a list of matrices with identical dimensions.", arg))
    x <- array(unlist(lapply(x, as.matrix)),
               dim = c(dims[[1]][1], dims[[1]][2], length(x)))
  }
  ## Strip every attribute except the dimensions. A sienaDependent carries a
  ## class and several attributes on top of a plain 3-d array, and arithmetic
  ## on it would otherwise dispatch somewhere unexpected.
  d <- dim(x)
  if (length(d) != 3L)
    stop(sprintf("`%s` must be an actor x component x wave array (3 dimensions), got %d.",
                 arg, length(d)))
  x <- array(as.numeric(unclass(x)), dim = d)
  if (any(is.na(x)))
    stop(sprintf("`%s` contains NA; the screens are defined on a complete binary array.", arg))
  if (!all(x %in% c(0, 1)))
    stop(sprintf("`%s` must be binary (0/1).", arg))
  x
}


## Position of an observed statistic inside its attainable range, and the
## verdict that follows. Exactly 0 or exactly 1 disqualifies; nothing else does.
.searchnet_position_verdict <- function(obs, lo, hi) {
  pos <- if (isTRUE(all.equal(hi, lo))) NA_real_ else (obs - lo) / (hi - lo)
  saturated <- !is.na(pos) &&
    (isTRUE(all.equal(pos, 0)) || isTRUE(all.equal(pos, 1)))
  list(position = pos,
       estimable = !saturated,
       verdict = if (saturated) "SATURATED: cannot converge"
                 else "interior: estimable in principle")
}


## Turn one effect on in a sienaEffects object, reporting rather than silently
## skipping when the row does not exist. A silently skipped effect is a model
## that converges and is not the model that was specified.
.searchnet_turn_on <- function(eff, shortName, interaction1 = "",
                               type = "eval", name = "dv", verbose = TRUE) {
  sel <- eff$shortName == shortName & eff$type == type & eff$name == name &
    (if (identical(interaction1, "")) TRUE else eff$interaction1 == interaction1)
  if (!any(sel)) {
    if (verbose)
      message(sprintf("  [MISSING] %s %s %s is not offered for this data object",
                      type, shortName, interaction1))
    return(structure(eff, searchnet_missing = TRUE))
  }
  eff[sel, "include"] <- TRUE
  eff
}


## p-value and joint Mahalanobis distance from a sienaGOF object, read off the
## object rather than parsed out of its printed form. The MHD that
## print.sienaGOF reports is sum(attr(x, "originalMahalanobisDistances")).
.searchnet_gof_pm <- function(fit, auxfn, varName = "dv", join = TRUE, ...) {
  g <- try(RSiena::sienaGOF(fit, auxfn, verbose = FALSE, join = join,
                            varName = varName, ...), silent = TRUE)
  if (inherits(g, "try-error"))
    return(list(p = NA_real_, mhd = NA_real_, gof = NULL,
                error = sub("\n.*", "", as.character(g))))
  mhd <- try(sum(attr(g, "originalMahalanobisDistances")), silent = TRUE)
  list(p = tryCatch(as.numeric(g[[1]]$p), error = function(e) NA_real_),
       mhd = if (inherits(mhd, "try-error")) NA_real_ else as.numeric(mhd),
       gof = g, error = NA_character_)
}


## Observed and simulated counts at the two lowest outdegree cells. This is
## where a constant-rate function manufactures actors the data never contains,
## so it is the cell that makes the mechanism visible rather than inferred.
## Defensive by construction: a diagnostic must never abort the ladder it is
## diagnosing, so every failure returns NA rather than raising.
.searchnet_floor_profile <- function(fit, varName = "dv") {
  out <- rep(NA_real_, 4L)
  names(out) <- c("obs_deg0", "sim_deg0", "obs_deg1", "sim_deg1")
  g <- try(RSiena::sienaGOF(fit, RSiena::OutdegreeDistribution, verbose = FALSE,
                            join = TRUE, varName = varName, levls = 0:4),
           silent = TRUE)
  if (inherits(g, "try-error")) return(out)
  d <- try(RSiena::descriptives.sienaGOF(g), silent = TRUE)
  if (inherits(d, "try-error") || is.null(dim(d)) || ncol(d) < 2L) return(out)
  rn <- tolower(rownames(d))
  r_obs <- which(startsWith(rn, "obs"))[1]
  r_sim <- which(startsWith(rn, "mean"))[1]
  if (!is.na(r_obs)) { out["obs_deg0"] <- d[r_obs, 1]; out["obs_deg1"] <- d[r_obs, 2] }
  if (!is.na(r_sim)) { out["sim_deg0"] <- d[r_sim, 1]; out["sim_deg1"] <- d[r_sim, 2] }
  out
}


# ---------------------------------------------------------------------------- #
#  (a) boundary_screen
# ---------------------------------------------------------------------------- #

#' Screen Degree-Threshold Effects for Structural Unestimability
#'
#' Under method-of-moments estimation an effect whose observed statistic lies
#' exactly on the boundary of its attainable support cannot converge: the
#' estimate diverges, because no parameter value makes the simulated mean equal
#' a value the simulation can only approach.  Membership is a property of the
#' observed data and of the sampling rule that produced it, so it is checkable
#' before any model is fitted, and this function is the whole check.
#'
#' The screen is the reason a balanced panel and an unbalanced panel built from
#' the same source can give opposite verdicts on the same effect.  A balanced
#' panel requires every actor to hold a tie in every wave, which puts the
#' \code{outIso} statistic at exactly zero and \code{outTrunc(1)} and
#' \code{outThreshold(1)} at exactly their maxima.  The sampling rule, not the
#' dataset, is what decides.
#'
#' @param x A binary actor-by-component-by-wave array, a list of binary
#'   actor-by-component matrices (one per wave), or an RSiena
#'   \code{sienaDependent} object of \code{type = "bipartite"}.
#' @return A \code{data.frame} with one row per candidate effect and columns
#'   \describe{
#'     \item{\code{effect}}{RSiena short name of the candidate effect.}
#'     \item{\code{observed}}{The observed value of that effect's statistic.}
#'     \item{\code{attainable_min}, \code{attainable_max}}{The endpoints of the
#'       statistic's support on a panel of this shape.}
#'     \item{\code{position}}{Where \code{observed} sits in that range, on
#'       \eqn{[0, 1]}.}
#'     \item{\code{estimable}}{Logical; \code{FALSE} exactly when
#'       \code{position} is 0 or 1.}
#'     \item{\code{verdict}}{The same judgement in words.}
#'   }
#' @seealso \code{\link{scope_confound_screen}} for the other pre-estimation
#'   screen, which concerns the theory terms rather than the structural ones.
#' @export
#' @examples
#' set.seed(1)
#' arr <- array(rbinom(60 * 20 * 3, 1, 0.15), c(60, 20, 3))
#'
#' ## Unbalanced: actors may hold nothing in a wave, so outIso is interior.
#' boundary_screen(arr)
#'
#' ## The balanced-panel rule moves outIso onto its boundary.
#' keep <- apply(apply(arr, c(1, 3), sum) > 0, 1, all)
#' boundary_screen(arr[keep, , , drop = FALSE])
boundary_screen <- function(x) {

  arr <- .searchnet_incidence_array(x)
  M <- dim(arr)[1]; N <- dim(arr)[2]; TT <- dim(arr)[3]
  deg <- apply(arr, c(1, 3), sum)   ## actor outdegree in each wave

  cand <- list(
    ## number of actor-waves at zero ties
    outIso        = list(obs = sum(deg == 0),         lo = 0, hi = M * TT),
    ## sum of min(degree, 1): the count of actor-waves holding anything
    outTrunc1     = list(obs = sum(pmin(deg, 1)),     lo = 0, hi = M * TT),
    ## number of actor-waves at or above one tie
    outThreshold1 = list(obs = sum(deg >= 1),         lo = 0, hi = M * TT),
    ## sum of max(degree - 1, 0): ties beyond the first
    in2Plus       = list(obs = sum(pmax(deg - 1, 0)), lo = 0, hi = M * TT * (N - 1))
  )

  do.call(rbind, lapply(names(cand), function(nm) {
    z <- cand[[nm]]
    v <- .searchnet_position_verdict(z$obs, z$lo, z$hi)
    data.frame(effect = nm, observed = z$obs,
               attainable_min = z$lo, attainable_max = z$hi,
               position = round(v$position, 4),
               estimable = v$estimable, verdict = v$verdict,
               stringsAsFactors = FALSE)
  }))
}


# ---------------------------------------------------------------------------- #
#  (b) scope_confound_screen
# ---------------------------------------------------------------------------- #

## The three treatments of a coupling matrix. Each returns an N x N matrix
## with a zero diagonal.
.searchnet_treat_W <- function(W, treatment, band_quantile) {
  W <- as.matrix(W); storage.mode(W) <- "double"; diag(W) <- 0
  if (identical(treatment, "raw")) return(W)
  if (identical(treatment, "rownorm")) {
    rs <- rowSums(W)
    rs[rs == 0] <- 1
    return(W / rs)
  }
  if (identical(treatment, "band")) {
    ## Keep the strongest tail of the off-diagonal distribution and binarise
    ## it. This reinterprets the construct: a banded tension matrix means
    ## estranged pairs, not degrees of estrangement, and a paper using one
    ## should say so.
    v <- W[upper.tri(W)]
    thr <- as.numeric(stats::quantile(v, probs = band_quantile, names = FALSE))
    B <- matrix(0, nrow(W), ncol(W), dimnames = dimnames(W))
    B[W >= thr] <- 1
    diag(B) <- 0
    return(B)
  }
  stop("treatment must be one of 'raw', 'rownorm', 'band'.")
}


#' Screen a Coupling Matrix for the Dense-Coupling Scope Confound
#'
#' The coupling statistic an \code{XWX} effect carries is a weighted count of
#' an actor's other ties.  When \eqn{W} is dense that sum is approximately the
#' mean coupling times actor degree, so the theory term competes with the
#' degree effect for the same variance instead of carrying pairwise structure.
#' The symptom during estimation is a coupling block that will not converge
#' while the degree effect collapses.  The cause is visible beforehand, as the
#' correlation between the coupling statistic and actor scope across all
#' actor-wave-component cells.
#'
#' Three treatments of \eqn{W} are reported side by side, because the reflex
#' remedy is the one that does not work.  \strong{Row-normalization does not
#' touch the confound}: it rescales the statistic but leaves the density
#' pattern that produced it, so its correlation is effectively
#' indistinguishable from the raw matrix and its nonzero share is identical.
#' \strong{Banding does}: keeping only the strongest tail sparsifies \eqn{W},
#' and the statistic stops tracking portfolio size.  Reading the three rows
#' together is the point of the function.
#'
#' @param x A binary actor-by-component-by-wave array, a list of matrices, or
#'   an RSiena bipartite \code{sienaDependent}, as in
#'   \code{\link{boundary_screen}}.
#' @param W A single component-by-component coupling matrix, or a named list of
#'   them.  Diagonals are zeroed.
#' @param treatments Character vector, any of \code{"raw"}, \code{"rownorm"},
#'   \code{"band"} (default: all three, in that order).
#' @param band_quantile Numeric in \eqn{(0, 1)}.  The quantile of the
#'   off-diagonal distribution above which pairs are kept by the \code{"band"}
#'   treatment (default \code{0.90}, the strongest decile).
#' @param threshold Numeric in \eqn{[0, 1]}.  Absolute correlation at or above
#'   which the verdict reads "confounded" (default \code{0.5}).  This is a
#'   reporting convention, not a test: the correlation itself is the finding.
#' @param waves Integer vector of wave indices to pool over, or \code{NULL}
#'   (default) for all waves.
#' @return A \code{data.frame} with one row per matrix and treatment and
#'   columns \code{matrix}, \code{treatment}, \code{nonzero_share} (share of
#'   off-diagonal cells that are nonzero), \code{r_scope} (correlation of the
#'   coupling statistic with actor scope), \code{delta_vs_raw} (change in
#'   \code{r_scope} relative to that matrix's raw treatment), and
#'   \code{verdict}.
#' @seealso \code{\link{boundary_screen}}
#' @export
#' @examples
#' set.seed(2)
#' arr <- array(rbinom(40 * 12 * 2, 1, 0.3), c(40, 12, 2))
#' W <- matrix(runif(144), 12, 12); W <- (W + t(W)) / 2; diag(W) <- 0
#' scope_confound_screen(arr, list(dense = W))
scope_confound_screen <- function(x, W,
                                  treatments = c("raw", "rownorm", "band"),
                                  band_quantile = 0.90,
                                  threshold = 0.5,
                                  waves = NULL) {

  arr <- .searchnet_incidence_array(x)
  N <- dim(arr)[2]; TT <- dim(arr)[3]
  treatments <- match.arg(treatments, c("raw", "rownorm", "band"),
                          several.ok = TRUE)
  stopifnot(is.numeric(band_quantile), length(band_quantile) == 1,
            band_quantile > 0, band_quantile < 1)

  if (is.matrix(W) || inherits(W, "Matrix")) W <- list(W = W)
  stopifnot(is.list(W), length(W) >= 1)
  if (is.null(names(W)) || any(!nzchar(names(W))))
    names(W) <- paste0("W", seq_along(W))

  if (is.null(waves)) waves <- seq_len(TT)
  waves <- as.integer(waves)
  if (any(waves < 1L) || any(waves > TT))
    stop(sprintf("`waves` must index waves 1..%d.", TT))

  ## Statistic for actor i and component c in wave t:
  ##   s_ict = sum_c' W[c, c'] x_ic't      (the diagonal is zero, so c' != c)
  ## which for the whole wave is X_t %*% t(W). Correlate the vectorised cells
  ## against actor scope, repeated across components.
  scope_vec <- unlist(lapply(waves, function(t)
    rep(rowSums(arr[, , t, drop = FALSE]), times = N)))

  rows <- list()
  for (nm in names(W)) {
    Wm <- as.matrix(W[[nm]])
    if (nrow(Wm) != N || ncol(Wm) != N)
      stop(sprintf("W matrix '%s' is %dx%d but the panel has %d components.",
                   nm, nrow(Wm), ncol(Wm), N))
    raw_r <- NA_real_
    for (tr in treatments) {
      Wt <- .searchnet_treat_W(Wm, tr, band_quantile)
      offdiag <- Wt[upper.tri(Wt)]
      share <- mean(abs(offdiag) > 1e-12)
      stat <- unlist(lapply(waves, function(t)
        as.numeric(arr[, , t, drop = TRUE] %*% t(Wt))))
      r <- suppressWarnings(stats::cor(stat, scope_vec))
      if (identical(tr, "raw")) raw_r <- r
      rows[[length(rows) + 1L]] <- data.frame(
        matrix = nm, treatment = tr,
        nonzero_share = round(share, 4),
        r_scope = round(r, 4),
        delta_vs_raw = if (is.na(raw_r)) NA_real_ else round(r - raw_r, 4),
        verdict = if (is.na(r)) "degenerate: statistic has no variance"
                  else if (abs(r) >= threshold) "confounded with actor scope"
                  else "carries pairwise structure",
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------- #
#  (c) gof_battery
# ---------------------------------------------------------------------------- #

#' Five-Statistic Goodness-of-Fit Battery for a Bipartite SAOM
#'
#' Runs \code{\link[RSiena]{sienaGOF}} on five auxiliary statistics and returns
#' a verdict for each, rather than one pass/fail on the two degree
#' distributions.  The two degree distributions are the standard pair; the two
#' projections ask whether the model reproduces the co-membership structure the
#' theory terms exist to explain; the fifth is a weaker and more forgiving
#' question, whether the \emph{spread} of each degree distribution is
#' reproduced when its whole shape is not.
#'
#' The battery is fixed here so that it cannot be chosen after seeing which
#' statistics pass, which would be a specification search on the fit.  Every
#' statistic computed is reported, passing or not.
#'
#' @param fit A \code{sienaFit} object estimated with \code{returnDeps = TRUE}.
#'   A cheap way to obtain one from an existing fit is
#'   \code{sienaAlgorithmCreate(nsub = 0)} with \code{prevAns}, which runs
#'   phase 3 only.
#' @param varName Character. Name of the bipartite dependent variable in the
#'   data object (default \code{"dv"}).
#' @param alpha Numeric. Significance level below which a statistic is reported
#'   as a misfit (default \code{0.05}).
#' @param levls_coaffiliation Integer vector of levels for the actor
#'   co-affiliation statistic (default \code{0:12}).  Counts above the maximum
#'   are pooled into it.
#' @param levls_copresence Integer vector of levels for the component
#'   co-presence statistic (default \code{0:30}).
#' @param join Logical. Join periods before testing (default \code{TRUE}).
#' @param verbose Logical. Print each row as it completes (default
#'   \code{TRUE}).
#' @return A \code{data.frame} with columns \code{statistic}, \code{p},
#'   \code{mhd} and \code{verdict}, one row per statistic.  The fitted
#'   \code{sienaGOF} objects are attached as the \code{"gof"} attribute, so
#'   \code{\link[RSiena]{descriptives.sienaGOF}} and the plot method remain
#'   available for reading \emph{where} a misfit sits.
#' @seealso \code{\link{rate_ladder}}
#' @export
#' @examples
#' \dontrun{
#' fit <- siena07(alg, data = dat, effects = eff, returnDeps = TRUE)
#' gof_battery(fit, varName = "dv")
#' }
gof_battery <- function(fit, varName = "dv", alpha = 0.05,
                        levls_coaffiliation = 0:12,
                        levls_copresence = 0:30,
                        join = TRUE, verbose = TRUE) {

  if (!inherits(fit, "sienaFit"))
    stop("`fit` must be a sienaFit object returned by siena07().")
  if (is.null(fit$sims))
    stop("`fit` carries no simulated networks; re-estimate with returnDeps = TRUE.")

  ## Actor co-affiliation: the XX' projection, how many components each pair of
  ## actors holds in common.
  ActorCoAffiliation <- function(i, obsData, sims, period, groupName, varName) {
    x <- RSiena::sparseMatrixExtraction(i, obsData, sims, period, groupName, varName)
    cp <- as.matrix(x %*% Matrix::t(x))
    v <- cp[upper.tri(cp)]
    tabulate(pmin(v, max(levls_coaffiliation)) + 1L,
             nbins = length(levls_coaffiliation))
  }
  ## Component co-presence: the X'X projection, how many actors each pair of
  ## components shares. This is the projection the coupling matrices speak to.
  ComponentCoPresence <- function(i, obsData, sims, period, groupName, varName) {
    x <- RSiena::sparseMatrixExtraction(i, obsData, sims, period, groupName, varName)
    cp <- as.matrix(Matrix::t(x) %*% x)
    v <- cp[upper.tri(cp)]
    tabulate(pmin(v, max(levls_copresence)) + 1L,
             nbins = length(levls_copresence))
  }
  ## A length-1 auxiliary statistic breaks sienaGOF's plot key, so this returns
  ## three elements. Total ties is near-fitted by the density parameter and is
  ## a floor check; the two dispersions ask about spread rather than shape.
  DegreeSpread <- function(i, obsData, sims, period, groupName, varName) {
    x <- RSiena::sparseMatrixExtraction(i, obsData, sims, period, groupName, varName)
    m <- as.matrix(x)
    c(ties = sum(m), sd_scope = stats::sd(rowSums(m)),
      sd_popularity = stats::sd(colSums(m)))
  }

  spec <- list(
    list(label = "Actor scope (outdegree)",         fn = RSiena::OutdegreeDistribution),
    list(label = "Component popularity (indegree)", fn = RSiena::IndegreeDistribution),
    list(label = "Actor co-affiliation (XX')",      fn = ActorCoAffiliation),
    list(label = "Component co-presence (X'X)",     fn = ComponentCoPresence),
    list(label = "Tie volume and degree dispersion", fn = DegreeSpread)
  )

  gofs <- list()
  rows <- lapply(spec, function(s) {
    z <- .searchnet_gof_pm(fit, s$fn, varName = varName, join = join)
    gofs[[s$label]] <<- z$gof
    verdict <- if (is.na(z$p)) sprintf("NOT COMPUTED: %s", z$error)
               else if (z$p >= alpha) "fits"
               else "misfit"
    if (verbose)
      message(sprintf("%-34s p = %-8s MHD = %-10s %s", s$label,
                      format(z$p), format(round(z$mhd, 2)), verdict))
    data.frame(statistic = s$label, p = z$p, mhd = z$mhd,
               verdict = verdict, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  attr(out, "gof") <- gofs
  out
}


# ---------------------------------------------------------------------------- #
#  (d) rate_ladder
# ---------------------------------------------------------------------------- #

#' Fit a Model Across the Rate Functions RSiena Offers
#'
#' A constant rate gives an actor holding one component the same number of
#' change opportunities as one holding forty-six.  The surplus is spent
#' dropping the only tie held, which manufactures low-degree actors the data
#' never contains.  This function fits the same objective function under each
#' rate form in turn and reports convergence and fit for every rung, together
#' with the simulated counts at the two lowest outdegree cells so the mechanism
#' is visible rather than inferred.
#'
#' Convergence and fit routinely select different rungs, which is the reason
#' both are reported and neither is optimized here.  A rate effect
#' redistributes opportunities across actors; it does not make any particular
#' degree value an attractor.  So it has a mechanism to work through when the
#' misfit sits at the floor, and none when the misfit is an interior spike.
#' Reading the \code{sim_deg0} column against \code{obs_deg0} is what
#' distinguishes those two cases.
#'
#' @param data A \code{siena} data object from
#'   \code{\link[RSiena]{sienaDataCreate}}.
#' @param effects A \code{sienaEffects} object carrying the base specification
#'   (structural and theory terms already included).  Each rung starts from a
#'   copy of this and adds one rate effect.
#' @param varName Character. Name of the bipartite dependent variable
#'   (default \code{"dv"}).
#' @param rate_covariate Character or \code{NULL}.  Name of the actor covariate
#'   used by the \code{RateX} rung.  When \code{NULL} that rung is skipped and
#'   reported as skipped.
#' @param rungs Character vector naming the rungs to fit, from
#'   \code{"constant"}, \code{"outRate"}, \code{"outRateLog"},
#'   \code{"outRateInv"}, \code{"RateX"} (default: all five).
#' @param algorithm A \code{sienaAlgorithm} object, or \code{NULL} (default) to
#'   build one with \code{cond = FALSE}, \code{firstg = 0.02} and the
#'   \code{n3} and \code{seed} given here.  Unconditional estimation is the
#'   setting these panels need; conditional estimation fails on slowly changing
#'   panels with "unlikely to terminate this epoch".
#' @param n3 Integer. Phase-3 iterations when \code{algorithm} is built here
#'   (default \code{1000}).
#' @param seed Integer or \code{NULL}. Random seed when \code{algorithm} is
#'   built here.
#' @param nbrNodes Integer. Worker processes for \code{siena07} (default
#'   \code{1}).  Hold this fixed across any comparison: it moves the estimate.
#' @param continuation_threshold Numeric.  Overall convergence ratio at or
#'   above which one continuation run is budgeted before the rung is judged
#'   (default \code{0.25}).  Set to \code{Inf} to disable continuations.
#' @param projname Character or \code{NULL}. Base path for RSiena's output
#'   files; a per-rung suffix is appended.  Ignored when \code{algorithm} is
#'   supplied.
#' @param verbose Logical. Print each rung as it completes (default
#'   \code{TRUE}).
#' @return A \code{data.frame} with one row per rung and columns \code{rung},
#'   \code{rate_effect}, \code{status}, \code{max_abs_t}, \code{overall_ratio},
#'   \code{rate_estimate}, \code{rate_se}, \code{out_p}, \code{out_mhd},
#'   \code{in_p}, \code{in_mhd}, \code{obs_deg0}, \code{sim_deg0},
#'   \code{obs_deg1}, \code{sim_deg1}.  The fitted models are attached as the
#'   \code{"fits"} attribute.
#' @seealso \code{\link{gof_battery}}, \code{\link{boundary_screen}}
#' @export
#' @examples
#' \dontrun{
#' dat <- sienaDataCreate(dv = dv, size = coCovar(s), nodeSets = list(a, b))
#' eff <- getEffects(dat)
#' eff <- includeEffects(eff, inPop, outAct, name = "dv")
#' rate_ladder(dat, eff, rate_covariate = "size", n3 = 1000)
#' }
rate_ladder <- function(data, effects, varName = "dv",
                        rate_covariate = NULL,
                        rungs = c("constant", "outRate", "outRateLog",
                                  "outRateInv", "RateX"),
                        algorithm = NULL, n3 = 1000, seed = NULL,
                        nbrNodes = 1L, continuation_threshold = 0.25,
                        projname = NULL, verbose = TRUE) {

  if (!inherits(data, "siena"))
    stop("`data` must be a siena data object from sienaDataCreate().")
  if (!inherits(effects, "sienaEffects"))
    stop("`effects` must be a sienaEffects object from getEffects().")
  rungs <- match.arg(rungs, c("constant", "outRate", "outRateLog",
                              "outRateInv", "RateX"), several.ok = TRUE)
  if (is.null(projname)) projname <- file.path(tempdir(), "rate_ladder")

  na_row <- function(rung, rate_effect, status) data.frame(
    rung = rung, rate_effect = rate_effect, status = status,
    max_abs_t = NA_real_, overall_ratio = NA_real_,
    rate_estimate = NA_real_, rate_se = NA_real_,
    out_p = NA_real_, out_mhd = NA_real_, in_p = NA_real_, in_mhd = NA_real_,
    obs_deg0 = NA_real_, sim_deg0 = NA_real_,
    obs_deg1 = NA_real_, sim_deg1 = NA_real_,
    stringsAsFactors = FALSE)

  fits <- vector("list", length(rungs))
  names(fits) <- rungs
  rows <- vector("list", length(rungs))

  for (k in seq_along(rungs)) {
    rung <- rungs[k]
    rate_effect <- if (identical(rung, "constant")) NA_character_ else rung
    i1 <- if (identical(rung, "RateX")) rate_covariate else ""

    if (verbose) message(sprintf("=== [%d/%d] %s", k, length(rungs), rung))

    if (identical(rung, "RateX") && is.null(rate_covariate)) {
      rows[[k]] <- na_row(rung, rate_effect,
                          "SKIPPED: RateX needs `rate_covariate`")
      if (verbose) message("  skipped: no rate_covariate supplied")
      next
    }

    eff <- effects
    if (!identical(rung, "constant")) {
      eff <- .searchnet_turn_on(eff, rung, i1, type = "rate", name = varName,
                                verbose = verbose)
      if (isTRUE(attr(eff, "searchnet_missing"))) {
        rows[[k]] <- na_row(rung, rate_effect,
                            "NOT AVAILABLE: effect not offered for this data")
        next
      }
    }

    alg <- algorithm
    if (is.null(alg)) {
      alg_args <- list(projname = sprintf("%s_%s", projname, rung),
                       cond = FALSE, firstg = 0.02, n3 = n3)
      if (!is.null(seed)) alg_args$seed <- as.integer(seed)
      alg <- do.call(RSiena::sienaAlgorithmCreate, alg_args)
    }

    f <- try(RSiena::siena07(alg, data = data, effects = eff, batch = TRUE,
                             verbose = FALSE, useCluster = nbrNodes > 1,
                             nbrNodes = nbrNodes, returnDeps = TRUE),
             silent = TRUE)
    if (inherits(f, "try-error")) {
      rows[[k]] <- na_row(rung, rate_effect,
                          sprintf("FAILED: %s", sub("\n.*", "", as.character(f))))
      if (verbose) message("  estimation failed")
      next
    }

    ## One continuation before judging, so a rung is not condemned for a slow
    ## first run. Applied identically to every rung or the comparison is not one.
    if (is.finite(continuation_threshold) &&
        !is.null(f$tconv.max) && f$tconv.max >= continuation_threshold) {
      f2 <- try(RSiena::siena07(alg, data = data, effects = eff, batch = TRUE,
                                verbose = FALSE, useCluster = nbrNodes > 1,
                                nbrNodes = nbrNodes, returnDeps = TRUE,
                                prevAns = f), silent = TRUE)
      if (!inherits(f2, "try-error")) f <- f2
    }
    fits[[k]] <- f

    od <- .searchnet_gof_pm(f, RSiena::OutdegreeDistribution, varName = varName)
    id <- .searchnet_gof_pm(f, RSiena::IndegreeDistribution, varName = varName)
    fl <- .searchnet_floor_profile(f, varName = varName)

    est <- NA_real_; se <- NA_real_
    if (!identical(rung, "constant")) {
      ef <- as.data.frame(f$effects)
      j <- which(ef$shortName == rung)
      if (length(j)) {
        est <- f$theta[j][1]
        se <- suppressWarnings(sqrt(diag(f$covtheta))[j][1])
      }
    }

    rows[[k]] <- data.frame(
      rung = rung, rate_effect = rate_effect, status = "fitted",
      max_abs_t = if (length(f$tconv)) max(abs(f$tconv), na.rm = TRUE) else NA_real_,
      overall_ratio = if (length(f$tconv.max)) f$tconv.max else NA_real_,
      rate_estimate = est, rate_se = se,
      out_p = od$p, out_mhd = od$mhd, in_p = id$p, in_mhd = id$mhd,
      obs_deg0 = fl[["obs_deg0"]], sim_deg0 = fl[["sim_deg0"]],
      obs_deg1 = fl[["obs_deg1"]], sim_deg1 = fl[["sim_deg1"]],
      stringsAsFactors = FALSE)

    if (verbose)
      message(sprintf("  max|t| %.3f  ratio %.3f | outdeg p=%s MHD=%s | simulated isolates %s (data has %s)",
                      rows[[k]]$max_abs_t, rows[[k]]$overall_ratio,
                      format(od$p), format(round(od$mhd, 1)),
                      format(round(fl[["sim_deg0"]], 1)),
                      format(fl[["obs_deg0"]])))
  }

  out <- do.call(rbind, rows)
  attr(out, "fits") <- fits
  out
}
