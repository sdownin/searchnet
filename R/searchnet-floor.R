###############################################################################
## searchnet-floor.R
##
## Identification and power for an ESTIMATED actor floor
## (searchnet_degree_bounds(actor = c(min = 1), estimate_floor = TRUE)).
##
## An estimated floor is the free outIso evaluation effect: the cost of
## dropping the last tie, relative to the other effects in the model. Its
## method-of-moments target is the number of actors holding nothing at the end
## of each period, so it is informed only by observed transitions into and out
## of degree 0. A panel in which no actor ever drops to 0 carries no
## information on the size of that cost beyond a bound, and is reported as
## non-identified, never as a null.
###############################################################################

.FLOOR_ID_STATUS <- c("identified", "first_tie_only", "bound_only", "uninformative")

#' Can an Estimated Floor Be Identified From These Data?
#'
#' Counts, per wave transition, the observed transitions of actors to and from
#' degree 0 in a bipartite actor-by-component panel, and says whether an
#' estimated actor floor (\code{outIso} under
#' \code{searchnet_degree_bounds(actor = c(min = 1), estimate_floor = TRUE)})
#' is identifiable in principle. Call it before estimating.
#'
#' @section What is counted:
#' For each transition from wave \eqn{w} to \eqn{w + 1}: \code{at_risk}, the
#' actors holding exactly one component at \eqn{w} (one drop from degree 0);
#' \code{to_zero}, actors holding at least one component at \eqn{w} and none
#' at \eqn{w + 1} (observed last-tie drops); \code{from_zero}, actors holding
#' none at \eqn{w} and at least one at \eqn{w + 1}; \code{stay_zero}, actors
#' holding none at both. These are NET changes between observations: an actor
#' that dropped its last tie and added another between two waves is not seen,
#' so the counts are lower bounds on the events that occurred.
#'
#' @section Status:
#' \describe{
#'   \item{\code{"identified"}}{at least one observed transition to degree 0
#'     (and not every actor ends at 0): the coefficient is identified in
#'     principle. Its precision still depends on how many drops there are;
#'     see \code{\link{searchnet_floor_power}}.}
#'   \item{\code{"first_tie_only"}}{no transition to degree 0, but actors
#'     already holding nothing are observed. The evaluation coefficient is
#'     then informed only by those actors' first ties, which says nothing
#'     about reluctance to drop the last tie: for that, only a bound.}
#'   \item{\code{"bound_only"}}{no actor is observed at degree 0, but actors
#'     at degree 1 were at risk. The data say the cost of dropping the last
#'     tie is large relative to the other effects, without saying how large:
#'     only a bound can be reported, and an estimate is not a finding.}
#'   \item{\code{"uninformative"}}{no actor at or near degree 0 in any wave:
#'     the data carry no information on the floor.}
#' }
#'
#' @param x An \eqn{M \times N \times W}{M x N x W} array, a list of
#'   \eqn{M \times N}{M x N} matrices (one per wave), a bipartite
#'   \code{sienaDependent}, or a \code{siena} data object (its first bipartite
#'   dependent network). Missing ties count as absent. At least two waves.
#' @return An object of class \code{"searchnet_floor_identification"}: a list
#'   with \code{by_wave} (one row per transition: \code{from_wave},
#'   \code{to_wave}, \code{actors}, \code{at_risk}, \code{to_zero},
#'   \code{from_zero}, \code{stay_zero}, \code{isolates_end}), \code{totals}
#'   (the column sums as a named integer vector), \code{status},
#'   \code{identifiable} (logical) and \code{message}.
#' @seealso \code{\link{searchnet_degree_bounds}},
#'   \code{\link{searchnet_floor_power}}
#' @export
#' @examples
#' B1 <- matrix(c(1, 0, 0,
#'                0, 1, 1,
#'                1, 1, 0), 3, 3, byrow = TRUE)
#' B2 <- B1; B2[1, 1] <- 0           # actor 1 drops its last component
#' searchnet_floor_identification(list(B1, B2))
#' searchnet_floor_identification(list(B1, B1))   # no drop: only a bound
searchnet_floor_identification <- function(x) {
  arr <- .searchnet_bounds_as_array(x)
  W <- dim(arr)[3]
  if (W < 2L)
    stop("searchnet_floor_identification() needs at least two waves.", call. = FALSE)
  deg <- apply(arr, 3, rowSums)
  deg <- matrix(deg, nrow = dim(arr)[1])
  by_wave <- do.call(rbind, lapply(seq_len(W - 1L), function(w) {
    d0 <- deg[, w]; d1 <- deg[, w + 1L]
    data.frame(from_wave = w, to_wave = w + 1L, actors = length(d0),
               at_risk = sum(d0 == 1), to_zero = sum(d0 > 0 & d1 == 0),
               from_zero = sum(d0 == 0 & d1 > 0), stay_zero = sum(d0 == 0 & d1 == 0),
               isolates_end = sum(d1 == 0))
  }))
  cols <- c("at_risk", "to_zero", "from_zero", "stay_zero", "isolates_end")
  totals <- vapply(cols, function(k) as.integer(sum(by_wave[[k]])), integer(1))
  all_zero_end <- all(by_wave$isolates_end == by_wave$actors)
  status <- if (totals[["to_zero"]] > 0L && !all_zero_end) "identified"
            else if (totals[["from_zero"]] + totals[["stay_zero"]] > 0L) "first_tie_only"
            else if (totals[["at_risk"]] > 0L) "bound_only"
            else "uninformative"
  if (totals[["to_zero"]] > 0L && all_zero_end) status <- "bound_only"
  msg <- switch(status,
    identified = sprintf(paste0(
      "%d observed transition(s) to degree 0 over %d period(s): an estimated floor ",
      "is identified in principle. Its precision depends on that count."),
      totals[["to_zero"]], W - 1L),
    first_tie_only = paste0(
      "No observed transition to degree 0. Actors already at 0 inform the outIso ",
      "coefficient only through their first ties, which says nothing about ",
      "reluctance to drop the last tie: for that, only a bound can be reported."),
    bound_only = if (all_zero_end)
      paste0("Every actor ends a period at degree 0: the coefficient is not ",
             "identified (a bound in the other direction).")
    else sprintf(paste0(
      "No observed transition to degree 0, with %d actor-period(s) at degree 1 at ",
      "risk: the cost of dropping the last tie is not identified. Only a bound can ",
      "be reported; an estimate would not be a finding, and this is not a null."),
      totals[["at_risk"]]),
    uninformative = paste0(
      "No actor at or near degree 0 in any wave: the data carry no information ",
      "on the floor."))
  out <- list(by_wave = by_wave, totals = totals, status = status,
              identifiable = identical(status, "identified"), message = msg)
  class(out) <- "searchnet_floor_identification"
  out
}

#' @rdname searchnet_floor_identification
#' @param ... Unused.
#' @method print searchnet_floor_identification
#' @export
print.searchnet_floor_identification <- function(x, ...) {
  cat("SAOM-NK estimated-floor identification (transitions to and from degree 0)\n")
  print(x$by_wave, row.names = FALSE)
  cat(sprintf("  totals: %s\n", paste(sprintf("%s %d", names(x$totals), x$totals),
                                      collapse = ", ")))
  cat(sprintf("  status: %s\n  %s\n", x$status, x$message))
  cat("  Counts are net changes between waves (lower bounds on events).\n")
  invisible(x)
}


#' Power and Identification of an Estimated Floor, by Observed Drops
#'
#' Summarizes one or more \code{\link{searchnet_recovery}} runs made with an
#' estimated floor (\code{searchnet_degree_bounds(actor = c(min = 1),
#' estimate_floor = TRUE)}) by the number of observed last-tie drops in each
#' replication (\code{floor_drops}, transitions to degree 0 counted by
#' \code{\link{searchnet_floor_identification}}). For each planted
#' \code{outIso} value and each bin of that count it reports how many
#' replications fell there, how many were non-identified, not converged or
#' failed, and, over the converged ones, the bias, Wald coverage, rejection
#' rate of \eqn{H_0: \mathrm{outIso} = 0} and the approximate minimum
#' detectable effect (MDE).
#'
#' @section Reading it:
#' The count of observed drops is an outcome of each simulated panel, not a
#' design setting, so bins are conditional summaries: a bin with few
#' replications has large Monte Carlo error (\code{mcse_*}), and because the
#' count is correlated with the estimate, bias and coverage within a bin are
#' not estimator bias. Read the bins for precision (\code{mean_se},
#' \code{mde}); read bias in design cells where few replications are
#' non-identified, since summarizing only the estimable replications selects
#' on the outcome that identifies the parameter. Replications with
#' zero drops are non-identified by construction; they are counted, never
#' folded into a null. The MDE is
#' \eqn{(z_{1-\alpha/2} + z_{\mathrm{power}})\,\overline{\mathrm{SE}}}, the
#' same approximation \code{\link{searchnet_recovery}} uses. The estimated
#' parameter is reluctance relative to the other effects of the planted
#' model, at the stated design; nothing more is shown.
#'
#' @param ... One or more \code{"searchnet_recovery"} objects run with an
#'   estimated floor, or a single list of them.
#' @param breaks Numeric breaks for binning \code{floor_drops}, as in
#'   \code{\link{cut}} with \code{right = FALSE}. Default
#'   \code{c(0, 1, 5, 10, 20, 40, Inf)}: 0, 1-4, 5-9, 10-19, 20-39, 40+.
#' @param by_true Logical. Report by planted \code{outIso} value as well as by
#'   bin (\code{TRUE}, default), or pooled over planted values (\code{FALSE}).
#' @param alpha,power,ci_level Test level, target power and Wald interval
#'   level. Defaults 0.05, 0.80, 0.95.
#' @return A data frame with one row per (planted value, bin): \code{true},
#'   \code{drops}, \code{drops_mean}, \code{n_reps}, \code{n_non_identified},
#'   \code{n_not_converged}, \code{n_error}, \code{n_converged},
#'   \code{mean_est}, \code{bias}, \code{mcse_bias}, \code{coverage},
#'   \code{rejection}, \code{mcse_rejection}, \code{mean_se}, \code{mde}.
#' @seealso \code{\link{searchnet_recovery}},
#'   \code{\link{searchnet_floor_identification}}
#' @export
searchnet_floor_power <- function(..., breaks = c(0, 1, 5, 10, 20, 40, Inf),
                                  by_true = TRUE, alpha = 0.05, power = 0.80,
                                  ci_level = 0.95) {
  recs <- list(...)
  if (length(recs) == 1L && is.list(recs[[1L]]) &&
      !inherits(recs[[1L]], "searchnet_recovery"))
    recs <- recs[[1L]]
  if (!length(recs) || !all(vapply(recs, inherits, logical(1), "searchnet_recovery")))
    stop("searchnet_floor_power() takes searchnet_recovery objects.", call. = FALSE)
  rows <- lapply(recs, function(r) {
    if (is.null(r$reps$floor_drops) || !isTRUE(r$settings$degree_bounds$estimate_floor))
      stop("each recovery run must use an estimated floor ",
           "(searchnet_degree_bounds(actor = c(min = 1), estimate_floor = TRUE)).",
           call. = FALSE)
    est <- r$estimates[r$estimates$effect == "outIso", c("rep", "estimate", "se"),
                       drop = FALSE]
    d <- merge(r$reps[, c("rep", "status", "floor_drops")], est, by = "rep",
               all.x = TRUE)
    d$true <- unname(r$theta_true[["outIso"]])
    d
  })
  d <- do.call(rbind, rows)
  d$drops <- cut(d$floor_drops, breaks = breaks, right = FALSE,
                 include.lowest = TRUE, dig.lab = 6)
  levels(d$drops) <- .floor_bin_labels(breaks)
  if (!isTRUE(by_true)) d$true <- NA_real_
  z_ci  <- stats::qnorm(1 - (1 - ci_level) / 2)
  z_rej <- stats::qnorm(1 - alpha / 2)
  keys <- unique(d[, c("true", "drops")])
  keys <- keys[order(-keys$true, as.integer(keys$drops)), , drop = FALSE]
  out <- do.call(rbind, lapply(seq_len(nrow(keys)), function(k) {
    ## %in% matches NA to NA: a failed simulation has no drop count.
    s <- d[d$true %in% keys$true[k] & d$drops %in% keys$drops[k], , drop = FALSE]
    cv <- s[s$status == "converged" & is.finite(s$se), , drop = FALSE]
    n <- nrow(cv)
    tv <- if (isTRUE(by_true)) keys$true[k] else NA_real_
    err <- if (isTRUE(by_true)) cv$estimate - tv else rep(NA_real_, n)
    cover <- if (n && isTRUE(by_true)) mean(abs(err) <= z_ci * cv$se) else NA_real_
    rej <- if (n) mean(abs(cv$estimate / cv$se) > z_rej) else NA_real_
    mse <- if (n) mean(cv$se) else NA_real_
    data.frame(
      true = tv,
      drops = if (is.na(keys$drops[k])) "unknown" else as.character(keys$drops[k]),
      drops_mean = if (all(is.na(s$floor_drops))) NA_real_
                   else mean(s$floor_drops, na.rm = TRUE),
      n_reps = nrow(s),
      n_non_identified = sum(s$status == "non_identified"),
      n_not_converged = sum(s$status == "not_converged"),
      n_error = sum(s$status %in% c("error_simulation", "error_estimation")),
      n_converged = n,
      mean_est = if (n) mean(cv$estimate) else NA_real_,
      bias = if (n) mean(err) else NA_real_,
      mcse_bias = if (n > 1L) stats::sd(cv$estimate) / sqrt(n) else NA_real_,
      coverage = cover,
      rejection = rej,
      mcse_rejection = if (n) sqrt(rej * (1 - rej) / n) else NA_real_,
      mean_se = mse,
      mde = if (n) .recovery_mde(mse, alpha, power) else NA_real_,
      stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  attr(out, "settings") <- list(alpha = alpha, power = power, ci_level = ci_level,
                                breaks = breaks, by_true = isTRUE(by_true))
  out
}

.floor_bin_labels <- function(breaks) {
  lo <- breaks[-length(breaks)]; hi <- breaks[-1L]
  vapply(seq_along(lo), function(i) {
    if (is.infinite(hi[i])) sprintf("%g+", lo[i])
    else if (hi[i] - lo[i] == 1) sprintf("%g", lo[i])
    else sprintf("%g-%g", lo[i], hi[i] - 1)
  }, character(1))
}
