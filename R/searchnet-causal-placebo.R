###############################################################################
## searchnet-causal-placebo.R
##
## Two design gates for the causal wrappers in searchnet-causal.R:
##
##   searchnet_placebo()               a real-time random-date placebo
##   searchnet_shock_support_check()   pre- vs post-shock structural support
##
## Both answer the same question before any estimate is read: does the design
## produce the result by construction? The placebo asks it of the estimator
## (pseudo-events on units that were never exposed must return zero). The
## support check asks it of the shocked world (a post-shock state with fewer
## ties, fewer occupied portfolios, or a lower attainable degree makes a lower
## outcome follow arithmetically, whatever the shock "did").
###############################################################################


# ============================================================================
#  searchnet_placebo
# ============================================================================

#' Real-time random-date placebo for a searchnet causal design
#'
#' Gates a difference-in-differences, synthetic control, or regression
#' discontinuity estimate by re-running the same estimator on pseudo-events
#' assigned to units that were \emph{never exposed}. If the design is sound,
#' the placebo estimates center on zero, and the real estimate is extreme
#' relative to them only when the shock had an effect.
#'
#' @section Real-time draws:
#' Pseudo-events are drawn \strong{period by period}, in calendar order. At
#' each step \eqn{t}, every never-exposed unit that is observed at \eqn{t} and
#' has not yet received a pseudo-event receives one with probability
#' \eqn{h_t}, the empirical treatment hazard of the real panel:
#' \deqn{h_t = \frac{\#\{i : g_i = t\}}{\#\{i \textrm{ observed at } t :
#'   g_i = 0 \textrm{ or } g_i \ge t\}},}
#' where \eqn{g_i} is unit \eqn{i}'s \code{first_treat} (0 for never treated).
#' The at-risk set at \eqn{t} uses only what is known at \eqn{t}, so the
#' pseudo-treated group reproduces the real timing distribution without
#' conditioning on the future. Units that are ever really exposed are dropped
#' from the placebo panel entirely, so no pseudo-event falls on or after a
#' real exposure, and no real exposure contaminates a pseudo-control.
#'
#' The tempting alternative, drawing each unit's pseudo-date uniformly from
#' the periods it was actually observed (survived), is \strong{not} a null:
#' it conditions on survival to the pseudo-date and beyond, so when the
#' outcome or the panel's composition is tied to survival it manufactures an
#' "effect" with no exposure at all. That design is refused
#' (\code{design = "uniform_survived"} is an error that says why).
#'
#' @section Gate verdict:
#' Two legs, reported separately:
#' \describe{
#'   \item{\code{centered}}{The placebo distribution's mean is within
#'     tolerance of zero. It fails only when \eqn{|\bar{b}|} exceeds both
#'     three Monte Carlo standard errors and \code{mean_tol}. A failure is a
#'     \emph{design defect}: the estimator returns an effect when exposure is
#'     random, and no real estimate from it should be read.}
#'   \item{\code{extreme}}{The real estimate's two-sided placebo p-value,
#'     \eqn{(1 + \#\{|b_r| \ge |\hat\beta|\}) / (1 + R)}, is at most
#'     \code{alpha}. A failure here on a centered placebo is the correct
#'     reading of a world with no effect, not a defect.}
#' }
#' \code{pass} is \code{TRUE} only when both legs hold. A placebo that passes
#' shows that the estimator is unbiased when exposure is random; it does not
#' make real exposure random. A null is a bound on an association.
#'
#' @param panel A \code{data.frame} from \code{\link{searchnet_causal_panel}}
#'   (columns \code{actor_id}, \code{step}, \code{outcome}, \code{treated},
#'   \code{first_treat}), with at least one never-treated
#'   (\code{first_treat == 0}) unit and at least one treated unit.
#' @param estimator The estimator to gate. One of
#'   \describe{
#'     \item{\code{"did"}}{\code{\link{searchnet_did}} (Callaway and
#'       Sant'Anna), summarized by the simple aggregate ATT. It needs at least
#'       five units in every timing group, so with few never-exposed units
#'       many draws are infeasible and are counted, not dropped silently.}
#'     \item{\code{"did_2x2"}}{A fast stacked two-by-two DID: for each treated
#'       unit with event step \eqn{g}, its post-minus-pre mean change minus
#'       the same change averaged over never-treated units, averaged over
#'       treated units. Handles staggered timing; needs no extra package.}
#'     \item{\code{"synth"}}{\code{\link{searchnet_synth}} per treated unit
#'       (donors: the never-treated units), summarized by the mean post-event
#'       gap averaged over treated units. Slow; use few draws.}
#'     \item{\code{"rd"}}{A sharp RD in event time on the treated units'
#'       mean outcome (cutoff 0, via \pkg{rdrobust}).}
#'   }
#'   or a function \code{function(panel, ...)} returning one number, called
#'   on the real panel and on every placebo panel.
#' @param n_draws Integer. Number of placebo draws (default 200).
#' @param seed Integer. Seed for the draws; the global RNG state is restored
#'   on exit.
#' @param design Character. \code{"real_time"} (the only accepted value).
#'   \code{"uniform_survived"} is recognized so that it can be refused with an
#'   explanation.
#' @param hazard Character. \code{"period"} (default) uses the step-specific
#'   empirical hazard \eqn{h_t}; \code{"pooled"} uses one constant hazard
#'   (all real events over all at-risk unit-steps), which spreads pseudo-events
#'   over the whole window. Both are real-time draws.
#' @param alpha Numeric. Level for the \code{extreme} leg (default 0.05).
#' @param mean_tol Numeric or \code{NULL}. Absolute tolerance on the placebo
#'   mean for the \code{centered} leg. \code{NULL} (default) uses a quarter of
#'   the placebo standard deviation.
#' @param min_valid Numeric in (0, 1]. Minimum share of feasible draws for a
#'   verdict (default 0.5). Below it, or below 20 feasible draws whatever
#'   \code{n_draws} is, the verdict is \code{"infeasible"}: the Monte Carlo
#'   error of fewer draws is too large for either leg.
#' @param \dots Passed to the estimator (e.g. arguments for
#'   \code{did::att_gt()}).
#'
#' @return An object of class \code{searchnet_placebo}: a list with
#'   \describe{
#'     \item{\code{estimate}}{The real estimate.}
#'     \item{\code{draws}}{Numeric vector of placebo estimates
#'       (\code{NA} for an infeasible draw).}
#'     \item{\code{n_pseudo}}{Integer vector, pseudo-treated units per draw.}
#'     \item{\code{placebo_mean}, \code{placebo_sd}, \code{mc_se}}{Summary of
#'       the feasible draws.}
#'     \item{\code{p_value}}{The real estimate's placebo p-value.}
#'     \item{\code{gate}}{A list: \code{centered}, \code{extreme},
#'       \code{pass}, \code{verdict}, \code{mean_tol}, \code{alpha}.}
#'     \item{\code{hazard}}{\code{data.frame} of \code{step}, \code{events},
#'       \code{at_risk}, \code{h}.}
#'     \item{\code{n_never}, \code{n_infeasible}, \code{estimator},
#'       \code{seed}, \code{design}}{Bookkeeping.}
#'   }
#'
#' @seealso \code{\link{searchnet_shock_support_check}},
#'   \code{\link{searchnet_did}}, \code{\link{searchnet_causal_panel}}.
#' @export
#' @examples
#' ## A synthetic panel: 30 units, 10 treated at step 6, planted effect 1.5.
#' set.seed(1)
#' panel <- expand.grid(actor_id = factor(1:30), step = 1:10)
#' tr <- as.integer(panel$actor_id) <= 10
#' panel$treated     <- as.integer(tr)
#' panel$first_treat <- ifelse(tr, 6L, 0L)
#' panel$outcome <- 0.2 * panel$step + rnorm(nrow(panel)) +
#'   ifelse(tr & panel$step >= 6, 1.5, 0)
#' pl <- searchnet_placebo(panel, estimator = "did_2x2", n_draws = 99, seed = 1)
#' pl
searchnet_placebo <- function(panel, estimator = c("did", "did_2x2", "synth", "rd"),
                              n_draws = 200, seed = 1L,
                              design = c("real_time", "uniform_survived"),
                              hazard = c("period", "pooled"),
                              alpha = 0.05, mean_tol = NULL, min_valid = 0.5,
                              ...) {
  design <- match.arg(design)
  if (identical(design, "uniform_survived"))
    stop(structure(
      class = c("searchnet_placebo_design_error", "error", "condition"),
      list(message = paste0(
        "searchnet_placebo() refuses design = \"uniform_survived\". Drawing ",
        "each unit's pseudo-date uniformly from the periods it was observed ",
        "conditions on the unit surviving to (and past) that date, so it is ",
        "not a null: when the outcome or the panel's composition is tied to ",
        "survival, it returns an 'effect' with no exposure at all. Use ",
        "design = \"real_time\", which assigns pseudo-events period by period ",
        "at the empirical treatment hazard among units at risk at that period."),
        call = sys.call(-1))))

  hazard <- match.arg(hazard)
  est_fun <- if (is.function(estimator)) estimator else NULL
  est_name <- if (is.null(est_fun)) match.arg(estimator) else "custom"

  required <- c("actor_id", "step", "outcome", "first_treat")
  miss <- setdiff(required, names(panel))
  if (length(miss))
    stop("Panel missing required columns: ", paste(miss, collapse = ", "),
         "\nUse searchnet_causal_panel() to create the panel.", call. = FALSE)
  n_draws <- as.integer(n_draws)
  if (length(n_draws) != 1L || is.na(n_draws) || n_draws < 1L)
    stop("`n_draws` must be a positive integer.", call. = FALSE)

  panel <- as.data.frame(panel)
  panel$actor_id <- as.factor(panel$actor_id)
  panel$step <- as.integer(panel$step)
  panel$first_treat <- as.integer(panel$first_treat)

  unit_ft <- tapply(panel$first_treat, panel$actor_id, function(x) x[1])
  unit_ft <- unit_ft[!is.na(unit_ft)]
  never <- names(unit_ft)[unit_ft == 0L]
  ever  <- names(unit_ft)[unit_ft > 0L]
  if (!length(ever))
    stop("searchnet_placebo(): the panel has no treated unit (no first_treat > 0), ",
         "so there is no treatment hazard to reproduce.", call. = FALSE)
  if (length(never) < 2L)
    stop(sprintf(paste0(
      "searchnet_placebo(): the panel has %d never-exposed unit(s). A real-time ",
      "placebo assigns pseudo-events to never-exposed units only, and needs at ",
      "least 2 of them (one pseudo-treated, one pseudo-control). An aggregate ",
      "shock that hits every actor has none; build a comparison arm from an ",
      "unshocked run, as in the causal-inference vignette."), length(never)),
      call. = FALSE)

  ## ---- empirical hazard, at risk in real time -------------------------------
  steps <- sort(unique(panel$step))
  obs <- split(as.character(panel$actor_id), panel$step)
  haz <- data.frame(step = steps, events = 0L, at_risk = 0L, h = 0)
  for (k in seq_along(steps)) {
    s <- steps[k]
    here <- unique(obs[[as.character(s)]])
    g <- unit_ft[here]
    haz$at_risk[k] <- sum(g == 0L | g >= s)
    haz$events[k]  <- sum(g == s)
  }
  haz$h <- ifelse(haz$at_risk > 0, haz$events / haz$at_risk, 0)
  if (identical(hazard, "pooled")) {
    h_pool <- sum(haz$events) / max(1, sum(haz$at_risk))
    haz$h <- ifelse(haz$at_risk > 0, h_pool, 0)
  }

  ## ---- the statistic -------------------------------------------------------
  stat <- function(p) {
    out <- tryCatch({
      v <- if (!is.null(est_fun)) est_fun(p, ...) else
        .sn_placebo_stat(p, est_name, ...)
      as.numeric(v)[1]
    }, error = function(e) NA_real_)
    if (length(out) != 1L || !is.finite(out)) NA_real_ else out
  }

  real <- if (!is.null(est_fun)) as.numeric(est_fun(panel, ...))[1] else
    .sn_placebo_stat(panel, est_name, ...)
  if (!is.finite(real))
    stop("searchnet_placebo(): the estimator returned no finite estimate on ",
         "the real panel.", call. = FALSE)

  ## ---- placebo panel: never-exposed units only ------------------------------
  base <- panel[as.character(panel$actor_id) %in% never, , drop = FALSE]
  base$actor_id <- droplevels(base$actor_id)
  base_obs <- split(as.character(base$actor_id), base$step)

  ## ---- real-time draws, deterministic under `seed` ---------------------------
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(as.integer(seed))

  draws <- rep(NA_real_, n_draws)
  n_pseudo <- integer(n_draws)
  for (r in seq_len(n_draws)) {
    pseudo <- setNames(integer(length(never)), never)
    for (k in seq_along(steps)) {
      if (haz$h[k] <= 0) next
      cand <- unique(base_obs[[as.character(steps[k])]])
      cand <- cand[pseudo[cand] == 0L]
      if (!length(cand)) next
      hit <- cand[stats::runif(length(cand)) < haz$h[k]]
      pseudo[hit] <- steps[k]
    }
    n_pseudo[r] <- sum(pseudo > 0L)
    if (n_pseudo[r] == 0L || n_pseudo[r] == length(never)) next
    p <- base
    p$first_treat <- unname(pseudo[as.character(p$actor_id)])
    p$treated <- as.integer(p$first_treat > 0L)
    p$period <- ifelse(p$treated == 1L & p$step >= p$first_treat, "post", "pre")
    draws[r] <- stat(p)
  }

  ok <- is.finite(draws)
  n_ok <- sum(ok)
  b <- draws[ok]
  pm  <- if (n_ok) mean(b) else NA_real_
  psd <- if (n_ok > 1L) stats::sd(b) else NA_real_
  mcse <- if (n_ok > 1L) psd / sqrt(n_ok) else NA_real_
  tol <- if (is.null(mean_tol)) 0.25 * psd else mean_tol
  pval <- if (n_ok) (1 + sum(abs(b) >= abs(real))) / (1 + n_ok) else NA_real_

  feasible <- n_ok >= max(20L, ceiling(min_valid * n_draws))
  centered <- feasible && (isTRUE(psd == 0) ||
                             !(abs(pm) > 3 * mcse && abs(pm) > tol))
  extreme <- feasible && pval <= alpha
  verdict <- if (!feasible) "infeasible" else if (!centered)
    "fail: placebo not centered on zero (design defect)" else if (!extreme)
    "fail: real estimate not extreme against the placebo (no evidence of an effect)" else
    "pass"

  structure(list(
    estimate     = real,
    draws        = draws,
    n_pseudo     = n_pseudo,
    placebo_mean = pm,
    placebo_sd   = psd,
    mc_se        = mcse,
    p_value      = pval,
    gate = list(centered = centered, extreme = extreme,
                pass = centered && extreme, verdict = verdict,
                mean_tol = tol, alpha = alpha),
    hazard       = haz,
    n_never      = length(never),
    n_ever       = length(ever),
    n_infeasible = n_draws - n_ok,
    estimator    = est_name,
    seed         = as.integer(seed),
    design       = paste0("real_time/", hazard)
  ), class = "searchnet_placebo")
}


#' @export
print.searchnet_placebo <- function(x, ...) {
  cat("searchnet real-time placebo (", x$estimator, ", ", x$design, ")\n", sep = "")
  cat(sprintf("  units: %d exposed, %d never exposed (placebo pool)\n",
              x$n_ever, x$n_never))
  cat(sprintf("  draws: %d feasible of %d (seed %d)\n",
              length(x$draws) - x$n_infeasible, length(x$draws), x$seed))
  cat(sprintf("  real estimate:  %.4f\n", x$estimate))
  cat(sprintf("  placebo mean:   %.4f (sd %.4f, MC se %.4f, tolerance %.4f)\n",
              x$placebo_mean, x$placebo_sd, x$mc_se, x$gate$mean_tol))
  cat(sprintf("  placebo p:      %.4f (alpha %.2f)\n", x$p_value, x$gate$alpha))
  cat("  gate:           ", x$gate$verdict, "\n", sep = "")
  invisible(x)
}


## One number per panel, for each built-in estimator.
.sn_placebo_stat <- function(panel, estimator, ...) {
  switch(estimator,
    did = {
      att <- suppressWarnings(suppressMessages(searchnet_did(panel, ...)))
      agg <- suppressWarnings(suppressMessages(did::aggte(att, type = "simple")))
      agg$overall.att
    },
    did_2x2 = .sn_did_2x2(panel),
    synth = .sn_synth_stat(panel, ...),
    rd = .sn_rd_event_stat(panel, ...),
    stop("Unknown estimator: ", estimator)
  )
}

## Stacked 2x2 DID: each treated unit's post-minus-pre change, minus the mean
## change of never-treated units over the same split, averaged over treated
## units. Units with no pre- or post-period observation are skipped.
.sn_did_2x2 <- function(panel) {
  ft <- tapply(panel$first_treat, panel$actor_id, function(x) x[1])
  ft <- ft[!is.na(ft)]
  tr <- names(ft)[ft > 0L]
  co <- names(ft)[ft == 0L]
  if (!length(tr) || !length(co)) return(NA_real_)
  id <- as.character(panel$actor_id)
  co_rows <- panel[id %in% co, , drop = FALSE]
  co_id <- as.character(co_rows$actor_id)
  co_cache <- list()
  eff <- vapply(tr, function(u) {
    g <- ft[[u]]
    y <- panel$outcome[id == u]; s <- panel$step[id == u]
    if (!any(s < g) || !any(s >= g)) return(NA_real_)
    key <- as.character(g)
    if (is.null(co_cache[[key]])) {
      pre  <- tapply(co_rows$outcome[co_rows$step < g],  co_id[co_rows$step < g],  mean)
      post <- tapply(co_rows$outcome[co_rows$step >= g], co_id[co_rows$step >= g], mean)
      both <- intersect(names(pre)[!is.na(pre)], names(post)[!is.na(post)])
      co_cache[[key]] <<- if (length(both)) mean(post[both] - pre[both]) else NA_real_
    }
    (mean(y[s >= g]) - mean(y[s < g])) - co_cache[[key]]
  }, numeric(1))
  if (all(is.na(eff))) NA_real_ else mean(eff, na.rm = TRUE)
}

## Synthetic control per treated unit; donors are the never-treated units.
.sn_synth_stat <- function(panel, ...) {
  if (!requireNamespace("Synth", quietly = TRUE))
    stop("Package 'Synth' is required for estimator = \"synth\".")
  ft <- tapply(panel$first_treat, panel$actor_id, function(x) x[1])
  ft <- ft[!is.na(ft)]
  tr <- names(ft)[ft > 0L]
  co <- names(ft)[ft == 0L]
  gaps <- vapply(tr, function(u) {
    sub <- panel[as.character(panel$actor_id) %in% c(u, co), , drop = FALSE]
    sub$actor_id <- droplevels(sub$actor_id)
    sub$treated <- as.integer(as.character(sub$actor_id) == u)
    sub$shock_step <- as.integer(ft[[u]])
    res <- tryCatch({
      utils::capture.output(sc <- suppressWarnings(
        searchnet_synth(sub, treated_unit = u, ...)))
      mean(sc$gap$gap[sc$gap$step >= ft[[u]]])
    }, error = function(e) NA_real_)
    res
  }, numeric(1))
  if (all(is.na(gaps))) NA_real_ else mean(gaps, na.rm = TRUE)
}

## Sharp RD in event time on the treated units' mean outcome, cutoff 0.
.sn_rd_event_stat <- function(panel, ...) {
  if (!requireNamespace("rdrobust", quietly = TRUE))
    stop("Package 'rdrobust' is required for estimator = \"rd\".")
  tp <- panel[panel$first_treat > 0L, , drop = FALSE]
  if (!nrow(tp)) return(NA_real_)
  tp$e <- tp$step - tp$first_treat
  agg <- stats::aggregate(outcome ~ e, data = tp, FUN = mean)
  if (sum(agg$e < 0) < 3L || sum(agg$e >= 0) < 3L) return(NA_real_)
  rd <- suppressWarnings(rdrobust::rdrobust(y = agg$outcome, x = agg$e, c = 0, ...))
  rd$coef[1]
}


# ============================================================================
#  searchnet_shock_support_check
# ============================================================================

#' Compare structural support before and after a shock
#'
#' A before-and-after comparison is only a comparison of behavior when the
#' two states offer comparable room to behave. If the post-shock state has
#' fewer ties, fewer active actors or components, fewer distinct portfolios,
#' or a lower attainable degree, a lower post-shock outcome can follow
#' arithmetically from the poorer world rather than from any response to the
#' shock. This check counts those primitives in equal-length windows on
#' either side of the shock and warns when the post-shock state is
#' structurally impoverished.
#'
#' @details
#' For a simulated environment the metrics are computed from the bipartite
#' state at each step (\code{env$bi_env_arr}):
#' \describe{
#'   \item{\code{ties}}{Total actor-component ties.}
#'   \item{\code{active_actors}, \code{active_components}}{Actors and
#'     components with at least one tie.}
#'   \item{\code{distinct_portfolios}}{Distinct non-empty component sets
#'     held by actors at a step.}
#'   \item{\code{portfolios_visited}}{Distinct non-empty portfolios seen
#'     anywhere in the window (windows have equal length).}
#'   \item{\code{max_K_AC}, \code{max_K_CA}, \code{max_K_AA},
#'     \code{max_K_CC}}{The largest \{K\} degree reached: actor scope,
#'     component popularity, actor co-membership, component co-occurrence.}
#' }
#' Per-step metrics are averaged over the window. For a panel (from
#' \code{\link{searchnet_causal_panel}}) the metrics are generic: units
#' observed, distinct outcome values, the outcome's range, and (for a
#' nonnegative outcome such as a \{K\} degree) its total.
#'
#' A metric is flagged when its post-shock value is below
#' \eqn{(1 - \mathrm{tol})} times its pre-shock value. A flag does not say
#' the outcome difference is mechanical; it says the comparison cannot rule
#' that out. The remedy is to make the post-state a relabeling or permutation
#' of the pre-state, holding the structural primitive constant, or at minimum
#' to report these counts beside any before-and-after level claim.
#'
#' @param env_or_panel A simulated \code{SaomNkRSienaBiEnv}, or a panel
#'   \code{data.frame} with columns \code{step} and \code{outcome} (and
#'   \code{actor_id}).
#' @param shock_step Integer. The first post-shock step.
#' @param window Integer or \code{NULL}. Steps on each side of the shock.
#'   \code{NULL} (default) uses the largest equal window available.
#' @param tol Numeric in (0, 1). Relative shrinkage that flags a metric
#'   (default 0.25).
#' @param warn Logical. Emit a warning (class
#'   \code{searchnet_impoverished_support_warning}) when any metric is
#'   flagged (default \code{TRUE}).
#'
#' @return An object of class \code{searchnet_shock_support}: a list with
#'   \code{metrics} (a \code{data.frame} of \code{metric}, \code{pre},
#'   \code{post}, \code{ratio}, \code{flagged}), \code{impoverished}
#'   (logical), \code{flagged} (names), \code{verdict}, \code{window},
#'   \code{source} (\code{"env"} or \code{"panel"}), and \code{tol}.
#'
#' @seealso \code{\link{searchnet_placebo}}, \code{\link{searchnet_causal_panel}}.
#' @export
#' @examples
#' ## A panel whose outcome is a count that collapses after step 6.
#' panel <- expand.grid(actor_id = factor(1:8), step = 1:10)
#' panel$outcome <- ifelse(panel$step < 6, 4 + as.integer(panel$actor_id) %% 3, 1)
#' chk <- searchnet_shock_support_check(panel, shock_step = 6, warn = FALSE)
#' chk
searchnet_shock_support_check <- function(env_or_panel, shock_step, window = NULL,
                                          tol = 0.25, warn = TRUE) {
  stopifnot(is.numeric(shock_step), length(shock_step) == 1L)
  stopifnot(is.numeric(tol), tol > 0, tol < 1)
  shock_step <- as.integer(shock_step)

  if (inherits(env_or_panel, "SaomNkRSienaBiEnv")) {
    env <- env_or_panel
    .searchnet_require_path(env, "searchnet_shock_support_check()")
    arr <- env$bi_env_arr
    if (is.null(arr) || length(dim(arr)) != 3L)
      stop("No bipartite path ($bi_env_arr) found. Run saomnk_run() first.",
           call. = FALSE)
    steps <- seq_len(dim(arr)[3])
    win <- .sn_support_window(steps, shock_step, window)
    per_step <- function(t) {
      B <- arr[, , t]
      B <- matrix(as.numeric(B > 0), nrow = dim(arr)[1])
      AA <- B %*% t(B); diag(AA) <- 0
      CC <- t(B) %*% B; diag(CC) <- 0
      rs <- rowSums(B)
      rows <- if (any(rs > 0)) apply(B[rs > 0, , drop = FALSE], 1, paste, collapse = "") else character(0)
      c(ties = sum(B),
        active_actors = sum(rs > 0),
        active_components = sum(colSums(B) > 0),
        distinct_portfolios = length(unique(rows)),
        max_K_AC = max(rs),
        max_K_CA = max(colSums(B)),
        max_K_AA = max(rowSums(AA > 0)),
        max_K_CC = max(colSums(CC > 0)))
    }
    visited <- function(ts) {
      rows <- unlist(lapply(ts, function(t) {
        B <- matrix(as.numeric(arr[, , t] > 0), nrow = dim(arr)[1])
        rs <- rowSums(B)
        if (any(rs > 0)) apply(B[rs > 0, , drop = FALSE], 1, paste, collapse = "") else character(0)
      }))
      length(unique(rows))
    }
    pre_m  <- rowMeans(vapply(win$pre,  per_step, numeric(8)))
    post_m <- rowMeans(vapply(win$post, per_step, numeric(8)))
    pre_m  <- c(pre_m,  portfolios_visited = visited(win$pre))
    post_m <- c(post_m, portfolios_visited = visited(win$post))
    source <- "env"
  } else if (is.data.frame(env_or_panel)) {
    panel <- env_or_panel
    miss <- setdiff(c("step", "outcome"), names(panel))
    if (length(miss))
      stop("Panel missing required columns: ", paste(miss, collapse = ", "),
           call. = FALSE)
    steps <- sort(unique(as.integer(panel$step)))
    win <- .sn_support_window(steps, shock_step, window)
    nonneg <- all(panel$outcome >= 0, na.rm = TRUE)
    per_step <- function(t) {
      y <- panel$outcome[panel$step == t]
      y <- y[is.finite(y)]
      c(units_observed = length(y),
        distinct_values = length(unique(y)),
        outcome_range = if (length(y)) diff(range(y)) else 0,
        outcome_total = if (nonneg) sum(y) else NA_real_)
    }
    pre_m  <- rowMeans(vapply(win$pre,  per_step, numeric(4)))
    post_m <- rowMeans(vapply(win$post, per_step, numeric(4)))
    if (!nonneg) { pre_m <- pre_m[-4]; post_m <- post_m[-4] }
    source <- "panel"
  } else {
    stop("`env_or_panel` must be a simulated SaomNkRSienaBiEnv or a data.frame.",
         call. = FALSE)
  }

  ratio <- ifelse(pre_m > 0, post_m / pre_m, NA_real_)
  flagged <- !is.na(ratio) & ratio < (1 - tol)
  metrics <- data.frame(metric = names(pre_m), pre = unname(pre_m),
                        post = unname(post_m), ratio = unname(ratio),
                        flagged = unname(flagged), stringsAsFactors = FALSE)
  imp <- any(flagged)
  verdict <- if (imp) "impoverished" else "comparable"

  if (imp && isTRUE(warn)) {
    f <- metrics[metrics$flagged, ]
    detail <- paste(sprintf("%s %.3g -> %.3g (%.0f%% lower)", f$metric, f$pre,
                            f$post, 100 * (1 - f$ratio)), collapse = "; ")
    warning(structure(
      class = c("searchnet_impoverished_support_warning", "warning", "condition"),
      list(message = paste0(
        "searchnet_shock_support_check(): the post-shock state is structurally ",
        "impoverished relative to the pre-shock state (", detail, "). A ",
        "before-and-after outcome difference can follow arithmetically from the ",
        "smaller support. Hold the structural primitive constant (make the ",
        "post-state a relabeling of the pre-state) or report these counts beside ",
        "any level comparison."),
        call = NULL)))
  }

  structure(list(metrics = metrics, impoverished = imp,
                 flagged = metrics$metric[metrics$flagged], verdict = verdict,
                 window = win, source = source, tol = tol,
                 shock_step = shock_step),
            class = "searchnet_shock_support")
}

.sn_support_window <- function(steps, shock_step, window) {
  pre_all  <- steps[steps < shock_step]
  post_all <- steps[steps >= shock_step]
  if (length(pre_all) < 1L || length(post_all) < 1L)
    stop(sprintf("shock_step = %d leaves no step on one side (steps %d to %d).",
                 shock_step, min(steps), max(steps)), call. = FALSE)
  w <- min(length(pre_all), length(post_all))
  if (!is.null(window)) w <- min(w, as.integer(window))
  list(pre = utils::tail(pre_all, w), post = utils::head(post_all, w), width = w)
}


#' @export
print.searchnet_shock_support <- function(x, ...) {
  cat(sprintf("searchnet shock support check (%s; %d steps each side of step %d)\n",
              x$source, x$window$width, x$shock_step))
  m <- x$metrics
  m$pre <- signif(m$pre, 4); m$post <- signif(m$post, 4); m$ratio <- round(m$ratio, 3)
  print(m, row.names = FALSE)
  cat(sprintf("verdict: %s (flag when post < %.2f x pre)\n", x$verdict, 1 - x$tol))
  invisible(x)
}
