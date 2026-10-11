# ---------------------------------------------------------------------------- #
#  searchnet-stationarity.R
#
#  Within-run stationarity check for one simulated SAOM-NK path.
#
#  WHY THIS FILE EXISTS
#  --------------------
#  searchnet_ergodicity_sweep() asks whether runs from different starts end in
#  the same place. It does not ask whether ONE run, read at its end, is a draw
#  from the stationary law at all. Two failure modes look alike at the end of a
#  run and must be kept apart:
#
#  * trending: the run is still moving toward the stationary law, so its end
#    state is a point on the way, not a draw from it;
#  * absorbed: the run stopped changing. A frozen end state is a hitting point
#    (where the chain stopped), not an equilibrium distribution. Reading it as
#    one names the distribution by the theory rather than by what the
#    simulation did.
#
#  A third outcome is that the run is too short to tell. A fourth, the one a
#  user hopes for, is that no trend is detectable. That is a BOUND, not a
#  proof: the check reports the smallest drift it could have detected (80%
#  power at the stated alpha), and "stationary" means only that no drift at
#  least that large was found.
#
#  The trajectory is read from the run's per-ministep path ($bi_env_arr, the
#  {K} long tables, $actor_util_df). The legacy replay route is refused,
#  because its $bi_env_arr is not a path.
# ---------------------------------------------------------------------------- #

.SEARCHNET_STATIONARITY_STATS <- c("density", "mean_K_AC", "mean_K_CA",
                                   "mean_K_AA", "mean_K_CC", "utility")

## Verdicts from best to worst; the overall verdict is the worst one present.
.SEARCHNET_STATIONARITY_LEVELS <- c("stationary", "too_short", "trending",
                                    "absorbed")


## Long-run variance (spectral density at frequency zero) of a series, from an
## autoregressive fit with the order chosen by AIC. This is the estimator coda
## uses for geweke.diag() and effectiveSize() (spectrum0.ar), written with
## stats::ar() so no extra dependency is needed. A fitted AR polynomial at or
## past a unit root gives an infinite long-run variance, so the series carries
## no usable information and its effective sample size is zero.
.searchnet_lrv <- function(y) {
  n <- length(y)
  v <- stats::var(y)
  if (n < 3L || !is.finite(v) || v <= 0) return(0)
  fit <- tryCatch(
    stats::ar(y - mean(y), aic = TRUE,
              order.max = max(1L, min(n - 2L, floor(10 * log10(n)))),
              method = "yule-walker", demean = FALSE),
    error = function(e) NULL)
  if (is.null(fit)) return(v)
  denom <- 1 - sum(fit$ar)
  if (!is.finite(denom) || denom <= 1e-8) return(Inf)
  as.numeric(fit$var.pred) / denom^2
}

## Effective sample size of a series from its long-run variance, capped at n.
.searchnet_ess <- function(y, lrv = .searchnet_lrv(y)) {
  n <- length(y)
  v <- stats::var(y)
  if (n < 3L || !is.finite(v) || v <= 0) return(0)
  if (!is.finite(lrv) || lrv <= 0) return(0)
  min(n, n * v / lrv)
}

## Share of the run since a series last changed: (T - t_last) / T, where
## t_last is the last step whose value differs from the step before it (0 if
## it never changed after the first recorded step).
.searchnet_share_since_change <- function(y, tol = 1e-12) {
  T <- length(y)
  if (T < 2L) return(1)
  ch <- which(abs(diff(y)) > tol)
  last <- if (length(ch)) max(ch) + 1L else 1L
  (T - last) / T
}


## Per-step trajectories of the requested statistics, one column each, read
## from the run's path. Aborts if the tables disagree with the path.
.searchnet_stationarity_trajectories <- function(env, statistics) {
  arr <- env$bi_env_arr
  if (is.null(arr) || length(dim(arr)) != 3L || dim(arr)[3] < 1L)
    stop("No simulated path ($bi_env_arr) on this environment. Run saomnk_run() first.",
         call. = FALSE)
  M <- dim(arr)[1]; N <- dim(arr)[2]; T <- dim(arr)[3]
  flat <- matrix(as.numeric(arr), nrow = M * N, ncol = T)
  dens <- colSums(flat) / (M * N)

  by_step <- function(df, col, what) {
    if (is.null(df) || !nrow(df))
      stop(sprintf("The run carries no %s table; cannot read its trajectory.", what),
           call. = FALSE)
    step <- as.integer(df$chain_step_id)
    v <- tapply(as.numeric(df[[col]]), step, mean)
    if (length(v) != T || !identical(as.integer(names(v)), seq_len(T)))
      stop(sprintf(paste0("The %s table covers %d steps; the path has %d. ",
                          "The trajectory tables do not belong to this path; aborting."),
                   what, length(v), T), call. = FALSE)
    as.numeric(v)
  }

  out <- matrix(NA_real_, nrow = T, ncol = length(statistics),
                dimnames = list(NULL, statistics))
  for (s in statistics) {
    out[, s] <- switch(s,
      density   = dens,
      mean_K_AC = by_step(env$K_AC_df, "value", "K_AC"),
      mean_K_CA = by_step(env$K_CA_df, "value", "K_CA"),
      mean_K_AA = by_step(env$K_AA_df, "value", "K_AA"),
      mean_K_CC = by_step(env$K_CC_df, "value", "K_CC"),
      utility   = by_step(env$actor_util_df, "utility", "actor utility"))
  }

  ## Abort gate: scope and popularity are the path's row and column sums, so
  ## their means are N and M times its density. A mismatch means the tables
  ## were computed from a different network sequence than $bi_env_arr.
  if ("mean_K_AC" %in% statistics &&
      max(abs(out[, "mean_K_AC"] - N * dens)) > 1e-8)
    stop("K_AC trajectory disagrees with the path's density; aborting.", call. = FALSE)
  if ("mean_K_CA" %in% statistics &&
      max(abs(out[, "mean_K_CA"] - M * dens)) > 1e-8)
    stop("K_CA trajectory disagrees with the path's density; aborting.", call. = FALSE)

  ## Network-level change record, including the move off the starting state.
  start <- env$bi_env_arr_initial
  prev <- if (!is.null(start) && length(start) == M * N)
    cbind(as.numeric(start), flat[, -T, drop = FALSE]) else
    cbind(flat[, 1L], flat[, -T, drop = FALSE])
  changed <- colSums(abs(flat - prev)) > 0
  last_net <- if (any(changed)) max(which(changed)) else 0L

  list(traj = out, M = M, N = N, T = T,
       network = list(n_steps = T, n_changes = sum(changed),
                      last_change_step = last_net,
                      share_since_last_change = (T - last_net) / T))
}


## One statistic, windows method: slope of window means on window index, with
## Var(window mean) = S(0) / L from the long-run variance of the detrended
## segment.
.searchnet_stationarity_windows <- function(y, n_windows, alpha, power = 0.8) {
  n <- length(y)
  L <- n %/% n_windows
  yy <- y[(n - n_windows * L + 1L):n]
  k <- seq_len(n_windows)
  m <- vapply(k, function(w) mean(yy[((w - 1L) * L + 1L):(w * L)]), numeric(1))
  t_idx <- seq_along(y)
  res <- stats::residuals(stats::lm(y ~ t_idx))
  lrv <- .searchnet_lrv(res)
  sxx <- sum((k - mean(k))^2)
  slope <- sum((k - mean(k)) * (m - mean(m))) / sxx
  se <- sqrt(lrv / L / sxx)
  z <- if (is.finite(se) && se > 0) slope / se else NA_real_
  crit <- stats::qnorm(1 - alpha / 2) + stats::qnorm(power)
  list(drift = slope * (n_windows - 1L), se_drift = se * (n_windows - 1L),
       z = z, p_value = if (is.na(z)) NA_real_ else 2 * stats::pnorm(-abs(z)),
       mdd = crit * se * (n_windows - 1L),
       sd_within = stats::sd(res), ess = .searchnet_ess(res, lrv),
       window_means = m)
}

## p-value of the slope of window means on window index, from an ordinary
## regression of the n_windows means (n_windows - 2 df). It uses no estimate
## of the long-run variance, so it is the confirmation a low-ESS trend needs.
## NA with fewer than three windows or means that lie exactly on a line.
.searchnet_window_means_test <- function(y, n_windows) {
  n <- length(y)
  K <- max(3L, as.integer(n_windows))
  L <- n %/% K
  if (L < 1L) return(NA_real_)
  yy <- y[(n - K * L + 1L):n]
  k <- seq_len(K)
  m <- vapply(k, function(w) mean(yy[((w - 1L) * L + 1L):(w * L)]), numeric(1))
  fit <- stats::lm(m ~ k)
  cf <- tryCatch(summary(fit)$coefficients, warning = function(w) NULL,
                 error = function(e) NULL)
  if (is.null(cf) || nrow(cf) < 2L || !is.finite(cf[2L, 4L])) {
    rss <- sum(stats::residuals(fit)^2)
    if (rss == 0 && stats::coef(fit)[2L] != 0) return(0)
    return(NA_real_)
  }
  cf[2L, 4L]
}

## One statistic, Geweke method: first 10% against last 50% of the segment,
## each mean's variance from its own long-run variance (as coda does).
.searchnet_stationarity_geweke <- function(y, alpha, power = 0.8,
                                           frac1 = 0.1, frac2 = 0.5) {
  n <- length(y)
  na <- max(1L, floor(frac1 * n)); nb <- max(1L, floor(frac2 * n))
  a <- y[seq_len(na)]; b <- y[(n - nb + 1L):n]
  va <- if (na >= 3L) .searchnet_lrv(a) / na else NA_real_
  vb <- if (nb >= 3L) .searchnet_lrv(b) / nb else NA_real_
  se <- sqrt(va + vb)
  drift <- mean(b) - mean(a)
  z <- if (is.finite(se) && se > 0) drift / se else NA_real_
  t_idx <- seq_along(y)
  res <- stats::residuals(stats::lm(y ~ t_idx))
  crit <- stats::qnorm(1 - alpha / 2) + stats::qnorm(power)
  list(drift = drift, se_drift = se, z = z,
       p_value = if (is.na(z)) NA_real_ else 2 * stats::pnorm(-abs(z)),
       mdd = crit * se, sd_within = stats::sd(res),
       ess = .searchnet_ess(res), n_first = na)
}


# ---------------------------------------------------------------------------- #
#  searchnet_stationarity_check
# ---------------------------------------------------------------------------- #

#' Check Whether One Simulated Run Has Reached Its Stationary Law
#'
#' Reads the per-ministep trajectory of a simulated run and asks, for each
#' statistic, whether the part after burn-in still drifts, has stopped moving
#' altogether, or is too short to say. An end state is a draw from the
#' stationary law only if the run is neither trending nor frozen when it is
#' read.
#'
#' Each statistic gets one verdict:
#' \describe{
#'   \item{\code{"absorbed"}}{The statistic has not changed over the last
#'     \code{absorbed_share} of the run's ministeps. A run that stopped
#'     changing has hit a state it does not leave (or leaves too rarely to
#'     see); its end state is a hitting point, not a draw from a stationary
#'     distribution. The network-level share of the run since the last tie
#'     change is reported alongside, so a frozen network can be told apart
#'     from a statistic pinned at its ceiling while ties still move.}
#'   \item{\code{"too_short"}}{Fewer than \code{min_ess} effective samples
#'     after burn-in (or, for \code{"geweke"}, fewer than 10 ministeps in the
#'     first segment). Nothing is concluded.}
#'   \item{\code{"trending"}}{A drift was detected at level \code{alpha}.}
#'   \item{\code{"stationary"}}{No drift was detected at level \code{alpha}.
#'     This is a bound, not a proof: the result reports the minimum detectable
#'     drift (\code{mdd}), the smallest drift the test would detect with 80
#'     percent power, and the verdict says only that no drift at least that
#'     large was found.}
#' }
#' The checks run in that order: a segment shorter than \code{min_ess}
#' ministeps is \code{"too_short"} before anything else, then absorption, then
#' effective sample size, then the trend test. The overall verdict is the worst
#' per-statistic verdict, in the order stationary < too_short < trending <
#' absorbed.
#'
#' @section Methods:
#' Both methods use the post-burn-in segment (the last \code{1 - burn_in} of
#' the ministeps) and an autocorrelation-robust variance: the spectral density
#' at frequency zero from an autoregressive fit with the order chosen by AIC,
#' the estimator \pkg{coda} uses for \code{geweke.diag()} and
#' \code{effectiveSize()}.
#' \describe{
#'   \item{\code{"windows"}}{The segment is split into \code{n_windows}
#'     consecutive windows of equal length \eqn{L}. The slope of the window
#'     means on the window index is tested against zero, with
#'     \eqn{\mathrm{Var}(\bar y_k) = S(0)/L} and \eqn{S(0)} estimated from
#'     the segment net of its linear trend. \code{drift} is the slope times
#'     \code{n_windows - 1}: the change in the statistic from the first window
#'     to the last.}
#'   \item{\code{"geweke"}}{Geweke's z: the mean of the last 50 percent of the
#'     segment minus the mean of its first 10 percent, divided by the standard
#'     error from each part's own \eqn{S(0)}. \code{drift} is that
#'     difference.}
#' }
#' The effective sample size is that of the segment net of its linear trend,
#' so a run with a clean trend is reported as trending rather than as too
#' short. \code{drift_sd} and \code{mdd_sd} express the drift and the minimum
#' detectable drift in units of the within-run standard deviation (also net of
#' the linear trend).
#'
#' @section Reading the result:
#' The default statistics are not six independent tests: \code{mean_K_AC} and
#' \code{mean_K_CA} are \eqn{N} and \eqn{M} times the density, so they carry
#' the same trajectory and get the same verdict. Each statistic is tested at
#' \code{alpha} without a multiplicity correction, so the chance that a
#' stationary run reads \code{"trending"} on at least one statistic exceeds
#' \code{alpha}. A long run that reads \code{"stationary"} with a large
#' \code{mdd_sd} has not shown much: lengthen the run until the bound is
#' small enough to matter for the claim being made.
#'
#' @param env A \code{SaomNkRSienaBiEnv} after \code{\link{saomnk_run}} (or
#'   \code{search_rsiena()}), on the default state-carrying path. An
#'   environment simulated with \code{path = "legacy_replay"} is refused.
#' @param statistics Character vector, any of \code{"density"},
#'   \code{"mean_K_AC"}, \code{"mean_K_CA"}, \code{"mean_K_AA"},
#'   \code{"mean_K_CC"} (the means of the four K degrees over actors or components)
#'   and \code{"utility"} (mean actor utility). Default all six.
#' @param burn_in Numeric in [0, 1). Share of the ministeps discarded from the
#'   start before testing. Default 0.5.
#' @param n_windows Integer >= 2. Number of windows for \code{"windows"}.
#'   Default 4.
#' @param method \code{"windows"} (default) or \code{"geweke"}.
#' @param alpha Numeric. Test level. Default 0.05.
#' @param min_ess Numeric. Minimum effective sample size after burn-in for a
#'   verdict other than \code{"too_short"}. Default 50.
#' @param absorbed_share Numeric in (0, 1). A statistic unchanged over at least
#'   this share of the run's ministeps is \code{"absorbed"}. Default 0.25.
#'
#' @return An object of class \code{searchnet_stationarity}: a list with
#'   \code{$results} (one row per statistic: \code{verdict}, \code{n_post},
#'   \code{ess}, \code{mean}, \code{sd_within}, \code{drift},
#'   \code{drift_sd}, \code{z}, \code{p_value}, \code{mdd}, \code{mdd_sd},
#'   \code{share_since_change}), \code{$overall} (the worst verdict),
#'   \code{$network} (ministeps, tie changes, last change step and the share
#'   of the run since it), \code{$trajectories} (the per-ministep series) and
#'   \code{$settings}.
#'
#' @seealso \code{\link{searchnet_ergodicity_sweep}} for independence from the
#'   starting state across runs.
#'
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 6, N = 8, density = 0.3, seed = 1)
#' model <- saomnk_model(density = -0.6)
#' ## 150 opportunities per actor from a start away from the stationary
#' ## density: expect "trending" or "too_short", not "stationary"
#' saomnk_run(env, model, steps_per_actor = 150, seed = 1)
#' st <- searchnet_stationarity_check(env)
#' st
#' st$results[, c("statistic", "verdict", "drift_sd", "mdd_sd")]
#' }
#' @export
searchnet_stationarity_check <- function(env,
                                         statistics = c("density", "mean_K_AC",
                                                        "mean_K_CA", "mean_K_AA",
                                                        "mean_K_CC", "utility"),
                                         burn_in = 0.5,
                                         n_windows = 4,
                                         method = c("windows", "geweke"),
                                         alpha = 0.05,
                                         min_ess = 50,
                                         absorbed_share = 0.25) {
  .searchnet_require_path(env, "searchnet_stationarity_check()")
  if (!inherits(env, "SaomNkRSienaBiEnv"))
    stop("`env` must be a SaomNkRSienaBiEnv.", call. = FALSE)
  method <- match.arg(method)
  if (!is.character(statistics) || !length(statistics))
    stop("`statistics` must be a non-empty character vector.", call. = FALSE)
  bad <- setdiff(statistics, .SEARCHNET_STATIONARITY_STATS)
  if (length(bad))
    stop(sprintf("Unknown statistic(s): %s. Choose from %s.",
                 paste(bad, collapse = ", "),
                 paste(.SEARCHNET_STATIONARITY_STATS, collapse = ", ")), call. = FALSE)
  statistics <- unique(statistics)
  stopifnot(is.numeric(burn_in), length(burn_in) == 1L, burn_in >= 0, burn_in < 1)
  stopifnot(is.numeric(n_windows), length(n_windows) == 1L, n_windows >= 2,
            n_windows == round(n_windows))
  stopifnot(is.numeric(alpha), length(alpha) == 1L, alpha > 0, alpha < 1)
  stopifnot(is.numeric(min_ess), length(min_ess) == 1L, min_ess >= 2)
  stopifnot(is.numeric(absorbed_share), length(absorbed_share) == 1L,
            absorbed_share > 0, absorbed_share < 1)
  n_windows <- as.integer(n_windows)

  tr <- .searchnet_stationarity_trajectories(env, statistics)
  T <- tr$T
  n_burn <- floor(burn_in * T)
  post <- seq.int(n_burn + 1L, length.out = T - n_burn)
  n_post <- length(post)

  rows <- lapply(statistics, function(s) {
    y_all <- tr$traj[, s]
    y <- y_all[post]
    share <- .searchnet_share_since_change(y_all)
    out <- list(statistic = s, verdict = NA_character_, n_post = n_post,
                ess = NA_real_, mean = if (n_post) mean(y) else NA_real_,
                sd_within = NA_real_, drift = NA_real_, drift_sd = NA_real_,
                z = NA_real_, p_value = NA_real_, mdd = NA_real_,
                mdd_sd = NA_real_, share_since_change = share)
    if (n_post < max(min_ess, 2L * n_windows)) {
      out$verdict <- "too_short"
      return(out)
    }
    if (share >= absorbed_share) {
      out$verdict <- "absorbed"
      return(out)
    }
    r <- if (method == "windows")
      .searchnet_stationarity_windows(y, n_windows, alpha) else
      .searchnet_stationarity_geweke(y, alpha)
    out$ess <- r$ess
    out$sd_within <- r$sd_within
    out$drift <- r$drift
    out$z <- r$z
    out$p_value <- r$p_value
    out$mdd <- r$mdd
    if (is.finite(r$sd_within) && r$sd_within > 0) {
      out$drift_sd <- r$drift / r$sd_within
      out$mdd_sd <- r$mdd / r$sd_within
    }
    short_first <- method == "geweke" && r$n_first < 10L
    low_ess <- !is.finite(r$ess) || r$ess < min_ess || short_first || is.na(r$z)
    if (low_ess) {
      ## Too few effective samples to trust the long-run variance. A trend is
      ## still reported if a test that does not use it also rejects: the
      ## regression of the window means on their index, with the residual
      ## variance of the means themselves (n_windows - 2 df).
      wm <- .searchnet_window_means_test(y, n_windows)
      out$verdict <- if (!is.na(r$p_value) && r$p_value < alpha &&
                         !is.na(wm) && wm < alpha) "trending" else "too_short"
    } else if (r$p_value < alpha) {
      out$verdict <- "trending"
    } else {
      out$verdict <- "stationary"
    }
    out
  })
  results <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  rownames(results) <- NULL

  lv <- match(results$verdict, .SEARCHNET_STATIONARITY_LEVELS)
  overall <- .SEARCHNET_STATIONARITY_LEVELS[max(lv)]

  structure(
    list(results = results, overall = overall, network = tr$network,
         trajectories = tr$traj,
         settings = list(method = method, burn_in = burn_in, n_burn = n_burn,
                         n_windows = n_windows, alpha = alpha, power = 0.8,
                         min_ess = min_ess, absorbed_share = absorbed_share,
                         M = tr$M, N = tr$N)),
    class = "searchnet_stationarity")
}


#' Print a Stationarity Check
#'
#' @param x A \code{searchnet_stationarity} object.
#' @param digits Integer. Digits for the drift columns. Default 3.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @method print searchnet_stationarity
#' @export
print.searchnet_stationarity <- function(x, digits = 3, ...) {
  s <- x$settings; net <- x$network; r <- x$results
  cat("Stationarity check for one simulated run\n")
  cat(sprintf("  %d actors x %d components, %d ministeps; burn-in %d (%.0f%%), %d tested\n",
              s$M, s$N, net$n_steps, s$n_burn, 100 * s$burn_in,
              net$n_steps - s$n_burn))
  cat(sprintf("  method: %s%s, alpha %.3g, minimum ESS %g\n", s$method,
              if (s$method == "windows") sprintf(" (%d windows)", s$n_windows) else
                " (first 10% vs last 50%)", s$alpha, s$min_ess))
  cat(sprintf("  network: %d tie changes; last change at ministep %d; %.1f%% of the run since\n",
              net$n_changes, net$last_change_step, 100 * net$share_since_last_change))
  cat("\n")
  f <- function(v) ifelse(is.na(v), "     -", formatC(v, digits = digits, format = "f", width = 6))
  tab <- data.frame(
    statistic = r$statistic, verdict = r$verdict,
    ESS = ifelse(is.na(r$ess), "-", sprintf("%.0f", r$ess)),
    drift_sd = f(r$drift_sd), mdd_sd = f(r$mdd_sd),
    p = ifelse(is.na(r$p_value), "-", formatC(r$p_value, digits = 3, format = "g")),
    frozen = sprintf("%.0f%%", 100 * r$share_since_change),
    stringsAsFactors = FALSE)
  print(tab, row.names = FALSE, right = TRUE)
  cat(sprintf("\n  overall: %s\n", toupper(x$overall)))
  cat("  drift_sd and mdd_sd are in within-run SD units; mdd is the smallest drift\n")
  cat("  detectable with 80% power. 'frozen' is the share of the run since the\n")
  cat("  statistic last changed.\n")
  if (any(r$verdict == "stationary"))
    cat("  'stationary' is a bound: no drift of at least mdd was detected. It is\n",
        "  not proof that the run has reached its stationary law.\n", sep = "")
  if (any(r$verdict == "absorbed"))
    cat("  'absorbed': the statistic stopped changing. Its end value is where the\n",
        "  run stopped (a hitting point), not a draw from a stationary distribution.\n", sep = "")
  invisible(x)
}
