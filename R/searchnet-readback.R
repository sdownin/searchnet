## ---------------------------------------------------------------------------
## Readback diagnostic: is the "outcome" just the evaluation function?
## ---------------------------------------------------------------------------

## Actor degrees in a bipartite state: K_AC = components held, K_AA = other
## actors sharing at least one component.
.readback_k <- function(B) {
  B <- (as.matrix(B) > 0) * 1
  co <- B %*% t(B)
  diag(co) <- 0
  cbind(K_AC = rowSums(B), K_AA = rowSums(co > 0))
}

## Run `expr` under a fixed seed without disturbing the caller's RNG stream.
.with_local_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  expr
}

.ols_slope <- function(y, x) {
  ok <- is.finite(y) & is.finite(x)
  y <- y[ok]; x <- x[ok]
  if (length(x) < 3L || stats::var(x) == 0) return(NA_real_)
  stats::cov(x, y) / stats::var(x)
}

#' Detect an outcome that is the evaluation function read back
#'
#' In a stochastic actor-oriented model each actor's utility is the
#' evaluation function \eqn{u_i(x) = \sum_k \theta_k s_{ik}(x)}: the model's
#' own statistics weighted by the declared coefficients. Several of those
#' statistics are degrees (\code{density} is the actor's number of ties,
#' \code{K_AC}). An analysis that takes "utility" (or anything built from it)
#' as an outcome and regresses it on K statistics therefore recovers the
#' declared \eqn{\theta}, and a K gradient that looks like a finding about
#' search is the objective function read back. This diagnostic makes that
#' visible before the result is interpreted.
#'
#' It does two things.
#' \enumerate{
#'   \item \strong{Readback regression.} The outcome is regressed (OLS, with
#'     intercept) on the model's own statistics, pooled over actors and the
#'     selected states. It reports \eqn{R^2} and the recovered coefficients
#'     next to the declared \eqn{\theta}. When both that \eqn{R^2} and the
#'     \eqn{R^2} of the outcome on the declared evaluation function
#'     \eqn{S\theta} alone reach \code{r2_threshold}, the outcome is reported
#'     as the objective: exactly, if the coefficients also match \eqn{\theta}
#'     within \code{coef_tol}, otherwise up to the reported scale.
#'   \item \strong{Random-portfolio null for the K gradient.} The slope of the
#'     outcome on a K statistic is compared with the slopes the evaluation
#'     function produces on random portfolios: \code{n_null} bipartite
#'     states drawn with no search at all, each scored with the declared
#'     \eqn{\theta}. An observed gradient inside that null interval is one the
#'     objective produces mechanically, whatever the dynamics did.
#' }
#'
#' @param x Either a \code{SaomNkRSienaBiEnv} after a run, or a bipartite
#'   state (an \eqn{M \times N}{M x N} 0/1 matrix) or list of states. For a
#'   matrix input \code{stats_fun}, \code{theta} and \code{outcome} are
#'   required.
#' @param outcome The outcome to test. For an environment, \code{NULL}
#'   (default) uses the per-actor utility the package reports
#'   (\code{env$actor_util_df}), which is exactly the quantity most often
#'   mistaken for an outcome. Otherwise a numeric vector with one value per
#'   (state, actor), states outermost and actors in order within a state (for
#'   a single state, length \eqn{M}), or a data frame with columns
#'   \code{chain_step_id}, \code{actor_id} and \code{outcome} (environment
#'   input only).
#' @param theta Named numeric vector of declared coefficients, names matching
#'   the columns of \code{stats_fun()}. For an environment, \code{NULL} reads
#'   the declared values from the model.
#' @param stats_fun Function mapping a bipartite state to an \eqn{M \times p}
#'   matrix of the model's statistics with column names. For an environment,
#'   \code{NULL} uses \code{env$get_struct_mod_stats_mat_from_bi_mat}.
#' @param k_stat Which K statistic the gradient is taken on: \code{"K_AC"}
#'   (components held, default) or \code{"K_AA"} (actors co-holding).
#' @param steps Chain steps to use for an environment. \code{NULL} takes up to
#'   \code{max_steps} evenly spaced steps including the last.
#' @param max_steps Maximum number of states used when \code{steps} is
#'   \code{NULL}.
#' @param n_null Number of random portfolios in the null. Default 200.
#' @param null_density Tie probability of the random portfolios; \code{NULL}
#'   uses the mean density of the analyzed states.
#' @param r2_threshold,coef_tol Readback criteria: the \eqn{R^2} threshold for
#'   the verdict (default 0.99), and the tolerance, relative to
#'   \code{max(1, max(abs(theta)))}, within which recovered coefficients count
#'   as matching the declared ones (default 0.01).
#' @param seed Seed for the null draws (the caller's RNG state is restored).
#' @return An object of class \code{"searchnet_readback"}: a list with
#'   \code{r_squared}, \code{r_squared_declared}, \code{scale},
#'   \code{coefficients} (data frame: effect, declared, recovered,
#'   difference), \code{coefficients_match}, \code{intercept},
#'   \code{outcome_is_objective},
#'   \code{k_stat}, \code{k_gradient}, \code{null} (vector of null slopes),
#'   \code{null_interval}, \code{gradient_in_null}, \code{n_obs},
#'   \code{n_states} and \code{outcome_source}. Its print method states the
#'   verdict in plain words.
#' @examples
#' ## A planted readback: the "outcome" is the evaluation function itself.
#' set.seed(1)
#' stats_fun <- function(B) cbind(density = rowSums(B),
#'                                inPop   = as.vector(B %*% colSums(B)))
#' theta <- c(density = -0.5, inPop = 0.2)
#' states <- replicate(5, matrix(rbinom(6 * 8, 1, 0.4), 6, 8), simplify = FALSE)
#' y <- unlist(lapply(states, function(B) stats_fun(B) %*% theta))
#' searchnet_readback_check(states, outcome = y, theta = theta,
#'                          stats_fun = stats_fun, n_null = 50)
#' @export
searchnet_readback_check <- function(x, outcome = NULL, theta = NULL,
                                     stats_fun = NULL,
                                     k_stat = c("K_AC", "K_AA"),
                                     steps = NULL, max_steps = 25L,
                                     n_null = 200L, null_density = NULL,
                                     r2_threshold = 0.99, coef_tol = 0.01,
                                     seed = 1L) {
  k_stat <- match.arg(k_stat)
  outcome_source <- "supplied"

  ## ---- 1. States, statistics, theta, outcome --------------------------- ##
  if (inherits(x, "SaomNkRSienaBiEnv")) {
    env <- x
    if (is.null(stats_fun))
      stats_fun <- function(B) env$get_struct_mod_stats_mat_from_bi_mat(B)
    if (is.null(theta)) {
      td <- env$get_bipartite_effects_theta_df()
      theta <- stats::setNames(as.numeric(td$initialValue), td$effect_level)
    }
    arr <- env$bi_env_arr
    if (!is.null(arr) && length(dim(arr)) == 3L && dim(arr)[3] >= 1L) {
      n_t <- dim(arr)[3]
      if (is.null(steps))
        steps <- unique(round(seq(1, n_t, length.out = min(max_steps, n_t))))
      if (any(steps < 1 | steps > n_t))
        stop(sprintf("`steps` must lie in 1..%d.", n_t), call. = FALSE)
      states <- lapply(steps, function(t) arr[, , t])
    } else {
      steps <- NA_integer_
      states <- list(env$bipartite_matrix)
    }
    M <- nrow(states[[1]])
    if (is.null(outcome)) {
      ud <- env$actor_util_df
      if (!is.null(ud) && !anyNA(steps)) {
        ud <- as.data.frame(ud)
        key <- paste(ud$chain_step_id, as.integer(as.character(ud$actor_id)))
        want <- paste(rep(steps, each = M), rep(seq_len(M), times = length(steps)))
        y <- ud$utility[match(want, key)]
        outcome_source <- "env$actor_util_df (reported utility)"
      } else {
        y <- NULL
      }
      if (is.null(y) || anyNA(y)) {
        y <- unlist(lapply(states, function(B) as.vector(stats_fun(B)[, names(theta), drop = FALSE] %*% theta)))
        outcome_source <- "evaluation function computed from theta"
      }
    } else if (is.data.frame(outcome)) {
      if (!all(c("chain_step_id", "actor_id", "outcome") %in% names(outcome)))
        stop("An outcome data frame needs chain_step_id, actor_id and outcome ",
             "columns.", call. = FALSE)
      key <- paste(outcome$chain_step_id, as.integer(as.character(outcome$actor_id)))
      want <- paste(rep(steps, each = M), rep(seq_len(M), times = length(steps)))
      y <- outcome$outcome[match(want, key)]
    } else {
      y <- as.numeric(outcome)
      if (length(y) == M && length(states) > 1L) {
        ## One value per actor: the final state.
        states <- states[length(states)]
        steps <- steps[length(steps)]
      }
    }
  } else {
    states <- if (is.matrix(x)) list(x) else x
    if (!is.list(states) || !length(states) || !all(vapply(states, is.matrix, logical(1))))
      stop("`x` must be a SaomNkRSienaBiEnv, a bipartite matrix, or a list of ",
           "matrices.", call. = FALSE)
    if (is.null(stats_fun) || is.null(theta) || is.null(outcome))
      stop("For matrix input, `stats_fun`, `theta` and `outcome` are all ",
           "required.", call. = FALSE)
    y <- as.numeric(outcome)
    M <- nrow(states[[1]])
    steps <- seq_along(states)
  }

  if (is.null(names(theta)) || any(!nzchar(names(theta))))
    stop("`theta` must be named by statistic.", call. = FALSE)
  S_list <- lapply(states, function(B) {
    S <- as.matrix(stats_fun(B))
    miss <- setdiff(names(theta), colnames(S))
    if (length(miss))
      stop("stats_fun() returned no column for: ", paste(miss, collapse = ", "),
           call. = FALSE)
    S[, names(theta), drop = FALSE]
  })
  S <- do.call(rbind, S_list)
  K <- do.call(rbind, lapply(states, .readback_k))[, k_stat]
  if (length(y) != nrow(S))
    stop(sprintf("`outcome` has %d values; %d states x %d actors = %d expected.",
                 length(y), length(states), M, nrow(S)), call. = FALSE)
  ok <- is.finite(y) & apply(is.finite(S), 1, all)
  if (sum(ok) < ncol(S) + 2L)
    stop("Too few complete observations for the readback regression.",
         call. = FALSE)

  ## ---- 2. Readback regression ------------------------------------------ ##
  df <- data.frame(y = y[ok], S[ok, , drop = FALSE], check.names = FALSE)
  colnames(df) <- c("y", paste0("s", seq_len(ncol(S))))
  fit <- stats::lm(y ~ ., data = df)
  ss_tot <- sum((df$y - mean(df$y))^2)
  r2 <- if (ss_tot > 0) 1 - sum(stats::residuals(fit)^2) / ss_tot else NA_real_
  cf <- stats::coef(fit)
  recovered <- unname(cf[-1])
  coefs <- data.frame(effect = names(theta), declared = unname(theta),
                      recovered = recovered,
                      difference = recovered - unname(theta),
                      stringsAsFactors = FALSE)
  tol <- coef_tol * max(1, max(abs(theta)))
  est <- !is.na(coefs$recovered)
  coefs_match <- any(est) && all(abs(coefs$difference[est]) <= tol)
  ## The declared evaluation function itself as a single regressor. This is
  ## the verdict's basis: it is immune to aliased statistics (two identical
  ## columns leave one coefficient NA) and it also catches a rescaled or
  ## shifted objective, which recovers c * theta rather than theta.
  u <- as.vector(S[ok, , drop = FALSE] %*% theta)
  r2_declared <- if (stats::var(u) > 0 && ss_tot > 0) stats::cor(u, df$y)^2 else NA_real_
  scale <- if (stats::var(u) > 0) stats::cov(u, df$y) / stats::var(u) else NA_real_
  is_objective <- isTRUE(r2 >= r2_threshold) && isTRUE(r2_declared >= r2_threshold)

  ## ---- 3. Random-portfolio null for the K gradient --------------------- ##
  k_grad <- .ols_slope(y, K)
  N <- ncol(states[[1]])
  p0 <- if (is.null(null_density)) mean(vapply(states, function(B) mean(B > 0), numeric(1)))
        else null_density
  p0 <- min(max(p0, 1 / (M * N)), 1 - 1 / (M * N))
  null_slopes <- .with_local_seed(seed, vapply(seq_len(n_null), function(r) {
    ## Heterogeneous per-actor tie probabilities with mean p0, so K varies
    ## across actors even at small M. No search, no dynamics.
    p_i <- pmin(1, stats::runif(M, 0, 2 * p0))
    B <- matrix(stats::rbinom(M * N, 1, rep(p_i, times = N)), M, N)
    Sr <- as.matrix(stats_fun(B))[, names(theta), drop = FALSE]
    .ols_slope(as.vector(Sr %*% theta), .readback_k(B)[, k_stat])
  }, numeric(1)))
  null_ok <- null_slopes[is.finite(null_slopes)]
  null_int <- if (length(null_ok) >= 10L)
    stats::quantile(null_ok, c(0.025, 0.975), names = FALSE) else c(NA_real_, NA_real_)
  in_null <- is.finite(k_grad) && all(is.finite(null_int)) &&
    k_grad >= null_int[1] && k_grad <= null_int[2]

  structure(list(
    r_squared = r2,
    r_squared_declared = r2_declared,
    scale = scale,
    coefficients = coefs,
    coefficients_match = coefs_match,
    intercept = unname(cf[1]),
    outcome_is_objective = is_objective,
    k_stat = k_stat,
    k_gradient = k_grad,
    null = null_slopes,
    null_interval = null_int,
    null_density = p0,
    gradient_in_null = in_null,
    n_obs = sum(ok),
    n_states = length(states),
    steps = steps,
    outcome_source = outcome_source,
    criteria = list(r2_threshold = r2_threshold, coef_tol = tol)
  ), class = "searchnet_readback")
}

#' @export
print.searchnet_readback <- function(x, digits = 3, ...) {
  f <- function(v) formatC(v, digits = digits, format = "f")
  cat("searchnet readback check\n")
  cat(sprintf("  outcome: %s; %d observations (%d states)\n",
              x$outcome_source, x$n_obs, x$n_states))
  cat(sprintf("  R^2 of outcome on the model's own statistics: %s\n", f(x$r_squared)))
  co <- x$coefficients
  co$declared <- f(co$declared); co$recovered <- f(co$recovered)
  co$difference <- f(co$difference)
  print(co, row.names = FALSE)
  cat(sprintf("  %s gradient of the outcome: %s; random-portfolio null 95%% interval [%s, %s] (n = %d)\n",
              x$k_stat, f(x$k_gradient), f(x$null_interval[1]), f(x$null_interval[2]),
              length(x$null)))
  cat("\n")
  if (isTRUE(x$outcome_is_objective)) {
    how <- if (isTRUE(x$coefficients_match))
      "and the regression recovers the declared theta"
    else sprintf("and it is the declared evaluation function up to a scale of %s",
                 f(x$scale))
    cat("  VERDICT: THE OUTCOME IS THE OBJECTIVE. It is the evaluation function\n",
        "  read back: the model's own statistics explain it (R^2 = ", f(x$r_squared),
        "),\n  ", how, ".\n",
        "  A K gradient in this outcome restates the coefficients you declared;\n",
        "  it is not a finding about search.\n", sep = "")
  } else {
    cat("  VERDICT: the outcome is not the evaluation function read back\n",
        "  (R^2 or recovered coefficients fall short of the readback criteria).\n",
        sep = "")
  }
  if (isTRUE(x$gradient_in_null)) {
    cat("  The ", x$k_stat, " gradient lies inside the random-portfolio null: the\n",
        "  objective produces it on portfolios drawn with no search at all.\n", sep = "")
  } else if (is.finite(x$k_gradient) && all(is.finite(x$null_interval))) {
    cat("  The ", x$k_stat, " gradient lies outside the random-portfolio null.\n", sep = "")
  }
  invisible(x)
}
