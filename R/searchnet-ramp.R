###############################################################################
## searchnet-ramp.R
##
## Continuous rate-of-environmental-change operators.
##
## Background. searchnet's only time-varying-parameter mechanism was the
## piecewise-constant shock: `preprocess_theta_shocks()` cuts the ministep chain
## into contiguous blocks and `shock_theta_matrix()` overwrites whole blocks of
## the theta matrix with a constant. That expresses "a shock happened at time
## t", and nothing else. It cannot express "the environment erodes at rate r",
## because a ramp is not a step function and no number of steps is a ramp.
##
## `search_rsiena()` already accepts a user-supplied `theta_matrix` of shape
## iterations x n_parameters, applied one row per ministep. So the missing
## capability is not in the engine -- it is the absence of any helper that
## builds such a matrix correctly. The hard part is the column order, which is
## decided by RSiena's effects table rather than by the structure model, and so
## cannot be reconstructed by hand with any confidence.
##
## These functions therefore delegate matrix construction to
## `env$prepare_theta_scaffold()` (which is the same code path `search_rsiena()`
## uses) and then overwrite named columns with a trajectory.
##
## Two distinct operators, deliberately kept apart:
##
##   saomnk_theta_ramp()  -- a DIRECTED change. The parameter goes from A to B
##                           over a window. This is environmental erosion,
##                           technological obsolescence, gradual deregulation:
##                           processes with a direction.
##
##   saomnk_theta_drift() -- UNDIRECTED volatility. The parameter follows a
##                           random walk with a given step standard deviation.
##                           This is landscape instability: the environment is
##                           not going anywhere in particular, it will not sit
##                           still. Distinct from a ramp, and the two compose.
##
## Both return a plain numeric matrix, so they compose by feeding the output of
## one in as the `theta_matrix` argument of the next, and both can be handed
## straight to `saomnk_run(theta_matrix = )` or `env$search_rsiena()`.
###############################################################################


## ---------------------------------------------------------------------------
## Internal: easing curves
## ---------------------------------------------------------------------------
## Each maps progress p in [0, 1] to eased progress in [0, 1], with f(0) = 0 and
## f(1) = 1 exactly, so that a ramp lands exactly on `from` and `to`.
##
## The sigmoid is 1/(1 + exp(-steepness * (p - 0.5))) rescaled onto [0, 1]. The
## raw logistic does NOT satisfy f(0) = 0: at steepness 6 it starts at 0.047 and
## ends at 0.953, so an unrescaled ramp would jump discontinuously at both ends
## by ~5% of the total change. The rescaling removes that jump and preserves the
## S shape.
.searchnet_ease <- function(p, easing = "linear", steepness = 6, k = 3) {

  p <- pmin(pmax(p, 0), 1)

  switch(
    easing,
    linear = p,
    sigmoid = {
      raw <- function(x) 1 / (1 + exp(-steepness * (x - 0.5)))
      lo <- raw(0); hi <- raw(1)
      (raw(p) - lo) / (hi - lo)
    },
    exponential = {
      if (isTRUE(all.equal(k, 0))) return(p)  ## degenerate: k -> 0 is linear
      (exp(k * p) - 1) / (exp(k) - 1)
    },
    stop(sprintf("Unknown easing '%s'. Use 'linear', 'sigmoid' or 'exponential'.",
                 easing))
  )
}


## ---------------------------------------------------------------------------
## Internal: resolve a user-supplied effect name to theta-matrix column(s)
## ---------------------------------------------------------------------------
## `colnames(theta_matrix)` are effect_level names: the RSiena shortName, with a
## `_1`, `_2`, ... suffix appended when the model includes several effects of
## the same type (see get_rsiena_effects_theta_df()). A user may reasonably
## supply either form. Exact match wins; a bare shortName that expands to
## several levels is an error, not a guess, because silently ramping the wrong
## one of two `egoX` terms would be undetectable in the output.
.searchnet_resolve_theta_col <- function(theta_matrix, effect) {

  cn <- colnames(theta_matrix)
  if (is.null(cn))
    stop("theta_matrix has no column names; cannot resolve effect '", effect, "'.")

  hit <- which(cn == effect)
  if (length(hit) == 1L) return(hit)

  ## Fall back to shortName -> effect_level expansion ("egoX" -> "egoX_1","egoX_2")
  base <- sub("_[0-9]+$", "", cn)
  hit2 <- which(base == effect)

  if (length(hit2) == 1L) return(hit2)

  if (length(hit2) > 1L)
    stop(sprintf(
      "Effect '%s' is ambiguous: it matches %d columns (%s). Name the exact effect_level you mean.",
      effect, length(hit2), paste(cn[hit2], collapse = ", ")))

  stop(sprintf(
    "Effect '%s' is not in the theta matrix. Available effects: %s",
    effect, paste(cn, collapse = ", ")))
}


## ---------------------------------------------------------------------------
## Internal: obtain a theta matrix, either supplied or freshly built
## ---------------------------------------------------------------------------
.searchnet_theta_base <- function(env, structure_model, iterations,
                                  theta_matrix = NULL, verbose = FALSE) {

  if (!is.null(theta_matrix)) {
    if (!is.matrix(theta_matrix) || !is.numeric(theta_matrix))
      stop("`theta_matrix` must be a numeric matrix.")
    if (is.null(colnames(theta_matrix)))
      stop("`theta_matrix` must have column names (effect_level names).")
    if (!missing(iterations) && !is.null(iterations) &&
        nrow(theta_matrix) != iterations)
      stop(sprintf("`theta_matrix` has %d rows but `iterations` is %d.",
                   nrow(theta_matrix), iterations))
    return(theta_matrix)
  }

  if (!inherits(env, "SaomNkRSienaBiEnv"))
    stop("`env` must be a SaomNkRSienaBiEnv object (or supply `theta_matrix` directly).")
  if (!is.list(structure_model))
    stop("`structure_model` must be a structure-model list.")

  env$prepare_theta_scaffold(structure_model, iterations, verbose = verbose)
}


# --------------------------------------------------------------------------- #
#  saomnk_theta_ramp
# --------------------------------------------------------------------------- #

#' Build a Theta Matrix With Gradually Changing Parameters
#'
#' Constructs a per-ministep parameter matrix in which one or more effects move
#' continuously from a starting value to an ending value over a window of the
#' decision chain. This is the continuous-rate-of-change counterpart to
#' \code{\link{saomnk_shock}}, which can only express piecewise-constant regime
#' changes.
#'
#' A ramp is specified as a fraction of the chain rather than as an absolute
#' ministep index, so the same specification is scale-free across
#' \code{iterations}: \code{start = 0.25, window = 0.5} always means "begins a
#' quarter of the way in, completes three quarters of the way in", whether the
#' chain is 300 or 30,000 steps long.
#'
#' Outside the window the parameter is held at \code{from} (before) and
#' \code{to} (after), so a ramp is a complete trajectory over the whole chain,
#' not a fragment.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object. May be \code{NULL} if
#'   \code{theta_matrix} is supplied.
#' @param structure_model A structure model list (as built by
#'   \code{\link{saomnk_model}}). Ignored if \code{theta_matrix} is supplied.
#' @param iterations Integer. Number of ministeps (rows of the theta matrix).
#' @param changes A list of change specifications. Each element is a list with:
#'   \describe{
#'     \item{\code{effect}}{Character. Effect name (RSiena shortName, e.g.
#'       \code{"inPop"}) or exact \code{effect_level} (e.g. \code{"egoX_2"}) when
#'       the model contains several effects of the same type.}
#'     \item{\code{from}}{Numeric. Parameter value at the start of the ramp.
#'       Defaults to the model's own value for that effect if omitted.}
#'     \item{\code{to}}{Numeric. Parameter value at the end of the ramp.}
#'     \item{\code{start}}{Numeric in \eqn{[0, 1]}. Fraction of the chain at which the
#'       ramp begins (default \code{0}).}
#'     \item{\code{window}}{Numeric in (0, 1]. Fraction of the chain over which
#'       the ramp completes (default \code{1 - start}).}
#'     \item{\code{easing}}{Optional per-change override of the \code{easing}
#'       argument.}
#'   }
#' @param easing Character. Shape of the trajectory, one of \code{"linear"}
#'   (constant rate of change), \code{"sigmoid"} (slow-fast-slow; a tipping
#'   point) or \code{"exponential"} (accelerating; compounding erosion).
#'   All three are normalized to land exactly on \code{from} and \code{to}.
#' @param k Numeric. Curvature of the \code{"exponential"} easing (default
#'   \code{3}); larger \code{k} defers more of the change to the end of the
#'   window. \code{k -> 0} degenerates to linear.
#' @param steepness Numeric. Steepness of the \code{"sigmoid"} easing
#'   (default \code{6}).
#' @param theta_matrix Optional numeric matrix to modify instead of building a
#'   fresh one. Supply the output of a previous \code{saomnk_theta_ramp()} or
#'   \code{\link{saomnk_theta_drift}} call to compose trajectories.
#' @param verbose Logical. Print the resolved trajectory endpoints.
#'
#' @return A numeric matrix with \code{iterations} rows and one column per
#'   simulated parameter, with column names matching RSiena's
#'   \code{effect_level} names. Pass it to \code{\link{saomnk_run}} via
#'   \code{theta_matrix = }, or to \code{env$search_rsiena(theta_matrix = )}.
#'
#' @seealso \code{\link{saomnk_theta_drift}} for undirected volatility,
#'   \code{\link{saomnk_shock}} for piecewise-constant regime changes.
#'
#' @export
#' @examples
#' \dontrun{
#' env <- saomnk_env(M = 6, N = 12, seed = 42)
#' mod <- saomnk_model(density = -0.5,
#'                     influence_matrix = saomnk_block_diagonal(12, 4))
#'
#' ## The returns to component popularity erode steadily over the middle half
#' ## of the chain: an environment that is slowly de-agglomerating.
#' tm <- saomnk_theta_ramp(env, mod, iterations = 600,
#'                         changes = list(
#'                           list(effect = "inPop", from = 0.5, to = -0.5,
#'                                start = 0.25, window = 0.5)
#'                         ),
#'                         easing = "sigmoid")
#'
#' saomnk_run(env, mod, steps_per_actor = 100, theta_matrix = tm)
#' }
saomnk_theta_ramp <- function(env, structure_model, iterations, changes,
                              easing = c("linear", "sigmoid", "exponential"),
                              k = 3, steepness = 6,
                              theta_matrix = NULL, verbose = FALSE) {

  easing <- match.arg(easing)

  if (!is.list(changes) || !length(changes))
    stop("`changes` must be a non-empty list of change specifications.")
  if (!is.null(names(changes)) && !is.null(changes$effect))
    stop("`changes` looks like a single change specification. Wrap it: changes = list(list(effect = ...)).")

  theta_matrix <- .searchnet_theta_base(env, structure_model, iterations,
                                        theta_matrix = theta_matrix,
                                        verbose = verbose)
  n_iter <- nrow(theta_matrix)

  for (idx in seq_along(changes)) {

    ch <- changes[[idx]]
    if (!is.list(ch))
      stop(sprintf("changes[[%d]] must be a list.", idx))
    if (is.null(ch$effect))
      stop(sprintf("changes[[%d]] has no `effect`.", idx))
    if (is.null(ch$to))
      stop(sprintf("changes[[%d]] (effect '%s') has no `to` value.", idx, ch$effect))

    col <- .searchnet_resolve_theta_col(theta_matrix, ch$effect)

    ## `from` defaults to whatever the model already says the parameter is.
    from <- if (is.null(ch$from)) theta_matrix[1L, col] else as.numeric(ch$from)
    to   <- as.numeric(ch$to)

    start  <- if (is.null(ch$start))  0 else as.numeric(ch$start)
    if (start < 0 || start >= 1)
      stop(sprintf("changes[[%d]]: `start` must be in [0, 1).", idx))
    window <- if (is.null(ch$window)) (1 - start) else as.numeric(ch$window)
    if (window <= 0 || window > 1)
      stop(sprintf("changes[[%d]]: `window` must be in (0, 1].", idx))
    if (start + window > 1 + 1e-9)
      stop(sprintf("changes[[%d]]: start + window = %.3f exceeds 1 (the end of the chain).",
                   idx, start + window))

    ease_i <- if (is.null(ch$easing)) easing else ch$easing

    ## Row boundaries. A window is at least one row wide, so that a very short
    ## window on a short chain still produces a real (if abrupt) transition
    ## rather than a division by zero.
    first_row <- max(1L, floor(start * n_iter) + 1L)
    last_row  <- min(n_iter, max(first_row, ceiling((start + window) * n_iter)))

    traj <- rep(NA_real_, n_iter)
    if (first_row > 1L) traj[1L:(first_row - 1L)] <- from

    span <- last_row - first_row
    p <- if (span == 0L) 1 else seq.int(0L, span) / span
    traj[first_row:last_row] <- from + (to - from) *
      .searchnet_ease(p, easing = ease_i, steepness = steepness, k = k)

    if (last_row < n_iter) traj[(last_row + 1L):n_iter] <- to

    theta_matrix[, col] <- traj

    if (verbose)
      cat(sprintf("  ramp: %-20s %8.4f -> %8.4f  rows %d..%d  (%s)\n",
                  colnames(theta_matrix)[col], from, to, first_row, last_row, ease_i))
  }

  theta_matrix
}


# --------------------------------------------------------------------------- #
#  saomnk_theta_drift
# --------------------------------------------------------------------------- #

#' Build a Theta Matrix With a Randomly Drifting Parameter
#'
#' Constructs a per-ministep parameter matrix in which one effect follows a
#' Gaussian random walk. Where \code{\link{saomnk_theta_ramp}} models an
#' environment that is going somewhere, this models an environment that will not
#' sit still: the landscape is unstable, but not systematically improving or
#' worsening.
#'
#' The two are separate operators because they are separate constructs. A ramp
#' is a rate of change with a direction; drift is a variance with none. They are
#' also separately identified in a simulation design: doubling \code{sd} raises
#' volatility without moving the expected parameter, and the pair therefore
#' supports a factorial design over direction and instability.
#'
#' Reproducibility: for a fixed \code{seed} the trajectory is exactly
#' reproducible, and \code{\link[base]{set.seed}} state outside the call is
#' restored on exit, so drawing a drift does not perturb any surrounding
#' simulation stream.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object. May be \code{NULL} if
#'   \code{theta_matrix} is supplied.
#' @param structure_model A structure model list. Ignored if
#'   \code{theta_matrix} is supplied.
#' @param iterations Integer. Number of ministeps (rows of the theta matrix).
#' @param effect Character. Effect name (RSiena shortName) or exact
#'   \code{effect_level} to make volatile.
#' @param sd Numeric > 0. Standard deviation of the per-ministep increment.
#'   Note the walk accumulates: total dispersion after \code{n} steps is
#'   \code{sd * sqrt(n)}, so a chain of 900 ministeps with \code{sd = 0.01}
#'   wanders by roughly 0.3.
#' @param seed Integer. Random seed for the walk.
#' @param drift Numeric. Deterministic per-step increment added to each
#'   Gaussian step (default \code{0}). Non-zero \code{drift} gives a random walk
#'   with trend -- for a purely deterministic trend use
#'   \code{\link{saomnk_theta_ramp}} instead.
#' @param start Numeric or \code{NULL}. Starting value of the walk. Defaults to
#'   the model's own value for that effect. Ignored when \code{add = TRUE}.
#' @param add Logical. If \code{FALSE} (default) the column is REPLACED by the
#'   walk. If \code{TRUE} the walk is mean-zero-anchored and SUPERIMPOSED on
#'   whatever the column already holds --- which is how volatility composes with
#'   a ramp, giving an environment that is both eroding and unstable. Replacing
#'   a ramp with a walk would discard the ramp, so composition needs this flag
#'   rather than an implicit rule.
#' @param bounds Numeric length-2 vector or \code{NULL}. If supplied, the walk
#'   is clamped (reflected at the boundary is NOT used; values are truncated) to
#'   \code{[bounds[1], bounds[2]]}. Useful to keep a parameter within a
#'   theoretically meaningful range.
#' @param theta_matrix Optional numeric matrix to modify instead of building a
#'   fresh one, so drift composes with \code{\link{saomnk_theta_ramp}}.
#' @param verbose Logical. Print a summary of the realized walk.
#'
#' @return A numeric matrix with \code{iterations} rows and one column per
#'   simulated parameter.
#'
#' @seealso \code{\link{saomnk_theta_ramp}}
#'
#' @export
#' @examples
#' \dontrun{
#' env <- saomnk_env(M = 6, N = 12, seed = 42)
#' mod <- saomnk_model(density = -0.5,
#'                     influence_matrix = saomnk_block_diagonal(12, 4))
#'
#' ## A volatile environment: component popularity returns wander.
#' tm <- saomnk_theta_drift(env, mod, iterations = 600,
#'                          effect = "inPop", sd = 0.02, seed = 99)
#'
#' ## Erosion PLUS instability: compose the two with add = TRUE.
#' tm <- saomnk_theta_ramp(env, mod, iterations = 600,
#'                         changes = list(list(effect = "inPop",
#'                                             from = 0.5, to = -0.5)))
#' tm <- saomnk_theta_drift(NULL, NULL, iterations = 600, effect = "inPop",
#'                          sd = 0.02, seed = 99, theta_matrix = tm, add = TRUE)
#' }
saomnk_theta_drift <- function(env, structure_model, iterations, effect,
                               sd = 0.05, seed = 123, drift = 0,
                               start = NULL, add = FALSE, bounds = NULL,
                               theta_matrix = NULL, verbose = FALSE) {

  if (!is.character(effect) || length(effect) != 1L)
    stop("`effect` must be a single effect name.")
  if (!is.numeric(sd) || length(sd) != 1L || is.na(sd) || sd <= 0)
    stop("`sd` must be a single positive number.")
  if (!is.null(bounds) && (!is.numeric(bounds) || length(bounds) != 2L || bounds[1] >= bounds[2]))
    stop("`bounds` must be a numeric length-2 vector with bounds[1] < bounds[2].")

  theta_matrix <- .searchnet_theta_base(env, structure_model, iterations,
                                        theta_matrix = theta_matrix,
                                        verbose = verbose)
  n_iter <- nrow(theta_matrix)
  col <- .searchnet_resolve_theta_col(theta_matrix, effect)

  ## `start = NA` means "continue from whatever this column already holds at
  ## row 1", which is what composition with a prior ramp wants.
  x0 <- if (is.null(start) || (length(start) == 1L && is.na(start))) {
    theta_matrix[1L, col]
  } else {
    as.numeric(start)
  }

  ## Draw from an isolated RNG stream so the caller's seed state is untouched.
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)

  set.seed(as.integer(seed))
  steps <- stats::rnorm(n_iter, mean = drift, sd = sd)
  ## First row is the starting value itself: the walk begins where the model is.
  steps[1L] <- 0
  walk <- cumsum(steps)

  traj <- if (add) {
    ## Superimpose the walk on the existing trajectory. The walk starts at 0, so
    ## row 1 is unchanged and the underlying ramp (or shock schedule) survives.
    theta_matrix[, col] + walk
  } else {
    x0 + walk
  }

  if (!is.null(bounds))
    traj <- pmin(pmax(traj, bounds[1L]), bounds[2L])

  theta_matrix[, col] <- traj

  if (verbose)
    cat(sprintf("  drift: %-20s start %.4f  end %.4f  range [%.4f, %.4f]  sd=%.4g seed=%s\n",
                colnames(theta_matrix)[col], traj[1L], traj[n_iter],
                min(traj), max(traj), sd, seed))

  theta_matrix
}
