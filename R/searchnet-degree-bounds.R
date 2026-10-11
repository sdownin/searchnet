###############################################################################
## searchnet-degree-bounds.R
##
## Degree bounds on actors (portfolio size) and components (number of holders)
## that hold in forward simulation AND in estimation.
##
## Two mechanisms, both RSiena's own, verified on RSiena 1.5.0 (R 4.5.3) with a
## two-mode dependent network on 2026-10-10:
##
##   * an actor MAXIMUM is RSiena's `MaxDegree` algorithm setting. It removes
##     every option that would push an actor's outdegree above the cap, so the
##     cap is exact (simulated maximum outdegree equal to the cap).
##
##   * every FLOOR, and a component cap, is a FIXED evaluation effect at a large
##     coefficient (`bound_penalty`, default 20). A move that crosses the bound
##     changes the objective by -20, so its choice probability relative to
##     staying put is about exp(-20) = 2e-9 per opportunity: a modeling
##     assumption, not an estimated quantity, and soft in principle. A gate
##     after every simulated period aborts if a bound is ever violated.
##
## The effect behind each bound (statistic semantics confirmed by simulation,
## see tests/testthat/test-degree-bounds.R):
##
##   actor min 1        outIso            "Number of out-isolates", theta -p
##   actor min k > 1    outTrunc(k)       "Sum of outdegrees trunc(k)" =
##                                         sum_i min(x_i+, k), theta +p
##   component min 1    antiInIso         "Number of indegrees at least 1", +p
##   component min 2    + in2Plus         "Number of indegrees at least 2", +p
##   component min 3    + in3Plus         "Number of indegrees at least 3", +p
##   component max 1    in2Plus           theta -p
##   component max 2    in3Plus           theta -p
##
## A floor also ATTRACTS: an actor below its floor gains +p from adding a
## component, so a start below the floor is repaired within the first
## opportunities. A component cap built from an indicator is a BARRIER only:
## it stops a component from crossing the cap, but a component already above
## it is not pulled back (the indicator does not change while the indegree
## stays above the threshold). Starting states are therefore checked.
##
## Non-implementations (RSiena 1.5.0 offers no statistic, so no bound is
## faked): a component floor above 3 (no `in4Plus`, and no indegree analogue
## of `outTrunc`), a component cap above 2, and any `MaxDegree` for indegrees
## (`MaxDegree` constrains outdegrees only).
###############################################################################

.SEARCHNET_BOUND_PENALTY <- 20

## Component floors and caps RSiena 1.5.0 can express for a two-mode DV.
.SEARCHNET_COMPONENT_MIN_MAX <- 3L
.SEARCHNET_COMPONENT_MAX_MAX <- 2L

.searchnet_not_implemented <- function(msg)
  stop(structure(class = c("searchnet_not_implemented", "error", "condition"),
                 list(message = msg, call = NULL)))

## One side's bounds, as c(min =, max =) with NA for "none".
.searchnet_bounds_side <- function(x, side) {
  if (is.null(x)) return(c(min = NA_real_, max = NA_real_))
  if (is.list(x)) x <- unlist(x)
  if (!is.numeric(x) && !all(is.na(x)))
    stop(sprintf("degree_bounds$%s must be numeric, e.g. c(min = 1, max = 3).", side),
         call. = FALSE)
  nm <- names(x)
  x <- as.numeric(x)
  if (is.null(nm)) {
    if (length(x) != 2L)
      stop(sprintf(paste0("degree_bounds$%s must be named, c(min = , max = ), ",
                          "or an unnamed pair c(min, max)."), side), call. = FALSE)
    nm <- c("min", "max")
  }
  bad <- setdiff(nm, c("min", "max"))
  if (length(bad) || anyDuplicated(nm))
    stop(sprintf("degree_bounds$%s accepts only `min` and `max` (got %s).",
                 side, paste(nm, collapse = ", ")), call. = FALSE)
  out <- c(min = NA_real_, max = NA_real_)
  out[nm] <- x
  for (k in c("min", "max")) {
    v <- out[[k]]
    if (is.na(v)) next
    if (!is.finite(v) || v != round(v) || v < 0)
      stop(sprintf("degree_bounds$%s[\"%s\"] must be a non-negative integer (got %s).",
                   side, k, format(v)), call. = FALSE)
  }
  if (isTRUE(out[["min"]] == 0)) out[["min"]] <- NA_real_   # no floor
  if (isTRUE(out[["max"]] == 0))
    stop(sprintf(paste0("degree_bounds$%s[\"max\"] = 0 would forbid every tie. ",
                        "Use max >= 1."), side), call. = FALSE)
  if (!is.na(out[["min"]]) && !is.na(out[["max"]]) && out[["min"]] > out[["max"]])
    stop(sprintf("degree_bounds$%s: min (%d) exceeds max (%d).", side,
                 as.integer(out[["min"]]), as.integer(out[["max"]])), call. = FALSE)
  out
}

## The effects table behind a set of bounds.
.searchnet_bound_effects_table <- function(actor, component, penalty) {
  rows <- list()
  add <- function(side, shortName, theta, internal_parameter, role)
    rows[[length(rows) + 1L]] <<- data.frame(
      side = side, shortName = shortName, theta = theta,
      internal_parameter = internal_parameter, role = role,
      stringsAsFactors = FALSE)
  amin <- actor[["min"]]
  if (!is.na(amin)) {
    if (amin == 1) {
      add("actor", "outIso", -penalty, NA_real_, "actor floor 1 (out-isolate penalty)")
    } else {
      add("actor", "outTrunc", penalty, amin,
          sprintf("actor floor %d (outdegree truncated at %d)", as.integer(amin),
                  as.integer(amin)))
    }
  }
  cmin <- component[["min"]]
  if (!is.na(cmin)) {
    thr <- c("antiInIso", "in2Plus", "in3Plus")
    for (t in seq_len(cmin))
      add("component", thr[t], penalty, NA_real_,
          sprintf("component floor %d (indegree at least %d)", as.integer(cmin), t))
  }
  cmax <- component[["max"]]
  if (!is.na(cmax)) {
    nm <- c("in2Plus", "in3Plus")[cmax]
    add("component", nm, -penalty, NA_real_,
        sprintf("component cap %d (indegree at least %d penalized)",
                as.integer(cmax), as.integer(cmax) + 1L))
  }
  if (!length(rows))
    return(data.frame(side = character(0), shortName = character(0),
                      theta = numeric(0), internal_parameter = numeric(0),
                      role = character(0), stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

#' Degree Bounds for a SAOM-NK Model
#'
#' Builds (and validates) the degree bounds that \code{\link{saomnk_model}},
#' \code{\link{searchnet_recovery}}, \code{\link{searchnet_coevolve}} and
#' \code{\link{run_calibrated_counterfactual}} accept through their
#' \code{degree_bounds} argument. Any of those also accepts the raw forms
#' below, so calling this constructor is optional; it is exported so the
#' bounds can be built, printed and checked on their own.
#'
#' @section What each bound is, in RSiena terms:
#' \itemize{
#'   \item An \strong{actor maximum} is RSiena's \code{MaxDegree} algorithm
#'     setting for the bipartite dependent network: options that would push an
#'     actor above the cap are removed. It is exact.
#'   \item An \strong{actor minimum} of 1 is the \code{outIso} (out-isolate)
#'     effect, fixed at \code{-bound_penalty}; a minimum \eqn{k > 1} is
#'     \code{outTrunc} with internal parameter \eqn{k}
#'     (\eqn{\sum_i \min(x_{i+}, k)}), fixed at \code{+bound_penalty}. An actor
#'     at its floor therefore cannot drop a component (it must add one first,
#'     then drop), and an actor below its floor is strongly drawn to add one.
#'   \item A \strong{component minimum} \eqn{k \le 3} is the sum of the fixed
#'     indicator effects \code{antiInIso}, \code{in2Plus}, \code{in3Plus} up to
#'     \eqn{k}; a \strong{component maximum} \eqn{c \le 2} is \code{in2Plus}
#'     (\eqn{c = 1}) or \code{in3Plus} (\eqn{c = 2}) fixed at
#'     \code{-bound_penalty}. A component cap is a barrier: it holds from a
#'     start that satisfies it, and does not pull a component above the cap
#'     back down.
#' }
#' The penalty effects are \strong{fixed modeling assumptions, not estimated}
#' parameters: they are entered with \code{fix = TRUE, test = FALSE}, and an
#' estimate is never reported for them. A move across a penalized bound has
#' probability about \eqn{e^{-20}} relative to staying put, so the bound is
#' soft in principle; every simulated period ends with a gate that aborts if a
#' bound was crossed.
#'
#' @section Non-implementations:
#' RSiena 1.5.0 offers no indegree statistic beyond \code{in3Plus} and no
#' indegree analogue of \code{outTrunc} or \code{outMore} for a two-mode
#' network, and \code{MaxDegree} applies to outdegrees only. A component
#' minimum above 3 or a component maximum above 2 is therefore refused with a
#' \code{"searchnet_not_implemented"} error. That is a statement about the
#' software, not about the world.
#'
#' @section Maximum likelihood:
#' Estimation under bounds uses the method of moments. RSiena 1.5.0 warns
#' "maxlike and MaxDegree are incompatible", and an ML fit of a bipartite
#' network with \code{MaxDegree} stopped with a NaN score (verified
#' 2026-10-10); ML with only a fixed floor penalty returned a missing standard
#' error for a period rate. searchnet therefore refuses \code{maxlike = TRUE}
#' whenever bounds are set.
#'
#' @param actor Actor (portfolio size) bounds: \code{c(min = , max = )};
#'   either may be omitted or \code{NA}. \code{min = 0} means no floor.
#' @param component Component (number of holders) bounds, same form.
#' @param bound_penalty Positive number: the absolute value of the fixed
#'   coefficient on each penalty effect. Default 20.
#' @param repair_initial Logical. When a simulation starts from a state below a
#'   floor: \code{FALSE} (default) stops with the violating actors or
#'   components listed; \code{TRUE} adds randomly chosen ties (seeded from the
#'   run seed) until every floor holds, and says so. A start above a cap is
#'   never repaired.
#' @return An object of class \code{"searchnet_degree_bounds"}.
#' @seealso \code{\link{searchnet_check_degree_bounds}},
#'   \code{\link{searchnet_portfolio_infeasible}}
#' @export
#' @examples
#' b <- searchnet_degree_bounds(actor = c(min = 1, max = 3))
#' b
#' mod <- saomnk_model(density = -1, degree_bounds = b)
searchnet_degree_bounds <- function(actor = NULL, component = NULL,
                                    bound_penalty = .SEARCHNET_BOUND_PENALTY,
                                    repair_initial = FALSE) {
  if (!is.numeric(bound_penalty) || length(bound_penalty) != 1L ||
      !is.finite(bound_penalty) || bound_penalty <= 0)
    stop("`bound_penalty` must be a single positive number.", call. = FALSE)
  if (bound_penalty < 10)
    warning(sprintf(paste0("`bound_penalty` = %g: a crossing has probability about ",
                           "exp(-%g) = %.1e per opportunity, which is not negligible ",
                           "over a long run. 20 is the default."),
                    bound_penalty, bound_penalty, exp(-bound_penalty)), call. = FALSE)
  if (!is.logical(repair_initial) || length(repair_initial) != 1L || is.na(repair_initial))
    stop("`repair_initial` must be TRUE or FALSE.", call. = FALSE)
  a <- .searchnet_bounds_side(actor, "actor")
  cmp <- .searchnet_bounds_side(component, "component")
  if (!is.na(cmp[["min"]]) && cmp[["min"]] > .SEARCHNET_COMPONENT_MIN_MAX)
    .searchnet_not_implemented(sprintf(paste0(
      "A component minimum of %d is a NON-IMPLEMENTATION in RSiena 1.5.0: for a ",
      "two-mode network it offers indegree-threshold effects only at 1, 2 and 3 ",
      "(antiInIso, in2Plus, in3Plus) and no indegree analogue of outTrunc, so a ",
      "floor above 3 has no statistic to carry it. This says nothing about the ",
      "world; use a component minimum of at most 3."), as.integer(cmp[["min"]])))
  if (!is.na(cmp[["max"]]) && cmp[["max"]] > .SEARCHNET_COMPONENT_MAX_MAX)
    .searchnet_not_implemented(sprintf(paste0(
      "A component maximum of %d is a NON-IMPLEMENTATION in RSiena 1.5.0: ",
      "MaxDegree constrains outdegrees (actors) only, and the indegree-threshold ",
      "effects stop at in3Plus, which can carry a cap of at most 2. No component ",
      "cap above 2 is offered; this says nothing about the world."),
      as.integer(cmp[["max"]])))
  out <- list(actor = a, component = cmp, penalty = as.numeric(bound_penalty),
              repair_initial = repair_initial)
  out$effects <- .searchnet_bound_effects_table(a, cmp, out$penalty)
  class(out) <- "searchnet_degree_bounds"
  out
}

## Normalize any accepted form to a "searchnet_degree_bounds" object (or NULL).
.searchnet_as_degree_bounds <- function(degree_bounds,
                                        bound_penalty = .SEARCHNET_BOUND_PENALTY,
                                        repair_initial = FALSE) {
  if (is.null(degree_bounds)) return(NULL)
  if (inherits(degree_bounds, "searchnet_degree_bounds")) return(degree_bounds)
  if (is.list(degree_bounds)) {
    bad <- setdiff(names(degree_bounds), c("actor", "component"))
    if (is.null(names(degree_bounds)) || length(bad))
      stop("`degree_bounds` as a list must be named `actor` and/or `component`, ",
           "e.g. list(actor = c(min = 1, max = 3)).", call. = FALSE)
    b <- searchnet_degree_bounds(actor = degree_bounds$actor,
                                 component = degree_bounds$component,
                                 bound_penalty = bound_penalty,
                                 repair_initial = repair_initial)
  } else if (is.numeric(degree_bounds) || all(is.na(degree_bounds))) {
    b <- searchnet_degree_bounds(actor = degree_bounds,
                                 bound_penalty = bound_penalty,
                                 repair_initial = repair_initial)
  } else {
    stop("`degree_bounds` must be NULL, c(min = , max = ) for actors, a list(actor = , ",
         "component = ), or a searchnet_degree_bounds() object.", call. = FALSE)
  }
  if (.searchnet_bounds_empty(b)) return(NULL)
  b
}

.searchnet_bounds_empty <- function(b)
  is.null(b) || (all(is.na(b$actor)) && all(is.na(b$component)))

#' @rdname searchnet_degree_bounds
#' @param x A \code{"searchnet_degree_bounds"} object.
#' @param ... Unused.
#' @method print searchnet_degree_bounds
#' @export
print.searchnet_degree_bounds <- function(x, ...) {
  cat("SAOM-NK degree bounds\n")
  cat(paste0("  ", .searchnet_bounds_lines(x), collapse = "\n"), "\n", sep = "")
  invisible(x)
}

.searchnet_bounds_fmt <- function(v) {
  f <- function(z) if (is.na(z)) "-" else format(as.integer(z))
  sprintf("min %s, max %s", f(v[["min"]]), f(v[["max"]]))
}

## The lines print methods show: the bounds and how each is carried.
.searchnet_bounds_lines <- function(b) {
  out <- c(sprintf("actor (portfolio size):     %s", .searchnet_bounds_fmt(b$actor)),
           sprintf("component (holders):        %s", .searchnet_bounds_fmt(b$component)))
  if (!is.na(b$actor[["max"]]))
    out <- c(out, sprintf("MaxDegree = %d on the bipartite network (exact; RSiena algorithm setting)",
                          as.integer(b$actor[["max"]])))
  for (i in seq_len(nrow(b$effects))) {
    e <- b$effects[i, ]
    out <- c(out, sprintf("%-10s theta = %+g, FIXED: %s", e$shortName, e$theta, e$role))
  }
  c(out, paste0("These are fixed modeling assumptions, not estimated parameters. ",
                "Initial state below a floor: ",
                if (isTRUE(b$repair_initial)) "repaired (seeded random additions)."
                else "error (repair_initial = FALSE)."))
}


## ---------------------------------------------------------------------------
## Structure-model plumbing
## ---------------------------------------------------------------------------
## The bounds live in sm$dv_bipartite$degree_bounds; the penalty effects are
## ordinary fixed entries of sm$dv_bipartite$effects marked `bound = TRUE`, so
## they reach the RSiena effects table, the theta matrix and every simulation
## route exactly as any other fixed effect does.
.searchnet_attach_degree_bounds <- function(sm, degree_bounds,
                                            dv_name = .DV_NAME) {
  b <- .searchnet_as_degree_bounds(degree_bounds)
  effs <- sm$dv_bipartite$effects
  effs <- effs[!vapply(effs, function(e) isTRUE(e$bound), logical(1))]
  sm$dv_bipartite$degree_bounds <- b
  if (!is.null(b)) {
    clash <- intersect(vapply(effs, function(e) as.character(e$effect), character(1)),
                       b$effects$shortName)
    if (length(clash))
      stop(sprintf(paste0("The model already declares %s, which the degree bounds ",
                          "use as a fixed penalty effect. Drop it from the model, or ",
                          "drop the bound."), paste(clash, collapse = ", ")),
           call. = FALSE)
    for (i in seq_len(nrow(b$effects))) {
      e <- list(effect = b$effects$shortName[i], parameter = b$effects$theta[i],
                dv_name = dv_name, fix = TRUE, bound = TRUE)
      if (!is.na(b$effects$internal_parameter[i]))
        e$internal_parameter <- b$effects$internal_parameter[i]
      effs[[length(effs) + 1L]] <- e
    }
  }
  sm$dv_bipartite$effects <- effs
  sm
}

.searchnet_model_bounds <- function(sm) {
  if (!is.list(sm) || is.null(sm$dv_bipartite)) return(NULL)
  b <- sm$dv_bipartite$degree_bounds
  if (.searchnet_bounds_empty(b)) NULL else b
}

## Bounds must fit the network: an actor holds at most N components, a
## component at most M actors.
.searchnet_bounds_check_dims <- function(b, M, N) {
  if (is.null(b)) return(invisible(TRUE))
  chk <- function(v, lim, side, what) {
    for (k in c("min", "max")) {
      if (!is.na(v[[k]]) && v[[k]] > lim)
        stop(sprintf("degree_bounds$%s[\"%s\"] = %d exceeds the %d %s in the network.",
                     side, k, as.integer(v[[k]]), lim, what), call. = FALSE)
    }
  }
  chk(b$actor, N, "actor", "components")
  chk(b$component, M, "component", "actors")
  invisible(TRUE)
}

## The MaxDegree argument for sienaAlgorithmCreate(), or NULL.
.searchnet_max_degree <- function(b, dv_name, N = NULL) {
  if (is.null(b) || is.na(b$actor[["max"]])) return(NULL)
  k <- as.integer(b$actor[["max"]])
  if (!is.null(N) && k >= N) return(NULL)      # no constraint
  stats::setNames(k, dv_name)
}

## Add the MaxDegree setting to an algorithm argument list, refusing ML.
.searchnet_bounds_algorithm_args <- function(args, b, dv_name, N = NULL,
                                             where = "searchnet") {
  if (is.null(b)) return(args)
  .searchnet_bounds_refuse_ml(b, args$maxlike, where)
  md <- .searchnet_max_degree(b, dv_name, N)
  if (!is.null(args$MaxDegree) && !is.null(md))
    stop(sprintf("%s: `MaxDegree` is set by `degree_bounds`; do not pass it as well.",
                 where), call. = FALSE)
  if (!is.null(md)) args$MaxDegree <- md
  args
}

.searchnet_bounds_refuse_ml <- function(b, maxlike, where = "searchnet") {
  if (is.null(b) || !isTRUE(maxlike)) return(invisible(TRUE))
  stop(structure(
    class = c("searchnet_not_implemented", "error", "condition"),
    list(message = paste0(
      where, ": maximum likelihood (maxlike = TRUE) is refused with degree bounds. ",
      "RSiena 1.5.0 warns 'maxlike and MaxDegree are incompatible', and an ML fit of ",
      "a bipartite network with MaxDegree stopped with a NaN score (verified ",
      "2026-10-10); with only a fixed floor penalty, ML returned a missing standard ",
      "error for a period rate. Estimate by the method of moments (maxlike = FALSE)."),
      call = NULL)))
}

## Include the penalty effects in a sienaEffects object, fixed, untested.
.searchnet_apply_bound_effects <- function(eff, b, dv_name) {
  if (is.null(b) || !nrow(b$effects)) return(eff)
  for (i in seq_len(nrow(b$effects))) {
    sn <- b$effects$shortName[i]
    args <- list(eff, shortName = sn, character = TRUE, name = dv_name,
                 type = "eval", initialValue = b$effects$theta[i],
                 fix = TRUE, test = FALSE, include = TRUE)
    ip <- b$effects$internal_parameter[i]
    if (!is.na(ip)) args$parameter <- ip
    row_ok <- any(eff$name == dv_name & eff$shortName == sn & eff$type == "eval")
    if (!row_ok)
      .searchnet_not_implemented(sprintf(paste0(
        "The degree bound needs RSiena effect '%s' for dependent variable '%s', and ",
        "the effects table has no such row (non-implementation for this data)."),
        sn, dv_name))
    utils::capture.output(eff <- do.call(RSiena::setEffect, args))
    r <- which(eff$name == dv_name & eff$shortName == sn & eff$type == "eval" &
                 eff$include)
    if (length(r) != 1L || !isTRUE(eff$fix[r]) ||
        !isTRUE(all.equal(eff$initialValue[r], b$effects$theta[i])) ||
        (!is.na(ip) && !isTRUE(eff$parm[r] == ip)))
      stop(sprintf("searchnet: the degree-bound effect '%s' was not set as requested.",
                   sn), call. = FALSE)
  }
  eff
}


## ---------------------------------------------------------------------------
## Checking observed or simulated states
## ---------------------------------------------------------------------------
## Any accepted input as an M x N x W numeric array.
.searchnet_bounds_as_array <- function(x) {
  if (inherits(x, "siena")) {
    dv <- x$depvars
    bip <- which(vapply(dv, function(d) identical(attr(d, "type"), "bipartite"),
                        logical(1)))
    if (!length(bip))
      stop("the siena data object holds no bipartite dependent network.", call. = FALSE)
    x <- dv[[bip[1L]]]
  }
  if (inherits(x, "sienaDependent")) {
    if (!is.null(attr(x, "sparse")) && isTRUE(attr(x, "sparse")))
      x <- simplify2array(lapply(x, as.matrix))
    x <- array(as.numeric(x), dim = dim(x)[1:3])
  }
  if (is.list(x) && !is.array(x)) {
    if (!length(x)) stop("no waves supplied.", call. = FALSE)
    x <- simplify2array(lapply(x, function(m) as.matrix(m)))
    if (length(dim(x)) == 2L) x <- array(x, c(dim(x), 1L))
  }
  if (is.matrix(x) || inherits(x, "Matrix")) {
    x <- as.matrix(x)
    x <- array(x, c(dim(x), 1L))
  }
  if (!is.array(x) || length(dim(x)) != 3L)
    stop("expected an M x N matrix, an M x N x W array, a list of M x N matrices, ",
         "or a siena data object.", call. = FALSE)
  x[is.na(x)] <- 0      # a missing tie is not a held tie
  storage.mode(x) <- "numeric"
  x
}

## One row per violation: side, id, wave, degree, bound, kind.
.searchnet_bounds_violations <- function(arr, b) {
  rows <- list()
  for (w in seq_len(dim(arr)[3])) {
    B <- arr[, , w, drop = FALSE]
    B <- matrix(B, dim(arr)[1], dim(arr)[2])
    for (side in c("actor", "component")) {
      deg <- if (side == "actor") rowSums(B) else colSums(B)
      v <- b[[side]]
      lo <- if (!is.na(v[["min"]])) which(deg < v[["min"]]) else integer(0)
      hi <- if (!is.na(v[["max"]])) which(deg > v[["max"]]) else integer(0)
      if (length(lo))
        rows[[length(rows) + 1L]] <- data.frame(
          side = side, id = lo, wave = w, degree = deg[lo], bound = v[["min"]],
          kind = "below min", stringsAsFactors = FALSE)
      if (length(hi))
        rows[[length(rows) + 1L]] <- data.frame(
          side = side, id = hi, wave = w, degree = deg[hi], bound = v[["max"]],
          kind = "above max", stringsAsFactors = FALSE)
    }
  }
  if (!length(rows))
    return(data.frame(side = character(0), id = integer(0), wave = integer(0),
                      degree = numeric(0), bound = numeric(0), kind = character(0),
                      stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

.searchnet_bounds_violation_text <- function(v, max_rows = 20L) {
  show <- utils::head(v, max_rows)
  lines <- sprintf("    %s %d, wave %d: degree %g (%s %g)", show$side, show$id,
                   show$wave, show$degree,
                   ifelse(show$kind == "below min", "min", "max"), show$bound)
  if (nrow(v) > max_rows)
    lines <- c(lines, sprintf("    ... and %d more", nrow(v) - max_rows))
  paste(lines, collapse = "\n")
}

#' Check Observed Waves Against Degree Bounds
#'
#' Stops, listing every actor or component, wave and degree, when any wave of
#' a bipartite network violates the bounds. Estimation under bounds assumes
#' the bounds held in every observed wave: a method-of-moments fit cannot
#' reproduce an observed degree the model forbids, so a violating panel is an
#' error before estimation rather than a fit to report. searchnet calls this
#' automatically before every estimation with \code{degree_bounds} set.
#'
#' @param x An \eqn{M \times N}{M x N} 0/1 matrix, an
#'   \eqn{M \times N \times W}{M x N x W} array, a list of \eqn{M \times N}{M x N}
#'   matrices, a bipartite \code{sienaDependent}, or a \code{siena} data object
#'   (its first bipartite dependent network is checked). Missing ties count as
#'   absent.
#' @param bounds Degree bounds in any form \code{\link{searchnet_degree_bounds}}
#'   documents.
#' @return \code{TRUE}, invisibly, when every wave satisfies the bounds.
#'   Otherwise an error of class \code{"searchnet_degree_bounds_violation"}
#'   whose \code{violations} element is a data frame with columns \code{side},
#'   \code{id}, \code{wave}, \code{degree}, \code{bound} and \code{kind}.
#' @export
#' @examples
#' B1 <- matrix(c(1, 0, 0,
#'                0, 1, 1), 2, 3, byrow = TRUE)
#' searchnet_check_degree_bounds(B1, c(min = 1, max = 2))
#' B2 <- B1; B2[1, 1] <- 0
#' try(searchnet_check_degree_bounds(list(B1, B2), c(min = 1)))
searchnet_check_degree_bounds <- function(x, bounds) {
  b <- .searchnet_as_degree_bounds(bounds)
  if (is.null(b)) return(invisible(TRUE))
  arr <- .searchnet_bounds_as_array(x)
  .searchnet_bounds_check_dims(b, dim(arr)[1], dim(arr)[2])
  v <- .searchnet_bounds_violations(arr, b)
  if (nrow(v))
    stop(structure(
      class = c("searchnet_degree_bounds_violation", "error", "condition"),
      list(message = sprintf(paste0(
        "%d degree-bound violation(s) in the observed waves:\n%s\n",
        "Estimation under these bounds assumes they held in every wave; fix the ",
        "data or the bounds."), nrow(v), .searchnet_bounds_violation_text(v)),
        call = NULL, violations = v)))
  invisible(TRUE)
}


## ---------------------------------------------------------------------------
## Initial state and end-of-period gate
## ---------------------------------------------------------------------------
## Adds seeded random ties until every floor holds, never breaking a cap.
.searchnet_bounds_repair <- function(B, b, seed) {
  M <- nrow(B); N <- ncol(B)
  amax <- if (is.na(b$actor[["max"]])) Inf else b$actor[["max"]]
  cmax <- if (is.na(b$component[["max"]])) Inf else b$component[["max"]]
  .with_local_seed(.searchnet_seed(seed, "degree_bounds:repair"), {
    amin <- b$actor[["min"]]
    if (!is.na(amin)) for (i in which(rowSums(B) < amin)) {
      while (sum(B[i, ]) < amin) {
        cand <- which(B[i, ] == 0 & colSums(B) < cmax)
        if (!length(cand)) break
        B[i, cand[sample.int(length(cand), 1L)]] <- 1
      }
    }
    cmin <- b$component[["min"]]
    if (!is.na(cmin)) for (j in which(colSums(B) < cmin)) {
      while (sum(B[, j]) < cmin) {
        cand <- which(B[, j] == 0 & rowSums(B) < amax)
        if (!length(cand)) break
        B[cand[sample.int(length(cand), 1L)], j] <- 1
      }
    }
    B
  })
}

## The start state of a simulation under bounds: returned unchanged when it
## satisfies them, repaired when it is only below a floor and repair is on,
## and an error otherwise.
.searchnet_bounds_start <- function(B, b, seed, where = "search_rsiena()") {
  if (is.null(b)) return(B)
  .searchnet_bounds_check_dims(b, nrow(B), ncol(B))
  v <- .searchnet_bounds_violations(array(B, c(dim(B), 1L)), b)
  if (!nrow(v)) return(B)
  if (any(v$kind == "above max"))
    stop(sprintf(paste0(
      "%s: the start state is above a degree cap:\n%s\n",
      "A cap is not repaired (MaxDegree only stops additions; a component cap ",
      "is a barrier). Start from a state within the bounds."),
      where, .searchnet_bounds_violation_text(v[v$kind == "above max", ])),
      call. = FALSE)
  if (!isTRUE(b$repair_initial))
    stop(sprintf(paste0(
      "%s: the start state is below a degree floor:\n%s\n",
      "Under the floor's penalty an actor below it is strongly drawn to add a ",
      "component, so it would be repaired within its first opportunities, but the ",
      "start would not satisfy the stated bounds. Start from a state within them, ",
      "or set repair_initial = TRUE to add seeded random ties first."),
      where, .searchnet_bounds_violation_text(v)), call. = FALSE)
  B2 <- .searchnet_bounds_repair(B, b, seed)
  v2 <- .searchnet_bounds_violations(array(B2, c(dim(B2), 1L)), b)
  if (nrow(v2))
    stop(sprintf("%s: the start state could not be repaired within the bounds:\n%s",
                 where, .searchnet_bounds_violation_text(v2)), call. = FALSE)
  message(sprintf(paste0("%s: repaired the start state to the degree floors by ",
                         "adding %d seeded random tie(s)."),
                  where, as.integer(sum(B2 != B))))
  B2
}

## Abort gate after a simulated period.
.searchnet_bounds_end_gate <- function(B, b, where) {
  if (is.null(b)) return(invisible(TRUE))
  v <- .searchnet_bounds_violations(array(B, c(dim(B), 1L)), b)
  if (nrow(v))
    stop(sprintf(paste0(
      "searchnet: degree-bound gate failed in %s: the simulated state violates the ",
      "bounds:\n%s\nThe penalty (%g) was not large enough to hold the bound on this ",
      "run; raise `bound_penalty`. Aborting rather than returning a path outside ",
      "the stated bounds."), where, .searchnet_bounds_violation_text(v), b$penalty),
      call. = FALSE)
  invisible(TRUE)
}


## The same gate at every ministep of a period: replays the realized toggles
## from the period's start state and stops at the first state outside the
## bounds. `frame` is .searchnet_chain_frame() output.
.searchnet_bounds_chain_gate <- function(B0, frame, b, where,
                                         dv = "self$bipartite_rsienaDV") {
  if (is.null(b) || !nrow(frame)) return(invisible(TRUE))
  N <- ncol(B0)
  stab <- as.logical(frame$stability)
  from <- as.numeric(frame$id_from) + 1
  to   <- as.numeric(frame$id_to) + 1
  B <- B0; rd <- rowSums(B0); cd <- colSums(B0)
  out <- function(x, v) (!is.na(v[["min"]]) && x < v[["min"]]) ||
    (!is.na(v[["max"]]) && x > v[["max"]])
  for (k in which(!stab & frame$dv_varname == dv & to <= N)) {
    i <- from[k]; j <- to[k]
    d <- if (B[i, j] == 1) -1 else 1
    B[i, j] <- B[i, j] + d; rd[i] <- rd[i] + d; cd[j] <- cd[j] + d
    if (out(rd[i], b$actor) || out(cd[j], b$component))
      stop(sprintf(paste0(
        "searchnet: degree-bound gate failed in %s at ministep %d: actor %d now ",
        "holds %g component(s) and component %d has %g holder(s), outside the ",
        "bounds (actor %s; component %s). The penalty (%g) did not hold the bound ",
        "on this run; raise `bound_penalty`. Aborting."), where, k, as.integer(i),
        rd[i], as.integer(j), cd[j], .searchnet_bounds_fmt(b$actor),
        .searchnet_bounds_fmt(b$component), b$penalty), call. = FALSE)
  }
  invisible(TRUE)
}


## ---------------------------------------------------------------------------
## Landscapes
## ---------------------------------------------------------------------------

#' Flag Portfolios That Violate Degree Bounds
#'
#' For landscape plots and animations that enumerate an actor's candidate
#' portfolios: returns which candidates the degree bounds forbid, so a figure
#' can grey them out instead of coloring the large negative utility the fixed
#' penalty effects assign them. Nothing is changed in any existing figure; a
#' plotting script opts in by calling this function.
#'
#' @param portfolios A \eqn{K \times N}{K x N} 0/1 matrix, one candidate
#'   portfolio per row (or a single length-\eqn{N} vector).
#' @param degree_bounds Bounds in any form \code{\link{searchnet_degree_bounds}}
#'   documents, or a \code{saomnk_model} carrying them.
#' @param state Optional \eqn{M \times N}{M x N} matrix: the current holdings
#'   of every actor. Needed only for component bounds, which depend on the
#'   other actors' holdings.
#' @param actor Integer: the focal actor's row in \code{state}, whose row each
#'   candidate replaces. Required with \code{state}.
#' @return A logical vector of length \eqn{K}, \code{TRUE} where the portfolio
#'   is infeasible. Attribute \code{"reason"} gives, per row, the bound
#'   violated (\code{""} when feasible).
#' @export
#' @examples
#' configs <- as.matrix(expand.grid(rep(list(0:1), 3)))
#' searchnet_portfolio_infeasible(configs, c(min = 1, max = 2))
searchnet_portfolio_infeasible <- function(portfolios, degree_bounds,
                                           state = NULL, actor = NULL) {
  b <- if (inherits(degree_bounds, "saomnk_model"))
    .searchnet_model_bounds(degree_bounds)
  else .searchnet_as_degree_bounds(degree_bounds)
  P <- if (is.null(dim(portfolios))) matrix(portfolios, nrow = 1L) else as.matrix(portfolios)
  K <- nrow(P)
  reason <- rep("", K)
  if (is.null(b)) {
    out <- rep(FALSE, K); attr(out, "reason") <- reason
    return(out)
  }
  deg <- rowSums(P)
  if (!is.na(b$actor[["min"]])) reason[deg < b$actor[["min"]]] <- "actor below min"
  if (!is.na(b$actor[["max"]])) reason[reason == "" & deg > b$actor[["max"]]] <- "actor above max"
  if (!all(is.na(b$component))) {
    if (is.null(state) || is.null(actor))
      stop("component bounds depend on the other actors' holdings: supply `state` ",
           "and `actor`.", call. = FALSE)
    state <- as.matrix(state)
    if (ncol(state) != ncol(P))
      stop("`state` and `portfolios` must have the same number of components.",
           call. = FALSE)
    others <- colSums(state[-actor, , drop = FALSE])
    for (k in seq_len(K)) {
      if (nzchar(reason[k])) next
      cd <- others + P[k, ]
      if (!is.na(b$component[["min"]]) && any(cd < b$component[["min"]]))
        reason[k] <- "component below min"
      else if (!is.na(b$component[["max"]]) && any(cd > b$component[["max"]]))
        reason[k] <- "component above max"
    }
  }
  out <- nzchar(reason)
  attr(out, "reason") <- reason
  out
}

## Per-actor statistic columns for the penalty effects, used by the engine's
## utility decomposition (get_struct_mod_stats_mat_from_bi_mat). Each equals
## RSiena's statistic up to a constant, so its CHANGE for any toggle (what
## drives choices) is RSiena's, and it is zero on every state within the
## bounds: the penalty never shifts a reported utility on a feasible path.
## For the indicator effects the zero point depends on the sign: as a floor
## (theta > 0) the column counts components BELOW the threshold, negated; as a
## cap (theta < 0) it counts components AT OR ABOVE it.
.SEARCHNET_BOUND_STAT_EFFECTS <- c("outIso", "outTrunc", "antiInIso", "in2Plus",
                                   "in3Plus")

.searchnet_bound_stat <- function(effect, parm, theta, B) {
  deg <- rowSums(B); cdeg <- colSums(B); N <- ncol(B); M <- nrow(B)
  thr <- c(antiInIso = 1, in2Plus = 2, in3Plus = 3)
  if (effect == "outIso") return(as.numeric(deg == 0))
  if (effect == "outTrunc") {
    k <- if (is.null(parm) || is.na(parm)) 1 else parm
    return(pmin(deg, k) - k)
  }
  if (effect %in% names(thr)) {
    cnt <- sum(cdeg >= thr[[effect]])
    return(rep(if (isTRUE(theta > 0)) cnt - N else cnt, M))
  }
  stop("not a degree-bound statistic: ", effect, call. = FALSE)
}
