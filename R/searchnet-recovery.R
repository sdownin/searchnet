###############################################################################
## searchnet-recovery.R
##
## Planted-truth recovery and power harness for a bipartite SAOM.
##
## Simulates actor-by-component panels from a declared theta, re-estimates
## each panel with RSiena under one fixed algorithm, and reports per effect
## bias, RMSE, interval coverage, rejection rate and an approximate minimum
## detectable effect. Non-converged, non-identified and failed replications
## are counted under their own status and never silently dropped.
##
## Forward simulation follows the route search_rsiena() uses since 0.11.0
## (searchnet-path.R): one unconditional RSiena period per wave transition
## (simOnly, cond = FALSE, nsub = 0, n3 = 2, run 1 kept), each started from the
## previous period's end state. Effect requests reuse the coevolve helpers,
## seeds come from .searchnet_seed(), so streams never collide.
###############################################################################

.RECOVERY_STATUS <- c("converged", "not_converged", "non_identified",
                      "error_simulation", "error_estimation")
.RECOVERY_CRITERIA <- c("max_abs_bias", "min_coverage", "max_rmse",
                        "max_size", "min_converged_share")
.RECOVERY_DV <- "bip"

## ---------------------------------------------------------------------------
## Effect specification
## ---------------------------------------------------------------------------
## Accepts a character vector of shortNames ("inPop", or "inPop:creation"
## for a non-eval type) or a data frame with columns shortName, and optionally
## type and interaction1. Returns a data frame with a `label` column, which is
## the name theta_true must use. density is added when absent: getEffects()
## includes it for every bipartite DV, so a spec without it would estimate an
## effect the caller never planted.
.recovery_parse_spec <- function(effects_spec) {
  if (is.character(effects_spec)) {
    if (!length(effects_spec) || any(!nzchar(effects_spec)))
      stop("`effects_spec` must be a non-empty character vector.", call. = FALSE)
    parts <- strsplit(effects_spec, ":", fixed = TRUE)
    spec <- data.frame(
      shortName    = vapply(parts, `[`, character(1), 1L),
      type         = vapply(parts, function(p) if (length(p) > 1L) p[2L] else "eval",
                            character(1)),
      interaction1 = "",
      stringsAsFactors = FALSE)
  } else if (is.data.frame(effects_spec)) {
    if (is.null(effects_spec$shortName))
      stop("`effects_spec` as a data frame needs a `shortName` column.",
           call. = FALSE)
    spec <- data.frame(
      shortName    = as.character(effects_spec$shortName),
      type         = if (is.null(effects_spec$type)) "eval"
                     else as.character(effects_spec$type),
      interaction1 = if (is.null(effects_spec$interaction1)) ""
                     else as.character(effects_spec$interaction1),
      stringsAsFactors = FALSE)
    spec$interaction1[is.na(spec$interaction1)] <- ""
  } else {
    stop("`effects_spec` must be a character vector or a data frame.",
         call. = FALSE)
  }
  bad_type <- setdiff(spec$type, c("eval", "endow", "creation"))
  if (length(bad_type))
    stop("effect type(s) ", paste(sQuote(bad_type), collapse = ", "),
         " not recognized; use eval, endow or creation.", call. = FALSE)
  if (!any(spec$shortName == "density" & spec$type == "eval"))
    spec <- rbind(data.frame(shortName = "density", type = "eval",
                             interaction1 = "", stringsAsFactors = FALSE), spec)
  spec$label <- with(spec, paste0(
    shortName,
    ifelse(nzchar(interaction1), paste0("(", interaction1, ")"), ""),
    ifelse(type == "eval", "", paste0(":", type))))
  if (anyDuplicated(spec$label))
    stop("`effects_spec` names an effect twice: ",
         paste(unique(spec$label[duplicated(spec$label)]), collapse = ", "),
         call. = FALSE)
  spec
}

## ---------------------------------------------------------------------------
## RSiena data and effects
## ---------------------------------------------------------------------------
.recovery_data <- function(waves_list, covariates, M, N, allowOnly = TRUE) {
  W <- length(waves_list)
  arr <- array(unlist(lapply(waves_list, as.numeric)), dim = c(M, N, W))
  bip <- RSiena::sienaDependent(arr, type = "bipartite",
                                nodeSet = c("ACTORS", "COMPONENTS"),
                                allowOnly = allowOnly)
  args <- list(bip)
  names(args) <- .RECOVERY_DV
  for (nm in names(covariates))
    args[[nm]] <- RSiena::coCovar(as.numeric(covariates[[nm]]),
                                  nodeSet = "ACTORS")
  args$nodeSets <- list(RSiena::sienaNodeSet(M, "ACTORS"),
                        RSiena::sienaNodeSet(N, "COMPONENTS"))
  do.call(RSiena::sienaDataCreate, args)
}

## Include the spec's effects and verify each one, using the coevolve helpers.
## Returns the effects object with attribute "recovery_labels": one label per
## included row, in RSiena's order ("rate" for basic rate rows).
.recovery_effects <- function(dat, spec, bounds = NULL) {
  eff <- RSiena::getEffects(dat)
  requests <- lapply(seq_len(nrow(spec)), function(i)
    list(name = .RECOVERY_DV, shortName = spec$shortName[i],
         type = spec$type[i],
         interaction1 = if (nzchar(spec$interaction1[i])) spec$interaction1[i]
                        else NULL))
  ## includeEffects() prints the rows it touched; keep the harness quiet.
  for (r in requests) {
    utils::capture.output(
      ok <- try(eff <- .searchnet_include_one(eff, r), silent = TRUE))
    if (inherits(ok, "try-error")) next
  }
  status <- .searchnet_effect_status(eff, requests)
  if (any(!status$included)) {
    bad <- spec$label[!status$included]
    why <- ifelse(status$exists[!status$included],
                  "row exists but was not included",
                  "NO SUCH ROW for a bipartite DV (non-implementation)")
    stop("searchnet_recovery(): requested effect(s) not available:\n",
         paste0("    ", bad, " -- ", why, collapse = "\n"),
         "\n  A non-implementation says nothing about the mechanism; ",
         "choose an effect RSiena offers for a two-mode network.",
         call. = FALSE)
  }
  ## Degree bounds: fixed penalty effects, labeled "bound:<shortName>". They
  ## are modeling assumptions, never part of theta_true or of the estimates.
  bound_names <- character(0)
  if (!is.null(bounds)) {
    eff <- .searchnet_apply_bound_effects(eff, bounds, .RECOVERY_DV)
    bound_names <- bounds$effects$shortName
  }
  inc <- which(eff$include)
  labels <- vapply(inc, function(r) {
    if (eff$type[r] == "rate" && eff$shortName[r] == "Rate") return("rate")
    if (eff$type[r] == "eval" && eff$shortName[r] %in% bound_names &&
        isTRUE(eff$fix[r])) return(paste0("bound:", eff$shortName[r]))
    hit <- which(spec$shortName == eff$shortName[r] & spec$type == eff$type[r] &
                   spec$interaction1 == eff$interaction1[r])
    if (length(hit) != 1L) return(NA_character_)
    spec$label[hit]
  }, character(1))
  if (anyNA(labels))
    stop("searchnet_recovery(): RSiena included effect(s) the spec did not ",
         "name: ", paste(eff$effectName[inc][is.na(labels)], collapse = ", "),
         ". Add them to `effects_spec` with a planted value.", call. = FALSE)
  attr(eff, "recovery_labels") <- labels
  eff
}

## ---------------------------------------------------------------------------
## Simulation of one planted panel
## ---------------------------------------------------------------------------
.recovery_simulate_panel <- function(spec, theta, M, N, waves, init_density,
                                     covariates, seed, rep, bounds = NULL) {
  B <- .with_local_seed(.searchnet_seed(seed, "recovery:init", rep),
                        matrix(stats::rbinom(M * N, 1L, init_density), M, N))
  ## Under degree bounds the wave-1 draw is brought within them (seeded): ties
  ## above a cap dropped, then ties added below a floor. Part of the planted
  ## data-generating process, so wave 1 satisfies the bounds like every wave.
  if (!is.null(bounds)) B <- .recovery_bounded_start(B, bounds, seed, rep)
  panel <- list(B)
  for (w in 2:waves) {
    dat <- .recovery_data(list(B, B), covariates, M, N, allowOnly = FALSE)
    eff <- .recovery_effects(dat, spec, bounds)
    lab <- attr(eff, "recovery_labels")
    is_b <- startsWith(lab, "bound:")
    th <- numeric(length(lab))
    th[!is_b] <- unname(theta[lab[!is_b]])
    th[is_b]  <- eff$initialValue[which(eff$include)][is_b]
    bound <- max(50, ceiling(2 * max(abs(th))) + 1)
    alg <- do.call(RSiena::sienaAlgorithmCreate, .searchnet_bounds_algorithm_args(
      list(projname = NULL, simOnly = TRUE, cond = FALSE, nsub = 0, n3 = 2,
           seed = .searchnet_seed(seed, "recovery:sim", rep, w), silent = TRUE),
      bounds, .RECOVERY_DV, N = N, where = "searchnet_recovery()"))
    fit <- NULL
    utils::capture.output(fit <- RSiena::siena07(
      alg, data = dat, effects = eff, thetaValues = rbind(th, th),
      thetaBound = bound, batch = TRUE, silent = TRUE, returnDeps = TRUE,
      returnChains = !is.null(bounds)))
    B_prev <- B
    B <- .searchnet_sims_bipartite(fit$sims[[1L]], M, N, dv = .RECOVERY_DV)
    if (!is.null(bounds)) {
      ## Every ministep of the period, then the end state, within the bounds.
      where <- sprintf("recovery rep %d, wave %d", rep, w)
      .searchnet_bounds_chain_gate(
        B_prev, .searchnet_chain_frame(fit$chain[[1L]][[1L]][[1L]]), bounds,
        where, dv = .RECOVERY_DV)
      .searchnet_bounds_end_gate(B, bounds, where)
    }
    panel[[w]] <- B
  }
  panel
}

## Wave 1 within the bounds: trim ties above a cap, then add ties below a
## floor, both seeded; stop if the bounds cannot be met at this density.
.recovery_bounded_start <- function(B, b, seed, rep) {
  B <- .with_local_seed(.searchnet_seed(seed, "recovery:bounds_trim", rep), {
    amax <- b$actor[["max"]]
    if (!is.na(amax)) for (i in which(rowSums(B) > amax)) {
      on <- which(B[i, ] == 1)
      B[i, on[sample.int(length(on), length(on) - amax)]] <- 0
    }
    cmax <- b$component[["max"]]
    if (!is.na(cmax)) for (j in which(colSums(B) > cmax)) {
      on <- which(B[, j] == 1)
      B[on[sample.int(length(on), length(on) - cmax)], j] <- 0
    }
    B
  })
  B <- .searchnet_bounds_repair(B, b, .searchnet_seed(seed, "recovery:bounds_fill", rep))
  v <- .searchnet_bounds_violations(array(B, c(dim(B), 1L)), b)
  if (nrow(v))
    stop("the wave-1 draw could not be brought within the degree bounds:\n",
         .searchnet_bounds_violation_text(v), call. = FALSE)
  B
}

## ---------------------------------------------------------------------------
## Estimation of one panel under the fixed algorithm
## ---------------------------------------------------------------------------
.recovery_estimate <- function(panel, spec, covariates, M, N, est_seed,
                               n3, nsub, nbrNodes, algorithm_args,
                               bounds = NULL) {
  ## Estimation under bounds: the panel must satisfy them, the penalty effects
  ## are entered fixed, and the actor cap is MaxDegree (method of moments).
  if (!is.null(bounds)) searchnet_check_degree_bounds(panel, bounds)
  dat <- .recovery_data(panel, covariates, M, N, allowOnly = TRUE)
  eff <- .recovery_effects(dat, spec, bounds)
  alg <- do.call(RSiena::sienaAlgorithmCreate, .searchnet_bounds_algorithm_args(
    utils::modifyList(list(
      projname = NULL, seed = est_seed, n3 = as.integer(n3),
      nsub = as.integer(nsub), cond = FALSE, silent = TRUE), algorithm_args),
    bounds, .RECOVERY_DV, N = N, where = "searchnet_recovery()"))
  fit <- NULL
  utils::capture.output(fit <- RSiena::siena07(
    alg, data = dat, effects = eff, batch = TRUE, silent = TRUE,
    nbrNodes = as.integer(nbrNodes), useCluster = nbrNodes > 1L,
    returnDeps = FALSE))
  lab <- attr(eff, "recovery_labels")
  n_rate <- sum(lab == "rate")
  lab[lab == "rate"] <- sprintf("rate (period %d)", seq_len(n_rate))
  se <- if (is.null(fit$covtheta)) rep(NA_real_, length(fit$theta))
        else sqrt(pmax(diag(as.matrix(fit$covtheta)), 0))
  se[!is.finite(se)] <- NA_real_
  ## Fixed bound effects carry no standard error and are not estimates.
  keep <- !startsWith(lab, "bound:")
  list(effect = lab[keep], estimate = as.numeric(fit$theta)[keep],
       se = as.numeric(se)[keep],
       tconv_max = if (is.null(fit$tconv.max)) NA_real_
                   else as.numeric(fit$tconv.max))
}

## ---------------------------------------------------------------------------
## Summary statistics and verdict
## ---------------------------------------------------------------------------
.recovery_mde <- function(mean_se, alpha, power)
  (stats::qnorm(1 - alpha / 2) + stats::qnorm(power)) * mean_se

.recovery_table <- function(est, truth, alpha, power, ci_level) {
  z_ci  <- stats::qnorm(1 - (1 - ci_level) / 2)
  z_rej <- stats::qnorm(1 - alpha / 2)
  do.call(rbind, lapply(names(truth), function(e) {
    d <- est[est$effect == e, , drop = FALSE]
    n <- nrow(d)
    tv <- unname(truth[e])
    if (n == 0L) {
      return(data.frame(effect = e, true = tv, n_converged = 0L,
                        mean_est = NA_real_, bias = NA_real_, mcse_bias = NA_real_,
                        rmse = NA_real_, emp_sd = NA_real_, mean_se = NA_real_,
                        coverage = NA_real_, mcse_coverage = NA_real_,
                        rejection = NA_real_, mcse_rejection = NA_real_,
                        mde = NA_real_, stringsAsFactors = FALSE))
    }
    err <- d$estimate - tv
    sdv <- if (n > 1L) stats::sd(d$estimate) else NA_real_
    cover <- mean(abs(err) <= z_ci * d$se)
    rej <- mean(abs(d$estimate / d$se) > z_rej)
    mse <- mean(d$se)
    data.frame(
      effect = e, true = tv, n_converged = n,
      mean_est = mean(d$estimate), bias = mean(err),
      mcse_bias = sdv / sqrt(n),
      rmse = sqrt(mean(err^2)), emp_sd = sdv, mean_se = mse,
      coverage = cover, mcse_coverage = sqrt(cover * (1 - cover) / n),
      rejection = rej, mcse_rejection = sqrt(rej * (1 - rej) / n),
      mde = .recovery_mde(mse, alpha, power),
      stringsAsFactors = FALSE)
  }))
}

## Verdict against an explicit criterion. NULL criterion -> "not assessed".
## Returns list(verdict, checks) where checks is one row per (effect, test).
.recovery_verdict <- function(table, criterion, assessed, n_converged, reps) {
  if (is.null(criterion))
    return(list(verdict = "not assessed", checks = NULL,
                reason = "no `criterion` supplied"))
  if (!is.list(criterion) || is.null(names(criterion)) ||
      any(!nzchar(names(criterion))))
    stop("`criterion` must be a named list.", call. = FALSE)
  unknown <- setdiff(names(criterion), .RECOVERY_CRITERIA)
  if (length(unknown))
    stop("unknown criterion element(s): ", paste(unknown, collapse = ", "),
         ". Recognized: ", paste(.RECOVERY_CRITERIA, collapse = ", "), ".",
         call. = FALSE)
  criterion <- criterion[!vapply(criterion, is.null, logical(1))]
  if (!length(criterion))
    return(list(verdict = "not assessed", checks = NULL,
                reason = "every `criterion` element is NULL"))
  pick <- function(val, e) {
    if (length(val) == 1L && is.null(names(val))) return(val)
    if (e %in% names(val)) return(unname(val[[e]]))
    NA_real_
  }
  rows <- list()
  add <- function(effect, test, value, threshold, pass)
    rows[[length(rows) + 1L]] <<- data.frame(
      effect = effect, test = test, value = value, threshold = threshold,
      pass = pass, stringsAsFactors = FALSE)
  if (!is.null(criterion$min_converged_share)) {
    sh <- n_converged / reps
    add("(all)", "converged share >=", sh, criterion$min_converged_share,
        sh >= criterion$min_converged_share)
  }
  for (e in assessed) {
    r <- table[table$effect == e, , drop = FALSE]
    chk <- function(nm, test, value, cmp) {
      if (is.null(criterion[[nm]])) return(invisible())
      th <- pick(criterion[[nm]], e)
      if (is.na(th)) return(invisible())
      add(e, test, value, th, is.finite(value) && cmp(value, th))
    }
    chk("max_abs_bias", "|bias| <=", abs(r$bias), `<=`)
    chk("min_coverage", "coverage >=", r$coverage, `>=`)
    chk("max_rmse", "RMSE <=", r$rmse, `<=`)
    if (isTRUE(r$true == 0))
      chk("max_size", "size (rejection | theta = 0) <=", r$rejection, `<=`)
  }
  checks <- if (length(rows)) do.call(rbind, rows) else NULL
  if (is.null(checks))
    return(list(verdict = "not assessed", checks = NULL,
                reason = "no criterion element applied to an assessed effect"))
  list(verdict = if (all(checks$pass)) "PASS" else "FAIL", checks = checks,
       reason = if (n_converged == 0L) "no replication converged" else NULL)
}

## ---------------------------------------------------------------------------
## Exported harness
## ---------------------------------------------------------------------------

#' Planted-Truth Recovery and Power Harness for a Bipartite SAOM
#'
#' Simulates actor-by-component panels from a declared parameter vector
#' (\code{theta_true}), re-estimates every panel with RSiena under one fixed
#' algorithm, and reports how well each planted parameter is recovered: bias,
#' RMSE, confidence-interval coverage, the rejection rate of
#' \eqn{H_0: \theta = 0}, and an approximate minimum detectable effect (MDE).
#' This is the planted-truth gate the Design Defect Ledger asks for before a
#' SAOM-NK design is trusted: a design should return zero where nothing is
#' planted and recover what is planted.
#'
#' @section What the result does and does not show:
#' Recovery is shown at the stated \code{M}, \code{N}, \code{waves},
#' \code{reps}, planted \code{theta_true}, initial density and algorithm
#' settings, and nothing more. A design that recovers its parameters at
#' \code{M = 30} says nothing about \code{M = 10}, a different density, or an
#' added effect. With few \code{reps} the Monte Carlo standard errors
#' (\code{mcse_*} columns) are large and a PASS or FAIL may not survive more
#' replications; read them before reading the verdict.
#'
#' @section Simulation route:
#' Wave 1 is an independent Bernoulli draw at \code{init_density}. Each later
#' wave is the end state of one unconditional RSiena period
#' (\code{simOnly = TRUE}, \code{cond = FALSE}) started from the previous
#' wave, with basic rate \code{theta_true["rate"]} and the declared effects.
#' This is the same route \code{search_rsiena()} uses since 0.11.0.
#'
#' @section Estimation, held fixed:
#' Every replication is estimated with the same algorithm: unconditional
#' method of moments (\code{cond = FALSE}, so period rates are estimated with
#' standard errors and are directly comparable to the planted rate), the given
#' \code{n3}, \code{nsub} and \code{nbrNodes}, and a per-replication seed. Per
#' the house convention, \code{nbrNodes} enters the algorithm (Phase 2
#' averaging and gain) and must be held fixed across any comparison of two
#' harness runs, together with \code{seed}.
#'
#' @section Replication status, never dropped silently:
#' Each replication is classified as \code{"converged"} (overall maximum
#' convergence ratio \code{tconv.max} below \code{conv_threshold} and a finite
#' standard error for every parameter), \code{"not_converged"} (\code{tconv.max}
#' at or above the threshold, or missing), \code{"non_identified"} (a standard
#' error is missing: a non-identification, not a fit), \code{"error_simulation"}
#' or \code{"error_estimation"}. Summary statistics use converged replications
#' only; every status is counted and printed, and \code{$reps} keeps the
#' estimates of non-converged fits for inspection.
#'
#' @section Statistics:
#' Over the \eqn{n} converged replications, with planted value \eqn{\theta}:
#' \code{bias} \eqn{= \bar{\hat\theta} - \theta} with Monte Carlo SE
#' \eqn{\mathrm{sd}(\hat\theta)/\sqrt{n}}; \code{rmse}; \code{coverage}, the
#' share of \code{ci_level} Wald intervals containing \eqn{\theta}, with Monte
#' Carlo SE \eqn{\sqrt{c(1-c)/n}}; \code{rejection}, the share of fits with
#' \eqn{|\hat\theta/\mathrm{SE}| > z_{1-\alpha/2}} (power when \eqn{\theta \ne
#' 0}, size when \eqn{\theta = 0}).
#'
#' \code{mde} \eqn{= (z_{1-\alpha/2} + z_{\mathrm{power}})\,\overline{\mathrm{SE}}}
#' is an \strong{approximation}: it treats the Wald statistic as normal with
#' standard error equal to the mean estimated SE at the planted values, and
#' ignores that the SE itself changes with \eqn{\theta}. It is the smallest
#' effect the design would detect with probability \code{power} at level
#' \code{alpha} under that approximation. Report it with every null.
#'
#' @section Null mode (size check):
#' With \code{null = TRUE} and a single \code{focal} effect, the focal planted
#' value is replaced by 0 before simulating. Its \code{rejection} column is then
#' the empirical size, which should be close to \code{alpha}; the criterion
#' element \code{max_size} turns that into a gate.
#'
#' @section Verdict:
#' A verdict is given only when \code{criterion} is supplied explicitly;
#' otherwise it is \code{"not assessed"}. Recognized elements:
#' \code{max_abs_bias}, \code{min_coverage}, \code{max_rmse} (each a scalar for
#' every assessed effect, or a named vector by effect label), \code{max_size}
#' (applied to assessed effects planted at 0) and \code{min_converged_share}.
#' Assessed effects are \code{focal}, or every non-rate effect when
#' \code{focal} is \code{NULL}. The verdict is \code{"PASS"} only when every
#' applicable check passes; an effect with no converged replication fails.
#'
#' @param effects_spec Character vector of RSiena shortNames for the bipartite
#'   dependent network, optionally suffixed with a type
#'   (\code{"inPop:creation"}, \code{"inPop:endow"}; the house convention is
#'   \code{creation + endow} rather than \code{eval + endow}), or a data frame
#'   with columns \code{shortName}, \code{type} and \code{interaction1} (for
#'   covariate effects such as \code{egoX}). \code{density} is added when
#'   absent.
#' @param theta_true Named numeric vector of planted values: \code{rate} (the
#'   basic rate per period) plus one entry per effect label. Labels are the
#'   shortName, with \code{"(cov)"} for an \code{interaction1} and
#'   \code{":type"} for a non-eval type, e.g. \code{c(rate = 4, density = -1.5,
#'   inPop = 0.2)}.
#' @param reps Integer. Number of planted panels. Default 50.
#' @param M,N Integers. Number of actors and components.
#' @param waves Integer, at least 2. Observation waves per panel. Default 3.
#' @param nbrNodes Integer. RSiena \code{nbrNodes}, held fixed for every
#'   replication. Default 1.
#' @param seed Integer base seed. Per-replication seeds for the initial draw,
#'   each simulated period and the estimation are derived from it by
#'   purpose-namespaced hashing, so the same \code{seed} gives identical
#'   results.
#' @param alpha Two-sided test level for the rejection rate and MDE.
#' @param power Target power for the MDE. Default 0.80.
#' @param criterion \code{NULL} (default; verdict \code{"not assessed"}) or a
#'   named list; see the Verdict section.
#' @param focal Character vector of effect labels to assess. Default
#'   \code{NULL}: every non-rate effect.
#' @param null Logical. Null mode; requires a single \code{focal} effect.
#' @param ci_level Confidence level of the Wald intervals for coverage.
#'   Default 0.95.
#' @param init_density Density of the wave-1 Bernoulli draw. Default 0.2.
#' @param covariates Optional named list of numeric actor covariates (length
#'   \code{M}), entered with \code{coCovar(nodeSet = "ACTORS")}. Note that
#'   \code{coCovar()} centers by default, so planted coefficients refer to the
#'   centered covariate.
#' @param n3,nsub Phase-3 iterations and Phase-2 subphases of the estimation
#'   algorithm. Defaults 500 and 4.
#' @param conv_threshold Overall maximum convergence ratio below which a fit
#'   counts as converged. Default 0.25, the manual's guidance for published
#'   results.
#' @param algorithm_args Named list of further
#'   \code{\link[RSiena]{sienaAlgorithmCreate}} arguments, applied to every
#'   replication.
#' @param keep_panels Logical. Keep the simulated panels in the result.
#'   Default \code{FALSE}.
#' @param verbose Logical. Print one line per replication. Default
#'   \code{FALSE}.
#' @param degree_bounds Optional degree bounds for the planted
#'   data-generating process, in any form
#'   \code{\link{searchnet_degree_bounds}} accepts. Every simulated period
#'   uses RSiena's \code{MaxDegree} for an actor cap and fixed penalty effects
#'   for floors and component caps; the wave-1 draw is brought within the
#'   bounds (seeded: ties above a cap dropped, then ties below a floor added);
#'   and every wave is gated. The penalty effects are not part of
#'   \code{theta_true} or of the estimates. Default \code{NULL}.
#' @param estimate_with_bounds Logical. With \code{degree_bounds}, estimate
#'   each panel under the same bounds (\code{TRUE}, default: data checked,
#'   penalty effects fixed, \code{MaxDegree} set; method of moments only) or
#'   ignoring them (\code{FALSE}), which measures the cost of omitting a bound
#'   that generated the data.
#'
#' @return An object of class \code{"searchnet_recovery"}: a list with
#'   \code{table} (one row per parameter: \code{effect}, \code{true},
#'   \code{n_converged}, \code{mean_est}, \code{bias}, \code{mcse_bias},
#'   \code{rmse}, \code{emp_sd}, \code{mean_se}, \code{coverage},
#'   \code{mcse_coverage}, \code{rejection}, \code{mcse_rejection},
#'   \code{mde}), \code{reps} (per replication: \code{rep}, \code{status},
#'   \code{tconv_max}, \code{message}), \code{estimates} (long form, every
#'   replication that produced estimates), \code{status_counts},
#'   \code{verdict}, \code{checks}, \code{settings}, \code{theta_true},
#'   \code{elapsed} (seconds) and \code{provenance}.
#'
#' @seealso \code{\link{searchnet_coevolve}}; the simulation route is the one \code{search_rsiena()} uses.
#' @export
#' @examples
#' \donttest{
#' rec <- searchnet_recovery(c("density", "inPop"),
#'                           theta_true = c(rate = 3, density = -1.2, inPop = 0.15),
#'                           reps = 2, M = 12, N = 8, waves = 3,
#'                           n3 = 100, nsub = 2, seed = 1)
#' rec
#' }
searchnet_recovery <- function(effects_spec, theta_true, reps = 50L, M, N,
                               waves = 3L, nbrNodes = 1L, seed = 1L,
                               alpha = 0.05, power = 0.80,
                               criterion = NULL, focal = NULL, null = FALSE,
                               ci_level = 0.95, init_density = 0.2,
                               covariates = NULL, n3 = 500L, nsub = 4L,
                               conv_threshold = 0.25, algorithm_args = list(),
                               keep_panels = FALSE, verbose = FALSE,
                               degree_bounds = NULL,
                               estimate_with_bounds = TRUE) {
  cl <- match.call()
  t_start <- proc.time()[["elapsed"]]

  ## ---- validation ---------------------------------------------------------
  if (missing(M) || missing(N))
    stop("`M` (actors) and `N` (components) must be given.", call. = FALSE)
  M <- as.integer(M); N <- as.integer(N); waves <- as.integer(waves)
  reps <- as.integer(reps); nbrNodes <- as.integer(nbrNodes)
  if (M < 2L || N < 2L) stop("`M` and `N` must be at least 2.", call. = FALSE)
  if (waves < 2L) stop("`waves` must be at least 2.", call. = FALSE)
  if (reps < 1L) stop("`reps` must be at least 1.", call. = FALSE)
  for (a in c("alpha", "power", "ci_level", "init_density")) {
    v <- get(a)
    if (!is.numeric(v) || length(v) != 1L || !(v > 0 && v < 1))
      stop("`", a, "` must be a number strictly between 0 and 1.", call. = FALSE)
  }
  spec <- .recovery_parse_spec(effects_spec)
  if (!is.numeric(theta_true) || is.null(names(theta_true)))
    stop("`theta_true` must be a named numeric vector.", call. = FALSE)
  need <- c("rate", spec$label)
  miss <- setdiff(need, names(theta_true))
  if (length(miss))
    stop("`theta_true` lacks planted value(s) for: ",
         paste(miss, collapse = ", "), ". Names must be: ",
         paste(need, collapse = ", "), ".", call. = FALSE)
  extra <- setdiff(names(theta_true), need)
  if (length(extra))
    stop("`theta_true` names effect(s) not in `effects_spec`: ",
         paste(extra, collapse = ", "), ".", call. = FALSE)
  theta_true <- theta_true[need]
  if (!all(is.finite(theta_true)))
    stop("`theta_true` must be finite.", call. = FALSE)
  if (theta_true[["rate"]] <= 0)
    stop("`theta_true[\"rate\"]` must be positive.", call. = FALSE)
  if (!is.null(focal)) {
    bad <- setdiff(focal, spec$label)
    if (length(bad))
      stop("`focal` names effect(s) not in `effects_spec`: ",
           paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  if (isTRUE(null)) {
    if (length(focal) != 1L)
      stop("null mode needs exactly one `focal` effect.", call. = FALSE)
    if (focal == "density")
      stop("null mode on density is not a size check: density is the ",
           "baseline tie propensity.", call. = FALSE)
    theta_true[[focal]] <- 0
  }
  if (!is.null(covariates)) {
    if (!is.list(covariates) || is.null(names(covariates)) ||
        any(!nzchar(names(covariates))))
      stop("`covariates` must be a named list.", call. = FALSE)
    if (any(vapply(covariates, length, integer(1)) != M))
      stop("each covariate must have length M = ", M, ".", call. = FALSE)
    if (.RECOVERY_DV %in% names(covariates))
      stop("a covariate may not be named '", .RECOVERY_DV, "'.", call. = FALSE)
  }
  if (!is.list(algorithm_args))
    stop("`algorithm_args` must be a list.", call. = FALSE)
  fixed_keys <- intersect(names(algorithm_args), c("seed", "projname"))
  if (length(fixed_keys))
    stop("`algorithm_args` may not set ", paste(fixed_keys, collapse = ", "),
         ": the harness derives per-replication seeds from `seed`.",
         call. = FALSE)

  bounds <- .searchnet_as_degree_bounds(degree_bounds)
  if (!is.logical(estimate_with_bounds) || length(estimate_with_bounds) != 1L ||
      is.na(estimate_with_bounds))
    stop("`estimate_with_bounds` must be TRUE or FALSE.", call. = FALSE)
  est_bounds <- if (isTRUE(estimate_with_bounds)) bounds else NULL
  if (!is.null(bounds)) {
    .searchnet_bounds_check_dims(bounds, M, N)
    clash <- intersect(spec$shortName, bounds$effects$shortName)
    if (length(clash))
      stop("`effects_spec` names ", paste(clash, collapse = ", "),
           ", which the degree bounds use as a fixed penalty effect.", call. = FALSE)
    if (!is.null(algorithm_args$MaxDegree))
      stop("`algorithm_args` may not set MaxDegree when `degree_bounds` is given: ",
           "the bounds set it.", call. = FALSE)
  }
  .searchnet_bounds_refuse_ml(est_bounds, algorithm_args$maxlike,
                              "searchnet_recovery()")

  assessed <- if (is.null(focal)) spec$label else focal

  ## ---- pre-flight: every requested effect must exist for this data shape ---
  ## Done once, outside the replication loop, so an unavailable effect is an
  ## error the caller sees rather than `reps` replications filed under
  ## "error_simulation".
  probe_B <- matrix(rep_len(c(1, 0, 0), M * N), M, N)
  .recovery_effects(.recovery_data(list(probe_B, probe_B), covariates, M, N,
                                   allowOnly = FALSE), spec, bounds)

  ## ---- replications -------------------------------------------------------
  rep_rows <- vector("list", reps)
  est_rows <- vector("list", reps)
  panels   <- if (keep_panels) vector("list", reps) else NULL
  for (r in seq_len(reps)) {
    status <- NA_character_; msg <- ""; tcm <- NA_real_; est <- NULL
    panel <- tryCatch(
      .recovery_simulate_panel(spec, theta_true, M, N, waves, init_density,
                               covariates, seed, r, bounds = bounds),
      error = function(e) e)
    if (inherits(panel, "error")) {
      status <- "error_simulation"; msg <- conditionMessage(panel)
    } else {
      if (keep_panels) panels[[r]] <- panel
      est <- tryCatch(
        .recovery_estimate(panel, spec, covariates, M, N,
                           .searchnet_seed(seed, "recovery:est", r),
                           n3, nsub, nbrNodes, algorithm_args,
                           bounds = est_bounds),
        error = function(e) e)
      if (inherits(est, "error")) {
        status <- "error_estimation"; msg <- conditionMessage(est); est <- NULL
      } else {
        tcm <- est$tconv_max
        status <- if (any(is.na(est$se))) "non_identified"
                  else if (!is.finite(tcm) || tcm >= conv_threshold) "not_converged"
                  else "converged"
      }
    }
    rep_rows[[r]] <- data.frame(rep = r, status = status, tconv_max = tcm,
                                message = msg, stringsAsFactors = FALSE)
    if (!is.null(est))
      est_rows[[r]] <- data.frame(rep = r, status = status,
                                  effect = est$effect, estimate = est$estimate,
                                  se = est$se, stringsAsFactors = FALSE)
    if (verbose)
      message(sprintf("searchnet_recovery(): rep %d/%d %s (tconv.max %s)",
                      r, reps, status,
                      if (is.finite(tcm)) sprintf("%.3f", tcm) else "NA"))
  }
  rep_df <- do.call(rbind, rep_rows)
  est_df <- do.call(rbind, est_rows[!vapply(est_rows, is.null, logical(1))])
  if (is.null(est_df))
    est_df <- data.frame(rep = integer(0), status = character(0),
                         effect = character(0), estimate = numeric(0),
                         se = numeric(0), stringsAsFactors = FALSE)

  ## Truth per estimated parameter: one rate per period.
  truth <- c(stats::setNames(rep(theta_true[["rate"]], waves - 1L),
                             sprintf("rate (period %d)", seq_len(waves - 1L))),
             theta_true[spec$label])
  conv <- est_df[est_df$status == "converged", , drop = FALSE]
  tab <- .recovery_table(conv, truth, alpha, power, ci_level)
  rownames(tab) <- NULL
  n_conv <- sum(rep_df$status == "converged")
  counts <- table(factor(rep_df$status, levels = .RECOVERY_STATUS))
  v <- .recovery_verdict(tab, criterion, assessed, n_conv, reps)

  out <- list(
    table = tab, reps = rep_df, estimates = est_df,
    status_counts = stats::setNames(as.integer(counts), names(counts)),
    n_converged = n_conv, verdict = v$verdict, checks = v$checks,
    verdict_reason = v$reason,
    theta_true = theta_true, spec = spec, assessed = assessed,
    criterion = criterion,
    settings = list(M = M, N = N, waves = waves, reps = reps,
                    nbrNodes = nbrNodes, seed = seed, alpha = alpha,
                    power = power, ci_level = ci_level,
                    init_density = init_density, n3 = as.integer(n3),
                    nsub = as.integer(nsub), conv_threshold = conv_threshold,
                    cond = FALSE, null = isTRUE(null), focal = focal,
                    algorithm_args = algorithm_args,
                    degree_bounds = bounds,
                    estimate_with_bounds = !is.null(est_bounds)),
    panels = panels,
    elapsed = proc.time()[["elapsed"]] - t_start,
    provenance = .searchnet_provenance(seed = seed, call = cl))
  class(out) <- "searchnet_recovery"
  out
}

## ---------------------------------------------------------------------------
## Methods
## ---------------------------------------------------------------------------

.recovery_scope_line <- function(s) {
  out <- sprintf(paste0("Shown: recovery at M = %d, N = %d, waves = %d, reps = %d, ",
                        "init_density = %g, n3 = %d, nsub = %d, nbrNodes = %d. ",
                        "Nothing more."),
                 s$M, s$N, s$waves, s$reps, s$init_density, s$n3, s$nsub, s$nbrNodes)
  b <- s$degree_bounds
  if (!is.null(b))
    out <- paste0(out, sprintf(paste0(
      "
  Degree bounds in the DGP: actor %s; component %s (fixed, not estimated). ",
      "Estimated %s the bounds."),
      .searchnet_bounds_fmt(b$actor), .searchnet_bounds_fmt(b$component),
      if (isTRUE(s$estimate_with_bounds)) "WITH" else "WITHOUT"))
  out
}

.recovery_status_line <- function(x) {
  sc <- x$status_counts
  paste(sprintf("%s %d", names(sc), sc), collapse = ", ")
}

#' @rdname searchnet_recovery
#' @param x A \code{"searchnet_recovery"} object.
#' @param digits Digits to print.
#' @param ... Unused.
#' @method print searchnet_recovery
#' @export
print.searchnet_recovery <- function(x, digits = 3, ...) {
  s <- x$settings
  cat(sprintf("SAOM-NK planted-truth %s: bipartite SAOM, %d reps\n",
              if (s$null) "null (size) check" else "recovery",
              s$reps))
  cat(sprintf("  replications: %s\n", .recovery_status_line(x)))
  cat(sprintf("  summaries use the %d converged replication(s) (tconv.max < %g)\n\n",
              x$n_converged, s$conv_threshold))
  tab <- x$table[, c("effect", "true", "mean_est", "bias", "rmse",
                     "coverage", "rejection", "mde")]
  names(tab)[names(tab) == "rejection"] <- "reject"
  num <- vapply(tab, is.numeric, logical(1))
  tab[num] <- lapply(tab[num], round, digits = digits)
  print(tab, row.names = FALSE)
  cat(sprintf(paste0("\n  coverage: %g%% Wald intervals; reject: share with ",
                     "|est/SE| > z at alpha = %g (size where true = 0);\n",
                     "  mde: (z_{1-alpha/2} + z_power) * mean SE at power = %g ",
                     "(an approximation).\n"),
              100 * s$ci_level, s$alpha, s$power))
  cat(sprintf("  verdict: %s%s\n", x$verdict,
              if (!is.null(x$verdict_reason)) paste0(" (", x$verdict_reason, ")")
              else ""))
  if (x$n_converged < 30L)
    cat(sprintf(paste0("  caution: %d converged replication(s); Monte Carlo ",
                       "error is large (see summary() mcse_* columns).\n"),
                x$n_converged))
  cat("  ", .recovery_scope_line(s), "\n", sep = "")
  invisible(x)
}

#' @rdname searchnet_recovery
#' @param object A \code{"searchnet_recovery"} object.
#' @method summary searchnet_recovery
#' @export
summary.searchnet_recovery <- function(object, ...) {
  out <- list(table = object$table, checks = object$checks,
              verdict = object$verdict, verdict_reason = object$verdict_reason,
              status_counts = object$status_counts, reps = object$reps,
              settings = object$settings, elapsed = object$elapsed,
              assessed = object$assessed)
  class(out) <- "summary.searchnet_recovery"
  out
}

#' @rdname searchnet_recovery
#' @method print summary.searchnet_recovery
#' @export
print.summary.searchnet_recovery <- function(x, digits = 3, ...) {
  s <- x$settings
  cat("SAOM-NK planted-truth recovery: summary\n\n")
  cat("Settings\n")
  cat(sprintf("  M = %d, N = %d, waves = %d, reps = %d, seed = %s\n",
              s$M, s$N, s$waves, s$reps, format(s$seed)))
  cat(sprintf("  estimation: cond = FALSE, n3 = %d, nsub = %d, nbrNodes = %d (fixed for every rep)\n",
              s$n3, s$nsub, s$nbrNodes))
  cat(sprintf("  converged means tconv.max < %g and every SE finite; elapsed %.1f s\n\n",
              s$conv_threshold, x$elapsed))
  cat("Replication status (none dropped silently)\n")
  print(x$status_counts)
  nc <- x$reps[x$reps$status != "converged", , drop = FALSE]
  if (nrow(nc)) {
    cat("\n  Not converged / failed:\n")
    nc$message <- substr(nc$message, 1L, 60L)
    nc$tconv_max <- round(nc$tconv_max, digits)
    print(nc, row.names = FALSE)
  }
  cat("\nPer-parameter recovery (Monte Carlo SEs in mcse_*)\n")
  tab <- x$table
  num <- vapply(tab, is.numeric, logical(1))
  tab[num] <- lapply(tab[num], round, digits = digits)
  print(tab, row.names = FALSE)
  cat("\nVerdict: ", x$verdict,
      if (!is.null(x$verdict_reason)) paste0(" (", x$verdict_reason, ")") else "",
      "\n", sep = "")
  if (!is.null(x$checks)) {
    ch <- x$checks
    ch$value <- round(ch$value, digits)
    print(ch, row.names = FALSE)
  }
  cat("\n", .recovery_scope_line(s), "\n", sep = "")
  invisible(x)
}

#' @rdname searchnet_recovery
#' @param y Unused.
#' @param include_rates Logical. Plot the period rates too. Default FALSE.
#' @export
plot.searchnet_recovery <- function(x, y, include_rates = FALSE, ...) {
  d <- x$estimates[x$estimates$status == "converged", , drop = FALSE]
  if (!include_rates) d <- d[!grepl("^rate \\(period", d$effect), , drop = FALSE]
  if (!nrow(d)) {
    message("plot.searchnet_recovery(): no converged estimates to plot.")
    return(invisible(x))
  }
  eff <- unique(d$effect)
  d$effect <- factor(d$effect, levels = eff)
  graphics::boxplot(estimate ~ effect, data = d, horizontal = TRUE, las = 1,
                    xlab = "estimate (converged replications)", ylab = "",
                    main = sprintf("Planted-truth recovery, %d of %d reps",
                                   x$n_converged, x$settings$reps), ...)
  tv <- x$table$true[match(eff, x$table$effect)]
  graphics::points(tv, seq_along(eff), pch = 4, cex = 1.6, lwd = 2)
  graphics::legend("bottomright", legend = "planted value", pch = 4,
                   bty = "n")
  invisible(x)
}
