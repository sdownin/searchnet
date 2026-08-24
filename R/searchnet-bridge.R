#' @title Empirical Bridge: SAOM Estimation to SaoMNK Simulation
#' @description
#' Functions that bridge between empirical SAOM estimation (e.g., from the ORM
#' project) and SaoMNK counterfactual simulation.  The core idea is that
#' RSiena's \code{siena07()} estimates the actor-oriented conditional logit
#' utility for an observed network; these estimated parameters can be
#' transplanted directly into SaoMNK's bipartite structure model to run
#' calibrated counterfactual simulations.
#'
#' The workflow is:
#' \enumerate{
#'   \item Estimate a SAOM with RSiena on empirical data (outside SaoMNK).
#'   \item Use \code{\link{saom_to_saomnk}} to map estimated thetas to
#'         SaoMNK's effect parameterization.
#'   \item Use \code{\link{empirical_to_saomnk_env}} to build a bipartite
#'         environment from the empirical network.
#'   \item Use \code{\link{run_calibrated_counterfactual}} to compare baseline
#'         vs. counterfactual scenarios.
#' }
#'
#' @name searchnet-bridge
#' @importFrom stats coef setNames sd quantile rnorm
NULL


# ---------------------------------------------------------------------------- #
#  Internal constants
# ---------------------------------------------------------------------------- #

## Canonical DV name (mirrors saomnk-api.R)
.BRIDGE_DV_NAME <- "self$bipartite_rsienaDV"

## The four K-degree measures compared by run_calibrated_counterfactual()
.BRIDGE_K_MEASURES <- c("K_AC", "K_CA", "K_AA", "K_CC")

## RSiena rate-function effects.  These have no representation in the SaoMNK
## structure model, which governs opportunity through `iterations` rather than
## through an estimated rate function.  They are extracted and reported, never
## silently dropped.
.BRIDGE_RATE_SHORTNAMES <- c("Rate", "RateX", "outRate", "outRateInv", "outRateLog")

## The RSiena slot the bridge registers the influence matrix W into, and the
## SaoMNK effect that reads it.  The engine addresses dyadic covariates by the
## declared R6 field name (see R/saomnk-base.R, the XWX branch of
## `include_rsiena_effect_from_eff_list()`), so the covariate entry and the
## effect entry must name the SAME slot or the effect is registered against
## nothing.
.BRIDGE_W_SLOT     <- "self$component_1_coDyadCovar"
.BRIDGE_W_EFFECT   <- "XWX"

## SaoMNK effects that are identified by a registered covariate (`interaction1`).
## Converting one out of a SAOM produces the effect NAME but not the covariate
## it was estimated on, and the bridge registers no monadic or actor-component
## covariates.  Such an effect either never reaches the utility function (the
## engine warns and skips) or kills the run (`totInDist2` / `simEgoInDist2`
## dereference a covariate that is not there), so they are dropped with a
## warning before the first environment is built.  `XWX` is excluded because the
## bridge registers W itself.
.BRIDGE_COVARIATE_DEPENDENT <- c("egoX", "altX", "X", "totInDist2", "simEgoInDist2")

## Effects RSiena does not implement for a BIPARTITE dependent variable, which
## is the only kind SaoMNK simulates.  Verified against RSiena 1.5.0 by calling
## `getEffects()` on a bipartite `sienaDataCreate()` object: `transTriads`
## appears in the `symmetricObjective` effect group only, so it is unreachable
## from a two-mode DV no matter what parameter it is given.  This is a
## NON-IMPLEMENTATION, distinct from a null (an estimated effect near zero) and
## from a non-identification (a model that will not estimate).
.BRIDGE_NOT_IN_BIPARTITE <- c("transTriads")

## The caveat that must accompany every matched-seed counterfactual.  Wording
## follows D:/innovation_shocks/03_STUDY_DESIGN.md section 6.3.
.BRIDGE_MATCHED_SEED_CAVEAT <- paste0(
  "Matched seeds fix INITIALISATION only. Both arms share the landscape, the W ",
  "matrix, the NK noise matrix and the initial holdings, so \"same actors, same ",
  "landscape, same start\" holds exactly. They do NOT share the realized ",
  "trajectory: after the first divergent ministep the two arms consume the RNG ",
  "stream differently, so \"same realized path\" does not hold. A single ",
  "baseline/counterfactual pair is therefore not a counterfactual; report the ",
  "delta as an average over replications with its Monte Carlo error, never as a ",
  "point counterfactual for a single run."
)


# ---------------------------------------------------------------------------- #
#  saom_to_saomnk
# ---------------------------------------------------------------------------- #

#' Convert SAOM estimated parameters to SaoMNK structure model
#'
#' Maps an RSiena theta vector to SaoMNK's bipartite effect system.
#' Both frameworks use the same actor-oriented conditional logit utility,
#' so the mapping is direct for shared effects, with sign/scale adjustments
#' for framework-specific parameterizations.
#'
#' Rate parameters are extracted but not simulated: the SaoMNK structure model
#' has no rate-function representation, so opportunity is governed by
#' \code{iterations} rather than by an estimated rate.  Any rate effect found in
#' \code{saom_result} is returned in \code{$rate_params} and announced with a
#' \code{warning()} rather than dropped silently.
#'
#' @param saom_result An RSiena \code{sienaFit} result object (from
#'   \code{siena07}), OR a named numeric vector of theta values.
#' @param scale_factor Numeric multiplier applied to all converted parameters
#'   (default \code{1.0}).  Useful for sensitivity analysis.
#' @param draw_theta Logical. If \code{FALSE} (the default, and the historical
#'   behavior) the point estimates are converted, so the counterfactual carries
#'   no parameter uncertainty.  If \code{TRUE}, one theta vector is drawn from
#'   the estimated sampling distribution
#'   \eqn{N(\hat\theta, \Sigma_\theta)}{N(thetahat, covtheta)} using
#'   \code{coef(saom_result)} and \code{saom_result$covtheta}, so that repeated
#'   calls propagate estimation error into downstream counterfactuals.  Requires
#'   a \code{sienaFit} (a bare theta vector carries no covariance matrix).
#' @param draw_seed Integer or \code{NULL} (default).  Seed used for the theta
#'   draw, so that a drawn parameter set is reproducible.  Ignored when
#'   \code{draw_theta = FALSE}.
#' @param strict Logical, default \code{TRUE}.  Several entries in the crosswalk
#'   are not translations but SUBSTANTIVE RE-INTERPRETATIONS: \code{recip} is
#'   converted into half a \code{cycle4} parameter, \code{cycle3} into
#'   \code{cycle4}, \code{simX} and \code{sameX} into \code{egoX}, \code{higher}
#'   into \code{altX}.  A counterfactual run through any of them is calibrated to
#'   a DIFFERENT model from the one that was estimated.  With
#'   \code{strict = TRUE} such an effect is refused, with a message naming each
#'   one and the effect it would become.  \code{strict = FALSE} restores the
#'   warn-and-proceed behavior; the approximations are then listed in a
#'   \code{warning()} and in \code{$approximate}, and must be reported with any
#'   result built from them.  \code{strict} does NOT relax a non-implementation:
#'   an effect whose SaoMNK counterpart does not exist for a bipartite dependent
#'   variable (see \code{.BRIDGE_NOT_IN_BIPARTITE}) is refused either way.
#' @param verbose Logical. If \code{TRUE}, prints the mapping table and
#'   diagnostics (default \code{TRUE}).
#' @return A list with components:
#'   \describe{
#'     \item{\code{effects}}{List of effect specs ready for a SaoMNK structure
#'       model (each element has \code{effect}, \code{parameter}, \code{fix},
#'       \code{dv_name}).}
#'     \item{\code{mapping_table}}{A \code{data.frame} documenting each
#'       conversion: SAOM effect name, point estimate \code{saom_theta}, the
#'       value actually converted \code{saom_theta_used} (identical to
#'       \code{saom_theta} unless \code{draw_theta = TRUE}), SE, SaoMNK effect,
#'       converted parameter, \code{mapping_status} (\code{"exact"} or
#'       \code{"approximate"}), and description.  \code{mapping_status} is what
#'       makes an approximation visible in the returned object rather than only
#'       in the prose of \code{description}.}
#'     \item{\code{rate_params}}{A \code{data.frame} of extracted RSiena rate
#'       parameters (\code{Rate}, \code{RateX}, \code{outRate},
#'       \code{outRateInv}, \code{outRateLog}).  These are NOT applied to the
#'       simulation; see the warning raised when any are present.}
#'     \item{\code{unmapped}}{Character vector of SAOM effect names that
#'       could not be mapped (and were therefore dropped).}
#'     \item{\code{approximate}}{Character vector of SAOM effect names converted
#'       through an APPROXIMATE crosswalk entry.  Empty unless
#'       \code{strict = FALSE}, since \code{strict = TRUE} refuses them.}
#'     \item{\code{strict}}{The value of \code{strict} used.}
#'     \item{\code{theta_point}}{Named numeric vector of point estimates.}
#'     \item{\code{theta_used}}{Named numeric vector of the thetas actually
#'       converted (the drawn vector when \code{draw_theta = TRUE}).}
#'     \item{\code{theta_drawn}}{Logical: was theta drawn from its sampling
#'       distribution?}
#'     \item{\code{covtheta}}{The estimated covariance matrix, or \code{NULL}.}
#'     \item{\code{scale_factor}}{The scale factor used.}
#'   }
#' @export
#' @examples
#' ## From a named vector of estimated thetas
#' thetas <- c(density = -1.2, inPop = 0.3, outAct = -0.2)
#' bridge <- saom_to_saomnk(thetas)
#'
#' ## An approximate mapping is refused by default and named in the message
#' try(saom_to_saomnk(c(density = -1.2, recip = 0.8), verbose = FALSE))
#' ## ... and accepted, with a warning, when the approximation is intended
#' bridge_loose <- saom_to_saomnk(c(density = -1.2, recip = 0.8),
#'                                strict = FALSE, verbose = FALSE)
#'
#' ## From a sienaFit object
#' \dontrun{
#' fit <- siena07(alg, data = mydata, effects = myeffects)
#' bridge <- saom_to_saomnk(fit, scale_factor = 0.5)
#'
#' ## Carry estimation uncertainty into the counterfactual: one draw per call
#' draws <- lapply(1:20, function(r) saom_to_saomnk(fit, draw_theta = TRUE,
#'                                                  draw_seed = 100 + r,
#'                                                  verbose = FALSE))
#' }
saom_to_saomnk <- function(saom_result, scale_factor = 1.0,
                           draw_theta = FALSE, draw_seed = NULL,
                           strict = TRUE, verbose = TRUE) {

  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    stop("strict must be a single TRUE or FALSE.", call. = FALSE)
  }

  ## -- Extract theta vector, SEs and effect metadata ------------------------ ##

  extracted  <- .extract_saom_theta(saom_result)
  theta_point <- extracted$theta
  se          <- extracted$se
  covtheta    <- extracted$covtheta
  eff_meta    <- extracted$meta

  ## -- Optionally draw theta from its estimated sampling distribution ------- ##

  if (isTRUE(draw_theta)) {
    thetas <- .draw_saom_theta(theta_point, covtheta, draw_seed)
  } else {
    thetas <- theta_point
  }

  ## -- Split off rate parameters (not representable in SaoMNK) -------------- ##

  is_rate     <- .is_rate_effect(names(thetas), eff_meta)
  rate_params <- .rate_param_table(thetas, theta_point, se, eff_meta, is_rate)

  ## -- SAOM -> SaoMNK mapping table ----------------------------------------- ##
  ## Each entry: saom name, saomnk shortName, status, transform, description.
  ## Sign conventions: RSiena density is negative (costly); SaoMNK density
  ## likewise.  The transform is the identity (* scale_factor) unless a
  ## rescaling is needed for the bipartite representation.
  ##
  ## `status` is the load-bearing field, and it has three values:
  ##
  ##   "exact"        the SaoMNK effect IS the estimated effect, up to
  ##                  scale_factor.  Converting it changes nothing about what
  ##                  the model says.
  ##   "approximate"  the SaoMNK effect is a DIFFERENT statistic standing in for
  ##                  the estimated one.  A counterfactual run through it is
  ##                  calibrated to a different model than the one estimated, so
  ##                  `strict = TRUE` refuses it and `strict = FALSE` warns.
  ##   "unavailable"  the SaoMNK effect does not exist for a bipartite dependent
  ##                  variable in RSiena 1.5.0, so the mapping can never be
  ##                  simulated.  Refused regardless of `strict`, and reported
  ##                  as a NON-IMPLEMENTATION rather than as a null.

  mapping <- list(
    # Structural effects
    list(saom = "density",      saomnk = "density",      status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Scope cost (negative = costly to add ties)"),
    list(saom = "outdegree",    saomnk = "density",      status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Alternative RSiena label for the same outdegree/density term"),
    list(saom = "recip",        saomnk = "cycle4",       status = "approximate",
         transform = function(x) x * 0.5 * scale_factor,
         desc = "Reciprocity -> HALF a 4-cycle parameter; a different statistic"),
    list(saom = "transTrip",    saomnk = "transTriads",  status = "unavailable",
         transform = function(x) x * scale_factor,
         desc = "Transitive triplets -> transitive triads (one-mode only)"),
    list(saom = "cycle3",       saomnk = "cycle4",       status = "approximate",
         transform = function(x) x * scale_factor,
         desc = "3-cycles -> 4-cycles; the bipartite cycle, not the same statistic"),
    list(saom = "gwespFF",      saomnk = "transTriads",  status = "unavailable",
         transform = function(x) x * scale_factor,
         desc = "GWESP (geometrically weighted) -> unweighted transitive triads"),

    # Popularity / activity effects
    list(saom = "inPop",        saomnk = "inPop",        status = "exact",
         transform = function(x) x * scale_factor,
         desc = "In-degree popularity (preferential attachment)"),
    list(saom = "inPopSqrt",    saomnk = "inPopSqrt",    status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Sqrt in-popularity; RSiena implements inPopSqrt for bipartite DVs"),
    list(saom = "outAct",       saomnk = "outAct",       status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Out-degree activity (scope expansion)"),
    list(saom = "outActSqrt",   saomnk = "outActSqrt",   status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Sqrt out-activity"),

    # Covariate effects
    list(saom = "egoX",         saomnk = "egoX",         status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Ego covariate effect"),
    list(saom = "altX",         saomnk = "altX",         status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Alter covariate effect"),
    list(saom = "simX",         saomnk = "egoX",         status = "approximate",
         transform = function(x) x * scale_factor,
         desc = "Covariate SIMILARITY re-read as an ego main effect"),
    list(saom = "sameX",        saomnk = "egoX",         status = "approximate",
         transform = function(x) x * scale_factor,
         desc = "Same-category homophily re-read as an ego main effect"),

    # Dyadic covariate effects
    list(saom = "X",            saomnk = "X",            status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Dyadic covariate effect"),
    list(saom = "XWX",          saomnk = "XWX",          status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Epistasis: XW=>X closure on the influence matrix W"),
    list(saom = "higher",       saomnk = "altX",         status = "approximate",
         transform = function(x) x * scale_factor,
         desc = "Higher-covariate ordering re-read as an alter main effect"),

    # Distance effects
    list(saom = "totInDist2",   saomnk = "totInDist2",   status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Total in-degree distance-2"),
    list(saom = "simEgoInDist2", saomnk = "simEgoInDist2", status = "exact",
         transform = function(x) x * scale_factor,
         desc = "Similar ego in-degree distance-2")
  )

  ## -- Apply mapping -------------------------------------------------------- ##

  converted   <- list()
  log_rows    <- list()
  unmapped    <- character(0)
  approx_rows <- list()   # entries whose crosswalk status is "approximate"
  unavail_rows <- list()  # entries whose SaoMNK target is not implemented

  for (i in seq_along(thetas)) {

    ## Rate parameters never enter the evaluation-effect crosswalk; they are
    ## carried in $rate_params and warned about below.
    if (is_rate[i]) next

    effect_name <- names(thetas)[i]
    theta_val   <- as.numeric(thetas[i])
    ## Strip interaction suffixes for matching (e.g., "egoX.assets" -> "egoX")
    base_name   <- sub("\\..+$", "", effect_name)

    matched <- FALSE
    for (m in mapping) {
      if (base_name == m$saom || effect_name == m$saom) {
        beta_val <- m$transform(theta_val)

        ## A mapping onto an effect RSiena does not implement for a bipartite
        ## DV is collected and refused below; it is never converted, because a
        ## converted value would imply the effect could be simulated.
        if (identical(m$status, "unavailable")) {
          unavail_rows[[length(unavail_rows) + 1]] <- list(
            saom = effect_name, saomnk = m$saomnk, theta = theta_val,
            desc = m$desc)
          matched <- TRUE
          break
        }
        if (identical(m$status, "approximate")) {
          approx_rows[[length(approx_rows) + 1]] <- list(
            saom = effect_name, saomnk = m$saomnk, theta = theta_val,
            beta = beta_val, desc = m$desc)
        }

        converted[[length(converted) + 1]] <- list(
          effect       = m$saomnk,
          parameter    = beta_val,
          fix          = TRUE,
          dv_name      = .BRIDGE_DV_NAME,
          source_effect = effect_name,
          source_theta  = theta_val,
          mapping_status = m$status
        )
        log_rows[[length(log_rows) + 1]] <- data.frame(
          saom_effect      = effect_name,
          saom_theta       = as.numeric(theta_point[i]),
          saom_theta_used  = theta_val,
          saom_se          = se[i],
          saomnk_effect    = m$saomnk,
          saomnk_parameter = beta_val,
          mapping_status   = m$status,
          description      = m$desc,
          stringsAsFactors = FALSE
        )
        matched <- TRUE
        break
      }
    }
    if (!matched) {
      unmapped <- c(unmapped, effect_name)
      if (verbose) {
        cat(sprintf("  [unmapped] %s (theta = %.4f)\n", effect_name, theta_val))
      }
    }
  }

  ## -- Refuse non-implementations, unconditionally -------------------------- ##
  ##
  ## Three outcomes must be kept apart: a NULL is an estimated effect near zero,
  ## a NON-IDENTIFICATION is a model that will not estimate, and a
  ## NON-IMPLEMENTATION is an effect the software does not offer at all.  Only
  ## the last applies here, and it says nothing about the world; silently
  ## converting the parameter anyway would let it be read as one of the first
  ## two.  `strict` does not relax this: no argument can make RSiena implement
  ## the effect.

  if (length(unavail_rows) > 0) {
    lines <- vapply(unavail_rows, function(u)
      sprintf("  - %s (theta = %.4f) -> %s : %s",
              u$saom, u$theta, u$saomnk, u$desc),
      character(1))
    stop(sprintf(
      paste0("saom_to_saomnk(): %d estimated effect(s) map onto a SaoMNK effect that ",
             "RSiena does not implement for a BIPARTITE dependent variable, which is ",
             "the only kind SaoMNK simulates:\n%s\n",
             "This is a NON-IMPLEMENTATION, not a null and not a non-identification: ",
             "RSiena 1.5.0 offers %s in the symmetricObjective effect group only ",
             "(verified with getEffects() on a bipartite sienaData object), so it can ",
             "never enter a SaoMNK structure model and no parameter value would make ",
             "it act. Report it as unavailable; do not report it as a zero effect. ",
             "strict = FALSE does not relax this. Drop the effect from the SAOM ",
             "before bridging, or re-specify the closure hypothesis on a statistic ",
             "the bipartite DV has (cycle4)."),
      length(unavail_rows), paste(lines, collapse = "\n"),
      paste(unique(vapply(unavail_rows, function(u) u$saomnk, character(1))),
            collapse = ", ")),
      call. = FALSE)
  }

  ## -- Refuse (or warn about) approximate mappings -------------------------- ##

  approximate <- vapply(approx_rows, function(a) a$saom, character(1))

  if (length(approx_rows) > 0) {
    lines <- vapply(approx_rows, function(a)
      sprintf("  - %s (theta = %.4f) -> %s = %.4f : %s",
              a$saom, a$theta, a$saomnk, a$beta, a$desc),
      character(1))
    if (isTRUE(strict)) {
      stop(sprintf(
        paste0("saom_to_saomnk(strict = TRUE): %d estimated effect(s) have only an ",
               "APPROXIMATE SaoMNK counterpart. Each is a different statistic standing ",
               "in for the estimated one, so a counterfactual converted through it is ",
               "calibrated to a DIFFERENT model from the one that was estimated:\n%s\n",
               "Refusing to convert. Either drop these effects from the SAOM before ",
               "bridging, or call saom_to_saomnk(..., strict = FALSE) to accept the ",
               "approximations -- in which case they are returned in $approximate and ",
               "must be reported with any result built from them."),
        length(approx_rows), paste(lines, collapse = "\n")),
        call. = FALSE)
    }
    warning(sprintf(
      paste0("saom_to_saomnk(strict = FALSE): %d estimated effect(s) were converted ",
             "through an APPROXIMATE crosswalk entry, i.e. onto a different statistic ",
             "from the one estimated:\n%s\n",
             "The converted model is therefore not the estimated model. Report the ",
             "approximation with any result built from it; it is listed in ",
             "$approximate and flagged in $mapping_table$mapping_status."),
      length(approx_rows), paste(lines, collapse = "\n")),
      call. = FALSE)
  }

  mapping_table <- if (length(log_rows) > 0) {
    do.call(rbind, log_rows)
  } else {
    data.frame(
      saom_effect = character(0), saom_theta = numeric(0),
      saom_theta_used = numeric(0), saom_se = numeric(0),
      saomnk_effect = character(0), saomnk_parameter = numeric(0),
      mapping_status = character(0), description = character(0),
      stringsAsFactors = FALSE
    )
  }
  rownames(mapping_table) <- NULL

  n_eval <- sum(!is_rate)

  if (verbose) {
    cat(sprintf("\n=== SAOM -> SaoMNK Parameter Bridge ===\n"))
    cat(sprintf("Mapped: %d / %d evaluation effects (%.0f%%)\n",
                nrow(mapping_table), n_eval,
                100 * nrow(mapping_table) / max(n_eval, 1)))
    cat(sprintf("Unmapped: %s\n",
                if (length(unmapped)) paste(unmapped, collapse = ", ") else "none"))
    cat(sprintf("Rate parameters extracted (NOT simulated): %s\n",
                if (nrow(rate_params)) paste(rate_params$saom_effect, collapse = ", ") else "none"))
    cat(sprintf("Theta source: %s\n",
                if (isTRUE(draw_theta)) "one draw from N(thetahat, covtheta)" else "point estimates"))
    cat(sprintf("Scale factor: %.2f\n", scale_factor))
    cat(sprintf("strict = %s; approximate mappings used: %s\n\n",
                strict,
                if (length(approximate)) paste(approximate, collapse = ", ") else "none"))
    print(mapping_table[, c("saom_effect", "saom_theta", "saom_theta_used",
                            "saomnk_effect", "saomnk_parameter", "mapping_status")])
    if (nrow(rate_params)) {
      cat("\nRate parameters (carried, not applied):\n")
      print(rate_params[, c("saom_effect", "saom_shortName", "saom_theta", "saom_se")])
    }
  }

  ## -- Loud reporting of everything that does NOT reach the simulation ------ ##

  if (nrow(rate_params) > 0) {
    warning(sprintf(
      paste0("saom_to_saomnk(): %d rate parameter(s) were extracted but CANNOT be ",
             "mapped into the SaoMNK structure model, which has no rate-function ",
             "representation: %s. They are returned in $rate_params and are NOT ",
             "applied; opportunity in the simulation is governed by `iterations`. ",
             "Any counterfactual built from this object therefore holds the rate ",
             "function at the SaoMNK default rather than at the estimated values."),
      nrow(rate_params), paste(rate_params$saom_effect, collapse = ", ")),
      call. = FALSE)
  }

  if (length(unmapped) > 0) {
    warning(sprintf(
      paste0("saom_to_saomnk(): %d estimated effect(s) have no SaoMNK counterpart ",
             "and were DROPPED: %s. The converted structure model is therefore not ",
             "the estimated model; report the omission with any result built from it."),
      length(unmapped), paste(unmapped, collapse = ", ")),
      call. = FALSE)
  }

  list(
    effects       = converted,
    mapping_table = mapping_table,
    rate_params   = rate_params,
    unmapped      = unmapped,
    approximate   = approximate,
    strict        = isTRUE(strict),
    theta_point   = theta_point,
    theta_used    = thetas,
    theta_drawn   = isTRUE(draw_theta),
    covtheta      = covtheta,
    scale_factor  = scale_factor
  )
}


# ---------------------------------------------------------------------------- #
#  saom_to_saomnk internal helpers
# ---------------------------------------------------------------------------- #

#' Extract theta, standard errors and effect metadata from a SAOM result
#'
#' @param saom_result A \code{sienaFit} object or a named numeric vector.
#' @return A list with \code{theta} (named numeric), \code{se} (numeric),
#'   \code{covtheta} (matrix or \code{NULL}) and \code{meta} (a
#'   \code{data.frame} of RSiena effect metadata, or \code{NULL} when it cannot
#'   be aligned with theta).
#' @keywords internal
.extract_saom_theta <- function(saom_result) {

  if (inherits(saom_result, "sienaFit")) {

    ## RSiena defines no `coef.sienaFit`, so `coef()` dispatches to
    ## `stats::coef.default()`, which reads `object$coefficients` -- an element a
    ## sienaFit does not have. It therefore returned NULL for every real fit and
    ## the whole branch was dead. The estimates live in `$theta`; see the Value
    ## section of ?siena07, which names theta, covtheta and se.
    thetas <- if (!is.null(saom_result$theta)) {
      as.numeric(saom_result$theta)
    } else {
      as.numeric(coef(saom_result))
    }
    if (!length(thetas))
      stop("could not read estimates from the sienaFit: neither `$theta` nor ",
           "coef() returned anything. This is not a model result, it is a ",
           "read failure.", call. = FALSE)

    eff_df <- if (!is.null(saom_result$effects) &&
                  !is.null(saom_result$effects$effectName)) {
      saom_result$effects
    } else if (!is.null(saom_result$requestedEffects) &&
               !is.null(saom_result$requestedEffects$effectName)) {
      saom_result$requestedEffects
    } else {
      stop("Cannot find effectName in sienaFit object (checked $effects and $requestedEffects)",
           call. = FALSE)
    }

    ## Align the effect rows with the estimated parameters.  RSiena normally
    ## returns one row per estimated parameter; if it does not, fall back to
    ## name-based classification rather than mis-labeling rate effects.
    if (nrow(eff_df) != length(thetas) && !is.null(eff_df$include)) {
      eff_df <- eff_df[which(as.logical(eff_df$include)), , drop = FALSE]
    }
    if (nrow(eff_df) != length(thetas)) {
      warning(sprintf(
        paste0("saom_to_saomnk(): sienaFit has %d effect rows but %d estimated ",
               "parameters; effect metadata (including the rate-parameter flag) ",
               "cannot be aligned and will be inferred from effect names only."),
        nrow(eff_df), length(thetas)), call. = FALSE)
      if (is.null(names(thetas))) {
        names(thetas) <- paste0("theta", seq_along(thetas))
      }
      eff_df <- NULL
    } else {
      ## Name theta by shortName, NOT effectName.
      ##
      ## The crosswalk in saom_to_saomnk() keys on RSiena shortNames
      ## ("density", "transTrip", "gwespFF", "egoX"), and the function's own
      ## @examples pass shortName-keyed vectors. Naming theta with RSiena's long
      ## effectName ("outdegree (density)", "transitive triplets") meant a real
      ## sienaFit could never match a single crosswalk entry: every evaluation
      ## effect fell through to $unmapped and "0 of N mapped" was reported as a
      ## modeling fact rather than the naming failure it was.
      ##
      ## `interaction1` is appended as a `.suffix` so that several instances of
      ## one effect (an egoX per covariate) stay distinguishable; the crosswalk
      ## already strips anything after the first dot.
      sn <- if (!is.null(eff_df$shortName)) as.character(eff_df$shortName)
            else as.character(eff_df$effectName)
      i1 <- if (!is.null(eff_df$interaction1)) as.character(eff_df$interaction1)
            else rep("", length(sn))
      i1[is.na(i1)] <- ""
      nm <- ifelse(nzchar(i1), paste0(sn, ".", i1), sn)
      ## Keep duplicates distinguishable (e.g. one Rate per period).
      if (anyDuplicated(nm)) nm <- make.unique(nm, sep = ".")
      names(thetas) <- nm
    }

    covtheta <- saom_result$covtheta
    ## suppressWarnings(): a non-PD covtheta yields NaN SEs, which is reported
    ## by .draw_saom_theta() rather than as a bare "NaNs produced" warning here.
    se <- tryCatch(
      suppressWarnings(sqrt(diag(as.matrix(covtheta)))),
      error = function(e) rep(NA_real_, length(thetas))
    )
    if (length(se) != length(thetas)) se <- rep(NA_real_, length(thetas))

    meta <- if (is.null(eff_df)) NULL else data.frame(
      effect_name  = as.character(eff_df$effectName),
      shortName    = if (!is.null(eff_df$shortName)) as.character(eff_df$shortName) else NA_character_,
      type         = if (!is.null(eff_df$type)) as.character(eff_df$type) else NA_character_,
      period       = if (!is.null(eff_df$period)) as.character(eff_df$period) else NA_character_,
      interaction1 = if (!is.null(eff_df$interaction1)) as.character(eff_df$interaction1) else NA_character_,
      stringsAsFactors = FALSE
    )

  } else if (is.numeric(saom_result) && !is.null(names(saom_result))) {

    thetas   <- saom_result
    se       <- rep(NA_real_, length(thetas))
    covtheta <- NULL
    meta     <- NULL

  } else {
    stop("saom_result must be a sienaFit object or a named numeric vector",
         call. = FALSE)
  }

  list(theta = thetas, se = se, covtheta = covtheta, meta = meta)
}


#' Identify RSiena rate-function effects
#'
#' Rate effects are recognized from RSiena metadata when it is available
#' (\code{type == "rate"}, or a rate \code{shortName}), and otherwise from the
#' effect name.
#'
#' @param effect_names Character vector of effect names.
#' @param meta Optional effect metadata from \code{.extract_saom_theta}.
#' @return Logical vector, one element per effect.
#' @keywords internal
.is_rate_effect <- function(effect_names, meta = NULL) {

  flag <- rep(FALSE, length(effect_names))

  ## Metadata WINS when it is present and aligned. The name heuristic is the
  ## fallback for a bare theta vector, not a supplement.
  ##
  ## An earlier version OR-ed the heuristic in unconditionally, so an effect
  ## whose metadata says type == "eval", shortName == "egoX" was still
  ## classified as a rate parameter whenever its name contained the word
  ## "rate". That is not contrived: a monadic covariate called `rate` (growth
  ## rate, churn rate, interest rate) gets the RSiena effectName "rate ego",
  ## and the estimated homophily effect was then excluded from the crosswalk
  ## and silently dropped from every counterfactual.
  meta_aligned <- !is.null(meta) && nrow(meta) == length(effect_names) &&
    (!is.null(meta$type) || !is.null(meta$shortName))

  if (meta_aligned) {
    if (!is.null(meta$type))
      flag <- flag | (!is.na(meta$type) & meta$type == "rate")
    if (!is.null(meta$shortName))
      flag <- flag | (!is.na(meta$shortName) &
                        meta$shortName %in% .BRIDGE_RATE_SHORTNAMES)
    return(flag)
  }

  key      <- tolower(trimws(ifelse(is.na(effect_names), "", effect_names)))
  base_key <- sub("\\..+$", "", key)

  base_key %in% tolower(.BRIDGE_RATE_SHORTNAMES) | grepl("\\brate\\b", key)
}


#' Assemble the table of extracted rate parameters
#'
#' @param thetas Named numeric vector of thetas actually used.
#' @param theta_point Named numeric vector of point estimates.
#' @param se Numeric vector of standard errors.
#' @param meta Optional effect metadata.
#' @param is_rate Logical vector flagging rate effects.
#' @return A \code{data.frame} with one row per rate parameter (possibly zero).
#' @keywords internal
.rate_param_table <- function(thetas, theta_point, se, meta, is_rate) {

  idx <- which(is_rate)

  empty <- data.frame(
    saom_effect      = character(0), saom_shortName = character(0),
    saom_type        = character(0), saom_period    = character(0),
    saom_interaction1 = character(0),
    saom_theta       = numeric(0),   saom_theta_used = numeric(0),
    saom_se          = numeric(0),   note            = character(0),
    stringsAsFactors = FALSE
  )
  if (length(idx) == 0) return(empty)

  has_meta <- !is.null(meta) && nrow(meta) == length(thetas)

  out <- data.frame(
    saom_effect       = names(thetas)[idx],
    saom_shortName    = if (has_meta) meta$shortName[idx]    else NA_character_,
    saom_type         = if (has_meta) meta$type[idx]         else NA_character_,
    saom_period       = if (has_meta) meta$period[idx]       else NA_character_,
    saom_interaction1 = if (has_meta) meta$interaction1[idx] else NA_character_,
    saom_theta        = as.numeric(theta_point[idx]),
    saom_theta_used   = as.numeric(thetas[idx]),
    saom_se           = as.numeric(se[idx]),
    note              = "extracted; NOT applied to the SaoMNK simulation",
    stringsAsFactors  = FALSE
  )
  rownames(out) <- NULL
  out
}


#' Draw one theta vector from its estimated sampling distribution
#'
#' Draws \eqn{\theta \sim N(\hat\theta, \Sigma_\theta)}{theta ~ N(thetahat,
#' covtheta)} by eigen-decomposition of \code{covtheta}.  An all-\code{NA}
#' covariance matrix means the fit did not identify, and is an error rather
#' than something to proceed through.
#'
#' @param theta Named numeric vector of point estimates.
#' @param covtheta Estimated covariance matrix, or \code{NULL}.
#' @param draw_seed Integer seed, or \code{NULL}.
#' @return A named numeric vector of the same length as \code{theta}.
#' @keywords internal
.draw_saom_theta <- function(theta, covtheta, draw_seed = NULL) {

  p <- length(theta)

  if (is.null(covtheta)) {
    stop(paste0("draw_theta = TRUE requires the estimated covariance matrix of theta, ",
                "but none is available. Pass a sienaFit object (which carries ",
                "$covtheta) rather than a bare named theta vector."),
         call. = FALSE)
  }

  covtheta <- as.matrix(covtheta)

  if (all(is.na(covtheta))) {
    stop(paste0("The estimated covariance matrix of theta is entirely NA: the fit ",
                "did not identify. Refusing to draw parameter uncertainty from an ",
                "undefined sampling distribution. Re-estimate the SAOM, or call ",
                "saom_to_saomnk() with draw_theta = FALSE and report that the ",
                "counterfactual carries no parameter uncertainty."),
         call. = FALSE)
  }

  if (nrow(covtheta) != p || ncol(covtheta) != p) {
    stop(sprintf(paste0("covtheta is %d x %d but theta has length %d; the covariance ",
                        "matrix cannot be matched to the parameter vector."),
                 nrow(covtheta), ncol(covtheta), p), call. = FALSE)
  }

  if (any(is.na(covtheta))) {
    bad <- names(theta)[apply(is.na(covtheta), 1, any)]
    stop(sprintf(paste0("covtheta has NA entries for %d effect(s): %s. These ",
                        "parameters were not identified by the fit, so theta cannot ",
                        "be drawn. Drop them from the model or use draw_theta = FALSE."),
                 length(bad), paste(bad, collapse = ", ")), call. = FALSE)
  }

  ## Symmetrise (RSiena's covtheta can be asymmetric at the last digits) and
  ## check the spectrum before using it as a covariance.
  covtheta <- (covtheta + t(covtheta)) / 2
  ev  <- eigen(covtheta, symmetric = TRUE)
  tol <- 1e-8 * max(1, max(abs(ev$values)))

  if (min(ev$values) < -tol) {
    stop(sprintf(paste0("covtheta is not positive semi-definite (smallest eigenvalue ",
                        "%.3g): it is not a valid covariance matrix, so theta cannot ",
                        "be drawn from it. Check the SAOM convergence before ",
                        "propagating parameter uncertainty."),
                 min(ev$values)), call. = FALSE)
  }

  if (any(ev$values < tol)) {
    warning(sprintf(paste0("covtheta is numerically singular (%d of %d eigenvalues ",
                           "below %.3g); those directions are drawn as fixed. The ",
                           "drawn theta understates uncertainty along them."),
                    sum(ev$values < tol), p, tol), call. = FALSE)
  }

  vals <- pmax(ev$values, 0)

  if (!is.null(draw_seed)) set.seed(as.integer(draw_seed))
  z <- rnorm(p)

  drawn <- as.numeric(theta) + as.numeric(ev$vectors %*% (sqrt(vals) * z))
  names(drawn) <- names(theta)
  drawn
}


# ---------------------------------------------------------------------------- #
#  empirical_to_saomnk_env
# ---------------------------------------------------------------------------- #

#' Create SaoMNK environment from empirical MI data
#'
#' Converts a one-mode firm-firm supply chain network into a bipartite
#' firm x SIC-category representation suitable for SaoMNK simulation.
#' The bipartite matrix encodes which SIC categories each firm participates
#' in, both directly and through supply-chain partnerships.
#'
#' @param mi_data MI data object (typically loaded from an RDS file).
#'   Expected structure: \code{mi_data$imputations[[imp]][[wave]]} containing
#'   \code{$network} (MxM adjacency matrix) and \code{$covariates} (named list
#'   with at least \code{$sic}).
#' @param wave Integer wave number to use (default \code{1}).
#' @param imputation Integer imputation number (default \code{1}).
#' @param min_firms_per_sic Minimum number of firms required in a SIC category
#'   for that category to be included (default \code{2}).
#' @return A list with components:
#'   \describe{
#'     \item{\code{env}}{A \code{SaomNkRSienaBiEnv} object with the bipartite
#'       matrix set to the empirical data.}
#'     \item{\code{W}}{An \eqn{N \times N}{N x N} influence matrix estimated from
#'       SIC co-occurrence in supply chains (normalized to [0,1]).}
#'     \item{\code{actor_attrs}}{A \code{data.frame} of actor (firm) covariates.}
#'     \item{\code{component_attrs}}{A \code{data.frame} of component (SIC
#'       category) attributes.}
#'     \item{\code{bi_matrix}}{The MxN bipartite matrix.}
#'     \item{\code{sic_mapping}}{Named integer vector mapping SIC codes to
#'       column indices.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' mi_data <- readRDS("path/to/mi_data.rds")
#' bridge_env <- empirical_to_saomnk_env(mi_data, wave = 3)
#' bridge_env$env  # the SaomNkRSienaBiEnv object
#' bridge_env$W    # influence matrix (estimated from co-occurrence)
#' }
empirical_to_saomnk_env <- function(mi_data, wave = 1, imputation = 1,
                                     min_firms_per_sic = 2) {

  ## -- Validate inputs ------------------------------------------------------ ##

  stopifnot(is.list(mi_data), !is.null(mi_data$imputations))
  if (imputation > length(mi_data$imputations)) {
    stop(sprintf("Imputation %d requested but only %d available.",
                 imputation, length(mi_data$imputations)), call. = FALSE)
  }
  imp_data <- mi_data$imputations[[imputation]]
  if (wave > length(imp_data)) {
    stop(sprintf("Wave %d requested but only %d available.",
                 wave, length(imp_data)), call. = FALSE)
  }

  wave_data <- imp_data[[wave]]
  net       <- wave_data$network
  sic_codes <- wave_data$covariates$sic

  stopifnot(is.matrix(net), !is.null(sic_codes))

  M <- nrow(net)  # number of firms

  ## -- Filter SIC categories ------------------------------------------------ ##

  sic_counts <- table(sic_codes)
  valid_sics <- sort(names(sic_counts[sic_counts >= min_firms_per_sic]))
  N          <- length(valid_sics)

  if (N == 0) {
    stop(sprintf("No SIC categories have >= %d firms. Lower min_firms_per_sic.",
                 min_firms_per_sic), call. = FALSE)
  }

  cat(sprintf("Empirical environment: M=%d firms, N=%d SIC categories (from %d unique)\n",
              M, N, length(unique(sic_codes))))

  ## -- Build bipartite matrix ----------------------------------------------- ##
  ## firm i -> SIC j if firm i's own SIC == j

  bi_matrix <- matrix(0L, nrow = M, ncol = N)
  for (i in seq_len(M)) {
    sic_idx <- which(valid_sics == sic_codes[i])
    if (length(sic_idx) > 0) {
      bi_matrix[i, sic_idx] <- 1L
    }
  }

  ## Add secondary ties: if firm i supplies to firm j in SIC k, firm i gets
  ## a tie to SIC k (captures the scope of supply-chain reach).
  for (i in seq_len(M)) {
    partners <- which(net[i, ] > 0)
    for (j in partners) {
      partner_sic_idx <- which(valid_sics == sic_codes[j])
      if (length(partner_sic_idx) > 0) {
        bi_matrix[i, partner_sic_idx] <- 1L
      }
    }
  }

  bi_prob <- sum(bi_matrix) / (M * N)

  ## -- Build influence matrix W from SIC co-occurrence ---------------------- ##
  ## W[j,k] = frequency that SIC j and SIC k appear in the same firm's
  ## bipartite row.  This captures empirical component complementarity.

  W <- matrix(0, nrow = N, ncol = N)
  for (i in seq_len(M)) {
    active_sics <- which(bi_matrix[i, ] > 0)
    if (length(active_sics) > 1) {
      for (a in seq_along(active_sics)) {
        for (b in seq_along(active_sics)) {
          if (a != b) {
            W[active_sics[a], active_sics[b]] <- W[active_sics[a], active_sics[b]] + 1
          }
        }
      }
    }
  }
  ## Normalize W to [0, 1]
  if (max(W) > 0) W <- W / max(W)
  diag(W) <- 1

  ## -- Create SaoMNK environment -------------------------------------------- ##

  env <- SaomNkRSienaBiEnv$new(list(
    M         = M,
    N         = N,
    BI_PROB   = bi_prob,
    rand_seed = 42L,
    name      = sprintf("empirical_wave%d", wave)
  ))

  ## Override random initial matrix with empirical data
  env$bipartite_matrix      <- bi_matrix
  env$bipartite_matrix_init <- bi_matrix

  ## -- Actor attributes ----------------------------------------------------- ##

  actor_attrs <- data.frame(
    id  = seq_len(M),
    sic = sic_codes,
    stringsAsFactors = FALSE
  )
  if (!is.null(wave_data$covariates$is_seed)) {
    actor_attrs$is_seed <- wave_data$covariates$is_seed
  }
  ## Add financial covariates if available
  fin_covs <- c("assets", "revenue", "hhi", "profitability", "quickratio")
  for (cov_name in fin_covs) {
    if (!is.null(wave_data$covariates[[cov_name]])) {
      actor_attrs[[cov_name]] <- wave_data$covariates[[cov_name]]
    }
  }

  ## -- Component attributes ------------------------------------------------- ##

  component_attrs <- data.frame(
    id       = seq_len(N),
    sic_code = valid_sics,
    n_firms  = as.integer(sic_counts[valid_sics]),
    stringsAsFactors = FALSE
  )

  cat(sprintf("Bipartite density: %.3f, Influence matrix density: %.3f\n",
              bi_prob, sum(W > 0) / (N * N)))
  cat(sprintf("Actor covariates: %s\n", paste(names(actor_attrs), collapse = ", ")))

  list(
    env             = env,
    W               = W,
    actor_attrs     = actor_attrs,
    component_attrs = component_attrs,
    bi_matrix       = bi_matrix,
    sic_mapping     = setNames(seq_len(N), valid_sics)
  )
}


# ---------------------------------------------------------------------------- #
#  run_calibrated_counterfactual
# ---------------------------------------------------------------------------- #

#' Run a calibrated counterfactual simulation
#'
#' Takes a calibrated environment (from \code{\link{empirical_to_saomnk_env}})
#' and parameter bridge (from \code{\link{saom_to_saomnk}}), and runs a matched
#' pair of simulations -- baseline and counterfactual -- \code{n_reps} times.
#' Within each replication BOTH arms are constructed and run at the same seed,
#' so the only difference between them is the parameter modification.  The four
#' K-degree measures are differenced within each replication and then averaged
#' across replications with a Monte Carlo standard error, a paired t-test and a
#' paired quantile interval.  Per-actor and per-component deltas are returned as
#' well, so heterogeneous effects can be computed downstream.
#'
#' @param bridge_env Output from \code{\link{empirical_to_saomnk_env}}.
#' @param bridge_params Output from \code{\link{saom_to_saomnk}}.
#' @param scenario Named list describing the counterfactual modification:
#'   \describe{
#'     \item{\code{name}}{Character. Scenario label for output.}
#'     \item{\code{modify}}{Named list of \code{effect -> multiplier}
#'       overrides.  For example, \code{list(cycle4 = 2.0)} doubles the
#'       bipartite closure parameter.  The keys are SaoMNK-side effect names --
#'       what \code{\link{saom_to_saomnk}} produced -- and a key that matches no
#'       baseline effect is a hard error, not a silent no-op.}
#'     \item{\code{description}}{Optional character description.}
#'   }
#' @param iterations Integer. Number of RSiena simulation iterations per wave
#'   (default \code{50}).
#' @param n_reps Integer. Number of Monte Carlo replications (default
#'   \code{10}).  Replication \code{r} uses seed \code{seed + r - 1} for BOTH
#'   arms, so the two arms are matched within a replication and independent
#'   across replications.
#' @param seed Integer. Base random seed (default \code{42}).
#' @param conf_level Numeric confidence level for the paired interval across
#'   replications (default \code{0.95}).
#' @param verbose Logical. Print per-replication progress and the summary
#'   (default \code{TRUE}).
#' @section Matched seeds fix initialisation, not the path:
#' Within a replication both arms are constructed and run at the same seed, so
#' the landscape, the W matrix, the NK noise matrix and the initial holdings are
#' bit-identical across arms.  This fixes INITIALISATION only.  It does not fix
#' the realized trajectory: after the first divergent ministep the two arms
#' consume the RNG stream differently, so \dQuote{same actors, same landscape,
#' same start} holds exactly while \dQuote{same realized path} does not.  A
#' single baseline/counterfactual pair is therefore not a counterfactual; the
#' delta must be reported as an average over replications with its Monte Carlo
#' error.  (This mirrors the disclosure in
#' \code{03_STUDY_DESIGN.md} section 6.3 for \code{run_counterfactual_pair()}.)
#' @return A list with components:
#'   \describe{
#'     \item{\code{scenario}}{The scenario specification.}
#'     \item{\code{baseline}}{List with \code{env} and \code{k4} summary for the
#'       FIRST replication, plus \code{k4_reps} (one summary per replication)
#'       and \code{envs}.}
#'     \item{\code{counterfactual}}{The same, for the counterfactual arm.}
#'     \item{\code{comparison}}{Named list of delta values for each K-degree
#'       measure (\code{delta_K_AC}, \code{delta_K_CA}, \code{delta_K_AA},
#'       \code{delta_K_CC}).  Each is now the MEAN delta over replications; with
#'       \code{n_reps = 1} it is the single-run delta as before.}
#'     \item{\code{rep_deltas}}{Tidy \code{data.frame}, one row per replication
#'       x measure: \code{rep}, \code{seed}, \code{measure}, \code{baseline},
#'       \code{counterfactual}, \code{delta}.}
#'     \item{\code{summary}}{\code{data.frame}, one row per measure:
#'       \code{mean_delta}, \code{sd_delta}, \code{mc_se}, paired t-test
#'       (\code{t_stat}, \code{df}, \code{p_value}, \code{ci_lower},
#'       \code{ci_upper}) and the paired quantile interval (\code{q_lower},
#'       \code{q_upper}).}
#'     \item{\code{actor_deltas}}{Tidy \code{data.frame} of per-actor deltas:
#'       \code{rep}, \code{seed}, \code{actor}, \code{actor_id}, arm values and
#'       deltas for \code{K_AC} and \code{K_AA}.  Use this for actor-level
#'       heterogeneous effects.}
#'     \item{\code{component_deltas}}{The same, per component, for \code{K_CA}
#'       and \code{K_CC}.}
#'     \item{\code{seeds}}{Integer vector of the replication seeds used.}
#'     \item{\code{influence}}{What happened to the influence matrix W: the
#'       RSiena slot it was registered into, the \code{XWX} weight actually
#'       used, and \code{weight_source} -- \code{"fitted"} when the SAOM
#'       estimated an \code{XWX} term, otherwise a weight of \code{0} with the
#'       reason stated.  A zero weight means epistasis is switched OFF in both
#'       arms; no value is invented to avoid that.}
#'     \item{\code{caveat}}{The matched-seed caveat, for reporting.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' mi_data <- readRDS("path/to/mi_data.rds")
#' bridge_env <- empirical_to_saomnk_env(mi_data, wave = 3)
#'
#' thetas <- c(density = -1.2, cycle3 = 0.8, inPop = 0.3)
#' ## cycle3 -> cycle4 is an APPROXIMATE mapping, so it must be opted into
#' bridge_params <- saom_to_saomnk(thetas, strict = FALSE)
#'
#' result <- run_calibrated_counterfactual(
#'   bridge_env, bridge_params,
#'   scenario = get_orm_scenarios()$double_closure,
#'   n_reps = 20
#' )
#' result$comparison    # mean delta K values over replications
#' result$summary       # with Monte Carlo error and paired t-tests
#' result$actor_deltas  # per-actor heterogeneity
#' }
run_calibrated_counterfactual <- function(bridge_env, bridge_params,
                                          scenario = list(
                                            name   = "double_popularity",
                                            modify = list(inPop = 2.0)
                                          ),
                                          iterations = 50,
                                          n_reps = 10,
                                          seed = 42,
                                          conf_level = 0.95,
                                          verbose = TRUE) {

  stopifnot(is.list(bridge_env), !is.null(bridge_env$env))
  stopifnot(is.list(bridge_params), !is.null(bridge_params$effects))
  stopifnot(is.list(scenario), !is.null(scenario$name), !is.null(scenario$modify))

  n_reps <- as.integer(n_reps)
  if (is.na(n_reps) || n_reps < 1L) {
    stop("n_reps must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be a number strictly between 0 and 1.", call. = FALSE)
  }

  env     <- bridge_env$env
  W       <- bridge_env$W
  effects <- bridge_params$effects

  ## -- Drop covariate-dependent effects the bridge cannot supply ------------ ##
  ## The crosswalk produces the effect NAME but not the covariate the effect was
  ## estimated on, and this function registers no monadic or actor-component
  ## covariate.  Left in place, `egoX`/`altX`/`X` are skipped by the engine with
  ## a warning (inert, so the "calibrated" model silently omits them) and
  ## `totInDist2`/`simEgoInDist2` dereference a covariate that is not there and
  ## kill the run.  Dropping them here is the same disclosure discipline as
  ## $unmapped in saom_to_saomnk(): loud, named, and stated as a cost.

  needs_cov <- vapply(effects, function(e) {
    e$effect %in% .BRIDGE_COVARIATE_DEPENDENT &&
      (is.null(e$interaction1) || !nzchar(as.character(e$interaction1)))
  }, logical(1))

  if (any(needs_cov)) {
    dropped <- vapply(effects[needs_cov], function(e) {
      sprintf("%s (from %s, parameter %.4f)", e$effect,
              if (is.null(e$source_effect)) "?" else e$source_effect,
              e$parameter)
    }, character(1))
    warning(sprintf(
      paste0("run_calibrated_counterfactual(): %d converted effect(s) are identified ",
             "by a registered covariate that this bridge does not supply, and were ",
             "DROPPED from both arms: %s. The bridge registers only the influence ",
             "matrix W; monadic and actor-component covariates must be added to the ",
             "structure model by hand. Both arms are therefore run WITHOUT these ",
             "effects, so the baseline is not the estimated model; report the ",
             "omission with the result."),
      sum(needs_cov), paste(dropped, collapse = ", ")), call. = FALSE)
    effects <- effects[!needs_cov]
  }

  ## -- Register the influence matrix W, and the effect that reads it -------- ##
  ## DEFECT 1: the covariate entry previously carried neither `nodeSet` nor
  ## `effect`.  Without `effect` the engine walks the coDyadCovars list into
  ## `include_rsiena_effect_from_eff_list()`, whose first test is
  ## `if (eff$effect == 'Rate')`, and the run dies with "argument is of length
  ## zero" far from the cause; without `nodeSet` the N x N shape is ambiguous
  ## whenever M == N.  The entry now matches the shape `saomnk_model()` emits
  ## (R/saomnk-api.R): effect, parameter, dv_name, fix, nodeSet, interaction1, x.
  ##
  ## DEFECT 2: registering W is not enough.  No effect referenced it, so the
  ## matrix sat in the RSiena data object contributing nothing, and
  ## get_orm_scenarios()$epistasis_boost -- which multiplies `XWX` -- could never
  ## act.  An XWX effect pointed at the same slot is added below.

  W_ok <- is.matrix(W) && is.numeric(W) &&
    nrow(W) == env$N && ncol(W) == env$N
  if (!is.null(W) && !W_ok) {
    stop(sprintf(paste0("bridge_env$W must be a numeric %d x %d matrix (one row and ",
                        "column per component); got %s. The influence matrix cannot be ",
                        "registered, so no epistasis effect can be simulated."),
                 env$N, env$N,
                 if (is.null(dim(W))) sprintf("an object of class %s", class(W)[1])
                 else paste(dim(W), collapse = " x ")),
         call. = FALSE)
  }

  ## The XWX weight is FITTED if the SAOM estimated one, and 0 otherwise.  Zero
  ## is used rather than an invented default, and is reported in $influence and
  ## in the printed output; a zero-weight W is registered but inert.
  ##
  ## The XWX effect lives in the coDyadCovars entry, not in `effects`: that entry
  ## is BOTH the covariate registration and the effect declaration (the engine
  ## walks coDyadCovars into `include_rsiena_effect_from_eff_list()` at
  ## R/saomnk-class.R:638), and it is the shape `saomnk_model()` emits.  Adding a
  ## second entry in `effects` would register the same slot twice.
  xwx_idx <- which(vapply(effects, function(e)
    identical(e$effect, .BRIDGE_W_EFFECT), logical(1)))

  if (!W_ok) {
    xwx_weight <- NA_real_
    xwx_source <- "none: bridge_env carries no W matrix"
    if (length(xwx_idx) > 0) {
      warning(paste0("run_calibrated_counterfactual(): the SAOM estimated an XWX ",
                     "(epistasis) effect, but bridge_env carries no W matrix for it to ",
                     "read, so it was DROPPED. Supply bridge_env$W."), call. = FALSE)
      effects <- effects[-xwx_idx]
    }
  } else if (length(xwx_idx) > 0) {
    xwx_weight <- as.numeric(effects[[xwx_idx[1]]]$parameter)
    xwx_source <- "fitted: taken from the estimated XWX effect"
    effects <- effects[-xwx_idx]      # moved into the coDyadCovars entry below
  } else {
    xwx_weight <- 0
    xwx_source <- "none: no XWX effect was estimated, so the weight is 0 (not invented)"
  }

  influence <- list(
    slot        = if (W_ok) .BRIDGE_W_SLOT else NA_character_,
    effect      = .BRIDGE_W_EFFECT,
    weight      = xwx_weight,
    weight_source = xwx_source,
    registered  = W_ok,
    note        = if (isTRUE(W_ok) && isTRUE(xwx_weight == 0))
      paste0("W is registered and an XWX effect reads it, but its weight is 0 ",
             "because the estimated SAOM carried no XWX term. Epistasis is ",
             "therefore switched OFF in both arms, and any scenario multiplying ",
             "XWX cannot move it. Supply a fitted XWX estimate to change this.")
    else NA_character_
  )

  ## -- Build baseline structure model --------------------------------------- ##
  ##
  ## Since the 2026-08-23 theta-storage repair every branch reads the
  ## coefficient from `parameter` (falling back to `initialValue`), so
  ## `parameter` alone would do. Both are still set, equal, for compatibility
  ## with any engine version in between.

  .as_eff <- function(e, param) {
    out <- list(effect       = e$effect,
                parameter    = param,
                initialValue = param,
                fix          = TRUE,
                dv_name      = .BRIDGE_DV_NAME)
    if (!is.null(e$interaction1) && nzchar(as.character(e$interaction1)))
      out$interaction1 <- e$interaction1
    out
  }

  baseline_effects <- lapply(effects, function(e) .as_eff(e, e$parameter))

  .as_w_entry <- function(weight) list(
    effect       = .BRIDGE_W_EFFECT,
    parameter    = weight,
    initialValue = weight,
    dv_name      = .BRIDGE_DV_NAME,
    fix          = TRUE,
    nodeSet      = c("COMPONENTS", "COMPONENTS"),
    interaction1 = .BRIDGE_W_SLOT,
    x            = W
  )

  coDyad_list <- if (W_ok) list(.as_w_entry(xwx_weight)) else list()

  baseline_model <- list(
    dv_bipartite = list(
      name         = .BRIDGE_DV_NAME,
      type         = "bipartite",
      effects      = baseline_effects,
      coCovars     = list(),
      varCovars    = list(),
      coDyadCovars = coDyad_list,
      varDyadCovars = list(),
      interactions  = list()
    )
  )

  ## -- Validate the scenario against the baseline effects ------------------- ##
  ## DEFECT 3: a `modify` key that matched no effect used to be dropped in
  ## silence, so a typo produced a delta near zero with a full Monte Carlo
  ## apparatus around it and nothing anywhere saying the intervention had not
  ## happened. That is the package's documented silent-failure class, and it is
  ## now a refusal that names the key and lists what could have been modified.

  available <- c(vapply(effects, function(e) as.character(e$effect), character(1)),
                 if (W_ok) .BRIDGE_W_EFFECT else character(0))
  modify_keys <- names(scenario$modify)

  if (is.null(modify_keys) || !all(nzchar(modify_keys))) {
    stop("scenario$modify must be a NAMED list of effect -> multiplier; at least ",
         "one entry has no name, so it cannot be matched to an effect.",
         call. = FALSE)
  }
  if (anyDuplicated(modify_keys)) {
    stop(sprintf(paste0("scenario$modify names an effect more than once (%s); only one ",
                        "multiplier per effect can be applied."),
                 paste(unique(modify_keys[duplicated(modify_keys)]), collapse = ", ")),
         call. = FALSE)
  }

  unknown_keys <- setdiff(modify_keys, available)
  if (length(unknown_keys) > 0) {
    stop(sprintf(
      paste0("scenario '%s' modifies %d effect(s) that the baseline model does not ",
             "contain: %s. Available effects: %s. A key that matches nothing is not a ",
             "weak intervention -- it is NO intervention, and it would otherwise be ",
             "reported as a delta near zero with a full Monte Carlo interval around it. ",
             "Check the spelling, and note that the keys are SaoMNK-side effect names ",
             "(what saom_to_saomnk() produced), not the SAOM names they came from."),
      as.character(scenario$name), length(unknown_keys),
      paste(unknown_keys, collapse = ", "),
      if (length(available)) paste(available, collapse = ", ") else "<none>"),
      call. = FALSE)
  }

  ## A multiplier on a zero baseline parameter is arithmetically inert. It is
  ## the same silent no-op as a typo, so it is named too.
  .baseline_param_of <- function(k) {
    if (identical(k, .BRIDGE_W_EFFECT) && W_ok) return(xwx_weight)
    j <- which(vapply(effects, function(e) identical(as.character(e$effect), k),
                      logical(1)))[1]
    as.numeric(effects[[j]]$parameter)
  }
  inert <- modify_keys[vapply(modify_keys, function(k) {
    isTRUE(.baseline_param_of(k) == 0) &&
      !isTRUE(as.numeric(scenario$modify[[k]]) == 1)
  }, logical(1))]
  if (length(inert) > 0) {
    warning(sprintf(
      paste0("scenario '%s' multiplies %d effect(s) whose baseline parameter is 0, so ",
             "the multiplier cannot change anything: %s. The counterfactual arm is ",
             "identical to the baseline in this respect and the reported delta is Monte ",
             "Carlo noise, not an effect.%s"),
      as.character(scenario$name), length(inert), paste(inert, collapse = ", "),
      if (.BRIDGE_W_EFFECT %in% inert && !is.na(influence$note))
        paste0(" ", influence$note) else ""),
      call. = FALSE)
  }

  ## -- Build counterfactual model (modify specified effects) ----------------- ##

  cf_effects <- lapply(effects, function(e) {
    param <- e$parameter
    if (e$effect %in% modify_keys) {
      param <- param * scenario$modify[[e$effect]]
    }
    .as_eff(e, param)
  })

  cf_model <- baseline_model
  cf_model$dv_bipartite$effects <- cf_effects
  ## XWX lives in the coDyadCovars entry, so a scenario multiplying it has to
  ## reach that entry rather than the effects list.
  if (W_ok && .BRIDGE_W_EFFECT %in% modify_keys) {
    cf_model$dv_bipartite$coDyadCovars <-
      list(.as_w_entry(xwx_weight * scenario$modify[[.BRIDGE_W_EFFECT]]))
  }

  if (verbose) {
    cat(sprintf("\n=== Calibrated Counterfactual: %s ===\n", scenario$name))
    cat(sprintf("Baseline effects: %d, Iterations: %d, Replications: %d\n",
                length(baseline_effects), iterations, n_reps))
    cat(sprintf("Influence matrix W: %s; %s effect weight = %s (%s)\n",
                if (W_ok) sprintf("registered as %s (%d x %d)",
                                  .BRIDGE_W_SLOT, env$N, env$N) else "NOT registered",
                .BRIDGE_W_EFFECT,
                if (is.na(xwx_weight)) "NA" else format(xwx_weight),
                xwx_source))
    cat("Both arms share the replication seed (matched initialisation).\n")
  }

  ## -- Replication loop ------------------------------------------------------ ##
  ## Replication r runs BOTH arms at rand_seed = seed + r - 1, for both the
  ## environment construction and the simulation run, so that the landscape,
  ## the W matrix, the noise matrix and the initial holdings are bit-identical
  ## across arms.  See .BRIDGE_MATCHED_SEED_CAVEAT for what this does NOT buy.

  seeds <- as.integer(seed) + seq_len(n_reps) - 1L

  base_envs  <- vector("list", n_reps)
  cf_envs    <- vector("list", n_reps)
  base_k4s   <- vector("list", n_reps)
  cf_k4s     <- vector("list", n_reps)
  rep_rows   <- vector("list", n_reps)
  actor_rows <- vector("list", n_reps)
  comp_rows  <- vector("list", n_reps)

  actor_ids <- if (!is.null(bridge_env$actor_attrs) &&
                   nrow(bridge_env$actor_attrs) == env$M) {
    as.character(bridge_env$actor_attrs$id)
  } else {
    as.character(seq_len(env$M))
  }
  comp_ids <- if (!is.null(bridge_env$component_attrs) &&
                  nrow(bridge_env$component_attrs) == env$N) {
    as.character(bridge_env$component_attrs$id)
  } else {
    as.character(seq_len(env$N))
  }

  for (r in seq_len(n_reps)) {

    seed_r <- seeds[r]
    if (verbose) {
      cat(sprintf("  [rep %d/%d] seed %d: baseline ... ", r, n_reps, seed_r))
    }

    env_b <- .bridge_build_env(bridge_env, seed_r,
                               sprintf("base_%s_rep%02d", scenario$name, r))
    .bridge_run_arm(env_b, baseline_model, iterations, seed_r)
    ## Once, on the first baseline arm: did the parameters actually reach the
    ## simulated theta?  See .bridge_check_theta() for why this cannot be taken
    ## on trust.
    if (r == 1L) .bridge_check_theta(env_b, baseline_model)
    k4_b <- .extract_k4_summary(env_b)

    if (verbose) cat("counterfactual ... ")

    env_c <- .bridge_build_env(bridge_env, seed_r,
                               sprintf("cf_%s_rep%02d", scenario$name, r))
    .bridge_run_arm(env_c, cf_model, iterations, seed_r)
    k4_c <- .extract_k4_summary(env_c)

    ## Aggregate (one row per measure)
    base_means <- vapply(.BRIDGE_K_MEASURES,
                         function(m) as.numeric(k4_b[[paste0("mean_", m)]]), numeric(1))
    cf_means   <- vapply(.BRIDGE_K_MEASURES,
                         function(m) as.numeric(k4_c[[paste0("mean_", m)]]), numeric(1))

    rep_rows[[r]] <- data.frame(
      rep            = r,
      seed           = seed_r,
      measure        = .BRIDGE_K_MEASURES,
      baseline       = unname(base_means),
      counterfactual = unname(cf_means),
      delta          = unname(cf_means - base_means),
      stringsAsFactors = FALSE
    )

    ## Per-actor and per-component deltas (DEFECT 3: no longer discarded)
    actor_rows[[r]] <- .bridge_unit_deltas(
      k4_b, k4_c, c("K_AC", "K_AA"), "actor", actor_ids, r, seed_r
    )
    comp_rows[[r]] <- .bridge_unit_deltas(
      k4_b, k4_c, c("K_CA", "K_CC"), "component", comp_ids, r, seed_r
    )

    base_envs[[r]] <- env_b
    cf_envs[[r]]   <- env_c
    base_k4s[[r]]  <- k4_b
    cf_k4s[[r]]    <- k4_c

    if (verbose) {
      cat(sprintf("delta K_AC %+.4f\n",
                  rep_rows[[r]]$delta[rep_rows[[r]]$measure == "K_AC"]))
    }
  }

  rep_deltas       <- do.call(rbind, rep_rows)
  actor_deltas     <- do.call(rbind, actor_rows)
  component_deltas <- do.call(rbind, comp_rows)
  rownames(rep_deltas) <- rownames(actor_deltas) <- rownames(component_deltas) <- NULL

  ## -- Monte Carlo summary across replications ------------------------------- ##

  if (n_reps < 2L) {
    warning(paste0("run_calibrated_counterfactual(): n_reps = 1, so no Monte Carlo ",
                   "error is available and the reported delta is a single realization, ",
                   "not an estimate. ", .BRIDGE_MATCHED_SEED_CAVEAT),
            call. = FALSE)
  }

  summary_df <- do.call(rbind, lapply(.BRIDGE_K_MEASURES, function(m) {
    sub <- rep_deltas[rep_deltas$measure == m, , drop = FALSE]
    .bridge_delta_summary(m, sub$baseline, sub$counterfactual, conf_level)
  }))
  rownames(summary_df) <- NULL

  ## Historical element names retained; each is now the MEAN over replications
  ## (identical to the old single-run delta when n_reps == 1).
  comparison <- setNames(
    as.list(summary_df$mean_delta),
    paste0("delta_", summary_df$measure)
  )

  if (verbose) {
    cat("\n--- Mean delta over replications (paired, matched seeds) ---\n")
    print(summary_df[, c("measure", "n_reps", "mean_delta", "mc_se",
                         "ci_lower", "ci_upper", "p_value")])
    if (!is.na(influence$note)) cat(sprintf("\nNOTE: %s\n", influence$note))
    cat(sprintf("\nCAVEAT: %s\n\n", .BRIDGE_MATCHED_SEED_CAVEAT))
  }

  list(
    scenario         = scenario,
    baseline         = list(env  = base_envs[[1]], k4 = base_k4s[[1]],
                            envs = base_envs,      k4_reps = base_k4s),
    counterfactual   = list(env  = cf_envs[[1]],   k4 = cf_k4s[[1]],
                            envs = cf_envs,        k4_reps = cf_k4s),
    comparison       = comparison,
    rep_deltas       = rep_deltas,
    summary          = summary_df,
    actor_deltas     = actor_deltas,
    component_deltas = component_deltas,
    n_reps           = n_reps,
    seeds            = seeds,
    conf_level       = conf_level,
    influence        = influence,
    caveat           = .BRIDGE_MATCHED_SEED_CAVEAT
  )
}


# ---------------------------------------------------------------------------- #
#  run_calibrated_counterfactual internal helpers
# ---------------------------------------------------------------------------- #

#' Check that the requested parameters reached the simulated theta
#'
#' A calibrated counterfactual is only calibrated if the numbers asked for are
#' the numbers simulated, so this remains verified rather than assumed.
#' Historical context: until the 2026-08-23 theta-storage repair,
#' \code{get_theta_matrix()} read the effects table's \code{parm} column while
#' the \code{cycle4} / \code{XWX} / \code{X} branches of
#' \code{include_rsiena_effect_from_eff_list()} wrote \code{initialValue}, so
#' those effects registered, printed, and then simulated at \code{parm}'s
#' default (1 for cycle4, 0 for XWX/X) whatever the caller asked.  The engine
#' now carries theta in \code{initialValue} throughout and the reader reads it,
#' so this check should find nothing -- it stays as the guard that says so out
#' loud if the two ever drift apart again.
#'
#' @param env A \code{SaomNkRSienaBiEnv} after \code{.bridge_run_arm}.
#' @param model The structure model that was requested.
#' @return Character vector of mismatch descriptions, invisibly (empty if none).
#' @keywords internal
.bridge_check_theta <- function(env, model) {

  th <- env$theta_matrix
  if (is.null(th) || is.null(dim(th)) || is.null(colnames(th)))
    return(invisible(character(0)))

  used <- as.numeric(th[1, ])
  names(used) <- sub("_[0-9]+$", "", colnames(th))

  wanted <- c(model$dv_bipartite$effects, model$dv_bipartite$coDyadCovars)
  bad <- character(0)

  for (e in wanted) {
    nm  <- as.character(e$effect)
    tgt <- as.numeric(e$parameter)
    hit <- which(names(used) == nm)
    if (length(hit) == 0L) {
      ## The rate parameter is deliberately absent from the theta matrix under
      ## conditional simulation; anything else missing is a real absence.
      if (!nm %in% .BRIDGE_RATE_SHORTNAMES)
        bad <- c(bad, sprintf("%s: asked %.4f, ABSENT from the simulated theta",
                              nm, tgt))
      next
    }
    got <- used[hit[1]]
    if (!isTRUE(all.equal(unname(got), tgt, tolerance = 1e-6)))
      bad <- c(bad, sprintf("%s: asked %.4f, simulated %.4f", nm, tgt, unname(got)))
  }

  if (length(bad) > 0) {
    warning(sprintf(
      paste0("run_calibrated_counterfactual(): %d effect(s) were registered but are NOT ",
             "simulated at the parameter they were given:\n  %s\n",
             "The counterfactual is therefore not calibrated in those effects, and a ",
             "scenario multiplying one of them will move nothing. This is an engine ",
             "defect, not a modeling result: the declared coefficient must reach ",
             "the effects table's `initialValue` column, which get_theta_matrix() ",
             "reads (theta-storage convention, 2026-08-23). Report the affected ",
             "effects as NOT SIMULATED; do not report their deltas as effects of ",
             "the intervention."),
      length(bad), paste(bad, collapse = "\n  ")), call. = FALSE)
  }

  invisible(bad)
}


#' Run one arm of one replication
#'
#' Uses \code{search_rsiena()}, which is the entry point that actually applies
#' the structure model's parameters.
#'
#' This function previously called
#' \code{search_rsiena_multiwave_run(waves = 1)} followed by
#' \code{search_rsiena_process_stats()}, and that combination could not produce
#' a counterfactual at all:
#' \itemize{
#'   \item \code{search_rsiena_multiwave_run()} passes no \code{thetaValues} to
#'     \code{siena07()}, so the coefficients used are RSiena's
#'     \code{initialValue} column.  At the time of the measurement the engine
#'     wrote coefficients into \code{parm} via \code{setEffect(parameter = )},
#'     leaving \code{initialValue} at zero for the whole structural family, so
#'     the calibrated thetas never reached the simulation.  Measured directly:
#'     with \code{density} at -0.5 and at -4.0 the final mean scope was 1.800
#'     both times, i.e. exactly the initial value, while through
#'     \code{search_rsiena()} the same two models give 2.950 and 1.100.  Every
#'     delta this function reported was therefore identically zero, with a
#'     Monte Carlo interval printed around it.  (Since the 2026-08-23
#'     theta-storage repair, \code{initialValue} carries the declared
#'     coefficients, which incidentally repairs the multiwave path too.)
#'   \item it also leaves \code{self$chain_stats} unset, so the following
#'     \code{search_rsiena_process_stats()} stopped with "chain_stats missing"
#'     before the first replication finished.
#' }
#' \code{search_rsiena()} builds the per-ministep theta matrix from the
#' effects' \code{initialValue} column (the theta-storage convention) and
#' passes it as \code{thetaValues}, and it processes the ministep chain
#' itself, so both problems close together.
#'
#' @param env A \code{SaomNkRSienaBiEnv} from \code{.bridge_build_env}.
#' @param model The structure model for this arm.
#' @param iterations Integer ministeps in the simulated decision chain.
#' @param seed_r Integer replication seed, shared by both arms.
#' @return The environment, invisibly, after simulation.
#' @keywords internal
.bridge_run_arm <- function(env, model, iterations, seed_r) {
  env$search_rsiena(
    structure_model = model,
    iterations      = iterations,
    run_seed        = seed_r,
    process_chain   = TRUE,
    verbose         = FALSE
  )
  invisible(env)
}


#' Build a simulation environment for one arm of one replication
#'
#' Constructs a fresh \code{SaomNkRSienaBiEnv} from the empirical starting
#' matrix at a given seed.  Both arms of a replication call this with the SAME
#' seed, which is what makes the landscape, the W matrix, the noise matrix and
#' the initial holdings bit-identical across arms.
#'
#' @param bridge_env Output from \code{\link{empirical_to_saomnk_env}}.
#' @param seed Integer seed for this replication.
#' @param name Character environment name.
#' @return A \code{SaomNkRSienaBiEnv} object.
#' @keywords internal
.bridge_build_env <- function(bridge_env, seed, name) {

  env <- bridge_env$env
  bi  <- bridge_env$bi_matrix
  if (is.null(bi)) bi <- env$bipartite_matrix_init
  if (is.null(bi)) {
    stop("bridge_env has neither $bi_matrix nor an initial bipartite matrix; cannot build a matched environment.",
         call. = FALSE)
  }

  new_env <- SaomNkRSienaBiEnv$new(list(
    M           = env$M,
    N           = env$N,
    BI_PROB     = sum(bi) / (nrow(bi) * ncol(bi)),
    init_matrix = bi,
    rand_seed   = as.integer(seed),
    name        = name,
    ## Inherit the caller's output directory. Each replication writes an RSiena
    ## log and a sink file, so 2 * n_reps of them land wherever this points; the
    ## constructor's default is getwd().
    dir_output  = env$DIR_OUTPUT
  ))
  new_env$bipartite_matrix      <- bi
  new_env$bipartite_matrix_init <- bi
  new_env
}


#' Per-unit (actor or component) delta table for one replication
#'
#' @param k4_b Baseline K-4 summary.
#' @param k4_c Counterfactual K-4 summary.
#' @param measures Character vector of measures to difference.
#' @param unit_label Either \code{"actor"} or \code{"component"}.
#' @param unit_ids Character vector of unit identifiers.
#' @param rep_id Integer replication index.
#' @param seed_r Integer replication seed.
#' @return A tidy \code{data.frame}, one row per unit.
#' @keywords internal
.bridge_unit_deltas <- function(k4_b, k4_c, measures, unit_label, unit_ids,
                                rep_id, seed_r) {

  n_unit <- length(k4_b[[measures[1]]])
  if (length(k4_c[[measures[1]]]) != n_unit) {
    stop(sprintf("Arms disagree on the number of %ss (%d vs %d); per-unit deltas are undefined.",
                 unit_label, n_unit, length(k4_c[[measures[1]]])), call. = FALSE)
  }
  if (length(unit_ids) != n_unit) unit_ids <- as.character(seq_len(n_unit))

  out <- data.frame(
    rep  = rep_id,
    seed = seed_r,
    unit = seq_len(n_unit),
    unit_id = unit_ids,
    stringsAsFactors = FALSE
  )
  names(out)[names(out) == "unit"]    <- unit_label
  names(out)[names(out) == "unit_id"] <- paste0(unit_label, "_id")

  for (m in measures) {
    b <- as.numeric(k4_b[[m]])
    cf <- as.numeric(k4_c[[m]])
    out[[paste0(m, "_baseline")]]       <- b
    out[[paste0(m, "_counterfactual")]] <- cf
    out[[paste0("delta_", m)]]          <- cf - b
  }
  out
}


#' Summarize the paired deltas for one measure across replications
#'
#' @param measure Character measure name.
#' @param base_vals Numeric vector of baseline values, one per replication.
#' @param cf_vals Numeric vector of counterfactual values, one per replication.
#' @param conf_level Numeric confidence level.
#' @return A one-row \code{data.frame}.
#' @keywords internal
.bridge_delta_summary <- function(measure, base_vals, cf_vals, conf_level) {

  d <- cf_vals - base_vals
  n <- length(d)

  sd_d  <- if (n > 1L) sd(d) else NA_real_
  mc_se <- if (n > 1L) sd_d / sqrt(n) else NA_real_

  tt <- if (n > 1L && !is.na(sd_d) && sd_d > 0) {
    tryCatch(
      stats::t.test(cf_vals, base_vals, paired = TRUE, conf.level = conf_level),
      error = function(e) NULL
    )
  } else NULL

  alpha <- 1 - conf_level
  qs <- if (n > 1L) {
    unname(quantile(d, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE))
  } else {
    c(NA_real_, NA_real_)
  }

  data.frame(
    measure       = measure,
    n_reps        = n,
    mean_baseline = mean(base_vals),
    mean_cf       = mean(cf_vals),
    mean_delta    = mean(d),
    sd_delta      = sd_d,
    mc_se         = mc_se,
    t_stat        = if (is.null(tt)) NA_real_ else unname(tt$statistic),
    df            = if (is.null(tt)) NA_real_ else unname(tt$parameter),
    p_value       = if (is.null(tt)) NA_real_ else unname(tt$p.value),
    ci_lower      = if (is.null(tt)) NA_real_ else unname(tt$conf.int[1]),
    ci_upper      = if (is.null(tt)) NA_real_ else unname(tt$conf.int[2]),
    q_lower       = qs[1],
    q_upper       = qs[2],
    conf_level    = conf_level,
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------------- #
#  .extract_k4_summary  (internal helper)
# ---------------------------------------------------------------------------- #

#' Extract K-4 degree summary from a simulated environment
#'
#' Computes mean K-degree values from the bipartite matrix at the end of
#' the simulation chain.  Works with the current state of the environment
#' regardless of whether formal results processing has been called.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after simulation.
#' @return A named list of mean K-degree values.
#' @keywords internal
.extract_k4_summary <- function(env) {
  bi <- env$bipartite_matrix
  M  <- nrow(bi)
  N  <- ncol(bi)

  ## K_AC: actor -> component degree (row sums = scope)
  K_AC <- rowSums(bi)
  ## K_CA: component -> actor degree (col sums = popularity)
  K_CA <- colSums(bi)

  ## K_AA: actor-actor co-affiliation (bipartite projection onto actors)
  AA <- bi %*% t(bi)
  diag(AA) <- 0
  K_AA <- rowSums(AA > 0)

  ## K_CC: component-component co-affiliation (bipartite projection onto components)
  CC <- t(bi) %*% bi
  diag(CC) <- 0
  K_CC <- rowSums(CC > 0)

  list(
    mean_K_AC = mean(K_AC),
    mean_K_CA = mean(K_CA),
    mean_K_AA = mean(K_AA),
    mean_K_CC = mean(K_CC),
    sd_K_AC   = sd(K_AC),
    sd_K_CA   = sd(K_CA),
    sd_K_AA   = sd(K_AA),
    sd_K_CC   = sd(K_CC),
    K_AC      = K_AC,
    K_CA      = K_CA,
    K_AA      = K_AA,
    K_CC      = K_CC
  )
}


# ---------------------------------------------------------------------------- #
#  get_orm_scenarios
# ---------------------------------------------------------------------------- #

#' Get predefined counterfactual scenarios for the ORM paper
#'
#' Returns a named list of scenario specifications suitable for use with
#' \code{\link{run_calibrated_counterfactual}}.  Each scenario modifies one
#' or more effect parameters via a multiplier applied to the baseline value.
#'
#' @return Named list of scenario specifications.  Each element is a list
#'   with \code{name}, \code{description}, and \code{modify}.
#' @section Keys are SaoMNK-side effect names:
#' Every \code{modify} key must name an effect the converted baseline model
#' actually contains, because \code{\link{run_calibrated_counterfactual}} now
#' refuses a key that matches nothing rather than ignoring it.  A scenario is
#' therefore only usable against a bridge whose SAOM estimated the effect it
#' names: \code{remove_homophily} needs an \code{egoX} in the fitted model,
#' \code{epistasis_boost} needs a fitted \code{XWX} weight, and so on.
#' @export
#' @examples
#' scenarios <- get_orm_scenarios()
#' names(scenarios)
#' scenarios$double_closure
get_orm_scenarios <- function() {
  list(
    ## Closure for a BIPARTITE dependent variable is the 4-cycle, not the
    ## transitive triad: RSiena 1.5.0 offers `transTriads` in the
    ## symmetricObjective group only, so a scenario keyed on `transTriads`
    ## could never match an effect a SaoMNK model is able to carry.
    double_closure = list(
      name        = "Double Closure (2x 4-cycle)",
      description = paste0("What if closure tendency were twice as strong? ",
                           "Bipartite closure is cycle4; transTriads does not ",
                           "exist for a two-mode dependent variable."),
      modify      = list(cycle4 = 2.0)
    ),
    remove_homophily = list(
      name        = "Remove Homophily",
      description = "What if firms ignored industry similarity?",
      modify      = list(egoX = 0.0)
    ),
    double_popularity = list(
      name        = "Double Popularity",
      description = "What if preferential attachment were stronger?",
      modify      = list(inPop = 2.0)
    ),
    density_shock = list(
      name        = "Density Shock (-50%)",
      description = "What if tie formation became much costlier?",
      modify      = list(density = 1.5)  # more negative = costlier
    ),
    remove_rivalry = list(
      name        = "Remove Rivalry",
      description = "What if competitive avoidance disappeared?",
      modify      = list(cycle4 = 0.0)
    ),
    epistasis_boost = list(
      name        = "Epistasis Boost (2x)",
      description = "What if component interdependencies doubled?",
      modify      = list(XWX = 2.0)
    )
  )
}


# ---------------------------------------------------------------------------- #
#  run_counterfactual_with_uncertainty
# ---------------------------------------------------------------------------- #

#' Counterfactual with BOTH sources of uncertainty separated
#'
#' \code{\link{saom_to_saomnk}} can draw one theta from its estimated sampling
#' distribution, and \code{\link{run_calibrated_counterfactual}} can average a
#' delta over Monte Carlo replications, but nothing composed the two: producing
#' an interval on a model-implied effect meant hand-assembling a loop, and a
#' hand-assembled loop is where the two variances get added together by
#' accident.  This wrapper runs the composition and reports the two sources
#' SEPARATELY.
#'
#' @section The two variances are not the same quantity:
#' \describe{
#'   \item{Monte Carlo variance}{Within a draw, across replications.  It is a
#'     property of the SIMULATION: the same parameters, run again, give a
#'     different realized path.  It shrinks like \eqn{1/n\_reps} and can be made
#'     arbitrarily small by running the simulation longer.  It says nothing
#'     about how well the parameters are known.}
#'   \item{Parameter variance}{Between draws.  It is a property of the
#'     ESTIMATE: \eqn{\hat\theta}{thetahat} is uncertain, and the counterfactual
#'     inherits that.  It does NOT shrink with \code{n_reps}; only more data, or
#'     a better-identified model, reduces it.}
#' }
#' Reporting their sum as "the" standard error, or quoting the Monte Carlo error
#' as if it were an inferential interval on the model-implied effect, conflates a
#' computing budget with an estimation result.  Both are returned here, plus the
#' between-draw variance they are decomposed from, so a table can state which one
#' it is quoting.
#'
#' The decomposition is the one-way random-effects (ANOVA) estimator.  With
#' \eqn{\bar d_i}{dbar_i} the mean delta of draw \eqn{i} and \eqn{s_i}{s_i} its
#' within-draw standard deviation across replications:
#' \code{var_mc = mean(s_i^2) / n_reps} is the Monte Carlo variance of one
#' draw's mean, \code{var_between = var(dbar_i)} is what is observed between
#' draws, and \code{var_parameter = max(0, var_between - var_mc)} is the part
#' attributable to theta.  The floor at zero can bite when \code{n_draws} is
#' small or the simulation is noisy relative to the parameter uncertainty; when
#' it does, \code{parameter_var_floored} is \code{TRUE} and the parameter
#' variance is not distinguishable from Monte Carlo noise at this budget.
#'
#' Each draw gets its OWN block of replication seeds
#' (\code{seed + (d - 1) * n_reps + r - 1}), so draws are independent and the
#' decomposition above is valid.  Within a draw both arms still share the
#' replication seed, so the matched-seed caveat is unchanged.
#'
#' @param bridge_env Output from \code{\link{empirical_to_saomnk_env}}.
#' @param saom_result A \code{sienaFit} (from \code{siena07}).  A bare theta
#'   vector is refused: it carries no covariance matrix, so there is no sampling
#'   distribution to draw from and no parameter uncertainty to report.
#' @param scenario Scenario specification; see
#'   \code{\link{run_calibrated_counterfactual}}.
#' @param n_draws Integer. Number of theta draws (default \code{20}).
#' @param n_reps Integer. Monte Carlo replications PER DRAW (default \code{10}).
#' @param iterations Integer. RSiena iterations per wave (default \code{50}).
#' @param seed Integer. Base replication seed (default \code{42}).
#' @param draw_seed_base Integer. Base seed for the theta draws; draw \code{d}
#'   uses \code{draw_seed_base + d} (default \code{1000}).
#' @param scale_factor Numeric multiplier passed to \code{\link{saom_to_saomnk}}.
#' @param strict Logical, passed to \code{\link{saom_to_saomnk}} (default
#'   \code{TRUE}).
#' @param conf_level Numeric confidence level (default \code{0.95}).
#' @param keep_runs Logical. Keep every per-draw counterfactual result object
#'   (default \code{FALSE}; each carries \code{2 * n_reps} simulated
#'   environments, so keeping them is expensive).
#' @param verbose Logical. Print per-draw progress and the pooled summary
#'   (default \code{TRUE}).
#' @return A list with components:
#'   \describe{
#'     \item{\code{scenario}}{The scenario specification.}
#'     \item{\code{summary}}{One row per K-degree measure: \code{mean_delta}
#'       (pooled over draws), \code{se_total} and its t interval
#'       (\code{ci_lower}, \code{ci_upper}), the quantile interval over draw
#'       means (\code{q_lower}, \code{q_upper}), and the decomposition
#'       \code{var_between}, \code{var_mc}, \code{var_parameter},
#'       \code{sd_mc}, \code{sd_parameter}, \code{share_parameter},
#'       \code{parameter_var_floored}.}
#'     \item{\code{draw_deltas}}{One row per draw x measure: \code{draw},
#'       \code{draw_seed}, \code{measure}, \code{mean_delta}, \code{sd_delta},
#'       \code{mc_se}.}
#'     \item{\code{rep_deltas}}{Every replication-level delta, with a
#'       \code{draw} column, for downstream re-analysis.}
#'     \item{\code{theta_draws}}{\code{n_draws x p} matrix of the drawn thetas.}
#'     \item{\code{influence}}{The W / \code{XWX} report from the first draw.}
#'     \item{\code{runs}}{The per-draw result objects when
#'       \code{keep_runs = TRUE}, otherwise \code{NULL}.}
#'     \item{\code{caveat}}{The matched-seed caveat.}
#'     \item{\code{uncertainty_note}}{One sentence stating that the two
#'       variances are different quantities, for pasting into a table note.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fit <- siena07(alg, data = mydata, effects = myeffects)
#' bridge_env <- empirical_to_saomnk_env(mi_data, wave = 3)
#' out <- run_counterfactual_with_uncertainty(
#'   bridge_env, fit,
#'   scenario = get_orm_scenarios()$double_popularity,
#'   n_draws = 25, n_reps = 10
#' )
#' out$summary[, c("measure", "mean_delta", "sd_mc", "sd_parameter")]
#' }
run_counterfactual_with_uncertainty <- function(bridge_env, saom_result,
                                                scenario = list(
                                                  name   = "double_popularity",
                                                  modify = list(inPop = 2.0)
                                                ),
                                                n_draws = 20,
                                                n_reps = 10,
                                                iterations = 50,
                                                seed = 42,
                                                draw_seed_base = 1000L,
                                                scale_factor = 1.0,
                                                strict = TRUE,
                                                conf_level = 0.95,
                                                keep_runs = FALSE,
                                                verbose = TRUE) {

  stopifnot(is.list(bridge_env), !is.null(bridge_env$env))
  stopifnot(is.list(scenario), !is.null(scenario$name), !is.null(scenario$modify))

  n_draws <- as.integer(n_draws)
  n_reps  <- as.integer(n_reps)
  if (is.na(n_draws) || n_draws < 1L)
    stop("n_draws must be a positive integer.", call. = FALSE)
  if (is.na(n_reps) || n_reps < 1L)
    stop("n_reps must be a positive integer.", call. = FALSE)
  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop("conf_level must be a number strictly between 0 and 1.", call. = FALSE)

  if (!inherits(saom_result, "sienaFit")) {
    stop(paste0("run_counterfactual_with_uncertainty() needs the FIT, not a converted ",
                "bridge or a bare theta vector: the parameter uncertainty it reports is ",
                "drawn from the estimated covariance matrix, which only a sienaFit ",
                "carries. For a counterfactual at the point estimates, with Monte Carlo ",
                "error only, call run_calibrated_counterfactual() directly."),
         call. = FALSE)
  }
  if (n_draws < 2L) {
    warning(paste0("run_counterfactual_with_uncertainty(): n_draws = 1, so there is no ",
                   "between-draw variation and the parameter component of the ",
                   "uncertainty is undefined. The result is one draw's counterfactual, ",
                   "not an interval over the sampling distribution of theta."),
            call. = FALSE)
  }

  draw_seeds <- as.integer(draw_seed_base) + seq_len(n_draws)

  ## Warnings from the per-draw conversion (dropped effects, approximations,
  ## rate parameters) are identical on every draw. Collect them and raise each
  ## distinct message ONCE at the end, so the report is not buried under
  ## n_draws copies of the same sentence -- and is not suppressed either.
  warn_log <- character(0)
  .collect <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      warn_log <<- c(warn_log, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }

  draw_rows  <- vector("list", n_draws)
  rep_rows   <- vector("list", n_draws)
  runs       <- if (isTRUE(keep_runs)) vector("list", n_draws) else NULL
  theta_list <- vector("list", n_draws)
  influence  <- NULL

  for (d in seq_len(n_draws)) {

    if (verbose) cat(sprintf("  [draw %d/%d] theta seed %d ... ",
                             d, n_draws, draw_seeds[d]))

    bp <- .collect(saom_to_saomnk(saom_result,
                                  scale_factor = scale_factor,
                                  draw_theta   = TRUE,
                                  draw_seed    = draw_seeds[d],
                                  strict       = strict,
                                  verbose      = FALSE))
    theta_list[[d]] <- bp$theta_used

    res <- .collect(run_calibrated_counterfactual(
      bridge_env, bp, scenario = scenario,
      iterations = iterations, n_reps = n_reps,
      seed = as.integer(seed) + (d - 1L) * n_reps,
      conf_level = conf_level, verbose = FALSE))

    if (is.null(influence)) influence <- res$influence
    if (isTRUE(keep_runs)) runs[[d]] <- res

    rd <- res$rep_deltas
    rd$draw      <- d
    rd$draw_seed <- draw_seeds[d]
    rep_rows[[d]] <- rd

    draw_rows[[d]] <- do.call(rbind, lapply(.BRIDGE_K_MEASURES, function(m) {
      dv <- rd$delta[rd$measure == m]
      data.frame(draw = d, draw_seed = draw_seeds[d], measure = m,
                 mean_delta = mean(dv),
                 sd_delta   = if (length(dv) > 1L) sd(dv) else NA_real_,
                 mc_se      = if (length(dv) > 1L) sd(dv) / sqrt(length(dv)) else NA_real_,
                 stringsAsFactors = FALSE)
    }))

    if (verbose) {
      cat(sprintf("delta K_AC %+.4f\n",
                  draw_rows[[d]]$mean_delta[draw_rows[[d]]$measure == "K_AC"]))
    }
  }

  draw_deltas <- do.call(rbind, draw_rows)
  rep_deltas  <- do.call(rbind, rep_rows)
  rownames(draw_deltas) <- rownames(rep_deltas) <- NULL

  theta_draws <- do.call(rbind, theta_list)

  summary_df <- do.call(rbind, lapply(.BRIDGE_K_MEASURES, function(m) {
    sub <- draw_deltas[draw_deltas$measure == m, , drop = FALSE]
    .bridge_variance_decomposition(m, sub$mean_delta, sub$sd_delta,
                                   n_reps, conf_level)
  }))
  rownames(summary_df) <- NULL

  ## Re-raise the collected per-draw warnings, once each.
  if (length(warn_log) > 0) {
    tab <- table(warn_log)
    for (msg in names(tab)) {
      warning(sprintf("[raised on %d of %d draw(s)] %s",
                      as.integer(tab[[msg]]), n_draws, msg), call. = FALSE)
    }
  }

  note <- paste0(
    "Monte Carlo error (within draw, across replications) and parameter ",
    "uncertainty (between draws, from the estimated sampling distribution of ",
    "theta) are different quantities and are reported separately. The first ",
    "shrinks with more replications and reflects the simulation budget; the ",
    "second does not shrink with replications and reflects how well the SAOM ",
    "is estimated. Do not add them into a single 'standard error' without ",
    "saying so: se_total below is the standard error of the pooled mean over ",
    "draws, which contains both."
  )

  if (verbose) {
    cat(sprintf("\n--- Pooled over %d theta draws x %d replications ---\n",
                n_draws, n_reps))
    print(summary_df[, c("measure", "mean_delta", "se_total",
                         "sd_mc", "sd_parameter", "share_parameter")])
    cat(sprintf("\nUNCERTAINTY: %s\n", note))
    cat(sprintf("\nCAVEAT: %s\n\n", .BRIDGE_MATCHED_SEED_CAVEAT))
  }

  list(
    scenario         = scenario,
    n_draws          = n_draws,
    n_reps           = n_reps,
    draw_seeds       = draw_seeds,
    summary          = summary_df,
    draw_deltas      = draw_deltas,
    rep_deltas       = rep_deltas,
    theta_draws      = theta_draws,
    influence        = influence,
    runs             = runs,
    conf_level       = conf_level,
    caveat           = .BRIDGE_MATCHED_SEED_CAVEAT,
    uncertainty_note = note
  )
}


#' One-way variance decomposition of a pooled counterfactual delta
#'
#' Separates the Monte Carlo component (within draw, across replications) from
#' the parameter component (between draws).  See the \emph{two variances}
#' section of \code{\link{run_counterfactual_with_uncertainty}} for why they
#' must not be pooled into one number.
#'
#' @param measure Character measure name.
#' @param draw_means Numeric vector of per-draw mean deltas.
#' @param draw_sds Numeric vector of per-draw within-draw standard deviations.
#' @param n_reps Integer replications per draw.
#' @param conf_level Numeric confidence level.
#' @return A one-row \code{data.frame}.
#' @keywords internal
.bridge_variance_decomposition <- function(measure, draw_means, draw_sds,
                                           n_reps, conf_level) {

  n_draws <- length(draw_means)

  ## Monte Carlo variance OF A DRAW MEAN: the average within-draw variance of a
  ## single replication, divided by the number of replications averaged.
  var_mc <- if (all(is.na(draw_sds))) NA_real_ else
    mean(draw_sds^2, na.rm = TRUE) / n_reps

  var_between <- if (n_draws > 1L) stats::var(draw_means) else NA_real_

  var_param <- if (is.na(var_between) || is.na(var_mc)) NA_real_ else
    max(0, var_between - var_mc)
  floored <- !is.na(var_between) && !is.na(var_mc) && (var_between - var_mc) < 0

  se_total <- if (n_draws > 1L) sqrt(var_between / n_draws) else NA_real_
  alpha <- 1 - conf_level
  half  <- if (n_draws > 1L) stats::qt(1 - alpha / 2, n_draws - 1L) * se_total else NA_real_
  qs    <- if (n_draws > 1L)
    unname(quantile(draw_means, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE))
  else c(NA_real_, NA_real_)

  data.frame(
    measure       = measure,
    n_draws       = n_draws,
    n_reps        = as.integer(n_reps),
    mean_delta    = mean(draw_means),
    se_total      = se_total,
    ci_lower      = mean(draw_means) - half,
    ci_upper      = mean(draw_means) + half,
    q_lower       = qs[1],
    q_upper       = qs[2],
    var_between   = var_between,
    var_mc        = var_mc,
    var_parameter = var_param,
    sd_mc         = if (is.na(var_mc)) NA_real_ else sqrt(var_mc),
    sd_parameter  = if (is.na(var_param)) NA_real_ else sqrt(var_param),
    share_parameter = if (is.na(var_param) || is.na(var_between) || var_between == 0)
      NA_real_ else var_param / var_between,
    parameter_var_floored = floored,
    conf_level    = conf_level,
    stringsAsFactors = FALSE
  )
}
