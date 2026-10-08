#' @title Causal Inference Wrappers for searchnet Simulations
#' @description
#' Functions that connect searchnet's \code{theta_shocks} output to standard
#' causal inference packages: \pkg{did} (Callaway & Sant'Anna 2021),
#' \pkg{Synth} (Abadie, Diamond & Hainmueller 2010), and \pkg{rdrobust}
#' (Calonico, Cattaneo & Titiunik 2014).
#'
#' The key insight is that searchnet's \code{theta_shocks} create
#' treatment/control conditions within SAOM simulations.
#' The \code{actor_util_df} and K-4 trajectory data have a natural panel
#' structure (actor x step) with a treatment onset at the shock step.
#' This maps directly to DID, synthetic control, and RD designs.
#'
#' @name searchnet-causal
#' @importFrom stats aggregate setNames
NULL


# ============================================================================
#  searchnet_causal_panel
# ============================================================================

#' Prepare searchnet simulation data for causal inference
#'
#' Extracts panel data from a shocked simulation with treatment/control
#' structure.  The returned data frame is formatted for direct input to
#' \code{\link{searchnet_did}}, \code{\link{searchnet_synth}}, and
#' \code{\link{searchnet_rd}}.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after simulation with
#'   \code{theta_shocks} (i.e., after calling \code{\link{saomnk_run}} with
#'   a \code{shocks} argument).
#' @param shock_step Integer. The simulation step at which the shock occurred.
#'   Steps before this value are the pre-treatment period; steps at or after
#'   are the post-treatment period.
#' @param outcome Character. Which outcome variable to extract.
#'   One of \code{"utility"} (default), \code{"K_AC"}, \code{"K_CA"},
#'   \code{"K_AA"}, or \code{"K_CC"}.
#' @param treated_actors Optional integer vector of actor IDs considered
#'   treated.
#'   \itemize{
#'     \item If \code{NULL} (default), all actors are treated and the design
#'       is a simple before/after comparison (useful for aggregate shocks that
#'       affect the entire environment).
#'     \item If specified, creates a proper DID design with explicit
#'       treated/control groups.
#'   }
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{\code{actor_id}}{Factor. Actor identifier.}
#'     \item{\code{step}}{Integer. Simulation chain step.}
#'     \item{\code{period}}{Character. \code{"pre"} or \code{"post"} relative
#'       to \code{shock_step}.}
#'     \item{\code{outcome}}{Numeric. The outcome variable value.}
#'     \item{\code{treated}}{Integer. 1 if the actor is in the treated group,
#'       0 otherwise.}
#'     \item{\code{first_treat}}{Integer. The step at which treatment begins
#'       for treated actors; 0 for never-treated (control) actors. Required
#'       by \code{did::att_gt()}.}
#'     \item{\code{shock_step}}{Integer. The shock step stored for downstream
#'       use by \code{\link{searchnet_rd}}.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' env <- saomnk_env(M = 6, N = 8, seed = 42)
#' mod <- saomnk_model(density = -0.5,
#'                     influence_matrix = saomnk_block_diagonal(8, 2))
#' s1 <- saomnk_shock("density", parameter = -0.5, portion = 1)
#' s2 <- saomnk_shock("density", parameter = -2.0, portion = 1)
#' saomnk_run(env, mod, steps_per_actor = 20, seed = 123,
#'            shocks = list(s1, s2))
#'
#' panel <- searchnet_causal_panel(env, shock_step = 60,
#'                                  outcome = "utility")
#' head(panel)
#' }
searchnet_causal_panel <- function(env, shock_step, outcome = "utility",
                                    treated_actors = NULL) {
  .searchnet_require_path(env, "searchnet_causal_panel()")

  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))
  stopifnot(is.numeric(shock_step), length(shock_step) == 1, shock_step > 0)

  outcome <- match.arg(outcome,
                       choices = c("utility", "K_AC", "K_CA", "K_AA", "K_CC"))

  # ------------------------------------------------------------------
  #  Extract the appropriate data frame from the environment

  # ------------------------------------------------------------------
  if (outcome == "utility") {
    raw <- env$actor_util_df
    if (is.null(raw))
      stop("No actor_util_df found. Run saomnk_run() first.")
    panel <- data.frame(
      actor_id = as.factor(raw$actor_id),
      step     = as.integer(raw$chain_step_id),
      outcome  = as.numeric(raw$utility),
      stringsAsFactors = FALSE
    )
  } else {
    k_field <- paste0(outcome, "_df")
    raw <- env[[k_field]]
    if (is.null(raw))
      stop(sprintf("No %s found in environment. Run saomnk_run() first.",
                   k_field))
    id_col <- if (outcome %in% c("K_AC", "K_AA")) "actor_id" else "component_id"
    panel <- data.frame(
      actor_id = as.factor(raw[[id_col]]),
      step     = as.integer(raw$chain_step_id),
      outcome  = as.numeric(raw$value),
      stringsAsFactors = FALSE
    )
  }

  # ------------------------------------------------------------------
  #  Add period and treatment indicators
  # ------------------------------------------------------------------
  panel$period <- ifelse(panel$step < shock_step, "pre", "post")

  all_actors <- unique(panel$actor_id)

  if (is.null(treated_actors)) {
    # Before/after design: all actors are treated
    panel$treated    <- 1L
    panel$first_treat <- shock_step
  } else {
    treated_actors <- as.factor(treated_actors)
    panel$treated     <- ifelse(panel$actor_id %in% treated_actors, 1L, 0L)
    panel$first_treat <- ifelse(panel$actor_id %in% treated_actors,
                                shock_step, 0L)
  }

  panel$shock_step <- as.integer(shock_step)

  # Ensure step is integer (required by did)
  panel$step <- as.integer(panel$step)

  panel
}


# ============================================================================
#  searchnet_did
# ============================================================================

#' Run Difference-in-Differences on searchnet simulation
#'
#' Uses the \pkg{did} package (Callaway & Sant'Anna 2021) for group-time
#' average treatment effects.  Requires that the panel contain both treated
#' and control actors---i.e., \code{searchnet_causal_panel()} was called
#' with an explicit \code{treated_actors} argument.
#'
#' \code{did::att_gt()} additionally requires at least 5 distinct units
#' (more if \code{xformla} covariates are supplied) in every
#' \code{first_treat} group, including the never-treated (control) group.
#' \code{searchnet_did()} checks this before calling \code{did::att_gt()}
#' and fails with an actionable message naming the actual requirement and
#' the observed counts, rather than passing through \code{did}'s opaque
#' "never-treated group is too small" error. A design with too few control
#' actors is a genuine scope limit of the Callaway & Sant'Anna estimator,
#' not a defect in the panel; widen the design (more actors, fewer
#' \code{treated_actors}) or, for staggered treatment timing, pass
#' \code{control_group = "notyettreated"}.
#'
#' @param panel A \code{data.frame} returned by
#'   \code{\link{searchnet_causal_panel}} with both treated and control
#'   groups.
#' @param \dots Additional arguments passed to \code{did::att_gt()}.
#' @return A \code{did::MP} object (group-time average treatment effects).
#'   Use \code{did::aggte()} to aggregate and \code{did::ggdid()} to plot.
#' @export
#' @examples
#' \dontrun{
#' panel <- searchnet_causal_panel(env, shock_step = 60,
#'                                  treated_actors = c(1, 2, 3))
#' att <- searchnet_did(panel)
#' summary(att)
#' did::ggdid(did::aggte(att, type = "dynamic"))
#' }
searchnet_did <- function(panel, ...) {
  if (!requireNamespace("did", quietly = TRUE))
    stop("Package 'did' is required for DID analysis.\n",
         "Install with: install.packages('did')")

  # Validate panel structure
  required_cols <- c("actor_id", "step", "outcome", "treated", "first_treat")
  missing <- setdiff(required_cols, names(panel))
  if (length(missing) > 0)
    stop("Panel missing required columns: ",
         paste(missing, collapse = ", "),
         "\nUse searchnet_causal_panel() to create the panel.")

  if (all(panel$treated == 1L))
    stop("DID requires both treated and control groups.\n",
         "Re-run searchnet_causal_panel() with explicit treated_actors.")

  # ------------------------------------------------------------------
  #  Pre-flight group-size check
  # ------------------------------------------------------------------
  # did::att_gt() (via its internal pre_process_did()) requires at least
  # `5 + <number of covariates in xformla>` distinct units in every
  # first_treat group. When the never-treated (control) group falls below
  # that floor and control_group = "nevertreated" (the default here and in
  # did::att_gt()), did halts with an opaque "The never-treated group is
  # too small to serve as a reliable control" error that names no numbers.
  # This is a genuine scope limit of the Callaway & Sant'Anna estimator --
  # not a defect in how searchnet_causal_panel() builds the panel, which is
  # correctly balanced and correctly encodes first_treat = 0 for controls.
  # Surface the actual requirement and observed counts here, before handing
  # off to did, instead of letting its unexplained message pass through.
  dots          <- list(...)
  control_group <- if (!is.null(dots$control_group)) dots$control_group[[1]] else "nevertreated"
  n_covariates  <- if (!is.null(dots$xformla)) length(all.vars(dots$xformla)) else 0L
  min_group_n   <- n_covariates + 5L

  group_units <- unique(panel[, c("actor_id", "first_treat")])
  group_sizes <- table(group_units$first_treat)
  control_n   <- if ("0" %in% names(group_sizes)) as.integer(group_sizes[["0"]]) else 0L
  n_total     <- length(unique(panel$actor_id))

  if (identical(control_group, "nevertreated") && control_n > 0 &&
      control_n < min_group_n) {
    stop(sprintf(paste0(
      "searchnet_did(): the never-treated (control) group has only %d ",
      "actor(s), but did::att_gt() requires at least %d units per ",
      "first_treat group (5 baseline, +1 per covariate in `xformla`) ",
      "before it will treat a group as a reliable comparison; below that ",
      "it refuses to estimate rather than return an unreliable ATT. This ",
      "is a scope limit of the design (%d actor(s) total, %d never-",
      "treated), not a bug in searchnet_causal_panel()'s panel. Either: ",
      "(1) widen the design so at least %d actors are never-treated ",
      "controls (fewer treated_actors and/or a larger `env`), or (2) if ",
      "treatment timing is staggered across actors, call searchnet_did() ",
      "with control_group = 'notyettreated' so not-yet-treated actors can ",
      "serve as controls instead of requiring a never-treated group."),
      control_n, min_group_n, n_total, control_n, min_group_n),
      call. = FALSE)
  }

  # Ensure actor_id is numeric for did::att_gt
  panel$actor_id_num <- as.integer(panel$actor_id)

  att <- did::att_gt(
    yname  = "outcome",
    tname  = "step",
    idname = "actor_id_num",
    gname  = "first_treat",
    data   = panel,
    ...
  )

  att
}


# ============================================================================
#  searchnet_synth
# ============================================================================

#' Run Synthetic Control on searchnet simulation
#'
#' Uses the \pkg{Synth} package (Abadie, Diamond & Hainmueller 2010) for
#' single-unit counterfactual estimation.  Ideal for \code{theta_shocks}
#' affecting a single actor or a small treatment group: the synthetic control
#' is a data-driven weighted combination of control actors that best
#' reproduces the treated actor's pre-treatment trajectory.
#'
#' @param panel A \code{data.frame} returned by
#'   \code{\link{searchnet_causal_panel}}.  Must contain both treated and
#'   control actors.
#' @param treated_unit The \code{actor_id} (as it appears in the panel) of
#'   the single treated unit for which to construct a synthetic control.
#' @param predictors Integer vector of pre-treatment steps to use as
#'   matching predictors.  If \code{NULL} (default), all pre-treatment steps
#'   are used.
#' @param \dots Additional arguments passed to \code{Synth::synth()}.
#' @return A list with components:
#'   \describe{
#'     \item{\code{synth_out}}{The raw \code{Synth::synth()} output.}
#'     \item{\code{dataprep_out}}{The \code{Synth::dataprep()} output.}
#'     \item{\code{gap}}{A \code{data.frame} with columns \code{step},
#'       \code{treated}, \code{synthetic}, and \code{gap} for plotting.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' panel <- searchnet_causal_panel(env, shock_step = 60,
#'                                  treated_actors = c(1))
#' sc <- searchnet_synth(panel, treated_unit = 1)
#' plot(sc$gap$step, sc$gap$gap, type = "l",
#'      xlab = "Step", ylab = "Gap (Treated - Synthetic)")
#' abline(v = 60, lty = 2)
#' }
searchnet_synth <- function(panel, treated_unit, predictors = NULL, ...) {
  if (!requireNamespace("Synth", quietly = TRUE))
    stop("Package 'Synth' is required for synthetic control analysis.\n",
         "Install with: install.packages('Synth')")

  required_cols <- c("actor_id", "step", "outcome", "treated", "shock_step")
  missing <- setdiff(required_cols, names(panel))
  if (length(missing) > 0)
    stop("Panel missing required columns: ",
         paste(missing, collapse = ", "),
         "\nUse searchnet_causal_panel() to create the panel.")

  shock_step <- panel$shock_step[1]

  # Ensure actor_id is numeric
  panel$actor_id_num <- as.integer(panel$actor_id)
  treated_num <- as.integer(factor(treated_unit,
                                   levels = levels(panel$actor_id)))

  control_ids <- unique(panel$actor_id_num[panel$treated == 0L])
  if (length(control_ids) < 2)
    stop("Synthetic control requires at least 2 control units.")

  all_steps   <- sort(unique(panel$step))
  pre_steps   <- all_steps[all_steps < shock_step]
  post_steps  <- all_steps[all_steps >= shock_step]

  if (is.null(predictors)) predictors <- pre_steps
  predictors <- intersect(predictors, pre_steps)

  if (length(predictors) < 2)
    stop("Need at least 2 pre-treatment steps as predictors.")

  ## Drop any pre-treatment step whose outcome is IDENTICAL across every
  ## control unit. Synth::dataprep()/synth() need cross-sectional variance in
  ## each predictor to fit the donor weights; a zero-variance predictor stops
  ## the whole call with "At least one predictor in X0 has no variation across
  ## control units", naming none of the offending steps.
  ##
  ## This became reachable after the v0.9.0 theta-storage repair: XWX/cycle4
  ## coefficients now actually drive the simulation (previously they simulated
  ## at 0 regardless of what was declared), and a genuinely coupled process can
  ## legitimately pin every control actor to the same value at an early step --
  ## that is a real property of the DGP, not a data error, and the fix is to
  ## drop that step as a predictor and say so, never to fail opaquely or to
  ## silently proceed with a predictor Synth cannot use.
  control_rows <- panel[panel$actor_id_num %in% control_ids, , drop = FALSE]
  degenerate <- vapply(predictors, function(s) {
    v <- control_rows$outcome[control_rows$step == s]
    length(v) > 0 && stats::sd(v, na.rm = TRUE) %in% c(0, NA)
  }, logical(1))

  if (any(degenerate)) {
    warning(sprintf(paste0("searchnet_synth(): dropping pre-treatment step(s) ",
                          "%s as predictor(s): the outcome is identical across ",
                          "every control unit at that step, which Synth cannot ",
                          "use to fit donor weights. This can be a genuine ",
                          "property of a strongly coupled process, not a data ",
                          "error."), paste(predictors[degenerate], collapse = ", ")),
            call. = FALSE)
    predictors <- predictors[!degenerate]
  }
  if (length(predictors) < 2)
    stop("Fewer than 2 usable pre-treatment predictors remain after dropping ",
         "step(s) with no cross-sectional variation among control units. ",
         "Supply more pre-treatment steps or a different `predictors` set.",
         call. = FALSE)

  # Build predictor specification for Synth::dataprep
  # Each pre-treatment step becomes a predictor via special.predictors
  special_preds <- lapply(predictors, function(s) {
    list("outcome", s, "mean")
  })
  names(special_preds) <- paste0("step_", predictors)

  dataprep_out <- Synth::dataprep(
    foo                  = panel,
    predictors           = NULL,
    predictors.op        = "mean",
    special.predictors   = special_preds,
    dependent            = "outcome",
    unit.variable        = "actor_id_num",
    time.variable        = "step",
    treatment.identifier = treated_num,
    controls.identifier  = control_ids,
    time.predictors.prior = pre_steps,
    time.optimize.ssr    = pre_steps,
    time.plot            = all_steps
  )

  synth_out <- Synth::synth(data.prep.obj = dataprep_out, ...)

  # Construct gap data frame for easy plotting
  treated_vals   <- dataprep_out$Y1plot
  synthetic_vals <- dataprep_out$Y0plot %*% synth_out$solution.w

  gap_df <- data.frame(
    step      = as.integer(rownames(treated_vals)),
    treated   = as.numeric(treated_vals),
    synthetic = as.numeric(synthetic_vals),
    gap       = as.numeric(treated_vals - synthetic_vals)
  )

  list(
    synth_out    = synth_out,
    dataprep_out = dataprep_out,
    gap          = gap_df
  )
}


# ============================================================================
#  searchnet_rd
# ============================================================================

#' Run Regression Discontinuity on searchnet simulation
#'
#' Uses \pkg{rdrobust} (Calonico, Cattaneo & Titiunik 2014) for sharp
#' regression discontinuity estimation at the shock threshold.  The running
#' variable is the simulation step; the cutoff is the \code{shock_step}.
#'
#' The function aggregates the panel to the mean outcome per step (averaging
#' across actors) before fitting the RD, which treats the shock as a sharp
#' discontinuity in the time series.
#'
#' @param panel A \code{data.frame} returned by
#'   \code{\link{searchnet_causal_panel}}.
#' @param \dots Additional arguments passed to \code{rdrobust::rdrobust()}.
#' @return A list with components:
#'   \describe{
#'     \item{\code{rd}}{The \code{rdrobust} object with estimates and
#'       inference.}
#'     \item{\code{agg_data}}{The step-level aggregated data used for
#'       estimation, with columns \code{step} and \code{mean_outcome}.}
#'     \item{\code{shock_step}}{Integer. The cutoff used.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' panel <- searchnet_causal_panel(env, shock_step = 60)
#' rd_result <- searchnet_rd(panel)
#' summary(rd_result$rd)
#' }
searchnet_rd <- function(panel, ...) {
  if (!requireNamespace("rdrobust", quietly = TRUE))
    stop("Package 'rdrobust' is required for RD analysis.\n",
         "Install with: install.packages('rdrobust')")

  required_cols <- c("step", "outcome", "shock_step")
  missing <- setdiff(required_cols, names(panel))
  if (length(missing) > 0)
    stop("Panel missing required columns: ",
         paste(missing, collapse = ", "),
         "\nUse searchnet_causal_panel() to create the panel.")

  shock_step <- panel$shock_step[1]

  # Aggregate to mean outcome per step
  agg <- stats::aggregate(outcome ~ step, data = panel, FUN = mean)
  names(agg) <- c("step", "mean_outcome")

  rd_out <- rdrobust::rdrobust(
    y = agg$mean_outcome,
    x = agg$step,
    c = shock_step,
    ...
  )

  list(
    rd        = rd_out,
    agg_data  = agg,
    shock_step = shock_step,
    design    = "time"
  )
}


# ============================================================================
#  searchnet_rd_cross
# ============================================================================

#' Cross-sectional regression discontinuity at an eligibility threshold
#'
#' Sharp RD in an \emph{actor characteristic} rather than in time.  Where
#' \code{\link{searchnet_rd}} treats the shock step as a cutoff in the running
#' variable "simulation step" (an interrupted-time-series design), this
#' function compares actors just above versus just below a threshold on a
#' running variable such as scope (\eqn{K_{AC}}), degree, or any actor
#' covariate.  This is the design used for eligibility rules: subsidy
#' thresholds, grant paylines, size-based regulation, index inclusion.
#'
#' Assignment near the cutoff is as-good-as-random when actors cannot
#' precisely manipulate the running variable, so the discontinuity in
#' outcomes at the threshold identifies a local average treatment effect.
#'
#' @details
#' The design requires a shock that applies \emph{only} to actors on one side
#' of the cutoff.  Construct it by running the simulation with
#' \code{treated_actors} set to those actors, or by supplying a pre-shock
#' running variable and a post-shock outcome.
#'
#' Two forms are supported:
#' \describe{
#'   \item{Sharp}{Treatment is a deterministic function of the running
#'     variable (all actors above the cutoff are treated). The default.}
#'   \item{Fuzzy}{Treatment probability jumps at the cutoff but is not
#'     deterministic; supply \code{treatment} and the estimator uses
#'     \code{rdrobust}'s fuzzy option, returning a local IV estimate.}
#' }
#'
#' @param panel A \code{data.frame} from \code{\link{searchnet_causal_panel}},
#'   or any actor-level data with one row per actor.
#' @param running Character or numeric vector. The running (forcing) variable.
#'   If character, the name of a column in \code{panel}; if numeric, the values
#'   themselves (length must equal the number of actors).
#' @param cutoff Numeric. The eligibility threshold.
#' @param outcome Character. Name of the outcome column. Default
#'   \code{"outcome"}.
#' @param post_step Integer or \code{NULL}. If the panel is actor-by-step,
#'   the step at which to measure the outcome (defaults to the last step).
#'   Ignored if the panel already has one row per actor.
#' @param treatment Character or \code{NULL}. Name of a 0/1 treatment column.
#'   Supplying it triggers a fuzzy RD.
#' @param \dots Additional arguments passed to \code{rdrobust::rdrobust()},
#'   e.g. \code{p} (polynomial order), \code{kernel}, \code{bwselect}.
#'
#' @return A list with
#'   \describe{
#'     \item{\code{rd}}{The \code{rdrobust} object.}
#'     \item{\code{data}}{The actor-level frame used, with columns
#'       \code{actor_id}, \code{running}, \code{outcome}, and (if fuzzy)
#'       \code{treatment}.}
#'     \item{\code{cutoff}}{The threshold used.}
#'     \item{\code{design}}{\code{"cross-section"}.}
#'     \item{\code{fuzzy}}{Logical.}
#'   }
#'
#' @references
#' Calonico, S., Cattaneo, M. D., & Titiunik, R. (2014). Robust nonparametric
#' confidence intervals for regression-discontinuity designs.
#' \emph{Econometrica}, \bold{82}(6), 2295--2326.
#'
#' Lee, D. S., & Lemieux, T. (2010). Regression discontinuity designs in
#' economics. \emph{Journal of Economic Literature}, \bold{48}(2), 281--355.
#'
#' @seealso \code{\link{searchnet_rd}} for the time-based (interrupted
#'   time series) design.
#'
#' @examples
#' \dontrun{
#' # Subsidy available only to actors with scope >= 5
#' env <- saomnk_env(M = 40, N = 12, seed = 1)
#' mod <- saomnk_model(density = -0.5, influence_matrix = saomnk_block_diagonal(12, 3))
#' saomnk_run(env, mod, steps_per_actor = 30, seed = 42)
#'
#' scope   <- saomnk_get_degrees(env)$K_AC          # running variable
#' treated <- which(scope >= 5)
#' panel   <- searchnet_causal_panel(env, shock_step = 15, treated_actors = treated)
#'
#' rd <- searchnet_rd_cross(panel, running = scope, cutoff = 5)
#' summary(rd$rd)
#' }
#'
#' @export
searchnet_rd_cross <- function(panel, running, cutoff,
                               outcome = "outcome",
                               post_step = NULL,
                               treatment = NULL, ...) {
  if (!requireNamespace("rdrobust", quietly = TRUE))
    stop("Package 'rdrobust' is required for RD analysis.\n",
         "Install with: install.packages('rdrobust')")

  if (!is.data.frame(panel))
    stop("`panel` must be a data.frame.")
  if (!outcome %in% names(panel))
    stop("Outcome column '", outcome, "' not found in `panel`.")

  # ---- collapse actor x step panel to one row per actor -------------------
  df <- panel
  if ("step" %in% names(df) && "actor_id" %in% names(df)) {
    target_step <- if (is.null(post_step)) max(df$step, na.rm = TRUE) else post_step
    if (!target_step %in% df$step)
      stop("post_step = ", target_step, " not present in panel$step.")
    df <- df[df$step == target_step, , drop = FALSE]
  }
  if (anyDuplicated(df$actor_id))
    stop("Panel has multiple rows per actor after collapsing; ",
         "supply `post_step` to select a single step.")

  n_actors <- nrow(df)

  # ---- resolve the running variable ---------------------------------------
  if (is.character(running) && length(running) == 1L) {
    if (!running %in% names(df))
      stop("Running variable column '", running, "' not found in `panel`.")
    run_vals <- df[[running]]
    run_name <- running
  } else if (is.numeric(running)) {
    if (length(running) != n_actors)
      stop("`running` has length ", length(running), " but there are ",
           n_actors, " actors.")
    run_vals <- running
    run_name <- "running"
  } else {
    stop("`running` must be a column name or a numeric vector.")
  }

  if (cutoff <= min(run_vals, na.rm = TRUE) ||
      cutoff >= max(run_vals, na.rm = TRUE))
    stop("`cutoff` (", cutoff, ") lies outside the range of the running ",
         "variable [", min(run_vals, na.rm = TRUE), ", ",
         max(run_vals, na.rm = TRUE), "]; no discontinuity can be estimated.")

  n_below <- sum(run_vals < cutoff, na.rm = TRUE)
  n_above <- sum(run_vals >= cutoff, na.rm = TRUE)
  if (n_below < 5L || n_above < 5L)
    warning("Sparse support around the cutoff (", n_below, " below, ",
            n_above, " above). RD estimates will be unstable.")

  y <- df[[outcome]]
  out_df <- data.frame(
    actor_id = df$actor_id,
    running  = run_vals,
    outcome  = y,
    stringsAsFactors = FALSE
  )

  # ---- sharp or fuzzy ------------------------------------------------------
  fuzzy <- !is.null(treatment)
  if (fuzzy) {
    if (!treatment %in% names(df))
      stop("Treatment column '", treatment, "' not found in `panel`.")
    tvals <- df[[treatment]]
    out_df$treatment <- tvals
    rd_out <- rdrobust::rdrobust(y = y, x = run_vals, c = cutoff,
                                 fuzzy = tvals, ...)
  } else {
    rd_out <- rdrobust::rdrobust(y = y, x = run_vals, c = cutoff, ...)
  }

  list(
    rd            = rd_out,
    data          = out_df,
    cutoff        = cutoff,
    running_name  = run_name,
    design        = "cross-section",
    fuzzy         = fuzzy,
    n_below       = n_below,
    n_above       = n_above
  )
}


# ============================================================================
#  searchnet_causal_plot
# ============================================================================

#' Plot causal inference results from searchnet
#'
#' Unified plotting interface for DID, synthetic control, and RD results
#' produced by \code{\link{searchnet_did}}, \code{\link{searchnet_synth}},
#' and \code{\link{searchnet_rd}}.
#'
#' Each plot states its estimate in the title and says how to read the
#' figure in the subtitle; the shock is a dashed vermillion line.
#' \describe{
#'   \item{\code{"did"}}{The event-study aggregation of the group-time
#'     effects (\code{did::aggte(type = "dynamic")}): one point per ministep
#'     relative to the shock, with its simultaneous confidence band, grey
#'     before the shock and vermillion after. The title reports the simple
#'     aggregate ATT and its standard error.}
#'   \item{\code{"synth"}}{The treated unit (solid, vermillion) against its
#'     synthetic control (dashed, grey), labeled at the line ends. The title
#'     reports the mean post-shock gap.}
#'   \item{\code{"rd"}}{Step means with a local fit on each side of the shock
#'     step. The title reports the RD estimate and p-value.}
#' }
#'
#' @param result Output from \code{\link{searchnet_did}},
#'   \code{\link{searchnet_synth}}, or \code{\link{searchnet_rd}}.
#' @param type Character. Auto-detected from the result's class or
#'   structure.  Can be overridden to one of \code{"did"}, \code{"synth"},
#'   or \code{"rd"}.
#' @param annotate Logical. If \code{TRUE} (default), add reading guides on
#'   the data: the shock labeled, and for \code{"did"} a note on the
#'   pre-shock estimates (whether their bands cover zero) and on the average
#'   effect after the shock. \code{FALSE} omits them.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' \dontrun{
#' # DID event study
#' att <- searchnet_did(panel)
#' searchnet_causal_plot(att, type = "did")
#'
#' # Synthetic control gap
#' sc <- searchnet_synth(panel, treated_unit = 1)
#' searchnet_causal_plot(sc, type = "synth")
#'
#' # RD plot
#' rd_result <- searchnet_rd(panel)
#' searchnet_causal_plot(rd_result, type = "rd")
#' }
searchnet_causal_plot <- function(result, type = NULL, annotate = TRUE) {

  # -------------------------------------------------------------------
  #  Auto-detect type
  # -------------------------------------------------------------------
  if (is.null(type)) {
    if (inherits(result, "MP")) {
      type <- "did"
    } else if (is.list(result) && "synth_out" %in% names(result)) {
      type <- "synth"
    } else if (is.list(result) && "rd" %in% names(result)) {
      type <- "rd"
    } else {
      stop("Cannot auto-detect result type. Specify type = 'did', ",
           "'synth', or 'rd'.")
    }
  }
  type <- match.arg(type, choices = c("did", "synth", "rd"))
  ev  <- .sn_role[["event"]]
  ink <- .sn_role[["ink"]]
  x_lab <- "Ministep (one decision opportunity)"

  # -------------------------------------------------------------------
  #  DID event study plot
  # -------------------------------------------------------------------
  if (type == "did") {
    if (!requireNamespace("did", quietly = TRUE))
      stop("Package 'did' required for DID plotting.")
    agg <- did::aggte(result, type = "dynamic")
    simple <- tryCatch(did::aggte(result, type = "simple"), error = function(e) NULL)
    crit <- agg$crit.val.egt
    if (is.null(crit) || !is.finite(crit)) crit <- stats::qnorm(0.975)
    es <- data.frame(e = agg$egt, att = agg$att.egt, se = agg$se.egt)
    es$lo <- es$att - crit * es$se
    es$hi <- es$att + crit * es$se
    ## did reports NA standard errors for event times it cannot bootstrap
    ## (for example the reference period); draw those as points only.
    es <- es[is.finite(es$att), , drop = FALSE]
    es$lo[!is.finite(es$lo)] <- es$att[!is.finite(es$lo)]
    es$hi[!is.finite(es$hi)] <- es$att[!is.finite(es$hi)]
    es$when <- factor(ifelse(es$e < 0, "before", "after"), levels = c("before", "after"))
    cols <- c(before = "grey45", after = ev)

    ov <- if (!is.null(simple)) c(simple$overall.att, simple$overall.se) else
      c(agg$overall.att, agg$overall.se)
    sig <- is.finite(ov[2]) && abs(ov[1]) > stats::qnorm(0.975) * ov[2]
    title <- if (sig)
      sprintf("The shock %s the outcome by %.2f on average (SE %.2f)",
              if (ov[1] < 0) "lowers" else "raises", abs(ov[1]), ov[2]) else
      sprintf("No clear average effect of the shock (ATT %.2f, SE %.2f)", ov[1], ov[2])
    many <- nrow(es) > 40
    p <- ggplot2::ggplot(es, ggplot2::aes(.data$e, .data$att, color = .data$when,
                                          fill = .data$when)) +
      ggplot2::geom_hline(yintercept = 0, color = "grey55", linewidth = 0.4) +
      ggplot2::geom_vline(xintercept = -0.5, color = ev, linetype = "dashed",
                          linewidth = 0.6)
    p <- if (many) {
      p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi,
                                            group = .data$when),
                               color = NA, alpha = 0.18) +
        ggplot2::geom_line(ggplot2::aes(group = .data$when), linewidth = 0.5) +
        ggplot2::geom_point(size = 0.9)
    } else {
      p + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
                                 width = 0, linewidth = 0.5) +
        ggplot2::geom_point(size = 1.8)
    }
    p <- p +
      ggplot2::scale_color_manual(values = cols, guide = "none") +
      ggplot2::scale_fill_manual(values = cols, guide = "none") +
      ggplot2::labs(
        title = title,
        subtitle = paste0(
          "Effect on treated actors at each ministep before and after the shock ",
          "(difference-in-differences).\nPoints: estimates; band: 95% simultaneous ",
          "confidence; grey line: no effect."),
        x = "Ministeps relative to the shock", y = "Effect on the outcome (ATT)") +
      theme_searchnet()

    if (isTRUE(annotate)) {
      pre <- es[es$when == "before", ]
      post <- es[es$when == "after", ]
      y_rng <- range(c(es$lo, es$hi, 0), na.rm = TRUE)
      dy <- diff(y_rng)
      p <- p + ggplot2::annotate("text", x = -0.5, y = y_rng[2], label = " shock",
                                 hjust = 0, vjust = 1, color = ev, size = 3.2,
                                 fontface = "bold")
      if (nrow(pre)) {
        cover <- mean(pre$lo <= 0 & pre$hi >= 0, na.rm = TRUE)
        xs <- stats::median(pre$e)
        ## The plot extends furthest on the side the effect goes, and the
        ## pre-shock half of that side is empty: put the note there.
        up <- !(mean(post$att, na.rm = TRUE) < 0)
        ## Text starts just inside the average line (or 30% of the range
        ## out when there is no average line) and grows toward zero; the
        ## arrow runs beside it, from the text's far end up to zero.
        y0 <- if (sig && sign(ov[1]) == (if (up) 1 else -1))
          ov[1] - sign(ov[1]) * 0.06 * dy else (if (up) 0.3 else -0.3) * dy
        xa <- xs - 0.02 * diff(range(es$e))
        p <- p + ggplot2::annotate(
          "text", x = xs, y = y0, hjust = 0, vjust = if (up) 1 else 0,
          size = 3, color = ink, lineheight = 0.95,
          label = if (cover >= 0.95)
            "before the shock: estimates sit on zero,\nas they should (no pre-trend)" else
            sprintf("before the shock: %.0f%% of bands cover zero;\ncheck for a pre-trend",
                    100 * cover)) +
          ggplot2::annotate("segment", x = xa, xend = xa, y = y0,
                            yend = if (up) 0.04 * dy else -0.04 * dy,
                            color = ink, linewidth = 0.35,
                            arrow = grid::arrow(length = grid::unit(0.07, "in"),
                                                type = "closed"))
      }
      if (nrow(post) && sig) {
        p <- p + ggplot2::geom_hline(yintercept = ov[1], color = ev, linetype = "dotted",
                                     linewidth = 0.5) +
          ggplot2::annotate("text", x = min(es$e), y = ov[1],
                            label = sprintf("average after the shock: %.2f", ov[1]),
                            hjust = 0, vjust = if (ov[1] < 0) 1.6 else -0.6, size = 3,
                            color = ev)
      }
    }
    return(p)
  }

  # -------------------------------------------------------------------
  #  Synthetic control: treated vs synthetic
  # -------------------------------------------------------------------
  if (type == "synth") {
    gap <- result$gap
    if ("shock_step" %in% names(result))
      shock <- result$shock_step
    else
      shock <- result$dataprep_out$tag$time.optimize.ssr[length(
        result$dataprep_out$tag$time.optimize.ssr)] + 1L

    plot_df <- data.frame(
      step  = rep(gap$step, 2),
      value = c(gap$treated, gap$synthetic),
      series = factor(rep(c("Treated", "Synthetic Control"), each = nrow(gap)),
                      levels = c("Treated", "Synthetic Control"))
    )
    post_gap <- mean(gap$gap[gap$step >= shock])
    title <- sprintf("After the shock the treated unit runs %.2f %s its synthetic control",
                     abs(post_gap), if (post_gap < 0) "below" else "above")
    ends <- plot_df[plot_df$step == max(plot_df$step), ]
    ends$label <- c("treated", "synthetic control")[as.integer(ends$series)]

    p <- ggplot2::ggplot(plot_df,
                         ggplot2::aes(x = .data$step, y = .data$value,
                                      color = .data$series, linetype = .data$series)) +
      ggplot2::geom_vline(xintercept = shock, linetype = "dashed", color = ev,
                          linewidth = 0.6) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_text(data = ends, ggplot2::aes(label = .data$label),
                         hjust = -0.08, size = 3.2, show.legend = FALSE) +
      ggplot2::scale_color_manual(values = c("Treated" = ev,
                                             "Synthetic Control" = "grey35"),
                                  guide = "none") +
      ggplot2::scale_linetype_manual(values = c("Treated" = "solid",
                                                "Synthetic Control" = "dashed"),
                                     guide = "none") +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.2))) +
      ggplot2::labs(x = x_lab, y = "Outcome", title = title,
                    subtitle = paste0("Synthetic control: a weighted mix of untreated units ",
                                      "fit to the treated unit before the shock.\n",
                                      "The gap after the dashed line is the estimated effect.")) +
      theme_searchnet()
    if (isTRUE(annotate))
      p <- p + ggplot2::annotate("text", x = shock, y = -Inf,
                                 label = "shock ", hjust = 1, vjust = -0.6, color = ev,
                                 size = 3.2, fontface = "bold")
    return(p)
  }

  # -------------------------------------------------------------------
  #  RD plot
  # -------------------------------------------------------------------
  if (type == "rd") {
    agg  <- result$agg_data
    if (is.null(agg))
      stop("type = \"rd\" plots the time design of searchnet_rd(); this result ",
           "has no agg_data.")
    cutoff <- result$shock_step
    rd_obj <- result$rd
    est  <- rd_obj$coef[1]
    pval <- rd_obj$pv[1]

    p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data$step, y = .data$mean_outcome)) +
      ggplot2::geom_vline(xintercept = cutoff, linetype = "dashed", color = ev,
                          linewidth = 0.6) +
      ggplot2::geom_point(alpha = 0.5, size = 1.3, color = "grey40") +
      ggplot2::geom_smooth(data = agg[agg$step < cutoff, ], method = "loess",
                           formula = y ~ x, se = TRUE, color = "grey25",
                           fill = "grey60", alpha = 0.25) +
      ggplot2::geom_smooth(data = agg[agg$step >= cutoff, ], method = "loess",
                           formula = y ~ x, se = TRUE, color = ev, fill = ev,
                           alpha = 0.15) +
      ggplot2::labs(x = x_lab, y = "Mean outcome",
                    title = sprintf("The outcome %s by %.2f at the shock (RD estimate, %s)",
                                    if (est < 0) "drops" else "jumps", abs(est),
                                    if (is.finite(pval) && pval < 0.001) "p < 0.001" else
                                      sprintf("p = %.3f", pval)),
                    subtitle = paste0("Points: mean outcome at each ministep.\n",
                                      "Lines: fits before (grey) and after (vermillion); ",
                                      "the estimate is the jump at the dashed line.")) +
      theme_searchnet()
    if (isTRUE(annotate))
      p <- p + ggplot2::annotate("text", x = cutoff,
                                 y = max(agg$mean_outcome, na.rm = TRUE),
                                 label = " shock", hjust = 0, vjust = 1, color = ev,
                                 size = 3.2, fontface = "bold")
    return(p)
  }
}
