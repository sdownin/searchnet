###############################################################################
## searchnet-moment-gate.R
##
## searchnet_moment_gate(): does a simulated world reproduce the observed
## network's moments BEFORE anything is read off it?
##
## Two rules are built in, both learned from simulated worlds that looked
## calibrated and were not:
##
##   1. Judge by the MEAN. A world "reproduces" a moment when the mean over
##      replications is within a stated tolerance of the observed value. Whether
##      the observed value falls inside the replications' dispersion band is
##      not the test: noisy worlds have wide bands and pass trivially. The
##      replication SD and the Monte Carlo SE are reported beside the mean so
##      the reader can see how well the mean is resolved, but neither widens
##      the tolerance.
##   2. Refuse worlds parameterized from RSiena's default starting values.
##      getNetworkStartingVals() floors the tie-creation proportion and trims
##      the density start for numerical safety; those values are not
##      calibrated, and a sparse network simulated from them comes out several
##      times too dense.
##
## The tolerance has no default: it is stated before the simulation is seen.
## A failed gate stops by default (on_fail = "stop").
###############################################################################


## Moments the gate can compute from matrices. State moments are averaged over
## the evolved periods (every period after the first when there are two or
## more); change moments over consecutive-period transitions.
.MG_STATE_MOMENTS  <- c("density", "mean_K_AC", "mean_K_CA", "mean_K_AA",
                        "mean_K_CC", "sd_K_AC", "sd_K_CA")
.MG_CHANGE_MOMENTS <- c("change_rate", "tie_jaccard")
.MG_MOMENTS        <- c(.MG_STATE_MOMENTS, .MG_CHANGE_MOMENTS)


# --------------------------------------------------------------------------- #
#  internal helpers: panels and their moments
# --------------------------------------------------------------------------- #

## Moments of one panel: a list of T >= 1 M x N 0/1 matrices in period order.
## Returns a named numeric vector over .MG_MOMENTS with attribute "dims"
## (M, N, T). Change moments are NA for a one-period panel.
.mg_panel_moments <- function(panel) {
  T_ <- length(panel)
  M <- nrow(panel[[1L]]); N <- ncol(panel[[1L]])
  arr <- array(unlist(lapply(panel, function(m) as.integer(as.matrix(m)))),
               dim = c(M, N, T_))
  k <- searchnet_k_readings(arr)
  s <- attr(k, "summary")
  per <- if (T_ >= 2L) seq_len(T_)[-1L] else 1L
  sd_by <- function(col, lvl) {
    vapply(per, function(w) {
      v <- k[[col]][k$level == lvl & k$period == s$period[w]]
      if (length(v) < 2L) NA_real_ else stats::sd(v)
    }, numeric(1))
  }
  chg <- NA_real_; tj <- NA_real_
  if (T_ >= 2L) {
    chg <- mean(vapply(2:T_, function(w)
      sum(arr[, , w] != arr[, , w - 1L]) / (M * N), numeric(1)))
    tj <- mean(s$tie_jaccard[2:T_], na.rm = TRUE)
    if (is.nan(tj)) tj <- NA_real_
  }
  out <- c(density   = mean(s$density[per]),
           mean_K_AC = mean(s$mean_K_AC[per]),
           mean_K_CA = mean(s$mean_K_CA[per]),
           mean_K_AA = mean(s$mean_K_AA[per]),
           mean_K_CC = mean(s$mean_K_CC[per]),
           sd_K_AC   = mean(sd_by("K_AC", "actor")),
           sd_K_CA   = mean(sd_by("K_CA", "component")),
           change_rate = chg,
           tie_jaccard = tj)
  attr(out, "dims") <- c(M = M, N = N, T = T_)
  out
}

## A matrix-like input (matrix, 3-d array, list of matrices,
## searchnet_bipartite) as a list of matrices in period order.
.mg_as_panel <- function(x, arg) {
  if (inherits(x, "searchnet_bipartite")) x <- x$B
  if (is.matrix(x) && !is.data.frame(x)) return(list(x))
  if (is.array(x) && length(dim(x)) == 3L)
    return(lapply(seq_len(dim(x)[3L]), function(w) matrix(x[, , w], dim(x)[1L], dim(x)[2L])))
  if (is.list(x) && length(x) && all(vapply(x, is.matrix, logical(1))))
    return(lapply(x, as.matrix))
  stop("`", arg, "` must be an M x N matrix, an M x N x W array, a list of ",
       "M x N matrices, or a searchnet_bipartite object.", call. = FALSE)
}

## The start matrix of an environment's path.
.mg_env_start <- function(env) {
  w1 <- if (length(env$rsiena_model_waves)) env$rsiena_model_waves[[1L]] else NULL
  if (!is.null(w1) && !is.null(w1$searchnet_start)) return(w1$searchnet_start)
  if (!is.null(env$path_start_matrix)) return(env$path_start_matrix)
  env$bipartite_matrix_init
}

## One environment as a list of replication panels. A Monte Carlo run
## (non-empty env$mc_results) contributes one panel per replication, each
## starting from env$bipartite_matrix (the state the replications started
## from); otherwise the environment's own path is one panel: its start, then
## each multiwave wave, or the final state of a single run.
.mg_env_panels <- function(env) {
  mc <- env$mc_results
  if (length(mc)) {
    start <- env$bipartite_matrix
    return(lapply(mc, function(r) {
      if (!length(r$bipartite_waves))
        stop("A Monte Carlo replication in `simulated` stored no waves.",
             call. = FALSE)
      c(list(start), r$bipartite_waves)
    }))
  }
  start <- .mg_env_start(env)
  waves <- env$bipartite_matrix_waves
  if (length(waves)) return(list(c(list(start), waves)))
  if (is.null(env$bipartite_matrix) || is.null(start))
    stop("An environment in `simulated` has not been run: run saomnk_run(), ",
         "a multiwave run or saomnk_monte_carlo() first.", call. = FALSE)
  list(list(start, env$bipartite_matrix))
}

## Read a precomputed moment table. Wide: one row per replication (or one row
## for observed) with moment-named columns, optional `rep`. Long: columns
## `moment` and `value`, optional `rep`. A named numeric vector is one row.
.mg_table_moments <- function(x, arg) {
  if (is.numeric(x) && !is.null(names(x)) && is.null(dim(x)))
    x <- as.data.frame(as.list(x))
  if (!is.data.frame(x))
    stop("`", arg, "` is not a recognized moment table.", call. = FALSE)
  if (all(c("moment", "value") %in% names(x))) {
    rep_id <- if ("rep" %in% names(x)) x$rep else rep(1L, nrow(x))
    if (anyDuplicated(paste(rep_id, x$moment)))
      stop("`", arg, "` (long moment table) repeats a moment within a ",
           "replication; add a `rep` column.", call. = FALSE)
    reps <- unique(rep_id); mom <- unique(as.character(x$moment))
    out <- matrix(NA_real_, length(reps), length(mom),
                  dimnames = list(as.character(reps), mom))
    out[cbind(match(rep_id, reps), match(as.character(x$moment), mom))] <-
      as.numeric(x$value)
    return(out)
  }
  cols <- setdiff(names(x), "rep")
  bad <- cols[!vapply(x[cols], is.numeric, logical(1))]
  if (length(bad))
    stop("`", arg, "` (wide moment table) has non-numeric column(s): ",
         paste(bad, collapse = ", "), ".", call. = FALSE)
  out <- as.matrix(x[cols]); storage.mode(out) <- "double"
  rownames(out) <- if ("rep" %in% names(x)) as.character(x$rep) else NULL
  out
}

.mg_is_env <- function(x) inherits(x, "SaomNkRSienaBiEnv")

## Is this object a precomputed moment table (rather than matrices)?
.mg_is_table <- function(x) {
  is.data.frame(x) || (is.numeric(x) && is.null(dim(x)) && !is.null(names(x)))
}

## theta_source declared on an object: attribute, or env$provenance$theta_source.
.mg_declared_source <- function(x) {
  a <- attr(x, "theta_source", exact = TRUE)
  if (!is.null(a)) return(as.character(a)[1L])
  if (.mg_is_env(x)) {
    p <- tryCatch(x$provenance, error = function(e) NULL)
    if (is.list(p) && !is.null(p$theta_source)) return(as.character(p$theta_source)[1L])
  }
  NULL
}

.mg_is_starting_values <- function(s) {
  if (is.null(s) || is.na(s)) return(FALSE)
  key <- gsub("[^a-z]", "", tolower(s))
  key %in% c("startingvalues", "startingvals", "startvalues", "startvals",
             "getnetworkstartingvals", "defaultstartingvalues", "rsienadefaults",
             "rsienastartingvalues")
}

## Parse the tolerance specification into a data frame (moment, tolerance,
## tol_type) covering exactly `moments`.
.mg_parse_tolerance <- function(tolerance, moments) {
  if (is.data.frame(tolerance)) {
    if (!all(c("moment", "tolerance") %in% names(tolerance)))
      stop("A tolerance data frame needs columns `moment` and `tolerance` ",
           "(and optionally `type`, \"abs\" or \"rel\").", call. = FALSE)
    ty <- if ("type" %in% names(tolerance)) as.character(tolerance$type)
          else rep("abs", nrow(tolerance))
    tolerance <- stats::setNames(
      lapply(seq_len(nrow(tolerance)), function(i)
        stats::setNames(as.numeric(tolerance$tolerance[i]), ty[i])),
      as.character(tolerance$moment))
  }
  if (is.numeric(tolerance)) tolerance <- as.list(tolerance)
  if (!is.list(tolerance) || is.null(names(tolerance)) ||
      any(!nzchar(names(tolerance))))
    stop("`tolerance` must be named by moment, e.g. ",
         "list(density = 0.01, sd_K_AC = c(rel = 0.15)).", call. = FALSE)
  if (anyDuplicated(names(tolerance)))
    stop("`tolerance` names a moment twice: ",
         paste(unique(names(tolerance)[duplicated(names(tolerance))]),
               collapse = ", "), ".", call. = FALSE)
  miss <- setdiff(moments, names(tolerance))
  if (length(miss))
    stop("`tolerance` has no entry for moment(s): ", paste(miss, collapse = ", "),
         ". Every gated moment needs a tolerance stated in advance.", call. = FALSE)
  extra <- setdiff(names(tolerance), moments)
  if (length(extra))
    stop("`tolerance` names moment(s) that are not gated: ",
         paste(extra, collapse = ", "), ". Add them to `moments` or remove ",
         "them (a misspelled name would otherwise go unchecked).", call. = FALSE)
  rows <- lapply(moments, function(m) {
    v <- tolerance[[m]]
    if (is.list(v)) v <- unlist(v)
    ty <- if (is.null(names(v)) || !nzchar(names(v)[1L])) "abs" else names(v)[1L]
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v <= 0)
      stop("The tolerance for '", m, "' must be one positive number, ",
           "absolute (0.01 or c(abs = 0.01)) or relative (c(rel = 0.1)).",
           call. = FALSE)
    if (!ty %in% c("abs", "rel"))
      stop("The tolerance for '", m, "' is labeled '", ty, "'; use \"abs\" ",
           "(absolute difference) or \"rel\" (difference relative to the ",
           "observed value).", call. = FALSE)
    data.frame(moment = m, tolerance = unname(as.numeric(v)), tol_type = ty,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}


# --------------------------------------------------------------------------- #
#  searchnet_moment_gate
# --------------------------------------------------------------------------- #

#' Gate a Simulated World on the Observed Network's Moments
#'
#' Checks, before any estimate or counterfactual is read off a simulated
#' SAOM-NK world, that the world reproduces the observed actor-by-component
#' network: its density, the mean and spread of the \{K\} degrees, and how
#' fast ties change. A world that misses these is not a model of the data, and
#' a test run in it answers a question about a different network.
#'
#' \strong{The verdict uses the simulated mean.} For each moment the gate
#' computes the mean over replications and passes the moment when
#' \eqn{|\bar{s} - o|}{|mean_sim - obs|} is within the stated tolerance
#' (absolute, or relative to \eqn{|o|}{|obs|}). The replication SD
#' (\code{sim_sd}) and the Monte Carlo standard error of the mean
#' (\code{mcse} \eqn{= \mathrm{sd}/\sqrt{R}}{= sd / sqrt(R)}) are reported
#' beside it, but they do not widen the tolerance. A criterion of the form
#' "the observed value lies within the simulated range" or "within 1.96
#' simulated SDs" is not used, because a world with wide replication-to-
#' replication dispersion passes it while its mean misses the data by a large
#' factor. The column \code{obs_in_sim_range} shows that band check for
#' information only; it does not enter the verdict. \code{mcse_resolved} is
#' \code{FALSE} when \eqn{2 \times}{2 x} MCSE exceeds the (absolute)
#' tolerance, a sign that more replications are needed before the verdict is
#' stable.
#'
#' \strong{Starting-value worlds are refused.} RSiena's
#' \code{getNetworkStartingVals()} floors the tie-creation proportion at 0.02
#' and trims the density start to \eqn{[-3, 3]}; these keep the estimator
#' numerically safe and are not calibrated parameters. A sparse network
#' simulated from them comes out several times too dense, and every gate or
#' test run in that world is run in the wrong world. When
#' \code{theta_source} is \code{"starting_values"}, or any simulated object
#' declares that provenance (an attribute \code{"theta_source"}, or
#' \code{env$provenance$theta_source} on an environment), the function errors
#' whatever \code{on_fail} says. Take rates and density from a fit of rates
#' and density only (no focal or structural effect) instead.
#'
#' \strong{Which periods are compared.} Each panel (the observed one and every
#' simulated replication) is read as a start followed by evolved periods. With
#' two or more periods, the state moments (\code{density}, the mean and SD
#' degrees) average over every period after the first, the ones a simulation
#' started from the first period has to reproduce; \code{change_rate} (share
#' of the \eqn{M \times N}{M x N} cells that change between consecutive
#' periods) and \code{tie_jaccard} average over transitions. A one-period
#' panel gives state moments for that period and no change moments. For an
#' environment the panel is its start matrix followed by each multiwave wave
#' (or the final state of a single \code{\link{saomnk_run}}); an environment
#' with Monte Carlo results (\code{\link{saomnk_monte_carlo}}) contributes one
#' panel per replication, each starting from \code{env$bipartite_matrix}.
#'
#' @param observed The observed network: an \eqn{M \times N}{M x N} 0/1
#'   matrix, an \eqn{M \times N \times W}{M x N x W} array, a list of
#'   matrices, or a \code{\link{searchnet_bipartite_from_long}} result. Or a
#'   precomputed moment table: a named numeric vector, a one-row data frame
#'   with moment-named columns, or a long data frame with columns
#'   \code{moment} and \code{value}.
#' @param simulated The simulated world: a \code{SaomNkRSienaBiEnv}
#'   environment, a list of environments (replications), a list of
#'   replication panels (each a matrix, 3-d array, list of matrices or
#'   \code{searchnet_bipartite}), or a moment table with one row per
#'   replication (wide, optional \code{rep} column) or long (\code{rep},
#'   \code{moment}, \code{value}).
#' @param moments Character vector of moments to gate. Available:
#'   \code{"density"}, \code{"mean_K_AC"}, \code{"mean_K_CA"},
#'   \code{"mean_K_AA"}, \code{"mean_K_CC"}, \code{"sd_K_AC"},
#'   \code{"sd_K_CA"}, \code{"change_rate"}, \code{"tie_jaccard"}. With moment
#'   tables, any name present in both tables may be used.
#' @param tolerance Required; no default. The largest acceptable difference of
#'   the simulated mean from the observed value, one entry per gated moment
#'   and named by it: a number or \code{c(abs = x)} for an absolute
#'   difference, \code{c(rel = x)} for a difference relative to the observed
#'   value. A list (\code{list(density = 0.005, sd_K_AC = c(rel = 0.15))}), a
#'   named numeric vector (all absolute), or a data frame with columns
#'   \code{moment}, \code{tolerance} and optionally \code{type}. State it
#'   before seeing the simulation.
#' @param on_fail What a failed gate does: \code{"stop"} (default) signals an
#'   error of class \code{"searchnet_moment_gate_error"} whose \code{gate}
#'   element holds the result; \code{"warn"} warns and returns; \code{"return"}
#'   returns silently. A starting-values refusal is an error in every mode.
#' @param theta_source Optional character label for where the world's
#'   parameters came from (for example \code{"rates_density_fit"},
#'   \code{"saom_fit"}, \code{"planted"}). Recorded in the provenance.
#'   \code{"starting_values"} is refused.
#'
#' @return An object of class \code{"searchnet_moment_gate"}: a list with
#'   \describe{
#'     \item{\code{table}}{a data frame with one row per moment: \code{moment},
#'       \code{observed}, \code{sim_mean}, \code{sim_sd}, \code{mcse},
#'       \code{diff} (\code{sim_mean - observed}), \code{rel_diff},
#'       \code{tolerance}, \code{tol_type}, \code{pass}, and the information
#'       columns \code{mcse_resolved} and \code{obs_in_sim_range};}
#'     \item{\code{pass}, \code{verdict}}{\code{TRUE}/\code{"PASS"} when every
#'       moment passes;}
#'     \item{\code{n_reps}}{the number of simulated replications;}
#'     \item{\code{sim_moments}}{the per-replication moments (replications by
#'       moments);}
#'     \item{\code{provenance}}{\code{theta_source} and where it was declared,
#'       the panel dimensions, the searchnet and R versions, a timestamp and
#'       the call.}
#'   }
#'
#' @seealso \code{\link{searchnet_k_readings}} (the observed moments),
#'   \code{\link{saom_to_saomnk}}, \code{\link{run_calibrated_counterfactual}},
#'   and \code{\link{gof_battery}}, which asks a different question (whether
#'   a fitted SAOM reproduces auxiliary statistics).
#' @export
#' @examples
#' ## Synthetic panels: 20 actors x 15 components, 3 periods, each period
#' ## redraws 10 percent of the cells from a world of density p.
#' sim_panel <- function(p, W = 3, M = 20, N = 15) {
#'   B <- array(0L, c(M, N, W))
#'   B[, , 1] <- rbinom(M * N, 1, p)
#'   for (w in 2:W) {
#'     redraw <- rbinom(M * N, 1, 0.1) == 1
#'     b <- B[, , w - 1]
#'     b[redraw] <- rbinom(sum(redraw), 1, p)
#'     B[, , w] <- b
#'   }
#'   B
#' }
#' set.seed(1)
#' observed <- sim_panel(0.20)
#' tol <- list(density = 0.02, mean_K_AC = c(rel = 0.10),
#'             mean_K_CA = c(rel = 0.10), sd_K_AC = c(rel = 0.25),
#'             sd_K_CA = c(rel = 0.25), change_rate = 0.02)
#'
#' ## A world with the same density passes.
#' g <- searchnet_moment_gate(observed, lapply(1:20, function(r) sim_panel(0.20)),
#'                            tolerance = tol, theta_source = "rates_density_fit")
#' g
#'
#' ## A world twice as dense fails; on_fail = "return" keeps the result.
#' g2 <- searchnet_moment_gate(observed, lapply(1:20, function(r) sim_panel(0.40)),
#'                             tolerance = tol, on_fail = "return")
#' g2$verdict
searchnet_moment_gate <- function(observed, simulated,
                                  moments = c("density", "mean_K_AC", "mean_K_CA",
                                              "sd_K_AC", "sd_K_CA", "change_rate"),
                                  tolerance,
                                  on_fail = c("stop", "warn", "return"),
                                  theta_source = NULL) {
  fn <- "searchnet_moment_gate()"
  .call <- match.call()
  on_fail <- match.arg(on_fail)
  if (missing(tolerance) || is.null(tolerance))
    stop(fn, ": `tolerance` is required and has no default. State, for each ",
         "gated moment and before seeing the simulation, how far the simulated ",
         "MEAN may sit from the observed value, e.g. ",
         "tolerance = list(density = 0.005, sd_K_AC = c(rel = 0.15)).",
         call. = FALSE)
  if (!is.character(moments) || !length(moments) || anyNA(moments) ||
      anyDuplicated(moments))
    stop(fn, ": `moments` must be a character vector of distinct moment names.",
         call. = FALSE)
  if (!is.null(theta_source) &&
      (!is.character(theta_source) || length(theta_source) != 1L ||
       is.na(theta_source)))
    stop(fn, ": `theta_source` must be NULL or a single string.", call. = FALSE)

  ## ---- provenance of the world's parameters (checked first) --------------
  sim_items <- if (.mg_is_env(simulated) || .mg_is_table(simulated) ||
                   !is.list(simulated) || inherits(simulated, "searchnet_bipartite"))
                 list(simulated) else simulated
  declared <- c(list(.mg_declared_source(simulated)),
                lapply(sim_items, .mg_declared_source))
  declared <- unique(unlist(declared))
  sources <- unique(c(theta_source, declared))
  bad <- sources[vapply(sources, .mg_is_starting_values, logical(1))]
  if (length(bad))
    stop(fn, ": refused. The simulated world was parameterized from RSiena ",
         "default starting values (theta_source = '", bad[1L], "'",
         if (!bad[1L] %in% theta_source) ", declared on the simulated object" else "",
         "). getNetworkStartingVals() floors the tie-creation proportion at ",
         "0.02 and trims the density start to [-3, 3]; these are numerical-",
         "safety defaults, not calibrated parameters, and a sparse network ",
         "simulated from them comes out several times too dense. A gate passed ",
         "or failed in that world says nothing about the data. Take the rates ",
         "and density from a fit of rates and density only (no focal or ",
         "structural effect), simulate from that, and record its provenance ",
         "(for example theta_source = 'rates_density_fit').", call. = FALSE)
  if (length(declared) > 1L)
    warning(fn, ": the simulated objects declare different theta sources (",
            paste(declared, collapse = ", "), "); a gate pools them as one ",
            "world.", call. = FALSE)
  theta_rec <- if (!is.null(theta_source)) theta_source
               else if (length(declared)) declared[1L] else NA_character_
  theta_origin <- if (!is.null(theta_source)) "argument"
                  else if (length(declared)) "declared on the simulated object"
                  else "not stated"

  ## ---- observed moments ----------------------------------------------------
  obs_dims <- NULL
  if (.mg_is_table(observed)) {
    om <- .mg_table_moments(observed, "observed")
    if (nrow(om) != 1L)
      stop(fn, ": an observed moment table must have exactly one row (or one ",
           "value per moment).", call. = FALSE)
    obs_vec <- om[1L, ]
  } else {
    obs_vec <- .mg_panel_moments(.mg_as_panel(observed, "observed"))
    obs_dims <- attr(obs_vec, "dims")
  }

  ## ---- simulated moments ---------------------------------------------------
  sim_dims <- NULL
  if (.mg_is_table(simulated)) {
    sim_mat <- .mg_table_moments(simulated, "simulated")
  } else {
    panels <- list()
    for (it in sim_items) {
      if (.mg_is_env(it)) panels <- c(panels, .mg_env_panels(it))
      else panels <- c(panels, list(.mg_as_panel(it, "simulated")))
    }
    if (!length(panels))
      stop(fn, ": `simulated` holds no replications.", call. = FALSE)
    pm <- lapply(panels, .mg_panel_moments)
    sim_dims <- t(vapply(pm, function(v) attr(v, "dims"), numeric(3)))
    sim_mat <- do.call(rbind, lapply(pm, function(v) { attr(v, "dims") <- NULL; v }))
    if (!is.null(obs_dims)) {
      if (any(sim_dims[, "M"] != obs_dims[["M"]]) ||
          any(sim_dims[, "N"] != obs_dims[["N"]]))
        stop(fn, ": the simulated network is not the observed size (observed ",
             obs_dims[["M"]], " x ", obs_dims[["N"]], "; simulated ",
             paste(unique(sprintf("%d x %d", sim_dims[, "M"], sim_dims[, "N"])),
                   collapse = ", "), ").", call. = FALSE)
      if (any(sim_dims[, "T"] != obs_dims[["T"]]))
        warning(fn, ": the observed panel has ", obs_dims[["T"]],
                " period(s) but simulated replications have ",
                paste(sort(unique(sim_dims[, "T"])), collapse = ", "),
                "; the moments average over different numbers of periods.",
                call. = FALSE)
    }
  }

  ## ---- moment availability -------------------------------------------------
  unknown <- setdiff(moments, union(.MG_MOMENTS, intersect(names(obs_vec),
                                                           colnames(sim_mat))))
  if (length(unknown))
    stop(fn, ": unknown moment(s): ", paste(unknown, collapse = ", "),
         ". Available: ", paste(.MG_MOMENTS, collapse = ", "), ".", call. = FALSE)
  for (m in moments) {
    if (!m %in% names(obs_vec) || is.na(obs_vec[[m]]))
      stop(fn, ": the observed value of '", m, "' is missing",
           if (m %in% .MG_CHANGE_MOMENTS) " (a change moment needs at least two periods)"
           else "", ".", call. = FALSE)
    if (!m %in% colnames(sim_mat) || all(is.na(sim_mat[, m])))
      stop(fn, ": no simulated value of '", m, "'",
           if (m %in% .MG_CHANGE_MOMENTS) " (a change moment needs at least two periods)"
           else "", ".", call. = FALSE)
  }
  tol <- .mg_parse_tolerance(tolerance, moments)

  ## ---- the gate: simulated MEAN against the observed value -----------------
  R <- nrow(sim_mat)
  if (R < 2L)
    warning(fn, ": one simulated replication; its value is a single draw with ",
            "no Monte Carlo standard error. Gate on several replications.",
            call. = FALSE)
  rows <- lapply(seq_along(moments), function(i) {
    m <- moments[i]
    s <- sim_mat[, m]; s <- s[!is.na(s)]
    o <- as.numeric(obs_vec[[m]])
    mu <- mean(s)
    sdv <- if (length(s) >= 2L) stats::sd(s) else NA_real_
    mcse <- sdv / sqrt(length(s))
    d <- mu - o
    rd <- if (o != 0) d / abs(o) else NA_real_
    t_i <- tol$tolerance[i]; ty <- tol$tol_type[i]
    if (ty == "rel" && o == 0)
      stop(fn, ": a relative tolerance for '", m, "' is undefined because the ",
           "observed value is 0; state an absolute tolerance.", call. = FALSE)
    tol_abs <- if (ty == "rel") t_i * abs(o) else t_i
    data.frame(moment = m, observed = o, sim_mean = mu, sim_sd = sdv,
               mcse = mcse, diff = d, rel_diff = rd, tolerance = t_i,
               tol_type = ty, pass = abs(d) <= tol_abs,
               mcse_resolved = if (is.na(mcse)) NA else 2 * mcse <= tol_abs,
               obs_in_sim_range = o >= min(s) && o <= max(s),
               n_used = length(s), stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows); rownames(tab) <- NULL
  ok <- all(tab$pass)

  if (is.null(rownames(sim_mat))) rownames(sim_mat) <- as.character(seq_len(R))
  prov <- list(
    theta_source = theta_rec, theta_source_origin = theta_origin,
    observed_dims = obs_dims,
    simulated_dims = if (is.null(sim_dims)) NULL else unique(sim_dims),
    n_reps = R, moments = moments, on_fail = on_fail,
    searchnet_version = .searchnet_running_version(),
    R_version = R.version.string,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    call = .call)
  out <- structure(list(table = tab, pass = ok,
                        verdict = if (ok) "PASS" else "FAIL",
                        n_reps = R, observed_moments = obs_vec[moments],
                        sim_moments = sim_mat[, intersect(colnames(sim_mat),
                                                          union(moments, .MG_MOMENTS)),
                                              drop = FALSE],
                        tolerance = tol, provenance = prov),
                   class = "searchnet_moment_gate")

  if (!ok && on_fail != "return") {
    f <- tab[!tab$pass, , drop = FALSE]
    msg <- paste0(fn, ": FAIL. The simulated world does not reproduce ",
                  nrow(f), " of ", nrow(tab), " observed moment(s) (judged by ",
                  "the simulated mean over ", R, " replication(s)):\n",
                  paste(sprintf("  %s: observed %s, simulated mean %s (diff %s; tolerance %s %s)",
                                f$moment, signif(f$observed, 4), signif(f$sim_mean, 4),
                                signif(f$diff, 3), f$tol_type, signif(f$tolerance, 3)),
                        collapse = "\n"),
                  "\nDo not read estimates or counterfactuals from this world. ",
                  "Use on_fail = \"return\" to inspect the result.")
    if (on_fail == "stop")
      stop(structure(class = c("searchnet_moment_gate_error", "error", "condition"),
                     list(message = msg, call = NULL, gate = out)))
    warning(msg, call. = FALSE)
  }
  out
}


#' @export
print.searchnet_moment_gate <- function(x, digits = 4, ...) {
  p <- x$provenance
  cat(sprintf("<searchnet_moment_gate> %s: %d of %d moment(s) pass, %d replication(s)\n",
              x$verdict, sum(x$table$pass), nrow(x$table), x$n_reps))
  cat(sprintf("theta_source: %s (%s)\n",
              if (is.na(p$theta_source)) "not stated" else p$theta_source,
              p$theta_source_origin))
  t <- x$table
  show <- data.frame(
    moment = t$moment,
    observed = signif(t$observed, digits), sim_mean = signif(t$sim_mean, digits),
    sim_sd = signif(t$sim_sd, 3), mcse = signif(t$mcse, 3),
    diff = signif(t$diff, 3),
    tolerance = paste(t$tol_type, signif(t$tolerance, 3)),
    result = ifelse(t$pass, "PASS", "FAIL"),
    stringsAsFactors = FALSE)
  print(show, row.names = FALSE)
  cat("PASS/FAIL compares the simulated MEAN with the observed value; the SD\n",
      "and MCSE are reported, not added to the tolerance.\n", sep = "")
  unres <- t$moment[!is.na(t$mcse_resolved) & !t$mcse_resolved]
  if (length(unres))
    cat("Not resolved (2 x MCSE > tolerance), add replications: ",
        paste(unres, collapse = ", "), "\n", sep = "")
  invisible(x)
}
