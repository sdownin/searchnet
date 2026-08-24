###############################################################################
## searchnet-behavior.R
##
## Network--behavior coevolution for the bipartite searchnet engine.
##
## WHAT RSIENA ACTUALLY SUPPORTS (verified, not recalled)
## ------------------------------------------------------
## RSiena 1.5.0 DOES support a behavior dependent variable coevolving with a
## bipartite network dependent variable. `sienaDataCreate()` accepts both, and
## `getEffects()` returns a populated effect set for the behavior DV.
##
## The behavior DV must live on the ACTORS node set (the ROW mode of the
## bipartite network). A behavior on the COMPONENTS node set is also
## constructible, but the influence effects below are then defined over
## component-to-component distance-2 neighbourhoods, which is a different
## substantive claim; `saomnk_behavior()` therefore defaults to ACTORS and
## requires the caller to say so explicitly for COMPONENTS.
##
## The crucial limitation, which is NOT a bug and cannot be worked around at
## the R level: the classic one-mode influence effects `avAlt`, `totAlt`,
## `avSim` and `totSim` DO NOT EXIST for a behavior attached to a bipartite
## network. They cannot be included, because in a bipartite network an actor's
## direct alters are COMPONENTS, and components have no behavior to average.
##
## RSiena's substitutes are the distance-2 effects: two actors are
## distance-2 neighbours when they hold a component in common. These ARE the
## bipartite influence effects, and they are genuinely available:
##
##   avInAltDist2   average behavior of distance-2 (co-holding) alters
##   totInAltDist2  total   behavior of distance-2 alters
##   avTInAltDist2  average of the total over distance-2 alters
##   totAInAltDist2 total of the average over distance-2 alters
##   avInSimDist2   average behavior SIMILARITY to distance-2 alters
##   totInSimDist2  total   behavior similarity to distance-2 alters
##
## `avInSimDist2` is the bipartite analogue of `avSim`, and is the effect a
## caller reaching for "imitation" or "social influence" almost always wants.
##
## Also available on the behavior DV:
##   linear, quad             shape (baseline tendency, self-reinforcement)
##   constant                 constant term
##   threshold, threshold2-4  threshold shapes
##   simAllNear, simAllFar    similarity to the whole population
##   avGroup                  average of the group
##   outdeg                   own outdegree in the bipartite net -> behavior
##   outIsolate               being an isolate -> behavior
##   popAlt                   popularity of the components held -> behavior
##   effFrom                  effect of an actor covariate on behavior
##   avXAlt, totXAlt          average/total of a COMPONENT covariate over the
##                            components held (needs interaction2 = the net)
##   avXInAltDist2, totXInAltDist2, avTXInAltDist2, totAXInAltDist2
##                            actor-covariate versions of the dist-2 effects
##
## And in the other direction -- selection ON behavior, i.e. behavior
## entering the NETWORK evaluation function -- RSiena offers, among others:
##   egoX, egoSqX, altInDist2, totInDist2, simEgoInDist2, sameEgoInDist2,
##   inPopX, sameXInPop, diffXInPop, sameXCycle4, avGroupEgoX,
##   degAbsDiffX, degPosDiffX, degNegDiffX, sameWXClosure
## These are reached through the ordinary `dv_bipartite$coCovars`-style route,
## with interaction1 set to the behavior DV name.
##
## `saomnk_behavior_effects()` regenerates this list from a live
## `getEffects()` call rather than trusting the comment above, and the test
## suite asserts the availability claims against RSiena directly.
###############################################################################


## Name under which the behavior DV is registered with sienaDataCreate().
## sienaDataCreate() takes the DV name from the NAME OF THE `...` ARGUMENT, and
## every downstream includeEffects()/setEffect() call addresses it by that
## string, exactly as `self$bipartite_rsienaDV` does for the network.
.SEARCHNET_BEHAVIOR_DV_NAME <- 'self$behavior_rsienaDV'


# --------------------------------------------------------------------------- #
#  saomnk_behavior
# --------------------------------------------------------------------------- #

#' Declare a Coevolving Behavior Dependent Variable
#'
#' Builds a \code{dv_behavior} block for a searchnet structure model. Adding it
#' to a structure model makes an actor-level behavior (performance,
#' aspiration, capability, absorptive capacity -- whatever the attribute is
#' theorized to be) a second dependent variable that evolves jointly with the
#' bipartite actor-component network, rather than being a fixed covariate.
#'
#' Coevolution means both directions are live at once. The network shapes the
#' behavior through the distance-2 influence effects (an actor is pulled
#' towards the behavior of the actors it shares components with), and the
#' behavior shapes the network through selection effects declared on
#' \code{dv_bipartite} with \code{interaction1} set to this DV's name.
#'
#' @section Which effects are available:
#' For a behavior attached to a BIPARTITE network, RSiena does \strong{not}
#' provide the one-mode influence effects \code{avAlt}, \code{totAlt},
#' \code{avSim} or \code{totSim}. In a bipartite network an actor's direct
#' alters are components, which have no behavior to average. The available
#' influence effects are the distance-2 family --- \code{avInAltDist2},
#' \code{totInAltDist2}, \code{avTInAltDist2}, \code{totAInAltDist2},
#' \code{avInSimDist2}, \code{totInSimDist2} --- where two actors are
#' distance-2 neighbours when they hold a component in common.
#' \code{avInSimDist2} is the bipartite counterpart of \code{avSim}.
#' Call \code{\link{saomnk_behavior_effects}} for the list RSiena reports for
#' your own model, rather than relying on this paragraph.
#'
#' @param values Numeric. Either a length-\code{M} vector (replicated across
#'   waves) or an \code{M x waves} matrix of behavior values. RSiena requires
#'   behavior to be integer-valued with a modest number of categories; values
#'   are rounded and shifted to start at 1, and the mapping is reported when
#'   \code{verbose = TRUE}.
#' @param effects A list of effect specifications for the behavior evaluation
#'   function. Each is a list with \code{effect} (RSiena shortName),
#'   \code{parameter}, and optionally \code{interaction1} / \code{interaction2}.
#'   For network-dependent effects set
#'   \code{interaction1 = "self$bipartite_rsienaDV"}. Defaults to a
#'   \code{linear} + \code{quad} shape, which is the minimum RSiena needs to
#'   identify a behavior process.
#' @param rates A list of rate-effect specifications. Defaults to a single basic
#'   \code{Rate} effect, which sets how often actors get the opportunity to
#'   change their behavior relative to their network ties.
#' @param name Character. DV name. Leave at the default unless you know why you
#'   are changing it --- the engine addresses this DV by name throughout.
#' @param waves Integer. Number of observation waves (default \code{2}, matching
#'   the bipartite DV that \code{search_rsiena()} constructs).
#' @param nodeSet Character. \code{"ACTORS"} (default) or \code{"COMPONENTS"}.
#' @param verbose Logical. Report the integer recoding applied to \code{values}.
#'
#' @return A list with class \code{"saomnk_behavior"}, to be placed in a
#'   structure model under the name \code{dv_behavior}.
#'
#' @seealso \code{\link{saomnk_behavior_effects}}
#'
#' @export
#' @examples
#' \dontrun{
#' env <- saomnk_env(M = 6, N = 12, seed = 42)
#' mod <- saomnk_model(density = -0.5,
#'                     influence_matrix = saomnk_block_diagonal(12, 4))
#'
#' ## Performance coevolves with the network: actors are pulled towards the
#' ## performance of the actors they share components with.
#' mod$dv_behavior <- saomnk_behavior(
#'   values  = sample(1:3, 6, replace = TRUE),
#'   effects = list(
#'     list(effect = "linear",       parameter =  0.0),
#'     list(effect = "quad",         parameter = -0.2),
#'     list(effect = "avInSimDist2", parameter =  0.5,
#'          interaction1 = "self$bipartite_rsienaDV")
#'   )
#' )
#'
#' saomnk_run(env, mod, steps_per_actor = 30)
#' }
saomnk_behavior <- function(values,
                            effects = NULL,
                            rates   = NULL,
                            name    = .SEARCHNET_BEHAVIOR_DV_NAME,
                            waves   = 2L,
                            nodeSet = c("ACTORS", "COMPONENTS"),
                            verbose = FALSE) {

  nodeSet <- match.arg(nodeSet)
  waves   <- as.integer(waves)
  if (is.na(waves) || waves < 2L)
    stop("`waves` must be at least 2: RSiena needs two observations to define a change process.")

  if (!is.numeric(values) || !length(values))
    stop("`values` must be a non-empty numeric vector or matrix.")

  ## Normalize to an M x waves matrix.
  if (is.matrix(values)) {
    if (ncol(values) == 1L) {
      values <- matrix(rep(values[, 1L], waves), ncol = waves)
    } else if (ncol(values) != waves) {
      ## Trust the matrix the caller supplied and adopt its wave count.
      waves <- ncol(values)
    }
  } else {
    values <- matrix(rep(values, waves), ncol = waves)
  }

  ## RSiena behavior must be integer-valued and, in practice, low-cardinality.
  ## Round, then shift so the minimum is 1. Report the shift: silently moving a
  ## caller's performance scale would make every reported coefficient refer to
  ## units the caller did not choose.
  vals_int <- round(values)
  shift    <- 1L - min(vals_int, na.rm = TRUE)
  if (shift != 0L) vals_int <- vals_int + shift
  storage.mode(vals_int) <- "integer"

  n_cat <- length(unique(as.vector(vals_int[!is.na(vals_int)])))
  if (n_cat < 2L)
    stop("`values` is constant. A behavior dependent variable needs at least ",
         "two distinct values, or there is no change process to model.")
  if (n_cat > 20L)
    warning(sprintf(
      "Behavior has %d distinct integer values. RSiena treats behavior as an ordinal scale with a small number of categories; consider binning.",
      n_cat))

  if (verbose && shift != 0L)
    cat(sprintf("saomnk_behavior(): values rounded and shifted by %+d; range is now [%d, %d].\n",
                shift, min(vals_int), max(vals_int)))

  if (is.null(effects)) {
    effects <- list(
      list(effect = "linear", parameter =  0.0),
      list(effect = "quad",   parameter = -0.2)
    )
  }
  if (is.null(rates)) {
    rates <- list(list(effect = "Rate", parameter = 1.0))
  }

  ## Stamp dv_name / fix onto every effect spec so add_rsiena_effects() and
  ## get_input_from_structure_model() see the same shape they see for
  ## dv_bipartite effects.
  .stamp <- function(lst) {
    lapply(seq_along(lst), function(i) {
      e <- lst[[i]]
      if (!is.list(e) || is.null(e$effect))
        stop(sprintf("Behavior effect spec %d must be a list with an `effect` field.", i))
      if (is.null(e$parameter))    e$parameter    <- 0
      if (is.null(e$dv_name))      e$dv_name      <- name
      if (is.null(e$fix))          e$fix          <- TRUE
      if (is.null(e$interaction1)) e$interaction1 <- ''
      if (is.null(e$interaction2)) e$interaction2 <- ''
      e
    })
  }

  out <- list(
    name    = name,
    nodeSet = nodeSet,
    waves   = waves,
    values  = vals_int,
    shift   = shift,
    rates   = .stamp(rates),
    effects = .stamp(effects)
  )
  class(out) <- c("saomnk_behavior", "list")
  out
}


# --------------------------------------------------------------------------- #
#  saomnk_behavior_effects
# --------------------------------------------------------------------------- #

#' Report the Behavior Effects RSiena Offers for a Bipartite Network
#'
#' Constructs a minimal bipartite-plus-behavior \code{sienaData} object of the
#' requested size and returns the effect table RSiena reports for the behavior
#' dependent variable. Use this instead of trusting documentation --- including
#' searchnet's own --- about which influence effects exist.
#'
#' The distinction that matters most: the one-mode influence effects
#' \code{avAlt}, \code{totAlt}, \code{avSim} and \code{totSim} are absent for a
#' bipartite network, because direct alters are components and components have
#' no behavior. Their bipartite counterparts are the distance-2 effects
#' (\code{avInAltDist2}, \code{avInSimDist2}, ...), where two actors are
#' neighbours when they hold a component in common.
#'
#' @param M Integer. Number of actors in the probe object (default \code{10}).
#' @param N Integer. Number of components in the probe object (default \code{6}).
#' @param direction Character. \code{"influence"} lists effects on the BEHAVIOR
#'   DV (network shapes behavior); \code{"selection"} lists effects on the
#'   NETWORK DV that reference the behavior (behavior shapes network);
#'   \code{"both"} returns both, tagged.
#' @param network_only Logical. If \code{TRUE} (default \code{FALSE}), restrict
#'   the influence listing to effects that reference the bipartite network.
#'
#' @return A \code{data.frame} with columns \code{direction}, \code{shortName},
#'   \code{effectName}, \code{interaction1}, \code{interaction2}, \code{type}.
#'
#' @export
#' @examples
#' \dontrun{
#' ## Which influence effects can a bipartite network exert on behavior?
#' saomnk_behavior_effects(direction = "influence", network_only = TRUE)
#'
#' ## Confirm for yourself that avSim is not among them:
#' "avSim" %in% saomnk_behavior_effects(direction = "influence")$shortName
#' }
saomnk_behavior_effects <- function(M = 10L, N = 6L,
                                    direction = c("influence", "selection", "both"),
                                    network_only = FALSE) {

  direction <- match.arg(direction)
  M <- as.integer(M); N <- as.integer(N)
  if (M < 2L || N < 2L) stop("`M` and `N` must both be at least 2.")

  W <- 3L
  set.seed(1L)
  arr <- array(stats::rbinom(M * N * W, 1L, 0.3), dim = c(M, N, W))
  beh <- matrix(rep(seq_len(3L), length.out = M * W), nrow = M, ncol = W)

  net <- RSiena::sienaDependent(arr, type = "bipartite",
                                nodeSet = c("ACTORS", "COMPONENTS"))
  bhv <- RSiena::sienaDependent(beh, type = "behavior",
                                nodeSet = "ACTORS", allowOnly = FALSE)
  acov <- RSiena::coCovar(stats::rnorm(M), nodeSet = "ACTORS")
  ccov <- RSiena::coCovar(stats::rnorm(N), nodeSet = "COMPONENTS")

  dat <- RSiena::sienaDataCreate(
    net = net, behavior = bhv, actor_cov = acov, component_cov = ccov,
    nodeSets = list(RSiena::sienaNodeSet(M, nodeSetName = "ACTORS"),
                    RSiena::sienaNodeSet(N, nodeSetName = "COMPONENTS"))
  )

  eff <- RSiena::getEffects(dat)
  class(eff) <- "data.frame"

  keep <- c("shortName", "effectName", "interaction1", "interaction2", "type")

  out <- list()
  if (direction %in% c("influence", "both")) {
    inf <- eff[eff$name == "behavior", keep, drop = FALSE]
    if (network_only)
      inf <- inf[inf$interaction1 == "net" | inf$interaction2 == "net", , drop = FALSE]
    if (nrow(inf)) inf <- cbind(direction = "influence", inf, stringsAsFactors = FALSE)
    out$influence <- inf
  }
  if (direction %in% c("selection", "both")) {
    sel <- eff[eff$name == "net" &
                 (eff$interaction1 == "behavior" | eff$interaction2 == "behavior"),
               keep, drop = FALSE]
    if (nrow(sel)) sel <- cbind(direction = "selection", sel, stringsAsFactors = FALSE)
    out$selection <- sel
  }

  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}


# --------------------------------------------------------------------------- #
#  saomnk_get_behavior
# --------------------------------------------------------------------------- #

#' Extract the Simulated Behavior Trajectory
#'
#' Recovers the coevolving behavior dependent variable from a completed run.
#' Without this the behavior is write-only: it influences the simulated network
#' but its own path is buried in \code{env$rsiena_model$sims}.
#'
#' Two views are available. The default long data frame gives one row per
#' simulation run per actor, which is what a plot or a panel regression wants.
#' \code{wide = TRUE} gives a runs-by-actors matrix.
#'
#' Note that a "run" is one row of the theta matrix, not one ministep. Under the
#' unconditional estimation that a two-DV model forces, each run contains
#' several ministeps, so the behavior is observed at the end of each run rather
#' than after every individual change. Per-ministep behavior changes are in
#' \code{env$chain_stats}, in the \code{beh_difference} column of the rows whose
#' \code{dv_varname} is the behavior DV.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after a run whose structure
#'   model declared a \code{dv_behavior} block.
#' @param wide Logical. Return a runs x actors matrix instead of a long data
#'   frame (default \code{FALSE}).
#' @param name Character. DV name to extract (default: the standard behavior
#'   DV name).
#'
#' @return A \code{data.frame} with columns \code{run}, \code{actor_id},
#'   \code{value}, or a numeric matrix when \code{wide = TRUE}.
#'
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 4, N = 6, seed = 42)
#' mod <- saomnk_model(density = -0.5)
#' mod$dv_behavior <- saomnk_behavior(
#'   values  = c(1, 2, 3, 2),
#'   effects = list(
#'     list(effect = "linear", parameter =  0.0),
#'     list(effect = "quad",   parameter = -0.2)
#'   )
#' )
#' saomnk_run(env, mod, steps_per_actor = 5, seed = 12345)
#'
#' beh <- saomnk_get_behavior(env)
#' head(beh)
#' saomnk_get_behavior(env, wide = TRUE)[1:3, ]
#' }
#' @export
saomnk_get_behavior <- function(env, wide = FALSE,
                                name = .SEARCHNET_BEHAVIOR_DV_NAME) {

  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))
  sims <- env$rsiena_model$sims
  if (is.null(sims))
    stop("No simulation results on this environment. Run saomnk_run() first.")
  if (!name %in% names(sims[[1L]][[1L]]))
    stop(sprintf(
      "'%s' is not among the simulated dependent variables (%s). Did the structure model include a dv_behavior block?",
      name, paste(names(sims[[1L]][[1L]]), collapse = ", ")))

  vals <- lapply(sims, function(run) {
    v <- run[[1L]][[name]]
    if (is.list(v)) v <- v[[1L]]
    as.numeric(v)
  })

  mat <- do.call(rbind, vals)
  colnames(mat) <- paste0("actor_", seq_len(ncol(mat)))
  rownames(mat) <- seq_len(nrow(mat))

  if (wide) return(mat)

  data.frame(
    run      = rep(seq_len(nrow(mat)), times = ncol(mat)),
    actor_id = rep(seq_len(ncol(mat)), each  = nrow(mat)),
    value    = as.vector(mat),
    stringsAsFactors = FALSE
  )
}


## ---------------------------------------------------------------------------
## Internal: is there a behavior DV in this structure model?
## ---------------------------------------------------------------------------
.searchnet_has_behavior <- function(structure_model) {
  is.list(structure_model) &&
    'dv_behavior' %in% names(structure_model) &&
    !is.null(structure_model$dv_behavior) &&
    !is.null(structure_model$dv_behavior$values)
}


## ---------------------------------------------------------------------------
## Internal: build the sienaDependent behavior object for an environment
## ---------------------------------------------------------------------------
## Reconciles the declared behavior values against the environment's actual M
## (or N, for a COMPONENTS behavior). A length mismatch is an error rather than
## a recycle: silently recycling would attach the wrong actor's performance to
## the wrong actor, and nothing downstream would reveal it.
.searchnet_build_behavior_dv <- function(env, dv_behavior) {

  node_set <- if (is.null(dv_behavior$nodeSet)) "ACTORS" else dv_behavior$nodeSet
  n_expected <- if (identical(node_set, "COMPONENTS")) env$N else env$M

  vals <- dv_behavior$values
  if (!is.matrix(vals)) vals <- matrix(vals, ncol = 1L)

  if (nrow(vals) != n_expected)
    stop(sprintf(
      "dv_behavior has %d rows but node set '%s' has %d members. Behavior values must be one per node.",
      nrow(vals), node_set, n_expected))

  if (ncol(vals) < 2L)
    vals <- matrix(rep(vals[, 1L], 2L), ncol = 2L)

  storage.mode(vals) <- "integer"

  RSiena::sienaDependent(vals, type = "behavior",
                         nodeSet = node_set, allowOnly = FALSE)
}
