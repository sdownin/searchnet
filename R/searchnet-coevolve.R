###############################################################################
## searchnet-coevolve.R
##
## Multivariate SAOM: the ARCHITECTURE as a second dependent network.
##
## WHAT THIS FILE IS FOR
## --------------------
## searchnet's standing dependent variable is a two-mode actor-by-component
## network (M x N x W): who occupies which component of the activity system.
## The component-to-component architecture -- which components depend on which
## -- has until now entered as a FIXED structure (the influence/epistasis
## matrix), i.e. as a constant of the environment.
##
## This file lets the architecture be a DEPENDENT NETWORK instead: a directed
## one-mode network on the COMPONENT node set (N x N x W) that evolves under
## its own rate and evaluation functions, jointly estimated with the bipartite
## network. Substantively this is the module-to-module dependency graph of a
## software project, the interface map of a product, or the task-precedence
## graph of an operating routine -- a thing developers refactor, not a thing
## they are handed.
##
## The payoff is that "does the organisation reorganise to fit the
## architecture, or does the architecture get refactored to fit the
## organisation" stops being a static congruence correlation and becomes a
## selection-versus-influence question with two separately signed, separately
## tested coefficients estimated from the same panel.
##
## WHAT RSIENA ACTUALLY OFFERS FOR THIS PAIRING (verified 2026-08-23 against
## RSiena 1.5.0 / R 4.5.3 by building the toy data object and reading
## getEffects(), NOT recalled from documentation)
## ---------------------------------------------------------------------------
## The bipartite DV is ACTORS x COMPONENTS. The new one-mode DV sits on
## COMPONENTS, which is the bipartite network's SECOND mode. That node-set
## placement is decisive, and it is what makes this pairing effect-poor.
##
## Exactly TWO cross-network channels exist between the two dependent
## networks, and they are asymmetric:
##
##   (1) ARCHITECTURE <- ORGANISATION.  On the one-mode component DV:
##         from.w.ind   interaction1 = <bipartite DV>, interaction2 = <bipartite DV>
##       "architecture: from memberships agr. weighted by memberships indegree".
##       This is the ONLY effect the component DV gains from the presence of
##       the bipartite DV. It is the refactoring channel: component-to-component
##       ties form along lines of shared occupancy.
##
##   (2) ORGANISATION <- ARCHITECTURE.  On the bipartite DV:
##         sameWXClosure  interaction1 = <component DV>, interaction2 = <a covariate>
##       "memberships: mixed <architecture> closure same <covariate>".
##       This one is CONDITIONAL: it appears only when some actor covariate or
##       behaviour DV is present to serve as interaction2. With no covariate in
##       the data object, the bipartite DV gains NOTHING AT ALL from the
##       presence of the component DV.
##
## NON-IMPLEMENTATIONS (absent for this node-set pairing; reported as absent,
## which says nothing about whether the corresponding mechanism is real):
##   crprod, crprodRecip, crprodMutual, from, fromMutual, to, toBack, toRecip,
##   mixedInXW, XWX, XWX1, XWX2, XXW, WXX, cl.XWX, sharedTo, both
## These exist in allEffects, but under effect groups whose node-set
## requirements this pairing does not meet. Verified: putting the one-mode DV
## on ACTORS instead (the FIRST mode) makes `to`, `toBack`, `toRecip`,
## `sharedTo`, `toAny`, `JoutMix`, `gwespFBMix`, `inPopOutW` and a large
## `*Intn` family appear immediately in BOTH directions. The poverty is
## specific to a one-mode network on the second mode, not to multivariate SAOM.
##
## THE XWX ROUTE, AND WHY IT IS A DIFFERENT CLAIM.  `XWX`, `XWX1`, `XWX2`
## ("memberships: XW=>X closure of W") DO become available on the bipartite DV
## when an N x N structure on COMPONENTS is supplied as a DYADIC COVARIATE
## (effect group `dyadSecondBipartiteObjective`). They do not become available
## from a dependent network. `searchnet_coevolve_data(architecture_lag = TRUE)`
## therefore ALSO registers the observed component network, lagged, as a
## varying dyadic covariate, which reopens that family.
##
## This is deliberately NOT the same estimand as a cross-network effect between
## two dependent variables. It is the effect of the LAGGED, EXOGENOUSLY TREATED
## architecture on tie formation in the bipartite network. It cannot absorb
## simultaneous feedback, and a coefficient on it must not be reported as if it
## were a coevolution parameter. It is offered because it is the only route to
## the XW=>X closure mechanism, and it is labelled everywhere it appears.
##
## Registering that covariate also brings in a further set of rows that pair a
## dependent network with it, verified present in the smoke test:
##   on the component DV : toU, sharedToU, avAltU.2M.tie  (interaction1 =
##                         <bipartite DV>, interaction2 = architectureLag)
##   on the bipartite DV : sharedToU, avAltU.2M.tie       (interaction1 =
##                         <either DV>,   interaction2 = architectureLag)
## Every one of these inherits the same caveat: the architecture enters through
## the LAGGED COVARIATE slot, so none of them is a coevolution parameter.
##
## WHAT THIS FILE DELIBERATELY DOES NOT DO
## ---------------------------------------
## * It does not touch the R6 engine, `saomnk_*` classes, or the simulation
##   path. It is an estimation-side entry point only: data in, sienaFit out.
##   Nothing here generates a landscape or runs a search.
## * It does not invent a cross-network effect to fill the asymmetry above.
##   Direction (2) between two dependent networks is a NON-IMPLEMENTATION in
##   RSiena 1.5.0 for this pairing, and is reported as such rather than
##   silently substituted with the lagged-covariate proxy.
## * It does not interpret coefficients, choose a specification, or decide when
##   a model has converged. It reports the convergence quantities and refuses
##   to return an unidentified fit; the specification remains the analyst's.
## * It does not write to NAMESPACE. Exported names are listed in the roxygen
##   blocks and must be registered by hand.
##
## ORDERING CONSTRAINT (verified, and a silent-failure trap if missed):
## sienaDataCreate() requires one-mode networks to be passed BEFORE bipartite
## networks. Violating it raises
##   "One-mode networks (if any) should be given before bipartite networks".
## The builders below always emit the component DV first.
##
## NOTE ON allEffects: in RSiena 1.5.0 the effects table is a lazy-loaded
## DATASET, not a namespace object. `RSiena:::allEffects` errors with "object
## 'allEffects' not found"; use
##   e <- new.env(); utils::data("allEffects", package = "RSiena", envir = e)
###############################################################################


## Default dependent-variable names. These are passed as the NAMES OF THE `...`
## ARGUMENTS to sienaDataCreate(), which is where RSiena takes DV names from.
## An unnamed DV gets a deparsed-expression name, and every later
## includeEffects(name = ...) / setEffect(name = ...) lookup then matches zero
## rows and returns the effects object UNCHANGED AND WITHOUT WARNING. That is
## the silent-failure class the assertion pass in searchnet_coevolve_effects()
## exists to catch.
.SEARCHNET_COEV_NAMES <- c(
  bipartite = "memberships",
  component = "architecture",
  behavior  = "performance"
)

## Name under which the lagged architecture is registered as a dyadic covariate.
.SEARCHNET_COEV_LAGNAME <- "architectureLag"

## Node set names. The bipartite DV is ACTORS x COMPONENTS; the component DV
## lives on COMPONENTS, the second mode.
.SEARCHNET_COEV_NODESETS <- c(actors = "ACTORS", components = "COMPONENTS")


# --------------------------------------------------------------------------- #
#  internal helpers
# --------------------------------------------------------------------------- #

## Coerce a list of W matrices, or a 3-d array, to a 3-d array. Anything else
## is a user error and is named as such.
.searchnet_as_wave_array <- function(x, arg) {
  if (is.array(x) && length(dim(x)) == 3L) return(x)
  if (is.list(x)) {
    if (!length(x))
      stop("`", arg, "` is an empty list; supply at least two waves.",
           call. = FALSE)
    ok <- vapply(x, function(m) is.matrix(m) || is.data.frame(m), logical(1))
    if (!all(ok))
      stop("`", arg, "` must be a list of matrices; element(s) ",
           paste(which(!ok), collapse = ", "), " are not matrices.",
           call. = FALSE)
    x <- lapply(x, as.matrix)
    d <- vapply(x, dim, integer(2))
    if (any(d[1L, ] != d[1L, 1L]) || any(d[2L, ] != d[2L, 1L]))
      stop("`", arg, "` waves have inconsistent dimensions: ",
           paste(sprintf("wave %d is %dx%d", seq_along(x), d[1L, ], d[2L, ]),
                 collapse = "; "), ".", call. = FALSE)
    return(array(unlist(x), dim = c(d[1L, 1L], d[2L, 1L], length(x))))
  }
  if (is.matrix(x))
    stop("`", arg, "` is a single matrix. A dependent network needs at least ",
         "two waves: supply an array with a third dimension, or a list of ",
         "matrices.", call. = FALSE)
  stop("`", arg, "` must be a 3-d array or a list of matrices; got ",
       class(x)[1L], ".", call. = FALSE)
}

## Report which of a set of requested effects actually carries include == TRUE.
## Returns a data frame with one row per request. Used by both the assertion
## pass and the printed summary.
.searchnet_effect_status <- function(eff, requests) {
  do.call(rbind, lapply(requests, function(r) {
    i1 <- if (is.null(r$interaction1)) "" else r$interaction1
    i2 <- if (is.null(r$interaction2)) "" else r$interaction2
    ty <- if (is.null(r$type)) "eval" else r$type
    hit <- eff[eff$name == r$name &
                 eff$shortName == r$shortName &
                 eff$interaction1 == i1 &
                 eff$interaction2 == i2 &
                 eff$type == ty, , drop = FALSE]
    data.frame(
      dv           = r$name,
      shortName    = r$shortName,
      type         = ty,
      interaction1 = i1,
      interaction2 = i2,
      exists       = nrow(hit) > 0L,
      included     = nrow(hit) > 0L && any(hit$include),
      stringsAsFactors = FALSE
    )
  }))
}

## Apply one effect request via includeEffects(), addressing the DV by name.
## includeEffects() takes the shortName unquoted, so the call is constructed.
.searchnet_include_one <- function(eff, r) {
  args <- list(eff, include = TRUE, name = r$name, character = TRUE)
  args[[length(args) + 1L]] <- r$shortName
  names(args)[length(args)] <- ""
  if (!is.null(r$interaction1) && nzchar(r$interaction1))
    args$interaction1 <- r$interaction1
  if (!is.null(r$interaction2) && nzchar(r$interaction2))
    args$interaction2 <- r$interaction2
  if (!is.null(r$type)) args$type <- r$type
  suppressMessages(do.call(RSiena::includeEffects, args))
}


# --------------------------------------------------------------------------- #
#  searchnet_coevolve_data
# --------------------------------------------------------------------------- #

#' Build a Coevolution Data Object: Bipartite Network Plus Dependent Architecture
#'
#' Assembles an RSiena data object in which the actor-by-component bipartite
#' network and the component-by-component architecture are BOTH dependent
#' variables, optionally alongside an actor behaviour.
#'
#' @section Why this changes the question:
#' With the architecture entered as a fixed covariate, congruence between
#' structure and organisation can only be described. With it entered as a
#' dependent network, the two directions are separately parameterised: the
#' architecture can be shown to move towards the organisation, the organisation
#' towards the architecture, both, or neither.
#'
#' @section What RSiena offers, and what it does not:
#' Verified against RSiena 1.5.0. Between these two dependent networks exactly
#' one cross-network effect exists, \code{from.w.ind} on the component DV
#' (architecture follows organisation). In the other direction only
#' \code{sameWXClosure} is available on the bipartite DV, and only when a
#' covariate or behaviour is present to serve as its \code{interaction2}. The
#' classic multivariate family --- \code{crprod}, \code{from}, \code{to},
#' \code{toBack}, \code{mixedInXW}, \code{sharedTo} --- is a
#' \strong{non-implementation} for a one-mode network on the bipartite
#' network's SECOND mode. That is a statement about RSiena, not about the
#' world. Call \code{\link{searchnet_coevolve_available_effects}} to regenerate
#' the list for your own data rather than trusting this paragraph.
#'
#' @section The lagged-architecture covariate:
#' \code{architecture_lag = TRUE} additionally registers the observed component
#' network, lagged by one wave, as a varying dyadic covariate named
#' \code{"architectureLag"}. This unlocks the \code{XWX}, \code{XWX1} and
#' \code{XWX2} closure effects on the bipartite DV, which are otherwise
#' unreachable. It is a DIFFERENT ESTIMAND from a cross-network effect: the
#' architecture is treated as exogenous and lagged, so it cannot absorb
#' simultaneous feedback. Do not report a coefficient on it as a coevolution
#' parameter.
#'
#' @param bipartite An \code{M x N x W} array, or a list of \code{W} matrices
#'   each \code{M x N}: actors by components, the standing searchnet DV.
#' @param component An \code{N x N x W} array, or a list of \code{W} matrices
#'   each \code{N x N}: the directed component-to-component architecture.
#'   Diagonals are forced to zero, with a message if any were non-zero.
#' @param behavior Optional. A length-\code{M} vector (replicated across waves)
#'   or an \code{M x W} matrix of integer-valued actor behaviour. Becomes a
#'   third dependent variable on the ACTORS node set.
#' @param actor_covars Optional named list of actor covariates. A length-\code{M}
#'   vector becomes a \code{coCovar}; an \code{M x W} matrix a \code{varCovar}.
#' @param component_covars Optional named list of component covariates, in the
#'   same two shapes with \code{N} in place of \code{M}.
#' @param dyad_covars Optional named list of dyadic covariates. Dispatched on
#'   shape: \code{N x N} or \code{N x N x (W-1)} is COMPONENTS by COMPONENTS,
#'   \code{M x M} is ACTORS by ACTORS, \code{M x N} is ACTORS by COMPONENTS.
#' @param architecture_lag Logical. Register the lagged component network as a
#'   varying dyadic covariate, unlocking the \code{XWX} family. Default
#'   \code{TRUE}. Requires \code{W >= 3} to produce more than a single lag
#'   period; with \code{W == 2} a single-period covariate is built.
#' @param names Character vector with elements \code{bipartite},
#'   \code{component} and \code{behavior} giving the DV names used throughout.
#'   Change these only with reason: every downstream \code{name =} lookup uses
#'   them, and a mismatch fails silently in RSiena.
#' @param allow_only Logical, passed to \code{sienaDependent} for the behaviour
#'   DV as \code{allowOnly}. \code{FALSE} lets the simulation move behaviour in
#'   both directions even when the observed panel only ever increases.
#' @param verbose Logical. Report the assembled dimensions and node sets.
#'
#' @return A \code{siena} data object, with attribute \code{"searchnet_coev"}
#'   carrying the DV names, the lag-covariate name (or \code{NA}) and the
#'   dimensions, so the effects builder does not have to guess them.
#'
#' @seealso \code{\link{searchnet_coevolve_effects}},
#'   \code{\link{searchnet_coevolve}},
#'   \code{\link{searchnet_coevolve_available_effects}}
#'
#' @export
#' @examples
#' \dontrun{
#' set.seed(1)
#' M <- 8; N <- 6; W <- 3
#' bip <- array(rbinom(M * N * W, 1, 0.3), dim = c(M, N, W))
#' arc <- array(rbinom(N * N * W, 1, 0.3), dim = c(N, N, W))
#'
#' dat <- searchnet_coevolve_data(bipartite = bip, component = arc)
#' eff <- searchnet_coevolve_effects(dat)
#' fit <- searchnet_coevolve(dat, eff, n3 = 500, nsub = 2)
#' }
searchnet_coevolve_data <- function(bipartite,
                                    component,
                                    behavior         = NULL,
                                    actor_covars     = NULL,
                                    component_covars = NULL,
                                    dyad_covars      = NULL,
                                    architecture_lag = TRUE,
                                    names            = .SEARCHNET_COEV_NAMES,
                                    allow_only       = FALSE,
                                    verbose          = TRUE) {

  if (!requireNamespace("RSiena", quietly = TRUE))
    stop("RSiena is required for searchnet_coevolve_data().", call. = FALSE)

  nm <- .SEARCHNET_COEV_NAMES
  if (!is.null(names)) nm[base::names(names)] <- names
  if (anyDuplicated(nm))
    stop("Dependent-variable names must be distinct; got: ",
         paste(sprintf("%s = '%s'", base::names(nm), nm), collapse = ", "),
         ".", call. = FALSE)

  bip <- .searchnet_as_wave_array(bipartite, "bipartite")
  cmp <- .searchnet_as_wave_array(component, "component")

  M  <- dim(bip)[1L]; Nb <- dim(bip)[2L]; Wb <- dim(bip)[3L]
  Nc <- dim(cmp)[1L]; Nc2 <- dim(cmp)[2L]; Wc <- dim(cmp)[3L]

  ## ---- dimension agreement, each mismatch named explicitly ----------------
  if (Nc != Nc2)
    stop("`component` must be square on the component node set: waves are ",
         Nc, " x ", Nc2, ". A directed one-mode network cannot be rectangular.",
         call. = FALSE)
  if (Nb != Nc)
    stop("Component-count mismatch: `bipartite` has ", Nb,
         " columns (components) but `component` is ", Nc, " x ", Nc,
         ". Both must describe the same component node set.", call. = FALSE)
  if (Wb != Wc)
    stop("Wave-count mismatch: `bipartite` has ", Wb, " waves but `component` ",
         "has ", Wc, ". Coevolving dependent variables must be observed on the ",
         "same panel.", call. = FALSE)
  if (Wb < 2L)
    stop("Only ", Wb, " wave supplied. A SAOM needs at least two observations ",
         "to define a change process.", call. = FALSE)

  N <- Nb; W <- Wb

  ## ---- the architecture must have a zero diagonal -------------------------
  diag_nonzero <- FALSE
  for (w in seq_len(W)) {
    if (any(cmp[, , w][diag(N) == 1] != 0, na.rm = TRUE)) diag_nonzero <- TRUE
    d <- cmp[, , w]; diag(d) <- 0; cmp[, , w] <- d
  }
  if (diag_nonzero && verbose)
    message("searchnet_coevolve_data(): non-zero diagonal entries in ",
            "`component` were set to zero (self-dependency is not a tie).")

  ## ---- node sets ----------------------------------------------------------
  ns <- .SEARCHNET_COEV_NODESETS
  actorSet <- RSiena::sienaNodeSet(M, nodeSetName = ns[["actors"]])
  compSet  <- RSiena::sienaNodeSet(N, nodeSetName = ns[["components"]])

  ## ---- dependent variables ------------------------------------------------
  dv_component <- RSiena::sienaDependent(
    cmp, type = "oneMode", nodeSet = ns[["components"]])
  dv_bipartite <- RSiena::sienaDependent(
    bip, type = "bipartite",
    nodeSet = c(ns[["actors"]], ns[["components"]]))

  ## sienaDataCreate() requires one-mode BEFORE bipartite. This ordering is
  ## load-bearing, not cosmetic.
  args <- list()
  args[[nm[["component"]]]] <- dv_component
  args[[nm[["bipartite"]]]] <- dv_bipartite

  if (!is.null(behavior)) {
    beh <- behavior
    if (!is.numeric(beh))
      stop("`behavior` must be numeric.", call. = FALSE)
    if (!is.matrix(beh)) {
      if (length(beh) != M)
        stop("`behavior` has length ", length(beh), " but there are ", M,
             " actors.", call. = FALSE)
      beh <- matrix(rep(beh, W), nrow = M, ncol = W)
    }
    if (nrow(beh) != M)
      stop("`behavior` has ", nrow(beh), " rows but there are ", M,
           " actors.", call. = FALSE)
    if (ncol(beh) != W)
      stop("`behavior` has ", ncol(beh), " columns but the panel has ", W,
           " waves.", call. = FALSE)
    storage.mode(beh) <- "integer"
    if (length(unique(as.vector(beh[!is.na(beh)]))) < 2L)
      stop("`behavior` is constant across all actors and waves; there is no ",
           "change process to model.", call. = FALSE)
    args[[nm[["behavior"]]]] <- RSiena::sienaDependent(
      beh, type = "behavior", nodeSet = ns[["actors"]], allowOnly = allow_only)
  }

  ## ---- monadic covariates -------------------------------------------------
  .add_monadic <- function(lst, n, setname, label) {
    if (is.null(lst)) return(invisible(NULL))
    if (!is.list(lst) || is.null(base::names(lst)) || any(!nzchar(base::names(lst))))
      stop("`", label, "` must be a NAMED list; unnamed covariates cannot be ",
           "addressed by later effect specifications.", call. = FALSE)
    for (k in base::names(lst)) {
      v <- lst[[k]]
      if (is.matrix(v)) {
        if (nrow(v) != n)
          stop("`", label, "$", k, "` has ", nrow(v), " rows; expected ", n,
               ".", call. = FALSE)
        args[[k]] <<- RSiena::varCovar(v, nodeSet = setname)
      } else {
        if (length(v) != n)
          stop("`", label, "$", k, "` has length ", length(v), "; expected ",
               n, ".", call. = FALSE)
        args[[k]] <<- RSiena::coCovar(as.numeric(v), nodeSet = setname)
      }
    }
    invisible(NULL)
  }
  .add_monadic(actor_covars,     M, ns[["actors"]],     "actor_covars")
  .add_monadic(component_covars, N, ns[["components"]], "component_covars")

  ## ---- dyadic covariates, dispatched on shape -----------------------------
  if (!is.null(dyad_covars)) {
    if (!is.list(dyad_covars) || is.null(base::names(dyad_covars)))
      stop("`dyad_covars` must be a NAMED list.", call. = FALSE)
    for (k in base::names(dyad_covars)) {
      v <- dyad_covars[[k]]
      d <- dim(v)
      if (is.null(d) || length(d) < 2L)
        stop("`dyad_covars$", k, "` must be a matrix or 3-d array.",
             call. = FALSE)
      sets <- if (d[1L] == N && d[2L] == N) {
        c(ns[["components"]], ns[["components"]])
      } else if (d[1L] == M && d[2L] == M) {
        c(ns[["actors"]], ns[["actors"]])
      } else if (d[1L] == M && d[2L] == N) {
        c(ns[["actors"]], ns[["components"]])
      } else {
        stop("`dyad_covars$", k, "` is ", d[1L], " x ", d[2L],
             ", which matches no node-set pair (M = ", M, ", N = ", N, ").",
             call. = FALSE)
      }
      args[[k]] <- if (length(d) == 3L) {
        RSiena::varDyadCovar(v, nodeSets = sets)
      } else {
        RSiena::coDyadCovar(v, nodeSets = sets)
      }
    }
  }

  ## ---- the lagged architecture as a dyadic covariate ----------------------
  ## This is the ONLY route to XWX / XWX1 / XWX2 on the bipartite DV. It is a
  ## lagged exogenous covariate, not a coevolution channel, and is named so.
  lag_name <- NA_character_
  if (isTRUE(architecture_lag)) {
    lag_name <- .SEARCHNET_COEV_LAGNAME
    if (lag_name %in% base::names(args))
      stop("A covariate named '", lag_name, "' already exists; rename it or ",
           "set architecture_lag = FALSE.", call. = FALSE)
    lag_arr <- cmp[, , seq_len(W - 1L), drop = FALSE]
    args[[lag_name]] <- RSiena::varDyadCovar(
      lag_arr, nodeSets = c(ns[["components"]], ns[["components"]]))
  }

  args$nodeSets <- list(actorSet, compSet)

  dat <- do.call(RSiena::sienaDataCreate, args)

  attr(dat, "searchnet_coev") <- list(
    names    = nm,
    lag_name = lag_name,
    M = M, N = N, W = W,
    has_behavior  = !is.null(behavior),
    has_covariate = !is.null(actor_covars) || !is.null(component_covars) ||
      !is.null(behavior)
  )

  if (verbose) {
    cat("searchnet_coevolve_data()\n")
    cat(sprintf("  actors (M)      : %d   [node set '%s']\n", M, ns[["actors"]]))
    cat(sprintf("  components (N)  : %d   [node set '%s']\n", N, ns[["components"]]))
    cat(sprintf("  waves (W)       : %d\n", W))
    cat(sprintf("  DV bipartite    : '%s'   (%d x %d x %d, ACTORS x COMPONENTS)\n",
                nm[["bipartite"]], M, N, W))
    cat(sprintf("  DV one-mode     : '%s'   (%d x %d x %d, directed on COMPONENTS)\n",
                nm[["component"]], N, N, W))
    if (!is.null(behavior))
      cat(sprintf("  DV behaviour    : '%s'   (%d x %d, on ACTORS)\n",
                  nm[["behavior"]], M, W))
    if (!is.na(lag_name))
      cat(sprintf("  dyadic covar    : '%s'  (lagged architecture; unlocks XWX,\n",
                  lag_name),
          "                    XWX1, XWX2 -- a LAGGED EXOGENOUS estimand, not\n",
          "                    a coevolution parameter)\n", sep = "")
  }

  dat
}


# --------------------------------------------------------------------------- #
#  searchnet_coevolve_available_effects
# --------------------------------------------------------------------------- #

#' Report the Effects RSiena Actually Offers for a Coevolution Data Object
#'
#' Regenerates the effect table from a live \code{getEffects()} call, split by
#' dependent variable, and separately reports the CROSS-NETWORK rows --- those
#' whose \code{interaction1} or \code{interaction2} names another dependent
#' variable. Use this rather than trusting documentation, including this
#' package's own.
#'
#' The cross-network block is the one worth reading. For a bipartite DV paired
#' with a one-mode DV on its SECOND mode, that block is very short, and its
#' shortness is a fact about RSiena's implemented effect groups rather than
#' evidence that the mechanisms are absent from the data.
#'
#' @param dat A data object from \code{\link{searchnet_coevolve_data}}, or any
#'   \code{siena} object.
#' @param type Character vector of effect types to report. Default
#'   \code{"eval"}; use \code{c("eval", "endow", "creation")} for the full set.
#' @param cross_only Logical. Report only rows referencing another dependent
#'   variable. Default \code{FALSE}.
#' @param print Logical. Print a formatted report. Default \code{TRUE}.
#'
#' @return Invisibly, a list with elements \code{by_dv} (a named list of data
#'   frames) and \code{cross} (a data frame of cross-network rows).
#'
#' @export
#' @examples
#' \dontrun{
#' dat <- searchnet_coevolve_data(bip, arc)
#' searchnet_coevolve_available_effects(dat, cross_only = TRUE)
#' }
searchnet_coevolve_available_effects <- function(dat,
                                                 type       = "eval",
                                                 cross_only = FALSE,
                                                 print      = TRUE) {
  if (!inherits(dat, "siena"))
    stop("`dat` must be a siena data object.", call. = FALSE)

  eff <- RSiena::getEffects(dat)
  dvn <- base::names(dat$depvars)
  eff <- eff[eff$type %in% type, , drop = FALSE]

  cross <- eff[eff$interaction1 %in% dvn | eff$interaction2 %in% dvn, ,
               drop = FALSE]
  cross <- data.frame(
    dv           = cross$name,
    type         = cross$type,
    shortName    = cross$shortName,
    interaction1 = cross$interaction1,
    interaction2 = cross$interaction2,
    effectName   = cross$effectName,
    stringsAsFactors = FALSE
  )

  by_dv <- lapply(dvn, function(v) {
    s <- eff[eff$name == v, , drop = FALSE]
    data.frame(
      type         = s$type,
      shortName    = s$shortName,
      interaction1 = s$interaction1,
      interaction2 = s$interaction2,
      included     = s$include,
      effectName   = s$effectName,
      stringsAsFactors = FALSE
    )
  })
  base::names(by_dv) <- dvn

  if (print) {
    cat("\n=== RSiena effects available for this data object ===\n")
    cat("    RSiena ", as.character(utils::packageVersion("RSiena")),
        "; types reported: ", paste(type, collapse = ", "), "\n", sep = "")
    if (!cross_only) {
      for (v in dvn) {
        cat(sprintf("\n-- DV '%s' : %d effects --\n", v, nrow(by_dv[[v]])))
        print(utils::head(by_dv[[v]][, c("type", "shortName", "interaction1",
                                         "interaction2", "included")], 200L),
              row.names = FALSE)
      }
    }
    cat("\n-- CROSS-NETWORK effects (interaction names another DV) --\n")
    if (!nrow(cross)) {
      cat("  NONE. This is a NON-IMPLEMENTATION, not a zero effect: RSiena\n",
          "  offers no cross-network effect for this node-set pairing. It is\n",
          "  not evidence that the networks fail to influence one another.\n",
          sep = "")
    } else {
      print(cross, row.names = FALSE)
    }
    cat("\n")
  }

  invisible(list(by_dv = by_dv, cross = cross))
}


# --------------------------------------------------------------------------- #
#  searchnet_coevolve_effects
# --------------------------------------------------------------------------- #

#' Starting Effects for a Bipartite-Plus-Architecture Coevolution Model
#'
#' Builds a defensible starting specification with structural effects on BOTH
#' dependent networks, then VERIFIES that every requested effect is actually
#' present with \code{include == TRUE} and stops naming any that is not.
#'
#' @section Why the verification pass exists:
#' \code{includeEffects()} matches on \code{name}, \code{shortName},
#' \code{interaction1} and \code{interaction2}. When any of those does not
#' match a row, RSiena can return the effects object unchanged. The model then
#' estimates cleanly, prints a tidy table, and silently omits the effect the
#' analyst believed they were testing. Every request made here is therefore
#' checked after the fact, and a missing effect is an error rather than a
#' warning.
#'
#' @section Defaults:
#' On the component (architecture) DV: \code{density}, \code{recip},
#' \code{transTrip}. On the bipartite DV: \code{density} (auto-included by
#' \code{getEffects}), \code{inPop}, \code{outAct}, \code{cycle4}. On the
#' behaviour DV, if present: \code{linear}, \code{quad}.
#'
#' The cross-network requests are governed separately, because they are the
#' scientific point and because they are the ones that may not exist:
#' \code{architecture_from_organisation} adds \code{from.w.ind}, and
#' \code{organisation_from_architecture} adds \code{XWX} on the lagged
#' architecture covariate --- a lagged exogenous estimand, NOT a coevolution
#' parameter. See the file header.
#'
#' @param dat A data object from \code{\link{searchnet_coevolve_data}}.
#' @param bipartite_effects Character vector of shortNames for the bipartite DV,
#'   or \code{NULL} for the default set.
#' @param component_effects Character vector of shortNames for the component DV,
#'   or \code{NULL} for the default set.
#' @param behavior_effects Character vector of shortNames for the behaviour DV,
#'   or \code{NULL} for the default set. Ignored when there is no behaviour DV.
#' @param architecture_from_organisation Logical. Include \code{from.w.ind} on
#'   the component DV: architecture ties form along lines of shared occupancy.
#'   This is the only genuine cross-network effect between the two dependent
#'   networks. Default \code{TRUE}.
#' @param organisation_from_architecture Logical. Include \code{XWX} on the
#'   bipartite DV using the lagged architecture covariate. Requires
#'   \code{architecture_lag = TRUE} at data-build time. Default \code{TRUE}
#'   when that covariate exists, and silently skipped when it does not unless
#'   \code{strict = TRUE}. Read the caveat in the file header before reporting
#'   this coefficient.
#' @param extra A list of additional effect requests, each a list with
#'   \code{name}, \code{shortName} and optionally \code{interaction1},
#'   \code{interaction2}, \code{type}. Verified on the same terms as the rest.
#' @param strict Logical. Treat an unavailable-but-requested cross-network
#'   effect as an error rather than a skip. Default \code{TRUE}.
#' @param verbose Logical. Print the inclusion table. Default \code{TRUE}.
#'
#' @return A \code{sienaEffects} object, with attribute
#'   \code{"searchnet_coev_requests"} recording the verified request table.
#'
#' @export
#' @examples
#' \dontrun{
#' dat <- searchnet_coevolve_data(bip, arc)
#' eff <- searchnet_coevolve_effects(dat)
#' ## architecture structure only, no cross-network claim:
#' eff0 <- searchnet_coevolve_effects(dat,
#'           architecture_from_organisation = FALSE,
#'           organisation_from_architecture = FALSE)
#' }
searchnet_coevolve_effects <- function(dat,
                                       bipartite_effects = NULL,
                                       component_effects = NULL,
                                       behavior_effects  = NULL,
                                       architecture_from_organisation = TRUE,
                                       organisation_from_architecture = TRUE,
                                       extra   = NULL,
                                       strict  = TRUE,
                                       verbose = TRUE) {

  meta <- attr(dat, "searchnet_coev")
  if (is.null(meta))
    stop("`dat` was not built by searchnet_coevolve_data(); the DV names ",
         "needed to address effects are unknown. Rebuild it, or call ",
         "RSiena::getEffects() directly.", call. = FALSE)

  nm  <- meta$names
  bpn <- nm[["bipartite"]]
  cpn <- nm[["component"]]
  bhn <- nm[["behavior"]]

  if (is.null(component_effects))
    component_effects <- c("density", "recip", "transTrip")
  if (is.null(bipartite_effects))
    bipartite_effects <- c("density", "inPop", "outAct", "cycle4")
  if (is.null(behavior_effects))
    behavior_effects <- c("linear", "quad")

  requests <- list()
  for (s in component_effects)
    requests[[length(requests) + 1L]] <- list(name = cpn, shortName = s)
  for (s in bipartite_effects)
    requests[[length(requests) + 1L]] <- list(name = bpn, shortName = s)
  if (isTRUE(meta$has_behavior))
    for (s in behavior_effects)
      requests[[length(requests) + 1L]] <- list(name = bhn, shortName = s)

  ## ---- cross-network channel 1: architecture <- organisation --------------
  if (isTRUE(architecture_from_organisation))
    requests[[length(requests) + 1L]] <- list(
      name = cpn, shortName = "from.w.ind",
      interaction1 = bpn, interaction2 = bpn)

  ## ---- cross-network channel 2: organisation <- architecture (LAGGED) -----
  if (isTRUE(organisation_from_architecture)) {
    if (is.na(meta$lag_name)) {
      msg <- paste0(
        "organisation_from_architecture = TRUE requires the lagged ",
        "architecture covariate, which this data object does not carry ",
        "(architecture_lag was FALSE). Between two DEPENDENT networks in this ",
        "node-set pairing, RSiena 1.5.0 implements no effect for this ",
        "direction at all -- that is a NON-IMPLEMENTATION, not a null.")
      if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
    } else {
      requests[[length(requests) + 1L]] <- list(
        name = bpn, shortName = "XWX", interaction1 = meta$lag_name)
    }
  }

  if (!is.null(extra)) {
    if (!is.list(extra))
      stop("`extra` must be a list of effect requests.", call. = FALSE)
    for (i in seq_along(extra)) {
      e <- extra[[i]]
      if (!is.list(e) || is.null(e$name) || is.null(e$shortName))
        stop("`extra[[", i, "]]` needs both `name` and `shortName`.",
             call. = FALSE)
      requests[[length(requests) + 1L]] <- e
    }
  }

  eff <- RSiena::getEffects(dat)

  ## Apply. An includeEffects() failure on one request must not abort the rest:
  ## the verification pass below reports ALL failures at once, which is more
  ## useful than the first one.
  for (r in requests) {
    ok <- try(eff <- .searchnet_include_one(eff, r), silent = TRUE)
    if (inherits(ok, "try-error")) next
  }

  ## ---- VERIFY. Silent effect-dropping is the defect this guards against. --
  status <- .searchnet_effect_status(eff, requests)

  if (verbose) {
    cat("\nsearchnet_coevolve_effects(): requested effects\n")
    print(status, row.names = FALSE)
    inc <- eff[eff$include, , drop = FALSE]
    cat(sprintf("\n  %d effects included in total (including rates auto-added ",
                nrow(inc)),
        "by getEffects).\n", sep = "")
  }

  bad <- status[!status$included, , drop = FALSE]
  if (nrow(bad)) {
    ## Index by row rather than apply(): apply() coerces the data frame to a
    ## character matrix and pads the logical columns, so `exists` would arrive
    ## as " TRUE"/"FALSE" and an identical() test on it would be wrong.
    lines <- vapply(seq_len(nrow(bad)), function(i) {
      sprintf("    %-14s on DV '%s' (type %s%s%s) -- %s",
              bad$shortName[i], bad$dv[i], bad$type[i],
              if (nzchar(bad$interaction1[i]))
                paste0(", interaction1 = '", bad$interaction1[i], "'") else "",
              if (nzchar(bad$interaction2[i]))
                paste0(", interaction2 = '", bad$interaction2[i], "'") else "",
              if (!bad$exists[i])
                "NO SUCH ROW in getEffects (non-implementation for this pairing)"
              else "row exists but include is FALSE")
    }, character(1))
    stop("The following requested effects are NOT included, and the model ",
         "would have estimated without them:\n",
         paste(lines, collapse = "\n"),
         "\n  Rows marked NO SUCH ROW are non-implementations for this ",
         "node-set pairing: RSiena does not offer the effect, which says ",
         "nothing about whether the mechanism operates. Call ",
         "searchnet_coevolve_available_effects(dat) to see what does exist, ",
         "and do not report a non-implementation as a zero.",
         call. = FALSE)
  }

  attr(eff, "searchnet_coev_requests") <- status
  eff
}


# --------------------------------------------------------------------------- #
#  searchnet_coevolve
# --------------------------------------------------------------------------- #

#' Estimate a Bipartite-Plus-Architecture Coevolution SAOM
#'
#' Thin wrapper around \code{\link[RSiena]{siena07}} with defaults suited to a
#' two-dependent-network model, a convergence report that keeps the overall
#' convergence ratio and the per-parameter t-ratios apart, and a hard stop when
#' the covariance matrix of the estimates is unavailable.
#'
#' @section The two convergence quantities are not the same thing:
#' The manual's guidance for publishable results is \strong{overall maximum
#' convergence ratio < 0.25}, a single number summarising the whole fit
#' (\code{fit$tconv.max}). The \strong{per-parameter t-ratios for deviations
#' from targets} (\code{fit$tconv}) are a vector, conventionally required below
#' 0.10 in absolute value. They answer different questions, and reporting one
#' under the other's threshold is loose. Both are printed, labelled, and
#' returned.
#'
#' @section Non-identification is not a fit:
#' When \code{diag(fit$covtheta)} is entirely \code{NA}, no standard errors
#' exist. The point estimates are then unquotable and the correct report is a
#' non-identification together with the tie densities of both dependent
#' networks --- not a table of coefficients without stars. This function stops
#' rather than returning such an object.
#'
#' @param dat A data object from \code{\link{searchnet_coevolve_data}}.
#' @param eff A \code{sienaEffects} object, normally from
#'   \code{\link{searchnet_coevolve_effects}}.
#' @param projname Character. RSiena project name for the output file.
#' @param n3 Integer. Phase-3 simulations. Default \code{1000}.
#' @param nsub Integer. Phase-2 subphases. Default \code{4}.
#' @param seed Integer or \code{NULL}. Passed to
#'   \code{\link[RSiena]{sienaAlgorithmCreate}}. Fix it across any comparison:
#'   an unseeded difference includes estimator noise the analyst introduced.
#' @param nbrNodes Integer. Parallel nodes. Default \code{1}. Hold this FIXED
#'   across any models being compared, for the same reason as \code{seed}.
#' @param useCluster Logical. Default \code{nbrNodes > 1}.
#' @param returnChains Logical. Return the simulated ministep chains. Default
#'   \code{TRUE}; set \code{FALSE} for large models, where the chains dominate
#'   the object size.
#' @param returnDeps Logical. Return simulated networks. Default \code{TRUE}.
#' @param prevAns A previous \code{sienaFit} to take starting values from.
#' @param batch Logical. Non-interactive reporting. Default \code{TRUE}.
#' @param silent Logical. Suppress RSiena's own progress output. Default
#'   \code{FALSE}.
#' @param stop_on_nonidentification Logical. Stop when \code{diag(covtheta)} is
#'   all \code{NA}. Default \code{TRUE}. Setting it \code{FALSE} returns the
#'   object for inspection but does not make the estimates quotable.
#' @param verbose Logical. Print the convergence report. Default \code{TRUE}.
#' @param ... Further arguments passed to \code{\link[RSiena]{siena07}}.
#'
#' @return The \code{sienaFit} object, with attribute
#'   \code{"searchnet_convergence"} carrying \code{tconv_max} (overall ratio),
#'   \code{tconv} (per-parameter t-ratios), \code{se_available} and the two
#'   conventional thresholds.
#'
#' @export
#' @examples
#' \dontrun{
#' dat <- searchnet_coevolve_data(bip, arc)
#' eff <- searchnet_coevolve_effects(dat)
#' fit <- searchnet_coevolve(dat, eff, n3 = 1000, seed = 42)
#' }
searchnet_coevolve <- function(dat,
                               eff,
                               projname     = "searchnet_coevolve",
                               n3           = 1000L,
                               nsub         = 4L,
                               seed         = NULL,
                               nbrNodes     = 1L,
                               useCluster   = nbrNodes > 1L,
                               returnChains = TRUE,
                               returnDeps   = TRUE,
                               prevAns      = NULL,
                               batch        = TRUE,
                               silent       = FALSE,
                               stop_on_nonidentification = TRUE,
                               verbose      = TRUE,
                               ...) {

  if (!inherits(dat, "siena"))
    stop("`dat` must be a siena data object.", call. = FALSE)
  if (!inherits(eff, "sienaEffects"))
    stop("`eff` must be a sienaEffects object.", call. = FALSE)

  alg <- RSiena::sienaAlgorithmCreate(
    projname = projname, n3 = as.integer(n3), nsub = as.integer(nsub),
    seed = seed, silent = silent)

  call_args <- list(alg, data = dat, effects = eff,
                    batch = batch, silent = silent,
                    returnDeps = returnDeps, returnChains = returnChains,
                    nbrNodes = as.integer(nbrNodes), useCluster = useCluster)
  if (!is.null(prevAns)) call_args$prevAns <- prevAns
  call_args <- c(call_args, list(...))

  fit <- do.call(RSiena::siena07, call_args)

  ## ---- the two convergence quantities, kept apart ------------------------
  tconv_max <- if (!is.null(fit$tconv.max)) as.numeric(fit$tconv.max) else NA_real_
  tconv     <- if (!is.null(fit$tconv)) as.numeric(fit$tconv) else rep(NA_real_, length(fit$theta))

  ## ---- identification check ----------------------------------------------
  cov_diag <- if (!is.null(fit$covtheta)) diag(as.matrix(fit$covtheta)) else NA_real_
  se_available <- any(is.finite(cov_diag))

  enames <- if (!is.null(fit$effects$effectName))
    fit$effects$effectName else rep("", length(tconv))

  if (verbose) {
    cat("\n=== searchnet_coevolve(): convergence ===\n\n")
    cat("[1] OVERALL MAXIMUM CONVERGENCE RATIO (tconv.max)\n")
    cat("    A single summary of the whole fit. The manual's guidance for\n")
    cat("    publishable results is < 0.25.\n")
    cat(sprintf("    tconv.max = %s   -> %s\n\n",
                if (is.na(tconv_max)) "NA" else sprintf("%.4f", tconv_max),
                if (is.na(tconv_max)) "NOT REPORTED by RSiena"
                else if (tconv_max < 0.25) "meets the < 0.25 guidance"
                else "DOES NOT meet the < 0.25 guidance"))
    cat("[2] PER-PARAMETER t-RATIOS FOR DEVIATIONS FROM TARGETS (tconv)\n")
    cat("    A vector, one per estimated parameter. A different quantity from\n")
    cat("    [1]; conventionally required below 0.10 in absolute value.\n")
    n <- min(length(tconv), length(enames))
    if (n) {
      tab <- data.frame(
        parameter = substr(enames[seq_len(n)], 1L, 52L),
        t_ratio   = round(tconv[seq_len(n)], 4L),
        ok        = abs(tconv[seq_len(n)]) < 0.10,
        stringsAsFactors = FALSE)
      print(tab, row.names = FALSE)
      nbad <- sum(!tab$ok, na.rm = TRUE)
      cat(sprintf("\n    %d of %d parameters exceed |t| = 0.10.\n", nbad, n))
    }
    cat("\n    These two are NOT interchangeable. Reporting the overall ratio\n")
    cat("    as if it were a t-ratio, or vice versa, is loose.\n")
    cat(sprintf("\n[3] IDENTIFICATION: standard errors %s\n",
                if (se_available) "available" else "NOT AVAILABLE"))
  }

  if (!se_available) {
    msg <- paste0(
      "diag(covtheta) is entirely NA: no standard errors could be computed. ",
      "This is a NON-IDENTIFICATION, not a fit, and the point estimates are ",
      "unquotable. Report it as a non-identification together with the tie ",
      "densities of both dependent networks. Common causes here: an ",
      "architecture network too sparse or too dense to carry the requested ",
      "structural effects, a cross-network effect with almost no supporting ",
      "configurations, or too few waves for the number of parameters.")
    if (stop_on_nonidentification) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  attr(fit, "searchnet_convergence") <- list(
    tconv_max          = tconv_max,
    tconv              = tconv,
    se_available       = se_available,
    threshold_overall  = 0.25,
    threshold_tratio   = 0.10
  )

  fit
}
