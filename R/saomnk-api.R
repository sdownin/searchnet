#' @title User-Friendly API for SaoMNK Simulations
#' @description
#' Wrapper functions that provide a clean interface to the searchnet simulation
#' engine. Users never need to construct internal \code{self$} references,
#' RSiena effect naming conventions, or deeply nested list structures.
#'
#' @name saomnk-api
#' @import Matrix
NULL


# ---------------------------------------------------------------------------- #
# Internal: coerce a time-varying coupling to an N x N x P array                 #
# ---------------------------------------------------------------------------- #

#' Coerce a time-varying coupling structure to an N x N x P array
#'
#' Accepts either an \eqn{N \times N \times P} numeric array or a list of
#' \eqn{P} \eqn{N \times N} matrices and returns the array form that
#' \code{sienaDependent}'s \code{varDyadCovar} companion expects.  P counts
#' PERIODS, one fewer than the number of waves; the wave count itself cannot be
#' checked here because the model object does not see the dependent variable, so
#' the engine re-validates it against the actual DV.
#'
#' Diagonals are zeroed.  A coupling of a component with itself is not a
#' coupling, and leaving it non-zero silently inflates every XWX statistic,
#' which is exactly the class of error that produces a converged model of a
#' specification the author did not write.
#'
#' @param x An array or a list of matrices.
#' @param nm Name of the coupling, used only in error messages.
#' @return A numeric \eqn{N \times N \times P} array.
#' @keywords internal
#' @noRd
.saomnk_as_dyad_array <- function(x, nm = "W") {
  lbl <- sprintf("`influence_arrays[[\"%s\"]]`", nm)

  if (is.list(x) && !is.array(x)) {
    if (length(x) < 1L)
      stop(lbl, " is an empty list; it needs one matrix per period.",
           call. = FALSE)
    ok <- vapply(x, function(m) is.matrix(m) && is.numeric(m), logical(1))
    if (!all(ok))
      stop(lbl, " must be a list of numeric matrices; element(s) ",
           paste(which(!ok), collapse = ", "), " are not.", call. = FALSE)
    dims <- vapply(x, dim, integer(2))
    if (length(unique(as.vector(dims))) != 1L)
      stop(lbl, " mixes matrix dimensions: ",
           paste(apply(dims, 2, paste, collapse = "x"), collapse = ", "),
           ". Every period must share one N x N shape.", call. = FALSE)
    N <- dims[1, 1]
    out <- array(0, dim = c(N, N, length(x)))
    for (i in seq_along(x)) out[, , i] <- x[[i]]
    dimnames(out) <- list(rownames(x[[1]]), colnames(x[[1]]), NULL)
  } else if (is.array(x) && length(dim(x)) == 3L) {
    if (!is.numeric(x)) stop(lbl, " must be numeric.", call. = FALSE)
    if (dim(x)[1] != dim(x)[2])
      stop(lbl, " is ", paste(dim(x), collapse = "x"),
           "; the first two dimensions must be equal (N x N x P).",
           call. = FALSE)
    out <- x
    storage.mode(out) <- "double"
  } else if (is.matrix(x)) {
    stop(lbl, " is a single matrix. A time-varying coupling needs one matrix ",
         "per period; pass it through `epistasis_matrices` if it is static.",
         call. = FALSE)
  } else {
    stop(lbl, " must be an N x N x P array or a list of P N x N matrices.",
         call. = FALSE)
  }

  if (dim(out)[3] < 1L)
    stop(lbl, " has no periods.", call. = FALSE)
  if (anyNA(out))
    stop(lbl, " contains NA; couplings must be complete.", call. = FALSE)
  for (i in seq_len(dim(out)[3])) diag(out[, , i]) <- 0
  out
}

# ---------------------------------------------------------------------------- #
#  Internal constants and helpers
# ---------------------------------------------------------------------------- #

## The canonical RSiena DV reference used throughout the structure model.
.DV_NAME <- "self$bipartite_rsienaDV"

## Map user-friendly effect names to RSiena shortcodes
.EFFECT_MAP <- c(

  density    = "density",
  popularity = "inPop",
  scope      = "outAct",
  epistasis  = "XWX"
)


#' Create a Block-Diagonal Epistasis Matrix
#'
#' Constructs an \eqn{N \times N}{N x N} binary block-diagonal matrix
#' representing modular component interaction structure.  Each block is a
#' fully connected sub-group, and components in different blocks do not
#' interact.
#'
#' @param N Integer. Number of components (matrix dimension).
#' @param blocks Integer. Number of modules (blocks).
#' @return An \eqn{N \times N}{N x N} numeric matrix of 0s and 1s.
#' @export
#' @examples
#' ## 12 components in 4 modules of 3
#' saomnk_block_diagonal(12, 4)
saomnk_block_diagonal <- function(N, blocks) {
  stopifnot(is.numeric(N), length(N) == 1, N >= 1)
  stopifnot(is.numeric(blocks), length(blocks) == 1, blocks >= 1, blocks <= N)
  block_sizes <- rep(N %/% blocks, blocks)
  remainder   <- N %% blocks
  if (remainder > 0) {
    block_sizes[seq_len(remainder)] <- block_sizes[seq_len(remainder)] + 1
  }
  block_list <- lapply(block_sizes, function(s) matrix(1, nrow = s, ncol = s))
  as.matrix(Matrix::bdiag(block_list))[1:N, 1:N]
}


# ---------------------------------------------------------------------------- #
#  saomnk_env
# ---------------------------------------------------------------------------- #

#' Create a SaoMNK Search Environment
#'
#' Initialises a bipartite search environment of \eqn{M} actors and \eqn{N}
#' components.  This is the starting point for any SaoMNK simulation.
#'
#' @param M Integer. Number of actors (firms, agents).
#' @param N Integer. Number of components (resources, technologies, markets).
#' @param density Numeric in \eqn{[0, 1]}.
#'   Initial bipartite network density.
#'   \code{0} (default) means all actors start unaffiliated.
#' @param seed Integer or \code{NULL}. Random seed for reproducibility of the
#'   initial network draw.
#' @param name Character or \code{NULL}. Optional label for the environment
#'   (used in file paths and plot titles).
#' @return A \code{SaomNkRSienaBiEnv} R6 object.
#' @export
#' @examples
#' env <- saomnk_env(M = 6, N = 12, seed = 42)
saomnk_env <- function(M, N, density = 0, seed = NULL, name = NULL) {

  stopifnot(is.numeric(M), length(M) == 1, M >= 1)
  stopifnot(is.numeric(N), length(N) == 1, N >= 1)
  stopifnot(is.numeric(density), length(density) == 1, density >= 0, density <= 1)

  params <- list(
    M       = as.integer(M),
    N       = as.integer(N),
    BI_PROB = density
  )
  if (!is.null(seed))
    params$rand_seed <- as.integer(seed)
  if (!is.null(name))
    params$name <- name

  SaomNkRSienaBiEnv$new(params)
}


# ---------------------------------------------------------------------------- #
#  saomnk_model
# ---------------------------------------------------------------------------- #

#' Define a SaoMNK Structure Model
#'
#' Builds the SAOM objective function specification that governs actor search
#' behaviour.
#' Users specify effect weights using plain-language parameters; the function
#' assembles the nested list with internal RSiena naming conventions so that
#' callers never need to write \code{"self$bipartite_rsienaDV"} or similar
#' references.
#'
#' @param density Numeric. Weight on the density (intercept) effect.
#'   Negative values produce sparse networks (default \code{-0.5}).
#' @param popularity Numeric. Weight on the in-degree popularity effect
#'   (\code{inPop} in RSiena). Controls preferential attachment to
#'   high-popularity components (default \code{0}).
#' @param scope Numeric. Weight on the out-degree activity effect
#'   (\code{outAct} in RSiena). Controls actors' tendency to expand scope
#'   (default \code{0}).
#' @param influence_matrix An \eqn{N \times N}{N x N} numeric matrix \eqn{W}
#'   encoding pairwise component interactions -- which components affect one
#'   another, and with what sign -- or \code{NULL} (default) to omit the XWX
#'   effect.  Use \code{\link{saomnk_block_diagonal}} for convenient modular
#'   structures.
#'
#'   This is the \emph{input} to the model: the influence matrix in the sense
#'   of Rivkin and Siggelkow (2007).  It is distinct from epistasis, which is
#'   the \emph{consequence} -- a portfolio's fitness running through \eqn{W}
#'   as \eqn{X'WX}.  The realised epistasis of a simulated system is reported
#'   separately as \eqn{K_{CC}}; see \code{\link{saomnk_get_degrees}}.
#' @param influence_weight Numeric. Weight on the XWX effect (default
#'   \code{0.1}).  Ignored when \code{influence_matrix} is \code{NULL}.
#' @param influence_matrices Named list of \eqn{N \times N}{N x N} numeric
#'   matrices encoding multiple pairwise component interaction structures,
#'   e.g. \code{list(W_mod = modular_matrix, W_hier = hier_matrix)}.  Each
#'   matrix generates a separate \code{coDyadCovar} entry with an XWX effect.
#'   When provided, \code{influence_matrix} is ignored.
#' @param influence_weights Named numeric vector of weights for each matrix in
#'   \code{influence_matrices}.  Names must match those in
#'   \code{influence_matrices}.  If \code{NULL} (default), all matrices use
#'   \code{influence_weight}.
#' @param influence_arrays Named list of \emph{time-varying} coupling
#'   structures, one entry per \eqn{W}.  Each entry is either an
#'   \eqn{N \times N \times P}{N x N x P} numeric array or a list of \eqn{P}
#'   \eqn{N \times N}{N x N} matrices, where \eqn{P} is the number of
#'   \emph{periods} in the dependent variable, that is, one fewer than the
#'   number of waves.  Each entry generates an RSiena \code{varDyadCovar} with
#'   an \code{XWX} effect, so a coupling can change between periods rather than
#'   being held constant across the whole panel.  Static
#'   \code{influence_matrices} and time-varying \code{influence_arrays} may be
#'   supplied together; they occupy separate slots and both are estimated.
#' @param influence_array_weights Named numeric vector of weights for each
#'   entry in \code{influence_arrays}.  Names must match.  If \code{NULL}
#'   (default), all use \code{influence_weight}.
#' @param epistasis_matrix,epistasis_weight,epistasis_matrices,epistasis_weights
#'   \strong{Deprecated} in 0.4.0; renamed to the four \code{influence_*}
#'   arguments above.  The old names still work and are passed through
#'   unchanged, but emit a warning.  The rename corrects a semantic
#'   conflation: \eqn{W} is an influence (interaction) matrix; epistasis is
#'   what it produces.
#' @param dyad_covariates Named list of \eqn{M \times N}{M x N} actor-by-
#'   component covariate matrices.  Each entry may optionally carry a
#'   \code{"weight"} attribute (numeric; default \code{0.1}) and an
#'   \code{"effect"} attribute (character; default \code{"egoXaltX"}).
#' @param strategies Named list of numeric vectors supplying actor-level
#'   covariates.
#'   Each element is an M-length vector of actor attributes; the list name
#'   determines the RSiena covariate effect:
#'   \describe{
#'     \item{\code{"egoX"}}{Ego (actor scope) covariate}
#'     \item{\code{"inPopX"}}{Popularity-seeking covariate}
#'     \item{\code{"altX"}}{Alter (component) covariate}
#'   }
#'   Each entry may optionally carry a \code{"weight"} attribute (numeric);
#'   if absent the default weight is \code{0.2}.
#' @param component_covariates Named list of N-length numeric vectors for
#'   constant component covariates (e.g., component quality scores).
#'   Names become the RSiena effect type (e.g., \code{"altX"}).
#'   Each entry may carry a \code{"weight"} attribute; default is \code{0.2}.
#' @param dyad_covariate An \eqn{M \times N}{M x N} actor-by-component
#'   covariate matrix, or \code{NULL} (default).
#' @param dyad_covariate_effect Character. RSiena effect name for
#'   \code{dyad_covariate} (default \code{"X"}, the dyadic-covariate effect
#'   for a bipartite dependent variable; \code{egoXaltX} is a one-mode
#'   effect and is NOT valid here).
#' @param dyad_covariate_weight Numeric. Weight for the dyad covariate effect
#'   (default \code{0.1}).
#' @param \dots Additional effects specified as named lists and appended to the
#'   effects section.  Each must include at minimum \code{effect} (character)
#'   and \code{parameter} (numeric) entries.
#' @return A list with class \code{"saomnk_model"} ready to pass to
#'   \code{\link{saomnk_run}}.
#' @export
#' @examples
#' ## Minimal: density-only model
#' mod <- saomnk_model(density = -1)
#'
#' ## With epistasis
#' K <- saomnk_block_diagonal(12, 4)
#' mod <- saomnk_model(density = -0.5, popularity = 0.2, epistasis_matrix = K)
#'
#' ## With heterogeneous actor strategies
#' mod <- saomnk_model(
#'   density = -0.3,
#'   popularity = 0.2,
#'   scope = 0.1,
#'   epistasis_matrix = saomnk_block_diagonal(12, 4),
#'   strategies = list(
#'     egoX   = c(-1, 0, 1, -1, 0, 1),
#'     inPopX = c( 1, 0,-1,  1, 0,-1)
#'   )
#' )
#'
#' ## With multiple W-matrices
#' mod <- saomnk_model(
#'   density = -0.5,
#'   epistasis_matrices = list(
#'     W_modular = saomnk_block_diagonal(12, 4),
#'     W_full    = matrix(1, 12, 12)
#'   ),
#'   epistasis_weights = c(W_modular = 0.2, W_full = -0.1)
#' )
#'
#' ## With a coupling that CHANGES between periods (three waves, two periods)
#' mod <- saomnk_model(
#'   density = -0.5,
#'   influence_arrays = list(
#'     W_regime = array(c(saomnk_block_diagonal(12, 4),
#'                        saomnk_block_diagonal(12, 2)), dim = c(12, 12, 2))
#'   ),
#'   influence_array_weights = c(W_regime = 0.2)
#' )
saomnk_model <- function(density            = -0.5,
                          popularity         = 0,
                          scope              = 0,
                          influence_matrix   = NULL,
                          influence_weight   = 0.1,
                          influence_matrices = NULL,
                          influence_weights  = NULL,
                          influence_arrays   = NULL,
                          influence_array_weights = NULL,
                          epistasis_matrix   = NULL,
                          epistasis_weight   = NULL,
                          epistasis_matrices = NULL,
                          epistasis_weights  = NULL,
                          dyad_covariates    = NULL,
                          strategies         = NULL,
                          component_covariates = NULL,
                          dyad_covariate     = NULL,
                          dyad_covariate_effect = "X",
                          dyad_covariate_weight = 0.1,
                          ...) {

  ## -- 0. Deprecated arguments (renamed in 0.4.0) ------------------------- ##
  ## `epistasis_*` -> `influence_*`.  W is the influence / interaction matrix
  ## (the INPUT); epistasis is the fitness coupling it produces (the EFFECT),
  ## reported as K_CC.  Old names keep working, with a warning.
  .dep <- function(old_val, old_nm, new_nm, new_val, default = NULL) {
    if (is.null(old_val)) return(new_val)
    warning("`", old_nm, "` is deprecated as of searchnet 0.4.0; use `",
            new_nm, "` instead. W is the influence (interaction) matrix, ",
            "the model INPUT; epistasis is the resulting fitness coupling, ",
            "reported as K_CC.", call. = FALSE)
    if (identical(new_val, default)) old_val else new_val
  }
  influence_matrix   <- .dep(epistasis_matrix,   "epistasis_matrix",
                             "influence_matrix",   influence_matrix,   NULL)
  influence_weight   <- .dep(epistasis_weight,   "epistasis_weight",
                             "influence_weight",   influence_weight,   0.1)
  influence_matrices <- .dep(epistasis_matrices, "epistasis_matrices",
                             "influence_matrices", influence_matrices, NULL)
  influence_weights  <- .dep(epistasis_weights,  "epistasis_weights",
                             "influence_weights",  influence_weights,  NULL)

  ## -- 1. Core effects ---------------------------------------------------- ##

  effects_list <- list(
    list(effect = "density", parameter = density,
         dv_name = .DV_NAME, fix = TRUE)
  )

  if (popularity != 0) {
    effects_list[[length(effects_list) + 1]] <-
      list(effect = "inPop", parameter = popularity,
           dv_name = .DV_NAME, fix = TRUE)
  }

  if (scope != 0) {
    effects_list[[length(effects_list) + 1]] <-
      list(effect = "outAct", parameter = scope,
           dv_name = .DV_NAME, fix = TRUE)
  }

  ## Absorb extra named effects from `...`
  extra <- list(...)
  for (ex in extra) {
    if (is.list(ex) && !is.null(ex$effect)) {
      if (is.null(ex$dv_name)) ex$dv_name <- .DV_NAME
      if (is.null(ex$fix))     ex$fix     <- TRUE
      effects_list[[length(effects_list) + 1]] <- ex
    }
  }

  ## -- 2. Actor strategy covariates (coCovars) ---------------------------- ##

  coCovars_list <- list()

  if (!is.null(strategies)) {
    stopifnot(is.list(strategies))
    strat_counter <- 0L
    for (nm in names(strategies)) {
      strat_counter <- strat_counter + 1L
      weight <- attr(strategies[[nm]], "weight")
      if (is.null(weight)) weight <- 0.2
      ## Strip extra attributes so RSiena's coCovar() sees a plain vector
      x_clean <- as.vector(strategies[[nm]])
      coCovars_list[[length(coCovars_list) + 1]] <-
        list(
          effect       = nm,
          parameter    = weight,
          dv_name      = .DV_NAME,
          fix          = TRUE,
          interaction1 = sprintf("self$strat_%d_coCovar", strat_counter),
          x            = x_clean
        )
    }
  }

  ## Component-level constant covariates (also go into coCovars with
  ## COMPONENTS nodeSet)
  if (!is.null(component_covariates)) {
    stopifnot(is.list(component_covariates))
    comp_cov_counter <- 0L
    for (nm in names(component_covariates)) {
      comp_cov_counter <- comp_cov_counter + 1L
      weight <- attr(component_covariates[[nm]], "weight")
      if (is.null(weight)) weight <- 0.2
      coCovars_list[[length(coCovars_list) + 1]] <-
        list(
          effect       = nm,
          parameter    = weight,
          dv_name      = .DV_NAME,
          fix          = TRUE,
          interaction1 = sprintf("self$component_%d_coCovar", comp_cov_counter),
          x            = component_covariates[[nm]]
        )
    }
  }

  ## -- 3. Epistasis (coDyadCovars) ---------------------------------------- ##

  coDyadCovars_list <- list()

  if (!is.null(influence_matrices)) {
    ## Multiple W-matrices: influence_matrices is a named list
    stopifnot(is.list(influence_matrices), !is.null(names(influence_matrices)))
    for (nm in names(influence_matrices)) {
      w_mat <- influence_matrices[[nm]]
      stopifnot(is.matrix(w_mat) || inherits(w_mat, "Matrix"))
      w_mat <- as.matrix(w_mat)
      ## Per-matrix weight: look up in influence_weights, fall back to influence_weight
      w_weight <- if (!is.null(influence_weights) && nm %in% names(influence_weights)) {
        influence_weights[[nm]]
      } else {
        influence_weight
      }
      slot <- length(coDyadCovars_list) + 1L
      coDyadCovars_list[[slot]] <-
        list(
          effect       = "XWX",
          parameter    = w_weight,
          dv_name      = .DV_NAME,
          fix          = TRUE,
          nodeSet      = c("COMPONENTS", "COMPONENTS"),
          interaction1 = sprintf("self$component_%d_coDyadCovar", slot),
          x            = w_mat
        )
    }
  } else if (!is.null(influence_matrix)) {
    ## Single W-matrix (backward compatible)
    stopifnot(is.matrix(influence_matrix) || inherits(influence_matrix, "Matrix"))
    influence_matrix <- as.matrix(influence_matrix)
    coDyadCovars_list[[length(coDyadCovars_list) + 1]] <-
      list(
        effect       = "XWX",
        parameter    = influence_weight,
        dv_name      = .DV_NAME,
        fix          = TRUE,
        nodeSet      = c("COMPONENTS", "COMPONENTS"),
        interaction1 = "self$component_1_coDyadCovar",
        x            = influence_matrix
      )
  }

  ## -- 3b. Time-varying influence (varDyadCovars) ------------------------- ##
  ## An N x N x P array per coupling, P = waves - 1. These occupy their own
  ## `self$component_<k>_varDyadCovar` slots, so a model may carry static and
  ## time-varying couplings at once without either renumbering the other. The
  ## wave count cannot be checked here (the model does not see the dependent
  ## variable), so it is validated in the engine against the actual DV.
  varDyadCovars_list <- list()

  if (!is.null(influence_arrays)) {
    stopifnot(is.list(influence_arrays), !is.null(names(influence_arrays)),
              all(nzchar(names(influence_arrays))))
    if (!is.null(influence_array_weights) &&
        !all(names(influence_array_weights) %in% names(influence_arrays)))
      warning("`influence_array_weights` carries names absent from ",
              "`influence_arrays`; those weights are ignored.", call. = FALSE)
    for (nm in names(influence_arrays)) {
      w_arr <- .saomnk_as_dyad_array(influence_arrays[[nm]], nm)
      w_weight <- if (!is.null(influence_array_weights) &&
                      nm %in% names(influence_array_weights)) {
        influence_array_weights[[nm]]
      } else {
        influence_weight
      }
      slot <- length(varDyadCovars_list) + 1L
      varDyadCovars_list[[slot]] <-
        list(
          effect       = "XWX",
          parameter    = w_weight,
          dv_name      = .DV_NAME,
          fix          = TRUE,
          nodeSet      = c("COMPONENTS", "COMPONENTS"),
          interaction1 = sprintf("self$component_%d_varDyadCovar", slot),
          x            = w_arr
        )
    }
  }

  ## Named list of M x N dyadic covariates
  if (!is.null(dyad_covariates)) {
    stopifnot(is.list(dyad_covariates), !is.null(names(dyad_covariates)))
    for (dc_nm in names(dyad_covariates)) {
      dc_mat <- dyad_covariates[[dc_nm]]
      stopifnot(is.matrix(dc_mat))
      dc_weight <- attr(dc_mat, "weight")
      if (is.null(dc_weight)) dc_weight <- 0.1
      dc_effect <- attr(dc_mat, "effect")
      if (is.null(dc_effect)) dc_effect <- "egoXaltX"
      dc_slot <- length(coDyadCovars_list) + 1L
      coDyadCovars_list[[dc_slot]] <-
        list(
          effect       = dc_effect,
          parameter    = dc_weight,
          dv_name      = .DV_NAME,
          fix          = TRUE,
          nodeSet      = c("ACTORS", "COMPONENTS"),
          interaction1 = sprintf("self$component_%d_coDyadCovar", dc_slot),
          x            = dc_mat
        )
    }
  }

  ## Single actor-by-component dyad covariate (backward compatible)
  if (!is.null(dyad_covariate)) {
    stopifnot(is.matrix(dyad_covariate))
    ## Determine the slot index: start after any component coDyadCovars
    dyad_slot <- length(coDyadCovars_list) + 1L
    coDyadCovars_list[[dyad_slot]] <-
      list(
        effect       = dyad_covariate_effect,
        parameter    = dyad_covariate_weight,
        dv_name      = .DV_NAME,
        fix          = TRUE,
        nodeSet      = c("ACTORS", "COMPONENTS"),
        interaction1 = sprintf("self$component_%d_coDyadCovar", dyad_slot),
        x            = dyad_covariate
      )
  }

  ## -- 4. Assemble the structure model ------------------------------------ ##

  sm <- list(
    dv_bipartite = list(
      name          = .DV_NAME,
      effects       = effects_list,
      coCovars      = coCovars_list,
      varCovars     = list(),
      coDyadCovars  = coDyadCovars_list,
      varDyadCovars = varDyadCovars_list,
      interactions  = list()
    )
  )

  class(sm) <- c("saomnk_model", "list")
  sm
}


# ---------------------------------------------------------------------------- #
#  saomnk_shock
# ---------------------------------------------------------------------------- #

#' Define an Exogenous Shock to a SaoMNK Model Parameter
#'
#' Constructs a shock specification that can be passed to
#' \code{\link{saomnk_run}} via the \code{shocks} argument.  Shocks divide the
#' total simulation into proportional \emph{portions}; each portion has its own
#' parameter values.
#'
#' The simulation timeline is split into consecutive segments whose relative
#' sizes are determined by each shock's \code{portion} value.  To create a
#' two-phase simulation (e.g., baseline then shocked), supply two shock objects:
#' the first for the baseline regime and the second for the post-shock regime.
#'
#' @param effect Character vector. Which effect(s) to set for this segment.
#'   Accepts user-friendly names (\code{"density"}, \code{"popularity"},
#'   \code{"scope"}, \code{"epistasis"}) which are mapped to RSiena shortcodes,
#'   or raw RSiena effect names (e.g., \code{"inPop"}).
#' @param parameter Numeric vector (same length as \code{effect}).
#'   Parameter value(s) during this segment of the simulation.
#' @param portion Integer. Relative size of this segment (default \code{1}).
#'   For example, two shocks with \code{portion = 1} each split the simulation
#'   in half; portions of 2 and 1 give a 2/3--1/3 split.
#' @return A list with class \code{"saomnk_shock"}.
#' @export
#' @examples
#' ## Baseline regime (first half): density = -0.5
#' s1 <- saomnk_shock("density", parameter = -0.5, portion = 1)
#'
#' ## Shocked regime (second half): density drops to -2.0
#' s2 <- saomnk_shock("density", parameter = -2.0, portion = 1)
#'
#' ## Pass both to saomnk_run(shocks = list(s1, s2))
saomnk_shock <- function(effect, parameter, portion = 1L) {

  stopifnot(is.character(effect), length(effect) >= 1)
  stopifnot(is.numeric(parameter), length(parameter) == length(effect))
  stopifnot(is.numeric(portion), length(portion) == 1, portion >= 1)

  ## Map friendly names to RSiena names where applicable
  mapped <- vapply(effect, function(e) {
    if (e %in% names(.EFFECT_MAP)) .EFFECT_MAP[[e]] else e
  }, character(1), USE.NAMES = FALSE)

  shock <- list(
    effect    = mapped,
    parameter = parameter,
    portion   = as.integer(portion)
  )
  class(shock) <- c("saomnk_shock", "list")
  shock
}


# ---------------------------------------------------------------------------- #
#  saomnk_run
# ---------------------------------------------------------------------------- #

#' Run a SaoMNK Search Simulation
#'
#' Executes the SAOM-based search simulation on a prepared environment and
#' structure model.  This is the primary entry point for running simulations
#' after \code{\link{saomnk_env}} and \code{\link{saomnk_model}}.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object created by
#'   \code{\link{saomnk_env}}.
#' @param model A structure model created by \code{\link{saomnk_model}}.
#' @param steps_per_actor Integer. Number of decision-chain micro-steps per
#'   actor (default \code{30}).  Total simulation steps = \code{M *
#'   steps_per_actor}.
#' @param seed Integer or \code{NULL}. Random seed for the RSiena simulation
#'   run.
#' @param shocks A list of \code{\link{saomnk_shock}} objects defining
#'   parameter regime changes, or \code{NULL} (default) for no shocks.
#' @param theta_matrix Optional numeric matrix of per-ministep parameter values
#'   (\code{iterations} rows x one column per simulated effect), as built by
#'   \code{\link{saomnk_theta_ramp}} or \code{\link{saomnk_theta_drift}}. When
#'   supplied it defines the parameter trajectory directly and its row count
#'   overrides \code{steps_per_actor}. Default \code{NULL}, in which case the
#'   engine builds a constant theta matrix from \code{model} exactly as before.
#' @param verbose Logical. If \code{TRUE}, print RSiena diagnostic output
#'   during the simulation (default \code{FALSE}).
#' @return The \code{env} object (modified in place), returned invisibly.
#' @export
#' @examples
#' env <- saomnk_env(M = 3, N = 6, seed = 1234)
#' mod <- saomnk_model(density = -0.5,
#'                     influence_matrix = saomnk_block_diagonal(6, 2))
#' saomnk_run(env, mod, steps_per_actor = 5, seed = 12345)
saomnk_run <- function(env, model, steps_per_actor = 30,
                        seed = NULL, shocks = NULL, theta_matrix = NULL,
                        verbose = FALSE) {

  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))
  stopifnot(is.list(model))

  ## Prepare theta_shocks in the internal list-of-lists format
  theta_shocks <- NULL
  if (!is.null(shocks)) {
    stopifnot(is.list(shocks))
    theta_shocks <- lapply(shocks, function(s) {
      ## Strip class to plain list for internal engine
      as.list(s)
    })
  }

  run_seed <- if (!is.null(seed)) as.integer(seed) else 123L

  if (!is.null(theta_matrix)) {
    if (!is.matrix(theta_matrix) || !is.numeric(theta_matrix))
      stop("`theta_matrix` must be a numeric matrix (see saomnk_theta_ramp()).")
    ## nrow(theta_matrix) is the ministep count, so iterations_per_actor must
    ## not also be passed: search_rsiena() prefers theta_matrix, but passing
    ## both invites a silent mismatch between what the caller asked for and
    ## what ran.
    env$search_rsiena(
      structure_model = model,
      theta_matrix    = theta_matrix,
      run_seed        = run_seed,
      theta_shocks    = theta_shocks,
      verbose         = verbose
    )
    return(invisible(env))
  }

  env$search_rsiena(
    structure_model      = model,
    iterations_per_actor = as.integer(steps_per_actor),
    run_seed             = run_seed,
    theta_shocks         = theta_shocks,
    verbose              = verbose
  )

  invisible(env)
}


# ---------------------------------------------------------------------------- #
#  saomnk_monte_carlo
# ---------------------------------------------------------------------------- #

#' Run Parallel Monte Carlo Replications
#'
#' Executes independent replications of a SaoMNK multiwave simulation, each
#' starting from the same initial bipartite matrix but with a different random
#' seed.  When \code{parallel = TRUE} and the \pkg{future} / \pkg{future.apply}
#' packages are available, replications run across multiple R worker processes.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object created by
#'   \code{\link{saomnk_env}}.
#' @param model A structure model created by \code{\link{saomnk_model}}.
#' @param replications Integer. Number of independent replications
#'   (default \code{10}).
#' @param waves Integer. Number of waves per replication (default \code{2}).
#' @param iterations Integer. RSiena iterations per wave (default \code{500}).
#' @param seed Integer. Base random seed; replication \emph{r} uses
#'   \code{seed + r}.
#' @param parallel Logical. If \code{TRUE}, run replications in parallel
#'   using the \pkg{future} back-end (default \code{FALSE}).
#' @param workers Integer or \code{NULL}. Number of parallel workers.
#'   If \code{NULL}, uses the current \pkg{future} plan.
#' @return The \code{env} object (modified in place) with
#'   \code{env$mc_results} populated; returned invisibly.
#' @export
#' @examples
#' \dontrun{
#' env <- saomnk_env(M = 6, N = 12, seed = 42)
#' mod <- saomnk_model(density = -0.5,
#'                     influence_matrix = saomnk_block_diagonal(12, 4))
#' saomnk_monte_carlo(env, mod, replications = 20, parallel = TRUE, workers = 4)
#' }
saomnk_monte_carlo <- function(env, model, replications = 10, waves = 2,
                                iterations = 500, seed = 12345,
                                parallel = FALSE, workers = NULL) {

  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))
  stopifnot(is.list(model))

  env$search_rsiena_monte_carlo(
    structure_model = model,
    replications    = as.integer(replications),
    waves           = as.integer(waves),
    iterations      = as.integer(iterations),
    rand_seed       = as.integer(seed),
    parallel        = parallel,
    workers         = workers
  )

  invisible(env)
}


# ---------------------------------------------------------------------------- #
#  saomnk_plot_k4
# ---------------------------------------------------------------------------- #

#' Plot the K-4 Coupled Degree Panel
#'
#' Visualises the four coupled degree processes---actor scope (\eqn{K_{AC}}),
#' component popularity (\eqn{K_{CA}}), actor sociality (\eqn{K_{AA}}), and
#' component epistasis (\eqn{K_{CC}})---over the simulated decision chain.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after running
#'   \code{\link{saomnk_run}}.
#' @param smooth Numeric. Loess smoothing span passed to
#'   \code{plot_degree_4panel} (default \code{0.3}).
#' @return The plot object (invisibly), or side-effect display.
#' @export
#' @examples
#' \dontrun{
#' saomnk_plot_k4(env, smooth = 0.25)
#' }
saomnk_plot_k4 <- function(env, smooth = 0.3) {
  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))
  env$plot_degree_4panel(loess_span = smooth)
}


# ---------------------------------------------------------------------------- #
#  saomnk_plot_snapshots
# ---------------------------------------------------------------------------- #

#' Plot Network Snapshots
#'
#' Renders bipartite network snapshots at selected simulation steps, together
#' with their unipartite projections (social and epistasis networks).
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after running
#'   \code{\link{saomnk_run}}.
#' @param steps Integer vector of simulation step indices to snapshot, or
#'   \code{NULL} (default) for five evenly-spaced steps plus the initial state.
#' @return Called for side effects (plot display).
#' @export
#' @examples
#' \dontrun{
#' saomnk_plot_snapshots(env)
#' saomnk_plot_snapshots(env, steps = c(1, 10, 50))
#' }
saomnk_plot_snapshots <- function(env, steps = NULL) {
  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))

  total_steps <- env$get_step()
  if (total_steps == 0)
    stop("No simulation steps recorded. Run saomnk_run() first.")

  if (is.null(steps)) {
    ## Pick ~5 evenly spaced steps
    n_snaps <- min(5L, total_steps)
    steps <- unique(round(seq(1, total_steps, length.out = n_snaps)))
  }

  env$plot_snapshots(steps)
}


# ---------------------------------------------------------------------------- #
#  saomnk_plot_utility
# ---------------------------------------------------------------------------- #

#' Plot Utility Contributions
#'
#' Decomposes each actor's objective-function value into contributions from
#' individual network statistics over the simulated decision chain.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after running
#'   \code{\link{saomnk_run}}.
#' @param smooth Numeric. Loess smoothing span (default \code{0.35}).
#' @param weighted Logical. If \code{TRUE} (default), multiply raw statistics
#'   by their \eqn{\theta} weights.
#' @return Called for side effects (plot display).
#' @export
saomnk_plot_utility <- function(env, smooth = 0.35, weighted = TRUE) {
  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))
  env$plot_utility_contributions(loess_span = smooth, use_thetas = weighted)
}


# ---------------------------------------------------------------------------- #
#  saomnk_summary
# ---------------------------------------------------------------------------- #

#' Print Model Summary
#'
#' Displays a regression-style summary table of the SAOM model specification
#' and its parameter values.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after running
#'   \code{\link{saomnk_run}}.
#' @return The formatted summary table (invisibly).
#' @export
#' @examples
#' \dontrun{
#' saomnk_summary(env)
#' }
saomnk_summary <- function(env) {
  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))
  env$search_rsiena_model_summary()
}


# ---------------------------------------------------------------------------- #
#  saomnk_get_degrees
# ---------------------------------------------------------------------------- #

#' Extract Coupled Degree Data Frames
#'
#' Returns a named list of the four coupled degree data frames:
#' \eqn{K_{AC}} (actor scope), \eqn{K_{CA}} (component popularity),
#' \eqn{K_{AA}} (actor sociality), and \eqn{K_{CC}} (component epistasis).
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after running
#'   \code{\link{saomnk_run}}.
#' @return A named list with elements \code{K_AC}, \code{K_CA}, \code{K_AA},
#'   \code{K_CC}.
#' @export
saomnk_get_degrees <- function(env) {
  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))
  list(
    K_AC = env$K_AC_df,
    K_CA = env$K_CA_df,
    K_AA = env$K_AA_df,
    K_CC = env$K_CC_df
  )
}


# ---------------------------------------------------------------------------- #
#  saomnk_get_bipartite
# ---------------------------------------------------------------------------- #

#' Extract the Bipartite Network Matrix
#'
#' Returns the current \eqn{M \times N}{M x N} bipartite adjacency matrix
#' from the environment (after simulation).
#'
#' @param env A \code{SaomNkRSienaBiEnv} object.
#' @param step Integer or \code{NULL}. If \code{NULL} (default), returns the
#'   current (final) matrix.  If an integer, returns the matrix at that
#'   simulation step from the stored chain.
#' @return An \eqn{M \times N}{M x N} numeric matrix.
#' @export
saomnk_get_bipartite <- function(env, step = NULL) {
  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))
  if (is.null(step)) {
    return(env$bipartite_matrix)
  }
  stopifnot(is.numeric(step), length(step) == 1)
  total <- env$get_step()
  if (step < 1 || step > total)
    stop(sprintf("step must be between 1 and %d", total))
  env$bi_env_arr[, , step]
}


# ---------------------------------------------------------------------------- #
#  print method for saomnk_model
# ---------------------------------------------------------------------------- #

#' @export
print.saomnk_model <- function(x, ...) {

  effs <- x$dv_bipartite$effects
  cat("SaoMNK Structure Model\n")
  cat("----------------------\n")
  cat("Effects:\n")
  for (e in effs) {
    label <- names(which(.EFFECT_MAP == e$effect))
    if (length(label) == 0) label <- e$effect
    cat(sprintf("  %-15s  theta = %s\n", label, format(e$parameter)))
  }

  covs <- x$dv_bipartite$coCovars
  if (length(covs)) {
    cat("Actor covariates:\n")
    for (cv in covs) {
      cat(sprintf("  %-15s  theta = %s\n", cv$effect, format(cv$parameter)))
    }
  }

  dycovs <- x$dv_bipartite$coDyadCovars
  if (length(dycovs)) {
    cat("Dyad covariates:\n")
    for (dc in dycovs) {
      label <- names(which(.EFFECT_MAP == dc$effect))
      if (length(label) == 0) label <- dc$effect
      mat_dim <- if (!is.null(dc$x)) paste(dim(dc$x), collapse = "x") else "?"
      cat(sprintf("  %-15s  theta = %s  [%s matrix]\n",
                  label, format(dc$parameter), mat_dim))
    }
  }

  invisible(x)
}
