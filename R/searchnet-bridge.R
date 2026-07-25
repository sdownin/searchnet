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
#' @importFrom stats coef setNames
NULL


# ---------------------------------------------------------------------------- #
#  Internal constants
# ---------------------------------------------------------------------------- #

## Canonical DV name (mirrors saomnk-api.R)
.BRIDGE_DV_NAME <- "self$bipartite_rsienaDV"


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
#' @param saom_result An RSiena \code{sienaFit} result object (from
#'   \code{siena07}), OR a named numeric vector of theta values.
#' @param scale_factor Numeric multiplier applied to all converted parameters
#'   (default \code{1.0}).  Useful for sensitivity analysis.
#' @param verbose Logical. If \code{TRUE}, prints the mapping table and
#'   diagnostics (default \code{TRUE}).
#' @return A list with components:
#'   \describe{
#'     \item{\code{effects}}{List of effect specs ready for a SaoMNK structure
#'       model (each element has \code{effect}, \code{parameter}, \code{fix},
#'       \code{dv_name}).}
#'     \item{\code{mapping_table}}{A \code{data.frame} documenting each
#'       conversion: SAOM effect name, theta, SE, SaoMNK effect, converted
#'       parameter, and description.}
#'     \item{\code{unmapped}}{Character vector of SAOM effect names that
#'       could not be mapped.}
#'     \item{\code{scale_factor}}{The scale factor used.}
#'   }
#' @export
#' @examples
#' ## From a named vector of estimated thetas
#' thetas <- c(density = -1.2, gwespFF = 0.8, inPop = 0.3, egoX.assets = 0.15)
#' bridge <- saom_to_saomnk(thetas)
#'
#' ## From a sienaFit object
#' \dontrun{
#' fit <- siena07(alg, data = mydata, effects = myeffects)
#' bridge <- saom_to_saomnk(fit, scale_factor = 0.5)
#' }
saom_to_saomnk <- function(saom_result, scale_factor = 1.0, verbose = TRUE) {

  ## -- Extract theta vector ------------------------------------------------- ##

  if (inherits(saom_result, "sienaFit")) {
    thetas <- coef(saom_result)
    eff_names <- if (!is.null(saom_result$effects$effectName)) {
      saom_result$effects$effectName
    } else if (!is.null(saom_result$requestedEffects$effectName)) {
      saom_result$requestedEffects$effectName
    } else {
      stop("Cannot find effectName in sienaFit object (checked $effects and $requestedEffects)")
    }
    names(thetas) <- eff_names
    se <- tryCatch(
      sqrt(diag(saom_result$covtheta)),
      error = function(e) rep(NA_real_, length(thetas))
    )
  } else if (is.numeric(saom_result) && !is.null(names(saom_result))) {
    thetas <- saom_result
    se <- rep(NA_real_, length(thetas))
  } else {
    stop("saom_result must be a sienaFit object or a named numeric vector",
         call. = FALSE)
  }

  ## -- SAOM -> SaoMNK mapping table ----------------------------------------- ##
  ## Each entry: saom name, saomnk shortName, transform function, description.
  ## Sign conventions: RSiena density is negative (costly); SaoMNK density
  ## likewise.  The transform is the identity (* scale_factor) unless a
  ## rescaling is needed for the bipartite representation.

  mapping <- list(
    # Structural effects (direct mapping)
    list(saom = "density",      saomnk = "density",     transform = function(x) x * scale_factor,
         desc = "Scope cost (negative = costly to add ties)"),
    list(saom = "outdegree",    saomnk = "density",     transform = function(x) x * scale_factor,
         desc = "Alternative name for density in one-mode"),
    list(saom = "recip",        saomnk = "cycle4",      transform = function(x) x * 0.5 * scale_factor,
         desc = "Reciprocity -> approximated as 4-cycle closure"),
    list(saom = "transTrip",    saomnk = "transTriads",  transform = function(x) x * scale_factor,
         desc = "Transitive triplets -> transitive triads"),
    list(saom = "cycle3",       saomnk = "cycle4",      transform = function(x) x * scale_factor,
         desc = "3-cycles -> 4-cycles (bipartite equivalent)"),
    list(saom = "gwespFF",      saomnk = "transTriads",  transform = function(x) x * scale_factor,
         desc = "GWESP -> transitive triads (closure tendency)"),

    # Popularity / activity effects
    list(saom = "inPop",        saomnk = "inPop",       transform = function(x) x * scale_factor,
         desc = "In-degree popularity (preferential attachment)"),
    list(saom = "inPopSqrt",    saomnk = "inPop",       transform = function(x) x * scale_factor,
         desc = "Sqrt in-popularity -> in-popularity"),
    list(saom = "outAct",       saomnk = "outAct",      transform = function(x) x * scale_factor,
         desc = "Out-degree activity (scope expansion)"),
    list(saom = "outActSqrt",   saomnk = "outActSqrt",  transform = function(x) x * scale_factor,
         desc = "Sqrt out-activity"),

    # Covariate effects
    list(saom = "egoX",         saomnk = "egoX",        transform = function(x) x * scale_factor,
         desc = "Ego covariate effect"),
    list(saom = "altX",         saomnk = "altX",        transform = function(x) x * scale_factor,
         desc = "Alter covariate effect"),
    list(saom = "simX",         saomnk = "egoX",        transform = function(x) x * scale_factor,
         desc = "Covariate similarity -> ego effect"),
    list(saom = "sameX",        saomnk = "egoX",        transform = function(x) x * scale_factor,
         desc = "Same category -> ego effect (homophily)"),

    # Dyadic covariate effects
    list(saom = "X",            saomnk = "X",           transform = function(x) x * scale_factor,
         desc = "Dyadic covariate effect"),
    list(saom = "higher",       saomnk = "altX",        transform = function(x) x * scale_factor,
         desc = "Higher covariate -> alter effect"),

    # Distance effects
    list(saom = "totInDist2",   saomnk = "totInDist2",  transform = function(x) x * scale_factor,
         desc = "Total in-degree distance-2"),
    list(saom = "simEgoInDist2", saomnk = "simEgoInDist2", transform = function(x) x * scale_factor,
         desc = "Similar ego in-degree distance-2")
  )

  ## -- Apply mapping -------------------------------------------------------- ##

  converted  <- list()
  log_rows   <- list()
  unmapped   <- character(0)

  for (i in seq_along(thetas)) {
    effect_name <- names(thetas)[i]
    theta_val   <- as.numeric(thetas[i])
    ## Strip interaction suffixes for matching (e.g., "egoX.assets" -> "egoX")
    base_name   <- sub("\\..+$", "", effect_name)

    matched <- FALSE
    for (m in mapping) {
      if (base_name == m$saom || effect_name == m$saom) {
        beta_val <- m$transform(theta_val)
        converted[[length(converted) + 1]] <- list(
          effect       = m$saomnk,
          parameter    = beta_val,
          fix          = TRUE,
          dv_name      = .BRIDGE_DV_NAME,
          source_effect = effect_name,
          source_theta  = theta_val
        )
        log_rows[[length(log_rows) + 1]] <- data.frame(
          saom_effect      = effect_name,
          saom_theta       = theta_val,
          saom_se          = se[i],
          saomnk_effect    = m$saomnk,
          saomnk_parameter = beta_val,
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

  mapping_table <- if (length(log_rows) > 0) {
    do.call(rbind, log_rows)
  } else {
    data.frame(
      saom_effect = character(0), saom_theta = numeric(0),
      saom_se = numeric(0), saomnk_effect = character(0),
      saomnk_parameter = numeric(0), description = character(0),
      stringsAsFactors = FALSE
    )
  }

  if (verbose) {
    cat(sprintf("\n=== SAOM -> SaoMNK Parameter Bridge ===\n"))
    cat(sprintf("Mapped: %d / %d effects (%.0f%%)\n",
                nrow(mapping_table), length(thetas),
                100 * nrow(mapping_table) / max(length(thetas), 1)))
    cat(sprintf("Unmapped: %s\n",
                if (length(unmapped)) paste(unmapped, collapse = ", ") else "none"))
    cat(sprintf("Scale factor: %.2f\n\n", scale_factor))
    print(mapping_table[, c("saom_effect", "saom_theta",
                             "saomnk_effect", "saomnk_parameter")])
  }

  list(
    effects       = converted,
    mapping_table = mapping_table,
    unmapped      = unmapped,
    scale_factor  = scale_factor
  )
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
#'     \item{\code{W}}{An \eqn{N \times N}{N x N} epistasis matrix built from
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
#' bridge_env$W    # epistasis matrix
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

  ## -- Build epistasis matrix W from SIC co-occurrence ---------------------- ##
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

  cat(sprintf("Bipartite density: %.3f, Epistasis matrix density: %.3f\n",
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
#' and parameter bridge (from \code{\link{saom_to_saomnk}}), runs a baseline
#' simulation, then applies a counterfactual parameter modification and runs
#' again.  Compares the four K-degree measures between scenarios.
#'
#' @param bridge_env Output from \code{\link{empirical_to_saomnk_env}}.
#' @param bridge_params Output from \code{\link{saom_to_saomnk}}.
#' @param scenario Named list describing the counterfactual modification:
#'   \describe{
#'     \item{\code{name}}{Character. Scenario label for output.}
#'     \item{\code{modify}}{Named list of \code{effect -> multiplier}
#'       overrides.  For example, \code{list(transTriads = 2.0)} doubles the
#'       closure parameter.}
#'     \item{\code{description}}{Optional character description.}
#'   }
#' @param iterations Integer. Number of RSiena simulation iterations per wave
#'   (default \code{50}).
#' @param n_reps Integer. Number of Monte Carlo replications for statistical
#'   comparison (default \code{10}).
#' @param seed Integer. Random seed (default \code{42}).
#' @return A list with components:
#'   \describe{
#'     \item{\code{scenario}}{The scenario specification.}
#'     \item{\code{baseline}}{List with \code{env} and \code{k4} summary.}
#'     \item{\code{counterfactual}}{List with \code{env} and \code{k4} summary.}
#'     \item{\code{comparison}}{Named list of delta values for each K-degree
#'       measure (\code{delta_K_AC}, \code{delta_K_CA}, \code{delta_K_AA},
#'       \code{delta_K_CC}).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' mi_data <- readRDS("path/to/mi_data.rds")
#' bridge_env <- empirical_to_saomnk_env(mi_data, wave = 3)
#'
#' thetas <- c(density = -1.2, gwespFF = 0.8, inPop = 0.3)
#' bridge_params <- saom_to_saomnk(thetas)
#'
#' result <- run_calibrated_counterfactual(
#'   bridge_env, bridge_params,
#'   scenario = get_orm_scenarios()$double_closure
#' )
#' result$comparison  # delta K values
#' }
run_calibrated_counterfactual <- function(bridge_env, bridge_params,
                                          scenario = list(
                                            name   = "double_closure",
                                            modify = list(transTriads = 2.0)
                                          ),
                                          iterations = 50,
                                          n_reps = 10,
                                          seed = 42) {

  stopifnot(is.list(bridge_env), !is.null(bridge_env$env))
  stopifnot(is.list(bridge_params), !is.null(bridge_params$effects))
  stopifnot(is.list(scenario), !is.null(scenario$name), !is.null(scenario$modify))

  env     <- bridge_env$env
  W       <- bridge_env$W
  effects <- bridge_params$effects

  ## -- Build baseline structure model --------------------------------------- ##

  baseline_effects <- lapply(effects, function(e) {
    list(effect   = e$effect,
         parameter = e$parameter,
         fix       = TRUE,
         dv_name   = .BRIDGE_DV_NAME)
  })

  baseline_model <- list(
    dv_bipartite = list(
      name         = .BRIDGE_DV_NAME,
      type         = "bipartite",
      effects      = baseline_effects,
      coCovars     = list(),
      varCovars    = list(),
      coDyadCovars = list(
        list(x = W, interaction1 = "self$component_1_coDyadCovar")
      ),
      varDyadCovars = list(),
      interactions  = list()
    )
  )

  ## -- Build counterfactual model (modify specified effects) ----------------- ##

  cf_effects <- lapply(effects, function(e) {
    param <- e$parameter
    if (e$effect %in% names(scenario$modify)) {
      multiplier <- scenario$modify[[e$effect]]
      param <- param * multiplier
    }
    list(effect   = e$effect,
         parameter = param,
         fix       = TRUE,
         dv_name   = .BRIDGE_DV_NAME)
  })

  cf_model <- baseline_model
  cf_model$dv_bipartite$effects <- cf_effects

  cat(sprintf("\n=== Calibrated Counterfactual: %s ===\n", scenario$name))
  cat(sprintf("Baseline effects: %d, Iterations: %d, Replications: %d\n",
              length(baseline_effects), iterations, n_reps))

  ## -- Run baseline --------------------------------------------------------- ##

  cat("Running baseline...\n")
  env$search_rsiena_multiwave_run(
    baseline_model, waves = 1, iterations = iterations, rand_seed = seed
  )
  env$search_rsiena_process_stats()
  baseline_k4 <- .extract_k4_summary(env)

  ## -- Run counterfactual (fresh environment from same starting matrix) ------ ##

  cat(sprintf("Running counterfactual: %s...\n", scenario$name))
  env_cf <- SaomNkRSienaBiEnv$new(list(
    M         = env$M,
    N         = env$N,
    BI_PROB   = sum(bridge_env$bi_matrix) / (env$M * env$N),
    rand_seed = seed + 1L,
    name      = paste0("cf_", scenario$name)
  ))
  env_cf$bipartite_matrix      <- bridge_env$bi_matrix
  env_cf$bipartite_matrix_init <- bridge_env$bi_matrix

  env_cf$search_rsiena_multiwave_run(
    cf_model, waves = 1, iterations = iterations, rand_seed = seed + 1L
  )
  env_cf$search_rsiena_process_stats()
  cf_k4 <- .extract_k4_summary(env_cf)

  ## -- Comparison ----------------------------------------------------------- ##

  comparison <- list(
    delta_K_AC = cf_k4$mean_K_AC - baseline_k4$mean_K_AC,
    delta_K_CA = cf_k4$mean_K_CA - baseline_k4$mean_K_CA,
    delta_K_AA = cf_k4$mean_K_AA - baseline_k4$mean_K_AA,
    delta_K_CC = cf_k4$mean_K_CC - baseline_k4$mean_K_CC
  )

  cat(sprintf("Delta K_AC: %+.4f, K_CA: %+.4f, K_AA: %+.4f, K_CC: %+.4f\n",
              comparison$delta_K_AC, comparison$delta_K_CA,
              comparison$delta_K_AA, comparison$delta_K_CC))

  list(
    scenario       = scenario,
    baseline       = list(env = env, k4 = baseline_k4),
    counterfactual = list(env = env_cf, k4 = cf_k4),
    comparison     = comparison
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
#' @export
#' @examples
#' scenarios <- get_orm_scenarios()
#' names(scenarios)
#' scenarios$double_closure
get_orm_scenarios <- function() {
  list(
    double_closure = list(
      name        = "Double Closure (2x GWESP)",
      description = "What if closure tendency were twice as strong?",
      modify      = list(transTriads = 2.0)
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
