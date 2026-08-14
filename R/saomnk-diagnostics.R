# =============================================================================
# saomnk-diagnostics.R
# Network model diagnostics for searchnet
#
# EXPERIMENTAL / INTERNAL (not exported).
#
# Specification-robustness diagnostics: SAI, CFC, DGF. These are retained
# as work in progress and are deliberately NOT exported: CFC lacks a
# formal definition and DGF's risk thresholds are heuristic. They are not
# described in the software paper. Do not export without first defining
# CFC and justifying the DGF cut-points.
# Functions provide specification curve analysis (SAI), cross-framework
# concordance (CFC), density-GOF frontier diagnostics (DGF), SAOM/TERGM
# estimate extraction, and effect crosswalk mapping.
#
# All exported functions are prefixed with saomnk_ to avoid namespace
# collisions. Internal helpers remain unexported.
# =============================================================================


# ---------------------------------------------------------------------------
# SAI: Specification Agreement Index
# ---------------------------------------------------------------------------

#' Specification Agreement Index (SAI)
#'
#' Computes sign-agreement, significance-agreement, and composite SAI scores
#' across a grid of model specifications for each effect. Provides specification
#' curve data for visualization.
#'
#' @param estimates A data.frame with columns: \code{effect}, \code{specification},
#'   \code{estimate}, \code{std_error}. Each row is one effect from one
#'   specification in the multiverse grid.
#' @param reference Character or NULL. Name of the specification to use as the
#'   reference (denominator) for concordance. If NULL, the median specification
#'   is used (specification whose estimate is closest to the median for each
#'   effect).
#' @param weights Numeric vector or NULL. Optional GOF-based weights for each
#'   specification. If provided, must be a named vector where names match
#'   specification identifiers, or a numeric vector of the same length as
#'   unique specifications. Values are normalized to sum to 1.
#' @param alpha Numeric. Significance threshold (default 0.05).
#' @param effects Character vector or NULL. Subset of effects to analyze. If
#'   NULL, all effects in the data are used.
#'
#' @return An S3 object of class \code{"saomnk_sai"} with components:
#'   \describe{
#'     \item{table}{A data.frame with per-effect SAI scores: effect,
#'       sai_sign, sai_sig, sai_composite, n_specs, n_positive, n_negative,
#'       n_significant, median_estimate, median_se, reference_spec.}
#'     \item{curve}{A data.frame of specification curve data: effect,
#'       specification, estimate, std_error, ci_lo, ci_hi, sign, significant,
#'       rank, weight. Sorted by estimate within each effect for curve plots.}
#'     \item{reference}{Character. The reference specification used.}
#'     \item{alpha}{Numeric. The significance threshold.}
#'     \item{call}{The matched call.}
#'   }
#'
#' @details
#' The Specification Agreement Index quantifies how robust a network model
#' estimate is across a multiverse of defensible specifications. For each
#' effect \eqn{k}:
#'
#' \deqn{SAI_{sign}(k) = \frac{|\sum_s w_s \cdot \text{sgn}(\hat{\theta}_{ks})|}
#'   {\sum_s w_s}}
#'
#' \deqn{SAI_{sig}(k) = \frac{\sum_s w_s \cdot I(p_{ks} < \alpha)}
#'   {\sum_s w_s}}
#'
#' \deqn{SAI_{composite}(k) = SAI_{sign}(k) \times SAI_{sig}(k)}
#'
#' where \eqn{s} indexes specifications, \eqn{w_s} are (optionally GOF-based)
#' weights, and \eqn{\text{sgn}} returns the sign of the estimate.
#'
#' SAI_sign ranges from 0 (half positive, half negative) to 1 (unanimous sign).
#' SAI_sig ranges from 0 (never significant) to 1 (always significant).
#' SAI_composite ranges from 0 to 1, with 1 indicating a fully robust finding.
#'
#' @examples
#' # Simulated multiverse grid
#' grid <- data.frame(
#'   effect = rep(c("reciprocity", "transitivity", "homophily"), each = 5),
#'   specification = rep(paste0("spec_", 1:5), 3),
#'   estimate = c(
#'     0.8, 0.9, 0.7, 0.85, 0.75,    # reciprocity: all positive
#'     0.3, -0.1, 0.2, 0.4, -0.05,   # transitivity: mixed sign
#'     0.5, 0.6, 0.55, 0.45, 0.52    # homophily: all positive
#'   ),
#'   std_error = rep(0.15, 15)
#' )
#'
#' result <- saomnk_sai(grid)
#' print(result)
#' plot(result, type = "curve")
#'
#' @importFrom stats setNames pnorm qnorm median
#' @export
saomnk_sai <- function(estimates, reference = NULL, weights = NULL, alpha = 0.05,
                        effects = NULL) {

  cl <- match.call()

  ## ---- Input validation ----
  if (!is.data.frame(estimates)) {
    stop("`estimates` must be a data.frame.", call. = FALSE)
  }

  required_cols <- c("effect", "specification", "estimate", "std_error")
  missing_cols <- setdiff(required_cols, names(estimates))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  # Coerce types
  estimates$effect        <- as.character(estimates$effect)
  estimates$specification <- as.character(estimates$specification)
  estimates$estimate      <- as.numeric(estimates$estimate)
  estimates$std_error     <- as.numeric(estimates$std_error)

  # Drop rows with NA estimates
  n_before <- nrow(estimates)
  estimates <- estimates[!is.na(estimates$estimate) & !is.na(estimates$std_error), ]
  if (nrow(estimates) == 0) {
    stop("No valid (non-NA) rows in `estimates`.", call. = FALSE)
  }
  if (nrow(estimates) < n_before) {
    message("Dropped ", n_before - nrow(estimates), " rows with NA values.")
  }

  # Subset effects if requested
  if (!is.null(effects)) {
    estimates <- estimates[estimates$effect %in% effects, ]
    if (nrow(estimates) == 0) {
      stop("No rows match the specified `effects`.", call. = FALSE)
    }
  }

  all_effects <- unique(estimates$effect)
  all_specs   <- unique(estimates$specification)
  n_specs     <- length(all_specs)

  ## ---- Weights ----
  if (is.null(weights)) {
    # Equal weights
    w <- stats::setNames(rep(1 / n_specs, n_specs), all_specs)
  } else {
    if (!is.null(names(weights))) {
      # Named vector: match by specification name
      missing_w <- setdiff(all_specs, names(weights))
      if (length(missing_w) > 0) {
        stop("Weights missing for specifications: ",
             paste(missing_w, collapse = ", "), call. = FALSE)
      }
      w <- weights[all_specs]
    } else {
      if (length(weights) != n_specs) {
        stop("`weights` length (", length(weights), ") does not match ",
             "number of specifications (", n_specs, ").", call. = FALSE)
      }
      w <- stats::setNames(weights, all_specs)
    }
    # Normalize to sum to 1
    w <- w / sum(w)
  }

  ## ---- Compute z-scores, p-values, signs ----
  estimates$z_value     <- estimates$estimate / estimates$std_error
  estimates$p_value     <- 2 * stats::pnorm(-abs(estimates$z_value))
  estimates$sign        <- sign(estimates$estimate)
  estimates$significant <- as.integer(estimates$p_value < alpha)

  ## ---- Confidence intervals ----
  z_crit <- stats::qnorm(1 - alpha / 2)
  estimates$ci_lo <- estimates$estimate - z_crit * estimates$std_error
  estimates$ci_hi <- estimates$estimate + z_crit * estimates$std_error

  ## ---- Assign weights to each row ----
  estimates$weight <- w[estimates$specification]

  ## ---- Determine reference specification per effect ----
  ref_specs <- character(length(all_effects))
  names(ref_specs) <- all_effects

  for (eff in all_effects) {
    rows <- estimates[estimates$effect == eff, ]
    if (!is.null(reference)) {
      if (!(reference %in% rows$specification)) {
        warning("Reference '", reference, "' not found for effect '", eff,
                "'; using median specification.", call. = FALSE)
        med_est <- stats::median(rows$estimate)
        ref_specs[eff] <- rows$specification[which.min(abs(rows$estimate - med_est))]
      } else {
        ref_specs[eff] <- reference
      }
    } else {
      # Median specification: closest to median estimate
      med_est <- stats::median(rows$estimate)
      ref_specs[eff] <- rows$specification[which.min(abs(rows$estimate - med_est))]
    }
  }

  ## ---- Compute SAI per effect ----
  sai_table <- data.frame(
    effect          = character(0),
    sai_sign        = numeric(0),
    sai_sig         = numeric(0),
    sai_composite   = numeric(0),
    n_specs         = integer(0),
    n_positive      = integer(0),
    n_negative      = integer(0),
    n_zero          = integer(0),
    n_significant   = integer(0),
    median_estimate = numeric(0),
    median_se       = numeric(0),
    reference_spec  = character(0),
    stringsAsFactors = FALSE
  )

  for (eff in all_effects) {
    rows <- estimates[estimates$effect == eff, ]
    wt   <- rows$weight / sum(rows$weight)  # re-normalize within effect

    # SAI_sign: absolute value of weighted sign sum
    weighted_sign_sum <- sum(wt * rows$sign)
    sai_sign_val <- abs(weighted_sign_sum)

    # SAI_sig: weighted proportion significant
    sai_sig_val <- sum(wt * rows$significant)

    # SAI_composite
    sai_comp_val <- sai_sign_val * sai_sig_val

    row_out <- data.frame(
      effect          = eff,
      sai_sign        = round(sai_sign_val, 4),
      sai_sig         = round(sai_sig_val, 4),
      sai_composite   = round(sai_comp_val, 4),
      n_specs         = nrow(rows),
      n_positive      = sum(rows$sign > 0),
      n_negative      = sum(rows$sign < 0),
      n_zero          = sum(rows$sign == 0),
      n_significant   = sum(rows$significant),
      median_estimate = round(stats::median(rows$estimate), 4),
      median_se       = round(stats::median(rows$std_error), 4),
      reference_spec  = ref_specs[eff],
      stringsAsFactors = FALSE
    )
    sai_table <- rbind(sai_table, row_out)
  }

  ## ---- Build specification curve data ----
  curve_data <- do.call(rbind, lapply(all_effects, function(eff) {
    rows <- estimates[estimates$effect == eff, ]
    rows <- rows[order(rows$estimate), ]
    rows$rank <- seq_len(nrow(rows))
    rows[, c("effect", "specification", "estimate", "std_error",
             "ci_lo", "ci_hi", "sign", "significant", "rank", "weight",
             "z_value", "p_value")]
  }))
  rownames(curve_data) <- NULL

  ## ---- Assemble output ----
  out <- list(
    table     = sai_table,
    curve     = curve_data,
    reference = if (!is.null(reference)) reference else "(median)",
    alpha     = alpha,
    call      = cl
  )
  class(out) <- "saomnk_sai"
  return(out)
}


#' Print method for saomnk_sai objects
#'
#' @param x An object of class \code{"saomnk_sai"}.
#' @param ... Additional arguments (ignored).
#'
#' @keywords internal
#' @noRd
print.saomnk_sai <- function(x, ...) {
  cat("Specification Agreement Index (SAI)\n")
  cat("====================================\n")
  cat("Reference specification:", x$reference, "\n")
  cat("Significance threshold: alpha =", x$alpha, "\n")
  cat("Effects evaluated:", nrow(x$table), "\n\n")

  # Format table for display
  tbl <- x$table
  tbl$sai_sign      <- sprintf("%.3f", tbl$sai_sign)
  tbl$sai_sig       <- sprintf("%.3f", tbl$sai_sig)
  tbl$sai_composite <- sprintf("%.3f", tbl$sai_composite)
  tbl$median_estimate <- sprintf("%.4f", as.numeric(tbl$median_estimate))

  # Summary interpretation
  cat("Per-effect SAI scores:\n")
  print(tbl[, c("effect", "sai_sign", "sai_sig", "sai_composite",
                 "n_specs", "n_significant")],
        row.names = FALSE)

  # Flag fragile effects
  fragile <- x$table[x$table$sai_composite < 0.5, ]
  if (nrow(fragile) > 0) {
    cat("\n! Fragile effects (SAI_composite < 0.50):\n")
    cat("  ", paste(fragile$effect, collapse = ", "), "\n")
  }

  robust <- x$table[x$table$sai_composite >= 0.9, ]
  if (nrow(robust) > 0) {
    cat("\nRobust effects (SAI_composite >= 0.90):\n")
    cat("  ", paste(robust$effect, collapse = ", "), "\n")
  }

  invisible(x)
}


#' Plot method for saomnk_sai objects
#'
#' Produces one of three visualization types for specification curve analysis.
#'
#' @param x An object of class \code{"saomnk_sai"}.
#' @param type Character. One of \code{"curve"} (specification curve with CI),
#'   \code{"tile"} (sign/significance heatmap across specs), or
#'   \code{"forest"} (forest plot of median estimates with SAI scores).
#' @param effects Character vector or NULL. Subset of effects to plot.
#' @param ... Additional arguments passed to ggplot2.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_segment geom_point
#'   scale_color_manual facet_wrap labs theme_minimal theme element_text
#'   geom_tile geom_text scale_fill_gradient2 geom_vline geom_errorbarh
#' @exportS3Method
plot.saomnk_sai <- function(x, type = c("curve", "tile", "forest"),
                             effects = NULL, ...) {

  type <- match.arg(type)
  curve <- x$curve
  tbl   <- x$table

  if (!is.null(effects)) {
    curve <- curve[curve$effect %in% effects, ]
    tbl   <- tbl[tbl$effect %in% effects, ]
  }

  if (type == "curve") {
    # ---- Specification curve ----
    curve$sig_label <- ifelse(curve$significant == 1, "Significant", "Not significant")

    p <- ggplot2::ggplot(curve,
           ggplot2::aes(x = rank, y = estimate, color = sig_label)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_segment(
        ggplot2::aes(x = rank, xend = rank, y = ci_lo, yend = ci_hi),
        alpha = 0.3, linewidth = 0.5
      ) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::scale_color_manual(
        values = c("Significant" = "#2166AC", "Not significant" = "#B2182B"),
        name = NULL
      ) +
      ggplot2::facet_wrap(~ effect, scales = "free") +
      ggplot2::labs(
        title = "Specification Curve",
        subtitle = paste0("SAI | alpha = ", x$alpha),
        x = "Specification rank (by estimate)",
        y = "Estimate"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(face = "bold"),
        legend.position = "bottom"
      )

    return(p)

  } else if (type == "tile") {
    # ---- Sign/significance tile heatmap ----
    curve$tile_val <- ifelse(
      curve$significant == 1,
      ifelse(curve$sign > 0, 1, -1),
      0
    )
    curve$tile_label <- ifelse(
      curve$significant == 1,
      ifelse(curve$sign > 0, "+sig", "-sig"),
      "n.s."
    )

    p <- ggplot2::ggplot(curve,
           ggplot2::aes(x = specification, y = effect, fill = tile_val)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = tile_label), size = 2.5) +
      ggplot2::scale_fill_gradient2(
        low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
        midpoint = 0, name = "Direction",
        breaks = c(-1, 0, 1),
        labels = c("Neg. sig.", "Not sig.", "Pos. sig.")
      ) +
      ggplot2::labs(
        title = "Sign-Significance Agreement Across Specifications",
        x = "Specification",
        y = "Effect"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
        panel.grid = ggplot2::element_blank()
      )

    return(p)

  } else if (type == "forest") {
    # ---- Forest plot with SAI annotations ----
    tbl$label <- paste0(
      tbl$effect, "  [SAI=",
      sprintf("%.2f", tbl$sai_composite), "]"
    )
    tbl$label <- factor(tbl$label, levels = rev(tbl$label))

    z_crit <- stats::qnorm(1 - x$alpha / 2)
    tbl$ci_lo <- tbl$median_estimate - z_crit * tbl$median_se
    tbl$ci_hi <- tbl$median_estimate + z_crit * tbl$median_se

    tbl$robust <- ifelse(tbl$sai_composite >= 0.5, "Robust", "Fragile")

    p <- ggplot2::ggplot(tbl,
           ggplot2::aes(y = label, x = median_estimate, color = robust)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
        height = 0.2
      ) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_color_manual(
        values = c("Robust" = "#2166AC", "Fragile" = "#B2182B"),
        name = NULL
      ) +
      ggplot2::labs(
        title = "Forest Plot with SAI Scores",
        x = "Median Estimate",
        y = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")

    return(p)
  }
}


# ---------------------------------------------------------------------------
# CFC: Cross-Framework Concordance
# ---------------------------------------------------------------------------

#' Cross-Framework Concordance (CFC)
#'
#' Compares estimates from SAOM and TERGM for theoretically equivalent effects
#' using a crosswalk mapping. Quantifies the degree to which substantive
#' conclusions agree across frameworks.
#'
#' @param saom_estimates A data.frame with columns: effect, estimate, std_error.
#'   Typically from \code{saomnk_extract_estimates_saom()}.
#' @param tergm_estimates A data.frame with columns: effect, estimate, std_error.
#'   Typically from \code{saomnk_extract_estimates_tergm()}.
#' @param crosswalk A data.frame mapping SAOM effects to TERGM effects. See
#'   \code{\link{saomnk_default_crosswalk}}.
#' @param alpha Numeric. Significance threshold (default 0.05).
#' @param ... Additional arguments (reserved).
#'
#' @return An S3 object of class \code{"saomnk_cfc"} with components:
#'   \describe{
#'     \item{concordance}{Data.frame of per-effect concordance scores.}
#'     \item{overall}{Named numeric: overall CFC score.}
#'     \item{crosswalk}{The crosswalk used.}
#'   }
#'
#' @keywords internal
#' @noRd
saomnk_cfc <- function(saom_estimates, tergm_estimates,
                        crosswalk = saomnk_default_crosswalk(),
                        alpha = 0.05, ...) {


  # -- STUB --
  # Full implementation in next release.
  # Will compute:
  #   CFC_sign:  sign agreement across matched effect pairs
  #   CFC_sig:   significance agreement
  #   CFC_magnitude: ratio of point estimates (bounded)
  #   CFC_composite: weighted product

  stop("saomnk_cfc() is not yet implemented. Coming in a future release.",
       call. = FALSE)
}


#' Print method for saomnk_cfc objects
#'
#' @param x An object of class \code{"saomnk_cfc"}.
#' @param ... Additional arguments (ignored).
#'
#' @keywords internal
#' @noRd
print.saomnk_cfc <- function(x, ...) {
  cat("Cross-Framework Concordance (CFC)\n")
  cat("===================================\n")
  cat("Not yet implemented.\n")
  invisible(x)
}


#' Plot method for saomnk_cfc objects
#'
#' @param x An object of class \code{"saomnk_cfc"}.
#' @param ... Additional arguments (ignored).
#'
#' @keywords internal
#' @noRd
plot.saomnk_cfc <- function(x, ...) {
  stop("plot.saomnk_cfc() is not yet implemented.", call. = FALSE)
}


# ---------------------------------------------------------------------------
# DGF: Density-GOF Frontier
# ---------------------------------------------------------------------------

#' Density-GOF Frontier (DGF)
#'
#' Analytic approximation of the density threshold beyond which network model
#' GOF degrades. Identifies the binding statistic (typically triangles or
#' GWESP) that drives degeneracy at a given density.
#'
#' @param n_nodes Integer. Number of nodes in the network.
#' @param density Numeric. Observed network density (0 to 1).
#' @param model_terms Character vector. ERGM/TERGM terms in the specification
#'   (e.g., \code{c("edges", "mutual", "gwesp.OTP")}).
#' @param directed Logical. Whether the network is directed (default TRUE).
#'
#' @return A list with components:
#'   \describe{
#'     \item{expected_triangles}{Numeric. Expected triangle count under
#'       Bernoulli assumption at the given density.}
#'     \item{max_triangles}{Numeric. Maximum possible triangles.}
#'     \item{triangle_density}{Numeric. Ratio of expected to max.}
#'     \item{binding_statistic}{Character. The term most likely to cause
#'       degeneracy.}
#'     \item{degeneracy_risk}{Character. "low", "moderate", or "high".}
#'     \item{n_nodes}{Integer.}
#'     \item{density}{Numeric.}
#'   }
#'
#' @details
#' For a directed network with \eqn{n} nodes and density \eqn{d}, the expected
#' number of transitive triples under a Bernoulli model is approximately
#' \eqn{n(n-1)(n-2) d^3}. When this count is large relative to the number of
#' edges, GWESP and triangle-based statistics become near-degenerate.
#'
#' @keywords internal
#' @noRd
saomnk_dgf <- function(n_nodes, density, model_terms, directed = TRUE) {

  if (n_nodes < 3) stop("Need at least 3 nodes.", call. = FALSE)
  if (density < 0 || density > 1) stop("Density must be in [0, 1].", call. = FALSE)

  # Dyad and edge counts
  if (directed) {
    n_dyads <- n_nodes * (n_nodes - 1)
  } else {
    n_dyads <- n_nodes * (n_nodes - 1) / 2
  }
  expected_edges <- n_dyads * density

  # Triangle counts (transitive triples for directed, triangles for undirected)
  if (directed) {
    max_triangles <- n_nodes * (n_nodes - 1) * (n_nodes - 2)
    expected_triangles <- max_triangles * density^3
  } else {
    max_triangles <- choose(n_nodes, 3)
    expected_triangles <- max_triangles * density^3
  }

  triangle_density <- expected_triangles / max(max_triangles, 1)

  # Identify binding statistic
  triangle_terms <- c("gwesp", "gwesp.OTP", "gwesp.ITP", "gwesp.OSP",
                       "gwesp.ISP", "triangle", "ttriple", "ctriple",
                       "transitiveties", "gwdsp")
  has_triangle_term <- any(tolower(model_terms) %in% tolower(triangle_terms))

  if (has_triangle_term && density > 0.15) {
    binding_statistic <- model_terms[tolower(model_terms) %in%
                                       tolower(triangle_terms)][1]
  } else {
    binding_statistic <- "none"
  }

  # Risk assessment
  if (density > 0.3 && has_triangle_term) {
    risk <- "high"
  } else if (density > 0.15 && has_triangle_term) {
    risk <- "moderate"
  } else if (density > 0.4) {
    risk <- "moderate"
  } else {
    risk <- "low"
  }

  list(
    expected_triangles = round(expected_triangles, 1),
    max_triangles      = max_triangles,
    triangle_density   = round(triangle_density, 6),
    binding_statistic  = binding_statistic,
    degeneracy_risk    = risk,
    n_nodes            = n_nodes,
    density            = density
  )
}


#' DARP: Degeneracy-Aware Reporting Protocol
#'
#' Placeholder for structured degeneracy reporting. Will produce a formatted
#' summary of model convergence, degeneracy diagnostics, and specification
#' sensitivity conditional on network density.
#'
#' @param ... Reserved for future arguments.
#'
#' @return A list (stub).
#'
#' @keywords internal
#' @noRd
saomnk_darp <- function(...) {
  stop("saomnk_darp() is not yet implemented. Coming in a future release.",
       call. = FALSE)
}


# ---------------------------------------------------------------------------
# Crosswalk: SAOM-TERGM effect mapping
# ---------------------------------------------------------------------------

#' Default SAOM-TERGM Effect Crosswalk
#'
#' Returns a data.frame mapping theoretically equivalent effects between
#' SAOM (RSiena) and TERGM/ERGM parameterizations.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{saom_effect}{Character. RSiena effect name.}
#'     \item{tergm_term}{Character. ERGM/TERGM model term.}
#'     \item{category}{Character. Structural category (endogenous, exogenous,
#'       covariate).}
#'     \item{sign_convention}{Character. Whether the expected sign relationship
#'       is "same" or "opposite" across frameworks.}
#'     \item{notes}{Character. Implementation notes.}
#'   }
#'
#' @keywords internal
#' @noRd
saomnk_default_crosswalk <- function() {
  data.frame(
    saom_effect = c(
      # -- Rate / baseline --
      "Rate",
      "density (outdegree)",
      # -- Reciprocity --
      "reciprocity",
      # -- Transitivity / closure --
      "transitive triplets",
      "transitive ties",
      "three-cycles",
      "gwespFF",
      "gwespBB",
      "gwespFB",
      "gwespBF",
      # -- Degree / centralization --
      "indegree popularity (sqrt)",
      "outdegree popularity (sqrt)",
      "outdegree activity (sqrt)",
      "indegree activity (sqrt)",
      # -- Covariate effects --
      "altX (alter)",
      "egoX (ego)",
      "sameX (similarity)",
      "diffX",
      "simX (ego-alter similarity)",
      "X ego",
      "X alter",
      "same X",
      # -- Structural --
      "isolate",
      "inPop",
      "outAct"
    ),
    tergm_term = c(
      # -- Rate / baseline --
      NA_character_,
      "edges",
      # -- Reciprocity --
      "mutual",
      # -- Transitivity / closure --
      "ttriple",
      "transitiveties",
      "ctriple",
      "gwesp.OTP",
      "gwesp.ITP",
      "gwesp.OSP",
      "gwesp.ISP",
      # -- Degree / centralization --
      "nodeicov (log)",
      "nodeocov (log)",
      "nodeocov",
      "nodeicov",
      # -- Covariate effects --
      "nodeicov",
      "nodeocov",
      "nodematch",
      "absdiff",
      "absdiff (negative)",
      "nodeocov",
      "nodeicov",
      "nodematch",
      # -- Structural --
      "isolates",
      "gwdsp",
      "nodeocov"
    ),
    category = c(
      "rate", "baseline",
      "endogenous",
      "endogenous", "endogenous", "endogenous",
      "endogenous", "endogenous", "endogenous", "endogenous",
      "degree", "degree", "degree", "degree",
      "covariate", "covariate", "covariate", "covariate",
      "covariate", "covariate", "covariate", "covariate",
      "structural", "structural", "structural"
    ),
    sign_convention = c(
      NA_character_, "same",
      "same",
      "same", "same", "same",
      "same", "same", "same", "same",
      "same", "same", "same", "same",
      "same", "same", "same", "same",
      "opposite", "same", "same", "same",
      "same", "same", "same"
    ),
    notes = c(
      "SAOM rate has no TERGM equivalent; TERGM uses formation/dissolution",
      "SAOM outdegree ~ TERGM edges (intercept-like)",
      "Direct mapping",
      "SAOM transitive triplets ~ TERGM ttriple (count)",
      "SAOM transitive ties ~ TERGM transitiveties",
      "SAOM three-cycles ~ TERGM ctriple",
      "Geometrically weighted OTP",
      "Geometrically weighted ITP",
      "Geometrically weighted OSP",
      "Geometrically weighted ISP",
      "SAOM sqrt transform vs TERGM log transform",
      "SAOM sqrt transform vs TERGM log transform",
      "Direct mapping with possible scale difference",
      "Direct mapping with possible scale difference",
      "Alter covariate effect",
      "Ego covariate effect",
      "Homophily (match on categorical)",
      "Heterophily / absolute difference",
      "Continuous similarity (note sign flip)",
      "Ego attribute on outgoing ties",
      "Alter attribute on incoming ties",
      "Categorical match",
      "No-tie tendency",
      "Indirect connectivity",
      "Out-degree activity"
    ),
    stringsAsFactors = FALSE
  )
}


#' Map effects between SAOM and TERGM
#'
#' Given vectors of SAOM and TERGM effect names, returns matched pairs using
#' the crosswalk.
#'
#' @param saom_effects Character vector. Effect names from SAOM output.
#' @param tergm_effects Character vector. Effect names from TERGM output.
#' @param crosswalk A data.frame from \code{\link{saomnk_default_crosswalk}}
#'   or custom.
#'
#' @return A data.frame of matched pairs with columns: saom_effect, tergm_term,
#'   category, sign_convention.
#'
#' @keywords internal
#' @noRd
saomnk_map_effects <- function(saom_effects, tergm_effects,
                                crosswalk = saomnk_default_crosswalk()) {

  # Find SAOM effects in the crosswalk
  matched <- crosswalk[crosswalk$saom_effect %in% saom_effects &
                         !is.na(crosswalk$tergm_term), ]

  # Filter to only those where the TERGM side is also present
  # Use partial matching for TERGM terms (they may include parentheticals)
  matched_final <- matched[sapply(matched$tergm_term, function(tt) {
    any(grepl(gsub("\\.", "\\\\.", tt), tergm_effects, fixed = FALSE))
  }), ]

  if (nrow(matched_final) == 0) {
    message("No matching effects found between SAOM and TERGM outputs.")
  }

  matched_final[, c("saom_effect", "tergm_term", "category", "sign_convention")]
}


# ---------------------------------------------------------------------------
# Estimate extraction utilities
# ---------------------------------------------------------------------------

#' Extract estimates from a sienaFit object
#'
#' Pulls effect names, estimates, standard errors, and convergence diagnostics
#' from an RSiena model fit into a tidy data.frame suitable for
#' \code{\link{saomnk_sai}}.
#'
#' @param fit A \code{sienaFit} object from \code{RSiena::siena07()}.
#' @param specification Character. A label for this specification (used as the
#'   \code{specification} column in the output).
#' @param include_rate Logical. Whether to include rate parameters (default
#'   FALSE).
#'
#' @return A data.frame with columns: effect, specification, estimate,
#'   std_error, convergence_t.
#'
#' @export
saomnk_extract_estimates_saom <- function(fit, specification = "saom",
                                           include_rate = FALSE) {
  if (!requireNamespace("RSiena", quietly = TRUE)) {
    stop("Package 'RSiena' required for saomnk_extract_estimates_saom().",
         call. = FALSE)
  }

  # Extract from sienaFit object
  effs <- fit$effects
  theta <- fit$theta
  se <- fit$se

  # Build data.frame
  out <- data.frame(
    effect        = effs$effectName,
    specification = specification,
    estimate      = theta,
    std_error     = se,
    type          = effs$type,
    stringsAsFactors = FALSE
  )

  # Convergence t-ratios (if available)
  if (!is.null(fit$tconv)) {
    out$convergence_t <- fit$tconv
  } else {
    out$convergence_t <- NA_real_
  }

  # Optionally drop rate parameters
  if (!include_rate) {
    out <- out[out$type != "rate", ]
  }

  out$type <- NULL
  rownames(out) <- NULL
  return(out)
}


#' Extract estimates from a TERGM/btergm fit object
#'
#' Pulls coefficient names, estimates, and standard errors from a tergm or
#' btergm model fit into a tidy data.frame suitable for
#' \code{\link{saomnk_sai}}.
#'
#' @param fit A model fit from \code{tergm::tergm()}, \code{btergm::btergm()},
#'   or \code{ergm::ergm()}.
#' @param specification Character. A label for this specification.
#'
#' @return A data.frame with columns: effect, specification, estimate,
#'   std_error.
#'
#' @importFrom stats coef
#' @keywords internal
#' @noRd
saomnk_extract_estimates_tergm <- function(fit, specification = "tergm") {

  # Try to extract coefficients generically
  coefs <- tryCatch(
    stats::coef(fit),
    error = function(e) {
      tryCatch(
        fit@coef,
        error = function(e2) {
          stop("Cannot extract coefficients from this object type.",
               call. = FALSE)
        }
      )
    }
  )

  # Standard errors
  se <- tryCatch({
    summary_obj <- summary(fit)
    if (is.matrix(summary_obj$coefficients)) {
      summary_obj$coefficients[, "Std. Error"]
    } else if (!is.null(fit@se)) {
      fit@se
    } else {
      rep(NA_real_, length(coefs))
    }
  }, error = function(e) {
    rep(NA_real_, length(coefs))
  })

  data.frame(
    effect        = names(coefs),
    specification = specification,
    estimate      = as.numeric(coefs),
    std_error     = as.numeric(se),
    stringsAsFactors = FALSE
  )
}


#' Format SAI table for publication
#'
#' Produces a cleanly formatted data.frame suitable for inclusion in a
#' manuscript table.
#'
#' @param sai_obj An object of class \code{"saomnk_sai"}.
#' @param digits Integer. Number of decimal places (default 3).
#' @param stars Logical. Add significance stars to SAI_composite (default TRUE).
#'   Stars: *** >= 0.90, ** >= 0.70, * >= 0.50.
#'
#' @return A data.frame formatted for publication.
#'
#' @keywords internal
#' @noRd
saomnk_format_sai_table <- function(sai_obj, digits = 3, stars = TRUE) {

  tbl <- sai_obj$table

  out <- data.frame(
    Effect        = tbl$effect,
    `SAI (sign)`  = sprintf(paste0("%.", digits, "f"), tbl$sai_sign),
    `SAI (sig)`   = sprintf(paste0("%.", digits, "f"), tbl$sai_sig),
    `SAI`         = sprintf(paste0("%.", digits, "f"), tbl$sai_composite),
    `N specs`     = tbl$n_specs,
    `N sig`       = tbl$n_significant,
    `Median est.` = sprintf(paste0("%.", digits + 1, "f"), tbl$median_estimate),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (stars) {
    star_str <- ifelse(tbl$sai_composite >= 0.90, "***",
                  ifelse(tbl$sai_composite >= 0.70, "**",
                    ifelse(tbl$sai_composite >= 0.50, "*", "")))
    out$SAI <- paste0(out$SAI, star_str)
  }

  return(out)
}
