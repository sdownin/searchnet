#' @title S3 Print Methods for User-Facing searchnet Objects
#' @description
#' Every S3 method on a user-facing constructor return lives in this file:
#' \code{print.saomnk_model}, \code{print.saomnk_shock},
#' \code{print.saomnk_assent}, and \code{print.saomnk_summary}.  Keeping them
#' together (rather than next to the constructors that build the objects) means
#' a reader auditing the console-facing behaviour of the package reads one
#' file.
#'
#' NAMESPACE is manually maintained (see its header): each method here must
#' have a matching \code{S3method(print, <class>)} line there, and
#' \code{tools/check_namespace_sync.R} fails if one is missing.
#'
#' @name saomnk-methods
NULL


# ---------------------------------------------------------------------------- #
#  Internal label helpers
# ---------------------------------------------------------------------------- #

#' Friendly label for an RSiena shortcode
#'
#' Reverses \code{.EFFECT_MAP} (saomnk-api.R): "inPop" prints as
#' "popularity (inPop)" so the console shows both the user-facing vocabulary
#' and the RSiena name the engine actually registers.
#'
#' @param shortcode Character scalar, an RSiena effect shortName.
#' @return Character scalar label.
#' @keywords internal
#' @noRd
.saomnk_effect_label <- function(shortcode) {
  friendly <- names(.EFFECT_MAP)[match(shortcode, .EFFECT_MAP)]
  if (length(friendly) == 1 && !is.na(friendly)) {
    sprintf("%s (%s)", friendly, shortcode)
  } else {
    as.character(shortcode)
  }
}

#' Fixed/free marker for an effect specification
#' @param fix The \code{fix} entry of an effect spec (logical or NULL).
#' @return "[fixed]" or "[free]".
#' @keywords internal
#' @noRd
.saomnk_fix_label <- function(fix) {
  if (isTRUE(fix)) "[fixed]" else "[free]"
}


# ---------------------------------------------------------------------------- #
#  print.saomnk_model
# ---------------------------------------------------------------------------- #

#' Print a SaoMNK Structure Model
#'
#' Compact console summary of a specification built by
#' \code{\link{saomnk_model}}: the effects with their parameter values and
#' fixed/free status, any actor or component covariates, static influence
#' matrices (\code{coDyadCovars}), time-varying influence arrays
#' (\code{varDyadCovars}), and whether a coevolving behaviour DV
#' (\code{dv_behavior}) is attached.  A reader typing the object at the
#' console sees the specification, not a raw list dump.
#'
#' @param x A \code{saomnk_model} object.
#' @param ... Ignored; present for S3 consistency.
#' @return \code{x}, invisibly.
#' @export
#' @examples
#' saomnk_model(density = -0.5, popularity = 0.2,
#'              influence_matrix = saomnk_block_diagonal(12, 4))
print.saomnk_model <- function(x, ...) {

  dv <- x$dv_bipartite

  cat("SaoMNK Structure Model\n")
  cat("----------------------\n")

  effs <- dv$effects
  cat(sprintf("Effects on the bipartite network DV (%d):\n",
              length(effs)))
  for (e in effs) {
    cat(sprintf("  %-22s theta = %6s  %s\n",
                .saomnk_effect_label(e$effect),
                format(e$parameter), .saomnk_fix_label(e$fix)))
  }

  covs <- dv$coCovars
  if (length(covs)) {
    cat(sprintf("Actor/component covariates (%d):\n", length(covs)))
    for (cv in covs) {
      len <- if (!is.null(cv$x)) sprintf("  (length %d)", length(cv$x)) else ""
      cat(sprintf("  %-22s theta = %6s  %s%s\n",
                  cv$effect, format(cv$parameter),
                  .saomnk_fix_label(cv$fix), len))
    }
  }

  dycovs <- dv$coDyadCovars
  if (length(dycovs)) {
    cat(sprintf("Influence / dyadic covariates (%d):\n", length(dycovs)))
    for (dc in dycovs) {
      mat_dim <- if (!is.null(dc$x)) {
        paste(dim(dc$x), collapse = " x ")
      } else "?"
      kind <- if (identical(dc$effect, "XWX")) "influence matrix W" else "dyad covariate"
      cat(sprintf("  %-22s theta = %6s  %s  [%s, %s]\n",
                  .saomnk_effect_label(dc$effect), format(dc$parameter),
                  .saomnk_fix_label(dc$fix), kind, mat_dim))
    }
  }

  vdycovs <- dv$varDyadCovars
  if (length(vdycovs)) {
    cat(sprintf("Time-varying influence arrays (%d):\n", length(vdycovs)))
    for (vc in vdycovs) {
      dims <- if (!is.null(vc$x) && length(dim(vc$x)) == 3L) {
        sprintf("%d x %d over %d period(s)",
                dim(vc$x)[1], dim(vc$x)[2], dim(vc$x)[3])
      } else "?"
      cat(sprintf("  %-22s theta = %6s  %s  [%s]\n",
                  .saomnk_effect_label(vc$effect), format(vc$parameter),
                  .saomnk_fix_label(vc$fix), dims))
    }
  }

  if (length(dv$interactions)) {
    cat(sprintf("Interactions declared: %d\n", length(dv$interactions)))
  }

  bh <- x$dv_behavior
  if (!is.null(bh)) {
    n_eff  <- length(bh$effects)
    waves  <- if (!is.null(bh$waves)) bh$waves else NA_integer_
    cat(sprintf(
      "Behaviour DV (dv_behavior): present -- %d effect(s), %s wave(s), nodeSet %s\n",
      n_eff,
      ifelse(is.na(waves), "?", format(waves)),
      if (!is.null(bh$nodeSet)) bh$nodeSet else "?"))
  }

  invisible(x)
}


# ---------------------------------------------------------------------------- #
#  print.saomnk_shock
# ---------------------------------------------------------------------------- #

#' Print a SaoMNK Shock Specification
#'
#' Shows which parameter(s) the shock sets, to what values, and the relative
#' portion of the simulation chain the segment occupies.  A single shock
#' object defines one segment; the values in force \emph{before} it are set by
#' the preceding segment (or the base model), so the "from" side of the change
#' is only determined once all shocks are passed together to
#' \code{\link{saomnk_run}}.
#'
#' @param x A \code{saomnk_shock} object.
#' @param ... Ignored; present for S3 consistency.
#' @return \code{x}, invisibly.
#' @export
#' @examples
#' saomnk_shock("density", parameter = -2.0, portion = 1)
print.saomnk_shock <- function(x, ...) {

  cat("SaoMNK shock segment\n")
  cat(sprintf("  portion: %d (relative share of the simulation chain)\n",
              x$portion))
  cat(sprintf("  sets %d parameter(s) for this segment:\n", length(x$effect)))
  for (i in seq_along(x$effect)) {
    cat(sprintf("    %-22s ->  theta = %s\n",
                .saomnk_effect_label(x$effect[i]), format(x$parameter[i])))
  }
  cat("  (segment boundaries are set jointly by all shocks passed to saomnk_run)\n")

  invisible(x)
}


# ---------------------------------------------------------------------------- #
#  print.saomnk_assent
# ---------------------------------------------------------------------------- #

#' Print a SaoMNK Assent (Confirmation) Rule
#'
#' Reports the rule type -- uniform probability, per-actor probabilities, a
#' dyad-specific probability matrix, or attribute screening -- with its
#' parameters, plus any component-side selectivity.
#'
#' @param x A \code{saomnk_assent} object built by \code{\link{saomnk_assent}}.
#' @param ... Ignored; present for S3 consistency.
#' @return \code{x}, invisibly.
#' @export
#' @examples
#' saomnk_assent(prob = 0.5)
print.saomnk_assent <- function(x, ...) {

  cat("SaoMNK assent (confirmation) rule")
  if (!is.null(x$name)) cat(sprintf(": '%s'", x$name))
  cat("\n")

  if (!is.null(x$prob) && is.matrix(x$prob)) {
    cat(sprintf("  type: dyad-specific probability matrix (%d x %d)\n",
                nrow(x$prob), ncol(x$prob)))
  } else if (!is.null(x$prob) && length(x$prob) == 1L) {
    cat(sprintf("  type: uniform confirmation probability, p = %s\n",
                format(x$prob)))
  } else if (!is.null(x$prob)) {
    cat(sprintf("  type: per-actor confirmation probabilities (length %d, range [%s, %s])\n",
                length(x$prob), format(min(x$prob)), format(max(x$prob))))
  } else {
    cat("  type: attribute screening\n")
    cat(sprintf("    confirmed at %s when actor_attribute > %s, else at %s\n",
                format(x$rate_high), format(x$threshold), format(x$rate_low)))
    cat(sprintf("    actor_attribute: length %d\n", length(x$actor_attribute)))
  }

  if (!is.null(x$component_selectivity)) {
    cat(sprintf(
      "  component selectivity: length %d, max %s (confirmation scaled by 1 - selectivity)\n",
      length(x$component_selectivity), format(max(x$component_selectivity))))
  }

  invisible(x)
}


# ---------------------------------------------------------------------------- #
#  print.saomnk_summary
# ---------------------------------------------------------------------------- #

#' Print a SaoMNK Model Summary Table
#'
#' Displays the regression-style table returned by
#' \code{\link{saomnk_summary}} as formatted text.  Without this method the
#' return value would auto-print as a quoted character string full of
#' \code{"\\n"} escapes.
#'
#' @param x A \code{saomnk_summary} object (a classed character string).
#' @param ... Ignored; present for S3 consistency.
#' @return \code{x}, invisibly.
#' @export
#' @examples
#' \dontrun{
#' saomnk_summary(env)   # auto-prints via this method
#' }
print.saomnk_summary <- function(x, ...) {
  txt <- paste(unclass(x), collapse = "\n")
  cat(txt)
  if (!grepl("\n$", txt)) cat("\n")
  invisible(x)
}
