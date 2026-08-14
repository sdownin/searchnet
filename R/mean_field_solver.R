#' @title Mean-Field Equilibrium Solver for the SaoMNK Brock-Durlauf Reduction
#' @description Standalone helper operationalising Theorem~4 of the SaoMNK
#'   proof set (the Gibbs/Brock-Durlauf equivalence): the mean-field
#'   self-consistency equation
#'   \deqn{m^{*} = \tanh\!\left(\frac{\beta_{\mathrm{eff}}\, m^{*}}{2T}\right)}{
#'         m* = tanh( beta_eff * m* / (2 * T) )}
#'   with effective coupling
#'   \deqn{\beta_{\mathrm{eff}} = \tfrac{1}{2} (M - 1)\, \theta_{\mathrm{inPop}}}{
#'         beta_eff = 0.5 * (M - 1) * theta_inPop}
#'   as recorded in \code{inst/proofs/PROOF_TABLE.md} (rows L8, L10, L16).
#'   In the Curie--Weiss form used here, the bifurcation
#'   threshold is \eqn{\beta_{\mathrm{eff}} > 2T} (equivalently
#'   \eqn{\beta J = 1} in the underlying proof), under which the trivial root
#'   \eqn{m^{*} = 0} loses stability and a symmetric pair \eqn{\pm m^{*}_{+}}
#'   emerges.
#'
#'   This file complements the existing \code{searchnet-brock-durlauf.R}
#'   utilities (\code{\link{bd_self_consistency}} etc.) by exposing a single
#'   end-user-friendly entry-point parameterised in the SaoMNK quantities a
#'   strategy researcher actually controls (\code{theta_inPop}, \code{M},
#'   \code{T}) rather than the spin-form \code{(beta, J, h)} triple of
#'   Brock & Durlauf.
#'
#'   \strong{What this object is, and is not.} This solver returns the
#'   zero-field \emph{linear} Curie--Weiss roots: the reference object of
#'   PROOF_TABLE.md L16's Option C, valid as a description of the live
#'   simulation only in the linearised regime near \eqn{p = 1/2} and below
#'   threshold. It is \emph{not} the stationary law of a simulation run with
#'   RSiena's \code{inPop}, whose evaluation delta is sqrt-form; that law is
#'   the Option B fixed point, \code{\link{saomnk_inpop_self_consistency}}.
#'   Treating these roots as the prediction for a supercritical \code{inPop}
#'   run overstates the coupling (the linearised slope at the realised
#'   equilibrium is the correct local coupling, not
#'   \eqn{0.5 (M-1) \theta}) and was the source of a persistent spurious
#'   discrepancy of \eqn{\approx 0.38} in the Theorem 4 empirical check,
#'   diagnosed 2026-08-14. \code{diagnose_mean_field_fit()} reports both
#'   objects and an \code{in_BD_regime} flag.
#' @name mean-field-solver
NULL


# ---------------------------------------------------------------------------- #
#  solve_mean_field
# ---------------------------------------------------------------------------- #

#' Solve the SaoMNK Mean-Field Self-Consistency Equation
#'
#' Solves the Curie--Weiss / Brock--Durlauf self-consistency equation for the
#' equilibrium population mean \eqn{m^{*}} of the SaoMNK logit ministep
#' under the symmetric, mean-field specialisation of Theorem~4.  Specifically,
#' for an inPop-only structure model with \eqn{M} symmetric actors and Gibbs
#' temperature \eqn{T}, the population magnetisation satisfies
#' \deqn{m^{*} = \tanh\!\left(\frac{\beta_{\mathrm{eff}}\, m^{*}}{2T}\right),
#'       \quad
#'       \beta_{\mathrm{eff}} \equiv \tfrac{1}{2} (M - 1)\, \theta_{\mathrm{inPop}}.}
#'
#' Multiple equilibria are returned when present.  The routine bracketed-roots
#' the residual on a fine grid in \eqn{[-1, 1]} and refines each bracket with
#' \code{\link[stats]{uniroot}}; for users who want a single-seed iteration
#' trace, fixed-point iteration from \code{seeds} is also performed and the
#' convergence path is returned in \code{convergence_path}.
#'
#' @param theta_inPop Numeric (length 1).  The RSiena \code{inPop} (popularity)
#'   coefficient.  Positive values induce coordination / herding; negative
#'   values induce anti-coordination (whose unique fixed point is
#'   \eqn{m^{*} = 0}).
#' @param M Integer (\eqn{\geq 2}).  Number of actors.
#' @param T Numeric (\eqn{> 0}).  Gibbs temperature (default \code{1}).
#' @param tol Numeric (\eqn{> 0}).  Convergence tolerance for fixed-point
#'   iteration and \code{uniroot} (default \code{1e-8}).
#' @param max_iter Integer (\eqn{\geq 1}).  Maximum number of fixed-point
#'   iterations per seed (default \code{1000}).
#' @param seeds Numeric vector of starting values in \eqn{[-1, 1]} for the
#'   fixed-point iteration trace (default \code{c(-0.99, 0, 0.99)}).
#' @param dedup_digits Integer.  Number of digits to round to when
#'   de-duplicating equilibria found from different seeds / brackets
#'   (default \code{6}).
#' @return A \code{list} with components:
#'   \describe{
#'     \item{\code{m_star}}{Numeric vector of distinct fixed points found,
#'       sorted ascending.  Length \eqn{1} subcritical, length \eqn{3}
#'       supercritical (typically \eqn{-m^{*}_{+}, 0, +m^{*}_{+}}).}
#'     \item{\code{beta_eff}}{Effective coupling
#'       \eqn{\beta_{\mathrm{eff}} = (M - 1)\, \theta_{\mathrm{inPop}} / 2}.}
#'     \item{\code{above_critical}}{Logical: \code{TRUE} iff
#'       \code{beta_eff > 2 * T} (multi-equilibria regime).}
#'     \item{\code{T}}{Temperature used.}
#'     \item{\code{M}}{Actor count used.}
#'     \item{\code{theta_inPop}}{Coupling used.}
#'     \item{\code{convergence_path}}{Named list (one element per seed) of
#'       numeric vectors recording the fixed-point trajectory; the final
#'       element of each vector is the converged value.}
#'   }
#' @details The factor of \eqn{2} in the denominator of the self-consistency
#'   equation is the standard Brock--Durlauf spin-coding convention; it
#'   coincides with the \eqn{\beta J} parameterisation of
#'   \code{\link{bd_self_consistency}} via
#'   \eqn{\beta J = \beta_{\mathrm{eff}} / (2T)}.
#' @references
#'   Brock, W. A. & Durlauf, S. N. (2001). Discrete choice with social
#'   interactions. *Review of Economic Studies* 68(2), 235--260.
#'
#'   Downing, S. (2026). Theorem 4: Gibbs stationary distribution and the
#'   Brock--Durlauf reduction of the SaoMNK ministep.
#'   \code{inst/proofs/PROOF_TABLE.md}, rows L8, L10, L16--L18.
#' @examples
#' ## Subcritical: theta_inPop * (M - 1) / 2 < 2 * T  =>  unique m* = 0
#' fp <- solve_mean_field(theta_inPop = 0.05, M = 10, T = 1)
#' fp$m_star          # ~ 0
#' fp$above_critical  # FALSE
#'
#' ## Supercritical: large coupling => symmetric pair plus unstable 0
#' fp2 <- solve_mean_field(theta_inPop = 1.0, M = 10, T = 1)
#' fp2$m_star          # length 3: (-m+, 0, +m+)
#' fp2$above_critical  # TRUE
#' @export
solve_mean_field <- function(theta_inPop, M, T = 1,
                             tol = 1e-8, max_iter = 1000L,
                             seeds = c(-0.99, 0, 0.99),
                             dedup_digits = 6L) {

  ## ---- Validation ----------------------------------------------------------
  stopifnot(is.numeric(theta_inPop), length(theta_inPop) == 1L,
            is.finite(theta_inPop))
  stopifnot(is.numeric(M), length(M) == 1L, is.finite(M), M >= 2)
  stopifnot(is.numeric(T), length(T) == 1L, is.finite(T), T > 0)
  stopifnot(is.numeric(tol), length(tol) == 1L, tol > 0)
  stopifnot(is.numeric(max_iter), length(max_iter) == 1L, max_iter >= 1L)
  stopifnot(is.numeric(seeds), length(seeds) >= 1L,
            all(is.finite(seeds)), all(seeds >= -1), all(seeds <= 1))
  stopifnot(is.numeric(dedup_digits), length(dedup_digits) == 1L,
            dedup_digits >= 1L)

  ## ---- Effective coupling and bifurcation flag ----------------------------
  ## Per proof eq. (beta_eff): beta_eff = 0.5 * (M - 1) * theta_inPop.
  beta_eff <- 0.5 * (M - 1) * theta_inPop

  ## Bifurcation: stability of m=0 root requires |d/dm tanh(beta_eff*m/(2T))|
  ## at m=0 = beta_eff/(2T) < 1, i.e. beta_eff < 2T.  Strict ">" means above
  ## critical (multi-equilibria regime).
  above_critical <- (beta_eff > 2 * T)

  ## Self-consistency map  m  ->  tanh(beta_eff * m / (2 * T))
  fp_map <- function(m) tanh(beta_eff * m / (2 * T))
  residual <- function(m) m - fp_map(m)

  ## ---- Fixed-point iteration from each seed (trace + candidate root) ------
  conv_paths <- vector("list", length(seeds))
  names(conv_paths) <- sprintf("seed_%g", seeds)
  fp_roots <- numeric(0)

  for (s_idx in seq_along(seeds)) {
    m <- seeds[s_idx]
    path <- numeric(max_iter + 1L)
    path[1L] <- m
    converged <- FALSE
    final_iter <- max_iter

    for (k in seq_len(max_iter)) {
      m_new <- fp_map(m)
      path[k + 1L] <- m_new
      if (abs(m_new - m) < tol) {
        converged <- TRUE
        final_iter <- k
        break
      }
      m <- m_new
    }

    conv_paths[[s_idx]] <- path[seq_len(final_iter + 1L)]
    if (converged) fp_roots <- c(fp_roots, path[final_iter + 1L])
  }

  ## ---- Bracketed root-finding on a fine grid (catches all equilibria) -----
  grid_n <- 2001L
  grid <- seq(-1, 1, length.out = grid_n)
  fvals <- residual(grid)
  bracket_roots <- numeric(0)

  exact_zeros <- which(fvals == 0)
  if (length(exact_zeros) > 0L) {
    bracket_roots <- c(bracket_roots, grid[exact_zeros])
  }
  sign_changes <- which(fvals[-grid_n] * fvals[-1L] < 0)
  for (idx in sign_changes) {
    r <- tryCatch(
      stats::uniroot(residual, lower = grid[idx], upper = grid[idx + 1L],
                     tol = tol)$root,
      error = function(e) NA_real_
    )
    if (is.finite(r)) bracket_roots <- c(bracket_roots, r)
  }

  ## ---- Combine, de-duplicate, sort ----------------------------------------
  all_roots <- c(fp_roots, bracket_roots)
  if (length(all_roots) == 0L) {
    ## Fall back to m = 0 (the trivial root must always solve the equation
    ## under the symmetric specialisation -- tanh(0) = 0).
    all_roots <- 0
  }
  all_roots <- sort(unique(round(all_roots, digits = dedup_digits)))

  ## In the supercritical regime we require all three roots {-m+, 0, m+}.
  ## When fixed-point iteration from m=0 stays at the unstable root but the
  ## grid did not bracket it (e.g. very small numerical residual), inject 0
  ## explicitly.
  if (above_critical && !any(abs(all_roots) < 10 * tol)) {
    all_roots <- sort(c(all_roots, 0))
  }

  list(
    m_star            = all_roots,
    beta_eff          = beta_eff,
    above_critical    = above_critical,
    T                 = T,
    M                 = M,
    theta_inPop       = theta_inPop,
    convergence_path  = conv_paths
  )
}
