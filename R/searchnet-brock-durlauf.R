#' @title Brock--Durlauf Mean-Field Reduction Utilities
#' @description Functions operationalizing Theorem 5 of the SaoMNK proof
#'   table (Part L), which establishes that SaoMNK reduces to the
#'   Brock & Durlauf (2001, RES) binary discrete-choice-with-social-
#'   interactions model in the M -> infinity, congestion-only, mean-field
#'   limit.
#'
#'   The five utility functions here (i) solve the canonical B&D
#'   self-consistency equation for the equilibrium magnetization,
#'   (ii) count equilibria (regime classification), (iii) compute the
#'   B&D social multiplier from either explicit parameters or a fitted
#'   SaoMNK environment, (iv) report the Landau-Ginzburg quartic
#'   coefficient and basin steepness from the AMR herding-steepness
#'   derivation, and (v) verify the M -> infinity reduction empirically
#'   by comparing simulated mean adoption to the analytical fixed point.
#'
#'   See \code{inst/proofs/PROOF_TABLE.md} (Part L, Theorem 5) and the
#'   AMR mathematical appendix for derivations.
#' @name searchnet-brock-durlauf
NULL


# ---------------------------------------------------------------------------- #
#  Internal helpers (NOT exported)
# ---------------------------------------------------------------------------- #

## B&D self-consistency residual on the spin form m in [-1, 1].
##   f(m) = m - tanh(beta * h + beta * J * m)
.bd_residual <- function(m, beta, J, h) {
  m - tanh(beta * h + beta * J * m)
}

## Spin-form magnetization m in [-1, 1] from a {0, 1} adoption fraction p.
##   m = 2 * p - 1
.bd_p_to_m <- function(p) 2 * p - 1

## Adoption fraction p in [0, 1] from spin-form magnetization m in [-1, 1].
##   p = (m + 1) / 2
.bd_m_to_p <- function(m) (m + 1) / 2


# ---------------------------------------------------------------------------- #
#  1. bd_self_consistency
# ---------------------------------------------------------------------------- #

#' Solve the Brock & Durlauf Self-Consistency Equation
#'
#' Solves the canonical B&D (2001) mean-field self-consistency equation
#' \deqn{m = \tanh(\beta h + \beta J m)}{m = tanh(beta * h + beta * J * m)}
#' for the spin-form magnetization \eqn{m \in [-1, 1]}.  When the model
#' admits multiple equilibria (high \eqn{\beta J}), all of them are
#' returned.
#'
#' @param beta Numeric inverse-temperature (selection intensity).  Must be
#'   non-negative.
#' @param J Numeric peer-interaction strength in spin form.  Positive
#'   \eqn{J} = coordination / herding regime; negative \eqn{J} =
#'   anti-coordination / congestion regime.
#' @param h Numeric private-utility bias (external field) in spin form.
#' @param x0 Numeric starting value for fixed-point iteration when
#'   \code{all_roots = FALSE} (default \code{0}, must be in \eqn{[-1, 1]}).
#' @param tol Numeric convergence tolerance (default \code{1e-10}).
#' @param max_iter Integer maximum number of fixed-point iterations
#'   (default \code{1000}).
#' @param all_roots Logical.  If \code{TRUE} (default), scan
#'   \eqn{[-1, 1]} at 1001 grid points to bracket all sign changes of the
#'   residual \eqn{f(m) = m - \tanh(\beta h + \beta J m)} and return all
#'   equilibrium values via \code{\link[stats]{uniroot}}.  If
#'   \code{FALSE}, run fixed-point iteration starting from \code{x0} and
#'   return a single scalar.
#' @return A numeric vector of equilibrium magnetizations.  Length 1 in
#'   the unique-equilibrium regime, length 3 in the multiple-equilibria
#'   regime (when \code{all_roots = TRUE}); a scalar when
#'   \code{all_roots = FALSE}.
#' @references
#'   Brock, W. A. & Durlauf, S. N. (2001). Discrete choice with social
#'   interactions. *Review of Economic Studies* 68(2), 235--260.
#' @examples
#' ## Unique equilibrium (subcritical: beta * J < 1, h = 0)
#' bd_self_consistency(beta = 1, J = 0.5, h = 0)
#'
#' ## Three equilibria (supercritical: beta * J > 1, h = 0)
#' bd_self_consistency(beta = 1, J = 2, h = 0)
#'
#' ## Tilted field, h != 0: fixed-point iteration
#' bd_self_consistency(beta = 1, J = 2, h = 0.1, all_roots = FALSE)
#' @export
bd_self_consistency <- function(beta, J, h,
                                x0 = 0, tol = 1e-10,
                                max_iter = 1000, all_roots = TRUE) {

  stopifnot(is.numeric(beta), length(beta) == 1, beta >= 0)
  stopifnot(is.numeric(J),    length(J)    == 1)
  stopifnot(is.numeric(h),    length(h)    == 1)
  stopifnot(is.numeric(x0),   length(x0)   == 1, x0 >= -1, x0 <= 1)
  stopifnot(is.numeric(tol),  length(tol)  == 1, tol > 0)
  stopifnot(is.numeric(max_iter), length(max_iter) == 1, max_iter >= 1)
  stopifnot(is.logical(all_roots), length(all_roots) == 1)

  if (all_roots) {
    grid <- seq(-1, 1, length.out = 1001)
    fvals <- .bd_residual(grid, beta = beta, J = J, h = h)
    roots <- numeric(0)

    ## Capture exact zeros lying on grid points (rare but possible)
    exact_zeros <- which(fvals == 0)
    if (length(exact_zeros) > 0) {
      roots <- c(roots, grid[exact_zeros])
    }

    ## Bracket every sign change and refine via uniroot
    sign_changes <- which(fvals[-length(fvals)] * fvals[-1] < 0)
    for (idx in sign_changes) {
      lo <- grid[idx]
      hi <- grid[idx + 1]
      r <- tryCatch(
        stats::uniroot(.bd_residual, lower = lo, upper = hi,
                       beta = beta, J = J, h = h, tol = tol)$root,
        error = function(e) NA_real_
      )
      if (is.finite(r)) roots <- c(roots, r)
    }

    if (length(roots) == 0) {
      ## Degenerate edge case: no sign change found (e.g., beta == 0).
      ## Fall back to fixed-point iteration from x0.
      return(bd_self_consistency(beta = beta, J = J, h = h,
                                 x0 = x0, tol = tol,
                                 max_iter = max_iter,
                                 all_roots = FALSE))
    }

    ## De-duplicate near-coincident roots (within sqrt(tol))
    roots <- sort(unique(round(roots, digits = -log10(sqrt(tol)))))
    return(roots)
  }

  ## Fixed-point iteration branch
  m <- x0
  for (k in seq_len(max_iter)) {
    m_new <- tanh(beta * h + beta * J * m)
    if (abs(m_new - m) < tol) {
      return(m_new)
    }
    m <- m_new
  }
  warning("bd_self_consistency: fixed-point iteration did not converge ",
          "within max_iter = ", max_iter, " (final residual ",
          format(abs(.bd_residual(m, beta, J, h)), digits = 3), ")")
  m
}


# ---------------------------------------------------------------------------- #
#  1b. saomnk_inpop_self_consistency
# ---------------------------------------------------------------------------- #

#' Solve the SaoMNK-inPop Self-Consistency Equation (Operational Fixed Point)
#'
#' Solves the adoption-form fixed-point equation that the live RSiena
#' bipartite SAOM with only \code{density} and \code{inPop} effects
#' active actually obeys at stationarity. This is *not* the canonical
#' Brock & Durlauf (2001) fixed point: B&D assumes a linear peer-mean
#' coupling \eqn{J m}, while RSiena's \code{inPop} statistic uses the
#' sqrt-transformed column-degree \eqn{\sqrt{n_j + 1}} as a variance
#' stabiliser. The two coincide only in the local linearisation around
#' \eqn{m = 1/2} (PROOF\_TABLE.md row L16); elsewhere the saomnk-inPop
#' fixed point differs from B&D's, and this function gives the former.
#'
#' The fixed-point equation is:
#' \deqn{m = \sigma\bigl(\beta(h_b + \theta_{\mathrm{inPop}} \sqrt{M m + 1})\bigr)}
#' where \eqn{\sigma(x) = 1/(1 + e^{-x})} is the logistic function, and
#' the social-interaction coefficient appears non-linearly through the
#' sqrt term.
#'
#' This function is the **Option B** reference for
#' \code{\link{verify_brock_durlauf_reduction}}: it gives the
#' theoretical equilibrium that the \emph{live RSiena simulation}
#' should converge to (matching the simulation's own functional form),
#' as opposed to \code{\link{bd_self_consistency}} which gives the
#' B&D limit that holds only in the local-linearisation regime.
#'
#' @param beta Numeric inverse-temperature.
#' @param theta_inPop Numeric SAOM \code{inPop} coefficient (the
#'   coefficient passed as \code{popularity} in
#'   \code{\link{saomnk_model}}).
#' @param h_b Numeric SAOM \code{density} coefficient (private-utility
#'   bias in adoption form).
#' @param M Integer actor count.
#' @param x0 Numeric initial guess in \eqn{[0, 1]}; default 0.5.
#' @param tol Numeric convergence tolerance; default \code{1e-10}.
#' @param max_iter Integer maximum iterations; default \code{1000}.
#' @return Adoption-form fixed point \eqn{m^* \in [0, 1]}.  When the
#'   equation has multiple solutions (rare for inPop given the sqrt
#'   non-linearity damps positive feedback), the iteration starting
#'   from \code{x0} converges to one of them; future versions may
#'   add an \code{all_roots} argument analogous to
#'   \code{\link{bd_self_consistency}}.
#' @references
#'   See PROOF\_TABLE.md row L16 for the linearisation that connects
#'   this fixed-point equation to the canonical
#'   \citep{BrockDurlauf2001} form.
#' @seealso \code{\link{bd_self_consistency}} for the B&D linear-
#'   coupling fixed point; \code{\link{verify_brock_durlauf_reduction}}
#'   for the harness that uses both as comparison targets.
#' @examples
#' ## Sub-critical regime around m = 0.5 (Option C regime where B&D
#' ## linearisation is locally accurate)
#' theta <- 0.5 / sqrt(2 * 100)        # B&D-rescaled inPop coef
#' saomnk_inpop_self_consistency(beta = 1, theta_inPop = theta,
#'                                h_b = -0.25, M = 100)
#' ## Compare to B&D linear form
#' bd_self_consistency(beta = 1, J = 0.5, h = -0.25, all_roots = FALSE)
#' @export
saomnk_inpop_self_consistency <- function(beta, theta_inPop, h_b, M,
                                          x0 = 0.5, tol = 1e-10,
                                          max_iter = 1000L) {
  stopifnot(is.numeric(beta), length(beta) == 1, beta >= 0,
            is.numeric(theta_inPop), length(theta_inPop) == 1,
            is.numeric(h_b), length(h_b) == 1,
            is.numeric(M), length(M) == 1, M >= 2,
            is.numeric(x0), length(x0) == 1, x0 >= 0, x0 <= 1,
            is.numeric(tol), length(tol) == 1, tol > 0,
            is.numeric(max_iter), length(max_iter) == 1, max_iter >= 1)

  sigmoid <- function(x) 1 / (1 + exp(-x))
  M_num <- as.numeric(M)

  m <- x0
  for (iter in seq_len(max_iter)) {
    m_new <- sigmoid(beta * (h_b + theta_inPop * sqrt(M_num * m + 1)))
    if (abs(m_new - m) < tol) {
      return(m_new)
    }
    m <- m_new
  }
  warning("saomnk_inpop_self_consistency: did not converge within ",
          max_iter, " iterations (final |delta| = ",
          format(abs(m_new - m), digits = 3), ")")
  m
}


# ---------------------------------------------------------------------------- #
#  2. bd_equilibrium_count
# ---------------------------------------------------------------------------- #

#' Count Equilibria of the Brock & Durlauf Self-Consistency Equation
#'
#' Returns the number of distinct equilibrium magnetizations of the B&D
#' mean-field model.  In the symmetric case (\code{h = 0}) this is
#' determined analytically by the Curie-Weiss criterion: the trivial
#' \eqn{m = 0} root is unstable and flanked by two symmetric stable
#' roots iff \eqn{\beta J > 1}.  For the asymmetric case
#' (\code{h != 0}), the count is computed by enumerating all roots via
#' \code{\link{bd_self_consistency}}.
#'
#' @param beta Numeric inverse-temperature.  Must be non-negative.
#' @param J Numeric peer-interaction strength in spin form.
#' @param h Numeric private-utility bias (default \code{0}).
#' @return Integer scalar: \code{1L} (unique) or \code{3L} (multiple
#'   equilibria).  Returns \code{1L} for any J <= 0 with h = 0 since
#'   anti-coordination admits only the trivial equilibrium.
#' @examples
#' bd_equilibrium_count(beta = 1, J = 0.5)   # 1 (subcritical)
#' bd_equilibrium_count(beta = 1, J = 2.0)   # 3 (supercritical)
#' bd_equilibrium_count(beta = 1, J = 2.0, h = 0.5)  # 1 (field destroys mult.)
#' @export
bd_equilibrium_count <- function(beta, J, h = 0) {

  stopifnot(is.numeric(beta), length(beta) == 1, beta >= 0)
  stopifnot(is.numeric(J),    length(J)    == 1)
  stopifnot(is.numeric(h),    length(h)    == 1)

  if (h == 0) {
    ## Stability of m = 0 root: derivative of tanh(beta*J*m) w.r.t. m at m=0
    ##   is beta*J*(1 - 0^2) = beta*J.
    ## When beta*J > 1, the trivial root is unstable and two symmetric
    ## stable roots appear (Curie-Weiss).
    if (beta * J > 1) {
      return(3L)
    }
    return(1L)
  }

  ## Asymmetric case: enumerate
  roots <- bd_self_consistency(beta = beta, J = J, h = h, all_roots = TRUE)
  as.integer(length(roots))
}


# ---------------------------------------------------------------------------- #
#  3. saomnk_social_multiplier
# ---------------------------------------------------------------------------- #

#' Brock & Durlauf Social Multiplier
#'
#' Computes the B&D (2001) social multiplier
#' \deqn{\mathcal{M} = \frac{1}{1 - \beta J (1 - m^{*2})}}{M = 1 / (1 - beta * J * (1 - m_star^2))}
#' which measures how a marginal change in private utility \eqn{h} is
#' amplified by social feedback.  As \eqn{\beta J (1 - m^{*2}) \to 1}
#' the multiplier diverges, signalling proximity to the Curie-Weiss
#' phase transition.
#'
#' Two calling conventions are supported:
#' \enumerate{
#'   \item Pass \code{beta}, \code{J}, \code{h} explicitly.  The
#'         equilibrium \eqn{m^*} is computed internally via
#'         \code{\link{bd_self_consistency}}.
#'   \item Pass a fitted SaoMNK environment via \code{env}.  The
#'         B&D-equivalent \eqn{J} and \eqn{h} are extracted from the
#'         environment's structure-model effects table; the
#'         empirical \eqn{m^*} is read off from the bipartite matrix
#'         via \eqn{m^* = 2 \bar{B} - 1}.
#' }
#' The spin-form recoding contributes a factor of 4 to \eqn{J}
#' (Rb4 of the proof table) which is applied automatically in the
#' env-based path.
#'
#' @param env Optional SaoMNK environment (\code{SaomNkRSienaBiEnv}) from
#'   which to extract parameters.  If \code{NULL} (default), explicit
#'   \code{beta}, \code{J}, \code{h} must be supplied.
#' @param beta Numeric inverse-temperature.  Required when \code{env} is
#'   \code{NULL}.
#' @param J Numeric spin-form peer-interaction strength.  Ignored when
#'   \code{env} is supplied.
#' @param h Numeric spin-form private-utility bias.  Ignored when
#'   \code{env} is supplied.
#' @param m_star Numeric equilibrium magnetization at which to evaluate
#'   the multiplier.  If \code{NULL}, computed internally.
#' @return A list with components:
#'   \item{multiplier}{The B&D social multiplier (numeric).}
#'   \item{m_star}{The equilibrium magnetization used (numeric).}
#'   \item{bd_J_spin}{The spin-form peer-interaction strength used (numeric).}
#'   \item{bd_h_spin}{The spin-form private bias used (numeric).}
#'   \item{warning_near_threshold}{Logical, \code{TRUE} when
#'     \code{abs(multiplier) > 10} (i.e., near the phase transition).}
#' @examples
#' ## Explicit-parameter form
#' saomnk_social_multiplier(beta = 1, J = 0.5, h = 0)
#'
#' ## Near phase transition
#' saomnk_social_multiplier(beta = 1, J = 0.95, h = 0)
#' @export
saomnk_social_multiplier <- function(env = NULL, beta = NULL,
                                     J = NULL, h = NULL,
                                     m_star = NULL) {

  ## ---- Path A: env-based extraction -------------------------------------- ##
  if (!is.null(env)) {
    if (is.null(beta)) beta <- 1

    ## Extract spin-form h from density effect
    bd_h_spin <- 0
    if (!is.null(env$theta_density) &&
        is.numeric(env$theta_density) &&
        length(env$theta_density) >= 1) {
      bd_h_spin <- -as.numeric(env$theta_density[[1]])
    } else if (!is.null(env$structure_model)) {
      sm_eff <- tryCatch(env$structure_model$dv_bipartite$effects,
                         error = function(e) NULL)
      if (!is.null(sm_eff)) {
        for (eff in sm_eff) {
          if (!is.null(eff$effect) && eff$effect == "density") {
            bd_h_spin <- -as.numeric(eff$parameter)
            break
          }
        }
      }
    }

    ## Extract spin-form J from congestion (cong) effect (Rb4: factor of 4)
    bd_J_spin <- 0
    if (!is.null(env$theta_cong) &&
        is.numeric(env$theta_cong) &&
        length(env$theta_cong) >= 1) {
      bd_J_spin <- -as.numeric(env$theta_cong[[1]]) / 4
    } else if (!is.null(env$structure_model)) {
      sm_eff <- tryCatch(env$structure_model$dv_bipartite$effects,
                         error = function(e) NULL)
      if (!is.null(sm_eff)) {
        for (eff in sm_eff) {
          enm <- eff$effect
          if (!is.null(enm) && grepl("cong|congest", enm, ignore.case = TRUE)) {
            bd_J_spin <- -as.numeric(eff$parameter) / 4
            break
          }
        }
      }
    }

    ## Empirical m_star from bipartite matrix
    if (is.null(m_star)) {
      bi <- tryCatch(env$bipartite_matrix, error = function(e) NULL)
      if (is.null(bi) || !is.matrix(bi)) {
        warning("saomnk_social_multiplier: env$bipartite_matrix unavailable; ",
                "using m_star = 0")
        m_star <- 0
      } else {
        m_star <- 2 * mean(rowMeans(bi)) - 1
      }
    }

    J_use <- bd_J_spin
    h_use <- bd_h_spin

  } else {
    ## ---- Path B: explicit (beta, J, h) ------------------------------------ ##
    stopifnot(is.numeric(beta), length(beta) == 1, beta >= 0)
    stopifnot(is.numeric(J),    length(J)    == 1)
    stopifnot(is.numeric(h),    length(h)    == 1)

    if (is.null(m_star)) {
      m_star <- bd_self_consistency(beta = beta, J = J, h = h,
                                    all_roots = FALSE)
    }
    J_use <- J
    h_use <- h
  }

  denom <- 1 - beta * J_use * (1 - m_star^2)
  if (abs(denom) < .Machine$double.eps^0.5) {
    multiplier <- sign(denom) * 1e12
    if (multiplier == 0) multiplier <- 1e12
    warning("saomnk_social_multiplier: denominator near zero; multiplier ",
            "diverges (returning sign * 1e12). The system is at the ",
            "Curie-Weiss critical point.")
  } else {
    multiplier <- 1 / denom
  }

  list(
    multiplier              = multiplier,
    m_star                  = m_star,
    bd_J_spin               = J_use,
    bd_h_spin               = h_use,
    warning_near_threshold  = abs(multiplier) > 10
  )
}


# ---------------------------------------------------------------------------- #
#  4. bd_landau_steepness
# ---------------------------------------------------------------------------- #

#' Landau-Ginzburg Quartic Coefficient and Basin Steepness
#'
#' Computes the Landau-Ginzburg quartic coefficient
#' \deqn{b = \frac{2 \tau (1 + 3 m^{*2})}{(1 - m^{*2})^3}}{
#'        b = 2 * tau * (1 + 3 * m_star^2) / (1 - m_star^2)^3}
#' from the AMR herding-steepness derivation, and the predicted basin
#' steepness exponent
#' \deqn{s = 2 + \frac{b\, \delta_m^{2}}{2(\beta J - 1)}}{
#'        s = 2 + b * delta_m^2 / (2 * beta * J - 2)}
#' for a small fluctuation \eqn{\delta_m} around the equilibrium.
#'
#' Source: AMR_Mathematical_Appendix.Rmd, line 652.
#'
#' Note that for \eqn{|m^*|} close to 1 the formula diverges; the result
#' is capped at \code{1e6} with a warning.  Likewise, when
#' \eqn{\beta J = 1} the system is at the critical point and the
#' steepness expression has a removable singularity (returned as
#' \code{Inf} with a warning).
#'
#' @param beta Numeric inverse-temperature.
#' @param J Numeric spin-form peer-interaction strength.
#' @param m_star Numeric equilibrium magnetization in \eqn{[-1, 1]}.
#' @param tau Numeric coupling-time constant (default \code{1}).
#' @param delta_m Numeric fluctuation amplitude (default \code{0.05}).
#' @return A list with components:
#'   \item{b}{Landau quartic coefficient (numeric, possibly capped).}
#'   \item{steepness}{Predicted basin steepness exponent
#'     \eqn{s} (numeric).}
#'   \item{capped}{Logical, \code{TRUE} if the formula diverged and
#'     \code{b} was capped at \code{1e6}.}
#' @examples
#' bd_landau_steepness(beta = 1, J = 2, m_star = 0.5)
#' bd_landau_steepness(beta = 1, J = 2, m_star = 0.95)  # capped
#' @export
bd_landau_steepness <- function(beta, J, m_star, tau = 1, delta_m = 0.05) {

  stopifnot(is.numeric(beta),    length(beta)    == 1)
  stopifnot(is.numeric(J),       length(J)       == 1)
  stopifnot(is.numeric(m_star),  length(m_star)  == 1)
  stopifnot(is.numeric(tau),     length(tau)     == 1)
  stopifnot(is.numeric(delta_m), length(delta_m) == 1)

  one_minus_m2 <- 1 - m_star^2
  capped <- FALSE

  if (one_minus_m2 <= 0) {
    warning("bd_landau_steepness: |m_star| >= 1; b diverges, capping at 1e6.")
    b <- 1e6
    capped <- TRUE
  } else {
    b <- 2 * tau * (1 + 3 * m_star^2) / one_minus_m2^3
    if (!is.finite(b) || abs(b) > 1e6) {
      warning("bd_landau_steepness: |m_star| close to 1; b magnitude exceeds ",
              "1e6, capping.")
      b <- sign(b) * 1e6
      if (b == 0) b <- 1e6
      capped <- TRUE
    }
  }

  denom <- 2 * beta * J - 2
  if (abs(denom) < .Machine$double.eps^0.5) {
    warning("bd_landau_steepness: 2*beta*J - 2 is near zero (system at ",
            "Curie-Weiss critical point); steepness is infinite.")
    steepness <- Inf
  } else {
    steepness <- 2 + b * delta_m^2 / denom
  }

  list(
    b         = b,
    steepness = steepness,
    capped    = capped
  )
}


# ---------------------------------------------------------------------------- #
#  5. verify_brock_durlauf_reduction
# ---------------------------------------------------------------------------- #

#' Empirically Verify the Brock & Durlauf Mean-Field Reduction
#'
#' For each \eqn{M} in \code{M_seq}, instantiates a SaoMNK environment
#' and runs \code{n_replicates} simulations under congestion-only,
#' density-only structure models (all NK / scope / herding / synergy /
#' epistasis effects zeroed).  The empirical end-state mean adoption
#' is compared to the analytical B&D fixed point predicted by Theorem 5
#' (Part L of the proof table).  Convergence at rate
#' \eqn{O(1 / \sqrt{M})} is the headline empirical signature of the
#' mean-field reduction.
#'
#' The function is robust to absent or failing simulation back-ends:
#' if \code{\link{saomnk_env}}, \code{\link{saomnk_model}}, or
#' \code{\link{saomnk_run}} throw, the offending row's
#' \code{m_b_empirical} (and derived columns) are returned as
#' \code{NA} with a warning identifying the failure.
#'
#' \strong{Important caveat (L16 honesty gap).} Exact \emph{numerical}
#' agreement between the live RSiena simulation and the analytical B&D
#' fixed point is \emph{not} expected and is not what this harness
#' verifies. RSiena's bipartite \code{inPop} statistic is
#' sqrt-transformed (\eqn{s = \sum_j b_{ij} \sqrt{n_j + 1}}) for
#' variance stabilisation, whereas B&D's mean-field uses the linear
#' coupling \eqn{J \cdot m}. The harness applies a one-step sqrt
#' linearisation (\code{sqrt_correction = TRUE}) to bring the two
#' closer, but the residual nonlinearity persists. What the harness
#' \emph{does} verify is: (i) the simulation pipeline runs end-to-end
#' under the Rb1-Rb5 restrictions; (ii) the empirical equilibrium
#' direction matches B&D theory (\eqn{J_b > 0} drives \eqn{m > 0.5},
#' \eqn{J_b < 0} drives \eqn{m < 0.5}); (iii) the empirical
#' equilibrium is stable across simulation seeds. Exact numerical
#' verification of Theorem 5 requires a native \code{congestion}
#' RSiena effect (\eqn{s = \sum_j b_{ij} (n_j / M)} without the sqrt
#' transform); this is on the package roadmap and is documented in
#' \code{inst/proofs/PROOF_TABLE.md} row L16.
#'
#' @param M_seq Integer vector of actor counts (default
#'   \code{c(50, 100, 200, 500)}).
#' @param J_b Numeric B&D peer-interaction strength in adoption form.
#'   Used as the \code{popularity} parameter in
#'   \code{\link{saomnk_model}} (with the sqrt-linearisation rescaling
#'   described under \code{sqrt_correction}). Positive \code{J_b} =
#'   coordination (peer alignment); negative \code{J_b} =
#'   anti-coordination (congestion). \strong{Default \code{0.5}}: this
#'   is the \emph{Option C regime} that keeps \eqn{m^* \approx 0.5}
#'   (where the sqrt linearisation is locally accurate); large
#'   \eqn{|J_b|} pushes the equilibrium far from 0.5 where empirical
#'   and B&D analytical disagree (Option B regime; see L16).
#' @param h_b Numeric B&D private-utility bias.  Used as the
#'   \code{density} parameter. \strong{Default \code{-J_b/2}}: when
#'   \code{J_b > 0} this is the symmetric configuration that gives
#'   \eqn{m_b^* = 0.5} exactly, putting the system inside the Option C
#'   regime by construction.
#' @param beta Numeric inverse-temperature passed to
#'   \code{\link{bd_self_consistency}} (default \code{1}).
#' @param n_components Integer number of components \eqn{N}
#'   (default \code{8}). Must be \eqn{\ge 4} because RSiena's bipartite
#'   \code{sienaDependent} rejects degenerate \eqn{N \le 2} arrays.
#'   Under restriction Rb2(b) (PROOF\_TABLE.md L4) the \eqn{N} components
#'   become independent B&D problems when \code{scope = 0} and
#'   \code{influence_weight = 0}, providing \eqn{N} statistical replicates
#'   per simulated environment for free.
#' @param n_steps Integer ministeps \emph{per actor} per replicate
#'   (default \code{50}). The CTMC mixing time is approximately
#'   independent of \eqn{M} when measured in steps-per-actor, so this
#'   parameter controls convergence quality directly. Recommend
#'   \eqn{\ge 30} for \eqn{|\theta| \le 2}; smaller values risk
#'   pre-asymptotic bias unrelated to the L16 sqrt gap.
#' @param n_replicates Integer number of independent simulation replicates
#'   per \eqn{M} (default \code{5}).
#' @param sqrt_correction Logical. RSiena's \code{inPop} statistic
#'   uses a sqrt transform of column-degree (variance stabiliser), not
#'   the linear mean-field form B&D assumes. When \code{TRUE} (default),
#'   the harness applies the L16 linearisation correction by passing
#'   \code{popularity = J_b / sqrt(2M)} (the rescaling that matches
#'   the slope of \eqn{\sqrt{Mm+1}} at \eqn{m=1/2}); when \code{FALSE},
#'   the raw \code{J_b} is used. The corrected version is locally
#'   accurate near \eqn{m = 1/2} (Option C regime). See
#'   PROOF\_TABLE.md row L16 for full derivation.
#' @param seed Integer random seed (default \code{12345}).
#' @return A \code{data.frame} with columns (Option B = saomnk-inPop
#'   reference; Option C = B&D reference):
#'   \item{M}{Actor count.}
#'   \item{m_b_empirical}{Mean end-state adoption fraction (in [0,1])
#'     averaged across replicates.}
#'   \item{m_BD_analytical}{Analytical B&D fixed-point adoption
#'     fraction (Option C reference). Theorem 5 predicts the empirical
#'     should match this in the Option C regime.}
#'   \item{m_saomnk_analytical}{Analytical SAOM-inPop fixed-point
#'     adoption fraction (Option B reference). The empirical should
#'     match this for any \eqn{(\beta, J_b, h_b)} where the
#'     simulation reaches stationarity.}
#'   \item{abs_error_BD}{\code{abs(m_b_empirical - m_BD_analytical)};
#'     small in Option C regime, can be large outside it.}
#'   \item{abs_error_saomnk}{\code{abs(m_b_empirical -
#'     m_saomnk_analytical)}; small in any regime where the simulation
#'     converged.}
#'   \item{in_BD_regime}{Logical: \code{TRUE} when
#'     \eqn{|m_{BD} - 0.5| < 0.15} so the sqrt linearisation is locally
#'     accurate and B&D theory should match empirically.}
#'   \item{log_M}{\code{log10(M)}.}
#'   \item{log_error_BD}{\code{log10(abs_error_BD)}.}
#'   \item{log_error_saomnk}{\code{log10(abs_error_saomnk)}.}
#'   \item{qualitative_match}{Logical: empirical and B&D analytical
#'     are on the same side of 0.5.}
#' @examples
#' \dontrun{
#'   ## Default (Option C regime): J_b=0.5, h_b=-0.25, m*~=0.5
#'   ## empirical -> B&D fixed point at O(M^-1/2)
#'   verify_brock_durlauf_reduction(M_seq = c(50, 100, 200),
#'                                  n_replicates = 3,
#'                                  n_steps = 500)
#'
#'   ## Outside Option C regime: large h_b drives m far from 0.5
#'   ## empirical follows m_saomnk_analytical (Option B), not B&D
#'   verify_brock_durlauf_reduction(M_seq = c(50, 100),
#'                                  J_b = 2, h_b = -1,
#'                                  n_replicates = 2)
#' }
#' @export
verify_brock_durlauf_reduction <- function(M_seq        = c(50, 100, 200, 500),
                                           J_b          = 0.5,
                                           h_b          = -J_b / 2,
                                           beta         = 1,
                                           n_components = 8,
                                           n_steps      = 50,
                                           n_replicates = 5,
                                           sqrt_correction = TRUE,
                                           seed         = 12345) {

  stopifnot(is.numeric(M_seq), length(M_seq) >= 1, all(M_seq >= 2))
  stopifnot(is.numeric(J_b),  length(J_b)  == 1)
  stopifnot(is.numeric(h_b),  length(h_b)  == 1)
  stopifnot(is.numeric(beta), length(beta) == 1, beta >= 0)
  stopifnot(is.numeric(n_components), length(n_components) == 1, n_components >= 4)
  stopifnot(is.numeric(n_steps), length(n_steps) == 1, n_steps >= 1)
  stopifnot(is.numeric(n_replicates), length(n_replicates) == 1, n_replicates >= 1)
  stopifnot(is.logical(sqrt_correction), length(sqrt_correction) == 1)

  ## ---- Analytical B&D fixed point (spin -> adoption) -------------------- ##
  ## Sign convention: in the SaoMNK API, "density" multiplies the
  ## adoption-form network density (positive = encourage adoption).  In
  ## B&D spin form, h is the bias toward spin +1.  Recoding
  ##   p = (m + 1) / 2  =>  density (adoption form) = h_b
  ## We pass into bd_self_consistency the spin-form parameters; per
  ## Rb4 of the proof table the spin-form J is -J_b/4 if J_b is the
  ## adoption-form congestion coefficient, but the user's J_b argument
  ## is *already* in B&D spin form (as documented), so we feed it
  ## directly with sign chosen so that positive J_b = coordination.
  ## Convert {0,1}-form (h_b, J_b) -> spin-form (h_spin, J_spin) per Rb4
  ## (PROOF_TABLE.md L6): J_spin = J_b/4, h_spin = h_b/2 + J_b/4.  This is
  ## essential because bd_self_consistency operates on the spin-form
  ## tanh equation, while the harness's user-facing arguments are in
  ## adoption form (matching the saomnk_model() API where `density = h_b`
  ## and `popularity = J_b` parametrise actor utility on b in {0,1}).
  J_spin <- J_b / 4
  h_spin <- h_b / 2 + J_b / 4
  m_star_spin <- tryCatch(
    bd_self_consistency(beta = beta, J = J_spin, h = h_spin, all_roots = FALSE),
    error = function(e) {
      warning("verify_brock_durlauf_reduction: B&D analytical fixed point ",
              "could not be computed: ", conditionMessage(e))
      NA_real_
    }
  )
  m_BD_analytical <- if (is.finite(m_star_spin)) .bd_m_to_p(m_star_spin) else NA_real_

  ## Option C regime check: B&D linearisation is locally accurate when
  ## the equilibrium magnetisation sits near 0.5.  Empirical match to
  ## B&D should be expected only inside this regime; outside, compare
  ## to the saomnk-inPop analytical instead (Option B reference).
  in_BD_regime <- if (is.finite(m_BD_analytical)) {
    abs(m_BD_analytical - 0.5) < 0.15
  } else {
    NA
  }

  ## ---- Per-M empirical loop --------------------------------------------- ##
  rows <- vector("list", length(M_seq))

  for (k in seq_along(M_seq)) {
    M_k <- as.integer(M_seq[k])
    rep_means <- numeric(n_replicates)
    rep_ok    <- logical(n_replicates)

    for (r in seq_len(n_replicates)) {
      this_seed <- as.integer(seed + 1000L * k + r)

      sim_p <- tryCatch({
        env_obj <- saomnk_env(M = M_k, N = as.integer(n_components),
                              density = 0.5, seed = this_seed)
        ## All NK / scope / herding / synergy effects zeroed; only
        ## density (h_b) and a popularity-as-congestion proxy (-J_b)
        ## remain.  Notes:
        ##  * The SaoMNK API does not expose a top-level "congestion"
        ##    RSiena effect; popularity (inPop) is the closest mean-field
        ##    coordination channel.  See L16 for full discussion.
        ##  * RSiena's inPop is sqrt-transformed (variance stabiliser),
        ##    so when `sqrt_correction = TRUE` we rescale the coefficient
        ##    by 1/sqrt(M/2) to recover the linear mean-field form
        ##    (one-step linearisation per L16).
        ##  * `influence_matrix = NULL` causes RSiena to drop the
        ##    density effect ("Effect not found"); we therefore pass a
        ##    minimal real matrix with `influence_weight = 0`.
        ## Sign convention: positive J_b = coordination (peer alignment) =
        ## positive `popularity` coefficient on inPop. Negative J_b =
        ## anti-coordination (congestion).
        ##
        ## Rescaling derivation (L16): the inPop statistic
        ## sqrt(M*m + 1) Taylor-expanded around m=1/2 has slope
        ## sqrt(M/2), giving sqrt(M*m + 1) ~= sqrt(2M)*m at first order.
        ## To match B&D's linear J_b*m form near m=1/2, we therefore
        ## rescale pop_coef = J_b / sqrt(2M).  This factor must be
        ## reused inside saomnk_inpop_self_consistency() below so the
        ## Option B reference uses the SAME theta the simulation sees.
        pop_coef <- if (sqrt_correction) J_b / sqrt(max(2, 2 * M_k)) else J_b
        N_int    <- as.integer(n_components)
        epi_mat  <- saomnk_block_diagonal(N_int, max(2L, N_int %/% 2L))
        model <- saomnk_model(density    = h_b,
                              popularity = pop_coef,
                              scope      = 0,
                              influence_matrix = epi_mat,
                              influence_weight = 0)
        ## n_steps is interpreted as steps_per_actor (so mixing time is
        ## independent of M).  Each actor revises n_steps times,
        ## giving total CTMC ministeps = M * n_steps.  For meaningful
        ## convergence to the Gibbs stationary distribution, we
        ## recommend n_steps >= 30 with theta magnitudes <= 2; smaller
        ## values risk pre-asymptotic bias unrelated to the L16 gap.
        saomnk_run(env_obj, model,
                   steps_per_actor = as.integer(n_steps),
                   seed            = this_seed,
                   verbose         = FALSE)
        bi <- env_obj$bipartite_matrix
        if (is.null(bi) || !is.matrix(bi)) {
          stop("env$bipartite_matrix not populated after run")
        }
        mean(bi)  # average over all M*N cells = grand mean adoption
      },
      error = function(e) {
        warning("verify_brock_durlauf_reduction: simulation failed for ",
                "M = ", M_k, ", replicate ", r, ": ",
                conditionMessage(e))
        NA_real_
      })

      rep_means[r] <- sim_p
      rep_ok[r]    <- is.finite(sim_p)
    }

    if (any(rep_ok)) {
      m_b_emp <- mean(rep_means[rep_ok])
    } else {
      m_b_emp <- NA_real_
    }

    ## ---- Option B reference: saomnk-inPop fixed point at THIS M ------- ##
    ## This is the equilibrium that the live RSiena simulation actually
    ## obeys (matching the simulation's own functional form), as opposed
    ## to m_BD_analytical which holds only in the Option C regime.
    pop_coef_M <- if (sqrt_correction) J_b / sqrt(max(2, 2 * M_k)) else J_b
    m_saomnk_analytical <- tryCatch(
      saomnk_inpop_self_consistency(beta        = beta,
                                    theta_inPop = pop_coef_M,
                                    h_b         = h_b,
                                    M           = M_k),
      error = function(e) NA_real_
    )

    abs_err_BD <- if (is.finite(m_b_emp) && is.finite(m_BD_analytical)) {
      abs(m_b_emp - m_BD_analytical)
    } else NA_real_
    abs_err_saomnk <- if (is.finite(m_b_emp) && is.finite(m_saomnk_analytical)) {
      abs(m_b_emp - m_saomnk_analytical)
    } else NA_real_
    log_err_BD <- if (is.finite(abs_err_BD) && abs_err_BD > 0) log10(abs_err_BD) else NA_real_
    log_err_saomnk <- if (is.finite(abs_err_saomnk) && abs_err_saomnk > 0) log10(abs_err_saomnk) else NA_real_

    qualitative_match <- if (is.finite(m_b_emp) && is.finite(m_BD_analytical)) {
      ## Direction check: same side of 0.5 baseline (coordination vs.
      ## anti-coordination).  Use 0.5 as origin since adoption form.
      ## sign(0) = 0 is treated as match for either side.
      sign(m_b_emp - 0.5) == sign(m_BD_analytical - 0.5) ||
        abs(m_BD_analytical - 0.5) < 0.01
    } else {
      NA
    }

    rows[[k]] <- data.frame(
      M                   = M_k,
      m_b_empirical       = m_b_emp,
      m_BD_analytical     = m_BD_analytical,
      m_saomnk_analytical = m_saomnk_analytical,
      abs_error_BD        = abs_err_BD,
      abs_error_saomnk    = abs_err_saomnk,
      in_BD_regime        = in_BD_regime,
      log_M               = log10(M_k),
      log_error_BD        = log_err_BD,
      log_error_saomnk    = log_err_saomnk,
      qualitative_match   = qualitative_match,
      stringsAsFactors    = FALSE
    )
  }

  do.call(rbind, rows)
}
