#' @title Brock--Durlauf Mean-Field Reduction Utilities
#' @description Functions operationalizing Property 5 of the SAOM-NK proof
#'   table (Part L), which establishes that SAOM-NK reduces to the
#'   Brock & Durlauf (2001, RES) binary discrete-choice-with-social-
#'   interactions model in the M -> infinity, congestion-only, mean-field
#'   limit.
#'
#'   The five utility functions here (i) solve the canonical B&D
#'   self-consistency equation for the equilibrium magnetization,
#'   (ii) count equilibria (regime classification), (iii) compute the
#'   B&D social multiplier from either explicit parameters or a fitted
#'   SAOM-NK environment, (iv) report the Landau-Ginzburg quartic
#'   coefficient and basin steepness from the herding-steepness
#'   derivation, and (v) verify the M -> infinity reduction empirically
#'   by comparing simulated mean adoption to the analytical fixed point.
#'
#'   See \code{inst/proofs/PROOF_TABLE.md} (Part L, Property 5) for derivations.
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

#' Solve the SAOM-NK inPop Self-Consistency Equation (Operational Fixed Point)
#'
#' Solves the mean-field fixed point of the live RSiena bipartite SAOM with
#' only the \code{density} and \code{inPop} effects active. Two facts fix
#' its form (re-derived 2026-10-07 for searchnet 0.11.0, which simulates
#' genuine state-carrying paths):
#'
#' \enumerate{
#'   \item On a two-mode dependent variable RSiena 1.5.0's \code{inPop} is
#'     linear, \eqn{s_i = \sum_j x_{ij} x_{+j}} (the square-root form is the
#'     separate effect \code{inPopSqrt}). Adding tie \eqn{(i,j)} changes the
#'     objective by \eqn{\Delta = h_b + \theta(n_{-i,j} + 1)}, where
#'     \eqn{n_{-i,j}} counts the other holders of component \eqn{j}; in mean
#'     field \eqn{n_{-i,j} = (M-1)m}.
#'   \item In a SAOM ministep the actor chooses among \eqn{N} toggles and no
#'     change. For one actor with a common \eqn{\Delta}, the number of held
#'     ties \eqn{k} is a birth-death chain whose stationary law is
#'     \deqn{\pi_k \propto \binom{N}{k} e^{2\Delta k}
#'       \bigl(1 + (N-k)e^{\Delta} + k e^{-\Delta}\bigr),}
#'     so the per-tie adoption probability is \eqn{F_N(\Delta) = E[k]/N}.
#'     For \eqn{N = 1} this is exactly the binary logit \eqn{\sigma(\Delta)};
#'     as \eqn{N \to \infty} it tends to \eqn{\sigma(2\Delta)}; near
#'     \eqn{m = 1/2} it is \eqn{\sigma(\kappa_N \Delta)} to first order with
#'     \eqn{\kappa_N = 2N/(N+1)}.
#' }
#'
#' The fixed point solved is therefore
#' \deqn{m = F_N\bigl(\beta(h_b + \theta_{\mathrm{inPop}}(1 + (M-1)m))\bigr).}
#' Checked against direct simulation of the ministep chain and against
#' \code{saomnk_run()} at \eqn{M = 12}, \eqn{N = 6}: the map gives 0.998 and
#' 0.859 where the simulations give 0.99 to 1.00 and 0.84 to 0.86.
#'
#' Before 0.11.0 this function solved
#' \eqn{m = \sigma(\beta(h_b + \theta\sqrt{Mm + 1}))}. That form was
#' identified on the replayed chain, whose terminal state was a draw near
#' the initial density, and it reads \code{inPop} as \code{inPopSqrt}.
#'
#' @param beta Numeric inverse-temperature (scales the objective).
#' @param theta_inPop Numeric SAOM \code{inPop} coefficient (the
#'   coefficient passed as \code{popularity} in
#'   \code{\link{saomnk_model}}).
#' @param h_b Numeric SAOM \code{density} coefficient.
#' @param M Integer actor count.
#' @param N Integer component count (the size of each actor's choice set).
#'   \code{NULL} uses the large-\eqn{N} limit \eqn{\sigma(2\Delta)}; pass the
#'   environment's \eqn{N} for the finite-choice-set law.
#' @param x0 Numeric initial guess in \eqn{[0, 1]}; default 0.5.
#' @param tol Numeric convergence tolerance; default \code{1e-10}.
#' @param max_iter Integer maximum iterations; default \code{1000}.
#' @return Adoption-form fixed point \eqn{m^* \in [0, 1]}. With positive
#'   \code{theta_inPop} the map is increasing and may have several fixed
#'   points; iteration from \code{x0} returns the one it reaches. With
#'   negative \code{theta_inPop} the map is decreasing, the fixed point is
#'   unique, and it is found by root bracketing if iteration oscillates.
#' @references
#'   See \code{PROOF_TABLE.md} row L16 for the change statistic of the
#'   two-mode \code{inPop} effect.
#' @seealso \code{\link{bd_self_consistency}} for the B&D binary-logit
#'   fixed point; \code{\link{verify_brock_durlauf_reduction}}
#'   for the harness that uses both as comparison targets.
#' @examples
#' ## Field placing the fixed point at 1/2: Delta(1/2) = 0
#' M <- 12; theta <- 0.15
#' saomnk_inpop_self_consistency(beta = 1, theta_inPop = theta,
#'                               h_b = -theta * (1 + (M - 1) / 2),
#'                               M = M, N = 6)
#' ## Coordination regime: the fixed point is near full adoption
#' saomnk_inpop_self_consistency(beta = 1, theta_inPop = 0.6, h_b = -1,
#'                               M = 12, N = 6)
#' @export
saomnk_inpop_self_consistency <- function(beta, theta_inPop, h_b, M,
                                          N = NULL,
                                          x0 = 0.5, tol = 1e-10,
                                          max_iter = 1000L) {
  stopifnot(is.numeric(beta), length(beta) == 1, beta >= 0,
            is.numeric(theta_inPop), length(theta_inPop) == 1,
            is.numeric(h_b), length(h_b) == 1,
            is.numeric(M), length(M) == 1, M >= 2,
            is.null(N) || (is.numeric(N) && length(N) == 1 && N >= 1),
            is.numeric(x0), length(x0) == 1, x0 >= 0, x0 <= 1,
            is.numeric(tol), length(tol) == 1, tol > 0,
            is.numeric(max_iter), length(max_iter) == 1, max_iter >= 1)

  M_num <- as.numeric(M)
  F_map <- function(m) {
    delta <- beta * (h_b + theta_inPop * (1 + (M_num - 1) * m))
    .saomnk_ministep_adoption(delta, N)
  }

  m <- x0
  for (iter in seq_len(max_iter)) {
    m_new <- F_map(m)
    if (abs(m_new - m) < tol) {
      return(m_new)
    }
    m <- m_new
  }
  ## A decreasing map (negative theta) can cycle; its fixed point is unique.
  g <- function(m) m - F_map(m)
  if (g(0) * g(1) < 0) {
    return(stats::uniroot(g, c(0, 1), tol = tol)$root)
  }
  warning("saomnk_inpop_self_consistency: did not converge within ",
          max_iter, " iterations (final |delta| = ",
          format(abs(m_new - m), digits = 3), ")")
  m
}

## Per-tie stationary adoption probability of one actor's row under the SAOM
## ministep (choice among N toggles and no change) when every tie has the same
## objective change `delta` for adding it. N = NULL is the large-N limit.
.saomnk_ministep_adoption <- function(delta, N = NULL) {
  if (is.null(N)) return(1 / (1 + exp(-2 * delta)))
  k <- 0:N
  ## log(1 + (N-k) e^delta + k e^-delta), computed without overflow
  lz <- vapply(k, function(kk) {
    a <- c(0, if (N - kk > 0) log(N - kk) + delta, if (kk > 0) log(kk) - delta)
    mx <- max(a); mx + log(sum(exp(a - mx)))
  }, numeric(1))
  lw <- lchoose(N, k) + 2 * delta * k + lz
  w <- exp(lw - max(lw))
  sum(k * w) / (N * sum(w))
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
#'   \item Pass a fitted SAOM-NK environment via \code{env}.  The
#'         B&D-equivalent \eqn{J} and \eqn{h} are extracted from the
#'         environment's structure-model effects table; the
#'         empirical \eqn{m^*} is read off from the bipartite matrix
#'         via \eqn{m^* = 2 \bar{B} - 1}.
#' }
#' The spin-form recoding contributes a factor of 4 to \eqn{J}
#' (Rb4 of the proof table) which is applied automatically in the
#' env-based path.
#'
#' @param env Optional SAOM-NK environment (\code{SaomNkRSienaBiEnv}) from
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
#' from the herding-steepness derivation, and the predicted basin
#' steepness exponent
#' \deqn{s = 2 + \frac{b\, \delta_m^{2}}{2(\beta J - 1)}}{
#'        s = 2 + b * delta_m^2 / (2 * beta * J - 2)}
#' for a small fluctuation \eqn{\delta_m} around the equilibrium.
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
#' For each \eqn{M} in \code{M_seq}, instantiates a SAOM-NK environment
#' and runs \code{n_replicates} simulations under congestion-only,
#' density-only structure models (all NK / scope / herding / synergy /
#' epistasis effects zeroed).  The empirical end-state mean adoption
#' is compared to the analytical B&D fixed point predicted by Property 5
#' (Part L of the proof table).  Convergence at rate
#' \eqn{O(1 / \sqrt{M})} is the headline empirical signature of the
#' mean-field reduction.
#'
#' The empirical end-state mean adoption is compared to \emph{two}
#' analytical references:
#'
#' \enumerate{
#'   \item \strong{Option C reference (B&D, primary):} the canonical
#'     Brock and Durlauf (2001) fixed point
#'     \eqn{m = \sigma(\beta(h + J m))} computed via
#'     \code{\link{bd_self_consistency}}.  This is what Property 5 predicts
#'     in the regime where the first-order rescaling (see
#'     \code{rescale}) is accurate (\eqn{m^* \approx 0.5}, achieved by setting
#'     \code{h_b = -J_b / 2} with modest \code{J_b}).
#'   \item \strong{Option B reference (saomnk-inPop, secondary):} the
#'     mean-field fixed point of the live RSiena pipeline, computed via
#'     \code{\link{saomnk_inpop_self_consistency}} with the linear
#'     two-mode \code{inPop} statistic and the ministep choice law over
#'     \eqn{N} toggles.  This is what the
#'     simulation should match in \emph{any} regime (regardless of where
#'     \eqn{m^*} sits), and is the correct comparison target outside the
#'     Option C regime.
#' }
#'
#' Convergence of \code{m_b_empirical} to \code{m_BD_analytical} (when
#' \code{in_BD_regime = TRUE}) and to \code{m_saomnk_analytical} (always)
#' is what the harness reports.
#'
#' Failed simulation replicates (an error in \code{\link{saomnk_env}},
#' \code{\link{saomnk_model}}, or \code{\link{saomnk_run}}) are excluded
#' from \code{m_b_empirical}, counted in \code{n_ok}, listed in
#' \code{attr(result, "failures")}, and reported by one warning stating
#' how many of the replicates failed. If every replicate fails the
#' function stops. A row is \code{NA} only when all of its replicates
#' failed.
#'
#' \strong{Important caveat (L16).} RSiena 1.5.0's two-mode \code{inPop}
#' statistic is linear (\eqn{s = \sum_j b_{ij} x_{+j}}), so the marginal
#' utility matches B&D's linear coupling with
#' \eqn{\theta_{\mathrm{inPop}} = J_b/(M-1)}. The choice rule does not:
#' a SAOM ministep chooses among \eqn{N} toggles and no change, and the
#' per-tie adoption law is the binary logit only for \eqn{N = 1}. Near
#' \eqn{m = 1/2} it is \eqn{\sigma(\kappa_N \Delta)} with
#' \eqn{\kappa_N = 2N/(N+1)}. With \code{rescale = TRUE} (default) the
#' harness divides both coefficients by \eqn{\kappa_N} and removes the
#' own-tie constant from the field, so the two agree to first order near
#' \eqn{m = 1/2}; away from it they differ. What the harness
#' \emph{does} verify is: (i) the simulation pipeline runs end-to-end
#' under the Rb1-Rb5 restrictions; (ii) the empirical equilibrium
#' direction matches B&D theory (\eqn{J_b > 0} drives \eqn{m > 0.5},
#' \eqn{J_b < 0} drives \eqn{m < 0.5}); (iii) the empirical
#' equilibrium is stable across simulation seeds. Exact numerical
#' verification of Property 5 would need a single binary choice per
#' actor (\eqn{N = 1}), which RSiena's bipartite dependent variable
#' does not admit; see \code{inst/proofs/PROOF_TABLE.md} row L16.
#'
#' @param M_seq Integer vector of actor counts (default
#'   \code{c(50, 100, 200, 500)}).
#' @param J_b Numeric B&D peer-interaction strength in adoption form.
#'   Used as the \code{popularity} parameter in
#'   \code{\link{saomnk_model}} (with the rescaling described under
#'   \code{rescale}). Positive \code{J_b} =
#'   coordination (peer alignment); negative \code{J_b} =
#'   anti-coordination (congestion). \strong{Default \code{0.5}}: this
#'   is the \emph{Option C regime} that keeps \eqn{m^* \approx 0.5}
#'   (where the first-order rescaling is accurate); large
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
#'   Under restriction Rb2(b) (\code{PROOF_TABLE.md} L4) the \eqn{N} components
#'   become independent B&D problems when \code{scope = 0} and
#'   \code{influence_weight = 0}, providing \eqn{N} statistical replicates
#'   per simulated environment for free.
#' @param n_steps Expected decision opportunities \emph{per actor} per
#'   replicate (default \code{50}); passed to \code{saomnk_run()} as
#'   \code{steps_per_actor}, which since 0.11.0 is a basic rate summed over
#'   one unit of time, not a fixed ministep count. The CTMC mixing time is approximately
#'   independent of \eqn{M} when measured in steps-per-actor, so this
#'   parameter controls convergence quality directly. Recommend
#'   \eqn{\ge 30} for \eqn{|\theta| \le 2}; smaller values risk
#'   pre-asymptotic bias.
#' @param n_replicates Integer number of independent simulation replicates
#'   per \eqn{M} (default \code{5}).
#' @param rescale Logical. When \code{TRUE} (default) the simulation uses
#'   \code{popularity} \eqn{\theta = J_b / (\kappa_N (M-1))} and
#'   \code{density} \eqn{h_b/\kappa_N - \theta}, with
#'   \eqn{\kappa_N = 2N/(N+1)}, which matches the B&D map
#'   \eqn{m = \sigma(\beta(h_b + J_b m))} to first order near
#'   \eqn{m = 1/2}. When \code{FALSE}, \code{J_b} and \code{h_b} are
#'   passed unchanged.
#' @param sqrt_correction Deprecated alias of \code{rescale}. Before 0.11.0
#'   it applied \eqn{J_b/\sqrt{2M}}, a rescaling derived for
#'   \code{inPopSqrt}; supplying it now sets \code{rescale} with a
#'   warning.
#' @param seed Integer random seed (default \code{12345}).
#' @return A \code{data.frame} with columns (Option B = saomnk-inPop
#'   reference; Option C = B&D reference):
#'   \item{M}{Actor count.}
#'   \item{n_ok}{Number of replicates that ran; \code{m_b_empirical}
#'     averages these.}
#'   \item{m_b_empirical}{Mean end-state adoption fraction (in
#'     \eqn{[0,1]}) averaged across replicates.}
#'   \item{m_BD_analytical}{Analytical B&D fixed-point adoption
#'     fraction (Option C reference). Property 5 predicts the empirical
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
#'     \eqn{|m_{BD} - 0.5| < 0.15}, where the first-order rescaling is
#'     accurate and B&D theory should match empirically.}
#'   \item{log_M}{\code{log10(M)}.}
#'   \item{log_error_BD}{\code{log10(abs_error_BD)}.}
#'   \item{log_error_saomnk}{\code{log10(abs_error_saomnk)}.}
#'   \item{qualitative_match}{Logical: empirical and B&D analytical
#'     are on the same side of 0.5.}
#' @references
#'   Brock, W. A. & Durlauf, S. N. (2001). Discrete choice with social
#'   interactions. \emph{Review of Economic Studies}, 68(2), 235--260.
#'
#'   Ellis, R. S. (1985). \emph{Entropy, Large Deviations, and Statistical
#'   Mechanics}. Springer.
#' @seealso \code{\link{bd_self_consistency}} (B&D analytical),
#'   \code{\link{saomnk_inpop_self_consistency}} (saomnk-inPop analytical),
#'   \code{\link{bd_equilibrium_count}},
#'   \code{\link{saomnk_social_multiplier}},
#'   \code{\link{bd_landau_steepness}};
#'   \code{vignette("saomnk-brock-durlauf")} for the full Property 5
#'   walkthrough including the Option B / Option C tradeoff.
#' @keywords utilities
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
                                           rescale      = TRUE,
                                           seed         = 12345,
                                           sqrt_correction = NULL) {

  stopifnot(is.numeric(M_seq), length(M_seq) >= 1, all(M_seq >= 2))
  stopifnot(is.numeric(J_b),  length(J_b)  == 1)
  stopifnot(is.numeric(h_b),  length(h_b)  == 1)
  stopifnot(is.numeric(beta), length(beta) == 1, beta >= 0)
  stopifnot(is.numeric(n_components), length(n_components) == 1, n_components >= 4)
  stopifnot(is.numeric(n_steps), length(n_steps) == 1, n_steps >= 1)
  stopifnot(is.numeric(n_replicates), length(n_replicates) == 1, n_replicates >= 1)
  if (!is.null(sqrt_correction)) {
    warning("`sqrt_correction` is deprecated as of searchnet 0.11.0; use ",
            "`rescale`. The square-root rescaling it named was derived for ",
            "inPopSqrt, not the linear two-mode inPop.", call. = FALSE)
    rescale <- sqrt_correction
  }
  stopifnot(is.logical(rescale), length(rescale) == 1)
  ## First-order slope of the ministep adoption law at m = 1/2
  ## (see saomnk_inpop_self_consistency()).
  kappa_N <- 2 * n_components / (n_components + 1)

  ## ---- Analytical B&D fixed point (spin -> adoption) -------------------- ##
  ## Sign convention: in the SAOM-NK API, "density" multiplies the
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
  ## Failed replicates are excluded from m_b_empirical and recorded in
  ## attr(out, "failures"); n_ok says how many replicates each row averages.
  ## One warning with counts replaces the per-replicate warnings, and a run
  ## in which every replicate fails stops.
  rows <- vector("list", length(M_seq))
  failures <- data.frame(M = integer(0), replicate = integer(0),
                         message = character(0), stringsAsFactors = FALSE)
  saomnk_ref_failures <- character(0)

  for (k in seq_along(M_seq)) {
    M_k <- as.integer(M_seq[k])
    rep_means <- numeric(n_replicates)
    rep_ok    <- logical(n_replicates)

    for (r in seq_len(n_replicates)) {
      ## Separate streams for the initial draw and the dynamics; one seed fed
      ## to both made them the same stream.
      init_seed <- .searchnet_seed(seed, "brock_durlauf:init", k, r)
      this_seed <- .searchnet_seed(seed, "brock_durlauf:run", k, r)

      sim_p <- tryCatch({
        env_obj <- saomnk_env(M = M_k, N = as.integer(n_components),
                              density = 0.5, seed = init_seed)
        ## All NK / scope / herding / synergy effects zeroed; only
        ## density and inPop remain. The SAOM-NK API exposes no top-level
        ## "congestion" RSiena effect, so inPop is the coordination channel
        ## (L16). `influence_matrix = NULL` makes RSiena drop the density
        ## effect ("Effect not found"), so a minimal real matrix is passed
        ## with `influence_weight = 0`.
        ##
        ## Rescaling (re-derived 2026-10-07). Two-mode inPop is linear:
        ## adding tie (i,j) changes the objective by
        ## h + theta * (n_{-i,j} + 1). The ministep adoption law near
        ## m = 1/2 is sigmoid(kappa_N * Delta), kappa_N = 2N/(N+1). Matching
        ## sigmoid(beta * (h_b + J_b m)) to first order gives
        ## theta = J_b / (kappa_N (M-1)) and h = h_b / kappa_N - theta.
        ## The Option B reference below uses the same coefficients.
        pop_coef  <- if (rescale) J_b / (kappa_N * (M_k - 1)) else J_b
        dens_coef <- if (rescale) h_b / kappa_N - pop_coef else h_b
        N_int    <- as.integer(n_components)
        infl_mat  <- saomnk_block_diagonal(N_int, max(2L, N_int %/% 2L))
        model <- saomnk_model(density    = dens_coef,
                              popularity = pop_coef,
                              scope      = 0,
                              influence_matrix = infl_mat,
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
        failures[nrow(failures) + 1L, ] <<- list(M_k, as.integer(r),
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
    pop_coef_M  <- if (rescale) J_b / (kappa_N * (M_k - 1)) else J_b
    dens_coef_M <- if (rescale) h_b / kappa_N - pop_coef_M else h_b
    m_saomnk_analytical <- tryCatch(
      saomnk_inpop_self_consistency(beta        = beta,
                                    theta_inPop = pop_coef_M,
                                    h_b         = dens_coef_M,
                                    M           = M_k,
                                    N           = n_components),
      error = function(e) {
        saomnk_ref_failures <<- c(saomnk_ref_failures,
                                  sprintf("M = %d: %s", M_k, conditionMessage(e)))
        NA_real_
      }
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
      n_ok                = sum(rep_ok),
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

  n_total <- length(M_seq) * n_replicates
  if (nrow(failures) == n_total) {
    stop(sprintf(paste0("verify_brock_durlauf_reduction: all %d simulation replicates ",
                        "failed; there is no empirical value to compare. First failure ",
                        "(M = %d, replicate %d): %s"),
                 n_total, failures$M[1], failures$replicate[1], failures$message[1]),
         call. = FALSE)
  }
  if (nrow(failures) > 0) {
    warning(sprintf(paste0("verify_brock_durlauf_reduction: %d of %d simulation replicates ",
                           "failed and are excluded from m_b_empirical (see the n_ok ",
                           "column and attr(, \"failures\")). First failure (M = %d, ",
                           "replicate %d): %s"),
                    nrow(failures), n_total, failures$M[1], failures$replicate[1],
                    failures$message[1]),
            call. = FALSE)
  }
  if (length(saomnk_ref_failures) > 0) {
    warning(sprintf(paste0("verify_brock_durlauf_reduction: the saomnk-inPop analytical ",
                           "reference could not be computed for %d of %d M values ",
                           "(m_saomnk_analytical and its errors are NA there): %s"),
                    length(saomnk_ref_failures), length(M_seq),
                    paste(saomnk_ref_failures, collapse = "; ")),
            call. = FALSE)
  }

  out <- do.call(rbind, rows)
  attr(out, "failures") <- failures
  attr(out, "provenance") <- .searchnet_provenance(seed = seed, call = match.call())
  out
}
