% Generated manually for R CMD check compliance
\name{verify_brock_durlauf_reduction}
\alias{verify_brock_durlauf_reduction}
\title{Empirically Verify the Brock & Durlauf Mean-Field Reduction (Dual Mode)}
\description{
For each \eqn{M} in \code{M_seq}, instantiates a SaoMNK environment and
runs \code{n_replicates} simulations under density-only +
inPop-only structure models (all NK / scope / herding / synergy /
epistasis effects zeroed).  The empirical end-state mean adoption is
compared to \emph{two} analytical references:

\enumerate{
  \item \strong{Option C reference (B&D, primary):} the canonical
    Brock and Durlauf (2001) fixed point
    \eqn{m = \sigma(\beta(h + J m))} computed via
    \code{\link{bd_self_consistency}}.  This is what Theorem 5 predicts
    in the regime where the sqrt linearisation of inPop is locally
    accurate (\eqn{m^* \approx 0.5}, achieved by setting
    \code{h_b = -J_b / 2} with modest \code{J_b}).
  \item \strong{Option B reference (saomnk-inPop, secondary):} the
    actual fixed point of the live RSiena pipeline,
    \eqn{m = \sigma(\beta(h + \theta\sqrt{Mm + 1}))} computed via
    \code{\link{saomnk_inpop_self_consistency}}.  This is what the
    simulation should match in \emph{any} regime (regardless of where
    \eqn{m^*} sits), and is the correct comparison target outside the
    Option C regime.
}

Convergence at rate \eqn{O(1/\sqrt{M})} of \code{m_b_empirical} to
\code{m_BD_analytical} (when \code{in_BD_regime = TRUE}) and to
\code{m_saomnk_analytical} (always) is the headline empirical signature
of the mean-field reduction.

The function is robust to absent or failing simulation back-ends: if
\code{\link{saomnk_env}}, \code{\link{saomnk_model}}, or
\code{\link{saomnk_run}} throw, the offending row's
\code{m_b_empirical} (and derived columns) are returned as \code{NA}
with a warning identifying the failure.

Important caveat: exact numerical agreement between the live RSiena
simulation and the analytical B&D fixed point is \emph{not} expected
outside the Option C regime, because RSiena's bipartite \code{inPop}
statistic is sqrt-transformed (variance stabiliser) while B&D's
mean-field uses linear coupling.  The harness applies a one-step
sqrt linearisation (\code{sqrt_correction = TRUE}) but residual
non-linearity persists.  Full discussion in
\code{inst/proofs/PROOF_TABLE.md} row L16.
}
\usage{
verify_brock_durlauf_reduction(M_seq = c(50, 100, 200, 500),
                               J_b = 0.5, h_b = -J_b / 2, beta = 1,
                               n_components = 8, n_steps = 2000,
                               n_replicates = 5,
                               sqrt_correction = TRUE, seed = 12345)
}
\arguments{
  \item{M_seq}{Integer vector of actor counts (default
    \code{c(50, 100, 200, 500)}).}
  \item{J_b}{Numeric B&D peer-interaction strength in adoption form.
    Used as the \code{popularity} parameter (with sqrt-linearisation
    rescaling per \code{sqrt_correction}). Positive \code{J_b} =
    coordination; negative \code{J_b} = anti-coordination.  Default
    \code{0.5} (Option C regime).}
  \item{h_b}{Numeric B&D private-utility bias used as the
    \code{density} parameter.  Default \code{-J_b/2}: the symmetric
    configuration that gives \eqn{m^* = 0.5} exactly, putting the
    system inside the Option C regime by construction.}
  \item{beta}{Numeric inverse-temperature (default \code{1}).}
  \item{n_components}{Integer number of components \eqn{N} (default
    \code{8}). Must be \eqn{\ge 4} because RSiena's bipartite
    \code{sienaDependent} rejects degenerate \eqn{N \le 2} arrays.}
  \item{n_steps}{Integer total simulation steps per replicate (default
    \code{2000}).}
  \item{n_replicates}{Integer number of independent simulation
    replicates per \eqn{M} (default \code{5}).}
  \item{sqrt_correction}{Logical.  When \code{TRUE} (default), applies
    L16 sqrt linearisation by passing
    \code{popularity = J_b / sqrt(M/2)}.  When \code{FALSE}, raw
    \code{J_b} is used.}
  \item{seed}{Integer random seed (default \code{12345}).}
}
\value{
A \code{data.frame} with columns:
  \item{M}{Actor count.}
  \item{m_b_empirical}{Mean end-state adoption fraction in [0,1]
    averaged across replicates.}
  \item{m_BD_analytical}{B&D analytical fixed-point adoption fraction
    in [0,1] (Option C reference).}
  \item{m_saomnk_analytical}{SaoMNK-inPop analytical fixed-point
    adoption fraction in [0,1] (Option B reference).}
  \item{abs_error_BD}{\code{abs(m_b_empirical - m_BD_analytical)}.}
  \item{abs_error_saomnk}{\code{abs(m_b_empirical -
    m_saomnk_analytical)}.}
  \item{in_BD_regime}{Logical: \code{TRUE} when
    \eqn{|m_{BD} - 0.5| < 0.15}.}
  \item{log_M, log_error_BD, log_error_saomnk}{\code{log10}
    transforms of the corresponding columns (for slope-of-error
    plotting).}
  \item{qualitative_match}{Logical: empirical and B&D analytical on
    same side of 0.5.}
}
\references{
Brock, W. A. & Durlauf, S. N. (2001). Discrete choice with social
interactions. \emph{Review of Economic Studies}, 68(2), 235--260.

Ellis, R. S. (1985). \emph{Entropy, Large Deviations, and Statistical
Mechanics}. Springer.
}
\seealso{
\code{\link{bd_self_consistency}} (B&D analytical),
\code{\link{saomnk_inpop_self_consistency}} (saomnk-inPop analytical),
\code{\link{bd_equilibrium_count}},
\code{\link{saomnk_social_multiplier}},
\code{\link{bd_landau_steepness}};
\code{vignette("saomnk-brock-durlauf")} for the full Theorem 5
walkthrough including the Option B / Option C tradeoff.
}
\examples{
\dontrun{
## Default Option C regime: J_b = 0.5, h_b = -0.25, m* ~= 0.5
## empirical -> B&D fixed point at O(M^-1/2) (in_BD_regime = TRUE)
verify_brock_durlauf_reduction(M_seq = c(50, 100, 200),
                               n_replicates = 3, n_steps = 500)

## Outside Option C regime: large h_b drives m far from 0.5
## empirical follows m_saomnk_analytical (Option B), not B&D
verify_brock_durlauf_reduction(M_seq = c(50, 100),
                               J_b = 2, h_b = -1,
                               n_replicates = 2)
}
}
\keyword{utilities}
