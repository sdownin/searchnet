% Generated manually for R CMD check compliance
\name{bd_landau_steepness}
\alias{bd_landau_steepness}
\title{Landau--Ginzburg Quartic Coefficient and Basin Steepness}
\description{
Computes the Landau--Ginzburg quartic coefficient
\deqn{b = \frac{2 \tau (1 + 3 m^{*2})}{(1 - m^{*2})^3}}
from the AMR herding-steepness derivation, and the predicted basin
steepness exponent
\deqn{s = 2 + \frac{b\, \delta_m^{2}}{2(\beta J - 1)}}
for a small fluctuation \eqn{\delta_m} around the equilibrium.

For \eqn{|m^*|} close to 1 the formula diverges; the result is capped at
\code{1e6} with a warning.  When \eqn{\beta J = 1} the system is at the
critical point and the steepness expression has a removable singularity
(returned as \code{Inf} with a warning).
}
\usage{
bd_landau_steepness(beta, J, m_star, tau = 1, delta_m = 0.05)
}
\arguments{
  \item{beta}{Numeric inverse-temperature.}
  \item{J}{Numeric spin-form peer-interaction strength.}
  \item{m_star}{Numeric equilibrium magnetization in \eqn{[-1, 1]}.}
  \item{tau}{Numeric coupling-time constant (default \code{1}).}
  \item{delta_m}{Numeric fluctuation amplitude (default \code{0.05}).}
}
\value{
A list with components:
  \item{b}{Landau quartic coefficient (numeric, possibly capped).}
  \item{steepness}{Predicted basin steepness exponent \eqn{s}
    (numeric).}
  \item{capped}{Logical, \code{TRUE} if the formula diverged and
    \code{b} was capped at \code{1e6}.}
}
\references{
Brock, W. A. & Durlauf, S. N. (2001). Discrete choice with social
interactions. \emph{Review of Economic Studies}, 68(2), 235--260.

Goldenfeld, N. (1992). \emph{Lectures on Phase Transitions and the
Renormalization Group}. Addison--Wesley.
}
\seealso{
\code{\link{bd_self_consistency}},
\code{\link{bd_equilibrium_count}},
\code{\link{saomnk_social_multiplier}},
\code{\link{verify_brock_durlauf_reduction}};
\code{vignette("saomnk-brock-durlauf")} for the full Theorem 5
walkthrough.
}
\examples{
bd_landau_steepness(beta = 1, J = 2, m_star = 0.5)
bd_landau_steepness(beta = 1, J = 2, m_star = 0.95)  # capped
}
\keyword{utilities}
