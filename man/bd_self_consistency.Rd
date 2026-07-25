% Generated manually for R CMD check compliance
\name{bd_self_consistency}
\alias{bd_self_consistency}
\title{Solve the Brock & Durlauf Self-Consistency Equation}
\description{
Solves the canonical Brock & Durlauf (2001) mean-field self-consistency
equation \eqn{m = \tanh(\beta h + \beta J m)} for the spin-form
magnetization \eqn{m \in [-1, 1]}.  When the model admits multiple
equilibria (high \eqn{\beta J}), all of them are returned.
}
\usage{
bd_self_consistency(beta, J, h, x0 = 0, tol = 1e-10,
                    max_iter = 1000, all_roots = TRUE)
}
\arguments{
  \item{beta}{Numeric inverse-temperature (selection intensity).  Must be
    non-negative.}
  \item{J}{Numeric peer-interaction strength in spin form.  Positive
    \eqn{J} corresponds to the coordination / herding regime; negative
    \eqn{J} to the anti-coordination / congestion regime.}
  \item{h}{Numeric private-utility bias (external field) in spin form.}
  \item{x0}{Numeric starting value for fixed-point iteration when
    \code{all_roots = FALSE} (default \code{0}, must lie in
    \eqn{[-1, 1]}).}
  \item{tol}{Numeric convergence tolerance (default \code{1e-10}).}
  \item{max_iter}{Integer maximum number of fixed-point iterations
    (default \code{1000}).}
  \item{all_roots}{Logical.  If \code{TRUE} (default), scan
    \eqn{[-1, 1]} at 1001 grid points to bracket all sign changes of the
    residual \eqn{f(m) = m - \tanh(\beta h + \beta J m)} and refine each
    via \code{\link[stats]{uniroot}}.  If \code{FALSE}, run fixed-point
    iteration starting from \code{x0} and return a single scalar.}
}
\value{
A numeric vector of equilibrium magnetizations.  Length 1 in the
unique-equilibrium regime, length 3 in the multiple-equilibria regime
when \code{all_roots = TRUE}; a scalar when \code{all_roots = FALSE}.
}
\references{
Brock, W. A. & Durlauf, S. N. (2001). Discrete choice with social
interactions. \emph{Review of Economic Studies}, 68(2), 235--260.
}
\seealso{
\code{\link{bd_equilibrium_count}},
\code{\link{saomnk_social_multiplier}},
\code{\link{bd_landau_steepness}},
\code{\link{verify_brock_durlauf_reduction}};
\code{vignette("saomnk-brock-durlauf")} for the full Theorem 5
walkthrough.
}
\examples{
## Unique equilibrium (subcritical: beta * J < 1, h = 0)
bd_self_consistency(beta = 1, J = 0.5, h = 0)

## Three equilibria (supercritical: beta * J > 1, h = 0)
bd_self_consistency(beta = 1, J = 2, h = 0)

## Tilted field, h != 0: fixed-point iteration
bd_self_consistency(beta = 1, J = 2, h = 0.1, all_roots = FALSE)
}
\keyword{utilities}
