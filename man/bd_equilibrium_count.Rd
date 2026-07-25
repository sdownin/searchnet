% Generated manually for R CMD check compliance
\name{bd_equilibrium_count}
\alias{bd_equilibrium_count}
\title{Count Equilibria of the Brock & Durlauf Self-Consistency Equation}
\description{
Returns the number of distinct equilibrium magnetizations of the
Brock & Durlauf (2001) mean-field model.  In the symmetric case
(\code{h = 0}) the count is determined analytically by the Curie--Weiss
criterion: the trivial \eqn{m = 0} root is unstable and flanked by two
symmetric stable roots iff \eqn{\beta J > 1}.  For the asymmetric case
(\code{h != 0}), the count is computed by enumerating all roots via
\code{\link{bd_self_consistency}}.
}
\usage{
bd_equilibrium_count(beta, J, h = 0)
}
\arguments{
  \item{beta}{Numeric inverse-temperature.  Must be non-negative.}
  \item{J}{Numeric peer-interaction strength in spin form.}
  \item{h}{Numeric private-utility bias (default \code{0}).}
}
\value{
Integer scalar: \code{1L} (unique equilibrium) or \code{3L} (multiple
equilibria).  Returns \code{1L} for \eqn{J \le 0} with \eqn{h = 0}
since anti-coordination admits only the trivial equilibrium.
}
\references{
Brock, W. A. & Durlauf, S. N. (2001). Discrete choice with social
interactions. \emph{Review of Economic Studies}, 68(2), 235--260.
}
\seealso{
\code{\link{bd_self_consistency}},
\code{\link{saomnk_social_multiplier}},
\code{\link{bd_landau_steepness}},
\code{\link{verify_brock_durlauf_reduction}};
\code{vignette("saomnk-brock-durlauf")} for the full Theorem 5
walkthrough.
}
\examples{
bd_equilibrium_count(beta = 1, J = 0.5)              # 1 (subcritical)
bd_equilibrium_count(beta = 1, J = 2.0)              # 3 (supercritical)
bd_equilibrium_count(beta = 1, J = 2.0, h = 0.5)     # 1 (field destroys mult.)
}
\keyword{utilities}
