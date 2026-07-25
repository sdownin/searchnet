% Generated manually for R CMD check compliance
\name{saomnk_social_multiplier}
\alias{saomnk_social_multiplier}
\title{Brock & Durlauf Social Multiplier}
\description{
Computes the Brock & Durlauf (2001) social multiplier
\deqn{\mathcal{M} = \frac{1}{1 - \beta J (1 - m^{*2})},}
which measures how a marginal change in private utility \eqn{h} is
amplified by social feedback.  As \eqn{\beta J (1 - m^{*2}) \to 1} the
multiplier diverges, signalling proximity to the Curie--Weiss phase
transition.

Two calling conventions are supported.  If \code{env} is supplied, the
B&D-equivalent \eqn{J} and \eqn{h} are extracted from the environment's
structure-model effects table and the empirical \eqn{m^*} is read off
from the bipartite matrix via \eqn{m^* = 2 \bar{B} - 1}; the spin-form
recoding contributes a factor of 4 to \eqn{J} (Rb4 of the proof table)
and is applied automatically.  Otherwise, explicit \code{beta},
\code{J}, \code{h} must be supplied.
}
\usage{
saomnk_social_multiplier(env = NULL, beta = NULL,
                         J = NULL, h = NULL,
                         m_star = NULL)
}
\arguments{
  \item{env}{Optional SaoMNK environment (\code{SaomNkRSienaBiEnv}) from
    which to extract parameters.  If \code{NULL} (default), explicit
    \code{beta}, \code{J}, \code{h} must be supplied.}
  \item{beta}{Numeric inverse-temperature.  Required when \code{env} is
    \code{NULL}.}
  \item{J}{Numeric spin-form peer-interaction strength.  Ignored when
    \code{env} is supplied.}
  \item{h}{Numeric spin-form private-utility bias.  Ignored when
    \code{env} is supplied.}
  \item{m_star}{Numeric equilibrium magnetization at which to evaluate
    the multiplier.  If \code{NULL}, computed internally.}
}
\value{
A list with components:
  \item{multiplier}{The B&D social multiplier (numeric).}
  \item{m_star}{The equilibrium magnetization used (numeric).}
  \item{bd_J_spin}{The spin-form peer-interaction strength used.}
  \item{bd_h_spin}{The spin-form private bias used.}
  \item{warning_near_threshold}{Logical, \code{TRUE} when
    \code{abs(multiplier) > 10} (i.e., near the phase transition).}
}
\references{
Brock, W. A. & Durlauf, S. N. (2001). Discrete choice with social
interactions. \emph{Review of Economic Studies}, 68(2), 235--260.

Manski, C. F. (1993). Identification of endogenous social effects: The
reflection problem. \emph{Review of Economic Studies}, 60(3), 531--542.
}
\seealso{
\code{\link{bd_self_consistency}},
\code{\link{bd_equilibrium_count}},
\code{\link{bd_landau_steepness}},
\code{\link{verify_brock_durlauf_reduction}};
\code{vignette("saomnk-brock-durlauf")} for the full Theorem 5
walkthrough.
}
\examples{
## Explicit-parameter form
saomnk_social_multiplier(beta = 1, J = 0.5, h = 0)

## Near phase transition (large multiplier)
saomnk_social_multiplier(beta = 1, J = 0.95, h = 0)

\dontrun{
## Env-based extraction from a fitted SaoMNK environment
env <- saomnk_env(M = 50, N = 1, density = 0.5, seed = 1)
mod <- saomnk_model(density = -0.5, popularity = -2)
saomnk_run(env, mod, steps_per_actor = 50, seed = 1)
saomnk_social_multiplier(env = env, beta = 1)
}
}
\keyword{utilities}
