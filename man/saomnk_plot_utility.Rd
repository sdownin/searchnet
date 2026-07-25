% Generated manually for R CMD check compliance
\name{saomnk_plot_utility}
\alias{saomnk_plot_utility}
\title{Plot Utility Contributions}
\description{
Decomposes each actor's objective-function value into contributions from
individual network statistics over the simulated decision chain.
}
\usage{
saomnk_plot_utility(env, smooth = 0.35, weighted = TRUE)
}
\arguments{
  \item{env}{A \code{SaomNkRSienaBiEnv} object after simulation.}
  \item{smooth}{Numeric. Loess smoothing span.}
  \item{weighted}{Logical. Multiply raw statistics by theta weights.}
}
\value{
Called for side effects (plot display).
}
\examples{
\dontrun{
saomnk_plot_utility(env)
}
}
