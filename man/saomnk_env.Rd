% Generated manually for R CMD check compliance
\name{saomnk_env}
\alias{saomnk_env}
\title{Create a SaoMNK Search Environment}
\description{
Initialises a bipartite search environment of \eqn{M} actors and \eqn{N}
components. This is the starting point for any SaoMNK simulation.
}
\usage{
saomnk_env(M, N, density = 0, seed = NULL, name = NULL)
}
\arguments{
  \item{M}{Integer. Number of actors (firms, agents).}
  \item{N}{Integer. Number of components (resources, technologies, markets).}
  \item{density}{Numeric in [0, 1]. Initial bipartite network density.}
  \item{seed}{Integer or NULL. Random seed for reproducibility.}
  \item{name}{Character or NULL. Optional label for the environment.}
}
\value{
A \code{SaomNkRSienaBiEnv} R6 object.
}
\examples{
\dontrun{
env <- saomnk_env(M = 6, N = 12, seed = 42)
}
}
