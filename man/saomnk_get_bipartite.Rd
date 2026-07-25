% Generated manually for R CMD check compliance
\name{saomnk_get_bipartite}
\alias{saomnk_get_bipartite}
\title{Extract the Bipartite Network Matrix}
\description{
Returns the current M x N bipartite adjacency matrix from the environment.
}
\usage{
saomnk_get_bipartite(env, step = NULL)
}
\arguments{
  \item{env}{A \code{SaomNkRSienaBiEnv} object.}
  \item{step}{Integer or NULL. Simulation step to retrieve, or NULL for current.}
}
\value{
An M x N numeric matrix.
}
\examples{
\dontrun{
mat <- saomnk_get_bipartite(env)
}
}
