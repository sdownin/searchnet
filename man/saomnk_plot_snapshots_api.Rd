% Generated manually for R CMD check compliance
\name{saomnk_plot_snapshots}
\alias{saomnk_plot_snapshots}
\title{Plot Network Snapshots}
\description{
Renders bipartite network snapshots at selected simulation steps, together
with their unipartite projections.
}
\usage{
saomnk_plot_snapshots(env, steps = NULL)
}
\arguments{
  \item{env}{A \code{SaomNkRSienaBiEnv} object after simulation.}
  \item{steps}{Integer vector of simulation step indices, or NULL for auto.}
}
\value{
Called for side effects (plot display).
}
\examples{
\dontrun{
saomnk_plot_snapshots(env)
}
}
