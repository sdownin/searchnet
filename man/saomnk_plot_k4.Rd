% Generated manually for R CMD check compliance
\name{saomnk_plot_k4}
\alias{saomnk_plot_k4}
\title{Plot the K-4 Coupled Degree Panel}
\description{
Visualizes the four coupled degree processes over the simulated decision chain.
}
\usage{
saomnk_plot_k4(env, smooth = 0.3)
}
\arguments{
  \item{env}{A \code{SaomNkRSienaBiEnv} object after simulation.}
  \item{smooth}{Numeric. Loess smoothing span.}
}
\value{
The plot object (invisibly).
}
\examples{
\dontrun{
saomnk_plot_k4(env)
}
}
