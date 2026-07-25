% Generated manually for R CMD check compliance
\name{saomnk_get_degrees}
\alias{saomnk_get_degrees}
\title{Extract Coupled Degree Data Frames}
\description{
Returns a named list of the four coupled degree data frames.
}
\usage{
saomnk_get_degrees(env)
}
\arguments{
  \item{env}{A \code{SaomNkRSienaBiEnv} object after simulation.}
}
\value{
A named list with elements \code{K_AC}, \code{K_CA}, \code{K_AA}, \code{K_CC}.
}
\examples{
\dontrun{
degs <- saomnk_get_degrees(env)
}
}
