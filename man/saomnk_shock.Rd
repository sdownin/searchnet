% Generated manually for R CMD check compliance
\name{saomnk_shock}
\alias{saomnk_shock}
\title{Define an Exogenous Shock to a SaoMNK Model Parameter}
\description{
Constructs a shock specification that can be passed to
\code{\link{saomnk_run}} via the \code{shocks} argument.
}
\usage{
saomnk_shock(effect, parameter, portion = 1L)
}
\arguments{
  \item{effect}{Character vector. Which effect(s) to set for this segment.}
  \item{parameter}{Numeric vector (same length as effect). Parameter values.}
  \item{portion}{Integer. Relative size of this segment (default 1).}
}
\value{
A list with class \code{"saomnk_shock"}.
}
\examples{
s1 <- saomnk_shock("density", parameter = -0.5, portion = 1)
}
