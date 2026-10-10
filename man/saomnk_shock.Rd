% Generated manually for R CMD check compliance
\name{saomnk_shock}
\alias{saomnk_shock}
\title{Define an Exogenous Shock to a SAOM-NK Model Parameter}
\description{
Constructs a shock specification that can be passed to
\code{\link{saomnk_run}} via the \code{shocks} argument.
}
\usage{
saomnk_shock(effect, parameter, portion = 1L, new_value = NULL, step = NULL)
}
\arguments{
  \item{effect}{Character vector. Which effect(s) to set for this segment.}
  \item{parameter}{Numeric vector (same length as effect). Parameter values.}
  \item{portion}{Integer. Relative duration of this segment (default 1);
    since 0.11.0 portions divide model time, not a ministep count.}
  \item{new_value}{\strong{Deprecated} alias for \code{parameter}, accepted
    for older scripts with a once-per-session warning.}
  \item{step}{Not supported. An absolute switch step cannot be expressed as
    one segment; supplying it stops with the two-shock construction that does
    express it (a baseline segment of \code{portion = s} followed by a shocked
    segment of \code{portion = T - s}).}
}
\value{
A list with class \code{"saomnk_shock"}.
}
\examples{
s1 <- saomnk_shock("density", parameter = -0.5, portion = 1)
}
