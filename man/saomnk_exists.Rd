% Generated manually for R CMD check compliance
\name{saomnk_exists}
\alias{saomnk_exists}
\title{Check That a Value Exists}
\description{
A safe existence check. Returns TRUE when the value is not NULL, not NA,
and not NaN.
}
\usage{
saomnk_exists(x)
}
\arguments{
  \item{x}{Any R object.}
}
\value{
Logical scalar.
}
\examples{
saomnk_exists(42)
saomnk_exists(NULL)
}
