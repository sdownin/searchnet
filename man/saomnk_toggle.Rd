% Generated manually for R CMD check compliance
\name{saomnk_toggle}
\alias{saomnk_toggle}
\title{Toggle a Tie in a One-Mode Network Matrix}
\description{
Flips a tie from 0 to 1 or 1 to 0 at position (i, j) in a square
one-mode adjacency matrix.
}
\usage{
saomnk_toggle(m, i, j)
}
\arguments{
  \item{m}{A square numeric matrix.}
  \item{i}{Row index.}
  \item{j}{Column index.}
}
\value{
The modified matrix.
}
\examples{
m <- matrix(0, 3, 3)
saomnk_toggle(m, 1, 2)
}
