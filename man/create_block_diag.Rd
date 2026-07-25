% Generated manually for R CMD check compliance
\name{create_block_diag}
\alias{create_block_diag}
\title{Create a Block-Diagonal Epistasis Matrix}
\description{
Builds a symmetric block-diagonal binary matrix of dimension N x N
with B approximately equal-sized blocks.
}
\usage{
create_block_diag(N, B)
}
\arguments{
  \item{N}{Integer. Total number of components.}
  \item{B}{Integer. Number of blocks (modules).}
}
\value{
A numeric N x N matrix.
}
\examples{
create_block_diag(12, 3)
}
