% Generated manually for R CMD check compliance
\name{saomnk_block_diagonal}
\alias{saomnk_block_diagonal}
\title{Create a Block-Diagonal Epistasis Matrix}
\description{
Constructs an N x N binary block-diagonal matrix representing modular
component interaction structure.
}
\usage{
saomnk_block_diagonal(N, blocks)
}
\arguments{
  \item{N}{Integer. Number of components (matrix dimension).}
  \item{blocks}{Integer. Number of modules (blocks).}
}
\value{
An N x N numeric matrix of 0s and 1s.
}
\examples{
saomnk_block_diagonal(12, 4)
}
