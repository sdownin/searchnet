% Generated manually for R CMD check compliance
\name{get_jaccard_index}
\alias{get_jaccard_index}
\title{Compute Jaccard Index Between Two Binary Matrices}
\description{
Measures the similarity between two binary matrices as the ratio of
maintained ties to total ties.
}
\usage{
get_jaccard_index(m0, m1)
}
\arguments{
  \item{m0}{A binary matrix (the baseline state).}
  \item{m1}{A binary matrix of the same dimensions (the comparison state).}
}
\value{
A numeric scalar in [0, 1].
}
\examples{
m0 <- matrix(c(1,0,0,1), 2, 2)
m1 <- matrix(c(1,1,0,1), 2, 2)
get_jaccard_index(m0, m1)
}
