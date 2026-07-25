% Generated manually for R CMD check compliance
\name{saomnk_empirical_epistasis}
\alias{saomnk_empirical_epistasis}
\title{Compute an Empirical Epistasis Matrix from Observed Bipartite Data}
\description{
Given a bipartite matrix B (actors x components), computes the component
co-occurrence matrix as a measure of empirical epistasis. Components that
frequently co-occur in the same actors' portfolios have higher epistasis.

Three methods are available: Jaccard similarity, cosine similarity, and
normalized co-occurrence counts.
}
\usage{
saomnk_empirical_epistasis(B, method = "jaccard", threshold = 0, diagonal = 0)
}
\arguments{
  \item{B}{A binary bipartite matrix (M x N) or a SaomNkRSienaBiEnv object.}
  \item{method}{One of \code{"jaccard"}, \code{"cosine"}, or \code{"cooccurrence"}.}
  \item{threshold}{Minimum similarity to retain; entries below this are set to 0.}
  \item{diagonal}{Value for diagonal entries (default 0).}
}
\value{
An N x N symmetric numeric epistasis matrix suitable for use as
\code{epistasis_matrix} in \code{\link{saomnk_model}}.
}
\examples{
B <- matrix(sample(0:1, 40, replace = TRUE, prob = c(0.6, 0.4)), 5, 8)
W <- saomnk_empirical_epistasis(B, method = "jaccard")
}
