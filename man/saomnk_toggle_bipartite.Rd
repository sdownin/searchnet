% Generated manually for R CMD check compliance
\name{saomnk_toggle_bipartite}
\alias{saomnk_toggle_bipartite}
\title{Toggle a Tie in a Bipartite Network Matrix}
\description{
Flips a tie from 0 to 1 or 1 to 0 at position (i, j) in a rectangular
bipartite adjacency matrix.
}
\usage{
saomnk_toggle_bipartite(m, i, j)
}
\arguments{
  \item{m}{A numeric matrix (rows = actors, columns = components).}
  \item{i}{Row index (actor).}
  \item{j}{Column index (component).}
}
\value{
The modified matrix.
}
\examples{
m <- matrix(0, 3, 5)
saomnk_toggle_bipartite(m, 1, 2)
}
