\name{merge_nodes}
\alias{merge_nodes}
\title{Node Collapse via M&A (Hernandez & Menon, 2018)}
\description{
Merges two or more actor (firm) nodes or component (activity) nodes in the
bipartite network, implementing the "node collapse" mechanism described by
Hernandez and Menon (2018). When firms merge via M&A, a node is removed from
the competitive network and K-system dimensions (K_CC, K_CA, K_AA) reconfigure
for all remaining firms---not only the merging parties.

This is a method of the \code{\link{SaomNkRSienaBiEnv}} R6 class.
}
\section{Usage}{
\preformatted{
env$merge_nodes(node_type = "actor", node_ids = c(1, 2),
                merge_method = "union", integration_friction = 0.0,
                verbose = FALSE)
}
}
\arguments{
  \item{node_type}{Character. Either \code{"actor"} (firm merger) or
    \code{"component"} (activity consolidation).}
  \item{node_ids}{Integer vector of length >= 2. Indices of nodes to merge.
    The first element becomes the surviving node.}
  \item{merge_method}{Character. Either \code{"union"} (merged entity performs
    all activities either firm performed) or \code{"intersection"} (only
    shared activities retained).}
  \item{integration_friction}{Numeric in [0, 1]. Probability that each
    activity is randomly dropped during integration, modeling post-merger
    disruption per Feldman and Hernandez (2022) synergy life-cycle dynamics.}
  \item{verbose}{Logical. If TRUE, prints diagnostic summary.}
}
\value{
Returns \code{invisible(self)} for method chaining. Side effects:
updates \code{bipartite_matrix}, \code{social_matrix}, \code{search_matrix},
\code{M} or \code{N}, and appends to \code{merge_log}.
}
\references{
Hernandez, E., & Menon, A. (2018). Acquisitions, node collapse, and network
revolution. \emph{Management Science}, 64(4), 1652--1671.

Hernandez, E., & Menon, A. (2021). Corporate strategy and network change.
\emph{Academy of Management Review}, 46(1), 80--107.

Feldman, E. R., & Hernandez, E. (2022). Synergy in mergers and acquisitions:
Typology, life cycles, and value. \emph{Academy of Management Review}, 47(4),
549--578.
}
\examples{
\dontrun{
config <- list(M = 10, N = 15, bipartite_prob = 0.3)
env <- SaomNkRSienaBiEnv$new(config_environ_params = config)

# Merge firms 1 and 2 (union of activity portfolios)
env$merge_nodes(node_type = "actor", node_ids = c(1, 2),
                merge_method = "union", verbose = TRUE)

# Merge with integration friction (30\% activity loss)
env$merge_nodes(node_type = "actor", node_ids = c(3, 4),
                merge_method = "union", integration_friction = 0.3)
}
}
