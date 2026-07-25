\name{strategic_alliance_formation}
\alias{strategic_alliance_formation}
\title{Strategic Alliance Formation with K-System Moderation}
\description{
Forms alliances between actor (firm) nodes with knowledge transfer moderated
by architectural coupling (K_CC). Implements the theoretical logic of
Lavie (2006) on competitive advantage of interconnected firms,
Hernandez and Shaver (2019) on network synergy, and Hernandez and Menon
(2021) on endogenous network reconfiguration through corporate strategy.

Tightly coupled firms (high K_CC) absorb less knowledge from alliance
partners due to absorptive capacity constraints, formalizing the
cooperative interdependence paradox (K-system Proposition 4).

This is a method of the \code{\link{SaomNkRSienaBiEnv}} R6 class.
}
\section{Usage}{
\preformatted{
env$strategic_alliance_formation(actor_i, actor_j,
    alliance_type = "knowledge_transfer", transfer_rate = 0.3,
    complementarity_threshold = 0.0, verbose = FALSE)
}
}
\arguments{
  \item{actor_i}{Integer. Index of the first alliance partner.}
  \item{actor_j}{Integer. Index of the second alliance partner.}
  \item{alliance_type}{Character. One of:
    \describe{
      \item{\code{"knowledge_transfer"}}{Bidirectional: both partners may
        adopt each other's unique activities.}
      \item{\code{"one_way"}}{Unidirectional: only \code{actor_i} receives
        knowledge from \code{actor_j}.}
      \item{\code{"merger"}}{Calls \code{merge_nodes()} internally.}
    }
  }
  \item{transfer_rate}{Numeric in [0, 1]. Base probability that each of
    the partner's unique activities is transferred. Effective rate is
    reduced by the recipient's coupling penalty.}
  \item{complementarity_threshold}{Numeric in [0, 1]. Minimum Jaccard
    distance (1 - overlap) required. If complementarity is below this
    threshold, a warning is issued (alliance proceeds regardless).}
  \item{verbose}{Logical. If TRUE, prints complementarity, coupling
    penalties, and transfer summary.}
}
\value{
Returns \code{invisible(self)} for method chaining. Side effects:
updates \code{bipartite_matrix} with transferred activities, recalculates
projections, and appends to \code{alliance_log}.
}
\references{
Lavie, D. (2006). The competitive advantage of interconnected firms: An
extension of the resource-based view. \emph{Academy of Management Review},
31(3), 638--658.

Hernandez, E., & Shaver, J. M. (2019). Network synergy.
\emph{Administrative Science Quarterly}, 64(1), 171--202.

Hernandez, E., & Menon, A. (2021). Corporate strategy and network change.
\emph{Academy of Management Review}, 46(1), 80--107.
}
\examples{
\dontrun{
config <- list(M = 10, N = 15, bipartite_prob = 0.3)
env <- SaomNkRSienaBiEnv$new(config_environ_params = config)

# Bidirectional knowledge transfer alliance
env$strategic_alliance_formation(actor_i = 1, actor_j = 5,
    alliance_type = "knowledge_transfer",
    transfer_rate = 0.4, verbose = TRUE)

# Check alliance log
print(env$alliance_log)
}
}
