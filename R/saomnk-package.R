#' searchnet: Network-Embedded Search Simulation Engine
#'
#' R6-based simulation engine for strategic search and adaptation
#' on NK fitness landscapes embedded in social networks. Uses RSiena's
#' Stochastic Actor-Oriented Model methodology to represent firm activity
#' systems as bipartite actor-component networks with endogenous feedback
#' loops. Formerly known as SaoMNK.
#'
#' @section Core Classes:
#' \describe{
#'   \item{\code{\link{SaomNkRSienaBiEnv_base}}}{Base class with network
#'     structures, projections, covariates, and RSiena data setup.}
#'   \item{\code{\link{SaomNkRSienaBiEnv}}}{Extended class adding simulation
#'     execution, plotting, experiments, and multi-wave support.}
#'   \item{\code{\link{SaomNkRSienaBiEnv_search_rsiena}}}{Search class with
#'     RSiena-specific simulation methods.}
#' }
#'
#' @section Key Concepts:
#' \describe{
#'   \item{Bipartite Network}{M actors x N components, representing activity systems.}
#'   \item{K4 Degree Distributions}{K_AA (social), K_AC (actor scope),
#'     K_CA (component popularity), K_CC (epistasis).}
#'   \item{Shocks}{Parameter perturbations for intervention analysis (subsidies, etc.).}
#'   \item{Multi-wave}{Dynamic strategy evolution across simulation periods.}
#' }
#'
#' @docType package
#' @name searchnet-package
"_PACKAGE"
