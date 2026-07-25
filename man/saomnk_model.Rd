% Generated manually for R CMD check compliance
\name{saomnk_model}
\alias{saomnk_model}
\alias{print.saomnk_model}
\title{Define a SaoMNK Structure Model}
\description{
Builds the SAOM objective function specification that governs actor search
behaviour. Users specify effect weights using plain-language parameters.
}
\usage{
saomnk_model(density = -0.5, popularity = 0, scope = 0,
  epistasis_matrix = NULL, epistasis_weight = 0.1,
  strategies = NULL, component_covariates = NULL,
  dyad_covariate = NULL, dyad_covariate_effect = "egoXaltX",
  dyad_covariate_weight = 0.1, ...)

\method{print}{saomnk_model}(x, ...)
}
\arguments{
  \item{density}{Numeric. Weight on the density effect (default -0.5).}
  \item{popularity}{Numeric. Weight on the in-degree popularity effect.}
  \item{scope}{Numeric. Weight on the out-degree activity effect.}
  \item{epistasis_matrix}{An N x N numeric matrix or NULL.}
  \item{epistasis_weight}{Numeric. Weight on the epistasis effect.}
  \item{strategies}{Named list of numeric vectors for actor-level covariates.}
  \item{component_covariates}{Named list of N-length numeric vectors.}
  \item{dyad_covariate}{An M x N actor-by-component covariate matrix or NULL.}
  \item{dyad_covariate_effect}{Character. RSiena effect name for dyad covariate.}
  \item{dyad_covariate_weight}{Numeric. Weight for the dyad covariate effect.}
  \item{x}{A \code{saomnk_model} object.}
  \item{\dots}{Additional effects as named lists.}
}
\value{
A list with class \code{"saomnk_model"}.
}
\examples{
mod <- saomnk_model(density = -1)
}
