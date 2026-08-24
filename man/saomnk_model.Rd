% Generated manually for R CMD check compliance
% NOT roxygen-owned: this file has no roxygen2 header, so roxygenise() will
% silently SKIP it rather than regenerate it. Edit it by hand, or delete it
% first and then roxygenise. Corrected 2026-08-24 -- it had documented the
% pre-0.4.0 argument names since v0.2.0.
\name{saomnk_model}
\alias{saomnk_model}
\alias{print.saomnk_model}
\title{Define a SaoMNK Structure Model}
\description{
Builds the SAOM objective function specification that governs actor search
behavior. Users specify effect weights using plain-language parameters.
}
\usage{
saomnk_model(density = -0.5, popularity = 0, scope = 0,
  influence_matrix = NULL, influence_weight = 0.1,
  influence_matrices = NULL, influence_weights = NULL,
  influence_arrays = NULL, influence_array_weights = NULL,
  epistasis_matrix = NULL, epistasis_weight = NULL,
  epistasis_matrices = NULL, epistasis_weights = NULL,
  dyad_covariates = NULL, strategies = NULL, component_covariates = NULL,
  dyad_covariate = NULL, dyad_covariate_effect = "X",
  dyad_covariate_weight = 0.1, ...)

\method{print}{saomnk_model}(x, ...)
}
\arguments{
  \item{density}{Numeric. Weight on the density effect (default -0.5).}
  \item{popularity}{Numeric. Weight on the in-degree popularity effect.}
  \item{scope}{Numeric. Weight on the out-degree activity effect.}
  \item{influence_matrix}{An N x N numeric matrix, or NULL. The
    \strong{influence matrix} \eqn{W}: which components interact, and with
    what sign. This is the model \emph{input}, supplied by the analyst.}
  \item{influence_weight}{Numeric. Coefficient on the \code{XWX} effect that
    carries \code{influence_matrix} into the objective function.}
  \item{influence_matrices}{Named list of N x N matrices, for horseracing
    several influence structures in one specification.}
  \item{influence_weights}{Named numeric vector of coefficients matching
    \code{influence_matrices}.}
  \item{influence_arrays}{Named list of N x N x waves arrays, for
    time-varying influence structures.}
  \item{influence_array_weights}{Named numeric vector matching
    \code{influence_arrays}.}
  \item{epistasis_matrix, epistasis_weight, epistasis_matrices,
    epistasis_weights}{\strong{Deprecated} since 0.4.0; use the
    \code{influence_*} equivalents. Still accepted, with a warning.}
  \item{dyad_covariates}{Named list of M x N actor-by-component matrices.}
  \item{strategies}{Named list of numeric vectors for actor-level covariates.}
  \item{component_covariates}{Named list of N-length numeric vectors.}
  \item{dyad_covariate}{An M x N actor-by-component covariate matrix or NULL.}
  \item{dyad_covariate_effect}{Character. RSiena effect name for the dyad
    covariate (default \code{"X"}).}
  \item{dyad_covariate_weight}{Numeric. Weight for the dyad covariate effect.}
  \item{x}{A \code{saomnk_model} object.}
  \item{\dots}{Additional effects as named lists.}
}
\details{
The argument is named \code{influence_matrix}, not \code{epistasis_matrix},
because the two name different things. \eqn{W} is the \emph{input}: an N x N
matrix stating which components interact. \eqn{K_{CC}} is the realized
inter-component structure that \eqn{W} drives through search --- a degree of
the bipartite projection, reported by \code{\link{saomnk_get_degrees}}.
\emph{Epistatic fitness} is the outcome: the \code{XWX} effect weighted by
\code{influence_weight}. A degree does not measure a fitness consequence.
See the terminology section of the package README.
}
\value{
A list with class \code{"saomnk_model"}.
}
\seealso{\code{\link{saomnk_env}}, \code{\link{saomnk_run}},
  \code{\link{saomnk_get_degrees}}}
\examples{
mod <- saomnk_model(density = -1)
mod2 <- saomnk_model(density = -0.5, influence_matrix = saomnk_block_diagonal(8, 2),
                     influence_weight = 0.3)
}
