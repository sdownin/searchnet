% Generated manually for R CMD check compliance
\name{saomnk_run}
\alias{saomnk_run}
\title{Run a SaoMNK Search Simulation}
\description{
Executes the SAOM-based search simulation on a prepared environment and
structure model.
}
\usage{
saomnk_run(env, model, steps_per_actor = 30, seed = NULL,
  shocks = NULL, theta_matrix = NULL, verbose = FALSE, restart = TRUE)
}
\arguments{
  \item{env}{A \code{SaomNkRSienaBiEnv} object.}
  \item{model}{A structure model created by \code{\link{saomnk_model}}.}
  \item{steps_per_actor}{Numeric. The basic rate summed over the run: the
    expected number of decision opportunities per actor at zero rate effects.
    Since 0.11.0 the run is one unit of model time simulated as a genuine SAOM
    path, so the realized number of ministeps is random and rate effects
    change it. Before 0.11.0 it was a fixed count of \code{M * steps_per_actor}
    ministeps, each drawn independently from the starting state.}
  \item{seed}{Integer or NULL. Random seed.}
  \item{shocks}{A list of \code{\link{saomnk_shock}} objects or NULL.}
  \item{theta_matrix}{Optional numeric matrix of parameter values on a time
    grid, as built by \code{\link{saomnk_theta_ramp}} or
    \code{\link{saomnk_theta_drift}}. Each block of identical rows is one
    segment started from the previous segment's end state (at most 50; a finer
    schedule is coarsened). Its row count sets the summed basic rate to
    \code{nrow(theta_matrix) / M} per actor and overrides
    \code{steps_per_actor}. Default \code{NULL}.}
  \item{verbose}{Logical. Print diagnostic output.}
  \item{restart}{Logical. If \code{TRUE} (default), the run starts from
    \code{env$bipartite_matrix_init}; if \code{FALSE}, it continues from
    \code{env$bipartite_matrix}, so successive calls form one path.}
}
\seealso{
\code{\link{saomnk_theta_ramp}}, \code{\link{saomnk_theta_drift}},
\code{\link{saomnk_behavior}}
}
\value{
The \code{env} object (modified in place), returned invisibly. The run's
provenance (searchnet, RSiena and R versions, RNG kind, seed, call) is stored
in \code{env$provenance}; see \code{\link{searchnet_provenance}}.
}
\examples{
\dontrun{
env <- saomnk_env(M = 3, N = 6, seed = 1234)
mod <- saomnk_model(density = -0.5)
saomnk_run(env, mod, steps_per_actor = 5, seed = 12345)
}
}
