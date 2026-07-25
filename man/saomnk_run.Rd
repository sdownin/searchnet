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
  shocks = NULL, verbose = FALSE)
}
\arguments{
  \item{env}{A \code{SaomNkRSienaBiEnv} object.}
  \item{model}{A structure model created by \code{\link{saomnk_model}}.}
  \item{steps_per_actor}{Integer. Decision-chain micro-steps per actor.}
  \item{seed}{Integer or NULL. Random seed.}
  \item{shocks}{A list of \code{\link{saomnk_shock}} objects or NULL.}
  \item{verbose}{Logical. Print diagnostic output.}
}
\value{
The \code{env} object (modified in place), returned invisibly.
}
\examples{
\dontrun{
env <- saomnk_env(M = 3, N = 6, seed = 1234)
mod <- saomnk_model(density = -0.5)
saomnk_run(env, mod, steps_per_actor = 5, seed = 12345)
}
}
