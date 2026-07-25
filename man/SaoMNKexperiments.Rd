% Generated manually for R CMD check compliance
\name{SaoMNKexperiments}
\alias{SaoMNKexperiments}
\title{R6 Class for Monte Carlo Experiment Batches}
\description{
R6 class for running batches of SaoMNK simulations, aggregating results,
and analyzing market entry survival curves.
}
\examples{
\dontrun{
exp <- SaoMNKexperiments$new(
  experiment_name = "test",
  n_simulations = 10,
  base_environ_params = list(M = 6, N = 12, BI_PROB = 0.3)
)
}
}
