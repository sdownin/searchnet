\name{did_shock_analysis}
\alias{did_shock_analysis}
\title{Difference-in-Differences Causal Analysis for Network Shocks}
\description{
Implements a comprehensive difference-in-differences (DID) analysis for
network shock experiments, following the causal identification approach
advocated by Hernandez, Lee, and Shaver (2025). Supports multiple outcome
variables (utility, K statistics, degree), parallel trends pre-testing,
and automated visualization.

Designed for use after running \code{search_rsiena()} with
\code{theta_shocks} to evaluate the causal effect of exogenous
interventions on network outcomes.

This is a method of the \code{\link{SaomNkRSienaBiEnv}} R6 class.
}
\section{Usage}{
\preformatted{
env$did_shock_analysis(treatment_step = NULL, outcome_var = "utility",
    treatment_ids = NULL, control_ids = NULL, covariates = NULL,
    parallel_trends_test = TRUE, plot = TRUE, verbose = FALSE)
}
}
\arguments{
  \item{treatment_step}{Integer (required). The chain step at which the
    shock/intervention begins. Observations before this step are
    pre-treatment; at or after are post-treatment.}
  \item{outcome_var}{Character. One of \code{"utility"}, \code{"K_AC"},
    \code{"K_AA"}, \code{"K_CC"}, \code{"K_CA"}, or \code{"degree"}.}
  \item{treatment_ids}{Integer vector. Actor IDs in the treatment group.
    If NULL, defaults to first half of actors.}
  \item{control_ids}{Integer vector. Actor IDs in the control group.
    If NULL, defaults to all non-treatment actors.}
  \item{covariates}{Character vector. Column names from actor data to
    include as controls in the DID regression.}
  \item{parallel_trends_test}{Logical. If TRUE, tests the parallel trends
    assumption by regressing pre-treatment outcomes on time x treatment
    interaction. A significant coefficient (p < 0.05) indicates violation.}
  \item{plot}{Logical. If TRUE, generates a ggplot showing treatment vs.
    control group mean trajectories with the DID estimate annotated.}
  \item{verbose}{Logical. If TRUE, prints detailed output.}
}
\value{
A named list with components:
  \item{did_estimate}{Numeric. The DID coefficient (treatment effect).}
  \item{se}{Numeric. Standard error of the DID estimate.}
  \item{p_value}{Numeric. P-value for the DID estimate.}
  \item{ci_lower, ci_upper}{Numeric. 95\% confidence interval bounds.}
  \item{parallel_trends_p}{Numeric or NA. P-value from parallel trends
    test. Values > 0.05 indicate the assumption holds.}
  \item{treatment_step}{Integer. The specified treatment step.}
  \item{n_treated, n_control}{Integer. Group sizes.}
  \item{treatment_ids, control_ids}{Integer vectors. Group membership.}
  \item{model}{lm object. The fitted DID regression model.}
  \item{model_summary}{Summary of the lm object.}
  \item{panel_data}{Data frame. The constructed DID panel.}
  \item{plot}{ggplot object (if \code{plot = TRUE}).}

Also stores results in \code{self$did_analysis_results}.
}
\references{
Hernandez, E., Lee, J. K., & Shaver, J. M. (2025). Toward an improved
causal test of network effects: Does alliance network position enhance
firm innovation? \emph{Strategic Management Journal}, 46(4), 863--897.
}
\examples{
\dontrun{
config <- list(M = 12, N = 15, bipartite_prob = 0.3)
env <- SaomNkRSienaBiEnv$new(config_environ_params = config)

# Run simulation with shock
theta_shocks <- list(
  list(effect = "egoX", parameter = 2.0, shock_on = 1,
       chain_step_ids = 50:100, label = "Capability shock")
)
env$search_rsiena(structure_model = list(effects = list(
  list(effect = "density")
)), theta_shocks = theta_shocks, iterations = 100)

# DID analysis
results <- env$did_shock_analysis(
  treatment_step = 50,
  outcome_var = "utility",
  treatment_ids = 1:6,
  control_ids = 7:12,
  parallel_trends_test = TRUE,
  plot = TRUE,
  verbose = TRUE
)

# Access results
cat("DID Estimate:", results$did_estimate, "\n")
cat("P-value:", results$p_value, "\n")
}
}
