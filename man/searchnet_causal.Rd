% Generated manually for R CMD check compliance
\name{searchnet_causal_panel}
\alias{searchnet_causal_panel}
\alias{searchnet_did}
\alias{searchnet_synth}
\alias{searchnet_rd}
\alias{searchnet_causal_plot}
\title{Causal Inference Wrappers}
\description{
Functions wrapping difference-in-differences, synthetic control,
regression discontinuity, and panel data construction for causal
inference on SaoMNK simulation experiments.

\code{searchnet_causal_panel}, \code{searchnet_did}, \code{searchnet_synth},
and \code{searchnet_rd} take a \code{SaomNkRSienaBiEnv} object as their
first argument. \code{searchnet_causal_plot} takes a results object.
}
\examples{
\dontrun{
panel <- searchnet_causal_panel(env)
did_results <- searchnet_did(env)
}
}
