#!/usr/bin/env Rscript
#' Basic SaoMNK Simulation Example
#'
#' Demonstrates the core API: create environment, configure model, run, visualize.

library(SaoMNK)

## 1. Environment Configuration
environ_params <- list(
  M = 9,          # Number of actors (firms)
  N = 16,         # Number of components (activities)
  BI_PROB = 0.5,  # Initial bipartite density
  rand_seed = 123,
  name = "basic_example"
)

## 2. Actor Strategies (covariates for SAOM structural effects)
actor_strats <- list(
  egoX   = rep(c(-1, 0, 1), 3),   # 3 strategy types across 9 actors
  inPopX = rep(c(1, 0, -1), 3)
)

## 3. Component Payoffs (attractiveness of each activity)
set.seed(42)
component_payoffs <- runif(environ_params$N, min = 0, max = 1)

## 4. Structure Model: defines the utility/objective function
dv_name <- "self$bipartite_rsienaDV"
structure_model <- list(
  dv_bipartite = list(
    name = dv_name,
    effects = list(
      list(effect = "density", parameter = -1,  fix = TRUE, dv_name = dv_name),
      list(effect = "inPop",   parameter = 0.1, fix = TRUE, dv_name = dv_name),
      list(effect = "outAct",  parameter = 0.1, fix = TRUE, dv_name = dv_name)
    ),
    coCovars = list(
      list(effect = "altX",    parameter = 1,   fix = TRUE, dv_name = dv_name,
           interaction1 = "self$component_1_coCovar", x = component_payoffs),
      list(effect = "egoX",    parameter = 1,   fix = TRUE, dv_name = dv_name,
           interaction1 = "self$strat_1_coCovar", x = actor_strats[["egoX"]]),
      list(effect = "inPopX",  parameter = 0.3, fix = TRUE, dv_name = dv_name,
           interaction1 = "self$strat_2_coCovar", x = actor_strats[["inPopX"]])
    ),
    varCovars = list()
  )
)

## 5. Initialize and Run
m1 <- SaomNkRSienaBiEnv$new(environ_params)

m1$search_rsiena_multiwave_run(
  structure_model,
  waves = 1,
  iterations = 500,
  rand_seed = 12345
)

## 6. Process Results
m1$search_rsiena_multiwave_process_results()

## 7. Visualize
# Actor utility trajectories by strategy type
m1$search_rsiena_multiwave_plot("utility_strategy_summary", thin_factor = 1)

# K4 degree distribution panel (K_AA, K_AC, K_CA, K_CC)
m1$search_rsiena_multiwave_plot("K_4panel", thin_factor = 1)

cat("\nSimulation complete. Check plots output.\n")
