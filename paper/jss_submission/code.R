#' ---
#' title: "searchnet: replication code for the JSS manuscript"
#' output:
#'   html_document:
#'     toc: true
#' ---
#'
#' The R code of every chunk in `searchnet-jss.Rmd`, in manuscript order,
#' extracted with `knitr::purl()` by `paper/build_submission.R`.
#' Running this script reproduces the results reported in the paper.
#' Code the manuscript shows without evaluating (`eval = FALSE`) is
#' commented out. `code.html` is this script rendered with
#' `knitr::spin()`. The two simulated figures included as image files
#' are produced by `replication/make_k_system_figures.R`; the other
#' image files are diagrams.
#'
#' Generated file: edit the manuscript, not this script.

#+ setup
## JSS conventions: R> prompt, "+" continuation, no output comment prefix,
## code width at most 70 characters.
##
## SEARCHNET_JSS_FAST=1 renders with every chunk unevaluated (format check
## only; paper/build_submission.R --fast sets it). A submission build must
## leave it unset so that all output is regenerated from the code.
.fast <- identical(Sys.getenv("SEARCHNET_JSS_FAST"), "1")
knitr::opts_chunk$set(
  prompt     = TRUE,
  comment    = NA,
  warning    = FALSE,
  message    = FALSE,
  fig.width  = 6.5,
  fig.height = 4.5,
  fig.align  = "center",
  eval       = !.fast,
  cache      = FALSE
)
options(prompt = "R> ", continue = "+  ", width = 70,
        useFancyQuotes = FALSE, digits = 4)

## Versions reported in "Computational details", read at render time rather
## than typed: the package version from the DESCRIPTION this paper ships
## with, and RSiena and R from the session that renders it.
.desc <- file.path("..", "DESCRIPTION")
sn_version <- if (file.exists(.desc)) {
  unname(read.dcf(.desc, fields = "Version")[1, 1])
} else {
  as.character(utils::packageVersion("searchnet"))
}
rsiena_version <- as.character(utils::packageVersion("RSiena"))
r_version <- paste(R.version$major, R.version$minor, sep = ".")

## Package-size counts reported in the text, read from the repository at
## render time. Paths listed in .public-exclude do not ship in the public
## package and are not counted.
.excl <- file.path("..", ".public-exclude")
.excl <- if (file.exists(.excl)) trimws(readLines(.excl, warn = FALSE)) else ""
.shipped <- function(dir, pattern) {
  f <- list.files(file.path("..", dir), pattern = pattern)
  sum(!file.path(dir, f) %in% .excl)
}
n_vignettes  <- .shipped("vignettes", "\\.Rmd$")
n_test_files <- .shipped("tests/testthat", "^test.*\\.R$")
## From the repository when rendered there; otherwise (e.g. code.R run from
## the replication bundle) from the installed package's copy.
.ptf <- file.path("..", "inst", "proofs", "PROOF_TABLE.md")
if (!file.exists(.ptf))
  .ptf <- system.file("proofs", "PROOF_TABLE.md", package = "searchnet")
.pt <- if (nzchar(.ptf) && file.exists(.ptf)) readLines(.ptf, warn = FALSE) else character()
## One row per step; the row ID is a part letter and a number (A1, L18).
n_proof_steps <- sum(grepl("^\\|\\s*[A-Z][0-9]+\\s*\\|", .pt))


#+ load-packages
library(searchnet)
library(ggplot2)
library(Matrix)


#+ api-env
env <- saomnk_env(M = 8, N = 12, density = 0, seed = 42)


#+ api-model
## Define a modular influence matrix: 4 blocks of 3 components
K_matrix <- saomnk_block_diagonal(12, 4)

## Specify the model
mod <- saomnk_model(
  density    = -0.5,     # sparsity pressure
  popularity =  0.2,     # preferential attachment
  scope      =  0.1,     # scope expansion
  influence_matrix = K_matrix,
  influence_weight = 0.05
)
mod


#+ api-model-strat
## Heterogeneous actors: varying scope and popularity preferences
mod_strat <- saomnk_model(
  density    = -0.3,
  popularity =  0.2,
  scope      =  0.1,
  influence_matrix = K_matrix,
  influence_weight = 0.05,
  strategies = list(
    egoX   = rep(c(-1, 0, 1, 0), length.out = 8),
    inPopX = rep(c( 1, 0,-1, 0), length.out = 8)
  )
)


#+ api-run
saomnk_run(env, mod, steps_per_actor = 30, seed = 12345)


#+ k4-panel
saomnk_plot_k4(env, smooth = 0.3)


#+ snapshots
n_steps <- env$get_step()
saomnk_plot_snapshots(env,
  steps = unique(pmax(1, round(c(0, 0.5, 1) * n_steps))))


#+ utility
saomnk_plot_utility(env, smooth = 0.35)


#+ calibration-example
# ## Convert fitted SAOM parameters to SaoMNK
# bridge <- saom_to_saomnk(saom_fit, scale_factor = 1.0)
# print(bridge$mapping_table)
# 
# ## Initialize environment from empirical data
# env_data <- empirical_to_saomnk_env(mi_data, wave = 5)
# 
# ## Run calibrated counterfactual
# result <- run_calibrated_counterfactual(
#   env_data, bridge,
#   scenario = list(name = "double_closure",
#                   modify = list(transTriads = 2.0)),
#   iterations = 50
# )
# cat(sprintf("K_AC change: %.3f\n", result$comparison$delta_K_AC))


#+ nk-verify
nk <- nk_landscape(N = 10, K = 3, seed = 42)
nk_verify_reduction(N = 10, K = 3, seed = 42)


#+ levinthal-setup
## Two INDEPENDENT searchers (popularity = scope = 0 removes all
## coupling), 12 components, empty start.  M >= 2 is required by
## RSiena's bipartite data constructor; with the interaction terms
## zeroed the actors do not influence one another, so each is a
## classical NK searcher.
env_nk <- saomnk_env(M = 2, N = 12, density = 0, seed = 1234)

## Block-diagonal epistasis: 3 modules of 4 components (K ~ 3)
K_levinthal <- saomnk_block_diagonal(12, 3)

## Only epistasis active; no endogenous effects
mod_nk <- saomnk_model(
  density    = -0.3,
  popularity = 0,
  scope      = 0,
  influence_matrix = K_levinthal,
  influence_weight = 0.15
)

saomnk_run(env_nk, mod_nk, steps_per_actor = 100, seed = 42)


#+ levinthal-degrees
env_nk$plot_actor_degrees(loess_span = 0.5)


#+ ergodicity
erg <- searchnet_ergodicity_sweep(
  M = 12, N = 15, start_densities = c(0.1, 0.8),
  run_lengths = c(15, 30, 60, 120, 240),
  replicates = 10, equivalence_margin = 0.05, seed = 42
)
erg
plot(erg)


#+ shock-setup
env_base <- saomnk_env(M = 6, N = 8, density = 0, seed = 42)
env_shock <- saomnk_env(M = 6, N = 8, density = 0, seed = 42)

K_small <- saomnk_block_diagonal(8, 2)
mod_base <- saomnk_model(
  density = -0.5,
  influence_matrix = K_small,
  influence_weight = 0.5
)

## Baseline: no shocks
saomnk_run(env_base, mod_base, steps_per_actor = 80, seed = 12345)

## Shocked: density drops from -0.5 to -2.0 at midpoint
shock_baseline <- saomnk_shock("density", parameter = -0.5,
                               portion = 1)
shock_event    <- saomnk_shock("density", parameter = -2.0,
                               portion = 1)

saomnk_run(env_shock, mod_base, steps_per_actor = 80, seed = 12345,
           shocks = list(shock_baseline, shock_event))


#+ shock-comparison
## Display baseline K-4 panel
saomnk_plot_k4(env_base, smooth = 0.3)


#+ shock-treatment
## Display shocked K-4 panel
saomnk_plot_k4(env_shock, smooth = 0.3)


#+ causal-panel
## Treated arm: the six actors of the shocked run (env_shock, above).
## Comparison arm: the six actors of the unshocked run (env_base), which
## shares the initial network and the run seed. did::att_gt() needs at
## least five never-treated units; ids 7-12 keep the arms' units distinct.
shock_step <- min(env_shock$theta_shocks[[2]]$chain_step_ids)
panel_tr <- searchnet_causal_panel(env_shock, shock_step = shock_step,
                                   outcome = "utility")
panel_co <- searchnet_causal_panel(env_base, shock_step = shock_step,
                                   outcome = "utility",
                                   treated_actors = integer(0))
panel_co$actor_id <- factor(as.integer(as.character(panel_co$actor_id)) + 6L)

## Keep the steps both arms reach, so the panel is balanced.
common <- seq_len(min(max(panel_tr$step), max(panel_co$step)))
panel <- rbind(panel_tr[panel_tr$step %in% common, ],
               panel_co[panel_co$step %in% common, ])
str(panel)


#+ causal-did
did_result <- searchnet_did(panel)
did::aggte(did_result, type = "simple")
searchnet_causal_plot(did_result)


#+ basin-landscape
# env <- SaomNkRSienaBiEnv$new(list(M = 8, N = 8, BI_PROB = 0.4,
#                                   rand_seed = 42))
# env$compute_fitness_landscape(n_landscapes = 1)
# 
# ## Extract landscape data
# landscape <- env$fitness_landscape[1,,]
# N <- env$N
# configs  <- landscape[, 1:N]
# fitness  <- landscape[, 2*N + 1]
# is_peak  <- landscape[, 2*N + 2] == 1


#+ app-variance-spec
# env <- saomnk_env(M = M, N = N, density = 0.2, seed = seed)
# ## arch_type: "modular", "hierarchical" or "random"
# K   <- make_influence_matrix(N, arch_type)
# 
# mod <- saomnk_model(density = -0.5, popularity = coupling,
#                     scope = 0.05, influence_matrix = K,
#                     influence_weight = 0.1)
# 
# saomnk_run(env, mod, steps_per_actor = 15L, seed = seed + 1L)
# bi_mat <- saomnk_get_bipartite(env)


#+ bd-verification
# library(searchnet)
# 
# # Option C regime: J_b modest, h_b symmetric -> m* ~= 0.5
# result <- verify_brock_durlauf_reduction(
#   M_seq = c(20, 50, 100),
#   J_b = 0.5, h_b = -0.25,        # symmetric, sub-threshold
#   beta = 1,
#   n_components = 8,
#   n_steps = 100,                  # 100 ministeps per actor
#   n_replicates = 3
# )
# print(result[, c("M", "m_b_empirical", "m_BD_analytical",
#                  "m_saomnk_analytical", "in_BD_regime",
#                  "abs_error_BD", "abs_error_saomnk")])
# # in_BD_regime = TRUE everywhere; abs_error_BD ~ 0.01-0.05


#+ benchmark-overhead
## --- searchnet wrapper timing (M=4, N=6, 30 steps/actor) ---
time_searchnet <- system.time({
  e_bench <- saomnk_env(M = 4, N = 6, seed = 1)
  m_bench <- saomnk_model(
    density = -0.5,
    influence_matrix = saomnk_block_diagonal(6, 2)
  )
  saomnk_run(e_bench, m_bench, steps_per_actor = 30, seed = 1)
})

## --- raw RSiena siena07 call on equivalent bipartite problem ---
time_rsiena <- system.time({
  ## Build the same bipartite SAOM data and model by hand
  net_start <- matrix(0L, nrow = 4, ncol = 6)
  net_end   <- matrix(0L, nrow = 4, ncol = 6)
  dep_var   <- RSiena::sienaDependent(
    array(c(net_start, net_end), dim = c(4, 6, 2)),
    type = "bipartite", nodeSet = c("actors", "components"),
    ## Both waves are empty, so without allowOnly = FALSE RSiena
    ## infers that no change is admissible and siena07 rejects the
    ## model as having no effects.
    allowOnly = FALSE
  )
  actors_set <- RSiena::sienaNodeSet(4, nodeSetName = "actors")
  comp_set   <- RSiena::sienaNodeSet(6, nodeSetName = "components")
  dat_raw    <- RSiena::sienaDataCreate(
    dep_var,
    nodeSets = list(actors_set, comp_set)
  )
  ## getEffects() already includes the rate and density terms for
  ## a bipartite dependent variable: the specification being timed.
  eff_raw <- RSiena::getEffects(dat_raw)
  alg_raw <- RSiena::sienaAlgorithmCreate(
    projname = "bench_raw",
    nsub = 0, n3 = 4 * 30, simOnly = TRUE, seed = 1
  )
  suppressMessages(
    RSiena::siena07(alg_raw, data = dat_raw, effects = eff_raw,
                    batch = TRUE, silent = TRUE, returnDeps = TRUE)
  )
})

overhead_pct <- round(
  100 * (time_searchnet["elapsed"] - time_rsiena["elapsed"]) /
    time_rsiena["elapsed"], 1
)
cat(sprintf(
  "searchnet: %.2fs | raw RSiena: %.2fs | overhead: %.1f%%\n",
  time_searchnet["elapsed"], time_rsiena["elapsed"], overhead_pct
))


#+ landscape-scaling
## Landscape timing: full enumeration vs. sampled approximation
landscape_sizes <- c(6, 8, 10, 12)
landscape_times <- data.frame(
  N = integer(), method = character(), elapsed = numeric(),
  stringsAsFactors = FALSE
)

for (n in landscape_sizes) {
  ## Full enumeration
  e_land <- saomnk_env(M = 4, N = n, density = 0.3, seed = 42)
  m_land <- saomnk_model(
    density = -0.5,
    influence_matrix = saomnk_block_diagonal(n, max(2, n %/% 3)),
    influence_weight = 0.05
  )
  saomnk_run(e_land, m_land, steps_per_actor = 10, seed = 1)

  t_full <- system.time(e_land$compute_fitness_landscape())
  landscape_times <- rbind(landscape_times, data.frame(
    N = n, method = "full", elapsed = t_full["elapsed"]
  ))

  ## Sampled approximation (S = 1000)
  t_samp <- system.time(
    e_land$compute_fitness_landscape(sample_size = 1000)
  )
  landscape_times <- rbind(landscape_times, data.frame(
    N = n, method = "sample_1000", elapsed = t_samp["elapsed"]
  ))
}

## Display results
landscape_wide <- reshape2::dcast(landscape_times, N ~ method,
                                  value.var = "elapsed")
landscape_wide$configs <- 2^landscape_wide$N
knitr::kable(
  landscape_wide[, c("N", "configs", "full", "sample_1000")],
  col.names = c("$N$", "Configurations", "Full (s)", "Sample (s)"),
  digits = 3,
  caption = paste("Fitness landscape computation time: full",
                  "enumeration vs.\\ sampled approximation",
                  "($S = 1{,}000$).")
)


#' ## Session information
#+ session-info
sessionInfo()
