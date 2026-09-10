#!/usr/bin/env Rscript
# =============================================================================
# reproduce_all.R
# Replication script for: "searchnet: Network-Embedded Strategic Search
# Simulation in R" (Journal of Statistical Software)
#
# Reproduces all manuscript illustrations and saves figures as PDF.
# Total runtime: < 5 minutes on a standard workstation.
# =============================================================================

cat("=================================================================\n")
cat("  searchnet JSS Replication Script\n")
cat("  Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=================================================================\n\n")

total_start <- proc.time()

## Locate this script whether it is run with Rscript or with source().
## (sys.frame(1)$ofile is only defined under source(); using it alone made
## this script fail under `Rscript reproduce_all.R`.)
.script_dir <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) return(dirname(normalizePath(f, mustWork = FALSE)))
  o <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(o)) dirname(normalizePath(o, mustWork = FALSE)) else "."
})

# ---------------------------------------------------------------------------
# 0. Setup: install/load searchnet
# ---------------------------------------------------------------------------
cat("--- Step 0: Loading packages ---\n")

if (!requireNamespace("searchnet", quietly = TRUE)) {
  cat("searchnet not found. Attempting local install...\n")
  pkg_dir <- normalizePath(file.path(.script_dir, "..", ".."),
                           mustWork = FALSE)
  if (file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
    install.packages(pkg_dir, repos = NULL, type = "source")
  } else {
    stop("Cannot find searchnet package. Please install it first.\n",
         "  install.packages('path/to/SaoMNK', repos = NULL, type = 'source')")
  }
}

library(searchnet)
library(ggplot2)
library(Matrix)

# Create output directory
fig_dir <- file.path(.script_dir, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

cat("  searchnet version:", as.character(packageVersion("searchnet")), "\n")
cat("  Output directory:", fig_dir, "\n\n")

options(digits = 4)

# =============================================================================
# ILLUSTRATION 1: Levinthal Replication (Section 5.1)
# Independent searchers on an exogenous NK landscape, M=2, N=12.
# M >= 2 is required by RSiena's bipartite data constructor; with popularity
# and scope zeroed the two actors do not influence one another, so each is a
# classical NK searcher.  These are the values in the manuscript chunk.
# =============================================================================
cat("--- Illustration 1: Levinthal Replication (M=2, N=12) ---\n")
t1 <- proc.time()

set.seed(1234)
env_nk <- saomnk_env(M = 2, N = 12, density = 0, seed = 1234)

# Block-diagonal influence matrix: 3 modules of 4 components (K ~ 3)
K_levinthal <- saomnk_block_diagonal(12, 3)

# Only the influence term active; no endogenous effects
mod_nk <- saomnk_model(
  density    = -0.3,
  popularity = 0,
  scope      = 0,
  influence_matrix = K_levinthal,
  influence_weight = 0.15
)

saomnk_run(env_nk, mod_nk, steps_per_actor = 100, seed = 42)

# Figure 1: Actor scope trajectory
pdf(file.path(fig_dir, "fig_levinthal_scope.pdf"), width = 6.5, height = 3.5)
env_nk$plot_actor_degrees(loess_span = 0.5)
dev.off()

t1_elapsed <- (proc.time() - t1)["elapsed"]
cat("  Time:", round(t1_elapsed, 2), "seconds\n")
cat("  Saved: fig_levinthal_scope.pdf\n\n")


# =============================================================================
# ILLUSTRATION 2: Endogenous Landscape (Section 5.2)
# 12 actors, 12 components, heterogeneous strategies
# =============================================================================
cat("--- Illustration 2: Endogenous Landscape (M=12, N=12) ---\n")
t2 <- proc.time()

set.seed(1234)
env_endog <- saomnk_env(M = 12, N = 12, density = 0, seed = 1234)

K_modular <- saomnk_block_diagonal(12, 4)

mod_endog <- saomnk_model(
  density    = -0.3,
  popularity =  0.2,
  scope      =  0.1,
  influence_matrix = K_modular,
  influence_weight = 0.02,
  strategies = list(
    egoX   = rep(c(-1, 0, 1), length.out = 12),
    inPopX = rep(c( 1, 0,-1), length.out = 12)
  )
)

saomnk_run(env_endog, mod_endog, steps_per_actor = 30, seed = 12345)

# Figure 2: {K}-4 panel
pdf(file.path(fig_dir, "fig_endogenous_k4.pdf"), width = 8, height = 8)
saomnk_plot_k4(env_endog, smooth = 0.3)
dev.off()

# Figure 3: Utility decomposition
pdf(file.path(fig_dir, "fig_endogenous_utility.pdf"), width = 8, height = 5)
saomnk_plot_utility(env_endog, smooth = 0.35)
dev.off()

# Figure 4: Network snapshots
pdf(file.path(fig_dir, "fig_endogenous_snapshots.pdf"), width = 10, height = 4)
saomnk_plot_snapshots(env_endog, steps = c(1, 120, 240))
dev.off()

t2_elapsed <- (proc.time() - t2)["elapsed"]
cat("  Time:", round(t2_elapsed, 2), "seconds\n")
cat("  Saved: fig_endogenous_k4.pdf, fig_endogenous_utility.pdf,",
    "fig_endogenous_snapshots.pdf\n\n")


# =============================================================================
# ILLUSTRATION 3: Exogenous Shocks (Section 5.3)
# Baseline vs. treatment with density shock, M=6, N=8
# =============================================================================
cat("--- Illustration 3: Exogenous Shocks (M=6, N=8) ---\n")
t3 <- proc.time()

set.seed(42)
env_base  <- saomnk_env(M = 6, N = 8, density = 0, seed = 42)
env_shock <- saomnk_env(M = 6, N = 8, density = 0, seed = 42)

K_small <- saomnk_block_diagonal(8, 2)
mod_base <- saomnk_model(
  density = -0.5,
  influence_matrix = K_small,
  influence_weight = 0.5
)

# Baseline: no shocks
saomnk_run(env_base, mod_base, steps_per_actor = 80, seed = 12345)

# Shocked: density drops from -0.5 to -2.0 at midpoint
shock_baseline <- saomnk_shock("density", parameter = -0.5, portion = 1)
shock_event    <- saomnk_shock("density", parameter = -2.0, portion = 1)

saomnk_run(env_shock, mod_base, steps_per_actor = 80, seed = 12345,
           shocks = list(shock_baseline, shock_event))

# Figure 5: Baseline {K}-4 panel
pdf(file.path(fig_dir, "fig_shock_baseline_k4.pdf"), width = 8, height = 8)
saomnk_plot_k4(env_base, smooth = 0.3)
dev.off()

# Figure 6: Shocked {K}-4 panel
pdf(file.path(fig_dir, "fig_shock_treatment_k4.pdf"), width = 8, height = 8)
saomnk_plot_k4(env_shock, smooth = 0.3)
dev.off()

t3_elapsed <- (proc.time() - t3)["elapsed"]
cat("  Time:", round(t3_elapsed, 2), "seconds\n")
cat("  Saved: fig_shock_baseline_k4.pdf, fig_shock_treatment_k4.pdf\n\n")


# =============================================================================
# API DEMONSTRATION (Section 4.2--4.3)
# Reproduces the code from the User API section
# =============================================================================
cat("--- API Demonstration (M=8, N=12) ---\n")
t4 <- proc.time()

set.seed(42)
env <- saomnk_env(M = 8, N = 12, density = 0, seed = 42)

K_matrix <- saomnk_block_diagonal(12, 4)

mod <- saomnk_model(
  density    = -0.5,
  popularity =  0.2,
  scope      =  0.1,
  influence_matrix = K_matrix,
  influence_weight = 0.05
)

saomnk_run(env, mod, steps_per_actor = 30, seed = 12345)

# Figure 7: API {K}-4 panel
pdf(file.path(fig_dir, "fig_api_k4.pdf"), width = 8, height = 8)
saomnk_plot_k4(env, smooth = 0.3)
dev.off()

# Figure 8: API utility decomposition
pdf(file.path(fig_dir, "fig_api_utility.pdf"), width = 8, height = 5)
saomnk_plot_utility(env, smooth = 0.35)
dev.off()

t4_elapsed <- (proc.time() - t4)["elapsed"]
cat("  Time:", round(t4_elapsed, 2), "seconds\n")
cat("  Saved: fig_api_k4.pdf, fig_api_utility.pdf\n\n")


# =============================================================================
# NK EQUIVALENCE VERIFICATION (Section 4.6)
# Reproduces the nk-verify chunk of the manuscript: rebuilding each locus's
# payoff table keyed by its neighborhood pattern and recomputing fitness must
# return the original values exactly.
# =============================================================================
cat("--- NK Equivalence Verification ---\n")
t5 <- proc.time()

nk <- nk_landscape(N = 10, K = 3, seed = 42)
nk_check <- nk_verify_reduction(N = 10, K = 3, seed = 42)

cat("\n  Verification checks:\n")
cat("    [1] Configurations enumerated:", nk_check$n_configs, "\n")
cat("    [2] Max absolute difference:  ",
    format(nk_check$max_difference, scientific = TRUE), "\n")
cat("    [3] Reduction check passed:   ", nk_check$passed, "\n")
stopifnot(nk_check$passed)

# Degree extraction on a runnable environment (M >= 2), for the {K} readout
# used throughout the manuscript.
degrees <- saomnk_get_degrees(env)
cat("\n  Actor scope (K_AC) range:",
    range(degrees$K_AC$value, na.rm = TRUE), "\n")
cat("  Component epistasis (K_CC) range:",
    range(degrees$K_CC$value, na.rm = TRUE), "\n")

t5_elapsed <- (proc.time() - t5)["elapsed"]
cat("  Time:", round(t5_elapsed, 2), "seconds\n\n")


# =============================================================================
# {K}-SYSTEM DEMONSTRATION FIGURES (Sections 3.2 and 5.3)
# Regenerates the two figures the manuscript includes as PNG files:
#   paper/figures/fig_k_coupling_sim.png
#   paper/figures/fig_k_shock_relocation.png
# =============================================================================
cat("--- {K}-system demonstration figures ---\n")
t6 <- proc.time()

source(file.path(.script_dir, "make_k_system_figures.R"))

t6_elapsed <- (proc.time() - t6)["elapsed"]
cat("  Time:", round(t6_elapsed, 2), "seconds\n\n")


# =============================================================================
# SUMMARY
# =============================================================================
total_elapsed <- (proc.time() - total_start)["elapsed"]

cat("=================================================================\n")
cat("  Replication Complete\n")
cat("  Total time:", round(total_elapsed, 1), "seconds\n")
cat("  Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("-----------------------------------------------------------------\n")
cat("  Illustration 1 (Levinthal):  ", round(t1_elapsed, 1), "s\n")
cat("  Illustration 2 (Endogenous): ", round(t2_elapsed, 1), "s\n")
cat("  Illustration 3 (Shocks):     ", round(t3_elapsed, 1), "s\n")
cat("  API Demo:                    ", round(t4_elapsed, 1), "s\n")
cat("  NK Equivalence:              ", round(t5_elapsed, 1), "s\n")
cat("  {K}-system figures:          ", round(t6_elapsed, 1), "s\n")
cat("=================================================================\n")
cat("\nAll figures saved to:", fig_dir, "\n")
