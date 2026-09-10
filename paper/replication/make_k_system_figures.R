#!/usr/bin/env Rscript
# =============================================================================
# make_k_system_figures.R
#
# Generates the two simulated {K}-system demonstration figures used in
# "searchnet: Network-Embedded Strategic Search Simulation in R" (JSS):
#
#   paper/figures/fig_k_coupling_sim.png      coupled degree dynamics
#   paper/figures/fig_k_shock_relocation.png  a shock relocates the system
#
# Both figures are produced entirely by searchnet from seeded simulations.
# They contain no empirical data and support no empirical claim.
#
# Runtime: well under one minute on a standard workstation.
#
# Standalone use:
#   Rscript paper/replication/make_k_system_figures.R
# It is also sourced by reproduce_all.R.
# =============================================================================

suppressPackageStartupMessages({
  library(searchnet)
  library(ggplot2)
})

## --- Output directory: paper/figures/, resolved relative to this script -----
.this_file <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) {
    normalizePath(f, mustWork = FALSE)
  } else {
    o <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
    if (!is.null(o)) normalizePath(o, mustWork = FALSE) else NA_character_
  }
})
fig_out <- if (!is.na(.this_file)) {
  file.path(dirname(dirname(.this_file)), "figures")
} else {
  file.path("..", "figures")
}
if (!dir.exists(fig_out)) dir.create(fig_out, recursive = TRUE)

K_DIMS   <- c("K_AC", "K_CA", "K_AA", "K_CC")
K_LABELS <- c(K_AC = "K[AC]~(actor~scope)",
              K_CA = "K[CA]~(component~popularity)",
              K_AA = "K[AA]~(actor~sociality)",
              K_CC = "K[CC]~(component~epistasis)")

## Mean of each {K} dimension at each step of the simulated chain.
k_means <- function(env) {
  d <- saomnk_get_degrees(env)
  res <- do.call(rbind, lapply(K_DIMS, function(k) {
    df <- as.data.frame(d[[k]])
    ag <- stats::aggregate(list(value = df$value),
                           by = list(step = df$chain_step_id),
                           FUN = mean, na.rm = TRUE)
    ag$dimension <- k
    ag
  }))
  res$dimension <- factor(res$dimension, levels = K_DIMS,
                          labels = unname(K_LABELS[K_DIMS]))
  res
}

theme_jss <- function() {
  theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey92", color = NA),
          legend.position  = "bottom",
          legend.title     = element_blank())
}

## Shared design: 20 actors, 24 components, block-diagonal influence matrix.
SIM_M <- 20
SIM_N <- 24
SIM_STEPS <- 40

make_model <- function(density = -5.0, popularity = 0.15) {
  saomnk_model(density          = density,
               popularity       = popularity,
               scope            = 0.05,
               influence_matrix = saomnk_block_diagonal(SIM_N, 4),
               influence_weight = 0.05)
}

# =============================================================================
# FIGURE A: coupled degree dynamics
# One seeded run. All four dimensions are read off the same bipartite
# incidence matrix, so they rise together from an empty start.
# =============================================================================
cat("--- Figure A: coupled degree dynamics ---\n")
tA <- proc.time()

set.seed(20260909)
env_cpl <- saomnk_env(M = SIM_M, N = SIM_N, density = 0, seed = 20260909)
saomnk_run(env_cpl, make_model(), steps_per_actor = SIM_STEPS, seed = 20260909)

km <- k_means(env_cpl)

p_cpl <- ggplot(km, aes(x = step, y = value,
                        color = dimension, linetype = dimension)) +
  geom_line(linewidth = 0.65) +
  scale_color_brewer(palette = "Dark2", labels = scales::parse_format()) +
  scale_linetype_manual(values = c("solid", "22", "42", "1343"),
                        labels = scales::parse_format()) +
  labs(x = "Simulated micro-step", y = "Mean degree") +
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
  theme_jss()

ggsave(file.path(fig_out, "fig_k_coupling_sim.png"), p_cpl,
       width = 7.0, height = 4.4, dpi = 300)

## Coupling statistics quoted in the manuscript caption.
wide <- stats::reshape(km, direction = "wide", idvar = "step",
                       timevar = "dimension", v.names = "value")
cmat <- stats::cor(wide[, -1])
cat(sprintf("  terminal means: %s\n",
            paste(sprintf("%s=%.2f", K_DIMS,
                          sapply(K_DIMS, function(k)
                            km$value[km$dimension == unname(K_LABELS[k]) &
                                       km$step == max(km$step)])),
                  collapse = "  ")))
cat(sprintf("  minimum pairwise correlation across the four series: %.3f\n",
            min(cmat[upper.tri(cmat)])))
cat("  Time:", round((proc.time() - tA)["elapsed"], 2), "seconds\n")
cat("  Saved: fig_k_coupling_sim.png\n\n")

# =============================================================================
# FIGURE B: a shock relocates the coupled system
# Three arms from identical starting conditions and identical seeds. The two
# treated arms receive an exogenous parameter change at the midpoint. Before
# the shock the arms coincide exactly; after it they separate in opposite
# directions.
# =============================================================================
cat("--- Figure B: shock relocation ---\n")
tB <- proc.time()

run_arm <- function(shocks = NULL) {
  set.seed(77001)
  e <- saomnk_env(M = SIM_M, N = SIM_N, density = 0, seed = 77001)
  saomnk_run(e, make_model(), steps_per_actor = SIM_STEPS, seed = 77001,
             shocks = shocks)
  e
}

arms <- list(
  "Control (no shock)" = run_arm(NULL),
  "Sparsity shock" = run_arm(list(saomnk_shock("density", -5.0, 1),
                                  saomnk_shock("density", -6.5, 1))),
  "Popularity shock" = run_arm(list(saomnk_shock("popularity", 0.15, 1),
                                    saomnk_shock("popularity", 1.20, 1)))
)

km2 <- do.call(rbind, lapply(names(arms), function(nm) {
  x <- k_means(arms[[nm]]); x$arm <- nm; x
}))
km2$arm <- factor(km2$arm, levels = names(arms))

shock_step <- max(km2$step) / 2

p_shk <- ggplot(km2, aes(x = step, y = value, color = arm, linetype = arm)) +
  geom_vline(xintercept = shock_step, linetype = "dotted",
             color = "grey40", linewidth = 0.4) +
  geom_line(linewidth = 0.65) +
  facet_wrap(~dimension, scales = "free_y", ncol = 2,
             labeller = label_parsed) +
  scale_color_manual(values = c("grey35", "#1B9E77", "#D95F02")) +
  scale_linetype_manual(values = c("solid", "42", "22")) +
  labs(x = "Simulated micro-step", y = "Mean degree") +
  theme_jss()

ggsave(file.path(fig_out, "fig_k_shock_relocation.png"), p_shk,
       width = 7.0, height = 5.0, dpi = 300)

## Terminal values by arm, quoted in the manuscript caption.
term <- do.call(rbind, lapply(K_DIMS, function(k) {
  lab <- unname(K_LABELS[k])
  s <- km2[km2$dimension == lab & km2$step == max(km2$step), ]
  data.frame(dimension = k,
             control    = s$value[s$arm == "Control (no shock)"],
             sparsity   = s$value[s$arm == "Sparsity shock"],
             popularity = s$value[s$arm == "Popularity shock"])
}))
cat("  Terminal means by arm:\n")
print(term, row.names = FALSE, digits = 3)
cat("  Time:", round((proc.time() - tB)["elapsed"], 2), "seconds\n")
cat("  Saved: fig_k_shock_relocation.png\n\n")

cat("Figures written to:", normalizePath(fig_out), "\n")
