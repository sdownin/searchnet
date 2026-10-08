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

## --- Load searchnet: the source tree if run from it, else the installed one,
## as make_figure1_mental_model.R does (the plot style, theme_searchnet(), is
## exported from 0.11.2.9000 on).
if (!"package:searchnet" %in% search()) {
  .pkg_root <- if (!is.na(.this_file)) dirname(dirname(dirname(.this_file))) else file.path("..", "..")
  .desc <- file.path(.pkg_root, "DESCRIPTION")
  if (file.exists(.desc) && grepl("^Package: searchnet", readLines(.desc, n = 1)) &&
      requireNamespace("pkgload", quietly = TRUE)) {
    suppressMessages(pkgload::load_all(.pkg_root, quiet = TRUE))
  } else {
    suppressPackageStartupMessages(library(searchnet))
  }
}
suppressPackageStartupMessages(library(ggplot2))
fig_out <- if (!is.na(.this_file)) {
  file.path(dirname(dirname(.this_file)), "figures")
} else {
  file.path("..", "figures")
}
if (!dir.exists(fig_out)) dir.create(fig_out, recursive = TRUE)

K_DIMS   <- c("K_AC", "K_CA", "K_AA", "K_CC")
## Channel labels as the package draws them: symbol and plain name.
K_LABELS <- c(K_AC = 'K[AC]*"  scope"',
              K_CA = 'K[CA]*"  popularity"',
              K_AA = 'K[AA]*"  sociality"',
              K_CC = 'K[CC]*"  coupling"')

## Visual grammar of the package plots (R/plot-readable.R): actor channels
## orange, component channels blue; direct ties solid, links through a shared
## partner dashed; the event vermillion; context grey.
OI <- c(orange = "#E69F00", blue = "#0072B2", vermillion = "#D55E00",
        grey = "grey45", ink = "grey20")
K_COLS  <- c(K_AC = OI[["orange"]], K_CA = OI[["blue"]],
             K_AA = OI[["orange"]], K_CC = OI[["blue"]])
K_LTY   <- c(K_AC = "solid", K_CA = "solid", K_AA = "22", K_CC = "22")

## Spread end-of-line labels so that none sits closer than `gap` to another.
spread <- function(y, gap) {
  o <- order(y); z <- y[o]
  for (i in seq_along(z)[-1]) z[i] <- max(z[i], z[i - 1] + gap)
  z <- z - (mean(z) - mean(y[o]))
  y[o] <- z
  y
}

## Mean of each {K} dimension at each step of the simulated chain.
k_means <- function(env) {
  d <- saomnk_get_degrees(env)
  res <- do.call(rbind, lapply(K_DIMS, function(k) {
    df <- as.data.frame(d[[k]])
    ag <- stats::aggregate(list(value = df$value),
                           by = list(step = df$chain_step_id),
                           FUN = mean, na.rm = TRUE)
    ag$dimension <- k
    ag$k <- k
    ag
  }))
  res$dimension <- factor(res$dimension, levels = K_DIMS,
                          labels = unname(K_LABELS[K_DIMS]))
  res
}

theme_jss <- function() {
  theme_searchnet(base_size = 10) +
    theme(strip.text  = element_text(hjust = 0, size = 9),
          legend.position = "none")
}

## Shared design: 20 actors, 24 components, block-diagonal influence matrix.
SIM_M <- 20
SIM_N <- 24
SIM_STEPS <- 40

## Baselines. Since 0.11.0 runs are genuine state-carrying paths, and the
## original baseline (density -5.0, popularity 0.15) keeps the network nearly
## empty. The coupling figure uses the point chosen by a pre-registered,
## seeded search on regime criteria only (final density in [0.15, 0.60], at
## most 2 actors at degree 0 or N, at least 3 tie changes per actor, on three
## seeds): see paper/replication/reparam/. No baseline on that search's grid
## met the regime criteria for the shock figure with the original shock
## sizes, so the shock figure keeps the original baseline.
COUPLING_DENSITY    <- -2.0
COUPLING_POPULARITY <- 0.15
SHOCK_DENSITY       <- -5.0
SHOCK_POPULARITY    <- 0.15
SPARSITY_TO         <- -6.5    # density after the sparsity shock
POPULARITY_TO       <- 1.20    # popularity after the popularity shock

make_model <- function(density = SHOCK_DENSITY, popularity = SHOCK_POPULARITY) {
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
saomnk_run(env_cpl, make_model(COUPLING_DENSITY, COUPLING_POPULARITY),
           steps_per_actor = SIM_STEPS, seed = 20260909)

km <- k_means(env_cpl)

## Coupling statistics quoted in the manuscript caption (also used for the
## figure title below).
wide <- stats::reshape(km[, c("step", "value", "dimension")], direction = "wide",
                       idvar = "step", timevar = "dimension", v.names = "value")
cmat <- stats::cor(wide[, -1])

## Lines labeled at their right ends instead of a legend.
endA <- do.call(rbind, lapply(split(km, km$k), function(d) d[d$step == max(d$step), ]))
endA$y_lab <- spread(endA$value, gap = 0.06 * diff(range(km$value)))
endA$lab <- unname(K_LABELS[endA$k])
x_max <- max(km$step)
## Title from the run: the four series move together when every pairwise
## correlation is high.
min_cor <- min(cmat[upper.tri(cmat)])
title_A <- if (min_cor >= 0.9) "All four {K} degrees rise together" else
  "The four {K} degrees over one run"
## Note: which pair ends highest, computed from the terminal means.
proj_high <- min(endA$value[endA$k %in% c("K_AA", "K_CC")]) >
  max(endA$value[endA$k %in% c("K_AC", "K_CA")])
p_cpl <- ggplot(km, aes(x = step, y = value, group = k)) +
  geom_line(aes(color = k, linetype = k), linewidth = 0.7) +
  geom_segment(data = endA, aes(x = step, xend = x_max * 1.015, y = value, yend = y_lab,
                                color = k), linewidth = 0.3) +
  geom_text(data = endA, aes(x = x_max * 1.02, y = y_lab, label = lab, color = k),
            parse = TRUE, hjust = 0, size = 3.1) +
  scale_color_manual(values = K_COLS, guide = "none") +
  scale_linetype_manual(values = K_LTY, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.2))) +
  coord_cartesian(clip = "off") +
  labs(title = title_A,
       subtitle = paste0("Mean degree at each ministep of one seeded run (", SIM_M,
                         " actors, ", SIM_N, " components).\n",
                         "Orange: actor channels; blue: component channels. ",
                         "Solid: direct ties; dashed: links through a shared partner."),
       x = "Ministep (one decision opportunity)", y = "Mean degree") +
  theme_jss()
if (proj_high) {
  ## Arrow to the dashed K_AA line two thirds of the way along.
  xa <- round(0.62 * x_max)
  ya <- km$value[km$k == "K_AA" & km$step == xa][1]
  p_cpl <- p_cpl +
    annotate("segment", x = xa - 0.12 * x_max, xend = xa - 0.01 * x_max,
             y = ya + 0.22 * max(km$value), yend = ya + 0.02 * max(km$value),
             color = OI[["ink"]], linewidth = 0.35,
             arrow = arrow(length = unit(0.07, "in"), type = "closed")) +
    annotate("text", x = xa - 0.13 * x_max, y = ya + 0.23 * max(km$value),
             hjust = 1, vjust = 0, size = 3.1, color = OI[["ink"]], lineheight = 0.95,
             label = "links through a shared partner\n(dashed) end highest")
}

ggsave(file.path(fig_out, "fig_k_coupling_sim.png"), p_cpl,
       width = 7.0, height = 4.4, dpi = 300)
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
  "Sparsity shock" = run_arm(list(saomnk_shock("density", SHOCK_DENSITY, 1),
                                  saomnk_shock("density", SPARSITY_TO, 1))),
  "Popularity shock" = run_arm(list(saomnk_shock("popularity", SHOCK_POPULARITY, 1),
                                    saomnk_shock("popularity", POPULARITY_TO, 1)))
)

km2 <- do.call(rbind, lapply(names(arms), function(nm) {
  x <- k_means(arms[[nm]]); x$arm <- nm; x
}))
km2$arm <- factor(km2$arm, levels = names(arms))

## Since 0.11.0 a shock divides model time, not the ministep count, and the
## realized number of ministeps is random, so the arms end at different
## steps. The shock falls after the first segment of a treated arm.
shock_step <- arms[["Sparsity shock"]]$path_segments$n_ministeps[1]
pre <- km2[km2$step <= shock_step, ]
pre_w <- stats::reshape(pre[, c("step", "value", "dimension", "arm")],
                        direction = "wide", idvar = c("step", "dimension"),
                        timevar = "arm", v.names = "value")
cat(sprintf("  shock after ministep %d; arms identical before it: %s
",
            shock_step,
            isTRUE(all.equal(pre_w[[3]], pre_w[[4]])) &&
              isTRUE(all.equal(pre_w[[3]], pre_w[[5]]))))

## Direction of each treated arm against the control at the end of the run,
## per channel, for the title.
end_by <- function(arm, k) {
  s <- km2[km2$arm == arm & km2$k == k, ]
  s$value[s$step == max(s$step)]
}
dir_of <- function(arm) sign(vapply(K_DIMS, function(k)
  end_by(arm, k) - end_by("Control (no shock)", k), numeric(1)))
d_pop <- dir_of("Popularity shock"); d_spa <- dir_of("Sparsity shock")
title_B <- if (all(d_pop > 0) && all(d_spa < 0)) {
  "A popularity shock raises all four {K} degrees; the sparsity arm stays near zero, like the control"
} else if (all(d_pop > 0)) {
  "A popularity shock raises all four {K} degrees"
} else {
  "Three runs share a start and a seed until the shock"
}
ARM_COLS <- c("Control (no shock)" = OI[["grey"]], "Sparsity shock" = OI[["blue"]],
              "Popularity shock" = OI[["vermillion"]])
ARM_LTY  <- c("Control (no shock)" = "solid", "Sparsity shock" = "42",
              "Popularity shock" = "solid")
first_dim <- levels(km2$dimension)[1]
## Arm key written in the empty upper-left of the first panel, each name in
## its line's color, in place of a legend.
y_top <- max(km2$value[km2$dimension == first_dim])
endB <- data.frame(arm = factor(names(ARM_COLS), levels = names(ARM_COLS)),
                   y_lab = y_top * c(0.80, 0.70, 0.90),
                   lab = c("control (no shock)", "sparsity shock (dashed)",
                           "popularity shock"))
endB$dimension <- factor(first_dim, levels = levels(km2$dimension))
note_B <- data.frame(dimension = factor(first_dim, levels = levels(km2$dimension)),
                     x = shock_step, label = " shock")
pre_note <- data.frame(dimension = factor(first_dim, levels = levels(km2$dimension)),
                       x = 0.5 * shock_step, xend = 0.5 * shock_step,
                       y = 0.30 * y_top, yend = 0.04 * y_top,
                       label = "all three runs\ncoincide before\nthe shock")
p_shk <- ggplot(km2, aes(x = step, y = value, color = arm, linetype = arm)) +
  geom_vline(xintercept = shock_step, linetype = "dotted",
             color = OI[["vermillion"]], linewidth = 0.6) +
  geom_line(linewidth = 0.65) +
  geom_text(data = note_B, aes(x = x, y = Inf, label = label), inherit.aes = FALSE,
            hjust = 0, vjust = 1.4, size = 3, color = OI[["vermillion"]],
            fontface = "bold") +
  geom_text(data = endB, aes(x = 0, y = y_lab, label = lab),
            hjust = 0, size = 3, show.legend = FALSE) +
  geom_segment(data = pre_note, aes(x = x, xend = xend, y = y, yend = yend),
               inherit.aes = FALSE, color = OI[["ink"]], linewidth = 0.35,
               arrow = arrow(length = unit(0.07, "in"), type = "closed")) +
  geom_text(data = pre_note, aes(x = x, y = y, label = label), inherit.aes = FALSE,
            hjust = 0.5, vjust = -0.2, size = 3, color = OI[["ink"]], lineheight = 0.95) +
  facet_wrap(~dimension, scales = "free_y", ncol = 2,
             labeller = label_parsed) +
  scale_color_manual(values = ARM_COLS, guide = "none") +
  scale_linetype_manual(values = ARM_LTY, guide = "none") +
  labs(title = title_B,
       subtitle = sprintf(paste0("Mean degree at each ministep. Three runs with the same start ",
                                 "and seed; two get a parameter\nshock at the dotted line ",
                                 "(sparsity: density %.1f to %.1f; popularity: %.2f to %.2f)."),
                          SHOCK_DENSITY, SPARSITY_TO, SHOCK_POPULARITY, POPULARITY_TO),
       x = "Ministep (one decision opportunity)", y = "Mean degree") +
  theme_jss()

ggsave(file.path(fig_out, "fig_k_shock_relocation.png"), p_shk,
       width = 7.0, height = 5.0, dpi = 300)

## Terminal values by arm, quoted in the manuscript caption.
term <- do.call(rbind, lapply(K_DIMS, function(k) {
  lab <- unname(K_LABELS[k])
  s <- km2[km2$dimension == lab, ]
  s <- do.call(rbind, lapply(split(s, s$arm), function(a) a[a$step == max(a$step), ]))
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
