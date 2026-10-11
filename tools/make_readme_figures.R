#!/usr/bin/env Rscript
###############################################################################
## make_readme_figures.R
##
## Regenerates the figures shown in README.md, writing PNGs to man/figures/
## (the pkgdown/GitHub convention, so they ship with the package).
##
## Usage, from the package root:
##
##     Rscript tools/make_readme_figures.R
##
## Every figure comes from a seeded synthetic run of exported searchnet
## functions; no data files are read. Seeds are fixed below, so the script is
## deterministic for a given searchnet, RSiena and R version. Total runtime is
## well under two minutes on a laptop.
##
## The architectures, NK validation and shock figures are shared with the JSS
## paper: their builders live in tools/figures_shared.R, sourced
## below, and this script only sizes and saves them. The hero, DID and
## objective figures are README-only and are built here.
##
## Styling: light background, Okabe-Ito colors (the colorblind-safe palette
## the package's own basin and synthetic-control plots use), PNGs at 2x
## (200 dpi) so they stay sharp on high-density displays.
##
## Figures written:
##   readme-hero.png           W heatmap, start and end networks, {K}-4 panel
##   readme-architectures.png  four influence-matrix architectures, 0/1 and signed weights
##   readme-nk-validation.png  classical NK: adaptive walks and peak counts
##   readme-shock.png          {K}-4 panel with a two-segment density shock
##   readme-did.png            DID event-time plot recovering a planted shock
##   readme-objective.png      the objective by class of effects (rosetta_plot row I)
###############################################################################

t_start <- Sys.time()

if (file.exists("DESCRIPTION") &&
    any(grepl("^Package: searchnet", readLines("DESCRIPTION", n = 1)))) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressMessages(library(searchnet))
}
suppressMessages({
  library(ggplot2)
  library(patchwork)
  library(ggraph)
})

out_dir <- file.path("man", "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## Shared builders and helpers (Okabe-Ito palette, theme, W heatmap, {K}-4
## restyle), also used by the JSS paper.
source(file.path("tools", "figures_shared.R"))
oi           <- fs_oi
theme_readme <- fs_theme(11)
quiet        <- fs_quiet
restyle_k4   <- fs_restyle_k4
w_heatmap    <- fs_w_heatmap

save_png <- function(plot, file, width, height, dpi = 200) {
  path <- file.path(out_dir, file)
  ggsave(path, plot, width = width, height = height, dpi = dpi,
         bg = "white", device = ragg::agg_png)
  cat(sprintf("  wrote %-28s %6.0f KB
", path, file.size(path) / 1024))
  invisible(path)
}


###############################################################################
## 1. Hero: input W, the network at the start and at the end of the run, and
##    the {K}-4 coupled degrees that connect them
###############################################################################
cat("[1/6] hero\n")
M <- 8; N <- 12
W_hero  <- saomnk_block_diagonal(N, 3)          # three modules of four
module  <- rep(1:3, each = 4)
strat   <- rep(c(-1, 1), length.out = M)

env <- saomnk_env(M = M, N = N, density = 0.15, seed = 42)
mod <- saomnk_model(density = -1.5, popularity = 0,
                    influence_matrix = W_hero, influence_weight = 0.5,
                    strategies = list(egoX = strat))
B0 <- saomnk_get_bipartite(env)                 # the state before the run
quiet(saomnk_run(env, mod, steps_per_actor = 30, seed = 12345))
B  <- saomnk_get_bipartite(env)                 # the state after it
n_steps <- dim(env$bi_env_arr)[3] - 1L

## Both network panels are drawn with the same grammar as
## saomnk_plot_snapshots() (actors circles, components squares) and share ONE
## Fruchterman-Reingold layout, computed on the union of the start and end
## ties, so only the ties differ between the panels. Kept custom rather than
## taken from saomnk_plot_snapshots(): that function colors components by
## initially used / unused, and these panels color them by W module.
node_group <- factor(
  c(ifelse(strat < 0, "Actors: strategy -1", "Actors: strategy 1"), paste("Components: module", module)),
  levels = c("Actors: strategy -1", "Actors: strategy 1", paste("Components: module", 1:3)))
net_graph <- function(Bm, status) {
  g <- igraph::graph_from_biadjacency_matrix(Bm, directed = FALSE)
  igraph::V(g)$kind  <- ifelse(igraph::V(g)$type, "component", "actor")
  igraph::V(g)$group <- node_group
  igraph::V(g)$lab   <- c(as.character(seq_len(M)), LETTERS[seq_len(N)])
  el <- igraph::as_edgelist(g, names = FALSE)   # actor index, M + component index
  igraph::E(g)$status <- status[cbind(el[, 1], el[, 2] - M)]
  g
}
set.seed(7)
xy <- igraph::layout_with_fr(igraph::graph_from_biadjacency_matrix((B0 + B) > 0, directed = FALSE))

kept <- B0 == 1 & B == 1
st0  <- ifelse(kept, "kept", "dropped")          # start panel: ties later dropped
st1  <- ifelse(kept, "kept", "formed")           # end panel: ties formed in the run
net_cols <- c("Actors: strategy -1" = oi[["orange"]], "Actors: strategy 1" = oi[["sky"]],
              "Components: module 1" = oi[["green"]], "Components: module 2" = oi[["blue"]],
              "Components: module 3" = oi[["purple"]])
net_panel <- function(g, title, subtitle) {
  ggraph(g, layout = "manual", x = xy[, 1], y = xy[, 2]) +
    geom_edge_link(aes(edge_colour = status, edge_linetype = status), edge_width = 0.6) +
    scale_edge_colour_manual(values = c(kept = "grey72", formed = "grey25", dropped = "grey45"),
                             guide = "none") +
    scale_edge_linetype_manual(values = c(kept = "solid", formed = "solid", dropped = "22"),
                               guide = "none") +
    geom_node_point(aes(shape = kind, color = group), size = 6.5) +
    geom_node_text(aes(label = lab), color = "white", size = 2.9, fontface = "bold") +
    scale_shape_manual(values = c(actor = 16, component = 15), guide = "none") +
    scale_color_manual(values = net_cols, name = NULL, drop = FALSE) +
    coord_cartesian(clip = "off") +
    labs(title = title, subtitle = subtitle) +
    theme_readme +
    theme(panel.grid = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank()) +
    ## Legend keys take the node shapes: circles for the two actor groups,
    ## squares for the three component modules (levels order of `group`).
    guides(color = guide_legend(nrow = 3,
                                override.aes = list(size = 4, shape = c(16, 16, 15, 15, 15))))
}
p_start <- net_panel(net_graph(B0, st0), "Network at the start of the run",
                     sprintf("ministep 0: %d ties; dashed: dropped later", sum(B0)))
p_end   <- net_panel(net_graph(B, st1), "Network at the end of the run",
                     sprintf("ministep %d: %d ties (%d formed, dark; %d dropped)",
                             n_steps, sum(B), sum(B == 1 & B0 == 0), sum(B0 == 1 & B == 0)))

p_w <- w_heatmap(W_hero, "Influence matrix W (input)",
                 "saomnk_block_diagonal(12, 3)\ndiagonal unused (XWX: j != h)",
                 labels = LETTERS[seq_len(N)])

p_k4 <- restyle_k4(saomnk_plot_k4(env)) +
  guides(color = guide_legend(nrow = 2,
                              override.aes = list(alpha = 1, shape = NA, linewidth = 1)))

## Left column: the input and the two states, top to bottom; right two
## columns: the {K}-4 panel at full height, so the trajectories get the
## vertical range. Identical node legends are collected once.
## free() on the {K}-4 panel stops patchwork aligning the W panel with its
## facets across the row, so the matrix sits directly under its title; the spare height
## goes below it, as padding between the input and the two network states,
## which sit together.
left_col <- (p_w + theme(plot.margin = margin(5.5, 5.5, 30, 5.5))) / p_start / p_end +
  plot_layout(heights = c(0.85, 1, 1), guides = "collect") &
  theme(legend.position = "bottom")
## Each column keeps its own legend (node colors under the networks, line
## colors under the {K}-4 panel): legends cannot be collected across a free()d
## panel without overprinting its axis.
hero <- left_col - free(p_k4 + theme(legend.position = "bottom")) +
  plot_layout(widths = c(1, 2))
## 160 dpi: the {K}-4 panel's thousands of translucent points make this the
## largest README file.
save_png(hero, "readme-hero.png", width = 12, height = 11, dpi = 150)


###############################################################################
## 2-4. Figures shared with the JSS paper (tools/figures_shared.R)
###############################################################################
cat("[2/6] architectures
")
save_png(fs_fig_architectures(), "readme-architectures.png",
         width = 13, height = 7.6, dpi = 170)

cat("[3/6] NK validation
")
save_png(fs_fig_nk_validation(), "readme-nk-validation.png", width = 12, height = 4.2)

cat("[4/6] shock
")
save_png(fs_fig_shock(fs_shock_run(steps_per_actor = 100)), "readme-shock.png",
         width = 8.5, height = 6)


###############################################################################
## 5. Causal pipeline: DID event-time estimates of the planted shock
###############################################################################
cat("[5/6] DID\n")
## Same design as vignettes/saomnk-causal-inference.Rmd: a shocked arm and an
## unshocked comparison arm, six actors each, identical seeds.
mod_c <- saomnk_model(density = -0.5, popularity = 0.15,
                      influence_matrix = saomnk_block_diagonal(8, 2))
env_t <- saomnk_env(M = 6, N = 8, seed = 42)
quiet(saomnk_run(env_t, mod_c, steps_per_actor = 20, seed = 12345,
                 shocks = list(saomnk_shock("density", parameter = -0.5, portion = 1),
                               saomnk_shock("density", parameter = -2.0, portion = 1))))
env_c <- saomnk_env(M = 6, N = 8, seed = 42)
quiet(saomnk_run(env_c, mod_c, steps_per_actor = 20, seed = 12345))
s_step <- min(env_t$theta_shocks[[2]]$chain_step_ids)
pan_t <- searchnet_causal_panel(env_t, shock_step = s_step, outcome = "utility")
pan_c <- searchnet_causal_panel(env_c, shock_step = s_step, outcome = "utility",
                                treated_actors = integer(0))
pan_c$actor_id <- factor(as.integer(as.character(pan_c$actor_id)) + 6L)
common <- seq_len(min(max(pan_t$step), max(pan_c$step)))
panel  <- rbind(pan_t[pan_t$step %in% common, ], pan_c[pan_c$step %in% common, ])
att <- NULL
quiet(att <- suppressWarnings(searchnet_did(panel)))
simple <- NULL
quiet(simple <- did::aggte(att, type = "simple"))
p_did <- NULL
quiet(p_did <- suppressWarnings(searchnet_causal_plot(att, type = "did")))
## searchnet_causal_plot() draws the title, reading guide and notes; the
## README adds what the outcome and the two arms are.
p_did <- p_did +
  labs(caption = "Outcome: actor utility. Shocked arm vs. unshocked arm, 6 actors each, same start and seed.") +
  theme(plot.background = element_rect(fill = "white", color = NA))
save_png(p_did, "readme-did.png", width = 9, height = 4.8)


###############################################################################
## 6. The objective by class of effects (image fallback for the README math)
###############################################################################
cat("[6/6] objective by class\n")
## rosetta_plot() with no model and no comparison entry draws row I alone:
## the general objective, one colored summand per class of
## inst/rosetta/classes.yaml, its chip, effects and construct, and black {K}
## badges in two rows (view = "both"): each class's decision dimension
## (field `reads`, utility) above, its outcome dimension (field `moves`,
## network) below. No simulation is run.
p_obj <- rosetta_plot(NULL, compare = NULL, glyph = FALSE, view = "both",
                      classes = c("complementarity", "scope", "crowding",
                                  "contact", "imitation", "covariate"))
save_png(p_obj, "readme-objective.png", width = 12, height = 4.2)

cat(sprintf("done in %.1f s\n", as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
