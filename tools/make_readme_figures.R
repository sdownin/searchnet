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
## Styling: light background, Okabe-Ito colors (the colorblind-safe palette
## the package's own policy and synthetic-control plots use), PNGs at 2x
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

## Okabe-Ito
oi <- c(orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
        yellow = "#F0E442", blue = "#0072B2", vermillion = "#D55E00",
        purple = "#CC79A7", black = "#000000")

theme_readme <- theme_bw(base_size = 11) +
  theme(plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(color = "grey30", size = 9.5),
        legend.position  = "bottom")

save_png <- function(plot, file, width, height, dpi = 200) {
  path <- file.path(out_dir, file)
  ggsave(path, plot, width = width, height = height, dpi = dpi,
         bg = "white", device = ragg::agg_png)
  cat(sprintf("  wrote %-28s %6.0f KB\n", path, file.size(path) / 1024))
  invisible(path)
}

quiet <- function(expr) invisible(capture.output(suppressMessages(expr)))

## A {K}-4 panel from saomnk_plot_k4(). Since searchnet 0.11.2.9000 the
## function draws the package grammar itself ({K}-dimension strips with display
## names, actors orange, components blue, computed title, reading guides);
## the README keeps its computed title and only sets a subtitle where the
## figure needs one.
restyle_k4 <- function(p, title = NULL, subtitle = NULL) {
  if (!is.null(title)) p <- p + labs(title = title)
  if (!is.null(subtitle)) p <- p + labs(subtitle = subtitle)
  p + theme(plot.background = element_rect(fill = "white", color = NA))
}

## Heatmap of an influence matrix W. The diagonal is drawn light gray, not
## shaded by value: the engine's XWX statistic sums over j != h
## (R/saomnk-base.R), so W's diagonal never enters the objective.
diag_note <- "diagonal unused (XWX sums over j != h)"
w_heatmap <- function(W, title, subtitle = NULL, labels = seq_len(nrow(W))) {
  N  <- nrow(W)
  diag(W) <- NA
  df <- data.frame(row = rep(seq_len(N), times = N),
                   col = rep(seq_len(N), each = N),
                   w   = as.vector(W))
  ggplot(df, aes(col, row, fill = w)) +
    geom_tile(color = "grey85", linewidth = 0.3) +
    scale_y_reverse(breaks = seq_len(N), labels = labels, expand = c(0, 0)) +
    scale_x_continuous(breaks = seq_len(N), labels = labels, position = "top",
                       expand = c(0, 0)) +
    scale_fill_gradient(low = "white", high = oi[["blue"]], limits = c(0, 1),
                        na.value = "grey90", guide = "none") +
    coord_equal() +
    labs(title = title, subtitle = subtitle, x = "component j", y = "component i") +
    theme_readme +
    theme(panel.grid = element_blank(), axis.ticks = element_blank(),
          axis.text = element_text(size = 7))
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
## 2. Influence-matrix architectures under one model
###############################################################################
cat("[2/6] architectures\n")
N2 <- 12
W_arch <- list(
  "Modular"      = saomnk_block_diagonal(N2, 3),
  "Nested modules" = (saomnk_block_diagonal(N2, 2) + saomnk_block_diagonal(N2, 4) +
                        diag(N2)) / 3,
  "Local (ring)" = nk_to_saomnk(nk_landscape(N2, 2, model = "adjacent", seed = 1))$influence_matrix,
  "Random"       = nk_to_saomnk(nk_landscape(N2, 2, model = "random",   seed = 1))$influence_matrix
)
W_code <- c("Modular"        = "saomnk_block_diagonal(12, 3)",
            "Nested modules" = "mean of block_diagonal at 2, 4, 12 blocks",
            "Local (ring)"   = "nk_to_saomnk(nk_landscape(12, 2, \"adjacent\"))",
            "Random"         = "nk_to_saomnk(nk_landscape(12, 2, \"random\"))")
## Ties and mean K_CC (components co-held with each component, excluding
## itself) at the end of one run of the same model on W.
end_kcc <- function(W) {
  e <- saomnk_env(M = 8, N = N2, density = 0.15, seed = 42)
  m <- saomnk_model(density = -1.5, influence_matrix = W, influence_weight = 0.5)
  quiet(saomnk_run(e, m, steps_per_actor = 30, seed = 12345))
  Bf <- saomnk_get_bipartite(e)
  sprintf("end of run: %d ties, mean K_CC %.1f", sum(Bf),
          mean(colSums((crossprod(Bf)) > 0) - (colSums(Bf) > 0)))
}

## Row 2: the same support as row 1, with signed real weights. Off-diagonal
## nonzero entries get a symmetric draw from Uniform(-1, 1); nested modules
## keep their magnitudes (module depth) and get a random sign. Positive =
## complements (holding both pays), negative = substitutes (holding both
## costs). Conventional NK cannot express the negative case: its matrix only
## says WHO interacts, and the payoffs are drawn separately, i.i.d. U(0, 1).
signed_version <- function(W, nm, seed = 2026) {
  W <- unname(as.matrix(W)); n <- nrow(W)
  set.seed(seed)
  ## Same nonzero cells as W. A symmetric W gets a symmetric draw (one weight
  ## per pair); an asymmetric one, such as nk_landscape()'s random pattern
  ## (row j lists the components that affect j), gets one weight per cell.
  sym <- isSymmetric(W)
  S <- matrix(0, n, n)
  nz <- which((if (sym) upper.tri(W) else row(W) != col(W)) & W != 0)
  S[nz] <- if (nm == "Nested modules") W[nz] * sample(c(-1, 1), length(nz), replace = TRUE)
           else stats::runif(length(nz), -1, 1)
  if (sym) S <- S + t(S)
  diag(S) <- diag(W)
  stopifnot(identical(S != 0, W != 0 | (row(W) == col(W) & diag(W)[row(W)] != 0)))
  S
}
w_heatmap_signed <- function(W, title, subtitle, legend = FALSE) {
  N  <- nrow(W)
  diag(W) <- NA
  df <- data.frame(row = rep(seq_len(N), times = N), col = rep(seq_len(N), each = N),
                   w = as.vector(W))
  ggplot(df, aes(col, row, fill = w)) +
    geom_tile(color = "grey85", linewidth = 0.3) +
    scale_y_reverse(breaks = seq_len(N), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq_len(N), position = "top", expand = c(0, 0)) +
    scale_fill_gradient2(low = "#018571", mid = "white", high = "#A6611A", midpoint = 0,
                         limits = c(-1, 1), na.value = "grey90",
                         name = "weight w_hj\n(+ complements,\n- substitutes)",
                         guide = if (legend) "colourbar" else "none") +
    coord_equal() +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_readme +
    theme(panel.grid = element_blank(), axis.ticks = element_blank(),
          axis.text = element_text(size = 7), plot.subtitle = element_text(size = 8),
          legend.position = "right", legend.title = element_text(size = 8),
          legend.text = element_text(size = 7), legend.key.height = grid::unit(0.5, "cm"))
}

row_label <- function(head, body) {
  ggplot() +
    annotate("text", x = 0, y = 1, label = head, hjust = 0, vjust = 1, fontface = "bold", size = 3.6) +
    annotate("text", x = 0, y = 0.80, label = body, hjust = 0, vjust = 1, size = 2.9,
             color = "grey30", lineheight = 1) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void()
}

arch_plots <- lapply(names(W_arch), function(nm) {
  W <- unname(as.matrix(W_arch[[nm]]))
  w_heatmap(W, nm, sprintf("%s\n%s\n%s",
                           W_code[[nm]], end_kcc(W), diag_note)) +
    labs(x = NULL, y = NULL) +
    theme(plot.subtitle = element_text(size = 8))
})
signed_plots <- lapply(seq_along(W_arch), function(k) {
  nm <- names(W_arch)[k]
  S  <- signed_version(W_arch[[nm]], nm)
  w_heatmap_signed(S, paste(nm, "(signed)"),
                   sprintf("%s\n%s",
                           if (nm == "Nested modules") "same magnitudes, random signs"
                           else "same pattern, weights U(-1, 1)", end_kcc(S)),
                   legend = k == length(W_arch))
})
lab1 <- row_label("Conventional NK",
                  "binary pattern: who\ninteracts (nested:\nmodule depth)\n\npayoffs drawn apart,\ni.i.d. Uniform(0, 1)")
lab2 <- row_label("SAOM-NK",
                  "signed real weights\non the same pattern\n\nbrown: complements\nteal: substitutes")
arch <- wrap_plots(c(list(lab1), arch_plots, list(lab2), signed_plots), nrow = 2,
                   widths = c(0.55, 1, 1, 1, 1))
save_png(arch, "readme-architectures.png", width = 13, height = 7.6, dpi = 170)


###############################################################################
## 3. Classical NK reproduction (validation)
###############################################################################
cat("[3/6] NK validation\n")
N3 <- 12
walk_df <- do.call(rbind, lapply(c(0, 3, 8), function(k) {
  nk <- nk_landscape(N3, k, model = "random", seed = 2026 + k)
  set.seed(11)
  starts <- sample.int(2^N3, 30) - 1L
  do.call(rbind, lapply(seq_along(starts), function(i) {
    w <- nk_walk(nk, start = starts[i], type = "steepest")
    data.frame(K = sprintf("K = %d", k), walk = i,
               step = seq_along(w$fitness) - 1L, fitness = w$fitness)
  }))
}))
walk_df$K <- factor(walk_df$K, levels = c("K = 0", "K = 3", "K = 8"))
k_cols <- c("K = 0" = oi[["blue"]], "K = 3" = oi[["orange"]], "K = 8" = oi[["vermillion"]])
p_walk <- ggplot(walk_df, aes(step, fitness, group = interaction(K, walk), color = K)) +
  geom_line(alpha = 0.55, linewidth = 0.5) +
  geom_point(data = function(d) d[ave(d$step, d$K, d$walk, FUN = max) == d$step, ],
             size = 1.4) +
  facet_wrap(~ K, nrow = 1) +
  scale_color_manual(values = k_cols, guide = "none") +
  labs(title = "Adaptive walks stop at local peaks",
       subtitle = "nk_walk(type = \"steepest\") from 30 random starts, N = 12; dots mark the peak reached",
       x = "Step", y = "Fitness") +
  theme_readme

sweep <- nk_sweep_K(N = N3, K_values = c(0, 1, 2, 3, 4, 6, 8, 11),
                    n_landscapes = 4, n_walks = 5, model = "random", seed = 2026)
opt_col <- grep("optima", names(sweep), value = TRUE)[1]
sweep$n_opt <- sweep[[opt_col]]
sweep$ref   <- 2^N3 / (sweep$K + 1)
red <- NULL
quiet(red <- nk_verify_reduction(N = 10, K = 3, seed = 42))
p_sweep <- ggplot(sweep, aes(K, n_opt)) +
  geom_line(aes(y = ref, linetype = "2^N / (K + 1), fully random limit"), color = "grey40") +
  geom_line(color = oi[["vermillion"]], linewidth = 0.8) +
  geom_point(color = oi[["vermillion"]], size = 2.2) +
  scale_y_log10() +
  scale_linetype_manual(values = "dashed", name = NULL) +
  labs(title = "Ruggedness rises with K",
       subtitle = sprintf("nk_sweep_K(): mean local optima, 4 landscapes per K\nnk_verify_reduction(N = 10, K = 3): max |NK - SaoMNK|\n= %s over %d configurations",
                          format(signif(red$max_difference, 2)), red$n_configs),
       x = "K (epistatic partners per component)", y = "Local optima (log scale)") +
  theme_readme + theme(legend.position = c(0.68, 0.88),
                       legend.background = element_blank())
nkfig <- p_walk + p_sweep + plot_layout(widths = c(1.9, 1))
save_png(nkfig, "readme-nk-validation.png", width = 12, height = 4.2)


###############################################################################
## 4. Shock response: two-segment density schedule
###############################################################################
cat("[4/6] shock\n")
## Same design as vignettes/saomnk-simulation.Rmd, section 3.2: density -0.5
## in the first half of model time, -2.0 in the second.
env_s <- saomnk_env(M = 6, N = 8, density = 0, seed = 42)
mod_s <- saomnk_model(density = -0.5, influence_matrix = saomnk_block_diagonal(8, 2),
                      influence_weight = 0.5)
quiet(saomnk_run(env_s, mod_s, steps_per_actor = 100, seed = 12345,
                 shocks = list(saomnk_shock("density", parameter = -0.5, portion = 1),
                               saomnk_shock("density", parameter = -2.0, portion = 1))))
shock_step <- min(env_s$theta_shocks[[2]]$chain_step_ids)
p_shock <- restyle_k4(saomnk_plot_k4(env_s))
save_png(p_shock, "readme-shock.png", width = 8.5, height = 6)


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
