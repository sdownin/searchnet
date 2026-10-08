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
##   readme-hero.png           W heatmap, end-of-run bipartite network, {K}-4 panel
##   readme-architectures.png  four influence-matrix architectures, same model
##   readme-nk-validation.png  classical NK: adaptive walks and peak counts
##   readme-shock.png          {K}-4 panel with a two-segment density shock
##   readme-did.png            DID event-time plot recovering a planted shock
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
## function draws the package grammar itself (channel strips with plain
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
## 1. Hero: input W, end-of-run bipartite network, {K}-4 coupled degrees
###############################################################################
cat("[1/5] hero\n")
M <- 8; N <- 12
W_hero  <- saomnk_block_diagonal(N, 3)          # three modules of four
module  <- rep(1:3, each = 4)
strat   <- rep(c(-1, 1), length.out = M)

env <- saomnk_env(M = M, N = N, density = 0.15, seed = 42)
mod <- saomnk_model(density = -1.5, popularity = 0,
                    influence_matrix = W_hero, influence_weight = 0.5,
                    strategies = list(egoX = strat))
quiet(saomnk_run(env, mod, steps_per_actor = 30, seed = 12345))

## Bipartite network at the end of the run (saomnk_get_bipartite()), drawn
## with the same grammar as saomnk_plot_snapshots(): actors as circles,
## components as squares, Fruchterman-Reingold layout. Kept custom rather
## than taken from saomnk_plot_snapshots(): that function colors components
## by initially used / unused, and this panel colors them by W module.
B  <- saomnk_get_bipartite(env)
g  <- igraph::graph_from_biadjacency_matrix(B, directed = FALSE)
igraph::V(g)$kind  <- ifelse(igraph::V(g)$type, "component", "actor")
igraph::V(g)$group <- factor(
  c(ifelse(strat < 0, "Actors: strategy -1", "Actors: strategy 1"), paste("Components: module", module)),
  levels = c("Actors: strategy -1", "Actors: strategy 1", paste("Components: module", 1:3)))
igraph::V(g)$lab   <- c(as.character(seq_len(M)), LETTERS[seq_len(N)])
set.seed(7)
net_cols <- c("Actors: strategy -1" = oi[["orange"]], "Actors: strategy 1" = oi[["sky"]],
              "Components: module 1" = oi[["green"]], "Components: module 2" = oi[["blue"]],
              "Components: module 3" = oi[["purple"]])
p_net <- ggraph(g, layout = "fr") +
  geom_edge_link(color = "grey60", edge_width = 0.5) +
  geom_node_point(aes(shape = kind, color = group), size = 7) +
  geom_node_text(aes(label = lab), color = "white", size = 3, fontface = "bold") +
  scale_shape_manual(values = c(actor = 16, component = 15), guide = "none") +
  scale_color_manual(values = net_cols, name = NULL) +
  labs(title = "Network at the end of the run",
       subtitle = sprintf("%d actors (circles), %d components (squares), %d ties",
                          M, N, sum(B))) +
  theme_readme +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()) +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 4)))

p_w <- w_heatmap(W_hero, "Influence matrix W (input)",
                 paste0("saomnk_block_diagonal(12, 3); components A-L\n", diag_note),
                 labels = LETTERS[seq_len(N)])

p_k4 <- restyle_k4(saomnk_plot_k4(env)) +
  guides(color = guide_legend(nrow = 2,
                              override.aes = list(alpha = 1, shape = NA, linewidth = 1)))

hero <- (p_w / p_net + plot_layout(heights = c(1, 1.25))) | p_k4
hero <- hero + plot_layout(widths = c(1, 1.45))
## 170 dpi (1785 px wide, still about 2x the README column): the {K}-4
## panel's thousands of translucent points make this the largest file.
save_png(hero, "readme-hero.png", width = 10.5, height = 6.9, dpi = 170)


###############################################################################
## 2. Influence-matrix architectures under one model
###############################################################################
cat("[2/5] architectures\n")
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
arch_plots <- lapply(names(W_arch), function(nm) {
  W <- unname(as.matrix(W_arch[[nm]]))
  e <- saomnk_env(M = 8, N = N2, density = 0.15, seed = 42)
  m <- saomnk_model(density = -1.5, influence_matrix = W, influence_weight = 0.5)
  quiet(saomnk_run(e, m, steps_per_actor = 30, seed = 12345))
  Bf  <- saomnk_get_bipartite(e)
  kcc <- mean(colSums((crossprod(Bf)) > 0) - (colSums(Bf) > 0))
  w_heatmap(W, nm, sprintf("%s\nmean K_CC at end of run: %.1f\n%s",
                           W_code[[nm]], kcc, diag_note)) +
    labs(x = NULL, y = NULL) +
    theme(plot.subtitle = element_text(size = 8))
})
arch <- wrap_plots(arch_plots, nrow = 1)
save_png(arch, "readme-architectures.png", width = 12, height = 3.9)


###############################################################################
## 3. Classical NK reproduction (validation)
###############################################################################
cat("[3/5] NK validation\n")
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
cat("[4/5] shock\n")
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
cat("[5/5] DID\n")
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

cat(sprintf("done in %.1f s\n", as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
