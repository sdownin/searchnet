###############################################################################
## figures_shared.R
##
## One source for the figures that appear both in the README and in the JSS
## paper (paper/searchnet-jss.Rmd). Each builder takes a base font size and a
## layout switch, so the same figure renders at README size (the defaults,
## which reproduce man/figures/readme-*.png) or at JSS text width (about
## 6.5 in; compact = TRUE, base_size about 8).
##
## Sourced by:
##   tools/make_readme_figures.R              README PNGs (man/figures/)
##   paper/replication/make_shared_figures.R  paper PNGs (paper/figures/; dev only)
##   paper/searchnet-jss.Rmd                  the shock figure, knitted
##
## Builders (each returns a ggplot/patchwork object; the numbers a caption may
## quote are attached as attr(p, "numbers")):
##   fs_fig_architectures()  four influence-matrix architectures, conventional
##                           (0/1) and signed rows
##   fs_fig_nk_validation()  classical NK: adaptive walks and peak counts
##   fs_shock_run()          the README's two-segment density-shock run
##   fs_fig_shock(env)       the {K}-4 panel of a shocked run
##
## Requires searchnet (loaded by the caller), ggplot2 and patchwork. Every
## figure comes from seeded synthetic runs; no data files are read.
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

## Okabe-Ito
fs_oi <- c(orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
           yellow = "#F0E442", blue = "#0072B2", vermillion = "#D55E00",
           purple = "#CC79A7", black = "#000000")

## Light theme shared by the figures. At base_size = 11 it is the README theme.
fs_theme <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.title       = element_text(face = "bold", size = base_size + 1),
          plot.subtitle    = element_text(color = "grey30", size = base_size - 1.5),
          legend.position  = "bottom")
}

fs_quiet <- function(expr) invisible(capture.output(suppressMessages(expr)))

## A {K}-4 panel from saomnk_plot_k4(). Since searchnet 0.11.2.9000 the
## function draws the package grammar itself ({K}-dimension strips with display
## names, actors orange, components blue, computed title, reading guides);
## callers keep its computed title and only set a subtitle where needed.
fs_restyle_k4 <- function(p, title = NULL, subtitle = NULL) {
  if (!is.null(title)) p <- p + labs(title = title)
  if (!is.null(subtitle)) p <- p + labs(subtitle = subtitle)
  p + theme(plot.background = element_rect(fill = "white", color = NA))
}

## Heatmap of an influence matrix W. The diagonal is drawn light gray, not
## shaded by value: the engine's XWX statistic sums over j != h
## (R/saomnk-base.R), so W's diagonal never enters the objective.
fs_diag_note <- "diagonal unused (XWX sums over j != h)"
fs_w_heatmap <- function(W, title, subtitle = NULL, labels = seq_len(nrow(W)),
                         base_size = 11, axis_size = 7 * base_size / 11) {
  s  <- base_size / 11
  N  <- nrow(W)
  diag(W) <- NA
  df <- data.frame(row = rep(seq_len(N), times = N),
                   col = rep(seq_len(N), each = N),
                   w   = as.vector(W))
  ggplot(df, aes(col, row, fill = w)) +
    geom_tile(color = "grey85", linewidth = 0.3 * s) +
    scale_y_reverse(breaks = seq_len(N), labels = labels, expand = c(0, 0)) +
    scale_x_continuous(breaks = seq_len(N), labels = labels, position = "top",
                       expand = c(0, 0)) +
    scale_fill_gradient(low = "white", high = fs_oi[["blue"]], limits = c(0, 1),
                        na.value = "grey90", guide = "none") +
    coord_equal() +
    labs(title = title, subtitle = subtitle, x = "component j", y = "component i") +
    fs_theme(base_size) +
    theme(panel.grid = element_blank(), axis.ticks = element_blank(),
          axis.text = element_text(size = axis_size))
}


###############################################################################
## Influence-matrix architectures under one model
###############################################################################
## Row 1: four 0/1 (or, for nested modules, depth-weighted) patterns, as
## conventional NK uses them. Row 2: the same support with signed real weights.
## Each panel reports ties and mean K_CC at the end of one run of the same
## model on that W.
##
## compact = TRUE is the JSS layout: narrower row labels, the API call wrapped
## onto two lines, the diagonal note moved to the caption, the signed-weight
## legend under the figure.
fs_fig_architectures <- function(base_size = 11, compact = FALSE) {
  s  <- base_size / 11
  ax_sz  <- if (compact) base_size - 3 else 7 * s      # tick labels
  sub_sz <- if (compact) base_size - 2.2 else 8 * s    # subtitles
  N2 <- 12
  W_arch <- list(
    "Modular"      = saomnk_block_diagonal(N2, 3),
    "Nested modules" = (saomnk_block_diagonal(N2, 2) + saomnk_block_diagonal(N2, 4) +
                          diag(N2)) / 3,
    "Local (ring)" = nk_to_saomnk(nk_landscape(N2, 2, model = "adjacent", seed = 1))$influence_matrix,
    "Random"       = nk_to_saomnk(nk_landscape(N2, 2, model = "random",   seed = 1))$influence_matrix
  )
  W_code <- if (!compact) {
    c("Modular"        = "saomnk_block_diagonal(12, 3)",
      "Nested modules" = "mean of block_diagonal at 2, 4, 12 blocks",
      "Local (ring)"   = "nk_to_saomnk(nk_landscape(12, 2, \"adjacent\"))",
      "Random"         = "nk_to_saomnk(nk_landscape(12, 2, \"random\"))")
  } else {
    c("Modular"        = "saomnk_block_diagonal(\n  12, 3)\n ",
      "Nested modules" = "mean of block_diagonal\nat 2, 4, 12 blocks\n ",
      "Local (ring)"   = "nk_to_saomnk(\n  nk_landscape(12, 2,\n  \"adjacent\"))",
      "Random"         = "nk_to_saomnk(\n  nk_landscape(12, 2,\n  \"random\"))")
  }
  ## Ties and mean K_CC (components co-held with each component, excluding
  ## itself) at the end of one run of the same model on W.
  end_stats <- function(W) {
    e <- saomnk_env(M = 8, N = N2, density = 0.15, seed = 42)
    m <- saomnk_model(density = -1.5, influence_matrix = W, influence_weight = 0.5)
    fs_quiet(saomnk_run(e, m, steps_per_actor = 30, seed = 12345))
    Bf <- saomnk_get_bipartite(e)
    c(ties = sum(Bf),
      kcc = mean(colSums((crossprod(Bf)) > 0) - (colSums(Bf) > 0)))
  }
  stat_lab <- function(st) sprintf(if (compact) "end of run: %d ties,\nmean K_CC %.1f"
                                   else "end of run: %d ties, mean K_CC %.1f",
                                   as.integer(st[["ties"]]), st[["kcc"]])

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
    ## Exact-support assertion: the signed matrix has exactly W's nonzero cells.
    stopifnot(identical(S != 0, W != 0 | (row(W) == col(W) & diag(W)[row(W)] != 0)))
    S
  }
  w_heatmap_signed <- function(W, title, subtitle, legend = FALSE) {
    N  <- nrow(W)
    diag(W) <- NA
    df <- data.frame(row = rep(seq_len(N), times = N), col = rep(seq_len(N), each = N),
                     w = as.vector(W))
    ggplot(df, aes(col, row, fill = w)) +
      geom_tile(color = "grey85", linewidth = 0.3 * s) +
      scale_y_reverse(breaks = seq_len(N), expand = c(0, 0)) +
      scale_x_continuous(breaks = seq_len(N), position = "top", expand = c(0, 0)) +
      scale_fill_gradient2(low = "#018571", mid = "white", high = "#A6611A", midpoint = 0,
                           limits = c(-1, 1), na.value = "grey90",
                           name = if (compact) "weight w_hj (+ complements, - substitutes)"
                                  else "weight w_hj\n(+ complements,\n- substitutes)",
                           guide = if (legend) "colourbar" else "none") +
      coord_equal() +
      labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
      fs_theme(base_size) +
      theme(panel.grid = element_blank(), axis.ticks = element_blank(),
            axis.text = element_text(size = ax_sz), plot.subtitle = element_text(size = sub_sz),
            legend.position = if (compact) "bottom" else "right",
            legend.title = element_text(size = 8 * s),
            legend.text = element_text(size = 7 * s),
            legend.key.height = grid::unit(if (compact) 0.25 else 0.5, "cm")) +
      (if (compact) theme(legend.key.width = grid::unit(1.6, "cm")) else NULL)
  }

  row_label <- function(head, body) {
    ggplot() +
      annotate("text", x = 0, y = 1, label = head, hjust = 0, vjust = 1, fontface = "bold",
               size = 3.6 * s) +
      annotate("text", x = 0, y = 0.80, label = body, hjust = 0, vjust = 1, size = 2.9 * s,
               color = "grey30", lineheight = 1) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
      theme_void()
  }

  conv_stats <- lapply(W_arch, function(W) end_stats(unname(as.matrix(W))))
  S_arch <- lapply(names(W_arch), function(nm) signed_version(W_arch[[nm]], nm))
  names(S_arch) <- names(W_arch)
  sign_stats <- lapply(S_arch, end_stats)

  arch_plots <- lapply(names(W_arch), function(nm) {
    W <- unname(as.matrix(W_arch[[nm]]))
    sub <- if (compact) sprintf("%s\n%s", W_code[[nm]], stat_lab(conv_stats[[nm]]))
           else sprintf("%s\n%s\n%s", W_code[[nm]], stat_lab(conv_stats[[nm]]), fs_diag_note)
    fs_w_heatmap(W, nm, sub, base_size = base_size, axis_size = ax_sz) +
      labs(x = NULL, y = NULL) +
      theme(plot.subtitle = element_text(size = sub_sz))
  })
  signed_plots <- lapply(seq_along(W_arch), function(k) {
    nm <- names(W_arch)[k]
    w_heatmap_signed(S_arch[[nm]], if (compact) nm else paste(nm, "(signed)"),
                     sprintf("%s\n%s",
                             if (nm == "Nested modules")
                               (if (compact) "signed: same magnitudes,\nrandom signs"
                                else "same magnitudes, random signs")
                             else (if (compact) "signed: same pattern,\nweights U(-1, 1)"
                                   else "same pattern, weights U(-1, 1)"),
                             stat_lab(sign_stats[[nm]])),
                     legend = k == length(W_arch))
  })
  if (!compact) {
    lab1 <- row_label("Conventional NK",
                      "binary pattern: who\ninteracts (nested:\nmodule depth)\n\npayoffs drawn apart,\ni.i.d. Uniform(0, 1)")
    lab2 <- row_label("SAOM-NK",
                      "signed real weights\non the same pattern\n\nbrown: complements\nteal: substitutes")
    arch <- wrap_plots(c(list(lab1), arch_plots, list(lab2), signed_plots), nrow = 2,
                       widths = c(0.55, 1, 1, 1, 1))
  } else {
    lab1 <- row_label("Conventional\nNK",
                      "\n\n0/1 pattern:\nwho interacts\n\npayoffs drawn\napart, i.i.d.\nU(0, 1)")
    lab2 <- row_label("SAOM-NK",
                      "\nsigned weights\non the same\npattern\n\nbrown:\ncomplements\nteal:\nsubstitutes")
    arch <- wrap_plots(c(list(lab1), arch_plots, list(lab2), signed_plots), nrow = 2,
                       widths = c(0.55, 1, 1, 1, 1)) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold", size = base_size + 0.5))
  }
  attr(arch, "numbers") <- data.frame(
    architecture = names(W_arch),
    conventional_ties = vapply(conv_stats, `[[`, numeric(1), "ties"),
    conventional_kcc  = vapply(conv_stats, `[[`, numeric(1), "kcc"),
    signed_ties       = vapply(sign_stats, `[[`, numeric(1), "ties"),
    signed_kcc        = vapply(sign_stats, `[[`, numeric(1), "kcc"),
    row.names = NULL)
  arch
}


###############################################################################
## Classical NK reproduction (validation)
###############################################################################
fs_fig_nk_validation <- function(base_size = 11, compact = FALSE) {
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
  k_cols <- c("K = 0" = fs_oi[["blue"]], "K = 3" = fs_oi[["orange"]],
              "K = 8" = fs_oi[["vermillion"]])
  p_walk <- ggplot(walk_df, aes(step, fitness, group = interaction(K, walk), color = K)) +
    geom_line(alpha = 0.55, linewidth = if (compact) 0.35 else 0.5) +
    geom_point(data = function(d) d[ave(d$step, d$K, d$walk, FUN = max) == d$step, ],
               size = if (compact) 0.9 else 1.4) +
    facet_wrap(~ K, nrow = 1) +
    scale_color_manual(values = k_cols, guide = "none") +
    labs(title = "Adaptive walks stop at local peaks",
         subtitle = paste0("nk_walk(type = \"steepest\") from 30 random starts, N = 12;",
                           if (compact) "\n" else " ", "dots mark the peak reached"),
         x = "Step", y = "Fitness") +
    fs_theme(base_size)

  sweep <- nk_sweep_K(N = N3, K_values = c(0, 1, 2, 3, 4, 6, 8, 11),
                      n_landscapes = 4, n_walks = 5, model = "random", seed = 2026)
  opt_col <- grep("optima", names(sweep), value = TRUE)[1]
  sweep$n_opt <- sweep[[opt_col]]
  ref_full <- 2^N3 / (N3 + 1)   # expected local optima in the fully random case K = N - 1
  red <- NULL
  fs_quiet(red <- nk_verify_reduction(N = 10, K = 3, seed = 42))
  sub_sweep <- if (!compact) {
    sprintf("nk_sweep_K(): mean local optima, 4 landscapes per K\nnk_verify_reduction(N = 10, K = 3): max |NK - SAOM-NK|\n= %s over %d configurations",
            format(signif(red$max_difference, 2)), red$n_configs)
  } else {
    sprintf("nk_sweep_K(): mean local optima,\n4 landscapes per K\nnk_verify_reduction(N = 10, K = 3):\nmax |NK - SAOM-NK| = %s over %d\nconfigurations",
            format(signif(red$max_difference, 2)), red$n_configs)
  }
  p_sweep <- ggplot(sweep, aes(K, n_opt)) +
    geom_hline(aes(yintercept = ref_full,
                   linetype = "2^N / (N + 1): fully random\ncase, K = N - 1"),
               color = "grey40") +
    geom_line(color = fs_oi[["vermillion"]], linewidth = if (compact) 0.6 else 0.8) +
    geom_point(color = fs_oi[["vermillion"]], size = if (compact) 1.4 else 2.2) +
    scale_y_log10() +
    scale_linetype_manual(values = "dashed", name = NULL) +
    labs(title = "Ruggedness rises with K",
         subtitle = sub_sweep,
         x = "K (epistatic partners per component)", y = "Local optima (log scale)") +
    fs_theme(base_size) +
    theme(legend.position = if (compact) c(0.6, 0.1) else c(0.7, 0.13),
          legend.background = element_blank())
  if (compact) p_sweep <- p_sweep +
    theme(legend.text = element_text(size = base_size - 1.5),
          legend.key.height = grid::unit(0.3, "cm"))
  nkfig <- p_walk + p_sweep + plot_layout(widths = c(1.9, 1))

  ends <- walk_df[ave(walk_df$step, walk_df$K, walk_df$walk, FUN = max) == walk_df$step, ]
  attr(nkfig, "numbers") <- list(
    walk_peaks = aggregate(fitness ~ K, ends, function(x) length(unique(round(x, 12)))),
    walk_steps = aggregate(step ~ K, ends, mean),
    sweep = sweep[, c("K", "n_opt")], ref_full_random = ref_full,
    reduction = list(max_difference = red$max_difference, n_configs = red$n_configs))
  nkfig
}


###############################################################################
## Shock response: two-segment density schedule
###############################################################################
## Same design as vignettes/saomnk-simulation.Rmd, section 3.2: density -0.5
## in the first half of model time, -2.0 in the second. Returns the env.
fs_shock_run <- function(steps_per_actor = 100) {
  env_s <- saomnk_env(M = 6, N = 8, density = 0, seed = 42)
  mod_s <- saomnk_model(density = -0.5, influence_matrix = saomnk_block_diagonal(8, 2),
                        influence_weight = 0.5)
  fs_quiet(saomnk_run(env_s, mod_s, steps_per_actor = steps_per_actor, seed = 12345,
                      shocks = list(saomnk_shock("density", parameter = -0.5, portion = 1),
                                    saomnk_shock("density", parameter = -2.0, portion = 1))))
  env_s
}

## The {K}-4 panel of a shocked run, as saomnk_plot_k4() draws it (dashed line
## at the shock, computed title and annotation), on a white background.
## clip_off = TRUE lets the shock annotation run past the panel edge, which it
## does when the shock sits near mid-run on a narrower page (the JSS layout).
fs_fig_shock <- function(env, ..., clip_off = FALSE) {
  p <- fs_restyle_k4(saomnk_plot_k4(env, ...))
  if (clip_off) p <- p + coord_cartesian(clip = "off")
  p
}
