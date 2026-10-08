#!/usr/bin/env Rscript
# =============================================================================
# make_figure1_mental_model.R
#
# Generates Figure 1 of "searchnet: Network-Embedded Strategic Search
# Simulation in R" (JSS): a mental model of the statistical model, read as the
# user's workflow one decision at a time.
#
#   paper/figures/fig1_mental_model.png   (7 in wide, 300 dpi)
#   paper/figures/fig1_mental_model.pdf
#
# Every panel is computed from ONE seeded run of the package:
#   (a) What you specify   the influence matrix W and the effect weights,
#                          exactly as passed to saomnk_model()
#   (b) The state          the bipartite network B before the focal ministep,
#                          read with saomnk_get_bipartite(env, step = t - 1)
#   (c) One decision       every option of the focal actor's ministep (toggle
#                          each component, or stay), its change in the
#                          evaluation function split by effect, and its logit
#                          choice probability
#   (d) What you read out  B after the ministep and the four {K} degrees of
#                          the focal actor and the toggled component, read
#                          with saomnk_get_bipartite() and saomnk_get_degrees()
#
# Panel (c) does not reimplement the model. The per-actor statistics s_ik(x)
# come from the engine's own env$get_struct_mod_stats_mat_from_bi_mat() (the
# statistics pinned to RSiena's targets in
# tests/testthat/test-structural-stats-vs-rsiena.R, and the stats_fun that
# searchnet_readback_check() uses), and the weights from
# env$get_bipartite_effects_theta_df(). The script then checks, for EVERY
# ministep of the run, that the probability this gives the move RSiena
# actually took equals RSiena's own recorded LogChoiceProb (env$chain_stats);
# it stops if they differ by more than 1e-8.
#
# The focal ministep is chosen by a fixed rule, not by eye: the first ministep
# in the second half of the run that adds a tie and whose taken move is the
# most probable option.
#
# The figure contains no empirical data and supports no empirical claim.
# Runtime: a few seconds.
#
# Standalone use, from the package root:
#   Rscript paper/replication/make_figure1_mental_model.R
# It is also sourced by reproduce_all.R.
# =============================================================================

## --- Load searchnet: the source tree if run from it, else the installed one --
.fig1_file <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) {
    normalizePath(f, mustWork = FALSE)
  } else {
    o <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
    if (!is.null(o)) normalizePath(o, mustWork = FALSE) else NA_character_
  }
})
.fig1_paper <- if (!is.na(.fig1_file)) dirname(dirname(.fig1_file)) else file.path("..")
if (!"package:searchnet" %in% search()) {
  .pkg_root <- dirname(.fig1_paper)
  .desc <- file.path(.pkg_root, "DESCRIPTION")
  if (file.exists(.desc) && grepl("^Package: searchnet", readLines(.desc, n = 1)) &&
      requireNamespace("pkgload", quietly = TRUE)) {
    suppressMessages(pkgload::load_all(.pkg_root, quiet = TRUE))
  } else {
    suppressPackageStartupMessages(library(searchnet))
  }
}
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

fig1_out <- file.path(.fig1_paper, "figures")
if (!dir.exists(fig1_out)) dir.create(fig1_out, recursive = TRUE)

## --- Visual grammar ---------------------------------------------------------
## Okabe-Ito, as in tools/make_readme_figures.R. One color per role:
##   orange      the focal actor and its holdings
##   vermillion  the action: the move taken and the tie it creates
##   blue        components (squares); actors are circles
##   effects     density grey, popularity purple, scope sky, influence green
oi <- c(orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
        yellow = "#F0E442", blue = "#0072B2", vermillion = "#D55E00",
        purple = "#CC79A7", black = "#000000")
eff_cols <- c(density = "grey55", inPop = oi[["purple"]],
              outAct = oi[["sky"]], XWX = oi[["green"]])
eff_names <- c(density = "density", inPop = "popularity",
               outAct = "scope", XWX = "influence")
eff_plain <- c(density = "cost of each holding",
               inPop   = "pull to popular components",
               outAct  = "cost of a wide portfolio",
               XWX     = "payoff from linked pairs")
note_col <- "grey20"
BASE <- 7

theme_fig1 <- theme_minimal(base_size = BASE) +
  theme(plot.background  = element_rect(fill = "white", color = NA),
        panel.grid       = element_blank(),
        plot.title       = element_text(face = "bold", size = BASE + 0.5,
                                        margin = margin(b = 2)),
        plot.subtitle    = element_text(color = "grey30", size = BASE - 1),
        axis.title       = element_text(size = BASE - 0.5, color = "grey25"),
        axis.text        = element_text(size = BASE - 0.5, color = "grey20"),
        plot.margin      = margin(3, 4, 3, 4))
theme_void7 <- theme_void(base_size = BASE) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.title      = element_text(face = "bold", size = BASE + 0.5,
                                       margin = margin(b = 2)),
        plot.subtitle   = element_text(color = "grey30", size = BASE - 1),
        plot.margin     = margin(3, 4, 3, 4))
## Column header, drawn as its own cell above each column: panel letter and
## name, then the API call that produces or reads the panel, in monospace.
## Wrapped so the header spans its whole cell rather than aligning to the
## indented panel area below it.
header <- function(title, code) {
  wrap_elements(full = ggplot() +
    annotate("text", x = 0, y = 1, hjust = 0, vjust = 1, label = title,
             fontface = "bold", size = (BASE + 2) / .pt) +
    annotate("text", x = 0, y = 0.6, hjust = 0, vjust = 1, label = code,
             family = "mono", size = (BASE - 1.5) / .pt, color = "grey25",
             lineheight = 1) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.margin = margin(2, 3, 0, 6)))
}

# =============================================================================
# 1. One seeded run
# =============================================================================
FIG1_M <- 4
FIG1_N <- 6
FIG1_W <- saomnk_block_diagonal(FIG1_N, 2)   # two modules: A-C and D-F
comp_lab <- LETTERS[seq_len(FIG1_N)]

fig1_env <- saomnk_env(M = FIG1_M, N = FIG1_N, density = 0.3, seed = 7)
fig1_mod <- saomnk_model(density = -1.2, popularity = 0.25, scope = -0.15,
                         influence_matrix = FIG1_W, influence_weight = 0.8)
invisible(capture.output(
  saomnk_run(fig1_env, fig1_mod, steps_per_actor = 8, seed = 2026)))

arr   <- fig1_env$bi_env_arr                 # state AFTER each ministep
chain <- fig1_env$chain_stats                # RSiena's ministep records
n_t   <- dim(arr)[3]
td    <- fig1_env$get_bipartite_effects_theta_df()
theta <- stats::setNames(as.numeric(td$initialValue), td$effect_level)
stopifnot(identical(sort(names(theta)), sort(names(eff_cols))))
theta <- theta[names(eff_cols)]

stats_of <- function(B, i)
  fig1_env$get_struct_mod_stats_mat_from_bi_mat(B)[i, names(theta)]

## Decompose one ministep: for actor i in state B, every option (toggle each
## component, then stay) with the change in each effect's contribution
## theta_k * (s_ik(after) - s_ik(before)) and the logit probability.
decode <- function(B, i) {
  s0 <- stats_of(B, i)
  contrib <- t(vapply(seq_len(FIG1_N), function(j) {
    B1 <- B
    B1[i, j] <- 1 - B1[i, j]
    theta * (stats_of(B1, i) - s0)
  }, numeric(length(theta))))
  contrib <- rbind(contrib, stay = 0)
  net <- rowSums(contrib)
  list(contrib = contrib, net = net, p = exp(net) / sum(exp(net)))
}

## Gate: our probability of the move RSiena took equals RSiena's own record,
## at every ministep. Option index: component j, or N + 1 for stay.
gate <- vapply(2:n_t, function(t) {
  d <- decode(arr[, , t - 1], chain$id_from[t])
  log(d$p[chain$id_to[t]]) - chain$LogChoiceProb[t]
}, numeric(1))
if (max(abs(gate)) > 1e-8)
  stop(sprintf("Decoded choice probabilities differ from RSiena's LogChoiceProb by %.2g.",
               max(abs(gate))))
cat(sprintf("  gate: decoded probabilities match RSiena's LogChoiceProb at all %d ministeps (max |diff| = %.1e)\n",
            length(gate), max(abs(gate))))

## Focal ministep: first in the second half that adds a tie and whose taken
## move is the most probable option.
focal_t <- NA_integer_
for (t in seq(ceiling(n_t / 2), n_t)) {
  i <- chain$id_from[t]; j <- chain$id_to[t]
  if (t < 2 || j > FIG1_N || arr[i, j, t - 1] == 1) next
  if (which.max(decode(arr[, , t - 1], i)$p) == j) { focal_t <- t; break }
}
stopifnot(!is.na(focal_t))
foc_i <- chain$id_from[focal_t]
foc_j <- chain$id_to[focal_t]
B_before <- saomnk_get_bipartite(fig1_env, step = focal_t - 1)
B_after  <- saomnk_get_bipartite(fig1_env, step = focal_t)
dec <- decode(B_before, foc_i)
stopifnot(sum(abs(B_after - B_before)) == 1, B_after[foc_i, foc_j] == 1)
cat(sprintf("  focal ministep %d of %d: actor %d adds component %s (p = %.2f)\n",
            focal_t, n_t, foc_i, comp_lab[foc_j], dec$p[foc_j]))

# =============================================================================
# 2. Panel (a): what you specify
# =============================================================================
W_df <- expand.grid(row = seq_len(FIG1_N), col = seq_len(FIG1_N))
W_df$w <- FIG1_W[cbind(W_df$row, W_df$col)]
W_df$w[W_df$row == W_df$col] <- NA            # XWX sums over j != h
held <- which(B_before[foc_i, ] == 1)
W_hit <- W_df[(W_df$row == foc_j & W_df$col %in% held) |
                (W_df$col == foc_j & W_df$row %in% held), ]
W_hit <- W_hit[!is.na(W_hit$w) & W_hit$w > 0, ]

p_W <- ggplot(W_df, aes(col, row)) +
  geom_tile(aes(fill = w), color = "grey80", linewidth = 0.25) +
  geom_tile(data = W_hit, fill = NA, color = oi[["vermillion"]], linewidth = 0.6) +
  scale_fill_gradient(low = "white", high = oi[["green"]], limits = c(0, 1),
                      na.value = "grey92", guide = "none") +
  scale_x_continuous(breaks = seq_len(FIG1_N), labels = comp_lab,
                     position = "top", expand = c(0, 0)) +
  scale_y_reverse(breaks = seq_len(FIG1_N), labels = comp_lab, expand = c(0, 0)) +
  coord_equal(clip = "off") +
  labs(title = "Influence matrix W", x = NULL, y = NULL) +
  annotate("text", x = 0.5, y = FIG1_N + 0.75, hjust = 0, vjust = 1,
           size = (BASE - 1) / .pt, color = note_col, lineheight = 0.95,
           label = "green: pairs that pay off\nwhen held together;\noutlined: pairs used in (c)") +
  theme_fig1 +
  theme(axis.text = element_text(size = BASE - 1.5),
        plot.margin = margin(3, 4, 26, 4))

## One row per effect: the effect name on the axis, its plain meaning written
## above its bar, and the weight passed to saomnk_model() at the bar's end.
wt_df <- data.frame(eff = factor(names(theta), levels = rev(names(theta))),
                    theta = unname(theta))
wt_df$name  <- eff_names[as.character(wt_df$eff)]
wt_df$plain <- eff_plain[as.character(wt_df$eff)]
wt_df$y <- as.integer(wt_df$eff)
p_wt <- ggplot(wt_df, aes(y = y)) +
  geom_vline(xintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_segment(aes(x = 0, xend = theta, yend = y, color = eff), linewidth = 1.6) +
  geom_text(aes(x = theta, label = sprintf("%+.2f", theta),
                hjust = ifelse(theta < 0, 1.15, -0.15)),
            size = (BASE - 1.5) / .pt, color = "grey20") +
  geom_text(aes(x = -1.75, y = y + 0.38, label = plain), hjust = 0,
            size = (BASE - 1.5) / .pt, color = note_col, fontface = "italic") +
  scale_color_manual(values = eff_cols, guide = "none") +
  scale_y_continuous(breaks = wt_df$y, labels = wt_df$name,
                     limits = c(0.6, length(theta) + 0.6)) +
  scale_x_continuous(limits = c(-1.75, 1.35), breaks = c(-1, 0, 1)) +
  labs(title = "Effect weights", x = "weight", y = NULL) +
  theme_fig1 +
  theme(axis.text.y = element_text(size = BASE - 1, hjust = 1),
        axis.line.x = element_line(color = "grey60", linewidth = 0.3),
        axis.ticks.x = element_line(color = "grey60", linewidth = 0.3))


# =============================================================================
# 3. Bipartite network drawing (panels b and d)
# =============================================================================
## Two columns: actors (circles) left, components (squares) right, in the
## same positions in (b) and (d) so the one changed tie is easy to find.
act_y  <- seq(5.25, 1.75, length.out = FIG1_M)
comp_y <- seq(6, 1, length.out = FIG1_N)

draw_net <- function(B, title, new_tie = NULL) {
  ed <- which(B == 1, arr.ind = TRUE)
  ed <- data.frame(i = ed[, 1], j = ed[, 2])
  ed$role <- ifelse(ed$i == foc_i, "focal", "other")
  if (!is.null(new_tie))
    ed$role[ed$i == new_tie[1] & ed$j == new_tie[2]] <- "new"
  ed$x <- 0; ed$y <- act_y[ed$i]; ed$xend <- 1; ed$yend <- comp_y[ed$j]
  ed$role <- factor(ed$role, levels = c("other", "focal", "new"))
  ed <- ed[order(ed$role), ]
  an <- data.frame(x = 0, y = act_y, lab = seq_len(FIG1_M),
                   fill = ifelse(seq_len(FIG1_M) == foc_i, oi[["orange"]], "grey85"))
  cn <- data.frame(x = 1, y = comp_y, lab = comp_lab)
  ggplot() +
    geom_segment(data = ed, aes(x = x, y = y, xend = xend, yend = yend,
                                color = role, linewidth = role)) +
    scale_color_manual(values = c(other = "grey70", focal = oi[["orange"]],
                                  new = oi[["vermillion"]]), guide = "none") +
    scale_linewidth_manual(values = c(other = 0.45, focal = 0.9, new = 1.6),
                           guide = "none") +
    geom_point(data = an, aes(x, y), shape = 21, size = 5, fill = an$fill,
               color = "grey30", stroke = 0.4) +
    geom_text(data = an, aes(x, y, label = lab), size = BASE / .pt,
              fontface = "bold", color = "grey10") +
    geom_point(data = cn, aes(x, y), shape = 22, size = 4.8, fill = oi[["blue"]],
               color = "grey20", stroke = 0.3) +
    geom_text(data = cn, aes(x, y, label = lab), size = BASE / .pt,
              fontface = "bold", color = "white") +
    annotate("text", x = c(0, 1), y = 0.35, label = c("actors", "components"),
             size = (BASE - 1) / .pt, color = "grey30") +
    coord_cartesian(xlim = c(-0.3, 1.3), ylim = c(0.25, 6.3), clip = "off") +
    labs(title = title) +
    theme_void7
}

## B as the 0/1 matrix saomnk_get_bipartite() returns, focal row in orange.
draw_mat <- function(B, title, new_tie = NULL) {
  df <- expand.grid(i = seq_len(FIG1_M), j = seq_len(FIG1_N))
  df$x <- B[cbind(df$i, df$j)]
  df$fill <- ifelse(df$x == 0, "white", ifelse(df$i == foc_i, oi[["orange"]], "grey45"))
  if (!is.null(new_tie))
    df$fill[df$i == new_tie[1] & df$j == new_tie[2]] <- oi[["vermillion"]]
  ggplot(df, aes(j, i)) +
    geom_tile(fill = df$fill, color = "grey75", linewidth = 0.3) +
    geom_text(aes(label = x), size = (BASE - 1.5) / .pt,
              color = ifelse(df$x == 1, "white", "grey60")) +
    annotate("rect", xmin = 0.5, xmax = FIG1_N + 0.5, ymin = foc_i - 0.5,
             ymax = foc_i + 0.5, fill = NA, color = oi[["orange"]], linewidth = 0.7) +
    scale_x_continuous(breaks = seq_len(FIG1_N), labels = comp_lab,
                       position = "top", expand = c(0, 0)) +
    scale_y_reverse(breaks = seq_len(FIG1_M), expand = c(0, 0)) +
    coord_equal(clip = "off") +
    labs(title = title, x = NULL, y = NULL) +
    theme_fig1 + theme(axis.text = element_text(size = BASE - 1.5))
}

# =============================================================================
# 4. Panel (b): the state
# =============================================================================
p_b_net <- draw_net(B_before, "Network B, before") +
  annotate("text", x = -0.3, y = act_y[foc_i] + 0.95, hjust = 0, vjust = 0,
           size = (BASE - 1) / .pt, color = note_col, lineheight = 0.95,
           label = sprintf("actor %d gets\nto move next", foc_i)) +
  annotate("segment", x = -0.2, xend = -0.07, y = act_y[foc_i] + 0.9,
           yend = act_y[foc_i] + 0.3, color = note_col, linewidth = 0.3,
           arrow = arrow(length = unit(0.05, "in"), type = "closed"))
p_b_mat <- draw_mat(B_before, "B as a matrix")

# =============================================================================
# 5. Panel (c): one decision, decoded
# =============================================================================
opt_lab <- c(ifelse(B_before[foc_i, ] == 1, paste("drop", comp_lab), paste("add", comp_lab)),
             "stay")
opt_lev <- rev(opt_lab)
cdf <- do.call(rbind, lapply(names(theta), function(k)
  data.frame(opt = opt_lab, eff = k, v = dec$contrib[, k])))
cdf$opt <- factor(cdf$opt, levels = opt_lev)
cdf$eff <- factor(cdf$eff, levels = names(theta))
ndf <- data.frame(opt = factor(opt_lab, levels = opt_lev), net = dec$net,
                  p = dec$p, taken = seq_along(opt_lab) == foc_j)
ndf$plab <- ifelse(ndf$taken, sprintf("%.2f drawn", ndf$p), sprintf("%.2f", ndf$p))
taken_lab <- opt_lab[foc_j]
xr <- range(c(0, tapply(pmin(cdf$v, 0), cdf$opt, sum), tapply(pmax(cdf$v, 0), cdf$opt, sum)))
xr <- c(floor(xr[1]) - 0.2, ceiling(xr[2]) + 0.2)
y_taken <- match(taken_lab, opt_lev)
shade <- annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_taken - 0.5,
                  ymax = y_taken + 0.5, fill = oi[["vermillion"]], alpha = 0.13)

p_c_bar <- ggplot(cdf, aes(x = v, y = opt)) +
  shade +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_col(aes(fill = eff), width = 0.68, orientation = "y") +
  geom_point(data = ndf, aes(x = net, y = opt), shape = 21, size = 1.7,
             fill = "black", color = "white", stroke = 0.4) +
  scale_fill_manual(values = eff_cols, labels = eff_names, name = NULL) +
  scale_x_continuous(limits = xr, breaks = seq(ceiling(xr[1]), floor(xr[2]), 1)) +
  labs(title = "Payoff change",
       x = "change, by effect", y = NULL) +
  theme_fig1 +
  theme(axis.line.x = element_line(color = "grey60", linewidth = 0.3),
        axis.ticks.x = element_line(color = "grey60", linewidth = 0.3),
        legend.position = "bottom",
        legend.key.size = unit(0.1, "in"),
        legend.text = element_text(size = BASE - 1, margin = margin(l = 1, r = 3)),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-4, 0, 0, 0)) +
  guides(fill = guide_legend(nrow = 2))

p_c_prob <- ggplot(ndf, aes(x = p, y = opt)) +
  shade +
  geom_col(aes(fill = taken), width = 0.68, orientation = "y") +
  geom_text(aes(label = plab), hjust = -0.1, size = (BASE - 1.5) / .pt,
            color = ifelse(ndf$taken, oi[["vermillion"]], "grey20"),
            fontface = ifelse(ndf$taken, "bold", "plain")) +
  scale_fill_manual(values = c(`FALSE` = "grey60", `TRUE` = oi[["vermillion"]]),
                    guide = "none") +
  scale_x_continuous(limits = c(0, 1.25), breaks = c(0, 0.5, 1),
                     labels = c("0", ".5", "1"), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "Chance", x = "probability", y = NULL) +
  theme_fig1 +
  theme(axis.text.y = element_blank(),
        axis.line.x = element_line(color = "grey60", linewidth = 0.3),
        axis.ticks.x = element_line(color = "grey60", linewidth = 0.3))

c_note <- ggplot() +
  annotate("text", x = 0, y = 1, hjust = 0, vjust = 1, size = (BASE - 0.5) / .pt,
           color = note_col, lineheight = 1.05,
           label = paste0(
             "Each row: one move actor ", foc_i, " could make.\n",
             "Bars: what each effect adds or takes away;\n",
             "dot: the net change.\n",
             "Higher net change, higher chance. The actor\n",
             "draws one move; shaded: the one drawn.")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
  theme_void() + theme(plot.margin = margin(4, 4, 2, 8))

p_c <- ((p_c_bar | p_c_prob) + plot_layout(widths = c(2.1, 1))) / c_note +
  plot_layout(heights = c(3.0, 1))

# =============================================================================
# 6. Panel (d): what you read out
# =============================================================================
mid_y <- (act_y[foc_i] + comp_y[foc_j]) / 2
p_d_net <- draw_net(B_after, "Network B, after", new_tie = c(foc_i, foc_j)) +
  annotate("text", x = 0.42, y = 6.25, hjust = 0.5, vjust = 0,
           size = (BASE - 1) / .pt, color = oi[["vermillion"]], fontface = "bold",
           label = sprintf("new tie %d-%s", foc_i, comp_lab[foc_j])) +
  annotate("segment", x = 0.45, xend = 0.5 + 0.02, y = 6.15, yend = mid_y + 0.2,
           color = note_col, linewidth = 0.3,
           arrow = arrow(length = unit(0.05, "in"), type = "closed"))

K_get <- function(k, id, t) {
  df <- as.data.frame(saomnk_get_degrees(fig1_env)[[k]])
  idcol <- if (k %in% c("K_AC", "K_AA")) "actor_id" else "component_id"
  df$value[df$chain_step_id == t & as.integer(as.character(df[[idcol]])) == id]
}
kdf <- data.frame(
  k     = c("K_AC", "K_CA", "K_AA", "K_CC"),
  plain = c("scope", "popularity", "sociality", "coupling"),
  id    = c(foc_i, foc_j, foc_i, foc_j))
kdf$who <- ifelse(kdf$k %in% c("K_AC", "K_AA"), as.character(foc_i), comp_lab[foc_j])
kdf$before <- mapply(K_get, kdf$k, kdf$id, focal_t - 1)
kdf$after  <- mapply(K_get, kdf$k, kdf$id, focal_t)
kdf$lab <- sprintf("K[%s]*'(%s) %s'", sub("K_", "", kdf$k), kdf$who, kdf$plain)
kdf$row <- rev(seq_len(nrow(kdf)))      # first row on top
kdf$moved <- kdf$after != kdf$before
cat("  {K} before -> after:", paste(sprintf("%s %g->%g", kdf$k, kdf$before, kdf$after),
                                    collapse = "; "), "\n")
kmax <- max(c(kdf$before, kdf$after))
p_d_k <- ggplot(kdf, aes(y = row)) +
  geom_segment(data = kdf[kdf$moved, ], aes(x = before, xend = after - 0.16, yend = row),
               color = oi[["vermillion"]], linewidth = 0.6,
               arrow = arrow(length = unit(0.045, "in"), type = "closed")) +
  geom_point(aes(x = before), shape = 21, size = 1.9, fill = "white", color = "grey35") +
  geom_point(aes(x = after), shape = 21, size = 1.9,
             fill = ifelse(kdf$moved, oi[["vermillion"]], "grey35"),
             color = ifelse(kdf$moved, oi[["vermillion"]], "grey35")) +
  geom_text(data = kdf[!kdf$moved, ], aes(x = after, label = "same"),
            hjust = -0.35, size = (BASE - 1.5) / .pt, color = "grey40") +
  scale_y_continuous(breaks = kdf$row, labels = parse(text = kdf$lab),
                     limits = c(0.6, nrow(kdf) + 0.4)) +
  scale_x_continuous(limits = c(0.5, kmax + 0.9), breaks = 1:kmax) +
  labs(title = "Degrees", subtitle = "open: before; filled: after",
       x = "degree", y = NULL) +
  theme_fig1 +
  theme(axis.text.y = element_text(size = BASE - 1, hjust = 1),
        plot.subtitle = element_text(size = BASE - 1.5, color = "grey30",
                                     margin = margin(b = 1)),
        panel.grid.major.x = element_line(color = "grey92", linewidth = 0.25),
        axis.line.x = element_line(color = "grey60", linewidth = 0.3))

# =============================================================================
# 7. Assemble: a header row, then the panels, left to right
# =============================================================================
h_a <- header("(a) What you specify", "saomnk_model()\nsaomnk_env()")
h_b <- header("(b) The state", sprintf("saomnk_get_bipartite(\n  env, %d)", focal_t - 1))
h_c <- header("(c) One decision, decoded",
              sprintf("saomnk_run(): ministep %d of %d\nenv$get_struct_mod_stats_mat_from_bi_mat()",
                      focal_t, n_t))
h_d <- header("(d) What you read out",
              sprintf("saomnk_get_bipartite(env, %d)\nsaomnk_get_degrees(env)", focal_t))

design <- c(
  area(1, 1, 1, 1), area(1, 2, 1, 2), area(1, 3, 1, 3), area(1, 4, 1, 4),   # headers
  area(2, 1, 2, 1), area(2, 2, 2, 2), area(2, 3, 3, 3), area(2, 4, 2, 4),   # tops; c spans
  area(3, 1, 3, 1), area(3, 2, 3, 2),                       area(3, 4, 3, 4)    # bottoms
)
fig1 <- wrap_plots(h_a, h_b, h_c, h_d,
                   p_W, p_b_net, p_c, p_d_net,
                   p_wt, p_b_mat, p_d_k,
                   design = design) +
  plot_layout(widths = c(1.35, 1.15, 2.05, 1.45), heights = c(0.5, 2.0, 1.55)) &
  theme(plot.background = element_rect(fill = "white", color = NA))

FIG1_W_IN <- 7.0
FIG1_H_IN <- 4.3
png_path <- file.path(fig1_out, "fig1_mental_model.png")
pdf_path <- file.path(fig1_out, "fig1_mental_model.pdf")
if (requireNamespace("ragg", quietly = TRUE)) {
  ggsave(png_path, fig1, width = FIG1_W_IN, height = FIG1_H_IN, dpi = 300,
         bg = "white", device = ragg::agg_png)
} else {
  ggsave(png_path, fig1, width = FIG1_W_IN, height = FIG1_H_IN, dpi = 300, bg = "white")
}
ggsave(pdf_path, fig1, width = FIG1_W_IN, height = FIG1_H_IN, bg = "white",
       device = grDevices::pdf, useDingbats = FALSE)
cat(sprintf("  wrote %s (%.0f KB)\n  wrote %s (%.0f KB)\n",
            png_path, file.size(png_path) / 1024, pdf_path, file.size(pdf_path) / 1024))
