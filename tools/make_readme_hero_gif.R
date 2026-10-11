#!/usr/bin/env Rscript
###############################################################################
## make_readme_hero_gif.R
##
## Animated version of the README hero figure (man/figures/readme-hero.png,
## section 1 of tools/make_readme_figures.R): the same seeded run, played
## ministep by ministep. Writes man/figures/readme-hero.gif.
##
## Usage, from the package root:
##
##     Rscript tools/make_readme_hero_gif.R
##
## Layout, as in the static figure:
##   left:  the influence matrix W (static), and below it ONE network panel
##          that evolves over the run on the static figure's fixed
##          Fruchterman-Reingold layout (the union of start and end ties), so
##          nodes never move. Ties present at the start are light grey, ties
##          formed since the start dark; a tie just dropped is drawn dashed
##          and fades out over a few frames. The last frame shows the end ties
##          only, as the static figure's end panel does.
##   right: the {K}-4 panel of saomnk_plot_k4(). Points are revealed up to the
##          current ministep; the group and mean loess lines are the static
##          figure's own lines (computed once, on the whole run, by
##          ggplot_build()) revealed progressively, so their past never changes
##          and the last frame is the static panel. A dashed vertical line
##          marks the current ministep. Axes are the full-run limits.
##
## Frames are rendered with ggplot2/patchwork to PNG in a temporary directory,
## then ffmpeg (on PATH) builds the GIF with a two-pass palette. Seeds are
## fixed, so the run is deterministic for a given searchnet, RSiena and R
## version. Only the GIF is written into the package.
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
})
source(file.path("tools", "figures_shared.R"))
oi           <- fs_oi
theme_readme <- fs_theme(11)

out_gif <- file.path("man", "figures", "readme-hero.gif")
work    <- Sys.getenv("SEARCHNET_GIF_WORKDIR", file.path(tempdir(), "readme_hero_gif"))
unlink(work, recursive = TRUE)
dir.create(work, recursive = TRUE, showWarnings = FALSE)

frame_every <- as.integer(Sys.getenv("SEARCHNET_HERO_EVERY", "2"))  # ministeps per frame
fps         <- 12L
hold_last   <- 3       # seconds on the last frame
fade_steps  <- 8L      # ministeps over which a dropped tie fades (4 frames)


## ---- 1. the run (identical to make_readme_figures.R, section 1) -------------
M <- 8; N <- 12
W_hero  <- saomnk_block_diagonal(N, 3)          # three modules of four
module  <- rep(1:3, each = 4)
strat   <- rep(c(-1, 1), length.out = M)

env <- saomnk_env(M = M, N = N, density = 0.15, seed = 42)
mod <- saomnk_model(density = -1.5, popularity = 0,
                    influence_matrix = W_hero, influence_weight = 0.5,
                    strategies = list(egoX = strat))
B0 <- saomnk_get_bipartite(env)                 # the state before the run
fs_quiet(saomnk_run(env, mod, steps_per_actor = 30, seed = 12345))
B  <- saomnk_get_bipartite(env)                 # the state after it

## bi_env_arr[, , k] is the state after ministep k (k = 1..T); ministep 0 is B0.
arr <- env$bi_env_arr
T_steps <- dim(arr)[3]
state_at <- function(t) if (t == 0) B0 else arr[, , t]
stopifnot(identical(unname(state_at(T_steps) == 1), unname(B == 1)))

## Same layout call and seed as the static script.
set.seed(7)
xy <- igraph::layout_with_fr(igraph::graph_from_biadjacency_matrix((B0 + B) > 0, directed = FALSE))


## ---- 2. network panel -------------------------------------------------------
node_group <- factor(
  c(ifelse(strat < 0, "Actors: strategy -1", "Actors: strategy 1"), paste("Components: module", module)),
  levels = c("Actors: strategy -1", "Actors: strategy 1", paste("Components: module", 1:3)))
nodes <- data.frame(x = xy[, 1], y = xy[, 2], group = node_group,
                    kind = rep(c("actor", "component"), c(M, N)),
                    lab = c(as.character(seq_len(M)), LETTERS[seq_len(N)]))
net_cols <- c("Actors: strategy -1" = oi[["orange"]], "Actors: strategy 1" = oi[["sky"]],
              "Components: module 1" = oi[["green"]], "Components: module 2" = oi[["blue"]],
              "Components: module 3" = oi[["purple"]])
pairs <- expand.grid(i = seq_len(M), j = seq_len(N))
pairs$x <- xy[pairs$i, 1];     pairs$y <- xy[pairs$i, 2]
pairs$xend <- xy[M + pairs$j, 1]; pairs$yend <- xy[M + pairs$j, 2]
pad_x <- diff(range(xy[, 1])) * 0.04; pad_y <- diff(range(xy[, 2])) * 0.04
net_xlim <- range(xy[, 1]) + c(-pad_x, pad_x)
net_ylim <- range(xy[, 2]) + c(-pad_y, pad_y)

## drop_hist[, , t + 1]: ministep at which each tie was last dropped, up to t.
drop_hist <- array(NA_real_, c(M, N, T_steps + 1))
{
  ld <- matrix(NA_real_, M, N)
  for (t in 0:T_steps) {
    if (t >= 1) ld[state_at(t - 1) == 1 & state_at(t) == 0] <- t
    drop_hist[, , t + 1] <- ld
  }
}

net_frame <- function(t) {
  S  <- state_at(t)
  ld <- drop_hist[, , t + 1]
  e  <- pairs
  e$now   <- S[cbind(e$i, e$j)] == 1
  e$start <- B0[cbind(e$i, e$j)] == 1
  e$ago   <- t - ld[cbind(e$i, e$j)]
  e$status <- ifelse(e$now, ifelse(e$start, "kept", "formed"),
                     ifelse(!is.na(e$ago) & e$ago < fade_steps & t < T_steps, "dropped", NA))
  e <- e[!is.na(e$status), ]
  e$alpha <- ifelse(e$status == "dropped", 1 - e$ago / fade_steps, 1)
  e$status <- factor(e$status, levels = c("kept", "formed", "dropped"))
  ## draw order: light and fading ties under the dark (formed) ones
  e <- e[order(e$status == "formed"), ]
  ggplot() +
    geom_segment(data = e, aes(x, y, xend = xend, yend = yend, colour = status,
                               linetype = status, alpha = alpha), linewidth = 0.6) +
    scale_colour_manual(values = c(kept = "grey72", formed = "grey25", dropped = "grey45"),
                        guide = "none", drop = FALSE) +
    scale_linetype_manual(values = c(kept = "solid", formed = "solid", dropped = "22"),
                          guide = "none", drop = FALSE) +
    scale_alpha_identity() +
    node_layers(nodes) +
    labs(title = "The network over the run",
         subtitle = sprintf(paste0("ministep %d of %d: %d ties\n",
                                   "light: held at the start; dark: formed since;",
                                   " dashed: just dropped"),
                            t, T_steps, sum(S))) +
    coord_cartesian(xlim = net_xlim, ylim = net_ylim, clip = "off") +
    theme_readme +
    theme(panel.grid = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank()) +
    guides(fill = guide_legend(nrow = 3,
                               override.aes = list(size = 4, shape = c(21, 21, 22, 22, 22))))
}
## Nodes use the fill aesthetic (filled shapes 21/22 with no border), so the
## edge colour scale and the node colour legend stay separate. They look as the
## static figure's solid circles and squares.
node_layers <- function(nodes) {
  list(
    geom_point(data = nodes, aes(x, y, fill = group, shape = kind), size = 6.5,
               stroke = 0, colour = "transparent"),
    geom_text(data = nodes, aes(x, y, label = lab), colour = "white", size = 2.9,
              fontface = "bold"),
    scale_shape_manual(values = c(actor = 21, component = 22), guide = "none"),
    scale_fill_manual(values = net_cols, name = NULL, drop = FALSE)
  )
}


## ---- 3. {K}-4 panel: the static panel, built once ---------------------------
## The hero's {K}-4 panel exactly as the static script draws it, plus a
## vertical marker layer (placed inside the x range so it does not widen it).
p_k4 <- fs_restyle_k4(saomnk_plot_k4(env)) +
  guides(color = guide_legend(nrow = 2,
                              override.aes = list(alpha = 1, shape = NA, linewidth = 1))) +
  geom_vline(xintercept = T_steps, colour = "grey35", linetype = "22", linewidth = 0.5) +
  theme(legend.position = "bottom")
stopifnot(length(p_k4$layers) == 5L,
          mapply(function(l, g) inherits(l$geom, g), p_k4$layers,
                 c("GeomPoint", "GeomSmooth", "GeomSmooth", "GeomText", "GeomVline")))
k4_built <- ggplot_build(p_k4)
full_data <- k4_built$data

## Reveal a smooth layer's precomputed curve up to x = t, ending exactly at t.
reveal_line <- function(d, t) {
  if (t < min(d$x)) return(d[0, ])
  out <- lapply(split(d, list(d$PANEL, d$group), drop = TRUE), function(g) {
    g <- g[order(g$x), ]
    keep <- g[g$x <= t, ]
    if (t < max(g$x) && nrow(keep) > 0) {
      tip <- keep[nrow(keep), ]
      tip$x <- t
      tip$y <- stats::approx(g$x, g$y, xout = t)$y
      keep <- rbind(keep, tip)
    }
    keep
  })
  do.call(rbind, out)
}

k4_frame <- function(t) {
  b <- k4_built
  d <- full_data
  d[[1]] <- d[[1]][d[[1]]$x <= t, ]
  d[[2]] <- reveal_line(d[[2]], t)
  d[[3]] <- reveal_line(d[[3]], t)
  d[[4]] <- d[[4]][d[[4]]$x <= t, ]
  d[[5]]$xintercept <- t
  if (t >= T_steps) d[[5]] <- d[[5]][0, ]        # no marker on the final frame
  b$data <- d
  ggplot_gtable(b)
}


## ---- 4. compose and render frames ------------------------------------------
p_w <- fs_w_heatmap(W_hero, "Influence matrix W (input)",
                    "saomnk_block_diagonal(12, 3)\ndiagonal unused (XWX: j != h)",
                    labels = LETTERS[seq_len(N)])

frame_plot <- function(t) {
  left_col <- (p_w + theme(plot.margin = margin(5.5, 5.5, 30, 5.5))) / net_frame(t) +
    plot_layout(heights = c(0.85, 1.6), guides = "collect") &
    theme(legend.position = "bottom")
  left_col - wrap_elements(full = k4_frame(t)) + plot_layout(widths = c(1, 2))
}

steps <- unique(c(0L, seq(frame_every, T_steps, by = frame_every), T_steps))
cat(sprintf("rendering %d frames (ministeps 0..%d, every %d)\n",
            length(steps), T_steps, frame_every))
for (k in seq_along(steps)) {
  f <- file.path(work, sprintf("frame_%04d.png", k))
  ragg::agg_png(f, width = 12, height = 11, units = "in", res = 100, background = "white")
  print(frame_plot(steps[k]))
  invisible(dev.off())
  if (k %% 20 == 0) cat(sprintf("  frame %d / %d\n", k, length(steps)))
}


## ---- 5. end-state check -----------------------------------------------------
## The last frame shows state_at(T) and the full lines. Compare with the static
## figure's end state: the end ties B, and the static panel's own lines (a
## fresh, unmodified build of the same panel).
static_built <- ggplot_build(fs_restyle_k4(saomnk_plot_k4(env)))
last <- function(d) {
  d <- d[order(d$PANEL, d$group, d$x), ]
  d[!duplicated(d[, c("PANEL", "group")], fromLast = TRUE), c("PANEL", "group", "x", "y")]
}
end_lines_anim   <- rbind(last(reveal_line(full_data[[2]], T_steps)),
                          last(reveal_line(full_data[[3]], T_steps)))
end_lines_static <- rbind(last(static_built$data[[2]]), last(static_built$data[[3]]))
tie_ok  <- identical(unname(state_at(T_steps) == 1), unname(B == 1))
line_ok <- isTRUE(all.equal(end_lines_anim, end_lines_static, check.attributes = FALSE))
pts_ok  <- nrow(full_data[[1]][full_data[[1]]$x <= T_steps, ]) == nrow(static_built$data[[1]])
Kdf <- as.data.frame(env$get_K4_df())
k_means <- tapply(Kdf$value[Kdf$chain_step_id == T_steps],
                  Kdf$effect[Kdf$chain_step_id == T_steps], mean)
black_end <- last(static_built$data[[3]])$y
cat(sprintf("end-state check: ties %s (%d ties, %d formed, %d dropped); lines %s; points %s\n",
            if (tie_ok) "match" else "DIFFER", sum(B), sum(B == 1 & B0 == 0),
            sum(B0 == 1 & B == 0), if (line_ok) "match" else "DIFFER",
            if (pts_ok) "match" else "DIFFER"))
cat("  mean K at the last ministep: ",
    paste(sprintf("%s %.2f", names(k_means), k_means), collapse = ", "), "\n")
cat("  black mean line at its right end (panels 1-4): ",
    paste(sprintf("%.2f", black_end), collapse = ", "), "\n")
stopifnot(tie_ok, line_ok, pts_ok)


## ---- 6. GIF (ffmpeg, two-pass palette) --------------------------------------
pal <- file.path(work, "palette.png")
pattern <- file.path(work, "frame_%04d.png")
vf <- sprintf("tpad=stop_mode=clone:stop_duration=%g", hold_last)
system2("ffmpeg", c("-v", "error", "-y", "-framerate", fps, "-i", shQuote(pattern),
                    "-vf", shQuote(paste0(vf, ",palettegen=max_colors=128:stats_mode=full")),
                    shQuote(pal)))
system2("ffmpeg", c("-v", "error", "-y", "-framerate", fps, "-i", shQuote(pattern),
                    "-i", shQuote(pal),
                    "-lavfi", shQuote(paste0(vf, "[x];[x][1:v]paletteuse=dither=none:diff_mode=rectangle")),
                    "-loop", "0", shQuote(out_gif)))
stopifnot(file.exists(out_gif))
cat(sprintf("wrote %s (%.2f MB, %d frames + %gs hold at %d fps) in %.0f s\n",
            out_gif, file.size(out_gif) / 2^20, length(steps), hold_last, fps,
            as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
