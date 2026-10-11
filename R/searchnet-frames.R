###############################################################################
## searchnet-frames.R
##
## Frames, filmstrips and animations of a stored run, in the style of the
## README hero animation (tools/make_readme_hero_gif.R):
##
##   searchnet_frames_prepare()  the per-run precompute every frame reuses: a
##                               fixed network layout, the {K}-4 panel built
##                               once, the W heatmap, a landscape cache
##   searchnet_frame()           one composed frame at one ministep
##   searchnet_filmstrip()       several frames on one page, or one per page
##   searchnet_animate()         a GIF of the run (gifski, ffmpeg or magick)
##
## All of them read only what saomnk_run() stored on the env (bi_env_arr, the
## K4 data, theta_matrix); none simulates. The {K}-4 view is the panel of
## saomnk_plot_k4(), built once by ggplot_build() and revealed up to the
## current ministep, so its past never changes, its axes are the full-run
## axes, and the frame at the last ministep carries the static panel's data.
##
## The helpers below reimplement, inside the package, the README figure
## builders the hero script sources from tools/figures_shared.R (fs_oi,
## fs_theme, fs_w_heatmap), since tools/ does not ship as package code. The
## tools/ scripts keep their own copies, so the README figures are unchanged.
###############################################################################

## ---- shared styling --------------------------------------------------------

.sn_frames_theme <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.title       = element_text(face = "bold", size = base_size + 1),
          plot.subtitle    = element_text(color = "grey30", size = base_size - 1.5),
          legend.position  = "bottom")
}

## Heatmap of an influence matrix W; the diagonal is drawn light gray because
## the XWX statistic sums over j != h and never reads it.
.sn_frames_w_heatmap <- function(W, title = "Influence matrix W",
                                 subtitle = "diagonal unused (XWX: j != h)",
                                 labels = seq_len(nrow(W)), base_size = 11) {
  N <- nrow(W)
  W <- unname(as.matrix(W))
  diag(W) <- NA
  lim <- c(min(0, W, na.rm = TRUE), max(1, W, na.rm = TRUE))
  df <- data.frame(row = rep(seq_len(N), times = N),
                   col = rep(seq_len(N), each = N),
                   w   = as.vector(W))
  fill <- if (lim[1] < 0) {
    scale_fill_gradient2(low = .searchnet_diverging[1], mid = "white",
                         high = .searchnet_diverging[3], midpoint = 0,
                         na.value = "grey90", guide = "none")
  } else {
    scale_fill_gradient(low = "white", high = .searchnet_okabe_ito[["blue"]],
                        limits = lim, na.value = "grey90", guide = "none")
  }
  ggplot(df, aes(.data$col, .data$row, fill = .data$w)) +
    geom_tile(color = "grey85", linewidth = 0.3) +
    scale_y_reverse(breaks = seq_len(N), labels = labels, expand = c(0, 0)) +
    scale_x_continuous(breaks = seq_len(N), labels = labels, position = "top",
                       expand = c(0, 0)) +
    fill +
    coord_equal() +
    labs(title = title, subtitle = subtitle, x = "component j", y = "component i") +
    .sn_frames_theme(base_size) +
    theme(panel.grid = element_blank(), axis.ticks = element_blank(),
          axis.text = element_text(size = 7 * base_size / 11))
}

## The single static influence matrix of the run's model, or NULL.
.sn_frames_get_w <- function(env) {
  cd <- tryCatch(env$config_structure_model$dv_bipartite$coDyadCovars,
                 error = function(e) NULL)
  for (e in cd) {
    if (identical(e$effect, "XWX") && is.matrix(e$x)) return(unname(as.matrix(e$x)))
  }
  NULL
}

## Run fn with a fixed seed, leaving the caller's RNG state as it was.
.sn_frames_with_seed <- function(seed, fn) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  })
  set.seed(seed)
  fn()
}


## ---- precompute ------------------------------------------------------------

#' Prepare a stored run for frames, filmstrips and animations
#'
#' Computes, once per run, everything that [searchnet_frame()],
#' [searchnet_filmstrip()] and [searchnet_animate()] reuse across frames: the
#' fixed network layout, the K-4 panel of [saomnk_plot_k4()] built once
#' (so each frame reveals its data up to the current ministep on the
#' full-run axes), the influence matrix heatmap, and a cache for the
#' landscape view. Passing the returned object instead of the env to those
#' functions skips this work; passing the env prepares it on the fly.
#'
#' Nothing is simulated: the object reads the path that [saomnk_run()]
#' stored on the env. Ministep `0` is the state before the first ministep,
#' and ministep `t` the state after it.
#'
#' @param env A `SaomNkRSienaBiEnv` after [saomnk_run()].
#' @param views Character vector, any of `"network"`, `"k4"`, `"landscape"`
#'   and `"w"`: the views to prepare. See [searchnet_frame()].
#' @param actor Integer, the focal actor of the landscape view. `NULL`
#'   (default) picks the actor who changes a tie most often over the run
#'   (the lowest index on ties).
#' @param layout Node positions for the network view: `NULL` (default)
#'   computes a Fruchterman-Reingold layout on the union of every tie held at
#'   any ministep of the run, with a fixed seed, so nodes never move between
#'   frames; or a numeric matrix with \eqn{M + N} rows (actors first) and two
#'   columns.
#' @param smooth Loess span of the K-4 panel, passed to [saomnk_plot_k4()].
#' @param seed Integer seed of the default layout. The caller's random
#'   number stream is left unchanged.
#' @param base_size Base font size of the views.
#' @param max_n Largest number of components for which the landscape view
#'   is drawn (it evaluates all \eqn{2^N} portfolios).
#'
#' @return An object of class `searchnet_frames`: a list holding the env,
#'   the number of ministeps `n_steps`, the start state `B0`, the layout, the
#'   built K-4 panel and the other per-run pieces. Its `print()` method
#'   summarizes it.
#'
#' @seealso [searchnet_frame()], [searchnet_filmstrip()], [searchnet_animate()]
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 5, N = 6, density = 0.2, seed = 42)
#' mod <- saomnk_model(density = -1.5,
#'                     influence_matrix = saomnk_block_diagonal(6, 2),
#'                     influence_weight = 0.5)
#' invisible(capture.output(saomnk_run(env, mod, steps_per_actor = 6, seed = 1)))
#' fr <- searchnet_frames_prepare(env)
#' fr
#' }
#' @export
searchnet_frames_prepare <- function(env, views = c("network", "k4"), actor = NULL,
                                     layout = NULL, smooth = 0.3, seed = 7L,
                                     base_size = 11, max_n = 12L) {
  if (inherits(env, "searchnet_frames")) return(env)
  stopifnot(inherits(env, "SaomNkRSienaBiEnv"))
  .searchnet_require_path(env, "searchnet_frames_prepare()")
  views <- .sn_frames_views(views)
  arr <- env$bi_env_arr
  n_t <- dim(arr)[3]
  B0 <- .sn_view_state(env, 0L)$B
  M <- nrow(B0); N <- ncol(B0)
  state_at <- function(t) if (t == 0L) B0 else unname(as.matrix(arr[, , t]))

  ## Moves per actor: the actor whose row changed at each ministep.
  moves <- integer(M)
  prev <- B0
  for (t in seq_len(n_t)) {
    S <- state_at(t)
    d <- which(rowSums(S != prev) > 0)
    moves[d] <- moves[d] + 1L
    prev <- S
  }

  fr <- list(env = env, views = views, n_steps = n_t, M = M, N = N, B0 = B0,
             B_end = state_at(n_t), moves = moves, base_size = base_size,
             max_n = as.integer(max_n), smooth = smooth,
             cache = new.env(parent = emptyenv()))
  fr$state_at <- state_at
  fr$W <- .sn_frames_get_w(env)
  fr$labels <- .sn_component_labels(N)
  fr$actor <- if (is.null(actor)) which.max(moves) else .sn_view_actor(env, actor)

  ## Network layout: FR on the union of all ties over the run.
  if (is.null(layout)) {
    U <- B0 > 0
    for (t in seq_len(n_t)) U <- U | (arr[, , t] > 0)
    g <- igraph::graph_from_biadjacency_matrix(U * 1, directed = FALSE)
    layout <- .sn_frames_with_seed(seed, function() igraph::layout_with_fr(g))
  }
  layout <- as.matrix(layout)
  if (!is.numeric(layout) || nrow(layout) != M + N || ncol(layout) < 2L)
    stop(sprintf("layout must be a numeric matrix with M + N = %d rows and 2 columns", M + N),
         call. = FALSE)
  fr$layout <- layout[, 1:2, drop = FALSE]
  fr$net <- .sn_frames_net_setup(fr)

  if ("k4" %in% views) fr$k4 <- .sn_frames_k4_setup(env, smooth, n_t)
  if ("w" %in% views && !is.null(fr$W))
    fr$w_plot <- .sn_frames_w_heatmap(fr$W, labels = fr$labels, base_size = base_size)
  if ("landscape" %in% views && N <= fr$max_n) fr$grid <- .sn_frames_grid_setup(fr)
  structure(fr, class = "searchnet_frames")
}

#' @export
print.searchnet_frames <- function(x, ...) {
  cat(sprintf("<searchnet_frames> %d actors x %d components, ministeps 0..%d\n",
              x$M, x$N, x$n_steps))
  cat(sprintf("  views prepared: %s; landscape actor %d (%d moves)\n",
              paste(x$views, collapse = ", "), x$actor, x$moves[x$actor]))
  invisible(x)
}

.sn_frames_views <- function(views) {
  ok <- c("network", "k4", "landscape", "w")
  views <- unique(as.character(views))
  bad <- setdiff(views, ok)
  if (length(views) == 0L || length(bad))
    stop(sprintf("views must be among %s", paste(sprintf("\"%s\"", ok), collapse = ", ")),
         call. = FALSE)
  views
}

.sn_frames_step <- function(fr, step) {
  step <- suppressWarnings(as.integer(step))
  if (length(step) != 1L || is.na(step) || step < 0L || step > fr$n_steps)
    stop(sprintf("step must be a single integer in 0..%d", fr$n_steps), call. = FALSE)
  step
}


## ---- network view ----------------------------------------------------------

.sn_frames_net_setup <- function(fr) {
  M <- fr$M; N <- fr$N; xy <- fr$layout
  strat <- tryCatch(as.character(fr$env$get_actor_strategies()), error = function(e) NULL)
  actor_group <- if (is.null(strat) || length(strat) != M || all(strat == "none")) {
    rep("Actors", M)
  } else paste("Actors: strategy", strat)
  module <- NULL
  if (!is.null(fr$W)) {
    A <- (fr$W != 0) | t(fr$W != 0)
    diag(A) <- FALSE
    cm <- igraph::components(igraph::graph_from_adjacency_matrix(A * 1, mode = "undirected"))
    if (cm$no > 1L && cm$no <= 5L && all(cm$csize > 1L)) module <- cm$membership
  }
  comp_group <- if (is.null(module)) rep("Components", N) else paste("Components: module", module)
  a_lv <- unique(actor_group[order(actor_group)])
  c_lv <- unique(comp_group[order(comp_group)])
  oi <- .searchnet_okabe_ito
  a_cols <- c(oi[["orange"]], oi[["sky"]], oi[["yellow"]], oi[["vermillion"]], oi[["black"]])
  c_cols <- c(oi[["blue"]], oi[["green"]], oi[["purple"]], "#7F7F7F", "#4D4D4D")
  cols <- c(stats::setNames(rep_len(a_cols, length(a_lv)), a_lv),
            stats::setNames(rep_len(c_cols, length(c_lv)), c_lv))
  nodes <- data.frame(x = xy[, 1], y = xy[, 2],
                      group = factor(c(actor_group, comp_group), levels = c(a_lv, c_lv)),
                      kind = rep(c("actor", "component"), c(M, N)),
                      lab = c(as.character(seq_len(M)), fr$labels),
                      stringsAsFactors = FALSE)
  pairs <- expand.grid(i = seq_len(M), j = seq_len(N))
  pairs$x <- xy[pairs$i, 1];        pairs$y <- xy[pairs$i, 2]
  pairs$xend <- xy[M + pairs$j, 1]; pairs$yend <- xy[M + pairs$j, 2]
  pad_x <- max(diff(range(xy[, 1])), 1e-6) * 0.06
  pad_y <- max(diff(range(xy[, 2])), 1e-6) * 0.06
  size <- min(6.5, 6.5 * sqrt(20 / (M + N)))
  list(nodes = nodes, pairs = pairs, cols = cols,
       shapes = c(actor = 21, component = 22),
       legend_shapes = ifelse(grepl("^Actors", c(a_lv, c_lv)), 21, 22),
       xlim = range(xy[, 1]) + c(-pad_x, pad_x), ylim = range(xy[, 2]) + c(-pad_y, pad_y),
       size = size, text_size = size * 0.45, show_labels = (M + N) <= 60L)
}

.sn_frames_network <- function(fr, step, legend = TRUE) {
  nt <- fr$net
  S <- fr$state_at(step)
  e <- nt$pairs
  now <- S[cbind(e$i, e$j)] > 0
  start <- fr$B0[cbind(e$i, e$j)] > 0
  e <- e[now, , drop = FALSE]
  e$status <- factor(ifelse(start[now], "held at start", "formed since"),
                     levels = c("held at start", "formed since"))
  e <- e[order(e$status == "formed since"), , drop = FALSE]
  p <- ggplot() +
    geom_segment(data = e, aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend,
                               colour = .data$status), linewidth = 0.6) +
    scale_colour_manual(values = c("held at start" = "grey72", "formed since" = "grey25"),
                        guide = "none", drop = FALSE) +
    geom_point(data = nt$nodes, aes(.data$x, .data$y, fill = .data$group, shape = .data$kind),
               size = nt$size, stroke = 0, colour = "transparent") +
    scale_shape_manual(values = nt$shapes, guide = "none") +
    scale_fill_manual(values = nt$cols, name = NULL, drop = FALSE) +
    labs(title = "The network over the run",
         subtitle = sprintf("ministep %d of %d: %d ties\nlight: held at the start; dark: formed since",
                            step, fr$n_steps, sum(S > 0))) +
    coord_cartesian(xlim = nt$xlim, ylim = nt$ylim, clip = "off") +
    .sn_frames_theme(fr$base_size) +
    theme(panel.grid = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank())
  if (nt$show_labels)
    p <- p + geom_text(data = nt$nodes, aes(.data$x, .data$y, label = .data$lab),
                       colour = "white", size = nt$text_size, fontface = "bold")
  p <- if (legend) {
    p + guides(fill = guide_legend(nrow = min(3L, length(nt$cols)),
                                   override.aes = list(size = 4, shape = nt$legend_shapes)))
  } else p + guides(fill = "none")
  attr(p, "ties") <- S
  p
}


## ---- {K}-4 view ------------------------------------------------------------

.sn_frames_k4_setup <- function(env, smooth, n_t) {
  p <- saomnk_plot_k4(env, smooth = smooth) +
    theme(plot.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom") +
    geom_vline(xintercept = n_t, colour = "grey35", linetype = "22", linewidth = 0.5)
  built <- suppressMessages(suppressWarnings(ggplot_build(p)))
  kinds <- vapply(p$layers, function(l) {
    g <- l$geom
    if (inherits(g, "GeomSmooth") || inherits(g, "GeomLine") || inherits(g, "GeomPath")) "line"
    else if (inherits(g, "GeomVline")) "vline"
    else "point"
  }, character(1))
  kinds[length(kinds)] <- "marker"
  list(built = built, data = built$data, kinds = kinds)
}

## Reveal a line layer's precomputed curve up to x = t, ending exactly at t.
.sn_frames_reveal_line <- function(d, t) {
  if (nrow(d) == 0L || t < min(d$x)) return(d[0, , drop = FALSE])
  if (t >= max(d$x)) return(d)          # whole curve: the static layer as built
  out <- lapply(split(d, list(d$PANEL, d$group), drop = TRUE), function(g) {
    g <- g[order(g$x), , drop = FALSE]
    keep <- g[g$x <= t, , drop = FALSE]
    if (t < max(g$x) && nrow(keep) > 0L) {
      tip <- keep[nrow(keep), , drop = FALSE]
      tip$x <- t
      for (v in intersect(c("y", "ymin", "ymax"), names(g)))
        if (is.numeric(g[[v]]) && all(is.finite(g[[v]])))
          tip[[v]] <- stats::approx(g$x, g[[v]], xout = t, ties = "ordered")$y
      keep <- rbind(keep, tip)
    }
    keep
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

## The {K}-4 layer data revealed up to ministep t (exported to the tests).
.sn_frames_k4_data <- function(fr, t) {
  k <- fr$k4
  d <- k$data
  for (l in seq_along(d)) {
    dl <- d[[l]]
    d[[l]] <- switch(k$kinds[l],
      line   = .sn_frames_reveal_line(dl, t),
      vline  = dl[dl$xintercept <= t, , drop = FALSE],
      marker = if (t >= fr$n_steps) dl[0, , drop = FALSE] else { dl$xintercept <- t; dl },
      point  = if ("x" %in% names(dl) && is.numeric(dl$x)) dl[dl$x <= t, , drop = FALSE] else dl)
  }
  d
}

.sn_frames_k4 <- function(fr, step, compact = FALSE) {
  b <- fr$k4$built
  b$data <- .sn_frames_k4_data(fr, step)
  ## Compact (filmstrip grid): no title, subtitle, caption or legend; the
  ## data, scales and axes are those of the full panel.
  if (compact)
    b$plot <- b$plot + theme(legend.position = "none", plot.title = element_blank(),
                             plot.subtitle = element_blank(), plot.caption = element_blank())
  g <- suppressMessages(suppressWarnings(ggplot_gtable(b)))
  g
}


## ---- landscape view --------------------------------------------------------

## The W-fitted grid: a balanced row/column split of the components with the
## least |W| across it, each axis in reflected Gray-code order so adjacent
## rows (or columns) differ by one component.
.sn_frames_grid_setup <- function(fr) {
  N <- fr$N
  kr <- N %/% 2L
  rows <- seq_len(kr)
  if (!is.null(fr$W) && kr >= 1L && N >= 2L) {
    A <- abs(fr$W) + abs(t(fr$W))
    diag(A) <- 0
    sp <- utils::combn(N, kr)
    cut <- apply(sp, 2L, function(r) sum(A[r, setdiff(seq_len(N), r), drop = FALSE]))
    rows <- sp[, which.min(cut)]
  }
  cols <- setdiff(seq_len(N), rows)
  gray <- function(k) {
    if (k == 0L) return(matrix(0L, 1L, 0L))
    r <- 0:(2L^k - 1L)
    g <- bitwXor(r, bitwShiftR(r, 1L))
    t(vapply(g, function(v) as.integer(bitwAnd(v, 2L^(0:(k - 1L))) > 0), integer(k)))
  }
  gr <- gray(length(rows)); gc <- gray(length(cols))
  cells <- expand.grid(row = seq_len(nrow(gr)), col = seq_len(nrow(gc)))
  bits <- matrix(0L, nrow(cells), N)
  if (length(rows)) bits[, rows] <- gr[cells$row, , drop = FALSE]
  if (length(cols)) bits[, cols] <- gc[cells$col, , drop = FALSE]
  cells$index <- as.vector(bits %*% 2L^(seq_len(N) - 1L)) + 1L
  list(rows = rows, cols = cols, cells = cells)
}

.sn_frames_landscape_data <- function(fr, step) {
  st <- .sn_view_state(fr$env, step)
  i <- fr$actor
  key <- paste(c(as.integer(st$B[-i, ] > 0), format(st$theta, digits = 15)), collapse = ",")
  land <- fr$cache[[key]]
  if (is.null(land)) {
    land <- searchnet_actor_landscape(fr$env, i, step = step, max_n = fr$max_n)
    fr$cache[[key]] <- land
  }
  ## The cache key ignores the focal actor's own row, so set `current` here.
  cur <- 1L + sum(st$B[i, ] * 2L^(seq_len(fr$N) - 1L))
  land$current <- seq_len(nrow(land)) == cur
  land
}

.sn_frames_landscape <- function(fr, step) {
  if (is.null(fr$grid)) return(NULL)
  land <- .sn_frames_landscape_data(fr, step)
  gd <- fr$grid
  cells <- gd$cells
  cells$f <- land$f[cells$index]
  cells$peak <- land$local_peak[cells$index]
  cells$current <- land$current[cells$index]
  lab <- function(ix) if (length(ix)) paste(fr$labels[ix], collapse = " ") else "(none)"
  ggplot(cells, aes(.data$col, .data$row)) +
    geom_tile(aes(fill = .data$f), colour = if (fr$N <= 8L) "grey90" else NA, linewidth = 0.2) +
    scale_fill_gradientn(colours = .searchnet_sequential, name = "f", guide = "none") +
    geom_point(data = cells[cells$peak, , drop = FALSE], shape = 1, colour = "black",
               size = if (fr$N <= 8L) 2.6 else 1.4, stroke = 0.7) +
    geom_tile(data = cells[cells$current, , drop = FALSE], fill = NA,
              colour = .searchnet_okabe_ito[["vermillion"]], linewidth = 1.1) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    coord_equal() +
    labs(title = sprintf("Landscape of actor %d", fr$actor),
         subtitle = sprintf(paste0("all 2^%d portfolios, others fixed; %d local peaks (rings)\n",
                                   "red square: current portfolio; colors scaled per frame"),
                            fr$N, sum(land$local_peak)),
         x = sprintf("columns: %s (Gray order)", lab(gd$cols)),
         y = sprintf("rows: %s (Gray order)", lab(gd$rows))) +
    .sn_frames_theme(fr$base_size) +
    theme(panel.grid = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = fr$base_size - 2))
}


## ---- one frame -------------------------------------------------------------

#' One frame of a stored run
#'
#' Composes the views of a simulated path at one ministep, in the style of
#' the README hero animation. Every view reads the run that [saomnk_run()]
#' stored on the env; nothing is simulated.
#'
#' The views:
#' \describe{
#'   \item{`"network"`}{the bipartite network at `step` on a fixed layout
#'     (by default Fruchterman-Reingold on the union of all ties over the
#'     run, seeded, so nodes never move between frames). Ties held at the
#'     start are light grey; ties formed since are dark.}
#'   \item{`"k4"`}{the K-4 panel of [saomnk_plot_k4()], with points and
#'     loess lines revealed up to `step` on the full-run axes and a dashed
#'     marker at `step`. The lines are the full-run lines, computed once, so
#'     the past of every frame is the same and the last frame is the static
#'     panel.}
#'   \item{`"landscape"`}{the focal actor's objective at all \eqn{2^N}
#'     portfolios ([searchnet_actor_landscape()]) on a grid whose row/column
#'     split of the components is the balanced one with the least \eqn{|W|}
#'     across it, each axis in Gray-code order; local peaks ringed, the
#'     current portfolio outlined. Drawn only when \eqn{N} is at most
#'     `max_n` (12); otherwise skipped with a message.}
#'   \item{`"w"`}{the influence matrix \eqn{W} (static), when the model has
#'     a single static one.}
#' }
#'
#' @param env A `SaomNkRSienaBiEnv` after [saomnk_run()], or the object
#'   returned by [searchnet_frames_prepare()] (faster for many frames).
#' @param step Integer ministep, `0` (the state before the first ministep)
#'   to the number of ministeps of the run.
#' @param views Character vector of views, in display order: any of
#'   `"network"`, `"k4"`, `"landscape"` and `"w"`.
#' @param actor,layout Focal actor of the landscape view and network layout;
#'   see [searchnet_frames_prepare()]. Used only when `env` is an env.
#' @param arrange `"row"` (default) places the views side by side, `"column"`
#'   stacks them.
#' @param legend Logical; `FALSE` gives the compact form the filmstrip grid
#'   uses: no network legend, and the K-4 panel without its title,
#'   subtitle, caption and legend.
#' @param ... Further arguments to [searchnet_frames_prepare()] when `env` is
#'   an env (for example `smooth` or `seed`).
#'
#' @return A ggplot object (a `cowplot` composition when there are several
#'   views), with attributes `step` and `ties` (the \eqn{M \times N}{M x N}
#'   tie matrix at `step`). Print it to draw it.
#'
#' @seealso [searchnet_filmstrip()], [searchnet_animate()],
#'   [searchnet_frames_prepare()]
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 5, N = 6, density = 0.2, seed = 42)
#' mod <- saomnk_model(density = -1.5,
#'                     influence_matrix = saomnk_block_diagonal(6, 2),
#'                     influence_weight = 0.5)
#' invisible(capture.output(saomnk_run(env, mod, steps_per_actor = 6, seed = 1)))
#' p <- searchnet_frame(env, step = 10)
#' f <- file.path(tempdir(), "frame10.png")
#' ggplot2::ggsave(f, p, width = 10, height = 6, dpi = 72)
#' searchnet_frame(env, step = 10, views = c("landscape", "w"))
#' unlink(f)
#' }
#' @export
searchnet_frame <- function(env, step, views = c("network", "k4", "landscape"),
                            actor = NULL, layout = NULL, arrange = c("row", "column"),
                            legend = TRUE, ...) {
  .searchnet_need("cowplot", "searchnet_frame()")
  arrange <- match.arg(arrange)
  views <- .sn_frames_views(views)
  fr <- .sn_frames_ensure(env, views, actor, layout, ...)
  step <- .sn_frames_step(fr, step)
  panels <- list(); widths <- numeric(0)
  for (v in views) {
    p <- switch(v,
      network = .sn_frames_network(fr, step, legend = legend),
      k4 = .sn_frames_k4(fr, step, compact = !legend),
      landscape = {
        if (fr$N > fr$max_n) {
          message(sprintf("searchnet_frame(): landscape view skipped, N = %d > %d (2^N portfolios)",
                          fr$N, fr$max_n))
          NULL
        } else .sn_frames_landscape(fr, step)
      },
      w = {
        if (is.null(fr$w_plot)) message("searchnet_frame(): \"w\" view skipped, the model has no single static W")
        fr$w_plot
      })
    if (!is.null(p)) {
      panels[[v]] <- p
      widths <- c(widths, c(network = 1, k4 = 1.7, landscape = 1, w = 0.8)[[v]])
    }
  }
  if (length(panels) == 0L) stop("no view could be drawn", call. = FALSE)
  out <- if (length(panels) == 1L && inherits(panels[[1]], "ggplot")) {
    panels[[1]]
  } else if (arrange == "row") {
    cowplot::plot_grid(plotlist = panels, nrow = 1, rel_widths = widths)
  } else {
    cowplot::plot_grid(plotlist = panels, ncol = 1,
                       rel_heights = c(network = 1, k4 = 1.3, landscape = 1, w = 0.8)[names(panels)])
  }
  ## Without the network view nothing names the ministep; add a heading.
  if (!"network" %in% names(panels) && legend) {
    head <- cowplot::ggdraw() +
      cowplot::draw_label(sprintf("ministep %d of %d", step, fr$n_steps),
                          fontface = "bold", size = fr$base_size + 1, x = 0.01, hjust = 0)
    out <- cowplot::plot_grid(head, out, ncol = 1, rel_heights = c(0.06, 1))
  }
  attr(out, "step") <- step
  attr(out, "ties") <- fr$state_at(step)
  out
}

.sn_frames_ensure <- function(env, views, actor, layout, ...) {
  if (inherits(env, "searchnet_frames")) {
    fr <- env
    missing_k4 <- "k4" %in% views && is.null(fr$k4)
    if (missing_k4) fr$k4 <- .sn_frames_k4_setup(fr$env, fr$smooth, fr$n_steps)
    if ("landscape" %in% views && is.null(fr$grid) && fr$N <= fr$max_n)
      fr$grid <- .sn_frames_grid_setup(fr)
    if ("w" %in% views && is.null(fr$w_plot) && !is.null(fr$W))
      fr$w_plot <- .sn_frames_w_heatmap(fr$W, labels = fr$labels, base_size = fr$base_size)
    return(fr)
  }
  searchnet_frames_prepare(env, views = views, actor = actor, layout = layout, ...)
}


## ---- output helpers --------------------------------------------------------

.sn_frames_open_png <- function(file, width, height, res, units = "px") {
  if (capabilities("cairo")) {
    grDevices::png(file, width = width, height = height, units = units, res = res,
                   bg = "white", type = "cairo")
  } else {
    grDevices::png(file, width = width, height = height, units = units, res = res, bg = "white")
  }
}

.sn_frames_draw <- function(p) {
  if (inherits(p, "gtable")) {
    grid::grid.newpage()
    grid::grid.draw(p)
  } else print(p)
}

## One row with the network and {K}-4 legends, shared by a filmstrip grid.
.sn_frames_legend_row <- function(fr, views) {
  pick <- function(g) {
    k <- which(grepl("^guide-box", g$layout$name) &
                 !vapply(g$grobs, inherits, logical(1), "zeroGrob"))
    if (length(k)) g$grobs[[k[1]]] else NULL
  }
  legs <- list()
  if ("network" %in% views)
    legs$network <- pick(ggplotGrob(.sn_frames_network(fr, fr$n_steps, legend = TRUE)))
  if ("k4" %in% views) legs$k4 <- pick(.sn_frames_k4(fr, fr$n_steps))
  legs <- Filter(Negate(is.null), legs)
  if (!length(legs)) return(NULL)
  cowplot::plot_grid(plotlist = legs, nrow = 1)
}

.sn_frames_default_steps <- function(fr, n = 120L) {
  as.integer(unique(round(seq(0, fr$n_steps, length.out = min(fr$n_steps + 1L, n)))))
}


## ---- filmstrip -------------------------------------------------------------

#' A filmstrip of a stored run
#'
#' Frames of [searchnet_frame()] at several ministeps, either on one page
#' (`layout = "grid"`) or one per page (`layout = "pages"`).
#'
#' @inheritParams searchnet_frame
#' @param steps Integer vector of ministeps (`0` to the number of ministeps).
#'   `NULL` (default) takes four evenly spaced ministeps from the start to
#'   the end of the run.
#' @param file Output file, or `NULL` (default) to only return the plots.
#'   With `layout = "grid"` the page is written as PDF when `file` ends in
#'   `.pdf` and as PNG otherwise. With `layout = "pages"`, a `.pdf` file
#'   gets one page per step, and any other name is the stem of numbered
#'   PNG files (`<stem>_001.png`, `<stem>_002.png`, ...).
#' @param views Views of each frame (default `c("network", "k4")`).
#' @param layout `"grid"` (one page, frames in a grid labeled by ministep)
#'   or `"pages"` (one frame per page or file).
#' @param ncol Number of columns of the grid. `NULL` puts up to four frames
#'   in one row and uses a near-square grid beyond that.
#' @param width,height Page size in inches. `NULL` picks a size from the
#'   number of frames and views.
#' @param res Resolution of PNG output, in pixels per inch.
#' @param ... Further arguments to [searchnet_frames_prepare()].
#'
#' @return Invisibly: for `"grid"`, one ggplot (a `cowplot` composition)
#'   with attributes `steps` and `frames` (the list of per-step frames); for
#'   `"pages"`, the list of per-step frames, with attribute `files` when
#'   files were written.
#'
#' @seealso [searchnet_frame()], [searchnet_animate()]
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 5, N = 6, density = 0.2, seed = 42)
#' mod <- saomnk_model(density = -1.5,
#'                     influence_matrix = saomnk_block_diagonal(6, 2),
#'                     influence_weight = 0.5)
#' invisible(capture.output(saomnk_run(env, mod, steps_per_actor = 6, seed = 1)))
#' f <- file.path(tempdir(), "filmstrip.png")
#' searchnet_filmstrip(env, file = f, res = 60)   # four evenly spaced ministeps
#' file.exists(f)
#' unlink(f)
#' }
#' @export
searchnet_filmstrip <- function(env, steps = NULL, file = NULL, views = c("network", "k4"),
                                layout = c("grid", "pages"), ncol = NULL,
                                width = NULL, height = NULL, res = 100, ...) {
  .searchnet_need("cowplot", "searchnet_filmstrip()")
  layout <- match.arg(layout)
  views <- .sn_frames_views(views)
  fr <- .sn_frames_ensure(env, views, NULL, NULL, ...)
  if (is.null(steps)) steps <- unique(as.integer(round(seq(0, fr$n_steps, length.out = 4L))))
  steps <- vapply(steps, function(s) .sn_frames_step(fr, s), integer(1))
  n <- length(steps)
  nv <- length(views)

  if (layout == "grid") {
    frames <- lapply(steps, function(s)
      searchnet_frame(fr, s, views = views, arrange = "column", legend = FALSE))
    if (is.null(ncol)) ncol <- if (n <= 4L) n else ceiling(sqrt(n))
    nrow <- ceiling(n / ncol)
    labelled <- lapply(seq_len(n), function(k) {
      head <- cowplot::ggdraw() +
        cowplot::draw_label(sprintf("ministep %d", steps[k]), fontface = "bold",
                            size = fr$base_size + 3)
      cowplot::plot_grid(head, frames[[k]], ncol = 1, rel_heights = c(0.06, 1))
    })
    page <- cowplot::plot_grid(plotlist = labelled, ncol = ncol)
    leg <- .sn_frames_legend_row(fr, views)
    if (!is.null(leg)) page <- cowplot::plot_grid(page, leg, ncol = 1,
                                                  rel_heights = c(1, 0.9 / (4.2 * nv * nrow)))
    if (is.null(width)) width <- 5 * ncol
    if (is.null(height)) height <- (1 + 4.2 * nv) * nrow + 0.9
    attr(page, "steps") <- steps
    attr(page, "frames") <- frames
    if (!is.null(file)) {
      if (grepl("\\.pdf$", file, ignore.case = TRUE)) {
        grDevices::pdf(file, width = width, height = height, onefile = TRUE)
      } else {
        .sn_frames_open_png(file, width, height, res, units = "in")
      }
      tryCatch(.sn_frames_draw(page), finally = grDevices::dev.off())
    }
    return(invisible(page))
  }

  frames <- lapply(steps, function(s) searchnet_frame(fr, s, views = views))
  if (is.null(width)) width <- c(4.5, 8, 10.5, 13)[min(nv, 4L)]
  if (is.null(height)) height <- 6
  if (!is.null(file)) {
    if (grepl("\\.pdf$", file, ignore.case = TRUE)) {
      grDevices::pdf(file, width = width, height = height, onefile = TRUE)
      tryCatch(for (p in frames) .sn_frames_draw(p), finally = grDevices::dev.off())
      files <- file
    } else {
      stem <- sub("\\.png$", "", file, ignore.case = TRUE)
      files <- sprintf("%s_%03d.png", stem, seq_len(n))
      for (k in seq_len(n)) {
        .sn_frames_open_png(files[k], width, height, res, units = "in")
        tryCatch(.sn_frames_draw(frames[[k]]), finally = grDevices::dev.off())
      }
    }
    attr(frames, "files") <- files
  }
  invisible(frames)
}


## ---- animation -------------------------------------------------------------

## The GIF renderers available here, in order of preference.
.sn_frames_renderers <- function() {
  c(gifski = requireNamespace("gifski", quietly = TRUE),
    ffmpeg = nzchar(Sys.which("ffmpeg")),
    magick = requireNamespace("magick", quietly = TRUE))
}

#' Animate a stored run as a GIF
#'
#' Renders frames of [searchnet_frame()] to PNG files in a temporary
#' directory and assembles them into a looping GIF. The frames read the run
#' stored on the env, the layout is seeded and the K-4 panel is built
#' once, so the GIF is deterministic for a given run.
#'
#' Three renderers are supported: the \pkg{gifski} package (preferred), the
#' `ffmpeg` program on the `PATH` (a two-pass palette), or the \pkg{magick}
#' package. `renderer = "auto"` takes the first one available. The polished
#' README animations are made with separate scripts in the package sources'
#' `tools/` directory, some of which render with optional manim scenes; this
#' function needs none of that.
#'
#' @inheritParams searchnet_frame
#' @param steps Integer vector of ministeps, in display order. `NULL`
#'   (default) subsamples the run to about 120 frames, always including the
#'   start (`0`) and the last ministep.
#' @param file Output `.gif` file. Required.
#' @param views Views of each frame (default `c("network", "k4")`).
#' @param fps Frames per second.
#' @param width,height Frame size in pixels.
#' @param res Resolution of the frames, in pixels per inch (sets the text
#'   size relative to the frame).
#' @param hold_last Seconds the last frame is held before the loop restarts.
#' @param renderer `"auto"`, `"gifski"`, `"ffmpeg"` or `"magick"`.
#' @param ... Further arguments to [searchnet_frames_prepare()].
#'
#' @return Invisibly, `file`, with attributes `steps` and `renderer`.
#'
#' @seealso [searchnet_frame()], [searchnet_filmstrip()]
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 5, N = 6, density = 0.2, seed = 42)
#' mod <- saomnk_model(density = -1.5,
#'                     influence_matrix = saomnk_block_diagonal(6, 2),
#'                     influence_weight = 0.5)
#' invisible(capture.output(saomnk_run(env, mod, steps_per_actor = 6, seed = 1)))
#' if (requireNamespace("gifski", quietly = TRUE)) {
#'   f <- file.path(tempdir(), "run.gif")
#'   searchnet_animate(env, steps = round(seq(0, env$get_step(), length.out = 3)), file = f,
#'                     width = 400, height = 300, res = 40, renderer = "gifski")
#'   unlink(f)
#' }
#' }
#' @export
searchnet_animate <- function(env, steps = NULL, file, views = c("network", "k4"),
                              fps = 10, width = 1200, height = 1000, res = 100,
                              hold_last = 2, renderer = c("auto", "gifski", "ffmpeg", "magick"),
                              ...) {
  .searchnet_need("cowplot", "searchnet_animate()")
  renderer <- match.arg(renderer)
  if (missing(file) || !is.character(file) || length(file) != 1L || !nzchar(file))
    stop("file must be the path of the .gif to write", call. = FALSE)
  avail <- .sn_frames_renderers()
  if (renderer == "auto") {
    if (!any(avail))
      stop(paste0("searchnet_animate() needs a GIF renderer, and none was found. Install one of:\n",
                  "  the gifski package:  install.packages(\"gifski\")\n",
                  "  the magick package:  install.packages(\"magick\")\n",
                  "  the ffmpeg program, on the PATH (https://ffmpeg.org/download.html)"),
           call. = FALSE)
    renderer <- names(avail)[avail][1]
  } else if (!avail[[renderer]]) {
    stop(sprintf("renderer \"%s\" is not available here (%s)", renderer,
                 if (renderer == "ffmpeg") "ffmpeg is not on the PATH"
                 else sprintf("install.packages(\"%s\")", renderer)), call. = FALSE)
  }
  stopifnot(is.numeric(fps), length(fps) == 1L, fps > 0,
            is.numeric(hold_last), length(hold_last) == 1L, hold_last >= 0)

  views <- .sn_frames_views(views)
  fr <- .sn_frames_ensure(env, views, NULL, NULL, ...)
  if (is.null(steps)) steps <- .sn_frames_default_steps(fr)
  steps <- vapply(steps, function(s) .sn_frames_step(fr, s), integer(1))

  work <- tempfile("searchnet_frames_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  pngs <- file.path(work, sprintf("frame_%05d.png", seq_along(steps)))
  for (k in seq_along(steps)) {
    .sn_frames_open_png(pngs[k], width, height, res)
    tryCatch(.sn_frames_draw(searchnet_frame(fr, steps[k], views = views)),
             finally = grDevices::dev.off())
  }
  n_hold <- as.integer(round(hold_last * fps))

  if (renderer == "gifski") {
    seq_files <- c(pngs, rep(pngs[length(pngs)], n_hold))
    gifski::gifski(seq_files, gif_file = file, width = width, height = height,
                   delay = 1 / fps, loop = TRUE, progress = FALSE)
  } else if (renderer == "ffmpeg") {
    ff <- Sys.which("ffmpeg")
    pal <- file.path(work, "palette.png")
    pattern <- file.path(work, "frame_%05d.png")
    vf <- sprintf("tpad=stop_mode=clone:stop_duration=%g", hold_last)
    st1 <- system2(ff, c("-v", "error", "-y", "-framerate", fps, "-i", shQuote(pattern),
                         "-vf", shQuote(paste0(vf, ",palettegen=max_colors=256:stats_mode=full")),
                         shQuote(pal)))
    st2 <- system2(ff, c("-v", "error", "-y", "-framerate", fps, "-i", shQuote(pattern),
                         "-i", shQuote(pal), "-lavfi",
                         shQuote(paste0(vf, "[x];[x][1:v]paletteuse=dither=none:diff_mode=rectangle")),
                         "-loop", "0", shQuote(file)))
    if (!identical(as.integer(st1), 0L) || !identical(as.integer(st2), 0L))
      stop("ffmpeg failed to write the GIF", call. = FALSE)
  } else {
    img <- magick::image_read(c(pngs, rep(pngs[length(pngs)], n_hold)))
    anim <- magick::image_animate(img, delay = max(1L, as.integer(round(100 / fps))),
                                  loop = 0, optimize = TRUE)
    magick::image_write(anim, path = file, format = "gif")
  }
  if (!file.exists(file)) stop("the GIF was not written", call. = FALSE)
  out <- file
  attr(out, "steps") <- steps
  attr(out, "renderer") <- renderer
  invisible(out)
}
