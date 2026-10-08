###############################################################################
## rosetta-plot.R
##
## rosetta_plot(): the objective by class of effects (row I), a foundational
## model rewritten term by term in the same classes (row II), and the
## bipartite state and influence matrix behind them (row III).
###############################################################################

.rosetta_pal <- list(ink = "#1F1F1F", muted = "#5A5A5A", panel = "#F4F4F2",
                     actor = "#22313F", component = "#B5651D", tie = "#5E7E9B",
                     wlo = "#4A6FA5", whi = "#C8553D", bin = "#3A3A3A")

## geom_label() renamed its border width from label.size to linewidth in
## ggplot2 4.0; pass whichever the installed version takes.
.rosetta_geom_label <- function(..., border = 0.25) {
  new <- utils::packageVersion("ggplot2") >= "4.0.0"
  args <- c(list(...), if (new) list(linewidth = border) else list(label.size = border))
  do.call(ggplot2::geom_label, args)
}

.rosetta_status_word <- c(active = "active", fixed = "held fixed", zero = "set to zero",
                          absent = "absent", linearized = "linearized")

.rosetta_wrap <- function(x, width) vapply(x, function(s)
  paste(strwrap(s, width), collapse = "\n"), "", USE.NAMES = FALSE)

## The {K} dimensions in drawing order: the four shipped dimensions, then dimensions
## opened by registered classes (in order of first appearance), then "other".
.ROSETTA_K_ORDER <- c("K_CC", "K_AC", "K_AA", "K_CA")

## Dimension levels in drawing order for the dimensions present in `ch`.
.rosetta_k_levels <- function(ch) {
  c(intersect(.ROSETTA_K_ORDER, ch),
    setdiff(unique(ch), c(.ROSETTA_K_ORDER, "other")),
    intersect("other", ch))
}

## Reorder class rows so classes sharing a dimension are adjacent; stable
## within a dimension. `by` names the dimension column (k_channel, the alias
## of moves, by default); `then`, when given, orders the classes within a
## dimension by a second column, so that the second view's badges also group.
.rosetta_order_by_k <- function(terms, by = "k_channel", then = NULL) {
  if (!nrow(terms)) return(terms)
  ch <- terms[[by]]
  key2 <- if (!is.null(then) && !is.null(terms[[then]]))
    match(terms[[then]], .rosetta_k_levels(terms[[then]])) else rep(0L, length(ch))
  out <- terms[order(match(ch, .rosetta_k_levels(ch)), key2, seq_along(ch)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

## Plotmath text of a dimension badge: "Sociality K_AA" with K_AA as italic K
## subscripted AA; "other" stays a word.
.rosetta_k_badge_text <- function(ch, labels = character(0)) {
  vapply(ch, function(k) {
    nm <- if (k %in% names(labels)) gsub("\"", "", labels[[k]]) else ""
    if (identical(k, "other")) return(sprintf("\"%s\"", if (nzchar(nm)) nm else "other"))
    sub <- sub("^K_", "", k)
    sym <- if (grepl("^[A-Za-z][A-Za-z0-9]*$", sub)) sprintf("italic(K)[italic(%s)]", sub)
           else sprintf("italic(K)[\"%s\"]", sub)
    if (nzchar(nm)) sprintf("bold(\"%s\")~%s", nm, sym) else sym
  }, "", USE.NAMES = FALSE)
}

## One row per {K} dimension among the drawn classes: its columns (terms must
## already be ordered by .rosetta_order_by_k()), the badge's center, and the
## classes it groups. Exposed on the figure as attr(, "k_groups").
## `by` names the dimension column the badges follow; the result's column is
## called k_channel whichever view it shows.
.rosetta_k_groups <- function(terms, labels = .rosetta_k_labels(), by = "k_channel") {
  if (!nrow(terms)) return(data.frame(k_channel = character(0), name = character(0),
                                      badge = character(0), xmin = numeric(0), xmax = numeric(0),
                                      x = numeric(0), n = integer(0), classes = character(0)))
  r <- rle(terms[[by]])
  xmax <- cumsum(r$lengths); xmin <- xmax - r$lengths + 1L
  data.frame(k_channel = r$values,
             name = vapply(r$values, function(k) if (k %in% names(labels)) labels[[k]] else "", "",
                           USE.NAMES = FALSE),
             badge = .rosetta_k_badge_text(r$values, labels),
             xmin = xmin, xmax = xmax, x = (xmin + xmax) / 2, n = r$lengths,
             classes = vapply(seq_along(xmin), function(i)
               paste(terms$class[xmin[i]:xmax[i]], collapse = ", "), ""),
             stringsAsFactors = FALSE)
}

## Layers drawing one black badge per dimension at height y, centered on its
## group, on a bracket (a line with end ticks) when the group spans >= 2 columns.
.rosetta_k_layers <- function(groups, y, half = 0.36, tick = 0.22) {
  if (!nrow(groups)) return(list())
  groups$y <- y
  out <- list()
  br <- groups[groups$n >= 2, , drop = FALSE]
  if (nrow(br)) {
    seg <- data.frame(x0 = br$xmin - half, x1 = br$xmax + half, y = y, yt = y + tick)
    out <- c(out, list(
      ggplot2::geom_segment(data = seg, ggplot2::aes(x = x0, xend = x1, y = y, yend = y),
                            colour = .rosetta_pal$ink, linewidth = 0.55),
      ggplot2::geom_segment(data = seg, ggplot2::aes(x = x0, xend = x0, y = y, yend = yt),
                            colour = .rosetta_pal$ink, linewidth = 0.55),
      ggplot2::geom_segment(data = seg, ggplot2::aes(x = x1, xend = x1, y = y, yend = yt),
                            colour = .rosetta_pal$ink, linewidth = 0.55)))
  }
  c(out, list(.rosetta_geom_label(data = groups, ggplot2::aes(x = x, y = y, label = badge),
                                  parse = TRUE, fill = .rosetta_pal$ink, colour = "white",
                                  size = 3.4, border = 0, label.r = grid::unit(0.18, "lines"))))
}

## Row I: the objective by class. With `groups_reads` (view = "both"), two
## badge rows: what the classes read above, what they move below.
.rosetta_row_objective <- function(terms, classes, title, groups = .rosetta_k_groups(terms),
                                   groups_reads = NULL) {
  n <- nrow(terms)
  col <- classes$color[match(terms$class, classes$id)]
  on <- terms$status %in% .ROSETTA_ON
  eq <- ifelse(terms$class == "complementarity",
               "sum(F(bold(b)[i]*';'~W^(k)), k %in% c)",
               "'+'~sum(theta[k]*s[ik], k %in% c)")
  eq[!on] <- "'+'~0"
  eq[1] <- sub("^'\\+'~", "", eq[1])
  x <- seq_len(n)
  d <- data.frame(x = x, eq = eq, chip = terms$label, col = col, on = on,
                  eff = paste0("effects: ", ifelse(nzchar(terms$effects), terms$effects, "none")),
                  cons = .rosetta_wrap(terms$construct, 22), stringsAsFactors = FALSE)
  p <- ggplot2::ggplot(d) +
    ggplot2::annotate("text", x = 0.35, y = 4, label = "u[i]~'='", parse = TRUE, size = 6,
                      colour = .rosetta_pal$ink) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = 4, label = eq), parse = TRUE, size = 4.6,
                       colour = ifelse(d$on, d$col, "#9A9A9A")) +
    .rosetta_geom_label(data = d[d$on, , drop = FALSE],
                        ggplot2::aes(x = x, y = 3.15, label = chip), fill = d$col[d$on],
                        colour = "white", fontface = "bold", size = 3.4, border = 0,
                        label.r = grid::unit(0.18, "lines")) +
    .rosetta_geom_label(data = d[!d$on, , drop = FALSE],
                        ggplot2::aes(x = x, y = 3.15, label = chip), fill = "white",
                        colour = d$col[!d$on], fontface = "bold", size = 3.4, border = 0.5,
                        label.r = grid::unit(0.18, "lines")) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = 2.6, label = eff), size = 2.9,
                       colour = d$col, lineheight = 0.9) +
    .rosetta_geom_label(ggplot2::aes(x = x, y = 1.85, label = cons), fill = "white",
                        colour = .rosetta_pal$ink, size = 2.7, border = 0.35,
                        lineheight = 0.9, label.r = grid::unit(0.1, "lines")) +
    (if (is.null(groups_reads)) .rosetta_k_layers(groups, y = 1.0) else
      c(.rosetta_k_layers(groups_reads, y = 1.0), .rosetta_k_layers(groups, y = 0.38),
        list(ggplot2::annotate("text", x = 0.3, y = c(1.0, 0.38),
                               label = c("reads", "moves"),
                               size = 2.6, fontface = "italic", lineheight = 0.85,
                               colour = .rosetta_pal$muted)))) +
    ggplot2::scale_x_continuous(limits = c(-0.1, n + 0.6), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(if (is.null(groups_reads)) 0.6 else 0.02, 4.5),
                                expand = c(0, 0)) +
    ggplot2::labs(title = title) + .rosetta_theme()
  p
}

## Width in inches of each label as drawn by geom_text() at ggplot size
## `size` (the widest line of a multi-line label). Measured on a null PDF
## device so the caller's current device is left as it was.
.rosetta_text_width_in <- function(labels, size, parse = FALSE) {
  old <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  on.exit({ grDevices::dev.off(); if (old > 1L) grDevices::dev.set(old) }, add = TRUE)
  gp <- grid::gpar(fontsize = size * ggplot2::.pt, lineheight = 0.9)
  vapply(labels, function(l) {
    lab <- if (parse) parse(text = l) else l
    grid::convertWidth(grid::grobWidth(grid::textGrob(lab, gp = gp)), "inches", valueOnly = TRUE)
  }, numeric(1), USE.NAMES = FALSE)
}

## Layout of row II's top line for a figure `width_in` inches wide. The
## relation label ("u_i =", "u_i ~", ...) is left-aligned at rel_x and its
## measured width is reserved; each column's expression is centered on its
## column and must fit its half-width (column 1: the space right of the label).
## Rule: wrap at the widest strwrap() width (26 down to 8 characters) that
## fits; if no width fits, shrink the text in proportion, down to size 1.8.
## The label itself shrinks only when it would leave column 1 less than 0.25
## of a column on its left.
.rosetta_row2_layout <- function(expr, relation, n, width_in = 12, size = 3,
                                 rel_size = 5, rel_x = 0.02, gap = 0.04) {
  rel <- switch(.rs_or(relation, ""), "special-case-linearized" = "u[i] %~~% phantom(0)",
                "equilibrium-characterization" = "pi %prop% exp(beta*Phi)", "u[i]~'='")
  panel_in <- max(width_in - 12 / 72, 1)          # plot.margin: 6 pt each side
  upi <- (n + 0.7) / panel_in                       # x data units per inch
  rel_w <- .rosetta_text_width_in(rel, rel_size, parse = TRUE) * upi
  room <- 1 - 0.25 - gap - rel_x
  if (rel_w > room) {
    rel_size <- rel_size * room / rel_w
    rel_w <- room
  }
  half <- rep(0.47, length(expr))
  half[1] <- min(0.47, 1 - (rel_x + rel_w + gap))
  out <- character(length(expr))
  sz <- rep(size, length(expr))
  for (i in seq_along(expr)) {
    allowed <- 2 * half[i] / upi
    best <- NULL
    for (w in 26:8) {
      s <- .rosetta_wrap(expr[i], w)
      if (.rosetta_text_width_in(s, size) <= allowed) { best <- s; break }
    }
    if (is.null(best)) {
      best <- .rosetta_wrap(expr[i], 8)
      sz[i] <- max(1.8, size * allowed / .rosetta_text_width_in(best, size))
    }
    out[i] <- best
  }
  list(rel = rel, rel_x = rel_x, rel_size = rel_size, rel_right = rel_x + rel_w,
       expr = out, size = sz, half = half, upi = upi)
}

## Row II: a special case term by term.
## Its columns follow row I's {K} grouping; the badges are drawn here only
## when the row stands alone (show_k = TRUE). `width_in` is the width of the
## figure the row is drawn into; the top line is laid out for it.
.rosetta_row_special <- function(terms, classes, entry, groups = .rosetta_k_groups(terms),
                                 show_k = FALSE, width_in = 12) {
  n <- nrow(terms)
  col <- classes$color[match(terms$class, classes$id)]
  on <- terms$status %in% .ROSETTA_ON
  expr <- ifelse(on, paste("+", terms$expression), "+ 0")
  expr[1] <- sub("^\\+ ", "", expr[1])
  lay <- .rosetta_row2_layout(expr, entry$relation, n, width_in)
  d <- data.frame(x = seq_len(n), expr = lay$expr, esize = lay$size, chip = terms$label,
                  col = col, on = on, st = .rosetta_status_word[terms$status],
                  cons = .rosetta_wrap(ifelse(on, terms$construct, ""), 24),
                  stringsAsFactors = FALSE)
  ggplot2::ggplot(d) +
    ggplot2::annotate("rect", xmin = -0.05, xmax = n + 0.55, ymin = 0.65, ymax = 4.45,
                      fill = .rosetta_pal$panel, colour = "#D6D6D2") +
    ggplot2::annotate("text", x = lay$rel_x, y = 3.7, label = lay$rel, parse = TRUE,
                      size = lay$rel_size, hjust = 0, colour = .rosetta_pal$ink) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = 3.7, label = expr, size = esize),
                       colour = ifelse(d$on, d$col, "#8C8C8C"), lineheight = 0.9) +
    ggplot2::scale_size_identity() +
    .rosetta_geom_label(data = d[d$on, , drop = FALSE],
                        ggplot2::aes(x = x, y = 2.75, label = chip), fill = d$col[d$on],
                        colour = "white", fontface = "bold", size = 3.4, border = 0,
                        label.r = grid::unit(0.18, "lines")) +
    .rosetta_geom_label(data = d[!d$on, , drop = FALSE],
                        ggplot2::aes(x = x, y = 2.75, label = chip), fill = "white",
                        colour = d$col[!d$on], fontface = "bold", size = 3.4, border = 0.5,
                        label.r = grid::unit(0.18, "lines")) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = 2.2, label = st), size = 2.8, fontface = "italic",
                       colour = .rosetta_pal$muted) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = 1.45, label = cons), size = 2.8,
                       colour = d$col, lineheight = 0.9) +
    (if (show_k) .rosetta_k_layers(groups, y = 0.92, tick = 0.18) else list()) +
    ggplot2::scale_x_continuous(limits = c(-0.1, n + 0.6), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.6, 4.5), expand = c(0, 0)) +
    ggplot2::labs(title = sprintf("II. %s, term by term (%s)", entry$title, entry$relation),
                  subtitle = .rosetta_wrap(paste0(entry$model, ". ", .rosetta_cite_short(entry)), 140)) +
    .rosetta_theme()
}

.rosetta_cite_short <- function(e) {
  k <- vapply(e$citations, function(c) .rs_or(c$key, ""), "")
  if (!length(k)) return("")
  lab <- vapply(k, function(s) {
    p <- strsplit(s, "_")[[1]]
    yr <- p[length(p)]
    au <- p[-length(p)]
    au <- paste0(toupper(substring(au, 1, 1)), substring(au, 2))
    paste(if (length(au) > 2) paste(au[1], "et al.") else paste(au, collapse = " and "), yr)
  }, "")
  paste0("Source: ", paste(lab, collapse = "; "))
}

## Row III, left: the bipartite state of a tiny seeded run.
.rosetta_glyph <- function(B, W = NULL, classes, show_w = TRUE, title = "") {
  M <- nrow(B); N <- ncol(B)
  ax <- if (M == 1) (N + 1) / 2 else seq(1, N, length.out = M)
  act <- data.frame(x = ax, y = 0, lab = letters[9 + seq_len(M) - 1][seq_len(M)])
  act$lab[is.na(act$lab)] <- ""
  com <- data.frame(x = seq_len(N), y = 1)
  ed <- which(B != 0, arr.ind = TRUE)
  ties <- data.frame(x = ax[ed[, 1]], y = 0, xend = ed[, 2], yend = 1)
  p <- ggplot2::ggplot()
  if (show_w && !is.null(W)) {
    ww <- which(upper.tri(W) & (W != 0 | t(W) != 0), arr.ind = TRUE)
    if (nrow(ww)) {
      wl <- data.frame(x = ww[, 1], xend = ww[, 2], y = 1, yend = 1)
      p <- p + ggplot2::geom_curve(data = wl, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                                   curvature = -0.35, colour = classes$color[classes$id == "complementarity"],
                                   linewidth = 0.5, alpha = 0.6)
    }
  }
  p + ggplot2::geom_segment(data = ties, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            colour = .rosetta_pal$tie, linewidth = 0.8) +
    ggplot2::geom_point(data = com, ggplot2::aes(x = x, y = y), shape = 22, size = 6,
                        fill = .rosetta_pal$component, colour = "white") +
    ggplot2::geom_point(data = act, ggplot2::aes(x = x, y = y), shape = 21, size = 9,
                        fill = .rosetta_pal$actor, colour = "white") +
    ggplot2::geom_text(data = act, ggplot2::aes(x = x, y = y, label = lab), colour = "white",
                       fontface = "bold", size = 3.4) +
    ggplot2::scale_x_continuous(limits = c(0.4, N + 0.6)) +
    ggplot2::scale_y_continuous(limits = c(-0.35, 1.75)) +
    ggplot2::labs(title = title, subtitle = "circles: actors; squares: components") +
    .rosetta_theme()
}

## Row III, middle and right: W and its binary pattern with K per row.
.rosetta_w_panels <- function(W) {
  N <- nrow(W)
  d <- expand.grid(r = seq_len(N), c = seq_len(N))
  d$w <- W[cbind(d$r, d$c)]
  lim <- max(abs(W), 1e-9)
  p1 <- ggplot2::ggplot(d, ggplot2::aes(x = c, y = r, fill = w)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient2(low = .rosetta_pal$wlo, mid = "#F7F7F7", high = .rosetta_pal$whi,
                                  midpoint = 0, limits = c(-lim, lim), name = "W") +
    ggplot2::scale_y_reverse(breaks = seq_len(N)) + ggplot2::coord_equal() +
    ggplot2::labs(title = "influence matrix W", subtitle = "sign and size of each interaction",
                  x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"))
  ## Exactly 1[W_jh != 0] off the diagonal. The diagonal is left blank: the
  ## engine's XWX statistic sums over j != h (R/saomnk-base.R), so W_jj never
  ## enters the objective, and K counts a row's other components.
  E <- (W != 0) * 1
  diag(E) <- 0
  d$b <- E[cbind(d$r, d$c)]
  k <- rowSums(E)
  bars <- data.frame(r = seq_len(N), xmin = N + 1, xmax = N + 1 + 0.8 * N / 2 * k / max(1, max(k)), k = k)
  p2 <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = d, ggplot2::aes(x = c, y = r, fill = factor(b)), colour = "white",
                       linewidth = 0.3, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c("0" = "#EFEFEF", "1" = .rosetta_pal$bin)) +
    ggplot2::geom_rect(data = bars, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = r - 0.35,
                                                 ymax = r + 0.35), fill = "#8A8A8A") +
    ## The count itself, just past each bar (a zero-length bar still shows 0).
    ggplot2::geom_text(data = bars, ggplot2::aes(x = xmax + 0.15, y = r, label = k),
                       hjust = 0, size = 2.6, colour = "#4A4A4A") +
    ggplot2::annotate("text", x = N + 1 + 0.4 * N / 2, y = 0.1, label = "K per row", size = 2.8) +
    ggplot2::scale_y_reverse() + ggplot2::coord_equal(clip = "off") +
    ggplot2::labs(title = "binary pattern 1[W_jh != 0], j != h",
                  subtitle = paste("which components interact, not how much;",
                                   "diagonal blank: XWX sums over j != h", sep = "\n"),
                  x = NULL, y = NULL) +
    ggplot2::theme_void(base_size = 9) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.subtitle = ggplot2::element_text(colour = .rosetta_pal$muted))
  list(p1, p2)
}

.rosetta_theme <- function() {
  ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11.5,
                                                      margin = ggplot2::margin(b = 2)),
                   plot.subtitle = ggplot2::element_text(colour = .rosetta_pal$muted, size = 8.5),
                   plot.margin = ggplot2::margin(4, 6, 4, 6))
}

#' Plot a model and a foundational special case in the classes of the objective
#'
#' Draws the SAOM-NK objective class by class and rewrites a foundational
#' model term by term in the same classes:
#'
#' \describe{
#'   \item{Row I}{the objective of `x` (or the general objective, every class
#'     switched on, when `x` is `NULL`): one summand per class, a chip per
#'     class (filled when switched on, outlined when zero or absent), the
#'     effects it contains, and its construct. Classes on the same \{K\}
#'     dimension sit side by side (K_CC, K_AC, K_AA, K_CA, then dimensions
#'     opened by registered classes, then other), and each dimension gets one
#'     black badge naming its dimension (for example "Sociality K_AA"),
#'     centered under its classes on a bracket when it groups two or more.
#'     `view` chooses the field: the dimension each class moves (the
#'     default), the one it reads, or both, in two badge rows (reads above,
#'     moves below), each with its own brackets. See
#'     [searchnet_effect_dimensions()] and `inst/rosetta/K_DIMENSIONS.md`.}
#'   \item{Row II}{the entry `compare`, in row I's columns: each class's term
#'     in the entry's own notation, `+ 0` for a class it switches off,
#'     outlined chips for classes set to zero, absent or held fixed, and the
#'     status in words.}
#'   \item{Row III}{the bipartite state reached by a tiny seeded run of the
#'     entry's restriction (circles are actors, squares components, curves
#'     the influence matrix), the influence matrix `W` as a signed heatmap,
#'     and its binary pattern `1[W_jh != 0]` off the diagonal with each row's
#'     count, the K that a binary NK influence pattern records. The diagonal
#'     is blank because the engine's XWX statistic sums over `j != h`.}
#' }
#'
#' The dimension groups are attached to the result as `attr(, "k_groups")`,
#' a data.frame with one row per badge (`k_channel`, `name`, `xmin`, `xmax`,
#' `x`, `n`, `classes`), for the field drawn (moves when `view` is
#' `"both"`); with `view = "both"` the reads row's groups are
#' attached as `attr(, "k_groups_reads")`.
#'
#' @param x A model (anything [rosetta_translate()] accepts), or `NULL` for
#'   the general objective.
#' @param compare Entry id for row II, or `NULL` to draw row I only (with
#'   row III when `x` has an influence matrix).
#' @param glyph Logical; run the entry's restriction to draw the bipartite
#'   state (default `TRUE`; one short simulation).
#' @param W Optional influence matrix for row III (default: the matrix of
#'   `x`, else of the entry's restriction, else a seeded modular draw).
#' @param seed Seed for the run and any drawn matrix.
#' @param include_private Logical; allow entries from `entries-private/`.
#' @param classes Class ids to draw (default: every registered class that
#'   is switched on in `x` or `compare`, plus the five shipped core classes).
#' @param view Which field the \{K\} badges show: `"moves"` (default; the
#'   dimension each class's target statistic moves, `k_channel`), `"reads"`
#'   (the dimension its change statistic depends on), or `"both"`.
#' @param file Optional path of a PNG to write; the path is then returned.
#' @param width,height,dpi Size of the PNG in inches, and its resolution. `width`
#'   also sets the layout of row II: long expressions are wrapped, and shrunk
#'   if wrapping is not enough, to fit the figure width so they do not run
#'   into the relation label. When printing without `file`, pass the width of
#'   the device.
#' @return A ggplot object (assembled with \pkg{cowplot}); print it, or save
#'   it with `ggplot2::ggsave()`. With `file`, the PNG is written and its path
#'   returned invisibly.
#' @seealso [rosetta_translate()], [rosetta_compare()]
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("yaml", quietly = TRUE)) {
#'   p <- rosetta_plot(NULL, compare = "nk-adaptive-walk")
#' }
#' }
rosetta_plot <- function(x = NULL, compare = NULL, glyph = TRUE, W = NULL, seed = 1L,
                         include_private = FALSE, classes = NULL, file = NULL,
                         width = 12, height = 10, dpi = 110,
                         view = c("moves", "reads", "both")) {
  view <- match.arg(view)
  cl <- rosetta_classes()
  if (is.null(x)) {
    s <- .rosetta_spec(M = NA, N = NA, effects = lapply(seq_len(nrow(cl)), function(i) {
      eff <- trimws(strsplit(cl$effects[i], ",")[[1]])
      list(class = cl$id[i], effect = paste(eff, collapse = ", "), parameter = 1, fix = TRUE)
    }), source = "general objective")
    title <- "I. The SAOM-NK objective by class of effects"
  } else {
    s <- .rosetta_as_spec(x, include_private)
    title <- sprintf("I. Model%s by class of effects",
                     if (!is.null(s$source)) paste0(" (", s$source, ")") else "")
  }
  t1 <- .rosetta_terms_of_spec(s, cl)
  if (is.null(x)) t1$effects <- cl$effects[match(t1$class, cl$id)]
  e <- if (!is.null(compare)) rosetta_entry(compare, include_private) else NULL
  t2 <- if (!is.null(e)) .rosetta_terms_of_entry(e, cl) else NULL
  core <- c("complementarity", "scope", "crowding", "contact", "imitation")
  if (is.null(classes)) {
    classes <- union(core, c(if (!is.null(x)) t1$class[t1$status %in% .ROSETTA_ON & t1$class != "other"],
                             if (!is.null(t2)) t2$class[t2$status %in% .ROSETTA_ON]))
  }
  t1 <- t1[match(intersect(classes, t1$class), t1$class), , drop = FALSE]
  kgd <- NULL
  if (view == "moves") {
    t1 <- .rosetta_order_by_k(t1)
    kg <- .rosetta_k_groups(t1)
  } else if (view == "reads") {
    t1 <- .rosetta_order_by_k(t1, by = "reads")
    kg <- .rosetta_k_groups(t1, by = "reads")
  } else {
    t1 <- .rosetta_order_by_k(t1, by = "moves", then = "reads")
    kg <- .rosetta_k_groups(t1, by = "moves")
    kgd <- .rosetta_k_groups(t1, by = "reads")
  }
  rows <- list(.rosetta_row_objective(t1, cl, title, kg, kgd))
  heights <- c(1)
  es <- NULL
  if (!is.null(e)) {
    t2 <- t2[match(t1$class, t2$class), , drop = FALSE]
    rows[[2]] <- .rosetta_row_special(t2, cl, e, kg, show_k = FALSE, width_in = width)
    heights <- c(heights, 1)
    es <- tryCatch(rosetta_model(e, M = if (is.numeric(e$restrictions$M)) NULL else 3L,
                                 N = 6L, K = 2L, seed = seed),
                   error = function(err) NULL)
  }
  Wm <- .rs_or(W, .rs_or(s$influence_matrix, .rs_or(if (!is.null(es)) es$influence_matrix,
                                                     .rosetta_real_W(8L, 2L, seed))))
  bottom <- list()
  if (!is.null(es) && glyph) {
    run <- tryCatch(rosetta_run(es, steps_per_actor = 3, seed = seed), error = function(err) NULL)
    if (!is.null(run)) {
      on_c <- "complementarity" %in% t2$class[t2$status %in% .ROSETTA_ON]
      bottom[[1]] <- .rosetta_glyph(run$B, es$influence_matrix, cl, show_w = on_c,
                                    title = sprintf("state after a short run (seed %d)", seed))
    }
  }
  if (!is.null(es) || !is.null(s$influence_matrix) || !is.null(W)) {
    bottom <- c(bottom, .rosetta_w_panels(as.matrix(Wm)))
  }
  if (length(bottom)) {
    rows[[length(rows) + 1L]] <- cowplot::plot_grid(plotlist = bottom, nrow = 1,
                                                    rel_widths = c(rep(1, length(bottom))))
    heights <- c(heights, 1.15)
  }
  badge_txt <- switch(view,
    moves = "Black badge: the {K} dimension its classes move, one per dimension; a bracket spans classes that share it.",
    reads = "Black badge: the {K} dimension its classes read, one per dimension; a bracket spans classes that share it.",
    both = "Black badges: upper row, the {K} dimension the classes read; lower row, the dimension they move; brackets span classes that share one.")
  key <- paste("Filled chip: class switched on. Outlined chip: zero, absent or held fixed.",
               badge_txt, sep = "\n")
  head <- ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = key, size = 3,
                                                colour = .rosetta_pal$muted, hjust = 0) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) + ggplot2::theme_void()
  p <- cowplot::plot_grid(plotlist = c(list(head), rows), ncol = 1, rel_heights = c(0.12, heights))
  attr(p, "k_groups") <- kg
  if (!is.null(kgd)) attr(p, "k_groups_reads") <- kgd
  if (is.null(file)) return(p)
  ggplot2::ggsave(file, p, width = width, height = height, dpi = dpi, bg = "white")
  invisible(normalizePath(file, winslash = "/"))
}
