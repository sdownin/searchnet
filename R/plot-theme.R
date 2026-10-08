# plot-theme.R
# =============================================================================
# One visual style for every searchnet plot.
#
# The look follows tools/make_readme_figures.R and
# paper/replication/make_figure1_mental_model.R: a white background, a light
# major grid, a bold title over a grey subtitle, the legend at the bottom, and
# the Okabe-Ito colorblind-safe palette for categories. Heatmaps use a white to
# Okabe-Ito blue sequential scale; signed quantities (for example a signed
# influence matrix W) use a blue-white-vermillion diverging scale with white at
# zero.
# =============================================================================


## Okabe and Ito (2008), in the order categories are filled. The first two are
## the actor strategy groups of the README figures, the next two the component
## groups.
.searchnet_okabe_ito <- c(orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
                          vermillion = "#D55E00", blue = "#0072B2",
                          purple = "#CC79A7", yellow = "#F0E442",
                          black = "#000000")

## Sequential: white to Okabe-Ito blue, darkened at the top so a dense cell
## still reads against the blue mid-range.
.searchnet_sequential <- c("#FFFFFF", "#9ED3F0", "#56B4E9", "#0072B2", "#023858")

## Diverging: Okabe-Ito blue below the midpoint, vermillion above, white at the
## midpoint (zero by default).
.searchnet_diverging <- c("#0072B2", "#FFFFFF", "#D55E00")


#' searchnet Plot Theme
#'
#' The ggplot2 theme every searchnet plot uses: a white background, a light
#' major grid with no minor grid, a bold title over a grey subtitle, the legend
#' at the bottom, and light grey facet strips. It matches the figures in the
#' README and the JSS paper.
#'
#' @param base_size Base font size in points.
#' @param base_family Base font family.
#' @param legend_position Legend position, passed to
#'   \code{ggplot2::theme(legend.position = )}. Default \code{"bottom"}.
#' @param grid Logical. Draw the major grid lines. Default \code{TRUE}.
#' @param axes Logical. Draw axis text, ticks and titles. Set \code{FALSE} for
#'   network drawings, where the coordinates carry no meaning; this also drops
#'   the grid and the panel border.
#' @return A ggplot2 \code{theme} object.
#' @seealso \code{\link{searchnet_palette}}, \code{\link{scale_color_searchnet}}
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_searchnet() +
#'   theme_searchnet()
#' @export
theme_searchnet <- function(base_size = 11, base_family = "",
                            legend_position = "bottom", grid = TRUE,
                            axes = TRUE) {
  th <- ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border     = ggplot2::element_rect(fill = NA, color = "grey70"),
      panel.grid.major = ggplot2::element_line(color = "grey92", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 1,
                                               margin = ggplot2::margin(b = 3)),
      plot.subtitle    = ggplot2::element_text(color = "grey30",
                                               size = base_size - 1.5),
      plot.caption     = ggplot2::element_text(color = "grey40",
                                               size = base_size - 2.5),
      axis.title       = ggplot2::element_text(color = "grey20"),
      axis.text        = ggplot2::element_text(color = "grey25",
                                               size = base_size - 2),
      axis.ticks       = ggplot2::element_line(color = "grey70"),
      strip.background = ggplot2::element_rect(fill = "grey94", color = "grey70"),
      strip.text       = ggplot2::element_text(color = "grey15",
                                               size = base_size - 2,
                                               margin = ggplot2::margin(3, 3, 3, 3)),
      legend.position  = legend_position,
      legend.key       = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.title     = ggplot2::element_text(size = base_size - 1.5),
      legend.text      = ggplot2::element_text(size = base_size - 2)
    )
  ## panel.grid.major is set explicitly above, and an explicit child element
  ## wins over a blank parent, so blank it too.
  if (!isTRUE(grid))
    th <- th + ggplot2::theme(panel.grid = ggplot2::element_blank(),
                              panel.grid.major = ggplot2::element_blank())
  if (!isTRUE(axes))
    th <- th + ggplot2::theme(panel.grid   = ggplot2::element_blank(),
                              panel.grid.major = ggplot2::element_blank(),
                              panel.border = ggplot2::element_blank(),
                              axis.text    = ggplot2::element_blank(),
                              axis.ticks   = ggplot2::element_blank(),
                              axis.title   = ggplot2::element_blank())
  th
}


#' searchnet Color Palettes
#'
#' Colors for searchnet plots. \code{"categorical"} is the Okabe-Ito
#' colorblind-safe palette (orange, sky blue, bluish green, vermillion, blue,
#' reddish purple, yellow, black). \code{"sequential"} runs from white to
#' Okabe-Ito blue, for counts and heatmaps. \code{"diverging"} runs from blue
#' through white to vermillion, for signed quantities such as a signed
#' influence matrix W, where white marks zero.
#'
#' @param type One of \code{"categorical"}, \code{"sequential"},
#'   \code{"diverging"}.
#' @param n Number of colors. \code{NULL} returns the defining colors of the
#'   palette (named, for \code{"categorical"}). For \code{"categorical"} with
#'   more than eight categories the Okabe-Ito colors run out, and the palette
#'   falls back to \code{grDevices::hcl.colors(n, "Dark 3")} with a warning.
#' @param reverse Logical. Reverse the order.
#' @return A character vector of hex colors.
#' @seealso \code{\link{scale_color_searchnet}}, \code{\link{theme_searchnet}}
#' @examples
#' searchnet_palette()
#' searchnet_palette("categorical", 3)
#' searchnet_palette("diverging", 5)
#' @export
searchnet_palette <- function(type = c("categorical", "sequential", "diverging"),
                              n = NULL, reverse = FALSE) {
  type <- match.arg(type)
  cols <- switch(type,
    categorical = {
      if (is.null(n)) {
        .searchnet_okabe_ito
      } else if (n <= length(.searchnet_okabe_ito)) {
        unname(.searchnet_okabe_ito[seq_len(n)])
      } else {
        warning(sprintf(paste0("searchnet_palette(): %d categories exceed the 8 ",
                               "Okabe-Ito colors; using hcl.colors(\"Dark 3\")."), n),
                call. = FALSE)
        grDevices::hcl.colors(n, "Dark 3")
      }
    },
    sequential = if (is.null(n)) .searchnet_sequential else
      grDevices::colorRampPalette(.searchnet_sequential)(n),
    diverging  = if (is.null(n)) .searchnet_diverging else
      grDevices::colorRampPalette(.searchnet_diverging)(n)
  )
  if (isTRUE(reverse)) rev(cols) else cols
}


## Shared builder behind scale_color_searchnet() and scale_fill_searchnet().
.scale_searchnet <- function(aesthetics, type, reverse, midpoint, na.value, ...) {
  type <- match.arg(type, c("categorical", "sequential", "diverging"))
  switch(type,
    categorical = ggplot2::discrete_scale(
      aesthetics = aesthetics,
      palette = function(n) searchnet_palette("categorical", n, reverse = reverse),
      na.value = na.value, ...),
    sequential = ggplot2::scale_color_gradientn(
      colours = searchnet_palette("sequential", reverse = reverse),
      aesthetics = aesthetics, na.value = na.value, ...),
    diverging = {
      d <- searchnet_palette("diverging", reverse = reverse)
      ggplot2::scale_color_gradient2(low = d[1], mid = d[2], high = d[3],
                                     midpoint = midpoint, aesthetics = aesthetics,
                                     na.value = na.value, ...)
    })
}

#' searchnet Color and Fill Scales
#'
#' ggplot2 scales built on \code{\link{searchnet_palette}}.
#' \code{type = "categorical"} is a discrete Okabe-Ito scale;
#' \code{"sequential"} is a continuous white-to-blue scale for counts and
#' heatmaps; \code{"diverging"} is a continuous blue-white-vermillion scale with
#' white at \code{midpoint}, for signed quantities.
#'
#' @param type One of \code{"categorical"}, \code{"sequential"},
#'   \code{"diverging"}.
#' @param ... Passed to the underlying ggplot2 scale constructor
#'   (\code{discrete_scale}, \code{scale_color_gradientn} or
#'   \code{scale_color_gradient2}), for example \code{name}, \code{labels},
#'   \code{limits} or \code{guide}.
#' @param reverse Logical. Reverse the palette.
#' @param midpoint Value mapped to white by the diverging scale. Default 0.
#' @param na.value Color for missing values.
#' @return A ggplot2 scale.
#' @seealso \code{\link{theme_searchnet}}
#' @examples
#' library(ggplot2)
#' W <- expand.grid(i = 1:4, j = 1:4)
#' W$w <- c(-1, -0.5, 0, 0.5, 1, 0.25, -0.25, 0, 0.75, -0.75, 0.1, -0.1, 0, 1, -1, 0.5)
#' ggplot(W, aes(j, i, fill = w)) + geom_tile() +
#'   scale_fill_searchnet("diverging") + theme_searchnet()
#' @export
scale_color_searchnet <- function(type = "categorical", ..., reverse = FALSE,
                                  midpoint = 0, na.value = "grey85") {
  .scale_searchnet("colour", type, reverse, midpoint, na.value, ...)
}

#' @rdname scale_color_searchnet
#' @export
scale_colour_searchnet <- scale_color_searchnet

#' @rdname scale_color_searchnet
#' @export
scale_fill_searchnet <- function(type = "categorical", ..., reverse = FALSE,
                                 midpoint = 0, na.value = "grey85") {
  .scale_searchnet("fill", type, reverse, midpoint, na.value, ...)
}


# -----------------------------------------------------------------------------
# Internal: an RSiena sienaGOF object drawn in the package style
# -----------------------------------------------------------------------------
## RSiena's own plot.sienaGOF() draws with lattice. This reads the same fields
## (Simulations, Observations, p, and the "key" attribute) and draws the same
## content with ggplot2: a violin of the simulated statistic at each level, the
## simulated 5% and 95% quantiles as dashed lines, and the observed statistic
## in vermillion. As in RSiena, levels with zero variance or NaN are dropped.
.searchnet_gof_ggplot <- function(gof, title = NULL, period = 1, perc = 0.05) {
  joined <- isTRUE(attr(gof, "joined"))
  x <- if (joined) gof[[1]] else gof[[period]]
  sims <- as.matrix(x$Simulations)
  obs  <- matrix(x$Observations, ncol = ncol(sims))[1, ]
  key  <- attr(x, "key")
  if (is.null(key) || length(key) != ncol(sims))
    key <- as.character(seq_len(ncol(sims)))
  both <- rbind(sims, obs)
  keep <- colSums(is.nan(both)) == 0 & apply(both, 2, stats::var) > 0
  if (!any(keep)) keep <- colSums(is.nan(both)) == 0
  lev  <- key[keep]
  sim_df <- data.frame(stat  = factor(rep(lev, each = nrow(sims)), levels = lev),
                       value = as.vector(sims[, keep, drop = FALSE]))
  q <- apply(sims[, keep, drop = FALSE], 2, stats::quantile,
             probs = c(perc, 1 - perc), names = FALSE)
  band <- data.frame(stat = factor(rep(lev, 2), levels = lev),
                     value = c(q[1, ], q[2, ]),
                     which = rep(c("lo", "hi"), each = length(lev)))
  obs_df <- data.frame(stat = factor(lev, levels = lev), value = obs[keep])
  if (is.null(title))
    title <- paste("Goodness of Fit of", attr(gof, "auxiliaryStatisticName"))
  pal <- searchnet_palette()
  ggplot2::ggplot(sim_df, ggplot2::aes(.data$stat, .data$value)) +
    ggplot2::geom_violin(fill = pal[["sky"]], color = NA, alpha = 0.35,
                         scale = "width") +
    ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, color = "grey35",
                          fill = "white") +
    ggplot2::geom_line(data = band, ggplot2::aes(group = .data$which),
                       linetype = "dashed", color = "grey45") +
    ggplot2::geom_line(data = obs_df, ggplot2::aes(group = 1),
                       color = pal[["vermillion"]], linewidth = 0.8) +
    ggplot2::geom_point(data = obs_df, color = pal[["vermillion"]], size = 2) +
    ggplot2::labs(title = title,
                  subtitle = sprintf(paste0("Violins: simulated; dashed: %g%% and %g%% ",
                                            "quantiles; vermillion: observed"),
                                     100 * perc, 100 * (1 - perc)),
                  x = paste("p:", paste(round(x$p, 3), collapse = " ")),
                  y = "Statistic") +
    theme_searchnet()
}
