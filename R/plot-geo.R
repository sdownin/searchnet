#' @title Geographic layouts for component interaction networks
#' @description
#' searchnet represents a search environment as a bipartite network of actors
#' affiliating with components, where the component-by-component interaction
#' matrix W supplies the coupling structure. When components are real places
#' (country markets, regions, sites), that interaction structure is far easier
#' to read on a map than in a force-directed layout: the question is usually
#' whether coupling tracks geography or cuts across it, and only a geographic
#' layout answers that by eye.
#'
#' These functions plot an influence matrix over geographic coordinates,
#' with edges drawn as great-circle arcs.
#'
#' @name plot-geo
NULL


# ---------------------------------------------------------------------------- #
#  saomnk_geo_arc
# ---------------------------------------------------------------------------- #

#' Great-circle arc between two points
#'
#' Spherical linear interpolation between two lon/lat points, returned as a
#' data frame of intermediate positions. Used to draw edges that follow the
#' earth's curvature rather than straight lines in projected space.
#'
#' @param lon1,lat1 Numeric. Origin longitude and latitude in degrees.
#' @param lon2,lat2 Numeric. Destination longitude and latitude in degrees.
#' @param n Integer. Number of interpolated points (default 50).
#'
#' @return A data frame with columns \code{lon} and \code{lat}.
#' @examples
#' ## Kansas to Tokyo, 20 interpolated points along the great circle
#' arc <- saomnk_geo_arc(-98, 39, 138, 36, n = 20)
#' head(arc)
#' @export
saomnk_geo_arc <- function(lon1, lat1, lon2, lat2, n = 50) {
  d2r <- pi / 180
  p1 <- c(cos(lat1 * d2r) * cos(lon1 * d2r),
          cos(lat1 * d2r) * sin(lon1 * d2r), sin(lat1 * d2r))
  p2 <- c(cos(lat2 * d2r) * cos(lon2 * d2r),
          cos(lat2 * d2r) * sin(lon2 * d2r), sin(lat2 * d2r))
  om <- acos(max(-1, min(1, sum(p1 * p2))))
  if (om < 1e-9) {
    return(data.frame(lon = c(lon1, lon2), lat = c(lat1, lat2)))
  }
  f <- seq(0, 1, length.out = n)
  a <- sin((1 - f) * om) / sin(om)
  b <- sin(f * om) / sin(om)
  x <- outer(a, p1) + outer(b, p2)
  lat <- asin(x[, 3]) / d2r
  lon <- atan2(x[, 2], x[, 1]) / d2r
  # split arcs that cross the antimeridian so ggplot does not draw a seam
  jump <- which(abs(diff(lon)) > 180)
  if (length(jump)) lon[seq_len(jump[1])] <- lon[seq_len(jump[1])] +
    ifelse(mean(lon) < 0, -360, 360) * 0
  data.frame(lon = lon, lat = lat)
}


# ---------------------------------------------------------------------------- #
#  saomnk_plot_geo_network
# ---------------------------------------------------------------------------- #

#' Plot a component influence matrix on geographic coordinates
#'
#' Draws the component-by-component influence matrix \code{W} as a network
#' positioned by real-world coordinates, with a world basemap. Edge opacity and
#' width encode coupling strength; node size and fill encode any component-level
#' attribute (component degree, actor count, treatment status).
#'
#' @param W Numeric \eqn{N \times N} influence matrix. Row and column names
#'   must match \code{coords$id}.
#' @param coords Data frame with columns \code{id}, \code{lon}, \code{lat}.
#' @param top_edges Integer. Draw only the strongest this-many edges, which
#'   keeps a dense W legible (default 250). Set \code{NULL} for all non-zero.
#' @param node_size Named numeric vector, or \code{NULL} for constant size.
#' @param node_value Named numeric or factor vector mapped to node fill.
#' @param edge_color,node_color,bg,land Colours.
#' @param label Character vector of component ids to label, or \code{NULL}.
#' @param title,subtitle Character.
#' @param negative Logical. If \code{TRUE}, treat W as a dissociative coupling
#'   (conflict, distance, sanction) and colour edges accordingly.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' cc <- data.frame(id = c("USA","GBR","JPN"),
#'                  lon = c(-98, -2, 138), lat = c(39, 54, 36))
#' W <- matrix(runif(9), 3, 3, dimnames = list(cc$id, cc$id))
#' saomnk_plot_geo_network(W, cc)
#' }
#' @export
saomnk_plot_geo_network <- function(W, coords,
                                    top_edges  = 250,
                                    node_size  = NULL,
                                    node_value = NULL,
                                    edge_color = "#4FD1C5",
                                    node_color = "#FFFFFF",
                                    bg         = "#0B1220",
                                    land       = "#1B2740",
                                    label      = NULL,
                                    title      = NULL,
                                    subtitle   = NULL,
                                    negative   = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for saomnk_plot_geo_network()")

  ids <- intersect(rownames(W), coords$id)
  if (length(ids) < 2) stop("W rownames and coords$id do not overlap")
  W <- W[ids, ids, drop = FALSE]
  cd <- coords[match(ids, coords$id), , drop = FALSE]
  rownames(cd) <- ids

  # ---- edge list from the upper triangle ----
  n <- length(ids)
  iu <- which(upper.tri(W) & W > 0, arr.ind = TRUE)
  if (!nrow(iu)) stop("W has no positive off-diagonal entries")
  ed <- data.frame(a = ids[iu[, 1]], b = ids[iu[, 2]],
                   w = W[iu], stringsAsFactors = FALSE)
  ed <- ed[order(-ed$w), ]
  if (!is.null(top_edges)) ed <- utils::head(ed, top_edges)

  arcs <- do.call(rbind, lapply(seq_len(nrow(ed)), function(k) {
    p <- saomnk_geo_arc(cd[ed$a[k], "lon"], cd[ed$a[k], "lat"],
                        cd[ed$b[k], "lon"], cd[ed$b[k], "lat"])
    p$grp <- k; p$w <- ed$w[k]; p
  }))

  world <- ggplot2::map_data("world")

  nd <- data.frame(id = ids, lon = cd$lon, lat = cd$lat,
                   stringsAsFactors = FALSE)
  nd$size <- if (is.null(node_size)) 2.2 else
    as.numeric(node_size[match(ids, names(node_size))])
  nd$size[is.na(nd$size)] <- min(nd$size, na.rm = TRUE)
  if (!is.null(node_value)) nd$val <- node_value[match(ids, names(node_value))]

  hi <- if (negative) "#F6AD55" else edge_color
  lo <- if (negative) "#7B341E" else "#1C4E5A"

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = world,
      ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
      fill = land, colour = NA) +
    ggplot2::geom_path(data = arcs,
      ggplot2::aes(x = .data$lon, y = .data$lat, group = .data$grp,
                   alpha = .data$w, linewidth = .data$w, colour = .data$w)) +
    ggplot2::scale_colour_gradient(low = lo, high = hi, guide = "none") +
    ggplot2::scale_alpha(range = c(0.06, 0.55), guide = "none") +
    ggplot2::scale_linewidth(range = c(0.15, 0.9), guide = "none")

  if (is.null(node_value)) {
    p <- p + ggplot2::geom_point(data = nd,
      ggplot2::aes(x = .data$lon, y = .data$lat, size = .data$size),
      colour = node_color, alpha = 0.9, stroke = 0)
  } else {
    p <- p + ggplot2::geom_point(data = nd,
      ggplot2::aes(x = .data$lon, y = .data$lat, size = .data$size,
                   fill = .data$val),
      shape = 21, colour = "#0B1220", stroke = 0.3, alpha = 0.95)
  }
  p <- p + ggplot2::scale_size(range = c(1.2, 7), guide = "none")

  if (!is.null(label) && requireNamespace("ggrepel", quietly = TRUE)) {
    lb <- nd[nd$id %in% label, ]
    p <- p + ggrepel::geom_text_repel(data = lb,
      ggplot2::aes(x = .data$lon, y = .data$lat, label = .data$id),
      colour = "#CADCFC", size = 2.9, segment.colour = "#3A4A6B",
      min.segment.length = 0, max.overlaps = 40, seed = 1)
  }

  p +
    ggplot2::coord_fixed(1.35, xlim = c(-165, 175), ylim = c(-55, 78),
                         expand = FALSE) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg, colour = NA),
      plot.title    = ggplot2::element_text(colour = "#FFFFFF", size = 15,
                                            face = "bold",
                                            margin = ggplot2::margin(b = 3)),
      plot.subtitle = ggplot2::element_text(colour = "#8FA3C8", size = 10,
                                            margin = ggplot2::margin(b = 6)),
      legend.position = "none",
      plot.margin = ggplot2::margin(10, 10, 8, 10))
}
