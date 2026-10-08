#' Standalone plot functions for SaoMNK degree visualizations.
#'
#' Extracted from saomnk-class.R plot methods. Each function takes an
#' \code{env} (SaomNkRSienaBiEnv) object as its first argument in place of
#' the former \code{self} reference. Since searchnet 0.11.2.9000 all three
#' are drawn by one builder (R/plot-readable.R) shared with the R6 methods.

# -- plot_degree_4panel --------------------------------------------------------

#' Plot the four-panel degree grid (K_AC, K_CA, K_AA, K_CC)
#'
#' One panel per \eqn{\{K\}} dimension, labeled with its display name:
#' expansiveness (\eqn{K_{AC}}, components each actor holds), popularity
#' (\eqn{K_{CA}}, actors holding each component), sociality (\eqn{K_{AA}},
#' the number of OTHER actors each actor shares at least one component with)
#' and epistasis (\eqn{K_{CC}}, the number of OTHER components each component
#' is co-held with). The two projection degrees are partner counts that
#' exclude the node itself (through searchnet 0.12.1 the engine
#' counted it, adding 1 for every non-isolated node). Faint points are nodes at each ministep (actors
#' orange, components blue, or one color per group when there are several);
#' the black line is the loess-smoothed mean. The title states what the run
#' shows (computed from the smoothed means), the subtitle how to read the
#' figure, and the caption lists the model weights. A shock is marked by a
#' dashed vermillion line.
#'
#' @param env SaomNkRSienaBiEnv object
#' @param loess_span Numeric span for loess smoother
#' @param plot_return Logical; if TRUE return the ggplot object
#' @param plot_save Logical; if TRUE save to disk
#' @param plot_file Character string appended to output filename
#' @param plot_dir Directory for saved plot; defaults to working directory
#' @param thin_factor Integer thinning factor for chain steps
#' @param thin_pct Numeric proportion of rows to sample (0-1)
#' @param point_alpha_dimmer Numeric multiplier to dim point alpha
#' @param experiment Character experiment name (key into env$experiments)
#' @param annotate Logical. If \code{TRUE} (default), add reading guides on
#'   the data: a "mean" label on the first panel's line, and a note with an
#'   arrow marking either the level the first series settles at or, in a
#'   shocked run, the shock and how far the first series moves. \code{FALSE}
#'   draws the same figure without them.
#' @return A ggplot object (if \code{plot_return} is TRUE)
#' @export
saomnk_plot_degree_4panel <- function(env,
                                      loess_span = 0.5,
                                      plot_return = TRUE,
                                      plot_save = FALSE,
                                      plot_file = '',
                                      plot_dir = NA,
                                      thin_factor = 1,
                                      thin_pct = 1,
                                      point_alpha_dimmer = 1,
                                      experiment = '',
                                      annotate = TRUE) {
  env$plot_degree_4panel(loess_span = loess_span, plot_return = plot_return,
                         plot_save = plot_save, plot_file = plot_file,
                         plot_dir = plot_dir, thin_factor = thin_factor,
                         thin_pct = thin_pct, point_alpha_dimmer = point_alpha_dimmer,
                         experiment = experiment, annotate = annotate)
}


# -- plot_component_degrees ----------------------------------------------------

#' Plot component degrees (K_CA and K_CC) over the simulation chain
#'
#' Popularity (\eqn{K_{CA}}) above epistasis (\eqn{K_{CC}}), in the style of
#' \code{\link{saomnk_plot_degree_4panel}}.
#'
#' @param env SaomNkRSienaBiEnv object
#' @param loess_span Numeric span for loess smoother
#' @param return_plot Logical; if TRUE return the ggplot object
#' @param annotate Logical. Add reading guides (default \code{TRUE}); see
#'   \code{\link{saomnk_plot_degree_4panel}}.
#' @return A ggplot object (if \code{return_plot} is TRUE)
#' @export
saomnk_plot_component_degrees <- function(env,
                                          loess_span = 0.5,
                                          return_plot = TRUE,
                                          annotate = TRUE) {
  .searchnet_require_path(env, "saomnk_plot_component_degrees()")
  env$plot_component_degrees(loess_span = loess_span, return_plot = return_plot,
                             annotate = annotate)
}


# -- plot_actor_degrees --------------------------------------------------------

#' Plot actor degrees (K_AC and K_AA) over the simulation chain
#'
#' Scope (\eqn{K_{AC}}) above sociality (\eqn{K_{AA}}), in the style of
#' \code{\link{saomnk_plot_degree_4panel}}.
#'
#' @param env SaomNkRSienaBiEnv object
#' @param loess_span Numeric span for loess smoother
#' @param return_plot Logical; if TRUE return the ggplot object
#' @param annotate Logical. Add reading guides (default \code{TRUE}); see
#'   \code{\link{saomnk_plot_degree_4panel}}.
#' @return A ggplot object (if \code{return_plot} is TRUE)
#' @export
saomnk_plot_actor_degrees <- function(env,
                                      loess_span = 0.5,
                                      return_plot = TRUE,
                                      annotate = TRUE) {
  .searchnet_require_path(env, "saomnk_plot_actor_degrees()")
  env$plot_actor_degrees(loess_span = loess_span, return_plot = return_plot,
                         annotate = annotate)
}
