#' Standalone plot functions for SaoMNK utility visualizations.
#'
#' Extracted from saomnk-class.R plot methods. Each function takes an
#' \code{env} (SaomNkRSienaBiEnv) object as its first argument in place of
#' the former \code{self} reference.

# -- plot_actor_utility --------------------------------------------------------

#' Plot individual actor utility over the simulation chain
#'
#' @param env SaomNkRSienaBiEnv object
#' @param xints Numeric vector of x-intercept positions for vertical lines
#' @param thin_factor Integer thinning factor for chain steps
#' @param loess_span Numeric span for loess smoother
#' @param return_plot Logical; if TRUE return the ggplot object
#' @return A ggplot object (if \code{return_plot} is TRUE)
#' @export
saomnk_plot_actor_utility <- function(env,
                                      xints = c(),
                                      thin_factor = 1,
                                      loess_span = 0.5,
                                      return_plot = TRUE) {
  .searchnet_require_path(env, "saomnk_plot_actor_utility()")
  ## Individual actors
  actthin <- env$actor_util_df %>% filter(chain_step_id %% thin_factor == 0)
  print(dim(actthin))
  tmpdf <- actthin %>% mutate(actor_id = as.factor(actor_id))
  npoints <- nrow(env$chain_stats) * env$M
  point_size <- 6 / log10(npoints)
  point_alpha <- min(1, 2.2 / log(npoints))
  suppressMessages({
    plt.act <- ggplot(aes(x = chain_step_id, y = utility, color = strategy),
                      data = tmpdf) +
      geom_point(alpha = point_alpha, shape = 1, size = point_size) +
      geom_smooth(aes(fill = actor_id, linetype = actor_id),
                  method = 'loess', span = loess_span, alpha = .05) +
      geom_smooth(aes(x = chain_step_id, y = mean),
                  data = actthin %>%
                    group_by(chain_step_id, actor_id) %>%
                    dplyr::summarize(mean = mean(utility, na.rm = TRUE)),
                  method = 'loess', color = 'black', span = loess_span,
                  alpha = .05, linewidth = 1.1) +
      geom_hline(yintercept = 0, linetype = 4, color = 'black') +
      theme_searchnet()
  })

  if (length(xints))
    plt.act <- plt.act + geom_vline(xintercept = xints, linetype = 1, color = 'black')

  if (return_plot)
    return(plt.act)
}


# -- plot_strategy_utility -----------------------------------------------------

#' Plot average utility by strategy over the simulation chain
#'
#' @param env SaomNkRSienaBiEnv object
#' @param xints Numeric vector of x-intercept positions for vertical lines
#' @param thin_factor Integer thinning factor for chain steps
#' @param loess_span Numeric span for loess smoother
#' @param return_plot Logical; if TRUE return the ggplot object
#' @return A ggplot object (if \code{return_plot} is TRUE)
#' @export
saomnk_plot_strategy_utility <- function(env,
                                         xints = c(),
                                         thin_factor = 1,
                                         loess_span = 0.5,
                                         return_plot = TRUE) {
  .searchnet_require_path(env, "saomnk_plot_strategy_utility()")
  actthin <- env$actor_util_df %>% filter(chain_step_id %% thin_factor == 0)
  print(dim(actthin))
  tmpdf <- actthin %>% mutate(actor_id = actor_id)
  npoints <- nrow(env$chain_stats) * env$M
  point_size <- 4 / log10(npoints)
  point_alpha <- min(1, 1 / log(npoints))
  suppressMessages({
    plt.act <- tmpdf %>%
      ggplot(aes(x = chain_step_id, y = utility)) +
      geom_point(aes(shape = actor_id, color = strategy),
                 alpha = point_size, shape = 1, size = point_size) +
      geom_smooth(aes(fill = strategy, color = strategy),
                  method = 'loess', span = loess_span, alpha = .1) +
      geom_smooth(aes(x = chain_step_id, y = mean, color = strategy),
                  method = 'loess', color = 'black', span = loess_span,
                  alpha = .05, linewidth = 1.1,
                  data = actthin %>%
                    group_by(chain_step_id) %>%
                    dplyr::summarize(mean = mean(utility, na.rm = TRUE)) %>%
                    mutate(strategy = NA)) +
      geom_hline(yintercept = 0, linetype = 4, color = 'black') +
      ggtitle("Average Utility by Strategy") +
      theme_searchnet()
  })

  if (!is.null(env$theta_shocks)) {
    suppressMessages({
      layout <- ggplot_build(plt.act)$layout
    })
    shock_rects <- env$get_theta_shock_rects_df(env$theta_shocks) %>%
      mutate(utility = 0, chain_step_id = 0, effect_name = NULL, effect = NULL)
    y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(max(x$y.range), nrow(shock_rects))))
    plt.act <- plt.act +
      geom_rect(data = shock_rects,
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                fill = 'grey55', color = 'grey40', linetype = 2, alpha = .08) +
      geom_text(data = shock_rects,
                aes(x = (start + end) / 2, y = y_maxs, label = label),
                vjust = 1, size = 3)
  }

  if (return_plot)
    return(plt.act)
}


# -- plot_utility_contributions_basic ------------------------------------------

#' Plot basic utility contributions (statistic decomposition) without strategy coloring
#'
#' One panel per effect plus the total, in the style of
#' \code{\link{saomnk_plot_utility_contributions}}, without splitting actors by
#' strategy.
#'
#' @param env SaomNkRSienaBiEnv object
#' @param use_thetas Logical; multiply statistics by theta weights
#' @param loess_span Numeric span for loess smoother
#' @param return_plot Logical; if TRUE return the ggplot object
#' @param save_plot Logical; if TRUE save to disk
#' @param thin_factor Integer thinning factor for chain steps
#' @param annotate Logical. Add reading guides (default \code{TRUE}); see
#'   \code{\link{saomnk_plot_utility_contributions}}.
#' @return A ggplot object (if \code{return_plot} is TRUE)
#' @export
saomnk_plot_utility_contributions_basic <- function(env,
                                                    use_thetas = TRUE,
                                                    loess_span = 0.5,
                                                    return_plot = TRUE,
                                                    save_plot = FALSE,
                                                    thin_factor = 1,
                                                    annotate = TRUE) {
  .searchnet_require_path(env, "saomnk_plot_utility_contributions_basic()")
  env$plot_utility_contributions_basic(use_thetas = use_thetas, loess_span = loess_span,
                                       return_plot = return_plot, save_plot = save_plot,
                                       thin_factor = thin_factor, annotate = annotate)
}


# -- plot_utility_contributions ------------------------------------------------

#' Plot utility contributions with strategy coloring and optional experiment data
#'
#' One panel per effect, labeled with a plain name (for example
#' "crowding (inPop): ties to components others hold"), plus a top panel with
#' total utility. With \code{use_thetas = TRUE} each panel is the effect's
#' contribution, statistic times weight, so the panels add up to the total.
#' Without actor strategies each effect has its own color; with strategies,
#' actors are colored by strategy. The title names the largest term at the end
#' of the run, the subtitle says how to read the figure, and the caption lists
#' the model weights. A shock is marked by a dashed vermillion line.
#'
#' @param env SaomNkRSienaBiEnv object
#' @param use_thetas Logical; multiply statistics by theta weights
#' @param loess_span Numeric span for loess smoother
#' @param plot_return Logical; if TRUE return the ggplot object
#' @param plot_save Logical; if TRUE save to disk
#' @param plot_file Character string appended to output filename
#' @param plot_dir Directory for saved plot; defaults to working directory
#' @param hide_zeros Logical; drop facets where all values are zero
#' @param thin_factor Integer thinning factor for chain steps
#' @param thin_pct Numeric proportion of rows to sample (0-1)
#' @param point_alpha_dimmer Numeric multiplier to dim point alpha
#' @param experiment Character experiment name (key into env$experiments)
#' @param annotate Logical. If \code{TRUE} (default), label the largest term
#'   at the end of its line and name the shock, if any, in the top panel.
#' @return A ggplot object (if \code{plot_return} is TRUE)
#' @export
saomnk_plot_utility_contributions <- function(env,
                                              use_thetas = TRUE,
                                              loess_span = 0.5,
                                              plot_return = TRUE,
                                              plot_save = FALSE,
                                              plot_file = '',
                                              plot_dir = NA,
                                              hide_zeros = FALSE,
                                              thin_factor = 1,
                                              thin_pct = 1,
                                              point_alpha_dimmer = 1,
                                              experiment = '',
                                              annotate = TRUE) {
  .searchnet_require_path(env, "saomnk_plot_utility_contributions()")
  env$plot_utility_contributions(use_thetas = use_thetas, loess_span = loess_span,
                                 plot_return = plot_return, plot_save = plot_save,
                                 plot_file = plot_file, plot_dir = plot_dir,
                                 hide_zeros = hide_zeros, thin_factor = thin_factor,
                                 thin_pct = thin_pct,
                                 point_alpha_dimmer = point_alpha_dimmer,
                                 experiment = experiment, annotate = annotate)
}
