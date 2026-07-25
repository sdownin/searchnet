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
                    dplyr::summarize(mean = mean(utility, na.rm = T)),
                  method = 'loess', color = 'black', span = loess_span,
                  alpha = .05, linewidth = 1.1) +
      geom_hline(yintercept = 0, linetype = 4, color = 'black') +
      theme_bw()
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
                    dplyr::summarize(mean = mean(utility, na.rm = T)) %>%
                    mutate(strategy = NA)) +
      geom_hline(yintercept = 0, linetype = 4, color = 'black') +
      ggtitle("Average Utility by Strategy") +
      theme_bw()
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
                fill = 'darkorange', color = 'orange', linetype = 2, alpha = .08) +
      geom_text(data = shock_rects,
                aes(x = (start + end) / 2, y = y_maxs, label = label),
                vjust = 1, size = 3)
  }

  if (return_plot)
    return(plt.act)
}


# -- plot_utility_contributions_basic ------------------------------------------

#' Plot basic utility contributions (statistic decomposition) without strategy colouring
#'
#' @param env SaomNkRSienaBiEnv object
#' @param use_thetas Logical; multiply statistics by theta weights
#' @param loess_span Numeric span for loess smoother
#' @param return_plot Logical; if TRUE return the ggplot object
#' @param save_plot Logical; if TRUE save to disk
#' @param thin_factor Integer thinning factor for chain steps
#' @return A ggplot object (if \code{return_plot} is TRUE)
#' @export
saomnk_plot_utility_contributions_basic <- function(env,
                                                    use_thetas = TRUE,
                                                    loess_span = 0.5,
                                                    return_plot = TRUE,
                                                    save_plot = FALSE,
                                                    thin_factor = 1) {

  theta_df_norates <- env$get_rsiena_effects_theta_df(no_rates = TRUE)
  theta_levels_norate <- theta_df_norates$effect_level
  sim_title_str <- env$get_structure_model_param_str()
  efflvls <- c('UTILITY', theta_levels_norate)
  actor_stats_df <- env$actor_stats_df
  actor_util_df <- env$actor_util_df %>% mutate(
    effect_id = NA,
    value = utility,
    effect_name = 'UTILITY',
    effect_level = 'UTILITY',
    utility = NULL
  )

  plt_title <- sprintf('Actor Utility Statistics Decomposition:\n%s', sim_title_str)

  ## use actor stats contributions to utility instead of original stats
  if (use_thetas) {
    actor_stats_df <- actor_stats_df %>% mutate(value = value_contributions)
    plt_title <- sprintf('Actor Utility Contributions (statistic * theta):\n%s', sim_title_str)
  }

  ## Add utility as extra 'effect'
  act_effs <- actor_stats_df %>% bind_rows(actor_util_df)
  act_effs$effect_level <- factor(act_effs$effect_level, levels = efflvls)

  # plot signals
  act_effs2 <- act_effs %>% filter(chain_step_id %% thin_factor == 0)

  npoints <- nrow(env$chain_stats) * env$M
  point_size <- 4 / log10(npoints)
  point_alpha <- min(1, 15 / sqrt(npoints))

  plt2 <- act_effs2 %>% ggplot(aes(x = chain_step_id, y = value, linetype = actor_id))

  suppressMessages({
    plt2 <- plt2 +
      geom_point(alpha = point_alpha, shape = 1, size = point_size) +
      geom_smooth(method = 'loess', alpha = .1, span = loess_span) +
      geom_hline(yintercept = 0, linetype = 2) +
      facet_grid(effect_level ~ ., scales = 'free_y') +
      theme_bw() + theme(legend.position = 'bottom') +
      ggtitle(plt_title)
  })

  if (!is.null(env$theta_shocks)) {
    suppressMessages({
      layout <- ggplot_build(plt2)$layout
    })
    shock_rects <- env$get_theta_shock_rects_df(env$theta_shocks) %>%
      mutate(value = 0, chain_step_id = 0)
    y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(max(x$y.range), nrow(shock_rects))))
    plt2 <- plt2 + geom_rect(data = shock_rects,
                              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                              fill = 'darkorange', color = 'orange', linetype = 2, alpha = .05)
    plt2 <- plt2 + geom_text(data = shock_rects,
                              aes(x = (start + end) / 2, y = y_maxs, label = label),
                              vjust = 1, size = 2.7)
  }

  plotname2 <- sprintf('plot_actor_utility_components_thin%s_%s.png',
                        thin_factor, round(as.numeric(Sys.time()) * 100))

  if (save_plot) {
    plot_dir <- getwd()
    ggsave(filename = file.path(plot_dir, plotname2), plt2,
           height = 12, width = 8, dpi = 600, units = 'in')
  }

  if (return_plot)
    return(plt2)
}


# -- plot_utility_contributions ------------------------------------------------

#' Plot utility contributions with strategy colouring and optional experiment data
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
                                              experiment = '') {

  theta_df_norates <- env$get_rsiena_effects_theta_df(no_rates = TRUE)
  theta_levels_norate <- theta_df_norates$effect_level
  sim_title_str <- env$get_structure_model_param_str()
  efflvls <- c('UTILITY', theta_levels_norate)
  actor_strats <- env$get_actor_strategies()

  plt_title <- sprintf('Actor Utility Statistics Decomposition:\n%s', sim_title_str)
  if (use_thetas) {
    plt_title <- sprintf('Actor Utility Contributions (statistic * theta):\n%s', sim_title_str)
  }

  act_effs2 <- env$get_actor_utility_effects(
    use_thetas = use_thetas,
    thin_factor = thin_factor,
    thin_pct = thin_pct,
    experiment = experiment
  )

  if (hide_zeros) {
    act_effs2 <- act_effs2 %>% group_by(effect_level) %>% filter(any(value != 0)) %>% ungroup()
    act_effs2$effect_level <- droplevels(act_effs2$effect_level)
  }

  npoints <- nrow(act_effs2)
  point_size <- 4 / log10(npoints)
  point_alpha <- min(1, 15 / sqrt(npoints)) * point_alpha_dimmer

  plt2 <- act_effs2 %>% ggplot(aes(x = chain_step_id, y = value))

  suppressMessages({
    plt2 <- plt2 +
      geom_point(aes(color = strategy, fill = strategy),
                 alpha = point_alpha, shape = 1, size = point_size) +
      geom_smooth(aes(color = strategy, fill = strategy, linetype = strategy),
                  method = 'loess', alpha = .1, span = loess_span) +
      geom_hline(yintercept = 0, linetype = 2) +
      facet_grid(effect_level ~ ., scales = 'free_y') +
      theme_bw() + theme(legend.position = 'bottom') +
      ggtitle(plt_title)
  })

  if (!is.null(env$theta_shocks)) {
    suppressMessages({
      layout <- ggplot_build(plt2)$layout
    })
    shock_rects <- env$get_theta_shock_rects_df(env$theta_shocks) %>%
      mutate(value = 0, chain_step_id = 0)
    y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(max(x$y.range), nrow(shock_rects))))
    plt2 <- plt2 + geom_rect(data = shock_rects,
                              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                              fill = 'darkorange', color = 'orange', linetype = 2, alpha = .05)
    plt2 <- plt2 + geom_text(data = shock_rects,
                              aes(x = (start + end) / 2, y = y_maxs, label = label),
                              vjust = 1, size = 2.7)
  }

  if (plot_save) {
    nfacets <- length(efflvls)
    plot_file <- paste0('util_contribs_', plot_file, round(as.numeric(Sys.time()) * 10))
    ggsave(file = file.path(ifelse(is.na(plot_dir) || plot_dir == '', getwd(), plot_dir),
                            sprintf("%s_%s.jpeg", env$config_environ_params$name, plot_file)),
           plt2,
           width = 8, height = 2 + 1.2 * nfacets, units = 'in', dpi = 400)
  }

  if (plot_return)
    return(plt2)
}
