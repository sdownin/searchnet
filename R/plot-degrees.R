#' Standalone plot functions for SaoMNK degree visualizations.
#'
#' Extracted from saomnk-class.R plot methods. Each function takes an
#' \code{env} (SaomNkRSienaBiEnv) object as its first argument in place of
#' the former \code{self} reference.

# -- plot_degree_4panel --------------------------------------------------------

#' Plot the four-panel degree grid (K_AA, K_AC, K_CA, K_CC)
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
                                      experiment = '') {
  sim_title_str <- env$get_structure_model_param_str()

  actor_strats <- env$get_actor_strategies()
  avg_mat <- apply(env$bi_env_arr, c(1, 2), mean)
  component_actor_strats <- actor_strats[apply(avg_mat, 2, which.max)]

  Kdf <- if (!is.null(env$experiments) && experiment %in% names(env$experiments)) {
    env$experiments[[experiment]]$K4_df
  } else {
    env$get_K4_df()
  }

  Kdf <- Kdf %>% filter(chain_step_id %% thin_factor == 0)

  suppressMessages({
    Klabels_df <- Kdf %>%
      group_by(panel_label, panel_label_text, node_type, dyad_type) %>%
      dplyr::summarize(n = n())
  })
  Klabels_df$panel_label[which(Klabels_df$node_type == 'Actor'     & Klabels_df$dyad_type == '2-mode  (bipartite)')] <- 'K_AC'
  Klabels_df$panel_label[which(Klabels_df$node_type == 'Component' & Klabels_df$dyad_type == '2-mode  (bipartite)')] <- 'K_CA'

  if (thin_pct < 1) {
    sample_rows <- sample(1:nrow(Kdf), size = round(nrow(Kdf) * thin_pct), replace = FALSE)
    Kdf <- Kdf %>% filter(row_number() %in% sample_rows)
  }

  npoints <- nrow(Kdf)
  point_size <- 6 / log10(npoints)
  point_alpha <- min(1, 15 / sqrt(npoints)) * point_alpha_dimmer

  suppressMessages({
    plt <- Kdf %>%
      ggplot(aes(x = chain_step_id, y = value)) +
      geom_point(aes(fill = node_group, color = node_group),
                 pch = 1, alpha = point_alpha, size = point_size) +
      geom_smooth(aes(group = node_group, color = node_group, fill = node_group),
                  method = 'loess', alpha = .05, span = loess_span) +
      geom_smooth(aes(x = chain_step_id, y = mean), span = loess_span,
                  data = Kdf %>%
                    group_by(chain_step_id, node_type, dyad_type) %>%
                    dplyr::summarize(mean = mean(value)),
                  method = 'loess', se = FALSE, color = 'black', linewidth = 1) +
      geom_text(data = Klabels_df,
                aes(label = panel_label, x = Inf, y = -Inf),
                hjust = 1.15, vjust = -.5, size = 7, color = 'black', fontface = 'bold') +
      geom_hline(yintercept = 0, linetype = 2) +
      scale_y_continuous(position = 'right') +
      facet_grid(dyad_type ~ node_type, switch = 'y') +
      theme_bw() + theme(strip.placement = 'inside', legend.position = 'bottom') +
      labs(group = 'Strategy', fill = 'Strategy', color = 'Strategy') +
      ylab('Node Degree') +
      ggtitle(sprintf('Actor and Component Degrees: K_AA, K_AC, K_CA, K_CC\n%s', sim_title_str))
  })

  if (!is.null(env$theta_shocks)) {
    suppressMessages({
      layout <- ggplot_build(plt)$layout
      shock_rects <- env$get_theta_shock_rects_df(env$theta_shocks) %>%
        mutate(value = 0, chain_step_id = 0, effect_name = NULL, effect = NULL)
      y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(max(x$y.range), nrow(shock_rects))))
      plt <- plt +
        geom_rect(data = shock_rects,
                  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                  fill = 'darkorange', color = 'orange', linetype = 2, alpha = .05) +
        geom_text(data = shock_rects,
                  aes(x = (start + end) / 2, y = y_maxs, label = label),
                  vjust = 1, size = 3)
    })
  }

  if (is.null(actor_strats))
    plt <- plt + theme(legend.position = 'none')

  if (plot_save) {
    plot_file <- paste0('K4panel_', plot_file, round(as.numeric(Sys.time()) * 10))
    ggsave(filename = file.path(ifelse(is.na(plot_dir) || plot_dir == '', getwd(), plot_dir),
                            sprintf("%s_%s.jpeg", env$config_environ_params$name, plot_file)),
           plt,
           width = 8, height = 8, units = 'in', dpi = 400)
  }

  if (plot_return)
    return(plt)
}


# -- plot_component_degrees ----------------------------------------------------

#' Plot component degrees (K_CA and K_CC) over the simulation chain
#'
#' @param env SaomNkRSienaBiEnv object
#' @param loess_span Numeric span for loess smoother
#' @param return_plot Logical; if TRUE return the ggplot object
#' @return A ggplot object (if \code{return_plot} is TRUE)
#' @export
saomnk_plot_component_degrees <- function(env,
                                          loess_span = 0.5,
                                          return_plot = TRUE) {

  Kdf2 <- env$K_CA_df %>% mutate(effect = 'K_CA') %>%
    bind_rows(
      env$K_CC_df %>% mutate(effect = 'K_CC')
    )

  npoints <- nrow(env$chain_stats) * env$M
  point_size <- 6 / log10(npoints)
  point_alpha <- min(1, 2.2 / log(npoints))

  suppressMessages({
    plt <- Kdf2 %>%
      ggplot(aes(x = chain_step_id, y = value)) +
      geom_point(aes(fill = component_id, color = component_id),
                 pch = 1, alpha = point_alpha, size = point_size) +
      geom_smooth(aes(fill = component_id, color = component_id, linetype = component_id),
                  method = 'loess', alpha = .1, span = loess_span) +
      geom_smooth(aes(x = chain_step_id, y = mean), span = loess_span,
                  data = Kdf2 %>%
                    group_by(chain_step_id, effect) %>%
                    dplyr::summarize(mean = mean(value)),
                  method = 'loess', se = FALSE, color = 'black', linewidth = 1) +
      geom_hline(yintercept = 0, linetype = 2) +
      facet_grid(effect ~ .) +
      theme_bw() +
      ggtitle('Component Degrees: K_CA, K_CC')
  })

  if (!is.null(env$theta_shocks)) {
    suppressMessages({
      layout <- ggplot_build(plt)$layout
    })
    shock_rects <- env$get_theta_shock_rects_df(env$theta_shocks) %>%
      mutate(value = 0, chain_step_id = 0, effect_name = NULL, effect = NULL)
    y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(max(x$y.range), nrow(shock_rects))))
    plt <- plt +
      geom_rect(data = shock_rects,
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                fill = 'darkorange', color = 'orange', linetype = 2, alpha = .05) +
      geom_text(data = shock_rects,
                aes(x = (start + end) / 2, y = y_maxs, label = label),
                vjust = 1, size = 3)
  }

  if (return_plot)
    return(plt)
}


# -- plot_actor_degrees --------------------------------------------------------

#' Plot actor degrees (K_AA and K_AC) over the simulation chain
#'
#' @param env SaomNkRSienaBiEnv object
#' @param loess_span Numeric span for loess smoother
#' @param return_plot Logical; if TRUE return the ggplot object
#' @return A ggplot object (if \code{return_plot} is TRUE)
#' @export
saomnk_plot_actor_degrees <- function(env,
                                      loess_span = 0.5,
                                      return_plot = TRUE) {

  Kdf1 <- env$K_AA_df %>% mutate(effect = 'K_AA', node_id = actor_id) %>%
    bind_rows(
      env$K_AC_df %>% mutate(effect = 'K_AC', node_id = actor_id)
    )

  npoints <- nrow(env$chain_stats) * env$M
  point_size <- 6 / log10(npoints)
  point_alpha <- min(1, 2.2 / log(npoints))

  suppressMessages({
    plt <- Kdf1 %>%
      ggplot(aes(x = chain_step_id, y = value)) +
      geom_point(aes(color = strategy, fill = strategy),
                 pch = 1, alpha = point_alpha, size = point_size) +
      geom_smooth(aes(color = strategy, fill = strategy, linetype = actor_id),
                  method = 'loess', alpha = .1, span = loess_span) +
      geom_smooth(aes(x = chain_step_id, y = mean), span = loess_span,
                  data = Kdf1 %>%
                    group_by(chain_step_id, effect) %>%
                    dplyr::summarize(mean = mean(value)),
                  method = 'loess', se = FALSE, color = 'black', linewidth = 1) +
      geom_hline(yintercept = 0, linetype = 2) +
      facet_grid(effect ~ .) +
      theme_bw() +
      ggtitle('Actor Degrees: K_AA, K_AC')
  })

  if (!is.null(env$theta_shocks)) {
    suppressMessages({
      layout <- ggplot_build(plt)$layout
    })
    shock_rects <- env$get_theta_shock_rects_df(env$theta_shocks) %>%
      mutate(value = 0, chain_step_id = 0, effect_name = NULL, effect = NULL)
    y_maxs <- unlist(lapply(layout$panel_params, function(x) rep(max(x$y.range), nrow(shock_rects))))
    plt <- plt +
      geom_rect(data = shock_rects,
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                fill = 'darkorange', color = 'orange', linetype = 2, alpha = .05) +
      geom_text(data = shock_rects,
                aes(x = (start + end) / 2, y = y_maxs, label = label),
                vjust = 1, size = 3)
  }

  if (return_plot)
    return(plt)
}
