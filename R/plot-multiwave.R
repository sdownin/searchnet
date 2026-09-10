# ============================================================================
# plot-multiwave.R
# Standalone multiwave plot functions extracted from saomnk-class.R
# Each function takes `env` (an SaoMNK environment) as its first argument.
# All self$ references have been replaced with env$.
# ============================================================================


#' @export
saomnk_search_rsiena_multiwave_plot <- function(env,
                                                 type = c(),
                                                 rolling_window = 10,
                                                 actor_ids = c(),
                                                 component_ids = c(),
                                                 wave_ids = c(),
                                                 thin_factor = 1,
                                                 thin_wave_factor = 1,
                                                 smooth_method = 'loess',
                                                 show_utility_points = TRUE,
                                                 show_strategy_means = TRUE,
                                                 append_plot = FALSE,
                                                 histogram_position = 'identity',
                                                 scale_utility = TRUE,
                                                 return_plot = TRUE,
                                                 plot_file = NA, plot_dir = NA,
                                                 loess_span = 0.4) {
  plist <- list()
  if (length(type)==0 |  'K_4panel' %in% type)
    plist[['K_4panel']] <- saomnk_search_rsiena_multiwave_plot_K_4panel(env, actor_ids, component_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )

  if (length(type)==0 |  'K_AA_strategy_summary' %in% type)
    plist[['K_AA_strategy_summary']] <- saomnk_search_rsiena_multiwave_plot_K_AA_strategy_summary(env, actor_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )

  if (length(type)==0 |  'K_AC_strategy_summary' %in% type)
    plist[['K_AC_strategy_summary']] <- saomnk_search_rsiena_multiwave_plot_K_AC_strategy_summary(env, actor_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )

  if (length(type)==0 |  'K_CA_strategy_summary' %in% type)
    plist[['K_CA_strategy_summary']] <- saomnk_search_rsiena_multiwave_plot_K_CA_strategy_summary(env, component_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )

  if (length(type)==0 |  'K_CC_strategy_summary' %in% type)
    plist[['K_CC_strategy_summary']] <- saomnk_search_rsiena_multiwave_plot_K_CC_strategy_summary(env, component_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )


  if (length(type)==0 |  'utility_strategy_summary' %in% type)
    plist[['utility_strategy_summary']] <- saomnk_search_rsiena_multiwave_plot_actor_utility_strategy_summary(env, actor_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, scale_utility, return_plot=TRUE, plot_file=plot_file, loess_span=loess_span )

  if (length(type)==0 |  'utility_by_strategy' %in% type)
    plist[['utility_by_strategy']] <- saomnk_search_rsiena_multiwave_plot_actor_utility_by_strategy(env, actor_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, return_plot=TRUE, plot_file=plot_file )
  if (length(type)==0 |  'utility_density_by_strategy' %in% type)
    plist[['utility_density_by_strategy']] <- saomnk_search_rsiena_multiwave_plot_actor_utility_density_by_strategy(env, thin_wave_factor, return_plot=TRUE, plot_file=plot_file )
  if (length(type)==0 |  'utility_ridge_density_by_strategy' %in% type)
    plist[['utility_ridge_density_by_strategy']] <- saomnk_search_rsiena_multiwave_plot_utility_ridge_density_by_strategy(env, actor_ids, wave_ids, thin_factor, thin_wave_factor, show_utility_points, show_strategy_means, return_plot=TRUE, plot_file=plot_file )
  #  SET plots
  env$multiwave_plots <- if(append_plot) { append(env$multiwave_plots, plist) } else { plist }

  if(return_plot)
    return(plist)
}


#' @export
saomnk_search_rsiena_multiwave_plot_utility_ridge_density_by_strategy <- function(env,
                                                                                   actor_ids = c(),
                                                                                   wave_ids = c(),
                                                                                   thin_factor = 1,
                                                                                   thin_wave_factor = 1,
                                                                                   show_utility_points = TRUE,
                                                                                   show_strategy_means = TRUE,
                                                                                   scale_utility = TRUE,
                                                                                   return_plot = TRUE,
                                                                                   plot_file = NA,
                                                                                   plot_dir = NA,
                                                                                   plot_periods = 4) {
  actor_strat <- env$get_actor_strategies()
  nstep <- sum(!env$chain_stats$stability)
  coveffs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
  covparams <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
  covfixs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
  structeffs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$effect)
  structparams <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
  structfixs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
  covDvTypes <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$interaction1)
  componentDV_ids <- grep('self\\$component_\\d{1,2}_coCovar', covDvTypes)
  stratDV_ids     <- grep('self\\$strat_\\d{1,2}_coCovar', covDvTypes )
  compoeffs   <- coveffs[ componentDV_ids ]
  compoparams <- covparams[ componentDV_ids ]
  compofixs   <- covfixs[ componentDV_ids ]
  strateffs   <- coveffs[ stratDV_ids ]
  stratparams <- covparams[ stratDV_ids ]
  stratfixs   <- covfixs[ stratDV_ids ]
  actor_component_period <- env$M * env$N
  density_ridges_rel_min_height = 1e-07
  ## Compare 2 actors utilty
  dat <- env$actor_wave_util %>%
    filter(chain_step_id %% thin_factor == 0) %>%
    filter(wave_id %% thin_wave_factor == 0 ) %>%
    mutate(
      strategy = actor_strat[ actor_id ],
      chain_below_med =  chain_step_id < median(chain_step_id),
      actor_component_period = 1 + floor( chain_step_id / actor_component_period )
    ) %>%
    mutate(
      stabilization_summary_period = ifelse(actor_component_period <= (plot_periods - 1),
                                            actor_component_period,
                                            sprintf('%s+\n(%s-%s)',plot_periods,plot_periods,max(actor_component_period)))
    )
  util_lab <- 'Actor Utility'
  if(scale_utility) {
    util_sc <- scale(dat$utility)
    util_lab <- sprintf('Actor Utility\n(Standardized Center = %.2f; Scale = %.2f)',
                        attr(util_sc, 'scaled:center'),
                        attr(util_sc, 'scaled:scale'))
    if (!all(dat$utility == 0))
      dat <- dat %>% mutate(utility = c(scale(utility)))
  }
  density_rng <- range(dat$utility, na.rm=TRUE)
  density_absdiff_scale <- abs(diff(density_rng)) * 0.15
  util_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
  point_size <- 10 / log( nstep )
  point_alpha <- min( 1,  1/log10( nstep ) )
  if(length(actor_ids))
    dat <- dat %>% filter(actor_id %in% actor_ids)
  if(length(wave_ids))
    dat <- dat %>% filter(wave_id %in% wave_ids)
  dat_acp_stabil_means <- dat %>% group_by(stabilization_summary_period, strategy) %>%
    dplyr::summarize(mean=mean(utility, na.rm=TRUE)) %>%
    mutate(PeriodFct = forcats::fct_rev(as.factor(stabilization_summary_period)))
  ##==============================================
  strat_legend_title <- sprintf("Strategy (%s) :  ", paste(strateffs, collapse = '_'))
  strat_break <- levels(actor_strat)
  strat_labs <- sapply(1:length(levels(actor_strat)), function(i) {
    a <- levels(actor_strat)[i]
    names(a) <- a
    return(a)
  })
  dat_dens_rigde <- dat %>%
    mutate(PeriodFct = forcats::fct_rev(as.factor(stabilization_summary_period)))
  group_dens_means <- dat_dens_rigde %>% ungroup() %>% group_by(strategy) %>%
    dplyr::summarize(n=n(),mean=mean(utility,na.rm=TRUE))
  ##---------------------
  ## Start Plot
  plt.dr <- ggplot(dat_dens_rigde, aes(y = PeriodFct, x = utility, color=strategy, fill=strategy)) +
    ggridges::stat_density_ridges(aes(point_color = strategy, point_fill = strategy, point_shape = strategy),
                        quantile_lines = TRUE, alpha = .3, rel_min_height = density_ridges_rel_min_height,
                        point_size=.4,
                        jittered_points = TRUE,
                        position = ggridges::position_raincloud(adjust_vlines = FALSE, ygap = -.1, height = .15),
                        quantiles = c(0.5), linewidth=.75 ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    ggridges::scale_fill_cyclical(
      breaks = strat_break,
      labels = strat_labs,
      values = scales::hue_pal()(length(levels(actor_strat))),
      guide = "legend"
    ) +
    labs(
      x = util_lab,
      y = sprintf(" Time Period \n(Actor-Component-Period = %s decision steps)", actor_component_period ),
      title = "Actor Utility Stabilization Paths",
      subtitle = sprintf("(Decision Chain Iterations: %s )", nstep),
      color = strat_legend_title,
      fill = strat_legend_title,
      point_color = strat_legend_title,
      point_fill = strat_legend_title,
      point_shape = strat_legend_title
    ) +
    geom_vline(xintercept = 0, linetype=1) +
    coord_cartesian(clip = "off") +
    ggridges::theme_ridges(grid = TRUE, center=TRUE) +
    theme(legend.position = 'bottom')
  if(show_strategy_means) {
    plt.dr <- plt.dr +
      geom_vline(data = group_dens_means,  aes(xintercept = mean, color=strategy),
                 linetype=3, linewidth=1.3) +
      geom_text(data = group_dens_means,
                aes(x = mean, y = 1+length(unique(dat_dens_rigde$stabilization_summary_period)), label = round(mean, 2), color=strategy),
                inherit.aes = FALSE, size = 5, nudge_x=-.1, nudge_y=.35  )
  }

  if(return_plot)
    return(plt.dr)
}


#' @export
saomnk_search_rsiena_multiwave_plot_K_4panel <- function(env,
                                                          actor_ids = c(),
                                                          component_ids = c(),
                                                          wave_ids = c(),
                                                          thin_factor = 1,
                                                          thin_wave_factor = 1,
                                                          smooth_method = 'loess',
                                                          show_utility_points = TRUE,
                                                          return_plot = TRUE,
                                                          plot_file = NA, plot_dir = NA) {
  K_AA  <- saomnk_search_rsiena_multiwave_plot_K_AA_strategy_summary(env, actor_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, show_legend=TRUE, show_title=FALSE, return_plot=TRUE)
  K_AC <- saomnk_search_rsiena_multiwave_plot_K_AC_strategy_summary(env, actor_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, show_legend=FALSE, show_title=FALSE, return_plot=TRUE)
  K_CA <- saomnk_search_rsiena_multiwave_plot_K_CA_strategy_summary(env, component_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, show_legend=FALSE, show_title=FALSE, return_plot=TRUE)
  K_CC  <- saomnk_search_rsiena_multiwave_plot_K_CC_strategy_summary(env, component_ids, wave_ids, thin_factor, thin_wave_factor, smooth_method, show_utility_points, show_legend=TRUE, show_title=FALSE, return_plot=TRUE)
  strateffs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
  stratparams <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
  stratfixs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
  structeffs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$effect)
  structparams <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
  structfixs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
  covDvTypes <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$interaction1)
  componentDV_ids <- grep('self\\$component_\\d{1,2}_coCovar', covDvTypes)
  stratDV_ids     <- grep('self\\$strat_\\d{1,2}_coCovar', covDvTypes )

  maintitle <- sprintf('Environment: Actors (M) = %s, Components (N) = %s, Init.Prob. = %.2f\nActor Strategy:  %s\nComponent Payoff:  %s\nStructure:  %s',
                       env$M, env$N, env$BI_PROB,
                       paste( paste(paste(strateffs[stratDV_ids], stratparams[stratDV_ids], sep='= '), stratfixs[stratDV_ids], sep='' ), collapse = ';  '),
                       paste( paste(paste(strateffs[componentDV_ids], stratparams[componentDV_ids], sep='= '), stratfixs[componentDV_ids], sep=''), collapse = ';  '),
                       paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
  )

  combined_plot_notitle <- ggarrange(
    K_AC, K_CA,
    K_AA, K_CC,
    nrow = 2, ncol = 2,
    common.legend = TRUE,
    legend = "bottom"
  )

  combined_plot <- ggpubr::annotate_figure(
    combined_plot_notitle,
    top = ggpubr::text_grob(maintitle, color = "black", size = 14)
  )

  env$multiwave_plots <- list(combined_plot=combined_plot)

  if(!is.na(plot_file))
    ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", env$config_environ_params$name, plot_file)),
           combined_plot,
           width = 10, height = 8, units = 'in', dpi = 600)

  if(return_plot)
    return(combined_plot)
}


#' @export
saomnk_search_rsiena_multiwave_plot_K_CC_strategy_summary <- function(env,
                                                                       component_ids = c(),
                                                                       wave_ids = c(),
                                                                       thin_factor = 1,
                                                                       thin_wave_factor = 1,
                                                                       smooth_method = 'loess',
                                                                       show_utility_points = TRUE,
                                                                       show_legend = TRUE,
                                                                       show_title = TRUE,
                                                                       return_plot = TRUE,
                                                                       plot_file = NA, plot_dir = NA) {
  ## actor strategy
  if ( !identical(attr(env$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
    stop("Actor Strategy env$strat_1_coCovar are not  set.")
  if ( !identical(attr(env$component_1_coCovar, 'nodeSet'), 'COMPONENTS') )
    stop("Component payoff values in env$component_1_coCovar are not set.")
  range_midpoint <- min(env$component_1_coCovar, na.rm=TRUE) + ( abs(diff(range(env$component_1_coCovar, na.rm = TRUE))) / 2 )
  component_types <- as.factor( ifelse(env$component_1_coCovar > range_midpoint, 'High', 'Low') )
  nstep <- sum(!env$chain_stats$stability)
  strateffs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
  stratparams <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
  stratfixs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
  structeffs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$effect)
  structparams <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
  structfixs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
  ## Compare 2 actors utilty
  dat <- env$K_wave_C %>%
    filter(chain_step_id %% thin_factor == 0) %>%
    filter(wave_id %% thin_wave_factor == 0 ) %>%
    mutate(
      component_type = component_types[ component_id ],
      chain_below_med =  chain_step_id < median(chain_step_id)
    )
  dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
  y_lab <- 'K_CC: Component Epistasis Degree'
  density_rng <- range(dat$value, na.rm=TRUE)
  density_absdiff_scale <- abs(diff(density_rng)) * 0.15
  y_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
  point_size <- 10 / log( nstep )
  point_alpha <- min( 1,  .5/log10( nstep ) )
  if(length(component_ids))
    dat <- dat %>% filter(component_id %in% component_ids)
  if(length(wave_ids))
    dat <- dat %>% filter(wave_id %in% wave_ids)
  dat_wave_means <- dat %>% group_by(wave_id) %>%
    dplyr::summarize(mean=mean(value, na.rm=TRUE))
  plt <- ggplot(dat, aes(x=chain_step_id, y=value)) +
    geom_hline(data=dat_wave_means, aes(yintercept=mean), linetype=3, col='black' ) +
    facet_grid(wave_id ~ .)
  if(show_utility_points)
    plt <- plt + geom_point(aes(color=component_id), alpha=point_alpha, shape=1, size=point_size, show.legend = FALSE)
  if(env$exists(smooth_method))
    plt <- plt + geom_smooth(aes(color=component_id, fill=component_id), method = smooth_method, linewidth=.5, alpha=.09, show.legend = FALSE, se=FALSE)
  plt <- plt + theme_bw() +
    ylim(y_lim) +
    ylab(y_lab) +
    xlab('Actor Decision Chain Ministep') +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  if (show_title)
    plt <- plt + ggtitle(sprintf('Strategy:  %s\nStructure:  %s',
                                 paste( paste(paste(strateffs, stratparams, sep='= '), stratfixs, sep='' ), collapse = ';  '),
                                 paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
    ))
  plt <- plt +  guides(color = guide_legend(nrow = 1))

  #### Density
  stratmeans <- dat %>% group_by(component_id, wave_id) %>%
    dplyr::summarize(n=n(), mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE))
  plt2 <- ggplot(dat, aes(x=value, color=component_id, fill=component_id)) +
    geom_density(alpha=.01, linewidth=.5, show.legend = FALSE)  +
    geom_vline(data = stratmeans, aes(xintercept = mean, color=component_id), linetype=2, linewidth=.5, show.legend = FALSE) +
    geom_vline(data = dat_wave_means,  aes(xintercept=mean), linetype=3, col='black' ) +
    labs(y='', x='') +
    xlim(y_lim) +
    coord_flip() +
    facet_grid(wave_id ~ .) +
    ylab('K_CC Density') +
    theme_bw() + theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = 'none',
      plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
      axis.text.y = element_blank(),
      axis.ticks.y=element_blank()
    )
  if (show_title)
    plt2 <- plt2 + ggtitle('\n\n\n')


  combined_plot <- ggarrange(
    plt, plt2,
    ncol = 2,
    widths = c(4.1,0.9),
    common.legend = TRUE,
    legend = ifelse(show_legend, "bottom", "none")
  )
  if(!is.na(plot_file))
    ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", env$config_environ_params$name, plot_file)),
           combined_plot,
           width = 10, height = 8, units = 'in', dpi = 600)
  if(return_plot)
    return(combined_plot)
}


#' @export
saomnk_search_rsiena_multiwave_plot_K_CA_strategy_summary <- function(env,
                                                                       component_ids = c(),
                                                                       wave_ids = c(),
                                                                       thin_factor = 1,
                                                                       thin_wave_factor = 1,
                                                                       smooth_method = 'loess',
                                                                       show_utility_points = TRUE,
                                                                       show_legend = TRUE,
                                                                       show_title = TRUE,
                                                                       return_plot = TRUE,
                                                                       plot_file = NA, plot_dir = NA) {
  ## actor strategy
  if ( !identical(attr(env$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
    stop("Actor Strategy env$strat_1_coCovar are not set.")
  if ( !identical(attr(env$component_1_coCovar, 'nodeSet'), 'COMPONENTS') )
    stop("Component payoff values in env$component_1_coCovar are not set.")
  range_midpoint <- min(env$component_1_coCovar, na.rm=TRUE) + ( abs(diff(range(env$component_1_coCovar, na.rm = TRUE))) / 2 )
  component_types <- as.factor( ifelse(env$component_1_coCovar > range_midpoint, 'High', 'Low') )
  nstep <- sum(!env$chain_stats$stability)
  strateffs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
  stratparams <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
  stratfixs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
  structeffs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$effect)
  structparams <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
  structfixs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
  ## Compare 2 actors utilty
  dat <- env$K_wave_B2 %>%
    filter(chain_step_id %% thin_factor == 0) %>%
    filter(wave_id %% thin_wave_factor == 0 ) %>%
    mutate(
      component_type = component_types[ component_id ],
      chain_below_med =  chain_step_id < median(chain_step_id)
    )
  dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
  y_lab <- 'K_CA: Component-Actor Degree'
  density_rng <- range(dat$value, na.rm=TRUE)
  density_absdiff_scale <- abs(diff(density_rng)) * 0.15
  y_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
  point_size <- 10 / log( nstep )
  point_alpha <- min( 1,  .5/log10( nstep ) )
  if(length(component_ids))
    dat <- dat %>% filter(component_id %in% component_ids)
  if(length(wave_ids))
    dat <- dat %>% filter(wave_id %in% wave_ids)
  dat_wave_means <- dat %>% group_by(wave_id) %>%
    dplyr::summarize(mean=mean(value, na.rm=TRUE))
  plt <- ggplot(dat, aes(x=chain_step_id, y=value)) +
    geom_hline(data=dat_wave_means, aes(yintercept=mean), linetype=3, col='black' ) +
    facet_grid(wave_id ~ .)
  if(show_utility_points)
    plt <- plt + geom_point(aes(color=component_id), alpha=point_alpha, shape=1, size=point_size, show.legend = FALSE)
  if(env$exists(smooth_method))
    plt <- plt + geom_smooth(aes(color=component_id, fill=component_id), method = smooth_method, linewidth=.5, alpha=.09, se=FALSE, show.legend = FALSE)
  plt <- plt + theme_bw() +
    ylim(y_lim) +
    ylab(y_lab) +
    xlab('Actor Decision Chain Ministep') +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.key.width = unit(0.8, "cm"),
      legend.spacing.x = unit(0.3, "cm")
    )
  if(show_title)
    plt <- plt + ggtitle(sprintf('Strategy:  %s\nStructure:  %s',
                                 paste( paste(paste(strateffs, stratparams, sep='= '), stratfixs, sep='' ), collapse = ';  '),
                                 paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
    ))
  plt <- plt +  guides(color = guide_legend(nrow = 1))

  #### Density
  stratmeans <- dat %>% group_by(component_id, wave_id) %>%
    dplyr::summarize(n=n(), mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE))
  plt2 <- ggplot(dat, aes(x=value, color=component_id, fill=component_id)) +
    geom_density(alpha=.01, linewidth=.5, show.legend = FALSE)  +
    geom_vline(data = stratmeans, aes(xintercept = mean, color=component_id), linetype=2, linewidth=.5, show.legend = FALSE) +
    geom_vline(data = dat_wave_means,  aes(xintercept=mean), linetype=3, col='black' ) +
    labs(y='', x='') +
    xlim(y_lim) +
    coord_flip() +
    facet_grid(wave_id ~ .) +
    ylab('K_CA Density') +
    theme_bw() + theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = 'none',
      plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
      axis.text.y = element_blank(),
      axis.ticks.y=element_blank()
    )
  if (show_title)
    plt2 <- plt2 + ggtitle('\n\n\n')


  combined_plot <- ggarrange(
    plt, plt2,
    ncol = 2,
    widths = c(4.1,0.9),
    common.legend = TRUE,
    legend = ifelse(show_legend, "bottom", "none")
  )
  if(!is.na(plot_file))
    ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", env$config_environ_params$name, plot_file)),
           combined_plot,
           width = 10, height = 8, units = 'in', dpi = 600)
  if(return_plot)
    return(combined_plot)
}


#' @export
saomnk_search_rsiena_multiwave_plot_K_AC_strategy_summary <- function(env,
                                                                       actor_ids = c(),
                                                                       wave_ids = c(),
                                                                       thin_factor = 1,
                                                                       thin_wave_factor = 1,
                                                                       smooth_method = 'loess',
                                                                       show_utility_points = TRUE,
                                                                       show_legend = TRUE,
                                                                       show_title = TRUE,
                                                                       return_plot = TRUE,
                                                                       plot_file = NA, plot_dir = NA) {
  ## actor strategy
  if ( !identical(attr(env$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
    stop("Actor Strategy env$strat_1_coCovar not set.")
  actor_strat <-  env$get_actor_strategies()
  nstep <- sum(!env$chain_stats$stability)
  strateffs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
  stratparams <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
  stratfixs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
  structeffs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$effect)
  structparams <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
  structfixs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
  ## Compare 2 actors utilty
  dat <- env$K_wave_B1 %>%
    filter(chain_step_id %% thin_factor == 0) %>%
    filter(wave_id %% thin_wave_factor == 0 ) %>%
    mutate(
      strategy = actor_strat[ actor_id ],
      chain_below_med =  chain_step_id < median(chain_step_id)
    )
  dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
  y_lab <- 'K_AC: Actor-Component Degree'
  density_rng <- range(dat$value, na.rm=TRUE)
  density_absdiff_scale <- abs(diff(density_rng)) * 0.15
  y_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
  point_size <- 10 / log( nstep )
  point_alpha <- min( 1,  1/log10( nstep ) )
  if(length(actor_ids))
    dat <- dat %>% filter(actor_id %in% actor_ids)
  if(length(wave_ids))
    dat <- dat %>% filter(wave_id %in% wave_ids)
  dat_wave_means <- dat %>% group_by(wave_id) %>%
    dplyr::summarize(mean=mean(value, na.rm=TRUE))
  plt <- ggplot(dat, aes(x=chain_step_id, y=value)) +
    geom_hline(data=dat_wave_means, aes(yintercept=mean), linetype=3, col='black' ) +
    facet_grid(wave_id ~ .)
  if(show_utility_points)
    plt <- plt + geom_point(aes(color=strategy), alpha=point_alpha, shape=1, size=point_size)
  if(env$exists(smooth_method))
    plt <- plt + geom_smooth(aes(linetype=actor_id, color=strategy, fill=strategy), method = smooth_method, linewidth=1, alpha=.09)
  plt <- plt + theme_bw() +
    ylim(y_lim) +
    ylab(y_lab) +
    xlab('Actor Decision Chain Ministep') +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.key.width = unit(0.8, "cm"),
      legend.spacing.x = unit(0.3, "cm")
    )
  if(show_title)
    plt <- plt + ggtitle(sprintf('Strategy:  %s\nStructure:  %s',
                                 paste( paste(paste(strateffs, stratparams, sep='= '), stratfixs, sep='' ), collapse = ';  '),
                                 paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
    ))
  plt <- plt +  guides(color = guide_legend(nrow = 1))

  #### Density
  stratmeans <- dat %>% group_by(strategy, wave_id) %>%
    dplyr::summarize(n=n(), mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE))
  plt2 <- ggplot(dat, aes(x=value, color=strategy, fill=strategy)) +
    geom_density(alpha=.1, linewidth=1)  +
    geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
    geom_vline(data = dat_wave_means,  aes(xintercept=mean), linetype=3, col='black' ) +
    labs(y='', x='') +
    xlim(y_lim) +
    coord_flip() +
    facet_grid(wave_id ~ .) +
    ylab('K_AC Density') +
    theme_bw() + theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = 'none',
      plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
      axis.text.y = element_blank(),
      axis.ticks.y=element_blank()
    )
  if (show_title)
    plt2 <- plt2 + ggtitle('\n\n\n')


  combined_plot <- ggarrange(
    plt, plt2,
    ncol = 2,
    widths = c(4.1,0.9),
    common.legend = TRUE,
    legend = ifelse(show_legend, "bottom", "none")
  )
  if(!is.na(plot_file))
    ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", env$config_environ_params$name, plot_file)),
           combined_plot,
           width = 10, height = 8, units = 'in', dpi = 600)
  if(return_plot)
    return(combined_plot)
}


#' @export
saomnk_search_rsiena_multiwave_plot_K_AA_strategy_summary <- function(env,
                                                                       actor_ids = c(),
                                                                       wave_ids = c(),
                                                                       thin_factor = 1,
                                                                       thin_wave_factor = 1,
                                                                       smooth_method = 'loess',
                                                                       show_utility_points = TRUE,
                                                                       show_legend = TRUE,
                                                                       show_title = TRUE,
                                                                       return_plot = TRUE,
                                                                       plot_file = NA, plot_dir = NA) {
  ## actor strategy
  if ( !identical(attr(env$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
    stop("Actor Strategy env$strat_1_coCovar not set.")
  actor_strat <-  env$get_actor_strategies()
  nstep <- sum(!env$chain_stats$stability)
  strateffs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
  stratparams <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
  stratfixs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
  structeffs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$effect)
  structparams <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
  structfixs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
  ## Compare 2 actors utilty
  dat <- env$K_wave_A %>%
    filter(chain_step_id %% thin_factor == 0) %>%
    filter(wave_id %% thin_wave_factor == 0 ) %>%
    mutate(
      strategy = actor_strat[ actor_id ],
      chain_below_med =  chain_step_id < median(chain_step_id)
    )
  dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
  y_lab <- 'K_AA: Social Degree'
  density_rng <- range(dat$value, na.rm=TRUE)
  density_absdiff_scale <- abs(diff(density_rng)) * 0.15
  y_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
  point_size <- 10 / log( nstep )
  point_alpha <- min( 1,  1/log10( nstep ) )
  if(length(actor_ids))
    dat <- dat %>% filter(actor_id %in% actor_ids)
  if(length(wave_ids))
    dat <- dat %>% filter(wave_id %in% wave_ids)
  dat_wave_means <- dat %>% group_by(wave_id) %>%
    dplyr::summarize(mean=mean(value, na.rm=TRUE))
  plt <- ggplot(dat, aes(x=chain_step_id, y=value)) +
    geom_hline(data=dat_wave_means, aes(yintercept=mean), linetype=3, col='black' ) +
    facet_grid(wave_id ~ .)
  if(show_utility_points)
    plt <- plt + geom_point(aes(color=strategy), alpha=point_alpha, shape=1, size=point_size)
  if(env$exists(smooth_method))
    plt <- plt + geom_smooth(aes(linetype=actor_id, color=strategy, fill=strategy), method = smooth_method, linewidth=1, alpha=.09)
  plt <- plt + theme_bw() +
    ylim(y_lim) +
    ylab(y_lab) +
    xlab('Actor Decision Chain Ministep') +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.key.width = unit(0.8, "cm"),
      legend.spacing.x = unit(0.3, "cm")
    )
  if(show_title)
    plt <- plt + ggtitle(sprintf('Strategy:  %s\nStructure:  %s',
                                 paste( paste(paste(strateffs, stratparams, sep='= '), stratfixs, sep='' ), collapse = ';  '),
                                 paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
    ))
  plt <- plt +  guides(color = guide_legend(nrow = 1))

  #### Density
  stratmeans <- dat %>% group_by(strategy, wave_id) %>%
    dplyr::summarize(n=n(), mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE))
  plt2 <- ggplot(dat, aes(x=value, color=strategy, fill=strategy)) +
    geom_density(alpha=.1, linewidth=1)  +
    geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
    geom_vline(data = dat_wave_means,  aes(xintercept=mean), linetype=3, col='black' ) +
    labs(y='', x='') +
    xlim(y_lim) +
    coord_flip() +
    facet_grid(wave_id ~ .) +
    ylab('K_AA Density') +
    theme_bw() + theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = 'none',
      plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
      axis.text.y = element_blank(),
      axis.ticks.y=element_blank()
    )
  if (show_title)
    plt2 <- plt2 + ggtitle('\n\n\n')


  combined_plot <- ggarrange(
    plt, plt2,
    ncol = 2,
    widths = c(4.1,0.9),
    common.legend = TRUE,
    legend = ifelse(show_legend, "bottom", "none")
  )
  if(!is.na(plot_file))
    ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", env$config_environ_params$name, plot_file)),
           combined_plot,
           width = 10, height = 8, units = 'in', dpi = 600)
  if(return_plot)
    return(combined_plot)
}


#' @export
saomnk_search_rsiena_multiwave_plot_actor_utility_strategy_summary <- function(env,
                                                                                actor_ids = c(),
                                                                                wave_ids = c(),
                                                                                thin_factor = 1,
                                                                                thin_wave_factor = 1,
                                                                                smooth_method = 'loess',
                                                                                show_utility_points = TRUE,
                                                                                scale_utility = TRUE,
                                                                                return_plot = TRUE,
                                                                                plot_file = NA, plot_dir = NA,
                                                                                loess_span = 0.4) {
  ## actor strategy
  if ( !identical(attr(env$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
    stop("Actor Strategy env$strat_1_coCovar not set.")
  actor_strat <- env$get_actor_strategies()
  nstep <- sum(!env$chain_stats$stability)
  strateffs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
  stratparams <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
  stratfixs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
  structeffs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$effect)
  structparams <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
  structfixs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
  covDvTypes <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$interaction1)
  componentDV_ids <- grep('self\\$component_\\d{1,2}_coCovar', covDvTypes)
  stratDV_ids     <- grep('self\\$strat_\\d{1,2}_coCovar', covDvTypes )
  ## Compare 2 actors utilty
  dat <- env$actor_wave_util %>%
    filter(chain_step_id %% thin_factor == 0) %>%
    filter(wave_id %% thin_wave_factor == 0 ) %>%
    mutate(
      strategy = actor_strat[ actor_id ],
      chain_below_med =  chain_step_id < median(chain_step_id)
    )
  dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
  util_lab <- 'Actor Utility'
  if(scale_utility) {
    util_sc <- scale(dat$utility)
    util_lab <- sprintf('Actor Utility\n(Standardized Center = %.2f; Scale = %.2f)',
                        attr(util_sc, 'scaled:center'),
                        attr(util_sc, 'scaled:scale'))
    if (!all(dat$utility == 0))
      dat <- dat %>% mutate(utility = c(scale(utility)))
  }
  density_rng <- range(dat$utility, na.rm=TRUE)
  density_absdiff_scale <- abs(diff(density_rng)) * 0.15
  util_lim <- c(density_rng[1] - density_absdiff_scale,  density_rng[2] + density_absdiff_scale)
  point_size <- 10 / log( nstep )
  point_alpha <- min( 1,  1/log10( nstep ) )
  if(length(actor_ids))
    dat <- dat %>% filter(actor_id %in% actor_ids)
  if(length(wave_ids))
    dat <- dat %>% filter(wave_id %in% wave_ids)
  dat_wave_means <- dat %>% group_by(wave_id) %>%
    dplyr::summarize(mean=mean(utility, na.rm=TRUE))
  plt <- ggplot(dat, aes(x=chain_step_id, y=utility)) +
    geom_hline(data=dat_wave_means, aes(yintercept=mean), linetype=3, col='black' ) +
    facet_grid(wave_id ~ .)
  if(show_utility_points)
    plt <- plt + geom_point(aes(color=strategy), alpha=point_alpha, shape=1, size=point_size)
  if(env$exists(smooth_method))
    plt <- plt + geom_smooth(aes(color=strategy, fill=strategy, shape=actor_id),
                             method = smooth_method, span=loess_span, linewidth=1, alpha=.09)
  # Population Mean in black
  plt <- plt + geom_smooth(aes(x=chain_step_id, y=utility_mean), method = smooth_method, span=loess_span,
                           data=dat %>% group_by(chain_step_id)%>%dplyr::summarize(utility_mean=mean(utility)),
                           color='black', linetype=1, alpha=.1)
  plt <- plt + theme_bw() +
    ylim(util_lim) +
    ylab(util_lab) +
    xlab('Actor Decision Chain Ministep') +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.key.width = unit(0.8, "cm"),
      legend.spacing.x = unit(0.3, "cm")
    )

  ACTORS     <- sienaNodeSet(env$M, nodeSetName="ACTORS")
  COMPONENTS <- sienaNodeSet(env$N, nodeSetName="COMPONENTS")
  if (! 'coCovars' %in% names(env$config_structure_model$dv_bipartite) ) {
    rsiena_data <- sienaDataCreate(list(env$bipartite_rsienaDV), nodeSets = list(ACTORS, COMPONENTS))
    return(rsiena_data)
  }

  plt <- plt + ggtitle(sprintf('Environment: Actors (M) = %s, Components (N) = %s, Init.Prob. = %.2f\nActor Strategy:  %s\nComponent Payoff:  %s\nStructure:  %s',
                               env$M, env$N, env$BI_PROB,
                               paste( paste(paste(strateffs[stratDV_ids], stratparams[stratDV_ids], sep='= '), stratfixs[stratDV_ids], sep='' ), collapse = ';  '),
                               paste( paste(paste(strateffs[componentDV_ids], stratparams[componentDV_ids], sep='= '), stratfixs[componentDV_ids], sep=''), collapse = ';  '),
                               paste( paste(paste(structeffs, structparams, sep='= '), structfixs, sep=''), collapse = ';  ')
  ))
  plt <- plt +  guides(color = guide_legend(nrow = 1))

  #### Density
  stratmeans <- dat %>% group_by(strategy, wave_id) %>%
    dplyr::summarize(n=n(), mean=mean(utility, na.rm=TRUE), sd=sd(utility, na.rm=TRUE))
  ## Actor density fact plots comparing H1 to H2 utility distribution
  plt2 <- ggplot(dat, aes(x=utility, color=strategy, fill=strategy)) +
    geom_density(alpha=.1, linewidth=1)  +
    geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
    geom_vline(data = dat_wave_means,  aes(xintercept=mean), linetype=3, col='black' ) +
    labs(y='', x='') +
    xlim(util_lim) +
    coord_flip() +
    facet_grid(wave_id ~ .) +
    ylab('Actor Utility Density') +
    theme_bw() + theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = 'none',
      plot.margin=unit(c(5.5, 5.5, 5.5, -23), 'pt'),
      axis.text.y = element_blank(),
      axis.ticks.y=element_blank()
      ) + ggtitle('\n\n\n')


  combined_plot <- ggarrange(
    plt, plt2,
    ncol = 2,
    widths = c(4.1,0.9),
    common.legend = TRUE,
    legend = "bottom"
  )
  if(!is.na(plot_file))
    ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", env$config_environ_params$name, plot_file)),
           combined_plot,
           width = 10, height = 8, units = 'in', dpi = 600)

  if(return_plot)
    return(combined_plot)
}


#' @export
saomnk_search_rsiena_multiwave_plot_actor_utility_by_strategy <- function(env,
                                                                           actor_ids = c(),
                                                                           thin_factor = 1,
                                                                           thin_wave_factor = 1,
                                                                           smooth_method = 'loess',
                                                                           show_utility_points = TRUE,
                                                                           return_plot = TRUE,
                                                                           plot_file = NA, plot_dir = NA) {
  ## actor strategy
  if ( !identical(attr(env$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
    stop("Actor Strategy env$strat_1_coCovar not set.")
  actor_strat <- env$get_actor_strategies()
  ## Compare 2 actors utilty
  dat <- env$actor_wave_util %>%
    filter(chain_step_id %% thin_factor == 0) %>%
    filter(wave_id %% thin_wave_factor == 0 ) %>%
    mutate(strategy = actor_strat[ actor_id ] )
  if(length(actor_ids))
    dat <- dat %>% filter(actor_id %in% actor_ids)
  plt <- ggplot(dat, aes(x=chain_step_id, y=utility)) +
    geom_hline(data=dat%>%group_by(wave_id)%>%dplyr::summarize(mean=mean(utility, na.rm=TRUE)), aes(yintercept=mean), linetype=2, col='black' ) +
    facet_wrap( ~ wave_id)
  if(show_utility_points)
    plt <- plt + geom_point(aes(color=strategy), alpha=.25, shape=1, size=2)
  if(env$exists(smooth_method))
    plt <- plt + geom_smooth(aes(linetype=actor_id, color=strategy), method = smooth_method, linewidth=1, alpha=.15)
  plt <- plt + theme_bw()
  # Add marginal density plots
  plt <- ggExtra::ggMarginal(plt, type = "density", margins = "y")

  if(!is.na(plot_file))
    ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", env$config_environ_params$name, plot_file)),
           plt,
           width = 10, height = 8, units = 'in', dpi = 600)
  if(return_plot)
    return(plt)
}


#' @export
saomnk_search_rsiena_multiwave_plot_actor_utility_density_by_strategy <- function(env,
                                                                                   thin_wave_factor = 1,
                                                                                   return_plot = TRUE,
                                                                                   plot_file = NA,
                                                                                   plot_dir = NA) {
  ## actor strategy
  if ( !identical(attr(env$strat_1_coCovar, 'nodeSet'), 'ACTORS') )
    stop("Actor Strategy env$strat_1_coCovar not set.")
  actor_strat <-  env$get_actor_strategies()
  strateffs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$effect)
  stratparams <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)x$parameter)
  stratfixs   <- sapply(env$config_structure_model$dv_bipartite$coCovars, function(x)ifelse(x$fix,'','(var)'))
  structeffs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$effect)
  structparams <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)x$parameter)
  structfixs   <- sapply(env$config_structure_model$dv_bipartite$effects, function(x)ifelse(x$fix,'','(var)'))
  ## Compare 2 actors utilty
  dat <- env$actor_wave_util %>%
    filter(wave_id %% thin_wave_factor == 0 ) %>%
    mutate(
      strategy = actor_strat[ actor_id ] ,
      chain_below_med =  chain_step_id < median(chain_step_id)
    )
  dat$chain_half <- factor(ifelse(dat$chain_below_med, '1st Half', '2nd Half'))
  stratmeans <- dat %>% group_by(strategy, chain_half, wave_id) %>%
    dplyr::summarize(n=n(), mean=mean(utility, na.rm=TRUE), sd=sd(utility, na.rm=TRUE))
  ## Actor density fact plots comparing H1 to H2 utility distribution
  plt <- ggplot(dat, aes(x=utility, color=strategy, fill=strategy)) +
    geom_density(alpha=.1, linewidth=1)  +
    facet_grid( wave_id ~ chain_half) +
    geom_vline(data = stratmeans, aes(xintercept = mean, color=strategy), linetype=2, linewidth=.9) +
    theme_bw() +
    ggtitle(sprintf('Strategy: %s\nStructure: %s',
                    paste( paste(paste(strateffs, stratparams, sep='='), stratfixs, sep='' ), collapse = '; '),
                    paste( paste(paste(structeffs, structparams, sep='='), structfixs, sep=''), collapse = '; ')
                    ))

  if(!is.na(plot_file))
    ggsave(filename = file.path(ifelse(is.na(plot_dir),getwd(),plot_dir), sprintf("%s_%s.png", env$config_environ_params$name, plot_file)),
           plt,
           width = 10, height = 8, units = 'in', dpi = 600)

  if(return_plot)
    return(plt)
}
