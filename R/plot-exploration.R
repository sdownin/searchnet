# plot-exploration.R
# Standalone exploration/exploitation plot functions extracted from SaoMNK R6 class.
# Each function takes `env` (an SaoMNK environment/instance) as its first argument.
# All self$ references have been replaced with env$.


# ---- plot_exploration_exploitation_consistent --------------------------------

#' @export
saomnk_plot_exploration_exploitation_consistent <- function(
    env,
    actor_ids = c(),
    thin_factor = 1,
    thin_pct = 1,
    smooth_method = 'loess',
    show_points = TRUE,
    show_individuals = FALSE,
    show_group_means = TRUE,
    loess_span = 0.3,
    point_alpha_dimmer = 1,
    line_alpha = 0.5,
    group_line_size = 2,
    se_ribbon = TRUE,
    ylim = c(0, 1),
    plot_return = TRUE,
    plot_save = FALSE,
    plot_file = '',
    plot_dir = NA
) {

  # Get actor strategies - matching the utility plot logic
  if (attr(env$strat_1_coCovar, 'nodeSet') != 'ACTORS')
    stop("Actor Strategy env$strat_1_coCovar not set.")

  actor_strat <- env$get_actor_strategies()

  # Get bipartite array data
  bi_env_arr <- env$bi_env_arr
  n_steps <- dim(bi_env_arr)[3]
  n_actors <- env$M
  n_components <- env$N

  # Define old (exploitation) and new (exploration) components
  old_components <- 1:8
  new_components <- 9:16

  # Get unique chain steps
  unique_steps <- sort(unique(env$chain_stats$chain_step_id))

  # Calculate exploration/exploitation metrics
  metrics_list <- list()

  for (t_idx in 1:min(n_steps, length(unique_steps))) {
    incidence_t <- bi_env_arr[, , t_idx]

    for (i in 1:n_actors) {
      actor_activities <- which(incidence_t[i, ] > 0)

      if (length(actor_activities) > 0) {
        n_old <- sum(actor_activities %in% old_components)
        n_new <- sum(actor_activities %in% new_components)
        n_total <- length(actor_activities)

        metrics_list[[length(metrics_list) + 1]] <- data.frame(
          chain_step_id = unique_steps[t_idx],
          actor_id = i,
          n_total_activities = n_total,
          n_old_activities = n_old,
          n_new_activities = n_new,
          prop_exploration = n_new / n_total,
          prop_exploitation = n_old / n_total
        )
      }
    }
  }

  # Combine metrics and add strategy - matching utility plot logic
  metrics_df <- bind_rows(metrics_list) %>%
    mutate(
      strategy = actor_strat[actor_id],
      strategy = factor(strategy)
    )

  # Apply thinning - matching utility plot
  if (thin_factor > 1) {
    metrics_df <- metrics_df %>%
      filter(chain_step_id %% thin_factor == 0)
  }

  if (thin_pct < 1) {
    sample_rows <- sample(1:nrow(metrics_df), size = round(nrow(metrics_df) * thin_pct), replace = FALSE)
    metrics_df <- metrics_df[sample_rows, ]
  }

  # Filter actors if specified
  if (length(actor_ids) > 0) {
    metrics_df <- metrics_df %>%
      filter(actor_id %in% actor_ids)
  }

  # Calculate plot parameters matching utility plot
  npoints <- nrow(metrics_df)
  point_size <- 6 / log10(npoints)
  point_alpha <- min(1, 2.2/log(npoints)) * point_alpha_dimmer

  # Reshape to long format
  plot_data <- metrics_df %>%
    pivot_longer(
      cols = c(prop_exploration, prop_exploitation),
      names_to = "activity_type",
      values_to = "proportion"
    ) %>%
    mutate(
      activity_type = case_when(
        activity_type == "prop_exploration" ~ "Exploration",
        activity_type == "prop_exploitation" ~ "Exploitation"
      ),
      activity_label = factor(activity_type, levels = c("Exploitation", "Exploration")),
      actor_id_factor = factor(actor_id)
    )

  # Calculate group means by strategy and activity type
  group_means <- plot_data %>%
    group_by(chain_step_id, strategy, activity_type, activity_label) %>%
    summarise(
      mean_proportion = mean(proportion, na.rm = TRUE),
      se_proportion = sd(proportion, na.rm = TRUE) / sqrt(n()),
      n_obs = n(),
      .groups = "drop"
    )

  # Create main plot
  plt <- ggplot()

  # Add individual points if requested - color by strategy (matching utility plot)
  if (show_points) {
    plt <- plt + geom_point(
      data = plot_data,
      aes(x = chain_step_id,
          y = proportion,
          color = strategy),
      alpha = point_alpha,
      shape = 1,
      size = point_size
    )
  }

  # Add individual lines if requested
  if (show_individuals) {
    plt <- plt + geom_line(
      data = plot_data,
      aes(x = chain_step_id,
          y = proportion,
          color = strategy,
          linetype = activity_label,
          group = interaction(actor_id, activity_type)),
      alpha = line_alpha * 0.3,
      size = 0.5
    )
  }

  # Add group mean lines
  if (show_group_means) {
    # Add confidence ribbons
    if (se_ribbon) {
      plt <- plt + geom_ribbon(
        data = group_means,
        aes(x = chain_step_id,
            ymin = pmax(0, mean_proportion - se_proportion),
            ymax = pmin(1, mean_proportion + se_proportion),
            fill = strategy,
            group = interaction(strategy, activity_label)),
        alpha = 0.15
      )
    }

    # Add mean lines - color by strategy, linetype by activity
    plt <- plt + geom_line(
      data = group_means,
      aes(x = chain_step_id,
          y = mean_proportion,
          color = strategy,
          linetype = activity_label,
          group = interaction(strategy, activity_label)),
      size = group_line_size
    )

    # Add smoothed trends
    if (!is.null(smooth_method) && env$exists(smooth_method)) {
      plt <- plt + geom_smooth(
        data = group_means,
        aes(x = chain_step_id,
            y = mean_proportion,
            color = strategy,
            linetype = activity_label,
            group = interaction(strategy, activity_label)),
        method = smooth_method,
        span = loess_span,
        se = FALSE,
        size = 0.8,
        alpha = 0.6
      )
    }
  }

  # Add population mean (black line) - similar to utility plot
  pop_mean <- plot_data %>%
    group_by(chain_step_id, activity_type) %>%
    summarise(
      pop_mean_prop = mean(proportion, na.rm = TRUE),
      .groups = "drop"
    )

  plt <- plt + geom_smooth(
    data = pop_mean,
    aes(x = chain_step_id, y = pop_mean_prop, linetype = activity_type),
    method = smooth_method,
    span = loess_span,
    color = 'black',
    alpha = 0.1,
    size = 1
  )

  # Add shock rectangles if they exist (matching utility plot)
  if (!is.null(env$theta_shocks)) {
    suppressMessages({
      layout <- ggplot_build(plt)$layout
    })
    shock_rects <- env$get_theta_shock_rects_df(env$theta_shocks) %>%
      mutate(proportion = 0, chain_step_id = 0)

    y_maxs <- rep(ylim[2] * 0.95, nrow(shock_rects))

    plt <- plt +
      geom_rect(
        data = shock_rects,
        aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
        fill = 'darkorange',
        color = 'orange',
        linetype = 2,
        alpha = 0.05
      ) +
      geom_text(
        data = shock_rects,
        aes(x = (start + end) / 2, y = y_maxs, label = label),
        vjust = 0,
        size = 3
      )
  }

  # Get parameter string for title (matching utility plot)
  params <- env$get_structure_model_params()
  sim_title_str <- env$get_structure_model_param_str(params)

  # Customize appearance
  plt <- plt +
    scale_linetype_manual(
      name = "Activity Type",
      values = c("Exploitation" = "solid",
                 "Exploration" = "dashed"),
      drop = FALSE
    ) +
    labs(
      title = sim_title_str,
      subtitle = sprintf("Exploration (New Activities: C%d-C%d) vs Exploitation (Old Activities: C%d-C%d)",
                         min(new_components), max(new_components),
                         min(old_components), max(old_components)),
      x = "Simulation Step",
      y = "Proportion of Activities"
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.key.width = unit(0.8, "cm"),
      legend.spacing.x = unit(0.3, "cm")
    ) +
    coord_cartesian(ylim = ylim) +
    scale_y_continuous(labels = scales::percent) +
    guides(color = guide_legend(nrow = 1))

  # For side density plot (optional, matching utility plot style)
  if (plot_return) {
    # Calculate strategy means
    stratmeans <- plot_data %>%
      group_by(strategy, activity_type) %>%
      summarise(
        n = n(),
        mean = mean(proportion, na.rm = TRUE),
        sd = sd(proportion, na.rm = TRUE),
        .groups = "drop"
      )

    # Create density plot
    nrows_title <- stringr::str_count(sim_title_str, "\\\n")

    plt2 <- ggplot(plot_data, aes(x = proportion, color = strategy, fill = strategy)) +
      geom_density(alpha = 0.1, linewidth = 1) +
      geom_vline(
        data = stratmeans,
        aes(xintercept = mean, color = strategy),
        linetype = 2,
        linewidth = 0.9
      ) +
      facet_wrap(~activity_type, ncol = 1) +
      labs(y = '', x = '') +
      xlim(ylim) +
      coord_flip() +
      ylab('Proportion Density') +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = 'none',
        plot.margin = unit(c(5.5, 5.5, 5.5, -23), 'pt'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      ggtitle(paste(rep('\n', nrows_title), collapse = ''))

    # Combine plots
    suppressMessages({
      combined_plot <- ggarrange(
        plt, plt2,
        ncol = 2,
        widths = c(4.1, 0.9),
        common.legend = TRUE,
        legend = "bottom"
      )
    })

    # Save if requested
    if (plot_save) {
      plot_file <- paste0('explore_exploit_consistent_', plot_file, round(as.numeric(Sys.time()) * 10))
      ggsave(
        file = file.path(
          ifelse(is.na(plot_dir) || plot_dir == '', getwd(), plot_dir),
          sprintf("%s_%s.jpeg", env$config_environ_params$name, plot_file)
        ),
        combined_plot,
        width = 10,
        height = 7,
        units = 'in',
        dpi = 400
      )
    }

    return(combined_plot)
  }

  return(plt)
}


# ---- plot_exploration_exploitation -------------------------------------------

#' @export
saomnk_plot_exploration_exploitation <- function(
    env,
    actor_ids = c(),
    thin_factor = 1,
    thin_pct = 1,
    smooth_method = 'loess',
    show_points = TRUE,
    show_individuals = TRUE,
    show_group_means = TRUE,
    loess_span = 0.3,
    point_alpha_dimmer = 1,
    line_alpha = 0.5,
    group_line_size = 2,
    ylim = NULL,
    plot_return = TRUE,
    plot_save = FALSE,
    plot_file = '',
    plot_dir = NA
) {

  # Get bipartite network data from chain
  bi_env_arr <- env$bi_env_arr
  n_steps <- dim(bi_env_arr)[3]
  n_actors <- env$M
  n_components <- env$N

  # Define old (initial) and new components
  old_components <- 1:8
  new_components <- 9:16

  # Get unique chain steps
  unique_steps <- sort(unique(env$chain_stats$chain_step_id))

  # Calculate exploration/exploitation metrics for each actor at each step
  metrics_list <- list()

  for (t_idx in 1:min(n_steps, length(unique_steps))) {
    if (t_idx > dim(bi_env_arr)[3]) break

    incidence_t <- bi_env_arr[, , t_idx]

    for (i in 1:n_actors) {
      actor_activities <- which(incidence_t[i, ] > 0)

      if (length(actor_activities) > 0) {
        n_old <- sum(actor_activities %in% old_components)
        n_new <- sum(actor_activities %in% new_components)
        n_total <- length(actor_activities)

        metrics_list[[length(metrics_list) + 1]] <- data.frame(
          chain_step_id = unique_steps[t_idx],
          actor_id = as.character(i),
          n_total_activities = n_total,
          n_old_activities = n_old,
          n_new_activities = n_new,
          prop_exploration = n_new / n_total,
          prop_exploitation = n_old / n_total,
          exploration_score = n_new / n_total,
          exploitation_score = n_old / n_total
        )
      }
    }
  }

  # Combine metrics
  metrics_df <- bind_rows(metrics_list)

  # Add actor strategies
  if (exists('strat_1_coCovar', where = env)) {
    actor_strat <- env$get_actor_strategies()
    metrics_df$strategy <- actor_strat[as.numeric(metrics_df$actor_id)]
  } else {
    metrics_df$strategy <- "0"
  }

  # Filter actors if specified
  if (length(actor_ids) > 0) {
    metrics_df <- metrics_df %>% filter(actor_id %in% actor_ids)
  }

  # Apply thinning
  metrics_df <- metrics_df %>%
    filter(chain_step_id %% thin_factor == 0)

  if (thin_pct < 1) {
    sample_rows <- sample(1:nrow(metrics_df), size = round(nrow(metrics_df) * thin_pct), replace = FALSE)
    metrics_df <- metrics_df[sample_rows, ]
  }

  # Reshape to long format for plotting
  plot_data <- metrics_df %>%
    pivot_longer(
      cols = c(exploration_score, exploitation_score),
      names_to = "behavior_type",
      values_to = "score"
    ) %>%
    mutate(
      behavior_type = case_when(
        behavior_type == "exploration_score" ~ "Exploration",
        behavior_type == "exploitation_score" ~ "Exploitation"
      ),
      strategy_label = paste("Strategy", strategy)
    )

  # Calculate group means
  group_means <- plot_data %>%
    group_by(chain_step_id, strategy, strategy_label, behavior_type) %>%
    summarise(
      mean_score = mean(score, na.rm = TRUE),
      se_score = sd(score, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # Create plot
  p <- ggplot()

  # Add individual points if requested
  if (show_points) {
    p <- p + geom_point(
      data = plot_data,
      aes(x = chain_step_id, y = score, color = behavior_type),
      alpha = point_alpha_dimmer * 0.3,
      size = 1
    )
  }

  # Add individual lines if requested
  if (show_individuals) {
    p <- p + geom_line(
      data = plot_data,
      aes(x = chain_step_id, y = score,
          color = behavior_type,
          group = interaction(actor_id, behavior_type)),
      alpha = line_alpha * 0.3,
      size = 0.5
    )
  }

  # Add group mean lines
  if (show_group_means) {
    # Add confidence ribbons
    p <- p + geom_ribbon(
      data = group_means,
      aes(x = chain_step_id,
          ymin = mean_score - se_score,
          ymax = mean_score + se_score,
          fill = behavior_type,
          group = interaction(strategy_label, behavior_type)),
      alpha = 0.15
    )

    # Add mean lines with different linetypes for strategies
    p <- p + geom_line(
      data = group_means,
      aes(x = chain_step_id,
          y = mean_score,
          color = behavior_type,
          linetype = strategy_label,
          group = interaction(strategy_label, behavior_type)),
      size = group_line_size
    )

    # Add smoothed trends if requested
    if (!is.null(smooth_method)) {
      p <- p + geom_smooth(
        data = group_means,
        aes(x = chain_step_id,
            y = mean_score,
            color = behavior_type,
            linetype = strategy_label,
            group = interaction(strategy_label, behavior_type)),
        method = smooth_method,
        span = loess_span,
        se = FALSE,
        size = 0.8,
        alpha = 0.6
      )
    }
  }

  # Add shock rectangles if they exist
  if (!is.null(env$theta_shocks)) {
    shock_rects <- env$get_theta_shock_rects_df(env$theta_shocks)
    p <- p +
      geom_rect(
        data = shock_rects,
        aes(xmin = start, xmax = end),
        ymin = -Inf, ymax = Inf,
        fill = 'darkorange',
        alpha = 0.05
      ) +
      geom_text(
        data = shock_rects,
        aes(x = (start + end) / 2, y = ifelse(is.null(ylim), 0.95, ylim[2] * 0.95),
            label = label),
        vjust = 0,
        size = 3
      )
  }

  # Customize appearance
  p <- p +
    scale_color_manual(
      name = "Activity Type",
      values = c("Exploration" = "#E74C3C",
                 "Exploitation" = "#3498DB")
    ) +
    scale_fill_manual(
      name = "Activity Type",
      values = c("Exploration" = "#E74C3C",
                 "Exploitation" = "#3498DB"),
      guide = "none"
    ) +
    scale_linetype_manual(
      name = "Strategy Group",
      values = c("Strategy 0" = "solid",
                 "Strategy 100" = "dashed")
    ) +
    labs(
      title = "Exploration vs Exploitation Dynamics",
      subtitle = sprintf("M=%d actors, N=%d components (Old: C1-C8, New: C9-C16)",
                         env$M, env$N),
      x = "Simulation Step",
      y = "Proportion of Activities"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.minor = element_blank()
    ) +
    coord_cartesian(ylim = ylim) +
    scale_y_continuous(labels = scales::percent)

  # Add parameter information to title
  params <- env$get_structure_model_params()
  sim_title_str <- env$get_structure_model_param_str(params)
  p <- p + ggtitle(sim_title_str,
                   subtitle = sprintf("Exploration (New Activities: C%d-C%d) vs Exploitation (Old Activities: C%d-C%d)",
                                      min(new_components), max(new_components),
                                      min(old_components), max(old_components)))

  # Save if requested
  if (plot_save) {
    plot_file <- paste0('explore_exploit_', plot_file, round(as.numeric(Sys.time()) * 10))
    ggsave(
      file = file.path(
        ifelse(is.na(plot_dir) || plot_dir == '', getwd(), plot_dir),
        sprintf("%s_%s.jpeg", env$config_environ_params$name, plot_file)
      ),
      p,
      width = 10,
      height = 6,
      units = 'in',
      dpi = 400
    )
  }

  if (plot_return) {
    return(p)
  }
}


# ---- plot_exploration_exploitation_phase -------------------------------------

#' @export
saomnk_plot_exploration_exploitation_phase <- function(
    env,
    time_window = 50,
    show_trajectories = TRUE,
    plot_return = TRUE
) {

  # Get the metrics using the same calculation as above
  bi_env_arr <- env$bi_env_arr
  old_components <- 1:8
  new_components <- 9:16

  # Calculate metrics (simplified)
  metrics_list <- list()
  unique_steps <- sort(unique(env$chain_stats$chain_step_id))

  for (t_idx in 1:min(dim(bi_env_arr)[3], length(unique_steps))) {
    incidence_t <- bi_env_arr[, , t_idx]

    for (i in 1:env$M) {
      actor_activities <- which(incidence_t[i, ] > 0)
      if (length(actor_activities) > 0) {
        metrics_list[[length(metrics_list) + 1]] <- data.frame(
          chain_step_id = unique_steps[t_idx],
          actor_id = as.character(i),
          exploration = sum(actor_activities %in% new_components) / length(actor_activities),
          exploitation = sum(actor_activities %in% old_components) / length(actor_activities)
        )
      }
    }
  }

  metrics_df <- bind_rows(metrics_list)

  # Add strategy
  if (exists('strat_1_coCovar', where = env)) {
    actor_strat <- env$get_actor_strategies()
    metrics_df$strategy <- factor(actor_strat[as.numeric(metrics_df$actor_id)])
  }

  # Create time periods
  metrics_df <- metrics_df %>%
    mutate(
      time_period = cut(chain_step_id,
                        breaks = seq(0, max(chain_step_id), by = time_window),
                        include.lowest = TRUE,
                        labels = FALSE)
    )

  # Create phase space plot
  p <- ggplot(metrics_df, aes(x = exploitation, y = exploration)) +
    geom_abline(intercept = 1, slope = -1, linetype = "dashed", color = "gray50") +
    geom_point(aes(color = factor(time_period)), alpha = 0.6, size = 2) +
    scale_color_viridis_d(name = "Time Period") +
    coord_fixed() +
    xlim(0, 1) + ylim(0, 1) +
    labs(
      title = "Actor Positions in Exploration-Exploitation Space",
      x = "Exploitation (proportion old activities)",
      y = "Exploration (proportion new activities)"
    ) +
    theme_minimal()

  # Add trajectories if requested
  if (show_trajectories) {
    p <- p + geom_path(
      aes(group = actor_id, color = factor(time_period)),
      alpha = 0.3,
      size = 0.5
    )
  }

  # Add strategy facets if exists
  if ("strategy" %in% names(metrics_df)) {
    p <- p + facet_wrap(~paste("Strategy", strategy))
  }

  # Add region labels
  p <- p +
    annotate("text", x = 0.8, y = 0.15, label = "Exploitation\nFocus",
             hjust = 0.5, color = "gray50", fontface = "italic") +
    annotate("text", x = 0.15, y = 0.8, label = "Exploration\nFocus",
             hjust = 0.5, color = "gray50", fontface = "italic") +
    annotate("text", x = 0.5, y = 0.5, label = "Balanced",
             hjust = 0.5, color = "gray50", fontface = "italic", angle = -45)

  if (plot_return) {
    return(p)
  }
}


# ---- plot_exploration_exploitation_improved ----------------------------------

#' @export
saomnk_plot_exploration_exploitation_improved <- function(
    env,
    actor_ids = c(),
    thin_factor = 1,
    show_points = FALSE,
    show_individuals = FALSE,
    show_group_means = TRUE,
    show_difference = TRUE,
    loess_span = 0.2,
    point_alpha = 0.2,
    line_alpha = 0.5,
    group_line_size = 2.5,
    se_ribbon = TRUE,
    ylim = c(0, 1),
    plot_return = TRUE,
    plot_save = FALSE,
    plot_file = '',
    plot_dir = NA
) {

  # Get actor strategies
  if (attr(env$strat_1_coCovar, 'nodeSet') != 'ACTORS')
    stop("Actor Strategy env$strat_1_coCovar not set.")

  actor_strat <- env$get_actor_strategies()

  # Get bipartite array data
  bi_env_arr <- env$bi_env_arr
  n_steps <- dim(bi_env_arr)[3]
  n_actors <- env$M
  n_components <- env$N

  # Define components
  old_components <- 1:8
  new_components <- 9:16

  # Get unique chain steps
  unique_steps <- sort(unique(env$chain_stats$chain_step_id))

  # Calculate metrics
  metrics_list <- list()

  for (t_idx in 1:min(n_steps, length(unique_steps))) {
    incidence_t <- bi_env_arr[, , t_idx]

    for (i in 1:n_actors) {
      actor_activities <- which(incidence_t[i, ] > 0)

      if (length(actor_activities) > 0) {
        n_old <- sum(actor_activities %in% old_components)
        n_new <- sum(actor_activities %in% new_components)
        n_total <- length(actor_activities)

        metrics_list[[length(metrics_list) + 1]] <- data.frame(
          chain_step_id = unique_steps[t_idx],
          actor_id = i,
          n_total_activities = n_total,
          n_old_activities = n_old,
          n_new_activities = n_new,
          prop_exploration = n_new / n_total,
          prop_exploitation = n_old / n_total
        )
      }
    }
  }

  # Combine metrics
  metrics_df <- bind_rows(metrics_list) %>%
    mutate(
      strategy = factor(actor_strat[actor_id], levels = c("0", "100")),
      strategy_label = ifelse(strategy == "0", "Control (0)", "Subsidized (100)")
    )

  # Apply thinning
  if (thin_factor > 1) {
    metrics_df <- metrics_df %>%
      filter(chain_step_id %% thin_factor == 0)
  }

  # Filter actors if specified
  if (length(actor_ids) > 0) {
    metrics_df <- metrics_df %>%
      filter(actor_id %in% actor_ids)
  }

  # Reshape to long format
  plot_data <- metrics_df %>%
    pivot_longer(
      cols = c(prop_exploration, prop_exploitation),
      names_to = "activity_type",
      values_to = "proportion"
    ) %>%
    mutate(
      activity_type = case_when(
        activity_type == "prop_exploration" ~ "Exploration",
        activity_type == "prop_exploitation" ~ "Exploitation"
      )
    )

  # Calculate group means
  group_means <- plot_data %>%
    group_by(chain_step_id, strategy, strategy_label, activity_type) %>%
    summarise(
      mean_proportion = mean(proportion, na.rm = TRUE),
      se_proportion = sd(proportion, na.rm = TRUE) / sqrt(n()),
      n_obs = n(),
      .groups = "drop"
    )

  # Get parameter string
  params <- env$get_structure_model_params()
  sim_title_str <- env$get_structure_model_param_str(params)

  # Create improved main plot
  p_main <- ggplot()

  # Add shock rectangles first (background)
  if (!is.null(env$theta_shocks)) {
    shock_rects <- env$get_theta_shock_rects_df(env$theta_shocks)
    p_main <- p_main +
      geom_rect(
        data = shock_rects,
        aes(xmin = start, xmax = end),
        ymin = -Inf, ymax = Inf,
        fill = '#FFA500',
        alpha = 0.1
      ) +
      geom_vline(
        data = shock_rects,
        aes(xintercept = start),
        linetype = "dotted",
        color = "darkorange",
        size = 0.8
      ) +
      annotate(
        "text",
        x = shock_rects$start[1] + (shock_rects$end[1] - shock_rects$start[1])/2,
        y = ylim[2] * 0.98,
        label = shock_rects$label[1],
        size = 3.5,
        fontface = "bold",
        color = "darkorange"
      )
  }

  # Add individual points if requested
  if (show_points) {
    p_main <- p_main + geom_point(
      data = plot_data,
      aes(x = chain_step_id, y = proportion, color = strategy_label),
      alpha = point_alpha,
      size = 0.8
    )
  }

  # Add confidence ribbons
  if (se_ribbon) {
    p_main <- p_main + geom_ribbon(
      data = group_means,
      aes(x = chain_step_id,
          ymin = pmax(0, mean_proportion - se_proportion),
          ymax = pmin(1, mean_proportion + se_proportion),
          fill = strategy_label,
          group = interaction(strategy_label, activity_type)),
      alpha = 0.2
    )
  }

  # Add mean lines with improved styling
  p_main <- p_main + geom_line(
    data = group_means,
    aes(x = chain_step_id,
        y = mean_proportion,
        color = strategy_label,
        linetype = activity_type,
        group = interaction(strategy_label, activity_type)),
    size = group_line_size
  )

  # Add smoothed trends with better visibility
  if (!is.null(loess_span)) {
    p_main <- p_main + geom_smooth(
      data = group_means,
      aes(x = chain_step_id,
          y = mean_proportion,
          color = strategy_label,
          linetype = activity_type,
          group = interaction(strategy_label, activity_type)),
      method = "loess",
      span = loess_span,
      se = FALSE,
      size = 1,
      alpha = 0.8
    )
  }

  # Improved color scheme and styling
  p_main <- p_main +
    scale_color_manual(
      name = "Strategy Group",
      values = c("Control (0)" = "#2C3E50",
                 "Subsidized (100)" = "#E74C3C")
    ) +
    scale_fill_manual(
      name = "Strategy Group",
      values = c("Control (0)" = "#2C3E50",
                 "Subsidized (100)" = "#E74C3C"),
      guide = "none"
    ) +
    scale_linetype_manual(
      name = "Activity Type",
      values = c("Exploitation" = "solid",
                 "Exploration" = "longdash")
    ) +
    labs(
      title = sim_title_str,
      subtitle = "Activity Portfolio Evolution: Exploitation (Old: C1-C8) vs Exploration (New: C9-C16)",
      x = "Simulation Step",
      y = "Proportion of Activities"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9)
    ) +
    scale_y_continuous(
      labels = scales::percent,
      limits = ylim,
      breaks = seq(0, 1, 0.25)
    ) +
    guides(
      color = guide_legend(order = 1, nrow = 1),
      linetype = guide_legend(order = 2, nrow = 1)
    )

  # Create difference plot if requested
  if (show_difference) {
    # Calculate differences between strategies
    diff_data <- group_means %>%
      select(chain_step_id, strategy, activity_type, mean_proportion) %>%
      pivot_wider(
        names_from = strategy,
        values_from = mean_proportion,
        names_prefix = "strategy_"
      ) %>%
      mutate(
        difference = strategy_100 - strategy_0,
        activity_type = factor(activity_type, levels = c("Exploitation", "Exploration"))
      )

    p_diff <- ggplot(diff_data, aes(x = chain_step_id, y = difference)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray50", size = 0.5) +
      geom_line(aes(color = activity_type), size = 1.5) +
      geom_smooth(
        aes(color = activity_type),
        method = "loess",
        span = loess_span * 1.5,
        se = TRUE,
        alpha = 0.2,
        size = 0.8
      )

    # Add shock period
    if (!is.null(env$theta_shocks)) {
      p_diff <- p_diff +
        geom_rect(
          data = shock_rects,
          aes(xmin = start, xmax = end),
          ymin = -Inf, ymax = Inf,
          fill = '#FFA500',
          alpha = 0.1
        )
    }

    p_diff <- p_diff +
      scale_color_manual(
        name = "Activity Type",
        values = c("Exploitation" = "#3498DB",
                   "Exploration" = "#E74C3C")
      ) +
      labs(
        title = "Strategy Differential (Subsidized - Control)",
        subtitle = "Positive values indicate subsidized actors have higher proportion",
        x = "Simulation Step",
        y = "Difference in Proportion"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, color = "gray40")
      ) +
      scale_y_continuous(labels = scales::percent)

    # Combine plots
    combined_plot <- p_main / p_diff +
      plot_layout(heights = c(3, 1))

    return(combined_plot)
  }

  return(p_main)
}


# ---- plot_exploration_exploitation_faceted ------------------------------------

#' @export
saomnk_plot_exploration_exploitation_faceted <- function(
    env,
    metrics_df,
    loess_span = 0.3,
    show_points = FALSE
) {

  # Prepare data
  plot_data <- metrics_df %>%
    mutate(
      strategy_label = ifelse(strategy == "0", "Control (0)", "Subsidized (100)")
    ) %>%
    select(chain_step_id, actor_id, strategy_label,
           Exploitation = prop_exploitation,
           Exploration = prop_exploration) %>%
    pivot_longer(
      cols = c(Exploitation, Exploration),
      names_to = "Activity_Type",
      values_to = "Proportion"
    )

  # Calculate means
  group_means <- plot_data %>%
    group_by(chain_step_id, strategy_label, Activity_Type) %>%
    summarise(
      mean_prop = mean(Proportion, na.rm = TRUE),
      se_prop = sd(Proportion, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # Create faceted plot
  p <- ggplot(group_means, aes(x = chain_step_id, y = mean_prop)) +
    facet_grid(Activity_Type ~ strategy_label, scales = "free_y") +
    geom_ribbon(
      aes(ymin = mean_prop - se_prop,
          ymax = mean_prop + se_prop),
      alpha = 0.2
    ) +
    geom_line(size = 1.5, color = "#2C3E50") +
    geom_smooth(
      method = "loess",
      span = loess_span,
      se = FALSE,
      color = "#E74C3C",
      size = 1,
      linetype = "dashed"
    )

  # Add points if requested
  if (show_points) {
    p <- p + geom_point(
      data = plot_data,
      aes(x = chain_step_id, y = Proportion),
      alpha = 0.1,
      size = 0.5
    )
  }

  p <- p +
    labs(
      title = "Activity Type Evolution by Strategy Group",
      x = "Simulation Step",
      y = "Proportion"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "gray95"),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(labels = scales::percent)

  return(p)
}


# ---- plot_exploration_exploitation_by_strategy -------------------------------

#' @export
saomnk_plot_exploration_exploitation_by_strategy <- function(
    env,
    actor_ids = c(),
    thin_factor = 1,
    show_points = TRUE,
    show_individuals = FALSE,
    show_group_means = TRUE,
    loess_span = 0.3,
    point_alpha = 0.3,
    line_alpha = 0.5,
    group_line_size = 2,
    se_ribbon = TRUE,
    ylim = c(0, 1),
    plot_return = TRUE,
    plot_save = FALSE,
    plot_file = '',
    plot_dir = NA
) {

  # Get actor strategies using the R6 method
  actor_strategies <- env$get_actor_strategies()

  # Get bipartite array data
  bi_env_arr <- env$bi_env_arr
  n_steps <- dim(bi_env_arr)[3]
  n_actors <- env$M
  n_components <- env$N

  # Define old (exploitation) and new (exploration) components
  old_components <- 1:8
  new_components <- 9:16

  # Get unique chain steps
  unique_steps <- sort(unique(env$chain_stats$chain_step_id))

  # Calculate exploration/exploitation metrics
  metrics_list <- list()

  for (t_idx in 1:min(n_steps, length(unique_steps))) {
    incidence_t <- bi_env_arr[, , t_idx]

    for (i in 1:n_actors) {
      actor_activities <- which(incidence_t[i, ] > 0)

      if (length(actor_activities) > 0) {
        n_old <- sum(actor_activities %in% old_components)
        n_new <- sum(actor_activities %in% new_components)
        n_total <- length(actor_activities)

        metrics_list[[length(metrics_list) + 1]] <- data.frame(
          chain_step_id = unique_steps[t_idx],
          actor_id = as.character(i),
          strategy = as.character(actor_strategies[i]),
          n_total_activities = n_total,
          n_old_activities = n_old,
          n_new_activities = n_new,
          prop_exploration = n_new / n_total,
          prop_exploitation = n_old / n_total
        )
      }
    }
  }

  # Combine metrics
  metrics_df <- bind_rows(metrics_list) %>%
    mutate(
      strategy = factor(strategy),
      strategy_label = paste("Strategy", strategy)
    )

  # Apply thinning
  if (thin_factor > 1) {
    metrics_df <- metrics_df %>%
      filter(chain_step_id %% thin_factor == 0)
  }

  # Filter actors if specified
  if (length(actor_ids) > 0) {
    metrics_df <- metrics_df %>%
      filter(actor_id %in% actor_ids)
  }

  # Reshape to long format
  plot_data <- metrics_df %>%
    pivot_longer(
      cols = c(prop_exploration, prop_exploitation),
      names_to = "activity_type",
      values_to = "proportion"
    ) %>%
    mutate(
      activity_type = case_when(
        activity_type == "prop_exploration" ~ "Exploration",
        activity_type == "prop_exploitation" ~ "Exploitation"
      ),
      activity_label = factor(activity_type, levels = c("Exploitation", "Exploration"))
    )

  # Calculate group means by strategy and activity type
  group_means <- plot_data %>%
    group_by(chain_step_id, strategy, strategy_label, activity_type, activity_label) %>%
    summarise(
      mean_proportion = mean(proportion, na.rm = TRUE),
      se_proportion = sd(proportion, na.rm = TRUE) / sqrt(n()),
      n_obs = n(),
      .groups = "drop"
    )

  # Create plot
  p <- ggplot()

  # Add individual points if requested
  if (show_points) {
    p <- p + geom_point(
      data = plot_data,
      aes(x = chain_step_id,
          y = proportion,
          color = strategy_label),
      alpha = point_alpha,
      size = 0.8
    )
  }

  # Add individual lines if requested
  if (show_individuals) {
    p <- p + geom_line(
      data = plot_data,
      aes(x = chain_step_id,
          y = proportion,
          color = strategy_label,
          linetype = activity_label,
          group = interaction(actor_id, activity_type)),
      alpha = line_alpha * 0.3,
      size = 0.4
    )
  }

  # Add group mean lines
  if (show_group_means) {
    # Add confidence ribbons if requested
    if (se_ribbon) {
      p <- p + geom_ribbon(
        data = group_means,
        aes(x = chain_step_id,
            ymin = mean_proportion - se_proportion,
            ymax = mean_proportion + se_proportion,
            fill = strategy_label,
            group = interaction(strategy_label, activity_label)),
        alpha = 0.15
      )
    }

    # Add mean lines with strategy as color and activity type as linetype
    p <- p + geom_line(
      data = group_means,
      aes(x = chain_step_id,
          y = mean_proportion,
          color = strategy_label,
          linetype = activity_label,
          group = interaction(strategy_label, activity_label)),
      size = group_line_size
    )

    # Add smoothed trends
    if (!is.null(loess_span)) {
      p <- p + geom_smooth(
        data = group_means,
        aes(x = chain_step_id,
            y = mean_proportion,
            color = strategy_label,
            linetype = activity_label,
            group = interaction(strategy_label, activity_label)),
        method = "loess",
        span = loess_span,
        se = FALSE,
        size = 0.8,
        alpha = 0.7
      )
    }
  }

  # Add shock rectangles if they exist
  if (!is.null(env$theta_shocks)) {
    shock_rects <- env$get_theta_shock_rects_df(env$theta_shocks)
    p <- p +
      geom_rect(
        data = shock_rects,
        aes(xmin = start, xmax = end),
        ymin = -Inf, ymax = Inf,
        fill = 'darkorange',
        alpha = 0.05
      ) +
      geom_text(
        data = shock_rects,
        aes(x = (start + end) / 2,
            y = ylim[2] * 0.95,
            label = label),
        vjust = 0,
        size = 3
      )
  }

  # Customize appearance with strategy colors and activity linetypes
  p <- p +
    scale_color_manual(
      name = "Strategy Group",
      values = c("Strategy 0" = "#3498DB",
                 "Strategy 100" = "#E74C3C")
    ) +
    scale_fill_manual(
      name = "Strategy Group",
      values = c("Strategy 0" = "#3498DB",
                 "Strategy 100" = "#E74C3C"),
      guide = "none"
    ) +
    scale_linetype_manual(
      name = "Activity Type",
      values = c("Exploitation" = "solid",
                 "Exploration" = "dashed")
    ) +
    labs(
      title = "Exploration vs Exploitation by Strategy Groups",
      subtitle = sprintf("M=%d actors, N=%d components (Old Activities: C1-C8, New Activities: C9-C16)",
                         env$M, env$N),
      x = "Simulation Step",
      y = "Proportion of Activities"
    ) +
    theme_bw() +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray50")
    ) +
    coord_cartesian(ylim = ylim) +
    scale_y_continuous(labels = scales::percent)

  # Add parameter information if available
  if (!is.null(env$get_structure_model_params)) {
    params <- env$get_structure_model_params()
    sim_title_str <- env$get_structure_model_param_str(params)
    p <- p + labs(caption = sim_title_str)
  }

  # Save if requested
  if (plot_save) {
    plot_file <- paste0('strategy_explore_exploit_', plot_file, round(as.numeric(Sys.time()) * 10))
    ggsave(
      file = file.path(
        ifelse(is.na(plot_dir) || plot_dir == '', getwd(), plot_dir),
        sprintf("%s_%s.jpeg", env$config_environ_params$name, plot_file)
      ),
      p,
      width = 10,
      height = 6,
      units = 'in',
      dpi = 400
    )
  }

  if (plot_return) {
    return(p)
  }
}


# ---- plot_strategy_exploration_exploitation ----------------------------------

#' @export
saomnk_plot_strategy_exploration_exploitation <- function(
    env,
    result,
    show_points = TRUE,
    show_group_means = TRUE,
    group_line_size = 2
) {

  # Use the metrics from the result
  metrics_df <- result$metrics

  # Reshape to long format
  plot_data <- metrics_df %>%
    select(chain_step_id, actor_id, strategy, exploration, exploitation) %>%
    pivot_longer(
      cols = c(exploration, exploitation),
      names_to = "activity_type",
      values_to = "proportion"
    ) %>%
    mutate(
      activity_type = str_to_title(activity_type),
      strategy_label = paste("Strategy", strategy)
    )

  # Calculate group means
  group_means <- plot_data %>%
    group_by(chain_step_id, strategy, strategy_label, activity_type) %>%
    summarise(
      mean_proportion = mean(proportion, na.rm = TRUE),
      se_proportion = sd(proportion, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # Create plot
  p <- ggplot()

  # Add points
  if (show_points) {
    p <- p + geom_point(
      data = plot_data,
      aes(x = chain_step_id, y = proportion, color = strategy_label),
      alpha = 0.2,
      size = 0.8
    )
  }

  # Add group means
  if (show_group_means) {
    # Confidence ribbons
    p <- p + geom_ribbon(
      data = group_means,
      aes(x = chain_step_id,
          ymin = mean_proportion - se_proportion,
          ymax = mean_proportion + se_proportion,
          fill = strategy_label,
          group = interaction(strategy_label, activity_type)),
      alpha = 0.15
    )

    # Mean lines
    p <- p + geom_line(
      data = group_means,
      aes(x = chain_step_id,
          y = mean_proportion,
          color = strategy_label,
          linetype = activity_type,
          group = interaction(strategy_label, activity_type)),
      size = group_line_size
    )
  }

  # Customize
  p <- p +
    scale_color_manual(
      name = "Strategy Group",
      values = c("Strategy 0" = "#3498DB",
                 "Strategy 100" = "#E74C3C")
    ) +
    scale_fill_manual(
      name = "Strategy Group",
      values = c("Strategy 0" = "#3498DB",
                 "Strategy 100" = "#E74C3C"),
      guide = "none"
    ) +
    scale_linetype_manual(
      name = "Activity Type",
      values = c("Exploitation" = "solid",
                 "Exploration" = "dashed")
    ) +
    labs(
      title = "Exploration vs Exploitation by Strategy Groups",
      subtitle = "Colors: Strategy groups | Line types: Activity types",
      x = "Simulation Step",
      y = "Proportion"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.box = "vertical"
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))

  return(p)
}


# ---- plot_subsidized_risk_taking ---------------------------------------------

#' @export
saomnk_plot_subsidized_risk_taking <- function(
    env,
    metrics_df = NULL,
    shock_time = NULL,
    show_se_ribbon = TRUE,
    line_size = 2,
    point_alpha = 0.3
) {

  if (is.null(metrics_df)) {
    metrics_df <- env$calculate_explore_exploit_risk_adjusted()
  }

  plot_data <- metrics_df %>%
    mutate(
      post_subsidy = ifelse(!is.null(shock_time),
                            chain_step_id >= shock_time,
                            FALSE),
      strategy_label = case_when(
        strategy == "0" ~ "Control (No Subsidy)",
        strategy == "100" ~ "Treatment (Subsidized)",
        TRUE ~ as.character(strategy)
      )
    )

  p <- ggplot(plot_data) +
    geom_line(
      aes(x = chain_step_id,
          y = risk_taking_score,
          color = strategy_label,
          group = interaction(actor_id, strategy_label)),
      alpha = point_alpha, size = 0.5
    ) +
    stat_summary(
      aes(x = chain_step_id,
          y = risk_taking_score,
          color = strategy_label),
      fun = mean,
      geom = "line",
      size = line_size
    )

  if (show_se_ribbon) {
    p <- p + stat_summary(
      aes(x = chain_step_id,
          y = risk_taking_score,
          fill = strategy_label),
      fun.data = mean_se,
      geom = "ribbon",
      alpha = 0.2
    )
  }

  if (!is.null(shock_time)) {
    p <- p +
      geom_vline(xintercept = shock_time,
                 linetype = "dashed",
                 color = "gray50") +
      annotate("text", x = shock_time, y = 0.9,
               label = "Subsidy\nShock",
               hjust = -0.1, size = 3)
  }

  p <- p +
    scale_color_manual(
      values = c("Control (No Subsidy)" = "#2C3E50",
                 "Treatment (Subsidized)" = "#E74C3C")
    ) +
    scale_fill_manual(
      values = c("Control (No Subsidy)" = "#2C3E50",
                 "Treatment (Subsidized)" = "#E74C3C"),
      guide = "none"
    ) +
    labs(
      title = "Firm Risk-Taking Behavior: Subsidized vs Control",
      subtitle = "Higher scores indicate more exploration of risky new activities",
      x = "Simulation Step",
      y = "Risk-Taking Score",
      color = "Treatment Group"
    ) +
    theme_minimal() +
    ylim(0, 1)

  return(p)
}


# ---- plot_did_exploration ----------------------------------------------------

#' @export
saomnk_plot_did_exploration <- function(
    env,
    metrics_df = NULL,
    treatment_time = 50
) {

  if (is.null(metrics_df)) {
    metrics_df <- env$calculate_explore_exploit_risk_adjusted()
    metrics_df <- env$calculate_social_logic_influence(metrics_df)
  }

  # Calculate summaries in two steps to avoid n() issues
  did_summary <- metrics_df %>%
    dplyr::mutate(
      period = factor(ifelse(chain_step_id < treatment_time, "Pre", "Post"), levels = c("Pre", "Post")),
      treatment = ifelse(strategy == "100", "Treated", "Control")
    ) %>%
    dplyr::group_by(period, treatment) %>%
    dplyr::summarise(
      mean_exploration = mean(exploration_adjusted, na.rm = TRUE),
      mean_risk_taking = mean(risk_taking_score, na.rm = TRUE),
      sd_risk_taking = sd(risk_taking_score, na.rm = TRUE),
      n_obs = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se_risk_taking = sd_risk_taking / sqrt(n_obs)
    )

  p <- ggplot(did_summary, aes(x = period, y = mean_risk_taking,
                               color = treatment, group = treatment)) +
    geom_line(size = 2) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = mean_risk_taking - se_risk_taking,
                      ymax = mean_risk_taking + se_risk_taking),
                  width = 0.1) +
    scale_color_manual(values = c("Control" = "#2C3E50",
                                  "Treated" = "#E74C3C")) +
    labs(
      title = "Difference-in-Differences: Subsidy Effect on Risk-Taking",
      subtitle = "Comparing subsidized vs non-subsidized firms",
      x = "Period",
      y = "Mean Risk-Taking Score"
    ) +
    theme_minimal()

  return(p)
}


# ---- plot_exploration_exploitation_subsidies ---------------------------------

#' @export
saomnk_plot_exploration_exploitation_subsidies <- function(
    env,
    metrics_df = NULL,
    show_points = FALSE,
    show_se_ribbon = TRUE,
    loess_span = 0.3,
    shock_time = NULL
) {

  if (is.null(metrics_df)) {
    metrics_df <- env$calculate_explore_exploit_risk_adjusted()
    metrics_df <- env$calculate_social_logic_influence(metrics_df)
  }

  plot_data <- metrics_df %>%
    tidyr::pivot_longer(
      cols = c(exploration_adjusted, exploitation_adjusted),
      names_to = "behavior_type",
      values_to = "intensity"
    ) %>%
    dplyr::mutate(
      behavior_type = dplyr::case_when(
        behavior_type == "exploration_adjusted" ~ "Exploration",
        behavior_type == "exploitation_adjusted" ~ "Exploitation"
      ),
      strategy_label = dplyr::case_when(
        strategy == "0" ~ "Control (0)",
        strategy == "100" ~ "Subsidized (100)",
        TRUE ~ paste("Strategy", strategy)
      ),
      activity_type = behavior_type
    )

  # Calculate group means with explicit dplyr namespace
  group_means <- plot_data %>%
    dplyr::group_by(chain_step_id, strategy, strategy_label, activity_type) %>%
    dplyr::summarise(
      mean_proportion = mean(intensity, na.rm = TRUE),
      sd_proportion = sd(intensity, na.rm = TRUE),
      n_obs = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se_proportion = sd_proportion / sqrt(n_obs)
    )

  p <- ggplot()

  if (show_points) {
    p <- p + geom_point(
      data = plot_data,
      aes(x = chain_step_id, y = intensity, color = strategy_label),
      alpha = 0.1, size = 0.5
    )
  }

  if (show_se_ribbon) {
    p <- p + geom_ribbon(
      data = group_means,
      aes(x = chain_step_id,
          ymin = mean_proportion - se_proportion,
          ymax = mean_proportion + se_proportion,
          fill = strategy_label,
          group = interaction(strategy_label, activity_type)),
      alpha = 0.15
    )
  }

  p <- p + geom_line(
    data = group_means,
    aes(x = chain_step_id,
        y = mean_proportion,
        color = strategy_label,
        linetype = activity_type),
    size = 1.5
  )

  if (!is.null(shock_time)) {
    p <- p +
      geom_vline(xintercept = shock_time, linetype = "dashed",
                 color = "gray50", alpha = 0.7) +
      annotate("text", x = shock_time, y = 0.95,
               label = "Subsidy", hjust = -0.1, size = 3,
               color = "gray40")
  }

  p <- p +
    scale_color_manual(
      name = "Strategy Group",
      values = c("Control (0)" = "#2C3E50",
                 "Subsidized (100)" = "#E74C3C")
    ) +
    scale_fill_manual(
      name = "Strategy Group",
      values = c("Control (0)" = "#2C3E50",
                 "Subsidized (100)" = "#E74C3C"),
      guide = "none"
    ) +
    scale_linetype_manual(
      name = "Activity Type",
      values = c("Exploitation" = "solid",
                 "Exploration" = "longdash")
    ) +
    labs(
      title = "Risk-Adjusted Exploration vs Exploitation: Subsidies Impact",
      subtitle = "Social logic constraints applied; moral licensing effect included",
      x = "Simulation Step",
      y = "Activity Intensity (Risk-Adjusted)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))

  return(p)
}


# ---- plot_exploration_risk_multiperiod ---------------------------------------

#' @export
saomnk_plot_exploration_risk_multiperiod <- function(
    env,
    metrics_df = NULL,
    show_points = FALSE,
    show_se_ribbon = TRUE,
    line_size = 1.5,
    point_alpha = 0.1,
    loess_span = 0.3
) {

  if (is.null(metrics_df)) {
    metrics_df <- env$calculate_explore_exploit_risk_adjusted()
  }

  # Automatically detect shock start period
  shock_time <- NULL
  shock_label <- NULL
  if (!is.null(env$theta_shocks) && length(env$theta_shocks) > 0) {
    # Find the first element where shock_on = 1
    shock_idx <- which(sapply(env$theta_shocks, function(x) x$shock_on == 1))[1]
    if (!is.na(shock_idx)) {
      shock_time <- min(env$theta_shocks[[shock_idx]]$chain_step_ids)
      shock_label <- ifelse(!is.null(env$theta_shocks[[shock_idx]]$label),
                            env$theta_shocks[[shock_idx]]$label,
                            "Subsidy\nShock")
    }
  }

  # Prepare data
  plot_data <- metrics_df %>%
    dplyr::mutate(
      strategy_label = dplyr::case_when(
        strategy == "0" ~ "Control (No Subsidy)",
        strategy == "100" ~ "Treatment (Subsidized)",
        TRUE ~ as.character(strategy)
      )
    )

  # Create the plot
  p <- ggplot(plot_data)

  # Add individual trajectories if requested
  if (show_points) {
    p <- p + geom_line(
      aes(x = chain_step_id,
          y = risk_taking_score,
          color = strategy_label,
          group = interaction(actor_id, strategy_label)),
      alpha = point_alpha,
      size = 0.3
    )
  }

  # Add mean lines
  p <- p + stat_summary(
    aes(x = chain_step_id,
        y = risk_taking_score,
        color = strategy_label),
    fun = mean,
    geom = "line",
    size = line_size
  )

  # Add SE ribbons if requested
  if (show_se_ribbon) {
    p <- p + stat_summary(
      aes(x = chain_step_id,
          y = risk_taking_score,
          fill = strategy_label),
      fun.data = mean_se,
      geom = "ribbon",
      alpha = 0.2
    )
  }

  # Add shock period shading
  if (!is.null(shock_time)) {
    shock_end <- max(plot_data$chain_step_id)
    p <- p +
      annotate("rect",
               xmin = shock_time,
               xmax = shock_end,
               ymin = -Inf,
               ymax = Inf,
               fill = "orange",
               alpha = 0.1) +
      annotate("text",
               x = shock_time + (shock_end - shock_time) / 2,
               y = max(plot_data$risk_taking_score) * 0.95,
               label = shock_label,
               size = 3.5,
               color = "black")
  }

  # Format the plot
  p <- p +
    scale_color_manual(
      values = c("Control (No Subsidy)" = "#2C3E50",
                 "Treatment (Subsidized)" = "#E74C3C")
    ) +
    scale_fill_manual(
      values = c("Control (No Subsidy)" = "#2C3E50",
                 "Treatment (Subsidized)" = "#E74C3C"),
      guide = "none"
    ) +
    labs(
      title = "Firm Risk-Taking Behavior: Subsidized vs Control",
      subtitle = "Higher scores indicate more exploration of risky new activities",
      x = "Simulation Step",
      y = "Risk-Taking Score",
      color = "Treatment Group"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    ) +
    ylim(0, 1)

  return(p)
}


# ---- plot_exploration_risk_did -----------------------------------------------

#' @export
saomnk_plot_exploration_risk_did <- function(
    env,
    metrics_df = NULL,
    pre_periods = NULL,
    post_periods = NULL
) {

  if (is.null(metrics_df)) {
    metrics_df <- env$calculate_explore_exploit_risk_adjusted()
  }

  # Automatically detect shock period
  shock_time <- NULL
  if (!is.null(env$theta_shocks) && length(env$theta_shocks) > 0) {
    shock_time <- min(env$theta_shocks[[1]]$chain_step_ids)
  }

  # Define pre/post periods if not provided
  if (is.null(shock_time)) {
    # If no shock, use midpoint
    midpoint <- median(unique(metrics_df$chain_step_id))
    pre_periods <- metrics_df$chain_step_id < midpoint
    post_periods <- metrics_df$chain_step_id >= midpoint
  } else {
    pre_periods <- metrics_df$chain_step_id < shock_time
    post_periods <- metrics_df$chain_step_id >= shock_time
  }

  # Calculate summaries
  did_summary <- metrics_df %>%
    mutate(
      period = dplyr::case_when(
        chain_step_id %in% metrics_df$chain_step_id[pre_periods] ~ "Pre",
        chain_step_id %in% metrics_df$chain_step_id[post_periods] ~ "Post",
        TRUE ~ NA_character_
      ),
      treatment = ifelse(strategy == "100", "Treated", "Control")
    ) %>%
    filter(!is.na(period)) %>%
    dplyr::group_by(period, treatment) %>%
    dplyr::summarise(
      mean_risk_taking = mean(risk_taking_score, na.rm = TRUE),
      se_risk_taking = sd(risk_taking_score, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  # Create the plot
  p <- ggplot(did_summary, aes(x = period, y = mean_risk_taking,
                               color = treatment, group = treatment)) +
    geom_line(size = 2) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = mean_risk_taking - se_risk_taking,
                      ymax = mean_risk_taking + se_risk_taking),
                  width = 0.1) +
    scale_color_manual(values = c("Control" = "#2C3E50",
                                  "Treated" = "#E74C3C")) +
    labs(
      title = "Difference-in-Differences: Subsidy Effect on Risk-Taking",
      subtitle = "Comparing subsidized vs non-subsidized firms",
      x = "Period",
      y = "Mean Risk-Taking Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )

  return(p)
}


# ---- plot_exploration_event_study --------------------------------------------

#' @export
saomnk_plot_exploration_event_study <- function(
    env,
    metric = "exploration",
    save_plot = FALSE,
    plot_dir = NULL
) {

  # Get dynamic results
  ed <- data.table::rbindlist(
    env$test_exploration_shocks_did(test_type = 'dynamic', metric = metric),
    idcol = 'test_id'
  )

  # Add metadata
  ed <- ed %>%
    mutate(
      test_type = 'dynamic',
      stat_type = toupper(metric),
      combined_comparison = 'Common Treatment Scale'
    ) %>%
    filter(event.time >= -1)

  # Get simulation parameters
  sim_title_str <- env$get_structure_model_param_str()

  # Find shock timing
  shock_idx <- which(sapply(env$theta_shocks, function(x) x$shock_on == 1))[1]
  shock_start <- min(env$theta_shocks[[shock_idx]]$chain_step_ids)
  shock_label <- ifelse(!is.null(env$theta_shocks[[shock_idx]]$label),
                        env$theta_shocks[[shock_idx]]$label, "subsidy")

  # Create the plot matching K_AC style
  p <- ggplot(ed, aes(x = event.time, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +

    # Add shock region
    annotate("rect", xmin = -0.5, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = 'darkorange', alpha = 0.05) +

    # Add confidence bands and estimates
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.2, fill = "cyan3") +
    geom_line(color = "cyan3", size = 1.2) +
    geom_point(color = "cyan3", size = 2) +

    # Add shock label
    annotate("text", x = max(ed$event.time) * 0.5,
             y = max(ed$conf.high) * 0.9,
             label = shock_label, vjust = 0, size = 2.7, color = 'black') +

    facet_grid(stat_type ~ test_id) +

    labs(
      x = sprintf("Event Time\n(Shock Starts at Simulated Decision Chain Step %d)", shock_start),
      y = "Avg Treatment Effect on Treated (ATT)",
      title = paste("Multiperiod Diff-in-Diff Test of", toupper(metric)),
      subtitle = sim_title_str
    ) +

    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "gray90", color = "gray50"),
      strip.text = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )

  # Save plot if requested
  if (save_plot && !is.null(plot_dir)) {
    if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
    filename <- file.path(plot_dir, paste0("exploration_event_study_", metric, ".png"))
    ggsave(filename, p, width = 10, height = 8, dpi = 300)
    cat("Plot saved to:", filename, "\n")
  }

  return(p)
}


# ---- plot_exploration_did_combined -------------------------------------------

#' @export
saomnk_plot_exploration_did_combined <- function(
    env,
    metrics = c("exploration", "n_new", "prop_new"),
    save_plot = FALSE,
    plot_dir = NULL
) {

  # Create data for all metrics
  all_data <- list()

  for (metric in metrics) {
    ed <- data.table::rbindlist(
      env$test_exploration_shocks_did(test_type = 'dynamic', metric = metric),
      idcol = 'test_id'
    )

    ed <- ed %>%
      mutate(
        test_type = 'dynamic',
        stat_type = toupper(metric),
        combined_comparison = 'Common Treatment Scale'
      ) %>%
      filter(event.time >= -1)

    all_data[[metric]] <- ed
  }

  # Combine all data
  combined_data <- bind_rows(all_data)

  # Get simulation parameters
  sim_title_str <- env$get_structure_model_param_str()

  # Find shock info
  shock_idx <- which(sapply(env$theta_shocks, function(x) x$shock_on == 1))[1]
  shock_start <- min(env$theta_shocks[[shock_idx]]$chain_step_ids)

  # Create separate plots for each metric (matching K_AC multi-panel style)
  plot_list <- list()

  for (metric in unique(combined_data$stat_type)) {
    metric_data <- combined_data %>% filter(stat_type == metric)

    p <- ggplot(metric_data, aes(x = event.time, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +

      # Add shock region
      annotate("rect", xmin = -0.5, xmax = Inf, ymin = -Inf, ymax = Inf,
               fill = 'darkorange', alpha = 0.05) +

      # Add estimates
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                  alpha = 0.2, fill = "cyan3") +
      geom_line(color = "cyan3", size = 1.2) +
      geom_point(color = "cyan3", size = 2) +

      # Add shock label
      annotate("text", x = max(metric_data$event.time) * 0.5,
               y = max(metric_data$conf.high) * 0.9,
               label = "subsidy", vjust = 0, size = 2.7, color = 'black') +

      facet_wrap(~ stat_type) +

      labs(x = NULL, y = "Avg Treatment Effect on Treated (ATT)",
           title = metric) +

      theme_minimal() +
      theme(
        strip.background = element_rect(fill = "gray90"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )

    plot_list[[metric]] <- p
  }

  # Combine plots
  combined_plot <- ggarrange(plotlist = plot_list, nrow = length(plot_list),
                             common.legend = FALSE)

  # Add common title and x-label
  combined_plot <- annotate_figure(
    combined_plot,
    top = text_grob(paste("Multiperiod Diff-in-Diff Tests of Exploration Metrics\n", sim_title_str),
                    face = "bold", size = 14),
    bottom = text_grob(sprintf("Event Time\n(Shock Starts at Simulated Decision Chain Step %d)", shock_start))
  )

  # Save if requested
  if (save_plot && !is.null(plot_dir)) {
    if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
    filename <- file.path(plot_dir, "exploration_did_combined.png")
    ggsave(filename, combined_plot, width = 10, height = 12, dpi = 300)
    cat("Plot saved to:", filename, "\n")
  }

  return(combined_plot)
}
