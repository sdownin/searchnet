# ============================================================================
# plot-shocks.R
# Standalone shock-related plot functions extracted from saomnk-class.R
# Each function takes `env` (an SaoMNK environment) as its first argument.
# All self$ references have been replaced with env$.
# ============================================================================


#' @export
saomnk_plot_shocks <- function(env, verbose = FALSE) {
  if(is.null(env$theta_shocks))
    stop('env$theta_shocks is missing.')
  sim_title_str <- env$get_structure_model_param_str()
  theta_shocks <- env$theta_shocks
  actor_strats <- as.factor(env$get_actor_strategies())
  nsteps <- max(unlist(lapply(theta_shocks, function(x)x$chain_step_ids)))
  util <- env$actor_util_df
  util$shock_id <- NA
  util$shock_label <- NA
  util$shock_on <- NA
  util$treatment_group <- 0
  Kdf <- env$get_K4_df()
  Kdf$shock_id <- NA
  Kdf$shock_label <- NA
  Kdf$shock_on <- NA
  Kdf$treatment_group <- 0
  statdf <- env$actor_stats_df
  statdf$shock_id <- NA
  statdf$shock_label <- NA
  statdf$shock_on <- NA
  statdf$treatment_group <- 0
  for (i in 1:length(theta_shocks)) {

    shock <- theta_shocks[[ i ]]

    util_idx <- which(util$chain_step_id %in% shock$chain_step_ids)
    Kdf_idx  <- which(Kdf$chain_step_id %in% shock$chain_step_ids)
    statdf_idx  <- which(statdf$chain_step_id %in% shock$chain_step_ids)

    util$shock_id[ util_idx ]      <- i
    Kdf$shock_id[ Kdf_idx ]        <- i
    statdf$shock_id[ statdf_idx ]  <- i

    util$shock_on[ util_idx ]      <- shock$shock_on
    Kdf$shock_on[ Kdf_idx ]        <- shock$shock_on
    statdf$shock_on[ statdf_idx ]  <- shock$shock_on

    util$shock_label[ util_idx ]      <- ifelse(is.null(shock$label), as.character(i), shock$label)
    Kdf$shock_label[ Kdf_idx ]        <- ifelse(is.null(shock$label), as.character(i), shock$label)
    statdf$shock_label[ statdf_idx ]  <- ifelse(is.null(shock$label), as.character(i), shock$label)

    strat_effs <- env$get_rsiena_effects_theta_df(no_rates = T) %>% filter(grepl('(self\\$)?strat_\\d{1,2}',effect_key,ignore.case = T))
    ## LOOP EFFECTS j IN SHOCK i
    for (j in 1:nrow(strat_effs)) {

      strat_eff_j <- strat_effs[j,]
      strat_eff_shock_eff_id <- which(shock$effect_level == strat_eff_j$effect_level )

      is_treated <- FALSE
      if(length(strat_eff_shock_eff_id)) {
        is_treated <- (shock$parameter[ strat_eff_shock_eff_id ] != 0 )
      }
      if(is_treated){
        strat_cov_attr <- gsub('self\\$','', strat_eff_j$interaction1)
        strat_treated_ids <- which( env[[ strat_cov_attr ]] != 0 )
        util_treat_idx <- which( util$actor_id %in% strat_treated_ids )
        Kdf_treat_idx  <- which( Kdf$actor_id %in% strat_treated_ids )
        statdf_treat_idx  <- which( statdf$actor_id %in% strat_treated_ids )
        util$treatment_group[ util_treat_idx ]      <- min(shock$chain_step_ids)
        Kdf$treatment_group[ Kdf_treat_idx ]        <- min(shock$chain_step_ids)
        statdf$treatment_group[ statdf_treat_idx ]  <- min(shock$chain_step_ids)
      }

    }##/end j effect loop in shock i

  }##/end i shock loop


  util <- util %>% mutate(value = utility, utility=NULL) ## Swap in utility for the 'value' to be computed
  statdf <- statdf %>% mutate(value = value_contributions, value_contributions=NULL) ## USE VALUE CONTRIBUTIONS


  pu <- util %>%
    ggplot(aes(x=shock_id, y=value, color=strategy,fill=strategy)) +
    geom_point(position='jitter', alpha=.1) + geom_boxplot(aes(shape=shock_label), alpha=.2) +
    ggtitle(sprintf('Utility by Exogenous Shock\n%s', sim_title_str)) +
    theme_bw()

  pk <- Kdf %>% filter(effect %in% c('K_AC','K_AA')) %>%
    ggplot(aes(x=shock_id, y=value, color=node_group,fill=node_group)) +
    geom_point(position='jitter', alpha=.1) + geom_boxplot(aes(shape=shock_label), alpha=.2) +
    facet_grid(effect ~ ., scales='free_y') +
    ggtitle(sprintf('Actor and Component Degrees by Exogenous Shock\n%s', sim_title_str)) +
    theme_bw()

  ps <- statdf %>%
    ggplot(aes(x=shock_id, y=value, color=strategy,fill=strategy)) +
    geom_point(position='jitter', alpha=.1) + geom_boxplot(aes(shape=shock_label), alpha=.2) +
    facet_grid(effect_level ~ ., scales='free_y') +
    ggtitle(sprintf('Actor Utility Contribution (statistic * theta) by Exogenous Shock\n%s', sim_title_str)) +
    theme_bw()


  return(list(pu=pu, pk=pk, ps=ps))

}


#' @export
saomnk_plot_K_attribute_shocks <- function(env,
                                            K_type = 'K_AC',
                                            new_components = NULL,
                                            plot_type = c("raw", "did", "both")) {

  plot_type <- match.arg(plot_type)

  # Get the K data for new components
  Kdf_new <- env$compute_K_attribute_shocks(K_type = K_type,
                                             new_components = new_components,
                                             verbose = TRUE)

  # Extract new_components from the dataframe if not provided
  if (is.null(new_components)) {
    new_comp_str <- unique(Kdf_new$new_components)[1]
    if (!is.na(new_comp_str) && new_comp_str != "") {
      new_components <- as.numeric(unlist(strsplit(new_comp_str, ",")))
    } else {
      new_components <- which(colSums(env$bipartite_matrix_init) == 0)
    }
  }

  # Identify shock time
  shock_times <- Kdf_new %>%
    filter(treatment_group > 0) %>%
    pull(treatment_group) %>%
    unique()

  if (length(shock_times) == 0) {
    warning("No treatment groups found. Using midpoint of simulation.")
    shock_times <- max(Kdf_new$chain_step_id) / 2
  } else {
    shock_times <- min(shock_times)
  }

  # Prepare data
  plot_data <- Kdf_new %>%
    mutate(
      period = factor(ifelse(chain_step_id < shock_times, "Pre-shock", "Post-shock"), levels = c("Pre-shock", "Post-shock")),
      treatment = ifelse(strategy == "100", "Treated", "Control")
    ) %>%
    group_by(chain_step_id, treatment) %>%
    summarise(
      mean_degree = mean(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = 'drop'
    )

  # Calculate diff-in-diff data
  did_data <- plot_data %>%
    select(chain_step_id, treatment, mean_degree) %>%
    pivot_wider(names_from = treatment, values_from = mean_degree) %>%
    mutate(
      diff = Treated - Control,
      pre_control = ifelse(chain_step_id < shock_times, Control, NA),
      pre_treated = ifelse(chain_step_id < shock_times, Treated, NA)
    )

  pre_diff <- mean(did_data$diff[did_data$chain_step_id < shock_times], na.rm = TRUE)

  did_data <- did_data %>%
    mutate(
      diff_adjusted = diff - pre_diff,
      control_normalized = Control - Control[which.min(abs(chain_step_id - shock_times))],
      treated_normalized = Treated - Control[which.min(abs(chain_step_id - shock_times))]
    )

  if (plot_type == "raw") {
    p <- plot_data %>%
      ggplot(aes(x = chain_step_id, y = mean_degree, color = treatment)) +
      geom_line(size = 1.2) +
      geom_ribbon(aes(ymin = mean_degree - se, ymax = mean_degree + se, fill = treatment),
                  alpha = 0.2, color = NA) +
      geom_vline(xintercept = shock_times, linetype = "dashed", color = "red", size = 1) +
      labs(
        title = sprintf("%s for New Components Only (C%d-C%d)",
                        K_type,
                        min(new_components),
                        max(new_components)),
        x = "Chain Step",
        y = "Average Degree (New Components)",
        color = "Group",
        fill = "Group"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "bottom"
      ) +
      scale_y_continuous(limits = c(0, NA))

  } else if (plot_type == "did") {
    p <- did_data %>%
      pivot_longer(cols = c(control_normalized, treated_normalized),
                   names_to = "group",
                   values_to = "value") %>%
      mutate(group = ifelse(group == "control_normalized", "Control (baseline)", "Treated")) %>%
      ggplot(aes(x = chain_step_id, y = value, color = group)) +
      geom_line(size = 1.2) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
      geom_vline(xintercept = shock_times, linetype = "dashed", color = "red", size = 1) +
      annotate("text", x = shock_times + 10, y = min(did_data$treated_normalized, na.rm = TRUE) * 0.5,
               label = "Treatment effect\n(negative = less exploration)",
               hjust = 0, vjust = 0.5, size = 3) +
      labs(
        title = sprintf("Diff-in-Diff: %s for New Components (C%d-C%d)",
                        K_type,
                        min(new_components),
                        max(new_components)),
        subtitle = "Relative to control group baseline at treatment time",
        x = "Chain Step",
        y = "Degree Difference from Control Baseline",
        color = "Group"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic"),
        legend.position = "bottom"
      )

  } else {  # plot_type == "both"
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      warning("Package 'patchwork' needed for combined plots. Returning raw plot only.")
      plot_type <- "raw"
      return(saomnk_plot_K_attribute_shocks(env, K_type = K_type,
                                             new_components = new_components,
                                             plot_type = "raw"))
    }

    p1 <- plot_data %>%
      ggplot(aes(x = chain_step_id, y = mean_degree, color = treatment)) +
      geom_line(size = 1) +
      geom_vline(xintercept = shock_times, linetype = "dashed", color = "red", size = 0.8) +
      labs(
        title = "Raw Values",
        x = "Chain Step",
        y = "Average Degree",
        color = "Group"
      ) +
      theme_minimal() +
      scale_y_continuous(limits = c(0, NA))

    p2 <- did_data %>%
      ggplot(aes(x = chain_step_id)) +
      geom_line(aes(y = diff_adjusted), size = 1.2, color = "darkblue") +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
      geom_vline(xintercept = shock_times, linetype = "dashed", color = "red", size = 0.8) +
      labs(
        title = "Treatment Effect (Treated - Control)",
        x = "Chain Step",
        y = "Difference in Degrees"
      ) +
      theme_minimal()

    p <- p1 / p2 +
      plot_annotation(
        title = sprintf("%s for New Components Only (C%d-C%d)",
                        K_type, min(new_components), max(new_components)),
        theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      )
  }

  return(list(
    data = Kdf_new,
    plot = p,
    new_components = new_components,
    did_data = if (exists("did_data")) did_data else NULL
  ))
}


#' @export
saomnk_plot_K_AC_NEW_shocks <- function(env, verbose = FALSE) {

  # Get K data for new components
  Kdf_new <- env$compute_K_attribute_shocks(K_type = 'K_AC', verbose = verbose)

  # Get test results for DiD
  test_results <- env$test_shocks_new_components(model_type = 'did', K_type = 'K_AC', verbose = verbose)

  # Extract shock information
  theta_shocks <- env$theta_shocks
  shock_idx <- which(sapply(theta_shocks, function(x) x$shock_on == 1))[1]
  shock_time <- min(theta_shocks[[shock_idx]]$chain_step_ids)

  # Get environment parameters for title
  sim_title_str <- sprintf("Environment: Actors (M) = %d, Components (N) = %d, Init.P. = %.2f",
                           env$M, env$N, env$p_bipartite_init)

  # Check if all values are zero
  all_zero <- all(Kdf_new$value == 0, na.rm = TRUE)
  if (all_zero && verbose) {
    cat("\nNote: All actors have zero connections to new components throughout the simulation.\n")
    cat("This is expected if the subsidy effectively prevents exploration of new activities.\n")
  }

  # Create data for top panel (subsidy effect)
  top_data <- Kdf_new %>%
    filter(strategy == "100") %>%
    group_by(chain_step_id) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')

  # Create data for bottom panel (DiD)
  bottom_data <- Kdf_new %>%
    filter(strategy %in% c("100", "0")) %>%
    group_by(chain_step_id) %>%
    summarise(
      treated = mean(value[strategy == "100"], na.rm = TRUE),
      control = mean(value[strategy == "0"], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(diff = treated - control)

  has_variation <- var(bottom_data$diff, na.rm = TRUE) > 0

  # Top panel
  p_top <- ggplot(top_data, aes(x = chain_step_id, y = value)) +
    geom_rect(aes(xmin = shock_time, xmax = Inf, ymin = -Inf, ymax = Inf),
              fill = "pink", alpha = 0.3) +
    geom_line(color = "cyan3", size = 1) +
    geom_vline(xintercept = shock_time, linetype = "dashed",
               color = "orange2", size = 0.8) +
    geom_hline(yintercept = 0, color = "black", size = 0.3) +
    annotate("text", x = 5, y = max(c(top_data$value, 0.1)) * 0.8,
             label = "subsidy", hjust = 0, vjust = 1, size = 3) +
    labs(title = "K_AC_NEW", x = "", y = "") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    scale_y_continuous(expand = c(0.02, 0), limits = c(0, max(c(top_data$value, 1))))

  # Bottom panel
  y_range <- if (has_variation) range(bottom_data$diff) else c(-0.5, 0.5)

  p_bottom <- ggplot(bottom_data, aes(x = chain_step_id, y = diff)) +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    geom_line(color = "cyan3", size = 1) +
    geom_vline(xintercept = shock_time, linetype = "dashed",
               color = "orange2", size = 0.8) +
    annotate("text", x = 5, y = min(y_range) * 0.8,
             label = "treatment_100__control_0", hjust = 0, vjust = 1, size = 3) +
    labs(title = "K_AC_NEW", x = "", y = "") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    scale_y_continuous(limits = y_range)

  if (!has_variation) {
    p_bottom <- p_bottom +
      annotate("text", x = mean(range(bottom_data$chain_step_id)), y = 0,
               label = "No variation: Treatment and control have identical outcomes",
               hjust = 0.5, vjust = 0.5, size = 3, color = "red", fontface = "italic")
  }

  # Y-axis labels
  y_lab_top <- textGrob("Avg Degree (New Components)",
                        rot = 90, gp = gpar(fontsize = 9))
  y_lab_bottom <- textGrob("Avg Treatment Effect on Treated (ATT)",
                           rot = 90, gp = gpar(fontsize = 9))

  # Combine plots
  plots <- arrangeGrob(
    p_top, p_bottom,
    ncol = 1,
    heights = c(1, 1),
    left = y_lab_top
  )

  # Add main title and subtitle
  title <- textGrob("Degrees to New Components (C9-C16) Only",
                    gp = gpar(fontsize = 12, fontface = "bold"))
  subtitle <- textGrob(sim_title_str,
                       gp = gpar(fontsize = 10))

  # Add x-axis label
  x_label <- textGrob(sprintf("Event Time\n(Shock Starts at Simulated Decision Chain Step %d)",
                              shock_time),
                      gp = gpar(fontsize = 9))

  # Strategy comparison label
  strat_label <- textGrob("Intervention Comparison:    treatment_100__control_0",
                          gp = gpar(fontsize = 9))

  # Final assembly
  final_plot <- arrangeGrob(
    title,
    subtitle,
    plots,
    x_label,
    strat_label,
    ncol = 1,
    heights = c(0.06, 0.04, 0.8, 0.05, 0.05)
  )

  # Return plot and data
  return(list(
    plot = final_plot,
    data = list(
      top = top_data,
      bottom = bottom_data,
      test_results = test_results
    )
  ))
}
