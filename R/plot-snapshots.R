# ============================================================================
# plot-snapshots.R
# Standalone snapshot and degree progress plot functions extracted from
# saomnk-class.R.  Each function takes `env` (an SaoMNK environment) as its
# first argument.  All self$ references have been replaced with env$.
# ============================================================================


#' Plot bipartite snapshots directly from the stored state array
#'
#' Legacy array-based snapshot plotter. Operates on `env$bi_env_arr` and
#' `env$bipartite_matrix_init` rather than delegating to the R6 method, and
#' selects steps by index with an option to prepend the initial state.
#'
#' RENAMED 2026-08-09. This function was previously also called
#' `saomnk_plot_snapshots`, which collided with the API function of that name in
#' `saomnk-api.R`. Both were exported, and because R sources files in
#' alphabetical order the API version silently won: calling
#' `saomnk_plot_snapshots(env, snapshot_ids = ...)` failed with an unused-argument
#' error even though this definition existed. No caller in the package, its
#' vignettes, its papers or the dependent projects used `snapshot_ids`, so
#' renaming is behaviour-preserving.
#'
#' @param env A SaomNK environment.
#' @param snapshot_ids Integer vector of step indices. Defaults to first,
#'   second and last.
#' @param include_init Logical; prepend the initial state as step 0.
#' @return Called for side effects (plot display).
#' @seealso [saomnk_plot_snapshots()] for the R6-delegating API version.
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 4, N = 6, seed = 42)
#' mod <- saomnk_model(density = -0.5, popularity = 0.2)
#' saomnk_run(env, mod, steps_per_actor = 5, seed = 12345)
#'
#' saomnk_plot_snapshots_from_array(env, snapshot_ids = c(1, 10),
#'                                  include_init = FALSE)
#' }
#' @export
saomnk_plot_snapshots_from_array <- function(env, snapshot_ids = c(), include_init = TRUE) {
  if(!length(snapshot_ids))
    snapshot_ids <- c(1, 2, dim(env$bi_env_arr)[3]  )
  if(include_init)
    snapshot_ids <- c(0, snapshot_ids)
  for (i in 1:length(snapshot_ids)) {
    step <- snapshot_ids[ i ]
    mat <- if (step == 0) {
      env$bipartite_matrix_init
    } else {
      env$bi_env_arr[,,step]
    }
    saomnk_plot_bipartite_system_from_mat(env, mat, step)
  }
}


#' @export
saomnk_plot_degree_progress_from_sims <- function(env, K_soc_list, K_env_list, plot_save = FALSE) {

  n <- length(K_soc_list)

  degree_summary <- do.call(rbind, lapply(1:n, function(iter) {
    data.frame(
      Iteration = iter,
      Mean_K_S = mean(K_soc_list[[iter]]),
      Q25_K_S = quantile(K_soc_list[[iter]], 0.25),
      Q75_K_S = quantile(K_soc_list[[iter]], 0.75),
      Mean_K_E = mean(K_env_list[[iter]]),
      Q25_K_E = quantile(K_env_list[[iter]], 0.25),
      Q75_K_E = quantile(K_env_list[[iter]], 0.75)
    )
  }))

  (plt <- ggplot(degree_summary, aes(x = Iteration)) +
      geom_line(aes(y = Mean_K_S, color = "Mean K_S")) +
      geom_ribbon(aes(ymin = Q25_K_S, ymax = Q75_K_S, fill = "K_S"), alpha = 0.1) +
      geom_line(aes(y = Mean_K_E, color = "Mean K_E")) +
      geom_ribbon(aes(ymin = Q25_K_E, ymax = Q75_K_E, fill = "K_E"), alpha = 0.1) +
      scale_color_manual(values = c("Mean K_S" = "blue", "Mean K_E" = "red")) +
      scale_fill_manual(values = c("K_S" = "blue", "K_E" = "red")) +
      labs(title = "Degree Progress Over Iterations",
           x = "Iteration",
           y = "Degree",
           color = "Mean Degree",
           fill = "IQR (Mid-50%)") +
      theme_minimal()
  )

  # Calculate average degree (K) for the social space and component interaction space
  avg_degree_social <- mean(degree_summary$Mean_K_S)
  avg_degree_component <- mean(degree_summary$Mean_K_E)

  if (plot_save) {
    keystring <- sprintf("K_S_%.2f_K_CC_%.2f",
                          avg_degree_social, avg_degree_component)
    ggsave(filename = sprintf('Ks_degree_SAOM-NK_networks_%s_%s.png',
                              keystring, round(as.numeric(Sys.time())*100)),
           plot = plt, height = 4.5, width = 9, units = 'in', dpi = 300)
  }
}


#' @export
saomnk_plot_degree_progress <- function(env, plot_save = FALSE) {
  if (length(env$degree_history_K_S) == 0 || length(env$degree_history_K_E) == 0) {
    stop("No degree history recorded. Run the simulation first.")
  }

  degree_summary <- do.call(rbind, lapply(1:env$ITERATION, function(iter) {
    data.frame(
      Iteration = iter,
      Mean_K_S = mean(env$degree_history_K_S[[iter]]),
      Q25_K_S = quantile(env$degree_history_K_S[[iter]], 0.25),
      Q75_K_S = quantile(env$degree_history_K_S[[iter]], 0.75),
      Mean_K_E = mean(env$degree_history_K_E[[iter]]),
      Q25_K_E = quantile(env$degree_history_K_E[[iter]], 0.25),
      Q75_K_E = quantile(env$degree_history_K_E[[iter]], 0.75)
    )
  }))

  (plt <- ggplot(degree_summary, aes(x = Iteration)) +
    geom_line(aes(y = Mean_K_S, color = "Mean K_S")) +
    geom_ribbon(aes(ymin = Q25_K_S, ymax = Q75_K_S, fill = "K_S"), alpha = 0.1) +
    geom_line(aes(y = Mean_K_E, color = "Mean K_E")) +
    geom_ribbon(aes(ymin = Q25_K_E, ymax = Q75_K_E, fill = "K_E"), alpha = 0.1) +
    scale_color_manual(values = c("Mean K_S" = "blue", "Mean K_E" = "red")) +
    scale_fill_manual(values = c("K_S" = "blue", "K_E" = "red")) +
    labs(title = "Degree Progress Over Iterations",
         x = "Iteration",
         y = "Degree",
         color = "Mean Degree",
         fill = "IQR (Mid-50%)") +
    theme_minimal()
  )

  # Calculate average degree (K) for the social space and component interaction space
  avg_degree_social <- mean(igraph::degree(env$get_social_igraph()))
  avg_degree_component <- mean(igraph::degree(env$get_component_igraph()))

  if (plot_save) {
    keystring <- sprintf("%s_sim%.0f_iter%.0f_N%d_M%d_BI_PROB_%.2f_K_S_%.2f_K_CC_%.2f",
                         env$SIM_NAME, env$TIMESTAMP, env$ITERATION, env$N, env$M, env$BI_PROB, avg_degree_social, avg_degree_component)
    ggsave(filename = sprintf('Ks_degree_SAOM-NK_networks_%s_%s.png', keystring, env$TIMESTAMP),
           plot = plt, height = 4.5, width = 9, units = 'in', dpi = 300)
  }
}


#' @export
saomnk_plot_bipartite_system_from_mat <- function(env,
                                                   bipartite_matrix,
                                                   RSIENA_ITERATION,
                                                   plot_save = FALSE,
                                                   return_plot = TRUE,
                                                   normalize_degree = FALSE,
                                                   initial_bipartite_matrix = NULL) {

  N <- ncol(bipartite_matrix)
  M <- nrow(bipartite_matrix)

  TS <- round(as.numeric(Sys.time()) * 100)

  # Generate labels for components in letter-number sequence
  generate_component_labels <- function(n) {
    letters <- LETTERS  # Uppercase alphabet letters
    if (n <= 26)
      return(letters[1:n])
    labels <- c()
    repeat_count <- ceiling(n / length(letters))
    for (i in 1:repeat_count) {
      labels <- c(labels, paste0(letters, i))
    }
    return(labels[1:n])  # Return only as many as needed
  }

  component_labels <- generate_component_labels(env$N)

  # Determine which components are "new" (initially isolated) vs "old" (initially connected)
  if (!is.null(initial_bipartite_matrix)) {
    component_initial_degrees <- colSums(initial_bipartite_matrix)
    component_is_new <- component_initial_degrees == 0
  } else if (!is.null(env$bipartite_matrix_init)) {
    component_initial_degrees <- colSums(env$bipartite_matrix_init)
    component_is_new <- component_initial_degrees == 0
  } else {
    component_is_new <- rep(FALSE, N)
    warning("No initial bipartite matrix found - unable to determine new vs old components")
  }

  # Get actor strategies for coloring
  actor_strategies <- env$get_actor_strategies()

  actor_colors <- scales::hue_pal()( length(levels(actor_strategies))  )

  # 1. Bipartite network plot using ggraph with vertex labels
  ig_bipartite <- igraph::graph_from_biadjacency_matrix(bipartite_matrix, directed = FALSE, weighted = TRUE)

  projs <- igraph::bipartite_projection(ig_bipartite, multiplicity = TRUE, which = 'both')
  ig_social <- projs$proj1
  ig_component <- projs$proj2

  # Set node attributes for shape and label
  V(ig_bipartite)$shape <- ifelse(V(ig_bipartite)$type, "square", "circle")

  # Create color vector for all vertices
  vertex_colors <- character(vcount(ig_bipartite))

  # Assign actor colors based on strategy (first M vertices)
  vertex_colors[1:M] <- actor_colors

  # Assign component colors based on new/old status (next N vertices)
  component_colors <- ifelse(component_is_new, 'darkgreen', 'tan')
  vertex_colors[(M+1):(M+N)] <- component_colors

  # Assign colors to vertices
  V(ig_bipartite)$color <- vertex_colors

  # Labels
  V(ig_bipartite)$label <- c(as.character(1:env$M), as.character(component_labels))

  # Count new and old components for subtitle
  n_new <- sum(component_is_new)
  n_old <- sum(!component_is_new)

  # Create subtitle with strategy info
  strategy_range_text <- paste(levels(env$get_actor_strategies()), collapse = ', ')

  bipartite_plot <- ggraph(ig_bipartite, layout = "fr") +
    geom_edge_link(color = "gray") +
    geom_node_point(aes(shape = shape, color = color), size = 6) +
    geom_node_text(aes(label = label), vjust = 0.5, hjust = 0.5, size = 3, color = "white") +
    scale_shape_manual(values = c("circle" = 16, "square" = 15)) +
    scale_color_identity() +
    labs(title = "[DGP] Bipartite Environment",
         subtitle = sprintf("Actors (strategy: %s) and Components (%d old, %d new)",
                            strategy_range_text, n_old, n_new)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      plot.margin = margin(5, 5, 5, 5, "pt"),
      plot.subtitle = element_text(size = 9, color = "gray40")
    )
  node_size <- igraph::degree(ig_social)
  node_color <- eigen_centrality(ig_social)$vector
  node_text <- 1:env$M

  social_plot <- ggraph(ig_social, layout = "fr") +
    geom_edge_link(color = "gray") +
    geom_node_point(aes(size = node_size, color = node_color)) +
    geom_node_text(aes(label = node_text), vjust = 0.5, hjust = 0.5, size = 3, color='white') +
    scale_color_gradient(low = "green", high = "red") +
    labs(title = "[Proj1] Actor Social Network\n(common components)",
         color = "Eigenvector\nCentrality", size = "Degree\nCentrality") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    guides(
      color = guide_legend(order = 1, nrow = 2),
      size = guide_legend(order = 2, nrow = 2)
    )

  # 3. Component influence matrix heatmap
  component_matrix <- igraph::as_adjacency_matrix(ig_component, type = 'both', sparse = FALSE, attr = 'weight')

  component_df <- melt(component_matrix)
  colnames(component_df) <- c("Component1", "Component2", "Weight")

  # Replace component numbers with labels in the heatmap data frame
  component_df$Component1 <- factor(component_df$Component1, labels = component_labels)
  component_df$Component2 <- factor(component_df$Component2, labels = component_labels)

  heatmap_plot <- ggplot(component_df, aes(x = Component1, y = forcats::fct_rev(Component2), fill = Weight)) +
    geom_tile() +
    labs(title = "[Proj2] Component Heatmap\n(common actors)", x = "Component 1", y = "Component 2") +
    theme_minimal() +
    theme(legend.position = "bottom")

  if ( sum(component_df$Weight) == 0 ) {
    heatmap_plot <- heatmap_plot + scale_fill_gradient(low = "white", high = "white")
  } else {
    heatmap_plot <- heatmap_plot + scale_fill_gradient(low = "white", high = "red")
  }

  # Calculate average degree (K) for the social space and component interaction space
  avg_degree_social <- mean(igraph::degree(ig_social, mode = 'all', loops = FALSE, normalized = normalize_degree))
  avg_degree_component <- mean(igraph::degree(ig_component, mode = 'all', loops = FALSE, normalized = normalize_degree))
  density_current <- igraph::edge_density(ig_bipartite, loops = FALSE)

  # Create a main title using sprintf with simulation parameters
  main_title <- sprintf("Environment M=%s, N=%s:   Step = %s,  Density = %.2f, K_AA = %.2f, K_CC = %.2f",
                        M, N,
                        RSIENA_ITERATION, density_current, avg_degree_social, avg_degree_component)

  # Arrange plots with a main title
  plt <- grid.arrange(
    social_plot, bipartite_plot, heatmap_plot,
    ncol = 3,
    top = textGrob(main_title, gp = gpar(fontsize = 16, fontface = "bold"))
  )

  if(plot_save){
    keystring <- sprintf("%s_sim%.0f_iter%.0f_N%d_M%d_BI_PROB_%.2f_K_S_%.2f_K_CC_%.2f",
                         env$SIM_NAME, TS, RSIENA_ITERATION, N, M, env$BI_PROB, avg_degree_social, avg_degree_component)
    ggsave(filename = sprintf('3plot_SAOM_NK_networks_%s_%s.png', keystring, TS),
           plot = plt, height = 4.5, width = 9, units = 'in', dpi = 300)
  }

  if(return_plot)
    return(plt)
}
