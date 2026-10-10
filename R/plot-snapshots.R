# ============================================================================
# plot-snapshots.R
# Standalone snapshot and degree progress plot functions extracted from
# saomnk-class.R.  Each function takes `env` (an SAOM-NK environment) as its
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
#' renaming is behavior-preserving.
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
  .searchnet_require_path(env, "saomnk_plot_snapshots_from_array()")
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
      scale_color_manual(values = c("Mean K_S" = "#0072B2", "Mean K_E" = "#D55E00")) +
      scale_fill_manual(values = c("K_S" = "#0072B2", "K_E" = "#D55E00")) +
      labs(title = "Degree Progress Over Iterations",
           x = "Iteration",
           y = "Degree",
           color = "Mean Degree",
           fill = "IQR (Mid-50%)") +
      theme_searchnet()
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
  .searchnet_require_path(env, "saomnk_plot_degree_progress()")
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
    scale_color_manual(values = c("Mean K_S" = "#0072B2", "Mean K_E" = "#D55E00")) +
    scale_fill_manual(values = c("K_S" = "#0072B2", "K_E" = "#D55E00")) +
    labs(title = "Degree Progress Over Iterations",
         x = "Iteration",
         y = "Degree",
         color = "Mean Degree",
         fill = "IQR (Mid-50%)") +
    theme_searchnet()
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

  actor_colors <- searchnet_palette("categorical", length(levels(actor_strategies))  )

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
  component_colors <- ifelse(component_is_new, searchnet_palette()[["green"]], searchnet_palette()[["purple"]])
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
    theme_searchnet(axes = FALSE) +
    theme(legend.position = "none", plot.margin = margin(5, 5, 5, 5, "pt"))
  node_size <- igraph::degree(ig_social)
  node_color <- eigen_centrality(ig_social)$vector
  node_text <- 1:env$M

  social_plot <- ggraph(ig_social, layout = "fr") +
    geom_edge_link(color = "gray") +
    geom_node_point(aes(size = node_size, color = node_color)) +
    geom_node_text(aes(label = node_text), vjust = 0.5, hjust = 0.5, size = 3, color='white') +
    scale_color_viridis_c(option = "viridis", end = 0.9) +
    labs(title = "[Proj1] Actor Social Network\n(common components)",
         color = "Eigenvector\nCentrality", size = "Degree\nCentrality") +
    theme_searchnet(axes = FALSE) +
    theme(legend.box = "vertical") +
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
    theme_searchnet() +
    theme(legend.position = "bottom")

  if ( sum(component_df$Weight) == 0 ) {
    heatmap_plot <- heatmap_plot + scale_fill_gradient(low = "white", high = "white")
  } else {
    heatmap_plot <- heatmap_plot + scale_fill_gradient(low = "white", high = searchnet_palette()[["blue"]])
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


# ---------------------------------------------------------------------------- #
#  Snapshot palettes and panel builder for saomnk_plot_snapshots()
# ---------------------------------------------------------------------------- #

## Okabe-Ito (Okabe and Ito 2008), the colorblind-safe palette the package's
## policy, basin and README plots use. Actor strategy groups take orange, sky
## blue, yellow, reddish purple and black in that order (vermillion is kept
## for events); components take blue (initially unused) and reddish purple
## (initially used), matching the {K} degree panels.
.okabe_ito <- c(orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
                yellow = "#F0E442", blue = "#0072B2", vermillion = "#D55E00",
                purple = "#CC79A7", black = "#000000")

.snapshot_palettes <- c("okabe-ito", "legacy")

## Colors saomnk_plot_snapshots() uses under each palette, for n_strat actor
## strategy groups.
.snapshot_palette <- function(palette, n_strat) {
  if (identical(palette, "legacy")) {
    return(list(
      actors     = scales::hue_pal()(n_strat),
      components = c(new = "darkgreen", old = "tan"),
      social     = list(type = "gradient", low = "green", high = "red"),
      heatmap    = c(low = "white", high = "red")
    ))
  }
  actor_pool <- .okabe_ito[c("orange", "sky", "yellow", "purple", "black")]
  if (n_strat > length(actor_pool))
    stop(sprintf(paste0("palette = \"okabe-ito\" has %d actor colors but there are %d ",
                        "strategy groups; supply node_colors or use palette = \"legacy\"."),
                 length(actor_pool), n_strat), call. = FALSE)
  list(
    actors     = unname(actor_pool[seq_len(n_strat)]),
    components = c(new = .okabe_ito[["blue"]], old = .okabe_ito[["purple"]]),
    social     = list(type = "actor"),
    heatmap    = c(low = "white", high = .okabe_ito[["blue"]])
  )
}

## Build the three snapshot panels (actor projection, bipartite network,
## component projection heatmap) for one bipartite matrix. Returns the three
## ggplots, the arranged grob (not drawn), and the node colors used.
.snapshot_panels <- function(env, mat, step, palette = "okabe-ito",
                             node_colors = NULL) {
  M <- nrow(mat); N <- ncol(mat)
  comp_labels <- if (N <= 26) LETTERS[seq_len(N)] else
    paste0(rep(LETTERS, ceiling(N / 26)),
           rep(seq_len(ceiling(N / 26)), each = 26))[seq_len(N)]

  init <- env$bipartite_matrix_init
  comp_is_new <- if (!is.null(init)) colSums(init) == 0 else rep(FALSE, N)

  strat <- env$get_actor_strategies()
  if (!is.factor(strat)) strat <- factor(strat)
  lev <- levels(strat)
  n_strat <- max(1L, length(lev))

  pal <- .snapshot_palette(palette, n_strat)
  actor_cols <- stats::setNames(pal$actors, if (length(lev)) lev else "none")
  comp_cols  <- pal$components
  if (!is.null(node_colors)) {
    if (!is.character(node_colors) || is.null(names(node_colors)) ||
        any(!nzchar(names(node_colors))))
      stop("node_colors must be a named character vector.", call. = FALSE)
    bad <- setdiff(names(node_colors), c(names(actor_cols), names(comp_cols)))
    if (length(bad))
      stop(sprintf(paste0("node_colors names not recognized: %s. Use actor strategy ",
                          "levels (%s) and/or \"new\", \"old\" for components."),
                   paste(bad, collapse = ", "), paste(names(actor_cols), collapse = ", ")),
           call. = FALSE)
    hit_a <- intersect(names(node_colors), names(actor_cols))
    hit_c <- intersect(names(node_colors), names(comp_cols))
    actor_cols[hit_a] <- node_colors[hit_a]
    comp_cols[hit_c]  <- node_colors[hit_c]
  }

  ## Each actor takes the color of its own strategy level. (The R6 method
  ## recycles the level colors over actor index, which agrees only when
  ## strategies alternate in level order.)
  a_col <- if (length(lev)) unname(actor_cols[as.character(strat)]) else
    rep(unname(actor_cols[[1]]), M)
  c_col <- unname(ifelse(comp_is_new, comp_cols[["new"]], comp_cols[["old"]]))

  ig_bi <- igraph::graph_from_biadjacency_matrix(mat, directed = FALSE, weighted = TRUE)
  projs <- igraph::bipartite_projection(ig_bi, multiplicity = TRUE, which = "both")
  ig_social <- projs$proj1
  ig_comp   <- projs$proj2

  igraph::V(ig_bi)$shape <- ifelse(igraph::V(ig_bi)$type, "square", "circle")
  igraph::V(ig_bi)$color <- c(a_col, c_col)
  igraph::V(ig_bi)$label <- c(as.character(seq_len(M)), comp_labels)

  k_aa <- mean(igraph::degree(ig_social, mode = "all", loops = FALSE))
  k_cc <- mean(igraph::degree(ig_comp, mode = "all", loops = FALSE))
  n_ties <- sum(mat != 0)
  dens <- igraph::edge_density(ig_bi, loops = FALSE)

  ## Plain subtitles: what is drawn, and what the colors mean when they vary.
  bi_sub <- sprintf("%d actors (circles), %d components (squares), %d tie%s", M, N, n_ties, if (n_ties == 1) "" else "s")
  if (length(lev) > 1)
    bi_sub <- paste0(bi_sub, "\nactor color = strategy (", paste(lev, collapse = ", "), ")")
  if (any(comp_is_new) && any(!comp_is_new))
    bi_sub <- paste0(bi_sub, if (length(lev) > 1) "; " else "\n",
                     "darker squares: unused at the start")

  bipartite_plot <- ggraph(ig_bi, layout = "fr") +
    geom_edge_link(color = "grey70") +
    geom_node_point(aes(shape = shape, color = color), size = 6) +
    geom_node_text(aes(label = label), vjust = 0.5, hjust = 0.5, size = 3, color = "white",
                   fontface = "bold") +
    scale_shape_manual(values = c("circle" = 16, "square" = 15)) +
    scale_color_identity() +
    labs(title = "Who holds what", subtitle = bi_sub) +
    theme_searchnet(axes = FALSE) +
    theme(legend.position = "none", plot.margin = margin(5, 5, 5, 5, "pt"))

  node_size  <- igraph::degree(ig_social)
  node_text  <- seq_len(M)
  social_sub <- sprintf("K_AA: %.1f partners per actor\nnode size: number of partners",
                        k_aa)
  if (identical(pal$social$type, "actor")) {
    social_plot <- ggraph(ig_social, layout = "fr") +
      geom_edge_link(color = "grey70") +
      geom_node_point(aes(size = node_size), color = a_col) +
      geom_node_text(aes(label = node_text), vjust = 0.5, hjust = 0.5, size = 3,
                     color = "white", fontface = "bold") +
      scale_size_continuous(range = c(4, 8), guide = "none") +
      labs(title = "Actors linked by a shared component", subtitle = social_sub) +
      theme_searchnet(axes = FALSE)
  } else {
    node_color <- if (igraph::ecount(ig_social) > 0)
      igraph::eigen_centrality(ig_social)$vector else rep(0, igraph::vcount(ig_social))
    social_plot <- ggraph(ig_social, layout = "fr") +
      geom_edge_link(color = "gray") +
      geom_node_point(aes(size = node_size, color = node_color)) +
      geom_node_text(aes(label = node_text), vjust = 0.5, hjust = 0.5, size = 3, color = "white") +
      scale_color_gradient(low = pal$social$low, high = pal$social$high) +
      labs(title = "Actors linked by a shared component", subtitle = social_sub,
           color = "Eigenvector\nCentrality", size = "Degree\nCentrality") +
      theme_searchnet(axes = FALSE) +
      theme(legend.box = "vertical") +
      guides(color = guide_legend(order = 1, nrow = 2),
             size = guide_legend(order = 2, nrow = 2))
  }

  cm <- igraph::as_adjacency_matrix(ig_comp, type = "both", sparse = FALSE, attr = "weight")
  comp_df <- reshape2::melt(cm)
  colnames(comp_df) <- c("Component1", "Component2", "Weight")
  comp_df$Component1 <- factor(comp_df$Component1, labels = comp_labels)
  comp_df$Component2 <- factor(comp_df$Component2, labels = comp_labels)
  comp_df$Component2 <- factor(comp_df$Component2, levels = rev(levels(comp_df$Component2)))
  empty <- sum(comp_df$Weight) == 0
  heat_high <- if (empty) pal$heatmap[["low"]] else pal$heatmap[["high"]]
  heatmap_plot <- ggplot(comp_df, aes(x = Component1, y = Component2, fill = Weight)) +
    geom_tile(color = "grey90", linewidth = 0.2) +
    labs(title = "Components sharing actors",
         subtitle = sprintf("K_CC: %.1f partners per component\ndarker: more actors in common",
                            k_cc),
         x = NULL, y = NULL, fill = "actors in\ncommon") +
    coord_equal() +
    theme_searchnet(grid = FALSE) +
    theme(axis.ticks = element_blank()) +
    scale_fill_gradient(low = pal$heatmap[["low"]], high = heat_high,
                        guide = if (empty) "none" else "colourbar")
  if (empty)
    heatmap_plot <- heatmap_plot +
      annotate("text", x = (N + 1) / 2, y = (N + 1) / 2, size = 3, color = "grey35",
               label = "no two components are\nheld by the same actor yet")

  title <- sprintf("Step %s%s: %d tie%s, density %.2f; mean K_AA %.1f, mean K_CC %.1f",
                   step, if (step == 0) " (start)" else "", n_ties,
                   if (n_ties == 1) "" else "s", dens, k_aa, k_cc)
  grob <- gridExtra::arrangeGrob(
    social_plot, bipartite_plot, heatmap_plot, ncol = 3,
    top = grid::textGrob(title, x = grid::unit(8, "pt"), hjust = 0,
                         gp = grid::gpar(fontsize = 13, fontface = "bold")))
  list(step = step, social = social_plot, bipartite = bipartite_plot,
       heatmap = heatmap_plot, grob = grob,
       colors = list(actors = actor_cols, components = comp_cols))
}
