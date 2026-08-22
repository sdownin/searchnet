# ============================================================================
# plot-markets.R
# Standalone market-related plot functions extracted from saomnk-class.R
# Each function takes `env` (an SaoMNK environment) as its first argument.
# All self$ references have been replaced with env$.
# ============================================================================


#' @export
saomnk_plot_bipartite_ring_markets <- function(env,
                                                step_ids = c(),
                                                component_groups = NULL,
                                                influence_matrix = NULL,
                                                actor_strategies = NULL,
                                                component_labels = NULL,
                                                actor_labels = NULL,
                                                actor_size = 5,
                                                component_size = 12,
                                                edge_alpha = 0.2,
                                                path_arrow_size = 0.15,
                                                path_linewidth = 0.8,
                                                ring_radius = 11,
                                                center_radius = 4,
                                                market_alpha = 0.2,
                                                epistatic_int_mat = NULL) {

  affiliation_array <- env$bi_env_arr

  # Check if the input is a 3D array
  if (!is.array(affiliation_array) || length(dim(affiliation_array)) != 3) {
    stop("Input must be a 3D array with dimensions [M actors, N components, S steps]")
  }

  if (length(step_ids)==0)
    step_ids <- 1:dim(affiliation_array)[3]

  affiliation_array <- affiliation_array[,,step_ids]

  # Get dimensions
  dims <- dim(affiliation_array)
  M <- dims[1]  # Number of actors
  N <- dims[2]  # Number of components
  S <- dims[3]  # Number of steps in decision chain


  if (!is.null(epistatic_int_mat)) {
    warning("`epistatic_int_mat` is deprecated as of searchnet 0.8.2; use ",
            "`influence_matrix` instead. W is the influence matrix, the model ",
            "INPUT; epistasis is the resulting fitness coupling, reported as K_CC.",
            call. = FALSE)
    if (is.null(influence_matrix)) influence_matrix <- epistatic_int_mat
  }
  # Set default influence matrix if not provided
  if (is.null(influence_matrix) && is.null(component_groups) &&
      (is.null(env$markets) || is.null(env$markets$component_groups) )
      ) {
    stop("Either influence_matrix or component_groups must be provided")
  }

  ## use markets$component_groups if set during environment init
  if(is.null(component_groups) & !is.null(env$markets)) {
    component_groups <- env$markets[['component_groups']]
  }

  # If influence_matrix is provided but component_groups is not, generate groups
  if (is.null(component_groups) && !is.null(influence_matrix)) {
    # Convert influence matrix to graph
    g <- graph_from_adjacency_matrix(
      influence_matrix,
      mode = "undirected",
      weighted = TRUE,
      diag = FALSE
    )

    # Set threshold for meaningful interactions
    E(g)$weight[E(g)$weight < 0.3] <- 0
    g <- delete_edges(g, which(E(g)$weight == 0))

    # Community detection to find component groups
    communities <- cluster_louvain(g)

    # Convert membership to list format for many-to-many mapping
    component_groups <- list()
    for (i in 1:max(communities$membership)) {
      component_groups[[i]] <- which(communities$membership == i)
    }
  }

  # Ensure component_groups is in proper format
  if (!is.list(component_groups)) {
    if (is.matrix(component_groups) || is.data.frame(component_groups)) {
      # Assuming binary matrix format [component, group]
      component_groups_list <- list()
      for (g in 1:ncol(component_groups)) {
        component_groups_list[[g]] <- which(component_groups[, g] > 0)
      }
      component_groups <- component_groups_list
    } else {
      # Assuming vector format with one group per component
      component_groups_list <- list()
      for (g in sort(unique(component_groups))) {
        component_groups_list[[length(component_groups_list) + 1]] <- which(component_groups == g)
      }
      component_groups <- component_groups_list
    }
  }

  # Fix component indices to ensure they're within range 1:N
  for (g in 1:length(component_groups)) {
    component_groups[[g]] <- component_groups[[g]][component_groups[[g]] > 0 & component_groups[[g]] <= N]
  }

  # Get the number of groups
  num_groups <- length(component_groups)

  # Create component-to-groups mapping (initializing with empty lists)
  component_to_groups <- vector("list", N)
  for (i in 1:N) {
    component_to_groups[[i]] <- integer(0)
  }

  # Map groups to components (using safer variable names)
  for (group_idx in 1:num_groups) {
    for (comp_idx in component_groups[[group_idx]]) {
      if (comp_idx > 0 && comp_idx <= N) {  # Extra safety check
        component_to_groups[[comp_idx]] <- unique(append(component_to_groups[[comp_idx]], group_idx))
      }
    }
  }

  # Set default labels if not provided
  if (is.null(component_labels)) {
    component_labels <- paste0("C", 1:N)
  }
  if (is.null(actor_labels)) {
    actor_labels <- paste0("A", 1:M)
  }

  # Set default strategies if not provided
  if (is.null(actor_strategies)) {
    actor_strategies <- rep("Default", M)
  }

  # Create positions for components in a ring
  component_angles <- seq(0, 2*pi, length.out = N+1)[1:N]
  component_data <- data.frame(
    id = component_labels,
    x = ring_radius * cos(component_angles),
    y = ring_radius * sin(component_angles),
    angle = component_angles,
    type = "component"
  )

  # Add group memberships to component data (for visualization)
  component_data$primary_group  <- sapply(component_to_groups, function(x) ifelse(length(x) == 1, x[1],  NA ))

  groups_all <- unique(c(unlist(component_to_groups)))
  groups_not_unique <- groups_all[which( ! groups_all %in% unique(component_data$primary_group))]


  group_colors <- RColorBrewer::brewer.pal(num_groups, 'Accent')
  market_regions <- list()

  # For each group, create a convex hull around its components
  for (g in 1:num_groups) {
    # Skip if group is empty
    if (length(component_groups[[g]]) == 0) {
      next
    }

    # Get indices of components in this group (with safety checks)
    group_component_indices <- component_groups[[g]]
    group_component_indices <- group_component_indices[group_component_indices > 0 &
                                                         group_component_indices <= N]

    # Skip if no valid components in this group
    if (length(group_component_indices) == 0) {
      next
    }

    if (length(group_component_indices) < 3) {
      # Need at least 3 points for a polygon
      # Add extra points around each component to create a small region
      extra_points <- data.frame()
      for (idx in group_component_indices) {
        angle <- component_data$angle[idx]
        base_x <- component_data$x[idx]
        base_y <- component_data$y[idx]

        # Add points in a small arc around the component
        for (offset in seq(-pi/3, pi/3, length.out = 5)) {
          extra_points <- rbind(extra_points, data.frame(
            x = base_x + 1.0 * cos(angle + offset),
            y = base_y + 1.0 * sin(angle + offset)
          ))
        }
      }
      points <- rbind(component_data[group_component_indices, c("x", "y")], extra_points)
    } else {
      # Use actual component positions for convex hull
      points <- component_data[group_component_indices, c("x", "y")]
    }

    # Create a buffer around points to make the region larger
    buffer_points <- data.frame()
    for (i in 1:nrow(points)) {
      angle <- atan2(points$y[i], points$x[i])
      buffer_points <- rbind(buffer_points, data.frame(
        x = points$x[i] + 1.5 * cos(angle),
        y = points$y[i] + 1.5 * sin(angle)
      ))
    }

    all_points <- rbind(points, buffer_points)

    # Create a convex hull
    if (nrow(all_points) >= 3) {
      ch <- chull(all_points$x, all_points$y)
      hull <- all_points[c(ch, ch[1]), ]

      market_regions[[g]] <- data.frame(
        x = hull$x,
        y = hull$y,
        group = g
      )
    } else if (nrow(all_points) > 0) {
      # If not enough points for a hull, create a circle
      angles <- seq(0, 2*pi, length.out = 30)
      center_x <- mean(all_points$x)
      center_y <- mean(all_points$y)
      radius <- 1.5

      market_regions[[g]] <- data.frame(
        x = center_x + radius * cos(angles),
        y = center_y + radius * sin(angles),
        group = g
      )
    }
  }

  # Combine market regions into one data frame
  if (length(market_regions) > 0) {
    market_regions_df <- do.call(rbind, market_regions)
    market_regions_df$group <- as.factor(market_regions_df$group)
  } else {
    market_regions_df <- data.frame(
      x = numeric(0),
      y = numeric(0),
      group = factor()
    )
  }

  # Calculate actor positions at each step
  actor_positions <- array(0, dim = c(M, 2, S))

  for (s in 1:S) {
    for (i in 1:M) {
      # Get weights (connections) for this actor at step s
      weights <- affiliation_array[i, , s]

      if (sum(weights) > 0) {
        # Normalize weights
        weights <- weights / sum(weights)

        # Calculate weighted position based on components' circular positions
        weighted_x <- sum(weights * component_data$x)
        weighted_y <- sum(weights * component_data$y)

        # Scale position to be inside the ring but not at the exact center
        dist_from_center <- sqrt(weighted_x^2 + weighted_y^2)

        if (dist_from_center > 0) {
          # Scale to be within center_radius
          scale_factor <- min(1, center_radius / dist_from_center)
          actor_positions[i, 1, s] <- weighted_x * scale_factor
          actor_positions[i, 2, s] <- weighted_y * scale_factor
        } else {
          # If exactly at center, place slightly off-center
          actor_positions[i, 1, s] <- rnorm(1, 0, 0.1)
          actor_positions[i, 2, s] <- rnorm(1, 0, 0.1)
        }
      } else {
        # If no connections, maintain previous position or place near center
        if (s > 1) {
          actor_positions[i, , s] <- actor_positions[i, , s-1]
        } else {
          # Start at a random position near center
          angle <- runif(1, 0, 2*pi)
          actor_positions[i, 1, s] <- center_radius * 0.5 * cos(angle)
          actor_positions[i, 2, s] <- center_radius * 0.5 * sin(angle)
        }
      }
    }
  }

  # Create actor data for final positions (last step)
  actor_data <- data.frame(
    id = actor_labels,
    x = actor_positions[, 1, S],
    y = actor_positions[, 2, S],
    strategy = actor_strategies,
    type = "actor"
  )

  # Combine data
  all_nodes <- rbind(
    component_data %>% select(id, x, y, type),
    actor_data %>% select(id, x, y, type)
  )

  # Create edge data for final state (actor to component)
  edges_list <- list()
  edge_counter <- 1

  for (i in 1:M) {
    for (j in 1:N) {
      if (affiliation_array[i, j, S] > 0) {
        edges_list[[edge_counter]] <- data.frame(
          from = actor_labels[i],
          to = component_labels[j],
          weight = affiliation_array[i, j, S]
        )
        edge_counter <- edge_counter + 1
      }
    }
  }

  if (length(edges_list) > 0) {
    edges <- do.call(rbind, edges_list)

    # Join with node positions
    edges <- edges %>%
      left_join(actor_data %>% select(id, x_from = x, y_from = y),
                by = c("from" = "id")) %>%
      left_join(component_data %>% select(id, x_to = x, y_to = y),
                by = c("to" = "id"))
  } else {
    edges <- data.frame(
      from = character(0),
      to = character(0),
      weight = numeric(0),
      x_from = numeric(0),
      y_from = numeric(0),
      x_to = numeric(0),
      y_to = numeric(0)
    )
  }

  # Create path data
  paths_list <- list()
  path_counter <- 1

  for (i in 1:M) {
    for (s in 1:(S-1)) {
      # Only create path segments if there's actual movement
      if (actor_positions[i, 1, s] != actor_positions[i, 1, s+1] ||
          actor_positions[i, 2, s] != actor_positions[i, 2, s+1]) {

        paths_list[[path_counter]] <- data.frame(
          actor = actor_labels[i],
          strategy = actor_strategies[i],
          x = actor_positions[i, 1, s],
          y = actor_positions[i, 2, s],
          xend = actor_positions[i, 1, s+1],
          yend = actor_positions[i, 2, s+1],
          step = s
        )
        path_counter <- path_counter + 1
      }
    }
  }

  if (length(paths_list) > 0) {
    paths <- do.call(rbind, paths_list)
  } else {
    paths <- data.frame(
      actor = character(0),
      strategy = character(0),
      x = numeric(0),
      y = numeric(0),
      xend = numeric(0),
      yend = numeric(0),
      step = numeric(0)
    )
  }

  # Generate a palette for strategies
  strategy_colors <- scales::hue_pal()(length(unique(actor_strategies)))
  names(strategy_colors) <- rev(unique(actor_strategies))

  # Create multi-membership visualization for components
  # For components that belong to multiple groups
  pie_data <- data.frame(
    id = character(0),
    x = numeric(0),
    y = numeric(0),
    group = numeric(0),
    start = numeric(0),
    end = numeric(0)
  )

  # Try to create pie chart segments for components with multiple groups
  for (i in 1:N) {
    groups <- component_to_groups[[i]]
    if (length(groups) > 1) {  # Only for components with multiple groups
      # Create pie chart segments
      segment_size <- 2 * pi / length(groups)
      for (j in 1:length(groups)) {
        start_angle <- (j - 1) * segment_size
        end_angle <- j * segment_size

        pie_data <- rbind(pie_data, data.frame(
          id = component_labels[i],
          x = component_data$x[i],
          y = component_data$y[i],
          group = groups[j],
          start = start_angle,
          end = end_angle
        ))
      }
    }
  }

  # Create the plot
  p <- ggplot() +
    # Draw market regions with translucency to show overlaps
    geom_polygon(data = market_regions_df,
                 aes(x = x, y = y, group = group, fill = group),
                 alpha = market_alpha) +
    # Draw edges for final state (actor to component)
    geom_segment(data = edges,
                 aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                     alpha = weight),
                 color = "gray70") +
    # Draw actor paths with arrows
    geom_segment(data = paths,
                 aes(x = x, y = y, xend = xend, yend = yend,
                     color = strategy),
                 arrow = arrow(type = "closed",
                               length = unit(path_arrow_size, "inches")),
                 linewidth = path_linewidth) +
    # Draw component nodes
    geom_point(data = component_data,
               aes(x = x, y = y, fill = as.factor(primary_group)),
               size = component_size,
               shape = 22,
               alpha = .35,
               color = "black") +
    # Draw actor nodes
    geom_point(data = actor_data,
               aes(x = x, y = y, color = strategy),
               shape = 1,
               size = actor_size * 3) +
    # Add labels
    geom_text(data = component_data,
              aes(x = x * 1.1, y = y * 1.1, label = id),
              size = 3) +
    geom_text(data = actor_data,
              aes(x = x, y = y, label = id),
              vjust = -1.5, size = 3) +
    # Set colors, sizes, and scales
    scale_color_manual(values = strategy_colors, name = "Strategy") +
    scale_fill_manual(values = group_colors, name = "Market", na.value = NA, na.translate = FALSE) +
    scale_alpha_continuous(range = c(0.1, 0.8), name = "Weight") +
    # Set proper axis labels and scales
    coord_equal() +
    xlim(-ring_radius * 1.2, ring_radius * 1.2) +
    ylim(-ring_radius * 1.2, ring_radius * 1.2) +
    labs(title = "Market Entry and Repositioning: Actors and Components",
         subtitle = paste0(M, " actors and ", N, " components with decision paths (", S, " steps)")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())

  # If ggforce is available, add pie charts for multi-group components
  if (requireNamespace("ggforce", quietly = TRUE) && nrow(pie_data) > 0) {
    p <- p + ggforce::geom_arc_bar(data = pie_data,
                                   aes(x0 = x, y0 = y, r0 = 0, r = component_size / 24,
                                       start = start, end = end, fill = as.factor(group)),
                                   color = NA, size = 0.25)
  } else if (nrow(pie_data) > 0) {
    message("Package 'ggforce' not available. Component pie charts will not be displayed.")
  }

  return(p)
}


#' @export
saomnk_plot_bipartite_ring_markets_animation <- function(env,
                                                          step_ids = c(),
                                                          component_groups = NULL,
                                                          influence_matrix = NULL,
                                                          actor_strategies = NULL,
                                                          component_labels = NULL,
                                                          actor_labels = NULL,
                                                          actor_size = 5,
                                                          component_size = 12,
                                                          edge_alpha = 0.2,
                                                          path_arrow_size = 0.15,
                                                          path_linewidth = 0.8,
                                                          ring_radius = 11,
                                                          center_radius = 4,
                                                          market_alpha = 0.2,
                                                          animation_fps = 10,
                                                          animation_duration = 10,
                                                          epistatic_int_mat = NULL) {

  # Load required libraries if not already loaded
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    stop("Package 'gganimate' is required for animation. Please install it.")
  }

  affiliation_array <- env$bi_env_arr

  # Check if the input is a 3D array
  if (!is.array(affiliation_array) || length(dim(affiliation_array)) != 3) {
    stop("Input must be a 3D array with dimensions [M actors, N components, S steps]")
  }

  if (length(step_ids)==0)
    step_ids <- 1:dim(affiliation_array)[3]

  affiliation_array <- affiliation_array[,,step_ids]

  # Get dimensions
  dims <- dim(affiliation_array)
  M <- dims[1]  # Number of actors
  N <- dims[2]  # Number of components
  S <- dims[3]  # Number of steps in decision chain


  if (!is.null(epistatic_int_mat)) {
    warning("`epistatic_int_mat` is deprecated as of searchnet 0.8.2; use ",
            "`influence_matrix` instead. W is the influence matrix, the model ",
            "INPUT; epistasis is the resulting fitness coupling, reported as K_CC.",
            call. = FALSE)
    if (is.null(influence_matrix)) influence_matrix <- epistatic_int_mat
  }
  # Set default influence matrix if not provided
  if (is.null(influence_matrix) && is.null(component_groups) && !is.null(env$markets)) {
    component_groups <- env$markets$component_groups
  }

  # If influence_matrix is provided but component_groups is not, generate groups
  if (is.null(component_groups) && !is.null(influence_matrix)) {
    # Convert influence matrix to graph
    g <- graph_from_adjacency_matrix(
      influence_matrix,
      mode = "undirected",
      weighted = TRUE,
      diag = FALSE
    )

    # Set threshold for meaningful interactions
    E(g)$weight[E(g)$weight < 0.4] <- 0
    g <- delete_edges(g, which(E(g)$weight == 0))

    # Community detection to find component groups
    communities <- cluster_louvain(g)

    # Convert membership to list format for many-to-many mapping
    component_groups <- list()
    for (i in 1:max(communities$membership)) {
      component_groups[[i]] <- which(communities$membership == i)
    }
  }

  # Ensure component_groups is in proper format
  if (!is.list(component_groups)) {
    if (is.matrix(component_groups) || is.data.frame(component_groups)) {
      # Assuming binary matrix format [component, group]
      component_groups_list <- list()
      for (g in 1:ncol(component_groups)) {
        component_groups_list[[g]] <- which(component_groups[, g] > 0)
      }
      component_groups <- component_groups_list
    } else {
      # Assuming vector format with one group per component
      component_groups_list <- list()
      for (g in sort(unique(component_groups))) {
        component_groups_list[[length(component_groups_list) + 1]] <- which(component_groups == g)
      }
      component_groups <- component_groups_list
    }
  }

  # Fix component indices to ensure they're within range 1:N
  for (g in 1:length(component_groups)) {
    component_groups[[g]] <- component_groups[[g]][component_groups[[g]] > 0 & component_groups[[g]] <= N]
  }

  # Get the number of groups
  num_groups <- length(component_groups)


  # Create component-to-groups mapping (initializing with empty lists)
  component_to_groups <- vector("list", N)
  for (i in 1:N) {
    component_to_groups[[i]] <- integer(0)
  }

  # Map groups to components (using safer variable names)
  for (group_idx in 1:num_groups) {
    for (comp_idx in component_groups[[group_idx]]) {
      if (comp_idx > 0 && comp_idx <= N) {  # Extra safety check
        component_to_groups[[comp_idx]] <- unique(append(component_to_groups[[comp_idx]], group_idx))
      }
    }
  }

  # Set default labels if not provided
  if (is.null(component_labels)) {
    component_labels <- paste0("C", 1:N)
  }
  if (is.null(actor_labels)) {
    actor_labels <- paste0("A", 1:M)
  }

  # Set default strategies if not provided
  if (is.null(actor_strategies)) {
    actor_strategies <- rep("Default", M)
  }

  # Create positions for components in a ring
  component_angles <- seq(0, 2*pi, length.out = N+1)[1:N]
  component_data <- data.frame(
    id = component_labels,
    x = ring_radius * cos(component_angles),
    y = ring_radius * sin(component_angles),
    angle = component_angles,
    type = "component"
  )

  # Add group memberships to component data (for visualization)
  component_data$primary_group <- sapply(component_to_groups, function(x) ifelse(length(x) == 1, x[1], NA))

  # Generate unique colors for each group
  group_colors <- RColorBrewer::brewer.pal(num_groups, 'Accent')

  # Create market region data (one region per group)
  market_regions <- list()

  # For each group, create a convex hull around its components
  for (g in 1:num_groups) {
    # Skip if group is empty
    if (length(component_groups[[g]]) == 0) {
      next
    }

    # Get indices of components in this group (with safety checks)
    group_component_indices <- component_groups[[g]]
    group_component_indices <- group_component_indices[group_component_indices > 0 &
                                                         group_component_indices <= N]

    # Skip if no valid components in this group
    if (length(group_component_indices) == 0) {
      next
    }

    if (length(group_component_indices) < 3) {
      extra_points <- data.frame()
      for (idx in group_component_indices) {
        angle <- component_data$angle[idx]
        base_x <- component_data$x[idx]
        base_y <- component_data$y[idx]

        for (offset in seq(-pi/3, pi/3, length.out = 5)) {
          extra_points <- rbind(extra_points, data.frame(
            x = base_x + 1.0 * cos(angle + offset),
            y = base_y + 1.0 * sin(angle + offset)
          ))
        }
      }
      points <- rbind(component_data[group_component_indices, c("x", "y")], extra_points)
    } else {
      points <- component_data[group_component_indices, c("x", "y")]
    }

    # Create a buffer around points to make the region larger
    buffer_points <- data.frame()
    for (i in 1:nrow(points)) {
      angle <- atan2(points$y[i], points$x[i])
      buffer_points <- rbind(buffer_points, data.frame(
        x = points$x[i] + 1.5 * cos(angle),
        y = points$y[i] + 1.5 * sin(angle)
      ))
    }

    all_points <- rbind(points, buffer_points)

    if (nrow(all_points) >= 3) {
      ch <- chull(all_points$x, all_points$y)
      hull <- all_points[c(ch, ch[1]), ]

      market_regions[[g]] <- data.frame(
        x = hull$x,
        y = hull$y,
        group = g
      )
    } else if (nrow(all_points) > 0) {
      angles <- seq(0, 2*pi, length.out = 30)
      center_x <- mean(all_points$x)
      center_y <- mean(all_points$y)
      radius <- 1.5

      market_regions[[g]] <- data.frame(
        x = center_x + radius * cos(angles),
        y = center_y + radius * sin(angles),
        group = g
      )
    }
  }

  # Combine market regions into one data frame
  if (length(market_regions) > 0) {
    market_regions_df <- do.call(rbind, market_regions)
    market_regions_df$group <- as.factor(market_regions_df$group)
  } else {
    market_regions_df <- data.frame(
      x = numeric(0),
      y = numeric(0),
      group = factor()
    )
  }

  # ANIMATION-SPECIFIC CODE STARTS HERE

  all_actor_positions <- data.frame()
  all_edges <- data.frame()

  # Create a helper data frame for the step counter
  step_counter <- data.frame(
    x = -ring_radius * 1.1,
    y = ring_radius * 1.1,
    step = 1:S
  )

  for (s in 1:S) {
    for (i in 1:M) {
      weights <- affiliation_array[i, , s]

      x_pos <- 0
      y_pos <- 0

      if (sum(weights) > 0) {
        weights <- weights / sum(weights)

        weighted_x <- sum(weights * component_data$x)
        weighted_y <- sum(weights * component_data$y)

        dist_from_center <- sqrt(weighted_x^2 + weighted_y^2)

        if (dist_from_center > 0) {
          scale_factor <- min(1, center_radius / dist_from_center)
          x_pos <- weighted_x * scale_factor
          y_pos <- weighted_y * scale_factor
        } else {
          x_pos <- rnorm(1, 0, 0.1)
          y_pos <- rnorm(1, 0, 0.1)
        }
      } else {
        if (s > 1) {
          prev_pos <- all_actor_positions %>%
            filter(actor_id == actor_labels[i], step == s-1)
          if (nrow(prev_pos) > 0) {
            x_pos <- prev_pos$x
            y_pos <- prev_pos$y
          } else {
            angle <- runif(1, 0, 2*pi)
            x_pos <- center_radius * 0.5 * cos(angle)
            y_pos <- center_radius * 0.5 * sin(angle)
          }
        } else {
          angle <- runif(1, 0, 2*pi)
          x_pos <- center_radius * 0.5 * cos(angle)
          y_pos <- center_radius * 0.5 * sin(angle)
        }
      }

      all_actor_positions <- rbind(all_actor_positions, data.frame(
        actor_id = actor_labels[i],
        strategy = actor_strategies[i],
        x = x_pos,
        y = y_pos,
        step = s
      ))

      for (j in 1:N) {
        if (affiliation_array[i, j, s] > 0) {
          all_edges <- rbind(all_edges, data.frame(
            from = actor_labels[i],
            to = component_labels[j],
            weight = affiliation_array[i, j, s],
            step = s
          ))
        }
      }
    }
  }

  # Join edges with positions
  all_edges <- all_edges %>%
    left_join(all_actor_positions %>% select(actor_id, x_from = x, y_from = y, step),
              by = c("from" = "actor_id", "step" = "step")) %>%
    left_join(component_data %>% select(id, x_to = x, y_to = y),
              by = c("to" = "id"))

  # Create path data for animation
  all_paths <- data.frame()

  for (i in 1:M) {
    for (s in 1:(S-1)) {
      actor_start <- all_actor_positions %>%
        filter(actor_id == actor_labels[i], step == s)

      actor_end <- all_actor_positions %>%
        filter(actor_id == actor_labels[i], step == s+1)

      if (nrow(actor_start) > 0 && nrow(actor_end) > 0) {
        if (actor_start$x != actor_end$x || actor_start$y != actor_end$y) {
          all_paths <- rbind(all_paths, data.frame(
            actor = actor_labels[i],
            strategy = actor_strategies[i],
            x = actor_start$x,
            y = actor_start$y,
            xend = actor_end$x,
            yend = actor_end$y,
            step = s,
            show_at_step = s+1
          ))
        }
      }
    }
  }

  # Prepare data for gradual path visualization
  path_animation_data <- data.frame()

  for (s in 1:S) {
    visible_paths <- all_paths %>% filter(show_at_step <= s)

    if (nrow(visible_paths) > 0) {
      visible_paths$current_step <- s
      path_animation_data <- rbind(path_animation_data, visible_paths)
    }
  }


  strategy_colors <- scales::hue_pal()(length(unique(actor_strategies)))
  names(strategy_colors) <- unique(actor_strategies)


  # Create multi-membership visualization for components
  pie_data <- data.frame(
    id = character(0),
    x = numeric(0),
    y = numeric(0),
    group = numeric(0),
    start = numeric(0),
    end = numeric(0)
  )

  for (i in 1:N) {
    groups <- component_to_groups[[i]]
    if (length(groups) > 1) {
      segment_size <- 2 * pi / length(groups)
      for (j in 1:length(groups)) {
        start_angle <- (j - 1) * segment_size
        end_angle <- j * segment_size

        pie_data <- rbind(pie_data, data.frame(
          id = component_labels[i],
          x = component_data$x[i],
          y = component_data$y[i],
          group = groups[j],
          start = start_angle,
          end = end_angle
        ))
      }
    }
  }

  # Create the plot with animation
  p <- ggplot() +
    geom_polygon(data = market_regions_df,
                 aes(x = x, y = y, group = group, fill = group),
                 alpha = market_alpha) +
    geom_point(data = component_data,
               aes(x = x, y = y, fill = as.factor(primary_group)),
               size = component_size,
               shape = 22,
               alpha = .35,
               color = "black") +
    geom_text(data = component_data,
              aes(x = x * 1.1, y = y * 1.1, label = id),
              size = 3) +
    geom_segment(data = all_edges,
                 aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                     alpha = weight, group = paste(from, to)),
                 color = "gray70") +
    geom_segment(data = all_paths,
                 aes(x = x, y = y, xend = xend, yend = yend, color = strategy,
                     group = paste(actor, step)),
                 arrow = arrow(type = "closed", length = unit(path_arrow_size, "inches")),
                 linewidth = path_linewidth) +
    geom_point(data = all_actor_positions,
               aes(x = x, y = y, color = strategy, group = actor_id),
               shape = 1,
               size = actor_size * 3) +
    geom_text(data = all_actor_positions,
              aes(x = x, y = y, label = actor_id, group = actor_id),
              vjust = -1.5, size = 3) +
    scale_color_manual(values = strategy_colors, name = "Strategy") +
    scale_fill_manual(values = group_colors, name = "Market", na.value = NA, na.translate = FALSE) +
    scale_alpha_continuous(range = c(0.1, 0.8), name = "Weight") +
    coord_equal() +
    xlim(-ring_radius * 1.2, ring_radius * 1.2) +
    ylim(-ring_radius * 1.2, ring_radius * 1.2) +
    labs(title = "Market Entry and Repositioning: Actors and Components",
         subtitle = "Step: {closest_state}") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())

  # If ggforce is available, add pie charts for multi-group components
  if (requireNamespace("ggforce", quietly = TRUE) && nrow(pie_data) > 0) {
    p <- p + ggforce::geom_arc_bar(data = pie_data,
                                   aes(x0 = x, y0 = y, r0 = 0, r = component_size / 24,
                                       start = start, end = end, fill = as.factor(group)),
                                   color = NA, size = 0.25)
  } else if (nrow(pie_data) > 0) {
    message("Package 'ggforce' not available. Component pie charts will not be displayed.")
  }


  # Add animation elements
  anim <- p +
    geom_point(data = all_actor_positions,
               aes(x = x, y = y, color = strategy, group = actor_id),
               shape = 1,
               size = actor_size * 3) +
    geom_text(data = all_actor_positions,
              aes(x = x, y = y, label = actor_id, group = actor_id),
              vjust = -1.5, size = 3) +
    geom_segment(data = all_edges,
                 aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                     alpha = weight, group = paste(from, to, step)),
                 color = rgb(.5,.5,.5, .4) ) +
    geom_segment(data = path_animation_data,
                 aes(x = x, y = y, xend = xend, yend = yend,
                     color = strategy, group = paste(actor, step)),
                 arrow = arrow(type = "closed", length = unit(path_arrow_size, "inches")),
                 linewidth = path_linewidth) +
    gganimate::transition_states(
      states = step,
      transition_length = 2,
      state_length = 3
    ) +
    gganimate::ease_aes('linear')

  # Render the animation
  animated_plot <- gganimate::animate(
    anim,
    nframes = S * 3,
    fps = animation_fps,
    duration = animation_duration,
    width = 800,
    height = 800,
    renderer = gganimate::gifski_renderer()
  )

  return(animated_plot)
}


#' @export
saomnk_plot_market_entry_survival <- function(env,
                                               experiment_name = 'market_entry',
                                               cumulative = FALSE,
                                               conf_level = 0.95,
                                               return_data = FALSE) {
  if (is.null(env$experiments) || is.null(env$experiments[[experiment_name]]))
    stop('market_entry experiment not available. First call mcsim_market_entry()')

  # sim results
  sim_results <- env$experiments[[experiment_name]]

  # Extract data from simulation results
  survival_data <- sim_results$survival_data
  entry_df <- sim_results$entry_df
  first_entries <- sim_results$first_entries
  all_actors <- sim_results$all_actors
  max_step <- sim_results$max_step
  total_sims <- sim_results$total_sims

  if (!length(survival_data) || !nrow(survival_data)) {
    cat('\nNo Entries to plot.\n')
    return(sim_results)
  }

  if ( ! cumulative ) {

   survival_data$survival_rate <-   1 - survival_data$entry_rate
   survival_data$ci_lower_entry <-  1 - survival_data$ci_lower_entry
   survival_data$ci_upper_entry <-  1 - survival_data$ci_upper_entry
   # Create the cumulative entry plot
   plot <- ggplot2::ggplot(survival_data,
                           ggplot2::aes(x = step, y = survival_rate,
                                        color = actor, group = actor)) +
     ggplot2::geom_step(size = 1) +
     ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry,
                                       fill = actor), alpha = 0.2, color = NA) +
     ggplot2::labs(
       x = "Chain Step ID",
       y = "Proportion Not Yet Entered Market (Survival Rate)",
       title = "Market Entry Survival Curves by Actor",
       subtitle = paste0(conf_level * 100, "% Confidence Intervals")
     ) +
     ggplot2::scale_y_continuous(
       labels = scales::percent_format(),
       limits = c(0, 1),
       breaks = seq(0, 1, by = 0.25)
     ) +
     ggplot2::scale_color_brewer(palette = "Set1", name = "Actor ID") +
     ggplot2::scale_fill_brewer(palette = "Set1", name = "Actor ID") +
     ggplot2::theme_minimal() +
     ggplot2::theme(
       legend.position = "right",
       panel.grid.minor = ggplot2::element_blank(),
       plot.title = ggplot2::element_text(face = "bold")
     )

 } else {

   # Create the cumulative entry plot
   plot <- ggplot2::ggplot(survival_data,
                           ggplot2::aes(x = step, y = entry_rate,
                                        color = actor, group = actor)) +
     ggplot2::geom_step(size = 1) +
     ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry,
                                       fill = actor), alpha = 0.2, color = NA) +
     ggplot2::labs(
       x = "Chain Step ID",
       y = "Proportion Entered Market",
       title = "Cumulative Market Entry Curves by Actor",
       subtitle = paste0(conf_level * 100, "% Confidence Intervals")
     ) +
     ggplot2::scale_y_continuous(
       labels = scales::percent_format(),
       limits = c(0, 1),
       breaks = seq(0, 1, by = 0.25)
     ) +
     ggplot2::scale_color_brewer(palette = "Set1", name = "Actor ID") +
     ggplot2::scale_fill_brewer(palette = "Set1", name = "Actor ID") +
     ggplot2::theme_minimal() +
     ggplot2::theme(
       legend.position = "right",
       panel.grid.minor = ggplot2::element_blank(),
       plot.title = ggplot2::element_text(face = "bold")
     )


 }

  print(plot)

  # Return list with data and plot
  if(return_data)
    return(list(
      survival_data = survival_data,
      plot = plot
    ))
}


#' @export
saomnk_plot_market_entry_survival_strategy <- function(env,
                                                        experiment_name = 'market_entry',
                                                        actor_curves = FALSE,
                                                        cumulative = FALSE,
                                                        conf_level = 0.95,
                                                        return_data = FALSE) {
  if (is.null(env$experiments) || is.null(env$experiments[[experiment_name]]))
    stop('market_entry experiment not available. First call mcsim_market_entry()')

  # sim results
  sim_results <- env$experiments[[experiment_name]]

  # Extract data from simulation results
  survival_data <- sim_results$survival_data
  entry_df <- sim_results$entry_df
  first_entries <- sim_results$first_entries
  all_actors <- sim_results$all_actors
  max_step <- sim_results$max_step
  total_sims <- sim_results$total_sims


  if (!length(survival_data) || !nrow(survival_data)) {
    cat('\nNo Entries to plot.\n')
    return(sim_results)
  }


  if ( ! cumulative ) {

    survival_data$survival_rate <-   1 - survival_data$entry_rate
    survival_data$ci_lower_entry <-  1 - survival_data$ci_lower_entry
    survival_data$ci_upper_entry <-  1 - survival_data$ci_upper_entry

    if(actor_curves) {
      plot <- ggplot2::ggplot(survival_data,
                              ggplot2::aes(x = step, y = survival_rate,
                                           color = strategy, linetype = actor))
    } else {
      survival_data <- survival_data %>% group_by(step, strategy) %>%
        dplyr::summarize(
          survival_rate = mean(survival_rate, na.rm=TRUE),
          entry_rate = mean(entry_rate, na.rm=TRUE),
          ci_lower_entry = mean(ci_lower_entry, na.rm=TRUE),
          ci_upper_entry = mean(ci_upper_entry, na.rm=TRUE)
        )
      plot <- ggplot2::ggplot(survival_data,
                              ggplot2::aes(x = step, y = survival_rate,
                                           color = strategy))
    }

    plot <- plot +
      ggplot2::geom_step(size = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry,
                                        fill = strategy), alpha = 0.2, color = NA) +
      ggplot2::labs(
        x = "Chain Step ID",
        y = "Proportion Not Yet Entered Market (Survival Rate)",
        title = sprintf("Market Entry Survival Curves by Strategy%s", ifelse(actor_curves,'',' (Averaged)') ),
        subtitle = paste0(conf_level * 100, "% Confidence Intervals")
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(),
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.25)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "right",
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold")
      )

  } else {

    if(actor_curves) {
      plot <- ggplot2::ggplot(survival_data,
                              ggplot2::aes(x = step, y = entry_rate,
                                           color = strategy, linetype = actor))
    } else {
      survival_data <- survival_data %>% group_by(step, strategy) %>%
        dplyr::summarize(
          survival_rate = mean(survival_rate, na.rm=TRUE),
          entry_rate = mean(entry_rate, na.rm=TRUE),
          ci_lower_entry = mean(ci_lower_entry, na.rm=TRUE),
          ci_upper_entry = mean(ci_upper_entry, na.rm=TRUE)
        )
      plot <- ggplot2::ggplot(survival_data,
                              ggplot2::aes(x = step, y = entry_rate,
                                           color = strategy))
    }

    plot <- plot +
      ggplot2::geom_step(size = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry,
                                        fill = strategy), alpha = 0.2, color = NA) +
      ggplot2::labs(
        x = "Chain Step ID",
        y = "Proportion Entered Market",
        title = sprintf("Cumulative Entry Curves by Strategy%s", ifelse(actor_curves,'',' (Averaged)') ),
        subtitle = paste0(conf_level * 100, "% Confidence Intervals")
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(),
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.25)
      ) +
      ggplot2::scale_color_brewer(palette = "Set1", name = "Actor ID") +
      ggplot2::scale_fill_brewer(palette = "Set1", name = "Actor ID") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "right",
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold")
      )

  }

  print(plot)

  # Return list with data and plot
  if(return_data)
    return(list(
      survival_data = survival_data,
      plot = plot
    ))
}


#' @export
saomnk_plot_market_entry_survival_v0 <- function(env,
                                                  n = 50,
                                                  environ_params = NULL,
                                                  structure_model = NULL,
                                                  steps_per_actor = NULL,
                                                  theta_shocks = NULL,
                                                  conf_level = 0.95,
                                                  verbose = FALSE) {
  environ_params <- if (!is.null(env$config_environ_params)){
    env$config_environ_params
  } else if (!is.null(environ_params)) {
    environ_params
  } else {
    stop('missing environ_params and env$config_environ_params')
  }

  structure_model <- if (!is.null(env$config_structure_model)){
    env$config_structure_model
  } else if (!is.null(structure_model)) {
    structure_model
  } else {
    stop('missing structure_model and env$config_structure_model')
  }


  # Initialize lists
  util_list <- list()
  entry_list <- list()

  batch_seeds <- sample(1:9999999, size = n, replace = FALSE)


  # Run simulations with multiple seeds
  for (i in 1:length(batch_seeds)) {
    run_seed_i <- batch_seeds[i]

    cat(sprintf('\n run %s, seed=%s \n', i, run_seed_i))

    # Run RSiena search using variable parameters in theta_shocks
    env_i <- env$clone(deep = TRUE)

    env_i$search_rsiena(
      structure_model = structure_model,
      iterations_per_actor = steps_per_actor,
      theta_shocks = theta_shocks,
      run_seed = run_seed_i,
      verbose = verbose
    )

    util_list[[as.character(run_seed_i)]] <- env_i$actor_util_df

    # Process network data
    arr <- env_i$bi_env_arr[,,]

    # Convert the 3D array to indices where value is 1
    indices1 <- which(arr == 1, arr.ind = TRUE)
    colnames(indices1) <- c('from', 'to', 'step')

    # Find ties in new markets (entries)
    idx_new <- which(indices1[,2] %in% c(10:12))

    # Store entry data
    if(length(idx_new) > 0) {
      entry_list[[as.character(run_seed_i)]] <- data.frame(
        from = indices1[idx_new, 1],
        to = indices1[idx_new, 2],
        chain_step_id = indices1[idx_new, 3],
        sim_seed = env_i$rsiena_env_seed
      )
    }
  }


  # Combine all results
  util_df <- data.table::rbindlist(util_list, idcol = 'run_seed')
  entry_df <- data.table::rbindlist(entry_list, idcol = 'run_seed')

  # Get all unique actors and maximum step
  all_actors <- unique(entry_df$from)
  max_step <- max(entry_df$chain_step_id)

  # Count total simulations
  total_sims <- length(batch_seeds)

  # Find first entry time for each actor in each simulation
  first_entries <- entry_df %>%
    dplyr::group_by(run_seed, from) %>%
    dplyr::summarize(
      first_entry_step = min(chain_step_id),
      .groups = "drop"
    )

  # Initialize empty data frame for survival data
  survival_data <- data.frame()

  # Calculate z-value outside all loops
  z_value <- qnorm(1 - (1 - conf_level) / 2)

  # Process each actor separately
  for (actor_id in all_actors) {
    # Get entries for this actor
    actor_entries <- subset(first_entries, from == actor_id)

    # Count actor's simulations
    actor_sim_count <- length(unique(actor_entries$run_seed))

    # For each step, calculate survival and entry rates
    for (s in 0:max_step) {
      if (s == 0) {
        surviving_sims <- total_sims
      } else {
        entered_after_s <- sum(actor_entries$first_entry_step > s)
        never_entered <- total_sims - actor_sim_count
        surviving_sims <- entered_after_s + never_entered
      }

      survival_rate <- surviving_sims / total_sims
      entry_rate <- 1 - survival_rate

      se_entry <- sqrt(entry_rate * (1 - entry_rate) / total_sims)

      ci_lower_entry <- max(0, entry_rate - z_value * se_entry)
      ci_upper_entry <- min(1, entry_rate + z_value * se_entry)

      row_data <- data.frame(
        step = s,
        actor = actor_id,
        total_sims = total_sims,
        surviving_sims = surviving_sims,
        survival_rate = survival_rate,
        entry_rate = entry_rate,
        ci_lower_entry = ci_lower_entry,
        ci_upper_entry = ci_upper_entry
      )

      survival_data <- rbind(survival_data, row_data)
    }
  }

  # Ensure actor is a factor for proper plotting
  survival_data$actor <- factor(survival_data$actor)

  # Create the cumulative entry plot
  plot <- ggplot2::ggplot(survival_data,
                          ggplot2::aes(x = step, y = entry_rate,
                                       color = actor, group = actor)) +
    ggplot2::geom_step(size = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_entry, ymax = ci_upper_entry,
                                      fill = actor), alpha = 0.2, color = NA) +
    ggplot2::labs(
      x = "Chain Step ID",
      y = "Proportion Entered Market",
      title = "Cumulative Market Entry Curves by Actor",
      subtitle = paste0(conf_level * 100, "% Confidence Intervals")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25)
    ) +
    ggplot2::scale_color_brewer(palette = "Set1", name = "Actor ID") +
    ggplot2::scale_fill_brewer(palette = "Set1", name = "Actor ID") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )

  # Return list with data and plot
  return(list(
    entry_df = entry_df,
    util_df = util_df,
    survival_data = survival_data,
    plot = plot,
    first_entries = first_entries
  ))
}
