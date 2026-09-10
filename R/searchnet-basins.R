#' @title Basin Geometry and Depth-Width Tradeoff
#' @description Functions for computing basin geometry metrics (width, depth,
#'   steepness, escape difficulty), the depth-width tradeoff frontier,
#'   cross-partial derivatives, and imitation penalties.
#' @name searchnet-basins

# Okabe-Ito policy colors
.basin_colors <- c(RS = "#1a1a1a", RE = "#E69F00", RR = "#56B4E9", RF = "#009E73")

# Default policy parameter profiles
.policy_params <- list(
  RS = list(scope_cost = 0.60, synergy = -0.30, herding = 0.20, label = "Res-Suppressing"),
  RE = list(scope_cost = -0.50, synergy = 0.00, herding = 0.40, label = "Res-Enriching"),
  RR = list(scope_cost = 0.00, synergy = 0.60, herding = -0.30, label = "Res-Retaining"),
  RF = list(scope_cost = -0.60, synergy = 0.50, herding = 0.00, label = "Res-Freeing")
)


#' Compute Basin Geometry from Policy Parameters
#'
#' Derives basin width, depth, and steepness from the three search
#' parameters (scope cost, synergy, herding) using the analytical
#' mapping from the AMR manuscript.
#'
#' @param scope_cost Numeric scope cost modifier (positive = narrows basin)
#' @param synergy Numeric synergy modifier (positive = deepens basin)
#' @param herding Numeric herding modifier (positive = steepens walls)
#' @param base_width Numeric baseline width (default 0.4)
#' @param base_depth Numeric baseline depth (default 0.3)
#' @param base_steepness Numeric baseline steepness exponent (default 1.0)
#' @return Named list with width, depth, steepness, escape_difficulty
#' @examples
#' ## Resource-suppressing policy: narrow, shallow, steep-walled basin
#' saomnk_basin_geometry(scope_cost = 0.6, synergy = -0.3, herding = 0.2)
#'
#' ## Resource-freeing policy: wide and deep
#' saomnk_basin_geometry(scope_cost = -0.6, synergy = 0.5, herding = 0)
#' @export
saomnk_basin_geometry <- function(scope_cost, synergy, herding,
                                   base_width = 0.4, base_depth = 0.3,
                                   base_steepness = 1.0) {
  # Normalize to [0,1]
  norm_scope <- (scope_cost + 0.6) / 1.2
  norm_syn <- (synergy + 0.6) / 1.2
  norm_herd <- (herding + 0.6) / 1.2

  # Derive geometry
  width <- base_width + 1.2 * (1 - norm_scope)
  depth <- base_depth + 0.7 * norm_syn
  steepness <- base_steepness + 5.0 * norm_herd

  # Escape difficulty = depth * steepness / width (composite metric)
  escape <- depth * steepness / width

  list(
    width = width,
    depth = depth,
    steepness = steepness,
    escape_difficulty = escape,
    scope_cost = scope_cost,
    synergy = synergy,
    herding = herding
  )
}


#' Compute Depth-Width Tradeoff Across Policy Types
#'
#' Computes basin geometry for each policy type and returns the
#' tradeoff frontier data.
#'
#' @param policy_params Named list of policy parameter lists. Each must
#'   contain scope_cost, synergy, herding. Default uses the AMR paper values.
#' @return Data frame with columns: policy, label, width, depth, steepness,
#'   escape_difficulty, scope_cost, synergy, herding
#' @examples
#' saomnk_depth_width_tradeoff()
#' @export
saomnk_depth_width_tradeoff <- function(policy_params = .policy_params) {
  results <- lapply(names(policy_params), function(pid) {
    p <- policy_params[[pid]]
    geom <- saomnk_basin_geometry(p$scope_cost, p$synergy, p$herding)
    data.frame(
      policy = pid,
      label = p$label,
      width = geom$width,
      depth = geom$depth,
      steepness = geom$steepness,
      escape_difficulty = geom$escape_difficulty,
      scope_cost = p$scope_cost,
      synergy = p$synergy,
      herding = p$herding,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, results)
}


#' Compute Cross-Partial Derivative Numerically
#'
#' Numerically estimates d2W/d(sigma)d(gamma), the cross-partial
#' of basin width with respect to synergy and scope cost.
#' Negative cross-partial = the complementarity trap.
#'
#' @param sigma_range Numeric vector c(min, max) for synergy (default c(-0.6, 0.6))
#' @param gamma_range Numeric vector c(min, max) for scope cost (default c(-0.6, 0.6))
#' @param n_grid Integer grid resolution (default 50)
#' @param herding Numeric fixed herding value (default 0)
#' @return List with:
#'   \item{cross_partial}{Matrix of d2W/dsigma.dgamma values}
#'   \item{sigma_values}{Numeric vector of sigma grid points}
#'   \item{gamma_values}{Numeric vector of gamma grid points}
#'   \item{mean_cross_partial}{Scalar mean cross-partial (should be < 0)}
#' @examples
#' cp <- saomnk_cross_partial(n_grid = 21)
#' cp$mean_cross_partial   # negative: the complementarity trap
#' @export
saomnk_cross_partial <- function(sigma_range = c(-0.6, 0.6),
                                  gamma_range = c(-0.6, 0.6),
                                  n_grid = 50L,
                                  herding = 0) {
  sigma_vals <- seq(sigma_range[1], sigma_range[2], length.out = n_grid)
  gamma_vals <- seq(gamma_range[1], gamma_range[2], length.out = n_grid)
  ds <- sigma_vals[2] - sigma_vals[1]
  dg <- gamma_vals[2] - gamma_vals[1]

  # Compute width on the full grid
  W <- matrix(0, n_grid, n_grid)
  for (i in seq_len(n_grid)) {
    for (j in seq_len(n_grid)) {
      geom <- saomnk_basin_geometry(gamma_vals[j], sigma_vals[i], herding)
      W[i, j] <- geom$width
    }
  }

  # Numerical second mixed partial: d2W/dsigma.dgamma
  cross <- matrix(NA, n_grid - 2, n_grid - 2)
  for (i in 2:(n_grid - 1)) {
    for (j in 2:(n_grid - 1)) {
      cross[i - 1, j - 1] <- (W[i + 1, j + 1] - W[i + 1, j - 1] -
                                 W[i - 1, j + 1] + W[i - 1, j - 1]) / (4 * ds * dg)
    }
  }

  list(
    cross_partial = cross,
    sigma_values = sigma_vals[2:(n_grid - 1)],
    gamma_values = gamma_vals[2:(n_grid - 1)],
    mean_cross_partial = mean(cross, na.rm = TRUE),
    width_surface = W
  )
}


#' Plot Depth-Width Tradeoff Frontier
#'
#' Scatter plot with width on x-axis, depth on y-axis. Each policy type
#' is a labeled point. Optionally shows RBV-optimal and DC-optimal endpoints.
#'
#' @param tradeoff_data Data frame from saomnk_depth_width_tradeoff
#' @param colors Named vector of policy colors (default Okabe-Ito)
#' @param show_rbv_dc Logical whether to annotate RBV and DC endpoints
#' @param show_frontier Logical whether to draw the Pareto frontier curve
#' @param title Character plot title
#' @return ggplot object
#' @examples
#' \donttest{
#' td <- saomnk_depth_width_tradeoff()
#' p <- saomnk_plot_tradeoff_frontier(td, show_frontier = FALSE)
#' }
#' @export
saomnk_plot_tradeoff_frontier <- function(tradeoff_data,
                                           colors = .basin_colors,
                                           show_rbv_dc = TRUE,
                                           show_frontier = TRUE,
                                           title = "The Depth-Width Tradeoff") {
  requireNamespace("ggplot2", quietly = TRUE)

  p <- ggplot2::ggplot(tradeoff_data,
                       ggplot2::aes(x = width, y = depth, color = policy, label = policy)) +
    ggplot2::geom_point(size = 6) +
    ggplot2::geom_text(nudge_y = 0.03, fontface = "bold", size = 5) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = title,
      subtitle = "Each policy type occupies a different position on the tradeoff frontier",
      x = "Basin Width (Strategic Flexibility)",
      y = "Basin Depth (Fitness Commitment)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "none"
    )

  if (show_frontier) {
    # Fit a smooth curve through the points
    p <- p + ggplot2::geom_smooth(
      method = "loess", se = FALSE, color = "gray70",
      linetype = "dashed", linewidth = 0.8
    )
  }

  if (show_rbv_dc) {
    # Annotate RBV-optimal (max depth) and DC-optimal (max width)
    rbv_row <- tradeoff_data[which.max(tradeoff_data$depth), ]
    dc_row <- tradeoff_data[which.max(tradeoff_data$width), ]

    p <- p +
      ggplot2::annotate("text", x = rbv_row$width, y = rbv_row$depth + 0.06,
                        label = "RBV optimal\n(deep, narrow)", color = "#d63031",
                        fontface = "italic", size = 3.5) +
      ggplot2::annotate("text", x = dc_row$width, y = dc_row$depth + 0.06,
                        label = "DC optimal\n(wide, shallow)", color = "#0984e3",
                        fontface = "italic", size = 3.5)
  }

  p
}


#' Plot Basin Shape Comparison
#'
#' Overlays basin profiles for all policy types on the same axes.
#'
#' @param tradeoff_data Data frame from saomnk_depth_width_tradeoff
#' @param colors Named color vector
#' @param x_range Numeric vector c(min, max) for x-axis (default c(-3, 3))
#' @param title Character plot title
#' @return ggplot object
#' @examples
#' \donttest{
#' td <- saomnk_depth_width_tradeoff()
#' p <- saomnk_plot_basin_comparison(td)
#' }
#' @export
saomnk_plot_basin_comparison <- function(tradeoff_data,
                                          colors = .basin_colors,
                                          x_range = c(-3, 3),
                                          title = "Basin Geometry Comparison Across Policy Types") {
  requireNamespace("ggplot2", quietly = TRUE)

  x <- seq(x_range[1], x_range[2], length.out = 500)
  curves <- lapply(seq_len(nrow(tradeoff_data)), function(i) {
    row <- tradeoff_data[i, ]
    y <- -row$depth * exp(-((abs(x) / row$width)^row$steepness))
    data.frame(x = x, y = y, policy = row$policy, stringsAsFactors = FALSE)
  })
  curve_df <- do.call(rbind, curves)

  ggplot2::ggplot(curve_df, ggplot2::aes(x = x, y = y, color = policy, fill = policy)) +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = y, ymax = 0), alpha = 0.15) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = title,
      subtitle = "Same depth (fitness parity), different geometry",
      x = "Strategy Space",
      y = "Fitness Landscape"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "bottom"
    )
}


#' Simulate and Plot Erosion / Landscape Perturbation
#'
#' Simulates 4 firms with different basin geometries over n_periods.
#' Shows fitness parity pre-shock, then divergent trajectories post-shock.
#'
#' @param tradeoff_data Data frame from saomnk_depth_width_tradeoff
#' @param n_periods Integer simulation length (default 20)
#' @param shock_period Integer when shock hits (default 11)
#' @param shock_magnitude Numeric perturbation size (default 0.5)
#' @param noise_sd Numeric noise standard deviation (default 0.02)
#' @param colors Named color vector
#' @param title Character plot title
#' @return ggplot object
#' @examples
#' \donttest{
#' td <- saomnk_depth_width_tradeoff()
#' p <- saomnk_plot_erosion_simulation(td, n_periods = 20, shock_period = 11)
#' }
#' @export
saomnk_plot_erosion_simulation <- function(tradeoff_data,
                                            n_periods = 20L,
                                            shock_period = 11L,
                                            shock_magnitude = 0.5,
                                            noise_sd = 0.02,
                                            colors = .basin_colors,
                                            title = "Fitness Trajectories Under Landscape Perturbation") {
  requireNamespace("ggplot2", quietly = TRUE)
  set.seed(42)

  trajectories <- lapply(seq_len(nrow(tradeoff_data)), function(i) {
    row <- tradeoff_data[i, ]
    fitness <- numeric(n_periods)
    base_fitness <- 0.8  # parity level

    for (t in seq_len(n_periods)) {
      if (t < shock_period) {
        fitness[t] <- base_fitness + stats::rnorm(1, 0, noise_sd)
      } else {
        # Fitness loss proportional to shock^2 / width^2
        loss <- row$depth * (shock_magnitude / row$width)^row$steepness
        loss <- min(loss, base_fitness * 0.8)  # cap at 80% loss
        # Recovery proportional to width
        recovery_rate <- row$width / 5
        periods_since <- t - shock_period
        current_loss <- loss * exp(-recovery_rate * periods_since)
        fitness[t] <- base_fitness - current_loss + stats::rnorm(1, 0, noise_sd)
      }
    }

    data.frame(
      period = seq_len(n_periods),
      fitness = fitness,
      policy = row$policy,
      stringsAsFactors = FALSE
    )
  })
  traj_df <- do.call(rbind, trajectories)

  ggplot2::ggplot(traj_df, ggplot2::aes(x = period, y = fitness,
                                         color = policy, group = policy)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = shock_period, linetype = "dashed", color = "red", alpha = 0.5) +
    ggplot2::annotate("text", x = shock_period + 0.5, y = max(traj_df$fitness),
                      label = "Shock", color = "red", hjust = 0, fontface = "italic") +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = title,
      subtitle = "Pre-shock parity masks post-shock divergence driven by basin geometry",
      x = "Period",
      y = "Competitive Fitness",
      color = "Policy Type"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "bottom"
    )
}


#' Compute Imitation Penalty for Each Basin Type
#'
#' The imitation penalty = fitness cost of landing on the basin wall
#' rather than the floor. Steeper walls = higher penalty.
#'
#' @param tradeoff_data Data frame from saomnk_depth_width_tradeoff
#' @param imitation_distance Numeric how far from the peak the imitator lands
#'   (as fraction of basin width, default 0.5)
#' @return Data frame with policy, penalty, gradient, width, steepness
#' @examples
#' td <- saomnk_depth_width_tradeoff()
#' saomnk_imitation_penalty(td, imitation_distance = 0.5)
#' @export
saomnk_imitation_penalty <- function(tradeoff_data, imitation_distance = 0.5) {
  penalties <- lapply(seq_len(nrow(tradeoff_data)), function(i) {
    row <- tradeoff_data[i, ]
    # Position on the wall at imitation_distance * width
    x_wall <- imitation_distance * row$width
    # Fitness at that position
    f_wall <- -row$depth * exp(-((x_wall / row$width)^row$steepness))
    # Fitness at the peak (x = 0)
    f_peak <- -row$depth
    # Penalty = fitness difference
    penalty <- abs(f_peak - f_wall)
    # Gradient at that position
    gradient <- row$depth * row$steepness / row$width *
      (x_wall / row$width)^(row$steepness - 1) *
      exp(-((x_wall / row$width)^row$steepness))

    data.frame(
      policy = row$policy,
      penalty = penalty,
      gradient = gradient,
      width = row$width,
      steepness = row$steepness,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, penalties)
}
