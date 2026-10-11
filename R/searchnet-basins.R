## Basin geometry of a parameterized landscape (internal).
##
## Functions for computing basin geometry metrics (width, depth, steepness,
## escape difficulty) from search parameters, the depth-width table across a
## set of parameter sets, a numerical cross-partial of basin width, and the
## fitness penalty of landing off a basin's floor. None is exported.

# Okabe-Ito colors, one per default parameter set
.basin_colors <- c(A = "#1a1a1a", B = "#E69F00", C = "#56B4E9", D = "#009E73")

# Default parameter sets (illustrative corners of the parameter space)
.basin_param_sets <- list(
  A = list(scope_cost = 0.60, synergy = -0.30, herding = 0.20, label = "Set A"),
  B = list(scope_cost = -0.50, synergy = 0.00, herding = 0.40, label = "Set B"),
  C = list(scope_cost = 0.00, synergy = 0.60, herding = -0.30, label = "Set C"),
  D = list(scope_cost = -0.60, synergy = 0.50, herding = 0.00, label = "Set D")
)


#' Basin geometry from search parameters
#'
#' Derives basin width, depth, and steepness from the three search
#' parameters (scope cost, synergy, herding) using the analytical
#' mapping in the package's basin-geometry derivation.
#'
#' @param scope_cost Numeric scope cost modifier (positive = narrows basin)
#' @param synergy Numeric synergy modifier (positive = deepens basin)
#' @param herding Numeric herding modifier (positive = steepens walls)
#' @param base_width Numeric baseline width (default 0.4)
#' @param base_depth Numeric baseline depth (default 0.3)
#' @param base_steepness Numeric baseline steepness exponent (default 1.0)
#' @return Named list with width, depth, steepness, escape_difficulty and the
#'   three inputs.
#' @examples
#' ## High scope cost, negative synergy: narrow, shallow, steep-walled basin
#' basin_geometry(scope_cost = 0.6, synergy = -0.3, herding = 0.2)
#' @keywords internal
#' @noRd
basin_geometry <- function(scope_cost, synergy, herding,
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


#' Basin geometry for each of a set of parameter sets
#'
#' Computes basin geometry for each parameter set and returns one row per
#' set: the data behind a depth-width frontier.
#'
#' @param param_sets Named list of parameter sets. Each must contain
#'   scope_cost, synergy, herding and label. The default uses four
#'   illustrative corners of the parameter space (sets A to D).
#' @return Data frame with columns: param_set, label, width, depth, steepness,
#'   escape_difficulty, scope_cost, synergy, herding
#' @examples
#' basin_geometry_table()
#' @keywords internal
#' @noRd
basin_geometry_table <- function(param_sets = .basin_param_sets) {
  results <- lapply(names(param_sets), function(pid) {
    p <- param_sets[[pid]]
    geom <- basin_geometry(p$scope_cost, p$synergy, p$herding)
    data.frame(
      param_set = pid,
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


#' Cross-partial of basin width in synergy and scope cost
#'
#' Numerically estimates d2W/d(sigma)d(gamma), the cross-partial
#' of basin width with respect to synergy and scope cost.
#'
#' @param sigma_range Numeric vector c(min, max) for synergy (default c(-0.6, 0.6))
#' @param gamma_range Numeric vector c(min, max) for scope cost (default c(-0.6, 0.6))
#' @param n_grid Integer grid resolution (default 50)
#' @param herding Numeric fixed herding value (default 0)
#' @return List with cross_partial (matrix of d2W/dsigma.dgamma values),
#'   sigma_values, gamma_values, mean_cross_partial and width_surface.
#' @examples
#' cp <- basin_width_cross_partial(n_grid = 21)
#' cp$mean_cross_partial
#' @keywords internal
#' @noRd
basin_width_cross_partial <- function(sigma_range = c(-0.6, 0.6),
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
      geom <- basin_geometry(gamma_vals[j], sigma_vals[i], herding)
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


#' Plot basin depth against basin width
#'
#' Scatter plot with width on x-axis, depth on y-axis. Each parameter set
#' is a labeled point. Optionally marks the deepest and the widest basin.
#'
#' @param tradeoff_data Data frame from basin_geometry_table()
#' @param colors Named vector of colors, one per parameter set
#' @param show_extremes Logical whether to annotate the deepest and the widest
#'   basin
#' @param show_frontier Logical whether to draw a smoothed frontier curve
#' @param title Character plot title
#' @return ggplot object
#' @keywords internal
#' @noRd
plot_basin_depth_width <- function(tradeoff_data,
                                   colors = .basin_colors,
                                   show_extremes = TRUE,
                                   show_frontier = TRUE,
                                   title = "The Depth-Width Tradeoff") {
  requireNamespace("ggplot2", quietly = TRUE)

  p <- ggplot2::ggplot(tradeoff_data,
                       ggplot2::aes(x = width, y = depth, color = param_set,
                                    label = param_set)) +
    ggplot2::geom_point(size = 6) +
    ggplot2::geom_text(nudge_y = 0.03, fontface = "bold", size = 5) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = title,
      subtitle = "Each point: one parameter set, placed by the width and depth of its basin",
      x = "Basin Width",
      y = "Basin Depth"
    ) +
    theme_searchnet(base_size = 12) +
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

  if (show_extremes) {
    # Annotate the deepest (max depth) and the widest (max width) basin
    deep_row <- tradeoff_data[which.max(tradeoff_data$depth), ]
    wide_row <- tradeoff_data[which.max(tradeoff_data$width), ]

    p <- p +
      ggplot2::annotate("text", x = deep_row$width, y = deep_row$depth + 0.06,
                        label = "Deepest\n(deep, narrow)", color = "#D55E00",
                        fontface = "italic", size = 3.5) +
      ggplot2::annotate("text", x = wide_row$width, y = wide_row$depth + 0.06,
                        label = "Widest\n(wide, shallow)", color = "#0072B2",
                        fontface = "italic", size = 3.5)
  }

  p
}


#' Plot basin profiles
#'
#' Overlays the basin profile of every parameter set on the same axes.
#'
#' @param tradeoff_data Data frame from basin_geometry_table()
#' @param colors Named color vector
#' @param x_range Numeric vector c(min, max) for x-axis (default c(-3, 3))
#' @param title Character plot title
#' @return ggplot object
#' @keywords internal
#' @noRd
plot_basin_profiles <- function(tradeoff_data,
                                colors = .basin_colors,
                                x_range = c(-3, 3),
                                title = "Basin Geometry Comparison Across Parameter Sets") {
  requireNamespace("ggplot2", quietly = TRUE)

  x <- seq(x_range[1], x_range[2], length.out = 500)
  curves <- lapply(seq_len(nrow(tradeoff_data)), function(i) {
    row <- tradeoff_data[i, ]
    y <- -row$depth * exp(-((abs(x) / row$width)^row$steepness))
    data.frame(x = x, y = y, param_set = row$param_set, stringsAsFactors = FALSE)
  })
  curve_df <- do.call(rbind, curves)

  ggplot2::ggplot(curve_df, ggplot2::aes(x = x, y = y, color = param_set,
                                         fill = param_set)) +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = y, ymax = 0), alpha = 0.15) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = title,
      subtitle = "Each curve: one parameter set's basin, drawn from its depth, width and steepness",
      x = "Strategy Space",
      y = "Fitness Landscape",
      color = "Parameter Set", fill = "Parameter Set"
    ) +
    theme_searchnet(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "bottom"
    )
}


#' Simulate and plot fitness after a landscape shock
#'
#' Simulates one actor per parameter set over n_periods: a common fitness
#' level before the shock, then a loss and recovery set by each basin's
#' geometry. Sets the RNG seed to 42.
#'
#' @param tradeoff_data Data frame from basin_geometry_table()
#' @param n_periods Integer simulation length (default 20)
#' @param shock_period Integer when shock hits (default 11)
#' @param shock_magnitude Numeric perturbation size (default 0.5)
#' @param noise_sd Numeric noise standard deviation (default 0.02)
#' @param colors Named color vector
#' @param title Character plot title
#' @return ggplot object
#' @keywords internal
#' @noRd
plot_basin_shock_recovery <- function(tradeoff_data,
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
    base_fitness <- 0.8  # common pre-shock level

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
      param_set = row$param_set,
      stringsAsFactors = FALSE
    )
  })
  traj_df <- do.call(rbind, trajectories)

  ggplot2::ggplot(traj_df, ggplot2::aes(x = period, y = fitness,
                                         color = param_set, group = param_set)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = shock_period, linetype = "dashed", color = "#D55E00", alpha = 0.5) +
    ggplot2::annotate("text", x = shock_period + 0.5, y = max(traj_df$fitness),
                      label = "Shock", color = "#D55E00", hjust = 0, fontface = "italic") +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = title,
      subtitle = "Each line: one parameter set's fitness per period; the landscape shifts at the dashed line",
      x = "Period",
      y = "Fitness",
      color = "Parameter Set"
    ) +
    theme_searchnet(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "bottom"
    )
}


#' Imitation penalty for each basin
#'
#' The imitation penalty is the fitness cost of landing on the basin wall
#' rather than the floor. Steeper walls give a higher penalty.
#'
#' @param tradeoff_data Data frame from basin_geometry_table()
#' @param imitation_distance Numeric how far from the peak the imitator lands
#'   (as fraction of basin width, default 0.5)
#' @return Data frame with param_set, penalty, gradient, width, steepness
#' @examples
#' td <- basin_geometry_table()
#' basin_imitation_penalty(td, imitation_distance = 0.5)
#' @keywords internal
#' @noRd
basin_imitation_penalty <- function(tradeoff_data, imitation_distance = 0.5) {
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
      param_set = row$param_set,
      penalty = penalty,
      gradient = gradient,
      width = row$width,
      steepness = row$steepness,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, penalties)
}
