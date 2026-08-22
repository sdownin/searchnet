# plot-phase-space.R
# =============================================================================
# Phase Space Visualization System for SearchNet (SaoMNK)
# =============================================================================
# Plots the network-behavior-fitness phase space -- the "showstopper" figure.
# Three fundamental dimensions of any search system:
#   1. NETWORK -- structural properties (K_AC, K_CA, K_AA, K_CC, density, ...)
#   2. BEHAVIOR -- actor-level behavioral properties (exploration rate, turnover, ...)
#   3. FITNESS -- outcome measures (utility, utility change, rank, ...)
#
# Each function takes `env` (SaomNkRSienaBiEnv) as its first argument.
# Standalone functions -- no self$ references.
# =============================================================================


# -----------------------------------------------------------------------------
# SearchNet palette (consistent with manim / brand)
# -----------------------------------------------------------------------------
.searchnet_palette <- c(
  navy  = "#1b3a5c",
  teal  = "#2a9d8f",
  amber = "#e9c46a",
  coral = "#e76f51",
  slate = "#415a77",
  sage  = "#6b9080",
  plum  = "#7b2d8e",
  steel = "#778da9"
)

.searchnet_strategy_colors <- c(
  "#1b3a5c", "#2a9d8f", "#e76f51", "#e9c46a",
  "#415a77", "#6b9080", "#7b2d8e", "#778da9"
)

.searchnet_time_gradient <- c("#1b3a5c", "#2a9d8f", "#e9c46a", "#e76f51")


# -----------------------------------------------------------------------------
# Internal: Variable name labels for axes
# -----------------------------------------------------------------------------
.phase_var_labels <- c(
  K_AC          = "Scope (K[AC])",
  K_CA          = "Popularity (K[CA])",
  K_AA          = "Sociality (K[AA])",
  K_CC          = "Epistasis (K[CC])",
  density       = "Network Density",
  scope         = "Actor Scope (K[AC])",
  popularity    = "Component Popularity (K[CA])",
  exploration_rate = "Exploration Rate",
  strategy      = "Strategy Type",
  scope_change  = "Scope Change (Delta~K[AC])",
  turnover_rate = "Tie Turnover Rate",
  utility       = "Utility",
  nk_fitness    = "NK Fitness",
  utility_change = "Utility Change (Delta~U)",
  rank          = "Utility Rank"
)

.phase_var_label <- function(var) {
  if (var %in% names(.phase_var_labels)) .phase_var_labels[[var]]
  else gsub("_", " ", var)
}

.phase_dimension_class <- function(var) {
  network_vars  <- c("K_AC", "K_CA", "K_AA", "K_CC", "density", "scope", "popularity")
  behavior_vars <- c("exploration_rate", "strategy", "scope_change", "turnover_rate")
  fitness_vars  <- c("utility", "nk_fitness", "utility_change", "rank")
  if (var %in% network_vars)  return("Network")
  if (var %in% behavior_vars) return("Behavior")
  if (var %in% fitness_vars)  return("Fitness")
  return("Unknown")
}


# -----------------------------------------------------------------------------
# Internal: Extract a unified phase-space dataframe from env
# -----------------------------------------------------------------------------
.extract_phase_data <- function(env, vars, thin_factor = 1) {

  actor_strats <- tryCatch(env$get_actor_strategies(), error = function(e) NULL)
  n_actors     <- env$M
  n_components <- env$N

  # -- Utility data (always present after simulation) --
  util_df <- env$actor_util_df
  if (is.null(util_df) || nrow(util_df) == 0)
    stop("Phase space plot requires a completed simulation (actor_util_df is empty).")

  util_df$actor_id <- as.numeric(as.character(util_df$actor_id))

  # Apply thinning
  if (thin_factor > 1) {
    util_df <- util_df %>% filter(chain_step_id %% thin_factor == 0)
  }

  unique_steps <- sort(unique(util_df$chain_step_id))
  n_steps_used <- length(unique_steps)

  # -- K-4 degree data --
  K4_df <- tryCatch(env$get_K4_df(), error = function(e) NULL)

  # -- Build per-actor-per-step dataframe --
  bi_env_arr <- env$bi_env_arr
  arr_steps   <- if (!is.null(bi_env_arr)) dim(bi_env_arr)[3] else 0

  # Initialize with utility columns
  base_df <- util_df %>%
    select(chain_step_id, actor_id, utility) %>%
    mutate(actor_id = as.integer(actor_id))

  # Add strategy
  if (!is.null(actor_strats)) {
    base_df$strategy <- as.character(actor_strats[base_df$actor_id])
  } else {
    base_df$strategy <- "1"
  }

  # -- Compute derived variables as needed --
  need_vars <- unique(vars)

  # Utility change (delta)
  if ("utility_change" %in% need_vars) {
    base_df <- base_df %>%
      arrange(actor_id, chain_step_id) %>%
      group_by(actor_id) %>%
      mutate(utility_change = utility - lag(utility, default = NA)) %>%
      ungroup()
  }

  # Rank (within each step)
  if ("rank" %in% need_vars) {
    base_df <- base_df %>%
      group_by(chain_step_id) %>%
      mutate(rank = rank(-utility, ties.method = "average")) %>%
      ungroup()
  }

  # NK fitness (alias for utility in current implementation)
  if ("nk_fitness" %in% need_vars) {
    base_df$nk_fitness <- base_df$utility
  }

  # -- K-degree variables from K4_df --
  k_vars_needed <- intersect(need_vars, c("K_AC", "K_CA", "K_AA", "K_CC"))
  if (length(k_vars_needed) > 0 && !is.null(K4_df)) {
    for (kvar in k_vars_needed) {
      kdf_sub <- K4_df %>%
        filter(effect == kvar, !is.na(actor_id)) %>%
        select(chain_step_id, actor_id, value) %>%
        mutate(actor_id = as.integer(as.character(actor_id)))
      names(kdf_sub)[names(kdf_sub) == "value"] <- kvar

      if (thin_factor > 1) {
        kdf_sub <- kdf_sub %>% filter(chain_step_id %% thin_factor == 0)
      }

      # For component-level K (K_CA, K_CC), actor_id may be NA -- skip those
      kdf_sub <- kdf_sub %>% filter(!is.na(actor_id))

      if (nrow(kdf_sub) > 0) {
        base_df <- base_df %>%
          left_join(kdf_sub, by = c("chain_step_id", "actor_id"))
      } else {
        base_df[[kvar]] <- NA_real_
      }
    }
  }

  # Scope and popularity aliases
  if ("scope" %in% need_vars && !"scope" %in% names(base_df)) {
    if ("K_AC" %in% names(base_df)) {
      base_df$scope <- base_df$K_AC
    } else {
      base_df$scope <- NA_real_
    }
  }
  if ("popularity" %in% need_vars && !"popularity" %in% names(base_df)) {
    if ("K_CA" %in% names(base_df)) {
      base_df$popularity <- base_df$K_CA
    } else {
      base_df$popularity <- NA_real_
    }
  }

  # -- Density (mean bipartite fill per actor at each step) --
  if ("density" %in% need_vars) {
    if (!is.null(bi_env_arr) && arr_steps > 0) {
      dens_list <- lapply(seq_along(unique_steps), function(idx) {
        step_id <- unique_steps[idx]
        if (idx <= arr_steps) {
          bi_mat <- bi_env_arr[, , idx]
          data.frame(
            chain_step_id = step_id,
            actor_id      = 1:n_actors,
            density       = rowSums(bi_mat > 0) / n_components
          )
        } else {
          NULL
        }
      })
      dens_df <- bind_rows(dens_list)
      base_df <- base_df %>%
        left_join(dens_df, by = c("chain_step_id", "actor_id"))
    } else {
      base_df$density <- NA_real_
    }
  }

  # -- Exploration rate (fraction of ties to NEW components) --
  if ("exploration_rate" %in% need_vars || "turnover_rate" %in% need_vars) {
    if (!is.null(bi_env_arr) && arr_steps > 0) {
      new_components <- which(colSums(env$bipartite_matrix_init) == 0)
      old_components <- setdiff(1:n_components, new_components)

      explore_list <- lapply(seq_along(unique_steps), function(idx) {
        step_id <- unique_steps[idx]
        if (idx <= arr_steps) {
          bi_mat <- bi_env_arr[, , idx]
          exp_rates <- sapply(1:n_actors, function(a) {
            ties <- which(bi_mat[a, ] > 0)
            if (length(ties) == 0) return(NA_real_)
            sum(ties %in% new_components) / length(ties)
          })

          # Turnover: Jaccard distance from previous step
          turnover <- rep(NA_real_, n_actors)
          if (idx > 1) {
            bi_mat_prev <- bi_env_arr[, , idx - 1]
            for (a in 1:n_actors) {
              prev_ties <- which(bi_mat_prev[a, ] > 0)
              curr_ties <- which(bi_mat[a, ] > 0)
              union_n <- length(union(prev_ties, curr_ties))
              if (union_n > 0) {
                turnover[a] <- 1 - length(intersect(prev_ties, curr_ties)) / union_n
              }
            }
          }

          data.frame(
            chain_step_id    = step_id,
            actor_id         = 1:n_actors,
            exploration_rate = exp_rates,
            turnover_rate    = turnover
          )
        } else {
          NULL
        }
      })
      exp_df <- bind_rows(explore_list)
      cols_to_join <- intersect(c("exploration_rate", "turnover_rate"), need_vars)
      exp_df_sel <- exp_df[, c("chain_step_id", "actor_id", cols_to_join), drop = FALSE]
      base_df <- base_df %>%
        left_join(exp_df_sel, by = c("chain_step_id", "actor_id"))
    } else {
      if ("exploration_rate" %in% need_vars) base_df$exploration_rate <- NA_real_
      if ("turnover_rate" %in% need_vars)    base_df$turnover_rate <- NA_real_
    }
  }

  # -- Scope change --
  if ("scope_change" %in% need_vars) {
    scope_col <- if ("K_AC" %in% names(base_df)) "K_AC" else if ("scope" %in% names(base_df)) "scope" else NULL
    if (!is.null(scope_col)) {
      base_df <- base_df %>%
        arrange(actor_id, chain_step_id) %>%
        group_by(actor_id) %>%
        mutate(scope_change = .data[[scope_col]] - lag(.data[[scope_col]], default = NA)) %>%
        ungroup()
    } else {
      base_df$scope_change <- NA_real_
    }
  }

  # -- Normalized time for color encoding --
  step_range <- range(base_df$chain_step_id, na.rm = TRUE)
  base_df$time_norm <- if (diff(step_range) > 0) {
    (base_df$chain_step_id - step_range[1]) / diff(step_range)
  } else {
    0.5
  }

  # -- Phase classification (exploration vs exploitation regime) --
  if ("exploration_rate" %in% names(base_df)) {
    base_df$phase <- ifelse(base_df$exploration_rate > 0.5, "Exploration", "Exploitation")
  } else {
    base_df$phase <- NA_character_
  }

  # Drop rows with NA in requested variables
  base_df
}


# =============================================================================
# Function 1: saomnk_plot_phase_space_3d
# =============================================================================

#' 3D Phase Space Trajectory Plot
#'
#' Plot system evolution trajectories in 3D network-behavior-fitness space.
#' Each point is one actor at one timestep. Trajectories show evolution over time.
#' Color encodes time (early = cool, late = warm), strategy type, actor identity,
#' or exploration/exploitation phase.
#'
#' @param env SaomNkRSienaBiEnv object after simulation
#' @param x_var Network dimension variable (default \code{"K_AC"}).
#'   Supported: \code{"K_AC"}, \code{"K_CA"}, \code{"K_AA"}, \code{"K_CC"},
#'   \code{"density"}, \code{"scope"}, \code{"popularity"}.
#' @param y_var Behavior dimension variable (default \code{"exploration_rate"}).
#'   Supported: \code{"exploration_rate"}, \code{"strategy"},
#'   \code{"scope_change"}, \code{"turnover_rate"}.
#' @param z_var Fitness dimension variable (default \code{"utility"}).
#'   Supported: \code{"utility"}, \code{"nk_fitness"},
#'   \code{"utility_change"}, \code{"rank"}.
#' @param color_by What drives the color mapping: \code{"time"} (early = cool,
#'   late = warm), \code{"strategy"}, \code{"actor"}, or \code{"phase"}
#'   (exploration vs exploitation).
#' @param trajectories Logical; if \code{TRUE}, connect points for each actor
#'   as a trajectory line (default \code{TRUE}).
#' @param smooth Loess smoothing span for trajectory lines. Set to 0 to disable.
#' @param alpha Point transparency (0-1).
#' @param thin_factor Integer thinning factor for chain steps.
#' @param title Optional custom plot title.
#' @param ... Additional arguments (currently unused; reserved for future use).
#' @return A \code{plotly} interactive 3D scatter if \pkg{plotly} is available;
#'   otherwise a \code{ggplot} 2D faceted projection.
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 4, N = 6, seed = 42)
#' mod <- saomnk_model(density = -0.5, popularity = 0.2)
#' saomnk_run(env, mod, steps_per_actor = 5, seed = 12345)
#'
#' ## searchnet 0.8.1: coerce the stored utility table to a plain data.frame
#' ## before phase-space plotting (the extractor returns a data.table, which
#' ## the plot functions index with data.frame semantics)
#' env$actor_util_df <- as.data.frame(env$actor_util_df)
#'
#' p <- saomnk_plot_phase_space_3d(env, x_var = "K_AC",
#'                                 y_var = "exploration_rate",
#'                                 z_var = "utility", color_by = "time")
#' }
#' @export
saomnk_plot_phase_space_3d <- function(env,
                                       x_var = "K_AC",
                                       y_var = "exploration_rate",
                                       z_var = "utility",
                                       color_by = "time",
                                       trajectories = TRUE,
                                       smooth = 0.3,
                                       alpha = 0.6,
                                       thin_factor = 1,
                                       title = NULL,
                                       ...) {

  # Extract data
  df <- .extract_phase_data(env, vars = c(x_var, y_var, z_var), thin_factor = thin_factor)

  # Remove incomplete cases for the three axes
  df <- df[complete.cases(df[, c(x_var, y_var, z_var)]), ]
  if (nrow(df) == 0) stop("No complete observations for the requested variables.")

  # Color variable
  if (color_by == "time") {
    df$color_var <- df$time_norm
  } else if (color_by == "strategy") {
    df$color_var <- as.factor(df$strategy)
  } else if (color_by == "actor") {
    df$color_var <- as.factor(df$actor_id)
  } else if (color_by == "phase") {
    df$color_var <- as.factor(df$phase)
  } else {
    df$color_var <- df$time_norm
  }

  # Axis labels
  x_label <- .phase_var_label(x_var)
  y_label <- .phase_var_label(y_var)
  z_label <- .phase_var_label(z_var)

  auto_title <- if (is.null(title)) {
    sprintf("Phase Space: %s vs %s vs %s",
            .phase_dimension_class(x_var),
            .phase_dimension_class(y_var),
            .phase_dimension_class(z_var))
  } else {
    title
  }

  # -- Try plotly 3D (interactive) --
  if (requireNamespace("plotly", quietly = TRUE)) {
    return(.phase_space_3d_plotly(
      df, x_var, y_var, z_var,
      x_label, y_label, z_label,
      color_by, trajectories, smooth, alpha, auto_title
    ))
  }

  # -- Fallback: ggplot 2D projections --
  .phase_space_2d_ggplot(
    df, x_var, y_var, z_var,
    x_label, y_label, z_label,
    color_by, trajectories, smooth, alpha, auto_title
  )
}


# -----------------------------------------------------------------------------
# Internal: plotly 3D implementation
# -----------------------------------------------------------------------------
.phase_space_3d_plotly <- function(df, x_var, y_var, z_var,
                                   x_label, y_label, z_label,
                                   color_by, trajectories, smooth, alpha,
                                   title) {

  # Determine colorscale
  if (color_by == "time") {
    # Custom navy -> teal -> amber -> coral gradient
    colorscale <- list(
      list(0,    .searchnet_palette[["navy"]]),
      list(0.33, .searchnet_palette[["teal"]]),
      list(0.66, .searchnet_palette[["amber"]]),
      list(1,    .searchnet_palette[["coral"]])
    )
    fig <- plotly::plot_ly(
      data = df,
      x = ~get(x_var), y = ~get(y_var), z = ~get(z_var),
      color = ~time_norm,
      colors = colorscale,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 3,
        opacity = alpha,
        line = list(width = 0)
      ),
      text = ~paste0(
        "Actor: ", actor_id,
        "<br>Strategy: ", strategy,
        "<br>Step: ", chain_step_id,
        "<br>", x_var, ": ", round(get(x_var), 3),
        "<br>", y_var, ": ", round(get(y_var), 3),
        "<br>", z_var, ": ", round(get(z_var), 3)
      ),
      hoverinfo = "text"
    )
  } else {
    # Discrete color
    n_levels <- length(unique(df$color_var))
    pal <- if (color_by == "phase") {
      c(Exploration = .searchnet_palette[["teal"]],
        Exploitation = .searchnet_palette[["coral"]])
    } else {
      .searchnet_strategy_colors[1:min(n_levels, length(.searchnet_strategy_colors))]
    }

    fig <- plotly::plot_ly(
      data = df,
      x = ~get(x_var), y = ~get(y_var), z = ~get(z_var),
      color = ~color_var,
      colors = pal,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 3,
        opacity = alpha,
        line = list(width = 0)
      ),
      text = ~paste0(
        "Actor: ", actor_id,
        "<br>Strategy: ", strategy,
        "<br>Step: ", chain_step_id,
        "<br>", x_var, ": ", round(get(x_var), 3),
        "<br>", y_var, ": ", round(get(y_var), 3),
        "<br>", z_var, ": ", round(get(z_var), 3)
      ),
      hoverinfo = "text"
    )
  }

  # -- Add trajectory lines --
  if (trajectories) {
    actor_ids <- sort(unique(df$actor_id))
    for (aid in actor_ids) {
      adf <- df %>%
        filter(actor_id == aid) %>%
        arrange(chain_step_id)

      if (nrow(adf) < 2) next

      # Optional loess smoothing per axis
      if (smooth > 0 && nrow(adf) >= 5) {
        tryCatch({
          x_smooth <- predict(loess(as.formula(paste(x_var, "~ chain_step_id")),
                                    data = adf, span = smooth))
          y_smooth <- predict(loess(as.formula(paste(y_var, "~ chain_step_id")),
                                    data = adf, span = smooth))
          z_smooth <- predict(loess(as.formula(paste(z_var, "~ chain_step_id")),
                                    data = adf, span = smooth))
          adf[[x_var]] <- x_smooth
          adf[[y_var]] <- y_smooth
          adf[[z_var]] <- z_smooth
        }, error = function(e) NULL)
      }

      # Line color by strategy
      strat <- as.character(adf$strategy[1])
      strat_idx <- which(sort(unique(df$strategy)) == strat)
      line_col <- .searchnet_strategy_colors[
        ((strat_idx - 1) %% length(.searchnet_strategy_colors)) + 1
      ]

      fig <- fig %>% plotly::add_trace(
        data = adf,
        x = ~get(x_var), y = ~get(y_var), z = ~get(z_var),
        type = "scatter3d",
        mode = "lines",
        line = list(color = line_col, width = 2),
        opacity = 0.5,
        showlegend = FALSE,
        hoverinfo = "skip"
      )

      # Arrow endpoint marker (last point -- larger)
      endpoint <- adf[nrow(adf), ]
      fig <- fig %>% plotly::add_trace(
        data = endpoint,
        x = ~get(x_var), y = ~get(y_var), z = ~get(z_var),
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 7,
          color = line_col,
          symbol = "diamond",
          opacity = 0.9,
          line = list(width = 1, color = "white")
        ),
        showlegend = FALSE,
        hoverinfo = "skip"
      )
    }
  }

  # -- Layout --
  fig <- fig %>% plotly::layout(
    title = list(
      text = title,
      font = list(family = "Times New Roman", size = 18, color = "#1b3a5c")
    ),
    scene = list(
      xaxis = list(title = x_label, gridcolor = "#dde3ea", zerolinecolor = "#bbb"),
      yaxis = list(title = y_label, gridcolor = "#dde3ea", zerolinecolor = "#bbb"),
      zaxis = list(title = z_label, gridcolor = "#dde3ea", zerolinecolor = "#bbb"),
      camera = list(
        eye = list(x = 1.6, y = 1.6, z = 1.0)
      ),
      bgcolor = "#fafbfc"
    ),
    paper_bgcolor = "#ffffff",
    legend = list(
      title = list(text = ifelse(color_by == "time", "Time", "Group")),
      font  = list(family = "Times New Roman")
    )
  )

  fig
}


# -----------------------------------------------------------------------------
# Internal: ggplot 2D fallback -- three panels showing XY, XZ, YZ projections
# -----------------------------------------------------------------------------
.phase_space_2d_ggplot <- function(df, x_var, y_var, z_var,
                                   x_label, y_label, z_label,
                                   color_by, trajectories, smooth, alpha,
                                   title) {

  npoints <- nrow(df)
  point_size <- max(0.4, 4 / log10(max(npoints, 10)))

  # Color aesthetic
  color_aes <- if (color_by == "time") {
    aes(color = time_norm)
  } else if (color_by == "strategy") {
    aes(color = strategy)
  } else if (color_by == "actor") {
    aes(color = factor(actor_id))
  } else if (color_by == "phase") {
    aes(color = phase)
  } else {
    aes(color = time_norm)
  }

  # Build three projection panels
  p1 <- ggplot(df, aes(x = .data[[x_var]], y = .data[[z_var]])) +
    geom_point(color_aes, alpha = alpha, size = point_size, shape = 16) +
    labs(x = x_label, y = z_label) +
    theme_bw() +
    theme(legend.position = "none")

  p2 <- ggplot(df, aes(x = .data[[y_var]], y = .data[[z_var]])) +
    geom_point(color_aes, alpha = alpha, size = point_size, shape = 16) +
    labs(x = y_label, y = z_label) +
    theme_bw() +
    theme(legend.position = "none")

  p3 <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(color_aes, alpha = alpha, size = point_size, shape = 16) +
    labs(x = x_label, y = y_label) +
    theme_bw()

  # Add trajectories
  if (trajectories) {
    traj_layer <- if (smooth > 0) {
      geom_smooth(aes(group = actor_id), method = "loess", span = smooth,
                  se = FALSE, linewidth = 0.4, alpha = 0.3)
    } else {
      geom_path(aes(group = actor_id), alpha = 0.3, linewidth = 0.3)
    }
    p1 <- p1 + traj_layer
    p2 <- p2 + traj_layer
    p3 <- p3 + traj_layer
  }

  # Color scale
  if (color_by == "time") {
    grad <- scale_color_gradientn(colours = .searchnet_time_gradient, name = "Time")
    p1 <- p1 + grad; p2 <- p2 + grad; p3 <- p3 + grad
  } else if (color_by == "strategy" || color_by == "actor") {
    pal <- scale_color_manual(values = .searchnet_strategy_colors, name = "Group")
    p1 <- p1 + pal; p2 <- p2 + pal; p3 <- p3 + pal
  } else if (color_by == "phase") {
    pal <- scale_color_manual(
      values = c(Exploration = .searchnet_palette[["teal"]],
                 Exploitation = .searchnet_palette[["coral"]]),
      name = "Phase"
    )
    p1 <- p1 + pal; p2 <- p2 + pal; p3 <- p3 + pal
  }

  # Arrange with cowplot
  combined <- cowplot::plot_grid(
    p1, p2, p3,
    ncol = 3,
    labels = c("Network x Fitness", "Behavior x Fitness", "Network x Behavior"),
    label_size = 10,
    label_fontface = "italic",
    label_colour = .searchnet_palette[["navy"]]
  )

  title_grob <- cowplot::ggdraw() +
    cowplot::draw_label(title, fontface = "bold", size = 14,
                        colour = .searchnet_palette[["navy"]],
                        fontfamily = "serif")

  cowplot::plot_grid(title_grob, combined, ncol = 1, rel_heights = c(0.08, 1))
}


# =============================================================================
# Function 2: saomnk_plot_phase_heatmap
# =============================================================================

#' 2D Phase Space Heatmap
#'
#' Density heatmap showing where actors spend time in the 2D projection
#' of the network-behavior-fitness space. Reveals attractors and basins of
#' attraction as high-density regions.
#'
#' @param env SaomNkRSienaBiEnv object after simulation.
#' @param x_var First dimension variable.
#' @param y_var Second dimension variable.
#' @param fill_var What to show as heat: \code{"density"} (time spent),
#'   \code{"fitness"} (mean utility within bin), or \code{"count"} (raw count).
#' @param facet_by Optional faceting variable: \code{"strategy"},
#'   \code{"phase"}, or \code{"time_period"}.
#' @param bins Number of bins for hexagonal binning (default 30).
#' @param thin_factor Integer thinning factor for chain steps.
#' @return A \code{ggplot} object.
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 4, N = 6, seed = 42)
#' mod <- saomnk_model(density = -0.5, popularity = 0.2)
#' saomnk_run(env, mod, steps_per_actor = 5, seed = 12345)
#'
#' ## searchnet 0.8.1: coerce the stored utility table to a plain data.frame
#' env$actor_util_df <- as.data.frame(env$actor_util_df)
#'
#' p <- saomnk_plot_phase_heatmap(env, x_var = "K_AC", y_var = "utility",
#'                                bins = 10)
#' }
#' @export
saomnk_plot_phase_heatmap <- function(env,
                                      x_var = "K_AC",
                                      y_var = "utility",
                                      fill_var = "density",
                                      facet_by = NULL,
                                      bins = 30,
                                      thin_factor = 1) {

  all_vars <- c(x_var, y_var)
  if (fill_var == "fitness") all_vars <- c(all_vars, "utility")
  if (!is.null(facet_by) && facet_by == "phase") all_vars <- c(all_vars, "exploration_rate")

  df <- .extract_phase_data(env, vars = all_vars, thin_factor = thin_factor)
  df <- df[complete.cases(df[, c(x_var, y_var)]), ]
  if (nrow(df) == 0) stop("No complete observations for the requested variables.")

  x_label <- .phase_var_label(x_var)
  y_label <- .phase_var_label(y_var)

  # Time period thirds for faceting
  if (!is.null(facet_by) && facet_by == "time_period") {
    df$time_period <- cut(df$time_norm,
                          breaks = c(-Inf, 1/3, 2/3, Inf),
                          labels = c("Early", "Middle", "Late"))
  }

  # Build plot
  plt <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]]))

  if (fill_var == "density") {
    plt <- plt +
      geom_hex(bins = bins) +
      scale_fill_gradientn(
        colours = c(.searchnet_palette[["navy"]],
                    .searchnet_palette[["teal"]],
                    .searchnet_palette[["amber"]],
                    .searchnet_palette[["coral"]]),
        name = "Density\n(time spent)"
      )
  } else if (fill_var == "count") {
    plt <- plt +
      geom_hex(bins = bins) +
      scale_fill_gradientn(
        colours = c(.searchnet_palette[["navy"]],
                    .searchnet_palette[["teal"]],
                    .searchnet_palette[["amber"]]),
        name = "Count"
      )
  } else if (fill_var == "fitness") {
    plt <- plt +
      stat_summary_hex(aes(z = utility), fun = mean, bins = bins) +
      scale_fill_gradientn(
        colours = c(.searchnet_palette[["coral"]],
                    .searchnet_palette[["amber"]],
                    .searchnet_palette[["teal"]]),
        name = "Mean\nUtility"
      )
  }

  # Faceting
  if (!is.null(facet_by)) {
    facet_formula <- switch(facet_by,
      strategy    = ~ strategy,
      phase       = ~ phase,
      time_period = ~ time_period,
      NULL
    )
    if (!is.null(facet_formula)) {
      plt <- plt + facet_wrap(facet_formula)
    }
  }

  plt <- plt +
    labs(
      x = x_label,
      y = y_label,
      title = sprintf("Phase Space Heatmap: %s vs %s", x_label, y_label),
      subtitle = sprintf("Fill = %s | Bins = %d", fill_var, bins)
    ) +
    theme_bw() +
    theme(
      plot.title    = element_text(face = "bold", colour = .searchnet_palette[["navy"]],
                                   family = "serif", size = 14),
      plot.subtitle = element_text(colour = .searchnet_palette[["slate"]],
                                   family = "serif", size = 10, face = "italic"),
      legend.position = "right"
    )

  plt
}


# =============================================================================
# Function 3: saomnk_plot_phase_evolution
# =============================================================================

#' Phase Space Evolution Animation Frames
#'
#' Generate frame-by-frame data for animating system evolution through
#' phase space. Each frame shows actor positions at one timestep with
#' trailing trajectories from previous steps.
#'
#' @param env SaomNkRSienaBiEnv object after simulation.
#' @param x_var First dimension variable.
#' @param y_var Second dimension variable.
#' @param z_var Third dimension variable (used only if \pkg{plotly} is available
#'   and \code{use_3d = TRUE}; otherwise the 2D projection is used).
#' @param trail_length Number of past steps to include as trajectory trail
#'   behind each actor's current position (default 10).
#' @param fps Frames per second for the animation (default 5).
#' @param thin_factor Integer thinning factor for chain steps.
#' @param use_3d Logical; attempt 3D plotly animation if \code{TRUE} and
#'   \pkg{plotly} is available (default \code{FALSE} -- uses gganimate).
#' @return A \code{gganimate} object (if \pkg{gganimate} is available),
#'   otherwise a list of \code{ggplot} frame objects.
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 4, N = 6, seed = 42)
#' mod <- saomnk_model(density = -0.5, popularity = 0.2)
#' saomnk_run(env, mod, steps_per_actor = 5, seed = 12345)
#'
#' ## searchnet 0.8.1: coerce the stored utility table to a plain data.frame
#' env$actor_util_df <- as.data.frame(env$actor_util_df)
#'
#' anim <- saomnk_plot_phase_evolution(env, x_var = "K_AC", y_var = "utility",
#'                                     thin_factor = 5)
#' }
#' @export
saomnk_plot_phase_evolution <- function(env,
                                        x_var = "K_AC",
                                        y_var = "utility",
                                        z_var = "exploration_rate",
                                        trail_length = 10,
                                        fps = 5,
                                        thin_factor = 5,
                                        use_3d = FALSE) {

  df <- .extract_phase_data(env, vars = c(x_var, y_var, z_var), thin_factor = thin_factor)
  df <- df[complete.cases(df[, c(x_var, y_var)]), ]
  if (nrow(df) == 0) stop("No complete observations for the requested variables.")

  x_label <- .phase_var_label(x_var)
  y_label <- .phase_var_label(y_var)

  unique_steps <- sort(unique(df$chain_step_id))

  # -- Try gganimate --
  if (requireNamespace("gganimate", quietly = TRUE)) {
    # Build trail data: for each frame_step, include points from
    # (frame_step - trail_length) through frame_step, with alpha fading
    trail_frames <- lapply(seq_along(unique_steps), function(frame_idx) {
      current_step <- unique_steps[frame_idx]
      trail_start  <- max(1, frame_idx - trail_length)
      trail_steps  <- unique_steps[trail_start:frame_idx]

      trail_df <- df %>% filter(chain_step_id %in% trail_steps)
      trail_df$frame_step  <- current_step
      trail_df$trail_alpha <- (trail_df$chain_step_id - min(trail_steps)) /
                              max(1, current_step - min(trail_steps))
      trail_df$is_current  <- trail_df$chain_step_id == current_step
      trail_df
    })
    anim_df <- bind_rows(trail_frames)

    plt <- ggplot(anim_df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      # Trail points (fading)
      geom_point(
        data = anim_df %>% filter(!is_current),
        aes(alpha = trail_alpha, color = strategy),
        size = 1.5, shape = 16
      ) +
      # Current positions (bright, larger)
      geom_point(
        data = anim_df %>% filter(is_current),
        aes(color = strategy),
        size = 4, shape = 16, alpha = 0.9
      ) +
      # Trajectory lines per actor
      geom_path(
        aes(group = actor_id, color = strategy, alpha = trail_alpha),
        linewidth = 0.4
      ) +
      scale_color_manual(values = .searchnet_strategy_colors, name = "Strategy") +
      scale_alpha_continuous(range = c(0.05, 0.8), guide = "none") +
      labs(x = x_label, y = y_label,
           title = "Phase Space Evolution -- Step {closest_state}") +
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold", colour = .searchnet_palette[["navy"]],
                                  family = "serif", size = 14)
      ) +
      gganimate::transition_states(frame_step, transition_length = 1, state_length = 0) +
      gganimate::ease_aes("linear")

    return(plt)
  }

  # -- Fallback: list of ggplot frames --
  frames <- lapply(seq_along(unique_steps), function(frame_idx) {
    current_step <- unique_steps[frame_idx]
    trail_start  <- max(1, frame_idx - trail_length)
    trail_steps  <- unique_steps[trail_start:frame_idx]

    frame_df   <- df %>% filter(chain_step_id %in% trail_steps)
    current_df <- df %>% filter(chain_step_id == current_step)

    ggplot() +
      # Trail
      geom_point(data = frame_df, aes(x = .data[[x_var]], y = .data[[y_var]],
                                       color = strategy),
                 alpha = 0.15, size = 1, shape = 16) +
      geom_path(data = frame_df, aes(x = .data[[x_var]], y = .data[[y_var]],
                                      group = actor_id, color = strategy),
                alpha = 0.2, linewidth = 0.3) +
      # Current
      geom_point(data = current_df, aes(x = .data[[x_var]], y = .data[[y_var]],
                                         color = strategy),
                 size = 4, shape = 16, alpha = 0.9) +
      scale_color_manual(values = .searchnet_strategy_colors, name = "Strategy") +
      labs(x = x_label, y = y_label,
           title = sprintf("Phase Space Evolution -- Step %d", current_step)) +
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold", colour = .searchnet_palette[["navy"]],
                                  family = "serif", size = 14)
      )
  })

  message(sprintf("Generated %d animation frames. Use lapply(frames, print) to view.", length(frames)))
  invisible(frames)
}


# =============================================================================
# Function 4: saomnk_plot_phase_comparison
# =============================================================================

#' Compare Phase Space Trajectories Across Conditions
#'
#' Side-by-side or overlay of phase space trajectories from multiple simulations
#' (e.g., baseline vs shocked, or different strategy compositions).
#'
#' @param envs Named list of SaomNkRSienaBiEnv objects. Names become condition
#'   labels.
#' @param x_var First dimension variable.
#' @param y_var Second dimension variable.
#' @param z_var Third dimension variable (used for 3D overlay if \pkg{plotly}
#'   is available and \code{overlay = TRUE}).
#' @param overlay Logical; if \code{TRUE}, overlay all conditions on the same
#'   axes. If \code{FALSE}, facet into separate panels (default \code{FALSE}).
#' @param thin_factor Integer thinning factor for chain steps.
#' @param smooth Loess smoothing span for trajectories.
#' @param alpha Point transparency.
#' @return A \code{ggplot} object (2D) or \code{plotly} object (3D overlay).
#' @examples
#' \donttest{
#' env1 <- saomnk_env(M = 4, N = 6, seed = 42)
#' env2 <- saomnk_env(M = 4, N = 6, seed = 42)
#' mod_weak   <- saomnk_model(density = -0.5)
#' mod_strong <- saomnk_model(density = -0.5, popularity = 0.4)
#' saomnk_run(env1, mod_weak,   steps_per_actor = 5, seed = 12345)
#' saomnk_run(env2, mod_strong, steps_per_actor = 5, seed = 12345)
#'
#' ## searchnet 0.8.1: coerce the stored utility tables to plain data.frames
#' env1$actor_util_df <- as.data.frame(env1$actor_util_df)
#' env2$actor_util_df <- as.data.frame(env2$actor_util_df)
#'
#' p <- saomnk_plot_phase_comparison(list(weak = env1, strong = env2),
#'                                   x_var = "K_AC", y_var = "utility")
#' }
#' @export
saomnk_plot_phase_comparison <- function(envs,
                                         x_var = "K_AC",
                                         y_var = "utility",
                                         z_var = "exploration_rate",
                                         overlay = FALSE,
                                         thin_factor = 1,
                                         smooth = 0.3,
                                         alpha = 0.5) {

  if (!is.list(envs) || length(envs) == 0)
    stop("envs must be a named list of SaomNkRSienaBiEnv objects.")

  cond_names <- if (is.null(names(envs))) paste0("Condition_", seq_along(envs)) else names(envs)

  # Extract and combine
  all_dfs <- lapply(seq_along(envs), function(i) {
    df <- .extract_phase_data(envs[[i]], vars = c(x_var, y_var, z_var),
                              thin_factor = thin_factor)
    df$condition <- cond_names[i]
    # Make actor_id unique across conditions
    df$actor_condition <- paste0(cond_names[i], "_", df$actor_id)
    df
  })
  combined <- bind_rows(all_dfs)
  combined$condition <- factor(combined$condition, levels = cond_names)

  combined <- combined[complete.cases(combined[, c(x_var, y_var)]), ]
  if (nrow(combined) == 0) stop("No complete observations across conditions.")

  x_label <- .phase_var_label(x_var)
  y_label <- .phase_var_label(y_var)

  n_conds  <- length(cond_names)
  cond_pal <- .searchnet_strategy_colors[1:min(n_conds, length(.searchnet_strategy_colors))]
  names(cond_pal) <- cond_names

  npoints    <- nrow(combined)
  point_size <- max(0.5, 4 / log10(max(npoints, 10)))

  plt <- ggplot(combined, aes(x = .data[[x_var]], y = .data[[y_var]],
                               color = condition)) +
    geom_point(alpha = alpha * 0.5, size = point_size, shape = 16)

  # Mean trajectories per condition
  if (smooth > 0) {
    plt <- plt +
      geom_smooth(aes(group = condition), method = "loess",
                  span = smooth, se = TRUE, alpha = 0.15, linewidth = 1.2)
  }

  plt <- plt +
    scale_color_manual(values = cond_pal, name = "Condition") +
    labs(
      x     = x_label,
      y     = y_label,
      title = sprintf("Phase Space Comparison: %s vs %s", x_label, y_label),
      subtitle = paste(cond_names, collapse = " | ")
    ) +
    theme_bw() +
    theme(
      plot.title    = element_text(face = "bold", colour = .searchnet_palette[["navy"]],
                                   family = "serif", size = 14),
      plot.subtitle = element_text(colour = .searchnet_palette[["slate"]],
                                   family = "serif", size = 10, face = "italic"),
      legend.position = "bottom"
    )

  if (!overlay) {
    plt <- plt + facet_wrap(~ condition, scales = "free")
  }

  # -- 3D overlay with plotly if requested and available --
  if (overlay && requireNamespace("plotly", quietly = TRUE)) {
    combined_3d <- combined[complete.cases(combined[, c(x_var, y_var, z_var)]), ]
    if (nrow(combined_3d) > 0) {
      z_label <- .phase_var_label(z_var)
      fig <- plotly::plot_ly()
      for (i in seq_along(cond_names)) {
        cdf <- combined_3d %>% filter(condition == cond_names[i])
        fig <- fig %>% plotly::add_trace(
          data = cdf,
          x = ~get(x_var), y = ~get(y_var), z = ~get(z_var),
          color = I(cond_pal[i]),
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 2.5, opacity = alpha),
          name = cond_names[i],
          text = ~paste0("Condition: ", condition,
                         "<br>Actor: ", actor_id,
                         "<br>Step: ", chain_step_id),
          hoverinfo = "text"
        )
      }
      fig <- fig %>% plotly::layout(
        title = list(
          text = sprintf("Phase Space Comparison: %s / %s / %s",
                         .phase_dimension_class(x_var),
                         .phase_dimension_class(y_var),
                         .phase_dimension_class(z_var)),
          font = list(family = "Times New Roman", size = 18, color = "#1b3a5c")
        ),
        scene = list(
          xaxis = list(title = x_label),
          yaxis = list(title = y_label),
          zaxis = list(title = z_label),
          bgcolor = "#fafbfc"
        )
      )
      return(fig)
    }
  }

  plt
}


# =============================================================================
# Function 5: searchnet_export_phase_space
# =============================================================================

#' Export Phase Space Data for External Visualization
#'
#' Export the network-behavior-fitness data to CSV for use with
#' manim, Python, Tableau, or other 3D visualization tools. The exported
#' CSV contains one row per actor per timestep with all requested variables.
#'
#' @param env SaomNkRSienaBiEnv object after simulation.
#' @param vars Character vector of variables to include. Defaults to a
#'   comprehensive set spanning all three dimensions.
#' @param file Output CSV file path.
#' @param thin_factor Integer thinning factor for chain steps (default 1).
#' @return Invisible path to the written file.
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 4, N = 6, seed = 42)
#' mod <- saomnk_model(density = -0.5, popularity = 0.2)
#' saomnk_run(env, mod, steps_per_actor = 5, seed = 12345)
#'
#' f <- tempfile(fileext = ".csv")
#' searchnet_export_phase_space(env, file = f)
#' head(read.csv(f))
#' unlink(f)
#' }
#' @export
searchnet_export_phase_space <- function(env,
                                         vars = c("K_AC", "K_AA",
                                                   "exploration_rate", "turnover_rate",
                                                   "utility", "utility_change", "rank"),
                                         file = "phase_space_data.csv",
                                         thin_factor = 1) {

  df <- .extract_phase_data(env, vars = vars, thin_factor = thin_factor)

  # Select output columns
  out_cols <- c("chain_step_id", "actor_id", "strategy", "time_norm", "phase")
  out_cols <- c(out_cols, intersect(vars, names(df)))
  out_df   <- df[, out_cols, drop = FALSE]

  write.csv(out_df, file = file, row.names = FALSE)
  message(sprintf("Phase space data exported: %s (%d rows, %d columns)",
                  file, nrow(out_df), ncol(out_df)))
  invisible(file)
}
