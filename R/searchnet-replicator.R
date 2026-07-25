#' @title Replicator Dynamics and Evolutionary Game Theory
#' @description Functions for running replicator dynamics, ELO tournaments,
#'   and computing evolutionarily stable strategies (ESS) for policy type
#'   populations in the SaoMNK framework.
#' @name searchnet-replicator

# Okabe-Ito policy colors
.policy_colors <- c(RS = "#1a1a1a", RE = "#E69F00", RR = "#56B4E9", RF = "#009E73")

#' Run Replicator Dynamics on Policy Type Populations
#'
#' Implements discrete-time replicator dynamics with softmax selection.
#' The replicator equation: x_i(t+1) = x_i(t) * exp(s * f_i) / Z
#' where s is selection strength and Z is the normalization constant.
#'
#' @param fitness_matrix Matrix of fitness values. Rows = policy types,
#'   columns = replications or conditions. If a named vector, treated as
#'   a single condition.
#' @param population Numeric vector of initial population shares (must sum to 1).
#'   If NULL, starts with equal shares.
#' @param generations Integer number of generations to simulate (default 500).
#' @param selection_strength Numeric selection intensity (default 200).
#'   Higher values = stronger selection pressure.
#' @param policy_labels Character vector of policy type names.
#' @return Data frame with columns: generation, policy_type, share, fitness
#' @export
saomnk_replicator_dynamics <- function(fitness_matrix,
                                        population = NULL,
                                        generations = 500L,
                                        selection_strength = 200,
                                        policy_labels = c("RS", "RE", "RR", "RF")) {
  # Handle vector input
  if (is.null(dim(fitness_matrix))) {
    fitness <- as.numeric(fitness_matrix)
  } else {
    fitness <- rowMeans(fitness_matrix)
  }
  n_types <- length(fitness)

  if (is.null(population)) {
    population <- rep(1 / n_types, n_types)
  }
  stopifnot(abs(sum(population) - 1) < 1e-6)
  stopifnot(length(population) == n_types)

  if (length(policy_labels) != n_types) {
    policy_labels <- paste0("Type_", seq_len(n_types))
  }

  # Normalize fitness to [0, 1] range for numerical stability
  f_min <- min(fitness)
  f_max <- max(fitness)
  if (f_max > f_min) {
    f_norm <- (fitness - f_min) / (f_max - f_min)
  } else {
    f_norm <- rep(0.5, n_types)
  }

  # Storage
  results <- vector("list", generations + 1)
  x <- population

  for (g in 0:generations) {
    results[[g + 1]] <- data.frame(
      generation = g,
      policy_type = policy_labels,
      share = x,
      fitness = fitness,
      stringsAsFactors = FALSE
    )

    if (g < generations) {
      # Softmax replicator update
      logits <- selection_strength * f_norm
      logits <- logits - max(logits)  # numerical stability
      weights <- x * exp(logits)
      x <- weights / sum(weights)
      # Clamp to avoid numerical extinction
      x <- pmax(x, 1e-12)
      x <- x / sum(x)
    }
  }

  do.call(rbind, results)
}


#' Compute Evolutionarily Stable Strategy (ESS)
#'
#' Extracts terminal population shares from replicator dynamics
#' as the evolutionarily stable strategy.
#'
#' @param replicator_result Data frame output from saomnk_replicator_dynamics
#' @param threshold Numeric minimum share to be considered present (default 0.01)
#' @return Named numeric vector of ESS population shares
#' @export
saomnk_ess <- function(replicator_result, threshold = 0.01) {
  max_gen <- max(replicator_result$generation)
  terminal <- replicator_result[replicator_result$generation == max_gen, ]
  ess <- setNames(terminal$share, terminal$policy_type)
  # Flag types below threshold
  attr(ess, "extinct") <- names(ess)[ess < threshold]
  attr(ess, "surviving") <- names(ess)[ess >= threshold]
  ess
}


#' Run ELO Tournament Between Policy Types
#'
#' Runs a round-robin ELO tournament where policy types compete
#' head-to-head based on fitness outcomes from simulation.
#'
#' @param fitness_by_type Named list of fitness vectors, one per policy type.
#'   Each vector contains fitness outcomes from multiple replications.
#' @param matches_per_pair Integer number of matches per pairwise comparison
#'   (default 60).
#' @param K_elo Numeric ELO update constant (default 32).
#' @param initial_rating Numeric starting ELO rating (default 1500).
#' @param policy_labels Character vector of policy type labels.
#' @return List with components:
#'   \item{ratings}{Named numeric vector of final ELO ratings}
#'   \item{history}{Data frame of match results}
#'   \item{parity_test}{Logical: are all ratings within 20 points?}
#' @export
saomnk_elo_tournament <- function(fitness_by_type,
                                   matches_per_pair = 60L,
                                   K_elo = 32,
                                   initial_rating = 1500,
                                   policy_labels = names(fitness_by_type)) {
  n_types <- length(fitness_by_type)
  if (is.null(policy_labels)) {
    policy_labels <- paste0("Type_", seq_len(n_types))
  }

  ratings <- setNames(rep(initial_rating, n_types), policy_labels)
  history <- list()
  match_id <- 0

  for (i in seq_len(n_types - 1)) {
    for (j in (i + 1):n_types) {
      for (m in seq_len(matches_per_pair)) {
        match_id <- match_id + 1

        # Sample fitness from each type
        f_i <- sample(fitness_by_type[[i]], 1)
        f_j <- sample(fitness_by_type[[j]], 1)

        # Determine outcome
        if (f_i > f_j) {
          s_i <- 1; s_j <- 0
        } else if (f_j > f_i) {
          s_i <- 0; s_j <- 1
        } else {
          s_i <- 0.5; s_j <- 0.5
        }

        # Expected scores
        e_i <- 1 / (1 + 10^((ratings[j] - ratings[i]) / 400))
        e_j <- 1 - e_i

        # Update ratings
        ratings[i] <- ratings[i] + K_elo * (s_i - e_i)
        ratings[j] <- ratings[j] + K_elo * (s_j - e_j)

        history[[match_id]] <- data.frame(
          match = match_id,
          type_a = policy_labels[i],
          type_b = policy_labels[j],
          fitness_a = f_i,
          fitness_b = f_j,
          outcome_a = s_i,
          rating_a = ratings[i],
          rating_b = ratings[j],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  history_df <- do.call(rbind, history)
  rating_range <- max(ratings) - min(ratings)

  list(
    ratings = ratings,
    history = history_df,
    parity_test = rating_range < 20,
    rating_range = rating_range
  )
}


#' Plot Replicator Dynamics Trajectories
#'
#' Creates a line plot showing how population shares evolve over generations.
#'
#' @param replicator_result Data frame output from saomnk_replicator_dynamics
#' @param colors Named vector of colors for policy types (default Okabe-Ito)
#' @param title Character plot title
#' @return ggplot object
#' @export
saomnk_plot_replicator <- function(replicator_result,
                                    colors = .policy_colors,
                                    title = "Replicator Dynamics: Evolutionary Competition Among Policy Types") {
  requireNamespace("ggplot2", quietly = TRUE)

  ggplot2::ggplot(replicator_result,
                  ggplot2::aes(x = generation, y = share,
                               color = policy_type, group = policy_type)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = title,
      x = "Generation",
      y = "Population Share",
      color = "Policy Type"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "bottom"
    )
}


#' Plot Replicator Dynamics on Strategy Simplex
#'
#' Creates a 2D De Finetti diagram (simplex projection) showing
#' evolutionary pressure and population trajectory.
#'
#' @param replicator_result Data frame from saomnk_replicator_dynamics
#' @param x_type Character name of policy type for x-axis
#' @param y_type Character name of policy type for y-axis
#' @param colors Named color vector
#' @param title Character plot title
#' @return ggplot object
#' @export
saomnk_plot_simplex <- function(replicator_result,
                                 x_type = "RE", y_type = "RF",
                                 colors = .policy_colors,
                                 title = "Strategy Simplex") {
  requireNamespace("ggplot2", quietly = TRUE)

  # Reshape to wide format
  wide <- stats::reshape(
    replicator_result[, c("generation", "policy_type", "share")],
    idvar = "generation", timevar = "policy_type",
    direction = "wide"
  )
  names(wide) <- gsub("share\\.", "", names(wide))

  x_col <- x_type
  y_col <- y_type

  ggplot2::ggplot(wide, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
    ggplot2::geom_path(color = colors[x_type], linewidth = 1, alpha = 0.7) +
    ggplot2::geom_point(data = wide[1, ], size = 4, shape = 16, color = "black") +
    ggplot2::geom_point(data = wide[nrow(wide), ], size = 5, shape = 8,
                        color = colors[y_type], stroke = 2) +
    ggplot2::labs(
      title = title,
      x = paste0(x_type, " Share"),
      y = paste0(y_type, " Share")
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(text = ggplot2::element_text(family = "serif"))
}


#' Plot ELO Tournament Results
#'
#' Bar chart of final ELO ratings with parity band.
#'
#' @param tournament_result List output from saomnk_elo_tournament
#' @param colors Named color vector
#' @param title Character plot title
#' @return ggplot object
#' @export
saomnk_plot_elo <- function(tournament_result,
                             colors = .policy_colors,
                             title = "ELO Tournament Ratings") {
  requireNamespace("ggplot2", quietly = TRUE)

  df <- data.frame(
    policy_type = names(tournament_result$ratings),
    rating = as.numeric(tournament_result$ratings),
    stringsAsFactors = FALSE
  )

  mean_rating <- mean(df$rating)

  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(policy_type, -rating),
                                    y = rating, fill = policy_type)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_hline(yintercept = mean_rating, linetype = "dashed", color = "gray50") +
    ggplot2::geom_hline(yintercept = c(mean_rating - 10, mean_rating + 10),
                        linetype = "dotted", color = "gray70") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Rating range: ", round(tournament_result$rating_range, 1),
                        " | Parity: ", ifelse(tournament_result$parity_test, "YES", "NO")),
      x = "Policy Type",
      y = "ELO Rating"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "none"
    )
}
