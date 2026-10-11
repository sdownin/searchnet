## Replicator dynamics and Elo tournaments over strategy profiles (internal).
##
## Functions for running replicator dynamics over a population of strategy
## profiles, extracting terminal shares, and running a round-robin Elo
## tournament between profiles from their fitness draws. None is exported.

# Okabe-Ito colors, one per default strategy profile
.profile_colors <- c(A = "#1a1a1a", B = "#E69F00", C = "#56B4E9", D = "#009E73")

#' Replicator dynamics over strategy profiles
#'
#' Implements discrete-time replicator dynamics with softmax selection.
#' The replicator equation: x_i(t+1) = x_i(t) * exp(s * f_i) / Z
#' where s is selection strength and Z is the normalization constant.
#'
#' @param fitness_matrix Matrix of fitness values. Rows = strategy profiles,
#'   columns = replications or conditions. If a named vector, treated as
#'   a single condition.
#' @param population Numeric vector of initial population shares (must sum to 1).
#'   If NULL, starts with equal shares.
#' @param generations Integer number of generations to simulate (default 500).
#' @param selection_strength Numeric selection intensity (default 200).
#'   Higher values = stronger selection pressure.
#' @param profile_labels Character vector of profile names. If its length
#'   does not match the number of profiles, "Type_1", "Type_2", ... are used.
#' @return Data frame with columns: generation, profile, share, fitness
#' @examples
#' traj <- replicator_dynamics(c(A = 0.80, B = 0.81, C = 0.79, D = 0.82),
#'                             generations = 100)
#' tail(traj, 4)   # terminal population shares
#' @keywords internal
#' @noRd
replicator_dynamics <- function(fitness_matrix,
                                population = NULL,
                                generations = 500L,
                                selection_strength = 200,
                                profile_labels = c("A", "B", "C", "D")) {
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

  if (length(profile_labels) != n_types) {
    profile_labels <- paste0("Type_", seq_len(n_types))
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
      profile = profile_labels,
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


#' Terminal shares of a replicator run (evolutionarily stable strategy)
#'
#' Extracts terminal population shares from replicator dynamics
#' as the evolutionarily stable strategy.
#'
#' @param replicator_result Data frame output from replicator_dynamics()
#' @param threshold Numeric minimum share to be considered present (default 0.01)
#' @return Named numeric vector of terminal shares, with attributes
#'   "extinct" and "surviving"
#' @keywords internal
#' @noRd
replicator_ess <- function(replicator_result, threshold = 0.01) {
  max_gen <- max(replicator_result$generation)
  terminal <- replicator_result[replicator_result$generation == max_gen, ]
  ess <- setNames(terminal$share, terminal$profile)
  # Flag profiles below threshold
  attr(ess, "extinct") <- names(ess)[ess < threshold]
  attr(ess, "surviving") <- names(ess)[ess >= threshold]
  ess
}


#' Elo tournament between strategy profiles
#'
#' Runs a round-robin Elo tournament where strategy profiles compete
#' head-to-head on fitness outcomes drawn from simulation.
#'
#' @param fitness_by_profile Named list of fitness vectors, one per profile.
#'   Each vector contains fitness outcomes from multiple replications.
#' @param matches_per_pair Integer number of matches per pairwise comparison
#'   (default 60).
#' @param K_elo Numeric Elo update constant (default 32).
#' @param initial_rating Numeric starting Elo rating (default 1500).
#' @param profile_labels Character vector of profile labels.
#' @return List with components ratings (named numeric vector of final
#'   ratings), history (data frame of match results), ratings_within_20
#'   (logical: are all ratings within 20 points?) and rating_range.
#' @keywords internal
#' @noRd
elo_tournament <- function(fitness_by_profile,
                           matches_per_pair = 60L,
                           K_elo = 32,
                           initial_rating = 1500,
                           profile_labels = names(fitness_by_profile)) {
  n_types <- length(fitness_by_profile)
  if (is.null(profile_labels)) {
    profile_labels <- paste0("Type_", seq_len(n_types))
  }

  ratings <- setNames(rep(initial_rating, n_types), profile_labels)
  history <- list()
  match_id <- 0

  for (i in seq_len(n_types - 1)) {
    for (j in (i + 1):n_types) {
      for (m in seq_len(matches_per_pair)) {
        match_id <- match_id + 1

        # Sample fitness from each profile
        f_i <- sample(fitness_by_profile[[i]], 1)
        f_j <- sample(fitness_by_profile[[j]], 1)

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
          type_a = profile_labels[i],
          type_b = profile_labels[j],
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
    ratings_within_20 = rating_range < 20,
    rating_range = rating_range
  )
}


#' Plot replicator population shares over generations
#'
#' @param replicator_result Data frame output from replicator_dynamics()
#' @param colors Named vector of colors, one per profile (default Okabe-Ito)
#' @param title Character plot title
#' @return ggplot object
#' @keywords internal
#' @noRd
plot_replicator_shares <- function(replicator_result,
                                   colors = .profile_colors,
                                   title = "Replicator Dynamics Across Strategy Profiles") {
  requireNamespace("ggplot2", quietly = TRUE)

  ggplot2::ggplot(replicator_result,
                  ggplot2::aes(x = generation, y = share,
                               color = profile, group = profile)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = title,
      x = "Generation",
      y = "Population Share",
      color = "Strategy Profile"
    ) +
    theme_searchnet(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "bottom"
    )
}


#' Plot a replicator run on two coordinates of the strategy simplex
#'
#' Plots the population trajectory projected on the shares of two
#' profiles (start: dot; end: star).
#'
#' @param replicator_result Data frame from replicator_dynamics()
#' @param x_profile Character name of the profile on the x-axis
#' @param y_profile Character name of the profile on the y-axis
#' @param colors Named color vector
#' @param title Character plot title
#' @return ggplot object
#' @keywords internal
#' @noRd
plot_replicator_simplex <- function(replicator_result,
                                    x_profile = "B", y_profile = "D",
                                    colors = .profile_colors,
                                    title = "Strategy Simplex") {
  requireNamespace("ggplot2", quietly = TRUE)

  # Reshape to wide format
  wide <- stats::reshape(
    replicator_result[, c("generation", "profile", "share")],
    idvar = "generation", timevar = "profile",
    direction = "wide"
  )
  names(wide) <- gsub("share\\.", "", names(wide))

  x_col <- x_profile
  y_col <- y_profile

  ggplot2::ggplot(wide, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
    ggplot2::geom_path(color = colors[x_profile], linewidth = 1, alpha = 0.7) +
    ggplot2::geom_point(data = wide[1, ], size = 4, shape = 16, color = "black") +
    ggplot2::geom_point(data = wide[nrow(wide), ], size = 5, shape = 8,
                        color = colors[y_profile], stroke = 2) +
    ggplot2::labs(
      title = title,
      x = paste0(x_profile, " Share"),
      y = paste0(y_profile, " Share")
    ) +
    ggplot2::coord_equal() +
    theme_searchnet(base_size = 12) +
    ggplot2::theme(text = ggplot2::element_text(family = "serif"))
}


#' Plot Elo tournament ratings
#'
#' Bar chart of final Elo ratings with a +/- 10 point band around the mean.
#'
#' @param tournament_result List output from elo_tournament()
#' @param colors Named color vector
#' @param title Character plot title
#' @return ggplot object
#' @keywords internal
#' @noRd
plot_elo_ratings <- function(tournament_result,
                             colors = .profile_colors,
                             title = "Elo Tournament Ratings") {
  requireNamespace("ggplot2", quietly = TRUE)

  df <- data.frame(
    profile = names(tournament_result$ratings),
    rating = as.numeric(tournament_result$ratings),
    stringsAsFactors = FALSE
  )

  mean_rating <- mean(df$rating)

  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(profile, -rating),
                                    y = rating, fill = profile)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_hline(yintercept = mean_rating, linetype = "dashed", color = "gray50") +
    ggplot2::geom_hline(yintercept = c(mean_rating - 10, mean_rating + 10),
                        linetype = "dotted", color = "gray70") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Rating range: ", round(tournament_result$rating_range, 1),
                        " | Within 20 points: ",
                        ifelse(tournament_result$ratings_within_20, "YES", "NO")),
      x = "Strategy Profile",
      y = "Elo Rating"
    ) +
    theme_searchnet(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      legend.position = "none"
    )
}
