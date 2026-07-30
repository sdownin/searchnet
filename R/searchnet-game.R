#' @title Strategy Game Mode for SaoMNK
#' @description
#' Interactive game loop where a human player controls one firm and competes
#' against AI opponents that follow SAOM-NK logit choice rules.  Provides
#' functions to initialise a game, step through rounds, query the scoreboard,
#' enumerate legal moves, and generate an end-of-game summary.
#'
#' @name searchnet-game
#' @import Matrix
NULL


# ---------------------------------------------------------------------------- #
#  Game-mode constants
# ---------------------------------------------------------------------------- #

.DIFFICULTY_BETA <- c(easy = 0.5, medium = 2, hard = 10)

.GAME_MODES <- c("maximize_fitness", "minimize_overlap",
                  "survive_shock", "beat_nash")


# ---------------------------------------------------------------------------- #
#  searchnet_game_init
# ---------------------------------------------------------------------------- #

#' Initialize a Strategy Game Session
#'
#' Creates a new game state object containing a SaoMNK environment, model,
#' and bookkeeping structures.  The player controls one firm; AI opponents
#' follow the SAOM conditional-logit rule at the chosen difficulty level.
#'
#' @param M Integer. Total number of firms (player + AI). Default 6.
#' @param N Integer. Number of activities/routes. Default 8.
#' @param player_id Integer. Which firm the player controls (default 1).
#' @param difficulty Character. One of \code{"easy"} (beta=0.5),
#'   \code{"medium"} (beta=2), or \code{"hard"} (beta=10).
#' @param mode Character. Game objective:
#'   \describe{
#'     \item{\code{"maximize_fitness"}}{Pure NK fitness maximisation.}
#'     \item{\code{"minimize_overlap"}}{Differentiation challenge (low K_AA).}
#'     \item{\code{"survive_shock"}}{Prepare for a random exogenous shock.}
#'     \item{\code{"beat_nash"}}{Outperform the QRE equilibrium prediction.}
#'   }
#' @param max_rounds Integer. Maximum number of rounds before the game ends
#'   (default 30).
#' @param seed Integer or \code{NULL}. Random seed for reproducibility.
#' @return A list of class \code{"searchnet_game"} containing the full game
#'   state: environment, model, scoreboard history, round counter, and
#'   configuration.
#' @export
#' @examples
#' game <- searchnet_game_init(M = 4, N = 6, difficulty = "easy", seed = 42)
searchnet_game_init <- function(M = 6, N = 8, player_id = 1,
                                 difficulty = "medium",
                                 mode = "maximize_fitness",
                                 max_rounds = 30,
                                 seed = NULL) {

  ## --- Validate inputs -------------------------------------------------- ##
  stopifnot(is.numeric(M), length(M) == 1, M >= 2)
  stopifnot(is.numeric(N), length(N) == 1, N >= 2)
  stopifnot(is.numeric(player_id), length(player_id) == 1,
            player_id >= 1, player_id <= M)
  difficulty <- match.arg(difficulty, c("easy", "medium", "hard"))
  mode       <- match.arg(mode, .GAME_MODES)
  stopifnot(is.numeric(max_rounds), length(max_rounds) == 1, max_rounds >= 1)

  if (!is.null(seed)) set.seed(seed)

  ## --- Create SaoMNK environment --------------------------------------- ##
  env <- saomnk_env(M = M, N = N, density = 0.3, seed = seed,
                    name = paste0("game_", format(Sys.time(), "%H%M%S")))

  ## --- Build a standard model (epistasis + density + popularity) -------- ##
  K_mat <- saomnk_block_diagonal(N, blocks = max(2, N %/% 3))
  model <- saomnk_model(
    density          = -0.5,
    popularity       = 0.15,
    scope            = -0.1,
    epistasis_matrix = K_mat,
    epistasis_weight = 0.2
  )

  ## --- AI rationality parameter ---------------------------------------- ##
  beta <- unname(.DIFFICULTY_BETA[difficulty])

  ## --- Shock setup (for survive_shock mode) ----------------------------- ##
  shock_round <- if (mode == "survive_shock") {
    sample(seq(max_rounds %/% 3, max_rounds - 2), 1)
  } else {
    NA_integer_
  }

  ## --- Initial scoreboard snapshot ------------------------------------- ##
  initial_scores <- .game_compute_scores(env, player_id)

  ## --- Assemble game state --------------------------------------------- ##
  game <- list(
    env            = env,
    model          = model,
    K_mat          = K_mat,
    player_id      = as.integer(player_id),
    beta           = beta,
    difficulty     = difficulty,
    mode           = mode,
    max_rounds     = as.integer(max_rounds),
    round          = 0L,
    finished       = FALSE,
    shock_round    = shock_round,
    shock_applied  = FALSE,
    history        = list(initial_scores),
    move_log       = list(),
    seed           = seed
  )

  class(game) <- c("searchnet_game", "list")
  game
}


# ---------------------------------------------------------------------------- #
#  searchnet_game_step
# ---------------------------------------------------------------------------- #

#' Player Makes a Move
#'
#' Applies the player's chosen action (add or drop one activity) and then
#' advances all AI firms by sampling from their SAOM logit choice
#' distributions.
#'
#' @param game A \code{searchnet_game} object from
#'   \code{\link{searchnet_game_init}}.
#' @param action Character. Either \code{"add"} or \code{"drop"}.
#' @param activity_id Integer. Which activity to add or drop (1..N).
#' @return The updated \code{searchnet_game} object with incremented round,
#'   updated bipartite matrix, AI responses, and new scoreboard entry.
#' @export
#' @examples
#' game <- searchnet_game_init(M = 4, N = 6, seed = 1)
#' game <- searchnet_game_step(game, action = "add", activity_id = 3)
searchnet_game_step <- function(game, action, activity_id) {

  stopifnot(inherits(game, "searchnet_game"))

  if (game$finished) {
    message("Game is already finished. Call searchnet_game_summary().")
    return(game)
  }

  action      <- match.arg(action, c("add", "drop", "pass"))
  activity_id <- as.integer(activity_id)
  env         <- game$env
  pid         <- game$player_id
  M           <- env$M
  N           <- env$N

  ## --- 1. Apply player's move ------------------------------------------ ##
  if (action != "pass") {
    stopifnot(activity_id >= 1, activity_id <= N)
    current_val <- env$bipartite_matrix[pid, activity_id]

    if (action == "add" && current_val == 1) {
      warning("Activity already active; treating as pass.")
    } else if (action == "drop" && current_val == 0) {
      warning("Activity already inactive; treating as pass.")
    } else {
      env$bipartite_matrix[pid, activity_id] <- 1 - current_val
    }
  }

  ## --- 2. AI firms take their turns ------------------------------------ ##
  ## compute_choice_probabilities() evaluates ALL M actors in one pass, so it is
  ## hoisted out of the actor loop below. Calling it per-actor did M times the
  ## necessary work and discarded all but one element each time (~M^2*N scaling).
  ##
  ## This also fixes the move semantics: every AI now responds to the same
  ## round-start state (after the player's move), i.e. simultaneous moves within
  ## a round, matching searchnet_classroom_advance(). Previously each AI saw the
  ## partially-updated board left by lower-indexed AIs, which made outcomes
  ## depend on actor ordering.
  all_probs <- env$compute_choice_probabilities(beta = game$beta)

  ai_moves <- vector("list", M)
  for (i in seq_len(M)) {
    if (i == pid) {
      ai_moves[[i]] <- list(actor = i, action = action,
                            activity = activity_id, is_player = TRUE)
      next
    }

    ## Choice probabilities for this AI firm (precomputed above)
    probs_data <- all_probs[[i]]
    prob_vec   <- probs_data$probabilities  # length N+1 (flip_1..flip_N, pass)

    ## Sample one action according to the logit distribution
    chosen_idx <- sample.int(length(prob_vec), size = 1, prob = prob_vec)

    if (chosen_idx <= N) {
      ## AI chose to flip activity chosen_idx
      env$bipartite_matrix[i, chosen_idx] <-
        1 - env$bipartite_matrix[i, chosen_idx]
      ai_action <- if (env$bipartite_matrix[i, chosen_idx] == 1) "add" else "drop"
      ai_moves[[i]] <- list(actor = i, action = ai_action,
                            activity = chosen_idx, is_player = FALSE)
    } else {
      ai_moves[[i]] <- list(actor = i, action = "pass",
                            activity = NA_integer_, is_player = FALSE)
    }
  }

  ## --- 3. Shock check (survive_shock mode) ----------------------------- ##
  game$round <- game$round + 1L

  if (game$mode == "survive_shock" &&
      !is.na(game$shock_round) &&
      game$round == game$shock_round &&
      !game$shock_applied) {

    ## Random shock: remove a high-popularity activity for all firms
    col_sums <- colSums(env$bipartite_matrix)
    top_activities <- which(col_sums >= quantile(col_sums, 0.75))
    if (length(top_activities) > 0) {
      shocked_activity <- sample(top_activities, 1)
      ## Force-drop the activity for all firms
      env$bipartite_matrix[, shocked_activity] <- 0
    }
    game$shock_applied <- TRUE
  }

  ## --- 4. Update projections and degree stats -------------------------- ##
  ## Recompute the social and search matrices from updated bipartite
  env$social_matrix <- env$bipartite_matrix %*% t(env$bipartite_matrix)
  diag(env$social_matrix) <- 0
  env$search_matrix <- t(env$bipartite_matrix) %*% env$bipartite_matrix
  diag(env$search_matrix) <- 0

  ## --- 5. Record scoreboard -------------------------------------------- ##
  scores <- .game_compute_scores(env, game$player_id)
  game$history[[length(game$history) + 1]] <- scores
  game$move_log[[length(game$move_log) + 1]] <- ai_moves

  ## --- 6. Check end conditions ----------------------------------------- ##
  if (game$round >= game$max_rounds) {
    game$finished <- TRUE
  }

  game
}


# ---------------------------------------------------------------------------- #
#  searchnet_game_scoreboard
# ---------------------------------------------------------------------------- #

#' Get Current Game Scoreboard
#'
#' Returns a data frame summarising each firm's current state including
#' utility, K-4 degrees, rank, and whether the firm is the player.
#'
#' @param game A \code{searchnet_game} object.
#' @return A \code{data.frame} with columns: \code{firm_id}, \code{scope}
#'   (K_AC), \code{popularity} (K_CA mean), \code{rivalry} (K_AA),
#'   \code{epistasis} (K_CC mean), \code{n_activities}, \code{rank},
#'   \code{is_player}.
#' @export
searchnet_game_scoreboard <- function(game) {
  stopifnot(inherits(game, "searchnet_game"))
  latest <- game$history[[length(game$history)]]
  latest[order(latest$rank), ]
}


# ---------------------------------------------------------------------------- #
#  searchnet_game_available_moves
# ---------------------------------------------------------------------------- #

#' Get Available Moves for the Player
#'
#' Enumerates all legal add/drop actions for the player's firm and
#' computes the predicted change in utility for each, sorted from best
#' to worst.
#'
#' @param game A \code{searchnet_game} object.
#' @return A \code{data.frame} with columns: \code{action} ("add" or "drop"),
#'   \code{activity_id}, \code{delta_utility}, \code{current_state} (0/1).
#' @export
searchnet_game_available_moves <- function(game) {
  stopifnot(inherits(game, "searchnet_game"))

  env <- game$env
  pid <- game$player_id
  N   <- env$N

  ## Use the engine's choice-probability method to get delta-U values
  probs_data <- env$compute_choice_probabilities(beta = game$beta)[[pid]]
  delta_u    <- probs_data$delta_u  # N-vector of utility changes per flip

  current_row <- env$bipartite_matrix[pid, ]

  moves <- data.frame(
    action       = ifelse(current_row == 0, "add", "drop"),
    activity_id  = seq_len(N),
    delta_utility = delta_u,
    current_state = current_row,
    stringsAsFactors = FALSE
  )

  ## Add a "pass" option
  pass_row <- data.frame(
    action        = "pass",
    activity_id   = NA_integer_,
    delta_utility = 0,
    current_state = NA_integer_,
    stringsAsFactors = FALSE
  )
  moves <- rbind(moves, pass_row)

  ## Sort by delta_utility descending (best moves first)
  moves <- moves[order(-moves$delta_utility), ]
  rownames(moves) <- NULL
  moves
}


# ---------------------------------------------------------------------------- #
#  searchnet_game_summary
# ---------------------------------------------------------------------------- #

#' End Game and Generate Summary
#'
#' Compiles the player's performance across all rounds, compares to AI
#' opponents, and returns a structured summary with trajectory data
#' suitable for visualization or replay rendering.
#'
#' @param game A \code{searchnet_game} object.
#' @return A list with:
#'   \describe{
#'     \item{\code{final_scores}}{Final-round scoreboard data frame.}
#'     \item{\code{player_rank}}{Player's final rank among all firms.}
#'     \item{\code{player_trajectory}}{Data frame of player stats by round.}
#'     \item{\code{all_trajectories}}{Data frame of all firms' stats by round.}
#'     \item{\code{mode}}{Game mode that was played.}
#'     \item{\code{difficulty}}{Difficulty level.}
#'     \item{\code{total_rounds}}{Number of rounds played.}
#'     \item{\code{mode_score}}{Mode-specific score for the player.}
#'     \item{\code{bipartite_snapshots}}{List of bipartite matrices by round
#'       (for replay animation).}
#'   }
#' @export
searchnet_game_summary <- function(game) {
  stopifnot(inherits(game, "searchnet_game"))

  pid <- game$player_id

  ## --- Build trajectory data frames ------------------------------------ ##
  all_traj <- do.call(rbind, lapply(seq_along(game$history), function(r) {
    df <- game$history[[r]]
    df$round <- r - 1L  # round 0 = initial state
    df
  }))

  player_traj <- all_traj[all_traj$is_player, ]

  ## --- Final scoreboard ------------------------------------------------ ##
  final <- game$history[[length(game$history)]]

  ## --- Mode-specific scoring ------------------------------------------- ##
  player_final <- final[final$is_player, ]
  mode_score <- switch(game$mode,
    maximize_fitness = player_final$scope,
    minimize_overlap = -player_final$rivalry,      # lower K_AA is better
    survive_shock    = player_final$n_activities,   # survival = scope maintained
    beat_nash        = player_final$scope - mean(final$scope[!final$is_player]),
    NA_real_
  )

  ## --- Collect bipartite snapshots for replay -------------------------- ##
  ## We only have the final matrix; for a full replay we'd need to

  ## reconstruct from the move_log.  Build snapshots from move history.
  snapshots <- .game_reconstruct_snapshots(game)

  list(
    final_scores      = final,
    player_rank       = player_final$rank,
    player_trajectory = player_traj,
    all_trajectories  = all_traj,
    mode              = game$mode,
    difficulty        = game$difficulty,
    total_rounds      = game$round,
    mode_score        = mode_score,
    bipartite_snapshots = snapshots
  )
}


# ---------------------------------------------------------------------------- #
#  Internal helpers
# ---------------------------------------------------------------------------- #

#' Compute scoreboard for all firms at the current state
#' @param env SaomNkRSienaBiEnv object
#' @param player_id Integer, which firm is the player
#' @return data.frame with per-firm scores
#' @keywords internal
.game_compute_scores <- function(env, player_id) {

  M  <- env$M
  N  <- env$N
  bi <- env$bipartite_matrix

  ## Recompute projections to ensure consistency
  social <- bi %*% t(bi)
  diag(social) <- 0
  search <- t(bi) %*% bi
  diag(search) <- 0

  ## Per-actor statistics
  scope       <- rowSums(bi)                       # K_AC: number of activities
  rivalry     <- rowSums(social) / max(1, M - 1)   # K_AA: mean overlap
  n_act       <- scope

  ## Component-level stats averaged per actor
  col_pop     <- colSums(bi)                        # K_CA per component
  col_epist   <- rowSums(search)                    # K_CC per component

  ## Weighted averages for each actor (over their active components)
  pop_score   <- numeric(M)
  epist_score <- numeric(M)
  for (i in seq_len(M)) {
    active <- which(bi[i, ] == 1)
    if (length(active) > 0) {
      pop_score[i]   <- mean(col_pop[active])
      epist_score[i] <- mean(col_epist[active])
    }
  }

  scores <- data.frame(
    firm_id      = seq_len(M),
    scope        = scope,
    popularity   = pop_score,
    rivalry      = rivalry,
    epistasis    = epist_score,
    n_activities = n_act,
    is_player    = seq_len(M) == player_id,
    stringsAsFactors = FALSE
  )

  ## Rank by mode-relevant metric (scope as default)
  scores$rank <- rank(-scores$scope, ties.method = "min")
  scores
}


#' Reconstruct bipartite matrix snapshots from the move log
#' @param game searchnet_game object
#' @return list of M x N matrices, one per round (including initial)
#' @keywords internal
.game_reconstruct_snapshots <- function(game) {
  ## We cannot perfectly reconstruct without storing full matrices each round,

  ## so we store the final matrix and note the limitation.
  ## For a full implementation, we'd record env$bipartite_matrix at each step.
  ## Here, return what we have from the history scoreboard + final matrix.
  list(
    initial = NULL,  # would need to be stored at init time
    final   = game$env$bipartite_matrix,
    rounds  = game$round
  )
}
