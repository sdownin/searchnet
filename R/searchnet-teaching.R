#' @title Classroom Teaching Module for searchnet
#' @description
#' Capsim-style strategy simulation functions for classroom use. Students play
#' as firm executives making strategic decisions on a shared bipartite landscape.
#' Each student controls one actor; AI opponents fill out the competitive field.
#' Supports multiple industry presets (airline, tech, pharma) and difficulty
#' levels with optional exogenous shocks.
#'
#' @name searchnet-teaching
#' @import Matrix
NULL


# ---------------------------------------------------------------------------- #
#  Internal helpers
# ---------------------------------------------------------------------------- #

#' Load a teaching preset JSON
#' @param industry Character. Industry name ("airline", "tech", "pharma").
#' @return A list parsed from the JSON preset file.
#' @keywords internal
.load_teaching_preset <- function(industry) {
  preset_file <- system.file(
    "teaching", "presets", paste0(industry, "_preset.json"),
    package = "searchnet"
  )
  if (preset_file == "" || !file.exists(preset_file)) {
    # Fallback: try local path (dev mode)
    preset_file <- file.path(
      find.package("searchnet", quiet = TRUE)[1] %||% ".",
      "inst", "teaching", "presets", paste0(industry, "_preset.json")
    )
  }
  if (!file.exists(preset_file)) {
    # Final fallback for development
    dev_path <- file.path("D:/Search_networks/SaoMNK/inst/teaching/presets",
                          paste0(industry, "_preset.json"))
    if (file.exists(dev_path)) {
      preset_file <- dev_path
    } else {
      stop("Preset file not found for industry '", industry, "'. ",
           "Available presets: airline, tech, pharma", call. = FALSE)
    }
  }
  jsonlite::fromJSON(preset_file, simplifyVector = FALSE)
}


#' Null-coalescing operator (if not already defined)
#'
#' Internal, unexported infix operator.  No help page is generated: an Rd name
#' may not contain the vertical bar, so the topic that roxygen produced for
#' this operator carried an illegal name and checkRd rejected it.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a


#' Create AI firm decision logic
#' @param env SaoMNK environment
#' @param model saomnk_model object
#' @param n_ai Number of AI actors
#' @param actor_ids Integer vector of AI actor IDs
#' @return List with AI configuration
#' @keywords internal
.create_ai_opponents <- function(env, model, n_ai, actor_ids) {
  list(
    n_ai      = n_ai,
    actor_ids = actor_ids,
    model     = model
  )
}


# ---------------------------------------------------------------------------- #
#  searchnet_classroom_init
# ---------------------------------------------------------------------------- #

#' Create a Classroom Simulation Session
#'
#' Initialises a Capsim-style classroom session where each student controls one
#' firm competing on a shared bipartite landscape. AI opponents fill out the
#' industry to create realistic competitive dynamics.
#'
#' @param n_students Integer. Number of students (each controls one firm).
#' @param n_rounds Integer. Number of decision rounds (default 10). Each round
#'   represents one year of strategic decisions.
#' @param N Integer. Number of activities/markets (default 12).
#' @param industry Character. Industry preset:
#'   \code{"airline"} (hub-and-spoke routes),
#'   \code{"tech"} (platform products),
#'   \code{"pharma"} (drug portfolios),
#'   or \code{"custom"} (user-specified parameters).
#' @param difficulty Character. Difficulty level:
#'   \code{"intro"} (fewer AI, no shocks),
#'   \code{"intermediate"} (moderate AI, mild shocks),
#'   \code{"advanced"} (many AI, frequent shocks).
#' @param shocks Logical. Include surprise exogenous shocks? (default FALSE).
#'   Shocks are pre-scheduled but their timing is unknown to students.
#' @param seed Integer or NULL. Random seed for reproducibility.
#' @param custom_params Named list of custom parameters when
#'   \code{industry = "custom"}. Must include at minimum: \code{N},
#'   \code{activity_names}, \code{density}, \code{influence_weight}.
#' @return A list of class \code{"searchnet_classroom"} containing the
#'   environment, model, student roster, AI configuration, round tracker,
#'   decision log, and leaderboard history.
#' @export
#' @examples
#' \dontrun{
#' # Start a 30-student airline class
#' session <- searchnet_classroom_init(
#'   n_students = 30, n_rounds = 8,
#'   industry = "airline", difficulty = "intro"
#' )
#' }
searchnet_classroom_init <- function(n_students, n_rounds = 10, N = 12,
                                      industry = "airline",
                                      difficulty = "intro",
                                      shocks = FALSE, seed = NULL,
                                      custom_params = NULL) {
  # ---- Validate inputs ----
  stopifnot(is.numeric(n_students), length(n_students) == 1, n_students >= 1)
  stopifnot(is.numeric(n_rounds), length(n_rounds) == 1, n_rounds >= 1)
  stopifnot(is.numeric(N), length(N) == 1, N >= 2)
  stopifnot(industry %in% c("airline", "tech", "pharma", "custom"))
  stopifnot(difficulty %in% c("intro", "intermediate", "advanced"))

  # ---- Load preset or custom config ----
  if (industry == "custom") {
    if (is.null(custom_params)) {
      stop("custom_params required when industry = 'custom'", call. = FALSE)
    }
    preset <- custom_params
    preset$difficulty_settings <- preset$difficulty_settings %||% list(
      intro        = list(steps_per_round = 5, n_AI = 2, shock_probability = 0),
      intermediate = list(steps_per_round = 10, n_AI = 4, shock_probability = 0.1),
      advanced     = list(steps_per_round = 20, n_AI = 8, shock_probability = 0.3)
    )
  } else {
    preset <- .load_teaching_preset(industry)
  }

  # ---- Extract difficulty settings ----
  diff_settings <- preset$difficulty_settings[[difficulty]]
  n_ai           <- diff_settings$n_AI
  steps_per_round <- diff_settings$steps_per_round
  shock_prob     <- if (shocks) diff_settings$shock_probability else 0

  # ---- Resolve N and activity names ----
  N_actual <- preset$N %||% N
  activity_names <- preset$activity_names %||% paste0("Activity_", seq_len(N_actual))
  if (length(activity_names) < N_actual) {
    activity_names <- c(activity_names,
                        paste0("Activity_", seq(length(activity_names) + 1, N_actual)))
  }

  # ---- Total actors = students + AI ----
  M_total <- n_students + n_ai

  # ---- Create environment ----
  env <- saomnk_env(
    M       = M_total,
    N       = N_actual,
    density = preset$density %||% 0.3,
    seed    = seed,
    name    = paste0("classroom_", industry)
  )

  # ---- Build influence matrix ----
  epist_type <- preset$epistasis %||% "modular"
  n_blocks   <- preset$blocks %||% 3
  W <- saomnk_block_diagonal(N_actual, n_blocks)

  # ---- Build model ----
  ## The shipped preset JSONs (inst/teaching/presets/*.json) use the key
  ## `epistasis_weight`, the pre-0.4.0 name, not `influence_weight`. Before the
  ## v0.9.0 theta-storage repair this silently didn't matter -- the influence
  ## weight simulated at 0 regardless of what was declared -- so the mismatch
  ## was harmless. Now that the weight genuinely drives behavior, reading the
  ## wrong key flattens every preset onto the 0.4 fallback and erases the
  ## intended pedagogical contrast across industries (airline 0.4, tech 0.5,
  ## pharma 0.45). Check both keys; `influence_weight` wins if a preset is ever
  ## updated to the current name.
  model <- saomnk_model(
    density          = preset$density_param %||% -0.5,
    popularity       = preset$popularity %||% 0.3,
    influence_matrix = W,
    influence_weight = preset$influence_weight %||% preset$epistasis_weight %||% 0.4
  )

  # ---- Student roster ----
  student_roster <- data.frame(
    actor_id    = seq_len(n_students),
    student_id  = paste0("student_", seq_len(n_students)),
    label       = paste0("Firm_", LETTERS[((seq_len(n_students) - 1) %% 26) + 1],
                         ifelse(seq_len(n_students) > 26,
                                as.character(ceiling(seq_len(n_students) / 26)), "")),
    stringsAsFactors = FALSE
  )

  # ---- AI actor IDs ----
  ai_actor_ids <- seq(n_students + 1, M_total)

  # ---- Shock schedule (pre-generated but hidden from students) ----
  shock_schedule <- NULL
  if (shocks && shock_prob > 0) {
    set.seed(seed)
    shock_rounds <- which(runif(n_rounds) < shock_prob)
    shock_types  <- c("demand_collapse", "cost_spike", "regulation",
                      "technology_disruption", "new_entrant")
    if (length(shock_rounds) > 0) {
      shock_schedule <- data.frame(
        round      = shock_rounds,
        shock_type = sample(shock_types, length(shock_rounds), replace = TRUE),
        magnitude  = round(runif(length(shock_rounds), 0.5, 2.0), 2),
        stringsAsFactors = FALSE
      )
    }
  }

  # ---- Build session object ----
  session <- list(
    # Core simulation objects
    env              = env,
    model            = model,

    # Configuration
    industry         = industry,
    difficulty       = difficulty,
    N                = N_actual,
    activity_names   = activity_names,
    n_students       = n_students,
    n_ai             = n_ai,
    n_rounds         = n_rounds,
    steps_per_round  = steps_per_round,
    seed             = seed,

    # Roster and IDs
    student_roster   = student_roster,
    ai_actor_ids     = ai_actor_ids,

    # State tracking
    current_round    = 0L,
    decisions        = list(),      # round -> list of student decisions
    pending          = character(), # students who haven't submitted this round
    round_history    = list(),      # snapshots after each round

    # Shock schedule
    shock_schedule   = shock_schedule,
    shocks_revealed  = character(),  # shock descriptions shown to students

    # Leaderboard
    leaderboard      = data.frame(
      student_id = student_roster$student_id,
      label      = student_roster$label,
      actor_id   = student_roster$actor_id,
      cumulative_utility = rep(0, n_students),
      current_scope      = rowSums(env$bipartite_matrix)[seq_len(n_students)],
      current_K_AA       = rep(0, n_students),
      rank               = seq_len(n_students),
      stringsAsFactors   = FALSE
    ),

    # AI configuration
    ai_config = .create_ai_opponents(env, model, n_ai, ai_actor_ids)
  )

  class(session) <- "searchnet_classroom"
  message("Classroom session initialised: ",
          n_students, " students + ", n_ai, " AI firms in ",
          industry, " industry (", difficulty, " mode)")
  message("Activities: ", paste(activity_names, collapse = ", "))
  message("Rounds: ", n_rounds, " | Steps per round: ", steps_per_round)
  if (!is.null(shock_schedule)) {
    message("Shocks scheduled: ", nrow(shock_schedule),
            " surprise events (timing hidden from students)")
  }

  session
}


# ---------------------------------------------------------------------------- #
#  searchnet_classroom_submit
# ---------------------------------------------------------------------------- #

#' Submit a Student Decision for One Round
#'
#' Records the strategic decisions made by a student for the current round.
#' The student specifies which activities to add to their portfolio and which
#' to drop. Once all students have submitted, the round can be advanced.
#'
#' @param classroom A \code{searchnet_classroom} object from
#'   \code{\link{searchnet_classroom_init}}.
#' @param student_id Character. Student identifier (e.g., "student_1").
#' @param adds Integer vector of activity IDs (column indices) to add.
#'   These must be activities the student's firm does not currently hold.
#' @param drops Integer vector of activity IDs (column indices) to drop.
#'   These must be activities the student's firm currently holds.
#' @return The updated classroom object (invisibly). Prints a confirmation
#'   message and the number of students still pending.
#' @export
#' @examples
#' \dontrun{
#' session <- searchnet_classroom_submit(
#'   session, "student_1", adds = c(3, 7), drops = c(1)
#' )
#' }
searchnet_classroom_submit <- function(classroom, student_id,
                                        adds = NULL, drops = NULL) {
  stopifnot(inherits(classroom, "searchnet_classroom"))

  # ---- Validate student ----
  roster <- classroom$student_roster
  row_idx <- which(roster$student_id == student_id)
  if (length(row_idx) == 0) {
    stop("Unknown student_id '", student_id, "'. ",
         "Valid IDs: ", paste(roster$student_id, collapse = ", "),
         call. = FALSE)
  }
  actor_id <- roster$actor_id[row_idx]

  # ---- Validate round state ----
  if (classroom$current_round >= classroom$n_rounds) {
    stop("Game is over. All ", classroom$n_rounds, " rounds completed.",
         call. = FALSE)
  }
  round_key <- paste0("round_", classroom$current_round + 1)

  # ---- Validate adds/drops against current portfolio ----
  B <- classroom$env$bipartite_matrix
  current_portfolio <- which(B[actor_id, ] == 1)

  if (!is.null(adds)) {
    adds <- as.integer(adds)
    invalid_adds <- adds[adds %in% current_portfolio]
    if (length(invalid_adds) > 0) {
      warning("Activity(ies) ", paste(invalid_adds, collapse = ", "),
              " already held; ignoring duplicate adds.")
      adds <- setdiff(adds, current_portfolio)
    }
    out_of_range <- adds[adds < 1 | adds > classroom$N]
    if (length(out_of_range) > 0) {
      stop("Activity IDs out of range: ", paste(out_of_range, collapse = ", "),
           ". Valid range: 1-", classroom$N, call. = FALSE)
    }
  }
  if (!is.null(drops)) {
    drops <- as.integer(drops)
    invalid_drops <- drops[!drops %in% current_portfolio]
    if (length(invalid_drops) > 0) {
      warning("Activity(ies) ", paste(invalid_drops, collapse = ", "),
              " not held; ignoring invalid drops.")
      drops <- intersect(drops, current_portfolio)
    }
  }

  # ---- Record decision ----
  if (is.null(classroom$decisions[[round_key]])) {
    classroom$decisions[[round_key]] <- list()
  }
  classroom$decisions[[round_key]][[student_id]] <- list(
    actor_id  = actor_id,
    adds      = adds,
    drops     = drops,
    timestamp = Sys.time()
  )

  # ---- Track pending submissions ----
  all_students  <- roster$student_id
  submitted     <- names(classroom$decisions[[round_key]])
  still_pending <- setdiff(all_students, submitted)
  classroom$pending <- still_pending

  message(student_id, " (", roster$label[row_idx], ") submitted: +",
          length(adds), " / -", length(drops), " activities")
  if (length(still_pending) == 0) {
    message("All students have submitted! Call searchnet_classroom_advance() ",
            "to execute Round ", classroom$current_round + 1, ".")
  } else {
    message(length(still_pending), " student(s) still pending: ",
            paste(still_pending, collapse = ", "))
  }

  invisible(classroom)
}


# ---------------------------------------------------------------------------- #
#  searchnet_classroom_advance
# ---------------------------------------------------------------------------- #

#' Advance to the Next Round
#'
#' Executes all student decisions simultaneously, then runs AI firm decisions
#' via the SAOM logit choice rule for the configured number of ministeps.
#' Applies any scheduled shocks and updates the leaderboard.
#'
#' @param classroom A \code{searchnet_classroom} object.
#' @param force Logical. If TRUE, advances even if not all students have
#'   submitted (missing students take no action). Default FALSE.
#' @return The updated classroom object (invisibly). Prints round summary,
#'   any shock events, and the updated leaderboard.
#' @export
#' @examples
#' \dontrun{
#' session <- searchnet_classroom_advance(session)
#' }
searchnet_classroom_advance <- function(classroom, force = FALSE) {
  stopifnot(inherits(classroom, "searchnet_classroom"))

  next_round <- classroom$current_round + 1
  round_key  <- paste0("round_", next_round)

  if (next_round > classroom$n_rounds) {
    stop("Game is already over after round ", classroom$n_rounds, ".",
         call. = FALSE)
  }

  # ---- Check all submissions received ----
  if (length(classroom$pending) > 0 && !force) {
    stop(length(classroom$pending), " student(s) have not submitted: ",
         paste(classroom$pending, collapse = ", "),
         "\nUse force = TRUE to advance anyway (non-submitters do nothing).",
         call. = FALSE)
  }

  # ---- Apply student decisions (simultaneous) ----
  B <- classroom$env$bipartite_matrix
  decisions <- classroom$decisions[[round_key]] %||% list()

  for (sid in names(decisions)) {
    d <- decisions[[sid]]
    if (!is.null(d$adds) && length(d$adds) > 0) {
      B[d$actor_id, d$adds] <- 1
    }
    if (!is.null(d$drops) && length(d$drops) > 0) {
      B[d$actor_id, d$drops] <- 0
    }
  }
  classroom$env$bipartite_matrix <- B

  # ---- Apply shock if scheduled ----
  shock_msg <- NULL
  if (!is.null(classroom$shock_schedule)) {
    shock_row <- classroom$shock_schedule[classroom$shock_schedule$round == next_round, ]
    if (nrow(shock_row) > 0) {
      shock_msg <- paste0("SHOCK in Round ", next_round, ": ",
                          shock_row$shock_type, " (magnitude ",
                          shock_row$magnitude, ")")
      classroom$shocks_revealed <- c(classroom$shocks_revealed, shock_msg)

      # Modify model parameters based on shock type
      # (density becomes more negative = higher costs)
      shock_density_modifier <- -1 * shock_row$magnitude
      classroom$model$effects[[1]]$parameter <-
        classroom$model$effects[[1]]$parameter + shock_density_modifier
    }
  }

  # ---- Run AI decisions via SAOM ministeps ----
  # The AI actors follow the stochastic logit choice rule.
  # We run a limited number of ministeps to simulate AI decision-making.
  ai_steps <- classroom$steps_per_round * classroom$n_ai
  if (ai_steps > 0) {
    tryCatch({
      saomnk_run(
        classroom$env, classroom$model,
        steps_per_actor = classroom$steps_per_round,
        seed = (classroom$seed %||% 42) + next_round * 100
      )
    }, error = function(e) {
      warning("AI decision step encountered an error: ", e$message,
              "\nContinuing with student decisions only.")
    })
  }

  # ---- Compute round metrics ----
  B_now  <- classroom$env$bipartite_matrix
  S_now  <- (B_now > 0) %*% t(B_now > 0)
  diag(S_now) <- 0

  round_snapshot <- data.frame(
    round    = next_round,
    actor_id = seq_len(nrow(B_now)),
    scope    = rowSums(B_now),
    K_AA     = rowSums(S_now > 0),
    stringsAsFactors = FALSE
  )

  # Compute utility (simplified: scope benefit - crowding cost)
  popularity_vec <- colSums(B_now)
  for (i in seq_len(nrow(B_now))) {
    held <- which(B_now[i, ] == 1)
    util <- length(held) * 0.5  # base scope value
    if (length(held) > 0) {
      # Epistasis bonus from block structure
      W <- classroom$model$influence_matrix %||% diag(ncol(B_now))
      if (!is.null(W) && nrow(W) == ncol(B_now)) {
        epist_bonus <- sum(W[held, held]) - length(held)  # exclude diagonal
        # Safely get XWX weight from model
        xwx_weight <- 0.1  # default
        if (is.list(classroom$model$effects)) {
          for (.eff in classroom$model$effects) {
            if (is.list(.eff) && identical(.eff$effect, "XWX")) {
              xwx_weight <- .eff$parameter %||% 0.1
              break
            }
          }
        }
        util <- util + epist_bonus * xwx_weight
      }
      # Crowding penalty
      crowd_penalty <- sum(popularity_vec[held] - 1) * 0.1
      util <- util - crowd_penalty
    }
    round_snapshot$utility[round_snapshot$actor_id == i] <- round(util, 3)
  }

  classroom$round_history[[round_key]] <- round_snapshot

  # ---- Update leaderboard ----
  student_ids <- classroom$student_roster$actor_id
  student_snap <- round_snapshot[round_snapshot$actor_id %in% student_ids, ]

  classroom$leaderboard$current_scope <- student_snap$scope
  classroom$leaderboard$current_K_AA  <- student_snap$K_AA
  classroom$leaderboard$cumulative_utility <-
    classroom$leaderboard$cumulative_utility + student_snap$utility
  classroom$leaderboard$rank <-
    rank(-classroom$leaderboard$cumulative_utility, ties.method = "min")

  # ---- Advance round counter ----
  classroom$current_round <- next_round
  classroom$pending       <- character()

  # ---- Print summary ----
  message("\n=== Round ", next_round, " of ", classroom$n_rounds, " Complete ===")
  if (!is.null(shock_msg)) message("  ** ", shock_msg, " **")
  message("  Students acted: ", length(decisions))
  message("  AI firms acted: ", classroom$n_ai,
          " (", classroom$steps_per_round, " steps each)")

  # Top 5 leaderboard
  lb <- classroom$leaderboard[order(classroom$leaderboard$rank), ]
  n_show <- min(5, nrow(lb))
  message("\n  --- Leaderboard (Top ", n_show, ") ---")
  for (i in seq_len(n_show)) {
    message("  #", lb$rank[i], " ", lb$label[i],
            " | Utility: ", round(lb$cumulative_utility[i], 2),
            " | Scope: ", lb$current_scope[i],
            " | K_AA: ", lb$current_K_AA[i])
  }

  if (next_round == classroom$n_rounds) {
    message("\n  GAME OVER! Final standings are set.")
    message("  Use searchnet_classroom_debrief() to generate reports.")
  } else {
    message("\n  Round ", next_round + 1, " is now open for submissions.")
  }

  invisible(classroom)
}


# ---------------------------------------------------------------------------- #
#  searchnet_classroom_leaderboard
# ---------------------------------------------------------------------------- #

#' Get the Class Leaderboard
#'
#' Returns the current student rankings including cumulative utility, scope,
#' K_AA (competitive overlap), and rank. Optionally includes per-round history.
#'
#' @param classroom A \code{searchnet_classroom} object.
#' @param history Logical. If TRUE, returns the full round-by-round history
#'   rather than just the current snapshot. Default FALSE.
#' @return A data frame with student rankings, sorted by cumulative utility.
#' @export
#' @examples
#' \dontrun{
#' searchnet_classroom_leaderboard(session)
#' searchnet_classroom_leaderboard(session, history = TRUE)
#' }
searchnet_classroom_leaderboard <- function(classroom, history = FALSE) {
  stopifnot(inherits(classroom, "searchnet_classroom"))

  if (history && length(classroom$round_history) > 0) {
    # Build full history for student actors
    student_ids <- classroom$student_roster$actor_id
    history_df <- do.call(rbind, lapply(names(classroom$round_history), function(rk) {
      snap <- classroom$round_history[[rk]]
      snap <- snap[snap$actor_id %in% student_ids, ]
      snap$student_id <- classroom$student_roster$student_id[
        match(snap$actor_id, classroom$student_roster$actor_id)
      ]
      snap$label <- classroom$student_roster$label[
        match(snap$actor_id, classroom$student_roster$actor_id)
      ]
      snap
    }))
    return(history_df)
  }

  lb <- classroom$leaderboard[order(classroom$leaderboard$rank), ]
  lb$round <- classroom$current_round
  lb
}


# ---------------------------------------------------------------------------- #
#  searchnet_classroom_debrief
# ---------------------------------------------------------------------------- #

#' Generate Debrief Materials After the Game Ends
#'
#' Produces a suite of reports and visualizations summarizing the classroom
#' simulation session. Output includes a final leaderboard CSV, per-student
#' trajectory reports, a class-wide K-4 animation data export, phase space
#' comparison data, and AI counterfactual analysis.
#'
#' @param classroom A \code{searchnet_classroom} object (game should be
#'   complete or at least several rounds in).
#' @param output_dir Character. Directory for output files (default "debrief").
#'   Created if it does not exist.
#' @return A named list of generated file paths, printed to the console.
#'   Includes:
#'   \describe{
#'     \item{\code{leaderboard_csv}}{Final rankings with all metrics}
#'     \item{\code{history_csv}}{Round-by-round trajectory for all students}
#'     \item{\code{k4_plot}}{ggplot2 K-4 panel for the entire session}
#'     \item{\code{decision_log_csv}}{Full log of every student decision}
#'     \item{\code{shock_log}}{Record of all shocks that occurred}
#'   }
#' @export
#' @examples
#' \dontrun{
#' files <- searchnet_classroom_debrief(session, output_dir = "my_class_debrief")
#' }
searchnet_classroom_debrief <- function(classroom, output_dir = "debrief") {
  stopifnot(inherits(classroom, "searchnet_classroom"))

  if (classroom$current_round == 0) {
    stop("No rounds have been played yet. Nothing to debrief.", call. = FALSE)
  }

  # ---- Create output directory ----
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  generated <- list()

  # ---- 1. Final leaderboard CSV ----
  lb_file <- file.path(output_dir, "final_leaderboard.csv")
  lb <- searchnet_classroom_leaderboard(classroom)
  write.csv(lb, lb_file, row.names = FALSE)
  generated$leaderboard_csv <- lb_file

  # ---- 2. Round-by-round history CSV ----
  if (length(classroom$round_history) > 0) {
    hist_file <- file.path(output_dir, "round_history.csv")
    hist_df <- searchnet_classroom_leaderboard(classroom, history = TRUE)
    write.csv(hist_df, hist_file, row.names = FALSE)
    generated$history_csv <- hist_file
  }

  # ---- 3. K-4 panel plot ----
  tryCatch({
    k4_file <- file.path(output_dir, "k4_panel.png")
    p <- saomnk_plot_k4(classroom$env, smooth = 0.3)
    ggplot2::ggsave(k4_file, p, width = 12, height = 10, dpi = 150)
    generated$k4_plot <- k4_file
  }, error = function(e) {
    message("K-4 panel generation skipped: ", e$message)
  })

  # ---- 4. Decision log CSV ----
  if (length(classroom$decisions) > 0) {
    dec_file <- file.path(output_dir, "decision_log.csv")
    dec_rows <- list()
    for (rk in names(classroom$decisions)) {
      for (sid in names(classroom$decisions[[rk]])) {
        d <- classroom$decisions[[rk]][[sid]]
        dec_rows[[length(dec_rows) + 1]] <- data.frame(
          round      = as.integer(gsub("round_", "", rk)),
          student_id = sid,
          actor_id   = d$actor_id,
          adds       = paste(d$adds, collapse = ";"),
          drops      = paste(d$drops, collapse = ";"),
          timestamp  = as.character(d$timestamp),
          stringsAsFactors = FALSE
        )
      }
    }
    dec_df <- do.call(rbind, dec_rows)
    write.csv(dec_df, dec_file, row.names = FALSE)
    generated$decision_log_csv <- dec_file
  }

  # ---- 5. Shock log ----
  if (!is.null(classroom$shock_schedule)) {
    shock_file <- file.path(output_dir, "shock_log.csv")
    write.csv(classroom$shock_schedule, shock_file, row.names = FALSE)
    generated$shock_log <- shock_file
  }

  # ---- 6. Session summary text ----
  summary_file <- file.path(output_dir, "session_summary.txt")
  lines <- c(
    paste0("searchnet Classroom Debrief"),
    paste0("==========================="),
    paste0("Industry:   ", classroom$industry),
    paste0("Difficulty: ", classroom$difficulty),
    paste0("Students:   ", classroom$n_students),
    paste0("AI firms:   ", classroom$n_ai),
    paste0("Rounds:     ", classroom$current_round, " of ", classroom$n_rounds),
    paste0("Activities: ", classroom$N, " (", classroom$industry, " preset)"),
    "",
    "Activity Names:",
    paste0("  ", seq_along(classroom$activity_names), ". ",
           classroom$activity_names),
    "",
    "Final Leaderboard:",
    paste0("  #", lb$rank, " ", lb$label,
           " (Utility: ", round(lb$cumulative_utility, 2),
           ", Scope: ", lb$current_scope,
           ", K_AA: ", lb$current_K_AA, ")")
  )
  if (length(classroom$shocks_revealed) > 0) {
    lines <- c(lines, "", "Shocks:", paste0("  ", classroom$shocks_revealed))
  }
  writeLines(lines, summary_file)
  generated$summary_txt <- summary_file

  # ---- Report ----
  message("\nDebrief materials generated in '", output_dir, "':")
  for (nm in names(generated)) {
    message("  ", nm, ": ", generated[[nm]])
  }

  invisible(generated)
}


# ---------------------------------------------------------------------------- #
#  searchnet_classroom_status
# ---------------------------------------------------------------------------- #

#' Get Current Classroom Session Status
#'
#' Provides a quick summary of the session state: current round, pending
#' submissions, and a student's current portfolio.
#'
#' @param classroom A \code{searchnet_classroom} object.
#' @param student_id Character or NULL. If provided, shows that student's
#'   current portfolio and metrics.
#' @return A list with session status information, printed to the console.
#' @examples
#' \donttest{
#' session <- searchnet_classroom_init(n_students = 4, n_rounds = 6,
#'                                     industry = "airline",
#'                                     difficulty = "intro", seed = 1)
#' status <- searchnet_classroom_status(session)
#' status$rounds_remaining
#' }
#' @export
searchnet_classroom_status <- function(classroom, student_id = NULL) {
  stopifnot(inherits(classroom, "searchnet_classroom"))

  status <- list(
    industry        = classroom$industry,
    difficulty      = classroom$difficulty,
    current_round   = classroom$current_round,
    total_rounds    = classroom$n_rounds,
    rounds_remaining = classroom$n_rounds - classroom$current_round,
    students_total  = classroom$n_students,
    ai_firms        = classroom$n_ai,
    pending         = classroom$pending
  )

  message("=== Session Status ===")
  message("  Industry: ", status$industry, " (", status$difficulty, ")")
  message("  Round: ", status$current_round, "/", status$total_rounds)
  message("  Pending submissions: ", length(status$pending))

  if (!is.null(student_id)) {
    roster <- classroom$student_roster
    row_idx <- which(roster$student_id == student_id)
    if (length(row_idx) > 0) {
      actor_id <- roster$actor_id[row_idx]
      B <- classroom$env$bipartite_matrix
      held <- which(B[actor_id, ] == 1)
      held_names <- classroom$activity_names[held]

      message("\n  --- ", roster$label[row_idx], " (", student_id, ") ---")
      message("  Portfolio: ", paste(held_names, collapse = ", "))
      message("  Scope: ", length(held))

      lb_row <- classroom$leaderboard[classroom$leaderboard$student_id == student_id, ]
      if (nrow(lb_row) > 0) {
        message("  Rank: #", lb_row$rank,
                " | Cumulative Utility: ", round(lb_row$cumulative_utility, 2))
      }

      status$student_portfolio <- held_names
      status$student_scope     <- length(held)
    }
  }

  invisible(status)
}


# ---------------------------------------------------------------------------- #
#  Print method
# ---------------------------------------------------------------------------- #

#' @export
print.searchnet_classroom <- function(x, ...) {
  cat("searchnet Classroom Session\n")
  cat("  Industry:   ", x$industry, " (", x$difficulty, ")\n")
  cat("  Students:   ", x$n_students, "\n")
  cat("  AI firms:   ", x$n_ai, "\n")
  cat("  Round:      ", x$current_round, " / ", x$n_rounds, "\n")
  cat("  Activities: ", x$N, "\n")
  if (x$current_round > 0) {
    cat("  Leader:     ", x$leaderboard$label[which.min(x$leaderboard$rank)], "\n")
  }
  invisible(x)
}
