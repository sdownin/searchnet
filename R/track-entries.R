#' Competitive Entry/Exit Tracking for SaoMNK Simulations
#'
#' Records metadata about each ADD and DROP action in the SAOM ministep
#' chain: was the activity occupied by rivals? How many rivals were present?
#' This enables measurement of competitive aggressiveness and forbearance
#' behavior following Baum & Korn (1996, 1999).
#'
#' Works with both the searchnet package's simulation output (via
#' \code{env$bi_env_arr} and \code{env$chain_stats}) and the CD4 engine's
#' output format (holdings history list or entry log data.frame).
#'
#' @name track-entries
NULL


# ---------------------------------------------------------------------------- #
#  track_entry_decisions
# ---------------------------------------------------------------------------- #

#' Track competitive entry/exit decisions during SAOM-NK simulation
#'
#' Compares consecutive bipartite matrices in the ministep chain to identify
#' each ADD (0 -> 1) and DROP (1 -> 0) action, then records how many OTHER
#' firms held that activity at the moment of the decision.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object with a completed simulation
#'   (requires \code{env$bi_env_arr} and \code{env$chain_stats}).
#'   Alternatively, pass \code{NULL} and supply \code{holdings_history} or
#'   \code{entry_log} directly.
#' @param holdings_history Optional list of holdings matrices (one per round),
#'   as produced by the CD4 engine. Each element is an \eqn{M \times N}{M x N}
#'   binary matrix. Used when \code{env} is \code{NULL}.
#' @param entry_log Optional data.frame with columns \code{firm},
#'   \code{activity}, \code{n_rivals_present}, \code{was_competitive} (as
#'   produced by the CD4 forbearance experiment). Passed through with
#'   standardised column names.
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{step}{Integer chain step or round index.}
#'     \item{firm}{Integer firm/actor ID (1-indexed).}
#'     \item{activity}{Integer activity/component ID (1-indexed).}
#'     \item{action_type}{Character, \code{"add"} or \code{"drop"}.}
#'     \item{n_rivals_present}{Integer count of OTHER firms holding
#'       the activity at the moment of decision.}
#'     \item{is_competitive_entry}{Logical. TRUE if \code{action_type == "add"}
#'       and \code{n_rivals_present > 0}.}
#'     \item{is_competitive_exit}{Logical. TRUE if \code{action_type == "drop"}
#'       and \code{n_rivals_present > 0} (exiting a contested activity).}
#'   }
#' @export
track_entry_decisions <- function(env = NULL,
                                  holdings_history = NULL,
                                  entry_log = NULL) {

  # ---- Path 1: CD4 entry_log pass-through ----
  if (!is.null(entry_log)) {
    stopifnot(is.data.frame(entry_log))
    out <- data.frame(
      step              = if ("ministep" %in% names(entry_log)) entry_log$ministep
                          else if ("round" %in% names(entry_log)) entry_log$round
                          else seq_len(nrow(entry_log)),
      firm              = entry_log$firm,
      activity          = entry_log$activity,
      action_type       = rep("add", nrow(entry_log)),
      n_rivals_present  = entry_log$n_rivals_present,
      is_competitive_entry = as.logical(entry_log$was_competitive),
      is_competitive_exit  = rep(FALSE, nrow(entry_log)),
      stringsAsFactors = FALSE
    )
    return(out)
  }

  # ---- Path 2: Holdings history list (CD4 round-boundary) ----
  if (!is.null(holdings_history)) {
    stopifnot(is.list(holdings_history), length(holdings_history) >= 2)
    records <- vector("list", length(holdings_history) - 1)

    for (t in 2:length(holdings_history)) {
      mat_prev <- holdings_history[[t - 1]]
      mat_curr <- holdings_history[[t]]
      M <- nrow(mat_prev)
      N <- ncol(mat_prev)
      diffs <- mat_curr - mat_prev

      for (i in seq_len(M)) {
        changed <- which(diffs[i, ] != 0)
        if (length(changed) == 0) next
        for (j in changed) {
          rivals_present <- sum(mat_prev[-i, j])
          if (diffs[i, j] == 1) {
            records[[length(records) + 1]] <- data.frame(
              step = t - 1, firm = i, activity = j,
              action_type = "add",
              n_rivals_present = rivals_present,
              is_competitive_entry = rivals_present > 0,
              is_competitive_exit  = FALSE,
              stringsAsFactors = FALSE
            )
          } else if (diffs[i, j] == -1) {
            records[[length(records) + 1]] <- data.frame(
              step = t - 1, firm = i, activity = j,
              action_type = "drop",
              n_rivals_present = rivals_present,
              is_competitive_entry = FALSE,
              is_competitive_exit  = rivals_present > 0,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    return(bind_rows(records))
  }

  # ---- Path 3: SaoMNK env object ----
  if (is.null(env))
    stop("Provide one of: env (SaoMNK object), holdings_history, or entry_log.")
  if (is.null(env$bi_env_arr))
    stop("env$bi_env_arr is NULL. Run the simulation with process_chain = TRUE first.")
  if (is.null(env$chain_stats))
    stop("env$chain_stats is NULL. Run the simulation with returnChains = TRUE first.")

  bi_arr   <- env$bi_env_arr
  chain_df <- env$chain_stats
  M <- dim(bi_arr)[1]
  N <- dim(bi_arr)[2]
  n_steps <- dim(bi_arr)[3]

  # Pre-allocate lists
  records <- vector("list", sum(!chain_df$stability))
  rec_idx <- 0L

  for (s in seq_len(nrow(chain_df))) {
    if (chain_df$stability[s]) next

    actor_i <- chain_df$id_from[s]
    comp_j  <- chain_df$id_to[s]
    step_id <- chain_df$chain_step_id[s]

    # Determine action type from bi_env_arr.
    # bi_env_arr[,,s] is the state AFTER step s has been applied.
    # The previous state is bi_env_arr[,,s-1] (or bipartite_matrix_init for s==1).
    state_after <- bi_arr[actor_i, comp_j, s]
    action <- if (state_after == 1) "add" else "drop"

    # Count rivals at the moment of decision (state BEFORE the toggle).
    # The state before toggle is the opposite of state_after for the focal cell,
    # but for OTHER firms we use the current state (they haven't changed).
    rivals_present <- sum(bi_arr[-actor_i, comp_j, s])

    rec_idx <- rec_idx + 1L
    records[[rec_idx]] <- data.frame(
      step = step_id,
      firm = actor_i,
      activity = comp_j,
      action_type = action,
      n_rivals_present = rivals_present,
      is_competitive_entry = (action == "add" & rivals_present > 0),
      is_competitive_exit  = (action == "drop" & rivals_present > 0),
      stringsAsFactors = FALSE
    )
  }

  bind_rows(records[seq_len(rec_idx)])
}


# ---------------------------------------------------------------------------- #
#  compute_forbearance_metrics
# ---------------------------------------------------------------------------- #

#' Compute forbearance metrics from entry tracking data
#'
#' Aggregates the raw entry/exit log into summary measures of competitive
#' aggressiveness vs. forbearance.
#'
#' @param entry_log A \code{data.frame} produced by \code{track_entry_decisions()}.
#' @param by_firm Logical. If \code{TRUE}, return metrics for each firm
#'   separately. Default \code{FALSE} returns population-level aggregates.
#' @return A \code{data.frame} (if \code{by_firm = TRUE}) or a named
#'   \code{list} with:
#'   \describe{
#'     \item{competitive_entry_rate}{Fraction of ADDs into rival-occupied activities.}
#'     \item{mean_rivals_at_entry}{Mean number of rivals present at ADD actions.}
#'     \item{avoidance_rate}{Fraction of ADDs into EMPTY activities (pioneering).}
#'     \item{competitive_exit_rate}{Fraction of DROPs from rival-occupied activities.}
#'     \item{entry_count}{Total number of ADD actions.}
#'     \item{drop_count}{Total number of DROP actions.}
#'   }
#' @export
compute_forbearance_metrics <- function(entry_log, by_firm = FALSE) {
  stopifnot(is.data.frame(entry_log))
  if (nrow(entry_log) == 0) {
    return(list(
      competitive_entry_rate = NA_real_,
      mean_rivals_at_entry   = NA_real_,
      avoidance_rate         = NA_real_,
      competitive_exit_rate  = NA_real_,
      entry_count            = 0L,
      drop_count             = 0L
    ))
  }

  .compute <- function(df) {
    adds  <- df %>% filter(action_type == "add")
    drops <- df %>% filter(action_type == "drop")
    n_adds  <- nrow(adds)
    n_drops <- nrow(drops)

    list(
      competitive_entry_rate = if (n_adds > 0)
        mean(adds$is_competitive_entry) else NA_real_,
      mean_rivals_at_entry   = if (n_adds > 0)
        mean(adds$n_rivals_present) else NA_real_,
      avoidance_rate         = if (n_adds > 0)
        mean(!adds$is_competitive_entry) else NA_real_,
      competitive_exit_rate  = if (n_drops > 0)
        mean(drops$is_competitive_exit) else NA_real_,
      entry_count            = n_adds,
      drop_count             = n_drops
    )
  }

  if (!by_firm) {
    return(.compute(entry_log))
  }

  # Per-firm metrics
  firms <- sort(unique(entry_log$firm))
  per_firm <- lapply(firms, function(f) {
    metrics <- .compute(entry_log %>% filter(firm == f))
    as.data.frame(c(firm = f, metrics), stringsAsFactors = FALSE)
  })
  bind_rows(per_firm)
}


# ---------------------------------------------------------------------------- #
#  compute_forbearance_trajectory
# ---------------------------------------------------------------------------- #

#' Compute competitive entry rate over time (rolling window)
#'
#' Useful for plotting how forbearance evolves across the simulation.
#'
#' @param entry_log A \code{data.frame} produced by \code{track_entry_decisions()}.
#' @param window Integer. Number of consecutive ADD events in the rolling
#'   window. Default 50.
#' @return A \code{data.frame} with columns \code{step}, \code{window_center},
#'   \code{competitive_entry_rate}, \code{mean_rivals_at_entry}.
#' @export
compute_forbearance_trajectory <- function(entry_log, window = 50L) {
  adds <- entry_log %>% filter(action_type == "add") %>% arrange(step)
  n <- nrow(adds)
  if (n < window) {
    warning("Fewer ADD events (", n, ") than window size (", window,
            "). Returning single-row summary.")
    return(data.frame(
      step                   = median(adds$step),
      window_center          = ceiling(n / 2),
      competitive_entry_rate = mean(adds$is_competitive_entry),
      mean_rivals_at_entry   = mean(adds$n_rivals_present)
    ))
  }

  starts <- seq(1, n - window + 1, by = max(1L, window %/% 4))
  out <- lapply(starts, function(s) {
    w <- adds[s:(s + window - 1), ]
    data.frame(
      step                   = median(w$step),
      window_center          = s + window %/% 2,
      competitive_entry_rate = mean(w$is_competitive_entry),
      mean_rivals_at_entry   = mean(w$n_rivals_present)
    )
  })
  bind_rows(out)
}
