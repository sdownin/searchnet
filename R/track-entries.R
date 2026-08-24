#' Competitive Entry/Exit Tracking for SaoMNK Simulations
#'
#' Records metadata about each ADD and DROP action in the SAOM ministep
#' chain: was the activity occupied by rivals, how many were present, and
#' WHICH ones.
#'
#' Works with both the searchnet package's simulation output (via
#' \code{env$bi_env_arr} and \code{env$chain_stats}) and the CD4 engine's
#' output format (holdings history list or entry log data.frame).
#'
#' @section Which statistics are dyadic, and which are not:
#'
#' Mutual forbearance in the sense of Edwards (1955), Bernheim and Whinston
#' (1990), Baum and Korn (1996, 1999) and Gimeno (1999) is a property of a
#' FIRM PAIR, conditional on how many markets that pair meets in. It is not a
#' property of a firm, and it is not the same thing as avoiding occupied cells.
#'
#' The population-level statistics in \code{compute_forbearance_metrics()}
#' (\code{competitive_entry_rate}, \code{avoidance_rate} and the rest) are
#' counts over rivals with no rival identity in them. Entering a cell held by a
#' rival met nowhere else and entering a cell held by a rival met in twenty
#' markets are the same event to those statistics, and only the second is what
#' mutual forbearance is about. Cell avoidance is also just as consistent with
#' differentiation or crowding avoidance, which is the alternative the
#' multimarket literature spends its effort ruling out. Those statistics are
#' therefore descriptive measures of entry into contested space. They are
#' useful, they are not the multimarket construct, and they are deliberately
#' documented here without a citation that would imply otherwise.
#'
#' The dyadic construct is served by \code{expand_entry_rivals()},
#' \code{compute_multimarket_contact()} and \code{compute_dyadic_forbearance()},
#' which are conditional on the pair and, when holdings are supplied, corrected
#' for exposure.
#'
#' @section Rival identity and the row-drop indexing trap:
#'
#' Rival identity is read as \code{setdiff(which(column == 1), i)} on the FULL
#' column, never as \code{which(mat[-i, j] == 1)}. Dropping row \code{i} shifts
#' every index above \code{i} down by one, so the second form returns firm IDs
#' that are silently wrong for every rival ranked after the focal actor, while
#' still returning the correct COUNT. That is why the defect would not have
#' shown up in any of the existing count-based metrics.
#'
#' @name track-entries
NULL


# ---------------------------------------------------------------------------- #
#  .rival_ids_in_column  (internal)
# ---------------------------------------------------------------------------- #

#' Identify rivals holding an activity, excluding the focal actor
#'
#' @param col Numeric/integer vector of length M: one activity column of the
#'   holdings matrix, indexed by firm.
#' @param i Integer index of the focal actor to exclude.
#' @return Integer vector of rival firm IDs, sorted ascending, possibly empty.
#' @keywords internal
.rival_ids_in_column <- function(col, i) {
  ids <- which(col == 1)
  ids[ids != i]
}

#' Collapse a rival ID set to a single storable field
#'
#' A pipe-delimited character field rather than a list-column, because the
#' entry log is written to CSV by \code{searchnet-export.R} and passed through
#' \code{bind_rows()} in several places, both of which mishandle list-columns.
#'
#' @param ids Integer vector of rival firm IDs.
#' @return Length-1 character; \code{""} when no rivals are present.
#' @keywords internal
.collapse_rival_ids <- function(ids) {
  if (length(ids) == 0) return("")
  paste(sort(as.integer(ids)), collapse = "|")
}


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
#'   standardized column names.
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{step}{Integer chain step or round index.}
#'     \item{firm}{Integer firm/actor ID (1-indexed).}
#'     \item{activity}{Integer activity/component ID (1-indexed).}
#'     \item{action_type}{Character, \code{"add"} or \code{"drop"}.}
#'     \item{n_rivals_present}{Integer count of OTHER firms holding
#'       the activity at the moment of decision.}
#'     \item{rival_ids}{Character. Pipe-delimited 1-indexed IDs of the OTHER
#'       firms holding the activity at the moment of decision, e.g.
#'       \code{"3|7|12"}. Empty string when no rivals were present.
#'       \code{NA_character_} when identity is not recoverable, which is the
#'       case for the CD4 \code{entry_log} pass-through, since that input
#'       carries a rival COUNT and no matrix from which identity could be
#'       reconstructed. NA rather than \code{""}, so that "no rivals" and
#'       "unknown rivals" stay distinguishable downstream.}
#'     \item{state_idx}{Integer index of the pre-decision state in the source
#'       object (the \code{holdings_history} element, or the third-dimension
#'       slice of \code{env$bi_env_arr}). \code{NA_integer_} for the
#'       pass-through path. Carried so that exposure baselines can be aligned
#'       to the exact state a decision was taken from, rather than inferred
#'       from \code{step}, which is a chain step ID in one path and a round
#'       index in another.}
#'     \item{is_competitive_entry}{Logical. TRUE if \code{action_type == "add"}
#'       and \code{n_rivals_present > 0}.}
#'     \item{is_competitive_exit}{Logical. TRUE if \code{action_type == "drop"}
#'       and \code{n_rivals_present > 0} (exiting a contested activity).}
#'   }
#'
#'   Existing columns keep their names and meanings. \code{rival_ids} and
#'   \code{state_idx} are additions, so callers that select columns by name are
#'   unaffected.
#' @seealso \code{\link{expand_entry_rivals}} for the one-row-per-dyad form,
#'   \code{\link{compute_dyadic_forbearance}} for the pair-level measure.
#' @examples
#' ## From a holdings history: 3 firms, 4 activities, 5 rounds
#' set.seed(7)
#' h <- list(matrix(rbinom(12, 1, 0.4), nrow = 3))
#' for (t in 2:5) {
#'   m <- h[[t - 1]]
#'   i <- sample(3, 1); j <- sample(4, 1)
#'   m[i, j] <- 1 - m[i, j]   # one firm toggles one activity per round
#'   h[[t]] <- m
#' }
#' entry_log <- track_entry_decisions(holdings_history = h)
#' entry_log
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
      # Identity is genuinely unavailable on this path: the CD4 forbearance
      # experiment emits a count, and there is no matrix here to recover WHICH
      # firms it counted. NA rather than "" so that a downstream dyadic
      # computation fails loudly instead of treating these as uncontested.
      # If a rival_ids column is already present on the input, honour it.
      rival_ids         = if ("rival_ids" %in% names(entry_log))
                            as.character(entry_log$rival_ids)
                          else rep(NA_character_, nrow(entry_log)),
      state_idx         = rep(NA_integer_, nrow(entry_log)),
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
          # Identity and count both come from the SAME source, the full
          # pre-decision column, so they cannot drift apart. See the indexing
          # note in the file header for why mat_prev[-i, j] is not used to
          # derive IDs.
          rival_ids      <- .rival_ids_in_column(mat_prev[, j], i)
          rivals_present <- length(rival_ids)
          if (diffs[i, j] == 1) {
            records[[length(records) + 1]] <- data.frame(
              step = t - 1, firm = i, activity = j,
              action_type = "add",
              n_rivals_present = rivals_present,
              rival_ids = .collapse_rival_ids(rival_ids),
              state_idx = t - 1L,
              is_competitive_entry = rivals_present > 0,
              is_competitive_exit  = FALSE,
              stringsAsFactors = FALSE
            )
          } else if (diffs[i, j] == -1) {
            records[[length(records) + 1]] <- data.frame(
              step = t - 1, firm = i, activity = j,
              action_type = "drop",
              n_rivals_present = rivals_present,
              rival_ids = .collapse_rival_ids(rival_ids),
              state_idx = t - 1L,
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

    # Identify rivals at the moment of decision (state BEFORE the toggle).
    # Only the focal actor's own cell changed at step s, so slice s is the
    # correct read for every OTHER firm; the focal row is excluded by ID.
    # Identity is taken from the full column, not from bi_arr[-actor_i, , ],
    # because dropping the focal row renumbers every firm above it. The count
    # is then derived from the identity set, so the two cannot disagree.
    rival_ids      <- .rival_ids_in_column(bi_arr[, comp_j, s], actor_i)
    rivals_present <- length(rival_ids)

    rec_idx <- rec_idx + 1L
    records[[rec_idx]] <- data.frame(
      step = step_id,
      firm = actor_i,
      activity = comp_j,
      action_type = action,
      n_rivals_present = rivals_present,
      rival_ids = .collapse_rival_ids(rival_ids),
      state_idx = s,
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

#' Compute population-level entry-into-contested-space metrics
#'
#' Aggregates the raw entry/exit log into summary measures of how often firms
#' move into activities that rivals already hold.
#'
#' @section What this is not:
#'
#' These are UNCONDITIONAL, NON-DYADIC statistics. They count rivals without
#' identifying them, so they cannot distinguish restraint toward a specific
#' rival from a general preference for empty space, and they are not the
#' multimarket forbearance construct of Edwards (1955), Bernheim and Whinston
#' (1990), Baum and Korn (1996, 1999) or Gimeno (1999), all of which are defined
#' on the firm pair and conditional on contact. Nor are they corrected for
#' exposure: a firm whose rivals hold most of the space will show a high
#' \code{competitive_entry_rate} through arithmetic alone.
#'
#' For the pair-level construct use \code{\link{compute_dyadic_forbearance}}.
#' The function name is retained for backward compatibility with
#' \code{plot-forbearance.R} and downstream consumers.
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
#' @examples
#' set.seed(7)
#' h <- list(matrix(rbinom(12, 1, 0.4), nrow = 3))
#' for (t in 2:5) {
#'   m <- h[[t - 1]]
#'   i <- sample(3, 1); j <- sample(4, 1)
#'   m[i, j] <- 1 - m[i, j]
#'   h[[t]] <- m
#' }
#' entry_log <- track_entry_decisions(holdings_history = h)
#' compute_forbearance_metrics(entry_log)
#' compute_forbearance_metrics(entry_log, by_firm = TRUE)
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
#' @examples
#' set.seed(7)
#' h <- list(matrix(rbinom(12, 1, 0.4), nrow = 3))
#' for (t in 2:16) {
#'   m <- h[[t - 1]]
#'   i <- sample(3, 1); j <- sample(4, 1)
#'   m[i, j] <- 1 - m[i, j]
#'   h[[t]] <- m
#' }
#' entry_log <- track_entry_decisions(holdings_history = h)
#' compute_forbearance_trajectory(entry_log, window = 3)
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


# ---------------------------------------------------------------------------- #
#  expand_entry_rivals
# ---------------------------------------------------------------------------- #

#' Expand an entry log to one row per decision-rival dyad
#'
#' Every dyadic statistic needs the decision expanded against each rival that
#' was present, because a single ADD into a cell held by three rivals is three
#' dyadic events, not one.
#'
#' @param entry_log A \code{data.frame} from \code{track_entry_decisions()},
#'   carrying a \code{rival_ids} column.
#' @param drop_uncontested Logical. If \code{TRUE} (default) rows with no
#'   rivals present are dropped, since they generate no dyad. If \code{FALSE}
#'   they are retained with \code{rival = NA_integer_}.
#' @return A \code{data.frame} with all input columns plus \code{rival}
#'   (integer firm ID of the rival). Rows whose \code{rival_ids} is \code{NA}
#'   (identity not recoverable) are dropped with a warning, never silently.
#' @examples
#' set.seed(7)
#' h <- list(matrix(rbinom(12, 1, 0.4), nrow = 3))
#' for (t in 2:8) {
#'   m <- h[[t - 1]]
#'   i <- sample(3, 1); j <- sample(4, 1)
#'   m[i, j] <- 1 - m[i, j]
#'   h[[t]] <- m
#' }
#' entry_log <- track_entry_decisions(holdings_history = h)
#'
#' ## One row per decision-rival dyad: an ADD against two rivals is two rows
#' expand_entry_rivals(entry_log)
#' @export
expand_entry_rivals <- function(entry_log, drop_uncontested = TRUE) {
  stopifnot(is.data.frame(entry_log))
  if (!"rival_ids" %in% names(entry_log)) {
    stop("entry_log has no rival_ids column. It was produced by a version of ",
         "track_entry_decisions() that recorded only rival COUNTS. Rerun the ",
         "tracking step against the simulation output; rival identity cannot ",
         "be reconstructed from a count.")
  }

  unknown <- is.na(entry_log$rival_ids)
  if (any(unknown)) {
    warning(sum(unknown), " of ", nrow(entry_log), " decisions have ",
            "unrecoverable rival identity (rival_ids = NA) and are dropped. ",
            "These come from the CD4 entry_log pass-through path, which ",
            "carries counts only.")
    entry_log <- entry_log[!unknown, , drop = FALSE]
  }
  if (nrow(entry_log) == 0) return(cbind(entry_log, rival = integer(0)))

  parsed <- strsplit(entry_log$rival_ids, "|", fixed = TRUE)
  n_each <- vapply(parsed, length, integer(1))

  if (drop_uncontested) {
    keep   <- n_each > 0
    parsed <- parsed[keep]
    n_each <- n_each[keep]
    base   <- entry_log[keep, , drop = FALSE]
    if (nrow(base) == 0) return(cbind(entry_log[0, , drop = FALSE],
                                      rival = integer(0)))
    out <- base[rep(seq_len(nrow(base)), n_each), , drop = FALSE]
    out$rival <- as.integer(unlist(parsed, use.names = FALSE))
  } else {
    reps <- pmax(n_each, 1L)
    out  <- entry_log[rep(seq_len(nrow(entry_log)), reps), , drop = FALSE]
    out$rival <- as.integer(unlist(
      lapply(parsed, function(p) if (length(p)) p else NA_character_),
      use.names = FALSE))
  }

  # A rival must never be the focal firm; if it is, the exclusion in
  # .rival_ids_in_column() failed and every downstream dyadic number is wrong.
  self <- !is.na(out$rival) & out$rival == out$firm
  if (any(self)) {
    stop("Focal firm appears as its own rival in ", sum(self), " rows. ",
         "The self-exclusion in track_entry_decisions() is broken; do not use ",
         "these results.")
  }

  rownames(out) <- NULL
  out
}


# ---------------------------------------------------------------------------- #
#  compute_multimarket_contact
# ---------------------------------------------------------------------------- #

#' Multimarket contact matrix from a holdings matrix
#'
#' Contact between firms \eqn{i} and \eqn{j} is the number of activities both
#' hold. This is the conditioning variable the multimarket forbearance
#' construct is defined on, and it is what distinguishes forbearance from
#' ordinary crowding avoidance.
#'
#' @param holdings An \eqn{M \times N}{M x N} binary firm-by-activity matrix,
#'   or a list of such matrices, in which case contact is computed from the
#'   element given by \code{at}.
#' @param at Integer index into \code{holdings} when it is a list. Defaults to
#'   the first element, that is contact measured at the start rather than
#'   after the behavior being explained, which is the usual requirement.
#' @return An \eqn{M \times M}{M x M} integer matrix with a zero diagonal.
#' @examples
#' ## 3 firms, 4 activities: firms 1 and 2 meet in two activities
#' H <- rbind(c(1, 1, 0, 0),
#'            c(1, 1, 1, 0),
#'            c(0, 0, 1, 1))
#' compute_multimarket_contact(H)
#' @export
compute_multimarket_contact <- function(holdings, at = 1L) {
  if (is.list(holdings)) {
    stopifnot(length(holdings) >= at, at >= 1)
    holdings <- holdings[[at]]
  }
  stopifnot(is.matrix(holdings))
  H <- (holdings > 0) * 1L
  C <- H %*% t(H)
  diag(C) <- 0L
  storage.mode(C) <- "integer"
  C
}


# ---------------------------------------------------------------------------- #
#  compute_dyadic_forbearance
# ---------------------------------------------------------------------------- #

#' Pair-level forbearance, optionally corrected for exposure
#'
#' For each ordered pair (\code{firm}, \code{rival}), how often the focal firm
#' moved into activities that specific rival held. With \code{holdings}
#' supplied, the observed rate is compared against what random choice from the
#' focal firm's available activities would have produced, which is the
#' correction that separates restraint from the arithmetic of a rival simply
#' occupying a lot of space.
#'
#' @section Reading the output:
#'
#' \code{forbearance_index} is \code{expected_rate - observed_rate}, so
#' POSITIVE means the focal firm entered the rival's space LESS than chance,
#' that is restraint. It is defined only when \code{holdings} is supplied.
#' Without \code{holdings} the function returns incidence counts and shares
#' only, and those are not rates against opportunity; the
#' \code{exposure_corrected} column records which case a row is.
#'
#' Pair the result with \code{\link{compute_multimarket_contact}} to test the
#' multimarket prediction, that restraint rises with contact. This function
#' deliberately does not fold contact in itself: contact is measured on a
#' different object and at a different time point than the decisions, and
#' joining them silently is how the timing assumption stops being visible.
#'
#' @param entry_log A \code{data.frame} from \code{track_entry_decisions()}.
#' @param holdings Optional list of \eqn{M \times N}{M x N} holdings matrices
#'   indexed to match the log's \code{state_idx}, or a 3-d array
#'   \code{[firm, activity, state]} such as \code{env$bi_env_arr}. Supplying
#'   this enables the exposure correction.
#' @param min_entries Integer. Pairs with fewer than this many focal-firm ADD
#'   actions are returned but flagged \code{sparse = TRUE}. Default 5.
#' @return A \code{data.frame}, one row per ordered pair observed, with
#'   \code{firm}, \code{rival}, \code{n_entries_focal},
#'   \code{n_entries_vs_rival}, \code{observed_rate}, \code{expected_rate},
#'   \code{forbearance_index}, \code{exposure_corrected} and \code{sparse}.
#' @examples
#' set.seed(7)
#' h <- list(matrix(rbinom(12, 1, 0.4), nrow = 3))
#' for (t in 2:16) {
#'   m <- h[[t - 1]]
#'   i <- sample(3, 1); j <- sample(4, 1)
#'   m[i, j] <- 1 - m[i, j]
#'   h[[t]] <- m
#' }
#' entry_log <- track_entry_decisions(holdings_history = h)
#'
#' ## Incidence shares only (no exposure correction)
#' compute_dyadic_forbearance(entry_log, min_entries = 1)
#'
#' ## Exposure-corrected: positive forbearance_index = restraint
#' compute_dyadic_forbearance(entry_log, holdings = h, min_entries = 1)
#' @export
compute_dyadic_forbearance <- function(entry_log, holdings = NULL,
                                       min_entries = 5L) {
  stopifnot(is.data.frame(entry_log))

  adds <- entry_log[entry_log$action_type == "add", , drop = FALSE]
  if (nrow(adds) == 0) {
    return(data.frame(firm = integer(0), rival = integer(0),
                      n_entries_focal = integer(0),
                      n_entries_vs_rival = integer(0),
                      observed_rate = numeric(0), expected_rate = numeric(0),
                      forbearance_index = numeric(0),
                      exposure_corrected = logical(0), sparse = logical(0)))
  }

  # Denominator: every ADD the focal firm made, contested or not.
  n_focal <- table(adds$firm)

  dy <- expand_entry_rivals(adds, drop_uncontested = TRUE)
  if (nrow(dy) == 0) {
    return(data.frame(firm = integer(0), rival = integer(0),
                      n_entries_focal = integer(0),
                      n_entries_vs_rival = integer(0),
                      observed_rate = numeric(0), expected_rate = numeric(0),
                      forbearance_index = numeric(0),
                      exposure_corrected = logical(0), sparse = logical(0)))
  }

  pair <- aggregate(list(n_entries_vs_rival = rep(1L, nrow(dy))),
                    by = list(firm = dy$firm, rival = dy$rival), FUN = sum)
  pair$n_entries_focal <- as.integer(n_focal[as.character(pair$firm)])
  pair$observed_rate   <- pair$n_entries_vs_rival / pair$n_entries_focal

  # ---- exposure baseline ----
  if (is.null(holdings)) {
    pair$expected_rate      <- NA_real_
    pair$forbearance_index  <- NA_real_
    pair$exposure_corrected <- FALSE
    message("compute_dyadic_forbearance(): no holdings supplied, so ",
            "observed_rate is an incidence share, not a rate against ",
            "opportunity, and forbearance_index is NA. Supply holdings (a ",
            "list of matrices or env$bi_env_arr) for the exposure correction.")
  } else {
    get_state <- if (is.array(holdings) && length(dim(holdings)) == 3) {
      function(k) holdings[, , k]
    } else if (is.list(holdings)) {
      function(k) holdings[[k]]
    } else {
      stop("holdings must be a list of matrices or a 3-d array.")
    }
    n_states <- if (is.list(holdings)) length(holdings) else dim(holdings)[3]

    if (all(is.na(adds$state_idx))) {
      stop("holdings supplied but every state_idx is NA, so decisions cannot ",
           "be aligned to states. This entry log came from the CD4 ",
           "pass-through path; rerun tracking against the simulation output.")
    }
    bad <- !is.na(adds$state_idx) &
           (adds$state_idx < 1 | adds$state_idx > n_states)
    if (any(bad)) {
      stop(sum(bad), " decisions have a state_idx outside the range of the ",
           "supplied holdings (1..", n_states, "). The log and the holdings ",
           "are not from the same run.")
    }

    # A range check only catches holdings that are too short. It does not catch
    # holdings of the right length from a DIFFERENT run, which would silently
    # produce a plausible exposure baseline for the wrong states. So re-derive
    # the rival set from the supplied holdings and require it to reproduce what
    # the log recorded. Both paths read rivals from the same pre-decision
    # column, so exact agreement is the correct expectation, not an
    # approximation.
    chk <- which(!is.na(adds$state_idx) & !is.na(adds$rival_ids))
    if (length(chk)) {
      recomputed <- vapply(chk, function(r) {
        .collapse_rival_ids(.rival_ids_in_column(
          get_state(adds$state_idx[r])[, adds$activity[r]], adds$firm[r]))
      }, character(1))
      disagree <- which(recomputed != adds$rival_ids[chk])
      if (length(disagree)) {
        r <- chk[disagree[1]]
        stop(length(disagree), " of ", length(chk), " decisions disagree with ",
             "the supplied holdings. The log and the holdings are not from ",
             "the same run. First mismatch: firm ", adds$firm[r],
             ", activity ", adds$activity[r], ", state ", adds$state_idx[r],
             ", log recorded rivals '", adds$rival_ids[r],
             "' but holdings give '", recomputed[disagree[1]], "'.")
      }
    }

    # For each ADD, the choice set is the activities the focal firm did NOT
    # hold before the decision. Under random choice from that set, the chance
    # of landing on a given rival's space is the rival's share of it.
    keys <- paste(adds$firm, adds$state_idx, sep = "_")
    uniq <- !duplicated(keys)
    exp_acc <- new.env(parent = emptyenv())

    for (r in which(uniq)) {
      i <- adds$firm[r]
      s <- adds$state_idx[r]
      if (is.na(s)) next
      H <- get_state(s)
      own <- H[i, ]
      # The focal cell has already been toggled in slice s on the env path, so
      # exclude the activity this decision landed on from its own choice set
      # rather than trusting own[] for that one cell.
      avail <- which(own == 0)
      j_hit <- adds$activity[r]
      avail <- union(avail, j_hit)
      if (length(avail) == 0) next
      sub <- H[, avail, drop = FALSE]
      share <- rowSums(sub > 0) / length(avail)
      share[i] <- NA_real_
      assign(keys[r], share, envir = exp_acc)
    }

    # Average each pair's expected rate over the focal firm's ADD occasions.
    exp_by_pair <- vapply(seq_len(nrow(pair)), function(k) {
      i <- pair$firm[k]; j <- pair$rival[k]
      rows <- which(adds$firm == i & !is.na(adds$state_idx))
      if (length(rows) == 0) return(NA_real_)
      ks <- unique(paste(i, adds$state_idx[rows], sep = "_"))
      vals <- vapply(ks, function(kk) {
        if (!exists(kk, envir = exp_acc, inherits = FALSE)) return(NA_real_)
        get(kk, envir = exp_acc)[j]
      }, numeric(1))
      mean(vals, na.rm = TRUE)
    }, numeric(1))

    pair$expected_rate      <- exp_by_pair
    pair$forbearance_index  <- pair$expected_rate - pair$observed_rate
    pair$exposure_corrected <- TRUE
  }

  pair$sparse <- pair$n_entries_focal < min_entries
  pair <- pair[, c("firm", "rival", "n_entries_focal", "n_entries_vs_rival",
                   "observed_rate", "expected_rate", "forbearance_index",
                   "exposure_corrected", "sparse")]
  pair <- pair[order(pair$firm, pair$rival), ]
  rownames(pair) <- NULL
  pair
}
