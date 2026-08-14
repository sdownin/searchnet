#' @title Export SaoMNK Simulation Results to CSV
#'
#' @description
#' Functions to export simulation results from \code{SaomNkRSienaBiEnv} objects
#' to CSV files for consumption by external tools such as Python/manim
#' visualizations. Each function extracts a specific aspect of the simulation
#' output (K-4 degree trajectories, bipartite matrix snapshots, actor utilities)
#' and writes it in a flat, analysis-ready format.
#'
#' @name searchnet-export
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Safely coerce factor columns to character for CSV output
#' @param df A data.frame possibly containing factor columns
#' @return The same data.frame with factors converted to character
#' @keywords internal
.defactor <- function(df) {
  factor_cols <- vapply(df, is.factor, logical(1))
  df[factor_cols] <- lapply(df[factor_cols], as.character)
  df
}


#' Validate that an env object has the expected simulation output
#' @param env A SaomNkRSienaBiEnv object
#' @param fields Character vector of field names to check
#' @return TRUE invisibly; stops with informative message on failure
#' @keywords internal
.validate_env <- function(env, fields = character(0)) {
  if (!inherits(env, "SaomNkRSienaBiEnv")) {
    stop("'env' must be a SaomNkRSienaBiEnv object.", call. = FALSE)
  }
  for (fld in fields) {
    if (is.null(env[[fld]])) {
      stop(
        sprintf(
          "'%s' is NULL. Run the simulation with returnChains=TRUE and call search_rsiena_process_stats() first.",
          fld
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}


# ---------------------------------------------------------------------------
# K-4 degree trajectories
# ---------------------------------------------------------------------------

#' Export K-4 degree trajectories to CSV
#'
#' Extracts the four K-degree measures (\code{K_AC}, \code{K_CA}, \code{K_AA},
#' \code{K_CC}) from the simulation chain and writes two CSV files: one with
#' actor-level detail per chain step, and one with per-step summary statistics
#' (mean, sd) aggregated across actors or components.
#'
#' The actor-level file contains columns:
#' \describe{
#'   \item{step}{Chain step id (integer)}
#'   \item{actor_id}{Actor id (for K_AC, K_AA) or NA}
#'   \item{component_id}{Component id (for K_CA, K_CC) or NA}
#'   \item{K_type}{One of K_AC, K_CA, K_AA, K_CC}
#'   \item{value}{Degree value at this step}
#'   \item{strategy}{Actor/component strategy label}
#'   \item{stability}{Whether the chain step was a no-change step}
#' }
#'
#' The summary file contains columns:
#' \describe{
#'   \item{step}{Chain step id}
#'   \item{K_type}{Degree type}
#'   \item{mean_value}{Mean degree across actors/components}
#'   \item{sd_value}{Standard deviation of degree}
#'   \item{min_value}{Minimum degree}
#'   \item{max_value}{Maximum degree}
#' }
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after simulation
#'   (with chain stats processed).
#' @param file Output CSV file path for the actor-level data. The summary file
#'   is written to the same directory with \code{_summary} appended before the
#'   extension.
#' @param include_new_old Logical; if \code{TRUE}, also export the NEW/OLD
#'   component decomposition variants (K_AC_NEW, K_AC_OLD, etc.). Default
#'   \code{FALSE}.
#' @return Invisible character vector of written file paths.
#' @export
searchnet_export_k4 <- function(env, file = "k4_trajectory.csv",
                                include_new_old = FALSE) {
  .validate_env(env, c("K_AC_df", "K_CA_df", "K_AA_df", "K_CC_df"))

  # ---- Assemble long-form K data ----
  k_frames <- list(
    data.frame(
      step         = as.integer(env$K_AC_df$chain_step_id),
      actor_id     = as.character(env$K_AC_df$actor_id),
      component_id = NA_character_,
      K_type       = "K_AC",
      value        = env$K_AC_df$value,
      strategy     = as.character(env$K_AC_df$strategy),
      stability    = env$K_AC_df$stability,
      stringsAsFactors = FALSE
    ),
    data.frame(
      step         = as.integer(env$K_AA_df$chain_step_id),
      actor_id     = as.character(env$K_AA_df$actor_id),
      component_id = NA_character_,
      K_type       = "K_AA",
      value        = env$K_AA_df$value,
      strategy     = as.character(env$K_AA_df$strategy),
      stability    = env$K_AA_df$stability,
      stringsAsFactors = FALSE
    ),
    data.frame(
      step         = as.integer(env$K_CA_df$chain_step_id),
      actor_id     = NA_character_,
      component_id = as.character(env$K_CA_df$component_id),
      K_type       = "K_CA",
      value        = env$K_CA_df$value,
      strategy     = as.character(env$K_CA_df$strategy),
      stability    = env$K_CA_df$stability,
      stringsAsFactors = FALSE
    ),
    data.frame(
      step         = as.integer(env$K_CC_df$chain_step_id),
      actor_id     = NA_character_,
      component_id = as.character(env$K_CC_df$component_id),
      K_type       = "K_CC",
      value        = env$K_CC_df$value,
      ## K_CC is component-by-component, so there is no actor on these rows and
      ## therefore no strategy: `strategy` is an actor attribute, and K_CC_df is
      ## built without it. Reading env$K_CC_df$strategy returned NULL, which
      ## data.frame() saw as a zero-length column against 160 rows and rejected
      ## with "arguments imply differing number of rows: 160, 1, 0". NA_character_
      ## matches how actor_id is already handled two lines above, for the same
      ## reason.
      strategy     = NA_character_,
      stability    = env$K_CC_df$stability,
      stringsAsFactors = FALSE
    )
  )

  if (include_new_old) {
    new_old_pairs <- list(
      list(df = "K_AC_NEW_df", type = "K_AC_NEW", id_col = "actor_id",     id_na = "component_id"),
      list(df = "K_AC_OLD_df", type = "K_AC_OLD", id_col = "actor_id",     id_na = "component_id"),
      list(df = "K_AA_NEW_df", type = "K_AA_NEW", id_col = "actor_id",     id_na = "component_id"),
      list(df = "K_AA_OLD_df", type = "K_AA_OLD", id_col = "actor_id",     id_na = "component_id"),
      list(df = "K_CA_NEW_df", type = "K_CA_NEW", id_col = "component_id", id_na = "actor_id"),
      list(df = "K_CA_OLD_df", type = "K_CA_OLD", id_col = "component_id", id_na = "actor_id"),
      list(df = "K_CC_NEW_df", type = "K_CC_NEW", id_col = "component_id", id_na = "actor_id"),
      list(df = "K_CC_OLD_df", type = "K_CC_OLD", id_col = "component_id", id_na = "actor_id")
    )
    for (p in new_old_pairs) {
      src <- env[[p$df]]
      if (!is.null(src) && nrow(src) > 0) {
        k_frames[[length(k_frames) + 1L]] <- data.frame(
          step         = as.integer(src$chain_step_id),
          actor_id     = if (p$id_col == "actor_id") as.character(src$actor_id) else NA_character_,
          component_id = if (p$id_col == "component_id") as.character(src$component_id) else NA_character_,
          K_type       = p$type,
          value        = src$value,
          strategy     = as.character(src$strategy),
          stability    = src$stability,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  k_long <- do.call(rbind, k_frames)

  # ---- Write actor/component-level file ----
  utils::write.csv(k_long, file = file, row.names = FALSE)

  # ---- Compute and write summary ----
  steps   <- sort(unique(k_long$step))
  k_types <- unique(k_long$K_type)
  summary_rows <- vector("list", length(steps) * length(k_types))
  idx <- 0L
  for (s in steps) {
    for (kt in k_types) {
      vals <- k_long$value[k_long$step == s & k_long$K_type == kt]
      if (length(vals) == 0) next
      idx <- idx + 1L
      summary_rows[[idx]] <- data.frame(
        step       = s,
        K_type     = kt,
        mean_value = mean(vals, na.rm = TRUE),
        sd_value   = stats::sd(vals, na.rm = TRUE),
        min_value  = min(vals, na.rm = TRUE),
        max_value  = max(vals, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
  summary_df <- do.call(rbind, summary_rows[seq_len(idx)])

  summary_file <- sub("(\\.csv)$", "_summary\\1", file)
  if (summary_file == file) summary_file <- paste0(file, "_summary.csv")
  utils::write.csv(summary_df, file = summary_file, row.names = FALSE)

  message(sprintf("Wrote K-4 trajectories to:\n  %s\n  %s", file, summary_file))
  invisible(c(detail = file, summary = summary_file))
}


# ---------------------------------------------------------------------------
# Bipartite matrix snapshots
# ---------------------------------------------------------------------------

#' Export bipartite matrix snapshots to CSV
#'
#' For selected chain steps, writes the M x N bipartite incidence matrix in
#' long format. This is useful for rendering network graphs in manim or other
#' visualization tools.
#'
#' Output columns:
#' \describe{
#'   \item{step}{Chain step id}
#'   \item{actor_id}{Row index (actor, 1-based)}
#'   \item{component_id}{Column index (component, 1-based)}
#'   \item{tie}{0 or 1 indicating whether a tie exists}
#' }
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after simulation.
#' @param steps Integer vector of chain step indices to export. If \code{NULL}
#'   (default), selects up to 20 evenly spaced steps across the full chain.
#' @param file Output CSV file path.
#' @param sparse Logical; if \code{TRUE} (default), only writes rows where
#'   \code{tie == 1}, substantially reducing file size for sparse networks.
#' @return Invisible path to the written file.
#' @export
searchnet_export_snapshots <- function(env, steps = NULL, file = "snapshots.csv",
                                       sparse = TRUE) {
  .validate_env(env, "bi_env_arr")

  arr <- env$bi_env_arr
  n_steps <- dim(arr)[3]
  M <- dim(arr)[1]
  N <- dim(arr)[2]

  # Select steps
  if (is.null(steps)) {
    max_snaps <- min(20L, n_steps)
    steps <- unique(round(seq(1, n_steps, length.out = max_snaps)))
  }
  steps <- steps[steps >= 1 & steps <= n_steps]
  if (length(steps) == 0) {
    stop("No valid steps to export. bi_env_arr has ", n_steps, " steps.", call. = FALSE)
  }

  # Build long-form data
  frames <- vector("list", length(steps))
  for (i in seq_along(steps)) {
    s <- steps[i]
    mat <- arr[, , s]
    if (sparse) {
      # Only active ties
      idx <- which(mat != 0, arr.ind = TRUE)
      if (nrow(idx) == 0) {
        frames[[i]] <- data.frame(
          step = integer(0), actor_id = integer(0),
          component_id = integer(0), tie = integer(0)
        )
      } else {
        frames[[i]] <- data.frame(
          step         = rep(as.integer(s), nrow(idx)),
          actor_id     = as.integer(idx[, 1]),
          component_id = as.integer(idx[, 2]),
          tie          = as.integer(mat[idx]),
          stringsAsFactors = FALSE
        )
      }
    } else {
      grid <- expand.grid(actor_id = seq_len(M), component_id = seq_len(N))
      grid$step <- as.integer(s)
      grid$tie  <- as.integer(c(mat))
      frames[[i]] <- grid[, c("step", "actor_id", "component_id", "tie")]
    }
  }

  out <- do.call(rbind, frames)
  utils::write.csv(out, file = file, row.names = FALSE)

  message(sprintf(
    "Wrote %d snapshots (%d rows) to:\n  %s",
    length(steps), nrow(out), file
  ))
  invisible(file)
}


# ---------------------------------------------------------------------------
# Actor utility trajectories
# ---------------------------------------------------------------------------

#' Export actor utility trajectories to CSV
#'
#' Extracts the actor utility data frame computed during the simulation chain
#' and writes it to CSV. Includes both the raw actor-level data and a per-step
#' summary.
#'
#' Actor-level output columns:
#' \describe{
#'   \item{step}{Chain step id}
#'   \item{actor_id}{Actor id}
#'   \item{utility}{Total utility value}
#'   \item{strategy}{Actor strategy label}
#'   \item{stability}{Whether this was a no-change step}
#' }
#'
#' Summary output columns:
#' \describe{
#'   \item{step}{Chain step id}
#'   \item{mean_utility}{Mean utility across actors}
#'   \item{sd_utility}{Standard deviation}
#'   \item{min_utility}{Minimum}
#'   \item{max_utility}{Maximum}
#' }
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after simulation.
#' @param file Output CSV file path for actor-level data. Summary file is
#'   written with \code{_summary} appended before the extension.
#' @return Invisible character vector of written file paths.
#' @export
searchnet_export_utility <- function(env, file = "utility_trajectory.csv") {
  .validate_env(env, "actor_util_df")

  util <- env$actor_util_df
  out <- data.frame(
    step      = as.integer(util$chain_step_id),
    actor_id  = as.character(util$actor_id),
    utility   = util$utility,
    strategy  = as.character(util$strategy),
    stability = util$stability,
    stringsAsFactors = FALSE
  )

  utils::write.csv(out, file = file, row.names = FALSE)

  # ---- Summary ----
  steps <- sort(unique(out$step))
  summary_rows <- vector("list", length(steps))
  for (i in seq_along(steps)) {
    s <- steps[i]
    vals <- out$utility[out$step == s]
    summary_rows[[i]] <- data.frame(
      step         = s,
      mean_utility = mean(vals, na.rm = TRUE),
      sd_utility   = stats::sd(vals, na.rm = TRUE),
      min_utility  = min(vals, na.rm = TRUE),
      max_utility  = max(vals, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
  summary_df <- do.call(rbind, summary_rows)

  summary_file <- sub("(\\.csv)$", "_summary\\1", file)
  if (summary_file == file) summary_file <- paste0(file, "_summary.csv")
  utils::write.csv(summary_df, file = summary_file, row.names = FALSE)

  message(sprintf("Wrote utility trajectories to:\n  %s\n  %s", file, summary_file))
  invisible(c(detail = file, summary = summary_file))
}


# ---------------------------------------------------------------------------
# Full export bundle
# ---------------------------------------------------------------------------

#' Export full simulation results for manim visualization
#'
#' Convenience wrapper that calls \code{\link{searchnet_export_k4}},
#' \code{\link{searchnet_export_snapshots}}, and
#' \code{\link{searchnet_export_utility}} to write a complete set of CSV files
#' into the specified directory.
#'
#' Also writes a small \code{metadata.csv} containing simulation parameters
#' (M, N, number of chain steps) for use by downstream scripts.
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after simulation.
#' @param dir Output directory. Created (recursively) if it does not exist.
#' @param prefix Optional string prepended to each output file name
#'   (e.g., \code{"sim01_"}).
#' @param include_new_old Logical; passed to \code{searchnet_export_k4}.
#'   Default \code{FALSE}.
#' @param snapshot_steps Integer vector passed to
#'   \code{searchnet_export_snapshots}. \code{NULL} for automatic selection.
#' @param sparse Logical; passed to \code{searchnet_export_snapshots}.
#'   Default \code{TRUE}.
#' @return Invisible named list of all written file paths.
#' @export
searchnet_export_all <- function(env, dir = "searchnet_export", prefix = "",
                                 include_new_old = FALSE,
                                 snapshot_steps = NULL,
                                 sparse = TRUE) {
  .validate_env(env, c("bi_env_arr", "actor_util_df",
                        "K_AC_df", "K_CA_df", "K_AA_df", "K_CC_df"))

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  .fp <- function(name) file.path(dir, paste0(prefix, name))

  # K-4 degrees
  k4_files <- searchnet_export_k4(
    env,
    file = .fp("k4_trajectory.csv"),
    include_new_old = include_new_old
  )

  # Snapshots
  snap_file <- searchnet_export_snapshots(
    env,
    steps = snapshot_steps,
    file  = .fp("snapshots.csv"),
    sparse = sparse
  )

  # Utility
  util_files <- searchnet_export_utility(
    env,
    file = .fp("utility_trajectory.csv")
  )

  # Metadata
  meta_file <- .fp("metadata.csv")
  n_steps <- dim(env$bi_env_arr)[3]
  meta <- data.frame(
    parameter = c("M", "N", "n_chain_steps"),
    value     = c(dim(env$bi_env_arr)[1],
                  dim(env$bi_env_arr)[2],
                  n_steps),
    stringsAsFactors = FALSE
  )
  utils::write.csv(meta, file = meta_file, row.names = FALSE)

  all_files <- list(
    k4_detail    = k4_files[["detail"]],
    k4_summary   = k4_files[["summary"]],
    snapshots    = snap_file,
    util_detail  = util_files[["detail"]],
    util_summary = util_files[["summary"]],
    metadata     = meta_file
  )

  message(sprintf(
    "\nExported %d files to %s/",
    length(all_files), normalizePath(dir, mustWork = FALSE)
  ))
  invisible(all_files)
}


# ---------------------------------------------------------------------------
# Dashboard-format export for manim scenes
# ---------------------------------------------------------------------------

#' Export simulation results in dashboard-scene CSV formats
#'
#' Produces the five CSV files consumed by the manim dashboard panel scenes
#' (\code{KDegreeEvolutionScene}, \code{BipartiteNetworkScene},
#' \code{FitnessLandscapeScene}, \code{ShockResponseScene}, and
#' \code{GameTheoreticScene}).  The output schemas match those expected by
#' \code{inst/manim/dashboard_panels.py} so the files can be dropped
#' directly into \code{inst/manim/data/} and rendered.
#'
#' Five files are written:
#'
#' \describe{
#'   \item{\code{k4_trajectories.csv}}{
#'     Columns: \code{round}, \code{carrier}, \code{K_AC}, \code{K_CC},
#'     \code{K_CA}, \code{K_AA}.
#'     One row per carrier per round (mean across actors/components within
#'     each round).
#'   }
#'   \item{\code{bipartite_snapshots.csv}}{
#'     Columns: \code{round}, \code{carrier}, \code{route}, \code{active}.
#'     Long-form active ties at selected snapshot steps.
#'   }
#'   \item{\code{utility_trajectories.csv}}{
#'     Columns: \code{round}, \code{carrier}, \code{total_utility},
#'     \code{nk_component}, \code{scope_cost}, \code{popularity},
#'     \code{rivalry}.
#'     One row per carrier per round.
#'   }
#'   \item{\code{shock_events.csv}}{
#'     Columns: \code{round}, \code{shock_type}, \code{magnitude},
#'     \code{description}.
#'     Empty by default (baseline with no shocks); populate via
#'     \code{shock_df} argument.
#'   }
#'   \item{\code{phase_space.csv}}{
#'     Columns: \code{round}, \code{carrier}, \code{PC1}, \code{PC2},
#'     \code{PC3}.
#'     PCA reduction of carrier state vectors sampled at up to 50
#'     evenly spaced steps.
#'   }
#' }
#'
#' @param env A \code{SaomNkRSienaBiEnv} object after simulation
#'   (with \code{bi_env_arr} populated and, optionally, chain stats
#'   processed for utility data).
#' @param dir Output directory.  Created recursively if it does not exist.
#'   Defaults to the package's \code{inst/manim/data} directory when
#'   \code{NULL}.
#' @param carrier_labels Character vector of carrier labels. Must have
#'   length equal to the number of actors (\code{M}).  If \code{NULL}
#'   (default), generates labels \code{A1, A2, ...}.
#' @param n_snapshots Integer; number of evenly-spaced bipartite snapshots
#'   to export.  Default 20.
#' @param n_phase_samples Integer; number of evenly-spaced steps for
#'   phase-space PCA.  Default 50.
#' @param shock_df Optional data.frame with columns \code{round},
#'   \code{shock_type}, \code{magnitude}, \code{description}.  Written
#'   directly to \code{shock_events.csv}.  If \code{NULL} (default), an
#'   empty template is written.
#' @param theta Named numeric vector of SAOM objective-function parameters
#'   used for utility decomposition when \code{actor_util_df} is not
#'   available.  Expected names: \code{density}, \code{inPop},
#'   \code{outAct}.  Default \code{c(density = -1.5, inPop = -6.0,
#'   outAct = 0.3)}.
#'
#' @return Invisible named character vector of the five written file paths.
#' @export
searchnet_export_for_manim <- function(env,
                                       dir = NULL,
                                       carrier_labels = NULL,
                                       n_snapshots = 20L,
                                       n_phase_samples = 50L,
                                       shock_df = NULL,
                                       theta = c(density = -1.5,
                                                  inPop  = -6.0,
                                                  outAct =  0.3)) {
  .validate_env(env, "bi_env_arr")

  arr <- env$bi_env_arr
  M       <- dim(arr)[1]
  N       <- dim(arr)[2]
  n_steps <- dim(arr)[3]

  # Resolve output directory

  if (is.null(dir)) {
    pkg_root <- system.file(package = "searchnet")
    if (nzchar(pkg_root)) {
      dir <- file.path(pkg_root, "manim", "data")
    } else {
      dir <- "searchnet_manim_data"
    }
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Carrier labels
  if (is.null(carrier_labels)) {
    carrier_labels <- paste0("A", seq_len(M))
  }
  stopifnot(length(carrier_labels) == M)

  # Map chain steps to rounds (each round = M ministeps)
  step_to_round <- function(s) ceiling(s / M)

  # -------------------------------------------------------------------
  # 1. k4_trajectories.csv
  # -------------------------------------------------------------------
  message("  Exporting k4_trajectories.csv ...")
  k4_rows <- vector("list", n_steps * M)
  idx <- 0L
  for (s in seq_len(n_steps)) {
    mat <- arr[, , s]
    round_num <- step_to_round(s)
    soc  <- mat %*% t(mat); diag(soc) <- 0
    epi  <- t(mat) %*% mat; diag(epi) <- 0

    for (c_i in seq_len(M)) {
      row_vec <- mat[c_i, ]
      K_AC <- sum(row_vec)
      K_AA <- sum(soc[c_i, ] > 0)
      K_CA <- sum(mat[, row_vec > 0] > 0) - K_AC
      K_CC_routes <- if (K_AC > 0) {
        mean(colSums(epi[row_vec > 0, , drop = FALSE] > 0))
      } else {
        0
      }

      idx <- idx + 1L
      k4_rows[[idx]] <- data.frame(
        round   = round_num,
        carrier = carrier_labels[c_i],
        K_AC    = K_AC,
        K_CC    = K_CC_routes,
        K_CA    = K_CA,
        K_AA    = K_AA,
        stringsAsFactors = FALSE
      )
    }
  }
  k4_all <- do.call(rbind, k4_rows[seq_len(idx)])

  # Average across steps within each round
  k4_avg <- stats::aggregate(
    cbind(K_AC, K_CC, K_CA, K_AA) ~ round + carrier,
    data = k4_all, FUN = mean
  )
  k4_avg <- k4_avg[order(k4_avg$round, k4_avg$carrier), ]

  k4_file <- file.path(dir, "k4_trajectories.csv")
  utils::write.csv(k4_avg, k4_file, row.names = FALSE)
  message(sprintf("    k4_trajectories.csv: %d rows", nrow(k4_avg)))

  # -------------------------------------------------------------------
  # 2. bipartite_snapshots.csv
  # -------------------------------------------------------------------
  message("  Exporting bipartite_snapshots.csv ...")
  snap_steps <- unique(round(seq(1, n_steps,
                                  length.out = min(n_snapshots, n_steps))))
  snap_rows <- list()
  for (s in snap_steps) {
    mat <- arr[, , s]
    round_num <- step_to_round(s)
    idx_active <- which(mat != 0, arr.ind = TRUE)
    if (nrow(idx_active) > 0) {
      snap_rows[[length(snap_rows) + 1L]] <- data.frame(
        round   = rep(round_num, nrow(idx_active)),
        carrier = carrier_labels[idx_active[, 1]],
        route   = as.integer(idx_active[, 2]),
        active  = 1L,
        stringsAsFactors = FALSE
      )
    }
  }
  snap_df <- if (length(snap_rows) > 0) do.call(rbind, snap_rows) else {
    data.frame(round = integer(0), carrier = character(0),
               route = integer(0), active = integer(0),
               stringsAsFactors = FALSE)
  }
  snap_file <- file.path(dir, "bipartite_snapshots.csv")
  utils::write.csv(snap_df, snap_file, row.names = FALSE)
  message(sprintf("    bipartite_snapshots.csv: %d rows", nrow(snap_df)))

  # -------------------------------------------------------------------
  # 3. utility_trajectories.csv
  # -------------------------------------------------------------------
  message("  Exporting utility_trajectories.csv ...")

  use_actual_util <- FALSE
  if (!is.null(env$actor_util_df) && nrow(env$actor_util_df) > 0) {
    util <- env$actor_util_df
    step_ids <- suppressWarnings(as.integer(as.character(util$chain_step_id)))
    actor_ids <- suppressWarnings(as.integer(as.character(util$actor_id)))
    if (!all(is.na(step_ids)) && !all(is.na(actor_ids))) {
      util_out <- data.frame(
        round          = step_to_round(step_ids),
        carrier        = carrier_labels[pmin(actor_ids, M)],
        total_utility  = as.numeric(util$utility),
        nk_component   = NA_real_,
        scope_cost     = NA_real_,
        popularity     = NA_real_,
        rivalry        = NA_real_,
        stringsAsFactors = FALSE
      )
      util_out <- util_out[complete.cases(util_out[, c("round", "carrier")]), ]
      use_actual_util <- nrow(util_out) > 0
    }
  }
  if (!use_actual_util) {
    # Fallback: compute approximate utility from bipartite matrices
    util_rows <- vector("list", n_steps * M)
    uidx <- 0L
    for (s in seq_len(n_steps)) {
      mat <- arr[, , s]
      round_num <- step_to_round(s)
      soc <- mat %*% t(mat); diag(soc) <- 0
      for (c_i in seq_len(M)) {
        K_AC <- sum(mat[c_i, ])
        K_AA <- sum(soc[c_i, ] > 0)
        density_u <- theta[["density"]] * K_AC
        inpop_u   <- theta[["inPop"]]   * K_AA
        outact_u  <- theta[["outAct"]]  * K_AC
        total_u   <- density_u + inpop_u + outact_u

        uidx <- uidx + 1L
        util_rows[[uidx]] <- data.frame(
          round          = round_num,
          carrier        = carrier_labels[c_i],
          total_utility  = total_u,
          nk_component   = outact_u,
          scope_cost     = density_u,
          popularity     = 0,
          rivalry        = inpop_u,
          stringsAsFactors = FALSE
        )
      }
    }
    util_out <- do.call(rbind, util_rows[seq_len(uidx)])
  }

  # Average within round x carrier
  # Replace NA columns with 0 for aggregation
  for (.col in c("nk_component", "scope_cost", "popularity", "rivalry")) {
    if (.col %in% names(util_out)) util_out[[.col]][is.na(util_out[[.col]])] <- 0
  }
  util_avg <- stats::aggregate(
    cbind(total_utility, nk_component, scope_cost,
          popularity, rivalry) ~ round + carrier,
    data = util_out, FUN = mean
  )
  util_avg <- util_avg[order(util_avg$round, util_avg$carrier), ]

  util_file <- file.path(dir, "utility_trajectories.csv")
  utils::write.csv(util_avg, util_file, row.names = FALSE)
  message(sprintf("    utility_trajectories.csv: %d rows", nrow(util_avg)))

  # -------------------------------------------------------------------
  # 4. shock_events.csv
  # -------------------------------------------------------------------
  message("  Exporting shock_events.csv ...")
  if (is.null(shock_df)) {
    shock_df <- data.frame(
      round       = integer(0),
      shock_type  = character(0),
      magnitude   = numeric(0),
      description = character(0),
      stringsAsFactors = FALSE
    )
  }
  shock_file <- file.path(dir, "shock_events.csv")
  utils::write.csv(shock_df, shock_file, row.names = FALSE)
  message(sprintf("    shock_events.csv: %d rows", nrow(shock_df)))

  # -------------------------------------------------------------------
  # 5. phase_space.csv  (PCA of carrier state vectors)
  # -------------------------------------------------------------------
  message("  Exporting phase_space.csv ...")
  phase_steps <- unique(round(seq(1, n_steps,
                                   length.out = min(n_phase_samples, n_steps))))

  state_matrix <- matrix(0, nrow = length(phase_steps) * M, ncol = N)
  meta_round   <- integer(length(phase_steps) * M)
  meta_carrier <- character(length(phase_steps) * M)
  pidx <- 0L
  for (s in phase_steps) {
    mat <- arr[, , s]
    round_num <- step_to_round(s)
    for (c_i in seq_len(M)) {
      pidx <- pidx + 1L
      state_matrix[pidx, ] <- mat[c_i, ]
      meta_round[pidx]   <- round_num
      meta_carrier[pidx] <- carrier_labels[c_i]
    }
  }

  if (pidx > 3 && N > 3) {
    state_matrix <- state_matrix[seq_len(pidx), , drop = FALSE]
    meta_round   <- meta_round[seq_len(pidx)]
    meta_carrier <- meta_carrier[seq_len(pidx)]

    col_var <- apply(state_matrix, 2, stats::var)
    state_filtered <- state_matrix[, col_var > 0, drop = FALSE]

    if (ncol(state_filtered) >= 3) {
      pca_result <- stats::prcomp(state_filtered, center = TRUE, scale. = TRUE)
      pc_scores  <- pca_result$x[, 1:3, drop = FALSE]
      phase_out <- data.frame(
        round   = meta_round,
        carrier = meta_carrier,
        PC1     = pc_scores[, 1],
        PC2     = pc_scores[, 2],
        PC3     = pc_scores[, 3],
        stringsAsFactors = FALSE
      )
    } else {
      phase_out <- data.frame(
        round   = meta_round,
        carrier = meta_carrier,
        PC1     = state_filtered[, 1],
        PC2     = if (ncol(state_filtered) >= 2) state_filtered[, 2] else 0,
        PC3     = 0,
        stringsAsFactors = FALSE
      )
    }
  } else {
    phase_out <- data.frame(
      round = integer(0), carrier = character(0),
      PC1 = numeric(0), PC2 = numeric(0), PC3 = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  phase_file <- file.path(dir, "phase_space.csv")
  utils::write.csv(phase_out, phase_file, row.names = FALSE)
  message(sprintf("    phase_space.csv: %d rows", nrow(phase_out)))

  # -------------------------------------------------------------------
  # Summary
  # -------------------------------------------------------------------
  all_files <- c(
    k4_trajectories       = k4_file,
    bipartite_snapshots   = snap_file,
    utility_trajectories  = util_file,
    shock_events          = shock_file,
    phase_space           = phase_file
  )

  message(sprintf(
    "\nExported 5 dashboard CSVs to %s/",
    normalizePath(dir, mustWork = FALSE)
  ))
  invisible(all_files)
}
