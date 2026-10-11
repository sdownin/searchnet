###############################################################################
## searchnet-path.R
##
## State-carrying simulation for search_rsiena() (0.11.0).
##
## Before 0.11.0, search_rsiena() ran siena07(simOnly = TRUE) under RSiena's
## conditional default with two identical waves and n3 = the step count. With
## a conditional target distance of 0, every phase-3 run stopped after ONE
## ministep, and RSiena started every run from wave 1. The runs were then
## concatenated and replayed cumulatively from the initial matrix as though
## they were one path. No simulated decision ever responded to the current
## state, and the fixed ministep budget made any rate effect zero-sum.
##
## What replaces it. A call simulates one unit of model time, split into
## segments. A segment is a maximal block of identical theta rows. Each
## segment is ONE unconditional RSiena period (cond = FALSE, nsub = 0,
## n3 = 2, run 1 kept), started from the previous segment's end state, with
## basic rate f * iterations_per_actor where f is the segment's share of the
## rows. RSiena's own end network of segment s is the start of segment s + 1,
## and the engine stops if the within-segment replay of RSiena's chain does
## not end exactly at RSiena's end network.
##
## The old route survives only as search_rsiena(path = "legacy_replay"), for
## reproducing archived numbers. Its output is tagged as independent draws,
## and every consumer that reads a path refuses it.
###############################################################################

.SEARCHNET_PATH_GENUINE <- "genuine"
.SEARCHNET_PATH_LEGACY  <- "independent_draws"


## ---------------------------------------------------------------------------
## Path tags and the consumer guard
## ---------------------------------------------------------------------------

.searchnet_tag_path <- function(arr, kind) {
  if (!is.null(arr)) attr(arr, "searchnet_path") <- kind
  arr
}

.searchnet_path_kind <- function(env) {
  kind <- tryCatch(attr(env$bi_env_arr, "searchnet_path"), error = function(e) NULL)
  if (is.null(kind))
    kind <- tryCatch(env$searchnet_path_kind, error = function(e) NULL)
  kind
}

## Every consumer that reads $bi_env_arr, $chain_stats or a statistic computed
## from them as a path calls this first. It refuses the legacy route's output
## with an error, and passes anything else (including an environment that has
## not been simulated, which the consumer reports in its own words).
.searchnet_require_path <- function(env, caller = "this function") {
  if (identical(.searchnet_path_kind(env), .SEARCHNET_PATH_LEGACY)) {
    stop(structure(
      class = c("searchnet_not_a_path_error", "error", "condition"),
      list(message = paste0(
        caller, " refuses this environment: it was simulated with ",
        "search_rsiena(path = \"legacy_replay\"), whose $bi_env_arr replays ",
        "independent one-ministep draws as though they were a path. Nothing ",
        "computed from it describes the model's dynamics. Re-run with the ",
        "default path = \"genuine\"."),
        call = NULL)))
  }
  invisible(TRUE)
}

## The one warning the legacy route raises, on every call.
.searchnet_warn_not_a_path <- function() {
  warning(structure(
    class = c("searchnet_not_a_path", "warning", "condition"),
    list(message = paste0(
      "search_rsiena(path = \"legacy_replay\"): the result is a sequence of ",
      "independent one-ministep draws from the starting state, replayed as ",
      "though it were a path. It is kept only to reproduce archived numbers; ",
      "every path consumer refuses it."),
      call = NULL)))
}


## ---------------------------------------------------------------------------
## Segments (S2, S5)
## ---------------------------------------------------------------------------
## A segment is a maximal block of consecutive identical theta rows. `breaks`
## are extra row indices at which a segment must start (shock boundaries), so
## two shocks that happen to carry the same values still map onto their own
## ministeps. Beyond `max_segments`, the run is cut into `max_segments`
## equal-duration segments, each carrying the theta row at its midpoint.
.searchnet_segments <- function(theta_matrix, breaks = integer(0),
                                max_segments = 50L, quiet = FALSE) {
  n <- nrow(theta_matrix)
  if (is.null(n) || n < 1L) stop("theta_matrix has no rows.", call. = FALSE)
  if (n > 1L) {
    d <- theta_matrix[-1L, , drop = FALSE] != theta_matrix[-n, , drop = FALSE]
    d[is.na(d)] <- TRUE
    change <- which(rowSums(d) > 0) + 1L
  } else {
    change <- integer(0)
  }
  breaks <- as.integer(breaks)
  breaks <- breaks[!is.na(breaks) & breaks > 1L & breaks <= n]
  starts <- sort(unique(c(1L, change, breaks)))
  ends   <- c(starts[-1L] - 1L, n)
  coarsened <- FALSE
  if (length(starts) > max_segments) {
    k <- as.integer(max_segments)
    b <- unique(round(seq(0, n, length.out = k + 1L)))
    starts <- b[-length(b)] + 1L
    ends   <- b[-1L]
    coarsened <- TRUE
    if (!quiet)
      message(sprintf(paste0(
        "searchnet: the theta matrix has more distinct segments than ",
        "max_segments = %d; simulating %d equal-duration segments, each at ",
        "the theta row at its midpoint."), k, length(starts)))
  }
  data.frame(
    segment_id = seq_along(starts),
    row_start  = starts,
    row_end    = ends,
    n_rows     = ends - starts + 1L,
    theta_row  = if (coarsened) as.integer(floor((starts + ends) / 2)) else starts,
    stringsAsFactors = FALSE
  )
}


## ---------------------------------------------------------------------------
## Seeds (S4)
## ---------------------------------------------------------------------------
## Segment seeds are drawn once per call from run_seed, and the global RNG
## state is restored afterward. A seed of the form run_seed + s would make
## replication r's segment 2 the same stream as replication r + 1's segment 1.
.searchnet_segment_seeds <- function(run_seed, n) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(as.integer(run_seed))
  sample.int(.Machine$integer.max, as.integer(n))
}


## ---------------------------------------------------------------------------
## Full RSiena theta for one unconditional period
## ---------------------------------------------------------------------------
## `theta_row` has the width of the theta matrix search_rsiena() has always
## built: without the basic rate for a single dependent variable (RSiena's
## conditional width), with every basic rate when there are two. RSiena's
## unconditional width includes every basic rate, so they are inserted here.
##
## The bipartite basic rate is `bip_rate` (S1). Any other DV's basic rate is
## scaled by the same factor, preserving the ratio the theta row declares
## between the two DVs' rates.
.searchnet_full_theta <- function(env, theta_row, bip_rate) {
  full <- env$get_rsiena_effects_theta_df(no_rates = FALSE)
  is_basic <- full$shortName == "Rate" & full$type == "rate"
  n_dv <- env$get_n_rsiena_depvars()
  out <- numeric(nrow(full))
  names(out) <- full$effect_level
  theta_row <- as.numeric(theta_row)
  if (n_dv > 1L) {
    if (length(theta_row) != nrow(full))
      stop(sprintf("theta matrix has %d columns; RSiena expects %d for this model.",
                   length(theta_row), nrow(full)), call. = FALSE)
    out[!is_basic] <- theta_row[!is_basic]
    bip_idx <- which(is_basic & full$name == "self$bipartite_rsienaDV")
    declared_bip <- if (length(bip_idx)) theta_row[bip_idx[1L]] else NA_real_
    for (r in which(is_basic)) {
      if (full$name[r] == "self$bipartite_rsienaDV") {
        out[r] <- bip_rate
      } else if (is.finite(declared_bip) && declared_bip > 0) {
        out[r] <- bip_rate * theta_row[r] / declared_bip
      } else {
        out[r] <- bip_rate * theta_row[r]
      }
    }
  } else {
    if (length(theta_row) != sum(!is_basic))
      stop(sprintf("theta matrix has %d columns; RSiena expects %d for this model.",
                   length(theta_row), sum(!is_basic)), call. = FALSE)
    out[!is_basic] <- theta_row
    out[is_basic]  <- bip_rate
  }
  out
}


## ---------------------------------------------------------------------------
## Chain parsing and replay
## ---------------------------------------------------------------------------
## RSiena returns one period's ministep chain in one of two formats, and which
## one depends on the RSiena version AND on `returnDataFrame`:
##
##   LIST format (RSiena <= 1.5.x always; RSiena >= 1.6.0 with
##   returnDataFrame = FALSE): a list with one record per ministep, IN CHAIN
##   ORDER. Each record is a list of 13 declared fields:
##     [[1]] aspect ("Network"/"Behavior")  [[2]] 0 = network, 1 = behavior
##     [[3]] dependent variable name        [[4]] ego (0-indexed)
##     [[5]] alter (0-indexed)              [[6]] behavior difference
##     [[7]] reciprocal rate                [[8]] log option-set probability
##     [[9]] log choice probability         [[10]], [[11]] zero-length
##     [[12]] RSiena's missing flag         [[13]] RSiena's diagonal flag
##   searchnet has always called [[12]] "diagonal" and [[13]] "stability"; the
##   column names are kept for compatibility. [[13]] is TRUE for a no-change
##   ministep (bipartite: alter == N on the 0-indexed scale).
##
##   DATA-FRAME format (RSiena >= 1.6.0 with returnDataFrame = TRUE, verified
##   on 1.6.6; RSiena 1.5.0 ignored the flag in forward simulation): a
##   `chains.data.frame` with ten columns Aspect, Var, VarName, Ego, Alter,
##   Diff, ReciRate, LogOptionSetProb, LogChoiceProb, Diagonal, one row per
##   ministep. The missing flag (list field [[12]]) is not carried. The rows
##   are SORTED BY EGO AND ALTER, not in chain order; the row names keep each
##   ministep's chain position (1..n). Indices stay 0-based.
##
## Both formats are detected structurally, not by version string.
##
## A related RSiena 1.6.x change: sienaDataCreate() returns class "sienadata"
## (1.5.0: "siena"); a sienaGroup is still c("sienaGroup", "siena").
.searchnet_is_siena_data <- function(x) inherits(x, c("siena", "sienadata"))

.SEARCHNET_RSIENA_CHAIN_DF_COLS <- c("Aspect", "Var", "VarName", "Ego", "Alter",
                                     "Diff", "ReciRate", "LogOptionSetProb",
                                     "LogChoiceProb", "Diagonal")

## A data-frame chain put back into chain order by its row names. Stops if the
## row names are not a permutation of 1..n, since the order would then be lost.
.searchnet_chain_df_ordered <- function(x) {
  miss <- setdiff(.SEARCHNET_RSIENA_CHAIN_DF_COLS, names(x))
  if (length(miss))
    stop(sprintf(paste0(
      "searchnet: unrecognized RSiena ministep chain format: a data frame ",
      "without column(s) %s (columns: %s). RSiena's chain format may have ",
      "changed again."), paste(miss, collapse = ", "),
      paste(names(x), collapse = ", ")), call. = FALSE)
  n <- nrow(x)
  if (!n) return(x)
  pos <- suppressWarnings(as.integer(rownames(x)))
  if (anyNA(pos) || !identical(sort(pos), seq_len(n)))
    stop(paste0(
      "searchnet: RSiena returned the ministep chain as a data frame whose ",
      "row names are not the chain positions 1..n, so the order of the ",
      "ministeps cannot be recovered. Simulate with returnDataFrame = FALSE."),
      call. = FALSE)
  x[order(pos), , drop = FALSE]
}

## Is `ministeps` (one period of `fit$chain`) in RSiena's data-frame format?
.searchnet_chain_is_df <- function(ministeps) is.data.frame(ministeps)

## Number of ministeps in one period of `fit$chain`, either format.
.searchnet_chain_n <- function(ministeps) {
  if (.searchnet_chain_is_df(ministeps)) nrow(ministeps) else length(ministeps)
}

## The four fields every replay needs, typed, in chain order, either format:
## dependent variable name, 0-indexed ego and alter, and the no-change flag
## (declared field 13 / column Diagonal).
.searchnet_chain_fields <- function(ministeps) {
  if (.searchnet_chain_is_df(ministeps)) {
    d <- .searchnet_chain_df_ordered(ministeps)
    return(list(name  = as.character(d$VarName),
                ego   = as.integer(d$Ego),
                alter = as.integer(d$Alter),
                stab  = as.logical(d$Diagonal)))
  }
  .searchnet_chain_check_list(ministeps)
  list(name  = vapply(ministeps, function(x) as.character(x[[3]]), character(1)),
       ego   = vapply(ministeps, function(x) as.integer(x[[4]]),   integer(1)),
       alter = vapply(ministeps, function(x) as.integer(x[[5]]),   integer(1)),
       stab  = vapply(ministeps, function(x) as.logical(x[[13]]),  logical(1)))
}

## Guard for the list format: every record must declare 13 fields with
## [[10]] and [[11]] zero-length, or the positional reading below is wrong.
.searchnet_chain_check_list <- function(ministeps) {
  if (!length(ministeps)) return(invisible(TRUE))
  ok <- is.list(ministeps) && all(vapply(ministeps, function(x)
    is.list(x) && length(x) == 13L && !length(x[[10]]) && !length(x[[11]]),
    logical(1)))
  if (!ok)
    stop(paste0(
      "searchnet: unrecognized RSiena ministep chain format: expected a list ",
      "of 13-field ministep records (fields 10 and 11 empty) or a ",
      "chains.data.frame. RSiena's chain format may have changed again."),
      call. = FALSE)
  invisible(TRUE)
}

## One period's ministeps as the 11-column character frame
## search_rsiena_process_ministep_chain() has always built, in chain order,
## from either format. In the list format fields 10 and 11 are zero-length, so
## unlisting leaves 11 values per ministep. The data-frame format does not
## carry the missing flag, so `diagonal` is NA there.
.searchnet_chain_frame <- function(ministeps) {
  cols <- c("dv_type", "dv_type_bin", "dv_varname", "id_from", "id_to",
            "beh_difference", "reciprocal_rate", "LogOptionSetProb",
            "LogChoiceProb", "diagonal", "stability")
  if (!.searchnet_chain_n(ministeps)) {
    df <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                        stringsAsFactors = FALSE)
    return(df)
  }
  if (.searchnet_chain_is_df(ministeps)) {
    d <- .searchnet_chain_df_ordered(ministeps)
    df <- data.frame(
      dv_type = as.character(d$Aspect), dv_type_bin = as.character(d$Var),
      dv_varname = as.character(d$VarName), id_from = as.character(d$Ego),
      id_to = as.character(d$Alter), beh_difference = as.character(d$Diff),
      reciprocal_rate = as.character(d$ReciRate),
      LogOptionSetProb = as.character(d$LogOptionSetProb),
      LogChoiceProb = as.character(d$LogChoiceProb),
      diagonal = NA_character_, stability = as.character(d$Diagonal),
      stringsAsFactors = FALSE)
    rownames(df) <- NULL
    return(df)
  }
  .searchnet_chain_check_list(ministeps)
  m <- t(matrix(unlist(ministeps), ncol = length(ministeps)))
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- cols
  df
}

## One long K table of get_chain_stats_list(): chain_step_id, the node id (a factor), value and
## stability, then the `extra` columns. Rows are steps in order, nodes
## fastest; `vals` holds one vector per step. as.factor() over one step's ids,
## repeated per step, gives the codes and levels as.factor() over the long
## column gives.
.searchnet_chain_long <- function(vals, ids, id_name, steps, stab, extra = list()) {
  n_per <- length(ids)
  cols <- list(rep(steps, each = n_per), rep(as.factor(ids), length(steps)),
               unlist(vals, use.names = FALSE), rep(stab, each = n_per))
  names(cols) <- c('chain_step_id', id_name, 'value', 'stability')
  data.table::rbindlist(list(c(cols, extra)))
}

## RSiena's end-of-period bipartite network for one run's `$sims` entry.
.searchnet_sims_bipartite <- function(sims_run, M, N,
                                      dv = "self$bipartite_rsienaDV") {
  grp <- sims_run[[1L]]
  el <- if (!is.null(names(grp)) && dv %in% names(grp)) grp[[dv]] else grp[[1L]]
  if (is.list(el)) el <- el[[1L]]
  B <- matrix(0, M, N)
  if (length(el) && NROW(el)) {
    el <- as.matrix(el)
    B[cbind(el[, 1], el[, 2])] <- el[, 3]
  }
  B
}

## RSiena's end-of-period behavior vector for one run's `$sims` entry.
.searchnet_sims_behavior <- function(sims_run, dv) {
  v <- sims_run[[1L]][[dv]]
  if (is.list(v)) v <- v[[1L]]
  as.numeric(v)
}

## Replay a period's realized bipartite toggles from `B0`.
.searchnet_path_replay_end <- function(B0, frame, N,
                                       dv = "self$bipartite_rsienaDV") {
  B <- B0
  if (!nrow(frame)) return(B)
  stab <- as.logical(frame$stability)
  from <- as.numeric(frame$id_from) + 1
  to   <- as.numeric(frame$id_to) + 1
  for (k in which(!stab & frame$dv_varname == dv & to <= N)) {
    B[from[k], to[k]] <- 1 - B[from[k], to[k]]
  }
  B
}


## ---------------------------------------------------------------------------
## One unconditional period
## ---------------------------------------------------------------------------
## Builds the RSiena data at the given start state: both bipartite waves are
## the start state. For a behavior DV, wave 1 is the carried behavior and wave
## 2 is the declared starting behavior, which keeps RSiena's behavior range
## equal to the declared one (RSiena derives the range from all waves).
.searchnet_prepare_period_data <- function(env, structure_model, B,
                                           beh = NULL, verbose = FALSE) {
  env$bipartite_rsienaDV <- RSiena::sienaDependent(
    array(c(B, B), dim = c(env$M, env$N, 2)),
    type = "bipartite", nodeSet = c("ACTORS", "COMPONENTS"), allowOnly = FALSE)
  sm <- structure_model
  if (.searchnet_has_behavior(sm) && !is.null(beh)) {
    decl <- sm$dv_behavior$values
    decl1 <- if (is.matrix(decl)) decl[, 1L] else decl
    sm$dv_behavior$values <- cbind(as.numeric(beh), as.numeric(decl1))
  }
  env$set_behavior_rsienaDV(sm, verbose = verbose)
  env$rsiena_data    <- env$get_rsiena_data_from_structure_model(sm)
  env$rsiena_effects <- RSiena::getEffects(env$rsiena_data)
  env$add_rsiena_effects(sm, verbose = verbose)
  invisible(env)
}

## Run one unconditional period from the data currently on `env` and return
## run 1. `n3 = 1` fails inside siena07() under cond = FALSE; run 1 is
## identical for n3 = 2 and n3 = 3 (pre-registration, probe P2).
.searchnet_run_period <- function(env, theta_full, seed, verbose = FALSE) {
  ## An actor degree cap is RSiena's MaxDegree on the bipartite DV
  ## (R/searchnet-degree-bounds.R); NULL leaves the algorithm as before.
  alg_args <- .searchnet_bounds_algorithm_args(
    list(projname = NULL, simOnly = TRUE, cond = FALSE, nsub = 0, n3 = 2,
         seed = as.integer(seed), silent = !verbose),
    .searchnet_model_bounds(env$config_structure_model),
    dv_name = "self$bipartite_rsienaDV", N = env$N, where = "search_rsiena()")
  alg <- do.call(RSiena::sienaAlgorithmCreate, alg_args)
  env$rsiena_algorithm <- alg
  tv <- rbind(theta_full, theta_full)
  ## siena07() refuses any |theta| above thetaBound (default 50), and under
  ## time semantics a basic rate is routinely larger (pre-registration,
  ## deviation 2).
  bound <- max(50, ceiling(2 * max(abs(theta_full))) + 1)
  run <- function() RSiena::siena07(
    alg, data = env$rsiena_data, effects = env$rsiena_effects,
    thetaValues = tv, thetaBound = bound, batch = TRUE, silent = !verbose,
    returnDeps = TRUE, returnChains = TRUE, returnThetas = TRUE,
    ## returnDataFrame = FALSE keeps the chain in RSiena's list format, in
    ## chain order, on every RSiena version. With TRUE, RSiena >= 1.6.0
    ## returns an ego/alter-sorted data frame instead (1.5.0 ignored the
    ## flag here, so the 1.5.0 chain is byte-identical either way).
    returnDataFrame = FALSE, returnLoglik = TRUE, verbose = verbose)
  fit <- if (verbose) run() else {
    res <- NULL
    utils::capture.output(res <- run())
    res
  }
  fit
}


## ---------------------------------------------------------------------------
## Rate effects RSiena cannot simulate unconditionally here
## ---------------------------------------------------------------------------
## Verified 2026-10-07 on RSiena 1.5.0, plain RSiena without searchnet: an
## unconditional simulation of a bipartite dependent variable with `outRate`,
## `outRateInv` or `outRateLog` together with `inPop` or `XWX` corrupts the
## heap, and R crashes at the next garbage collection (Windows status
## 0xC0000374 / 0xC0000005). With density alone or with egoX it ran. Which
## other combinations are affected is not known, and a corrupted heap need
## not crash, so the degree-dependent rate effects are refused outright on
## the state-carrying route rather than risk a crash or a silently wrong path.
## Before 0.11.0 they "ran" because every phase-3 run was a single ministep
## under a fixed budget, where no rate effect could act (ledger B18).
.SEARCHNET_UNSAFE_RATE_EFFECTS <- c("outRate", "outRateInv", "outRateLog",
                                    "inRate", "inRateInv", "inRateLog")

.searchnet_check_rate_effects <- function(env) {
  full <- env$get_rsiena_effects_theta_df(no_rates = FALSE)
  bad <- full$type == "rate" & full$shortName %in% .SEARCHNET_UNSAFE_RATE_EFFECTS
  if (any(bad))
    stop(structure(
      class = c("searchnet_unsupported_effect", "error", "condition"),
      list(message = sprintf(paste0(
        "search_rsiena(): the degree-dependent rate effect(s) %s cannot be ",
        "simulated on the state-carrying route. RSiena 1.5.0 corrupts memory ",
        "when it simulates them unconditionally for a bipartite network with ",
        "inPop or XWX (R then crashes). Drop the effect, or use RateX on an ",
        "actor covariate, which simulates correctly."),
        paste(unique(full$shortName[bad]), collapse = ", ")),
        call = NULL)))
  invisible(TRUE)
}


## ---------------------------------------------------------------------------
## The path itself
## ---------------------------------------------------------------------------
## Simulates every segment in order, carrying state, and checks the abort
## gate (S3) for each. Returns everything search_rsiena() needs to assemble
## the composite fit and the path.
.searchnet_simulate_path <- function(env, structure_model, theta_matrix,
                                     per_actor_total, run_seed,
                                     B_start, beh_start = NULL,
                                     segments, verbose = FALSE) {
  M <- env$M; N <- env$N
  n_rows <- nrow(theta_matrix)
  .searchnet_check_rate_effects(env)
  seeds <- .searchnet_segment_seeds(run_seed, nrow(segments))
  has_beh <- .searchnet_has_behavior(structure_model)
  beh_name <- if (has_beh) structure_model$dv_behavior$name else NULL
  if (has_beh && is.null(beh_name)) beh_name <- .SEARCHNET_BEHAVIOR_DV_NAME

  B <- B_start
  beh <- beh_start
  chains <- vector("list", nrow(segments))
  sims   <- vector("list", nrow(segments))
  ends   <- vector("list", nrow(segments))
  beh_ends <- vector("list", nrow(segments))
  starts <- vector("list", nrow(segments))
  theta_full_rows <- NULL
  n_ministeps <- integer(nrow(segments))
  fit <- NULL
  full_names <- NULL

  for (s in seq_len(nrow(segments))) {
    starts[[s]] <- B
    ## The data for segment 1 was built by the caller at B_start.
    if (s > 1L)
      .searchnet_prepare_period_data(env, structure_model, B, beh, verbose = verbose)
    f <- segments$n_rows[s] / n_rows
    th <- .searchnet_full_theta(env, theta_matrix[segments$theta_row[s], ],
                                bip_rate = f * per_actor_total)
    if (is.null(full_names)) full_names <- names(th)
    if (!identical(names(th), full_names))
      stop("searchnet: the RSiena effects table changed between segments; ",
           "the theta vector cannot be carried.", call. = FALSE)
    fit <- .searchnet_run_period(env, th, seeds[s], verbose = verbose)
    if (is.null(fit$chain) || is.null(fit$sims))
      stop("searchnet: siena07() returned no chain or no simulated networks.",
           call. = FALSE)
    run_chain <- fit$chain[[1L]]
    run_sims  <- fit$sims[[1L]]
    frame <- .searchnet_chain_frame(run_chain[[1L]][[1L]])
    B_end <- .searchnet_sims_bipartite(run_sims, M, N)
    ## Abort gate (S3, practice 4): the within-segment replay must end at
    ## RSiena's own end network. If it does not, the chain is not this
    ## period's path and nothing downstream can be trusted.
    B_rep <- .searchnet_path_replay_end(B, frame, N)
    if (any(B_rep != B_end))
      stop(sprintf(paste0(
        "searchnet: path terminus gate failed in segment %d: replaying RSiena's ",
        "chain from the segment's start state ends %d toggles away from ",
        "RSiena's end network. The simulation is not a path; aborting."),
        s, sum(B_rep != B_end)), call. = FALSE)
    ## Degree-bound gate: a penalized bound is soft in principle, so check it
    ## at every ministep of the segment, not only at its end.
    .searchnet_bounds_chain_gate(B, frame, .searchnet_model_bounds(structure_model),
                                 sprintf("segment %d", s))
    if (has_beh) beh <- .searchnet_sims_behavior(run_sims, beh_name)
    chains[[s]] <- run_chain
    sims[[s]]   <- run_sims
    ends[[s]]   <- B_end
    beh_ends[[s]] <- beh
    n_ministeps[s] <- nrow(frame)
    theta_full_rows <- rbind(theta_full_rows, th)
    B <- B_end
  }
  rownames(theta_full_rows) <- NULL
  segments$seed <- seeds
  segments$duration <- segments$n_rows / n_rows
  segments$basic_rate <- segments$duration * per_actor_total
  segments$n_ministeps <- n_ministeps
  list(fit = fit, chains = chains, sims = sims, starts = starts, ends = ends,
       beh_ends = beh_ends, segments = segments,
       theta_user = theta_matrix[segments$theta_row, , drop = FALSE],
       theta_full = theta_full_rows)
}

## The fit object search_rsiena() leaves on the environment: the last
## segment's sienaFit, carrying every segment's run 1 in order, so `$chain`,
## `$sims` and `$thetaUsed` have one entry per segment.
.searchnet_composite_fit <- function(res) {
  fit <- res$fit
  fit$chain <- res$chains
  fit$sims  <- res$sims
  tu <- res$theta_user
  rownames(tu) <- NULL
  fit$thetaUsed <- tu
  fit$thetaUsedFull <- res$theta_full
  fit$n3 <- length(res$chains)
  fit$searchnet_segments <- res$segments
  fit$searchnet_path <- .SEARCHNET_PATH_GENUINE
  fit
}


## ---------------------------------------------------------------------------
## Covariate centering (S9)
## ---------------------------------------------------------------------------
## RSiena centers a covariate on its mean unless it is created with
## centered = FALSE. Before 0.11.0 searchnet never passed `centered`, so every
## monadic covariate was silently centered (a 0/1 covariate became -0.5/+0.5).
## From 0.11.0 a monadic covariate is uncentered unless its declaration says
## `centered = TRUE`; dyadic covariates keep RSiena's default unless declared.
## The legacy route keeps the old behavior so archived numbers reproduce.
.searchnet_covariate_centered <- function(env, eff, monadic = TRUE) {
  if (!is.null(eff$centered)) return(isTRUE(eff$centered))
  if (!monadic) return(TRUE)
  legacy <- identical(tryCatch(env$searchnet_path_kind, error = function(e) NULL),
                      .SEARCHNET_PATH_LEGACY)
  if (legacy) return(TRUE)
  if (!isTRUE(.searchnet_deprecation_flags$covariate_centering)) {
    message(paste0(
      "searchnet: a covariate was declared without `centered`; it is used ",
      "uncentered (centered = FALSE), the default since 0.11.0. Before 0.11.0 ",
      "RSiena centered it on its mean. Add `centered = TRUE` to the ",
      "declaration to restore that. (Shown once per session.)"))
    .searchnet_deprecation_flags$covariate_centering <- TRUE
  }
  FALSE
}

## The centering RSiena actually applied, read from the data object.
.searchnet_centering_table <- function(rsiena_data) {
  out <- list()
  for (kind in c("cCovars", "vCovars", "dycCovars", "dyvCovars")) {
    lst <- rsiena_data[[kind]]
    if (!length(lst)) next
    for (nm in names(lst)) {
      a <- attributes(lst[[nm]])
      out[[length(out) + 1L]] <- data.frame(
        covariate = nm, kind = kind,
        centered  = isTRUE(a$centered),
        mean      = if (is.null(a$mean)) NA_real_ else as.numeric(a$mean)[1L],
        stringsAsFactors = FALSE)
    }
  }
  if (!length(out))
    return(data.frame(covariate = character(0), kind = character(0),
                      centered = logical(0), mean = numeric(0)))
  do.call(rbind, out)
}


## ---------------------------------------------------------------------------
## Arguments that no longer do anything
## ---------------------------------------------------------------------------
## Warns once per session per argument when a caller passes a non-default
## value, so old scripts keep running and are told why the value is ignored.
.searchnet_ignored_arg <- function(name, supplied, default, why) {
  if (isTRUE(all.equal(supplied, default))) return(invisible(FALSE))
  flag <- paste0("ignored_arg_", name)
  if (!isTRUE(.searchnet_deprecation_flags[[flag]])) {
    warning(sprintf(paste0("`%s` is deprecated and ignored since searchnet ",
                           "0.11.0: %s (Shown once per session.)"), name, why),
            call. = FALSE)
    assign(flag, TRUE, envir = .searchnet_deprecation_flags)
  }
  invisible(TRUE)
}


## The multiwave methods name their arguments `iterations` and `rand_seed`;
## search_rsiena() names the same quantities `iterations_per_actor` (times M)
## and `run_seed`. Callers written against search_rsiena() failed with
## "unused arguments" (2026-10-07), so the multiwave methods accept both
## namings and resolve them here. Supplying both names of one argument stops.
.searchnet_multiwave_resolve <- function(env, iterations, iterations_given,
                                         iterations_per_actor,
                                         rand_seed, rand_seed_given, run_seed) {
  if (!is.null(iterations_per_actor)) {
    if (iterations_given)
      stop("Supply `iterations` or `iterations_per_actor`, not both.", call. = FALSE)
    iterations <- as.numeric(iterations_per_actor) * env$M
  }
  if (!is.null(run_seed)) {
    if (rand_seed_given)
      stop("Supply `rand_seed` or `run_seed`, not both.", call. = FALSE)
    rand_seed <- run_seed
  }
  list(iterations = iterations, rand_seed = rand_seed)
}


## ---------------------------------------------------------------------------
## Multiwave route (S8)
## ---------------------------------------------------------------------------
## Appends `waves` waves to the environment. Wave w is one unconditional
## period from the end of wave w - 1 (the first from the current state) with
## basic rate `iterations / M`. Each wave keeps its own chain, replayed from
## that wave's start (stored on the wave's fit as `$searchnet_start`).
.searchnet_multiwave_append <- function(env, structure_model, waves, iterations,
                                        rand_seed, verbose = FALSE) {
  if (env$M < 2)
    stop("multiwave simulation requires M >= 2 actors.", call. = FALSE)
  waves <- as.integer(waves)
  if (waves < 1L) stop("`waves` must be at least 1.", call. = FALSE)
  if (is.null(env$bipartite_matrix))
    stop("bipartite_matrix is not set on this environment.", call. = FALSE)
  env$searchnet_path_kind <- .SEARCHNET_PATH_GENUINE
  env$config_structure_model <- structure_model
  input_effs <- env$get_input_from_structure_model(structure_model)
  per_actor <- as.numeric(iterations) / env$M

  has_beh <- .searchnet_has_behavior(structure_model)
  decl_beh <- if (has_beh) structure_model$dv_behavior$values else NULL
  beh <- NULL
  if (has_beh) {
    beh <- as.numeric(if (is.matrix(decl_beh)) decl_beh[, 1L] else decl_beh)
    if (length(env$rsiena_model_waves) && !is.null(env$behavior_state) &&
        length(env$behavior_state) == length(beh))
      beh <- as.numeric(env$behavior_state)
  }

  B <- matrix(as.numeric(env$bipartite_matrix), env$M, env$N)
  B <- .searchnet_bounds_start(B, .searchnet_model_bounds(structure_model),
                               rand_seed, "multiwave simulation")
  .searchnet_prepare_period_data(env, structure_model, B, beh, verbose = verbose)
  env$covariate_centering <- .searchnet_centering_table(env$rsiena_data)
  theta_row <- env$get_theta_matrix(input_effs, 1L, verbose = verbose)
  seg <- data.frame(segment_id = 1L, row_start = 1L, row_end = 1L,
                    n_rows = 1L, theta_row = 1L)
  seeds <- .searchnet_segment_seeds(rand_seed, waves)
  w0 <- length(env$rsiena_model_waves)
  fit_w <- NULL
  for (w in seq_len(waves)) {
    if (w > 1L)
      .searchnet_prepare_period_data(env, structure_model, B, beh, verbose = verbose)
    res <- .searchnet_simulate_path(env, structure_model, theta_row, per_actor,
                                    run_seed = seeds[w], B_start = B,
                                    beh_start = beh, segments = seg,
                                    verbose = verbose)
    fit_w <- .searchnet_composite_fit(res)
    fit_w$searchnet_start <- B
    fit_w$searchnet_wave_seed <- seeds[w]
    env$rsiena_model_waves[[w0 + w]] <- fit_w
    B <- res$ends[[1L]]
    if (has_beh) beh <- res$beh_ends[[1L]]
    env$bipartite_matrix_waves[[w0 + w]] <- B
  }
  env$rsiena_model <- fit_w
  env$path_segments <- NULL
  env$path_start_matrix <- env$rsiena_model_waves[[w0 + 1L]]$searchnet_start
  env$set_system_from_bipartite_matrix(B)
  env$behavior_state <- beh
  if (!is.null(decl_beh)) env$behavior_values <- decl_beh
  invisible(env)
}
