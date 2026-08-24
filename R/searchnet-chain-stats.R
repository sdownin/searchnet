# ---------------------------------------------------------------------------- #
#  searchnet-chain-stats.R
#
#  Event-level statistics on a simulated ministep chain AND on an observed
#  event log, computed in one comparable frame.
#
#  WHY THIS EXISTS
#  ---------------
#  A relational event model (REM) observes the event sequence and treats the
#  network as implied. A stochastic actor-oriented model observes the network at
#  panel waves and treats the event sequence as a latent object -- the ministep
#  chain -- that the estimator never sees. The two are inverses of one another.
#
#  Where BOTH are observable (a software repository gives a commit log AND a
#  repository state at every release tag; an email corpus gives messages AND a
#  membership roster), a fitted SAOM can be asked a question it is not usually
#  asked: does the chain it implies reproduce event-level regularities that were
#  never targeted by the estimator? Method of moments matches the wave-to-wave
#  target statistics by construction, so a goodness-of-fit test on those is close
#  to circular. Event-ordering statistics are not in that set.
#
#  WHAT THE COMPARISON CAN AND CANNOT CLAIM
#  ----------------------------------------
#  The simulated chain is a DRAW, not a reconstruction of what happened, and no
#  aggregation of draws makes it one. Every claim licensed here is therefore
#  DISTRIBUTIONAL: whether the distribution of a statistic over simulated chains
#  covers the value computed on the observed log. `searchnet_chain_compare()`
#  enforces this by refusing to run on a single simulated chain.
#
#  A correction worth stating plainly, because an earlier version of this file
#  got it wrong. Under method-of-moments these chains are NOT conditioned on the
#  observed endpoints. Each phase-3 run simulates FORWARD from the observed wave
#  at the start of the period and does not arrive at the period's end state:
#  replaying one verified chain from wave 1 left 21 cells differing from the
#  observed wave 2 on a 12 x 8 panel. Endpoint conditioning is a property of
#  likelihood-based augmented chains, not of these.
#
#  This makes the comparison STRONGER, not weaker. Only the starting state is
#  pinned, so an event-ordering statistic computed on the chain is a genuine
#  forward prediction of the fitted process rather than an artefact of being
#  steered toward a known endpoint. What remains true is that the fitted targets
#  match wave-to-wave change in expectation, so degree-driven statistics are
#  still nearer the estimator's reach than `focusing` is.
#
#  The four statistics are those of Tonellato, Tasselli, Conaldi, Lerner and Lomi
#  (2024), "A Microstructural Approach to Self-Organizing: The Emergence of
#  Attention Networks", Organization Science 35(2): 496-524, who estimate them
#  directly as REM effects on an Apache bug repository. Their attention
#  clustering statistic is defined there as the number of bipartite four-cycles,
#  which is the `cycle4` statistic RSiena already offers on a two-mode dependent
#  variable -- so the same quantity can be an estimated effect in one framework
#  and an emergent property in the other. That is the point of the comparison.
#
#  Computing them here does NOT put them in the model. Adding `cycle4` to a
#  specification and then reporting that the chain exhibits four-cycles is
#  circular in exactly the way this file exists to avoid. See the `fitted_effects`
#  argument of `searchnet_chain_compare()`, which refuses that combination.
# ---------------------------------------------------------------------------- #


## Column names of the returned frame, fixed so simulated and observed output
## are byte-comparable and can be rbind()ed without alignment logic.
.SEARCHNET_CHAIN_STAT_COLS <- c(
  "event_id", "source", "chain_id", "actor", "component",
  "tie_before", "change",
  "focusing", "reinforcing", "activity", "mixing", "clustering",
  "actor_degree", "component_degree"
)

## The four statistics of theoretical interest, named once.
.SEARCHNET_REM_STATS <- c("focusing", "reinforcing", "mixing", "clustering")

## How each statistic scales in the number of events, used by the per-event
## normalisation in searchnet_chain_compare(). Measured, not assumed: doubling a
## chain's length roughly doubles `activity` and `reinforcing` and quadruples
## `mixing`, which is their product. `clustering` is listed at 1 as an
## approximation -- its growth depends on the density trajectory and is not a
## clean power -- and that is why the length-ratio warning exists.
.SEARCHNET_STAT_SCALING <- list(
  focusing = 1, reinforcing = 1, activity = 1, mixing = 2, clustering = 1
)

## The one dependent variable this file understands. Declared here rather than
## reused from saomnk-api.R's `.DV_NAME` so that the positive filter in
## searchnet_chain_stats() does not depend on cross-file load order.
.SEARCHNET_BIPARTITE_DV_NAME <- "self$bipartite_rsienaDV"


#' Three-Paths From an Actor to a Component in a Bipartite Network
#'
#' Counts the paths \eqn{i \to m' \to i' \to m} with \eqn{i' \neq i} and
#' \eqn{m' \neq m}.  Closing such a path with the tie \eqn{(i, m)} completes a
#' bipartite four-cycle, so this is exactly the number of four-cycles the event
#' would create, and it is the attention-clustering statistic of Tonellato et
#' al. (2024).
#'
#' \strong{It is NOT the `cycle4` change statistic; it is twice it.}  RSiena
#' defines the actor-level four-cycle statistic with a leading one-quarter (see
#' the RSiena manual's definition of \code{snet_i11}), which makes the
#' network-level target equal the raw four-cycle count and the actor's own
#' statistic half the cycles running through that actor.  The change statistic
#' is therefore half of what this function returns.  The factor is constant, so
#' it cancels in any comparison of like with like, but the two must not be
#' described as the same quantity.
#'
#' \strong{Binary input only.}  The identity \code{co[i] == d_i} used by the
#' degenerate-term correction holds only for a 0/1 matrix.  Callers are
#' responsible for validation; \code{.searchnet_replay()} does it.
#'
#' Evaluated for one \eqn{(i, m)} pair at a time in \eqn{O(MN)}, by way of row
#' \eqn{i} of the co-membership matrix \eqn{BB'}.  Forming \eqn{BB'B} in full
#' would be \eqn{O(MN\min(M,N))} per event and is unnecessary.  Note the total
#' cost is still \eqn{O(EMN)} over a chain of \eqn{E} events, which is the
#' binding constraint at repository scale; maintaining \eqn{BB'} by rank-one
#' update would reduce it and has not been done.
#'
#' @param B Bipartite incidence matrix, actors in rows, components in columns.
#'   Must be binary.
#' @param i Actor (row) index.
#' @param m Component (column) index.
#' @return A single non-negative number.
#' @keywords internal
#' @noRd
.searchnet_three_paths <- function(B, i, m) {
  ## co[i'] = |N(i) intersect N(i')|, the shared-component count.
  ## `B %*% B[i, ]` rather than `B[i, ] %*% t(B)`: the latter allocates and
  ## coerces a full M x N transpose on every event.
  co <- as.vector(B %*% B[i, ])
  tot <- sum(co * B[, m])
  ## Subtract the degenerate terms. Both vanish when B[i, m] == 0, which is the
  ## case for every tie-creation event; they matter only for deletions, where
  ## the statistic describes the cycles the tie currently participates in.
  b_im <- B[i, m]
  if (b_im != 0) {
    d_i <- co[i]           ## = actor i's degree
    d_m <- sum(B[, m])     ## = component m's degree
    tot <- tot - b_im * d_i - b_im * d_m + b_im
  }
  tot
}


#' Replay an Event Sequence and Compute Event-Level Statistics
#'
#' @param events Two-column integer matrix or data.frame of (actor, component),
#'   in the order the events occurred.
#' @param B0 Bipartite incidence matrix giving the state BEFORE the first event.
#' @param source Character label carried into the `source` column.
#' @param chain_id Integer label carried into the `chain_id` column.
#' @return A data.frame with the columns in `.SEARCHNET_CHAIN_STAT_COLS`.
#' @keywords internal
#' @noRd
.searchnet_replay <- function(events, B0, source = "observed", chain_id = 1L) {

  events <- as.matrix(events)
  storage.mode(events) <- "integer"
  n_ev <- nrow(events)
  B <- B0
  if (!is.matrix(B))
    stop("`B0` must be a matrix.", call. = FALSE)
  if (anyNA(B))
    stop("`B0` contains NA; the replayed state would be undefined.",
         call. = FALSE)
  ## The degenerate-term correction in .searchnet_three_paths() rests on
  ## co[i] == d_i, which holds only for a 0/1 matrix. A weighted or count-valued
  ## incidence matrix returns wrong four-cycle counts with no error, so refuse it.
  if (!all(B %in% c(0, 1)))
    stop("`B0` must be binary (0/1). The four-cycle count is defined on a ",
         "binary incidence matrix, and a weighted matrix would return wrong ",
         "values silently.", call. = FALSE)
  storage.mode(B) <- "integer"
  M <- nrow(B); N <- ncol(B)

  if (n_ev && (max(events[, 1]) > M || max(events[, 2]) > N ||
               min(events) < 1L))
    stop(sprintf(paste0("event indices out of range for a %d x %d incidence ",
                        "matrix; actors must be 1..%d and components 1..%d"),
                 M, N, M, N), call. = FALSE)

  ## Running counters. These are histories over the REPLAYED sequence, which is
  ## what makes the simulated and observed frames comparable: both start from a
  ## clean history at the same state B0.
  focus_ct <- matrix(0L, M, N)   ## prior events on the (i, m) pair
  act_ct   <- integer(M)         ## prior events by actor i  (cumulative attention)
  reinf_ct <- integer(N)         ## prior events on component m

  out <- data.frame(
    event_id         = seq_len(n_ev),
    source           = rep(source, n_ev),
    chain_id         = rep(as.integer(chain_id), n_ev),
    actor            = events[, 1],
    component        = events[, 2],
    tie_before       = integer(n_ev),
    change           = character(n_ev),
    focusing         = numeric(n_ev),
    reinforcing      = numeric(n_ev),
    activity         = numeric(n_ev),
    mixing           = numeric(n_ev),
    clustering       = numeric(n_ev),
    actor_degree     = numeric(n_ev),
    component_degree = numeric(n_ev),
    stringsAsFactors = FALSE
  )

  for (e in seq_len(n_ev)) {
    i <- events[e, 1]; m <- events[e, 2]

    ## Every statistic is evaluated on the state BEFORE the event, which is what
    ## a relational event model conditions on.
    out$tie_before[e]       <- B[i, m]
    out$focusing[e]         <- focus_ct[i, m]
    out$reinforcing[e]      <- reinf_ct[m]
    out$activity[e]         <- act_ct[i]
    out$mixing[e]           <- act_ct[i] * reinf_ct[m]
    out$clustering[e]       <- .searchnet_three_paths(B, i, m)
    out$actor_degree[e]     <- sum(B[i, ])
    out$component_degree[e] <- sum(B[, m])
    out$change[e]           <- if (B[i, m] == 0L) "create" else "delete"

    ## Advance the state and the histories.
    B[i, m] <- 1L - B[i, m]
    focus_ct[i, m] <- focus_ct[i, m] + 1L
    act_ct[i]      <- act_ct[i] + 1L
    reinf_ct[m]    <- reinf_ct[m] + 1L
  }

  ## Order the columns FIRST, then stamp the attribute. `[.data.frame` with a
  ## column index drops non-standard attributes, so stamping before subsetting
  ## silently discards `B_final` and every caller receives NULL.
  out <- out[, .SEARCHNET_CHAIN_STAT_COLS]
  attr(out, "B_final") <- B
  out
}


#' Event-Level Statistics for a Simulated Chain or an Observed Event Log
#'
#' Computes the four attention micro-mechanism statistics of Tonellato et al.
#' (2024) -- focusing, reinforcing, mixing and clustering -- for every
#' tie-change event, evaluated on the network state immediately before that
#' event.  Accepts either a simulated \pkg{searchnet} environment (whose latent
#' ministep chain it reads) or an observed event log, and returns the same
#' columns for both so that the two are directly comparable.
#'
#' @param x Either a \code{SaomNkRSienaBiEnv} that has been run (so that
#'   \code{$chain_stats} exists), or a data.frame / matrix event log whose first
#'   two columns are actor and component indices in event order.
#' @param B0 Bipartite incidence matrix giving the state before the first event.
#'   Required when \code{x} is an event log; taken from
#'   \code{x$bipartite_matrix_init} when \code{x} is an environment.
#' @param actor,component Column names or positions identifying the actor and
#'   component columns of an event-log \code{x}.  Ignored for environments.
#' @param source Character label written to the \code{source} column.  Defaults
#'   to \code{"simulated"} for an environment and \code{"observed"} for a log.
#' @param chain_id Integer label written to the \code{chain_id} column.  Use it
#'   to keep replications apart when binding several simulated chains.
#'
#' @return A data.frame with one row per tie-change event and the columns
#'   \code{event_id}, \code{source}, \code{chain_id}, \code{actor},
#'   \code{component}, \code{tie_before}, \code{change}, \code{focusing},
#'   \code{reinforcing}, \code{activity}, \code{mixing}, \code{clustering},
#'   \code{actor_degree}, \code{component_degree}.
#'
#' @details
#' \strong{What the simulated chain is.}  RSiena's ministep chain is a latent
#' sequence.  Under method-of-moments each phase-3 run simulates \emph{forward}
#' from the observed wave at the start of the period and is not conditioned on
#' the period's end state -- see \code{\link{searchnet_chain_from_fit}}, which is
#' the supported way to get one chain per run.  It is a draw, not a
#' reconstruction, and repeated draws differ.  Statistics computed on one chain
#' are therefore a sample of size one; see
#' \code{\link{searchnet_chain_compare}}, which requires several.
#'
#' \strong{Behaviour ministeps are excluded.}  A behaviour ministep changes an
#' actor attribute rather than a tie, and its \code{id_to} column carries a
#' behaviour value rather than a component id, so including it would corrupt the
#' replayed state.
#'
#' @references
#' Tonellato, M., Tasselli, S., Conaldi, G., Lerner, J. and Lomi, A. (2024).
#' A Microstructural Approach to Self-Organizing: The Emergence of Attention
#' Networks. \emph{Organization Science} 35(2): 496-524.
#'
#' @seealso \code{\link{searchnet_chain_compare}},
#'   \code{\link{searchnet_repertoire}}
#' @export
searchnet_chain_stats <- function(x,
                                  B0        = NULL,
                                  actor     = 1,
                                  component = 2,
                                  source    = NULL,
                                  chain_id  = 1L) {

  ## ---- environment path ---------------------------------------------------
  if (inherits(x, "SaomNkRSienaBiEnv")) {

    if (is.null(x$chain_stats))
      stop("No ministep chain on this environment. Run the search first ",
           "(`search_rsiena()` / `saomnk_run()`), which sets `$chain_stats`.",
           call. = FALSE)

    ## GUARD: `$chain_stats` is not necessarily one SAOM path.
    ##
    ## search_rsiena_process_ministep_chain() hard-codes `period <- 1`
    ## (saomnk-class.R:5263) and concatenates EVERY phase-3 run into one frame
    ## (:5268-5271), then replays the lot cumulatively from
    ## `bipartite_matrix_init` (:5324-5344). RSiena does not carry state across
    ## phase-3 runs -- each re-draws from the observed wave-1 state -- so the
    ## concatenation is a sequence of INDEPENDENT draws replayed as though it
    ## were sequential.
    ##
    ## Demonstrated: initialise with exactly one tie at density = -8. A genuine
    ## path deletes that tie once and never recreates it; the composite frame
    ## toggles the same dyad repeatedly and ends holding the tie, which the
    ## model forbids. The inflation lands hardest on `focusing`, the repeat-pair
    ## counter -- which is precisely the statistic with the strongest claim to
    ## being free of what the estimator targeted.
    ##
    ## Refuse rather than return numbers that look fine and are not.
    n_runs <- tryCatch(length(x$rsiena_model$chain), error = function(e) NA_integer_)
    if (!is.na(n_runs) && n_runs > 1L)
      stop(sprintf(paste0(
        "this environment's `$chain_stats` concatenates %d phase-3 runs into ",
        "one frame and replays them cumulatively from the initial matrix. ",
        "RSiena restarts each run from the observed wave-1 state, so the ",
        "result is not a single ministep path and its event-ordering ",
        "statistics are artefacts of the concatenation -- `focusing` most of ",
        "all. Extract one run at a time from `$rsiena_model$chain[[run]]` and ",
        "pass each as its own `chain_id`. See the file header."), n_runs),
        call. = FALSE)

    cs <- x$chain_stats
    ## Realised BIPARTITE tie changes only, selected by a POSITIVE filter.
    ##
    ## An earlier version excluded the behaviour DV by name and kept everything
    ## else. That is a blacklist, and the package also supports one-mode
    ## dependent variables on the actor set (`self$social_rsienaDV`) and on the
    ## component set (`self$search_rsienaDV`); see saomnk-class.R:444,447. Their
    ## ministeps pass `tie_change`, so they were replayed as bipartite
    ## (actor, component) toggles with the wrong index semantics -- silently
    ## wrong except when an index happened to exceed the matrix bound. Name the
    ## one DV this function understands instead.
    keep <- !cs$stability &
      cs$tie_change &
      cs$dv_varname == .SEARCHNET_BIPARTITE_DV_NAME

    n_other <- sum(!cs$stability & cs$tie_change &
                     cs$dv_varname != .SEARCHNET_BIPARTITE_DV_NAME)
    if (n_other)
      message(sprintf(paste0("searchnet_chain_stats(): ignoring %d realised ",
                             "ministep(s) on other dependent variables; only ",
                             "the bipartite DV is replayed."), n_other))

    if (!any(keep))
      stop("The chain contains no realised bipartite tie changes; there is ",
           "nothing to compute event statistics on.", call. = FALSE)

    events <- cbind(as.integer(cs$id_from[keep]), as.integer(cs$id_to[keep]))
    if (is.null(B0)) B0 <- x$bipartite_matrix_init
    if (is.null(B0))
      stop("`x$bipartite_matrix_init` is NULL; supply `B0` explicitly.",
           call. = FALSE)
    if (is.null(source)) source <- "simulated"

    return(.searchnet_replay(events, B0, source = source, chain_id = chain_id))
  }

  ## ---- event-log path -----------------------------------------------------
  if (is.null(B0))
    stop("`B0` is required for an event log: the statistics are evaluated on ",
         "the network state before each event, so the starting state must be ",
         "given. Use a matrix of zeros if the panel starts empty.",
         call. = FALSE)

  if (!(is.data.frame(x) || is.matrix(x)))
    stop("`x` must be a `SaomNkRSienaBiEnv`, a data.frame, or a matrix.",
         call. = FALSE)

  ev <- as.data.frame(x, stringsAsFactors = FALSE)
  .pick <- function(sel, what) {
    if (is.character(sel)) {
      if (!sel %in% names(ev))
        stop(sprintf("column '%s' (the %s column) not found in `x`", sel, what),
             call. = FALSE)
      ev[[sel]]
    } else {
      if (sel > ncol(ev))
        stop(sprintf("column %d (the %s column) is beyond the %d columns of `x`",
                     sel, what, ncol(ev)), call. = FALSE)
      ev[[sel]]
    }
  }
  a <- .pick(actor, "actor")
  c_ <- .pick(component, "component")

  ## Character ids are common in real logs (usernames, file paths). Map them to
  ## the dimnames of B0 so the caller keeps control of the index order rather
  ## than inheriting whatever order the log happened to arrive in.
  if (is.character(a) || is.factor(a)) {
    if (is.null(rownames(B0)))
      stop("`x` has character actor ids but `B0` has no rownames to map them ",
           "onto. Give `B0` rownames, or pass integer indices.", call. = FALSE)
    a <- match(as.character(a), rownames(B0))
    if (anyNA(a)) stop("some actor ids in `x` are absent from rownames(B0).",
                       call. = FALSE)
  }
  if (is.character(c_) || is.factor(c_)) {
    if (is.null(colnames(B0)))
      stop("`x` has character component ids but `B0` has no colnames to map ",
           "them onto. Give `B0` colnames, or pass integer indices.",
           call. = FALSE)
    c_ <- match(as.character(c_), colnames(B0))
    if (anyNA(c_)) stop("some component ids in `x` are absent from colnames(B0).",
                        call. = FALSE)
  }

  if (is.null(source)) source <- "observed"
  .searchnet_replay(cbind(as.integer(a), as.integer(c_)), B0,
                    source = source, chain_id = chain_id)
}


#' Extract Per-Run Ministep Chains From a Fitted SAOM
#'
#' Pulls the latent ministep chains out of a \code{sienaFit} estimated with
#' \code{returnChains = TRUE} and returns them in the same frame
#' \code{\link{searchnet_chain_stats}} produces, one chain per (phase-3 run,
#' period) pair.  This is the supported route from an \emph{empirical} fit to the
#' many chains \code{\link{searchnet_chain_compare}} requires.
#'
#' @param fit A \code{sienaFit} from \code{siena07(..., returnChains = TRUE)}.
#' @param dat The \code{siena} data object the fit was estimated on.  Needed for
#'   the observed network at the start of each period, which is where each
#'   chain begins.
#' @param dv_name Name of the bipartite dependent variable.  Defaults to the
#'   only bipartite dependent variable when there is exactly one.
#' @param periods Integer vector of periods to extract.  Default all.
#' @param runs Integer vector of phase-3 runs to extract.  Default all;
#'   supply a subset when the full set is more than you need.
#' @param group Data group (default 1).  Note this is the second subscript of
#'   \code{fit$chain} and it indexes the GROUP, not the dependent variable --
#'   all dependent variables share one list per group and are told apart by the
#'   name each ministep carries.
#'
#' @return A data.frame with the columns of \code{\link{searchnet_chain_stats}}
#'   plus \code{run} and \code{period}.  \code{chain_id} is unique per
#'   (run, period), because the event-history counters restart at each period's
#'   observed wave and pooling two periods under one \code{chain_id} would
#'   concatenate incomparable histories.
#'
#' @section What a phase-3 chain is, and is not:
#' Under method-of-moments estimation each phase-3 run \strong{simulates forward
#' from the observed wave at the start of the period}.  It is \emph{not}
#' conditioned on the period's end state and does not arrive there: replaying one
#' verified chain from wave 1 left 21 cells differing from the observed wave 2 on
#' a 12 x 8 panel.
#'
#' That is good news for a generative-sufficiency argument rather than bad.  Only
#' the starting state is pinned, so every event-ordering statistic computed on
#' the chain is a genuine forward prediction of the fitted process, not an
#' artefact of being steered toward a known endpoint.  Do not describe these
#' chains as endpoint-conditioned; that is true of likelihood-based augmented
#' chains, not of these.
#'
#' @section Why not the environment path:
#' \code{$chain_stats} on a \code{SaomNkRSienaBiEnv} concatenates every phase-3
#' run into one frame and replays them cumulatively from the initial matrix,
#' which is not a single path -- see the guard in
#' \code{\link{searchnet_chain_stats}}.  Use this function instead.
#'
#' @seealso \code{\link{searchnet_chain_stats}},
#'   \code{\link{searchnet_chain_compare}}
#' @export
searchnet_chain_from_fit <- function(fit, dat, dv_name = NULL,
                                     periods = NULL, runs = NULL,
                                     group = 1L) {

  if (is.null(fit$chain))
    stop("this fit carries no chains. Re-estimate with ",
         "`siena07(..., returnChains = TRUE)`.", call. = FALSE)
  if (is.null(dat$depvars))
    stop("`dat` does not look like a siena data object: no `$depvars`.",
         call. = FALSE)

  ## ---- identify the dependent variable ------------------------------------
  dv_types <- vapply(dat$depvars, function(d) {
    ty <- attr(d, "type"); if (is.null(ty)) NA_character_ else as.character(ty)
  }, character(1))

  if (is.null(dv_name)) {
    bip <- names(dat$depvars)[!is.na(dv_types) & dv_types == "bipartite"]
    if (length(bip) != 1L)
      stop(sprintf(paste0("`dv_name` is required: the data object carries %d ",
                          "bipartite dependent variable(s) (%s). Name the one ",
                          "to extract."), length(bip),
                   if (length(bip)) paste(bip, collapse = ", ") else "none"),
           call. = FALSE)
    dv_name <- bip
  }
  if (!dv_name %in% names(dat$depvars))
    stop(sprintf("no dependent variable named '%s'. Available: %s",
                 dv_name, paste(names(dat$depvars), collapse = ", ")),
         call. = FALSE)
  if (!identical(unname(dv_types[dv_name]), "bipartite"))
    stop(sprintf(paste0("dependent variable '%s' has type '%s'. This replay ",
                        "assumes a bipartite network -- for a one-mode network ",
                        "'no change' is encoded as id_from == id_to rather than ",
                        "id_to == N, so the events would be read wrongly. This ",
                        "is a scope limit, not a statement about the model."),
                 dv_name, unname(dv_types[dv_name])), call. = FALSE)

  arr <- dat$depvars[[dv_name]]
  if (length(dim(arr)) != 3L)
    stop(sprintf("dependent variable '%s' is not an M x N x waves array.",
                 dv_name), call. = FALSE)
  N <- dim(arr)[2]; n_waves <- dim(arr)[3]

  n_runs_all <- length(fit$chain)
  if (is.null(runs)) runs <- seq_len(n_runs_all)
  runs <- runs[runs >= 1L & runs <= n_runs_all]
  if (!length(runs))
    stop("no valid runs selected; the fit carries ", n_runs_all, ".",
         call. = FALSE)

  ## The SECOND subscript of `fit$chain` is the data GROUP, not the dependent
  ## variable. Every dependent variable's ministeps are interleaved in one list
  ## per group and are separated by the name carried at declared position 3 --
  ## which is why the `nm == dv_name` filter below exists at all.
  ##
  ## Indexing it by the dependent variable's position was a real defect and it
  ## broke precisely the case this function is wanted for: `sienaDataCreate()`
  ## requires one-mode dependent variables to be declared BEFORE bipartite ones,
  ## so in any coevolution model the bipartite DV sits at position 2 and the
  ## read failed with "subscript out of bounds". It went unnoticed because in
  ## the single-DV case the DV index and the group index are both 1.
  if (group < 1L || group > length(fit$chain[[runs[1]]]))
    stop(sprintf(paste0("`group` = %d but this fit carries %d data group(s). ",
                        "Note this subscript is the GROUP, not the dependent ",
                        "variable -- all dependent variables share one list ",
                        "per group and are separated by name."),
                 group, length(fit$chain[[runs[1]]])), call. = FALSE)

  n_per_all <- length(fit$chain[[runs[1]]][[group]])
  if (is.null(periods)) periods <- seq_len(n_per_all)
  periods <- periods[periods >= 1L & periods <= min(n_per_all, n_waves - 1L)]
  if (!length(periods))
    stop("no valid periods selected; the fit carries ", n_per_all, ".",
         call. = FALSE)

  ## ---- replay each (run, period) ------------------------------------------
  ## Fields are read by DECLARED index, never from unlist(). A ministep declares
  ## 13 elements of which 10 and 11 are zero-length, so unlisting silently
  ## shifts everything after position 9 by two.
  ##  [[3]] dv name   [[4]] ego (0-indexed)   [[5]] alter (0-indexed)
  ##  [[12]] diagonal [[13]] stability
  out <- list(); finals <- list(); cid <- 0L

  for (p in periods) {
    B0 <- arr[, , p]
    storage.mode(B0) <- "integer"
    for (r in runs) {
      ms <- fit$chain[[r]][[group]][[p]]
      if (!length(ms)) next

      nm   <- vapply(ms, function(x) as.character(x[[3]]), character(1))
      ego  <- vapply(ms, function(x) as.integer(x[[4]]),   integer(1))
      alt  <- vapply(ms, function(x) as.integer(x[[5]]),   integer(1))
      stab <- vapply(ms, function(x) as.logical(x[[13]]),  logical(1))

      ## Realised changes on THIS dependent variable only. For a bipartite DV
      ## RSiena encodes "the actor declined to change anything" as alter == N
      ## on the 0-indexed scale, which is one past the last component.
      keep <- nm == dv_name & !stab & alt != N
      if (!any(keep)) next

      cid <- cid + 1L
      d <- .searchnet_replay(cbind(ego[keep] + 1L, alt[keep] + 1L), B0,
                             source = "simulated", chain_id = cid)
      finals[[length(finals) + 1L]] <- attr(d, "B_final")
      d$run <- as.integer(r)
      d$period <- as.integer(p)
      out[[length(out) + 1L]] <- d
    }
  }

  if (!length(out))
    stop(sprintf(paste0("no realised tie changes found for '%s' in the ",
                        "selected runs and periods. Every ministep was a ",
                        "no-change, which is a statement about the fitted ",
                        "rate, not a failure here."), dv_name), call. = FALSE)

  res <- do.call(rbind, out)
  rownames(res) <- NULL
  attr(res, "B_final_by_chain") <- finals
  attr(res, "dv_name") <- dv_name
  res
}


#' Compare Simulated Chains Against an Observed Event Log
#'
#' Tests whether the distribution of event-level statistics across simulated
#' ministep chains covers the values computed on an observed event log.  This is
#' a generative-sufficiency check: the statistics compared are properties of the
#' event ORDERING, which method-of-moments estimation of a panel SAOM does not
#' target, so agreement is evidence the fitted process captures something it was
#' not told about.
#'
#' @param simulated A data.frame from \code{\link{searchnet_chain_stats}}
#'   covering \strong{several} chains, distinguished by \code{chain_id}, or a
#'   list of such data.frames.
#' @param observed A data.frame from \code{\link{searchnet_chain_stats}} on the
#'   observed event log.
#' @param stats Character vector of statistics to compare.  Defaults to the four
#'   attention micro-mechanisms.
#' @param fun Summary applied to each statistic within a chain.  Default
#'   \code{mean}.
#' @param fitted_effects Optional character vector of RSiena effect shortNames
#'   included in the model that produced \code{simulated}.  Supplying it turns on
#'   a circularity guard: comparing a statistic the model was fitted on is not a
#'   free prediction, and the corresponding rows are flagged.
#' @param normalize Logical (default \code{TRUE}); divide each chain's summary
#'   by that chain's event count before comparing.  See the note below -- with
#'   \code{FALSE} the comparison is confounded by chain length.
#'
#' @return A data.frame with one row per statistic and columns \code{statistic},
#'   \code{observed}, \code{sim_mean}, \code{sim_sd}, \code{sim_lo},
#'   \code{sim_hi} (the 2.5th and 97.5th percentiles across chains),
#'   \code{covered}, \code{p_mc} (Monte Carlo p-value), \code{z},
#'   \code{n_chains}, \code{n_ev_obs}, \code{n_ev_sim}, \code{normalized} and
#'   \code{fitted}.
#'
#' @details
#' \strong{These statistics scale with chain length, and chain length is not a
#' mechanism.}  All four are running counters, so a chain's mean grows with the
#' number of events in it: doubling the events roughly doubles \code{activity}
#' and quadruples \code{mixing}.  A SAOM chain's length is the rate parameter
#' times the number of actors, and cancellation means a simulated chain
#' generally carries more ministeps than the observed log carries net changes.
#' Comparing raw means would therefore be substantially a test of whether the
#' event counts match, which is not the hypothesis.  \code{normalize = TRUE}
#' divides by each chain's event count; \code{FALSE} warns when the counts
#' differ by more than ten per cent.
#'
#' \strong{Report \code{p_mc}, not \code{covered}.}  The percentile interval is
#' badly anti-conservative at small \code{n_chains} -- at two chains a nominal
#' 95 per cent interval covers a true null only about a third of the time -- so
#' \code{covered} is reported for description only.  \code{p_mc} follows the
#' \code{sienaGOF} convention, \code{(1 + #\{dev_sim >= dev_obs\}) / (n + 1)},
#' and is bounded below by \code{1/(n+1)}; a warning fires below twenty chains.
#'
#' \strong{The circularity guard, and how far "not fitted" reaches.}
#' \code{cycle4} is the clustering statistic up to a factor of two, and
#' \code{inPop} is monotone in reinforcing.  A model carrying those has been
#' fitted toward the quantity being tested.  Beyond the named effects, note that
#' method of moments matches the target statistics \emph{in expectation}, so the
#' degree structure is close to what the estimator was aimed at -- and
#' \code{clustering}, \code{reinforcing} and the degrees sit nearer its reach
#' than \code{focusing} does.  (This paragraph previously said the chain is
#' "conditioned on both observed endpoints".  It is not: under method of moments
#' each phase-3 run simulates forward from the period's starting wave and does
#' not arrive at its end state.  See the file header and
#' \code{\link{searchnet_chain_from_fit}}.  The correction weakens this caution
#' rather than strengthening it, and it is the sentence a referee would quote
#' back, so it must not drift again.)
#' \strong{\code{focusing} is the only one of the four that is unambiguously a
#' free statement about event ordering}, and a claim of generative sufficiency is
#' safest stated on it.  \code{mixing} is \code{activity * reinforcing} by
#' construction and is not independent evidence.
#'
#' @seealso \code{\link{searchnet_chain_stats}}
#' @export
searchnet_chain_compare <- function(simulated,
                                    observed,
                                    stats          = .SEARCHNET_REM_STATS,
                                    fun            = mean,
                                    fitted_effects = NULL,
                                    normalize      = TRUE) {

  if (is.list(simulated) && !is.data.frame(simulated)) {
    simulated <- do.call(rbind, Map(function(d, k) {
      d$chain_id <- as.integer(k); d
    }, simulated, seq_along(simulated)))
  }
  stopifnot(is.data.frame(simulated), is.data.frame(observed))

  missing_cols <- setdiff(stats, names(simulated))
  if (length(missing_cols))
    stop("`simulated` lacks the column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  missing_cols <- setdiff(stats, names(observed))
  if (length(missing_cols))
    stop("`observed` lacks the column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)

  chains <- unique(simulated$chain_id)
  n_chains <- length(chains)
  if (n_chains < 2L)
    stop(sprintf(paste0("only %d simulated chain supplied. The ministep chain ",
                        "is a DRAW from a distribution over sequences ",
                        "consistent with the observed endpoints, so a single ",
                        "chain is a sample of size one and licenses no ",
                        "comparison. Simulate several and bind them with ",
                        "distinct `chain_id`s."), n_chains), call. = FALSE)
  if (n_chains < 20L)
    warning(sprintf(paste0("%d simulated chains. The Monte Carlo p-value has ",
                           "granularity 1/(n+1) = %.3f, so it cannot fall ",
                           "below that however extreme the observation. Twenty ",
                           "or more chains is the usable floor; fifty is ",
                           "better."), n_chains, 1 / (n_chains + 1)),
            call. = FALSE)

  ## Effects that make a statistic non-free, i.e. targeted by estimation.
  ##
  ## Verified against getEffects() on a two-mode DV: `cycle4`, `inPop`,
  ## `inPopSqrt`, `outAct`, `outActSqrt`, `inPopX` and `outActX` all exist for
  ## bipartite dependent variables. `cycle4ND` does not.
  ##
  ## CORRECTED 2026-08-23. A first pass listed `inPopX` and `outActX` as
  ## unavailable and dropped them, on a check run against a data object with NO
  ## COVARIATES -- and a covariate-weighted effect naturally does not appear
  ## when there is no covariate to weight by. With `coCovar`s present both are
  ## offered. They are covariate-weighted popularity and activity, so they
  ## target the reinforcing and activity statistics exactly as their unweighted
  ## forms do, and leaving them out meant a model carrying them was NOT flagged.
  ##
  ## `mixing` is activity x reinforcing by construction, so it is targeted
  ## whenever EITHER component is.
  .reinf <- c("inPop", "inPopSqrt", "inPopX")
  .act   <- c("outAct", "outActSqrt", "outActX")
  circular_map <- list(
    clustering  = c("cycle4"),
    reinforcing = .reinf,
    activity    = .act,
    mixing      = c(.reinf, .act)
  )

  ## Event counts. Every statistic here is a running counter, so its mean over a
  ## chain grows with the chain's length: doubling the events doubles `activity`
  ## and quadruples `mixing`. Chain length is itself rate x duration, so a raw
  ## comparison of means is substantially a test of whether the simulated chain
  ## has the same event count as the log -- which is not the hypothesis.
  n_ev_sim <- vapply(chains, function(k) sum(simulated$chain_id == k),
                     numeric(1))
  n_ev_obs <- nrow(observed)

  if (!normalize) {
    ratio <- mean(n_ev_sim) / max(n_ev_obs, 1)
    if (is.finite(ratio) && (ratio > 1.1 || ratio < 0.9))
      warning(sprintf(paste0("normalize = FALSE and the simulated chains ",
                             "average %.0f events against %d observed (ratio ",
                             "%.2f). These statistics scale with chain length, ",
                             "so this comparison partly tests event counts ",
                             "rather than event ordering."),
                      mean(n_ev_sim), n_ev_obs, ratio), call. = FALSE)
  }

  rows <- lapply(stats, function(s) {
    ## Per-statistic scaling exponent. These are running counters, but they do
    ## NOT all grow at the same rate in the number of events n:
    ##   focusing, reinforcing, activity  ~ n
    ##   mixing = activity * reinforcing  ~ n^2
    ## Dividing everything by n therefore leaves `mixing` still scaling with n,
    ## which an earlier version of this function did. Normalising it by n^2 is
    ## the same as taking the product of the two separately normalised parts.
    ##
    ## `clustering` is the honest exception: its growth depends on the density
    ## trajectory and is not a clean power of n, so n^1 is an approximation.
    ## Where chain and log lengths differ materially, match event counts rather
    ## than trusting normalisation to absorb it -- hence the ratio warning.
    pw <- .SEARCHNET_STAT_SCALING[[s]]
    if (is.null(pw)) pw <- 1
    per_chain <- vapply(seq_along(chains), function(j) {
      v <- as.numeric(fun(simulated[[s]][simulated$chain_id == chains[j]]))
      if (normalize) v / (max(n_ev_sim[j], 1)^pw) else v
    }, numeric(1))
    obs <- as.numeric(fun(observed[[s]]))
    if (normalize) obs <- obs / (max(n_ev_obs, 1)^pw)

    qs  <- stats::quantile(per_chain, c(0.025, 0.975), na.rm = TRUE,
                           names = FALSE)
    mu  <- mean(per_chain, na.rm = TRUE)
    sdv <- stats::sd(per_chain, na.rm = TRUE)

    ## Monte Carlo p-value in the style of sienaGOF: how extreme is the observed
    ## deviation from the simulated centre, against the simulated chains' own
    ## deviations. The percentile interval below is reported alongside but is
    ## badly anti-conservative at small n -- at n = 2 a nominal 95 per cent
    ## interval covers a true null only about a third of the time -- so `p_mc`
    ## is the quantity to report, not `covered`.
    dev_sim <- abs(per_chain - mu)
    dev_obs <- abs(obs - mu)
    p_mc <- (1 + sum(dev_sim >= dev_obs, na.rm = TRUE)) / (n_chains + 1)

    fitted_flag <- !is.null(fitted_effects) &&
      any(fitted_effects %in% circular_map[[s]])
    data.frame(
      statistic = s,
      observed  = obs,
      sim_mean  = mu,
      sim_sd    = sdv,
      sim_lo    = qs[1],
      sim_hi    = qs[2],
      covered   = obs >= qs[1] && obs <= qs[2],
      p_mc      = p_mc,
      z         = if (is.finite(sdv) && sdv > 0) (obs - mu) / sdv else NA_real_,
      n_chains  = n_chains,
      n_ev_obs  = n_ev_obs,
      n_ev_sim  = mean(n_ev_sim),
      normalized = normalize,
      fitted    = fitted_flag,
      stringsAsFactors = FALSE
    )
  })

  res <- do.call(rbind, rows)
  rownames(res) <- NULL

  if (any(res$fitted))
    warning("Statistic(s) ",
            paste(res$statistic[res$fitted], collapse = ", "),
            " correspond to effects the model was fitted on (",
            paste(intersect(fitted_effects, unlist(circular_map)),
                  collapse = ", "),
            "). Reproducing a targeted statistic is not a free prediction and ",
            "must not be reported as generative sufficiency.", call. = FALSE)

  class(res) <- c("searchnet_chain_compare", "data.frame")
  res
}


#' Fit the Null Model for a Generative-Sufficiency Comparison
#'
#' Estimates a rate-and-density-only SAOM on the same data, with chains
#' returned.  This is the reference arm without which coverage by a focal model
#' says nothing: the evidential quantity is the \emph{gap} between a model that
#' reproduces the observed event-level regularities and one that does not.
#'
#' @param dat A \code{siena} data object.
#' @param algorithm Optional \code{sienaAlgorithm}.  One is created if absent,
#'   with \code{cond = FALSE} -- see the note below.
#' @param seed Integer seed used when creating the default algorithm.
#' @param n3 Phase-3 iterations for the default algorithm.  Each yields chains,
#'   so this sets how many null chains are available.
#' @param ... Passed to \code{siena07}.
#'
#' @return A \code{sienaFit} with \code{$chain} populated.
#'
#' @section Why unconditional estimation:
#' The default algorithm sets \code{cond = FALSE}.  Under conditional estimation
#' the rate parameters are derived rather than estimated by the method of
#' moments and come without usable standard errors, which matters for any
#' downstream statement about rates.  If you pass your own \code{algorithm},
#' the focal and null arms must share its settings or the comparison includes
#' differences you introduced.
#'
#' @section What counts as null here:
#' Every effect except the basic rate and outdegree/density is switched off.
#' The default \code{getEffects()} set for a two-mode dependent variable is
#' already rate-plus-density, so this is usually a no-op -- it is done
#' explicitly anyway so the arm does not silently inherit whatever a caller's
#' effects object happened to carry.
#'
#' @seealso \code{\link{searchnet_chain_gap}},
#'   \code{\link{searchnet_chain_from_fit}}
#' @export
searchnet_chain_null_model <- function(dat, algorithm = NULL, seed = 1L,
                                       n3 = 100L, ...) {
  if (!requireNamespace("RSiena", quietly = TRUE))
    stop("RSiena is required to fit the null model.", call. = FALSE)

  eff <- RSiena::getEffects(dat)
  keep <- eff$shortName %in% c("Rate", "density")
  dropped <- unique(eff$shortName[eff$include & !keep])
  eff$include[!keep] <- FALSE
  if (!any(eff$include))
    stop("switching off every non-rate, non-density effect left nothing to ",
         "estimate. Check the effects object for this data.", call. = FALSE)
  if (length(dropped))
    message("searchnet_chain_null_model(): null arm excludes ",
            paste(dropped, collapse = ", "), ".")

  if (is.null(algorithm))
    algorithm <- RSiena::sienaAlgorithmCreate(projname = NULL, seed = seed,
                                              n3 = n3, cond = FALSE)

  RSiena::siena07(algorithm, data = dat, effects = eff,
                  returnChains = TRUE, batch = TRUE, silent = TRUE, ...)
}


#' The Focal-Minus-Null Gap in Generative Sufficiency
#'
#' Runs \code{\link{searchnet_chain_compare}} for a focal model and for a
#' rate-and-density-only null against the same observed event log, and reports
#' per statistic whether the comparison is \emph{informative}.
#'
#' @param focal Chain-stats frame for the focal model, from
#'   \code{\link{searchnet_chain_from_fit}}.
#' @param null Chain-stats frame for the null model.
#' @param observed Chain-stats frame for the observed event log.
#' @param alpha Threshold applied to the Monte Carlo p-value (default 0.05).
#' @param ... Passed to \code{\link{searchnet_chain_compare}} for BOTH arms, so
#'   they are compared on identical terms.
#'
#' @return A data.frame, one row per statistic, with the focal and null
#'   \code{p_mc} and \code{z}, a \code{gap} column (\code{|z_null| - |z_focal|},
#'   positive when the focal model is closer to the observed value), and a
#'   \code{verdict}.
#'
#' @section Reading the verdict:
#' \describe{
#'   \item{\code{informative}}{the null fails and the focal model covers. This
#'     is the only cell that supports a generative-sufficiency claim.}
#'   \item{\code{undiscriminating}}{both cover. The statistic is reproduced by
#'     rate and density alone, so the focal model reproducing it is not
#'     evidence for the focal model. Report it; do not count it.}
#'   \item{\code{focal_fails}}{the null covers and the focal model does not --
#'     adding structure moved the chain away from the observed log.}
#'   \item{\code{both_fail}}{neither covers.}
#' }
#'
#' A paper should state the verdict for every statistic it computed, not only
#' the informative ones. An undiscriminating result is a real finding about what
#' the statistic can detect.
#'
#' @seealso \code{\link{searchnet_chain_null_model}},
#'   \code{\link{searchnet_chain_compare}}
#' @export
searchnet_chain_gap <- function(focal, null, observed, alpha = 0.05, ...) {

  f <- suppressWarnings(searchnet_chain_compare(focal, observed, ...))
  n <- suppressWarnings(searchnet_chain_compare(null,  observed, ...))

  if (!identical(f$statistic, n$statistic))
    stop("the focal and null comparisons cover different statistics; they ",
         "must be run on identical terms.", call. = FALSE)

  focal_covers <- f$p_mc >  alpha
  null_covers  <- n$p_mc >  alpha

  verdict <- ifelse(!null_covers &  focal_covers, "informative",
             ifelse( null_covers &  focal_covers, "undiscriminating",
             ifelse( null_covers & !focal_covers, "focal_fails",
                                                  "both_fail")))

  out <- data.frame(
    statistic   = f$statistic,
    observed    = f$observed,
    focal_mean  = f$sim_mean,
    null_mean   = n$sim_mean,
    p_mc_focal  = f$p_mc,
    p_mc_null   = n$p_mc,
    z_focal     = f$z,
    z_null      = n$z,
    gap         = abs(n$z) - abs(f$z),
    verdict     = verdict,
    fitted      = f$fitted,
    stringsAsFactors = FALSE
  )
  attr(out, "alpha") <- alpha
  class(out) <- c("searchnet_chain_gap", "data.frame")
  out
}


#' Calibrate the Generative-Sufficiency Test on Its Own Chains
#'
#' Leave-one-out size check.  Each chain is held out in turn, treated as though
#' it were the observed log, and compared against the remaining chains.  Because
#' every chain genuinely comes from the fitted process, the null hypothesis is
#' true by construction, so the Monte Carlo p-values should be approximately
#' uniform and the rejection rate at \code{alpha} should be approximately
#' \code{alpha}.
#'
#' Run this before reporting any coverage result.  It validates the machinery
#' and the per-event normalisation in one exercise, it needs no re-estimation,
#' and a test that over-rejects on its own data cannot support a claim about
#' someone else's.
#'
#' @param chains A chain-stats frame covering several chains, from
#'   \code{\link{searchnet_chain_from_fit}}.
#' @param alpha Nominal level (default 0.05).
#' @param stats Statistics to calibrate.  Defaults to the four.
#' @param max_holdout Optional cap on how many chains to hold out, for speed.
#' @param ... Passed to \code{\link{searchnet_chain_compare}}.
#'
#' @return A data.frame with one row per statistic: \code{statistic},
#'   \code{n_holdout}, \code{rejection_rate}, \code{nominal},
#'   \code{distinct_p}, \code{zero_frac}, \code{ks_p} and \code{ks_valid}.
#'
#' @section Interpreting it:
#' A rejection rate materially above \code{alpha} means the test is
#' anti-conservative on data the model actually generated, and any coverage
#' claim built on it is overstated.  Below \code{alpha} means it is
#' conservative and will miss real misfit.  Note the Monte Carlo p-value is
#' bounded below by \code{1/n}, so with few chains the rejection rate can be
#' exactly zero for arithmetic reasons rather than because the test is well
#' calibrated -- read \code{n_holdout} alongside the rate.
#'
#' @section Ties, and why \code{ks_p} is guarded:
#' The Kolmogorov-Smirnov test assumes a continuous reference.  A sparse
#' discrete statistic yields heavily tied p-values and KS then rejects because
#' of the ties, not because the test is miscalibrated.  \code{ks_p} is therefore
#' computed only when the held-out p-values are sufficiently distinct, and
#' \code{ks_valid} records whether it was; \code{zero_frac} reports how much of
#' the underlying statistic is exactly zero.
#'
#' This is not hypothetical.  On a synthetic 12 x 8 panel \code{focusing} was
#' zero for \strong{92.9 per cent} of events, giving 14 distinct p-values out of
#' 25 and \code{ks_p = 0.0015} while the rejection rate sat at a healthy 0.04.
#'
#' \strong{That carries a scope condition worth stating in any paper using
#' this.}  \code{focusing} is the statistic furthest from what method of moments
#' targets, and so the natural one to lead a sufficiency claim on -- but it is
#' also the one that goes degenerate on short or sparse chains, because it
#' counts repeat attention to the same pair and there is little of that to
#' count.  Check \code{zero_frac} before leaning on it.  A setting with genuine
#' repeat attention over long chains is where it has power; a short panel is
#' not.
#'
#' @seealso \code{\link{searchnet_chain_gap}}
#' @export
searchnet_chain_calibrate <- function(chains, alpha = 0.05,
                                      stats = .SEARCHNET_REM_STATS,
                                      max_holdout = NULL, ...) {

  ids <- unique(chains$chain_id)
  if (length(ids) < 4L)
    stop(sprintf(paste0("leave-one-out calibration needs at least 4 chains ",
                        "(got %d): each holdout is compared against the rest, ",
                        "and searchnet_chain_compare() itself refuses fewer ",
                        "than 2."), length(ids)), call. = FALSE)

  hold <- if (!is.null(max_holdout) && max_holdout < length(ids))
    ids[seq_len(max_holdout)] else ids

  pv <- lapply(hold, function(k) {
    obs_k <- chains[chains$chain_id == k, , drop = FALSE]
    rest  <- chains[chains$chain_id != k, , drop = FALSE]
    r <- suppressWarnings(
      searchnet_chain_compare(rest, obs_k, stats = stats, ...))
    stats::setNames(r$p_mc, r$statistic)
  })
  P <- do.call(rbind, pv)

  out <- do.call(rbind, lapply(stats, function(s) {
    p <- P[, s]
    p <- p[is.finite(p)]
    ## Fraction of the statistic's raw values that are exactly zero. A sparse
    ## discrete statistic produces heavily tied p-values, and a KS test against
    ## a CONTINUOUS uniform then rejects because of the ties rather than because
    ## the test is miscalibrated. Measured on a synthetic panel: `focusing` was
    ## zero for 92.9 per cent of events, giving 14 distinct p-values out of 25
    ## and ks_p = 0.0015 while the rejection rate sat at a healthy 0.04.
    zero_frac <- if (s %in% names(chains)) mean(chains[[s]] == 0, na.rm = TRUE)
                 else NA_real_
    distinct  <- length(unique(p))
    ## Require the p-values to be MOSTLY distinct. A half-distinct threshold is
    ## too loose: `focusing` came through at 14 of 25 and KS still rejected on
    ## ties alone. Eighty per cent separates the tied case from the merely
    ## granular one on the fixtures tested.
    ks_ok <- length(p) >= 5L && distinct >= ceiling(0.8 * length(p))
    data.frame(
      statistic      = s,
      n_holdout      = length(p),
      rejection_rate = if (length(p)) mean(p <= alpha) else NA_real_,
      nominal        = alpha,
      distinct_p     = distinct,
      zero_frac      = round(zero_frac, 3),
      ks_p           = if (ks_ok)
        suppressWarnings(stats::ks.test(p, "punif")$p.value) else NA_real_,
      ks_valid       = ks_ok,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  attr(out, "p_values") <- P
  class(out) <- c("searchnet_chain_calibrate", "data.frame")
  out
}


#' @export
print.searchnet_chain_gap <- function(x, ...) {
  cat("\nGenerative sufficiency: focal model against a rate+density null\n")
  cat(sprintf("alpha = %s. Only `informative` rows support a sufficiency claim.\n\n",
              format(attr(x, "alpha"))))
  df <- as.data.frame(x)
  for (nm in c("observed", "focal_mean", "null_mean"))
    df[[nm]] <- signif(df[[nm]], 4)
  for (nm in c("p_mc_focal", "p_mc_null")) df[[nm]] <- round(df[[nm]], 4)
  for (nm in c("z_focal", "z_null", "gap")) df[[nm]] <- round(df[[nm]], 3)
  print(df[, c("statistic", "observed", "p_mc_null", "p_mc_focal", "gap",
               "verdict", "fitted")], row.names = FALSE)
  nu <- sum(x$verdict == "undiscriminating")
  if (nu)
    cat(sprintf(paste0("\n%d statistic(s) are reproduced by rate and density ",
                       "alone. Report them; do not count them as evidence.\n"),
                nu))
  cat("\n")
  invisible(x)
}


#' @export
print.searchnet_chain_calibrate <- function(x, ...) {
  cat("\nLeave-one-out calibration of the sufficiency test\n")
  cat("Each chain held out as a pseudo-observed log; the null is TRUE by\n")
  cat("construction, so rejection should sit near nominal.\n\n")
  df <- as.data.frame(x)
  df$reject <- round(df$rejection_rate, 4)
  df$ks_p <- ifelse(is.na(df$ks_p), "  n/a", format(round(df$ks_p, 4)))
  print(df[, c("statistic", "n_holdout", "reject", "nominal",
               "distinct_p", "zero_frac", "ks_p", "ks_valid")],
        row.names = FALSE)
  cat("\np_mc is bounded below by 1/n, so a zero rate with few chains is\n")
  cat("arithmetic, not calibration. Read n_holdout alongside it.\n")
  if (any(!x$ks_valid))
    cat("ks_p is withheld where the held-out p-values are too tied for a KS\n",
        "test against a continuous uniform to mean anything; see zero_frac.\n",
        sep = "")
  if (any(x$zero_frac > 0.8, na.rm = TRUE))
    cat("WARNING: a statistic is >80% zeros and has little power here.\n")
  cat("\n")
  invisible(x)
}


#' @export
print.searchnet_chain_compare <- function(x, ...) {
  cat("\nEvent-level comparison: simulated ministep chains vs observed log\n")
  cat(sprintf("Chains: %d.  Observed events: %d.  Simulated events (mean): %.0f.\n",
              x$n_chains[1], x$n_ev_obs[1], x$n_ev_sim[1]))
  cat(sprintf("Per-event normalisation: %s.\n",
              if (isTRUE(x$normalized[1])) "ON" else "OFF"))
  df <- as.data.frame(x)
  for (nm in c("observed", "sim_mean", "sim_sd", "sim_lo", "sim_hi"))
    df[[nm]] <- signif(df[[nm]], 4)
  df$z    <- round(df$z, 3)
  df$p_mc <- round(df$p_mc, 4)
  cat("\n")
  print(df[, c("statistic", "observed", "sim_mean", "sim_lo", "sim_hi",
               "covered", "p_mc", "z", "fitted")], row.names = FALSE)
  cat("\nReport p_mc, not covered: the percentile interval is anti-conservative\n")
  cat("at small n. p_mc cannot fall below 1/(n+1) =",
      format(round(1 / (x$n_chains[1] + 1), 4)), "\n")
  cat("Of the four, `focusing` is the one furthest from what method-of-moments\n")
  cat("targets; the others are closer to the degree structure it matches.\n")
  if (any(x$fitted))
    cat("\nNOTE: rows marked fitted=TRUE were targeted by estimation and are ",
        "not free predictions.\n", sep = "")
  cat("\n")
  invisible(x)
}
