###############################################################################
## searchnet-composition.R
##
## Actors who enter or leave an observed panel, handled the RSiena way: as an
## exogenous composition change (Huisman and Snijders 2003, Sociological
## Methods & Research 32, 253-287). An actor is active only during the time
## intervals in which it is present; outside them it cannot change ties, and
## its absence is not modeled. Without this, a firm that closes looks like an
## actor that dropped every tie, and a firm that enters looks like an actor
## that created every tie from an empty portfolio. Both distort the estimated
## rate and every effect read off tie changes.
##
## Conventions, verified against RSiena 1.5.0 and 1.6.6:
##   * Times are in RSiena wave units: wave 1 is time 1, wave W is time W.
##     An interval c(a, b) is closed: the actor gave valid data at every wave
##     w with a <= w <= b. c(1, 2.5) left halfway through period 2 (between
##     waves 2 and 3), so wave 3 is absent; c(1.4, 3) joined in period 1, so
##     wave 1 is absent.
##   * Absent actor-waves are coded NA in the dependent array. RSiena's
##     sienaDataCreate() fills those cells according to the composition
##     option (option 1: 0 before entry, last value carried forward after
##     leaving); cells that are not NA are kept as given.
##   * A composition change applies to the FIRST node set of a bipartite
##     network (the actors) only. RSiena errors for the second node set, so
##     components entering or leaving are not supported here.
###############################################################################


# --------------------------------------------------------------------------- #
#  internal helpers
# --------------------------------------------------------------------------- #

## Dates and date-times as numbers, so they interpolate like numeric periods.
.comp_num <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(as.numeric(x))
  x
}

## One time per actor, aligned to the actor universe, from a column name of
## `data`, a two-column data frame (actor id, time) or a named vector.
## NA means "no entry" (present from the start) or "no exit" (present to the
## end).
.comp_actor_times <- function(spec, data, actor, act_u, arg) {
  if (is.null(spec)) return(NULL)
  ids <- NULL; tms <- NULL
  if (is.character(spec) && length(spec) == 1L && is.null(names(spec))) {
    if (!spec %in% names(data))
      stop("`", arg, "` = '", spec, "' is not a column of `data`. Give a ",
           "column name, a data frame (actor, time) or a named vector.",
           call. = FALSE)
    ids <- as.character(data[[actor]]); tms <- data[[spec]]
    tm_chr <- as.character(tms)
    per_actor <- tapply(tm_chr, ids, function(v) length(unique(v)))
    if (any(per_actor > 1L))
      stop("`", arg, "` column '", spec, "' must be constant within an actor; ",
           "it varies for: ",
           paste(utils::head(names(per_actor)[per_actor > 1L], 5), collapse = ", "),
           ". One ", arg, " time per actor (use `presence` for re-entry).",
           call. = FALSE)
    keep <- !duplicated(ids)
    ids <- ids[keep]; tms <- tms[keep]
  } else if (is.data.frame(spec)) {
    if (ncol(spec) != 2L)
      stop("`", arg, "` as a data frame must have two columns: the actor id, ",
           "then the time.", call. = FALSE)
    ids <- as.character(spec[[1L]]); tms <- spec[[2L]]
  } else if (is.atomic(spec) && !is.null(names(spec))) {
    ids <- names(spec); tms <- unname(spec)
  } else {
    stop("`", arg, "` must be a column name of `data`, a two-column data ",
         "frame (actor, time) or a named vector.", call. = FALSE)
  }
  if (anyNA(ids))
    stop("`", arg, "` has an NA actor id.", call. = FALSE)
  if (anyDuplicated(ids))
    stop("`", arg, "` gives more than one time for actor(s): ",
         paste(utils::head(unique(ids[duplicated(ids)]), 5), collapse = ", "),
         ". Use `presence` for an actor that leaves and returns.", call. = FALSE)
  unknown <- setdiff(ids, act_u)
  if (length(unknown))
    stop("`", arg, "` names actor(s) not in the actor universe: ",
         paste(utils::head(unknown, 5), collapse = ", "), ".", call. = FALSE)
  tms[match(act_u, ids)]
}

## Times in period units to RSiena wave time (1..W). Numeric (or Date)
## periods interpolate linearly, so 2003.5 between periods 2003 and 2004 is
## halfway through that period. Non-numeric periods accept period labels only.
.comp_wave_time <- function(t, periods, arg) {
  W <- length(periods)
  out <- rep(NA_real_, length(t))
  attr(out, "below") <- attr(out, "above") <- logical(length(t))
  ok <- !is.na(t)
  if (!any(ok)) return(out)
  p <- .comp_num(periods)
  if (is.numeric(p)) {
    tt <- .comp_num(t[ok])
    if (is.character(tt) || is.factor(tt))
      tt <- suppressWarnings(as.numeric(as.character(tt)))
    if (!is.numeric(tt) || anyNA(tt))
      stop("`", arg, "` times must be numbers in the units of the period ",
           "column.", call. = FALSE)
    if (is.unsorted(p, strictly = TRUE))
      stop("`", arg, "` times need the periods in strictly increasing order.",
           call. = FALSE)
    wt <- stats::approx(p, seq_len(W), xout = pmin(pmax(tt, p[1L]), p[W]))$y
    below <- above <- logical(length(t))
    below[ok] <- tt < p[1L]; above[ok] <- tt > p[W]
    out[ok] <- wt
    attr(out, "below") <- below; attr(out, "above") <- above
    return(out)
  }
  lab <- as.character(periods)
  hit <- match(as.character(t[ok]), lab)
  if (anyNA(hit))
    stop("With non-numeric periods, `", arg, "` must give period labels; ",
         "not found: ", paste(utils::head(unique(as.character(t[ok])[is.na(hit)]), 5),
                              collapse = ", "),
         ". Use numeric periods for times within a period.", call. = FALSE)
  out[ok] <- hit
  attr(out, "below") <- attr(out, "above") <- logical(length(t))
  out
}

## Presence (actor x wave logical) from closed intervals in wave time.
.comp_presence_from_intervals <- function(intervals, W) {
  P <- matrix(FALSE, length(intervals), W)
  for (i in seq_along(intervals)) {
    x <- intervals[[i]]
    for (k in seq(1L, length(x), by = 2L)) {
      lo <- ceiling(x[k] - 1e-9); hi <- floor(x[k + 1L] + 1e-9)
      if (lo <= hi) P[i, lo:hi] <- TRUE
    }
  }
  P
}

## Closed intervals in wave time from presence: one interval per run of
## observed waves. Without timing information an actor is taken to enter
## halfway between its last unobserved wave and its first observed one, and to
## leave halfway between its last observed wave and the next.
.comp_intervals_from_presence <- function(P) {
  W <- ncol(P)
  lapply(seq_len(nrow(P)), function(i) {
    r <- rle(as.logical(P[i, ]))
    ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
    on <- which(r$values)
    lo <- starts[on]; hi <- ends[on]
    lo <- ifelse(lo > 1L, lo - 0.5, lo)
    hi <- ifelse(hi < W, hi + 0.5, hi)
    as.numeric(rbind(lo, hi))
  })
}

## Intervals as RSiena should receive them. An exit exactly at an
## intermediate wave b < W is moved just after it: RSiena 1.5.0 and 1.6.6
## drop a leave event at time 0 of a period, so an interval ending exactly at
## wave b keeps the actor active through the whole of period b (verified by
## simulation, 2026-10-11). The actor was observed at wave b either way.
.comp_rsiena_intervals <- function(intervals, W) {
  lapply(intervals, function(x) {
    k <- seq(2L, length(x), by = 2L)
    hit <- abs(x[k] - round(x[k])) < 1e-9 & x[k] < W - 1e-9
    x[k[hit]] <- x[k[hit]] + 1e-4
    x
  })
}

## Validate intervals in wave time; returns the presence matrix.
.comp_check_intervals <- function(intervals, W, ids) {
  if (W < 2L)
    stop("A composition change needs at least two waves.", call. = FALSE)
  for (i in seq_along(intervals)) {
    x <- intervals[[i]]
    if (!is.numeric(x) || !length(x) || length(x) %% 2L || anyNA(x))
      stop("Actor '", ids[i], "': each composition entry is an even-length ",
           "numeric vector of interval end points.", call. = FALSE)
    if (any(x < 1 - 1e-9 | x > W + 1e-9))
      stop("Actor '", ids[i], "': composition times must lie in [1, ", W,
           "] (wave time).", call. = FALSE)
    if (is.unsorted(x))
      stop("Actor '", ids[i], "': interval end points must be nondecreasing ",
           "(entry before exit).", call. = FALSE)
  }
  P <- .comp_presence_from_intervals(intervals, W)
  never <- which(rowSums(P) == 0L)
  if (length(never))
    stop("Actor(s) present at no observed wave: ",
         paste(utils::head(ids[never], 5), collapse = ", "),
         ". RSiena needs every actor in the node set to be observed at least ",
         "once; drop them from the actor universe.", call. = FALSE)
  thin <- which(colSums(P) < 2L)
  if (length(thin))
    stop("Fewer than two actors are present at wave(s) ",
         paste(thin, collapse = ", "), "; RSiena needs at least two active ",
         "actors in every period.", call. = FALSE)
  P
}

## The stored composition of an observed object, from entry/exit or presence.
.comp_build <- function(entry, exit, presence, data, actor, act_u, periods,
                        per_u) {
  W <- length(per_u)
  M <- length(act_u)
  if (!is.null(presence)) {
    if (!is.null(entry) || !is.null(exit))
      stop("Give either `entry`/`exit` or `presence`, not both.", call. = FALSE)
    P <- presence
    if (is.data.frame(P)) P <- as.matrix(P)
    if (!is.matrix(P) || !(is.logical(P) || is.numeric(P)))
      stop("`presence` must be a logical actor x period matrix.", call. = FALSE)
    if (anyNA(P)) stop("`presence` contains NA.", call. = FALSE)
    if (is.numeric(P)) {
      if (any(P != 0 & P != 1)) stop("`presence` must be logical (or 0/1).",
                                     call. = FALSE)
      P <- P == 1
    }
    rn <- rownames(P)
    if (is.null(rn)) {
      if (nrow(P) != M)
        stop("`presence` has ", nrow(P), " rows but there are ", M, " actors; ",
             "give rownames (actor ids) or one row per actor in universe order.",
             call. = FALSE)
    } else {
      miss <- setdiff(act_u, rn)
      if (length(miss))
        stop("`presence` lacks row(s) for actor(s): ",
             paste(utils::head(miss, 5), collapse = ", "), ".", call. = FALSE)
      P <- P[match(act_u, rn), , drop = FALSE]
    }
    cn <- colnames(P)
    if (is.null(cn)) {
      if (ncol(P) != W)
        stop("`presence` has ", ncol(P), " columns but there are ", W,
             " periods.", call. = FALSE)
    } else {
      miss <- setdiff(per_u, cn)
      if (length(miss))
        stop("`presence` lacks column(s) for period(s): ",
             paste(utils::head(miss, 5), collapse = ", "), ".", call. = FALSE)
      P <- P[, match(per_u, cn), drop = FALSE]
    }
    intervals <- .comp_intervals_from_presence(P)
    ent <- ext <- NULL
    source <- "presence"
  } else {
    e_raw <- .comp_actor_times(entry, data, actor, act_u, "entry")
    x_raw <- .comp_actor_times(exit,  data, actor, act_u, "exit")
    if (is.null(e_raw)) e_raw <- rep(NA, M)
    if (is.null(x_raw)) x_raw <- rep(NA, M)
    ent <- .comp_wave_time(e_raw, periods, "entry")
    ext <- .comp_wave_time(x_raw, periods, "exit")
    late <- which(attr(ent, "above"))
    if (length(late))
      stop("Actor(s) entering after the last period: ",
           paste(utils::head(act_u[late], 5), collapse = ", "),
           ". They are never observed; drop them from the actor universe.",
           call. = FALSE)
    early <- which(attr(ext, "below"))
    if (length(early))
      stop("Actor(s) leaving before the first period: ",
           paste(utils::head(act_u[early], 5), collapse = ", "),
           ". They are never observed; drop them from the actor universe.",
           call. = FALSE)
    ent <- as.numeric(ent); ext <- as.numeric(ext)
    ent[is.na(ent)] <- 1; ext[is.na(ext)] <- W
    bad <- which(ent > ext)
    if (length(bad))
      stop("Entry after exit for actor(s): ",
           paste(utils::head(act_u[bad], 5), collapse = ", "), ".", call. = FALSE)
    intervals <- lapply(seq_len(M), function(i) c(ent[i], ext[i]))
    names(ent) <- names(ext) <- act_u
    source <- "entry/exit"
  }
  names(intervals) <- act_u
  P <- .comp_check_intervals(intervals, W, act_u)
  dimnames(P) <- list(act_u, per_u)
  list(intervals = intervals, presence = P, entry = ent, exit = ext,
       source = source)
}


# --------------------------------------------------------------------------- #
#  searchnet_composition
# --------------------------------------------------------------------------- #

#' RSiena Composition Change for Actors Who Enter or Leave
#'
#' Returns the RSiena composition-change object
#' (\code{\link[RSiena]{sienaCompositionChange}}) for an observed panel whose
#' actors enter or leave, built from the intervals stored by
#' \code{\link{searchnet_bipartite_from_long}} (arguments \code{entry},
#' \code{exit} or \code{presence}). \code{\link{searchnet_coevolve_data}}
#' includes it automatically; call this function directly when assembling a
#' \code{sienaDataCreate()} call by hand.
#'
#' @section What composition change does:
#' Following Huisman and Snijders (2003), an actor is active only during the
#' time intervals in which it is present. Outside them it cannot create or
#' drop ties, and its arrival and departure are exogenous events, not part of
#' the modeled change. A closure is therefore not read as an actor dropping
#' every tie, and an entry is not read as an actor creating a portfolio from
#' nothing. With a composition change in the data, RSiena estimates by
#' unconditional method of moments, whatever the algorithm object says.
#'
#' Times are RSiena wave times: wave 1 is time 1 and wave \eqn{W} is time
#' \eqn{W}. An interval \code{c(a, b)} is closed, so the actor gave valid data
#' at every wave \eqn{w} with \eqn{a \le w \le b}. Absent actor-waves are coded
#' \code{NA} in the dependent array, and RSiena fills them according to
#' \code{option}. Only the actor node set can change composition: RSiena does
#' not support a composition change on the second node set of a bipartite
#' network.
#'
#' One adjustment is made on the way to RSiena. An interval that ends exactly
#' at an intermediate wave \eqn{b < W} is passed as ending just after it
#' (\eqn{b + 10^{-4}}). RSiena 1.5.0 and 1.6.6 drop a departure that falls at
#' time 0 of a period, so an interval ending exactly at wave \eqn{b} would keep
#' the actor active, and changing ties, through the whole of period \eqn{b}
#' (checked by simulation). The actor is observed at wave \eqn{b} either way.
#'
#' @param x A \code{"searchnet_bipartite"} object built with \code{entry},
#'   \code{exit} or \code{presence}; or a logical actor-by-wave presence
#'   matrix; or a list with one numeric vector of interval end points (wave
#'   time) per actor.
#' @param option RSiena's treatment of the tie variables of absent actors:
#'   \code{1} (default) zero before entry, last value carried forward after
#'   leaving and used in the method-of-moments statistics; \code{2} as 1 but
#'   the carried-forward values treated as missing; \code{3} missing whenever
#'   absent. See \code{\link[RSiena]{sienaCompositionChange}}.
#' @param nodeSet Name of the actor node set. Default \code{"ACTORS"}, the
#'   name \code{\link{searchnet_coevolve_data}} and
#'   \code{\link{searchnet_recovery}} use.
#'
#' @return An object of class \code{"compositionChange"}, ready for
#'   \code{\link[RSiena]{sienaDataCreate}}.
#'
#' @references Huisman, M. and Snijders, T. A. B. (2003). Statistical analysis
#'   of longitudinal network data with changing composition.
#'   \emph{Sociological Methods & Research}, 32(2), 253--287.
#' @seealso \code{\link{searchnet_bipartite_from_long}},
#'   \code{\link{searchnet_coevolve_data}}
#' @export
#' @examples
#' long <- data.frame(
#'   actor     = c("a1", "a2", "a3", "a1", "a2", "a1", "a2", "a4"),
#'   component = c("c1", "c2", "c1", "c2", "c2", "c1", "c1", "c2"),
#'   period    = c(1, 1, 1, 2, 2, 3, 3, 3)
#' )
#' ## a3 closes halfway through period 2; a4 enters during period 2
#' obs <- searchnet_bipartite_from_long(
#'   long, "actor", "component", "period",
#'   exit = c(a3 = 2.5), entry = c(a4 = 2.4))
#' obs$composition$presence
#' if (requireNamespace("RSiena", quietly = TRUE))
#'   searchnet_composition(obs)
searchnet_composition <- function(x, option = 1L, nodeSet = "ACTORS") {
  if (!requireNamespace("RSiena", quietly = TRUE))
    stop("RSiena is required for searchnet_composition().", call. = FALSE)
  if (!is.numeric(option) || length(option) != 1L || !option %in% 1:3)
    stop("`option` must be 1, 2 or 3 (RSiena's option 4 is not available).",
         call. = FALSE)
  if (!is.character(nodeSet) || length(nodeSet) != 1L || !nzchar(nodeSet))
    stop("`nodeSet` must be a single node-set name.", call. = FALSE)
  if (inherits(x, "searchnet_bipartite")) {
    if (is.null(x$composition))
      stop("This searchnet_bipartite object has no composition change: build ",
           "it with `entry`, `exit` or `presence` in ",
           "searchnet_bipartite_from_long().", call. = FALSE)
    intervals <- x$composition$intervals
    W <- ncol(x$composition$presence)
    ids <- names(intervals)
  } else if (is.matrix(x) && (is.logical(x) || is.numeric(x))) {
    if (anyNA(x)) stop("The presence matrix contains NA.", call. = FALSE)
    P <- x == 1 | x == TRUE
    intervals <- .comp_intervals_from_presence(P)
    W <- ncol(P)
    ids <- if (is.null(rownames(P))) as.character(seq_len(nrow(P))) else rownames(P)
  } else if (is.list(x)) {
    intervals <- lapply(x, as.numeric)
    W <- max(vapply(intervals, function(v) if (length(v)) max(v) else 1, numeric(1)))
    W <- max(2L, as.integer(ceiling(W - 1e-9)))
    ids <- if (is.null(names(x))) as.character(seq_along(x)) else names(x)
  } else {
    stop("`x` must be a searchnet_bipartite object with a composition, a ",
         "logical presence matrix, or a list of intervals.", call. = FALSE)
  }
  .comp_check_intervals(intervals, W, ids)
  RSiena::sienaCompositionChange(unname(.comp_rsiena_intervals(intervals, W)),
                                 nodeSet = nodeSet, option = as.integer(option))
}
