# ---------------------------------------------------------------------------- #
#  searchnet-repertoire.R
#
#  Actor-level behavioral repertoires from a ministep chain.
#
#  WHAT A REPERTOIRE IS HERE
#  -------------------------
#  A fitted SAOM reports one evaluation function for the whole population. Actor
#  heterogeneity, if it is modeled at all, enters through covariate effects the
#  analyst chose in advance. But the chain records what each actor ACTUALLY did
#  at every ministep, and actors who share one evaluation function still realize
#  very different sequences of moves, because they occupy different positions.
#
#  A repertoire is the profile of an actor's realized moves over the event-level
#  statistics in `searchnet_chain_stats()`: how far an actor's changes run along
#  familiarity (focusing), along popularity (reinforcing), along local closure
#  (clustering), and how much it creates versus deletes. Clustering those
#  profiles partitions actors into behavior types -- an emergent typology rather
#  than an assumed one.
#
#  WHY NOT sienaRI
#  ---------------
#  RSiena's `sienaRI()` decomposes the relative importance of EFFECTS in an
#  actor's choice probabilities, which is the natural first thought here. Two
#  reasons this file does not depend on it. First, its support for two-mode
#  dependent variables is not something to assume; a non-implementation says
#  nothing about the world, and building the construct on top of it would make
#  the construct unavailable exactly where searchnet is used. Second, effect
#  importance is a property of the MODEL evaluated at an actor's position, while
#  what is wanted here is a property of the actor's REALIZED behavior. Those are
#  different objects and the second is the one that answers "what did this actor
#  do".
#
#  `searchnet_repertoire_ri()` is provided as an optional bridge for callers who
#  do want the model-side decomposition, and it fails loudly rather than
#  silently if sienaRI cannot take the fit.
#
#  THE NULL IS NOT OPTIONAL
#  ------------------------
#  Any clustering procedure returns clusters. `searchnet_repertoire_null()`
#  re-runs the whole pipeline on actor labels permuted across events, which
#  breaks the association between actors and their move profiles while holding
#  the event set, the statistics and the clustering algorithm fixed. It changes
#  only the thing under suspicion. If the observed separation is not clearly
#  outside the permuted distribution, the repertoires are an artefact of k-means
#  and must be reported as such.
#
#  WHAT "ONLY THE THING UNDER SUSPICION" COSTS TO GET RIGHT
#  --------------------------------------------------------
#  A free permutation of actor labels across events does NOT change only the
#  thing under suspicion, and the reason is mechanical rather than statistical.
#  `focusing` counts an actor's repeats of its OWN prior pairs, so it grows with
#  the actor's own event count; `mixing` is activity x reinforcing and grows
#  faster still. A real actor with many events therefore has a high profile and
#  one with few events a low profile, for arithmetic reasons, in a population
#  with no behavioral types at all. Under a free permutation each pseudo-actor
#  receives a random mixture of events drawn from the whole pool, whose per-event
#  mean converges on the population mean whatever the pseudo-actor's own count,
#  so the permuted profiles collapse towards a point while the real ones stay
#  spread out by activity volume. The observed silhouette then beats the permuted
#  distribution, and the small p establishes only that actors differ in HOW MUCH
#  they do -- which nobody doubted -- rather than in WHAT they do.
#
#  Two routes out, both implemented, neither free:
#
#    * `strata = "n_events"` (default) permutes labels only WITHIN bins of
#      similar own-event count. Each actor keeps its own event count exactly, and
#      each pseudo-actor draws its events from actors of comparable volume, so
#      the volume-driven component of the profile survives into the null and
#      stops being evidence. The test becomes conditional: are actors of
#      comparable activity distinguishable by what they do?
#
#    * `method = "residualise"` removes the volume component from the FEATURES
#      instead, regressing each on log(n_events) across actors and clustering the
#      residuals, with a free permutation on top.
#
#  The stratified permutation is the recommended default. It leaves the reported
#  statistic exactly the one the observed run computed and changes only the
#  reference distribution, whereas residualisation changes the construct itself
#  -- the repertoires become "behavior net of a linear-in-log volume trend",
#  a different object, and one that inherits that functional form as an
#  assumption. Residualisation is offered because it attacks the same confound
#  from the other side and agreement between the two is worth more than either
#  alone.
#
#  A SECOND ASYMMETRY: WHO IS ALLOWED TO CHOOSE k
#  ----------------------------------------------
#  When k is chosen automatically, the observed silhouette is a MAXIMUM over
#  `k_range` while a permutation forced to the observed k is a single draw. A
#  maximum over several candidates beats a single draw on average even under the
#  null, so that comparison is anticonservative. `k_selection = "reselect"`
#  (default) lets every permutation re-select over the same `k_range`, making it
#  max against max. `k_selection = "fixed"` restores the older behavior, whose
#  motivation -- partitions of different granularity are not directly comparable
#  -- is also real. Both concerns are genuine; the resolution is that the
#  silhouette is scale-free and defined across k, so comparing maxima is
#  legitimate, while comparing a maximum with a non-maximum is not. Pass `k`
#  explicitly whenever theory supplies it and the question does not arise.
# ---------------------------------------------------------------------------- #


## Default features. Deliberately the four attention micro-mechanism statistics
## plus the create/delete balance -- not every column available -- so that the
## typology is interpretable against the mechanisms rather than being whatever
## a kitchen-sink feature set happens to separate.
.SEARCHNET_REPERTOIRE_FEATURES <- c(
  "focusing", "reinforcing", "mixing", "clustering", "create_share"
)


#' Average Silhouette Width
#'
#' Implemented here rather than taken from \pkg{cluster} to avoid adding a
#' dependency for one quantity.
#'
#' @param d A \code{dist} object.
#' @param cl Integer cluster assignments, in the same order as the rows of the
#'   data that produced \code{d}.
#' @return Mean silhouette width, or \code{NA_real_} when fewer than two
#'   non-singleton clusters exist.
#' @keywords internal
#' @noRd
.searchnet_silhouette <- function(d, cl) {
  D <- as.matrix(d)
  n <- length(cl)
  ks <- unique(cl)
  if (length(ks) < 2L || n < 3L) return(NA_real_)
  s <- vapply(seq_len(n), function(i) {
    own <- which(cl == cl[i] & seq_len(n) != i)
    if (!length(own)) return(0)                 ## singleton: contributes 0
    a <- mean(D[i, own])
    b <- min(vapply(setdiff(ks, cl[i]), function(k)
      mean(D[i, cl == k]), numeric(1)))
    if (max(a, b) == 0) 0 else (b - a) / max(a, b)
  }, numeric(1))
  mean(s)
}


#' Aggregate Event-Level Statistics to Actor Profiles
#'
#' @param chain_stats A data.frame from \code{\link{searchnet_chain_stats}}.
#' @param min_events Minimum events an actor must have to receive a profile.
#' @return A list with `profiles` (data.frame, one row per retained actor) and
#'   `dropped` (data.frame of actors below the threshold, with their counts).
#' @keywords internal
#' @noRd
.searchnet_actor_profiles <- function(chain_stats, min_events = 5L) {

  need <- c("actor", "change", "focusing", "reinforcing", "mixing",
            "clustering")
  miss <- setdiff(need, names(chain_stats))
  if (length(miss))
    stop("`chain_stats` lacks the column(s): ", paste(miss, collapse = ", "),
         ". Was it produced by searchnet_chain_stats()?", call. = FALSE)

  cnt <- table(chain_stats$actor)
  keep_actors <- as.integer(names(cnt)[cnt >= min_events])
  drop_actors <- as.integer(names(cnt)[cnt < min_events])

  if (!length(keep_actors))
    stop(sprintf(paste0("no actor has at least %d events (the busiest has %d). ",
                        "Lower `min_events`, or simulate a longer chain."),
                 min_events, max(cnt)), call. = FALSE)

  sub <- chain_stats[chain_stats$actor %in% keep_actors, , drop = FALSE]
  sp  <- split(sub, sub$actor)

  profiles <- do.call(rbind, lapply(sp, function(d) {
    data.frame(
      actor        = d$actor[1],
      n_events     = nrow(d),
      focusing     = mean(d$focusing),
      reinforcing  = mean(d$reinforcing),
      mixing       = mean(d$mixing),
      clustering   = mean(d$clustering),
      create_share = mean(d$change == "create"),
      stringsAsFactors = FALSE
    )
  }))
  rownames(profiles) <- NULL

  dropped <- if (length(drop_actors)) {
    data.frame(actor = drop_actors,
               n_events = as.integer(cnt[as.character(drop_actors)]),
               stringsAsFactors = FALSE)
  } else {
    data.frame(actor = integer(0), n_events = integer(0))
  }

  list(profiles = profiles, dropped = dropped)
}


#' Behavioral Repertoires From a Ministep Chain
#'
#' Partitions actors into behavior types from the profile of their realized
#' moves, using the event-level statistics returned by
#' \code{\link{searchnet_chain_stats}}.  A repertoire is an emergent typology of
#' what actors DO, as distinct from the single evaluation function a stochastic
#' actor-oriented model fits for the whole population.
#'
#' @param chain_stats A data.frame from \code{\link{searchnet_chain_stats}}, or
#'   a \code{SaomNkRSienaBiEnv} that has been run (in which case the statistics
#'   are computed for you).
#' @param k Number of repertoires.  If \code{NULL} (default), chosen over
#'   \code{k_range} by maximum average silhouette width, and the full criterion
#'   path is returned so the choice is auditable.
#' @param k_range Integer vector of candidate \code{k} values.  Ignored when
#'   \code{k} is given.
#' @param features Character vector of profile columns to cluster on.  Defaults
#'   to the four attention micro-mechanisms plus the create/delete balance.
#' @param min_events Minimum realized events an actor needs to be assigned a
#'   repertoire.  Actors below it are reported in \code{$dropped}, never
#'   silently discarded.
#' @param scale Logical; standardize features before clustering (default
#'   \code{TRUE}).  With raw scales, \code{mixing} -- a product of two counts --
#'   dominates every other feature.
#' @param residualise Logical; if \code{TRUE}, each feature is regressed on
#'   \code{log(n_events)} across actors and the RESIDUALS are clustered instead
#'   of the raw profile means (default \code{FALSE}).  Several of the features
#'   are mechanically increasing in an actor's own event count -- \code{focusing}
#'   counts repeats of the actor's own prior pairs, \code{mixing} is activity
#'   times popularity -- so without this the leading axis of the profile space
#'   can be activity volume rather than behavior.  Turning it on changes the
#'   CONSTRUCT: the repertoires become behavior net of a linear-in-log volume
#'   trend, and that functional form becomes an assumption.  \code{$profiles} are
#'   reported on the original, un-residualised scale either way, so they remain
#'   readable; only the geometry the clustering sees is changed.
#' @param nstart Passed to \code{\link[stats]{kmeans}}.
#' @param seed Optional integer seed, for a reproducible partition.
#'
#' @return An object of class \code{"searchnet_repertoire"}: a list with
#'   \code{assignment} (actor, n_events, repertoire), \code{profiles} (cluster
#'   means on the ORIGINAL scale, plus size), \code{k}, \code{criterion} (the
#'   silhouette path when \code{k} was chosen automatically), \code{silhouette}
#'   (at the chosen \code{k}), \code{dropped}, \code{features}, and
#'   \code{residualise}.
#'
#' @section The reported silhouette and the criterion path agree:
#' When \code{k} is chosen automatically, \code{$silhouette} is the value in the
#' \code{$criterion} row for the chosen \code{k}, because the partition returned
#' IS the criterion loop's fit at that \code{k} rather than an independent
#' re-fit.  Each candidate is seeded identically, so requesting a \code{k}
#' explicitly at the same \code{seed} reproduces the auto-selected partition
#' exactly.  An earlier version re-fitted after selection; at \code{nstart = 1}
#' the printed silhouette and the criterion row then disagreed for most seeds,
#' because they were two different k-means solutions at the same \code{k}.
#'
#' @section Interpretation, and its limits:
#' Repertoires are descriptive.  They are computed from a ministep chain, which
#' is a draw from a distribution over sequences consistent with the observed
#' panel endpoints, so an actor's repertoire is not a fixed attribute recovered
#' from data -- it is a property of one sampled history.  Re-run over several
#' chains and report the stability of the assignment before treating a
#' repertoire as an actor characteristic.  And run
#' \code{\link{searchnet_repertoire_null}}: k-means returns k clusters whether
#' or not there is structure.
#'
#' @seealso \code{\link{searchnet_chain_stats}},
#'   \code{\link{searchnet_repertoire_null}}
#' @export
searchnet_repertoire <- function(chain_stats,
                                 k           = NULL,
                                 k_range     = 2:6,
                                 features    = .SEARCHNET_REPERTOIRE_FEATURES,
                                 min_events  = 5L,
                                 scale       = TRUE,
                                 residualise = FALSE,
                                 nstart      = 25L,
                                 seed        = NULL) {

  if (inherits(chain_stats, "SaomNkRSienaBiEnv"))
    chain_stats <- searchnet_chain_stats(chain_stats)

  ap       <- .searchnet_actor_profiles(chain_stats, min_events = min_events)
  profiles <- ap$profiles

  ## Validate against the ADVERTISED set, not against every column. `actor` and
  ## `n_events` are identifiers, not behavior: validating with
  ## setdiff(features, names(profiles)) let `features = "actor"` through, and it
  ## ran, partitioning actors by the numeric value of their id.
  available <- setdiff(names(profiles), c("actor", "n_events"))
  miss <- setdiff(features, available)
  if (length(miss))
    stop("unknown feature(s): ", paste(miss, collapse = ", "),
         ". Available: ", paste(available, collapse = ", "),
         if (any(miss %in% c("actor", "n_events")))
           ". `actor` and `n_events` are identifiers, not behavior, and are "
         else NULL,
         if (any(miss %in% c("actor", "n_events")))
           "deliberately not clusterable." else NULL,
         call. = FALSE)

  X <- as.matrix(profiles[, features, drop = FALSE])

  ## Volume residualisation, BEFORE standardization: the thing being removed is
  ## a mechanical dependence on the actor's own event count, and it is present in
  ## the raw feature. Doing it after scaling would be the same linear operation
  ## up to a constant, but the diagnostic below (was there any volume variation
  ## to regress on?) belongs with the counts.
  if (isTRUE(residualise)) {
    lx <- log(profiles$n_events)
    if (length(lx) < 3L || stats::sd(lx) == 0) {
      warning("`residualise = TRUE` but log(n_events) has no variation across ",
              "actors (or there are fewer than 3 actors); features were left ",
              "unchanged. There is no volume confound to remove here.",
              call. = FALSE)
    } else {
      cn <- colnames(X)
      X  <- vapply(seq_len(ncol(X)),
                   function(j) as.numeric(stats::lm(X[, j] ~ lx)$residuals),
                   numeric(nrow(X)))
      X  <- matrix(X, nrow = nrow(profiles), dimnames = list(NULL, cn))
    }
  }

  if (scale) {
    X <- base::scale(X)
    ## A zero-variance feature yields NaN and silently poisons the distance
    ## matrix. Drop it, and say which.
    bad <- apply(X, 2, function(z) all(is.na(z) | is.nan(z)))
    if (any(bad)) {
      warning("feature(s) ", paste(features[bad], collapse = ", "),
              " have zero variance across actors and were dropped.",
              call. = FALSE)
      X <- X[, !bad, drop = FALSE]
      features <- features[!bad]
    }
    if (!ncol(X))
      stop("every requested feature has zero variance across actors; there is ",
           "nothing to cluster.", call. = FALSE)
  }

  n_actors <- nrow(X)
  if (!is.null(seed)) set.seed(seed)

  d         <- stats::dist(X)
  criterion <- NULL
  km        <- NULL
  sil_final <- NA_real_

  if (is.null(k)) {
    k_range <- k_range[k_range >= 2L & k_range < n_actors]
    if (!length(k_range))
      stop(sprintf(paste0("no candidate k is usable: %d actors have profiles, ",
                          "so k must lie in 2..%d."),
                   n_actors, max(2L, n_actors - 1L)), call. = FALSE)

    ## Seed EACH candidate identically rather than once before the loop. Two
    ## things follow, and both are contract. (i) The fit at candidate kk is
    ## bit-identical to the fit an explicit `k = kk` would produce at this seed,
    ## so auto-selection and pinning k agree. (ii) The chosen candidate's fit can
    ## be REUSED below instead of re-fitted, which is what makes `$silhouette`
    ## equal to the `$criterion` row for the chosen k. The previous code re-fitted
    ## after selection; at nstart = 1 the two quantities then disagreed for most
    ## seeds, and a reader of the printed object was looking at two different
    ## k-means solutions at the same k.
    fits <- lapply(k_range, function(kk) {
      if (!is.null(seed)) set.seed(seed)
      stats::kmeans(X, centers = kk, nstart = nstart)
    })
    sil <- vapply(fits, function(f) .searchnet_silhouette(d, f$cluster),
                  numeric(1))
    criterion <- data.frame(k = k_range, silhouette = sil,
                            stringsAsFactors = FALSE)
    if (all(is.na(sil)))
      stop("silhouette is undefined at every candidate k; cannot choose k ",
           "automatically. Pass `k` explicitly.", call. = FALSE)
    best      <- which.max(sil)
    k         <- k_range[best]
    km        <- fits[[best]]
    sil_final <- sil[best]
  }

  if (k >= n_actors)
    stop(sprintf("k = %d but only %d actors have profiles.", k, n_actors),
         call. = FALSE)

  if (is.null(km)) {
    if (!is.null(seed)) set.seed(seed)
    km        <- stats::kmeans(X, centers = k, nstart = nstart)
    sil_final <- .searchnet_silhouette(d, km$cluster)
  }

  assignment <- data.frame(
    actor      = profiles$actor,
    n_events   = profiles$n_events,
    repertoire = as.integer(km$cluster),
    stringsAsFactors = FALSE
  )

  ## Cluster means on the ORIGINAL scale. Standardized centers are unreadable,
  ## and a typology nobody can read is a typology nobody will check.
  orig <- profiles[, features, drop = FALSE]
  prof <- do.call(rbind, lapply(sort(unique(km$cluster)), function(kk) {
    idx <- km$cluster == kk
    out <- as.data.frame(t(colMeans(orig[idx, , drop = FALSE])))
    cbind(repertoire = as.integer(kk), size = sum(idx), out)
  }))
  rownames(prof) <- NULL

  structure(list(
    assignment = assignment,
    profiles   = prof,
    k          = as.integer(k),
    criterion  = criterion,
    silhouette = sil_final,
    dropped     = ap$dropped,
    features    = features,
    n_actors    = n_actors,
    residualise = isTRUE(residualise)
  ), class = "searchnet_repertoire")
}


#' Bin Actors by Own Event Count
#'
#' Rank-based bins of roughly equal SIZE, then merged so that two actors with the
#' same event count are never split across bins.  Both halves matter.  Equal-size
#' bins keep every stratum permutable, which pure value-quantile cuts do not when
#' the count distribution is lumpy.  The tie merge is what makes the strata mean
#' what they say: actors with identical counts are exactly the pair the null has
#' most reason to treat as exchangeable, and a rank cut falling between them
#' would freeze both.
#'
#' @param counts Named integer vector of per-actor event counts; names are the
#'   actor labels as characters.
#' @param n_strata Target number of bins.  Fewer are returned when ties force
#'   bins together; never more.
#' @return Integer vector of consecutive bin indices, named as \code{counts} is.
#' @keywords internal
#' @noRd
.searchnet_repertoire_strata <- function(counts, n_strata = 10L) {
  n_strata <- max(1L, as.integer(n_strata))
  out <- rep(1L, length(counts))
  names(out) <- names(counts)
  if (n_strata == 1L || length(unique(counts)) < 2L) return(out)

  ord  <- order(counts)
  n    <- length(counts)
  ## Bin by position in the sorted order, so bins are of near-equal size.
  gpos <- as.integer(cut(seq_len(n), breaks = min(n_strata, n), labels = FALSE))
  ## Then collapse ties: every actor sharing a count takes the LOWEST bin any of
  ## them was given, which merges the bin a tied run straddles.
  cs   <- counts[ord]
  gmin <- tapply(gpos, cs, min)
  g    <- as.integer(gmin[as.character(counts)])
  ## Renumber consecutively so `n_strata` in the returned object is the number of
  ## bins that actually exist.
  out[] <- as.integer(factor(g, levels = sort(unique(g))))
  out
}


#' Permute a Label Vector Within Blocks
#'
#' @param actor The event-level actor label vector.
#' @param block Integer block index, one per element of \code{actor}.
#' @return \code{actor} shuffled within each block.  Every label keeps its
#'   multiplicity, so every actor keeps its own event count exactly.
#' @keywords internal
#' @noRd
.searchnet_permute_within <- function(actor, block) {
  out <- actor
  for (b in unique(block)) {
    idx <- which(block == b)
    if (length(idx) < 2L) next
    ## `sample(idx)` would be wrong for a length-1 idx (it would permute
    ## seq_len(idx)); the guard above plus explicit sample.int() removes any
    ## dependence on that quirk. With a single block spanning every event this
    ## consumes exactly the RNG that `sample(actor)` consumes, so `strata =
    ## "none"` and a degenerate stratification agree bit for bit.
    out[idx] <- actor[idx[sample.int(length(idx))]]
  }
  out
}


#' Permutation Null for Behavioral Repertoires
#'
#' Re-runs \code{\link{searchnet_repertoire}} on chains whose actor labels have
#' been permuted across events.  This holds the event set, the statistics and the
#' clustering algorithm fixed and breaks the association between an actor and the
#' moves attributed to it.  By default the permutation is confined to strata of
#' similar own-event count, so that activity VOLUME -- which several of the
#' features are mechanically increasing in -- is not what the test detects.
#'
#' @param chain_stats As for \code{\link{searchnet_repertoire}}.
#' @param n_perm Number of permutations (default 199).
#' @param k Number of repertoires.  When supplied it is imposed on the observed
#'   run and on every permutation, and \code{k_selection} does not arise.  When
#'   \code{NULL} it is chosen by maximum silhouette over \code{k_range}; see
#'   \code{k_selection} for what the permutations are then allowed to do.
#' @param k_range Candidate \code{k} values, used by the observed run when
#'   \code{k = NULL} and by every permutation when
#'   \code{k_selection = "reselect"}.
#' @param k_selection One of \code{"reselect"} (default) or \code{"fixed"}, and
#'   relevant only when \code{k = NULL}.  See the section below.
#' @param strata One of \code{"n_events"} (default) or \code{"none"}.  See the
#'   section below.
#' @param n_strata Number of event-count bins when \code{strata = "n_events"}
#'   (default 10, i.e. deciles).  Bins are value-based, so equal-count actors are
#'   never split across bins, and fewer than \code{n_strata} bins are used when
#'   the count distribution cannot support that many.
#' @param method One of \code{"permute"} (default) or \code{"residualise"}.
#'   \code{"residualise"} clusters features residualised on \code{log(n_events)}
#'   in BOTH the observed and the permuted runs and permutes freely on top; it is
#'   an alternative attack on the same confound and forces \code{strata} to
#'   \code{"none"}, since the confound has already been removed from the
#'   features.
#' @param seed Optional integer seed.
#' @param ... Passed to \code{\link{searchnet_repertoire}}.
#'
#' @return An object of class \code{"searchnet_repertoire_null"} with
#'   \code{observed} (silhouette on the real labels), \code{permuted} (numeric
#'   vector), \code{p_value} (the proportion of permutations at least as
#'   separated as observed, with the standard \code{(r + 1) / (n + 1)}
#'   correction), \code{k} (the observed run's k), \code{k_permuted} (the k each
#'   permutation used), \code{n_perm}, \code{strata}, \code{k_selection},
#'   \code{method}, and \code{strata_sizes} (actors per event-count bin).
#'
#' @section What the stratified permutation holds fixed, exactly:
#' Under \code{strata = "n_events"} the actors are binned by their own total
#' event count and the event-to-actor assignment is shuffled only within a bin.
#' Held fixed: the event set and every event-level statistic, unaltered; each
#' actor's own event count, exactly, not merely in distribution; the set of
#' actors that clear \code{min_events}, so no permutation can fail the filter;
#' and the composition of the event pool within each bin, so any mechanical
#' dependence of a feature on the event count of the actor who generated it
#' survives into the null at the resolution of the bins.  Broken: which of the
#' comparably-active actors performed which of their events.
#'
#' The test is therefore CONDITIONAL.  A small p says actors of similar activity
#' volume are distinguishable by what they do.  It no longer says merely that
#' actors differ in how much they do, which a free permutation
#' (\code{strata = "none"}) will report as significant in a population with no
#' behavioral types at all, because \code{focusing} counts an actor's repeats of
#' its own prior pairs and \code{mixing} is activity times popularity.
#'
#' Two costs, both of which should be reported rather than absorbed.  Bins are
#' coarse, so within-bin count variation leaves some confounding behind; widen
#' \code{n_strata} to reduce it, at the price of the second cost.  And a bin
#' holding one actor admits no permutation at all, so that actor is frozen at its
#' observed labeling; in the limit where every bin is a singleton the null
#' degenerates to the identity and p is 1 by construction.  \code{$strata_sizes}
#' is returned so this is visible, and a warning fires when most actors sit
#' alone.  Run \code{strata = "none"} alongside and report both: a result that is
#' significant free but not stratified is a result about activity volume.
#'
#' @section Who is allowed to choose k:
#' With \code{k = NULL} the observed silhouette is a maximum over
#' \code{k_range}.  \code{k_selection = "reselect"} (default) lets every
#' permutation maximise over the same \code{k_range}, so the comparison is
#' maximum against maximum.  \code{k_selection = "fixed"} imposes the observed
#' \code{k} on every permutation, which compares a maximum with a single draw and
#' is anticonservative -- but which does keep every partition at one granularity,
#' the concern that motivated it.  The resolution taken here is that the average
#' silhouette width is scale-free and comparable across \code{k}, so comparing
#' maxima is legitimate; comparing a maximum with a non-maximum is not.  When
#' theory fixes \code{k}, pass it and neither issue arises.
#'
#' @section How to read it:
#' A small p-value means the actor-to-behavior association carries separation
#' that permuted labels do not reproduce.  A large one means the partition is
#' what k-means produces from this feature geometry regardless of who did what,
#' and the repertoires should not be interpreted.  Report the value either way:
#' this is a measurement, and a null result here is informative rather than a
#' failure.
#'
#' @seealso \code{\link{searchnet_repertoire}},
#'   \code{\link{searchnet_repertoire_stability}}
#' @export
searchnet_repertoire_null <- function(chain_stats,
                                      n_perm       = 199L,
                                      k            = NULL,
                                      k_range      = 2:6,
                                      k_selection  = c("reselect", "fixed"),
                                      strata       = c("n_events", "none"),
                                      n_strata     = 10L,
                                      method       = c("permute",
                                                       "residualise"),
                                      seed         = NULL,
                                      ...) {

  k_selection <- match.arg(k_selection)
  strata      <- match.arg(strata)
  method      <- match.arg(method)

  if (inherits(chain_stats, "SaomNkRSienaBiEnv"))
    chain_stats <- searchnet_chain_stats(chain_stats)

  dots <- list(...)
  if ("residualise" %in% names(dots) && method == "residualise" &&
      !isTRUE(dots$residualise))
    stop("`method = \"residualise\"` contradicts `residualise = FALSE`. Pass ",
         "one or the other.", call. = FALSE)
  resid <- method == "residualise" || isTRUE(dots$residualise)
  dots$residualise <- NULL

  if (method == "residualise" && strata != "none") {
    ## Not silently overridden: stacking both corrections would strip the volume
    ## signal twice and there would be no way to say which one the p-value came
    ## from.
    strata <- "none"
  }

  ## The observed run. When k = NULL it maximises over k_range; that maximum is
  ## the statistic every permutation must be compared against.
  obs <- do.call(searchnet_repertoire,
                 c(list(chain_stats, k = k, k_range = k_range, seed = seed,
                        residualise = resid), dots))
  k_obs <- obs$k

  ## Each permutation either re-selects over the same k_range (honest, max vs
  ## max) or is forced to the observed k.
  k_perm_arg <- if (is.null(k) && k_selection == "reselect") NULL else k_obs

  ## Strata are computed ONCE, from the observed counts. Since a within-block
  ## shuffle preserves every label's multiplicity, the counts -- and hence the
  ## strata, and hence which actors clear min_events -- are invariant across
  ## permutations.
  tab <- table(chain_stats$actor)
  cnt <- as.integer(tab)
  names(cnt) <- names(tab)
  if (strata == "n_events") {
    bin_of_actor <- .searchnet_repertoire_strata(cnt, n_strata = n_strata)
    block <- unname(bin_of_actor[as.character(chain_stats$actor)])
    sizes <- table(bin_of_actor)
    if (sum(sizes == 1L) > 0.5 * length(bin_of_actor))
      warning(sprintf(paste0("%d of %d actors sit alone in their event-count ",
                             "stratum and are therefore frozen at their ",
                             "observed labeling. The stratified null has ",
                             "little or no power here; reduce `n_strata`, or ",
                             "report `strata = \"none\"` alongside and treat ",
                             "the difference as the volume effect."),
                      sum(sizes == 1L), length(bin_of_actor)), call. = FALSE)
  } else {
    bin_of_actor <- rep(1L, length(cnt))
    names(bin_of_actor) <- names(cnt)
    block <- rep(1L, nrow(chain_stats))
    sizes <- table(bin_of_actor)
  }

  if (!is.null(seed)) set.seed(seed)
  res <- lapply(seq_len(n_perm), function(b) {
    cs <- chain_stats
    cs$actor <- .searchnet_permute_within(cs$actor, block)
    out <- try(do.call(searchnet_repertoire,
                       c(list(cs, k = k_perm_arg, k_range = k_range,
                              residualise = resid), dots)),
               silent = TRUE)
    if (inherits(out, "try-error")) c(NA_real_, NA_real_)
    else c(out$silhouette, as.numeric(out$k))
  })
  perm   <- vapply(res, `[`, numeric(1), 1L)
  perm_k <- vapply(res, `[`, numeric(1), 2L)

  keep <- !is.na(perm)
  ok   <- perm[keep]
  if (!length(ok))
    stop("every permutation failed to produce a partition; the null cannot be ",
         "computed. This usually means `min_events` is too high for the ",
         "permuted label distribution.", call. = FALSE)
  if (length(ok) < n_perm)
    warning(sprintf("%d of %d permutations failed and were excluded.",
                    n_perm - length(ok), n_perm), call. = FALSE)

  structure(list(
    observed     = obs$silhouette,
    permuted     = ok,
    p_value      = (sum(ok >= obs$silhouette) + 1) / (length(ok) + 1),
    k            = k_obs,
    k_permuted   = as.integer(perm_k[keep]),
    n_perm       = length(ok),
    strata       = strata,
    n_strata     = length(sizes),
    strata_sizes = as.integer(sizes),
    k_selection  = if (is.null(k)) k_selection else "supplied",
    method       = method
  ), class = "searchnet_repertoire_null")
}


#' Adjusted Rand Index Between Two Partitions
#'
#' Hubert-Arabie form, from the definition, so that no clustering package becomes
#' a dependency for one number.
#'
#' @param a,b Cluster labels of the same objects, in the same order.
#' @return Adjusted Rand index; 1 for identical partitions up to relabeling, 0
#'   for the expected value of an unrelated pair.
#' @keywords internal
#' @noRd
.searchnet_ari <- function(a, b) {
  if (length(a) != length(b))
    stop("partitions of different length.", call. = FALSE)
  n <- length(a)
  if (n < 2L) return(NA_real_)
  ct  <- table(a, b)
  ch2 <- function(z) z * (z - 1) / 2
  idx <- sum(ch2(as.numeric(ct)))
  sa  <- sum(ch2(as.numeric(rowSums(ct))))
  sb  <- sum(ch2(as.numeric(colSums(ct))))
  tot <- ch2(as.numeric(n))
  exp <- sa * sb / tot
  mx  <- (sa + sb) / 2
  if (mx == exp) {
    ## Both partitions are trivial in the same way -- each all-in-one-cluster, or
    ## each all-singletons -- so the index has no variance to adjust. They agree
    ## exactly, and 1 is the honest answer; returning NaN here would propagate
    ## into the mean and destroy an otherwise informative stability report.
    return(1)
  }
  (idx - exp) / (mx - exp)
}


#' Stability of Behavioral Repertoires Across Chains
#'
#' A repertoire is computed from ONE ministep chain, which is a draw from a
#' distribution over sequences consistent with the observed panel endpoints.  An
#' actor's repertoire is therefore a property of that sampled history, not a
#' recovered attribute.  This function clusters several chains independently at
#' the same \code{k} and reports the pairwise adjusted Rand index of the actor
#' assignments, which is the evidence that licenses calling a repertoire an actor
#' characteristic rather than an accident of one sampled history.
#'
#' @param chain_stats_list A list of data.frames from
#'   \code{\link{searchnet_chain_stats}} (or of run \code{SaomNkRSienaBiEnv}
#'   objects), one per chain.  At least two.
#' @param k Number of repertoires.  Required, and imposed on every chain.
#' @param seed Optional integer seed.  The SAME seed is used for every chain, so
#'   a difference between two chains is a difference between the chains and not
#'   between two k-means starts.
#' @param ... Passed to \code{\link{searchnet_repertoire}}.
#'
#' @return An object of class \code{"searchnet_repertoire_stability"}: a list
#'   with \code{ari} (a chains-by-chains matrix, 1 on the diagonal),
#'   \code{pairs} (one row per unordered pair: \code{chain_i}, \code{chain_j},
#'   \code{n_common}, \code{ari}), \code{mean_ari}, \code{min_ari}, \code{k},
#'   \code{n_chains}, \code{assignments} (the per-chain
#'   \code{searchnet_repertoire} objects) and \code{common_actors} (actors
#'   assigned in every chain).
#'
#' @section What is compared, and what it cannot tell you:
#' Each pair is scored only over the actors assigned a repertoire in BOTH chains;
#' an actor that clears \code{min_events} in one chain and not the other carries
#' no information about agreement and is excluded, with the surviving count
#' reported in \code{$pairs$n_common} so that a high index computed over three
#' actors cannot be mistaken for a high index computed over thirty.  Cluster
#' LABELS are arbitrary and differ between runs, which is why the adjusted Rand
#' index -- a function of the co-membership relation alone -- is the right
#' statistic and a label-matching accuracy is not.
#'
#' The index is adjusted for chance, so 0 is what unrelated partitions give and
#' negative values are possible.  It is a measurement: report it whatever it
#' says.  A low value does not invalidate the repertoires of any single chain --
#' each is a correct description of its own history -- it says the typology is
#' not transportable, and the construct should then be reported at the level of a
#' chain rather than of an actor.  This function does not test a hypothesis and
#' returns no p-value; for that, pair it with
#' \code{\link{searchnet_repertoire_null}} on each chain.
#'
#' @seealso \code{\link{searchnet_repertoire}},
#'   \code{\link{searchnet_repertoire_null}}
#' @export
searchnet_repertoire_stability <- function(chain_stats_list,
                                           k,
                                           seed = NULL,
                                           ...) {

  if (inherits(chain_stats_list, "data.frame") ||
      inherits(chain_stats_list, "SaomNkRSienaBiEnv"))
    stop("`chain_stats_list` must be a LIST of chain-stats frames, one per ",
         "chain. Stability is a statement about several sampled histories and ",
         "cannot be computed from one.", call. = FALSE)
  if (!is.list(chain_stats_list) || length(chain_stats_list) < 2L)
    stop("at least two chains are needed to report stability; ",
         length(chain_stats_list), " supplied.", call. = FALSE)
  if (missing(k) || is.null(k))
    stop("`k` is required. Letting each chain choose its own k would compare ",
         "partitions of different granularity, and a low agreement would then ",
         "be uninterpretable -- it could be instability of the assignment or ",
         "instability of the selection. Fix k, and report the k-selection ",
         "question separately.", call. = FALSE)

  nc  <- length(chain_stats_list)
  nms <- names(chain_stats_list)
  if (is.null(nms) || any(!nzchar(nms))) nms <- paste0("chain", seq_len(nc))

  fits <- lapply(seq_len(nc), function(i) {
    cs <- chain_stats_list[[i]]
    if (inherits(cs, "SaomNkRSienaBiEnv")) cs <- searchnet_chain_stats(cs)
    out <- try(searchnet_repertoire(cs, k = k, seed = seed, ...), silent = TRUE)
    if (inherits(out, "try-error"))
      stop(sprintf("chain %s could not be clustered at k = %d: %s",
                   nms[i], k, conditionMessage(attr(out, "condition"))),
           call. = FALSE)
    out
  })
  names(fits) <- nms

  maps <- lapply(fits, function(f) {
    v <- f$assignment$repertoire
    names(v) <- as.character(f$assignment$actor)
    v
  })
  common <- Reduce(intersect, lapply(maps, names))

  ari <- matrix(NA_real_, nc, nc, dimnames = list(nms, nms))
  diag(ari) <- 1
  rows <- list()
  for (i in seq_len(nc - 1L)) {
    for (j in (i + 1L):nc) {
      sh <- intersect(names(maps[[i]]), names(maps[[j]]))
      v  <- if (length(sh) >= 2L)
        .searchnet_ari(maps[[i]][sh], maps[[j]][sh]) else NA_real_
      ari[i, j] <- ari[j, i] <- v
      rows[[length(rows) + 1L]] <- data.frame(
        chain_i = nms[i], chain_j = nms[j], n_common = length(sh), ari = v,
        stringsAsFactors = FALSE)
    }
  }
  pairs <- do.call(rbind, rows)
  rownames(pairs) <- NULL

  if (all(is.na(pairs$ari)))
    warning("no pair of chains shares two or more assigned actors, so no ",
            "adjusted Rand index could be computed. Stability is undefined ",
            "here, which is not the same as low.", call. = FALSE)

  structure(list(
    ari           = ari,
    pairs         = pairs,
    mean_ari      = if (all(is.na(pairs$ari))) NA_real_
                    else mean(pairs$ari, na.rm = TRUE),
    min_ari       = if (all(is.na(pairs$ari))) NA_real_
                    else min(pairs$ari, na.rm = TRUE),
    k             = as.integer(k),
    n_chains      = nc,
    assignments   = fits,
    common_actors = as.integer(common)
  ), class = "searchnet_repertoire_stability")
}


#' Model-Side Effect Importance per Actor (sienaRI bridge)
#'
#' Optional bridge to \code{RSiena::sienaRI}, which decomposes the relative
#' importance of EFFECTS in each actor's choice probabilities.  This is a
#' property of the fitted model evaluated at an actor's position, and is a
#' different object from the realized-behavior repertoires of
#' \code{\link{searchnet_repertoire}}.  Provided for callers who want both.
#'
#' @param dat A \code{siena} data object.
#' @param ans A \code{sienaFit} from \code{siena07}.
#' @param ... Passed to \code{RSiena::sienaRI}.
#' @return Whatever \code{RSiena::sienaRI} returns.
#'
#' @section Two-mode dependent variables, and the restriction that usually bites:
#' Bipartite dependent variables have been supported since RSiena 1.3.8, but
#' with a \emph{dimensional} restriction that is easy to miss: the function
#' refuses a two-mode network whose second mode is at least as large as its
#' first.  In a contributor-by-module panel the modules routinely outnumber the
#' contributors, in which case this is unavailable however the model is
#' specified.  Check the dimensions before building anything on it.
#'
#' Two further version traps.  \code{sienaRI} was temporarily withdrawn in
#' RSiena 1.4.6 for a memory leak and reinstated in 1.5.1, so an install at
#' 1.5.0 does not export it at all; and from RSiena 1.6 the entry point is
#' renamed \code{interpret_size()}, with a different argument order.  It also
#' covers method-of-moments fits and \code{eval}-type effects only -- no
#' endowment, creation or rate effects, and no interactions.
#'
#' If the call fails, this function reports the failure and names the cause
#' rather than returning a degraded result.  A capability the software does not
#' offer is a non-implementation: it says nothing about the world, and no
#' substitute quantity should be reported in its place.
#' \code{\link{searchnet_repertoire}} measures realized behavior and is subject
#' to none of the above.
#'
#' @seealso \code{\link{searchnet_repertoire}}
#' @export
searchnet_repertoire_ri <- function(dat, ans, ...) {
  if (!requireNamespace("RSiena", quietly = TRUE))
    stop("RSiena is not installed.", call. = FALSE)
  if (!"sienaRI" %in% getNamespaceExports("RSiena"))
    stop("this RSiena build (", as.character(utils::packageVersion("RSiena")),
         ") does not export `sienaRI`. It was withdrawn in 1.4.6 for a memory ",
         "leak and reinstated in 1.5.1; from 1.6 the entry point is renamed ",
         "`interpret_size()`. Use searchnet_repertoire() instead, which ",
         "measures realized behavior and needs none of this.", call. = FALSE)

  ## Check the dimensional restriction BEFORE calling, so the failure names the
  ## real cause. RSiena stops with "interpret_size does not work for bipartite
  ## networks with second mode >= first mode" -- and in a contributor-by-module
  ## panel the modules usually outnumber the contributors, so this is the
  ## expected outcome rather than an edge case.
  dv <- try(dat$depvars, silent = TRUE)
  if (!inherits(dv, "try-error") && is.list(dv)) {
    for (nm in names(dv)) {
      d <- dim(dv[[nm]])
      if (length(d) >= 2L && identical(attr(dv[[nm]], "type"), "bipartite") &&
          !is.na(d[1]) && !is.na(d[2]) && d[2] >= d[1])
        stop(sprintf(paste0("dependent variable '%s' is bipartite with second ",
                            "mode %d >= first mode %d. RSiena's relative-",
                            "importance routine does not support that shape, ",
                            "so this is a NON-IMPLEMENTATION, not a null. Use ",
                            "searchnet_repertoire() instead."),
                     nm, d[2], d[1]), call. = FALSE)
    }
  }
  ## Resolved at run time rather than written as RSiena::sienaRI. The symbol is
  ## absent from some RSiena builds (1.4.6-1.5.0, and again from 1.6 under the
  ## new name) and a literal `::` makes R CMD check report it as a missing or
  ## unexported object on exactly those builds. The guard above has already
  ## established that RSiena exports it before we get here.
  .sienaRI <- get("sienaRI", envir = asNamespace("RSiena"))
  out <- try(.sienaRI(data = dat, ans = ans, ...), silent = TRUE)
  if (inherits(out, "try-error"))
    stop("RSiena::sienaRI() failed on this fit: ",
         conditionMessage(attr(out, "condition")),
         "\nIf the dependent variable is two-mode, this may be a ",
         "non-implementation in RSiena rather than a problem with the fit. ",
         "Report it as such; do not substitute another quantity for it. ",
         "`searchnet_repertoire()` measures realized behavior instead and ",
         "does not depend on sienaRI.", call. = FALSE)
  out
}


#' @export
print.searchnet_repertoire <- function(x, ...) {
  cat(sprintf("\nBehavioral repertoires: k = %d over %d actors\n",
              x$k, x$n_actors))
  cat(sprintf("Features: %s%s\n", paste(x$features, collapse = ", "),
              if (isTRUE(x$residualise))
                " (residualised on log(n_events))" else ""))
  cat(sprintf("Average silhouette width: %s\n",
              if (is.na(x$silhouette)) "undefined"
              else format(round(x$silhouette, 3))))
  if (!is.null(x$criterion)) {
    cat("\nk chosen by maximum silhouette:\n")
    print(x$criterion, row.names = FALSE)
  }
  cat("\nCluster profiles (original scale):\n")
  prof <- x$profiles
  num <- vapply(prof, is.numeric, logical(1))
  prof[num] <- lapply(prof[num], function(z) round(z, 3))
  print(prof, row.names = FALSE)
  if (nrow(x$dropped))
    cat(sprintf("\n%d actor(s) below the event threshold and unassigned.\n",
                nrow(x$dropped)))
  cat("\nRun searchnet_repertoire_null() before interpreting these.\n\n")
  invisible(x)
}


#' @export
print.searchnet_repertoire_null <- function(x, ...) {
  cat(sprintf("\nPermutation null for repertoires (k = %d, %d permutations)\n",
              x$k, x$n_perm))
  cat(sprintf("Observed silhouette : %s\n", format(round(x$observed, 4))))
  cat(sprintf("Permuted, mean [min, max] : %s [%s, %s]\n",
              format(round(mean(x$permuted), 4)),
              format(round(min(x$permuted), 4)),
              format(round(max(x$permuted), 4))))
  cat(sprintf("p = %s\n", format.pval(x$p_value, digits = 3)))

  if (!is.null(x$strata)) {
    cat(sprintf("Permutation : %s\n",
                if (identical(x$strata, "n_events"))
                  sprintf(paste0("within %d event-count strata (sizes %s); ",
                                 "each actor keeps its own event count"),
                          x$n_strata, paste(x$strata_sizes, collapse = "/"))
                else "free across all events (activity volume NOT held fixed)"))
  }
  if (identical(x$method, "residualise"))
    cat("Features    : residualised on log(n_events) before clustering\n")
  if (identical(x$k_selection, "reselect")) {
    cat(sprintf(paste0("k selection : re-chosen per permutation (max vs max); ",
                       "permuted k in [%d, %d]\n"),
                min(x$k_permuted), max(x$k_permuted)))
  } else if (identical(x$k_selection, "fixed")) {
    cat("k selection : observed k imposed on every permutation ",
        "(ANTICONSERVATIVE:\n              the observed statistic is a maximum ",
        "over k_range and the\n              permuted ones are not)\n", sep = "")
  }

  cat("\nA large p means k-means produces this separation from the feature\n",
      "geometry regardless of which actor made which move. Report it either\n",
      "way; a null here is a measurement, not a failure.\n", sep = "")
  if (identical(x$strata, "none") && !identical(x$method, "residualise"))
    cat("A free permutation also breaks the actor/activity-volume association,\n",
        "so a small p here may only say actors differ in HOW MUCH they do.\n",
        "Re-run with strata = \"n_events\" before interpreting it.\n", sep = "")
  cat("\n")
  invisible(x)
}


#' @export
print.searchnet_repertoire_stability <- function(x, ...) {
  cat(sprintf("\nRepertoire stability across %d chains (k = %d)\n",
              x$n_chains, x$k))
  cat(sprintf("Actors assigned in every chain: %d\n", length(x$common_actors)))
  cat(sprintf("Pairwise adjusted Rand index, mean %s [min %s]\n",
              format(round(x$mean_ari, 3)), format(round(x$min_ari, 3))))
  cat("\n")
  pr <- x$pairs
  pr$ari <- round(pr$ari, 3)
  print(pr, row.names = FALSE)
  cat("\nARI is chance-adjusted: 0 is what unrelated partitions give. A low\n",
      "value does not invalidate any one chain's repertoires; it says the\n",
      "typology is a property of a sampled history rather than of the actors,\n",
      "and should be reported at that level.\n\n", sep = "")
  invisible(x)
}
