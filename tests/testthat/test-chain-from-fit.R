###############################################################################
## test-chain-from-fit.R
## Unit tests for searchnet_chain_from_fit() in R/searchnet-chain-stats.R
##
## The load-bearing thing here is the FIELD EXTRACTION. An RSiena ministep is a
## list that DECLARES 13 elements, two of which ([[10]] and [[11]]) are
## zero-length, so unlist() returns 11 and silently shifts every position after
## 9 by two. Reading `stability` at position 13 of the unlisted vector therefore
## returns NA for every ministep, and NA-driven filtering fails quietly rather
## than loudly. Section 2 below re-extracts ego, alter and stability BY DECLARED
## INDEX independently of the implementation and demands the function agree,
## then demonstrates that the naive unlist() reading would NOT -- so the test
## proves the declared-index handling is doing work, rather than merely passing.
##
## Everything else is checked against an independent recomputation from
## `fit$chain` and `dat$depvars` rather than against a recorded number, so the
## file does not have to pin the estimator's output. Nothing here asserts a
## hard-coded event count.
##
## One real RSiena fit is built once and reused (M = 10 actors, N = 6
## components, 3 waves, one bipartite dependent variable). Two auxiliary fits --
## a one-mode-only fit and a two-dependent-variable fit -- are built lazily for
## the refusal tests. All three are tiny; the estimation costs well under a
## second each.
###############################################################################

## helper-setup.R sources every file in R/ via inst/saomnk-loader.R. Re-source
## the one file under test if that did not reach it, so a failure here is a
## failure of the code and not of the harness.
if (!exists("searchnet_chain_from_fit", mode = "function") &&
    exists("dir_r") &&
    file.exists(file.path(dir_r, "searchnet-chain-stats.R"))) {
  source(file.path(dir_r, "searchnet-chain-stats.R"), local = FALSE)
}


## ---------------------------------------------------------------------------
## Fixtures.
##
## Built lazily and cached, NOT at file scope: the skip() calls have to run
## inside a test, and a file-scope fit would be estimated even on a machine
## without RSiena.
## ---------------------------------------------------------------------------

.cff_cache <- new.env(parent = emptyenv())

## Panel with real wave-to-wave movement in both periods. The flip rate matters:
## a panel whose waves barely differ would make the "each chain starts from ITS
## OWN period's observed wave" test in section 4 vacuous, because starting from
## wave 1 and starting from wave 2 would give the same answer.
.cff_panel <- function(M, N, W, seed, p_init = 0.30, p_flip = 0.15) {
  set.seed(seed)
  arr <- array(0L, c(M, N, W))
  arr[, , 1] <- matrix(as.integer(stats::runif(M * N) < p_init), M, N)
  for (w in 2:W) {
    prev <- arr[, , w - 1]
    flip <- matrix(stats::runif(M * N) < p_flip, M, N)
    prev[flip] <- 1L - prev[flip]
    arr[, , w] <- prev
  }
  arr
}

.cff_quiet <- function(expr) {
  suppressMessages(suppressWarnings(
    utils::capture.output(val <- force(expr))))
  val
}

## The main fixture: one bipartite dependent variable, two periods.
cff_fixture <- function() {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  if (!is.null(.cff_cache$main)) return(.cff_cache$main)

  M <- 10; N <- 6; W <- 3
  arr <- .cff_panel(M, N, W, seed = 4242)

  f <- tryCatch(.cff_quiet({
    actors <- RSiena::sienaNodeSet(M, nodeSetName = "actors")
    comps  <- RSiena::sienaNodeSet(N, nodeSetName = "comps")
    mynet  <- RSiena::sienaDependent(arr, type = "bipartite",
                                     nodeSet = c("actors", "comps"))
    dat <- RSiena::sienaDataCreate(mynet, nodeSets = list(actors, comps))
    alg <- RSiena::sienaAlgorithmCreate(
      projname = NULL, nsub = 1, n3 = 10, seed = 271828, cond = FALSE)
    fit <- RSiena::siena07(
      alg, data = dat, effects = RSiena::getEffects(dat),
      returnChains = TRUE, batch = TRUE, silent = TRUE, useCluster = FALSE)
    list(fit = fit, dat = dat, M = M, N = N, W = W)
  }), error = function(e) skip(paste("RSiena fixture failed:", e$message)))

  if (is.null(f$fit$chain))
    skip("the fitted object carries no chains; returnChains was not honoured")
  .cff_cache$main <- f
  f
}

## A one-mode-only fit, for the scope-limit refusal.
cff_one_mode <- function() {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  if (!is.null(.cff_cache$one)) return(.cff_cache$one)

  M <- 10; W <- 3
  arr2 <- .cff_panel(M, M, W, seed = 11, p_init = 0.20, p_flip = 0.10)
  for (w in seq_len(W)) diag(arr2[, , w]) <- 0L

  f <- tryCatch(.cff_quiet({
    actors <- RSiena::sienaNodeSet(M, nodeSetName = "actors")
    friend <- RSiena::sienaDependent(arr2, nodeSet = "actors")
    dat <- RSiena::sienaDataCreate(friend, nodeSets = list(actors))
    alg <- RSiena::sienaAlgorithmCreate(
      projname = NULL, nsub = 1, n3 = 10, seed = 99, cond = FALSE)
    fit <- RSiena::siena07(
      alg, data = dat, effects = RSiena::getEffects(dat),
      returnChains = TRUE, batch = TRUE, silent = TRUE, useCluster = FALSE)
    list(fit = fit, dat = dat)
  }), error = function(e) skip(paste("one-mode fixture failed:", e$message)))
  .cff_cache$one <- f
  f
}

## A two-dependent-variable fit: one one-mode network, then one bipartite.
## sienaDataCreate() insists one-mode networks are declared first, so the
## bipartite dependent variable is necessarily at position 2 of `$depvars`.
cff_two_dv <- function() {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  if (!is.null(.cff_cache$two)) return(.cff_cache$two)

  M <- 10; N <- 6; W <- 3
  arrB <- .cff_panel(M, N, W, seed = 777)
  arrO <- .cff_panel(M, M, W, seed = 778, p_init = 0.20, p_flip = 0.10)
  for (w in seq_len(W)) diag(arrO[, , w]) <- 0L

  f <- tryCatch(.cff_quiet({
    actors <- RSiena::sienaNodeSet(M, nodeSetName = "actors")
    comps  <- RSiena::sienaNodeSet(N, nodeSetName = "comps")
    friend <- RSiena::sienaDependent(arrO, nodeSet = "actors")
    mynet  <- RSiena::sienaDependent(arrB, type = "bipartite",
                                     nodeSet = c("actors", "comps"))
    dat <- RSiena::sienaDataCreate(friend, mynet,
                                   nodeSets = list(actors, comps))
    alg <- RSiena::sienaAlgorithmCreate(
      projname = NULL, nsub = 1, n3 = 10, seed = 1234, cond = FALSE)
    fit <- RSiena::siena07(
      alg, data = dat, effects = RSiena::getEffects(dat),
      returnChains = TRUE, batch = TRUE, silent = TRUE, useCluster = FALSE)
    list(fit = fit, dat = dat, N = N)
  }), error = function(e) skip(paste("two-DV fixture failed:", e$message)))
  .cff_cache$two <- f
  f
}


## ---------------------------------------------------------------------------
## Independent re-extraction, by DECLARED index. Shares no code with the
## implementation: it walks fit$chain itself and applies the documented filter
## (right dependent variable, not a stability step, alter != N) from scratch.
##
## Note the second subscript. In RSiena 1.5.0 the level below the run is the
## DATA GROUP, not the dependent variable -- every dependent variable's
## ministeps are interleaved inside one per-period list and told apart by the
## name at declared position 3. This fixture has a single group and a single
## dependent variable, so group index 1 is also `which(names(dat$depvars) ==
## dv_name)` and the two readings agree here. Section 6 covers the case where
## they do not.
## ---------------------------------------------------------------------------
cff_extract <- function(fit, group, period, dv_name, N) {
  ms <- fit$chain[[group]][[1]][[period]]
  if (!length(ms)) return(NULL)
  nm   <- vapply(ms, function(x) as.character(x[[3]]), character(1))
  ego  <- vapply(ms, function(x) as.integer(x[[4]]),   integer(1))
  alt  <- vapply(ms, function(x) as.integer(x[[5]]),   integer(1))
  stab <- vapply(ms, function(x) as.logical(x[[13]]),  logical(1))
  keep <- nm == dv_name & !stab & alt != N
  list(ego = ego, alt = alt, stab = stab, nm = nm, keep = keep,
       n_ministeps = length(ms))
}


# ===========================================================================
# 1. Shape, columns and chain identity
# ===========================================================================

test_that("the frame carries the chain-stats columns plus run and period", {
  f <- cff_fixture()
  res <- searchnet_chain_from_fit(f$fit, f$dat)

  expect_s3_class(res, "data.frame")
  ## The fixed column set comes FIRST and in order, so a frame from this
  ## function rbinds against one from searchnet_chain_stats() with no
  ## alignment logic; `run` and `period` are appended.
  expect_identical(names(res),
                   c(.SEARCHNET_CHAIN_STAT_COLS, "run", "period"))
  expect_true(all(res$source == "simulated"))
  expect_type(res$run, "integer")
  expect_type(res$period, "integer")
  expect_type(res$chain_id, "integer")
  expect_identical(rownames(res), as.character(seq_len(nrow(res))))
  expect_identical(attr(res, "dv_name"), "mynet")
})

test_that("chain_id is unique per (run, period), in both directions", {
  f <- cff_fixture()
  res <- searchnet_chain_from_fit(f$fit, f$dat)

  key <- paste(res$run, res$period, sep = "/")
  ## One chain_id per key ...
  per_key <- tapply(res$chain_id, key, function(v) length(unique(v)))
  expect_true(all(per_key == 1L))
  ## ... and one key per chain_id. Both directions are needed: the first alone
  ## would pass if two periods collapsed onto a single chain_id, which is
  ## exactly the pooling the documentation forbids, because the event-history
  ## counters restart at each period's observed wave.
  per_cid <- tapply(key, res$chain_id, function(v) length(unique(v)))
  expect_true(all(per_cid == 1L))
  expect_equal(length(unique(res$chain_id)), length(unique(key)))
  ## chain_id is a dense 1..k labeling of this call's chains.
  expect_equal(sort(unique(res$chain_id)), seq_len(length(unique(key))))
  ## event_id restarts within each chain and is a complete run of 1..n_e.
  ids <- split(res$event_id, res$chain_id)
  expect_true(all(vapply(ids, function(v) identical(v, seq_along(v)),
                         logical(1))))
})

test_that("the chain count equals (run, period) pairs with a realized change", {
  f <- cff_fixture()
  res <- searchnet_chain_from_fit(f$fit, f$dat)

  n_runs <- length(f$fit$chain)
  n_per  <- length(f$fit$chain[[1]][[1]])
  expect_gt(n_runs, 1L)
  expect_equal(n_per, f$W - 1L)

  ## Count, independently, the (run, period) pairs carrying at least one
  ## realized change on this dependent variable.
  n_expected <- 0L
  for (r in seq_len(n_runs)) for (p in seq_len(n_per)) {
    e <- cff_extract(f$fit, r, p, "mynet", f$N)
    if (!is.null(e) && any(e$keep)) n_expected <- n_expected + 1L
  }
  expect_equal(length(unique(res$chain_id)), n_expected)
  expect_equal(nrow(unique(res[, c("run", "period")])), n_expected)
  ## Non-vacuous: this fixture really does produce a chain for every pair.
  expect_equal(n_expected, n_runs * n_per)
  expect_setequal(unique(res$run), seq_len(n_runs))
  expect_setequal(unique(res$period), seq_len(n_per))
})


# ===========================================================================
# 2. Field extraction -- the highest-value test in the file
#
# The function's `actor` and `component` must equal an independent re-extraction
# by DECLARED index, plus one for the 0-indexing. And the naive unlist() reading
# of `stability` must DISAGREE, or the declared-index handling is not being
# exercised and the test proves nothing.
# ===========================================================================

test_that("actor and component match an independent declared-index extraction", {
  f <- cff_fixture()
  res <- searchnet_chain_from_fit(f$fit, f$dat)

  n_runs <- length(f$fit$chain)
  n_per  <- length(f$fit$chain[[1]][[1]])

  ## Compared per (run, period) rather than against one long vector, so the
  ## check does not depend on the order the function happens to bind chains in.
  bad_actor <- bad_comp <- character(0)
  n_checked <- 0L
  for (p in seq_len(n_per)) for (r in seq_len(n_runs)) {
    e <- cff_extract(f$fit, r, p, "mynet", f$N)
    if (is.null(e) || !any(e$keep)) next
    sub <- res[res$run == r & res$period == p, ]
    n_checked <- n_checked + 1L
    if (!identical(sub$actor, e$ego[e$keep] + 1L))
      bad_actor <- c(bad_actor, sprintf("run %d period %d", r, p))
    if (!identical(sub$component, e$alt[e$keep] + 1L))
      bad_comp <- c(bad_comp, sprintf("run %d period %d", r, p))
  }
  expect_gt(n_checked, 0L)
  expect_equal(bad_actor, character(0))
  expect_equal(bad_comp, character(0))

  ## The +1 is not cosmetic: RSiena's ego and alter are 0-indexed, so a missing
  ## increment would push every index down one and silently address the wrong
  ## cell of the incidence matrix.
  expect_gte(min(res$actor), 1L)
  expect_lte(max(res$actor), f$M)
  expect_gte(min(res$component), 1L)
  expect_lte(max(res$component), f$N)
})

test_that("a ministep declares 13 elements of which two are zero-length", {
  f <- cff_fixture()
  ## The premise of the whole extraction, checked on EVERY ministep in the fit
  ## rather than on one sampled step.
  lens <- unlist(lapply(f$fit$chain, function(r)
    lapply(r[[1]], function(p) vapply(p, length, integer(1)))))
  expect_true(all(lens == 13L))

  empties <- unlist(lapply(f$fit$chain, function(r)
    lapply(r[[1]], function(p)
      vapply(p, function(x) length(x[[10]]) + length(x[[11]]), integer(1)))))
  expect_true(all(empties == 0L))

  ms <- f$fit$chain[[1]][[1]][[1]][[1]]
  expect_length(ms, 13L)
  expect_length(unlist(ms), 11L)
})

test_that("a naive unlist() reading of stability disagrees with the declared one", {
  f <- cff_fixture()
  ms <- f$fit$chain[[1]][[1]][[1]]
  skip_if(length(ms) == 0L, "empty ministep list")

  declared <- vapply(ms, function(x) as.logical(x[[13]]), logical(1))
  naive    <- vapply(ms, function(x) as.logical(unlist(x)[13]), logical(1))

  ## Position 13 of the unlisted vector does not exist -- unlist() returns 11
  ## elements -- so the naive reading is NA for every ministep. That is the
  ## failure mode the declared-index handling exists to prevent, and it is a
  ## QUIET one: NA-driven filtering drops rows rather than raising.
  expect_true(all(is.na(naive)))
  expect_false(isTRUE(all.equal(naive, declared)))
  expect_true(any(declared))   ## non-vacuous: some steps really are stability

  ## And the shift is exactly two: stability lands at unlisted position 11.
  shifted <- vapply(ms, function(x) as.logical(unlist(x)[11]), logical(1))
  expect_identical(shifted, declared)
  ## Likewise the diagonal flag, declared at 12, lands at 10.
  expect_identical(vapply(ms, function(x) as.logical(unlist(x)[10]), logical(1)),
                   vapply(ms, function(x) as.logical(x[[12]]), logical(1)))

  ## Positions at or before 9 are NOT shifted, which is why ego and alter
  ## survive an unlist() and stability does not. This is the asymmetry that
  ## makes the bug hard to see.
  expect_identical(vapply(ms, function(x) as.integer(unlist(x)[4]), integer(1)),
                   vapply(ms, function(x) as.integer(x[[4]]), integer(1)))
  expect_identical(vapply(ms, function(x) as.integer(unlist(x)[5]), integer(1)),
                   vapply(ms, function(x) as.integer(x[[5]]), integer(1)))
})

test_that("stability steps are excluded from the events", {
  f <- cff_fixture()
  res <- searchnet_chain_from_fit(f$fit, f$dat)

  n_runs <- length(f$fit$chain)
  n_per  <- length(f$fit$chain[[1]][[1]])
  n_stab <- 0L; n_keep <- 0L; n_all <- 0L
  for (r in seq_len(n_runs)) for (p in seq_len(n_per)) {
    e <- cff_extract(f$fit, r, p, "mynet", f$N)
    if (is.null(e)) next
    n_all  <- n_all + e$n_ministeps
    n_stab <- n_stab + sum(e$stab)
    n_keep <- n_keep + sum(e$keep)
  }
  expect_gt(n_stab, 0L)              ## non-vacuous
  expect_equal(nrow(res), n_keep)
  expect_equal(n_all - n_keep, n_stab)
})


# ===========================================================================
# 3. alter == N is "no change" and must not become an event
# ===========================================================================

test_that("alter == N no-change ministeps are excluded", {
  f <- cff_fixture()
  res <- searchnet_chain_from_fit(f$fit, f$dat)
  N <- f$N

  n_runs <- length(f$fit$chain)
  n_per  <- length(f$fit$chain[[1]][[1]])
  n_nochange <- 0L; n_all <- 0L
  coincide <- TRUE
  for (r in seq_len(n_runs)) for (p in seq_len(n_per)) {
    e <- cff_extract(f$fit, r, p, "mynet", f$N)
    if (is.null(e)) next
    n_all      <- n_all + e$n_ministeps
    n_nochange <- n_nochange + sum(e$alt == N)
    ## In this fit the stability flag and alter == N pick out exactly the same
    ## ministeps. Asserted rather than assumed, because if they ever came apart
    ## the arithmetic in the next block would silently change meaning.
    if (!identical(e$stab, e$alt == N)) coincide <- FALSE
  }
  expect_true(coincide)
  expect_gt(n_nochange, 0L)          ## non-vacuous: they do occur
  expect_equal(nrow(res), n_all - n_nochange)

  ## The decisive assertion: on the 1-indexed scale a no-change step would be
  ## component N + 1, one past the last column of the incidence matrix. It is
  ## absent.
  expect_false(any(res$component == N + 1L))
  expect_lte(max(res$component), N)

  ## And 0-indexed alter really does reach N in this fit, so the filter has
  ## something to do. Without it .searchnet_replay() would refuse the whole
  ## chain with "out of range" -- loudly here, but only because M and N differ;
  ## on a square panel it would address the wrong cell instead.
  alts <- unlist(lapply(f$fit$chain, function(r)
    lapply(r[[1]], function(p)
      vapply(p, function(x) as.integer(x[[5]]), integer(1)))))
  expect_equal(max(alts), N)
})


# ===========================================================================
# 4. Each chain starts from ITS OWN period's observed wave
# ===========================================================================

test_that("a period-2 chain's first event reads the observed wave 2", {
  f <- cff_fixture()
  res <- searchnet_chain_from_fit(f$fit, f$dat)
  A <- f$dat$depvars[[1]]
  W2 <- A[, , 2]

  first <- res[res$period == 2L & res$event_id == 1L, ]
  expect_gt(nrow(first), 0L)
  expect_equal(first$tie_before,
               as.integer(W2[cbind(first$actor, first$component)]))
})

test_that("the start state is the period's wave, not wave 1", {
  ## The assertion above is only worth something if wave 1 and wave 2 could
  ## have given different answers. They do: this panel moves 13 of 60 cells
  ## between the two waves, and the first events of the period-2 chains land on
  ## some of them.
  f <- cff_fixture()
  res <- searchnet_chain_from_fit(f$fit, f$dat)
  A <- f$dat$depvars[[1]]
  W1 <- A[, , 1]; W2 <- A[, , 2]
  expect_gt(sum(W1 != W2), 0L)

  first <- res[res$period == 2L & res$event_id == 1L, ]
  cell  <- cbind(first$actor, first$component)
  disagree <- which(W1[cell] != W2[cell])
  expect_gt(length(disagree), 0L)    ## the check below is discriminating
  expect_equal(first$tie_before[disagree], as.integer(W2[cell][disagree]))
  expect_false(any(first$tie_before[disagree] == as.integer(W1[cell][disagree])))
})

test_that("a whole period-2 chain replays from wave 2, not from wave 1", {
  ## Stronger than the first-event check: replay each period-2 chain
  ## independently from each of the two waves and demand the function's
  ## `tie_before` column match the wave-2 replay throughout, and differ from
  ## the wave-1 replay somewhere.
  f <- cff_fixture()
  res <- searchnet_chain_from_fit(f$fit, f$dat)
  A <- f$dat$depvars[[1]]
  B_w1 <- A[, , 1]; storage.mode(B_w1) <- "integer"
  B_w2 <- A[, , 2]; storage.mode(B_w2) <- "integer"

  n_runs <- length(f$fit$chain)
  mismatched_w2 <- character(0)
  n_differ_from_w1 <- 0L
  for (r in seq_len(n_runs)) {
    e <- cff_extract(f$fit, r, 2L, "mynet", f$N)
    if (is.null(e) || !any(e$keep)) next
    ev <- cbind(e$ego[e$keep] + 1L, e$alt[e$keep] + 1L)
    sub <- res[res$run == r & res$period == 2L, ]
    from2 <- .searchnet_replay(ev, B_w2)$tie_before
    from1 <- .searchnet_replay(ev, B_w1)$tie_before
    if (!identical(sub$tie_before, from2))
      mismatched_w2 <- c(mismatched_w2, sprintf("run %d", r))
    if (!identical(from1, from2)) n_differ_from_w1 <- n_differ_from_w1 + 1L
  }
  expect_equal(mismatched_w2, character(0))
  expect_gt(n_differ_from_w1, 0L)
})

test_that("a chain does not continue from the previous chain's end state", {
  ## Phase-3 runs are independent draws. RSiena restarts each one from the
  ## period's observed wave, so chain r must not inherit chain r-1's final
  ## matrix -- the defect the environment-path guard in
  ## searchnet_chain_stats() exists to refuse.
  f <- cff_fixture()
  res <- searchnet_chain_from_fit(f$fit, f$dat)
  A <- f$dat$depvars[[1]]
  W1 <- A[, , 1]
  finals <- attr(res, "B_final_by_chain")
  expect_true(is.list(finals))
  expect_equal(length(finals), length(unique(res$chain_id)))

  ## Every period-1 chain's first event reads the observed wave 1 ...
  first <- res[res$period == 1L & res$event_id == 1L, ]
  expect_gt(nrow(first), 0L)
  expect_equal(first$tie_before,
               as.integer(W1[cbind(first$actor, first$component)]))

  ## ... and at least one of those cells is one where the PREVIOUS chain ended
  ## up somewhere else, so a cumulative replay would have read a different
  ## value. Without this, the check above could pass on a fit whose chains all
  ## happened to end where they started.
  ord <- order(first$chain_id)
  first <- first[ord, ]
  n_disagree <- 0L
  for (k in seq_len(nrow(first))[-1]) {
    prev <- finals[[first$chain_id[k] - 1L]]
    if (prev[first$actor[k], first$component[k]] !=
        W1[first$actor[k], first$component[k]]) n_disagree <- n_disagree + 1L
  }
  expect_gt(n_disagree, 0L)
})


# ===========================================================================
# 5. Refusals, each by its own message
# ===========================================================================

test_that("a fit without chains is refused and says how to get them", {
  f <- cff_fixture()
  bare <- f$fit
  bare$chain <- NULL
  expect_error(searchnet_chain_from_fit(bare, f$dat),
               "carries no chains")
  expect_error(searchnet_chain_from_fit(bare, f$dat),
               "returnChains = TRUE", fixed = TRUE)
})

test_that("a `dat` with no $depvars is refused by name", {
  f <- cff_fixture()
  nodep <- f$dat
  nodep$depvars <- NULL
  expect_error(searchnet_chain_from_fit(f$fit, nodep),
               "does not look like a siena data object")
  expect_error(searchnet_chain_from_fit(f$fit, list()),
               "no `$depvars`", fixed = TRUE)
})

test_that("an unknown dv_name is refused and the available names are listed", {
  f <- cff_fixture()
  expect_error(searchnet_chain_from_fit(f$fit, f$dat, dv_name = "nosuchnet"),
               "no dependent variable named 'nosuchnet'")
  ## The message must name what IS available, or the caller is left guessing.
  expect_error(searchnet_chain_from_fit(f$fit, f$dat, dv_name = "nosuchnet"),
               "mynet")
})

test_that("a one-mode dependent variable is refused as a scope limit", {
  f <- cff_one_mode()
  ## Named explicitly: the encoding of "no change" differs between the two
  ## kinds of network, so replaying a one-mode chain with the bipartite rule
  ## would read the events wrongly rather than fail.
  expect_error(searchnet_chain_from_fit(f$fit, f$dat, dv_name = "friend"),
               "has type 'oneMode'")
  expect_error(searchnet_chain_from_fit(f$fit, f$dat, dv_name = "friend"),
               "id_from == id_to", fixed = TRUE)
  ## And the refusal is framed as a limitation of this replay, NOT as a claim
  ## that a one-mode SAOM is wrong or that its chains are meaningless.
  expect_error(searchnet_chain_from_fit(f$fit, f$dat, dv_name = "friend"),
               "scope limit, not a statement about the model", fixed = TRUE)

  ## With no bipartite dependent variable at all, the automatic choice reports
  ## a count of zero rather than picking the one-mode variable by default.
  expect_error(searchnet_chain_from_fit(f$fit, f$dat),
               "carries 0 bipartite dependent variable")
  expect_error(searchnet_chain_from_fit(f$fit, f$dat), "none", fixed = TRUE)
})

test_that("run and period selections that leave nothing valid are refused", {
  f <- cff_fixture()
  n_runs <- length(f$fit$chain)
  n_per  <- length(f$fit$chain[[1]][[1]])

  expect_error(searchnet_chain_from_fit(f$fit, f$dat, runs = n_runs + 5L),
               "no valid runs selected")
  expect_error(searchnet_chain_from_fit(f$fit, f$dat, runs = 0L),
               "no valid runs selected")
  expect_error(searchnet_chain_from_fit(f$fit, f$dat, runs = integer(0)),
               "no valid runs selected")
  ## The message reports how many the fit actually carries.
  expect_error(searchnet_chain_from_fit(f$fit, f$dat, runs = -1L),
               sprintf("the fit carries %d", n_runs))

  expect_error(searchnet_chain_from_fit(f$fit, f$dat, periods = n_per + 3L),
               "no valid periods selected")
  expect_error(searchnet_chain_from_fit(f$fit, f$dat, periods = 0L),
               "no valid periods selected")
  ## A period beyond the number of waves is out of range even if the chain
  ## list were longer: there is no observed wave to start it from.
  expect_error(searchnet_chain_from_fit(f$fit, f$dat, periods = f$W),
               "no valid periods selected")
})


# ===========================================================================
# 6. KNOWN FAILURE: the second subscript of fit$chain is the GROUP, not the
#    dependent variable
# ===========================================================================

test_that("a bipartite DV that is not first in $depvars is still extracted", {
  ## KNOWN FAILURE, and the bug is in the CODE, not in this test.
  ##
  ## searchnet_chain_from_fit() computes
  ##     dv_index <- which(names(dat$depvars) == dv_name)
  ## and then reads
  ##     ms <- fit$chain[[r]][[dv_index]][[p]]
  ##
  ## In RSiena 1.5.0 that second subscript is the DATA GROUP, not the dependent
  ## variable. Verified on this fixture: the data object carries two dependent
  ## variables and one group, and `length(fit$chain[[1]])` is 1, while
  ## `fit$chain[[1]][[1]][[1]]` holds the ministeps of BOTH variables
  ## interleaved -- their names at declared position 3 are "friend" and
  ## "mynet". The function already tells them apart with `nm == dv_name`, so
  ## the group is the only thing the second subscript can be.
  ##
  ## Consequence: any siena data object whose bipartite dependent variable is
  ## not at position 1 fails with "subscript out of bounds". That is not a rare
  ## shape -- sienaDataCreate() REQUIRES one-mode networks to be declared
  ## before bipartite ones, so every coevolution model of a one-mode network
  ## and a bipartite one puts the bipartite variable at position 2 and cannot
  ## be read at all. It works today only because the single-dependent-variable
  ## case makes dv_index == 1 == the group index.
  ##
  ## Fix in R/searchnet-chain-stats.R: index the group, not the dependent
  ## variable, at BOTH sites that use `dv_index` -- the period count
  ##     n_per_all <- length(fit$chain[[runs[1]]][[dv_index]])
  ## and the ministep list
  ##     ms <- fit$chain[[r]][[dv_index]][[p]]
  ## become `[[1L]]`, with a `group` argument if multi-group data is ever
  ## supported. The existing `nm == dv_name` filter already selects the
  ## variable, so `dv_index` then has no remaining use.
  ##
  ## Verified out of tree on a copy of the function with exactly that
  ## substitution: this two-variable fixture then yields 1452 events over 100
  ## chains, all indices in range, and the single-dependent-variable fixture
  ## returns a result identical to the current one. The fix is not a guess.
  ##
  ## Deliberately NOT weakened to expect_error(): the coevolution case is
  ## exactly the one this function is wanted for, and an error there is a
  ## defect rather than a documented scope limit.
  f <- cff_two_dv()
  expect_equal(which(names(f$dat$depvars) == "mynet"), 2L)
  expect_equal(length(f$fit$chain[[1]]), 1L)

  ## Both variables' ministeps really are interleaved in the one list.
  nms <- unique(vapply(f$fit$chain[[1]][[1]][[1]],
                       function(x) as.character(x[[3]]), character(1)))
  expect_setequal(nms, c("friend", "mynet"))

  res <- searchnet_chain_from_fit(f$fit, f$dat, dv_name = "mynet")
  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0L)
  ## Only the bipartite variable's ministeps, and none out of range.
  expect_lte(max(res$component), f$N)
  expect_gte(min(res$component), 1L)
})


# ===========================================================================
# 7. runs / periods subsetting
# ===========================================================================

test_that("a runs subset returns exactly those runs", {
  f <- cff_fixture()
  n_runs <- length(f$fit$chain)
  skip_if(n_runs < 5L, "too few phase-3 runs to subset")

  res <- searchnet_chain_from_fit(f$fit, f$dat, runs = c(2L, 5L))
  expect_setequal(unique(res$run), c(2L, 5L))
  ## The extracted rows are byte-identical to the same runs taken from the
  ## full extraction -- subsetting changes which chains appear, never their
  ## contents. Only chain_id is renumbered, because it labels this call.
  full <- searchnet_chain_from_fit(f$fit, f$dat)
  drop <- setdiff(names(res), "chain_id")
  a <- res[order(res$period, res$run, res$event_id), drop]
  b <- full[full$run %in% c(2L, 5L), ][order(full$period[full$run %in% c(2L, 5L)],
                                             full$run[full$run %in% c(2L, 5L)],
                                             full$event_id[full$run %in% c(2L, 5L)]),
                                       drop]
  rownames(a) <- rownames(b) <- NULL
  expect_equal(a, b)
  ## chain_id is renumbered densely from 1 within the call.
  expect_equal(sort(unique(res$chain_id)),
               seq_len(length(unique(paste(res$run, res$period)))))
})

test_that("a periods subset returns exactly that period", {
  f <- cff_fixture()
  n_per <- length(f$fit$chain[[1]][[1]])
  skip_if(n_per < 2L, "only one period")

  res <- searchnet_chain_from_fit(f$fit, f$dat, periods = 2L)
  expect_setequal(unique(res$period), 2L)
  expect_equal(length(unique(res$chain_id)), length(unique(res$run)))
  ## Cross-check against the full extraction.
  full <- searchnet_chain_from_fit(f$fit, f$dat)
  expect_equal(nrow(res), sum(full$period == 2L))
})

test_that("runs and periods can be subset together", {
  f <- cff_fixture()
  skip_if(length(f$fit$chain) < 4L, "too few phase-3 runs to subset")
  res <- searchnet_chain_from_fit(f$fit, f$dat, runs = c(1L, 3L), periods = 2L)
  expect_setequal(unique(res$run), c(1L, 3L))
  expect_setequal(unique(res$period), 2L)
  expect_lte(length(unique(res$chain_id)), 2L)
})

test_that("out-of-range entries are dropped from an otherwise valid selection", {
  f <- cff_fixture()
  n_runs <- length(f$fit$chain)
  res <- searchnet_chain_from_fit(f$fit, f$dat,
                                  runs = c(2L, n_runs + 100L),
                                  periods = c(1L, 99L))
  expect_setequal(unique(res$run), 2L)
  expect_setequal(unique(res$period), 1L)
})


# ===========================================================================
# 8. Composition: the frame feeds searchnet_chain_compare() unchanged
# ===========================================================================

test_that("chains from a fit compare against an observed log without reshaping", {
  f <- cff_fixture()
  sim <- searchnet_chain_from_fit(f$fit, f$dat)

  ## An observed event log on the same node sets, replayed from wave 1.
  set.seed(20260823)
  A <- f$dat$depvars[[1]]
  B0 <- A[, , 1]; storage.mode(B0) <- "integer"
  obs <- searchnet_chain_stats(
    data.frame(a = sample.int(f$M, 40, replace = TRUE),
               b = sample.int(f$N, 40, replace = TRUE)),
    B0 = B0)

  ## The extra `run` and `period` columns must not get in the way: the
  ## comparison addresses `chain_id` and the statistic columns by name.
  res <- searchnet_chain_compare(sim, obs)
  expect_s3_class(res, "searchnet_chain_compare")
  expect_equal(nrow(res), 4L)
  expect_equal(res$statistic, .SEARCHNET_REM_STATS)
  expect_equal(res$n_chains, rep(length(unique(sim$chain_id)), 4L))
  expect_true(all(is.finite(res$observed)))
  expect_true(all(res$sim_lo <= res$sim_hi))

  ## And the two frames rbind after dropping the two extra columns, which is
  ## the point of keeping the fixed column set first and in order.
  expect_identical(names(sim)[seq_along(names(obs))], names(obs))
  bound <- rbind(sim[, .SEARCHNET_CHAIN_STAT_COLS], obs)
  expect_equal(nrow(bound), nrow(sim) + nrow(obs))
})

test_that("a single-run extraction is refused by the comparison, as designed", {
  ## One phase-3 run still yields two chains here (one per period), so the
  ## refusal has to be provoked with a single period as well. The guard is
  ## searchnet_chain_compare()'s, and it must survive the extra columns.
  f <- cff_fixture()
  sim <- searchnet_chain_from_fit(f$fit, f$dat, runs = 1L, periods = 1L)
  expect_equal(length(unique(sim$chain_id)), 1L)

  A <- f$dat$depvars[[1]]
  B0 <- A[, , 1]; storage.mode(B0) <- "integer"
  set.seed(7)
  obs <- searchnet_chain_stats(
    data.frame(a = sample.int(f$M, 10, replace = TRUE),
               b = sample.int(f$N, 10, replace = TRUE)),
    B0 = B0)
  expect_error(searchnet_chain_compare(sim, obs), "sample of size one")
})
