###############################################################################
## test-chain-stats.R
## Unit tests for R/searchnet-chain-stats.R
##
## The load-bearing object here is `.searchnet_three_paths()`. Every event-level
## clustering number in the simulated-vs-observed comparison is one call to it,
## and it is a hand-derived inclusion-exclusion correction on a matrix product
## rather than an enumeration -- exactly the shape of thing that is off by a
## degenerate term and still looks plausible. So it is tested twice, by two
## methods that share no code: against four-cycle counts worked out by hand on
## tiny incidence matrices, and against an explicit nested-loop enumeration of
## the definition on random matrices at several densities and shapes.
##
## Nothing in this file estimates an RSiena model. The replay, the counters and
## the comparison are all deterministic given the fixtures, and the only
## randomness (the brute-force cross-check matrices) is seeded.
###############################################################################

## helper-setup.R sources every file in R/ via inst/saomnk-loader.R. Re-source
## the one file under test if that did not reach it, so a failure here is a
## failure of the code and not of the harness.
if (!exists("searchnet_chain_stats", mode = "function") &&
    exists("dir_r") &&
    file.exists(file.path(dir_r, "searchnet-chain-stats.R"))) {
  source(file.path(dir_r, "searchnet-chain-stats.R"), local = FALSE)
}


## ---------------------------------------------------------------------------
## Independent reference implementation.
##
## This is the DEFINITION, transcribed: the number of paths i -> m' -> i' -> m
## with i' != i and m' != m. It shares nothing with the implementation -- no
## matrix product, no correction terms -- which is the whole point. It is O(MN)
## per call with a double loop, so it is only ever used on small fixtures.
## ---------------------------------------------------------------------------
bf_three_paths <- function(B, i, m) {
  M <- nrow(B); N <- ncol(B)
  n <- 0
  for (ip in seq_len(M)) {
    if (ip == i) next
    for (mp in seq_len(N)) {
      if (mp == m) next
      n <- n + B[i, mp] * B[ip, mp] * B[ip, m]
    }
  }
  n
}

## Compare the two implementations on EVERY (i, m) cell of a matrix.
expect_three_paths_agree <- function(B, label) {
  fast <- brute <- matrix(NA_real_, nrow(B), ncol(B))
  for (i in seq_len(nrow(B))) {
    for (m in seq_len(ncol(B))) {
      fast[i, m]  <- .searchnet_three_paths(B, i, m)
      brute[i, m] <- bf_three_paths(B, i, m)
    }
  }
  expect_equal(fast, brute, info = label)
}


# ===========================================================================
# 1. .searchnet_three_paths(): hand-computed four-cycle counts
# ===========================================================================

test_that("an empty network has no three-paths anywhere", {
  B <- matrix(0L, 4, 5)
  for (i in 1:4) for (m in 1:5) {
    expect_equal(.searchnet_three_paths(B, i, m), 0,
                 info = sprintf("empty network, (i=%d, m=%d)", i, m))
  }
})

test_that("a single tie has no three-paths anywhere", {
  ## A three-path needs three ties; one tie cannot make one, whether the query
  ## is the tie itself (where the degenerate corrections fire) or another cell.
  B <- matrix(0L, 3, 3)
  B[2, 2] <- 1L
  for (i in 1:3) for (m in 1:3) {
    expect_equal(.searchnet_three_paths(B, i, m), 0,
                 info = sprintf("single tie at (2,2), query (i=%d, m=%d)", i, m))
  }
})

test_that("the minimal four-cycle configuration returns exactly 1", {
  ## Actors {1,2}, components {1,2}; ties (1,2), (2,2), (2,1) present, (1,1)
  ## absent. The one path 1 -> 2' -> 2 -> 1 closes into a four-cycle when
  ## (1,1) is added, and there is no other.
  B <- rbind(c(0L, 1L),
             c(1L, 1L))
  expect_equal(.searchnet_three_paths(B, 1, 1), 1)

  ## Every other cell is a tie, and each sits in zero completed cycles here.
  expect_equal(.searchnet_three_paths(B, 1, 2), 0)
  expect_equal(.searchnet_three_paths(B, 2, 1), 0)
  expect_equal(.searchnet_three_paths(B, 2, 2), 0)

  ## Removing any one of the three supporting ties destroys the path.
  for (cell in list(c(1, 2), c(2, 2), c(2, 1))) {
    Bm <- B; Bm[cell[1], cell[2]] <- 0L
    expect_equal(.searchnet_three_paths(Bm, 1, 1), 0,
                 info = sprintf("tie (%d,%d) removed", cell[1], cell[2]))
  }
})

test_that("an existing tie makes the degenerate corrections fire", {
  ## The i' == i and m' == m terms are non-zero only when B[i, m] == 1. Both
  ## fixtures below query a PRESENT tie, so the uncorrected matrix product
  ## overcounts and the arithmetic of the correction is what is on trial.

  ## (a) 2x2 complete. Uncorrected product = 4; true count = 1.
  B <- matrix(1L, 2, 2)
  expect_equal(.searchnet_three_paths(B, 1, 1), 1)
  expect_equal(bf_three_paths(B, 1, 1), 1)

  ## (b) 3x4, query the present tie (1,1). By hand: N(1) = {1,2,4}, so
  ##     m' in {2,4}; only i' = 2 with m' = 2 completes, since B[2,1] = 1 and
  ##     B[2,2] = 1. Actor 3 is not tied to component 1 at all.
  B2 <- rbind(c(1L, 1L, 0L, 1L),
              c(1L, 1L, 1L, 0L),
              c(0L, 1L, 1L, 1L))
  expect_equal(.searchnet_three_paths(B2, 1, 1), 1)

  ## The uncorrected product really is 5 here, so the -d_i -d_m +1 correction
  ## is carrying 4 units. If it were dropped the test above would read 5.
  co <- as.vector(B2[1, , drop = FALSE] %*% t(B2))
  expect_equal(sum(co * B2[, 1]), 5)
})

test_that("a rectangular network is not transposed", {
  ## M = 2, N = 3. A transposition would either error on the bounds or silently
  ## answer a different question; the hand counts below pin it down.
  B <- rbind(c(1L, 1L, 0L),
             c(0L, 1L, 1L))
  ## (1,3) absent: 1 -> 2 -> 2 -> 3 is the only path.
  expect_equal(.searchnet_three_paths(B, 1, 3), 1)
  ## (2,1) absent: 2 -> 2 -> 1 -> 1 is the only path.
  expect_equal(.searchnet_three_paths(B, 2, 1), 1)
  ## Present ties in this configuration sit in no completed cycle.
  expect_equal(.searchnet_three_paths(B, 1, 2), 0)
  expect_equal(.searchnet_three_paths(B, 2, 2), 0)

  ## And the same matrix transposed answers the transposed question.
  expect_equal(.searchnet_three_paths(t(B), 3, 1), 1)
  expect_equal(.searchnet_three_paths(t(B), 1, 2), 1)
})

test_that("a hand-worked 3x4 example matches on every cell", {
  B <- rbind(c(1L, 1L, 0L, 0L),
             c(1L, 0L, 1L, 0L),
             c(0L, 1L, 1L, 0L))
  ## (1,3): actor 2 shares component 1 with actor 1 and holds 3; actor 3 shares
  ## component 2 with actor 1 and holds 3. Two paths.
  expect_equal(.searchnet_three_paths(B, 1, 3), 2)
  ## (2,2): actor 1 via component 1, actor 3 via component 3. Two paths.
  expect_equal(.searchnet_three_paths(B, 2, 2), 2)
  ## (3,1): actor 1 via component 2, actor 2 via component 3. Two paths.
  expect_equal(.searchnet_three_paths(B, 3, 1), 2)
  ## Component 4 is isolated, so nothing reaches it.
  expect_equal(.searchnet_three_paths(B, 1, 4), 0)
  expect_three_paths_agree(B, "hand-worked 3x4")
})


# ===========================================================================
# 2. .searchnet_three_paths(): brute-force cross-check
#
# The highest-value test in the file: the fast path and an explicit enumeration
# of the definition must agree on every cell of every matrix, across densities
# (where the degenerate corrections fire on a varying fraction of cells) and
# across shapes (where a transposition would show up).
# ===========================================================================

test_that("the fast path agrees with brute force on random matrices", {
  set.seed(20260823)
  shapes <- list(c(2, 2), c(3, 3), c(4, 6), c(6, 4), c(5, 5), c(2, 9), c(9, 2),
                 c(7, 7), c(8, 3), c(3, 8))
  densities <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)

  for (sh in shapes) {
    for (p in densities) {
      M <- sh[1]; N <- sh[2]
      B <- matrix(as.integer(stats::runif(M * N) < p), M, N)
      expect_three_paths_agree(
        B, sprintf("random %dx%d at density %.2f (realised %.2f)",
                   M, N, p, mean(B)))
    }
  }
})

test_that("the fast path agrees with brute force on adversarial structures", {
  ## Structures chosen because they stress a particular term: a complete
  ## bipartite graph maximises both degenerate corrections; a star gives one
  ## actor every component; a perfect matching gives co[i'] = 0 off-diagonal.
  cases <- list(
    complete_4x4  = matrix(1L, 4, 4),
    complete_2x7  = matrix(1L, 2, 7),
    star_actor    = { B <- matrix(0L, 4, 5); B[1, ] <- 1L; B },
    star_comp     = { B <- matrix(0L, 4, 5); B[, 1] <- 1L; B },
    matching      = diag(5) * 1L,
    matching_rect = { B <- matrix(0L, 3, 6); B[cbind(1:3, 1:3)] <- 1L; B },
    two_blocks    = { B <- matrix(0L, 6, 6); B[1:3, 1:3] <- 1L
                      B[4:6, 4:6] <- 1L; B },
    one_row_empty = { B <- matrix(1L, 4, 4); B[3, ] <- 0L; B },
    one_col_empty = { B <- matrix(1L, 4, 4); B[, 3] <- 0L; B }
  )
  for (nm in names(cases)) expect_three_paths_agree(cases[[nm]], nm)
})

test_that("the count is symmetric under swapping the two modes", {
  ## Paths i -> m' -> i' -> m in B correspond one-for-one to paths
  ## m -> i' -> m' -> i in t(B), so the two must agree cell by cell. This
  ## catches an i/m mix-up that a square fixture would hide.
  set.seed(4321)
  for (rep in 1:20) {
    M <- sample(2:6, 1); N <- sample(2:6, 1)
    B <- matrix(as.integer(stats::runif(M * N) < 0.45), M, N)
    for (i in seq_len(M)) for (m in seq_len(N)) {
      expect_equal(.searchnet_three_paths(B, i, m),
                   .searchnet_three_paths(t(B), m, i),
                   info = sprintf("rep %d, %dx%d, (i=%d, m=%d)",
                                  rep, M, N, i, m))
    }
  }
})


# ===========================================================================
# 3. .searchnet_replay(): state advance, tie_before, change
# ===========================================================================

test_that("replay advances the state to the hand-toggled matrix", {
  ## REGRESSION TEST. This caught a real bug, now fixed.
  ##
  ## .searchnet_replay() used to end with
  ##     attr(out, "B_final") <- B
  ##     out[, .SEARCHNET_CHAIN_STAT_COLS]
  ## and `[.data.frame` with a column index drops non-standard attributes, so
  ## the attribute was set and thrown away on the very next line. The subset is
  ## a no-op on content -- `out` is already built with exactly those columns in
  ## exactly that order -- so its ONLY observable effect was to destroy
  ## B_final, which came back NULL for every input.
  ##
  ## The source now reorders first and stamps second. Keep this assertion: the
  ## final state is the only handle a caller has on where the chain ended up,
  ## and the failure mode was silent.
  B0 <- matrix(0L, 2, 2)
  ev <- cbind(c(1L, 1L, 2L, 1L, 2L),
              c(1L, 2L, 1L, 1L, 1L))
  out <- .searchnet_replay(ev, B0)

  ## By hand: (1,1) created at e1 and deleted at e4 -> 0; (1,2) created -> 1;
  ## (2,1) created at e3 and deleted at e5 -> 0.
  expect_equal(attr(out, "B_final"),
               matrix(c(0L, 0L, 1L, 0L), 2, 2))
})

test_that("tie_before and change are read off the pre-event state", {
  B0 <- matrix(0L, 2, 2)
  ev <- cbind(c(1L, 1L, 2L, 1L, 2L),
              c(1L, 2L, 1L, 1L, 1L))
  out <- .searchnet_replay(ev, B0)

  expect_equal(out$tie_before, c(0L, 0L, 0L, 1L, 1L))
  expect_equal(out$change,
               c("create", "create", "create", "delete", "delete"))
})

test_that("create-then-delete of the same pair round-trips", {
  ## Three events on one pair. `tie_before` on the third is the state check:
  ## it can only be 0 if the delete at e2 actually landed.
  B0 <- matrix(0L, 3, 3)
  out <- .searchnet_replay(cbind(rep(2L, 3), rep(3L, 3)), B0)
  expect_equal(out$tie_before, c(0L, 1L, 0L))
  expect_equal(out$change, c("create", "delete", "create"))
  ## Each event is a repeat of the same pair, so focusing ticks every time.
  expect_equal(out$focusing, c(0, 1, 2))
  ## Degrees follow the toggling state rather than the event count.
  expect_equal(out$actor_degree, c(0, 1, 0))
  expect_equal(out$component_degree, c(0, 1, 0))
})

test_that("replay starting from a non-empty B0 deletes rather than creates", {
  B0 <- matrix(1L, 2, 2)
  out <- .searchnet_replay(cbind(rep(1L, 3), rep(1L, 3)), B0)
  expect_equal(out$tie_before, c(1L, 0L, 1L))
  expect_equal(out$change, c("delete", "create", "delete"))
  ## Actor 1 starts holding both components, drops one, takes it back.
  expect_equal(out$actor_degree, c(2, 1, 2))
})

test_that("replay returns exactly the fixed column set, in order", {
  out <- .searchnet_replay(cbind(1L, 1L), matrix(0L, 2, 2))
  expect_identical(names(out), .SEARCHNET_CHAIN_STAT_COLS)
  expect_equal(nrow(out), 1L)
  expect_equal(out$event_id, 1L)
  expect_equal(out$source, "observed")
  expect_equal(out$chain_id, 1L)
})

test_that("replay rejects event indices outside the incidence matrix", {
  B0 <- matrix(0L, 2, 3)
  expect_error(.searchnet_replay(cbind(3L, 1L), B0), "out of range")
  expect_error(.searchnet_replay(cbind(1L, 4L), B0), "out of range")
  expect_error(.searchnet_replay(cbind(0L, 1L), B0), "out of range")
})

test_that("replay of an empty event log is an empty frame, not an error", {
  out <- .searchnet_replay(matrix(integer(0), nrow = 0, ncol = 2),
                           matrix(0L, 2, 2))
  expect_equal(nrow(out), 0L)
  expect_identical(names(out), .SEARCHNET_CHAIN_STAT_COLS)
})


# ===========================================================================
# 4. .searchnet_replay(): the running counters count PRIOR events only
# ===========================================================================

test_that("focusing, reinforcing and activity count strictly prior events", {
  ## Same five-event fixture, worked through by hand:
  ##   e1 (1,1): nothing has happened          -> f=0 r=0 a=0
  ##   e2 (1,2): actor 1 has acted once        -> f=0 r=0 a=1
  ##   e3 (2,1): component 1 has been hit once -> f=0 r=1 a=0
  ##   e4 (1,1): pair seen once, comp 1 twice, actor 1 twice -> f=1 r=2 a=2
  ##   e5 (2,1): pair seen once, comp 1 three times, actor 2 once -> f=1 r=3 a=1
  B0 <- matrix(0L, 2, 2)
  ev <- cbind(c(1L, 1L, 2L, 1L, 2L),
              c(1L, 2L, 1L, 1L, 1L))
  out <- .searchnet_replay(ev, B0)

  expect_equal(out$focusing,    c(0, 0, 0, 1, 1))
  expect_equal(out$reinforcing, c(0, 0, 1, 2, 3))
  expect_equal(out$activity,    c(0, 1, 0, 2, 1))
})

test_that("the first event on a pair always has focusing == 0", {
  set.seed(99)
  M <- 4; N <- 5
  ev <- cbind(sample.int(M, 60, replace = TRUE),
              sample.int(N, 60, replace = TRUE))
  out <- .searchnet_replay(ev, matrix(0L, M, N))
  key <- paste(ev[, 1], ev[, 2], sep = "-")
  first <- !duplicated(key)
  expect_true(all(out$focusing[first] == 0))
  ## and every repeat has focusing equal to how many times the pair came before
  expect_equal(out$focusing, as.numeric(ave(seq_along(key), key,
                                            FUN = seq_along) - 1L))
})

test_that("mixing is exactly activity times reinforcing", {
  set.seed(2468)
  M <- 5; N <- 7
  ev <- cbind(sample.int(M, 200, replace = TRUE),
              sample.int(N, 200, replace = TRUE))
  out <- .searchnet_replay(ev, matrix(0L, M, N))
  expect_identical(out$mixing, out$activity * out$reinforcing)
  ## Not vacuous: the product is non-zero somewhere.
  expect_true(any(out$mixing > 0))
})

test_that("degrees and clustering are evaluated before the event", {
  ## Ties are laid down so the four-cycle only closes on the last event; the
  ## pre-event evaluation means clustering is 1 there and 0 everywhere before.
  B0 <- matrix(0L, 2, 2)
  ev <- cbind(c(1L, 2L, 2L, 1L),
              c(2L, 2L, 1L, 1L))
  out <- .searchnet_replay(ev, B0)

  expect_equal(out$clustering, c(0, 0, 0, 1))
  ## Degrees are of the pre-event state too: at e4 actor 1 holds component 2
  ## only, and component 1 has just been taken by actor 2.
  expect_equal(out$actor_degree,     c(0, 0, 1, 1))
  expect_equal(out$component_degree, c(0, 1, 0, 1))
})

test_that("clustering during a replay equals a fresh count on the same state", {
  ## Cross-check the replay's incremental state against a from-scratch
  ## recomputation, so a state-update bug cannot hide behind the statistic.
  set.seed(1357)
  M <- 4; N <- 5
  ev <- cbind(sample.int(M, 40, replace = TRUE),
              sample.int(N, 40, replace = TRUE))
  out <- .searchnet_replay(ev, matrix(0L, M, N))

  B <- matrix(0L, M, N)
  expected <- numeric(nrow(ev))
  tie_before_ref <- integer(nrow(ev))
  for (e in seq_len(nrow(ev))) {
    expected[e]       <- bf_three_paths(B, ev[e, 1], ev[e, 2])
    tie_before_ref[e] <- B[ev[e, 1], ev[e, 2]]
    B[ev[e, 1], ev[e, 2]] <- 1L - B[ev[e, 1], ev[e, 2]]
  }
  ## Agreement over all 40 events is the state check: an incremental update
  ## that drifted from the from-scratch state would show up as a divergent
  ## clustering value at the first event after the drift.
  expect_equal(out$clustering, expected)
  ## Degrees, likewise recomputed from scratch alongside the toggle above.
  expect_equal(out$tie_before, tie_before_ref)
})


# ===========================================================================
# 5. searchnet_chain_stats() on an event log
# ===========================================================================

make_log_fixture <- function() {
  B0 <- matrix(0L, 3, 4,
               dimnames = list(c("alice", "bob", "carol"),
                               c("w", "x", "y", "z")))
  int_log <- data.frame(actor = c(1L, 2L, 2L, 3L, 1L),
                        comp  = c(1L, 1L, 2L, 2L, 2L),
                        stringsAsFactors = FALSE)
  chr_log <- data.frame(actor = c("alice", "bob", "bob", "carol", "alice"),
                        comp  = c("w", "w", "x", "x", "x"),
                        stringsAsFactors = FALSE)
  list(B0 = B0, int_log = int_log, chr_log = chr_log)
}

test_that("an integer event log replays and is labelled 'observed'", {
  f <- make_log_fixture()
  out <- searchnet_chain_stats(f$int_log, B0 = f$B0)

  expect_identical(names(out), .SEARCHNET_CHAIN_STAT_COLS)
  expect_equal(nrow(out), 5L)
  expect_true(all(out$source == "observed"))
  expect_true(all(out$chain_id == 1L))
  expect_equal(out$actor, c(1L, 2L, 2L, 3L, 1L))
  expect_equal(out$component, c(1L, 1L, 2L, 2L, 2L))
  expect_equal(out$change, rep("create", 5))
  ## By hand: at e5 actor 1 holds w, actor 2 holds w and x, so the path
  ## 1 -> w -> 2 -> x is the single three-path closing (alice, x).
  expect_equal(out$clustering, c(0, 0, 0, 0, 1))
})

test_that("character ids are mapped through the dimnames of B0", {
  f <- make_log_fixture()
  a <- searchnet_chain_stats(f$int_log, B0 = f$B0)
  b <- searchnet_chain_stats(f$chr_log, B0 = f$B0)
  ## Identical results: the character log names exactly the same cells.
  expect_equal(b$actor, a$actor)
  expect_equal(b$component, a$component)
  expect_equal(b$clustering, a$clustering)
  ## Compare the whole frame, not just the mapped columns, so a mapping that
  ## happened to land the right indices but a different state cannot pass.
  expect_equal(as.data.frame(b), as.data.frame(a))
})

test_that("factor ids are mapped like character ids, not by level number", {
  ## The trap: as.integer() on a factor gives level codes, which need not be
  ## the B0 index order. Reversing the levels must not change the answer.
  f <- make_log_fixture()
  fac_log <- data.frame(
    actor = factor(f$chr_log$actor, levels = c("carol", "bob", "alice")),
    comp  = factor(f$chr_log$comp,  levels = c("z", "y", "x", "w")),
    stringsAsFactors = FALSE)
  out <- searchnet_chain_stats(fac_log, B0 = f$B0)
  expect_equal(out$actor, c(1L, 2L, 2L, 3L, 1L))
  expect_equal(out$component, c(1L, 1L, 2L, 2L, 2L))
})

test_that("columns can be selected by name as well as by position", {
  f <- make_log_fixture()
  by_pos  <- searchnet_chain_stats(f$int_log, B0 = f$B0)
  by_name <- searchnet_chain_stats(f$int_log, B0 = f$B0,
                                   actor = "actor", component = "comp")
  expect_equal(by_name$actor, by_pos$actor)
  expect_equal(by_name$component, by_pos$component)
})

test_that("source and chain_id labels are carried through", {
  f <- make_log_fixture()
  out <- searchnet_chain_stats(f$int_log, B0 = f$B0,
                               source = "simulated", chain_id = 7L)
  expect_true(all(out$source == "simulated"))
  expect_true(all(out$chain_id == 7L))
  expect_type(out$chain_id, "integer")
})

test_that("a matrix event log is accepted as well as a data.frame", {
  f <- make_log_fixture()
  m <- as.matrix(f$int_log)
  expect_equal(searchnet_chain_stats(m, B0 = f$B0)$clustering,
               searchnet_chain_stats(f$int_log, B0 = f$B0)$clustering)
})

test_that("a missing B0 is an informative error, not a silent default", {
  f <- make_log_fixture()
  expect_error(searchnet_chain_stats(f$int_log),
               "`B0` is required for an event log")
  ## The message must say what to do about it.
  expect_error(searchnet_chain_stats(f$int_log), "matrix of zeros")
})

test_that("out-of-range integer ids are rejected with the matrix dimensions", {
  f <- make_log_fixture()
  bad_actor <- data.frame(actor = c(1L, 9L), comp = c(1L, 1L))
  bad_comp  <- data.frame(actor = c(1L, 1L), comp = c(1L, 9L))
  expect_error(searchnet_chain_stats(bad_actor, B0 = f$B0), "out of range")
  expect_error(searchnet_chain_stats(bad_comp,  B0 = f$B0), "out of range")
  expect_error(searchnet_chain_stats(bad_actor, B0 = f$B0), "3 x 4")
})

test_that("character ids absent from the dimnames are rejected", {
  f <- make_log_fixture()
  bad_a <- data.frame(actor = c("alice", "dave"), comp = c("w", "w"),
                      stringsAsFactors = FALSE)
  bad_c <- data.frame(actor = c("alice", "bob"), comp = c("w", "q"),
                      stringsAsFactors = FALSE)
  expect_error(searchnet_chain_stats(bad_a, B0 = f$B0),
               "absent from rownames(B0)", fixed = TRUE)
  expect_error(searchnet_chain_stats(bad_c, B0 = f$B0),
               "absent from colnames(B0)", fixed = TRUE)
})

test_that("character ids against an unnamed B0 are rejected", {
  f <- make_log_fixture()
  B0_bare <- matrix(0L, 3, 4)
  expect_error(searchnet_chain_stats(f$chr_log, B0 = B0_bare),
               "no rownames to map them onto", fixed = TRUE)
  ## Only colnames missing: actor ids map, component ids cannot.
  B0_rows <- B0_bare
  rownames(B0_rows) <- rownames(f$B0)
  expect_error(searchnet_chain_stats(f$chr_log, B0 = B0_rows),
               "no colnames to map", fixed = TRUE)
})

test_that("a column selector beyond the log is an informative error", {
  f <- make_log_fixture()
  expect_error(searchnet_chain_stats(f$int_log, B0 = f$B0, component = 5),
               "beyond the 2 columns")
  expect_error(searchnet_chain_stats(f$int_log, B0 = f$B0, actor = "firm"),
               "not found in `x`")
})

test_that("an unsupported `x` is rejected by type", {
  f <- make_log_fixture()
  expect_error(searchnet_chain_stats(list(1, 2), B0 = f$B0),
               "must be a `SaomNkRSienaBiEnv`", fixed = TRUE)
})


# ===========================================================================
# 6. searchnet_chain_compare()
# ===========================================================================

## Per-chain means of `clustering` equal to 1..k, one row per chain, so every
## quantile below can be checked against quantile(1:k) by hand.
make_sim_frame <- function(vals) {
  data.frame(chain_id    = seq_along(vals),
             clustering  = as.numeric(vals),
             focusing    = 0,
             reinforcing = 0,
             mixing      = 0,
             stringsAsFactors = FALSE)
}
make_obs_frame <- function(val) {
  data.frame(chain_id    = 1L,
             clustering  = as.numeric(val),
             focusing    = 0,
             reinforcing = 0,
             mixing      = 0,
             stringsAsFactors = FALSE)
}

test_that("a single simulated chain is refused outright", {
  sim <- make_sim_frame(3)
  obs <- make_obs_frame(3)
  expect_error(searchnet_chain_compare(sim, obs, stats = "clustering"),
               "sample of size one")
  ## The refusal is deliberate, and the message must say what to do instead.
  expect_error(searchnet_chain_compare(sim, obs, stats = "clustering"),
               "Simulate several")
  ## Many rows but one chain_id is still one chain.
  sim_many <- do.call(rbind, replicate(20, make_sim_frame(3), simplify = FALSE))
  expect_error(searchnet_chain_compare(sim_many, obs, stats = "clustering"),
               "only 1 simulated chain")
})

test_that("coverage, interval and z are computed from the per-chain summaries", {
  ## Per-chain means 1,2,3,4,5.  quantile(1:5, c(.025,.975)) = c(1.1, 4.9),
  ## mean 3, sd(1:5) = sqrt(2.5).
  sim <- make_sim_frame(1:5)
  obs <- make_obs_frame(3)
  res <- searchnet_chain_compare(sim, obs, stats = "clustering")

  expect_equal(nrow(res), 1L)
  expect_equal(res$statistic, "clustering")
  expect_equal(res$observed, 3)
  expect_equal(res$sim_mean, 3)
  expect_equal(res$sim_sd, sqrt(2.5))
  expect_equal(res$sim_lo, 1.1)
  expect_equal(res$sim_hi, 4.9)
  expect_true(res$covered)
  expect_equal(res$z, 0)
  expect_equal(res$n_chains, 5L)
  expect_false(res$fitted)
  expect_s3_class(res, "searchnet_chain_compare")
})

test_that("an observed value outside the percentile range is not covered", {
  sim <- make_sim_frame(1:5)
  ## 5 sits above the 97.5th percentile of 4.9, 1 sits below the 2.5th of 1.1.
  expect_false(searchnet_chain_compare(sim, make_obs_frame(5),
                                       stats = "clustering")$covered)
  expect_false(searchnet_chain_compare(sim, make_obs_frame(1),
                                       stats = "clustering")$covered)
  ## and the boundary values themselves are inside
  expect_true(searchnet_chain_compare(sim, make_obs_frame(1.1),
                                      stats = "clustering")$covered)
  expect_true(searchnet_chain_compare(sim, make_obs_frame(4.9),
                                      stats = "clustering")$covered)
  ## z is the standardised deviation from the simulated mean
  res <- searchnet_chain_compare(sim, make_obs_frame(5), stats = "clustering")
  expect_equal(res$z, (5 - 3) / sqrt(2.5))
})

test_that("z is NA rather than infinite when the chains do not vary", {
  sim <- make_sim_frame(rep(2, 4))
  res <- searchnet_chain_compare(sim, make_obs_frame(7), stats = "clustering")
  expect_equal(res$sim_sd, 0)
  expect_true(is.na(res$z))
})

test_that("`fun` summarises within a chain before the comparison", {
  ## Two chains, several rows each; with fun = mean the per-chain values are
  ## 2 and 8, with fun = max they are 3 and 9.
  ## normalize = FALSE isolates what this test is about (the within-chain
  ## summary) from the per-event normalisation tested separately below.
  sim <- data.frame(chain_id   = c(1L, 1L, 1L, 2L, 2L, 2L),
                    clustering = c(1, 2, 3, 7, 8, 9))
  obs <- data.frame(clustering = c(4, 6))
  r_mean <- suppressWarnings(
    searchnet_chain_compare(sim, obs, stats = "clustering", normalize = FALSE))
  expect_equal(r_mean$sim_mean, 5)
  expect_equal(r_mean$observed, 5)
  r_max <- suppressWarnings(
    searchnet_chain_compare(sim, obs, stats = "clustering", fun = max,
                            normalize = FALSE))
  expect_equal(r_max$sim_mean, 6)
  expect_equal(r_max$observed, 6)
})

test_that("normalize divides each summary by that chain's event count", {
  ## Chain 1: 3 events, mean 2 -> 2/3. Chain 2: 3 events, mean 8 -> 8/3.
  ## Simulated mean = (2/3 + 8/3)/2 = 5/3. Observed: 2 events, mean 5 -> 5/2.
  sim <- data.frame(chain_id   = c(1L, 1L, 1L, 2L, 2L, 2L),
                    clustering = c(1, 2, 3, 7, 8, 9))
  obs <- data.frame(clustering = c(4, 6))
  r <- suppressWarnings(
    searchnet_chain_compare(sim, obs, stats = "clustering", normalize = TRUE))
  expect_equal(r$sim_mean, 5 / 3)
  expect_equal(r$observed, 5 / 2)
  expect_true(r$normalized)
  expect_equal(r$n_ev_obs, 2L)
  expect_equal(r$n_ev_sim, 3)
})

test_that("normalize = FALSE warns when simulated and observed lengths differ", {
  ## These statistics are running counters, so their means scale with chain
  ## length. Comparing unnormalised means across differently-sized chains is
  ## partly a test of event counts, which is not the hypothesis.
  sim <- data.frame(chain_id   = rep(1:2, each = 10),
                    clustering = as.numeric(1:20))
  obs <- data.frame(clustering = c(1, 2))
  msgs <- character(0)
  withCallingHandlers(
    searchnet_chain_compare(sim, obs, stats = "clustering", normalize = FALSE),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_true(any(grepl("scale with chain length", msgs)))
})

test_that("p_mc follows the sienaGOF convention and is bounded by 1/(n+1)", {
  sim <- make_sim_frame(1:5)
  obs <- make_obs_frame(1000)          ## wildly extreme
  r <- suppressWarnings(
    searchnet_chain_compare(sim, obs, stats = "clustering"))
  ## 5 chains -> the smallest attainable p is 1/6, however extreme the value.
  expect_equal(r$p_mc, 1 / 6)
  expect_gte(r$p_mc, 1 / (r$n_chains + 1))
  expect_lte(r$p_mc, 1)
})

test_that("fewer than 20 chains warns about p_mc granularity", {
  sim <- make_sim_frame(1:5)
  msgs <- character(0)
  withCallingHandlers(
    searchnet_chain_compare(sim, make_obs_frame(3), stats = "clustering"),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_true(any(grepl("granularity", msgs)))
})

test_that("a list of chain frames is bound and renumbered", {
  sim_list <- lapply(1:4, function(k)
    data.frame(chain_id = 99L, clustering = as.numeric(k)))
  res <- searchnet_chain_compare(sim_list, make_obs_frame(2.5),
                                 stats = "clustering")
  ## The incoming chain_ids were all 99; the list position must override them,
  ## or all four chains would collapse into one and the function would refuse.
  expect_equal(res$n_chains, 4L)
  expect_equal(res$sim_mean, 2.5)
})

test_that("all four statistics are compared by default", {
  sim <- make_sim_frame(1:5)
  obs <- make_obs_frame(3)
  res <- searchnet_chain_compare(sim, obs)
  expect_equal(res$statistic, .SEARCHNET_REM_STATS)
  expect_equal(nrow(res), 4L)
})

test_that("missing statistic columns are named in the error", {
  sim <- make_sim_frame(1:5)
  obs <- make_obs_frame(3)
  expect_error(searchnet_chain_compare(sim, obs, stats = "activity"),
               "`simulated` lacks the column")
  sim2 <- cbind(sim, activity = 1)
  expect_error(searchnet_chain_compare(sim2, obs, stats = "activity"),
               "`observed` lacks the column")
})

test_that("fitting cycle4 flags clustering as circular and warns", {
  sim <- make_sim_frame(1:5)
  obs <- make_obs_frame(3)

  expect_warning(
    searchnet_chain_compare(sim, obs, stats = "clustering",
                            fitted_effects = c("density", "cycle4")),
    "cycle4")
  expect_warning(
    searchnet_chain_compare(sim, obs, stats = "clustering",
                            fitted_effects = "cycle4"),
    "not a free prediction")

  res <- suppressWarnings(
    searchnet_chain_compare(sim, obs, stats = "clustering",
                            fitted_effects = "cycle4"))
  expect_true(res$fitted)
  ## The flag must not disturb the numbers it annotates.
  expect_equal(res$sim_lo, 1.1)
  expect_equal(res$covered, TRUE)
})

test_that("only the statistics the model was fitted on are flagged", {
  sim <- make_sim_frame(1:5)
  obs <- make_obs_frame(3)
  res <- suppressWarnings(
    searchnet_chain_compare(sim, obs, fitted_effects = c("inPop", "cycle4")))
  expect_equal(res$fitted[res$statistic == "clustering"], TRUE)
  expect_equal(res$fitted[res$statistic == "reinforcing"], TRUE)
  expect_equal(res$fitted[res$statistic == "focusing"], FALSE)
  ## `mixing` IS flagged here: it is activity x reinforcing by construction, so
  ## it is targeted whenever either component effect is fitted, and `inPop` is.
  ## An earlier version mapped mixing to `inPopX`/`outActX`, which do not exist
  ## for bipartite dependent variables, so that guard could never fire.
  expect_equal(res$fitted[res$statistic == "mixing"], TRUE)
})

test_that("mixing is flagged via either of its component effects", {
  sim <- make_sim_frame(1:5); obs <- make_obs_frame(3)
  via_out <- suppressWarnings(
    searchnet_chain_compare(sim, obs, fitted_effects = "outActSqrt"))
  expect_true(via_out$fitted[via_out$statistic == "mixing"])
  via_in <- suppressWarnings(
    searchnet_chain_compare(sim, obs, fitted_effects = "inPop"))
  expect_true(via_in$fitted[via_in$statistic == "mixing"])
  ## and not flagged when neither component is fitted
  neither <- suppressWarnings(
    searchnet_chain_compare(sim, obs, fitted_effects = "density"))
  expect_false(neither$fitted[neither$statistic == "mixing"])
})

test_that("an unrelated fitted effect raises no warning and flags nothing", {
  sim <- make_sim_frame(1:5)
  obs <- make_obs_frame(3)
  warned <- character(0)
  res <- withCallingHandlers(
    searchnet_chain_compare(sim, obs, stats = "clustering",
                            fitted_effects = c("density", "outActSqrt")),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  ## Narrowed to the CIRCULARITY warning specifically. The small-n granularity
  ## warning is a separate concern and fires here regardless of fitted_effects.
  expect_length(grep("not free prediction|fitted on", warned), 0)
  expect_false(res$fitted)
})

test_that("print.searchnet_chain_compare reports the chains and the caveat", {
  sim <- make_sim_frame(1:5)
  obs <- make_obs_frame(3)
  res <- suppressWarnings(
    searchnet_chain_compare(sim, obs, stats = "clustering"))
  txt <- capture.output(print(res))
  expect_true(any(grepl("Chains: 5", txt)))
  ## The print method now steers the reader to p_mc and away from `covered`,
  ## because the percentile interval is anti-conservative at small n.
  expect_true(any(grepl("Report p_mc, not covered", txt)))
  expect_true(any(grepl("focusing", txt)))
  expect_true(any(grepl("clustering", txt)))

  res_f <- suppressWarnings(
    searchnet_chain_compare(sim, obs, stats = "clustering",
                            fitted_effects = "cycle4"))
  expect_true(any(grepl("fitted=TRUE", capture.output(print(res_f)))))
})

test_that("print returns its argument invisibly", {
  sim <- make_sim_frame(1:5)
  res <- searchnet_chain_compare(sim, make_obs_frame(3), stats = "clustering")
  vis <- NULL
  capture.output(vis <- withVisible(print(res)))
  expect_false(vis$visible)
  expect_identical(vis$value, res)
})


# ===========================================================================
# 7. End to end: a replayed log feeds the comparison unchanged
# ===========================================================================

test_that("chains built by searchnet_chain_stats() compare without reshaping", {
  ## The point of the fixed column set: simulated and observed frames rbind and
  ## compare with no alignment logic. Deterministic, seeded, no estimation.
  set.seed(864209)
  M <- 4; N <- 5
  B0 <- matrix(0L, M, N)

  obs <- searchnet_chain_stats(
    data.frame(a = sample.int(M, 30, replace = TRUE),
               b = sample.int(N, 30, replace = TRUE)),
    B0 = B0)

  sim <- do.call(rbind, lapply(1:6, function(k)
    searchnet_chain_stats(
      data.frame(a = sample.int(M, 30, replace = TRUE),
                 b = sample.int(N, 30, replace = TRUE)),
      B0 = B0, source = "simulated", chain_id = k)))

  expect_equal(sort(unique(sim$chain_id)), 1:6)
  res <- searchnet_chain_compare(sim, obs)
  expect_equal(nrow(res), 4L)
  expect_equal(res$n_chains, rep(6L, 4))
  expect_true(all(is.finite(res$observed)))
  expect_true(all(res$sim_lo <= res$sim_hi))
  ## rbind of a simulated and an observed frame needs no column alignment.
  expect_identical(names(sim), names(obs))
  expect_equal(nrow(rbind(sim, obs)), nrow(sim) + nrow(obs))
})
