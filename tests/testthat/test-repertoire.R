###############################################################################
## test-repertoire.R
## Unit tests for R/searchnet-repertoire.R
##
## The load-bearing object here is `.searchnet_silhouette()`. It is what chooses
## k when the caller does not, so every automatic typology in the package rests
## on its arithmetic, and it is a hand-rolled reimplementation of a definition
## that lives in another package -- exactly the shape of thing that is subtly
## wrong (a own-cluster self-distance left in the mean, a `b` taken over all
## clusters rather than the other ones, a singleton silently turned into NaN)
## and still returns plausible numbers. So it is tested twice, by two methods
## that share no code: against hand-computed values on tiny configurations, and
## against a from-the-definition reference implementation -- which builds its own
## distance matrix with an explicit double loop rather than reusing `dist()` --
## on many seeded random configurations of varying n, dimension and k.
##
## Everything below is deterministic given the seeds. Nothing here estimates an
## RSiena model, and `searchnet_repertoire_ri()` is exercised only on its error
## paths.
##
## NOT TESTED HERE: the documented `SaomNkRSienaBiEnv` input path of
## `searchnet_repertoire()` and `searchnet_repertoire_null()`. Reaching it needs
## a real simulation run with a ministep chain, which is the business of the
## simulation test files; the branch is one line
## (`chain_stats <- searchnet_chain_stats(chain_stats)`) and testing it here
## would buy a minute of runtime for no additional coverage of this file.
###############################################################################

## helper-setup.R sources every file in R/ via inst/saomnk-loader.R. Re-source
## the one file under test if that did not reach it, so a failure here is a
## failure of the code and not of the harness.
if (!exists("searchnet_repertoire", mode = "function") &&
    exists("dir_r") &&
    file.exists(file.path(dir_r, "searchnet-repertoire.R"))) {
  source(file.path(dir_r, "searchnet-repertoire.R"), local = FALSE)
}


## ---------------------------------------------------------------------------
## Independent reference implementation of the average silhouette width.
##
## Transcribed from the definition, and deliberately sharing nothing with the
## implementation: it builds the distance matrix itself from the raw coordinates
## with an explicit double loop, so `as.matrix(dist(X))` is on trial too, and it
## indexes own-cluster members with setdiff() rather than a compound logical.
##
## The two conventions the code fixes and the definition leaves open are matched
## explicitly, because a cross-check that quietly disagrees about them tests
## nothing:
##   * a point alone in its cluster contributes 0
##   * a point whose a and b are both 0 contributes 0 rather than NaN
## ---------------------------------------------------------------------------
bf_dist <- function(X) {
  X <- as.matrix(X)
  n <- nrow(X)
  D <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      D[i, j] <- sqrt(sum((X[i, ] - X[j, ])^2))
    }
  }
  D
}

bf_silhouette <- function(X, cl) {
  D  <- bf_dist(X)
  n  <- length(cl)
  ks <- unique(cl)
  if (length(ks) < 2L || n < 3L) return(NA_real_)
  s <- numeric(n)
  for (i in seq_len(n)) {
    own <- setdiff(which(cl == cl[i]), i)
    if (length(own) == 0L) { s[i] <- 0; next }
    a <- mean(D[i, own])
    b <- Inf
    for (k in ks) {
      if (k == cl[i]) next
      b <- min(b, mean(D[i, which(cl == k)]))
    }
    s[i] <- if (max(a, b) == 0) 0 else (b - a) / max(a, b)
  }
  mean(s)
}


## ---------------------------------------------------------------------------
## Fixture builders.
##
## `make_repertoire_stats()` gives every event of an actor the SAME statistic
## values, so the per-actor mean that `.searchnet_actor_profiles()` computes is
## exactly the value written in the spec. That is what makes the cluster-profile
## assertions hand-checkable rather than approximate.
## ---------------------------------------------------------------------------
make_repertoire_stats <- function(spec, n_events = 6L) {
  do.call(rbind, lapply(seq_len(nrow(spec)), function(r) {
    s <- spec[r, , drop = FALSE]
    n_create <- as.integer(round(s$create_share * n_events))
    data.frame(
      actor       = rep(s$actor, n_events),
      change      = c(rep("create", n_create),
                      rep("delete", n_events - n_create)),
      focusing    = rep(s$focusing,    n_events),
      reinforcing = rep(s$reinforcing, n_events),
      mixing      = rep(s$mixing,      n_events),
      clustering  = rep(s$clustering,  n_events),
      stringsAsFactors = FALSE)
  }))
}

## Two clean groups, of DIFFERENT sizes so the profile rows can be identified by
## `size` without relying on k-means' arbitrary cluster numbering.
##   actors 1,2,3 -> features 0, 1, 2      (mean 1),   create_share 1
##   actors 4,5   -> features 100, 102     (mean 101), create_share 0
two_group_spec <- data.frame(
  actor        = 1:5,
  focusing     = c(0, 1, 2, 100, 102),
  reinforcing  = c(0, 1, 2, 100, 102),
  mixing       = c(0, 1, 2, 100, 102),
  clustering   = c(0, 1, 2, 100, 102),
  create_share = c(1, 1, 1, 0, 0),
  stringsAsFactors = FALSE
)
two_group_stats <- function(n_events = 6L)
  make_repertoire_stats(two_group_spec, n_events = n_events)


# ===========================================================================
# 1. .searchnet_silhouette(): hand-computed values and the stated conventions
# ===========================================================================

test_that("two tight, well-separated clusters give a silhouette near 1", {
  ## Within-cluster spread 1e-6, between-cluster distance 100. Worked through:
  ## the six a values are 1.5, 1, 1.5 (and the mirror image) times 1e-6 and the
  ## b values are all 100 to eight figures, so the mean silhouette is
  ## 1 - (1/3)(1.5 + 1 + 1.5) x 1e-8 = 1 - 1.333e-8. The bound below is a real
  ## bound on that arithmetic, not a loose "close to 1".
  X <- matrix(c(0, 1e-6, 2e-6, 100, 100 + 1e-6, 100 + 2e-6), ncol = 1)
  cl <- c(1L, 1L, 1L, 2L, 2L, 2L)
  s <- .searchnet_silhouette(stats::dist(X), cl)
  expect_equal(1 - s, (4 / 3) * 1e-8, tolerance = 1e-4)
  expect_true(s > 1 - 2e-8)
  expect_true(s <= 1)
  expect_equal(s, bf_silhouette(X, cl))

  ## Loosening the clusters by three orders of magnitude moves the answer by
  ## exactly three orders of magnitude away from 1, which is what says the
  ## quantity is tracking the ratio rather than saturating.
  Y <- matrix(c(0, 1e-3, 2e-3, 100, 100.001, 100.002), ncol = 1)
  expect_equal(.searchnet_silhouette(stats::dist(Y), cl), bf_silhouette(Y, cl))
  expect_true(1 - .searchnet_silhouette(stats::dist(Y), cl) >
                100 * (1 - s))
})

test_that("a deliberately interleaved partition gives a negative silhouette", {
  ## Not a smoke test: the sign is the whole diagnostic value of the quantity,
  ## and a (b - a) written the wrong way round would show up here and nowhere
  ## else in this file.
  X <- matrix(c(0, 0.1, 10, 10.1), ncol = 1)
  cl <- c(1L, 2L, 1L, 2L)          ## each cluster spans both tight groups
  s <- .searchnet_silhouette(stats::dist(X), cl)
  expect_true(s < 0)
  expect_equal(s, bf_silhouette(X, cl))
})

test_that("a singleton cluster contributes 0 to the mean, by the code's own convention", {
  ## Points 0, 1, 10 with clusters {1,2} and {3}. By hand:
  ##   i=1: a = 1,  b = 10 -> (10 - 1)/10 = 9/10
  ##   i=2: a = 1,  b = 9  -> (9 - 1)/9   = 8/9
  ##   i=3: alone in its cluster          -> 0
  ##   mean = (9/10 + 8/9 + 0)/3 = 161/270
  X <- matrix(c(0, 1, 10), ncol = 1)
  cl <- c(1L, 1L, 2L)
  expect_equal(.searchnet_silhouette(stats::dist(X), cl), 161 / 270)

  ## The singleton is averaged IN, not dropped. Had it been excluded from the
  ## mean the answer would be (9/10 + 8/9)/2 = 161/180, so this assertion
  ## distinguishes the two conventions rather than merely being consistent with
  ## the one in force.
  expect_false(isTRUE(all.equal(.searchnet_silhouette(stats::dist(X), cl),
                                161 / 180)))
})

test_that("fewer than two clusters is NA_real_, not 0 and not an error", {
  X <- matrix(c(0, 1, 2, 3, 4), ncol = 1)
  expect_identical(.searchnet_silhouette(stats::dist(X), rep(1L, 5)), NA_real_)
  ## A single cluster label that happens not to be 1 behaves the same way.
  expect_identical(.searchnet_silhouette(stats::dist(X), rep(7L, 5)), NA_real_)
})

test_that("fewer than three points is NA_real_ even with two clusters", {
  X <- matrix(c(0, 5), ncol = 1)
  expect_identical(.searchnet_silhouette(stats::dist(X), c(1L, 2L)), NA_real_)
  ## One point, one cluster: both guards fire.
  expect_identical(.searchnet_silhouette(stats::dist(matrix(0, 1, 1)), 1L),
                   NA_real_)
})

test_that("identical points give 0 rather than NaN", {
  ## Every distance is 0, so a and b are both 0 and the naive (b - a)/max(a, b)
  ## is 0/0. The guard must turn that into 0, because a single NaN poisons the
  ## mean and would make the k-selection path choose k by accident.
  X <- matrix(rep(1, 12), ncol = 2)
  cl <- c(1L, 1L, 1L, 2L, 2L, 2L)
  s <- .searchnet_silhouette(stats::dist(X), cl)
  expect_false(is.nan(s))
  expect_false(is.na(s))
  expect_equal(s, 0)
  expect_equal(s, bf_silhouette(X, cl))
})

test_that("a cluster of coincident points has a = 0 and silhouette 1 there", {
  ## Half-degenerate: the own-cluster distances vanish but b does not, so the
  ## max(a, b) == 0 guard must NOT fire and those points must score exactly 1.
  X <- matrix(c(0, 0, 0, 10, 20), ncol = 1)
  cl <- c(1L, 1L, 1L, 2L, 2L)
  ## i = 1,2,3: a = 0, b = mean(10, 20) = 15 -> 1 each.
  ## i = 4: a = 10, b = 10 -> 0.  i = 5: a = 10, b = 20 -> 1/2.
  expect_equal(.searchnet_silhouette(stats::dist(X), cl), (3 + 0 + 0.5) / 5)
  expect_equal(.searchnet_silhouette(stats::dist(X), cl), bf_silhouette(X, cl))
})

test_that("the value does not depend on how the clusters are labelled", {
  set.seed(11223)
  X <- matrix(stats::rnorm(30), ncol = 3)
  cl <- c(1L, 1L, 2L, 2L, 3L, 3L, 1L, 3L, 2L, 1L)
  base <- .searchnet_silhouette(stats::dist(X), cl)
  ## Arbitrary, non-contiguous, out-of-order labels for the SAME partition. A
  ## `for (k in seq_len(max(cl)))` anywhere inside would break on these.
  cl2 <- c(9L, 4L, 40L)[cl]
  expect_equal(.searchnet_silhouette(stats::dist(X), cl2), base)
  cl3 <- c(-2L, 0L, 5L)[cl]
  expect_equal(.searchnet_silhouette(stats::dist(X), cl3), base)
  ## And as character labels, where setdiff()/unique() take a different path.
  expect_equal(.searchnet_silhouette(stats::dist(X), as.character(cl)), base)
})

test_that("a full distance matrix is accepted as well as a dist object", {
  set.seed(4477)
  X <- matrix(stats::rnorm(24), ncol = 2)
  cl <- rep(1:3, each = 4)
  expect_equal(.searchnet_silhouette(stats::dist(X), cl),
               .searchnet_silhouette(as.matrix(stats::dist(X)), cl))
})


# ===========================================================================
# 2. .searchnet_silhouette(): brute-force cross-check
#
# The highest-value test in the file. The fast path and a from-the-definition
# reference must agree on every seeded random configuration, across point
# counts, dimensions, cluster counts and cluster-label vocabularies -- including
# configurations that contain singletons, where the two implementations have to
# agree about a convention rather than about arithmetic.
# ===========================================================================

test_that("the fast path agrees with a from-the-definition reference", {
  set.seed(20260823)
  n_cases <- 0L
  n_singleton <- 0L
  for (n in c(3, 4, 5, 7, 10, 14, 20, 25)) {
    for (p in c(1, 2, 3, 5)) {
      for (k in 2:min(6, n - 1)) {
        for (rep in 1:3) {
          X <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
          ## Free assignment, so cluster sizes are uneven and singletons occur.
          cl <- sample.int(k, n, replace = TRUE)
          if (length(unique(cl)) < 2L) next
          if (any(table(cl) == 1L)) n_singleton <- n_singleton + 1L
          n_cases <- n_cases + 1L
          expect_equal(
            .searchnet_silhouette(stats::dist(X), cl),
            bf_silhouette(X, cl),
            info = sprintf("n=%d p=%d k=%d rep=%d", n, p, k, rep))
        }
      }
    }
  }
  ## The sweep is not vacuous, and it really did exercise the singleton branch.
  expect_gt(n_cases, 200)
  expect_gt(n_singleton, 20)
})

test_that("the reference agrees on tightly clustered and heavily tied data", {
  ## Random normal points almost never produce equal distances. These do: coarse
  ## integer grids give ties, duplicated rows give zero distances, and both are
  ## where a max(a, b) guard or a min() over other clusters can misbehave.
  set.seed(778899)
  for (rep in 1:40) {
    n <- sample(3:16, 1)
    p <- sample(1:3, 1)
    X <- matrix(sample(0:2, n * p, replace = TRUE), nrow = n, ncol = p)
    k <- sample(2:max(2, min(5, n - 1)), 1)
    cl <- sample.int(k, n, replace = TRUE)
    if (length(unique(cl)) < 2L) next
    s <- .searchnet_silhouette(stats::dist(X), cl)
    expect_false(is.nan(s), info = sprintf("rep %d produced NaN", rep))
    expect_equal(s, bf_silhouette(X, cl), info = sprintf("rep %d", rep))
  }
})

test_that("the silhouette stays inside [-1, 1] over random configurations", {
  set.seed(31415)
  for (rep in 1:60) {
    n <- sample(3:20, 1)
    p <- sample(1:4, 1)
    X <- matrix(stats::rnorm(n * p) * sample(c(0.01, 1, 100), 1),
                nrow = n, ncol = p)
    k <- sample(2:max(2, min(6, n - 1)), 1)
    cl <- sample.int(k, n, replace = TRUE)
    s <- .searchnet_silhouette(stats::dist(X), cl)
    if (is.na(s)) next
    expect_true(s >= -1 && s <= 1, info = sprintf("rep %d gave %s", rep, s))
  }
})

test_that("the silhouette is invariant to a rigid rescaling of the space", {
  ## Multiplying every coordinate by c multiplies a and b by c and cancels in
  ## (b - a)/max(a, b). A stray additive term anywhere would break this.
  set.seed(2718)
  X <- matrix(stats::rnorm(40), ncol = 2)
  cl <- rep(1:4, each = 5)
  base <- .searchnet_silhouette(stats::dist(X), cl)
  for (cc in c(0.001, 0.5, 3, 1000)) {
    expect_equal(.searchnet_silhouette(stats::dist(X * cc), cl), base,
                 info = sprintf("scale factor %g", cc))
  }
})


# ===========================================================================
# 3. .searchnet_actor_profiles(): min_events filtering
# ===========================================================================

## Counts: actor 1 -> 3 events, actor 2 -> 2, actor 3 -> 4.
tiny_stats <- function() {
  data.frame(
    actor       = c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 3L),
    change      = c("create", "create", "delete",
                    "create", "delete",
                    "create", "create", "create", "delete"),
    focusing    = c(0, 1, 2,  0, 1,  0, 0, 1, 2),
    reinforcing = c(1, 2, 3,  4, 5,  6, 7, 8, 9),
    mixing      = c(0, 2, 6,  0, 4,  0, 0, 8, 18),
    clustering  = c(0, 0, 1,  2, 2,  0, 1, 1, 2),
    stringsAsFactors = FALSE)
}

test_that("an actor at exactly min_events is kept, one below it is dropped", {
  ap <- .searchnet_actor_profiles(tiny_stats(), min_events = 3L)
  ## Actor 1 has exactly 3 -- the boundary, and the one a `>` instead of `>=`
  ## would silently lose.
  expect_equal(ap$profiles$actor, c(1, 3))
  expect_equal(ap$profiles$n_events, c(3L, 4L))
  ## Actor 2, with 2 events, is reported rather than silently discarded.
  expect_equal(nrow(ap$dropped), 1L)
  expect_equal(ap$dropped$actor, 2L)
  expect_equal(ap$dropped$n_events, 2L)
})

test_that("lowering the threshold to the boundary keeps every actor", {
  ap <- .searchnet_actor_profiles(tiny_stats(), min_events = 2L)
  expect_equal(ap$profiles$actor, c(1, 2, 3))
  expect_equal(nrow(ap$dropped), 0L)
  ## An empty `dropped` is still a data.frame with the same two columns, so a
  ## caller can rbind or nrow() it without a special case.
  expect_s3_class(ap$dropped, "data.frame")
  expect_named(ap$dropped, c("actor", "n_events"))
})

test_that("every dropped actor is reported with its true event count", {
  ap <- .searchnet_actor_profiles(tiny_stats(), min_events = 4L)
  expect_equal(ap$profiles$actor, 3)
  expect_equal(ap$dropped$actor, c(1L, 2L))
  expect_equal(ap$dropped$n_events, c(3L, 2L))
  ## Nothing is lost: retained plus dropped accounts for every actor, and the
  ## event counts add back up to the number of rows in the input.
  expect_setequal(c(ap$profiles$actor, ap$dropped$actor),
                  unique(tiny_stats()$actor))
  expect_equal(sum(ap$profiles$n_events) + sum(ap$dropped$n_events),
               nrow(tiny_stats()))
})


# ===========================================================================
# 4. .searchnet_actor_profiles(): the aggregation itself
# ===========================================================================

test_that("each statistic aggregates to the per-actor mean", {
  ap <- .searchnet_actor_profiles(tiny_stats(), min_events = 3L)
  p <- ap$profiles
  ## Actor 1: focusing (0,1,2) -> 1; reinforcing (1,2,3) -> 2;
  ##          mixing (0,2,6) -> 8/3; clustering (0,0,1) -> 1/3.
  expect_equal(p$focusing[p$actor == 1],    1)
  expect_equal(p$reinforcing[p$actor == 1], 2)
  expect_equal(p$mixing[p$actor == 1],      8 / 3)
  expect_equal(p$clustering[p$actor == 1],  1 / 3)
  ## Actor 3: focusing (0,0,1,2) -> 0.75; reinforcing (6,7,8,9) -> 7.5;
  ##          mixing (0,0,8,18) -> 6.5; clustering (0,1,1,2) -> 1.
  expect_equal(p$focusing[p$actor == 3],    0.75)
  expect_equal(p$reinforcing[p$actor == 3], 7.5)
  expect_equal(p$mixing[p$actor == 3],      6.5)
  expect_equal(p$clustering[p$actor == 3],  1)
  ## The aggregate is a mean, not a sum: actor 3's reinforcing total is 30.
  expect_false(isTRUE(all.equal(p$reinforcing[p$actor == 3], 30)))
})

test_that("create_share is the proportion of events that create a tie", {
  ap <- .searchnet_actor_profiles(tiny_stats(), min_events = 2L)
  p <- ap$profiles
  expect_equal(p$create_share[p$actor == 1], 2 / 3)   ## 2 creates of 3
  expect_equal(p$create_share[p$actor == 2], 1 / 2)   ## 1 create of 2
  expect_equal(p$create_share[p$actor == 3], 3 / 4)   ## 3 creates of 4
  expect_true(all(p$create_share >= 0 & p$create_share <= 1))
})

test_that("create_share is 1 and 0 at the extremes, not a count", {
  cs <- tiny_stats()
  cs$change <- rep("create", nrow(cs))
  expect_true(all(.searchnet_actor_profiles(cs, min_events = 2L)$profiles$create_share == 1))
  cs$change <- rep("delete", nrow(cs))
  expect_true(all(.searchnet_actor_profiles(cs, min_events = 2L)$profiles$create_share == 0))
})

test_that("profiles carry exactly the expected columns", {
  ap <- .searchnet_actor_profiles(tiny_stats(), min_events = 2L)
  expect_named(ap$profiles,
               c("actor", "n_events", "focusing", "reinforcing", "mixing",
                 "clustering", "create_share"))
  expect_equal(rownames(ap$profiles), as.character(seq_len(3)))
})

test_that("non-contiguous actor ids keep their statistics attached to them", {
  ## The trap: split()/table() key on the character form of the id, so a naive
  ## implementation orders actors 2, 3, 10 as "10", "2", "3" and the profile
  ## rows end up attached to the wrong actors. Values are chosen so a
  ## lexicographic shuffle is visible.
  spec <- data.frame(actor = c(2L, 10L, 3L),
                     focusing = c(20, 100, 30), reinforcing = c(20, 100, 30),
                     mixing = c(20, 100, 30), clustering = c(20, 100, 30),
                     create_share = c(0, 0.5, 1), stringsAsFactors = FALSE)
  ap <- .searchnet_actor_profiles(make_repertoire_stats(spec, n_events = 6L),
                                  min_events = 5L)
  expect_equal(ap$profiles$actor, c(2, 3, 10))
  expect_equal(ap$profiles$focusing, c(20, 30, 100))
  expect_equal(ap$profiles$create_share, c(0, 1, 0.5))
})

test_that("profiles are computed from a real searchnet_chain_stats() frame", {
  ## The two files are meant to compose without reshaping. Replay a seeded event
  ## log through the actual producer and aggregate the result.
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  set.seed(515151)
  M <- 5; N <- 6
  log <- data.frame(a = sample.int(M, 60, replace = TRUE),
                    b = sample.int(N, 60, replace = TRUE))
  cs <- searchnet_chain_stats(log, B0 = matrix(0L, M, N))
  ap <- .searchnet_actor_profiles(cs, min_events = 5L)
  expect_equal(nrow(ap$profiles) + nrow(ap$dropped), M)
  expect_equal(sum(ap$profiles$n_events) + sum(ap$dropped$n_events), 60)
  ## Cross-check one actor's aggregate against a direct tapply.
  a1 <- ap$profiles$actor[1]
  expect_equal(ap$profiles$focusing[1], mean(cs$focusing[cs$actor == a1]))
  expect_equal(ap$profiles$create_share[1],
               mean(cs$change[cs$actor == a1] == "create"))
})


# ===========================================================================
# 5. .searchnet_actor_profiles(): errors
# ===========================================================================

test_that("a missing required column is named in the error", {
  for (col in c("actor", "change", "focusing", "reinforcing", "mixing",
                "clustering")) {
    cs <- tiny_stats()
    cs[[col]] <- NULL
    expect_error(.searchnet_actor_profiles(cs, min_events = 2L),
                 col, fixed = TRUE,
                 info = sprintf("dropping column '%s'", col))
  }
  ## Several missing at once are all named, and the message says where the frame
  ## was supposed to come from.
  cs <- tiny_stats()
  cs$mixing <- NULL; cs$clustering <- NULL
  expect_error(.searchnet_actor_profiles(cs, min_events = 2L),
               "mixing, clustering", fixed = TRUE)
  expect_error(.searchnet_actor_profiles(cs, min_events = 2L),
               "searchnet_chain_stats()", fixed = TRUE)
})

test_that("when no actor clears the threshold the busiest count is reported", {
  ## The busiest actor has 4 events, so a threshold of 5 must say "4" -- the
  ## number that tells the caller what to lower `min_events` to.
  expect_error(.searchnet_actor_profiles(tiny_stats(), min_events = 5L),
               "no actor has at least 5 events (the busiest has 4)",
               fixed = TRUE)
  expect_error(.searchnet_actor_profiles(tiny_stats(), min_events = 99L),
               "the busiest has 4", fixed = TRUE)
  ## And it says what to do about it.
  expect_error(.searchnet_actor_profiles(tiny_stats(), min_events = 5L),
               "Lower `min_events`", fixed = TRUE)
})


# ===========================================================================
# 6. searchnet_repertoire(): the basic contract
# ===========================================================================

test_that("a chain_stats data.frame with an explicit k gives a full object", {
  res <- searchnet_repertoire(two_group_stats(), k = 2, seed = 1)

  expect_s3_class(res, "searchnet_repertoire")
  expect_equal(res$k, 2L)
  expect_type(res$k, "integer")
  expect_equal(res$n_actors, 5L)
  expect_named(res$assignment, c("actor", "n_events", "repertoire"))
  expect_equal(res$assignment$actor, 1:5)
  expect_equal(res$assignment$n_events, rep(6L, 5))
  expect_true(all(res$assignment$repertoire %in% 1:2))
  expect_type(res$assignment$repertoire, "integer")
  expect_equal(res$features, .SEARCHNET_REPERTOIRE_FEATURES)
  ## `criterion` is the audit trail of an AUTOMATIC choice; with k given there
  ## was no choice to audit and it must be absent rather than fabricated.
  expect_null(res$criterion)
  expect_false(is.na(res$silhouette))
})

test_that("the two planted groups are recovered as the two repertoires", {
  res <- searchnet_repertoire(two_group_stats(), k = 2, seed = 1)
  g <- res$assignment$repertoire
  expect_equal(length(unique(g[1:3])), 1L)
  expect_equal(length(unique(g[4:5])), 1L)
  expect_false(g[1] == g[4])
  ## Cleanly separated groups: the silhouette should be high, not merely finite.
  expect_gt(res$silhouette, 0.7)
})

test_that("cluster profiles are on the ORIGINAL, unstandardised scale", {
  ## Group A (actors 1,2,3): every feature 0, 1, 2  -> mean 1;   create_share 1
  ## Group B (actors 4,5):   every feature 100, 102 -> mean 101; create_share 0
  ## Standardised centres would sit near +/-1, so these assertions distinguish
  ## the two scales rather than merely checking the arithmetic.
  res <- searchnet_repertoire(two_group_stats(), k = 2, seed = 1)
  prof <- res$profiles

  expect_named(prof, c("repertoire", "size", .SEARCHNET_REPERTOIRE_FEATURES))
  expect_equal(nrow(prof), 2L)
  ## Sizes 3 and 2 identify the rows without depending on k-means' numbering.
  a <- prof[prof$size == 3L, ]
  b <- prof[prof$size == 2L, ]
  expect_equal(nrow(a), 1L); expect_equal(nrow(b), 1L)

  expect_equal(a$focusing,     1);   expect_equal(b$focusing,     101)
  expect_equal(a$reinforcing,  1);   expect_equal(b$reinforcing,  101)
  expect_equal(a$mixing,       1);   expect_equal(b$mixing,       101)
  expect_equal(a$clustering,   1);   expect_equal(b$clustering,   101)
  expect_equal(a$create_share, 1);   expect_equal(b$create_share, 0)

  ## Every retained actor is in exactly one cluster.
  expect_equal(sum(prof$size), nrow(res$assignment))
  expect_equal(sum(prof$size), res$n_actors)
  expect_setequal(prof$repertoire, unique(res$assignment$repertoire))
  ## and `size` really is the count of that repertoire in `assignment`.
  for (i in seq_len(nrow(prof))) {
    expect_equal(prof$size[i],
                 sum(res$assignment$repertoire == prof$repertoire[i]))
  }
})

test_that("cluster profiles equal the mean of the member actors' own profiles", {
  ## A second, general statement of the same property: recompute each cluster
  ## mean from `.searchnet_actor_profiles()` output and require agreement.
  set.seed(6060)
  spec <- data.frame(actor = 1:12,
                     focusing     = stats::rnorm(12, 0, 3),
                     reinforcing  = stats::rnorm(12, 0, 3),
                     mixing       = stats::rnorm(12, 0, 3),
                     clustering   = stats::rnorm(12, 0, 3),
                     create_share = rep(c(0, 1/3, 2/3, 1), 3),
                     stringsAsFactors = FALSE)
  cs  <- make_repertoire_stats(spec, n_events = 6L)
  res <- searchnet_repertoire(cs, k = 3, seed = 42)
  ap  <- .searchnet_actor_profiles(cs, min_events = 5L)

  for (kk in sort(unique(res$assignment$repertoire))) {
    members <- res$assignment$actor[res$assignment$repertoire == kk]
    want <- colMeans(ap$profiles[ap$profiles$actor %in% members,
                                 res$features, drop = FALSE])
    got  <- unlist(res$profiles[res$profiles$repertoire == kk, res$features])
    expect_equal(got, want, ignore_attr = TRUE,
                 info = sprintf("repertoire %d", kk))
  }
})

test_that("min_events is honoured and short-lived actors land in $dropped", {
  cs <- rbind(two_group_stats(6L),
              make_repertoire_stats(
                data.frame(actor = 6L, focusing = 50, reinforcing = 50,
                           mixing = 50, clustering = 50, create_share = 1,
                           stringsAsFactors = FALSE),
                n_events = 2L))
  res <- searchnet_repertoire(cs, k = 2, seed = 1)
  expect_equal(res$n_actors, 5L)
  expect_equal(res$dropped$actor, 6L)
  expect_equal(res$dropped$n_events, 2L)
  expect_false(6L %in% res$assignment$actor)
  ## Lowering the threshold brings the actor back in.
  res2 <- searchnet_repertoire(cs, k = 2, min_events = 2L, seed = 1)
  expect_equal(res2$n_actors, 6L)
  expect_equal(nrow(res2$dropped), 0L)
})

test_that("a subset of features is used, and only that subset", {
  res <- searchnet_repertoire(two_group_stats(), k = 2,
                              features = c("focusing", "create_share"),
                              seed = 1)
  expect_equal(res$features, c("focusing", "create_share"))
  expect_named(res$profiles, c("repertoire", "size", "focusing",
                               "create_share"))
})

test_that("scale = FALSE clusters on the raw feature scale", {
  res <- searchnet_repertoire(two_group_stats(), k = 2, scale = FALSE, seed = 1)
  expect_s3_class(res, "searchnet_repertoire")
  expect_equal(res$k, 2L)
  expect_equal(nrow(res$assignment), 5L)
  ## The planted groups are separated on the raw scale too, and the profiles are
  ## still the original-scale means.
  expect_equal(sort(res$profiles$size), c(2L, 3L))
  expect_equal(res$profiles$focusing[res$profiles$size == 3L], 1)
  expect_equal(res$profiles$focusing[res$profiles$size == 2L], 101)
})

test_that("scale = TRUE and scale = FALSE are genuinely different computations", {
  ## Not a tautology: `mixing` is a product of two counts and dominates the raw
  ## distance, so a fixture where one feature has a much larger spread must be
  ## able to partition differently under the two settings. If these ever agree
  ## for every fixture, the `scale` argument is doing nothing.
  spec <- data.frame(
    actor        = 1:6,
    focusing     = c(0, 0, 0, 1, 1, 1),
    reinforcing  = c(0, 0, 0, 1, 1, 1),
    clustering   = c(0, 0, 0, 1, 1, 1),
    create_share = c(0, 0, 0, 1, 1, 1),
    mixing       = c(0, 500, 1000, 0, 500, 1000),
    stringsAsFactors = FALSE)
  cs <- make_repertoire_stats(spec, n_events = 6L)
  scaled <- searchnet_repertoire(cs, k = 2, scale = TRUE,  seed = 3)
  raw    <- searchnet_repertoire(cs, k = 2, scale = FALSE, seed = 3)
  ## Compare partitions up to relabelling, via the "same cluster?" relation.
  same <- function(g) outer(g, g, "==")
  expect_false(identical(same(scaled$assignment$repertoire),
                         same(raw$assignment$repertoire)))
})


# ===========================================================================
# 7. searchnet_repertoire(): automatic k selection
# ===========================================================================

test_that("automatic k returns a criterion path over the feasible k_range", {
  res <- searchnet_repertoire(two_group_stats(), k_range = 2:4, seed = 1)
  expect_s3_class(res$criterion, "data.frame")
  expect_named(res$criterion, c("k", "silhouette"))
  expect_equal(res$criterion$k, 2:4)
  expect_equal(nrow(res$criterion), 3L)
})

test_that("the chosen k is the argmax of the silhouette column", {
  ## Asserted as a relationship between the two returned fields rather than
  ## against a hard-coded k, so the test states the CONTRACT and cannot be
  ## satisfied by a k that merely happens to be right on this fixture.
  for (sd in c(1, 7, 99)) {
    res <- searchnet_repertoire(two_group_stats(), k_range = 2:4, seed = sd)
    expect_equal(res$k, res$criterion$k[which.max(res$criterion$silhouette)],
                 info = sprintf("seed %d", sd))
  }
  ## A larger, less trivial fixture: three planted groups over 12 actors.
  set.seed(808)
  grp <- rep(1:3, each = 4)
  spec <- data.frame(actor = 1:12,
                     focusing     = c(0, 10, 20)[grp] + stats::runif(12, -.2, .2),
                     reinforcing  = c(0, 10, 20)[grp] + stats::runif(12, -.2, .2),
                     mixing       = c(0, 10, 20)[grp] + stats::runif(12, -.2, .2),
                     clustering   = c(0, 10, 20)[grp] + stats::runif(12, -.2, .2),
                     create_share = c(0, 0.5, 1)[grp],
                     stringsAsFactors = FALSE)
  res <- searchnet_repertoire(make_repertoire_stats(spec, 6L),
                              k_range = 2:6, seed = 5)
  expect_equal(res$criterion$k, 2:6)
  expect_equal(res$k, res$criterion$k[which.max(res$criterion$silhouette)])
  ## With three cleanly planted groups the criterion should in fact pick 3.
  expect_equal(res$k, 3L)
})

test_that("k_range is filtered to values feasible for the actor count", {
  ## 5 actors: k must lie in 2..4, so 1 and 5..7 are dropped from the path
  ## rather than errored on or silently attempted.
  res <- searchnet_repertoire(two_group_stats(), k_range = 1:7, seed = 1)
  expect_equal(res$criterion$k, 2:4)
  expect_true(res$k >= 2L && res$k < 5L)
})

test_that("an entirely infeasible k_range is an informative error", {
  expect_error(searchnet_repertoire(two_group_stats(), k_range = 8:10),
               "no candidate k is usable", fixed = TRUE)
  ## The message must report the actor count and the usable interval.
  expect_error(searchnet_repertoire(two_group_stats(), k_range = 8:10),
               "5 actors have profiles, so k must lie in 2..4", fixed = TRUE)
  ## k_range = 1 alone is infeasible for the same reason (k >= 2 is required).
  expect_error(searchnet_repertoire(two_group_stats(), k_range = 1),
               "no candidate k is usable", fixed = TRUE)
})

test_that("k at or above the actor count is refused with both numbers", {
  expect_error(searchnet_repertoire(two_group_stats(), k = 5),
               "k = 5 but only 5 actors have profiles", fixed = TRUE)
  expect_error(searchnet_repertoire(two_group_stats(), k = 9),
               "k = 9 but only 5 actors have profiles", fixed = TRUE)
  ## k = n - 1 is the largest feasible value and must be allowed.
  expect_s3_class(searchnet_repertoire(two_group_stats(), k = 4, seed = 1),
                  "searchnet_repertoire")
})


# ===========================================================================
# 8. searchnet_repertoire(): degenerate features, unknown features, seeds
# ===========================================================================

test_that("a zero-variance feature is dropped with a warning naming it", {
  spec <- two_group_spec
  spec$clustering <- 7            ## constant across every actor
  cs <- make_repertoire_stats(spec, 6L)

  expect_warning(searchnet_repertoire(cs, k = 2, seed = 1), "clustering")
  expect_warning(searchnet_repertoire(cs, k = 2, seed = 1), "zero variance")

  res <- suppressWarnings(searchnet_repertoire(cs, k = 2, seed = 1))
  ## The run continues, and the dropped feature is gone from the recorded
  ## feature set and from the profiles -- not silently retained as NaN.
  expect_s3_class(res, "searchnet_repertoire")
  expect_equal(res$features,
               c("focusing", "reinforcing", "mixing", "create_share"))
  expect_false("clustering" %in% names(res$profiles))
  expect_equal(nrow(res$assignment), 5L)
  expect_false(is.na(res$silhouette))
})

test_that("two zero-variance features are both named in one warning", {
  spec <- two_group_spec
  spec$clustering <- 7
  spec$mixing     <- 3
  cs <- make_repertoire_stats(spec, 6L)
  expect_warning(searchnet_repertoire(cs, k = 2, seed = 1),
                 "mixing, clustering", fixed = TRUE)
  res <- suppressWarnings(searchnet_repertoire(cs, k = 2, seed = 1))
  expect_equal(res$features, c("focusing", "reinforcing", "create_share"))
})

test_that("all features zero-variance is an error, not an empty partition", {
  spec <- data.frame(actor = 1:5, focusing = 1, reinforcing = 1, mixing = 1,
                     clustering = 1, create_share = 1, stringsAsFactors = FALSE)
  cs <- make_repertoire_stats(spec, 6L)
  expect_error(suppressWarnings(searchnet_repertoire(cs, k = 2, seed = 1)),
               "every requested feature has zero variance", fixed = TRUE)
  expect_error(suppressWarnings(searchnet_repertoire(cs, k = 2, seed = 1)),
               "nothing to cluster", fixed = TRUE)
})

test_that("scale = FALSE reaches kmeans with constant features", {
  ## OBSERVATION, not a demand. The zero-variance guard lives inside
  ## `if (scale)`, so with scale = FALSE a fully constant feature set is passed
  ## straight to stats::kmeans() and the caller gets kmeans's message ("more
  ## cluster centers than distinct data points") instead of the guard's
  ## explanation. Recorded here so the asymmetry is visible in the test record;
  ## whether to lift the guard out of the `if` is the author's call.
  spec <- data.frame(actor = 1:5, focusing = 1, reinforcing = 1, mixing = 1,
                     clustering = 1, create_share = 1, stringsAsFactors = FALSE)
  cs <- make_repertoire_stats(spec, 6L)
  expect_error(searchnet_repertoire(cs, k = 2, scale = FALSE, seed = 1))
  ## and the message is NOT the searchnet one, which is the point of the note.
  msg <- tryCatch(searchnet_repertoire(cs, k = 2, scale = FALSE, seed = 1),
                  error = conditionMessage)
  expect_false(grepl("zero variance", msg))
})

test_that("an unknown feature names it and lists what is available", {
  expect_error(searchnet_repertoire(two_group_stats(), k = 2,
                                    features = c("focusing", "bogus")),
               "unknown feature(s): bogus", fixed = TRUE)
  expect_error(searchnet_repertoire(two_group_stats(), k = 2,
                                    features = "bogus"),
               "Available: focusing, reinforcing, mixing, clustering, create_share",
               fixed = TRUE)
  ## `activity` is a real chain_stats column but NOT an actor-profile column, so
  ## it is the mistake a caller is most likely to make; it must be caught here
  ## rather than producing an all-NA feature matrix.
  expect_error(searchnet_repertoire(two_group_stats(), k = 2,
                                    features = "activity"),
               "unknown feature(s): activity", fixed = TRUE)
})

test_that("`actor` and `n_events` are rejected as features", {
  ## They are identifiers, not behaviour. This test previously DOCUMENTED a bug:
  ## validation ran `setdiff(features, names(profiles))` over the whole profile
  ## frame while the error message advertised that set minus the two identifier
  ## columns, so `features = "actor"` passed validation and ran, partitioning
  ## actors by the numeric value of their id. Validation now runs against the
  ## advertised set, so both are refused with a message that says why.
  expect_error(
    searchnet_repertoire(two_group_stats(), k = 2, features = "actor", seed = 1),
    "unknown feature(s): actor", fixed = TRUE)
  expect_error(
    searchnet_repertoire(two_group_stats(), k = 2, features = "actor", seed = 1),
    "identifiers, not behaviour", fixed = TRUE)

  ## `n_events` took the same path and used to die later with a message about
  ## variance, naming the wrong problem. It is now refused up front.
  expect_error(
    searchnet_repertoire(two_group_stats(), k = 2, features = "n_events"),
    "unknown feature(s): n_events", fixed = TRUE)

  ## The advertised set is still reachable and still works.
  ok <- searchnet_repertoire(two_group_stats(), k = 2,
                             features = c("focusing", "clustering"), seed = 1)
  expect_s3_class(ok, "searchnet_repertoire")
})

test_that("the same seed reproduces the partition exactly", {
  ## nstart = 1 so that k-means' random starts actually bite; with the default
  ## nstart = 25 on a clean fixture the answer is stable regardless of the seed
  ## and the test would prove nothing.
  set.seed(9090)
  spec <- data.frame(actor = 1:20,
                     focusing     = stats::rnorm(20),
                     reinforcing  = stats::rnorm(20),
                     mixing       = stats::rnorm(20),
                     clustering   = stats::rnorm(20),
                     create_share = stats::runif(20),
                     stringsAsFactors = FALSE)
  ## create_share must be realisable as a proportion of 6 events.
  spec$create_share <- round(spec$create_share * 6) / 6
  cs <- make_repertoire_stats(spec, 6L)

  a <- searchnet_repertoire(cs, k = 5, nstart = 1L, seed = 4242)
  b <- searchnet_repertoire(cs, k = 5, nstart = 1L, seed = 4242)
  expect_identical(a$assignment, b$assignment)
  expect_identical(a$profiles, b$profiles)
  expect_identical(a$silhouette, b$silhouette)

  ## The same for the automatic-k path, where the criterion loop consumes RNG
  ## before the final fit and a mis-placed set.seed() would show up.
  ca <- searchnet_repertoire(cs, k_range = 2:6, nstart = 1L, seed = 77)
  cb <- searchnet_repertoire(cs, k_range = 2:6, nstart = 1L, seed = 77)
  expect_identical(ca$criterion, cb$criterion)
  expect_identical(ca$k, cb$k)
  expect_identical(ca$assignment, cb$assignment)

  ## Non-vacuity: with nstart = 1 the seed genuinely changes the answer on this
  ## fixture, so the identity above is a property of the seeding and not of a
  ## k-means run that could only ever return one thing.
  sils <- vapply(1:20, function(s)
    searchnet_repertoire(cs, k = 5, nstart = 1L, seed = s)$silhouette,
    numeric(1))
  expect_gt(length(unique(sils)), 1L)
})

test_that("choosing k automatically gives the same partition as asking for it", {
  ## `searchnet_repertoire()` seeds EACH candidate in the criterion loop
  ## identically, so the criterion loop's RNG consumption does not leak into the
  ## partition that is returned. The consequence is worth locking in: reaching
  ## k = 3 by selection and reaching it by request give the SAME repertoires at
  ## the same seed, which is what lets a caller re-run a reported analysis with
  ## k pinned.
  ##
  ## CONTRACT CHANGED (2026-08-23). This comment used to record the price of the
  ## old implementation -- the final fit was an INDEPENDENT re-fit, so
  ## `$silhouette` need not equal the `$criterion` row for the chosen k, and at
  ## nstart = 1 the two differed for 29 of 40 seeds on this fixture. The chosen
  ## candidate's fit is now reused rather than re-fitted, so the two agree by
  ## construction; that is asserted in the next test rather than merely noted.
  set.seed(9090)
  spec <- data.frame(actor = 1:20,
                     focusing     = stats::rnorm(20),
                     reinforcing  = stats::rnorm(20),
                     mixing       = stats::rnorm(20),
                     clustering   = stats::rnorm(20),
                     create_share = round(stats::runif(20) * 6) / 6,
                     stringsAsFactors = FALSE)
  cs <- make_repertoire_stats(spec, 6L)
  for (s in c(1, 2, 3, 17, 33)) {
    auto <- searchnet_repertoire(cs, k_range = 2:6, nstart = 1L, seed = s)
    fixed <- searchnet_repertoire(cs, k = auto$k, nstart = 1L, seed = s)
    expect_identical(auto$assignment, fixed$assignment,
                     info = sprintf("seed %d", s))
    expect_identical(auto$profiles, fixed$profiles)
    expect_identical(auto$silhouette, fixed$silhouette)
  }
})

test_that("an unseeded call still returns a valid partition", {
  set.seed(1)
  res <- searchnet_repertoire(two_group_stats(), k = 2)
  expect_equal(sort(res$profiles$size), c(2L, 3L))
  expect_equal(sum(res$profiles$size), nrow(res$assignment))
})

test_that("a repertoire runs end to end on a real searchnet_chain_stats() frame", {
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  set.seed(246810)
  M <- 8; N <- 6
  log <- data.frame(a = sample.int(M, 200, replace = TRUE),
                    b = sample.int(N, 200, replace = TRUE))
  cs  <- searchnet_chain_stats(log, B0 = matrix(0L, M, N))
  res <- searchnet_repertoire(cs, k_range = 2:5, seed = 11)
  expect_s3_class(res, "searchnet_repertoire")
  expect_equal(nrow(res$assignment) + nrow(res$dropped), M)
  expect_equal(sum(res$profiles$size), nrow(res$assignment))
  expect_equal(res$k, res$criterion$k[which.max(res$criterion$silhouette)])
  expect_true(all(is.finite(res$criterion$silhouette)))
})

test_that("character actor ids are rejected, though late and obscurely", {
  ## OBSERVATION. `.searchnet_actor_profiles()` does as.integer(names(table(...)))
  ## on the actor column, so non-numeric ids coerce to NA, no actor is retained,
  ## `profiles` collapses to NULL and the caller is told about "unknown
  ## features" instead of about the actor ids. searchnet_chain_stats() always
  ## emits integer actors, so this is out of contract rather than a live bug --
  ## recorded because the failure mode names the wrong thing.
  cs <- two_group_stats()
  cs$actor <- paste0("firm_", cs$actor)
  expect_error(suppressWarnings(searchnet_repertoire(cs, k = 2)))
})


# ===========================================================================
# 9. searchnet_repertoire_null()
# ===========================================================================

## Shadow `searchnet_repertoire` in the environment `searchnet_repertoire_null()`
## resolves it from, so every k it is called with can be recorded. This is the
## only way to see that k is held fixed across the permutations: the returned
## object carries silhouettes, and a silhouette does not reveal how many
## clusters produced it.
record_repertoire_k <- function(expr) {
  env  <- environment(searchnet_repertoire_null)
  orig <- get("searchnet_repertoire", envir = env)
  seen <- list()
  spy <- function(chain_stats, k = NULL, ...) {
    ## Wrapped in list(): `seen[[i]] <- NULL` DELETES rather than stores, and a
    ## k of NULL -- the observed run -- is exactly the case this test is about.
    seen[[length(seen) + 1L]] <<- list(k)
    orig(chain_stats, k = k, ...)
  }
  ok <- tryCatch({ assign("searchnet_repertoire", spy, envir = env); TRUE },
                 error = function(e) FALSE)
  if (!ok) skip("cannot shadow searchnet_repertoire in a locked namespace")
  on.exit(assign("searchnet_repertoire", orig, envir = env), add = TRUE)
  value <- force(expr)
  list(value = value, k_seen = seen)
}

## Structured: three planted groups of four actors, every event of an actor
## carrying that actor's values, so permuting the actor labels across events
## destroys the association completely.
structured_stats <- local({
  set.seed(1001)
  grp <- rep(1:3, each = 4)
  spec <- data.frame(actor = 1:12,
                     focusing     = c(0, 10, 20)[grp] + stats::runif(12, -.3, .3),
                     reinforcing  = c(0, 10, 20)[grp] + stats::runif(12, -.3, .3),
                     mixing       = c(0, 10, 20)[grp] + stats::runif(12, -.3, .3),
                     clustering   = c(0, 10, 20)[grp] + stats::runif(12, -.3, .3),
                     create_share = c(0, 0.5, 1)[grp],
                     stringsAsFactors = FALSE)
  make_repertoire_stats(spec, n_events = 6L)
})

## Unstructured: the same event set, but each event's statistics are drawn
## independently of who did it, so the observed labelling is exchangeable with
## any permutation of it.
unstructured_stats <- local({
  set.seed(2002)
  n_ev <- 12 * 6
  data.frame(
    actor       = rep(1:12, each = 6),
    change      = sample(c("create", "delete"), n_ev, replace = TRUE),
    focusing    = stats::rnorm(n_ev),
    reinforcing = stats::rnorm(n_ev),
    mixing      = stats::rnorm(n_ev),
    clustering  = stats::rnorm(n_ev),
    stringsAsFactors = FALSE)
})

test_that("with k = NULL every permutation re-selects k over the same k_range", {
  ## CONTRACT DELIBERATELY CHANGED (2026-08-23). This test previously asserted
  ## the opposite: that the observed run's k was imposed on every permutation.
  ## That comparison is anticonservative, because with k = NULL the observed
  ## silhouette is a MAXIMUM over k_range while a permutation pinned to one k is
  ## a single draw, and a maximum over several candidates beats a single draw on
  ## average even under the null. The default is now `k_selection = "reselect"`:
  ## each permutation maximises over the same k_range, so the comparison is
  ## maximum against maximum. The older behaviour is still reachable and is
  ## asserted in the next test, together with the reason it is not the default.
  r <- record_repertoire_k(
    searchnet_repertoire_null(structured_stats, n_perm = 5L, k = NULL,
                              k_range = 2:5, seed = 3))
  ks <- lapply(r$k_seen, `[[`, 1L)
  expect_equal(length(ks), 6L)         ## 1 observed + 5 permutations
  expect_null(ks[[1]])                 ## the observed run chose k itself
  ## And so did every permutation: each was handed k = NULL, not the observed k.
  expect_true(all(vapply(ks[-1], is.null, logical(1))))
  ## The chosen k is reported, and it is not the trivially-smallest value.
  expect_equal(r$value$k, 3L)
  ## The k each permutation actually landed on is returned, one per permutation,
  ## so the granularity concern that motivated the old behaviour is visible
  ## rather than hidden.
  expect_equal(length(r$value$k_permuted), r$value$n_perm)
  expect_true(all(r$value$k_permuted >= 2L & r$value$k_permuted <= 5L))
  expect_equal(r$value$k_selection, "reselect")
})

test_that("k_selection = 'fixed' restores the older, anticonservative behaviour", {
  r <- record_repertoire_k(
    searchnet_repertoire_null(structured_stats, n_perm = 5L, k = NULL,
                              k_range = 2:5, k_selection = "fixed", seed = 3))
  ks <- lapply(r$k_seen, `[[`, 1L)
  expect_equal(length(ks), 6L)
  expect_null(ks[[1]])
  perm_ks <- vapply(ks[-1], function(z) as.integer(z), integer(1))
  expect_true(all(perm_ks == r$value$k))
  expect_equal(length(unique(perm_ks)), 1L)
  expect_equal(r$value$k_selection, "fixed")
  expect_true(all(r$value$k_permuted == r$value$k))
})

test_that("the honest default is not more likely to reject than the fixed-k one", {
  ## The direction the asymmetry predicts, on data with no actor-to-behaviour
  ## association: letting the permutations maximise too can only raise their
  ## silhouettes, so p can only rise. Asserted as a weak inequality on the
  ## permuted distribution rather than on p, which is a coarser quantity.
  ##
  ## `unstructured_stats` gives every actor exactly 6 events, so the stratified
  ## and free permutations coincide here and this isolates the k question.
  re <- searchnet_repertoire_null(unstructured_stats, n_perm = 39L, k = NULL,
                                  k_range = 2:5, k_selection = "reselect",
                                  seed = 909)
  fx <- searchnet_repertoire_null(unstructured_stats, n_perm = 39L, k = NULL,
                                  k_range = 2:5, k_selection = "fixed",
                                  seed = 909)
  expect_equal(re$observed, fx$observed)          ## same observed statistic
  expect_gte(mean(re$permuted), mean(fx$permuted))
  expect_gte(re$p_value, fx$p_value)
})

test_that("an explicitly supplied k makes the selection question moot", {
  res <- searchnet_repertoire_null(structured_stats, n_perm = 5L, k = 3,
                                   seed = 3)
  expect_equal(res$k_selection, "supplied")
  expect_true(all(res$k_permuted == 3L))
})

test_that("an explicitly supplied k is passed through to every run", {
  r <- record_repertoire_k(
    searchnet_repertoire_null(structured_stats, n_perm = 4L, k = 4, seed = 3))
  expect_equal(length(r$k_seen), 5L)
  expect_true(all(vapply(r$k_seen,
                         function(z) identical(as.integer(z[[1L]]), 4L),
                         logical(1))))
  expect_equal(r$value$k, 4L)
})

test_that("the p-value uses the (r + 1)/(n + 1) correction", {
  res <- searchnet_repertoire_null(unstructured_stats, n_perm = 49L, k = 3,
                                   seed = 20260823)
  ## Recomputed from the returned pieces, so the correction is checked as
  ## arithmetic rather than asserted against a number this run happened to give.
  expect_equal(res$p_value,
               (sum(res$permuted >= res$observed) + 1) /
                 (length(res$permuted) + 1))
  ## The correction is what keeps p away from 0: with 49 permutations the
  ## smallest attainable value is 1/50, not 0.
  expect_gte(res$p_value, 1 / 50)
  expect_lte(res$p_value, 1)
  expect_gt(res$p_value, 0)
})

test_that("p is in (0, 1] whatever the data does", {
  for (fx in list(structured_stats, unstructured_stats)) {
    res <- searchnet_repertoire_null(fx, n_perm = 19L, k = 3, seed = 5)
    expect_gt(res$p_value, 0)
    expect_lte(res$p_value, 1)
  }
})

test_that("n_perm reports the number of permutations actually used", {
  res <- searchnet_repertoire_null(unstructured_stats, n_perm = 19L, k = 3,
                                   seed = 8)
  expect_equal(res$n_perm, length(res$permuted))
  expect_equal(res$n_perm, 19L)
  expect_true(all(is.finite(res$permuted)))
  expect_s3_class(res, "searchnet_repertoire_null")
  ## Permuting `actor` is a permutation of the SAME label multiset, so every
  ## actor keeps its event count and no permutation can fail the min_events
  ## filter. Stated as an assertion because it is why n_perm is complete here.
  expect_equal(res$k, 3L)
})

test_that("observed separation sits above the permuted distribution when actors differ", {
  ## Direction only, not a specific p: the fixture plants three actor types, so
  ## breaking the actor-to-behaviour association must cost separation.
  res <- searchnet_repertoire_null(structured_stats, n_perm = 49L, k = 3,
                                   seed = 13)
  expect_gt(res$observed, max(res$permuted))
  expect_lt(res$p_value, 0.05)
  expect_gt(res$observed, mean(res$permuted))
})

test_that("observed separation is unremarkable when actor labels carry nothing", {
  ## The companion fixture, and the more important one: k-means returns k
  ## clusters from any feature geometry, so a procedure that reported structure
  ## here would report it everywhere. k is supplied explicitly to both runs so
  ## that the comparison is not confounded by the observed run's freedom to
  ## choose the k that flatters it.
  res <- searchnet_repertoire_null(unstructured_stats, n_perm = 49L, k = 3,
                                   seed = 13)
  expect_gt(res$p_value, 0.05)
  expect_lte(res$observed, max(res$permuted))

  ## And the two fixtures are ordered the way the construct requires.
  res_s <- searchnet_repertoire_null(structured_stats, n_perm = 49L, k = 3,
                                     seed = 13)
  expect_gt(res$p_value, res_s$p_value)
})

test_that("the observed silhouette in the null is the one searchnet_repertoire gives", {
  obs <- searchnet_repertoire(structured_stats, k = 3, seed = 21)
  res <- searchnet_repertoire_null(structured_stats, n_perm = 5L, k = 3,
                                   seed = 21)
  expect_equal(res$observed, obs$silhouette)
})

test_that("extra arguments reach both the observed and the permuted runs", {
  ## `features` is passed through `...`; if it reached only the observed run the
  ## permuted silhouettes would be computed in a different feature space and the
  ## comparison would be meaningless.
  r <- record_repertoire_k(
    searchnet_repertoire_null(structured_stats, n_perm = 3L, k = 2, seed = 2,
                              features = c("focusing", "create_share")))
  expect_equal(length(r$k_seen), 4L)
  res <- r$value
  expect_equal(res$n_perm, 3L)
  ## Same call without the restriction gives a different observed value, so the
  ## argument really did change the computation.
  full <- searchnet_repertoire_null(structured_stats, n_perm = 3L, k = 2,
                                    seed = 2)
  expect_false(isTRUE(all.equal(res$observed, full$observed)))
})


# ===========================================================================
# 10. searchnet_repertoire_ri(): the error paths only
#
# No RSiena estimation anywhere. The function is a bridge whose whole stated
# purpose is to fail loudly and name the cause rather than substitute a
# degraded quantity, so the error text is the contract.
# ===========================================================================

test_that("searchnet_repertoire_ri() names the cause when it cannot run", {
  if (!requireNamespace("RSiena", quietly = TRUE)) {
    expect_error(searchnet_repertoire_ri(NULL, NULL),
                 "RSiena is not installed", fixed = TRUE)
  } else if (!"sienaRI" %in% getNamespaceExports("RSiena")) {
    expect_error(searchnet_repertoire_ri(NULL, NULL),
                 "does not export", fixed = TRUE)
  } else {
    ## Objects of the wrong class, so sienaRI rejects them immediately. This is
    ## a validation failure, not an estimation.
    bad_dat <- structure(list(), class = "not_siena")
    bad_ans <- structure(list(), class = "not_sienaFit")
    expect_error(searchnet_repertoire_ri(bad_dat, bad_ans),
                 "RSiena::sienaRI() failed on this fit", fixed = TRUE)
    ## The three things the message must say, because the point of the wrapper
    ## is the interpretation it attaches to the failure.
    expect_error(searchnet_repertoire_ri(bad_dat, bad_ans),
                 "non-implementation in RSiena", fixed = TRUE)
    expect_error(searchnet_repertoire_ri(bad_dat, bad_ans),
                 "do not substitute another quantity", fixed = TRUE)
    expect_error(searchnet_repertoire_ri(bad_dat, bad_ans),
                 "searchnet_repertoire()", fixed = TRUE)
    ## The underlying message is carried through, not swallowed.
    msg <- tryCatch(searchnet_repertoire_ri(bad_dat, bad_ans),
                    error = conditionMessage)
    expect_gt(nchar(msg), nchar("RSiena::sienaRI() failed on this fit: "))
  }
})


# ===========================================================================
# 11. print methods
# ===========================================================================

test_that("print.searchnet_repertoire reports k, features, silhouette and profiles", {
  res <- searchnet_repertoire(two_group_stats(), k = 2, seed = 1)
  txt <- capture.output(print(res))
  expect_true(any(grepl("Behavioural repertoires: k = 2 over 5 actors", txt)))
  expect_true(any(grepl("Features: focusing, reinforcing, mixing, clustering, create_share",
                        txt, fixed = TRUE)))
  expect_true(any(grepl("Average silhouette width", txt)))
  expect_true(any(grepl("Cluster profiles (original scale)", txt, fixed = TRUE)))
  ## The mandatory caveat: any clustering procedure returns clusters.
  expect_true(any(grepl("searchnet_repertoire_null()", txt, fixed = TRUE)))
  ## With k given there is no criterion path, so the header must not appear.
  expect_false(any(grepl("k chosen by maximum silhouette", txt)))
  ## No unassigned actors here, so no threshold line.
  expect_false(any(grepl("below the event threshold", txt)))
})

test_that("print.searchnet_repertoire shows the criterion path and the dropped count", {
  cs <- rbind(two_group_stats(6L),
              make_repertoire_stats(
                data.frame(actor = 6L, focusing = 50, reinforcing = 50,
                           mixing = 50, clustering = 50, create_share = 1,
                           stringsAsFactors = FALSE),
                n_events = 2L))
  res <- searchnet_repertoire(cs, k_range = 2:4, seed = 1)
  txt <- capture.output(print(res))
  expect_true(any(grepl("k chosen by maximum silhouette", txt)))
  expect_true(any(grepl("silhouette", txt)))
  expect_true(any(grepl("1 actor(s) below the event threshold", txt,
                        fixed = TRUE)))
})

test_that("print.searchnet_repertoire says 'undefined' rather than printing NA", {
  ## Hand-built object: the normal path cannot produce an NA silhouette (k >= 2
  ## and k < n_actors together force n_actors >= 3), so the branch is reachable
  ## only by construction. It is still the branch a caller sees if the object
  ## is ever built some other way, and "undefined" is the honest word.
  obj <- structure(list(
    assignment = data.frame(actor = 1:3, n_events = rep(5L, 3),
                            repertoire = c(1L, 1L, 1L)),
    profiles   = data.frame(repertoire = 1L, size = 3L, focusing = 0),
    k          = 1L,
    criterion  = NULL,
    silhouette = NA_real_,
    dropped    = data.frame(actor = integer(0), n_events = integer(0)),
    features   = "focusing",
    n_actors   = 3L), class = "searchnet_repertoire")
  txt <- capture.output(print(obj))
  expect_true(any(grepl("Average silhouette width: undefined", txt,
                        fixed = TRUE)))
  expect_false(any(grepl("width: NA", txt, fixed = TRUE)))
})

test_that("print.searchnet_repertoire returns its argument invisibly", {
  res <- searchnet_repertoire(two_group_stats(), k = 2, seed = 1)
  vis <- NULL
  capture.output(vis <- withVisible(print(res)))
  expect_false(vis$visible)
  expect_identical(vis$value, res)
})

test_that("print.searchnet_repertoire_null reports k, both sides and the caveat", {
  res <- searchnet_repertoire_null(structured_stats, n_perm = 9L, k = 3,
                                   seed = 4)
  txt <- capture.output(print(res))
  expect_true(any(grepl("Permutation null for repertoires (k = 3, 9 permutations)",
                        txt, fixed = TRUE)))
  expect_true(any(grepl("Observed silhouette", txt)))
  expect_true(any(grepl("Permuted, mean [min, max]", txt, fixed = TRUE)))
  expect_true(any(grepl("^p = ", txt)))
  ## The interpretive instruction is part of the output, by design.
  expect_true(any(grepl("a null here is a measurement, not a failure", txt,
                        fixed = TRUE)))
})

test_that("print.searchnet_repertoire_null returns its argument invisibly", {
  res <- searchnet_repertoire_null(structured_stats, n_perm = 9L, k = 3,
                                   seed = 4)
  vis <- NULL
  capture.output(vis <- withVisible(print(res)))
  expect_false(vis$visible)
  expect_identical(vis$value, res)
})


# ===========================================================================
# 12. searchnet_repertoire(): the reported silhouette IS the criterion row
#
# DEFECT FIXED 2026-08-23. The final k-means fit used to be an independent
# re-fit after selection, so `$silhouette` and the `$criterion` row for the
# chosen k were two different k-means solutions at the same k. At nstart = 1
# they disagreed for 29 of 40 seeds. The criterion loop's fit for the chosen k
# is now reused, and each candidate is seeded identically so that pinning k
# still reproduces the auto-selected partition exactly.
# ===========================================================================

## A fixture with enough structure to make k-selection non-trivial and enough
## noise that nstart = 1 lands in different local optima across seeds.
wobbly_stats <- local({
  set.seed(9090)
  spec <- data.frame(actor = 1:20,
                     focusing     = stats::rnorm(20),
                     reinforcing  = stats::rnorm(20),
                     mixing       = stats::rnorm(20),
                     clustering   = stats::rnorm(20),
                     create_share = round(stats::runif(20) * 6) / 6,
                     stringsAsFactors = FALSE)
  make_repertoire_stats(spec, n_events = 6L)
})

test_that("$silhouette equals the $criterion row for the chosen k, at every seed", {
  ## nstart = 1 so the local optimum genuinely depends on the start; this is the
  ## setting in which the old code disagreed for most seeds.
  for (s in 1:40) {
    res <- searchnet_repertoire(wobbly_stats, k_range = 2:6, nstart = 1L,
                                seed = s)
    expect_equal(res$silhouette,
                 res$criterion$silhouette[res$criterion$k == res$k],
                 info = sprintf("seed %d", s))
    ## and the chosen k really is the argmax, so the identity above is not being
    ## satisfied by picking whichever row happens to match.
    expect_equal(res$k,
                 res$criterion$k[which.max(res$criterion$silhouette)],
                 info = sprintf("seed %d", s))
    expect_equal(res$silhouette, max(res$criterion$silhouette, na.rm = TRUE),
                 info = sprintf("seed %d", s))
  }
})

test_that("the fixture is one where nstart = 1 could disagree", {
  ## Non-vacuity for the test above: if every seed gave the same partition the
  ## identity would hold trivially and would have held before the fix too.
  sils <- vapply(1:40, function(s)
    searchnet_repertoire(wobbly_stats, k_range = 2:6, nstart = 1L,
                         seed = s)$silhouette, numeric(1))
  expect_gt(length(unique(sils)), 3L)
})

test_that("the preserved property survives the fix: auto k equals pinned k", {
  ## The reason the fix seeds each CANDIDATE rather than seeding once before the
  ## loop. Restated over many more seeds than the original test used, because
  ## this is the property the fix was most at risk of breaking.
  for (s in 1:25) {
    auto  <- searchnet_repertoire(wobbly_stats, k_range = 2:6, nstart = 1L,
                                  seed = s)
    fixed <- searchnet_repertoire(wobbly_stats, k = auto$k, nstart = 1L,
                                  seed = s)
    expect_identical(auto$assignment, fixed$assignment,
                     info = sprintf("seed %d", s))
    expect_identical(auto$silhouette, fixed$silhouette,
                     info = sprintf("seed %d", s))
  }
})

test_that("the criterion row for any k is the fit that pinning that k gives", {
  ## The general form: not just the chosen k, EVERY candidate k.
  res <- searchnet_repertoire(wobbly_stats, k_range = 2:6, nstart = 1L,
                              seed = 17)
  for (kk in res$criterion$k) {
    pinned <- searchnet_repertoire(wobbly_stats, k = kk, nstart = 1L, seed = 17)
    expect_equal(pinned$silhouette,
                 res$criterion$silhouette[res$criterion$k == kk],
                 info = sprintf("k = %d", kk))
  }
})


# ===========================================================================
# 13. .searchnet_repertoire_strata() and .searchnet_permute_within()
# ===========================================================================

test_that("actors with equal event counts are never split across strata", {
  ## The tie merge is the whole point: two actors with the same count are the
  ## pair the null has most reason to treat as exchangeable, and a rank cut
  ## falling between them would freeze both.
  cnt <- c(a = 10L, b = 10L, c = 10L, d = 10L, e = 50L, f = 50L, g = 99L)
  st <- .searchnet_repertoire_strata(cnt, n_strata = 7L)
  expect_equal(length(unique(st[c("a", "b", "c", "d")])), 1L)
  expect_equal(length(unique(st[c("e", "f")])), 1L)
  expect_equal(length(unique(st)), 3L)
  ## Bins are consecutive from 1 and ordered by count.
  expect_setequal(unique(st), 1:3)
  expect_lt(st[["a"]], st[["e"]])
  expect_lt(st[["e"]], st[["g"]])
  expect_equal(names(st), names(cnt))
})

test_that("distinct counts are split into roughly equal-sized strata", {
  cnt <- seq_len(20L)
  names(cnt) <- as.character(seq_len(20L))
  st <- .searchnet_repertoire_strata(cnt, n_strata = 10L)
  expect_equal(length(unique(st)), 10L)
  expect_true(all(table(st) == 2L))
  ## Monotone in the count: a stratum is an interval of the count axis.
  expect_false(is.unsorted(st))
})

test_that("a degenerate count distribution collapses to a single stratum", {
  cnt <- c(a = 6L, b = 6L, c = 6L, d = 6L)
  expect_equal(unname(.searchnet_repertoire_strata(cnt, 10L)), rep(1L, 4))
  ## n_strata = 1 is the explicit request for the same thing.
  cnt2 <- c(a = 1L, b = 5L, c = 20L)
  expect_equal(unname(.searchnet_repertoire_strata(cnt2, 1L)), rep(1L, 3))
  ## More strata than actors is not an error; there are simply fewer bins.
  expect_lte(length(unique(.searchnet_repertoire_strata(cnt2, 99L))), 3L)
})

test_that("a within-block shuffle preserves every label's multiplicity", {
  set.seed(1)
  actor <- rep(1:6, times = c(3, 3, 7, 7, 20, 20))
  block <- rep(1:3, times = c(6, 14, 40))
  for (i in 1:20) {
    out <- .searchnet_permute_within(actor, block)
    ## Each actor keeps its own event count EXACTLY, not merely in expectation.
    expect_equal(as.vector(table(out)), as.vector(table(actor)))
    expect_equal(names(table(out)), names(table(actor)))
    ## and no event leaves its block.
    expect_equal(unname(unclass(table(block, out))),
                 unname(unclass(table(block, actor))))
  }
})

test_that("a single block consumes the same RNG as a free permutation", {
  ## This is why `strata = "none"` and a degenerate stratification agree bit for
  ## bit, which in turn is why the pre-existing equal-count fixtures in this
  ## file produce identical numbers before and after the stratification change.
  v <- rep(1:9, each = 4)
  set.seed(31337); a <- .searchnet_permute_within(v, rep(1L, length(v)))
  set.seed(31337); b <- sample(v)
  expect_identical(a, b)
})

test_that("a singleton block is left alone rather than mangled", {
  ## `sample(idx)` on a length-1 idx would permute seq_len(idx) instead. The
  ## guard makes the single-actor stratum a no-op, which is also the honest
  ## behaviour: there is no one to swap with.
  v <- c(4L, 4L, 9L)
  set.seed(2)
  out <- .searchnet_permute_within(v, c(1L, 1L, 2L))
  expect_identical(out[3], 9L)
  expect_equal(sort(out), sort(v))
})


# ===========================================================================
# 14. searchnet_repertoire_null(): the activity-volume confound
#
# The reason `strata` exists. `focusing` counts an actor's repeats of its own
# prior pairs and `mixing` is activity x popularity, so both are mechanically
# increasing in an actor's own event count. A free permutation gives every
# pseudo-actor a mixture drawn from the whole event pool, whose per-event mean
# converges on the population mean whatever the pseudo-actor's count, so the
# permuted profiles lose the volume-driven spread that the real ones keep.
# ===========================================================================

## No behavioural types whatsoever: every actor draws its component uniformly at
## random from the same distribution. The ONLY heterogeneity is volume -- three
## tiers of 20, 120 and 400 events. Built through the real
## `searchnet_chain_stats()` so the mechanical dependence is the genuine one and
## not something this file wrote in by hand.
volume_only_stats <- function(seed = 22L, M = 18L, N = 7L,
                              tiers = c(20L, 120L, 400L)) {
  set.seed(seed)
  counts <- rep(tiers, each = M / length(tiers))
  log <- do.call(rbind, lapply(seq_len(M), function(i)
    data.frame(a = rep(i, counts[i]),
               b = sample.int(N, counts[i], replace = TRUE))))
  log <- log[sample.int(nrow(log)), ]
  searchnet_chain_stats(log, B0 = matrix(0L, M, N))
}

test_that("the volume fixture really has the confound and no behaviour types", {
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  ap <- .searchnet_actor_profiles(volume_only_stats(), min_events = 5L)$profiles
  ## Three volume tiers, six actors each.
  expect_equal(as.integer(table(ap$n_events)), c(6L, 6L, 6L))
  ## `focusing` is near-deterministic in the actor's own event count. That is the
  ## mechanism, measured rather than assumed.
  expect_gt(stats::cor(ap$focusing, ap$n_events), 0.99)
  expect_gt(stats::cor(ap$mixing,   ap$n_events), 0.99)
  ## `clustering` is not: it is the feature carrying no volume signal, which is
  ## what keeps the observed geometry off the silhouette ceiling.
  expect_lt(abs(stats::cor(ap$clustering, ap$n_events)), 0.9)
})

test_that("the free null rejects, and the stratified null does not, on volume alone", {
  ## THE TEST THE `strata` ARGUMENT EXISTS FOR. There are no behavioural types
  ## in this fixture at all. A procedure that reports p < 0.05 here is reporting
  ## that actors differ in HOW MUCH they do, dressed up as a claim that they
  ## differ in WHAT they do.
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  cs <- volume_only_stats()
  fe <- c("focusing", "mixing", "clustering")

  free <- searchnet_repertoire_null(cs, n_perm = 49L, k = 3, strata = "none",
                                    features = fe, seed = 7)
  strat <- searchnet_repertoire_null(cs, n_perm = 49L, k = 3,
                                     strata = "n_events", features = fe,
                                     seed = 7)

  expect_lt(free$p_value, 0.05)          ## the free null is fooled
  expect_gt(strat$p_value, 0.05)         ## the stratified null is not
  expect_gt(strat$p_value, free$p_value)

  ## The mechanism, not just the outcome: under the free permutation the pseudo-
  ## actors' profiles collapse towards the population mean and the permuted
  ## silhouettes fall below the observed one; under stratification they keep the
  ## tier structure and rise above it.
  expect_lt(max(free$permuted), free$observed)
  expect_gt(mean(strat$permuted), strat$observed)

  ## The strata are the three volume tiers, recovered from the counts alone.
  expect_equal(strat$n_strata, 3L)
  expect_equal(strat$strata_sizes, c(6L, 6L, 6L))
  expect_equal(free$n_strata, 1L)
})

test_that("the same ordering holds across several fixture seeds", {
  ## Guarding against a single lucky fixture. Direction only, and at 99
  ## permutations rather than 49: p is a Monte Carlo quantity and fixture 33 sits
  ## near 0.05, so a coarse grid puts it on the wrong side of the threshold. The
  ## ORDERING (free below, stratified far above) is stable at every resolution
  ## tried; only the crossing of an arbitrary threshold is not.
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  fe <- c("focusing", "mixing", "clustering")
  for (fs in c(11L, 22L, 33L)) {
    cs <- volume_only_stats(seed = fs)
    free <- searchnet_repertoire_null(cs, n_perm = 99L, k = 3, strata = "none",
                                      features = fe, seed = 7)
    strat <- searchnet_repertoire_null(cs, n_perm = 99L, k = 3,
                                       strata = "n_events", features = fe,
                                       seed = 7)
    expect_gt(strat$p_value / free$p_value, 5,
              label = sprintf("p ratio (fixture %d)", fs))
    expect_lt(free$p_value, 0.05, label = sprintf("free p (fixture %d)", fs))
    expect_gt(strat$p_value, 0.05, label = sprintf("strat p (fixture %d)", fs))
  }
})

test_that("OBSERVATION: with the default feature set the free null is not fooled here", {
  ## Recorded rather than demanded, and deliberately not tuned away. With all
  ## five default features the free null gives a LARGE p on the volume-only
  ## fixture, so the confound's severity depends on the feature set: three of
  ## the five defaults carry no volume signal, and their standardised noise
  ## dilutes the two that do. The free permutation also preserves each actor's
  ## own event count, so a low-volume pseudo-actor's profile stays noisy while a
  ## high-volume one's is tight, and k-means can score that heteroscedastic
  ## geometry well. The confound is real -- the test above demonstrates it -- but
  ## it is not universal, and reporting both nulls is what tells a reader which
  ## situation they are in.
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  cs <- volume_only_stats()
  free <- searchnet_repertoire_null(cs, n_perm = 39L, k = 3, strata = "none",
                                    seed = 7)
  expect_gt(free$p_value, 0.05)
})

test_that("strata = 'n_events' is the default", {
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  cs <- volume_only_stats()
  fe <- c("focusing", "mixing", "clustering")
  a <- searchnet_repertoire_null(cs, n_perm = 19L, k = 3, features = fe,
                                 seed = 7)
  b <- searchnet_repertoire_null(cs, n_perm = 19L, k = 3, strata = "n_events",
                                 features = fe, seed = 7)
  expect_equal(a$strata, "n_events")
  expect_identical(a$permuted, b$permuted)
  expect_identical(a$p_value, b$p_value)
})

test_that("the equal-count fixtures are unaffected by stratification", {
  ## Every actor in `structured_stats` has exactly 6 events, so there is one
  ## stratum and the stratified permutation IS the free one -- bit for bit,
  ## which is what keeps the older tests in this file meaningful.
  a <- searchnet_repertoire_null(structured_stats, n_perm = 19L, k = 3,
                                 strata = "n_events", seed = 5)
  b <- searchnet_repertoire_null(structured_stats, n_perm = 19L, k = 3,
                                 strata = "none", seed = 5)
  expect_identical(a$permuted, b$permuted)
  expect_equal(a$n_strata, 1L)
})

test_that("a mostly-singleton stratification warns that it has no power", {
  ## 12 actors with 12 distinct counts and 10 strata: most actors end up alone
  ## and frozen at their observed labelling, so the null degenerates towards the
  ## identity. That must be said out loud rather than reported as p = 1.
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  set.seed(555)
  M <- 12; N <- 6
  counts <- 8:19
  log <- do.call(rbind, lapply(seq_len(M), function(i)
    data.frame(a = rep(i, counts[i]),
               b = sample.int(N, counts[i], replace = TRUE))))
  cs <- searchnet_chain_stats(log[sample.int(nrow(log)), ],
                              B0 = matrix(0L, M, N))
  expect_warning(searchnet_repertoire_null(cs, n_perm = 5L, k = 3,
                                           n_strata = 10L, seed = 1),
                 "frozen at their")
  expect_warning(searchnet_repertoire_null(cs, n_perm = 5L, k = 3,
                                           n_strata = 10L, seed = 1),
                 "little or no power")
  ## Fewer strata restores exchangeability and the warning goes away.
  expect_silent(searchnet_repertoire_null(cs, n_perm = 5L, k = 3,
                                          n_strata = 3L, seed = 1))
})

test_that("the null reports how it was run", {
  res <- searchnet_repertoire_null(structured_stats, n_perm = 9L, k = 3,
                                   seed = 4)
  expect_equal(res$strata, "n_events")
  expect_equal(res$method, "permute")
  expect_equal(res$k_selection, "supplied")
  expect_type(res$strata_sizes, "integer")
  expect_equal(sum(res$strata_sizes), 12L)   ## every actor is in exactly one
})


# ===========================================================================
# 15. searchnet_repertoire_null(): the residualisation route
#
# The alternative attack on the same confound. Reported honestly: on the
# volume-only fixture it does NOT neutralise the confound, because the
# dependence of `focusing` on the event count is close to LINEAR in the count
# and not in its logarithm. That is a limitation of the functional form, and it
# is the reason the stratified permutation -- which assumes no functional form
# at all -- is the recommended default rather than this.
# ===========================================================================

test_that("residualise = TRUE changes the geometry but not the reported profiles", {
  res_raw <- searchnet_repertoire(structured_stats, k = 3, seed = 1)
  res_rsd <- suppressWarnings(
    searchnet_repertoire(structured_stats, k = 3, seed = 1, residualise = TRUE))
  expect_false(res_raw$residualise)
  expect_true(res_rsd$residualise)
  expect_setequal(res_rsd$assignment$actor, res_raw$assignment$actor)
  ## Every actor in this fixture has 6 events, so log(n_events) is constant and
  ## there is nothing to regress on; the function says so instead of dividing by
  ## zero, and leaves the features alone.
  expect_warning(searchnet_repertoire(structured_stats, k = 3, seed = 1,
                                      residualise = TRUE),
                 "no variation across actors")
})

test_that("residualisation removes the volume trend from the clustered features", {
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  cs <- volume_only_stats()
  fe <- c("focusing", "mixing", "clustering")
  res <- searchnet_repertoire(cs, k = 3, seed = 1, residualise = TRUE,
                              features = fe)
  expect_true(res$residualise)
  expect_equal(nrow(res$assignment), 18L)
  ## `$profiles` stay on the original, un-residualised scale, so the typology
  ## remains readable against the mechanisms rather than against residuals.
  ## Residuals average to zero, so a set of cluster means that are ALL strictly
  ## positive and reach well beyond the residual range distinguishes the two
  ## scales rather than merely being consistent with one of them.
  expect_true(all(res$profiles$focusing > 0))
  expect_gt(max(res$profiles$focusing), 5)
  ## Not the same partition as the un-residualised run: if it were, the argument
  ## would be doing nothing on the fixture built to need it.
  raw <- searchnet_repertoire(cs, k = 3, seed = 1, features = fe)
  same <- function(g) outer(g, g, "==")
  expect_false(identical(same(res$assignment$repertoire),
                         same(raw$assignment$repertoire)))
})

test_that("method = 'residualise' forces a free permutation and records it", {
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  cs <- volume_only_stats()
  res <- searchnet_repertoire_null(cs, n_perm = 19L, k = 3,
                                   features = c("focusing", "mixing",
                                                "clustering"),
                                   method = "residualise", seed = 7)
  expect_equal(res$method, "residualise")
  ## Stacking both corrections would strip the volume signal twice with no way
  ## to say which one the p-value came from, so `strata` is forced to "none".
  expect_equal(res$strata, "none")
})

test_that("method = 'residualise' contradicting residualise = FALSE is an error", {
  expect_error(searchnet_repertoire_null(structured_stats, n_perm = 2L, k = 3,
                                         method = "residualise",
                                         residualise = FALSE),
               "contradicts", fixed = TRUE)
})

test_that("REPORTED: residualising on log(n_events) does NOT fix this fixture", {
  ## An honest negative, not tuned away. The volume-only fixture's `focusing`
  ## rises roughly LINEARLY in the event count (1.2, 8.4, 28.3 at counts 20, 120,
  ## 400), so a linear-in-log regression leaves a large structured residual and
  ## the three tiers stay separated. The route is offered because agreement
  ## between two different corrections is worth more than either alone -- but on
  ## this fixture they DISAGREE, and the stratified permutation is the one to
  ## believe, because it imposes no functional form.
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  cs <- volume_only_stats()
  fe <- c("focusing", "mixing", "clustering")
  rsd <- searchnet_repertoire_null(cs, n_perm = 49L, k = 3, features = fe,
                                   method = "residualise", seed = 7)
  strat <- searchnet_repertoire_null(cs, n_perm = 49L, k = 3, features = fe,
                                     strata = "n_events", seed = 7)
  expect_lt(rsd$p_value, 0.05)     ## still fooled
  expect_gt(strat$p_value, 0.05)   ## not fooled
})


# ===========================================================================
# 16. .searchnet_ari()
# ===========================================================================

test_that("the adjusted Rand index is 1 for identical partitions up to labels", {
  a <- c(1L, 1L, 2L, 2L, 3L, 3L)
  expect_equal(.searchnet_ari(a, a), 1)
  ## Relabelled, non-contiguous, out of order: the same partition.
  expect_equal(.searchnet_ari(a, c(40L, 40L, 7L, 7L, -1L, -1L)), 1)
  expect_equal(.searchnet_ari(a, c("x", "x", "y", "y", "z", "z")), 1)
})

test_that("the adjusted Rand index matches hand-computed values", {
  ## a = 11|22, b = 12|12. Contingency all ones, so index = 0, sa = sb = 2,
  ## expected = 2*2/C(4,2) = 2/3, max = 2, ARI = (0 - 2/3)/(2 - 2/3) = -0.5.
  expect_equal(.searchnet_ari(c(1, 1, 2, 2), c(1, 2, 1, 2)), -0.5)
  ## a = 111|22, b = 11|222. index = 2, sa = 4, sb = 4, expected = 16/10 = 1.6,
  ## max = 4, ARI = 0.4/2.4 = 1/6.
  expect_equal(.searchnet_ari(c(1, 1, 1, 2, 2), c(1, 1, 2, 2, 2)), 1 / 6)
})

test_that("two trivial partitions that agree give 1 rather than NaN", {
  ## Both all-in-one-cluster, and both all-singletons: the index has no variance
  ## to adjust, but the partitions ARE the same and a NaN would poison the mean
  ## of an otherwise informative stability report.
  expect_equal(.searchnet_ari(rep(1L, 5), rep(2L, 5)), 1)
  expect_equal(.searchnet_ari(1:5, 5:1), 1)
  ## All-in-one against all-singletons is a genuine disagreement, not a tie.
  expect_lt(.searchnet_ari(rep(1L, 6), 1:6), 0.5)
})

test_that("unrelated partitions score near zero on average", {
  set.seed(4321)
  v <- replicate(200, .searchnet_ari(sample.int(3, 30, replace = TRUE),
                                     sample.int(3, 30, replace = TRUE)))
  expect_lt(abs(mean(v)), 0.05)
  ## Chance-adjusted, so negative values must be possible.
  expect_true(any(v < 0))
  expect_true(all(v <= 1))
})

test_that("mismatched lengths and degenerate inputs are handled", {
  expect_error(.searchnet_ari(1:3, 1:4), "different length", fixed = TRUE)
  expect_true(is.na(.searchnet_ari(1L, 1L)))
})


# ===========================================================================
# 17. searchnet_repertoire_stability()
#
# A repertoire is a property of ONE sampled history. The docstring has always
# said to check stability across chains; until now it provided no way to.
# ===========================================================================

## Three chains that share planted actor types but differ in the noise, as
## repeated ministep chains from one fitted model would.
stability_chains <- function(n_chain = 3L, sd = 0.4, seed = 1234L) {
  set.seed(seed)
  grp <- rep(1:3, each = 5)
  lapply(seq_len(n_chain), function(i) {
    spec <- data.frame(
      actor        = 1:15,
      focusing     = c(0, 10, 20)[grp] + stats::rnorm(15, 0, sd),
      reinforcing  = c(0, 10, 20)[grp] + stats::rnorm(15, 0, sd),
      mixing       = c(0, 10, 20)[grp] + stats::rnorm(15, 0, sd),
      clustering   = c(0, 10, 20)[grp] + stats::rnorm(15, 0, sd),
      create_share = c(0, 0.5, 1)[grp],
      stringsAsFactors = FALSE)
    make_repertoire_stats(spec, n_events = 6L)
  })
}

test_that("chains carrying the same actor types agree almost perfectly", {
  st <- searchnet_repertoire_stability(stability_chains(), k = 3, seed = 1)
  expect_s3_class(st, "searchnet_repertoire_stability")
  expect_equal(st$k, 3L)
  expect_equal(st$n_chains, 3L)
  expect_equal(st$mean_ari, 1)
  expect_equal(st$min_ari, 1)
  expect_equal(nrow(st$pairs), 3L)          ## C(3, 2) unordered pairs
  expect_named(st$pairs, c("chain_i", "chain_j", "n_common", "ari"))
  expect_true(all(st$pairs$n_common == 15L))
  expect_equal(st$common_actors, 1:15)
  ## The matrix form agrees with the pair list and is symmetric with a unit
  ## diagonal, which is what makes it safe to average or to plot.
  expect_equal(dim(st$ari), c(3L, 3L))
  expect_equal(unname(diag(st$ari)), rep(1, 3))
  expect_equal(st$ari, t(st$ari))
  expect_equal(unname(st$ari[1, 2]), st$pairs$ari[1])
})

test_that("chains carrying nothing agree no better than chance", {
  ## The companion, and the more important one: a procedure that reported high
  ## stability here would report it everywhere.
  set.seed(31337)
  chains <- lapply(1:4, function(i) {
    n_ev <- 15 * 6
    data.frame(actor       = rep(1:15, each = 6),
               change      = sample(c("create", "delete"), n_ev, replace = TRUE),
               focusing    = stats::rnorm(n_ev),
               reinforcing = stats::rnorm(n_ev),
               mixing      = stats::rnorm(n_ev),
               clustering  = stats::rnorm(n_ev),
               stringsAsFactors = FALSE)
  })
  st <- searchnet_repertoire_stability(chains, k = 3, seed = 1)
  expect_equal(nrow(st$pairs), 6L)
  expect_lt(st$mean_ari, 0.3)
  ## and the two fixtures are ordered the way the construct requires.
  good <- searchnet_repertoire_stability(stability_chains(), k = 3, seed = 1)
  expect_gt(good$mean_ari, st$mean_ari)
})

test_that("noisier chains degrade agreement", {
  ## Direction only: the index has to be sensitive to how much the chains differ,
  ## or it is not measuring stability.
  tight <- searchnet_repertoire_stability(stability_chains(sd = 0.4), k = 3,
                                          seed = 1)
  loose <- searchnet_repertoire_stability(stability_chains(sd = 9), k = 3,
                                          seed = 1)
  expect_gt(tight$mean_ari, loose$mean_ari)
})

test_that("only actors assigned in BOTH chains are scored, and the count is reported", {
  ## An actor that clears min_events in one chain and not the other carries no
  ## information about agreement. Chain 2 loses actor 15 to the threshold.
  ch <- stability_chains(n_chain = 2L)
  keep <- !(ch[[2]]$actor == 15 &
              stats::ave(seq_len(nrow(ch[[2]])), ch[[2]]$actor,
                         FUN = seq_along) > 3)
  ch[[2]] <- ch[[2]][keep, , drop = FALSE]
  st <- searchnet_repertoire_stability(ch, k = 3, seed = 1)
  expect_equal(st$pairs$n_common, 14L)
  expect_equal(st$common_actors, 1:14)
  expect_false(15L %in% st$common_actors)
  ## The per-chain fits are returned, so the exclusion is auditable rather than
  ## something the caller has to take on trust.
  expect_equal(nrow(st$assignments[[1]]$assignment), 15L)
  expect_equal(nrow(st$assignments[[2]]$assignment), 14L)
  expect_equal(st$assignments[[2]]$dropped$actor, 15L)
})

test_that("chain names are carried through when the list has them", {
  ch <- stability_chains()
  names(ch) <- c("burnin", "main", "long")
  st <- searchnet_repertoire_stability(ch, k = 3, seed = 1)
  expect_equal(rownames(st$ari), c("burnin", "main", "long"))
  expect_setequal(unique(c(st$pairs$chain_i, st$pairs$chain_j)),
                  c("burnin", "main", "long"))
  ## Unnamed lists get positional names rather than NULL row labels.
  st2 <- searchnet_repertoire_stability(stability_chains(), k = 3, seed = 1)
  expect_equal(rownames(st2$ari), c("chain1", "chain2", "chain3"))
})

test_that("k is required, and the error says why", {
  ch <- stability_chains()
  expect_error(searchnet_repertoire_stability(ch), "`k` is required",
               fixed = TRUE)
  expect_error(searchnet_repertoire_stability(ch, k = NULL),
               "different granularity", fixed = TRUE)
})

test_that("one chain is refused with a message about what stability is", {
  expect_error(searchnet_repertoire_stability(stability_chains(n_chain = 1L),
                                              k = 3),
               "at least two chains", fixed = TRUE)
  ## The likely user error -- passing a single frame instead of a list -- is
  ## caught by name rather than dying inside lapply().
  expect_error(searchnet_repertoire_stability(structured_stats, k = 3),
               "must be a LIST", fixed = TRUE)
})

test_that("a chain that cannot be clustered names itself in the error", {
  ch <- stability_chains(n_chain = 2L)
  names(ch) <- c("ok", "broken")
  ch[[2]]$focusing <- NULL
  expect_error(searchnet_repertoire_stability(ch, k = 3), "chain broken",
               fixed = TRUE)
})

test_that("extra arguments reach every chain's fit", {
  st <- searchnet_repertoire_stability(stability_chains(), k = 3, seed = 1,
                                       features = c("focusing", "clustering"))
  expect_true(all(vapply(st$assignments,
                         function(f) identical(f$features,
                                               c("focusing", "clustering")),
                         logical(1))))
})

test_that("print.searchnet_repertoire_stability reports the pairs and the caveat", {
  st <- searchnet_repertoire_stability(stability_chains(), k = 3, seed = 1)
  txt <- capture.output(print(st))
  expect_true(any(grepl("Repertoire stability across 3 chains (k = 3)", txt,
                        fixed = TRUE)))
  expect_true(any(grepl("Pairwise adjusted Rand index", txt, fixed = TRUE)))
  expect_true(any(grepl("n_common", txt, fixed = TRUE)))
  ## The interpretive instruction is part of the output, as in the other two
  ## print methods in this file.
  expect_true(any(grepl("chance-adjusted", txt, fixed = TRUE)))
  vis <- NULL
  capture.output(vis <- withVisible(print(st)))
  expect_false(vis$visible)
  expect_identical(vis$value, st)
})


# ===========================================================================
# 18. print.searchnet_repertoire_null(): the new diagnostic lines
# ===========================================================================

test_that("the null's print says how the permutation was stratified", {
  res <- searchnet_repertoire_null(structured_stats, n_perm = 9L, k = 3,
                                   seed = 4)
  txt <- capture.output(print(res))
  expect_true(any(grepl("keeps its own event count", txt, fixed = TRUE)))
  ## And the free permutation carries the warning that it does not.
  free <- searchnet_repertoire_null(structured_stats, n_perm = 9L, k = 3,
                                    strata = "none", seed = 4)
  ftxt <- capture.output(print(free))
  expect_true(any(grepl("activity volume NOT held fixed", ftxt, fixed = TRUE)))
  expect_true(any(grepl("may only say actors differ in HOW MUCH", ftxt,
                        fixed = TRUE)))
})

test_that("the null's print flags the anticonservative k policy when it is used", {
  fx <- searchnet_repertoire_null(structured_stats, n_perm = 9L, k = NULL,
                                  k_range = 2:4, k_selection = "fixed", seed = 4)
  txt <- capture.output(print(fx))
  expect_true(any(grepl("ANTICONSERVATIVE", txt, fixed = TRUE)))

  re <- searchnet_repertoire_null(structured_stats, n_perm = 9L, k = NULL,
                                  k_range = 2:4, seed = 4)
  rtxt <- capture.output(print(re))
  expect_true(any(grepl("max vs max", rtxt, fixed = TRUE)))
  expect_false(any(grepl("ANTICONSERVATIVE", rtxt, fixed = TRUE)))

  ## With k supplied there is no selection to describe, so neither line appears.
  sup <- searchnet_repertoire_null(structured_stats, n_perm = 9L, k = 3, seed = 4)
  stxt <- capture.output(print(sup))
  expect_false(any(grepl("k selection", stxt, fixed = TRUE)))
})

test_that("print.searchnet_repertoire notes residualisation when it was used", {
  skip_if_not(exists("searchnet_chain_stats", mode = "function"))
  cs <- volume_only_stats()
  res <- searchnet_repertoire(cs, k = 3, seed = 1, residualise = TRUE)
  txt <- capture.output(print(res))
  expect_true(any(grepl("residualised on log(n_events)", txt, fixed = TRUE)))
  ## and says nothing when it was not, so the default output is unchanged.
  plain <- searchnet_repertoire(cs, k = 3, seed = 1)
  expect_false(any(grepl("residualised", capture.output(print(plain)),
                         fixed = TRUE)))
})
