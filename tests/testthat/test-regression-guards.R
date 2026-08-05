# Regression guards for defects that previously reached users.
# Each test names the failure it prevents so the intent survives refactoring.

# ---------------------------------------------------------------------------
# GUARD 1: package must install and load as a package.
#
# A stale copy of the standalone loader once lived in R/. Because it made
# top-level source()/library() calls with a path that only resolved in a dev
# checkout, it broke BOTH `R CMD INSTALL` ("cannot open file .../utils.R") and
# `devtools::load_all()`. The canonical loader belongs in inst/, never R/.
# ---------------------------------------------------------------------------

test_that("package namespace loads and core API is exported", {
  expect_true(requireNamespace("searchnet", quietly = TRUE))
  for (fn in c("saomnk_env", "saomnk_model", "saomnk_run",
               "saomnk_get_degrees", "saomnk_block_diagonal",
               "searchnet_game_init", "searchnet_game_step",
               "searchnet_classroom_init")) {
    expect_true(exists(fn, where = asNamespace("searchnet"), inherits = FALSE),
                info = paste("missing export:", fn))
  }
})

test_that("no stray loader script sits in R/ (breaks R CMD INSTALL)", {
  # Only meaningful when run from a source checkout; skip on installed pkg.
  rdir <- file.path("..", "..", "R")
  skip_if_not(dir.exists(rdir), "not a source checkout")
  stray <- list.files(rdir, pattern = "loader", ignore.case = TRUE)
  expect_length(stray, 0)
})

# ---------------------------------------------------------------------------
# GUARD 2: game step must not recompute choice probabilities per actor.
#
# searchnet_game_step() once called env$compute_choice_probabilities() inside
# the actor loop. That function evaluates ALL M actors per call, so the loop did
# M times the necessary work and discarded the rest, giving ~M^2*N scaling
# (18 s/move at M=20,N=30). Fixed in 0.3.1 by hoisting the call.
#
# Guard: cost must grow sub-quadratically in M.
# ---------------------------------------------------------------------------

test_that("game_step scales sub-quadratically in M", {
  skip_on_cran()
  timed <- function(M, N) {
    g <- searchnet_game_init(M = M, N = N, seed = 42)
    system.time(for (i in 1:3) g <- searchnet_game_step(g, "add", (i %% N) + 1)
                )[["elapsed"]]
  }
  t_small <- timed(6, 10)
  t_large <- timed(18, 10)   # 3x the actors

  # Under the old per-actor bug this ratio ran ~9x+ (quadratic). Allow generous
  # headroom for timing noise but fail if quadratic behaviour returns.
  skip_if(t_small < 0.05, "timing resolution too coarse on this machine")
  expect_lt(t_large / t_small, 6)
})

# ---------------------------------------------------------------------------
# GUARD 3: within a round, AI moves must not depend on actor ordering.
#
# Before 0.3.1 each AI observed the partially-updated board left by
# lower-indexed actors, so outcomes depended on iteration order. 0.3.1 made all
# AI respond to the same round-start state (simultaneous moves), matching
# searchnet_classroom_advance().
#
# Guard: identical seed + identical action must reproduce identical state.
# ---------------------------------------------------------------------------

test_that("game_step is deterministic given a seed (simultaneous-move)", {
  play <- function() {
    set.seed(99)
    g <- searchnet_game_init(M = 8, N = 10, seed = 123)
    for (a in c(2, 5, 3)) g <- searchnet_game_step(g, "add", a)
    g$env$bipartite_matrix
  }
  expect_identical(play(), play())
})

# ---------------------------------------------------------------------------
# GUARD 4: game API returns well-formed structures.
# ---------------------------------------------------------------------------

test_that("game API surface is well-formed end to end", {
  g <- searchnet_game_init(M = 6, N = 8, seed = 7)
  expect_s3_class(g, "searchnet_game")

  g <- searchnet_game_step(g, "add", 3)
  expect_equal(g$round, 1L)

  sb <- searchnet_game_scoreboard(g)
  expect_true(is.data.frame(sb))
  expect_equal(nrow(sb), 6L)

  mv <- searchnet_game_available_moves(g)
  expect_true(is.data.frame(mv))
  expect_true(nrow(mv) > 0)

  expect_no_error(searchnet_game_summary(g))
})

# ---------------------------------------------------------------------------
# GUARD 5: degree accessors return usable panels.
#
# Downstream analysis merges K_AC with actor utilities on
# (actor_id, chain_step_id). Silent renaming of those columns would break every
# analysis script without erroring.
# ---------------------------------------------------------------------------

test_that("saomnk_get_degrees returns the K4 panel with stable column names", {
  env <- saomnk_env(M = 6, N = 8, density = 0.3, seed = 5)
  mod <- saomnk_model(density = -0.5, popularity = 0.1, scope = -0.05,
                      influence_matrix = saomnk_block_diagonal(8, 2),
                      influence_weight = 0.1)
  saomnk_run(env, mod, steps_per_actor = 3, seed = 5, verbose = FALSE)

  d <- saomnk_get_degrees(env)
  for (ch in c("K_AC", "K_CA")) {
    expect_true(ch %in% names(d), info = paste("missing channel:", ch))
    expect_true(all(c("actor_id", "chain_step_id", "value") %in% names(d[[ch]])) ||
                all(c("component_id", "chain_step_id", "value") %in% names(d[[ch]])),
                info = paste("unexpected columns in", ch))
  }
})
