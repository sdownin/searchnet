###############################################################################
## test-formal-utility.R
## Unit tests for SaomNkRSienaBiEnv$compute_formal_utility()
###############################################################################

## ---- Helper: create an env with a specific bipartite matrix ----------------
make_utility_env <- function(bi_mat, seed = 200) {
  M <- nrow(bi_mat)
  N <- ncol(bi_mat)
  params <- make_small_environ_params(M = M, N = N, rand_seed = seed)
  params$init_matrix <- bi_mat
  env <- SaomNkRSienaBiEnv$new(params)
  env
}

## ==========================================================================
## Test 1: Empty bipartite (all zeros) -- all utility components should be 0
## ==========================================================================
test_that("all-zero bipartite gives zero for every utility component", {
  skip_if_not_installed("RSiena")

  M <- 3; N <- 4
  bi_mat <- matrix(0L, M, N)
  env <- make_utility_env(bi_mat, seed = 201)

  # No fitness landscape => nk_fitness = 0 by default
  util <- env$compute_formal_utility(
    beta_F = 1, beta_s = 1, beta_w = 1, beta_h = 1,
    beta_cong = 1, beta_disp = 1, beta_comp = 1,
    beta_clos = 1, beta_riv = 1, beta_leg = 1
  )

  expect_equal(nrow(util), M)
  for (col in c("nk_fitness", "scope_cost", "synergy", "herding",
                "congestion", "displacement", "complementarity",
                "closure", "rivalry", "legitimacy", "total")) {
    expect_true(all(util[[col]] == 0),
                info = paste(col, "should be 0 for all-zero bipartite"))
  }
})

## ==========================================================================
## Test 2: Scope cost = -beta_s * (k/N)^2
##   Actor holding k activities, only scope cost turned on
## ==========================================================================
test_that("scope cost equals -beta_s * (k/N)^2", {
  skip_if_not_installed("RSiena")

  M <- 3; N <- 6
  # Actor 1 holds 2 activities, actor 2 holds 4, actor 3 holds 0
  bi_mat <- matrix(0L, M, N)
  bi_mat[1, 1:2] <- 1
  bi_mat[2, 1:4] <- 1
  env <- make_utility_env(bi_mat, seed = 202)

  beta_s <- 0.7
  util <- env$compute_formal_utility(
    beta_F = 0, beta_s = beta_s, beta_w = 0, beta_h = 0,
    beta_cong = 0, beta_disp = 0, beta_comp = 0,
    beta_clos = 0, beta_riv = 0, beta_leg = 0
  )

  expect_equal(util$scope_cost[1], -beta_s * (2 / N)^2, tolerance = 1e-12)
  expect_equal(util$scope_cost[2], -beta_s * (4 / N)^2, tolerance = 1e-12)
  expect_equal(util$scope_cost[3], 0, tolerance = 1e-12,
               info = "zero activities => zero scope cost")
})

## ==========================================================================
## Test 3: Synergy = beta_w * b'Wb / N^2 for known W
##   W = [[0,1],[1,0]], b_i = (1,1) => b'Wb = 2, synergy = 2/4 = 0.5
## ==========================================================================
test_that("synergy equals beta_w * b'Wb / N^2 for known W", {
  skip_if_not_installed("RSiena")

  M <- 2; N <- 2
  bi_mat <- matrix(c(1, 1,
                      1, 0), nrow = M, ncol = N, byrow = TRUE)
  env <- make_utility_env(bi_mat, seed = 203)

  # Inject a known W matrix
  W <- matrix(c(0, 1, 1, 0), N, N)
  env$component_1_coDyadCovar <- W

  beta_w <- 1.0
  util <- env$compute_formal_utility(
    beta_F = 0, beta_s = 0, beta_w = beta_w, beta_h = 0,
    beta_cong = 0, beta_disp = 0, beta_comp = 0,
    beta_clos = 0, beta_riv = 0, beta_leg = 0
  )

  # Actor 1: b=(1,1), b'Wb = 1*0*1+1*1*1+1*1*1+1*0*1 = 2, synergy = 2/4 = 0.5
  expect_equal(util$synergy[1], beta_w * 2 / N^2, tolerance = 1e-12)

  # Actor 2: b=(1,0), only 1 activity held => synergy = 0
  expect_equal(util$synergy[2], 0, tolerance = 1e-12,
               info = "fewer than 2 held activities => synergy = 0")
})

## ==========================================================================
## Test 4: Herding -- two identical actors vs two disjoint actors
##   herding_i = mean overlap with others / N
## ==========================================================================
test_that("herding is higher for identical actors than disjoint", {
  skip_if_not_installed("RSiena")

  N <- 4

  # Identical: both actors hold activities 1-2
  bi_ident <- matrix(c(1, 1, 0, 0,
                        1, 1, 0, 0), nrow = 2, ncol = N, byrow = TRUE)
  env_ident <- make_utility_env(bi_ident, seed = 204)

  util_ident <- env_ident$compute_formal_utility(
    beta_F = 0, beta_s = 0, beta_w = 0, beta_h = 1,
    beta_cong = 0, beta_disp = 0, beta_comp = 0,
    beta_clos = 0, beta_riv = 0, beta_leg = 0
  )

  # Disjoint: actor 1 holds {1,2}, actor 2 holds {3,4}
  bi_disj <- matrix(c(1, 1, 0, 0,
                       0, 0, 1, 1), nrow = 2, ncol = N, byrow = TRUE)
  env_disj <- make_utility_env(bi_disj, seed = 205)

  util_disj <- env_disj$compute_formal_utility(
    beta_F = 0, beta_s = 0, beta_w = 0, beta_h = 1,
    beta_cong = 0, beta_disp = 0, beta_comp = 0,
    beta_clos = 0, beta_riv = 0, beta_leg = 0
  )

  # Identical: overlap = 2, herding = 2/N = 0.5 for each actor
  expect_equal(util_ident$herding[1], 2 / N, tolerance = 1e-12)
  # Disjoint: overlap = 0, herding = 0
  expect_equal(util_disj$herding[1], 0, tolerance = 1e-12)
  # Identical herding > disjoint herding
  expect_true(util_ident$herding[1] > util_disj$herding[1])
})

## ==========================================================================
## Test 5: Congestion -- all actors on same activity vs spread out
##   congestion_i = sum_j b_{ij} * n_j / (M * N)
## ==========================================================================
test_that("congestion is higher when all actors share same activity", {
  skip_if_not_installed("RSiena")

  M <- 4; N <- 4

  # All on activity 1
  bi_same <- matrix(0L, M, N)
  bi_same[, 1] <- 1
  env_same <- make_utility_env(bi_same, seed = 206)

  util_same <- env_same$compute_formal_utility(
    beta_F = 0, beta_s = 0, beta_w = 0, beta_h = 0,
    beta_cong = 1, beta_disp = 0, beta_comp = 0,
    beta_clos = 0, beta_riv = 0, beta_leg = 0
  )

  # Spread out: each actor on a different activity
  bi_spread <- diag(M)  # M=N=4
  env_spread <- make_utility_env(bi_spread, seed = 207)

  util_spread <- env_spread$compute_formal_utility(
    beta_F = 0, beta_s = 0, beta_w = 0, beta_h = 0,
    beta_cong = 1, beta_disp = 0, beta_comp = 0,
    beta_clos = 0, beta_riv = 0, beta_leg = 0
  )

  # All on same: n_1 = M, congestion_i = 1*M/(M*N) = 1/N
  expect_equal(util_same$congestion[1], -1 * M / (M * N), tolerance = 1e-12)
  # Spread: n_j = 1 for each, congestion_i = 1*1/(M*N) = 1/(M*N)
  expect_equal(util_spread$congestion[1], -1 * 1 / (M * N), tolerance = 1e-12)
  # Same-activity congestion should be worse (more negative)
  expect_true(util_same$congestion[1] < util_spread$congestion[1])
})

## ==========================================================================
## Test 6: Legitimacy Hill function
##   n_j = M (all hold) vs n_j = 1 (only one holds)
##   legitimacy = mean( b_i * n_j^h / (K^h * M^h + n_j^h) )
## ==========================================================================
test_that("legitimacy is higher when all actors hold the activity", {
  skip_if_not_installed("RSiena")

  M <- 4; N <- 3
  h_hill <- 2; K_half <- 0.4

  # All actors hold activity 1 only
  bi_all <- matrix(0L, M, N)
  bi_all[, 1] <- 1
  env_all <- make_utility_env(bi_all, seed = 208)

  util_all <- env_all$compute_formal_utility(
    beta_F = 0, beta_s = 0, beta_w = 0, beta_h = 0,
    beta_cong = 0, beta_disp = 0, beta_comp = 0,
    beta_clos = 0, beta_riv = 0, beta_leg = 1,
    h_hill = h_hill, K_half = K_half
  )

  # Only actor 1 holds activity 1
  bi_one <- matrix(0L, M, N)
  bi_one[1, 1] <- 1
  env_one <- make_utility_env(bi_one, seed = 209)

  util_one <- env_one$compute_formal_utility(
    beta_F = 0, beta_s = 0, beta_w = 0, beta_h = 0,
    beta_cong = 0, beta_disp = 0, beta_comp = 0,
    beta_clos = 0, beta_riv = 0, beta_leg = 1,
    h_hill = h_hill, K_half = K_half
  )

  # Verify exact value for actor 1, all-hold case: n_1=M=4
  # legitimacy = mean(b_i * n_j^h / (K^h*M^h + n_j^h))
  # b_i = (1,0,0), only j=1 contributes: M^h / (K^h*M^h + M^h) = 1 / (K^h + 1)
  expected_all <- (1 / (K_half^h_hill + 1)) / N  # mean over N dims, only 1 nonzero
  # Wait: formula is mean(b_i * ...) where mean is over all N, not just held
  # Actually looking at code: mean(b_i * n_j^h / (K^h*M^h + n_j^h))
  # But b_i has scope=1, and legitimacy uses: mean(b_i * ...)
  # For actor 1 in env_all: b_i=(1,0,0), n_j=(4,0,0)
  # Only j=1 nonzero: 4^2 / (0.4^2 * 4^2 + 4^2) = 16 / (2.56 + 16) = 16/18.56
  hill_val <- M^h_hill / (K_half^h_hill * M^h_hill + M^h_hill)
  expected_all_exact <- hill_val / N  # mean spreads over N dimensions
  # Correction: looking at code more carefully:
  # mean(b_i * n_j^h_hill / (K_half^h_hill * M^h_hill + n_j^h_hill))
  # This is sum / N since scope>0 path just uses mean() over full vector
  # Actually no: it uses mean() which divides by N (length of b_i)
  # Wait, looking at code again: "mean(b_i * n_j^h_hill / ...)" -- this is mean over N elements
  # But scope > 0 guard just wraps this. So:
  # For all-hold, actor 1: b_i = (1,0,0), n_j = (4,0,0)
  # Element 1: 1 * 4^2 / (0.16*16 + 16) = 1 * 16/18.56
  # Elements 2,3: 0
  # mean = 16/18.56 / 3
  # Hmm, but legitimacy_val for scope=0 is 0. The code says:
  # legitimacy_val <- if (scope > 0) { mean(b_i * n_j^h / ...) } else 0

  # Just verify relative ordering: all-hold > one-hold
  expect_true(util_all$legitimacy[1] > util_one$legitimacy[1],
              info = "Full adoption should yield higher legitimacy")
  expect_true(util_all$legitimacy[1] > 0,
              info = "Legitimacy should be positive when all actors hold activity")

  # Actor with zero activities has zero legitimacy (use env_one where only actor 1 holds)
  expect_equal(util_one$legitimacy[2], 0, tolerance = 1e-12,
               info = "Actor holding nothing has zero legitimacy")
})

## ==========================================================================
## Test 7: Total equals sum of the 10 individual components
## ==========================================================================
test_that("total equals sum of the 10 utility components", {
  skip_if_not_installed("RSiena")

  M <- 4; N <- 4
  set.seed(300)
  bi_mat <- matrix(sample(0:1, M * N, replace = TRUE), M, N)
  env <- make_utility_env(bi_mat, seed = 301)

  util <- env$compute_formal_utility(
    beta_F = 0, beta_s = 0.5, beta_w = 0.3, beta_h = 0.2,
    beta_cong = 0.1, beta_disp = 0.1, beta_comp = 0.1,
    beta_clos = 0.05, beta_riv = 0.05, beta_leg = 0.1
  )

  component_cols <- c("nk_fitness", "scope_cost", "synergy", "herding",
                       "congestion", "displacement", "complementarity",
                       "closure", "rivalry", "legitimacy")
  row_sums <- rowSums(util[, component_cols])

  for (i in 1:M) {
    expect_equal(util$total[i], row_sums[i], tolerance = 1e-12,
                 info = sprintf("Actor %d: total should equal sum of components", i))
  }
})

## ==========================================================================
## Test 8: NK component isolated (beta_F=1, all others=0)
##   With a fitness landscape, only NK contributes
## ==========================================================================
test_that("NK component is sole contributor when beta_F=1, all others=0", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("psych")

  M <- 3; N <- 4
  set.seed(310)
  bi_mat <- matrix(sample(0:1, M * N, replace = TRUE, prob = c(0.5, 0.5)),
                   M, N)
  env <- make_utility_env(bi_mat, seed = 311)

  # Generate a fitness landscape so NK component is nonzero
  tryCatch(
    env$compute_fitness_landscape(
      n_landscapes = 1,
      project_int_mat = TRUE,
      verbose = FALSE
    ),
    error = function(e) stop(paste("compute_fitness_landscape failed:", e$message))
  )

  util <- env$compute_formal_utility(
    beta_F = 1, beta_s = 0, beta_w = 0, beta_h = 0,
    beta_cong = 0, beta_disp = 0, beta_comp = 0,
    beta_clos = 0, beta_riv = 0, beta_leg = 0
  )

  # Total should equal nk_fitness
  for (i in 1:M) {
    expect_equal(util$total[i], util$nk_fitness[i], tolerance = 1e-12,
                 info = sprintf("Actor %d: total = nk_fitness when only beta_F", i))
  }

  # All non-NK components should be zero
  for (col in c("scope_cost", "synergy", "herding", "congestion",
                "displacement", "complementarity", "closure",
                "rivalry", "legitimacy")) {
    expect_true(all(util[[col]] == 0),
                info = paste(col, "should be 0 when its beta = 0"))
  }
})

## ==========================================================================
## Test 9: Returns one row per actor
## ==========================================================================
test_that("compute_formal_utility returns one row per actor", {
  skip_if_not_installed("RSiena")

  M <- 4; N <- 5
  set.seed(320)
  bi_mat <- matrix(sample(0:1, M * N, replace = TRUE), M, N)
  env <- make_utility_env(bi_mat, seed = 321)

  # All actors
  util_all <- env$compute_formal_utility()
  expect_equal(nrow(util_all), M,
               info = "Should return M rows when actor_id = NULL")
  expect_equal(util_all$actor_id, 1:M)

  # Single actor
  util_one <- env$compute_formal_utility(actor_id = 2)
  expect_equal(nrow(util_one), 1)
  expect_equal(util_one$actor_id, 2)

  # Subset of actors
  util_sub <- env$compute_formal_utility(actor_id = c(1, 3))
  expect_equal(nrow(util_sub), 2)
  expect_equal(util_sub$actor_id, c(1, 3))

  # Expected columns
  expected_cols <- c("actor_id", "nk_fitness", "scope_cost", "synergy",
                     "herding", "congestion", "displacement",
                     "complementarity", "closure", "rivalry",
                     "legitimacy", "total")
  expect_equal(names(util_all), expected_cols)
})
