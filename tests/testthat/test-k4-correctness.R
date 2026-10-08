###############################################################################
## test-k4-correctness.R
## Mathematical correctness tests for the K-4 coupled degree measures
##
## Philosophy: "Users don't have to trust us -- they just trust the tests."
## These tests verify the K-4 degree computations against hand-computable
## examples. A reviewer can check every assertion against the definitions:
##
##   B      = bipartite matrix (M x N)
##   K_AC   = rowSums(B)                               (actor scope)
##   K_CA   = colSums(B)                               (component popularity)
##   K_AA   = rowSums( offdiag(B %*% t(B)) > 0 )       (actor sociality)
##   K_CC   = colSums( offdiag(t(B) %*% B) > 0 )       (component epistasis)
##
## The {K} definitions in inst/rosetta/K_DIMENSIONS.md:
##   K_AA(i) counts the OTHER actors sharing at least one component with i
##        (positive OFF-diagonal entries of row i of B %*% t(B)).
##   K_CC(j) counts the OTHER components co-held with j
##        (positive OFF-diagonal entries of column j of t(B) %*% B).
## Through searchnet 0.12.1 the engine also counted the node itself (the
## diagonal), so every non-isolated node's K_AA / K_CC was 1 too large. The
## values below pin the exclusive definition.
###############################################################################

## Exclusive projection degrees, computed independently of the engine.
.k4_offdiag_degree <- function(P) { diag(P) <- 0; rowSums(P > 0) }


# ===========================================================================
# 1. Known small matrix produces expected K values
# ===========================================================================
test_that("K_AC = rowSums of bipartite matrix for known input", {
  ## B = [1 0 1 0]
  ##     [0 1 1 0]
  ##     [1 1 0 1]
  B <- matrix(c(1,0,1, 0,1,1, 1,1,0, 0,0,1), nrow = 3, ncol = 4)
  ## Expected K_AC (actor scope): c(2, 2, 3)
  K_AC <- rowSums(B)
  expect_equal(K_AC, c(2, 2, 3))
})

test_that("K_CA = colSums of bipartite matrix for known input", {
  B <- matrix(c(1,0,1, 0,1,1, 1,1,0, 0,0,1), nrow = 3, ncol = 4)
  ## Expected K_CA (component popularity): c(2, 2, 2, 1)
  K_CA <- colSums(B)
  expect_equal(K_CA, c(2, 2, 2, 1))
})

test_that("K_AA (actor sociality) = positive entries per row of B %*% t(B)", {
  B <- matrix(c(1,0,1, 0,1,1, 1,1,0, 0,0,1), nrow = 3, ncol = 4)
  ## B %*% t(B):
  ##   Row 1: (1*1+0*0+1*1+0*0, 1*0+0*1+1*1+0*0, 1*1+0*1+1*0+0*1) = (2, 1, 1)
  ##   Row 2: (0*1+1*0+1*1+0*0, 0*0+1*1+1*1+0*0, 0*1+1*1+1*0+0*1) = (1, 2, 1)
  ##   Row 3: (1*1+1*0+0*1+1*0, 1*0+1*1+0*1+1*0, 1*1+1*1+0*0+1*1) = (1, 1, 3)
  BBt <- B %*% t(B)
  ## K_AA counts the OTHER actors sharing a component (diagonal excluded)
  K_AA <- .k4_offdiag_degree(BBt)
  ## Actor 1 shares components with actors 2 and 3: off-diagonal (1,1) => 2
  ## Actor 2 shares components with actors 1 and 3: off-diagonal (1,1) => 2
  ## Actor 3 shares components with actors 1 and 2: off-diagonal (1,1) => 2
  expect_equal(K_AA, c(2, 2, 2))
})

test_that("K_CC (component epistasis) = positive entries per col of t(B) %*% B", {
  B <- matrix(c(1,0,1, 0,1,1, 1,1,0, 0,0,1), nrow = 3, ncol = 4)
  BtB <- t(B) %*% B
  ## K_CC counts the OTHER components co-held with j (diagonal excluded)
  K_CC <- .k4_offdiag_degree(BtB)
  ## t(B) %*% B off the diagonal:
  ##   comp 1 (actors 1,3): co-held with 2 (actor 3), 3 (actor 1), 4 (actor 3) => 3
  ##   comp 2 (actors 2,3): co-held with 1, 3 (actor 2), 4 (actor 3)           => 3
  ##   comp 3 (actors 1,2): co-held with 1 (actor 1), 2 (actor 2)              => 2
  ##   comp 4 (actor 3):    co-held with 1, 2 (actor 3)                        => 2
  expect_equal(unname(K_CC), c(3, 3, 2, 2))
})


# ===========================================================================
# 2. Isolated actor (zero row) produces K_AC = 0 and K_AA = 0
# ===========================================================================
test_that("isolated actor has K_AC = 0 and K_AA = 0", {
  ## Actor 2 has no components
  B <- matrix(c(1,0,0, 1,0,0, 0,0,0), nrow = 3, ncol = 3)
  K_AC <- rowSums(B)
  expect_equal(K_AC[2], 0)

  BBt <- B %*% t(B)
  K_AA <- .k4_offdiag_degree(BBt)
  ## Actor 2 contributes nothing to BBt => row 2 is all zeros
  expect_equal(K_AA[2], 0)
  ## Actor 1 holds components 1 and 2, which nobody else holds: it is not
  ## isolated but has no partner (the self-inclusive count used to report 1)
  expect_equal(K_AA[1], 0)
})

test_that("isolated component has K_CA = 0 and K_CC = 0", {
  ## Component 3 has no actors
  B <- matrix(c(1,0, 1,0, 0,0), nrow = 2, ncol = 3)
  K_CA <- colSums(B)
  expect_equal(K_CA[3], 0)

  BtB <- t(B) %*% B
  K_CC <- .k4_offdiag_degree(BtB)
  expect_equal(K_CC[3], 0)
  ## Components 1 and 2 are both held by actor 1: one partner each
  expect_equal(K_CC[1:2], c(1, 1))
})


# ===========================================================================
# 3. K_AA and K_CC are based on symmetric matrices
# ===========================================================================
test_that("B %*% t(B) is symmetric (K_AA projection)", {
  set.seed(99)
  B <- matrix(sample(0:1, 5 * 8, replace = TRUE), nrow = 5, ncol = 8)
  BBt <- B %*% t(B)
  expect_equal(BBt, t(BBt))
})

test_that("t(B) %*% B is symmetric (K_CC projection)", {
  set.seed(99)
  B <- matrix(sample(0:1, 5 * 8, replace = TRUE), nrow = 5, ncol = 8)
  BtB <- t(B) %*% B
  expect_equal(BtB, t(BtB))
})


# ===========================================================================
# 4. Full bipartite matrix has maximal K values
# ===========================================================================
test_that("all-ones bipartite matrix gives maximal K_AC and K_CA", {
  M <- 4; N <- 6
  B <- matrix(1, nrow = M, ncol = N)
  expect_equal(rowSums(B), rep(N, M))
  expect_equal(colSums(B), rep(M, N))
})

test_that("all-ones bipartite matrix gives K_AA = M - 1 and K_CC = N - 1", {
  M <- 4; N <- 6
  B <- matrix(1, nrow = M, ncol = N)
  BBt <- B %*% t(B)
  K_AA <- .k4_offdiag_degree(BBt)
  ## Every actor shares all components with every OTHER actor
  expect_equal(K_AA, rep(M - 1, M))

  BtB <- t(B) %*% B
  K_CC <- .k4_offdiag_degree(BtB)
  expect_equal(K_CC, rep(N - 1, N))
})


# ===========================================================================
# 5. Identity-like bipartite (each actor has unique component) => K_AA = 0
# ===========================================================================
test_that("diagonal bipartite gives K_AA = 0 (no shared components)", {
  ## 4 actors, 4 components, each actor affiliates with exactly one unique component
  B <- diag(4)
  BBt <- B %*% t(B)
  K_AA <- .k4_offdiag_degree(BBt)
  ## No actor shares a component with any other: no partner, though the
  ## actor is not isolated (the self-inclusive count used to report 1)
  expect_equal(K_AA, rep(0, 4))
})


# ===========================================================================
# 6. K measures are consistent across bi_env_arr steps (integration test)
# ===========================================================================
test_that("K values match hand-computation from bi_env_arr", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 500),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  ## Both are postconditions of the run above; assert rather than skip, so a
  ## silently empty engine result fails instead of reporting green.
  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_AC_df))

  ## Pick the first step and verify K_AC matches rowSums
  step <- 1
  B <- env$bi_env_arr[, , step]
  expected_K_AC <- rowSums(B)

  k_ac_step <- env$K_AC_df[env$K_AC_df$chain_step_id == step, ]
  if (nrow(k_ac_step) > 0) {
    actual_K_AC <- k_ac_step$value[order(as.numeric(k_ac_step$actor_id))]
    expect_equal(actual_K_AC, unname(expected_K_AC),
                 info = "K_AC should equal rowSums of bipartite matrix at step 1")
  }
})

test_that("K_CA values match hand-computation from bi_env_arr", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 501),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_CA_df))

  step <- 1
  B <- env$bi_env_arr[, , step]
  expected_K_CA <- colSums(B)

  k_ca_step <- env$K_CA_df[env$K_CA_df$chain_step_id == step, ]
  if (nrow(k_ca_step) > 0) {
    actual_K_CA <- k_ca_step$value[order(as.numeric(k_ca_step$component_id))]
    expect_equal(actual_K_CA, unname(expected_K_CA),
                 info = "K_CA should equal colSums of bipartite matrix at step 1")
  }
})

test_that("K_AA values match hand-computation from bi_env_arr", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 502),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_AA_df))

  step <- 1
  B <- env$bi_env_arr[, , step]
  expected_K_AA <- .k4_offdiag_degree(B %*% t(B))

  k_aa_step <- env$K_AA_df[env$K_AA_df$chain_step_id == step, ]
  if (nrow(k_aa_step) > 0) {
    actual_K_AA <- k_aa_step$value[order(as.numeric(k_aa_step$actor_id))]
    expect_equal(actual_K_AA, unname(expected_K_AA),
                 info = "K_AA should count positive off-diagonal entries in B %*% t(B)")
  }
})

test_that("K_CC values match hand-computation from bi_env_arr", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 503),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_CC_df))

  step <- 1
  B <- env$bi_env_arr[, , step]
  expected_K_CC <- .k4_offdiag_degree(t(B) %*% B)

  k_cc_step <- env$K_CC_df[env$K_CC_df$chain_step_id == step, ]
  if (nrow(k_cc_step) > 0) {
    actual_K_CC <- k_cc_step$value[order(as.numeric(k_cc_step$component_id))]
    expect_equal(actual_K_CC, unname(expected_K_CC),
                 info = "K_CC should count positive off-diagonal entries in t(B) %*% B")
  }
})


# ===========================================================================
# 7. K values at multiple steps are internally consistent
# ===========================================================================
test_that("K_AC sum equals total number of ties at each step", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 510),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  expect_false(is.null(env$bi_env_arr))
  expect_false(is.null(env$K_AC_df))

  n_steps <- dim(env$bi_env_arr)[3]
  ## Check a few steps
  check_steps <- unique(c(1, min(3, n_steps), n_steps))

  for (s in check_steps) {
    B <- env$bi_env_arr[, , s]
    total_ties <- sum(B)

    k_ac_step <- env$K_AC_df[env$K_AC_df$chain_step_id == s, ]
    if (nrow(k_ac_step) > 0) {
      expect_equal(sum(k_ac_step$value), total_ties,
                   info = paste("Sum of K_AC should equal total ties at step", s))
    }
  }
})

test_that("Sum of K_AC equals sum of K_CA at each step (conservation)", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 520),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  expect_false(is.null(env$K_AC_df))
  expect_false(is.null(env$K_CA_df))

  ## Both K_AC and K_CA sum to total number of ties at each step
  steps <- unique(env$K_AC_df$chain_step_id)
  for (s in steps[1:min(5, length(steps))]) {
    sum_ac <- sum(env$K_AC_df$value[env$K_AC_df$chain_step_id == s])
    sum_ca <- sum(env$K_CA_df$value[env$K_CA_df$chain_step_id == s])
    expect_equal(sum_ac, sum_ca,
                 info = paste("Sum(K_AC) should equal Sum(K_CA) at step", s))
  }
})


# ===========================================================================
# 8. saomnk_get_degrees() against an independent computation, every step
# ===========================================================================
## One step of a degree frame as a vector ordered by node id.
.k4_step_values <- function(df, step, id) {
  d <- df[df$chain_step_id == step, ]
  d$value[order(as.numeric(as.character(d[[id]])))]
}

test_that("saomnk_get_degrees() equals off-diagonal projection degrees and .rosetta_k_summary()", {
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 6, N = 9, density = 0.25, seed = 11)
  invisible(utils::capture.output(
    saomnk_run(env, saomnk_model(density = -0.4, popularity = -0.2),
               steps_per_actor = 4, seed = 11)))
  deg <- saomnk_get_degrees(env)
  arr <- env$bi_env_arr
  steps <- seq_len(dim(arr)[3])
  expect_gt(length(steps), 5L)
  for (s in steps) {
    B <- arr[, , s]
    S <- B %*% t(B); P <- t(B) %*% B
    ## Off-diagonal entries > 0, counted row by row: independent of the
    ## engine's incremental sign-counting.
    want_AA <- vapply(seq_len(nrow(B)), function(i) sum(S[i, -i] > 0), numeric(1))
    want_CC <- vapply(seq_len(ncol(B)), function(j) sum(P[j, -j] > 0), numeric(1))
    got_AA <- .k4_step_values(deg$K_AA, s, "actor_id")
    got_CC <- .k4_step_values(deg$K_CC, s, "component_id")
    expect_equal(got_AA, want_AA, info = sprintf("K_AA, step %d", s))
    expect_equal(got_CC, want_CC, info = sprintf("K_CC, step %d", s))
    expect_equal(.k4_step_values(deg$K_AC, s, "actor_id"), unname(rowSums(B)))
    expect_equal(.k4_step_values(deg$K_CA, s, "component_id"), unname(colSums(B)))
    ## the package's other definition of the same degrees (Rosetta)
    rk <- .rosetta_k_summary(B)
    expect_equal(mean(got_AA), rk$K_AA, info = sprintf("mean K_AA, step %d", s))
    expect_equal(mean(got_CC), rk$K_CC, info = sprintf("mean K_CC, step %d", s))
    ## a degree counts OTHER nodes: never more than M - 1 / N - 1
    expect_true(all(got_AA <= nrow(B) - 1) && all(got_CC <= ncol(B) - 1))
  }
})

test_that("NEW/OLD degree subsets: K_CA is the column sum, K_CC the projection degree", {
  skip_if_not_installed("RSiena")
  ## A sparse start leaves components unheld; those are the NEW components.
  env <- saomnk_env(M = 5, N = 10, density = 0.12, seed = 4)
  invisible(utils::capture.output(
    saomnk_run(env, saomnk_model(density = 0.3), steps_per_actor = 4, seed = 4)))
  arr <- env$bi_env_arr
  new <- which(colSums(env$bi_env_arr_initial) == 0)
  old <- setdiff(seq_len(ncol(arr)), new)
  expect_gt(length(new), 0L)
  expect_gt(length(old), 0L)
  for (s in unique(c(1L, dim(arr)[3] %/% 2L, dim(arr)[3]))) {
    for (grp in list(list(cols = new, sfx = "NEW"), list(cols = old, sfx = "OLD"))) {
      Bs <- matrix(arr[, grp$cols, s], nrow = dim(arr)[1])
      ca <- .k4_step_values(env[[sprintf("K_CA_%s_df", grp$sfx)]], s, "component_id")
      cc <- .k4_step_values(env[[sprintf("K_CC_%s_df", grp$sfx)]], s, "component_id")
      aa <- .k4_step_values(env[[sprintf("K_AA_%s_df", grp$sfx)]], s, "actor_id")
      ac <- .k4_step_values(env[[sprintf("K_AC_%s_df", grp$sfx)]], s, "actor_id")
      info <- sprintf("%s, step %d", grp$sfx, s)
      expect_equal(ca, unname(colSums(Bs)), info = info)
      expect_equal(cc, unname(.k4_offdiag_degree(t(Bs) %*% Bs)), info = info)
      expect_equal(aa, unname(.k4_offdiag_degree(Bs %*% t(Bs))), info = info)
      expect_equal(ac, unname(rowSums(Bs)), info = info)
    }
  }
})

test_that("K_CC_df carries the NEW/OLD component strategy (K_CA_df's is not overwritten)", {
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 5, N = 10, density = 0.12, seed = 4)
  invisible(utils::capture.output(
    saomnk_run(env, saomnk_model(density = 0.3), steps_per_actor = 2, seed = 4)))
  new <- which(colSums(env$bi_env_arr_initial) == 0)
  for (nm in c("K_CA_df", "K_CC_df")) {
    d <- env[[nm]]
    expect_false(is.null(d$strategy), info = nm)
    want <- ifelse(as.numeric(as.character(d$component_id)) %in% new, "NEW", "OLD")
    expect_equal(as.character(d$strategy), want, info = nm)
  }
})
