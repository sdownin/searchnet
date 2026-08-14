###############################################################################
## test-reproducibility.R
## Reproducibility guarantees for SaoMNK simulations
##
## Philosophy: "Users don't have to trust us -- they just trust the tests."
## Reproducibility is foundational for scientific computing. These tests
## verify that identical seeds produce identical results, and different seeds
## produce different results, giving reviewers confidence in the simulation
## engine's random number control.
###############################################################################


# ===========================================================================
# 1. Same seed + same params = identical bipartite_matrix_init
# ===========================================================================
test_that("same seed produces identical initial bipartite matrix", {
  env1 <- SaomNkRSienaBiEnv_base$new(list(
    M = 5, N = 8, BI_PROB = 0.4, name = "repro1", rand_seed = 777
  ))
  env2 <- SaomNkRSienaBiEnv_base$new(list(
    M = 5, N = 8, BI_PROB = 0.4, name = "repro2", rand_seed = 777
  ))

  expect_identical(env1$bipartite_matrix_init, env2$bipartite_matrix_init)
})

test_that("same seed + same M/N/density via saomnk_env gives identical init", {
  tryCatch(
    source(file.path(dir_r, "saomnk-api.R"), local = FALSE),
    error = function(e) stop("Could not source saomnk-api.R")
  )

  env1 <- saomnk_env(M = 6, N = 10, density = 0.3, seed = 42)
  env2 <- saomnk_env(M = 6, N = 10, density = 0.3, seed = 42)

  expect_identical(
    unname(env1$bipartite_matrix_init),
    unname(env2$bipartite_matrix_init)
  )
})


# ===========================================================================
# 2. Same seed + same params + same run_seed = identical simulation output
# ===========================================================================
test_that("same run_seed produces identical final bipartite matrix", {
  skip_if_not_installed("RSiena")

  env1 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 600),
    error = function(e) stop(paste("Sim 1 failed:", e$message))
  )
  env2 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 600),
    error = function(e) stop(paste("Sim 2 failed:", e$message))
  )

  expect_identical(env1$bipartite_matrix, env2$bipartite_matrix)
})

test_that("same run_seed produces identical chain_stats length", {
  skip_if_not_installed("RSiena")

  env1 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 601),
    error = function(e) stop(paste("Sim 1 failed:", e$message))
  )
  env2 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 601),
    error = function(e) stop(paste("Sim 2 failed:", e$message))
  )

  if (is.data.frame(env1$chain_stats) && is.data.frame(env2$chain_stats)) {
    expect_equal(nrow(env1$chain_stats), nrow(env2$chain_stats))
  }
})

test_that("same seed produces identical bi_env_arr trajectories", {
  skip_if_not_installed("RSiena")

  env1 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 602),
    error = function(e) stop(paste("Sim 1 failed:", e$message))
  )
  env2 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 602),
    error = function(e) stop(paste("Sim 2 failed:", e$message))
  )

  if (!is.null(env1$bi_env_arr) && !is.null(env2$bi_env_arr)) {
    expect_identical(env1$bi_env_arr, env2$bi_env_arr)
  }
})

test_that("same seed produces identical K_AC_df", {
  skip_if_not_installed("RSiena")

  env1 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 603),
    error = function(e) stop(paste("Sim 1 failed:", e$message))
  )
  env2 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 603),
    error = function(e) stop(paste("Sim 2 failed:", e$message))
  )

  if (!is.null(env1$K_AC_df) && !is.null(env2$K_AC_df)) {
    expect_equal(env1$K_AC_df$value, env2$K_AC_df$value)
  }
})


# ===========================================================================
# 3. Different seeds produce different results
# ===========================================================================
test_that("different env seeds produce different initial matrices", {
  env1 <- SaomNkRSienaBiEnv_base$new(list(
    M = 5, N = 8, BI_PROB = 0.4, name = "diff1", rand_seed = 111
  ))
  env2 <- SaomNkRSienaBiEnv_base$new(list(
    M = 5, N = 8, BI_PROB = 0.4, name = "diff2", rand_seed = 222
  ))

  expect_false(
    identical(env1$bipartite_matrix_init, env2$bipartite_matrix_init),
    info = "Different seeds should produce different initial matrices (high probability)"
  )
})

test_that("different run seeds produce different simulation outcomes", {
  skip_if_not_installed("RSiena")

  env1 <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 10, rand_seed = 700),
    error = function(e) stop(paste("Sim 1 failed:", e$message))
  )

  ## Create env2 with same init but different run seed
  params2 <- make_small_environ_params(M = 4, N = 8, rand_seed = 700)
  env2 <- tryCatch(
    SaomNkRSienaBiEnv$new(params2),
    error = function(e) stop(paste("Init 2 failed:", e$message))
  )
  struct <- make_minimal_structure_model()
  tryCatch(
    env2$search_rsiena(
      structure_model = struct,
      iterations_per_actor = 10,
      run_seed = 999,  # different run seed
      verbose = FALSE
    ),
    error = function(e) stop(paste("Sim 2 failed:", e$message))
  )

  ## Initial matrices should be identical (same env seed)
  expect_identical(env1$bipartite_matrix_init, env2$bipartite_matrix_init)

  ## Final matrices should differ (different run seeds, high probability)
  expect_false(
    identical(env1$bipartite_matrix, env2$bipartite_matrix),
    info = "Different run seeds should produce different final matrices"
  )
})


# ===========================================================================
# 4. Deterministic across repeated runs
# ===========================================================================
test_that("three consecutive runs with same seed all produce identical output", {
  skip_if_not_installed("RSiena")

  results <- list()
  for (i in 1:3) {
    results[[i]] <- tryCatch(
      run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 888),
      error = function(e) stop(paste("Sim", i, "failed:", e$message))
    )
  }

  ## All three should have identical final matrices
  expect_identical(results[[1]]$bipartite_matrix, results[[2]]$bipartite_matrix)
  expect_identical(results[[2]]$bipartite_matrix, results[[3]]$bipartite_matrix)
})


# ===========================================================================
# 5. Reproducibility of random_bipartite_matrix (unit level)
# ===========================================================================
test_that("random_bipartite_matrix is reproducible across multiple calls", {
  env <- SaomNkRSienaBiEnv_base$new(list(
    M = 6, N = 10, BI_PROB = 0.5, name = "rb_test", rand_seed = 1
  ))

  ## Call the method 5 times with the same seed — should be identical
  mats <- lapply(1:5, function(x) { set.seed(42); env$random_bipartite_matrix() })

  for (i in 2:5) {
    expect_identical(mats[[1]], mats[[i]],
                     info = paste("Call", i, "should match call 1"))
  }
})

test_that("random_bipartite_matrix with different BI_PROB produces different matrices", {
  env1 <- SaomNkRSienaBiEnv_base$new(list(
    M = 6, N = 10, BI_PROB = 0.1, name = "rb_sparse", rand_seed = 10
  ))
  env2 <- SaomNkRSienaBiEnv_base$new(list(
    M = 6, N = 10, BI_PROB = 0.9, name = "rb_dense", rand_seed = 20
  ))

  mat1 <- env1$random_bipartite_matrix()
  mat2 <- env2$random_bipartite_matrix()

  # Different density params should yield different matrices
  expect_false(identical(mat1, mat2))
})
