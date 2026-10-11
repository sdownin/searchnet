test_that("configuration encoding round-trips for all N", {
  for (N in 1:12) {
    codes <- 0:(2^N - 1)
    expect_identical(as.integer(nk_code(nk_bits(codes, N))),
                     as.integer(codes),
                     info = paste("N =", N))
  }
})

test_that("nk_bits uses MSB-first convention", {
  # Guards the endianness bug class: intToBits() is LSB-first, so a naive
  # decode silently returns the high (zero) bits of the 32-bit word.
  expect_equal(as.vector(nk_bits(44L, 6)), c(1, 0, 1, 1, 0, 0))
  expect_equal(nk_code(c(1, 0, 1, 1, 0, 0)), 44L)
  expect_equal(as.vector(nk_bits(1L, 4)), c(0, 0, 0, 1))
})

test_that("landscape config matrix agrees with nk_bits", {
  nk <- nk_landscape(N = 8, K = 2, seed = 1)
  expect_identical(nk$configs, nk_bits(0:255, 8))
})

test_that("K = 0 landscapes are single-peaked", {
  for (s in 1:5) {
    nk <- nk_landscape(N = 9, K = 0, seed = s)
    expect_equal(nrow(nk_local_optima(nk)), 1L,
                 info = paste("seed", s))
  }
})

test_that("local optima increase with K", {
  counts <- vapply(c(0, 2, 4, 8), function(k) {
    nrow(nk_local_optima(nk_landscape(N = 10, K = k, seed = 42)))
  }, numeric(1))
  expect_true(all(diff(counts) > 0))
})

test_that("influence matrix has K+1 dependencies per locus", {
  for (k in c(0, 2, 5)) {
    nk <- nk_landscape(N = 9, K = k, model = "adjacent", seed = 3)
    expect_true(all(rowSums(nk$influence_matrix) == k + 1),
                info = paste("K =", k))
    expect_true(all(diag(nk$influence_matrix) == 1))
  }
})

test_that("all neighborhood models produce valid landscapes", {
  for (m in c("adjacent", "random", "block")) {
    nk <- nk_landscape(N = 9, K = 2, model = m, seed = 3)
    expect_s3_class(nk, "nk_landscape")
    expect_length(nk$fitness, 2^9)
    expect_true(all(nk$fitness >= 0 & nk$fitness <= 1))
    expect_gte(nrow(nk_local_optima(nk)), 1L)
  }
})

test_that("adaptive walks terminate at local optima", {
  nk <- nk_landscape(N = 10, K = 3, seed = 7)
  optima <- nk_local_optima(nk)$code
  for (type in c("steepest", "greedy", "random")) {
    w <- nk_walk(nk, start = 0, type = type)
    expect_true(w$terminal %in% optima, info = type)
    # fitness is non-decreasing along the path
    expect_true(all(diff(w$fitness) > 0) || w$steps == 0)
  }
})

test_that("walks are reproducible given a seed", {
  nk <- nk_landscape(N = 9, K = 2, seed = 11)
  set.seed(99); w1 <- nk_walk(nk, type = "steepest")
  set.seed(99); w2 <- nk_walk(nk, type = "steepest")
  expect_identical(w1$path, w2$path)
})

test_that("landscape generation is reproducible given a seed", {
  a <- nk_landscape(N = 8, K = 3, seed = 123)
  b <- nk_landscape(N = 8, K = 3, seed = 123)
  expect_identical(a$fitness, b$fitness)
  expect_identical(a$dependencies, b$dependencies)
})

test_that("nk_fitness accepts codes and bit vectors", {
  nk <- nk_landscape(N = 6, K = 2, seed = 1)
  expect_equal(nk_fitness(nk, c(1, 0, 1, 1, 0, 0)),
               nk_fitness(nk, 44L))
  expect_length(nk_fitness(nk), 2^6)
})

test_that("nk_verify_reduction encoding check passes for every model", {
  for (m in c("adjacent", "random", "block")) {
    res <- nk_verify_reduction(N = 9, K = 3, model = m, seed = 42,
                               verbose = FALSE)
    expect_true(res$passed, info = m)
    expect_identical(res$model, m)
    expect_lt(res$encoding_max_difference, 1e-12)
    expect_identical(res$config_mismatches, 0L)
  }
})

## The encoding check must be able to fail. Rebuild a landscape the way
## nk_landscape() does, but with a deliberately wrong bit order, and require
## the check to catch it. The earlier check compared the landscape's
## contributions with themselves and could not.
corrupt_nk <- function(nk, configs, weights_rev = FALSE) {
  bad <- nk
  bad$configs <- configs
  for (i in seq_len(nk$N)) {
    di <- nk$dependencies[[i]]
    bw <- 2^(seq_along(di) - 1)
    if (weights_rev) bw <- rev(bw)
    pat <- as.integer(configs[, di, drop = FALSE] %*% bw)
    bad$contributions[, i] <- nk$tables[[i]][pat + 1L]
  }
  bad$fitness <- rowMeans(bad$contributions)
  bad
}

test_that("nk_verify_reduction detects a reversed (LSB-first) bit order", {
  nk <- nk_landscape(N = 8, K = 3, model = "random", seed = 5)
  bad <- corrupt_nk(nk, nk$configs[, 8:1])
  res <- nk_verify_reduction(landscape = bad, verbose = FALSE)
  expect_false(res$passed)
  expect_gt(res$encoding_max_difference, 1e-3)
  expect_gt(res$config_mismatches, 0L)
})

test_that("nk_verify_reduction detects a misordered table key", {
  nk <- nk_landscape(N = 8, K = 3, model = "random", seed = 5)
  bad <- corrupt_nk(nk, nk$configs, weights_rev = TRUE)
  res <- nk_verify_reduction(landscape = bad, verbose = FALSE)
  expect_false(res$passed)
  expect_identical(res$config_mismatches, 0L)   # configs are right ...
  expect_gt(res$encoding_max_difference, 1e-3) # ... the fitness is not
})

test_that("nk_verify_reduction engine check: utility and walk match NK", {
  res <- nk_verify_reduction(N = 8, K = 3, model = "random", seed = 42,
                             engine = TRUE, verbose = FALSE)
  expect_true(res$passed)
  expect_lt(res$engine_max_difference, 1e-12)
  expect_true(res$engine_walk_identical)
  expect_gte(res$engine_walk_steps, 1L)
})

test_that("nk_verify_reduction engine check fails on a corrupted landscape", {
  nk <- nk_landscape(N = 6, K = 2, model = "random", seed = 9)
  bad <- corrupt_nk(nk, nk$configs[, 6:1])
  res <- nk_verify_reduction(landscape = bad, engine = TRUE, verbose = FALSE)
  expect_false(res$passed)
})

test_that("nk_walk reports is_local_optimum from the terminal state", {
  nk <- nk_landscape(N = 10, K = 3, seed = 7)
  full <- nk_walk(nk, start = 0, type = "steepest")
  expect_true(full$is_local_optimum)
  expect_gte(full$steps, 2L)
  cut <- nk_walk(nk, start = 0, type = "steepest", max_steps = 1L)
  expect_identical(cut$steps, 1L)
  expect_false(cut$is_local_optimum)
  expect_identical(cut$terminal, cut$path[length(cut$path)])
  expect_identical(cut$path, full$path[1:2])
})

test_that("nk_sweep_K gives the 2^N/(N+1) reference only at K = N - 1", {
  sw <- nk_sweep_K(N = 6, K_values = c(0, 2, 5), n_landscapes = 2,
                   n_walks = 2, seed = 1)
  expect_true(all(is.na(sw$expected_optima[sw$K < 5])))
  expect_equal(sw$expected_optima[sw$K == 5], 2^6 / 7)
})

test_that("nk_to_saomnk emits a usable specification", {
  nk <- nk_landscape(N = 8, K = 2, seed = 42)
  spec <- nk_to_saomnk(nk)
  expect_equal(spec$env_params$M, 1)
  expect_equal(spec$env_params$N, 8)
  expect_equal(dim(spec$influence_matrix), c(8, 8))
  expect_true(all(rowSums(spec$influence_matrix) == 3))
})

test_that("invalid parameters are rejected", {
  expect_error(nk_landscape(N = 5, K = 5))       # K > N-1
  expect_error(nk_landscape(N = 5, K = -1))      # K < 0
})
