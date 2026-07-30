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

test_that("epistasis matrix has K+1 dependencies per locus", {
  for (k in c(0, 2, 5)) {
    nk <- nk_landscape(N = 9, K = k, model = "adjacent", seed = 3)
    expect_true(all(rowSums(nk$epistasis_matrix) == k + 1),
                info = paste("K =", k))
    expect_true(all(diag(nk$epistasis_matrix) == 1))
  }
})

test_that("all neighbourhood models produce valid landscapes", {
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

test_that("nk_verify_reduction passes", {
  res <- nk_verify_reduction(N = 9, K = 3, seed = 42)
  expect_true(res$passed)
  expect_lt(res$max_difference, 1e-12)
})

test_that("nk_to_saomnk emits a usable specification", {
  nk <- nk_landscape(N = 8, K = 2, seed = 42)
  spec <- nk_to_saomnk(nk)
  expect_equal(spec$env_params$M, 1)
  expect_equal(spec$env_params$N, 8)
  expect_equal(dim(spec$epistasis_matrix), c(8, 8))
  expect_true(all(rowSums(spec$epistasis_matrix) == 3))
})

test_that("invalid parameters are rejected", {
  expect_error(nk_landscape(N = 5, K = 5))       # K > N-1
  expect_error(nk_landscape(N = 5, K = -1))      # K < 0
})
