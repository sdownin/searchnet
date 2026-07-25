###############################################################################
## test-power-key.R
## Unit tests for SaomNkRSienaBiEnv$power_key_index()
###############################################################################

## ---- Helper: create a minimal env with controllable N and bipartite matrix --
make_pki_env <- function(M = 3, N = 4, seed = 99) {
  params <- make_small_environ_params(M = M, N = N, rand_seed = seed)
  env <- SaomNkRSienaBiEnv$new(params)
  env
}

## ==========================================================================
## Test 1: Identity epistasis E=I, b_i = (1,0,...,0), d = 1
##   PK = 2^(N-1), 2^(N-2), ..., 1
##   masked = b_i * I[1,] = (1,0,...,0)
##   index = 1 * 2^(N-1) + 1 = 2^(N-1) + 1
## ==========================================================================
test_that("identity E: b_i=(1,0,0,0), d=1 gives 2^(N-1)+1", {
  skip_if_not_installed("RSiena")

  N <- 4
  env <- make_pki_env(M = 3, N = N, seed = 101)

  E_identity <- diag(N)
  b_i <- c(1, 0, 0, 0)
  d <- 1

  idx <- env$power_key_index(b_i, d, E = E_identity)

  expect_equal(idx, 2^(N - 1) + 1,
               info = "Only the d-th bit survives under identity E")
})

## ==========================================================================
## Test 2: All-zero config always gives index = 1 regardless of E
##   masked = (0,...,0) * anything = (0,...,0)  =>  sum = 0  =>  index = 1
## ==========================================================================
test_that("all-zero config gives index 1 regardless of E", {
  skip_if_not_installed("RSiena")

  N <- 5
  env <- make_pki_env(M = 3, N = N, seed = 102)

  b_zero <- rep(0, N)

  # Identity E
  expect_equal(env$power_key_index(b_zero, 1, E = diag(N)), 1L)
  expect_equal(env$power_key_index(b_zero, 3, E = diag(N)), 1L)

  # Full-ones E
  E_full <- matrix(1, N, N)
  expect_equal(env$power_key_index(b_zero, 1, E = E_full), 1L)
  expect_equal(env$power_key_index(b_zero, N, E = E_full), 1L)

  # Random E
  set.seed(42)
  E_rand <- matrix(sample(0:1, N * N, replace = TRUE), N, N)
  for (d in 1:N) {
    expect_equal(env$power_key_index(b_zero, d, E = E_rand), 1L,
                 info = paste("all-zero config, d =", d))
  }
})

## ==========================================================================
## Test 3: All-one config with identity E for each d
##   Under E = I, only bit d survives: masked = (0,..,0,1,0,..,0) at pos d
##   Power key for position d = 2^(N - d)
##   index = 2^(N - d) + 1
## ==========================================================================
test_that("all-one config with identity E gives 2^(N-d)+1 for each d", {
  skip_if_not_installed("RSiena")

  N <- 6
  env <- make_pki_env(M = 3, N = N, seed = 103)

  E_identity <- diag(N)
  b_all <- rep(1, N)

  for (d in 1:N) {
    idx <- env$power_key_index(b_all, d, E = E_identity)
    expected <- 2^(N - d) + 1L
    expect_equal(idx, expected,
                 info = sprintf("d=%d: expected index %d", d, expected))
  }
})

## ==========================================================================
## Test 4: Full epistasis E = matrix(1), all bits contribute
##   N=3, b_i = (1,1,1), d = any
##   masked = (1,1,1) * (1,1,1) = (1,1,1)
##   PK = c(4,2,1)
##   index = 4+2+1+1 = 8
## ==========================================================================
test_that("full epistasis E=1 matrix: b_i=(1,1,1) N=3 gives index=8", {
  skip_if_not_installed("RSiena")

  N <- 3
  env <- make_pki_env(M = 2, N = N, seed = 104)

  E_full <- matrix(1, N, N)
  b_i <- c(1, 1, 1)

  for (d in 1:N) {
    idx <- env$power_key_index(b_i, d, E = E_full)
    # sum = 1*4 + 1*2 + 1*1 = 7; index = 7 + 1 = 8
    expect_equal(idx, 8L,
                 info = sprintf("d=%d, full epistasis all-ones", d))
  }
})

## ==========================================================================
## Test 5: Block-diagonal E isolates correct bits
##   N=4, E = block-diagonal with two 2x2 blocks of ones
##   Block 1: dims 1-2 see bits 1-2;  Block 2: dims 3-4 see bits 3-4
##   b_i = (1,0,1,0)
##   For d=1: masked = (1,0,0,0) -> PK=(8,4,2,1) -> 8+1 = 9
##   For d=3: masked = (0,0,1,0) -> sum = 2 -> index = 3
## ==========================================================================
test_that("block-diagonal E masks bits to the correct block", {
  skip_if_not_installed("RSiena")

  N <- 4
  env <- make_pki_env(M = 2, N = N, seed = 105)

  E_block <- matrix(0, N, N)
  E_block[1:2, 1:2] <- 1
  E_block[3:4, 3:4] <- 1

  b_i <- c(1, 0, 1, 0)
  PK <- 2^((N - 1):0)  # c(8, 4, 2, 1)

  # d=1 (block 1): E[1,] = (1,1,0,0), masked = (1,0,0,0), sum = 8, idx = 9
  idx1 <- env$power_key_index(b_i, 1, E = E_block)
  expect_equal(idx1, as.integer(sum(c(1, 0, 0, 0) * PK) + 1),
               info = "d=1, block-diag: only block-1 bits visible")

  # d=2 (block 1): E[2,] = (1,1,0,0), masked = (1,0,0,0), sum = 8, idx = 9
  idx2 <- env$power_key_index(b_i, 2, E = E_block)
  expect_equal(idx2, as.integer(sum(c(1, 0, 0, 0) * PK) + 1),
               info = "d=2, block-diag: only block-1 bits visible")

  # d=3 (block 2): E[3,] = (0,0,1,1), masked = (0,0,1,0), sum = 2, idx = 3
  idx3 <- env$power_key_index(b_i, 3, E = E_block)
  expect_equal(idx3, as.integer(sum(c(0, 0, 1, 0) * PK) + 1),
               info = "d=3, block-diag: only block-2 bits visible")

  # d=4 (block 2): E[4,] = (0,0,1,1), masked = (0,0,1,0), sum = 2, idx = 3
  idx4 <- env$power_key_index(b_i, 4, E = E_block)
  expect_equal(idx4, as.integer(sum(c(0, 0, 1, 0) * PK) + 1),
               info = "d=4, block-diag: only block-2 bits visible")

  # Cross-block isolation: d=1 should NOT see bit 3
  b_alt <- c(0, 0, 1, 1)
  idx_cross <- env$power_key_index(b_alt, 1, E = E_block)
  expect_equal(idx_cross, 1L,
               info = "d=1 sees no bits from block 2")
})
