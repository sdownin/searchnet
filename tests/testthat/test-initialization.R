## test-initialization.R
## Tests for SaomNkRSienaBiEnv_base and SaomNkRSienaBiEnv construction

# ---------------------------------------------------------------------------
# Helper: minimal config list
# ---------------------------------------------------------------------------
make_config <- function(M = 3, N = 5, BI_PROB = 0.5,
                        name = "test", rand_seed = 123, ...) {
  c(list(M = M, N = N, BI_PROB = BI_PROB,
         name = name, rand_seed = rand_seed), list(...))
}

# ===========================================================================
# 1. Base class construction
# ===========================================================================
test_that("base class stores M, N, BI_PROB correctly", {
  cfg <- make_config()
  env <- SaomNkRSienaBiEnv_base$new(cfg)

  expect_equal(env$M, 3)
  expect_equal(env$N, 5)
  expect_equal(env$BI_PROB, 0.5)
})

test_that("base class stores SIM_NAME from config", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(name = "my_sim"))
  expect_equal(env$SIM_NAME, "my_sim")
})

test_that("UUID is generated on construction", {
  env <- SaomNkRSienaBiEnv_base$new(make_config())
  expect_true(is.character(env$UUID))
  expect_true(nchar(env$UUID) > 0)
})

test_that("TIMESTAMP is set on construction", {
  env <- SaomNkRSienaBiEnv_base$new(make_config())
  expect_true(is.numeric(env$TIMESTAMP))
})

test_that("ITERATION starts at zero", {
  env <- SaomNkRSienaBiEnv_base$new(make_config())
  expect_equal(env$ITERATION, 0)
})

test_that("rand_seed is stored", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(rand_seed = 42))
  expect_equal(env$rsiena_env_seed, 42)
})

# ===========================================================================
# 2. random_bipartite_matrix
# ===========================================================================
test_that("random_bipartite_matrix produces correct dimensions", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 4, N = 7))
  mat <- env$random_bipartite_matrix(999)
  expect_equal(dim(mat), c(4L, 7L))
})

test_that("random_bipartite_matrix contains only 0s and 1s", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 6, N = 10))
  mat <- env$random_bipartite_matrix(111)
  expect_true(all(mat %in% c(0L, 1L)))
})

test_that("random_bipartite_matrix is reproducible with same seed", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 4, N = 6))
  mat1 <- env$random_bipartite_matrix(77)
  mat2 <- env$random_bipartite_matrix(77)
  expect_identical(mat1, mat2)
})

test_that("different seeds produce different matrices (high probability)", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 5, N = 8))
  mat1 <- env$random_bipartite_matrix(1)
  mat2 <- env$random_bipartite_matrix(999)
  expect_false(identical(mat1, mat2))
})

# ===========================================================================
# 3. Edge cases: BI_PROB = 0, 1
# ===========================================================================
test_that("BI_PROB = 0 produces all-zero matrix", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 3, N = 4, BI_PROB = 0))
  expect_true(all(env$bipartite_matrix == 0))
})

test_that("BI_PROB = 1 produces all-one matrix", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 3, N = 4, BI_PROB = 1))
  expect_true(all(env$bipartite_matrix == 1))
})

# ===========================================================================
# 4. Minimal dimensions (M = 1, N = 1)
# ===========================================================================
test_that("M=1, N=1 construction succeeds", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 1, N = 1, BI_PROB = 1))
  expect_equal(dim(env$bipartite_matrix), c(1L, 1L))
})

# ===========================================================================
# 5. set_system_from_bipartite_matrix
# ===========================================================================
test_that("set_system_from_bipartite_matrix creates valid igraph", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 4, N = 6))
  expect_true(igraph::is_igraph(env$bipartite_igraph))
  expect_true(igraph::is_bipartite(env$bipartite_igraph))
})

test_that("bipartite_matrix dimensions match M x N", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 3, N = 5))
  expect_equal(nrow(env$bipartite_matrix), 3)
  expect_equal(ncol(env$bipartite_matrix), 5)
})

test_that("social_matrix is M x M", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 4, N = 7))
  expect_equal(dim(env$social_matrix), c(4L, 4L))
})

test_that("search_matrix is N x N", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 4, N = 7))
  expect_equal(dim(env$search_matrix), c(7L, 7L))
})

test_that("bipartite_matrix_init is cached", {
  env <- SaomNkRSienaBiEnv_base$new(make_config(M = 3, N = 5))
  expect_false(is.null(env$bipartite_matrix_init))
  expect_equal(unname(env$bipartite_matrix_init), unname(env$bipartite_matrix))
})

# ===========================================================================
# 6. Custom init_matrix
# ===========================================================================
test_that("custom init_matrix overrides random generation", {
  custom <- matrix(c(1, 0, 0, 1, 1, 0), nrow = 2, ncol = 3)
  cfg <- make_config(M = 2, N = 3, BI_PROB = NULL, init_matrix = custom)
  env <- SaomNkRSienaBiEnv_base$new(cfg)
  expect_equal(unname(env$bipartite_matrix[1:2, 1:3]), custom)
})

test_that("custom init_matrix updates BI_PROB to empirical density", {
  custom <- matrix(c(1, 0, 0, 1, 1, 0), nrow = 2, ncol = 3)
  cfg <- make_config(M = 2, N = 3, BI_PROB = NULL, init_matrix = custom)
  env <- SaomNkRSienaBiEnv_base$new(cfg)
  expect_equal(env$BI_PROB, sum(custom) / (2 * 3))
})

# ===========================================================================
# 7. Extended class (SaomNkRSienaBiEnv)
# ===========================================================================
test_that("extended class inherits from base", {
  env <- SaomNkRSienaBiEnv$new(make_config())
  expect_true(inherits(env, "SaomNkRSienaBiEnv_base"))
  expect_true(inherits(env, "SaomNkRSienaBiEnv"))
})

test_that("extended class stores same fields as base", {
  env <- SaomNkRSienaBiEnv$new(make_config(M = 4, N = 6))
  expect_equal(env$M, 4)
  expect_equal(env$N, 6)
  expect_true(igraph::is_igraph(env$bipartite_igraph))
})
