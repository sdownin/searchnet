## test-projections.R
## Tests for bipartite projection methods

# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------
make_env_with_matrix <- function(mat) {
  M <- nrow(mat)
  N <- ncol(mat)
  cfg <- list(M = M, N = N, BI_PROB = NULL,
              name = "proj_test", rand_seed = 1,
              init_matrix = mat)
  SaomNkRSienaBiEnv_base$new(cfg)
}

# ===========================================================================
# 1. project_social_space (actor-actor) dimensions
# ===========================================================================
test_that("social projection has M x M dimensions", {
  mat <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1), nrow = 3, ncol = 3)
  env <- make_env_with_matrix(mat)
  expect_equal(dim(env$social_matrix), c(3L, 3L))
})

test_that("search projection has N x N dimensions", {
  mat <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1), nrow = 3, ncol = 3)
  env <- make_env_with_matrix(mat)
  expect_equal(dim(env$search_matrix), c(3L, 3L))
})

# ===========================================================================
# 2. Hand-calculated projection: 2 actors x 3 components
# ===========================================================================
#
#   Bipartite B (2 x 3):
#     Actor 1: [1, 1, 0]
#     Actor 2: [0, 1, 1]
#
#   Social projection (B %*% t(B) -- counts shared components):
#     S[1,1] = 2, S[1,2] = 1, S[2,1] = 1, S[2,2] = 2
#     But igraph multiplicity projection zeroes the diagonal (self-loops removed).
#     Off-diagonal: S[1,2] = S[2,1] = 1
#
#   Search projection (t(B) %*% B -- counts shared actors):
#     C[1,1]=1, C[1,2]=1, C[1,3]=0
#     C[2,1]=1, C[2,2]=2, C[2,3]=1
#     C[3,1]=0, C[3,2]=1, C[3,3]=1
#     Off-diagonal weighted adjacency (diagonal zeroed by igraph):
#     C[1,2]=1, C[2,3]=1, C[1,3]=0
#
test_that("social projection matches hand calculation", {
  mat <- matrix(c(1, 0, 1, 1, 0, 1), nrow = 2, ncol = 3)
  # row 1 = [1,1,0], row 2 = [0,1,1]
  env <- make_env_with_matrix(mat)

  # Off-diagonal should be 1 (one shared component: #2)
  expect_equal(env$social_matrix[1, 2], 1)
  expect_equal(env$social_matrix[2, 1], 1)
  # Diagonal is zeroed by igraph
  expect_equal(env$social_matrix[1, 1], 0)
  expect_equal(env$social_matrix[2, 2], 0)
})

test_that("search projection matches hand calculation", {
  mat <- matrix(c(1, 0, 1, 1, 0, 1), nrow = 2, ncol = 3)
  env <- make_env_with_matrix(mat)

  # Components 1 and 2 share actor 1 => weight 1

  expect_equal(env$search_matrix[1, 2], 1)
  expect_equal(env$search_matrix[2, 1], 1)
  # Components 2 and 3 share actor 2 => weight 1
  expect_equal(env$search_matrix[2, 3], 1)
  expect_equal(env$search_matrix[3, 2], 1)
  # Components 1 and 3 share no actors => weight 0
  expect_equal(env$search_matrix[1, 3], 0)
  expect_equal(env$search_matrix[3, 1], 0)
})

# ===========================================================================
# 3. Projections from identity-like bipartite
# ===========================================================================
test_that("identity bipartite yields empty social projection (no shared components)", {
  # Each actor has exactly one unique component
  mat <- diag(3)
  env <- make_env_with_matrix(mat)
  # No pair of actors shares a component => social proj is all zeros
  expect_true(all(env$social_matrix == 0))
  expect_true(all(env$search_matrix == 0))
})

# ===========================================================================
# 4. Full bipartite (all ones) yields complete projections
# ===========================================================================
test_that("all-ones bipartite yields fully connected social projection", {
  mat <- matrix(1, nrow = 3, ncol = 4)
  env <- make_env_with_matrix(mat)
  # All off-diagonal social ties should be positive
  diag_mask <- diag(3) == 1
  expect_true(all(env$social_matrix[!diag_mask] > 0))
})

test_that("all-ones bipartite yields fully connected search projection", {
  mat <- matrix(1, nrow = 3, ncol = 4)
  env <- make_env_with_matrix(mat)
  diag_mask <- diag(4) == 1
  expect_true(all(env$search_matrix[!diag_mask] > 0))
})

# ===========================================================================
# 5. get_bipartite_projections returns both projections
# ===========================================================================
test_that("get_bipartite_projections returns list with proj1 and proj2", {
  mat <- matrix(c(1, 0, 0, 1, 1, 0), nrow = 2, ncol = 3)
  env <- make_env_with_matrix(mat)
  ig <- env$bipartite_igraph
  projs <- env$get_bipartite_projections(ig)
  expect_true(is.list(projs))
  expect_true("proj1" %in% names(projs))
  expect_true("proj2" %in% names(projs))
  expect_true(igraph::is_igraph(projs$proj1))
  expect_true(igraph::is_igraph(projs$proj2))
})

test_that("proj1 vertex count = M, proj2 vertex count = N", {
  mat <- matrix(c(1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1), nrow = 3, ncol = 4)
  env <- make_env_with_matrix(mat)
  projs <- env$get_bipartite_projections(env$bipartite_igraph)
  expect_equal(igraph::vcount(projs$proj1), 3)
  expect_equal(igraph::vcount(projs$proj2), 4)
})

# ===========================================================================
# 6. Projection igraphs stored on object
# ===========================================================================
test_that("social_igraph and search_igraph are valid igraphs", {
  env <- make_env_with_matrix(matrix(c(1, 0, 1, 1), nrow = 2, ncol = 2))
  expect_true(igraph::is_igraph(env$social_igraph))
  expect_true(igraph::is_igraph(env$search_igraph))
})

# ===========================================================================
# 7. Projection weights are non-negative integers (multiplicity counts)
# ===========================================================================
test_that("projection weights are non-negative", {
  mat <- matrix(c(1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0), nrow = 3, ncol = 4)
  env <- make_env_with_matrix(mat)
  expect_true(all(env$social_matrix >= 0))
  expect_true(all(env$search_matrix >= 0))
})
