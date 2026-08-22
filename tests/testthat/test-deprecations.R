###############################################################################
## test-deprecations.R
## The 0.8.2 terminology renames keep every old name working, with a warning.
## Pattern follows test-api.R ("deprecated argument names still work").
###############################################################################

test_that("saomnk_empirical_epistasis() is a warning alias of saomnk_empirical_influence()", {
  set.seed(11)
  B <- matrix(rbinom(60, 1, 0.4), nrow = 10, ncol = 6)

  ## The alias warns once per session; reset so this test is order-independent.
  .searchnet_reset_deprecations()
  expect_warning(old <- saomnk_empirical_epistasis(B), "saomnk_empirical_influence")
  new <- saomnk_empirical_influence(B)
  expect_identical(old, new)

  ## Second call in the same session is silent (one-time warning, by design).
  expect_silent(old2 <- saomnk_empirical_epistasis(B))
  expect_identical(old2, new)

  ## Same for every method and the threshold/diagonal arguments.
  .searchnet_reset_deprecations()
  for (m in c("jaccard", "cosine", "cooccurrence")) {
    a <- suppressWarnings(saomnk_empirical_epistasis(B, method = m, threshold = 0.1, diagonal = 1))
    b <- saomnk_empirical_influence(B, method = m, threshold = 0.1, diagonal = 1)
    expect_identical(a, b, info = m)
  }
})

test_that("the returned object is an influence-matrix estimate: N x N, symmetric, diagonal as set", {
  set.seed(12)
  B <- matrix(rbinom(80, 1, 0.5), nrow = 10, ncol = 8)
  W <- saomnk_empirical_influence(B, diagonal = 0)
  expect_equal(dim(W), c(8, 8))
  expect_true(isSymmetric(unname(W)))
  expect_true(all(diag(W) == 0))
})

test_that("epistatic_int_mat still works as a deprecated alias of influence_matrix", {
  skip_if_not_installed("igraph")
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = 4, N = 6, rand_seed = 13))
  W <- saomnk_block_diagonal(6, 2)

  expect_warning(g_old <- env$get_component_groups_list(epistatic_int_mat = W),
                 "influence_matrix")
  g_new <- env$get_component_groups_list(influence_matrix = W)
  expect_identical(g_old, g_new)

  ## new name takes precedence when both are supplied
  W2 <- saomnk_block_diagonal(6, 3)
  both <- suppressWarnings(env$get_component_groups_list(influence_matrix = W2,
                                                         epistatic_int_mat = W))
  expect_identical(both, env$get_component_groups_list(influence_matrix = W2))

  ## and the exported plot functions expose the new name in their formals
  expect_true("influence_matrix" %in% names(formals(saomnk_plot_bipartite_ring_markets)))
  expect_true("epistatic_int_mat" %in% names(formals(saomnk_plot_bipartite_ring_markets)))
  expect_true("influence_matrix" %in% names(formals(saomnk_plot_bipartite_ring_markets_animation)))
})
