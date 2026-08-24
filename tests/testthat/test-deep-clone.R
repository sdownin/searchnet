###############################################################################
## test-deep-clone.R
## clone(deep = TRUE) must produce an env that shares no mutable state.
##
## R6's default deep clone recurses only into fields that are themselves R6
## objects. A data.table is not one, so before private$deep_clone() existed the
## clone and the original were bound to the SAME data.table, and `:=` -- which
## deliberately bypasses copy-on-modify -- wrote through to both.
##
## The failure this prevents is cross-contaminated runs in a seed batch
## (plot-markets.R clones a template env per seed), which presents as results
## that are subtly wrong rather than as an error.
###############################################################################

make_clone_env <- function() {
  cfg <- list(M = 4, N = 6, BI_PROB = 0.3,
              name = "clone_test", rand_seed = 42)
  SaomNkRSienaBiEnv_base$new(cfg)
}

test_that("a data.table field is copied, not shared, by clone(deep = TRUE)", {
  skip_if_not_installed("data.table")
  env <- make_clone_env()
  env$actor_stats_df <- data.table::data.table(actor = 1:4, value = 1)

  cl <- env$clone(deep = TRUE)

  ## Distinct objects. address() is the direct evidence; identical() would pass
  ## even when both names point at one table, so it cannot see this bug.
  expect_false(identical(data.table::address(env$actor_stats_df),
                         data.table::address(cl$actor_stats_df)))

  ## The substantive assertion: a by-reference update on the clone must not
  ## reach the original. This is the assertion that fails without deep_clone().
  cl$actor_stats_df[, probe := 1L]
  expect_false("probe" %in% names(env$actor_stats_df))
  expect_true("probe" %in% names(cl$actor_stats_df))

  ## ...and symmetrically, so neither direction is privileged.
  env$actor_stats_df[, other := 2L]
  expect_false("other" %in% names(cl$actor_stats_df))
})

test_that("clone(deep = TRUE) isolates value-typed fields", {
  env <- make_clone_env()
  cl  <- env$clone(deep = TRUE)

  before <- env$bipartite_matrix[1, 1]
  cl$bipartite_matrix[1, 1] <- if (is.na(before)) 1 else before + 99
  expect_identical(env$bipartite_matrix[1, 1], before)
})

test_that("clone(deep = TRUE) isolates igraph fields", {
  skip_if_not_installed("igraph")
  env <- make_clone_env()
  cl  <- env$clone(deep = TRUE)
  skip_if(is.null(env$bipartite_igraph))

  n_before <- igraph::gsize(env$bipartite_igraph)
  skip_if(n_before < 1)

  ## igraph's API returns new graphs rather than mutating, so this should hold
  ## with or without deep_clone(). Asserted anyway: the graphs carry an internal
  ## environment, and if a future igraph made mutation in-place this is where
  ## the regression would surface.
  cl$bipartite_igraph <- igraph::delete_edges(cl$bipartite_igraph,
                                              igraph::E(cl$bipartite_igraph)[1])
  expect_identical(igraph::gsize(env$bipartite_igraph), n_before)
})

test_that("deep_clone leaves non-data.table fields untouched", {
  env <- make_clone_env()
  env$config_environ_params <- list(a = 1, b = "two")
  cl <- env$clone(deep = TRUE)

  ## A pass-through, not a transformation: the guard must not silently coerce
  ## or drop anything it does not recognize.
  expect_identical(cl$config_environ_params, env$config_environ_params)
  expect_identical(cl$M, env$M)
  expect_identical(cl$N, env$N)
})
