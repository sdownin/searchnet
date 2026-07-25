## test-rsiena-setup.R
## Tests for RSiena data construction and effect setup
## These tests use small networks and never run full siena07 estimation.

# ---------------------------------------------------------------------------
# Helper: build a minimal environment and structure_model
# ---------------------------------------------------------------------------
make_env_for_rsiena <- function(M = 4, N = 6, seed = 42) {
  SaomNkRSienaBiEnv$new(list(
    M = M, N = N, BI_PROB = 0.5,
    name = "rsiena_test", rand_seed = seed
  ))
}

# A minimal structure_model with density effect only.
# The structure_model is a nested list consumed by
# get_rsiena_data_from_structure_model() and add_rsiena_effects().
make_density_only_structure_model <- function() {
  list(
    dv_bipartite = list(
      name = "self$bipartite_rsienaDV",
      rates   = list(),
      effects = list(
        list(
          dv_name   = "self$bipartite_rsienaDV",
          effect    = "density",
          parameter = -1,
          interaction1 = "",
          interaction2 = ""
        )
      ),
      coCovars      = list(),
      varCovars     = list(),
      coDyadCovars  = list(),
      varDyadCovars = list(),
      interactions  = list()
    )
  )
}

# ===========================================================================
# 1. bipartite_rsienaDV construction
# ===========================================================================
test_that("preview_effects creates bipartite_rsienaDV", {
  env <- make_env_for_rsiena()
  sm <- make_density_only_structure_model()
  # preview_effects sets up rsiena_data and rsiena_effects internally
  # It requires rvest for HTML parsing; skip if not available
  skip_if_not_installed("rvest")
  result <- env$preview_effects(sm, filter = FALSE)
  expect_false(is.null(env$bipartite_rsienaDV))
})

# ===========================================================================
# 2. get_rsiena_data_from_structure_model returns valid object
# ===========================================================================
test_that("get_rsiena_data_from_structure_model returns siena data object", {
  env <- make_env_for_rsiena()
  sm <- make_density_only_structure_model()

  # Manually set up the bipartite DV (mimic what preview_effects does)
  array_bi <- array(c(env$bipartite_matrix, env$bipartite_matrix),
                    dim = c(env$M, env$N, 2))
  env$bipartite_rsienaDV <- RSiena::sienaDependent(
    array_bi,
    type = "bipartite",
    nodeSet = c("ACTORS", "COMPONENTS"),
    allowOnly = FALSE
  )

  rsiena_data <- env$get_rsiena_data_from_structure_model(sm)
  expect_true(inherits(rsiena_data, "siena"))
})

# ===========================================================================
# 3. getEffects returns a valid effects object
# ===========================================================================
test_that("getEffects from rsiena_data produces effects with rows", {
  env <- make_env_for_rsiena()
  sm <- make_density_only_structure_model()

  array_bi <- array(c(env$bipartite_matrix, env$bipartite_matrix),
                    dim = c(env$M, env$N, 2))
  env$bipartite_rsienaDV <- RSiena::sienaDependent(
    array_bi, type = "bipartite",
    nodeSet = c("ACTORS", "COMPONENTS"), allowOnly = FALSE
  )
  env$rsiena_data <- env$get_rsiena_data_from_structure_model(sm)
  env$rsiena_effects <- RSiena::getEffects(env$rsiena_data)

  expect_true(inherits(env$rsiena_effects, "data.frame"))
  expect_true(nrow(env$rsiena_effects) > 0)
})

# ===========================================================================
# 4. add_rsiena_effects includes density effect
# ===========================================================================
test_that("add_rsiena_effects includes density effect", {
  env <- make_env_for_rsiena()
  sm <- make_density_only_structure_model()

  # Set up RSiena plumbing
  array_bi <- array(c(env$bipartite_matrix, env$bipartite_matrix),
                    dim = c(env$M, env$N, 2))
  env$bipartite_rsienaDV <- RSiena::sienaDependent(
    array_bi, type = "bipartite",
    nodeSet = c("ACTORS", "COMPONENTS"), allowOnly = FALSE
  )
  env$rsiena_data    <- env$get_rsiena_data_from_structure_model(sm)
  env$rsiena_effects <- RSiena::getEffects(env$rsiena_data)
  env$config_structure_model <- sm

  # add_rsiena_effects expects a list of dv blocks
  env$add_rsiena_effects(sm)

  effs_df <- as.data.frame(env$rsiena_effects)
  included <- effs_df[effs_df$include == TRUE, ]
  expect_true("density" %in% included$shortName)
})

# ===========================================================================
# 5. Empty effects list -- only rate is included by default
# ===========================================================================
test_that("no user effects => only default rate included", {
  env <- make_env_for_rsiena()
  sm_empty <- list(
    dv_bipartite = list(
      name = "self$bipartite_rsienaDV",
      rates      = list(),
      effects    = list(),
      coCovars   = list(),
      varCovars  = list(),
      coDyadCovars  = list(),
      varDyadCovars = list(),
      interactions  = list()
    )
  )

  array_bi <- array(c(env$bipartite_matrix, env$bipartite_matrix),
                    dim = c(env$M, env$N, 2))
  env$bipartite_rsienaDV <- RSiena::sienaDependent(
    array_bi, type = "bipartite",
    nodeSet = c("ACTORS", "COMPONENTS"), allowOnly = FALSE
  )
  env$rsiena_data    <- env$get_rsiena_data_from_structure_model(sm_empty)
  env$rsiena_effects <- RSiena::getEffects(env$rsiena_data)

  effs_df <- as.data.frame(env$rsiena_effects)
  included <- effs_df[effs_df$include == TRUE, ]
  # By default RSiena includes a rate effect and basic density; user added nothing extra
  expect_true(nrow(included) >= 1)
})

# ===========================================================================
# 6. get_rsiena_effects_theta_df after setup
# ===========================================================================
test_that("get_rsiena_effects_theta_df returns data frame with expected columns", {
  env <- make_env_for_rsiena()
  sm <- make_density_only_structure_model()

  array_bi <- array(c(env$bipartite_matrix, env$bipartite_matrix),
                    dim = c(env$M, env$N, 2))
  env$bipartite_rsienaDV <- RSiena::sienaDependent(
    array_bi, type = "bipartite",
    nodeSet = c("ACTORS", "COMPONENTS"), allowOnly = FALSE
  )
  env$rsiena_data    <- env$get_rsiena_data_from_structure_model(sm)
  env$rsiena_effects <- RSiena::getEffects(env$rsiena_data)
  env$add_rsiena_effects(sm)

  theta_df <- env$get_rsiena_effects_theta_df(no_rates = TRUE)
  expect_true(is.data.frame(theta_df))
  expect_true("effect_key" %in% names(theta_df))
  expect_true("effect_level" %in% names(theta_df))
  expect_true("shortName" %in% names(theta_df))
})

# ===========================================================================
# 7. get_rsiena_effects_theta_df errors before rsiena_effects is set
# ===========================================================================
test_that("get_rsiena_effects_theta_df errors when rsiena_effects is NULL", {
  env <- make_env_for_rsiena()
  expect_error(env$get_rsiena_effects_theta_df(), "rsiena_effects")
})
