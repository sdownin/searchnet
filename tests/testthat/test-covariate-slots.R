## test-covariate-slots.R
##
## Regression tests for the covariate-registration block shared by
## get_rsiena_data_from_structure_model() and its near-duplicate twin
## get_rsiena_data_static() in R/saomnk-class.R.
##
## Four things are pinned here, each of which was silently wrong:
##
##   A. the strat coDyadCovar loop indexed the COMPONENT id vector, so a
##      strategy dyadic covariate fetched the component one;
##   B. the varCovar loops built their property name with the `_coCovar`
##      suffix, so a varCovar overwrote the coCovar slot of the same index
##      and nothing answered to interaction1 = "self$<kind>_i_varCovar";
##   C. the coDyadCovar nodeSet inference repeated the outer condition in
##      the inner `if`, making the second branch unreachable -- an
##      unambiguous N x N (or M x M) covariate was rejected outright;
##   D. the block reached into `$coDyadCovar` / `$varDyadCovar` (singular)
##      and relied on `$` partial-matching to the plural key producers emit.
##
## No siena07() estimation is run; every test stops at sienaDataCreate().

DVN <- "self$bipartite_rsienaDV"

## Environment with the bipartite DV already installed, as preview_effects()
## would leave it. `waves` matters: RSiena refuses changing covariates with
## only two waves, so the varCovar tests ask for three.
make_cov_env <- function(M = 4, N = 6, waves = 2, seed = 42) {
  env <- SaomNkRSienaBiEnv$new(list(
    M = M, N = N, BI_PROB = 0.5,
    name = "covariate_slot_test", rand_seed = seed,
    dir_output = tempdir()
  ))
  arr <- array(rep(env$bipartite_matrix, waves), dim = c(env$M, env$N, waves))
  env$bipartite_rsienaDV <- RSiena::sienaDependent(
    arr, type = "bipartite",
    nodeSet = c("ACTORS", "COMPONENTS"), allowOnly = FALSE
  )
  env
}

## Skeleton structure model; `...` overrides any of the covariate lists.
make_cov_structure_model <- function(...) {
  dv <- list(
    name    = DVN,
    rates   = list(),
    effects = list(list(effect = "density", parameter = -1,
                        dv_name = DVN, fix = TRUE)),
    coCovars      = list(),
    varCovars     = list(),
    coDyadCovars  = list(),
    varDyadCovars = list(),
    interactions  = list()
  )
  overrides <- list(...)
  for (nm in names(overrides)) dv[[nm]] <- overrides[[nm]]
  list(dv_bipartite = dv)
}

## get_rsiena_data_from_structure_model() consults self$config_structure_model
## via get_structure_model_params(), so register the model on the env first.
register <- function(env, sm) {
  env$config_structure_model <- sm
  env$get_rsiena_data_from_structure_model(sm)
}

# ===========================================================================
# DEFECT A -- the strat coDyadCovar loop must index the strat id vector
# ===========================================================================
test_that("a strategy coDyadCovar is not filled from the component one", {
  env <- make_cov_env(M = 4, N = 6)

  ## Both kinds present, component first, so the component id vector (1) and
  ## the strat id vector (2) disagree. Explicit nodeSets keep the inference
  ## branch out of it: the only thing under test is WHICH element is fetched.
  sm <- make_cov_structure_model(coDyadCovars = list(
    list(effect = "XWX", parameter = 0.1, dv_name = DVN, fix = TRUE,
         interaction1 = "self$component_1_coDyadCovar",
         x = matrix(as.numeric(1:36), nrow = 6, ncol = 6),
         nodeSet = c("COMPONENTS", "COMPONENTS")),
    list(effect = "WWX", parameter = 0.2, dv_name = DVN, fix = TRUE,
         interaction1 = "self$strat_1_coDyadCovar",
         x = matrix(as.numeric(1:16), nrow = 4, ncol = 4),
         nodeSet = c("ACTORS", "ACTORS"))
  ))

  rsiena_data <- suppressWarnings(register(env, sm))
  expect_true(inherits(rsiena_data, "siena"))

  ## Wrong index => strat_1_coDyadCovar would be the component's 6x6 matrix
  ## on the COMPONENTS node set.
  expect_equal(dim(env$strat_1_coDyadCovar), c(4L, 4L))
  expect_equal(attr(env$strat_1_coDyadCovar, "nodeSet"), c("ACTORS", "ACTORS"))
  expect_equal(dim(env$component_1_coDyadCovar), c(6L, 6L))
  expect_equal(attr(env$component_1_coDyadCovar, "nodeSet"),
               c("COMPONENTS", "COMPONENTS"))

  ## And the two must not be the same object dressed twice.
  expect_false(identical(as.numeric(env$strat_1_coDyadCovar),
                         as.numeric(env$component_1_coDyadCovar)))
})

test_that("get_rsiena_data_static keeps strat and component coDyadCovars apart", {
  env <- make_cov_env(M = 4, N = 6)

  sm <- make_cov_structure_model(coDyadCovars = list(
    list(effect = "XWX", parameter = 0.1, dv_name = DVN, fix = TRUE,
         interaction1 = "self$component_1_coDyadCovar",
         x = matrix(as.numeric(1:36), nrow = 6, ncol = 6),
         nodeSet = c("COMPONENTS", "COMPONENTS")),
    list(effect = "WWX", parameter = 0.2, dv_name = DVN, fix = TRUE,
         interaction1 = "self$strat_1_coDyadCovar",
         x = matrix(as.numeric(1:16), nrow = 4, ncol = 4),
         nodeSet = c("ACTORS", "ACTORS"))
  ))
  env$config_structure_model <- sm

  input_varlist <- list()
  input_varlist[[DVN]] <- env$bipartite_rsienaDV
  rsiena_data <- suppressWarnings(env$get_rsiena_data_static(sm, input_varlist))

  expect_true(inherits(rsiena_data, "siena"))
  expect_equal(dim(rsiena_data$dycCovars[["strat_1_coDyadCovar"]]), c(4L, 4L))
  expect_equal(dim(rsiena_data$dycCovars[["component_1_coDyadCovar"]]), c(6L, 6L))
})

# ===========================================================================
# DEFECT B -- varCovar registers under a _varCovar name, not _coCovar
# ===========================================================================
test_that("a component varCovar fills its own slot and leaves coCovar alone", {
  env <- make_cov_env(M = 4, N = 6, waves = 3)

  sm <- make_cov_structure_model(
    coCovars = list(
      list(effect = "inPop", parameter = 0.2, dv_name = DVN, fix = TRUE,
           interaction1 = "self$component_1_coCovar",
           x = as.numeric(rep(0:1, length.out = 6)))
    ),
    varCovars = list(
      list(effect = "inPopX", parameter = 0.1, dv_name = DVN, fix = TRUE,
           interaction1 = "self$component_1_varCovar",
           x = matrix(as.numeric(1:12), nrow = 6, ncol = 2))
    )
  )

  rsiena_data <- suppressWarnings(register(env, sm))

  ## With the _coCovar suffix the varCovar landed in component_1_coCovar and
  ## component_1_varCovar stayed NULL.
  expect_true(inherits(env$component_1_varCovar, "varCovar"))
  expect_true(inherits(env$component_1_coCovar,  "coCovar"))
  expect_false(inherits(env$component_1_coCovar, "varCovar"))

  ## The RSiena variable name is what an effect's interaction1 addresses.
  expect_true("self$component_1_varCovar" %in% names(rsiena_data$vCovars))
  expect_true("self$component_1_coCovar"  %in% names(rsiena_data$cCovars))
})

test_that("a strategy varCovar fills its own slot and leaves coCovar alone", {
  env <- make_cov_env(M = 4, N = 6, waves = 3)

  sm <- make_cov_structure_model(
    coCovars = list(
      list(effect = "egoX", parameter = 0.2, dv_name = DVN, fix = TRUE,
           interaction1 = "self$strat_1_coCovar",
           x = as.numeric(rep(0:1, length.out = 4)))
    ),
    varCovars = list(
      list(effect = "egoX", parameter = 0.1, dv_name = DVN, fix = TRUE,
           interaction1 = "self$strat_1_varCovar",
           x = matrix(as.numeric(1:8), nrow = 4, ncol = 2))
    )
  )

  rsiena_data <- suppressWarnings(register(env, sm))

  expect_true(inherits(env$strat_1_varCovar, "varCovar"))
  expect_true(inherits(env$strat_1_coCovar,  "coCovar"))
  expect_false(inherits(env$strat_1_coCovar, "varCovar"))

  expect_true("self$strat_1_varCovar" %in% names(rsiena_data$vCovars))
  expect_true("self$strat_1_coCovar"  %in% names(rsiena_data$cCovars))
})

test_that("get_rsiena_data_static registers varCovars under _varCovar names", {
  env <- make_cov_env(M = 4, N = 6, waves = 3)

  sm <- make_cov_structure_model(
    coCovars = list(
      list(effect = "inPop", parameter = 0.2, dv_name = DVN, fix = TRUE,
           interaction1 = "self$component_1_coCovar",
           x = as.numeric(rep(0:1, length.out = 6)))
    ),
    varCovars = list(
      list(effect = "inPopX", parameter = 0.1, dv_name = DVN, fix = TRUE,
           interaction1 = "self$component_1_varCovar",
           x = matrix(as.numeric(1:12), nrow = 6, ncol = 2))
    )
  )
  env$config_structure_model <- sm

  input_varlist <- list()
  input_varlist[[DVN]] <- env$bipartite_rsienaDV
  rsiena_data <- suppressWarnings(env$get_rsiena_data_static(sm, input_varlist))

  ## Sharing the _coCovar name made the varCovar clobber the coCovar entry of
  ## input_varlist, so one of the two simply vanished from the siena object.
  expect_true("component_1_coCovar"  %in% names(rsiena_data$cCovars))
  expect_true("component_1_varCovar" %in% names(rsiena_data$vCovars))
})

# ===========================================================================
# DEFECT C -- both nodeSet-inference branches must be reachable
# ===========================================================================
test_that("an N x N component coDyadCovar infers COMPONENTS x COMPONENTS", {
  env <- make_cov_env(M = 4, N = 6)

  sm <- make_cov_structure_model(coDyadCovars = list(
    list(effect = "XWX", parameter = 0.1, dv_name = DVN, fix = TRUE,
         interaction1 = "self$component_1_coDyadCovar",
         x = matrix(as.numeric(1:36), nrow = 6, ncol = 6))   ## no nodeSet
  ))

  ## The unreachable inner branch meant this hit the stop() instead.
  rsiena_data <- suppressWarnings(register(env, sm))
  expect_true(inherits(rsiena_data, "siena"))
  expect_equal(attr(env$component_1_coDyadCovar, "nodeSet"),
               c("COMPONENTS", "COMPONENTS"))
})

test_that("an M x N component coDyadCovar still infers ACTORS x COMPONENTS", {
  env <- make_cov_env(M = 4, N = 6)

  sm <- make_cov_structure_model(coDyadCovars = list(
    list(effect = "XWX", parameter = 0.1, dv_name = DVN, fix = TRUE,
         interaction1 = "self$component_1_coDyadCovar",
         x = matrix(as.numeric(1:24), nrow = 4, ncol = 6))    ## no nodeSet
  ))

  rsiena_data <- suppressWarnings(register(env, sm))
  expect_true(inherits(rsiena_data, "siena"))
  expect_equal(attr(env$component_1_coDyadCovar, "nodeSet"),
               c("ACTORS", "COMPONENTS"))
})

test_that("an M x M strategy coDyadCovar infers ACTORS x ACTORS", {
  env <- make_cov_env(M = 4, N = 6)

  sm <- make_cov_structure_model(coDyadCovars = list(
    list(effect = "WWX", parameter = 0.2, dv_name = DVN, fix = TRUE,
         interaction1 = "self$strat_1_coDyadCovar",
         x = matrix(as.numeric(1:16), nrow = 4, ncol = 4))    ## no nodeSet
  ))

  rsiena_data <- suppressWarnings(register(env, sm))
  expect_true(inherits(rsiena_data, "siena"))
  expect_equal(attr(env$strat_1_coDyadCovar, "nodeSet"), c("ACTORS", "ACTORS"))
})

test_that("M == N stays ambiguous and still errors", {
  ## The stop() is correct here: a 5x5 matrix with M == N == 5 could be either
  ## an actor-by-component or a component-by-component covariate, and nothing
  ## in the dimensions decides. This must NOT be "fixed" away.
  env <- make_cov_env(M = 5, N = 5)

  sm <- make_cov_structure_model(coDyadCovars = list(
    list(effect = "XWX", parameter = 0.1, dv_name = DVN, fix = TRUE,
         interaction1 = "self$component_1_coDyadCovar",
         x = matrix(as.numeric(1:25), nrow = 5, ncol = 5))    ## no nodeSet
  ))

  expect_error(suppressWarnings(register(env, sm)),
               "Cannot distinguish actors from components")
})

test_that("dimensions matching neither shape error rather than guess", {
  env <- make_cov_env(M = 4, N = 6)

  sm <- make_cov_structure_model(coDyadCovars = list(
    list(effect = "XWX", parameter = 0.1, dv_name = DVN, fix = TRUE,
         interaction1 = "self$component_1_coDyadCovar",
         x = matrix(as.numeric(1:21), nrow = 3, ncol = 7))    ## neither MxN nor NxN
  ))

  expect_error(suppressWarnings(register(env, sm)),
               "Cannot distinguish actors from components")
})

test_that("an explicit nodeSet overrides dimension inference", {
  env <- make_cov_env(M = 5, N = 5)

  sm <- make_cov_structure_model(coDyadCovars = list(
    list(effect = "XWX", parameter = 0.1, dv_name = DVN, fix = TRUE,
         interaction1 = "self$component_1_coDyadCovar",
         x = matrix(as.numeric(1:25), nrow = 5, ncol = 5),
         nodeSet = c("COMPONENTS", "COMPONENTS"))
  ))

  rsiena_data <- suppressWarnings(register(env, sm))
  expect_true(inherits(rsiena_data, "siena"))
  expect_equal(attr(env$component_1_coDyadCovar, "nodeSet"),
               c("COMPONENTS", "COMPONENTS"))
})

# ===========================================================================
# D -- the dyadic accessors are exact, not `$` prefix matches
# ===========================================================================
test_that("a decoy singular coDyadCovar key does not hijack the plural one", {
  ## `$coDyadCovar` on a list holding only `coDyadCovars` partial-matches, so
  ## the old code worked by accident. Add a real `coDyadCovar` key and the
  ## exact match wins: the block would read the decoy instead. With both keys
  ## present `$coDyadCovar` is unambiguous, so this is the silent-wrong-answer
  ## case rather than the NULL[[1]] one.
  env <- make_cov_env(M = 4, N = 6)

  sm <- make_cov_structure_model(coDyadCovars = list(
    list(effect = "XWX", parameter = 0.1, dv_name = DVN, fix = TRUE,
         interaction1 = "self$component_1_coDyadCovar",
         x = matrix(as.numeric(1:36), nrow = 6, ncol = 6),
         nodeSet = c("COMPONENTS", "COMPONENTS"))
  ))
  sm$dv_bipartite$coDyadCovar <- list(
    list(effect = "DECOY", parameter = 99, dv_name = DVN, fix = TRUE,
         interaction1 = "self$component_1_coDyadCovar",
         x = matrix(-1, nrow = 4, ncol = 6),
         nodeSet = c("ACTORS", "COMPONENTS"))
  )

  rsiena_data <- suppressWarnings(register(env, sm))
  expect_true(inherits(rsiena_data, "siena"))
  expect_equal(dim(env$component_1_coDyadCovar), c(6L, 6L))
  expect_equal(attr(env$component_1_coDyadCovar, "nodeSet"),
               c("COMPONENTS", "COMPONENTS"))
})

test_that("a decoy singular varDyadCovar key does not hijack the plural one", {
  env <- make_cov_env(M = 4, N = 6, waves = 3)

  sm <- make_cov_structure_model(
    coCovars = list(
      list(effect = "inPop", parameter = 0.2, dv_name = DVN, fix = TRUE,
           interaction1 = "self$component_1_coCovar",
           x = as.numeric(rep(0:1, length.out = 6)))
    ),
    varDyadCovars = list(
      list(effect = "XWX", parameter = 0.1, dv_name = DVN, fix = TRUE,
           interaction1 = "self$component_1_varDyadCovar",
           x = array(as.numeric(1:72), dim = c(6, 6, 2)))
    )
  )
  sm$dv_bipartite$varDyadCovar <- list(
    list(effect = "DECOY", parameter = 99, dv_name = DVN, fix = TRUE,
         interaction1 = "self$component_1_varDyadCovar",
         x = array(-1, dim = c(4, 6, 2)))
  )

  rsiena_data <- suppressWarnings(register(env, sm))
  expect_true(inherits(rsiena_data, "siena"))
  expect_equal(dim(env$component_1_varDyadCovar)[1:2], c(6L, 6L))
})

## ---------------------------------------------------------------------------
## E. The covariate block's ENTRY GATE counted only the constant kinds.
##
## get_rsiena_data_from_structure_model() gated the whole block on
##   has_coCovars || has_coDyadCovars
## so a structure model declaring ONLY time-varying covariates took the
## no-covariates early return and registered NOTHING -- silently, with a
## converged model estimating a specification the author did not write.
##
## That is precisely the shape of a multi-W horserace built from
## `influence_arrays` and no static `influence_matrix`: every coupling dropped.
## ---------------------------------------------------------------------------

test_that("a model with ONLY time-varying influence still registers it", {
  env <- make_cov_env(M = 6, N = 6, waves = 3)
  W1 <- saomnk_block_diagonal(6, 2)
  W2 <- saomnk_block_diagonal(6, 3)

  sm <- saomnk_model(
    density = -0.5,
    influence_arrays = list(Wt = array(c(W1, W2), dim = c(6, 6, 2))),
    influence_array_weights = c(Wt = 0.3)
  )
  ## Precondition: the model really does declare ONLY the varying kind.
  expect_length(sm$dv_bipartite$coDyadCovars, 0L)
  expect_length(sm$dv_bipartite$varDyadCovars, 1L)

  d <- suppressWarnings(env$get_rsiena_data_from_structure_model(sm))
  expect_true(inherits(d, "siena"))
  ## The assertion that fails against the unfixed gate:
  expect_length(d$dyvCovars, 1L)
  expect_length(d$dycCovars, 0L)
})

test_that("all four static/time-varying declaration shapes register correctly", {
  W1 <- saomnk_block_diagonal(6, 2)
  W2 <- saomnk_block_diagonal(6, 3)
  Wt <- array(c(W1, W2), dim = c(6, 6, 2))

  shape <- function(...) {
    env <- make_cov_env(M = 6, N = 6, waves = 3)
    d <- suppressWarnings(
      env$get_rsiena_data_from_structure_model(saomnk_model(density = -0.5, ...)))
    c(constant = length(d$dycCovars), varying = length(d$dyvCovars))
  }

  expect_equal(shape(influence_arrays = list(Wt = Wt)),
               c(constant = 0L, varying = 1L))
  expect_equal(shape(influence_matrix = W1),
               c(constant = 1L, varying = 0L))
  expect_equal(shape(influence_matrix = W1, influence_arrays = list(Wt = Wt)),
               c(constant = 1L, varying = 1L))
  expect_equal(shape(),
               c(constant = 0L, varying = 0L))
})
