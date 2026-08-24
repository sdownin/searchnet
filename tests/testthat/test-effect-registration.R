## Regression tests for effect registration.
##
## Guards the defect introduced in aa39135 (2026-04-07) and fixed in 0.3.2, in which
## monadic covariate effects were silently dropped and, once included, carried their
## coefficient in the wrong column so they had no effect on the simulation.
##
## Two invariants are asserted here:
##   R1  every effect declared in a structure_model is actually INCLUDED
##   R2  its declared `parameter` reaches the `initialValue` column, which is
##       what get_theta_matrix() reads (`theta_in <- effs$initialValue`) to
##       drive the simulation. (Until the 2026-08-23 theta-storage repair this
##       invariant was stated on `parm`; that convention corrupted the
##       statistic of '#'-carrying effects and left cycle4/XWX/X inert --
##       see test-theta-storage.R.)
##
## A third, behavioural check (R3) confirms the effect is not merely registered but live.

DV_NAME <- "self$bipartite_rsienaDV"

.mk_env <- function(M = 6, N = 8, seed = 1234) {
  SaomNkRSienaBiEnv$new(list(M = M, N = N, BI_PROB = 0.3,
                             rand_seed = seed, name = "_effect_registration_"))
}

.mk_model <- function(N = 8, M = 6, rates = list(), egoX_par = 0.7) {
  grp <- rep(c(0, 1), length.out = M)
  list(dv_bipartite = list(
    name = DV_NAME,
    rates = rates,
    effects = list(
      list(effect = "density", parameter = -0.5, dv_name = DV_NAME, fix = TRUE),
      list(effect = "inPop",   parameter = -0.3, dv_name = DV_NAME, fix = TRUE)
    ),
    coCovars = list(
      list(effect = "egoX", parameter = egoX_par, dv_name = DV_NAME, fix = TRUE,
           interaction1 = "self$strat_1_coCovar", x = grp)
    ),
    coDyadCovars = list(
      list(effect = "XWX", parameter = 0.6, dv_name = DV_NAME, fix = TRUE,
           nodeSet = c("COMPONENTS", "COMPONENTS"),
           interaction1 = "self$component_1_coDyadCovar",
           x = create_block_diag(N, 2))
    )
  ))
}

.included <- function(env) {
  tb <- as.data.frame(env$rsiena_effects)
  tb[tb$include %in% TRUE, , drop = FALSE]
}


test_that("R1: monadic covariate effects are not silently dropped", {
  env <- .mk_env()
  expect_no_error(
    suppressWarnings(env$search_rsiena(structure_model = .mk_model(),
                                       iterations_per_actor = 5, run_seed = 999))
  )
  inc <- .included(env)
  expect_true("egoX" %in% inc$shortName)
  expect_true("XWX"  %in% inc$shortName)
})


test_that("R2: declared parameter reaches the `initialValue` column that drives the theta matrix", {
  env <- .mk_env()
  suppressWarnings(env$search_rsiena(structure_model = .mk_model(egoX_par = 0.7),
                                     iterations_per_actor = 5, run_seed = 999))
  inc <- .included(env)
  ego <- inc[inc$shortName == "egoX", , drop = FALSE]
  expect_equal(nrow(ego), 1L)
  ## `initialValue`, not `parm`, is what get_theta_matrix() reads
  ## (theta-storage convention, 2026-08-23).
  expect_equal(unname(ego$initialValue[1]), 0.7)
  ## and `parm` stays at RSiena's default, so the STATISTIC is untouched.
  expect_equal(unname(ego$parm[1]), 0)
})


test_that("R3: a covariate effect is live -- changing its parameter changes the outcome", {
  final_scope <- function(par) {
    env <- .mk_env()
    suppressWarnings(env$search_rsiena(structure_model = .mk_model(egoX_par = par),
                                       iterations_per_actor = 20, run_seed = 777))
    arr <- env$bi_env_arr
    rowSums(arr[, , dim(arr)[3]])
  }
  expect_false(identical(final_scope(0), final_scope(3)))
})


test_that("heterogeneous rate effects (RateX) register with their covariate", {
  rates <- list(list(effect = "RateX", parameter = 0.8, dv_name = DV_NAME,
                     fix = TRUE, interaction1 = "self$strat_1_coCovar"))
  env <- .mk_env()
  expect_no_error(
    suppressWarnings(env$search_rsiena(structure_model = .mk_model(rates = rates),
                                       iterations_per_actor = 5, run_seed = 999))
  )
  inc <- .included(env)
  rx <- inc[inc$shortName == "RateX", , drop = FALSE]
  expect_equal(nrow(rx), 1L)
  expect_equal(rx$type[1], "rate")
  expect_equal(rx$interaction1[1], "self$strat_1_coCovar")
  ## theta-storage convention (2026-08-23): the coefficient lives in
  ## `initialValue`, and get_theta_matrix() reads it from there.
  expect_equal(unname(rx$initialValue[1]), 0.8)
})


test_that("degree-dependent rate effects (outRate) register", {
  rates <- list(list(effect = "outRate", parameter = 0.3, dv_name = DV_NAME, fix = TRUE))
  env <- .mk_env()
  expect_no_error(
    suppressWarnings(env$search_rsiena(structure_model = .mk_model(rates = rates),
                                       iterations_per_actor = 5, run_seed = 999))
  )
  expect_true("outRate" %in% .included(env)$shortName)
})


test_that("non-basic rate effects occupy theta columns; only basic Rate is excluded", {
  rates <- list(list(effect = "RateX", parameter = 0.8, dv_name = DV_NAME,
                     fix = TRUE, interaction1 = "self$strat_1_coCovar"))
  env <- .mk_env()
  suppressWarnings(env$search_rsiena(structure_model = .mk_model(rates = rates),
                                     iterations_per_actor = 5, run_seed = 999))
  th <- env$get_rsiena_effects_theta_df(no_rates = TRUE)
  ## The basic rate parameter is dropped ...
  expect_false("Rate" %in% th$shortName)
  ## ... but RateX is not: a blanket /rate/i filter under-counts the thetaValues columns
  ## and RSiena rejects the matrix.
  expect_true("RateX" %in% th$shortName)
})
