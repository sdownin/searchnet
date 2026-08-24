###############################################################################
## test-theta-storage.R
##
## Theta must be carried in the effects table's `initialValue` column and read
## from there by get_theta_matrix(). `parm` is RSiena's INTERNAL effect
## parameter (the `#` substitution in effect/function names -- a root exponent
## for cycle4, inPopX, outActX); it is NOT a coefficient, and writing a
## coefficient into it silently changes WHICH statistic is computed.
##
## Guards the defect found 2026-08-23 (see the "`parm` is NOT theta" section of
## the SAOM Estimation method note): get_theta_matrix() read `effs$parm`, while
## the cycle4 / XWX / X branches of include_rsiena_effect_from_eff_list() wrote
## the coefficient into `initialValue`. Consequence: cycle4 was ALWAYS simulated
## at its parm default (1), and XWX / X at 0, whatever the caller asked for.
##
## These are BEHAVIORAL tests where possible: the package's own history
## records an effect that "registered but was inert", caught only because a
## test checked behavior rather than registration.
###############################################################################

DV_NAME <- "self$bipartite_rsienaDV"

.theta_env <- function(M = 6, N = 8, seed = 4242) {
  SaomNkRSienaBiEnv$new(list(M = M, N = N, BI_PROB = 0.3,
                             rand_seed = seed, name = "_theta_storage_",
                             dir_output = tempdir()))
}

.cycle4_model <- function(cycle4_par) {
  ## Both `parameter` and `initialValue` are supplied (equal), matching the
  ## shape the bridge has always emitted, so this model is accepted by every
  ## historical version of the cycle4 branch. The declared coefficient must
  ## reach the simulation either way.
  list(dv_bipartite = list(
    name = DV_NAME,
    effects = list(
      list(effect = "density", parameter = -1, dv_name = DV_NAME, fix = TRUE),
      list(effect = "cycle4", parameter = cycle4_par,
           initialValue = cycle4_par, dv_name = DV_NAME, fix = TRUE)
    ),
    coCovars = list(), varCovars = list(),
    coDyadCovars = list(), varDyadCovars = list(), interactions = list()
  ))
}

test_that("cycle4: the declared coefficient reaches the simulated theta", {
  skip_if_not_installed("RSiena")
  env <- .theta_env()
  th <- suppressWarnings(suppressMessages(
    env$prepare_theta_scaffold(.cycle4_model(-2), iterations = 30)))
  nm <- sub("_[0-9]+$", "", colnames(th))
  expect_true("cycle4" %in% nm)
  ## Before the 2026-08 repair this column was parm's default, 1, regardless
  ## of the declared coefficient.
  expect_equal(unname(th[1, which(nm == "cycle4")[1]]), -2)
})

test_that("cycle4: changing ONLY its coefficient changes the simulated outcome", {
  skip_if_not_installed("RSiena")
  run_arm <- function(cycle4_par) {
    env <- .theta_env()
    suppressWarnings(suppressMessages(
      env$search_rsiena(structure_model = .cycle4_model(cycle4_par),
                        iterations_per_actor = 15, run_seed = 777,
                        verbose = FALSE)))
    arr <- env$bi_env_arr
    arr[, , dim(arr)[3]]
  }
  lo <- run_arm(-2)
  hi <- run_arm( 2)
  ## Identical final matrices under identical seeds means the coefficient
  ## never reached siena07's thetaValues: the effect registered but was inert.
  expect_false(identical(lo, hi))
  ## Direction check: a positive 4-cycle coefficient must not produce FEWER
  ## 4-cycles than a negative one (weak inequality; equal only if both hit a
  ## boundary, which the previous assertion already excludes).
  count_c4 <- function(m) { XXt <- m %*% t(m); sum(diag(XXt %*% XXt %*% XXt)) }
  expect_gte(count_c4(hi), count_c4(lo))
})

test_that("cycle4: the effect's statistic is not renamed by the coefficient", {
  ## Writing the coefficient into `parm` rewrites the '#' in RSiena's effect
  ## name (e.g. "4 cycles (-2)") and changes the statistic to count^(1/-2).
  ## The registered effect must remain the raw count, "4 cycles (1)".
  skip_if_not_installed("RSiena")
  env <- .theta_env()
  suppressWarnings(suppressMessages(
    env$prepare_theta_scaffold(.cycle4_model(-2), iterations = 10)))
  tb <- as.data.frame(env$rsiena_effects)
  row <- tb[tb$include %in% TRUE & tb$shortName == "cycle4", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_equal(unname(row$parm[1]), 1)
  expect_equal(unname(row$initialValue[1]), -2)
})

test_that("XWX: the declared influence weight reaches the simulated theta", {
  skip_if_not_installed("RSiena")
  N <- 8
  env <- .theta_env(N = N)
  model <- list(dv_bipartite = list(
    name = DV_NAME,
    effects = list(
      list(effect = "density", parameter = -0.5, dv_name = DV_NAME, fix = TRUE)
    ),
    coCovars = list(), varCovars = list(),
    coDyadCovars = list(
      list(effect = "XWX", parameter = 0.8, dv_name = DV_NAME, fix = TRUE,
           nodeSet = c("COMPONENTS", "COMPONENTS"),
           interaction1 = "self$component_1_coDyadCovar",
           x = create_block_diag(N, 2))
    ),
    varDyadCovars = list(), interactions = list()
  ))
  th <- suppressWarnings(suppressMessages(
    env$prepare_theta_scaffold(model, iterations = 30)))
  nm <- sub("_[0-9]+$", "", colnames(th))
  expect_true("XWX" %in% nm)
  ## Before the repair this column was parm's default, 0: the influence weight
  ## registered, printed, and simulated at zero.
  expect_equal(unname(th[1, which(nm == "XWX")[1]]), 0.8)
})

test_that("X: the declared dyadic-covariate weight reaches the simulated theta", {
  skip_if_not_installed("RSiena")
  M <- 6; N <- 8
  env <- .theta_env(M = M, N = N)
  dc <- matrix(stats::rnorm(M * N), M, N)
  model <- list(dv_bipartite = list(
    name = DV_NAME,
    effects = list(
      list(effect = "density", parameter = -0.5, dv_name = DV_NAME, fix = TRUE)
    ),
    coCovars = list(), varCovars = list(),
    coDyadCovars = list(
      list(effect = "X", parameter = 0.4, dv_name = DV_NAME, fix = TRUE,
           nodeSet = c("ACTORS", "COMPONENTS"),
           interaction1 = "self$component_1_coDyadCovar",
           x = dc)
    ),
    varDyadCovars = list(), interactions = list()
  ))
  th <- suppressWarnings(suppressMessages(
    env$prepare_theta_scaffold(model, iterations = 30)))
  nm <- sub("_[0-9]+$", "", colnames(th))
  expect_true("X" %in% nm)
  expect_equal(unname(th[1, which(nm == "X")[1]]), 0.4)
})

test_that("internal_parameter sets RSiena's parm without touching theta", {
  ## A caller may legitimately want cycle4's square-root form (parm = 2).
  ## That is a DIFFERENT request from a coefficient, made through a different
  ## key, so the two cannot be confused at the call site.
  skip_if_not_installed("RSiena")
  env <- .theta_env()
  model <- .cycle4_model(0.3)
  model$dv_bipartite$effects[[2]]$initialValue <- NULL
  model$dv_bipartite$effects[[2]]$internal_parameter <- 2
  th <- suppressWarnings(suppressMessages(
    env$prepare_theta_scaffold(model, iterations = 10)))
  tb <- as.data.frame(env$rsiena_effects)
  row <- tb[tb$include %in% TRUE & tb$shortName == "cycle4", , drop = FALSE]
  expect_equal(unname(row$parm[1]), 2)          ## the sqrt form was selected
  expect_equal(unname(row$initialValue[1]), 0.3) ## theta untouched by parm
  nm <- sub("_[0-9]+$", "", colnames(th))
  expect_equal(unname(th[1, which(nm == "cycle4")[1]]), 0.3)
})
