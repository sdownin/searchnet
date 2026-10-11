###############################################################################
## test-searchnet-stationarity.R
##
## searchnet_stationarity_check(): within-run verdicts on one simulated path.
## Each verdict is exercised on a run built to produce it, with fixed seeds:
##   stationary  a long run at moderate parameters, started near its level
##   trending    a short run from a near-complete start under a strong negative
##               density, so the density is still falling when the run ends
##   absorbed    a complete start under density = -8: every tie is deleted
##               and the empty network is never left
##   too_short   a run of about a dozen ministeps
## Synthetic data only.
###############################################################################

.st_model <- function(d, p = 0) list(dv_bipartite = list(
  name = DV_NAME,
  effects = list(
    list(effect = "density", parameter = d, dv_name = DV_NAME, fix = TRUE),
    list(effect = "inPop",   parameter = p, dv_name = DV_NAME, fix = TRUE)),
  rates = list(), coCovars = list(), varCovars = list(), coDyadCovars = list(),
  varDyadCovars = list(), interactions = list()))

.st_run <- function(M, N, p0, d, p, per_actor, seed, B_init = NULL) {
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(
    M = M, N = N, BI_PROB = p0, rand_seed = seed, name = "_st_"))
  if (!is.null(B_init)) {
    env$bipartite_matrix_init <- B_init
    env$set_system_from_bipartite_matrix(B_init)
  }
  suppressMessages(suppressWarnings(
    env$search_rsiena(structure_model = .st_model(d, p),
                      iterations_per_actor = per_actor, run_seed = seed)))
  env
}


## ---- internals on synthetic series (no simulation) -------------------------

test_that("long-run variance and ESS behave on white noise and on AR(1)", {
  set.seed(1)
  e <- stats::rnorm(4000)
  expect_equal(.searchnet_lrv(e) / stats::var(e), 1, tolerance = 0.15)
  expect_gt(.searchnet_ess(e), 3000)
  x <- as.numeric(stats::filter(stats::rnorm(20000), 0.9, method = "recursive"))
  ## S(0) / var = (1 + phi) / (1 - phi) = 19 for an AR(1) with phi = 0.9
  expect_equal(.searchnet_lrv(x) / stats::var(x), 19, tolerance = 0.25)
  expect_lt(.searchnet_ess(x), 2000)
  expect_equal(.searchnet_ess(rep(1, 50)), 0)
})

test_that("share since the last change counts the frozen tail", {
  expect_equal(.searchnet_share_since_change(c(1, 2, 3, 3, 3, 3, 3, 3, 3, 3)), 0.7)
  expect_equal(.searchnet_share_since_change(1:10), 0)
  expect_equal(.searchnet_share_since_change(rep(2, 4)), 0.75)
})


## ---- guards -----------------------------------------------------------------

test_that("guards: unsimulated env, unknown statistic, bad arguments, legacy path", {
  skip_if_not_installed("RSiena")
  env0 <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = 4, N = 5, rand_seed = 3))
  expect_error(searchnet_stationarity_check(env0), "No simulated path")

  env <- .st_run(4, 5, 0.4, -0.5, 0, 20, 31)
  expect_error(searchnet_stationarity_check(env, statistics = "betweenness"),
               "Unknown statistic")
  expect_error(searchnet_stationarity_check(env, burn_in = 1))
  expect_error(searchnet_stationarity_check(env, n_windows = 1))
  expect_error(searchnet_stationarity_check(env, alpha = 0))

  attr(env$bi_env_arr, "searchnet_path") <- "independent_draws"
  expect_error(searchnet_stationarity_check(env), class = "searchnet_not_a_path_error")
})

test_that("abort gate: K tables that do not belong to the path stop the check", {
  skip_if_not_installed("RSiena")
  env <- .st_run(4, 5, 0.4, -0.5, 0, 20, 32)
  k <- data.table::copy(env$K_AC_df)
  k$value[1] <- k$value[1] + 1L
  env$K_AC_df <- k
  expect_error(searchnet_stationarity_check(env, statistics = c("density", "mean_K_AC")),
               "disagrees with the path")
})


## ---- verdicts ---------------------------------------------------------------

test_that("a long run at moderate parameters reads stationary, with a bound", {
  skip_if_not_installed("RSiena")
  env <- .st_run(5, 6, 0.4, -0.6, 0.1, 1500, 11)
  st <- searchnet_stationarity_check(env)
  expect_s3_class(st, "searchnet_stationarity")
  expect_identical(st$overall, "stationary")
  expect_true(all(st$results$verdict == "stationary"))
  ## The bound is reported and finite: the minimum detectable drift is a
  ## positive number of within-run SDs, and every ESS clears the floor.
  expect_true(all(is.finite(st$results$mdd_sd) & st$results$mdd_sd > 0))
  expect_true(all(st$results$ess >= 50))
  ## Scope and popularity are rescaled density, so they share its verdict.
  r <- st$results
  expect_equal(r$drift_sd[r$statistic == "mean_K_AC"], r$drift_sd[r$statistic == "density"])
  expect_equal(nrow(st$trajectories), st$network$n_steps)
  out <- capture.output(print(st))
  expect_true(any(grepl("is a bound", out)))
  expect_true(any(grepl("overall: STATIONARY", out)))
})

test_that("a short run from an extreme start reads trending (both methods)", {
  skip_if_not_installed("RSiena")
  env <- .st_run(10, 12, 0.95, -1.5, 0, 12, 12)
  for (m in c("windows", "geweke")) {
    st <- searchnet_stationarity_check(env, method = m)
    expect_identical(st$overall, "trending", info = m)
    d <- st$results[st$results$statistic == "density", ]
    expect_identical(d$verdict, "trending", info = m)
    ## Density is falling toward its low equilibrium.
    expect_lt(d$drift, 0)
  }
})

test_that("a run whose network freezes reads absorbed, not stationary", {
  skip_if_not_installed("RSiena")
  B1 <- matrix(1, 4, 5)
  env <- .st_run(4, 5, 0, -8, 0, 60, 13, B_init = B1)
  st <- searchnet_stationarity_check(env)
  expect_identical(st$overall, "absorbed")
  expect_identical(st$results$verdict[st$results$statistic == "density"], "absorbed")
  expect_gte(st$network$share_since_last_change, 0.25)
  ## The network ended empty and stayed there.
  expect_equal(sum(env$bi_env_arr[, , dim(env$bi_env_arr)[3]]), 0)
  expect_true(any(grepl("hitting point", capture.output(print(st)))))
})

test_that("a tiny run reads too_short", {
  skip_if_not_installed("RSiena")
  env <- .st_run(4, 4, 0.4, -0.2, 0, 2, 14)
  st <- searchnet_stationarity_check(env)
  expect_identical(st$overall, "too_short")
  expect_true(all(st$results$verdict == "too_short"))
  expect_identical(searchnet_stationarity_check(env, method = "geweke")$overall,
                   "too_short")
})

test_that("the check is deterministic for a seeded run", {
  skip_if_not_installed("RSiena")
  a <- .st_run(5, 6, 0.4, -0.6, 0.1, 300, 21)
  b <- .st_run(5, 6, 0.4, -0.6, 0.1, 300, 21)
  sa <- searchnet_stationarity_check(a)
  expect_identical(sa, searchnet_stationarity_check(a))
  expect_identical(sa, searchnet_stationarity_check(b))
  expect_identical(searchnet_stationarity_check(a, method = "geweke"),
                   searchnet_stationarity_check(b, method = "geweke"))
})
