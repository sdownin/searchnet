###############################################################################
## test-silent-failures.R
## Regression tests for the silent-failure paths found by the 2026-10-07
## inventory. Each failure must now stop with an informative message, or, where
## partial results are useful, return them with a failures record and a warning
## that states counts. Never a silent NA or a quiet fallback.
###############################################################################

## Replace one method on an R6 object (methods are locked bindings).
mock_method <- function(obj, name, fun) {
  unlockBinding(name, obj)
  assign(name, fun, envir = obj)
  lockBinding(name, obj)
  invisible(obj)
}

## Temporarily bind `name` in `env`, restoring (or removing) it on exit.
## testthat::local_mocked_bindings() only reaches pkgload-loaded packages;
## this harness sources R/ into the global environment instead.
local_override <- function(name, value, env = globalenv(), frame = parent.frame()) {
  had <- exists(name, envir = env, inherits = FALSE)
  old <- if (had) get(name, envir = env, inherits = FALSE)
  assign(name, value, envir = env)
  withr::defer(if (had) assign(name, old, envir = env) else rm(list = name, envir = env),
               envir = frame)
}

## The harness attaches every imported package whole, so plyr::summarise masks
## dplyr's and grouped summaries lose their groups. The installed package is
## not affected: NAMESPACE imports summarise/summarize from dplyr only.
local_dplyr_verbs <- function(frame = parent.frame()) {
  local_override("summarise", dplyr::summarise, frame = frame)
  local_override("summarize", dplyr::summarize, frame = frame)
}

new_bare_env <- function() {
  SaomNkRSienaBiEnv$new(make_small_environ_params())
}

## Exploration-style metrics panel: actors 1-2 treated ("100"), 3-4 control ("0").
fake_metrics <- function(steps = 1:6, strategies = c("100", "100", "0", "0"), seed = 1) {
  set.seed(seed)
  d <- expand.grid(chain_step_id = steps, actor_id = seq_along(strategies))
  d$strategy <- strategies[d$actor_id]
  d$exploration <- runif(nrow(d))
  d$risk_taking_score <- runif(nrow(d))
  d$n_new_activities <- rpois(nrow(d), 2)
  d
}

## ---- saomnk-base.R: a declared effect that cannot be included -------------

test_that("a covariate effect declared without its covariate stops the run", {
  skip_if_not_installed("RSiena")
  env <- new_bare_env()
  sm <- make_minimal_structure_model()
  sm$dv_bipartite$effects[[2]] <- list(effect = "egoX", parameter = 0.5,
                                       dv_name = DV_NAME, fix = TRUE)
  expect_error(
    env$search_rsiena(structure_model = sm, iterations_per_actor = 2,
                      run_seed = 1, verbose = FALSE),
    "'egoX' effect cannot be included.*WITHOUT this effect")
})

test_that("an XWX effect on an unregistered covariate stops the run", {
  skip_if_not_installed("RSiena")
  env <- new_bare_env()
  sm <- make_minimal_structure_model()
  sm$dv_bipartite$effects[[2]] <- list(effect = "XWX", parameter = 0.5,
                                       dv_name = DV_NAME, fix = TRUE,
                                       interaction1 = "no_such_covariate")
  expect_error(
    env$search_rsiena(structure_model = sm, iterations_per_actor = 2,
                      run_seed = 1, verbose = FALSE),
    "XWX effect failed")
})

test_that("saomnk.skip_missing_effects restores skipping, with a warning", {
  skip_if_not_installed("RSiena")
  op <- options(saomnk.skip_missing_effects = TRUE)
  on.exit(options(op))
  env <- new_bare_env()
  sm <- make_minimal_structure_model()
  sm$dv_bipartite$effects[[2]] <- list(effect = "egoX", parameter = 0.5,
                                       dv_name = DV_NAME, fix = TRUE)
  expect_warning(
    env$search_rsiena(structure_model = sm, iterations_per_actor = 2,
                      run_seed = 1, verbose = FALSE),
    "egoX.*skipping")
})

## ---- saomnk-experiments.R: failed runs are recorded, not dropped ----------

test_that("the experiments runner records failed runs and warns with counts", {
  ctr <- new.env()
  ctr$n <- 0L
  ctr$fail_on <- 1L
  FakeEnv <- R6::R6Class("FakeEnv", public = list(
    actor_util_df = NULL, bi_env_arr = NULL, rsiena_run_seed = NULL,
    initialize = function(params) invisible(self),
    search_rsiena = function(..., run_seed) {
      ctr$n <- ctr$n + 1L
      if (ctr$n %in% ctr$fail_on) stop("planted run failure")
      self$rsiena_run_seed <- run_seed
      self$actor_util_df <- data.frame(x = 1)
      invisible(self)
    }))
  exp <- SaoMNKexperiments$new(name = "f", n = 3,
                               environ_params = make_small_environ_params(),
                               structure_model = make_minimal_structure_model(),
                               steps_per_actor = 2, rand_seed = 1)
  capture.output(
    expect_warning(exp$run_simulations(SaoMNK_class = FakeEnv),
                   "1 of 3 simulation runs failed.*planted run failure"))
  expect_equal(nrow(exp$failures), 1L)
  expect_equal(exp$failures$stage, "search_rsiena")
  expect_length(exp$simulation_results, 2L)

  ctr$n <- 0L
  ctr$fail_on <- 1:3
  capture.output(
    expect_error(exp$run_simulations(SaoMNK_class = FakeEnv),
                 "All 3 simulation runs failed.*planted run failure"))
})

## ---- ising_hysteresis_sweep(): no starting matrix passed off as a result ---

test_that("ising_hysteresis_sweep stops when every replicate at a step fails", {
  skip_if_not_installed("RSiena")
  env <- new_bare_env()
  sm <- make_minimal_structure_model()
  sm$dv_bipartite$effects[[2]] <- list(effect = "noSuchEffect", parameter = 1,
                                       dv_name = DV_NAME, fix = TRUE)
  ## The old code returned the starting matrix for each failed replicate and
  ## completed the sweep with a full-looking, entirely unsimulated result.
  expect_error(
    env$ising_hysteresis_sweep(n_steps = 2, n_reps_per_step = 1,
                               structure_model = sm, iterations_per_actor = 1),
    "all 1 replicate\\(s\\) failed.*noSuchEffect")
})

test_that("a clean hysteresis sweep reports zero failures and full n_ok", {
  skip_if_not_installed("RSiena")
  env <- new_bare_env()
  res <- env$ising_hysteresis_sweep(n_steps = 2, n_reps_per_step = 1,
                                    structure_model = make_minimal_structure_model(),
                                    iterations_per_actor = 2)
  expect_equal(nrow(res$failures), 0L)
  expect_true(all(res$forward_path$n_ok == 1L))
  expect_true(all(res$reverse_path$n_ok == 1L))
})

## ---- test_shocks_new_components(): no quiet before/after fallback ---------

fake_K_new <- function(strategies = c("100", "100", "0", "0")) {
  d <- fake_metrics(strategies = strategies)
  data.frame(chain_step_id = d$chain_step_id, actor_id = d$actor_id,
             strategy = d$strategy, value = d$exploration,
             treatment_group = ifelse(d$strategy == "0", 0, 4),
             new_components = "5", stringsAsFactors = FALSE)
}

test_that("test_shocks_new_components stops when the DiD estimation fails", {
  skip_if_not_installed("did")
  local_dplyr_verbs()
  env <- new_bare_env()
  mock_method(env, "compute_K_attribute_shocks", function(...) fake_K_new())
  local_mocked_bindings(att_gt = function(...) stop("planted att_gt failure"),
                        .package = "did")
  expect_error(env$test_shocks_new_components(),
               "none of 1 treatment/control pair.*planted att_gt failure")
})

test_that("test_shocks_new_components records a failed pair and warns with counts", {
  skip_if_not_installed("did")
  local_dplyr_verbs()
  env <- new_bare_env()
  mock_method(env, "compute_K_attribute_shocks",
              function(...) fake_K_new(c("100", "100", "50", "50", "0", "0")))
  local_mocked_bindings(
    att_gt = function(..., data) {
      if ("50" %in% data$strategy) stop("planted att_gt failure")
      structure(list(), class = "MP")
    },
    aggte = function(...) list(overall.att = 1),
    .package = "did")
  expect_warning(res <- env$test_shocks_new_components(),
                 "1 of 2 treatment/control pair.*0 skipped, 1 failed")
  expect_length(res, 1L)
  expect_equal(attr(res, "failures")$stage, "did_failed")
})

## ---- exploration analyses ---------------------------------------------------

test_that("check_exploration_data_availability returns and warns a metrics failure", {
  env <- new_bare_env()
  mock_method(env, "get_K4_df", function(...) data.frame(chain_step_id = 1:3))
  mock_method(env, "calculate_explore_exploit_risk_adjusted",
              function(...) stop("planted metrics failure"))
  capture.output(
    expect_warning(res <- env$check_exploration_data_availability(),
                   "planted metrics failure"))
  expect_false(res$metrics_ok)
  expect_match(res$metrics_error, "planted metrics failure")
})

exploration_env <- function(metrics) {
  env <- new_bare_env()
  mock_method(env, "calculate_explore_exploit_risk_adjusted", function(...) metrics)
  if ("calculate_social_logic_influence" %in% names(env))
    mock_method(env, "calculate_social_logic_influence", function(m) m)
  mock_method(env, "plot_exploration_risk_multiperiod", function(...) NULL)
  mock_method(env, "plot_exploration_risk_did", function(...) NULL)
  mock_method(env, "test_multiperiod_exploration_risk", function(...) list(plot = NULL))
  env
}

test_that("analyze_exploration_risk_shocks stops when there is no pre-shock data", {
  env <- exploration_env(fake_metrics())
  ## Used to print an ERROR banner and return a list without estimates.
  expect_error(env$analyze_exploration_risk_shocks(shock_time = 1),
               "no pre-shock data")
})

test_that("analyze_exploration_risk_shocks stops when the treated arm is missing", {
  env <- exploration_env(fake_metrics(strategies = c("0", "0", "0", "0")))
  ## Used to print "Unable to calculate DiD estimates" and return NA.
  capture.output(
    expect_error(env$analyze_exploration_risk_shocks(shock_time = 4),
                 "found 0 treated and 1 control"))
})

test_that("analyze_exploration_risk_shocks stops when the social-logic step fails", {
  env <- exploration_env(fake_metrics())
  skip_if_not("calculate_social_logic_influence" %in% names(env))
  mock_method(env, "calculate_social_logic_influence",
              function(m) stop("planted social-logic failure"))
  expect_error(env$analyze_exploration_risk_shocks(shock_time = 4),
               "planted social-logic failure")
})

test_that("analyze_exploration_risk_shocks records failed auxiliary parts with counts", {
  env <- exploration_env(fake_metrics())
  mock_method(env, "plot_exploration_risk_multiperiod",
              function(...) stop("planted plot failure"))
  mock_method(env, "plot_exploration_risk_did",
              function(...) stop("planted plot failure"))
  capture.output(
    expect_warning(res <- env$analyze_exploration_risk_shocks(shock_time = 4),
                   "2 of 3 auxiliary components failed"))
  expect_equal(nrow(res$failures), 2L)
  expect_null(res$plots$trajectories)
  expect_true(is.finite(res$did_estimates$risk_taking))
})

test_that("analyze_simple_exploration_shocks_fixed stops instead of printing DiD = 0", {
  local_dplyr_verbs()
  env <- exploration_env(fake_metrics(strategies = c("0", "0", "0", "0")))
  expect_error(env$analyze_simple_exploration_shocks_fixed(shock_time = 4),
               "cannot be computed; no data in Treated/Pre, Treated/Post")
})

test_that("analyze_simple_exploration_shocks stops instead of printing DiD = 0", {
  env <- new_bare_env()
  mock_method(env, "calculate_simple_exploration_metrics",
              function(...) fake_metrics(strategies = c("0", "0", "0", "0")))
  expect_error(env$analyze_simple_exploration_shocks(shock_time = 4),
               "DiD for 'n_new_activities' cannot be computed")
})

test_that("a failed did-package estimate is returned and warned, not cat()", {
  skip_if_not_installed("did")
  local_dplyr_verbs()
  env <- exploration_env(fake_metrics())
  local_mocked_bindings(att_gt = function(...) stop("planted att_gt failure"),
                        .package = "did")
  capture.output(
    expect_warning(res <- env$analyze_simple_exploration_shocks_fixed(shock_time = 4),
                   "'did' package estimate failed.*planted att_gt failure"))
  expect_null(res$did_results)
  expect_match(res$did_error, "planted att_gt failure")
  expect_true(is.finite(res$did_estimate))
})

test_that("diagnose_did_detailed returns its att_gt test outcomes and warns on failure", {
  skip_if_not_installed("did")
  local_dplyr_verbs()
  env <- new_bare_env()
  m <- fake_metrics()
  k4 <- data.frame(effect = "K_AC", chain_step_id = m$chain_step_id,
                   actor_id = m$actor_id, strategy = m$strategy,
                   value = m$exploration, stringsAsFactors = FALSE)
  mock_method(env, "compute_K_shocks", function(...) NULL)
  mock_method(env, "compute_exploration_shocks", function(...) NULL)
  mock_method(env, "get_K4_df", function(...) k4)
  mock_method(env, "calculate_explore_exploit_risk_adjusted", function(...) m)
  env$theta_shocks <- list(list(chain_step_ids = 1:3, shock_on = 0),
                           list(chain_step_ids = 4:6, shock_on = 1))
  local_mocked_bindings(att_gt = function(...) stop("planted att_gt failure"),
                        .package = "did")
  capture.output(
    expect_warning(res <- env$diagnose_did_detailed(),
                   "2 of 2 att_gt\\(\\) tests failed"))
  expect_false(any(res$att_gt_tests$ok))
})

## ---- did_shock_analysis(): parallel-trends p-value never silently NA -------

test_that("did_shock_analysis says why the parallel-trends test was not computed", {
  env <- new_bare_env()
  set.seed(2)
  env$actor_util_df <- data.frame(actor_id = rep(1:4, each = 6),
                                  chain_step_id = rep(1:6, 4),
                                  utility = rnorm(24))
  expect_warning(res <- env$did_shock_analysis(treatment_step = 3, plot = FALSE),
                 "parallel-trends test not computed: 8 pre-treatment observation")
  expect_true(is.na(res$parallel_trends_p))
  expect_match(res$parallel_trends_status, "^not computed")
})

## ---- verify_brock_durlauf_reduction(): failed replicates counted -----------

test_that("verify_brock_durlauf_reduction counts failed replicates and stops if all fail", {
  skip_if_not_installed("RSiena")
  real_run <- saomnk_run
  ctr <- new.env()
  ctr$n <- 0L
  local_override("saomnk_run", function(...) {
    ctr$n <- ctr$n + 1L
    if (ctr$n == 1L) stop("planted replicate failure")
    real_run(...)
  }, env = environment(verify_brock_durlauf_reduction))
  expect_warning(
    out <- verify_brock_durlauf_reduction(M_seq = 4, n_components = 4,
                                          n_steps = 2, n_replicates = 2, seed = 1),
    "1 of 2 simulation replicates failed.*planted replicate failure")
  expect_equal(out$n_ok, 1L)
  expect_equal(nrow(attr(out, "failures")), 1L)
  expect_true(is.finite(out$m_b_empirical))
})

test_that("verify_brock_durlauf_reduction stops when every replicate fails", {
  local_override("saomnk_run", function(...) stop("planted replicate failure"),
                 env = environment(verify_brock_durlauf_reduction))
  expect_error(verify_brock_durlauf_reduction(M_seq = 4, n_components = 4,
                                              n_steps = 2, n_replicates = 2, seed = 1),
               "all 2 simulation replicates failed")
})

## ---- bridge and diagnostics: NA standard errors and summaries ------------

test_that("a sienaFit without a readable covtheta warns that its SEs are NA", {
  fit <- structure(list(
    theta = c(1, -0.5),
    covtheta = NULL,
    effects = data.frame(effectName = c("outdegree (density)", "transitive triplets"),
                         shortName = c("density", "transTrip"),
                         type = c("eval", "eval"), stringsAsFactors = FALSE)),
    class = "sienaFit")
  expect_warning(x <- .extract_saom_theta(fit),
                 "standard errors are NA for all 2 parameters: the sienaFit carries no covtheta")
  expect_true(all(is.na(x$se)))

  fit$covtheta <- diag(3)
  expect_warning(.extract_saom_theta(fit), "3 diagonal entries but there are 2 estimates")

  fit$covtheta <- diag(2) * 4
  expect_silent(x <- .extract_saom_theta(fit))
  expect_equal(unname(x$se), c(2, 2))
})

test_that("the bridge's paired summary refuses missing replications", {
  ## A missing value used to turn every inferential column NA without a word.
  expect_error(.bridge_delta_summary("K_AC", c(1, NA, 2), c(2, 3, 4), 0.95),
               "missing in 1 of 3 replications")
  s <- .bridge_delta_summary("K_AC", c(1, 2, 3), c(2, 4, 5), 0.95)
  expect_true(is.finite(s$p_value))
})

test_that("saomnk_extract_estimates_tergm warns when SEs cannot be extracted", {
  fit <- structure(list(coefficients = c(edges = -1, mutual = 0.5)), class = "fakefit")
  expect_warning(out <- saomnk_extract_estimates_tergm(fit, "spec1"),
                 "standard errors could not be extracted for specification 'spec1'")
  expect_equal(out$estimate, c(-1, 0.5))
  expect_true(all(is.na(out$std_error)))
})

## ---- test helper: errors fail, they do not skip ---------------------------

test_that("run_tiny_sim lets an init error through instead of skipping", {
  skip_if_not_installed("RSiena")
  ## Under the old helper this test was reported as SKIPPED.
  local_override("SaomNkRSienaBiEnv",
                 list(new = function(...) stop("planted init failure")),
                 env = environment(run_tiny_sim))
  expect_error(run_tiny_sim(), "planted init failure")
})

## ---- classroom leaderboard: the epistasis bonus reads the real model -----

test_that("the classroom leaderboard's epistasis bonus uses the model's W and weight", {
  skip_if_not_installed("RSiena")
  cls <- suppressMessages(searchnet_classroom_init(n_students = 2, n_rounds = 2,
                                                   N = 6, seed = 3))
  cls <- suppressMessages(searchnet_classroom_advance(cls, force = TRUE))
  B <- cls$env$bipartite_matrix
  xwx <- Filter(function(e) identical(e$effect, "XWX"),
                cls$model$dv_bipartite$coDyadCovars)[[1]]
  W <- as.matrix(xwx$x)
  pop <- colSums(B)
  bonus <- vapply(seq_len(nrow(B)), function(i) {
    h <- which(B[i, ] == 1)
    if (length(h)) sum(W[h, h]) - sum(diag(W)[h]) else 0
  }, numeric(1))
  expected <- vapply(seq_len(nrow(B)), function(i) {
    h <- which(B[i, ] == 1)
    if (!length(h)) return(0)
    round(length(h) * 0.5 + bonus[i] * xwx$parameter - sum(pop[h] - 1) * 0.1, 3)
  }, numeric(1))
  ## The bonus was identically 0 (W fell back to the identity); the check
  ## below is vacuous unless some firm actually earns one.
  expect_true(any(bonus > 0))
  expect_equal(cls$round_history[[length(cls$round_history)]]$utility, expected)
})
