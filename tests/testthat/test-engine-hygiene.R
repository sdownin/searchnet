###############################################################################
## test-engine-hygiene.R
## Seed streams, run provenance, fit_rsiena_shocks(), the readback check, the
## opportunity table, argument-compatibility shims, and theta in K4 plots.
###############################################################################

## ---------------------------------------------------------------------------
## 1. Seed streams
## ---------------------------------------------------------------------------

test_that(".searchnet_seed is deterministic and in the valid integer range", {
  a <- .searchnet_seed(42, "ergodicity:init", 1, 2, 30)
  b <- .searchnet_seed(42, "ergodicity:init", 1, 2, 30)
  expect_identical(a, b)
  expect_type(a, "integer")
  expect_true(a >= 1L && a <= .Machine$integer.max)
  ## Large and negative bases are accepted and stay in range.
  for (base in c(-5, 0, 2^31 - 1, 2^40, 1e12)) {
    s <- .searchnet_seed(base, "x", 1)
    expect_true(!is.na(s) && s >= 1L)
  }
  expect_error(.searchnet_seed(NA, "x"), "finite")
  expect_error(.searchnet_seed(1, ""), "purpose")
})

test_that("seed streams never collide across purposes, arms, reps and lengths", {
  purposes <- c("ergodicity:init", "ergodicity:run",
                "two_sided:run", "two_sided:confirm",
                "brock_durlauf:init", "brock_durlauf:run",
                "monte_carlo:rep", "hysteresis:init", "hysteresis:run")
  grid <- expand.grid(purpose = purposes, arm = 1:4, rep = 1:12,
                      iters = c(15, 30, 60, 120, 240),
                      stringsAsFactors = FALSE)
  seeds <- mapply(function(p, a, r, i) .searchnet_seed(42, p, a, r, i),
                  grid$purpose, grid$arm, grid$rep, grid$iters)
  expect_equal(anyDuplicated(seeds), 0L)

  ## The collision the additive scheme had: arm 2's initial-draw seed equaled
  ## arm 1's dynamics seed. Reproduce it, then show the new scheme separates
  ## every (init, run) pair, including across arms.
  old_init <- function(arm, r, it) 42 + arm * 10000 + r * 100 + it
  old_run  <- function(arm, r, it) 42 + arm * 20000 + r * 100 + it
  expect_equal(old_init(2, 1, 15), old_run(1, 1, 15))
  init <- outer(1:4, 1:12, Vectorize(function(a, r) .searchnet_seed(42, "ergodicity:init", a, r, 15)))
  run  <- outer(1:4, 1:12, Vectorize(function(a, r) .searchnet_seed(42, "ergodicity:run", a, r, 15)))
  expect_length(intersect(as.vector(init), as.vector(run)), 0L)

  ## Neighboring base seeds do not share replications (seed + r did).
  reps_1 <- vapply(1:50, function(r) .searchnet_seed(1, "monte_carlo:rep", r), integer(1))
  reps_2 <- vapply(1:50, function(r) .searchnet_seed(2, "monte_carlo:rep", r), integer(1))
  expect_length(intersect(reps_1, reps_2), 0L)

  ## Index tuples of different length are different streams.
  expect_false(.searchnet_seed(7, "p", 1, 2) == .searchnet_seed(7, "p", 1, 2, 0))
})

test_that("a seeded two-sided run is reproducible under the new seed scheme", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  run_once <- function() {
    env <- saomnk_env(M = 4, N = 6, density = 0.3, seed = 42)
    mod <- saomnk_model(density = -0.5, popularity = 0.2)
    suppressMessages(
      saomnk_run_two_sided(env, mod, assent = saomnk_assent(prob = 0.5),
                           waves = 2, steps_per_actor = 3, seed = 9))
  }
  r1 <- run_once(); r2 <- run_once()
  expect_identical(r1$confirmed, r2$confirmed)
  expect_identical(r1$proposals, r2$proposals)
  prov <- searchnet_provenance(r1)
  expect_s3_class(prov, "searchnet_provenance")
  expect_equal(prov$seed, 9)
})

## ---------------------------------------------------------------------------
## 5. Run provenance
## ---------------------------------------------------------------------------

test_that("saomnk_run records provenance on the environment", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 4, N = 6, density = 0.3, seed = 42)
  expect_null(env$provenance)
  expect_null(searchnet_provenance(env))
  saomnk_run(env, saomnk_model(density = -0.5), steps_per_actor = 3, seed = 77)
  p <- searchnet_provenance(env)
  expect_s3_class(p, "searchnet_provenance")
  expect_identical(p$seed, 77L)
  expect_true(p$seed_supplied)
  expect_identical(p$rng_kind, RNGkind())
  expect_identical(p$R_version, R.version.string)
  expect_identical(p$RSiena_version, as.character(utils::packageVersion("RSiena")))
  expect_true(is.character(p$searchnet_version) && nzchar(p$searchnet_version))
  expect_match(p$call_text, "saomnk_run")
  expect_identical(p$env_seed, env$rsiena_env_seed)
  expect_output(print(p), "RNG kind")

  ## Without a seed, the defaulted seed is what is recorded.
  saomnk_run(env, saomnk_model(density = -0.5), steps_per_actor = 2)
  expect_identical(env$provenance$seed, 123L)
  expect_false(env$provenance$seed_supplied)
})

## ---------------------------------------------------------------------------
## 2. fit_rsiena_shocks end to end
## ---------------------------------------------------------------------------

test_that("fit_rsiena_shocks estimates one model per shock segment, with GOF", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  env <- saomnk_env(M = 5, N = 8, density = 0.3, seed = 42)
  mod <- saomnk_model(density = -0.5, popularity = 0.2)
  shocks <- list(saomnk_shock("density", parameter = -0.5, portion = 1),
                 saomnk_shock("density", parameter = -2.0, portion = 1))
  saomnk_run(env, mod, steps_per_actor = 20, seed = 12345, shocks = shocks)

  ## The engine's shock entries are plain lists, not print-masked
  ## saomnk_shock objects.
  expect_false(inherits(env$theta_shocks[[1]], "saomnk_shock"))

  invisible(capture.output(
    res <- env$fit_rsiena_shocks(n_obs = 4, add_gof = TRUE)))
  expect_length(res, 2L)
  for (i in seq_along(res)) {
    expect_s3_class(res[[i]]$rsiena_model, "sienaFit")
    expect_true(all(is.finite(res[[i]]$rsiena_model$theta)))
    conv <- res[[i]]$convergence
    expect_true(all(c("tconv", "tconv_max", "check_all") %in% names(conv)))
    expect_type(conv$check_all, "logical")
    expect_length(conv$check_all, 1L)
    expect_named(res[[i]]$rsiena_gof,
                 c("OutdegreeDistribution", "IndegreeDistribution"))
  }
  ## The fitted segments are written back to the environment.
  expect_identical(env$theta_shocks, res)
})

test_that("fit_rsiena_shocks fails clearly on a segment too short to estimate", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 4, N = 6, density = 0.3, seed = 42)
  saomnk_run(env, saomnk_model(density = -0.5), steps_per_actor = 5, seed = 1,
             shocks = list(saomnk_shock("density", parameter = -0.5, portion = 1),
                           saomnk_shock("density", parameter = -1, portion = 1)))
  env$theta_shocks[[1]]$chain_step_ids <- 3L
  expect_error(env$fit_rsiena_shocks(n_obs = 4, add_gof = FALSE),
               "at least 2 observations")
})

## ---------------------------------------------------------------------------
## 3. Readback check
## ---------------------------------------------------------------------------

readback_fixture <- function(n_states = 6, M = 8, N = 10, seed = 3) {
  set.seed(seed)
  stats_fun <- function(B) cbind(density = rowSums(B),
                                 inPop   = as.vector(B %*% colSums(B)),
                                 egoX    = rowSums(B) * rep(c(0, 1), length.out = nrow(B)))
  theta <- c(density = -0.6, inPop = 0.15, egoX = 0.4)
  states <- replicate(n_states,
                      matrix(stats::rbinom(M * N, 1, stats::runif(1, 0.2, 0.6)), M, N),
                      simplify = FALSE)
  list(stats_fun = stats_fun, theta = theta, states = states)
}

test_that("readback check flags a planted outcome that IS the objective", {
  fx <- readback_fixture()
  y <- unlist(lapply(fx$states, function(B) fx$stats_fun(B) %*% fx$theta))
  rb <- searchnet_readback_check(fx$states, outcome = y, theta = fx$theta,
                                 stats_fun = fx$stats_fun, n_null = 100)
  expect_s3_class(rb, "searchnet_readback")
  expect_true(rb$outcome_is_objective)
  expect_true(rb$coefficients_match)
  expect_equal(rb$r_squared, 1, tolerance = 1e-10)
  expect_equal(rb$coefficients$recovered, unname(fx$theta), tolerance = 1e-8)
  expect_equal(rb$intercept, 0, tolerance = 1e-8)
  expect_length(rb$null, 100L)
  expect_output(print(rb), "THE OUTCOME IS THE OBJECTIVE")

  ## A rescaled, shifted objective is still the objective (up to scale).
  rb2 <- searchnet_readback_check(fx$states, outcome = 3 * y + 2, theta = fx$theta,
                                  stats_fun = fx$stats_fun, n_null = 50)
  expect_true(rb2$outcome_is_objective)
  expect_false(rb2$coefficients_match)
  expect_equal(rb2$scale, 3, tolerance = 1e-8)
  expect_output(print(rb2), "up to a scale")
})

test_that("readback check does not flag an outcome independent of the objective", {
  fx <- readback_fixture()
  set.seed(11)
  y <- stats::rnorm(length(fx$states) * nrow(fx$states[[1]]))
  rb <- searchnet_readback_check(fx$states, outcome = y, theta = fx$theta,
                                 stats_fun = fx$stats_fun, n_null = 50)
  expect_false(rb$outcome_is_objective)
  expect_lt(rb$r_squared, 0.99)
  expect_output(print(rb), "not the evaluation function read back")

  ## Objective plus substantial independent noise is not a readback either.
  y2 <- unlist(lapply(fx$states, function(B) fx$stats_fun(B) %*% fx$theta)) +
    stats::rnorm(length(y), sd = 3)
  expect_false(searchnet_readback_check(fx$states, outcome = y2, theta = fx$theta,
                                        stats_fun = fx$stats_fun,
                                        n_null = 20)$outcome_is_objective)
})

test_that("readback null restores the caller's RNG state and is reproducible", {
  fx <- readback_fixture()
  y <- unlist(lapply(fx$states, function(B) fx$stats_fun(B) %*% fx$theta))
  set.seed(5); before <- .Random.seed
  a <- searchnet_readback_check(fx$states, outcome = y, theta = fx$theta,
                                stats_fun = fx$stats_fun, n_null = 30, seed = 2)
  expect_identical(.Random.seed, before)
  b <- searchnet_readback_check(fx$states, outcome = y, theta = fx$theta,
                                stats_fun = fx$stats_fun, n_null = 30, seed = 2)
  expect_identical(a$null, b$null)
})

test_that("readback check on a simulated environment flags its reported utility", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 6, N = 8, density = 0.3, seed = 42)
  saomnk_run(env, saomnk_model(density = -0.5, popularity = 0.2),
             steps_per_actor = 8, seed = 5)
  rb <- searchnet_readback_check(env, n_null = 50)
  expect_match(rb$outcome_source, "actor_util_df")
  expect_true(rb$outcome_is_objective)
  expect_equal(rb$coefficients$recovered, rb$coefficients$declared,
               tolerance = 1e-6)
  ## A user-supplied outcome unrelated to the objective is not flagged.
  set.seed(1)
  y <- stats::rnorm(rb$n_obs)
  expect_false(searchnet_readback_check(env, outcome = y,
                                        n_null = 20)$outcome_is_objective)
})

test_that("readback check validates its inputs", {
  fx <- readback_fixture()
  expect_error(searchnet_readback_check(fx$states, outcome = 1:3,
                                        theta = fx$theta, stats_fun = fx$stats_fun),
               "expected")
  expect_error(searchnet_readback_check(fx$states, theta = fx$theta,
                                        stats_fun = fx$stats_fun),
               "required")
  expect_error(searchnet_readback_check(fx$states, outcome = rep(0, 48),
                                        theta = c(foo = 1), stats_fun = fx$stats_fun),
               "no column")
})

## ---------------------------------------------------------------------------
## 4. Opportunity table
## ---------------------------------------------------------------------------

test_that("opportunity table counts ministeps and changes per group and arm", {
  chain <- data.frame(id_from   = c(1, 1, 2, 3, 1, 4, 1, 2),
                      stability = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  ot <- searchnet_opportunity_table(chain, groups = c("a", "a", "b", "b"))
  expect_s3_class(ot, "searchnet_opportunity_table")
  expect_equal(ot$group, c("a", "b"))
  expect_equal(ot$opportunities, c(6, 2))
  expect_equal(ot$changes, c(5, 1))
  expect_equal(ot$stays, c(1, 1))
  expect_equal(ot$opportunity_share, c(0.75, 0.25))
  expect_equal(ot$actor_share, c(0.5, 0.5))
  expect_equal(ot$change_rate, c(5 / 6, 1 / 2))
  expect_equal(unique(ot$arm_total_opportunities), 8)
  expect_output(print(ot), "fixed ministep budget")

  ## Two arms, same budget: shares sum to 1 within each arm (zero-sum).
  chain2 <- data.frame(id_from = c(3, 3, 4, 3, 1, 4, 3, 2),
                       stability = rep(FALSE, 8))
  ot2 <- searchnet_opportunity_table(list(control = chain, treated = chain2),
                                     groups = c("a", "a", "b", "b"))
  expect_equal(unique(ot2$arm), c("control", "treated"))
  expect_equal(as.numeric(tapply(ot2$opportunity_share, ot2$arm, sum)),
               c(1, 1))
  expect_equal(ot2$opportunities[ot2$arm == "treated"], c(2, 6))

  ## An actor who never moves still counts toward its group's size.
  ot3 <- searchnet_opportunity_table(chain[chain$id_from != 4, ],
                                     groups = c("a", "a", "b", "b"))
  expect_equal(ot3$n_actors, c(2, 2))
  expect_error(searchnet_opportunity_table(chain, groups = c("a", "b")),
               "outside")
})

test_that("opportunity table reads a simulated run's chain, bipartite DV only", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 4, N = 6, density = 0.3, seed = 42)
  saomnk_run(env, saomnk_model(density = -0.5), steps_per_actor = 5, seed = 3)
  ot <- searchnet_opportunity_table(env, groups = c("g1", "g1", "g2", "g2"))
  expect_equal(sum(ot$opportunities), nrow(env$chain_stats))
  expect_equal(sum(ot$changes), sum(env$chain_stats$tie_change))
  ## Default grouping is the actor strategies.
  ot_def <- searchnet_opportunity_table(env)
  expect_equal(sum(ot_def$n_actors), env$M)
})

## ---------------------------------------------------------------------------
## 6. API compatibility shims
## ---------------------------------------------------------------------------

test_that("saomnk_shock(new_value = ) is shimmed with a once-per-session warning", {
  .searchnet_reset_deprecations()
  expect_warning(s <- saomnk_shock("density", new_value = -1), "parameter")
  expect_identical(s, saomnk_shock("density", parameter = -1))
  expect_silent(saomnk_shock("density", new_value = -2))   # warned once only
  expect_error(saomnk_shock("density", parameter = -1, new_value = -2), "not both")
})

test_that("saomnk_shock(step = ) stops and names the construction to use", {
  expect_error(saomnk_shock("density", step = 15, new_value = -1),
               "`portion`")
  expect_error(saomnk_shock("density", step = 15, new_value = -1),
               "parameter")
  expect_error(saomnk_shock("density"), "needs `parameter`")
})

test_that("deprecated epistasis_matrix warns once per session and still works", {
  .searchnet_reset_deprecations()
  W <- saomnk_block_diagonal(8, 2)
  expect_warning(m1 <- saomnk_model(density = -0.5, epistasis_matrix = W),
                 "influence_matrix")
  expect_silent(m2 <- saomnk_model(density = -0.5, epistasis_matrix = W))
  expect_identical(m1, m2)
  expect_identical(m1, saomnk_model(density = -0.5, influence_matrix = W))
})

## ---------------------------------------------------------------------------
## 7. K4 plotting helpers read the declared coefficient, not `parm`
## ---------------------------------------------------------------------------

test_that("K4 plot titles report the declared XWX coefficient, not RSiena's parm", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  W <- saomnk_block_diagonal(6, 2)
  env <- saomnk_env(M = 4, N = 6, density = 0.3, seed = 42)
  saomnk_run(env, saomnk_model(density = -0.5, influence_matrix = W,
                               influence_weight = 0.37),
             steps_per_actor = 3, seed = 4)
  eff <- env$rsiena_effects[env$rsiena_effects$include, ]
  xwx <- eff$shortName == "XWX"
  expect_true(any(xwx))
  ## The two fields disagree, which is what makes the source matter.
  expect_equal(eff$initialValue[xwx], 0.37)
  expect_false(isTRUE(all.equal(eff$parm[xwx], 0.37)))

  params <- env$get_structure_model_params()
  expect_true(any(abs(params$covs - 0.37) < 1e-12))
  expect_match(env$get_structure_model_param_str(), "0.37", fixed = TRUE)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  p <- suppressWarnings(saomnk_plot_k4(env))
  expect_s3_class(p, "ggplot")
  ## Since 0.11.2.9000 the model weights are reported in the caption; the
  ## title states what the run shows.
  expect_match(p$labels$caption, "0.37", fixed = TRUE)
})
