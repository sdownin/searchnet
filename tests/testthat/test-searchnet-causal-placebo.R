###############################################################################
## test-searchnet-causal-placebo.R
##
## searchnet_placebo(): a real-time random-date placebo gate for the causal
## wrappers. searchnet_shock_support_check(): pre- vs post-shock structural
## support. Synthetic panels and small simulated worlds only.
###############################################################################

## Unit fixed effects, a common trend, noise; `effect` planted on treated
## units from `onset` on.
make_placebo_panel <- function(seed, effect = 0, n = 30L, n_tr = 10L,
                               onset = 6L, n_steps = 10L) {
  set.seed(seed)
  p <- expand.grid(actor_id = factor(seq_len(n)), step = seq_len(n_steps))
  tr <- as.integer(p$actor_id) <= n_tr
  p$treated     <- as.integer(tr)
  p$first_treat <- ifelse(tr, onset, 0L)
  fe <- stats::rnorm(n)[as.integer(p$actor_id)]
  p$outcome <- fe + 0.2 * p$step + stats::rnorm(nrow(p)) +
    ifelse(tr & p$step >= onset, effect, 0)
  p
}


test_that("placebo centers on zero and rejects near nominal in no-effect worlds", {
  res <- vapply(seq_len(30), function(s) {
    pl <- searchnet_placebo(make_placebo_panel(100 + s), estimator = "did_2x2",
                            n_draws = 99, seed = s)
    c(p = pl$p_value, mean = pl$placebo_mean, sd = pl$placebo_sd,
      centered = pl$gate$centered)
  }, numeric(4))
  ## Real estimate rejected at 5% in few no-effect worlds (loose bound).
  expect_lte(mean(res["p", ] <= 0.05), 0.2)
  ## Placebo means sit near zero relative to their spread.
  expect_lt(abs(mean(res["mean", ])), 0.1 * mean(res["sd", ]))
  ## The centered leg holds in (nearly) every world.
  expect_gte(mean(res["centered", ]), 0.9)
})


test_that("placebo detects a planted shock and the gate passes", {
  pl <- searchnet_placebo(make_placebo_panel(1, effect = 1.5),
                          estimator = "did_2x2", n_draws = 99, seed = 1)
  expect_s3_class(pl, "searchnet_placebo")
  expect_equal(pl$estimate, 1.5, tolerance = 0.25)
  expect_lte(pl$p_value, 0.05)
  expect_true(pl$gate$centered)
  expect_true(pl$gate$pass)
  expect_identical(pl$gate$verdict, "pass")

  ## The same world with no effect: centered, not extreme, gate fails on
  ## the "extreme" leg only.
  pl0 <- searchnet_placebo(make_placebo_panel(1, effect = 0),
                           estimator = "did_2x2", n_draws = 99, seed = 1)
  expect_true(pl0$gate$centered)
  expect_false(pl0$gate$extreme)
  expect_match(pl0$gate$verdict, "not extreme")
  expect_output(print(pl0), "real-time placebo")
})


test_that("placebo draws are deterministic under a seed and leave the global RNG alone", {
  panel <- make_placebo_panel(3)
  set.seed(99); before <- stats::runif(1); set.seed(99)
  a <- searchnet_placebo(panel, estimator = "did_2x2", n_draws = 40, seed = 7)
  after <- stats::runif(1)
  expect_identical(before, after)
  b <- searchnet_placebo(panel, estimator = "did_2x2", n_draws = 40, seed = 7)
  c <- searchnet_placebo(panel, estimator = "did_2x2", n_draws = 40, seed = 8)
  expect_identical(a$draws, b$draws)
  expect_identical(a$n_pseudo, b$n_pseudo)
  expect_false(identical(a$draws, c$draws))
})


test_that("pseudo-events follow the empirical hazard in real time", {
  ## Common timing: 10 of 30 units treated at step 6, so h_6 = 1/3 and every
  ## other step has hazard 0. Pseudo-treated counts are Binomial(20, 1/3).
  panel <- make_placebo_panel(4)
  pl <- searchnet_placebo(panel, estimator = "did_2x2", n_draws = 400, seed = 2)
  expect_equal(pl$hazard$h[pl$hazard$step == 6], 1 / 3)
  expect_true(all(pl$hazard$h[pl$hazard$step != 6] == 0))
  expect_equal(mean(pl$n_pseudo), 20 / 3, tolerance = 0.08)

  ## Staggered timing: the at-risk set at a step excludes units already
  ## treated, so h at the second onset is events / (units not yet treated).
  p2 <- panel
  p2$first_treat[as.integer(p2$actor_id) %in% 6:10] <- 8L
  pl2 <- searchnet_placebo(p2, estimator = "did_2x2", n_draws = 20, seed = 2,
                           min_valid = 0)
  expect_equal(pl2$hazard$h[pl2$hazard$step == 6], 5 / 30)
  expect_equal(pl2$hazard$h[pl2$hazard$step == 8], 5 / 25)

  ## A custom estimator is called on the real panel and every placebo panel,
  ## and placebo panels hold only never-exposed units.
  calls <- list()
  f <- function(p) {
    calls[[length(calls) + 1L]] <<- unique(as.character(p$actor_id))
    .sn_did_2x2(p)
  }
  searchnet_placebo(panel, estimator = f, n_draws = 30, seed = 1)
  expect_setequal(calls[[1]], as.character(1:30))          # the real panel
  expect_gt(length(calls), 1L)
  placebo_ids <- unique(unlist(calls[-1]))
  expect_length(intersect(placebo_ids, as.character(1:10)), 0L)
  expect_true(all(placebo_ids %in% as.character(11:30)))
})


test_that("the uniform-over-survived-periods design is refused with a reason", {
  panel <- make_placebo_panel(5)
  err <- tryCatch(searchnet_placebo(panel, estimator = "did_2x2",
                                    design = "uniform_survived"),
                  error = function(e) e)
  expect_s3_class(err, "searchnet_placebo_design_error")
  expect_match(conditionMessage(err), "conditions on the unit surviving")
  expect_match(conditionMessage(err), "real_time")
})


test_that("placebo refuses panels with no never-exposed units or no treated units", {
  panel <- make_placebo_panel(6)
  all_tr <- panel; all_tr$first_treat <- 6L
  expect_error(searchnet_placebo(all_tr, estimator = "did_2x2"),
               "never-exposed")
  none <- panel; none$first_treat <- 0L
  expect_error(searchnet_placebo(none, estimator = "did_2x2"), "no treated unit")
})


test_that("too few feasible draws give an infeasible verdict, not a reading", {
  pl <- searchnet_placebo(make_placebo_panel(7, effect = 2),
                          estimator = "did_2x2", n_draws = 10, seed = 1)
  expect_identical(pl$gate$verdict, "infeasible")
  expect_false(pl$gate$pass)
})


test_that("the did estimator path runs through searchnet_did()", {
  skip_if_not_installed("did")
  skip_on_cran()
  pl <- searchnet_placebo(make_placebo_panel(2, effect = 1.5, n = 40L),
                          estimator = "did", n_draws = 30, seed = 2)
  expect_equal(pl$estimate, 1.5, tolerance = 0.4)
  expect_gte(sum(is.finite(pl$draws)), 25)
  expect_lt(abs(pl$placebo_mean), pl$placebo_sd)
})


test_that("support check flags an impoverished panel and passes a neutral one", {
  p <- expand.grid(actor_id = factor(1:8), step = 1:10)
  p$outcome <- ifelse(p$step < 6, 4 + as.integer(p$actor_id) %% 3, 1)
  expect_warning(chk <- searchnet_shock_support_check(p, shock_step = 6),
                 class = "searchnet_impoverished_support_warning")
  expect_true(chk$impoverished)
  expect_true(all(c("distinct_values", "outcome_range", "outcome_total") %in% chk$flagged))
  expect_output(print(chk), "impoverished")

  q <- make_placebo_panel(8)
  expect_silent(chk2 <- searchnet_shock_support_check(q, shock_step = 6))
  expect_false(chk2$impoverished)
  expect_identical(chk2$window$width, 5L)
})


test_that("support check flags an impoverishing density shock in a simulated world", {
  skip_if_not_installed("RSiena")
  run_world <- function(p2) {
    env <- saomnk_env(M = 8, N = 8, seed = 42)
    mod <- saomnk_model(density = -0.5, popularity = 0.15,
                        influence_matrix = saomnk_block_diagonal(8, 2))
    s1 <- saomnk_shock("density", parameter = -0.5, portion = 1)
    s2 <- saomnk_shock("density", parameter = p2, portion = 1)
    utils::capture.output(saomnk_run(env, mod, steps_per_actor = 20, seed = 12345,
                                     shocks = list(s1, s2)))
    env
  }
  imp <- run_world(-3)
  ss <- min(imp$theta_shocks[[2]]$chain_step_ids)
  expect_warning(chk <- searchnet_shock_support_check(imp, ss),
                 class = "searchnet_impoverished_support_warning")
  expect_true(chk$impoverished)
  expect_true("ties" %in% chk$flagged)

  neu <- run_world(-0.5)
  ss2 <- min(neu$theta_shocks[[2]]$chain_step_ids)
  chk2 <- searchnet_shock_support_check(neu, ss2, warn = FALSE)
  expect_false(chk2$impoverished)
  expect_identical(chk2$source, "env")
})
