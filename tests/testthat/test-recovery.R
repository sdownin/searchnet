###############################################################################
## test-recovery.R
## searchnet_recovery(): planted-truth recovery and power harness.
## Pure-logic tests run everywhere; anything that calls RSiena is skipped on
## CRAN and uses tiny settings (M = 8, N = 6, 2-3 reps, short phase 3).
###############################################################################

tiny_recovery <- function(...) {
  args <- utils::modifyList(list(
    effects_spec = c("density", "inPop"),
    theta_true = c(rate = 3, density = -1.2, inPop = 0.15),
    reps = 2L, M = 8L, N = 6L, waves = 3L, n3 = 60L, nsub = 1L, seed = 7L),
    list(...))
  do.call(searchnet_recovery, args)
}

## ---- pure logic ---------------------------------------------------------------

test_that("effect spec parsing adds density and builds labels", {
  s <- .recovery_parse_spec(c("inPop", "inPop:endow"))
  expect_equal(s$label, c("density", "inPop", "inPop:endow"))
  expect_equal(s$type, c("eval", "eval", "endow"))
  d <- .recovery_parse_spec(data.frame(shortName = c("density", "egoX"),
                                       interaction1 = c("", "x")))
  expect_equal(d$label, c("density", "egoX(x)"))
  expect_error(.recovery_parse_spec(c("inPop:sideways")), "not recognized")
  expect_error(.recovery_parse_spec(c("inPop", "inPop")), "twice")
})

test_that("argument validation names the problem", {
  expect_error(searchnet_recovery("inPop", c(rate = 3, density = -1), M = 8, N = 6),
               "lacks planted value")
  expect_error(searchnet_recovery("inPop", c(rate = 3, density = -1, inPop = 0,
                                            outAct = 1), M = 8, N = 6),
               "not in `effects_spec`")
  expect_error(searchnet_recovery("inPop", c(rate = 3, density = -1, inPop = 0)),
               "must be given")
  expect_error(searchnet_recovery("inPop", c(rate = 3, density = -1, inPop = 0),
                                  M = 8, N = 6, null = TRUE), "exactly one")
  expect_error(searchnet_recovery("inPop", c(rate = 3, density = -1, inPop = 0),
                                  M = 8, N = 6, algorithm_args = list(seed = 3)),
               "may not set")
})

test_that("MDE follows (z_{1-alpha/2} + z_power) * mean SE", {
  expect_equal(.recovery_mde(0.1, 0.05, 0.80),
               (qnorm(0.975) + qnorm(0.80)) * 0.1)
  est <- data.frame(effect = "a", estimate = c(0.9, 1.1, 1.3, 0.2),
                    se = c(0.2, 0.3, 0.25, 0.25), stringsAsFactors = FALSE)
  tab <- .recovery_table(est, c(a = 1), alpha = 0.05, power = 0.9,
                         ci_level = 0.95)
  expect_equal(tab$mean_se, 0.25)
  expect_equal(tab$mde, (qnorm(0.975) + qnorm(0.9)) * 0.25)
  expect_equal(tab$bias, mean(est$estimate) - 1)
  expect_equal(tab$rmse, sqrt(mean((est$estimate - 1)^2)))
  ## |0.2 - 1| = 0.8 > 1.96 * 0.25, the other three cover
  expect_equal(tab$coverage, 0.75)
  expect_equal(tab$mcse_coverage, sqrt(0.75 * 0.25 / 4))
  expect_equal(tab$rejection, mean(abs(est$estimate / est$se) > qnorm(0.975)))
  expect_equal(tab$mcse_bias, sd(est$estimate) / 2)
})

test_that("an effect with no converged replication gets NA statistics", {
  est <- data.frame(effect = character(0), estimate = numeric(0),
                    se = numeric(0), stringsAsFactors = FALSE)
  tab <- .recovery_table(est, c(a = 1), 0.05, 0.8, 0.95)
  expect_equal(tab$n_converged, 0L)
  expect_true(is.na(tab$bias) && is.na(tab$mde))
})

test_that("verdict needs an explicit criterion and applies each check", {
  tab <- data.frame(effect = c("rate (period 1)", "density", "inPop", "outAct"),
                    true = c(3, -1, 0.2, 0),
                    bias = c(0.5, 0.01, -0.04, 0.001),
                    coverage = c(0.8, 0.95, 0.93, 0.96),
                    rmse = c(1, 0.1, 0.05, 0.02),
                    rejection = c(1, 1, 0.7, 0.12),
                    stringsAsFactors = FALSE)
  v0 <- .recovery_verdict(tab, NULL, c("density", "inPop"), 10, 10)
  expect_identical(v0$verdict, "not assessed")
  v1 <- .recovery_verdict(tab, list(max_abs_bias = 0.05, min_coverage = 0.90),
                          c("density", "inPop"), 10, 10)
  expect_identical(v1$verdict, "PASS")
  expect_equal(nrow(v1$checks), 4L)
  ## rate rows are not assessed unless named in focal
  expect_false(any(grepl("rate", v1$checks$effect)))
  v2 <- .recovery_verdict(tab, list(max_abs_bias = 0.03), c("density", "inPop"),
                          10, 10)
  expect_identical(v2$verdict, "FAIL")
  expect_false(v2$checks$pass[v2$checks$effect == "inPop"])
  ## named per-effect thresholds
  v3 <- .recovery_verdict(tab, list(max_abs_bias = c(inPop = 0.05)),
                          c("density", "inPop"), 10, 10)
  expect_identical(v3$verdict, "PASS")
  expect_equal(v3$checks$effect, "inPop")
  ## size check applies only to effects planted at zero
  v4 <- .recovery_verdict(tab, list(max_size = 0.10), c("inPop", "outAct"), 10, 10)
  expect_identical(v4$verdict, "FAIL")
  expect_equal(v4$checks$effect, "outAct")
  ## converged share
  v5 <- .recovery_verdict(tab, list(min_converged_share = 0.9), "density", 8, 10)
  expect_identical(v5$verdict, "FAIL")
  expect_error(.recovery_verdict(tab, list(max_bias = 1), "density", 10, 10),
               "unknown criterion")
  ## an effect without converged replications fails rather than passing
  tab$bias[3] <- NA
  v6 <- .recovery_verdict(tab, list(max_abs_bias = 1), "inPop", 0, 10)
  expect_identical(v6$verdict, "FAIL")
})

## ---- RSiena-backed (slow) -------------------------------------------------------

test_that("harness returns the documented structure", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  rec <- tiny_recovery()
  expect_s3_class(rec, "searchnet_recovery")
  expect_equal(rec$table$effect,
               c("rate (period 1)", "rate (period 2)", "density", "inPop"))
  expect_true(all(c("true", "mean_est", "bias", "rmse", "coverage",
                    "rejection", "mde", "mcse_bias", "mcse_coverage",
                    "n_converged") %in% names(rec$table)))
  expect_equal(nrow(rec$reps), 2L)
  expect_equal(sum(rec$status_counts), 2L)
  expect_identical(rec$verdict, "not assessed")
  expect_equal(rec$table$true, c(3, 3, -1.2, 0.15))
  out <- capture.output(print(rec))
  expect_true(any(grepl("Nothing more", out)))
  expect_true(any(grepl("verdict: not assessed", out)))
  out2 <- capture.output(print(summary(rec)))
  expect_true(any(grepl("none dropped silently", out2)))
  ## the MDE in the object is the documented formula on the converged SEs
  conv <- rec$estimates[rec$estimates$status == "converged", ]
  if (nrow(conv)) {
    se_in <- mean(conv$se[conv$effect == "inPop"])
    expect_equal(rec$table$mde[rec$table$effect == "inPop"],
                 (qnorm(0.975) + qnorm(0.8)) * se_in)
  }
})

test_that("same seed gives identical results; a different seed does not", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  a <- tiny_recovery()
  b <- tiny_recovery()
  expect_identical(a$table, b$table)
  expect_identical(a$estimates, b$estimates)
  expect_identical(a$reps, b$reps)
  c2 <- tiny_recovery(seed = 8L)
  expect_false(identical(a$estimates$estimate, c2$estimates$estimate))
})

test_that("non-converged replications are counted separately, never dropped", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  ## A threshold no fit can meet makes every replication non-converged.
  rec <- tiny_recovery(conv_threshold = 1e-12,
                       criterion = list(max_abs_bias = 10))
  expect_equal(rec$n_converged, 0L)
  expect_equal(unname(rec$status_counts["converged"]), 0L)
  expect_equal(sum(rec$status_counts), 2L)
  expect_true(all(rec$reps$status %in% c("not_converged", "non_identified")))
  ## their estimates are kept for inspection but excluded from summaries
  expect_gt(nrow(rec$estimates), 0L)
  expect_true(all(rec$table$n_converged == 0L))
  expect_true(all(is.na(rec$table$bias)))
  expect_identical(rec$verdict, "FAIL")
})

test_that("null mode plants zero on the focal effect", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  rec <- tiny_recovery(focal = "inPop", null = TRUE,
                       criterion = list(max_size = 1))
  expect_equal(rec$theta_true[["inPop"]], 0)
  expect_true(rec$settings$null)
  expect_equal(rec$assessed, "inPop")
  expect_equal(rec$table$true[rec$table$effect == "inPop"], 0)
  if (rec$n_converged > 0L) {
    expect_identical(rec$verdict, "PASS")
    expect_equal(rec$checks$test, "size (rejection | theta = 0) <=")
  }
})

test_that("an effect RSiena does not offer is reported, not silently dropped", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  expect_error(
    tiny_recovery(effects_spec = c("density", "transTrip"),
                  theta_true = c(rate = 3, density = -1, transTrip = 0.1)),
    "non-implementation")
})
