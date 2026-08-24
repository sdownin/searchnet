###############################################################################
## test-bridge-counterfactual.R
## Unit tests for the revised empirical bridge in R/searchnet-bridge.R
##
## The bridge is the seam between an estimated SAOM and a simulated
## counterfactual, so the things that must not be wrong here are the ones that
## would silently change a reported number rather than crash a run:
##
##   * `.draw_saom_theta()` turns an estimated covariance matrix into parameter
##     uncertainty. If the eigen-decomposition were wrong the draws would still
##     look like draws -- centered, plausible, reproducible -- while carrying the
##     wrong dispersion. So it is checked against the DEFINITION: over many
##     draws the empirical first and second moments must recover theta and
##     covtheta. Its five refusals are checked one at a time, each by its own
##     message, because a guard that fires with the wrong message sends the
##     analyst to the wrong problem.
##   * `.bridge_delta_summary()` produces the Monte Carlo error, the paired
##     t-test and the quantile interval that get reported. Every quantity is
##     re-derived by hand from a five-element vector AND cross-checked against
##     `stats::t.test()` / `stats::quantile()`.
##   * `.is_rate_effect()` decides what silently does NOT reach the simulation.
##     A false positive drops an estimated evaluation effect; a false negative
##     feeds a rate parameter into the utility crosswalk. Both are tested with
##     adversarial names.
##
## Nothing in this file estimates an RSiena model and nothing runs a
## simulation. `saom_to_saomnk()` is exercised against a MOCK `sienaFit` (a
## plain list carrying only the fields the function reads), and the parts of
## `run_calibrated_counterfactual()` / `empirical_to_saomnk_env()` that need the
## engine are covered only up to the guards that fire before the first
## environment is constructed; the rest is skipped with an explicit reason
## rather than faked.
##
## Three CODE defects are documented below as deliberate failing tests --
## sections 4.6 (coef() on a sienaFit), 4.7 (theta named by effectName) and
## 3.4 (name heuristic overriding metadata). None of them has been weakened to
## pass.
###############################################################################

## helper-setup.R sources every file in R/ via inst/saomnk-loader.R. Re-source
## the one file under test if that did not reach it, so a failure here is a
## failure of the code and not of the harness.
if (!exists("saom_to_saomnk", mode = "function") &&
    exists("dir_r") &&
    file.exists(file.path(dir_r, "searchnet-bridge.R"))) {
  source(file.path(dir_r, "searchnet-bridge.R"), local = FALSE)
}


## ---------------------------------------------------------------------------
## Mock sienaFit fixtures.
##
## A real `sienaFit` from siena07() carries $theta, $covtheta, $se and an
## $effects data.frame (see ?siena07, section Value). `make_siena_fit_real()`
## builds exactly that shape and is used only by the two known-failure tests in
## section 4 that show the bridge cannot read it.
##
## `make_siena_fit_workable()` is the same object plus two DELIBERATE
## deviations from RSiena, each of which exists to route around one of those
## defects so that the rest of the bridge can be tested at all:
##
##   $coefficients  -- `.extract_saom_theta()` calls `coef()`, and there is no
##                     `coef.sienaFit` method in RSiena or in this package, so
##                     `coef.default()` reads `$coefficients`, which a real fit
##                     does not have. See test 4.6.
##   effectName set to the RSiena shortName, not the real long effectName --
##                     `.extract_saom_theta()` names theta from `effectName`
##                     ("outdegree (density)") while the crosswalk inside
##                     `saom_to_saomnk()` matches shortNames ("density"). See
##                     test 4.7.
##
## Both deviations are noted at every use. They are scaffolding around known
## bugs, not a claim about what RSiena returns.
## ---------------------------------------------------------------------------

## NOTE on the third effect. The fixture used `transTrip` until the crosswalk
## learned to refuse mappings whose SaoMNK target does not exist for a bipartite
## dependent variable. `transTrip -> transTriads` is exactly such a mapping
## (transTriads lives in RSiena's symmetricObjective group only), so every test
## that merely needs "a second evaluation effect" now uses `inPop`, which maps
## exactly. The refusal itself is tested on purpose in section 4.9.
make_siena_fit_real <- function(theta = c(4.2, -1.7, 0.9),
                                effect_name = c("basic rate parameter net",
                                                "outdegree (density)",
                                                "indegree - popularity"),
                                short_name = c("Rate", "density", "inPop"),
                                type = c("rate", "eval", "eval"),
                                covtheta = diag(c(0.50, 0.04, 0.09))) {
  ## The effects frame has one row per CANDIDATE effect, which need not equal
  ## length(theta) -- that is exactly the misalignment the bridge has to cope
  ## with, so the fixture must be able to express it.
  n_row <- length(effect_name)
  fit <- list(
    theta    = theta,
    covtheta = covtheta,
    se       = sqrt(diag(covtheta)),
    effects  = data.frame(
      effectName   = effect_name,
      shortName    = short_name,
      type         = type,
      period       = ifelse(type == "rate", "1", NA_character_),
      interaction1 = rep("", n_row),
      include      = rep(TRUE, n_row),
      stringsAsFactors = FALSE
    )
  )
  class(fit) <- "sienaFit"
  fit
}

## The workable mock: shortName duplicated into effectName, plus $coefficients.
make_siena_fit_workable <- function(theta = c(4.2, -1.7, 0.9),
                                    short_name = c("Rate", "density", "inPop"),
                                    type = c("rate", "eval", "eval"),
                                    covtheta = diag(c(0.50, 0.04, 0.09)),
                                    include = NULL) {
  fit <- make_siena_fit_real(theta = theta, effect_name = short_name,
                             short_name = short_name, type = type,
                             covtheta = covtheta)
  fit$coefficients <- theta          # workaround for the coef() defect
  if (!is.null(include)) fit$effects$include <- include
  fit
}

## A K-4 summary of the shape `.extract_k4_summary()` returns, but built by
## hand so the deltas below are arithmetic rather than simulation output.
make_k4 <- function(K_AC, K_CA, K_AA, K_CC) {
  out <- list(K_AC = K_AC, K_CA = K_CA, K_AA = K_AA, K_CC = K_CC)
  for (m in c("K_AC", "K_CA", "K_AA", "K_CC")) {
    out[[paste0("mean_", m)]] <- mean(out[[m]])
    out[[paste0("sd_",   m)]] <- stats::sd(out[[m]])
  }
  out
}


# ===========================================================================
# 1. .draw_saom_theta(): the parameter-uncertainty draw
#
# The highest-value block in the file. Everything downstream that claims to
# propagate estimation error passes through this one function.
# ===========================================================================

## A genuinely non-diagonal, positive-definite covariance. Non-diagonal
## matters: a bug that used only diag(covtheta) would pass every diagonal test.
PD_SIGMA <- matrix(c( 0.25,  0.10, -0.05,
                      0.10,  0.16,  0.02,
                     -0.05,  0.02,  0.09), 3, 3,
                   dimnames = list(NULL, NULL))
PD_THETA <- c(density = -1.50, gwespFF = 0.80, inPop = 0.25)


test_that("the fixture covariance really is positive definite", {
  ## Not a test of the code -- a test of the fixture, so that a failure in the
  ## distributional test below cannot be blamed on a bad Sigma.
  expect_true(min(eigen(PD_SIGMA, symmetric = TRUE)$values) > 1e-6)
  expect_equal(PD_SIGMA, t(PD_SIGMA))
})


# --- 1.1 reproducibility ----------------------------------------------------

test_that("the same draw_seed gives a bit-identical draw", {
  a <- .draw_saom_theta(PD_THETA, PD_SIGMA, draw_seed = 20260823L)
  b <- .draw_saom_theta(PD_THETA, PD_SIGMA, draw_seed = 20260823L)
  expect_identical(a, b)
  ## and the names of theta survive the draw, since the crosswalk downstream
  ## matches on them
  expect_identical(names(a), names(PD_THETA))
  expect_length(a, 3L)
})

test_that("different draw_seeds give different draws", {
  a <- .draw_saom_theta(PD_THETA, PD_SIGMA, draw_seed = 1L)
  b <- .draw_saom_theta(PD_THETA, PD_SIGMA, draw_seed = 2L)
  expect_false(isTRUE(all.equal(unname(a), unname(b))))
  ## Every coordinate should move; a draw that only perturbed the first
  ## parameter would still pass a bare inequality.
  expect_true(all(abs(a - b) > 0))
})

test_that("draw_seed = NULL consumes the ambient stream, so an outer seed fixes it", {
  set.seed(4242)
  a <- .draw_saom_theta(PD_THETA, PD_SIGMA, draw_seed = NULL)
  set.seed(4242)
  b <- .draw_saom_theta(PD_THETA, PD_SIGMA, draw_seed = NULL)
  expect_identical(a, b)
  ## Successive draws off one outer seed differ -- this is what makes the
  ## many-draw loop in the next test independent.
  set.seed(4242)
  c1 <- .draw_saom_theta(PD_THETA, PD_SIGMA, draw_seed = NULL)
  c2 <- .draw_saom_theta(PD_THETA, PD_SIGMA, draw_seed = NULL)
  expect_false(isTRUE(all.equal(unname(c1), unname(c2))))
})


# --- 1.2 distributional correctness -----------------------------------------

test_that("many draws recover theta and covtheta as their first two moments", {
  ## The definition, checked directly: theta ~ N(thetahat, covtheta). Only the
  ## mean and the covariance are asserted, because they are the only things the
  ## downstream counterfactual uses, and they are exactly what an eigen
  ## transposition or a missing sqrt() would get wrong while still producing
  ## reproducible, plausible-looking numbers.
  n_draw <- 20000L
  set.seed(11235)
  draws <- vapply(seq_len(n_draw),
                  function(i) .draw_saom_theta(PD_THETA, PD_SIGMA, draw_seed = NULL),
                  numeric(3))
  draws <- t(draws)                                   # n_draw x 3
  expect_equal(dim(draws), c(n_draw, 3L))

  ## Mean. The largest sd is 0.5, so the Monte Carlo se of a column mean is
  ## 0.5/sqrt(20000) = 0.0035; 0.02 is a ~5.7-sigma envelope.
  expect_lt(max(abs(colMeans(draws) - unname(PD_THETA))), 0.02)

  ## Covariance, every entry including the off-diagonals. The se of a sample
  ## covariance entry here is at most sqrt((0.25*0.25 + 0.25^2)/20000) = 0.0025.
  expect_lt(max(abs(stats::cov(draws) - PD_SIGMA)), 0.02)

  ## Not vacuous: the draws actually move. A degenerate implementation that
  ## returned theta every time would pass the mean check above.
  expect_gt(min(apply(draws, 2, stats::sd)), 0.25)

  ## The correlation structure, which is where the off-diagonal terms live.
  ## cor(1,2) = 0.10/sqrt(0.25*0.16) = 0.5; cor(1,3) = -0.05/sqrt(0.25*0.09)
  ## = -1/3.
  emp <- stats::cor(draws)
  expect_lt(abs(emp[1, 2] - 0.5), 0.02)
  expect_lt(abs(emp[1, 3] + 1 / 3), 0.02)
})

test_that("a zero covariance matrix returns theta exactly", {
  ## The degenerate end of the same definition, and deterministic: with
  ## covtheta = 0 every eigenvalue is 0, so the draw must be the point estimate.
  ## This also pins down that the singular-direction branch does not perturb.
  Z <- matrix(0, 3, 3)
  drawn <- suppressWarnings(.draw_saom_theta(PD_THETA, Z, draw_seed = 7L))
  expect_identical(drawn, PD_THETA)
})


# --- 1.3 the guards, one message each ---------------------------------------

test_that("no covtheta is refused, and the message says to pass a sienaFit", {
  expect_error(.draw_saom_theta(PD_THETA, NULL, draw_seed = 1L),
               "requires the estimated covariance matrix of theta")
  expect_error(.draw_saom_theta(PD_THETA, NULL, draw_seed = 1L),
               "sienaFit", fixed = TRUE)
})

test_that("an all-NA covtheta is refused as a fit that did not identify", {
  NAmat <- matrix(NA_real_, 3, 3)
  expect_error(.draw_saom_theta(PD_THETA, NAmat, draw_seed = 1L),
               "entirely NA")
  ## The distinguishing phrase: this is a non-identification, not a bad matrix.
  expect_error(.draw_saom_theta(PD_THETA, NAmat, draw_seed = 1L),
               "did not identify", fixed = TRUE)
  ## and it must offer the escape hatch rather than just refusing
  expect_error(.draw_saom_theta(PD_THETA, NAmat, draw_seed = 1L),
               "draw_theta = FALSE", fixed = TRUE)
})

test_that("a partially-NA covtheta names the offending effects", {
  ## The whole point of this guard over the all-NA one: the analyst has to be
  ## told WHICH parameters were not identified. NA at (2,3)/(3,2) makes rows 2
  ## and 3 offending; row 1 is clean and must not be named.
  P <- PD_SIGMA
  P[2, 3] <- P[3, 2] <- NA_real_
  expect_error(.draw_saom_theta(PD_THETA, P, draw_seed = 1L),
               "NA entries for 2 effect")
  expect_error(.draw_saom_theta(PD_THETA, P, draw_seed = 1L),
               "gwespFF, inPop", fixed = TRUE)
  ## A single offending row is reported as one effect, by name.
  P1 <- PD_SIGMA
  P1[1, 1] <- NA_real_
  expect_error(.draw_saom_theta(PD_THETA, P1, draw_seed = 1L),
               "NA entries for 1 effect")
  expect_error(.draw_saom_theta(PD_THETA, P1, draw_seed = 1L),
               "density", fixed = TRUE)
})

test_that("a dimension mismatch reports both dimensions and the theta length", {
  small <- diag(c(0.1, 0.2))
  expect_error(.draw_saom_theta(PD_THETA, small, draw_seed = 1L),
               "covtheta is 2 x 2 but theta has length 3", fixed = TRUE)
  big <- diag(rep(0.1, 5))
  expect_error(.draw_saom_theta(PD_THETA, big, draw_seed = 1L),
               "covtheta is 5 x 5 but theta has length 3", fixed = TRUE)
  ## Non-square is caught too.
  expect_error(.draw_saom_theta(PD_THETA, matrix(0.1, 3, 2), draw_seed = 1L),
               "covtheta is 3 x 2", fixed = TRUE)
})

test_that("a non-positive-semi-definite covtheta is refused with its eigenvalue", {
  ## Eigenvalues 3 and -1: comfortably below the -tol threshold.
  bad <- matrix(c(1, 2, 2, 1), 2, 2)
  th  <- c(a = 0, b = 0)
  expect_error(.draw_saom_theta(th, bad, draw_seed = 1L),
               "not positive semi-definite")
  expect_error(.draw_saom_theta(th, bad, draw_seed = 1L),
               "-1", fixed = TRUE)
  ## and it must point at the diagnosis rather than at the matrix
  expect_error(.draw_saom_theta(th, bad, draw_seed = 1L),
               "convergence", fixed = TRUE)
})

test_that("a negative eigenvalue inside the tolerance is not an error", {
  ## tol = 1e-8 * max(1, max|eigenvalue|). A rounding-level negative eigenvalue
  ## -- exactly what symmetrising RSiena's covtheta leaves behind -- must warn
  ## and proceed, not stop.
  th <- c(a = 0, b = 0)
  nearly <- diag(c(0.09, -1e-12))
  expect_warning(.draw_saom_theta(th, nearly, draw_seed = 3L),
                 "numerically singular")
  expect_silent(suppressWarnings(.draw_saom_theta(th, nearly, draw_seed = 3L)))
})


# --- 1.4 near-singular directions are held fixed -----------------------------

test_that("a singular direction warns and is drawn as fixed rather than erroring", {
  ## Perfectly correlated pair: covtheta = 0.09 * J has eigenvalues 0.18 and 0.
  ## The zero direction carries no dispersion, so both coordinates must move by
  ## exactly the same amount -- that is what "drawn as fixed" means here.
  th <- c(alpha = 1.0, beta = -2.0)
  S  <- matrix(0.09, 2, 2)

  expect_warning(.draw_saom_theta(th, S, draw_seed = 99L),
                 "numerically singular")
  expect_warning(.draw_saom_theta(th, S, draw_seed = 99L),
                 "1 of 2 eigenvalues", fixed = TRUE)
  expect_warning(.draw_saom_theta(th, S, draw_seed = 99L),
                 "understates uncertainty")

  drawn <- suppressWarnings(.draw_saom_theta(th, S, draw_seed = 99L))
  dev <- drawn - th
  expect_equal(unname(dev[1]), unname(dev[2]))
  ## Not degenerate: the surviving direction really does carry variance.
  devs <- vapply(1:200, function(s)
    unname(suppressWarnings(.draw_saom_theta(th, S, draw_seed = s))[1] - th[1]),
    numeric(1))
  expect_gt(stats::sd(devs), 0.05)
  ## Var along the surviving direction is 0.09 for each coordinate.
  expect_lt(abs(stats::sd(devs) - 0.3), 0.05)
})


# ===========================================================================
# 2. .bridge_delta_summary(): the reported arithmetic
#
# Every number here reaches a table in a paper, so each is derived twice: once
# by hand from the five deltas, once from the base-R reference implementation.
# ===========================================================================

## d = cf - base = c(0.5, 0.0, 1.0, -0.5, 1.0); mean 0.4; deviations
## c(0.1, -0.4, 0.6, -0.9, 0.6); sum of squares 1.70; var 1.70/4 = 0.425.
DS_BASE <- c(1.0, 2.0, 3.0, 4.0, 5.0)
DS_CF   <- c(1.5, 2.0, 4.0, 3.5, 6.0)
DS_D    <- DS_CF - DS_BASE

test_that("the delta fixture is what the hand arithmetic below assumes", {
  expect_equal(DS_D, c(0.5, 0.0, 1.0, -0.5, 1.0))
  expect_equal(sum((DS_D - 0.4)^2), 1.70)
})

test_that("mean_delta, sd_delta and mc_se are the hand-computed values", {
  res <- .bridge_delta_summary("K_AC", DS_BASE, DS_CF, 0.95)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_equal(res$measure, "K_AC")
  expect_equal(res$n_reps, 5L)

  expect_equal(res$mean_baseline, 3.0)
  expect_equal(res$mean_cf, 3.4)
  expect_equal(res$mean_delta, 0.4)
  expect_equal(res$sd_delta, sqrt(0.425))
  expect_equal(res$mc_se, sqrt(0.425) / sqrt(5))
  ## mc_se = sd/sqrt(n) is the definition being pinned; state it twice so a
  ## change to either half is caught.
  expect_equal(res$mc_se, res$sd_delta / sqrt(res$n_reps))
  expect_equal(res$conf_level, 0.95)
})

test_that("the delta is counterfactual minus baseline", {
  ## SIGN CONVENTION. `.bridge_delta_summary()` computes `cf_vals - base_vals`
  ## and `.bridge_unit_deltas()` computes `cf - b`, so a positive delta means
  ## the counterfactual arm is HIGHER. This is NOT stated in the roxygen for
  ## either helper, nor in the @return block of run_calibrated_counterfactual()
  ## which only names the columns `baseline`, `counterfactual`, `delta`. The
  ## convention is asserted here so that it cannot flip silently, but the
  ## documentation gap is real.
  up   <- .bridge_delta_summary("m", c(1, 1, 1), c(2, 2, 2), 0.95)
  down <- .bridge_delta_summary("m", c(2, 2, 2), c(1, 1, 1), 0.95)
  expect_equal(up$mean_delta, 1)
  expect_equal(down$mean_delta, -1)
})

test_that("the paired t-test agrees with stats::t.test() and with the formula", {
  res <- .bridge_delta_summary("K_AC", DS_BASE, DS_CF, 0.95)
  tt  <- stats::t.test(DS_CF, DS_BASE, paired = TRUE, conf.level = 0.95)

  expect_equal(res$t_stat,  unname(tt$statistic))
  expect_equal(res$df,      unname(tt$parameter))
  expect_equal(res$p_value, unname(tt$p.value))
  expect_equal(res$ci_lower, tt$conf.int[1])
  expect_equal(res$ci_upper, tt$conf.int[2])

  ## Independent hand derivation, sharing no code with t.test():
  n  <- 5; se <- sqrt(0.425) / sqrt(n); tstat <- 0.4 / se; df <- n - 1
  expect_equal(res$t_stat, tstat)
  expect_equal(res$df, df)
  expect_equal(res$p_value, 2 * stats::pt(-abs(tstat), df))
  half <- stats::qt(0.975, df) * se
  expect_equal(res$ci_lower, 0.4 - half)
  expect_equal(res$ci_upper, 0.4 + half)
})

test_that("the quantile interval is stats::quantile() at the stated conf_level", {
  for (cl in c(0.95, 0.90, 0.80, 0.50)) {
    res <- .bridge_delta_summary("K_AC", DS_BASE, DS_CF, cl)
    a   <- 1 - cl
    q   <- unname(stats::quantile(DS_D, probs = c(a / 2, 1 - a / 2)))
    expect_equal(res$q_lower, q[1], info = sprintf("conf_level %.2f", cl))
    expect_equal(res$q_upper, q[2], info = sprintf("conf_level %.2f", cl))
    expect_equal(res$conf_level, cl)
    ## The t interval must track the same conf_level.
    tt <- stats::t.test(DS_CF, DS_BASE, paired = TRUE, conf.level = cl)
    expect_equal(res$ci_lower, tt$conf.int[1], info = sprintf("t at %.2f", cl))
  }
  ## A narrower level really is narrower, so conf_level is not being ignored.
  wide   <- .bridge_delta_summary("m", DS_BASE, DS_CF, 0.99)
  narrow <- .bridge_delta_summary("m", DS_BASE, DS_CF, 0.50)
  expect_gt(wide$ci_upper - wide$ci_lower, narrow$ci_upper - narrow$ci_lower)
  expect_gt(wide$q_upper - wide$q_lower, narrow$q_upper - narrow$q_lower)
})

test_that("n = 1 degrades to a single realization without NaN", {
  res <- expect_silent(.bridge_delta_summary("K_AC", 3, 4.25, 0.95))

  expect_equal(res$n_reps, 1L)
  expect_equal(res$mean_baseline, 3)
  expect_equal(res$mean_cf, 4.25)
  expect_equal(res$mean_delta, 1.25)
  ## Everything that needs a second replication is NA, not NaN and not 0.
  for (nm in c("sd_delta", "mc_se", "t_stat", "df", "p_value",
               "ci_lower", "ci_upper", "q_lower", "q_upper")) {
    expect_true(is.na(res[[nm]]), info = nm)
    expect_false(is.nan(res[[nm]]), info = nm)
  }
  ## The frame still has the full column set, so rbind across measures works
  ## even when one measure was run once.
  full <- .bridge_delta_summary("K_AC", DS_BASE, DS_CF, 0.95)
  expect_identical(names(res), names(full))
  expect_equal(nrow(rbind(res, full)), 2L)
})

test_that("the single-realization warning is raised by the caller, not here", {
  ## `.bridge_delta_summary()` is deliberately silent at n = 1; the warning
  ## about a single realization lives in run_calibrated_counterfactual() at the
  ## `n_reps < 2L` check, which sits AFTER the replication loop and therefore
  ## after two full engine runs.
  expect_silent(.bridge_delta_summary("K_AC", 3, 4, 0.95))
  skip(paste0("The n_reps = 1 warning in run_calibrated_counterfactual() is ",
              "raised after the replication loop, so reaching it requires two ",
              "real SaomNkRSienaBiEnv simulation runs. Out of scope here."))
})

test_that("zero-variance deltas give NA rather than an infinite t", {
  ## sd(d) == 0 makes the t statistic 0/0. The code refuses to call t.test()
  ## in that case; the alternative is Inf or NaN in a reported table.
  res <- .bridge_delta_summary("K_CC", c(1, 2, 3), c(2, 3, 4), 0.95)
  expect_equal(res$mean_delta, 1)
  expect_equal(res$sd_delta, 0)
  expect_equal(res$mc_se, 0)
  expect_true(is.na(res$t_stat))
  expect_true(is.na(res$p_value))
  expect_true(is.na(res$ci_lower))
  ## The quantile interval is still defined, and is degenerate at the delta.
  expect_equal(res$q_lower, 1)
  expect_equal(res$q_upper, 1)
})

test_that("the summary frame has stable columns and types across inputs", {
  a <- .bridge_delta_summary("K_AC", DS_BASE, DS_CF, 0.95)
  b <- .bridge_delta_summary("K_CC", 3, 4, 0.95)
  c3 <- .bridge_delta_summary("K_AA", c(1, 2, 3), c(2, 3, 4), 0.80)
  expect_identical(names(a), names(b))
  expect_identical(names(a), names(c3))
  expect_identical(vapply(a, function(z) class(z)[1], character(1)),
                   vapply(c3, function(z) class(z)[1], character(1)))
  expect_type(a$measure, "character")
})


# ===========================================================================
# 3. .is_rate_effect() and .rate_param_table()
#
# What this pair decides is what leaves the model. A rate effect wrongly passed
# through is converted by the crosswalk into a utility parameter; an evaluation
# effect wrongly caught is dropped from the simulation and reported as a rate.
# ===========================================================================

# --- 3.1 metadata-driven recognition ----------------------------------------

test_that("type == 'rate' in the metadata is recognized", {
  meta <- data.frame(
    effect_name = c("alpha", "beta", "gamma"),
    shortName   = c("density", "egoX", "transTrip"),
    type        = c("eval", "rate", "eval"),
    period      = NA_character_, interaction1 = "",
    stringsAsFactors = FALSE)
  expect_equal(.is_rate_effect(meta$effect_name, meta), c(FALSE, TRUE, FALSE))
})

test_that("every documented rate shortName is recognized from the metadata", {
  short <- c("Rate", "RateX", "outRate", "outRateInv", "outRateLog")
  meta <- data.frame(
    ## Deliberately opaque names: only the shortName can carry the decision.
    effect_name = paste0("opaque_", seq_along(short)),
    shortName   = short,
    type        = "eval",             # metadata type says eval on purpose
    period      = NA_character_, interaction1 = "",
    stringsAsFactors = FALSE)
  expect_true(all(.is_rate_effect(meta$effect_name, meta)))
  ## The constant the function reads is the same list.
  expect_setequal(.BRIDGE_RATE_SHORTNAMES, short)
})

test_that("metadata that cannot be aligned with theta is ignored", {
  ## The alignment rule: meta is used only when nrow(meta) == length(names).
  ## A shorter meta must not be recycled onto the wrong effects.
  meta <- data.frame(effect_name = "x", shortName = "Rate", type = "rate",
                     period = NA_character_, interaction1 = "",
                     stringsAsFactors = FALSE)
  expect_equal(.is_rate_effect(c("density", "transTrip", "inPop"), meta),
               c(FALSE, FALSE, FALSE))
  expect_equal(.is_rate_effect(c("density", "Rate", "inPop"), meta),
               c(FALSE, TRUE, FALSE))
})


# --- 3.2 name-based fallback, with and without adversaries -------------------

test_that("rate shortNames are recognized from the name alone, with suffixes", {
  nm <- c("Rate", "RateX", "outRate", "outRateInv", "outRateLog",
          "RateX.assets", "Rate.net", "outRateLog.wave2")
  expect_true(all(.is_rate_effect(nm)))
  ## Case does not matter: the key is lower-cased before matching.
  expect_true(all(.is_rate_effect(c("rate", "RATEX", "OutRateInv"))))
  ## Whitespace is trimmed.
  expect_true(all(.is_rate_effect(c("  Rate  ", "\tRateX"))))
})

test_that("real RSiena rate effect names are recognized as rate effects", {
  ## Verbatim from RSiena's allEffects table, with the xxxxxx placeholder
  ## filled in the way siena07() fills it.
  nm <- c("basic rate parameter net",
          "rate net period 1",
          "outdegree effect on rate net",
          "effect 1/outdegree on rate net",
          "effect ln(outdegree+1) on rate net",
          "effect assets on rate net",
          "constant net rate (period 2)")
  expect_true(all(.is_rate_effect(nm)))
})

test_that("evaluation effects whose names merely contain 'rate' are not caught", {
  ## Adversarial: every one of these embeds the letters r-a-t-e inside a longer
  ## word, or extends a rate shortName. None is a rate effect.
  nm <- c("density", "transTrip", "gwespFF", "inPop", "outActSqrt",
          "egoX.corporate", "altX.accurate", "simX.moderate",
          "corporateGovernance", "separateX", "deliberateX",
          "outRateInvSomething",     # extends a rate shortName
          "outRateLogistic",
          "rateOfChangeX",           # 'rate' glued to a following word
          "egoX.growth_rate",        # underscore is a word character
          "X.rate_of_return")
  flagged <- .is_rate_effect(nm)
  expect_false(any(flagged),
               info = paste("wrongly flagged:", paste(nm[flagged], collapse = ", ")))
})

test_that("NA and empty effect names do not break the classifier", {
  expect_equal(.is_rate_effect(c(NA_character_, "", "density")),
               c(FALSE, FALSE, FALSE))
  expect_equal(.is_rate_effect(character(0)), logical(0))
})


# --- 3.3 the crosswalk is not fed rate effects -------------------------------

test_that("no rate shortName collides with a crosswalk key", {
  ## Structural invariant: if any SAOM key in the mapping list inside
  ## saom_to_saomnk() were also a rate shortName, the two halves of the bridge
  ## would disagree about the same effect.
  crosswalk_keys <- c("density", "outdegree", "recip", "transTrip", "cycle3",
                      "gwespFF", "inPop", "inPopSqrt", "outAct", "outActSqrt",
                      "egoX", "altX", "simX", "sameX", "X", "XWX", "higher",
                      "totInDist2", "simEgoInDist2")
  expect_length(intersect(crosswalk_keys, .BRIDGE_RATE_SHORTNAMES), 0L)
  expect_false(any(.is_rate_effect(crosswalk_keys)))
})


# --- 3.4 KNOWN FAILURE: the name heuristic overrides the metadata ------------

test_that("metadata that says 'eval' wins over a name containing the word rate", {
  ## KNOWN FAILURE, and the bug is in the CODE, not in this test.
  ##
  ## The roxygen for .is_rate_effect() states the contract explicitly:
  ##
  ##   "Rate effects are recognized from RSiena metadata when it is available
  ##    (type == "rate", or a rate shortName), and OTHERWISE from the effect
  ##    name."
  ##
  ## The implementation does not do that. Its last line is
  ##
  ##     flag | base_key %in% tolower(.BRIDGE_RATE_SHORTNAMES) | grepl("\\brate\\b", key)
  ##
  ## so the name heuristic is OR-ed in unconditionally, even when aligned
  ## metadata is present and says `type == "eval"` with a non-rate shortName.
  ##
  ## The case below is not contrived. A monadic covariate named `rate` -- a
  ## growth rate, a churn rate, an interest rate, all ordinary firm covariates
  ## -- gives RSiena the effectName "rate ego" with shortName "egoX" and type
  ## "eval". `\\brate\\b` matches it, so the estimated homophily effect is
  ## classified as a rate parameter: silently excluded from the crosswalk,
  ## reported in $rate_params as "extracted; NOT applied", and dropped from
  ## every counterfactual built from the fit. The same happens to "rate alter",
  ## "rate similarity", and any effect on a covariate whose name ends a word at
  ## "rate" ("growth-rate ego" -- a hyphen is a word boundary too).
  ##
  ## Fix in R/searchnet-bridge.R: make the name heuristic the fallback the
  ## documentation already promises, rather than an additional OR term.
  ##
  ##     if (!is.null(meta) && nrow(meta) == length(effect_names)) {
  ##       return(flag)          # metadata is authoritative when aligned
  ##     }
  ##     base_key %in% tolower(.BRIDGE_RATE_SHORTNAMES) | grepl("\\brate\\b", key)
  ##
  ## Deliberately NOT weakened to expect TRUE. Expecting TRUE would encode
  ## "an estimated effect may be dropped because of what a covariate is called"
  ## as the intended behavior of the bridge.
  meta <- data.frame(
    effect_name  = c("rate ego", "rate alter", "outdegree (density)"),
    shortName    = c("egoX", "altX", "density"),
    type         = c("eval", "eval", "eval"),
    period       = NA_character_,
    interaction1 = c("rate", "rate", ""),
    stringsAsFactors = FALSE)
  expect_equal(.is_rate_effect(meta$effect_name, meta),
               c(FALSE, FALSE, FALSE))
})


# --- 3.5 .rate_param_table() -------------------------------------------------

RATE_TABLE_COLS <- c("saom_effect", "saom_shortName", "saom_type", "saom_period",
                     "saom_interaction1", "saom_theta", "saom_theta_used",
                     "saom_se", "note")

test_that("the rate table carries the documented columns and values", {
  thetas      <- c(Rate = 5.10, density = -1.70, RateX.assets = 0.42)
  theta_point <- c(Rate = 5.00, density = -1.70, RateX.assets = 0.40)
  se          <- c(0.30, 0.08, 0.05)
  meta <- data.frame(
    effect_name  = names(thetas),
    shortName    = c("Rate", "density", "RateX"),
    type         = c("rate", "eval", "rate"),
    period       = c("1", NA, "1"),
    interaction1 = c("", "", "assets"),
    stringsAsFactors = FALSE)
  is_rate <- .is_rate_effect(names(thetas), meta)
  expect_equal(is_rate, c(TRUE, FALSE, TRUE))

  tab <- .rate_param_table(thetas, theta_point, se, meta, is_rate)

  expect_identical(names(tab), RATE_TABLE_COLS)
  expect_equal(nrow(tab), 2L)
  expect_equal(tab$saom_effect, c("Rate", "RateX.assets"))
  expect_equal(tab$saom_shortName, c("Rate", "RateX"))
  expect_equal(tab$saom_type, c("rate", "rate"))
  expect_equal(tab$saom_period, c("1", "1"))
  expect_equal(tab$saom_interaction1, c("", "assets"))
  ## Point estimate and used value are kept apart -- they differ under a draw.
  expect_equal(tab$saom_theta, c(5.00, 0.40))
  expect_equal(tab$saom_theta_used, c(5.10, 0.42))
  expect_equal(tab$saom_se, c(0.30, 0.05))
  expect_true(all(grepl("NOT applied", tab$note)))
  ## Rownames were reset, so the table can be rbind-ed and indexed positionally
  ## rather than carrying the theta names of the rows it was selected from.
  expect_identical(rownames(tab), c("1", "2"))
})

test_that("the rate table is empty-safe and column-identical when empty", {
  thetas <- c(density = -1.7, transTrip = 0.4)
  empty  <- .rate_param_table(thetas, thetas, c(0.1, 0.2), NULL,
                              c(FALSE, FALSE))
  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), RATE_TABLE_COLS)
  ## The empty and non-empty frames must be rbind-compatible, since
  ## saom_to_saomnk() returns whichever it gets and callers stack them.
  nonempty <- .rate_param_table(c(Rate = 5), c(Rate = 5), 0.3, NULL, TRUE)
  expect_identical(names(nonempty), names(empty))
  expect_equal(nrow(rbind(empty, nonempty)), 1L)
  expect_identical(vapply(empty, function(z) class(z)[1], character(1)),
                   vapply(nonempty, function(z) class(z)[1], character(1)))
})

test_that("the rate table fills metadata columns with NA when meta is absent", {
  thetas <- c(Rate = 5.0, density = -1.7)
  tab <- .rate_param_table(thetas, thetas, c(0.3, 0.08), NULL,
                           c(TRUE, FALSE))
  expect_equal(nrow(tab), 1L)
  expect_equal(tab$saom_effect, "Rate")
  expect_true(is.na(tab$saom_shortName))
  expect_true(is.na(tab$saom_type))
  expect_true(is.na(tab$saom_period))
  expect_true(is.na(tab$saom_interaction1))
  ## The columns saom_to_saomnk()'s verbose printer selects must still exist.
  expect_true(all(c("saom_effect", "saom_shortName", "saom_theta", "saom_se")
                  %in% names(tab)))
})

test_that("misaligned metadata is not spliced into the rate table", {
  thetas <- c(Rate = 5.0, density = -1.7, RateX = 0.4)
  short_meta <- data.frame(effect_name = "Rate", shortName = "Rate",
                           type = "rate", period = "1", interaction1 = "",
                           stringsAsFactors = FALSE)
  tab <- .rate_param_table(thetas, thetas, c(0.3, 0.08, 0.05), short_meta,
                           c(TRUE, FALSE, TRUE))
  expect_equal(nrow(tab), 2L)
  expect_equal(tab$saom_effect, c("Rate", "RateX"))
  ## has_meta is FALSE here, so every metadata column is NA rather than a
  ## recycled value from the one row that was supplied.
  expect_true(all(is.na(tab$saom_shortName)))
  expect_true(all(is.na(tab$saom_type)))
})


# ===========================================================================
# 4. saom_to_saomnk(): conversion and guards, against a mock sienaFit
# ===========================================================================

# --- 4.1 the bare named-vector path -----------------------------------------

test_that("a named theta vector converts through the crosswalk", {
  ## All four mappings here are EXACT, so the default strict = TRUE converts
  ## them without complaint. `outActSqrt` replaces the `gwespFF` this fixture
  ## used to carry: gwespFF's target is not implemented for a bipartite DV and
  ## is now refused (section 4.9).
  thetas <- c(density = -1.2, outActSqrt = 0.8, inPop = 0.3, egoX.assets = 0.15)
  b <- saom_to_saomnk(thetas, verbose = FALSE)

  expect_length(b$effects, 4L)
  expect_equal(vapply(b$effects, `[[`, character(1), "effect"),
               c("density", "outActSqrt", "inPop", "egoX"))
  expect_equal(vapply(b$effects, `[[`, numeric(1), "parameter"),
               c(-1.2, 0.8, 0.3, 0.15))
  ## Every row is flagged exact, and nothing is listed as approximate.
  expect_equal(b$mapping_table$mapping_status, rep("exact", 4L))
  expect_equal(b$approximate, character(0))
  expect_true(b$strict)
  expect_true(all(vapply(b$effects, `[[`, logical(1), "fix")))
  expect_true(all(vapply(b$effects, `[[`, character(1), "dv_name") ==
                    .BRIDGE_DV_NAME))
  expect_equal(b$unmapped, character(0))
  expect_equal(nrow(b$rate_params), 0L)
  expect_false(b$theta_drawn)
  expect_null(b$covtheta)
  ## The interaction suffix is stripped for matching but kept in the log.
  expect_equal(b$mapping_table$saom_effect[4], "egoX.assets")
})

test_that("scale_factor multiplies the converted parameters", {
  thetas <- c(density = -1.2, recip = 0.8)
  ## `recip -> cycle4 * 0.5` is an APPROXIMATE mapping, so it has to be opted
  ## into; the refusal under the default is checked in section 4.9.
  b <- suppressWarnings(
    saom_to_saomnk(thetas, scale_factor = 0.5, strict = FALSE, verbose = FALSE))
  expect_equal(b$scale_factor, 0.5)
  ## density is the identity times scale; recip carries an extra 0.5 in the
  ## crosswalk (reciprocity -> 4-cycle), so 0.8 * 0.5 * 0.5 = 0.2.
  expect_equal(b$mapping_table$saomnk_parameter, c(-0.6, 0.2))
})

test_that("a non-sienaFit, non-named-numeric input is refused", {
  expect_error(saom_to_saomnk(list(a = 1), verbose = FALSE),
               "must be a sienaFit object or a named numeric vector")
  expect_error(saom_to_saomnk(c(1, 2, 3), verbose = FALSE),
               "must be a sienaFit object or a named numeric vector")
  expect_error(saom_to_saomnk("density", verbose = FALSE),
               "must be a sienaFit object or a named numeric vector")
})


# --- 4.2 unmapped effects are named in the warning ---------------------------

test_that("unmapped effects raise a warning that names each dropped effect", {
  thetas <- c(density = -1.2, unheardOf = 0.4, alsoMissing = -0.2)
  expect_warning(saom_to_saomnk(thetas, verbose = FALSE),
                 "2 estimated effect\\(s\\) have no SaoMNK counterpart")
  expect_warning(saom_to_saomnk(thetas, verbose = FALSE),
                 "unheardOf, alsoMissing", fixed = TRUE)
  ## The warning must say what the omission costs, not just that it happened.
  expect_warning(saom_to_saomnk(thetas, verbose = FALSE),
                 "not the estimated model", fixed = TRUE)

  b <- suppressWarnings(saom_to_saomnk(thetas, verbose = FALSE))
  expect_equal(b$unmapped, c("unheardOf", "alsoMissing"))
  expect_equal(nrow(b$mapping_table), 1L)
  expect_length(b$effects, 1L)
})

test_that("a fully mapped model raises no warning at all", {
  warned <- character(0)
  b <- withCallingHandlers(
    saom_to_saomnk(c(density = -1.2, inPop = 0.3), verbose = FALSE),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_length(warned, 0)
  expect_length(b$effects, 2L)
})


# --- 4.3 rate parameters are carried, warned about, and never converted ------

test_that("rate parameters land in $rate_params and fire their own warning", {
  ## Mock sienaFit; see the fixture note at the top of the file for the two
  ## deliberate deviations from a real RSiena object.
  fit <- make_siena_fit_workable()      # Rate, density, inPop

  expect_warning(saom_to_saomnk(fit, verbose = FALSE),
                 "1 rate parameter\\(s\\) were extracted")
  expect_warning(saom_to_saomnk(fit, verbose = FALSE),
                 "no rate-function", fixed = TRUE)
  expect_warning(saom_to_saomnk(fit, verbose = FALSE),
                 "governed by `iterations`", fixed = TRUE)
  ## The consequence for the counterfactual must be stated, not implied.
  expect_warning(saom_to_saomnk(fit, verbose = FALSE),
                 "SaoMNK default rather than at the estimated values",
                 fixed = TRUE)

  b <- suppressWarnings(saom_to_saomnk(fit, verbose = FALSE))
  expect_equal(nrow(b$rate_params), 1L)
  expect_equal(b$rate_params$saom_effect, "Rate")
  expect_equal(b$rate_params$saom_theta, 4.2)
  expect_equal(b$rate_params$saom_se, sqrt(0.50))
  ## and the rate parameter reached NEITHER the effects list NOR the mapping
  ## table -- the whole point of splitting it off.
  expect_equal(nrow(b$mapping_table), 2L)
  expect_false("Rate" %in% b$mapping_table$saom_effect)
  expect_length(b$effects, 2L)
  expect_false(any(vapply(b$effects, `[[`, character(1), "source_effect") == "Rate"))
  ## A rate effect is not an unmapped effect; it must not be double-reported.
  expect_equal(b$unmapped, character(0))
})

test_that("a fit with no rate parameters raises no rate warning", {
  fit <- make_siena_fit_workable(theta = c(-1.7, 0.9),
                                 short_name = c("density", "inPop"),
                                 type = c("eval", "eval"),
                                 covtheta = diag(c(0.04, 0.09)))
  warned <- character(0)
  b <- withCallingHandlers(saom_to_saomnk(fit, verbose = FALSE),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_length(warned, 0)
  expect_equal(nrow(b$rate_params), 0L)
  expect_length(b$effects, 2L)
})


# --- 4.4 draw_theta = FALSE is the unchanged historical path -----------------

test_that("draw_theta defaults to FALSE and leaves theta at the point estimate", {
  fit <- make_siena_fit_workable()
  b   <- suppressWarnings(saom_to_saomnk(fit, verbose = FALSE))

  expect_false(b$theta_drawn)
  expect_identical(b$theta_used, b$theta_point)
  expect_equal(unname(b$theta_point), c(4.2, -1.7, 0.9))
  ## The two theta columns of the mapping table coincide exactly.
  expect_identical(b$mapping_table$saom_theta_used,
                   b$mapping_table$saom_theta)
  ## Repeated calls are identical -- no RNG is touched on this path.
  b2 <- suppressWarnings(saom_to_saomnk(fit, verbose = FALSE))
  expect_identical(b$theta_used, b2$theta_used)
  expect_equal(b$mapping_table, b2$mapping_table)
  ## covtheta is carried through even when it is not used.
  expect_equal(b$covtheta, diag(c(0.50, 0.04, 0.09)))
})

test_that("draw_theta = TRUE on a bare theta vector is refused, not silently ignored", {
  thetas <- c(density = -1.2, inPop = 0.3)
  expect_error(saom_to_saomnk(thetas, draw_theta = TRUE, verbose = FALSE),
               "requires the estimated covariance matrix of theta")
})


# --- 4.5 draw_theta = TRUE propagates estimation error -----------------------

test_that("draw_theta = TRUE sets theta_drawn and moves saom_theta_used", {
  fit <- make_siena_fit_workable()
  b <- suppressWarnings(
    saom_to_saomnk(fit, draw_theta = TRUE, draw_seed = 202608L, verbose = FALSE))

  expect_true(b$theta_drawn)
  ## The point estimates are preserved untouched alongside the draw.
  expect_equal(unname(b$theta_point), c(4.2, -1.7, 0.9))
  expect_false(isTRUE(all.equal(unname(b$theta_used), unname(b$theta_point))))
  expect_identical(names(b$theta_used), names(b$theta_point))

  ## The mapping table reports BOTH: the estimate and the value converted.
  expect_equal(b$mapping_table$saom_theta, c(-1.7, 0.9))
  expect_false(isTRUE(all.equal(b$mapping_table$saom_theta_used,
                                b$mapping_table$saom_theta)))
  ## saom_theta_used is exactly the drawn value for the same effect...
  expect_equal(b$mapping_table$saom_theta_used,
               unname(b$theta_used[c("density", "inPop")]))
  ## ...and the converted parameter is the transform of the DRAWN value, not
  ## of the point estimate. This is the assertion that would catch a draw that
  ## was computed, reported, and then not actually used.
  expect_equal(b$mapping_table$saomnk_parameter,
               b$mapping_table$saom_theta_used)
  expect_equal(vapply(b$effects, `[[`, numeric(1), "parameter"),
               b$mapping_table$saom_theta_used)

  ## The rate parameter is drawn too, and reported as drawn.
  expect_equal(b$rate_params$saom_theta, 4.2)
  expect_false(isTRUE(all.equal(b$rate_params$saom_theta_used, 4.2)))
})

test_that("draw_seed makes the whole conversion reproducible", {
  fit <- make_siena_fit_workable()
  a <- suppressWarnings(saom_to_saomnk(fit, draw_theta = TRUE,
                                       draw_seed = 11L, verbose = FALSE))
  b <- suppressWarnings(saom_to_saomnk(fit, draw_theta = TRUE,
                                       draw_seed = 11L, verbose = FALSE))
  c3 <- suppressWarnings(saom_to_saomnk(fit, draw_theta = TRUE,
                                        draw_seed = 12L, verbose = FALSE))
  expect_identical(a$theta_used, b$theta_used)
  expect_equal(a$mapping_table, b$mapping_table)
  expect_false(isTRUE(all.equal(unname(a$theta_used), unname(c3$theta_used))))
})

test_that("a draw plus a scale_factor composes in the documented order", {
  ## The transform is applied to the DRAWN theta, so the reported parameter is
  ## scale_factor * drawn, never scale_factor * point.
  fit <- make_siena_fit_workable()
  drawn <- .draw_saom_theta(setNames(c(4.2, -1.7, 0.9),
                                     c("Rate", "density", "inPop")),
                            diag(c(0.50, 0.04, 0.09)), draw_seed = 77L)
  b <- suppressWarnings(saom_to_saomnk(fit, scale_factor = 0.5,
                                       draw_theta = TRUE, draw_seed = 77L,
                                       verbose = FALSE))
  expect_equal(unname(b$theta_used), unname(drawn))
  expect_equal(b$mapping_table$saomnk_parameter,
               0.5 * unname(drawn[c("density", "inPop")]))
})


# --- 4.6 KNOWN FAILURE: coef() cannot read a sienaFit ------------------------

test_that("saom_to_saomnk() reads theta out of a real sienaFit", {
  ## KNOWN FAILURE, and the bug is in the CODE, not in this test.
  ##
  ## .extract_saom_theta() line 348 is
  ##
  ##     thetas <- coef(saom_result)
  ##
  ## RSiena defines no `coef.sienaFit` method (methods(class = "sienaFit")
  ## returns only print and summary) and neither does this package, so `coef()`
  ## dispatches to stats::coef.default(), which returns `object$coefficients`.
  ## A sienaFit has no `coefficients` element: ?siena07's Value section names
  ## `theta` ("Estimated value of theta"), `covtheta` and `se`. So `thetas` is
  ## NULL for every real fit, the row-count check downstream sees "0 estimated
  ## parameters", and
  ##
  ##     names(thetas) <- paste0("theta", seq_along(thetas))
  ##
  ## then fails with "attempt to set an attribute on NULL".
  ##
  ## The whole sienaFit branch of the bridge is therefore dead on real input.
  ## Only the bare-named-vector branch works today, which is why the mock in
  ## this file has to carry a `$coefficients` field that RSiena never produces.
  ##
  ## Fix in R/searchnet-bridge.R, inside .extract_saom_theta():
  ##
  ##     thetas <- saom_result$theta
  ##     if (is.null(thetas)) thetas <- coef(saom_result)   # non-RSiena fits
  ##     if (is.null(thetas)) stop("sienaFit carries no $theta", call. = FALSE)
  ##
  ## Deliberately NOT weakened to expect_error(). The bridge's documented
  ## first argument is "An RSiena sienaFit result object (from siena07)".
  fit <- make_siena_fit_real()
  b <- suppressWarnings(saom_to_saomnk(fit, verbose = FALSE))
  expect_equal(unname(b$theta_point), c(4.2, -1.7, 0.9))
})


# --- 4.7 KNOWN FAILURE: theta is named by effectName, crosswalk keys shortName -

test_that("a real sienaFit's effects reach the crosswalk", {
  ## KNOWN FAILURE, and the bug is in the CODE, not in this test. Independent
  ## of 4.6 -- it survives any fix to coef().
  ##
  ## .extract_saom_theta() names the theta vector from the RSiena metadata with
  ##
  ##     names(thetas) <- as.character(eff_df$effectName)
  ##
  ## `effectName` is RSiena's long human-readable label: "outdegree (density)",
  ## "transitive triplets", "reciprocity", "GWESP I -> K -> J (69)", "assets
  ## ego". The crosswalk inside saom_to_saomnk() matches those names against
  ## keys that are RSiena SHORTNAMES -- "density", "transTrip", "recip",
  ## "gwespFF", "egoX" -- after stripping a trailing ".suffix". A long
  ## effectName can never equal a shortName, so with a genuine fit EVERY
  ## evaluation effect falls through to `unmapped`, the effects list comes back
  ## empty, and the "0 of N mapped" result is reported as a modeling fact
  ## rather than as a naming failure. The function's own @examples use
  ## shortName keys (`c(density = -1.2, gwespFF = 0.8, ...)`), which is what
  ## the crosswalk was written for.
  ##
  ## Verified against RSiena's own allEffects table:
  ##   effectName "outdegree (density)"  <-> shortName "density"
  ##   effectName "transitive triplets"  <-> shortName "transTrip"
  ##
  ## Fix in R/searchnet-bridge.R, inside .extract_saom_theta(): name theta from
  ## shortName, and append interaction1 as the suffix the crosswalk already
  ## knows how to strip, keeping effectName in `meta` for reporting.
  ##
  ##     nm <- as.character(eff_df$shortName)
  ##     i1 <- as.character(eff_df$interaction1)
  ##     names(thetas) <- ifelse(nzchar(i1) & !is.na(i1), paste0(nm, ".", i1), nm)
  ##
  ## Deliberately NOT weakened to assert that everything is unmapped. A bridge
  ## that maps nothing is not a bridge.
  fit <- make_siena_fit_real()
  fit$coefficients <- fit$theta      # route around defect 4.6 only
  b <- suppressWarnings(saom_to_saomnk(fit, verbose = FALSE))
  expect_equal(b$unmapped, character(0))
  expect_equal(nrow(b$mapping_table), 2L)
  expect_equal(b$mapping_table$saomnk_effect, c("density", "inPop"))
})


# --- 4.8 effect-row alignment -----------------------------------------------

test_that("excluded effect rows are filtered before alignment", {
  ## RSiena's effects object carries every candidate effect with an `include`
  ## flag; only the included ones are estimated. The bridge filters on it.
  fit <- make_siena_fit_workable(
    theta      = c(4.2, -1.7),
    short_name = c("Rate", "density", "inPop"),
    type       = c("rate", "eval", "eval"),
    covtheta   = diag(c(0.50, 0.04)))
  fit$effects <- fit$effects[1:3, ]
  fit$effects$include <- c(TRUE, TRUE, FALSE)
  fit$coefficients <- c(4.2, -1.7)

  b <- suppressWarnings(saom_to_saomnk(fit, verbose = FALSE))
  expect_equal(names(b$theta_point), c("Rate", "density"))
  expect_equal(nrow(b$mapping_table), 1L)
  expect_equal(b$mapping_table$saom_effect, "density")
})

test_that("an unresolvable row mismatch warns and falls back to name inference", {
  fit <- make_siena_fit_workable(
    theta      = c(4.2, -1.7),
    short_name = c("Rate", "density", "inPop"),
    type       = c("rate", "eval", "eval"),
    covtheta   = diag(c(0.50, 0.04)))
  fit$effects <- fit$effects[1:3, ]
  fit$effects$include <- c(TRUE, TRUE, TRUE)     # 3 rows, 2 estimates
  fit$coefficients <- c(4.2, -1.7)

  ## Two warnings are raised on this path -- the misalignment and then the
  ## unmapped fallout of it -- so both are collected rather than matched one at
  ## a time, which would let the other bubble out of the test.
  warned <- character(0)
  b <- withCallingHandlers(saom_to_saomnk(fit, verbose = FALSE),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_length(warned, 2L)
  expect_true(any(grepl("3 effect rows but 2 estimated", warned)))
  expect_true(any(grepl("inferred from effect names only", warned, fixed = TRUE)))
  expect_true(any(grepl("have no SaoMNK counterpart", warned, fixed = TRUE)))

  ## $coefficients here is unnamed, so the fallback invents positional names
  ## and every effect becomes unmapped -- which is loudly reported, not silent.
  expect_equal(names(b$theta_point), c("theta1", "theta2"))
  expect_equal(b$unmapped, c("theta1", "theta2"))
})

test_that("a sienaFit with no effectName anywhere is refused", {
  fit <- make_siena_fit_workable()
  fit$effects <- NULL
  expect_error(saom_to_saomnk(fit, verbose = FALSE),
               "Cannot find effectName in sienaFit object")
})

test_that("$requestedEffects is used when $effects is absent", {
  fit <- make_siena_fit_workable()
  fit$requestedEffects <- fit$effects
  fit$effects <- NULL
  b <- suppressWarnings(saom_to_saomnk(fit, verbose = FALSE))
  expect_equal(names(b$theta_point), c("Rate", "density", "inPop"))
})


# ===========================================================================
# 4.9 strict: approximate mappings, and mappings with no bipartite target
#
# The crosswalk contains three kinds of entry and the difference between them
# is the difference between a calibrated counterfactual and a counterfactual
# calibrated to some other model. What is tested here is that the three kinds
# behave differently, and that the messages say which kind fired.
# ===========================================================================

test_that("transTriads really is unavailable for a bipartite DV in this RSiena", {
  ## The claim the refusal rests on, checked against RSiena rather than
  ## asserted. If a future RSiena adds transTriads to the bipartite effect
  ## group, this test fails FIRST and the refusal can be relaxed deliberately.
  skip_if_not_installed("RSiena")
  set.seed(9)
  M <- 8; N <- 5
  b1 <- matrix(stats::rbinom(M * N, 1, 0.4), M, N)
  b2 <- matrix(stats::rbinom(M * N, 1, 0.4), M, N)
  arr <- array(c(b1, b2), dim = c(M, N, 2))
  ACT <- RSiena::sienaNodeSet(M, nodeSetName = "ACTORS")
  CMP <- RSiena::sienaNodeSet(N, nodeSetName = "COMPONENTS")
  dv <- RSiena::sienaDependent(arr, type = "bipartite",
                               nodeSet = c("ACTORS", "COMPONENTS"),
                               allowOnly = FALSE)
  ## A component x component dyadic covariate, so that XWX -- the effect the
  ## bridge registers W into -- is present in the table too.
  wc <- RSiena::coDyadCovar(matrix(0.3, N, N),
                            nodeSet = c("COMPONENTS", "COMPONENTS"))
  dat <- RSiena::sienaDataCreate(dv, wc, nodeSets = list(ACT, CMP))
  sn <- unique(as.character(suppressMessages(RSiena::getEffects(dat))$shortName))

  expect_false("transTriads" %in% sn)
  ## and the effects the crosswalk DOES target for bipartite structure are there
  expect_true(all(c("density", "cycle4", "inPop", "inPopSqrt",
                    "outAct", "outActSqrt", "XWX") %in% sn))
  ## The constant the code refuses on names exactly this effect.
  expect_true("transTriads" %in% .BRIDGE_NOT_IN_BIPARTITE)
})

test_that("a mapping onto transTriads is refused as a non-implementation", {
  for (nm in c("transTrip", "gwespFF")) {
    th <- c(density = -1.2, 0.8); names(th)[2] <- nm
    expect_error(saom_to_saomnk(th, verbose = FALSE),
                 "RSiena does not implement for a BIPARTITE", fixed = TRUE,
                 info = nm)
    expect_error(saom_to_saomnk(th, verbose = FALSE),
                 "NON-IMPLEMENTATION", fixed = TRUE, info = nm)
    ## The distinction that matters for reporting: not a null, not a
    ## non-identification.
    expect_error(saom_to_saomnk(th, verbose = FALSE),
                 "not a null", fixed = TRUE, info = nm)
    ## Both the source effect and the target it would have become are named.
    expect_error(saom_to_saomnk(th, verbose = FALSE), nm, fixed = TRUE)
    expect_error(saom_to_saomnk(th, verbose = FALSE), "transTriads", fixed = TRUE)
  }
})

test_that("strict = FALSE does NOT relax a non-implementation", {
  ## No argument can make RSiena implement the effect, so the escape hatch that
  ## exists for approximations must not appear to work here.
  th <- c(density = -1.2, transTrip = 0.8)
  expect_error(saom_to_saomnk(th, strict = FALSE, verbose = FALSE),
               "NON-IMPLEMENTATION", fixed = TRUE)
  expect_error(saom_to_saomnk(th, strict = FALSE, verbose = FALSE),
               "strict = FALSE does not relax this", fixed = TRUE)
})

test_that("approximate mappings are refused by default and named one by one", {
  th <- c(density = -1.2, recip = 0.8, cycle3 = 0.4,
          simX.assets = 0.2, sameX.sic = 0.1, higher.size = -0.3)
  err <- tryCatch(saom_to_saomnk(th, verbose = FALSE),
                  error = function(e) conditionMessage(e))
  expect_type(err, "character")
  expect_match(err, "APPROXIMATE", fixed = TRUE)
  expect_match(err, "5 estimated effect", fixed = TRUE)
  ## Every approximate source effect is named, with what it would become.
  for (nm in c("recip", "cycle3", "simX.assets", "sameX.sic", "higher.size"))
    expect_match(err, nm, fixed = TRUE, info = nm)
  for (nm in c("cycle4", "egoX", "altX"))
    expect_match(err, nm, fixed = TRUE, info = nm)
  ## The reason, not just the refusal.
  expect_match(err, "DIFFERENT model", fixed = TRUE)
  ## and the way out
  expect_match(err, "strict = FALSE", fixed = TRUE)
  ## The exact mapping in the same vector is NOT named as approximate.
  expect_false(grepl("density (theta", err, fixed = TRUE))
})

test_that("strict = FALSE converts approximations, warns, and records them", {
  th <- c(density = -1.2, recip = 0.8, cycle3 = 0.4)

  expect_warning(saom_to_saomnk(th, strict = FALSE, verbose = FALSE),
                 "APPROXIMATE crosswalk entry", fixed = TRUE)
  expect_warning(saom_to_saomnk(th, strict = FALSE, verbose = FALSE),
                 "not the estimated model", fixed = TRUE)

  b <- suppressWarnings(saom_to_saomnk(th, strict = FALSE, verbose = FALSE))
  expect_false(b$strict)
  expect_equal(b$approximate, c("recip", "cycle3"))
  expect_equal(b$mapping_table$mapping_status,
               c("exact", "approximate", "approximate"))
  expect_equal(vapply(b$effects, `[[`, character(1), "effect"),
               c("density", "cycle4", "cycle4"))
  ## recip carries the documented 0.5, cycle3 does not.
  expect_equal(b$mapping_table$saomnk_parameter, c(-1.2, 0.4, 0.4))
  ## and the status travels on the effect specs too, not only the table
  expect_equal(vapply(b$effects, `[[`, character(1), "mapping_status"),
               c("exact", "approximate", "approximate"))
})

test_that("an all-exact model is silent under either value of strict", {
  th <- c(density = -1.2, inPop = 0.3, outActSqrt = 0.1, X.dist = -0.2)
  for (s in c(TRUE, FALSE)) {
    warned <- character(0)
    b <- withCallingHandlers(
      saom_to_saomnk(th, strict = s, verbose = FALSE),
      warning = function(w) {
        warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning")
      })
    expect_length(warned, 0)
    expect_equal(b$approximate, character(0))
    expect_equal(unique(b$mapping_table$mapping_status), "exact")
  }
})

test_that("strict must be a single TRUE or FALSE", {
  th <- c(density = -1.2)
  for (bad in list(NA, "yes", c(TRUE, FALSE), NULL, 1)) {
    expect_error(saom_to_saomnk(th, strict = bad, verbose = FALSE),
                 "strict must be a single TRUE or FALSE", fixed = TRUE)
  }
})

test_that("an estimated XWX maps exactly, so epistasis can be calibrated", {
  ## Without this crosswalk entry the fitted epistasis weight had nowhere to go
  ## and every counterfactual ran with W switched off.
  b <- saom_to_saomnk(c(density = -1.2, XWX = 0.75), verbose = FALSE)
  expect_equal(vapply(b$effects, `[[`, character(1), "effect"),
               c("density", "XWX"))
  expect_equal(b$mapping_table$mapping_status, c("exact", "exact"))
  expect_equal(b$mapping_table$saomnk_parameter, c(-1.2, 0.75))
})


# ===========================================================================
# 5. .bridge_unit_deltas(): the per-actor / per-component tables
# ===========================================================================

test_that("actor deltas are tidy, correctly named, and cf minus baseline", {
  ## SIGN CONVENTION: the code computes `cf - b`, i.e. counterfactual minus
  ## baseline. This is NOT documented in the roxygen for .bridge_unit_deltas()
  ## (which says only "A tidy data.frame, one row per unit") nor in the
  ## @return block of run_calibrated_counterfactual(). Asserted here so it
  ## cannot flip unnoticed; the documentation gap stands.
  k4_b <- make_k4(K_AC = c(1, 2, 3), K_CA = c(2, 4),
                  K_AA = c(0, 1, 2), K_CC = c(1, 1))
  k4_c <- make_k4(K_AC = c(2, 2, 5), K_CA = c(3, 4),
                  K_AA = c(1, 1, 0), K_CC = c(0, 2))

  out <- .bridge_unit_deltas(k4_b, k4_c, c("K_AC", "K_AA"), "actor",
                             c("f1", "f2", "f3"), rep_id = 2L, seed_r = 77L)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_identical(names(out),
                   c("rep", "seed", "actor", "actor_id",
                     "K_AC_baseline", "K_AC_counterfactual", "delta_K_AC",
                     "K_AA_baseline", "K_AA_counterfactual", "delta_K_AA"))
  expect_equal(out$rep, rep(2L, 3))
  expect_equal(out$seed, rep(77L, 3))
  expect_equal(out$actor, 1:3)
  expect_equal(out$actor_id, c("f1", "f2", "f3"))

  expect_equal(out$K_AC_baseline, c(1, 2, 3))
  expect_equal(out$K_AC_counterfactual, c(2, 2, 5))
  expect_equal(out$delta_K_AC, c(1, 0, 2))
  expect_equal(out$K_AA_baseline, c(0, 1, 2))
  expect_equal(out$delta_K_AA, c(1, 0, -2))
  ## The identity, stated directly.
  expect_equal(out$delta_K_AC, out$K_AC_counterfactual - out$K_AC_baseline)
  expect_equal(out$delta_K_AA, out$K_AA_counterfactual - out$K_AA_baseline)
})

test_that("component deltas are labeled by the component, not the actor", {
  k4_b <- make_k4(K_AC = c(1, 2, 3), K_CA = c(2, 4),
                  K_AA = c(0, 1, 2), K_CC = c(1, 1))
  k4_c <- make_k4(K_AC = c(2, 2, 5), K_CA = c(3, 4),
                  K_AA = c(1, 1, 0), K_CC = c(0, 2))

  out <- .bridge_unit_deltas(k4_b, k4_c, c("K_CA", "K_CC"), "component",
                             c("sic20", "sic35"), rep_id = 1L, seed_r = 42L)
  expect_identical(names(out),
                   c("rep", "seed", "component", "component_id",
                     "K_CA_baseline", "K_CA_counterfactual", "delta_K_CA",
                     "K_CC_baseline", "K_CC_counterfactual", "delta_K_CC"))
  expect_equal(nrow(out), 2L)
  expect_equal(out$component, 1:2)
  expect_equal(out$component_id, c("sic20", "sic35"))
  expect_equal(out$delta_K_CA, c(1, 0))
  expect_equal(out$delta_K_CC, c(-1, 1))
  ## The actor and component tables rbind within their own kind but are
  ## distinct frames -- names must not collide.
  expect_false("actor" %in% names(out))
})

test_that("absent or mis-sized unit ids fall back to the positional index", {
  k4_b <- make_k4(K_AC = c(1, 2, 3), K_CA = c(2, 4),
                  K_AA = c(0, 1, 2), K_CC = c(1, 1))
  k4_c <- make_k4(K_AC = c(2, 2, 5), K_CA = c(3, 4),
                  K_AA = c(1, 1, 0), K_CC = c(0, 2))

  ## NULL ids (length 0)
  a <- .bridge_unit_deltas(k4_b, k4_c, c("K_AC", "K_AA"), "actor",
                           NULL, 1L, 1L)
  expect_equal(a$actor_id, c("1", "2", "3"))
  expect_type(a$actor_id, "character")

  ## too few
  b <- .bridge_unit_deltas(k4_b, k4_c, c("K_AC", "K_AA"), "actor",
                           c("only_one"), 1L, 1L)
  expect_equal(b$actor_id, c("1", "2", "3"))

  ## too many
  c3 <- .bridge_unit_deltas(k4_b, k4_c, c("K_AC", "K_AA"), "actor",
                            as.character(1:9), 1L, 1L)
  expect_equal(c3$actor_id, c("1", "2", "3"))

  ## exactly right: the supplied ids survive, including non-sequential ones
  d <- .bridge_unit_deltas(k4_b, k4_c, c("K_AC", "K_AA"), "actor",
                           c("9", "4", "7"), 1L, 1L)
  expect_equal(d$actor_id, c("9", "4", "7"))
  ## and the positional column is still 1:n regardless
  expect_equal(d$actor, 1:3)
})

test_that("arms that disagree on the number of units are refused", {
  k4_b <- make_k4(K_AC = c(1, 2, 3), K_CA = c(2, 4),
                  K_AA = c(0, 1, 2), K_CC = c(1, 1))
  k4_c <- make_k4(K_AC = c(2, 2), K_CA = c(3, 4),
                  K_AA = c(1, 1), K_CC = c(0, 2))

  expect_error(.bridge_unit_deltas(k4_b, k4_c, c("K_AC", "K_AA"), "actor",
                                   NULL, 1L, 1L),
               "Arms disagree on the number of actors \\(3 vs 2\\)")
  expect_error(.bridge_unit_deltas(k4_b, k4_c, c("K_AC", "K_AA"), "actor",
                                   NULL, 1L, 1L),
               "per-unit deltas are undefined", fixed = TRUE)
  ## The label is interpolated, so a component mismatch says "components".
  kb <- make_k4(K_AC = 1, K_CA = c(1, 2, 3), K_AA = 1, K_CC = c(1, 2, 3))
  kc <- make_k4(K_AC = 1, K_CA = c(1, 2),    K_AA = 1, K_CC = c(1, 2))
  expect_error(.bridge_unit_deltas(kb, kc, c("K_CA", "K_CC"), "component",
                                   NULL, 1L, 1L),
               "number of components")
})

## NOTE (gap, not a live bug): the arm-length guard checks only `measures[1]`.
## If a LATER measure had a different length, `out[[...]] <- v` on a data.frame
## recycles silently whenever the length divides the row count, fabricating
## per-unit values. It is not reachable through the current caller, because
## `.extract_k4_summary()` always returns K_AC/K_AA of length M and K_CA/K_CC
## of length N, and the two calls in run_calibrated_counterfactual() group them
## exactly that way. Worth closing if the measure groups ever change.

test_that("a single-unit table is well formed", {
  k4_b <- make_k4(K_AC = 4, K_CA = 4, K_AA = 0, K_CC = 0)
  k4_c <- make_k4(K_AC = 6, K_CA = 6, K_AA = 0, K_CC = 0)
  out <- .bridge_unit_deltas(k4_b, k4_c, "K_AC", "actor", "solo", 3L, 55L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$delta_K_AC, 2)
  expect_equal(out$actor_id, "solo")
  expect_identical(names(out), c("rep", "seed", "actor", "actor_id",
                                 "K_AC_baseline", "K_AC_counterfactual",
                                 "delta_K_AC"))
})

test_that("per-unit deltas aggregate to the replication-level delta", {
  ## The consistency the two tables must satisfy: run_calibrated_counterfactual()
  ## builds `rep_deltas` from mean_K_* and `actor_deltas` from the per-unit
  ## vectors, so the mean of the per-unit deltas has to equal the delta of the
  ## means. A bug in either path shows up as a disagreement here.
  set.seed(31415)
  b_vec <- stats::runif(12, 0, 5)
  c_vec <- stats::runif(12, 0, 5)
  k4_b <- make_k4(K_AC = b_vec, K_CA = 1, K_AA = b_vec, K_CC = 1)
  k4_c <- make_k4(K_AC = c_vec, K_CA = 1, K_AA = c_vec, K_CC = 1)
  out <- .bridge_unit_deltas(k4_b, k4_c, c("K_AC", "K_AA"), "actor",
                             NULL, 1L, 1L)
  expect_equal(mean(out$delta_K_AC),
               k4_c$mean_K_AC - k4_b$mean_K_AC)
})


# ===========================================================================
# 6. run_calibrated_counterfactual(): the guards that fire before the engine
#
# Everything past `conf_level` construction needs a real SaomNkRSienaBiEnv and
# two simulation runs per replication, so only the argument guards are tested
# here; the rest is skipped with a reason rather than mocked into meaninglessness.
# ===========================================================================

## Minimum object the guards inspect: $env non-NULL and $effects present.
## DIR_OUTPUT is set because a scenario that passes the guards goes on to build
## a real environment, and every arm writes an RSiena sink file into the
## environment's output directory -- which defaults to getwd(), i.e. this
## directory.
fake_bridge_env    <- list(env = list(M = 3L, N = 4L, DIR_OUTPUT = tempdir()),
                           bi_matrix = matrix(0L, 3, 4),
                           W = diag(4))
fake_bridge_params <- list(effects = list())
ok_scenario <- list(name = "s", modify = list(XWX = 2.0))

test_that("n_reps must be a positive integer", {
  expect_error(run_calibrated_counterfactual(fake_bridge_env, fake_bridge_params,
                                             scenario = ok_scenario, n_reps = 0,
                                             verbose = FALSE),
               "n_reps must be a positive integer", fixed = TRUE)
  expect_error(run_calibrated_counterfactual(fake_bridge_env, fake_bridge_params,
                                             scenario = ok_scenario, n_reps = -3,
                                             verbose = FALSE),
               "n_reps must be a positive integer", fixed = TRUE)
})

test_that("conf_level must lie strictly inside (0, 1)", {
  for (bad in list(0, 1, -0.2, 1.5, "0.95")) {
    expect_error(run_calibrated_counterfactual(fake_bridge_env, fake_bridge_params,
                                               scenario = ok_scenario,
                                               conf_level = bad, verbose = FALSE),
                 "conf_level must be a number strictly between 0 and 1",
                 fixed = TRUE,
                 info = paste("conf_level =", format(bad)))
  }
})

test_that("a malformed scenario or bridge object is refused before any run", {
  expect_error(run_calibrated_counterfactual(fake_bridge_env, fake_bridge_params,
                                             scenario = list(name = "s"),
                                             verbose = FALSE))
  expect_error(run_calibrated_counterfactual(fake_bridge_env, list(),
                                             scenario = ok_scenario,
                                             verbose = FALSE))
  expect_error(run_calibrated_counterfactual(list(), fake_bridge_params,
                                             scenario = ok_scenario,
                                             verbose = FALSE))
})

test_that("the matched-seed caveat says what matched seeds do NOT buy", {
  ## The caveat is a reporting obligation, not decoration: it is returned in
  ## $caveat and printed by the verbose path, and its content is what stops a
  ## single pair being read as a counterfactual.
  expect_type(.BRIDGE_MATCHED_SEED_CAVEAT, "character")
  expect_length(.BRIDGE_MATCHED_SEED_CAVEAT, 1L)
  expect_true(grepl("INITIALISATION only", .BRIDGE_MATCHED_SEED_CAVEAT,
                    fixed = TRUE))
  expect_true(grepl("do NOT share the realized", .BRIDGE_MATCHED_SEED_CAVEAT,
                    fixed = TRUE))
  expect_true(grepl("Monte Carlo error", .BRIDGE_MATCHED_SEED_CAVEAT,
                    fixed = TRUE))
  expect_true(grepl("not a counterfactual", .BRIDGE_MATCHED_SEED_CAVEAT,
                    fixed = TRUE))
})

test_that("the four K measures the comparison reports are the documented set", {
  expect_identical(.BRIDGE_K_MEASURES, c("K_AC", "K_CA", "K_AA", "K_CC"))
  ## The summary is assembled one row per measure, so the comparison names are
  ## determined by this constant.
  summ <- do.call(rbind, lapply(.BRIDGE_K_MEASURES, function(m)
    .bridge_delta_summary(m, DS_BASE, DS_CF, 0.95)))
  comparison <- setNames(as.list(summ$mean_delta),
                         paste0("delta_", summ$measure))
  expect_identical(names(comparison),
                   c("delta_K_AC", "delta_K_CA", "delta_K_AA", "delta_K_CC"))
  expect_equal(unlist(comparison, use.names = FALSE), rep(0.4, 4))
})

## --- 6.1 a scenario key that matches nothing is a refusal, not a no-op ------

test_that("an unmatched modify key is refused, naming the key and the alternatives", {
  bp <- saom_to_saomnk(c(density = -1.2, inPop = 0.3), verbose = FALSE)
  err <- tryCatch(
    run_calibrated_counterfactual(fake_bridge_env, bp,
                                  scenario = list(name = "typo",
                                                  modify = list(inPo = 2.0)),
                                  verbose = FALSE),
    error = function(e) conditionMessage(e))
  expect_type(err, "character")
  expect_match(err, "inPo", fixed = TRUE)          # the offending key
  expect_match(err, "typo", fixed = TRUE)          # the scenario it came from
  expect_match(err, "density, inPop", fixed = TRUE)  # what was available
  ## The reason this is an error and not a warning: a silent miss is reported
  ## as a delta near zero with a full Monte Carlo interval around it.
  expect_match(err, "NO intervention", fixed = TRUE)

  ## Several bad keys are all named, not just the first.
  err2 <- tryCatch(
    run_calibrated_counterfactual(fake_bridge_env, bp,
                                  scenario = list(name = "s",
                                                  modify = list(nope = 2, alsoNope = 3)),
                                  verbose = FALSE),
    error = function(e) conditionMessage(e))
  expect_match(err2, "nope, alsoNope", fixed = TRUE)
  expect_match(err2, "2 effect", fixed = TRUE)
})

test_that("the shipped scenarios stop when the fitted model lacks their effect", {
  ## get_orm_scenarios()$remove_homophily modifies egoX. A bridge whose SAOM
  ## estimated no egoX has no such effect, and the run must say so rather than
  ## simulate an intervention that does not happen.
  bp <- saom_to_saomnk(c(density = -1.2, inPop = 0.3), verbose = FALSE)
  expect_error(
    run_calibrated_counterfactual(fake_bridge_env, bp,
                                  scenario = get_orm_scenarios()$remove_homophily,
                                  verbose = FALSE),
    "egoX", fixed = TRUE)
})

test_that("XWX counts as available whenever a W matrix is registered", {
  ## The epistasis scenario must NOT be refused as an unknown key: the bridge
  ## registers W and the XWX effect that reads it, even when the SAOM estimated
  ## no XWX term. It is instead warned about as inert at weight 0.
  bp <- saom_to_saomnk(c(density = -1.2), verbose = FALSE)
  err <- tryCatch(
    run_calibrated_counterfactual(fake_bridge_env, bp,
                                  scenario = get_orm_scenarios()$epistasis_boost,
                                  n_reps = 1, verbose = FALSE),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w))
  ## Whatever happens next needs the engine; what matters is that it is not the
  ## unknown-key refusal.
  expect_false(grepl("does not contain", err, fixed = TRUE))
})

test_that("a modify list with an unnamed or duplicated key is refused", {
  bp <- saom_to_saomnk(c(density = -1.2, inPop = 0.3), verbose = FALSE)
  expect_error(
    run_calibrated_counterfactual(fake_bridge_env, bp,
                                  scenario = list(name = "s",
                                                  modify = list(density = 2, 3)),
                                  verbose = FALSE),
    "NAMED list", fixed = TRUE)
  dup <- list(name = "s", modify = list(density = 2, density = 3))
  expect_error(
    run_calibrated_counterfactual(fake_bridge_env, bp, scenario = dup,
                                  verbose = FALSE),
    "more than once", fixed = TRUE)
})

test_that("a W matrix of the wrong shape is refused before any run", {
  bp <- saom_to_saomnk(c(density = -1.2), verbose = FALSE)
  bad_env <- fake_bridge_env
  bad_env$W <- diag(3)                      # env$N is 4
  expect_error(
    run_calibrated_counterfactual(bad_env, bp, scenario = ok_scenario,
                                  verbose = FALSE),
    "must be a numeric 4 x 4 matrix", fixed = TRUE)
})

test_that("covariate-dependent effects with no covariate are dropped, loudly", {
  ## egoX converted out of a SAOM names the effect but not the covariate it was
  ## estimated on, and this bridge registers none. Left in, it is either inert
  ## (egoX) or fatal (totInDist2). It is dropped, and the drop is named.
  bp <- saom_to_saomnk(c(density = -1.2, egoX.assets = 0.4,
                         simEgoInDist2 = 0.2), verbose = FALSE)
  expect_warning(
    tryCatch(run_calibrated_counterfactual(fake_bridge_env, bp,
                                           scenario = list(name = "s",
                                                           modify = list(density = 2)),
                                           n_reps = 1, verbose = FALSE),
             error = function(e) NULL),
    "were DROPPED from both arms", fixed = TRUE)
  expect_warning(
    tryCatch(run_calibrated_counterfactual(fake_bridge_env, bp,
                                           scenario = list(name = "s",
                                                           modify = list(density = 2)),
                                           n_reps = 1, verbose = FALSE),
             error = function(e) NULL),
    "egoX", fixed = TRUE)
})


# ===========================================================================
# 7. empirical_to_saomnk_env(): the guards that fire before the engine
# ===========================================================================

make_mi_data <- function(M = 4, sic = c("20", "20", "35", "35")) {
  net <- matrix(0L, M, M)
  net[1, 2] <- 1L; net[3, 4] <- 1L
  list(imputations = list(list(
    list(network = net, covariates = list(sic = sic))
  )))
}

test_that("an out-of-range imputation or wave is refused with the count", {
  mi <- make_mi_data()
  expect_error(empirical_to_saomnk_env(mi, imputation = 2),
               "Imputation 2 requested but only 1 available", fixed = TRUE)
  expect_error(empirical_to_saomnk_env(mi, wave = 3),
               "Wave 3 requested but only 1 available", fixed = TRUE)
})

test_that("a min_firms_per_sic that empties the component set is refused", {
  mi <- make_mi_data()
  expect_error(empirical_to_saomnk_env(mi, min_firms_per_sic = 99),
               "No SIC categories have >= 99 firms", fixed = TRUE)
  expect_error(empirical_to_saomnk_env(mi, min_firms_per_sic = 99),
               "Lower min_firms_per_sic", fixed = TRUE)
})

test_that("a malformed mi_data object is refused", {
  expect_error(empirical_to_saomnk_env(list()))
  expect_error(empirical_to_saomnk_env(list(imputations = list(
    list(list(network = "not a matrix", covariates = list(sic = "20")))))))
})

test_that("the bipartite construction itself is not exercised here", {
  skip(paste0("Past the input guards, empirical_to_saomnk_env() constructs a ",
              "SaomNkRSienaBiEnv, which is a real engine object; the matrix and ",
              "W construction are covered by the engine's own test files."))
})


# ===========================================================================
# 8. get_orm_scenarios(): shape of the shipped scenarios
# ===========================================================================

test_that("every shipped scenario has the fields run_calibrated_counterfactual reads", {
  sc <- get_orm_scenarios()
  expect_type(sc, "list")
  expect_setequal(names(sc),
                  c("double_closure", "remove_homophily", "double_popularity",
                    "density_shock", "remove_rivalry", "epistasis_boost"))
  for (nm in names(sc)) {
    s <- sc[[nm]]
    ## These three are exactly what the stopifnot() at the top of
    ## run_calibrated_counterfactual() requires, plus the description.
    expect_type(s$name, "character")
    expect_type(s$description, "character")
    expect_type(s$modify, "list")
    expect_gt(length(s$modify), 0L)
    expect_true(all(nzchar(names(s$modify))), info = nm)
    expect_true(all(vapply(s$modify, is.numeric, logical(1))), info = nm)
  }
})

test_that("scenario modify keys are SaoMNK-side effect names, not SAOM ones", {
  ## The multiplier is matched against `e$effect`, which is the SaoMNK name
  ## produced by the crosswalk, not the SAOM name it came from. `cycle4`
  ## (SaoMNK) rather than `cycle3`/`recip` (SAOM) is the tell.
  sc <- get_orm_scenarios()
  keys <- unique(unlist(lapply(sc, function(s) names(s$modify))))
  expect_true("cycle4" %in% keys)
  expect_false(any(c("cycle3", "recip", "gwespFF", "transTrip") %in% keys))

  ## And the modification really is a multiplier applied to the converted
  ## parameter, which is what makes `egoX = 0.0` mean "remove" and
  ## `density = 1.5` mean "50% costlier".
  b <- suppressWarnings(
    saom_to_saomnk(c(density = -1.2, cycle3 = 0.8, egoX.sic = 0.4),
                   strict = FALSE, verbose = FALSE))
  modify <- sc$double_closure$modify
  cf <- vapply(b$effects, function(e) {
    if (e$effect %in% names(modify)) e$parameter * modify[[e$effect]] else e$parameter
  }, numeric(1))
  expect_equal(cf, c(-1.2, 1.6, 0.4))
})

test_that("double_closure targets a statistic a bipartite DV actually has", {
  ## It used to modify `transTriads`, which no bipartite SaoMNK model can carry,
  ## so the scenario was a no-op dressed as an intervention.
  sc <- get_orm_scenarios()$double_closure
  expect_identical(names(sc$modify), "cycle4")
  expect_equal(sc$modify$cycle4, 2.0)
})

test_that("no shipped scenario names an effect the crosswalk cannot produce", {
  ## `double_closure` used to be keyed on `transTriads`, which no bipartite
  ## SaoMNK model can carry: the scenario could never match an effect, and
  ## run_calibrated_counterfactual() now refuses such a key outright. Every
  ## shipped scenario must therefore name something the crosswalk can emit, or
  ## the influence effect the bridge registers itself.
  producible <- c("density", "cycle4", "inPop", "inPopSqrt", "outAct",
                  "outActSqrt", "egoX", "altX", "X", "XWX",
                  "totInDist2", "simEgoInDist2")
  keys <- unique(unlist(lapply(get_orm_scenarios(), function(s) names(s$modify))))
  expect_true(all(keys %in% producible),
              info = paste("not producible:",
                           paste(setdiff(keys, producible), collapse = ", ")))
  expect_false(any(keys %in% .BRIDGE_NOT_IN_BIPARTITE))
})


# ===========================================================================
# 9. .bridge_variance_decomposition(): the two variances, kept apart
#
# Pure arithmetic, no engine. Every number here is what a table would quote as
# "the" uncertainty, and the whole point of the function is that there is no
# single such number.
# ===========================================================================

test_that("the decomposition is exactly var_between = var_mc + var_parameter", {
  means <- c(1.0, 1.4, 0.6, 1.2, 0.8)
  sds   <- c(0.30, 0.20, 0.40, 0.25, 0.35)
  n_rep <- 4L

  res <- .bridge_variance_decomposition("K_AC", means, sds, n_rep, 0.95)

  expect_equal(res$n_draws, 5L)
  expect_equal(res$n_reps, n_rep)
  expect_equal(res$mean_delta, mean(means))

  ## Hand-derived: var_mc is the mean within-draw variance of ONE replication,
  ## divided by the number of replications averaged into a draw mean.
  expect_equal(res$var_mc, mean(sds^2) / n_rep)
  expect_equal(res$var_between, stats::var(means))
  expect_equal(res$var_parameter, stats::var(means) - mean(sds^2) / n_rep)
  ## The identity, stated directly.
  expect_equal(res$var_between, res$var_mc + res$var_parameter)
  expect_false(res$parameter_var_floored)
  expect_equal(res$sd_mc, sqrt(res$var_mc))
  expect_equal(res$sd_parameter, sqrt(res$var_parameter))
  expect_equal(res$share_parameter, res$var_parameter / res$var_between)
})

test_that("Monte Carlo variance shrinks with n_reps and parameter variance does not", {
  ## The behavioral difference between the two quantities, which is the reason
  ## they must not be added into one number and quoted as "the" standard error.
  means <- c(1.0, 1.4, 0.6, 1.2, 0.8)
  sds   <- rep(0.3, 5)
  few  <- .bridge_variance_decomposition("m", means, sds, 2L,  0.95)
  many <- .bridge_variance_decomposition("m", means, sds, 50L, 0.95)

  expect_lt(many$var_mc, few$var_mc)
  expect_equal(many$var_mc, few$var_mc * 2 / 50)
  ## var_between is a property of the draws, so more replications cannot move it
  expect_equal(many$var_between, few$var_between)
  ## ... and the parameter share therefore RISES as the simulation noise falls
  expect_gt(many$share_parameter, few$share_parameter)
})

test_that("a parameter variance below the Monte Carlo floor is flagged, not negative", {
  ## Identical draws: all the between-draw movement is simulation noise, so the
  ## ANOVA estimator would go negative. It is floored at zero AND flagged.
  res <- .bridge_variance_decomposition("m", rep(1.0, 4), rep(0.5, 4), 3L, 0.95)
  expect_equal(res$var_between, 0)
  expect_gt(res$var_mc, 0)
  expect_equal(res$var_parameter, 0)
  expect_true(res$parameter_var_floored)
  expect_equal(res$sd_parameter, 0)
  ## share_parameter is undefined when there is no between-draw variance at all
  expect_true(is.na(res$share_parameter))
})

test_that("the pooled interval uses the between-draw SE, not the Monte Carlo one", {
  means <- c(1.0, 1.4, 0.6, 1.2, 0.8)
  sds   <- c(0.30, 0.20, 0.40, 0.25, 0.35)
  res <- .bridge_variance_decomposition("m", means, sds, 4L, 0.95)

  expect_equal(res$se_total, stats::sd(means) / sqrt(5))
  half <- stats::qt(0.975, 4) * res$se_total
  expect_equal(res$ci_lower, mean(means) - half)
  expect_equal(res$ci_upper, mean(means) + half)
  ## The quantile interval is over the DRAW MEANS, not over replications.
  q <- unname(stats::quantile(means, probs = c(0.025, 0.975)))
  expect_equal(c(res$q_lower, res$q_upper), q)
  ## A narrower level is narrower, so conf_level is not ignored.
  narrow <- .bridge_variance_decomposition("m", means, sds, 4L, 0.50)
  expect_gt(res$ci_upper - res$ci_lower, narrow$ci_upper - narrow$ci_lower)
})

test_that("one draw degrades to NA rather than a fabricated interval", {
  res <- .bridge_variance_decomposition("m", 1.2, 0.3, 5L, 0.95)
  expect_equal(res$n_draws, 1L)
  expect_equal(res$mean_delta, 1.2)
  for (nm in c("var_between", "var_parameter", "se_total", "ci_lower",
               "ci_upper", "q_lower", "q_upper", "sd_parameter")) {
    expect_true(is.na(res[[nm]]), info = nm)
  }
  ## var_mc is still defined: it does not need a second draw.
  expect_equal(res$var_mc, 0.09 / 5)
  ## Column set is stable, so rows for different measures rbind.
  full <- .bridge_variance_decomposition("m", c(1, 2), c(0.1, 0.2), 5L, 0.95)
  expect_identical(names(res), names(full))
  expect_equal(nrow(rbind(res, full)), 2L)
})

test_that("run_counterfactual_with_uncertainty refuses anything but a sienaFit", {
  ## The parameter half of the uncertainty comes from covtheta, which only a
  ## fit carries; accepting a converted bridge would silently report Monte
  ## Carlo error as if it were an interval on the model-implied effect.
  expect_error(
    run_counterfactual_with_uncertainty(fake_bridge_env, c(density = -1.2),
                                        verbose = FALSE),
    "needs the FIT", fixed = TRUE)
  expect_error(
    run_counterfactual_with_uncertainty(fake_bridge_env,
                                        saom_to_saomnk(c(density = -1.2),
                                                       verbose = FALSE),
                                        verbose = FALSE),
    "run_calibrated_counterfactual() directly", fixed = TRUE)
  ## Argument guards fire before any draw.
  fit <- make_siena_fit_workable()
  expect_error(run_counterfactual_with_uncertainty(fake_bridge_env, fit,
                                                   n_draws = 0, verbose = FALSE),
               "n_draws must be a positive integer", fixed = TRUE)
  expect_error(run_counterfactual_with_uncertainty(fake_bridge_env, fit,
                                                   n_reps = 0, verbose = FALSE),
               "n_reps must be a positive integer", fixed = TRUE)
  expect_error(run_counterfactual_with_uncertainty(fake_bridge_env, fit,
                                                   conf_level = 1, verbose = FALSE),
               "conf_level must be a number strictly between 0 and 1", fixed = TRUE)
})


# ===========================================================================
# 10. End to end, against the real engine
#
# The rest of this file stops at the guards. This section does not, because the
# defects it covers are precisely the ones that only appear when the engine
# runs: a W matrix that cannot load, an effect that loads but is never read,
# and parameters that never reach the simulated theta. Each run here is small
# (M = 6, N = 4, a few dozen ministeps) and takes well under a second.
# ===========================================================================

make_bridge_env_small <- function(M = 6L, N = 4L, seed = 5L) {
  set.seed(seed)
  bi <- matrix(stats::rbinom(M * N, 1, 0.35), M, N)
  bi[1, 1] <- 1L                                   # never all-zero
  W <- matrix(0.25, N, N); diag(W) <- 1
  env <- SaomNkRSienaBiEnv$new(list(
    M = M, N = N, BI_PROB = mean(bi), rand_seed = 1L,
    name = "_bridge_e2e_", dir_output = tempdir()))
  env$bipartite_matrix      <- bi
  env$bipartite_matrix_init <- bi
  list(env = env, W = W, bi_matrix = bi,
       actor_attrs     = data.frame(id = seq_len(M)),
       component_attrs = data.frame(id = seq_len(N)))
}

test_that("run_calibrated_counterfactual() completes and returns the four measures", {
  skip_if_not_installed("RSiena")
  skip_on_cran()
  be <- make_bridge_env_small()
  bp <- saom_to_saomnk(c(density = -1.0, inPop = 0.2), verbose = FALSE)

  res <- suppressWarnings(run_calibrated_counterfactual(
    be, bp, scenario = list(name = "d", modify = list(density = 3.0)),
    iterations = 20, n_reps = 2, seed = 7, verbose = FALSE))

  expect_named(res$comparison,
               c("delta_K_AC", "delta_K_CA", "delta_K_AA", "delta_K_CC"))
  expect_equal(nrow(res$summary), 4L)
  expect_equal(nrow(res$rep_deltas), 8L)
  expect_equal(nrow(res$actor_deltas), 2L * be$env$M)
  expect_equal(nrow(res$component_deltas), 2L * be$env$N)
  expect_true(all(is.finite(res$summary$mean_delta)))
  expect_equal(res$seeds, 7:8)
})

test_that("the calibrated parameters reach the simulated theta", {
  ## The regression test for the defect that made every delta identically zero:
  ## search_rsiena_multiwave_run() simulated at initialValue (zero for the
  ## structural family) instead of at the parameters asked for.
  skip_if_not_installed("RSiena")
  skip_on_cran()
  be <- make_bridge_env_small()
  bp <- saom_to_saomnk(c(density = -1.0, inPop = 0.2), verbose = FALSE)

  res <- suppressWarnings(run_calibrated_counterfactual(
    be, bp, scenario = list(name = "d", modify = list(density = 3.0)),
    iterations = 20, n_reps = 1, seed = 7, verbose = FALSE))

  th_b <- res$baseline$env$theta_matrix
  th_c <- res$counterfactual$env$theta_matrix
  expect_false(is.null(th_b))
  nm_b <- sub("_[0-9]+$", "", colnames(th_b))

  expect_true("density" %in% nm_b)
  expect_equal(unname(th_b[1, which(nm_b == "density")[1]]), -1.0)
  expect_equal(unname(th_b[1, which(nm_b == "inPop")[1]]),    0.2)
  ## and the counterfactual arm really is the modified model
  nm_c <- sub("_[0-9]+$", "", colnames(th_c))
  expect_equal(unname(th_c[1, which(nm_c == "density")[1]]), -3.0)
  expect_equal(unname(th_c[1, which(nm_c == "inPop")[1]]),    0.2)
})

test_that("a costlier density really lowers scope, so the arms are not identical", {
  skip_if_not_installed("RSiena")
  skip_on_cran()
  be <- make_bridge_env_small()
  bp <- saom_to_saomnk(c(density = -0.5, inPop = 0.1), verbose = FALSE)

  res <- suppressWarnings(run_calibrated_counterfactual(
    be, bp, scenario = list(name = "d", modify = list(density = 6.0)),
    iterations = 40, n_reps = 3, seed = 3, verbose = FALSE))

  d_ac <- res$comparison$delta_K_AC
  expect_lt(d_ac, 0)
  ## Not a rounding artefact: the change is large relative to Monte Carlo error.
  mc <- res$summary$mc_se[res$summary$measure == "K_AC"]
  expect_gt(abs(d_ac), 2 * mc)
})

test_that("the W matrix loads, and an XWX effect is registered against it", {
  ## DEFECT 1 and DEFECT 2 together. Before the fix the covariate entry carried
  ## no `effect`, so the engine walked it into the effect-inclusion chain and
  ## died on `if (eff$effect == 'Rate')` with "argument is of length zero"; and
  ## no effect referenced the matrix even when it did load.
  skip_if_not_installed("RSiena")
  skip_on_cran()
  be <- make_bridge_env_small()
  bp <- saom_to_saomnk(c(density = -1.0, XWX = 0.5), verbose = FALSE)

  res <- suppressWarnings(run_calibrated_counterfactual(
    be, bp, scenario = list(name = "d", modify = list(density = 2.0)),
    iterations = 20, n_reps = 1, seed = 7, verbose = FALSE))

  ## The covariate is in the RSiena data object, under the slot the effect names
  dyc <- names(res$baseline$env$rsiena_data$dycCovars)
  expect_true("self$component_1_coDyadCovar" %in% dyc)

  ## and an XWX effect is included, pointing at that slot
  tb  <- as.data.frame(res$baseline$env$rsiena_effects)
  inc <- tb[tb$include %in% TRUE, , drop = FALSE]
  expect_true("XWX" %in% inc$shortName)
  expect_equal(inc$interaction1[inc$shortName == "XWX"][1],
               "self$component_1_coDyadCovar")

  ## The fitted weight is carried through, and reported as fitted
  expect_equal(res$influence$weight, 0.5)
  expect_match(res$influence$weight_source, "fitted")
  expect_true(res$influence$registered)
  expect_equal(res$influence$slot, "self$component_1_coDyadCovar")
})

test_that("an XWX weight now reaches the simulated theta, and the guard stays quiet", {
  ## HISTORY: until the 2026-08-23 theta-storage repair the engine set XWX
  ## through setEffect(initialValue = ) while get_theta_matrix() read `parm`,
  ## so the weight registered and simulated at zero, and this test asserted
  ## that .bridge_check_theta() SAID so. Its own comment read: "If a future
  ## engine fixes the branch, this test fails and the warning can go." The
  ## engine now carries theta in `initialValue` throughout, so the assertion
  ## is inverted: the weight must reach the simulated theta, and the guard
  ## (which stays, as a drift detector) must find nothing.
  skip_if_not_installed("RSiena")
  skip_on_cran()
  be <- make_bridge_env_small()
  bp <- saom_to_saomnk(c(density = -1.0, XWX = 0.5), verbose = FALSE)

  .warned <- character(0)
  res <- withCallingHandlers(
    run_calibrated_counterfactual(
      be, bp, scenario = list(name = "d", modify = list(density = 2.0)),
      iterations = 20, n_reps = 1, seed = 7, verbose = FALSE),
    warning = function(cnd) {
      .warned <<- c(.warned, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })

  expect_false(any(grepl("NOT simulated at the parameter", .warned, fixed = TRUE)))
  th <- res$baseline$env$theta_matrix
  nm <- sub("_[0-9]+$", "", colnames(th))
  expect_true("XWX" %in% nm)
  expect_equal(unname(th[1, which(nm == "XWX")[1]]), 0.5)
})

test_that("with no fitted XWX the weight is 0, said out loud, and never invented", {
  skip_if_not_installed("RSiena")
  skip_on_cran()
  be <- make_bridge_env_small()
  bp <- saom_to_saomnk(c(density = -1.0), verbose = FALSE)

  res <- suppressWarnings(run_calibrated_counterfactual(
    be, bp, scenario = list(name = "d", modify = list(density = 2.0)),
    iterations = 15, n_reps = 1, seed = 7, verbose = FALSE))

  expect_equal(res$influence$weight, 0)
  expect_match(res$influence$weight_source, "not invented", fixed = TRUE)
  expect_match(res$influence$note, "switched OFF in both arms", fixed = TRUE)

  ## and multiplying a zero weight is flagged as the no-op it is
  expect_warning(
    run_calibrated_counterfactual(
      be, bp, scenario = get_orm_scenarios()$epistasis_boost,
      iterations = 15, n_reps = 1, seed = 7, verbose = FALSE),
    "cannot change anything", fixed = TRUE)
})

test_that("run_counterfactual_with_uncertainty() separates the two variances", {
  skip_if_not_installed("RSiena")
  skip_on_cran()
  be  <- make_bridge_env_small()
  fit <- make_siena_fit_workable(
    theta      = c(3.0, -1.0, 0.2),
    short_name = c("Rate", "density", "inPop"),
    type       = c("rate", "eval", "eval"),
    covtheta   = diag(c(0.40, 0.25, 0.04)))

  out <- suppressWarnings(run_counterfactual_with_uncertainty(
    be, fit, scenario = list(name = "d", modify = list(density = 2.0)),
    n_draws = 3, n_reps = 2, iterations = 15, seed = 100, verbose = FALSE))

  expect_equal(out$n_draws, 3L)
  expect_equal(out$n_reps, 2L)
  expect_equal(nrow(out$summary), 4L)
  expect_equal(nrow(out$draw_deltas), 12L)       # 3 draws x 4 measures
  expect_equal(nrow(out$rep_deltas), 24L)        # 3 draws x 2 reps x 4 measures
  expect_equal(dim(out$theta_draws), c(3L, 3L))
  expect_null(out$runs)                          # keep_runs defaults to FALSE

  ## Each draw got its own block of replication seeds, so the draws are
  ## independent and the decomposition is defined.
  expect_equal(sort(unique(out$rep_deltas$seed)), 100:105)

  ## The two variances are both reported, and separately.
  expect_true(all(c("var_mc", "var_parameter", "var_between",
                    "sd_mc", "sd_parameter") %in% names(out$summary)))
  ok <- out$summary$parameter_var_floored |
    abs(out$summary$var_between -
          (out$summary$var_mc + out$summary$var_parameter)) < 1e-9
  expect_true(all(ok))

  ## The theta draws really are draws, not the point estimate repeated.
  expect_gt(stats::sd(out$theta_draws[, 2]), 0)

  ## The note that stops them being conflated travels with the result.
  expect_match(out$uncertainty_note, "different quantities", fixed = TRUE)
  expect_match(out$caveat, "INITIALISATION only", fixed = TRUE)
})
