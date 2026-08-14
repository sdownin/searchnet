###############################################################################
## test-theta-ramp.R
##
## Continuous rate-of-environmental-change operators: saomnk_theta_ramp() and
## saomnk_theta_drift().
###############################################################################

## Small, fast fixture shared by the tests below.
make_ramp_fixture <- function(M = 4L, N = 6L, seed = 1234L) {
  env <- saomnk_env(M = M, N = N, seed = seed)
  mod <- saomnk_model(density = -0.5,
                      influence_matrix = saomnk_block_diagonal(N, 2))
  list(env = env, mod = mod)
}


test_that("prepare_theta_scaffold returns a correctly shaped, named matrix", {
  fx <- make_ramp_fixture()
  tm <- fx$env$prepare_theta_scaffold(fx$mod, iterations = 40L)

  expect_true(is.matrix(tm))
  expect_equal(nrow(tm), 40L)
  expect_true(!is.null(colnames(tm)))
  expect_true("density" %in% colnames(tm))
  ## Default matrix is constant down every column: it is the no-change baseline.
  expect_true(all(apply(tm, 2, function(x) length(unique(x)) == 1L)))
})


test_that("a linear ramp is monotone, the right length, and hits both endpoints", {
  fx <- make_ramp_fixture()
  n <- 120L
  tm <- saomnk_theta_ramp(fx$env, fx$mod, iterations = n,
                          changes = list(list(effect = "density",
                                              from = 0.5, to = -0.5,
                                              start = 0.25, window = 0.5)),
                          easing = "linear")

  expect_equal(nrow(tm), n)
  col <- tm[, "density"]

  ## Monotone non-increasing over the whole chain (from > to).
  expect_true(all(diff(col) <= 1e-12))
  ## Endpoints exactly as specified, and flat outside the window.
  expect_equal(unname(col[1L]), 0.5)
  expect_equal(unname(col[n]), -0.5)
  expect_equal(unname(col[floor(0.25 * n)]), 0.5)
  expect_equal(unname(col[n - 1L]), -0.5)
  ## Strictly decreasing somewhere in the middle: the ramp actually ramps.
  mid <- col[floor(0.3 * n):floor(0.7 * n)]
  expect_true(any(diff(mid) < 0))
})


test_that("an upward ramp is monotone non-decreasing", {
  fx <- make_ramp_fixture()
  tm <- saomnk_theta_ramp(fx$env, fx$mod, iterations = 80L,
                          changes = list(list(effect = "density",
                                              from = -1, to = 1)))
  expect_true(all(diff(tm[, "density"]) >= -1e-12))
  expect_equal(unname(tm[1L, "density"]), -1)
  expect_equal(unname(tm[80L, "density"]), 1)
})


test_that("sigmoid and exponential easings differ from linear but share endpoints", {
  fx <- make_ramp_fixture()
  n <- 120L
  spec <- list(list(effect = "density", from = 0.5, to = -0.5,
                    start = 0.2, window = 0.6))

  lin <- saomnk_theta_ramp(fx$env, fx$mod, n, spec, easing = "linear")[, "density"]
  sig <- saomnk_theta_ramp(fx$env, fx$mod, n, spec, easing = "sigmoid")[, "density"]
  exp_ <- saomnk_theta_ramp(fx$env, fx$mod, n, spec, easing = "exponential")[, "density"]

  ## Different trajectories ...
  expect_false(isTRUE(all.equal(sig, lin)))
  expect_false(isTRUE(all.equal(exp_, lin)))
  expect_false(isTRUE(all.equal(sig, exp_)))

  ## ... but the same endpoints. The raw logistic does NOT start at 0, so an
  ## unrescaled sigmoid would jump by ~5% of the range at each end; this is the
  ## regression guard for that rescaling.
  for (v in list(lin, sig, exp_)) {
    expect_equal(unname(v[1L]), 0.5)
    expect_equal(unname(v[n]), -0.5)
  }

  ## All three remain monotone.
  for (v in list(lin, sig, exp_)) expect_true(all(diff(v) <= 1e-12))

  ## Sigmoid is slower than linear at the start of the window (S shape).
  w1 <- floor(0.2 * n) + 2L
  expect_gt(unname(sig[w1]), unname(lin[w1]))
})


test_that("ramp defaults `from` to the model's own parameter value", {
  fx <- make_ramp_fixture()
  base <- fx$env$prepare_theta_scaffold(fx$mod, 50L)
  tm <- saomnk_theta_ramp(fx$env, fx$mod, 50L,
                          changes = list(list(effect = "density", to = -2)))
  expect_equal(unname(tm[1L, "density"]), unname(base[1L, "density"]))
  expect_equal(unname(tm[50L, "density"]), -2)
})


test_that("ramp leaves other columns untouched", {
  fx <- make_ramp_fixture()
  base <- fx$env$prepare_theta_scaffold(fx$mod, 50L)
  tm <- saomnk_theta_ramp(fx$env, fx$mod, 50L,
                          changes = list(list(effect = "density", from = 1, to = -1)))
  others <- setdiff(colnames(tm), "density")
  skip_if(length(others) == 0L, "model has a single effect")
  expect_equal(tm[, others, drop = FALSE], base[, others, drop = FALSE])
})


test_that("ramp errors informatively on an unknown effect", {
  fx <- make_ramp_fixture()
  expect_error(
    saomnk_theta_ramp(fx$env, fx$mod, 20L,
                      changes = list(list(effect = "no_such_effect", to = 1))),
    "not in the theta matrix"
  )
})


test_that("ramp validates its change specifications", {
  fx <- make_ramp_fixture()
  expect_error(saomnk_theta_ramp(fx$env, fx$mod, 20L, changes = list()),
               "non-empty list")
  expect_error(saomnk_theta_ramp(fx$env, fx$mod, 20L,
                                 changes = list(list(effect = "density"))),
               "no `to` value")
  expect_error(saomnk_theta_ramp(fx$env, fx$mod, 20L,
                                 changes = list(list(effect = "density", to = 1,
                                                     start = 0.8, window = 0.5))),
               "exceeds 1")
  expect_error(saomnk_theta_ramp(fx$env, fx$mod, 20L,
                                 changes = list(list(effect = "density", to = 1,
                                                     start = 1.2))),
               "`start` must be in")
})


test_that("drift is exactly reproducible under a fixed seed and varies with it", {
  fx <- make_ramp_fixture()
  d1 <- saomnk_theta_drift(fx$env, fx$mod, 100L, effect = "density",
                           sd = 0.05, seed = 7L)
  d2 <- saomnk_theta_drift(fx$env, fx$mod, 100L, effect = "density",
                           sd = 0.05, seed = 7L)
  d3 <- saomnk_theta_drift(fx$env, fx$mod, 100L, effect = "density",
                           sd = 0.05, seed = 8L)

  expect_identical(d1, d2)
  expect_false(isTRUE(all.equal(d1[, "density"], d3[, "density"])))
  expect_equal(nrow(d1), 100L)
})


test_that("drift starts at the model value and actually wanders", {
  fx <- make_ramp_fixture()
  base <- fx$env$prepare_theta_scaffold(fx$mod, 100L)
  d <- saomnk_theta_drift(fx$env, fx$mod, 100L, effect = "density",
                          sd = 0.05, seed = 11L)
  expect_equal(unname(d[1L, "density"]), unname(base[1L, "density"]))
  expect_gt(stats::sd(d[, "density"]), 0)
  ## A drift is NOT monotone: that is the whole point of it being a random walk
  ## rather than a ramp.
  dd <- diff(d[, "density"])
  expect_true(any(dd > 0) && any(dd < 0))
})


test_that("drift does not disturb the caller's RNG stream", {
  fx <- make_ramp_fixture()
  set.seed(4242)
  before <- stats::runif(3)
  set.seed(4242)
  invisible(saomnk_theta_drift(fx$env, fx$mod, 50L, effect = "density",
                               sd = 0.05, seed = 999L))
  after <- stats::runif(3)
  expect_equal(before, after)
})


test_that("drift respects bounds and validates arguments", {
  fx <- make_ramp_fixture()
  d <- saomnk_theta_drift(fx$env, fx$mod, 200L, effect = "density",
                          sd = 0.5, seed = 3L, bounds = c(-1, 1))
  expect_true(all(d[, "density"] >= -1 & d[, "density"] <= 1))

  expect_error(saomnk_theta_drift(fx$env, fx$mod, 20L, effect = "density", sd = 0),
               "positive number")
  expect_error(saomnk_theta_drift(fx$env, fx$mod, 20L, effect = "nope", sd = 0.1),
               "not in the theta matrix")
})


test_that("ramp and drift compose through the theta_matrix argument", {
  fx <- make_ramp_fixture()
  n <- 150L
  tm <- saomnk_theta_ramp(fx$env, fx$mod, n,
                          changes = list(list(effect = "density",
                                              from = 0.5, to = -0.5)))
  tm2 <- saomnk_theta_drift(NULL, NULL, n, effect = "density", sd = 0.02,
                            seed = 5L, add = TRUE, theta_matrix = tm)

  expect_equal(dim(tm2), dim(tm))
  ## The composed series is no longer monotone, but the ramp still dominates:
  ## it trends downwards, which is what "eroding AND unstable" has to look like.
  expect_false(all(diff(tm2[, "density"]) <= 0))
  expect_lt(mean(tm2[floor(0.8 * n):n, "density"]),
            mean(tm2[1:floor(0.2 * n), "density"]))
  ## add = FALSE would have discarded the ramp entirely; check it differs.
  tm3 <- saomnk_theta_drift(NULL, NULL, n, effect = "density", sd = 0.02,
                            seed = 5L, start = NA, theta_matrix = tm)
  expect_false(isTRUE(all.equal(tm2[, "density"], tm3[, "density"])))
})


test_that("saomnk_run forwards theta_matrix and the run uses it", {
  ## skip_on_cran() removed 2026-08-14: the package is not on CRAN, and the
  ## gate meant these never ran locally either (bare test_dir() does not set
  ## NOT_CRAN), so the feature shipped with zero executed verification. If a
  ## CRAN submission happens, re-gate at that point with the cost understood.
  fx <- make_ramp_fixture(M = 4L, N = 6L)
  n <- 60L
  tm <- saomnk_theta_ramp(fx$env, fx$mod, n,
                          changes = list(list(effect = "density",
                                              from = 0.5, to = -1.5)))

  expect_silent_run <- try(saomnk_run(fx$env, fx$mod, theta_matrix = tm, seed = 99L),
                           silent = TRUE)
  expect_false(inherits(expect_silent_run, "try-error"))

  ## The engine stored exactly the matrix it was given ...
  expect_equal(nrow(fx$env$theta_matrix), n)
  expect_equal(unname(fx$env$theta_matrix[, "density"]), unname(tm[, "density"]))
  ## ... and RSiena used a distinct theta on each of the n runs.
  tu <- fx$env$rsiena_model$thetaUsed
  expect_equal(nrow(tu), n)
  expect_gt(length(unique(tu[, which(colnames(tm) == "density")])), 1L)
})


test_that("saomnk_run rejects a non-matrix theta_matrix", {
  fx <- make_ramp_fixture()
  expect_error(saomnk_run(fx$env, fx$mod, theta_matrix = 1:10),
               "numeric matrix")
})
