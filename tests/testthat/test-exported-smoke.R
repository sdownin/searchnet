###############################################################################
## test-exported-smoke.R
##
## Smoke and contract tests for exported functions that had little or no
## direct coverage: return type, names and columns, input validation, and
## determinism under a fixed seed. Synthetic data only.
##
##   searchnet_synth, searchnet_rd         (shocked SAOM-NK panel; Synth, rdrobust)
##   searchnet_chain_gap                   (pure; synthetic chain-stats frames)
##   searchnet_chain_null_model            (tiny RSiena bipartite fit)
##   saomnk_monte_carlo                    (M = 4, N = 6, 2 replications)
##   saomnk_confirm                        (the man page example)
##   boundary_screen                       (pure; arrays and lists)
##   gof_battery                           (tiny RSiena fit, returnDeps = TRUE)
##
## Anything that runs an RSiena estimation or a Synth optimization is behind
## skip_on_cran(); fixtures are built lazily inside a test and cached.
###############################################################################

.smoke_cache <- new.env(parent = emptyenv())

.smoke_quiet <- function(expr) {
  suppressMessages(suppressWarnings(
    utils::capture.output(val <- force(expr))))
  val
}


# ---------------------------------------------------------------------------
# 1. searchnet_synth and searchnet_rd
# ---------------------------------------------------------------------------

## A small shocked run (M = 6, N = 8): density shock halfway through, actor 1
## treated, five controls.
smoke_causal_panel <- function() {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  if (!is.null(.smoke_cache$panel)) return(.smoke_cache$panel)
  env <- saomnk_env(M = 6, N = 8, seed = 1)
  mod <- saomnk_model(density = -0.5, popularity = 0.15,
                      influence_matrix = saomnk_block_diagonal(8, 2))
  s1 <- saomnk_shock("density", parameter = -0.5, portion = 1)
  s2 <- saomnk_shock("density", parameter = -2.0, portion = 1)
  .smoke_quiet(saomnk_run(env, mod, steps_per_actor = 10, seed = 3,
                          shocks = list(s1, s2)))
  shock_step <- round(max(env$actor_util_df$chain_step_id) / 2)
  panel <- searchnet_causal_panel(env, shock_step = shock_step,
                                  treated_actors = 1)
  .smoke_cache$panel <- panel
  panel
}

test_that("searchnet_synth returns synth output and a gap frame aligned to the panel steps", {
  skip_if_not_installed("Synth")
  panel <- smoke_causal_panel()
  sc <- .smoke_quiet(searchnet_synth(panel, treated_unit = 1))

  expect_type(sc, "list")
  expect_named(sc, c("synth_out", "dataprep_out", "gap"))
  expect_s3_class(sc$gap, "data.frame")
  expect_named(sc$gap, c("step", "treated", "synthetic", "gap"))
  expect_identical(sc$gap$step, sort(unique(panel$step)))
  expect_equal(sc$gap$gap, sc$gap$treated - sc$gap$synthetic)
  ## The treated series is actor 1's own outcome path.
  a1 <- panel[panel$actor_id == "1", ]
  expect_equal(sc$gap$treated, a1$outcome[order(a1$step)])
  ## Donor weights are a convex combination over the five controls.
  w <- as.numeric(sc$synth_out$solution.w)
  expect_length(w, 5L)
  expect_true(all(w >= -1e-8))
  expect_equal(sum(w), 1, tolerance = 1e-3)

  ## Deterministic: the same panel gives the same gap.
  sc2 <- .smoke_quiet(searchnet_synth(panel, treated_unit = 1))
  expect_equal(sc2$gap, sc$gap)
})

test_that("searchnet_synth validates its panel", {
  skip_if_not_installed("Synth")
  panel <- smoke_causal_panel()
  expect_error(searchnet_synth(panel[, setdiff(names(panel), "shock_step")],
                               treated_unit = 1),
               "missing required columns: shock_step")
  ## Five treated actors leave one control: refused.
  p1 <- panel
  p1$treated <- ifelse(p1$actor_id %in% as.character(1:5), 1L, 0L)
  expect_error(searchnet_synth(p1, treated_unit = 1), "at least 2 control units")
  ## Only one pre-treatment step requested as a predictor.
  expect_error(searchnet_synth(panel, treated_unit = 1, predictors = 1),
               "at least 2 pre-treatment steps")
})

test_that("searchnet_rd aggregates by step and returns an rdrobust fit at the shock step", {
  skip_if_not_installed("rdrobust")
  panel <- smoke_causal_panel()
  rd <- searchnet_rd(panel)

  expect_named(rd, c("rd", "agg_data", "shock_step", "design"))
  expect_s3_class(rd$rd, "rdrobust")
  expect_identical(rd$design, "time")
  expect_identical(rd$shock_step, panel$shock_step[1])
  expect_named(rd$agg_data, c("step", "mean_outcome"))
  expect_equal(nrow(rd$agg_data), length(unique(panel$step)))
  expect_equal(rd$agg_data$mean_outcome,
               as.numeric(tapply(panel$outcome, panel$step, mean)))
  expect_equal(rd$rd$c, panel$shock_step[1])

  rd2 <- searchnet_rd(panel)
  expect_equal(rd2$rd$coef, rd$rd$coef)
  expect_error(searchnet_rd(panel[, c("step", "outcome")]),
               "missing required columns: shock_step")
})


# ---------------------------------------------------------------------------
# 2. searchnet_chain_gap
# ---------------------------------------------------------------------------

## One row per chain, so the per-event normalization divides by 1 and the
## per-chain summary is the value itself. `near` centers on the observed
## value 15.5 (p_mc = 1); `far` sits about 1000 away (p_mc = 1 / 31).
near_vals <- function(n = 30) as.numeric(seq_len(n))
far_vals  <- function(n = 30) as.numeric(1000 + seq_len(n))

make_arm <- function(clustering, focusing, reinforcing, mixing) {
  data.frame(chain_id = seq_along(clustering), clustering = clustering,
             focusing = focusing, reinforcing = reinforcing, mixing = mixing,
             stringsAsFactors = FALSE)
}
obs_frame <- data.frame(chain_id = 1L, clustering = 15.5, focusing = 15.5,
                        reinforcing = 15.5, mixing = 15.5)

## Focal covers clustering and focusing; null covers focusing and reinforcing.
focal_arm <- make_arm(near_vals(), near_vals(), far_vals(), far_vals())
null_arm  <- make_arm(far_vals(),  near_vals(), near_vals(), far_vals())

test_that("searchnet_chain_gap assigns each of the four verdicts from coverage", {
  g <- searchnet_chain_gap(focal_arm, null_arm, obs_frame)

  expect_s3_class(g, "searchnet_chain_gap")
  expect_s3_class(g, "data.frame")
  expect_named(g, c("statistic", "observed", "focal_mean", "null_mean",
                    "p_mc_focal", "p_mc_null", "z_focal", "z_null", "gap",
                    "verdict", "fitted"))
  v <- setNames(g$verdict, g$statistic)
  expect_identical(v[["clustering"]],  "informative")       # null fails, focal covers
  expect_identical(v[["focusing"]],    "undiscriminating")  # both cover
  expect_identical(v[["reinforcing"]], "focal_fails")       # null covers, focal fails
  expect_identical(v[["mixing"]],      "both_fail")
  expect_identical(attr(g, "alpha"), 0.05)
  expect_true(all(g$p_mc_focal[g$statistic %in% c("clustering", "focusing")] == 1))
  expect_equal(g$p_mc_null[g$statistic == "clustering"], 1 / 31)
})

test_that("gap is |z_null| - |z_focal| and matches the two compare arms", {
  g <- searchnet_chain_gap(focal_arm, null_arm, obs_frame)
  f <- suppressWarnings(searchnet_chain_compare(focal_arm, obs_frame))
  n <- suppressWarnings(searchnet_chain_compare(null_arm, obs_frame))
  expect_equal(g$gap, abs(g$z_null) - abs(g$z_focal))
  expect_equal(g$z_focal, f$z)
  expect_equal(g$z_null, n$z)
  expect_equal(g$p_mc_focal, f$p_mc)
  expect_equal(g$focal_mean, f$sim_mean)
  expect_equal(g$null_mean, n$sim_mean)
  ## Positive exactly where the focal model is closer to the observed log.
  expect_gt(g$gap[g$statistic == "clustering"], 0)
  expect_lt(g$gap[g$statistic == "reinforcing"], 0)
})

test_that("alpha sets the coverage threshold and is carried as an attribute", {
  ## p_mc for a `far` arm is 1/31 = 0.032: a miss at 0.05, a cover at 0.01.
  g <- searchnet_chain_gap(focal_arm, null_arm, obs_frame, alpha = 0.01)
  expect_identical(attr(g, "alpha"), 0.01)
  expect_true(all(g$verdict == "undiscriminating"))
  ## `...` reaches both arms: restricting stats restricts the rows.
  g2 <- searchnet_chain_gap(focal_arm, null_arm, obs_frame, stats = "clustering")
  expect_identical(g2$statistic, "clustering")
  expect_output(print(g2), "informative")
})


# ---------------------------------------------------------------------------
# 3. searchnet_chain_null_model and gof_battery (RSiena fits)
# ---------------------------------------------------------------------------

## Bipartite panel with real wave-to-wave movement, as in test-chain-from-fit.R.
.smoke_panel <- function(M, N, W, seed, p_init = 0.30, p_flip = 0.15) {
  set.seed(seed)
  arr <- array(0L, c(M, N, W))
  arr[, , 1] <- matrix(as.integer(stats::runif(M * N) < p_init), M, N)
  for (w in 2:W) {
    prev <- arr[, , w - 1]
    flip <- matrix(stats::runif(M * N) < p_flip, M, N)
    prev[flip] <- 1L - prev[flip]
    arr[, , w] <- prev
  }
  arr
}

smoke_siena_data <- function() {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  if (!is.null(.smoke_cache$dat)) return(.smoke_cache$dat)
  arr <- .smoke_panel(10, 6, 3, seed = 4242)
  actors <- RSiena::sienaNodeSet(10, nodeSetName = "actors")
  comps  <- RSiena::sienaNodeSet(6, nodeSetName = "comps")
  mynet  <- RSiena::sienaDependent(arr, type = "bipartite",
                                   nodeSet = c("actors", "comps"))
  dat <- RSiena::sienaDataCreate(mynet, nodeSets = list(actors, comps))
  .smoke_cache$dat <- dat
  dat
}

test_that("searchnet_chain_null_model fits a rate + density sienaFit with chains", {
  dat <- smoke_siena_data()
  fit <- tryCatch(.smoke_quiet(searchnet_chain_null_model(dat, seed = 7L, n3 = 10L,
                                                          useCluster = FALSE)),
                  error = function(e) skip(paste("RSiena fit failed:", e$message)))
  expect_s3_class(fit, "sienaFit")
  expect_false(is.null(fit$chain))
  inc <- fit$effects[fit$effects$include, ]
  expect_true(all(inc$shortName %in% c("Rate", "density")))
  expect_true("density" %in% inc$shortName)
  expect_false(isTRUE(fit$cond))

  ## Something that is not siena data is refused (by getEffects()).
  expect_error(searchnet_chain_null_model(list()))
})

smoke_gof_fit <- function() {
  dat <- smoke_siena_data()
  if (!is.null(.smoke_cache$gof_fit)) return(.smoke_cache$gof_fit)
  fit <- tryCatch(.smoke_quiet({
    alg <- RSiena::sienaAlgorithmCreate(projname = NULL, nsub = 1, n3 = 50,
                                        seed = 31415, cond = FALSE)
    RSiena::siena07(alg, data = dat, effects = RSiena::getEffects(dat),
                    returnDeps = TRUE, batch = TRUE, silent = TRUE,
                    useCluster = FALSE)
  }), error = function(e) skip(paste("RSiena fit failed:", e$message)))
  .smoke_cache$gof_fit <- fit
  fit
}

test_that("gof_battery returns five fixed rows with p, mhd and a verdict", {
  fit <- smoke_gof_fit()
  gb <- .smoke_quiet(gof_battery(fit, varName = "mynet", verbose = FALSE))

  expect_s3_class(gb, "data.frame")
  expect_named(gb, c("statistic", "p", "mhd", "verdict"))
  expect_equal(nrow(gb), 5L)
  expect_identical(gb$statistic,
                   c("Actor scope (outdegree)", "Component popularity (indegree)",
                     "Actor co-affiliation (XX')", "Component co-presence (X'X)",
                     "Tie volume and degree dispersion"))
  ok <- !is.na(gb$p)
  expect_true(all(gb$p[ok] >= 0 & gb$p[ok] <= 1))
  expect_true(all(gb$verdict[ok] %in% c("fits", "misfit")))
  expect_true(all(startsWith(gb$verdict[!ok], "NOT COMPUTED")))
  expect_identical(gb$verdict[ok], ifelse(gb$p[ok] >= 0.05, "fits", "misfit"))
  g <- attr(gb, "gof")
  expect_type(g, "list")
  expect_identical(names(g)[ok], gb$statistic[ok])
  ## At least the two standard degree distributions are computable here.
  expect_true(all(ok[1:2]))
})

test_that("gof_battery refuses a non-fit and a fit without simulated networks", {
  expect_error(gof_battery(list()), "must be a sienaFit")
  fake <- structure(list(sims = NULL), class = "sienaFit")
  expect_error(gof_battery(fake), "returnDeps = TRUE")
})


# ---------------------------------------------------------------------------
# 4. saomnk_monte_carlo
# ---------------------------------------------------------------------------

run_mc <- function(seed) {
  env <- saomnk_env(M = 4, N = 6, seed = 2)
  mod <- saomnk_model(density = -0.5)
  out <- .smoke_quiet(saomnk_monte_carlo(env, mod, replications = 2, waves = 2,
                                         iterations = 20, seed = seed))
  list(env = env, out = out)
}

test_that("saomnk_monte_carlo fills mc_results with one record per replication", {
  skip_if_not_installed("RSiena")
  r <- run_mc(9)
  expect_identical(r$out, r$env)               # returns the env, modified in place
  mc <- r$env$mc_results
  expect_length(mc, 2L)
  for (k in 1:2) {
    expect_named(mc[[k]], c("rep_id", "seed", "bipartite_final",
                            "bipartite_waves", "rsiena_model"))
    expect_identical(mc[[k]]$rep_id, k)
    expect_equal(dim(mc[[k]]$bipartite_final), c(4L, 6L))
    expect_true(all(mc[[k]]$bipartite_final %in% c(0, 1)))
    expect_length(mc[[k]]$bipartite_waves, 2L)
  }
  ## Replication seeds are distinct and recorded in the provenance.
  expect_false(identical(mc[[1]]$seed, mc[[2]]$seed))
  expect_false(is.null(r$env$provenance))
})

test_that("saomnk_monte_carlo is reproducible under a fixed seed", {
  skip_if_not_installed("RSiena")
  a <- run_mc(9)$env$mc_results
  b <- run_mc(9)$env$mc_results
  c <- run_mc(10)$env$mc_results
  for (k in 1:2) {
    expect_identical(a[[k]]$seed, b[[k]]$seed)
    expect_identical(a[[k]]$bipartite_final, b[[k]]$bipartite_final)
    expect_identical(a[[k]]$bipartite_waves, b[[k]]$bipartite_waves)
  }
  expect_false(identical(a[[1]]$seed, c[[1]]$seed))
  expect_error(saomnk_monte_carlo(list(), saomnk_model(density = -0.5)))
})


# ---------------------------------------------------------------------------
# 5. saomnk_confirm
# ---------------------------------------------------------------------------

## The man/saomnk_confirm.Rd example environment.
confirm_env <- function() {
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 4, N = 6, seed = 42)
  mod <- saomnk_model(density = -0.5, popularity = 0.2)
  .smoke_quiet(saomnk_run(env, mod, steps_per_actor = 5, seed = 12345))
  env
}

test_that("saomnk_confirm keeps a subset of proposals and writes it back to the env", {
  env <- confirm_env()
  prop0 <- unname(1 * (as.matrix(env$bipartite_matrix) > 0))
  quality <- c(1, 1, 0, 0)
  a <- saomnk_assent(actor_attribute = quality, rate_high = 0.8, rate_low = 0.2)
  cf <- saomnk_confirm(env, a, seed = 1)

  expect_named(cf, c("proposal", "confirmed", "diagnostics"))
  expect_equal(cf$proposal, prop0)
  expect_true(all(cf$confirmed <= cf$proposal))
  expect_true(all(cf$confirmed %in% c(0, 1)))
  expect_equal(unname(as.matrix(env$bipartite_matrix)), cf$confirmed)
  d <- cf$diagnostics
  expect_equal(d$proposals, sum(cf$proposal))
  expect_equal(d$confirmed, sum(cf$confirmed))
  expect_named(d$confirmation_rate_by_group, c("high", "low"))
  expect_named(d$attribute_degree_cor, c("proposal", "confirmed"))
})

test_that("confirmation rate 1 keeps every proposal and rate 0 keeps none", {
  env <- confirm_env()
  prop0 <- unname(1 * (as.matrix(env$bipartite_matrix) > 0))
  skip_if(sum(prop0) == 0, "example run produced no proposals")

  all_in <- saomnk_confirm(env, saomnk_assent(prob = 1), seed = 1)
  expect_equal(all_in$confirmed, prop0)
  expect_equal(all_in$diagnostics$confirmation_rate, 1)

  none <- saomnk_confirm(env, saomnk_assent(prob = 0), seed = 1)
  expect_true(all(none$confirmed == 0))
  expect_equal(none$diagnostics$confirmation_rate, 0)
  expect_true(all(as.matrix(env$bipartite_matrix) == 0))
})

test_that("saomnk_confirm is reproducible with a seed and validates its inputs", {
  env <- confirm_env()
  prop0 <- env$bipartite_matrix
  a <- saomnk_assent(prob = 0.5)
  c1 <- saomnk_confirm(env, a, seed = 11)
  env$bipartite_matrix <- prop0
  c2 <- saomnk_confirm(env, a, seed = 11)
  expect_identical(c1$confirmed, c2$confirmed)

  env$bipartite_matrix <- prop0
  expect_error(saomnk_confirm(env, list(prob = 0.5)))
  expect_error(saomnk_confirm(env, saomnk_assent(prob = matrix(0.5, 2, 2))),
               "matrix is 2x2")
  expect_error(saomnk_assent(), "supply either")
  expect_error(saomnk_assent(prob = 0.5, component_selectivity = 1), "\\[0, 1\\)")
})


# ---------------------------------------------------------------------------
# 6. boundary_screen
# ---------------------------------------------------------------------------

test_that("boundary_screen gives the same result for an array and a list of waves", {
  set.seed(1)
  arr <- array(rbinom(12 * 5 * 3, 1, 0.3), c(12, 5, 3))
  lst <- lapply(1:3, function(t) arr[, , t])
  a <- boundary_screen(arr)
  expect_identical(boundary_screen(lst), a)

  expect_s3_class(a, "data.frame")
  expect_named(a, c("effect", "observed", "attainable_min", "attainable_max",
                    "position", "estimable", "verdict"))
  expect_identical(a$effect, c("outIso", "outTrunc1", "outThreshold1", "in2Plus"))
  expect_true(all(a$position >= 0 & a$position <= 1))
  expect_identical(a$estimable, !(a$position %in% c(0, 1)))
  ## Hand-computed observed values.
  deg <- apply(arr, c(1, 3), sum)
  expect_equal(a$observed, c(sum(deg == 0), sum(pmin(deg, 1)), sum(deg >= 1),
                             sum(pmax(deg - 1, 0))))
})

test_that("degenerate panels put effects on the boundary", {
  ## All zero: every actor-wave is isolated.
  z <- boundary_screen(array(0, c(5, 4, 2)))
  expect_equal(z$position, c(1, 0, 0, 0))
  expect_true(!any(z$estimable))
  expect_true(all(z$verdict == "SATURATED: cannot converge"))

  ## All one: no isolates, every actor-wave holds a tie, in2Plus at its max.
  o <- boundary_screen(array(1, c(5, 4, 2)))
  expect_equal(o$position, c(0, 1, 1, 1))
  expect_true(!any(o$estimable))

  ## Balanced panel: every actor holds a tie in every wave, so outIso,
  ## outTrunc1 and outThreshold1 saturate while in2Plus stays interior.
  set.seed(3)
  arr <- array(rbinom(8 * 5 * 2, 1, 0.4), c(8, 5, 2))
  for (t in 1:2) for (i in 1:8) arr[i, 1 + (i %% 5), t] <- 1
  b <- boundary_screen(arr)
  expect_identical(b$estimable, c(FALSE, FALSE, FALSE, TRUE))
})

test_that("boundary_screen rejects non-binary, NA, wrong-rank and ragged input", {
  expect_error(boundary_screen(array(2, c(3, 3, 2))), "binary")
  expect_error(boundary_screen(array(c(NA, 0), c(3, 3, 2))), "NA")
  expect_error(boundary_screen(matrix(0, 3, 3)), "3 dimensions")
  expect_error(boundary_screen(list()), "empty list")
  expect_error(boundary_screen(list(matrix(0, 3, 3), matrix(0, 2, 3))),
               "identical dimensions")
})
