###############################################################################
## test-moment-gate.R
## searchnet_moment_gate(): a simulated world is judged by the distance of its
## MEAN from the observed moments, against a tolerance stated in advance; a
## world parameterized from RSiena default starting values is refused.
## Synthetic data only.
###############################################################################

## A synthetic panel: M x N x W, first period Bernoulli(p), each later period
## redraws a share `churn` of the cells from Bernoulli(p).
mg_panel <- function(p, W = 3, M = 40, N = 30, churn = 0.1) {
  B <- array(0L, c(M, N, W))
  B[, , 1] <- stats::rbinom(M * N, 1, p)
  for (w in seq_len(W)[-1L]) {
    redraw <- stats::rbinom(M * N, 1, churn) == 1
    b <- B[, , w - 1]
    b[redraw] <- stats::rbinom(sum(redraw), 1, p)
    B[, , w] <- b
  }
  B
}

mg_tol <- list(density = 0.02, mean_K_AC = c(rel = 0.10),
               mean_K_CA = c(rel = 0.10), sd_K_AC = c(rel = 0.30),
               sd_K_CA = c(rel = 0.30), change_rate = 0.02)

test_that("a world simulated to match the observed panel passes", {
  set.seed(101)
  obs <- mg_panel(0.20)
  sims <- lapply(1:30, function(r) mg_panel(0.20))
  g <- searchnet_moment_gate(obs, sims, tolerance = mg_tol,
                             theta_source = "rates_density_fit")
  expect_s3_class(g, "searchnet_moment_gate")
  expect_true(g$pass)
  expect_identical(g$verdict, "PASS")
  expect_identical(g$n_reps, 30L)
  expect_named(g$table, c("moment", "observed", "sim_mean", "sim_sd", "mcse",
                          "diff", "rel_diff", "tolerance", "tol_type", "pass",
                          "mcse_resolved", "obs_in_sim_range", "n_used"))
  expect_identical(g$table$moment, names(mg_tol))
  ## MCSE is the replication SD over sqrt(R).
  expect_equal(g$table$mcse, g$table$sim_sd / sqrt(30))
  expect_equal(g$table$diff, g$table$sim_mean - g$table$observed)
  ## Observed moments agree with searchnet_k_readings() on the evolved periods.
  s <- attr(searchnet_k_readings(obs), "summary")
  expect_equal(g$table$observed[g$table$moment == "density"], mean(s$density[2:3]))
  expect_equal(g$table$observed[g$table$moment == "change_rate"],
               mean(c(sum(obs[, , 2] != obs[, , 1]), sum(obs[, , 3] != obs[, , 2]))) /
                 (40 * 30))
  expect_identical(g$provenance$theta_source, "rates_density_fit")
  expect_identical(g$provenance$theta_source_origin, "argument")
  expect_output(print(g), "PASS: 6 of 6")
})

test_that("a world with the wrong density fails and stops by default", {
  set.seed(102)
  obs <- mg_panel(0.20)
  sims <- lapply(1:20, function(r) mg_panel(0.40))
  err <- tryCatch(searchnet_moment_gate(obs, sims, tolerance = mg_tol),
                  searchnet_moment_gate_error = function(e) e)
  expect_s3_class(err, "searchnet_moment_gate_error")
  expect_match(conditionMessage(err), "FAIL")
  expect_match(conditionMessage(err), "density: observed")
  expect_false(err$gate$pass)
  expect_false(err$gate$table$pass[err$gate$table$moment == "density"])

  expect_warning(g <- searchnet_moment_gate(obs, sims, tolerance = mg_tol,
                                            on_fail = "warn"), "FAIL")
  expect_identical(g$verdict, "FAIL")
  expect_silent(g2 <- searchnet_moment_gate(obs, sims, tolerance = mg_tol,
                                            on_fail = "return"))
  expect_identical(g2$verdict, "FAIL")
  expect_output(print(g2), "FAIL")
})

test_that("the verdict uses the mean, not whether observed lies in the band", {
  ## Observed density 0.20 lies inside the simulated range (0.05 to 0.55) and
  ## within 1.96 simulated SDs of the mean, but the mean (0.30) misses it by
  ## 0.10 against a tolerance of 0.02. The gate must FAIL.
  obs <- c(density = 0.20)
  sims <- data.frame(rep = 1:6, density = c(0.05, 0.15, 0.30, 0.35, 0.40, 0.55))
  g <- searchnet_moment_gate(obs, sims, moments = "density",
                             tolerance = list(density = 0.02), on_fail = "return")
  row <- g$table
  expect_true(row$obs_in_sim_range)
  expect_lt(abs(row$diff), 1.96 * row$sim_sd)     # a dispersion band would pass
  expect_equal(row$sim_mean, 0.30)
  expect_false(row$pass)
  expect_identical(g$verdict, "FAIL")

  ## And the converse: a tight world whose mean is within tolerance passes even
  ## though observed lies outside its narrow range.
  sims2 <- data.frame(density = c(0.209, 0.210, 0.211))
  g2 <- searchnet_moment_gate(obs, sims2, moments = "density",
                              tolerance = list(density = 0.02))
  expect_false(g2$table$obs_in_sim_range)
  expect_true(g2$pass)
})

test_that("a world parameterized from RSiena starting values is refused", {
  set.seed(103)
  obs <- mg_panel(0.20)
  sims <- lapply(1:5, function(r) mg_panel(0.20))
  ## Stated in the argument: refused whatever on_fail says.
  for (of in c("stop", "warn", "return"))
    expect_error(searchnet_moment_gate(obs, sims, tolerance = mg_tol,
                                       on_fail = of,
                                       theta_source = "starting_values"),
                 "refused.*starting values.*getNetworkStartingVals")
  ## Declared as an attribute on the simulated list or on one replication.
  sims_a <- structure(sims, theta_source = "starting_values")
  expect_error(searchnet_moment_gate(obs, sims_a, tolerance = mg_tol),
               "declared on the simulated object")
  sims_b <- sims
  attr(sims_b[[3]], "theta_source") <- "getNetworkStartingVals"
  expect_error(searchnet_moment_gate(obs, sims_b, tolerance = mg_tol), "refused")
  ## A declared source that is not starting values is recorded.
  sims_c <- structure(sims, theta_source = "rates_density_fit")
  g <- searchnet_moment_gate(obs, sims_c, tolerance = mg_tol, on_fail = "return")
  expect_identical(g$provenance$theta_source, "rates_density_fit")
  expect_identical(g$provenance$theta_source_origin,
                   "declared on the simulated object")
})

test_that("tolerance is required, complete, and well formed", {
  set.seed(104)
  obs <- mg_panel(0.20)
  sims <- lapply(1:5, function(r) mg_panel(0.20))
  expect_error(searchnet_moment_gate(obs, sims), "`tolerance` is required")
  expect_error(searchnet_moment_gate(obs, sims, tolerance = NULL),
               "`tolerance` is required")
  expect_error(searchnet_moment_gate(obs, sims, tolerance = mg_tol[-1]),
               "no entry for moment\\(s\\): density")
  expect_error(searchnet_moment_gate(obs, sims, moments = "density",
                                     tolerance = list(density = 0.01, densty = 0.01)),
               "not gated: densty")
  expect_error(searchnet_moment_gate(obs, sims, moments = "density",
                                     tolerance = 0.01), "named by moment")
  expect_error(searchnet_moment_gate(obs, sims, moments = "density",
                                     tolerance = list(density = -1)),
               "one positive number")
  expect_error(searchnet_moment_gate(obs, sims, moments = "density",
                                     tolerance = list(density = c(pct = 1))),
               "use \"abs\"")
  ## Data-frame form is equivalent to the list form.
  tdf <- data.frame(moment = names(mg_tol),
                    tolerance = vapply(mg_tol, function(v) unname(v), numeric(1)),
                    type = c("abs", "rel", "rel", "rel", "rel", "abs"))
  g1 <- searchnet_moment_gate(obs, sims, tolerance = mg_tol, on_fail = "return")
  g2 <- searchnet_moment_gate(obs, sims, tolerance = tdf, on_fail = "return")
  expect_equal(g1$table, g2$table)
  ## A relative tolerance on an observed zero is undefined.
  expect_error(searchnet_moment_gate(c(density = 0), data.frame(density = c(0.1, 0.2)),
                                     moments = "density",
                                     tolerance = list(density = c(rel = 0.1))),
               "observed value is 0")
})

test_that("inputs: moment tables, sizes, periods, unknown moments", {
  set.seed(105)
  obs <- mg_panel(0.20)
  ## Long simulated table.
  long <- data.frame(rep = rep(1:3, each = 1), moment = "density",
                     value = c(0.19, 0.20, 0.21))
  g <- searchnet_moment_gate(obs, long, moments = "density",
                             tolerance = list(density = 0.05))
  expect_equal(g$table$sim_mean, 0.20)
  ## Size mismatch is an error.
  expect_error(searchnet_moment_gate(obs, list(mg_panel(0.2, M = 10, N = 30)),
                                     tolerance = mg_tol), "not the observed size")
  ## A change moment needs two periods.
  expect_warning(
    expect_error(searchnet_moment_gate(obs[, , 1], list(mg_panel(0.2), mg_panel(0.2)),
                                       moments = "change_rate",
                                       tolerance = list(change_rate = 0.01)),
                 "at least two periods"),
    "different numbers of periods")
  expect_error(searchnet_moment_gate(obs, list(mg_panel(0.2), mg_panel(0.2)),
                                     moments = "clustering",
                                     tolerance = list(clustering = 0.1)),
               "unknown moment")
  ## Different period counts warn.
  expect_warning(searchnet_moment_gate(obs, list(mg_panel(0.2, W = 4),
                                                 mg_panel(0.2, W = 4)),
                                       moments = "density",
                                       tolerance = list(density = 0.2)),
                 "different numbers of periods")
  ## A single replication warns that there is no MCSE.
  expect_warning(searchnet_moment_gate(obs, list(mg_panel(0.2)),
                                       moments = "density",
                                       tolerance = list(density = 0.2)),
                 "one simulated replication")
  ## A searchnet_bipartite observed input works.
  long_obs <- data.frame(a = c("a1", "a1", "a2"), c = c("c1", "c2", "c2"),
                         t = c(1, 2, 2))
  sb <- searchnet_bipartite_from_long(long_obs, "a", "c", "t")
  g3 <- searchnet_moment_gate(sb, list(sb$B, sb$B), moments = c("density", "change_rate"),
                              tolerance = list(density = 0.01, change_rate = 0.01))
  expect_true(g3$pass)
  expect_equal(g3$table$observed, c(0.5, 0.75))
})

test_that("simulation environments are read as start plus evolved waves", {
  skip_if_not_installed("RSiena")
  mk <- function(seed) {
    env <- saomnk_env(M = 6, N = 10, density = 0.3, seed = 31)
    env$search_rsiena_multiwave_run(structure_model = saomnk_model(density = -1),
                                    waves = 2, iterations = 30, rand_seed = seed)
    env
  }
  envs <- lapply(1:4, mk)
  ## Observed: one environment's own panel. Simulated: the four environments.
  e1 <- envs[[1]]
  obs <- c(list(e1$rsiena_model_waves[[1]]$searchnet_start), e1$bipartite_matrix_waves)
  g <- searchnet_moment_gate(obs, envs, moments = c("density", "change_rate"),
                             tolerance = list(density = 0.5, change_rate = 0.5))
  expect_identical(g$n_reps, 4L)
  expect_equal(unname(g$sim_moments[1, "density"]),
               mean(c(mean(e1$bipartite_matrix_waves[[1]]),
                      mean(e1$bipartite_matrix_waves[[2]]))))
  ## Starting-values provenance recorded on an environment is refused.
  envs[[2]]$provenance$theta_source <- "starting_values"
  expect_error(searchnet_moment_gate(obs, envs, moments = "density",
                                     tolerance = list(density = 0.5)),
               "refused")
})
