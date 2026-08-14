###############################################################################
## test-behavior-coevolution.R
##
## Network--behaviour coevolution for a BIPARTITE network dependent variable.
##
## The first block of tests asserts what RSiena itself offers, by inspecting a
## live getEffects() object rather than by trusting documentation. If a future
## RSiena release adds the one-mode influence effects for bipartite networks,
## or removes a distance-2 effect, these tests are what will say so.
###############################################################################


make_behavior_fixture <- function(M = 5L, N = 6L, seed = 1234L,
                                  values = NULL, effects = NULL, rate = 0.3) {
  env <- saomnk_env(M = M, N = N, seed = seed)
  mod <- saomnk_model(density = -0.5,
                      influence_matrix = saomnk_block_diagonal(N, 2))
  if (is.null(values)) values <- rep_len(c(1, 2, 3), M)
  if (is.null(effects)) {
    effects <- list(
      list(effect = "linear", parameter =  0.1),
      list(effect = "quad",   parameter = -0.2)
    )
  }
  mod$dv_behavior <- saomnk_behavior(
    values  = values,
    rates   = list(list(effect = "Rate", parameter = rate)),
    effects = effects
  )
  list(env = env, mod = mod)
}


# --------------------------------------------------------------------------- #
#  What RSiena actually supports
# --------------------------------------------------------------------------- #

test_that("RSiena accepts a behaviour DV alongside a bipartite network DV", {
  eff <- saomnk_behavior_effects(M = 8L, N = 5L, direction = "influence")
  expect_s3_class(eff, "data.frame")
  expect_gt(nrow(eff), 0L)
})


test_that("the one-mode influence effects are NOT available for a bipartite network", {
  ## avAlt / totAlt / avSim / totSim require ego's DIRECT alters to have a
  ## behaviour. In a bipartite network ego's direct alters are components, and
  ## components have no behaviour. This is a property of the model, not a gap in
  ## searchnet, and it cannot be worked around at the R level.
  inf <- saomnk_behavior_effects(M = 8L, N = 5L, direction = "influence")
  for (nm in c("avAlt", "totAlt", "avSim", "totSim")) {
    expect_false(nm %in% inf$shortName,
                 label = sprintf("one-mode influence effect '%s' unexpectedly present", nm))
  }
})


test_that("the distance-2 influence effects ARE available for a bipartite network", {
  ## These are RSiena's bipartite substitutes: two actors are distance-2
  ## neighbours when they hold a component in common. avInSimDist2 is the
  ## bipartite counterpart of avSim.
  inf <- saomnk_behavior_effects(M = 8L, N = 5L, direction = "influence")
  for (nm in c("avInAltDist2", "totInAltDist2", "avTInAltDist2",
               "totAInAltDist2", "avInSimDist2", "totInSimDist2")) {
    expect_true(nm %in% inf$shortName,
                label = sprintf("distance-2 influence effect '%s' missing", nm))
  }
  ## Shape, degree and covariate effects on behaviour.
  for (nm in c("linear", "quad", "outdeg", "outIsolate", "popAlt",
               "effFrom", "avXAlt", "totXAlt", "avGroup")) {
    expect_true(nm %in% inf$shortName,
                label = sprintf("behaviour effect '%s' missing", nm))
  }
})


test_that("selection effects (behaviour -> network) are available", {
  sel <- saomnk_behavior_effects(M = 8L, N = 5L, direction = "selection")
  expect_gt(nrow(sel), 0L)
  for (nm in c("egoX", "altInDist2", "simEgoInDist2", "inPopX", "sameXCycle4")) {
    expect_true(nm %in% sel$shortName,
                label = sprintf("selection effect '%s' missing", nm))
  }
})


test_that("network_only filtering keeps only network-dependent influence effects", {
  all_inf <- saomnk_behavior_effects(M = 8L, N = 5L, direction = "influence")
  net_inf <- saomnk_behavior_effects(M = 8L, N = 5L, direction = "influence",
                                     network_only = TRUE)
  expect_lt(nrow(net_inf), nrow(all_inf))
  expect_true("avInSimDist2" %in% net_inf$shortName)
  ## `linear` is a pure shape effect and references no network.
  expect_false("linear" %in% net_inf$shortName)
})


# --------------------------------------------------------------------------- #
#  saomnk_behavior() constructor
# --------------------------------------------------------------------------- #

test_that("saomnk_behavior normalises values to an M x waves integer matrix", {
  b <- saomnk_behavior(values = c(1, 2, 3, 1, 2))
  expect_s3_class(b, "saomnk_behavior")
  expect_true(is.matrix(b$values))
  expect_equal(nrow(b$values), 5L)
  expect_equal(ncol(b$values), 2L)
  expect_type(b$values[1L, 1L], "integer")
  ## Both waves start identical, as the bipartite DV's two waves do.
  expect_equal(b$values[, 1L], b$values[, 2L])
})


test_that("saomnk_behavior shifts values to start at 1 and reports the shift", {
  b <- saomnk_behavior(values = c(-2, 0, 3))
  expect_equal(min(b$values), 1L)
  expect_equal(b$shift, 3L)
  ## The shift is affine, so the spacing between actors is preserved.
  expect_equal(as.vector(diff(b$values[, 1L])), c(2L, 3L))
})


test_that("saomnk_behavior rejects a constant behaviour", {
  expect_error(saomnk_behavior(values = rep(2, 6)), "at least\\s+two distinct")
})


test_that("saomnk_behavior stamps dv_name onto every effect spec", {
  b <- saomnk_behavior(values = c(1, 2, 3),
                       effects = list(list(effect = "linear", parameter = 0.5)))
  expect_equal(b$effects[[1L]]$dv_name, b$name)
  expect_equal(b$rates[[1L]]$dv_name, b$name)
})


# --------------------------------------------------------------------------- #
#  Engine plumbing
# --------------------------------------------------------------------------- #

test_that("the behaviour DV is actually present in the constructed siena data object", {
  fx <- make_behavior_fixture()
  suppressMessages(fx$env$prepare_theta_scaffold(fx$mod, iterations = 20L))

  dvs <- fx$env$rsiena_data$depvars
  expect_equal(length(dvs), 2L)
  expect_true("self$behavior_rsienaDV" %in% names(dvs))
  expect_equal(unname(attr(dvs[["self$behavior_rsienaDV"]], "type")), "behavior")
  expect_equal(unname(attr(dvs[["self$bipartite_rsienaDV"]], "type")), "bipartite")
  ## The behaviour DV lives on the ACTORS node set.
  expect_equal(unname(attr(dvs[["self$behavior_rsienaDV"]], "nodeSet")), "ACTORS")
})


test_that("behaviour effects are registered against the behaviour DV by name", {
  fx <- make_behavior_fixture(effects = list(
    list(effect = "linear", parameter = 0.1),
    list(effect = "quad",   parameter = -0.2),
    list(effect = "avInSimDist2", parameter = 0.5,
         interaction1 = "self$bipartite_rsienaDV")
  ))
  suppressMessages(fx$env$prepare_theta_scaffold(fx$mod, iterations = 20L))

  e <- as.data.frame(fx$env$rsiena_effects)
  e <- e[e$include, ]
  beh <- e[e$name == "self$behavior_rsienaDV", ]

  expect_true(all(c("linear", "quad", "avInSimDist2") %in% beh$shortName))
  ## The influence effect is bound to the bipartite network, not left dangling.
  expect_equal(beh$interaction1[beh$shortName == "avInSimDist2"],
               "self$bipartite_rsienaDV")
  ## Parameters carried through.
  expect_equal(beh$parm[beh$shortName == "avInSimDist2"], 0.5)
})


test_that("a behaviour DV widens the theta matrix to include both basic rates", {
  ## Two dependent variables force UNCONDITIONAL estimation, under which every
  ## basic rate occupies a theta column. Getting the width wrong makes siena07()
  ## refuse the matrix outright, so this is the load-bearing invariant.
  fx <- make_behavior_fixture()
  tm <- suppressMessages(fx$env$prepare_theta_scaffold(fx$mod, iterations = 20L))

  expect_equal(fx$env$get_n_rsiena_depvars(), 2L)
  cn <- colnames(tm)
  expect_true(any(grepl("^Rate", cn)))
  expect_equal(sum(grepl("^Rate", cn)), 2L)
  expect_true(all(c("density", "linear", "quad") %in% cn))
})


test_that("a model with no behaviour DV keeps the narrow, conditional theta width", {
  ## Regression guard: the single-DV path must be untouched.
  env <- saomnk_env(M = 5L, N = 6L, seed = 1234L)
  mod <- saomnk_model(density = -0.5,
                      influence_matrix = saomnk_block_diagonal(6, 2))
  tm <- env$prepare_theta_scaffold(mod, iterations = 20L)

  expect_equal(env$get_n_rsiena_depvars(), 1L)
  expect_false(any(grepl("^Rate", colnames(tm))))
  expect_true("density" %in% colnames(tm))
})


test_that("an environment reused across models does not carry a stale behaviour DV", {
  fx <- make_behavior_fixture()
  suppressMessages(fx$env$prepare_theta_scaffold(fx$mod, iterations = 20L))
  expect_false(is.null(fx$env$behavior_rsienaDV))

  plain <- saomnk_model(density = -0.5,
                        influence_matrix = saomnk_block_diagonal(6, 2))
  fx$env$prepare_theta_scaffold(plain, iterations = 20L)
  expect_null(fx$env$behavior_rsienaDV)
  expect_equal(fx$env$get_n_rsiena_depvars(), 1L)
})


test_that("behaviour values must be one per actor", {
  env <- saomnk_env(M = 5L, N = 6L, seed = 1234L)
  mod <- saomnk_model(density = -0.5,
                      influence_matrix = saomnk_block_diagonal(6, 2))
  mod$dv_behavior <- saomnk_behavior(values = c(1, 2, 3))  ## only 3, need 5
  expect_error(env$prepare_theta_scaffold(mod, iterations = 20L),
               "Behaviour values must be one per node")
})


# --------------------------------------------------------------------------- #
#  End-to-end coevolution run
# --------------------------------------------------------------------------- #

test_that("a coevolution run completes and both DVs actually move", {
  ## skip_on_cran() removed 2026-08-14: the package is not on CRAN, and the
  ## gate meant these never ran locally either (bare test_dir() does not set
  ## NOT_CRAN), so the feature shipped with zero executed verification. If a
  ## CRAN submission happens, re-gate at that point with the cost understood.
  fx <- make_behavior_fixture(M = 5L, N = 6L, rate = 0.5, effects = list(
    list(effect = "linear", parameter = 0.1),
    list(effect = "quad",   parameter = -0.2),
    list(effect = "avInSimDist2", parameter = 0.5,
         interaction1 = "self$bipartite_rsienaDV")
  ))

  suppressMessages(saomnk_run(fx$env, fx$mod, steps_per_actor = 10L, seed = 42L))

  cs <- fx$env$chain_stats
  expect_false(is.null(cs))
  ## Both dependent variables produced ministeps.
  expect_true("self$behavior_rsienaDV" %in% cs$dv_varname)
  expect_true("self$bipartite_rsienaDV" %in% cs$dv_varname)
  ## Some behaviour ministeps changed the behaviour.
  beh <- cs$dv_varname == "self$behavior_rsienaDV"
  expect_gt(sum(cs$beh_difference[beh] != 0), 0L)
})


test_that("behaviour ministeps never mutate the bipartite state trajectory", {
  ## A behaviour ministep's `id_to` is a behaviour value, not a component id.
  ## Toggling on it would silently corrupt every downstream network statistic.
  fx <- make_behavior_fixture(M = 5L, N = 6L, rate = 0.5)
  suppressMessages(saomnk_run(fx$env, fx$mod, steps_per_actor = 10L, seed = 7L))

  arr <- fx$env$bi_env_arr
  cs  <- fx$env$chain_stats
  expect_equal(dim(arr)[3L], nrow(cs))

  is_beh <- cs$dv_varname == "self$behavior_rsienaDV"
  changed <- vapply(2:dim(arr)[3L],
                    function(i) any(arr[, , i] != arr[, , i - 1L]),
                    logical(1))
  expect_false(any(changed & is_beh[-1L]))
  ## And the network did move on network ministeps, so the test is not vacuous.
  expect_gt(sum(changed & !is_beh[-1L]), 0L)
  ## tie_change is FALSE on every behaviour ministep.
  expect_false(any(cs$tie_change[is_beh]))
})


test_that("post-run statistics processing survives a coevolving behaviour DV", {
  fx <- make_behavior_fixture(M = 5L, N = 6L, rate = 0.3)
  suppressMessages(saomnk_run(fx$env, fx$mod, steps_per_actor = 8L, seed = 42L))

  ## The utility / K-4 decomposition is defined over the BIPARTITE evaluation
  ## function only; behaviour effects are excluded rather than fabricated.
  expect_silent(invisible(capture.output(fx$env$search_rsiena_process_stats())))
  expect_false(is.null(fx$env$actor_stats_df))
  expect_gt(nrow(fx$env$actor_stats_df), 0L)
  expect_false(any(c("linear", "quad") %in% fx$env$actor_stats_df$effect_name))
})


test_that("saomnk_get_behavior recovers the simulated behaviour trajectory", {
  fx <- make_behavior_fixture(M = 5L, N = 6L, rate = 0.5)
  suppressMessages(saomnk_run(fx$env, fx$mod, steps_per_actor = 10L, seed = 42L))

  long <- saomnk_get_behavior(fx$env)
  expect_s3_class(long, "data.frame")
  expect_named(long, c("run", "actor_id", "value"))
  expect_equal(length(unique(long$actor_id)), 5L)

  wide <- saomnk_get_behavior(fx$env, wide = TRUE)
  expect_true(is.matrix(wide))
  expect_equal(ncol(wide), 5L)
  expect_equal(nrow(wide) * ncol(wide), nrow(long))
  ## The behaviour is not frozen.
  expect_gt(length(unique(as.vector(wide))), 1L)
})


test_that("saomnk_get_behavior errors clearly when there is no behaviour DV", {
  env <- saomnk_env(M = 4L, N = 6L, seed = 1234L)
  mod <- saomnk_model(density = -0.5,
                      influence_matrix = saomnk_block_diagonal(6, 2))
  expect_error(saomnk_get_behavior(env), "Run saomnk_run\\(\\) first")

  suppressMessages(saomnk_run(env, mod, steps_per_actor = 5L, seed = 1L))
  expect_error(saomnk_get_behavior(env), "dv_behavior")
})


test_that("a ramp composes with a coevolving behaviour DV", {
  ## The two new capabilities are independent, and must remain so: a ramp on a
  ## network parameter has to work in a model that also evolves a behaviour.
  fx <- make_behavior_fixture(M = 5L, N = 6L, rate = 0.3)
  n <- 40L
  tm <- suppressMessages(
    saomnk_theta_ramp(fx$env, fx$mod, iterations = n,
                      changes = list(list(effect = "density",
                                          from = 0.5, to = -1.5)))
  )
  expect_true(any(grepl("^Rate", colnames(tm))))
  expect_equal(nrow(tm), n)

  suppressMessages(saomnk_run(fx$env, fx$mod, theta_matrix = tm, seed = 21L))
  expect_equal(nrow(fx$env$rsiena_model$thetaUsed), n)
  expect_gt(length(unique(fx$env$theta_matrix[, "density"])), 1L)
})
