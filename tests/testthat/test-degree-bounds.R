###############################################################################
## test-degree-bounds.R
## Degree bounds (R/searchnet-degree-bounds.R): an actor cap is RSiena's
## MaxDegree, floors and component caps are fixed penalty effects. Pure-logic
## tests run everywhere; anything that simulates is skipped on CRAN and small.
###############################################################################

set_start <- function(env, B) {
  env$set_system_from_bipartite_matrix(B)
  env$bipartite_matrix_init <- B
  invisible(env)
}

all_states <- function(env) {
  arr <- env$bi_env_arr
  B0 <- env$path_start_matrix
  c(list(B0), lapply(seq_len(dim(arr)[3]), function(t) arr[, , t]))
}

## ---- construction and validation ----------------------------------------------

test_that("bounds normalize from every accepted form", {
  b1 <- .searchnet_as_degree_bounds(c(min = 1, max = 3))
  expect_s3_class(b1, "searchnet_degree_bounds")
  expect_equal(unname(b1$actor), c(1, 3))
  expect_true(all(is.na(b1$component)))
  b2 <- .searchnet_as_degree_bounds(list(actor = c(max = 2), component = c(min = 1)))
  expect_true(is.na(b2$actor[["min"]]))
  expect_equal(b2$component[["min"]], 1)
  expect_identical(.searchnet_as_degree_bounds(b2), b2)
  expect_null(.searchnet_as_degree_bounds(NULL))
  expect_null(.searchnet_as_degree_bounds(c(min = 0)))        # no floor, no cap
  expect_equal(unname(.searchnet_as_degree_bounds(c(2, 4))$actor), c(2, 4))
})

test_that("invalid bounds are refused with the reason", {
  expect_error(searchnet_degree_bounds(actor = c(min = 3, max = 2)), "exceeds max")
  expect_error(searchnet_degree_bounds(actor = c(min = 1.5)), "non-negative integer")
  expect_error(searchnet_degree_bounds(actor = c(min = -1)), "non-negative integer")
  expect_error(searchnet_degree_bounds(actor = c(max = 0)), "forbid every tie")
  expect_error(searchnet_degree_bounds(actor = c(lo = 1)), "only `min` and `max`")
  expect_error(searchnet_degree_bounds(actor = c(min = 1), bound_penalty = -1),
               "positive")
  expect_warning(searchnet_degree_bounds(actor = c(min = 1), bound_penalty = 5),
                 "not negligible")
  expect_error(.searchnet_as_degree_bounds(list(actors = c(min = 1))),
               "named `actor` and/or `component`")
  expect_error(.searchnet_bounds_check_dims(searchnet_degree_bounds(c(max = 7)), 4, 6),
               "exceeds the 6 components")
  expect_error(.searchnet_bounds_check_dims(
    searchnet_degree_bounds(component = c(min = 3)), 2, 6), "exceeds the 2 actors")
})

test_that("component bounds RSiena cannot carry are non-implementations", {
  e1 <- tryCatch(searchnet_degree_bounds(component = c(min = 4)), error = identity)
  expect_s3_class(e1, "searchnet_not_implemented")
  expect_match(conditionMessage(e1), "NON-IMPLEMENTATION")
  expect_match(conditionMessage(e1), "in3Plus")
  e2 <- tryCatch(searchnet_degree_bounds(component = c(max = 3)), error = identity)
  expect_s3_class(e2, "searchnet_not_implemented")
  expect_match(conditionMessage(e2), "MaxDegree constrains outdegrees")
  ## what IS supported maps to RSiena's indicator effects
  b <- searchnet_degree_bounds(component = c(min = 2, max = 2))
  expect_equal(b$effects$shortName, c("antiInIso", "in2Plus", "in3Plus"))
  expect_equal(b$effects$theta, c(20, 20, -20))
})

test_that("each bound maps to its RSiena effect and sign", {
  expect_equal(searchnet_degree_bounds(c(min = 1))$effects$shortName, "outIso")
  expect_equal(searchnet_degree_bounds(c(min = 1))$effects$theta, -20)
  b2 <- searchnet_degree_bounds(c(min = 2), bound_penalty = 25)
  expect_equal(b2$effects$shortName, "outTrunc")
  expect_equal(b2$effects$theta, 25)
  expect_equal(b2$effects$internal_parameter, 2)
  ## a cap alone needs no penalty effect: it is MaxDegree
  b3 <- searchnet_degree_bounds(c(max = 2))
  expect_equal(nrow(b3$effects), 0L)
  expect_equal(.searchnet_max_degree(b3, "net", N = 5), c(net = 2L))
  expect_null(.searchnet_max_degree(b3, "net", N = 2))        # cap >= N binds nothing
})

test_that("saomnk_model records the bounds and print() says they are fixed assumptions", {
  mod <- saomnk_model(density = -1, degree_bounds = c(min = 1, max = 3))
  b <- .searchnet_model_bounds(mod)
  expect_s3_class(b, "searchnet_degree_bounds")
  is_b <- vapply(mod$dv_bipartite$effects, function(e) isTRUE(e$bound), logical(1))
  expect_equal(sum(is_b), 1L)
  e <- mod$dv_bipartite$effects[[which(is_b)]]
  expect_equal(e$effect, "outIso")
  expect_equal(e$parameter, -20)
  expect_true(e$fix)
  out <- paste(capture.output(print(mod)), collapse = "\n")
  expect_match(out, "fixed modeling assumptions, NOT estimated")
  expect_match(out, "MaxDegree = 3")
  expect_match(out, "outIso")
  ## no bounds: nothing printed, nothing added
  out0 <- paste(capture.output(print(saomnk_model(density = -1))), collapse = "\n")
  expect_false(grepl("Degree bounds", out0))
  expect_error(saomnk_model(density = -1, degree_bounds = c(min = 1),
                            extra = list(effect = "outIso", parameter = -1)),
               "already declares outIso")
  expect_output(print(searchnet_degree_bounds(c(min = 2))), "outTrunc")
})

test_that("maximum likelihood is refused with bounds, and why", {
  b <- searchnet_degree_bounds(c(min = 1, max = 3))
  e <- tryCatch(.searchnet_bounds_refuse_ml(b, TRUE, "x()"), error = identity)
  expect_s3_class(e, "searchnet_not_implemented")
  expect_match(conditionMessage(e), "maxlike and MaxDegree are incompatible")
  expect_true(.searchnet_bounds_refuse_ml(NULL, TRUE))
  expect_true(.searchnet_bounds_refuse_ml(b, FALSE))
  expect_error(searchnet_recovery("inPop", c(rate = 3, density = -1, inPop = 0),
                                  M = 8, N = 6, degree_bounds = c(min = 1),
                                  algorithm_args = list(maxlike = TRUE)),
               "maximum likelihood")
  ## estimating WITHOUT the bounds is ordinary estimation, so ML is not refused
  ## on the bounds' account there (it is not run here).
  expect_error(searchnet_recovery("outIso", c(rate = 3, density = -1, outIso = 0),
                                  M = 8, N = 6, degree_bounds = c(min = 1)),
               "fixed penalty effect")
})

## ---- the data check ------------------------------------------------------------

test_that("searchnet_check_degree_bounds lists actor, wave and degree", {
  B1 <- matrix(c(1, 0, 0,
                 0, 1, 1,
                 1, 1, 0), 3, 3, byrow = TRUE)
  expect_true(searchnet_check_degree_bounds(B1, c(min = 1, max = 2)))
  B2 <- B1; B2[1, 1] <- 0                     # actor 1 drops its last component
  B3 <- B1; B3[2, 1] <- 1                     # actor 2 holds three
  e <- tryCatch(searchnet_check_degree_bounds(list(B1, B2, B3), c(min = 1, max = 2)),
                error = identity)
  expect_s3_class(e, "searchnet_degree_bounds_violation")
  v <- e$violations
  expect_equal(nrow(v), 2L)
  expect_equal(v$id, c(1L, 2L))
  expect_equal(v$wave, c(2L, 3L))
  expect_equal(v$degree, c(0, 3))
  expect_equal(v$kind, c("below min", "above max"))
  expect_match(conditionMessage(e), "actor 1, wave 2: degree 0 \\(min 1\\)")
  ## an array and a sienaDependent are accepted too
  arr <- array(c(B1, B2), c(3, 3, 2))
  expect_error(searchnet_check_degree_bounds(arr, c(min = 1)), "1 degree-bound violation")
  skip_if_not_installed("RSiena")
  dep <- RSiena::sienaDependent(arr, type = "bipartite", nodeSet = c("A", "C"))
  expect_error(searchnet_check_degree_bounds(dep, c(min = 1)), "actor 1, wave 2")
  ## component bounds
  expect_error(searchnet_check_degree_bounds(B1, list(component = c(min = 2))),
               "component 3, wave 1: degree 1")
})

test_that("portfolio infeasibility flags exactly the bound-violating rows", {
  configs <- as.matrix(expand.grid(rep(list(0:1), 3)))
  f <- searchnet_portfolio_infeasible(configs, c(min = 1, max = 2))
  expect_equal(as.logical(f), rowSums(configs) < 1 | rowSums(configs) > 2)
  expect_equal(attr(f, "reason")[1], "actor below min")
  expect_equal(attr(f, "reason")[8], "actor above max")
  expect_false(any(searchnet_portfolio_infeasible(configs, NULL)))
  ## component bounds read the other actors' holdings
  state <- matrix(c(1, 0, 0,
                    0, 1, 0), 2, 3, byrow = TRUE)
  g <- searchnet_portfolio_infeasible(configs, list(component = c(max = 1)),
                                      state = state, actor = 1)
  ## actor 1 may not take component 2 (actor 2 holds it)
  expect_equal(as.logical(g), configs[, 2] == 1)
  expect_error(searchnet_portfolio_infeasible(configs, list(component = c(min = 1))),
               "supply `state`")
  mod <- saomnk_model(density = -1, degree_bounds = c(min = 1))
  expect_equal(sum(searchnet_portfolio_infeasible(configs, mod)), 1L)
})

test_that("bound statistics are zero within the bounds and carry RSiena's changes", {
  B <- matrix(c(1, 1, 0,
                0, 1, 0), 2, 3, byrow = TRUE)
  expect_equal(.searchnet_bound_stat("outIso", 1, -20, B), c(0, 0))
  expect_equal(.searchnet_bound_stat("outTrunc", 2, 20, B), c(0, -1))
  ## antiInIso as a floor: minus the number of empty components (component 3)
  expect_equal(.searchnet_bound_stat("antiInIso", 1, 20, B), c(-1, -1))
  ## in2Plus as a cap: number of components with two or more holders
  expect_equal(.searchnet_bound_stat("in2Plus", 1, -20, B), c(1, 1))
  ## change for a toggle equals the change in RSiena's statistic
  B2 <- B; B2[2, 3] <- 1
  rs <- function(X) sum(colSums(X) >= 1)
  expect_equal(.searchnet_bound_stat("antiInIso", 1, 20, B2)[1] -
                 .searchnet_bound_stat("antiInIso", 1, 20, B)[1], rs(B2) - rs(B))
})

test_that("the ministep gate stops at the first state outside the bounds", {
  B0 <- diag(1, 3)
  fr <- data.frame(dv_varname = "self$bipartite_rsienaDV",
                   id_from = c("0", "0", "1"), id_to = c("1", "0", "1"),
                   stability = c("FALSE", "FALSE", "FALSE"),
                   stringsAsFactors = FALSE)
  b <- searchnet_degree_bounds(c(min = 1))
  ## actor 1 adds component 2, drops component 1, then actor 2 drops its only one
  expect_error(.searchnet_bounds_chain_gate(B0, fr, b, "test"),
               "at ministep 3: actor 2 now holds 0")
  expect_true(.searchnet_bounds_chain_gate(B0, fr[1:2, ], b, "test"))
  expect_true(.searchnet_bounds_chain_gate(B0, fr, NULL, "test"))
  expect_error(.searchnet_bounds_end_gate(matrix(0, 2, 2), b, "x"), "gate failed in x")
})

## ---- simulation (RSiena) ---------------------------------------------------------

test_that("the recovery harness carries bounds into simulation and estimation", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  args <- list(effects_spec = c("density", "inPop"),
               theta_true = c(rate = 3, density = -1.5, inPop = 0.1),
               reps = 2L, M = 10L, N = 6L, waves = 3L, n3 = 60L, nsub = 1L,
               seed = 7L, degree_bounds = c(min = 1), keep_panels = TRUE)
  r1 <- do.call(searchnet_recovery, args)
  expect_false(any(grepl("^bound:", r1$estimates$effect)))
  expect_true(all(vapply(r1$panels, function(p)
    all(vapply(p, function(B) min(rowSums(B)), numeric(1)) >= 1), logical(1))))
  expect_true(r1$settings$estimate_with_bounds)
  expect_output(print(r1), "Estimated WITH the bounds")
  r0 <- do.call(searchnet_recovery, c(args, list(estimate_with_bounds = FALSE)))
  ## the same planted panels, estimated without the bound
  expect_identical(r0$panels, r1$panels)
  expect_output(print(r0), "Estimated WITHOUT the bounds")
})

test_that("an actor floor of 1 is never violated, and a cap is never exceeded", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 6, N = 5, density = 0.4, seed = 3)
  ## density -2.5 pushes every actor toward holding nothing
  mod <- saomnk_model(density = -2.5, degree_bounds = c(min = 1),
                      repair_initial = TRUE)
  suppressMessages(saomnk_run(env, mod, steps_per_actor = 25, seed = 5))
  degs <- vapply(all_states(env), function(B) min(rowSums(B)), numeric(1))
  expect_gt(length(degs), 50L)
  expect_true(all(degs >= 1))
  ## without the floor the same model empties actors
  env0 <- saomnk_env(M = 6, N = 5, density = 0.4, seed = 3)
  suppressMessages(saomnk_run(env0, saomnk_model(density = -2.5),
                              steps_per_actor = 25, seed = 5))
  expect_true(any(vapply(all_states(env0), function(B) min(rowSums(B)),
                         numeric(1)) == 0))

  ## cap: density +2 pushes toward holding everything
  env2 <- saomnk_env(M = 6, N = 5, density = 0, seed = 3)
  set_start(env2, matrix(c(1, 0, 0, 0, 0), 6, 5, byrow = TRUE))
  suppressMessages(saomnk_run(env2, saomnk_model(density = 2, degree_bounds = c(max = 2)),
                              steps_per_actor = 25, seed = 5))
  mx <- vapply(all_states(env2), function(B) max(rowSums(B)), numeric(1))
  expect_true(all(mx <= 2))
  expect_true(any(mx == 2))
})

test_that("choice probabilities flag the toggles that would leave the bounds", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 4, N = 4, density = 0, seed = 1)
  set_start(env, diag(1, 4))
  suppressMessages(saomnk_run(env, saomnk_model(density = -1, degree_bounds = c(min = 1, max = 2)),
                              steps_per_actor = 3, seed = 4))
  cp <- env$compute_choice_probabilities(beta = 1)
  B <- env$bipartite_matrix
  for (i in seq_len(4)) {
    expect_length(cp[[i]]$infeasible, 4L)
    deg <- sum(B[i, ])
    want <- if (deg == 1) B[i, ] == 1 else if (deg == 2) B[i, ] == 0 else rep(FALSE, 4)
    expect_equal(cp[[i]]$infeasible, as.logical(want))
  }
  ## no bounds, no flag
  env0 <- run_tiny_sim(M = 3, N = 4, iterations_per_actor = 2, rand_seed = 9)
  expect_null(env0$compute_choice_probabilities(beta = 1)[[1]]$infeasible)
})

test_that("a floor k > 1 (outTrunc) holds once reached", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 6, N = 5, density = 0.5, seed = 9)
  mod <- saomnk_model(density = -2.5, degree_bounds = c(min = 2),
                      repair_initial = TRUE)
  suppressMessages(saomnk_run(env, mod, steps_per_actor = 25, seed = 2))
  mins <- vapply(all_states(env), function(B) min(rowSums(B)), numeric(1))
  expect_true(all(mins >= 2))
  ## the chain's utilities carry no penalty term on this feasible path
  b <- .searchnet_model_bounds(mod)
  expect_equal(b$effects$shortName, "outTrunc")
})

test_that("a component floor of 1 holds in simulation", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 6, N = 4, density = 0.5, seed = 4)
  mod <- saomnk_model(density = -2, degree_bounds = list(component = c(min = 1)),
                      repair_initial = TRUE)
  suppressMessages(saomnk_run(env, mod, steps_per_actor = 20, seed = 8))
  expect_true(all(vapply(all_states(env), function(B) min(colSums(B)),
                         numeric(1)) >= 1))
})

test_that("under min 1 a switch from A to B passes through holding both", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  ## density -2 makes a single component the preferred portfolio, so actors
  ## at their floor keep trading one component for another
  env <- saomnk_env(M = 4, N = 4, density = 0, seed = 1)
  set_start(env, diag(1, 4))
  mod <- saomnk_model(density = -2, degree_bounds = c(min = 1))
  suppressMessages(saomnk_run(env, mod, steps_per_actor = 60, seed = 21))
  states <- all_states(env)
  swaps <- 0L
  for (i in seq_len(4)) {
    port <- lapply(states, function(B) which(B[i, ] == 1))
    single <- which(lengths(port) == 1L)
    for (k in seq_along(single)[-1]) {
      a <- port[[single[k - 1L]]]; b <- port[[single[k]]]
      if (a == b) next
      swaps <- swaps + 1L
      between <- port[single[k - 1L]:single[k]]
      ## never empty, and some state between them holds both A and B
      expect_true(all(lengths(between) >= 1L))
      expect_true(any(vapply(between, function(p) all(c(a, b) %in% p), logical(1))))
    }
  }
  expect_gt(swaps, 0L)
})

test_that("a start below the floor errors by default and is repaired on request", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 4, N = 4, density = 0, seed = 1)
  B0 <- diag(1, 4); B0[2, 2] <- 0
  set_start(env, B0)
  expect_error(saomnk_run(env, saomnk_model(density = -1, degree_bounds = c(min = 1)),
                          steps_per_actor = 2, seed = 1),
               "below a degree floor:\n    actor 2, wave 1: degree 0")
  expect_message(saomnk_run(env, saomnk_model(density = -1, degree_bounds = c(min = 1),
                                              repair_initial = TRUE),
                            steps_per_actor = 2, seed = 1),
                 "repaired the start state")
  expect_equal(sum(env$path_start_matrix[2, ]), 1)
  ## a start above a cap is never repaired
  set_start(env, matrix(1, 4, 4))
  expect_error(saomnk_run(env, saomnk_model(density = -1, degree_bounds = c(max = 2),
                                            repair_initial = TRUE),
                          steps_per_actor = 2, seed = 1),
               "above a degree cap")
})

test_that("estimation entry points check the observed waves first", {
  skip_if_not_installed("RSiena")
  B1 <- matrix(c(1, 0, 0,
                 0, 1, 1,
                 1, 0, 1,
                 0, 1, 0), 4, 3, byrow = TRUE)
  B2 <- B1; B2[4, 2] <- 0
  dep <- RSiena::sienaDependent(array(c(B1, B2), c(4, 3, 2)), type = "bipartite",
                                nodeSet = c("ACTORS", "COMPONENTS"))
  dat <- RSiena::sienaDataCreate(bip = dep, nodeSets = list(
    RSiena::sienaNodeSet(4, "ACTORS"), RSiena::sienaNodeSet(3, "COMPONENTS")))
  eff <- RSiena::getEffects(dat)
  expect_error(searchnet_coevolve(dat, eff, degree_bounds = c(min = 1),
                                  verbose = FALSE),
               "actor 4, wave 2: degree 0")
  ## the penalty effects enter fixed and untested, with the internal parameter
  eff2 <- .searchnet_apply_bound_effects(eff, searchnet_degree_bounds(c(min = 2)), "bip")
  r <- which(eff2$shortName == "outTrunc" & eff2$include)
  expect_length(r, 1L)
  expect_true(eff2$fix[r])
  expect_false(eff2$test[r])
  expect_equal(eff2$parm[r], 2)
  expect_equal(eff2$initialValue[r], 20)
})
