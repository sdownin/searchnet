###############################################################################
## test-path-not-replay.R
##
## Regression tests for the replayed-chain defect. They FAIL on the engine as
## of 2026-10-06 (dev at 0.10.0), by design: they are gates G1-G4 of
## docs/PREREG_2026-10-06_state_carrying_simulation.md, committed before any
## engine change, and they pass only once search_rsiena() simulates paths
## that carry state.
##
## The defect. search_rsiena() runs siena07(simOnly = TRUE) under cond = TRUE
## with two identical waves and n3 = the step count. With a target distance of
## 0, each phase-3 run is ONE ministep, and RSiena starts every run from wave
## 1. search_rsiena_process_ministep_chain() then replays those independent
## draws cumulatively from the initial matrix into $bi_env_arr, as though they
## were one path, and leaves the environment in the replayed final state. No
## simulated decision responds to the current state. A fixed ministep budget
## also makes any rate effect zero-sum. (Design Defect Ledger E10 and B18.)
##
## Every assertion reads only objects the package presents as the path
## ($bi_env_arr, $bi_env_arr_initial, $chain_stats, the post-run
## $bipartite_matrix) and RSiena's own output, so the tests hold whichever way
## the fix is implemented.
###############################################################################

.pnr_hamming <- function(a, b) sum(abs(a - b))

## RSiena's end-of-run network for run r, from `$sims` (an edge list).
.pnr_sims_ends <- function(env) {
  lapply(env$rsiena_model$sims, function(s) {
    el <- s[[1]][[1]][[1]]
    B <- matrix(0, env$M, env$N)
    if (length(el) && nrow(el)) B[cbind(el[, 1], el[, 2])] <- el[, 3]
    B
  })
}

## The state the path reports immediately BEFORE ministep k.
.pnr_state_before <- function(env, k) {
  if (k == 1L) env$bi_env_arr_initial else env$bi_env_arr[, , k - 1L]
}

## Log choice probability of the option `j` (j = N + 1 is "no change") for
## ego i in state B, under density and inPop. The change statistics are the
## ones RSiena 1.5.0 uses, pinned against its logged LogChoiceProb to 8.9e-16
## (innovation shocks R_validate_sim.R gate 1, re-verified 4.4e-16 here):
##   density  sg_j               (sg = +1 add, -1 drop)
##   inPop    sg_j * (x_+j - x_ij + 1)
.pnr_log_choice_prob <- function(B, i, j, theta) {
  x  <- B[i, ]
  sg <- 1 - 2 * x
  u  <- c(sg * theta[["density"]] + sg * (colSums(B) - x + 1) * theta[["inPop"]], 0)
  (u - max(u) - log(sum(exp(u - max(u)))))[j]
}

.pnr_model <- function(effects, rates = list(), coCovars = list()) {
  list(dv_bipartite = list(
    name = DV_NAME, rates = rates, effects = effects, coCovars = coCovars,
    varCovars = list(), coDyadCovars = list(), varDyadCovars = list(),
    interactions = list()))
}

.pnr_density_inpop <- function()
  .pnr_model(list(
    list(effect = "density", parameter = -0.6, dv_name = DV_NAME, fix = TRUE),
    list(effect = "inPop",   parameter = -0.4, dv_name = DV_NAME, fix = TRUE)))

.pnr_run <- function(model, M, N, BI_PROB, env_seed, run_seed, per_actor,
                     B_init = NULL) {
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(
    M = M, N = N, BI_PROB = BI_PROB, rand_seed = env_seed, name = "_pnr_"))
  if (!is.null(B_init)) {
    env$bipartite_matrix_init <- B_init
    env$set_system_from_bipartite_matrix(B_init)
  }
  suppressMessages(suppressWarnings(
    env$search_rsiena(structure_model = model, iterations_per_actor = per_actor,
                      run_seed = run_seed)))
  env
}


test_that("G2/G3(i): the reported path ends where RSiena's simulation ended (E10 signature absent)", {
  skip_if_not_installed("RSiena")
  env <- .pnr_run(.pnr_density_inpop(), M = 6, N = 8, BI_PROB = 0.4,
                  env_seed = 9501, run_seed = 9501, per_actor = 10)
  B_init <- env$bi_env_arr_initial
  ends   <- .pnr_sims_ends(env)
  h_init <- vapply(ends, .pnr_hamming, numeric(1), b = B_init)

  ## The defect's signature: every RSiena end-of-run network lies within one
  ## toggle of the initial matrix, because every run is one ministep from it.
  ## A period of about 60 opportunities moves much further than that.
  expect_false(all(h_init <= 1),
               info = sprintf("Hamming(end of run, B_init): mean %.2f, max %d over %d runs",
                              mean(h_init), max(h_init), length(h_init)))

  ## What the package presents as the end of the path must be a network RSiena
  ## actually simulated, not a cumulative replay of independent draws.
  ## Compared by value: storage modes differ (integer replay, double sims).
  terminal <- env$bi_env_arr[, , dim(env$bi_env_arr)[3]]
  h_term <- vapply(ends, .pnr_hamming, numeric(1), b = terminal)
  expect_true(any(h_term == 0),
              info = sprintf("min Hamming(path terminal, any RSiena end) = %d", min(h_term)))
  ## The environment is left in the path's terminal state.
  expect_equal(.pnr_hamming(env$bipartite_matrix, terminal), 0)
})


test_that("G3(ii): one tie at density = -8 is deleted once and never recreated", {
  skip_if_not_installed("RSiena")
  ## NEWS 0.9.0: a genuine path deletes the tie once and never recreates it;
  ## the replay toggles the same dyad on every draw that picks its owner.
  B1 <- matrix(0, 4, 4); B1[1, 1] <- 1
  model <- .pnr_model(list(
    list(effect = "density", parameter = -8, dv_name = DV_NAME, fix = TRUE)))
  env <- .pnr_run(model, M = 4, N = 4, BI_PROB = 0, env_seed = 1,
                  run_seed = 4242, per_actor = 10, B_init = B1)

  path_11 <- c(env$bi_env_arr_initial[1, 1], env$bi_env_arr[1, 1, ])
  toggles <- sum(diff(path_11) != 0)
  expect_lte(toggles, 1L, label = sprintf("toggles of dyad (1,1) on the path (%d)", toggles))
  expect_equal(path_11[length(path_11)], 0)
})


test_that("G1: every logged choice probability is reproduced from the path's preceding state", {
  skip_if_not_installed("RSiena")
  env <- .pnr_run(.pnr_density_inpop(), M = 6, N = 8, BI_PROB = 0.4,
                  env_seed = 9502, run_seed = 9502, per_actor = 10)
  cs <- env$chain_stats
  bip <- cs$dv_varname == DV_NAME
  theta <- c(density = -0.6, inPop = -0.4)
  ks <- which(bip)
  expect_gt(length(ks), 10L)
  err <- vapply(ks, function(k) {
    B <- .pnr_state_before(env, k)
    .pnr_log_choice_prob(B, as.integer(cs$id_from[k]), as.integer(cs$id_to[k]), theta) -
      as.numeric(cs$LogChoiceProb[k])
  }, numeric(1))
  ## RSiena drew ministep k from some state with these probabilities. If the
  ## path is genuine, that state is the one the path reports just before k.
  expect_lt(max(abs(err)), 1e-8,
            label = sprintf("max |log choice prob error| along the path (%.3g)", max(abs(err))))
})


test_that("G4: a rate effect changes how many opportunities occur, and does not starve the reference group", {
  skip_if_not_installed("RSiena")
  skip_on_cran()
  M <- 8; N <- 8; rho <- 25
  grp <- rep(c(0, 1), each = M / 2)
  model_for <- function(beta) .pnr_model(
    effects  = list(list(effect = "density", parameter = -0.6, dv_name = DV_NAME, fix = TRUE)),
    rates    = list(list(effect = "RateX", parameter = beta, dv_name = DV_NAME, fix = TRUE,
                         interaction1 = "self$strat_1_coCovar")),
    coCovars = list(list(effect = "egoX", parameter = 0, dv_name = DV_NAME, fix = TRUE,
                         interaction1 = "self$strat_1_coCovar", x = grp)))
  counts <- function(beta) {
    out <- t(vapply(c(7101, 7102, 7103, 7104, 7105), function(s) {
      env <- .pnr_run(model_for(beta), M = M, N = N, BI_PROB = 0.3,
                      env_seed = s, run_seed = s, per_actor = rho)
      cs <- env$chain_stats
      ego <- as.integer(cs$id_from[cs$dv_varname == DV_NAME])
      c(ref = sum(grp[ego] == 0), total = length(ego))
    }, numeric(2)))
    colSums(out)
  }
  ## The covariate value RSiena actually applied to the reference group
  ## (0 - mean if it centered the covariate, 0 if not).
  env0 <- .pnr_run(model_for(0), M = M, N = N, BI_PROB = 0.3,
                   env_seed = 7101, run_seed = 7101, per_actor = 2)
  cov <- env0$rsiena_data$cCovars[[1]]
  c0 <- if (isTRUE(attr(cov, "centered"))) 0 - attr(cov, "mean") else 0

  n0 <- counts(0); n2 <- counts(2)
  ## A fixed budget makes the total identical in both arms. A rate is a rate:
  ## raising one group's rate raises the total.
  expect_gt(n2[["total"]] / n0[["total"]], 1.3,
            label = sprintf("total opportunities, RateX 2 vs 0 (%d vs %d)",
                            n2[["total"]], n0[["total"]]))
  ## The reference group's opportunities scale with its own rate only:
  ## exp(beta * c0), which is 1 for an uncentered covariate.
  ratio <- n2[["ref"]] / n0[["ref"]]
  expect_lt(abs(ratio / exp(2 * c0) - 1), 0.25,
            label = sprintf("reference-group ratio %.3f against exp(2 * c0) = %.3f (%d vs %d)",
                            ratio, exp(2 * c0), n2[["ref"]], n0[["ref"]]))
})
