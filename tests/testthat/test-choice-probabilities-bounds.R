###############################################################################
## test-choice-probabilities-bounds.R
## compute_choice_probabilities() under degree bounds. An actor cap is RSiena's
## MaxDegree, a HARD constraint: the adds it removes get probability exactly 0
## and the logit is renormalized over the rest. A floor (or a component cap) is
## a FIXED PENALTY effect, a soft constraint: the crossing move keeps about
## exp(-20) of the mass and is flagged, not zeroed. The cap rule is checked
## against RSiena's own simulated ministeps.
###############################################################################

## An environment whose structure model carries `bounds`, set to state B.
.cpb_env <- function(B, bounds, seed = 1) {
  env <- saomnk_env(M = nrow(B), N = ncol(B), density = 0, seed = seed)
  mod <- saomnk_model(density = -1, degree_bounds = bounds, repair_initial = TRUE)
  suppressMessages(saomnk_run(env, mod, steps_per_actor = 1, seed = seed))
  env$bipartite_matrix <- B
  env
}

## Formal utility reduced to the scope term, so a toggle's utility change is
## known in closed form: U_i = -beta_s * (d_i / N)^2.
.cpb_cp <- function(env, ...)
  env$compute_choice_probabilities(beta = 1, beta_F = 0, beta_s = -4, beta_w = 0, ...)

test_that("at the MaxDegree cap, adds get probability exactly 0 and the row sums to 1", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  B <- rbind(c(1, 1, 0, 0),   # at the cap
             c(1, 0, 0, 0),   # below it
             c(0, 0, 1, 1))   # at the cap
  env <- .cpb_env(B, c(max = 2))
  cp  <- .cpb_cp(env)
  cp0 <- .cpb_cp(env, renormalize = FALSE)
  for (i in c(1L, 3L)) {
    p <- cp[[i]]$probabilities
    adds <- which(B[i, ] == 0)
    expect_identical(unname(p[adds]), c(0, 0))
    expect_equal(sum(p), 1, tolerance = 1e-12)
    expect_identical(cp[[i]]$removed, B[i, ] == 0)
    expect_identical(cp[[i]]$infeasible, B[i, ] == 0)
    ## the remaining options are the old logit conditioned on the feasible set
    keep <- c(B[i, ] == 1, TRUE)
    old <- cp0[[i]]$probabilities
    expect_equal(unname(p[keep]), unname(old[keep] / sum(old[keep])), tolerance = 1e-12)
    ## and the old behavior put most of the mass on the removed adds
    expect_gt(sum(old[adds]), 0.5)
    expect_identical(cp[[i]]$delta_u, cp0[[i]]$delta_u)
  }
  ## closed form at the cap: drops at -0.75, status quo at 0
  expect_equal(unname(cp[[1]]$probabilities["pass"]), 1 / (1 + 2 * exp(-0.75)),
               tolerance = 1e-12)
  ## below the cap nothing is removed, and with a cap only (no penalty
  ## effects) the probabilities are exactly the unbounded logit
  expect_false(any(cp[[2]]$removed))
  expect_identical(cp[[2]]$probabilities, cp0[[2]]$probabilities)
})

test_that("a floor penalty leaves the drop of the last tie tiny but nonzero, and flagged", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  B <- rbind(c(1, 0, 0, 0),   # at the floor of 1
             c(1, 1, 0, 0),
             c(0, 1, 1, 0))
  env <- .cpb_env(B, c(min = 1))
  cp  <- .cpb_cp(env)
  cp0 <- .cpb_cp(env, renormalize = FALSE)
  p1 <- cp[[1]]$probabilities
  expect_lt(p1[["flip_1"]], 1e-8)
  expect_gt(p1[["flip_1"]], 0)                    # soft: not hard-zeroed
  expect_identical(cp[[1]]$infeasible, c(TRUE, FALSE, FALSE, FALSE))
  expect_false(any(cp[[1]]$removed))
  expect_equal(sum(p1), 1, tolerance = 1e-12)
  ## the penalty enters the exponent as in RSiena: exp(-20) relative to the
  ## unpenalized weight of the same move
  w_old <- cp0[[1]]$probabilities / cp0[[1]]$probabilities[["pass"]]
  w_new <- p1 / p1[["pass"]]
  expect_equal(w_new[["flip_1"]], w_old[["flip_1"]] * exp(-20), tolerance = 1e-10)
  expect_equal(w_new[2:4], w_old[2:4], tolerance = 1e-12)
  ## the old behavior gave that move ordinary mass
  expect_gt(cp0[[1]]$probabilities[["flip_1"]], 1e-3)
  ## actors above the floor are unaffected by it
  for (i in 2:3) expect_equal(cp[[i]]$probabilities, cp0[[i]]$probabilities,
                              tolerance = 1e-12)
})

test_that("a floor k > 1 (outTrunc) penalizes only the drop that crosses it", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  B <- rbind(c(1, 1, 0, 0), c(1, 1, 1, 0))
  env <- .cpb_env(B, c(min = 2, max = 3))
  cp <- .cpb_cp(env)
  p1 <- cp[[1]]$probabilities
  expect_true(all(p1[c("flip_1", "flip_2")] < 1e-8))
  expect_identical(cp[[1]]$infeasible, c(TRUE, TRUE, FALSE, FALSE))
  ## actor 2 at the cap of 3, above the floor: its one add is removed exactly
  expect_identical(unname(cp[[2]]$probabilities[["flip_4"]]), 0)
  expect_identical(cp[[2]]$removed, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(sum(cp[[2]]$probabilities), 1, tolerance = 1e-12)
})

test_that("without degree bounds the result is unchanged by `renormalize`", {
  env <- run_tiny_sim(M = 3, N = 4, iterations_per_actor = 2, rand_seed = 9)
  a <- env$compute_choice_probabilities(beta = 1)
  b <- env$compute_choice_probabilities(beta = 1, renormalize = FALSE)
  expect_identical(a, b)
  expect_null(a[[1]]$infeasible)
  expect_null(a[[1]]$removed)
  expect_error(env$compute_choice_probabilities(renormalize = NA))
})

test_that("renormalized probabilities at the cap match RSiena's simulated ministeps", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  ## RSiena with MaxDegree 2 and outAct fixed at 0.25: the objective change of
  ## a toggle at outdegree d is 0.25 * (2d + 1) for an add and -0.25 * (2d - 1)
  ## for a drop, which is exactly the formal scope term at beta_s = -4, N = 4.
  M <- 40L; N <- 4L; cap <- 2L
  set.seed(11)
  B0 <- t(vapply(seq_len(M), function(i) {
    x <- integer(N); x[sample.int(N, cap)] <- 1L; x }, integer(N)))
  B1 <- B0; B1[1, ] <- c(1L, 0L, 0L, 0L)
  dv <- RSiena::sienaDependent(array(c(B0, B1), c(M, N, 2)), type = "bipartite",
                               nodeSet = c("actors", "comps"), allowOnly = FALSE)
  dat <- RSiena::sienaDataCreate(bip = dv, nodeSets = list(
    RSiena::sienaNodeSet(M, "actors"), RSiena::sienaNodeSet(N, "comps")))
  eff <- RSiena::getEffects(dat)
  utils::capture.output({
    eff <- RSiena::setEffect(eff, Rate, type = "rate", initialValue = 3, fix = TRUE,
                             period = 1, character = FALSE, name = "bip")
    eff <- RSiena::setEffect(eff, density, initialValue = 0, fix = TRUE, name = "bip")
    eff <- RSiena::setEffect(eff, outAct, initialValue = 0.25, fix = TRUE, name = "bip")
    alg <- RSiena::sienaAlgorithmCreate(projname = NULL, nsub = 0, n3 = 150, seed = 7,
                                        simOnly = TRUE, cond = FALSE,
                                        MaxDegree = c(bip = cap))
    fit <- RSiena::siena07(alg, data = dat, effects = eff, returnChains = TRUE,
                           batch = TRUE, silent = TRUE, useCluster = FALSE)
  })
  ## Replay every chain and tally the options chosen by actors AT the cap.
  tab <- c(stay = 0, drop = 0, add = 0)
  for (r in seq_along(fit$chain)) {
    f <- .searchnet_chain_fields(fit$chain[[r]][[1]][[1]])
    B <- B0
    for (s in seq_along(f$ego)) {
      if (f$name[s] != "bip") next
      i <- f$ego[s] + 1L; a <- f$alter[s]
      at_cap <- sum(B[i, ]) == cap
      if (a == N) { if (at_cap) tab[["stay"]] <- tab[["stay"]] + 1; next }
      j <- a + 1L
      if (at_cap) {
        k <- if (B[i, j] == 1L) "drop" else "add"
        tab[[k]] <- tab[[k]] + 1
      }
      B[i, j] <- 1L - B[i, j]
    }
  }
  n <- sum(tab)
  expect_gt(n, 5000)
  expect_identical(tab[["add"]], 0)          # MaxDegree: never an add at the cap

  env <- .cpb_env(rbind(c(1, 1, 0, 0), c(1, 0, 0, 0)), c(max = cap))
  p_new <- .cpb_cp(env)[[1]]$probabilities[["pass"]]
  p_old <- .cpb_cp(env, renormalize = FALSE)[[1]]$probabilities[["pass"]]
  emp <- tab[["stay"]] / n
  se <- sqrt(p_new * (1 - p_new) / n)
  expect_lt(abs(emp - p_new), 4 * se)        # renormalized: within MC error
  expect_gt(abs(emp - p_old), 20 * se)       # old behavior: far outside it
})
