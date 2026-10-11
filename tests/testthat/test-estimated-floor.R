###############################################################################
## test-estimated-floor.R
## An ESTIMATED actor floor (searchnet_degree_bounds(estimate_floor = TRUE)),
## the identification helper and the power summary (R/searchnet-floor.R).
## Pure-logic tests run everywhere; anything that calls RSiena is skipped on
## CRAN and uses tiny settings.
###############################################################################

## ---- construction ---------------------------------------------------------------

test_that("estimate_floor builds a free outIso effect at the starting value", {
  b <- searchnet_degree_bounds(actor = c(min = 1, max = 3), estimate_floor = TRUE,
                               floor_start = -1.5)
  expect_true(b$estimate_floor)
  expect_equal(b$floor_start, -1.5)
  expect_equal(nrow(b$effects), 1L)
  expect_identical(b$effects$shortName, "outIso")
  expect_equal(b$effects$theta, -1.5)
  expect_false(b$effects$fix)
  out <- capture.output(print(b))
  expect_true(any(grepl("ESTIMATED \\(free\\)", out)))
  expect_false(any(grepl("These are fixed modeling assumptions", out)))
  ## default keeps the v0.14.0 fixed penalty
  b0 <- searchnet_degree_bounds(actor = c(min = 1))
  expect_false(b0$estimate_floor)
  expect_true(b0$effects$fix)
  expect_equal(b0$effects$theta, -20)
  expect_true(any(grepl("FIXED", capture.output(print(b0)))))
})

test_that("saomnk_model carries the estimated floor as a free bound effect", {
  b <- searchnet_degree_bounds(actor = c(min = 1), estimate_floor = TRUE)
  mod <- saomnk_model(density = -1, degree_bounds = b)
  is_b <- vapply(mod$dv_bipartite$effects, function(e) isTRUE(e$bound), logical(1))
  e <- mod$dv_bipartite$effects[is_b][[1L]]
  expect_identical(e$effect, "outIso")
  expect_false(e$fix)
  expect_equal(e$parameter, -2)
  expect_true(any(grepl("actor floor ESTIMATED", capture.output(print(mod)))))
})

test_that("estimate_floor is validated, and a floor above 1 is not identified", {
  expect_error(searchnet_degree_bounds(actor = c(max = 3), estimate_floor = TRUE),
               "needs an actor floor")
  expect_error(searchnet_degree_bounds(actor = c(min = 1), estimate_floor = NA),
               "TRUE or FALSE")
  expect_error(searchnet_degree_bounds(actor = c(min = 1), estimate_floor = TRUE,
                                       floor_start = Inf), "finite")
  e <- tryCatch(searchnet_degree_bounds(actor = c(min = 2), estimate_floor = TRUE),
                error = identity)
  expect_s3_class(e, "searchnet_not_identified")
  expect_match(conditionMessage(e), "not identified")
  expect_match(conditionMessage(e), "outTrunc\\(2\\)")
})

test_that("an estimated floor is not enforced: degree 0 passes checks and gates", {
  b <- searchnet_degree_bounds(actor = c(min = 1, max = 2), estimate_floor = TRUE)
  B1 <- matrix(c(1, 0, 0,
                 0, 1, 1), 2, 3, byrow = TRUE)
  B2 <- B1; B2[1, 1] <- 0
  expect_true(searchnet_check_degree_bounds(list(B1, B2), b))
  ## the cap still holds
  B3 <- B1; B3[2, 1] <- 1
  expect_error(searchnet_check_degree_bounds(list(B1, B3), b), "above|max")
  configs <- as.matrix(expand.grid(rep(list(0:1), 3)))
  inf <- searchnet_portfolio_infeasible(configs, b)
  expect_false(inf[rowSums(configs) == 0])
  expect_true(all(inf[rowSums(configs) == 3]))
  ## start below the floor is accepted unchanged
  expect_identical(.searchnet_bounds_start(B2, b, seed = 1), B2)
})

## ---- identification helper -------------------------------------------------------

test_that("the identification helper counts transitions to and from degree 0", {
  B1 <- matrix(c(1, 0, 0,
                 0, 1, 1,
                 1, 1, 0,
                 1, 0, 0), 4, 3, byrow = TRUE)
  B2 <- B1; B2[1, 1] <- 0                 # actor 1: 1 -> 0
  B3 <- B2; B3[1, 2] <- 1; B3[4, 1] <- 0  # actor 1: 0 -> 1; actor 4: 1 -> 0
  id <- searchnet_floor_identification(list(B1, B2, B3))
  expect_s3_class(id, "searchnet_floor_identification")
  expect_equal(id$by_wave$at_risk, c(2L, 1L))
  expect_equal(id$by_wave$to_zero, c(1L, 1L))
  expect_equal(id$by_wave$from_zero, c(0L, 1L))
  expect_equal(id$by_wave$stay_zero, c(0L, 0L))
  expect_equal(id$by_wave$isolates_end, c(1L, 1L))
  expect_equal(id$totals[["to_zero"]], 2L)
  expect_identical(id$status, "identified")
  expect_true(id$identifiable)
  ## same counts from an array
  arr <- array(c(B1, B2, B3), c(4, 3, 3))
  expect_identical(searchnet_floor_identification(arr)$by_wave, id$by_wave)
  expect_true(any(grepl("status: identified", capture.output(print(id)))))
})

test_that("no drop to 0 with actors at 1 gives only a bound, not a null", {
  B1 <- matrix(c(1, 0, 0,
                 0, 1, 1), 2, 3, byrow = TRUE)
  B2 <- B1; B2[2, 2] <- 0                 # actor 2: 2 -> 1, no one reaches 0
  id <- searchnet_floor_identification(list(B1, B2))
  expect_identical(id$status, "bound_only")
  expect_false(id$identifiable)
  expect_match(id$message, "Only a bound")
  ## isolates present but no drop: first ties only
  B0 <- B1; B0[1, 1] <- 0
  expect_identical(searchnet_floor_identification(list(B0, B0))$status,
                   "first_tie_only")
  ## nobody near 0
  F <- matrix(1, 2, 3)
  expect_identical(searchnet_floor_identification(list(F, F))$status,
                   "uninformative")
  expect_error(searchnet_floor_identification(B1), "at least two waves")
})

## ---- power summary on a constructed recovery object ----------------------------

test_that("searchnet_floor_power bins by observed drops and keeps non-identified reps", {
  mk <- function(true, drops, status, est, se) {
    reps <- data.frame(rep = seq_along(drops), status = status, tconv_max = 0.1,
                       message = "", floor_drops = drops, stringsAsFactors = FALSE)
    ok <- status != "non_identified"
    estimates <- data.frame(rep = reps$rep[ok], status = status[ok],
                            effect = "outIso", estimate = est[ok], se = se[ok],
                            stringsAsFactors = FALSE)
    structure(list(reps = reps, estimates = estimates,
                   theta_true = c(rate = 3, density = -1, outIso = true),
                   settings = list(degree_bounds = searchnet_degree_bounds(
                     actor = c(min = 1), estimate_floor = TRUE))),
              class = "searchnet_recovery")
  }
  r1 <- mk(-2, c(0, 0, 3, 4, 12), c("non_identified", "non_identified",
                                    "converged", "converged", "converged"),
           c(NA, NA, -1.5, -2.5, -2), c(NA, NA, 1, 1, 0.5))
  p <- searchnet_floor_power(r1)
  expect_equal(p$drops, c("0", "1-4", "10-19"))
  expect_equal(p$n_reps, c(2L, 2L, 1L))
  expect_equal(p$n_non_identified, c(2L, 0L, 0L))
  expect_equal(p$n_converged, c(0L, 2L, 1L))
  expect_true(is.na(p$mde[1]))
  expect_equal(p$bias[2], 0)
  expect_equal(p$mde[2], (qnorm(0.975) + qnorm(0.8)) * 1)
  expect_equal(p$coverage[2], 1)
  pooled <- searchnet_floor_power(list(r1, r1), by_true = FALSE)
  expect_true(all(is.na(pooled$true)))
  expect_equal(pooled$n_reps, c(4L, 4L, 2L))
  expect_error(searchnet_floor_power(list(1)), "searchnet_recovery")
})

## ---- RSiena ------------------------------------------------------------------------

test_that("the estimated floor enters the RSiena effects table free", {
  skip_if_not_installed("RSiena")
  B <- matrix(rep_len(c(1, 0, 0), 24), 8, 3)
  dat <- .recovery_data(list(B, B), NULL, 8L, 3L, allowOnly = FALSE)
  eff <- RSiena::getEffects(dat)
  b <- searchnet_degree_bounds(actor = c(min = 1), estimate_floor = TRUE,
                               floor_start = -1.25)
  eff <- .searchnet_apply_bound_effects(eff, b, .RECOVERY_DV)
  r <- which(eff$shortName == "outIso" & eff$type == "eval" & eff$include)
  expect_length(r, 1L)
  expect_false(eff$fix[r])
  expect_equal(eff$initialValue[r], -1.25)
  b0 <- searchnet_degree_bounds(actor = c(min = 1))
  eff0 <- .searchnet_apply_bound_effects(RSiena::getEffects(dat), b0, .RECOVERY_DV)
  r0 <- which(eff0$shortName == "outIso" & eff0$type == "eval" & eff0$include)
  expect_true(eff0$fix[r0])
})

test_that("a tiny recovery estimates outIso and records observed drops", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  b <- searchnet_degree_bounds(actor = c(min = 1), estimate_floor = TRUE)
  rec <- searchnet_recovery("density",
                            theta_true = c(rate = 3, density = -1, outIso = -1),
                            reps = 2L, M = 20L, N = 6L, waves = 3L, n3 = 100L,
                            nsub = 2L, seed = 11L, degree_bounds = b)
  expect_true("outIso" %in% rec$table$effect)
  expect_equal(rec$table$true[rec$table$effect == "outIso"], -1)
  expect_true("floor_drops" %in% names(rec$reps))
  expect_true(all(rec$reps$floor_drops >= 0L))
  expect_true(any(grepl("estimated floor", capture.output(print(rec)))))
  expect_true(any(grepl("ESTIMATED", capture.output(print(rec)))))
  p <- searchnet_floor_power(rec)
  expect_equal(sum(p$n_reps), 2L)
  ## outIso must be planted
  expect_error(searchnet_recovery("density", theta_true = c(rate = 3, density = -1),
                                  reps = 1L, M = 8L, N = 4L, degree_bounds = b),
               "outIso")
})

test_that("a design with no observed drop to 0 is non-identified, not a null", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  b <- searchnet_degree_bounds(actor = c(min = 1), estimate_floor = TRUE)
  ## outIso planted so low, and the rate so slow, that no actor ever drops its
  ## last tie: every replication must be filed as non-identified.
  rec <- searchnet_recovery("density",
                            theta_true = c(rate = 1, density = -1, outIso = -25),
                            reps = 2L, M = 8L, N = 4L, waves = 3L, n3 = 60L,
                            nsub = 1L, seed = 5L, degree_bounds = b)
  expect_true(all(rec$reps$floor_drops == 0L))
  expect_true(all(rec$reps$status == "non_identified"))
  expect_equal(rec$n_converged, 0L)
  expect_true(all(grepl("not identified", rec$reps$message)))
  out <- rec$table[rec$table$effect == "outIso", ]
  expect_equal(out$n_converged, 0L)
  expect_true(is.na(out$rejection))
  p <- searchnet_floor_power(rec)
  expect_equal(p$drops, "0")
  expect_equal(p$n_non_identified, 2L)
})
