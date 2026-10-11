###############################################################################
## test-composition.R
## Actors who enter or leave: composition change in the observed-data layer,
## searchnet_composition(), searchnet_coevolve_data(), searchnet_k_readings()
## and searchnet_recovery(). Synthetic data only.
###############################################################################

.comp_long <- function() {
  data.frame(
    actor     = c("a1", "a2", "a3", "a1", "a2", "a1", "a2", "a4", "a5", "a5"),
    component = c("c1", "c2", "c1", "c2", "c2", "c1", "c1", "c2", "c1", "c1"),
    period    = c(2001, 2001, 2001, 2002, 2002, 2003, 2003, 2003, 2001, 2003),
    stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# searchnet_bipartite_from_long: entry, exit, presence
# ---------------------------------------------------------------------------
test_that("an exit mid-panel builds the right presence and NA rows", {
  obs <- searchnet_bipartite_from_long(.comp_long(), "actor", "component",
                                       "period", exit = c(a3 = 2001.5))
  P <- obs$composition$presence
  expect_equal(dimnames(P), list(c("a1", "a2", "a3", "a4", "a5"),
                                 c("2001", "2002", "2003")))
  expect_equal(unname(P["a3", ]), c(TRUE, FALSE, FALSE))
  expect_true(all(P[-3, ]))
  ## absent actor-waves are NA, present ones keep their ties
  expect_true(all(is.na(obs$B["a3", , c("2002", "2003")])))
  expect_equal(unname(obs$B["a3", , "2001"]), c(1L, 0L))
  expect_false(anyNA(obs$B[-3, , ]))
  ## stored intervals are in RSiena wave time: 2001.5 is halfway through period 1
  expect_equal(obs$composition$intervals$a3, c(1, 1.5))
  expect_equal(obs$composition$intervals$a1, c(1, 3))
  expect_identical(obs$composition$source, "entry/exit")
  expect_output(print(obs), "present")
})

test_that("entry and exit accept a column, a data frame or a named vector", {
  d <- .comp_long()
  d$open <- ifelse(d$actor == "a4", 2002.4, NA)
  d$close <- ifelse(d$actor == "a3", 2001.5, NA)
  o1 <- searchnet_bipartite_from_long(d, "actor", "component", "period",
                                      entry = "open", exit = "close")
  o2 <- searchnet_bipartite_from_long(
    d, "actor", "component", "period",
    entry = data.frame(id = "a4", t = 2002.4),
    exit = data.frame(id = "a3", t = 2001.5))
  o3 <- searchnet_bipartite_from_long(d, "actor", "component", "period",
                                      entry = c(a4 = 2002.4),
                                      exit = c(a3 = 2001.5))
  expect_equal(o1$composition$presence, o2$composition$presence)
  expect_equal(o1$composition$intervals, o3$composition$intervals)
  expect_equal(o1$composition$intervals$a4, c(2.4, 3))
  expect_equal(unname(o1$composition$presence["a4", ]), c(FALSE, FALSE, TRUE))
  ## an entry before the first period, an exit after the last: present throughout
  o4 <- searchnet_bipartite_from_long(d, "actor", "component", "period",
                                      entry = c(a1 = 1990), exit = c(a1 = 2010))
  expect_equal(o4$composition$intervals$a1, c(1, 3))
})

test_that("non-numeric periods take period labels as times", {
  d <- .comp_long()
  d$period <- paste0("y", d$period)
  o <- searchnet_bipartite_from_long(d, "actor", "component", "period",
                                     entry = c(a4 = "y2003"))
  expect_equal(o$composition$intervals$a4, c(3, 3))
  expect_error(searchnet_bipartite_from_long(d, "actor", "component", "period",
                                             entry = c(a4 = "y2002.5")),
               "period labels")
})

test_that("presence allows re-entry and uses midpoints without timing", {
  P <- matrix(TRUE, 5, 3, dimnames = list(paste0("a", 1:5),
                                          c("2001", "2002", "2003")))
  P["a3", c("2002", "2003")] <- FALSE
  P["a4", c("2001", "2002")] <- FALSE
  P["a5", "2002"] <- FALSE
  o <- searchnet_bipartite_from_long(.comp_long(), "actor", "component",
                                     "period", presence = P)
  expect_identical(o$composition$source, "presence")
  expect_equal(o$composition$intervals$a3, c(1, 1.5))
  expect_equal(o$composition$intervals$a4, c(2.5, 3))
  expect_equal(o$composition$intervals$a5, c(1, 1.5, 2.5, 3))
  expect_equal(o$composition$presence, P)
  expect_true(all(is.na(o$B["a5", , "2002"])))
})

test_that("composition errors name the problem", {
  d <- .comp_long()
  expect_error(searchnet_bipartite_from_long(d, "actor", "component", "period",
                                             exit = c(a1 = 2001.5)),
               "fall in a period where the actor is absent")
  expect_error(searchnet_bipartite_from_long(d, "actor", "component", "period",
                                             entry = c(a4 = 2004)),
               "after the last period")
  expect_error(searchnet_bipartite_from_long(d, "actor", "component", "period",
                                             entry = c(a4 = 2002.2),
                                             exit = c(a4 = 2002.8)),
               "present at no observed wave")
  expect_error(searchnet_bipartite_from_long(d, "actor", "component", "period",
                                             entry = c(zz = 2002)),
               "not in the actor universe")
  expect_error(searchnet_bipartite_from_long(d, "actor", "component",
                                             exit = c(a3 = 1)),
               "need a `period` column")
  expect_error(searchnet_bipartite_from_long(
    d, "actor", "component", "period", exit = c(a3 = 2001.5),
    presence = matrix(TRUE, 5, 3)), "not both")
  d$close <- ifelse(d$actor == "a1", c(2001.5, 2002.5), NA)
  expect_error(searchnet_bipartite_from_long(d, "actor", "component", "period",
                                             exit = "close"),
               "constant within an actor")
})

# ---------------------------------------------------------------------------
# searchnet_composition and RSiena
# ---------------------------------------------------------------------------
test_that("the composition object is accepted by sienaDataCreate", {
  skip_if_not_installed("RSiena")
  obs <- searchnet_bipartite_from_long(.comp_long(), "actor", "component",
                                       "period", exit = c(a3 = 2001.5),
                                       entry = c(a4 = 2002.4))
  cc <- searchnet_composition(obs)
  expect_s3_class(cc, "compositionChange")
  expect_identical(attr(cc, "nodeSet"), "ACTORS")
  expect_identical(attr(cc, "ccOption"), 1L)
  expect_length(cc, 5L)
  dep <- RSiena::sienaDependent(obs$B, type = "bipartite",
                                nodeSet = c("ACTORS", "COMPONENTS"))
  dat <- RSiena::sienaDataCreate(
    bip = dep, composition = cc,
    nodeSets = list(RSiena::sienaNodeSet(5, "ACTORS"),
                    RSiena::sienaNodeSet(2, "COMPONENTS")))
  expect_true(.searchnet_is_siena_data(dat))
  act <- attr(dat$compositionChange[[1L]], "activeStart")
  expect_equal(unname(act), unname(obs$composition$presence))
  ev <- attr(dat$compositionChange[[1L]], "events")
  expect_setequal(as.character(ev$event), c("leave", "join"))
})

test_that("an exit exactly at an intermediate wave is passed just after it", {
  skip_if_not_installed("RSiena")
  cc <- searchnet_composition(list(c(1, 2), c(1, 3), c(2, 3), c(1, 3)))
  expect_equal(cc[[1L]], c(1, 2 + 1e-4))
  expect_equal(cc[[2L]], c(1, 3))          # the last wave is left alone
  expect_equal(cc[[3L]], c(2, 3))          # an entry is left alone
  P <- matrix(c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE), 2, 3, byrow = TRUE)
  P <- rbind(P, TRUE)
  expect_equal(searchnet_composition(P)[[1L]], c(1, 2.5))
  expect_error(searchnet_composition(obs <- searchnet_bipartite_from_long(
    .comp_long(), "actor", "component", "period")), "no composition change")
  expect_error(searchnet_composition(list(c(1, 2)), option = 4), "option")
})

test_that("searchnet_coevolve_data adds the composition change automatically", {
  skip_if_not_installed("RSiena")
  obs <- searchnet_bipartite_from_long(.comp_long(), "actor", "component",
                                       "period", exit = c(a3 = 2001.5),
                                       entry = c(a4 = 2002.4))
  arch <- array(c(0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0), c(2, 2, 3))
  dat <- searchnet_coevolve_data(obs, arch, verbose = FALSE)
  expect_length(dat$compositionChange, 1L)
  expect_identical(attr(dat$compositionChange[[1L]], "nodeSet"), "ACTORS")
  expect_true(attr(dat, "searchnet_coev")$has_composition)
  expect_output(searchnet_coevolve_data(obs, arch), "composition")
  dat0 <- searchnet_coevolve_data(obs, arch, composition = FALSE,
                                  verbose = FALSE)
  expect_length(dat0$compositionChange, 0L)
  expect_false(attr(dat0, "searchnet_coev")$has_composition)
  dat1 <- searchnet_coevolve_data(obs$B, arch, verbose = FALSE,
                                  composition = searchnet_composition(obs))
  expect_length(dat1$compositionChange, 1L)
  expect_error(searchnet_coevolve_data(obs, arch, composition = "yes",
                                       verbose = FALSE), "`composition`")
})

# ---------------------------------------------------------------------------
# searchnet_k_readings: absent actors are skipped with their ties
# ---------------------------------------------------------------------------
test_that("k_readings excludes absent actors and their ties", {
  obs <- searchnet_bipartite_from_long(.comp_long(), "actor", "component",
                                       "period", exit = c(a3 = 2001.5),
                                       entry = c(a4 = 2002.4))
  k <- searchnet_k_readings(obs)
  a <- k[k$level == "actor", ]
  expect_setequal(a$id[a$period == 2001], c("a1", "a2", "a3", "a5"))
  expect_setequal(a$id[a$period == 2002], c("a1", "a2", "a5"))
  expect_setequal(a$id[a$period == 2003], c("a1", "a2", "a4", "a5"))
  s <- attr(k, "summary")
  expect_equal(s$n_actors, c(4L, 3L, 4L))
  expect_equal(s$density, s$ties / (s$n_actors * 2))
  ## each period equals the readings of the present actors alone
  for (w in 1:3) {
    on <- obs$composition$presence[, w]
    kw <- searchnet_k_readings(obs$B[on, , w])
    aw <- kw[kw$level == "actor", c("id", "K_AC", "K_AA")]
    ow <- a[a$period == obs$periods[w], c("id", "K_AC", "K_AA")]
    rownames(aw) <- rownames(ow) <- NULL
    expect_equal(ow, aw)
    cw <- kw[kw$level == "component", "K_CA"]
    expect_equal(k$K_CA[k$level == "component" & k$period == obs$periods[w]], cw)
  }
  ## tie stability over actors present at both waves only
  B <- obs$B
  both <- obs$composition$presence[, 1] & obs$composition$presence[, 2]
  p <- B[both, , 1]; cu <- B[both, , 2]
  expect_equal(s$tie_jaccard[2], sum(p & cu) / sum(p | cu))
  ## covariate: absent actor-periods are NA, universe order kept
  cv <- searchnet_k_covariate(k, "K_AC")
  expect_equal(rownames(cv), obs$actors)
  expect_true(is.na(cv["a4", "2001"]) && is.na(cv["a3", "2003"]))
  expect_equal(cv["a4", "2003"], 1)
})

test_that("a bare array reads an all-NA row as absence; other NA errors", {
  set.seed(3)
  B <- array(rbinom(5 * 4 * 2, 1, 0.4), c(5, 4, 2))
  B[2, , 2] <- NA
  k <- searchnet_k_readings(B)
  expect_equal(attr(k, "summary")$n_actors, c(5L, 4L))
  B[3, 1, 1] <- NA
  expect_error(searchnet_k_readings(B), "NA for a present actor")
})

test_that("the moment gate reads change over actors present at both waves", {
  set.seed(4)
  B <- array(rbinom(6 * 4 * 3, 1, 0.4), c(6, 4, 3))
  B[1, , 3] <- NA
  m <- .mg_panel_moments(lapply(1:3, function(w) B[, , w]))
  exp_chg <- mean(c(sum(B[, , 2] != B[, , 1]) / 24,
                    sum(B[-1, , 3] != B[-1, , 2]) / 20))
  expect_equal(unname(m["change_rate"]), exp_chg)
  expect_false(anyNA(m))
})

# ---------------------------------------------------------------------------
# searchnet_recovery with a composition design
# ---------------------------------------------------------------------------
test_that("composition design validation", {
  expect_error(.recovery_check_composition(list(stay = 0.1), 10, 3), "leave")
  expect_error(.recovery_check_composition(list(leave = 1), 10, 3), "\\[0, 1\\)")
  expect_error(.recovery_check_composition(list(enter = 0.5), 10, 3),
               "fewer than two")
  expect_error(.recovery_check_composition(list(leave = 0), 10, 3), "NULL")
  cmp <- .recovery_check_composition(list(leave = 0.2), 10, 3)
  plan <- .recovery_composition_plan(cmp, 10, 3, seed = 1, rep = 1)
  expect_equal(sum(plan$exit < 3), 2 + round(0.2 * 8))
  expect_true(all(plan$entry == 1))
  expect_true(all(plan$exit[plan$exit < 3] %% 1 > 0.04))
  cmp2 <- .recovery_check_composition(list(leave = 0.1, enter = 0.2), 10, 3)
  plan2 <- .recovery_composition_plan(cmp2, 10, 3, seed = 1, rep = 1)
  expect_equal(sum(plan2$entry > 1), 4)
  expect_identical(plan2, .recovery_composition_plan(cmp2, 10, 3, 1, 1))
  expect_error(searchnet_recovery(c("density", "inPop"),
                                  c(rate = 3, density = -1, inPop = 0),
                                  M = 8, N = 6, composition = list(leave = 0.2),
                                  degree_bounds = c(max = 4)),
               "cannot be combined")
})

test_that("a tiny recovery with composition runs, with and without it", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  args <- list(effects_spec = c("density", "inPop"),
               theta_true = c(rate = 3, density = -1.2, inPop = 0.15),
               reps = 1L, M = 12L, N = 6L, waves = 3L, n3 = 60L, nsub = 1L,
               seed = 7L, composition = list(leave = 0.2, enter = 0.1),
               keep_panels = TRUE)
  rec <- do.call(searchnet_recovery, args)
  expect_s3_class(rec, "searchnet_recovery")
  expect_false(any(rec$reps$status %in% c("error_simulation", "error_estimation")))
  panel <- rec$panels[[1L]]
  plan <- attr(panel, "composition")
  P <- .recovery_plan_presence(plan, 3L)
  for (w in 1:3) {
    expect_true(all(is.na(panel[[w]][!P[, w], ])))
    expect_false(anyNA(panel[[w]][P[, w], ]))
  }
  expect_true(rec$settings$estimate_with_composition)
  expect_output(print(rec), "Composition in the DGP")
  naive <- do.call(searchnet_recovery,
                   c(args, list(estimate_with_composition = FALSE)))
  expect_false(naive$settings$estimate_with_composition)
  expect_false(any(naive$reps$status %in% c("error_simulation", "error_estimation")))
  ## same seed, same planted panels
  expect_identical(naive$panels[[1L]], panel)
})
