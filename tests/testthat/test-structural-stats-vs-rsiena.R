###############################################################################
## test-structural-stats-vs-rsiena.R
##
## Pins the hand-computed actor statistics in
## get_struct_mod_stats_mat_from_bi_mat() (R/saomnk-base.R) to RSiena's OWN
## target statistics, not to a transcription of a formula.
##
## For each effect the SUM over actors of searchnet's actor statistic must equal
## the network-level target that siena07() reports for the same state
## (sienaAlgorithmCreate(simOnly = TRUE), both waves set to that state):
##
##   inPop      sum_j x_+j^2                         (x_+j counts ego)
##   inPopSqrt  sum_j x_+j^1.5                       (x_+j counts ego)
##   cycle4     number of bipartite four-cycles      (parameter 1)
##   XWX        sum_i sum_{j != h} x_ij x_ih w_hj    (component x component W)
##
## Added 2026-09-15. Before that date inPop counted ego twice, inPopSqrt was
## missing (its column was 0), cycle4 counted degenerate closed walks, and XWX
## summed over all actors rather than within ego. The previous cycle4 tests in
## test-vectorized-effects.R checked the formula against itself and could not
## see any of this.
##
## Extended 2026-10-04 to the degree and covariate statistics, with v an actor
## covariate, c a component covariate, d an actor x component dyadic covariate,
## each centered on its mean as RSiena centers it:
##
##   density        sum_i x_i+
##   outAct         sum_i x_i+^2
##   outActSqrt     sum_i x_i+^1.5
##   egoX           sum_i x_i+ v_i
##   altX           sum_i sum_j x_ij c_j
##   outActX        sum_i x_i+ sum_j x_ij c_j               (parameter 1)
##   X              sum_i sum_j x_ij d_ij
##   totInDist2     sum_i sum_j x_ij sum_{h != i} x_hj v_h
##   simEgoInDist2  sum_i sum_j x_ij [1 - |v_i - vbar_j| / R - simMean],
##                  vbar_j the mean v of j's OTHER holders (mean of v when
##                  there are none), R the range of v, simMean the mean
##                  similarity over ordered actor pairs
##
## Before that date egoX, altX, outActX and X read the raw covariate,
## totInDist2 also counted ego among the holders, and simEgoInDist2 computed
## similarity to distance-2 alters in the actor projection. saom_to_saomnk()
## nevertheless marked egoX, altX, X, totInDist2 and simEgoInDist2 "exact"; the
## last test here now ties every "exact" crosswalk entry to a pin in this file.
###############################################################################

context("Structural statistics vs RSiena siena07 targets")

## Every statistic this file pins to a siena07 target. Each test below loops
## over its own vector, so a name added here is compared with RSiena (and
## fails if RSiena reports no such target); the crosswalk test at the end
## accepts an "exact" bridge entry only if it names one of these.
.PINNED_STRUCTURAL <- c("inPop", "inPopSqrt", "cycle4", "XWX")
.PINNED_COVARIATE  <- c("density", "outAct", "outActSqrt", "egoX", "altX",
                        "outActX", "X", "totInDist2", "simEgoInDist2")
.PINNED_TO_RSIENA  <- c(.PINNED_STRUCTURAL, .PINNED_COVARIATE)

.rsiena_targets <- function(B, W) {
  n1 <- nrow(B); n2 <- ncol(B)
  actors <- RSiena::sienaNodeSet(n1, nodeSetName = "actors")
  comps  <- RSiena::sienaNodeSet(n2, nodeSetName = "comps")
  Xdep <- RSiena::sienaDependent(array(c(B, B), dim = c(n1, n2, 2)),
                                 type = "bipartite", nodeSet = c("actors", "comps"),
                                 allowOnly = FALSE)
  Wcov <- RSiena::coDyadCovar(W, nodeSets = c("comps", "comps"))
  dat <- RSiena::sienaDataCreate(Xdep, Wcov, nodeSets = list(actors, comps))
  eff <- RSiena::getEffects(dat)
  eff <- RSiena::includeEffects(eff, inPop, inPopSqrt, cycle4, verbose = FALSE)
  eff <- RSiena::includeEffects(eff, XWX, interaction1 = "Wcov", verbose = FALSE)
  alg <- RSiena::sienaAlgorithmCreate(projname = NULL, seed = 1, n3 = 5, nsub = 0,
                                      simOnly = TRUE, silent = TRUE)
  ans <- suppressWarnings(
    RSiena::siena07(alg, data = dat, effects = eff, batch = TRUE,
                    verbose = FALSE, silent = TRUE))
  e <- eff[eff$include & eff$type == "eval", ]
  stats::setNames(as.numeric(ans$targets), e$shortName)
}

.stats_env <- function(M, N, W) {
  dvn <- "self$bipartite_rsienaDV"
  env <- SaomNkRSienaBiEnv$new(list(M = M, N = N, BI_PROB = 0.3, rand_seed = 1,
                                    name = "_stats_vs_rsiena_", dir_output = tempdir()))
  sm <- list(dv_bipartite = list(
    name = dvn, rates = list(),
    effects = list(
      list(effect = "density",   parameter = -0.5, dv_name = dvn, fix = TRUE),
      list(effect = "inPop",     parameter = 0.1,  dv_name = dvn, fix = TRUE),
      list(effect = "inPopSqrt", parameter = 0.1,  dv_name = dvn, fix = TRUE),
      list(effect = "cycle4",    parameter = 0.1,  dv_name = dvn, fix = TRUE)
    ),
    coDyadCovars = list(
      list(effect = "XWX", parameter = 0.2, dv_name = dvn, fix = TRUE,
           nodeSet = c("COMPONENTS", "COMPONENTS"),
           interaction1 = "self$component_1_coDyadCovar", x = W))
  ))
  suppressWarnings(env$search_rsiena(structure_model = sm, iterations_per_actor = 2,
                                     run_seed = 5, verbose = FALSE))
  env
}

test_that("actor statistics sum to RSiena's inPop, inPopSqrt, cycle4 and XWX targets", {
  skip_on_cran()
  skip_if_not_installed("RSiena")

  set.seed(20260915)
  M <- 6; N <- 7
  ## Asymmetric W with a nonzero diagonal: the within-ego correction must
  ## remove w_jj, and nothing may assume symmetry.
  W <- matrix(round(stats::runif(N * N), 2), N, N)
  diag(W) <- round(stats::runif(N), 2)
  env <- .stats_env(M, N, W)
  ## The registered covariate slot must hold the W we compare against.
  expect_equal(unname(matrix(as.numeric(env$component_1_coDyadCovar), N, N)), W)

  ## Draw every state BEFORE the first siena07() call: siena07(seed = 1) resets
  ## the global RNG, so draws made inside the loop would repeat one state.
  states <- lapply(1:4, function(k)
    matrix(stats::rbinom(M * N, 1, stats::runif(1, 0.3, 0.7)), M, N))
  expect_true(length(unique(lapply(states, c))) == 4L)
  for (k in 1:4) {
    B <- states[[k]]
    tg <- .rsiena_targets(B, W)
    sm <- utils::capture.output(st <- env$get_struct_mod_stats_mat_from_bi_mat(B))
    tot <- colSums(st)
    for (nm in .PINNED_STRUCTURAL) {
      expect_equal(unname(tot[[nm]]), unname(tg[[nm]]), tolerance = 1e-8,
                   info = sprintf("state %d, effect %s", k, nm))
    }
    ## No effect in this model may fall through to "not yet implemented".
    expect_false(any(grepl("not yet implemented", sm)), info = paste(sm, collapse = " "))
  }
})

test_that("negative controls: the pre-2026-09-15 formulas do NOT match the targets", {
  skip_on_cran()
  skip_if_not_installed("RSiena")

  set.seed(7)
  M <- 6; N <- 7
  W <- matrix(round(stats::runif(N * N), 2), N, N); diag(W) <- 0.5
  B <- matrix(stats::rbinom(M * N, 1, 0.5), M, N)
  tg <- .rsiena_targets(B, W)
  xp <- colSums(B); S <- B %*% t(B)
  old_inpop  <- sum(B %*% (xp + 1))
  old_cycle4 <- sum(rowSums((S %*% S) * S) / 2)
  old_xwx    <- sum(B %*% W %*% t(B))
  expect_gt(abs(old_inpop  - tg[["inPop"]]),  1e-6)
  expect_gt(abs(old_cycle4 - tg[["cycle4"]]), 1e-6)
  expect_gt(abs(old_xwx    - tg[["XWX"]]),    1e-6)
})


## ---- Degree and covariate statistics (2026-10-04) --------------------------

.rsiena_targets_cov <- function(B, VA, VC, D) {
  n1 <- nrow(B); n2 <- ncol(B)
  actors <- RSiena::sienaNodeSet(n1, nodeSetName = "actors")
  comps  <- RSiena::sienaNodeSet(n2, nodeSetName = "comps")
  Xdep <- RSiena::sienaDependent(array(c(B, B), dim = c(n1, n2, 2)),
                                 type = "bipartite", nodeSet = c("actors", "comps"),
                                 allowOnly = FALSE)
  va <- RSiena::coCovar(VA, nodeSet = "actors")
  vc <- RSiena::coCovar(VC, nodeSet = "comps")
  dd <- RSiena::coDyadCovar(D, nodeSets = c("actors", "comps"))
  dat <- RSiena::sienaDataCreate(Xdep, va, vc, dd, nodeSets = list(actors, comps))
  ## density is included by default for a bipartite dependent variable.
  eff <- RSiena::getEffects(dat)
  eff <- RSiena::includeEffects(eff, outAct, outActSqrt, verbose = FALSE)
  eff <- RSiena::includeEffects(eff, egoX, totInDist2, simEgoInDist2,
                                interaction1 = "va", verbose = FALSE)
  eff <- RSiena::includeEffects(eff, altX, outActX, interaction1 = "vc", verbose = FALSE)
  eff <- RSiena::includeEffects(eff, X, interaction1 = "dd", verbose = FALSE)
  alg <- RSiena::sienaAlgorithmCreate(projname = NULL, seed = 1, n3 = 5, nsub = 0,
                                      simOnly = TRUE, silent = TRUE)
  ans <- suppressWarnings(
    RSiena::siena07(alg, data = dat, effects = eff, batch = TRUE,
                    verbose = FALSE, silent = TRUE))
  e <- eff[eff$include & eff$type == "eval", ]
  stats::setNames(as.numeric(ans$targets), e$shortName)
}

## The environment registers its covariates through searchnet's own structure
## model, so get_cov_data() is exercised on the slots a user's model fills.
.cov_env <- function(M, N, VA, VC, D) {
  dvn <- "self$bipartite_rsienaDV"
  env <- SaomNkRSienaBiEnv$new(list(M = M, N = N, BI_PROB = 0.3, rand_seed = 1,
                                    name = "_cov_stats_vs_rsiena_", dir_output = tempdir()))
  ## centered = TRUE: the targets below are RSiena's with its default
  ## centering. Since 0.11.0 searchnet creates monadic covariates uncentered
  ## unless the declaration says otherwise, so the declaration says so here.
  cov <- function(effect, slot, x, ...)
    list(effect = effect, parameter = 0.1, dv_name = dvn, fix = TRUE,
         interaction1 = slot, x = x, centered = TRUE, ...)
  sm <- list(dv_bipartite = list(
    name = dvn, rates = list(),
    effects = list(
      list(effect = "density",    parameter = -0.5, dv_name = dvn, fix = TRUE),
      list(effect = "outAct",     parameter = 0.1,  dv_name = dvn, fix = TRUE),
      list(effect = "outActSqrt", parameter = 0.1,  dv_name = dvn, fix = TRUE)
    ),
    coCovars = list(
      cov("egoX",    "self$strat_1_coCovar",     VA),
      cov("altX",    "self$component_1_coCovar", VC),
      cov("outActX", "self$component_2_coCovar", VC),
      ## totInDist2 and simEgoInDist2 attach to the covariate egoX already
      ## uses, through reuse_interaction1 (see R/saomnk-base.R).
      cov("totInDist2",    "self$strat_2_coCovar", VA,
          reuse_interaction1 = "self$strat_1_coCovar"),
      cov("simEgoInDist2", "self$strat_3_coCovar", VA,
          reuse_interaction1 = "self$strat_1_coCovar")
    ),
    coDyadCovars = list(
      list(effect = "X", parameter = 0.1, dv_name = dvn, fix = TRUE,
           nodeSet = c("ACTORS", "COMPONENTS"),
           interaction1 = "self$strat_1_coDyadCovar", x = D))
  ))
  suppressWarnings(env$search_rsiena(structure_model = sm, iterations_per_actor = 2,
                                     run_seed = 5, verbose = FALSE))
  env
}

test_that("actor statistics sum to RSiena's degree and covariate targets", {
  skip_on_cran()
  skip_if_not_installed("RSiena")

  set.seed(20261004)
  M <- 6; N <- 7
  VA <- round(stats::rnorm(M, 3, 2), 2)       ## nonzero mean: centering matters
  VC <- round(stats::rnorm(N, 2, 1), 2)
  D  <- matrix(round(stats::runif(M * N, 0, 3), 2), M, N)
  env <- .cov_env(M, N, VA, VC, D)
  inc <- as.data.frame(env$rsiena_effects)
  inc <- inc[inc$include & inc$type == "eval", ]
  expect_setequal(inc$shortName, .PINNED_COVARIATE)
  ## The registered slots must hold the covariates the targets are built from.
  expect_equal(as.numeric(env$strat_1_coCovar), VA)
  expect_equal(as.numeric(env$component_1_coCovar), VC)
  expect_equal(unname(matrix(as.numeric(env$strat_1_coDyadCovar), M, N)), D)

  ## Draw every state BEFORE the first siena07() call (it resets the RNG).
  states <- lapply(1:5, function(k)
    matrix(stats::rbinom(M * N, 1, stats::runif(1, 0.2, 0.6)), M, N))
  ## One state with an isolate and a component held by a single actor, so
  ## simEgoInDist2's no-other-holder rule is exercised.
  states[[1]][1, ] <- 0
  states[[1]][, 1] <- 0; states[[1]][2, 1] <- 1
  expect_true(length(unique(lapply(states, c))) == 5L)
  for (k in seq_along(states)) {
    B <- states[[k]]
    tg <- .rsiena_targets_cov(B, VA, VC, D)
    sm <- utils::capture.output(st <- env$get_struct_mod_stats_mat_from_bi_mat(B))
    tot <- colSums(st)
    for (nm in .PINNED_COVARIATE) {
      expect_equal(unname(tot[[nm]]), unname(tg[[nm]]), tolerance = 1e-8,
                   info = sprintf("state %d, effect %s", k, nm))
    }
    expect_false(any(grepl("not yet implemented", sm)), info = paste(sm, collapse = " "))
  }
})

test_that("negative controls: the pre-2026-10-04 covariate formulas do NOT match", {
  skip_on_cran()
  skip_if_not_installed("RSiena")

  set.seed(11)
  M <- 6; N <- 7
  VA <- round(stats::rnorm(M, 3, 2), 2)
  VC <- round(stats::rnorm(N, 2, 1), 2)
  D  <- matrix(round(stats::runif(M * N, 0, 3), 2), M, N)
  B  <- matrix(stats::rbinom(M * N, 1, 0.45), M, N)
  tg <- .rsiena_targets_cov(B, VA, VC, D)
  xa <- rowSums(B)
  old_egox <- sum(VA * xa)
  old_altx <- sum(B %*% VC)
  old_x    <- sum(B * D)
  old_tot  <- sum(B %*% c(VA %*% B))                      ## raw, ego counted
  ## similarity to the mean of distance-2 alters in the actor projection
  S <- B %*% t(B); d1 <- S > 0; diag(d1) <- FALSE
  d2 <- (d1 %*% d1 > 0) & !d1; diag(d2) <- FALSE
  n2 <- rowSums(d2); avg <- ifelse(n2 > 0, (d2 %*% VA) / pmax(n2, 1), 0)
  old_sim <- sum(ifelse(n2 > 0, 1 - abs(VA - avg) / diff(range(VA)), 0))
  expect_gt(abs(old_egox - tg[["egoX"]]),          1e-6)
  expect_gt(abs(old_altx - tg[["altX"]]),          1e-6)
  expect_gt(abs(old_x    - tg[["X"]]),             1e-6)
  expect_gt(abs(old_tot  - tg[["totInDist2"]]),    1e-6)
  expect_gt(abs(old_sim  - tg[["simEgoInDist2"]]), 1e-6)
})

test_that("saomnk_coholder_similarity() is not RSiena's simEgoInDist2", {
  ## The reason for the rename from saomnk_sim_ego_indist2(): its sum over
  ## actors is not the siena07 target. If this ever passes the other way, the
  ## documentation's account of the difference is wrong.
  skip_on_cran()
  skip_if_not_installed("RSiena")

  set.seed(3)
  M <- 6; N <- 7
  VA <- round(stats::rnorm(M, 3, 2), 2)
  VC <- round(stats::rnorm(N, 2, 1), 2)
  D  <- matrix(round(stats::runif(M * N, 0, 3), 2), M, N)
  states <- lapply(1:3, function(k) matrix(stats::rbinom(M * N, 1, 0.4), M, N))
  gaps <- vapply(states, function(B) {
    abs(sum(saomnk_coholder_similarity(B, VA)) -
          .rsiena_targets_cov(B, VA, VC, D)[["simEgoInDist2"]])
  }, numeric(1))
  expect_true(all(gaps > 1e-6), info = paste(signif(gaps, 3), collapse = ", "))
})

## ---- inPopX (2026-10-08) --------------------------------------------------
##
##   inPopX  sum_i sum_j x_ij w_j,  w_j = sum_h x_hj v_h (ego counted),
##           v an actor covariate centered as RSiena centers it, internal
##           parameter 1 ("ind. pop.^(1/#) weighted v")
##
## RSiena 1.5.0's two-mode siena07 target for inPopX is NOT deterministic:
## identical calls (same data, same seed) return either the sum above or the
## same sum with the LAST component's term, x_+N w_N, omitted. Found
## 2026-10-08 by probing with unit covariates: the gap is exactly the last
## column's term in every state tried. So the pin below is two-sided: every
## target RSiena returns is one of those two values, the full form occurs, and
## searchnet's column sums to the full form.

.rsiena_target_inpopx <- function(B, VA) {
  n1 <- nrow(B); n2 <- ncol(B)
  actors <- RSiena::sienaNodeSet(n1, nodeSetName = "actors")
  comps  <- RSiena::sienaNodeSet(n2, nodeSetName = "comps")
  Xdep <- RSiena::sienaDependent(array(c(B, B), dim = c(n1, n2, 2)),
                                 type = "bipartite", nodeSet = c("actors", "comps"),
                                 allowOnly = FALSE)
  va <- RSiena::coCovar(VA, nodeSet = "actors")
  dat <- RSiena::sienaDataCreate(Xdep, va, nodeSets = list(actors, comps))
  eff <- RSiena::includeEffects(RSiena::getEffects(dat), inPopX,
                                interaction1 = "va", verbose = FALSE)
  alg <- RSiena::sienaAlgorithmCreate(projname = NULL, seed = 1, n3 = 5, nsub = 0,
                                      simOnly = TRUE, silent = TRUE)
  ans <- suppressWarnings(
    RSiena::siena07(alg, data = dat, effects = eff, batch = TRUE,
                    verbose = FALSE, silent = TRUE))
  e <- eff[eff$include & eff$type == "eval", ]
  stats::setNames(as.numeric(ans$targets), e$shortName)[["inPopX"]]
}

test_that("inPopX sums to RSiena's two-mode inPopX target (full form)", {
  skip_on_cran()
  skip_if_not_installed("RSiena")

  set.seed(20261008)
  M <- 6; N <- 7
  VA <- round(stats::rnorm(M, 3, 2), 2)       ## nonzero mean: centering matters
  dvn <- "self$bipartite_rsienaDV"
  env <- SaomNkRSienaBiEnv$new(list(M = M, N = N, BI_PROB = 0.3, rand_seed = 1,
                                    name = "_inpopx_vs_rsiena_", dir_output = tempdir()))
  sm <- list(dv_bipartite = list(
    name = dvn, rates = list(),
    effects = list(list(effect = "density", parameter = -0.5, dv_name = dvn, fix = TRUE)),
    coCovars = list(list(effect = "inPopX", parameter = 0.1, dv_name = dvn, fix = TRUE,
                         interaction1 = "self$strat_1_coCovar", x = VA, centered = TRUE))
  ))
  suppressWarnings(env$search_rsiena(structure_model = sm, iterations_per_actor = 2,
                                     run_seed = 5, verbose = FALSE))
  expect_equal(as.numeric(env$strat_1_coCovar), VA)

  states <- lapply(1:8, function(k)
    matrix(stats::rbinom(M * N, 1, stats::runif(1, 0.3, 0.7)), M, N))
  vt <- VA - mean(VA)
  n_full <- 0L
  for (k in seq_along(states)) {
    B <- states[[k]]
    w <- colSums(B * vt)
    full <- sum(colSums(B) * w)
    no_last <- full - sum(B[, N]) * w[N]
    utils::capture.output(st <- env$get_struct_mod_stats_mat_from_bi_mat(B))
    expect_equal(sum(st[, "inPopX"]), full, tolerance = 1e-8, info = sprintf("state %d", k))
    tg <- replicate(5, .rsiena_target_inpopx(B, VA))
    ok <- abs(tg - full) < 1e-8 | abs(tg - no_last) < 1e-8
    expect_true(all(ok), info = sprintf("state %d: targets %s; full %.6f, no last %.6f",
                                        k, paste(signif(tg, 8), collapse = ", "), full, no_last))
    n_full <- n_full + sum(abs(tg - full) < 1e-8)
  }
  expect_gt(n_full, 0L)
})

test_that("negative control: the pre-2026-10-08 inPopX column does NOT match", {
  set.seed(9)
  M <- 6; N <- 7
  VA <- round(stats::rnorm(M, 3, 2), 2); vt <- VA - mean(VA)
  B <- matrix(stats::rbinom(M * N, 1, 0.5), M, N)
  w <- colSums(B * vt)
  expect_gt(abs(sum(rowSums(B * w)) - sum(colSums(B) * w)), 1e-6)
})

test_that("every crosswalk entry marked exact maps onto a statistic pinned here", {
  ## saom_to_saomnk()'s "exact" status says the SaoMNK effect IS the estimated
  ## RSiena effect. That is a claim about searchnet's statistic, so it needs a
  ## pin. Five entries carried it without one until 2026-10-04.
  cw <- .bridge_crosswalk()
  exact <- Filter(function(m) identical(m$status, "exact"), cw)
  expect_gt(length(exact), 0L)
  targets <- unique(vapply(exact, function(m) m$saomnk, character(1)))
  expect_identical(setdiff(targets, .PINNED_TO_RSIENA), character(0))
})
