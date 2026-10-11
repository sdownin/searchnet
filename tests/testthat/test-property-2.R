###############################################################################
## test-property-2.R
##
## Property 2 (generalization of NK) of the paper, Section 6 / online
## Appendix C; Property 2 in inst/proofs/PROOF_TABLE.md (Part D, D1-D6).
## Lean: SaomNK.card_config_gt, SaomNK.CoreSpec.landscape_endogeneity,
## SaomNK.logitProb_pos.
##
## The property says SAOM-NK leaves the NK model along three dimensions:
##   (a) multi-actor interaction (M > 1),
##   (b) endogenous landscape co-evolution (theta != 0),
##   (c) bounded rationality (beta < infinity).
##
## Each is checked numerically with package functions, with a negative control:
##
##   (a) The joint state space has 2^(M N) > 2^N configurations (enumerated
##       with the package's .lean_cfg). NEGATIVE CONTROL: with every coupling
##       coefficient zero (own-row effects only), each actor's utility is
##       invariant to every other actor's row, so M > 1 alone produces M
##       independent NK-type searchers and no externality. The externalities
##       the paper attributes to (a) need (a) AND (b); see the note below.
##   (b) Lemma 3 / D1 exactly: with theta_inPop != 0, if actor i holds d and a
##       rival j != i adds d, u_i changes by exactly theta_inPop; if i does not
##       hold d it does not change. Checked on every state of an M = 3, N = 3
##       system with the engine's own statistic function
##       (get_struct_mod_stats_mat_from_bi_mat). NEGATIVE CONTROL: at
##       theta_inPop = 0 no rival move changes u_i.
##   (c) Lemma 4 / D2: under the engine's logit choice rule
##       (compute_choice_probabilities) every single flip, including the
##       fitness-decreasing ones, has positive probability at finite beta;
##       beta = 0 is uniform over the N flips and "no change"; as beta grows the
##       mass on decreasing flips vanishes and the steepest improving flip
##       takes the mass (the greedy NK limit). NEGATIVE CONTROL: the classical
##       NK adaptive walk (nk_walk) never takes a fitness-decreasing step.
##
## Note on the statement (reported, not weakened): Appendix C says (a) alone
## introduces "competitive and cooperative externalities". What holds is the
## PROOF_TABLE D3 reading: M > 1 with theta = 0 enlarges the state space but
## couples nothing (control in (a)); externalities need M > 1 and a cross-row
## effect such as inPop. Likewise (b) is not independent of (a): Lemma 3
## requires M >= 2.
##
## Pure linear algebra on tiny state spaces, plus one M = 1 environment for
## (c). No RSiena simulation.
###############################################################################

## ---- helpers ---------------------------------------------------------------

## The engine's statistic function, called without building an RSiena model
## (same construction as test-potential-cycles.R). Returns the M x K matrix of
## actor statistics for the named effects.
.p2_stats <- function(B, effects, W = NULL) {
  f <- SaomNkRSienaBiEnv_base$public_methods$get_struct_mod_stats_mat_from_bi_mat
  theta_df <- data.frame(shortName = effects, initialValue = 1,
                         effect_level = effects, stringsAsFactors = FALSE)
  fake_self <- list(
    M = nrow(B), N = ncol(B),
    get_bipartite_effects_theta_df = function() theta_df,
    get_cov_data = function(item) W
  )
  environment(f) <- list2env(list(self = fake_self), parent = asNamespace("searchnet"))
  out <- f(B)
  colnames(out) <- effects
  out
}

## Actor utilities u(B) = S(B) theta, M-vector.
.p2_util <- function(B, theta, W) {
  S <- .p2_stats(B, names(theta), W)
  as.numeric(S %*% theta)
}

## Utilities of every state, one row per state code (row code + 1), computed
## once so that flips are table look-ups. .lean_cfg fills B by row from the
## bits of the code, so cell (i, d) is bit (i - 1) * N + d - 1.
.p2_table <- function(theta, M, N, W) {
  t(vapply(0:(2^(M * N) - 1),
           function(code) .p2_util(.lean_cfg(code, M, N), theta, W),
           numeric(M)))
}
.p2_flipcode <- function(code, i, d, N) bitwXor(code, 2L^((i - 1L) * N + d - 1L))

## A fixed asymmetric W for the XWX own-row term.
.p2_W <- matrix(c(0, 0.4, -0.2,
                  0.3, 0, 0.5,
                  -0.1, 0.2, 0), 3, 3, byrow = TRUE)

## ---- (a) multi-actor -------------------------------------------------------

test_that("Property 2(a): the M-actor state space strictly contains the NK one", {
  for (MN in list(c(2L, 3L), c(3L, 2L), c(2L, 4L))) {
    M <- MN[1]; N <- MN[2]
    codes <- 0:(2^(M * N) - 1)
    cfgs <- lapply(codes, .lean_cfg, M = M, N = N)
    keys <- vapply(cfgs, function(B) paste(B, collapse = ""), character(1))
    expect_equal(length(unique(keys)), 2^(M * N))
    expect_true(all(vapply(cfgs, function(B) all(dim(B) == c(M, N)),
                           logical(1))))
    expect_gt(2^(M * N), 2^N)          # card_config_gt, M >= 2, N >= 1
  }
})

test_that("Property 2(a)/(b) control: with theta_inPop = 0, M > 1 creates no externality", {
  ## Own-row effects only (outAct, XWX; inPop = 0): u_i depends on B[i, ]
  ## alone, so no move by actor j changes any other actor's utility.
  M <- 3L; N <- 3L
  U <- .p2_table(c(outAct = -0.4, XWX = 0.9, inPop = 0), M, N, .p2_W)
  max_change <- 0
  for (code in 0:(2^(M * N) - 1)) for (j in seq_len(M)) for (d in seq_len(N)) {
    k <- .p2_flipcode(code, j, d, N)
    max_change <- max(max_change, abs(U[k + 1L, -j] - U[code + 1L, -j]))
  }
  expect_lt(max_change, 1e-12)
})

## ---- (b) endogenous landscape (Lemma 3, D1) -------------------------------

test_that("Property 2(b): a rival's move shifts u_i by exactly theta_inPop (Lemma 3)", {
  M <- 3L; N <- 3L
  th_inpop <- 0.7
  theta <- c(outAct = -0.4, XWX = 0.9, inPop = th_inpop)
  U <- .p2_table(theta, M, N, .p2_W)
  n_held <- 0L; n_not_held <- 0L
  err_held <- 0; err_not_held <- 0
  for (code in 0:(2^(M * N) - 1)) {
    B <- .lean_cfg(code, M, N)
    for (j in seq_len(M)) for (d in seq_len(N)) {
      if (B[j, d] == 1L) next                     # rival j ADDS d
      du <- U[.p2_flipcode(code, j, d, N) + 1L, ] - U[code + 1L, ]
      for (i in setdiff(seq_len(M), j)) {
        if (B[i, d] == 1L) {
          n_held <- n_held + 1L
          err_held <- max(err_held, abs(du[i] - th_inpop))
        } else {
          n_not_held <- n_not_held + 1L
          err_not_held <- max(err_not_held, abs(du[i]))
        }
      }
    }
  }
  expect_gt(n_held, 100L)
  expect_gt(n_not_held, 100L)
  expect_lt(err_held, 1e-12)      # Delta u_i = theta_inPop exactly
  expect_lt(err_not_held, 1e-12)  # no change when i does not hold d

  ## Hence no single-actor NK fitness W(b_i) can represent u_i: two states
  ## with the same own row b_i carry different u_i.
  B  <- matrix(c(1L, 0L, 1L,
                 0L, 0L, 0L), 2, 3, byrow = TRUE)
  B2 <- B; B2[2, 1] <- 1L
  expect_identical(B[1, ], B2[1, ])
  expect_equal(.p2_util(B2, theta, .p2_W)[1] - .p2_util(B, theta, .p2_W)[1],
               th_inpop, tolerance = 1e-12)
})


## ---- (c) bounded rationality (Lemma 4, D2) --------------------------------

test_that("Property 2(c): finite beta gives every flip positive probability (Lemma 4)", {
  skip_if_not_installed("RSiena")
  N <- 5L
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = 1, N = N,
                                                         rand_seed = 610))
  E <- diag(N); E[1, 2] <- E[2, 1] <- 1; E[3, 4] <- E[4, 3] <- 1
  env$component_1_coDyadCovar <- E
  env$search_matrix <- E
  env$compute_fitness_landscape(n_landscapes = 1, component_coCovar = NULL,
                                normalize_int_mat = FALSE,
                                project_int_mat = FALSE,
                                component_value_sd = 0.1, verbose = FALSE)
  ## Pure NK utility: switch off the scope-cost and synergy defaults.
  probs_at <- function(beta)
    env$compute_choice_probabilities(beta = beta, beta_s = 0, beta_w = 0)[[1]]

  ## Find a starting row that has both improving and decreasing flips.
  found <- FALSE
  for (code in 0:(2^N - 1)) {
    env$bipartite_matrix[1, ] <- as.integer(intToBits(code))[seq_len(N)]
    du <- probs_at(1)$delta_u
    if (any(du < -1e-9) && any(du > 1e-9)) { found <- TRUE; break }
  }
  expect_true(found)
  dec <- which(du < -1e-9)
  best <- which.max(du)

  ## Finite beta: every option, including each fitness-decreasing flip, > 0.
  p1 <- probs_at(1)$probabilities
  expect_equal(sum(p1), 1, tolerance = 1e-12)
  expect_true(all(p1 > 0))
  expect_true(all(p1[dec] > 0))

  ## beta = 0: uniform over the N flips and "no change" (random search).
  p0 <- probs_at(0)$probabilities
  expect_equal(unname(p0), rep(1 / (N + 1), N + 1), tolerance = 1e-12)

  ## beta -> infinity: mass on decreasing flips vanishes and the steepest
  ## improving flip takes the mass (greedy NK limit). Betas are scaled by the
  ## margin between the best flip and the best decreasing flip, so
  ## beta * max(du) stays far from overflow.
  margin <- max(du) - max(du[dec])
  betas <- c(1, 5, 20, 40) / margin
  p_dec <- vapply(betas, function(b) sum(probs_at(b)$probabilities[dec]),
                  numeric(1))
  expect_lt(p_dec[length(p_dec)], p_dec[1])
  expect_lt(p_dec[length(p_dec)], 1e-8)
  b_big <- 40 / max(du)                 # beta * max(du) = 40
  p_inf <- probs_at(b_big)$probabilities
  gap <- sort(du, decreasing = TRUE)[1:2]
  if (b_big * (gap[1] - gap[2]) > 10) expect_gt(unname(p_inf[best]), 0.99)
})

test_that("Property 2(c) control: the NK adaptive walk never steps downhill", {
  nk <- nk_landscape(N = 8, K = 3, model = "random", seed = 2026)
  for (type in c("steepest", "greedy", "random")) {
    set.seed(11)
    for (s in sample.int(2^8, 25) - 1L) {
      w <- nk_walk(nk, start = s, type = type)
      if (length(w$fitness) > 1L) expect_true(all(diff(w$fitness) > 0))
    }
  }
})
