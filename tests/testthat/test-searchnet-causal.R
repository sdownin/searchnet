###############################################################################
## test-searchnet-causal.R
##
## Regression coverage for the searchnet -> did::att_gt() integration
## (searchnet_causal_panel() / searchnet_did()).
##
## Prior to this test file the causal-inference wrappers in
## R/searchnet-causal.R had no test coverage at all: the "vignette"
## scenario (6 actors, 3 treated / 3 control, common treatment timing)
## fails at did::att_gt() with an opaque "The never-treated group is too
## small to serve as a reliable control" error, invisible in the rendered
## vignette because message()/warning() output is suppressed there.
##
## This is a genuine scope limit of the Callaway & Sant'Anna (2021)
## estimator, not a bug in how searchnet_causal_panel() builds the panel:
## did::att_gt()'s internal pre_process_did() requires at least
## `5 + <covariates>` distinct units per first_treat group (see
## did:::pre_process_did, the `reqsize` / `gsize` check), and the
## vignette's 3-actor control group falls under that floor. searchnet_did()
## now checks this before delegating to did::att_gt() and fails with a
## message naming the actual requirement and the observed counts.
###############################################################################

## Small, fast fixture mirroring the vignette's simulation setup.
make_causal_fixture <- function(M, N = 8L, seed = 42L, run_seed = 12345L,
                                steps_per_actor = 20L) {
  env <- saomnk_env(M = M, N = N, seed = seed)
  K   <- saomnk_block_diagonal(N, 2)
  mod <- saomnk_model(density = -0.5, popularity = 0.15, influence_matrix = K)

  s_baseline <- saomnk_shock("density", parameter = -0.5, portion = 1)
  s_shocked  <- saomnk_shock("density", parameter = -2.0, portion = 1)

  saomnk_run(env, mod, steps_per_actor = steps_per_actor, seed = run_seed,
             shocks = list(s_baseline, s_shocked))

  total_steps <- max(env$actor_util_df$chain_step_id)
  list(env = env, shock_step = round(total_steps / 2))
}


test_that("searchnet_causal_panel() encodes a balanced panel with common treatment timing", {
  skip_if_not_installed("RSiena")

  fx <- make_causal_fixture(M = 6L)
  panel <- searchnet_causal_panel(fx$env, shock_step = fx$shock_step,
                                   outcome = "utility",
                                   treated_actors = c(1, 2, 3))

  expect_true(all(c("actor_id", "step", "outcome", "treated", "first_treat",
                     "shock_step") %in% names(panel)))

  ## Balanced: every actor appears at every step exactly once.
  tab <- table(panel$actor_id, panel$step)
  expect_true(all(tab == 1L))

  ## Treated actors 1-3 share one first_treat value (common timing); control
  ## actors 4-6 are coded first_treat = 0 (never-treated), as did::att_gt()
  ## requires.
  expect_equal(unique(panel$first_treat[panel$actor_id %in% c("1", "2", "3")]),
               fx$shock_step)
  expect_true(all(panel$first_treat[panel$actor_id %in% c("4", "5", "6")] == 0))
})


test_that("searchnet_did() fails with a clear, actionable error when the never-treated group is too small for did::att_gt()", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("did")

  ## This is exactly the vignette's scenario: M = 6, 3 treated / 3 control.
  ## did::att_gt() requires >= 5 units in the never-treated group; 3 falls
  ## short, and previously this surfaced only as did's own opaque message
  ## ("The never-treated group is too small to serve as a reliable
  ## control..."), passed straight through by searchnet_did(). It is a
  ## scope limit of the design, not a defect in the panel, so the fix is a
  ## clearer error, not a change to how the panel is built.
  fx <- make_causal_fixture(M = 6L)
  panel <- searchnet_causal_panel(fx$env, shock_step = fx$shock_step,
                                   outcome = "utility",
                                   treated_actors = c(1, 2, 3))

  err <- tryCatch(searchnet_did(panel), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(err$message, "never-treated", ignore.case = TRUE)
  ## The message must name the actual requirement and observed counts,
  ## not just repeat did's unexplained wording.
  expect_match(err$message, "3 actor", fixed = TRUE)
  expect_match(err$message, "5 units", fixed = TRUE)
  expect_match(err$message, "notyettreated", fixed = TRUE)
})


test_that("searchnet_did() succeeds once the never-treated group meets did::att_gt()'s minimum size", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("did")

  ## Widening the design to 5 treated / 5 control actors clears did's
  ## internal `reqsize = 5` floor for every first_treat group (including
  ## the never-treated group), so the same code path that fails above
  ## should succeed here -- confirming the pre-flight check does not
  ## block designs that are actually large enough.
  fx <- make_causal_fixture(M = 10L)
  panel <- searchnet_causal_panel(fx$env, shock_step = fx$shock_step,
                                   outcome = "utility",
                                   treated_actors = c(1, 2, 3, 4, 5))

  att <- tryCatch(searchnet_did(panel), error = function(e) e)
  expect_false(inherits(att, "error"))
  expect_true(inherits(att, "MP"))
})
