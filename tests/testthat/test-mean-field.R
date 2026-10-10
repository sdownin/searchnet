###############################################################################
## test-mean-field.R
##
## Unit and integration tests for the SAOM-NK mean-field equilibrium solver
## (Property 5 of the SAOM-NK proof set, inst/proofs/PROOF_TABLE.md Part L;
## Property 5 in the paper: Brock--Durlauf reduction of the
## SAOM logit ministep).
##
## Tests:
##   1. beta_eff = 0  =>  unique m* = 0
##   2. beta_eff = 4T (above critical) => symmetric pair +/- m*
##   3. Convergence of fixed-point iteration within tolerance
##   4. Integration: simulation with theta_inPop above critical produces a
##      population mean within 10% of an analytical m* (empirical Property 5
##      check)
###############################################################################

# ===========================================================================
# 1. solve_mean_field: subcritical (beta_eff = 0)
# ===========================================================================
test_that("beta_eff = 0 yields unique fixed point at m* = 0", {

  ## theta_inPop = 0 => beta_eff = 0 < 2 * T  (subcritical)
  fp <- solve_mean_field(theta_inPop = 0, M = 10, T = 1, tol = 1e-10)

  expect_equal(fp$beta_eff, 0)
  expect_false(fp$above_critical)
  ## Single root at zero
  expect_length(fp$m_star, 1L)
  expect_equal(fp$m_star, 0, tolerance = 1e-8)
})


test_that("subcritical regime returns unique m* = 0 (small theta_inPop)", {

  ## (M-1)*theta_inPop/2 = (10-1)*0.05/2 = 0.225 < 2  (subcritical)
  fp <- solve_mean_field(theta_inPop = 0.05, M = 10, T = 1)
  expect_false(fp$above_critical)
  expect_length(fp$m_star, 1L)
  expect_equal(fp$m_star, 0, tolerance = 1e-6)
})


# ===========================================================================
# 2. solve_mean_field: supercritical (beta_eff = 4T)
# ===========================================================================
test_that("beta_eff = 4 (above critical) yields symmetric pair m* and -m*", {

  ## Choose theta_inPop so that beta_eff = 0.5 * (M - 1) * theta_inPop = 4
  ## => theta_inPop = 8 / (M - 1).  With M = 9, theta_inPop = 1.
  M <- 9
  theta <- 1.0
  fp <- solve_mean_field(theta_inPop = theta, M = M, T = 1)

  expect_equal(fp$beta_eff, 4)
  expect_true(fp$above_critical)

  ## Three fixed points: -m+, 0, +m+
  expect_length(fp$m_star, 3L)

  ## Symmetry: outer roots are negatives of each other
  outer_pair <- fp$m_star[c(1L, length(fp$m_star))]
  expect_equal(outer_pair[1], -outer_pair[2], tolerance = 1e-6)

  ## Outer roots strictly outside [-eps, eps]
  expect_true(all(abs(outer_pair) > 0.1))

  ## Trivial root present
  expect_true(any(abs(fp$m_star) < 1e-4))
})


test_that("supercritical roots solve self-consistency to high precision", {

  fp <- solve_mean_field(theta_inPop = 1.0, M = 9, T = 1, tol = 1e-12)
  for (m in fp$m_star) {
    rhs <- tanh(fp$beta_eff * m / (2 * fp$T))
    expect_equal(m, rhs, tolerance = 1e-6)
  }
})


# ===========================================================================
# 3. Fixed-point iteration converges within tolerance
# ===========================================================================
test_that("fixed-point iteration converges within stated tolerance", {

  tol <- 1e-8
  fp <- solve_mean_field(theta_inPop = 1.0, M = 9, T = 1,
                         tol = tol, max_iter = 5000L,
                         seeds = c(-0.99, 0, 0.99))

  ## Each seed should have produced a path; check final-step residual
  for (path in fp$convergence_path) {
    expect_true(length(path) >= 2L)
    final <- path[length(path)]
    rhs <- tanh(fp$beta_eff * final / (2 * fp$T))
    ## At the converged value the residual is below tol (or m=0 unstable
    ## but exact solution).
    expect_lt(abs(final - rhs), max(10 * tol, 1e-6))
  }
})


test_that("convergence is monotone toward a fixed point from each seed", {

  fp <- solve_mean_field(theta_inPop = 1.5, M = 9, T = 1,
                         seeds = c(0.99))
  path <- fp$convergence_path[[1]]
  ## Residual must shrink across iterations (allow small numerical wiggle)
  resids <- abs(path - tanh(fp$beta_eff * path / (2 * fp$T)))
  ## Monotone non-increasing in expectation; final < initial
  expect_lt(resids[length(resids)], resids[1])
})


test_that("solve_mean_field validates its arguments", {

  expect_error(solve_mean_field(theta_inPop = NA, M = 10, T = 1))
  expect_error(solve_mean_field(theta_inPop = 0.1, M = 1, T = 1))
  expect_error(solve_mean_field(theta_inPop = 0.1, M = 10, T = 0))
  expect_error(solve_mean_field(theta_inPop = 0.1, M = 10, T = 1, tol = 0))
  expect_error(solve_mean_field(theta_inPop = 0.1, M = 10, T = 1,
                                seeds = 1.5))
})


# ===========================================================================
# 4. Integration: simulation population mean within 10% of analytical m*
# ===========================================================================
##
## This is the empirical validation of Property 5: when the
## SAOM-NK ministep is run with theta_inPop above the Curie--Weiss threshold, the realized
## population mean must concentrate near one of the analytical mean-field
## fixed points.  We check |p_emp - p_binding| < 0.15 in adoption form.
##
## The test is wrapped in skip_if_not_installed("RSiena") and tryCatch
## guards so that environments without RSiena (or transient simulator
## flake) skip rather than fail.
test_that("simulation lands near the binding (Option B) fixed point", {

  skip_if_not_installed("RSiena")
  ## HISTORY. Until 2026-08-14 this test asserted the simulation lands
  ## within 0.10 (spin) of the LINEAR Curie-Weiss roots at a nominally
  ## supercritical coupling, and was gated behind NOT_CRAN so it never ran.
  ## When it finally ran it failed at 0.38. The diagnosis then read RSiena's
  ## inPop as square-root form and adopted p = sigmoid(beta*(h_b +
  ## theta*sqrt(M*p+1))) as the binding object (L16 "Option B").
  ##
  ## RE-DERIVATION, 2026-10-07. That identification was made on
  ## search_rsiena()'s replayed chain, whose terminal state is a draw near
  ## the initial density (audit docs/AUDIT_2026-10-06_replayed_trajectories.md,
  ## section 7.1), and RSiena 1.5.0's two-mode inPop is linear
  ## (target sum_j x_+j^2; inPopSqrt is a separate effect). On genuine paths
  ## the binding object is the mean-field fixed point of the ministep:
  ## Delta = h_b + theta*((M-1)p + 1) and the per-tie law F_N(Delta) of a
  ## choice among N toggles and no change (saomnk_inpop_self_consistency()).
  ## Checked 2026-10-07 at these settings: map 0.998; direct simulation of
  ## the ministep chain 0.998; saomnk_run() 1.00 (20 steps per actor, six
  ## seeds) and 0.99 (100 steps per actor). The square-root map gave 0.70.

  ## Source the API wrappers (not loaded by helper-setup.R)
  tryCatch(
    if (dir.exists(dir_r)) source(file.path(dir_r, "saomnk-api.R"), local = FALSE),
    error = function(e) stop(paste("api source failed:", e$message))
  )

  M <- 12
  N <- 6
  ## theta_inPop chosen well above critical:
  ## beta_eff = (M-1)*theta_inPop/2 = 11 * 0.6 / 2 = 3.3 > 2  (supercritical)
  theta_inPop <- 0.6

  env <- tryCatch(
    saomnk_env(M = M, N = N, density = 0.5, seed = 4242L),
    error = function(e) stop(paste("saomnk_env failed:", conditionMessage(e)))
  )

  ## The density coefficient is the process's external field (h_b = -1.0
  ## here) and enters the binding fixed point through
  ## diagnose_mean_field_fit(), which extracts it from the structure model.
  mod <- saomnk_model(density    = -1.0,
                      popularity = theta_inPop)

  ok <- tryCatch({
    saomnk_run(env, mod, steps_per_actor = 20L,
               seed = 4242L, verbose = FALSE)
    TRUE
  }, error = function(e) {
    stop(paste("saomnk_run failed:", conditionMessage(e)))
    FALSE
  })

  diag <- env$diagnose_mean_field_fit(T = 1,
                                       theta_inPop_override = theta_inPop)

  ## The diagnostic reports the binding object itself.
  expect_true(diag$above_critical)
  expect_true(is.finite(diag$p_binding))
  expect_true(is.finite(diag$discrepancy_adopt))

  ## The linear-CW reference is out of its validity regime here and the
  ## diagnostic should say so.
  expect_false(diag$in_BD_regime)

  ## TOLERANCE: the original 0.15 (adoption form), kept unchanged. Its
  ## derivation (q95 of |p_emp - E[p]| at M = 12, N = 6) was made for a
  ## stationary state near 1/2; here the fixed point is near 1, where the
  ## fluctuations are smaller, so 0.15 is conservative.
  expect_lt(abs(diag$discrepancy_adopt), 0.15)
})


test_that("Option C: sub-threshold linear-CW regime, where B&D applies", {

  skip_if_not_installed("RSiena")

  tryCatch(
    if (dir.exists(dir_r)) source(file.path(dir_r, "saomnk-api.R"), local = FALSE),
    error = function(e) stop(paste("api source failed:", e$message))
  )

  ## L16's Option C: the Brock-Durlauf linear object is a valid description
  ## of the live process only near p = 1/2 and below threshold. Construct
  ## that regime deliberately: small coupling (beta_eff = (M-1)*theta/2 =
  ## 0.825 < 2, sub-threshold) and a field chosen so the binding fixed
  ## point sits near 1/2. Here the Option B and linear-CW objects must
  ## agree, and the simulation must land near both.
  M <- 12
  N <- 6
  theta_inPop <- 0.15
  ## Field placing the binding fixed point at 1/2 (re-derived 2026-10-07
  ## for the linear two-mode inPop): the ministep law is symmetric about
  ## Delta = 0, so p = 1/2 when h_b + theta*((M-1)/2 + 1) = 0, i.e.
  ## h_b = -0.15 * 6.5 = -0.975. The square-root field used before,
  ## -theta*sqrt(M/2 + 1) = -0.397, puts the binding fixed point at 0.86,
  ## where genuine paths land (0.84 to 0.86); the sqrt map called it 0.50.
  h_b <- -theta_inPop * (1 + (M - 1) / 2)

  p_star <- saomnk_inpop_self_consistency(beta = 1,
                                          theta_inPop = theta_inPop,
                                          h_b = h_b, M = M, N = N)
  ## Regime sanity: the construction really does sit near 1/2.
  expect_lt(abs(p_star - 0.5), 0.05)

  env <- tryCatch(
    saomnk_env(M = M, N = N, density = 0.5, seed = 4343L),
    error = function(e) stop(paste("saomnk_env failed:", conditionMessage(e)))
  )
  mod <- saomnk_model(density = h_b, popularity = theta_inPop)
  tryCatch(
    saomnk_run(env, mod, steps_per_actor = 20L, seed = 4343L,
               verbose = FALSE),
    error = function(e) stop(paste("saomnk_run failed:", conditionMessage(e)))
  )

  diag <- env$diagnose_mean_field_fit(T = 1,
                                      theta_inPop_override = theta_inPop)

  expect_false(diag$above_critical)
  expect_true(diag$in_BD_regime)

  ## Same tolerance as the binding test above (0.15, unchanged). The old
  ## note called this ~2.5 SD from the zero-theta SD of the grand mean
  ## (0.059). With the coupling the stationary SD is larger: over ten other
  ## seeds (2026-10-07) the gap had SD 0.10 and max 0.18, so 0.15 is about
  ## 1.5 SD. This is a fixed-seed check (seed 4343: gap 0.01), not a
  ## seed-robust bound; a seed-robust bound at a single end state would be
  ## near 0.25.
  expect_lt(abs(diag$discrepancy_adopt), 0.15)
})
