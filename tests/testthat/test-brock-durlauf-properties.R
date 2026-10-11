###############################################################################
## test-brock-durlauf-properties.R
##
## Property tests for the Brock and Durlauf (2001) mean-field utilities in
## R/searchnet-brock-durlauf.R (PROOF_TABLE.md Part L, Property 5; rows L4, L6,
## L16). Each block checks a property the roxygen documents, on small
## deterministic cases, against an independent recomputation rather than a
## recorded number:
##
##   bd_self_consistency            roots solve m = tanh(beta h + beta J m)
##   bd_equilibrium_count           Curie-Weiss threshold at beta J = 1
##   saomnk_social_multiplier       1 / (1 - beta J (1 - m^2)) IS dm*/dh / (beta (1 - m^2))
##   bd_landau_steepness            closed forms, capping, critical point
##   verify_brock_durlauf_reduction one tiny SAOM-NK run (skip_on_cran)
###############################################################################

## Residual of the spin-form self-consistency equation, written out here so
## the tests do not lean on the package's own internal helper.
bd_resid <- function(m, beta, J, h) m - tanh(beta * h + beta * J * m)


# ---------------------------------------------------------------------------
# 1. bd_self_consistency
# ---------------------------------------------------------------------------

test_that("every root from the all-roots scan satisfies the equation to tol", {
  ## Before the fix (searchnet <= 0.12.9) the roots were rounded to 5 decimals
  ## after uniroot() for de-duplication, leaving residuals near 3e-6 despite
  ## tol = 1e-10. They are now de-duplicated without rounding and polished.
  cases <- list(c(1, 2, 0), c(1, 0.5, 0.2), c(1, 2, 0.05), c(0.7, 3, -0.1),
                c(1, 2, 0.5), c(1, -1.5, 0.3), c(2, 4, -0.2))
  for (tol in c(1e-10, 1e-6)) {
    for (p in cases) {
      lab <- sprintf("(%s), tol = %g", paste(p, collapse = ", "), tol)
      expect_no_warning(r <- bd_self_consistency(beta = p[1], J = p[2], h = p[3],
                                                 tol = tol))
      expect_true(all(abs(bd_resid(r, p[1], p[2], p[3])) <= tol), info = lab)
      expect_true(all(r >= -1 & r <= 1), info = lab)
      expect_true(all(diff(r) > 0), info = lab)
    }
  }
})

test_that("subcritical h = 0 gives one root at 0; supercritical gives three, symmetric", {
  for (J in c(-2, -0.5, 0, 0.3, 0.9)) {
    r <- bd_self_consistency(beta = 1, J = J, h = 0)
    expect_length(r, 1L)
    expect_equal(r, 0, tolerance = 1e-8)
  }
  for (bJ in c(1.2, 2, 4)) {
    r <- bd_self_consistency(beta = 1, J = bJ, h = 0)
    expect_length(r, 3L)
    expect_true(0 %in% r)
    expect_equal(r, -rev(r), tolerance = 1e-12)
    expect_true(r[3] > 0)
  }
})

test_that("the fixed-point branch returns a stable root from the all-roots set", {
  ## Coordination (J >= 0) and mild anti-coordination (beta |J| < 1). The
  ## strong anti-coordination case is the next block.
  cases <- list(c(1, 0.5, 0), c(1, 0.5, 0.2), c(1, 2, 0.1), c(1, 2, -0.1),
                c(2, 1, 0.05), c(1, -0.8, 0.3), c(0.5, -1.5, 0.2))
  for (p in cases) {
    lab <- paste(p, collapse = ", ")
    m <- bd_self_consistency(beta = p[1], J = p[2], h = p[3], all_roots = FALSE)
    expect_length(m, 1L)
    ## The iteration branch is not rounded: it satisfies the equation tightly.
    expect_lt(abs(bd_resid(m, p[1], p[2], p[3])), 1e-8)
    all_r <- bd_self_consistency(beta = p[1], J = p[2], h = p[3])
    expect_true(min(abs(all_r - m)) < 1e-5, info = lab)
    ## Linear stability of the map m -> tanh(beta h + beta J m).
    expect_lt(abs(p[1] * p[2] * (1 - m^2)), 1, label = lab)
  }
})

test_that("the fixed-point branch returns an equilibrium under strong anti-coordination", {
  ## With J < 0 and beta |J| > 1 the unique root has map slope
  ## beta J (1 - m^2) < -1, so plain iteration falls into a 2-cycle. Before the
  ## fix (searchnet <= 0.12.9) the branch iterated anyway and returned a
  ## non-root after warning:
  ##   bd_self_consistency(1, -1.5, 0.3, all_roots = FALSE) -> -0.78187
  ##     (residual 1.68); the unique root is 0.11977, slope -1.478.
  ##   saomnk_social_multiplier(beta = 1.5, J = -1, h = 0.3) inherited it:
  ##     m_star -0.725, multiplier 0.584 instead of 0.17922 and 0.40786.
  ## J < 0 is now solved by bracketing the unique root on [-1, 1].
  tol <- 1e-10
  expect_no_warning(m <- bd_self_consistency(1, -1.5, 0.3, all_roots = FALSE))
  expect_lte(abs(bd_resid(m, 1, -1.5, 0.3)), tol)
  expect_equal(m, 0.11977, tolerance = 1e-4)
  root <- bd_self_consistency(1, -1.5, 0.3)
  expect_length(root, 1L)
  expect_lt(abs(m - root), tol)
  ## Map slope at the root is below -1: iteration alone could not have found it.
  expect_lt(-1.5 * (1 - m^2), -1)

  for (p in list(c(1, -1.05, 0.3), c(1, -3, 0), c(2, -5, -0.4), c(1, -1.5, 2))) {
    lab <- paste(p, collapse = ", ")
    expect_no_warning(mp <- bd_self_consistency(p[1], p[2], p[3], all_roots = FALSE))
    expect_lte(abs(bd_resid(mp, p[1], p[2], p[3])), tol, label = lab)
    expect_lt(abs(mp - bd_self_consistency(p[1], p[2], p[3])), tol, label = lab)
  }

  expect_no_warning(sm <- saomnk_social_multiplier(beta = 1.5, J = -1, h = 0.3))
  root2 <- bd_self_consistency(1.5, -1, 0.3)
  expect_lt(abs(sm$m_star - root2), tol)
  expect_equal(sm$m_star, 0.17922, tolerance = 1e-4)
  expect_equal(sm$multiplier, 1 / (1 + 1.5 * (1 - root2^2)))
  expect_equal(sm$multiplier, 0.40786, tolerance = 1e-4)
})

test_that("the coordination branch returns a verified root, including near criticality", {
  tol <- 1e-10
  ## beta J just above 1 converges slowly; the result is still polished.
  for (p in list(c(1, 1.0001, 0), c(1, 1.0001, 1e-6), c(1, 1, 0.01), c(1, 2, 0.5))) {
    lab <- paste(p, collapse = ", ")
    expect_no_warning(m <- bd_self_consistency(p[1], p[2], p[3], x0 = 0.3,
                                               max_iter = 50, all_roots = FALSE))
    expect_lte(abs(bd_resid(m, p[1], p[2], p[3])), tol, label = lab)
  }
  ## x0 selects the basin: from x0 = -0.9 at (1, 2, 0.05) the lower root.
  m_lo <- bd_self_consistency(1, 2, 0.05, x0 = -0.9, all_roots = FALSE)
  expect_lt(abs(m_lo - min(bd_self_consistency(1, 2, 0.05))), tol)
})

test_that("roots are odd in h: roots(-h) = -roots(h)", {
  for (p in list(c(1, 2, 0.05), c(1, 0.5, 0.3), c(1.5, 1, 0.02), c(1, 3, 0.4))) {
    r_pos <- bd_self_consistency(beta = p[1], J = p[2], h = p[3])
    r_neg <- bd_self_consistency(beta = p[1], J = p[2], h = -p[3])
    expect_equal(sort(r_neg), sort(-r_pos), tolerance = 1e-6)
  }
})

test_that("beta = 0 gives m = 0 in both branches", {
  expect_equal(bd_self_consistency(beta = 0, J = 3, h = 2), 0)
  expect_equal(bd_self_consistency(beta = 0, J = 3, h = 2, all_roots = FALSE), 0)
})

test_that("bd_self_consistency rejects invalid input", {
  expect_error(bd_self_consistency(beta = -0.1, J = 1, h = 0))
  expect_error(bd_self_consistency(beta = 1, J = 1, h = 0, x0 = 1.5,
                                   all_roots = FALSE))
  expect_error(bd_self_consistency(beta = 1, J = 1, h = 0, x0 = -1.01))
  expect_error(bd_self_consistency(beta = c(1, 2), J = 1, h = 0))
  expect_error(bd_self_consistency(beta = 1, J = "a", h = 0))
})


# ---------------------------------------------------------------------------
# 2. bd_equilibrium_count
# ---------------------------------------------------------------------------

test_that("the h = 0 count switches from 1 to 3 at beta J = 1", {
  expect_identical(bd_equilibrium_count(beta = 1, J = 0.99), 1L)
  expect_identical(bd_equilibrium_count(beta = 1, J = 1.01), 3L)
  expect_identical(bd_equilibrium_count(beta = 2, J = 0.49), 1L)
  expect_identical(bd_equilibrium_count(beta = 2, J = 0.51), 3L)
  for (J in c(-3, -0.1, 0)) expect_identical(bd_equilibrium_count(beta = 1, J = J), 1L)
})

test_that("the h = 0 count agrees with the number of roots away from beta J = 1", {
  for (bJ in c(-2, -0.5, 0, 0.4, 0.8, 0.95, 1.05, 1.3, 2, 3, 5)) {
    expect_identical(bd_equilibrium_count(beta = 1, J = bJ),
                     length(bd_self_consistency(beta = 1, J = bJ, h = 0)),
                     label = sprintf("beta J = %s", bJ))
  }
})

test_that("for h != 0 the count is the root count: small field keeps 3, large field leaves 1", {
  expect_identical(bd_equilibrium_count(beta = 1, J = 3, h = 0.05), 3L)
  expect_identical(bd_equilibrium_count(beta = 1, J = 2, h = 1), 1L)
  expect_identical(bd_equilibrium_count(beta = 1, J = 0.5, h = 0.2), 1L)
  ## The field that destroys multiplicity at beta = 1, beta J > 1 is
  ##   h_c = J m_c - atanh(m_c),  m_c = sqrt(1 - 1 / J)   (the spinodal),
  ## 0.53284 at J = 2. The count must switch there.
  m_c <- sqrt(1 - 1 / 2); h_c <- 2 * m_c - atanh(m_c)
  expect_identical(bd_equilibrium_count(beta = 1, J = 2, h = h_c - 0.01), 3L)
  expect_identical(bd_equilibrium_count(beta = 1, J = 2, h = h_c + 0.01), 1L)
  expect_identical(bd_equilibrium_count(beta = 1, J = 2, h = -(h_c + 0.01)), 1L)
  for (h in c(-0.3, -0.05, 0.05, 0.3, 1)) {
    expect_identical(bd_equilibrium_count(beta = 1, J = 2.5, h = h),
                     length(bd_self_consistency(beta = 1, J = 2.5, h = h)))
  }
  expect_error(bd_equilibrium_count(beta = -1, J = 1))
})

test_that("the documented examples bd_equilibrium_count(1, 2, h = 0.5 / 0.6) give 3 / 1", {
  ## The roxygen example annotated h = 0.5 as "# 1 (field destroys mult.)"
  ## through searchnet 0.12.9. The critical field at beta = 1, J = 2 is
  ## 0.53284 > 0.5, so 3 is right; the example now reads "# 3" and adds
  ## h = 0.6, above the critical field, as the "# 1" case.
  expect_identical(bd_equilibrium_count(beta = 1, J = 2.0, h = 0.5), 3L)
  expect_identical(bd_equilibrium_count(beta = 1, J = 2.0, h = 0.6), 1L)
  ## The Rd carries the corrected annotations.
  rd_path <- testthat::test_path("..", "..", "man", "bd_equilibrium_count.Rd")
  if (file.exists(rd_path)) {
    rd_txt <- readLines(rd_path, warn = FALSE)
    expect_true(any(grepl("h = 0\\.5\\)\\s+# 3", rd_txt)))
    expect_false(any(grepl("h = 0\\.5\\)\\s+# 1", rd_txt)))
  }
})


# ---------------------------------------------------------------------------
# 3. saomnk_social_multiplier
# ---------------------------------------------------------------------------

test_that("the multiplier equals 1 / (1 - beta J (1 - m*^2)) at the stable root", {
  for (p in list(c(1, 0.5, 0), c(1, 0.5, 0.2), c(1, 2, 0.1), c(0.8, -1, 0.3))) {
    res <- saomnk_social_multiplier(beta = p[1], J = p[2], h = p[3])
    expect_named(res, c("multiplier", "m_star", "bd_J_spin", "bd_h_spin",
                        "warning_near_threshold"))
    m <- bd_self_consistency(beta = p[1], J = p[2], h = p[3], all_roots = FALSE)
    expect_equal(res$m_star, m)
    expect_equal(res$multiplier, 1 / (1 - p[1] * p[2] * (1 - m^2)))
    expect_equal(res$bd_J_spin, p[2])
    expect_equal(res$bd_h_spin, p[3])
  }
})

test_that("the multiplier is the response dm*/dh scaled by beta (1 - m*^2)", {
  ## Implicit differentiation of m = tanh(beta h + beta J m):
  ##   dm/dh = beta (1 - m^2) / (1 - beta J (1 - m^2)) = beta (1 - m^2) * multiplier.
  ## Checked by central difference on the stable root.
  eps <- 1e-4
  for (p in list(c(1, 0.5, 0), c(1, 0.5, 0.2), c(1, 2, 0.1), c(0.8, 0.9, -0.15),
                 c(1, -1, 0.3))) {
    root_at <- function(h) bd_self_consistency(beta = p[1], J = p[2], h = h,
                                               tol = 1e-14, max_iter = 1e5,
                                               all_roots = FALSE)
    dm_dh <- (root_at(p[3] + eps) - root_at(p[3] - eps)) / (2 * eps)
    res <- saomnk_social_multiplier(beta = p[1], J = p[2], h = p[3])
    expect_equal(dm_dh, p[1] * (1 - res$m_star^2) * res$multiplier,
                 tolerance = 1e-5, label = paste(p, collapse = ", "))
  }
})

test_that("J = 0 gives multiplier 1; the threshold flag fires near beta J = 1", {
  r0 <- saomnk_social_multiplier(beta = 1, J = 0, h = 0.4)
  expect_equal(r0$multiplier, 1)
  expect_false(r0$warning_near_threshold)

  near <- saomnk_social_multiplier(beta = 1, J = 0.95, h = 0)
  expect_equal(near$m_star, 0)
  expect_equal(near$multiplier, 20)
  expect_true(near$warning_near_threshold)

  far <- saomnk_social_multiplier(beta = 1, J = 0.5, h = 0)
  expect_equal(far$multiplier, 2)
  expect_false(far$warning_near_threshold)
})

test_that("the env path reads theta_density, theta_cong and the bipartite matrix", {
  B <- matrix(c(1, 0, 1, 1,
                0, 0, 1, 0,
                1, 1, 1, 0), nrow = 3, byrow = TRUE)
  mock <- list(theta_density = -0.4, theta_cong = -2, bipartite_matrix = B)
  res <- saomnk_social_multiplier(env = mock)

  expect_equal(res$bd_h_spin, 0.4)          # -theta_density
  expect_equal(res$bd_J_spin, 0.5)          # -theta_cong / 4 (Rb4)
  expect_equal(res$m_star, 2 * mean(B) - 1)
  expect_equal(res$multiplier, 1 / (1 - 1 * 0.5 * (1 - res$m_star^2)))

  ## beta is honored on this path too.
  res2 <- saomnk_social_multiplier(env = mock, beta = 1.5)
  expect_equal(res2$multiplier, 1 / (1 - 1.5 * 0.5 * (1 - res$m_star^2)))

  ## Same extraction from an environment object, not only a list.
  e <- new.env()
  e$theta_density <- -0.4; e$theta_cong <- -2; e$bipartite_matrix <- B
  expect_equal(saomnk_social_multiplier(env = e), res)

  ## No bipartite matrix: warns and falls back to m_star = 0.
  expect_warning(r3 <- saomnk_social_multiplier(env = list(theta_density = 0,
                                                           theta_cong = 0)),
                 "bipartite_matrix unavailable")
  expect_equal(r3$m_star, 0)
  expect_equal(r3$multiplier, 1)
})


# ---------------------------------------------------------------------------
# 4. bd_landau_steepness
# ---------------------------------------------------------------------------

test_that("b and the steepness match their closed forms", {
  for (p in list(c(1, 2, 0.5, 1, 0.05), c(1, 1.5, -0.3, 2, 0.1),
                 c(2, 1, 0, 0.5, 0.2), c(1, 0.5, 0.2, 1, 0.05))) {
    beta <- p[1]; J <- p[2]; m <- p[3]; tau <- p[4]; dm <- p[5]
    res <- bd_landau_steepness(beta = beta, J = J, m_star = m, tau = tau,
                               delta_m = dm)
    b <- 2 * tau * (1 + 3 * m^2) / (1 - m^2)^3
    expect_equal(res$b, b)
    expect_equal(res$steepness, 2 + b * dm^2 / (2 * beta * J - 2))
    expect_false(res$capped)
  }
})

test_that("b is even in m* and increasing in |m*|", {
  m <- seq(0, 0.9, by = 0.05)
  b_pos <- vapply(m, function(x) bd_landau_steepness(1, 2, x)$b, numeric(1))
  b_neg <- vapply(m, function(x) bd_landau_steepness(1, 2, -x)$b, numeric(1))
  expect_equal(b_pos, b_neg)
  expect_true(all(diff(b_pos) > 0))
  expect_equal(b_pos[1], 2)   # tau = 1, m* = 0
})

test_that("b is capped with a warning at or near |m*| = 1; the critical point gives Inf", {
  expect_warning(r1 <- bd_landau_steepness(1, 2, m_star = 1), "\\|m_star\\| >= 1")
  expect_equal(r1$b, 1e6); expect_true(r1$capped)
  expect_warning(r2 <- bd_landau_steepness(1, 2, m_star = -1.2), "\\|m_star\\| >= 1")
  expect_true(r2$capped)
  expect_warning(r3 <- bd_landau_steepness(1, 2, m_star = 0.995), "close to 1")
  expect_equal(r3$b, 1e6); expect_true(r3$capped)
  expect_equal(r3$steepness, 2 + 1e6 * 0.05^2 / 2)

  expect_warning(r4 <- bd_landau_steepness(beta = 1, J = 1, m_star = 0.2),
                 "critical point")
  expect_identical(r4$steepness, Inf)
  expect_false(r4$capped)
  expect_warning(r5 <- bd_landau_steepness(beta = 0.5, J = 2, m_star = 0),
                 "critical point")
  expect_identical(r5$steepness, Inf)
})


# ---------------------------------------------------------------------------
# 5. verify_brock_durlauf_reduction (one tiny SAOM-NK run)
# ---------------------------------------------------------------------------

.bd_cache <- new.env(parent = emptyenv())

bd_verify_run <- function(key, ...) {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  if (is.null(.bd_cache[[key]]))
    .bd_cache[[key]] <- verify_brock_durlauf_reduction(
      M_seq = 12, n_components = 4, n_steps = 30, n_replicates = 2,
      seed = 2026L, ...)
  .bd_cache[[key]]
}

test_that("verify_brock_durlauf_reduction returns the documented columns", {
  v <- bd_verify_run("default", J_b = 0.5)
  expect_s3_class(v, "data.frame")
  expect_identical(names(v),
                   c("M", "n_ok", "m_b_empirical", "m_BD_analytical",
                     "m_saomnk_analytical", "abs_error_BD", "abs_error_saomnk",
                     "in_BD_regime", "log_M", "log_error_BD", "log_error_saomnk",
                     "qualitative_match"))
  expect_equal(nrow(v), 1L)
  expect_equal(v$M, 12L)
  expect_equal(v$n_ok, 2L)
  expect_equal(v$log_M, log10(12))
  expect_true(v$m_b_empirical >= 0 && v$m_b_empirical <= 1)
  expect_equal(v$abs_error_BD, abs(v$m_b_empirical - v$m_BD_analytical))
  expect_equal(v$abs_error_saomnk, abs(v$m_b_empirical - v$m_saomnk_analytical))
  expect_equal(nrow(attr(v, "failures")), 0L)
})

test_that("the default h_b = -J_b/2 puts the B&D fixed point at 1/2, inside the regime", {
  v <- bd_verify_run("default", J_b = 0.5)
  J_b <- 0.5; h_b <- -J_b / 2
  m_spin <- bd_self_consistency(1, J_b / 4, h_b / 2 + J_b / 4, all_roots = FALSE)
  expect_equal(v$m_BD_analytical, (m_spin + 1) / 2)
  expect_equal(v$m_BD_analytical, 0.5)
  expect_true(v$in_BD_regime)
  ## m = 1/2 is a fixed point of the SAOM-inPop map too (L15).
  expect_equal(v$m_saomnk_analytical, 0.5, tolerance = 1e-8)
  expect_lt(abs(v$m_b_empirical - v$m_saomnk_analytical), 0.1)
})

test_that("coordination (J_b = 2, h_b = 0) drives adoption above 1/2, near the SAOM-inPop fixed point", {
  v <- bd_verify_run("coord", J_b = 2, h_b = 0)
  m_spin <- bd_self_consistency(1, 2 / 4, 0 / 2 + 2 / 4, all_roots = FALSE)
  expect_equal(v$m_BD_analytical, (m_spin + 1) / 2)
  expect_gt(v$m_BD_analytical, 0.5)
  expect_gt(v$m_b_empirical, 0.5)
  expect_true(v$qualitative_match)
  ## The documentation says the empirical mean should match the SAOM-inPop
  ## reference in any regime where the simulation reaches stationarity.
  expect_lt(abs(v$m_b_empirical - v$m_saomnk_analytical), 0.1)
})

test_that("the same seed reproduces m_b_empirical exactly", {
  v1 <- bd_verify_run("coord", J_b = 2, h_b = 0)
  v2 <- verify_brock_durlauf_reduction(M_seq = 12, n_components = 4,
                                       n_steps = 30, n_replicates = 2,
                                       seed = 2026L, J_b = 2, h_b = 0)
  expect_identical(v2$m_b_empirical, v1$m_b_empirical)
})

test_that("verify_brock_durlauf_reduction validates its arguments", {
  expect_error(verify_brock_durlauf_reduction(M_seq = 1))
  expect_error(verify_brock_durlauf_reduction(M_seq = 12, n_components = 2))
  expect_error(verify_brock_durlauf_reduction(M_seq = 12, beta = -1))
  expect_error(verify_brock_durlauf_reduction(M_seq = 12, n_replicates = 0))
})
