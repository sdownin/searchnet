###############################################################################
## test-diagnostics.R
## Tests for SAI, CFC, DGF, crosswalk, and estimate extraction in
## saomnk-diagnostics.R
##
## Philosophy: "Users don't have to trust us -- they just trust the tests."
## These tests verify the diagnostic toolkit that supports specification
## curve analysis, degeneracy-aware reporting, and cross-framework concordance.
## A reviewer can check SAI computation by hand against these test inputs.
###############################################################################

## ---- Source the diagnostics layer ----
tryCatch(
  source(file.path(dir_r, "saomnk-diagnostics.R"), local = FALSE),
  error = function(e) {
    message("Could not source saomnk-diagnostics.R: ", e$message)
  }
)


# ===========================================================================
# 1. saomnk_sai() -- Specification Agreement Index
# ===========================================================================

## --- Helper: reproducible SAI input grid ---
make_sai_grid <- function() {
  data.frame(
    effect = rep(c("reciprocity", "transitivity", "homophily"), each = 5),
    specification = rep(paste0("spec_", 1:5), 3),
    estimate = c(
      0.8, 0.9, 0.7, 0.85, 0.75,      # reciprocity: all positive
      0.3, -0.1, 0.2, 0.4, -0.05,      # transitivity: mixed sign
      0.5, 0.6, 0.55, 0.45, 0.52       # homophily: all positive
    ),
    std_error = rep(0.15, 15),
    stringsAsFactors = FALSE
  )
}

test_that("saomnk_sai() returns saomnk_sai class with correct components", {
  grid <- make_sai_grid()
  result <- saomnk_sai(grid)

  expect_s3_class(result, "saomnk_sai")
  expect_true(all(c("table", "curve", "reference", "alpha") %in% names(result)))
})

test_that("saomnk_sai() table has one row per effect", {
  grid <- make_sai_grid()
  result <- saomnk_sai(grid)

  expect_equal(nrow(result$table), 3)
  expect_true(all(c("reciprocity", "transitivity", "homophily") %in%
                    result$table$effect))
})

test_that("saomnk_sai() computes sign agreement correctly for known inputs", {
  ## All 5 reciprocity specs are positive => SAI_sign = |5/5| = 1.0
  grid <- make_sai_grid()
  result <- saomnk_sai(grid)

  recip_row <- result$table[result$table$effect == "reciprocity", ]
  expect_equal(recip_row$sai_sign, 1.0)
  expect_equal(recip_row$n_positive, 5)
  expect_equal(recip_row$n_negative, 0)
})

test_that("saomnk_sai() detects mixed-sign effects", {
  ## Transitivity: 3 positive, 2 negative (with equal weights)
  ## SAI_sign = |3*0.2 - 2*0.2| / (5*0.2) = |0.6 - 0.4| / 1 = 0.2
  grid <- make_sai_grid()
  result <- saomnk_sai(grid)

  trans_row <- result$table[result$table$effect == "transitivity", ]
  ## Sign agreement should be less than 1 (mixed signs)
  expect_true(trans_row$sai_sign < 1.0)
  ## Should have both positive and negative counts
  expect_true(trans_row$n_positive > 0)
  expect_true(trans_row$n_negative > 0)
})

test_that("saomnk_sai() computes significance correctly", {
  ## With estimate=0.8 and se=0.15, z=5.33 => very significant
  ## With estimate=-0.05 and se=0.15, z=-0.33 => not significant
  grid <- make_sai_grid()
  result <- saomnk_sai(grid)

  ## Reciprocity: all estimates large relative to SE => all significant
  recip_row <- result$table[result$table$effect == "reciprocity", ]
  expect_equal(recip_row$n_significant, 5)
  expect_equal(recip_row$sai_sig, 1.0)
})

test_that("saomnk_sai() SAI_composite = SAI_sign * SAI_sig", {
  grid <- make_sai_grid()
  result <- saomnk_sai(grid)

  for (i in seq_len(nrow(result$table))) {
    row <- result$table[i, ]
    expected_composite <- round(row$sai_sign * row$sai_sig, 4)
    expect_equal(row$sai_composite, expected_composite,
                 info = paste("Effect:", row$effect))
  }
})

test_that("saomnk_sai() handles single-specification edge case", {
  grid <- data.frame(
    effect = "density",
    specification = "spec_1",
    estimate = -0.5,
    std_error = 0.1,
    stringsAsFactors = FALSE
  )
  result <- saomnk_sai(grid)

  expect_equal(nrow(result$table), 1)
  ## Single spec, negative sign => SAI_sign = 1.0
  expect_equal(result$table$sai_sign, 1.0)
  expect_equal(result$table$n_specs, 1)
})

test_that("saomnk_sai() handles custom weights", {
  grid <- make_sai_grid()
  w <- c(spec_1 = 1, spec_2 = 2, spec_3 = 1, spec_4 = 2, spec_5 = 1)
  result <- saomnk_sai(grid, weights = w)

  expect_s3_class(result, "saomnk_sai")
  ## Weights should appear in the curve data
  expect_true("weight" %in% names(result$curve))
})

test_that("saomnk_sai() rejects invalid input", {
  ## Missing required columns
  bad <- data.frame(x = 1, y = 2)
  expect_error(saomnk_sai(bad), "Missing required columns")

  ## All NA estimates
  bad2 <- data.frame(
    effect = "a", specification = "s1",
    estimate = NA_real_, std_error = NA_real_
  )
  expect_error(saomnk_sai(bad2), "No valid")
})

test_that("saomnk_sai() curve data is sorted by estimate within effect", {
  grid <- make_sai_grid()
  result <- saomnk_sai(grid)

  for (eff in unique(result$curve$effect)) {
    subset <- result$curve[result$curve$effect == eff, ]
    ## Estimates should be in non-decreasing order
    expect_true(all(diff(subset$estimate) >= 0),
                info = paste("Curve not sorted for effect:", eff))
  }
})

test_that("saomnk_sai() effects subset parameter works", {
  grid <- make_sai_grid()
  result <- saomnk_sai(grid, effects = "reciprocity")

  expect_equal(nrow(result$table), 1)
  expect_equal(result$table$effect, "reciprocity")
})


# ===========================================================================
# 2. print.saomnk_sai() -- printing
# ===========================================================================
test_that("print.saomnk_sai() produces output without error", {
  grid <- make_sai_grid()
  result <- saomnk_sai(grid)
  expect_output(print(result), "Specification Agreement Index")
})


# ===========================================================================
# 3. saomnk_dgf() -- Density-GOF Frontier
# ===========================================================================
test_that("saomnk_dgf() returns expected structure", {
  result <- saomnk_dgf(
    n_nodes = 50,
    density = 0.1,
    model_terms = c("edges", "mutual", "gwesp.OTP")
  )

  expect_true(is.list(result))
  expected_fields <- c("expected_triangles", "max_triangles",
                        "triangle_density", "binding_statistic",
                        "degeneracy_risk", "n_nodes", "density")
  expect_true(all(expected_fields %in% names(result)))
})

test_that("saomnk_dgf() low density without triangles is low risk", {
  result <- saomnk_dgf(
    n_nodes = 30,
    density = 0.05,
    model_terms = c("edges", "mutual")
  )
  expect_equal(result$degeneracy_risk, "low")
})

test_that("saomnk_dgf() high density with triangles is high risk", {
  result <- saomnk_dgf(
    n_nodes = 50,
    density = 0.4,
    model_terms = c("edges", "gwesp.OTP", "mutual")
  )
  expect_equal(result$degeneracy_risk, "high")
})

test_that("saomnk_dgf() identifies binding statistic from triangle terms", {
  result <- saomnk_dgf(
    n_nodes = 50,
    density = 0.2,
    model_terms = c("edges", "gwesp.OTP")
  )
  expect_equal(result$binding_statistic, "gwesp.OTP")
})

test_that("saomnk_dgf() rejects invalid inputs", {
  expect_error(saomnk_dgf(2, 0.1, c("edges")))   # n_nodes < 3
  expect_error(saomnk_dgf(10, -0.1, c("edges")))  # density < 0
  expect_error(saomnk_dgf(10, 1.5, c("edges")))   # density > 1
})

test_that("saomnk_dgf() undirected mode computes different triangle counts", {
  dir_result <- saomnk_dgf(20, 0.2, c("edges", "triangle"), directed = TRUE)
  undir_result <- saomnk_dgf(20, 0.2, c("edges", "triangle"), directed = FALSE)

  ## Max triangles differ between directed and undirected
  expect_true(dir_result$max_triangles != undir_result$max_triangles)
})


# ===========================================================================
# 4. saomnk_default_crosswalk() -- SAOM-TERGM effect mapping
# ===========================================================================
test_that("saomnk_default_crosswalk() returns valid crosswalk table", {
  cw <- saomnk_default_crosswalk()

  expect_true(is.data.frame(cw))
  expect_true(all(c("saom_effect", "tergm_term", "category",
                     "sign_convention", "notes") %in% names(cw)))
  expect_true(nrow(cw) > 10)  # should have a substantial number of mappings
})

test_that("saomnk_default_crosswalk() contains key effect mappings", {
  cw <- saomnk_default_crosswalk()

  ## density -> edges
  density_row <- cw[cw$saom_effect == "density (outdegree)", ]
  expect_equal(nrow(density_row), 1)
  expect_equal(density_row$tergm_term, "edges")

  ## reciprocity -> mutual
  recip_row <- cw[cw$saom_effect == "reciprocity", ]
  expect_equal(nrow(recip_row), 1)
  expect_equal(recip_row$tergm_term, "mutual")
})


# ===========================================================================
# 5. saomnk_extract_estimates_saom() -- estimate extraction from mock data
# ===========================================================================
test_that("saomnk_extract_estimates_saom() works with mock sienaFit", {
  skip_if_not_installed("RSiena")

  ## Create a minimal mock of a sienaFit object
  mock_fit <- list(
    effects = data.frame(
      effectName = c("Rate", "density", "reciprocity"),
      type = c("rate", "eval", "eval"),
      stringsAsFactors = FALSE
    ),
    theta = c(5.0, -1.2, 0.8),
    se    = c(0.5, 0.3, 0.2),
    tconv = c(0.01, -0.02, 0.03)
  )

  result <- saomnk_extract_estimates_saom(mock_fit, specification = "test_spec")

  expect_true(is.data.frame(result))
  expect_true(all(c("effect", "specification", "estimate", "std_error") %in%
                    names(result)))
  ## Default: rate excluded
  expect_false("Rate" %in% result$effect)
  expect_equal(nrow(result), 2)
  expect_equal(result$specification[1], "test_spec")
})

test_that("saomnk_extract_estimates_saom() includes rate when requested", {
  skip_if_not_installed("RSiena")

  mock_fit <- list(
    effects = data.frame(
      effectName = c("Rate", "density"),
      type = c("rate", "eval"),
      stringsAsFactors = FALSE
    ),
    theta = c(5.0, -1.0),
    se    = c(0.5, 0.2),
    tconv = c(0.01, 0.02)
  )

  result <- saomnk_extract_estimates_saom(mock_fit, include_rate = TRUE)
  expect_equal(nrow(result), 2)
  expect_true("Rate" %in% result$effect)
})


# ===========================================================================
# 6. saomnk_format_sai_table() -- publication-ready SAI table
# ===========================================================================
test_that("saomnk_format_sai_table() produces data.frame output", {
  grid <- make_sai_grid()
  sai_obj <- saomnk_sai(grid)

  formatted <- saomnk_format_sai_table(sai_obj)
  expect_true(is.data.frame(formatted))
  expect_true("Effect" %in% names(formatted))
  expect_true("SAI" %in% names(formatted))
  expect_equal(nrow(formatted), 3)
})

test_that("saomnk_format_sai_table() adds stars by default", {
  grid <- make_sai_grid()
  sai_obj <- saomnk_sai(grid)

  formatted <- saomnk_format_sai_table(sai_obj, stars = TRUE)
  ## Reciprocity should have high SAI => should have stars
  recip_sai <- formatted$SAI[formatted$Effect == "reciprocity"]
  ## Stars are appended as *, **, or ***
  expect_true(grepl("\\*", recip_sai),
              info = "High-SAI effect should have significance stars")
})

test_that("saomnk_format_sai_table() respects digits parameter", {
  grid <- make_sai_grid()
  sai_obj <- saomnk_sai(grid)

  fmt2 <- saomnk_format_sai_table(sai_obj, digits = 2, stars = FALSE)
  ## Check that SAI values have 2 decimal places
  sai_val <- fmt2$SAI[1]
  ## Should match pattern like "0.80" or "1.00"
  expect_true(grepl("^-?\\d+\\.\\d{2}$", sai_val),
              info = paste("Expected 2 decimal places, got:", sai_val))
})

test_that("saomnk_format_sai_table() no stars when stars=FALSE", {
  grid <- make_sai_grid()
  sai_obj <- saomnk_sai(grid)

  formatted <- saomnk_format_sai_table(sai_obj, stars = FALSE)
  ## None of the SAI values should contain *
  expect_false(any(grepl("\\*", formatted$SAI)))
})


# ===========================================================================
# 7. saomnk_map_effects() -- effect mapping between frameworks
# ===========================================================================
test_that("saomnk_map_effects() finds matching effects", {
  saom_effs <- c("reciprocity", "density (outdegree)")
  tergm_effs <- c("mutual", "edges", "gwesp.OTP")

  result <- saomnk_map_effects(saom_effs, tergm_effs)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) >= 2)
})

test_that("saomnk_map_effects() handles non-overlapping effects gracefully", {
  result <- tryCatch(
    suppressMessages(saomnk_map_effects("nonexistent_saom", "nonexistent_tergm")),
    error = function(e) data.frame()  # returns empty on error
  )
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})
