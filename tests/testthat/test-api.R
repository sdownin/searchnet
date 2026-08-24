###############################################################################
## test-api.R
## Tests for the user-friendly API wrappers in saomnk-api.R
##
## Philosophy: "Users don't have to trust us -- they just trust the tests."
## These tests verify that every public API function produces correct output
## types and structures, so a skeptical reviewer can read this file and
## confirm the wrapper layer works as documented.
###############################################################################

## ---- Source the API layer (helper-setup.R already loads base classes) ----
tryCatch(
  source(file.path(dir_r, "saomnk-api.R"), local = FALSE),
  error = function(e) {
    message("Could not source saomnk-api.R: ", e$message)
  }
)

# ===========================================================================
# 1. saomnk_env() -- environment construction
# ===========================================================================
test_that("saomnk_env() creates a valid SaomNkRSienaBiEnv object", {
  env <- saomnk_env(M = 4, N = 6, seed = 42)
  expect_s3_class(env, "SaomNkRSienaBiEnv")
  expect_equal(env$M, 4L)
  expect_equal(env$N, 6L)
})

test_that("saomnk_env() respects density parameter", {
  env <- saomnk_env(M = 4, N = 6, density = 1.0, seed = 99)
  expect_true(all(env$bipartite_matrix == 1))
})

test_that("saomnk_env() with density=0 gives all-zero bipartite", {
  env <- saomnk_env(M = 4, N = 6, density = 0, seed = 99)
  expect_true(all(env$bipartite_matrix == 0))
})

test_that("saomnk_env() stores name when provided", {
  env <- saomnk_env(M = 3, N = 5, name = "my_test_env")
  expect_equal(env$SIM_NAME, "my_test_env")
})

test_that("saomnk_env() rejects invalid parameters", {
  expect_error(saomnk_env(M = -1, N = 5))
  expect_error(saomnk_env(M = 3, N = 0))
  expect_error(saomnk_env(M = 3, N = 5, density = 1.5))
  expect_error(saomnk_env(M = 3, N = 5, density = -0.1))
})


# ===========================================================================
# 2. saomnk_model() -- structure model construction
# ===========================================================================
test_that("saomnk_model() produces saomnk_model class with correct structure", {
  mod <- saomnk_model(density = -1)

  expect_s3_class(mod, "saomnk_model")
  expect_true(is.list(mod))
  expect_true("dv_bipartite" %in% names(mod))

  dv <- mod$dv_bipartite
  expect_true(all(c("effects", "coCovars", "coDyadCovars") %in% names(dv)))
})

test_that("saomnk_model() density-only model has one effect", {
  mod <- saomnk_model(density = -0.5)
  effs <- mod$dv_bipartite$effects
  expect_length(effs, 1)
  expect_equal(effs[[1]]$effect, "density")
  expect_equal(effs[[1]]$parameter, -0.5)
})

test_that("saomnk_model() with popularity/scope adds effects", {
  mod <- saomnk_model(density = -1, popularity = 0.3, scope = 0.1)
  effs <- mod$dv_bipartite$effects
  ## Should have 3 effects: density + inPop + outAct
  expect_length(effs, 3)
  effect_names <- vapply(effs, `[[`, character(1), "effect")
  expect_true("density" %in% effect_names)
  expect_true("inPop" %in% effect_names)
  expect_true("outAct" %in% effect_names)
})

test_that("saomnk_model() with strategies creates coCovars", {
  strat <- list(egoX = c(-1, 0, 1, -1, 0, 1))
  mod <- saomnk_model(density = -1, strategies = strat)

  covs <- mod$dv_bipartite$coCovars
  expect_length(covs, 1)
  expect_equal(covs[[1]]$effect, "egoX")
  expect_equal(covs[[1]]$x, c(-1, 0, 1, -1, 0, 1))
  ## Default weight should be 0.2

  expect_equal(covs[[1]]$parameter, 0.2)
})

test_that("saomnk_model() with strategy weight attribute overrides default", {
  strat_vec <- c(1, 0, 1, 0)
  attr(strat_vec, "weight") <- 0.75
  mod <- saomnk_model(density = -1, strategies = list(egoX = strat_vec))

  expect_equal(mod$dv_bipartite$coCovars[[1]]$parameter, 0.75)
})

test_that("saomnk_model() with influence_matrix creates coDyadCovars", {
  epi_mat <- saomnk_block_diagonal(6, 2)
  mod <- saomnk_model(density = -1, influence_matrix = epi_mat,
                       influence_weight = 0.15)

  dcovs <- mod$dv_bipartite$coDyadCovars
  expect_length(dcovs, 1)
  expect_equal(dcovs[[1]]$effect, "XWX")
  expect_equal(dcovs[[1]]$parameter, 0.15)
  expect_true(is.matrix(dcovs[[1]]$x))
  expect_equal(dim(dcovs[[1]]$x), c(6, 6))
})

test_that("saomnk_model() without epistasis has empty coDyadCovars", {
  mod <- saomnk_model(density = -1)
  expect_length(mod$dv_bipartite$coDyadCovars, 0)
})

test_that("saomnk_model() empty sections are lists", {
  mod <- saomnk_model(density = -1)
  dv <- mod$dv_bipartite
  expect_true(is.list(dv$coCovars))
  expect_true(is.list(dv$coDyadCovars))
  expect_true(is.list(dv$varCovars))
  expect_true(is.list(dv$varDyadCovars))
  expect_true(is.list(dv$interactions))
})


# ===========================================================================
# 3. saomnk_block_diagonal() -- influence matrix construction
# ===========================================================================
test_that("saomnk_block_diagonal() produces correct dimensions", {
  mat <- saomnk_block_diagonal(12, 4)
  expect_equal(dim(mat), c(12, 12))
})

test_that("saomnk_block_diagonal() is symmetric", {
  mat <- saomnk_block_diagonal(8, 2)
  expect_equal(mat, t(mat))
})

test_that("saomnk_block_diagonal() contains only 0s and 1s", {
  mat <- saomnk_block_diagonal(10, 3)
  expect_true(all(mat %in% c(0, 1)))
})

test_that("saomnk_block_diagonal() with 1 block is all ones", {
  mat <- saomnk_block_diagonal(5, 1)
  expect_true(all(mat == 1))
})

test_that("saomnk_block_diagonal() with N blocks is identity", {
  mat <- saomnk_block_diagonal(4, 4)
  expect_equal(mat, diag(4))
})

test_that("saomnk_block_diagonal() with uneven blocks handles remainder", {
  ## 7 components in 3 blocks: sizes 3, 2, 2 (or similar distribution)
  mat <- saomnk_block_diagonal(7, 3)
  expect_equal(dim(mat), c(7, 7))
  expect_true(all(mat %in% c(0, 1)))
  ## All diagonal entries must be 1 (every component interacts with itself)
  expect_true(all(diag(mat) == 1))
})

test_that("saomnk_block_diagonal() rejects invalid inputs", {
  expect_error(saomnk_block_diagonal(0, 1))
  expect_error(saomnk_block_diagonal(5, 0))
  expect_error(saomnk_block_diagonal(3, 5))  # blocks > N
})


# ===========================================================================
# 4. print.saomnk_model() -- model printing
# ===========================================================================
test_that("print.saomnk_model() runs without error", {
  mod <- saomnk_model(density = -0.5, popularity = 0.2)
  expect_output(print(mod), "SaoMNK Structure Model")
})

test_that("print.saomnk_model() shows effects", {
  mod <- saomnk_model(density = -1, popularity = 0.3)
  output <- capture.output(print(mod))
  ## Should mention density
  expect_true(any(grepl("density", output)))
})

test_that("print.saomnk_model() shows epistasis when present", {
  epi <- saomnk_block_diagonal(6, 2)
  mod <- saomnk_model(density = -1, influence_matrix = epi)
  output <- capture.output(print(mod))
  expect_true(any(grepl("epistasis|XWX|Dyad", output, ignore.case = TRUE)))
})


# ===========================================================================
# 5. saomnk_run() -- simulation execution
# ===========================================================================
test_that("saomnk_run() executes without error on small env", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    saomnk_env(M = 4, N = 6, density = 0.3, seed = 42),
    error = function(e) stop(paste("saomnk_env failed:", e$message))
  )
  mod <- saomnk_model(density = -1)

  result <- tryCatch(
    saomnk_run(env, mod, steps_per_actor = 5, seed = 42),
    error = function(e) stop(paste("saomnk_run failed:", e$message))
  )

  ## Should return the env invisibly
  expect_s3_class(result, "SaomNkRSienaBiEnv")
  ## RSiena model should be populated
  expect_false(is.null(env$rsiena_model))
})

test_that("saomnk_run() rejects non-SaomNkRSienaBiEnv input", {
  mod <- saomnk_model(density = -1)
  expect_error(saomnk_run("not_an_env", mod))
})


# ===========================================================================
# 6. saomnk_summary() -- post-simulation summary
# ===========================================================================
test_that("saomnk_summary() runs after simulation", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 6, iterations_per_actor = 5, rand_seed = 55),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  result <- tryCatch(
    saomnk_summary(env),
    error = function(e) stop(paste("saomnk_summary failed:", e$message))
  )

  ## No crash is the key assertion
  expect_true(TRUE)
})


# ===========================================================================
# 7. saomnk_plot_k4() -- K-4 panel plot
# ===========================================================================
test_that("saomnk_plot_k4() returns without error after simulation", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 88),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  ## Should not error (actual plot output depends on graphics device)
  result <- tryCatch(
    saomnk_plot_k4(env, smooth = 0.5),
    error = function(e) stop(paste("saomnk_plot_k4 failed:", e$message))
  )

  expect_true(TRUE)
})


# ===========================================================================
# 8. saomnk_get_bipartite() -- matrix extraction
# ===========================================================================
test_that("saomnk_get_bipartite() returns correct dimensions", {
  env <- saomnk_env(M = 5, N = 7, density = 0.4, seed = 10)
  mat <- saomnk_get_bipartite(env)
  expect_equal(dim(mat), c(5, 7))
})

test_that("saomnk_get_bipartite() contains only 0s and 1s", {
  env <- saomnk_env(M = 4, N = 6, density = 0.5, seed = 20)
  mat <- saomnk_get_bipartite(env)
  expect_true(all(mat %in% c(0, 1)))
})

test_that("saomnk_get_bipartite() rejects non-SaomNkRSienaBiEnv", {
  expect_error(saomnk_get_bipartite("not_an_env"))
})


# ===========================================================================
# 9. saomnk_get_degrees() -- degree data frame extraction
# ===========================================================================
test_that("saomnk_get_degrees() returns named list with K_AC, K_CA, K_AA, K_CC", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 77),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  degrees <- saomnk_get_degrees(env)
  expect_true(is.list(degrees))
  expect_true(all(c("K_AC", "K_CA", "K_AA", "K_CC") %in% names(degrees)))
})

test_that("saomnk_get_degrees() data frames have expected columns", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 78),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  degrees <- saomnk_get_degrees(env)

  ## At minimum, each df should have value and chain_step_id columns
  for (nm in c("K_AC", "K_CA", "K_AA", "K_CC")) {
    if (!is.null(degrees[[nm]])) {
      expect_true(is.data.frame(degrees[[nm]]),
                  info = paste(nm, "should be a data.frame"))
      expect_true("value" %in% names(degrees[[nm]]),
                  info = paste(nm, "should have 'value' column"))
    }
  }
})


# ===========================================================================
# 10. saomnk_shock() -- shock specification
# ===========================================================================
test_that("saomnk_shock() creates correct structure", {
  s <- saomnk_shock("density", parameter = -2.0, portion = 1)
  expect_s3_class(s, "saomnk_shock")
  expect_equal(s$effect, "density")
  expect_equal(s$parameter, -2.0)
  expect_equal(s$portion, 1L)
})

test_that("saomnk_shock() maps friendly names to RSiena codes", {
  s <- saomnk_shock("popularity", parameter = 0.5)
  expect_equal(s$effect, "inPop")

  s2 <- saomnk_shock("scope", parameter = 0.3)
  expect_equal(s2$effect, "outAct")

  s3 <- saomnk_shock("epistasis", parameter = 0.1)
  expect_equal(s3$effect, "XWX")
})

test_that("saomnk_shock() passes through unknown effect names unchanged", {
  s <- saomnk_shock("myCustomEffect", parameter = 1.0)
  expect_equal(s$effect, "myCustomEffect")
})

test_that("saomnk_shock() rejects invalid inputs", {
  expect_error(saomnk_shock(123, parameter = 1))  # non-character
  expect_error(saomnk_shock("density", parameter = "abc"))  # non-numeric
})

# --- 0.4.0 rename: epistasis_* -> influence_* --------------------------------

test_that("influence_* arguments work without warning", {
  W <- saomnk_block_diagonal(8, 2)
  expect_silent(saomnk_model(density = -0.5, influence_matrix = W,
                             influence_weight = 0.3))
})

test_that("deprecated epistasis_* arguments warn but still work", {
  W <- saomnk_block_diagonal(8, 2)
  expect_warning(saomnk_model(density = -0.5, epistasis_matrix = W),
                 "deprecated")
  expect_warning(saomnk_model(density = -0.5, influence_matrix = W,
                              epistasis_weight = 0.3),
                 "influence_weight")
  expect_warning(saomnk_model(density = -0.5, epistasis_matrices = list(A = W)),
                 "influence_matrices")
  expect_warning(saomnk_model(density = -0.5, influence_matrices = list(A = W),
                              epistasis_weights = c(A = 0.2)),
                 "influence_weights")
})

test_that("old and new argument names produce identical models", {
  W <- saomnk_block_diagonal(8, 2)
  new <- saomnk_model(density = -0.5, influence_matrix = W,
                      influence_weight = 0.3)
  old <- suppressWarnings(
    saomnk_model(density = -0.5, epistasis_matrix = W,
                 epistasis_weight = 0.3))
  expect_identical(new, old)
})

test_that("new argument takes precedence when both are supplied", {
  W1 <- saomnk_block_diagonal(8, 2)
  W2 <- saomnk_block_diagonal(8, 4)
  both <- suppressWarnings(
    saomnk_model(density = -0.5, influence_matrix = W2, epistasis_matrix = W1))
  only_new <- saomnk_model(density = -0.5, influence_matrix = W2)
  expect_identical(both, only_new)
})
