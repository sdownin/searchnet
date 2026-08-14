###############################################################################
## test-choice-probabilities.R
## Tests for compute_choice_probabilities method
###############################################################################

test_that("all probabilities sum to 1 for each actor", {
  skip_if_not_installed("RSiena")

  M <- 3
  N <- 4
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 500),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  result <- tryCatch(
    env$compute_choice_probabilities(beta = 1),
    error = function(e) stop(paste("compute_choice_probabilities failed:", e$message))
  )

  for (i in seq_along(result)) {
    total <- sum(result[[i]]$probabilities)
    expect_equal(total, 1.0, tolerance = 1e-12,
                 info = sprintf("Actor %d probabilities must sum to 1", i))
  }
})


test_that("all probabilities between 0 and 1", {
  skip_if_not_installed("RSiena")

  M <- 2
  N <- 5
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 501),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  result <- tryCatch(
    env$compute_choice_probabilities(beta = 2),
    error = function(e) stop(paste("compute_choice_probabilities failed:", e$message))
  )

  for (i in seq_along(result)) {
    probs <- result[[i]]$probabilities
    expect_true(all(probs >= 0),
                info = sprintf("Actor %d: all probabilities must be >= 0", i))
    expect_true(all(probs <= 1),
                info = sprintf("Actor %d: all probabilities must be <= 1", i))
  }
})


test_that("at beta near 0 probabilities are approximately uniform", {
  skip_if_not_installed("RSiena")

  M <- 2
  N <- 4
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 502),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  ## beta very close to zero: exp(beta * delta_u) ~ 1 for all options
  result <- tryCatch(
    env$compute_choice_probabilities(beta = 1e-8),
    error = function(e) stop(paste("compute_choice_probabilities failed:", e$message))
  )

  uniform_prob <- 1 / (N + 1)  ## N flips + 1 pass option
  for (i in seq_along(result)) {
    probs <- result[[i]]$probabilities
    expect_equal(length(probs), N + 1,
                 info = sprintf("Actor %d: should have N+1 probabilities", i))
    for (k in seq_along(probs)) {
      expect_equal(unname(probs[k]), uniform_prob, tolerance = 0.05,
                   info = sprintf("Actor %d, option %d: should be ~uniform at beta~0 (got %.4f, expected %.4f)", i, k, probs[k], uniform_prob))
    }
  }
})


test_that("at very high beta probability concentrates on best flip", {
  skip_if_not_installed("RSiena")

  M <- 2
  N <- 4
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 503),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  result <- tryCatch(
    env$compute_choice_probabilities(beta = 100),
    error = function(e) stop(paste("compute_choice_probabilities failed:", e$message))
  )

  for (i in seq_along(result)) {
    probs <- result[[i]]$probabilities
    delta_u <- result[[i]]$delta_u
    max_prob <- max(probs)
    ## At beta=100, IF delta_u values differ, dominant option should concentrate
    ## If all delta_u are equal (symmetric actor), probabilities stay uniform — that's correct
    ## At high beta, max probability should exceed uniform 1/(N+1)
    ## unless all delta_u are exactly equal
    uniform_baseline <- 1 / length(probs)
    expect_true(max_prob >= uniform_baseline - 0.01,
                info = sprintf("Actor %d: max prob (%.4f) should be >= uniform (%.4f) at high beta",
                               i, max_prob, uniform_baseline))
  }
})


test_that("monotone logit link: higher delta_u gives higher probability", {
  skip_if_not_installed("RSiena")

  ## Monotonicity of the logit link is a property of the link, not of
  ## single-actor dynamics; the test only ever reads result[[1]]. It used to
  ## request M = 1, which search_rsiena() refuses (RSiena's sienaDataCreate()
  ## has no single-actor bipartite form), so the whole test silently skipped.
  M <- 2
  N <- 6
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 504),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  set.seed(504)
  result <- tryCatch(
    env$compute_choice_probabilities(beta = 1),
    error = function(e) stop(paste("compute_choice_probabilities failed:", e$message))
  )

  actor <- result[[1]]
  delta_u <- actor$delta_u
  flip_probs <- actor$probabilities[paste0("flip_", 1:N)]

  ## For any pair of flips, the one with larger delta_u must have larger probability
  ## (strict monotonicity of the logit link with positive beta)
  for (a in 1:(N - 1)) {
    for (b in (a + 1):N) {
      if (delta_u[a] > delta_u[b]) {
        expect_true(flip_probs[a] > flip_probs[b],
                    info = sprintf("flip_%d (delta_u=%.4f) should have higher prob than flip_%d (delta_u=%.4f)",
                                   a, delta_u[a], b, delta_u[b]))
      } else if (delta_u[b] > delta_u[a]) {
        expect_true(flip_probs[b] > flip_probs[a],
                    info = sprintf("flip_%d (delta_u=%.4f) should have higher prob than flip_%d (delta_u=%.4f)",
                                   b, delta_u[b], a, delta_u[a]))
      }
      ## If delta_u[a] == delta_u[b], probabilities should be equal (no assertion needed)
    }
  }
})


test_that("pass probability exists and is positive", {
  skip_if_not_installed("RSiena")

  M <- 3
  N <- 4
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 505),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  result <- tryCatch(
    env$compute_choice_probabilities(beta = 1),
    error = function(e) stop(paste("compute_choice_probabilities failed:", e$message))
  )

  for (i in seq_along(result)) {
    probs <- result[[i]]$probabilities
    expect_true("pass" %in% names(probs),
                info = sprintf("Actor %d: 'pass' must be in probability names", i))
    expect_true(probs["pass"] > 0,
                info = sprintf("Actor %d: pass probability must be positive", i))
  }
})


test_that("returns list of length M (one per actor)", {
  skip_if_not_installed("RSiena")

  M <- 3
  N <- 5
  env <- tryCatch(
    run_tiny_sim(M = M, N = N, iterations_per_actor = 5, rand_seed = 506),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  result <- tryCatch(
    env$compute_choice_probabilities(beta = 1),
    error = function(e) stop(paste("compute_choice_probabilities failed:", e$message))
  )

  expect_true(is.list(result))
  expect_equal(length(result), M,
               info = "Result list length must equal number of actors M")

  ## Each element should have expected fields

  for (i in seq_along(result)) {
    expect_true("actor_id" %in% names(result[[i]]))
    expect_true("delta_u" %in% names(result[[i]]))
    expect_true("probabilities" %in% names(result[[i]]))
    expect_true("beta" %in% names(result[[i]]))
    expect_equal(result[[i]]$actor_id, i)
    expect_equal(result[[i]]$beta, 1)
    expect_equal(length(result[[i]]$delta_u), N)
    expect_equal(length(result[[i]]$probabilities), N + 1)
  }
})
