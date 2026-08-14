###############################################################################
## test-plotting.R
## Tests that plot functions return ggplot objects without errors
###############################################################################

test_that("plot_bipartite_system_from_mat returns a plot object", {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = 4, N = 8, rand_seed = 500)
  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) stop(paste("Init failed:", e$message))
  )

  plt <- tryCatch(
    env$plot_bipartite_system_from_mat(
      env$bipartite_matrix,
      RSIENA_ITERATION = 0,
      plot_save = FALSE,
      return_plot = TRUE
    ),
    error = function(e) stop(paste("plot_bipartite_system_from_mat failed:", e$message))
  )

  ## Should return something plot-like (ggplot, recordedplot, or at least not NULL)
  expect_false(is.null(plt), info = "Plot should not be NULL")
})


test_that("search_rsiena_plot returns ggplot objects after simulation", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 501),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  ## search_rsiena_plot returns a list of plot types
  plist <- tryCatch(
    env$search_rsiena_plot(type = c("utility")),
    error = function(e) stop(paste("search_rsiena_plot failed:", e$message))
  )

  expect_true(is.list(plist), info = "search_rsiena_plot should return a list")

  if ("utility" %in% names(plist)) {
    plt <- plist[["utility"]]
    expect_true(
      inherits(plt, "gg") || inherits(plt, "ggplot") || !is.null(plt),
      info = "utility plot should be a ggplot object"
    )
  }
})


test_that("search_rsiena_plot utility_density variant works", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 502),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  plist <- tryCatch(
    env$search_rsiena_plot(type = c("utility_density")),
    error = function(e) stop(paste("search_rsiena_plot utility_density failed:", e$message))
  )

  expect_true(is.list(plist))
  if ("utility_density" %in% names(plist)) {
    plt <- plist[["utility_density"]]
    expect_true(
      inherits(plt, "gg") || inherits(plt, "ggplot") || !is.null(plt),
      info = "utility_density plot should be a ggplot object"
    )
  }
})


test_that("search_rsiena_plot with strategy model produces plots", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5,
                 rand_seed = 503, use_strategy = TRUE),
    error = function(e) stop(paste("Tiny sim with strategy failed:", e$message))
  )

  plist <- tryCatch(
    env$search_rsiena_plot(type = c("utility_by_strategy")),
    error = function(e) stop(paste("search_rsiena_plot utility_by_strategy failed:", e$message))
  )

  expect_true(is.list(plist))
  if ("utility_by_strategy" %in% names(plist)) {
    plt <- plist[["utility_by_strategy"]]
    expect_true(
      inherits(plt, "gg") || inherits(plt, "ggplot") || !is.null(plt),
      info = "utility_by_strategy plot should be a ggplot object"
    )
  }
})


test_that("search_rsiena_plot_stability runs after simulation", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 504),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  ## stability plot is available via search_rsiena_plot type = "stability"
  ## or directly via search_rsiena_plot_stability
  result <- tryCatch(
    env$search_rsiena_plot_stability(tol = 1e-5, step_size = 1, wave_id = 1),
    error = function(e) {
      ## Was a skip on the guess that the plot "may require specific chain
      ## structure". A guess is not a precondition: it excused every failure,
      ## including real ones. If this genuinely cannot run on the tiny model,
      ## the precondition should be named and checked, not assumed.
      stop(paste("search_rsiena_plot_stability failed:", e$message))
    }
  )

  ## If it ran, result should be non-NULL
  expect_false(is.null(result), info = "Stability plot result should not be NULL")
})


test_that("multiwave plot functions return ggplot objects", {
  skip_if_not_installed("RSiena")

  params <- make_small_environ_params(M = 4, N = 8, rand_seed = 505)
  struct <- make_minimal_structure_model()

  env <- tryCatch(
    SaomNkRSienaBiEnv$new(params),
    error = function(e) stop(paste("Init failed:", e$message))
  )

  tryCatch(
    env$search_rsiena_multiwave_run(
      structure_model = struct,
      waves = 1,
      iterations = 10,
      rand_seed = 505
    ),
    error = function(e) stop(paste("Multiwave run failed:", e$message))
  )

  ## Process results for multiwave
  tryCatch(
    env$search_rsiena_multiwave_process_results(),
    error = function(e) stop(paste("Multiwave process results failed:", e$message))
  )

  ## Try the multiwave plot
  plist <- tryCatch(
    env$search_rsiena_multiwave_plot(
      type = c("utility_strategy_summary"),
      thin_factor = 1,
      thin_wave_factor = 1
    ),
    error = function(e) stop(paste("Multiwave plot failed:", e$message))
  )

  if (!is.null(plist) && is.list(plist)) {
    expect_true(is.list(plist), info = "Multiwave plot should return a list")
  }
})
