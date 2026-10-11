###############################################################################
## test-deprecations.R
## Renamed and consolidated functions keep every old name working, with a
## warning: the 0.8.2 terminology renames, and the plot consolidation below.
## Pattern follows test-api.R ("deprecated argument names still work").
###############################################################################

test_that("saomnk_empirical_epistasis() is a warning alias of saomnk_empirical_influence()", {
  set.seed(11)
  B <- matrix(rbinom(60, 1, 0.4), nrow = 10, ncol = 6)

  ## The alias warns once per session; reset so this test is order-independent.
  .searchnet_reset_deprecations()
  expect_warning(old <- saomnk_empirical_epistasis(B), "saomnk_empirical_influence")
  new <- saomnk_empirical_influence(B)
  expect_identical(old, new)

  ## Second call in the same session is silent (one-time warning, by design).
  expect_silent(old2 <- saomnk_empirical_epistasis(B))
  expect_identical(old2, new)

  ## Same for every method and the threshold/diagonal arguments.
  .searchnet_reset_deprecations()
  for (m in c("jaccard", "cosine", "cooccurrence")) {
    a <- suppressWarnings(saomnk_empirical_epistasis(B, method = m, threshold = 0.1, diagonal = 1))
    b <- saomnk_empirical_influence(B, method = m, threshold = 0.1, diagonal = 1)
    expect_identical(a, b, info = m)
  }
})

test_that("the returned object is an influence-matrix estimate: N x N, symmetric, diagonal as set", {
  set.seed(12)
  B <- matrix(rbinom(80, 1, 0.5), nrow = 10, ncol = 8)
  W <- saomnk_empirical_influence(B, diagonal = 0)
  expect_equal(dim(W), c(8, 8))
  expect_true(isSymmetric(unname(W)))
  expect_true(all(diag(W) == 0))
})

test_that("epistatic_int_mat still works as a deprecated alias of influence_matrix", {
  skip_if_not_installed("igraph")
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(M = 4, N = 6, rand_seed = 13))
  W <- saomnk_block_diagonal(6, 2)

  expect_warning(g_old <- env$get_component_groups_list(epistatic_int_mat = W),
                 "influence_matrix")
  g_new <- env$get_component_groups_list(influence_matrix = W)
  expect_identical(g_old, g_new)

  ## new name takes precedence when both are supplied
  W2 <- saomnk_block_diagonal(6, 3)
  both <- suppressWarnings(env$get_component_groups_list(influence_matrix = W2,
                                                         epistatic_int_mat = W))
  expect_identical(both, env$get_component_groups_list(influence_matrix = W2))

  ## and the exported plot functions expose the new name in their formals
  expect_true("influence_matrix" %in% names(formals(saomnk_plot_bipartite_ring_markets)))
  expect_true("epistatic_int_mat" %in% names(formals(saomnk_plot_bipartite_ring_markets)))
  expect_true("influence_matrix" %in% names(formals(saomnk_plot_bipartite_ring_markets_animation)))
})


###############################################################################
## Plot consolidation: the exploration, multi-wave, _v0 and K_AC_NEW plot
## names forward to searchnet_plot_exploration(), searchnet_plot_multiwave(),
## searchnet_plot_cumulative_entry() and searchnet_plot_new_component_shocks().
## Each legacy name must (a) warn once per session naming its replacement and
## (b) return what the replacement returns for the same arguments, or fail
## with the same message where the tiny fixture cannot draw that view.
###############################################################################

## Comparable fingerprint of a plot object. ggplot objects carry environments
## and grobs carry per-call ids, so compare what is drawn: the built layer data
## and labels for a ggplot, layout and grob classes for a gtable.
.plot_fp <- function(p) {
  if (is.null(p)) return(NULL)
  if (inherits(p, "ggplot")) {
    b <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))
    flat <- function(d) {
      d <- as.data.frame(d)
      d[] <- lapply(d, function(col)
        if (is.list(col)) vapply(col, function(z) paste(class(z), collapse = "/"), "")
        else col)
      d
    }
    return(list(class = class(p), labels = ggplot2::get_labs(p),
                data = lapply(b$data, flat)))
  }
  if (inherits(p, "gtable"))
    return(list(class = class(p), layout = p$layout[, c("t", "l", "b", "r")],
                grobs = vapply(p$grobs, function(g) class(g)[1], "")))
  if (is.list(p) && !is.data.frame(p)) return(lapply(p, .plot_fp))
  p
}

## Run one call; return its fingerprint (or error message) and the
## deprecation messages it raised. Other warnings are muffled.
.plot_capture <- function(expr) {
  dep <- character(0)
  out <- withCallingHandlers(
    tryCatch(list(value = .plot_fp(suppressMessages(expr))),
             error = function(e) list(error = conditionMessage(e))),
    warning = function(w) {
      if (inherits(w, "deprecatedWarning")) dep <<- c(dep, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  list(out = out, dep = dep)
}

## old-vs-new check for one legacy name.
.expect_forwards <- function(old, new_fun, selector, args, label = old) {
  new_txt <- if (length(selector))
    sprintf("%s(%s)", new_fun, paste(sprintf('%s = "%s"', names(selector), unlist(selector)),
                                     collapse = ", "))
  else sprintf("%s()", new_fun)
  .searchnet_reset_deprecations()
  set.seed(2024); a <- .plot_capture(do.call(old, args))
  new_args <- args
  if (length(selector)) names(new_args)[names(new_args) == "env"] <- "x"
  set.seed(2024); b <- .plot_capture(do.call(new_fun, c(new_args[1], selector, new_args[-1])))
  expect_length(a$dep, 1)
  expect_true(grepl(new_txt, a$dep[1], fixed = TRUE), info = label)
  expect_length(b$dep, 0)
  expect_identical(a$out, b$out, info = label)
  ## once per session: the second call is silent and still forwards
  set.seed(2024); a2 <- .plot_capture(do.call(old, args))
  expect_length(a2$dep, 0)
  expect_identical(a2$out, b$out, info = label)
  invisible(a$out)
}

test_that("legacy wrappers keep their exact signatures", {
  ns <- asNamespace("searchnet")
  pairs <- c(
    saomnk_plot_exploration_exploitation = ".searchnet_ee_trajectory",
    saomnk_plot_exploration_exploitation_consistent = ".searchnet_ee_trajectory_density",
    saomnk_plot_exploration_exploitation_phase = ".searchnet_ee_phase_space",
    saomnk_plot_exploration_exploitation_improved = ".searchnet_ee_portfolio",
    saomnk_plot_exploration_exploitation_by_strategy = ".searchnet_ee_strategy_groups",
    saomnk_plot_exploration_exploitation_faceted = ".searchnet_ee_strategy_facets",
    saomnk_plot_strategy_exploration_exploitation = ".searchnet_ee_strategy_result",
    saomnk_plot_exploration_exploitation_subsidies = ".searchnet_ee_risk_subsidies",
    saomnk_search_rsiena_multiwave_plot = ".searchnet_mw_list",
    saomnk_search_rsiena_multiwave_plot_K_4panel = ".searchnet_mw_K_4panel",
    saomnk_search_rsiena_multiwave_plot_K_AA_strategy_summary = ".searchnet_mw_K_AA",
    saomnk_search_rsiena_multiwave_plot_K_AC_strategy_summary = ".searchnet_mw_K_AC",
    saomnk_search_rsiena_multiwave_plot_K_CA_strategy_summary = ".searchnet_mw_K_CA",
    saomnk_search_rsiena_multiwave_plot_K_CC_strategy_summary = ".searchnet_mw_K_CC",
    saomnk_search_rsiena_multiwave_plot_actor_utility_strategy_summary = ".searchnet_mw_utility_strategy",
    saomnk_search_rsiena_multiwave_plot_actor_utility_by_strategy = ".searchnet_mw_utility_actor",
    saomnk_search_rsiena_multiwave_plot_actor_utility_density_by_strategy = ".searchnet_mw_utility_density",
    saomnk_search_rsiena_multiwave_plot_utility_ridge_density_by_strategy = ".searchnet_mw_utility_ridge",
    saomnk_plot_market_entry_survival_v0 = "searchnet_plot_cumulative_entry",
    saomnk_plot_K_AC_NEW_shocks = "searchnet_plot_new_component_shocks")
  exported <- getNamespaceExports("searchnet")
  for (old in names(pairs)) {
    expect_true(old %in% exported, info = old)
    expect_identical(formals(get(old, ns)), formals(get(pairs[[old]], ns)), info = old)
  }
  for (new in c("searchnet_plot_exploration", "searchnet_plot_multiwave",
                "searchnet_plot_cumulative_entry", "searchnet_plot_new_component_shocks"))
    expect_true(new %in% exported, info = new)
  ## every selectable view resolves to an implementation
  expect_setequal(eval(formals(searchnet_plot_exploration)$type),
                  names(ns$.searchnet_exploration_types))
  expect_setequal(eval(formals(searchnet_plot_multiwave)$what),
                  names(ns$.searchnet_multiwave_table))
  for (f in c(ns$.searchnet_exploration_types, unlist(ns$.searchnet_multiwave_table)))
    expect_true(exists(f, envir = ns, inherits = FALSE), info = f)
})

test_that("searchnet_plot_multiwave() validates `by` against `what`", {
  expect_error(searchnet_plot_multiwave(NULL, what = "K_4panel", by = "strategy"),
               "does not apply")
  expect_error(searchnet_plot_multiwave(NULL, what = "K_AA", by = "actor"),
               "must be one of")
  expect_error(searchnet_plot_exploration(NULL, type = "nonesuch"), "should be one of")
})

test_that("exploration plot names forward to searchnet_plot_exploration()", {
  skip_if_not_installed("RSiena")
  local_close_new_devices()
  env <- run_tiny_sim(M = 4, N = 16, iterations_per_actor = 5, rand_seed = 42,
                      use_strategy = TRUE)
  set.seed(7)
  steps <- rep(1:6, each = 4)
  metrics_df <- data.frame(chain_step_id = steps, actor_id = rep(1:4, 6),
                           strategy = rep(c("0", "100"), 12),
                           prop_exploitation = runif(24), prop_exploration = runif(24))
  result <- list(metrics = data.frame(chain_step_id = steps, actor_id = rep(1:4, 6),
                                      strategy = rep(c("0", "100"), 12),
                                      exploration = runif(24), exploitation = runif(24)))
  cases <- list(
    list("saomnk_plot_exploration_exploitation", "trajectory", list(env = env)),
    list("saomnk_plot_exploration_exploitation_consistent", "trajectory_density", list(env = env)),
    list("saomnk_plot_exploration_exploitation_phase", "phase_space", list(env = env)),
    list("saomnk_plot_exploration_exploitation_improved", "portfolio", list(env = env)),
    list("saomnk_plot_exploration_exploitation_by_strategy", "strategy_groups", list(env = env)),
    list("saomnk_plot_exploration_exploitation_faceted", "strategy_facets",
         list(env = env, metrics_df = metrics_df)),
    list("saomnk_plot_strategy_exploration_exploitation", "strategy_result",
         list(env = env, result = result)),
    list("saomnk_plot_exploration_exploitation_subsidies", "risk_subsidies", list(env = env)))
  drawn <- 0L
  for (cs in cases) {
    out <- .expect_forwards(cs[[1]], "searchnet_plot_exploration", list(type = cs[[2]]), cs[[3]])
    drawn <- drawn + !is.null(out$value)
  }
  ## most views draw on this fixture, so equality is tested on real output
  expect_gte(drawn, 6L)

  ## positional arguments still bind to the old formals
  .searchnet_reset_deprecations()
  set.seed(1); a <- .plot_capture(saomnk_plot_exploration_exploitation(env, c(1, 2), 1, 1, "lm"))
  set.seed(1); b <- .plot_capture(searchnet_plot_exploration(env, "trajectory", actor_ids = c(1, 2),
                                                             smooth_method = "lm"))
  expect_identical(a$out, b$out)
  ## the default view is the one the unsuffixed old name drew
  set.seed(1); d <- .plot_capture(searchnet_plot_exploration(env))
  set.seed(1); e <- .plot_capture(searchnet_plot_exploration(env, type = "trajectory"))
  expect_identical(d$out, e$out)
})

test_that("multi-wave plot names forward to searchnet_plot_multiwave()", {
  skip_if_not_installed("RSiena")
  local_close_new_devices()
  params <- make_small_environ_params(M = 4, N = 8, rand_seed = 505)
  env <- SaomNkRSienaBiEnv$new(params)
  invisible(capture.output(suppressMessages(suppressWarnings({
    env$search_rsiena_multiwave_run(structure_model = make_strategy_structure_model(M = 4),
                                    waves = 1, iterations = 10, rand_seed = 505)
    env$search_rsiena_multiwave_process_results()
  }))))
  mw <- "saomnk_search_rsiena_multiwave_plot"
  cases <- list(
    list(mw, list(what = "list"), list(env = env, type = "K_AA_strategy_summary")),
    list(paste0(mw, "_K_4panel"), list(what = "K_4panel"), list(env = env)),
    list(paste0(mw, "_K_AA_strategy_summary"), list(what = "K_AA", by = "strategy"), list(env = env)),
    list(paste0(mw, "_K_AC_strategy_summary"), list(what = "K_AC", by = "strategy"), list(env = env)),
    list(paste0(mw, "_K_CA_strategy_summary"), list(what = "K_CA", by = "strategy"), list(env = env)),
    list(paste0(mw, "_K_CC_strategy_summary"), list(what = "K_CC", by = "strategy"), list(env = env)),
    list(paste0(mw, "_actor_utility_strategy_summary"), list(what = "utility", by = "strategy"),
         list(env = env)),
    list(paste0(mw, "_actor_utility_by_strategy"), list(what = "utility", by = "actor"),
         list(env = env)),
    list(paste0(mw, "_actor_utility_density_by_strategy"), list(what = "utility_density", by = "strategy"),
         list(env = env)),
    list(paste0(mw, "_utility_ridge_density_by_strategy"), list(what = "utility_ridge", by = "strategy"),
         list(env = env)))
  drawn <- 0L
  for (cs in cases) {
    out <- .expect_forwards(cs[[1]], "searchnet_plot_multiwave", cs[[2]], cs[[3]])
    drawn <- drawn + !is.null(out$value)
  }
  expect_gte(drawn, 5L)

  ## `by = NULL` takes the first grouping listed for that quantity
  set.seed(1); a <- .plot_capture(searchnet_plot_multiwave(env, what = "utility"))
  set.seed(1); b <- .plot_capture(searchnet_plot_multiwave(env, what = "utility", by = "strategy"))
  expect_identical(a$out, b$out)
})

test_that("version-tagged plot names forward to their descriptive replacements", {
  skip_if_not_installed("RSiena")
  local_close_new_devices()
  env <- run_tiny_sim(M = 4, N = 16, iterations_per_actor = 5, rand_seed = 43,
                      use_strategy = TRUE)
  .expect_forwards("saomnk_plot_K_AC_NEW_shocks", "searchnet_plot_new_component_shocks",
                   list(), list(env = env))
  ## the Monte Carlo entry curves: n = 2 short runs keep this fast
  invisible(capture.output(
    .expect_forwards("saomnk_plot_market_entry_survival_v0", "searchnet_plot_cumulative_entry",
                     list(), list(env = env, n = 2, steps_per_actor = 3))))
})
