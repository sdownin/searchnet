###############################################################################
## test-path-gates.R
##
## Gates G1 (extended), G5, G6 and G7 of the 2026-10-06 pre-registration for
## the state-carrying search_rsiena(). G1-G4 live in test-path-not-replay.R,
## committed before the engine change. These were written in the fix commit,
## before the fixed engine was run against them.
##
##   G1b  every logged choice probability, for density + inPop + XWX + egoX,
##        across two segments, with the covariate uncentered (the 0.11.0
##        default) and centered (declared), is reproduced from the path's
##        preceding state.
##   G5   independent re-derivation: an oracle simulator (helper-saom-oracle.R)
##        against search_rsiena(), R = 200 replicates each, for one segment
##        and for two segments with an XWX shock between them. Pass: max |z| < 3.
##   G6   the same run_seed gives a bit-identical path; seeds for run_seed and
##        run_seed + 1 share no segment seed.
##   G7   path = "legacy_replay" warns with class `searchnet_not_a_path`, and
##        every path consumer refuses its output.
###############################################################################

.pg_W <- function(N) {
  W <- kronecker(diag(2), matrix(1, N / 2, N / 2))
  W[upper.tri(W)] <- W[upper.tri(W)] * 0.5   # asymmetric on purpose
  diag(W) <- 0
  W
}

.pg_model <- function(theta, W = NULL, v = NULL, centered = NULL) {
  effects <- list(
    list(effect = "density", parameter = theta[["density"]], dv_name = DV_NAME, fix = TRUE))
  if ("inPop" %in% names(theta))
    effects[[length(effects) + 1L]] <-
      list(effect = "inPop", parameter = theta[["inPop"]], dv_name = DV_NAME, fix = TRUE)
  coCovars <- list()
  if (!is.null(v)) {
    cc <- list(effect = "egoX", parameter = theta[["egoX"]], dv_name = DV_NAME, fix = TRUE,
               interaction1 = "self$strat_1_coCovar", x = v)
    if (!is.null(centered)) cc$centered <- centered
    coCovars[[1L]] <- cc
  }
  coDyadCovars <- list()
  if (!is.null(W))
    coDyadCovars[[1L]] <- list(effect = "XWX", parameter = theta[["XWX"]], dv_name = DV_NAME,
                               fix = TRUE, nodeSet = c("COMPONENTS", "COMPONENTS"),
                               interaction1 = "self$component_1_coDyadCovar", x = W)
  list(dv_bipartite = list(
    name = DV_NAME, rates = list(), effects = effects, coCovars = coCovars,
    varCovars = list(), coDyadCovars = coDyadCovars, varDyadCovars = list(),
    interactions = list()))
}

.pg_env <- function(M, N, B0, seed = 1) {
  env <- SaomNkRSienaBiEnv$new(make_small_environ_params(
    M = M, N = N, BI_PROB = 0, rand_seed = seed, name = "_pg_"))
  env$bipartite_matrix_init <- B0
  env$set_system_from_bipartite_matrix(B0)
  env
}

.pg_xwx_shocks <- function(a, b) list(
  list(effect = "XWX", parameter = a, portion = 1),
  list(effect = "XWX", parameter = b, portion = 1))


for (.centered in list(NULL, TRUE)) local({
  centered <- .centered
  test_that(sprintf("G1b: choice probabilities for density+inPop+XWX+egoX over two segments (covariate %s)",
                    if (is.null(centered)) "uncentered by default" else "declared centered"), {
    skip_if_not_installed("RSiena")
    M <- 6; N <- 8
    set.seed(31)
    B0 <- matrix(rbinom(M * N, 1, 0.4), M, N)
    W  <- .pg_W(N)
    v  <- rep(c(0, 1), length.out = M)
    theta <- c(density = -0.6, inPop = -0.3, XWX = 0.5, egoX = 0.4)
    env <- .pg_env(M, N, B0)
    suppressMessages(env$search_rsiena(
      .pg_model(theta, W, v, centered), iterations_per_actor = 12, run_seed = 4401,
      theta_shocks = .pg_xwx_shocks(0.5, -0.8)))
    expect_equal(nrow(env$path_segments), 2L)

    cs <- env$chain_stats
    v_applied <- if (isTRUE(centered)) v - mean(v) else v
    tm <- env$rsiena_model$thetaUsed
    colnames(tm) <- colnames(env$theta_matrix)
    ks <- which(cs$dv_varname == DV_NAME)
    expect_gt(length(ks), 30L)
    expect_true(all(c(1L, 2L) %in% cs$segment_id[ks]))
    err <- vapply(ks, function(k) {
      B <- if (k == 1L) env$bi_env_arr_initial else env$bi_env_arr[, , k - 1L]
      th <- tm[cs$segment_id[k], c("density", "inPop", "XWX", "egoX")]
      .oracle_log_probs(B, as.integer(cs$id_from[k]), th, W = W, v = v_applied)[
        as.integer(cs$id_to[k])] - as.numeric(cs$LogChoiceProb[k])
    }, numeric(1))
    expect_lt(max(abs(err)), 1e-8,
              label = sprintf("max |log choice prob error| (%.3g)", max(abs(err))))
    ## The centering recorded on the environment is the one RSiena applied.
    cc <- env$covariate_centering
    expect_equal(cc$centered[cc$kind == "cCovars"], isTRUE(centered))
  })
})


test_that("G5: search_rsiena() agrees with an independent oracle simulator (one and two segments)", {
  skip_if_not_installed("RSiena")
  skip_on_cran()
  M <- 6; N <- 8; R <- 200; rho <- 10
  set.seed(52)
  B0 <- matrix(rbinom(M * N, 1, 0.35), M, N)
  W  <- .pg_W(N)
  theta <- c(density = -0.6, inPop = -0.2, XWX = 0.4)
  theta2 <- theta; theta2[["XWX"]] <- -0.4

  run_engine <- function(two) {
    t(vapply(seq_len(R), function(r) {
      env <- .pg_env(M, N, B0, seed = r)
      suppressMessages(env$search_rsiena(
        .pg_model(theta, W), iterations_per_actor = rho, run_seed = 90000 + r,
        theta_shocks = if (two) .pg_xwx_shocks(0.4, -0.4) else NULL,
        process_chain = FALSE))
      .oracle_end_stats(env$bipartite_matrix, B0, W)
    }, numeric(4)))
  }
  run_oracle <- function(two) {
    set.seed(777 + two)
    t(vapply(seq_len(R), function(r) {
      lam <- rep(rho, M)
      if (two) {
        h <- .oracle_simulate(B0, lam, theta, W, t_end = 0.5)$B
        B <- .oracle_simulate(h, lam, theta2, W, t_end = 0.5)$B
      } else {
        B <- .oracle_simulate(B0, lam, theta, W, t_end = 1)$B
      }
      .oracle_end_stats(B, B0, W)
    }, numeric(4)))
  }
  zs <- function(a, b) (colMeans(a) - colMeans(b)) /
    sqrt(apply(a, 2, var) / nrow(a) + apply(b, 2, var) / nrow(b))

  for (two in c(FALSE, TRUE)) {
    e <- run_engine(two); o <- run_oracle(two)
    z <- zs(e, o)
    expect_lt(max(abs(z)), 3,
              label = sprintf("%s: z = %s (engine means %s; oracle means %s)",
                              if (two) "two segments" else "one segment",
                              paste(sprintf("%s %.2f", names(z), z), collapse = ", "),
                              paste(sprintf("%.2f", colMeans(e)), collapse = "/"),
                              paste(sprintf("%.2f", colMeans(o)), collapse = "/")))
  }
})


test_that("G6: the same run_seed gives a bit-identical path, and adjacent seeds share no segment seed", {
  skip_if_not_installed("RSiena")
  M <- 5; N <- 6
  set.seed(61)
  B0 <- matrix(rbinom(M * N, 1, 0.4), M, N)
  W <- .pg_W(N)
  theta <- c(density = -0.5, inPop = -0.2, XWX = 0.3)
  run <- function(seed) {
    env <- .pg_env(M, N, B0)
    suppressMessages(env$search_rsiena(.pg_model(theta, W), iterations_per_actor = 8,
                                       run_seed = seed,
                                       theta_shocks = .pg_xwx_shocks(0.3, -0.3)))
    env
  }
  a <- run(1201); b <- run(1201); c1 <- run(1202)
  expect_identical(a$bi_env_arr, b$bi_env_arr)
  expect_identical(a$chain_stats, b$chain_stats)
  expect_identical(a$bipartite_matrix, b$bipartite_matrix)
  expect_length(intersect(a$path_segments$seed, c1$path_segments$seed), 0L)

  ## Drawing segment seeds leaves the global RNG where it was.
  set.seed(5); s0 <- .Random.seed
  invisible(.searchnet_segment_seeds(99, 10))
  expect_identical(.Random.seed, s0)
})


test_that("G7: the legacy replay warns, and every path consumer refuses it", {
  skip_if_not_installed("RSiena")
  M <- 5; N <- 6
  W <- .pg_W(N)
  mod <- .pg_model(c(density = -0.5, inPop = -0.2, XWX = 0.3), W)
  env <- .pg_env(M, N, matrix(rbinom(M * N, 1, 0.4), M, N))
  expect_warning(suppressMessages(env$search_rsiena(mod, iterations_per_actor = 4,
                                                    path = "legacy_replay")),
                 class = "searchnet_not_a_path")
  expect_identical(attr(env$bi_env_arr, "searchnet_path"), "independent_draws")

  refuses <- function(expr)
    expect_error(expr, class = "searchnet_not_a_path_error")
  refuses(env$search_rsiena_process_stats())
  refuses(env$get_K4_df())
  refuses(searchnet_chain_stats(env))
  refuses(searchnet_export_k4(env, file = tempfile(fileext = ".csv")))
  refuses(searchnet_export_snapshots(env, file = tempfile(fileext = ".csv")))
  refuses(searchnet_export_utility(env, file = tempfile(fileext = ".csv")))
  refuses(saomnk_plot_k4(env))
  refuses(saomnk_plot_snapshots(env))
  refuses(saomnk_plot_utility(env))
  refuses(saomnk_get_degrees(env))
  refuses(saomnk_get_bipartite(env, step = 1))
  refuses(saomnk_plot_actor_degrees(env))
  refuses(searchnet_plot_exploration(env))
  refuses(suppressWarnings(saomnk_plot_exploration_exploitation(env)))

  ## The three consumers that run their own simulation. Force the legacy
  ## route through the class, so the guard each one carries is exercised.
  gen <- SaomNkRSienaBiEnv
  orig <- gen$public_methods$search_rsiena
  gen$set("public", "search_rsiena", function(...) {
    args <- list(...)
    args$path <- NULL; args$max_segments <- NULL
    suppressWarnings(do.call(self$search_rsiena_legacy_replay, args))
  }, overwrite = TRUE)
  on.exit(gen$set("public", "search_rsiena", orig, overwrite = TRUE), add = TRUE)

  refuses(suppressMessages(searchnet_ergodicity_sweep(
    M = 4, N = 5, run_lengths = 2, replicates = 2)))
  env2 <- .pg_env(M, N, matrix(rbinom(M * N, 1, 0.4), M, N))
  refuses(.bridge_run_arm(env2, mod, iterations = 10, seed_r = 3))
  cls <- suppressMessages(searchnet_classroom_init(n_students = 2, n_rounds = 2, N = 6, seed = 3))
  refuses(suppressMessages(searchnet_classroom_advance(cls, force = TRUE)))
})
