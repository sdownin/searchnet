## searchnet_move_compass() and searchnet_actor_landscape(): the data behind
## the compass and the W-fitted grid (vignette "searchnet-landscape-views").

.lv_env <- function() {
  N <- 6L
  W <- saomnk_block_diagonal(N, 2)
  env <- saomnk_env(M = 4, N = N, density = 0.2, seed = 7)
  mod <- saomnk_model(density = -2, popularity = 0.2,
                      influence_matrix = W, influence_weight = 0.8)
  invisible(capture.output(suppressMessages(
    saomnk_run(env, mod, steps_per_actor = 3, seed = 11))))
  env
}

test_that("compass f_now equals the recorded utility at each step", {
  env <- .lv_env()
  ud <- as.data.frame(env$actor_util_df)
  n_t <- dim(env$bi_env_arr)[3]
  for (t in unique(c(1L, n_t %/% 2L, n_t))) {
    for (i in seq_len(env$M)) {
      cp <- searchnet_move_compass(env, actor = i, step = t)
      rec <- ud$utility[ud$chain_step_id == t &
                          as.integer(as.character(ud$actor_id)) == i]
      expect_equal(unique(cp$f_now), rec, tolerance = 1e-8)
    }
  }
})

test_that("compass rows are the N single toggles of the held portfolio", {
  env <- .lv_env()
  cp <- searchnet_move_compass(env, actor = 2, step = 0)
  B0 <- if (!is.null(env$path_start_matrix)) env$path_start_matrix else env$bipartite_matrix_init
  expect_equal(nrow(cp), env$N)
  expect_equal(cp$held, as.integer(B0[2, ]))
  expect_equal(cp$move, ifelse(B0[2, ] > 0, "drop", "add"))
  expect_equal(cp$delta, cp$f_after - cp$f_now)
  expect_identical(attr(cp, "step"), 0L)
  expect_identical(attr(cp, "local_peak"), all(cp$delta <= 1e-9))
})

test_that("compass deltas agree with the full landscape, and peaks are consistent", {
  env <- .lv_env()
  N <- env$N
  for (i in c(1L, 3L)) {
    land <- searchnet_actor_landscape(env, actor = i)
    cp <- searchnet_move_compass(env, actor = i)
    expect_equal(nrow(land), 2^N)
    cur <- which(land$current)
    expect_length(cur, 1L)
    expect_equal(land$f[cur], cp$f_now[1], tolerance = 1e-10)
    nbr <- bitwXor(cur - 1L, 2L^(seq_len(N) - 1L)) + 1L
    expect_equal(land$f[nbr] - land$f[cur], cp$delta, tolerance = 1e-10)
    expect_identical(land$local_peak[cur], attr(cp, "local_peak"))
    ## every peak is weakly better than all of its N neighbors; the global
    ## maximum is always a peak
    pk <- which(land$local_peak)
    expect_true(which.max(land$f) %in% pk)
    for (r in pk) {
      nb <- bitwXor(r - 1L, 2L^(seq_len(N) - 1L)) + 1L
      expect_true(all(land$f[r] >= land$f[nb] - 1e-9))
    }
  }
})

test_that("bad arguments are refused", {
  env <- .lv_env()
  n_t <- dim(env$bi_env_arr)[3]
  expect_error(searchnet_move_compass(env, actor = 0), "actor")
  expect_error(searchnet_move_compass(env, actor = 1, step = n_t + 1L), "step")
  expect_error(searchnet_actor_landscape(env, actor = 1, max_n = 4), "max_n")
})
