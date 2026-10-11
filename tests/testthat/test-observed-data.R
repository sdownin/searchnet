# Observed-data layer: searchnet_bipartite_from_long(), searchnet_k_readings(),
# searchnet_k_covariate(). Synthetic generic data only.

.obs_long <- function(seed = 1, M = 7, N = 6, W = 3, p = 0.35) {
  set.seed(seed)
  g <- expand.grid(actor = sprintf("a%02d", seq_len(M)),
                   component = sprintf("c%02d", seq_len(N)),
                   period = seq_len(W), stringsAsFactors = FALSE)
  g$weight <- round(stats::runif(nrow(g)), 2)
  g[stats::runif(nrow(g)) < p, , drop = FALSE]
}

## Long rows back out of a B array: one row per tie.
.obs_to_long <- function(B) {
  idx <- which(B == 1L, arr.ind = TRUE)
  dn <- dimnames(B)
  data.frame(actor = dn[[1]][idx[, 1]], component = dn[[2]][idx[, 2]],
             period = dn[[3]][idx[, 3]], stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# searchnet_bipartite_from_long
# ---------------------------------------------------------------------------
test_that("long -> B -> long round trip preserves every tie and its count", {
  d <- .obs_long()
  obs <- searchnet_bipartite_from_long(d, "actor", "component", "period")
  expect_s3_class(obs, "searchnet_bipartite")
  expect_true(is.integer(obs$B))
  expect_equal(dim(obs$B), c(length(unique(d$actor)), length(unique(d$component)), 3L))
  back <- .obs_to_long(obs$B)
  key_in  <- sort(paste(d$actor, d$component, d$period))
  key_out <- sort(paste(back$actor, back$component, back$period))
  expect_identical(key_out, key_in)
  expect_equal(as.vector(apply(obs$B, 3, sum)), as.vector(table(d$period)))
  expect_identical(obs$periods, 1:3)
  expect_output(print(obs), "searchnet_bipartite")
})

test_that("no period column gives a single M x N matrix", {
  d <- data.frame(actor = c("x", "y", "y"), component = c("p", "p", "q"))
  obs <- searchnet_bipartite_from_long(d, "actor", "component")
  expect_true(is.matrix(obs$B))
  expect_null(obs$periods)
  expect_equal(unname(obs$B), matrix(c(1L, 1L, 0L, 1L), 2, 2))
})

test_that("explicit universes fix order and give absent actors zero rows", {
  d <- data.frame(actor = c("a2", "a1", "a2"), component = c("c1", "c2", "c2"),
                  period = c(2, 1, 1))
  obs <- searchnet_bipartite_from_long(
    d, "actor", "component", "period",
    actors = c("a3", "a2", "a1"), components = c("c2", "c1", "c0"),
    periods = c(1, 2, 3))
  expect_equal(dimnames(obs$B)[[1]], c("a3", "a2", "a1"))
  expect_equal(dimnames(obs$B)[[2]], c("c2", "c1", "c0"))
  expect_equal(dim(obs$B), c(3L, 3L, 3L))
  expect_equal(sum(obs$B["a3", , ]), 0)        # absent actor: zero row
  expect_equal(sum(obs$B[, "c0", ]), 0)        # absent component: zero column
  expect_equal(sum(obs$B[, , "3"]), 0)         # empty period: zero slice
  expect_equal(obs$B["a2", "c1", "2"], 1L)
  expect_equal(obs$B["a1", "c2", "1"], 1L)
  ## composition is stable: every slice has the same rows
  expect_true(all(dim(obs$B)[1:2] == c(3L, 3L)))
})

test_that("factor ids default to their levels, numeric periods sort numerically", {
  d <- data.frame(actor = factor(c("b", "a"), levels = c("b", "a")),
                  component = c("k", "k"), period = c(10, 2))
  obs <- searchnet_bipartite_from_long(d, "actor", "component", "period")
  expect_equal(obs$actors, c("b", "a"))
  expect_equal(obs$periods, c(2, 10))
})

test_that("weights and threshold define ties; duplicates are summed with a message", {
  d <- data.frame(actor = c("a", "a", "a", "b"), component = c("x", "x", "y", "y"),
                  w = c(0.3, 0.4, 0.5, 0))
  expect_message(
    obs <- searchnet_bipartite_from_long(d, "actor", "component", weight = "w",
                                         threshold = 0.6),
    "1 duplicated actor-component row")
  expect_equal(obs$B["a", "x"], 1L)   # 0.3 + 0.4 = 0.7 > 0.6
  expect_equal(obs$B["a", "y"], 0L)   # 0.5 <= 0.6
  expect_equal(obs$B["b", "y"], 0L)   # 0 is not > 0.6
  obs0 <- suppressMessages(
    searchnet_bipartite_from_long(d, "actor", "component", weight = "w"))
  expect_equal(obs0$B["b", "y"], 0L)  # weight 0 is not > threshold 0
  expect_equal(obs0$B["a", "y"], 1L)
  ## presence: duplicates collapse to one tie
  expect_message(pres <- searchnet_bipartite_from_long(d, "actor", "component"),
                 "presence is a tie")
  expect_equal(sum(pres$B), 3L)
})

test_that("input errors are clear", {
  d <- .obs_long()
  expect_error(searchnet_bipartite_from_long(as.matrix(d), "actor", "component"),
               "must be a data frame")
  expect_error(searchnet_bipartite_from_long(d, "firm", "component"),
               "Column\\(s\\) not found in `data`: 'firm'")
  expect_error(searchnet_bipartite_from_long(d, "actor", "actor"),
               "must name different columns")
  d2 <- d; d2$actor[2] <- NA
  expect_error(searchnet_bipartite_from_long(d2, "actor", "component"),
               "NA in the actor column")
  d3 <- d; d3$weight[1] <- NA
  expect_error(searchnet_bipartite_from_long(d3, "actor", "component", "period",
                                             weight = "weight"),
               "NA in the weight column")
  d4 <- d; d4$weight <- as.character(d4$weight)
  expect_error(searchnet_bipartite_from_long(d4, "actor", "component", "period",
                                             weight = "weight"),
               "must be numeric")
  expect_error(searchnet_bipartite_from_long(d, "actor", "component", "period",
                                             actors = c("a01", "a02")),
               "are not in `actors`")
  expect_error(searchnet_bipartite_from_long(d, "actor", "component",
                                             actors = c("a01", "a01")),
               "duplicated")
  expect_error(searchnet_bipartite_from_long(d, "actor", "component", periods = 1:2),
               "`period` \\(the column\\) was not")
  expect_error(searchnet_bipartite_from_long(d, "actor", "component", threshold = "x"),
               "single number")
  expect_error(searchnet_bipartite_from_long(d[0, ], "actor", "component"),
               "no rows")
  ## empty data with explicit universes is an all-zero B
  z <- searchnet_bipartite_from_long(d[0, ], "actor", "component",
                                     actors = "u", components = c("v", "w"))
  expect_equal(sum(z$B), 0)
  expect_equal(dim(z$B), c(1L, 2L))
})

# ---------------------------------------------------------------------------
# searchnet_k_readings
# ---------------------------------------------------------------------------
test_that("K readings follow K_DIMENSIONS.md on a known matrix", {
  ## B = [1 0 1 0; 0 1 1 0; 1 1 0 1; 0 0 0 0]  (actor 4 is an isolate)
  B <- rbind(c(1, 0, 1, 0), c(0, 1, 1, 0), c(1, 1, 0, 1), c(0, 0, 0, 0))
  k <- searchnet_k_readings(B)
  a <- k[k$level == "actor", ]; cc <- k[k$level == "component", ]
  expect_equal(a$K_AC, c(2L, 2L, 3L, 0L))
  expect_equal(cc$K_CA, c(2L, 2L, 2L, 1L))
  ## K_AA excludes the actor itself; the isolate has 0, not -1 or 1
  expect_equal(a$K_AA, c(2L, 2L, 2L, 0L))
  ## K_CC excludes the component itself
  expect_equal(cc$K_CC, c(3L, 3L, 2L, 2L))
  expect_equal(a$sociality_strength, c(2L, 2L, 2L, 0L))
  expect_equal(cc$epistasis_strength, c(3L, 3L, 2L, 2L))
  expect_true(all(is.na(a$K_CA)) && all(is.na(cc$K_AC)))
  expect_true(all(is.na(k$period)))
  s <- attr(k, "summary")
  expect_equal(s$ties, 7L)
  expect_equal(s$density, 7 / 16)
  expect_equal(s$mean_K_AA, mean(a$K_AA))
  expect_true(is.na(s$tie_jaccard))
  ## mean pairwise Jaccard over actor pairs with a non-empty union
  J <- c(1/3, 1/4, 1/4, 0 / 2, 0 / 2, 0 / 3)  # (1,2) (1,3) (2,3) (1,4) (2,4) (3,4)
  expect_equal(s$actor_jaccard, mean(J))
})

test_that("accounting identities I1-I3 hold on random panels", {
  set.seed(7)
  B <- array(rbinom(9 * 8 * 4, 1, 0.3), dim = c(9, 8, 4))
  k <- searchnet_k_readings(B)
  for (w in 1:4) {
    a <- k[k$period == w & k$level == "actor", ]
    cc <- k[k$period == w & k$level == "component", ]
    expect_equal(sum(a$K_AC), sum(B[, , w]))
    expect_equal(sum(cc$K_CA), sum(B[, , w]))
    expect_equal(sum(cc$K_CA * (cc$K_CA - 1)), sum(a$sociality_strength))
    expect_equal(sum(a$K_AC * (a$K_AC - 1)), sum(cc$epistasis_strength))
  }
  s <- attr(k, "summary")
  expect_equal(nrow(s), 4L)
  tj <- sum(B[, , 1] & B[, , 2]) / sum(B[, , 1] | B[, , 2])
  expect_equal(s$tie_jaccard[2], tj)
})

test_that("k_readings accepts arrays, lists and from_long output alike", {
  obs <- searchnet_bipartite_from_long(.obs_long(), "actor", "component", "period")
  k1 <- searchnet_k_readings(obs)
  k2 <- searchnet_k_readings(obs$B)
  k3 <- searchnet_k_readings(lapply(1:3, function(w) obs$B[, , w]))
  expect_equal(k1$K_AA, k2$K_AA)
  expect_equal(k1$K_CC, k3$K_CC)
  expect_equal(k1$id[k1$level == "actor" & k1$period == 1], obs$actors)
  expect_equal(unique(k1$period), obs$periods)
  expect_error(searchnet_k_readings(.obs_long()), "searchnet_bipartite_from_long")
  expect_error(searchnet_k_readings(matrix(c(0, 2, 1, 0), 2)), "must be 0/1")
  expect_error(searchnet_k_readings(matrix(c(0, NA, 1, 0), 2)), "contains NA")
  expect_error(searchnet_k_readings(1:5), "must be an M x N matrix")
})

test_that("k_readings equals the simulation engine's degrees, step by step", {
  skip_if_not_installed("RSiena")
  env <- saomnk_env(M = 6, N = 9, density = 0.25, seed = 23)
  invisible(utils::capture.output(
    saomnk_run(env, saomnk_model(density = -0.3, popularity = -0.2),
               steps_per_actor = 4, seed = 23)))
  deg <- saomnk_get_degrees(env)
  arr <- env$bi_env_arr
  W <- dim(arr)[3]
  expect_gt(W, 5L)
  k <- searchnet_k_readings(arr)
  sim_vals <- function(df, s, id) {
    d <- df[df$chain_step_id == s, ]
    as.integer(d$value[order(as.numeric(as.character(d[[id]])))])
  }
  for (s in seq_len(W)) {
    a <- k[k$period == s & k$level == "actor", ]
    cc <- k[k$period == s & k$level == "component", ]
    info <- sprintf("step %d", s)
    expect_identical(a$K_AC, sim_vals(deg$K_AC, s, "actor_id"), info = info)
    expect_identical(a$K_AA, sim_vals(deg$K_AA, s, "actor_id"), info = info)
    expect_identical(cc$K_CA, sim_vals(deg$K_CA, s, "component_id"), info = info)
    expect_identical(cc$K_CC, sim_vals(deg$K_CC, s, "component_id"), info = info)
  }
  ## and the Rosetta summary's mean degrees on the final state
  rk <- .rosetta_k_summary(arr[, , W])
  s <- attr(k, "summary")[W, ]
  expect_equal(c(s$mean_K_AC, s$mean_K_CA, s$mean_K_AA, s$mean_K_CC),
               c(rk$K_AC, rk$K_CA, rk$K_AA, rk$K_CC))
})

# ---------------------------------------------------------------------------
# searchnet_k_covariate and searchnet_coevolve_data compatibility
# ---------------------------------------------------------------------------
test_that("k_covariate reshapes a reading to nodes x periods", {
  obs <- searchnet_bipartite_from_long(.obs_long(), "actor", "component", "period")
  k <- searchnet_k_readings(obs)
  m <- searchnet_k_covariate(k, "K_AA")
  expect_equal(dim(m), c(length(obs$actors), 3L))
  expect_equal(rownames(m), obs$actors)
  expect_equal(m[, 2], k$K_AA[k$level == "actor" & k$period == 2],
               ignore_attr = TRUE)
  mc <- searchnet_k_covariate(k, "K_CC")
  expect_equal(dim(mc), c(length(obs$components), 3L))
  expect_error(searchnet_k_covariate(data.frame(x = 1), "K_AA"), "searchnet_k_readings")
  expect_error(searchnet_k_covariate(k, "K_XX"))
})

test_that("from_long output and k_covariate feed searchnet_coevolve_data", {
  skip_if_not_installed("RSiena")
  obs <- searchnet_bipartite_from_long(.obs_long(seed = 3, M = 8, N = 6, W = 3),
                                       "actor", "component", "period")
  N <- length(obs$components)
  set.seed(3)
  arc <- array(rbinom(N * N * 3, 1, 0.3), dim = c(N, N, 3))
  k <- searchnet_k_readings(obs)
  dat <- searchnet_coevolve_data(
    bipartite = obs, component = arc,
    actor_covars = list(scope = searchnet_k_covariate(k, "K_AC")),
    component_covars = list(holders = searchnet_k_covariate(k, "K_CA")),
    verbose = FALSE)
  expect_true(.searchnet_is_siena_data(dat))
  dat2 <- searchnet_coevolve_data(bipartite = obs$B, component = arc,
                                  verbose = FALSE)
  expect_true(.searchnet_is_siena_data(dat2))
  expect_equal(dat$observations, 3L)
})
