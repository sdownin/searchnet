test_that("saomnk_sim_ego_indist2 matches the CD4 canonical definition", {
  # Reference transcribed from the CD2026 procedural engine's inline
  # simEgoInDist2 block. Kept as a literal transcription rather than a call so
  # the test does not depend on that project being present.
  cd4_ref <- function(B, perf, i) {
    M <- nrow(B); N <- ncol(B)
    rivals <- setdiff(seq_len(M), i)
    rh <- B[rivals, , drop = FALSE]
    n_co <- colSums(rh)
    co_sum <- as.numeric(perf[rivals] %*% rh)
    has_co <- n_co > 0
    avg <- rep(NA_real_, N); avg[has_co] <- co_sum[has_co] / n_co[has_co]
    rng <- max(perf) - min(perf)
    s <- rep(NA_real_, N)
    if (rng > 0) s[has_co] <- (rng - abs(perf[i] - avg[has_co])) / rng else s[has_co] <- 1
    m <- mean(s[has_co]); if (is.na(m)) m <- 0
    s[has_co] <- s[has_co] - m
    s
  }
  set.seed(7)
  for (trial in 1:50) {
    M <- sample(3:8, 1); N <- sample(4:12, 1)
    B <- matrix(rbinom(M * N, 1, runif(1, .2, .7)), nrow = M)
    perf <- runif(M, 0, 10)
    mine <- saomnk_sim_ego_indist2(B, perf, per_component = TRUE)
    for (i in seq_len(M)) {
      ref <- cd4_ref(B, perf, i)
      expect_equal(is.na(mine[i, ]), is.na(ref))
      ok <- !is.na(ref)
      if (any(ok)) expect_equal(mine[i, ok], ref[ok], tolerance = 1e-12)
    }
  }
})

test_that("components with no co-holders are invisible, not unattractive", {
  B <- matrix(0, 3, 4); B[1, 1] <- 1; B[2, 2] <- 1; B[3, 2] <- 1
  sm <- saomnk_sim_ego_indist2(B, c(1, 2, 3), per_component = TRUE)
  expect_true(is.na(sm[1, 1]))   # nobody else holds component 1
  expect_false(is.na(sm[2, 2]))  # actor 3 also holds component 2
})

test_that("identical performance yields no pull on any component", {
  set.seed(1)
  B <- matrix(rbinom(40, 1, 0.4), nrow = 5)
  expect_true(all(abs(saomnk_sim_ego_indist2(B, rep(3, 5))) < 1e-12))
})

test_that("a single actor feels no imitation pull", {
  expect_equal(saomnk_sim_ego_indist2(matrix(c(1, 0, 1), nrow = 1), 1), 0)
})

test_that("malformed input errors rather than recycling", {
  B <- matrix(rbinom(20, 1, .5), nrow = 4)
  expect_error(saomnk_sim_ego_indist2(B, runif(3)), "length")
  expect_error(saomnk_sim_ego_indist2(B, c(NA, runif(3))), "NA")
})
