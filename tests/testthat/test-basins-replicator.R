# Basin geometry, replicator dynamics and Elo tournaments: internal helpers.
# These are not exported; the tests guard their behavior and that they stay
# internal.

test_that("basin and replicator helpers are internal", {
  exports <- getNamespaceExports("searchnet")
  internal <- c("basin_geometry", "basin_geometry_table", "basin_width_cross_partial",
                "basin_imitation_penalty", "plot_basin_depth_width",
                "plot_basin_profiles", "plot_basin_shock_recovery",
                "replicator_dynamics", "replicator_ess", "elo_tournament",
                "plot_replicator_shares", "plot_replicator_simplex",
                "plot_elo_ratings", "counterfactual_scenarios")
  for (f in internal) {
    expect_true(exists(f, envir = asNamespace("searchnet"), inherits = FALSE), info = f)
    expect_false(f %in% exports, info = f)
  }
})

test_that("basin_geometry follows its analytical mapping", {
  g <- basin_geometry(scope_cost = 0.6, synergy = -0.3, herding = 0.2)
  expect_equal(g$width, 0.4)
  expect_equal(g$depth, 0.3 + 0.7 * 0.25)
  expect_equal(g$steepness, 1 + 5 * (0.8 / 1.2))
  expect_equal(g$escape_difficulty, g$depth * g$steepness / g$width)
})

test_that("basin_geometry_table returns one row per parameter set", {
  td <- basin_geometry_table()
  expect_s3_class(td, "data.frame")
  expect_identical(td$param_set, c("A", "B", "C", "D"))
  expect_named(td, c("param_set", "label", "width", "depth", "steepness",
                     "escape_difficulty", "scope_cost", "synergy", "herding"))
  custom <- basin_geometry_table(list(x = list(scope_cost = 0, synergy = 0,
                                               herding = 0, label = "x")))
  expect_equal(nrow(custom), 1L)
  pen <- basin_imitation_penalty(td, imitation_distance = 0.5)
  expect_identical(pen$param_set, td$param_set)
  expect_true(all(pen$penalty >= 0))
})

test_that("basin width cross-partial is zero for the additive mapping", {
  cp <- basin_width_cross_partial(n_grid = 11)
  expect_equal(dim(cp$cross_partial), c(9L, 9L))
  expect_equal(cp$mean_cross_partial, 0, tolerance = 1e-8)
})

test_that("replicator dynamics conserve shares and select the fittest profile", {
  traj <- replicator_dynamics(c(A = 0.80, B = 0.81, C = 0.79, D = 0.82),
                              generations = 100)
  expect_named(traj, c("generation", "profile", "share", "fitness"))
  sums <- tapply(traj$share, traj$generation, sum)
  expect_true(all(abs(sums - 1) < 1e-9))
  ess <- replicator_ess(traj)
  expect_identical(names(ess), c("A", "B", "C", "D"))
  expect_identical(attr(ess, "surviving"), "D")
  ## a label vector of the wrong length falls back to generic names
  traj3 <- replicator_dynamics(c(1, 2, 3), generations = 2)
  expect_identical(unique(traj3$profile), paste0("Type_", 1:3))
})

test_that("elo_tournament is reproducible under a seed and reports the range", {
  set.seed(42)
  fb <- list(A = rnorm(30, 0.80, 0.05), B = rnorm(30, 0.80, 0.05),
             C = rnorm(30, 0.80, 0.05), D = rnorm(30, 0.80, 0.05))
  set.seed(7); t1 <- elo_tournament(fb, matches_per_pair = 10)
  set.seed(7); t2 <- elo_tournament(fb, matches_per_pair = 10)
  expect_identical(t1, t2)
  expect_named(t1, c("ratings", "history", "ratings_within_20", "rating_range"))
  expect_equal(nrow(t1$history), 6L * 10L)
  expect_equal(sum(t1$ratings), 4 * 1500)
  expect_identical(t1$ratings_within_20, t1$rating_range < 20)
})

test_that("the basin and replicator plots build", {
  skip_if_not_installed("ggplot2")
  td <- basin_geometry_table()
  traj <- replicator_dynamics(c(A = 0.80, B = 0.81, C = 0.79, D = 0.82),
                              generations = 20)
  set.seed(1)
  fb <- list(A = rnorm(10), B = rnorm(10), C = rnorm(10), D = rnorm(10))
  tour <- elo_tournament(fb, matches_per_pair = 3)
  plots <- list(plot_basin_depth_width(td, show_frontier = FALSE),
                plot_basin_profiles(td),
                plot_basin_shock_recovery(td),
                plot_replicator_shares(traj),
                plot_replicator_simplex(traj, "B", "D"),
                plot_elo_ratings(tour))
  for (p in plots) {
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }
})
