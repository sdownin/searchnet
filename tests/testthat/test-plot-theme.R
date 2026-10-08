###############################################################################
## test-plot-theme.R
## The shared plot style (theme_searchnet(), searchnet_palette(), the
## scale_*_searchnet() scales) and the plots converted from base graphics to
## ggplot2: plot.searchnet_ergodicity(), search_rsiena_plot_stability(), the
## sienaGOF helper behind add_gof_to_rsiena_shocks(), and the restyled
## phase-space and bipartite-system plots. Each converted plot is built with
## ggplot_build() and drawn on a null device.
###############################################################################

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00",
               "#0072B2", "#CC79A7", "#F0E442", "#000000")

draws <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  invisible(TRUE)
}

is_gg <- function(p) inherits(p, "ggplot")

tiny_theme_env <- function() {
  env <- saomnk_env(M = 4, N = 6, density = 0.2, seed = 42)
  mod <- saomnk_model(density = -0.5, popularity = 0.2,
                      strategies = list(egoX = c(-1, 1, -1, 1)))
  invisible(capture.output(suppressMessages(
    saomnk_run(env, mod, steps_per_actor = 4, seed = 12345))))
  env
}

## ---------------------------------------------------------------------------
## Theme, palette, scales
## ---------------------------------------------------------------------------

test_that("theme_searchnet() is a complete theme with the package defaults", {
  th <- theme_searchnet()
  expect_s3_class(th, "theme")
  expect_identical(th$legend.position, "bottom")
  expect_s3_class(th$panel.grid.minor, "element_blank")
  expect_identical(th$plot.title$face, "bold")
  expect_identical(theme_searchnet(legend_position = "right")$legend.position, "right")
  expect_s3_class(theme_searchnet(grid = FALSE)$panel.grid, "element_blank")
  bare <- theme_searchnet(axes = FALSE)
  expect_s3_class(bare$axis.text, "element_blank")
  expect_s3_class(bare$axis.title, "element_blank")
})

test_that("searchnet_palette() returns Okabe-Ito, sequential and diverging colors", {
  cat_all <- searchnet_palette()
  expect_identical(unname(cat_all), okabe_ito)
  expect_identical(names(cat_all)[1:2], c("orange", "sky"))
  expect_identical(searchnet_palette("categorical", 3), okabe_ito[1:3])
  expect_identical(searchnet_palette("categorical", 2, reverse = TRUE), okabe_ito[2:1])
  expect_warning(p10 <- searchnet_palette("categorical", 10), "exceed the 8")
  expect_length(p10, 10)
  seq5 <- searchnet_palette("sequential", 5)
  expect_length(seq5, 5)
  expect_identical(toupper(seq5[1]), "#FFFFFF")
  div <- searchnet_palette("diverging", 3)
  expect_identical(toupper(div), c("#0072B2", "#FFFFFF", "#D55E00"))
  expect_error(searchnet_palette("rainbow"))
})

test_that("scale_*_searchnet() map categories to Okabe-Ito and zero to white", {
  df <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"), w = c(-1, 0, 1))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, color = g)) + ggplot2::geom_point() +
    scale_color_searchnet() + theme_searchnet()
  expect_identical(toupper(ggplot2::layer_data(p)$colour), okabe_ito[1:3])
  expect_true(draws(p))

  pf <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = w)) + ggplot2::geom_tile() +
    scale_fill_searchnet("diverging")
  fills <- toupper(ggplot2::layer_data(pf)$fill)
  expect_identical(fills[2], "#FFFFFF")
  expect_identical(fills[c(1, 3)], c("#0072B2", "#D55E00"))

  ps <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = w)) + ggplot2::geom_tile() +
    scale_fill_searchnet("sequential")
  expect_identical(toupper(ggplot2::layer_data(ps)$fill[1]), "#FFFFFF")
  expect_true(is_gg(ps + scale_colour_searchnet("sequential")))
})

## ---------------------------------------------------------------------------
## Converted from base graphics
## ---------------------------------------------------------------------------

test_that("plot.searchnet_ergodicity() returns x with a ggplot attached and draws", {
  skip_if_not_installed("RSiena")
  erg <- searchnet_ergodicity_sweep(M = 6, N = 6, run_lengths = c(5L, 10L, 20L),
                                    replicates = 3L)
  out <- plot(erg, draw = FALSE)
  expect_s3_class(out, "searchnet_ergodicity")
  expect_identical(out$summary, erg$summary)
  fig <- attr(out, "plot")
  expect_true(is_gg(fig))
  expect_silent(ggplot2::ggplot_build(fig))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_s3_class(plot(erg), "searchnet_ergodicity")
})

test_that("search_rsiena_plot_stability() returns the stability ggplot", {
  env <- run_tiny_sim(M = 4, N = 8, iterations_per_actor = 5, rand_seed = 504)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  res <- env$search_rsiena_plot_stability(tol = 1e-5, step_size = 1, wave_id = 1)
  expect_true(is_gg(res$degree_plot))
  expect_true(is_gg(res$stability_plot))
  expect_silent(ggplot2::ggplot_build(res$stability_plot))
  expect_identical(length(res$jaccard), length(res$sim_ids))
})

test_that("the sienaGOF helper draws a sienaGOF-shaped object with ggplot2", {
  set.seed(1)
  sims <- matrix(stats::rpois(200, 3), nrow = 50, ncol = 4)
  sims[, 4] <- 2                        # zero variance: dropped, as in RSiena
  one <- list(Simulations = sims, Observations = matrix(c(2, 3, 4, 2), nrow = 1),
              p = 0.42)
  attr(one, "key") <- c("0", "1", "2", "3")
  gof <- list(Joint = one)
  attr(gof, "joined") <- TRUE
  attr(gof, "auxiliaryStatisticName") <- "OutdegreeDistribution"
  p <- .searchnet_gof_ggplot(gof)
  expect_true(is_gg(p))
  expect_identical(p$labels$title, "Goodness of Fit of OutdegreeDistribution")
  expect_identical(levels(p$data$stat), c("0", "1", "2"))
  expect_true(draws(p))
})

## ---------------------------------------------------------------------------
## Restyled ggplot functions
## ---------------------------------------------------------------------------

test_that("phase-space heatmap uses the package style", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("hexbin")
  env <- tiny_theme_env()
  env$actor_util_df <- as.data.frame(env$actor_util_df)
  p <- saomnk_plot_phase_heatmap(env, x_var = "K_AC", y_var = "utility", bins = 5)
  expect_true(is_gg(p))
  expect_identical(p$theme$legend.position, "right")
  expect_identical(p$theme$plot.title$face, "bold")
  expect_null(p$theme$plot.title$family)
  expect_true(draws(p))
})

test_that("phase-space 2D projections (the no-plotly fallback) draw in the package theme", {
  df <- data.frame(K_AC = runif(20), exploration_rate = runif(20),
                   utility = runif(20), time_norm = seq(0, 1, length.out = 20),
                   actor_id = rep(1:4, 5), strategy = "none", phase = "Exploration")
  fig <- .phase_space_2d_ggplot(df, "K_AC", "exploration_rate", "utility",
                                "x", "y", "z", color_by = "time",
                                trajectories = FALSE, smooth = 0, alpha = 0.5,
                                title = "test")
  expect_true(is_gg(fig))
  expect_true(draws(fig))
})

test_that("saomnk_plot_bipartite_system_from_mat() draws in the package style", {
  skip_if_not_installed("RSiena")
  env <- tiny_theme_env()
  B <- saomnk_get_bipartite(env)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  g <- suppressWarnings(saomnk_plot_bipartite_system_from_mat(env, B, RSIENA_ITERATION = 1))
  expect_true(inherits(g, "gtable") || inherits(g, "grob"))
})

test_that("restyled package plots carry theme_searchnet()", {
  skip_if_not_installed("RSiena")
  env <- tiny_theme_env()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  p <- suppressWarnings(saomnk_plot_k4(env))
  expect_true(is_gg(p))
  expect_s3_class(p$theme$panel.grid.minor, "element_blank")
  expect_identical(p$theme$strip.background$fill, "grey94")
  expect_no_error(suppressMessages(suppressWarnings(ggplot2::ggplot_build(p))))
})
