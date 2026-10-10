###############################################################################
## test-plot-snapshots-palette.R
## saomnk_plot_snapshots(): colorblind-safe default palette, the legacy
## palette, node color overrides, and the returned (restylable) objects.
###############################################################################

local_close_new_devices()

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")

tiny_snapshot_env <- function() {
  env <- saomnk_env(M = 4, N = 6, density = 0.2, seed = 42)
  mod <- saomnk_model(density = -0.5, popularity = 0.2,
                      strategies = list(egoX = c(-1, 1, -1, 1)))
  invisible(capture.output(suppressMessages(
    saomnk_run(env, mod, steps_per_actor = 4, seed = 12345))))
  env
}

## Endpoints of a continuous color scale, as hex.
scale_ends <- function(p, aesthetic) {
  sc <- ggplot2::ggplot_build(p)$plot$scales$get_scales(aesthetic)
  toupper(substr(sc$palette(c(0, 1)), 1, 7))
}

test_that("default palette uses only Okabe-Ito node colors", {
  skip_if_not_installed("RSiena")
  env <- tiny_snapshot_env()
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  snaps <- saomnk_plot_snapshots(env, steps = 2)

  expect_length(snaps, 2)                 # initial state + step 2
  expect_identical(vapply(snaps, `[[`, numeric(1), "step"), c(0, 2))
  s <- snaps[[2]]
  expect_s3_class(s$social, "ggplot")
  expect_s3_class(s$bipartite, "ggplot")
  expect_s3_class(s$heatmap, "ggplot")
  expect_true(inherits(s$grob, "grob"))

  ## Every node color drawn in the bipartite panel is an Okabe-Ito color, so
  ## no red-green pair can occur among them.
  drawn <- unique(toupper(ggplot2::layer_data(s$bipartite, 2)$colour))
  expect_true(all(drawn %in% okabe_ito), info = paste(drawn, collapse = ", "))
  expect_true(all(toupper(unlist(s$colors)) %in% okabe_ito))

  ## Components are Okabe-Ito blue (initially unused) and reddish purple
  ## (initially used); actors in the projection keep their strategy colors.
  expect_identical(toupper(unname(s$colors$components)), c("#0072B2", "#CC79A7"))
  social_cols <- unique(toupper(ggplot2::layer_data(s$social, 2)$colour))
  expect_true(all(social_cols %in% toupper(s$colors$actors)),
              info = paste(social_cols, collapse = ", "))

  ## The heatmap runs from white to Okabe-Ito blue, not the old white to red.
  heat_ends <- scale_ends(s$heatmap, "fill")
  expect_identical(heat_ends[1], "#FFFFFF")
  expect_true(heat_ends[2] %in% c("#0072B2", "#FFFFFF"))   # white only if empty
})

test_that("legacy palette restores the earlier colors", {
  skip_if_not_installed("RSiena")
  env <- tiny_snapshot_env()
  snaps <- saomnk_plot_snapshots(env, steps = 2, palette = "legacy", draw = FALSE)
  s <- snaps[[2]]
  expect_identical(unname(s$colors$components), c("darkgreen", "tan"))
  expect_identical(unname(s$colors$actors), scales::hue_pal()(2))
  expect_identical(scale_ends(s$social, "colour"), c("#00FF00", "#FF0000"))
})

test_that("node_colors overrides, draw = FALSE, and argument checks", {
  skip_if_not_installed("RSiena")
  env <- tiny_snapshot_env()
  lev <- levels(env$get_actor_strategies())
  over <- stats::setNames(c("#111111", "#222222"), c(lev[1], "new"))
  s <- saomnk_plot_snapshots(env, steps = 1, node_colors = over, draw = FALSE)[[2]]
  expect_identical(s$colors$actors[[lev[1]]], "#111111")
  expect_identical(s$colors$components[["new"]], "#222222")

  ## Each actor takes its own strategy's color.
  strat <- as.character(env$get_actor_strategies())
  node_cols <- ggplot2::layer_data(s$bipartite, 2)
  expect_identical(toupper(node_cols$colour[seq_along(strat)]),
                   toupper(unname(s$colors$actors[strat])))

  expect_error(saomnk_plot_snapshots(env, steps = 1, node_colors = c(bogus = "red"),
                                     draw = FALSE), "not recognized")
  expect_error(saomnk_plot_snapshots(env, steps = 1, palette = "rainbow"))
  expect_error(saomnk_plot_snapshots(env, steps = 10^6, draw = FALSE), "steps must lie")
})
