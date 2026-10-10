###############################################################################
## test-plot-readable.R
## The reader-friendly defaults of the main plots (R/plot-readable.R and the
## functions that use it): computed takeaway titles, plain-language
## subtitles, {K}-dimension strip labels with display names, the model weights in
## the caption, and the optional reading guides (annotate = TRUE / FALSE).
###############################################################################

draws_ok <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  invisible(TRUE)
}

n_layers <- function(p) length(p$layers)

readable_env <- function(shock = FALSE, strategies = FALSE, M = 4) {
  env <- saomnk_env(M = M, N = 6, density = 0, seed = 42)
  mod <- if (strategies) {
    saomnk_model(density = -0.5, popularity = 0.2,
                 influence_matrix = saomnk_block_diagonal(6, 2), influence_weight = 0.3,
                 strategies = list(egoX = c(-1, 1, -1, 1)))
  } else {
    saomnk_model(density = -0.5, popularity = 0.2,
                 influence_matrix = saomnk_block_diagonal(6, 2), influence_weight = 0.3)
  }
  shocks <- if (shock) list(saomnk_shock("density", parameter = -0.5, portion = 1),
                            saomnk_shock("density", parameter = -2.0, portion = 1))
  invisible(capture.output(suppressMessages(
    saomnk_run(env, mod, steps_per_actor = 12, seed = 12345, shocks = shocks))))
  env
}

test_that("{K}-dimension labels carry the display names and partner counts", {
  lab <- searchnet:::.sn_k_label(c("K_AC", "K_CA", "K_AA", "K_CC"))
  expect_match(lab[1], "K[AC]", fixed = TRUE)
  expect_match(lab, "Expansiveness|Popularity|Sociality|Epistasis")
  expect_match(lab[3], "Sociality: actors sharing a component", fixed = TRUE)
  expect_match(lab[4], "Epistasis: components sharing an actor", fixed = TRUE)
})

test_that("plot labels use the dimension display names of the registry (terminology rule)", {
  ## One set of names everywhere: Expansiveness (K_AC), Popularity (K_CA),
  ## Sociality (K_AA), Epistasis (K_CC) -- the plot strips, the rosetta badges
  ## (classes.yaml k_channel_label) and inst/rosetta/K_DIMENSIONS.md. K_CC is never
  ## labeled "coupling" (author decision 2026-08-21, test-terminology.R).
  info <- searchnet:::.sn_k_info
  expect_identical(info$plain, c("expansiveness", "popularity", "sociality", "epistasis"))
  lab <- searchnet:::.sn_k_label(info$channel)
  expect_false(any(grepl("coupling", lab, ignore.case = TRUE)))
  skip_if_not_installed("yaml")
  reg <- searchnet:::.rosetta_k_labels()
  expect_identical(unname(reg[info$channel]), info$name)
  expect_identical(tolower(info$name), info$plain)
  ch <- readLines(file.path(searchnet:::.rosetta_home(), "K_DIMENSIONS.md"))
  for (nm in c("Expansiveness (K_AC)", "Popularity (K_CA)", "Sociality (K_AA)", "Epistasis (K_CC)"))
    expect_true(any(grepl(nm, ch, fixed = TRUE)), info = nm)
  expect_true(all(vapply(lab, function(l) is.expression(parse(text = l)), TRUE)))
  expect_identical(searchnet:::.sn_join(c("a", "b", "c")), "a, b and c")
  expect_identical(searchnet:::.sn_join("a"), "a")
})

test_that(".sn_settle_step() finds a plateau and refuses a trend", {
  x <- seq_len(100)
  y <- pmin(x, 40)
  s <- searchnet:::.sn_settle_step(x, y)
  expect_true(s >= 35 && s <= 45)
  expect_true(is.na(searchnet:::.sn_settle_step(x, x)))       # still rising
  expect_true(is.na(searchnet:::.sn_settle_step(x, rep(1, 100))))
})

test_that("saomnk_plot_k4() returns a ggplot with readable defaults; annotate toggles guides", {
  skip_if_not_installed("RSiena")
  env <- readable_env()
  p1 <- suppressWarnings(saomnk_plot_k4(env))
  p0 <- suppressWarnings(saomnk_plot_k4(env, annotate = FALSE))
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p0, "ggplot")
  expect_gt(n_layers(p1), n_layers(p0))
  expect_identical(p0$labels$title, p1$labels$title)
  expect_false(grepl("Environment:", p1$labels$title, fixed = TRUE))
  expect_match(p1$labels$caption, "^Model: ")
  expect_match(p1$labels$subtitle, "Black line: mean degree", fixed = TRUE)
  expect_identical(p1$labels$x, "Ministep (one decision opportunity)")
  ch <- levels(p1$data$channel)
  expect_length(ch, 4)
  expect_match(ch[1], "K[AC]", fixed = TRUE)
  expect_match(ch[4], "Epistasis", fixed = TRUE)
  ## One actor group and one component group: no legend.
  expect_true(draws_ok(p1))
  expect_true(draws_ok(p0))
})

test_that("a shocked run names the shock in the title and marks it", {
  skip_if_not_installed("RSiena")
  env <- readable_env(shock = TRUE)
  sh <- searchnet:::.sn_shock_boundaries(env)
  expect_s3_class(sh, "data.frame")
  expect_identical(sh$label[1], "density -0.5 to -2")
  p <- suppressWarnings(saomnk_plot_k4(env))
  expect_match(p$labels$title, sprintf("^The shock at ministep %d", sh$step[1]))
  vl <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)
  expect_length(vl, 1)
  expect_identical(vl[[1]]$aes_params$colour, "#D55E00")
  txt <- unlist(lapply(p$layers, function(l)
    if (inherits(l$geom, "GeomText") && "label" %in% names(l$data)) l$data$label))
  expect_true(any(grepl("shock: density -0.5 to -2", txt, fixed = TRUE)))
  expect_true(draws_ok(p))
})

test_that("strategies give a legend and per-group colors", {
  skip_if_not_installed("RSiena")
  env <- readable_env(strategies = TRUE)
  p <- suppressWarnings(saomnk_plot_degree_4panel(env, annotate = FALSE))
  expect_s3_class(p, "ggplot")
  sc <- p$scales$get_scales("colour")
  expect_false(identical(sc$guide, "none"))
  expect_true(draws_ok(p))
})

test_that("actor and component degree plots: two dimensions, annotate on and off", {
  skip_if_not_installed("RSiena")
  env <- readable_env()
  pa <- suppressWarnings(saomnk_plot_actor_degrees(env))
  pc <- suppressWarnings(saomnk_plot_component_degrees(env, annotate = FALSE))
  expect_s3_class(pa, "ggplot")
  expect_s3_class(pc, "ggplot")
  expect_identical(length(levels(pa$data$channel)), 2L)
  expect_match(levels(pa$data$channel)[1], "K[AC]", fixed = TRUE)
  expect_match(levels(pc$data$channel)[2], "K[CC]", fixed = TRUE)
  expect_gt(n_layers(suppressWarnings(env$plot_actor_degrees())),
            n_layers(suppressWarnings(env$plot_actor_degrees(annotate = FALSE))))
  expect_true(draws_ok(pa))
  expect_true(draws_ok(pc))
})

test_that("utility decomposition: plain strip labels, computed title, annotate toggles", {
  skip_if_not_installed("RSiena")
  env <- readable_env()
  p1 <- suppressWarnings(saomnk_plot_utility(env))
  p0 <- suppressWarnings(saomnk_plot_utility(env, annotate = FALSE))
  expect_s3_class(p1, "ggplot")
  expect_gt(n_layers(p1), n_layers(p0))
  pans <- levels(p1$data$panel)
  expect_match(pans[1], "^Total utility")
  expect_true(any(grepl("crowding (inPop)", pans, fixed = TRUE)))
  expect_match(p1$labels$title, "largest")
  expect_match(p1$labels$caption, "^Model: ")
  pb <- suppressWarnings(saomnk_plot_utility_contributions_basic(env, annotate = FALSE))
  expect_s3_class(pb, "ggplot")
  expect_true(draws_ok(p1))
  expect_true(draws_ok(pb))
})

test_that("searchnet_causal_plot(type = 'did') states the ATT and annotate toggles", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("did")
  env_t <- readable_env(shock = TRUE, M = 6)
  env_c <- readable_env(M = 6)
  s_step <- min(env_t$theta_shocks[[2]]$chain_step_ids)
  pt <- searchnet_causal_panel(env_t, shock_step = s_step, outcome = "utility")
  pc <- searchnet_causal_panel(env_c, shock_step = s_step, outcome = "utility",
                               treated_actors = integer(0))
  pc$actor_id <- factor(as.integer(as.character(pc$actor_id)) + 6L)
  common <- seq_len(min(max(pt$step), max(pc$step)))
  panel <- rbind(pt[pt$step %in% common, ], pc[pc$step %in% common, ])
  att <- NULL
  invisible(capture.output(att <- suppressWarnings(suppressMessages(searchnet_did(panel)))))
  skip_if(is.null(att))
  p1 <- suppressWarnings(searchnet_causal_plot(att))
  p0 <- suppressWarnings(searchnet_causal_plot(att, annotate = FALSE))
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p0, "ggplot")
  expect_gt(n_layers(p1), n_layers(p0))
  expect_match(p1$labels$title, "SE")
  expect_identical(p1$labels$x, "Ministeps relative to the shock")
  expect_true(draws_ok(p1))
})

test_that("plot.searchnet_ergodicity() takes annotate and keeps its return value", {
  skip_if_not_installed("RSiena")
  erg <- searchnet_ergodicity_sweep(M = 6, N = 6, run_lengths = c(5L, 10L, 20L),
                                    replicates = 3L)
  a1 <- plot(erg, draw = FALSE)
  a0 <- plot(erg, draw = FALSE, annotate = FALSE)
  expect_s3_class(a1, "searchnet_ergodicity")
  expect_s3_class(a0, "searchnet_ergodicity")
  expect_true(inherits(attr(a1, "plot"), "ggplot"))
  expect_true(inherits(attr(a0, "plot"), "ggplot"))
})

test_that("snapshots: plain panel titles; the R6 method draws the same panels", {
  skip_if_not_installed("RSiena")
  local_close_new_devices()  # building the panels and the R6 method both open one
  env <- readable_env()
  s <- saomnk_plot_snapshots(env, steps = 1, draw = FALSE)[[2]]
  expect_identical(s$bipartite$labels$title, "Who holds what")
  expect_match(s$social$labels$subtitle, "^K_AA: ")
  expect_match(s$heatmap$labels$subtitle, "^K_CC: ")
  grDevices::pdf(NULL)
  out <- env$plot_snapshots(c(1, 2), include_init = FALSE)
  expect_length(out, 2)
  expect_s3_class(out[[1]]$bipartite, "ggplot")
})

test_that("theme_searchnet(grid = FALSE) and axes = FALSE blank the major grid", {
  expect_s3_class(theme_searchnet(grid = FALSE)$panel.grid.major, "element_blank")
  expect_s3_class(theme_searchnet(axes = FALSE)$panel.grid.major, "element_blank")
  expect_s3_class(theme_searchnet()$panel.grid.major, "element_line")
})
