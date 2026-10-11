## searchnet_frames_prepare(), searchnet_frame(), searchnet_filmstrip() and
## searchnet_animate(): frames of a stored run, read without re-simulating.

.fr_env <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      env <- saomnk_env(M = 4, N = 6, density = 0.2, seed = 7)
      mod <- saomnk_model(density = -1.5, influence_matrix = saomnk_block_diagonal(6, 2),
                          influence_weight = 0.5, strategies = list(egoX = c(-1, 1, -1, 1)))
      invisible(capture.output(suppressMessages(
        saomnk_run(env, mod, steps_per_actor = 8, seed = 11))))
      cache <<- env
    }
    cache
  }
})

.fr_draw <- function(p) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  grDevices::png(f, width = 900, height = 500)
  on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
  print(p)
  TRUE
}

test_that("prepare reads the stored run and picks the most active actor", {
  env <- .fr_env()
  fr <- searchnet_frames_prepare(env, views = c("network", "k4", "landscape", "w"))
  expect_s3_class(fr, "searchnet_frames")
  n_t <- dim(env$bi_env_arr)[3]
  expect_identical(fr$n_steps, n_t)
  expect_equal(dim(fr$layout), c(env$M + env$N, 2L))
  expect_identical(fr$actor, which.max(fr$moves))
  expect_equal(sum(fr$moves), sum(vapply(seq_len(n_t), function(t)
    sum(rowSums(fr$state_at(t) != fr$state_at(t - 1L)) > 0), numeric(1))))
  ## ministep 1 is one toggle (or none) away from the start state
  expect_lte(sum(abs(fr$state_at(1L) - fr$B0)), 1)
  expect_output(print(fr), "searchnet_frames")
  ## the layout is seeded and leaves the caller's RNG alone
  set.seed(99); r1 <- stats::runif(1)
  set.seed(99); fr2 <- searchnet_frames_prepare(env); r2 <- stats::runif(1)
  expect_identical(r1, r2)
  expect_identical(fr2$layout, fr$layout)
})

test_that("a frame builds for each view and draws", {
  env <- .fr_env()
  fr <- searchnet_frames_prepare(env, views = c("network", "k4", "landscape", "w"))
  for (v in c("network", "k4", "landscape", "w")) {
    p <- searchnet_frame(fr, 5, views = v)
    expect_s3_class(p, "ggplot")
    expect_identical(attr(p, "step"), 5L)
    expect_true(.fr_draw(p))
  }
  p <- searchnet_frame(env, 0, views = c("network", "k4", "landscape"))
  expect_s3_class(p, "ggplot")
  expect_true(.fr_draw(p))
  expect_error(searchnet_frame(fr, fr$n_steps + 1L), "step must be")
  expect_error(searchnet_frame(fr, 1, views = "nope"), "views must be")
})

test_that("the landscape view is skipped with a message for large N", {
  env <- .fr_env()
  fr <- searchnet_frames_prepare(env, views = c("network", "landscape"), max_n = 4L)
  expect_message(p <- searchnet_frame(fr, 2, views = c("network", "landscape")),
                 "landscape view skipped")
  expect_s3_class(p, "ggplot")
})

test_that("the landscape view marks the actor's current portfolio and peaks", {
  env <- .fr_env()
  fr <- searchnet_frames_prepare(env, views = "landscape")
  t <- fr$n_steps %/% 2L
  land <- .sn_frames_landscape_data(fr, t)
  ref <- searchnet_actor_landscape(env, fr$actor, step = t)
  expect_equal(land$f, ref$f)
  expect_identical(land$current, ref$current)
  expect_identical(land$local_peak, ref$local_peak)
  ## the grid holds every portfolio exactly once
  expect_setequal(fr$grid$cells$index, seq_len(2^env$N))
})

test_that("the last frame equals the static K4 panel and the end-of-run ties", {
  env <- .fr_env()
  fr <- searchnet_frames_prepare(env)
  n_t <- fr$n_steps
  p <- searchnet_frame(fr, n_t, views = c("network", "k4"))
  expect_identical(unname(attr(p, "ties") > 0), unname(saomnk_get_bipartite(env) > 0))
  net <- .sn_frames_network(fr, n_t)
  expect_identical(unname(attr(net, "ties") > 0), unname(saomnk_get_bipartite(env) > 0))

  static <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(saomnk_plot_k4(env))))
  anim <- .sn_frames_k4_data(fr, n_t)
  n_static <- length(static$data)
  expect_identical(length(anim), n_static + 1L)       # plus the step marker
  expect_identical(nrow(anim[[n_static + 1L]]), 0L)   # no marker on the last frame
  for (l in seq_len(n_static)) {
    a <- anim[[l]]; s <- static$data[[l]]
    expect_identical(nrow(a), nrow(s))
    if (all(c("x", "y") %in% names(s)))
      expect_equal(a[, c("x", "y")], s[, c("x", "y")], ignore_attr = TRUE)
  }
  ## end values of each loess line (one per panel and group)
  last <- function(d) {
    d <- d[order(d$PANEL, d$group, d$x), ]
    d[!duplicated(d[, c("PANEL", "group")], fromLast = TRUE), c("PANEL", "group", "x", "y")]
  }
  lines <- which(fr$k4$kinds == "line")
  expect_gt(length(lines), 0L)
  for (l in lines) expect_equal(last(anim[[l]]), last(static$data[[l]]), ignore_attr = TRUE)

  ## a mid-run frame holds only the past, and its lines end at the marker
  mid <- .sn_frames_k4_data(fr, n_t %/% 2L)
  expect_true(all(mid[[1]]$x <= n_t %/% 2L))
  expect_equal(max(mid[[lines[1]]]$x), n_t %/% 2L)
  expect_equal(unique(mid[[n_static + 1L]]$xintercept), n_t %/% 2L)
})

test_that("filmstrip grid returns one plot with one frame per step", {
  env <- .fr_env()
  fr <- searchnet_frames_prepare(env)
  steps <- c(0L, 5L, 10L, fr$n_steps)
  g <- searchnet_filmstrip(fr, steps = steps)
  expect_s3_class(g, "ggplot")
  expect_identical(attr(g, "steps"), steps)
  expect_length(attr(g, "frames"), length(steps))
  expect_identical(attr(attr(g, "frames")[[4]], "step"), fr$n_steps)
})

test_that("filmstrip writes a grid page, a multipage PDF and numbered PNGs", {
  skip_on_cran()
  env <- .fr_env()
  fr <- searchnet_frames_prepare(env)
  steps <- c(0L, 6L, fr$n_steps)
  dir <- tempfile("frames_test_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  f <- file.path(dir, "strip.png")
  searchnet_filmstrip(fr, steps = steps, file = f, res = 40)
  expect_true(file.exists(f))

  pdf <- file.path(dir, "pages.pdf")
  out <- searchnet_filmstrip(fr, steps = steps, file = pdf, layout = "pages")
  expect_length(out, length(steps))
  expect_true(file.exists(pdf))
  expect_gt(file.size(pdf), 0)
  raw <- readBin(pdf, "raw", file.size(pdf))
  expect_length(grepRaw("/Type /Page[^s]", raw, all = TRUE), length(steps))

  png_stem <- file.path(dir, "pg.png")
  out <- searchnet_filmstrip(fr, steps = steps, file = png_stem, layout = "pages",
                             width = 6, height = 3, res = 40)
  files <- file.path(dir, sprintf("pg_%03d.png", seq_along(steps)))
  expect_identical(attr(out, "files"), files)
  expect_true(all(file.exists(files)))
  expect_setequal(list.files(dir), c("strip.png", "pages.pdf", basename(files)))
})

test_that("animate writes a GIF with the available renderer", {
  skip_on_cran()
  avail <- .sn_frames_renderers()
  skip_if_not(any(avail), "no GIF renderer available")
  env <- .fr_env()
  fr <- searchnet_frames_prepare(env)
  f <- tempfile(fileext = ".gif")
  on.exit(unlink(f))
  out <- searchnet_animate(fr, steps = c(0, 4, 8, fr$n_steps), file = f, fps = 4,
                           width = 400, height = 300, res = 40, hold_last = 0.5)
  expect_true(file.exists(f))
  expect_identical(attr(out, "renderer"), names(avail)[avail][1])
  expect_identical(rawToChar(readBin(f, "raw", 6)), "GIF89a")
  expect_identical(attr(out, "steps"), c(0L, 4L, 8L, fr$n_steps))
})

test_that("animate validates its file and renderer arguments", {
  env <- .fr_env()
  expect_error(searchnet_animate(env), "file must be")
  avail <- .sn_frames_renderers()
  if (!all(avail)) {
    missing <- names(avail)[!avail][1]
    expect_error(searchnet_animate(env, file = tempfile(fileext = ".gif"), renderer = missing),
                 "not available")
  }
  fr <- searchnet_frames_prepare(env)
  expect_identical(.sn_frames_default_steps(fr)[1], 0L)
  expect_identical(utils::tail(.sn_frames_default_steps(fr), 1), fr$n_steps)
  expect_lte(length(.sn_frames_default_steps(fr)), 120L)
})
