## Translation registry: entries, runnable restrictions, translation, figures.

skip_if_not_installed("yaml")

.rs_home <- file.path(pkg_root, "inst", "rosetta")
## Under R CMD check there is no source tree: use the registry installed with
## the package under test.
if (!file.exists(file.path(.rs_home, "classes.yaml")))
  .rs_home <- system.file("rosetta", package = "searchnet")
skip_if_not(file.exists(file.path(.rs_home, "classes.yaml")), "registry directory not found")
old_opt <- options(searchnet.rosetta_path = .rs_home)
withr::defer(options(old_opt), teardown_env())

## A registry copy with the shipped classes and the given entry files.
.rs_temp_registry <- function(entries = character(0)) {
  d <- tempfile("registry_")
  dir.create(file.path(d, "entries"), recursive = TRUE)
  file.copy(file.path(.rs_home, "classes.yaml"), d)
  if (length(entries)) file.copy(entries, file.path(d, "entries"))
  d
}

test_that("every shipped entry passes the schema, id and Lean checks", {
  v <- rosetta_validate(include_private = TRUE)
  err <- v[v$level == "error", , drop = FALSE]
  expect_equal(nrow(err), 0, info = paste(err$id, err$code, err$message, collapse = "\n"))
  expect_true(attr(v, "ok"))
  ## the Lean library was found, so E3 actually ran
  expect_false("A5" %in% v$code)
})

test_that("every Lean declaration an entry names is in the package library", {
  known <- searchnet_known <- .rosetta_lean_known()
  skip_if(is.null(known), "Lean sources not found")
  es <- .rosetta_load(include_private = TRUE)
  decls <- unique(unlist(lapply(es, function(e) unlist(e$lean))))
  expect_gt(length(decls), 10)
  for (d in decls) expect_true(d %in% known, info = d)
})

test_that("validation separates errors from advisory measurements", {
  bad <- file.path(tempdir(), "bad-entry.yaml")
  writeLines(c("schema_version: \"1\"", "id: bad-entry", "title: Bad", "model: Bad",
               "citations: [{key: x_2000, text: X}]", "relation: nests-everything",
               "statement: A sentence with an em\u2014dash.",
               "restrictions: {W: any}",
               "terms: [{class: nonsense, status: active}, {class: scope, status: maybe}]",
               "lean: [SaomNK.no_such_theorem]", "does_not_cover: []",
               "provenance: {ai_drafted: true, reviewed: false}"), bad, useBytes = TRUE)
  d <- .rs_temp_registry(bad)
  v <- rosetta_validate(path = d)
  expect_false(attr(v, "ok"))
  codes <- v$code[v$level == "error"]
  expect_true(all(c("E1", "E3", "E4", "E5") %in% codes))
  expect_true(all(c("A1", "A2") %in% v$code[v$level == "advisory"]))
  ## advisory findings alone never fail the check
  ok <- .rs_temp_registry(file.path(.rs_home, "entries", "nk-adaptive-walk.yaml"))
  v2 <- rosetta_validate(path = ok)
  expect_true(attr(v2, "ok"))
  expect_true(any(v2$level == "advisory"))
})

test_that("rosetta_model builds and runs every public entry's restriction", {
  ids <- rosetta_entries()$id
  expect_true(all(c("nk-adaptive-walk", "plain-saom-two-mode", "brock-durlauf-social-interactions",
                    "logit-quantal-response-equilibrium") %in% ids))
  for (id in ids) {
    e <- rosetta_entry(id)
    M <- if (is.numeric(e$restrictions$M)) NULL else 3L
    s <- rosetta_model(id, M = M, N = 5L, K = 2L, seed = 7L)
    expect_s3_class(s, "rosetta_spec")
    r <- suppressWarnings(rosetta_run(s, steps_per_actor = 2, seed = 7L))
    expect_equal(dim(r$B), c(s$M, 5L), info = id)
    expect_true(all(vapply(r[c("K_AC", "K_CA", "K_AA", "K_CC")], is.numeric, TRUE)), info = id)
    expect_equal(r$K_AC, mean(rowSums(r$B)), info = id)
    expect_true(all(r$B %in% c(0, 1)), info = id)
  }
})

test_that("the NK restriction is the classical adaptive walk", {
  s <- rosetta_model("nk-adaptive-walk", N = 6L, K = 2L, seed = 3L)
  expect_equal(s$M, 1L)
  expect_true(is.infinite(s$beta))
  ## Diagonal 0 (XWX sums over j != h): each row holds exactly K partners, and
  ## the pattern is nk_landscape()'s off the diagonal.
  expect_equal(unname(rowSums(s$influence_matrix)), rep(2, 6))
  expect_true(all(diag(s$influence_matrix) == 0))
  nk <- nk_landscape(6, 2, model = "random", seed = 3L)
  expect_equal(unname(s$influence_matrix), unname(nk$influence_matrix * 1) - diag(6))
  expect_length(.rosetta_meets(s, rosetta_entry("nk-adaptive-walk")), 0)
  r <- rosetta_run(s)
  expect_equal(r$engine, "nk_walk")
  lo <- nk_local_optima(r$landscape)
  term <- nk_code(as.integer(r$B))
  nb <- vapply(1:6, function(j) bitwXor(term, as.integer(2^(6 - j))), 1L)
  expect_true(all(r$landscape$fitness[nb + 1] <= r$landscape$fitness[term + 1]))
})

test_that("translation: canonical NK nests in a full model, not the reverse", {
  full <- saomnk_model(density = -1, popularity = -0.2,
                       influence_matrix = saomnk_block_diagonal(6, 2),
                       c4 = list(effect = "cycle4", parameter = 0.1))
  tr <- rosetta_translate(full)
  expect_s3_class(tr, "rosetta_translation")
  expect_true("nk-adaptive-walk" %in% tr$contains$id)
  expect_true("plain-saom-two-mode" %in% tr$contains$id)
  expect_false("nk-adaptive-walk" %in% tr$within$id)
  expect_equal(tr$terms$status[tr$terms$class == "complementarity"], "active")
  expect_equal(tr$terms$k_channel[tr$terms$class == "crowding"], "K_AA")

  nk <- rosetta_translate("nk-adaptive-walk")
  expect_true("nk-adaptive-walk" %in% nk$within$id)
  expect_false("plain-saom-two-mode" %in% nk$contains$id)
  expect_false("logit-quantal-response-equilibrium" %in% nk$contains$id)
  ## comparisons are related, never nested
  expect_false("rivkin-imitation" %in% c(tr$contains$id, tr$within$id))
})

test_that("nesting verdicts follow the shared rule", {
  full <- saomnk_model(density = -1, popularity = -0.2,
                       influence_matrix = saomnk_block_diagonal(6, 2),
                       c4 = list(effect = "cycle4", parameter = 0.1))
  v <- rosetta_translate(full)$nesting
  vd <- setNames(v$verdict, v$id)
  ## the engine's spec is multinomial; NK and Blume ask for single-flip revision
  expect_equal(vd[["nk-adaptive-walk"]], "conditional")
  expect_equal(vd[["blume-logit-dynamics"]], "conditional")
  expect_equal(vd[["plain-saom-two-mode"]], "nests")
  expect_equal(vd[["nk-population-search"]], "nests")
  expect_equal(vd[["rivkin-imitation"]], "does_not_nest")
  ## a single-flip spec reaches NK in the beta limit, Brock-Durlauf in the M limit
  s <- rosetta_model_from_json(list(M = 4, N = 5, revision_rule = "single_flip",
    influence_matrix = saomnk_block_diagonal(5, 2),
    effects = list(list(effect = "XWX", parameter = 1), list(effect = "density", parameter = -1),
                   list(effect = "inPop", parameter = 0.2))))
  v2 <- rosetta_translate(s)$nesting
  vd2 <- setNames(v2$verdict, v2$id)
  expect_equal(vd2[["nk-adaptive-walk"]], "nests_in_limit")
  expect_match(v2$limits[v2$id == "nk-adaptive-walk"], "beta to infinity")
  expect_equal(vd2[["brock-durlauf-social-interactions"]], "nests_in_limit")
  expect_equal(vd2[["plain-saom-two-mode"]], "does_not_nest")
  expect_match(v2$missing[v2$id == "plain-saom-two-mode"], "contact")
  ## a class fixed at zero is not included; a free coefficient at zero is
  s0 <- rosetta_model_from_json(list(M = 3, N = 4, effects = list(
    list(effect = "inPop", parameter = 0, fix = TRUE), list(effect = "outAct", parameter = 0, fix = FALSE))))
  t0 <- rosetta_translate(s0)$terms
  expect_equal(t0$status[t0$class == "crowding"], "zero")
  expect_equal(t0$status[t0$class == "scope"], "active")
  ## the same verdicts through rosetta_compare(spec, id)
  cmp <- rosetta_compare(s, "nk-adaptive-walk", "plain-saom-two-mode")
  vv <- attr(cmp, "verdicts")
  expect_equal(vv$verdict, c("nests_in_limit", "does_not_nest"))
})

test_that("an environment and a JSON specification translate like the model", {
  s <- rosetta_model("plain-saom-two-mode", M = 3L, N = 4L)
  js <- rosetta_model_to_json(s)
  s2 <- rosetta_model_from_json(js)
  expect_equal(s2$M, 3L)
  expect_equal(vapply(s2$effects, `[[`, "", "effect"), vapply(s$effects, `[[`, "", "effect"))
  expect_equal(rosetta_translate(js)$terms$status, rosetta_translate(s)$terms$status)
  f <- tempfile(fileext = ".json"); writeLines(js, f)
  expect_equal(rosetta_model_from_json(f)$N, 4L)
  lst <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  expect_equal(rosetta_model_from_json(lst)$N, 4L)
  inf <- rosetta_model_to_json(rosetta_model("nk-adaptive-walk", N = 4L))
  expect_true(is.infinite(rosetta_model_from_json(inf)$beta))
  r <- rosetta_run(s, steps_per_actor = 2)
  te <- rosetta_translate(r$env)
  expect_equal(te$terms$status[te$terms$class == "crowding"], "active")
})

test_that("rosetta_r_code returns runnable searchnet code", {
  code <- rosetta_r_code(rosetta_model("plain-saom-two-mode", M = 3L, N = 4L))
  expect_type(code, "character")
  expect_silent(parse(text = code))
  expect_match(code, "saomnk_model\\(")
  expect_match(rosetta_r_code("nk-adaptive-walk"), "nk_walk")
})

test_that("rosetta_compare returns classes by models", {
  cmp <- rosetta_compare(list("nk-adaptive-walk", "brock-durlauf-social-interactions",
                              mine = saomnk_model(density = -1, popularity = 0.3)))
  n_cls <- nrow(rosetta_classes())
  expect_equal(nrow(cmp), n_cls)
  expect_equal(ncol(cmp), 2 + 3)
  expect_true(all(c("nk-adaptive-walk", "mine") %in% names(cmp)))
  expect_match(cmp$mine[cmp$class == "crowding"], "^active")
  expect_equal(nrow(attr(cmp, "long")), 3 * n_cls)
})

test_that("rosetta_plot builds a figure", {
  p <- rosetta_plot(NULL, compare = "nk-adaptive-walk")
  expect_s3_class(p, "ggplot")
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))
  full <- saomnk_model(density = -1, popularity = -0.2,
                       influence_matrix = saomnk_block_diagonal(5, 2))
  p2 <- rosetta_plot(full, compare = "brock-durlauf-social-interactions", glyph = FALSE)
  expect_no_error(print(p2))
  expect_no_error(print(rosetta_plot(full)))
  f <- rosetta_plot(full, compare = "plain-saom-two-mode", glyph = FALSE,
                    file = tempfile(fileext = ".png"), width = 6, height = 5, dpi = 40)
  expect_true(file.exists(f))
})

test_that("rosetta_plot groups classes sharing a {K} dimension under one badge", {
  local_close_new_devices()  # rosetta_plot() opens a device to compose its panels
  full <- saomnk_model(density = -1, popularity = -0.2,
                       influence_matrix = saomnk_block_diagonal(6, 2),
                       c4 = list(effect = "cycle4", parameter = 0.1))
  p <- rosetta_plot(full, compare = "brock-durlauf-social-interactions", glyph = FALSE)
  g <- attr(p, "k_groups")
  ## default view: moves (k_channel, its alias). Scope moves K_CC; the
  ## crowding, contact and imitation classes all move K_AA.
  expect_equal(g$k_channel, c("K_CC", "K_AA"))
  expect_null(attr(p, "k_groups_reads"))
  expect_equal(sum(g$k_channel == "K_AA"), 1L)
  aa <- g[g$k_channel == "K_AA", ]
  expect_equal(aa$n, 3L)
  expect_equal(aa$xmax - aa$xmin, 2L)
  expect_equal(aa$x, (aa$xmin + aa$xmax) / 2)
  expect_equal(aa$classes, "crowding, contact, imitation")
  expect_equal(aa$name, "Sociality")
  expect_equal(g$classes[g$k_channel == "K_CC"], "complementarity, scope")
  expect_equal(g$name[g$k_channel == "K_CC"], "Epistasis")
  ## the row I panel draws one badge per dimension and one bracket (3 segment
  ## layers, one row per group of two or more classes)
  cl <- rosetta_classes()
  t1 <- .rosetta_terms_of_spec(.rosetta_as_spec(full, FALSE), cl)
  t1 <- .rosetta_order_by_k(t1[t1$class %in% c("complementarity", "scope", "crowding",
                                               "contact", "imitation"), , drop = FALSE])
  row1 <- .rosetta_row_objective(t1, cl, "I", g)
  is_badge <- function(l) inherits(l$geom, "GeomLabel") && isTRUE(l$geom_params$parse) &&
    any(grepl("italic(K)", l$data$badge, fixed = TRUE))
  labs <- Filter(is_badge, row1$layers)
  expect_length(labs, 1L)
  expect_equal(nrow(labs[[1]]$data), 2L)
  segs <- Filter(function(l) inherits(l$geom, "GeomSegment"), row1$layers)
  expect_length(segs, 3L)
  expect_equal(nrow(segs[[1]]$data), 2L)

  ## reads view: the dimension each class reads
  gd <- attr(rosetta_plot(full, glyph = FALSE, view = "reads"), "k_groups")
  expect_equal(gd$k_channel, c("K_CC", "K_AC", "K_AA", "K_CA"))
  expect_equal(gd$classes[gd$k_channel == "K_AA"], "crowding, contact")
  expect_equal(gd$classes[gd$k_channel == "K_CA"], "imitation")
  expect_equal(gd$classes[gd$k_channel == "K_AC"], "scope")
  ## display names: "decision" is the reads view, "outcome" the moves view
  expect_identical(attr(rosetta_plot(full, glyph = FALSE, view = "decision"), "k_groups"), gd)
  expect_identical(attr(rosetta_plot(full, glyph = FALSE, view = "outcome"), "k_groups"),
                   attr(rosetta_plot(full, glyph = FALSE, view = "moves"), "k_groups"))

  ## both views: two badge rows, decision above outcome, each with brackets;
  ## within a moves group the classes are ordered by what they read
  pb <- rosetta_plot(full, glyph = FALSE, view = "both")
  go <- attr(pb, "k_groups"); gb <- attr(pb, "k_groups_reads")
  expect_equal(go$k_channel, c("K_CC", "K_AA"))
  expect_equal(go$classes, c("complementarity, scope", "crowding, contact, imitation"))
  expect_equal(gb$k_channel, c("K_CC", "K_AC", "K_AA", "K_CA"))
  expect_equal(gb$classes[gb$k_channel == "K_CA"], "imitation")
  tb <- .rosetta_order_by_k(t1, by = "moves", then = "reads")
  rowb <- .rosetta_row_objective(tb, cl, "I", .rosetta_k_groups(tb, by = "moves"),
                                 .rosetta_k_groups(tb, by = "reads"))
  lb <- Filter(is_badge, rowb$layers)
  expect_length(lb, 2L)
  expect_equal(unname(vapply(lb, function(l) nrow(l$data), 1L)), c(4L, 2L))
  expect_no_error(print(pb))
  expect_error(rosetta_plot(full, glyph = FALSE, view = "sideways"))

  ## an out-of-order class list is regrouped, stably within a dimension
  tt <- data.frame(class = c("crowding", "scope", "contact", "complementarity"),
                   k_channel = c("K_AA", "K_AC", "K_AA", "K_CC"), stringsAsFactors = FALSE)
  expect_equal(.rosetta_order_by_k(tt)$class, c("complementarity", "scope", "crowding", "contact"))

  ## registered classes on a new dimension group the same way, after the shipped ones
  on.exit(rosetta_reset_classes(), add = TRUE)
  expect_error(rosetta_register_class("bad", effects = "x", k_channel = "AL"))
  rosetta_register_class("legitimacy", effects = "legitStat", k_channel = "K_AL",
                         k_label = "Legitimacy")
  rosetta_register_class("endorsement", effects = "endorseStat", k_channel = "K_AL")
  rosetta_register_class("rivalry", effects = "rivalStat", moves = "K_AA",
                         reads = "K_CA")
  expect_error(rosetta_register_class("clash", effects = "x", k_channel = "K_AA",
                                      moves = "K_CC"))
  rc <- rosetta_classes()
  expect_equal(rc$k_channel[rc$id == "rivalry"], "K_AA")
  expect_equal(rc$reads[rc$id == "rivalry"], "K_CA")
  expect_equal(rc$reads[rc$id == "legitimacy"], "K_AL")
  expect_true(attr(rosetta_validate(), "ok"))
  s <- rosetta_model_from_json(list(M = 3, N = 4, effects = list(
    list(effect = "legitStat", parameter = 0.3), list(effect = "inPop", parameter = -0.1),
    list(effect = "endorseStat", parameter = 0.2), list(effect = "rivalStat", parameter = 0.1))))
  g2 <- attr(rosetta_plot(s, glyph = FALSE), "k_groups")
  expect_equal(g2$k_channel, c("K_CC", "K_AA", "K_AL"))
  expect_equal(g2$classes[g2$k_channel == "K_AA"], "crowding, contact, imitation, rivalry")
  expect_equal(g2$classes[g2$k_channel == "K_AL"], "legitimacy, endorsement")
  expect_equal(g2$name[g2$k_channel == "K_AL"], "Legitimacy")
  expect_no_error(parse(text = g2$badge))
  rosetta_reset_classes()
  expect_false("K_AL" %in% names(.rosetta_k_labels()))
})

test_that("the binary pattern panel is 1[W != 0] off the diagonal", {
  W <- saomnk_block_diagonal(6, 2)
  pp <- .rosetta_w_panels(W)
  d <- pp[[2]]$layers[[1]]$data
  expect_true(all(d$b[d$r == d$c] == 0))
  off <- d$r != d$c
  expect_equal(d$b[off], as.numeric(W[cbind(d$r, d$c)][off] != 0))
  expect_equal(pp[[2]]$layers[[2]]$data$k, unname(rowSums(W != 0) - (diag(W) != 0)))
})

test_that("the K per row bars carry their counts as labels", {
  W <- saomnk_block_diagonal(6, 2)
  W[1, 4] <- 0.5                       # row 1 gains a partner: counts differ by row
  p2 <- .rosetta_w_panels(W)[[2]]
  is_text <- vapply(p2$layers, function(l) inherits(l$geom, "GeomText"), TRUE)
  expect_true(any(is_text))
  lab <- p2$layers[[which(is_text)[1]]]
  b <- ggplot2::layer_data(p2, which(is_text)[1])
  k <- unname(rowSums(W != 0) - (diag(W) != 0))
  expect_equal(as.numeric(as.character(b$label)), k)
  ## each label sits just past the end of its own bar
  bars <- p2$layers[[2]]$data
  expect_true(all(b$x > bars$xmax))
  expect_equal(lab$data$r, seq_len(6))
})

test_that("rosetta_model returns W with a zero diagonal for every W form", {
  for (id in c("nk-adaptive-walk", "plain-saom-two-mode", "rivkin-imitation")) {
    s <- rosetta_model(id, M = if (id == "nk-adaptive-walk") NULL else 3L, N = 6L, K = 2L,
                       seed = 11L)
    if (!is.null(s$influence_matrix))
      expect_true(all(diag(s$influence_matrix) == 0), info = id)
  }
  ## the one-actor run recovers K from the off-diagonal pattern
  s <- rosetta_model("nk-adaptive-walk", N = 5L, K = 3L, seed = 2L)
  s$K <- NULL
  expect_equal(rosetta_run(s)$landscape$K, 3L)
})

test_that("a restricted N survives YAML 1.1 boolean coercion (loader, export, model)", {
  src <- readLines(file.path(.rs_home, "entries", "nk-adaptive-walk.yaml"))
  d <- .rs_temp_registry()
  ## An UNQUOTED key `N:` is boolean FALSE to a YAML 1.1 reader.
  bare <- sub('^  "N": null', "  N: 4", src)
  expect_false(identical(bare, src))
  f <- file.path(d, "entries", "nk-adaptive-walk.yaml")
  writeLines(bare, f)
  expect_true("FALSE" %in% names(yaml::read_yaml(f)$restrictions))   # the trap
  e <- rosetta_entry("nk-adaptive-walk", path = d)
  expect_equal(e$restrictions$N, 4L)
  expect_false("FALSE" %in% names(e$restrictions))
  expect_true(isTRUE(e$provenance$ai_drafted) || isFALSE(e$provenance$ai_drafted))
  s <- rosetta_model("nk-adaptive-walk", path = d)
  expect_equal(s$N, 4L)
  withr::with_options(list(searchnet.rosetta_path = d), {
    js <- jsonlite::fromJSON(rosetta_export_json(), simplifyVector = FALSE)
  })
  r <- js$entries[[1]]$restrictions
  expect_equal(r$N, 4L)
  expect_false("FALSE" %in% names(r))
  ## The validator flags the unquoted key (E8) and passes it once quoted.
  v <- rosetta_validate(path = d)
  expect_true("E8" %in% v$code)
  expect_false(attr(v, "ok"))
  writeLines(sub('^  "N": null', '  "N": 4', src), f)
  v2 <- rosetta_validate(path = d)
  expect_false("E8" %in% v2$code)
  expect_equal(rosetta_model("nk-adaptive-walk", path = d)$N, 4L)
  ## yes/no values still read as logicals; y/n stay strings
  g <- tempfile(fileext = ".yaml")
  writeLines(c("a: yes", "b: no", "c: On", "d: n", "Y: 1", "e: true"), g)
  y <- .rosetta_read_yaml(g)
  expect_identical(y[c("a", "b", "c", "e")], list(a = TRUE, b = FALSE, c = TRUE, e = TRUE))
  expect_identical(y$d, "n")
  expect_identical(y$Y, 1L)
})

test_that("every shipped and private entry quotes its boolean-like keys", {
  v <- rosetta_validate(include_private = TRUE)
  expect_false("E8" %in% v$code, info = paste(v$message[v$code == "E8"], collapse = "; "))
})

test_that("private entries are read only on request and skipped when absent", {
  d <- .rs_temp_registry(file.path(.rs_home, "entries", c("nk-adaptive-walk.yaml",
                                                          "plain-saom-two-mode.yaml")))
  expect_false(dir.exists(file.path(d, "entries-private")))
  a <- rosetta_entries(include_private = TRUE, path = d)
  expect_equal(sort(a$id), c("nk-adaptive-walk", "plain-saom-two-mode"))
  dir.create(file.path(d, "entries-private"))
  file.copy(file.path(.rs_home, "entries", "rivkin-imitation.yaml"),
            file.path(d, "entries-private"))
  expect_equal(nrow(rosetta_entries(path = d)), 2)
  b <- rosetta_entries(include_private = TRUE, path = d)
  expect_equal(nrow(b), 3)
  expect_equal(b$visibility[b$id == "rivkin-imitation"], "private")
  js <- jsonlite::fromJSON(rosetta_export_json(), simplifyVector = FALSE)
  vis <- rosetta_entries(include_private = TRUE)
  expect_setequal(vapply(js$entries, `[[`, "", "id"), vis$id[vis$visibility == "public"])
})

test_that("a user-registered class round-trips through translation and export", {
  on.exit(rosetta_reset_classes(), add = TRUE)
  rosetta_register_class("prestige", effects = "prestigeStat", k_channel = "K_CA",
                         color = "#123456", construct = "status of components")
  expect_true("prestige" %in% rosetta_classes()$id)
  s <- rosetta_model_from_json(list(M = 3, N = 4, effects = list(
    list(effect = "prestigeStat", parameter = 0.4), list(effect = "inPop", parameter = -0.1))))
  expect_equal(s$effects[[1]]$class, "prestige")
  tr <- rosetta_translate(s)
  expect_equal(tr$terms$status[tr$terms$class == "prestige"], "active")
  expect_equal(tr$terms$k_channel[tr$terms$class == "prestige"], "K_CA")
  js <- jsonlite::fromJSON(rosetta_export_json(), simplifyVector = FALSE)
  ids <- vapply(js$classes, `[[`, "", "id")
  expect_true("prestige" %in% ids)
  expect_equal(js$classes[[which(ids == "prestige")]]$color, "#123456")
  expect_error(rosetta_register_class("Bad Id", effects = "x"))
  rosetta_reset_classes()
  expect_false("prestige" %in% rosetta_classes()$id)
})

test_that("the export follows the JSON contract", {
  f <- tempfile(fileext = ".json")
  rosetta_export_json(f)
  js <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_equal(js$schema_version, "1")
  expect_true(all(c("id", "label", "effects", "k_channel", "reads", "moves",
                    "color", "lean_stat", "description") %in% names(js$classes[[1]])))
  for (c0 in js$classes) expect_identical(c0$k_channel, c0$moves)
  e <- js$entries[[1]]
  expect_true(all(c("id", "title", "model", "citations", "relation", "restrictions", "terms",
                    "constructs", "lean", "does_not_cover") %in% names(e)))
  expect_true(all(c("class", "status", "expression", "construct") %in% names(e$terms[[1]])))
  st <- unlist(lapply(js$entries, function(e) vapply(e$terms, `[[`, "", "status")))
  expect_true(all(st %in% c("active", "fixed", "zero", "absent", "linearized")))
})

test_that("a template entry from a user's model validates", {
  mod <- saomnk_model(density = -1, popularity = -0.3)
  d <- .rs_temp_registry()
  f <- rosetta_new_entry("my-crowding-theory", from = mod, dir = file.path(d, "entries"))
  expect_true(file.exists(f))
  v <- rosetta_validate(path = d)
  expect_true(attr(v, "ok"), info = paste(v$message[v$level == "error"], collapse = "; "))
  expect_equal(rosetta_entry("my-crowding-theory", path = d)$terms[[3]]$status, "active")
})

test_that("rosetta_lean names the statements and writes an instance where supported", {
  l <- rosetta_lean("nk-adaptive-walk", dir = tempdir())
  expect_s3_class(l, "rosetta_lean")
  expect_true("SaomNK.utility_eq_nkFitness" %in% l$declarations$decl)
  expect_true(all(l$declarations$in_registry))
  expect_true(file.exists(l$instance))
  expect_match(paste(readLines(l$instance), collapse = "\n"), "greedy_limit")
  l2 <- rosetta_lean(rosetta_model("plain-saom-two-mode", M = 2L, N = 3L), dir = tempdir())
  expect_true(file.exists(l2$instance))
  l3 <- rosetta_lean(rosetta_model("plain-saom-two-mode", M = 2L, N = 9L), dir = tempdir())
  expect_null(l3$instance)
})

test_that("row II: the relation label and the first column do not overlap (nk-adaptive-walk)", {
  ## Boxes measured independently of the layout code: the panel width comes
  ## from the built gtable, text widths from grid at the size drawn.
  ## Order-proof: the registry is the source tree's (never an installed
  ## searchnet's), and every device opened here is closed on exit, even when
  ## an expectation errors, so no later test inherits it.
  withr::local_options(searchnet.rosetta_path = .rs_home)
  cl <- rosetta_classes()
  e <- rosetta_entry("nk-adaptive-walk")
  core <- c("complementarity", "scope", "crowding", "contact", "imitation")
  t2 <- .rosetta_terms_of_entry(e, cl)
  t2 <- .rosetta_order_by_k(t2[t2$class %in% core, , drop = FALSE])
  n <- nrow(t2)
  expect_gt(nchar(t2$expression[t2$class == "complementarity"]), 26)  # long enough to matter
  wid <- function(lab, size, parse = FALSE) {
    grid::convertWidth(grid::grobWidth(grid::textGrob(if (parse) parse(text = lab) else lab,
      gp = grid::gpar(fontsize = size * ggplot2::.pt, lineheight = 0.9))), "inches",
      valueOnly = TRUE)
  }
  ## Measures one row on its own null PDF device and closes it on exit.
  measure <- function(row, W) {
    grDevices::pdf(NULL, width = W, height = 2.5)
    dev <- grDevices::dev.cur()
    on.exit(if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev), add = TRUE)
    g <- ggplot2::ggplotGrob(row)
    panel_in <- W - sum(grid::convertWidth(g$widths, "inches", valueOnly = TRUE))
    upi <- (n + 0.7) / panel_in
    is_text <- vapply(row$layers, function(l) inherits(l$geom, "GeomText"), TRUE)
    rel_l <- Filter(function(l) isTRUE(l$geom_params$parse), row$layers[is_text])[[1]]
    expr_l <- Filter(function(l) identical(all.vars(l$mapping$label), "expr"),
                     row$layers[is_text])
    d <- row$data
    list(rel_l = rel_l, n_expr = length(expr_l),
         rel_right = rel_l$data$x +
           wid(rel_l$data$label, rel_l$aes_params$size, parse = TRUE) * upi,  # hjust = 0
         half = vapply(seq_len(n), function(i) wid(d$expr[i], d$esize[i]) * upi / 2, 1))
  }
  n_dev <- length(grDevices::dev.list())
  for (W in c(6, 8, 12)) {
    row <- .rosetta_row_special(t2, cl, e, width_in = W)
    m <- measure(row, W)
    rel_l <- m$rel_l; rel_right <- m$rel_right; half <- m$half
    d <- row$data
    expect_equal(m$n_expr, 1L)
    ## The layout's own reservation agrees: the label ends before column 1's
    ## allotted half-width begins.
    ex <- ifelse(t2$status %in% .ROSETTA_ON, paste("+", t2$expression), "+ 0")
    ex[1] <- sub("^\\+ ", "", ex[1])
    lay <- .rosetta_row2_layout(ex, e$relation, n, width_in = W)
    expect_equal(lay$expr, d$expr)
    expect_lt(lay$rel_right, 1 - lay$half[1])
    expect_equal(rel_l$aes_params$hjust, 0)
    expect_lt(rel_right, d$x[1] - half[1])                          # label | column 1
    expect_true(all(d$x[-n] + half[-n] < d$x[-1] - half[-1]))      # column | column
    expect_true(all(d$esize >= 1.8))
  }
  expect_equal(length(grDevices::dev.list()), n_dev)                 # no device left open
})
