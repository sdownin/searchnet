###############################################################################
## test-lean.R
##
## Formal verification with Lean 4 (R/lean-tools.R, R/lean-export.R,
## inst/lean). Offline tests (spec extraction, exact rationals, file
## generation, hash stability, the index convention, registry integrity) always
## run. Build tests export small models, check them against the SaomNK library
## and audit their axioms; they run only when a Lean toolchain and a built
## library are available, and never on CRAN.
###############################################################################

.lean_test_home <- function() {
  root <- if (exists("pkg_root")) pkg_root else normalizePath(file.path("..", ".."))
  h <- file.path(root, "inst", "lean")
  if (!file.exists(file.path(h, "lakefile.toml"))) h <- system.file("lean", package = "searchnet")
  h
}
withr::local_options(searchnet.lean_home = .lean_test_home(), .local_envir = teardown_env())

## ---------------------------------------------------------------------------
## Offline
## ---------------------------------------------------------------------------

test_that("lean_home() finds the shipped Lean project", {
  h <- lean_home()
  expect_true(file.exists(file.path(h, "lakefile.toml")))
  expect_true(file.exists(file.path(h, "lake-manifest.json")))
  expect_true(file.exists(file.path(h, "SaomNK.lean")))
  expect_match(readLines(file.path(h, "lean-toolchain"))[1], "^leanprover/lean4:")
})

test_that("the effect map covers the effects lean_spec() exports", {
  m <- lean_effect_map()
  expect_true(all(c("density", "outAct", "inPop", "cycle4", "XWX", "egoX", "altX") %in% m$effect))
  expect_true(all(c("effect", "slot", "lean", "potential", "proof", "note") %in% names(m)))
})

test_that("exact rationals reproduce the doubles exactly", {
  set.seed(1)
  xs <- c(0, 1, -1, 0.1, 1 / 3, -2.5e-7, 123456.789, runif(20, -10, 10))
  for (x in xs) {
    f <- .lean_frac(x)
    expect_identical(as.numeric(f$num) / 2^f$k, x)
  }
  f <- .lean_frac(0.1234567, digits = 4)
  expect_identical(f$num, "1235"); expect_identical(f$k, 4L)
})

test_that("Lean's LSB code is the row order of the engine's landscape table", {
  ## codeLSB_eq_sum: row(b) = 1 + sum_d b_d 2^(d-1). The engine enumerates
  ## configurations with expand.grid(), whose first column varies fastest.
  for (N in 1:5) {
    grid <- as.matrix(expand.grid(replicate(N, 0:1, simplify = FALSE)))
    expect_identical(as.vector(grid %*% 2^(0:(N - 1))), as.numeric(0:(2^N - 1)))
  }
  ## codeMSB_eq_sum: nk_code() is the classical power key.
  b <- c(1, 0, 1, 1)
  expect_identical(nk_code(b), as.integer(sum(b * 2^(3:0))))
})

test_that("lean_spec() reads an nk_landscape in LSB row order", {
  nk <- nk_landscape(N = 4, K = 2, seed = 3)
  sp <- lean_spec(nk)
  expect_s3_class(sp, "searchnet_lean_spec")
  expect_identical(c(sp$M, sp$N, sp$K), c(1L, 4L, 2L))
  for (code in 0:15) {
    b <- as.integer(intToBits(code))[1:4]
    expect_equal(sp$contrib[code + 1, ], nk$contributions[nk_code(b) + 1, ])
  }
  expect_true(.lean_table_consistent(sp$E, sp$contrib))
})

test_that("the R mirror of utilityQ is NK fitness for one actor, and counts nk_local_optima", {
  for (s in 1:4) {
    nk <- nk_landscape(N = 4, K = 1 + s %% 3, seed = s)
    sp <- lean_spec(nk)
    ns <- .lean_num_spec(sp, NULL, exact = FALSE)
    for (code in c(0, 5, 10, 15)) {
      B <- matrix(as.integer(intToBits(code))[1:4], 1)
      expect_equal(.lean_utility(ns, B, 1), nk$fitness[nk_code(B) + 1])
    }
    cl <- .lean_count_local_opt(ns, exact = FALSE)
    expect_identical(cl$count, nrow(nk_local_optima(nk)))
  }
})

test_that("lean_spec() reads a SaomNkRSienaBiEnv model definition", {
  skip_if_not_installed("RSiena")
  skip_if_not_installed("psych")
  env <- tryCatch(SaomNkRSienaBiEnv$new(make_small_environ_params(M = 2, N = 3, rand_seed = 5)),
                  error = function(e) skip(paste("init failed:", conditionMessage(e))))
  env$config_structure_model <- make_strategy_structure_model(2)
  tryCatch(env$compute_fitness_landscape(n_landscapes = 1, verbose = FALSE),
           error = function(e) skip(paste("landscape failed:", conditionMessage(e))))
  sp <- lean_spec(env)
  expect_identical(c(sp$M, sp$N), c(2L, 3L))
  ## the engine routes a bipartite 'density' to RSiena's outAct
  expect_equal(unname(sp$theta["outAct"]), -1)
  ## egoX enters as an actor-specific own term, centered as RSiena centers it
  expect_equal(sp$ego, 0.5 * (c(0, 1) - 0.5))
  expect_identical(dim(sp$contrib), c(8L, 3L))
  expect_identical(sp$config, (env$bipartite_matrix != 0) * 1L)
  f <- lean_export_model(sp, dir = withr::local_tempdir())
  expect_true(file.exists(f))
})

test_that("lean_spec() lists and warns about unmapped effects", {
  expect_warning(sp <- lean_spec(list(M = 2, N = 3, theta = c(density = 1, transTriads = 2))),
                 "transTriads")
  expect_true("transTriads" %in% sp$unmapped)
})

test_that("export is deterministic: same model, same hash and bytes", {
  d1 <- withr::local_tempdir(); d2 <- withr::local_tempdir()
  nk <- nk_landscape(N = 3, K = 1, seed = 11)
  m <- list(M = 2, N = 3, landscape = nk,
            theta = c(nk = 1, outAct = 0.05, inPop = -0.2, cycle4 = 0.1),
            config = matrix(c(1, 0, 1, 0, 1, 1), 2, byrow = TRUE))
  f1 <- lean_export_model(m, dir = d1); f2 <- lean_export_model(m, dir = d2)
  expect_identical(basename(f1), basename(f2))
  expect_identical(unname(tools::md5sum(f1)), unname(tools::md5sum(f2)))
  expect_match(basename(f1), "^Instance_[0-9a-f]{10}\\.lean$")
  txt <- readLines(f1, encoding = "UTF-8")
  for (thm in c("E_regular", "C_consistent", "nk_is_fitness", "exact_potential",
                "exists_nash", "gibbs_stationary", "index_convention", "count_local_opt"))
    expect_true(any(grepl(paste0("theorem ", thm, "\\b"), txt)), info = thm)
  expect_false(any(grepl("sorry|native_decide|^axiom", txt)))
  expect_true(any(grepl("^#print axioms ", txt)))
  ## a different coefficient changes the hash
  m$theta["inPop"] <- -0.3
  expect_false(identical(basename(lean_export_model(m, dir = d1)), basename(f1)))
})

test_that("export refuses oversized landscapes unless native = TRUE", {
  sp <- lean_spec(list(M = 1, N = 8))
  expect_error(lean_export_model(sp, dir = withr::local_tempdir()), "max_N")
})

test_that("multinomial revision does not claim Gibbs stationarity", {
  f <- lean_export_model(list(M = 2, N = 2, theta = c(outAct = -0.5, inPop = 0.2)),
                         dir = withr::local_tempdir(), revision = "multinomial")
  txt <- readLines(f, encoding = "UTF-8")
  expect_false(any(grepl("theorem gibbs_stationary", txt)))
  expect_true(any(grepl("theorem greedy_limit", txt)))
})

test_that("registry.yml names only declarations that exist in the sources", {
  skip_if_not_installed("yaml")
  reg <- lean_registry()
  expect_gt(nrow(reg), 30)
  known <- lean_declarations(kinds = c("theorem", "lemma", "def"))$decl
  expect_identical(setdiff(reg$decl, known), character(0))
})

test_that("Axioms.lean audits every theorem and lemma of the library", {
  d <- lean_declarations()
  ax <- readLines(file.path(lean_home(), "Axioms.lean"), encoding = "UTF-8")
  audited <- sub("^#print axioms ", "", grep("^#print axioms ", ax, value = TRUE))
  expect_setequal(audited, d$decl)
})

test_that("no Lean source uses sorry, axiom or native_decide", {
  f <- list.files(file.path(lean_home(), "SaomNK"), pattern = "\\.lean$", recursive = TRUE,
                  full.names = TRUE)
  txt <- paste(unlist(lapply(f, readLines, encoding = "UTF-8")), collapse = "\n")
  txt <- gsub("(?s)/-.*?-/", "", txt, perl = TRUE)           # doc and block comments
  code <- strsplit(txt, "\n")[[1]]
  code <- sub("--.*$", "", code)                          # line comments
  expect_false(any(grepl("\\bsorry\\b", code)))
  expect_false(any(grepl("^\\s*axiom ", code)))
  expect_false(any(grepl("native_decide", code)))
})

test_that("lean functions skip informatively without Lean", {
  withr::local_options(searchnet.lean_home = tempfile())
  withr::local_envvar(SEARCHNET_LEAN_HOME = "")
  withr::with_dir(tempdir(), {
    if (!is.na(lean_home())) skip("a Lean project is reachable from tempdir()")
    expect_false(isTRUE(lean_available()))
    expect_message(lean_check(tempdir()), "skipped")
  })
})

## ---------------------------------------------------------------------------
## Build: export small models, check them with Lean, audit axioms
## ---------------------------------------------------------------------------

test_that("exported instances build against SaomNK with only standard axioms", {
  skip_on_cran()
  skip_if_not(isTRUE(lean_available(check_build = TRUE)), "Lean / built SaomNK not available")
  out <- withr::local_tempdir()

  ## (1) M = 2, N = 3, K = 1: every exported effect, plus a relational layer
  nk3 <- nk_landscape(N = 3, K = 1, seed = 11)
  f1 <- lean_export_model(list(
    M = 2, N = 3, landscape = nk3,
    theta = c(nk = 1, density = -0.25, outAct = 0.05, inPop = -0.2, cycle4 = 0.1,
              XWX = 0.3, egoX = 0.5),
    W = matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3), ego = c(1, 2),
    config = matrix(c(1, 0, 1, 0, 1, 1), 2, byrow = TRUE),
    relational = list(ego = c(1, 2), theta_AB = 0.5, theta_BA = 0.25)), dir = out)
  ## (2) M = 2, N = 4, K = 2
  nk4 <- nk_landscape(N = 4, K = 2, seed = 7)
  f2 <- lean_export_model(list(
    M = 2, N = 4, landscape = nk4,
    theta = c(nk = 1, outAct = -0.02, inPop = -0.1),
    config = matrix(c(1, 1, 0, 0, 0, 1, 1, 0), 2, byrow = TRUE)), dir = out)
  ## (3) the classical NK model: one actor
  nk1 <- nk_landscape(N = 4, K = 2, seed = 3)
  f3 <- lean_export_model(nk1, dir = out)

  res <- lean_check(out)
  expect_length(attr(res, "errors"), 0)
  expect_true(all(res$status == "ok"))
  expect_true(all(vapply(strsplit(res$axioms, ", "),
                         function(a) all(a %in% c("propext", "Classical.choice", "Quot.sound")),
                         logical(1))))
  expect_false(any(grepl("sorryAx|ofReduceBool", res$axioms)))
  for (thm in c("exact_potential", "gibbs_stationary", "count_local_opt_card",
                "ego_no_relational_potential", "no_joint_potential"))
    expect_lean_theorem(res[res$file == basename(f1), ], thm)
  ## the Lean count of local optima of the one-actor model is nk_local_optima()
  fct <- attr(f3, "facts")
  expect_identical(as.integer(fct$value[fct$decl == "count_local_opt"]), nrow(nk_local_optima(nk1)))
  expect_lean_theorem(res[res$file == basename(f3), ], "count_local_opt")
})

test_that("the library builds and every declaration passes the axiom audit", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("SEARCHNET_LEAN_FULL"), "true"),
              "set SEARCHNET_LEAN_FULL=true to rebuild and audit the whole library")
  skip_if_not(isTRUE(lean_available()), "Lean not available")
  res <- lean_check()
  expect_length(attr(res, "errors"), 0)
  expect_true(all(res$status == "ok"))
  expect_setequal(res$decl, lean_declarations()$decl)
})
