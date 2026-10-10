###############################################################################
## test-teaching-onboarding.R
## searchnet_check_setup(), searchnet_classroom_submit_batch(), and
## searchnet_validate_preset(). Synthetic data only.
###############################################################################

## ---- searchnet_check_setup ----

test_that("check_setup returns a structured result without the smoke run", {
  res <- searchnet_check_setup(run_smoke = FALSE, verbose = FALSE)
  expect_s3_class(res, "searchnet_setup_check")
  expect_named(res, c("ok", "checks", "packages", "smoke", "r_version",
                      "searchnet_version"))
  expect_true(is.logical(res$ok) && length(res$ok) == 1L)
  expect_named(res$checks, c("check", "status", "detail", "fix"))
  expect_true(all(res$checks$status %in% c("ok", "warn", "fail")))
  expect_true(all(c("R version", "searchnet version", "RSiena",
                    "Writable tempdir", "Classroom presets") %in%
                  res$checks$check))
  expect_true("RSiena" %in% res$packages$package)
  expect_false(res$smoke$ran)
  expect_identical(res$ok, !any(res$checks$status == "fail"))
  expect_output(print(res), "searchnet setup check")
})

test_that("check_setup smoke run passes, writes only to tempdir, keeps RNG", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  set.seed(99)
  before_seed <- .Random.seed
  wd_before <- list.files(getwd(), all.files = TRUE)
  res <- searchnet_check_setup(run_smoke = TRUE, verbose = FALSE)
  expect_true(res$smoke$ran)
  expect_true(res$smoke$ok, info = res$smoke$error)
  expect_identical(.Random.seed, before_seed)
  expect_setequal(list.files(getwd(), all.files = TRUE), wd_before)
  expect_length(list.files(tempdir(), pattern = "^searchnet_check_"), 0L)
})


## ---- searchnet_classroom_submit_batch ----

.batch_class <- function() {
  suppressMessages(searchnet_classroom_init(n_students = 3, n_rounds = 3,
                                            seed = 11))
}

## An add the student does not hold and a drop the student holds, so the
## decision is valid under searchnet_classroom_submit()'s rules.
.valid_move <- function(cls, actor) {
  B <- cls$env$bipartite_matrix
  list(add = which(B[actor, ] == 0)[1], drop = which(B[actor, ] == 1)[1])
}

test_that("a valid batch matches submitting the rows one at a time", {
  skip_if_not_installed("RSiena")
  cls <- .batch_class()
  mv <- lapply(1:3, function(a) .valid_move(cls, a))
  fmt <- function(x) if (is.na(x)) "" else as.character(x)
  df <- data.frame(
    timestamp  = "2026-10-10 09:00",
    student_id = c("student_1", "2", "student_3"),
    round      = 1,
    adds       = vapply(mv, function(m) fmt(m$add), ""),
    drops      = vapply(mv, function(m) fmt(m$drop), ""),
    stringsAsFactors = FALSE)
  out <- suppressMessages(searchnet_classroom_submit_batch(cls, df))
  rep <- attr(out, "batch_report")
  expect_equal(rep$status, rep("submitted", 3))
  expect_length(out$pending, 0L)

  ref <- cls
  for (a in 1:3) {
    ref <- suppressMessages(searchnet_classroom_submit(
      ref, paste0("student_", a),
      adds = if (is.na(mv[[a]]$add)) NULL else mv[[a]]$add,
      drops = if (is.na(mv[[a]]$drop)) NULL else mv[[a]]$drop))
  }
  strip <- function(d) lapply(d, function(s) s[c("actor_id", "adds", "drops")])
  expect_equal(strip(out$decisions$round_1), strip(ref$decisions$round_1))
})

test_that("a CSV file with separators in cells is read", {
  skip_if_not_installed("RSiena")
  cls <- .batch_class()
  m <- .valid_move(cls, 1)
  skip_if(is.na(m$add) || is.na(m$drop))
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  writeLines(c("student_id,round,adds,drops",
               paste0("student_1,1,\"", m$add, "\",", m$drop)), f)
  out <- suppressMessages(searchnet_classroom_submit_batch(cls, f))
  expect_equal(out$decisions$round_1$student_1$adds, as.integer(m$add))
  expect_equal(out$decisions$round_1$student_1$drops, as.integer(m$drop))
})

test_that("invalid rows are reported and valid rows still submitted", {
  skip_if_not_installed("RSiena")
  cls <- .batch_class()
  df <- data.frame(
    student_id = c("student_1", "student_9", "student_2", "student_3", "student_3"),
    round      = c(1, 1, 2, 1, 1),
    adds       = c("", "1", "", "two", "1"),
    drops      = c("", "", "", "", "99"),
    stringsAsFactors = FALSE)
  out <- suppressMessages(searchnet_classroom_submit_batch(cls, df))
  rep <- attr(out, "batch_report")
  expect_equal(rep$status,
               c("submitted", "invalid", "invalid", "invalid", "invalid"))
  expect_match(rep$problem[2], "unknown student")
  expect_match(rep$problem[3], "not the current round")
  expect_match(rep$problem[4], "not a list of activity numbers")
  ## A named preset sets N itself (12), whatever N the call passes.
  expect_match(rep$problem[5], paste0("activity 99 outside 1-", cls$N))
  expect_match(rep$problem[4], "more than one row")
  expect_setequal(names(out$decisions$round_1), "student_1")
})

test_that("atomic = TRUE submits nothing when any row is invalid", {
  skip_if_not_installed("RSiena")
  cls <- .batch_class()
  df <- data.frame(student_id = c("student_1", "nobody"), round = 1,
                   adds = "", drops = "", stringsAsFactors = FALSE)
  expect_error(searchnet_classroom_submit_batch(cls, df, atomic = TRUE),
               "nothing was submitted")
  df$student_id[2] <- "student_2"
  out <- suppressMessages(searchnet_classroom_submit_batch(cls, df,
                                                           atomic = TRUE))
  expect_setequal(names(out$decisions$round_1), c("student_1", "student_2"))
})

test_that("missing columns give a clear error", {
  skip_if_not_installed("RSiena")
  cls <- .batch_class()
  expect_error(searchnet_classroom_submit_batch(
    cls, data.frame(student = "student_1", round = 1)), "Missing column")
})


## ---- searchnet_validate_preset ----

.good_preset <- function() {
  list(industry = "workshop", N = 4, activity_names = c("A", "B", "C", "D"),
       epistasis = "modular", blocks = 2, density = 0.3,
       density_param = -0.5, popularity = 0.2, influence_weight = 0.4,
       difficulty_settings = list(
         intro        = list(steps_per_round = 5, n_AI = 2, shock_probability = 0),
         intermediate = list(steps_per_round = 10, n_AI = 4, shock_probability = 0.1),
         advanced     = list(steps_per_round = 20, n_AI = 8, shock_probability = 0.3)))
}

test_that("the shipped presets and a well-formed list validate", {
  ## Source tree under devtools, the installed copy under R CMD check.
  dir <- file.path(.searchnet_source_root() %||% "", "inst", "teaching", "presets")
  if (!dir.exists(dir)) dir <- system.file("teaching", "presets", package = "searchnet")
  files <- list.files(dir, pattern = "_preset[.]json$", full.names = TRUE)
  expect_length(files, 3L)
  for (f in files) expect_silent(searchnet_validate_preset(f, quiet = TRUE))
  p <- searchnet_validate_preset(.good_preset(), quiet = TRUE)
  expect_identical(p$activity_names, c("A", "B", "C", "D"))
  ## The older key name is accepted.
  old <- .good_preset(); old$influence_weight <- NULL; old$epistasis_weight <- 0.4
  expect_silent(searchnet_validate_preset(old, quiet = TRUE))
})

test_that("bad presets fail with every problem listed", {
  bad <- .good_preset()
  bad$N <- 1
  bad$density <- 1.5
  bad$influence_weight <- NULL
  bad$activity_names <- c("A", "A")
  bad$difficulty_settings$advanced <- NULL
  bad$difficulty_settings$intro$n_AI <- 0
  err <- tryCatch(searchnet_validate_preset(bad, quiet = TRUE),
                  error = conditionMessage)
  for (pat in c("N must be", "density must be", "influence_weight is required",
                "duplicates", "missing: advanced", "intro\\$n_AI")) {
    expect_match(err, pat)
  }
  expect_error(searchnet_validate_preset(list(N = 4), quiet = TRUE),
               "activity_names is required")
  bad_ep <- .good_preset(); bad_ep$epistasis <- "random"
  expect_error(searchnet_validate_preset(bad_ep, quiet = TRUE), "not implemented")
  expect_error(searchnet_validate_preset(tempfile(fileext = ".json")),
               "not found")
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f))
  writeLines("{ not json", f)
  expect_error(searchnet_validate_preset(f), "not valid JSON")
})

test_that("unknown fields and short activity lists warn rather than fail", {
  p <- .good_preset()
  p$colour <- "blue"
  expect_warning(searchnet_validate_preset(p, quiet = TRUE), "unknown field")
  p <- .good_preset()
  p$activity_names <- c("A", "B")
  expect_warning(searchnet_validate_preset(p, quiet = TRUE), "Activity_k")
})

test_that("classroom_init(industry = 'custom') validates and accepts a path", {
  skip_if_not_installed("RSiena")
  bad <- .good_preset(); bad$density <- 2
  expect_error(searchnet_classroom_init(2, industry = "custom",
                                        custom_params = bad),
               "Invalid classroom preset")
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f))
  jsonlite::write_json(.good_preset(), f, auto_unbox = TRUE)
  cls <- suppressMessages(searchnet_classroom_init(
    2, industry = "custom", custom_params = f, seed = 4))
  expect_s3_class(cls, "searchnet_classroom")
  expect_equal(cls$N, 4)
  expect_equal(cls$n_ai, 2)
  expect_identical(cls$activity_names, c("A", "B", "C", "D"))
})
