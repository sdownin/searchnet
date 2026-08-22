###############################################################################
## test-methods.R
## Tests for the S3 print/summary methods in R/saomnk-methods.R
##
## Each method is held to three things: it prints the substance of the object
## (key substrings, no snapshots), it returns its argument, and it returns it
## invisibly. The last two are what make the methods composable -- print(x)
## inside a pipe or a loop must neither alter the object nor echo it twice.
###############################################################################

## ---- Source the layers under test (helper-setup.R already loads base) ----
tryCatch(
  {
    source(file.path(dir_r, "saomnk-api.R"),        local = FALSE)
    source(file.path(dir_r, "searchnet-assent.R"),  local = FALSE)
    source(file.path(dir_r, "saomnk-methods.R"),    local = FALSE)
  },
  error = function(e) {
    message("Could not source methods layer: ", e$message)
  }
)

## Shared assertion: print() returns its argument, invisibly.
expect_prints_invisibly <- function(obj) {
  vis <- NULL
  capture.output(vis <- withVisible(print(obj)))
  expect_false(vis$visible)
  expect_identical(vis$value, obj)
}


# ===========================================================================
# 1. print.saomnk_model()
# ===========================================================================
test_that("print.saomnk_model() shows header, effects, and thetas", {
  mod <- saomnk_model(density = -0.5, popularity = 0.2, scope = 0.1)
  out <- capture.output(print(mod))
  expect_true(length(out) > 0)
  expect_true(any(grepl("SaoMNK Structure Model", out)))
  expect_true(any(grepl("density", out)))
  expect_true(any(grepl("popularity \\(inPop\\)", out)))
  expect_true(any(grepl("scope \\(outAct\\)", out)))
  expect_true(any(grepl("-0\\.5", out)))
  expect_true(any(grepl("\\[fixed\\]", out)))
})

test_that("print.saomnk_model() returns its argument invisibly", {
  mod <- saomnk_model(density = -1)
  expect_prints_invisibly(mod)
})

test_that("print.saomnk_model() on a make_minimal_structure_model()-style fixture mentions its effects", {
  ## The fixture list is exactly what saomnk_model() assembles, minus the
  ## class; stamping the class must be enough for the method to describe it.
  m <- make_minimal_structure_model()
  class(m) <- c("saomnk_model", "list")
  out <- capture.output(print(m))
  expect_true(any(grepl("density", out)))
  expect_true(any(grepl("-1", out)))
})

test_that("print.saomnk_model() reports a static influence matrix with its dimensions", {
  mod <- saomnk_model(density = -1,
                      influence_matrix = saomnk_block_diagonal(6, 2),
                      influence_weight = 0.15)
  out <- capture.output(print(mod))
  expect_true(any(grepl("epistasis \\(XWX\\)", out)))
  expect_true(any(grepl("influence matrix W", out)))
  expect_true(any(grepl("6 x 6", out)))
  expect_true(any(grepl("0\\.15", out)))
})

test_that("print.saomnk_model() reports time-varying influence arrays with periods", {
  W2 <- array(c(saomnk_block_diagonal(6, 2), saomnk_block_diagonal(6, 3)),
              dim = c(6, 6, 2))
  mod <- saomnk_model(density = -1,
                      influence_arrays = list(W_regime = W2),
                      influence_array_weights = c(W_regime = 0.2))
  out <- capture.output(print(mod))
  expect_true(any(grepl("Time-varying influence", out)))
  expect_true(any(grepl("6 x 6 over 2 period", out)))
})

test_that("print.saomnk_model() reports actor strategy covariates", {
  mod <- saomnk_model(density = -1,
                      strategies = list(egoX = c(-1, 0, 1, -1)))
  out <- capture.output(print(mod))
  expect_true(any(grepl("covariates", out)))
  expect_true(any(grepl("egoX", out)))
})

test_that("print.saomnk_model() reports an attached dv_behavior block", {
  mod <- saomnk_model(density = -1)
  mod$dv_behavior <- saomnk_behavior(values = c(1, 2, 1, 3))
  out <- capture.output(print(mod))
  expect_true(any(grepl("dv_behavior.*present", out)))
  ## default behaviour spec: linear + quad, 2 waves
  expect_true(any(grepl("2 effect\\(s\\), 2 wave", out)))
})

test_that("print.saomnk_model() omits sections that are empty", {
  mod <- saomnk_model(density = -1)
  out <- capture.output(print(mod))
  expect_false(any(grepl("Influence / dyadic covariates", out)))
  expect_false(any(grepl("Time-varying influence", out)))
  expect_false(any(grepl("dv_behavior", out)))
})


# ===========================================================================
# 2. print.saomnk_shock()
# ===========================================================================
test_that("print.saomnk_shock() shows effect, target value, and portion", {
  s <- saomnk_shock("density", parameter = -2.0, portion = 2)
  out <- capture.output(print(s))
  expect_true(any(grepl("SaoMNK shock segment", out)))
  expect_true(any(grepl("portion: 2", out)))
  expect_true(any(grepl("density", out)))
  expect_true(any(grepl("-2", out)))
})

test_that("print.saomnk_shock() labels mapped shortcodes with their friendly names", {
  s <- saomnk_shock(c("popularity", "scope"), parameter = c(0.5, 0.3))
  out <- capture.output(print(s))
  expect_true(any(grepl("popularity \\(inPop\\)", out)))
  expect_true(any(grepl("scope \\(outAct\\)", out)))
  expect_true(any(grepl("sets 2 parameter", out)))
})

test_that("print.saomnk_shock() returns its argument invisibly", {
  s <- saomnk_shock("density", parameter = -2.0)
  expect_prints_invisibly(s)
})


# ===========================================================================
# 3. print.saomnk_assent()
# ===========================================================================
test_that("print.saomnk_assent() describes a uniform-probability rule", {
  a <- saomnk_assent(prob = 0.5)
  out <- capture.output(print(a))
  expect_true(any(grepl("assent", out)))
  expect_true(any(grepl("uniform confirmation probability", out)))
  expect_true(any(grepl("0\\.5", out)))
})

test_that("print.saomnk_assent() describes per-actor probabilities", {
  a <- saomnk_assent(prob = c(0.2, 0.4, 0.6, 0.8))
  out <- capture.output(print(a))
  expect_true(any(grepl("per-actor", out)))
  expect_true(any(grepl("length 4", out)))
})

test_that("print.saomnk_assent() describes a dyad-specific probability matrix", {
  a <- saomnk_assent(prob = matrix(0.5, nrow = 4, ncol = 8))
  out <- capture.output(print(a))
  expect_true(any(grepl("probability matrix \\(4 x 8\\)", out)))
})

test_that("print.saomnk_assent() describes attribute screening with rates and threshold", {
  a <- saomnk_assent(actor_attribute = c(0, 1, 1, 0),
                     rate_high = 0.62, rate_low = 0.16, threshold = 0.5,
                     name = "quality_screen")
  out <- capture.output(print(a))
  expect_true(any(grepl("attribute screening", out)))
  expect_true(any(grepl("0\\.62", out)))
  expect_true(any(grepl("0\\.16", out)))
  expect_true(any(grepl("0\\.5", out)))
  expect_true(any(grepl("quality_screen", out)))
})

test_that("print.saomnk_assent() reports component selectivity when present", {
  a <- saomnk_assent(prob = 0.5,
                     component_selectivity = rep(0.3, 8))
  out <- capture.output(print(a))
  expect_true(any(grepl("component selectivity", out)))
  expect_true(any(grepl("length 8", out)))
})

test_that("print.saomnk_assent() returns its argument invisibly", {
  a <- saomnk_assent(prob = 0.5)
  expect_prints_invisibly(a)
})


# ===========================================================================
# 4. saomnk_summary() classed return + print.saomnk_summary()
# ===========================================================================
test_that("saomnk_summary() returns a saomnk_summary object that prints the table once", {
  skip_if_not_installed("RSiena")

  env <- tryCatch(
    run_tiny_sim(M = 4, N = 6, iterations_per_actor = 5, rand_seed = 55),
    error = function(e) stop(paste("Tiny sim failed:", e$message))
  )

  ## The construction itself must not print: display belongs to the method.
  quiet <- capture.output(s <- saomnk_summary(env))
  expect_s3_class(s, "saomnk_summary")
  expect_true(is.character(s))

  out <- capture.output(print(s))
  expect_true(length(out) > 0)
  ## screenreg output carries the model header and the fixed density effect
  expect_true(any(grepl("Model", out)))
  ## No quoted-string dump: the raw escapes must not appear in the output
  expect_false(any(grepl("\\\\n", out)))

  expect_prints_invisibly(s)
})
