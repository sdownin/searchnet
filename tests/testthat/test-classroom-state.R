###############################################################################
## test-classroom-state.R
## searchnet_classroom_advance() must carry the board from round to round.
###############################################################################

## Regression (2026-10-07): each round called saomnk_run(), which runs
## search_rsiena(restart = TRUE) and so reset the board to the initial matrix:
## every round started from round 0, and the students' moves applied just
## before the AI step were wiped as well.
test_that("two classroom advances form one path: round 2 starts where round 1 ended", {
  skip_if_not_installed("RSiena")
  cls <- suppressMessages(searchnet_classroom_init(n_students = 2, n_rounds = 3,
                                                   N = 6, seed = 3))
  B0 <- cls$env$bipartite_matrix_init

  cls <- suppressMessages(searchnet_classroom_advance(cls, force = TRUE))
  B1 <- cls$env$bipartite_matrix
  expect_equal(unname(cls$env$path_start_matrix), unname(matrix(as.numeric(B0), nrow(B0))))

  cls <- suppressMessages(searchnet_classroom_advance(cls, force = TRUE))
  expect_equal(cls$current_round, 2)
  ## Round 2's simulated path starts from round 1's end state ...
  expect_equal(unname(cls$env$path_start_matrix), unname(matrix(as.numeric(B1), nrow(B1))))
  ## ... which is not the initial board (otherwise the check above is vacuous).
  expect_false(isTRUE(all.equal(unname(B1), unname(B0))))
})

test_that("a student's move survives into the AI step of the same round", {
  skip_if_not_installed("RSiena")
  cls <- suppressMessages(searchnet_classroom_init(n_students = 1, n_rounds = 2,
                                                   N = 6, seed = 5))
  B0 <- cls$env$bipartite_matrix
  add <- which(B0[1, ] == 0)[1]
  skip_if(is.na(add), "student 1 already holds every activity")
  cls <- suppressMessages(searchnet_classroom_submit(cls, "student_1", adds = add))
  cls <- suppressMessages(searchnet_classroom_advance(cls))
  ## The AI step starts from the board with the student's move applied.
  expect_equal(cls$env$path_start_matrix[1, add], 1)
})

## Same function, same class of defect: a scheduled shock wrote to
## model$effects[[1]], which a saomnk_model does not have, so the density the
## AI firms simulate under never changed.
test_that("a scheduled classroom shock changes the density the round runs under", {
  skip_if_not_installed("RSiena")
  cls <- suppressMessages(searchnet_classroom_init(n_students = 1, n_rounds = 2,
                                                   N = 6, seed = 5))
  dens <- function(m) Filter(function(e) identical(e$effect, "density"),
                             m$dv_bipartite$effects)[[1]]$parameter
  d0 <- dens(cls$model)
  cls$shock_schedule <- data.frame(round = 1, shock_type = "cost_spike",
                                   magnitude = 1, stringsAsFactors = FALSE)
  cls <- suppressMessages(searchnet_classroom_advance(cls, force = TRUE))
  expect_equal(dens(cls$model), d0 - 1)
  expect_null(cls$model$effects)
  expect_true(all(cls$env$theta_matrix[, "density"] == d0 - 1))
})
