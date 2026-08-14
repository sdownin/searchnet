## Rival identity in the entry log.
##
## The defect these tests exist for: rival COUNT was recorded and rival
## IDENTITY was not, while the docs claimed the multimarket forbearance
## construct of Baum and Korn, which is defined on the firm pair. A count
## cannot distinguish restraint toward a specific rival from a general
## preference for empty space.
##
## The trap that makes the fix worth testing: identity must be read from the
## FULL column as setdiff(which(col == 1), i), never as which(mat[-i, j] == 1).
## Dropping row i renumbers every firm above it, so the second form returns
## wrong IDs for exactly the rivals ranked after the focal actor while still
## returning the correct COUNT. Every existing count-based metric would stay
## green. test 2 is built so that the buggy form gives a different answer.

test_that("rival identity is read from the full column, not the row-dropped one", {
  ## Firm 2 is focal. Rivals are firms 4 and 5.
  ## Row-dropped indexing would report them as 3 and 4.
  col <- c(0, 1, 0, 1, 1)          # firms 1..5; focal firm 2 also holds it
  ids <- searchnet:::.rival_ids_in_column(col, i = 2)

  expect_identical(ids, c(4L, 5L))
  expect_false(identical(ids, c(3L, 4L)))   # the row-dropped answer

  ## The buggy form, stated explicitly so the difference is on the record.
  buggy <- which(col[-2] == 1)
  expect_identical(as.integer(buggy), c(3L, 4L))
  expect_false(identical(as.integer(buggy), ids))
})

test_that("focal firm is never its own rival", {
  col <- c(1, 1, 1)
  expect_identical(searchnet:::.rival_ids_in_column(col, i = 1), c(2L, 3L))
  expect_identical(searchnet:::.rival_ids_in_column(col, i = 3), c(1L, 2L))
})

test_that("count agrees with identity on every row of a tracked run", {
  ## Two rounds, five firms, four activities.
  h1 <- matrix(0L, 5, 4)
  h1[1, 1] <- 1L; h1[3, 1] <- 1L; h1[5, 1] <- 1L
  h1[2, 3] <- 1L
  h2 <- h1
  h2[2, 1] <- 1L    # firm 2 ADDs activity 1, held by firms 1, 3, 5
  h2[3, 1] <- 0L    # firm 3 DROPs activity 1

  log <- track_entry_decisions(holdings_history = list(h1, h2))

  expect_true(all(c("rival_ids", "state_idx") %in% names(log)))

  n_from_ids <- vapply(strsplit(log$rival_ids, "|", fixed = TRUE),
                       length, integer(1))
  expect_identical(n_from_ids, as.integer(log$n_rivals_present))

  add2 <- log[log$firm == 2 & log$action_type == "add", ]
  expect_equal(nrow(add2), 1)
  expect_identical(add2$rival_ids, "1|3|5")
  expect_equal(add2$n_rivals_present, 3)

  ## Firm 3 drops activity 1; rivals present are 1 and 5, not 1, 2 and 5,
  ## because rivals are read from the PRE-decision state.
  drop3 <- log[log$firm == 3 & log$action_type == "drop", ]
  expect_equal(nrow(drop3), 1)
  expect_identical(drop3$rival_ids, "1|5")
})

test_that("uncontested decisions get an empty string, not NA", {
  h1 <- matrix(0L, 3, 2)
  h2 <- h1; h2[1, 1] <- 1L        # firm 1 pioneers an empty activity
  log <- track_entry_decisions(holdings_history = list(h1, h2))

  expect_identical(log$rival_ids, "")
  expect_equal(log$n_rivals_present, 0)
  expect_false(is.na(log$rival_ids))
})

test_that("the count-only pass-through path reports NA identity, not empty", {
  cd4 <- data.frame(firm = c(1, 2), activity = c(3, 4),
                    n_rivals_present = c(2, 0),
                    was_competitive = c(TRUE, FALSE))
  log <- track_entry_decisions(entry_log = cd4)

  ## Unknown must stay distinguishable from "no rivals": treating these as
  ## uncontested would understate competitive entry.
  expect_true(all(is.na(log$rival_ids)))
  expect_true(all(is.na(log$state_idx)))
})

test_that("expand_entry_rivals produces one row per dyad and refuses counts", {
  h1 <- matrix(0L, 4, 2)
  h1[1, 1] <- 1L; h1[3, 1] <- 1L
  h2 <- h1; h2[2, 1] <- 1L        # firm 2 enters, rivals 1 and 3
  log <- track_entry_decisions(holdings_history = list(h1, h2))

  dy <- expand_entry_rivals(log)
  expect_equal(nrow(dy), 2)
  expect_identical(sort(dy$rival), c(1L, 3L))
  expect_true(all(dy$firm == 2))

  ## A log without identity must fail loudly rather than return zero dyads.
  old <- log; old$rival_ids <- NULL
  expect_error(expand_entry_rivals(old), "rival_ids")
})

test_that("multimarket contact counts shared activities with a zero diagonal", {
  H <- rbind(c(1, 1, 0, 0),
             c(1, 1, 1, 0),
             c(0, 0, 1, 1))
  C <- compute_multimarket_contact(H)

  expect_equal(C[1, 2], 2)   # activities 1 and 2
  expect_equal(C[2, 3], 1)   # activity 3
  expect_equal(C[1, 3], 0)
  expect_true(all(diag(C) == 0))
  expect_true(isSymmetric(C))
})

test_that("dyadic forbearance is signed so that positive means restraint", {
  ## Firm 1 has four empty activities available and one rival, firm 2, which
  ## holds two of them. Random choice would hit firm 2's space half the time.
  ## Firm 1 enters the two activities firm 2 does NOT hold: full restraint.
  h <- matrix(0L, 2, 4)
  h[2, 1] <- 1L; h[2, 2] <- 1L
  s1 <- h
  s2 <- s1; s2[1, 3] <- 1L
  s3 <- s2; s3[1, 4] <- 1L

  log <- track_entry_decisions(holdings_history = list(s1, s2, s3))
  fb  <- compute_dyadic_forbearance(log, holdings = list(s1, s2, s3))

  ## No contested entries at all, so no dyad is observed.
  expect_equal(nrow(fb), 0)

  ## Now the aggressive counterpart: firm 1 enters firm 2's space every time.
  a2 <- s1; a2[1, 1] <- 1L
  a3 <- a2; a3[1, 2] <- 1L
  alog <- track_entry_decisions(holdings_history = list(s1, a2, a3))
  afb  <- compute_dyadic_forbearance(alog, holdings = list(s1, a2, a3))

  expect_equal(nrow(afb), 1)
  expect_equal(afb$firm, 1)
  expect_equal(afb$rival, 2)
  expect_equal(afb$observed_rate, 1)
  expect_true(afb$exposure_corrected)
  ## Entered the rival's space more than chance, so the index is negative.
  expect_lt(afb$forbearance_index, 0)
})

test_that("dyadic forbearance without holdings returns NA index, not a number", {
  h1 <- matrix(0L, 3, 2)
  h1[2, 1] <- 1L
  h2 <- h1; h2[1, 1] <- 1L
  log <- track_entry_decisions(holdings_history = list(h1, h2))

  fb <- suppressMessages(compute_dyadic_forbearance(log))
  expect_true(all(is.na(fb$forbearance_index)))
  expect_false(any(fb$exposure_corrected))
})

test_that("holdings that are too short are rejected", {
  ## Three states, so the log carries state_idx up to 2.
  h1 <- matrix(0L, 3, 2); h1[2, 1] <- 1L
  h2 <- h1; h2[1, 1] <- 1L
  h3 <- h2; h3[3, 2] <- 1L
  log <- track_entry_decisions(holdings_history = list(h1, h2, h3))

  expect_error(compute_dyadic_forbearance(log, holdings = list(h1)),
               "outside the range")
})

test_that("holdings of the right length from a different run are rejected", {
  ## The range check cannot see this one: the substituted state has the same
  ## shape and a valid index, and would yield a plausible but wrong baseline.
  h1 <- matrix(0L, 3, 2); h1[2, 1] <- 1L
  h2 <- h1; h2[1, 1] <- 1L
  log <- track_entry_decisions(holdings_history = list(h1, h2))

  other <- matrix(0L, 3, 2); other[3, 1] <- 1L   # rival 3, not rival 2
  expect_error(compute_dyadic_forbearance(log, holdings = list(other, h2)),
               "not from the same run")
})

test_that("existing columns and metrics are unchanged by the addition", {
  h1 <- matrix(0L, 4, 3)
  h1[1, 1] <- 1L; h1[2, 2] <- 1L
  h2 <- h1; h2[3, 1] <- 1L; h2[4, 3] <- 1L
  log <- track_entry_decisions(holdings_history = list(h1, h2))

  expect_true(all(c("step", "firm", "activity", "action_type",
                    "n_rivals_present", "is_competitive_entry",
                    "is_competitive_exit") %in% names(log)))

  m <- compute_forbearance_metrics(log)
  expect_equal(m$entry_count, 2)
  expect_equal(m$competitive_entry_rate, 0.5)   # firm 3 contested, firm 4 not
  expect_equal(m$avoidance_rate, 0.5)
})
