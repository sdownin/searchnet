###############################################################################
## test-searchnet-ergodicity.R
##
## Coverage for searchnet_ergodicity_sweep(), the Theorem 4 (SAOM-QRE
## equivalence) demonstration.
##
## The function replaced a two-point comparison that printed its own conclusion
## unconditionally. These tests are written so that the same failure mode cannot
## reappear: they check that the verdict TRACKS the data rather than being
## constant, that a deliberately under-mixed sweep reports non-equivalence, and
## that the guards fire.
##
## The full sweep is slow (minutes), so the heavy end-to-end check is gated on
## NOT_CRAN and uses a reduced grid. The logic tests below are fast.
###############################################################################

test_that("guard: equivalence margin finer than the grid resolution is refused", {
  ## On a 4 x 5 grid one tie is 0.05 of the density, so a margin of 0.01
  ## asks the grid to resolve a fifth of a tie. This is the defect that made
  ## the original demonstration meaningless, so it is an error, not a warning.
  expect_error(
    searchnet_ergodicity_sweep(M = 4, N = 5, equivalence_margin = 0.01,
                               run_lengths = 10, replicates = 2),
    "finer than the density resolution"
  )
})

test_that("guard: a single arm or a single replicate is refused", {
  expect_error(
    searchnet_ergodicity_sweep(start_densities = 0.5, run_lengths = 10,
                               replicates = 2),
    "start_densities"
  )
  expect_error(
    searchnet_ergodicity_sweep(replicates = 1, run_lengths = 10),
    "replicates"
  )
})

test_that("guard: a W of the wrong dimension is refused", {
  expect_error(
    searchnet_ergodicity_sweep(M = 6, N = 8, W = diag(5),
                               run_lengths = 10, replicates = 2),
    "must be an 8 x 8 matrix"
  )
})

test_that("sweep returns a well-formed object with the expected shape", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("NOT_CRAN"), "true"),
              "slow simulation; set NOT_CRAN=true to run")

  erg <- searchnet_ergodicity_sweep(
    M = 6, N = 8, run_lengths = c(5, 20), replicates = 3,
    equivalence_margin = 0.10, seed = 7
  )

  expect_s3_class(erg, "searchnet_ergodicity")
  expect_named(erg, c("runs", "summary", "decay", "verdict", "config", "call"))

  ## 2 run lengths x 2 arms x 3 replicates
  expect_equal(nrow(erg$runs), 2 * 2 * 3)
  expect_equal(nrow(erg$summary), 2)
  expect_true(all(erg$runs$final_density >= 0 & erg$runs$final_density <= 1))

  ## print and plot must not error
  expect_output(print(erg), "Ergodicity sweep")
  expect_silent({
    pf <- tempfile(fileext = ".png"); grDevices::png(pf)
    plot(erg); grDevices::dev.off(); unlink(pf)
  })
})

test_that("the arms actually converge: the gap decays and ends equivalent", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("NOT_CRAN"), "true"),
              "slow simulation; set NOT_CRAN=true to run")

  erg <- searchnet_ergodicity_sweep(
    M = 12, N = 15, start_densities = c(0.1, 0.8),
    run_lengths = c(15, 60, 240), replicates = 6,
    equivalence_margin = 0.05, seed = 42
  )

  ## The substantive claim: a 0.70 starting gap is largely erased.
  expect_equal(erg$verdict$initial_gap, 0.70)
  expect_lt(erg$verdict$final_gap, 0.05)
  expect_gt(erg$verdict$collapse_pct, 90)

  ## The gap at the shortest run length must be clearly larger than at the
  ## longest. If this ever fails, either the chain is mixing instantly (so the
  ## sweep demonstrates nothing and the short end needs to be shorter) or the
  ## process_chain trap has returned and the "final" matrix is the initial one.
  expect_gt(erg$summary$gap[1], 4 * erg$verdict$final_gap)

  ## The computed verdict, at the margin declared in the call.
  expect_true(erg$verdict$equivalent)

  ## Decay is fitted above the Monte Carlo floor only, and the floor is real.
  expect_true(is.finite(erg$decay$floor))
  expect_gt(erg$decay$floor, 0)
  expect_lt(erg$decay$slope, 0)          # gap falls with run length
})

test_that("the verdict is computed, not constant: under-mixing reports NOT equivalent", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("NOT_CRAN"), "true"),
              "slow simulation; set NOT_CRAN=true to run")

  ## This is the test that the old demonstration could not have passed. Run the
  ## chain far too briefly to mix, and the function must refuse to call the
  ## arms equivalent. A sweep that reported equivalence here would be printing
  ## its conclusion rather than measuring it.
  erg <- searchnet_ergodicity_sweep(
    M = 12, N = 15, start_densities = c(0.1, 0.8),
    run_lengths = 5, replicates = 5,
    equivalence_margin = 0.05, seed = 42
  )

  expect_false(erg$verdict$equivalent)
  expect_gt(erg$verdict$final_gap, 0.05)
  expect_output(print(erg), "NOT EQUIVALENT")
})
