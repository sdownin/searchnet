###############################################################################
## test-rsiena-chain-format.R
##
## RSiena returns a period's ministep chain in two formats:
##
##   - a LIST of 13-field ministep records in chain order (RSiena <= 1.5.x
##     always; RSiena >= 1.6.0 with returnDataFrame = FALSE), and
##   - a `chains.data.frame` with ten columns whose rows are SORTED BY EGO AND
##     ALTER, the chain position kept in the row names (RSiena >= 1.6.0 with
##     returnDataFrame = TRUE).
##
## The switch arrived unannounced in RSiena 1.6.x and broke every vignette
## that simulates a path (the terminus gate fired, and the 11-column frame
## builder failed on a 10-column data frame). These tests pin both formats
## with small recorded fixtures and re-check the live RSiena, so the next
## format change fails here rather than silently in a vignette.
##
## Fixtures (tests/testthat/fixtures/), one seeded bipartite + behavior
## period, M = 5, N = 4, generated with plain RSiena (no searchnet):
##   rsiena_chain_list_1.5.0.rds  RSiena 1.5.0, returnDataFrame = FALSE
##   rsiena_chain_df_1.6.6.rds    RSiena 1.6.6, returnDataFrame = TRUE
## Same seed, same chain: RSiena 1.6.6 with returnDataFrame = FALSE returns a
## list identical() to the 1.5.0 one. Each fixture holds B_start, B_end (from
## $sims), the ministeps, and the DV name.
###############################################################################

.chain_fixture <- function(name) readRDS(test_path("fixtures", name))


test_that("the recorded fixtures have the structure each format declares", {
  lst <- .chain_fixture("rsiena_chain_list_1.5.0.rds")
  df  <- .chain_fixture("rsiena_chain_df_1.6.6.rds")

  expect_type(lst$ministeps, "list")
  expect_false(is.data.frame(lst$ministeps))
  expect_true(all(vapply(lst$ministeps, length, integer(1)) == 13L))

  expect_s3_class(df$ministeps, "data.frame")
  expect_identical(names(df$ministeps), .SEARCHNET_RSIENA_CHAIN_DF_COLS)
  expect_identical(nrow(df$ministeps), length(lst$ministeps))
  ## The data frame really is sorted (by variable, then ego, then alter), not
  ## in chain order, which is why a positional reading of it would be wrong.
  d <- df$ministeps
  expect_identical(order(d$Var, d$Ego, d$Alter), seq_len(nrow(d)))
  expect_false(identical(as.integer(rownames(df$ministeps)),
                         seq_len(nrow(df$ministeps))))
})


test_that("both formats parse to the same chain-ordered frame", {
  lst <- .chain_fixture("rsiena_chain_list_1.5.0.rds")
  df  <- .chain_fixture("rsiena_chain_df_1.6.6.rds")

  f_lst <- .searchnet_chain_frame(lst$ministeps)
  f_df  <- .searchnet_chain_frame(df$ministeps)

  expect_identical(names(f_df), names(f_lst))
  expect_identical(nrow(f_df), nrow(f_lst))
  ## The data frame does not carry the list's field 12 (RSiena's missing flag).
  expect_true(all(is.na(f_df$diagonal)))
  for (cl in c("dv_type", "dv_type_bin", "dv_varname", "id_from", "id_to",
               "beh_difference", "stability"))
    expect_identical(f_df[[cl]], f_lst[[cl]], label = cl)
  for (cl in c("reciprocal_rate", "LogOptionSetProb", "LogChoiceProb"))
    expect_equal(as.numeric(f_df[[cl]]), as.numeric(f_lst[[cl]]), label = cl)

  ## The typed field reader agrees across formats.
  expect_identical(.searchnet_chain_fields(df$ministeps),
                   .searchnet_chain_fields(lst$ministeps))
  expect_identical(.searchnet_chain_n(df$ministeps), length(lst$ministeps))
})


test_that("replaying either fixture ends at RSiena's own end network", {
  for (nm in c("rsiena_chain_list_1.5.0.rds", "rsiena_chain_df_1.6.6.rds")) {
    fx <- .chain_fixture(nm)
    fr <- .searchnet_chain_frame(fx$ministeps)
    B_rep <- .searchnet_path_replay_end(fx$B_start, fr, fx$N, dv = fx$dv_name)
    expect_true(any(fx$B_start != fx$B_end), label = paste(nm, "non-vacuous"))
    expect_identical(sum(B_rep != fx$B_end), 0L, label = nm)
  }
})


test_that("the data frame's row order is not the chain order", {
  ## Toggles commute, so the end network cannot detect a wrong order; the
  ## per-ministep sequence (chain_stats, K series, plots) can. Guards the
  ## premise of the reordering: if RSiena's row order were already the chain
  ## order this test would fail and the reordering could go.
  lst <- .chain_fixture("rsiena_chain_list_1.5.0.rds")
  d <- .chain_fixture("rsiena_chain_df_1.6.6.rds")$ministeps
  rownames(d) <- NULL
  expect_false(identical(.searchnet_chain_fields(d)$ego,
                         .searchnet_chain_fields(lst$ministeps)$ego))
})


test_that("an unrecognized chain format stops loudly", {
  lst <- .chain_fixture("rsiena_chain_list_1.5.0.rds")
  df  <- .chain_fixture("rsiena_chain_df_1.6.6.rds")

  short <- lapply(lst$ministeps, function(x) x[-12])
  expect_error(.searchnet_chain_frame(short), "unrecognized RSiena ministep chain format")
  expect_error(.searchnet_chain_fields(short), "unrecognized RSiena ministep chain format")

  no_diag <- df$ministeps[, setdiff(names(df$ministeps), "Diagonal")]
  expect_error(.searchnet_chain_frame(no_diag), "without column")

  bad_rn <- df$ministeps
  rownames(bad_rn) <- paste0("r", seq_len(nrow(bad_rn)))
  expect_error(.searchnet_chain_frame(bad_rn), "order of the")
})


test_that("the installed RSiena still returns one of the two known formats", {
  skip_on_cran()
  skip_if_not_installed("RSiena")
  set.seed(3)
  M <- 5L; N <- 4L
  B <- matrix(rbinom(M * N, 1, 0.4), M, N)
  run <- function(rdf) {
    actors <- RSiena::sienaNodeSet(M, "ACTORS")
    comps  <- RSiena::sienaNodeSet(N, "COMPONENTS")
    net <- RSiena::sienaDependent(array(c(B, B), dim = c(M, N, 2)),
                                  type = "bipartite",
                                  nodeSet = c("ACTORS", "COMPONENTS"),
                                  allowOnly = FALSE)
    dat <- RSiena::sienaDataCreate(net, nodeSets = list(actors, comps))
    eff <- RSiena::getEffects(dat)
    alg <- RSiena::sienaAlgorithmCreate(projname = NULL, simOnly = TRUE,
                                        cond = FALSE, nsub = 0, n3 = 2,
                                        seed = 11L, silent = TRUE)
    th <- eff$initialValue[eff$include]
    th[1] <- 2
    fit <- NULL
    utils::capture.output(fit <- RSiena::siena07(
      alg, data = dat, effects = eff, thetaValues = rbind(th, th),
      batch = TRUE, silent = TRUE, returnDeps = TRUE, returnChains = TRUE,
      returnDataFrame = rdf))
    el <- fit$sims[[1]][[1]][[1]][[1]]
    B_end <- matrix(0, M, N)
    B_end[cbind(el[, 1], el[, 2])] <- el[, 3]
    list(ms = fit$chain[[1]][[1]][[1]], B_end = B_end)
  }
  a <- run(FALSE)
  b <- run(TRUE)

  ## searchnet's own simulations ask for returnDataFrame = FALSE, and must get
  ## the 13-field list format on every RSiena version.
  expect_false(is.data.frame(a$ms))
  expect_true(all(vapply(a$ms, length, integer(1)) == 13L))
  ## Whatever comes back with returnDataFrame = TRUE must parse to the same
  ## chain, and both must replay to RSiena's end network.
  expect_identical(.searchnet_chain_fields(b$ms), .searchnet_chain_fields(a$ms))
  for (r in list(a, b)) {
    B_rep <- .searchnet_path_replay_end(B, .searchnet_chain_frame(r$ms), N,
                                        dv = "net")
    expect_identical(sum(B_rep != r$B_end), 0L)
  }
})
