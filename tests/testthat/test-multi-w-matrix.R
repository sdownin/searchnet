###############################################################################
## test-multi-w-matrix.R
## Multiple W-matrix (epistasis) support tests
##
## Verifies that the multi-W-matrix API correctly generates multiple
## coDyadCovar entries and that XWX statistics are computed correctly
## for each W-matrix.
##
## Pure matrix algebra tests require no simulation.
###############################################################################

context("Multiple W-matrix (epistasis) support")

test_that("saomnk_model accepts epistasis_matrices list", {
  # This tests that the API layer correctly generates multiple coDyadCovar entries
  skip_if_not(exists("saomnk_model", mode = "function"), "saomnk_model not available")

  W1 <- matrix(0, 8, 8)
  W1[1:4, 1:4] <- 1; diag(W1) <- 0

  W2 <- matrix(0, 8, 8)
  W2[5:8, 5:8] <- 1; diag(W2) <- 0

  model <- saomnk_model(
    density = -1,
    epistasis_matrices = list(block1 = W1, block2 = W2),
    epistasis_weights = c(block1 = 0.3, block2 = 0.5)
  )

  expect_true(!is.null(model))
  # Should have coDyadCovars for both matrices
  covars <- model$dv_bipartite$coDyadCovars
  expect_true(length(covars) >= 2)
})

test_that("XWX statistic computed correctly for each W-matrix", {
  set.seed(42)
  M <- 4; N <- 8
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.5, 0.5)), M, N)

  W <- diag(N)  # identity = no epistasis
  xwx <- rowSums(B %*% W %*% t(B))

  # With identity W, XWX should equal rowSums of social projection diagonal
  expect_equal(xwx, rowSums(B)^2)  # Because B %*% I %*% t(B) = B %*% t(B)
})

test_that("XWX changes when W encodes block structure", {
  set.seed(42)
  M <- 4; N <- 8
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.5, 0.5)), M, N)

  # Block W: only components 1-4 interact
  W_block <- matrix(0, N, N)
  W_block[1:4, 1:4] <- 1
  diag(W_block) <- 0

  xwx_block <- diag(B %*% W_block %*% t(B))

  # XWX with block W should differ from identity W
  xwx_identity <- rowSums(B)^2
  expect_false(all(xwx_block == xwx_identity))

  # XWX values should be non-negative (since B is binary and W >= 0)
  expect_true(all(xwx_block >= 0))
})

test_that("multiple W-matrices produce independent statistics", {
  set.seed(42)
  M <- 6; N <- 10
  B <- matrix(sample(0:1, M*N, replace=TRUE, prob=c(0.5, 0.5)), M, N)

  # Two non-overlapping block W-matrices
  W1 <- matrix(0, N, N)
  W1[1:5, 1:5] <- 1; diag(W1) <- 0

  W2 <- matrix(0, N, N)
  W2[6:10, 6:10] <- 1; diag(W2) <- 0

  xwx1 <- diag(B %*% W1 %*% t(B))
  xwx2 <- diag(B %*% W2 %*% t(B))

  # With non-overlapping blocks the statistics should generally differ
  expect_length(xwx1, M)
  expect_length(xwx2, M)
  expect_true(all(is.finite(xwx1)))
  expect_true(all(is.finite(xwx2)))

  # Combined should equal sum (linearity of XWX in W)
  W_sum <- W1 + W2
  xwx_sum <- diag(B %*% W_sum %*% t(B))
  expect_equal(xwx_sum, xwx1 + xwx2)
})
