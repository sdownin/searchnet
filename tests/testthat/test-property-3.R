###############################################################################
## test-property-3.R
##
## Property 3 (approximation of NK landscapes) of the paper, Section 6 /
## online Appendix C; Property 3 in inst/proofs/PROOF_TABLE.md (Part E,
## E1-E12). Lean: SaomNK.nkTermFull_in_dummy_span.
##
## Statement: for any NK landscape (N, K, A, {f_d}) there is a SAOM-NK
## parameterization that reproduces its fitness function (the paper: "to
## arbitrary precision"), by three strategies: (a) dummy covariates,
## (b) actor heterogeneity, (c) mixed logit.
##
## What is checked, on landscapes built by nk_landscape() across N, K and all
## three neighborhood models:
##
##   1. Strategy (a), Prop 1 / E2-E4, EXACTLY: one configuration dummy
##      w_{d,c}(x) = 1[x restricted to the neighborhood of d = c] per component
##      d and neighborhood pattern c (N * 2^(K+1) of them), with coefficient
##      alpha_{d,c} = f_d(c), sums to N * W(x) at every one of the 2^N
##      configurations; exactly N dummies are active at each x.
##   2. The package's own NK term reproduces the same function: the SAOM-NK
##      evaluation function of lean_spec(<nk_landscape>) (theta_nk = 1, the
##      engine's `nk` table effect), evaluated with the package's .lean_utility,
##      equals the dummy reconstruction divided by N, at every configuration.
##   3. NEGATIVE CONTROL: dummies built on a WRONG neighborhood (one partner
##      dropped) cannot reproduce a K >= 1 landscape; the least-squares
##      residual is far from zero.
##
## What holds for the other strategies (reported, not weakened):
##
##   4. Strategy (b), Prop 2 / E5-E7, as written in E6 enters utility as
##      u_i(b_i) = sum_d b_id sum_q gamma_q z_q^(i,d): LINEAR in the actor's own
##      row. A linear function reproduces an NK landscape only when K = 0; for
##      K >= 1 the best linear fit leaves a residual bounded away from zero,
##      whatever Q and M are. The parameter count Q * M >= 2^(K+1) is
##      necessary for matching dimensionality but not sufficient for
##      reproduction. Checked below.
##   5. The engine's built-in covariate effects (density / outAct / altX / XWX)
##      span functions of interaction order at most 2 in the own row. They
##      reproduce NK landscapes with K <= 1 exactly and generically fail for
##      K >= 2, so outside the `nk` table term and configuration dummies the
##      property needs effects RSiena does not provide. Checked below.
##   6. Strategy (c), Prop 3 / E8-E11 (McFadden-Train mixed logit) is an
##      approximation result about choice probabilities of random-utility
##      models, imported from the literature; it is not numerically checkable
##      as a property of the package and is not tested here.
##
## Pure enumeration on 2^N configurations, N <= 8. No RSiena call.
###############################################################################

## ---- helpers ---------------------------------------------------------------

## Configuration-dummy design for neighborhoods `deps` (list of locus vectors):
## one column per (d, pattern), in the pattern code used by nk_landscape()
## (bits of x[deps[[d]]] weighted 2^(0, 1, ...)).
.p3_dummies <- function(configs, deps) {
  cols <- list(); idx <- list()
  for (d in seq_along(deps)) {
    di <- deps[[d]]
    pat <- as.integer(configs[, di, drop = FALSE] %*% 2^(seq_along(di) - 1))
    for (c in 0:(2^length(di) - 1)) {
      cols[[length(cols) + 1L]] <- as.numeric(pat == c)
      idx[[length(idx) + 1L]] <- c(d = d, c = c)
    }
  }
  X <- do.call(cbind, cols)
  attr(X, "index") <- do.call(rbind, idx)
  X
}

## Coefficients alpha_{d,c} = f_d(c), read off the landscape's contribution
## table (any configuration carrying pattern c in the neighborhood of d).
.p3_alpha <- function(nk, X) {
  ix <- attr(X, "index")
  vapply(seq_len(nrow(ix)), function(k) {
    rows <- which(X[, k] == 1)
    nk$contributions[rows[1], ix[k, "d"]]
  }, numeric(1))
}

## Least-squares residual (max abs) of y on the columns of X (with intercept).
.p3_ls_resid <- function(X, y) {
  fit <- stats::lm.fit(cbind(1, X), y)
  max(abs(fit$residuals))
}

## Pairwise (order <= 2) and linear (order <= 1) designs in the own row.
.p3_linear <- function(configs) configs * 1
.p3_pairwise <- function(configs) {
  N <- ncol(configs)
  pr <- utils::combn(N, 2)
  cbind(configs, apply(pr, 2, function(p) configs[, p[1]] * configs[, p[2]]))
}

.p3_grid <- list(
  list(N = 6L, K = 0L, model = "adjacent"),
  list(N = 6L, K = 1L, model = "random"),
  list(N = 6L, K = 2L, model = "random"),
  list(N = 7L, K = 3L, model = "block"),
  list(N = 8L, K = 3L, model = "adjacent"),
  list(N = 6L, K = 5L, model = "random")
)

## ---- 1. strategy (a): exact reconstruction by configuration dummies --------

test_that("Property 3(a): N * 2^(K+1) configuration dummies reproduce W exactly", {
  for (g in .p3_grid) {
    nk <- nk_landscape(g$N, g$K, model = g$model, seed = 300 + g$N + g$K)
    X <- .p3_dummies(nk$configs, nk$dependencies)
    alpha <- .p3_alpha(nk, X)
    info <- sprintf("N=%d K=%d %s", g$N, g$K, g$model)

    expect_equal(ncol(X), g$N * 2^(g$K + 1), info = info)
    expect_true(all(rowSums(X) == g$N), info = info)   # one dummy per d
    recon <- as.numeric(X %*% alpha)
    expect_lt(max(abs(recon - g$N * nk$fitness)), 1e-12)
  }
})

## ---- 2. the package's SAOM-NK evaluation function ------------------------

test_that("Property 3: the package's SAOM-NK nk term equals the dummy reconstruction", {
  for (g in .p3_grid[c(2, 3, 5)]) {
    nk <- nk_landscape(g$N, g$K, model = g$model, seed = 300 + g$N + g$K)
    X <- .p3_dummies(nk$configs, nk$dependencies)
    recon_W <- as.numeric(X %*% .p3_alpha(nk, X)) / g$N

    spec <- lean_spec(nk)                    # M = 1, theta_nk = 1
    expect_equal(unname(spec$theta[["nk"]]), 1)
    ns <- .lean_num_spec(spec, digits = NULL, exact = FALSE)
    u <- vapply(seq_len(nrow(nk$configs)), function(r)
      .lean_utility(ns, nk$configs[r, , drop = FALSE], 1L), numeric(1))
    expect_lt(max(abs(u - recon_W)), 1e-12)
    expect_lt(max(abs(u - nk$fitness)), 1e-12)
  }
})

## ---- 3. negative control: wrong neighborhoods ------------------------------

test_that("Property 3 control: dummies on a wrong neighborhood do not reproduce W", {
  for (g in .p3_grid[-1]) {                  # K >= 1
    nk <- nk_landscape(g$N, g$K, model = g$model, seed = 300 + g$N + g$K)
    wrong <- lapply(seq_len(g$N), function(d) {
      partners <- setdiff(nk$dependencies[[d]], d)
      c(d, partners[-1])                     # drop one partner
    })
    X <- .p3_dummies(nk$configs, wrong)
    expect_gt(.p3_ls_resid(X, g$N * nk$fitness), 1e-3,
              label = sprintf("residual, N=%d K=%d %s", g$N, g$K, g$model))
  }
})

## ---- 4-5. what the other parameterizations reach --------------------------

test_that("Property 3(b) as written (linear in own row) reproduces only K = 0", {
  for (g in .p3_grid) {
    nk <- nk_landscape(g$N, g$K, model = g$model, seed = 300 + g$N + g$K)
    r <- .p3_ls_resid(.p3_linear(nk$configs), g$N * nk$fitness)
    lab <- sprintf("linear residual, N=%d K=%d %s", g$N, g$K, g$model)
    if (g$K == 0L) expect_lt(r, 1e-10, label = lab)
    else expect_gt(r, 1e-3, label = lab)
  }
})

test_that("Pairwise covariate effects (XWX-type) reproduce K <= 1 but not K >= 2", {
  for (g in .p3_grid) {
    nk <- nk_landscape(g$N, g$K, model = g$model, seed = 300 + g$N + g$K)
    r <- .p3_ls_resid(.p3_pairwise(nk$configs), g$N * nk$fitness)
    lab <- sprintf("pairwise residual, N=%d K=%d %s", g$N, g$K, g$model)
    if (g$K <= 1L) expect_lt(r, 1e-10, label = lab)
    else expect_gt(r, 1e-3, label = lab)
  }
})
