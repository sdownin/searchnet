###############################################################################
## k-dimensions.R
##
## How each effect relates to the four {K} dimensions (definitions:
## inst/rosetta/K_DIMENSIONS.md, the single statement of the rules).
##
##   searchnet_effect_dimensions()  the table for the package's effects
##   searchnet_classify_effect()    classify any statistic, including a user's
##
## READS: derived from the change statistic Delta s_ij by a walk-dependency
## test: each input of Delta s_ij is perturbed with the others held fixed, and
## the first input it depends on names the dimension. MOVES: derived from the
## target statistic sum_i s_i(B) by an exact identity with a {K} moment, fitted
## over random networks; with no exact identity, by which margin the target is
## invariant to. Both are computed from the engine's own statistic,
## get_struct_mod_stats_mat_from_bi_mat() in R/saomnk-base.R.
###############################################################################

## Inputs the dependency test perturbs, in a fixed order.
.K_INPUTS <- c("own_row", "others_j", "others_other", "own_attr", "others_attr",
               "target_attr", "other_comp_attr", "tie_attr", "actor_pair", "W")

## Reads rules: map a dependency profile (a named logical vector over the
## inputs of a change statistic) to the {K} dimension read, as a symbol.
## "walk" is the package rule (inst/rosetta/K_DIMENSIONS.md); "degree" is the
## documented alternative that reads a count of other holders as K_CA.
.k_rule_walk <- function(d) {
  if (d[["others_attr"]]) return("K_CA")
  if (d[["others_j"]] || d[["others_other"]] || d[["actor_pair"]]) return("K_AA")
  if (d[["W"]]) return("K_CC")
  if (d[["own_row"]] || d[["own_attr"]] || d[["target_attr"]] ||
      d[["other_comp_attr"]] || d[["tie_attr"]]) return("K_AC")
  ## Fall-through: a change statistic that depends on nothing (an intercept)
  ## reads only the actor's own move, K_AC.
  "K_AC"
}

.k_rule_degree <- function(d) {
  if (d[["others_j"]]) return(if (d[["others_other"]]) "K_AA" else "K_CA")
  if (d[["others_other"]] || d[["actor_pair"]]) return("K_AA")
  if (d[["W"]]) return("K_CC")
  if (d[["own_row"]]) return("K_AC")
  if (d[["target_attr"]]) return("K_CA")
  if (d[["own_attr"]] || d[["other_comp_attr"]] || d[["tie_attr"]]) return("K_AC")
  if (d[["others_attr"]]) return("K_CA")
  "none"
}

.k_rule <- function(rule) {
  if (is.function(rule)) return(rule)
  switch(match.arg(rule, c("walk", "degree")), walk = .k_rule_walk, degree = .k_rule_degree)
}

## Probe covariates: actor attribute v (M), component attribute c (N), tie
## attribute X (M x N), actor-pair covariate Z (M x M), component-pair
## covariate W (N x N).
.k_probe_cov <- function(M, N) {
  list(v = stats::rnorm(M), c = stats::rnorm(N),
       X = matrix(stats::rnorm(M * N), M, N),
       Z = { z <- matrix(stats::rnorm(M * M), M, M); z <- (z + t(z)) / 2; diag(z) <- 0; z },
       W = matrix(stats::rnorm(N * N), N, N))
}

## Engine statistic: the M-vector s_i(B) of one engine effect, without an
## RSiena model (the method only reads self$M, self$N, the effects table and
## get_cov_data()).
.k_engine_stat <- function(effect) {
  force(effect)
  f <- SaomNkRSienaBiEnv_base$public_methods$get_struct_mod_stats_mat_from_bi_mat
  function(B, cov) {
    covar <- switch(effect, egoX = cov$v, altX = cov$c, outActX = cov$c, inPopX = cov$v,
                    totInDist2 = cov$v, simEgoInDist2 = cov$v, X = cov$X, XWX = cov$W, NULL)
    theta_df <- data.frame(shortName = effect, initialValue = 1, effect_level = effect,
                           stringsAsFactors = FALSE)
    fake_self <- list(M = nrow(B), N = ncol(B),
                      get_bipartite_effects_theta_df = function() theta_df,
                      get_cov_data = function(item) covar)
    g <- f
    environment(g) <- list2env(list(self = fake_self), parent = environment(f))
    c(g(B))
  }
}

## Statistics the engine does not compute, from their definitions.
.k_local_stat <- function(effect) {
  switch(effect,
    outIso = function(B, cov) as.numeric(rowSums(B) == 0),
    outTrunc = function(B, cov) pmin(rowSums(B), 2),
    coholder_similarity = function(B, cov) {
      s <- saomnk_coholder_similarity(B, cov$v)
      s[is.na(s)] <- 0
      rowSums(B * s)
    },
    NULL)
}

## Change statistic of actor i toggling component j, from a statistic s.
.k_delta <- function(stat, B, i, j, cov) {
  B1 <- B; B1[i, j] <- 1
  B0 <- B; B0[i, j] <- 0
  stat(B1, cov)[i] - stat(B0, cov)[i]
}

## Zero-sum perturbation of x on the positions idx (keeps the mean of x).
.k_zero_sum <- function(x, idx) {
  if (length(idx) < 2) return(x)
  e <- stats::rnorm(length(idx)); x[idx] <- x[idx] + e - mean(e); x
}

## Dependency profile of a change statistic: which inputs it depends on.
## `change(B, i, j, cov)` returns Delta s_ij.
.k_dependency <- function(change, n_states = 12L, n_perturb = 3L, M = 6L, N = 7L, tol = 1e-9) {
  dep <- stats::setNames(rep(FALSE, length(.K_INPUTS)), .K_INPUTS)
  for (r in seq_len(n_states)) {
    B <- matrix(stats::rbinom(M * N, 1, stats::runif(1, 0.3, 0.6)), M, N)
    storage.mode(B) <- "double"
    cov <- .k_probe_cov(M, N)
    i <- sample.int(M, 1); j <- sample.int(N, 1)
    d0 <- change(B, i, j, cov)
    for (k in seq_len(n_perturb)) {
      for (inp in .K_INPUTS[!dep]) {
        B2 <- B; c2 <- cov
        switch(inp,
          own_row = { B2[i, -j] <- stats::rbinom(N - 1, 1, 0.5) },
          others_j = { B2[-i, j] <- stats::rbinom(M - 1, 1, 0.5) },
          others_other = { B2[-i, -j] <- stats::rbinom((M - 1) * (N - 1), 1, 0.5) },
          own_attr = { c2$v[i] <- c2$v[i] + stats::rnorm(1) },
          others_attr = { c2$v <- .k_zero_sum(c2$v, setdiff(seq_len(M), i)) },
          target_attr = { c2$c[j] <- c2$c[j] + stats::rnorm(1) },
          other_comp_attr = { c2$c <- .k_zero_sum(c2$c, setdiff(seq_len(N), j)) },
          tie_attr = { c2$X[i, j] <- c2$X[i, j] + stats::rnorm(1) },
          actor_pair = { z <- c2$Z; z[i, -i] <- z[-i, i] <- z[i, -i] + stats::rnorm(M - 1); c2$Z <- z },
          W = { c2$W <- matrix(stats::rnorm(N * N), N, N) })
        if (abs(change(B2, i, j, c2) - d0) > tol) dep[[inp]] <- TRUE
      }
    }
  }
  dep
}

## Candidate {K} moments of a state for the outcome identity, given the probe
## covariates (centered as RSiena centers them).
.k_moments <- function(B, cov) {
  vt <- cov$v - mean(cov$v); ct <- cov$c - mean(cov$c); Xt <- cov$X - mean(cov$X)
  A <- B %*% t(B); C <- t(B) %*% B
  offA <- A; diag(offA) <- 0; offC <- C; diag(offC) <- 0
  Wo <- cov$W; diag(Wo) <- 0
  c(ties = sum(B),
    K_AA = sum(offA),
    K_CC = sum(offC),
    four_cycles = sum(choose(A[upper.tri(A)], 2)),
    K_AC_attr = sum(vt * rowSums(B)),
    K_CA_attr = sum(ct * colSums(B)),
    K_AA_attr = sum(offA * rep(vt, each = nrow(B))),
    K_AA_pair = sum(offA * cov$Z),
    K_CC_attr = sum(ct * rowSums(offC)),
    K_CC_W = sum(t(Wo) * C),
    ties_attr = sum(Xt * B))
}

## Display names of the four dimensions (classes.yaml k_channel_label).
.K_DIM_NAME <- c(K_AC = "Expansiveness", K_CA = "Popularity", K_AA = "Sociality",
                 K_CC = "Epistasis")

.K_MOMENT_LABEL <- c(ties = "total ties",
                     K_AA = "Sociality strength",
                     K_CC = "Epistasis strength, unvalued",
                     four_cycles = "Sociality and Epistasis (four-cycle count)",
                     K_AC_attr = "Expansiveness, attribute-weighted",
                     K_CA_attr = "Popularity, attribute-weighted",
                     K_AA_attr = "Sociality strength, attribute-weighted",
                     K_AA_pair = "Sociality strength, valued by the actor-pair covariate",
                     K_CC_attr = "Epistasis strength, attribute-weighted",
                     K_CC_W = "Epistasis strength valued by W",
                     ties_attr = "total ties, tie-attribute-weighted")

## Moves: the dimension of a target statistic `target(B, cov)` (a scalar).
## 1. An exact linear identity with the candidate moments over random states
##    (covariates fixed): the moments with nonzero coefficients name it, the
##    total-ties term reported as ", plus total ties".
## 2. Otherwise, invariance: to resampling every row's ties within the row
##    (a moment of Expansiveness), or every column's (of Popularity).
## 3. Otherwise, dependence on actor attributes reads as similarity-weighted
##    Sociality; anything else is "none (candidate-centered)" when the caller says
##    the statistic is centered on the decision's candidates, else "none".
.k_outcome <- function(target, n_states = 40L, M = 6L, N = 7L, candidate_centered = FALSE,
                       tol = 1e-8) {
  cov <- .k_probe_cov(M, N)
  y <- numeric(n_states); X <- NULL
  for (r in seq_len(n_states)) {
    B <- matrix(stats::rbinom(M * N, 1, stats::runif(1, 0.2, 0.8)), M, N)
    storage.mode(B) <- "double"
    y[r] <- target(B, cov)
    X <- rbind(X, .k_moments(B, cov))
  }
  if (all(abs(y - y[1]) < tol)) return(list(moves = "none", identity = "constant"))
  fit <- stats::lm.fit(cbind(1, X), y)
  if (max(abs(fit$residuals)) < tol * max(1, max(abs(y)))) {
    b <- fit$coefficients[-1]
    b[is.na(b)] <- 0
    used <- names(b)[abs(b) > 1e-6]
    if (abs(fit$coefficients[1]) < 1e-6 && length(used)) {
      main <- setdiff(used, "ties")
      ## A weighted moment absorbs its unweighted base (a raw covariate is
      ## its centered form plus its mean times the base).
      base_of <- c(K_AC_attr = "ties", K_CA_attr = "ties", ties_attr = "ties",
                   K_AA_attr = "K_AA", K_AA_pair = "K_AA", K_CC_attr = "K_CC", K_CC_W = "K_CC")
      main <- setdiff(main, base_of[intersect(main, names(base_of))])
      if (any(names(base_of)[base_of == "ties"] %in% main)) used <- setdiff(used, "ties")
      lab <- if (length(main)) paste(.K_MOMENT_LABEL[main], collapse = " + ") else "total ties"
      if (length(main) && "ties" %in% used) lab <- paste0(lab, ", plus total ties")
      idn <- paste(sprintf("%s * %s", format(signif(b[used], 6), trim = TRUE), used),
                   collapse = " + ")
      return(list(moves = lab, identity = idn))
    }
  }
  inv <- function(how) {
    for (r in 1:10) {
      B <- matrix(stats::rbinom(M * N, 1, 0.5), M, N); storage.mode(B) <- "double"
      B2 <- if (how == "row") t(apply(B, 1, sample)) else apply(B, 2, sample)
      if (abs(target(B, cov) - target(B2, cov)) > tol) return(FALSE)
    }
    TRUE
  }
  if (inv("row")) return(list(moves = "Expansiveness, nonlinear moment", identity = "row-resampling invariant"))
  if (inv("col")) return(list(moves = "Popularity, nonlinear moment", identity = "column-resampling invariant"))
  B <- matrix(stats::rbinom(M * N, 1, 0.5), M, N); storage.mode(B) <- "double"
  c2 <- cov; c2$v <- stats::rnorm(M)
  if (!candidate_centered && abs(target(B, cov) - target(B, c2)) > tol)
    return(list(moves = "Sociality strength, similarity-weighted", identity = "depends on holders' attributes"))
  list(moves = if (candidate_centered) "none (candidate-centered)" else "none", identity = "")
}

## Run f with a local seed, restoring the caller's random number stream.
.k_with_seed <- function(seed, f) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (is.null(old)) { if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv()) }
    else assign(".Random.seed", old, envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  f()
}

#' Classify a statistic by the {K} dimensions it reads and moves
#'
#' Derives the two fields of any actor statistic `s_i(B)`, so that a
#' researcher's own effect classifies itself by the same rules as the
#' package's (stated once in `inst/rosetta/K_DIMENSIONS.md`; see
#' [searchnet_effect_dimensions()]). `reads` is the \{K\} dimension the
#' change statistic `Delta s_ij = s_i(b_ij = 1) - s_i(b_ij = 0)` depends on,
#' by a walk-dependency test on random networks; `moves` is the dimension of
#' the target statistic `sum_i s_i(B)`, by an exact identity with a \{K\}
#' moment (projection moments are strengths, sums over partners of shared
#' components).
#'
#' A statistic is a function `function(B, cov)` of the actor-by-component
#' 0/1 matrix `B` returning the M-vector `s_i(B)`. `cov` is a list of probe
#' covariates: `v` (actor attribute, length M), `c` (component attribute,
#' length N), `X` (tie attribute, M x N), `Z` (actor-pair covariate, M x M),
#' `W` (component-pair covariate, N x N); a statistic reads whichever it
#' uses. Alternatively give the change statistic `change(B, i, j, cov)` and
#' the target `target(B, cov)` directly.
#'
#' @param stat Statistic function `function(B, cov)` returning `s_i(B)`, or
#'   `NULL` when `change` and `target` are given.
#' @param change Optional change-statistic function `function(B, i, j, cov)`.
#' @param target Optional target-statistic function `function(B, cov)`.
#' @param rule Reads rule: `"walk"` (default, the package rule) or
#'   `"degree"` (documented alternative), or a function mapping a named
#'   logical dependency profile to a dimension symbol (`"K_AC"`, ...).
#' @param candidate_centered Logical; the statistic is centered on the mean
#'   over the decision's candidates, so a target with no identity moves
#'   `"none (candidate-centered)"`.
#' @param seed Seed for the random networks (the caller's random stream is
#'   restored afterwards).
#' @return A list: `reads` (dimension name), `moves`, `depends_on` (the
#'   inputs the change statistic depends on), `identity` (the fitted target
#'   identity, as coefficients on named moments).
#' @seealso [searchnet_effect_dimensions()]
#' @export
#' @examples
#' ## Toy statistic: ties to components weighted by how many OTHER actors hold
#' ## them, s_i = sum_j b_ij (b_+j - 1).
#' toy <- function(B, cov) c(B %*% (colSums(B) - 1))
#' searchnet_classify_effect(toy)[c("reads", "moves")]
#'
#' ## Toy statistic with an actor attribute: ties weighted by the attribute of
#' ## the other holders, s_i = sum_j b_ij sum_{h != i} b_hj v_h.
#' toy2 <- function(B, cov) c(B %*% c(cov$v %*% B)) - rowSums(B) * cov$v
#' searchnet_classify_effect(toy2)$reads
searchnet_classify_effect <- function(stat = NULL, change = NULL, target = NULL,
                                      rule = "walk", candidate_centered = FALSE, seed = 1L) {
  if (is.null(change)) {
    if (!is.function(stat)) stop("give `stat`, or both `change` and `target`", call. = FALSE)
    change <- function(B, i, j, cov) .k_delta(stat, B, i, j, cov)
  }
  if (is.null(target)) {
    if (!is.function(stat)) stop("give `stat`, or both `change` and `target`", call. = FALSE)
    target <- function(B, cov) sum(stat(B, cov))
  }
  rf <- .k_rule(rule)
  .k_with_seed(seed, function() {
    dep <- .k_dependency(change)
    out <- .k_outcome(target, candidate_centered = candidate_centered)
    sym <- rf(dep)
    list(reads = if (sym %in% names(.K_DIM_NAME)) unname(.K_DIM_NAME[sym]) else sym,
         moves = out$moves,
         depends_on = paste(names(dep)[dep], collapse = ", "), identity = out$identity)
  })
}

## The package's effects: class, how the statistic is obtained, and the
## formula strings (declared text; reads and moves are derived).
.k_effect_rows <- function() {
  r <- function(effect, class, source, change_statistic, target_statistic, notes = "")
    data.frame(effect = effect, class = class, source = source,
               change_statistic = change_statistic, target_statistic = target_statistic,
               notes = notes, stringsAsFactors = FALSE)
  rbind(
    r("density", "scope", "engine", "1 (constant: an intercept)",
      "sum_i b_i+ = sum_j b_+j (total ties)",
      "The change statistic is constant; rule walk falls through to the actor's own move (Expansiveness). Moves: the first moment shared by Expansiveness and Popularity."),
    r("outAct", "scope", "engine", "2 k_i + 1",
      "sum_i b_i+^2 = sum_{j != l} (B'B)_jl + total ties",
      "Reads and moves differ. Identity: sum_i b_i+ (b_i+ - 1) = sum_{j != l} (B'B)_jl, the total Epistasis strength."),
    r("outActSqrt", "scope", "engine", "(k_i + 1)^(3/2) - k_i^(3/2)",
      "sum_i b_i+^(3/2)",
      "No exact identity with Epistasis strength: the target is a 3/2 moment of Expansiveness."),
    r("inPop", "crowding", "engine", "n_j + 1",
      "sum_j b_+j^2 = sum_{i != h} (BB')_ih + total ties",
      paste("Tie convention: n_j + 1 is both component j's degree and the overlap the tie creates;",
            "the rule reads it as Sociality, the deciding actor's view. b_+j counts actor i (RSiena 1.5.0),",
            "so the target is total Sociality strength plus total ties. theta < 0 reads as crowding, theta > 0 as herding.")),
    r("inPopSqrt", "crowding", "engine", "sqrt(n_j + 1)",
      "sum_j b_+j^(3/2)",
      "Reads and moves differ. No exact identity with Sociality strength: the target is a 3/2 moment of Popularity."),
    r("XWX", "complementarity", "engine", "sum_{h != j} b_ih (w_hj + w_jh)",
      "sum_{j != h} w_hj (B'B)_jh",
      "W is the influence matrix (a component-pair covariate); the diagonal w_jj never enters."),
    r("nk", "complementarity", "none", "F(b_i with j on; W) - F(b_i with j off; W)",
      "sum_i F(b_i; W)",
      "Landscape fitness, not an engine statistic; its fields are those of XWX, its quadratic form (not derived)."),
    r("cycle4", "contact", "engine", "(1/2) sum_{h != i} b_hj o_ih^(-j)",
      "sum_{i < h} choose(o_ih, 2) = sum_{j < l} choose((B'B)_jl, 2)",
      "Reads and moves differ. Mode-symmetric four-cycle count; the engine's ego statistic carries 1/2 so that it sums to the count."),
    r("egoX", "covariate", "engine", "v_i - vbar",
      "sum_i b_i+ (v_i - vbar)", "Actor attribute."),
    r("altX", "covariate", "engine", "c_j - cbar",
      "sum_j b_+j (c_j - cbar)",
      "Reads and moves differ. Component attribute: read as an attribute of the actor's own move (Expansiveness); moves the attribute-weighted component degree."),
    r("X", "covariate", "engine", "w_ij - wbar",
      "sum_{i,j} b_ij (w_ij - wbar)", "Tie (actor x component) covariate."),
    r("outActX", "other", "engine", "sum_{l != j} b_il c~_l + (k_i + 1) c~_j",
      "sum_i b_i+ sum_j b_ij c~_j", "c~: the component covariate centered."),
    r("inPopX", "other", "engine", "sum_{h != i} b_hj v~_h + v~_i",
      "sum_j b_+j sum_h b_hj v~_h = sum_h v~_h (b_h+ + sum_{i != h} (BB')_ih)",
      paste("v~: the actor covariate centered; ego counted among the holders (RSiena 1.5.0, internal",
            "parameter 1). Fixed 2026-10-08: the column had recycled the N-vector of holder sums",
            "column-major. RSiena 1.5.0's two-mode siena07 target for inPopX is not deterministic",
            "(it sometimes omits the last component's term); the engine column equals its full form.")),
    r("totInDist2", "other", "engine", "sum_{h != i} b_hj v~_h",
      "sum_{i != h} (BB')_ih v~_h", "Reads who the other holders are (their attribute)."),
    r("simEgoInDist2", "other", "engine", "1 - |v_i - vbar_j^(-i)| / R - simMean",
      "sum_{i,j} b_ij (1 - |v_i - vbar_j^(-i)| / R - simMean)",
      paste("Reads and moves differ. The simulable route to imitation (listed under no class in classes.yaml).",
            "Nonlinear in the overlap: moves similarity-weighted Sociality, not by an exact identity.")),
    r("coholder_similarity", "imitation", "local", "sim_ij (centered co-holder performance similarity)",
      "sum_{i,j} b_ij sim_ij",
      paste("Reads and moves differ. saomnk_coholder_similarity() is a post hoc statistic, not simulated;",
            "classified with s_i = sum_j b_ij sim_ij, performance as the actor attribute.")),
    r("outIso", "other", "local", "-1[k_i = 0]", "#{i : b_i+ = 0}",
      "RSiena two-mode effect, not an engine statistic; derived from its definition."),
    r("outTrunc", "other", "local", "1[k_i < c]", "sum_i min(b_i+, c)",
      "RSiena two-mode effect (c = 2 here), not an engine statistic; derived from its definition."),
    r("inAct", "other", "absent", "not defined in two-mode form", "not defined in two-mode form",
      "RSiena 1.5.0 offers no two-mode inAct (components send no ties): a non-implementation, not a null.")
  )
}

.k_cache <- new.env(parent = emptyenv())

.k_reads_phrase <- c(Expansiveness = "the actor's own portfolio or attributes",
                     Popularity = "who the other holders are",
                     Sociality = "other actors' holdings (the overlap a move creates)",
                     Epistasis = "components coupled through W")

#' The {K} dimensions each effect reads and moves
#'
#' Classifies every effect of the package by the four \{K\} dimensions
#' (Expansiveness K_AC, Popularity K_CA, Sociality K_AA, Epistasis K_CC),
#' derived from the statistic the engine computes. The definitions (the four
#' dimensions, degree and strength, the two rules, the tie convention, the
#' two limits) are stated once, in `inst/rosetta/K_DIMENSIONS.md`
#' (`system.file("rosetta", "K_DIMENSIONS.md", package = "searchnet")`).
#'
#' \describe{
#'   \item{reads}{the dimension the *change statistic* `Delta s_ij` (the
#'     change in actor `i`'s statistic when it toggles component `j`)
#'     depends on, by the walk-dependency rule: other actors' attributes ->
#'     Popularity; else other actors' holdings -> Sociality; else W ->
#'     Epistasis; else the actor's own row or attributes -> Expansiveness.}
#'   \item{moves}{the dimension of the *target statistic* `sum_i s_i(B)`, by
#'     an exact identity fitted on random networks: what estimation matches
#'     and the coefficient moves first. Projection moments are strengths:
#'     Sociality strength `sum_{h != i} (BB')_ih`, Epistasis strength
#'     `sum_{l != j} (B'B)_jl`; the package's K_AA and K_CC degrees are the
#'     partner counts of the same projections.}
#' }
#'
#' The identities link them: `sum_j b_+j (b_+j - 1) = sum_{i != h} (BB')_ih`
#' (total Sociality strength) and `sum_i b_i+ (b_i+ - 1) = sum_{j != l}
#' (B'B)_jl` (total Epistasis strength). Two limits apply: `moves` is the
#' moment moved first, not the full equilibrium response; and the sign of
#' `theta` sets the reading (for `inPop`, `theta < 0` reads as crowding,
#' `theta > 0` as herding), never the dimension.
#'
#' Effects without a statistic to derive from (`nk`) carry the fields of
#' their quadratic form, and effects not offered in two-mode form (`inAct`)
#' are `"not classified"`
#' with the reason in `notes`. The same derivation classifies a user's own
#' statistic: [searchnet_classify_effect()]. The table is also shipped as
#' `inst/rosetta/effect_dimensions.csv`.
#'
#' @param effects Character vector of effect names, or `NULL` (default) for
#'   every effect. A name not in the table is returned as `"not classified"`.
#' @param rule Reads rule: `"walk"` (default) or `"degree"` (documented
#'   alternative; see [searchnet_classify_effect()]), or a function.
#' @param custom Optional named list of user statistics, each a statistic
#'   function `function(B, cov)` or a list with `change` and `target`; they
#'   are classified by [searchnet_classify_effect()] and appended.
#' @return A data.frame with one row per effect: `effect`, `class` (the class
#'   of `inst/rosetta/classes.yaml` that carries it, or `"other"`), `reads`,
#'   `moves`, `change_statistic`, `target_statistic` (plain formula strings),
#'   `reading` (one short phrase per field), `notes`, `depends_on` (the
#'   inputs the change statistic depends on).
#' @seealso [searchnet_classify_effect()], [rosetta_classes()]
#' @export
#' @examples
#' \donttest{
#' searchnet_effect_dimensions(c("inPop", "outAct", "altX", "cycle4"))[,
#'   c("effect", "reads", "moves")]
#' }
searchnet_effect_dimensions <- function(effects = NULL, rule = "walk", custom = NULL) {
  key <- if (is.character(rule)) rule else "custom_rule"
  tab <- if (is.character(rule)) .k_cache[[key]] else NULL
  if (is.null(tab)) {
    rows <- .k_effect_rows()
    rows$reads <- rows$moves <- rows$depends_on <- ""
    for (k in seq_len(nrow(rows))) {
      e <- rows$effect[k]
      st <- switch(rows$source[k], engine = .k_engine_stat(e), local = .k_local_stat(e), NULL)
      if (is.null(st)) next
      d <- searchnet_classify_effect(st, rule = rule)
      rows$reads[k] <- d$reads
      rows$moves[k] <- d$moves
      rows$depends_on[k] <- d$depends_on
    }
    nk <- rows$effect == "nk"
    xwx <- rows$effect == "XWX"
    rows$reads[nk] <- rows$reads[xwx]
    rows$moves[nk] <- rows$moves[xwx]
    na <- !nzchar(rows$reads)
    rows$reads[na] <- rows$moves[na] <- "not classified"
    tab <- rows
    if (is.character(rule)) .k_cache[[key]] <- tab
  }
  if (!is.null(custom)) {
    stopifnot(is.list(custom), !is.null(names(custom)), all(nzchar(names(custom))))
    add <- lapply(names(custom), function(nm) {
      x <- custom[[nm]]
      d <- if (is.function(x)) searchnet_classify_effect(x, rule = rule)
           else searchnet_classify_effect(change = x$change, target = x$target, rule = rule)
      data.frame(effect = nm, class = "other", source = "custom",
                 change_statistic = "user-supplied", target_statistic = "user-supplied",
                 notes = paste("identity:", d$identity), reads = d$reads,
                 moves = d$moves, depends_on = d$depends_on, stringsAsFactors = FALSE)
    })
    tab <- rbind(tab[, names(add[[1]])], do.call(rbind, add))
  }
  ph <- .k_reads_phrase[tab$reads]
  tab$reading <- ifelse(is.na(ph), ifelse(tab$reads == "not classified", "not classified",
                                          paste0("reads: ", tab$reads, " | moves: ", tab$moves)),
                        paste0("reads: ", ph, " | moves: ", tab$moves))
  out <- tab[, c("effect", "class", "reads", "moves", "change_statistic",
                 "target_statistic", "reading", "notes", "depends_on")]
  if (!is.null(effects)) {
    stopifnot(is.character(effects))
    hit <- match(effects, out$effect)
    o2 <- out[hit, , drop = FALSE]
    miss <- which(is.na(hit))
    if (length(miss)) {
      o2$effect[miss] <- effects[miss]
      o2$class[miss] <- "other"
      o2$reads[miss] <- o2$moves[miss] <- "not classified"
      o2$change_statistic[miss] <- o2$target_statistic[miss] <- o2$depends_on[miss] <- ""
      o2$reading[miss] <- "not classified"
      o2$notes[miss] <- "Not in the classification table."
    }
    out <- o2
  }
  rownames(out) <- NULL
  out
}
