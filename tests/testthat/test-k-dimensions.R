###############################################################################
## test-k-dimensions.R
##
## How each effect relates to the four {K} dimensions (searchnet_effect_dimensions(),
## searchnet_classify_effect(); definitions in inst/rosetta/K_DIMENSIONS.md),
## checked against the engine's own statistics (get_struct_mod_stats_mat_from_bi_mat
## in R/saomnk-base.R), not against a reimplementation:
##
##   1. the coupling identities I1-I3, exactly, for STRENGTHS, on 50 random
##      bipartite matrices of varied sizes and densities; and the package's
##      K_AA / K_CC degrees are distinct-partner counts, for which I2/I3 fail;
##   2. the change- and target-statistic formula strings reproduce the engine;
##   3. the DERIVATION (rule "walk" on the change statistic, identity fit on the
##      target) yields the agreed map, its dependency profiles follow from the
##      formulas, the rule is swappable, and a user's own statistic classifies
##      itself;
##   4. every class of classes.yaml carries reads and moves, consistent with the
##      derivation, and effect_dimensions.csv equals a fresh derivation.
###############################################################################

## Call the package's statistic function without building an RSiena model
## (the same device as test-potential-cycles.R). `cov` maps an effect name to
## its covariate.
.kc_stats <- function(B, effects, cov = list()) {
  f <- SaomNkRSienaBiEnv_base$public_methods$get_struct_mod_stats_mat_from_bi_mat
  theta_df <- data.frame(shortName = effects, initialValue = 1,
                         effect_level = effects, stringsAsFactors = FALSE)
  fake_self <- list(
    M = nrow(B), N = ncol(B),
    get_bipartite_effects_theta_df = function() theta_df,
    get_cov_data = function(item) cov[[item$effect]]
  )
  environment(f) <- list2env(list(self = fake_self), parent = globalenv())
  out <- f(B)
  colnames(out) <- effects
  out
}

.kc_random_B <- function(r) {
  M <- sample(3:9, 1); N <- sample(3:10, 1); p <- stats::runif(1, 0.1, 0.9)
  B <- matrix(stats::rbinom(M * N, 1, p), M, N)
  storage.mode(B) <- "double"
  B
}

## Engine change statistic of actor i toggling component j: s_i(b_ij = 1) -
## s_i(b_ij = 0), every other tie held fixed.
.kc_delta <- function(B, i, j, eff, cov = list()) {
  B1 <- B; B1[i, j] <- 1
  B0 <- B; B0[i, j] <- 0
  .kc_stats(B1, eff, cov)[i, 1] - .kc_stats(B0, eff, cov)[i, 1]
}

test_that("the coupling identities hold exactly for strengths on the engine's statistics", {
  set.seed(20261008)
  for (r in 1:50) {
    B <- .kc_random_B(r)
    st <- .kc_stats(B, c("density", "outAct", "inPop", "cycle4"))
    T_dens <- sum(st[, "density"]); T_out <- sum(st[, "outAct"])
    T_pop <- sum(st[, "inPop"]); T_c4 <- sum(st[, "cycle4"])
    BBt <- B %*% t(B); BtB <- t(B) %*% B
    off <- function(A) sum(A) - sum(diag(A))
    n_j <- colSums(B); x_i <- rowSums(B)
    soc_strength <- rowSums(BBt) - diag(BBt)      # Sociality strength of each actor
    epi_strength <- rowSums(BtB) - diag(BtB)      # Epistasis strength of each component
    ## I1: shared first moment
    expect_equal(sum(x_i), sum(n_j)); expect_equal(T_dens, sum(B))
    ## I2: second moment of Popularity = total Sociality strength
    expect_equal(T_pop - T_dens, sum(soc_strength))
    expect_equal(sum(n_j * (n_j - 1)), sum(soc_strength))
    ## I3: second moment of Expansiveness = total Epistasis strength
    expect_equal(T_out - T_dens, sum(epi_strength))
    expect_equal(sum(x_i * (x_i - 1)), sum(epi_strength))
    ## inPop counts ego in b_+j: its target includes the diagonal of BB'
    expect_equal(T_pop, sum(BBt))
    expect_equal(T_out, sum(BtB))
    ## cycle4 target is mode-symmetric
    expect_equal(T_c4, sum(choose(BBt[upper.tri(BBt)], 2)))
    expect_equal(T_c4, sum(choose(BtB[upper.tri(BtB)], 2)))
  }
})

test_that("each change-statistic formula string matches the engine exactly", {
  set.seed(7)
  ## Candidate degree quantities for a toggle (i, j), all excluding the tie:
  ##   k  own degree (K_AC)          n  target's degree (K_CA)
  ##   ov overlap with the target's other holders (K_AA)
  ##   wc held components coupled to j by W (K_CC via W)
  cand <- function(B, i, j, W) {
    Bm <- B; Bm[i, j] <- 0
    O <- Bm %*% t(Bm)
    c(k = sum(Bm[i, ]), n = sum(Bm[-i, j]),
      ov = sum(Bm[-i, j] * O[i, -i]),
      wc = sum(Bm[i, -j] * (W[-j, j] + W[j, -j])))
  }
  declared <- list(
    density    = list(ch = "none", f = function(q) 1),
    outAct     = list(ch = "k",    f = function(q) 2 * q[["k"]] + 1),
    outActSqrt = list(ch = "k",    f = function(q) (q[["k"]] + 1)^1.5 - q[["k"]]^1.5),
    inPop      = list(ch = "n",    f = function(q) q[["n"]] + 1),
    inPopSqrt  = list(ch = "n",    f = function(q) sqrt(q[["n"]] + 1)),
    cycle4     = list(ch = "ov",   f = function(q) 0.5 * q[["ov"]]),
    XWX        = list(ch = "wc",   f = function(q) q[["wc"]])
  )
  for (eff in names(declared)) {
    d <- numeric(0); pred <- numeric(0); Q <- NULL
    for (r in 1:60) {
      B <- .kc_random_B(r)
      W <- matrix(stats::rnorm(ncol(B)^2), ncol(B))
      i <- sample(nrow(B), 1); j <- sample(ncol(B), 1)
      q <- cand(B, i, j, W)
      d <- c(d, .kc_delta(B, i, j, eff, list(XWX = W)))
      pred <- c(pred, declared[[eff]]$f(q))
      Q <- rbind(Q, q)
    }
    ## the formula in change_statistic reproduces the engine exactly
    expect_equal(d, pred, tolerance = 1e-10, info = eff)
  }
})

test_that("covariate change statistics read the declared attribute", {
  set.seed(11)
  for (r in 1:30) {
    B <- .kc_random_B(r)
    M <- nrow(B); N <- ncol(B)
    v <- stats::rnorm(M); cc <- stats::rnorm(N)
    i <- sample(M, 1); j <- sample(N, 1)
    Bm <- B; Bm[i, j] <- 0
    ## egoX: actor-indexed (K_AC side), constant in degree
    expect_equal(.kc_delta(B, i, j, "egoX", list(egoX = v)), v[i] - mean(v))
    ## altX: component-indexed (K_CA side)
    expect_equal(.kc_delta(B, i, j, "altX", list(altX = cc)), cc[j] - mean(cc))
    ## totInDist2: the target's other holders, weighted by their attribute (K_CA)
    expect_equal(.kc_delta(B, i, j, "totInDist2", list(totInDist2 = v)),
                 sum(Bm[-i, j] * (v[-i] - mean(v))))
    ## outActX: own degree (K_AC) and the target's attribute
    k <- sum(Bm[i, ]); ct <- cc - mean(cc)
    expect_equal(.kc_delta(B, i, j, "outActX", list(outActX = cc)),
                 sum(Bm[i, -j] * ct[-j]) + (k + 1) * ct[j])
    ## simEgoInDist2: similarity to the mean of the target's other holders (K_CA)
    R <- max(v) - min(v)
    ps <- 1 - abs(outer(v, v, "-")) / R; diag(ps) <- NA
    vb <- if (sum(Bm[-i, j]) > 0) sum(Bm[-i, j] * v[-i]) / sum(Bm[-i, j]) else mean(v)
    expect_equal(.kc_delta(B, i, j, "simEgoInDist2", list(simEgoInDist2 = v)),
                 1 - abs(v[i] - vb) / R - mean(ps, na.rm = TRUE))
  }
})

test_that("each target statistic equals its declared moment", {
  set.seed(3)
  for (r in 1:30) {
    B <- .kc_random_B(r)
    M <- nrow(B); N <- ncol(B)
    v <- stats::rnorm(M); cc <- stats::rnorm(N); W <- matrix(stats::rnorm(N^2), N)
    cov <- list(XWX = W, egoX = v, altX = cc, totInDist2 = v, outActX = cc)
    effs <- c("density", "outAct", "inPop", "XWX", "cycle4", "egoX", "altX",
              "totInDist2", "outActX", "outActSqrt", "inPopSqrt")
    st <- colSums(.kc_stats(B, effs, cov))
    BBt <- B %*% t(B); BtB <- t(B) %*% B
    xi <- rowSums(B); nj <- colSums(B)
    vt <- v - mean(v); ct <- cc - mean(cc)
    expect_equal(st[["density"]], sum(B))                                  # K_AC = K_CA, first moment
    expect_equal(st[["outAct"]], sum(BtB))                                 # weighted K_CC + ties
    expect_equal(st[["inPop"]], sum(BBt))                                  # weighted K_AA + ties
    expect_equal(st[["XWX"]], sum((W * BtB)[row(W) != col(W)]))            # W-weighted K_CC
    expect_equal(st[["cycle4"]], sum(choose(BBt[upper.tri(BBt)], 2)))      # K_AA closure
    expect_equal(st[["cycle4"]], sum(choose(BtB[upper.tri(BtB)], 2)))      # = K_CC closure
    expect_equal(st[["egoX"]], M * mean(vt * (xi - mean(xi))))             # cov(v, K_AC)
    expect_equal(st[["altX"]], N * mean(ct * (nj - mean(nj))))             # cov(c, K_CA)
    expect_equal(st[["totInDist2"]], sum((BBt * rep(vt, each = M))[row(BBt) != col(BBt)]))
    expect_equal(st[["outActX"]], sum(ct * rowSums(BtB)))                  # c-weighted K_CC
    expect_equal(st[["outActSqrt"]], sum(xi^1.5))                          # concave in K_AC
    expect_equal(st[["inPopSqrt"]], sum(nj^1.5))                           # concave in K_CA
  }
})

test_that("the package's K_AA and K_CC degrees are distinct-partner counts", {
  set.seed(4)
  gap <- 0
  for (r in 1:30) {
    B <- .kc_random_B(r)
    k4 <- .extract_k4_summary(list(bipartite_matrix = B))
    A <- B %*% t(B); diag(A) <- 0
    C <- t(B) %*% B; diag(C) <- 0
    expect_equal(unname(k4$K_AA), unname(rowSums(A > 0)))
    expect_equal(unname(k4$K_CC), unname(rowSums(C > 0)))
    ## the identities are statements about strengths, not partner counts
    gap <- max(gap, abs(sum(colSums(B) * (colSums(B) - 1)) - sum(k4$K_AA)))
  }
  expect_gt(gap, 0)
})

test_that("the derivation reproduces the agreed map (rule walk)", {
  tab <- searchnet_effect_dimensions()
  rd <- stats::setNames(tab$reads, tab$effect)
  mv <- stats::setNames(tab$moves, tab$effect)
  e <- c("density", "outAct", "inPop", "XWX", "cycle4", "egoX", "altX",
         "simEgoInDist2", "coholder_similarity", "totInDist2")
  expect_equal(unname(rd[e]),
               c("Expansiveness", "Expansiveness", "Sociality", "Epistasis", "Sociality",
                 "Expansiveness", "Expansiveness", "Popularity", "Popularity", "Popularity"))
  expect_equal(unname(mv[e]),
               c("total ties", "Epistasis strength, unvalued, plus total ties",
                 "Sociality strength, plus total ties", "Epistasis strength valued by W",
                 "Sociality and Epistasis (four-cycle count)", "Expansiveness, attribute-weighted",
                 "Popularity, attribute-weighted", "Sociality strength, similarity-weighted",
                 "Sociality strength, similarity-weighted", "Sociality strength, attribute-weighted"))
  ## the square-root forms have no exact identity: nonlinear margin moments
  expect_equal(mv[["outActSqrt"]], "Expansiveness, nonlinear moment")
  expect_equal(mv[["inPopSqrt"]], "Popularity, nonlinear moment")
  expect_equal(rd[["inPopSqrt"]], "Sociality")
})

test_that("dependency profiles follow from the change-statistic formulas", {
  tab <- searchnet_effect_dimensions()
  dep <- function(e) strsplit(tab$depends_on[tab$effect == e], ", ")[[1]]
  expect_identical(dep("density"), character(0))
  expect_identical(dep("outAct"), "own_row")                 # 2 k_i + 1
  expect_identical(dep("inPop"), "others_j")                 # n_j + 1
  expect_identical(dep("egoX"), "own_attr")                  # v_i - vbar
  expect_identical(dep("altX"), "target_attr")               # c_j - cbar
  expect_identical(dep("X"), "tie_attr")                     # w_ij - wbar
  expect_setequal(dep("XWX"), c("own_row", "W"))             # sum_h b_ih (w_hj + w_jh)
  expect_setequal(dep("cycle4"), c("own_row", "others_j", "others_other"))
  expect_true(all(c("others_j", "others_attr") %in% dep("totInDist2")))
  expect_true(all(c("others_j", "others_attr") %in% dep("simEgoInDist2")))
})

test_that("the reads rule is one swappable function", {
  prof <- function(...) {
    d <- stats::setNames(rep(FALSE, length(.K_INPUTS)), .K_INPUTS)
    d[c(...)] <- TRUE
    d
  }
  expect_equal(.k_rule_walk(prof("others_attr", "others_j")), "K_CA")
  expect_equal(.k_rule_walk(prof("others_j")), "K_AA")
  expect_equal(.k_rule_walk(prof("actor_pair")), "K_AA")
  expect_equal(.k_rule_walk(prof("W", "own_row")), "K_CC")
  expect_equal(.k_rule_walk(prof("target_attr")), "K_AC")
  expect_equal(.k_rule_walk(prof()), "K_AC")
  ## the documented alternative reads a count of other holders as Popularity
  expect_equal(.k_rule_degree(prof("others_j")), "K_CA")
  expect_equal(.k_rule_degree(prof("target_attr")), "K_CA")
  tA <- searchnet_effect_dimensions(rule = "degree")
  expect_equal(tA$reads[tA$effect == "inPop"], "Popularity")
  expect_equal(tA$reads[tA$effect == "altX"], "Popularity")
  ## a user rule is a function of the profile
  tU <- searchnet_effect_dimensions(c("inPop", "outAct"),
                                    rule = function(d) if (d[["others_j"]]) "X1" else "X2")
  expect_equal(tU$reads, c("X1", "X2"))
  expect_error(searchnet_effect_dimensions(rule = "nonsense"))
})

test_that("a user's own statistic classifies itself", {
  set.seed(99); r0 <- stats::runif(1); set.seed(99)
  ## ties to components weighted by the number of OTHER holders
  toy <- function(B, cov) c(B %*% (colSums(B) - 1))
  d <- searchnet_classify_effect(toy)
  expect_equal(d$reads, "Sociality")
  expect_equal(d$moves, "Sociality strength")
  ## the caller's random stream is untouched
  expect_equal(stats::runif(1), r0)
  ## actor-pair covariate: overlap valued by Z_ih
  pair <- function(B, cov) { A <- B %*% t(B); diag(A) <- 0; rowSums(A * cov$Z) }
  dp <- searchnet_classify_effect(pair)
  expect_equal(dp$reads, "Sociality")
  expect_equal(dp$moves, "Sociality strength, valued by the actor-pair covariate")
  ## component-pair covariate: the actor's own co-held pairs valued by W
  cpair <- function(B, cov) { Wo <- cov$W; diag(Wo) <- 0; rowSums((B %*% Wo) * B) }
  dc <- searchnet_classify_effect(cpair)
  expect_equal(dc$reads, "Epistasis")
  expect_equal(dc$moves, "Epistasis strength valued by W")
  ## a statistic centered over the decision's candidates
  cc <- function(B, cov) {
    sim <- outer(cov$v, colSums(B * cov$v) / pmax(colSums(B), 1), function(a, b) -abs(a - b))
    sim <- sim - rowMeans(sim)
    rowSums(B * sim)
  }
  expect_equal(searchnet_classify_effect(cc, candidate_centered = TRUE)$moves,
               "none (candidate-centered)")
  ## change/target given directly, and through searchnet_effect_dimensions(custom = )
  d2 <- searchnet_classify_effect(change = function(B, i, j, cov) sum(B[-i, j]) + 1,
                                  target = function(B, cov) sum(colSums(B)^2))
  expect_equal(d2$reads, "Sociality")
  expect_equal(d2$moves, "Sociality strength, plus total ties")
  tc <- searchnet_effect_dimensions(custom = list(myToy = toy))
  expect_equal(tc$reads[tc$effect == "myToy"], "Sociality")
  expect_error(searchnet_classify_effect())
})

test_that("inst/rosetta/effect_dimensions.csv equals a fresh derivation", {
  f <- file.path(.rosetta_home(), "effect_dimensions.csv")
  skip_if_not(file.exists(f))
  shipped <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
  shipped[is.na(shipped)] <- ""
  fresh <- searchnet_effect_dimensions()
  expect_equal(shipped, fresh, ignore_attr = TRUE)
  expect_true(file.exists(file.path(.rosetta_home(), "K_DIMENSIONS.md")))
})

test_that("the table is complete, filled, and consistent with classes.yaml", {
  tab <- searchnet_effect_dimensions()
  expect_named(tab, c("effect", "class", "reads", "moves", "change_statistic",
                      "target_statistic", "reading", "notes", "depends_on"))
  expect_false(anyDuplicated(tab$effect) > 0)
  for (col in c("reads", "moves", "change_statistic", "target_statistic", "reading"))
    expect_true(all(nzchar(tab[[col]])), info = col)
  expect_true(all(tab$reads %in% c(unname(.K_DIM_NAME), "not classified")))
  ## the engine's whole effect set is in the table, and classified
  engine <- c("density", "outAct", "outActSqrt", "inPop", "inPopSqrt", "cycle4", "egoX",
              "altX", "outActX", "XWX", "X", "totInDist2", "simEgoInDist2", "inPopX")
  expect_true(all(engine %in% tab$effect))
  expect_true(all(tab$reads[tab$effect %in% engine] != "not classified"))
  ## unknown names come back unclassified, not dropped
  u <- searchnet_effect_dimensions(c("inPop", "noSuchEffect"))
  expect_equal(u$effect, c("inPop", "noSuchEffect"))
  expect_equal(u$reads[2], "not classified")
  expect_true(all(grepl("[^ -~]", unlist(tab)) == FALSE))   # ASCII only
  ## prose says "dimension": "channel" is reserved for mechanisms
  expect_false(any(grepl("channel", unlist(tab), ignore.case = TRUE)))

  skip_if_not_installed("yaml")
  cl <- rosetta_classes()
  ## every class carries both fields; k_channel is a deprecated alias of moves
  expect_true(all(nzchar(cl$reads)) && all(nzchar(cl$moves)))
  expect_equal(cl$k_channel, cl$moves)
  raw <- .rosetta_class_list()
  for (c0 in raw) {
    expect_false(is.null(c0$reads), info = c0$id)
    expect_false(is.null(c0$moves), info = c0$id)
  }
  ## the table covers every effect classes.yaml names, in the same class
  yeff <- unlist(lapply(raw, function(c0) unlist(c0$effects)))
  expect_true(all(yeff %in% tab$effect))
  expect_equal(tab$class[match(yeff, tab$effect)], .rosetta_effect_class(yeff, cl))
  ## a class reads what every one of its effects reads; it moves what one of
  ## its effects moves, or "other" when its effects move different dimensions
  for (k in cl$id) {
    effs <- trimws(strsplit(cl$effects[cl$id == k], ",")[[1]])
    expect_true(all(tab$reads[match(effs, tab$effect)] == .K_DIM_NAME[[cl$reads[cl$id == k]]]),
                info = k)
    mv <- tab$moves[match(effs, tab$effect)]
    if (cl$moves[cl$id == k] != "other")
      expect_true(any(startsWith(mv, .K_DIM_NAME[[cl$moves[cl$id == k]]])), info = k)
    else
      expect_gt(length(unique(sub("[ ,].*", "", mv))), 1L)
  }
})

test_that("rosetta_validate requires reads and moves on every class", {
  skip_if_not_installed("yaml")
  v <- rosetta_validate()
  expect_false(any(v$code == "E9"))
  d <- file.path(tempdir(), paste0("kc_reg_", Sys.getpid()))
  dir.create(file.path(d, "entries"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  y <- readLines(file.path(.rosetta_home(), "classes.yaml"))
  y <- y[!grepl("^    reads:", y)]
  writeLines(y, file.path(d, "classes.yaml"))
  v2 <- rosetta_validate(path = d)
  e9 <- v2[v2$code == "E9", ]
  expect_equal(nrow(e9), length(.rosetta_class_list(d)))
  expect_true(all(grepl("missing field reads", e9$message)))
  expect_false(attr(v2, "ok"))
})

test_that("the engine's inPopX column is sum_j b_ij w_j, and is classified", {
  ## Until 2026-10-08 the column was rowSums(B * w), which recycles the
  ## N-vector of holder sums w down B's columns. It is now
  ## s_i = sum_j b_ij sum_h b_hj v~_h (ego counted, v~ centered as RSiena
  ## centers it); its sum over actors is pinned to RSiena's target in
  ## test-structural-stats-vs-rsiena.R.
  B <- matrix(c(1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1), 3, 4)
  v <- c(0.3, -1.2, 2.0)
  vt <- v - mean(v)
  intended <- c(B %*% c(vt %*% B))
  engine <- unname(.kc_stats(B, "inPopX", list(inPopX = v))[, 1])
  expect_equal(engine, intended)
  expect_false(isTRUE(all.equal(engine, rowSums(B * c(vt %*% B)))))
  ## an uncentered declaration uses the raw covariate
  raw <- structure(v, centered = FALSE)
  expect_equal(unname(.kc_stats(B, "inPopX", list(inPopX = raw))[, 1]),
               c(B %*% c(v %*% B)))
  tab <- searchnet_effect_dimensions("inPopX")
  expect_equal(tab$reads, "Popularity")
  expect_false(identical(tab$moves, "not classified"))
})
