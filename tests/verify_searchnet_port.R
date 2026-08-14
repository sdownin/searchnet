# Verification for the diagnostics port and time-varying W.
#
# Usage: Rscript tests/verify_searchnet_port.R
#
# Runnable rather than testthat because this package has no testthat harness
# wired up; every check below prints PASS or FAIL and the script exits non-zero
# if anything fails, so it can gate a commit.

suppressMessages(library(RSiena))

FAILS <- 0L
ok <- function(label, cond, detail = "") {
  if (isTRUE(cond)) {
    cat(sprintf("PASS  %s\n", label))
  } else {
    FAILS <<- FAILS + 1L
    cat(sprintf("FAIL  %s   %s\n", label, detail))
  }
}
errs <- function(expr) tryCatch({ expr; NA_character_ },
                                error = function(e) conditionMessage(e))

src <- function(f) source(f, local = globalenv())
src("R/saomnk-api.R")
src("R/searchnet-screens.R")

cat("\n== .saomnk_as_dyad_array ==\n")
N <- 5L; P <- 3L
arr <- array(runif(N * N * P), c(N, N, P))
lst <- lapply(seq_len(P), function(i) matrix(runif(N * N), N, N))

a1 <- .saomnk_as_dyad_array(arr, "geo")
ok("array in, array out with same dims", identical(dim(a1), c(N, N, P)))
ok("diagonal zeroed on every period",
   all(vapply(seq_len(P), function(i) all(diag(a1[, , i]) == 0), logical(1))))

a2 <- .saomnk_as_dyad_array(lst, "geo")
ok("list of matrices coerces to N x N x P", identical(dim(a2), c(N, N, P)))
ok("list values preserved off-diagonal",
   isTRUE(all.equal(a2[1, 2, 2], lst[[2]][1, 2])))

ok("single matrix is rejected with a useful message",
   grepl("one matrix per period",
         errs(.saomnk_as_dyad_array(matrix(1, N, N), "geo")) %||% ""))
ok("non-square array rejected",
   grepl("first two dimensions",
         errs(.saomnk_as_dyad_array(array(1, c(4L, 5L, 2L)), "geo")) %||% ""))
ok("ragged list rejected",
   grepl("mixes matrix dimensions",
         errs(.saomnk_as_dyad_array(list(matrix(1, 4, 4), matrix(1, 5, 5)),
                                    "geo")) %||% ""))
ok("NA rejected",
   grepl("contains NA", errs({
     b <- arr; b[1, 2, 1] <- NA; .saomnk_as_dyad_array(b, "geo")
   }) %||% ""))
ok("empty list rejected",
   grepl("empty list", errs(.saomnk_as_dyad_array(list(), "geo")) %||% ""))

cat("\n== boundary_screen ==\n")
# The manuscript's central claim, as a test: the SAMPLING RULE, not the data,
# moves outIso onto its boundary.
set.seed(1)
M <- 60L; NC <- 20L; TT <- 3L
x <- array(rbinom(M * NC * TT, 1, 0.15), c(M, NC, TT))
unb <- boundary_screen(x)
keep <- apply(apply(x, c(1, 3), sum) > 0, 1, all)
bal <- boundary_screen(x[keep, , , drop = FALSE])

gv <- function(d, eff, col) d[[col]][d$effect == eff][1]
ok("unbalanced panel has interior outIso",
   grepl("interior", gv(unb, "outIso", "verdict")),
   gv(unb, "outIso", "verdict"))
ok("balanced panel saturates outIso",
   grepl("SATURATED|cannot converge", gv(bal, "outIso", "verdict")),
   gv(bal, "outIso", "verdict"))
ok("balanced outIso position is exactly 0",
   isTRUE(all.equal(as.numeric(gv(bal, "outIso", "position")), 0)))
ok("balancing actually removed actors", sum(keep) < M,
   sprintf("kept %d of %d", sum(keep), M))
ok("screen returns one row per candidate effect", nrow(unb) >= 3L)

cat("\n== scope_confound_screen ==\n")
# The manuscript's second claim, as a test. A dense W makes the coupling
# statistic a proxy for actor scope; ROW-NORMALIZATION DOES NOT FIX IT, because
# the confound lives in the density pattern rather than the scale, and banding
# does. W must carry variation for banding to have anything to select on: with
# an all-ones W every cell ties and the band treatment is a no-op, which is a
# property of that W rather than a defect in the screen.
set.seed(2)
W <- matrix(runif(NC * NC), NC, NC); W <- (W + t(W)) / 2; diag(W) <- 0
sc <- scope_confound_screen(x, W)
ok("returns one row per treatment", nrow(sc) >= 3L)
ok("carries the r_scope column", "r_scope" %in% names(sc))
gt <- function(tr) as.numeric(sc$r_scope[sc$treatment == tr][1])
raw <- gt("raw"); rn <- gt("rownorm"); bd <- gt("band")
ok("dense raw coupling tracks actor scope", abs(raw) > 0.5,
   sprintf("r = %.3f", raw))
ok("row-normalization does NOT fix the confound",
   abs(abs(rn) - abs(raw)) < 0.15,
   sprintf("raw %.3f vs rownorm %.3f", raw, rn))
ok("banding DOES reduce the confound", abs(bd) < abs(raw),
   sprintf("raw %.3f vs band %.3f", raw, bd))
ok("banding sparsifies",
   sc$nonzero_share[sc$treatment == "band"][1] <
     sc$nonzero_share[sc$treatment == "raw"][1],
   sprintf("raw share %.2f vs band share %.2f",
           sc$nonzero_share[sc$treatment == "raw"][1],
           sc$nonzero_share[sc$treatment == "band"][1]))

cat("\n== time-varying W assembly ==\n")
# The slots live under m$dv_bipartite, NOT at the top level of the model object.
# Checking the wrong level reports an empty list and looks like a total failure;
# that mistake was made once while writing this test and is pinned here.
Wm <- matrix(runif(36), 6, 6); Wm <- (Wm + t(Wm)) / 2; diag(Wm) <- 0
Wt <- array(runif(6 * 6 * 2), c(6, 6, 2))

ms <- suppressWarnings(saomnk_model(influence_matrices = list(geo = Wm)))
ok("static-only model still builds", inherits(ms, "saomnk_model"))
ok("static-only leaves varDyadCovars empty",
   length(ms$dv_bipartite$varDyadCovars) == 0L,
   sprintf("len %d", length(ms$dv_bipartite$varDyadCovars)))
ok("static-only populates coDyadCovars",
   length(ms$dv_bipartite$coDyadCovars) == 1L)

mm <- suppressWarnings(saomnk_model(influence_matrices = list(geo = Wm),
                                    influence_arrays  = list(tension = Wt)))
vd <- mm$dv_bipartite$varDyadCovars
ok("static and time-varying coexist in one model",
   length(vd) == 1L && length(mm$dv_bipartite$coDyadCovars) == 1L,
   sprintf("var %d, co %d", length(vd),
           length(mm$dv_bipartite$coDyadCovars)))
ok("time-varying entry carries the N x N x P array",
   identical(dim(vd[[1]]$x), c(6L, 6L, 2L)))
ok("time-varying entry uses XWX", identical(vd[[1]]$effect, "XWX"))
ok("time-varying entry points at its own slot",
   grepl("varDyadCovar", vd[[1]]$interaction1))
ok("static and varying do not renumber each other",
   grepl("component_1_coDyadCovar",
         mm$dv_bipartite$coDyadCovars[[1]]$interaction1) &&
     grepl("component_1_varDyadCovar", vd[[1]]$interaction1))

cat("\n== exports declared ==\n")
for (f in c("boundary_screen", "scope_confound_screen", "gof_battery",
            "rate_ladder"))
  ok(sprintf("%s is defined", f), is.function(get0(f)))

cat(sprintf("\n%s: %d failure(s)\n",
            if (FAILS == 0L) "ALL CHECKS PASSED" else "FAILURES", FAILS))
if (FAILS > 0L) quit(status = 1L)
