#!/usr/bin/env Rscript
# =============================================================================
# bench_protocol.R
#
# Re-measures the paper's two performance claims under searchnet 0.11.0 with
# the protocol fixed in the pre-registration:
#
#   Overhead: 10 repetitions after one discarded warm-up of each arm, arm
#     order rotating across repetitions.
#       A  saomnk_run(), M 4, N 6, 30 per actor (the paper's chunk)
#       B  the same model via env$search_rsiena(process_chain = FALSE)
#       C  the chunk's raw RSiena siena07() baseline
#   Scalability: saomnk_run(), density -0.5, block diagonal floor(N/3),
#     30 per actor, 10 repetitions (seeds 1000 + r), at the table's six sizes
#     plus 10x30 and 20x15 (a 2 x 2 factorial M {10, 20} x N {15, 30}).
#
# Output: results/bench_overhead.csv, results/bench_scalability.csv,
#         results/bench_summary.txt
# =============================================================================

.sd <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  dirname(normalizePath(f[1], mustWork = FALSE))
})
source(file.path(.sd, "reparam_common.R"))
suppressPackageStartupMessages(library(RSiena))

cpu <- tryCatch(trimws(system2("powershell", c("-NoProfile", "-Command",
  shQuote("(Get-CimInstance Win32_Processor).Name")), stdout = TRUE))[1],
  error = function(e) NA_character_)
ram <- tryCatch(trimws(system2("powershell", c("-NoProfile", "-Command",
  shQuote("[math]::Round((Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory/1GB)"))),
  stdout = TRUE)[1], error = function(e) NA_character_)
machine <- sprintf("%s | %s GB RAM | %s", cpu, ram, session_line())

arm_A <- function(r) system.time({
  e <- saomnk_env(M = 4, N = 6, seed = r)
  m <- saomnk_model(density = -0.5, influence_matrix = saomnk_block_diagonal(6, 2))
  saomnk_run(e, m, steps_per_actor = 30, seed = r)
})[["elapsed"]]

arm_B <- function(r) system.time({
  e <- saomnk_env(M = 4, N = 6, seed = r)
  m <- saomnk_model(density = -0.5, influence_matrix = saomnk_block_diagonal(6, 2))
  e$search_rsiena(structure_model = m, iterations_per_actor = 30L,
                  run_seed = as.integer(r), process_chain = FALSE)
})[["elapsed"]]

arm_C <- function(r) system.time({
  net0 <- matrix(0L, 4, 6)
  dep <- RSiena::sienaDependent(array(c(net0, net0), dim = c(4, 6, 2)),
    type = "bipartite", nodeSet = c("actors", "components"), allowOnly = FALSE)
  dat <- RSiena::sienaDataCreate(dep, nodeSets = list(
    RSiena::sienaNodeSet(4, nodeSetName = "actors"),
    RSiena::sienaNodeSet(6, nodeSetName = "components")))
  eff <- RSiena::getEffects(dat)
  alg <- RSiena::sienaAlgorithmCreate(projname = "bench_raw", nsub = 0,
    n3 = 4 * 30, simOnly = TRUE, seed = r)
  suppressMessages(RSiena::siena07(alg, data = dat, effects = eff,
    batch = TRUE, silent = TRUE, returnDeps = TRUE))
})[["elapsed"]]

old_wd <- setwd(tempdir())   # siena07 writes bench_raw.txt to the working dir
arms <- list(A = arm_A, B = arm_B, C = arm_C)
invisible(lapply(arms, function(f) utils::capture.output(f(999))))   # warm-up
ov <- list()
for (r in 1:10) {
  ord <- c("A", "B", "C")[((0:2 + r - 1) %% 3) + 1]
  for (a in ord) {
    utils::capture.output(t <- arms[[a]](r))
    ov[[length(ov) + 1]] <- data.frame(rep = r, arm = a, elapsed = t)
  }
}
ov <- do.call(rbind, ov)
write.csv(ov, file.path(results_dir, "bench_overhead.csv"), row.names = FALSE)

sizes <- data.frame(M = c(6, 10, 12, 20, 20, 30, 10, 20),
                    N = c(8, 15, 20, 30, 40, 50, 30, 15))
sc <- list()
for (i in seq_len(nrow(sizes))) {
  m_ <- sizes$M[i]; n_ <- sizes$N[i]
  for (r in 1:10) {
    e <- NULL
    t <- system.time({
      e <- saomnk_env(M = m_, N = n_, seed = 1000 + r)
      mod <- saomnk_model(density = -0.5,
        influence_matrix = saomnk_block_diagonal(n_, max(floor(n_ / 3), 1)))
      saomnk_run(e, mod, steps_per_actor = 30, seed = 1000 + r)
    })[["elapsed"]]
    sc[[length(sc) + 1]] <- data.frame(M = m_, N = n_, rep = r, elapsed = t,
      ministeps = sum(e$path_segments$n_ministeps),
      final_density = sum(e$bipartite_matrix) / (m_ * n_))
  }
  cat(sprintf("  %dx%d done\n", m_, n_))
}
setwd(old_wd)
sc <- do.call(rbind, sc)
write.csv(sc, file.path(results_dir, "bench_scalability.csv"), row.names = FALSE)

sink(file.path(results_dir, "bench_summary.txt"), split = TRUE)
cat("Machine:", machine, "\n")
cat("Background load not controlled; other processes may have been running.\n\n")
cat("Overhead, seconds per arm (median, IQR):\n")
for (a in c("A", "B", "C")) {
  x <- ov$elapsed[ov$arm == a]
  cat(sprintf("  %s  median %.3f  IQR %.3f-%.3f\n", a, median(x),
              quantile(x, 0.25), quantile(x, 0.75)))
}
mA <- median(ov$elapsed[ov$arm == "A"]); mB <- median(ov$elapsed[ov$arm == "B"])
mC <- median(ov$elapsed[ov$arm == "C"])
pr <- merge(ov[ov$arm == "A", c("rep", "elapsed")],
            ov[ov$arm == "C", c("rep", "elapsed")], by = "rep")
cat(sprintf("  paper's definition (A - C)/C: median of per-rep values %.0f%%; ratio of medians %.0f%%\n",
            100 * median((pr$elapsed.x - pr$elapsed.y) / pr$elapsed.y),
            100 * (mA - mC) / mC))
cat(sprintf("  share of A spent outside the simulation (A - B)/A: %.0f%%\n\n",
            100 * (mA - mB) / mA))
agg <- aggregate(cbind(elapsed, ministeps, final_density) ~ M + N, data = sc,
                 FUN = median)
cat("Scalability, medians of 10 repetitions:\n")
print(agg[order(agg$M, agg$N), ], row.names = FALSE)
fac <- agg[agg$M %in% c(10, 20) & agg$N %in% c(15, 30), ]
fit <- lm(log(elapsed) ~ log(M) + log(N), data = fac)
cat(sprintf("\nFactorial fit log(time) ~ log M + log N: b_M = %.2f, b_N = %.2f\n",
            coef(fit)[2], coef(fit)[3]))
fit_all <- lm(log(elapsed) ~ log(M * N), data = agg)
cat(sprintf("All eight sizes, log(time) ~ log(M N): slope %.2f\n",
            coef(fit_all)[2]))
sink()
