#!/usr/bin/env Rscript
## =============================================================================
## benchmark_search_rsiena.R
##
## Wall time of searchnet's saomnk_run() against the raw RSiena call it makes,
## at four problem sizes, with a breakdown of searchnet's time by component.
##
## Arms, timed in rotating order after one discarded warm-up per size:
##   searchnet  saomnk_env() + saomnk_model() + saomnk_run(), the user-facing
##              call (density -0.5, XWX on a block-diagonal influence matrix
##              with floor(N/3) blocks, steps_per_actor = 30, seed 1000 + r)
##   sim_only   the same model through env$search_rsiena(process_chain = FALSE):
##              searchnet's simulation without chain processing
##   raw        RSiena alone, built by hand to match what searchnet passes to
##              siena07(): a bipartite dependent variable with two identical
##              waves at the start state, the influence matrix as a
##              coDyadCovar, density + XWX at the same values, an
##              unconditional simOnly run (cond = FALSE, n3 = 2, nsub = 0)
##              with the basic rate set to steps_per_actor, and chains,
##              simulated networks and thetas returned
##
## Overhead is (searchnet - raw) / raw, from medians. The searchnet arm is
## split into components by timing inside the same run (the environment's
## methods are wrapped with timers, which adds two clock reads per ministep):
##   other        saomnk_env() and saomnk_model()
##   simulation   the rest of search_rsiena() outside chain processing: RSiena
##                data and effects, siena07() per segment, path gate, theta grid
##   utility      the per-ministep statistics and utilities
##                (get_struct_mod_stats_mat_from_bi_mat(), called once per step)
##   chain        the rest of chain processing: ministep frame, replay, K-4
##                degrees, long tables (search_rsiena_process_ministep_chain()
##                and search_rsiena_process_stats() minus utility)
## Rprof is not used: on Windows under load it dropped most samples.
##
## Usage (from the package root):
##   Rscript tools/benchmark_search_rsiena.R [--reps=10] [--out=DIR] [--lib=LIB]
##                                           [--load-all]
## By default the INSTALLED searchnet is benchmarked (from LIB when given), as
## users run it. --load-all loads the working tree with pkgload::load_all()
## instead; its functions are not byte-compiled, and R6 copies every method
## into each new environment, so the JIT recompiles them for every saomnk_env()
## and the timings are inflated. Install the tree to benchmark it.
## Results: DIR/benchmark_search_rsiena.csv (one row per rep and arm, with the
## component times of the searchnet arm), DIR/benchmark_search_rsiena_summary.csv
## and DIR/benchmark_search_rsiena.md. DIR defaults to a folder under tempdir().
## Background load is not controlled; compare runs made back to back.
## =============================================================================

args <- commandArgs(trailingOnly = TRUE)
.arg <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
n_reps  <- as.integer(.arg("reps", "10"))
out_dir <- .arg("out", file.path(tempdir(), "searchnet_benchmarks"))
lib     <- .arg("lib", NA_character_)
sizes <- data.frame(M = c(4L, 12L, 20L, 30L), N = c(6L, 20L, 40L, 50L))
steps_per_actor <- 30L

if ("--load-all" %in% args) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else if (!is.na(lib)) {
  suppressPackageStartupMessages(library(searchnet, lib.loc = lib))
} else {
  suppressPackageStartupMessages(library(searchnet))
}
suppressPackageStartupMessages(library(RSiena))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_dir <- normalizePath(out_dir)
old_wd <- setwd(tempdir())        # siena07 may write project files here
on.exit(setwd(old_wd), add = TRUE)

## Seconds, with a sub-millisecond clock when microbenchmark is installed
## (proc.time() ticks in milliseconds on Windows, too coarse per ministep).
.now <- if (requireNamespace("microbenchmark", quietly = TRUE)) {
  function() microbenchmark::get_nanotime() * 1e-9
} else {
  function() proc.time()[["elapsed"]]
}

## --- the three arms ---------------------------------------------------------
.W <- function(N) saomnk_block_diagonal(N, max(floor(N / 3), 1L))
.model <- function(N) saomnk_model(density = -0.5, influence_matrix = .W(N))

## Replace an R6 method on one environment by a timed wrapper that adds its
## elapsed time to acc[[slot]].
.time_method <- function(e, method, acc, slot) {
  orig <- e[[method]]
  unlockBinding(method, e)
  assign(method, function(...) {
    t0 <- .now()
    on.exit(acc[[slot]] <- acc[[slot]] + (.now() - t0))
    orig(...)
  }, envir = e)
  lockBinding(method, e)
}

arm_searchnet <- function(M, N, seed) {
  acc <- new.env()
  acc$utility <- 0; acc$chain_all <- 0
  t0 <- .now()
  e <- saomnk_env(M = M, N = N, seed = seed)
  mod <- .model(N)
  t_setup <- .now() - t0
  .time_method(e, "get_struct_mod_stats_mat_from_bi_mat", acc, "utility")
  .time_method(e, "search_rsiena_process_ministep_chain", acc, "chain_all")
  .time_method(e, "search_rsiena_process_stats", acc, "chain_all")
  utils::capture.output(
    saomnk_run(e, mod, steps_per_actor = steps_per_actor, seed = seed))
  total <- .now() - t0
  c(elapsed = total, other = t_setup,
    simulation = total - t_setup - acc$chain_all,
    chain = acc$chain_all - acc$utility, utility = acc$utility)
}

arm_sim_only <- function(M, N, seed) {
  t0 <- .now()
  e <- saomnk_env(M = M, N = N, seed = seed)
  utils::capture.output(
    e$search_rsiena(structure_model = .model(N),
                    iterations_per_actor = steps_per_actor,
                    run_seed = as.integer(seed), process_chain = FALSE))
  c(elapsed = .now() - t0)
}

## The start state searchnet simulates from, built outside the timed region
## so the raw arm starts from the same matrix.
.start_matrix <- function(M, N, seed) {
  e <- saomnk_env(M = M, N = N, seed = seed)
  matrix(as.numeric(e$bipartite_matrix_init), M, N)
}

arm_raw <- function(M, N, seed, B) {
  t0 <- .now()
  dep <- RSiena::sienaDependent(array(c(B, B), dim = c(M, N, 2)),
    type = "bipartite", nodeSet = c("ACTORS", "COMPONENTS"), allowOnly = FALSE)
  W <- RSiena::coDyadCovar(.W(N), nodeSets = c("COMPONENTS", "COMPONENTS"))
  dat <- RSiena::sienaDataCreate(dep, W, nodeSets = list(
    RSiena::sienaNodeSet(M, nodeSetName = "ACTORS"),
    RSiena::sienaNodeSet(N, nodeSetName = "COMPONENTS")))
  eff <- RSiena::getEffects(dat)
  eff <- RSiena::includeEffects(eff, XWX, interaction1 = "W", verbose = FALSE)
  th <- c(steps_per_actor, -0.5, 0.1)     # basic rate, density, XWX
  alg <- RSiena::sienaAlgorithmCreate(projname = NULL, simOnly = TRUE,
    cond = FALSE, nsub = 0, n3 = 2, seed = as.integer(seed), silent = TRUE)
  utils::capture.output(RSiena::siena07(alg, data = dat, effects = eff,
    thetaValues = rbind(th, th), thetaBound = 100, batch = TRUE, silent = TRUE,
    returnDeps = TRUE, returnChains = TRUE, returnThetas = TRUE))
  c(elapsed = .now() - t0)
}

## --- timing -----------------------------------------------------------------
rows <- list()
for (s in seq_len(nrow(sizes))) {
  M <- sizes$M[s]; N <- sizes$N[s]
  B0 <- lapply(1000L + 0:n_reps, function(sd) .start_matrix(M, N, sd))
  ## warm-up (first-call costs), discarded
  invisible(arm_searchnet(M, N, 1000L))
  invisible(arm_sim_only(M, N, 1000L))
  invisible(arm_raw(M, N, 1000L, B0[[1]]))
  for (r in seq_len(n_reps)) {
    seed <- 1000L + r
    arms <- list(
      searchnet = function() arm_searchnet(M, N, seed),
      sim_only  = function() arm_sim_only(M, N, seed),
      raw       = function() arm_raw(M, N, seed, B0[[r + 1L]]))
    ord <- names(arms)[((0:2 + r - 1L) %% 3L) + 1L]
    for (a in ord) {
      x <- arms[[a]]()
      rows[[length(rows) + 1L]] <- data.frame(
        M = M, N = N, rep = r, arm = a, elapsed = x[["elapsed"]],
        other = x["other"], simulation = x["simulation"],
        chain = x["chain"], utility = x["utility"], row.names = NULL)
    }
  }
  cat(sprintf("  %dx%d timed\n", M, N))
}
timings <- do.call(rbind, rows)

## --- summary ----------------------------------------------------------------
.q <- function(x, p) unname(stats::quantile(x, p))
summ <- do.call(rbind, lapply(seq_len(nrow(sizes)), function(s) {
  x <- timings[timings$M == sizes$M[s] & timings$N == sizes$N[s], ]
  sn <- x[x$arm == "searchnet", ]
  med <- function(a) median(x$elapsed[x$arm == a])
  iqr <- function(a) sprintf("%.3f-%.3f", .q(x$elapsed[x$arm == a], .25),
                             .q(x$elapsed[x$arm == a], .75))
  rw <- med("raw")
  data.frame(M = sizes$M[s], N = sizes$N[s], reps = n_reps,
             searchnet_s = med("searchnet"), searchnet_iqr = iqr("searchnet"),
             sim_only_s = med("sim_only"), sim_only_iqr = iqr("sim_only"),
             raw_s = rw, raw_iqr = iqr("raw"),
             overhead_pct = 100 * (med("searchnet") - rw) / rw,
             ## median component time as a percentage of the raw median
             simulation_pct_of_raw = 100 * median(sn$simulation) / rw,
             chain_pct_of_raw      = 100 * median(sn$chain) / rw,
             utility_pct_of_raw    = 100 * median(sn$utility) / rw,
             other_pct_of_raw      = 100 * median(sn$other) / rw)
}))

utils::write.csv(timings, file.path(out_dir, "benchmark_search_rsiena.csv"), row.names = FALSE)
utils::write.csv(summ, file.path(out_dir, "benchmark_search_rsiena_summary.csv"), row.names = FALSE)

md <- c(
  "# searchnet vs raw RSiena",
  "",
  sprintf("searchnet %s, RSiena %s, %s; %d reps per arm after a warm-up, arm order rotating; steps_per_actor = %d.",
          getNamespaceVersion("searchnet"),
          as.character(utils::packageVersion("RSiena")),
          R.version.string, n_reps, steps_per_actor),
  "Seconds are medians (IQR). Overhead = (searchnet - raw) / raw. Component columns: the median time of that part of the searchnet arm as % of the raw RSiena median.",
  "",
  "| M | N | searchnet s | sim only s | raw RSiena s | overhead | simulation | chain | utility | other |",
  "|---|---|---|---|---|---|---|---|---|---|",
  sprintf("| %d | %d | %.3f (%s) | %.3f (%s) | %.3f (%s) | %.0f%% | %.0f%% | %.0f%% | %.0f%% | %.0f%% |",
          summ$M, summ$N, summ$searchnet_s, summ$searchnet_iqr,
          summ$sim_only_s, summ$sim_only_iqr, summ$raw_s, summ$raw_iqr,
          summ$overhead_pct, summ$simulation_pct_of_raw, summ$chain_pct_of_raw,
          summ$utility_pct_of_raw, summ$other_pct_of_raw))
writeLines(md, file.path(out_dir, "benchmark_search_rsiena.md"))
cat(md, sep = "\n")
cat("\nResults in", out_dir, "\n")
