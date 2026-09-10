#!/usr/bin/env Rscript
# =============================================================================
# benchmark.R
# Computational benchmarks for: "searchnet: Network-Embedded Strategic Search
# Simulation in R" (Journal of Statistical Software)
#
# Reproduces Section 6 benchmarks:
#   1. searchnet overhead vs raw RSiena
#   2. Scalability across M/N problem sizes
#   3. Fitness landscape timing (full enumeration vs sampled)
#
# Total runtime: < 5 minutes
# =============================================================================

cat("=================================================================\n")
cat("  searchnet Benchmark Script\n")
cat("  Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=================================================================\n\n")

total_start <- proc.time()

library(searchnet)
library(RSiena)

# Output directory
fig_dir <- file.path(dirname(sys.frame(1)$ofile %||% "."), "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)


# =============================================================================
# BENCHMARK 1: searchnet overhead vs raw RSiena (Section 6.1)
# =============================================================================
cat("--- Benchmark 1: Overhead vs RSiena ---\n")

n_reps <- 5
overhead_results <- data.frame(
  rep        = integer(),
  searchnet  = numeric(),
  rsiena_raw = numeric(),
  overhead_pct = numeric()
)

for (r in seq_len(n_reps)) {
  set.seed(r)

  # searchnet wrapper timing (M=4, N=6, 30 steps/actor)
  t_sn <- system.time({
    e_bench <- saomnk_env(M = 4, N = 6, seed = r)
    m_bench <- saomnk_model(
      density = -0.5,
      influence_matrix = saomnk_block_diagonal(6, 2)
    )
    saomnk_run(e_bench, m_bench, steps_per_actor = 30, seed = r)
  })

  # Raw RSiena siena07 call on equivalent bipartite problem
  t_rs <- system.time({
    net_start <- matrix(0L, nrow = 4, ncol = 6)
    net_end   <- matrix(0L, nrow = 4, ncol = 6)
    dep_var   <- RSiena::sienaDependent(
      array(c(net_start, net_end), dim = c(4, 6, 2)),
      type = "bipartite", nodeSet = c("actors", "components")
    )
    actors_set <- RSiena::sienaNodeSet(4, nodeSetName = "actors")
    comp_set   <- RSiena::sienaNodeSet(6, nodeSetName = "components")
    dat_raw    <- RSiena::sienaDataCreate(
      dep_var,
      nodeSets = list(actors_set, comp_set)
    )
    eff_raw <- RSiena::getEffects(dat_raw)
    alg_raw <- RSiena::sienaAlgorithmCreate(
      projname = "bench_raw",
      nsub = 0, n3 = 4 * 30, simOnly = TRUE, seed = r
    )
    suppressMessages(
      RSiena::siena07(alg_raw, data = dat_raw, effects = eff_raw,
                      batch = TRUE, silent = TRUE, returnDeps = TRUE)
    )
  })

  ovh <- 100 * (t_sn["elapsed"] - t_rs["elapsed"]) / max(t_rs["elapsed"], 0.001)
  overhead_results <- rbind(overhead_results, data.frame(
    rep          = r,
    searchnet    = round(t_sn["elapsed"], 3),
    rsiena_raw   = round(t_rs["elapsed"], 3),
    overhead_pct = round(ovh, 1)
  ))
}

cat("  Results (", n_reps, "replications):\n", sep = "")
print(overhead_results)
cat("\n  Median overhead:",
    round(median(overhead_results$overhead_pct), 1), "%\n\n")

write.csv(overhead_results,
          file.path(fig_dir, "benchmark_overhead.csv"),
          row.names = FALSE)
cat("  Saved: benchmark_overhead.csv\n\n")


# =============================================================================
# BENCHMARK 2: Scalability across M/N sizes (Section 6.2, Table 2)
# =============================================================================
cat("--- Benchmark 2: Scalability ---\n")

# Problem sizes matching manuscript Table 2 (kept small for < 10 min total)
sizes <- data.frame(
  M = c(6, 10, 12, 20),
  N = c(8, 15, 20, 30)
)

n_scale_reps <- 3
scale_results <- data.frame(
  M = integer(), N = integer(),
  rep = integer(), elapsed = numeric()
)

for (i in seq_len(nrow(sizes))) {
  m_val <- sizes$M[i]
  n_val <- sizes$N[i]
  n_blocks <- max(floor(n_val / 3), 1)

  cat("  M=", m_val, ", N=", n_val, ": ", sep = "")

  for (r in seq_len(n_scale_reps)) {
    set.seed(1000 + r)
    t_scale <- system.time({
      e_s <- saomnk_env(M = m_val, N = n_val, seed = 1000 + r)
      m_s <- saomnk_model(
        density = -0.5,
        influence_matrix = saomnk_block_diagonal(n_val, n_blocks)
      )
      saomnk_run(e_s, m_s, steps_per_actor = 30, seed = 1000 + r)
    })

    scale_results <- rbind(scale_results, data.frame(
      M = m_val, N = n_val,
      rep = r, elapsed = round(t_scale["elapsed"], 3)
    ))
    cat(".")
  }
  cat("\n")
}

# Compute medians
scale_summary <- aggregate(elapsed ~ M + N, data = scale_results,
                           FUN = median)
names(scale_summary)[3] <- "median_seconds"
scale_summary$median_seconds <- round(scale_summary$median_seconds, 2)

cat("\n  Scalability summary (median seconds):\n")
print(scale_summary)

write.csv(scale_results,
          file.path(fig_dir, "benchmark_scalability.csv"),
          row.names = FALSE)
cat("\n  Saved: benchmark_scalability.csv\n\n")


# =============================================================================
# BENCHMARK 3: Fitness landscape timing (Section 6.3)
# Full enumeration for small N; sampled approximation for larger N
# =============================================================================
cat("--- Benchmark 3: Fitness Landscape Computation ---\n")

landscape_results <- data.frame(
  N = integer(), method = character(),
  configurations = integer(), elapsed = numeric()
)

# Full enumeration: N = 8, 10, 12
for (n_val in c(8, 10, 12)) {
  set.seed(2000)
  cat("  N=", n_val, " (full, 2^N=", 2^n_val, "): ", sep = "")

  e_ls <- saomnk_env(M = 4, N = n_val, seed = 2000)
  m_ls <- saomnk_model(
    density = -0.5,
    influence_matrix = saomnk_block_diagonal(n_val, max(floor(n_val / 3), 1))
  )
  saomnk_run(e_ls, m_ls, steps_per_actor = 10, seed = 2000)

  t_ls <- tryCatch({
    system.time({
      e_ls$compute_fitness_landscape()
    })
  }, error = function(e) {
    cat("(skipped: ", conditionMessage(e), ") ")
    c(elapsed = NA)
  })

  landscape_results <- rbind(landscape_results, data.frame(
    N = n_val, method = "full",
    configurations = 2^n_val,
    elapsed = round(t_ls["elapsed"], 3)
  ))
  cat(round(t_ls["elapsed"], 2), "s\n")
}

# Sampled approximation: N = 15, 20
for (n_val in c(15, 20)) {
  set.seed(3000)
  cat("  N=", n_val, " (sampled, S=10000): ", sep = "")

  e_ls <- saomnk_env(M = 4, N = n_val, seed = 3000)
  m_ls <- saomnk_model(
    density = -0.5,
    influence_matrix = saomnk_block_diagonal(n_val, max(floor(n_val / 3), 1))
  )
  saomnk_run(e_ls, m_ls, steps_per_actor = 10, seed = 3000)

  t_ls <- tryCatch({
    system.time({
      e_ls$compute_fitness_landscape(sample_size = 10000)
    })
  }, error = function(e) {
    cat("(skipped: ", conditionMessage(e), ") ")
    c(elapsed = NA)
  })

  landscape_results <- rbind(landscape_results, data.frame(
    N = n_val, method = "sampled",
    configurations = 10000,
    elapsed = round(t_ls["elapsed"], 3)
  ))
  cat(round(t_ls["elapsed"], 2), "s\n")
}

cat("\n  Landscape timing summary:\n")
print(landscape_results)

write.csv(landscape_results,
          file.path(fig_dir, "benchmark_landscape.csv"),
          row.names = FALSE)
cat("\n  Saved: benchmark_landscape.csv\n\n")


# =============================================================================
# SUMMARY
# =============================================================================
total_elapsed <- (proc.time() - total_start)["elapsed"]

cat("=================================================================\n")
cat("  Benchmarks Complete\n")
cat("  Total time:", round(total_elapsed, 1), "seconds\n")
cat("  Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=================================================================\n")
cat("\nAll results saved to:", fig_dir, "\n")
