## End-to-end pipeline test: R simulation -> CSV export
cat("=== searchnet Pipeline Test ===\n")

## Load engine — source files directly to avoid loader scoping issues
dir_r <- "D:/Search_networks/SaoMNK/R"
library(R6); library(igraph); library(RSiena); library(ggplot2); library(dplyr)
library(plyr); library(tidyr); library(Matrix); library(reshape2); library(uuid)
library(grid); library(gridExtra); library(texreg)
source(file.path(dir_r, "utils.R"))
source(file.path(dir_r, "saomnk-base.R"))
source(file.path(dir_r, "saomnk-class.R"))
source(file.path(dir_r, "searchnet-export.R"))
cat("Engine loaded.\n")

## Create environment
DV_NAME <- "self$bipartite_rsienaDV"
library(Matrix)
create_block_diag <- function(N, B) {
  block_sizes <- rep(N %/% B, B)
  r <- N %% B
  if (r > 0) block_sizes[1:r] <- block_sizes[1:r] + 1
  blocks <- lapply(block_sizes, function(s) matrix(1, nrow = s, ncol = s))
  as.matrix(Matrix::bdiag(blocks))[1:N, 1:N]
}

env <- SaomNkRSienaBiEnv$new(list(
  M = 6, N = 8, BI_PROB = 0.3, rand_seed = 42, name = "_pipeline_"
))
cat(sprintf("Environment: M=%d, N=%d\n", env$M, env$N))

## Run simulation
sm <- list(dv_bipartite = list(
  name = DV_NAME,
  effects = list(
    list(effect = "density", parameter = -0.5, dv_name = DV_NAME, fix = TRUE)
  ),
  coDyadCovars = list(
    list(effect = "XWX", parameter = 0.3, dv_name = DV_NAME, fix = TRUE,
         nodeSet = c("COMPONENTS", "COMPONENTS"),
         interaction1 = "self$component_1_coDyadCovar",
         x = create_block_diag(8, 2))
  )
))

env$search_rsiena(sm, iterations_per_actor = 30, run_seed = 12345)
cat(sprintf("Simulation done: %d steps\n", env$get_step()))

## Export
outdir <- "D:/Search_networks/SaoMNK/inst/manim/demo_data"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

## Export snapshots manually (chain stats may not be processed)
steps <- unique(round(seq(1, env$get_step(), length.out = 20)))
snap_rows <- list()
for (s in steps) {
  mat <- env$bi_env_arr[, , s]
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (mat[i, j] == 1) {
        snap_rows[[length(snap_rows) + 1]] <- data.frame(
          step = s, actor_id = i, component_id = j, tie = 1
        )
      }
    }
  }
}
snap_df <- do.call(rbind, snap_rows)
write.csv(snap_df, file.path(outdir, "snapshots.csv"), row.names = FALSE)
cat(sprintf("Exported snapshots: %d rows\n", nrow(snap_df)))

## Export K-4 trajectory (compute from bi_env_arr directly)
k4_rows <- list()
for (s in 1:env$get_step()) {
  mat <- env$bi_env_arr[, , s]
  K_AC <- mean(rowSums(mat))
  K_CA <- mean(colSums(mat))
  soc <- mat %*% t(mat); diag(soc) <- 0
  K_AA <- mean(rowSums(soc > 0))
  epi <- t(mat) %*% mat; diag(epi) <- 0
  K_CC <- mean(rowSums(epi > 0))
  k4_rows[[s]] <- data.frame(step = s, K_AC = K_AC, K_CA = K_CA, K_AA = K_AA, K_CC = K_CC)
}
k4_df <- do.call(rbind, k4_rows)
write.csv(k4_df, file.path(outdir, "k4_trajectory.csv"), row.names = FALSE)
cat(sprintf("Exported K-4 trajectory: %d steps\n", nrow(k4_df)))

cat("=== Pipeline test DONE ===\n")
