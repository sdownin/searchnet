## Pre-generate all figures for the JSS manuscript PDF
## These get saved as PNG files that the manuscript includes via knitr
setwd("D:/Search_networks/SaoMNK")
dir.create("paper/figures", showWarnings = FALSE)

cat("Loading searchnet...\n")
.saomnk_dir <- "R"
source("R/saomnk-loader.R")
source("R/saomnk-api.R")
source("R/plot-phase-space.R")
library(Matrix)
DV_NAME <- "self$bipartite_rsienaDV"

cat("=== Figure 1: K-4 Panel (Quick Start) ===\n")
env1 <- saomnk_env(M = 6, N = 8, density = 0.3, seed = 42)
model1 <- saomnk_model(density = -0.5, popularity = 0.2,
                        epistasis_matrix = saomnk_block_diagonal(8, 2),
                        epistasis_weight = 0.3)
saomnk_run(env1, model1, steps_per_actor = 30, seed = 12345)

png("paper/figures/fig_k4_panel.png", width = 900, height = 700, res = 120)
env1$plot_degree_4panel(loess_span = 0.3)
dev.off()
cat("  fig_k4_panel.png saved\n")

cat("=== Figure 2: Network Snapshots ===\n")
png("paper/figures/fig_snapshots.png", width = 1100, height = 400, res = 120)
env1$plot_snapshots(c(1, 60, 120, 180))
dev.off()
cat("  fig_snapshots.png saved\n")

cat("=== Figure 3: Utility Decomposition ===\n")
png("paper/figures/fig_utility.png", width = 900, height = 800, res = 120)
tryCatch(
  env1$plot_utility_contributions(loess_span = 0.35),
  error = function(e) {
    plot(1, type = "n", main = "Utility Contributions (placeholder)")
    text(1, 1, "Requires search_rsiena_process_stats()")
  }
)
dev.off()
cat("  fig_utility.png saved\n")

cat("=== Figure 4: Formal Utility Decomposition (10 components) ===\n")
util <- env1$compute_formal_utility(beta_F = 0, beta_s = 0.5, beta_w = 0.3,
                                     beta_h = 0.1, beta_cong = 0.2)
png("paper/figures/fig_10component.png", width = 900, height = 500, res = 120)
par(mar = c(5, 10, 3, 2))
cols <- c("#2a9d8f", "#e76f51", "#d69e2e", "#4a7c8f", "#e63946",
          "#c1121f", "#6b8e6b", "#7b2d8e", "#f4a261", "#f4d35e")
# Stack bars for each actor
bardata <- as.matrix(util[, 2:11])
rownames(bardata) <- paste0("Actor ", util$actor_id)
barplot(t(bardata), beside = FALSE, horiz = TRUE, las = 1,
        col = cols, border = NA,
        main = "10-Component Utility Decomposition",
        xlab = "Utility Contribution")
legend("bottomright", legend = names(util)[2:11], fill = cols,
       cex = 0.7, ncol = 2, bty = "n")
dev.off()
cat("  fig_10component.png saved\n")

cat("=== Figure 5: NK Validation (convergence from different starts) ===\n")
W5 <- create_block_diag(8, 2)
sm5 <- list(dv_bipartite = list(
  name = DV_NAME,
  effects = list(
    list(effect = "density", parameter = -0.5, dv_name = DV_NAME, fix = TRUE),
    list(effect = "inPop", parameter = 0.1, dv_name = DV_NAME, fix = TRUE)
  ),
  coDyadCovars = list(
    list(effect = "XWX", parameter = 0.25, dv_name = DV_NAME, fix = TRUE,
         nodeSet = c("COMPONENTS", "COMPONENTS"),
         interaction1 = "self$component_1_coDyadCovar", x = W5)
  )
))

init_probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)
final_dens <- numeric(5)
final_scope <- numeric(5)
for (idx in seq_along(init_probs)) {
  e5 <- SaomNkRSienaBiEnv$new(list(M = 4, N = 8, BI_PROB = init_probs[idx],
                                     rand_seed = 300 + idx, name = paste0("_conv_", idx)))
  e5$search_rsiena(sm5, iterations_per_actor = 50, run_seed = 400 + idx)
  B5 <- e5$bipartite_matrix
  final_dens[idx] <- sum(B5) / (4 * 8)
  final_scope[idx] <- mean(rowSums(B5))
}

png("paper/figures/fig_convergence.png", width = 800, height = 400, res = 120)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
plot(init_probs, final_dens, type = "b", pch = 19, col = "#2a9d8f", lwd = 2,
     xlab = "Initial Density", ylab = "Final Density",
     main = "Convergence: Density", ylim = c(0, 1))
abline(h = mean(final_dens), lty = 2, col = "#e76f51")
plot(init_probs, final_scope, type = "b", pch = 19, col = "#1a365d", lwd = 2,
     xlab = "Initial Density", ylab = "Mean Final Scope (K_AC)",
     main = "Convergence: Scope")
abline(h = mean(final_scope), lty = 2, col = "#e76f51")
dev.off()
cat("  fig_convergence.png saved\n")

cat("=== Figure 6: {K} Framework Table Visual ===\n")
png("paper/figures/fig_k4_from_matrix.png", width = 800, height = 600, res = 120)
B6 <- env1$bipartite_matrix
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
# K_AC
barplot(rowSums(B6), col = "#2a9d8f", main = expression(K[AC] ~ "(Actor Scope)"),
        ylab = "Degree", names.arg = paste0("A", 1:nrow(B6)))
# K_CA
barplot(colSums(B6), col = "#d69e2e", main = expression(K[CA] ~ "(Component Popularity)"),
        ylab = "Degree", names.arg = paste0("C", 1:ncol(B6)))
# K_AA
S <- (B6 > 0) %*% t(B6 > 0); diag(S) <- 0
barplot(rowSums(S > 0), col = "#7b2d8e", main = expression(K[AA] ~ "(Actor Sociality)"),
        ylab = "Degree", names.arg = paste0("A", 1:nrow(B6)))
# K_CC
E <- t(B6 > 0) %*% (B6 > 0); diag(E) <- 0
barplot(rowSums(E > 0), col = "#e76f51", main = expression(K[CC] ~ "(Component Epistasis)"),
        ylab = "Degree", names.arg = paste0("C", 1:ncol(B6)))
dev.off()
cat("  fig_k4_from_matrix.png saved\n")

cat("\n=== All figures generated ===\n")
cat("Files in paper/figures/:\n")
print(list.files("paper/figures", pattern = "\\.png$"))
