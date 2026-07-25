setwd("D:/Search_networks/SaoMNK")
.saomnk_dir <- "R"
source("R/saomnk-loader.R")
source("R/saomnk-api.R")
library(Matrix)
DV_NAME <- "self$bipartite_rsienaDV"

env1 <- saomnk_env(M = 6, N = 8, density = 0.3, seed = 42)
model1 <- saomnk_model(density = -0.5, popularity = 0.2,
                        epistasis_matrix = saomnk_block_diagonal(8, 2),
                        epistasis_weight = 0.3)
saomnk_run(env1, model1, steps_per_actor = 30, seed = 12345)

cat("Fig 3: 10-component utility\n")
util <- env1$compute_formal_utility(beta_F = 0, beta_s = 0.5, beta_w = 0.3, beta_h = 0.1, beta_cong = 0.2)
png("paper/figures/fig_10component.png", width = 900, height = 500, res = 120)
par(mar = c(5, 10, 3, 2))
cols <- c("#2a9d8f","#e76f51","#d69e2e","#4a7c8f","#e63946","#c1121f","#6b8e6b","#7b2d8e","#f4a261","#f4d35e")
bardata <- as.matrix(util[, 2:11])
rownames(bardata) <- paste0("Actor ", util$actor_id)
barplot(t(bardata), beside = FALSE, horiz = TRUE, las = 1, col = cols, border = NA,
        main = "10-Component Utility Decomposition", xlab = "Utility Contribution")
legend("bottomright", legend = names(util)[2:11], fill = cols, cex = 0.7, ncol = 2, bty = "n")
dev.off()
cat("  saved\n")

cat("Fig 4: {K} from matrix\n")
B6 <- env1$bipartite_matrix
png("paper/figures/fig_k4_from_matrix.png", width = 800, height = 600, res = 120)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
barplot(rowSums(B6), col = "#2a9d8f", main = expression(K[AC] ~ "(Scope)"), ylab = "Degree", names.arg = paste0("A", 1:nrow(B6)))
barplot(colSums(B6), col = "#d69e2e", main = expression(K[CA] ~ "(Popularity)"), ylab = "Degree", names.arg = paste0("C", 1:ncol(B6)))
S <- (B6 > 0) %*% t(B6 > 0); diag(S) <- 0
barplot(rowSums(S > 0), col = "#7b2d8e", main = expression(K[AA] ~ "(Sociality)"), ylab = "Degree", names.arg = paste0("A", 1:nrow(B6)))
E <- t(B6 > 0) %*% (B6 > 0); diag(E) <- 0
barplot(rowSums(E > 0), col = "#e76f51", main = expression(K[CC] ~ "(Epistasis)"), ylab = "Degree", names.arg = paste0("C", 1:ncol(B6)))
dev.off()
cat("  saved\n")

cat("Fig 5: Convergence\n")
W5 <- create_block_diag(8, 2)
sm5 <- list(dv_bipartite = list(name = DV_NAME,
  effects = list(list(effect = "density", parameter = -0.5, dv_name = DV_NAME, fix = TRUE),
                 list(effect = "inPop", parameter = 0.1, dv_name = DV_NAME, fix = TRUE)),
  coDyadCovars = list(list(effect = "XWX", parameter = 0.25, dv_name = DV_NAME, fix = TRUE,
    nodeSet = c("COMPONENTS","COMPONENTS"), interaction1 = "self$component_1_coDyadCovar", x = W5))))
init_probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)
fd <- fs <- numeric(5)
for (i in seq_along(init_probs)) {
  e <- SaomNkRSienaBiEnv$new(list(M=4, N=8, BI_PROB=init_probs[i], rand_seed=300+i, name=paste0("_c",i)))
  e$search_rsiena(sm5, iterations_per_actor=50, run_seed=400+i)
  B <- e$bipartite_matrix; fd[i] <- sum(B)/32; fs[i] <- mean(rowSums(B))
}
png("paper/figures/fig_convergence.png", width = 800, height = 400, res = 120)
par(mfrow = c(1,2), mar = c(4.5,4.5,3,1))
plot(init_probs, fd, type="b", pch=19, col="#2a9d8f", lwd=2, xlab="Initial Density", ylab="Final Density", main="Convergence: Density", ylim=c(0,1))
abline(h=mean(fd), lty=2, col="#e76f51")
plot(init_probs, fs, type="b", pch=19, col="#1a365d", lwd=2, xlab="Initial Density", ylab="Mean Scope", main="Convergence: Scope")
abline(h=mean(fs), lty=2, col="#e76f51")
dev.off()
cat("  saved\n")

cat("Fig 6: Bipartite heatmap\n")
png("paper/figures/fig_bipartite_heatmap.png", width = 700, height = 500, res = 120)
image(t(env1$bipartite_matrix[nrow(env1$bipartite_matrix):1, ]),
      col = c("#0f1729", "#2a9d8f"), axes = FALSE,
      main = "Bipartite Matrix B (Actors x Components)")
axis(1, at = seq(0, 1, length = ncol(env1$bipartite_matrix)),
     labels = paste0("C", 1:ncol(env1$bipartite_matrix)), cex.axis = 0.8)
axis(2, at = seq(0, 1, length = nrow(env1$bipartite_matrix)),
     labels = paste0("A", nrow(env1$bipartite_matrix):1), las = 1, cex.axis = 0.8)
dev.off()
cat("  saved\n")

cat("\nAll figures:\n")
print(list.files("paper/figures", pattern = "\\.png$"))
