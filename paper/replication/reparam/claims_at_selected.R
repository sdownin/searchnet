#!/usr/bin/env Rscript
# =============================================================================
# claims_at_selected.R
#
# Computes, at the SELECTED point of each registered search only, the
# statistics that the paper's text reads (the "reported regardless" lists of
# the pre-registration), plus the regime statistics of the two illustrations
# whose parameters are held (the API run and the shock illustration).
# Nothing here feeds back into selection.
#
# Output: results/claims_at_selected.txt
# =============================================================================

.sd <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  dirname(normalizePath(f[1], mustWork = FALSE))
})
source(file.path(.sd, "reparam_common.R"))

out <- file.path(results_dir, "claims_at_selected.txt")
sink(out, split = TRUE)
cat(session_line(), "\n\n")

rd <- function(f) read.csv(file.path(results_dir, f))
K_DIMS <- c("K_AC", "K_CA", "K_AA", "K_CC")

k_means <- function(env) {
  d <- saomnk_get_degrees(env)
  do.call(rbind, lapply(K_DIMS, function(k) {
    df <- as.data.frame(d[[k]])
    ag <- stats::aggregate(list(value = df$value),
                           by = list(step = df$chain_step_id),
                           FUN = mean, na.rm = TRUE)
    ag$dimension <- k
    ag
  }))
}
terminal <- function(km) sapply(K_DIMS, function(k) {
  s <- km[km$dimension == k, ]; s$value[s$step == max(s$step)] })
initial <- function(km) sapply(K_DIMS, function(k) {
  s <- km[km$dimension == k, ]; s$value[s$step == min(s$step)] })

# --- 1. K-system figures ------------------------------------------------------
cat("=== 1. K-system figures ===\n")
## Deviation D1 of the pre-registration: the joint search had no admissible
## point, so the coupling figure takes the registered selection rule applied to
## the points that pass the coupling criteria alone, and the relocation figure
## keeps its original baseline (-5.0, 0.15).
gk <- rd("grid_k_system.csv")
gk$admissible <- gk$coupling_admissible
sk <- select_point(gk, c("density", "popularity"), c(-5.0, 0.15))
write.csv(sk[, c("density", "popularity", "distance")],
          file.path(results_dir, "selected_k_coupling_D1.csv"), row.names = FALSE)
dA <- sk$density; pA <- sk$popularity
d0 <- -5.0; p0 <- 0.15
cat(sprintf("coupling figure: selected density %.2f, popularity %.2f (D1)\n", dA, pA))
cat(sprintf("relocation figure: original density %.2f, popularity %.2f (no admissible point)\n", d0, p0))
mk <- function(d, p) saomnk_model(density = d, popularity = p, scope = 0.05,
                                  influence_matrix = saomnk_block_diagonal(24, 4),
                                  influence_weight = 0.05)
set.seed(20260909)
eA <- saomnk_env(M = 20, N = 24, density = 0, seed = 20260909)
saomnk_run(eA, mk(dA, pA), steps_per_actor = 40, seed = 20260909)
kmA <- k_means(eA)
wide <- stats::reshape(kmA, direction = "wide", idvar = "step",
                       timevar = "dimension", v.names = "value")
cm <- stats::cor(wide[, -1])
cat("coupling: initial means ", paste(round(initial(kmA), 2), collapse = " / "), "\n")
cat("coupling: terminal means", paste(round(terminal(kmA), 2), collapse = " / "), "\n")
cat(sprintf("coupling: min pairwise correlation %.3f; realized ministeps %d; final density %.3f\n",
            min(cm[upper.tri(cm)]), eA$get_step(), sum(eA$bipartite_matrix) / 480))

arm <- function(shocks) {
  set.seed(77001)
  e <- saomnk_env(M = 20, N = 24, density = 0, seed = 77001)
  saomnk_run(e, mk(d0, p0), steps_per_actor = 40, seed = 77001, shocks = shocks)
  e
}
arms <- list(control = arm(NULL),
             sparsity = arm(list(saomnk_shock("density", d0, 1),
                                 saomnk_shock("density", d0 - 1.5, 1))),
             popularity = arm(list(saomnk_shock("popularity", p0, 1),
                                   saomnk_shock("popularity", p0 + 1.05, 1))))
tm <- sapply(arms, function(e) terminal(k_means(e)))
print(round(tm, 3))
cat("ordered sparsity < control < popularity, by dimension: ",
    paste(K_DIMS, apply(tm, 1, function(x) x[2] < x[1] && x[1] < x[3]),
          collapse = ", "), "\n")
s1 <- arms$sparsity$path_segments$n_ministeps[1]
pre <- lapply(arms, function(e) { k <- k_means(e); k[k$step <= s1, "value"] })
cat(sprintf("shock after ministep %d; arms identical before it: %s\n", s1,
            isTRUE(all.equal(pre$control, pre$sparsity)) &&
              isTRUE(all.equal(pre$control, pre$popularity))))
cat("final densities by arm:", paste(round(sapply(arms, function(e)
  sum(e$bipartite_matrix) / 480), 3), collapse = " / "), "\n\n")

# --- 2. Endogenous illustration ----------------------------------------------
cat("=== 2. Endogenous illustration ===\n")
se <- rd("selected_endogenous.csv")
cat(sprintf("selected density %.2f, popularity %.2f, scope %.3f\n",
            se$density, se$popularity, se$scope))
egoX <- rep(c(-1, 0, 1), length.out = 12)
eE <- saomnk_env(M = 12, N = 12, density = 0, seed = 1234)
saomnk_run(eE, saomnk_model(
  density = se$density, popularity = se$popularity, scope = se$scope,
  influence_matrix = saomnk_block_diagonal(12, 4), influence_weight = 0.02,
  strategies = list(egoX = egoX, inPopX = rep(c(1, 0, -1), length.out = 12))),
  steps_per_actor = 30, seed = 12345)
B <- eE$bipartite_matrix
kac <- rowSums(B)
g <- tapply(kac, egoX, mean)
cat("final K_AC by egoX group (-1 / 0 / +1):", paste(round(g, 2), collapse = " / "), "\n")
P <- (t(B) %*% B > 0) * 1; diag(P) <- 0
kcc <- rowSums(P)
cat(sprintf("final K_CC across components: range %d-%d, SD %.2f; final density %.3f; ministeps %d\n",
            min(kcc), max(kcc), stats::sd(kcc), sum(B) / 144, eE$get_step()))
cat(sprintf("pattern shown (strict order and +1 minus -1 >= 2): %s\n\n",
            g[["-1"]] < g[["0"]] && g[["0"]] < g[["1"]] && (g[["1"]] - g[["-1"]]) >= 2))

# --- 3. Theorem 2 example -----------------------------------------------------
cat("=== 3. Appendix Theorem 2 example ===\n")
st <- rd("selected_thm2.csv")
cat(sprintf("selected density %.2f, inPop %.2f, outAct %.2f\n",
            st$density, st$inPop, st$outAct))
DV <- "self$bipartite_rsienaDV"
sm2 <- list(dv_bipartite = list(name = DV,
  effects = list(
    list(effect = "density", parameter = st$density, dv_name = DV, fix = TRUE),
    list(effect = "inPop",   parameter = st$inPop,   dv_name = DV, fix = TRUE),
    list(effect = "outAct",  parameter = st$outAct,  dv_name = DV, fix = TRUE)),
  coDyadCovars = list(list(effect = "XWX", parameter = 0.3, dv_name = DV,
    fix = TRUE, nodeSet = c("COMPONENTS", "COMPONENTS"),
    interaction1 = "self$component_1_coDyadCovar", x = create_block_diag(6, 2)))))
eT <- SaomNkRSienaBiEnv$new(list(M = 4, N = 6, BI_PROB = 0, rand_seed = 99,
                                 name = "_thm2_endog_"))
utils::capture.output(eT$search_rsiena(sm2, iterations_per_actor = 25, run_seed = 12345))
B <- eT$bipartite_matrix
P <- (t(B) %*% B > 0) * 1; diag(P) <- 0
cat("final K_AC by actor:", paste(rowSums(B), collapse = " "), "\n")
cat("final K_CA by component:", paste(colSums(B), collapse = " "), "\n")
cat("final K_CC by component:", paste(rowSums(P), collapse = " "), "\n")
cat(sprintf("final density %.3f; ministeps %d\n", sum(B) / 24, eT$get_step()))
cat(sprintf("pattern shown (max K_AC < 6 and K_CA takes >= 2 values): %s\n\n",
            max(rowSums(B)) < 6 && length(unique(colSums(B))) >= 2))

# --- 4. Ergodicity --------------------------------------------------------------
cat("=== 4. Ergodicity sweep ===\n")
## No admissible point on the registered grid: the original -0.5 is kept
## (registered fallback), and its sweep is reported.
sg <- if (file.exists(file.path(results_dir, "selected_ergodicity.csv")))
  rd("selected_ergodicity.csv") else data.frame(density_par = -0.5)
cat(sprintf("density_par %.2f (%s)\n", sg$density_par,
            if (sg$density_par == -0.5) "original kept: no admissible point"
            else "selected"))
erg <- searchnet_ergodicity_sweep(
  M = 12, N = 15, start_densities = c(0.1, 0.8),
  run_lengths = c(15, 30, 60, 120, 240), replicates = 10,
  density_par = sg$density_par, equivalence_margin = 0.05, seed = 42)
print(erg)
print(erg$summary)
str(erg$decay)
fg <- erg$summary$gap[erg$summary$run_length == 240]
cat(sprintf("final gap %.4f; collapse %.1f%%\n", fg, 100 * (1 - fg / 0.70)))
cat("\n")

# --- 5. Held illustrations: regime statistics only ---------------------------
cat("=== 5. Held parameters: regime statistics ===\n")
eP <- saomnk_env(M = 8, N = 12, density = 0, seed = 42)
saomnk_run(eP, saomnk_model(density = -0.5, popularity = 0.2, scope = 0.1,
  influence_matrix = saomnk_block_diagonal(12, 4), influence_weight = 0.05),
  steps_per_actor = 30, seed = 12345)
cat("API run (criteria of 2.2):\n"); print(regime_stats(eP))
eB <- saomnk_env(M = 6, N = 8, density = 0, seed = 42)
saomnk_run(eB, saomnk_model(density = -0.5,
  influence_matrix = saomnk_block_diagonal(8, 2), influence_weight = 0.5),
  steps_per_actor = 80, seed = 12345)
cat("Shock illustration baseline (criteria of 2.1 control arm):\n"); print(regime_stats(eB))
sink()
