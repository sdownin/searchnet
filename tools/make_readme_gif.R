#!/usr/bin/env Rscript
###############################################################################
## make_readme_gif.R
##
## Regenerates man/figures/readme-landscape.gif, the animation in README.md:
## one seeded run in which actors hold components (the bipartite network) while
## a focal actor searches its fitness landscape, the SAOM objective over all
## 2^N portfolios it could hold, given everyone else's ties.
##
## Usage, from the package root:
##
##     Rscript tools/make_readme_gif.R [--export-only]
##
## Three stages:
##   1. R (this script): a seeded saomnk_run(); for every ministep, the focal
##      actor's objective f_i(x) = s_i(x)' theta over all 2^N rows x of its
##      portfolio, with the other actors' ties held at that ministep's state.
##      s_i is the package's own statistics function
##      (get_struct_mod_stats_mat_from_bi_mat), theta the run's theta_matrix.
##      A gate stops the script unless f_i at the realized portfolio
##      reproduces the utility the package recorded for that ministep
##      (actor_util_df) for every actor and ministep.
##   2. manim: inst/manim/scene_readme_landscape.py renders the JSON to MP4.
##   3. ffmpeg: palette-optimized GIF (two-pass palettegen / paletteuse).
##
## Requirements: searchnet (loaded from source with pkgload when run from the
## package root), jsonlite; Python with manim >= 0.18 (set SEARCHNET_PYTHON to
## the interpreter, default "python"); ffmpeg on PATH. Seeds are fixed below,
## so the data are deterministic for a given searchnet, RSiena and R version.
## Intermediate files go to a temporary directory; only the GIF is written
## into the package.
###############################################################################

export_only <- "--export-only" %in% commandArgs(trailingOnly = TRUE)

if (file.exists("DESCRIPTION") &&
    any(grepl("^Package: searchnet", readLines("DESCRIPTION", n = 1)))) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressMessages(library(searchnet))
}

## ---- 1. the run ------------------------------------------------------------
M <- 6L; N <- 8L
## Coefficients chosen so the landscape is rugged rather than monotone: a tie
## costs more (density) than popularity alone repays, and XWX rewards holding
## components of one module together, so every state of this run has the same
## four local peaks (no components, either full module, all eight) at heights
## the other actors' ties move.
W <- saomnk_block_diagonal(N, 2)            # two modules of four components
env <- saomnk_env(M = M, N = N, density = 0.15, seed = 42)
mod <- saomnk_model(density = -2.5, popularity = 0.2,
                    influence_matrix = W, influence_weight = 0.8)
invisible(capture.output(suppressMessages(
  saomnk_run(env, mod, steps_per_actor = 10, seed = 12345))))

arr   <- env$bi_env_arr                      # M x N x n_steps
n_t   <- dim(arr)[3]
ch    <- env$bi_env_changes                  # step, actor_i, comp_j
theta <- env$theta_matrix
B0    <- if (!is.null(env$path_start_matrix)) env$path_start_matrix else env$bipartite_matrix_init
theta_at <- function(t) theta[min(t, nrow(theta)), ]

## Focal actor: the one that changes its portfolio most often (ties: lowest id).
moves <- tabulate(ch[, "actor_i"], nbins = M)
focal <- which.max(moves)

## All 2^N portfolios; row r holds the binary digits of r - 1, LSB = component 1.
configs <- as.matrix(expand.grid(rep(list(0:1), N)))
colnames(configs) <- NULL
prep <- env$prepare_struct_mod_stats()
f_all <- function(state, i, t) {
  th <- theta_at(t)
  apply(configs, 1L, function(x) {
    s <- state; s[i, ] <- x
    sum(env$get_struct_mod_stats_mat_from_bi_mat(s, .prep = prep)[i, ] * th)
  })
}
row_of <- function(x) 1L + sum(x * 2L^(seq_len(N) - 1L))

## ---- 2. gate: the landscape reproduces the package's recorded utilities ----
ud <- as.data.frame(env$actor_util_df)
for (t in seq_len(n_t)) {
  st <- arr[, , t]
  s  <- env$get_struct_mod_stats_mat_from_bi_mat(st, .prep = prep)
  u  <- as.numeric(s %*% theta_at(t))
  rec <- ud$utility[ud$chain_step_id == t][order(as.integer(as.character(
    ud$actor_id[ud$chain_step_id == t])))]
  if (length(rec) != M || max(abs(u - rec)) > 1e-6)
    stop(sprintf("gate: ministep %d: recomputed utilities do not match actor_util_df", t))
}
## and the per-ministep state follows the recorded toggle from the previous one
prev <- B0
for (t in seq_len(n_t)) {
  d <- arr[, , t] - prev
  i <- ch[t, "actor_i"]; j <- ch[t, "comp_j"]
  if (sum(abs(d)) > 1 ||
      (sum(abs(d)) == 1 && (is.na(i) || is.na(j) || d[i, j] == 0)))
    stop(sprintf("gate: ministep %d is not the recorded single toggle", t))
  prev <- arr[, , t]
}
cat(sprintf("gate passed: %d ministeps, %d actors; focal actor %d moves %d times\n",
            n_t, M, focal, moves[focal]))

## ---- 3. focal landscape at every ministep (plus the start) ----------------
land <- matrix(NA_real_, n_t + 1L, nrow(configs))
land[1L, ] <- f_all(B0, focal, 1L)
for (t in seq_len(n_t)) land[t + 1L, ] <- f_all(arr[, , t], focal, t)

prev <- B0
steps <- vector("list", n_t)
for (t in seq_len(n_t)) {
  i <- ch[t, "actor_i"]; j <- ch[t, "comp_j"]
  ## A ministep in which the chosen actor keeps its portfolio has no toggle.
  delta <- if (is.na(i) || is.na(j)) 0 else arr[i, j, t] - prev[i, j]
  if (is.na(i)) i <- as.integer(env$chain_stats$id_from[t])
  steps[[t]] <- list(t = t, actor = i, comp = if (is.na(j)) 0L else j,
                     change = if (delta > 0) "add" else if (delta < 0) "drop" else "none")
  prev <- arr[, , t]
}

## K-dimension degrees at each state: mean K_AC (portfolio size) and mean K_AA
## (actors met through shared components), for the trace under the network.
k_of <- function(b) {
  S <- b %*% t(b); diag(S) <- 0
  c(K_AC = mean(rowSums(b)), K_AA = mean(rowSums(S > 0)))
}
kt <- rbind(k_of(B0), t(apply(arr, 3L, k_of)))

out <- list(
  M = M, N = N, focal = focal, W = unname(W),
  theta = as.list(theta_at(1L)),
  start = unname(B0),
  states = lapply(seq_len(n_t), function(t) unname(arr[, , t])),
  steps = steps,
  landscape = unname(land),
  focal_row = c(row_of(B0[focal, ]), sapply(seq_len(n_t), function(t) row_of(arr[focal, , t]))),
  K_AC = unname(kt[, "K_AC"]), K_AA = unname(kt[, "K_AA"]),
  seeds = list(env = 42L, run = 12345L)
)

## SEARCHNET_GIF_WORKDIR keeps the intermediates (JSON, MP4) for inspection;
## by default they live in this session's tempdir and vanish with it.
work <- Sys.getenv("SEARCHNET_GIF_WORKDIR", file.path(tempdir(), "readme_gif"))
dir.create(work, showWarnings = FALSE, recursive = TRUE)
json <- file.path(work, "readme_landscape.json")
jsonlite::write_json(out, json, auto_unbox = TRUE, digits = 8)
cat("wrote", json, "\n")
if (export_only) quit(status = 0)

## ---- 4. render ---------------------------------------------------------------
py    <- Sys.getenv("SEARCHNET_PYTHON", "python")
scene <- normalizePath(file.path("inst", "manim", "scene_readme_landscape.py"))
Sys.setenv(SEARCHNET_README_JSON = json)
status <- system2(py, c("-m", "manim", "render", "--media_dir", shQuote(work),
                        "-r", "880,520", "--fps", "10", "--disable_caching",
                        "-o", "readme_landscape", shQuote(scene), "ReadmeLandscapeScene"))
if (status != 0) stop("manim render failed")
mp4 <- list.files(work, pattern = "^readme_landscape[.]mp4$", recursive = TRUE, full.names = TRUE)[1]
if (is.na(mp4)) stop("manim output not found under ", work)

gif <- file.path("man", "figures", "readme-landscape.gif")
pal <- file.path(work, "palette.png")
system2("ffmpeg", c("-v", "error", "-y", "-i", shQuote(mp4),
                    "-vf", shQuote("fps=10,palettegen=max_colors=64:stats_mode=diff"), shQuote(pal)))
system2("ffmpeg", c("-v", "error", "-y", "-i", shQuote(mp4), "-i", shQuote(pal),
                    "-lavfi", shQuote("fps=10[x];[x][1:v]paletteuse=dither=none:diff_mode=rectangle"),
                    "-loop", "0", shQuote(gif)))
cat(sprintf("wrote %s (%.0f KB)\n", gif, file.size(gif) / 1024))
