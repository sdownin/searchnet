#!/usr/bin/env Rscript
###############################################################################
## make_all_actors_gif.R
##
## The "full field" companion to tools/make_readme_gif.R and
## tools/make_two_actor_gif.R (both left untouched): the same seeded setting,
## with ALL six actors followed at once, each on its own fitness landscape.
##
## Usage, from the package root:
##
##     Rscript tools/make_all_actors_gif.R [--export-only] [--out DIR] [--fps F]
##
## The GIF is written to DIR (default: the session's tempdir), never into the
## package; it is a draft for review, not a README figure.
##
## Why six grids. Each actor's objective f_i(x) = s_i(x)' theta depends on the
## other actors' ties, so no two actors share a landscape. Their portfolios do
## live in the same space (the 2^N subsets of the components), so the scene
## draws six grids on the same axes, each colored by one actor's own landscape
## on one shared color scale, with every actor's current portfolio marked on
## every grid (own token solid, the other five as hollow rings). Landscapes
## are never averaged.
##
## Stages:
##   1. R (this script): the run (seed rule below); the gate that every
##      recorded utility is reproduced by the package's own statistics
##      function; every actor's objective over all 2^N portfolios at every
##      ministep, the other actors' ties held at that ministep's state; the
##      gate that each landscape, read at the actor's realized portfolio, is
##      that actor's recorded utility at every ministep.
##   2. manim: inst/manim/scene_all_actors_landscape.py renders the JSON to a
##      lossless .mov.
##   3. ffmpeg: palette-optimized GIF (two-pass palettegen / paletteuse).
##
## Requirements as for make_readme_gif.R: searchnet, jsonlite, Python with
## manim (SEARCHNET_PYTHON), ffmpeg on PATH.
###############################################################################

args <- commandArgs(trailingOnly = TRUE)
export_only <- "--export-only" %in% args
out_dir <- if ("--out" %in% args) args[which(args == "--out") + 1L] else
  file.path(tempdir(), "all_actors_gif")
FPS <- if ("--fps" %in% args) as.integer(args[which(args == "--fps") + 1L]) else 10L

if (file.exists("DESCRIPTION") &&
    any(grepl("^Package: searchnet", readLines("DESCRIPTION", n = 1)))) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressMessages(library(searchnet))
}

## ---- 1. the setting: identical to the README run -------------------------
M <- 6L; N <- 8L
W <- saomnk_block_diagonal(N, 2)
ENV_SEED <- 42L
## Seed rule, fixed before looking at the animation: the README run seed
## 12345, unless the two-actor seed 12351 has clearly more movement (at least
## 25% more portfolio changes in total).
README_SEED <- 12345L; TWO_ACTOR_SEED <- 12351L

configs <- as.matrix(expand.grid(rep(list(0:1), N)))
colnames(configs) <- NULL
row_of <- function(x) 1L + sum(x * 2L^(seq_len(N) - 1L))

run_once <- function(run_seed) {
  env <- saomnk_env(M = M, N = N, density = 0.15, seed = ENV_SEED)
  mod <- saomnk_model(density = -2.5, popularity = 0.2,
                      influence_matrix = W, influence_weight = 0.8)
  invisible(capture.output(suppressMessages(
    saomnk_run(env, mod, steps_per_actor = 10, seed = run_seed))))
  env
}
n_moves <- function(env) {
  ch <- env$bi_env_changes
  tog <- !is.na(ch[, "actor_i"]) & !is.na(ch[, "comp_j"])
  tabulate(ch[tog, "actor_i"], nbins = M)
}

env_a <- run_once(README_SEED); mv_a <- n_moves(env_a)
env_b <- run_once(TWO_ACTOR_SEED); mv_b <- n_moves(env_b)
cat(sprintf("run seed %d: %d portfolio changes (per actor %s)\n", README_SEED, sum(mv_a), paste(mv_a, collapse = " ")))
cat(sprintf("run seed %d: %d portfolio changes (per actor %s)\n", TWO_ACTOR_SEED, sum(mv_b), paste(mv_b, collapse = " ")))
if (sum(mv_b) >= 1.25 * sum(mv_a)) {
  env <- env_b; RUN_SEED <- TWO_ACTOR_SEED
} else {
  env <- env_a; RUN_SEED <- README_SEED
}
cat(sprintf("chosen: run seed %d\n", RUN_SEED))

## ---- 2. gate: the package's recorded utilities are reproduced ------------
arr <- env$bi_env_arr; n_t <- dim(arr)[3]; ch <- env$bi_env_changes
theta <- env$theta_matrix
theta_at <- function(t) theta[min(t, nrow(theta)), ]
B0 <- if (!is.null(env$path_start_matrix)) env$path_start_matrix else env$bipartite_matrix_init
prep <- env$prepare_struct_mod_stats()
ud <- as.data.frame(env$actor_util_df)
rec_util <- matrix(NA_real_, n_t, M)
for (t in seq_len(n_t)) {
  s <- env$get_struct_mod_stats_mat_from_bi_mat(arr[, , t], .prep = prep)
  u <- as.numeric(s %*% theta_at(t))
  sel <- ud$chain_step_id == t
  rec <- ud$utility[sel][order(as.integer(as.character(ud$actor_id[sel])))]
  if (length(rec) != M || max(abs(u - rec)) > 1e-6)
    stop(sprintf("gate: ministep %d: recomputed utilities do not match actor_util_df", t))
  rec_util[t, ] <- rec
}
prev <- B0
for (t in seq_len(n_t)) {
  d <- arr[, , t] - prev
  i <- ch[t, "actor_i"]; j <- ch[t, "comp_j"]
  if (sum(abs(d)) > 1 ||
      (sum(abs(d)) == 1 && (is.na(i) || is.na(j) || d[i, j] == 0)))
    stop(sprintf("gate: ministep %d is not the recorded single toggle", t))
  prev <- arr[, , t]
}

## ---- 3. every actor's landscape at every ministep ------------------------
f_all <- function(state, i, t) {
  th <- theta_at(t)
  apply(configs, 1L, function(x) {
    s <- state; s[i, ] <- x
    sum(env$get_struct_mod_stats_mat_from_bi_mat(s, .prep = prep)[i, ] * th)
  })
}
land <- lapply(seq_len(M), function(i) {
  L <- matrix(NA_real_, n_t + 1L, nrow(configs))
  L[1L, ] <- f_all(B0, i, 1L)
  for (t in seq_len(n_t)) L[t + 1L, ] <- f_all(arr[, , t], i, t)
  L
})
rows <- lapply(seq_len(M), function(i)
  c(row_of(B0[i, ]), sapply(seq_len(n_t), function(t) row_of(arr[i, , t]))))
## each landscape, read at the realized portfolio, is the recorded utility,
## for every actor at every ministep
for (i in seq_len(M)) {
  got <- land[[i]][cbind(2:(n_t + 1L), rows[[i]][-1L])]
  if (max(abs(got - rec_util[, i])) > 1e-6)
    stop(sprintf("gate: actor %d's landscape misses its recorded utility", i))
}
cat(sprintf("gate passed: %d ministeps x %d actors, every landscape reproduces the recorded utility\n",
            n_t, M))

steps <- vector("list", n_t)
prev <- B0
for (t in seq_len(n_t)) {
  i <- ch[t, "actor_i"]; j <- ch[t, "comp_j"]
  delta <- if (is.na(i) || is.na(j)) 0 else arr[i, j, t] - prev[i, j]
  if (is.na(i)) i <- as.integer(env$chain_stats$id_from[t])
  steps[[t]] <- list(t = t, actor = i, comp = if (is.na(j)) 0L else j,
                     change = if (delta > 0) "add" else if (delta < 0) "drop" else "none")
  prev <- arr[, , t]
}

out <- list(
  M = M, N = N, W = unname(W),
  start = unname(B0),
  steps = steps,
  landscapes = unname(lapply(land, unname)),
  rows = rows,
  utilities = unname(rec_util),
  seeds = list(env = ENV_SEED, run = RUN_SEED)
)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
work <- file.path(out_dir, "work")
dir.create(work, showWarnings = FALSE, recursive = TRUE)
json <- file.path(work, "all_actors_landscape.json")
jsonlite::write_json(out, json, auto_unbox = TRUE, digits = 8)
cat("wrote", json, "\n")
if (export_only) quit(status = 0)

## ---- 4. render -------------------------------------------------------------
py    <- Sys.getenv("SEARCHNET_PYTHON", "python")
scene <- normalizePath(file.path("inst", "manim", "scene_all_actors_landscape.py"))
Sys.setenv(SEARCHNET_ALL_ACTORS_JSON = json)
status <- system2(py, c("-m", "manim", "render", "--media_dir", shQuote(work),
                        "-r", "1440,810", "--fps", FPS, "--disable_caching",
                        ## -t: lossless qtrle .mov (see make_two_actor_gif.R)
                        "-t",
                        "-o", "all_actors_landscape", shQuote(scene), "AllActorsLandscapeScene"))
if (status != 0) stop("manim render failed")
mov <- list.files(work, pattern = "^all_actors_landscape[.]mov$", recursive = TRUE, full.names = TRUE)[1]
if (is.na(mov)) stop("manim output not found under ", work)

gif <- file.path(out_dir, "all-actors-landscape.gif")
pal <- file.path(work, "palette.png")
system2("ffmpeg", c("-v", "error", "-y", "-i", shQuote(mov),
                    "-vf", shQuote(sprintf("fps=%d,palettegen=max_colors=256:stats_mode=diff", FPS)), shQuote(pal)))
system2("ffmpeg", c("-v", "error", "-y", "-i", shQuote(mov), "-i", shQuote(pal),
                    "-lavfi", shQuote(sprintf("fps=%d[x];[x][1:v]paletteuse=dither=none:diff_mode=rectangle", FPS)),
                    "-loop", "0", shQuote(gif)))
cat(sprintf("wrote %s (%.0f KB)\n", gif, file.size(gif) / 1024))
