#!/usr/bin/env Rscript
###############################################################################
## make_two_actor_gif.R
##
## A two-actor companion to tools/make_readme_gif.R (which it leaves
## untouched): the same seeded setting, but two focal actors are followed at
## once, each on its own fitness landscape.
##
## Usage, from the package root:
##
##     Rscript tools/make_two_actor_gif.R [--export-only] [--out DIR]
##
## The GIF is written to DIR (default: the session's tempdir), never into the
## package; it is a draft for review, not a README figure.
##
## Why two grids. Each actor's objective f_i(x) = s_i(x)' theta depends on the
## other actors' ties, including the other focal actor's, so the two actors do
## not share a landscape. Their portfolios do live in the same space (the 2^N
## subsets of the components), so the scene draws two grids on the same axes,
## each colored by one actor's own landscape, with both actors' current
## portfolios marked on both (own token solid, the other's hollow). Landscapes
## are never averaged.
##
## Stages:
##   1. R (this script): a deterministic search over run seeds for a pair of
##      actors that visibly interact (see "selection" below); the gate that
##      every recorded utility is reproduced by the package's own statistics
##      function; each focal actor's objective over all 2^N portfolios at every
##      ministep, the other actors' ties held at that ministep's state.
##   2. manim: inst/manim/scene_two_actor_landscape.py renders the JSON to a
##      lossless .mov.
##   3. ffmpeg: palette-optimized GIF (two-pass palettegen / paletteuse).
##
## Requirements as for make_readme_gif.R: searchnet, jsonlite, Python with
## manim (SEARCHNET_PYTHON), ffmpeg on PATH.
###############################################################################

args <- commandArgs(trailingOnly = TRUE)
export_only <- "--export-only" %in% args
out_dir <- if ("--out" %in% args) args[which(args == "--out") + 1L] else
  file.path(tempdir(), "two_actor_gif")

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
## README run seed first; later seeds are tried, in order, only if no pair in
## an earlier run meets the selection rule.
RUN_SEEDS <- 12345L + 0:7

configs <- as.matrix(expand.grid(rep(list(0:1), N)))
colnames(configs) <- NULL
row_of <- function(x) 1L + sum(x * 2L^(seq_len(N) - 1L))
flip <- outer(seq_len(nrow(configs)) - 1L, seq_len(N) - 1L,
              function(r, b) bitwXor(r, bitwShiftL(1L, b))) + 1L

run_once <- function(run_seed) {
  env <- saomnk_env(M = M, N = N, density = 0.15, seed = ENV_SEED)
  mod <- saomnk_model(density = -2.5, popularity = 0.2,
                      influence_matrix = W, influence_weight = 0.8)
  invisible(capture.output(suppressMessages(
    saomnk_run(env, mod, steps_per_actor = 10, seed = run_seed))))
  env
}

landscapes_of <- function(env, actors) {
  arr <- env$bi_env_arr; n_t <- dim(arr)[3]; theta <- env$theta_matrix
  B0 <- if (!is.null(env$path_start_matrix)) env$path_start_matrix else env$bipartite_matrix_init
  prep <- env$prepare_struct_mod_stats()
  f_all <- function(state, i, t) {
    th <- theta[min(t, nrow(theta)), ]
    apply(configs, 1L, function(x) {
      s <- state; s[i, ] <- x
      sum(env$get_struct_mod_stats_mat_from_bi_mat(s, .prep = prep)[i, ] * th)
    })
  }
  lapply(setNames(actors, actors), function(i) {
    L <- matrix(NA_real_, n_t + 1L, nrow(configs))
    L[1L, ] <- f_all(B0, i, 1L)
    for (t in seq_len(n_t)) L[t + 1L, ] <- f_all(arr[, , t], i, t)
    L
  })
}


## ---- 2. selection: a pair that visibly interacts ---------------------------
## For each run seed in order, candidate pairs (a, b) must
##   (i)  both change their portfolio at least 6 times (both tokens travel),
##   (ii) come within one component of each other at some ministep
##        (Hamming distance <= 1 between their portfolios), and
##   (iii) each must reshape the other's landscape where the other stands:
##        at least 2 of a's own toggles change which single add/drop would
##        raise b's objective from b's current portfolio (b's uphill
##        directions), and at least 2 of b's toggles do the same for a.
## (The set of local peaks is not used: under these coefficients it is the
## same four portfolios at every state, as noted in make_readme_gif.R, so the
## other actors move only the heights.)
## Among qualifying pairs the one with the largest min(reshapes a->b, b->a)
## wins (ties: more moves in total, then lower ids); the first seed with a
## qualifying pair is used.
pick <- NULL
for (rs in RUN_SEEDS) {
  env <- run_once(rs)
  arr <- env$bi_env_arr; ch <- env$bi_env_changes; n_t <- dim(arr)[3]
  B0 <- if (!is.null(env$path_start_matrix)) env$path_start_matrix else env$bipartite_matrix_init
  states <- c(list(B0), lapply(seq_len(n_t), function(t) arr[, , t]))
  toggled <- !is.na(ch[, "actor_i"]) & !is.na(ch[, "comp_j"])
  moves <- tabulate(ch[toggled, "actor_i"], nbins = M)
  cand <- list()
  for (a in 1:(M - 1L)) for (b in (a + 1L):M) {
    if (min(moves[a], moves[b]) < 6L) next
    ham <- sapply(states, function(s) sum(s[a, ] != s[b, ]))
    if (min(ham) > 1L) next
    cand[[length(cand) + 1L]] <- c(a = a, b = b, minham = min(ham))
  }
  if (!length(cand)) { cat(sprintf("run seed %d: no candidate pair\n", rs)); next }
  acts <- sort(unique(unlist(lapply(cand, `[`, c("a", "b")))))
  L <- landscapes_of(env, acts)
  rows_of <- function(i) sapply(states, function(st) row_of(st[i, ]))
  uphill <- function(f, r) which(f[flip[r, ]] > f[r] + 1e-9)
  reshapes <- function(from, to) {
    Lt <- L[[as.character(to)]]; rt <- rows_of(to)
    ts <- which(toggled & ch[, "actor_i"] == from)
    ## state index k = t + 1; 'to' did not move at t, so rt[t] == rt[t + 1]
    sum(sapply(ts, function(t) !identical(uphill(Lt[t, ], rt[t]),
                                           uphill(Lt[t + 1L, ], rt[t + 1L]))))
  }
  tab <- do.call(rbind, lapply(cand, function(p) {
    a <- p[["a"]]; b <- p[["b"]]
    c(p, ma = moves[a], mb = moves[b], ab = reshapes(a, b), ba = reshapes(b, a))
  }))
  cat(sprintf("run seed %d candidates:\n", rs)); print(tab)
  ok <- tab[pmin(tab[, "ab"], tab[, "ba"]) >= 2L, , drop = FALSE]
  if (nrow(ok)) {
    o <- order(-pmin(ok[, "ab"], ok[, "ba"]), -(ok[, "ma"] + ok[, "mb"]), ok[, "a"], ok[, "b"])
    pick <- list(seed = rs, row = ok[o[1], ], env = env, L = L)
    break
  }
}
if (is.null(pick)) stop("no run seed in RUN_SEEDS yields a qualifying pair")
env <- pick$env
focals <- as.integer(pick$row[c("a", "b")])
cat(sprintf("chosen: run seed %d, actors %d and %d (moves %d, %d; min Hamming %d; uphill-set changes %d->%d: %d, %d->%d: %d)\n",
            pick$seed, focals[1], focals[2], pick$row[["ma"]], pick$row[["mb"]],
            pick$row[["minham"]], focals[1], focals[2], pick$row[["ab"]],
            focals[2], focals[1], pick$row[["ba"]]))

## ---- 3. gate: the package's recorded utilities are reproduced ------------
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
## each focal landscape, read at the realized portfolio, is the recorded utility
land <- pick$L[as.character(focals)]
rows <- lapply(focals, function(i)
  c(row_of(B0[i, ]), sapply(seq_len(n_t), function(t) row_of(arr[i, , t]))))
for (k in 1:2) {
  got <- land[[k]][cbind(2:(n_t + 1L), rows[[k]][-1L])]
  if (max(abs(got - rec_util[, focals[k]])) > 1e-6)
    stop(sprintf("gate: actor %d's landscape misses its recorded utility", focals[k]))
}
cat(sprintf("gate passed: %d ministeps, %d actors\n", n_t, M))

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
  M = M, N = N, focals = focals, W = unname(W),
  start = unname(B0),
  states = lapply(seq_len(n_t), function(t) unname(arr[, , t])),
  steps = steps,
  landscapes = unname(lapply(land, unname)),
  rows = rows,
  seeds = list(env = ENV_SEED, run = pick$seed),
  selection = as.list(pick$row)
)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
work <- file.path(out_dir, "work")
dir.create(work, showWarnings = FALSE, recursive = TRUE)
json <- file.path(work, "two_actor_landscape.json")
jsonlite::write_json(out, json, auto_unbox = TRUE, digits = 8)
cat("wrote", json, "\n")
if (export_only) quit(status = 0)

## ---- 4. render -------------------------------------------------------------
py    <- Sys.getenv("SEARCHNET_PYTHON", "python")
scene <- normalizePath(file.path("inst", "manim", "scene_two_actor_landscape.py"))
Sys.setenv(SEARCHNET_TWO_ACTOR_JSON = json)
status <- system2(py, c("-m", "manim", "render", "--media_dir", shQuote(work),
                        "-r", "1100,560", "--fps", "10", "--disable_caching",
                        ## -t: lossless qtrle .mov instead of H.264 .mp4. The scene paints
                        ## an opaque white backdrop, so nothing is transparent; the point
                        ## is that encoder noise in an .mp4 makes nearly every GIF frame a
                        ## full-frame update (4.5 MB here, against 0.8 MB from the .mov).
                        "-t",
                        "-o", "two_actor_landscape", shQuote(scene), "TwoActorLandscapeScene"))
if (status != 0) stop("manim render failed")
mov <- list.files(work, pattern = "^two_actor_landscape[.]mov$", recursive = TRUE, full.names = TRUE)[1]
if (is.na(mov)) stop("manim output not found under ", work)

gif <- file.path(out_dir, "two-actor-landscape.gif")
pal <- file.path(work, "palette.png")
system2("ffmpeg", c("-v", "error", "-y", "-i", shQuote(mov),
                    "-vf", shQuote("fps=10,palettegen=max_colors=256:stats_mode=diff"), shQuote(pal)))
system2("ffmpeg", c("-v", "error", "-y", "-i", shQuote(mov), "-i", shQuote(pal),
                    "-lavfi", shQuote("fps=10[x];[x][1:v]paletteuse=dither=none:diff_mode=rectangle"),
                    "-loop", "0", shQuote(gif)))
cat(sprintf("wrote %s (%.0f KB)\n", gif, file.size(gif) / 1024))
