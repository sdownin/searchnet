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
##                                         [--shared=entrant|average]
##
## Without --shared the scene draws the six grids described below, unchanged.
## With --shared it draws ONE grid with all six tokens on it
## (SharedLandscapeScene in the same scene file):
##   entrant  each portfolio colored by the objective a hypothetical seventh
##            actor with the same parameters would get from it, given ALL six
##            actors' current ties, computed by the package's statistics
##            function on the 7-row state. Actor i's own landscape differs
##            from it only through actor i's own ties, which the entrant
##            counts as other holders; the script checks that difference
##            against its prediction and reports its largest value.
##   average  the mean of the six actors' own landscapes, with the largest
##            deviation of any actor's own landscape from that mean.
## The trajectories on the left stay each actor's OWN objective.
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
SHARED <- NA_character_
if (any(grepl("^--shared=", args))) SHARED <- sub("^--shared=", "", args[grepl("^--shared=", args)][1])
if ("--shared" %in% args) SHARED <- args[which(args == "--shared") + 1L]
if (!is.na(SHARED) && !SHARED %in% c("entrant", "average"))
  stop("--shared must be 'entrant' or 'average'")
## render size: the six-grid scene keeps 1440 x 810; the single shared grid
## reads as well at 1280 x 720 (same 16:9 frame, so the layout is identical)
RES <- if (is.na(SHARED)) "1440,810" else "1280,720"

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

## ---- 3b. optional: one shared landscape ---------------------------------
shared <- NULL
if (!is.na(SHARED)) {
  states <- c(list(B0), lapply(seq_len(n_t), function(t) arr[, , t]))
  if (SHARED == "entrant") {
    ## The package's statistics on the 7-row state: the six actors plus the
    ## entrant in row 7. The effects in this model (density, inPop, XWX) read
    ## only the matrix, so the one thing that changes is the empty statistics
    ## template, which gets one zero row more.
    eff <- prep$theta_df$shortName
    if (!all(eff %in% c("density", "inPop", "XWX")))
      stop("entrant view: the check below covers density, inPop and XWX only")
    prep7 <- prep
    prep7$mat_template <- rbind(prep$mat_template, 0)
    rownames(prep7$mat_template) <- seq_len(M + 1L)
    ## sanity: an entrant holding nothing leaves the six actors' statistics as they are
    for (t in c(1L, n_t)) {
      s6 <- env$get_struct_mod_stats_mat_from_bi_mat(arr[, , t], .prep = prep)
      s7 <- env$get_struct_mod_stats_mat_from_bi_mat(rbind(arr[, , t], 0), .prep = prep7)
      if (max(abs(s7[1:M, ] - s6)) > 1e-9)
        stop("entrant view: an empty entrant changed the actors' statistics")
    }
    f_ent <- function(state, t) {
      th <- theta_at(t)
      apply(configs, 1L, function(x)
        sum(env$get_struct_mod_stats_mat_from_bi_mat(rbind(state, x), .prep = prep7)[M + 1L, ] * th))
    }
    S <- matrix(NA_real_, n_t + 1L, nrow(configs))
    for (k in seq_len(n_t + 1L)) S[k, ] <- f_ent(states[[k]], max(1L, k - 1L))
    ## check: entrant(x) - own_i(x) = theta_inPop * sum_j x_j b_ij. The
    ## entrant counts actor i among the holders of each component; actor i,
    ## standing at x itself, does not count its current ties b_i. density and
    ## XWX depend on ego's own row only.
    k_pop <- which(eff == "inPop")
    dev <- matrix(NA_real_, n_t + 1L, M)
    for (k in seq_len(n_t + 1L)) {
      th_pop <- theta_at(max(1L, k - 1L))[k_pop]
      for (i in seq_len(M)) {
        d_obs  <- S[k, ] - land[[i]][k, ]
        d_pred <- th_pop * as.numeric(configs %*% states[[k]][i, ])
        if (max(abs(d_obs - d_pred)) > 1e-9)
          stop(sprintf("entrant check: ministep %d, actor %d: the difference is not its own-holdings effect",
                       k - 1L, i))
        dev[k, i] <- max(abs(d_obs))
      }
    }
    cat("entrant check passed: entrant - own_i = theta_inPop * (x . b_i) at every portfolio, actor and ministep\n")
  } else {
    S <- Reduce(`+`, land) / M
    dev <- matrix(NA_real_, n_t + 1L, M)
    for (k in seq_len(n_t + 1L)) for (i in seq_len(M))
      dev[k, i] <- max(abs(land[[i]][k, ] - S[k, ]))
  }
  max_dev <- max(dev)
  cat(sprintf("shared (%s): max |shown - own| over portfolios and actors, per ministep:\n", SHARED))
  print(round(apply(dev, 1L, max), 3))
  cat(sprintf("shared (%s): overall max |shown - own| = %.4f\n", SHARED, max_dev))
  shared <- list(mode = SHARED, landscape = unname(S), max_dev = max_dev,
                 max_dev_by_step = unname(apply(dev, 1L, max)))
}

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
if (!is.null(shared)) out$shared <- shared

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
work <- file.path(out_dir, "work")
dir.create(work, showWarnings = FALSE, recursive = TRUE)
stem <- if (is.na(SHARED)) "all_actors_landscape" else paste0("all_actors_shared_", SHARED)
json <- file.path(work, paste0(stem, ".json"))
jsonlite::write_json(out, json, auto_unbox = TRUE, digits = 8)
cat("wrote", json, "\n")
if (export_only) quit(status = 0)

## ---- 4. render -------------------------------------------------------------
py    <- Sys.getenv("SEARCHNET_PYTHON", "python")
scene <- normalizePath(file.path("inst", "manim", "scene_all_actors_landscape.py"))
Sys.setenv(SEARCHNET_ALL_ACTORS_JSON = json)
status <- system2(py, c("-m", "manim", "render", "--media_dir", shQuote(work),
                        "-r", RES, "--fps", FPS, "--disable_caching",
                        ## -t: lossless qtrle .mov (see make_two_actor_gif.R)
                        "-t",
                        "-o", stem, shQuote(scene),
                        if (is.na(SHARED)) "AllActorsLandscapeScene" else "SharedLandscapeScene"))
if (status != 0) stop("manim render failed")
mov <- list.files(work, pattern = paste0("^", stem, "[.]mov$"), recursive = TRUE, full.names = TRUE)[1]
if (is.na(mov)) stop("manim output not found under ", work)

gif <- file.path(out_dir, if (is.na(SHARED)) "all-actors-landscape.gif" else
  sprintf("all-actors-shared-%s.gif", SHARED))
pal <- file.path(work, if (is.na(SHARED)) "palette.png" else paste0(stem, "_palette.png"))
system2("ffmpeg", c("-v", "error", "-y", "-i", shQuote(mov),
                    "-vf", shQuote(sprintf("fps=%d,palettegen=max_colors=256:stats_mode=diff", FPS)), shQuote(pal)))
system2("ffmpeg", c("-v", "error", "-y", "-i", shQuote(mov), "-i", shQuote(pal),
                    "-lavfi", shQuote(sprintf("fps=%d[x];[x][1:v]paletteuse=dither=none:diff_mode=rectangle", FPS)),
                    "-loop", "0", shQuote(gif)))
cat(sprintf("wrote %s (%.0f KB)\n", gif, file.size(gif) / 1024))
