#!/usr/bin/env Rscript
###############################################################################
## make_architecture_gifs.R
##
## Review drafts: the README landscape animation (tools/make_readme_gif.R) for
## the four influence-matrix architectures of the README architectures figure
## (tools/figures_shared.R, fs_fig_architectures), at N = 8, each shown with
## three right-hand panels that do not depend on W having two blocks:
##
##   FittedGridScene   the 16 x 16 grid with its row/column split and axis
##                     order chosen from W (minimum cross-split |w|)
##   CompassScene      the eight single moves open to the focal actor, colored
##                     by the change in f each would produce
##   WOverlayScene     W with the focal actor's held block lit, each lit cell
##                     colored by its contribution to f
##
## Usage, from the package root:
##
##     Rscript tools/make_architecture_gifs.R DIR [--export-only] [--only=ring,random]
##                                               [--views=grid,compass,overlay]
##                                               [--raw-weight]
##
## Writes DIR/<arch>_<view>.gif (12 files) and the JSON per architecture. The
## README GIF and man/figures are not touched.
##
## The data stage is the one in tools/make_readme_gif.R, repeated per W: the
## same model (density -2.5, popularity 0.2), the same seeds (env 42, run
## 12345), the same focal-actor rule, the same landscape computation through
## the package's own statistics function, and the same two gates (recomputed
## utilities equal actor_util_df; each ministep is the recorded toggle).
##
## EQUAL TOTAL WEIGHT (the default). The four W differ in how much
## off-diagonal weight they carry, not only in where it sits: at N = 8 the
## two-module W has 24 unit cells, the nested W 24 cells summing to 10.67,
## and the ring and random W 16 unit cells each. At one common XWX weight,
## an architecture with more total |w| would simply have stronger
## complementarity, and differences in the landscape (its peaks, the size of
## the compass deltas, the lit cells of the overlay) would mix WHERE W puts
## interdependence with HOW MUCH of it there is. The comparison is about the
## first, so the XWX weight is scaled so that every W carries the same total
## off-diagonal weight as the two-module W at weight 0.8 (24 x 0.8 = 19.2):
##
##     weight = 0.8 * 24 / sum_{h != j} |w_hj|
##
## which gives modular 0.8, nested 1.8, ring 1.2 and random 1.2. This is the
## author's decision for the comparison and the default here; --raw-weight
## uses 0.8 for every W instead, for checking how much the scaling matters.
## vignette("searchnet-landscape-views") makes the same comparison.
## It additionally exports, for the focal actor's realized portfolio at every
## state, the per-effect parts of f (density, popularity, XWX), and gates that
## they sum to the landscape value.
###############################################################################

args <- commandArgs(trailingOnly = TRUE)
export_only <- "--export-only" %in% args
only <- sub("^--only=", "", grep("^--only=", args, value = TRUE)[1])
view_only <- sub("^--views=", "", grep("^--views=", args, value = TRUE)[1])
raw_weight <- "--raw-weight" %in% args   # default FALSE: equal total off-diagonal weight
out_dir <- args[!grepl("^--", args)][1]
if (is.na(out_dir)) stop("usage: Rscript tools/make_architecture_gifs.R DIR [--export-only] [--only=a,b]")

if (file.exists("DESCRIPTION") &&
    any(grepl("^Package: searchnet", readLines("DESCRIPTION", n = 1)))) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressMessages(library(searchnet))
}

M <- 6L; N <- 8L
offdiag <- function(W) { W <- abs(W); diag(W) <- 0; sum(W) }
## The four architectures of fs_fig_architectures(), at N = 8.
archs <- list(
  modular = list(label = "modular (two blocks)",
                 W = saomnk_block_diagonal(N, 2)),
  nested  = list(label = "nested modules",
                 W = (saomnk_block_diagonal(N, 2) + saomnk_block_diagonal(N, 4) + diag(N)) / 3),
  ring    = list(label = "local (ring, K = 2)",
                 W = nk_to_saomnk(nk_landscape(N, 2, model = "adjacent", seed = 1))$influence_matrix),
  random  = list(label = "random (K = 2)",
                 W = nk_to_saomnk(nk_landscape(N, 2, model = "random", seed = 1))$influence_matrix)
)
if (!is.na(only)) archs <- archs[strsplit(only, ",")[[1]]]
## Equal total off-diagonal weight across architectures (see the header):
## every W is scaled to the two-module W's 24 x 0.8 = 19.2.
ref_total <- 0.8 * offdiag(saomnk_block_diagonal(N, 2))

configs <- as.matrix(expand.grid(rep(list(0:1), N)))
colnames(configs) <- NULL
row_of <- function(x) 1L + sum(x * 2L^(seq_len(N) - 1L))

export_arch <- function(name, spec, work) {
  W <- unname(as.matrix(spec$W)); storage.mode(W) <- "double"
  wt <- if (raw_weight) 0.8 else ref_total / offdiag(W)
  env <- saomnk_env(M = M, N = N, density = 0.15, seed = 42)
  mod <- saomnk_model(density = -2.5, popularity = 0.2,
                      influence_matrix = W, influence_weight = wt)
  invisible(capture.output(suppressMessages(
    saomnk_run(env, mod, steps_per_actor = 10, seed = 12345))))

  arr <- env$bi_env_arr; n_t <- dim(arr)[3]
  ch <- env$bi_env_changes; theta <- env$theta_matrix
  B0 <- if (!is.null(env$path_start_matrix)) env$path_start_matrix else env$bipartite_matrix_init
  theta_at <- function(t) theta[min(t, nrow(theta)), ]
  moves <- tabulate(ch[, "actor_i"], nbins = M)
  focal <- which.max(moves)
  prep <- env$prepare_struct_mod_stats()
  stats <- function(s) env$get_struct_mod_stats_mat_from_bi_mat(s, .prep = prep)
  f_all <- function(state, i, t) {
    th <- theta_at(t)
    apply(configs, 1L, function(x) { s <- state; s[i, ] <- x; sum(stats(s)[i, ] * th) })
  }

  ## gates, as in make_readme_gif.R
  ud <- as.data.frame(env$actor_util_df)
  for (t in seq_len(n_t)) {
    u <- as.numeric(stats(arr[, , t]) %*% theta_at(t))
    rec <- ud$utility[ud$chain_step_id == t][order(as.integer(as.character(
      ud$actor_id[ud$chain_step_id == t])))]
    if (length(rec) != M || max(abs(u - rec)) > 1e-6)
      stop(sprintf("gate (%s): ministep %d: recomputed utilities do not match actor_util_df", name, t))
  }
  prev <- B0
  for (t in seq_len(n_t)) {
    d <- arr[, , t] - prev; i <- ch[t, "actor_i"]; j <- ch[t, "comp_j"]
    if (sum(abs(d)) > 1 || (sum(abs(d)) == 1 && (is.na(i) || is.na(j) || d[i, j] == 0)))
      stop(sprintf("gate (%s): ministep %d is not the recorded single toggle", name, t))
    prev <- arr[, , t]
  }

  land <- matrix(NA_real_, n_t + 1L, nrow(configs))
  land[1L, ] <- f_all(B0, focal, 1L)
  for (t in seq_len(n_t)) land[t + 1L, ] <- f_all(arr[, , t], focal, t)

  prev <- B0; steps <- vector("list", n_t)
  for (t in seq_len(n_t)) {
    i <- ch[t, "actor_i"]; j <- ch[t, "comp_j"]
    delta <- if (is.na(i) || is.na(j)) 0 else arr[i, j, t] - prev[i, j]
    if (is.na(i)) i <- as.integer(env$chain_stats$id_from[t])
    steps[[t]] <- list(t = t, actor = i, comp = if (is.na(j)) 0L else j,
                       change = if (delta > 0) "add" else if (delta < 0) "drop" else "none")
    prev <- arr[, , t]
  }
  focal_row <- c(row_of(B0[focal, ]), sapply(seq_len(n_t), function(t) row_of(arr[focal, , t])))

  ## per-effect parts of f at the focal actor's realized portfolio, gated
  st_list <- c(list(B0), lapply(seq_len(n_t), function(t) arr[, , t]))
  parts <- t(sapply(seq_along(st_list), function(k)
    stats(st_list[[k]])[focal, ] * theta_at(max(1L, k - 1L))))
  realized <- land[cbind(seq_len(n_t + 1L), focal_row)]
  if (max(abs(rowSums(parts) - realized)) > 1e-6)
    stop(sprintf("gate (%s): per-effect parts do not sum to the landscape value", name))

  k_of <- function(b) { S <- b %*% t(b); diag(S) <- 0
    c(K_AC = mean(rowSums(b)), K_AA = mean(rowSums(S > 0))) }
  kt <- rbind(k_of(B0), t(apply(arr, 3L, k_of)))
  th1 <- theta_at(1L)
  cat(sprintf("%s: gates passed; %d ministeps; focal actor %d moves %d times; XWX weight %.3f\n",
              name, n_t, focal, moves[focal], th1[["XWX"]]))

  out <- list(
    M = M, N = N, focal = focal, W = W, arch = name, arch_label = spec$label,
    theta = as.list(th1), start = unname(B0),
    states = lapply(seq_len(n_t), function(t) unname(arr[, , t])),
    steps = steps, landscape = unname(land), focal_row = focal_row,
    parts = unname(parts), part_names = colnames(parts),
    K_AC = unname(kt[, "K_AC"]), K_AA = unname(kt[, "K_AA"]),
    seeds = list(env = 42L, run = 12345L))
  json <- file.path(work, paste0(name, ".json"))
  jsonlite::write_json(out, json, auto_unbox = TRUE, digits = 10)
  json
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
work <- Sys.getenv("SEARCHNET_GIF_WORKDIR", file.path(tempdir(), "arch_gif"))
dir.create(work, showWarnings = FALSE, recursive = TRUE)
jsons <- vapply(names(archs), function(n) export_arch(n, archs[[n]], out_dir), "")
if (export_only) quit(status = 0)

py <- Sys.getenv("SEARCHNET_PYTHON", "python")
render_gif <- function(cls, out_name, gif, colors = 64L) {
  scene <- normalizePath(file.path("inst", "manim", "scene_architecture_views.py"))
  status <- system2(py, c("-m", "manim", "render", "--media_dir", shQuote(work),
                          "-r", "880,520", "--fps", "10", "--disable_caching",
                          "-o", out_name, shQuote(scene), cls))
  if (status != 0) stop("manim render failed")
  mp4 <- list.files(work, pattern = paste0("^", out_name, "[.]mp4$"), recursive = TRUE,
                    full.names = TRUE)[1]
  if (is.na(mp4)) stop("manim output not found under ", work)
  pal <- file.path(work, "palette.png")
  system2("ffmpeg", c("-v", "error", "-y", "-i", shQuote(mp4),
                      "-vf", shQuote(sprintf("fps=10,palettegen=max_colors=%d:stats_mode=diff", colors)),
                      shQuote(pal)))
  system2("ffmpeg", c("-v", "error", "-y", "-i", shQuote(mp4), "-i", shQuote(pal),
                      "-lavfi", shQuote("fps=10[x];[x][1:v]paletteuse=dither=none:diff_mode=rectangle"),
                      "-loop", "0", shQuote(gif)))
  cat(sprintf("wrote %s (%.0f KB)\n", gif, file.size(gif) / 1024))
}

views <- c(grid = "FittedGridScene", compass = "CompassScene", overlay = "WOverlayScene")
if (!is.na(view_only)) views <- views[strsplit(view_only, ",")[[1]]]
for (n in names(jsons)) {
  Sys.setenv(SEARCHNET_README_JSON = normalizePath(jsons[[n]]))
  for (v in names(views))
    render_gif(views[[v]], paste0(n, "_", v), file.path(out_dir, paste0(n, "_", v, ".gif")),
               colors = if (v == "compass") 48L else 64L)   # compass at 64 colors passes 3 MB
}
