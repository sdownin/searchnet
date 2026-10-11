###############################################################################
## landscape-views.R
##
## Landscape views that work for any influence matrix W:
##
##   searchnet_move_compass()      the N single moves (add or drop one
##                                 component) open to one actor at one state,
##                                 with the change in f each would produce
##   searchnet_actor_landscape()   f for every one of the 2^N portfolios the
##                                 actor could hold at that state, with the
##                                 local peaks marked (small N only)
##
## Both evaluate f through the engine's own statistic,
## get_struct_mod_stats_mat_from_bi_mat(), times the parameter row in force at
## that step, holding every other actor's portfolio fixed. They are the data
## behind the compass and the W-fitted grid of
## vignette("searchnet-landscape-views") and tools/make_architecture_gifs.R.
###############################################################################

## The state of a run after ministep `step` (0 = the start of the path) and
## the parameter row that scores it: row max(1, step) of theta_matrix, as in
## tools/make_architecture_gifs.R.
.sn_view_state <- function(env, step = NULL) {
  arr <- env$bi_env_arr
  if (is.null(arr)) stop("env has no simulated path; run saomnk_run() first", call. = FALSE)
  n_t <- dim(arr)[3]
  if (is.null(step)) step <- n_t
  step <- as.integer(step)
  if (length(step) != 1L || is.na(step) || step < 0L || step > n_t)
    stop(sprintf("step must be a single integer in 0..%d", n_t), call. = FALSE)
  B <- if (step == 0L) {
    if (!is.null(env$path_start_matrix)) env$path_start_matrix else env$bipartite_matrix_init
  } else arr[, , step]
  theta <- env$theta_matrix
  th <- theta[min(max(step, 1L), nrow(theta)), ]
  list(B = unname(as.matrix(B)), theta = th, step = step)
}

.sn_view_actor <- function(env, actor) {
  actor <- as.integer(actor)
  if (length(actor) != 1L || is.na(actor) || actor < 1L || actor > env$M)
    stop(sprintf("actor must be a single integer in 1..%d", env$M), call. = FALSE)
  actor
}

## f for actor i at each row of `portfolios` (a k x N 0/1 matrix), others fixed.
.sn_view_f <- function(env, B, theta, i, portfolios) {
  prep <- env$prepare_struct_mod_stats()
  apply(portfolios, 1L, function(x) {
    s <- B
    s[i, ] <- x
    sum(env$get_struct_mod_stats_mat_from_bi_mat(s, .prep = prep)[i, ] * theta)
  })
}

.sn_component_labels <- function(N) {
  if (N <= 26L) LETTERS[seq_len(N)] else paste0("C", seq_len(N))
}

#' The moves open to one actor: a move compass
#'
#' For one actor at one state of a simulated path, the change in the actor's
#' objective \eqn{f} that each of its \eqn{N} single moves would produce:
#' adding a component it does not hold, or dropping one it holds, with every
#' other actor's portfolio held fixed. This is the actor's decision view, the
#' data behind the compass in `vignette("searchnet-landscape-views")`. It
#' works for any influence matrix and any \eqn{N}, because it evaluates
#' \eqn{N + 1} portfolios rather than all \eqn{2^N}.
#'
#' The compass is local: it says whether the actor's current portfolio is a
#' local peak (no single move raises \eqn{f}), not how many peaks the
#' landscape has or where they are. For that, see
#' [searchnet_actor_landscape()] (small \eqn{N} only).
#'
#' @param env A `SaomNkRSienaBiEnv` after [saomnk_run()].
#' @param actor Integer, the focal actor (row of the bipartite matrix).
#' @param step Integer ministep: `0` is the start of the path, `t` the state
#'   after ministep `t`. `NULL` (default) is the final state. The state is
#'   scored with the parameter row in force at that step (row `max(1, step)`
#'   of `env$theta_matrix`).
#'
#' @return A data frame with one row per component: `component` (index),
#'   `label`, `held` (0/1 before the move), `move` (`"add"` or `"drop"`),
#'   `f_now`, `f_after`, and `delta = f_after - f_now`. Attributes `step`,
#'   `actor`, and `local_peak` (`TRUE` when no `delta` is positive, up to
#'   `1e-9`).
#'
#' @seealso [searchnet_actor_landscape()]
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 6, N = 8, density = 0.15, seed = 42)
#' mod <- saomnk_model(density = -2.5, popularity = 0.2,
#'                     influence_matrix = saomnk_block_diagonal(8, 2),
#'                     influence_weight = 0.8)
#' invisible(capture.output(saomnk_run(env, mod, steps_per_actor = 4, seed = 1)))
#' searchnet_move_compass(env, actor = 1)
#' }
#' @export
searchnet_move_compass <- function(env, actor, step = NULL) {
  st <- .sn_view_state(env, step)
  i <- .sn_view_actor(env, actor)
  N <- ncol(st$B)
  x <- st$B[i, ]
  P <- rbind(x, t(vapply(seq_len(N), function(j) { y <- x; y[j] <- 1 - y[j]; y }, numeric(N))))
  f <- unname(.sn_view_f(env, st$B, st$theta, i, P))
  out <- data.frame(component = seq_len(N), label = .sn_component_labels(N),
                    held = as.integer(x), move = ifelse(x > 0, "drop", "add"),
                    f_now = f[1L], f_after = f[-1L], delta = f[-1L] - f[1L],
                    stringsAsFactors = FALSE)
  attr(out, "step") <- st$step
  attr(out, "actor") <- i
  attr(out, "local_peak") <- all(out$delta <= 1e-9)
  out
}

#' The whole landscape of one actor, for small N
#'
#' The actor's objective \eqn{f} at every one of the \eqn{2^N} portfolios it
#' could hold at one state of a simulated path, with every other actor's
#' portfolio held fixed, and which portfolios are local peaks (no single add
#' or drop is better). This is the global view behind the W-fitted grid in
#' `vignette("searchnet-landscape-views")`. Its cost doubles with each
#' component, so it is limited to `max_n` components.
#'
#' @inheritParams searchnet_move_compass
#' @param max_n Largest \eqn{N} allowed (default 12, 4096 portfolios).
#'
#' @return A data frame with \eqn{2^N} rows: one 0/1 column per component
#'   (named by component label), `f`, `local_peak`, and `current` (`TRUE` for
#'   the portfolio the actor holds). Row `r` is the portfolio whose bits are
#'   the binary digits of `r - 1`, component 1 the lowest. Attributes `step`
#'   and `actor`.
#'
#' @seealso [searchnet_move_compass()]
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 6, N = 8, density = 0.15, seed = 42)
#' mod <- saomnk_model(density = -2.5, popularity = 0.2,
#'                     influence_matrix = saomnk_block_diagonal(8, 2),
#'                     influence_weight = 0.8)
#' invisible(capture.output(saomnk_run(env, mod, steps_per_actor = 4, seed = 1)))
#' land <- searchnet_actor_landscape(env, actor = 1)
#' sum(land$local_peak)
#' }
#' @export
searchnet_actor_landscape <- function(env, actor, step = NULL, max_n = 12L) {
  st <- .sn_view_state(env, step)
  i <- .sn_view_actor(env, actor)
  N <- ncol(st$B)
  if (N > max_n)
    stop(sprintf("N = %d gives 2^%d portfolios; raise max_n or use searchnet_move_compass()", N, N),
         call. = FALSE)
  P <- as.matrix(expand.grid(rep(list(0:1), N)))
  dimnames(P) <- NULL
  f <- unname(.sn_view_f(env, st$B, st$theta, i, P))
  idx <- seq_len(nrow(P)) - 1L
  peak <- vapply(seq_along(f), function(r) {
    all(f[r] >= f[bitwXor(idx[r], 2L^(seq_len(N) - 1L)) + 1L] - 1e-9)
  }, logical(1))
  cur <- 1L + sum(st$B[i, ] * 2L^(seq_len(N) - 1L))
  out <- as.data.frame(P)
  names(out) <- .sn_component_labels(N)
  out$f <- f
  out$local_peak <- peak
  out$current <- seq_len(nrow(P)) == cur
  attr(out, "step") <- st$step
  attr(out, "actor") <- i
  out
}
