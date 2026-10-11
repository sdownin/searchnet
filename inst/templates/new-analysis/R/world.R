## world.R: the small helpers analysis.Rmd uses. Generic and synthetic.
##
## A "redraw world" is the simplest panel model with a density and a rate of
## change: wave 1 is a Bernoulli(p) draw over the M x N actor-by-component
## cells, and in each later wave a share `redraw` of the cells is redrawn from
## Bernoulli(p). It has two parameters, both matched to observed moments, so
## any other moment it fails to reproduce (the spread of the {K} degrees, for
## example) is information about the data, not about a tuning choice.

## Simulate one redraw panel: an M x N x W 0/1 integer array.
## `shock`, when given, is list(period, actors, p_post): from `period` on, the
## listed actors' redrawn cells use p_post instead of p (a counterfactual).
world_redraw_panel <- function(M, N, W, p, redraw, shock = NULL) {
  stopifnot(W >= 1, p >= 0, p <= 1, redraw >= 0, redraw <= 1)
  B <- array(0L, c(M, N, W))
  B[, , 1] <- stats::rbinom(M * N, 1L, p)
  if (W >= 2) for (w in 2:W) {
    pw <- matrix(p, M, N)
    if (!is.null(shock) && w >= shock$period)
      pw[shock$actors, ] <- shock$p_post
    b <- B[, , w - 1]
    cells <- stats::rbinom(M * N, 1L, redraw) == 1L
    b[cells] <- stats::rbinom(sum(cells), 1L, pw[cells])
    B[, , w] <- b
  }
  B
}

## Match a redraw world to an observed array: p from the density after the
## first period, `redraw` from the share of cells that change per transition
## (a redrawn cell changes with probability 2 p (1 - p)).
world_calibrate <- function(B) {
  W <- dim(B)[3L]
  later <- if (W >= 2) 2:W else 1L
  p <- mean(B[, , later])
  change <- if (W >= 2)
    mean(vapply(2:W, function(w) mean(B[, , w] != B[, , w - 1]), numeric(1)))
  else 0
  redraw <- if (p > 0 && p < 1) min(1, change / (2 * p * (1 - p))) else 0
  list(p = p, redraw = redraw, M = dim(B)[1L], N = dim(B)[2L], W = W)
}

## Long format, one row per tie: the shape searchnet_bipartite_from_long()
## reads. Ids are zero-padded so their sorted order is the array order.
world_to_long <- function(B) {
  idx <- which(B == 1L, arr.ind = TRUE)
  data.frame(actor     = sprintf("a%03d", idx[, 1]),
             component = sprintf("c%03d", idx[, 2]),
             period    = as.integer(idx[, 3]),
             stringsAsFactors = FALSE)[order(idx[, 3], idx[, 1], idx[, 2]), ]
}

## Actor-by-period panel in the shape searchnet_placebo() reads, with an
## outcome taken from searchnet_k_readings() (default K_AC, actor scope).
world_actor_panel <- function(readings, treated_actors, first_period,
                              outcome = "K_AC") {
  a <- readings[readings$level == "actor", c("period", "id", outcome)]
  names(a) <- c("step", "actor_id", "outcome")
  tr <- a$actor_id %in% treated_actors
  data.frame(actor_id = factor(a$actor_id), step = as.integer(a$step),
             outcome = as.numeric(a$outcome), treated = as.integer(tr),
             first_treat = ifelse(tr, as.integer(first_period), 0L))
}
