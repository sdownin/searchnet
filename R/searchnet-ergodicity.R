# ---------------------------------------------------------------------------- #
#  searchnet-ergodicity.R
#
#  Ergodicity / independence-from-initial-conditions demonstration for the
#  SAOM-NK choice rule (Theorem 4, SAOM-QRE equivalence).
#
#  WHY THIS FILE EXISTS
#  --------------------
#  Blume's result says the logit-response Markov chain is ergodic with a unique
#  stationary Gibbs distribution, so every starting configuration converges to
#  the SAME equilibrium distribution. Until v0.9.1 that claim was illustrated in
#  four separate documents (the JSS main paper, its online appendix, the Blume
#  tutorial vignette, and the proof registry) by the same two-point comparison:
#  one run from a sparse start, one run from a dense start, on a 4 x 5 = 20-cell
#  matrix, followed by an UNCONDITIONAL cat() asserting that the two "converge
#  toward a similar equilibrium density."
#
#  That illustration could not fail. It printed its own conclusion regardless of
#  the numbers, on a grid so coarse that one tie is 0.05 of the density, and
#  from a single draw per arm, so there was no sampling distribution to compare
#  against. It printed the same sentence under a 0.1 gap and under a 0.2 gap.
#
#  What actually demonstrates the theorem is the RATE. An ergodic chain forgets
#  its initial condition geometrically, so the gap between arms should decay
#  toward zero as run length grows -- and that decay is measurable, falsifiable,
#  and much more informative than any single pair of numbers. This function
#  measures it, and reports a verdict COMPUTED FROM the result rather than
#  asserted alongside it.
#
#  THE process_chain TRAP
#  ----------------------
#  search_rsiena(process_chain = FALSE) does not write the simulated end state
#  back to $bipartite_matrix, so the matrix still holds the INITIAL draw. Any
#  final-density computed after such a call silently returns the starting
#  density and the chain appears not to mix at all. This function always forces
#  process_chain = TRUE and refuses to be talked out of it.
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#  searchnet_ergodicity_sweep
# ---------------------------------------------------------------------------- #

#' Measure Independence from Initial Conditions (Theorem 4)
#'
#' Runs the same fixed-coefficient model from two or more contrasting starting
#' densities, at a range of run lengths, with several replicates each, and
#' measures how fast the between-arm gap in final density decays. An ergodic
#' chain forgets where it started, so the gap should collapse geometrically
#' toward zero as run length grows.
#'
#' The verdict is computed from the simulated output, not asserted: at the
#' longest run length the function runs a two-one-sided-tests (TOST)
#' equivalence test of the arms' mean final densities against a declared
#' margin. Equivalence is concluded only if the confidence interval for the
#' difference lies entirely inside that margin, so the demonstration can fail.
#'
#' @param M Integer. Number of actors. Default 12.
#' @param N Integer. Number of components. Default 15. Note that \code{M * N}
#'   is the density resolution: on a 4 x 5 grid a single tie moves the density
#'   by 0.05, which is coarser than any equivalence margin worth declaring.
#' @param start_densities Numeric vector of starting tie probabilities, one per
#'   arm. Default \code{c(0.1, 0.8)}.
#' @param run_lengths Integer vector of \code{iterations_per_actor} values to
#'   sweep. Default \code{c(15, 30, 60, 120, 240)}.
#' @param replicates Integer. Independent replicates per arm per run length,
#'   each with its own initial draw and its own run seed. Default 6. Replicates
#'   are what turn two numbers into two distributions.
#' @param density_par,pop_par,epistasis_par Numeric. Fixed coefficients for the
#'   \code{density}, \code{inPop}, and \code{XWX} effects.
#' @param W Optional influence matrix (\code{N x N}) for the \code{XWX} effect.
#'   Defaults to a block-diagonal matrix with \code{blocks} blocks.
#' @param blocks Integer. Number of blocks in the default influence matrix.
#' @param seed Integer. Base seed; replicate and arm indices are offset from it
#'   so every run is distinct and the whole sweep is reproducible.
#' @param equivalence_margin Numeric. The declared TOST margin, in density
#'   units. Default 0.05. Declare it before looking at the result.
#' @param conf_level Numeric. Confidence level for the equivalence interval.
#'   Default 0.90, the conventional level for TOST at alpha = 0.05.
#' @param verbose Logical. Print progress per run length.
#'
#' @return An object of class \code{searchnet_ergodicity}: a list with
#'   \code{$runs} (tidy per-run results), \code{$summary} (per run length: mean
#'   and sd per arm, and the between-arm gap), \code{$decay} (the fitted
#'   geometric decay rate of the gap), \code{$verdict} (the TOST result at the
#'   longest run length), and \code{$call}.
#'
#' @details
#' The default configuration is calibrated so the demonstration succeeds
#' without being rigged: at 15 iterations per actor the arms are still far
#' apart, and only by roughly 240 do they become statistically indistinguishable.
#' A sweep that started at its converged answer would demonstrate nothing.
#'
#' @seealso \code{\link{plot.searchnet_ergodicity}}
#'
#' @examples
#' \dontrun{
#' erg <- searchnet_ergodicity_sweep()
#' erg
#' plot(erg)
#' }
#' @export
searchnet_ergodicity_sweep <- function(M = 12L, N = 15L,
                                       start_densities = c(0.1, 0.8),
                                       run_lengths = c(15L, 30L, 60L, 120L, 240L),
                                       replicates = 6L,
                                       density_par = -0.5,
                                       pop_par = 0.15,
                                       epistasis_par = 0.2,
                                       W = NULL,
                                       blocks = 3L,
                                       seed = 42L,
                                       equivalence_margin = 0.05,
                                       conf_level = 0.90,
                                       verbose = FALSE) {

  stopifnot(is.numeric(M), length(M) == 1, M >= 2)
  stopifnot(is.numeric(N), length(N) == 1, N >= 2)
  stopifnot(is.numeric(start_densities), length(start_densities) >= 2)
  stopifnot(all(start_densities >= 0), all(start_densities <= 1))
  stopifnot(is.numeric(run_lengths), length(run_lengths) >= 1,
            all(run_lengths >= 1))
  stopifnot(is.numeric(replicates), length(replicates) == 1, replicates >= 2)
  stopifnot(is.numeric(equivalence_margin), equivalence_margin > 0)

  M <- as.integer(M); N <- as.integer(N)
  run_lengths <- sort(unique(as.integer(run_lengths)))
  replicates  <- as.integer(replicates)

  ## A one-tie change moves the density by 1/(M*N). An equivalence margin
  ## finer than that is asking the grid to resolve something it cannot.
  resolution <- 1 / (M * N)
  if (equivalence_margin < resolution)
    stop(sprintf(paste0(
      "equivalence_margin (%.4f) is finer than the density resolution of a ",
      "%d x %d grid (%.4f = one tie). Either widen the margin or enlarge the ",
      "grid; as specified the test cannot resolve the difference it is ",
      "being asked to rule out."),
      equivalence_margin, M, N, resolution), call. = FALSE)

  DV <- "self$bipartite_rsienaDV"

  if (is.null(W)) {
    W <- create_block_diag(N, max(1L, round(N / blocks)))
  } else {
    if (!is.matrix(W) || nrow(W) != N || ncol(W) != N)
      stop(sprintf("W must be an %d x %d matrix.", N, N), call. = FALSE)
  }

  sm <- list(
    dv_bipartite = list(
      name = DV,
      effects = list(
        list(effect = "density", parameter = density_par, dv_name = DV, fix = TRUE),
        list(effect = "inPop",   parameter = pop_par,     dv_name = DV, fix = TRUE)
      ),
      coDyadCovars = list(
        list(effect = "XWX", parameter = epistasis_par, dv_name = DV, fix = TRUE,
             nodeSet = c("COMPONENTS", "COMPONENTS"),
             interaction1 = "self$component_1_coDyadCovar",
             x = W)
      )
    )
  )

  ## ------------------------------------------------------------------ ##
  ##  One run: draw an initial matrix at p0, simulate, return the final
  ##  density. process_chain = TRUE is NOT optional -- see the file header.
  ## ------------------------------------------------------------------ ##
  one_run <- function(p0, arm, rep_i, iters) {
    env <- SaomNkRSienaBiEnv$new(list(
      M = M, N = N, BI_PROB = p0,
      rand_seed = as.integer(seed + arm * 10000L + rep_i * 100L + iters),
      name = sprintf("_erg_a%d_r%02d_i%d_", arm, rep_i, iters)
    ))
    env$search_rsiena(
      sm,
      iterations_per_actor = iters,
      run_seed      = as.integer(seed + arm * 20000L + rep_i * 100L + iters),
      process_chain = TRUE
    )
    sum(env$bipartite_matrix) / (M * N)
  }

  n_arms <- length(start_densities)
  rows   <- vector("list", length(run_lengths) * n_arms * replicates)
  k <- 1L

  for (iters in run_lengths) {
    t0 <- Sys.time()
    for (a in seq_len(n_arms)) {
      for (r in seq_len(replicates)) {
        rows[[k]] <- data.frame(
          run_length    = iters,
          arm           = a,
          start_density = start_densities[a],
          replicate     = r,
          final_density = one_run(start_densities[a], a, r, iters),
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
    if (verbose)
      message(sprintf("  run_length %4d done (%.0fs)", iters,
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }

  runs <- do.call(rbind, rows)

  ## ------------------------------------------------------------------ ##
  ##  Summarize: per run length, the mean per arm and the max between-arm
  ##  gap. With two arms the max gap is just |mean_1 - mean_2|.
  ## ------------------------------------------------------------------ ##
  summ <- do.call(rbind, lapply(run_lengths, function(L) {
    sub  <- runs[runs$run_length == L, , drop = FALSE]
    mus  <- vapply(seq_len(n_arms),
                   function(a) mean(sub$final_density[sub$arm == a]), numeric(1))
    sds  <- vapply(seq_len(n_arms),
                   function(a) stats::sd(sub$final_density[sub$arm == a]), numeric(1))
    data.frame(
      run_length = L,
      gap        = max(mus) - min(mus),
      mean_lo    = mus[which.min(start_densities)],
      mean_hi    = mus[which.max(start_densities)],
      sd_pooled  = sqrt(mean(sds^2)),
      stringsAsFactors = FALSE
    )
  }))

  initial_gap <- max(start_densities) - min(start_densities)

  ## ------------------------------------------------------------------ ##
  ##  The Monte Carlo floor.
  ##
  ##  Once the arms are genuinely equivalent, the MEASURED gap does not go
  ##  to zero -- it settles at the expected absolute difference of two
  ##  sample means drawn from the same distribution, which for n replicates
  ##  per arm is sigma * sqrt(2/n) * sqrt(2/pi). Points at or below that
  ##  level are measuring estimator noise, not mixing, and including them
  ##  in a decay fit steepens the slope for a reason that has nothing to do
  ##  with the chain. Fit above the floor only, and say which points were
  ##  used.
  ## ------------------------------------------------------------------ ##
  sd_at_floor <- summ$sd_pooled[summ$run_length == max(run_lengths)]
  mc_floor    <- sd_at_floor * sqrt(2 / replicates) * sqrt(2 / pi)

  decay <- NULL
  usable <- summ$gap > 0 & summ$gap > mc_floor
  if (sum(usable) >= 2) {
    fit <- stats::lm(log(summ$gap[usable]) ~ log(summ$run_length[usable]))
    decay <- list(
      slope     = unname(stats::coef(fit)[2]),
      r_squared = summary(fit)$r.squared,
      n_points  = sum(usable),
      floor     = mc_floor,
      excluded  = summ$run_length[!usable]
    )
  } else {
    ## Every point is already at the noise floor: the sweep started after
    ## the chain had mixed and shows no decay to measure. Say so rather
    ## than fitting a line through noise.
    decay <- list(slope = NA_real_, r_squared = NA_real_,
                  n_points = sum(usable), floor = mc_floor,
                  excluded = summ$run_length[!usable])
  }

  ## ------------------------------------------------------------------ ##
  ##  Verdict at the longest run length: TOST equivalence. NOTE the logic
  ##  -- equivalence requires the WHOLE interval inside the margin. A
  ##  non-significant difference is not evidence of sameness, which is
  ##  exactly the error the old unconditional cat() institutionalized.
  ## ------------------------------------------------------------------ ##
  L_max <- max(run_lengths)
  sub   <- runs[runs$run_length == L_max, , drop = FALSE]
  a_lo  <- which.min(start_densities); a_hi <- which.max(start_densities)
  x <- sub$final_density[sub$arm == a_lo]
  y <- sub$final_density[sub$arm == a_hi]

  tt <- stats::t.test(x, y, conf.level = conf_level)
  ci <- as.numeric(tt$conf.int)
  equivalent <- (ci[1] > -equivalence_margin) && (ci[2] < equivalence_margin)

  verdict <- list(
    run_length  = L_max,
    difference  = mean(x) - mean(y),
    ci          = ci,
    conf_level  = conf_level,
    margin      = equivalence_margin,
    equivalent  = equivalent,
    initial_gap = initial_gap,
    final_gap   = summ$gap[summ$run_length == L_max],
    collapse_pct = 100 * (1 - summ$gap[summ$run_length == L_max] / initial_gap)
  )

  structure(
    list(runs = runs, summary = summ, decay = decay, verdict = verdict,
         config = list(M = M, N = N, start_densities = start_densities,
                       run_lengths = run_lengths, replicates = replicates,
                       resolution = resolution),
         call = match.call()),
    class = "searchnet_ergodicity"
  )
}


# ---------------------------------------------------------------------------- #
#  print method
# ---------------------------------------------------------------------------- #

#' Print an Ergodicity Sweep
#'
#' @param x A \code{searchnet_ergodicity} object.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @export
print.searchnet_ergodicity <- function(x, ...) {
  cfg <- x$config; v <- x$verdict

  cat("Ergodicity sweep (Theorem 4: independence from initial conditions)\n")
  cat(sprintf("  %d actors x %d components (%d cells; one tie = %.4f density)\n",
              cfg$M, cfg$N, cfg$M * cfg$N, cfg$resolution))
  cat(sprintf("  arms starting at %s, %d replicates each\n",
              paste(sprintf("%.2f", cfg$start_densities), collapse = " and "),
              cfg$replicates))

  cat("\n  run length   mean(lo)   mean(hi)        gap\n")
  for (i in seq_len(nrow(x$summary))) {
    s <- x$summary[i, ]
    cat(sprintf("  %10d   %8.4f   %8.4f   %8.4f\n",
                s$run_length, s$mean_lo, s$mean_hi, s$gap))
  }

  cat(sprintf("\n  initial gap %.4f --> final gap %.4f  (%.1f%% collapse)\n",
              v$initial_gap, v$final_gap, v$collapse_pct))

  if (!is.null(x$decay)) {
    cat(sprintf("  Monte Carlo floor (%d replicates): %.4f\n",
                cfg$replicates, x$decay$floor))
    if (is.na(x$decay$slope)) {
      cat("  no decay to fit: every run length is already at the noise floor\n")
    } else {
      cat(sprintf("  decay above the floor: log(gap) ~ %.2f * log(iterations), R^2 = %.3f\n",
                  x$decay$slope, x$decay$r_squared))
      if (length(x$decay$excluded))
        cat(sprintf("  (fit on %d point(s); %s excluded as at-or-below the floor)\n",
                    x$decay$n_points, paste(x$decay$excluded, collapse = ", ")))
    }
  }

  ## The verdict is computed, and it can come out either way.
  cat(sprintf("\n  TOST at %d iterations, margin +/- %.3f, %.0f%% CI [%.4f, %.4f]:\n",
              v$run_length, v$margin, 100 * v$conf_level, v$ci[1], v$ci[2]))
  if (isTRUE(v$equivalent)) {
    cat("  EQUIVALENT -- the arms are statistically indistinguishable at the\n")
    cat("  declared margin. The chain has forgotten where it started.\n")
  } else {
    cat("  NOT EQUIVALENT at this margin. Either the chain has not yet mixed\n")
    cat("  (extend run_lengths) or the margin is tighter than this run length\n")
    cat("  can support. This is a real negative, not a formatting problem.\n")
  }
  invisible(x)
}


# ---------------------------------------------------------------------------- #
#  plot method
# ---------------------------------------------------------------------------- #

#' Plot an Ergodicity Sweep
#'
#' Two panels: the arms' final densities converging as run length grows, and
#' the between-arm gap decaying on log-log axes (where geometric decay is a
#' straight line).
#'
#' @param x A \code{searchnet_ergodicity} object.
#' @param ... Passed to \code{plot}.
#' @return \code{x}, invisibly.
#' @export
plot.searchnet_ergodicity <- function(x, ...) {
  cfg <- x$config; s <- x$summary
  op <- graphics::par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  on.exit(graphics::par(op), add = TRUE)

  ## -- Panel A: the two arms closing -------------------------------------- ##
  ylim <- range(c(x$runs$final_density, cfg$start_densities))
  plot(range(s$run_length), ylim, type = "n", log = "x",
       xlab = "Iterations per actor (log scale)", ylab = "Final density",
       main = "A. Arms converge", ...)
  graphics::points(x$runs$run_length, x$runs$final_density,
                   pch = 16, cex = 0.6,
                   col = grDevices::adjustcolor(
                     c("#2166AC", "#B2182B")[x$runs$arm], alpha.f = 0.45))
  graphics::lines(s$run_length, s$mean_lo, col = "#2166AC", lwd = 2, type = "b", pch = 16)
  graphics::lines(s$run_length, s$mean_hi, col = "#B2182B", lwd = 2, type = "b", pch = 17)
  graphics::abline(h = cfg$start_densities, lty = 3, col = "grey55")
  graphics::legend("right", bty = "n", cex = 0.85,
                   legend = sprintf("start %.2f", cfg$start_densities),
                   col = c("#2166AC", "#B2182B"), lwd = 2, pch = c(16, 17))

  ## -- Panel B: the gap decaying ------------------------------------------ ##
  pos <- s$gap > 0
  plot(s$run_length[pos], s$gap[pos], type = "b", log = "xy", pch = 16, lwd = 2,
       col = "#333333", xlab = "Iterations per actor (log scale)",
       ylab = "Between-arm gap (log scale)", main = "B. Gap decays geometrically")
  graphics::abline(h = x$verdict$margin, lty = 2, col = "#B2182B")
  graphics::text(min(s$run_length[pos]), x$verdict$margin,
                 sprintf(" equivalence margin %.3f", x$verdict$margin),
                 adj = c(0, -0.5), cex = 0.75, col = "#B2182B")

  ## Shade the Monte Carlo floor: below it the measured gap is estimator
  ## noise, so the flattening there is not the chain failing to mix.
  if (!is.null(x$decay) && is.finite(x$decay$floor)) {
    graphics::rect(par("usr")[1], par("usr")[3], par("usr")[2],
                   log10(x$decay$floor),
                   col = grDevices::adjustcolor("grey60", alpha.f = 0.18),
                   border = NA)
    graphics::abline(h = x$decay$floor, lty = 3, col = "grey30")
    graphics::text(max(s$run_length[pos]), x$decay$floor,
                   "Monte Carlo floor ", adj = c(1, 1.4), cex = 0.7, col = "grey25")
  }
  ## Redraw over the shading.
  graphics::lines(s$run_length[pos], s$gap[pos], type = "b", pch = 16,
                  lwd = 2, col = "#333333")

  if (!is.null(x$decay) && !is.na(x$decay$slope)) {
    graphics::abline(a = stats::coef(stats::lm(
                       log10(s$gap[pos & s$gap > x$decay$floor]) ~
                       log10(s$run_length[pos & s$gap > x$decay$floor])))[1],
                     b = x$decay$slope, col = "#2166AC", lwd = 1.5, lty = 2)
    graphics::mtext(sprintf("slope %.2f (R^2 = %.3f), fit above floor only",
                            x$decay$slope, x$decay$r_squared),
                    side = 3, line = 0.1, cex = 0.75)
  }
  invisible(x)
}
