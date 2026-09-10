#' Standalone forbearance plot functions for SaoMNK
#'
#' Visualizations of competitive entry/exit behavior derived from the
#' entry tracking in \code{track-entries.R}. Each function follows the
#' package convention: takes an \code{env} (SaomNkRSienaBiEnv) object or
#' a pre-computed data.frame as its first argument.


# -- saomnk_plot_forbearance --------------------------------------------------

#' Plot competitive entry rate over the simulation chain
#'
#' Shows a rolling-window competitive entry rate (proportion of ADD actions
#' directed at rival-occupied activities), optionally grouped by actor
#' strategy.
#'
#' @param env SaomNkRSienaBiEnv object with a completed simulation, or
#'   \code{NULL} if \code{entry_log} is supplied directly.
#' @param entry_log Optional data.frame from \code{track_entry_decisions()}.
#'   If \code{NULL}, computed from \code{env}.
#' @param window Integer rolling-window size for smoothing. Default 50.
#' @param show_density Logical. If TRUE, overlay a marginal density on the
#'   right axis showing rivals-at-entry distribution. Default FALSE.
#' @param loess_span Numeric span for loess smoother. Default 0.4.
#' @param plot_return Logical; if TRUE return the ggplot object. Default TRUE.
#' @return A ggplot object (if \code{plot_return} is TRUE).
#' @examples
#' \donttest{
#' ## From a synthetic holdings history (no simulation required)
#' set.seed(7)
#' h <- list(matrix(rbinom(12, 1, 0.4), nrow = 3))
#' for (t in 2:30) {
#'   m <- h[[t - 1]]
#'   i <- sample(3, 1); j <- sample(4, 1)
#'   m[i, j] <- 1 - m[i, j]
#'   h[[t]] <- m
#' }
#' entry_log <- track_entry_decisions(holdings_history = h)
#' p <- saomnk_plot_forbearance(entry_log = entry_log, window = 5)
#' }
#' @export
saomnk_plot_forbearance <- function(env = NULL,
                                    entry_log = NULL,
                                    window = 50L,
                                    show_density = FALSE,
                                    loess_span = 0.4,
                                    plot_return = TRUE) {

  if (is.null(entry_log)) {
    if (is.null(env))
      stop("Provide either env or entry_log.")
    entry_log <- track_entry_decisions(env)
  }

  traj <- compute_forbearance_trajectory(entry_log, window = window)

  p <- ggplot(traj, aes(x = step, y = competitive_entry_rate)) +
    geom_line(color = "#D55E00", linewidth = 0.8) +
    geom_smooth(method = "loess", span = loess_span,
                color = "black", fill = "grey80", alpha = 0.3) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Chain step",
         y = "Competitive entry rate",
         title = "Competitive entry rate over simulation",
         subtitle = sprintf("Rolling window = %d ADD events", window)) +
    theme_bw() +
    theme(plot.title = element_text(size = 12, face = "bold"))

  if (plot_return) return(p)
  invisible(p)
}


# -- saomnk_plot_forbearance_spectrum ------------------------------------------

#' Plot forbearance spectrum analysis (multi-panel)
#'
#' Creates a 2x2 panel figure showing competitive entry behavior across
#' a range of theta_inPop values, following the design from the CD2026
#' forbearance spectrum experiment (script 54).
#'
#' @param forbearance_sweep_results A data.frame with columns:
#'   \describe{
#'     \item{theta_inPop}{Numeric. The population effect parameter value.}
#'     \item{competitive_entry_rate}{Numeric in \eqn{[0,1]}.}
#'     \item{mean_rivals_at_entry}{Numeric.}
#'     \item{terminal_kaa}{Numeric. Mean K_AA at simulation end.}
#'     \item{avoidance_rate}{Numeric in \eqn{[0,1]}. Optional.}
#'   }
#'   Each row represents one (theta_inPop, replicate) combination.
#' @param ci Numeric confidence level for ribbons. Default 0.95.
#' @return A ggplot object assembled via \code{cowplot::plot_grid()}.
#' @examples
#' \donttest{
#' ## Synthetic sweep: 5 theta_inPop values x 4 replicates
#' set.seed(1)
#' sweep <- expand.grid(theta_inPop = seq(-1, 1, by = 0.5), rep = 1:4)
#' sweep$competitive_entry_rate <- plogis(sweep$theta_inPop + rnorm(20, 0, 0.2))
#' sweep$mean_rivals_at_entry   <- 1 + sweep$competitive_entry_rate * 2
#' sweep$terminal_kaa           <- 2 + sweep$theta_inPop + rnorm(20, 0, 0.1)
#' sweep$avoidance_rate         <- 1 - sweep$competitive_entry_rate
#'
#' p <- saomnk_plot_forbearance_spectrum(sweep)
#' }
#' @export
saomnk_plot_forbearance_spectrum <- function(forbearance_sweep_results,
                                             ci = 0.95) {
  df <- forbearance_sweep_results
  stopifnot(all(c("theta_inPop", "competitive_entry_rate",
                   "mean_rivals_at_entry") %in% names(df)))

  z <- qnorm(1 - (1 - ci) / 2)

  # Summarize by theta_inPop
  summ <- df %>%
    group_by(theta_inPop) %>%
    summarise(
      comp_rate_mean = mean(competitive_entry_rate, na.rm = TRUE),
      comp_rate_se   = sd(competitive_entry_rate, na.rm = TRUE) / sqrt(n()),
      rivals_mean    = mean(mean_rivals_at_entry, na.rm = TRUE),
      rivals_se      = sd(mean_rivals_at_entry, na.rm = TRUE) / sqrt(n()),
      kaa_mean       = if ("terminal_kaa" %in% names(df))
                         mean(terminal_kaa, na.rm = TRUE) else NA_real_,
      kaa_se         = if ("terminal_kaa" %in% names(df))
                         sd(terminal_kaa, na.rm = TRUE) / sqrt(n()) else NA_real_,
      avoid_mean     = if ("avoidance_rate" %in% names(df))
                         mean(avoidance_rate, na.rm = TRUE) else NA_real_,
      avoid_se       = if ("avoidance_rate" %in% names(df))
                         sd(avoidance_rate, na.rm = TRUE) / sqrt(n()) else NA_real_,
      .groups = "drop"
    )

  # ---- Panel A: Competitive entry rate ----
  p_a <- ggplot(summ, aes(x = theta_inPop, y = comp_rate_mean)) +
    geom_ribbon(aes(ymin = comp_rate_mean - z * comp_rate_se,
                    ymax = comp_rate_mean + z * comp_rate_se),
                fill = "#D55E00", alpha = 0.2) +
    geom_line(color = "#D55E00", linewidth = 0.9) +
    geom_point(color = "#D55E00", size = 2) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = expression(theta[inPop]),
         y = "Competitive entry rate",
         title = "(A) Competitive entry rate") +
    theme_bw()

  # ---- Panel B: Mean rivals at entry ----
  p_b <- ggplot(summ, aes(x = theta_inPop, y = rivals_mean)) +
    geom_ribbon(aes(ymin = rivals_mean - z * rivals_se,
                    ymax = rivals_mean + z * rivals_se),
                fill = "#0072B2", alpha = 0.2) +
    geom_line(color = "#0072B2", linewidth = 0.9) +
    geom_point(color = "#0072B2", size = 2) +
    labs(x = expression(theta[inPop]),
         y = "Mean rivals at entry",
         title = "(B) Rival density at entry") +
    theme_bw()

  # ---- Panel C: Terminal K_AA ----
  if (all(!is.na(summ$kaa_mean))) {
    p_c <- ggplot(summ, aes(x = theta_inPop, y = kaa_mean)) +
      geom_ribbon(aes(ymin = kaa_mean - z * kaa_se,
                      ymax = kaa_mean + z * kaa_se),
                  fill = "#009E73", alpha = 0.2) +
      geom_line(color = "#009E73", linewidth = 0.9) +
      geom_point(color = "#009E73", size = 2) +
      labs(x = expression(theta[inPop]),
           y = expression(K[AA]),
           title = "(C) Terminal multi-market contact") +
      theme_bw()
  } else {
    p_c <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "terminal_kaa\nnot available") +
      theme_void()
  }

  # ---- Panel D: Avoidance rate ----
  if (all(!is.na(summ$avoid_mean))) {
    p_d <- ggplot(summ, aes(x = theta_inPop, y = avoid_mean)) +
      geom_ribbon(aes(ymin = avoid_mean - z * avoid_se,
                      ymax = avoid_mean + z * avoid_se),
                  fill = "#CC79A7", alpha = 0.2) +
      geom_line(color = "#CC79A7", linewidth = 0.9) +
      geom_point(color = "#CC79A7", size = 2) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = expression(theta[inPop]),
           y = "Pioneering entry rate",
           title = "(D) Pioneering (empty-activity) entry") +
      theme_bw()
  } else {
    p_d <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "avoidance_rate\nnot available") +
      theme_void()
  }

  cowplot::plot_grid(p_a, p_b, p_c, p_d, ncol = 2, align = "hv")
}


# -- saomnk_plot_entry_rivalry -------------------------------------------------

#' Plot rival-count distribution at entry
#'
#' Histogram of the number of rivals present when firms enter an activity.
#' Useful for understanding the shape of competitive intensity at entry.
#'
#' @param entry_log data.frame from \code{track_entry_decisions()}.
#' @param max_rivals Integer. Cap the x-axis at this value (values above
#'   are lumped). Default \code{NULL} (no cap).
#' @return A ggplot object.
#' @examples
#' \donttest{
#' set.seed(7)
#' h <- list(matrix(rbinom(12, 1, 0.4), nrow = 3))
#' for (t in 2:20) {
#'   m <- h[[t - 1]]
#'   i <- sample(3, 1); j <- sample(4, 1)
#'   m[i, j] <- 1 - m[i, j]
#'   h[[t]] <- m
#' }
#' entry_log <- track_entry_decisions(holdings_history = h)
#' p <- saomnk_plot_entry_rivalry(entry_log)
#' }
#' @export
saomnk_plot_entry_rivalry <- function(entry_log, max_rivals = NULL) {
  adds <- entry_log %>% filter(action_type == "add")
  if (nrow(adds) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                label = "No ADD events") + theme_void())
  }

  if (!is.null(max_rivals)) {
    adds <- adds %>%
      mutate(n_rivals_present = pmin(n_rivals_present, max_rivals))
  }

  ggplot(adds, aes(x = n_rivals_present)) +
    geom_histogram(fill = "#D55E00", color = "white",
                   binwidth = 1, boundary = -0.5) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40") +
    annotate("text", x = 0.3, y = Inf, vjust = 1.5, hjust = 1,
             label = "Pioneering", color = "grey40", size = 3) +
    annotate("text", x = 0.7, y = Inf, vjust = 1.5, hjust = 0,
             label = "Competitive", color = "#D55E00", size = 3) +
    labs(x = "Number of rivals present at entry",
         y = "Count of ADD events",
         title = "Distribution of rival presence at entry") +
    theme_bw()
}
