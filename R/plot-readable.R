# plot-readable.R
# =============================================================================
# Shared builders behind the package's main plots: the {K} degree panels,
# the utility decomposition, and the reading guides they draw.
#
# The design follows Figure 1 of the JSS paper
# (paper/replication/make_figure1_mental_model.R):
#   * a title that states what the run shows, computed from the run;
#   * a plain-language subtitle that says how to read the figure;
#   * at most one or two short notes with arrows, placed on the data
#     (annotate = TRUE), for example the shock or the level a series settles at;
#   * one visual grammar: actors orange, components blue, the event (a shock)
#     vermillion, the influence matrix W green, context grey; the four
#     {K} channels labeled K_AC, K_CA, K_AA, K_CC with their plain names
#     (scope, popularity, sociality, coupling);
#   * the model parameters in a one-line caption rather than the title;
#   * no legend when it would carry a single entry.
# Everything drawn is computed from the environment passed in.
# =============================================================================


## Colors by role. Okabe-Ito, as in plot-theme.R.
.sn_role <- c(actor = "#E69F00", component = "#0072B2", event = "#D55E00",
              W = "#009E73", context = "grey60", ink = "grey20")

## Actor strategy groups after the first: Okabe-Ito sky, yellow, purple,
## black. Vermillion is kept for events.
.sn_actor_pool <- c("#E69F00", "#56B4E9", "#F0E442", "#CC79A7", "#000000")

## The four {K} channels: plain name and what the degree counts.
.sn_k_info <- data.frame(
  channel = c("K_AC", "K_CA", "K_AA", "K_CC"),
  plain   = c("scope", "popularity", "sociality", "coupling"),
  counts  = c("components each actor holds",
              "actors holding each component",
              "actors sharing a component",
              "components sharing an actor"),
  node    = c("Actor", "Component", "Actor", "Component"),
  stringsAsFactors = FALSE)

## Plotmath strip label for a channel, e.g. K[AC]*"  scope: components ...".
.sn_k_label <- function(channel) {
  i <- match(channel, .sn_k_info$channel)
  sprintf('K[%s]*"  %s: %s"', sub("^K_", "", channel), .sn_k_info$plain[i],
          .sn_k_info$counts[i])
}

## Join plain names in prose: "scope", "scope and sociality",
## "scope, popularity and coupling".
.sn_join <- function(x) {
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

## One-line model caption: "Model: density = -0.5, inPop = 0.2, XWX = 0.05;
## 8 actors, 12 components." Built from get_structure_model_params(), so it
## reports the weights the run used.
.sn_model_caption <- function(env) {
  p <- tryCatch(env$get_structure_model_params(), error = function(e) NULL)
  if (is.null(p)) return(NULL)
  vals <- c(p$structeffs, p$covs)
  vals <- vals[!is.na(vals)]
  eff <- if (length(vals))
    paste(sprintf("%s = %s", names(vals), format(signif(vals, 4), trim = TRUE)),
          collapse = ", ") else "no effects"
  sprintf("Model: %s; %d actors, %d components.", eff, env$M, env$N)
}

## Shock boundaries of a run: one row per change of the shocked parameters,
## with the ministep at which the new values start and a plain label such as
## "density -0.5 to -2". NULL when the run has no shocks.
.sn_shock_boundaries <- function(env) {
  if (is.null(env$theta_shocks)) return(NULL)
  r <- tryCatch(suppressMessages(env$get_theta_shock_rects_df(env$theta_shocks)),
                error = function(e) NULL)
  if (is.null(r) || nrow(r) < 2) return(NULL)
  r <- r[order(r$start), , drop = FALSE]
  seg <- function(i) {
    e <- strsplit(as.character(r$effect_all[i]), ";", fixed = TRUE)[[1]]
    v <- strsplit(as.character(r$parameter[i]), "|", fixed = TRUE)[[1]]
    stats::setNames(rep_len(v, length(e)), e)
  }
  out <- lapply(2:nrow(r), function(i) {
    a <- seg(i - 1); b <- seg(i)
    eff <- union(names(a), names(b))
    lab <- vapply(eff, function(k)
      sprintf("%s %s to %s", k,
              if (k %in% names(a)) a[[k]] else "?",
              if (k %in% names(b)) b[[k]] else "?"), "")
    changed <- vapply(eff, function(k) !identical(a[k], b[k]), TRUE)
    if (any(changed)) lab <- lab[changed]
    data.frame(step = r$start[i], label = paste(lab, collapse = "; "),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

## Smoothed mean series: per step mean of `value` within each `facet`, then a
## loess fit, evaluated at the observed steps. Used to place notes on the line.
.sn_smooth_mean <- function(df, facet, span) {
  do.call(rbind, lapply(split(df, df[[facet]], drop = TRUE), function(d) {
    m <- stats::aggregate(d$value, list(step = d$chain_step_id), mean)
    names(m)[2] <- "mean"
    fit <- if (nrow(m) >= 8 && stats::var(m$mean) > 0)
      tryCatch(stats::predict(stats::loess(mean ~ step, data = m, span = span)),
               error = function(e) m$mean) else m$mean
    data.frame(facet = d[[facet]][1], step = m$step, mean = m$mean,
               smooth = as.numeric(fit), stringsAsFactors = FALSE)
  }))
}

## First step after which a smoothed series stays within `tol` (a share of its
## range) of its final level. NA when it settles only at the very end or never
## moves.
.sn_settle_step <- function(step, y, tol = 0.1, final_share = 0.15) {
  n <- length(y)
  if (n < 10) return(NA_real_)
  rng <- diff(range(y))
  if (!is.finite(rng) || rng <= 0) return(NA_real_)
  last <- mean(y[seq(max(1, floor(n * (1 - final_share))), n)])
  off <- abs(y - last) > tol * rng
  if (!any(off)) return(NA_real_)
  k <- max(which(off)) + 1
  if (k > n || k > 0.8 * n) return(NA_real_)
  step[k]
}

## A note: text at (xt, yt) and an arrow from it to (x, y), confined to one
## facet through the facet column(s) carried in `d`.
.sn_note_layers <- function(d, size = 3) {
  list(
    ggplot2::geom_segment(
      data = d, ggplot2::aes(x = .data$xs, y = .data$ys, xend = .data$x, yend = .data$y),
      inherit.aes = FALSE, color = .sn_role[["ink"]], linewidth = 0.35,
      arrow = grid::arrow(length = grid::unit(0.07, "in"), type = "closed")),
    ggplot2::geom_text(
      data = d, ggplot2::aes(x = .data$xt, y = .data$yt, label = .data$label,
                             hjust = .data$hjust, vjust = .data$vjust),
      inherit.aes = FALSE, color = .sn_role[["ink"]], size = size,
      lineheight = 0.95))
}

## Shock layers: a light vermillion wash after each boundary and a dashed
## vermillion line at it; `facet_df` is a data frame of facet values the
## layers repeat over.
.sn_shock_layers <- function(shocks, x_max) {
  if (is.null(shocks) || !nrow(shocks)) return(list())
  wash <- data.frame(xmin = shocks$step,
                     xmax = c(shocks$step[-1], x_max))
  list(
    ggplot2::geom_rect(data = wash, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                                 ymin = -Inf, ymax = Inf),
                       inherit.aes = FALSE, fill = .sn_role[["event"]], alpha = 0.06),
    ggplot2::geom_vline(xintercept = shocks$step, color = .sn_role[["event"]],
                        linetype = "dashed", linewidth = 0.6))
}

## Keep a note on the page: text placed inside the panel, on whichever side
## of the target has more room.
.sn_place <- function(x, y, xr, yr, prefer = c("below", "above")) {
  prefer <- match.arg(prefer)
  dy <- 0.28 * diff(yr)
  below <- y - dy; above <- y + dy
  yt <- if (prefer == "below") { if (below >= yr[1] + 0.05 * diff(yr)) below else above }
        else { if (above <= yr[2] - 0.05 * diff(yr)) above else below }
  right <- x < mean(xr)
  xt <- if (right) x + 0.08 * diff(xr) else x - 0.08 * diff(xr)
  list(xt = xt, yt = yt, hjust = if (right) 0 else 1,
       vjust = if (yt < y) 1 else 0,
       xs = xt, ys = yt + if (yt < y) 0.01 * diff(yr) else -0.01 * diff(yr))
}


# -----------------------------------------------------------------------------
# {K} degree panels
# -----------------------------------------------------------------------------

## Build a {K} degree plot for the channels requested, from a K4 data frame
## (env$get_K4_df() or an experiment's K4_df). The four-panel figure, the
## actor-degree and the component-degree plots all come from here.
.sn_degree_plot <- function(env, Kdf, channels = c("K_AC", "K_CA", "K_AA", "K_CC"),
                            ncol = 2, loess_span = 0.5, point_alpha_dimmer = 1,
                            annotate = TRUE) {
  Kdf <- as.data.frame(Kdf)
  Kdf <- Kdf[Kdf$effect %in% channels, , drop = FALSE]
  Kdf$channel <- factor(.sn_k_label(Kdf$effect), levels = .sn_k_label(channels))
  n_steps <- max(Kdf$chain_step_id)
  x_rng <- range(Kdf$chain_step_id)

  ## Groups: actor strategy levels, and components initially unused / used.
  Kdf$node_group <- as.character(Kdf$node_group)
  a_grp <- unique(Kdf$node_group[Kdf$node_type == "Actor"])
  c_grp <- intersect(c("NEW", "OLD"), unique(Kdf$node_group[Kdf$node_type == "Component"]))
  a_cols <- stats::setNames(rep_len(.sn_actor_pool, length(a_grp)), a_grp)
  c_cols <- stats::setNames(c(NEW = .sn_role[["component"]], OLD = "#CC79A7")[c_grp], c_grp)
  if (length(c_grp) == 1) c_cols[] <- .sn_role[["component"]]
  cols <- c(a_cols, c_cols)
  a_labs <- if (length(a_grp) == 1) "Actors" else paste("Actors: strategy", a_grp)
  c_labs <- if (length(c_grp) == 1) "Components" else
    c(NEW = "Components: initially unused", OLD = "Components: initially used")[c_grp]
  labs_all <- c(stats::setNames(a_labs, a_grp), stats::setNames(c_labs, c_grp))
  multi <- length(a_grp) > 1 || length(c_grp) > 1

  npoints <- nrow(Kdf)
  point_size <- min(2, 6 / log10(max(npoints, 10)))
  point_alpha <- min(1, 15 / sqrt(max(npoints, 1))) * point_alpha_dimmer

  mean_df <- stats::aggregate(Kdf$value, list(chain_step_id = Kdf$chain_step_id,
                                              channel = Kdf$channel), mean)
  names(mean_df)[3] <- "value"
  sm <- .sn_smooth_mean(Kdf, "channel", loess_span)
  shocks <- .sn_shock_boundaries(env)

  p <- ggplot2::ggplot(Kdf, ggplot2::aes(.data$chain_step_id, .data$value)) +
    .sn_shock_layers(shocks, x_rng[2]) +
    ggplot2::geom_point(ggplot2::aes(color = .data$node_group), shape = 1,
                        alpha = point_alpha, size = point_size)
  if (multi)
    p <- p + ggplot2::geom_smooth(ggplot2::aes(color = .data$node_group,
                                               group = .data$node_group),
                                  method = "loess", formula = y ~ x, span = loess_span,
                                  se = FALSE, linewidth = 0.7)
  p <- p +
    ggplot2::geom_smooth(data = mean_df, ggplot2::aes(group = 1), method = "loess",
                         formula = y ~ x, span = loess_span, se = FALSE,
                         color = "black", linewidth = 0.9) +
    ggplot2::scale_color_manual(values = cols, labels = labs_all, name = NULL,
                                guide = if (multi) "legend" else "none") +
    ggplot2::facet_wrap(~ channel, ncol = ncol, labeller = ggplot2::label_parsed,
                        scales = if (ncol == 1) "free_y" else "fixed") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.12))) +
    theme_searchnet() +
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = 9),
                   panel.spacing = grid::unit(0.8, "lines"))
  if (multi)
    p <- p + ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(alpha = 1, shape = NA, linewidth = 1)))

  ## Title: what the run shows, computed from the smoothed means.
  plain <- .sn_k_info$plain[match(channels, .sn_k_info$channel)]
  lvl <- levels(Kdf$channel)
  who <- if (length(channels) == 4) "all four {K} degrees" else .sn_join(plain)
  title <- NULL
  if (!is.null(shocks)) {
    b <- shocks$step[1]
    dirs <- vapply(lvl, function(l) {
      s <- sm[sm$facet == l, ]
      pre  <- s$mean[s$step < b & s$step >= b - 0.2 * diff(x_rng)]
      post <- s$mean[s$step >= x_rng[2] - 0.15 * diff(x_rng)]
      if (!length(pre) || !length(post)) return(0)
      d <- mean(post) - mean(pre)
      if (abs(d) < 0.05 * max(1e-9, diff(range(s$mean)))) 0 else sign(d)
    }, 0)
    title <- if (all(dirs < 0)) sprintf("The shock at ministep %d lowers %s", b, who) else
      if (all(dirs > 0)) sprintf("The shock at ministep %d raises %s", b, who) else
        sprintf("The shock at ministep %d moves %s", b,
                if (length(channels) == 4) "the four {K} degrees" else who)
  } else {
    rises <- vapply(lvl, function(l) {
      s <- sm[sm$facet == l, ]; n <- nrow(s)
      mean(s$mean[seq(ceiling(0.85 * n), n)]) > mean(s$mean[seq_len(max(1, ceiling(0.05 * n)))])
    }, TRUE)
    settles <- vapply(lvl, function(l) {
      s <- sm[sm$facet == l, ]; !is.na(.sn_settle_step(s$step, s$smooth))
    }, TRUE)
    title <- if (all(rises) && all(settles)) {
      sprintf("%s rise, then level off", .sn_cap(who))
    } else if (all(rises)) {
      sprintf("%s rise over the run", .sn_cap(who))
    } else {
      sprintf("%s over %d ministeps", .sn_cap(.sn_join(plain)), n_steps)
    }
  }

  node_txt <- if (length(unique(.sn_k_info$node[match(channels, .sn_k_info$channel)])) == 2)
    (if (multi) "one actor or component" else "one actor (orange) or component (blue)") else
    if (all(channels %in% c("K_AC", "K_AA"))) "one actor" else "one component"
  sub <- sprintf("Faint points: %s at each ministep%s. Black line: the mean.%s", node_txt,
                 if (multi) ", colored by group" else "",
                 if (length(channels) == 4)
                   "\nTop row: direct ties. Bottom row: links through a shared partner." else "")

  p <- p + ggplot2::labs(title = title, subtitle = sub, caption = .sn_model_caption(env),
                         x = "Ministep (one decision opportunity)", y = "Degree")

  if (isTRUE(annotate)) {
    first <- lvl[1]
    s1 <- sm[sm$facet == first, ]
    yv <- if (ncol == 1) Kdf$value[Kdf$channel == first] else Kdf$value
    y_rng <- c(min(0, min(yv)), max(yv) * 1.12)
    notes <- NULL
    ## "mean" at the right end of the first panel's line.
    end_lab <- data.frame(channel = factor(first, levels = lvl),
                          x = s1$step[nrow(s1)], y = s1$smooth[nrow(s1)], label = "mean")
    p <- p + ggplot2::geom_text(data = end_lab, ggplot2::aes(.data$x, .data$y, label = .data$label),
                                inherit.aes = FALSE, hjust = 1, vjust = -0.7, size = 3,
                                fontface = "bold")
    if (!is.null(shocks)) {
      b <- shocks$step[1]
      post <- s1[s1$step >= b, ]
      tx <- b + 0.3 * (x_rng[2] - b)
      ty <- stats::approx(post$step, post$smooth, xout = tx, rule = 2)$y
      pre_v  <- mean(s1$mean[s1$step < b & s1$step >= b - 0.2 * diff(x_rng)])
      post_v <- mean(s1$mean[s1$step >= x_rng[2] - 0.15 * diff(x_rng)])
      ## Text in the post-shock region, on the side the series moves away
      ## from, starting just right of the shock line.
      up <- post_v < mean(y_rng)
      yt <- if (up) y_rng[2] - 0.02 * diff(y_rng) else y_rng[1] + 0.12 * diff(y_rng)
      pl <- list(xt = b + 0.02 * diff(x_rng), yt = yt, hjust = 0,
                 vjust = if (up) 1 else 0,
                 xs = b + 0.12 * diff(x_rng),
                 ys = if (up) yt - 0.17 * diff(y_rng) else yt + 0.17 * diff(y_rng))
      notes <- data.frame(
        channel = factor(first, levels = lvl), x = tx, y = ty,
        label = sprintf("shock: %s\nmean %s %s from %.1f to %.1f",
                        shocks$label[1], .sn_k_info$plain[match(channels[1], .sn_k_info$channel)],
                        if (post_v < pre_v) "falls" else "rises", pre_v, post_v),
        pl, stringsAsFactors = FALSE)
    } else {
      ts <- .sn_settle_step(s1$step, s1$smooth)
      if (!is.na(ts)) {
        ty <- s1$smooth[match(ts, s1$step)]
        lev <- mean(s1$mean[s1$step >= x_rng[2] - 0.15 * diff(x_rng)])
        pl <- .sn_place(ts, ty, x_rng, y_rng, prefer = "below")
        notes <- data.frame(
          channel = factor(first, levels = lvl), x = ts, y = ty,
          label = sprintf("levels off near %.1f\nfrom about ministep %d", lev, round(ts)),
          pl, stringsAsFactors = FALSE)
      }
    }
    if (!is.null(notes)) p <- p + .sn_note_layers(notes)
  }
  p
}

.sn_cap <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))


# -----------------------------------------------------------------------------
# Utility decomposition
# -----------------------------------------------------------------------------

## Plain names for the effects a utility panel can show.
.sn_effect_plain <- c(
  density = "density: each tie held",
  inPop   = "popularity (inPop): ties to popular components",
  outAct  = "scope (outAct): breadth of the portfolio",
  XWX     = "influence (XWX): pairs linked in W held together")
.sn_effect_short <- c(density = "density", inPop = "popularity (inPop)",
                      outAct = "scope (outAct)", XWX = "influence (XWX)")
.sn_effect_cols <- c(UTILITY = "black", density = "grey45", inPop = "#CC79A7",
                     outAct = "#56B4E9", XWX = "#009E73")

## Utility decomposition from the long data frame of get_actor_utility_effects()
## (columns chain_step_id, value, effect_level with UTILITY first, strategy).
.sn_utility_plot <- function(env, act, use_thetas = TRUE, loess_span = 0.5,
                             point_alpha_dimmer = 1, annotate = TRUE) {
  act <- as.data.frame(act)
  lev <- levels(droplevels(factor(act$effect_level)))
  lev <- c(intersect("UTILITY", lev), setdiff(lev, "UTILITY"))
  strip <- ifelse(lev == "UTILITY",
                  if (use_thetas) "Total utility: the sum of the other panels" else "Utility",
                  ifelse(lev %in% names(.sn_effect_plain), .sn_effect_plain[lev], lev))
  act$panel <- factor(strip[match(as.character(act$effect_level), lev)], levels = strip)
  act$eff <- as.character(act$effect_level)

  strat <- as.character(act$strategy)
  strat[is.na(strat)] <- "none"
  act$group <- strat
  groups <- unique(strat)
  by_strategy <- length(groups) > 1

  npoints <- nrow(act)
  point_size <- min(1.6, 4 / log10(max(npoints, 10)))
  point_alpha <- min(1, 15 / sqrt(max(npoints, 1))) * point_alpha_dimmer
  x_rng <- range(act$chain_step_id)
  shocks <- .sn_shock_boundaries(env)

  p <- ggplot2::ggplot(act, ggplot2::aes(.data$chain_step_id, .data$value)) +
    .sn_shock_layers(shocks, x_rng[2]) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3)
  if (by_strategy) {
    gcols <- stats::setNames(rep_len(.sn_actor_pool, length(groups)), groups)
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = .data$group), shape = 1,
                          alpha = point_alpha, size = point_size) +
      ggplot2::geom_smooth(ggplot2::aes(color = .data$group, group = .data$group),
                           method = "loess", formula = y ~ x, span = loess_span,
                           se = FALSE, linewidth = 0.8) +
      ggplot2::scale_color_manual(values = gcols, labels = paste("Actors: strategy", groups),
                                  name = NULL) +
      ggplot2::guides(color = ggplot2::guide_legend(
        override.aes = list(alpha = 1, shape = NA, linewidth = 1)))
  } else {
    ecols <- ifelse(lev %in% names(.sn_effect_cols), .sn_effect_cols[lev], "grey40")
    names(ecols) <- lev
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = .data$eff), shape = 1,
                          alpha = point_alpha, size = point_size) +
      ggplot2::geom_smooth(ggplot2::aes(color = .data$eff, group = .data$eff),
                           method = "loess", formula = y ~ x, span = loess_span,
                           se = FALSE, linewidth = 0.9) +
      ggplot2::scale_color_manual(values = ecols, guide = "none")
  }
  ncol <- if (length(lev) > 3) 2 else 1
  p <- p + ggplot2::facet_wrap(~ panel, ncol = ncol, scales = "free_y") +
    theme_searchnet() +
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = 9),
                   panel.spacing = grid::unit(0.8, "lines"))

  ## Title: the largest term at the end of the run.
  eff_only <- act[act$eff != "UTILITY" & act$chain_step_id >= x_rng[2] - 0.2 * diff(x_rng), ]
  title <- if (use_thetas) "Where the actors' utility comes from" else
    "The statistics behind the actors' utility"
  big <- NULL
  if (nrow(eff_only) && use_thetas) {
    m <- tapply(eff_only$value, eff_only$eff, mean)
    m <- m[is.finite(m)]
    if (length(m) && max(abs(m)) > 0) {
      big <- names(m)[which.max(abs(m))]
      nm <- if (big %in% names(.sn_effect_short)) .sn_effect_short[[big]] else big
      title <- if (m[[big]] > 0)
        sprintf("By the end of the run, %s is the largest part of utility", nm) else
        sprintf("By the end of the run, %s is the largest cost", nm)
    }
  }
  sub <- paste0(
    if (use_thetas) "Each panel: one effect's contribution to an actor's utility (statistic x weight)."
    else "Each panel: one effect's statistic for an actor, before weighting.",
    "\nFaint points: one actor at one ministep. Line: the smoothed average",
    if (by_strategy) " for each strategy." else ".")
  p <- p + ggplot2::labs(title = title, subtitle = sub, caption = .sn_model_caption(env),
                         x = "Ministep (one decision opportunity)",
                         y = if (use_thetas) "Contribution to utility" else "Statistic")

  if (isTRUE(annotate)) {
    if (!is.null(big)) {
      pan <- strip[match(big, lev)]
      d <- act[act$eff == big, ]
      sm <- .sn_smooth_mean(transform(d, facet = "x"), "facet", loess_span)
      lab <- data.frame(panel = factor(pan, levels = strip), x = sm$step[nrow(sm)],
                        y = sm$smooth[nrow(sm)],
                        label = sprintf("largest term: %+.1f", mean(d$value[d$chain_step_id >=
                                          x_rng[2] - 0.2 * diff(x_rng)])))
      ## Below the line end when it sits in the upper half of its panel.
      high <- lab$y > mean(range(d$value))
      p <- p + ggplot2::geom_text(data = lab, ggplot2::aes(.data$x, .data$y, label = .data$label),
                                  inherit.aes = FALSE, hjust = 1,
                                  vjust = if (high) 2 else -1, size = 3,
                                  fontface = "bold", color = .sn_role[["ink"]])
    }
    if (!is.null(shocks)) {
      lab <- data.frame(panel = factor(strip[1], levels = strip), x = shocks$step,
                        label = paste0(" shock: ", shocks$label))
      p <- p + ggplot2::geom_text(data = lab, ggplot2::aes(.data$x, Inf, label = .data$label),
                                  inherit.aes = FALSE, hjust = 0, vjust = 1.4, size = 2.9,
                                  color = .sn_role[["event"]])
    }
  }
  p
}
