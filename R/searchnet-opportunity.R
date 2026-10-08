## ---------------------------------------------------------------------------
## Opportunity table: realized ministeps per group per arm
## ---------------------------------------------------------------------------

#' Tabulate realized ministep opportunities per group and arm
#'
#' Counts, from a run's ministep chain, how many decision opportunities each
#' group of actors actually received and how many of them changed a tie.
#'
#' Since searchnet 0.11.0 a run simulates one unit of model time, so the
#' number of opportunities is random and a rate effect changes the total as
#' well as each group's share; the table shows how many opportunities each
#' group actually received. Under \code{search_rsiena(path = "legacy_replay")}
#' (and every run before 0.11.0) the total was a fixed ministep budget, so
#' raising one group's rate could only raise its share by lowering every
#' other group's, and a rate-effect study could read a reallocation of a
#' zero-sum budget as an effect on the groups that lost opportunities.
#'
#' @param x A \code{SaomNkRSienaBiEnv} after a run (its \code{chain_stats}
#'   is read), a ministep-chain data frame with at least \code{id_from} and
#'   \code{stability} columns (the layout of \code{env$chain_stats}), or a
#'   list of either, one element per arm. List names become arm labels.
#' @param groups Actor-to-group assignment: a vector of length \eqn{M} (one
#'   label per actor, in actor order), or a list of such vectors, one per arm.
#'   \code{NULL} (default) uses \code{env$get_actor_strategies()} when
#'   \code{x} is an environment and a single group \code{"all"} otherwise.
#' @param M Number of actors. Needed only when \code{x} is a bare chain and
#'   \code{groups} is \code{NULL}; otherwise taken from the environment or
#'   from \code{length(groups)}.
#' @param dv Name of the dependent variable whose ministeps are counted.
#'   Default \code{"self$bipartite_rsienaDV"}; \code{NULL} counts every row.
#'   Ignored when the chain has no \code{dv_varname} column.
#' @return A data frame of class \code{"searchnet_opportunity_table"}, one row
#'   per arm and group, with columns \code{arm}, \code{group},
#'   \code{n_actors}, \code{opportunities}, \code{changes} (ministeps that
#'   toggled a tie), \code{stays}, \code{opportunity_share} (of the arm's
#'   total), \code{actor_share} (the share an equal-rate allocation would
#'   give), \code{opportunities_per_actor}, \code{changes_per_actor},
#'   \code{change_rate} (changes per opportunity) and
#'   \code{arm_total_opportunities}.
#' @examples
#' ## A hand-built chain: actors 1-2 in group "a", actors 3-4 in group "b".
#' chain <- data.frame(id_from   = c(1, 1, 2, 3, 1, 4, 1, 2),
#'                     stability = c(FALSE, TRUE, FALSE, FALSE, FALSE,
#'                                   TRUE, FALSE, FALSE))
#' searchnet_opportunity_table(chain, groups = c("a", "a", "b", "b"))
#' @export
searchnet_opportunity_table <- function(x, groups = NULL, M = NULL,
                                        dv = "self$bipartite_rsienaDV") {
  is_single <- inherits(x, "SaomNkRSienaBiEnv") || is.data.frame(x)
  arms <- if (is_single) list(x) else x
  if (!is.list(arms) || !length(arms))
    stop("`x` must be an environment, a chain data frame, or a non-empty ",
         "list of them.", call. = FALSE)
  arm_names <- names(arms)
  if (is.null(arm_names) || any(!nzchar(arm_names)))
    arm_names <- if (is_single) "arm1" else paste0("arm", seq_along(arms))

  grp_list <- if (is.list(groups) && !is.data.frame(groups)) {
    if (length(groups) != length(arms))
      stop("A list of `groups` needs one element per arm.", call. = FALSE)
    groups
  } else {
    rep(list(groups), length(arms))
  }

  rows <- vector("list", length(arms))
  for (a in seq_along(arms)) {
    xi <- arms[[a]]
    gi <- grp_list[[a]]
    Mi <- M
    if (inherits(xi, "SaomNkRSienaBiEnv")) {
      chain <- xi$chain_stats
      if (is.null(chain))
        stop(sprintf("Arm '%s' has no ministep chain (env$chain_stats); run ",
                     "the simulation first.", arm_names[a]), call. = FALSE)
      if (is.null(Mi)) Mi <- xi$M
      if (is.null(gi)) gi <- as.character(xi$get_actor_strategies())
    } else if (is.data.frame(xi)) {
      chain <- xi
    } else {
      stop(sprintf("Arm '%s' is neither an environment nor a chain data frame.",
                   arm_names[a]), call. = FALSE)
    }
    chain <- as.data.frame(chain)
    if (!all(c("id_from", "stability") %in% names(chain)))
      stop("A chain needs `id_from` and `stability` columns (the layout of ",
           "env$chain_stats).", call. = FALSE)
    if (!is.null(dv) && "dv_varname" %in% names(chain))
      chain <- chain[chain$dv_varname == dv, , drop = FALSE]

    ego <- as.integer(chain$id_from)
    changed <- if ("tie_change" %in% names(chain)) {
      as.logical(chain$tie_change)
    } else {
      !as.logical(chain$stability)
    }
    if (is.null(Mi)) Mi <- if (!is.null(gi)) length(gi) else max(c(ego, 0L))
    if (is.null(gi)) gi <- rep("all", Mi)
    if (length(gi) != Mi)
      stop(sprintf("Arm '%s': `groups` has length %d but there are %d actors.",
                   arm_names[a], length(gi), Mi), call. = FALSE)
    if (length(ego) && (any(is.na(ego)) || any(ego < 1L) || any(ego > Mi)))
      stop(sprintf("Arm '%s': chain names an actor outside 1..%d.",
                   arm_names[a], Mi), call. = FALSE)

    opp_actor <- tabulate(ego, nbins = Mi)
    chg_actor <- tabulate(ego[changed %in% TRUE], nbins = Mi)
    gi <- as.character(gi)
    glev <- unique(gi)
    total <- sum(opp_actor)
    rows[[a]] <- do.call(rbind, lapply(glev, function(g) {
      idx <- which(gi == g)
      opp <- sum(opp_actor[idx]); chg <- sum(chg_actor[idx])
      data.frame(
        arm = arm_names[a], group = g, n_actors = length(idx),
        opportunities = opp, changes = chg, stays = opp - chg,
        opportunity_share = if (total > 0) opp / total else NA_real_,
        actor_share = length(idx) / Mi,
        opportunities_per_actor = opp / length(idx),
        changes_per_actor = chg / length(idx),
        change_rate = if (opp > 0) chg / opp else NA_real_,
        arm_total_opportunities = total,
        stringsAsFactors = FALSE
      )
    }))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("searchnet_opportunity_table", "data.frame")
  out
}

#' @export
print.searchnet_opportunity_table <- function(x, digits = 3, ...) {
  cat("Realized ministep opportunities per group and arm\n")
  totals <- unique(x[, c("arm", "arm_total_opportunities")])
  cat(sprintf("  arm totals: %s\n",
              paste(sprintf("%s = %d", totals$arm, totals$arm_total_opportunities),
                    collapse = ", ")))
  cat("  Arm totals are random and respond to rate effects (since 0.11.0; a\n",
      " legacy-replay run had a fixed ministep budget). Compare opportunity_share with\n",
      " actor_share (the equal-rate allocation).\n\n", sep = "")
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  num <- vapply(df, is.double, logical(1))
  df[num] <- lapply(df[num], round, digits = digits)
  print(df[, setdiff(names(df), "arm_total_opportunities")], row.names = FALSE)
  invisible(x)
}
