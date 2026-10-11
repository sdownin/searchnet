# plot-unified.R
# Unified entry points for the exploration/exploitation and multi-wave plot
# families, the deprecated saomnk_* wrappers that forward to them, and the
# package naming policy.
#
# The implementations live, unchanged, as internal functions in
# plot-exploration.R (.searchnet_ee_*) and plot-multiwave.R (.searchnet_mw_*).
# The unified functions only dispatch; every argument after the selector is
# passed through, so a call through the new name and a call through the old
# name build the same object.


# ---- naming policy ------------------------------------------------------------

#' Naming policy for the searchnet API
#'
#' New exported functions use the `searchnet_` prefix. Names that begin with
#' `saomnk_` are frozen: no new `saomnk_` functions are added, and existing
#' ones keep their names, arguments, and return values. Where several
#' `saomnk_` functions drew variants of one plot, a single `searchnet_`
#' function now selects the variant with an argument
#' ([searchnet_plot_exploration()], [searchnet_plot_multiwave()]), and names
#' that carried a version or development tag have a descriptive replacement
#' ([searchnet_plot_cumulative_entry()], [searchnet_plot_new_component_shocks()]).
#'
#' The superseded names remain exported as thin wrappers that forward to the
#' replacement and return the same object. Each emits a deprecation warning
#' the first time it is called in a session, not on every call, so a script
#' that loops over one is not buried in repeats. The wrappers stay at least
#' until one minor release after the SAOM-NK software paper in the Journal of
#' Statistical Software is accepted. The full old-to-new mapping is in
#' [searchnet-deprecated-plots].
#'
#' @name searchnet-naming
#' @seealso [searchnet-deprecated-plots]
NULL


# ---- internal helpers ---------------------------------------------------------

## Warn once per session for a deprecated plot name. The flag lives in the
## same environment as the package's other deprecated aliases (utils.R), so
## .searchnet_reset_deprecations() re-arms it.
.searchnet_deprecated_plot <- function(old, new) {
  flag <- paste0("plot_", old)
  if (!isTRUE(.searchnet_deprecation_flags[[flag]])) {
    assign(flag, TRUE, envir = .searchnet_deprecation_flags)
    .Deprecated(new = new, package = "searchnet",
                msg = paste0(old, "() is deprecated; use ", new, ". ",
                             "Same arguments, same result. ",
                             "(Shown once per session; see ?searchnet-naming.)"))
  }
  invisible(NULL)
}

## Forward the calling wrapper's supplied arguments to `fun`. Arguments the
## caller left missing are left out, so the implementation's own defaults
## apply exactly as before; supplied ones are passed as symbols evaluated in
## the wrapper's frame, so lazy evaluation is preserved. With rename_env,
## the wrapper's `env` argument becomes the unified function's `x`.
.searchnet_forward <- function(fun, extra = list(), rename_env = TRUE) {
  frame <- parent.frame()
  fmls <- names(formals(sys.function(sys.parent())))
  supplied <- fmls[!vapply(fmls, function(a) eval(call("missing", as.name(a)), frame),
                           logical(1))]
  args <- lapply(supplied, as.name)
  names(args) <- supplied
  if (rename_env) names(args)[names(args) == "env"] <- "x"
  eval(as.call(c(list(fun), extra, args)), frame)
}


# ---- searchnet_plot_exploration ---------------------------------------------

.searchnet_exploration_types <- c(
  trajectory         = ".searchnet_ee_trajectory",
  trajectory_density = ".searchnet_ee_trajectory_density",
  phase_space        = ".searchnet_ee_phase_space",
  portfolio          = ".searchnet_ee_portfolio",
  strategy_groups    = ".searchnet_ee_strategy_groups",
  strategy_facets    = ".searchnet_ee_strategy_facets",
  strategy_result    = ".searchnet_ee_strategy_result",
  risk_subsidies     = ".searchnet_ee_risk_subsidies")

#' Plot exploration versus exploitation over a simulation run
#'
#' One entry point for the exploration/exploitation plot family. Exploitation
#' is an actor's share of ties to the original components (C1-C8) and
#' exploration its share of ties to the new components (C9-C16); `type`
#' selects which view of those shares is drawn.
#'
#' | `type` | What it shows | Replaces |
#' |---|---|---|
#' | `"trajectory"` | Per-actor and strategy-mean shares over chain steps | `saomnk_plot_exploration_exploitation()` |
#' | `"trajectory_density"` | The same trajectories with a side density panel per activity type, styled like the utility plot | `saomnk_plot_exploration_exploitation_consistent()` |
#' | `"phase_space"` | Actor positions in the exploitation-by-exploration plane, faceted by strategy | `saomnk_plot_exploration_exploitation_phase()` |
#' | `"portfolio"` | Activity-portfolio evolution with an optional subsidized-minus-control difference panel | `saomnk_plot_exploration_exploitation_improved()` |
#' | `"strategy_groups"` | Strategy-group means of both shares, computed from the run | `saomnk_plot_exploration_exploitation_by_strategy()` |
#' | `"strategy_facets"` | Activity type by strategy group in a facet grid, from a supplied `metrics_df` | `saomnk_plot_exploration_exploitation_faceted()` |
#' | `"strategy_result"` | Strategy-group means from a precomputed `result` (its `$metrics`) | `saomnk_plot_strategy_exploration_exploitation()` |
#' | `"risk_subsidies"` | Risk-adjusted exploration and exploitation under subsidies, with social-logic constraints | `saomnk_plot_exploration_exploitation_subsidies()` |
#'
#' @param x A simulated `SaomNkRSienaBiEnv` environment.
#' @param type Which view to draw; see the table above.
#' @param ... Further arguments for the selected view, with the same names
#'   and defaults as the function it replaces (for example `actor_ids`,
#'   `thin_factor`, `loess_span`, `plot_return`; `metrics_df` for
#'   `"strategy_facets"` and `"risk_subsidies"`; `result` for
#'   `"strategy_result"`; `time_window` for `"phase_space"`).
#' @return The object the replaced function returned for the same arguments:
#'   a ggplot, or a combined plot from `ggpubr::ggarrange()` for the views that
#'   add a side panel.
#' @seealso [searchnet-naming] for the naming policy;
#'   [searchnet-deprecated-plots] for the old names.
#' @examples
#' \dontrun{
#' searchnet_plot_exploration(env)
#' searchnet_plot_exploration(env, type = "phase_space", time_window = 25)
#' }
#' @export
searchnet_plot_exploration <- function(x,
                                       type = c("trajectory", "trajectory_density",
                                                "phase_space", "portfolio",
                                                "strategy_groups", "strategy_facets",
                                                "strategy_result", "risk_subsidies"),
                                       ...) {
  type <- match.arg(type)
  impl <- get(.searchnet_exploration_types[[type]], envir = asNamespace("searchnet"),
              inherits = FALSE)
  impl(x, ...)
}


# ---- searchnet_plot_multiwave -----------------------------------------------

## what -> named vector of by-options -> implementation. The first by-option
## is the default when `by = NULL`; an empty vector means `by` must be NULL.
.searchnet_multiwave_table <- list(
  list      = c(.none = ".searchnet_mw_list"),
  K_4panel  = c(.none = ".searchnet_mw_K_4panel"),
  K_AA      = c(strategy = ".searchnet_mw_K_AA"),
  K_AC      = c(strategy = ".searchnet_mw_K_AC"),
  K_CA      = c(strategy = ".searchnet_mw_K_CA"),
  K_CC      = c(strategy = ".searchnet_mw_K_CC"),
  utility   = c(strategy = ".searchnet_mw_utility_strategy",
                actor    = ".searchnet_mw_utility_actor"),
  utility_density = c(strategy = ".searchnet_mw_utility_density"),
  utility_ridge   = c(strategy = ".searchnet_mw_utility_ridge"))

#' Plot multi-wave simulation results
#'
#' One entry point for the multi-wave plot family, drawn after
#' `env$search_rsiena_multiwave_run()` and
#' `env$search_rsiena_multiwave_process_results()`. `what` selects the
#' quantity and `by` the grouping.
#'
#' | `what` | `by` | What it shows | Replaces `saomnk_search_rsiena_multiwave_plot...` |
#' |---|---|---|---|
#' | `"list"` | (none) | A named list of several of the plots below, chosen by `type` (all when `type` is empty) | `()` |
#' | `"K_4panel"` | (none) | The four K degree summaries in one 2 x 2 figure | `_K_4panel()` |
#' | `"K_AA"`, `"K_AC"`, `"K_CA"`, `"K_CC"` | `"strategy"` | One K degree over chain steps, summarized by actor strategy | `_K_AA_strategy_summary()` etc. |
#' | `"utility"` | `"strategy"` | Actor utility per wave with strategy smooths and a side density panel | `_actor_utility_strategy_summary()` |
#' | `"utility"` | `"actor"` | Actor utility per wave with one smooth per actor, colored by strategy | `_actor_utility_by_strategy()` |
#' | `"utility_density"` | `"strategy"` | Utility densities by strategy, first versus second half of each wave | `_actor_utility_density_by_strategy()` |
#' | `"utility_ridge"` | `"strategy"` | Ridge densities of utility by stabilization period and strategy | `_utility_ridge_density_by_strategy()` |
#'
#' @param x A `SaomNkRSienaBiEnv` environment with processed multi-wave
#'   results.
#' @param what Which quantity to plot; see the table above.
#' @param by Grouping, where `what` offers a choice. `NULL` takes the first
#'   option listed for that `what` (`"strategy"` where there is one).
#' @param ... Further arguments for the selected plot, with the same names
#'   and defaults as the function it replaces (for example `actor_ids`,
#'   `wave_ids`, `thin_factor`, `thin_wave_factor`, `smooth_method`,
#'   `return_plot`; `type` for `what = "list"`).
#' @return The object the replaced function returned for the same arguments:
#'   a ggplot or combined figure, or for `what = "list"` a named list of them.
#' @seealso [searchnet-naming] for the naming policy;
#'   [searchnet-deprecated-plots] for the old names.
#' @examples
#' \dontrun{
#' searchnet_plot_multiwave(env, what = "K_4panel")
#' searchnet_plot_multiwave(env, what = "utility", by = "actor", thin_factor = 2)
#' }
#' @export
searchnet_plot_multiwave <- function(x,
                                     what = c("list", "K_4panel", "K_AA", "K_AC",
                                              "K_CA", "K_CC", "utility",
                                              "utility_density", "utility_ridge"),
                                     by = NULL, ...) {
  what <- match.arg(what)
  opts <- .searchnet_multiwave_table[[what]]
  if (identical(names(opts), ".none")) {
    if (!is.null(by))
      stop(sprintf("`by` does not apply to what = \"%s\"; leave it NULL.", what),
           call. = FALSE)
    impl_name <- opts[[1]]
  } else {
    if (is.null(by)) by <- names(opts)[1]
    if (!is.character(by) || length(by) != 1L || !by %in% names(opts))
      stop(sprintf("`by` for what = \"%s\" must be one of: %s.", what,
                   paste0("\"", names(opts), "\"", collapse = ", ")), call. = FALSE)
    impl_name <- opts[[by]]
  }
  impl <- get(impl_name, envir = asNamespace("searchnet"), inherits = FALSE)
  impl(x, ...)
}


# ---- deprecated names -----------------------------------------------------------

#' Deprecated plot function names
#'
#' These names are superseded under the package [naming policy][searchnet-naming].
#' Each is a thin wrapper: it takes the same arguments as before, forwards
#' them to its replacement, and returns the same object. The first call in a
#' session emits a deprecation warning; later calls are silent.
#'
#' | Deprecated | Replacement |
#' |---|---|
#' | `saomnk_plot_exploration_exploitation()` | `searchnet_plot_exploration(type = "trajectory")` |
#' | `saomnk_plot_exploration_exploitation_consistent()` | `searchnet_plot_exploration(type = "trajectory_density")` |
#' | `saomnk_plot_exploration_exploitation_phase()` | `searchnet_plot_exploration(type = "phase_space")` |
#' | `saomnk_plot_exploration_exploitation_improved()` | `searchnet_plot_exploration(type = "portfolio")` |
#' | `saomnk_plot_exploration_exploitation_by_strategy()` | `searchnet_plot_exploration(type = "strategy_groups")` |
#' | `saomnk_plot_exploration_exploitation_faceted()` | `searchnet_plot_exploration(type = "strategy_facets")` |
#' | `saomnk_plot_strategy_exploration_exploitation()` | `searchnet_plot_exploration(type = "strategy_result")` |
#' | `saomnk_plot_exploration_exploitation_subsidies()` | `searchnet_plot_exploration(type = "risk_subsidies")` |
#' | `saomnk_search_rsiena_multiwave_plot()` | `searchnet_plot_multiwave(what = "list")` |
#' | `saomnk_search_rsiena_multiwave_plot_K_4panel()` | `searchnet_plot_multiwave(what = "K_4panel")` |
#' | `saomnk_search_rsiena_multiwave_plot_K_AA_strategy_summary()` | `searchnet_plot_multiwave(what = "K_AA", by = "strategy")` |
#' | `saomnk_search_rsiena_multiwave_plot_K_AC_strategy_summary()` | `searchnet_plot_multiwave(what = "K_AC", by = "strategy")` |
#' | `saomnk_search_rsiena_multiwave_plot_K_CA_strategy_summary()` | `searchnet_plot_multiwave(what = "K_CA", by = "strategy")` |
#' | `saomnk_search_rsiena_multiwave_plot_K_CC_strategy_summary()` | `searchnet_plot_multiwave(what = "K_CC", by = "strategy")` |
#' | `saomnk_search_rsiena_multiwave_plot_actor_utility_strategy_summary()` | `searchnet_plot_multiwave(what = "utility", by = "strategy")` |
#' | `saomnk_search_rsiena_multiwave_plot_actor_utility_by_strategy()` | `searchnet_plot_multiwave(what = "utility", by = "actor")` |
#' | `saomnk_search_rsiena_multiwave_plot_actor_utility_density_by_strategy()` | `searchnet_plot_multiwave(what = "utility_density", by = "strategy")` |
#' | `saomnk_search_rsiena_multiwave_plot_utility_ridge_density_by_strategy()` | `searchnet_plot_multiwave(what = "utility_ridge", by = "strategy")` |
#' | `saomnk_plot_market_entry_survival_v0()` | `searchnet_plot_cumulative_entry()` |
#' | `saomnk_plot_K_AC_NEW_shocks()` | `searchnet_plot_new_component_shocks()` |
#'
#' @param env A `SaomNkRSienaBiEnv` environment, passed on as the
#'   replacement's first argument.
#' @param actor_ids,component_ids,wave_ids,thin_factor,thin_pct,thin_wave_factor,smooth_method,show_points,show_individuals,show_group_means,show_difference,loess_span,point_alpha,point_alpha_dimmer,line_alpha,group_line_size,se_ribbon,ylim,plot_return,plot_save,plot_file,plot_dir,time_window,show_trajectories,metrics_df,result,show_se_ribbon,shock_time,type,rolling_window,show_utility_points,show_strategy_means,append_plot,histogram_position,scale_utility,return_plot,show_legend,show_title,plot_periods,n,environ_params,structure_model,steps_per_actor,theta_shocks,conf_level,verbose
#'   Passed unchanged to the replacement; see its documentation.
#' @return The replacement's return value.
#' @seealso [searchnet_plot_exploration()], [searchnet_plot_multiwave()],
#'   [searchnet_plot_cumulative_entry()],
#'   [searchnet_plot_new_component_shocks()], [searchnet-naming].
#' @name searchnet-deprecated-plots
#' @keywords internal
NULL

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_plot_exploration_exploitation <- function(env,
                                                 actor_ids = c(),
                                                 thin_factor = 1,
                                                 thin_pct = 1,
                                                 smooth_method = "loess",
                                                 show_points = TRUE,
                                                 show_individuals = TRUE,
                                                 show_group_means = TRUE,
                                                 loess_span = 0.3,
                                                 point_alpha_dimmer = 1,
                                                 line_alpha = 0.5,
                                                 group_line_size = 2,
                                                 ylim = NULL,
                                                 plot_return = TRUE,
                                                 plot_save = FALSE,
                                                 plot_file = "",
                                                 plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_plot_exploration_exploitation", 'searchnet_plot_exploration(type = "trajectory")')
  .searchnet_forward(searchnet_plot_exploration, list(type = "trajectory"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_plot_exploration_exploitation_consistent <- function(env,
                                                            actor_ids = c(),
                                                            thin_factor = 1,
                                                            thin_pct = 1,
                                                            smooth_method = "loess",
                                                            show_points = TRUE,
                                                            show_individuals = FALSE,
                                                            show_group_means = TRUE,
                                                            loess_span = 0.3,
                                                            point_alpha_dimmer = 1,
                                                            line_alpha = 0.5,
                                                            group_line_size = 2,
                                                            se_ribbon = TRUE,
                                                            ylim = c(0, 1),
                                                            plot_return = TRUE,
                                                            plot_save = FALSE,
                                                            plot_file = "",
                                                            plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_plot_exploration_exploitation_consistent", 'searchnet_plot_exploration(type = "trajectory_density")')
  .searchnet_forward(searchnet_plot_exploration, list(type = "trajectory_density"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_plot_exploration_exploitation_phase <- function(env,
                                                       time_window = 50,
                                                       show_trajectories = TRUE,
                                                       plot_return = TRUE) {
  .searchnet_deprecated_plot("saomnk_plot_exploration_exploitation_phase", 'searchnet_plot_exploration(type = "phase_space")')
  .searchnet_forward(searchnet_plot_exploration, list(type = "phase_space"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_plot_exploration_exploitation_improved <- function(env,
                                                          actor_ids = c(),
                                                          thin_factor = 1,
                                                          show_points = FALSE,
                                                          show_individuals = FALSE,
                                                          show_group_means = TRUE,
                                                          show_difference = TRUE,
                                                          loess_span = 0.2,
                                                          point_alpha = 0.2,
                                                          line_alpha = 0.5,
                                                          group_line_size = 2.5,
                                                          se_ribbon = TRUE,
                                                          ylim = c(0, 1),
                                                          plot_return = TRUE,
                                                          plot_save = FALSE,
                                                          plot_file = "",
                                                          plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_plot_exploration_exploitation_improved", 'searchnet_plot_exploration(type = "portfolio")')
  .searchnet_forward(searchnet_plot_exploration, list(type = "portfolio"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_plot_exploration_exploitation_faceted <- function(env,
                                                         metrics_df,
                                                         loess_span = 0.3,
                                                         show_points = FALSE) {
  .searchnet_deprecated_plot("saomnk_plot_exploration_exploitation_faceted", 'searchnet_plot_exploration(type = "strategy_facets")')
  .searchnet_forward(searchnet_plot_exploration, list(type = "strategy_facets"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_plot_exploration_exploitation_by_strategy <- function(env,
                                                             actor_ids = c(),
                                                             thin_factor = 1,
                                                             show_points = TRUE,
                                                             show_individuals = FALSE,
                                                             show_group_means = TRUE,
                                                             loess_span = 0.3,
                                                             point_alpha = 0.3,
                                                             line_alpha = 0.5,
                                                             group_line_size = 2,
                                                             se_ribbon = TRUE,
                                                             ylim = c(0, 1),
                                                             plot_return = TRUE,
                                                             plot_save = FALSE,
                                                             plot_file = "",
                                                             plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_plot_exploration_exploitation_by_strategy", 'searchnet_plot_exploration(type = "strategy_groups")')
  .searchnet_forward(searchnet_plot_exploration, list(type = "strategy_groups"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_plot_strategy_exploration_exploitation <- function(env,
                                                          result,
                                                          show_points = TRUE,
                                                          show_group_means = TRUE,
                                                          group_line_size = 2) {
  .searchnet_deprecated_plot("saomnk_plot_strategy_exploration_exploitation", 'searchnet_plot_exploration(type = "strategy_result")')
  .searchnet_forward(searchnet_plot_exploration, list(type = "strategy_result"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_plot_exploration_exploitation_subsidies <- function(env,
                                                           metrics_df = NULL,
                                                           show_points = FALSE,
                                                           show_se_ribbon = TRUE,
                                                           loess_span = 0.3,
                                                           shock_time = NULL) {
  .searchnet_deprecated_plot("saomnk_plot_exploration_exploitation_subsidies", 'searchnet_plot_exploration(type = "risk_subsidies")')
  .searchnet_forward(searchnet_plot_exploration, list(type = "risk_subsidies"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_search_rsiena_multiwave_plot <- function(env,
                                                type = c(),
                                                rolling_window = 10,
                                                actor_ids = c(),
                                                component_ids = c(),
                                                wave_ids = c(),
                                                thin_factor = 1,
                                                thin_wave_factor = 1,
                                                smooth_method = "loess",
                                                show_utility_points = TRUE,
                                                show_strategy_means = TRUE,
                                                append_plot = FALSE,
                                                histogram_position = "identity",
                                                scale_utility = TRUE,
                                                return_plot = TRUE,
                                                plot_file = NA,
                                                plot_dir = NA,
                                                loess_span = 0.4) {
  .searchnet_deprecated_plot("saomnk_search_rsiena_multiwave_plot", 'searchnet_plot_multiwave(what = "list")')
  .searchnet_forward(searchnet_plot_multiwave, list(what = "list"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_search_rsiena_multiwave_plot_K_4panel <- function(env,
                                                         actor_ids = c(),
                                                         component_ids = c(),
                                                         wave_ids = c(),
                                                         thin_factor = 1,
                                                         thin_wave_factor = 1,
                                                         smooth_method = "loess",
                                                         show_utility_points = TRUE,
                                                         return_plot = TRUE,
                                                         plot_file = NA,
                                                         plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_search_rsiena_multiwave_plot_K_4panel", 'searchnet_plot_multiwave(what = "K_4panel")')
  .searchnet_forward(searchnet_plot_multiwave, list(what = "K_4panel"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_search_rsiena_multiwave_plot_K_AA_strategy_summary <- function(env,
                                                                      actor_ids = c(),
                                                                      wave_ids = c(),
                                                                      thin_factor = 1,
                                                                      thin_wave_factor = 1,
                                                                      smooth_method = "loess",
                                                                      show_utility_points = TRUE,
                                                                      show_legend = TRUE,
                                                                      show_title = TRUE,
                                                                      return_plot = TRUE,
                                                                      plot_file = NA,
                                                                      plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_search_rsiena_multiwave_plot_K_AA_strategy_summary", 'searchnet_plot_multiwave(what = "K_AA", by = "strategy")')
  .searchnet_forward(searchnet_plot_multiwave, list(what = "K_AA", by = "strategy"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_search_rsiena_multiwave_plot_K_AC_strategy_summary <- function(env,
                                                                      actor_ids = c(),
                                                                      wave_ids = c(),
                                                                      thin_factor = 1,
                                                                      thin_wave_factor = 1,
                                                                      smooth_method = "loess",
                                                                      show_utility_points = TRUE,
                                                                      show_legend = TRUE,
                                                                      show_title = TRUE,
                                                                      return_plot = TRUE,
                                                                      plot_file = NA,
                                                                      plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_search_rsiena_multiwave_plot_K_AC_strategy_summary", 'searchnet_plot_multiwave(what = "K_AC", by = "strategy")')
  .searchnet_forward(searchnet_plot_multiwave, list(what = "K_AC", by = "strategy"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_search_rsiena_multiwave_plot_K_CA_strategy_summary <- function(env,
                                                                      component_ids = c(),
                                                                      wave_ids = c(),
                                                                      thin_factor = 1,
                                                                      thin_wave_factor = 1,
                                                                      smooth_method = "loess",
                                                                      show_utility_points = TRUE,
                                                                      show_legend = TRUE,
                                                                      show_title = TRUE,
                                                                      return_plot = TRUE,
                                                                      plot_file = NA,
                                                                      plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_search_rsiena_multiwave_plot_K_CA_strategy_summary", 'searchnet_plot_multiwave(what = "K_CA", by = "strategy")')
  .searchnet_forward(searchnet_plot_multiwave, list(what = "K_CA", by = "strategy"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_search_rsiena_multiwave_plot_K_CC_strategy_summary <- function(env,
                                                                      component_ids = c(),
                                                                      wave_ids = c(),
                                                                      thin_factor = 1,
                                                                      thin_wave_factor = 1,
                                                                      smooth_method = "loess",
                                                                      show_utility_points = TRUE,
                                                                      show_legend = TRUE,
                                                                      show_title = TRUE,
                                                                      return_plot = TRUE,
                                                                      plot_file = NA,
                                                                      plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_search_rsiena_multiwave_plot_K_CC_strategy_summary", 'searchnet_plot_multiwave(what = "K_CC", by = "strategy")')
  .searchnet_forward(searchnet_plot_multiwave, list(what = "K_CC", by = "strategy"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_search_rsiena_multiwave_plot_actor_utility_strategy_summary <- function(env,
                                                                               actor_ids = c(),
                                                                               wave_ids = c(),
                                                                               thin_factor = 1,
                                                                               thin_wave_factor = 1,
                                                                               smooth_method = "loess",
                                                                               show_utility_points = TRUE,
                                                                               scale_utility = TRUE,
                                                                               return_plot = TRUE,
                                                                               plot_file = NA,
                                                                               plot_dir = NA,
                                                                               loess_span = 0.4) {
  .searchnet_deprecated_plot("saomnk_search_rsiena_multiwave_plot_actor_utility_strategy_summary", 'searchnet_plot_multiwave(what = "utility", by = "strategy")')
  .searchnet_forward(searchnet_plot_multiwave, list(what = "utility", by = "strategy"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_search_rsiena_multiwave_plot_actor_utility_by_strategy <- function(env,
                                                                          actor_ids = c(),
                                                                          thin_factor = 1,
                                                                          thin_wave_factor = 1,
                                                                          smooth_method = "loess",
                                                                          show_utility_points = TRUE,
                                                                          return_plot = TRUE,
                                                                          plot_file = NA,
                                                                          plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_search_rsiena_multiwave_plot_actor_utility_by_strategy", 'searchnet_plot_multiwave(what = "utility", by = "actor")')
  .searchnet_forward(searchnet_plot_multiwave, list(what = "utility", by = "actor"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_search_rsiena_multiwave_plot_actor_utility_density_by_strategy <- function(env,
                                                                                  thin_wave_factor = 1,
                                                                                  return_plot = TRUE,
                                                                                  plot_file = NA,
                                                                                  plot_dir = NA) {
  .searchnet_deprecated_plot("saomnk_search_rsiena_multiwave_plot_actor_utility_density_by_strategy", 'searchnet_plot_multiwave(what = "utility_density", by = "strategy")')
  .searchnet_forward(searchnet_plot_multiwave, list(what = "utility_density", by = "strategy"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_search_rsiena_multiwave_plot_utility_ridge_density_by_strategy <- function(env,
                                                                                  actor_ids = c(),
                                                                                  wave_ids = c(),
                                                                                  thin_factor = 1,
                                                                                  thin_wave_factor = 1,
                                                                                  show_utility_points = TRUE,
                                                                                  show_strategy_means = TRUE,
                                                                                  scale_utility = TRUE,
                                                                                  return_plot = TRUE,
                                                                                  plot_file = NA,
                                                                                  plot_dir = NA,
                                                                                  plot_periods = 4) {
  .searchnet_deprecated_plot("saomnk_search_rsiena_multiwave_plot_utility_ridge_density_by_strategy", 'searchnet_plot_multiwave(what = "utility_ridge", by = "strategy")')
  .searchnet_forward(searchnet_plot_multiwave, list(what = "utility_ridge", by = "strategy"))
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_plot_market_entry_survival_v0 <- function(env,
                                                 n = 50,
                                                 environ_params = NULL,
                                                 structure_model = NULL,
                                                 steps_per_actor = NULL,
                                                 theta_shocks = NULL,
                                                 conf_level = 0.95,
                                                 verbose = FALSE) {
  .searchnet_deprecated_plot("saomnk_plot_market_entry_survival_v0", 'searchnet_plot_cumulative_entry()')
  .searchnet_forward(searchnet_plot_cumulative_entry, list(), rename_env = FALSE)
}

#' @rdname searchnet-deprecated-plots
#' @export
saomnk_plot_K_AC_NEW_shocks <- function(env,
                                        verbose = FALSE) {
  .searchnet_deprecated_plot("saomnk_plot_K_AC_NEW_shocks", 'searchnet_plot_new_component_shocks()')
  .searchnet_forward(searchnet_plot_new_component_shocks, list(), rename_env = FALSE)
}

