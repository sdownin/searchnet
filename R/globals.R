# Global variable declarations for R CMD check.
#
# Nearly every name below is a COLUMN NAME used inside a data-masked
# expression: dplyr verbs (filter, mutate, group_by, summarize) and ggplot2
# aes() mappings evaluate their arguments against the data frame, not the
# calling environment. R's static code analysis cannot see into data masks,
# so it reports each such column as "no visible binding for global
# variable". The names are bound at run time by the data, and the NOTE is a
# limitation of the checker rather than a defect in the code.
#
# Declaring them here silences that NOTE. It does NOT make them correct:
# globalVariables() suppresses a diagnostic and verifies nothing. A typo in
# a column name is still a run-time error, and adding a name here is not a
# substitute for a test that exercises the pipeline. Add a name only after
# confirming it really is a column supplied by the data at that point.
#
# Deliberately NOT listed: names that turned out to be genuine unbound
# objects or missing imports. Those are fixed at the call site instead -
# unqualified functions are namespace-qualified (e.g. stats::complete.cases,
# igraph::cluster_louvain, ggpubr::annotate_figure), and the undefined
# object '.saomnk_dir' in searchnet_proof() was removed from R/utils.R.
#
# Regenerate the list from a fresh `R CMD check --as-cran` log rather than
# by hand, and re-derive it after any large refactor so stale names do not
# accumulate and mask a real binding error.

utils::globalVariables(c(
  "action_type", "activity_label", "activity_type", "Activity_Type",
  "actor", "actor_id", "avoid_mean", "avoid_se", "avoidance_rate",
  "behavior_type", "chain_half", "chain_step_id", "ci_hi", "ci_lo",
  "ci_lower_entry", "ci_upper_entry", "color", "comp_rate_mean",
  "comp_rate_se", "competitive_entry_rate", "component_id", "Component1",
  "Component2", "condition", "conf.high", "conf.low", "control", "Control",
  "control_normalized", "diff_adjusted", "difference", "dyad_type",
  "effect", "effect_key", "effect_level", "end", "entry_rate", "estimate",
  "event.time", "exploitation", "Exploitation", "exploitation_adjusted",
  "exploitation_score", "exploration", "Exploration",
  "exploration_adjusted", "exploration_score", "firm", "fitness",
  "frame_step", "from", "generation", "group", "id", "intensity",
  "is_current", "Iteration", "kaa_mean", "kaa_se", "label", "mean_degree",
  "Mean_K_E", "Mean_K_S", "mean_outcome", "mean_prop", "mean_proportion",
  "mean_risk_taking", "mean_rivals_at_entry", "mean_score",
  "median_estimate", "n_obs", "n_rivals_present", "node_group",
  "node_type", "panel_label", "panel_label_text", "period", "PeriodFct",
  "phase", "policy", "policy_type", "pop_mean_prop", "primary_group",
  "prop_exploitation", "prop_exploration", "proportion", "Proportion",
  "Q25_K_E", "Q25_K_S", "Q75_K_E", "Q75_K_S", "rating",
  "risk_taking_score", "rivals_mean", "rivals_se", "robust", "run_seed",
  "score", "sd_proportion", "sd_risk_taking", "se", "se_prop",
  "se_proportion", "se_risk_taking", "se_score", "series", "shape",
  "share", "shock_id", "shock_label", "show_at_step", "sig_label",
  "specification", "stabilization_summary_period", "start", "stat_type",
  "step", "strategy", "strategy_0", "strategy_100", "strategy_label",
  "survival_rate", "terminal_kaa", "theta_inPop", "tile_label", "tile_val",
  "time_norm", "time_period", "to", "trail_alpha", "treated", "Treated",
  "treated_normalized", "treatment", "treatment_group", "type", "utility",
  "utility_mean", "value", "value_contributions", "wave_id", "weight",
  "Weight", "width", "x", "x_from", "x_to", "xend", "y", "y_from", "y_to",
  "yend"
))

