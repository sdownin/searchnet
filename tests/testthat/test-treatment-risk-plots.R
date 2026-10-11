# Risk-taking plots of a treated group (strategy "100") against a control
# group (strategy "0"). The standalone plot is internal; the view is reached
# through searchnet_plot_exploration(type = "treatment_risk") and the
# SaomNkRSienaBiEnv methods below.

test_that("the treated-group risk plot is internal and the view is selectable", {
  ns <- asNamespace("searchnet")
  expect_true(exists("plot_treated_risk_taking", envir = ns, inherits = FALSE))
  expect_false("plot_treated_risk_taking" %in% getNamespaceExports("searchnet"))
  expect_true("treatment_risk" %in% eval(formals(searchnet_plot_exploration)$type))
  expect_identical(ns$.searchnet_exploration_types[["treatment_risk"]],
                   ".searchnet_ee_treatment_risk")
  ## an unknown type lists the valid ones
  expect_error(searchnet_plot_exploration(NULL, type = "nonesuch"), "treatment_risk")
  methods <- names(SaomNkRSienaBiEnv$public_methods)
  for (m in c("plot_treated_risk_taking", "analyze_treatment_risk_taking",
              "plot_exploration_exploitation_treatment_risk"))
    expect_true(m %in% methods, info = m)
})

test_that("treated and control groups are labeled generically and drawn the same both ways", {
  skip_if_not_installed("RSiena")
  local_close_new_devices()
  env <- run_tiny_sim(M = 8, N = 16, iterations_per_actor = 10, rand_seed = 42,
                      use_strategy = TRUE)
  ## the fixture codes strategies 0/1; recode the second group as treated ("100")
  mdf <- env$calculate_explore_exploit_risk_adjusted()
  mdf$strategy[mdf$strategy == "1"] <- "100"
  mdf2 <- env$calculate_social_logic_influence(mdf)
  p_fun <- suppressWarnings(plot_treated_risk_taking(env, metrics_df = mdf, shock_time = 3))
  p_r6 <- suppressWarnings(env$plot_treated_risk_taking(metrics_df = mdf, shock_time = 3))
  expect_s3_class(p_fun, "ggplot")
  expect_true("post_shock" %in% names(p_fun$data))
  lab <- unique(p_fun$data[, c("strategy", "strategy_label")])
  lab <- setNames(as.character(lab$strategy_label), as.character(lab$strategy))
  expect_true(all(c("0", "100") %in% names(lab)))
  expect_identical(unname(lab[c("0", "100")]), c("Control (Untreated)", "Treated"))
  expect_identical(p_fun$data, p_r6$data)
  expect_identical(p_fun$labels$title, "Risk-Taking Behavior: Treated vs Control")

  set.seed(1)
  p_view <- suppressWarnings(searchnet_plot_exploration(env, type = "treatment_risk",
                                                        metrics_df = mdf2, shock_time = 3))
  set.seed(1)
  p_view_r6 <- suppressWarnings(env$plot_exploration_exploitation_treatment_risk(
    metrics_df = mdf2, shock_time = 3))
  expect_s3_class(p_view, "ggplot")
  drawn <- do.call(rbind, lapply(p_view$layers, function(l)
    if (is.data.frame(l$data) && "strategy_label" %in% names(l$data))
      unique(data.frame(strategy = as.character(l$data$strategy),
                        label = as.character(l$data$strategy_label)))))
  drawn <- unique(drawn)
  expect_identical(drawn$label[drawn$strategy == "0"], "Control (0)")
  expect_identical(drawn$label[drawn$strategy == "100"], "Treated (100)")
  expect_identical(p_view$labels$title, p_view_r6$labels$title)
})
