###############################################################################
## test-suggests-guard.R
##
## Plot-layout and graph-drawing packages are in Suggests, not Imports. Every
## function that uses one calls .searchnet_need() first. These tests mock the
## availability seam (.searchnet_has) to check that
##   1. a guarded function stops with an install hint naming the package, and
##   2. the core path (environment, model, run) never consults the guard.
###############################################################################

.moved_suggests <- c("cowplot", "ggExtra", "ggpubr", "ggraph", "ggridges",
                     "gridExtra", "reshape2", "stringr", "visNetwork")

test_that(".searchnet_need() names every missing package and how to install it", {
  testthat::local_mocked_bindings(.searchnet_has = function(pkg) FALSE,
                                  .package = "searchnet")
  expect_error(searchnet:::.searchnet_need("ggpubr", "some_plot()"),
               "some_plot\\(\\) needs the suggested package 'ggpubr'.*install.packages\\(\"ggpubr\"\\)")
  expect_error(searchnet:::.searchnet_need(c("ggraph", "reshape2")),
               "packages 'ggraph', 'reshape2'.*install.packages\\(c\\(\"ggraph\", \"reshape2\"\\)\\)")
})

test_that(".searchnet_need() is silent when the packages are available", {
  testthat::local_mocked_bindings(.searchnet_has = function(pkg) TRUE,
                                  .package = "searchnet")
  expect_invisible(searchnet:::.searchnet_need(.moved_suggests))
})

test_that("a guarded exported plot stops early with the install hint", {
  testthat::local_mocked_bindings(.searchnet_has = function(pkg) FALSE,
                                  .package = "searchnet")
  ## The guard runs before the environment argument is touched.
  expect_error(saomnk_plot_bipartite_system_from_mat(NULL, NULL),
               "needs the suggested packages 'ggraph', 'gridExtra', 'reshape2'")
})

test_that("the core simulation path does not need any Suggests package", {
  skip_on_cran()
  calls <- character(0)
  testthat::local_mocked_bindings(
    .searchnet_has = function(pkg) { calls <<- c(calls, pkg); FALSE },
    .package = "searchnet")
  env <- saomnk_env(M = 4, N = 6, seed = 42)
  mod <- saomnk_model(density = -0.5, popularity = 0.2)
  utils::capture.output(suppressMessages(
    saomnk_run(env, mod, steps_per_actor = 3, seed = 12345)))
  expect_gt(env$get_step(), 0)
  expect_length(calls, 0)
})

test_that("the moved packages are not in Imports and not imported in NAMESPACE", {
  desc <- system.file("DESCRIPTION", package = "searchnet")
  imports <- trimws(sub("\\(.*", "",
    strsplit(read.dcf(desc, fields = "Imports")[[1]], ",")[[1]]))
  expect_length(intersect(imports, .moved_suggests), 0)
  ns <- getNamespaceImports("searchnet")
  expect_length(intersect(names(ns), .moved_suggests), 0)
})
