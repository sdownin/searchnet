## searchnet_new_analysis(): the analysis skeleton.

.na_root <- function() {
  root <- tempfile("na_root_")
  dir.create(root)
  root
}

test_that("searchnet_new_analysis writes the skeleton inside path only", {
  root <- .na_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- file.path(root, "proj")
  files <- searchnet_new_analysis(path, title = "Test analysis")
  expect_setequal(basename(files), c("analysis.Rmd", "README.md", "world.R",
                                     "example_long.csv"))
  expect_true(all(file.exists(files)))
  ## Nothing outside path.
  expect_identical(list.files(root, all.files = TRUE, no.. = TRUE), "proj")
  expect_setequal(list.files(path, recursive = TRUE, all.files = TRUE),
                  c("analysis.Rmd", "README.md", "R/world.R",
                    "data/example_long.csv"))

  rmd <- readLines(file.path(path, "analysis.Rmd"))
  expect_false(any(grepl("{{", rmd, fixed = TRUE)))
  expect_true(any(grepl('title: "Test analysis"', rmd, fixed = TRUE)))
  expect_true(any(grepl("preregistered-criteria", rmd, fixed = TRUE)))
  for (fn in c("searchnet_check_setup", "searchnet_bipartite_from_long",
               "searchnet_k_readings", "searchnet_moment_gate",
               "searchnet_recovery", "searchnet_placebo",
               "searchnet_shock_support_check"))
    expect_true(any(grepl(fn, rmd, fixed = TRUE)), info = fn)

  readme <- readLines(file.path(path, "README.md"))
  pin <- sprintf('remotes::install_github("sdownin/searchnet@v%s")',
                 as.character(utils::packageVersion("searchnet")))
  expect_true(any(grepl(pin, readme, fixed = TRUE)))

  long <- utils::read.csv(file.path(path, "data", "example_long.csv"))
  expect_identical(names(long), c("actor", "component", "period"))
  obs <- searchnet_bipartite_from_long(long, "actor", "component", "period")
  expect_identical(dim(obs$B)[3L], 4L)
})

test_that("searchnet_new_analysis refuses a non-empty directory", {
  root <- .na_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("keep me", file.path(root, "notes.txt"))
  expect_error(searchnet_new_analysis(root), "not empty")
  expect_identical(list.files(root), "notes.txt")

  ## overwrite = TRUE writes the skeleton and leaves other files alone.
  searchnet_new_analysis(root, data = "long_csv", overwrite = TRUE)
  expect_identical(readLines(file.path(root, "notes.txt")), "keep me")
  expect_true(any(grepl('data: "long_csv"',
                        readLines(file.path(root, "analysis.Rmd")),
                        fixed = TRUE)))

  f <- file.path(root, "notes.txt")
  expect_error(searchnet_new_analysis(f), "is a file")
})

test_that("analysis.Rmd renders with tiny settings", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  root <- .na_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- file.path(root, "proj")
  searchnet_new_analysis(path)
  out <- NULL
  utils::capture.output(suppressWarnings(suppressMessages(
    out <- rmarkdown::render(file.path(path, "analysis.Rmd"),
                             output_format = rmarkdown::md_document(),
                             params = list(tiny = TRUE, run_recovery = TRUE),
                             envir = new.env(parent = globalenv()),
                             quiet = TRUE))))
  expect_true(file.exists(out))
  md <- readLines(out)
  expect_true(any(grepl("Moment gate:", md, fixed = TRUE)))
  expect_true(any(grepl("NULL:|No nulls", md)))
  ## Rendering also stays inside path.
  expect_identical(list.files(root, all.files = TRUE, no.. = TRUE), "proj")
})
