###############################################################################
## searchnet-new-analysis.R
##
## searchnet_new_analysis(): writes a reproducible SAOM-NK analysis skeleton
## from the template in inst/templates/new-analysis.
###############################################################################

#' Create a Reproducible SAOM-NK Analysis Skeleton
#'
#' Writes a small, self-contained research project to \code{path}: an
#' \code{analysis.Rmd} that calls the package's own functions in a sound
#' order, a helper file, a synthetic long-format data file, and a README with
#' the exact install line for the searchnet version that created it. Every
#' section runs on the bundled synthetic example.
#'
#' The sections of \code{analysis.Rmd} are: (1) setup, with
#' \code{\link{searchnet_check_setup}}, recorded searchnet and RSiena versions
#' and fixed seeds; (2) data, synthetic or read from a long CSV with
#' \code{\link{searchnet_bipartite_from_long}}; (3) \{K\} readings with
#' \code{\link{searchnet_k_readings}}; (4) a calibrated simulated world and
#' \code{\link{searchnet_moment_gate}}, with tolerances in a chunk labeled
#' \code{preregistered-criteria}; (5) a small \code{\link{searchnet_recovery}}
#' check (switchable with the \code{run_recovery} parameter); (6) a
#' counterfactual shock gated by \code{\link{searchnet_shock_support_check}}
#' and \code{\link{searchnet_placebo}}; (7) a report printing every null with
#' its minimum detectable effect.
#'
#' Nothing is written outside \code{path}. The files written are
#' \code{analysis.Rmd}, \code{README.md}, \code{R/world.R} and
#' \code{data/example_long.csv}. The caller's random number state is
#' restored afterwards.
#'
#' @param path Directory to create. It must not exist or be empty, unless
#'   \code{overwrite = TRUE}.
#' @param title Title of the analysis, used in \code{analysis.Rmd} and the
#'   README.
#' @param data Default data source of the analysis: \code{"synthetic"} (a
#'   panel generated in the document) or \code{"long_csv"} (the bundled
#'   \code{data/example_long.csv}, to be replaced by the user's own file).
#'   Either can be chosen at render time with the \code{data} parameter.
#' @param overwrite Logical. Write into a non-empty directory, replacing the
#'   skeleton's own files there? Other files are left untouched. Default
#'   \code{FALSE}.
#'
#' @return Invisibly, the normalized paths of the files written.
#' @seealso \code{vignette("searchnet-observed-data")}
#' @export
#' @examples
#' dir <- file.path(tempdir(), "my-analysis")
#' files <- searchnet_new_analysis(dir, title = "Example analysis")
#' basename(files)
#' \donttest{
#' if (requireNamespace("rmarkdown", quietly = TRUE) &&
#'     rmarkdown::pandoc_available()) {
#'   rmarkdown::render(file.path(dir, "analysis.Rmd"),
#'                     params = list(tiny = TRUE, run_recovery = FALSE),
#'                     quiet = TRUE)
#' }
#' }
#' unlink(dir, recursive = TRUE)
searchnet_new_analysis <- function(path, title = "SAOM-NK analysis",
                                   data = c("synthetic", "long_csv"),
                                   overwrite = FALSE) {
  data <- match.arg(data)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop("`path` must be a single directory path.", call. = FALSE)
  if (!is.character(title) || length(title) != 1L || is.na(title))
    stop("`title` must be a single string.", call. = FALSE)
  stopifnot(is.logical(overwrite), length(overwrite) == 1L, !is.na(overwrite))

  if (file.exists(path) && !dir.exists(path))
    stop("`path` exists and is a file: ", path, call. = FALSE)
  if (dir.exists(path) &&
      length(list.files(path, all.files = TRUE, no.. = TRUE)) && !overwrite)
    stop("`path` is not empty: ", path, ". Choose an empty or new directory, ",
         "or set overwrite = TRUE to replace the skeleton's files there.",
         call. = FALSE)

  tpl <- system.file("templates", "new-analysis", package = "searchnet")
  if (!nzchar(tpl))
    stop("The analysis template is missing from the searchnet installation.",
         call. = FALSE)

  ver <- as.character(utils::packageVersion("searchnet"))
  ## Text for the README, not a call: the package name is spliced in so the
  ## source holds no undeclared `pkg::` reference.
  install <- paste0("remotes", strrep(":", 2L),
                    sprintf('install_github("sdownin/searchnet@v%s")', ver))
  if (length(unclass(package_version(ver))[[1L]]) > 3L)
    install <- paste0(install, "  # development version: no release tag ",
                      "of this name; pin a commit instead")
  fill <- function(lines) {
    lines <- gsub("{{TITLE}}", gsub('"', "'", title), lines, fixed = TRUE)
    lines <- gsub("{{DATA}}", data, lines, fixed = TRUE)
    lines <- gsub("{{SEARCHNET_VERSION}}", ver, lines, fixed = TRUE)
    gsub("{{INSTALL_LINE}}", install, lines, fixed = TRUE)
  }

  dir.create(file.path(path, "R"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "data"), showWarnings = FALSE)
  out <- file.path(path, c("analysis.Rmd", "README.md", "R/world.R",
                           "data/example_long.csv"))
  for (f in c("analysis.Rmd", "README.md", "R/world.R"))
    writeLines(fill(readLines(file.path(tpl, f), warn = FALSE)),
               file.path(path, f), useBytes = TRUE)

  ## The example CSV is drawn with the template's own helper, at the sizes and
  ## parameters the synthetic branch of analysis.Rmd uses.
  helpers <- new.env(parent = baseenv())
  sys.source(file.path(tpl, "R", "world.R"), envir = helpers)
  long <- .with_local_seed(20261011L, helpers$world_to_long(
    helpers$world_redraw_panel(M = 30, N = 20, W = 4, p = 0.2, redraw = 0.15)))
  utils::write.csv(long, out[4L], row.names = FALSE)

  invisible(normalizePath(out, winslash = "/"))
}
