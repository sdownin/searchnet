# Optional (Suggests) packages.
#
# The simulation, estimation and {K} code needs only the packages in Imports.
# Composite plot layouts (ggpubr, cowplot, gridExtra), graph drawings (ggraph,
# visNetwork, reshape2) and a few chart types (ggridges, ggExtra, stringr) are
# in Suggests, and every function that uses one calls .searchnet_need() first,
# so a missing package stops with an install hint instead of an obscure
# "there is no package called" error halfway through a plot.

# Seam for tests: mock this to simulate a package that is not installed.
.searchnet_has <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.searchnet_need <- function(pkg, what = "this function") {
  missing <- pkg[!vapply(pkg, .searchnet_has, logical(1))]
  if (length(missing)) {
    stop(sprintf(
      "%s needs the suggested package%s %s. Install with install.packages(%s).",
      what,
      if (length(missing) > 1) "s" else "",
      paste0("'", missing, "'", collapse = ", "),
      paste(deparse(missing), collapse = "")),
      call. = FALSE)
  }
  invisible(TRUE)
}
