.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "searchnet v", utils::packageVersion(pkgname),
    " - Network-Embedded Search Simulation Engine\n",
    "  Author: Stephen Downing (University of Missouri)\n",
    "  Usage:  env <- SaomNkRSienaBiEnv$new(config_environ_params)"
  )
}

.onLoad <- function(libname, pkgname) {
  # Prevent sna/igraph namespace conflicts
  tryCatch(
    expr = { if ("package:sna" %in% search()) detach("package:sna") },
    error = function(e) invisible(NULL)
  )
}
