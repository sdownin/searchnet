#!/usr/bin/env Rscript
# =============================================================================
# sessionInfo.R
# Captures full session information for reproducibility documentation.
#
# Run this after reproduce_all.R and benchmark.R to record the exact
# package versions and system configuration used.
# =============================================================================

cat("=================================================================\n")
cat("  searchnet Session Info Capture\n")
cat("  Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=================================================================\n\n")

library(searchnet)
library(RSiena)
library(ggplot2)
library(Matrix)
library(igraph)

# Output directory
## Locate this script under Rscript or source(); sys.frame(1)$ofile alone
## exists only under source() and errors at top level under Rscript.
.script_dir <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) return(dirname(normalizePath(f, mustWork = FALSE)))
  o <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(o)) dirname(normalizePath(o, mustWork = FALSE)) else "."
})
fig_dir <- file.path(.script_dir, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Capture session info
si <- sessionInfo()

# Print to console
print(si)

# Save to file
sink(file.path(fig_dir, "session_info.txt"))
cat("searchnet JSS Replication -- Session Information\n")
cat("Captured:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=================================================================\n\n")

cat("Key package versions:\n")
cat("  searchnet:", as.character(packageVersion("searchnet")), "\n")
cat("  RSiena:   ", as.character(packageVersion("RSiena")), "\n")
cat("  igraph:   ", as.character(packageVersion("igraph")), "\n")
cat("  ggplot2:  ", as.character(packageVersion("ggplot2")), "\n")
cat("  Matrix:   ", as.character(packageVersion("Matrix")), "\n")
cat("  R6:       ", as.character(packageVersion("R6")), "\n")
cat("\n")

print(si)

cat("\n\n=================================================================\n")
cat("Platform details:\n")
cat("  R version:", R.version.string, "\n")
cat("  OS:       ", si$running, "\n")
cat("  Platform: ", si$platform, "\n")
cat("  Locale:   ", Sys.getlocale(), "\n")
cat("=================================================================\n")
sink()

cat("\nSaved: session_info.txt\n")
cat("Output directory:", fig_dir, "\n")
