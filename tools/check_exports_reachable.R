#!/usr/bin/env Rscript
# Confirm that the functions exported in v0.6.0 are reachable from the package
# namespace WITHOUT the ::: operator, and that the snapshot rename took effect.
#
# Motivation: before v0.6.0 these carried @export tags but were absent from the
# manually maintained NAMESPACE, so `searchnet::saom_to_saomnk` failed while
# `searchnet:::saom_to_saomnk` worked. A load test alone would not have caught
# that, because load_all() attaches everything regardless of NAMESPACE.
#
# Usage: Rscript tools/check_exports_reachable.R [package_root]

root <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "."

ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
exported <- trimws(gsub("[)]", "", sub("^export[(]", "",
                                       grep("^export[(]", ns, value = TRUE))))
s3 <- vapply(strsplit(trimws(gsub("[)]", "", sub("^S3method[(]", "",
             grep("^S3method[(]", ns, value = TRUE)))), ","),
             function(p) paste(trimws(p), collapse = "."), character(1))

want_export <- c(
  "saomnk_plot_geo_network", "saomnk_geo_arc",
  "saomnk_plot_snapshots", "saomnk_plot_snapshots_from_array",
  "saomnk_sai", "saomnk_extract_estimates_saom",
  "saom_to_saomnk", "empirical_to_saomnk_env", "run_calibrated_counterfactual",
  "searchnet_export_all", "searchnet_export_k4",
  "searchnet_export_snapshots", "searchnet_export_utility",
  "get_orm_scenarios", "saomnk_assent", "saomnk_confirm",
  "saomnk_monte_carlo", "saomnk_run_two_sided")
want_s3 <- c("plot.saomnk_sai")

fail <- 0L
cat("export() entries:\n")
for (f in want_export) {
  ok <- f %in% exported
  if (!ok) fail <- fail + 1L
  cat(sprintf("  %-36s %s\n", f, if (ok) "ok" else "MISSING"))
}
cat("S3 method registrations:\n")
for (f in want_s3) {
  ok <- f %in% s3
  if (!ok) fail <- fail + 1L
  cat(sprintf("  %-36s %s\n", f, if (ok) "ok" else "MISSING"))
}

# The rename must have removed the collision: exactly one definition of the
# API-signature function, and the legacy one under its new name.
src <- unlist(lapply(list.files(file.path(root, "R"), pattern = "[.]R$",
                                full.names = TRUE),
                     readLines, warn = FALSE))
n_api <- sum(grepl("^saomnk_plot_snapshots[[:space:]]*<-[[:space:]]*function", src))
n_leg <- sum(grepl("^saomnk_plot_snapshots_from_array[[:space:]]*<-", src))
cat(sprintf("\ndefinitions of saomnk_plot_snapshots: %d (want 1)\n", n_api))
cat(sprintf("definitions of saomnk_plot_snapshots_from_array: %d (want 1)\n", n_leg))
if (n_api != 1L || n_leg != 1L) fail <- fail + 1L

if (fail) { cat("\nFAILED:", fail, "problem(s)\n"); quit(status = 1) }
cat("\nAll v0.6.0 exports registered and the snapshot collision is resolved.\n")
