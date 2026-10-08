#!/usr/bin/env Rscript
###############################################################################
## make_effect_dimensions_csv.R
##
## Writes inst/rosetta/effect_dimensions.csv: the derived table of the {K}
## dimensions each effect reads and moves, searchnet_effect_dimensions() (rule
## "walk"), so other projects can pin it. tests/testthat/test-k-dimensions.R
## checks that the shipped file equals a fresh derivation; rerun this script
## after changing an effect or the rules.
##
## Usage, from the package root:  Rscript tools/make_effect_dimensions_csv.R
###############################################################################

if (file.exists("DESCRIPTION") &&
    any(grepl("^Package: searchnet", readLines("DESCRIPTION", n = 1)))) {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  stop("run from the searchnet package root")
}
tab <- searchnet_effect_dimensions()
out <- file.path("inst", "rosetta", "effect_dimensions.csv")
utils::write.csv(tab, out, row.names = FALSE)
cat(sprintf("wrote %s (%d effects)\n", out, nrow(tab)))
