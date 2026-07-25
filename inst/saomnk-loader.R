##
## searchnet Loader
##
## Source this file to load the searchnet framework in a standalone R session
## (without installing the package). This handles the correct load order.
##
## Usage in vignettes/scripts:
##   dir_r <- 'path/to/SaoMNK/R'
##   source(file.path(dir_r, 'saomnk-loader.R'))
##   env <- SaomNkRSienaBiEnv$new(list(M=3, N=6, BI_PROB=0.5, rand_seed=123, name='test'))
##

## Get the directory of this loader file
if (!exists('.saomnk_dir', envir = globalenv())) {
  .saomnk_dir <- tryCatch(
    dirname(sys.frame(1)$ofile),
    error = function(e) getwd()
  )
} else {
  .saomnk_dir <- get('.saomnk_dir', envir = globalenv())
}

## Load required packages
library(R6)
library(igraph)
library(RSiena)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(Matrix)
library(network)
library(reshape2)
library(cowplot)
library(ggraph)
library(ggpubr)
library(grid)
library(gridExtra)
library(texreg)
library(uuid)

## Source files in dependency order
source(file.path(.saomnk_dir, 'utils.R'), local = FALSE)
source(file.path(.saomnk_dir, 'saomnk-base.R'), local = FALSE)
source(file.path(.saomnk_dir, 'saomnk-class.R'), local = FALSE)

## Source plot files if they exist (Phase 2 extraction)
.plot_files <- c(
  'plot-utility.R', 'plot-degrees.R', 'plot-snapshots.R',
  'plot-shocks.R', 'plot-multiwave.R', 'plot-markets.R',
  'plot-exploration.R'
)
for (.pf in .plot_files) {
  .path <- file.path(.saomnk_dir, .pf)
  if (file.exists(.path)) source(.path, local = FALSE)
}

## Source experiments if needed
.exp_path <- file.path(.saomnk_dir, 'saomnk-experiments.R')
if (file.exists(.exp_path)) source(.exp_path, local = FALSE)

## Clean up temp vars (suppress warnings for vars that may not exist in this scope)
suppressWarnings(rm(.saomnk_dir, .plot_files, .pf, .path, .exp_path, envir = environment()))

## Return the class generator (for backward-compat with `searchnet <- source(...)$value`)
SaomNkRSienaBiEnv
