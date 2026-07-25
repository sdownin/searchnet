# Replication Archive for searchnet JSS Paper

## Overview

This archive reproduces all computational results in:

> Downing, S. (2026). "searchnet: Network-Embedded Strategic Search Simulation
> in R." *Journal of Statistical Software*.

All scripts use `set.seed()` for exact reproducibility. Total runtime is
under 10 minutes on a standard workstation.

## Requirements

- R >= 4.1.0
- The `searchnet` package and its dependencies (RSiena, R6, igraph, ggplot2,
  Matrix, network, reshape2, dplyr, etc.)

Install searchnet from the parent package directory:

```r
# From the repository root:
install.packages(".", repos = NULL, type = "source")

# Or using devtools/remotes:
remotes::install_local("path/to/SaoMNK")
```

## Files

| File | Purpose |
|------|---------|
| `reproduce_all.R` | Reproduces all manuscript illustrations (Figures 1--8) and the NK equivalence verification |
| `benchmark.R` | Reproduces computational benchmarks (Section 6): overhead, scalability, fitness landscape timing |
| `sessionInfo.R` | Captures session information for reproducibility documentation |

## Quick Start

```r
# Reproduce all manuscript figures (saves PDFs to figures/)
source("reproduce_all.R")

# Run benchmarks (saves CSV to figures/)
source("benchmark.R")

# Capture session info
source("sessionInfo.R")
```

## Output

All figures are saved as PDF files in `figures/`:

| File | Manuscript Reference |
|------|---------------------|
| `fig_levinthal_scope.pdf` | Figure 1: Levinthal replication -- actor scope trajectory |
| `fig_endogenous_k4.pdf` | Figure 2: Endogenous landscape -- {K}-4 panel |
| `fig_endogenous_utility.pdf` | Figure 3: Endogenous landscape -- utility decomposition |
| `fig_endogenous_snapshots.pdf` | Figure 4: Endogenous landscape -- network snapshots |
| `fig_shock_baseline_k4.pdf` | Figure 5: Shock analysis -- baseline {K}-4 panel |
| `fig_shock_treatment_k4.pdf` | Figure 6: Shock analysis -- treatment {K}-4 panel |
| `fig_api_k4.pdf` | Figure 7: API demonstration -- {K}-4 panel (Section 4) |
| `fig_api_utility.pdf` | Figure 8: API demonstration -- utility decomposition (Section 4) |
| `benchmark_overhead.csv` | Table: searchnet vs RSiena overhead |
| `benchmark_scalability.csv` | Table: scalability across M/N sizes |
| `benchmark_landscape.csv` | Table: fitness landscape computation timing |
| `session_info.txt` | Full R session information |

## Timing

Expected runtimes on a modern workstation (Intel Core i7, 64 GB RAM):

- `reproduce_all.R`: approximately 3--5 minutes
- `benchmark.R`: approximately 3--5 minutes
- `sessionInfo.R`: < 1 second

## Notes

- All random seeds match those used in the manuscript code chunks.
- Figures are produced at 6.5 x 4.5 inches (single-column) or 8 x 8 inches
  (panel figures) to match JSS formatting.
- The NK equivalence verification at the end of `reproduce_all.R` confirms
  that searchnet reproduces classical NK dynamics when endogenous effects are
  disabled.
