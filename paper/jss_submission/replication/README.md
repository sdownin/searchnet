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

# Or from GitHub (searchnet is not on CRAN):
remotes::install_github("sdownin/searchnet")
```

## Files

| File | Purpose |
|------|---------|
| `reproduce_all.R` | Reproduces all manuscript illustrations (Figures 1--8), the two {K}-system demonstration figures, and the NK equivalence verification |
| `make_k_system_figures.R` | Generates the two {K}-system demonstration figures into `paper/figures/`; also sourced by `reproduce_all.R` |
| `make_figure1_mental_model.R` | Generates manuscript Figure 1 (mental model of the statistical model) into `paper/figures/` from one seeded run; also sourced by `reproduce_all.R` |
| `benchmark.R` | Reproduces computational benchmarks (Section 6): overhead, scalability, fitness landscape timing |
| `sessionInfo.R` | Captures session information for reproducibility documentation |

## Quick Start

```sh
Rscript reproduce_all.R      # all manuscript figures
Rscript benchmark.R          # benchmark tables
Rscript sessionInfo.R        # session information
```

The scripts also work under `source()` from an interactive session.

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

Three further figures are written as PNG into `paper/figures/`, because the
manuscript includes them as image files rather than as knitted chunks:

| File | Manuscript Reference |
|------|---------------------|
| `fig_k_coupling_sim.png` | Section 3.2: coupled degree dynamics (simulated) |
| `fig_k_shock_relocation.png` | Section 5.3: a shock relocates the coupled system (simulated) |
| `fig1_mental_model.png` (and `.pdf`) | Section 1, Figure 1 (`fig:mentalmodel`): mental model of the statistical model, one seeded run |

## Figure coverage

Every figure the manuscript produces from code is covered here or is knitted
directly from a manuscript chunk:

- Knitted chunks (`k4-panel`, `snapshots`, `utility`, `levinthal-degrees`,
  `ergodicity`, `endogenous-k4`, `endogenous-utility`, `shock-comparison`,
  `shock-treatment`, `causal-did`, `benchmark-overhead`, `landscape-scaling`)
  run when the `.Rmd` is knitted. `reproduce_all.R` reruns the same
  simulations standalone; `ergodicity` and `causal-did` are knit-only.
- `fig_k_coupling_sim.png` and `fig_k_shock_relocation.png` come from
  `make_k_system_figures.R`.
- `fig1_mental_model.png` comes from `make_figure1_mental_model.R`, which
  also checks that its decoded choice probabilities equal RSiena's recorded
  `LogChoiceProb` at every ministep of the run.
- `manim_convergence_climb.png` and `manim_ising.png` are animation stills
  that the manuscript does not currently use; this bundle does not generate
  them.

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

## Building the manuscript and the submission bundle

`paper/build_submission.R` renders the JSS manuscript, the online appendix
and the web version, and regenerates `paper/jss_submission/` (manuscript
sources and PDF, bibliography, figures, these replication scripts, the
package source tarball, and `MANIFEST.txt` with SHA-256 checksums):

```sh
Rscript paper/build_submission.R          # full build (evaluates all code)
Rscript paper/build_submission.R --fast   # format check only
```
