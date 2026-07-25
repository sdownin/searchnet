# searchnet <img src="" align="right" height="139" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)]()
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**Network-Embedded Search Simulation Engine**

## Overview

**searchnet** (formerly SaoMNK) is an R package that endogenizes the NK fitness landscape by reformulating it as a bipartite network data generation process. It bridges two research traditions that have developed independently: NK fitness landscape models from evolutionary theory and computational organization theory (Kauffman & Levin, 1987; Levinthal, 1997) and Stochastic Actor-Oriented Models (SAOMs) from social network analysis (Snijders, 1996; Ripley et al., 2022).

The package represents a search environment as a bipartite network of *M* actors affiliating with *N* components, where the fitness landscape topology is determined by the network structure and evolves endogenously through actors' search decisions. Because the model is formalized as a SAOM, it inherits the statistical infrastructure of RSiena, including maximum likelihood estimation, goodness-of-fit testing, and the capacity for empirical estimation on network panel data.

The central theoretical contribution is the **{K} framework**: the classical scalar complexity parameter *K* is revealed to be a special case of a four-dimensional coupled degree process, each dimension carrying distinct strategic implications for search, adaptation, and competitive dynamics.

## Installation

```r
# Install from GitHub
devtools::install_github("sdownin/searchnet")

# Or source directly for development
.saomnk_dir <- "path/to/searchnet/R"
source(file.path(.saomnk_dir, "saomnk-loader.R"))
```

### Requirements

- R >= 4.1.0
- Key dependencies: R6, RSiena, igraph, ggplot2, dplyr, Matrix, network

## Quick Start

The clean API provides user-friendly wrappers that handle RSiena internals automatically:

```r
library(searchnet)

# 1. Create a search environment: 6 actors, 8 components
env <- saomnk_env(M = 6, N = 8, density = 0.3, seed = 42)

# 2. Define a structure model with epistasis
model <- saomnk_model(
  density = -0.5,
  popularity = 0.2,
  epistasis_matrix = saomnk_block_diagonal(8, 2),
  epistasis_weight = 0.3
)

# 3. Run simulation
saomnk_run(env, model, steps_per_actor = 30, seed = 12345)

# 4. Visualize the coupled degree dynamics
saomnk_plot_k4(env)

# 5. Inspect results
saomnk_summary(env)
```

### Adding Actor Strategies

Introduce heterogeneous search strategies via actor-level covariates:

```r
model <- saomnk_model(
  density = -0.3,
  popularity = 0.2,
  scope = 0.1,
  epistasis_matrix = saomnk_block_diagonal(12, 4),
  strategies = list(
    egoX   = c(-1, 0, 1, -1, 0, 1),
    inPopX = c( 1, 0, -1, 1, 0, -1)
  )
)
```

### Advanced / Direct API

Power users can work directly with the R6 simulation engine:

```r
# Create environment via R6 constructor
env <- SaomNkRSienaBiEnv$new(list(
  M = 6, N = 8, BI_PROB = 0.3,
  rand_seed = 42, name = "_demo_"
))

# Run search with a manually constructed structure model
env$search_rsiena(structure_model, iterations_per_actor = 30, run_seed = 12345)

# K-4 degree panel plot
env$plot_degree_4panel()

# Access bipartite matrix at each step
env$bipartite_matrices
```

## The {K} Framework

The scalar NK complexity parameter *K* decomposes into four coupled degree measures in the bipartite representation:

| Degree | Symbol | Projection | Interpretation |
|--------|--------|------------|----------------|
| Actor Scope | K_AC | Actor -> Component | How many components each actor uses; breadth of activity system |
| Component Popularity | K_CA | Component -> Actor | How many actors adopt each component; competitive crowding |
| Actor Sociality | K_AA | Actor -> Actor | Overlap in component portfolios; strategic similarity |
| Component Epistasis | K_CC | Component -> Component | Co-adoption coupling between components; interaction complexity |

These four dimensions are structurally coupled: changes in any one propagate through the bipartite structure to the others. The {K} framework provides a lens for analyzing how search, adaptation, and rivalry co-evolve.

## Features

### Simulation Engine
- `saomnk_run()` / `search_rsiena()` -- Run SAOM-based search simulation with configurable structure models
- `search_rsiena_multiwave_run()` -- Monte Carlo multi-wave execution for repeated experiments
- `saomnk_shock()` / `theta_shocks` -- Parametric shock schedules for quasi-experimental designs
- Reproducible simulations via random seed control

### Structure Model Specification
- **Structural effects**: density, popularity (`inPop`), scope (`outAct`)
- **Actor covariates** (`coCovars`): Strategy heterogeneity via `egoX`, `inPopX`, `altX`
- **Dyadic covariates** (`coDyadCovars`): Exogenous epistasis matrices via `XWX`, actor-component payoffs via `X`
- **Time-varying covariates**: Dynamic strategy programs
- **Effect interactions**: Strategy-epistasis interaction terms
- `saomnk_block_diagonal()` -- Convenient modular epistasis matrix construction

### Visualization (43 plot functions)
- **K-4 degree panel**: Four coupled degree trajectories (`saomnk_plot_k4`, `saomnk_plot_degree_4panel`)
- **Network snapshots**: Bipartite, social, and epistasis projections (`saomnk_plot_snapshots`)
- **Utility decomposition**: Per-actor, per-strategy, contribution breakdown (`saomnk_plot_actor_utility`, `saomnk_plot_utility_contributions`)
- **Exploration/exploitation analysis**: Phase plots, DID designs, event studies
- **Market dynamics**: Entry/survival visualization, bipartite ring markets
- **Shock analysis**: K-attribute shocks, pre/post comparison
- **Multi-wave summaries**: Ridge density plots, strategy-level K summaries

### Diagnostics
- **SAI** (Specification Agreement Index): Robustness across model specifications
- **CFC** (Cross-Framework Concordance): SAOM vs. TERGM estimate agreement
- **DGF** (Density-GOF Frontier): Density thresholds for model degradation
- Adapted from the companion [netcheck](https://github.com/sdownin/netcheck) package

### Export Pipeline
- R-to-CSV export functions for K-4 trajectories, bipartite snapshots, and actor utilities
- Structured output for consumption by external tools (Python/manim, Stata, etc.)

### Batch Experiments
- `SaoMNKexperiments` R6 class for systematic parameter sweeps
- Configurable across structure model parameters, shock schedules, and random seeds
- Aggregated result collection and comparative analysis

## Package Architecture

```
searchnet/
├── R/                          # Source files
│   ├── saomnk-api.R            # User-friendly wrapper functions
│   ├── saomnk-base.R           # R6 base class (bipartite management, projections)
│   ├── saomnk-class.R          # R6 simulation engine (search, chains, K-4, fitness)
│   ├── saomnk-diagnostics.R    # SAI / CFC / DGF diagnostic tools
│   ├── saomnk-experiments.R    # SaoMNKexperiments batch simulation class
│   ├── searchnet-causal.R      # DID / Synth / RD causal inference wrappers
│   ├── searchnet-export.R      # R-to-CSV export pipeline
│   ├── saomnk-loader.R         # Dependency-ordered source loader (development)
│   ├── saomnk-package.R        # Package-level documentation
│   ├── utils.R                 # Standalone helpers (Jaccard, toggle, existence)
│   ├── plot-utility.R          # Utility decomposition plots
│   ├── plot-degrees.R          # K-4 degree panel and degree distribution plots
│   ├── plot-exploration.R      # Exploration/exploitation and DID plots
│   ├── plot-markets.R          # Market entry, survival, bipartite ring plots
│   ├── plot-shocks.R           # Shock analysis visualizations
│   ├── plot-snapshots.R        # Network snapshot plots
│   ├── plot-multiwave.R        # Multi-wave summary plots
│   └── zzz.R                   # .onLoad / .onAttach
├── vignettes/                  # 5 JSS-ready vignettes
├── inst/manim/                 # Python manim animation scenes
│   ├── searchnet_viz.py        # Shared visualization utilities
│   ├── scene_k4_evolution.py   # K-4 coupled degree evolution animation
│   ├── scene_bipartite_evolution.py  # Bipartite network evolution
│   ├── scene_landscape_3d.py   # 3D fitness landscape rendering
│   └── scene_shock.py          # Parametric shock animation
├── inst/proofs/                # Formal proofs (NK⊂SaoMNK equivalence, online appendix)
├── tests/testthat/             # 619 unit tests (17 test files)
├── paper/                      # JSS manuscript
├── man/                        # Generated documentation
└── docs/                       # Strategy documents
```

## Testing

```r
# Run the full test suite
testthat::test_dir("tests/testthat")
# Expected: 619 passing, 0 failures
```

Tests cover initialization, simulation execution, chain statistics, K-4 degree computation (with canonical network validation), shock processing, export pipeline, formal utility decomposition, McFadden choice probabilities, NK equivalence verification (Theorem 1), DGP validation, reproducibility, and edge cases.

## Manim Visualization Pipeline

searchnet includes a Python/manim pipeline for producing publication-quality animations from simulation output.

**Workflow**: R simulation -> CSV export -> manim rendering

```r
# 1. Run simulation in R
env <- saomnk_env(M = 6, N = 12, density = 0.3, seed = 42)
model <- saomnk_model(density = -0.5, epistasis_matrix = saomnk_block_diagonal(12, 4))
saomnk_run(env, model, steps_per_actor = 30, seed = 100)

# 2. Export to CSV (uses searchnet-export.R functions)
# Exports K-4 trajectories, bipartite snapshots, and utility data
```

```bash
# 3. Render manim scenes
manim -pqh inst/manim/scene_k4_evolution.py K4EvolutionScene
manim -pqh inst/manim/scene_bipartite_evolution.py BipartiteEvolutionScene
manim -pqh inst/manim/scene_landscape_3d.py Landscape3DScene
manim -pqh inst/manim/scene_shock.py ShockScene
```

Four animation scenes are available:

| Scene | Description |
|-------|-------------|
| `scene_k4_evolution.py` | Animated K-4 coupled degree trajectories over simulation time |
| `scene_bipartite_evolution.py` | Bipartite actor-component network evolution |
| `scene_landscape_3d.py` | 3D fitness landscape surface rendering |
| `scene_shock.py` | Parametric shock effects on network structure |

## Causal Inference Pipeline

searchnet integrates with standard causal inference packages for analyzing simulation-based natural experiments:

```r
# Run a simulation with an exogenous shock
env <- saomnk_env(M = 6, N = 8, seed = 42)
model <- saomnk_model(density = -0.3, popularity = 0.2,
                       epistasis_matrix = saomnk_block_diagonal(8, 2),
                       epistasis_weight = 0.3)
saomnk_run(env, model, steps_per_actor = 30, seed = 100,
           shocks = list(saomnk_shock("density", step = 15, new_value = -1.0)))

# Extract panel data for causal analysis
panel <- searchnet_causal_panel(env, shock_step = 90, outcome = "utility",
                                 treated_actors = 1:3)

# Difference-in-Differences (Callaway & Sant'Anna 2021)
did_result <- searchnet_did(panel)
searchnet_causal_plot(did_result)

# Synthetic Control (Abadie et al.)
synth_result <- searchnet_synth(panel, treated_unit = 1)

# Regression Discontinuity
rd_result <- searchnet_rd(panel)
```

Requires optional packages: `did`, `Synth`, `rdrobust` (in Suggests).

## Formal Mathematical Foundation

The package ships with formal proofs establishing the canonical relationship:

**NK &sub; SaoMNK &equiv; Conditional Logit on Bipartite DGP &rarr; QRE at Stationarity**

Access the proofs via:

```r
searchnet_proof()  # list all proof files
searchnet_proof("saomnk_nk_equivalence_proof.tex")  # Theorems 1-3
```

Key results (proven in `inst/proofs/`):
- **Theorem 1 (Reduction)**: NK is a special case of SaoMNK under 5 restrictions
- **Theorem 2 (Generalization)**: SaoMNK extends NK along 3 independent dimensions
- **Theorem 3 (Approximation)**: Any NK payoff reproducible via McFadden & Train (2000) mixed logit
- **Theorem 4 (SAOM-QRE)**: Stationary distribution is a Quantal Response Equilibrium
- **Theorem 5 (Brock-Durlauf Recovery)**: SaoMNK contains the canonical economic discrete-choice-with-social-interactions model (Brock & Durlauf 2001) as a degenerate special case in the M -> infinity, congestion-only, mean-field limit -- complementary to Theorem 1's single-actor greedy limit. See `vignette('saomnk-brock-durlauf')`.

The engine includes `verify_nk_equivalence()` for computational verification of Theorem 1, and `verify_brock_durlauf_reduction()` for empirical verification of Theorem 5.

## Vignettes

| Vignette | Description |
|----------|-------------|
| `saomnk-introduction` | Quick-start tutorial: environment setup, structure model, simulation, K-4 visualization |
| `saomnk-theory` | Formal framework: Definitions 1-3, Theorems 1-4, {K} projections, SAOM vs ERGM |
| `saomnk-simulation` | Full workflow: strategies, fitness landscapes, shocks, multi-wave |
| `saomnk-experiments` | Batch experiment design, parameter sweeps, comparative analysis |
| `saomnk-nk-validation` | Levinthal (1997) reproduction, NK &sub; SaoMNK verification |
| `saomnk-brock-durlauf` | Theorem 5: Brock & Durlauf (2001) recovery, mean-field self-consistency, social multiplier, bifurcation diagram |
| `saomnk-causal-inference` | DID, Synthetic Control, and RD via theta_shocks |

## Documentation & Paper

| Resource | Link |
|----------|------|
| **Software paper (online edition)** — full manuscript, browsable with TOC | [sdownin.github.io/searchnet/paper](https://sdownin.github.io/searchnet/paper/) |
| Program site — the research program behind the package | [sdownin.github.io/saomnk](https://sdownin.github.io/saomnk/) |
| Manuscript source (Rmd, JSS format) | [`paper/searchnet-jss.Rmd`](paper/searchnet-jss.Rmd) |

## Citation

```bibtex
@Manual{downing2026searchnet,
  title  = {searchnet: Network-Embedded Search Simulation Engine},
  author = {Stephen Downing},
  year   = {2026},
  note   = {R package version 0.2.0},
  url    = {https://github.com/sdownin/searchnet}
}
```

## References

- Kauffman, S. A., & Levin, S. (1987). Towards a general theory of adaptive walks on rugged landscapes. *Journal of Theoretical Biology*, 128(1), 11--45.
- Kauffman, S. A., & Weinberger, E. D. (1989). The NK model of rugged fitness landscapes. *Journal of Theoretical Biology*, 141(2), 211--245.
- Levinthal, D. A. (1997). Adaptation on rugged landscapes. *Management Science*, 43(7), 934--950.
- Snijders, T. A. B. (1996). Stochastic actor-oriented models for network change. *Journal of Mathematical Sociology*, 21(1--2), 149--172.
- Ripley, R. M., Snijders, T. A. B., Boda, Z., Voros, A., & Preciado, P. (2022). *Manual for RSiena*. University of Oxford, Department of Statistics; Nuffield College.
- Baumann, O., Schmidt, J., & Stieglitz, N. (2019). Effective search in rugged performance landscapes: A review and outlook. *Journal of Management*, 45(1), 285--318.

## License

MIT. See [LICENSE](LICENSE) for details.

## Author

Stephen Downing, University of Missouri ([sdowning@missouri.edu](mailto:sdowning@missouri.edu))
