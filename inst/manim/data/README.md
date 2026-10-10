# Animation CSV data schemas

This directory holds CSV files written by `searchnet_export_for_manim()`, in
long format for animation tools such as Manim. Actors are the rows of the
bipartite matrix B, components its columns.

## 1. k4_trajectories.csv

{K} degrees across simulation rounds, averaged per actor per round.

| Column  | Type      | Description                                        |
|---------|-----------|----------------------------------------------------|
| `round` | integer   | Simulation round (1-based; each round = M steps)   |
| `actor` | character | Actor label (default "A1", "A2", ...)              |
| `K_AC`  | numeric   | Actor-to-component degree (bipartite degree)       |
| `K_CC`  | numeric   | Component-to-component degree (co-holding)         |
| `K_CA`  | numeric   | Component-to-actor degree (holders of its components) |
| `K_AA`  | numeric   | Actor-to-actor degree (one-mode projection)        |

## 2. bipartite_snapshots.csv

Long-form active ties at selected snapshot steps.

| Column      | Type      | Description                                     |
|-------------|-----------|-------------------------------------------------|
| `round`     | integer   | Simulation round at snapshot                    |
| `actor`     | character | Actor label                                     |
| `component` | integer   | Component index (1-based)                       |
| `active`    | integer   | Always 1 (sparse format; inactive ties omitted) |

## 3. utility_trajectories.csv

Actor utility decomposition, averaged per actor per round.

| Column          | Type      | Description                                  |
|-----------------|-----------|----------------------------------------------|
| `round`         | integer   | Simulation round                             |
| `actor`         | character | Actor label                                  |
| `total_utility` | numeric   | Total objective-function utility             |
| `nk_component`  | numeric   | NK fitness component (outAct contribution)   |
| `scope_cost`    | numeric   | Density (scope) cost component               |
| `popularity`    | numeric   | Popularity component (often 0 in baseline)   |
| `rivalry`       | numeric   | Crowding component (inPop contribution)      |

## 4. shock_events.csv

Exogenous shock events injected during the simulation.

| Column        | Type      | Description                                    |
|---------------|-----------|------------------------------------------------|
| `round`       | integer   | Round at which the shock occurs                |
| `shock_type`  | character | Type identifier (e.g. "W-matrix", "exit")      |
| `magnitude`   | numeric   | Shock magnitude (scale depends on type)        |
| `description` | character | Human-readable description of the shock        |

Empty in baseline (no-shock) simulations.

## 5. phase_space.csv

PCA reduction of actor state vectors (each actor's N-dimensional binary row
of B) onto three principal components.

| Column  | Type      | Description                                        |
|---------|-----------|----------------------------------------------------|
| `round` | integer   | Simulation round at sample point                   |
| `actor` | character | Actor label                                        |
| `PC1`   | numeric   | First principal component score                    |
| `PC2`   | numeric   | Second principal component score                   |
| `PC3`   | numeric   | Third principal component score                    |

## Generating these files

```r
library(searchnet)

env <- saomnk_env(M = 8, N = 12, seed = 42)
mod <- saomnk_model(density = -1.5, popularity = 0,
                    influence_matrix = saomnk_block_diagonal(12, 3))
saomnk_run(env, mod, steps_per_actor = 30, seed = 1)

searchnet_export_for_manim(env, dir = "manim_data")
```

The README animation (`man/figures/readme-landscape.gif`) is rendered from
`inst/manim/scene_readme_landscape.py` by `tools/make_readme_gif.R`.
