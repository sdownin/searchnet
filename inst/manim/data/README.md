# Dashboard CSV Data Schemas

This directory holds CSV files exported by `searchnet_export_for_manim()` and
consumed by the manim dashboard scenes in `dashboard_panels.py`.

## 1. k4_trajectories.csv

K-degree evolution across simulation rounds, averaged per carrier per round.

| Column    | Type      | Description                                        |
|-----------|-----------|----------------------------------------------------|
| `round`   | integer   | Simulation round (1-based; each round = M steps)   |
| `carrier` | character | Carrier label (e.g. "AA", "DL", or "A1", "A2")    |
| `K_AC`    | numeric   | Actor-to-component degree (bipartite degree)       |
| `K_CC`    | numeric   | Component-to-component degree (route co-presence)  |
| `K_CA`    | numeric   | Component-to-actor degree (routes shared w/ firm)  |
| `K_AA`    | numeric   | Actor-to-actor degree (social projection degree)   |

Used by: `KDegreeEvolutionScene`

## 2. bipartite_snapshots.csv

Long-form active ties at selected snapshot steps.

| Column    | Type      | Description                                        |
|-----------|-----------|----------------------------------------------------|
| `round`   | integer   | Simulation round at snapshot                       |
| `carrier` | character | Carrier label                                      |
| `route`   | integer   | Route (component) index (1-based)                  |
| `active`  | integer   | Always 1 (sparse format; inactive ties omitted)    |

Used by: `BipartiteNetworkScene`

## 3. utility_trajectories.csv

Actor utility decomposition, averaged per carrier per round.

| Column          | Type      | Description                                  |
|-----------------|-----------|----------------------------------------------|
| `round`         | integer   | Simulation round                             |
| `carrier`       | character | Carrier label                                |
| `total_utility` | numeric   | Total objective-function utility              |
| `nk_component`  | numeric   | NK fitness component (outAct contribution)    |
| `scope_cost`    | numeric   | Density (scope) cost component               |
| `popularity`    | numeric   | Popularity component (often 0 in baseline)   |
| `rivalry`       | numeric   | Rivalry component (inPop contribution)        |

Used by: `InformationFunnelScene`, `GameTheoreticScene`

## 4. shock_events.csv

Exogenous shock events injected during the simulation.

| Column        | Type      | Description                                    |
|---------------|-----------|------------------------------------------------|
| `round`       | integer   | Round at which the shock occurs                |
| `shock_type`  | character | Type identifier (e.g. "W-matrix", "exit")      |
| `magnitude`   | numeric   | Shock magnitude (scale depends on type)        |
| `description` | character | Human-readable description of the shock        |

Used by: `ShockResponseScene`

Empty in baseline (no-shock) simulations.

## 5. phase_space.csv

PCA reduction of carrier state vectors (N-dim binary route vectors
projected onto 3 principal components).

| Column    | Type      | Description                                        |
|-----------|-----------|----------------------------------------------------|
| `round`   | integer   | Simulation round at sample point                   |
| `carrier` | character | Carrier label                                      |
| `PC1`     | numeric   | First principal component score                    |
| `PC2`     | numeric   | Second principal component score                   |
| `PC3`     | numeric   | Third principal component score                    |

Used by: `FitnessLandscapeScene`

## Generating these files

From R:

```r
library(searchnet)

env <- saomnk_env(M = 8, N = 128)
model <- saomnk_model(env, density = -1.5, inPop = -6.0, outAct = 0.3)
saomnk_run(env, model, iterations_per_actor = 100)

searchnet_export_for_manim(
  env,
  dir = system.file("manim", "data", package = "searchnet"),
  carrier_labels = c("AA", "DL", "UA", "WN", "AS", "B6", "NK", "F9")
)
```

Then render from the command line:

```bash
cd inst/manim
python render_dashboard.py --list
python render_dashboard.py --scene k4_evolution --quality medium
python render_dashboard.py --dashboard all --quality high
```
