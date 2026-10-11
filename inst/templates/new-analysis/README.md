# {{TITLE}}

A SAOM-NK analysis skeleton created by `searchnet_new_analysis()` with
searchnet {{SEARCHNET_VERSION}}. Every section runs on the bundled synthetic
example; replace the data and keep the gates.

## Install the version this was created with

```r
install.packages("remotes")
{{INSTALL_LINE}}
```

`analysis.Rmd` records the searchnet and RSiena versions it ran with. If they
differ from the line above, results may differ too.

## Run

```r
rmarkdown::render("analysis.Rmd")                              # full settings
rmarkdown::render("analysis.Rmd", params = list(tiny = TRUE))  # smoke run only
rmarkdown::render("analysis.Rmd", params = list(data = "long_csv"))
```

Header parameters: `data` (`"synthetic"` or `"long_csv"`), `seed`, `tiny`
(small sizes for a smoke run; read nothing from it), `run_recovery` (the
slowest section; `false` skips it).

## Files

- `analysis.Rmd`: the analysis.
- `R/world.R`: the calibrated comparison world and small data helpers.
- `data/example_long.csv`: a synthetic long-format panel, one row per
  actor-component-period tie (columns `actor`, `component`, `period`).

## What each section checks

1. **Setup.** `searchnet_check_setup()` passes; versions and seeds are
   recorded. Guards against a broken installation being read as a result.
2. **Data.** Long records become an actor-by-component array via
   `searchnet_bipartite_from_long()`, so synthetic and real data share a path.
3. **{K} readings.** `searchnet_k_readings()` asserts the accounting
   identities; a malformed array stops here.
4. **Moment gate.** Tolerances are fixed in the `preregistered-criteria`
   chunk before any simulation. `searchnet_moment_gate()` checks that the
   simulated world reproduces the observed moments; it guards against tests
   run in the wrong world.
5. **Recovery.** `searchnet_recovery()` plants an effect and re-estimates it;
   it guards against reading an estimate or a null from a design that cannot
   identify it.
6. **Counterfactual shock.** `searchnet_shock_support_check()` asks whether
   the post-shock world is structurally poorer, and `searchnet_placebo()`
   asks whether the estimator finds an effect when exposure is random. Both
   guard against a result produced by construction.
7. **Report.** Every null is printed with its minimum detectable effect,
   since a null is a bound on an association.
