# searchnet 0.3.3

## Bug fixes

* **`inst/saomnk-loader.R` now sources every `.R` file in `R/` (28/28), not 11.**
  The loader previously sourced only `utils.R`, `saomnk-base.R`, `saomnk-class.R`,
  seven `plot-*.R` files and `saomnk-experiments.R`. Seventeen files were never
  loaded, so in any sourced (non-installed) session the following were simply
  absent: the classic NK layer (`nk_landscape()`, `nk_walk()`, `nk_local_optima()`,
  `nk_verify_reduction()`, `nk_to_saomnk()`), the clean functional API
  (`saomnk_env()`, `saomnk_model()`, `saomnk_run()`, `saomnk_shock()`), game and
  teaching modes, the causal-inference wrappers, basins, replicator, Brock–Durlauf,
  the mean-field solver, diagnostics, export, bridge, and three further plot modules.
  This is why 16 tests in `test-nk-classic.R` and `test-regression-guards.R` errored
  with "could not find function". Files are now discovered by glob, with the R6
  hierarchy (`utils` → `saomnk-base` → `saomnk-class`) loaded first and the rest
  alphabetically for determinism.

* **Loader self-location no longer silently fails.** `dirname(sys.frame(1)$ofile)`
  is `NULL` under `Rscript`, and the old fallback to `getwd()` produced
  `cannot open file '.../utils.R'`. Resolution now tries the caller's hint, every
  calling `source()` frame's `ofile`, `--file=`, and the working directory —
  expanding each candidate to `<d>`, `<d>/R`, `<d>/../R` — and validates by
  checking for `saomnk-base.R` before use. If nothing resolves it raises an
  actionable error instead of a confusing missing-file message.

* Loader dependencies are now split into hard (load must succeed) and soft
  (`cowplot`, `ggraph`, `ggpubr`, `grid`, `gridExtra`, `texreg`, `uuid`). A missing
  optional package degrades specific plot/report functions instead of aborting the
  whole load, and is named in the startup message.

* A file that fails to source no longer aborts the load silently: failures are
  collected in `.saomnk_failed` and surfaced as warnings.
  `options(saomnk.loader.strict = TRUE)` restores fail-fast behaviour;
  `options(saomnk.loader.quiet = TRUE)` suppresses the summary line.

## Notes

* Test suite with the fixed loader: **804 passing** (was 739), errors 16 → 1.
  The three remaining failures are pre-existing and unrelated to loading:
  `test-fitness.R:97` passes an `info=` argument that `expect_gte()` does not
  accept (a test bug); `test-multi-w-matrix.R:45` asserts `XWX == rowSums(B)^2`
  and gets `3 5 8 1` vs `4 9 25 1` (a real question about the XWX statistic, worth
  a separate look); and `test-regression-guards.R:19` checks
  `asNamespace("searchnet")`, which resolves to the **installed** build — currently
  **0.1.0**, long predating these functions. Reinstall the package to clear it.

# searchnet 0.3.2

## Bug fixes

* **Monadic covariate effects were silently dropped** (`egoX`, `altX`, `outActX`,
  `altXOutAct`, `homXOutAct`, `inPopX`), and so were any effects reaching the generic
  fallback in `include_rsiena_effect_from_eff_list()`. The consolidated branch introduced
  in 0.3.1 passed `shortName = eff$effect` to `RSiena::includeEffects()`, which has no
  such formal argument. The value therefore landed in `...`, where RSiena deparsed the
  unevaluated expression and searched for an effect literally named `eff$effect`. Because
  the call sat inside `tryCatch(..., error = function(e) warning(...))`, the failure was
  downgraded to a warning and the simulation continued **without the declared effect**.
  Both call sites now pass the effect name as a character string with `character = TRUE`.

* **Declared coefficients did not reach the simulation.** The same branch wrote the
  coefficient via `setEffect(initialValue = ...)`, but `get_theta_matrix()` reads
  `theta_in <- effs$parm` — the column populated by `setEffect(parameter = ...)`. An
  effect could therefore be included and still be inert. `parameter` is now always passed;
  `initialValue` is additionally passed when explicitly supplied, since the two are
  distinct (theta value vs. RSiena estimation start value).

  Together these two defects meant that, between 0.3.1 and this release, structure models
  declaring actor-strategy covariates ran as if those covariates were absent. Verified:
  with the fix, varying `egoX` from 0 to 3 changes the simulated outcome; before it did not.

* **Non-basic rate effects broke the theta matrix.** `get_rsiena_effects_theta_df(no_rates
  = TRUE)` filtered out every effect whose `shortName` matched `/rate/i`. RSiena excludes
  only the *basic* rate parameter from `thetaValues`, so any additional rate effect
  under-counted the columns and RSiena rejected the matrix ("should have N columns"). The
  filter now removes only the basic `Rate` parameter. This is behaviour-preserving for any
  model whose sole rate effect is the basic rate.

## New features

* **Heterogeneous and structural rate effects are now supported**:
  `RateX` (rate depends on an actor covariate, RSiena effect group `covarBipartiteRate`)
  and `outRate`, `outRateInv`, `outRateLog`, `inRateInv`, `inRateLog` (rate depends on the
  actor's own degree, group `bipartiteRate`). Declare them in the `rates` slot of a
  structure model:

  ```r
  rates = list(
    list(effect = 'RateX', parameter = 0.8, dv_name = DV_NAME, fix = TRUE,
         interaction1 = 'self$strat_1_coCovar')
  )
  ```

  This separates the *frequency* with which an actor may change (the rate function) from
  *which* change it prefers (the evaluation function) — the two are separately
  parameterised and separately estimable. Verified: with `RateX = 2` on a binary covariate,
  the two actor groups differ by a factor of ~12.6 in realised tie changes, against ~1.5 at
  `RateX = 0`.

## Testing

* New `tests/testthat/test-effect-registration.R` (15 assertions) guards the above:
  every declared effect must be included, its `parameter` must reach the `parm` column,
  and a covariate effect must demonstrably change simulated outcomes. These tests fail
  against 0.3.1 and pass here.

# searchnet 0.3.1

## Performance

* `searchnet_game_step()` no longer recomputes choice probabilities once per
  actor. `compute_choice_probabilities()` evaluates all `M` actors in a single
  pass, but it was being called inside the actor loop with all but one element
  discarded each time — `M` times the necessary work, giving roughly `M^2 * N`
  scaling. The call is now hoisted out of the loop.

  Measured (5 moves incl. `searchnet_game_available_moves()`, R 4.5.3):

  | Size | Before | After | Speedup |
  |------|--------|-------|---------|
  | M=8, N=12 | 0.78 s/move | 0.234 s/move | ~3.3x |
  | M=20, N=30 | 18.23 s/move | 1.358 s/move | ~13x |

  This makes turn-based interactive play viable at experiment-relevant sizes.

## Behavior change

* As a consequence of the above, AI opponents in `searchnet_game_step()` now
  all respond to the same round-start board state (after the player's move),
  i.e. **simultaneous moves within a round**. Previously each AI observed the
  partially-updated board left by lower-indexed actors, which made round
  outcomes depend on actor ordering.

  This matches the semantics already used by `searchnet_classroom_advance()`
  and is the correct behavior for controlled experimental play. Games seeded
  identically will not reproduce pre-0.3.1 trajectories.

# searchnet 0.3.0

* Added classic NK module and cross-sectional regression discontinuity.

# searchnet 0.2.0

* Initial public pre-release.
