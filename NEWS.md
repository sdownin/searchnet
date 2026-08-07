# searchnet 0.5.0

## New features

* **`saomnk_sim_ego_indist2()` and `saomnk_env_imitation()`: the K_CA imitation
  channel is now measurable on bipartite states.** For actor `i` and component
  `j`, the statistic is the centered performance similarity between `i` and the
  mean performance of `j`'s other holders, summed over the components `i` holds.
  It matches the definition used by the CD4 procedural engines, so the two are
  directly comparable.

  Components with no co-holders contribute nothing and are excluded from the
  centering mean. They are invisible rather than unattractive, which is what
  separates imitation from popularity; counting them as zeros would drag the
  center down and make held-but-unpopular components look repellent.

  Degenerate performance (zero range) yields similarity 1 everywhere, which
  centers to zero. That is the right answer on this channel: when all actors
  perform identically no component is more attractive than any other.

## Scope of the above, stated plainly

* **This is a STATISTIC, not yet an EFFECT.** RSiena's effect set for a
  bipartite dependent variable has 34 short names and none is a similarity
  effect; the nearest, `inPop_ego` and `outAct_ego`, are degree-based, and
  `simEgoInDist2` exists for one-mode networks only. searchnet's simulation path
  delegates to `siena07()`, so an effect RSiena cannot express cannot enter the
  evaluation function through it.

  K_CA is therefore now measurable in searchnet, where it was previously absent
  altogether, but it is still not simulable through `saomnk_run()`. Closing that
  gap needs either a C-level RSiena effect or a searchnet-native simulation
  loop. Callers must not read the presence of this statistic as evidence that
  imitation is driving a simulated trajectory.

# searchnet 0.3.4

## Testing

* **Two shipped tests were themselves wrong; both are fixed and the suite is now
  fully green (820 passing, 0 failures, 0 errors).**

* `test-multi-w-matrix.R` asserted `rowSums(B %*% W %*% t(B)) == rowSums(B)^2`
  for an identity `W`. That identity is false for `M > 1`:
  `rowSums(B B')_i = sum_j (B_i . B_j) = B_i . colSums(B)`, which equals
  `|B_i|^2` only when every actor holds the same bundle. On the seeded fixture
  it produced `3 5 8 1` against an expectation of `4 9 25 1`. Replaced with the
  three identities that actually hold: an identity `W` is a no-op
  (`B W B' == B B'`), the diagonal of `B B'` is actor scope, and its row sums
  are `B %*% colSums(B)`. **The engine was never implicated** — that block is
  pure matrix algebra and called no package code, so the `XWX` statistic was
  never in question.

* `test-fitness.R` passed `info =` to `expect_gte()`, which takes `label` and
  not `info`, so the expectation raised "unused argument" and errored instead
  of running. The landscape-peak check had therefore never actually executed.

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
  The three remaining failures were pre-existing and unrelated to loading, and
  all three are now resolved: `test-fitness.R:97` and `test-multi-w-matrix.R:45`
  were both faulty tests, fixed in 0.3.4 — note that the XWX one was a false
  assertion in the test, **not** a question about the statistic, contrary to the
  first version of this note; and `test-regression-guards.R:19` checks
  `asNamespace("searchnet")`, which was resolving to a stale installed build
  (0.1.0). Reinstalling cleared it.

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
