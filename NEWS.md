# searchnet 0.8.3

JSS submission preparation, Phase 1 (items b, c, d of JSS_SUBMISSION_PREP_2026-08-15.md),
merged onto the 0.8.2 terminology sweep.

## Style

* **`T`/`F` shorthand is gone from package code: 290 sites now read
  `TRUE`/`FALSE`**, plus two cramped assignments spaced. The sweep operated on
  `getParseData()` tokens rather than text, so strings and comments were
  untouchable by construction, and the two functions whose formal `T` is a
  temperature (`solve_mean_field()`, `diagnose_mean_field_fit()`) were
  excluded automatically. Every hunk of the diff was pair-checked line for
  line (245 insertions / 245 deletions, 0 mismatches). JSS and Hyndman both
  name this as a review item.

## Classes and methods

* **`print()` methods for the user-facing returns**, in `R/saomnk-methods.R`:
  `print.saomnk_model()` (effects with thetas and fixed/free flags, covariates,
  static influence matrices with dimensions, time-varying influence arrays with
  period counts, behaviour DV presence), `print.saomnk_shock()`,
  `print.saomnk_assent()`, and `print.saomnk_summary()`. JSS's minimum
  expectation is that R's class and method systems are leveraged on returns;
  typing a model object at the console now shows its specification instead of
  a list dump.

* `saomnk_summary()` printed its table and then a quoted, escape-riddled copy
  of the same string; it now prints once. The return carries class
  `saomnk_summary`.

## Documentation

* **Examples on every user-facing help page**: 83 of 155 pages had an
  `\examples{}` section; 130 of 160 do now (43 written; 5 pages for the
  diagnostic screens generated for the first time). The 30 pages without are
  internal helpers and `@name`-only module overviews. Simulation-dependent
  examples are `\donttest{}` with tiny seeded environments; none is
  `\dontrun{}`. All 36 executable blocks were run to prove they execute.

## Known issue (open, not fixed here)

* The four phase-space plot functions (`saomnk_plot_phase_space_3d()`,
  `_heatmap()`, `_evolution()`, `_comparison()`) error on real engine output:
  `.extract_phase_data()` returns a `data.table`, and the plots subset it with
  `df[complete.cases(df[, cols]), ]`, which under data.table j-semantics
  recycles to a length-2 logical. Their examples carry an explicit
  `as.data.frame()` workaround line until `.extract_phase_data()` returns a
  data.frame; that one-line fix is queued.

# searchnet 0.8.2

## Terminology

* **W is the influence matrix throughout; epistasis is the fitness outcome.**
  The 0.4.0 release renamed the API (`influence_matrix`, `influence_weight`,
  `influence_matrices`, `influence_weights`, with deprecation shims that keep
  working). This release finishes the prose half: roxygen and code comments,
  the generated help pages, every vignette, the proofs (`PROOF_TABLE.md`, the
  NK-equivalence proof, the proof tutorial), the JSS paper and its online
  appendix, the notation concordance and the submission bundle now all say
  "influence matrix" for the N x N object a user passes in, and reserve
  "epistasis" for what those influences produce through the utility. This is
  the standard the companion paper carries (CD2026, Paper T): W is the
  influence matrix, the NK interaction matrix with real-valued entries giving
  the magnitude and sign of one activity's influence on another's fitness
  contribution; its binary support E is the influence pattern; epistasis
  (K_CC) is the fitness interdependence W induces, indexed by the density and
  pattern of W. Theorem 1 (R2) now reads "E = A, the SaoMNK influence matrix
  equals the NK influence matrix". K_CC keeps its label "Epistasis";
  "epistasis parameter K" and the adjective "epistatic" are unchanged. One
  concordance sentence survives, in the `saomnk_model()` help for
  `influence_matrix`, so a reader arriving from the NK literature can find the
  object.

* `saomnk_empirical_epistasis()` is deprecated in favour of
  `saomnk_empirical_influence()`; it returns an influence-matrix estimate
  built from realized co-holding, not epistasis. Same arguments, same return;
  the old name warns once per session and then calls the new function.

* `epistatic_int_mat` on `saomnk_plot_bipartite_ring_markets()`,
  `saomnk_plot_bipartite_ring_markets_animation()`, their R6 method twins and
  `get_component_groups_list()` is deprecated in favour of `influence_matrix`.
  The new name takes the old argument's position, so positional calls are
  unaffected; the old name is kept as a trailing formal with a warning, and
  the new name wins if both are supplied.

* A terminology gate, `tests/testthat/test-terminology.R`, fails the suite if
  "epistasis matrix" or "interaction matrix" reappears in R/, man/,
  vignettes/ or inst/proofs/ outside the one allowed concordance sentence.
  The 2026-08-21 audit found 67 stale uses after the API rename had
  supposedly settled the question; the rule now lives in a test.

## Bug fixes

* `get_component_groups_list()` referenced a bare `N` where it meant
  `self$N`, so calling it directly with an influence matrix errored with
  "object 'N' not found". Found by the new deprecation test.

## Documentation

* Three help pages that carried stale titles were hand-maintained Rd files
  that roxygen refuses to overwrite (45 of 160 pages are in that state,
  marked "Generated manually for R CMD check compliance"); they are now
  roxygen-owned, with aliases, arguments, value and examples preserved. Five
  pages that existed only as roxygen sources (the four diagnostic screens and
  their overview) are generated for the first time.

# searchnet 0.8.1

## Bug fixes

* **The Theorem 4 empirical check no longer compares the simulation against an
  object that is not the law of the simulated process.** The check had asserted
  the live simulation lands within 0.10 (spin form) of the linear Curie-Weiss
  roots at a nominally supercritical coupling. It failed at 0.38 the first time
  it actually ran -- it had been gated behind NOT_CRAN since it was written --
  and three independent diagnoses (an adversarial code audit, an M-sweep over
  12..200 with 25+ seeds per cell, and exact finite-M Gibbs computation)
  converged on the same verdict: the assertion, not the simulation, was wrong.

  RSiena's `inPop` evaluation delta is sqrt-form, so the simulated process obeys
  the fixed point p = sigmoid(beta*(h_b + theta*sqrt(M*p+1))) -- the Option B
  object PROOF_TABLE.md L16 designates "the binding numerical comparison" --
  not the linear tanh roots. Exact per-column Gibbs laws reject the linear
  reading at |z| > 80 and the squared reading at |z| > 2000; the sqrt family
  matches every empirical anchor within 2 SD. The observed 0.38 was the
  sqrt-vs-linear model gap itself: flat in M (asymptote ~0.41), 15x the genuine
  finite-size budget at M = 12 (0.025). No tolerance against the old object was
  defensible at any M -- the 6-column-average observable had an asymptotic
  failure floor of 0.656.

  `diagnose_mean_field_fit()` now reports both objects: the binding Option B
  fixed point (which `discrepancy_adopt`/`discrepancy_spin` now measure
  against), and the linear-CW reference with an `in_BD_regime` flag marking
  L16's validity regime (near p = 1/2, sub-threshold). It also extracts the
  density coefficient as the field term -- previously dropped entirely, so the
  analytical side silently assumed h = 0 against a simulation running at
  h_b = -1 -- and matches the reference against STABLE roots only. The old
  `which.min` over all roots selected the unstable m = 0 root for 49 of 76
  seeds at M = 12, making the reported gap shrink exactly when the simulation
  was furthest from any attainable equilibrium.

  The rewritten test asserts |p_emp - p_binding| < 0.15 in adoption form, a
  derived bound: the exact finite-M q95 of |p_emp - E[p]| is 0.119-0.138 at
  M = 12, N = 6, plus short-chain autocorrelation excess. A second test
  constructs L16's Option C regime (sub-threshold, fixed point near 1/2) and
  verifies the Brock-Durlauf correspondence there -- the part of Theorem 4 the
  live harness CAN verify. The supercritical pitchfork is no longer asserted
  anywhere, per L16: it "cannot be empirically verified via the live harness."

* `solve_mean_field()` documentation now states what the function returns --
  the zero-field linear Curie-Weiss REFERENCE object, not the stationary law of
  an `inPop` simulation -- and cites `inst/proofs/PROOF_TABLE.md` rows L8/L10/
  L16 instead of a proof file that does not exist in the repository.

## Scope notes

* The manuscript record was swept for the same defect: the JSS paper and its
  Appendix H are consistent (H explicitly disclaims supercritical live
  verification), and the industrial-policy manuscript keeps `inPop`/`inPopSqrt`
  distinct throughout. The defective comparison existed only in
  `solve_mean_field()` + `diagnose_mean_field_fit()` + this test. One teaching
  primer overclaimed the coefficient-to-betaJ mapping and now states the
  linearity condition and L16 rescaling.

# searchnet 0.8.0

Merges the diagnostics port and time-varying W (developed as 0.7.0 on
`feature/diagnostics-and-time-varying-w`) into `dev`, which had meanwhile
advanced to 0.7.5 on unrelated work. The features were therefore unreleased
until this merge; 0.8.0 is the minor bump that releases them. The `v0.7.0` tag
remains as the feature branch's own marker and is an ancestor of `dev`, but
`dev` never released a 0.7.0.

See the 0.7.0 entry below for what the features do. Nothing changed in them at
merge: `tests/verify_searchnet_port.R` passes 28 of 28 against merged `dev`,
which also confirms the concurrent 0.7.1 through 0.7.5 work did not disturb
them.

# searchnet 0.7.1

## Bug fixes

* **`clone(deep = TRUE)` now actually deep-copies `data.table` fields.** R6's
  deep clone recurses only into fields that are themselves R6 objects, so a
  clone and its original were bound to the SAME table: identical
  `data.table::address()`, and a `:=` update on one added a column to the other.
  `:=` bypasses copy-on-modify by design, which is exactly why it defeated the
  default clone. A `private$deep_clone()` now copies data.tables and passes
  everything else through unchanged.

  No release shipped a defect from this: results are installed by assignment
  (`self$actor_stats_df <- ...`), never by reference update, so the per-seed
  clone in the market-plot batch loop was always safe. It closes a trap that
  would have armed the moment anyone wrote `:=` against an env's data.table, and
  whose symptom would have been cross-contaminated runs in a seed batch.

* **`searchnet_export_k4()` and `searchnet_export_all()` no longer fail on
  `K_CC`.** `strategy` is an actor attribute, and `K_CC_df` is component-by-
  component, so it is built without one. The exporter read
  `env$K_CC_df$strategy` anyway, which `data.frame()` saw as a zero-length
  column against 160 rows: "arguments imply differing number of rows: 160, 1,
  0". It now uses `NA_character_`, matching how `actor_id` was already handled
  on the same rows and for the same reason.

## Testing

* **The test harness was sourcing 4 of 34 files in `R/`, and had been since
  network--behaviour coevolution landed.** `helper-setup.R` hand-listed
  `utils.R`, `saomnk-base.R`, `saomnk-class.R` and `mean_field_solver.R`. When a
  `.searchnet_has_behavior()` call was added inside `saomnk-class.R`, the file
  defining it was not on that list, so every simulation-dependent test died with
  "could not find function" -- and the surrounding `tryCatch` turned each one
  into a skip. Five test files reported green while the simulation path was
  entirely broken.

  This is the same defect fixed in the loader at 0.3.3, where sourcing 11 of 28
  files left whole modules absent. The loader was fixed by globbing; this second,
  hand-kept copy then drifted the same way. `helper-setup.R` now delegates to
  `inst/saomnk-loader.R`, so load order is defined once.

* **The attached-package list is now derived from NAMESPACE.** Sourcing `R/*.R`
  directly does not activate `importFrom()`, so imported functions must be on
  the search path some other way. The hand-written `library()` list named 13
  packages while NAMESPACE imported from 11 more, so `scales::hue_pal` was
  absent and a plotting test errored. A third hand-kept dependency list, going
  stale the same way as the other two.

* **158 tests no longer report a failure as a skip.** 20 state guards of the form
  `if (is.null(env$K_AC_df)) skip(...)` became assertions: they sit after a
  `skip_if_not_installed("RSiena")` and after a run that did not throw, so an
  empty field there is the engine silently producing nothing. A further 138
  handlers of the form `tryCatch(..., error = function(e) skip(...))` now
  `stop()`, so a crash is an error rather than a green run. Two sites carrying an
  explicit author rationale for tolerating failure were left alone.

* Suite after these changes: **1585 passing, 0 failures, 1 error, 16 skips.**
  The single error is a pre-existing defect in `search_rsiena_multiwave_plot()`
  ("argument is of length zero"), which the skip pattern had been hiding; it is
  recorded rather than papered over.

* The M=1 NK-greedy test asked `search_rsiena()` for something the package
  refuses by design, since RSiena's `sienaDataCreate()` does not support
  single-actor bipartite networks. It now asserts that documented refusal. The
  greedy-property claim itself is marked in the file as NOT YET COVERED, to be
  re-tested against the landscape methods that do work at M = 1, rather than
  deleted and mistaken for covered ground.

# searchnet 0.7.0

## New features

* Four diagnostics from a companion methods manuscript are now package functions,
  so the checks live with the engine rather than in a paper's scripts:
  `boundary_screen()`, `scope_confound_screen()`, `gof_battery()` and
  `rate_ladder()`.

  `boundary_screen()` is the one to run first. It classifies candidate
  degree-threshold effects from the observed data alone, before any model is
  fitted: a statistic sitting at exactly 0 or exactly 1 of its attainable range
  cannot converge under method of moments, and nothing else disqualifies an
  effect. A balanced panel forces that position on every effect keyed to the
  empty portfolio.

  `scope_confound_screen()` reports the correlation between each coupling
  statistic and actor scope under raw, row-normalized and banded treatments.
  Row-normalization does NOT fix the confound, because it lives in the density
  pattern rather than the scale; banding does.

* `saomnk_model()` gains `influence_arrays` and `influence_array_weights` for
  **time-varying couplings**. Each entry is an N x N x P array or a list of P
  N x N matrices, P being periods (one fewer than waves), and generates an
  RSiena `varDyadCovar` carrying an `XWX` effect. This closes a long-standing
  gap where the `component_N_varDyadCovar` slots were declared while the
  assembly returned an empty list, so a coupling could not change between
  periods.

  Static and time-varying couplings coexist in one model and occupy separate
  slot sequences, so neither renumbers the other. Callers that do not pass
  `influence_arrays` are unaffected; that is asserted in the verification suite
  rather than assumed.

## Verification

* `tests/verify_searchnet_port.R`, 28 checks, all passing. Runnable rather than
  testthat because the package has no testthat harness wired up; it exits
  non-zero on failure so it can gate a commit.

## A note on version numbering

Tags `v0.5.0` and `v0.6.0` were never cut: `DESCRIPTION` had already been
advanced to 0.6.0 while the newest tag was `v0.4.1`, so version and tags had
drifted two minor versions apart before this release. This release bumps to
0.7.0 and tags it, which reconciles the two going forward but leaves that gap in
the tag history rather than back-filling tags for states no one can now
reconstruct.

# searchnet 0.5.1 (development)

## New features

* **`saomnk_theta_ramp()` and `saomnk_theta_drift()`: the environment can now
  change continuously, not only in steps.** Until now the only way to make a
  parameter move over time was `saomnk_shock()`, which cuts the ministep chain
  into contiguous blocks and holds a constant within each. That expresses "a
  shock happened at time t" and nothing else. No number of steps is a ramp, so
  "the environment erodes at rate r" was simply not sayable.

  `saomnk_theta_ramp()` moves one or more effects from a starting value to an
  ending value over a window of the chain, under `linear`, `sigmoid` or
  `exponential` easing. Windows are given as fractions of the chain rather than
  absolute ministep indices, so the same specification means the same thing at
  any chain length. `saomnk_theta_drift()` is the separate, undirected operator:
  a Gaussian random walk on one parameter, which is landscape *instability*
  rather than landscape *direction*. The two compose --- `add = TRUE`
  superimposes a walk on an existing ramp --- so a design can vary erosion and
  volatility independently.

  The engine already accepted a user-supplied `theta_matrix`; what was missing
  was any way to build one correctly. Column order is decided by RSiena's
  effects table, not by the structure model, so hand-building the matrix was not
  something a caller could do reliably. Both functions delegate to a new
  `env$prepare_theta_scaffold()`, which runs the same data-and-effects
  construction `search_rsiena()` runs and returns the correctly shaped, correctly
  named matrix.

  The sigmoid is rescaled onto `[0, 1]`. The raw logistic
  `1/(1 + exp(-6*(p - 0.5)))` starts at 0.047 and ends at 0.953, so an
  unrescaled version would jump discontinuously by about 5% of the total change
  at each end of the window. There is a regression test for this.

* **`saomnk_run()` now forwards a `theta_matrix` argument.** Additive, default
  `NULL`; existing calls behave exactly as before. When supplied, its row count
  is the ministep count and `steps_per_actor` is not also passed, so the two
  cannot silently disagree.

* **`saomnk_behavior()`, `saomnk_behavior_effects()`, `saomnk_get_behavior()`:
  network--behaviour coevolution.** A structure model may now carry a
  `dv_behavior` block, making an actor-level attribute (performance,
  aspiration, capability) a second dependent variable that evolves jointly with
  the bipartite network instead of sitting fixed as a covariate. Both directions
  are live: the network shapes the behaviour through influence effects, and the
  behaviour shapes the network through selection effects declared on
  `dv_bipartite` with `interaction1` pointing at the behaviour DV.

## What RSiena can and cannot do here, stated plainly

* **RSiena 1.5.0 does support bipartite + behaviour coevolution.** This was
  verified against a live `getEffects()` object, not recalled. `sienaDataCreate()`
  accepts both dependent variables and returns a populated effect set for the
  behaviour. Unlike the K_CA case below, this is a real EFFECT capability and
  not merely a statistic: the behaviour enters the simulated evaluation function
  and moves.

* **The one-mode influence effects `avAlt`, `totAlt`, `avSim` and `totSim` are
  NOT available for a behaviour attached to a bipartite network, and no amount
  of R-level work can add them.** In a bipartite network ego's direct alters are
  *components*, and components have no behaviour to average. This is a property
  of the model, not a gap in RSiena or in searchnet.

  RSiena's substitutes are the distance-2 family, where two actors are
  neighbours when they hold a component in common: `avInAltDist2`,
  `totInAltDist2`, `avTInAltDist2`, `totAInAltDist2`, `avInSimDist2`,
  `totInSimDist2`. **`avInSimDist2` is the bipartite counterpart of `avSim`**,
  and is what a caller reaching for "imitation" or "social influence" wants.
  Also available on the behaviour DV: `linear`, `quad`, `constant`,
  `threshold1-4`, `simAllNear`, `simAllFar`, `avGroup`, `outdeg`, `outIsolate`,
  `popAlt`, `effFrom`, `avXAlt`, `totXAlt`, and the covariate distance-2 family
  (`avXInAltDist2`, `totXInAltDist2`, `avTXInAltDist2`, `totAXInAltDist2`).

  In the selection direction RSiena offers `egoX`, `egoSqX`, `altInDist2`,
  `totInDist2`, `simEgoInDist2`, `sameEgoInDist2`, `inPopX`, `sameXInPop`,
  `diffXInPop`, `sameXCycle4`, `avGroupEgoX`, `degAbsDiffX`, `degPosDiffX`,
  `degNegDiffX` and `sameWXClosure`.

  `saomnk_behavior_effects()` regenerates all of this from a live `getEffects()`
  call rather than from documentation, and the test suite asserts both the
  presence of the distance-2 effects and the ABSENCE of `avSim`/`avAlt`. If a
  future RSiena release changes either, those tests will say so.

* **A behaviour DV changes RSiena's estimation mode, and therefore what a row of
  the theta matrix means.** RSiena estimates *conditionally* with one dependent
  variable and *unconditionally* with two or more. Under conditional estimation
  the conditioning variable's basic rate is deleted from the parameter vector,
  which is why searchnet has always been able to drop basic rates from the theta
  matrix. Under unconditional estimation every basic rate *is* a theta column,
  and `siena07()` rejects a matrix of the wrong width outright. `get_theta_matrix()`
  now derives the width from the number of dependent variables rather than
  assuming.

  The consequence for callers: with one DV each theta row corresponds to exactly
  one ministep. With two DVs the number of ministeps per row is drawn from the
  rate parameters, so **the chain is longer than the theta matrix has rows** and
  a "run" is no longer a ministep. `saomnk_get_behavior()` reports behaviour once
  per run; per-ministep behaviour changes are in `env$chain_stats`, in the
  `beh_difference` column of rows whose `dv_varname` is the behaviour DV.

* **An undeclared basic rate defaults to 1.0, with a message.** RSiena's `parm`
  column defaults to 0 for basic rates, and searchnet reads `parm` as theta. A
  rate of exactly 0 freezes that dependent variable for the entire simulation.
  That is never an intended specification, so it is substituted and announced
  rather than silently simulated.

* **The utility and K-4 decompositions cover the bipartite evaluation function
  only.** `linear`, `quad`, `avInSimDist2` and the rest are statistics of the
  behaviour, not of the bipartite matrix, and have no per-actor decomposition on
  that path. They are excluded from `actor_stats_df` rather than fabricated.
  Extending the decomposition to the behaviour evaluation function is a separate
  piece of work.

## Bug fixes / hardening

* `search_rsiena_process_ministep_chain()` and `get_chain_stats_list()` now skip
  behaviour ministeps when reconstructing the bipartite state trajectory. A
  behaviour ministep's `id_to` column carries a behaviour value, not a component
  id, so toggling on it would have silently corrupted every downstream network
  statistic. There is a test asserting the bipartite matrix never changes on a
  behaviour ministep.

* The generic effect-inclusion fallback in
  `include_rsiena_effect_from_eff_list()` now passes `interaction2` through to
  `includeEffects()` / `setEffect()`. Two-slot effects such as `avXAlt` and the
  covariate distance-2 family are identified by both a covariate and the network
  through which it reaches ego, and could not be included without it. Inert for
  every structure model that predates this release.

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
