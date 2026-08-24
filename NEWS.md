# searchnet 0.9.3

Documentation and terminology release. No behavioral change to the engine;
no result from 0.9.2 is invalidated.

## Terminology: "epistasis" names three things, not two

The v0.8.2 entry below states that "epistasis (K_CC) is the fitness
interdependence W induces, indexed by the density and pattern of W."
**That claim is corrected here**, and the earlier entry is left standing as
the dated record of what v0.8.2 said rather than rewritten.

Two things were wrong with it. First, `K_CC` is
`colSums((B'B) > 0)` --- a degree of the realized bipartite projection, in
which `W` does not appear. A degree cannot *measure* a fitness consequence;
the two are different kinds of quantity. Second, and less obviously, the
claim was false of the running code at the time it was written: until the
theta repair in v0.9.0 the declared `XWX` coefficient was never simulated, so
`W` could not influence `K_CC` at all in v0.8.3 and earlier.

The distinction now stated throughout the package, the README, and the JSS
manuscript:

| | | |
|---|---|---|
| **`W`** | the **input** | an N x N matrix, set by the analyst |
| **`K_CC`** | the **realized structure** | `colSums((B'B) > 0)`; `W` is absent from the formula, but drives the search that produces `B`, so `K_CC` evolves as a consequence of `W` |
| **epistatic fitness** | the **outcome** | the `XWX` effect weighted by `influence_weight`, plus the `synergy` term `b_i' W b_i / N^2` |

`K_CC` keeps its label *Component Epistasis*, per the v0.8.2 decision. No
argument, effect, or column name changes.

That `W` drives `K_CC` is now demonstrated rather than asserted: holding
seeds and every other parameter fixed and varying only `W`, mean `K_CC` moves
from 13.95 (no influence) to 16.40 (block-diagonal `W`) to 20.00 (its
complement), and zeroing the matrix gives a run bit-identical to zeroing
`influence_weight`.

## Fixes

* **`man/saomnk_model.Rd` documented the pre-0.4.0 API.** It listed
  `epistasis_matrix` and `epistasis_weight` as the argument names and
  described `W` as "the epistasis effect", unchanged since v0.2.0 --- so
  `?saomnk_model` had contradicted the package for two years of releases.
  Note for maintainers: this file is **not roxygen-owned**, and
  `roxygenise()` silently *skips* files without the roxygen header, so it
  would never have been regenerated. 45 of 165 `man/` files are in that
  state.

* **`paper/_searchnet-jss-web.Rmd` is now tracked.** It was gitignored while
  being the only source of `docs/paper/index.html`, which is served on the
  project's GitHub Pages site --- so the published page was unregenerable if
  that file were lost. It also shared `cache.path` with
  `paper/searchnet-jss.Rmd`, polluting the manuscript's knitr cache; it now
  has its own.

* **American English throughout**, per the project's style rule, including
  code comments: roughly 200 sites across 68 files. `summarise()` (dplyr) and
  `colour=` (ggplot2) are API names and were changed only where they appear
  in prose.

* The JSS manuscript's terminological note is rewritten and all four PDF
  artifacts re-rendered; `docs/paper/index.html` and the PDW slide deck are
  regenerated.

# searchnet 0.9.2

Version 0.9.1 was never released: a concurrent session tagged a
documentation-only commit as `v0.9.1` and pushed the tag, and the number was
skipped rather than reclaimed by force-pushing over a published tag. Nothing
described under 0.9.1 anywhere is missing here.

## New: the Theorem 4 demonstration is now a measurement

* **`searchnet_ergodicity_sweep()`** (new, exported, with `print()` and
  `plot()` methods) measures independence from initial conditions rather
  than asserting it. It runs the same fixed-coefficient model from two
  contrasting starting densities across a range of chain lengths, with
  replicates at each, and reports how fast the between-arm gap decays,
  ending in a two-one-sided-tests equivalence verdict against a margin
  declared in the call.

  **Why this replaced what was there.** Four documents -- the JSS paper, its
  online appendix, the Blume tutorial vignette, and the proof registry --
  illustrated Blume's ergodicity result the same way: one run from a sparse
  start, one from a dense start, on a 4 x 5 = 20-cell matrix, followed by an
  unconditional `cat()` asserting that the two "converge toward a similar
  equilibrium density." That illustration could not fail. It printed its
  conclusion regardless of the numbers; a single tie moved the density by
  0.05, coarser than any threshold worth declaring; and one draw per arm gave
  no sampling distribution. The same sentence was printed under a gap of 0.10
  in one document and 0.20 in another.

  The replacement is capable of returning "not equivalent," and does so at
  short run lengths. At the default configuration a starting gap of 0.70
  collapses by over 99% and decays as a power of run length with slope near
  -1 (R^2 > 0.9). The proof registry's F5 check, previously
  `density_gap < 0.5` on a pair of runs that started 0.70 apart, now requires
  both decay and equivalence.

  Two methodological details are surfaced rather than hidden: the Monte Carlo
  floor (once the arms genuinely agree, the measured gap settles at the
  expected difference of two sample means, not zero) is drawn on the plot and
  excluded from the decay fit; and an equivalence margin finer than the grid's
  density resolution is a hard error, since it asks the grid to resolve less
  than one tie.

* **The `process_chain` trap is now documented in five places.**
  `search_rsiena(process_chain = FALSE)` does not write the simulated end
  state back to `$bipartite_matrix`, so a final density computed after such a
  call silently returns the *starting* density and the chain appears never to
  mix. `searchnet_ergodicity_sweep()` forces `process_chain = TRUE`.

## Fixes

Two follow-on fixes surfaced by re-rendering the vignettes against the fixed
v0.9.0 engine -- both were reachable only because the theta-storage repair
made previously-inert code paths active for the first time.

* **README callout restructured** (`ccbab70`, documentation only). The v0.9.0
  upgrade notice was a separate red `[!CAUTION]` block above the standing
  `[!WARNING]` development-status block. With no external stars or followers
  yet, a severe red callout for a pre-adoption package overstated the
  audience; the necessary facts -- what broke, that estimation was
  unaffected, that it is fixed in v0.9.0 -- now sit briefly inside the one
  `[!WARNING]` block, with a link to this file's v0.9.0 entry for the full
  technical detail. No code change.

* **`searchnet_synth()`** now detects a pre-treatment predictor with zero
  variance across control units before calling `Synth::dataprep()`, drops it
  with a named warning, and only refuses outright below 2 usable predictors.
  Previously such a call died inside `Synth` with "At least one predictor in
  X0 has no variation across control units," naming none of the offending
  steps. A genuinely coupled process (the kind v0.9.0 now actually simulates)
  can legitimately pin every control actor to the same value at an early
  step; that is a property of the DGP, not a data error.
* **Teaching presets** (`inst/teaching/presets/*.json`) declare their
  coupling weight under the key `epistasis_weight`, the pre-0.4.0 name;
  `searchnet_classroom_init()` read `influence_weight`. Before v0.9.0 this
  was harmless -- the weight didn't simulate regardless of its value -- and
  after the fix it silently flattened all three presets onto the 0.4
  fallback, erasing their declared pedagogical contrast (airline 0.4, tech
  0.5, pharma 0.45). Now checks both keys.

# searchnet 0.9.0

## Theta-storage repair (2026-08-23): `parm` is NOT theta

* **Defect.** `get_theta_matrix()` built the simulated theta vector from the
  effects table's `parm` column, while the `cycle4`, `XWX` and `X` branches of
  `include_rsiena_effect_from_eff_list()` wrote the declared coefficient into
  `initialValue`. RSiena's `parm` is the *internal effect parameter* -- the `#`
  substitution in effect and function names, a root exponent for `cycle4`
  (`(count)^(1/#)`), `inPopX` and `outActX` -- not a coefficient. Consequences,
  all silent: `cycle4` was always simulated at `parm`'s default **1**, and
  `XWX` / `X` at **0**, whatever the caller declared; the sixteen branches that
  wrote coefficients into `parm` worked only because their effects carry no
  `#`; and for `inPopX` / `outActX` / `homXOutAct` (which do carry `#`) the
  declared coefficient also **changed which statistic was computed**.
* **Repair.** Theta is carried in `initialValue` throughout: every effect
  branch (including the static estimation path) writes the coefficient with
  `setEffect(initialValue = )`, and `get_theta_matrix()` reads
  `initialValue`. `setEffect(parameter = )` is reserved for genuine internal
  parameters, requested explicitly with a new `internal_parameter` key in a
  structure-model effect entry (e.g. `cycle4` with `internal_parameter = 2`
  for the square-root form), so a coefficient and an internal parameter cannot
  be confused at the call site. The public `parameter` key keeps meaning the
  coefficient. Verified behaviorally in
  `tests/testthat/test-theta-storage.R`: pre-repair, simulations with `cycle4`
  at -2 and +2 were bit-identical; post-repair the 4-cycle statistic responds
  monotonically (60 / 688 / 2559 at theta -2 / 0 / +2 on the seeded fixture).
* **Invalidated prior results.** Any *simulation* that declared a `cycle4`,
  `XWX` or `X` coefficient and expected it to matter -- including every
  `saomnk_model(influence_matrix = , influence_weight = )` run, whose
  influence weight silently simulated at 0 (the theta-shock and theta-ramp
  paths, which write the theta matrix directly, were unaffected in their
  shocked/ramped segments). *Empirical estimation* through plain
  `includeEffects()` + `siena07()` is unaffected: there `parm` stays at its
  default and theta is estimated.

Event-level statistics on the ministep chain, behavioral repertoires, and a
larger time-varying influence-matrix ladder. Motivated by a design that needs to
compare a fitted SAOM's *latent* event sequence against an *observed* event log,
which is possible whenever both are recorded (a code repository gives a commit
log and a repository state at every release tag).

## Ministep-chain event statistics

* **`searchnet_chain_stats()`** computes, for every tie-change event, the four
  attention micro-mechanism statistics of Tonellato, Tasselli, Conaldi, Lerner
  and Lomi (2024, *Organization Science* 35(2): 496-524) -- focusing,
  reinforcing, mixing and clustering -- each evaluated on the network state
  immediately *before* the event. It accepts either a simulated environment,
  whose latent chain it reads, or an observed event log, and returns identical
  columns for both so the two can be compared without reshaping.

  The clustering statistic is the number of bipartite four-cycles the event
  would close, which is the same quantity RSiena's `cycle4` targets. It is
  computed in `O(MN)` per event from one row of the co-membership matrix rather
  than by forming `BB'B`. The arithmetic is cross-checked against an independent
  brute-force transcription of the definition over random and adversarial
  fixtures in `tests/testthat/test-chain-stats.R`.

* **`searchnet_chain_from_fit()`** is the supported route from an *empirical*
  fit to the many chains a comparison needs: it pulls the latent ministep chains
  out of a `sienaFit` estimated with `returnChains = TRUE`, one chain per
  (phase-3 run, period), each replayed from the observed wave at the start of
  its period.

  Two things had to be got right. Fields are read by **declared index**: a
  ministep declares 13 elements of which 10 and 11 are zero-length, so
  `unlist()` silently shifts everything after position 9 by two, and the
  stability flag in particular is misread. And each period restarts from its own
  observed wave, so `chain_id` is unique per (run, period) rather than per run --
  pooling two periods would concatenate incomparable event histories.

  **A correction to this file's own earlier documentation.** Phase-3 chains under
  method-of-moments are *not* conditioned on the observed endpoints; each run
  simulates forward from the period's starting wave and does not arrive at its
  end state (verified: replaying one chain from wave 1 left 21 cells differing
  from the observed wave 2 on a 12 x 8 panel). Endpoint conditioning is a
  property of likelihood-based augmented chains. This makes the comparison
  stronger rather than weaker -- only the starting state is pinned, so an
  event-ordering statistic is a genuine forward prediction.

* **`searchnet_chain_stats()` refuses a `SaomNkRSienaBiEnv` whose `$chain_stats`
  concatenates several phase-3 runs.** That frame replays all runs cumulatively
  from the initial matrix, but RSiena restarts each run from the observed wave,
  so the concatenation is a sequence of independent draws replayed as though
  sequential. Demonstrated with one tie at `density = -8`: a genuine path deletes
  it once and never recreates it, while the composite frame toggled the same
  dyad eight times and ended still holding it. The inflation falls hardest on
  `focusing`. Use `searchnet_chain_from_fit()` instead.

## The null arm, without which coverage means nothing

Coverage of an observed log by a fitted model's chains is uninformative unless a
null model *fails* the same test. The evidential quantity is the gap.

* **`searchnet_chain_null_model()`** fits the rate-and-density-only arm on the
  same data with chains returned, switching off every other effect explicitly
  rather than trusting a default, and using `cond = FALSE` because conditional
  estimation derives the rate parameters rather than estimating them.

* **`searchnet_chain_gap()`** compares focal and null against the same log on
  identical terms and returns a per-statistic verdict. Only `informative` (null
  fails, focal covers) supports a sufficiency claim. `undiscriminating` means
  rate and density alone reproduce the statistic, so the focal model reproducing
  it is not evidence -- report it, do not count it.

* **`searchnet_chain_calibrate()`** is a leave-one-out size check on the test
  itself: each chain is held out as a pseudo-observed log, so the null is true
  by construction and rejection should sit near nominal. It needs no
  re-estimation, and a test that over-rejects on its own data cannot support a
  claim about anyone else's.

  Its first run earned its keep. Rejection sat at 0.04-0.08 against a nominal
  0.05 on a synthetic panel, but `focusing` came back with a Kolmogorov-Smirnov
  p of 0.0015 -- caused by ties, not miscalibration: the statistic was **zero
  for 92.9 per cent of events**, giving 14 distinct p-values out of 25. `ks_p`
  is now withheld when the p-values are too tied for a continuous reference to
  mean anything, and `zero_frac`, `distinct_p` and `ks_valid` are reported so
  the reason is visible. The scope condition is worth stating plainly:
  `focusing` is the statistic furthest from what method of moments targets and
  therefore the natural one to lead a sufficiency claim on, and also the one
  that goes degenerate when there is little repeat attention to count.

* **Per-statistic scaling in the normalization.** These are running counters but
  they do not all grow at the same rate: `focusing`, `reinforcing` and
  `activity` scale with the event count, while `mixing` is their product and
  scales with its square. Dividing everything by `n` left `mixing` still scaling
  with `n`. `clustering` is normalized at `n^1` as an acknowledged
  approximation -- its growth depends on the density trajectory and is not a
  clean power, which is why the length-ratio warning exists.

* **`searchnet_chain_compare()`** tests whether the distribution of those
  statistics across simulated chains covers the values computed on an observed
  log. Two guards are deliberate and load-bearing:

  - It **refuses a single chain**. The ministep chain is a draw from a
    distribution over sequences consistent with the observed panel endpoints,
    not a reconstruction of what happened, so one chain is a sample of size one
    and licenses no comparison.
  - Passing `fitted_effects` turns on a **circularity guard**. `cycle4` *is* the
    clustering statistic and `inPop` is monotone in reinforcing; a model carrying
    those effects has been fitted toward the quantity being tested, and
    reproducing it is not a free prediction. Affected rows are flagged and a
    warning is raised.

## Behavioral repertoires

* **`searchnet_repertoire()`** partitions actors into behavior types from the
  profile of their realized moves. This is a property of what actors *did*, as
  distinct from `sienaRI`'s decomposition of effect importance in the fitted
  model at an actor's position; the two are different objects and the package no
  longer needs the second to provide the first. `k` is chosen by maximum average
  silhouette width when not supplied, and the full criterion path is returned so
  the choice is auditable. Actors below the event threshold are reported in
  `$dropped`, never silently discarded.

* **`searchnet_repertoire_null()`** permutes actor labels across events and
  re-clusters, holding the event set, the statistics and the algorithm fixed and
  breaking only the actor-to-behavior association. k-means returns k clusters
  whether or not there is structure; this is how you find out which.

* **`searchnet_repertoire_ri()`** is an optional bridge to `RSiena::sienaRI()`.
  If the call fails it reports the failure and names the cause rather than
  substituting a degraded quantity. Whether `sienaRI` accepts a two-mode
  dependent variable is a property of the installed RSiena; a capability the
  software does not offer is a non-implementation and says nothing about the
  world.

## Influence matrices

* **Time-varying influence-matrix slots extended from 4 to 20**, matching the
  static `component_<k>_coDyadCovar` ladder. A multi-W horserace enters each
  coupling as its own `XWX` term and four was below what such a design needs.
  These are R6 public fields and must be declared: the engine assigns by name
  and R6 errors on assignment to an undeclared field rather than creating it.

* `saomnk_model()` now **fails loudly** when `influence_matrices` or
  `influence_arrays` exceeds the declared ladder (`.SEARCHNET_MAX_W_SLOTS`),
  naming the count and the ceiling, instead of erroring deep inside a run with a
  message that does not identify the cause.

## Packaging

* `stats`, `utils` and `grDevices` were imported in `NAMESPACE` but absent from
  `DESCRIPTION`'s `Imports:` field -- an `R CMD check` failure that would surface
  at JSS review. Added, along with `kmeans` and `dist` to the `stats` import.

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
  period counts, behavior DV presence), `print.saomnk_shock()`,
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

* `saomnk_empirical_epistasis()` is deprecated in favor of
  `saomnk_empirical_influence()`; it returns an influence-matrix estimate
  built from realized co-holding, not epistasis. Same arguments, same return;
  the old name warns once per session and then calls the new function.

* `epistatic_int_mat` on `saomnk_plot_bipartite_ring_markets()`,
  `saomnk_plot_bipartite_ring_markets_animation()`, their R6 method twins and
  `get_component_groups_list()` is deprecated in favor of `influence_matrix`.
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
  network--behavior coevolution landed.** `helper-setup.R` hand-listed
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
  network--behavior coevolution.** A structure model may now carry a
  `dv_behavior` block, making an actor-level attribute (performance,
  aspiration, capability) a second dependent variable that evolves jointly with
  the bipartite network instead of sitting fixed as a covariate. Both directions
  are live: the network shapes the behavior through influence effects, and the
  behavior shapes the network through selection effects declared on
  `dv_bipartite` with `interaction1` pointing at the behavior DV.

## What RSiena can and cannot do here, stated plainly

* **RSiena 1.5.0 does support bipartite + behavior coevolution.** This was
  verified against a live `getEffects()` object, not recalled. `sienaDataCreate()`
  accepts both dependent variables and returns a populated effect set for the
  behavior. Unlike the K_CA case below, this is a real EFFECT capability and
  not merely a statistic: the behavior enters the simulated evaluation function
  and moves.

* **The one-mode influence effects `avAlt`, `totAlt`, `avSim` and `totSim` are
  NOT available for a behavior attached to a bipartite network, and no amount
  of R-level work can add them.** In a bipartite network ego's direct alters are
  *components*, and components have no behavior to average. This is a property
  of the model, not a gap in RSiena or in searchnet.

  RSiena's substitutes are the distance-2 family, where two actors are
  neighbours when they hold a component in common: `avInAltDist2`,
  `totInAltDist2`, `avTInAltDist2`, `totAInAltDist2`, `avInSimDist2`,
  `totInSimDist2`. **`avInSimDist2` is the bipartite counterpart of `avSim`**,
  and is what a caller reaching for "imitation" or "social influence" wants.
  Also available on the behavior DV: `linear`, `quad`, `constant`,
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

* **A behavior DV changes RSiena's estimation mode, and therefore what a row of
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
  a "run" is no longer a ministep. `saomnk_get_behavior()` reports behavior once
  per run; per-ministep behavior changes are in `env$chain_stats`, in the
  `beh_difference` column of rows whose `dv_varname` is the behavior DV.

* **An undeclared basic rate defaults to 1.0, with a message.** RSiena's `parm`
  column defaults to 0 for basic rates, and searchnet reads `parm` as theta. A
  rate of exactly 0 freezes that dependent variable for the entire simulation.
  That is never an intended specification, so it is substituted and announced
  rather than silently simulated.

* **The utility and K-4 decompositions cover the bipartite evaluation function
  only.** `linear`, `quad`, `avInSimDist2` and the rest are statistics of the
  behavior, not of the bipartite matrix, and have no per-actor decomposition on
  that path. They are excluded from `actor_stats_df` rather than fabricated.
  Extending the decomposition to the behavior evaluation function is a separate
  piece of work.

## Bug fixes / hardening

* `search_rsiena_process_ministep_chain()` and `get_chain_stats_list()` now skip
  behavior ministeps when reconstructing the bipartite state trajectory. A
  behavior ministep's `id_to` column carries a behavior value, not a component
  id, so toggling on it would have silently corrupted every downstream network
  statistic. There is a test asserting the bipartite matrix never changes on a
  behavior ministep.

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
  `options(saomnk.loader.strict = TRUE)` restores fail-fast behavior;
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
  filter now removes only the basic `Rate` parameter. This is behavior-preserving for any
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
  the two actor groups differ by a factor of ~12.6 in realized tie changes, against ~1.5 at
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
