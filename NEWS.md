# searchnet 0.12.5

Released 2026-10-08. Documentation only; no code change since 0.12.2. README hero figure: the influence matrix and the start and end networks are stacked down the left, and the {K}-4 panel takes the right two columns at full height, so the degree trajectories have about twice the vertical range.

# searchnet 0.12.4

Released 2026-10-08. Documentation only; no code change since 0.12.2. The README no longer quotes mean K_CC values of 13.95 / 16.40 / 20.00 under varying W (withdrawn by the 2026-10-06 audit of replayed trajectories); the architectures figure reports current values. First public release since 0.12.1, so it carries 0.12.2 and 0.12.3.

# searchnet 0.12.3

Released 2026-10-08. Documentation only; no code change since 0.12.2. README figures: the hero shows the network at the start and at the end of the run on one shared layout (ties formed, kept and dropped marked) above the {K}-4 panel; the architectures figure adds a row with signed real weights on the same four patterns, beside conventional NK's nonnegative interaction pattern, with end-of-run ties and mean K_CC for each.

# searchnet 0.12.2

Released 2026-10-08. The {K} dimensions each effect reads and moves (`searchnet_effect_dimensions()`, `searchnet_classify_effect()`, `inst/rosetta/K_DIMENSIONS.md`), on the shared {K} specification. Reported K_AA and K_CC are now partner counts that exclude the node (1 lower for every non-isolated node); simulated networks, statistics and utilities are unchanged. Suite: 65 files, 924 tests, 6893 expectations, 0 failures, 0 errors, 9 skips.

## Paper and registry aligned with the {K} specification (2026-10-08)

* JSS paper: `tab:effects` names constructs (scope, crowding, complementarity) and gives each statistic as actor i's s_ik(B) with its reads and moves; the glossary reads inPop as crowding (agglomeration when positive) and cycle4 as contact (repeated overlap); `eq:choice` includes the no-change option; the parameter-mapping table follows `.bridge_crosswalk()`; covariate centering and the imitation term are defined as in RSiena 1.5.0. Every objective term checked against the RSiena manual.
* Rosetta: `k-scope` is titled Expansiveness; `k-sociality` reads and moves Sociality; the objective figure prints theta_k s_ik.


## {K} degrees and statistics aligned with the {K} specification (2026-10-08)

Reported K_AA and K_CC values change; simulated networks, statistics and
utilities do not (except the post hoc `inPopX` column, below).

* **K_AA and K_CC now exclude the node itself.** `saomnk_get_degrees()` (and
  `env$K_AA_df`, `env$K_CC_df`, `get_K4_df()`, the degree plots and exports
  built on them) counted the node in its own projection degree, so every
  non-isolated actor's K_AA and every held component's K_CC was 1 too large.
  They are now what the {K} specification and `inst/rosetta/K_DIMENSIONS.md`
  define, and what `.rosetta_k_summary()` and the rest of the package already
  computed: K_AA(i) = number of OTHER actors sharing at least one component
  with i, K_CC(j) = number of OTHER components co-held with j. **Values from
  earlier versions are lower by 1 for every non-isolated node** (unchanged for
  isolates); an actor holding only components nobody else holds now reports
  K_AA = 0, not 1. Figures and stated means of K_AA or K_CC from earlier
  versions should be regenerated.
* The NEW/OLD component subsets (`K_CA_NEW_df`, `K_CC_NEW_df`, `K_CA_OLD_df`,
  `K_CC_OLD_df`, `get_K4_df(type = "new" / "old")`) had K_CA and K_CC swapped:
  the "K_CA" frame held the component-projection degree and the "K_CC" frame
  the column sum. Fixed (and K_CC there is exclusive, as above). `K_CC_df`
  now carries its NEW/OLD `strategy` column; the assignment had been written
  to `K_CA_df` instead.
* A `density` effect on a bipartite dependent variable is RSiena's `density`
  (it always was when the effects table had a density row, which RSiena 1.5.0
  provides for bipartite variables). The fallback that silently substituted
  `outAct` (a different statistic) when no density row existed is replaced by
  an informative error.
* `inPopX`'s post hoc statistic column (stats and utility frames) recycled the
  N-vector of holder sums column-major (`rowSums(B * w)`), so each actor was
  weighted by the wrong components. It is now RSiena 1.5.0's two-mode form,
  `sum_j b_ij sum_h b_hj v~_h` (ego counted, covariate centered as declared),
  pinned to `siena07()` targets. RSiena 1.5.0's own two-mode inPopX target is
  not deterministic (identical calls sometimes omit the last component's term);
  the test records both values. `searchnet_effect_dimensions()` now classifies
  inPopX (reads Popularity; moves attribute-weighted Expansiveness and
  Sociality strength), and `inst/rosetta/effect_dimensions.csv` is regenerated.
* `saomnk_model()`: an `altX`, `altSqX` or `outActX` entry in `strategies`
  (actor covariates) is now an error pointing to `component_covariates`; for a
  bipartite dependent variable these effects need a component covariate. The
  documentation no longer lists `altX` under `strategies`. The `popularity`
  argument keeps its name but is documented as what `inPop` is read as:
  crowding (negative weight) or agglomeration (positive).
* Labels: utility panels and model printouts name `inPop` "crowding (inPop)"
  and `XWX` "complementarity (XWX)" (constructs, per the {K} specification's
  vocabulary), replacing "popularity (inPop)", "influence (XWX)" and
  "epistasis (XWX)".
* `saomnk_coholder_similarity()` documentation: it is not the CD4 engine's
  imitation statistic. Per component it equals the engine's `simPerfCand` add
  term only up to a per-decision constant (the centering sets differ), and its
  change on a flip never equals that term. It reads Popularity (K_CA).

## The {K} dimensions each effect reads and moves (2026-10-08)

No change to simulated results.

* `searchnet_effect_dimensions()` (new): every effect gets two fields. `reads` is the {K} dimension its change statistic depends on, derived by a walk-dependency test on random networks (who the other holders are -> Popularity; else other actors' holdings -> Sociality; else W -> Epistasis; else the actor's own row or attributes -> Expansiveness); `moves` is the dimension of its target statistic, by an identity fitted on random networks. Both are computed from the engine's own statistic. `rule = "degree"` gives a documented alternative reads rule.
* `searchnet_classify_effect()` (new): classifies a user's own statistic by the same rules.
* `inst/rosetta/K_DIMENSIONS.md` (new): the four dimensions (Expansiveness K_AC, Popularity K_CA, Sociality K_AA, Epistasis K_CC), degrees versus strengths (K_AA and K_CC are distinct-partner counts; the coupling identities hold for Sociality and Epistasis strengths), the two fields, the tie convention and the two limits, written once for the package, the README and the papers. `inst/rosetta/effect_dimensions.csv` (new, from `tools/make_effect_dimensions_csv.R`) ships the derived table.
* Rosetta: every class in `classes.yaml` carries `reads` and `moves`; `k_channel` stays as a deprecated alias of `moves`, so scope now moves K_CC and imitation K_AA. The crowding class is labeled "crowding" (id unchanged). `rosetta_validate()` errors (E9) on a class missing either field; `rosetta_export_json()`, `rosetta_classes()`, `rosetta_register_class()` and translation terms carry both. `rosetta_plot()` gains `view = c("moves", "reads", "both")`; "both" draws two badge rows, reads above moves.
* Plot strips use the display names and name the projection degrees as partner counts ("Sociality: number of actors sharing a component"); K_CC is "Epistasis", never "coupling" (terminology rule); the black line is labeled "mean degree". README hero, shock and objective figures and the paper's Figure 1 and {K}-system figures regenerated.
* The engine's `inPopX` column multiplies `B` by its weight vector with column-major recycling, so it is not the statistic its comment states; it is left unclassified, and a test records the state.
* Paper: tab:k4 uses the display names; NK's K is corrected to the W-degree (not K_CC); the glossary table has "{K} dimension read" and "Moves (target statistic)" columns; the class-colored objective shows both fields where they differ; 23 printed tildes before cross-references fixed.

# searchnet 0.12.1

Released 2026-10-08. Documentation only; no code change. The README hero
figure's legend now uses the node shapes (actors circles, components squares).
The JSS submission bundle and web paper are rebuilt on 0.12.0's code.

# searchnet 0.12.0

Released 2026-10-08. No change to simulated results. The translation registry
(Rosetta) is published: published NK-family models and theory constructs
stated in SAOM-NK terms, with functions to generate, translate, compare, plot
and export them. Formal results are labeled Properties 1-7 (two-layer
integrability is Property 6, the creation/endowment boundary Property 7).
Plots share one ggplot2 theme and palette (`theme_searchnet()`), with
reader-friendly titles and an optional `annotate` argument. Suite: 64 files,
902 tests, 5482 expectations, 0 failures, 0 errors, 5 skips.

## Plots that say what to look at (2026-10-08)

The main plots follow the design of the JSS paper's Figure 1: a title that states what the run shows (computed from the run), a plain-language subtitle saying how to read the figure, at most one or two notes with arrows on the data, the model weights in a one-line caption instead of a multi-line title, and one visual grammar (actors orange, components blue, a shock vermillion, context grey; the four channels labeled `K_AC` scope, `K_CA` popularity, `K_AA` sociality, `K_CC` coupling). No simulated result changes.

* `saomnk_plot_k4()`, `saomnk_plot_degree_4panel()`, `saomnk_plot_actor_degrees()`, `saomnk_plot_component_degrees()` and the R6 methods behind them share one builder: one strip per channel with its plain name, no legend when actors or components form a single group, a dashed vermillion line and light wash at a shock (replacing the grey rectangles and per-panel segment labels), x axis "Ministep (one decision opportunity)". The title is computed: "All four {K} degrees rise, then level off", "The shock at ministep t lowers all four {K} degrees", and so on. The actor and component versions use free y scales.
* `saomnk_plot_utility()`, `saomnk_plot_utility_contributions()`, `saomnk_plot_utility_contributions_basic()` and the R6 methods: strips with plain effect names (for example "popularity (inPop): ties to popular components"), one color per effect when there are no strategies, and a title naming the largest term at the end of the run.
* `searchnet_causal_plot()`: the DID plot is drawn by searchnet instead of `did::ggdid()` (whose axis labels overprinted at more than a few dozen event times); the title reports the simple ATT and SE, a band shows the simultaneous confidence interval. Synthetic control lines are labeled at their ends; the RD estimate moves from the plot body to the title.
* `plot.searchnet_ergodicity()`: arms labeled on their starting densities instead of a legend; titles state the computed verdict; overprinted labels moved.
* `saomnk_plot_snapshots()` and the R6 `plot_snapshots()` (which now draws the same panels): plain panel titles ("Who holds what", "Actors linked by a shared component", "Components sharing actors"), components blue rather than bluish green, projection actors in their strategy color sized by partners (the viridis eigenvector-centrality legend is gone), a note in an empty heatmap. `palette = "legacy"` is unchanged.
* New optional argument `annotate = TRUE` on all of the above: reading guides on the data (the "mean" line label, the level a series settles at, the shock and how far the first series moves, the pre-trend check and average effect in the DID plot, the run length at which the ergodicity arms meet). `annotate = FALSE` draws the same figure without them. Every other argument and return value is unchanged.
* `theme_searchnet(grid = FALSE)` and `axes = FALSE` now remove the major grid too (an explicit `panel.grid.major` had overridden the blank parent).
* The basin plots' subtitles say how to read them instead of asserting a result whatever the data.
* Paper and README: `paper/replication/make_k_system_figures.R` redrawn in the same grammar (direct labels, computed titles) and now loads the source tree when run from it; README hero, shock and DID figures regenerated.

## One plot style (2026-10-08)

* New `theme_searchnet()`, `searchnet_palette()` and `scale_color_searchnet()` / `scale_fill_searchnet()`: the ggplot2 theme and Okabe-Ito categorical, white-to-blue sequential and blue-white-vermillion diverging (white at zero) scales behind the README and JSS figures. Every package plot now uses them in place of `theme_bw()` / `theme_minimal()`, RColorBrewer, default hues and ad hoc red/blue; legends sit at the bottom unless a function sets its own position.
* Converted from base graphics to ggplot2: `plot.searchnet_ergodicity()` (still returns `x` invisibly; the figure is `attr(x, "plot")`, and `draw = FALSE` skips printing), the stability traces of `search_rsiena_plot_stability()` (returned as `stability_plot`) and the sienaGOF figures of `add_gof_to_rsiena_shocks()`. The base-graphics figures in the basins, Blume and Brock-Durlauf vignettes are ggplot2 too.
* Kept: `saomnk_plot_phase_space_3d()` stays a plotly 3D figure (ggplot2 has no 3D coordinates), restyled to the palette; `saomnk_plot_geo_network()` keeps its dark map, whose colors are arguments; `saomnk_plot_snapshots(palette = "legacy")` still restores the old colors.
* Shock and period windows are shaded neutral grey, not orange, so they never read as a category. README hero, shock and DID figures regenerated.

## Results relabeled as Properties 1-7 (2026-10-08)

* The package's formal results are now numbered as Properties, matching the JSS paper: former Theorems 1-6 are Properties 1-6 (Property 6 is two-layer integrability, `PROOF_TABLE.md` Part M) and the creation/endowment boundary (formerly Proposition 4, Part N) is Property 7. Lemmas, Definitions, Corollaries and Propositions 1-3 keep their numbers. Updated: `inst/proofs/` (proof table, tutorial, equivalence proof, online appendix master), vignettes, help pages, README and Rosetta entries. Lean declaration names are unchanged. Release notes below keep the numbering of their time.

## Translation registry published (2026-10-08)

* The translation registry ("Rosetta") now ships: published models of search and theory constructs stated in SAOM-NK terms, one YAML entry per model in `inst/rosetta/entries/`, with an effect-class registry (`classes.yaml`; complementarity, scope, crowding, contact, imitation, covariates; each with its {K} channel, color token and Lean statistic) and an entry schema (`schema.yaml`).
* Functions: `rosetta_classes()`, `rosetta_register_class()`, `rosetta_reset_classes()`, `rosetta_entries()`, `rosetta_entry()`, `rosetta_validate()` (errors and advisory measurements kept apart), `rosetta_model()` and `rosetta_run()` (an entry's restriction as a runnable specification; one-actor restrictions run as `nk_walk()`), `rosetta_saomnk_model()`, `rosetta_r_code()`, `rosetta_translate()`, `rosetta_compare()`, `rosetta_plot()`, `rosetta_new_entry()`, `rosetta_lean()`, `rosetta_export_json()`, `rosetta_model_to_json()`, `rosetta_model_from_json()`. New vignette `saomnk-rosetta`.
* 23 entries: NK adaptive walk, NK population search, Rivkin imitation, standard search with competition (Lenox, Rockart and Lewin 2006), strategic search (Giustiziero, Kaul and Martignoni 2026, Strategic Management Journal: the zero-noise single-flip limit with linearized competition, theta_density = c(M + 1)/N and theta_inPop = -2c/N per holder), logit QRE, Blume logit dynamics, Brock-Durlauf, plain two-mode SAOM, and construct translations (density dependence, exploration and local search, imitation and design under complexity, {K} epistasis, scope and sociality, market entry, mimetic isomorphism, modes and time horizons of competition, multimarket contact, perceived rivalry (not carried), search depth and breadth, momentum, structural inertia). Every entry is AI-drafted and marked unreviewed; citations were checked against Crossref.
* NAMESPACE exports the functions by name (`export()`) and registers the print methods with `S3method()`; the `exportPattern()` line and the `.onLoad()` late-registration hook that existed only to let builds omit the registry are gone.
* Fixes carried over from the registry's own notes (0.11.1): YAML 1.1 boolean keys (`N:`) are kept as text and reported by `rosetta_validate()` as E8; `rosetta_model()` returns a binary W with a zero diagonal; `rosetta_plot()` "K per row" bars carry their counts; the Brock-Durlauf entry states the kappa_N ministep linearization near m = 1/2.

# searchnet 0.11.2

Released 2026-10-08. No change to simulated results. `saomnk_plot_snapshots()`
gains a colorblind-safe default palette (`palette = "legacy"` keeps the old
colors), returns its panels for restyling, and now colors each actor by its own
strategy. README figures generated by the package (`tools/make_readme_figures.R`).
JSS submission build produces `code.R` / `code.html`. Suite: 62 files, 881 tests,
5291 expectations, 0 failures, 0 errors, 4 skips.

* `saomnk_plot_snapshots()` gains `palette` (default `"okabe-ito"`, colorblind safe: Okabe-Ito node colors, viridis centrality, white-to-blue heatmap; `"legacy"` keeps the old red-green colors), `node_colors` (named overrides by strategy level or `"new"`/`"old"`) and `draw`, and now returns the panels invisibly as ggplot objects plus the arranged grob, so they can be restyled. Existing calls are unchanged apart from the colors; each actor now takes its own strategy's color (the old code recycled the strategy colors over actor index). README figures: the W heatmaps in `readme-hero.png` and `readme-architectures.png` blank the diagonal, which XWX never uses (it sums over j != h).

# searchnet 0.11.1

Released 2026-10-08. No change to simulated results: every chain output is
identical to 0.11.0 (golden test). Chain processing is 3.3x faster (overhead
over raw RSiena 794% to 130%). Failure paths that were silently turned into
NA, a warning or a fallback now stop with an informative message or report
counted failures; the classroom interaction bonus is fixed. Documentation:
Properties 2 and 3 corrected where new tests showed the statements false, the
Brock-Durlauf correspondence stated for the RSiena ministep (kappa_N), and the
theory vignette's statistics table corrected. Suite: 61 files, 878 tests, 5271
expectations, 0 failures, 0 errors, 4 skips.

## Faster ministep-chain processing, identical results (2026-10-07)

* `search_rsiena()` / `saomnk_run()` chain processing no longer rebuilds the effects
  table and one data.frame per table at every ministep. `saomnk_run()` at M 4, N 6,
  30 per actor: median 0.915 s to 0.275 s (overhead over raw RSiena 794% to 130%);
  6x to 10x faster at M 6 to 30, N 8 to 50. Every output is unchanged, checked with
  `identical()` by the new `tests/testthat/test-chain-golden.R`.
## Silent failures: the remaining paths (2026-10-07)

Each now stops with an informative message, or returns partial results with
an explicit failures record and a warning that states counts.

* **Effects are never dropped.** A declared effect that cannot be included
  (a covariate effect with no covariate, a failed `RateX`/`outRate*`,
  covariate, `XWX` or `X` effect) now stops. These branches warned and ran
  the model without the effect. `options(saomnk.skip_missing_effects = TRUE)`
  restores skipping, as it already did for the generic fallback.
* **Experiments runner.** Failed runs are recorded in `$failures`
  (run, seed, stage, message) with one warning giving the count; if every run
  fails it stops. They were warned about one by one and dropped.
* **`ising_hysteresis_sweep()`.** A failed replicate returned the *starting*
  matrix as its result. Failures are now excluded and listed in `$failures`,
  `n_ok` gives the replicates behind each step, a step with no successful
  replicate stops the sweep, and an undefined loop area is classified `NA`
  rather than `"reversible"`.
* **`verify_brock_durlauf_reduction()`.** New `n_ok` column and
  `attr(, "failures")`; one warning with counts replaces per-replicate
  warnings; stops if every replicate fails. A failed saomnk-inPop analytical
  reference is warned about instead of becoming `NA` silently.
* **DiD helpers.** `test_shocks_new_components()` records skipped and failed
  pairs (`attr(, "failures")`) and warns; the quiet before/after fallback is
  gone, and it stops if no pair is estimated. `analyze_exploration_risk_shocks()`
  stops on missing pre-shock data, a missing arm, or a failed social-logic
  step, and records failed plots or multiperiod analysis in `$failures`.
  `analyze_simple_exploration_shocks()` and `_fixed()` stop instead of
  plotting an `NA` estimate as "DiD = 0.0000"; a failed `did`-package estimate
  is returned in `did_error` and warned. `diagnose_did_detailed()` returns
  `att_gt_tests`. `check_exploration_data_availability()` returns
  `metrics_error`. `did_shock_analysis()` adds `parallel_trends_status` and
  warns when the requested test cannot be computed.
* **Standard errors and summaries.** `saom_to_saomnk()` and
  `saomnk_extract_estimates_tergm()` warn when standard errors cannot be read
  (they became `NA` silently). The counterfactual bridge's paired summary
  stops on a missing replication. `solve_mean_field()` no longer drops a
  failed bracket or falls back to `m* = 0`.
* **Classroom leaderboard.** The epistasis bonus read `model$influence_matrix`
  and `model$effects`, which a `saomnk_model` does not have, so it was always
  0. It now reads W and its weight from the model's `XWX` entry. Classroom
  scores change.
* **Tests and vignettes.** `run_tiny_sim()` lets errors fail the test instead
  of converting them to skips. The causal-inference vignette's synthetic
  control and RD chunks no longer turn errors into hidden messages.

# searchnet 0.11.0

Released 2026-10-07. **Breaking: every simulated number changes.**
`search_rsiena()` now simulates genuine state-carrying paths; v0.10.0 and
earlier replayed independent one-ministep draws as if they were a path, so
simulated trajectories and end states from those versions must be re-run.
Monadic covariates now default to `centered = FALSE`. Also in this release:
optional formal verification with Lean 4 (`inst/lean`, `lean_*()`),
covariate and structural statistics pinned to RSiena's targets, hashed seed
streams, run provenance, readback and opportunity diagnostics, and fixes for
shocks that were silently ignored, multiwave arguments, the DID design, and a
classroom game that reset every round. Suite: 56 files, 833 tests, 4810
expectations, 0 failures, 0 errors, 3 skips.

## Silent failures on four paths (2026-10-07)

* `theta_shocks`: a shock whose `effect_level` matches no theta column, or that lacks a
  numeric `parameter` (e.g. `new_parameter`), now stops with the available columns. Both
  were skipped, so the run was unshocked. A single-entry schedule warns that it covers the
  whole run. The simulation vignette's shock is now applied.
* `search_rsiena_multiwave_run()` and `_extend()` also accept `iterations_per_actor` and
  `run_seed`, the `search_rsiena()` names (the vignette's call failed with "unused
  arguments").
* `saomnk_run()` gains `restart` (default `TRUE`). `searchnet_classroom_advance()` passes
  `FALSE`, so rounds continue from the current board instead of the initial matrix, and
  student moves are no longer erased. Its scheduled shocks now change the density (they
  wrote to a nonexistent field), and an AI-step error stops the round.
* The causal-inference vignette's DID chunks hid an error (3 never-treated actors; `did`
  needs 5). They now compare the shocked run with an unshocked comparison run and let
  errors through. `search_rsiena()` warns when chain statistics fail (was verbose-only).

## `search_rsiena()` simulates genuine paths; time replaces the ministep count (2026-10-07)

The fix pre-registered in `docs/PREREG_2026-10-06_state_carrying_simulation.md`, route (a).
**Breaking:** every simulated number changes. Intended to ship as 0.11.0 (the code and
documentation say "0.11.0"; DESCRIPTION is not yet bumped).

* **Time semantics.** A call simulates one unit of model time. `iterations_per_actor`
  (`steps_per_actor` in `saomnk_run()`, `iterations / M`, or `nrow(theta_matrix) / M`) is
  the basic rate summed over the run: the expected number of opportunities per actor at zero
  rate effects. The realized number of ministeps is random, and rate effects change it.
* **State is carried.** Each block of identical theta rows (a segment) is one
  unconditional RSiena period (`cond = FALSE`, `nsub = 0`, `n3 = 2`, run 1 kept), started
  from the previous segment's end network.
  * The engine stops if the replay of a segment's chain does not end at RSiena's own end
    network.
  * Ramps with more than `max_segments = 50` distinct rows are coarsened, with a message.
  * Segment seeds are drawn from `run_seed`, with no seed arithmetic.
* **What the environment now carries.**
  * `$chain_stats` gains `segment_id`.
  * `$bi_env_arr` is tagged `searchnet_path = "genuine"`.
  * `$path_segments` records each segment's rows, rate, seed and ministep count.
  * `$rsiena_model$chain`, `$sims` and `$thetaUsed` have one entry per segment.
  * The environment is left at RSiena's end state even with `process_chain = FALSE`.
  * The replay starts from the state the run actually started from, which fixes
    `restart = FALSE`.
  * `search_rsiena()` records `env$provenance`, including the segment seeds and the
    realized ministep count.
* **Shocks divide time.** `theta_shocks[[i]]$chain_step_ids` now index the path's
  ministeps, and the time-grid rows are kept as `theta_row_ids`.
* **Multiwave.** Each wave of `search_rsiena_multiwave_run()` (and `saomnk_monte_carlo()`)
  is one unconditional period from the previous wave's end, with basic rate
  `iterations / M`. `search_rsiena_multiwave_process_results()` replays each wave from its
  own start; before, it replayed the last wave's chain for every wave.
  `search_rsiena_multiwave_extend()` continues from the current state. The ignored
  arguments `returnDeps`, `returnChains`, `rsiena_phase2_nsub`, `rsiena_n2start_scale`,
  `dir_output` and `file_output` warn once per session.
* **Two dependent variables.** The behavior vector is carried across segments as well. The
  behavior basic rate keeps the ratio to the bipartite rate that the theta row declares.
* **Covariates are uncentered by default** (breaking). Monadic covariates (`coCovar`,
  `varCovar`) are created with `centered = FALSE` unless the declaration says
  `centered = TRUE`, and a message says so once per session. Dyadic covariates keep
  RSiena's default unless declared. The centering RSiena applied is recorded in
  `env$covariate_centering` and printed by `saomnk_summary()`.
* **Legacy route.** `search_rsiena(path = "legacy_replay")` reproduces the old behavior,
  centering included, for archived numbers only.
  * It warns on every call (class `searchnet_not_a_path`) and tags its array
    `"independent_draws"`.
  * Every path consumer refuses it with class `searchnet_not_a_path_error`:
    `search_rsiena_process_stats()`, `get_K4_df()`, `searchnet_chain_stats()`, the export
    functions, the K-4, snapshot, utility, degree, market, phase-space, shock and
    exploration plots, `saomnk_get_degrees()`, `saomnk_get_bipartite(step =)`,
    `searchnet_causal_panel()`, `searchnet_ergodicity_sweep()`,
    `searchnet_classroom_advance()` and the bridge's arm runner.
* **`searchnet_chain_stats()` accepts a multi-segment path.** It still refuses an untagged
  multi-run chain.
* **Degree-dependent rate effects are refused.** RSiena 1.5.0 corrupts the heap when it
  simulates a bipartite network unconditionally with `outRate`, `outRateInv` or
  `outRateLog` together with `inPop` or `XWX`; R then crashes. This was reproduced in plain
  RSiena. `search_rsiena()` stops with class `searchnet_unsupported_effect` instead.
* **Gates.**
  * `tests/testthat/test-path-not-replay.R` (G1-G4) now passes.
  * New `tests/testthat/test-path-gates.R` adds:
    * G1b, choice probabilities for density, inPop, XWX and egoX across two segments;
    * G5, an independent continuous-time oracle in `helper-saom-oracle.R`, 200
      replicates per arm;
    * G6, determinism and disjoint seeds;
    * G7, legacy refusal.
* **What must be regenerated.** Every vignette chunk, paper result, replication script and
  figure built on `search_rsiena()` trajectories. They are listed in an internal re-run
  list and have not been regenerated in this change.

## Known defect: `search_rsiena()` trajectories are not SAOM paths (2026-10-06)

**Status:** fixed 2026-10-07 (entry above). Found in a review of a downstream study and
re-verified on this version.

### How the replay works

* **The setup.** `search_rsiena()` runs `siena07(simOnly = TRUE)` with `cond` at its
  default (TRUE for one dependent variable), two identical waves, and `n3` equal to the step
  count.
* **What RSiena does with it.** With a conditional target distance of 0, every phase-3 run
  stops after one ministep, and RSiena starts every run from wave 1.
* **What searchnet then does.** `search_rsiena_process_ministep_chain()` replays those
  independent draws cumulatively from the initial matrix into `$bi_env_arr`, and leaves the
  environment in the replayed final state.

No simulated decision ever responds to the current state. On this version:

* Every RSiena end-of-run network lies within one toggle of the initial matrix (mean 0.97
  over 60 runs). The replayed state at the same points is 0 to 17 toggles away.
* Logged choice probabilities are reproduced from the initial matrix to 4.4e-16, and miss
  the replayed preceding state by up to 3.9 log-points.

### Why it went unnoticed

* **The replayed terminal state forgets theta.** Each cell ends at its initial value flipped
  by the parity of independent draws, so terminal density drifts toward 0.5 whatever the
  model is. At density -2, 0 and +2 the replay ends at 0.44, 0.49 and 0.57. Genuine
  unconditional periods end at 0.06, 0.51 and 0.94.
* **So "convergence" checks pass by construction.** Two arms from different starts
  "converge", and a target of 0.5 is "matched".
* **From an empty start, the influence matrix has no effect.** The `XWX` change statistic is
  zero for every candidate in an empty row, so W cannot affect any draw. Chains at `XWX` 0
  and 1.5 are identical under the same seed.

### Two related defects

* **Rate effects are zero-sum.** The total number of ministeps is fixed and the mover is
  drawn in proportion to rate, so a rate effect for one group takes opportunities from the
  other. At `RateX = 2` the total stayed at exactly 1000, as at `RateX = 0`, and the reference group fell to
  0.23 of its `RateX = 0` count.
* **Covariates are centered silently.** `coCovar()` is created without `centered`, so RSiena
  stores a 0/1 covariate as -0.5/+0.5. Under the fixed budget this cancels out of a rate
  effect. In continuous time it does not.

### The same design in other routes

* `search_rsiena_multiwave_run()`, and through it `saomnk_monte_carlo()`, moved at most two
  ties over three waves of 200 iterations.
* The two-DV route uses `cond = FALSE`, but still concatenates `n3` full periods, each
  started from wave 1.
* With `restart = FALSE`, the replay starts from `bipartite_matrix_init` rather than from the
  state the simulation actually started at.

### Affected and unaffected outputs

**Affected:** every output read from a run.
* `$bi_env_arr`, `$bi_env_changes` and the post-run `$bipartite_matrix`.
* `get_K4_df()` and the K-4 frames, `$actor_util_df` and `$actor_stats_df`.
* The snapshot, phase-space and utility plots, and the exports.
* Every wrapper: `saomnk_run()`, `saomnk_run_two_sided()`, `saomnk_monte_carlo()`,
  `verify_brock_durlauf_reduction()`, `searchnet_ergodicity_sweep()`,
  `run_calibrated_counterfactual()`, `run_counterfactual_with_uncertainty()`,
  `searchnet_classroom_advance()`, `SaoMNKexperiments$run_simulations()`, and the market
  entry/exit and Ising-hysteresis methods.

**Not affected:**
* Landscape enumeration and `nk_verify_reduction()`.
* Per-ministep choice probabilities from a given state: `compute_choice_probabilities()` and
  the game mode.
* Estimation with `siena07()`, and `searchnet_chain_from_fit()`.
* The analytic results.

### What this withdraws in the package's own documents

Every simulated trajectory, terminal state, convergence statement, counterfactual delta and
benchmark in the following documents is withdrawn. Nothing has been edited yet; a
claim-by-claim audit exists.

* **The JSS paper and online appendix.** This includes:
  * the architectural-shocks application: its rate separation, post-shock valleys and
    selection wedge;
  * the Levinthal illustration;
  * the ergodicity sweep ("within 0.01 by 240 iterations");
  * the shock K-4 break and the DID panel.
* **Proof-registry checks** F5, F7, I3, J1-J3, K1 and K2: their passes and their failures
  alike.
* **`PROOF_TABLE.md`:**
  * rows F3, F7, I1 and L8, as computational verifications;
  * row L15 outright.
* **The simulated illustrations in these vignettes:** Blume, Brock-Durlauf, theory,
  nk-validation, policy basins, calibration, causal-inference, experiments, introduction,
  PDW-workshop and simulation.

### Earlier entries this supersedes

* **0.9.2:** the ergodicity sweep's "measurement" of independence from initial conditions
  measured the replay's parity limit.
* **2026-09-15:** the I3 note that the simulated chain sits "farther still" from the exact
  law, cause undiagnosed. This defect is the cause.
* **0.9.0:** the refusal in `searchnet_chain_stats()` was right. It is now the model for
  every other consumer.

### Regression tests

`tests/testthat/test-path-not-replay.R` has four tests (gates G1-G4) that fail on this
version by design:
* the path ends at a network RSiena never simulated;
* one tie at `density = -8` is toggled 9 times, where a genuine path deletes it once;
* logged choice probabilities miss the path's own preceding states by 4.32 log-points;
* a rate effect leaves the total number of opportunities unchanged.

The suite reports these failures until the fix lands.

### The planned fix

The fix was pre-registered before any engine change.
* **One period per segment.** Each theta segment becomes one unconditional RSiena period
  (`cond = FALSE`), started from the previous segment's end state, with its chain replayed
  only within that period.
* **Steps become rates.** `iterations_per_actor` becomes the basic rate summed over the run,
  so rate effects change how many opportunities occur.
* **Centering is explicit.** Covariate centering is declared and recorded.
* **The old route is opt-in.** It survives only as an explicit legacy option, which warns,
  and every path consumer refuses its output.

Checked on RSiena 1.5.0: within one unconditional period, the replay reproduces RSiena's own
end network exactly, and reproduces every logged choice probability to 4.4e-16.

This is a semantic break, proposed as 0.11.0. Step arguments will mean expected
opportunities per actor, and the number of ministeps will be random.
## Formal verification with Lean 4 (2026-10-07)

* **New Lean library `SaomNK` in `inst/lean`** (Lean 4 + Mathlib, toolchain
  pinned): 273 machine-checked theorems and lemmas in 30 modules, with no
  `sorry`, no axiom and no `native_decide`. General building blocks for
  SAOM-NK models: configurations and flips; NK influence patterns
  (`IsKRegular`, decidable), NK fitness, the LSB/MSB power-key conventions and
  landscape tables; statistics (density, outAct, inPop, cycle4, XWX, ego and
  activity covariates, row-pair kernels) each with its exact potential; the
  algebra of exact potentials; Nash existence and flip stability as local
  maxima of the potential; Glauber, Metropolis and multinomial-logit choice
  with their zero-noise limits; Gibbs detailed and global balance for any
  reversible acceptance rule, with structural zeros (restricted chain) as the
  primary case; two-layer integrability; creation versus endowment; two-bloc
  mean-field equilibria and the Landau quartic; and a computable rational
  layer whose numbers are proved to be casts of the real-valued theory.
* **New R functions.** `lean_spec()` maps a model definition (a
  `SaomNkRSienaBiEnv`, an `nk_landscape`, or a list) to the library's
  statistics and lists unmapped effects (`lean_effect_map()`).
  `lean_export_model()` writes `Instance_<hash>.lean`, which instantiates the
  general theorems for the model and proves its numeric facts (K-regularity,
  landscape consistency, the index convention shared with R, utilities and
  potential at the current configuration, the number of single-flip-stable
  configurations) by kernel evaluation. `lean_check()` builds and audits
  axioms; `lean_setup()`, `lean_available()`, `lean_home()`,
  `lean_registry()`, `lean_declarations()` and the testthat expectation
  `expect_lean_theorem()` complete the set. `inst/lean/registry.yml` links
  declarations to proof-table steps, tests and R functions.
* Lean is optional: every `lean_*()` function skips with a message when no
  toolchain is found. New Suggests: gmp (exact numeric facts), processx,
  withr, yaml. New vignette `saomnk-lean`; new CI workflow
  `.github/workflows/lean.yml`.

## Engine hygiene: seed streams, provenance, readback and opportunity diagnostics (2026-10-07)

* **Seed streams that collided.** `searchnet_ergodicity_sweep()` derived each
  run's initial-draw seed as `seed + arm*10000 + rep*100 + iters` and its
  dynamics seed as `seed + arm*20000 + rep*100 + iters`, so arm 2's initial
  draw used exactly arm 1's dynamics seed. `verify_brock_durlauf_reduction()`
  and the hysteresis sweep fed one seed to both the initial draw and the run;
  `saomnk_run_two_sided()` used `seed + w` and `seed + 1000*w`, which meet at
  wave 1000; `saomnk_monte_carlo()` used `seed + r`, so base seeds 1 and 2
  shared all but one replication. All of these now derive their seeds from a
  new internal `.searchnet_seed(base, purpose, ...)`, a purpose-namespaced
  32-bit hash in base R. `tests/testthat/test-engine-hygiene.R` checks a grid
  of purposes x arms x replicates x run lengths for collisions and checks that
  a seeded run still reproduces. **Numeric outputs of these five functions
  change for a given seed**; results remain reproducible for a fixed seed.
  `run_calibrated_counterfactual()` gives both arms one seed by design
  (matched initialization) and is not changed here, although it also uses
  that seed for both the environment and the run.
* **Run provenance.** `saomnk_run()` and `saomnk_monte_carlo()` now store the
  searchnet, RSiena and R versions, the RNG kind, the seed actually used and
  the call in `env$provenance`; the sweep functions above attach the same
  record as a `"provenance"` attribute. New exported accessor
  `searchnet_provenance()`. Non-breaking: a new field and an attribute. Calls
  to the `search_rsiena()` method directly do not record provenance.
* **`fit_rsiena_shocks()` had no test, and its GOF step failed.**
  `add_gof_to_rsiena_shocks()` combined the per-effect `tconv` vector with
  `&&`, an error in R >= 4.3, so every `fit_rsiena_shocks(add_gof = TRUE)` on
  a model with more than one effect stopped. The convergence check is now
  `check_all` (the mangled key `checK_AAll` is kept as an alias), and the
  function restores the graphics `par()` it changes. `fit_rsiena_shocks()` also
  drops duplicate observation steps in short segments and stops with a clear
  message when a segment has fewer than two. Tested end to end.
* **`saomnk_run()` kept the `saomnk_shock` class** on the engine's shock
  entries (`as.list()` does not strip a class), so after a run or
  `fit_rsiena_shocks()` they printed through `print.saomnk_shock()`, which
  hides `chain_step_ids` and the fitted model. They are now plain lists.
* **New `searchnet_readback_check()`.** Detects an "outcome" that is the
  evaluation function read back: it regresses the outcome on the model's own
  statistics, reports R^2 and the recovered coefficients against the declared
  theta, and compares the outcome's K gradient with a random-portfolio null
  (portfolios drawn with no search, scored by the declared objective). The
  print method says plainly when the outcome is the objective. On a simulated
  environment the default outcome is the package's reported utility
  (`env$actor_util_df`), which it flags.
* **New `searchnet_opportunity_table()`.** Tabulates realized ministep
  opportunities and tie changes per group per arm from a run's chain, with
  each group's share of the arm's fixed ministep budget next to its equal-rate
  share, so rate-effect studies can see that the budget is zero-sum.
* **API compatibility.** `saomnk_shock(new_value = )`, a form an early README
  showed, is accepted as `parameter` with a once-per-session deprecation
  warning. `saomnk_shock(step = )` stops with a message naming `portion` and
  the two-shock construction, because an absolute step cannot be converted
  without the run length. The deprecated `epistasis_*` arguments of
  `saomnk_model()` now warn once per session per argument instead of on every
  call, and an error message that still pointed to `epistasis_matrices` now
  names `influence_matrices`.
* **K4 plotting helpers and `parm`: checked, no defect.** The K4 and multiwave
  plot titles read coefficients from the structure model's `parameter` key,
  never from the effects table's `parm` column. A test pins that the
  `saomnk_plot_k4()` title reports a declared `XWX` weight of 0.37 while
  RSiena's `parm` for that effect is 0.

## Covariate statistics pinned to RSiena, and the imitation statistic renamed (2026-10-04)

* **Six more actor statistics in `get_struct_mod_stats_mat_from_bi_mat()`
  disagreed with RSiena 1.5.0's `siena07()` targets, and `saom_to_saomnk()`
  marked five of their effects "exact".** `egoX`, `altX`, `outActX` and `X`
  read the raw covariate where RSiena reads it centered on its mean, which adds
  the mean times a degree term; `totInDist2` also counted ego among a
  component's holders; `simEgoInDist2` computed similarity to the mean of
  distance-2 alters in the actor projection, a different statistic. They now
  compute RSiena's definitions, listed at the top of
  `tests/testthat/test-structural-stats-vs-rsiena.R`. As on 2026-09-15, the
  matrix feeds the utility and K decompositions, not the simulation, which
  already handed these effects to RSiena; decompositions that included them
  were misreported.
* That test file now also pins `density`, `outAct` and `outActSqrt` (already
  correct) and the six above, registered through searchnet's own covariate
  slots, with negative controls for the old formulas. A further test fails if
  any "exact" crosswalk entry names a statistic the file does not pin; the
  crosswalk moved into the internal `.bridge_crosswalk()` so the test can read
  it. The bridge's descriptions of `totInDist2` and `simEgoInDist2` are
  corrected.
* **`saomnk_sim_ego_indist2()` is renamed `saomnk_coholder_similarity()`**, and
  the old name stays as a deprecated alias that warns once per session. The
  statistic is unchanged and is not RSiena's `simEgoInDist2`: it subtracts each
  actor's own mean over co-held components where RSiena subtracts one
  data-level constant, and a component with no co-holder contributes nothing
  where RSiena compares ego with the covariate mean. On 200 random states the
  two agreed in none (largest difference 8.09). Its documentation no longer
  says the centering follows RSiena, or that `simEgoInDist2` is one-mode only.
  `saomnk_env_imitation()` calls the new name.
* Not pinned: `inPopX`. RSiena 1.5.0 offers it for a bipartite dependent
  variable with an actor covariate only, and its target matched neither
  searchnet's statistic nor centered or ego-excluded variants of it. The code
  now says the column is not RSiena's effect.
* Known, not changed: the JSS paper still says `simEgoInDist2` is defined for
  one-mode networks only. A model that includes `simEgoInDist2` without a
  covariate stops in `saomnk_run()` with "Effect not found", because RSiena
  requires one.

## Structural statistics pinned to RSiena, and equilibrium claims restricted (2026-09-15)

* **Four hand-computed actor statistics in
  `get_struct_mod_stats_mat_from_bi_mat()` disagreed with RSiena 1.5.0's own
  `siena07()` targets.** `inPop` counted ego twice (`B %*% (colSums(B) + 1)`);
  `inPopSqrt` had no branch, printed "Effect not yet implemented" and left its
  column at 0; `cycle4` kept the diagonal of `BB'` and counted degenerate closed
  walks; `XWX` summed `B W B'` over every actor instead of within ego. They are
  now `sum_j x_ij x_+j`, `sum_j x_ij sqrt(x_+j)`,
  `(1/2) sum_{k != i} choose(ov_ik, 2)` and `sum_{j != h} x_ij x_ih w_hj`. The
  matrix feeds the post-hoc statistic and utility decompositions, not RSiena's
  simulation, so simulated chains are unchanged but decompositions that
  included these effects were misreported.
* New `tests/testthat/test-structural-stats-vs-rsiena.R` compares the actor sums
  with `siena07(simOnly = TRUE)` targets on random states (asymmetric `W` with a
  nonzero diagonal), with negative controls for the old formulas. The two
  `cycle4` tests in `test-vectorized-effects.R` compared the formula with itself
  and are replaced by a brute-force count.
* **Gibbs stationary law, detailed balance, dynamic QRE and zero-noise selection
  of potential maximizers are now claimed for single-flip logit revision only.**
  The multinomial ministep RSiena and searchnet use (conditional logit over all
  toggles plus pass) keeps irreducibility, aperiodicity and a unique stationary
  law, but for `M > 1` has no general Gibbs form and no zero-noise selection
  guarantee; a counterexample exists at `M = 2`, `N = 3`. Changed in the Blume,
  theory, Brock-Durlauf and proof-registry vignettes, `PROOF_TABLE.md`, the
  README, the JSS paper and online appendix, roxygen, and two manim captions.
* `cycle4` has an exact potential (the number of four-cycles, scaled); the Blume
  tutorial had listed it among effects that may violate the potential condition.
  `PROOF_TABLE.md` row B4 now states the verified `inPop`, `cycle4` and `XWX`
  definitions.
* Proof-registry check I3 still fails and is not tuned. The failure is not
  explained by the non-Gibbs law alone: in I3's setting the exact stationary law
  of the multinomial ministep differs from the Gibbs reference by total
  variation 0.15 on tie counts, but the simulated chain is farther still from
  that exact law (0.35 to 0.56 over three seeds). The cause is undiagnosed.
* Known, not changed: RSiena 1.5.0 offers `simEgoInDist2` for a bipartite
  dependent variable when an actor covariate is declared, contrary to the note
  in `R/searchnet-imitation.R` and the JSS paper; neither searchnet statistic of
  that name equals its target.

# searchnet 0.10.0

JSS submission preparation. Five parallel workstreams against
`R CMD check --as-cran`, the manuscript's dependence on unpublished work, and
the vignettes' check-time cost. Three engine defects surfaced along the way and
are fixed here.

## Engine defects

* **`compute_formal_utility()$nk_fitness` returned a different configuration's
  fitness.** `NK_land_raw` is indexed by configuration (rows) with the N
  per-dimension contributions in its columns, but the lookup passed
  `power_key_index()` -- a per-dimension neighborhood key -- as the ROW index,
  then clamped the result with `min(pk_idx, nrow(NK_land_raw))`. The clamp is
  what made this silent: an out-of-range key returned a neighboring row instead
  of failing. For `N = 4`, `K = 1` and `b_i = (0, 1, 0, 0)` the function
  returned **0.4879**, the value stored for the EMPTY portfolio, where the
  landscape stores **0.2825**. The row is now computed from the configuration
  directly (rows enumerate least-significant-bit first) for a full enumeration,
  matched against the stored configurations for a sampled landscape, and
  reports `NA` rather than a neighbor's value when the configuration is absent.

* **Every single-actor utility call returned `NaN`.** With `M = 1` there is no
  other actor, so `overlaps` is empty and `mean(numeric(0))` is `NaN`; because
  `$total` adds `beta_h * herding`, the whole utility became `NaN` even at
  `beta_h = 0`. That in turn made `compute_choice_probabilities()` return an
  all-`NaN` `delta_u`. Herding is now 0 when there is nobody to overlap with.

* **`DIR_OUTPUT` defaulted to `getwd()`**, and RSiena writes a report `.txt` per
  run into it, so runs dropped files into whatever directory the caller happened
  to be in. Running the vignettes left them in `vignettes/`, where `R CMD build`
  carries them into the tarball. The default is now `tempdir()`; callers that
  want the reports kept pass `dir_output`.

## CRAN readiness

* `R CMD check --as-cran` went from **2 ERRORs, 9 WARNINGs, 5 NOTEs** to a clean
  code and documentation surface. The ERRORs were one cause: `saom_to_saomnk()`'s
  own example passed `gwespFF`, which the function correctly refuses as a
  bipartite non-implementation, so the example demonstrated a call that errors by
  design -- and it halted example checking partway, hiding whatever came after.
* Non-ASCII characters removed from R sources; `ggrepel`, `future` and
  `future.apply` declared in Suggests; `ggExtra` and `ggridges` in Imports (they
  were used unqualified AND undeclared, so `ggMarginal()` would fail at runtime
  for any user without them); 27 `ggsave(file=)` partial argument matches
  corrected; 152 non-standard-evaluation column names declared via
  `utils::globalVariables()`; 17 missing function imports qualified at their call
  sites.
* `.saomnk_dir` was referenced in `searchnet_proof()`'s fallback branch and
  defined nowhere, so that branch could only ever raise "object not found". It
  now raises an informative error. It was deliberately NOT added to
  `globalVariables()`, which would have silenced the only diagnostic that found
  it.
* `RSiena::sienaRI` was written literally. `sienaRI` is present in RSiena 1.5.0's
  namespace but is **not exported**, so the `::` form could only fail; it is now
  resolved at runtime behind the guard that already proves it exists.
* `export(run_counterfactual_with_uncertainty)` added. It carried `@export`, a
  manual page, 8 test references, and all three of its sibling bridge functions
  are exported, but it had no NAMESPACE entry, so it was reachable only through
  `:::`. Same class as the 17 stranded exports fixed in 0.6.0.
  `tools/check_namespace_sync.R` had been exiting 1 because of it.

## Documentation

* Eight hand-maintained `man/` pages that roxygen refuses to overwrite were
  deleted so roxygen owns the topics they were shadowing, resolving 5 duplicated
  names and 23 duplicated aliases. Their substantive content -- references,
  `\seealso`, the Option B / Option C enumeration -- was migrated into the
  roxygen sources, not dropped.
* 26 pages that had never been generated at all are now present, which cleared
  15 "undocumented code objects" and two codoc mismatches.
* Two hazards were latent behind that shadowing and only appeared once the pages
  rendered: `\citep{}` is not an Rd macro, and an apostrophe inside
  `\code{(B'B)}` opens a string the Rd parser never sees closed, aborting
  `tools::parse_Rd()`.
* `saomnk_model.Rd` had been documenting the pre-0.4.0 signature
  (`epistasis_matrix`, `dyad_covariate_effect = "egoXaltX"`); it is regenerated
  from the current one. `tools::checkRd()` is clean across all pages.

## Vignettes

* The check-time cost is now finite. `R CMD check` tangles and sources every
  chunk, including `eval=FALSE` ones, so code that never ran during a build ran
  for real during a check: one basin vignette was killed at a 3000-second
  timeout. Tangled execution across all vignettes is now **367 s**; render is
  **557 s**, from a 26.4-minute baseline that included two failures.
* Basin vignette: the hang was an O(4^N) basin assignment that located each
  Hamming-1 neighbor by scanning all 2^N rows, five times over. Configurations
  are now indexed by bit string.
* `saomnk-proof-registry` had been failing outright ("subscript out of bounds",
  chunk A8) since before the 0.9.3 terminology work. `nk_fitness()` built a full
  N-bit power key and looked it up in a `2^(K+1) x N` table -- the same defect
  family as the engine bug above. Four further defects were masked behind it.
* Three registry checks (I3, J2, K1) are left FAILING and named in the vignette
  rather than tuned to pass. I3 is a systematic mismatch, not sampling noise:
  the empirical-versus-Gibbs correlation is stable at 0.34-0.48 across 400 to
  8000 samples and three seeds.

## The JSS manuscript

* Every figure in the paper is now generated by the replication bundle. Two
  figures are replaced by seeded simulations from a new
  `paper/replication/make_k_system_figures.R`, captioned explicitly as simulated
  output, and citations to unpublished work are removed. A software paper cannot carry a result its replication script cannot
  reproduce, which is the JSS requirement the old figures failed.
* **`paper/replication/reproduce_all.R` did not run at all as committed.** It
  aborted at startup under `Rscript` (`sys.frame(1)$ofile` is defined only under
  `source()`), and two of five blocks had drifted from the manuscript, using
  `M = 1`, which the engine now rejects. It runs end to end in 45 s.
* `paper/jss_submission/` regenerated from the swept sources; it had been
  carrying stale citations and figure references. Nothing in the
  repository regenerates that bundle, so it re-rots after every paper change.
* Five unreferenced figures that belong to other projects are excluded from the
  public snapshot.
* A forward outline of the sections the paper still lacks -- time-varying
  couplings, network-behavior coevolution, two-sided ties, pre-estimation
  diagnostics -- is in `paper/_drafts/`, in outline form rather than prose.

## Known and deliberately open

* `fit_rsiena_static()` passes a bare `structure_model` that is neither a formal
  nor a field (the field is `config_structure_model`). Its only caller is
  `fit_rsiena_shocks()`, and no test names either. Not fixed here.
* A basin vignette's four policy arms produce an identical landscape,
  because `compute_fitness_landscape()` takes no theta: a shock moves where
  firms sit, not the shape of the basins. The computed output now reports
  geometry and occupancy separately; the vignette's propositions are unchanged
  and need an author decision.

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
  the standard: W is the
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
