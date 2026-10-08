# AI-drafted edits, 2026-09-15

Record of every prose change drafted by Claude (Anthropic) in this package on
2026-09-15, kept so the author can review and re-voice the spans and assemble
the AI-use disclosure for the JSS submission. No earlier AI-use record file
existed in the package, so this file is new.

Scope of the session: (1) four actor statistics in
`get_struct_mod_stats_mat_from_bi_mat()` corrected to match RSiena 1.5.0's
`siena07()` targets; (2) Gibbs-law, detailed-balance, dynamic-QRE and
zero-noise-selection claims restricted to single-flip logit revision.

Line numbers are in the edited file, taken from `git diff` on 2026-09-15.
"Words" is the approximate number of new words at that location.

Tiers follow the author's collaboration model:
**mechanical** (no new prose), **derivative** (rewording or a qualifier on an
existing claim, no new sentence), **generative** (a new sentence or claim).
Rows marked **>40** exceed the 40-word guideline.

## Vignettes

| File | Line | Old -> new (abridged) | Words | Tier |
|---|---|---|---|---|
| vignettes/saomnk-blume-tutorial.Rmd | 59-63 | "...and why that distribution corresponds to a QRE" -> "...distribution. Its Gibbs form, and its reading as a QRE, hold for single-flip logit revision; the multinomial ministep that RSiena and searchnet use has a unique stationary law but, for $M > 1$, no general Gibbs form." | 33 | generative |
| vignettes/saomnk-blume-tutorial.Rmd | 153-156 | "That distribution is predictable, computable, and corresponds to ... (QRE)." -> "Under single-flip revision that distribution is the Gibbs measure ... (QRE); searchnet's multinomial ministep keeps the uniqueness but not, in general, the Gibbs form." | 23 | generative |
| vignettes/saomnk-blume-tutorial.Rmd | 199 | "That is the Gibbs measure at work." -> "That is ergodicity at work." | 1 | derivative |
| vignettes/saomnk-blume-tutorial.Rmd | 310-313 | "**Blume's theorem applies directly**: the SAOM CTMC has a unique stationary distribution, and it is the Gibbs measure" -> "the SAOM CTMC has a unique stationary distribution. **Blume's theorem gives it the Gibbs form only under single-flip revision**; the multinomial ministep (all toggles plus pass) ... has, for $M > 1$, no general Gibbs form." | 30 | generative |
| vignettes/saomnk-blume-tutorial.Rmd | 323 | "**Computability**:" -> "**Computability** (single-flip revision only):" | 3 | derivative |
| vignettes/saomnk-blume-tutorial.Rmd | 334-337 | "...for what the simulation converges to: the simulation finds the logit QRE..." -> "...for what a single-flip logit chain converges to. The multinomial ministep that searchnet simulates also has a unique stationary law, but for $M > 1$ it is not in general the Gibbs measure, so the name is not guaranteed there." | 33 | generative |
| vignettes/saomnk-blume-tutorial.Rmd | 435 | chunk output `cat("The Gibbs measure ... predicts that")` -> `cat("Under single-flip revision, the Gibbs measure ... predicts that")` | 3 | derivative |
| vignettes/saomnk-blume-tutorial.Rmd | 498-501 | new paragraph after the detailed-balance proof: "The cancellation needs the same logit denominator at x and x', as in single-flip revision. The multinomial SAOM ministep normalizes over each state's own toggles plus pass, so for $M > 1$ detailed balance and the Gibbs form fail in general." | 41 **>40** | generative |
| vignettes/saomnk-blume-tutorial.Rmd | 583-584 | "converges to the Gibbs measure." -> "converges to the unique stationary distribution, which is the Gibbs measure under single-flip revision." | 9 | derivative |
| vignettes/saomnk-blume-tutorial.Rmd | 598-603 | items 3-4: "exactly the logit best-response rule from Blume's theorem / Blume applies: ... Gibbs measure" -> "a logit response over the current state's toggles plus pass, not single-flip revision / What applies: unique stationary distribution; Gibbs under single-flip revision, but not in general under the multinomial ministep with $M > 1$" | 31 | generative |
| vignettes/saomnk-blume-tutorial.Rmd | 612-615 | "`simEgoInDist2` ... and `cycle4` (structural closure) introduce asymmetries" -> "`simEgoInDist2` ... introduce asymmetries ... `cycle4` (structural closure) does not: it has an exact potential, the number of four-cycles." | 14 | generative |
| vignettes/saomnk-blume-tutorial.Rmd | 830 | "the Gibbs distribution is diffuse" -> "the stationary distribution is diffuse" | 1 | mechanical |
| vignettes/saomnk-blume-tutorial.Rmd | 919 | chunk output "confirming Blume's theorem: the Gibbs measure is unique." -> "consistent with a unique stationary distribution (this does not test the Gibbs form)." | 11 | derivative |
| vignettes/saomnk-blume-tutorial.Rmd | 924-927 | "compares the exact Gibbs distribution (computed analytically) with the empirical distribution ... confirms ... sampling from the Gibbs measure" -> "compares the empirical tie-count distributions of the sparse-start and dense-start chains ... shows both reach the same stationary distribution, as uniqueness predicts; it does not test the Gibbs form." | 23 | generative |
| vignettes/saomnk-blume-tutorial.Rmd | 933 | "the Gibbs measure concentrates" -> "the stationary distribution concentrates" | 2 | mechanical |
| vignettes/saomnk-blume-tutorial.Rmd | 1068-1070 | diagram: "logit best response / Blume's theorem applies / unique stationary distribution (Gibbs/QRE)" -> "multinomial logit response / unique stationary distribution (ergodic chain) / Gibbs/QRE form only under single-flip revision" | 10 | derivative |
| vignettes/saomnk-proof-registry.Rmd | 411, 419 | check B4c code `sum(b1 * (n_j + 1))` -> `sum(b1 * n_j)` plus comment "n_j counts actor 1 (RSiena inPop)" | 6 | mechanical |
| vignettes/saomnk-proof-registry.Rmd | 980-982 | "converges to a unique Gibbs measure that is a QRE." -> "converges to a unique stationary distribution. It is a Gibbs measure and a QRE under single-flip logit revision; under the multinomial ministep with $M > 1$ neither is guaranteed." | 20 | generative |
| vignettes/saomnk-proof-registry.Rmd | 1106 | F6 label "Chain of identification:" -> "Chain of identification (single-flip revision):" | 2 | derivative |
| vignettes/saomnk-proof-registry.Rmd | 1112-1116 | F7 statement "is a logit QRE: (1)...(4) computable Gibbs form, (5) concentrates on Nash equilibria" -> "(1) exists, (2) is unique, (3) independent of initial conditions. Under single-flip revision it also (4) has the Gibbs form, a logit QRE, and (5) concentrates on potential maximizers ...; the multinomial ministep with $M > 1$ guarantees neither." | 27 | generative |
| vignettes/saomnk-proof-registry.Rmd | 1118 | F7 label "stationary dist = Gibbs = logit QRE (demonstrated by convergence)" -> "stationary dist is unique (demonstrated by convergence; Gibbs = logit QRE only under single-flip)" | 8 | derivative |
| vignettes/saomnk-proof-registry.Rmd | 1618-1623 | I3 "**Statement.** The empirical state frequencies ... converge to the Gibbs measure" -> "**Statement tested.** Whether [same] ... That is guaranteed only for single-flip revision; here the multinomial ministep's exact law differs from it (total variation 0.15 on tie counts). The check fails, but the simulated chain is also far from that exact law, so the cause is undiagnosed." | 46 **>40** | generative |
| vignettes/saomnk-proof-registry.Rmd | 1698 | I3 label "QRE = Gibbs:" -> "Gibbs law (single-flip only; see statement):" (threshold and logic unchanged; still fails) | 6 | derivative |
| vignettes/saomnk-theory.Rmd | 243-244 | Theorem 4 "is a Quantal Response Equilibrium" -> "is unique; under single-flip logit revision it is a Quantal Response Equilibrium" | 7 | derivative |
| vignettes/saomnk-theory.Rmd | 249-253 | "ergodic and its stationary distribution corresponds to a logit QRE ... (Blume, 1993)." -> "ergodic with a unique stationary distribution. For a potential game under single-flip logit revision, that distribution is the Gibbs measure, a logit QRE ... (Blume, 1993); the multinomial ministep used by RSiena and searchnet has no general Gibbs form or zero-noise selection guarantee for $M > 1$." | 37 | generative |
| vignettes/saomnk-theory.Rmd | 292 | table cell "(SaoMNK)" -> "(SaoMNK, single-flip revision)" | 2 | derivative |
| vignettes/saomnk-brock-durlauf.Rmd | 41 | "under logit choice" -> "under single-flip logit choice" | 1 | derivative |
| vignettes/saomnk-brock-durlauf.Rmd | 81-83 | "SAOM ministep process is *Glauber dynamics* ... Hamiltonian," -> "single-flip SAOM ministep is *Glauber dynamics* ... Hamiltonian (the multinomial ministep RSiena uses is not, and has no general Gibbs form for $M > 1$)," | 17 | generative |
| vignettes/saomnk-brock-durlauf.Rmd | 86 | "(Gibbs stationary distribution)" -> "(Gibbs stationary distribution, under single-flip revision)" | 3 | derivative |
| vignettes/saomnk-brock-durlauf.Rmd | 199 | "Under Rb1--Rb5," -> "Under Rb1--Rb5 and single-flip logit revision," | 4 | derivative |
| vignettes/saomnk-brock-durlauf.Rmd | 214 | "(Blume's theorem, F5/F7)" -> "(Blume's theorem, F5/F7, which requires single-flip revision)" | 4 | derivative |
| vignettes/saomnk-brock-durlauf.Rmd | 769 | "(stationary distribution exists and is Gibbs)" -> "(... exists and is unique; it is Gibbs under single-flip revision)" | 6 | derivative |
| vignettes/saomnk-brock-durlauf.Rmd | 779 | "under noisy logit choice" -> "under noisy single-flip logit choice" | 1 | derivative |

## Proof table

| File | Line (row) | Old -> new (abridged) | Words | Tier |
|---|---|---|---|---|
| inst/proofs/PROOF_TABLE.md | 86 (B4) | (c) `sum_j b_ij * (sum_m b_mj + 1)` -> `sum_j b_ij * sum_m b_mj (the column degree counts i)`; (d) `(1/2) [diag(BB')^2 * BB']_ii` -> `(1/2) sum_{k != i} choose((BB')_ik, 2)`; (e) `sum_k w_jk b_ik` -> `sum_{k != j} w_kj b_ik` | 6 | derivative |
| inst/proofs/PROOF_TABLE.md | 163 (F3) | appended "cycle4 also has an exact potential (the number of four-cycles, scaled)." | 11 | generative |
| inst/proofs/PROOF_TABLE.md | 165 (F5) | appended to justification: "The first equality needs the same logit denominator at x and x', which holds for single-flip revision; the multinomial SAOM ministep (all toggles plus pass) violates it, so for M > 1 detailed balance and the Gibbs law fail in general." | 41 **>40** | generative |
| inst/proofs/PROOF_TABLE.md | 166 (F6) | step (3) gains "under single-flip revision ... ; the multinomial ministep keeps uniqueness but, for M > 1, not the Gibbs form." | 17 | generative |
| inst/proofs/PROOF_TABLE.md | 167 (F7) | "with logit choice" -> "with single-flip logit choice" | 1 | derivative |
| inst/proofs/PROOF_TABLE.md | 167 (F7) | inserted "For the multinomial ministep used by RSiena and searchnet, (1)-(3) hold, but (4) and the beta -> infinity part of (5) are not guaranteed for M > 1; a counterexample to (5) exists at M = 2, N = 3." | 36 | generative |
| inst/proofs/PROOF_TABLE.md | 167 (F7) | plain English "and what it converges to" -> "and under single-flip revision what it converges to" | 3 | derivative |
| inst/proofs/PROOF_TABLE.md | 279 (L9) | "Under Rb1--Rb5," -> "Under Rb1--Rb5 and single-flip logit revision," | 4 | derivative |
| inst/proofs/PROOF_TABLE.md | 280 (L10) | "under restrictions Rb1--Rb5," -> "under restrictions Rb1--Rb5 and single-flip logit revision," | 4 | derivative |
| inst/proofs/PROOF_TABLE.md | 287 (L17) | "(SAOM ministep = Glauber dynamics" -> "(single-flip SAOM ministep = Glauber dynamics" | 1 | derivative |
| inst/proofs/PROOF_TABLE.md | 287 (L17) | hedge "the correspondence is one of stationary distributions and detailed balance, not literal generator identity, since ... 2-state spin flip." -> "the correspondence of stationary distributions and detailed balance holds for single-flip revision only, since ... 2-state spin flip, and for M > 1 that multinomial chain has no general Gibbs form." | 20 | generative |

Row B2 (line 84), named in the audit, contains no Gibbs or statistic claim and was not changed.

## JSS manuscript and online appendix

The live source is `paper/searchnet-jss.Rmd`. `paper/jss_submission/searchnet-jss.Rmd` is a byte-identical shipped copy and received identical edits. `paper/_searchnet-jss-web.Rmd` is the tracked source of the public Pages site; it received the same replacement text (its second passage had diverged wording).

| File | Line | Old -> new (abridged) | Words | Tier |
|---|---|---|---|---|
| paper/searchnet-jss.Rmd | 508-511 | appended to Theorem 4: "The resulting chain has a unique stationary law; its Gibbs form and zero-noise selection of potential maximizers are guaranteed for single-flip logit revision, not for this multinomial rule when $M > 1$." | 32 | generative |
| paper/searchnet-jss.Rmd | 985-988 | "The vignette also provides computational verification that the SAOM Markov chain satisfies detailed balance with respect to the Gibbs distribution, which is the mechanism underlying the convergence measured here." -> "The vignette derives detailed balance with respect to the Gibbs distribution for single-flip logit revision. The multinomial ministep simulated here has no general Gibbs form for $M > 1$, so the convergence measured here rests on ergodicity alone." | 26 | generative |
| paper/jss_submission/searchnet-jss.Rmd | 508-511 | identical to the live source | 32 | generative |
| paper/jss_submission/searchnet-jss.Rmd | 985-988 | identical to the live source | 26 | generative |
| paper/_searchnet-jss-web.Rmd | 477-480 | identical Theorem 4 sentence | 32 | generative |
| paper/_searchnet-jss-web.Rmd | 903-906 | "The tutorial provides computational verification that the SAOM Markov chain satisfies detailed balance ..., confirming convergence to the correct stationary distribution." -> same replacement as the live source | 29 | generative |
| paper/searchnet-jss-online-appendix.Rmd | 379-380 | Theorem 4 "is a Quantal Response Equilibrium" -> "is unique; under single-flip logit revision it is a Quantal Response Equilibrium" | 7 | derivative |
| paper/searchnet-jss-online-appendix.Rmd | 389-392 | "with logit best response ... equal to the Gibbs measure:" -> "with single-flip logit revision ... equal to the Gibbs measure (the multinomial ministep has no general Gibbs form for $M > 1$):" | 12 | generative |
| paper/searchnet-jss-online-appendix.Rmd | 403 | "**Limiting behavior**: as" -> "**Limiting behavior**: under single-flip revision, as" | 3 | derivative |

## README, docs, R sources, Rd, tests, manim

| File | Line | Old -> new (abridged) | Words | Tier |
|---|---|---|---|---|
| README.md | 326 | "&rarr; QRE at Stationarity" -> "&rarr; Unique Stationary Law (QRE under Single-Flip Revision)" | 6 | derivative |
| README.md | 339 | "Stationary distribution is a Quantal Response Equilibrium" -> "Stationary distribution is unique; it is a Gibbs measure and Quantal Response Equilibrium under single-flip logit revision, not in general for the multinomial ministep with M > 1" | 21 | generative |
| docs/B_D_2001_CROSS_CITATIONS.md | 47 | "the simulation's stationary distribution is a Gibbs measure" -> "under single-flip logit revision, the stationary distribution is a Gibbs measure" | 4 | derivative |
| docs/animation_gallery.Rmd | 220 | "searchnet converges to QRE." -> "searchnet converges (to a QRE only under single-flip revision)." | 6 | derivative |
| R/searchnet-ergodicity.R | 9-12 (comment) | "Blume's result says the logit-response Markov chain is ergodic with a unique stationary Gibbs distribution" -> "The logit-response Markov chain is ergodic with a unique stationary distribution (Gibbs, by Blume's result, only under single-flip revision; the multinomial ministep has no general Gibbs form for M > 1)" | 22 | generative |
| R/mean_field_solver.R | 3-4 (roxygen) | "(the Gibbs/Brock-Durlauf equivalence)" -> "(the Gibbs/Brock-Durlauf equivalence, under single-flip logit revision)" | 5 | derivative |
| R/mean_field_solver.R | 104-105 (roxygen) | "Theorem 4: Gibbs stationary distribution and the" -> "Theorem 4: stationary distribution (Gibbs under single-flip revision) and the" | 4 | derivative |
| man/mean-field-solver.Rd | 8-9 | hand-edited to mirror R/mean_field_solver.R 3-4 | 5 | derivative |
| man/solve_mean_field.Rd | 96-97 | hand-edited to mirror R/mean_field_solver.R 104-105 | 4 | derivative |
| tests/testthat/test-searchnet-ergodicity.R | 4-6 (comment) | "the Theorem 4 (SAOM-QRE equivalence) demonstration." -> "the Theorem 4 demonstration of independence from initial conditions (a unique stationary law; its Gibbs/QRE form holds only under single-flip revision)." | 17 | generative |
| inst/manim/scene_blume_part2.py | 579 (on-screen) | "Forward and backward rates balance: this is why the simulation converges" -> "Forward and backward rates balance under single-flip logit revision" | 4 | derivative |
| inst/manim/scene_blume_part2.py | 896 (on-screen) | "searchnet CTMC converges to QRE" -> "searchnet CTMC converges (QRE only if single-flip)" | 4 | derivative |
| R/saomnk-base.R | 1497-1540, 1579-1590 (code comments) | new comments documenting the corrected inPop, inPopSqrt, cycle4 and XWX formulas and XWX centering | ~150 **>40** | generative |
| tests/testthat/test-vectorized-effects.R | 62-66, 76-77, 91-92, 106 (code comments) | comments explaining the replaced self-referential tests and the brute-force count | ~60 **>40** | generative |
| tests/testthat/test-structural-stats-vs-rsiena.R | 1-21 and inline (new file, code comments) | header and comments describing the RSiena target comparison | ~160 **>40** | generative |
| NEWS.md | 1-47 | new "searchnet (development version)" entry, 6 bullets | ~400 **>40** | generative |

## Totals

| Tier | Locations |
|---|---|
| mechanical | 4 |
| derivative | 34 |
| generative | 32 |
| **all** | **70** |

Rows over 40 words: blume-tutorial 498-501; proof-registry 1618-1623; PROOF_TABLE F5; the code and test comments; NEWS.md.

## Not edited

- `paper/searchnet-jss-NEW2.tex` (lines 661, 1299): a stale rendered copy dated 2026-08-24, not a source; left unchanged.
- `paper/jss_submission/searchnet-jss-manuscript.pdf` and the rendered appendix HTML predate these edits and must be regenerated.
