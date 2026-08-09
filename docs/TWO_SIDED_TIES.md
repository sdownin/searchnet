# Two-Sided Tie Formation: Initiative and Confirmation

`SaoMNK/R/searchnet-assent.R`

## What this adds

SaoMNK's bipartite dependent variable is formed by unilateral ministeps: an
actor decides to add a component tie, and the tie exists. Many settings are
two-sided — the actor may only *propose*, and the tie exists only if the
component side confirms.

| Domain | Actor proposes | Component confirms |
|---|---|---|
| Alliances / status | firm seeks a prestigious partner | partner assents, staking its own standing |
| Employee mobility | worker applies | firm makes the offer |
| Investment | venture pitches | investor funds |
| Categories | producer claims membership | gatekeeper admits |

This corresponds to what Snijders calls *unilateral initiative with reciprocal
confirmation* in non-directed SAOMs, applied to the bipartite case.

## Why it cannot be done in the objective function

The objective function represents the **proposer's** preferences. A
counterparty veto is not a preference of the proposer, so no weighting of
`density`, `inPop`, `outAct`, or a dyadic covariate reproduces it. Adding a
`quality × prestige` term makes high-quality actors *want* prestigious
partners more; it does not make prestigious partners *refuse* low-quality ones.
The two produce different networks, and only the second is screening.

## Why it matters theoretically

The confirmation step is a screening device, and screening is what puts
information into a tie. Demonstration, from `saomnk_run_two_sided` with the
same model and the same seed:

```
                          cor(actor quality, degree)
proposal network (one-sided)              +0.010
confirmed network (two-sided)             +0.559

confirmation rate: high-quality actors 0.415, low-quality actors 0.090
```

Two networks, identical generating preferences, near-identical structural
parameters — and one carries a strong signal about actor quality while the
other carries none. **An observer can infer something from a confirmed tie that
they cannot infer from a proposed one.** Any theory in which ties act as
signals, certifications, or endorsements needs this distinction, and it is
invisible to a model that treats all ties as declared.

This is the mechanism behind the difference between a negotiated affiliation
and a declared one, and it generalizes: wherever one side can veto, the
resulting network is informative about whatever that side screens on.

## Usage

```r
mod <- saomnk_model(density = -2.2, popularity = 0.35)

# screening on a binary actor attribute, with the most prestigious
# components the most demanding
a <- saomnk_assent(actor_attribute = quality,
                   rate_high = 0.62, rate_low = 0.16,
                   component_selectivity = 0.45 * prestige,
                   name = "partner assent")

env <- saomnk_env(M = 100, N = 30, density = 0, seed = 7)
r <- saomnk_run_two_sided(env, mod, assent = a, waves = 3,
                          steps_per_actor = 6, seed = 7, verbose = TRUE)

r$degrees                    # K_AC, K_CA, K_AA, K_CC, gini, density per wave
r$assent_diagnostics[[3]]    # confirmation rates and the screening signal
r$proposals[[3]]             # what was asked for
r$confirmed[[3]]             # what was granted
```

Run with `assent = NULL` for the one-sided case — that comparison is what
isolates the contribution of the confirmation step.

Three ways to specify the rule, in increasing generality:

```r
saomnk_assent(prob = 0.5)                              # flat
saomnk_assent(actor_attribute = q, rate_high = .6, rate_low = .2)   # screening
saomnk_assent(prob = P_matrix)                         # full M x N control
```

## Functions

| Function | Purpose |
|---|---|
| `saomnk_assent()` | Build a confirmation rule |
| `saomnk_confirm(env, assent)` | Apply confirmation to a run; returns proposal, confirmed, diagnostics |
| `saomnk_run_two_sided(env, model, assent, waves)` | Multi-wave propose-then-confirm loop |

`saomnk_confirm` replaces `env$bipartite_matrix` with the confirmed network so
downstream projections and plots operate on realised ties. The proposal network
is **returned** rather than attached to the environment, because the R6
environment is locked and rejects new bindings.

## Diagnostics

`assent_diagnostics` reports:

- `proposals`, `confirmed`, `confirmation_rate`
- `confirmation_rate_by_group` — confirmation rates above and below the
  screening threshold, i.e. how selective the confirming side actually was
- `attribute_degree_cor` — the correlation between the screened attribute and
  degree, in the proposal and confirmed networks. The gap between these two is
  the information the confirmation step injected, and it is the quantity most
  worth reporting.

## Two notes for anyone extending this

**Effect names for bipartite dependents differ from one-mode.** The API
reference documents `egoXaltX` as the default `dyad_covariate_effect`, but that
effect does not exist for a bipartite DV. RSiena reports the valid list, in
which the dyadic-covariate effect is `X`. A model specified with an invalid
effect name is skipped with a warning and the simulation proceeds without it,
which is easy to miss.

**Verify that a covariate actually bites.** The reliable test is behavioural,
not structural: run the same seed at two very different weights and confirm the
resulting networks differ. Inspecting the model object is not sufficient,
because effects are split across `dv_bipartite$effects` and
`dv_bipartite$coDyadCovars`, so looking in one slot can suggest an effect is
missing when it is merely stored elsewhere.
