# SaomNK: a Lean 4 library for stochastic actor-oriented models on NK landscapes

Machine-checked building blocks for the SAOM-NK framework of the searchnet R
package: actors revising portfolios of activities on an NK fitness landscape
(Kauffman 1993; Levinthal 1997) by the ministeps of a stochastic actor-oriented
model (Snijders 2001). The library proves general results once, and the R
package exports any concrete model as a Lean file that instantiates them and
proves numeric facts about that model by kernel evaluation.

Status: 273 declarations (141 theorems, 132 lemmas) in 30 modules. No `sorry`, no `axiom`, no
`native_decide`. Every declaration depends only on `propext`,
`Classical.choice` and `Quot.sound`; `Axioms.lean` audits all of them.

## Layout

| Module | Content |
|---|---|
| `Core/{Indicator,Config,Flip}` | configurations, unilateral deviations, flips, Hamming distance |
| `NK/Influence` | influence patterns, `IsKRegular` (decidable), the K = 0 and K = N - 1 cases |
| `NK/Fitness` | NK term and NK fitness, held-only reading, dummy-covariate reconstruction |
| `NK/PowerKey` | LSB and MSB binary codes, `codeEquiv : (Fin N → Bool) ≃ Fin (2^N)` |
| `NK/Table` | raw and enumerated landscape tables; consistency; table lookup = NK term |
| `NK/Reduction` | one actor, aligned influence, no network effects: utility = N × NK fitness |
| `Stats/{Density,InPop,Cycle4,XWX,RowPairStat}` | statistics with flip formulas and their potentials |
| `Potential/{Rosenthal,Exact,Nash}` | `IsExactPotential` and its algebra, the building blocks, `Spec`, `CoreSpec`, Nash existence, flip stability = local maxima of the potential |
| `Choice/{Glauber,Metropolis,Logit,Limits}` | acceptance rules, reversibility, multinomial logit, zero-noise limits |
| `Chain/{Balance,Restricted,Reachable}` | Gibbs detailed and global balance for any reversible acceptance rule and state-independent rates; structural zeros (restricted chain, primary); irreducibility and uniqueness of potentials as corollaries |
| `TwoLayer/Integrability` | two coupled layers: integrability iff symmetric coupling, cycle affinity, Kolmogorov necessity, own-term examples |
| `Directional` | creation versus endowment: integrability iff the two change statistics agree |
| `MeanField/{TwoBloc,Landau}` | two-population logit mean field (existence and stability of aligned and polarized states), the Landau quartic |
| `Compute/{RatSpec,Cast,LocalOpt}` | computable rational models; casts to the real theory; local-optima counting with a proof of what the count means |
| `Examples/WeightedOverlap` | worked example of adding a statistic (used by the vignette) |

A clean build of the library takes about 5 minutes with Mathlib already built
(283 s on the reference machine; incremental rebuilds of one module, 15 to
60 s, most of it Mathlib import).

## Build

Toolchain and Mathlib are pinned (`lean-toolchain`, `lake-manifest.json`):
Lean `v4.34.0-rc2`, Mathlib `v4.34.0-rc2`.

```
lake exe cache get     # prebuilt Mathlib (about 0.5 GB download, 7 GB unpacked)
lake build             # the SaomNK library
lake env lean Axioms.lean
```

From R: `searchnet::lean_setup()` prints these commands and runs them only
after confirmation; `searchnet::lean_check()` builds and audits.

### Reusing an existing Mathlib

Mathlib is large, and on file systems with large clusters (exFAT) its build tree
takes far more space than its size. If a Mathlib checkout at the same revision
is already built on the machine, point this project at it instead of
downloading a second copy: write `.lake/package-overrides.json` listing each
dependency as a path dependency (`searchnet::lean_setup(packages_dir = ...)`
writes it from `lake-manifest.json`). The file is machine-specific and ignored
by git. While it exists, do not run `lake update` or `lake exe cache get` here.

### Upgrading Lean and Mathlib

1. Set `lean-toolchain` and the Mathlib `rev` in `lakefile.toml` to the new
   release tag; run `lake update` (without a package override) and commit the
   new `lake-manifest.json`.
2. `lake build`; fix renamed lemmas (deprecations show as warnings first).
3. Regenerate the audit with `searchnet:::.lean_write_axioms_file()` and run
   `lake env lean Axioms.lean`.
4. Re-export and re-check the example instances (`tests/testthat/test-lean.R`).

## Exporting a model from R

```r
library(searchnet)
nk   <- nk_landscape(N = 4, K = 2, seed = 3)
file <- lean_export_model(nk, dir = "lean_out")   # Instance_<hash>.lean
lean_check("lean_out")                            # decl / status / axioms
```

`lean_spec()` maps RSiena effects to the library's statistics
(`lean_effect_map()`); an exported file states, for that model, K-regularity of
the influence matrix, consistency of the landscape table, the index convention
shared with R, the exact potential, Nash existence, Gibbs stationarity (for
binary-logit or Metropolis ministeps), the zero-noise limit, exact utilities at
the current configuration, and the number of single-flip-stable configurations.

## Extending the library

Every statistic module follows one pattern: define the statistic, prove its
change on a flip, and prove one `isExactPotential_*` lemma (or a no-potential
theorem). Exact potentials add (`IsExactPotential.add`), so a new specification
is checked by combining lemmas, and Gibbs stationarity, Nash existence and the
local-optima characterization follow with no new proof. The vignette
`saomnk-lean` works an example. Record new headline results in `registry.yml`
and regenerate `Axioms.lean`.

## Not formalized

* That a finite irreducible continuous-time chain has a unique stationary law
  to which it converges (the textbook fact that turns global balance plus
  irreducibility into uniqueness and convergence).
* Gibbs stationarity under RSiena's default multinomial ministep (choice among
  all flips and no change); the library proves it for binary-logit and
  Metropolis acceptance, which is the protocol under which it holds.
* State-dependent rate functions; root (`#`-parameter) forms of statistics.
* The Fréchet derivative of the two-bloc map (only its partial derivatives), and
  global stability.

## References

Blume, L. E. (1993). The statistical mechanics of strategic interaction. *Games and
Economic Behavior*, 5, 387-424.
Brock, W. A., & Durlauf, S. N. (2001). Discrete choice with social interactions.
*Review of Economic Studies*, 68, 235-260.
Kauffman, S. A. (1993). *The Origins of Order*. Oxford University Press.
Levinthal, D. A. (1997). Adaptation on rugged landscapes. *Management Science*,
43, 934-950.
Monderer, D., & Shapley, L. S. (1996). Potential games. *Games and Economic
Behavior*, 14, 124-143.
Rosenthal, R. W. (1973). A class of games possessing pure-strategy Nash
equilibria. *International Journal of Game Theory*, 2, 65-67.
Snijders, T. A. B. (2001). The statistical evaluation of social network
dynamics. *Sociological Methodology*, 31, 361-395.
Snijders, T. A. B., van de Bunt, G. G., & Steglich, C. E. G. (2010). Introduction
to stochastic actor-based models for network dynamics. *Social Networks*, 32,
44-60.
