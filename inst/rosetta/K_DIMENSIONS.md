# The {K} dimensions: definitions

This file is the single statement of the four {K} dimensions and of how an
effect relates to them, used by `searchnet_effect_dimensions()`,
`searchnet_classify_effect()`, the class registry (`classes.yaml`: `reads`
for the decision dimension, `moves` for the outcome dimension),
`rosetta_plot(view = ...)`, the {K}-4 plots, the README and the
papers. Cite it; change it here and nowhere else. Installed copy:
`system.file("rosetta", "K_DIMENSIONS.md", package = "searchnet")`. The derived
table is shipped beside it as `effect_dimensions.csv`.

*Vocabulary: "{K} dimension" names the four degree types; "channel" is
reserved for mechanisms.*

## Summary

The four dimensions are the margins and projections of the actor-by-component
matrix B: Expansiveness (K_AC), an actor's margin, the components it holds;
Popularity (K_CA), a component's margin, which actors hold it and how many;
Sociality (K_AA), the actor projection BB^T, the actors who share a component
with an actor; and Epistasis (K_CC), the component projection B^T B, the
components co-held with a component, with W setting the value of each
co-holding. Each effect's decision dimension is the one its change statistic depends on, the
outermost of the actor's own portfolio (K_AC), the interaction among components
(K_CC), other actors' holdings (K_AA) and who those actors are (K_CA); its outcome dimension is
that of its target statistic, the moment estimation matches. A count of
other holders is read as the overlap a move creates (K_AA); Popularity is read
only when a statistic distinguishes among holders.

## 1. Objects

- `B`, the M x N actor-by-component affiliation matrix, `b_ij` in {0, 1} (actor
  `i` holds component `j`).
- The two one-mode projections of `B`, off the diagonal: `P_A = B B^T` (M x M;
  `(P_A)_ih` = components actors `i` and `h` share) and `P_C = B^T B` (N x N;
  `(P_C)_jk` = actors holding both `j` and `k`).
- One-mode layers in their own right (not projections): `W`, the N x N
  real-valued influence matrix among components (exogenous; binarized, it is
  NK's interaction matrix); and, where a model has one, `A`, an M x M relational
  layer among actors.

## 2. The four {K} dimensions are degree types

| Dimension | Symbol | Degree | Of node | In |
|---|---|---|---|---|
| Expansiveness | K_AC(i) | `sum_j b_ij` | actor i | B (its row) |
| Popularity | K_CA(j) | `sum_i b_ij` | component j | B (its column) |
| Sociality | K_AA(i) | `sum_{h != i} 1[(P_A)_ih > 0]` | actor i | the actor projection: actors sharing at least one component |
| Epistasis | K_CC(j) | `sum_{k != j} 1[(P_C)_jk > 0]` | component j | the component projection: components co-held with j |

The projection degrees are distinct-partner counts, as searchnet computes them
(`saomnk_get_degrees()`, the {K}-4 panel).

Weighted forms (same dimension, more resolution):

- **Sociality strength**: `sum_{h != i} (P_A)_ih`, overlap counted by shared
  components.
- **Epistasis strength**: `sum_{k != j} (P_C)_jk`, co-holding counted by actors.
- **Epistasis strength valued by W**: `sum_{k != j} W_jk (P_C)_jk`, co-holding
  valued by the influence matrix (the quantity `XWX` targets).
- **Popularity's holder profile**: `sum_i b_ij z_i`, a component's holders
  weighted by an attribute. The degree is its unweighted count.
- Degrees in the one-mode layers are named in words: the **W-degree**
  `sum_k 1[W_jk != 0]` (NK's K, the architecture) and the **A-degree**
  `sum_h a_ih`. A statement about K_CC or K_AA names which ties it counts:
  projection (co-holding, overlap) or layer (W, A). NK's K is the W-degree, not
  K_CC.

## 3. The four degrees are coupled

Three identities hold for every `B` (projections off the diagonal):

- (I1) `sum_i K_AC(i) = sum_j K_CA(j) = |B|`, the number of ties: the two
  bipartite degree types share their first moment.
- (I2) `sum_j K_CA(j) (K_CA(j) - 1) = sum_i [Sociality strength of i]`: the
  second moment of component degree is the first moment of Sociality strength
  (total pairwise overlap).
- (I3) `sum_i K_AC(i) (K_AC(i) - 1) = sum_j [Epistasis strength of j]`: the
  second moment of actor degree is the first moment of Epistasis strength
  (total co-holding).
- Target forms: `inPop` counting the actor, `sum_i sum_j b_ij b_+j = |B| +`
  total overlap; `outAct`, `sum_i K_AC(i)^2 = |B| +` total co-holding.
- The identities hold for STRENGTHS, not for the partner-count degrees.

So no effect moves one degree type in isolation; the identities say where a
quadratic effect on one mode's bipartite degree lands in the other mode's
projection.

## 4. How an effect relates to the dimensions: decision and outcome

The two fields contrast an effect's role in the objective function (the
actor's utility change for a move) with its role in the resulting network
structure. The field names in code are `reads` and `moves`.

- **decision** (field `reads`): the dimension its change statistic
  `Delta s_ij = s_i(b_ij = 1) - s_i(b_ij = 0)`, the actor's utility change for
  the move, depends on: what the deciding
  actor must observe to evaluate the move. Derived in code by a walk-dependency
  test: each input of `Delta s_ij` is perturbed on random networks with the
  others held fixed, and the first input it depends on, in this order, names the
  dimension: who the other holders are (their attributes) -> **Popularity**;
  else other actors' holdings (of `j`, or of components that make overlaps), or
  an actor-pair covariate -> **Sociality**; else `W` -> **Epistasis**; else (the
  actor's own row, its own attributes, the target component's attributes, or
  nothing at all, as for the constant change statistic of `density`) ->
  **Expansiveness**.
- **outcome** (field `moves`): the network-structure dimension of its target
  statistic `sum_i s_i(B)`, the moment
  estimation matches, written through the identities above. Derived in code as
  the exact identity with a {K} moment fitted on random networks. With no exact
  identity: a target invariant to reshuffling each actor's ties is a nonlinear
  moment of Expansiveness, one invariant to reshuffling each component's holders
  a nonlinear moment of Popularity, and one that depends on the holders'
  attributes is similarity-weighted Sociality strength. A statistic centered on
  the mean over the decision's candidates has no target of its own; the
  centering is one constant per decision, so its core statistic still ranks the
  candidates, and its outcome is the core's, marked "(candidate-centered,
  approximate)": for similarity to a component's other holders, "Sociality
  strength, similarity-weighted (candidate-centered, approximate)". A core with
  no outcome gives "none (candidate-centered)".
- An alternative decision rule, "degree" (the degree in the change statistic, a
  count of other holders read as Popularity), is available as `rule = "degree"`
  for comparison; it is not the package rule.

**The tie convention.** `inPop`'s change statistic `n_j + 1` (`n_j` the other
holders of `j`) is at once component `j`'s degree and the overlap the new tie
creates. It is read as Sociality, the deciding actor's view; Popularity is read
only by statistics that distinguish among holders.

**The two limits.** (1) The outcome is the moment the coefficient is identified from
and moves first, not its equilibrium footprint: in the coupled process every
coefficient eventually moves all four dimensions. (2) The sign of the
coefficient changes the reading (crowding or agglomeration for `inPop`), never
the dimension.

## 5. The package's effects (decision -> outcome), as derived

| Effect | Decision dimension | Outcome dimension (target statistic) |
|---|---|---|
| `density` | Expansiveness (fall-through) | total ties (I1) |
| `outAct` | Expansiveness | Epistasis strength, unvalued (I3), plus total ties |
| `outActSqrt` | Expansiveness | Expansiveness, nonlinear moment (no exact identity) |
| `inPop` | Sociality | Sociality strength (I2), plus total ties (the actor is counted, RSiena 1.5.0) |
| `inPopSqrt` | Sociality | Popularity, nonlinear moment (no exact identity) |
| `XWX` | Epistasis | Epistasis strength valued by W |
| `cycle4` | Sociality | Sociality and Epistasis (the four-cycle count is symmetric in the projections) |
| `simEgoInDist2`, `coholder_similarity` | Popularity | Sociality strength, similarity-weighted |
| `totInDist2` | Popularity | Sociality strength, attribute-weighted |
| `egoX` | Expansiveness | Expansiveness, attribute-weighted |
| `altX` | Expansiveness | Popularity, attribute-weighted |
| actor-pair covariate | Sociality | Sociality strength, valued by the actor-pair covariate |
| component-pair covariate (W) | Epistasis | Epistasis strength valued by W |

Constructs are readings of effects, never dimensions: scope (`density`,
`outAct`), complementarity (`W`, `XWX`), crowding (`inPop`, `inPopSqrt`;
agglomeration when the coefficient is positive), contact (`cycle4`), imitation
(covariate similarity).

## 6. Reported levels are not degrees or strengths

The {K}-4 panel plots per-node degrees at each ministep with the LOESS-smoothed
mean degree; its labels say "mean degree" and name what each degree counts, the
projection degrees as partners ("Sociality: actors sharing a component"). A
normalized level (for example a mean pairwise similarity of portfolios) is
neither a degree nor a strength, and any usage that writes K_AA or K_CC for one
names the level.
