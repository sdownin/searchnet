# Notation Concordance: searchnet JSS Paper and CD2026 "Architecture of Rivalry"

This document records the harmonized notation system shared between:
- **JSS paper**: `searchnet-jss.Rmd` (Journal of Statistical Software manuscript for the searchnet R package)
- **CD2026**: `architecture_of_rivalry.Rmd` + `appendix_supplement.Rmd` (Management Science submission)

The CD2026 appendix (Online Appendix with formal proofs) is the **canonical reference** for all mathematical notation, since it contains the complete proof apparatus. The JSS paper was updated on 2026-04-04 to align with CD2026.

---

## Core Symbols

| Symbol | Meaning | Notes |
|--------|---------|-------|
| N | Number of binary components (decisions, activities, routes) | Same in both papers |
| K | Epistasis parameter in classical NK model; each component depends on K others | Same in both papers |
| M | Number of actors (agents, firms) in SAOM-NK | Same in both papers |
| **x** in {0,1}^N | Binary configuration vector in the classical NK model (single-agent state) | JSS originally used **c**; updated to **x** to match CD2026 |
| **B** in {0,1}^{M x N} | Bipartite incidence matrix; row b_i is actor i's configuration | JSS originally used **X**; updated to **B** to match CD2026 |
| b_{ij} | Entry of bipartite matrix: 1 if actor i affiliates with component j | JSS originally used x_{ij}; updated to b_{ij} |
| b_i | Row vector: actor i's configuration in {0,1}^N | Same in both papers |
| **E** in {0,1}^{N x N} | Epistasis (interaction) matrix in SAOM-NK (E_{dd}=1 for all d) | JSS used **W** for the same concept; now uses **E** in formal context, **W** for RSiena's XWX weight matrix implementation |
| **A** in {0,1}^{N x N} | Epistasis (interaction) matrix in the classical NK model | Used in CD2026 Def 1; JSS does not distinguish A from E |
| **W** | Exogenous weight matrix for RSiena's XWX effect (complementarity matrix) | Both papers use W for the RSiena implementation; CD2026 simulation appendix also uses W for inter-activity complementarities |

## Component Payoff and Fitness

| Symbol | Meaning | Notes |
|--------|---------|-------|
| f_d | Component payoff function for component d, drawn i.i.d. Uniform(0,1) | JSS originally used w_i; updated to f_d to match CD2026 |
| d | Primary component index in formal/proof contexts | CD2026 canonical; JSS uses j in RSiena-facing contexts |
| j, k | Component indices in RSiena effect formulas (e.g., sum_j b_{ij}) | Both papers use j, k in effect formulas |
| N(d) | Epistatic neighborhood of component d: {j : A_{dj} = 1}, |N(d)| = K+1 | CD2026 notation |
| W(**x**) | NK fitness: (1/N) sum_{d=1}^{N} f_d(x_{N(d)}) | CD2026 Def 1; JSS refers to this implicitly |
| PK(N) | Power key: (2^{N-1}, 2^{N-2}, ..., 2^0) for binary-to-integer conversion | Same in both papers |
| NK_land | Payoff lookup table with i.i.d. Uniform(0,1) entries | Same in both papers |
| Hadamard product | Element-wise multiplication, used in epistasis masking: b_i . E_{d.} | Same in both papers |

## SAOM Parameters

| Symbol | Meaning | Notes |
|--------|---------|-------|
| theta | Vector of SAOM effect parameters (theta in R^p) | Same in both papers |
| beta > 0 | Logit precision parameter (inverse temperature) | CD2026 uses beta prominently; JSS now names it in notation paragraph |
| lambda_i | Rate function for actor i | CD2026 Def 5; JSS mentions rate function without explicit symbol |
| u_i(b_i; B_{-i}, E) | Actor i's utility from configuration b_i given others' configs | CD2026 Def 4; JSS uses f_i(B) in eq:saom-objective for the general SAOM form |

## SAOM Network Statistics (Effect Formulas)

| Statistic | RSiena Name | Formula | {K} Dimension |
|-----------|-------------|---------|---------------|
| Density | density | sum_j b_{ij} | -- |
| Activity/Scope | outAct | (sum_j b_{ij})^2 | K_AC |
| Popularity | inPop | sum_j b_{ij} (sum_m b_{mj} + 1) | K_CA |
| Four-cycles | cycle4 | (1/2)[diag(BB')^2 . BB']_{ii} | K_AA |
| Imitation | simEgoInDist2 | (portfolio similarity effect) | K_AA |
| Epistasis | XWX | sum_j b_{ij} sum_k w_{jk} b_{ik} | K_CC |
| Strategy heterogeneity | egoX | (actor-level covariate modifier) | -- |
| Component value | altX | (activity-level payoff differences) | -- |

## {K} Framework Dimensions

| Dimension | Symbol | Interpretation | Network Measure |
|-----------|--------|----------------|-----------------|
| Actor scope | K_AC | Components per actor | Bipartite row degree |
| Component popularity | K_CA | Actors per component | Bipartite column degree |
| Actor sociality | K_AA | Co-affiliating actors per actor | Actor projection degree |
| Component epistasis | K_CC | Co-occurring components per component | Component projection degree |

Projection matrices:
- P_AA = BB' - diag(BB') (actor co-membership projection)
- P_CC = B'B - diag(B'B) (component co-occurrence projection)

Both papers use identical {K} notation.

## Theorem Numbering

| # | Name | JSS | CD2026 | Content Alignment |
|---|------|-----|--------|-------------------|
| 1 | Reduction | Theorem 1 | Theorem 1 | ALIGNED -- Both state NK is a degenerate special case of SAOM-NK under 5 restrictions (R1-R5). JSS updated to list all 5 restrictions explicitly. |
| 2 | Generalization | Theorem 2 | Theorem 2 | ALIGNED -- Both state SAOM-NK strictly generalizes NK along 3 dimensions (multi-actor, endogenous co-evolution, bounded rationality). JSS updated to enumerate dimensions. |
| 3 | Approximation | Theorem 3 | Theorem 3 | ALIGNED -- Both state any NK payoff structure can be reproduced to arbitrary precision. JSS updated to reference 3 proof strategies (dummy covariates, actor heterogeneity, mixed logit). |
| 4 | SAOM-QRE Equivalence | Theorem 4 | (not in CD2026) | JSS-ONLY -- Establishes isomorphism between SAOM logit choice and quantal response equilibrium. Not present in CD2026. |

## Notation for the W/E Disambiguation

This is the most important disambiguation between the two papers:

- **E** (formal proofs): The N x N binary influence matrix (the support of W) encoding which components interact in the fitness function. E_{dd} = 1 for all d. This is the mathematical object in Definitions 1-5 of the CD2026 proofs.

- **W** (RSiena implementation): The N x N weight matrix supplied to RSiena's XWX bipartite effect. In practice, when encoding NK epistasis, W = E. But W can also encode continuous-valued complementarities (as in CD2026's simulation model, where W_{jk} represents synergy strength between activities j and k).

- **A** (classical NK): The NK influence matrix (Kauffman's interaction matrix; Rivkin and Siggelkow 2007 use the name influence matrix). In the SAOM-NK reduction theorem, restriction (R2) sets E = A.

Rule of thumb: Use **E** when writing formal mathematical statements about the SAOM-NK model. Use **W** when discussing RSiena implementation or empirical specifications. Use **A** when discussing the classical NK model specifically.

## Changes Made to JSS (2026-04-04)

1. **c** -> **x** for NK configuration vector (Section 3.1)
2. **X** -> **B** for bipartite incidence matrix (throughout)
3. x_{ij} -> b_{ij} for bipartite matrix entries (all effect formulas, choice equation)
4. w_i -> f_d for component fitness contribution (Section 2.1)
5. Added notation paragraph at start of Section 3 establishing alignment with CD2026
6. Added parenthetical note linking W (RSiena) to E (formal proofs) in Section 3.3
7. Updated Theorems 1-3 statements to match CD2026 content (5 restrictions, 3 dimensions, 3 proof strategies)
8. Added cross-reference note: "Theorems 1-3 correspond to Theorems 1-3 in [CD2026]; Theorem 4 is specific to this paper"
9. Changed NK $\subset$ SAOM to NK $\subsetneq$ SAOM-NK in feature comparison table
10. Updated X'WX to B'WB in feature comparison table

## Terminology concordance with Paper T (CD2026), recorded 2026-08-21

Paper T (CD2026, commit 707f0b2, "TERMINOLOGY STANDARD") and searchnet 0.8.2
share one standard: W is the influence matrix (the NK interaction matrix with
real-valued entries giving the magnitude and sign of one activity's influence
on another's fitness contribution); E is its binary influence pattern
(support); A is the NK influence matrix; epistasis is the fitness
interdependence W induces, reported as K_CC, which keeps the label
"Epistasis". Theorem 1 (R2): E = A, the SAOM-NK influence matrix equals the NK
influence matrix. The two documents are checked against each other so they
cannot drift silently; these are the deliberate differences that remain:

1. **Model name spelling.** The package, its proofs (`PROOF_TABLE.md`, the
   equivalence proof tex) and the JSS paper write "SaoMNK" where Paper T
   writes "SAOM-NK". Only the matrix term was swept; the name spelling is the
   package's historical form and is left internally consistent. Paper T's R2
   therefore reads "SAOM-NK influence matrix", the proof table's C2 "SaoMNK
   influence matrix".
2. **Where "interaction matrix" may still appear.** Paper T keeps one
   definitional aside in Section 3.1 ("the NK interaction matrix with
   real-valued entries"). searchnet keeps the same aside in exactly two
   places: the `saomnk_model()` help for `influence_matrix` (and its generated
   Rd) and the entry for A in this file ("Kauffman's interaction matrix"). The
   README's "it is also called the interaction matrix" is the third, outside
   the package sources. `tests/testthat/test-terminology.R` fails on any
   other occurrence in R/, man/, vignettes/ or inst/proofs/.
3. **Estimated influence matrices.** searchnet additionally names the
   co-occurrence object built from observed data an *influence-matrix
   estimate* (`saomnk_empirical_influence()`, and the bridge's W "estimated
   from SIC co-occurrence"). Paper T has no corresponding object; this is an
   extension of the standard, not a departure from it.
4. **Scenario key.** `get_orm_scenarios()` keeps the list key
   `epistasis_boost` ("What if component interdependencies doubled?"): it
   doubles the XWX weight, i.e. the induced epistasis, so the name is correct
   under the standard and it is a user-facing key.
