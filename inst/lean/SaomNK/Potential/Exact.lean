import SaomNK.Potential.Rosenthal
import SaomNK.Stats.RowPairStat
import SaomNK.Stats.XWX
import SaomNK.NK.Fitness

/-!
# Exact potentials of SAOM-NK evaluation functions

A utility profile `U : Fin M → Config M N → ℝ` (actor `i`'s evaluation function) has an exact
potential `Φ` (Monderer and Shapley 1996) when every unilateral deviation changes the deviator's
utility by exactly the change in `Φ` (`IsExactPotential`). For a SAOM this is the property that
makes the stationary law of the single-flip chain a Gibbs measure (`SaomNK.Chain.Balance`).

**The extension pattern.** Exact potentials are closed under sums and scalar multiples
(`IsExactPotential.add`, `.smul`). Every statistic module proves one `isExactPotential_*`
lemma, and a specification is checked by adding them up. To add a new statistic: define it,
prove its `isExactPotential_` lemma, and combine. The vignette `saomnk-lean` works an example.

Building blocks proved here:

* `isExactPotential_own`: any actor-indexed function of the actor's own row (NK fitness,
  density, outAct, XWX, ego and activity covariates, actor-specific landscapes);
* `isExactPotential_inPopW`: weighted crowding, with the Rosenthal potential;
* `isExactPotential_rowPair`, `isExactPotential_dyad`, `isExactPotential_cycle4`: symmetric
  pairwise statistics.

`Spec` packages the three classes into one evaluation function, and `Spec.exact_potential`
proves it is an exact potential game. `CoreSpec` is the common special case (NK term, density,
outAct, inPop) with a single coefficient per statistic.
-/

open Finset

noncomputable section

namespace SaomNK

variable {M N : ℕ}

/-! ## Exact and flip potentials of a utility profile -/

/-- `Φ` is an exact potential of the utility profile `U`. -/
def IsExactPotential (U : Fin M → Config M N → ℝ) (Φ : Config M N → ℝ) : Prop :=
  ∀ (B B' : Config M N) (i : Fin M), Unilateral B B' i → U i B' - U i B = Φ B' - Φ B

/-- `Φ` is an exact potential for single-entry flips (all a single-flip chain needs). -/
def IsFlipPotential (U : Fin M → Config M N → ℝ) (Φ : Config M N → ℝ) : Prop :=
  ∀ (B : Config M N) (i : Fin M) (j : Fin N), U i (flip B i j) - U i B = Φ (flip B i j) - Φ B

namespace IsExactPotential

variable {U V : Fin M → Config M N → ℝ} {Φ Ψ : Config M N → ℝ}

lemma toFlip (h : IsExactPotential U Φ) : IsFlipPotential U Φ :=
  fun B i j => h B _ i (flip_unilateral B i j)

lemma add (hU : IsExactPotential U Φ) (hV : IsExactPotential V Ψ) :
    IsExactPotential (fun i B => U i B + V i B) (fun B => Φ B + Ψ B) := by
  intro B B' i h
  have h1 := hU B B' i h
  have h2 := hV B B' i h
  linear_combination h1 + h2

lemma smul (c : ℝ) (hU : IsExactPotential U Φ) :
    IsExactPotential (fun i B => c * U i B) (fun B => c * Φ B) := by
  intro B B' i h
  have h1 := hU B B' i h
  linear_combination c * h1

lemma sub (hU : IsExactPotential U Φ) (hV : IsExactPotential V Ψ) :
    IsExactPotential (fun i B => U i B - V i B) (fun B => Φ B - Ψ B) := by
  intro B B' i h
  have h1 := hU B B' i h
  have h2 := hV B B' i h
  linear_combination h1 - h2

lemma zero : IsExactPotential (fun (_ : Fin M) (_ : Config M N) => (0 : ℝ)) (fun _ => 0) := by
  intro B B' i _; ring

/-- A potential can be shifted by a constant. -/
lemma add_const (hU : IsExactPotential U Φ) (c : ℝ) : IsExactPotential U (fun B => Φ B + c) := by
  intro B B' i h
  rw [hU B B' i h]; ring

/-- Utilities agreeing with an exact-potential profile have the same potential. -/
lemma congr (hU : IsExactPotential U Φ) (hUV : ∀ i B, V i B = U i B) : IsExactPotential V Φ := by
  intro B B' i h
  rw [hUV, hUV]; exact hU B B' i h

end IsExactPotential

namespace IsFlipPotential

variable {U V : Fin M → Config M N → ℝ} {Φ Ψ : Config M N → ℝ}

lemma add (hU : IsFlipPotential U Φ) (hV : IsFlipPotential V Ψ) :
    IsFlipPotential (fun i B => U i B + V i B) (fun B => Φ B + Ψ B) := by
  intro B i j
  have h1 := hU B i j
  have h2 := hV B i j
  linear_combination h1 + h2

lemma smul (c : ℝ) (hU : IsFlipPotential U Φ) :
    IsFlipPotential (fun i B => c * U i B) (fun B => c * Φ B) := by
  intro B i j
  have h1 := hU B i j
  linear_combination c * h1

end IsFlipPotential

/-! ## Building blocks -/

/-- **Own-row terms.** Any actor-indexed function of the actor's own row has the exact potential
`Σ_m g_m(b_m)`. Covers NK fitness (including actor-specific landscapes), density, outAct, XWX,
ego and activity covariates. -/
theorem isExactPotential_own (g : Fin M → (Fin N → Bool) → ℝ) :
    IsExactPotential (fun i B => g i (B i)) (fun B => ∑ m, g m (B m)) := by
  intro B B' i h
  exact (own_sum_diff g h).symm

/-- **Weighted crowding** has the Rosenthal potential `Σ_d v_d n_d (n_d + 1) / 2`. -/
theorem isExactPotential_inPopW (v : Fin N → ℝ) :
    IsExactPotential (fun (i : Fin M) (B : Config M N) => inPopW v B i)
      (fun B => ∑ d, v d * rosenthal (colSum B d)) :=
  fun _ _ _ h => inPopW_diff_eq_rosenthal v h

/-- **Crowding** has the Rosenthal potential `Σ_d n_d (n_d + 1) / 2`. -/
theorem isExactPotential_inPop :
    IsExactPotential (fun i (B : Config M N) => inPop B i)
      (fun B => ∑ d, rosenthal (colSum B d)) :=
  fun _ _ _ h => inPop_diff_eq_rosenthal h

/-- **Symmetric row-pair statistics** have the potential `½ Σ_p Σ_{k≠p} h(b_p, b_k)`. -/
theorem isExactPotential_rowPair (h : (Fin N → Bool) → (Fin N → Bool) → ℝ)
    (hsymm : ∀ b b', h b b' = h b' b) :
    IsExactPotential (fun i (B : Config M N) => rowPairStat h B i) (rowPairPotential h) :=
  fun _ _ _ hu => rowPairStat_exact_potential h hsymm hu

/-- **Symmetric dyadic-overlap statistics** have the potential `dyadPotential`. -/
theorem isExactPotential_dyad (h : Fin M → Fin M → ℝ → ℝ) (hsymm : ∀ p k, h p k = h k p) :
    IsExactPotential (fun i (B : Config M N) => dyadStat h B i) (dyadPotential h) :=
  fun _ _ _ hu => dyadStat_exact_potential h hsymm hu

/-- **The four-cycle count** has the potential `cycle4Potential`, the number of four-cycles. -/
theorem isExactPotential_cycle4 :
    IsExactPotential (fun i (B : Config M N) => cycle4 B i) cycle4Potential :=
  fun _ _ _ hu => cycle4_exact_potential hu

/-! ## The regular SAOM-NK evaluation function -/

/-- A SAOM-NK evaluation function in normal form: an actor-indexed own-row term, activity-weighted
crowding, and a row-pair kernel. Every RSiena two-mode effect the library maps lands in exactly
one of the three slots. -/
structure Spec (M N : ℕ) where
  /-- actor `i`'s own-row term (NK fitness, density, outAct, XWX, ego/activity covariates) -/
  own : Fin M → (Fin N → Bool) → ℝ
  /-- per-activity crowding weights: `θ_inPop` times an activity weight -/
  congestion : Fin N → ℝ
  /-- pairwise kernel between two actors' rows (four-cycles, similarity, ...) -/
  pair : (Fin N → Bool) → (Fin N → Bool) → ℝ

/-- Actor `i`'s evaluation function. -/
def Spec.utility (S : Spec M N) (i : Fin M) (B : Config M N) : ℝ :=
  S.own i (B i) + inPopW S.congestion B i + rowPairStat S.pair B i

/-- The candidate exact potential. -/
def Spec.potential (S : Spec M N) (B : Config M N) : ℝ :=
  (∑ m, S.own m (B m)) + (∑ d, S.congestion d * rosenthal (colSum B d))
    + rowPairPotential S.pair B

/-- The pair kernel is symmetric. -/
def Spec.PairSymmetric (S : Spec M N) : Prop := ∀ b b', S.pair b b' = S.pair b' b

/-- **Exact potential of the regular SAOM-NK evaluation function.** If the pair kernel is
symmetric, `Spec.potential` is an exact potential. The own-row term may differ across actors. -/
theorem Spec.exact_potential (S : Spec M N) (hS : S.PairSymmetric) :
    IsExactPotential S.utility S.potential := by
  have h := ((isExactPotential_own S.own).add (isExactPotential_inPopW S.congestion)).add
    (isExactPotential_rowPair S.pair hS)
  intro B B' i hu
  have := h B B' i hu
  simpa [Spec.utility, Spec.potential] using this

/-! ## The core specification -/

/-- The core specification: NK term, density, outAct, inPop, one coefficient each. -/
structure CoreSpec (M N : ℕ) where
  /-- influence pattern -/
  E : Influence N
  /-- component payoff functions, evaluated on the masked row -/
  f : Fin N → (Fin N → Bool) → ℝ
  θdensity : ℝ
  θoutAct : ℝ
  θinPop : ℝ

/-- Own-row part of the core utility. -/
def CoreSpec.own (S : CoreSpec M N) (b : Fin N → Bool) : ℝ :=
  nkTermFull S.E S.f b + S.θdensity * density b + S.θoutAct * outAct b

/-- The core SAOM-NK utility `U_i(B)`. -/
def CoreSpec.utility (S : CoreSpec M N) (i : Fin M) (B : Config M N) : ℝ :=
  S.own (B i) + S.θinPop * inPop B i

/-- The exact potential `Φ(B) = Σ_i own(b_i) + θ_inPop Σ_d n_d(n_d+1)/2`. -/
def CoreSpec.potential (S : CoreSpec M N) (B : Config M N) : ℝ :=
  (∑ i, S.own (B i)) + S.θinPop * ∑ d, rosenthal (colSum B d)

/-- The core specification as a `Spec`. -/
def CoreSpec.toSpec (S : CoreSpec M N) : Spec M N where
  own := fun _ b => S.own b
  congestion := fun _ => S.θinPop
  pair := fun _ _ => 0

lemma CoreSpec.toSpec_utility (S : CoreSpec M N) (i : Fin M) (B : Config M N) :
    S.toSpec.utility i B = S.utility i B := by
  simp [Spec.utility, CoreSpec.utility, CoreSpec.toSpec, inPopW, inPop, rowPairStat,
    Finset.mul_sum]

lemma CoreSpec.toSpec_potential (S : CoreSpec M N) (B : Config M N) :
    S.toSpec.potential B = S.potential B := by
  simp [Spec.potential, CoreSpec.potential, CoreSpec.toSpec, rowPairPotential, Finset.mul_sum]

/-- **Exact potential of the core specification.** For any actor `i` and any unilateral
deviation `B → B'` (the whole row may change), `U_i(B') - U_i(B) = Φ(B') - Φ(B)`. -/
theorem CoreSpec.exact_potential (S : CoreSpec M N) {B B' : Config M N} {i : Fin M}
    (h : Unilateral B B' i) :
    S.utility i B' - S.utility i B = S.potential B' - S.potential B := by
  have := S.toSpec.exact_potential (fun _ _ => rfl) B B' i h
  simpa [CoreSpec.toSpec_utility, CoreSpec.toSpec_potential] using this

/-- **Actor-indexed own terms.** The own term `g i` may differ across actors (actor-specific
influence matrices, payoff tables, idiosyncratic terms); the potential is the sum of the own terms
plus the Rosenthal congestion term. -/
theorem general_exact_potential (g : Fin M → (Fin N → Bool) → ℝ) (θ : ℝ)
    {B B' : Config M N} {i : Fin M} (h : Unilateral B B' i) :
    (g i (B' i) + θ * inPop B' i) - (g i (B i) + θ * inPop B i)
      = ((∑ m, g m (B' m)) + θ * ∑ d, rosenthal (colSum B' d))
        - ((∑ m, g m (B m)) + θ * ∑ d, rosenthal (colSum B d)) := by
  have h1 := own_sum_diff g h
  have h2 := inPop_diff_eq_rosenthal h
  linear_combination -h1 + θ * h2

/-- **Landscape endogeneity.** If actor `i` holds `d` and a rival `j ≠ i` adds `d`, actor `i`'s
core utility changes by exactly `θ_inPop`. -/
theorem CoreSpec.landscape_endogeneity (S : CoreSpec M N) (B : Config M N) {i j : Fin M}
    (hij : j ≠ i) {d : Fin N} (hi : B i d = true) (hj : B j d = false) :
    S.utility i (flip B j d) - S.utility i B = S.θinPop := by
  unfold CoreSpec.utility
  rw [flip_row_ne B d (Ne.symm hij)]
  have := inPop_rival_add B hij hi hj
  linear_combination S.θinPop * this

end SaomNK

end
