import SaomNK.Compute.RatSpec
import SaomNK.Potential.Nash
import SaomNK.NK.Table
import Mathlib.Data.Rat.BigOperators

/-!
# From rational computation to the real-valued theory

`RatSpec.toSpec` reads a concrete rational model as a `Spec` of the general theory, and
`RatSpec.cast_utility` / `cast_potential` show that the computable utility and potential are the
casts of the general ones. Consequences, for every concrete model:

* `RatSpec.exact_potentialQ`: the computable potential is an exact potential, in `ℚ`;
* `RatSpec.exists_nash`: a pure-strategy Nash equilibrium exists;
* `RatSpec.nkQ_eq_nkFitness`: if the contribution table is consistent with the influence
  pattern (a decidable check), the computable NK term is NK fitness in the sense of
  `SaomNK.NK.Fitness`.
-/

open Finset

noncomputable section

namespace SaomNK

variable {M N : ℕ}

@[simp] lemma cast_indQ (b : Bool) : ((indQ b : ℚ) : ℝ) = ind b := by
  cases b <;> simp [indQ, ind]

@[simp] lemma cast_densityQ (b : Fin N → Bool) : ((densityQ b : ℚ) : ℝ) = density b := by
  simp [densityQ, density]

@[simp] lemma cast_colSumQ (B : Config M N) (d : Fin N) :
    ((colSumQ B d : ℚ) : ℝ) = colSum B d := by
  simp [colSumQ, colSum]

@[simp] lemma cast_inPopQ (B : Config M N) (i : Fin M) : ((inPopQ B i : ℚ) : ℝ) = inPop B i := by
  simp [inPopQ, inPop]

@[simp] lemma cast_overlapQ (b b' : Fin N → Bool) : ((overlapQ b b' : ℚ) : ℝ) = overlapK b b' := by
  simp [overlapQ, overlapK]

@[simp] lemma cast_cycle4Q (B : Config M N) (i : Fin M) : ((cycle4Q B i : ℚ) : ℝ) = cycle4 B i := by
  simp [cycle4Q, cycle4, dyadStat, overlap_eq_overlapK]

lemma flipQ_eq_flip (B : Config M N) (i : Fin M) (j : Fin N) : flipQ B i j = flip B i j := rfl

/-- The real-valued table of a rational spec, indexed by natural codes. -/
def RatSpec.CReal (S : RatSpec M N) : ℕ → Fin N → ℝ :=
  fun c d => if h : c < 2 ^ N then (S.C ⟨c, h⟩ d : ℝ) else 0

/-- The general specification a concrete model denotes. -/
def RatSpec.toSpec (S : RatSpec M N) : Spec M N where
  own := fun i b => (S.ownQ i b : ℝ)
  congestion := fun _ => (S.θinPop : ℝ)
  pair := fun b b' => (S.θcycle4 : ℝ) * (overlapK b b' * (overlapK b b' - 1) / 2)

lemma RatSpec.toSpec_pairSymmetric (S : RatSpec M N) : S.toSpec.PairSymmetric := by
  intro b b'
  simp only [RatSpec.toSpec, overlapK_comm b b']

/-- **The computable utility is the cast of the general utility.** -/
theorem RatSpec.cast_utility (S : RatSpec M N) (i : Fin M) (B : Config M N) :
    ((S.utilityQ i B : ℚ) : ℝ) = S.toSpec.utility i B := by
  have h1 : inPopW (fun _ => (S.θinPop : ℝ)) B i = (S.θinPop : ℝ) * inPop B i := by
    simp [inPopW, inPop, Finset.mul_sum]
  have h2 : rowPairStat (fun b b' => (S.θcycle4 : ℝ) * (overlapK b b' * (overlapK b b' - 1) / 2))
      B i = (S.θcycle4 : ℝ) * cycle4 B i := by
    rw [cycle4_eq_rowPairStat, rowPairStat, rowPairStat, Finset.mul_sum]
  simp only [RatSpec.utilityQ, Spec.utility, RatSpec.toSpec, h1, h2]
  push_cast [cast_inPopQ, cast_cycle4Q]
  ring

/-- **The computable potential is the cast of the general potential.** -/
theorem RatSpec.cast_potential (S : RatSpec M N) (B : Config M N) :
    ((S.potentialQ B : ℚ) : ℝ) = S.toSpec.potential B := by
  have h2 : rowPairPotential
      (fun b b' => (S.θcycle4 : ℝ) * (overlapK b b' * (overlapK b b' - 1) / 2)) B
      = (S.θcycle4 : ℝ) * ((1 / 2) * ∑ p, ∑ k ∈ univ.erase p,
          overlapK (B p) (B k) * (overlapK (B p) (B k) - 1) / 2) := by
    simp only [rowPairPotential, Finset.mul_sum]
    refine Finset.sum_congr rfl fun p _ => Finset.sum_congr rfl fun k _ => ?_
    ring
  have hc : ((∑ d, colSumQ B d * (colSumQ B d + 1) / 2 : ℚ) : ℝ)
      = ∑ d, rosenthal (colSum B d) := by
    push_cast; simp [rosenthal]
  have hp : ((∑ p, ∑ k ∈ univ.erase p,
        overlapQ (B p) (B k) * (overlapQ (B p) (B k) - 1) / 2 : ℚ) : ℝ)
      = ∑ p, ∑ k ∈ univ.erase p, overlapK (B p) (B k) * (overlapK (B p) (B k) - 1) / 2 := by
    push_cast; simp
  have ho : ((∑ m, S.ownQ m (B m) : ℚ) : ℝ) = ∑ m, (S.ownQ m (B m) : ℝ) := Rat.cast_sum _ _
  simp only [RatSpec.potentialQ, Spec.potential, RatSpec.toSpec, Rat.cast_add, Rat.cast_mul,
    hc, hp, ho, h2, Rat.cast_div, Rat.cast_one, Rat.cast_ofNat]
  rw [Finset.mul_sum]

/-- **Exact potential of every concrete model, in `ℚ`.** -/
theorem RatSpec.exact_potentialQ (S : RatSpec M N) {B B' : Config M N} {i : Fin M}
    (h : Unilateral B B' i) :
    S.utilityQ i B' - S.utilityQ i B = S.potentialQ B' - S.potentialQ B := by
  have := S.toSpec.exact_potential S.toSpec_pairSymmetric B B' i h
  rw [← S.cast_utility, ← S.cast_utility, ← S.cast_potential, ← S.cast_potential] at this
  exact_mod_cast this

lemma RatSpec.isFlipPotential (S : RatSpec M N) :
    IsFlipPotential S.toSpec.utility S.toSpec.potential :=
  (S.toSpec.exact_potential S.toSpec_pairSymmetric).toFlip

/-- Every concrete model has a pure-strategy Nash equilibrium. -/
theorem RatSpec.exists_nash (S : RatSpec M N) : ∃ B, IsNash S.toSpec.utility B :=
  S.toSpec.exists_nash S.toSpec_pairSymmetric

/-- **Consistent tables give NK fitness.** -/
theorem RatSpec.nkQ_eq_nkFitness (S : RatSpec M N) (hC : S.TableConsistentQ)
    (b : Fin N → Bool) :
    ((nkQ S.C b : ℚ) : ℝ) = nkFitness S.E (tablePayoff S.CReal) b := by
  have hcons : TableConsistent S.E S.CReal := by
    refine tableConsistent_of_codes fun c d => ?_
    simp only [RatSpec.CReal, codeLSB_lt, dite_true, c.isLt]
    exact_mod_cast hC c d
  rw [nkFitness, nkTermFull_tableLookup hcons]
  simp only [nkQ, tableRowSum, RatSpec.CReal, codeLSB_lt, dite_true]
  push_cast
  ring

end SaomNK

end
