import SaomNK.Potential.Exact

/-!
# Pure Nash equilibria and single-flip stability

For any utility profile with an exact potential (Monderer and Shapley 1996): a maximizer of the
potential is a pure-strategy Nash equilibrium (`nash_of_potential_max`), and since the
configuration space is finite one exists (`exists_nash_of_exactPotential`).

The single-flip analogue matters for SAOMs, whose ministeps change one tie: a configuration is
*flip-stable* when no actor gains from toggling one entry. With a flip potential, flip-stable
configurations are exactly the local maxima of the potential on the hypercube
(`flipStable_iff_localMax`). So counting the local optima of a SAOM-NK game is counting local
maxima of one function, which is what `SaomNK.Compute.LocalOpt` computes for concrete models.
-/

open Finset

noncomputable section

namespace SaomNK

variable {M N : ℕ}

/-- Pure-strategy Nash equilibrium: no actor gains from any unilateral deviation. -/
def IsNash (U : Fin M → Config M N → ℝ) (B : Config M N) : Prop :=
  ∀ (i : Fin M) (B' : Config M N), Unilateral B B' i → U i B' ≤ U i B

/-- Flip stability: no actor gains from toggling a single entry. -/
def IsFlipStable (U : Fin M → Config M N → ℝ) (B : Config M N) : Prop :=
  ∀ (i : Fin M) (j : Fin N), U i (flip B i j) ≤ U i B

lemma IsNash.flipStable {U : Fin M → Config M N → ℝ} {B : Config M N} (h : IsNash U B) :
    IsFlipStable U B :=
  fun i j => h i _ (flip_unilateral B i j)

/-- A maximizer of an exact potential is a pure-strategy Nash equilibrium. -/
theorem nash_of_potential_max {U : Fin M → Config M N → ℝ} {Φ : Config M N → ℝ}
    (hΦ : IsExactPotential U Φ) (B : Config M N) (hmax : ∀ B', Φ B' ≤ Φ B) : IsNash U B := by
  intro i B' h
  have := hΦ B B' i h
  linarith [hmax B']

/-- **Existence of a pure-strategy Nash equilibrium** for any exact potential game on the finite
configuration space. -/
theorem exists_nash_of_exactPotential {U : Fin M → Config M N → ℝ} {Φ : Config M N → ℝ}
    (hΦ : IsExactPotential U Φ) : ∃ B : Config M N, IsNash U B := by
  obtain ⟨B, -, hB⟩ := Finset.exists_max_image (univ : Finset (Config M N)) Φ
    ⟨fun _ _ => false, mem_univ _⟩
  exact ⟨B, nash_of_potential_max hΦ B fun B' => hB B' (mem_univ _)⟩

/-- **Flip-stable configurations are the local maxima of the potential.** -/
theorem flipStable_iff_localMax {U : Fin M → Config M N → ℝ} {Φ : Config M N → ℝ}
    (hΦ : IsFlipPotential U Φ) (B : Config M N) :
    IsFlipStable U B ↔ ∀ (i : Fin M) (j : Fin N), Φ (flip B i j) ≤ Φ B := by
  constructor
  · intro h i j; have := hΦ B i j; linarith [h i j]
  · intro h i j; have := hΦ B i j; linarith [h i j]

/-- The regular SAOM-NK evaluation function has a pure-strategy Nash equilibrium. -/
theorem Spec.exists_nash (S : Spec M N) (hS : S.PairSymmetric) : ∃ B, IsNash S.utility B :=
  exists_nash_of_exactPotential (S.exact_potential hS)

/-- The core specification has a pure-strategy Nash equilibrium. -/
theorem CoreSpec.exists_pure_nash (S : CoreSpec M N) :
    ∃ B : Config M N, ∀ (i : Fin M) (B' : Config M N),
      Unilateral B B' i → S.utility i B' ≤ S.utility i B := by
  have hΦ : IsExactPotential S.utility S.potential := fun _ _ _ h => S.exact_potential h
  exact exists_nash_of_exactPotential hΦ

end SaomNK

end
