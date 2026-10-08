import SaomNK.Chain.Balance

/-!
# Creation versus endowment: directional change statistics

RSiena's creation/endowment split (Snijders, van de Bunt and Steglich 2010) evaluates a tie
formation with the creation function and a tie dissolution with the endowment function. With one
evaluation function per actor the change statistic of a drop is the exact sign-mirror of the
change statistic of the add; with the split it need not be. This module says what the split does
to integrability.

* `dir_two_cycle`: around add-then-drop at one entry, the two change statistics sum to the
  creation-minus-endowment difference on that move, whatever the evaluation function.
* `dir_four_cycle`: around add `j`, add `j'`, drop `j`, drop `j'`, they sum to the same
  difference taken over the two-add path.
* `dir_integrability_necessary`: an exact potential forces the creation and endowment change
  statistics to agree on every move (`DirSpec.Agree`).
* `dir_integrability_sufficient`: when they agree, the core potential plus the creation function
  summed over actors is exact.
* `dir_four_cycle_wedge`: under a constant wedge `endowment = creation − w · density`, the
  four-step cycle affinity is exactly `2w`.

Sign convention: `endowment` is whatever function of the row is added to the evaluation
difference on a drop, so RSiena's sign convention for the endowment function is absorbed into
the function.

The recommended practice, which the identities justify, is the disjoint parameterization
`creation + endowment` with no overlapping evaluation term for the same statistic: each
coefficient is then directly interpretable and the integrability condition is visible as
equality of two coefficients.
-/

open Finset

noncomputable section

namespace SaomNK

variable {M N : ℕ}

/-! ## The directional specification -/

/-- The core specification plus RSiena's creation and endowment functions on the actor's own
row. With `creation = endowment = 0` this is `CoreSpec` again. -/
structure DirSpec (M N : ℕ) extends CoreSpec M N where
  /-- evaluated on a tie formation (`0 → 1`) -/
  creation : (Fin N → Bool) → ℝ
  /-- evaluated on a tie dissolution (`1 → 0`) -/
  endowment : (Fin N → Bool) → ℝ

/-- Actor `i`'s change statistic for flipping entry `j`: the evaluation difference, plus the
creation difference on an add (`B i j = false`) or the endowment difference on a drop. -/
def DirSpec.change (S : DirSpec M N) (i : Fin M) (B : Config M N) (j : Fin N) : ℝ :=
  S.toCoreSpec.utility i (flip B i j) - S.toCoreSpec.utility i B
    + if B i j then S.endowment (flip B i j i) - S.endowment (B i)
      else S.creation (flip B i j i) - S.creation (B i)

/-- `Φ` is an exact potential for the directional process: every single-entry change statistic
is the potential difference. -/
def DirSpec.IsExactPotential (S : DirSpec M N) (Φ : Config M N → ℝ) : Prop :=
  ∀ (B : Config M N) (i : Fin M) (j : Fin N), S.change i B j = Φ (flip B i j) - Φ B

/-- Creation and endowment agree in their change statistics on every add. -/
def DirSpec.Agree (S : DirSpec M N) : Prop :=
  ∀ (B : Config M N) (i : Fin M) (j : Fin N), B i j = false →
    S.creation (flip B i j i) - S.creation (B i)
      = S.endowment (flip B i j i) - S.endowment (B i)

/-- Candidate potential: the core potential plus the creation function summed over actors. -/
def DirSpec.potential (S : DirSpec M N) (B : Config M N) : ℝ :=
  S.toCoreSpec.potential B + ∑ m, S.creation (B m)

/-! ## Cycle affinities -/

/-- **Two-step cycle.** Add entry `(i, j)` and drop it again: the evaluation differences cancel
and what survives is the creation-minus-endowment difference on that move. -/
theorem dir_two_cycle (S : DirSpec M N) (i : Fin M) (B : Config M N) (j : Fin N)
    (h : B i j = false) :
    S.change i B j + S.change i (flip B i j) j
      = (S.creation (flip B i j i) - S.creation (B i))
        - (S.endowment (flip B i j i) - S.endowment (B i)) := by
  unfold DirSpec.change
  have h1 : flip B i j i j = true := by rw [flip_self, h]; rfl
  rw [flip_flip, ite_eq_right (by rw [h]; exact Bool.false_ne_true), ite_eq_left h1]
  ring

/-- **Four-step cycle.** Add `j`, add `j'`, drop `j`, drop `j'`: the sum of the four change
statistics is the creation-minus-endowment difference over the two-add path. -/
theorem dir_four_cycle (S : DirSpec M N) (i : Fin M) (B : Config M N) {j j' : Fin N}
    (hjj : j ≠ j') (hj : B i j = false) (hj' : B i j' = false) :
    S.change i B j + S.change i (flip B i j) j'
      + S.change i (flip (flip B i j) i j') j
      + S.change i (flip B i j') j'
      = (S.creation (flip (flip B i j) i j' i) - S.creation (B i))
        - (S.endowment (flip (flip B i j) i j' i) - S.endowment (B i)) := by
  have h1 : flip B i j i j' = false := by
    rw [flip_apply, ite_eq_right (fun h => hjj h.2.symm)]; exact hj'
  have h2 : flip (flip B i j) i j' i j = true := by
    rw [flip_apply, ite_eq_right (fun h => hjj h.2), flip_self, hj]; rfl
  have h3 : flip B i j' i j' = true := by rw [flip_self, hj']; rfl
  have hd1 : flip (flip (flip B i j) i j') i j = flip B i j' := by
    rw [flip_comm B i hjj, flip_flip]
  unfold DirSpec.change
  rw [ite_eq_right (by rw [hj]; exact Bool.false_ne_true),
      ite_eq_right (by rw [h1]; exact Bool.false_ne_true), ite_eq_left h2, ite_eq_left h3, hd1, flip_flip]
  ring

/-! ## Integrability iff creation and endowment agree -/

/-- **Necessity.** An exact potential for the directional process forces creation and endowment
to have the same change statistic on every move. -/
theorem dir_integrability_necessary (S : DirSpec M N) (Φ : Config M N → ℝ)
    (hΦ : S.IsExactPotential Φ) : S.Agree := by
  intro B i j h
  have e1 := hΦ B i j
  have e2 := hΦ (flip B i j) i j
  rw [flip_flip] at e2
  have hc := dir_two_cycle S i B j h
  linarith

/-- **Sufficiency.** When creation and endowment agree, `DirSpec.potential` is exact. -/
theorem dir_integrability_sufficient (S : DirSpec M N) (hA : S.Agree) :
    S.IsExactPotential S.potential := by
  intro B i j
  unfold DirSpec.change DirSpec.potential
  have hu := flip_unilateral B i j
  have h1 := S.toCoreSpec.exact_potential hu
  have h2 : (∑ m, S.creation (flip B i j m)) - (∑ m, S.creation (B m))
      = S.creation (flip B i j i) - S.creation (B i) :=
    own_sum_diff (fun _ b => S.creation b) hu
  by_cases hb : B i j = true
  · rw [ite_eq_left hb]
    have h0 : flip B i j i j = false := by rw [flip_self, hb]; rfl
    have h3 := hA (flip B i j) i j h0
    rw [flip_flip] at h3
    linear_combination h1 - h2 + h3
  · rw [ite_eq_right hb]
    linear_combination h1 - h2

/-- **Constant wedge.** If the endowment function is the creation function less `w` per held
activity, the four-step cycle affinity is exactly `2w`: two drop steps, each carrying the wedge
once. -/
theorem dir_four_cycle_wedge (S : DirSpec M N) (w : ℝ)
    (hw : ∀ b, S.endowment b = S.creation b - w * density b)
    (i : Fin M) (B : Config M N) {j j' : Fin N}
    (hjj : j ≠ j') (hj : B i j = false) (hj' : B i j' = false) :
    S.change i B j + S.change i (flip B i j) j'
      + S.change i (flip (flip B i j) i j') j
      + S.change i (flip B i j') j' = 2 * w := by
  have h1 : flip B i j i j' = false := by
    rw [flip_apply, ite_eq_right (fun h => hjj h.2.symm)]; exact hj'
  have hd : density (flip (flip B i j) i j' i) = density (B i) + 2 := by
    rw [density_flip_add _ _ _ h1, density_flip_add _ _ _ hj]; ring
  rw [dir_four_cycle S i B hjj hj hj', hw, hw, hd]
  ring

end SaomNK

end
