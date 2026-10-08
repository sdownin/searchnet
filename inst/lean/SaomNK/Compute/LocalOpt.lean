import SaomNK.Compute.Cast
import Mathlib.SetTheory.Cardinal.Finite

/-!
# Counting local optima by evaluation, with a proof that the count means what it says

`RatSpec.countLocalOpt` enumerates the `2^(MN)` configuration codes and counts those at which no
actor gains from a single flip. This module proves that the number so computed is the number of
flip-stable configurations of the general model (`countLocalOpt_eq_card`), and that those are
exactly the local maxima of the exact potential on the hypercube. So `countLocalOpt S = k`,
proved by `decide +kernel` for a concrete model, is a theorem about that model's equilibria.
-/

open Finset

noncomputable section

namespace SaomNK

variable {M N : ℕ}

/-- The Boolean check is single-flip stability of the general model. -/
theorem RatSpec.isLocalOptB_iff (S : RatSpec M N) (B : Config M N) :
    S.isLocalOptB B = true ↔ IsFlipStable S.toSpec.utility B := by
  unfold RatSpec.isLocalOptB IsFlipStable
  simp only [List.all_eq_true, List.mem_finRange, true_implies, decide_eq_true_eq]
  refine forall_congr' fun i => forall_congr' fun j => ?_
  rw [flipQ_eq_flip, ← S.cast_utility, ← S.cast_utility, Rat.cast_le]

/-- ... equivalently, local maximality of the exact potential. -/
theorem RatSpec.isLocalOptB_iff_localMax (S : RatSpec M N) (B : Config M N) :
    S.isLocalOptB B = true ↔ ∀ (i : Fin M) (j : Fin N), S.potentialQ (flip B i j) ≤ S.potentialQ B := by
  rw [S.isLocalOptB_iff, flipStable_iff_localMax S.isFlipPotential]
  refine forall_congr' fun i => forall_congr' fun j => ?_
  rw [← S.cast_potential, ← S.cast_potential, Rat.cast_le]

/-- Configurations indexed by codes: `Fin (2^(MN)) ≃ Config M N`, entry `(i, j)` at bit
`j + N·i`. -/
def cfgEquiv (M N : ℕ) : Fin (2 ^ (M * N)) ≃ Config M N :=
  ((codeEquiv (M * N)).symm.trans
    (Equiv.arrowCongr finProdFinEquiv.symm (Equiv.refl Bool))).trans
    (Equiv.curry (Fin M) (Fin N) Bool)

lemma cfgEquiv_apply (c : Fin (2 ^ (M * N))) : cfgEquiv M N c = cfgOfCode M N c := rfl

lemma length_filter_range (n : ℕ) (p : ℕ → Bool) :
    ((List.range n).filter p).length = Fintype.card {c : Fin n // p c = true} := by
  rw [Fintype.card_subtype]
  have hr : Finset.range n = (univ : Finset (Fin n)).map Fin.valEmbedding := by
    ext x
    simp only [Finset.mem_range, Finset.mem_map, Finset.mem_univ, true_and,
      Fin.valEmbedding_apply]
    exact ⟨fun h => ⟨⟨x, h⟩, rfl⟩, fun ⟨a, ha⟩ => ha ▸ a.isLt⟩
  have h1 : ((List.range n).filter p).length = ((Finset.range n).filter (fun c => p c = true)).card := by
    rw [Finset.card_def, Finset.filter_val, Finset.range_val, Multiset.range,
      Multiset.filter_coe, Multiset.coe_card]
    simp
  rw [h1, hr, Finset.filter_map, Finset.card_map]
  rfl

/-- **The computed count is the number of flip-stable configurations.** -/
theorem RatSpec.countLocalOpt_eq_card (S : RatSpec M N) :
    S.countLocalOpt = Nat.card {B : Config M N // IsFlipStable S.toSpec.utility B} := by
  unfold RatSpec.countLocalOpt
  rw [length_filter_range, ← Nat.card_eq_fintype_card]
  refine Nat.card_congr ((cfgEquiv M N).subtypeEquiv fun c => ?_)
  rw [cfgEquiv_apply, ← S.isLocalOptB_iff]

/-- A positive count certifies a flip-stable configuration (and the general theory guarantees
one exists, so the count is always positive). -/
theorem RatSpec.countLocalOpt_pos (S : RatSpec M N) : 0 < S.countLocalOpt := by
  rw [S.countLocalOpt_eq_card]
  obtain ⟨B, hB⟩ := S.exists_nash
  have : Nonempty {B : Config M N // IsFlipStable S.toSpec.utility B} := ⟨⟨B, hB.flipStable⟩⟩
  exact Nat.card_pos

end SaomNK

end
