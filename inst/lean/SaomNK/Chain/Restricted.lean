import SaomNK.Chain.Balance

/-!
# Restricted chains: structural zeros and ones

A structural mask `free : Fin M → Fin N → Bool` fixes every entry with `free i j = false` at its
value in a reference configuration `B0` (RSiena's structural zeros and ones). The chain then runs
on the feasible set, a subcube of the configuration space.

* `reachable_restricted`: any feasible configuration is reachable from any other by single flips
  of free entries. Forbidding ties shrinks the state space but never disconnects it.
* `detailed_balance_restricted`, `global_balance_restricted`: the Gibbs weight restricted to the
  feasible set is stationary for the restricted generator (fixed entries carry rate zero), for
  any reversible acceptance rule and any exact potential.

The unrestricted results are the special case `free = fun _ _ => true`
(`SaomNK.Chain.Reachable`).
-/

open Finset Real

noncomputable section

namespace SaomNK

variable {M N : ℕ}

/-- Feasible set of a structural mask: entries outside `free` keep their value in `B0`. -/
def Feasible (free : Fin M → Fin N → Bool) (B0 B : Config M N) : Prop :=
  ∀ i j, free i j = false → B i j = B0 i j

/-- One-step relation of the restricted chain: a flip of a free entry. -/
def StepFree (free : Fin M → Fin N → Bool) (B B' : Config M N) : Prop :=
  ∃ i j, free i j = true ∧ B' = flip B i j

lemma feasible_refl (free : Fin M → Fin N → Bool) (B0 : Config M N) : Feasible free B0 B0 :=
  fun _ _ _ => rfl

/-- Flipping a free entry preserves feasibility. -/
lemma feasible_flip {free : Fin M → Fin N → Bool} {B0 B : Config M N}
    (hB : Feasible free B0 B) {i : Fin M} {j : Fin N} (hf : free i j = true) :
    Feasible free B0 (flip B i j) := by
  intro m k hfix
  rw [flip_apply]
  by_cases h : m = i ∧ k = j
  · obtain ⟨rfl, rfl⟩ := h
    simp_all
  · rw [ite_eq_right h]
    exact hB m k hfix

/-- **Reachability on the restricted set.** -/
theorem reachable_restricted (free : Fin M → Fin N → Bool) (B0 : Config M N)
    {B B' : Config M N} (hB : Feasible free B0 B) (hB' : Feasible free B0 B') :
    Relation.ReflTransGen (StepFree free) B B' := by
  suffices h : ∀ n, ∀ B : Config M N, Feasible free B0 B → diffCard B B' = n →
      Relation.ReflTransGen (StepFree free) B B' from h _ B hB rfl
  intro n
  induction n using Nat.strong_induction_on with
  | _ n ih =>
    intro B hBf hn
    by_cases hall : ∀ i j, B i j = B' i j
    · have : B = B' := by funext i j; exact hall i j
      rw [this]
    · simp only [not_forall] at hall
      obtain ⟨i, j, hij⟩ := hall
      have hfree : free i j = true := by
        cases hf : free i j
        · exact absurd ((hBf i j hf).trans (hB' i j hf).symm) hij
        · rfl
      have hcard := diffCard_flip (B' := B') hij
      have hlt : diffCard (flip B i j) B' < n := by omega
      exact Relation.ReflTransGen.head ⟨i, j, hfree, rfl⟩
        (ih _ hlt (flip B i j) (feasible_flip hBf hfree) rfl)

/-- Restricted rate: the flip rate on free entries, zero on fixed ones. -/
def flipRateFree (free : Fin M → Fin N → Bool) (acc : ℝ → ℝ → ℝ) (c : Fin M → Fin N → ℝ)
    (β : ℝ) (U : Fin M → Config M N → ℝ) (B : Config M N) (i : Fin M) (j : Fin N) : ℝ :=
  if free i j = true then flipRate acc c β U B i j else 0

/-- **Detailed balance on the restricted chain.** -/
theorem detailed_balance_restricted (free : Fin M → Fin N → Bool) {acc : ℝ → ℝ → ℝ}
    (hacc : IsReversibleAcceptance acc) {U : Fin M → Config M N → ℝ} {Φ : Config M N → ℝ}
    (hΦ : IsFlipPotential U Φ) (c : Fin M → Fin N → ℝ) (β : ℝ) (B : Config M N) (i : Fin M)
    (j : Fin N) :
    gibbsW β Φ B * flipRateFree free acc c β U B i j
      = gibbsW β Φ (flip B i j) * flipRateFree free acc c β U (flip B i j) i j := by
  unfold flipRateFree
  by_cases hf : free i j = true
  · rw [ite_eq_left hf, ite_eq_left hf]
    exact detailed_balance_of_potential hacc hΦ c β B i j
  · rw [ite_eq_right hf, ite_eq_right hf, mul_zero, mul_zero]

/-- **Stationarity on the restricted chain** (`π Q_F = 0`). -/
theorem global_balance_restricted (free : Fin M → Fin N → Bool) {acc : ℝ → ℝ → ℝ}
    (hacc : IsReversibleAcceptance acc) {U : Fin M → Config M N → ℝ} {Φ : Config M N → ℝ}
    (hΦ : IsFlipPotential U Φ) (c : Fin M → Fin N → ℝ) (β : ℝ) (B' : Config M N) :
    (∑ i, ∑ j, gibbsW β Φ (flip B' i j) * flipRateFree free acc c β U (flip B' i j) i j)
      = gibbsW β Φ B' * ∑ i, ∑ j, flipRateFree free acc c β U B' i j := by
  rw [Finset.mul_sum]
  refine Finset.sum_congr rfl fun i _ => ?_
  rw [Finset.mul_sum]
  refine Finset.sum_congr rfl fun j _ => ?_
  have h := detailed_balance_restricted free hacc hΦ c β (flip B' i j) i j
  rw [flip_flip] at h
  exact h

/-- Every free flip has strictly positive rate under binary-logit acceptance. -/
lemma flipRateFree_glauber_pos {free : Fin M → Fin N → Bool} {c : Fin M → Fin N → ℝ}
    (hc : ∀ i j, 0 < c i j) (β : ℝ) (U : Fin M → Config M N → ℝ) (B : Config M N) {i : Fin M}
    {j : Fin N} (hf : free i j = true) : 0 < flipRateFree free glauber c β U B i j := by
  unfold flipRateFree
  rw [ite_eq_left hf]
  exact flipRate_glauber_pos hc β U B i j

end SaomNK

end
