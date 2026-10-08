import SaomNK.Core.Indicator
import Mathlib.Algebra.BigOperators.Group.Finset.Basic
import Mathlib.Algebra.BigOperators.Ring.Finset
import Mathlib.Algebra.Order.BigOperators.Group.Finset
import Mathlib.Data.Fintype.Pi
import Mathlib.Data.Fintype.BigOperators
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.LinearCombination

/-!
# Configurations

A configuration `B : Config M N` is the bipartite incidence matrix of `M` actors (rows) and `N`
activities or components (columns). Row `i` is actor `i`'s portfolio. This is the state space of
the stochastic actor-oriented model (Snijders 2001) on a two-mode network, and for `M = 1` it is
the state space `{0,1}^N` of an NK landscape (Kauffman 1993).

The one structural notion every potential argument rests on is the unilateral deviation:
`Unilateral B B' i` says that only actor `i`'s row may differ.
-/

open Finset

namespace SaomNK

/-- Bipartite configuration: `M` actors (rows), `N` activities (columns). -/
abbrev Config (M N : ℕ) := Fin M → Fin N → Bool

variable {M N : ℕ}

/-- Column sum `n_d(B)`: the number of holders of activity `d`, focal actor included. -/
def colSum (B : Config M N) (d : Fin N) : ℝ := ∑ m, ind (B m d)

/-- Number of holders of `d` other than actor `i`. -/
def othersSum (B : Config M N) (i : Fin M) (d : Fin N) : ℝ :=
  ∑ m ∈ univ.erase i, ind (B m d)

lemma colSum_eq_othersSum_add (B : Config M N) (i : Fin M) (d : Fin N) :
    colSum B d = othersSum B i d + ind (B i d) := by
  unfold colSum othersSum
  rw [← Finset.add_sum_erase univ _ (mem_univ i), add_comm]

/-- `B'` is a unilateral deviation by actor `i` from `B`: every other row is unchanged. -/
def Unilateral (B B' : Config M N) (i : Fin M) : Prop := ∀ m, m ≠ i → B' m = B m

lemma Unilateral.symm {B B' : Config M N} {i : Fin M} (h : Unilateral B B' i) :
    Unilateral B' B i := fun m hm => (h m hm).symm

lemma Unilateral.refl (B : Config M N) (i : Fin M) : Unilateral B B i := fun _ _ => rfl

lemma othersSum_eq_of_unilateral {B B' : Config M N} {i : Fin M}
    (h : Unilateral B B' i) (d : Fin N) : othersSum B' i d = othersSum B i d := by
  unfold othersSum
  refine Finset.sum_congr rfl fun m hm => ?_
  rw [h m (Finset.ne_of_mem_erase hm)]

/-- Under a unilateral deviation by `i`, a sum over actors of row functions changes only
through actor `i`'s own summand. This is the step that makes every own-row term, including
actor-specific ones, part of an exact potential. -/
lemma own_sum_diff (g : Fin M → (Fin N → Bool) → ℝ) {B B' : Config M N} {i : Fin M}
    (h : Unilateral B B' i) :
    (∑ m, g m (B' m)) - (∑ m, g m (B m)) = g i (B' i) - g i (B i) := by
  rw [← Finset.add_sum_erase univ (fun m => g m (B' m)) (mem_univ i),
      ← Finset.add_sum_erase univ (fun m => g m (B m)) (mem_univ i)]
  have : ∑ m ∈ univ.erase i, g m (B' m) = ∑ m ∈ univ.erase i, g m (B m) :=
    Finset.sum_congr rfl fun m hm => by rw [h m (Finset.ne_of_mem_erase hm)]
  rw [this]; ring

/-- The same bookkeeping for any family indexed by actors that agrees off `i`. -/
lemma sum_sub_of_agree {α : Type*} (f : Fin M → α → ℝ) {q q' : Fin M → α} {i : Fin M}
    (h : ∀ k, k ≠ i → q' k = q k) :
    (∑ k, f k (q' k)) - (∑ k, f k (q k)) = f i (q' i) - f i (q i) := by
  rw [← Finset.add_sum_erase univ (fun k => f k (q' k)) (mem_univ i),
      ← Finset.add_sum_erase univ (fun k => f k (q k)) (mem_univ i)]
  have : ∑ k ∈ univ.erase i, f k (q' k) = ∑ k ∈ univ.erase i, f k (q k) :=
    Finset.sum_congr rfl fun k hk => by rw [h k (Finset.ne_of_mem_erase hk)]
  rw [this]; ring

/-- `|{0,1}^{M×N}| = 2^{MN}`. -/
theorem card_config (M N : ℕ) : Fintype.card (Config M N) = 2 ^ (M * N) := by
  simp only [Config, Fintype.card_fun, Fintype.card_bool, Fintype.card_fin]
  ring

/-- For `M ≥ 2` and `N ≥ 1` the multi-actor state space is strictly larger than the single-actor
(NK) state space `{0,1}^N`. -/
theorem card_config_gt (M N : ℕ) (hM : 2 ≤ M) (hN : 1 ≤ N) :
    Fintype.card (Fin N → Bool) < Fintype.card (Config M N) := by
  rw [card_config]
  simp only [Fintype.card_fun, Fintype.card_bool, Fintype.card_fin]
  exact Nat.pow_lt_pow_right (by norm_num) (by nlinarith)

end SaomNK
