import SaomNK.Core.Flip

/-!
# Density and outdegree activity

The two portfolio-size statistics of a bipartite SAOM, as functions of the actor's own row:
`density b = Σ_j b_j` (the outdegree) and `outAct b = (Σ_j b_j)^2` (RSiena's `outAct`, outdegree
activity). Both are own-row statistics, so each is part of an exact potential by
`own_sum_diff`; the flip formulas below are what a change statistic reduces to.

Sign convention, used throughout the library: coefficients carry their own sign. A scope cost
written `-θ (Σ_j b_j)^2` elsewhere is `θ' * outAct b` here with `θ' = -θ`.
-/

open Finset

namespace SaomNK

variable {M N : ℕ}

/-- Density: portfolio size (outdegree). -/
def density (b : Fin N → Bool) : ℝ := ∑ j, ind (b j)

/-- Outdegree activity: squared portfolio size. -/
def outAct (b : Fin N → Bool) : ℝ := (∑ j, ind (b j)) ^ 2

lemma outAct_eq_density_sq (b : Fin N → Bool) : outAct b = density b ^ 2 := rfl

lemma density_nonneg (b : Fin N → Bool) : 0 ≤ density b :=
  Finset.sum_nonneg fun _ _ => ind_nonneg _

lemma density_le (b : Fin N → Bool) : density b ≤ N := by
  unfold density
  calc ∑ j, ind (b j) ≤ ∑ _j : Fin N, (1 : ℝ) := Finset.sum_le_sum fun _ _ => ind_le_one _
    _ = N := by simp

/-- A flip changes the flipping actor's density by `+1` (add) or `-1` (drop). -/
lemma density_flip_self (B : Config M N) (i : Fin M) (n : Fin N) :
    density (flip B i n i) - density (B i) = if B i n then -1 else 1 := by
  unfold density
  rw [← Finset.sum_sub_distrib, ← Finset.add_sum_erase univ _ (mem_univ n)]
  rw [Finset.sum_eq_zero (fun j hj => by
      have hjn : j ≠ n := Finset.ne_of_mem_erase hj
      simp [flip_apply, hjn]), add_zero]
  rw [flip_apply]
  cases h : B i n <;> simp [ind]

/-- Adding an unheld activity raises the row's density by one. -/
lemma density_flip_add (B : Config M N) (i : Fin M) (j : Fin N) (h : B i j = false) :
    density (flip B i j i) = density (B i) + 1 := by
  have := density_flip_self B i j
  rw [h] at this
  simp only [Bool.false_eq_true, ite_false] at this
  linarith

/-- Dropping a held activity lowers the row's density by one. -/
lemma density_flip_drop (B : Config M N) (i : Fin M) (j : Fin N) (h : B i j = true) :
    density (flip B i j i) = density (B i) - 1 := by
  have := density_flip_self B i j
  rw [h] at this
  simp only [ite_true] at this
  linarith

/-- The change in `outAct` on a flip: `±(2 · density + 1)` on an add, `-(2 · density - 1)` on a
drop. -/
lemma outAct_flip_self (B : Config M N) (i : Fin M) (n : Fin N) :
    outAct (flip B i n i) - outAct (B i)
      = if B i n then 1 - 2 * density (B i) else 2 * density (B i) + 1 := by
  rw [outAct_eq_density_sq, outAct_eq_density_sq]
  cases h : B i n
  · rw [density_flip_add B i n h]; simp; ring
  · rw [density_flip_drop B i n h]; simp; ring

end SaomNK
