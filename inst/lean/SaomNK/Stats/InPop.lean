import SaomNK.Stats.Density

/-!
# Indegree popularity (crowding)

`inPop B i = Σ_j b_ij n_j(B)`: the activities actor `i` holds, each weighted by its number of
holders, focal actor included (RSiena's `inPop` on a two-mode network). It is a congestion
statistic: it is the only core statistic that couples actors, and its exact potential is the
Rosenthal (1973) congestion potential (`SaomNK.Potential.Rosenthal`).

`inPopW v` is the activity-weighted generalization `Σ_j v_j b_ij n_j(B)`: activity-specific
crowding intensities. `inPop` is `inPopW 1`.
-/

open Finset

namespace SaomNK

variable {M N : ℕ}

/-- Crowding / inPop: held activities weighted by their number of holders. -/
def inPop (B : Config M N) (i : Fin M) : ℝ := ∑ j, ind (B i j) * colSum B j

/-- Activity-weighted crowding: `Σ_j v_j b_ij n_j(B)`. -/
def inPopW (v : Fin N → ℝ) (B : Config M N) (i : Fin M) : ℝ :=
  ∑ j, v j * (ind (B i j) * colSum B j)

lemma inPop_eq_inPopW (B : Config M N) (i : Fin M) : inPop B i = inPopW (fun _ => 1) B i := by
  simp [inPop, inPopW]

/-- The "+1" variant `Σ_j b_ij (n_j + 1)` differs from `inPop` by the density, so it is absorbed
into the own-row part of any specification. -/
lemma inPop_plus_one (B : Config M N) (i : Fin M) :
    (∑ j, ind (B i j) * (colSum B j + 1)) = inPop B i + density (B i) := by
  unfold inPop density
  rw [← Finset.sum_add_distrib]
  refine Finset.sum_congr rfl fun j _ => ?_
  ring

lemma inPop_eq_sum_others_add_one (B : Config M N) (i : Fin M) :
    inPop B i = ∑ j, ind (B i j) * (othersSum B i j + 1) := by
  unfold inPop
  refine Finset.sum_congr rfl fun j _ => ?_
  rw [colSum_eq_othersSum_add B i j]
  cases h : B i j <;> simp [ind]

/-- An own flip changes `inPop` by `±(n_co + 1)`, `n_co` the number of rivals holding the
activity. -/
lemma inPop_flip_self (B : Config M N) (i : Fin M) (n : Fin N) :
    inPop (flip B i n) i - inPop B i = (if B i n then -1 else 1) * (othersSum B i n + 1) := by
  rw [inPop_eq_sum_others_add_one, inPop_eq_sum_others_add_one]
  have ho : ∀ j, othersSum (flip B i n) i j = othersSum B i j :=
    fun j => othersSum_eq_of_unilateral (flip_unilateral B i n) j
  simp only [ho]
  rw [← Finset.sum_sub_distrib, ← Finset.add_sum_erase univ _ (mem_univ n)]
  rw [Finset.sum_eq_zero (fun j hj => by
      have hjn : j ≠ n := Finset.ne_of_mem_erase hj
      simp [flip_apply, hjn]), add_zero]
  rw [flip_apply]
  cases h : B i n <;> simp [ind]

/-- A rival's flip of `d` changes every holder's column count of `d` by `±1` and no other. -/
lemma colSum_flip_rival (B : Config M N) (j : Fin M) (d k : Fin N) :
    colSum (flip B j d) k - colSum B k = if k = d then (if B j d then -1 else 1) else 0 := by
  unfold colSum
  rw [← Finset.sum_sub_distrib]
  by_cases hk : k = d
  · subst hk
    rw [Finset.sum_eq_single j]
    · cases h : B j k <;> simp [h, ind]
    · intro m _ hm; simp [flip_apply, hm]
    · intro h; exact absurd (mem_univ j) h
  · simp [flip_apply, hk]

/-- **Landscape endogeneity, statistic form.** If actor `i` holds `d` and a rival `j ≠ i`
adds `d`, actor `i`'s crowding rises by exactly one: rivals' moves reshape the focal actor's
payoff landscape. -/
theorem inPop_rival_add (B : Config M N) {i j : Fin M} (hij : j ≠ i) {d : Fin N}
    (hi : B i d = true) (hj : B j d = false) :
    inPop (flip B j d) i - inPop B i = 1 := by
  unfold inPop
  simp only [flip_row_ne B d (Ne.symm hij)]
  rw [← Finset.sum_sub_distrib]
  calc (∑ k, (ind (B i k) * colSum (flip B j d) k - ind (B i k) * colSum B k))
      = ∑ k, ind (B i k) * (if k = d then 1 else 0) := by
        refine Finset.sum_congr rfl fun k _ => ?_
        rw [← mul_sub, colSum_flip_rival, hj]
        simp
    _ = 1 := by simp [hi]

end SaomNK
