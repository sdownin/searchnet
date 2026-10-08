import SaomNK.Stats.InPop

/-!
# The Rosenthal congestion potential

Rosenthal (1973): in a congestion game the payoff change of a unilateral deviation equals the
change in `Σ_d Σ_{k=1}^{n_d} c_d(k)`. For the linear crowding statistic `inPop`, per activity
`Σ_{k=1}^{n} k = n (n + 1) / 2`, so the potential of `θ · inPop` is `θ Σ_d n_d (n_d + 1) / 2`
(`inPop_diff_eq_rosenthal`). With activity weights `v_d` (`inPopW`) the potential is
`Σ_d v_d n_d (n_d + 1) / 2` (`inPopW_diff_eq_rosenthal`).
-/

open Finset

noncomputable section

namespace SaomNK

variable {M N : ℕ}

/-- Rosenthal potential of a unit-weight linear congestion term on one activity: `n(n+1)/2`. -/
def rosenthal (n : ℝ) : ℝ := n * (n + 1) / 2

/-- Per activity: the Rosenthal finite difference equals the congestion finite difference.
`r` is the number of OTHER holders, `b`/`b'` the focal entry before and after. -/
lemma rosenthal_step (r : ℝ) (b b' : Bool) :
    ind b' * (r + ind b') - ind b * (r + ind b)
      = rosenthal (r + ind b') - rosenthal (r + ind b) := by
  unfold rosenthal
  cases b <;> cases b' <;> simp [ind] <;> ring

/-- **Weighted congestion is a Rosenthal potential game.** -/
lemma inPopW_diff_eq_rosenthal (v : Fin N → ℝ) {B B' : Config M N} {i : Fin M}
    (h : Unilateral B B' i) :
    inPopW v B' i - inPopW v B i
      = (∑ d, v d * rosenthal (colSum B' d)) - ∑ d, v d * rosenthal (colSum B d) := by
  unfold inPopW
  rw [← Finset.sum_sub_distrib, ← Finset.sum_sub_distrib]
  refine Finset.sum_congr rfl fun d _ => ?_
  rw [colSum_eq_othersSum_add B' i d, colSum_eq_othersSum_add B i d,
      othersSum_eq_of_unilateral h d, ← mul_sub, ← mul_sub, rosenthal_step]

/-- **Congestion is a Rosenthal potential game**: `inPop` changes exactly as
`Σ_d n_d (n_d + 1) / 2` does. -/
lemma inPop_diff_eq_rosenthal {B B' : Config M N} {i : Fin M} (h : Unilateral B B' i) :
    inPop B' i - inPop B i
      = (∑ d, rosenthal (colSum B' d)) - ∑ d, rosenthal (colSum B d) := by
  rw [inPop_eq_inPopW, inPop_eq_inPopW, inPopW_diff_eq_rosenthal _ h]
  simp

end SaomNK

end
