import SaomNK.Stats.Density

/-!
# Own-row covariate statistics: `XWX`, ego covariates, alter (activity) covariates

Three statistics that depend only on the actor's own row (and on fixed covariates):

* `xwx W b = Σ_j Σ_h b_j W_jh b_h`: the two-path statistic through a component-by-component
  dyadic covariate `W`, RSiena's `XWX` on a two-mode network. With `W` the influence or synergy
  matrix of the activities, it is the bilinear complementarity term `b' W b`. Whether the
  diagonal of `W` counts is a convention of the caller: zero it to exclude `j = h`.
* `egoStat z i b = z_i · Σ_j b_j`: an actor covariate times the outdegree (RSiena's `egoX`).
* `altStat v b = Σ_j v_j b_j`: an activity covariate summed over held activities (RSiena's
  `altX` on a two-mode network, before centering).

Each is an own-row (possibly actor-indexed) term, so each is part of an exact potential by
`own_sum_diff` with no further argument. Centering a covariate adds a multiple of the density,
which is again own-row.

Note the contrast with a one-mode relational layer: there an ego covariate on the degree admits
no common potential (`egoCovariate_not_ownAPotential` in `SaomNK.TwoLayer.Integrability`),
because both endpoints of a tie evaluate it.
-/

open Finset

namespace SaomNK

variable {M N : ℕ}

/-- Bilinear two-path statistic through a dyadic component covariate: `b' W b`. -/
def xwx (W : Fin N → Fin N → ℝ) (b : Fin N → Bool) : ℝ := ∑ j, ∑ h, ind (b j) * W j h * ind (b h)

/-- Ego covariate times outdegree. -/
def egoStat (z : Fin M → ℝ) (i : Fin M) (b : Fin N → Bool) : ℝ := z i * density b

/-- Activity covariate summed over held activities. -/
def altStat (v : Fin N → ℝ) (b : Fin N → Bool) : ℝ := ∑ j, v j * ind (b j)

/-- `xwx` with a symmetrized matrix is the same statistic: only `W + Wᵀ` matters. -/
lemma xwx_symmetrize (W : Fin N → Fin N → ℝ) (b : Fin N → Bool) :
    xwx W b = xwx (fun j h => (W j h + W h j) / 2) b := by
  unfold xwx
  have h1 : (∑ j, ∑ h, ind (b j) * W h j * ind (b h)) = ∑ j, ∑ h, ind (b j) * W j h * ind (b h) := by
    rw [Finset.sum_comm]
    exact Finset.sum_congr rfl fun j _ => Finset.sum_congr rfl fun h _ => by ring
  have h2 : (∑ j, ∑ h, ind (b j) * ((W j h + W h j) / 2) * ind (b h))
      = (1 / 2) * ((∑ j, ∑ h, ind (b j) * W j h * ind (b h))
          + ∑ j, ∑ h, ind (b j) * W h j * ind (b h)) := by
    rw [← Finset.sum_add_distrib, Finset.mul_sum]
    refine Finset.sum_congr rfl fun j _ => ?_
    rw [← Finset.sum_add_distrib, Finset.mul_sum]
    exact Finset.sum_congr rfl fun h _ => by ring
  rw [h2, h1]; ring

/-- With the identity matrix, `xwx` is the density (`ind` is idempotent). -/
lemma xwx_diag (b : Fin N → Bool) :
    xwx (fun j h => if j = h then 1 else 0) b = density b := by
  unfold xwx density
  refine Finset.sum_congr rfl fun j _ => ?_
  rw [Finset.sum_eq_single j]
  · simp [ind_mul_self]
  · intro h _ hj; simp [Ne.symm hj]
  · intro h; exact absurd (mem_univ j) h

/-- `altStat` with a constant covariate is that constant times the density. -/
lemma altStat_const (c : ℝ) (b : Fin N → Bool) : altStat (fun _ => c) b = c * density b := by
  simp [altStat, density, Finset.mul_sum]

end SaomNK
