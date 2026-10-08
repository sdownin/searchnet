import SaomNK.NK.Influence
import Mathlib.Tactic.FieldSimp

/-!
# NK fitness

The NK term of a SAOM-NK evaluation function is `Σ_d f_d(b ⊙ E_d)`: every component contributes
its payoff, evaluated on the actor's row masked by the component's influence row, whether or not
the actor holds the component. NK fitness (Kauffman 1993; Levinthal 1997) is its mean,
`W(x) = (1/N) Σ_d f_d(x ⊙ E_d)`.

A second reading, in which a component contributes only when it is held, is the special case in
which each payoff vanishes when its own component is off (`heldOnly_eq_full_of_vanishing`).

`dummy_reconstruction` and `nkTermFull_in_dummy_span` say that any NK term is an exact linear
combination of configuration dummies, so a SAOM with dummy covariates reproduces any NK
landscape exactly.
-/

open Finset

namespace SaomNK

variable {N : ℕ}

/-- The NK term: `Σ_d f_d(b ⊙ E_d)`. -/
def nkTermFull (E : Influence N) (f : Fin N → (Fin N → Bool) → ℝ) (b : Fin N → Bool) : ℝ :=
  ∑ d, f d (mask b (E d))

/-- NK fitness: `W(x) = (1/N) Σ_d f_d(x ⊙ E_d)`. -/
noncomputable def nkFitness (E : Influence N) (f : Fin N → (Fin N → Bool) → ℝ)
    (x : Fin N → Bool) : ℝ :=
  (1 / (N : ℝ)) * nkTermFull E f x

/-- The held-only NK term: a component contributes only when it is held. -/
def nkTermHeldOnly (E : Influence N) (f : Fin N → (Fin N → Bool) → ℝ) (b : Fin N → Bool) : ℝ :=
  ∑ d, ind (b d) * f d (mask b (E d))

/-- The NK term is `N` times NK fitness. -/
theorem nkTermFull_eq_mul_nkFitness (hN : 0 < N) (E : Influence N)
    (f : Fin N → (Fin N → Bool) → ℝ) (x : Fin N → Bool) :
    nkTermFull E f x = (N : ℝ) * nkFitness E f x := by
  unfold nkFitness
  have : (N : ℝ) ≠ 0 := by exact_mod_cast (ne_of_gt hN)
  field_simp

/-- **Held-only reading.** If every component payoff vanishes when its own component is off,
the held-only term equals the full NK term. -/
theorem heldOnly_eq_full_of_vanishing (E : Influence N)
    (f : Fin N → (Fin N → Bool) → ℝ) (hf : ∀ d y, y d = false → f d y = 0)
    (b : Fin N → Bool) : nkTermHeldOnly E f b = nkTermFull E f b := by
  unfold nkTermHeldOnly nkTermFull
  refine Finset.sum_congr rfl fun d _ => ?_
  cases hb : b d
  · rw [hf d _ (by simp [hb])]; simp
  · simp

/-- The NK term sees a row only through its masks: two rows that agree on every neighborhood
have the same NK term. -/
theorem nkTermFull_congr (E : Influence N) (f : Fin N → (Fin N → Bool) → ℝ)
    {b b' : Fin N → Bool} (h : ∀ d j, E d j = true → b j = b' j) :
    nkTermFull E f b = nkTermFull E f b' := by
  unfold nkTermFull
  refine Finset.sum_congr rfl fun d _ => ?_
  congr 1
  funext j
  simp only [mask_apply]
  cases hE : E d j
  · simp
  · simp [h d j hE]

/-- **Dummy reconstruction.** Any function on a finite configuration type is an exact linear
combination of configuration dummies `w_c(y) = 1[y = c]` with coefficients `f(c)`. -/
theorem dummy_reconstruction {S : Type*} [Fintype S] [DecidableEq S] (f : S → ℝ) (y : S) :
    f y = ∑ c, f c * (if y = c then 1 else 0) := by
  simp

/-- The number of dummies per component is `2^{K+1}` for a neighborhood of size `K+1`. -/
theorem card_neighborhood_configs (K : ℕ) :
    Fintype.card (Fin (K + 1) → Bool) = 2 ^ (K + 1) := by
  simp [Fintype.card_bool, Fintype.card_fin]

/-- The NK term lies in the span of the masked-configuration dummies: a SAOM whose evaluation
function carries one dummy covariate per (component, neighborhood pattern) reproduces any NK
landscape exactly. -/
theorem nkTermFull_in_dummy_span (E : Influence N) (f : Fin N → (Fin N → Bool) → ℝ)
    (x : Fin N → Bool) :
    nkTermFull E f x
      = ∑ d, ∑ c : Fin N → Bool, f d c * (if mask x (E d) = c then 1 else 0) := by
  unfold nkTermFull
  exact Finset.sum_congr rfl fun d _ => dummy_reconstruction (f d) _

end SaomNK
