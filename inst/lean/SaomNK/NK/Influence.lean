import SaomNK.Core.Config
import Mathlib.Data.Fintype.Card
import Mathlib.Data.Finset.Card

/-!
# Influence patterns

The influence matrix `E : Influence N` of an NK landscape (Kauffman 1993): `E d j = true` iff
component `j` enters the payoff of component `d`. Row `d` is the epistatic neighborhood of `d`.

`IsKRegular E K` is the classical NK restriction: every component depends on itself and on
exactly `K` others. It is decidable, so a concrete influence matrix exported from R can be
checked against its nominal `K` by `decide`.
-/

open Finset

namespace SaomNK

/-- Influence pattern of an NK landscape: `E d j` iff `j` enters `f_d`. -/
abbrev Influence (N : ℕ) := Fin N → Fin N → Bool

variable {N : ℕ}

/-- Componentwise mask of a row by an influence row: `(b ⊙ e)_j = b_j && e_j`. -/
def mask (b e : Fin N → Bool) : Fin N → Bool := fun j => b j && e j

@[simp] lemma mask_apply (b e : Fin N → Bool) (j : Fin N) : mask b e j = (b j && e j) := rfl

@[simp] lemma mask_mask (b e : Fin N → Bool) : mask (mask b e) e = mask b e := by
  funext j; simp [mask]

/-- Neighborhood of component `d`: the components that enter `f_d`, itself included. -/
def neighborhood (E : Influence N) (d : Fin N) : Finset (Fin N) := univ.filter fun j => E d j = true

/-- The classical NK restriction: every component depends on itself and on exactly `K` other
components. -/
def IsKRegular (E : Influence N) (K : ℕ) : Prop :=
  ∀ d, E d d = true ∧ (neighborhood E d).card = K + 1

instance (E : Influence N) (K : ℕ) : Decidable (IsKRegular E K) := by
  unfold IsKRegular neighborhood; infer_instance

lemma mem_neighborhood {E : Influence N} {d j : Fin N} : j ∈ neighborhood E d ↔ E d j = true := by
  simp [neighborhood]

/-- `K = 0`: every component depends on itself only. -/
theorem isKRegular_zero_iff (E : Influence N) :
    IsKRegular E 0 ↔ ∀ d j, E d j = decide (j = d) := by
  constructor
  · intro h d j
    obtain ⟨hdd, hcard⟩ := h d
    obtain ⟨a, ha⟩ := Finset.card_eq_one.mp hcard
    have hd : d ∈ neighborhood E d := mem_neighborhood.mpr hdd
    rw [ha, Finset.mem_singleton] at hd
    by_cases hj : j = d
    · subst hj; simp [hdd]
    · have : j ∉ neighborhood E d := by rw [ha, Finset.mem_singleton]; rw [← hd]; exact hj
      rw [mem_neighborhood] at this
      simp [hj, this]
  · intro h d
    refine ⟨by simp [h], ?_⟩
    have : neighborhood E d = {d} := by
      ext j; simp [mem_neighborhood, h]
    rw [this, Finset.card_singleton]

/-- `K = N - 1`: every component depends on every component (the maximally rugged case). -/
theorem isKRegular_full_iff (E : Influence N) (hN : 1 ≤ N) :
    IsKRegular E (N - 1) ↔ ∀ d j, E d j = true := by
  constructor
  · intro h d j
    obtain ⟨-, hcard⟩ := h d
    have hc : (neighborhood E d).card = Fintype.card (Fin N) := by
      rw [hcard, Fintype.card_fin]; omega
    have huniv := Finset.eq_univ_of_card _ hc
    have : j ∈ neighborhood E d := by rw [huniv]; exact Finset.mem_univ j
    exact mem_neighborhood.mp this
  · intro h d
    refine ⟨h d d, ?_⟩
    have : neighborhood E d = univ := by
      ext j; simp [mem_neighborhood, h]
    rw [this, Finset.card_univ, Fintype.card_fin]; omega

/-- Under `IsKRegular E K`, `K < N` (when `N ≥ 1`): a neighborhood cannot exceed the component
set. -/
theorem IsKRegular.lt {E : Influence N} {K : ℕ} (h : IsKRegular E K) (d : Fin N) : K < N := by
  have := Finset.card_le_univ (neighborhood E d)
  rw [(h d).2, Fintype.card_fin] at this
  omega

end SaomNK
