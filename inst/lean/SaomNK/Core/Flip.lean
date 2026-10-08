import SaomNK.Core.Config

/-!
# Single-entry flips

A SAOM ministep changes one tie: actor `i` toggles activity `j`. `flip B i j` is that move. The
lemmas here are the combinatorics of flips used by every chain-level result: flips are
involutions, flips by one actor are unilateral deviations, flips of distinct entries commute,
and the Hamming distance `diffCard` drops by one along a well-chosen flip.
-/

open Finset

namespace SaomNK

variable {M N : ℕ}

/-- Flip a single entry `(i, j)` of `B`. -/
def flip (B : Config M N) (i : Fin M) (j : Fin N) : Config M N :=
  fun m k => if m = i ∧ k = j then !B i j else B m k

lemma flip_apply (B : Config M N) (i : Fin M) (j : Fin N) (m : Fin M) (k : Fin N) :
    flip B i j m k = if m = i ∧ k = j then !B i j else B m k := rfl

lemma flip_unilateral (B : Config M N) (i : Fin M) (j : Fin N) :
    Unilateral B (flip B i j) i := by
  intro m hm
  funext k
  simp [flip_apply, hm]

@[simp] lemma flip_flip (B : Config M N) (i : Fin M) (j : Fin N) : flip (flip B i j) i j = B := by
  funext m k
  simp only [flip_apply]
  by_cases h : m = i ∧ k = j
  · obtain ⟨rfl, rfl⟩ := h; simp
  · simp [h]

@[simp] lemma flip_self (B : Config M N) (i : Fin M) (j : Fin N) : flip B i j i j = !B i j := by
  simp [flip_apply]

lemma flip_row_ne (B : Config M N) {i m : Fin M} (j : Fin N) (hm : m ≠ i) :
    flip B i j m = B m := by
  funext k; simp [flip_apply, hm]

lemma flip_same_row_ne (B : Config M N) (i : Fin M) {j k : Fin N} (hk : k ≠ j) :
    flip B i j i k = B i k := by
  simp [flip_apply, hk]

/-- Flips of two different entries of one row commute. -/
lemma flip_comm (B : Config M N) (i : Fin M) {j j' : Fin N} (h : j ≠ j') :
    flip (flip B i j) i j' = flip (flip B i j') i j := by
  funext m k
  simp only [flip_apply]
  by_cases hm : m = i
  · subst hm
    by_cases hk : k = j'
    · subst hk
      simp [Ne.symm h]
    · by_cases hk' : k = j
      · subst hk'
        simp [h]
      · simp [hk, hk']
  · simp [hm]

/-- Flips by two different actors commute. -/
lemma flip_comm_actors {B : Config M N} {i k : Fin M} (hik : i ≠ k) (n : Fin N) :
    flip (flip B i n) k n = flip (flip B k n) i n := by
  funext m m'
  simp only [flip_apply]
  by_cases hmi : m = i <;> by_cases hmk : m = k <;> by_cases hn : m' = n <;> simp_all

lemma flip_row_congr {B B' : Config M N} {k : Fin M} (n : Fin N) (hrow : B' k = B k) :
    flip B' k n k = flip B k n k := by
  funext m; simp only [flip_apply]; rw [hrow]

/-! ## Hamming distance -/

/-- Number of entries where `B` and `B'` differ. -/
def diffCard (B B' : Config M N) : ℕ :=
  (univ.filter fun p : Fin M × Fin N => B p.1 p.2 ≠ B' p.1 p.2).card

lemma diffCard_flip {B B' : Config M N} {i : Fin M} {j : Fin N} (h : B i j ≠ B' i j) :
    diffCard (flip B i j) B' + 1 = diffCard B B' := by
  unfold diffCard
  have hset : (univ.filter fun p : Fin M × Fin N => flip B i j p.1 p.2 ≠ B' p.1 p.2)
      = (univ.filter fun p : Fin M × Fin N => B p.1 p.2 ≠ B' p.1 p.2).erase (i, j) := by
    ext p
    rw [Finset.mem_filter, Finset.mem_erase, Finset.mem_filter]
    simp only [Finset.mem_univ, true_and]
    by_cases hp : p = (i, j)
    · subst hp
      have h1 : flip B i j i j = !B i j := flip_self B i j
      simp only [h1, ne_eq, not_true_eq_false, false_and, iff_false, not_not]
      cases hb : B i j <;> cases hb' : B' i j <;> simp_all
    · have hne : ¬ (p.1 = i ∧ p.2 = j) := by
        rintro ⟨h1, h2⟩; exact hp (Prod.ext h1 h2)
      rw [flip_apply, ite_eq_right hne]
      exact ⟨fun h => ⟨hp, h⟩, fun h => h.2⟩
  rw [hset, Finset.card_erase_add_one]
  simp [h]

lemma diffCard_eq_zero {B B' : Config M N} (h : diffCard B B' = 0) : B = B' := by
  unfold diffCard at h
  rw [Finset.card_eq_zero, Finset.filter_eq_empty_iff] at h
  funext i j
  by_contra hne
  exact h (Finset.mem_univ (i, j)) hne

end SaomNK
