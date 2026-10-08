import SaomNK.Stats.Cycle4

/-!
# Row-pair statistics

`rowPairStat h B i = Σ_{k≠i} h(b_i, b_k)` for any function `h` of two actors' rows. This is the
most general pairwise statistic on a two-mode network that treats rivals symmetrically: overlap
counts, the four-cycle count, similarity or distance between portfolios, and any kernel built
from them.

* `rowPairStat_exact_potential`: if `h` is symmetric, the statistic admits the exact potential
  `½ Σ_p Σ_{k≠p} h(b_p, b_k)`.
* `mixed_cycle_sum`, `mixed_no_exact_potential`: if actors weight the same symmetric kernel with
  different coefficients `c_i ≠ c_k`, the four-move cycle in which two actors each flip one
  entry and flip it back has a nonzero sum of change statistics whenever the kernel's interaction
  contrast is nonzero, so no exact potential exists. Heterogeneous weights on a shared pairwise
  term break integrability; heterogeneous own-row terms do not.
* `agreement`: the number of coordinates on which two rows agree (`N` minus the Hamming
  distance), a worked instance with contrast `-2`.
-/

open Finset

namespace SaomNK

variable {M N : ℕ}

/-- `s_i(B) = Σ_{k≠i} h (B i) (B k)` for any function `h` of two actors' rows. -/
def rowPairStat (h : (Fin N → Bool) → (Fin N → Bool) → ℝ) (B : Config M N) (i : Fin M) : ℝ :=
  ∑ k ∈ univ.erase i, h (B i) (B k)

/-- Its potential: half the sum over ordered pairs. -/
noncomputable def rowPairPotential (h : (Fin N → Bool) → (Fin N → Bool) → ℝ)
    (B : Config M N) : ℝ :=
  (1 / 2) * ∑ p, ∑ k ∈ univ.erase p, h (B p) (B k)

/-- **Any statistic built from a symmetric function of two actors' rows admits an exact
potential.** -/
theorem rowPairStat_exact_potential (h : (Fin N → Bool) → (Fin N → Bool) → ℝ)
    (hsymm : ∀ b b', h b b' = h b' b) {B B' : Config M N} {i : Fin M} (hu : Unilateral B B' i) :
    rowPairStat h B' i - rowPairStat h B i = rowPairPotential h B' - rowPairPotential h B := by
  unfold rowPairPotential
  have hinner : ∀ p, p ≠ i →
      ∑ k ∈ univ.erase p, (h (B' p) (B' k) - h (B p) (B k)) = h (B' p) (B' i) - h (B p) (B i) := by
    intro p hp
    rw [← Finset.add_sum_erase (univ.erase p) _ (Finset.mem_erase.mpr ⟨Ne.symm hp, mem_univ i⟩)]
    rw [Finset.sum_eq_zero (fun k hk => by
      rw [hu p hp, hu k (Finset.ne_of_mem_erase hk)]; ring), add_zero]
  have hsum : (∑ p, ∑ k ∈ univ.erase p, h (B' p) (B' k))
      - (∑ p, ∑ k ∈ univ.erase p, h (B p) (B k))
      = ∑ p, ∑ k ∈ univ.erase p, (h (B' p) (B' k) - h (B p) (B k)) := by
    simp only [Finset.sum_sub_distrib]
  have houter : ∑ p, ∑ k ∈ univ.erase p, (h (B' p) (B' k) - h (B p) (B k))
      = (∑ k ∈ univ.erase i, (h (B' i) (B' k) - h (B i) (B k)))
        + ∑ p ∈ univ.erase i, (h (B' p) (B' i) - h (B p) (B i)) := by
    rw [← Finset.add_sum_erase univ _ (mem_univ i)]
    congr 1
    exact Finset.sum_congr rfl fun p hp => hinner p (Finset.ne_of_mem_erase hp)
  have hsymmD : ∀ p, h (B' p) (B' i) - h (B p) (B i) = h (B' i) (B' p) - h (B i) (B p) := by
    intro p; rw [hsymm (B' p), hsymm (B p)]
  have hstat : rowPairStat h B' i - rowPairStat h B i
      = ∑ k ∈ univ.erase i, (h (B' i) (B' k) - h (B i) (B k)) := by
    unfold rowPairStat; rw [← Finset.sum_sub_distrib]
  rw [hstat, ← mul_sub, hsum, houter, Finset.sum_congr rfl (fun p _ => hsymmD p)]
  ring

/-! ## Overlap kernels: `dyadStat` statistics are row-pair statistics -/

/-- The overlap of two rows. -/
def overlapK (b b' : Fin N → Bool) : ℝ := ∑ j, ind (b j) * ind (b' j)

lemma overlap_eq_overlapK (B : Config M N) (i k : Fin M) :
    overlap B i k = overlapK (B i) (B k) := rfl

lemma overlapK_comm (b b' : Fin N → Bool) : overlapK b b' = overlapK b' b := by
  unfold overlapK; exact Finset.sum_congr rfl fun j _ => mul_comm _ _

/-- The four-cycle count is the row-pair statistic of the kernel `C(O, 2)`. -/
lemma cycle4_eq_rowPairStat (B : Config M N) (i : Fin M) :
    cycle4 B i = rowPairStat (fun b b' => overlapK b b' * (overlapK b b' - 1) / 2) B i := rfl

/-! ## Heterogeneous weights on a shared pairwise term -/

/-- Utility with an own-row term `F` and an actor-specific weight `c i` on a pair statistic. -/
def mixedUtility (F : (Fin N → Bool) → ℝ) (c : Fin M → ℝ)
    (h : (Fin N → Bool) → (Fin N → Bool) → ℝ) (B : Config M N) (i : Fin M) : ℝ :=
  F (B i) - c i * rowPairStat h B i

lemma rowPairStat_split (h : (Fin N → Bool) → (Fin N → Bool) → ℝ) (B X : Config M N)
    {i k : Fin M} (hik : i ≠ k) (hrest : ∀ m, m ≠ i → m ≠ k → X m = B m) :
    rowPairStat h X i = h (X i) (X k) + ∑ m ∈ (univ.erase i).erase k, h (X i) (B m) := by
  unfold rowPairStat
  rw [← Finset.add_sum_erase (univ.erase i) _ (Finset.mem_erase.mpr ⟨Ne.symm hik, mem_univ k⟩)]
  congr 1
  exact Finset.sum_congr rfl fun m hm => by
    rw [hrest m (Finset.ne_of_mem_erase (Finset.mem_of_mem_erase hm)) (Finset.ne_of_mem_erase hm)]

/-- Interaction contrast of a kernel at entry `n` for actors `i`, `k`. -/
def pairContrast (h : (Fin N → Bool) → (Fin N → Bool) → ℝ) (B : Config M N) (i k : Fin M)
    (n : Fin N) : ℝ :=
  h (flip B i n i) (B k) - h (B i) (B k) + h (B i) (flip B k n k) - h (flip B i n i) (flip B k n k)

/-- **The four-move cycle of two actors with different weights.** Actor `i` flips `n`, actor
`k` flips `n`, `i` flips back, `k` flips back. The sum of the movers' utility changes is the
weight gap times the kernel's interaction contrast. -/
theorem mixed_cycle_sum (F : (Fin N → Bool) → ℝ) (c : Fin M → ℝ)
    (h : (Fin N → Bool) → (Fin N → Bool) → ℝ) (hsymm : ∀ b b', h b b' = h b' b)
    (B : Config M N) {i k : Fin M} (hik : i ≠ k) (n : Fin N) :
    (mixedUtility F c h (flip B i n) i - mixedUtility F c h B i)
      + (mixedUtility F c h (flip (flip B i n) k n) k - mixedUtility F c h (flip B i n) k)
      + (mixedUtility F c h (flip (flip (flip B i n) k n) i n) i
          - mixedUtility F c h (flip (flip B i n) k n) i)
      + (mixedUtility F c h (flip (flip (flip (flip B i n) k n) i n) k n) k
          - mixedUtility F c h (flip (flip (flip B i n) k n) i n) k)
      = (c k - c i) * pairContrast h B i k n := by
  unfold pairContrast
  have hki : k ≠ i := Ne.symm hik
  have r1k : flip B i n k = B k := flip_row_ne B n hki
  have r1m : ∀ m, m ≠ i → flip B i n m = B m := fun m hm => flip_row_ne B n hm
  have r2i : flip (flip B i n) k n i = flip B i n i := flip_row_ne (flip B i n) n hik
  have r2k : flip (flip B i n) k n k = flip B k n k := flip_row_congr n r1k
  have r2m : ∀ m, m ≠ i → m ≠ k → flip (flip B i n) k n m = B m := fun m hm hmk => by
    rw [flip_row_ne (flip B i n) n hmk, r1m m hm]
  have r3i : flip (flip (flip B i n) k n) i n i = B i := by
    rw [flip_row_congr n r2i, flip_flip]
  have r3k : flip (flip (flip B i n) k n) i n k = flip B k n k := by
    rw [flip_row_ne _ n hki, r2k]
  have r3m : ∀ m, m ≠ i → m ≠ k → flip (flip (flip B i n) k n) i n m = B m := fun m hm hmk => by
    rw [flip_row_ne _ n hm, r2m m hm hmk]
  have r4k : flip (flip (flip (flip B i n) k n) i n) k n k = B k := by
    rw [flip_row_congr n (r3k.trans r2k.symm), flip_flip, r1k]
  have r4i : flip (flip (flip (flip B i n) k n) i n) k n i = B i := by
    rw [flip_row_ne _ n hik, r3i]
  have r4m : ∀ m, m ≠ i → m ≠ k → flip (flip (flip (flip B i n) k n) i n) k n m = B m :=
    fun m hm hmk => by rw [flip_row_ne _ n hmk, r3m m hm hmk]
  have sI0 := rowPairStat_split h B B hik (fun m _ _ => rfl)
  have sI1 := rowPairStat_split h B (flip B i n) hik (fun m hm _ => r1m m hm)
  have sI2 := rowPairStat_split h B (flip (flip B i n) k n) hik r2m
  have sI3 := rowPairStat_split h B (flip (flip (flip B i n) k n) i n) hik r3m
  have sK1 := rowPairStat_split h B (flip B i n) hki (fun m hmk hm => r1m m hm)
  have sK2 := rowPairStat_split h B (flip (flip B i n) k n) hki (fun m hmk hm => r2m m hm hmk)
  have sK3 := rowPairStat_split h B (flip (flip (flip B i n) k n) i n) hki
    (fun m hmk hm => r3m m hm hmk)
  have sK4 := rowPairStat_split h B (flip (flip (flip (flip B i n) k n) i n) k n) hki
    (fun m hmk hm => r4m m hm hmk)
  unfold mixedUtility
  rw [sI0, sI1, sI2, sI3, sK1, sK2, sK3, sK4]
  rw [r1k, r2i, r2k, r3i, r3k, r4i, r4k]
  rw [hsymm (B k) (flip B i n i), hsymm (flip B k n k) (flip B i n i), hsymm (flip B k n k) (B i),
    hsymm (B k) (B i)]
  ring

/-- **Heterogeneous weights on a shared pairwise term destroy the exact potential**, whenever the
kernel's interaction contrast is nonzero somewhere. -/
theorem mixed_no_exact_potential (F : (Fin N → Bool) → ℝ) (c : Fin M → ℝ)
    (h : (Fin N → Bool) → (Fin N → Bool) → ℝ) (hsymm : ∀ b b', h b b' = h b' b)
    (B : Config M N) {i k : Fin M} (hik : i ≠ k) (n : Fin N)
    (hcon : pairContrast h B i k n ≠ 0) (hc : c i ≠ c k) :
    ¬ ∃ Φ : Config M N → ℝ, ∀ (X : Config M N) (j : Fin M) (m : Fin N),
        mixedUtility F c h (flip X j m) j - mixedUtility F c h X j = Φ (flip X j m) - Φ X := by
  rintro ⟨Φ, hΦ⟩
  have hcyc := mixed_cycle_sum F c h hsymm B hik n
  rw [hΦ B i n, hΦ (flip B i n) k n, hΦ (flip (flip B i n) k n) i n,
    hΦ (flip (flip (flip B i n) k n) i n) k n] at hcyc
  have hclose : flip (flip (flip (flip B i n) k n) i n) k n = B := by
    rw [flip_comm_actors hik n, flip_flip, flip_flip]
  rw [hclose] at hcyc
  have h0 : (c k - c i) * pairContrast h B i k n = 0 := by rw [← hcyc]; ring
  rcases mul_eq_zero.mp h0 with h1 | h1
  · exact hc (by linarith)
  · exact hcon h1

/-! ## A worked kernel: agreement (N minus Hamming distance) -/

/-- Number of coordinates on which two rows agree. -/
def agreement (b b' : Fin N → Bool) : ℝ := ∑ n, if b n = b' n then 1 else 0

lemma agreement_symm (b b' : Fin N → Bool) : agreement b b' = agreement b' b := by
  unfold agreement
  exact Finset.sum_congr rfl fun n _ => by simp only [eq_comm]

/-- At a coordinate two actors share, the agreement kernel has interaction contrast `-2`. -/
theorem agreement_contrast (B : Config M N) {i k : Fin M} (n : Fin N) (hmatch : B i n = B k n) :
    pairContrast agreement B i k n = -2 := by
  unfold pairContrast agreement
  rw [← Finset.sum_sub_distrib, ← Finset.sum_add_distrib, ← Finset.sum_sub_distrib,
    ← Finset.add_sum_erase univ _ (mem_univ n)]
  rw [Finset.sum_eq_zero (fun m hm => by
    have hmn : m ≠ n := Finset.ne_of_mem_erase hm
    simp [flip_apply, hmn]), add_zero]
  simp only [flip_apply, and_self, ite_true]
  rw [hmatch]
  cases B k n <;> norm_num

/-- Two actors who weight agreement differently and share a coordinate admit no exact
potential. -/
theorem agreement_mixed_no_exact_potential (F : (Fin N → Bool) → ℝ) (c : Fin M → ℝ)
    (B : Config M N) {i k : Fin M} (hik : i ≠ k) (n : Fin N) (hmatch : B i n = B k n)
    (hc : c i ≠ c k) :
    ¬ ∃ Φ : Config M N → ℝ, ∀ (X : Config M N) (j : Fin M) (m : Fin N),
        mixedUtility F c agreement (flip X j m) j - mixedUtility F c agreement X j
          = Φ (flip X j m) - Φ X :=
  mixed_no_exact_potential F c agreement agreement_symm B hik n
    (by rw [agreement_contrast B n hmatch]; norm_num) hc

end SaomNK
