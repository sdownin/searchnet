import SaomNK.Core.Flip

/-!
# Overlap statistics and the four-cycle count

`overlap B i k = Σ_j b_ij b_kj` is the number of activities two actors share. Any statistic of
the form `s_i(B) = Σ_{k≠i} h_ik(O_ik)` with `h` symmetric in the pair admits an exact potential,
half the sum over ordered pairs (`dyadStat_exact_potential`).

RSiena's `cycle4` on a two-mode network, the number of four-cycles through actor `i`, is the
instance `h n = n (n - 1) / 2` (`cycle4`); its potential `cycle4Potential` is the number of
four-cycles in the bipartite graph. A constant multiple `c · cycle4` has potential
`c · cycle4Potential`, so any fixed rescaling of the statistic is covered; a state-dependent
rescaling is not.
-/

open Finset

namespace SaomNK

variable {M N : ℕ}

/-- Portfolio overlap of actors `i` and `k`. -/
def overlap (B : Config M N) (i k : Fin M) : ℝ := ∑ j, ind (B i j) * ind (B k j)

lemma overlap_comm (B : Config M N) (i k : Fin M) : overlap B i k = overlap B k i := by
  unfold overlap; exact Finset.sum_congr rfl fun j _ => mul_comm _ _

lemma overlap_eq_of_unilateral {B B' : Config M N} {i : Fin M} (hu : Unilateral B B' i)
    {p k : Fin M} (hp : p ≠ i) (hk : k ≠ i) : overlap B' p k = overlap B p k := by
  unfold overlap; rw [hu p hp, hu k hk]

/-- A dyadic-overlap statistic `s_i(B) = Σ_{k ≠ i} h i k (O_ik(B))`. -/
def dyadStat (h : Fin M → Fin M → ℝ → ℝ) (B : Config M N) (i : Fin M) : ℝ :=
  ∑ k ∈ univ.erase i, h i k (overlap B i k)

/-- Its potential: half the sum over ordered pairs. -/
noncomputable def dyadPotential (h : Fin M → Fin M → ℝ → ℝ) (B : Config M N) : ℝ :=
  (1 / 2) * ∑ p, ∑ k ∈ univ.erase p, h p k (overlap B p k)

/-- **Any symmetric dyadic-overlap statistic admits an exact potential.** -/
theorem dyadStat_exact_potential (h : Fin M → Fin M → ℝ → ℝ) (hsymm : ∀ p k, h p k = h k p)
    {B B' : Config M N} {i : Fin M} (hu : Unilateral B B' i) :
    dyadStat h B' i - dyadStat h B i = dyadPotential h B' - dyadPotential h B := by
  unfold dyadPotential
  have hzero : ∀ p, p ≠ i → ∀ k, k ≠ i →
      h p k (overlap B' p k) - h p k (overlap B p k) = 0 := by
    intro p hp k hk; rw [overlap_eq_of_unilateral hu hp hk]; ring
  have hinner : ∀ p, p ≠ i →
      ∑ k ∈ univ.erase p, (h p k (overlap B' p k) - h p k (overlap B p k))
        = h p i (overlap B' p i) - h p i (overlap B p i) := by
    intro p hp
    rw [← Finset.add_sum_erase (univ.erase p) _
      (Finset.mem_erase.mpr ⟨Ne.symm hp, mem_univ i⟩)]
    rw [Finset.sum_eq_zero (fun k hk => hzero p hp k (Finset.ne_of_mem_erase hk)), add_zero]
  have hsum : (∑ p, ∑ k ∈ univ.erase p, h p k (overlap B' p k))
      - (∑ p, ∑ k ∈ univ.erase p, h p k (overlap B p k))
      = ∑ p, ∑ k ∈ univ.erase p, (h p k (overlap B' p k) - h p k (overlap B p k)) := by
    simp only [Finset.sum_sub_distrib]
  have houter : ∑ p, ∑ k ∈ univ.erase p, (h p k (overlap B' p k) - h p k (overlap B p k))
      = (∑ k ∈ univ.erase i, (h i k (overlap B' i k) - h i k (overlap B i k)))
        + ∑ p ∈ univ.erase i, (h p i (overlap B' p i) - h p i (overlap B p i)) := by
    rw [← Finset.add_sum_erase univ _ (mem_univ i)]
    congr 1
    exact Finset.sum_congr rfl fun p hp => hinner p (Finset.ne_of_mem_erase hp)
  have hsymmD : ∀ p, h p i (overlap B' p i) - h p i (overlap B p i)
      = h i p (overlap B' i p) - h i p (overlap B i p) := by
    intro p; rw [hsymm p i, overlap_comm B' p i, overlap_comm B p i]
  have hstat : dyadStat h B' i - dyadStat h B i
      = ∑ k ∈ univ.erase i, (h i k (overlap B' i k) - h i k (overlap B i k)) := by
    unfold dyadStat; rw [← Finset.sum_sub_distrib]
  rw [hstat, ← mul_sub, hsum, houter, Finset.sum_congr rfl (fun p _ => hsymmD p)]
  ring

/-- The four-cycle count through actor `i`: `Σ_{k≠i} O_ik (O_ik - 1) / 2`. -/
noncomputable def cycle4 (B : Config M N) (i : Fin M) : ℝ :=
  dyadStat (fun _ _ n => n * (n - 1) / 2) B i

/-- Number of four-cycles in the bipartite graph: half the sum of the actor statistics. -/
noncomputable def cycle4Potential (B : Config M N) : ℝ :=
  dyadPotential (fun _ _ n => n * (n - 1) / 2) B

/-- **The four-cycle count admits an exact potential**, the number of four-cycles. -/
theorem cycle4_exact_potential {B B' : Config M N} {i : Fin M} (hu : Unilateral B B' i) :
    cycle4 B' i - cycle4 B i = cycle4Potential B' - cycle4Potential B :=
  dyadStat_exact_potential _ (fun _ _ => rfl) hu

/-- A constant multiple of `cycle4` has the same multiple of `cycle4Potential` as potential. -/
theorem smul_cycle4_exact_potential (c : ℝ) {B B' : Config M N} {i : Fin M}
    (hu : Unilateral B B' i) :
    c * cycle4 B' i - c * cycle4 B i = c * cycle4Potential B' - c * cycle4Potential B := by
  rw [← mul_sub, ← mul_sub, cycle4_exact_potential hu]

end SaomNK
