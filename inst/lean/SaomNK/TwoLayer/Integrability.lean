import SaomNK.Chain.Balance
import SaomNK.Stats.Cycle4

/-!
# Two-layer integrability: when does a coupled process have a potential?

State `(A, B)`: `B : Config M N` is the actor-by-activity layer, `A : Rel M` a symmetric
actor-by-actor relational layer (alliances, partnerships, communication ties). Each actor has
an evaluation function per layer. The coupling statistic `c_i(A,B) = Σ_{k≠i} A_ik O_ik(B)`, ties
weighted by portfolio overlap, enters the B-evaluation with coefficient `θ_AB` and the
A-evaluation with coefficient `θ_BA` (selection and influence in the sense of Snijders,
Steglich and Schweinberger 2007).

Formalized, for single-flip binary-logit revision (one entry of `B` or one unordered pair of `A`
per move):

* sufficiency: if `θ_AB = θ_BA` (and the relational own terms are potential-form,
  `OwnAPotential`) the joint potential is exact (`integrability_sufficient`), and the two-layer
  chain satisfies Gibbs detailed balance (`two_layer_detailed_balance`);
* the cycle affinity: around the elementary four-move cycle `B_ij on, A_ik on, B_ij off,
  A_ik off`, the acting actor's change statistics sum to `(θ_BA − θ_AB) B_kj` whatever the own
  terms (`cycle_affinity`), and in log-ratio units to `β (θ_BA − θ_AB) B_kj`
  (`cycle_log_ratio`, `cycle_log_ratio_total`);
* necessity: an exact potential forces `θ_AB = θ_BA` (`integrability_necessary`), and so does
  reversibility of the chain with respect to ANY strictly positive measure, by Kolmogorov's
  criterion (`symmetric_of_reversible`, `symmetric_of_reversible_total`). Under asymmetric
  coupling no Gibbs measure of any potential is stationary;
* own-term examples: a symmetric degree (density) term is potential-form
  (`density_ownAPotential`); an ego-covariate term with a non-constant covariate is not
  (`egoCovariate_not_ownAPotential`).
-/

open Finset Real

set_option linter.deprecated false

noncomputable section

namespace SaomNK.TwoLayer

variable {M N : ℕ}

/-- Relational layer among actors. -/
abbrev Rel (M : ℕ) := Fin M → Fin M → Bool

/-- Symmetry of the relational layer. -/
def IsSymm (A : Rel M) : Prop := ∀ p q, A p q = A q p

/-- Flip the unordered pair `{i, k}` of a relational layer. -/
def flipPair (A : Rel M) (i k : Fin M) : Rel M :=
  fun p q => if (p = i ∧ q = k) ∨ (p = k ∧ q = i) then !A i k else A p q

lemma flipPair_symm {A : Rel M} (hA : IsSymm A) (i k : Fin M) : IsSymm (flipPair A i k) := by
  intro p q
  show (if (p = i ∧ q = k) ∨ (p = k ∧ q = i) then !A i k else A p q)
      = (if (q = i ∧ p = k) ∨ (q = k ∧ p = i) then !A i k else A q p)
  by_cases h1 : (p = i ∧ q = k) ∨ (p = k ∧ q = i)
  · have h2 : (q = i ∧ p = k) ∨ (q = k ∧ p = i) := by
      rcases h1 with ⟨h, h'⟩ | ⟨h, h'⟩
      · exact Or.inr ⟨h', h⟩
      · exact Or.inl ⟨h', h⟩
    rw [if_pos h1, if_pos h2]
  · have h2 : ¬ ((q = i ∧ p = k) ∨ (q = k ∧ p = i)) := by
      rintro (⟨h, h'⟩ | ⟨h, h'⟩)
      · exact h1 (Or.inr ⟨h', h⟩)
      · exact h1 (Or.inl ⟨h', h⟩)
    rw [if_neg h1, if_neg h2, hA p q]

lemma flipPair_flipPair {A : Rel M} (hA : IsSymm A) (i k : Fin M) :
    flipPair (flipPair A i k) i k = A := by
  funext p q
  show (if (p = i ∧ q = k) ∨ (p = k ∧ q = i)
      then !(if (i = i ∧ k = k) ∨ (i = k ∧ k = i) then !A i k else A i k)
      else (if (p = i ∧ q = k) ∨ (p = k ∧ q = i) then !A i k else A p q)) = A p q
  rw [if_pos (show (i = i ∧ k = k) ∨ (i = k ∧ k = i) from Or.inl ⟨rfl, rfl⟩), Bool.not_not]
  by_cases h : (p = i ∧ q = k) ∨ (p = k ∧ q = i)
  · rw [if_pos h]
    rcases h with ⟨hp, hq⟩ | ⟨hp, hq⟩
    · rw [hp, hq]
    · rw [hp, hq]; exact hA _ _
  · rw [if_neg h, if_neg h]

/-- The pair `{i,k}` is unordered: on a symmetric layer, flipping it from either endpoint is the
same move. -/
lemma flipPair_comm {A : Rel M} (hA : IsSymm A) (i k : Fin M) :
    flipPair A k i = flipPair A i k := by
  funext p q
  show (if (p = k ∧ q = i) ∨ (p = i ∧ q = k) then !A k i else A p q)
      = (if (p = i ∧ q = k) ∨ (p = k ∧ q = i) then !A i k else A p q)
  by_cases h : (p = i ∧ q = k) ∨ (p = k ∧ q = i)
  · rw [if_pos h.symm, if_pos h, hA k i]
  · rw [if_neg (fun h' : (p = k ∧ q = i) ∨ (p = i ∧ q = k) => h h'.symm), if_neg h]

/-- A tie-weighted actor statistic on the relational layer, `Σ_{k≠i} A_ik w_ik`. The coupling
statistic is the instance `w = O(B)`; the degree is the instance `w = 1`. -/
def pairStat (w : Fin M → Fin M → ℝ) (A : Rel M) (i : Fin M) : ℝ :=
  ∑ k ∈ univ.erase i, ind (A i k) * w i k

/-- Its candidate potential: `Σ_{i<k} A_ik w_ik`, written as half the sum over ordered pairs. -/
def pairPotential (w : Fin M → Fin M → ℝ) (A : Rel M) : ℝ :=
  (1 / 2) * ∑ p, ∑ k ∈ univ.erase p, ind (A p k) * w p k

/-- The cross-layer coupling statistic `c_i(A,B) = Σ_{k≠i} A_ik O_ik(B)`. -/
def coupling (A : Rel M) (B : Config M N) (i : Fin M) : ℝ :=
  ∑ k ∈ univ.erase i, ind (A i k) * overlap B i k

/-- Its potential: `Σ_{i<k} A_ik O_ik`, written as half the sum over ordered pairs. -/
def couplingPotential (A : Rel M) (B : Config M N) : ℝ :=
  (1 / 2) * ∑ p, ∑ k ∈ univ.erase p, ind (A p k) * overlap B p k

/-- Two-layer specification. -/
structure TwoLayerSpec (M N : ℕ) where
  /-- own term on the portfolio row (NK fitness, density, outAct) -/
  ownB : (Fin N → Bool) → ℝ
  /-- congestion coefficient -/
  θinPop : ℝ
  /-- actor `i`'s single-layer A-evaluation term -/
  ownA : Fin M → Rel M → ℝ
  /-- the single-layer A potential -/
  ΨA : Rel M → ℝ
  θAB : ℝ
  θBA : ℝ

/-- Actor `i`'s B-evaluation. -/
def TwoLayerSpec.evalB (S : TwoLayerSpec M N) (i : Fin M) (A : Rel M) (B : Config M N) : ℝ :=
  S.ownB (B i) + S.θinPop * inPop B i + S.θAB * coupling A B i

/-- Actor `i`'s A-evaluation. -/
def TwoLayerSpec.evalA (S : TwoLayerSpec M N) (i : Fin M) (A : Rel M) (B : Config M N) : ℝ :=
  S.ownA i A + S.θBA * coupling A B i

/-- The A-layer own term is potential-form on its own layer. -/
def TwoLayerSpec.OwnAPotential (S : TwoLayerSpec M N) : Prop :=
  ∀ A : Rel M, IsSymm A → ∀ i k : Fin M, i ≠ k →
    S.ownA i (flipPair A i k) - S.ownA i A = S.ΨA (flipPair A i k) - S.ΨA A

/-- The candidate joint potential with common coupling coefficient `θC`. -/
def TwoLayerSpec.jointPotential (S : TwoLayerSpec M N) (θC : ℝ) (A : Rel M) (B : Config M N) :
    ℝ :=
  (∑ i, S.ownB (B i)) + S.θinPop * (∑ d, rosenthal (colSum B d)) + S.ΨA A
    + θC * couplingPotential A B

/-- `Φ` is an exact potential for the two-layer process: single-coordinate differences reproduce
every actor's change statistic in both layers (on symmetric relational states). -/
def TwoLayerSpec.IsExactPotential (S : TwoLayerSpec M N) (Φ : Rel M → Config M N → ℝ) : Prop :=
  (∀ A : Rel M, IsSymm A → ∀ (B : Config M N) (i : Fin M) (j : Fin N),
      S.evalB i A (flip B i j) - S.evalB i A B = Φ A (flip B i j) - Φ A B) ∧
  (∀ A : Rel M, IsSymm A → ∀ (B : Config M N) (i k : Fin M), i ≠ k →
      S.evalA i (flipPair A i k) B - S.evalA i A B = Φ (flipPair A i k) B - Φ A B)

/-! ### The three coupling identities -/

/-- B-flip: the coupling statistic changes as the coupling potential does (symmetric `A`). -/
lemma coupling_flip_diff {A : Rel M} (hA : IsSymm A) {B B' : Config M N} {i : Fin M}
    (hu : Unilateral B B' i) :
    coupling A B' i - coupling A B i = couplingPotential A B' - couplingPotential A B :=
  dyadStat_exact_potential (fun p k n => ind (A p k) * n)
    (fun p k => by funext n; rw [hA p k]) hu

/-- A-flip at `{i,k}`: actor `i`'s tie-weighted statistic changes by `ΔA_ik · w_ik`. -/
lemma pairStat_flipPair_diff (w : Fin M → Fin M → ℝ) (A : Rel M) {i k : Fin M} (hik : i ≠ k) :
    pairStat w (flipPair A i k) i - pairStat w A i
      = (ind (!A i k) - ind (A i k)) * w i k := by
  unfold pairStat
  rw [← Finset.sum_sub_distrib]
  have hterm : ∀ k' ∈ univ.erase i,
      ind (flipPair A i k i k') * w i k' - ind (A i k') * w i k'
        = if k' = k then (ind (!A i k) - ind (A i k)) * w i k else 0 := by
    intro k' _
    by_cases hk : k' = k
    · have : flipPair A i k i k' = !A i k := by
        show (if (i = i ∧ k' = k) ∨ (i = k ∧ k' = i) then !A i k else A i k') = !A i k
        rw [if_pos (Or.inl ⟨rfl, hk⟩)]
      rw [this, if_pos hk, hk]; ring
    · have : flipPair A i k i k' = A i k' := by
        show (if (i = i ∧ k' = k) ∨ (i = k ∧ k' = i) then !A i k else A i k') = A i k'
        rw [if_neg]
        rintro (⟨_, h⟩ | ⟨h, _⟩)
        · exact hk h
        · exact hik h
      rw [this, if_neg hk]; ring
  rw [Finset.sum_congr rfl hterm]
  simp [Finset.sum_ite_eq', Ne.symm hik]

/-- A-flip at `{i,k}`: the pair potential of a symmetric weight changes by `ΔA_ik · w_ik`
(symmetric `A`). -/
lemma pairPotential_flipPair_diff (w : Fin M → Fin M → ℝ) (hw : ∀ p q, w p q = w q p)
    {A : Rel M} (hA : IsSymm A) {i k : Fin M} (hik : i ≠ k) :
    pairPotential w (flipPair A i k) - pairPotential w A
      = (ind (!A i k) - ind (A i k)) * w i k := by
  unfold pairPotential
  have hterm : ∀ p, ∀ q ∈ univ.erase p,
      ind (flipPair A i k p q) * w p q - ind (A p q) * w p q
        = (if p = i ∧ q = k then (ind (!A i k) - ind (A i k)) * w i k else 0)
          + (if p = k ∧ q = i then (ind (!A i k) - ind (A i k)) * w i k else 0) := by
    intro p q _
    by_cases h1 : p = i ∧ q = k
    · obtain ⟨hp, hq⟩ := h1
      have hf : flipPair A i k p q = !A i k := by
        show (if (p = i ∧ q = k) ∨ (p = k ∧ q = i) then !A i k else A p q) = !A i k
        rw [if_pos (Or.inl ⟨hp, hq⟩)]
      have hne : ¬ (p = k ∧ q = i) := fun h => hik (hp.symm.trans h.1)
      rw [hf, if_pos ⟨hp, hq⟩, if_neg hne, hp, hq]; ring
    · by_cases h2 : p = k ∧ q = i
      · obtain ⟨hp, hq⟩ := h2
        have hf : flipPair A i k p q = !A i k := by
          show (if (p = i ∧ q = k) ∨ (p = k ∧ q = i) then !A i k else A p q) = !A i k
          rw [if_pos (Or.inr ⟨hp, hq⟩)]
        rw [hf, if_neg h1, if_pos ⟨hp, hq⟩, hp, hq, hA k i, hw k i]; ring
      · have hf : flipPair A i k p q = A p q := by
          show (if (p = i ∧ q = k) ∨ (p = k ∧ q = i) then !A i k else A p q) = A p q
          rw [if_neg]
          rintro (h | h)
          · exact h1 h
          · exact h2 h
        rw [hf, if_neg h1, if_neg h2]; ring
  have hpair : ∀ (a b : Fin M), a ≠ b →
      (∑ p, ∑ q ∈ univ.erase p,
        (if p = a ∧ q = b then (ind (!A i k) - ind (A i k)) * w i k else 0))
        = (ind (!A i k) - ind (A i k)) * w i k := by
    intro a b hab
    have hin : ∀ p, (∑ q ∈ univ.erase p,
        (if p = a ∧ q = b then (ind (!A i k) - ind (A i k)) * w i k else 0))
          = if p = a then (ind (!A i k) - ind (A i k)) * w i k else 0 := by
      intro p
      by_cases hp : p = a
      · rw [if_pos hp]
        simp [hp, Finset.sum_ite_eq', Ne.symm hab]
      · simp [hp]
    simp only [hin]
    simp
  rw [← mul_sub, ← Finset.sum_sub_distrib]
  simp_rw [← Finset.sum_sub_distrib]
  rw [Finset.sum_congr rfl (fun p _ => Finset.sum_congr rfl (fun q hq => hterm p q hq))]
  simp only [Finset.sum_add_distrib]
  rw [hpair i k hik, hpair k i (Ne.symm hik)]
  ring

/-- A-flip at `{i,k}`: actor `i`'s coupling statistic changes by `ΔA_ik · O_ik`. -/
lemma coupling_flipPair_diff (A : Rel M) (B : Config M N) {i k : Fin M} (hik : i ≠ k) :
    coupling (flipPair A i k) B i - coupling A B i
      = (ind (!A i k) - ind (A i k)) * overlap B i k :=
  pairStat_flipPair_diff (overlap B) A hik

/-- A-flip at `{i,k}`: the coupling potential changes by `ΔA_ik · O_ik` (symmetric `A`). -/
lemma couplingPotential_flipPair_diff {A : Rel M} (hA : IsSymm A) (B : Config M N)
    {i k : Fin M} (hik : i ≠ k) :
    couplingPotential (flipPair A i k) B - couplingPotential A B
      = (ind (!A i k) - ind (A i k)) * overlap B i k :=
  pairPotential_flipPair_diff (overlap B) (overlap_comm B) hA hik

/-! ### Sufficiency -/

/-- **Integrability, sufficiency.** If `θ_AB = θ_BA`, the joint potential is an exact
potential for the two-layer process.

Hypotheses, in plain words. (i) Single-flip (Glauber) revision: each move changes one entry of
`B` or one unordered pair of `A`, and `IsExactPotential` is stated on those moves only.
(ii) The own-term hypothesis `hown : S.OwnAPotential` is required: every actor's own
relational-layer term must change, on a pair flip, exactly as one common function `ΨA` of the
layer changes. Density (`θ · deg_i`, proved: `density_ownAPotential`) satisfies it, and
ego-covariate own terms do NOT (proved: `egoCovariate_not_ownAPotential`), and for those no
potential exists even with `θ_AB = θ_BA`. Transitive-triad own terms (which satisfy it) and
degree-squared own terms (which do not) are not formalized here.
The "if and only if" is therefore conditional on (ii). -/
theorem integrability_sufficient (S : TwoLayerSpec M N) (hown : S.OwnAPotential)
    (hθ : S.θAB = S.θBA) : S.IsExactPotential (S.jointPotential S.θAB) := by
  constructor
  · intro A hA B i j
    unfold TwoLayerSpec.evalB TwoLayerSpec.jointPotential
    have hu := flip_unilateral B i j
    have h1 : (∑ m, S.ownB (flip B i j m)) - (∑ m, S.ownB (B m))
        = S.ownB (flip B i j i) - S.ownB (B i) :=
      own_sum_diff (fun _ b => S.ownB b) hu
    have h2 := inPop_diff_eq_rosenthal hu
    have h3 := coupling_flip_diff hA hu
    linear_combination -h1 + S.θinPop * h2 + S.θAB * h3
  · intro A hA B i k hik
    unfold TwoLayerSpec.evalA TwoLayerSpec.jointPotential
    have h1 := hown A hA i k hik
    have h2 := coupling_flipPair_diff A B hik
    have h3 := couplingPotential_flipPair_diff hA B hik
    linear_combination h1 + S.θBA * h2 - S.θAB * h3
      - ((ind (!A i k) - ind (A i k)) * overlap B i k) * hθ

/-- With a potential, the two-layer binary-logit chain satisfies detailed balance for the Gibbs
weight `exp(β Φ)` on both kinds of move, by the same argument as for one layer. -/
theorem two_layer_detailed_balance (S : TwoLayerSpec M N) (Φ : Rel M → Config M N → ℝ)
    (hΦ : S.IsExactPotential Φ) (β cB cA : ℝ) :
    (∀ A : Rel M, IsSymm A → ∀ (B : Config M N) (i : Fin M) (j : Fin N),
      exp (β * Φ A B) * (cB * glauber β (S.evalB i A (flip B i j) - S.evalB i A B))
        = exp (β * Φ A (flip B i j))
          * (cB * glauber β (S.evalB i A (flip (flip B i j) i j) - S.evalB i A (flip B i j)))) ∧
    (∀ A : Rel M, IsSymm A → ∀ (B : Config M N) (i k : Fin M), i ≠ k →
      exp (β * Φ A B) * (cA * glauber β (S.evalA i (flipPair A i k) B - S.evalA i A B))
        = exp (β * Φ (flipPair A i k) B)
          * (cA * glauber β (S.evalA i (flipPair (flipPair A i k) i k) B
              - S.evalA i (flipPair A i k) B))) := by
  obtain ⟨hB, hA⟩ := hΦ
  constructor
  · intro A hAs B i j
    rw [flip_flip, hB A hAs B i j,
      show S.evalB i A B - S.evalB i A (flip B i j) = Φ A B - Φ A (flip B i j) by
        linarith [hB A hAs B i j]]
    have := exp_mul_glauber_comm β (Φ A B) (Φ A (flip B i j))
    linear_combination cB * this
  · intro A hAs B i k hik
    rw [flipPair_flipPair hAs, hA A hAs B i k hik,
      show S.evalA i A B - S.evalA i (flipPair A i k) B = Φ A B - Φ (flipPair A i k) B by
        linarith [hA A hAs B i k hik]]
    have := exp_mul_glauber_comm β (Φ A B) (Φ (flipPair A i k) B)
    linear_combination cA * this

/-! ### Necessity -/

/-- **Cycle affinity.** Around the elementary cycle
`B_ij on, A_ik on, B_ij off, A_ik off`, started from `A_ik = 0`, `B_ij = 0`, the sum of the acting
actor's change statistics equals `(θ_BA − θ_AB) · B_kj`, whatever the own terms, the congestion
coefficient, and the rest of the state. -/
theorem cycle_affinity (S : TwoLayerSpec M N) (A : Rel M) (B : Config M N)
    {i k : Fin M} (hik : i ≠ k) (j : Fin N) (hAik : A i k = false) (hBij : B i j = false) :
    (S.evalB i A (flip B i j) - S.evalB i A B)
      + (S.evalA i (flipPair A i k) (flip B i j) - S.evalA i A (flip B i j))
      + (S.evalB i (flipPair A i k) B - S.evalB i (flipPair A i k) (flip B i j))
      + (S.evalA i A B - S.evalA i (flipPair A i k) B)
      = (S.θBA - S.θAB) * ind (B k j) := by
  unfold TwoLayerSpec.evalB TwoLayerSpec.evalA
  have hc1 := coupling_flipPair_diff A (flip B i j) hik
  have hc2 := coupling_flipPair_diff A B hik
  rw [hAik] at hc1 hc2
  simp only [Bool.not_false, ind_true, ind_false, sub_zero, one_mul] at hc1 hc2
  have hO : overlap (flip B i j) i k - overlap B i k = ind (B k j) := by
    unfold overlap
    rw [← Finset.sum_sub_distrib]
    have hk : flip B i j k = B k := flip_row_ne B j (Ne.symm hik)
    have hterm : ∀ j', ind (flip B i j i j') * ind (flip B i j k j')
        - ind (B i j') * ind (B k j') = if j' = j then ind (B k j) else 0 := by
      intro j'
      rw [hk, flip_apply]
      by_cases hj : j' = j
      · rw [if_pos hj, if_pos ⟨rfl, hj⟩, hj, hBij]; simp
      · rw [if_neg hj, if_neg (fun h => hj h.2)]; ring
    rw [Finset.sum_congr rfl (fun j' _ => hterm j')]
    simp
  linear_combination (S.θBA - S.θAB) * hc1 - (S.θBA - S.θAB) * hc2 + (S.θBA - S.θAB) * hO

/-- A concrete state in which the cycle is live: `A = 0`, `B_kj = 1`, `B_ij = 0`, for `M ≥ 2`,
`N ≥ 1`. -/
lemma live_cycle_state (hM : 2 ≤ M) (hN : 1 ≤ N) :
    ∃ (i k : Fin M) (j : Fin N) (A : Rel M) (B : Config M N),
      i ≠ k ∧ IsSymm A ∧ A i k = false ∧ B i j = false ∧ B k j = true := by
  refine ⟨⟨0, by omega⟩, ⟨1, by omega⟩, ⟨0, by omega⟩, fun _ _ => false,
    fun m _ => decide (m = ⟨1, by omega⟩), ?_, fun _ _ => rfl, rfl, ?_, ?_⟩
  · simp [Fin.ext_iff]
  · simp [Fin.ext_iff]
  · simp

/-- **Integrability, necessity (potential form).** If the two-layer process admits an exact
potential, the cross-layer coefficients are equal. -/
theorem integrability_necessary (S : TwoLayerSpec M N) (hM : 2 ≤ M) (hN : 1 ≤ N)
    (Φ : Rel M → Config M N → ℝ) (hΦ : S.IsExactPotential Φ) : S.θAB = S.θBA := by
  obtain ⟨hB, hA⟩ := hΦ
  obtain ⟨i, k, j, A, B, hik, hAs, hAik, hBij, hBkj⟩ := live_cycle_state (M := M) (N := N) hM hN
  have hcyc := cycle_affinity S A B hik j hAik hBij
  rw [hBkj, ind_true, mul_one] at hcyc
  have e1 := hB A hAs B i j
  have e2 := hA A hAs (flip B i j) i k hik
  have e3 := hB (flipPair A i k) (flipPair_symm hAs i k) (flip B i j) i j
  have e4 := hA (flipPair A i k) (flipPair_symm hAs i k) B i k hik
  rw [flip_flip] at e3
  rw [flipPair_flipPair hAs] at e4
  linarith

/-- Rate of the B-move `(i,j)` in the two-layer chain: binary logit on actor `i`'s B-evaluation
change, with a positive draw constant `cB`. -/
def rateB (S : TwoLayerSpec M N) (β cB : ℝ) (A : Rel M) (B : Config M N) (i : Fin M)
    (j : Fin N) : ℝ :=
  cB * glauber β (S.evalB i A (flip B i j) - S.evalB i A B)

/-- Rate of the A-move `{i,k}` proposed by actor `i`, under the forcing convention. -/
def rateA (S : TwoLayerSpec M N) (β cA : ℝ) (A : Rel M) (B : Config M N) (i k : Fin M) : ℝ :=
  cA * glauber β (S.evalA i (flipPair A i k) B - S.evalA i A B)

/-- **Per-proposer lemma for necessity (Kolmogorov form).** If a strictly positive
measure `μ` satisfies detailed balance for the B-moves and, separately, for each proposer's own
A-rate `rateA … i k` on all symmetric states, then `θ_AB = θ_BA`.

This is NOT yet reversibility of the chain with symmetric toggling: under the forcing convention either
endpoint may toggle the pair `{i,k}`, so the chain's rate is `rateA … i k + rateA … k i`, and
detailed balance of that sum is a weaker hypothesis than balance of each summand. The result
for the total rate is `symmetric_of_reversible_total` below, which reduces to this one under
`OwnAPairSymm`. -/
theorem symmetric_of_reversible (S : TwoLayerSpec M N) (hM : 2 ≤ M) (hN : 1 ≤ N)
    {β cB cA : ℝ} (hβ : 0 < β) (hcB : 0 < cB) (hcA : 0 < cA)
    (μ : Rel M → Config M N → ℝ) (hμ : ∀ A B, 0 < μ A B)
    (hdbB : ∀ A : Rel M, IsSymm A → ∀ (B : Config M N) (i : Fin M) (j : Fin N),
      μ A B * rateB S β cB A B i j = μ A (flip B i j) * rateB S β cB A (flip B i j) i j)
    (hdbA : ∀ A : Rel M, IsSymm A → ∀ (B : Config M N) (i k : Fin M), i ≠ k →
      μ A B * rateA S β cA A B i k
        = μ (flipPair A i k) B * rateA S β cA (flipPair A i k) B i k) :
    S.θAB = S.θBA := by
  obtain ⟨i, k, j, A, B, hik, hAs, hAik, hBij, hBkj⟩ := live_cycle_state (M := M) (N := N) hM hN
  have hcyc := cycle_affinity S A B hik j hAik hBij
  rw [hBkj, ind_true, mul_one] at hcyc
  have hA's := flipPair_symm hAs i k
  -- the four moves, with their change statistics
  set Δ1 := S.evalB i A (flip B i j) - S.evalB i A B with hΔ1
  set Δ2 := S.evalA i (flipPair A i k) (flip B i j) - S.evalA i A (flip B i j) with hΔ2
  set Δ3 := S.evalB i (flipPair A i k) B - S.evalB i (flipPair A i k) (flip B i j) with hΔ3
  set Δ4 := S.evalA i A B - S.evalA i (flipPair A i k) B with hΔ4
  have h1 : μ A B * (cB * glauber β Δ1) = μ A (flip B i j) * (cB * glauber β (-Δ1)) := by
    have := hdbB A hAs B i j
    unfold rateB at this
    rw [flip_flip, show S.evalB i A B - S.evalB i A (flip B i j) = -Δ1 by rw [hΔ1]; ring]
      at this
    exact this
  have h2 : μ A (flip B i j) * (cA * glauber β Δ2)
      = μ (flipPair A i k) (flip B i j) * (cA * glauber β (-Δ2)) := by
    have := hdbA A hAs (flip B i j) i k hik
    unfold rateA at this
    rw [flipPair_flipPair hAs,
      show S.evalA i A (flip B i j) - S.evalA i (flipPair A i k) (flip B i j) = -Δ2 by
        rw [hΔ2]; ring] at this
    exact this
  have h3 : μ (flipPair A i k) (flip B i j) * (cB * glauber β Δ3)
      = μ (flipPair A i k) B * (cB * glauber β (-Δ3)) := by
    have := hdbB (flipPair A i k) hA's (flip B i j) i j
    unfold rateB at this
    rw [flip_flip,
      show S.evalB i (flipPair A i k) (flip B i j) - S.evalB i (flipPair A i k) B = -Δ3 by
        rw [hΔ3]; ring] at this
    exact this
  have h4 : μ (flipPair A i k) B * (cA * glauber β Δ4) = μ A B * (cA * glauber β (-Δ4)) := by
    have := hdbA (flipPair A i k) hA's B i k hik
    unfold rateA at this
    rw [flipPair_flipPair hAs,
      show S.evalA i (flipPair A i k) B - S.evalA i A B = -Δ4 by rw [hΔ4]; ring] at this
    exact this
  -- cancel the draw constants
  have r1 : glauber β Δ1 * μ A B = μ A (flip B i j) * glauber β (-Δ1) := by
    apply mul_left_cancel₀ (ne_of_gt hcB); linear_combination h1
  have r2 : glauber β Δ2 * μ A (flip B i j)
      = μ (flipPair A i k) (flip B i j) * glauber β (-Δ2) := by
    apply mul_left_cancel₀ (ne_of_gt hcA); linear_combination h2
  have r3 : glauber β Δ3 * μ (flipPair A i k) (flip B i j)
      = μ (flipPair A i k) B * glauber β (-Δ3) := by
    apply mul_left_cancel₀ (ne_of_gt hcB); linear_combination h3
  have r4 : glauber β Δ4 * μ (flipPair A i k) B = μ A B * glauber β (-Δ4) := by
    apply mul_left_cancel₀ (ne_of_gt hcA); linear_combination h4
  -- ratios
  have p0 := hμ A B
  have p1 := hμ A (flip B i j)
  have p2 := hμ (flipPair A i k) (flip B i j)
  have p3 := hμ (flipPair A i k) B
  have q1 : glauber β Δ1 / glauber β (-Δ1) = μ A (flip B i j) / μ A B := by
    rw [div_eq_div_iff (ne_of_gt (glauber_pos _ _)) (ne_of_gt p0)]; exact r1
  have q2 : glauber β Δ2 / glauber β (-Δ2)
      = μ (flipPair A i k) (flip B i j) / μ A (flip B i j) := by
    rw [div_eq_div_iff (ne_of_gt (glauber_pos _ _)) (ne_of_gt p1)]; exact r2
  have q3 : glauber β Δ3 / glauber β (-Δ3)
      = μ (flipPair A i k) B / μ (flipPair A i k) (flip B i j) := by
    rw [div_eq_div_iff (ne_of_gt (glauber_pos _ _)) (ne_of_gt p2)]; exact r3
  have q4 : glauber β Δ4 / glauber β (-Δ4) = μ A B / μ (flipPair A i k) B := by
    rw [div_eq_div_iff (ne_of_gt (glauber_pos _ _)) (ne_of_gt p3)]; exact r4
  -- Kolmogorov: the product of the four edge ratios is one
  have key : exp (β * (Δ1 + Δ2 + Δ3 + Δ4)) = 1 := by
    rw [show β * (Δ1 + Δ2 + Δ3 + Δ4) = β * Δ1 + β * Δ2 + β * Δ3 + β * Δ4 by ring,
      exp_add, exp_add, exp_add, ← glauber_ratio β Δ1, ← glauber_ratio β Δ2,
      ← glauber_ratio β Δ3, ← glauber_ratio β Δ4, q1, q2, q3, q4]
    field_simp
  rw [Real.exp_eq_one_iff] at key
  have hsum : Δ1 + Δ2 + Δ3 + Δ4 = 0 := by
    rcases mul_eq_zero.mp key with h | h
    · exact absurd h (ne_of_gt hβ)
    · exact h
  linarith

/-! ### Necessity for the chain with symmetric toggling: total rates under the forcing convention -/

/-- The two endpoints of a pair see the same own-term change when the pair is flipped. This is
what the total-rate necessity theorem needs; it is weaker than `OwnAPotential`. -/
def TwoLayerSpec.OwnAPairSymm (S : TwoLayerSpec M N) : Prop :=
  ∀ A : Rel M, IsSymm A → ∀ i k : Fin M, i ≠ k →
    S.ownA k (flipPair A i k) - S.ownA k A = S.ownA i (flipPair A i k) - S.ownA i A

/-- Under `OwnAPotential` both endpoints' own-term changes equal the change of the common `ΨA`,
so they coincide. -/
lemma TwoLayerSpec.ownAPairSymm_of_potential (S : TwoLayerSpec M N) (hown : S.OwnAPotential) :
    S.OwnAPairSymm := by
  intro A hA i k hik
  rw [hown A hA i k hik, ← flipPair_comm hA i k]
  exact hown A hA k i (Ne.symm hik)

/-- Under `OwnAPairSymm`, the two proposers' A-evaluation changes on the pair flip coincide: the
own-term changes agree by hypothesis, and the coupling changes agree because `A` and `O(B)` are
both symmetric. -/
lemma evalA_pair_change_eq (S : TwoLayerSpec M N) (hsym : S.OwnAPairSymm) {A : Rel M}
    (hA : IsSymm A) (B : Config M N) {i k : Fin M} (hik : i ≠ k) :
    S.evalA k (flipPair A i k) B - S.evalA k A B
      = S.evalA i (flipPair A i k) B - S.evalA i A B := by
  unfold TwoLayerSpec.evalA
  have h1 := hsym A hA i k hik
  have h2 := coupling_flipPair_diff A B hik
  have h3 : coupling (flipPair A i k) B k - coupling A B k
      = (ind (!A i k) - ind (A i k)) * overlap B i k := by
    rw [← flipPair_comm hA i k, hA i k, overlap_comm B i k]
    exact coupling_flipPair_diff A B (Ne.symm hik)
  linear_combination h1 + S.θBA * h3 - S.θBA * h2

/-- Under `OwnAPairSymm`, the two proposers' rates for the same pair flip coincide. -/
lemma rateA_pair_symm (S : TwoLayerSpec M N) (hsym : S.OwnAPairSymm) (β cA : ℝ) {A : Rel M}
    (hA : IsSymm A) (B : Config M N) {i k : Fin M} (hik : i ≠ k) :
    rateA S β cA A B k i = rateA S β cA A B i k := by
  unfold rateA
  rw [flipPair_comm hA i k, evalA_pair_change_eq S hsym hA B hik]

/-- Total rate of the A-move `{i,k}` under the forcing convention: either endpoint
may propose the toggle, so the chain's rate is the sum of the two proposers' rates. -/
def rateATotal (S : TwoLayerSpec M N) (β cA : ℝ) (A : Rel M) (B : Config M N) (i k : Fin M) :
    ℝ :=
  rateA S β cA A B i k + rateA S β cA A B k i

/-- Under `OwnAPairSymm` the total rate is the per-proposer rate with the draw constant doubled. -/
lemma rateATotal_eq (S : TwoLayerSpec M N) (hsym : S.OwnAPairSymm) (β cA : ℝ) {A : Rel M}
    (hA : IsSymm A) (B : Config M N) {i k : Fin M} (hik : i ≠ k) :
    rateATotal S β cA A B i k = rateA S β (2 * cA) A B i k := by
  unfold rateATotal
  rw [rateA_pair_symm S hsym β cA hA B hik]
  unfold rateA; ring

/-- **Integrability, necessity (Kolmogorov form, total rates).** If a strictly positive measure `μ`
satisfies detailed balance for the two-layer binary-logit chain with symmetric toggling, with the
A-move `{i,k}` at its total rate `rateA … i k + rateA … k i`, on all symmetric states, then
`θ_AB = θ_BA`. Contrapositive: under asymmetric coupling the chain is reversible with respect to
no measure, so no Gibbs measure of any potential is its stationary distribution.

Hypotheses, in plain words. (i) Single-flip (Glauber) revision, one entry of `B` or one
unordered pair of `A` per move, accepted by the binary logit. (ii) An own-term hypothesis on the
relational layer: `hsym : S.OwnAPairSymm`, that both endpoints of a pair see the same own-term
change on its flip. `OwnAPotential` implies it (`ownAPairSymm_of_potential`), so the hypothesis
sufficiency already carries suffices here too
(`symmetric_of_reversible_total_of_potential`). Density own terms satisfy it and ego-covariate
own terms do NOT (both proved below); transitive-triad (satisfies) and degree-squared (does not)
are argued, not formalized. Without (ii) the conclusion is false:
with `M = 2`, `N = 1`, endpoint own terms `0 · A_12` and `10 · A_12`, `θ_BA = 1` and
`θ_AB ≈ 0.7634`, the total-rate chain is exactly reversible (stationary-law detailed-balance
residual at machine precision), although the
coefficients differ. -/
theorem symmetric_of_reversible_total (S : TwoLayerSpec M N) (hM : 2 ≤ M) (hN : 1 ≤ N)
    (hsym : S.OwnAPairSymm)
    {β cB cA : ℝ} (hβ : 0 < β) (hcB : 0 < cB) (hcA : 0 < cA)
    (μ : Rel M → Config M N → ℝ) (hμ : ∀ A B, 0 < μ A B)
    (hdbB : ∀ A : Rel M, IsSymm A → ∀ (B : Config M N) (i : Fin M) (j : Fin N),
      μ A B * rateB S β cB A B i j = μ A (flip B i j) * rateB S β cB A (flip B i j) i j)
    (hdbA : ∀ A : Rel M, IsSymm A → ∀ (B : Config M N) (i k : Fin M), i ≠ k →
      μ A B * rateATotal S β cA A B i k
        = μ (flipPair A i k) B * rateATotal S β cA (flipPair A i k) B i k) :
    S.θAB = S.θBA := by
  refine symmetric_of_reversible S hM hN hβ hcB (mul_pos two_pos hcA) μ hμ hdbB ?_
  intro A hA B i k hik
  have h := hdbA A hA B i k hik
  rwa [rateATotal_eq S hsym β cA hA B hik,
    rateATotal_eq S hsym β cA (flipPair_symm hA i k) B hik] at h

/-- `symmetric_of_reversible_total` with the own-term hypothesis `OwnAPotential` in place
of the weaker `OwnAPairSymm`. -/
theorem symmetric_of_reversible_total_of_potential (S : TwoLayerSpec M N) (hM : 2 ≤ M)
    (hN : 1 ≤ N) (hown : S.OwnAPotential)
    {β cB cA : ℝ} (hβ : 0 < β) (hcB : 0 < cB) (hcA : 0 < cA)
    (μ : Rel M → Config M N → ℝ) (hμ : ∀ A B, 0 < μ A B)
    (hdbB : ∀ A : Rel M, IsSymm A → ∀ (B : Config M N) (i : Fin M) (j : Fin N),
      μ A B * rateB S β cB A B i j = μ A (flip B i j) * rateB S β cB A (flip B i j) i j)
    (hdbA : ∀ A : Rel M, IsSymm A → ∀ (B : Config M N) (i k : Fin M), i ≠ k →
      μ A B * rateATotal S β cA A B i k
        = μ (flipPair A i k) B * rateATotal S β cA (flipPair A i k) B i k) :
    S.θAB = S.θBA :=
  symmetric_of_reversible_total S hM hN (S.ownAPairSymm_of_potential hown) hβ hcB hcA μ hμ
    hdbB hdbA

/-! ### Cycle affinity in log-ratio units -/

/-- **Cycle affinity, log-ratio form.** Around the elementary cycle `B_ij on, A_ik on, B_ij off,
A_ik off` (started from `A_ik = 0`, `B_ij = 0`, symmetric `A`), the log of the product of the
acting actor's forward rates over the product of the reverse rates equals
`β · (θ_BA − θ_AB) · B_kj`: the affinity of `cycle_affinity` times `β`, not the affinity itself.
The draw constants `cB`, `cA` cancel (each appears twice in the numerator and twice in the
denominator); they need only be positive. -/
theorem cycle_log_ratio (S : TwoLayerSpec M N) (β : ℝ) {cB cA : ℝ} (hcB : 0 < cB)
    (hcA : 0 < cA)
    {A : Rel M} (hA : IsSymm A) (B : Config M N) {i k : Fin M} (hik : i ≠ k) (j : Fin N)
    (hAik : A i k = false) (hBij : B i j = false) :
    Real.log
      ((rateB S β cB A B i j * rateA S β cA A (flip B i j) i k
          * rateB S β cB (flipPair A i k) (flip B i j) i j * rateA S β cA (flipPair A i k) B i k)
        / (rateB S β cB A (flip B i j) i j * rateA S β cA (flipPair A i k) (flip B i j) i k
          * rateB S β cB (flipPair A i k) B i j * rateA S β cA A B i k))
      = β * ((S.θBA - S.θAB) * ind (B k j)) := by
  have hcyc := cycle_affinity S A B hik j hAik hBij
  set Δ1 := S.evalB i A (flip B i j) - S.evalB i A B with hΔ1
  set Δ2 := S.evalA i (flipPair A i k) (flip B i j) - S.evalA i A (flip B i j) with hΔ2
  set Δ3 := S.evalB i (flipPair A i k) B - S.evalB i (flipPair A i k) (flip B i j) with hΔ3
  set Δ4 := S.evalA i A B - S.evalA i (flipPair A i k) B with hΔ4
  have f1 : rateB S β cB A B i j = cB * glauber β Δ1 := by
    unfold rateB; rw [hΔ1]
  have f2 : rateA S β cA A (flip B i j) i k = cA * glauber β Δ2 := by
    unfold rateA; rw [hΔ2]
  have f3 : rateB S β cB (flipPair A i k) (flip B i j) i j = cB * glauber β Δ3 := by
    unfold rateB; rw [flip_flip, hΔ3]
  have f4 : rateA S β cA (flipPair A i k) B i k = cA * glauber β Δ4 := by
    unfold rateA; rw [flipPair_flipPair hA, hΔ4]
  have r1 : rateB S β cB A (flip B i j) i j = cB * glauber β (-Δ1) := by
    unfold rateB
    rw [flip_flip, show S.evalB i A B - S.evalB i A (flip B i j) = -Δ1 by rw [hΔ1]; ring]
  have r2 : rateA S β cA (flipPair A i k) (flip B i j) i k = cA * glauber β (-Δ2) := by
    unfold rateA
    rw [flipPair_flipPair hA,
      show S.evalA i A (flip B i j) - S.evalA i (flipPair A i k) (flip B i j) = -Δ2 by
        rw [hΔ2]; ring]
  have r3 : rateB S β cB (flipPair A i k) B i j = cB * glauber β (-Δ3) := by
    unfold rateB
    rw [show S.evalB i (flipPair A i k) (flip B i j) - S.evalB i (flipPair A i k) B = -Δ3 by
      rw [hΔ3]; ring]
  have r4 : rateA S β cA A B i k = cA * glauber β (-Δ4) := by
    unfold rateA
    rw [show S.evalA i (flipPair A i k) B - S.evalA i A B = -Δ4 by rw [hΔ4]; ring]
  rw [f1, f2, f3, f4, r1, r2, r3, r4]
  have key : cB * glauber β Δ1 * (cA * glauber β Δ2) * (cB * glauber β Δ3) * (cA * glauber β Δ4)
      / (cB * glauber β (-Δ1) * (cA * glauber β (-Δ2)) * (cB * glauber β (-Δ3))
          * (cA * glauber β (-Δ4)))
      = exp (β * (Δ1 + Δ2 + Δ3 + Δ4)) := by
    rw [show β * (Δ1 + Δ2 + Δ3 + Δ4) = β * Δ1 + β * Δ2 + β * Δ3 + β * Δ4 by ring,
      exp_add, exp_add, exp_add, ← glauber_ratio β Δ1, ← glauber_ratio β Δ2,
      ← glauber_ratio β Δ3, ← glauber_ratio β Δ4]
    have g1 := (glauber_pos β (-Δ1)).ne'
    have g2 := (glauber_pos β (-Δ2)).ne'
    have g3 := (glauber_pos β (-Δ3)).ne'
    have g4 := (glauber_pos β (-Δ4)).ne'
    have hB' := hcB.ne'
    have hA' := hcA.ne'
    field_simp
  rw [key, log_exp]
  linear_combination β * hcyc

/-- **Cycle affinity, log-ratio form, with symmetric toggling.** The same identity with the
A-moves at their total rate under the forcing convention, given `OwnAPairSymm`; the factor `2`
from the two proposers cancels as the draw constants do. -/
theorem cycle_log_ratio_total (S : TwoLayerSpec M N) (hsym : S.OwnAPairSymm) (β : ℝ)
    {cB cA : ℝ} (hcB : 0 < cB) (hcA : 0 < cA) {A : Rel M} (hA : IsSymm A) (B : Config M N)
    {i k : Fin M} (hik : i ≠ k) (j : Fin N) (hAik : A i k = false) (hBij : B i j = false) :
    Real.log
      ((rateB S β cB A B i j * rateATotal S β cA A (flip B i j) i k
          * rateB S β cB (flipPair A i k) (flip B i j) i j
          * rateATotal S β cA (flipPair A i k) B i k)
        / (rateB S β cB A (flip B i j) i j * rateATotal S β cA (flipPair A i k) (flip B i j) i k
          * rateB S β cB (flipPair A i k) B i j * rateATotal S β cA A B i k))
      = β * ((S.θBA - S.θAB) * ind (B k j)) := by
  have hA' := flipPair_symm hA i k
  rw [rateATotal_eq S hsym β cA hA (flip B i j) hik, rateATotal_eq S hsym β cA hA' B hik,
    rateATotal_eq S hsym β cA hA' (flip B i j) hik, rateATotal_eq S hsym β cA hA B hik]
  exact cycle_log_ratio S β hcB (mul_pos two_pos hcA) hA B hik j hAik hBij

/-! ### Own-term examples: which relational-layer own terms satisfy `OwnAPotential` -/

/-- Degree of actor `i` in the relational layer. -/
def relDegree (A : Rel M) (i : Fin M) : ℝ := ∑ k ∈ univ.erase i, ind (A i k)

/-- Number of ties, as half the sum of the degrees. -/
def tieCount (A : Rel M) : ℝ := (1 / 2) * ∑ p, ∑ k ∈ univ.erase p, ind (A p k)

lemma relDegree_eq_pairStat (A : Rel M) (i : Fin M) :
    relDegree A i = pairStat (fun _ _ => 1) A i := by
  simp [relDegree, pairStat]

lemma tieCount_eq_pairPotential (A : Rel M) : tieCount A = pairPotential (fun _ _ => 1) A := by
  simp [tieCount, pairPotential]

/-- **Example (positive).** A symmetric density own term `θ · deg_i(A)` satisfies
`OwnAPotential`, with potential `θ · (number of ties)`. -/
theorem density_ownAPotential (S : TwoLayerSpec M N) (θ : ℝ)
    (hown : ∀ i A, S.ownA i A = θ * relDegree A i) (hΨ : ∀ A, S.ΨA A = θ * tieCount A) :
    S.OwnAPotential := by
  intro A hA i k hik
  rw [hown, hown, hΨ, hΨ, relDegree_eq_pairStat, relDegree_eq_pairStat,
    tieCount_eq_pairPotential, tieCount_eq_pairPotential, ← mul_sub, ← mul_sub,
    pairStat_flipPair_diff (fun _ _ => 1) A hik,
    pairPotential_flipPair_diff (fun _ _ => 1) (fun _ _ => rfl) hA hik]

/-- **Example (negative).** An ego-covariate own term `z_i · deg_i(A)` admits no A-potential
unless the covariate is constant: on the empty layer, adding the tie `{i,k}` changes actor `i`'s
own term by `z_i` and actor `k`'s by `z_k`, and a common `ΨA` cannot match both. The same
argument applies to any own term whose change on a pair flip depends on which endpoint
evaluates it (degree-squared, actor-specific constants). -/
theorem egoCovariate_not_ownAPotential (S : TwoLayerSpec M N) (z : Fin M → ℝ)
    (hown : ∀ i A, S.ownA i A = z i * relDegree A i) {i k : Fin M} (hik : i ≠ k)
    (hz : z i ≠ z k) : ¬ S.OwnAPotential := by
  intro hpot
  have hA0 : IsSymm (fun _ _ => false : Rel M) := fun _ _ => rfl
  have h1 := hpot _ hA0 i k hik
  have h2 := hpot _ hA0 k i (Ne.symm hik)
  rw [hown, hown, relDegree_eq_pairStat, relDegree_eq_pairStat, ← mul_sub,
    pairStat_flipPair_diff (fun _ _ => 1) _ hik] at h1
  rw [hown, hown, relDegree_eq_pairStat, relDegree_eq_pairStat, ← mul_sub,
    pairStat_flipPair_diff (fun _ _ => 1) _ (Ne.symm hik), flipPair_comm hA0 i k] at h2
  simp only [Bool.not_false, ind_true, ind_false, sub_zero, mul_one] at h1 h2
  exact hz (h1.trans h2.symm)

end SaomNK.TwoLayer

end
