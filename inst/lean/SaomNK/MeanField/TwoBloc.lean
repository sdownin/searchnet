import Mathlib.Analysis.SpecialFunctions.Trigonometric.DerivHyp
import Mathlib.Analysis.Calculus.Deriv.Inv
import Mathlib.LinearAlgebra.Matrix.Notation
import Mathlib.Topology.Order.IntermediateValue
import Mathlib.Tactic.LinearCombination
import Mathlib.Tactic.FieldSimp
import Mathlib.Tactic.FinCases
import Mathlib.Tactic.Linarith

/-!
# Two-population mean-field coordination

Two blocs of actors, `A` and `B`, carry mean adoption levels (magnetizations, on the `±1` scale)
`mA, mB ∈ [-1, 1]`. The logit mean-field map is

    F(mA, mB) = (tanh (J mA + Jx mB + hA), tanh (J mB + Jx mA + hB)),

with within-bloc coupling `J`, cross-bloc coupling `Jx`, and bloc fields `hA, hB`: the
two-population version of discrete choice with social interactions (Brock and Durlauf 2001) and
of the Curie-Weiss model. Fixed points are equilibria; a fixed point `m*` is linearly stable for
`dm/dt = -m + F(m)` when every eigenvalue of `DF(m*)` is below 1 (`EigenBelowOne`).

Formalized (fields zero where stated):

1. the eigenstructure of `[[a, b], [b, a]]` and its stability test;
2. aligned states `(m, m)` solve `m = tanh ((J + Jx) m)`, polarized states `(m, -m)` solve
   `m = tanh ((J - Jx) m)`;
3. the derivative `tanh' = 1 - tanh²` and the Jacobian `DF`, entry by entry;
4. linear stability: for `Jx ≥ 0` each state is stable iff `(J + Jx)(1 - m²) < 1`;
5. existence: `m = tanh (c m)` has a nonzero root iff `c > 1` (the single-population
   multiplicity threshold), so a nonzero aligned equilibrium exists iff `J + Jx > 1` and a
   polarized one iff `J - Jx > 1`.

Not formalized: the Fréchet derivative of `F` as a map on `ℝ × ℝ` (only its four continuous
partial derivatives), uniqueness of the positive root, equilibria other than `(m, m)` and
`(m, -m)`, nonzero fields, and global stability.
-/

open Matrix

noncomputable section

namespace SaomNK.MeanField

/-! ## 1. Symmetric 2x2 matrices `[[a, b], [b, a]]` -/

/-- `(1, 1)` is an eigenvector of `[[a, b], [b, a]]` with eigenvalue `a + b`. -/
theorem symm2_mulVec_plus (a b : ℝ) :
    !![a, b; b, a] *ᵥ ![1, 1] = (a + b) • ![(1 : ℝ), 1] := by
  ext i
  fin_cases i
  all_goals
    first
    | (simp; done)
    | (simp; ring1)

/-- `(1, -1)` is an eigenvector of `[[a, b], [b, a]]` with eigenvalue `a - b`. -/
theorem symm2_mulVec_minus (a b : ℝ) :
    !![a, b; b, a] *ᵥ ![1, -1] = (a - b) • ![(1 : ℝ), -1] := by
  ext i
  fin_cases i
  all_goals
    first
    | (simp; done)
    | (simp; ring1)

lemma vec_one_one_ne_zero : (![1, 1] : Fin 2 → ℝ) ≠ 0 := by
  intro h
  have h0 := congrFun h 0
  simp at h0

lemma vec_one_neg_one_ne_zero : (![1, -1] : Fin 2 → ℝ) ≠ 0 := by
  intro h
  have h0 := congrFun h 0
  simp at h0

/-- The only real eigenvalues of `[[a, b], [b, a]]` are `a + b` and `a - b`. -/
theorem symm2_eigenvalue_cases (a b μ : ℝ) (v : Fin 2 → ℝ) (hv : v ≠ 0)
    (h : !![a, b; b, a] *ᵥ v = μ • v) : μ = a + b ∨ μ = a - b := by
  have h0 : a * v 0 + b * v 1 = μ * v 0 := by
    have h' := congrFun h 0
    simp only [Matrix.mulVec, dotProduct, Fin.sum_univ_two, Matrix.of_apply,
      Matrix.cons_val', Matrix.cons_val_zero, Matrix.cons_val_one,
      Pi.smul_apply, smul_eq_mul] at h'
    exact h'
  have h1 : b * v 0 + a * v 1 = μ * v 1 := by
    have h' := congrFun h 1
    simp only [Matrix.mulVec, dotProduct, Fin.sum_univ_two, Matrix.of_apply,
      Matrix.cons_val', Matrix.cons_val_zero, Matrix.cons_val_one,
      Pi.smul_apply, smul_eq_mul] at h'
    exact h'
  have hs : (a + b - μ) * (v 0 + v 1) = 0 := by linear_combination h0 + h1
  have hd : (a - b - μ) * (v 0 - v 1) = 0 := by linear_combination h0 - h1
  by_cases hp : μ = a + b
  · exact Or.inl hp
  by_cases hm : μ = a - b
  · exact Or.inr hm
  exfalso
  have e1 : v 0 + v 1 = 0 := by
    rcases mul_eq_zero.mp hs with h' | h'
    · exact absurd (by linarith : μ = a + b) hp
    · exact h'
  have e2 : v 0 - v 1 = 0 := by
    rcases mul_eq_zero.mp hd with h' | h'
    · exact absurd (by linarith : μ = a - b) hm
    · exact h'
  have z0 : v 0 = 0 := by linarith
  have z1 : v 1 = 0 := by linarith
  apply hv
  ext i
  fin_cases i
  all_goals simp [z0, z1]

/-- Linear stability test used throughout: every real eigenvalue of `M` is below 1. For the
symmetric Jacobians below every eigenvalue is real, so nothing is lost by testing real ones. -/
def EigenBelowOne (M : Matrix (Fin 2) (Fin 2) ℝ) : Prop :=
  ∀ (μ : ℝ) (v : Fin 2 → ℝ), v ≠ 0 → M *ᵥ v = μ • v → μ < 1

/-- All eigenvalues of `[[a, b], [b, a]]` are below 1 iff `a + b < 1` and `a - b < 1`. -/
theorem symm2_all_eigen_lt_one_iff (a b : ℝ) :
    EigenBelowOne !![a, b; b, a] ↔ (a + b < 1 ∧ a - b < 1) := by
  constructor
  · intro h
    exact ⟨h (a + b) ![1, 1] vec_one_one_ne_zero (symm2_mulVec_plus a b),
      h (a - b) ![1, -1] vec_one_neg_one_ne_zero (symm2_mulVec_minus a b)⟩
  · rintro ⟨h1, h2⟩ μ v hv hμ
    rcases symm2_eigenvalue_cases a b μ v hv hμ with h | h
    · rw [h]; exact h1
    · rw [h]; exact h2

/-- Scaled form used at the symmetric states: `[[J s, Jx s], [Jx s, J s]]` has eigenvalues
`(J + Jx) s` (eigenvector `(1, 1)`) and `(J - Jx) s` (eigenvector `(1, -1)`). -/
theorem symm2_eigen_scaled (J Jx s : ℝ) :
    !![J * s, Jx * s; Jx * s, J * s] *ᵥ ![1, 1] = ((J + Jx) * s) • ![(1 : ℝ), 1] ∧
    !![J * s, Jx * s; Jx * s, J * s] *ᵥ ![1, -1] = ((J - Jx) * s) • ![(1 : ℝ), -1] := by
  constructor
  · rw [show (J + Jx) * s = J * s + Jx * s by ring]
    exact symm2_mulVec_plus _ _
  · rw [show (J - Jx) * s = J * s - Jx * s by ring]
    exact symm2_mulVec_minus _ _

/-! ## The derivative of `tanh` -/

/-- Derivative factor of `tanh` at `u`: `tanh' u = 1 - tanh u ^ 2` (that is, `sech u ^ 2`). -/
def dtanh (u : ℝ) : ℝ := 1 - Real.tanh u ^ 2

lemma dtanh_pos (u : ℝ) : 0 < dtanh u := by
  unfold dtanh
  have := Real.tanh_sq_lt_one u
  linarith

lemma dtanh_neg (u : ℝ) : dtanh (-u) = dtanh u := by
  unfold dtanh
  rw [Real.tanh_neg, neg_sq]

lemma sinh_div_cosh_sq_mul (x : ℝ) :
    (Real.sinh x / Real.cosh x) ^ 2 * Real.cosh x ^ 2 = Real.sinh x ^ 2 := by
  have hc : Real.cosh x ≠ 0 := (Real.cosh_pos x).ne'
  rw [← mul_pow, div_mul_cancel₀ _ hc]

/-- `1 - tanh x ^ 2` equals the quotient-rule derivative of `sinh / cosh`. -/
lemma dtanh_eq (x : ℝ) :
    dtanh x = (Real.cosh x * Real.cosh x - Real.sinh x * Real.sinh x) / Real.cosh x ^ 2 := by
  have hc : Real.cosh x ≠ 0 := (Real.cosh_pos x).ne'
  have hc2 : Real.cosh x ^ 2 ≠ 0 := pow_ne_zero 2 hc
  rw [eq_div_iff hc2]
  unfold dtanh
  rw [Real.tanh_eq_sinh_div_cosh, sub_mul, sinh_div_cosh_sq_mul]
  ring1

/-- **Derivative of `tanh`**: `HasDerivAt tanh (1 - tanh x ^ 2) x`. -/
theorem hasDerivAt_tanh (x : ℝ) : HasDerivAt Real.tanh (dtanh x) x := by
  have hc : Real.cosh x ≠ 0 := (Real.cosh_pos x).ne'
  have hfun : Real.tanh = fun y => Real.sinh y / Real.cosh y := by
    funext y
    exact Real.tanh_eq_sinh_div_cosh y
  rw [hfun]
  exact ((Real.hasDerivAt_sinh x).fun_div (Real.hasDerivAt_cosh x) hc).congr_deriv
    (dtanh_eq x).symm

/-- Chain rule for `tanh ∘ u`. -/
theorem hasDerivAt_tanh_comp {u : ℝ → ℝ} {u' x : ℝ} (hu : HasDerivAt u u' x) :
    HasDerivAt (fun t => Real.tanh (u t)) (dtanh (u x) * u') x :=
  (hasDerivAt_tanh (u x)).comp x hu

lemma hasDerivAt_affine_left (k c d x : ℝ) :
    HasDerivAt (fun t : ℝ => k * t + c + d) k x :=
  ((((hasDerivAt_id' x).const_mul k).add_const c).add_const d).congr_deriv (mul_one k)

lemma hasDerivAt_affine_mid (k c d x : ℝ) :
    HasDerivAt (fun t : ℝ => c + k * t + d) k x :=
  ((((hasDerivAt_id' x).const_mul k).const_add c).add_const d).congr_deriv (mul_one k)

/-- `tanh` is continuous (from its derivative; the pinned Mathlib has no such lemma). -/
theorem continuous_tanh : Continuous Real.tanh :=
  continuous_iff_continuousAt.2 fun x => (hasDerivAt_tanh x).continuousAt

/-! ## 2. The mean-field map and its symmetric fixed points -/

/-- Mean-field map of the two-bloc model; `m = (mA, mB)`. -/
def F (J Jx hA hB : ℝ) (m : ℝ × ℝ) : ℝ × ℝ :=
  (Real.tanh (J * m.1 + Jx * m.2 + hA), Real.tanh (J * m.2 + Jx * m.1 + hB))

/-- **Aligned state** (`hA = hB = 0`): if `m = tanh ((J + Jx) m)`, then `(m, m)` is a fixed
point of `F`. -/
theorem fixed_point_symmetric (J Jx m : ℝ) (h : m = Real.tanh ((J + Jx) * m)) :
    F J Jx 0 0 (m, m) = (m, m) := by
  have e : Real.tanh (J * m + Jx * m + 0) = m := by
    rw [show J * m + Jx * m + 0 = (J + Jx) * m by ring]
    exact h.symm
  show (Real.tanh (J * m + Jx * m + 0), Real.tanh (J * m + Jx * m + 0)) = (m, m)
  rw [e]

/-- **Polarized state** (`hA = hB = 0`): if `m = tanh ((J - Jx) m)`, then `(m, -m)` is a fixed
point of `F` (oddness of `tanh`). -/
theorem fixed_point_antisymmetric (J Jx m : ℝ) (h : m = Real.tanh ((J - Jx) * m)) :
    F J Jx 0 0 (m, -m) = (m, -m) := by
  have e1 : Real.tanh (J * m + Jx * -m + 0) = m := by
    rw [show J * m + Jx * -m + 0 = (J - Jx) * m by ring]
    exact h.symm
  have e2 : Real.tanh (J * -m + Jx * m + 0) = -m := by
    rw [show J * -m + Jx * m + 0 = -((J - Jx) * m) by ring, Real.tanh_neg, ← h]
  show (Real.tanh (J * m + Jx * -m + 0), Real.tanh (J * -m + Jx * m + 0)) = (m, -m)
  rw [e1, e2]

/-! ## 3. The Jacobian -/

/-- Jacobian of `F` at `m`: row `i` holds the partial derivatives of component `i`
(proven entry by entry in `hasDerivAt_F1_fst` ... `hasDerivAt_F2_snd`). -/
def DF (J Jx hA hB : ℝ) (m : ℝ × ℝ) : Matrix (Fin 2) (Fin 2) ℝ :=
  !![J * dtanh (J * m.1 + Jx * m.2 + hA), Jx * dtanh (J * m.1 + Jx * m.2 + hA);
     Jx * dtanh (J * m.2 + Jx * m.1 + hB), J * dtanh (J * m.2 + Jx * m.1 + hB)]

lemma DF_apply_00 (J Jx hA hB : ℝ) (m : ℝ × ℝ) :
    DF J Jx hA hB m 0 0 = J * dtanh (J * m.1 + Jx * m.2 + hA) := by
  rfl

lemma DF_apply_01 (J Jx hA hB : ℝ) (m : ℝ × ℝ) :
    DF J Jx hA hB m 0 1 = Jx * dtanh (J * m.1 + Jx * m.2 + hA) := by
  rfl

lemma DF_apply_10 (J Jx hA hB : ℝ) (m : ℝ × ℝ) :
    DF J Jx hA hB m 1 0 = Jx * dtanh (J * m.2 + Jx * m.1 + hB) := by
  rfl

lemma DF_apply_11 (J Jx hA hB : ℝ) (m : ℝ × ℝ) :
    DF J Jx hA hB m 1 1 = J * dtanh (J * m.2 + Jx * m.1 + hB) := by
  rfl

/-- `∂F₁/∂mA = DF 0 0`. -/
theorem hasDerivAt_F1_fst (J Jx hA hB : ℝ) (m : ℝ × ℝ) :
    HasDerivAt (fun t => (F J Jx hA hB (t, m.2)).1) (DF J Jx hA hB m 0 0) m.1 := by
  rw [DF_apply_00]
  show HasDerivAt (fun t => Real.tanh (J * t + Jx * m.2 + hA))
    (J * dtanh (J * m.1 + Jx * m.2 + hA)) m.1
  exact (hasDerivAt_tanh_comp (hasDerivAt_affine_left J (Jx * m.2) hA m.1)).congr_deriv
    (mul_comm _ _)

/-- `∂F₁/∂mB = DF 0 1`. -/
theorem hasDerivAt_F1_snd (J Jx hA hB : ℝ) (m : ℝ × ℝ) :
    HasDerivAt (fun t => (F J Jx hA hB (m.1, t)).1) (DF J Jx hA hB m 0 1) m.2 := by
  rw [DF_apply_01]
  show HasDerivAt (fun t => Real.tanh (J * m.1 + Jx * t + hA))
    (Jx * dtanh (J * m.1 + Jx * m.2 + hA)) m.2
  exact (hasDerivAt_tanh_comp (hasDerivAt_affine_mid Jx (J * m.1) hA m.2)).congr_deriv
    (mul_comm _ _)

/-- `∂F₂/∂mA = DF 1 0`. -/
theorem hasDerivAt_F2_fst (J Jx hA hB : ℝ) (m : ℝ × ℝ) :
    HasDerivAt (fun t => (F J Jx hA hB (t, m.2)).2) (DF J Jx hA hB m 1 0) m.1 := by
  rw [DF_apply_10]
  show HasDerivAt (fun t => Real.tanh (J * m.2 + Jx * t + hB))
    (Jx * dtanh (J * m.2 + Jx * m.1 + hB)) m.1
  exact (hasDerivAt_tanh_comp (hasDerivAt_affine_mid Jx (J * m.2) hB m.1)).congr_deriv
    (mul_comm _ _)

/-- `∂F₂/∂mB = DF 1 1`. -/
theorem hasDerivAt_F2_snd (J Jx hA hB : ℝ) (m : ℝ × ℝ) :
    HasDerivAt (fun t => (F J Jx hA hB (m.1, t)).2) (DF J Jx hA hB m 1 1) m.2 := by
  rw [DF_apply_11]
  show HasDerivAt (fun t => Real.tanh (J * t + Jx * m.1 + hB))
    (J * dtanh (J * m.2 + Jx * m.1 + hB)) m.2
  exact (hasDerivAt_tanh_comp (hasDerivAt_affine_left J (Jx * m.1) hB m.2)).congr_deriv
    (mul_comm _ _)

/-- Jacobian at the aligned state `(m, m)` (fields zero): `[[J s, Jx s], [Jx s, J s]]` with
`s = 1 - tanh ((J + Jx) m) ^ 2`. -/
theorem DF_symmetric (J Jx m : ℝ) :
    DF J Jx 0 0 (m, m) =
      !![J * dtanh ((J + Jx) * m), Jx * dtanh ((J + Jx) * m);
         Jx * dtanh ((J + Jx) * m), J * dtanh ((J + Jx) * m)] := by
  have e : J * m + Jx * m + 0 = (J + Jx) * m := by ring
  have hDF : DF J Jx 0 0 (m, m) =
      !![J * dtanh (J * m + Jx * m + 0), Jx * dtanh (J * m + Jx * m + 0);
         Jx * dtanh (J * m + Jx * m + 0), J * dtanh (J * m + Jx * m + 0)] := rfl
  rw [hDF, e]

/-- Jacobian at the polarized state `(m, -m)` (fields zero): the same form, with
`s = 1 - tanh ((J - Jx) m) ^ 2`. -/
theorem DF_antisymmetric (J Jx m : ℝ) :
    DF J Jx 0 0 (m, -m) =
      !![J * dtanh ((J - Jx) * m), Jx * dtanh ((J - Jx) * m);
         Jx * dtanh ((J - Jx) * m), J * dtanh ((J - Jx) * m)] := by
  have e1 : J * m + Jx * -m + 0 = (J - Jx) * m := by ring
  have e2 : J * -m + Jx * m + 0 = -((J - Jx) * m) := by ring
  have hDF : DF J Jx 0 0 (m, -m) =
      !![J * dtanh (J * m + Jx * -m + 0), Jx * dtanh (J * m + Jx * -m + 0);
         Jx * dtanh (J * -m + Jx * m + 0), J * dtanh (J * -m + Jx * m + 0)] := rfl
  rw [hDF, e1, e2, dtanh_neg]

/-- Eigenpairs of the Jacobian at the aligned state: `(J + Jx) s` on `(1, 1)` and `(J - Jx) s`
on `(1, -1)`, `s = 1 - tanh ((J + Jx) m) ^ 2`. -/
theorem DF_symmetric_eigen (J Jx m : ℝ) :
    DF J Jx 0 0 (m, m) *ᵥ ![1, 1] = ((J + Jx) * dtanh ((J + Jx) * m)) • ![(1 : ℝ), 1] ∧
    DF J Jx 0 0 (m, m) *ᵥ ![1, -1] = ((J - Jx) * dtanh ((J + Jx) * m)) • ![(1 : ℝ), -1] := by
  rw [DF_symmetric]
  exact symm2_eigen_scaled J Jx _

/-- Eigenpairs of the Jacobian at the polarized state: `(J + Jx) s` on `(1, 1)` and
`(J - Jx) s` on `(1, -1)`, `s = 1 - tanh ((J - Jx) m) ^ 2`. -/
theorem DF_antisymmetric_eigen (J Jx m : ℝ) :
    DF J Jx 0 0 (m, -m) *ᵥ ![1, 1] = ((J + Jx) * dtanh ((J - Jx) * m)) • ![(1 : ℝ), 1] ∧
    DF J Jx 0 0 (m, -m) *ᵥ ![1, -1] = ((J - Jx) * dtanh ((J - Jx) * m)) • ![(1 : ℝ), -1] := by
  rw [DF_antisymmetric]
  exact symm2_eigen_scaled J Jx _

/-! ## 4. Stability criterion -/

/-- **Stability criterion**: for `Jx ≥ 0` and `s ≥ 0`, both eigenvalues `(J + Jx) s` and
`(J - Jx) s` are below 1 iff `(J + Jx) s < 1`. -/
theorem stable_iff (J Jx s : ℝ) (hJx : 0 ≤ Jx) (hs : 0 ≤ s) :
    ((J + Jx) * s < 1 ∧ (J - Jx) * s < 1) ↔ (J + Jx) * s < 1 := by
  constructor
  · exact fun h => h.1
  · intro h
    refine ⟨h, ?_⟩
    have : 0 ≤ Jx * s := mul_nonneg hJx hs
    nlinarith

/-- Stability of the scaled symmetric Jacobian: all eigenvalues below 1 iff `(J + Jx) s < 1`. -/
theorem scaled_stable_iff (J Jx s : ℝ) (hJx : 0 ≤ Jx) (hs : 0 ≤ s) :
    EigenBelowOne !![J * s, Jx * s; Jx * s, J * s] ↔ (J + Jx) * s < 1 := by
  rw [symm2_all_eigen_lt_one_iff,
    show J * s + Jx * s = (J + Jx) * s by ring, show J * s - Jx * s = (J - Jx) * s by ring]
  exact stable_iff J Jx s hJx hs

/-- The aligned state `(m, m)` (fields zero, `Jx ≥ 0`) is linearly stable iff
`(J + Jx) (1 - tanh ((J + Jx) m) ^ 2) < 1`. -/
theorem symmetric_state_stable_iff (J Jx m : ℝ) (hJx : 0 ≤ Jx) :
    EigenBelowOne (DF J Jx 0 0 (m, m)) ↔ (J + Jx) * dtanh ((J + Jx) * m) < 1 := by
  rw [DF_symmetric]
  exact scaled_stable_iff J Jx _ hJx (dtanh_pos _).le

/-- The polarized state `(m, -m)` (fields zero, `Jx ≥ 0`) is linearly stable iff
`(J + Jx) (1 - tanh ((J - Jx) m) ^ 2) < 1`. -/
theorem antisymmetric_state_stable_iff (J Jx m : ℝ) (hJx : 0 ≤ Jx) :
    EigenBelowOne (DF J Jx 0 0 (m, -m)) ↔ (J + Jx) * dtanh ((J - Jx) * m) < 1 := by
  rw [DF_antisymmetric]
  exact scaled_stable_iff J Jx _ hJx (dtanh_pos _).le

/-! ## 5. Existence of a positive root of `m = tanh (c m)` for `c > 1` -/

/-- Lower bound used for the intermediate value argument: `y / (2 y + 1) < tanh y` for `y > 0`
(from `exp (2 y) ≥ 1 + 2 y`). -/
theorem lower_bound_tanh (y : ℝ) (hy : 0 < y) : y / (2 * y + 1) < Real.tanh y := by
  obtain ⟨a, ha_def⟩ : ∃ a : ℝ, a = Real.exp y := ⟨_, rfl⟩
  obtain ⟨b, hb_def⟩ : ∃ b : ℝ, b = Real.exp (-y) := ⟨_, rfl⟩
  have ha : 0 < a := by rw [ha_def]; exact Real.exp_pos y
  have hb : 0 < b := by rw [hb_def]; exact Real.exp_pos (-y)
  have hab : a * b = 1 := by
    rw [ha_def, hb_def, ← Real.exp_add, add_neg_cancel, Real.exp_zero]
  have ha2 : y + y + 1 ≤ a * a := by
    rw [ha_def, ← Real.exp_add]
    exact Real.add_one_le_exp (y + y)
  have hkey : 2 * y * b ≤ a - b := by
    have e : a - b - 2 * y * b = b * (a * a - (y + y + 1)) := by
      linear_combination (-a) * hab
    have hnn : 0 ≤ b * (a * a - (y + y + 1)) := mul_nonneg hb.le (sub_nonneg.mpr ha2)
    linarith
  have hsub : 0 < a - b := by
    have : 0 < 2 * y * b := mul_pos (mul_pos two_pos hy) hb
    linarith
  have htanh : Real.tanh y = (a - b) / (a + b) := by
    rw [Real.tanh_eq, ha_def, hb_def]
  rw [htanh, div_lt_div_iff₀ (by linarith) (by linarith)]
  linarith [mul_pos hy hsub]

/-- **Existence of a nonzero symmetric solution**: if `c > 1`, some `m ∈ (0, 1)` satisfies
`m = tanh (c m)`. With `c = J + Jx` this gives an aligned fixed point `(m, m)`; with
`c = J - Jx` a polarized fixed point `(m, -m)` (see `fixed_point_symmetric`,
`fixed_point_antisymmetric`). -/
theorem exists_pos_fixed_point (c : ℝ) (hc : 1 < c) :
    ∃ m : ℝ, 0 < m ∧ m < 1 ∧ m = Real.tanh (c * m) := by
  obtain ⟨y, hy_def⟩ : ∃ y : ℝ, y = (c - 1) / 2 := ⟨_, rfl⟩
  have hy : 0 < y := by rw [hy_def]; linarith
  have hcy : c = 2 * y + 1 := by rw [hy_def]; ring
  have hcpos : 0 < c := by linarith
  obtain ⟨m0, hm0_def⟩ : ∃ m0 : ℝ, m0 = y / c := ⟨_, rfl⟩
  have hm0pos : 0 < m0 := by rw [hm0_def]; exact div_pos hy hcpos
  have hm0lt1 : m0 < 1 := by
    rw [hm0_def, div_lt_iff₀ hcpos]
    linarith
  have hcm0 : c * m0 = y := by
    rw [hm0_def]
    exact mul_div_cancel₀ y hcpos.ne'
  have hg0 : m0 < Real.tanh (c * m0) := by
    have h := lower_bound_tanh y hy
    rw [← hcy] at h
    rw [hcm0, hm0_def]
    exact h
  have hcont : Continuous (fun m : ℝ => Real.tanh (c * m) - m) :=
    (continuous_tanh.comp (continuous_const_mul c)).sub continuous_id
  have hmem : (0 : ℝ) ∈ Set.Ioo (Real.tanh (c * 1) - 1) (Real.tanh (c * m0) - m0) :=
    Set.mem_Ioo.mpr ⟨by linarith [Real.tanh_lt_one (c * 1)], by linarith⟩
  obtain ⟨m, hm, hgm⟩ := intermediate_value_Ioo' hm0lt1.le hcont.continuousOn hmem
  have hm' := Set.mem_Ioo.mp hm
  refine ⟨m, lt_trans hm0pos hm'.1, hm'.2, ?_⟩
  have hgm' : Real.tanh (c * m) - m = 0 := hgm
  linarith

/-! ## 6. Stability in terms of `m`, and the "only if" half of existence -/

/-- At a solution of `m = tanh (c m)`, the derivative factor is `1 - m ^ 2`. -/
theorem dtanh_at_root (c m : ℝ) (h : m = Real.tanh (c * m)) : dtanh (c * m) = 1 - m ^ 2 := by
  unfold dtanh
  rw [← h]

/-- The aligned equilibrium `(m, m)`, `m = tanh ((J + Jx) m)` (fields zero, `Jx ≥ 0`), is
linearly stable iff `(J + Jx) (1 - m ^ 2) < 1`. -/
theorem symmetric_state_stable_iff_root (J Jx m : ℝ) (hJx : 0 ≤ Jx)
    (h : m = Real.tanh ((J + Jx) * m)) :
    EigenBelowOne (DF J Jx 0 0 (m, m)) ↔ (J + Jx) * (1 - m ^ 2) < 1 := by
  rw [← dtanh_at_root _ _ h]
  exact symmetric_state_stable_iff J Jx m hJx

/-- The polarized equilibrium `(m, -m)`, `m = tanh ((J - Jx) m)` (fields zero, `Jx ≥ 0`), is
linearly stable iff `(J + Jx) (1 - m ^ 2) < 1`. -/
theorem antisymmetric_state_stable_iff_root (J Jx m : ℝ) (hJx : 0 ≤ Jx)
    (h : m = Real.tanh ((J - Jx) * m)) :
    EigenBelowOne (DF J Jx 0 0 (m, -m)) ↔ (J + Jx) * (1 - m ^ 2) < 1 := by
  rw [← dtanh_at_root _ _ h]
  exact antisymmetric_state_stable_iff J Jx m hJx

lemma tanh_pos_of_pos {x : ℝ} (hx : 0 < x) : 0 < Real.tanh x := by
  rw [Real.tanh_eq_sinh_div_cosh]
  exact div_pos (Real.sinh_pos_iff.mpr hx) (Real.cosh_pos x)

lemma tanh_nonpos_of_nonpos {x : ℝ} (hx : x ≤ 0) : Real.tanh x ≤ 0 := by
  have h : 0 ≤ Real.tanh (-x) := by
    rw [Real.tanh_eq_sinh_div_cosh]
    exact div_nonneg (Real.sinh_nonneg_iff.mpr (neg_nonneg.mpr hx)) (Real.cosh_pos _).le
  rw [Real.tanh_neg] at h
  linarith

/-- `tanh x < x` for `x > 0`: `x - tanh x` is strictly increasing on `[0, ∞)`, since its
derivative is `tanh x ^ 2 > 0` for `x > 0`. -/
theorem tanh_lt_self {x : ℝ} (hx : 0 < x) : Real.tanh x < x := by
  have hmono : StrictMonoOn (fun y : ℝ => y - Real.tanh y) (Set.Ici 0) := by
    refine strictMonoOn_of_deriv_pos (convex_Ici 0) ?_ ?_
    · exact (continuous_id.sub continuous_tanh).continuousOn
    · intro y hy
      have hy' : 0 < y := by
        rw [interior_Ici] at hy; exact hy
      have hd : HasDerivAt (fun y : ℝ => y - Real.tanh y) (1 - dtanh y) y := by
        exact (hasDerivAt_id' y).sub (hasDerivAt_tanh y)
      rw [hd.deriv]
      unfold dtanh
      have := pow_pos (tanh_pos_of_pos hy') 2
      linarith
  have h := hmono (Set.mem_Ici.mpr le_rfl) (Set.mem_Ici.mpr hx.le) hx
  have h' : (0 : ℝ) - Real.tanh 0 < x - Real.tanh x := h
  rw [Real.tanh_zero] at h'
  linarith

/-- `|tanh x| < |x|` for `x ≠ 0`. -/
theorem abs_tanh_lt_abs {x : ℝ} (hx : x ≠ 0) : |Real.tanh x| < |x| := by
  rcases lt_or_gt_of_ne hx with h | h
  · have h1 := tanh_lt_self (neg_pos.mpr h)
    rw [Real.tanh_neg] at h1
    rw [abs_of_nonpos (tanh_nonpos_of_nonpos h.le), abs_of_neg h]
    linarith
  · rw [abs_of_nonneg (tanh_pos_of_pos h).le, abs_of_pos h]
    exact tanh_lt_self h

lemma no_pos_root_of_le_one (c m : ℝ) (hc : c ≤ 1) (hm : 0 < m)
    (h : m = Real.tanh (c * m)) : False := by
  by_cases hcm : c * m ≤ 0
  · have := tanh_nonpos_of_nonpos hcm
    linarith
  · have h1 := tanh_lt_self (not_le.mp hcm)
    have h2 : c * m ≤ m := by nlinarith [mul_nonneg (sub_nonneg.mpr hc) hm.le]
    linarith

/-- **Only-if half**: for `c ≤ 1`, the only solution of `m = tanh (c m)` is `m = 0`. -/
theorem root_eq_zero_of_le_one (c m : ℝ) (hc : c ≤ 1) (h : m = Real.tanh (c * m)) :
    m = 0 := by
  rcases lt_trichotomy m 0 with hm | hm | hm
  · exfalso
    refine no_pos_root_of_le_one c (-m) hc (neg_pos.mpr hm) ?_
    rw [show c * -m = -(c * m) by ring, Real.tanh_neg, ← h]
  · exact hm
  · exact (no_pos_root_of_le_one c m hc hm h).elim

/-- **Existence iff**: `m = tanh (c m)` has a nonzero solution iff `c > 1`. -/
theorem exists_nonzero_root_iff (c : ℝ) :
    (∃ m : ℝ, m ≠ 0 ∧ m = Real.tanh (c * m)) ↔ 1 < c := by
  constructor
  · rintro ⟨m, hm0, hm⟩
    by_contra hc
    exact hm0 (root_eq_zero_of_le_one c m (not_lt.mp hc) hm)
  · intro hc
    obtain ⟨m, hm0, -, hm⟩ := exists_pos_fixed_point c hc
    exact ⟨m, hm0.ne', hm⟩

/-- An aligned equilibrium `(m, m)` with `m ≠ 0` exists (fields zero) iff `J + Jx > 1`. -/
theorem aligned_state_exists_iff (J Jx : ℝ) :
    (∃ m : ℝ, m ≠ 0 ∧ F J Jx 0 0 (m, m) = (m, m)) ↔ 1 < J + Jx := by
  refine Iff.trans ?_ (exists_nonzero_root_iff (J + Jx))
  constructor
  · rintro ⟨m, hm0, hF⟩
    have h1 : Real.tanh (J * m + Jx * m + 0) = m := congrArg Prod.fst hF
    rw [show J * m + Jx * m + 0 = (J + Jx) * m by ring] at h1
    exact ⟨m, hm0, h1.symm⟩
  · rintro ⟨m, hm0, hm⟩
    exact ⟨m, hm0, fixed_point_symmetric J Jx m hm⟩

/-- **Split existence**: a polarized equilibrium `(m, -m)` with `m ≠ 0` exists (fields zero)
iff `J - Jx > 1`. -/
theorem polarized_state_exists_iff (J Jx : ℝ) :
    (∃ m : ℝ, m ≠ 0 ∧ F J Jx 0 0 (m, -m) = (m, -m)) ↔ 1 < J - Jx := by
  refine Iff.trans ?_ (exists_nonzero_root_iff (J - Jx))
  constructor
  · rintro ⟨m, hm0, hF⟩
    have h1 : Real.tanh (J * m + Jx * -m + 0) = m := congrArg Prod.fst hF
    rw [show J * m + Jx * -m + 0 = (J - Jx) * m by ring] at h1
    exact ⟨m, hm0, h1.symm⟩
  · rintro ⟨m, hm0, hm⟩
    exact ⟨m, hm0, fixed_point_antisymmetric J Jx m hm⟩

end SaomNK.MeanField

end
