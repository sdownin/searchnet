import Mathlib.Analysis.SpecialFunctions.Pow.Real
import Mathlib.Tactic.FieldSimp
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity

/-!
# The Landau quartic

`V(x) = a x² + b x⁴` with `b > 0` is the normal form of a symmetric effective potential near a
pitchfork bifurcation (Landau theory). Its complete minimizer structure:

* `a ≥ 0`: `x = 0` is the unique global minimizer (`argmin_zero_of_nonneg`);
* `a < 0`: the minimum `−a²/(4b)` is attained exactly at `x² = −a/(2b)`, at `±√(−a/(2b))`, and
  the origin is strictly above it (`argmin_pm_of_neg`, `V_eq_min_iff`).

So a parameter path along which `a` crosses zero is a transition from one well to two.
-/

noncomputable section

namespace SaomNK.MeanField.Landau

/-- The Landau quartic `V(x) = a x² + b x⁴`. -/
def V (a b x : ℝ) : ℝ := a * x ^ 2 + b * x ^ 4

theorem V_eq_square_form {b : ℝ} (hb : 0 < b) (a x : ℝ) :
    V a b x = b * (x ^ 2 + a / (2 * b)) ^ 2 - a ^ 2 / (4 * b) := by
  unfold V
  field_simp
  ring

/-- The minimum value in the two-well phase, `−a²/(4b)`. -/
def Vmin (a b : ℝ) : ℝ := -(a ^ 2 / (4 * b))

theorem V_sub_Vmin {b : ℝ} (hb : 0 < b) (a x : ℝ) :
    V a b x - Vmin a b = b * (x ^ 2 + a / (2 * b)) ^ 2 := by
  rw [V_eq_square_form hb]
  unfold Vmin
  ring

/-- **One-well phase.** For `a ≥ 0`, `x = 0` is the unique global minimizer. -/
theorem argmin_zero_of_nonneg {a b : ℝ} (hb : 0 < b) (ha : 0 ≤ a) (x : ℝ) :
    V a b 0 ≤ V a b x ∧ (x ≠ 0 → V a b 0 < V a b x) := by
  unfold V
  constructor
  · nlinarith [sq_nonneg x, sq_nonneg (x ^ 2), mul_nonneg ha (sq_nonneg x),
      mul_nonneg hb.le (sq_nonneg (x ^ 2))]
  · intro hx
    have hx4 : 0 < x ^ 4 := by positivity
    nlinarith [mul_nonneg ha (sq_nonneg x), mul_pos hb hx4]

/-- The two-well minimizer `x* = √(−a/(2b))`. -/
def xstar (a b : ℝ) : ℝ := Real.sqrt (-a / (2 * b))

/-- **Two-well phase.** For `a < 0` the potential attains its minimum `−a²/(4b)` at `±x*`, and
the origin is strictly above it. -/
theorem argmin_pm_of_neg {a b : ℝ} (hb : 0 < b) (ha : a < 0) :
    (∀ x, Vmin a b ≤ V a b x) ∧
    V a b (xstar a b) = Vmin a b ∧ V a b (-xstar a b) = Vmin a b ∧
    Vmin a b < V a b 0 := by
  have hx2 : xstar a b ^ 2 = -a / (2 * b) := Real.sq_sqrt (by
    have : 0 < -a := by linarith
    positivity)
  refine ⟨fun x => ?_, ?_, ?_, ?_⟩
  · have h := mul_nonneg hb.le (sq_nonneg (x ^ 2 + a / (2 * b)))
    rw [← V_sub_Vmin hb] at h
    exact sub_nonneg.mp h
  · have h := V_sub_Vmin hb a (xstar a b)
    rw [hx2] at h
    have h0 : b * (-a / (2 * b) + a / (2 * b)) ^ 2 = 0 := by ring
    linarith
  · have h := V_sub_Vmin hb a (-xstar a b)
    rw [neg_sq, hx2] at h
    have h0 : b * (-a / (2 * b) + a / (2 * b)) ^ 2 = 0 := by ring
    linarith
  · unfold V Vmin
    have ha2 : 0 < a ^ 2 := by nlinarith
    have : 0 < a ^ 2 / (4 * b) := div_pos ha2 (by positivity)
    simp only [ne_eq, OfNat.ofNat_ne_zero, not_false_eq_true, zero_pow, mul_zero, add_zero]
    linarith

/-- In the two-well phase the minimum is attained exactly at `x² = −a/(2b)`. -/
theorem V_eq_min_iff {a b : ℝ} (hb : 0 < b) (x : ℝ) :
    V a b x = Vmin a b ↔ x ^ 2 = -a / (2 * b) := by
  have h := V_sub_Vmin hb a x
  constructor
  · intro hv
    have h' : b * (x ^ 2 + a / (2 * b)) ^ 2 = 0 := by rw [← h, hv]; ring
    have h'' : (x ^ 2 + a / (2 * b)) ^ 2 = 0 := by
      rcases mul_eq_zero.mp h' with h0 | h0
      · exact absurd h0 hb.ne'
      · exact h0
    have := pow_eq_zero_iff (n := 2) (by norm_num) |>.mp h''
    rw [neg_div]
    linarith
  · intro hx
    have h0 : b * (x ^ 2 + a / (2 * b)) ^ 2 = 0 := by rw [hx]; ring
    linarith

end SaomNK.MeanField.Landau

end
