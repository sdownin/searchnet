import Mathlib.Data.Real.Basic
import Mathlib.Tactic.Ring
import Mathlib.Tactic.NormNum

/-!
# Indicators

`ind b` is the real-valued indicator of a Boolean. Every statistic in the library is built from
it, so the facts here are the arithmetic that the potential computations reduce to.
-/

namespace SaomNK

/-- Real-valued indicator of a Boolean. -/
def ind (b : Bool) : ℝ := if b then 1 else 0

@[simp] lemma ind_true : ind true = 1 := rfl
@[simp] lemma ind_false : ind false = 0 := rfl

lemma ind_mul_self (b : Bool) : ind b * ind b = ind b := by cases b <;> simp [ind]
lemma ind_sq (b : Bool) : ind b ^ 2 = ind b := by cases b <;> simp [ind]
lemma ind_nonneg (b : Bool) : 0 ≤ ind b := by cases b <;> simp [ind]
lemma ind_le_one (b : Bool) : ind b ≤ 1 := by cases b <;> simp [ind]
lemma ind_not (b : Bool) : ind (!b) = 1 - ind b := by cases b <;> simp [ind]
lemma ind_and (a b : Bool) : ind (a && b) = ind a * ind b := by
  cases a <;> cases b <;> simp [ind]
lemma ind_eq_toNat (b : Bool) : ind b = (b.toNat : ℝ) := by cases b <;> simp [ind]

end SaomNK
