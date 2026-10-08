import Mathlib.Analysis.SpecialFunctions.Log.Basic
import Mathlib.Analysis.SpecialFunctions.Exp
import Mathlib.Tactic.FieldSimp
import Mathlib.Tactic.LinearCombination
import Mathlib.Tactic.Positivity

/-!
# Binary-logit (Glauber, heat-bath) acceptance

A SAOM ministep with a single proposed flip, accepted against the no-change alternative by the
binary logit, accepts with probability `σ(βΔ) = e^{βΔ} / (1 + e^{βΔ})`, where `Δ` is the
utility change and `β` the logit precision (Glauber dynamics; Blume 1993).

The single identity that drives every detailed-balance result is `exp_mul_glauber_comm`:
`e^{βa} σ(β(b-a)) = e^{βb} σ(β(a-b))`. `IsReversibleAcceptance` names that property so that any
other acceptance rule with it (Metropolis, `SaomNK.Choice.Metropolis`) plugs into the chain
results unchanged.
-/

open Real

noncomputable section

namespace SaomNK

/-- An acceptance rule `acc β Δ` is reversible for the Gibbs weight `exp(β ·)`. -/
def IsReversibleAcceptance (acc : ℝ → ℝ → ℝ) : Prop :=
  ∀ β a b : ℝ, exp (β * a) * acc β (b - a) = exp (β * b) * acc β (a - b)

/-- Binary-logit (Glauber, heat-bath) acceptance of a proposed change against no change. -/
def glauber (β Δ : ℝ) : ℝ := exp (β * Δ) / (1 + exp (β * Δ))

lemma glauber_pos (β Δ : ℝ) : 0 < glauber β Δ := by
  unfold glauber; positivity

lemma glauber_lt_one (β Δ : ℝ) : glauber β Δ < 1 := by
  unfold glauber
  rw [div_lt_one (by positivity)]
  linarith [exp_pos (β * Δ)]

/-- The algebraic heart of Gibbs detailed balance: `e^{βa} σ(β(b-a)) = e^{βb} σ(β(a-b))`. -/
lemma exp_mul_glauber_comm (β a b : ℝ) :
    exp (β * a) * glauber β (b - a) = exp (β * b) * glauber β (a - b) := by
  unfold glauber
  have ha := exp_pos (β * a)
  have hb := exp_pos (β * b)
  rw [mul_sub, mul_sub, exp_sub, exp_sub]
  field_simp
  ring

theorem glauber_reversible : IsReversibleAcceptance glauber := exp_mul_glauber_comm

/-- Kolmogorov edge weight: the ratio of forward to reverse acceptance is `e^{βΔ}`. -/
lemma glauber_ratio (β Δ : ℝ) : glauber β Δ / glauber β (-Δ) = exp (β * Δ) := by
  unfold glauber
  rw [mul_neg, exp_neg]
  have := exp_pos (β * Δ)
  field_simp
  ring

lemma log_glauber_ratio (β Δ : ℝ) :
    Real.log (glauber β Δ / glauber β (-Δ)) = β * Δ := by
  rw [glauber_ratio, log_exp]

/-- At a tie the binary logit accepts with probability one half, for every `β`. -/
lemma glauber_zero (β : ℝ) : glauber β 0 = 1 / 2 := by
  unfold glauber; rw [mul_zero, exp_zero]; norm_num

lemma glauber_eq_inv (β Δ : ℝ) : glauber β Δ = 1 / (1 + exp (β * -Δ)) := by
  unfold glauber
  have h := exp_pos (β * Δ)
  rw [mul_neg, exp_neg]
  field_simp
  ring

/-- Forward and reverse acceptances sum to one. -/
lemma glauber_add_neg (β Δ : ℝ) : glauber β Δ + glauber β (-Δ) = 1 := by
  unfold glauber
  rw [mul_neg, exp_neg]
  have h := exp_pos (β * Δ)
  field_simp
  ring

end SaomNK

end
