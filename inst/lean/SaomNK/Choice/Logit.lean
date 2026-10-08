import SaomNK.Choice.Glauber
import Mathlib.Algebra.BigOperators.Fin
import Mathlib.Algebra.BigOperators.Field

/-!
# The multinomial logit with a no-change alternative

RSiena's default ministep: actor `i` chooses among flipping each of the `N` entries and changing
nothing, with probabilities proportional to `e^{βΔ_j}` and `1` (McFadden 1974; Snijders 2001).

* `logitProb_pos`: at any finite `β` every flip, including every utility-decreasing one, has
  strictly positive probability;
* `logit_sum`: the `N + 1` probabilities sum to one.
-/

open Finset Real

noncomputable section

namespace SaomNK

variable {N : ℕ}

/-- Multinomial-logit probability of flip `j`, with the no-change alternative. -/
def logitProb (Δ : Fin N → ℝ) (β : ℝ) (j : Fin N) : ℝ :=
  exp (β * Δ j) / (1 + ∑ k, exp (β * Δ k))

/-- Probability of the no-change alternative. -/
def stayProb (Δ : Fin N → ℝ) (β : ℝ) : ℝ := 1 / (1 + ∑ k, exp (β * Δ k))

lemma logit_denom_pos (Δ : Fin N → ℝ) (β : ℝ) : 0 < 1 + ∑ k, exp (β * Δ k) := by
  have := Finset.sum_nonneg (s := univ) fun k (_ : k ∈ univ) => (exp_pos (β * Δ k)).le
  linarith

/-- At any finite `β`, every flip has strictly positive probability. -/
theorem logitProb_pos (Δ : Fin N → ℝ) (β : ℝ) (j : Fin N) : 0 < logitProb Δ β j := by
  unfold logitProb; exact div_pos (exp_pos _) (logit_denom_pos Δ β)

theorem stayProb_pos (Δ : Fin N → ℝ) (β : ℝ) : 0 < stayProb Δ β := by
  unfold stayProb; exact div_pos one_pos (logit_denom_pos Δ β)

/-- The choice probabilities sum to one. -/
theorem logit_sum (Δ : Fin N → ℝ) (β : ℝ) :
    stayProb Δ β + ∑ j, logitProb Δ β j = 1 := by
  unfold stayProb logitProb
  rw [← Finset.sum_div, ← add_div]
  exact div_self (logit_denom_pos Δ β).ne'

/-- The odds of flip `j` against no change are `e^{βΔ_j}`: the binary comparison inside the
multinomial logit is the Glauber odds. -/
theorem logitProb_div_stayProb (Δ : Fin N → ℝ) (β : ℝ) (j : Fin N) :
    logitProb Δ β j / stayProb Δ β = exp (β * Δ j) := by
  unfold logitProb stayProb
  have := (logit_denom_pos Δ β).ne'
  field_simp

end SaomNK

end
