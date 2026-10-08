import SaomNK.Choice.Logit
import SaomNK.Choice.Metropolis
import SaomNK.Core.Flip
import Mathlib.Order.Filter.AtTopBot.Field
import Mathlib.Topology.Algebra.Order.Field

/-!
# Zero-noise limits: SAOM search becomes NK adaptive walks

As the logit precision `β → ∞`:

* multinomial logit: if `j*` is the unique maximizer of the utility changes and `Δ_{j*} > 0`,
  the probability of flipping `j*` tends to 1 (`logitProb_tendsto_one`); if every change is
  negative, the no-change probability tends to 1 (`stayProb_tendsto_one`). This is steepest-ascent
  adaptive search on an NK landscape, stopping at local optima;
* binary acceptance: Metropolis acceptance tends to the keep-iff-not-worse rule of improvement
  search at every state (`metropolis_limit`), and Glauber acceptance does off ties
  (`glauber_limit`); at a tie Glauber accepts with probability 1/2 for every `β`.

`improvementStep_is_saom_limit` states the second fact for an arbitrary payoff profile on the
configuration space: one-flip improvement search (Levinthal 1997 for `M = 1`, and its many-firm
variants) is the zero-noise limit of single-flip SAOM search with the payoff as evaluation
function.
-/

open Finset Real Filter Topology

noncomputable section

namespace SaomNK

variable {M N : ℕ}

lemma tendsto_exp_mul_neg {c : ℝ} (hc : c < 0) :
    Tendsto (fun β : ℝ => exp (β * c)) atTop (𝓝 0) :=
  Real.tendsto_exp_atBot.comp (tendsto_id.atTop_mul_const_of_neg hc)

lemma logitProb_eq (Δ : Fin N → ℝ) (β : ℝ) (j : Fin N) :
    logitProb Δ β j = 1 / (exp (β * (0 - Δ j)) + ∑ k, exp (β * (Δ k - Δ j))) := by
  unfold logitProb
  have h1 : ∀ k, exp (β * (Δ k - Δ j)) = exp (β * Δ k) / exp (β * Δ j) := by
    intro k; rw [mul_sub, exp_sub]
  have h0 : exp (β * (0 - Δ j)) = 1 / exp (β * Δ j) := by
    rw [mul_sub, mul_zero, exp_sub, exp_zero]
  simp only [h1, h0]
  rw [← Finset.sum_div]
  have := exp_pos (β * Δ j)
  field_simp

/-- **Improving case.** If `j*` is the unique maximizer of the utility changes and
`Δ_{j*} > 0`, then `P(flip j*) → 1` as `β → ∞`. -/
theorem logitProb_tendsto_one (Δ : Fin N → ℝ) (j : Fin N) (hpos : 0 < Δ j)
    (hmax : ∀ k, k ≠ j → Δ k < Δ j) :
    Tendsto (fun β => logitProb Δ β j) atTop (𝓝 1) := by
  simp_rw [logitProb_eq]
  have hterm : ∀ k, Tendsto (fun β : ℝ => exp (β * (Δ k - Δ j))) atTop
      (𝓝 (if k = j then 1 else 0)) := by
    intro k
    by_cases hk : k = j
    · subst hk; simp
    · rw [ite_eq_right hk]
      exact tendsto_exp_mul_neg (by linarith [hmax k hk])
  have hsum : Tendsto (fun β : ℝ => ∑ k, exp (β * (Δ k - Δ j))) atTop (𝓝 1) := by
    have := tendsto_finsetSum univ (fun k _ => hterm k)
    simpa using this
  have h0 : Tendsto (fun β : ℝ => exp (β * (0 - Δ j))) atTop (𝓝 0) :=
    tendsto_exp_mul_neg (by linarith)
  have hden : Tendsto (fun β : ℝ => exp (β * (0 - Δ j)) + ∑ k, exp (β * (Δ k - Δ j)))
      atTop (𝓝 (0 + 1)) := h0.add hsum
  have := hden.inv₀ (by norm_num)
  simpa using this

/-- **Local-optimum case.** If every utility change is negative, the no-change probability
tends to 1 as `β → ∞`: the walk stops at a local optimum. -/
theorem stayProb_tendsto_one (Δ : Fin N → ℝ) (hneg : ∀ k, Δ k < 0) :
    Tendsto (fun β => stayProb Δ β) atTop (𝓝 1) := by
  unfold stayProb
  have hsum : Tendsto (fun β : ℝ => ∑ k, exp (β * Δ k)) atTop (𝓝 0) := by
    have := tendsto_finsetSum univ (fun k _ => tendsto_exp_mul_neg (hneg k))
    simpa using this
  have hden : Tendsto (fun β : ℝ => 1 + ∑ k, exp (β * Δ k)) atTop (𝓝 (1 + 0)) :=
    tendsto_const_nhds.add hsum
  have := hden.inv₀ (by norm_num)
  simpa using this

/-- The improvement-search acceptance rule: keep a proposed change iff the payoff does not fall. -/
def improveAccept (Δ : ℝ) : ℝ := if 0 ≤ Δ then 1 else 0

theorem glauber_tendsto_one {Δ : ℝ} (hΔ : 0 < Δ) :
    Tendsto (fun β => glauber β Δ) atTop (𝓝 1) := by
  have h0 : Tendsto (fun β : ℝ => exp (β * -Δ)) atTop (𝓝 0) := tendsto_exp_mul_neg (by linarith)
  have h1 : Tendsto (fun β : ℝ => 1 / (1 + exp (β * -Δ))) atTop (𝓝 (1 / (1 + 0))) :=
    tendsto_const_nhds.div (tendsto_const_nhds.add h0) (by norm_num)
  simp only [add_zero, div_one] at h1
  exact h1.congr fun β => (glauber_eq_inv β Δ).symm

theorem glauber_tendsto_zero {Δ : ℝ} (hΔ : Δ < 0) :
    Tendsto (fun β => glauber β Δ) atTop (𝓝 0) := by
  have h0 : Tendsto (fun β : ℝ => exp (β * Δ)) atTop (𝓝 0) := tendsto_exp_mul_neg hΔ
  have h1 : Tendsto (fun β : ℝ => exp (β * Δ) / (1 + exp (β * Δ))) atTop (𝓝 (0 / (1 + 0))) :=
    h0.div (tendsto_const_nhds.add h0) (by norm_num)
  simpa [glauber] using h1

/-- Off ties, Glauber acceptance tends to the improvement rule. -/
theorem glauber_limit {Δ : ℝ} (hΔ : Δ ≠ 0) :
    Tendsto (fun β => glauber β Δ) atTop (𝓝 (improveAccept Δ)) := by
  rcases lt_or_gt_of_ne hΔ with h | h
  · have : improveAccept Δ = 0 := by simp [improveAccept, not_le.mpr h]
    rw [this]; exact glauber_tendsto_zero h
  · have : improveAccept Δ = 1 := by simp [improveAccept, h.le]
    rw [this]; exact glauber_tendsto_one h

/-- At every state, ties included, Metropolis acceptance tends to the improvement rule. -/
theorem metropolis_limit (Δ : ℝ) :
    Tendsto (fun β => metropolis β Δ) atTop (𝓝 (improveAccept Δ)) := by
  by_cases h : 0 ≤ Δ
  · have : improveAccept Δ = 1 := by simp [improveAccept, h]
    rw [this]
    refine tendsto_const_nhds.congr' ?_
    filter_upwards [eventually_ge_atTop (0 : ℝ)] with β hβ
    exact (metropolis_eq_one hβ h).symm
  · have hn : Δ < 0 := lt_of_not_ge h
    have : improveAccept Δ = 0 := by simp [improveAccept, h]
    rw [this]
    have h0 := (tendsto_const_nhds (x := (1 : ℝ))).min (tendsto_exp_mul_neg hn)
    simpa [metropolis] using h0

/-- Payoff gain of actor `i` from flipping entry `j`, rivals fixed. -/
def payoffGain (pay : Fin M → Config M N → ℝ) (B : Config M N) (i : Fin M) (j : Fin N) : ℝ :=
  pay i (flip B i j) - pay i B

/-- One step of improvement search on an arbitrary payoff profile. -/
def improvementStep (pay : Fin M → Config M N → ℝ) (B : Config M N) (i : Fin M) (j : Fin N) :
    ℝ :=
  improveAccept (payoffGain pay B i j)

/-- **Improvement search is the zero-noise limit of single-flip SAOM search** with the payoff as
the evaluation function and Metropolis acceptance, at every state. -/
theorem improvementStep_is_saom_limit (pay : Fin M → Config M N → ℝ) (B : Config M N)
    (i : Fin M) (j : Fin N) :
    Tendsto (fun β => metropolis β (payoffGain pay B i j)) atTop
      (𝓝 (improvementStep pay B i j)) :=
  metropolis_limit _

/-- The same with binary-logit (Glauber) acceptance, the SAOM default, off ties. -/
theorem improvementStep_is_saom_limit_glauber (pay : Fin M → Config M N → ℝ) (B : Config M N)
    (i : Fin M) (j : Fin N) (h : payoffGain pay B i j ≠ 0) :
    Tendsto (fun β => glauber β (payoffGain pay B i j)) atTop
      (𝓝 (improvementStep pay B i j)) :=
  glauber_limit h

end SaomNK

end
