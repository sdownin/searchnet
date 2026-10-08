import SaomNK.Choice.Glauber

/-!
# Metropolis acceptance

`metropolis β Δ = min 1 (e^{βΔ})`: accept every improvement, accept a worsening with probability
`e^{βΔ}`. It is reversible for the same Gibbs weight as the binary logit
(`metropolis_reversible`), so every chain-level result stated for a reversible acceptance rule
holds for it too.
-/

open Real

noncomputable section

namespace SaomNK

/-- Metropolis acceptance at inverse temperature `β`. -/
def metropolis (β Δ : ℝ) : ℝ := min 1 (exp (β * Δ))

lemma metropolis_pos (β Δ : ℝ) : 0 < metropolis β Δ :=
  lt_min one_pos (exp_pos _)

lemma metropolis_le_one (β Δ : ℝ) : metropolis β Δ ≤ 1 := min_le_left _ _

lemma metropolis_eq_one {β Δ : ℝ} (hβ : 0 ≤ β) (hΔ : 0 ≤ Δ) : metropolis β Δ = 1 :=
  min_eq_left (one_le_exp (mul_nonneg hβ hΔ))

/-- `e^{βa} · min(1, e^{β(b-a)}) = min(e^{βa}, e^{βb})`. -/
lemma exp_mul_metropolis (β a b : ℝ) :
    exp (β * a) * metropolis β (b - a) = min (exp (β * a)) (exp (β * b)) := by
  unfold metropolis
  rw [mul_min_of_nonneg _ _ (exp_pos _).le, mul_one, ← exp_add]
  congr 2
  ring

/-- **Metropolis acceptance is reversible for the Gibbs weight.** -/
theorem metropolis_reversible : IsReversibleAcceptance metropolis := by
  intro β a b
  rw [exp_mul_metropolis, exp_mul_metropolis, min_comm]

end SaomNK

end
