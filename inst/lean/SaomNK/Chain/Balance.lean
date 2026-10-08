import SaomNK.Potential.Exact
import SaomNK.Choice.Metropolis

/-!
# Detailed and global balance of the single-flip chain

Protocol: the pair `(i, j)` is proposed at a positive constant rate `c i j` (for RSiena's default,
`c i j = λ_i / N`: actor `i` at rate `λ_i`, one tie drawn uniformly), and the flip is accepted by
a rule `acc β Δ` applied to actor `i`'s utility change `Δ`.

**Theorem (Gibbs stationarity).** If the utility profile has a flip potential `Φ` and the
acceptance rule is reversible for `exp(β ·)` (binary logit or Metropolis), then the Gibbs weight
`exp(β Φ)` satisfies detailed balance on every flip (`detailed_balance_of_potential`) and is
stationary for the generator (`global_balance_of_potential`, `π Q = 0`). Actor-specific (but
state-independent) rates do not disturb this. This is Blume's (1993) logit-potential result for
the SAOM ministep.

Not formalized: that a finite irreducible continuous-time chain has a unique stationary law to
which it converges. With `SaomNK.Chain.Reachable` (irreducibility) and `flipRate_pos`, uniqueness
follows from that textbook fact.
-/

open Finset Real

noncomputable section

namespace SaomNK

variable {M N : ℕ}

/-- Rate of the flip `(i, j)` at `B`: proposal rate times acceptance of `i`'s utility change. -/
def flipRate (acc : ℝ → ℝ → ℝ) (c : Fin M → Fin N → ℝ) (β : ℝ) (U : Fin M → Config M N → ℝ)
    (B : Config M N) (i : Fin M) (j : Fin N) : ℝ :=
  c i j * acc β (U i (flip B i j) - U i B)

/-- Unnormalized Gibbs weight `exp(β Φ(B))`. -/
def gibbsW (β : ℝ) (Φ : Config M N → ℝ) (B : Config M N) : ℝ := exp (β * Φ B)

lemma gibbsW_pos (β : ℝ) (Φ : Config M N → ℝ) (B : Config M N) : 0 < gibbsW β Φ B := exp_pos _

/-- **Detailed balance** of the Gibbs weight on every single-flip pair. -/
theorem detailed_balance_of_potential {acc : ℝ → ℝ → ℝ} (hacc : IsReversibleAcceptance acc)
    {U : Fin M → Config M N → ℝ} {Φ : Config M N → ℝ} (hΦ : IsFlipPotential U Φ)
    (c : Fin M → Fin N → ℝ) (β : ℝ) (B : Config M N) (i : Fin M) (j : Fin N) :
    gibbsW β Φ B * flipRate acc c β U B i j
      = gibbsW β Φ (flip B i j) * flipRate acc c β U (flip B i j) i j := by
  unfold gibbsW flipRate
  rw [flip_flip, hΦ B i j]
  have h2 : U i B - U i (flip B i j) = Φ B - Φ (flip B i j) := by linarith [hΦ B i j]
  rw [h2]
  have := hacc β (Φ B) (Φ (flip B i j))
  linear_combination c i j * this

/-- **Global balance** (`π Q = 0`): inflow into `B'` equals outflow from `B'`. -/
theorem global_balance_of_potential {acc : ℝ → ℝ → ℝ} (hacc : IsReversibleAcceptance acc)
    {U : Fin M → Config M N → ℝ} {Φ : Config M N → ℝ} (hΦ : IsFlipPotential U Φ)
    (c : Fin M → Fin N → ℝ) (β : ℝ) (B' : Config M N) :
    (∑ i, ∑ j, gibbsW β Φ (flip B' i j) * flipRate acc c β U (flip B' i j) i j)
      = gibbsW β Φ B' * ∑ i, ∑ j, flipRate acc c β U B' i j := by
  rw [Finset.mul_sum]
  refine Finset.sum_congr rfl fun i _ => ?_
  rw [Finset.mul_sum]
  refine Finset.sum_congr rfl fun j _ => ?_
  have h := detailed_balance_of_potential hacc hΦ c β (flip B' i j) i j
  rw [flip_flip] at h
  exact h

/-- Under binary-logit acceptance every flip with a positive proposal rate has positive rate. -/
lemma flipRate_glauber_pos {c : Fin M → Fin N → ℝ} (hc : ∀ i j, 0 < c i j) (β : ℝ)
    (U : Fin M → Config M N → ℝ) (B : Config M N) (i : Fin M) (j : Fin N) :
    0 < flipRate glauber c β U B i j :=
  mul_pos (hc i j) (glauber_pos _ _)

lemma flipRate_metropolis_pos {c : Fin M → Fin N → ℝ} (hc : ∀ i j, 0 < c i j) (β : ℝ)
    (U : Fin M → Config M N → ℝ) (B : Config M N) (i : Fin M) (j : Fin N) :
    0 < flipRate metropolis c β U B i j :=
  mul_pos (hc i j) (metropolis_pos _ _)

/-! ## The regular and core specifications -/

/-- Gibbs stationarity for the regular SAOM-NK evaluation function with binary-logit
ministeps and any state-independent proposal rates. -/
theorem Spec.global_balance (S : Spec M N) (hS : S.PairSymmetric) (c : Fin M → Fin N → ℝ)
    (β : ℝ) (B' : Config M N) :
    (∑ i, ∑ j, gibbsW β S.potential (flip B' i j)
        * flipRate glauber c β S.utility (flip B' i j) i j)
      = gibbsW β S.potential B' * ∑ i, ∑ j, flipRate glauber c β S.utility B' i j :=
  global_balance_of_potential glauber_reversible (S.exact_potential hS).toFlip c β B'

theorem Spec.detailed_balance (S : Spec M N) (hS : S.PairSymmetric) (c : Fin M → Fin N → ℝ)
    (β : ℝ) (B : Config M N) (i : Fin M) (j : Fin N) :
    gibbsW β S.potential B * flipRate glauber c β S.utility B i j
      = gibbsW β S.potential (flip B i j) * flipRate glauber c β S.utility (flip B i j) i j :=
  detailed_balance_of_potential glauber_reversible (S.exact_potential hS).toFlip c β B i j

/-- Transition rate of the flip `(i, j)` under the uniform single-flip protocol: actor rate
`λ/M`, tie draw `1/N`, binary logit on the utility change. -/
def CoreSpec.rate (S : CoreSpec M N) (β lam : ℝ) (B : Config M N) (i : Fin M) (j : Fin N) : ℝ :=
  lam / ((M : ℝ) * N) * glauber β (S.utility i (flip B i j) - S.utility i B)

/-- Unnormalized Gibbs weight `exp(β Φ)` of the core specification. -/
def CoreSpec.gibbs (S : CoreSpec M N) (β : ℝ) (B : Config M N) : ℝ := exp (β * S.potential B)

lemma CoreSpec.rate_pos (S : CoreSpec M N) {β lam : ℝ} (hlam : 0 < lam) (hM : 0 < M)
    (hN : 0 < N) (B : Config M N) (i : Fin M) (j : Fin N) : 0 < S.rate β lam B i j := by
  unfold CoreSpec.rate
  have hM' : (0 : ℝ) < M := by exact_mod_cast hM
  have hN' : (0 : ℝ) < N := by exact_mod_cast hN
  exact mul_pos (div_pos hlam (mul_pos hM' hN')) (glauber_pos _ _)

lemma CoreSpec.isFlipPotential (S : CoreSpec M N) : IsFlipPotential S.utility S.potential :=
  fun B i j => S.exact_potential (flip_unilateral B i j)

/-- **Detailed balance** of the core specification's Gibbs weight. -/
theorem CoreSpec.detailed_balance (S : CoreSpec M N) (β lam : ℝ) (B : Config M N) (i : Fin M)
    (j : Fin N) :
    S.gibbs β B * S.rate β lam B i j = S.gibbs β (flip B i j) * S.rate β lam (flip B i j) i j :=
  detailed_balance_of_potential glauber_reversible S.isFlipPotential
    (fun _ _ => lam / ((M : ℝ) * N)) β B i j

/-- **Global balance** of the core specification's Gibbs weight. -/
theorem CoreSpec.global_balance (S : CoreSpec M N) (β lam : ℝ) (B' : Config M N) :
    (∑ i, ∑ j, S.gibbs β (flip B' i j) * S.rate β lam (flip B' i j) i j)
      = S.gibbs β B' * ∑ i, ∑ j, S.rate β lam B' i j :=
  global_balance_of_potential glauber_reversible S.isFlipPotential
    (fun _ _ => lam / ((M : ℝ) * N)) β B'

end SaomNK

end
