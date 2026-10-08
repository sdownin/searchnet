import SaomNK.Chain.Balance
import SaomNK.Potential.Nash

/-!
# Worked example: adding a statistic

The pattern for extending the library, on a statistic it does not have: activity-weighted
overlap, `Σ_j v_j b_ij b_kj`, summed over rivals. Shared holdings of valuable activities count
more. (The vignette `saomnk-lean` walks through this file.)

1. Define the kernel on two rows.
2. Prove it symmetric.
3. Get its exact potential from the row-pair building block.
4. Add it to an existing specification; Nash existence and Gibbs stationarity follow with no
   new proof.
-/

open Finset

noncomputable section

namespace SaomNK.Examples

variable {M N : ℕ}

/-- Step 1: activity-weighted overlap of two portfolios. -/
def wOverlap (v : Fin N → ℝ) (b b' : Fin N → Bool) : ℝ := ∑ j, v j * (ind (b j) * ind (b' j))

/-- Step 2: it is symmetric. -/
lemma wOverlap_symm (v : Fin N → ℝ) (b b' : Fin N → Bool) : wOverlap v b b' = wOverlap v b' b := by
  unfold wOverlap
  exact Finset.sum_congr rfl fun j _ => by ring

/-- Step 3: the summed statistic has an exact potential. -/
theorem isExactPotential_wOverlap (v : Fin N → ℝ) :
    IsExactPotential (fun i (B : Config M N) => rowPairStat (wOverlap v) B i)
      (rowPairPotential (wOverlap v)) :=
  isExactPotential_rowPair _ (wOverlap_symm v)

/-- Step 4: add it, with coefficient `θ`, to the core specification. -/
theorem core_plus_wOverlap (S : CoreSpec M N) (θ : ℝ) (v : Fin N → ℝ) :
    IsExactPotential (fun i B => S.utility i B + θ * rowPairStat (wOverlap v) B i)
      (fun B => S.potential B + θ * rowPairPotential (wOverlap v) B) :=
  (show IsExactPotential S.utility S.potential from fun _ _ _ h => S.exact_potential h).add
    ((isExactPotential_wOverlap v).smul θ)

/-- ... so the extended model has a pure Nash equilibrium ... -/
theorem core_plus_wOverlap_nash (S : CoreSpec M N) (θ : ℝ) (v : Fin N → ℝ) :
    ∃ B, IsNash (fun i B => S.utility i B + θ * rowPairStat (wOverlap v) B i) B :=
  exists_nash_of_exactPotential (core_plus_wOverlap S θ v)

/-- ... and its binary-logit single-flip chain has the Gibbs stationary law. -/
theorem core_plus_wOverlap_gibbs (S : CoreSpec M N) (θ : ℝ) (v : Fin N → ℝ)
    (c : Fin M → Fin N → ℝ) (β : ℝ) (B' : Config M N) :
    (∑ i, ∑ j, gibbsW β (fun B => S.potential B + θ * rowPairPotential (wOverlap v) B) (flip B' i j)
        * flipRate glauber c β (fun i B => S.utility i B + θ * rowPairStat (wOverlap v) B i)
            (flip B' i j) i j)
      = gibbsW β (fun B => S.potential B + θ * rowPairPotential (wOverlap v) B) B'
        * ∑ i, ∑ j, flipRate glauber c β
            (fun i B => S.utility i B + θ * rowPairStat (wOverlap v) B i) B' i j :=
  global_balance_of_potential glauber_reversible (core_plus_wOverlap S θ v).toFlip c β B'

end SaomNK.Examples

end
