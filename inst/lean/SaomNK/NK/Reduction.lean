import SaomNK.Potential.Exact
import SaomNK.NK.Table

/-!
# Reduction: the NK model as a corner of SAOM-NK

Three restrictions turn the SAOM-NK evaluation function into NK fitness:

* R1, a single actor (`M = 1`);
* R2, the evaluation function's influence pattern equals the landscape's (`E = G`);
* R3, no network effects (all coefficients other than the NK term zero).

Under R1 to R3 the utility equals `N · W(x)` for every configuration (`utility_eq_nkFitness`).
Together with the zero-noise limits of `SaomNK.Choice.Limits` (the dynamic half), this is the
reduction of single-flip SAOM search to adaptive walks on an NK landscape (Kauffman 1993;
Levinthal 1997). For a landscape stored as an enumerated table the same identity holds with the
table's row sum (`utility_eq_tableRowSum`).
-/

open Finset

noncomputable section

namespace SaomNK

variable {N : ℕ}

/-- Under R1 (`M = 1`) and R3 (`θ = 0`), the core utility is the NK term of the single row. -/
lemma utility_single_actor (E : Influence N) (f : Fin N → (Fin N → Bool) → ℝ)
    (x : Fin N → Bool) :
    (CoreSpec.mk (M := 1) E f 0 0 0).utility 0 (fun _ => x) = nkTermFull E f x := by
  simp [CoreSpec.utility, CoreSpec.own]

/-- **Payoff equivalence.** Under R1, R2, R3 the SAOM-NK utility equals `N · W(x)` for every
configuration `x`. -/
theorem utility_eq_nkFitness (hN : 0 < N) (G : Influence N) (f : Fin N → (Fin N → Bool) → ℝ)
    (x : Fin N → Bool) :
    (CoreSpec.mk (M := 1) G f 0 0 0).utility 0 (fun _ => x) = (N : ℝ) * nkFitness G f x := by
  rw [utility_single_actor, nkTermFull_eq_mul_nkFitness hN]

/-- Payoff equivalence for a landscape stored as a consistent enumerated table. -/
theorem utility_eq_tableRowSum {G : Influence N} {C : ℕ → Fin N → ℝ} (hC : TableConsistent G C)
    (x : Fin N → Bool) :
    (CoreSpec.mk (M := 1) G (tablePayoff C) 0 0 0).utility 0 (fun _ => x) = tableRowSum C x := by
  rw [utility_single_actor, nkTermFull_tableLookup hC]

end SaomNK

end
