import SaomNK.Chain.Restricted

/-!
# Irreducibility of the unrestricted chain, and uniqueness of potentials

The unrestricted chain is the restricted chain with every entry free, so every configuration is
reachable from every other by single flips (`reachable`). Two consequences:

* with `flipRate_glauber_pos`, the single-flip chain is irreducible;
* an exact potential is unique up to an additive constant (`flipPotential_unique`), as in
  Monderer and Shapley (1996): the potential of a SAOM-NK specification is a property of the
  model, not of how it was written down.
-/

open Finset

noncomputable section

namespace SaomNK

variable {M N : ℕ}

/-- One-step neighbor relation of the single-flip chain. -/
def Step (B B' : Config M N) : Prop := ∃ i j, B' = flip B i j

/-- **Reachability**: any configuration is reachable from any other by single flips (at most
`M·N` of them). -/
theorem reachable (B B' : Config M N) : Relation.ReflTransGen Step B B' := by
  have h := reachable_restricted (fun _ _ => true) B (feasible_refl _ B)
    (B' := B') (fun _ _ hf => by simp at hf)
  induction h with
  | refl => exact Relation.ReflTransGen.refl
  | tail _ hstep ih =>
    obtain ⟨i, j, -, hY⟩ := hstep
    exact Relation.ReflTransGen.tail ih ⟨i, j, hY⟩

/-- **Uniqueness of the potential.** Two flip potentials of the same utility profile differ by
a constant. -/
theorem flipPotential_unique {U : Fin M → Config M N → ℝ} {Φ Ψ : Config M N → ℝ}
    (hΦ : IsFlipPotential U Φ) (hΨ : IsFlipPotential U Ψ) (B0 B : Config M N) :
    Φ B - Ψ B = Φ B0 - Ψ B0 := by
  induction reachable B0 B with
  | refl => rfl
  | tail _ hstep ih =>
    obtain ⟨i, j, rfl⟩ := hstep
    rename_i X _
    have h1 := hΦ X i j
    have h2 := hΨ X i j
    linarith

end SaomNK

end
