import SaomNK.NK.PowerKey
import Mathlib.Algebra.Order.Field.Rat
import Mathlib.Data.Rat.Cast.Order
import Mathlib.Data.Fin.VecNotation

/-!
# Computable rational specifications

`RatSpec M N` is a concrete SAOM-NK model with exact rational parameters: what `lean_export_model`
in the searchnet R package writes. Everything here is computable over `ℚ`, so facts about a
specific model (a utility value, a count of local optima, regularity of its influence matrix)
can be proved by kernel evaluation (`decide +kernel`), with no `native_decide` and no axioms
beyond the standard three.

`SaomNK.Compute.Cast` proves that every computable quantity here is the cast of the
corresponding real-valued quantity of the general theory (`RatSpec.cast_utility`), so a fact
proved by evaluation is a fact about the general model, and the general theorems
(exact potential, Gibbs stationarity, Nash existence) apply to the concrete model.

Conventions:

* `C` is the enumerated contribution table: row `c` holds the `N` component contributions at the
  configuration whose LSB code is `c` (R row `c + 1`). NK fitness is the row mean.
* `W`, `ego`, `alt` are stored already multiplied by their coefficients
  (`θ_XWX · W`, `θ_egoX · z_i`, `θ_altX · v_j`).
-/

open Finset

namespace SaomNK

/-- A concrete SAOM-NK model with exact rational parameters. -/
structure RatSpec (M N : ℕ) where
  /-- influence pattern of the NK landscape -/
  E : Influence N
  /-- enumerated contribution table, LSB-first rows -/
  C : Fin (2 ^ N) → Fin N → ℚ
  /-- weight on NK fitness (the row mean of `C`) -/
  θnk : ℚ
  θdensity : ℚ
  θoutAct : ℚ
  θinPop : ℚ
  θcycle4 : ℚ
  /-- `θ_XWX · W`, a component-by-component covariate -/
  W : Fin N → Fin N → ℚ
  /-- `θ_egoX · z_i`, an actor covariate on the outdegree -/
  ego : Fin M → ℚ
  /-- `θ_altX · v_j`, an activity covariate -/
  alt : Fin N → ℚ

variable {M N : ℕ}

/-- Rational indicator. -/
def indQ (b : Bool) : ℚ := if b then 1 else 0

def densityQ (b : Fin N → Bool) : ℚ := ∑ j, indQ (b j)

def colSumQ (B : Fin M → Fin N → Bool) (d : Fin N) : ℚ := ∑ m, indQ (B m d)

def inPopQ (B : Fin M → Fin N → Bool) (i : Fin M) : ℚ := ∑ j, indQ (B i j) * colSumQ B j

def overlapQ (b b' : Fin N → Bool) : ℚ := ∑ j, indQ (b j) * indQ (b' j)

def cycle4Q (B : Fin M → Fin N → Bool) (i : Fin M) : ℚ :=
  ∑ k ∈ univ.erase i, overlapQ (B i) (B k) * (overlapQ (B i) (B k) - 1) / 2

def xwxQ (W : Fin N → Fin N → ℚ) (b : Fin N → Bool) : ℚ :=
  ∑ j, ∑ h, indQ (b j) * W j h * indQ (b h)

def altQ (v : Fin N → ℚ) (b : Fin N → Bool) : ℚ := ∑ j, v j * indQ (b j)

/-- NK fitness read from the enumerated table: the row mean at the row's LSB code. -/
def nkQ (C : Fin (2 ^ N) → Fin N → ℚ) (b : Fin N → Bool) : ℚ :=
  (∑ d, C ⟨codeLSB b, codeLSB_lt b⟩ d) / N

/-- Own-row part of the utility. -/
def RatSpec.ownQ (S : RatSpec M N) (i : Fin M) (b : Fin N → Bool) : ℚ :=
  S.θnk * nkQ S.C b + S.θdensity * densityQ b + S.θoutAct * densityQ b ^ 2 + xwxQ S.W b
    + S.ego i * densityQ b + altQ S.alt b

/-- Actor `i`'s utility. -/
def RatSpec.utilityQ (S : RatSpec M N) (i : Fin M) (B : Fin M → Fin N → Bool) : ℚ :=
  S.ownQ i (B i) + S.θinPop * inPopQ B i + S.θcycle4 * cycle4Q B i

/-- The exact potential. -/
def RatSpec.potentialQ (S : RatSpec M N) (B : Fin M → Fin N → Bool) : ℚ :=
  (∑ m, S.ownQ m (B m)) + S.θinPop * ∑ d, colSumQ B d * (colSumQ B d + 1) / 2
    + S.θcycle4 * ((1 / 2) * ∑ p, ∑ k ∈ univ.erase p,
        overlapQ (B p) (B k) * (overlapQ (B p) (B k) - 1) / 2)

/-- The table respects the influence pattern, checked on codes (decidable). -/
def RatSpec.TableConsistentQ (S : RatSpec M N) : Prop :=
  ∀ (c : Fin (2 ^ N)) (d : Fin N),
    S.C ⟨codeLSB (mask (ofCode N c) (S.E d)), codeLSB_lt _⟩ d = S.C c d

instance (S : RatSpec M N) : Decidable S.TableConsistentQ := by
  unfold RatSpec.TableConsistentQ; infer_instance

/-- Decode a configuration from a natural number: entry `(i, j)` is bit `j + N·i`. -/
def cfgOfCode (M N : ℕ) (c : ℕ) : Fin M → Fin N → Bool := fun i j => c.testBit (j + N * i)

/-- Flip of a concrete configuration (the same function as `SaomNK.flip`, stated without the
real-valued library so this module stays light). -/
def flipQ (B : Fin M → Fin N → Bool) (i : Fin M) (j : Fin N) : Fin M → Fin N → Bool :=
  fun m k => if m = i ∧ k = j then !B i j else B m k

/-- Single-flip stability as a Boolean: no actor gains from toggling one entry. -/
def RatSpec.isLocalOptB (S : RatSpec M N) (B : Fin M → Fin N → Bool) : Bool :=
  (List.finRange M).all fun i => (List.finRange N).all fun j =>
    decide (S.utilityQ i (flipQ B i j) ≤ S.utilityQ i B)

/-- Number of single-flip-stable configurations, by enumeration of all `2^(MN)` codes. -/
def RatSpec.countLocalOpt (S : RatSpec M N) : ℕ :=
  ((List.range (2 ^ (M * N))).filter fun c => S.isLocalOptB (cfgOfCode M N c)).length

end SaomNK
