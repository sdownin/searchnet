import SaomNK.NK.Fitness
import SaomNK.NK.PowerKey

/-!
# NK landscapes stored as tables

Simulation code stores an NK landscape in one of two tables.

* A **raw payoff table** `T : ℕ → Fin N → ℝ`: the payoff of component `d` at row `x` is
  `T (codeMSB (x ⊙ E_d)) d`, the classical power-key lookup. (`rawPayoff`.)
* An **enumerated contribution table** `C : ℕ → Fin N → ℝ`, one row per configuration in LSB
  order: `C (codeLSB x) d` is component `d`'s contribution at configuration `x`. Fitness is the
  row mean. This is what a full enumeration of the landscape stores (`enumerate`).

The enumerated table is consistent with the influence pattern: masking a row by `E_d` does not
change component `d`'s entry (`TableConsistent`). Consistency is exactly what makes the row sum
of the table equal to the NK term (`nkTermFull_tableLookup`), and it can be checked on codes
(`tableConsistent_of_codes`), which is a finite, decidable statement for a concrete table.
-/

open Finset

namespace SaomNK

variable {N : ℕ}

/-- Payoff functions read from a raw table through the MSB power key. -/
def rawPayoff (T : ℕ → Fin N → ℝ) : Fin N → (Fin N → Bool) → ℝ := fun d y => T (codeMSB y) d

/-- Payoff functions read from an enumerated table through the LSB code. -/
def tablePayoff (C : ℕ → Fin N → ℝ) : Fin N → (Fin N → Bool) → ℝ := fun d y => C (codeLSB y) d

/-- Full enumeration of a raw table: row `c` holds the component contributions at
configuration `ofCode N c`. -/
def enumerate (E : Influence N) (T : ℕ → Fin N → ℝ) : ℕ → Fin N → ℝ :=
  fun c d => T (codeMSB (mask (ofCode N c) (E d))) d

/-- The enumerated table respects the influence pattern. -/
def TableConsistent (E : Influence N) (C : ℕ → Fin N → ℝ) : Prop :=
  ∀ (b : Fin N → Bool) (d : Fin N), C (codeLSB (mask b (E d))) d = C (codeLSB b) d

/-- Row sum of an enumerated table at configuration `b`. -/
def tableRowSum (C : ℕ → Fin N → ℝ) (b : Fin N → Bool) : ℝ := ∑ d, C (codeLSB b) d

/-- An enumerated raw table is consistent. -/
theorem enumerate_consistent (E : Influence N) (T : ℕ → Fin N → ℝ) :
    TableConsistent E (enumerate E T) := by
  intro b d
  simp [enumerate]

/-- The enumerated table reproduces the raw-table NK term exactly. -/
theorem tableRowSum_enumerate (E : Influence N) (T : ℕ → Fin N → ℝ) (b : Fin N → Bool) :
    tableRowSum (enumerate E T) b = nkTermFull E (rawPayoff T) b := by
  simp [tableRowSum, enumerate, nkTermFull, rawPayoff]

/-- **Table lookup is the NK term.** For a consistent table, the row sum at `b` is the NK term
with payoffs read from the table. -/
theorem nkTermFull_tableLookup {E : Influence N} {C : ℕ → Fin N → ℝ}
    (hC : TableConsistent E C) (b : Fin N → Bool) :
    nkTermFull E (tablePayoff C) b = tableRowSum C b := by
  unfold nkTermFull tablePayoff tableRowSum
  exact Finset.sum_congr rfl fun d _ => hC b d

/-- Consistency checked on codes `c < 2^N` implies consistency on all rows. The hypothesis is a
finite statement, decidable for a concrete table. -/
theorem tableConsistent_of_codes {E : Influence N} {C : ℕ → Fin N → ℝ}
    (h : ∀ (c : Fin (2 ^ N)) (d : Fin N), C (codeLSB (mask (ofCode N c) (E d))) d = C c d) :
    TableConsistent E C := by
  intro b d
  have := h ⟨codeLSB b, codeLSB_lt b⟩ d
  simpa using this

end SaomNK
