import SaomNK.NK.Influence
import Mathlib.Algebra.BigOperators.Fin
import Mathlib.Logic.Equiv.Fin.Basic

/-!
# Power keys: binary codes of configurations

An NK landscape stored as a table needs a convention that turns a row `b : Fin N → Bool` into a
row index. Two conventions occur in practice, and both are formalized here.

* **Least-significant-bit first** (`codeLSB`): `code(b) = Σ_j b_j 2^j` (0-indexed `j`). In R's
  1-indexed notation the table row is `1 + Σ_d b_d 2^(d-1)`: the first component is the least
  significant bit. This is the row order of `expand.grid(rep(list(0:1), N))`, which is how the
  searchnet engine enumerates its fitness landscape.
* **Most-significant-bit first** (`codeMSB`): `code(b) = Σ_j b_j 2^(N-1-j)`, the classical power
  key `PK = (2^(N-1), ..., 2^0)`, so that the row is `1 + Σ_d b_d PK_d`. This is the key used to
  look up a component's payoff in a raw `2^N × N` payoff table.

`codeEquiv` shows that the LSB code is a bijection `(Fin N → Bool) ≃ Fin (2^N)`, so a table
indexed by codes is the same thing as a function of configurations.
-/

open Finset

namespace SaomNK

/-- Least-significant-bit-first binary code of a row: `Σ_j b_j 2^j`. -/
def codeLSB : {N : ℕ} → (Fin N → Bool) → ℕ
  | 0, _ => 0
  | _ + 1, b => (b 0).toNat + 2 * codeLSB (fun j => b j.succ)

/-- Decode: the row whose `j`-th entry is bit `j` of `c`. -/
def ofCode (N : ℕ) (c : ℕ) : Fin N → Bool := fun j => c.testBit j

private lemma toNat_add_two_mul_mod (t : Bool) (y : ℕ) : (t.toNat + 2 * y) % 2 = t.toNat := by
  cases t <;> simp

private lemma toNat_add_two_mul_div (t : Bool) (y : ℕ) : (t.toNat + 2 * y) / 2 = y := by
  cases t <;> simp only [Bool.toNat_false, Bool.toNat_true] <;> omega

/-- Bit `k` of the code is entry `k` of the row (and `false` beyond `N`). -/
theorem testBit_codeLSB : ∀ {N : ℕ} (b : Fin N → Bool) (k : ℕ),
    (codeLSB b).testBit k = if h : k < N then b ⟨k, h⟩ else false
  | 0, b, k => by simp [codeLSB]
  | N + 1, b, 0 => by
      simp only [codeLSB, Nat.testBit_zero, toNat_add_two_mul_mod]
      cases h : b 0 <;> simp [h]
  | N + 1, b, k + 1 => by
      rw [codeLSB, Nat.testBit_succ, toNat_add_two_mul_div, testBit_codeLSB]
      by_cases h : k < N
      · simp only [h, dite_true, show k + 1 < N + 1 by omega]; rfl
      · simp only [h, dite_false, show ¬ (k + 1 < N + 1) by omega]

theorem codeLSB_lt : ∀ {N : ℕ} (b : Fin N → Bool), codeLSB b < 2 ^ N
  | 0, _ => by simp [codeLSB]
  | N + 1, b => by
      have := codeLSB_lt (fun j : Fin N => b j.succ)
      have ht : (b 0).toNat ≤ 1 := Bool.toNat_le _
      rw [codeLSB, pow_succ]
      omega

@[simp] theorem ofCode_codeLSB {N : ℕ} (b : Fin N → Bool) : ofCode N (codeLSB b) = b := by
  funext j
  simp [ofCode, testBit_codeLSB, j.isLt]

theorem codeLSB_ofCode {N c : ℕ} (hc : c < 2 ^ N) : codeLSB (ofCode N c) = c := by
  apply Nat.eq_of_testBit_eq
  intro k
  rw [testBit_codeLSB]
  by_cases h : k < N
  · simp only [h, dite_true]; rfl
  · simp only [h, dite_false]
    exact (Nat.testBit_lt_two_pow (lt_of_lt_of_le hc
      (Nat.pow_le_pow_right (by norm_num) (by omega)))).symm

theorem codeLSB_injective {N : ℕ} : Function.Injective (codeLSB (N := N)) := by
  intro b b' h
  rw [← ofCode_codeLSB b, ← ofCode_codeLSB b', h]

/-- The LSB code is a bijection between rows and `Fin (2^N)`. -/
def codeEquiv (N : ℕ) : (Fin N → Bool) ≃ Fin (2 ^ N) where
  toFun b := ⟨codeLSB b, codeLSB_lt b⟩
  invFun c := ofCode N c
  left_inv b := ofCode_codeLSB b
  right_inv c := Fin.ext (codeLSB_ofCode c.isLt)

/-- The LSB code as a weighted sum: `Σ_j b_j 2^j`. In R's 1-indexed notation the table row is
`1 + sum(b * 2^(0:(N-1)))`. -/
theorem codeLSB_eq_sum : ∀ {N : ℕ} (b : Fin N → Bool),
    codeLSB b = ∑ j : Fin N, (b j).toNat * 2 ^ (j : ℕ)
  | 0, _ => by simp [codeLSB]
  | N + 1, b => by
      rw [codeLSB, Fin.sum_univ_succ, codeLSB_eq_sum, Finset.mul_sum]
      simp only [Fin.val_zero, pow_zero, mul_one, Fin.val_succ, pow_succ]
      congr 1
      refine Finset.sum_congr rfl fun j _ => ?_
      ring

/-- Most-significant-bit-first code (the classical power key): `Σ_j b_j 2^(N-1-j)`. -/
def codeMSB {N : ℕ} (b : Fin N → Bool) : ℕ := codeLSB (fun k => b (Fin.rev k))

theorem codeMSB_eq_sum {N : ℕ} (b : Fin N → Bool) :
    codeMSB b = ∑ j : Fin N, (b j).toNat * 2 ^ (N - 1 - (j : ℕ)) := by
  rw [codeMSB, codeLSB_eq_sum]
  refine Fintype.sum_equiv Fin.revPerm _ _ fun k => ?_
  simp only [Fin.revPerm_apply, Fin.val_rev]
  have := k.isLt
  congr 2
  omega

theorem codeMSB_lt {N : ℕ} (b : Fin N → Bool) : codeMSB b < 2 ^ N := codeLSB_lt _

end SaomNK
