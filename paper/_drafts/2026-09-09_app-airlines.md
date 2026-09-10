# Draft prose: replacement for Section "Competitive convergence in U.S. airlines"

**Date:** 2026-09-09
**Status:** DRAFT PROSE FOR ABSORPTION, NOT FOR INSERTION.
**Drafted by:** Claude (AI-drafted; see the AI-provenance note at the end).

## Why this file exists

`paper/searchnet-jss.Rmd`, subsection `{#sec:app-airlines}`, currently reports
the empirical and simulation results of the unpublished companion manuscript:
the 4.8-fold pairwise-overlap series for U.S. carriers 1993-2025, the
channel-deletion table (baseline 0.034, imitation removed 0.108, epistasis
removed 0.010, all competition removed 0.399), and the 54-cell robustness
sweep. The companion goes to Management Science under blind review; this paper
publishes under the author's name. Carrying those numbers here is a
deanonymization bridge. Separately, JSS requires that every result in the paper
be reproducible from the replication bundle, and none of these are: they come
from an independent procedural implementation that is not shipped.

The figure `figures/cd2026_kaa_trajectory.png` has already been removed from
this subsection by a mechanical edit. The numbers have not been touched,
because replacing them requires new prose, which under the collaboration model
does not go into the working document.

Two framings follow. Both keep what the subsection contributes *to a software
paper* (the three-layer conformance split and the path-integral probe) and drop
the empirical payload. Both are written to be dropped in at the same location,
replacing everything from "**Research question.**" to "**Takeaway.**"

---

## Framing A: retitle as a validation-practice subsection

Keeps the material as a first-class subsection, but makes conformance testing
the subject rather than the airline finding. Suggested heading: "Validating
against an independent implementation."

> **The problem.** A simulation package that reimplements an existing model has
> to establish that it computes the same thing, and the obvious test is the
> wrong one. Bit-identical trajectories are not available across paradigms:
> `searchnet` is R6 over `RSiena` and a typical reference engine is procedural,
> so the random number stream is consumed in a different order. Reporting a
> cross-paradigm trajectory mismatch as a defect is a category error.
>
> **The design.** `searchnet` ships a conformance harness that runs a grid of
> parameter cells crossed with seeds through both implementations and compares
> them on three layers, with different standards for each. *Structural
> agreement* asks whether the two engines produce the same network statistics
> at the same design points, and must hold. *Invariants* ask whether both
> satisfy the properties the model is claimed to have, and must hold: the NK
> reduction of Theorem 1 and monotonicity of crowding in the rivalry parameter
> are the two the harness checks. *Trajectory hashes* ask whether the two took
> the same path, and need not agree. Separating the three is what makes the
> comparison interpretable, because it says in advance which disagreements are
> evidence of a bug and which are evidence of nothing.
>
> **A probe that locates its own boundary.** The potential-game property of
> Section 3.4 is tested on the simulation code rather than on a reduced model:
> add a component, then drop it, and net utility must return to zero. It does,
> to machine precision, for the core specification. The same probe marks where
> the guarantee stops. With a similarity-weighted imitation term active the
> path integral does not close, because that term depends on other actors'
> performances, which move when the focal actor moves. The formal guarantees
> are therefore claimed for the core specification only, and the probe is what
> draws the line rather than an assertion in the text.
>
> **Takeaway.** For anyone validating a simulation package against an existing
> implementation, the transferable practice is the three-layer split, which
> fixes the standard of evidence before the comparison is run, and the
> path-integral probe, which reports the scope of a formal guarantee instead of
> asserting it.

## Framing B: demote to a paragraph inside the applications preamble

Cuts the subsection entirely, deletes the "Competitive convergence" row from
the applications table, and moves the one durable point into the preamble that
already distinguishes the three modes of use. Shortest option, and the one that
removes the exposure most completely.

> The third mode, reference implementation, deserves a note because it changes
> what counts as a passing test. When `searchnet` is checked against an
> independent engine for the same model, the comparison runs on three layers
> with three different standards: structural agreement at matched design points
> and model invariants (the NK reduction of Theorem 1, monotonicity of crowding
> in the rivalry parameter) must hold, while trajectory hashes need not, since
> an R6 layer over `RSiena` and a procedural engine consume the random number
> stream in a different order. Fixing that standard in advance is what keeps a
> paradigm difference from being reported as a defect. The conformance harness
> in `inst/` implements the split.

## Framing C: replace the application with a reproducible one

If the applications inventory should stay at eight, the vacancy can be filled
by an application that the replication bundle can regenerate. The two new
simulation figures added in `paper/replication/make_k_system_figures.R` already
demonstrate the coupled dynamics and the shock relocation that the airline
material was carrying illustratively. A subsection built on those runs would be
fully reproducible and carries no anonymity exposure, but it is a new
application rather than an edit, and the author should decide whether the paper
needs an eighth.

---

## YOUR CALL

- Which framing, if any. A is the least disruptive to the paper's structure; B
  removes the exposure most completely; C keeps the count at eight at the cost
  of writing a new application.
- Whether the applications table row "Competitive convergence | U.S. airlines,
  1993-2025 | Reference implementation, conformance-checked" also goes. Under B
  it must. Under A it should be retitled, since the domain cell still names the
  companion's setting.
- The sentence in the applications preamble that says "eight independent
  research programs" needs its count adjusted under B.

## GAP

- Framings A and B both assert what the conformance harness checks. Verify
  against the harness itself before this reaches the document; the current text
  in the Rmd is the only source consulted here, and it describes the harness in
  the companion's terms.
- The claim that the path integral closes "to machine precision" for the core
  specification is carried over from the existing text and was not re-verified.
- The numeric deviation (2.68 at calibrated coefficients) has been dropped from
  both framings deliberately: it is a companion-specific quantity.

## AI provenance

Every indented block above is AI-drafted prose. Nothing from this file has been
inserted into `paper/searchnet-jss.Rmd`; the Rmd carries only a TODO comment
pointing here. Re-voice before use, and record the use in the project's session
notes so a disclosure statement can be assembled from the record.
