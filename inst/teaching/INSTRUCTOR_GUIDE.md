# searchnet Classroom Simulation: Instructor Guide

## Overview

The searchnet classroom module is a Capsim-style strategy simulation where students compete as firm executives making portfolio decisions on a shared competitive landscape. Each student controls one firm, choosing which activities (routes, products, therapeutic areas) to add or drop each round. AI opponents create realistic competitive dynamics. The simulation is powered by the SAOM-NK engine, which models bounded rationality, competitive interdependence, and endogenous landscape co-evolution.

**Time required:** 75-minute class session (minimum), or 2-3 sessions for deeper engagement.

**Prerequisites:** No coding experience required for students. Instructor needs R installed with the `searchnet` package.

---

## Quick Start

### 1. Install and Check (about 10 minutes on a fresh machine)

You need R 4.1.0 or later (https://CRAN.R-project.org). Then, in R:

```r
# Step 1: install searchnet from GitHub (once per machine)
install.packages("remotes")
remotes::install_github("sdownin/searchnet")

# Step 2: check the installation; runs a 2-30 second seeded test simulation
searchnet::searchnet_check_setup()

library(searchnet)
```

`searchnet_check_setup()` ends with either "All checks passed" or a list of
problems, each with the command that fixes it. Ask workshop participants to
run steps 1 and 2 before the session. For a reproducible class, pin a release
tag, for example `remotes::install_github("sdownin/searchnet@v0.12.9")`.

### 2. Initialize a Session

```r
session <- searchnet_classroom_init(
  n_students = 30,       # one firm per student
  n_rounds   = 8,        # 8 years of decisions
  industry   = "airline", # airline route network
  difficulty = "intro",   # 2 AI firms, no shocks
  seed       = 2026       # reproducible
)
```

### 3. Collect and Submit Decisions

Each round, students submit their adds and drops:

```r
session <- searchnet_classroom_submit(session, "student_1", adds = c(3, 7), drops = c(1))
session <- searchnet_classroom_submit(session, "student_2", adds = c(5),    drops = c(2, 4))
# ... collect all student decisions ...
```

For a whole class, submit the round at once from a form export (see
"Collecting Decisions with a Form" below):

```r
session <- searchnet_classroom_submit_batch(session, "round1_responses.csv")
attr(session, "batch_report")   # one line per row: submitted or why not
```

### 4. Advance the Round

```r
session <- searchnet_classroom_advance(session)
```

### 5. Check Leaderboard

```r
searchnet_classroom_leaderboard(session)
```

### 6. Generate Debrief

```r
searchnet_classroom_debrief(session, output_dir = "class_debrief")
```

---

## Suggested Class Configurations

| Class Size | Preset    | Difficulty   | Rounds | Session Length | Notes |
|:-----------|:----------|:-------------|:-------|:---------------|:------|
| 15-25      | airline   | intro        | 6-8    | 50 min         | MBA core strategy course |
| 25-40      | airline   | intermediate | 8-10   | 75 min         | MBA elective or PhD seminar |
| 15-30      | tech      | intermediate | 8-10   | 75 min         | Technology strategy course |
| 10-20      | pharma    | advanced     | 10-12  | 2 x 75 min     | Healthcare strategy or PhD |
| 5-10       | any       | advanced     | 12-15  | 3 x 50 min     | PhD seminar (deep analysis) |

### Decision Collection Methods

**Low-tech (any class size):** Students write decisions on paper/cards. TA or instructor enters them into R between rounds.

**Medium-tech:** Students submit via Google Form (or Qualtrics, Microsoft Forms). Instructor exports the responses as CSV and submits the round with `searchnet_classroom_submit_batch()`.

### Collecting Decisions with a Form

Build a form with four questions and export the responses as CSV each round.
Extra columns, such as the timestamp or e-mail address a form adds, are
ignored.

| Column       | Example     | Meaning |
|:-------------|:------------|:--------|
| `student_id` | `student_3` | Roster ID. A bare `3` is read as `student_3`. |
| `round`      | `1`         | The round being played (`session$current_round + 1`). |
| `adds`       | `3;7`       | Activity numbers to add, separated by semicolons, commas, bars, or spaces. Blank means none. |
| `drops`      | `1`         | Activity numbers to drop, same format. |

If your form uses other question titles, rename the columns or pass them:
`searchnet_classroom_submit_batch(session, "responses.csv", student_col = "Your ID", adds_col = "Enter", drops_col = "Exit")`.

Every row is checked before anything is submitted. A row is rejected when its
student is not on the roster, its round is not the current one, a cell is not
a list of activity numbers, an activity number is outside 1 to N, or the same
student appears in more than one row. Rejected rows are listed with the
reason; the valid rows are submitted. To submit nothing unless every row is
valid, pass `atomic = TRUE`. Adding an activity a firm already holds, or
dropping one it does not hold, is not an error: as with
`searchnet_classroom_submit()`, it is ignored with a warning. Students who
did not respond stay pending; `searchnet_classroom_advance(session, force =
TRUE)` plays the round with no move for them.

Filter the export to the current round's responses before submitting (a form
that collects every round in one sheet will otherwise produce wrong-round
rejections for the earlier rounds).

**High-tech:** Use the searchnet web app (saomnk-app) where students submit decisions directly through a browser interface.

---

## Industry Presets

### Airline (Default)

12 route markets organized into 3 complementarity clusters:
- **Hub cluster:** ATL-hub, ORD-hub, DFW-hub, LAX-nonstop
- **Coastal/Intl cluster:** SFO-nonstop, JFK-intl, MIA-intl, SEA-tech
- **Regional cluster:** DEN-mountain, BOS-northeast, PHX-sunbelt, MSP-midwest

Students immediately grasp the setting. Good for introducing concepts of scope, competitive overlap, and strategic groups.

### Tech (Platform Strategy)

12 product/service categories in 4 platform ecosystems:
- **Infrastructure:** Cloud-IaaS, Cloud-SaaS, AI-Models
- **Consumer platform:** Mobile-OS, Mobile-Apps, Social-Consumer
- **Enterprise:** AI-Hardware, Social-Enterprise, Cybersecurity
- **Commerce:** E-commerce, Payments, IoT-Devices

Stronger complementarities and popularity effects create winner-take-most dynamics. Good for platform strategy and ecosystem courses.

### Pharma (R&D Portfolios)

12 therapeutic areas in 4 research clusters:
- **Immuno-oncology:** Oncology-Solid, Oncology-Heme, Immunology
- **Chronic disease:** Neuroscience, Cardio-Metabolic, Rare-Disease
- **Platform tech:** Gene-Therapy, mRNA-Platform, Vaccines
- **Specialty:** Biosimilars, Anti-Infectives, Ophthalmology

High scope costs reflect capital intensity. Good for healthcare strategy and R&D management courses.

### Custom Presets

The three presets are JSON files in
`system.file("teaching", "presets", package = "searchnet")`. A custom preset
uses the same fields; copy one of them as a starting point.

| Field | Required | Type | Meaning |
|:------|:---------|:-----|:--------|
| `N` | yes | whole number, at least 2 | Number of activities. Overrides the `N` argument of `searchnet_classroom_init()`. |
| `activity_names` | yes | list of distinct strings, at most `N` | Activity labels, in column order. Fewer than `N` are padded as `Activity_k`. |
| `density` | yes | number in [0, 1] | Initial density of the firm-activity network. |
| `influence_weight` | yes | number | Weight on the complementarity (influence matrix) term. The older name `epistasis_weight`, used by the shipped presets, is also accepted. |
| `blocks` | no (default 3) | whole number, 1 to `N` | Number of complementarity clusters in the block-diagonal influence matrix. |
| `epistasis` | no | `"modular"` | The only structure implemented (block-diagonal). |
| `density_param` | no (default -0.5) | number | Density (cost of holding an activity) coefficient. |
| `popularity` | no (default 0.3) | number | Popularity (crowding) coefficient. |
| `difficulty_settings` | no | object | If given, must define `intro`, `intermediate`, and `advanced`, each with `steps_per_round` (whole number, at least 1), `n_AI` (whole number, at least 1), and `shock_probability` (number in [0, 1]). The defaults are 5/2/0, 10/4/0.1, and 20/8/0.3. |
| `industry`, `description` | no | string | Labels for your own records. |
| `learning_objectives` | no | list of strings | Notes for your own records. |

Any other field produces a warning and is ignored. Check a file before class,
then use it:

```r
searchnet_validate_preset("my_preset.json")   # lists every problem at once
session <- searchnet_classroom_init(
  n_students = 20, industry = "custom",
  custom_params = "my_preset.json",           # a path or a named list
  difficulty = "intro", seed = 2026
)
```

---

## Difficulty Levels

| Level          | AI Firms | Steps/Round | Shocks | Best For |
|:---------------|:---------|:------------|:-------|:---------|
| **intro**      | 2        | 5           | None   | First exposure; MBA core |
| **intermediate** | 4      | 10          | 10-15% per round | Elective; deeper engagement |
| **advanced**   | 8        | 20          | 30-40% per round | PhD; multi-session games |

---

## Discussion Questions for Debrief

### After Intro Games (Rounds 1-3)

1. What was your initial strategy? Did you specialize or diversify?
2. Look at the K_AA panel: are firms converging or diverging? Why?
3. Which activities became "crowded"? Did you anticipate that?

### After Intermediate Games (Midpoint)

4. Has a dominant strategic group emerged? What activities define it?
5. Look at the epistasis structure (the block-diagonal matrix). Did you discover the complementarity clusters through play, or did you need to be told?
6. Compare your portfolio to the nearest AI firm. Where do you overlap? Where do you differ?

### After Advanced Games / Full Debrief

7. **The Imitation Mirage:** Many students converge on the same "best" portfolio. But if everyone holds the same routes, K_AA is maximized and differentiation advantage disappears. Did this happen in your class? What is the strategic implication?
8. **Architecture vs. Content:** Two firms can have the same scope (K_AC) but very different competitive positions. How does the *pattern* of activities matter beyond their *number*?
9. **Shock Response:** If a shock hit (e.g., demand collapse on popular routes), which firms survived best -- the diversified ones or the focused ones? Why?
10. **Counterfactual Thinking:** The AI firms followed a logit choice rule (bounded rationality). If you could see the AI's "thought process," would it help or hurt your strategy?
11. **Did the Starting Position Decide It?** Students who drew a crowded opening portfolio often believe they were dealt a losing hand. Run the ergodicity demonstration live (below) and ask them to reconcile it with their experience: if the long-run distribution does not depend on the starting configuration, why did the opening position feel decisive within the rounds you played?

---

## Live Demonstration: Does the Starting Position Matter?

A five-minute demonstration that reliably changes how students read their own
results. It answers the objection that the simulation just rewards a lucky
opening draw.

```r
erg <- searchnet_ergodicity_sweep(
  M = 12, N = 15, start_densities = c(0.1, 0.8),
  run_lengths = c(15, 30, 60, 120, 240),
  replicates = 6, equivalence_margin = 0.05, seed = 42
)
erg          # printed verdict
plot(erg)    # two panels: arms converging, gap decaying
```

Two portfolios start at opposite extremes --- one nearly empty, one nearly
full --- and end up statistically indistinguishable, with the gap between them
shrinking as a power of the number of decisions made.

**The teaching point is the reconciliation, not the result.** Both things are
true at once: the long-run equilibrium does not depend on the starting
position, *and* the starting position dominates within any short run. A
typical class plays 10 rounds. The sweep shows the arms are still clearly
apart at 15 iterations per actor. So students are right that their opening
mattered --- for the horizon they actually played --- and also wrong to
conclude the game was decided at the draw. That gap between the short-run and
long-run answer is the substantive lesson, and it maps directly onto how long
a firm has to reposition before its starting configuration stops explaining
its performance.

**If you want to make it fail on purpose,** pass `run_lengths = 5` and the
function reports NOT EQUIVALENT. Showing students a demonstration that can
come out the other way is worth the extra minute: it is the difference
between a result and a slogan.

---

## Connection to Textbook Concepts

| Concept | Textbook Source | searchnet Mapping |
|:--------|:---------------|:-----------------|
| Competitive advantage | Porter (1985) | Utility function: firms with higher utility have "advantage" |
| Strategic groups | Caves & Porter (1977) | K_AA projection: firms clustered by activity overlap |
| Diversification | Rumelt (1974) | K_AC (scope): number of activities held |
| Competitive dynamics | Chen (1996) | Multimarket contact via bipartite overlap |
| NK landscapes | Kauffman (1993) | Influence matrix (W): component interdependence, the model input |
| Bounded rationality | Simon (1955) | Beta parameter: rationality thermostat |
| Resource complementarity | Milgrom & Roberts (1990) | Block-diagonal W: modular synergy structure |
| Red Queen competition | Barnett & Hansen (1996) | Co-evolutionary dynamics in K-4 trajectories |
| Platform strategy | Gawer & Cusumano (2002) | Tech preset with strong complementarities |
| Real options | McGrath (1999) | Sequential add/drop decisions under uncertainty |

---

## Grading Rubric Suggestions

### Option A: Performance-Based (30% of participation grade)

| Component | Weight | Criteria |
|:----------|:-------|:---------|
| Final rank | 10% | Top quartile = full credit; linear scale for rest |
| Participation | 10% | Submitted decisions every round on time |
| Debrief reflection | 10% | 1-page written analysis of own strategy and outcomes |

### Option B: Analysis-Based (Part of a larger assignment)

| Component | Weight | Criteria |
|:----------|:-------|:---------|
| Strategy memo (pre-game) | 15% | Articulate planned strategy using course concepts |
| Decision rationale (per round) | 25% | Brief justification for each round's adds/drops |
| Post-game analysis | 35% | 3-5 page paper connecting outcomes to theory |
| Peer comparison | 25% | Compare own trajectory to 2 classmates using K-4 data |

### Option C: Team-Based Variant

Students form teams of 2-3. Each team controls one firm. This works well for large classes (60+ students) and encourages strategic discussion.

---

## Troubleshooting

**"All students converge on the same strategy"**
This is the Imitation Mirage -- it is a feature, not a bug. Use it as a teaching moment about the tension between individual optimization and collective differentiation.

**"Students do not know what to do in Round 1"**
Give them the Student Handout. In the airline preset, suggest they "pick a hub and 2-3 connecting routes" as a starting heuristic.

**"The AI firms are too strong/weak"**
Adjust difficulty level. For more control, use `industry = "custom"` and set `n_AI` and `steps_per_round` in the preset's `difficulty_settings` (see Custom Presets).

**"A participant's installation does not work"**
Ask them to run `searchnet::searchnet_check_setup()` and read the last lines: each problem comes with the command that fixes it.

**"I want to add a shock mid-game"**
Set `shocks = TRUE` in the init call. Shock timing and type are pre-generated but hidden from students. To add manual shocks, modify the `shock_schedule` element of the session object directly.

---

## Technical Notes

- The session object is a plain R list with class `"searchnet_classroom"`. It is fully serializable with `saveRDS()` / `readRDS()` for multi-session games.
- Student decisions are applied simultaneously (not sequentially) to avoid first-mover advantage.
- AI firms follow the SAOM conditional logit choice rule with the same model parameters as the environment.
- The leaderboard uses cumulative utility across rounds, which incorporates scope value, epistasis bonuses, and crowding penalties.
