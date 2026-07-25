# Strategy Simulation: Student Handout

## What Is This Game?

You are the CEO of a firm competing in an industry. Each round, you decide which **activities** (markets, products, or technologies) to invest in or exit. Your competitors -- both classmates and AI-controlled firms -- are making the same decisions simultaneously.

Your goal: **build the most successful firm** by choosing the right combination of activities over multiple rounds.

This is not about luck. The landscape has structure: some activities complement each other, some markets are crowded, and the best strategy depends on what everyone else is doing.

---

## How It Works

### The Setup

- **You control one firm** in an industry of many firms
- The industry has a fixed set of **activities** (e.g., airline routes, tech products, drug pipelines)
- At the start, your firm holds a random portfolio of activities
- Some of the other firms are controlled by classmates; others are AI opponents

### Each Round

1. **Review your portfolio:** Look at which activities you currently hold
2. **Decide:** Choose activities to **add** (enter a new market) or **drop** (exit a market)
3. **Submit:** Hand in your decision to the instructor
4. **Wait:** All decisions are executed simultaneously -- no one gets to move first
5. **See results:** The leaderboard updates after all firms (students + AI) have acted

### The Leaderboard

You are ranked by **cumulative utility** -- a score that reflects:
- **Scope value:** Each activity you hold generates baseline value
- **Synergy bonus:** Activities that complement each other generate extra value (e.g., operating a hub plus its feeder routes)
- **Crowding penalty:** Activities held by many firms are worth less (more competition = thinner margins)

---

## Reading Your Dashboard

After each round, you can check four key metrics:

| Metric | What It Measures | What to Watch |
|:-------|:----------------|:-------------|
| **Scope (K_AC)** | How many activities you hold | Too many = high costs; too few = missed opportunities |
| **Popularity (K_CA)** | How crowded each activity is | Entering popular activities means more competition |
| **Overlap (K_AA)** | How many rivals share your activities | High overlap = head-to-head competition |
| **Synergy (K_CC)** | How tightly activities are bundled | Activities in the same "cluster" complement each other |

---

## Making Good Decisions

### Things to Consider

**Portfolio fit:** Activities within the same cluster have synergies. Holding ATL-hub + ORD-hub + DFW-hub (all in the same cluster) is more valuable than holding three unrelated routes.

**Competitive crowding:** If everyone piles into the same markets, those markets become less profitable. Sometimes the best move is to go where others are not.

**Timing:** Early rounds are for exploration. Later rounds are for refining and defending your position.

**Opponent awareness:** Watch the leaderboard. If the leader holds certain activities, entering those same markets increases your overlap (K_AA) with them -- which may or may not be a good thing.

### Common Mistakes

- **Over-diversifying:** Holding too many activities spreads you thin. Each activity has a cost.
- **Herding:** Following the crowd into popular markets. If K_CA is high, margins are thin.
- **Ignoring synergies:** Random portfolios miss complementarity bonuses. Look for natural clusters.
- **Not adapting:** Sticking with Round 1 choices even as the competitive landscape shifts.

---

## How Decisions Are Submitted

Your instructor will specify the exact method, but the basic information is:

1. **Your student ID** (e.g., "student_1")
2. **Activities to ADD** -- list the ID numbers of new activities to enter
3. **Activities to DROP** -- list the ID numbers of existing activities to exit
4. You can also **do nothing** (hold your current portfolio)

### Example Decision

> "I am student_5. I want to ADD activities 3 and 7, and DROP activity 1."

This means: enter markets #3 and #7, and exit market #1.

---

## Scoring Explained

Your score each round is approximately:

```
Round Score = (scope value) + (synergy bonus) - (crowding penalty)
```

- **Scope value:** +0.5 per activity held
- **Synergy bonus:** Extra points for holding activities that are in the same complementarity cluster (shown in the epistasis structure)
- **Crowding penalty:** -0.1 for each other firm sharing each of your activities

Your **cumulative utility** is the sum of round scores across all rounds. The final leaderboard ranks everyone by cumulative utility.

---

## Frequently Asked Questions

**Q: Can I see what other students are doing?**
A: You see the leaderboard after each round, which shows ranks and aggregate metrics. You do not see individual decisions until the debrief.

**Q: What do the AI firms do?**
A: They follow a "boundedly rational" decision rule -- they tend to make moves that improve their score, but they are not perfect. Think of them as competent but imperfect competitors.

**Q: Can I go bankrupt?**
A: No. Your firm always survives. But dropping all activities gives you zero utility, and you fall to the bottom of the leaderboard.

**Q: Is there a dominant strategy?**
A: No single portfolio dominates. The best strategy depends on what your competitors are doing. That is the point of the game.

**Q: What happens if I do not submit on time?**
A: Your firm takes no action that round (holds its current portfolio). This is a valid but passive choice.

**Q: How do shocks work?**
A: In intermediate and advanced modes, surprise events can change the rules mid-game (e.g., a market collapses, costs spike). You will not know when shocks are coming. Build a resilient portfolio.
