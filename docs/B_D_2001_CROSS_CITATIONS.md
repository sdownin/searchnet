# Brock & Durlauf (2001) Cross-Citation Pack

**Purpose.** Theorem 5 (SAOM-NK as B&D Generalization) is now formalized in the
searchnet R package as PROOF_TABLE.md Part L (rows L1-L17) with full code
operationalization in `R/searchnet-brock-durlauf.R`, vignette
`vignettes/saomnk-brock-durlauf.Rmd`, manim scene
`inst/manim/scene_brock_durlauf.py`, JSS manuscript Section "Three foundations"
and Online Appendix H. **Phase 5 (April 2026 enrichment)** added the
McFadden (1974) common-ancestor framing and the Glauber-thermodynamic duality
(SAOM = Glauber dynamics on potential game; B&D = thermodynamic free energy of
the *same* potential), recorded as PROOF_TABLE.md row L17 and elaborated in
the JSS manuscript "Three foundations from one ancestor" subsection.

The two **companion projects** that should cross-cite this work are sandboxed
outside the searchnet working directory and need manual pasting:

- `D:\CD2026\` -- the flagship MgmtSci paper "Architecture of Rivalry"
- `D:\industrial_policy_AMR\` -- the AMR paper "Industrial Policy & Firm Search"

This document provides paste-ready text blocks. Copy each block into the
indicated file and section.

---

## Part 1. CD2026 (Management Science) -- INSERT

The CD2026 paper already cites Snijders 2001 (sociology) and Blume 1993
(stat-mech). It does **not** currently cite Brock & Durlauf 2001. Adding the
B&D citation closes the third foundational pillar (economics) and provides
the rigorous econometric identification strategy Manski (1993) would otherwise
flag as missing.

### 1A. Theory section -- "Three foundations" subsection

**Target file:** `D:\CD2026\manuscript\sections_1_3_draft.md` (or
`paper.Rmd` if that's the canonical) -- find the section discussing the
SAOM-NK theoretical foundation.

**Paste this paragraph after the existing Snijders / Blume citation:**

> The SAOM-NK framework that operationalizes our {K} model is grounded in
> three independent literatures, each of which delivers a complementary
> equilibrium concept. Snijders (1996, 2001) supplies the sociological
> micro-foundation: actor-oriented logit choice over network ties.
> Blume (1993), via the potential-game machinery of Monderer and
> Shapley (1996), supplies the statistical-mechanical / game-theoretic
> equilibrium concept: the simulation's stationary distribution is a Gibbs
> measure that is also a logit Quantal Response Equilibrium (McKelvey &
> Palfrey, 1995). Brock and Durlauf (2001) supply the economic foundation:
> binary discrete choice with social interactions, with rational-expectations
> self-consistency $m^{*} = \tanh(\beta h + \beta J m^{*})$ and the
> well-known multiplicity threshold $\beta J > 1$. Theorem 5 of Downing
> (2026, searchnet JSS) formally establishes that Brock and Durlauf (2001)
> is recovered as a degenerate special case of SAOM-NK in the
> $M \to \infty$, congestion-only, mean-field limit. This three-foundation
> grounding is what allows our airline-rivalry analysis to draw both the
> social-multiplier diagnostic (Brock-Durlauf social multiplier, Corollary
> 3 of the searchnet proof registry) and the discrete-choice identification
> result (Manski's reflection problem resolved via logit nonlinearity)
> directly from established econometric theory.

### 1B. Empirics / identification section

**Target file:** `D:\CD2026\manuscript\sections_4_5_draft.md` or
`D:\CD2026\manuscript\redteam\R4_econometrics.md` -- find the section on
identification of social effects from the airline route data.

**Paste this paragraph in the identification subsection:**

> Brock and Durlauf's (2001) discrete-choice-with-social-interactions
> framework yields a constructive resolution to Manski's (1993) reflection
> problem: the logit nonlinearity in the equilibrium condition $m_j =
> \tanh(\beta h + \beta J m_j)$ breaks the linear collinearity that
> confounds endogenous social effects with exogenous contextual effects in
> the standard linear-in-means peer-effects regression. Our airline-route
> entry decisions inherit this identification result through Theorem 5 of
> the searchnet proof registry: with logit choice probabilities, $J$
> (rivalry-driven entry) and $h$ (route-level demand fundamentals) are
> separately identified from cross-route variation in adoption rates and
> from the curvature of the logit response. Bipartite structure
> (carriers $\times$ routes) provides additional identifying variation
> beyond Brock and Durlauf's single-choice setting, supporting the
> econometric specification in Section [4 / 5] of this manuscript.

### 1C. Theoretical contributions / red-team R1 response

**Target file:** `D:\CD2026\manuscript\redteam\R1_formal_theory.md`
-- the formal-theory referee report.

**Add this point to the response:**

> *On the choice-theoretic foundation:* The reviewer's question about
> whether {K} dynamics are anchored in econometric choice theory is
> addressed by Theorem 5 (Brock-Durlauf Recovery) in the companion
> searchnet (JSS) submission: the canonical Brock and Durlauf (2001)
> binary-choice-with-social-interactions model is a degenerate special
> case of our SAOM-NK simulation, recoverable by setting
> $M \to \infty$, $K = 0$, and zeroing all non-social coupling parameters.
> Specifically, our airline-rivalry application maps to Brock and Durlauf's
> $J$ (social-interaction strength) via the congestion coefficient
> $\beta_{cong}$ in the 10-component utility (Definition 2, see also
> PROOF_TABLE.md row H6), with the explicit $\{0,1\} \leftrightarrow
> \{-1,+1\}$ spin-recoding factor of 4 documented in Appendix H of the
> searchnet Online Appendix.

### 1D. References to add to CD2026 bibliography

**Target file:** `D:\CD2026\manuscript\references.bib` (or wherever the
bibliography lives).

```bibtex
@incollection{McFadden1974,
  author    = {McFadden, Daniel},
  title     = {Conditional logit analysis of qualitative choice behavior},
  booktitle = {Frontiers in Econometrics},
  editor    = {Zarembka, Paul},
  pages     = {105--142},
  publisher = {Academic Press},
  year      = {1974}
}

@article{BrockDurlauf2001,
  author  = {Brock, William A. and Durlauf, Steven N.},
  title   = {Discrete Choice with Social Interactions},
  journal = {Review of Economic Studies},
  volume  = {68},
  number  = {2},
  pages   = {235--260},
  year    = {2001}
}

@article{BlumeDurlauf2003,
  author  = {Blume, Lawrence E. and Durlauf, Steven N.},
  title   = {Equilibrium concepts for social interaction models},
  journal = {International Game Theory Review},
  volume  = {5},
  number  = {3},
  pages   = {193--209},
  year    = {2003}
}

@article{DurlaufIoannides2010,
  author  = {Durlauf, Steven N. and Ioannides, Yannis M.},
  title   = {Social interactions},
  journal = {Annual Review of Economics},
  volume  = {2},
  number  = {1},
  pages   = {451--478},
  year    = {2010}
}

@article{Durlauf1999,
  author  = {Durlauf, Steven N.},
  title   = {How can statistical mechanics contribute to social science?},
  journal = {Proceedings of the National Academy of Sciences},
  volume  = {96},
  number  = {19},
  pages   = {10582--10584},
  year    = {1999}
}

@article{Manski1993,
  author  = {Manski, Charles F.},
  title   = {Identification of Endogenous Social Effects: The Reflection Problem},
  journal = {Review of Economic Studies},
  volume  = {60},
  number  = {3},
  pages   = {531--542},
  year    = {1993}
}

@article{PinkKretschmerLeszczensky2020,
  author  = {Pink, Sebastian and Kretschmer, David and Leszczensky, Lars},
  title   = {Choice modelling in social networks using stochastic actor-oriented models},
  journal = {Journal of Choice Modelling},
  volume  = {34},
  pages   = {100202},
  year    = {2020}
}

@article{SnijdersSteglichSchweinberger2007,
  author  = {Snijders, Tom A. B. and Steglich, Christian E. G. and Schweinberger, Michael},
  title   = {Modeling the co-evolution of networks and behavior},
  journal = {Longitudinal Models in the Behavioral and Related Sciences},
  pages   = {41--71},
  year    = {2007},
  publisher = {Lawrence Erlbaum}
}
```

### 1E. Lineage paragraph (stronger version, ~157 words, parallel to AMR V17ap)

If CD2026 wants to match the AMR project's framing (V17ap.docx already
contains a parallel paragraph for the AMR paper), paste this *stronger*
version into the CD2026 theory section:

> The framework combines two lineages of discrete choice that share
> McFadden's (1974) random-utility ancestor but evolved without
> cross-citation. The economic social-interactions branch (Brock &
> Durlauf, 2001; Blume & Durlauf, 2003; Durlauf & Ioannides, 2010)
> extends McFadden by adding peer-coupling terms and characterising
> equilibrium distributions via mean-field self-consistency. The
> sociological network-dynamics branch (Snijders, 1996; Snijders,
> Steglich, & Schweinberger, 2007; Snijders et al., 2010) extends
> McFadden by embedding logit choice in a continuous-time Markov
> ministep process over network and behavioural states. Pink, Kretschmer,
> and Leszczensky (2020) noted the absence of cross-citation between
> the two and bridged SAOM to baseline RUM but did not connect SAOM to
> the equilibrium social-interactions branch. Our SAOM-NK model
> completes that bridge: SAOM provides the agent-level continuous-time
> dynamics, and under rational expectations about peer behaviour its
> stationary distribution coincides with the Brock-Durlauf mean-field
> equilibrium. The two literatures describe the same object, one as a
> dynamic process, the other as an equilibrium distribution.

---

## Part 2. AMR (Academy of Management Review) -- INSERT

The AMR paper *already cites* Brock & Durlauf 2001 extensively (see
`herding_steepness_appendix.Rmd` line 250 onward;
`AMR_Mathematical_Appendix.Rmd` lines 65, 588, 647, 692, 743, 806, 976). What
is missing is the *reciprocal* cross-citation to the searchnet JSS paper
where Theorem 5 establishes the formal reduction.

### 2A. Computational engine cross-citation

**Target file:** `D:\industrial_policy_AMR\AMR_Mathematical_Appendix.Rmd`
or `Industrial Policy V17.docx` -- find the section discussing the
computational implementation of the herding mean-field result.

**Paste this footnote or sentence near the herding/Landau-Ginzburg derivation:**

> The herding mean-field self-consistency derived here is operationalized
> as Theorem 5 (Brock-Durlauf Recovery via Congestion Limit) of the
> searchnet R package (Downing 2026, JSS submission) in the limit
> $M \to \infty$, congestion-only, with the explicit $\{0,1\}
> \leftrightarrow \{-1,+1\}$ spin-recoding factor of 4 tracked in Rb4 of
> PROOF_TABLE.md row L6. The R function
> `searchnet::verify_brock_durlauf_reduction()` provides numerical
> validation: empirical mean adoption from finite-$M$ SaoMNK CTMC
> simulations converges to the analytical Brock-Durlauf fixed point at
> rate $O(M^{-1/2})$ (Ellis 1985, Theorem IV.4.1).

### 2B. Tier 1/2/3 epistemic taxonomy adoption

**Target file:** `D:\industrial_policy_AMR\theory_structure_table.Rmd`
-- this already classifies AMR results into Tiers 1/2/3.

**Paste this note acknowledging adoption by searchnet:**

> The Tier 1/2/3 epistemic classification scheme introduced here has been
> adopted by the searchnet (JSS) proof registry as the framework for
> classifying mean-field reductions. Specifically, Lemma 6 (mean-field
> self-consistency, PROOF_TABLE.md row L9) is classified as Tier 1
> (analytically exact under rational expectations); Theorem 5 itself
> inherits Tier 1 status for stationary-distribution claims and Tier 2
> for finite-$M$ trajectory claims (where mixing-time effects matter).

### 2C. References to add to AMR bibliography

**Target file:** AMR `.bib` or reference manager.

```bibtex
@misc{Downing2026searchnet,
  author = {Downing, Stephen},
  title  = {searchnet: Stochastic Actor-Oriented Models on {NK} Fitness Landscapes},
  year   = {2026},
  howpublished = {{R} package; submission to \emph{Journal of Statistical Software}},
  note   = {See PROOF\_TABLE.md Part L (Theorem 5: Brock-Durlauf Recovery) and
            Online Appendix H}
}
```

---

## Part 3. Triad cross-citation summary

After applying the above pastes, the three-paper program will have a
mutually-reinforcing citation structure with **McFadden (1974) as common
ancestor** anchoring the entire genealogy:

| Paper | Cites... | For... |
|-------|---------|--------|
| **searchnet (JSS)** | McFadden 1974 (ancestor); Snijders 1996/2001/2007/2010; Pink et al 2020 (prior bridge); Blume 1993; Blume & Durlauf 2003; Brock & Durlauf 2001; Durlauf & Ioannides 2010; Monderer-Shapley 1996; Manski 1993; CD2026; AMR | Three formal foundations + Glauber-thermodynamic duality + companion empirical applications |
| **CD2026 (MgmtSci)** | McFadden 1974 (NEW); Snijders 1996/2010; Blume 1993; Brock-Durlauf 2001 (NEW); Blume & Durlauf 2003 (NEW); Durlauf & Ioannides 2010 (NEW); Pink et al 2020 (NEW); Manski 1993 (NEW); searchnet JSS | Theoretical grounding for {K} framework + identification strategy + computational engine |
| **AMR** | Brock-Durlauf 2001 (existing, V17ap); Durlauf 1999 (existing); Pink et al 2020 (V17ap); McFadden 1974 (V17ap); Snijders et al 2007 (V17ap); Blume & Durlauf 2003 (V17ap); Durlauf & Ioannides 2010 (V17ap); searchnet JSS (NEW) | Economic foundation for herding/steepness results + computational engine + lineage bridge |

The 8-citation overlap (McFadden, Snijders 1996/2001/2007/2010, Pink 2020,
Blume & Durlauf 2003, Brock & Durlauf 2001, Durlauf 1999, Durlauf &
Ioannides 2010, Manski 1993, plus the three-paper internal cross-cites)
turns the program from "three papers that share authorship" into "three
papers that share **lineage, mechanism, and theorem inventory**" -- a
substantively stronger position for review.

**Key lineage claim (consistent across all three papers).** SAOM and
Brock-Durlauf describe the *same Markov chain*, one as a dynamic process
(Glauber dynamics) and the other as an equilibrium distribution
(thermodynamic free energy). McFadden (1974) is the common RUM ancestor;
Pink, Kretschmer & Leszczensky (2020) provided the prior partial bridge
(SAOM <-> RUM); SaoMNK Theorems 4 and 5 together extend that bridge to
SAOM <-> RUM-with-social-interactions <-> Brock-Durlauf.

---

## Part 4. Status / what to do next

- [x] searchnet PROOF_TABLE.md Part L written
- [x] searchnet R utility functions written
- [x] searchnet vignette written
- [x] searchnet manim scene written
- [x] searchnet JSS manuscript bridging paragraph + bibliography updated
- [x] searchnet Online Appendix H written
- [x] searchnet existing Durlauf citations upgraded
- [x] This cross-citation pack written (you are reading it)
- [ ] **USER ACTION REQUIRED**: paste Part 1 sections into CD2026 manuscript
- [ ] **USER ACTION REQUIRED**: paste Part 2 sections into AMR manuscript

If sandbox permissions are loosened (e.g., adding D:\CD2026 to the allowed
read paths) a future agent can complete Part 1 automatically and run a
cross-cite QA to verify both directions of the citation are wired up.

*Document generated 2026-04-25 as part of B&D2001 ecosystem integration.*
