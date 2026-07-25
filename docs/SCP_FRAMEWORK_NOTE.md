# The SCP Connection: Structure-Conduct-Performance as a Stochastic 3-Body Problem

## The Classical SCP Paradox

Bain (1956) and Mason (1939) proposed the Structure-Conduct-Performance (SCP) paradigm as a
**linear causal chain**: industry Structure determines firm Conduct, which determines Performance.
Porter (1980, 1981) refined this into the Five Forces framework but preserved the directional logic.

The SCP paradigm was critiqued for ignoring feedback loops (Scherer & Ross, 1990), endogeneity
(Sutton, 1991), and the simultaneous co-determination of all three (Caves, 2007). These critiques
identified the problem but offered no formal resolution.

## The searchnet Resolution: SCP as Coupled Stochastic Process

The searchnet/{K} framework resolves the SCP paradox by formalizing all three dimensions as
components of a single coupled stochastic process on a bipartite network:

| SCP Dimension | searchnet Phase Space | Formal Object | Equation |
|---|---|---|---|
| **Structure** | Network | B ∈ {0,1}^{M×N}, BB', B'B | K_AC, K_CA, K_AA, K_CC |
| **Conduct** | Behavior | Actor covariates, choice function | P(flip j) = exp(β·ΔU) / Σ exp(β·ΔU) |
| **Performance** | Fitness/Utility | Objective function | U_i = NK + SAOM statistics |

### Why This Is a "3-Body Problem"

In classical mechanics, the 3-body problem (Poincaré, 1890) is analytically intractable because
three mutually gravitating bodies create chaotic, path-dependent dynamics. The SCP system has
the same structure:

1. **Structure → Conduct**: The network topology (who does what) defines the choice set and
   utility landscape for each actor's decisions
2. **Conduct → Structure**: Each actor's choices (add/drop activities) directly modify the
   bipartite network structure
3. **Performance → Conduct**: Utility outcomes feed back into future choice probabilities
   through the logit choice function
4. **Structure → Performance**: The {K} configuration determines the fitness landscape topology
5. **Conduct → Performance**: Behavioral covariates (strategy types) modulate utility components
6. **Performance → Structure**: High-fitness configurations attract imitation, reshaping the network

All six feedback channels operate **simultaneously** within each ministep of the continuous-time
Markov chain. This is why the system is analytically intractable (like the gravitational 3-body
problem) but computationally tractable (via simulation).

### The Phase Space Plot as SCP Visualization

The `saomnk_plot_phase_space_3d()` function literally plots the SCP 3-body trajectory:

```r
# The SCP phase space
saomnk_plot_phase_space_3d(env,
  x_var = "K_AC",              # Structure (scope)
  y_var = "exploration_rate",   # Conduct (search behavior)
  z_var = "utility",            # Performance
  color_by = "strategy"         # Firm type
)
```

Each actor traces a path through Structure × Conduct × Performance space. The path is:
- **Path-dependent**: each step constrains future steps (historical contingency)
- **Coupled**: my path affects your path (competitive interaction)
- **Stochastic**: bounded rationality introduces randomness (not deterministic)
- **Endogenous**: the landscape itself evolves as actors move (not given)

### Implications for Strategy Theory

1. **SCP is not a chain but a manifold**: The "correct" SCP relationship is not S→C→P
   but a trajectory on a 3D manifold where all three dimensions co-evolve.

2. **Strategic groups are attractors**: Clusters of firms in SCP phase space correspond
   to strategic groups (Caves & Porter, 1977), but they arise endogenously from the
   coupled dynamics rather than being exogenously defined by industry boundaries.

3. **Industry evolution is a flow**: The entire industry traces a trajectory through
   SCP space. Shocks (theta_shocks) perturb this trajectory. Some perturbations
   are absorbed (resilience); others cause phase transitions (industry transformation).

4. **The "structure debate" is dissolved**: The decades-long debate over whether structure
   determines performance or vice versa is dissolved by recognizing that both are
   simultaneously determined by the same underlying stochastic process.

5. **Complexity is in the coupling, not the components**: Individual S, C, or P
   measurements may be simple. The complexity arises from their mutual coupling —
   this is precisely what the {K} framework captures with four coupled degree measures.

## Connection to Existing Literature

- **Porter (1980, 1981)**: Five Forces as a special case of the {K} configuration
  where K_CA = rivalry intensity, K_CC = supplier/buyer bargaining, K_AC = scope
- **Caves & Porter (1977)**: Strategic groups as phase space attractors
- **Scherer & Ross (1990)**: SCP feedback loops formalized as CTMC ministeps
- **Sutton (1991)**: Endogenous sunk costs as a landscape deformation mechanism
- **Rumelt (1991)**: Persistent performance heterogeneity explained by attractor
  multiplicity in the SCP phase space (different firms, same industry, different basins)
- **McGahan & Porter (1997)**: Variance decomposition across S, C, P components
  reinterpreted as projections of the 3D trajectory onto each axis

## For the OrgSci Paper

The SCP connection provides a powerful framing: "We formalize the SCP system as a stochastic
3-body problem on bipartite networks, revealing that the classical paradox of causal direction
dissolves when Structure, Conduct, and Performance are modeled as coupled dimensions of a
single continuous-time process."

This connects the SAOM/network dynamics literature (OrgSci's home territory) to the SCP/IO
economics tradition (strategy's foundational paradigm), positioning the paper as bridging
two major intellectual traditions.
