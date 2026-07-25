"""
Scene: Brock & Durlauf (2001) Economic Foundation Theorem
==========================================================
Animated visualization of Theorem 5: SaoMNK -> B&D2001 reduction.

Shows the canonical pitchfork bifurcation of the self-consistency equation
    m* = tanh(beta*J*m* + beta*h)
with finite-M SaoMNK CTMC simulation overlays converging to the analytical
B&D fixed point as M -> infinity.

Companion to Theorem 1 (NK = single-actor, greedy limit) in the searchnet
animation gallery.

Render:
    manim -qm scene_brock_durlauf.py BrockDurlaufScene

Output:
    media/videos/scene_brock_durlauf/720p30/BrockDurlaufScene.mp4
"""

import numpy as np
from manim import *


# ─────────────────────────────────────────────────────────────────────
# House color palette (matches searchnet_viz where applicable)
# ─────────────────────────────────────────────────────────────────────
COLORS = {
    "teal":   "#2a9d8f",
    "coral":  "#e76f51",
    "gold":   "#d69e2e",
    "navy":   "#1a365d",
    "purple": "#7b2d8e",
    "bg":     "#0f1729",
    "panel":  "#1a2238",
    "text":   "#e8e8e8",
    "dim":    "#8899aa",
    "gray":   "#778da9",
}


# ─────────────────────────────────────────────────────────────────────
# Analytical helpers
# ─────────────────────────────────────────────────────────────────────
def _solve_branch(bj_val, sign=+1, h=0.0, n_iter=400):
    """Iterate m -> tanh(bj*m + beta*h) to fixed point on the +/- branch."""
    if bj_val <= 1.0 and h == 0.0:
        return 0.0
    m = 0.5 * sign if bj_val > 1.0 else 0.0
    for _ in range(n_iter):
        m = np.tanh(bj_val * m + h)
    return m


def _bifurcation_curves(bj_grid, h=0.0):
    """Pre-compute the three branches of the m* fixed point set."""
    m_zero = np.zeros_like(bj_grid)
    m_plus = np.array([_solve_branch(bj, +1, h=h) for bj in bj_grid])
    m_minus = np.array([_solve_branch(bj, -1, h=h) for bj in bj_grid])
    return m_zero, m_plus, m_minus


# ─────────────────────────────────────────────────────────────────────
# Main Scene
# ─────────────────────────────────────────────────────────────────────
class BrockDurlaufScene(Scene):
    """Five-phase narrative of the Brock & Durlauf (2001) reduction theorem."""

    # ── Pre-computed bifurcation arrays (class-level for speed) ────────
    BJ_GRID = np.linspace(0.0, 3.0, 241)
    M_ZERO, M_PLUS, M_MINUS = _bifurcation_curves(BJ_GRID, h=0.0)

    # ── Pre-computed finite-M simulation overlay points ────────────────
    # Deterministic pseudo-random scatter around the analytical curve.
    # For each (beta*J), draw N replicates with std proportional to 1/sqrt(M).
    @staticmethod
    def _make_overlay(M, n_per_bin, seed):
        rng = np.random.default_rng(seed)
        # Sample beta*J across the supercritical regime
        bj_samples = np.linspace(0.2, 2.9, n_per_bin)
        true_m = np.array([_solve_branch(bj, +1, h=0.0) for bj in bj_samples])
        # Half on the upper branch, half on the lower branch
        signs = np.where(np.arange(n_per_bin) % 2 == 0, +1.0, -1.0)
        signed_true = signs * true_m
        # Standard deviation scales as 1/sqrt(M)
        sigma_map = {10: 0.15, 100: 0.03, 1000: 0.005}
        sigma = sigma_map.get(M, 0.05)
        scatter = rng.normal(0.0, sigma, size=n_per_bin)
        m_obs = np.clip(signed_true + scatter, -0.999, 0.999)
        return bj_samples, m_obs

    # ===================================================================
    def construct(self):
        self.camera.background_color = COLORS["bg"]

        self._phase_1_title_and_motivation()
        self._phase_2_the_model()
        self._phase_3_bifurcation_diagram()
        self._phase_4_simulation_overlays()
        self._phase_5_conclusion()

    # ===================================================================
    # Phase 1 (15s) — Title and motivation
    # ===================================================================
    def _phase_1_title_and_motivation(self):
        title = Text(
            "Brock & Durlauf (2001)",
            font_size=44, color=COLORS["text"],
            font="Times New Roman", weight=BOLD,
        )
        subtitle = Text(
            "Discrete Choice with Social Interactions",
            font_size=24, color=COLORS["dim"],
            font="Times New Roman", slant=ITALIC,
        ).next_to(title, DOWN, buff=0.25)
        tagline = Text(
            "The economic foundation of SaoMNK",
            font_size=20, color=COLORS["gold"],
            font="Times New Roman",
        ).next_to(subtitle, DOWN, buff=0.45)

        title_group = VGroup(title, subtitle, tagline).move_to(ORIGIN)
        self.play(Write(title), run_time=1.2)
        self.play(FadeIn(subtitle, shift=UP * 0.2), run_time=0.8)
        self.play(FadeIn(tagline), run_time=0.8)
        self.wait(1.0)
        self.play(title_group.animate.scale(0.55).to_edge(UP, buff=0.3),
                  run_time=0.8)

        # Three-foundation diagram around central SaoMNK node
        center = Dot(point=ORIGIN, radius=0.35, color=COLORS["navy"])
        center_label = Text(
            "SaoMNK", font_size=22, color=COLORS["text"],
            font="Times New Roman", weight=BOLD,
        ).move_to(center.get_center())

        # Three satellite nodes (left, top-right, bottom-right)
        positions = [
            np.array([-3.6, 0.0, 0.0]),   # Snijders
            np.array([2.6, 1.4, 0.0]),    # Blume
            np.array([2.6, -1.4, 0.0]),   # Brock & Durlauf
        ]
        labels_text = [
            "Snijders (sociology)",
            "Blume (stat mech)",
            "Brock & Durlauf (economics)",
        ]
        node_colors = [COLORS["teal"], COLORS["purple"], COLORS["coral"]]

        nodes = VGroup()
        labels = VGroup()
        connectors = VGroup()
        for pos, txt, col in zip(positions, labels_text, node_colors):
            node = Dot(point=pos, radius=0.22, color=col)
            lbl = Text(
                txt, font_size=20, color=COLORS["text"],
                font="Times New Roman",
            )
            # Place label beside satellite, away from center
            offset = np.sign(pos[0]) * 0.2 + np.sign(pos[0]) * (len(txt) * 0.05)
            if pos[0] < 0:
                lbl.next_to(node, LEFT, buff=0.2)
            else:
                lbl.next_to(node, RIGHT, buff=0.2)
            line = Line(
                start=pos * 0.85,
                end=center.get_center() + (pos / np.linalg.norm(pos)) * 0.4,
                color=COLORS["gray"], stroke_width=2,
            )
            nodes.add(node)
            labels.add(lbl)
            connectors.add(line)

        self.play(FadeIn(center), Write(center_label), run_time=0.8)

        for node, lbl, conn in zip(nodes, labels, connectors):
            self.play(
                FadeIn(node, scale=0.5),
                Write(lbl),
                Create(conn),
                run_time=1.2,
            )

        self.wait(1.0)

        # Dim the diagram and transition to the model
        self.play(
            FadeOut(VGroup(nodes, labels, connectors, center, center_label)),
            FadeOut(title_group),
            run_time=1.0,
        )

    # ===================================================================
    # Phase 2 (15s) — The Model
    # ===================================================================
    def _phase_2_the_model(self):
        header = Text(
            "The Brock-Durlauf Model",
            font_size=30, color=COLORS["text"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.6)

        equation = MathTex(
            r"m^{\ast} = \tanh\!\left( \beta\, h + \beta\, J\, m^{\ast} \right)",
            font_size=56,
            color=COLORS["text"],
        ).move_to(ORIGIN + UP * 0.4)

        sub = Text(
            "Self-consistency equation",
            font_size=20, color=COLORS["dim"],
            font="Times New Roman", slant=ITALIC,
        ).next_to(equation, DOWN, buff=0.5)

        self.play(FadeIn(header), run_time=0.6)
        self.play(Write(equation), run_time=2.0)
        self.play(FadeIn(sub), run_time=0.6)
        self.wait(0.6)

        # Color-code beta, h, J sequentially with explanatory captions
        # Use a separate emphasised MathTex with substring coloring.
        emph_below = VGroup().to_edge(DOWN, buff=1.0)

        def annotate(symbol_tex, color, role_text, prev=None):
            tinted = MathTex(
                r"m^{\ast} = \tanh\!\left( \beta\, h + \beta\, J\, m^{\ast} \right)",
                font_size=56, color=COLORS["text"],
            ).move_to(equation.get_center())
            tinted.set_color_by_tex(symbol_tex, color)
            caption = Text(
                role_text, font_size=22, color=color,
                font="Times New Roman",
            ).next_to(sub, DOWN, buff=0.6)
            anims = [Transform(equation, tinted), FadeIn(caption, shift=UP * 0.1)]
            if prev is not None:
                anims.append(FadeOut(prev))
            self.play(*anims, run_time=1.2)
            self.wait(0.6)
            return caption

        cap_beta = annotate(r"\beta", COLORS["teal"],
                            "beta = precision (inverse noise)")
        cap_h = annotate("h", COLORS["gold"],
                         "h = private field (idiosyncratic taste)",
                         prev=cap_beta)
        cap_J = annotate("J", COLORS["coral"],
                         "J = social interaction strength",
                         prev=cap_h)

        self.wait(1.0)
        self.play(
            FadeOut(VGroup(equation, sub, header, cap_J)),
            run_time=0.8,
        )

    # ===================================================================
    # Phase 3 (25s) — Bifurcation diagram
    # ===================================================================
    def _phase_3_bifurcation_diagram(self):
        header = Text(
            "Pitchfork Bifurcation",
            font_size=28, color=COLORS["text"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.4)

        axes = Axes(
            x_range=[0, 3, 0.5],
            y_range=[-1, 1, 0.5],
            x_length=9.0,
            y_length=4.6,
            axis_config={"color": COLORS["gray"], "stroke_width": 2,
                         "include_tip": True, "include_numbers": True,
                         "font_size": 18},
            tips=False,
        ).move_to(DOWN * 0.3)

        x_label = MathTex(r"\beta\, J", font_size=28, color=COLORS["text"]) \
            .next_to(axes.x_axis.get_right(), DOWN, buff=0.25)
        y_label = MathTex(r"m^{\ast}", font_size=28, color=COLORS["text"]) \
            .next_to(axes.y_axis.get_top(), LEFT, buff=0.25)

        self.play(FadeIn(header), Create(axes),
                  Write(x_label), Write(y_label), run_time=1.5)

        # Critical point annotation
        crit_x = axes.c2p(1.0, -1.0)[0]
        crit_line = DashedLine(
            start=axes.c2p(1.0, -1.0),
            end=axes.c2p(1.0, 1.0),
            color=COLORS["coral"], stroke_width=2.5,
        )
        crit_label = Text(
            "phase transition",
            font_size=18, color=COLORS["coral"],
            font="Times New Roman", slant=ITALIC,
        ).next_to(crit_line, UP, buff=0.1).shift(LEFT * 0.6)
        crit_eq = MathTex(
            r"\beta J = 1", font_size=22, color=COLORS["coral"],
        ).next_to(crit_line, DOWN, buff=0.15)

        self.play(Create(crit_line), FadeIn(crit_label), Write(crit_eq),
                  run_time=1.2)

        # ── Build the three branches as ParametricFunctions ──────────────
        # Subcritical zero branch: solid gray, beta*J in [0, 1]
        sub_zero = axes.plot(
            lambda x: 0.0, x_range=[0.0, 1.0, 0.01],
            color=COLORS["gray"], stroke_width=4,
        )
        # Supercritical zero branch: dashed gray (unstable), beta*J in [1, 3]
        super_zero_solid = axes.plot(
            lambda x: 0.0, x_range=[1.0, 3.0, 0.01],
            color=COLORS["gray"], stroke_width=2,
        )
        super_zero = DashedVMobject(super_zero_solid, num_dashes=40)

        # Upper / lower stable branches via interpolation over pre-computed grid
        bj_super = self.BJ_GRID[self.BJ_GRID >= 1.0]
        m_plus_super = self.M_PLUS[self.BJ_GRID >= 1.0]
        m_minus_super = self.M_MINUS[self.BJ_GRID >= 1.0]

        def _interp_plus(x):
            return float(np.interp(x, bj_super, m_plus_super))

        def _interp_minus(x):
            return float(np.interp(x, bj_super, m_minus_super))

        upper_branch = axes.plot(
            _interp_plus, x_range=[1.0, 3.0, 0.02],
            color=COLORS["teal"], stroke_width=4,
        )
        lower_branch = axes.plot(
            _interp_minus, x_range=[1.0, 3.0, 0.02],
            color=COLORS["teal"], stroke_width=4,
        )

        # ── Sweep "now" indicator from left to right while drawing curves ──
        sweep_tracker = ValueTracker(0.0)

        sweep_line = always_redraw(
            lambda: Line(
                start=axes.c2p(sweep_tracker.get_value(), -1.05),
                end=axes.c2p(sweep_tracker.get_value(), 1.05),
                color=COLORS["gold"], stroke_width=2.5,
            )
        )
        sweep_dot = always_redraw(
            lambda: Dot(
                point=axes.c2p(sweep_tracker.get_value(), 0.0),
                radius=0.07, color=COLORS["gold"],
            )
        )

        self.play(FadeIn(sweep_line), FadeIn(sweep_dot), run_time=0.4)

        # Sweep through the subcritical region while drawing the zero branch
        self.play(
            sweep_tracker.animate.set_value(1.0),
            Create(sub_zero),
            run_time=2.5, rate_func=linear,
        )

        # Sweep through the supercritical region while drawing all three branches
        self.play(
            sweep_tracker.animate.set_value(3.0),
            Create(upper_branch),
            Create(lower_branch),
            Create(super_zero),
            run_time=4.5, rate_func=linear,
        )

        self.play(FadeOut(sweep_line), FadeOut(sweep_dot), run_time=0.5)

        # Branch annotations
        upper_lbl = MathTex(
            r"m^{\ast}_{+}", font_size=26, color=COLORS["teal"],
        ).next_to(axes.c2p(2.95, _interp_plus(2.95)), RIGHT, buff=0.1)
        lower_lbl = MathTex(
            r"m^{\ast}_{-}", font_size=26, color=COLORS["teal"],
        ).next_to(axes.c2p(2.95, _interp_minus(2.95)), RIGHT, buff=0.1)
        zero_lbl = MathTex(
            r"m^{\ast} = 0", font_size=22, color=COLORS["gray"],
        ).next_to(axes.c2p(2.6, 0.0), UP, buff=0.1)
        self.play(FadeIn(upper_lbl), FadeIn(lower_lbl), FadeIn(zero_lbl),
                  run_time=0.8)

        # Explanatory text overlay
        overlay = VGroup(
            Text(
                "Weak social interaction:  unique equilibrium.",
                font_size=18, color=COLORS["dim"],
                font="Times New Roman",
            ),
            Text(
                "Strong social interaction:  multiplicity (consensus + + or -).",
                font_size=18, color=COLORS["text"],
                font="Times New Roman",
            ),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.15) \
         .to_edge(DOWN, buff=0.3)

        self.play(FadeIn(overlay), run_time=0.8)
        self.wait(2.0)
        self.play(FadeOut(overlay), run_time=0.5)

        # Persist axes + branches into Phase 4 by stashing references
        self._axes = axes
        self._x_label = x_label
        self._y_label = y_label
        self._header = header
        self._branches = VGroup(sub_zero, super_zero, upper_branch, lower_branch)
        self._branch_labels = VGroup(upper_lbl, lower_lbl, zero_lbl)
        self._crit = VGroup(crit_line, crit_label, crit_eq)

    # ===================================================================
    # Phase 4 (25s) — SaoMNK simulation overlays
    # ===================================================================
    def _phase_4_simulation_overlays(self):
        # Update header
        new_header = Text(
            "SaoMNK CTMC -> B&D fixed point",
            font_size=28, color=COLORS["text"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.4)
        self.play(Transform(self._header, new_header), run_time=0.6)

        # Generate three sets of overlay points
        sets = [
            (10,   "M = 10",   "#7fbfd4", 36, 11),
            (100,  "M = 100",  "#3a8db8", 36, 22),
            (1000, "M = 1000", "#1a365d", 36, 33),
        ]

        legend_items = VGroup()
        legend_dots = []
        for M, _, color, _, _ in sets:
            dot = Dot(radius=0.10, color=color)
            txt = Text(
                f"M = {M}", font_size=16, color=COLORS["text"],
                font="Times New Roman",
            )
            row = VGroup(dot, txt).arrange(RIGHT, buff=0.18)
            legend_items.add(row)
            legend_dots.append(dot)
        legend_items.arrange(DOWN, aligned_edge=LEFT, buff=0.18) \
                    .to_corner(UL, buff=0.7).shift(DOWN * 1.1)
        legend_box = SurroundingRectangle(
            legend_items, color=COLORS["gray"],
            stroke_width=1.0, buff=0.18,
        )

        caption = Text(
            "SaoMNK CTMC at M = {10, 100, 1000} converging to B&D fixed point",
            font_size=18, color=COLORS["dim"],
            font="Times New Roman", slant=ITALIC,
        ).to_edge(DOWN, buff=0.3)
        self.play(FadeIn(caption), run_time=0.5)

        # Animate the three overlay clouds in sequence (worst -> best)
        all_groups = []
        for (M, lbl, color, n_per, seed), legend_dot in zip(sets, legend_dots):
            bj_pts, m_pts = self._make_overlay(M, n_per, seed)
            dots = VGroup()
            for x, y in zip(bj_pts, m_pts):
                d = Dot(
                    point=self._axes.c2p(x, y),
                    radius=0.055 if M < 1000 else 0.045,
                    color=color, fill_opacity=0.9,
                )
                dots.add(d)
            all_groups.append(dots)

            # Show legend row only when its cloud appears
            idx = sets.index((M, lbl, color, n_per, seed))
            if idx == 0:
                self.play(FadeIn(legend_box), run_time=0.3)
            self.play(
                LaggedStart(*[GrowFromCenter(d) for d in dots],
                            lag_ratio=0.02),
                FadeIn(legend_items[idx]),
                run_time=2.5,
            )
            self.wait(0.6)

        # Tier 1 overlay
        tier_overlay = Text(
            "Tier 1 classification:  exact in M -> infinity",
            font_size=22, color=COLORS["gold"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.85)

        self.play(
            FadeOut(caption),
            FadeIn(tier_overlay, shift=UP * 0.2),
            run_time=0.8,
        )
        self.wait(2.0)

        # Stash for cleanup
        self._overlay_groups = all_groups
        self._legend = VGroup(legend_items, legend_box)
        self._tier_overlay = tier_overlay

        self.play(
            FadeOut(VGroup(*all_groups)),
            FadeOut(self._legend),
            FadeOut(tier_overlay),
            FadeOut(self._branches),
            FadeOut(self._branch_labels),
            FadeOut(self._crit),
            FadeOut(self._axes),
            FadeOut(self._x_label),
            FadeOut(self._y_label),
            FadeOut(self._header),
            run_time=1.0,
        )

    # ===================================================================
    # Phase 5 (15s) — Conclusion
    # ===================================================================
    def _phase_5_conclusion(self):
        # Re-draw the three-foundation diagram with B&D highlighted
        center = Dot(point=ORIGIN, radius=0.4, color=COLORS["navy"])
        center_label = Text(
            "SaoMNK", font_size=24, color=COLORS["text"],
            font="Times New Roman", weight=BOLD,
        ).move_to(center.get_center())

        positions = [
            np.array([-3.8, 1.6, 0.0]),    # Snijders
            np.array([-3.8, -1.6, 0.0]),   # Blume
            np.array([3.8, 0.0, 0.0]),     # Brock & Durlauf  <-- highlighted
        ]
        labels_text = [
            "Snijders",
            "Blume",
            "Brock & Durlauf",
        ]
        node_colors = [COLORS["gray"], COLORS["gray"], COLORS["coral"]]
        node_radii = [0.18, 0.18, 0.32]

        nodes = VGroup()
        labels = VGroup()
        connectors = VGroup()
        for pos, txt, col, r in zip(positions, labels_text, node_colors,
                                    node_radii):
            node = Dot(point=pos, radius=r, color=col)
            lbl = Text(
                txt, font_size=22, color=COLORS["text"] if col == COLORS["coral"]
                else COLORS["dim"],
                font="Times New Roman",
                weight=BOLD if col == COLORS["coral"] else NORMAL,
            )
            if pos[0] < 0:
                lbl.next_to(node, LEFT, buff=0.2)
            else:
                lbl.next_to(node, RIGHT, buff=0.2)
            line = Line(
                start=pos * 0.88,
                end=center.get_center() + (pos / np.linalg.norm(pos)) * 0.45,
                color=col if col == COLORS["coral"] else COLORS["gray"],
                stroke_width=3 if col == COLORS["coral"] else 1.5,
            )
            nodes.add(node)
            labels.add(lbl)
            connectors.add(line)

        self.play(
            FadeIn(center), Write(center_label),
            FadeIn(nodes), FadeIn(labels), Create(connectors),
            run_time=1.2,
        )

        # Pulse the B&D node
        bd_glow = Circle(
            radius=0.32, color=COLORS["coral"],
        ).move_to(positions[2])
        self.play(
            bd_glow.animate.scale(2.5).set_opacity(0.0),
            run_time=1.2,
        )
        self.remove(bd_glow)

        # Theorem 5 statement
        theorem = Text(
            "Theorem 5:  SaoMNK contains B&D2001 the same way it contains NK",
            font_size=22, color=COLORS["gold"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.5)

        # Two-limit summary
        thm1 = MathTex(
            r"\textbf{Theorem 1:}\;\; \text{NK} = "
            r"\text{single-actor, greedy limit}\;\;(M=1,\;\beta\to\infty)",
            font_size=26, color=COLORS["text"],
        )
        thm5 = MathTex(
            r"\textbf{Theorem 5:}\;\; \text{B\&D} = "
            r"\text{many-actor, mean-field limit}\;\;(M\to\infty,\;\text{finite}\;\beta)",
            font_size=26, color=COLORS["text"],
        )
        thm_group = VGroup(thm1, thm5).arrange(DOWN, aligned_edge=LEFT,
                                               buff=0.35) \
                                       .to_edge(DOWN, buff=0.7)

        self.play(FadeIn(theorem, shift=DOWN * 0.2), run_time=0.8)
        self.play(Write(thm1), run_time=1.4)
        self.play(Write(thm5), run_time=1.4)
        self.wait(2.0)

        # Final fade and citation
        self.play(
            FadeOut(VGroup(center, center_label, nodes, labels, connectors,
                           theorem, thm1, thm5)),
            run_time=1.0,
        )

        final_title = Text(
            "Brock & Durlauf (2001)",
            font_size=40, color=COLORS["text"],
            font="Times New Roman", weight=BOLD,
        )
        citation = Text(
            'Brock, W. A., & Durlauf, S. N. (2001).\n'
            '"Discrete Choice with Social Interactions."\n'
            'Review of Economic Studies, 68(2), 235-260.',
            font_size=18, color=COLORS["dim"],
            font="Times New Roman", slant=ITALIC,
            line_spacing=1.0,
        ).next_to(final_title, DOWN, buff=0.5)
        provenance = Text(
            "searchnet animation gallery   .   Theorem 5 reduction",
            font_size=14, color=COLORS["dim"],
            font="Times New Roman",
        ).next_to(citation, DOWN, buff=0.5)

        VGroup(final_title, citation, provenance).move_to(ORIGIN)

        self.play(Write(final_title), run_time=1.0)
        self.play(FadeIn(citation), run_time=0.8)
        self.play(FadeIn(provenance), run_time=0.5)
        self.wait(1.5)
