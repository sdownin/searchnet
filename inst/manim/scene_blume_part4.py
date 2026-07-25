"""
Scene: Blume Tutorial Part 4 — Demonstrations
===============================================
Four scenes corresponding to the four runnable R demos in the
Blume Gibbs vignette:

  1. ConvergenceFromStartsScene  — 5 starting densities → 1 equilibrium
  2. TemperatureSweepScene       — Distribution sharpens as beta rises
  3. StationaryDistributionScene — Same stationary dist from opposite starts
  4. PhaseTransitionSweepScene   — 2x2 phase transition diagnostics

Render (individual):
    manim -qh scene_blume_part4.py ConvergenceFromStartsScene
    manim -qh scene_blume_part4.py TemperatureSweepScene
    manim -qh scene_blume_part4.py StationaryDistributionScene
    manim -qh scene_blume_part4.py PhaseTransitionSweepScene

Render (all):
    manim -qh scene_blume_part4.py
"""

import numpy as np
from manim import *
from searchnet_viz import SEARCHNET_COLORS, BaseSearchnetScene

C = SEARCHNET_COLORS


# ─────────────────────────────────────────────────────────────────────
# Helpers
# ─────────────────────────────────────────────────────────────────────

def _sigmoid(x):
    """Numerically stable sigmoid."""
    return np.where(x >= 0,
                    1.0 / (1.0 + np.exp(-x)),
                    np.exp(x) / (1.0 + np.exp(x)))


def _generate_convergence_trajectory(start_density, eq_density, n_steps,
                                     noise_scale=0.03, rng=None):
    """Simulate a density trajectory that converges from *start_density*
    toward *eq_density* with decaying noise."""
    if rng is None:
        rng = np.random.default_rng()
    traj = np.zeros(n_steps)
    traj[0] = start_density
    rate = 0.06
    for t in range(1, n_steps):
        gap = eq_density - traj[t - 1]
        noise = rng.normal(0, noise_scale * np.exp(-0.02 * t))
        traj[t] = traj[t - 1] + rate * gap + noise
        traj[t] = np.clip(traj[t], 0.0, 1.0)
    return traj


def _gibbs_probs(potentials, beta):
    """Gibbs probability vector for a set of potential values."""
    w = np.exp(beta * potentials)
    return w / w.sum()


def _ising_order_param(beta, beta_c=1.0):
    """Mean-field order parameter for 2D Ising-like model."""
    if beta <= beta_c:
        return 0.0
    return np.tanh(beta - beta_c) ** 0.6


# ─────────────────────────────────────────────────────────────────────
# Scene 1: Convergence From Multiple Starting Points
# ─────────────────────────────────────────────────────────────────────
class ConvergenceFromStartsScene(BaseSearchnetScene):
    """
    Five parallel lanes, each starting at a different network density.
    All converge to the same equilibrium, demonstrating ergodicity.
    """

    scene_title = "Convergence From Multiple Starts"
    scene_subtitle = "Five starting densities converge to one equilibrium"

    N_STEPS = 120
    EQ_DENSITY = 0.42

    START_DENSITIES = [0.05, 0.25, 0.50, 0.75, 0.95]
    LANE_COLORS = ["#2166ac", "#67a9cf", "#d1e5f0", "#ef8a62", "#b2182b"]

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993) — Demo 1: Convergence")

        rng = np.random.default_rng(42)

        # ── Generate trajectories ──────────────────────────────────
        trajectories = []
        for sd in self.START_DENSITIES:
            traj = _generate_convergence_trajectory(
                sd, self.EQ_DENSITY, self.N_STEPS, rng=rng
            )
            trajectories.append(traj)

        # ── Axes ───────────────────────────────────────────────────
        ax = Axes(
            x_range=[0, self.N_STEPS, 20],
            y_range=[0, 1.0, 0.2],
            x_length=10, y_length=5,
            axis_config={"color": C["text_dim"], "stroke_width": 1.2},
            tips=False,
        ).shift(DOWN * 0.2)

        x_label = Text("Sweep", font_size=16, color=C["text_dim"],
                        font="Times New Roman").next_to(ax, DOWN, buff=0.25)
        y_label = Text("Density", font_size=16, color=C["text_dim"],
                        font="Times New Roman").rotate(PI / 2).next_to(ax, LEFT, buff=0.3)

        # Y-axis tick labels
        y_ticks = VGroup()
        for v in [0.0, 0.2, 0.4, 0.6, 0.8, 1.0]:
            lbl = Text(f"{v:.1f}", font_size=11, color=C["text_dim"],
                       font="Times New Roman")
            lbl.next_to(ax.c2p(0, v), LEFT, buff=0.15)
            y_ticks.add(lbl)

        self.play(Create(ax), FadeIn(x_label), FadeIn(y_label),
                  FadeIn(y_ticks), run_time=0.8)

        # ── Equilibrium line ───────────────────────────────────────
        eq_line = DashedLine(
            ax.c2p(0, self.EQ_DENSITY),
            ax.c2p(self.N_STEPS, self.EQ_DENSITY),
            color=C["teal"], stroke_width=1.5, dash_length=0.1,
        )
        eq_label = MathTex(
            r"\pi^*",
            font_size=22, color=C["teal"],
        ).next_to(eq_line, RIGHT, buff=0.15)
        self.play(Create(eq_line), FadeIn(eq_label), run_time=0.5)

        # ── Starting-point labels on left ──────────────────────────
        start_labels = VGroup()
        for i, sd in enumerate(self.START_DENSITIES):
            lbl = Text(f"{sd:.0%}", font_size=13,
                       color=self.LANE_COLORS[i],
                       font="Times New Roman")
            lbl.next_to(ax.c2p(0, sd), LEFT, buff=0.4)
            start_labels.add(lbl)
        self.play(FadeIn(start_labels), run_time=0.4)

        # ── Draw trajectories progressively ────────────────────────
        lines = []
        dots = []
        for i in range(5):
            line = VMobject(stroke_color=self.LANE_COLORS[i],
                            stroke_width=2.5, stroke_opacity=0.85)
            line.set_points_as_corners([ax.c2p(0, trajectories[i][0])])
            self.add(line)
            lines.append(line)

            dot = Dot(
                ax.c2p(0, trajectories[i][0]),
                radius=0.07, color=self.LANE_COLORS[i],
            )
            self.add(dot)
            dots.append(dot)

        # Animate in batches
        batch_size = 4
        n_batches = self.N_STEPS // batch_size
        for b in range(n_batches):
            step = (b + 1) * batch_size
            anims = []
            for i in range(5):
                # Build new line up to current step
                pts = [ax.c2p(t, trajectories[i][t])
                       for t in range(0, min(step + 1, self.N_STEPS))]
                new_line = VMobject(stroke_color=self.LANE_COLORS[i],
                                   stroke_width=2.5, stroke_opacity=0.85)
                new_line.set_points_as_corners(pts)
                anims.append(Transform(lines[i], new_line))

                new_pos = ax.c2p(step, trajectories[i][min(step, self.N_STEPS - 1)])
                anims.append(dots[i].animate.move_to(new_pos))

            self.play(*anims, run_time=0.12)

        # ── Final cluster highlight ────────────────────────────────
        self.wait(0.5)

        # Draw a bracket around the converged dots
        final_ys = [trajectories[i][-1] for i in range(5)]
        y_min, y_max = min(final_ys), max(final_ys)
        bracket_top = ax.c2p(self.N_STEPS + 3, y_max)
        bracket_bot = ax.c2p(self.N_STEPS + 3, y_min)

        brace = Brace(
            VGroup(
                Dot(ax.c2p(self.N_STEPS, y_min), radius=0.01),
                Dot(ax.c2p(self.N_STEPS, y_max), radius=0.01),
            ),
            direction=RIGHT,
            color=C["amber"],
        )
        brace_label = MathTex(
            r"\approx \pi^*",
            font_size=20, color=C["amber"],
        ).next_to(brace, RIGHT, buff=0.1)

        self.play(GrowFromCenter(brace), FadeIn(brace_label), run_time=0.6)

        # ── Bottom label ───────────────────────────────────────────
        bottom_label = Text(
            "5 starting points \u2192 1 equilibrium",
            font_size=22, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.25)
        self.play(FadeIn(bottom_label), run_time=0.5)
        self.wait(2.5)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 2: Temperature Sweep
# ─────────────────────────────────────────────────────────────────────
class TemperatureSweepScene(BaseSearchnetScene):
    """
    Left panel: 5 bar charts showing the Gibbs distribution at
    increasing beta values.  Right panel: variance vs beta line plot.
    """

    scene_title = "Temperature Sweep"
    scene_subtitle = "Higher beta sharpens the Gibbs distribution"

    BETAS = [0.2, 0.5, 1.0, 2.0, 4.0]
    N_CONFIGS = 8

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993) — Demo 2: Temperature sweep")

        # ── Synthetic potential landscape ──────────────────────────
        potentials = np.array([0.3, 0.6, 1.2, 0.4, 0.9, 1.8, 0.5, 0.8])

        # ── Persistent section title ───────────────────────────────
        title = Text(
            "Gibbs Distribution at Varying \u03b2",
            font_size=24, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.25)
        self.play(FadeIn(title), run_time=0.4)

        # ── LEFT PANEL: 5 small bar charts stacked vertically ──────
        left_group = VGroup()
        bar_chart_groups = []

        chart_height = 0.7
        chart_width = 3.5
        v_spacing = 1.05

        for idx, beta in enumerate(self.BETAS):
            probs = _gibbs_probs(potentials, beta)
            chart_g = VGroup()

            # Beta label
            beta_lbl = MathTex(
                r"\beta=" + f"{beta:.1f}",
                font_size=16, color=C["amber"],
            )

            # Bars
            bar_group = VGroup()
            max_p = max(probs.max(), 0.01)
            for j in range(self.N_CONFIGS):
                bar_h = max((probs[j] / max_p) * chart_height, 0.01)
                bar = Rectangle(
                    width=chart_width / (self.N_CONFIGS + 1),
                    height=bar_h,
                    fill_color=interpolate_color(
                        ManimColor(C["slate"]),
                        ManimColor(C["teal"]),
                        probs[j] / max_p,
                    ),
                    fill_opacity=0.85,
                    stroke_width=0.5,
                    stroke_color=C["text_dim"],
                )
                bar.shift(RIGHT * j * (chart_width / self.N_CONFIGS))
                bar_group.add(bar)

            # Align bars to a common baseline
            for bar in bar_group:
                bar.align_to(bar_group[0], DOWN)
            bar_group.move_to(ORIGIN)

            beta_lbl.next_to(bar_group, LEFT, buff=0.2)
            chart_g.add(beta_lbl, bar_group)
            bar_chart_groups.append(chart_g)
            left_group.add(chart_g)

        left_group.arrange(DOWN, buff=0.15, aligned_edge=LEFT)
        left_group.shift(LEFT * 3.0 + DOWN * 0.2)

        # ── RIGHT PANEL: Variance vs beta ──────────────────────────
        beta_range = np.linspace(0.1, 5.0, 50)
        variances = []
        for b in beta_range:
            p = _gibbs_probs(potentials, b)
            variances.append(np.var(p))
        variances = np.array(variances)

        right_ax = Axes(
            x_range=[0, 5.5, 1],
            y_range=[0, max(variances) * 1.15, 0.02],
            x_length=4.5, y_length=3.8,
            axis_config={"color": C["text_dim"], "stroke_width": 1.2},
            tips=False,
        ).shift(RIGHT * 3.2 + DOWN * 0.2)

        rx_label = MathTex(r"\beta", font_size=18, color=C["text_dim"]
                           ).next_to(right_ax, DOWN, buff=0.2)
        ry_label = Text("Spread (variance)", font_size=13,
                        color=C["text_dim"], font="Times New Roman"
                        ).rotate(PI / 2).next_to(right_ax, LEFT, buff=0.2)
        right_title = Text("Distribution Spread vs. \u03b2",
                           font_size=16, color=C["text_dim"],
                           font="Times New Roman"
                           ).next_to(right_ax, UP, buff=0.1)

        # Full variance curve (draw incrementally later)
        var_points = [right_ax.c2p(beta_range[i], variances[i])
                      for i in range(len(beta_range))]
        var_line = VMobject(stroke_color=C["highlight"], stroke_width=2.5)
        var_line.set_points_smoothly(var_points)

        # ── Animate: bar charts appear in sequence, variance grows ─
        self.play(Create(right_ax), FadeIn(rx_label), FadeIn(ry_label),
                  FadeIn(right_title), run_time=0.6)

        # Markers on variance plot for each beta value
        var_dots = VGroup()
        for beta in self.BETAS:
            p = _gibbs_probs(potentials, beta)
            v = np.var(p)
            dot = Dot(right_ax.c2p(beta, v), radius=0.06, color=C["coral"])
            var_dots.add(dot)

        for idx, (chart_g, beta) in enumerate(zip(bar_chart_groups, self.BETAS)):
            self.play(FadeIn(chart_g), run_time=0.6)

            # Show partial variance curve up to this beta
            partial_idx = int(np.searchsorted(beta_range, beta))
            if partial_idx > 1:
                partial_pts = var_points[:partial_idx + 1]
                partial_line = VMobject(stroke_color=C["highlight"],
                                       stroke_width=2.5)
                partial_line.set_points_smoothly(partial_pts)
                self.play(Create(partial_line), FadeIn(var_dots[idx]),
                          run_time=0.4)
            else:
                self.play(FadeIn(var_dots[idx]), run_time=0.3)

            self.wait(0.3)

        # Complete the variance curve
        self.play(Create(var_line), run_time=0.6)

        # ── Declining arrow annotation ─────────────────────────────
        arrow = Arrow(
            right_ax.c2p(1.0, variances[10] * 0.9),
            right_ax.c2p(4.0, variances[40] * 1.3),
            color=C["coral"], stroke_width=2, buff=0.1,
        )
        arrow_label = Text("Sharpening", font_size=13, color=C["coral"],
                           font="Times New Roman"
                           ).next_to(arrow, UP, buff=0.05)
        self.play(GrowArrow(arrow), FadeIn(arrow_label), run_time=0.5)

        # ── Bottom label ───────────────────────────────────────────
        bottom_label = Text(
            "Higher \u03b2 \u2192 Sharper Gibbs distribution",
            font_size=22, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.2)
        self.play(FadeIn(bottom_label), run_time=0.5)
        self.wait(2.5)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 3: Stationary Distribution From Opposite Starts
# ─────────────────────────────────────────────────────────────────────
class StationaryDistributionScene(BaseSearchnetScene):
    """
    Two bar charts side by side: distributions from sparse (5%) and
    dense (95%) starts. Both converge to the same stationary distribution.
    """

    scene_title = "Unique Stationary Distribution"
    scene_subtitle = "Blume's Theorem: convergence from opposite starts"

    N_BINS = 12

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993) — Demo 3: Stationarity")

        rng = np.random.default_rng(42)

        # ── Generate synthetic tie-count distributions ─────────────
        # True stationary distribution (mixture of two betas for realism)
        true_mean = 5.0
        true_std = 1.8
        n_samples = 800

        # From sparse: slightly noisier early on, same asymptotic shape
        sparse_samples = rng.normal(true_mean, true_std, n_samples)
        sparse_samples += rng.normal(0, 0.3, n_samples)  # slight extra noise
        sparse_samples = np.clip(sparse_samples, 0, self.N_BINS - 1)

        # From dense: slightly shifted noise, same asymptotic shape
        dense_samples = rng.normal(true_mean, true_std, n_samples)
        dense_samples += rng.normal(0, 0.25, n_samples)
        dense_samples = np.clip(dense_samples, 0, self.N_BINS - 1)

        # Compute histograms
        bins = np.arange(self.N_BINS + 1) - 0.5
        sparse_hist, _ = np.histogram(sparse_samples, bins=bins, density=True)
        dense_hist, _ = np.histogram(dense_samples, bins=bins, density=True)

        # ── Section title ──────────────────────────────────────────
        title = Text(
            "Stationary Distribution: Sparse vs. Dense Start",
            font_size=22, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.25)
        self.play(FadeIn(title), run_time=0.4)

        # ── Create paired bar charts ───────────────────────────────
        chart_width = 5.0
        chart_height = 3.2
        bar_w = chart_width / (self.N_BINS + 1)
        max_h = max(sparse_hist.max(), dense_hist.max())

        def make_bar_chart(hist, x_offset, color, label_text):
            """Build a bar chart VGroup at the given x offset."""
            group = VGroup()
            bars = VGroup()
            for j in range(self.N_BINS):
                h = max((hist[j] / max_h) * chart_height, 0.01)
                bar = Rectangle(
                    width=bar_w * 0.85,
                    height=h,
                    fill_color=color,
                    fill_opacity=0.8,
                    stroke_width=0.5,
                    stroke_color=C["text_dim"],
                )
                bar.move_to([
                    x_offset + (j - self.N_BINS / 2 + 0.5) * bar_w,
                    -1.0 + h / 2,
                    0,
                ])
                bars.add(bar)

            # Baseline
            baseline = Line(
                [x_offset - chart_width / 2, -1.0, 0],
                [x_offset + chart_width / 2, -1.0, 0],
                color=C["text_dim"], stroke_width=1.2,
            )

            # Label
            label = Text(
                label_text, font_size=16, color=color,
                font="Times New Roman", weight=BOLD,
            ).move_to([x_offset, -1.0 - 0.4, 0])

            # Tie-count axis label
            ax_label = Text("Tie count", font_size=12,
                           color=C["text_dim"],
                           font="Times New Roman"
                           ).move_to([x_offset, -1.0 - 0.7, 0])

            group.add(bars, baseline, label, ax_label)
            return group, bars

        sparse_group, sparse_bars = make_bar_chart(
            sparse_hist, -3.2, C["teal"], "From Sparse (5%)"
        )
        dense_group, dense_bars = make_bar_chart(
            dense_hist, 3.2, C["coral"], "From Dense (95%)"
        )

        # ── Animate bars growing ───────────────────────────────────
        # Start bars at zero height and grow them
        for bars in [sparse_bars, dense_bars]:
            for bar in bars:
                bar.save_state()
                bar.stretch(0.001, 1, about_edge=DOWN)

        # Show baselines and labels first
        sparse_static = VGroup(sparse_group[1], sparse_group[2], sparse_group[3])
        dense_static = VGroup(dense_group[1], dense_group[2], dense_group[3])
        self.play(FadeIn(sparse_static), FadeIn(dense_static), run_time=0.5)

        # Grow bars simultaneously
        sparse_anims = [bar.animate.restore() for bar in sparse_bars]
        dense_anims = [bar.animate.restore() for bar in dense_bars]
        self.play(*sparse_anims, *dense_anims, run_time=2.0)
        self.add(sparse_group, dense_group)

        self.wait(0.8)

        # ── Highlight matching shapes ──────────────────────────────
        # Draw a connecting bracket
        match_rect = SurroundingRectangle(
            VGroup(sparse_bars, dense_bars),
            color=C["amber"], stroke_width=2, buff=0.2,
            corner_radius=0.1,
        )
        match_label = Text(
            "Same shape",
            font_size=18, color=C["amber"],
            font="Times New Roman", weight=BOLD,
        ).next_to(match_rect, UP, buff=0.1)

        self.play(Create(match_rect), FadeIn(match_label), run_time=0.6)
        self.wait(0.5)

        # ── KS distance ───────────────────────────────────────────
        # Compute actual KS distance between the two histograms
        sparse_cdf = np.cumsum(sparse_hist) / sparse_hist.sum()
        dense_cdf = np.cumsum(dense_hist) / dense_hist.sum()
        ks_stat = np.max(np.abs(sparse_cdf - dense_cdf))

        ks_text = MathTex(
            r"\text{KS} = " + f"{ks_stat:.3f}",
            font_size=28, color=C["highlight"],
        ).shift(UP * 2.8 + RIGHT * 0.0)
        self.play(FadeIn(ks_text), run_time=0.5)

        # ── Bottom label ───────────────────────────────────────────
        bottom_label = Text(
            "Blume's Theorem: Unique stationary distribution",
            font_size=22, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.2)
        self.play(FadeIn(bottom_label), run_time=0.5)

        self.wait(2.5)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 4: Phase Transition Sweep (2x2 grid)
# ─────────────────────────────────────────────────────────────────────
class PhaseTransitionSweepScene(BaseSearchnetScene):
    """
    2x2 grid of animated plots showing how network observables change
    as beta sweeps from low to high, with a critical point flash.
    """

    scene_title = "Phase Transition Sweep"
    scene_subtitle = "Randomness gives way to order at the critical point"

    BETA_RANGE = np.linspace(0.1, 4.0, 80)
    BETA_CRIT = 1.0

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993) — Demo 4: Phase transition")

        rng = np.random.default_rng(42)

        # ── Generate synthetic observables vs beta ─────────────────
        betas = self.BETA_RANGE
        n = len(betas)

        # K_CC: order parameter — near zero below critical, rises above
        k_cc = np.array([_ising_order_param(b, self.BETA_CRIT) for b in betas])
        k_cc += rng.normal(0, 0.015, n)
        k_cc = np.clip(k_cc, 0, 1)

        # Density: rises from ~0.3 to ~0.7 with a sigmoid around beta_c
        density = 0.3 + 0.4 * _sigmoid(2.5 * (betas - self.BETA_CRIT))
        density += rng.normal(0, 0.012, n)
        density = np.clip(density, 0, 1)

        # Scope heterogeneity: decreases as system orders
        scope_het = 0.8 - 0.5 * _sigmoid(2.0 * (betas - self.BETA_CRIT))
        scope_het += rng.normal(0, 0.015, n)
        scope_het = np.clip(scope_het, 0, 1)

        # Phase diagram: density vs K_CC (parametric, colored by beta)
        # (computed from the arrays above)

        # ── Section title ──────────────────────────────────────────
        title = Text(
            "Phase Transition: Four Diagnostics",
            font_size=24, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.2)
        self.play(FadeIn(title), run_time=0.4)

        # ── Build 2x2 grid of axes ─────────────────────────────────
        ax_w, ax_h = 4.8, 2.2
        panels = {}
        panel_configs = {
            "tl": {
                "pos": LEFT * 3.0 + UP * 0.8,
                "y_label": r"K_{CC}",
                "title": "Order parameter",
                "data": k_cc,
                "color": C["coral"],
            },
            "tr": {
                "pos": RIGHT * 3.0 + UP * 0.8,
                "y_label": "Density",
                "title": "Network density",
                "data": density,
                "color": C["teal"],
            },
            "bl": {
                "pos": LEFT * 3.0 + DOWN * 2.0,
                "y_label": "Heterogeneity",
                "title": "Scope heterogeneity",
                "data": scope_het,
                "color": C["amber"],
            },
        }

        create_anims = []

        for key, cfg in panel_configs.items():
            ax = Axes(
                x_range=[0, 4.5, 1],
                y_range=[0, 1.05, 0.25],
                x_length=ax_w, y_length=ax_h,
                axis_config={"color": C["text_dim"], "stroke_width": 1.0},
                tips=False,
            ).move_to(cfg["pos"])

            panel_title = Text(
                cfg["title"], font_size=13, color=C["text_dim"],
                font="Times New Roman",
            ).next_to(ax, UP, buff=0.05)

            x_lbl = MathTex(r"\beta", font_size=14, color=C["text_dim"]
                            ).next_to(ax, DOWN, buff=0.08)

            panels[key] = {
                "ax": ax, "title": panel_title, "x_lbl": x_lbl,
                "data": cfg["data"], "color": cfg["color"],
            }
            create_anims.extend([Create(ax), FadeIn(panel_title), FadeIn(x_lbl)])

        # ── Bottom-right: phase diagram (density vs K_CC) ──────────
        ax_pd = Axes(
            x_range=[0, 1.05, 0.25],
            y_range=[0, 1.05, 0.25],
            x_length=ax_w, y_length=ax_h,
            axis_config={"color": C["text_dim"], "stroke_width": 1.0},
            tips=False,
        ).move_to(RIGHT * 3.0 + DOWN * 2.0)

        pd_title = Text("Phase diagram", font_size=13,
                        color=C["text_dim"], font="Times New Roman"
                        ).next_to(ax_pd, UP, buff=0.05)
        pd_x_lbl = MathTex(r"K_{CC}", font_size=14, color=C["text_dim"]
                           ).next_to(ax_pd, DOWN, buff=0.08)
        pd_y_lbl = Text("Density", font_size=11, color=C["text_dim"],
                        font="Times New Roman"
                        ).rotate(PI / 2).next_to(ax_pd, LEFT, buff=0.08)

        create_anims.extend([Create(ax_pd), FadeIn(pd_title),
                             FadeIn(pd_x_lbl), FadeIn(pd_y_lbl)])

        self.play(*create_anims, run_time=0.8)

        # ── Critical beta line (vertical dashed) on 3 time-series ──
        crit_lines = VGroup()
        for key in ["tl", "tr", "bl"]:
            ax = panels[key]["ax"]
            cl = DashedLine(
                ax.c2p(self.BETA_CRIT, 0),
                ax.c2p(self.BETA_CRIT, 1.05),
                color=C["plum"], stroke_width=1.2, dash_length=0.06,
            )
            crit_lines.add(cl)
        self.play(Create(crit_lines), run_time=0.4)

        # ── Animate cursor sweep across beta ───────────────────────
        # We draw growing lines + a cursor dot on each panel
        # plus a growing scatter on the phase diagram

        cursor_dots = {}
        growing_lines = {}
        for key in ["tl", "tr", "bl"]:
            ax = panels[key]["ax"]
            d = panels[key]["data"]
            col = panels[key]["color"]
            dot = Dot(ax.c2p(betas[0], d[0]), radius=0.05, color=col)
            self.add(dot)
            cursor_dots[key] = dot
            line = VMobject(stroke_color=col, stroke_width=2.2)
            line.set_points_as_corners([ax.c2p(betas[0], d[0])])
            self.add(line)
            growing_lines[key] = line

        # Phase diagram dot
        pd_dot = Dot(ax_pd.c2p(k_cc[0], density[0]),
                     radius=0.05, color=C["highlight"])
        self.add(pd_dot)
        pd_trail = VGroup()

        # Sweep in chunks
        chunk = 3
        n_chunks = n // chunk
        flashed = False

        for ci in range(n_chunks):
            idx = min((ci + 1) * chunk, n - 1)
            anims = []

            for key in ["tl", "tr", "bl"]:
                ax = panels[key]["ax"]
                d = panels[key]["data"]
                col = panels[key]["color"]

                pts = [ax.c2p(betas[t], d[t]) for t in range(idx + 1)]
                new_line = VMobject(stroke_color=col, stroke_width=2.2)
                new_line.set_points_as_corners(pts)
                anims.append(Transform(growing_lines[key], new_line))
                anims.append(
                    cursor_dots[key].animate.move_to(ax.c2p(betas[idx], d[idx]))
                )

            # Phase diagram: add a small dot for the trail
            trail_dot = Dot(
                ax_pd.c2p(k_cc[idx], density[idx]),
                radius=0.03,
                color=interpolate_color(
                    ManimColor(C["slate"]),
                    ManimColor(C["coral"]),
                    betas[idx] / betas[-1],
                ),
                fill_opacity=0.7,
            )
            pd_trail.add(trail_dot)
            anims.append(FadeIn(trail_dot, run_time=0.05))
            anims.append(
                pd_dot.animate.move_to(ax_pd.c2p(k_cc[idx], density[idx]))
            )

            rt = 0.15
            # Flash at critical beta
            if not flashed and betas[idx] >= self.BETA_CRIT:
                flashed = True
                rt = 0.4
                # Create a flash effect using an expanding circle
                for key in ["tl", "tr", "bl"]:
                    ax = panels[key]["ax"]
                    d = panels[key]["data"]
                    flash_circ = Circle(
                        radius=0.02, color=C["plum"],
                        fill_opacity=0.6, stroke_width=2,
                    ).move_to(ax.c2p(betas[idx], d[idx]))
                    anims.append(flash_circ.animate.scale(8).set_opacity(0))

            self.play(*anims, run_time=rt)

        # ── Beta labels on phase diagram trail ─────────────────────
        for b_val in [0.5, 1.0, 2.0, 3.5]:
            b_idx = np.argmin(np.abs(betas - b_val))
            lbl = MathTex(
                f"{b_val:.1f}",
                font_size=11, color=C["text_dim"],
            ).next_to(ax_pd.c2p(k_cc[b_idx], density[b_idx]), UR, buff=0.06)
            self.play(FadeIn(lbl), run_time=0.15)

        # ── Critical-point annotation ──────────────────────────────
        crit_note = MathTex(
            r"\beta^* \approx 1.0",
            font_size=22, color=C["plum"],
        ).move_to(UP * 0.8 + LEFT * 0.3)
        crit_box = SurroundingRectangle(
            crit_note, color=C["plum"], stroke_width=1.5,
            buff=0.1, corner_radius=0.05,
        )
        self.play(FadeIn(crit_note), Create(crit_box), run_time=0.5)

        # ── Bottom label ───────────────────────────────────────────
        bottom_label = Text(
            "Phase transition: randomness \u2192 order",
            font_size=22, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.15)
        self.play(FadeIn(bottom_label), run_time=0.5)

        self.wait(3)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)
