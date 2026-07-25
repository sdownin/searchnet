"""
Scene: Blume's Logit-Gibbs Bridge
===================================
Four-scene manim animation illustrating Blume's (1993) foundational result:
logit best-response dynamics in potential games converge to the Gibbs measure.

Scenes:
  1. IsingLogitScene     — Agents on a grid making logit best-response moves
  2. TemperatureDialScene — Temperature dial showing equilibrium vs beta
  3. PotentialGibbsScene  — Potential surface with Gibbs probability cloud
  4. ConvergenceScene     — Random start → gradual organization

Render (individual):
    manim -qh scene_blume_gibbs.py IsingLogitScene
    manim -qh scene_blume_gibbs.py TemperatureDialScene
    manim -qh scene_blume_gibbs.py PotentialGibbsScene
    manim -qh scene_blume_gibbs.py ConvergenceScene

Render (all):
    manim -qh scene_blume_gibbs.py
"""

import numpy as np
from manim import *
from searchnet_viz import SEARCHNET_COLORS, BaseSearchnetScene

C = SEARCHNET_COLORS


# ─────────────────────────────────────────────────────────────────────
# Helper: Simple 2D Ising-like potential game on a grid
# ─────────────────────────────────────────────────────────────────────
def ising_potential(grid):
    """Compute potential (negative energy) for a binary grid.
    Phi = sum of matching-neighbor pairs.  Higher = more ordered."""
    rows, cols = grid.shape
    phi = 0.0
    for r in range(rows):
        for c in range(cols):
            if c + 1 < cols and grid[r, c] == grid[r, c + 1]:
                phi += 1.0
            if r + 1 < rows and grid[r, c] == grid[r + 1, c]:
                phi += 1.0
    return phi


def logit_flip_prob(delta_u, beta):
    """Logit probability of accepting a flip with utility change delta_u."""
    return np.exp(beta * delta_u) / (1.0 + np.exp(beta * delta_u))


def ising_step(grid, beta, rng):
    """One logit-best-response step: pick random cell, flip with logit prob."""
    rows, cols = grid.shape
    r, c = rng.integers(rows), rng.integers(cols)
    # Compute delta in potential from flipping (r, c)
    current = grid[r, c]
    neighbor_sum = 0
    for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        nr, nc = r + dr, c + dc
        if 0 <= nr < rows and 0 <= nc < cols:
            neighbor_sum += (1 if grid[nr, nc] == current else -1)
    # Flipping changes potential by -2 * neighbor_sum (matching pairs lost/gained)
    delta_phi = -2.0 * neighbor_sum
    p_flip = logit_flip_prob(delta_phi, beta)
    if rng.random() < p_flip:
        grid[r, c] = 1 - current
    return grid, (r, c)


# ─────────────────────────────────────────────────────────────────────
# Scene 1: Ising Logit Dynamics on a Grid
# ─────────────────────────────────────────────────────────────────────
class IsingLogitScene(BaseSearchnetScene):
    """
    Agents on a grid making logit best-response moves.
    Colored squares = two strategies. Watch clusters form.
    """

    scene_title = "Blume's Logit Dynamics"
    scene_subtitle = "Agents making noisy best-response decisions on a lattice"

    GRID_SIZE = 8
    BETA = 1.5
    N_STEPS = 200
    CELL_SIZE = 0.55

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993), Games and Economic Behavior")

        rng = np.random.default_rng(42)
        grid = rng.integers(0, 2, size=(self.GRID_SIZE, self.GRID_SIZE))

        # ── Persistent title ───────────────────────────────────────
        title = Text(
            "Logit Best-Response on a Lattice",
            font_size=26, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.3)
        self.play(FadeIn(title), run_time=0.4)

        # ── Beta label ─────────────────────────────────────────────
        beta_label = MathTex(
            r"\beta = " + f"{self.BETA:.1f}",
            font_size=28, color=C["amber"],
        ).next_to(title, DOWN, buff=0.15)
        self.play(FadeIn(beta_label), run_time=0.3)

        # ── Build grid of squares ──────────────────────────────────
        colors = [C["teal"], C["coral"]]
        cells = {}
        grid_group = VGroup()

        for r in range(self.GRID_SIZE):
            for cc in range(self.GRID_SIZE):
                sq = Square(
                    side_length=self.CELL_SIZE,
                    fill_opacity=0.85,
                    stroke_width=0.5,
                    stroke_color=C["bg_panel"],
                    fill_color=colors[grid[r, cc]],
                )
                x_pos = (cc - self.GRID_SIZE / 2 + 0.5) * self.CELL_SIZE
                y_pos = (self.GRID_SIZE / 2 - 0.5 - r) * self.CELL_SIZE - 0.5
                sq.move_to([x_pos, y_pos, 0])
                cells[(r, cc)] = sq
                grid_group.add(sq)

        self.play(FadeIn(grid_group), run_time=0.8)
        self.wait(0.5)

        # ── Animate logit dynamics ─────────────────────────────────
        # Show flips in batches for visual clarity
        batch_size = 10
        n_batches = self.N_STEPS // batch_size

        for batch in range(n_batches):
            anims = []
            for _ in range(batch_size):
                grid, (r, cc) = ising_step(grid, self.BETA, rng)
                new_color = colors[grid[r, cc]]
                anims.append(
                    cells[(r, cc)].animate.set_fill(new_color)
                )
            self.play(*anims, run_time=0.25)

        # ── Final state ────────────────────────────────────────────
        phi_final = ising_potential(grid)
        phi_max = 2 * self.GRID_SIZE * (self.GRID_SIZE - 1)
        order_pct = phi_final / phi_max * 100

        result_text = Text(
            f"Order: {order_pct:.0f}% of maximum",
            font_size=20, color=C["text_dim"],
            font="Times New Roman",
        ).to_edge(DOWN, buff=0.5)
        self.play(FadeIn(result_text), run_time=0.5)
        self.wait(2)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 2: Temperature Dial
# ─────────────────────────────────────────────────────────────────────
class TemperatureDialScene(BaseSearchnetScene):
    """
    Interactive temperature dial showing how the Gibbs distribution
    changes with beta (rationality / inverse temperature).
    """

    scene_title = "The Temperature Dial"
    scene_subtitle = "How rationality shapes the Gibbs distribution"

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993), Games and Economic Behavior")

        # ── Section title ──────────────────────────────────────────
        title = Text(
            "Gibbs Distribution vs. Rationality",
            font_size=26, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.3)
        self.play(FadeIn(title), run_time=0.4)

        # ── Create a toy potential landscape (bar chart of 8 states)
        n_states = 8
        rng = np.random.default_rng(42)
        potentials = np.array([0.2, 0.5, 1.0, 0.3, 0.8, 1.5, 0.4, 0.7])
        state_labels = [f"x{i+1}" for i in range(n_states)]

        # ── Axes ───────────────────────────────────────────────────
        ax = Axes(
            x_range=[0, n_states + 1, 1],
            y_range=[0, 0.55, 0.1],
            x_length=8, y_length=4,
            axis_config={"color": C["text_dim"], "stroke_width": 1.5},
            tips=False,
        ).shift(DOWN * 0.3)

        x_label = Text("Configuration", font_size=16, color=C["text_dim"],
                        font="Times New Roman").next_to(ax, DOWN, buff=0.3)
        y_label = Text("Probability", font_size=16, color=C["text_dim"],
                        font="Times New Roman").rotate(PI / 2).next_to(ax, LEFT, buff=0.35)
        self.play(Create(ax), FadeIn(x_label), FadeIn(y_label), run_time=0.6)

        # ── Beta tracker ───────────────────────────────────────────
        beta_tracker = ValueTracker(0.1)

        beta_tex = always_redraw(lambda: MathTex(
            r"\beta = " + f"{beta_tracker.get_value():.1f}",
            font_size=32, color=C["amber"],
        ).next_to(title, DOWN, buff=0.2))
        self.play(FadeIn(beta_tex), run_time=0.3)

        # ── Temperature interpretation labels ──────────────────────
        temp_label = always_redraw(lambda: Text(
            "HOT (random)" if beta_tracker.get_value() < 1.0
            else ("WARM (noisy)" if beta_tracker.get_value() < 3.0
                  else "COLD (precise)"),
            font_size=18,
            color=(C["coral"] if beta_tracker.get_value() < 1.0
                   else (C["amber"] if beta_tracker.get_value() < 3.0
                         else C["teal"])),
            font="Times New Roman",
        ).next_to(beta_tex, RIGHT, buff=0.5))
        self.play(FadeIn(temp_label), run_time=0.3)

        # ── Animated bars ──────────────────────────────────────────
        bar_width = 0.7
        bars = VGroup()

        def get_gibbs_probs(beta_val):
            weights = np.exp(beta_val * potentials)
            return weights / weights.sum()

        def create_bars(beta_val):
            probs = get_gibbs_probs(beta_val)
            bar_group = VGroup()
            for i in range(n_states):
                height = probs[i] * (4 / 0.55)  # scale to axis
                bar = Rectangle(
                    width=bar_width * (8 / (n_states + 1)),
                    height=max(height, 0.02),
                    fill_color=interpolate_color(
                        ManimColor(C["slate"]),
                        ManimColor(C["teal"]),
                        potentials[i] / potentials.max()
                    ),
                    fill_opacity=0.85,
                    stroke_width=0.5,
                    stroke_color=C["text_dim"],
                )
                x_coord = ax.c2p(i + 1, 0)[0]
                y_base = ax.c2p(0, 0)[1]
                bar.move_to([x_coord, y_base + bar.height / 2, 0])

                label = Text(state_labels[i], font_size=11,
                             color=C["text_dim"], font="Times New Roman")
                label.next_to(bar, DOWN, buff=0.08)
                bar_group.add(VGroup(bar, label))
            return bar_group

        bars = always_redraw(lambda: create_bars(beta_tracker.get_value()))
        self.play(FadeIn(bars), run_time=0.5)

        # ── Highlight the highest-potential state ──────────────────
        best_idx = np.argmax(potentials)
        pointer = always_redraw(lambda: Triangle(
            fill_color=C["highlight"], fill_opacity=0.9,
            stroke_width=0,
        ).scale(0.12).rotate(PI).move_to(
            ax.c2p(best_idx + 1, get_gibbs_probs(beta_tracker.get_value())[best_idx] * (4 / 0.55) / 4 + 0.1)
        ).shift(UP * 0.15))

        # ── Sweep beta from low to high ────────────────────────────
        self.wait(0.5)

        # Low beta — nearly uniform
        self.play(beta_tracker.animate.set_value(0.5), run_time=2)
        self.wait(1)

        # Medium beta — some structure
        self.play(beta_tracker.animate.set_value(2.0), run_time=2.5)
        self.wait(1)

        # High beta — concentrated on best state
        self.play(beta_tracker.animate.set_value(5.0), run_time=2.5)
        self.wait(1)

        # Very high beta — almost deterministic
        self.play(beta_tracker.animate.set_value(10.0), run_time=2)
        self.wait(1.5)

        # ── Annotation ─────────────────────────────────────────────
        annotation = Text(
            "As beta increases, the Gibbs measure concentrates\n"
            "on configurations with the highest potential.",
            font_size=16, color=C["text_light"],
            font="Times New Roman", line_spacing=1.3,
        ).to_edge(DOWN, buff=0.2)
        self.play(FadeIn(annotation), run_time=0.6)
        self.wait(2)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 3: Potential Function Surface with Gibbs Probability Cloud
# ─────────────────────────────────────────────────────────────────────
class PotentialGibbsScene(ThreeDScene):
    """
    3D surface showing the potential function Phi(x),
    with translucent spheres indicating the Gibbs probability
    at each point (larger sphere = higher probability).
    """

    def construct(self):
        self.camera.background_color = C["bg_dark"]

        # ── Title card ─────────────────────────────────────────────
        title = Text(
            "Potential Function & Gibbs Measure",
            font_size=32, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        )
        sub = Text(
            "pi(x) proportional to exp(beta * Phi(x))",
            font_size=18, color=C["text_dim"],
            font="Times New Roman", slant=ITALIC,
        )
        sub.next_to(title, DOWN, buff=0.15)
        tg = VGroup(title, sub).move_to(ORIGIN)
        self.add_fixed_in_frame_mobjects(tg)
        self.play(Write(title), run_time=0.8)
        self.play(FadeIn(sub), run_time=0.4)
        self.wait(1)
        self.play(FadeOut(tg), run_time=0.5)
        self.remove(tg)

        # ── Potential surface ──────────────────────────────────────
        def potential_func(u, v):
            """2D potential with multiple peaks."""
            x = (u - 0.5) * 6
            y = (v - 0.5) * 6
            # Two peaks and a valley
            z = (1.5 * np.exp(-((x - 1.5)**2 + (y - 1)**2) / 1.5)
                 + 1.0 * np.exp(-((x + 1.5)**2 + (y + 1)**2) / 2.0)
                 + 0.6 * np.exp(-((x)**2 + (y - 2)**2) / 1.0)
                 - 0.3)
            return np.array([x, y, z * 2])

        surface = Surface(
            potential_func,
            u_range=[0, 1], v_range=[0, 1],
            resolution=(35, 35),
            fill_opacity=0.7,
            stroke_width=0.3,
            stroke_color=C["text_dim"],
        )
        surface.set_color_by_gradient(
            ManimColor(C["navy"]),
            ManimColor(C["teal"]),
            ManimColor(C["amber"]),
            ManimColor(C["coral"]),
        )

        self.set_camera_orientation(phi=65 * DEGREES, theta=-45 * DEGREES)
        self.play(Create(surface), run_time=1.5)

        # ── Gibbs probability spheres at sample points ─────────────
        beta_val = 3.0
        sample_points = [
            (0.75, 0.58),  # near global peak
            (0.25, 0.33),  # near second peak
            (0.50, 0.83),  # near third peak
            (0.15, 0.75),  # low region
            (0.85, 0.20),  # low region
            (0.50, 0.50),  # valley
        ]

        phi_vals = []
        positions_3d = []
        for u, v in sample_points:
            pos = potential_func(u, v)
            phi_vals.append(pos[2])
            positions_3d.append(pos)

        phi_vals = np.array(phi_vals)
        gibbs_w = np.exp(beta_val * phi_vals)
        gibbs_p = gibbs_w / gibbs_w.sum()

        spheres = VGroup()
        for i, (pos, prob) in enumerate(zip(positions_3d, gibbs_p)):
            radius = 0.1 + prob * 1.5  # scale sphere by probability
            sphere = Sphere(
                radius=radius,
                resolution=(12, 12),
            ).move_to(pos + np.array([0, 0, radius + 0.1]))
            sphere.set_color(
                interpolate_color(
                    ManimColor(C["slate"]),
                    ManimColor(C["highlight"]),
                    prob / gibbs_p.max(),
                )
            )
            sphere.set_opacity(0.7)
            spheres.add(sphere)

        self.play(
            *[GrowFromCenter(s) for s in spheres],
            run_time=1.5,
        )
        self.wait(1)

        # ── Camera rotation to reveal structure ────────────────────
        self.begin_ambient_camera_rotation(rate=0.15)
        self.wait(4)
        self.stop_ambient_camera_rotation()

        # ── Annotation ─────────────────────────────────────────────
        note = Text(
            "Larger spheres = higher Gibbs probability\n"
            "The simulation spends more time at peaks",
            font_size=16, color=C["text_light"],
            font="Times New Roman", line_spacing=1.3,
        ).to_corner(DL, buff=0.3)
        self.add_fixed_in_frame_mobjects(note)
        self.play(FadeIn(note), run_time=0.5)
        self.wait(2)

        source = Text(
            "Blume (1993), Games and Economic Behavior",
            font_size=11, color=C["text_dim"], slant=ITALIC,
        ).to_corner(DR, buff=0.25)
        self.add_fixed_in_frame_mobjects(source)
        self.add(source)

        self.wait(2)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 4: Convergence Animation
# ─────────────────────────────────────────────────────────────────────
class ConvergenceScene(BaseSearchnetScene):
    """
    Starting from a random configuration, agents on a grid gradually
    organize via logit best-response. A side panel shows the potential
    function rising over time, illustrating convergence to the Gibbs
    measure's high-potential regions.
    """

    scene_title = "Convergence to Equilibrium"
    scene_subtitle = "From random to organized via logit best-response"

    GRID_SIZE = 10
    BETA = 2.0
    N_STEPS = 600
    CELL_SIZE = 0.42

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993), Games and Economic Behavior")

        # ── Layout: grid on left, potential plot on right ──────────
        title = Text(
            "Convergence to the Gibbs Measure",
            font_size=24, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.25)
        self.play(FadeIn(title), run_time=0.4)

        rng = np.random.default_rng(99)
        grid = rng.integers(0, 2, size=(self.GRID_SIZE, self.GRID_SIZE))

        # ── Grid (left side) ───────────────────────────────────────
        colors = [C["teal"], C["coral"]]
        cells = {}
        grid_group = VGroup()

        for r in range(self.GRID_SIZE):
            for cc in range(self.GRID_SIZE):
                sq = Square(
                    side_length=self.CELL_SIZE,
                    fill_opacity=0.85,
                    stroke_width=0.4,
                    stroke_color=C["bg_panel"],
                    fill_color=colors[grid[r, cc]],
                )
                x_pos = (cc - self.GRID_SIZE / 2 + 0.5) * self.CELL_SIZE - 3.0
                y_pos = (self.GRID_SIZE / 2 - 0.5 - r) * self.CELL_SIZE - 0.3
                sq.move_to([x_pos, y_pos, 0])
                cells[(r, cc)] = sq
                grid_group.add(sq)

        self.play(FadeIn(grid_group), run_time=0.6)

        # ── Potential plot (right side) ────────────────────────────
        ax = Axes(
            x_range=[0, self.N_STEPS, self.N_STEPS // 5],
            y_range=[0, 1.05, 0.2],
            x_length=5, y_length=3.5,
            axis_config={"color": C["text_dim"], "stroke_width": 1.2},
            tips=False,
        ).shift(RIGHT * 3.2 + DOWN * 0.3)

        ax_title = Text("Potential / Max", font_size=14,
                         color=C["text_dim"], font="Times New Roman"
                         ).next_to(ax, UP, buff=0.1)
        ax_xlabel = Text("Step", font_size=12, color=C["text_dim"],
                          font="Times New Roman").next_to(ax, DOWN, buff=0.15)
        self.play(Create(ax), FadeIn(ax_title), FadeIn(ax_xlabel), run_time=0.5)

        # ── Beta label ─────────────────────────────────────────────
        beta_label = MathTex(
            r"\beta = " + f"{self.BETA:.1f}",
            font_size=22, color=C["amber"],
        ).next_to(ax, RIGHT, buff=0.2).shift(UP * 1.5)
        self.play(FadeIn(beta_label), run_time=0.3)

        # ── Run dynamics and animate ───────────────────────────────
        phi_max = 2.0 * self.GRID_SIZE * (self.GRID_SIZE - 1)
        potential_history = []
        batch_size = 15
        n_batches = self.N_STEPS // batch_size

        # Collect initial potential
        phi_0 = ising_potential(grid) / phi_max
        potential_history.append(phi_0)

        line_points = [ax.c2p(0, phi_0)]
        potential_line = VMobject(
            stroke_color=C["highlight"], stroke_width=2.5
        )
        potential_line.set_points_as_corners(line_points)
        self.add(potential_line)

        for batch_idx in range(n_batches):
            cell_anims = []
            for _ in range(batch_size):
                grid, (r, cc) = ising_step(grid, self.BETA, rng)
                new_color = colors[grid[r, cc]]
                cell_anims.append(
                    cells[(r, cc)].animate.set_fill(new_color)
                )

            step = (batch_idx + 1) * batch_size
            phi_norm = ising_potential(grid) / phi_max
            potential_history.append(phi_norm)

            # Extend line
            new_point = ax.c2p(step, phi_norm)
            line_points.append(new_point)
            new_line = VMobject(
                stroke_color=C["highlight"], stroke_width=2.5
            )
            new_line.set_points_as_corners(line_points)

            self.play(
                *cell_anims,
                Transform(potential_line, new_line),
                run_time=0.15,
            )

        # ── Final annotation ───────────────────────────────────────
        phi_final = potential_history[-1]
        result = Text(
            f"Final order: {phi_final * 100:.0f}%",
            font_size=18, color=C["teal"],
            font="Times New Roman",
        ).next_to(ax, DOWN, buff=0.5)
        self.play(FadeIn(result), run_time=0.4)

        # ── Equilibrium annotation ─────────────────────────────────
        eq_line = DashedLine(
            ax.c2p(0, phi_final), ax.c2p(self.N_STEPS, phi_final),
            color=C["teal"], stroke_width=1.5, dash_length=0.08,
        )
        eq_label = MathTex(
            r"\pi_\beta",
            font_size=20, color=C["teal"],
        ).next_to(eq_line, RIGHT, buff=0.1)
        self.play(Create(eq_line), FadeIn(eq_label), run_time=0.5)

        convergence_note = Text(
            "The potential rises and stabilizes:\n"
            "the system has found the Gibbs equilibrium.",
            font_size=15, color=C["text_light"],
            font="Times New Roman", line_spacing=1.3,
        ).to_edge(DOWN, buff=0.15)
        self.play(FadeIn(convergence_note), run_time=0.5)

        self.wait(3)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)
