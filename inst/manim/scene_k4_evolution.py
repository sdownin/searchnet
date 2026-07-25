"""
Scene: K-4 Degree Panel Animation
===================================
Four coupled degree trajectories (K_AC, K_CA, K_AA, K_CC) evolving
over simulation time, displayed in a 2x2 grid with LOESS-style smoothing.

Reads from CSV exported by R with columns: step, K_AC, K_CA, K_AA, K_CC.

Render:
    manim -qh scene_k4_evolution.py K4EvolutionScene
    manim -ql scene_k4_evolution.py K4EvolutionScene  # preview
"""

from manim import *
import numpy as np
from pathlib import Path
from searchnet_viz import BaseSearchnetScene, SEARCHNET_COLORS, loess_smooth

C = SEARCHNET_COLORS

# Default data path — override via class attribute or subclass
DEFAULT_DATA = Path(__file__).parent / "data" / "k4_trajectories.csv"


class K4EvolutionScene(BaseSearchnetScene):
    """2x2 panel showing four coupled degree trajectories evolving over time."""

    scene_title = "Endogenous Degree Evolution"
    scene_subtitle = "Four coupled K parameters over simulation time"
    data_path = None  # Set before rendering or use demo data

    # Degree type definitions
    DEGREE_TYPES = [
        {"key": "K_AC", "label": "K_AC (Actor \u2192 Component)", "color": C["K_AC"], "pos": UP + LEFT},
        {"key": "K_CA", "label": "K_CA (Component \u2192 Actor)", "color": C["K_CA"], "pos": UP + RIGHT},
        {"key": "K_AA", "label": "K_AA (Actor \u2192 Actor)",     "color": C["K_AA"], "pos": DOWN + LEFT},
        {"key": "K_CC", "label": "K_CC (Component \u2192 Component)", "color": C["K_CC"], "pos": DOWN + RIGHT},
    ]

    def construct(self):
        # ── Title card ──────────────────────────────────────────────
        self.add_title()
        self.add_source("SearchNet Simulation")

        # ── Load or generate data ───────────────────────────────────
        data = self._load_data()
        steps = data["step"]
        n_steps = len(steps)

        # Determine axis ranges from data
        x_min, x_max = steps[0], steps[-1]
        all_vals = np.concatenate([data[dt["key"]] for dt in self.DEGREE_TYPES])
        y_min = max(0, np.floor(all_vals.min() * 20) / 20)
        y_max = np.ceil(all_vals.max() * 20) / 20 + 0.01

        # ── Create 2x2 axes grid ───────────────────────────────────
        panels = []
        for i, dt in enumerate(self.DEGREE_TYPES):
            # Position: 2x2 grid
            row = i // 2  # 0=top, 1=bottom
            col = i % 2   # 0=left, 1=right

            x_shift = -3.3 + col * 6.2
            y_shift = 1.5 - row * 3.8

            ax = Axes(
                x_range=[x_min, x_max, (x_max - x_min) / 5],
                y_range=[y_min, y_max, (y_max - y_min) / 4],
                x_length=5.2,
                y_length=3.0,
                axis_config={
                    "color": C["text_dim"],
                    "stroke_width": 1,
                    "include_tip": False,
                    "font_size": 16,
                },
            ).shift(RIGHT * x_shift + UP * y_shift)

            # Panel label
            label = Text(
                dt["label"],
                font_size=15,
                color=dt["color"],
                font="Times New Roman",
                weight=BOLD,
            )
            label.next_to(ax, UP, buff=0.15).align_to(ax, LEFT)

            # Axis labels
            x_lab = Text("Step", font_size=11, color=C["text_dim"])
            x_lab.next_to(ax, DOWN, buff=0.15)
            y_lab = Text("Degree", font_size=11, color=C["text_dim"])
            y_lab.next_to(ax, LEFT, buff=0.15).rotate(PI / 2)

            panels.append({
                "axes": ax,
                "label": label,
                "x_lab": x_lab,
                "y_lab": y_lab,
                "dt": dt,
            })

        # ── Draw all axes at once ───────────────────────────────────
        self.play(
            *[Create(p["axes"]) for p in panels],
            *[FadeIn(p["label"]) for p in panels],
            *[FadeIn(p["x_lab"]) for p in panels],
            *[FadeIn(p["y_lab"]) for p in panels],
            run_time=1.2,
        )

        # ── Progressive reveal of trajectories ──────────────────────
        for p in panels:
            dt = p["dt"]
            ax = p["axes"]
            raw_y = data[dt["key"]]

            # LOESS smoothing
            xs, ys = loess_smooth(steps, raw_y, frac=0.25, n_out=200)

            # Build smooth trajectory
            line_points = [ax.c2p(x, y) for x, y in zip(xs, ys)]
            trajectory = VMobject(color=dt["color"], stroke_width=2.5)
            trajectory.set_points_smoothly(line_points)

            # Confidence ribbon from raw data scatter
            raw_points = [ax.c2p(s, v) for s, v in zip(steps, raw_y)]
            raw_dots = VGroup(*[
                Dot(pt, radius=0.015, color=dt["color"], fill_opacity=0.3)
                for pt in raw_points[::max(1, len(raw_points) // 60)]
            ])

            # Animate: first scatter, then smooth line
            self.play(FadeIn(raw_dots), run_time=0.4)
            self.play(Create(trajectory), run_time=1.5, rate_func=rate_functions.ease_in_out_sine)

        # ── Final value annotations ─────────────────────────────────
        for p in panels:
            dt = p["dt"]
            ax = p["axes"]
            final_val = data[dt["key"]][-1]
            val_text = Text(
                f"{final_val:.3f}",
                font_size=14,
                color=dt["color"],
                weight=BOLD,
            )
            val_text.next_to(ax.c2p(steps[-1], final_val), RIGHT, buff=0.1)
            self.play(FadeIn(val_text), run_time=0.3)

        self.wait(3)

    def _load_data(self):
        """Load trajectory data from CSV or generate demo data."""
        csv_path = self.data_path or DEFAULT_DATA

        if Path(csv_path).exists():
            rows = self.load_csv(csv_path)
            steps = np.array([float(r["step"]) for r in rows])
            result = {"step": steps}
            for dt in self.DEGREE_TYPES:
                key = dt["key"]
                result[key] = np.array([float(r[key]) for r in rows])
            return result

        # Generate demo data with coupled dynamics
        return self._generate_demo_data()

    @staticmethod
    def _generate_demo_data(n_steps=200, seed=42):
        """
        Generate synthetic K-4 trajectories with coupled dynamics.
        Actor-component degrees grow, actor-actor converges, etc.
        """
        rng = np.random.default_rng(seed)
        steps = np.arange(n_steps)

        # Base trends with coupling
        t = steps / n_steps
        K_AC = 0.02 + 0.06 * t + 0.015 * np.sin(4 * np.pi * t) + rng.normal(0, 0.003, n_steps)
        K_CA = 0.01 + 0.04 * t + 0.010 * np.sin(3 * np.pi * t + 0.5) + rng.normal(0, 0.003, n_steps)
        K_AA = 0.005 + 0.03 * (1 - np.exp(-3 * t)) + rng.normal(0, 0.002, n_steps)
        K_CC = 0.01 + 0.02 * t ** 0.7 + 0.008 * np.sin(5 * np.pi * t) + rng.normal(0, 0.002, n_steps)

        # Ensure non-negative
        K_AC = np.maximum(K_AC, 0)
        K_CA = np.maximum(K_CA, 0)
        K_AA = np.maximum(K_AA, 0)
        K_CC = np.maximum(K_CC, 0)

        return {
            "step": steps.astype(float),
            "K_AC": K_AC,
            "K_CA": K_CA,
            "K_AA": K_AA,
            "K_CC": K_CC,
        }
