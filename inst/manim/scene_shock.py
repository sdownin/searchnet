"""
Scene: Exogenous Shock Animation
==================================
Timeline with pre/post shock zones showing K-4 trajectories
with a visible structural break. A dashed vertical line marks
the shock point with recovery annotation.

Reads from CSV with columns: step, K_AC, K_CA, K_AA, K_CC
and an optional 'shock_step' metadata.

Render:
    manim -qh scene_shock.py ShockScene
    manim -ql scene_shock.py ShockScene
"""

from manim import *
import numpy as np
from pathlib import Path
from searchnet_viz import BaseSearchnetScene, SEARCHNET_COLORS, loess_smooth

C = SEARCHNET_COLORS


class ShockScene(BaseSearchnetScene):
    """K-4 trajectories with exogenous shock and recovery annotation."""

    scene_title = "Exogenous Shock Analysis"
    scene_subtitle = "Structural break in degree trajectories"

    data_path = None    # CSV path
    shock_step = None   # Step at which shock occurs (auto-detected if None)

    DEGREE_TYPES = [
        {"key": "K_AC", "label": "K_AC", "color": C["K_AC"]},
        {"key": "K_CA", "label": "K_CA", "color": C["K_CA"]},
        {"key": "K_AA", "label": "K_AA", "color": C["K_AA"]},
        {"key": "K_CC", "label": "K_CC", "color": C["K_CC"]},
    ]

    def construct(self):
        self.add_title()
        self.add_source("SearchNet Simulation — Shock Experiment")

        # ── Load data ───────────────────────────────────────────────
        data = self._load_data()
        steps = data["step"]
        shock = data["shock_step"]

        # ── Main axes ───────────────────────────────────────────────
        x_min, x_max = steps[0], steps[-1]
        all_vals = np.concatenate([data[dt["key"]] for dt in self.DEGREE_TYPES])
        y_min = max(0, np.floor(all_vals.min() * 20) / 20)
        y_max = np.ceil(all_vals.max() * 20) / 20 + 0.01

        axes = Axes(
            x_range=[x_min, x_max, (x_max - x_min) / 5],
            y_range=[y_min, y_max, (y_max - y_min) / 4],
            x_length=11,
            y_length=5,
            axis_config={
                "color": C["text_dim"],
                "stroke_width": 1,
                "include_tip": False,
            },
        ).shift(DOWN * 0.3)

        x_lab = Text("Simulation Step", font_size=14, color=C["text_dim"])
        x_lab.next_to(axes, DOWN, buff=0.2)
        y_lab = Text("Degree", font_size=14, color=C["text_dim"])
        y_lab.next_to(axes, LEFT, buff=0.2).rotate(PI / 2)

        self.play(Create(axes), FadeIn(x_lab), FadeIn(y_lab), run_time=1.0)

        # ── Pre/post shock zones ────────────────────────────────────
        shock_x = axes.c2p(shock, 0)[0]
        axes_left = axes.c2p(x_min, 0)[0]
        axes_right = axes.c2p(x_max, 0)[0]
        axes_bottom = axes.c2p(0, y_min)[1]
        axes_top = axes.c2p(0, y_max)[1]
        zone_height = axes_top - axes_bottom

        # Pre-shock zone (light teal tint)
        pre_zone = Rectangle(
            width=shock_x - axes_left,
            height=zone_height,
            fill_color=C["teal"],
            fill_opacity=0.06,
            stroke_width=0,
        )
        pre_zone.move_to(np.array([(axes_left + shock_x) / 2, (axes_bottom + axes_top) / 2, 0]))

        pre_label = Text("Pre-Shock", font_size=14, color=C["teal"], slant=ITALIC)
        pre_label.move_to(np.array([(axes_left + shock_x) / 2, axes_top + 0.2, 0]))

        # Post-shock zone (light coral tint)
        post_zone = Rectangle(
            width=axes_right - shock_x,
            height=zone_height,
            fill_color=C["coral"],
            fill_opacity=0.06,
            stroke_width=0,
        )
        post_zone.move_to(np.array([(shock_x + axes_right) / 2, (axes_bottom + axes_top) / 2, 0]))

        post_label = Text("Post-Shock", font_size=14, color=C["coral"], slant=ITALIC)
        post_label.move_to(np.array([(shock_x + axes_right) / 2, axes_top + 0.2, 0]))

        self.play(
            FadeIn(pre_zone), FadeIn(post_zone),
            FadeIn(pre_label), FadeIn(post_label),
            run_time=0.8,
        )

        # ── Dashed shock line ───────────────────────────────────────
        shock_line = DashedLine(
            axes.c2p(shock, y_min),
            axes.c2p(shock, y_max),
            color=C["warning"],
            stroke_width=2.5,
            dash_length=0.12,
        )

        shock_marker = Text(
            "SHOCK",
            font_size=16,
            color=C["warning"],
            weight=BOLD,
        )
        shock_marker.next_to(shock_line, UP, buff=0.15)

        # Lightning bolt icon
        bolt = Text("\u26a1", font_size=24, color=C["warning"])
        bolt.next_to(shock_marker, LEFT, buff=0.1)

        self.play(Create(shock_line), FadeIn(shock_marker), FadeIn(bolt), run_time=0.8)

        # ── Draw pre-shock trajectories ─────────────────────────────
        pre_mask = steps <= shock
        legend_items = VGroup()

        for dt in self.DEGREE_TYPES:
            raw_y = data[dt["key"]]

            # Pre-shock segment
            pre_steps = steps[pre_mask]
            pre_vals = raw_y[pre_mask]

            if len(pre_steps) > 3:
                xs, ys = loess_smooth(pre_steps, pre_vals, frac=0.3, n_out=100)
                line_points = [axes.c2p(x, y) for x, y in zip(xs, ys)]
                pre_traj = VMobject(color=dt["color"], stroke_width=2.5)
                pre_traj.set_points_smoothly(line_points)
                self.play(Create(pre_traj), run_time=1.0)

            # Legend entry
            legend_line = Line(ORIGIN, RIGHT * 0.4, color=dt["color"], stroke_width=3)
            legend_text = Text(dt["label"], font_size=12, color=dt["color"])
            legend_text.next_to(legend_line, RIGHT, buff=0.1)
            legend_items.add(VGroup(legend_line, legend_text))

        legend_items.arrange(DOWN, aligned_edge=LEFT, buff=0.12)
        legend_items.to_corner(UR, buff=0.5)
        self.play(FadeIn(legend_items), run_time=0.5)

        # ── Shock flash effect ──────────────────────────────────────
        flash = Rectangle(
            width=14, height=8,
            fill_color=C["warning"],
            fill_opacity=0.2,
            stroke_width=0,
        )
        self.play(FadeIn(flash), run_time=0.15)
        self.play(FadeOut(flash), run_time=0.3)

        # ── Draw post-shock trajectories ────────────────────────────
        post_mask = steps >= shock

        for dt in self.DEGREE_TYPES:
            raw_y = data[dt["key"]]
            post_steps = steps[post_mask]
            post_vals = raw_y[post_mask]

            if len(post_steps) > 3:
                xs, ys = loess_smooth(post_steps, post_vals, frac=0.3, n_out=100)
                line_points = [axes.c2p(x, y) for x, y in zip(xs, ys)]
                post_traj = VMobject(color=dt["color"], stroke_width=2.5)
                post_traj.set_points_smoothly(line_points)
                self.play(Create(post_traj), run_time=1.0)

        # ── Recovery annotation ─────────────────────────────────────
        # Find the degree that recovers most
        recovery_info = []
        for dt in self.DEGREE_TYPES:
            raw_y = data[dt["key"]]
            shock_idx = np.argmin(np.abs(steps - shock))
            val_at_shock = raw_y[shock_idx]
            val_at_end = raw_y[-1]
            val_min_post = raw_y[shock_idx:].min()
            recovery = val_at_end - val_min_post
            recovery_info.append((dt, recovery, val_min_post, val_at_end))

        # Annotate the trajectory with strongest recovery
        recovery_info.sort(key=lambda x: x[1], reverse=True)
        best = recovery_info[0]
        best_dt = best[0]

        # Recovery arrow and annotation
        shock_idx = np.argmin(np.abs(steps - shock))
        min_post_idx = shock_idx + np.argmin(data[best_dt["key"]][shock_idx:])
        min_step = steps[min_post_idx]
        min_val = data[best_dt["key"]][min_post_idx]
        end_val = data[best_dt["key"]][-1]

        arrow_start = axes.c2p(min_step, min_val)
        arrow_end = axes.c2p(steps[-1] * 0.85, end_val)

        recovery_arrow = Arrow(
            arrow_start, arrow_end,
            color=C["positive"],
            stroke_width=2,
            buff=0.05,
            max_tip_length_to_length_ratio=0.15,
        )

        recovery_text = Text(
            "Recovery",
            font_size=14,
            color=C["positive"],
            slant=ITALIC,
        )
        recovery_text.next_to(recovery_arrow, UP, buff=0.1)

        self.play(Create(recovery_arrow), FadeIn(recovery_text), run_time=0.8)

        self.wait(4)

    def _load_data(self):
        """Load shock trajectory data or generate demo."""
        if self.data_path and Path(self.data_path).exists():
            rows = self.load_csv(self.data_path)
            steps = np.array([float(r["step"]) for r in rows])
            result = {"step": steps}
            for dt in self.DEGREE_TYPES:
                result[dt["key"]] = np.array([float(r[dt["key"]]) for r in rows])

            # Detect or use configured shock step
            if self.shock_step is not None:
                result["shock_step"] = self.shock_step
            elif "shock_step" in rows[0]:
                result["shock_step"] = float(rows[0]["shock_step"])
            else:
                result["shock_step"] = steps[len(steps) // 2]

            return result

        return self._generate_demo_data()

    @staticmethod
    def _generate_demo_data(n_steps=300, shock_frac=0.4, seed=42):
        """Generate synthetic K-4 trajectories with an exogenous shock."""
        rng = np.random.default_rng(seed)
        steps = np.arange(n_steps, dtype=float)
        shock_step = int(n_steps * shock_frac)
        t = steps / n_steps

        # Pre-shock: steady growth
        K_AC = np.where(
            steps < shock_step,
            0.02 + 0.05 * (steps / shock_step),
            0.02 + 0.05 - 0.03 * np.exp(-2 * (steps - shock_step) / n_steps) + 0.04 * ((steps - shock_step) / n_steps),
        ) + rng.normal(0, 0.003, n_steps)

        K_CA = np.where(
            steps < shock_step,
            0.015 + 0.03 * (steps / shock_step),
            0.015 + 0.03 - 0.025 * np.exp(-1.5 * (steps - shock_step) / n_steps) + 0.025 * ((steps - shock_step) / n_steps),
        ) + rng.normal(0, 0.002, n_steps)

        K_AA = np.where(
            steps < shock_step,
            0.005 + 0.025 * (1 - np.exp(-3 * steps / shock_step)),
            0.005 + 0.025 - 0.02 * np.exp(-1 * (steps - shock_step) / n_steps) + 0.015 * ((steps - shock_step) / n_steps),
        ) + rng.normal(0, 0.002, n_steps)

        K_CC = np.where(
            steps < shock_step,
            0.01 + 0.02 * (steps / shock_step) ** 0.7,
            0.01 + 0.02 - 0.018 * np.exp(-2.5 * (steps - shock_step) / n_steps) + 0.02 * ((steps - shock_step) / n_steps),
        ) + rng.normal(0, 0.002, n_steps)

        return {
            "step": steps,
            "K_AC": np.maximum(K_AC, 0),
            "K_CA": np.maximum(K_CA, 0),
            "K_AA": np.maximum(K_AA, 0),
            "K_CC": np.maximum(K_CC, 0),
            "shock_step": float(shock_step),
        }
