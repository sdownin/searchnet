"""
AMR Industrial Policy Basin Geometry Animations
================================================
Companion animations for:
  "Same Peak, Different Basin: How Industrial Policy Shapes
   the Hidden Geometry of Competitive Search"

Scenes:
  1. BasinGeometryScene    — 4 policy trajectories across a multi-basin landscape
  2. PolicyComparisonScene  — side-by-side mechanism diagrams
  3. ComplementarityTrapScene — morphing landscape + shrinking flexibility
  4. KCrossoverScene        — K-crossover: no universal best policy

Usage:
    manim -pql scene_amr_basin.py BasinGeometryScene
    manim -pql scene_amr_basin.py PolicyComparisonScene
    manim -pql scene_amr_basin.py ComplementarityTrapScene
    manim -pql scene_amr_basin.py KCrossoverScene
    manim -pql scene_amr_basin.py  # render all

Requires: manim CE >= 0.18, numpy
"""

import numpy as np
from manim import *

# Import shared palette
try:
    from searchnet_viz import SEARCHNET_COLORS as C, BaseSearchnetScene
except ImportError:
    # Fallback if running standalone
    C = {
        "navy":       "#1b3a5c",
        "teal":       "#2a9d8f",
        "amber":      "#e9c46a",
        "coral":      "#e76f51",
        "slate":      "#415a77",
        "sage":       "#6b9080",
        "plum":       "#7b2d8e",
        "steel":      "#778da9",
        "bg_dark":    "#0f1729",
        "bg_panel":   "#1a2238",
        "text_light": "#e8e8e8",
        "text_dim":   "#8899aa",
        "highlight":  "#f4a261",
        "warning":    "#e76f51",
        "positive":   "#2a9d8f",
        "neutral":    "#778da9",
    }

    class BaseSearchnetScene(Scene):
        def setup(self):
            self.camera.background_color = C["bg_dark"]

        def add_persistent_title(self, title_text, font="Times New Roman"):
            title = Text(
                title_text,
                font_size=28,
                color=C["text_light"],
                font=font,
                weight=BOLD,
            ).to_edge(UP, buff=0.3)
            self.play(FadeIn(title), run_time=0.5)
            return title

        def add_source(self, source_text, font_size=11):
            source = Text(
                source_text,
                font_size=font_size,
                color=C["text_dim"],
                slant=ITALIC,
            ).to_corner(DR, buff=0.25)
            self.add(source)
            return source


# ─────────────────────────────────────────────────────────────────────
# Policy color mapping
# ─────────────────────────────────────────────────────────────────────
POLICY_COLORS = {
    "P1": C["coral"],       # Resource-Suppressing (Subsidy)
    "P2": C["amber"],       # Resource-Enriching (Grant)
    "P3": C["teal"],        # Resource-Retaining (Tax Concession)
    "P4": C["plum"],        # Resource-Freeing (Guarantee)
}

POLICY_LABELS = {
    "P1": "Res-Suppressing",
    "P2": "Res-Enriching",
    "P3": "Res-Retaining",
    "P4": "Res-Freeing",
}


# ─────────────────────────────────────────────────────────────────────
# Synthetic landscape generation utilities
# ─────────────────────────────────────────────────────────────────────
def _make_basin_landscape(nx=200, ny=200):
    """
    Generate a 2D fitness landscape with 3 distinct basins (peaks).

    Returns
    -------
    X, Y : np.ndarray (nx, ny)
        Meshgrid coordinate arrays.
    Z : np.ndarray (nx, ny)
        Fitness values.
    peaks : list of (cx, cy)
        Peak center coordinates in data space.
    """
    x = np.linspace(-3, 3, nx)
    y = np.linspace(-3, 3, ny)
    X, Y = np.meshgrid(x, y)

    # Three basin centers with distinct shapes
    # Basin A (teal): narrow, deep — top-left
    cx_a, cy_a, sx_a, sy_a, h_a = -1.4, 1.3, 0.55, 0.55, 1.0
    # Basin B (amber): broad, shallow — bottom-center
    cx_b, cy_b, sx_b, sy_b, h_b = 0.3, -1.0, 1.1, 0.9, 0.65
    # Basin C (coral): medium depth, medium width — right
    cx_c, cy_c, sx_c, sy_c, h_c = 1.8, 0.8, 0.7, 0.8, 0.85

    Za = h_a * np.exp(-((X - cx_a) ** 2 / (2 * sx_a ** 2) + (Y - cy_a) ** 2 / (2 * sy_a ** 2)))
    Zb = h_b * np.exp(-((X - cx_b) ** 2 / (2 * sx_b ** 2) + (Y - cy_b) ** 2 / (2 * sy_b ** 2)))
    Zc = h_c * np.exp(-((X - cx_c) ** 2 / (2 * sx_c ** 2) + (Y - cy_c) ** 2 / (2 * sy_c ** 2)))

    # Add subtle ruggedness
    rng = np.random.default_rng(42)
    noise = 0.04 * np.sin(3.5 * X) * np.cos(4.2 * Y) + 0.03 * np.sin(6.1 * X + 1.2) * np.cos(5.3 * Y - 0.7)

    Z = Za + Zb + Zc + noise
    Z = np.clip(Z, 0, None)

    peaks = [(cx_a, cy_a), (cx_b, cy_b), (cx_c, cy_c)]
    return X, Y, Z, peaks


def _data_to_axes_coords(ax, data_x, data_y, X, Y):
    """Convert data-space coordinates to Axes mobject coordinates."""
    x_min, x_max = X[0, 0], X[0, -1]
    y_min, y_max = Y[0, 0], Y[-1, 0]
    return ax.c2p(data_x, data_y)


# ─────────────────────────────────────────────────────────────────────
# Scene 1: BasinGeometryScene
# ─────────────────────────────────────────────────────────────────────
class BasinGeometryScene(BaseSearchnetScene):
    """
    4 policy agent groups navigate a multi-basin fitness landscape,
    each converging on a different basin — illustrating how policy
    instruments shape search trajectories.
    """

    def construct(self):
        # ── Title card ──
        title = Text(
            "Same Peak, Different Basin",
            font_size=34,
            color=C["text_light"],
            font="Times New Roman",
            weight=BOLD,
        ).move_to(ORIGIN)
        sub = Text(
            "How Industrial Policy Shapes Competitive Search",
            font_size=18,
            color=C["text_dim"],
            font="Times New Roman",
            slant=ITALIC,
        ).next_to(title, DOWN, buff=0.2)
        self.play(Write(title), run_time=0.8)
        self.play(FadeIn(sub), run_time=0.4)
        self.wait(1.5)
        self.play(FadeOut(VGroup(title, sub)), run_time=0.5)

        # ── Generate landscape ──
        X, Y, Z, peaks = _make_basin_landscape(nx=200, ny=200)

        # Create axes
        ax = Axes(
            x_range=[-3, 3, 1],
            y_range=[-3, 3, 1],
            x_length=6.5,
            y_length=5.5,
            tips=False,
            axis_config={"color": C["steel"], "stroke_width": 1},
        ).shift(LEFT * 0.3 + DOWN * 0.2)

        # ── Contour heatmap as colored rectangles ──
        # Downsample for rendering efficiency
        step = 8
        Xd, Yd, Zd = X[::step, ::step], Y[::step, ::step], Z[::step, ::step]
        z_max = Zd.max()

        # Basin color assignment: tint each cell by dominant basin
        # Basin A peak at (-1.4, 1.3), B at (0.3, -1.0), C at (1.8, 0.8)
        basin_colors_hex = [C["teal"], C["amber"], C["coral"]]

        heatmap_rects = VGroup()
        dx = (Xd[0, 1] - Xd[0, 0]) if Xd.shape[1] > 1 else 0.3
        dy = (Yd[1, 0] - Yd[0, 0]) if Yd.shape[0] > 1 else 0.3

        for i in range(Zd.shape[0]):
            for j in range(Zd.shape[1]):
                xi, yi, zi = Xd[i, j], Yd[i, j], Zd[i, j]
                # Determine dominant basin by proximity-weighted fitness
                dists = [
                    np.sqrt((xi - px) ** 2 + (yi - py) ** 2)
                    for (px, py) in peaks
                ]
                # Gaussian influence of each peak
                influences = [
                    Zd[i, j] * np.exp(-d ** 2 / 3.0)
                    for d in dists
                ]
                dominant = int(np.argmax(influences))
                base_color = ManimColor(basin_colors_hex[dominant])

                # Intensity proportional to fitness
                intensity = zi / z_max if z_max > 0 else 0
                # Interpolate between dark background and basin color
                bg = ManimColor(C["bg_dark"])
                cell_color = interpolate_color(bg, base_color, intensity * 0.85)

                corner = ax.c2p(xi - dx / 2, yi - dy / 2)
                opposite = ax.c2p(xi + dx / 2, yi + dy / 2)
                w = abs(opposite[0] - corner[0])
                h = abs(opposite[1] - corner[1])
                center_pt = ax.c2p(xi, yi)

                rect = Rectangle(
                    width=w,
                    height=h,
                    fill_color=cell_color,
                    fill_opacity=0.9,
                    stroke_width=0,
                ).move_to(center_pt)
                heatmap_rects.add(rect)

        # ── Contour lines ──
        contour_lines = VGroup()
        levels = np.linspace(0.1, z_max * 0.9, 8)
        for level in levels:
            # Simple contour: draw a circle-ish path around each peak above level
            for pidx, (cx, cy) in enumerate(peaks):
                col = basin_colors_hex[pidx]
                n_pts = 60
                angles = np.linspace(0, 2 * np.pi, n_pts, endpoint=False)
                # Find approximate radius where Z ~ level
                # Use radial sampling
                radii = np.linspace(0.05, 2.5, 40)
                # Average radius where landscape ~ level around this peak
                ring_pts = []
                for angle in angles:
                    for r in radii:
                        px = cx + r * np.cos(angle)
                        py = cy + r * np.sin(angle)
                        if -3 <= px <= 3 and -3 <= py <= 3:
                            # Interpolate Z
                            ix = int((px + 3) / 6 * 199)
                            iy = int((py + 3) / 6 * 199)
                            ix = np.clip(ix, 0, 199)
                            iy = np.clip(iy, 0, 199)
                            if abs(Z[iy, ix] - level) < 0.04:
                                ring_pts.append((px, py))
                                break
                if len(ring_pts) >= 6:
                    # Sort by angle
                    ring_pts.sort(key=lambda p: np.arctan2(p[1] - cy, p[0] - cx))
                    ring_pts.append(ring_pts[0])  # close loop
                    points = [ax.c2p(p[0], p[1]) for p in ring_pts]
                    line = VMobject(color=col, stroke_width=0.8, stroke_opacity=0.5)
                    line.set_points_smoothly([np.array(p) for p in points])
                    contour_lines.add(line)

        # ── Policy trajectories ──
        # Each policy starts from a neutral position and curves toward its basin
        rng = np.random.default_rng(123)

        # Trajectory waypoints (data coordinates)
        trajectories = {
            # P1: Res-Suppressing -> narrow basin A (teal, top-left)
            "P1": [(-0.5, -0.5), (-0.8, 0.2), (-1.1, 0.7), (-1.3, 1.1), (-1.4, 1.3)],
            # P2: Res-Enriching -> broad shallow basin B (amber, bottom)
            "P2": [(0.2, 0.5), (0.2, 0.1), (0.3, -0.3), (0.3, -0.7), (0.3, -1.0)],
            # P3: Res-Retaining -> deep basin C (coral, right)
            "P3": [(0.5, -0.3), (0.9, 0.0), (1.2, 0.3), (1.5, 0.5), (1.8, 0.8)],
            # P4: Res-Freeing -> balanced path landing between B and C
            "P4": [(-0.2, 0.3), (0.3, 0.5), (0.8, 0.6), (1.3, 0.7), (1.6, 0.75)],
        }

        # Build trail lines and agent dots
        trail_groups = {}
        agent_dots = {}
        agent_labels = {}

        for policy, waypoints in trajectories.items():
            col = POLICY_COLORS[policy]

            # Smooth the trajectory with more intermediate points
            t_vals = np.linspace(0, 1, len(waypoints))
            t_fine = np.linspace(0, 1, 50)
            wx = np.interp(t_fine, t_vals, [w[0] for w in waypoints])
            wy = np.interp(t_fine, t_vals, [w[1] for w in waypoints])

            # Small random perturbation for natural feel
            wx += rng.normal(0, 0.02, len(wx))
            wy += rng.normal(0, 0.02, len(wy))

            scene_pts = [np.array(ax.c2p(wx[i], wy[i])) for i in range(len(wx))]

            trail = VMobject(color=col, stroke_width=2.5, stroke_opacity=0.85)
            trail.set_points_smoothly(scene_pts)
            trail_groups[policy] = trail

            # Agent dot at start
            start_pt = ax.c2p(waypoints[0][0], waypoints[0][1])
            dot = Dot(point=start_pt, radius=0.1, color=col)
            agent_dots[policy] = dot

            # Label
            label = Text(
                f"{policy}: {POLICY_LABELS[policy]}",
                font_size=12,
                color=col,
                font="Times New Roman",
            )
            end_pt = ax.c2p(waypoints[-1][0], waypoints[-1][1])
            label.next_to(end_pt, RIGHT, buff=0.15)
            agent_labels[policy] = label

        # ── Animate ──
        self.play(FadeIn(heatmap_rects), run_time=1.0)
        self.play(FadeIn(contour_lines), run_time=0.8)

        # Show agents appearing
        dot_group = VGroup(*agent_dots.values())
        self.play(*[FadeIn(d) for d in agent_dots.values()], run_time=0.5)

        # Animate each trajectory simultaneously
        trail_anims = []
        move_anims = []
        for policy in ["P1", "P2", "P3", "P4"]:
            trail = trail_groups[policy]
            dot = agent_dots[policy]
            trail_anims.append(Create(trail))
            move_anims.append(MoveAlongPath(dot, trail))

        self.play(*trail_anims, *move_anims, run_time=5.0)

        # Add labels
        self.play(
            *[FadeIn(agent_labels[p]) for p in ["P1", "P2", "P3", "P4"]],
            run_time=0.8,
        )
        self.wait(1.0)

        # ── Legend ──
        legend_items = VGroup()
        for i, policy in enumerate(["P1", "P2", "P3", "P4"]):
            col = POLICY_COLORS[policy]
            dot = Dot(radius=0.06, color=col)
            txt = Text(
                f"{policy}: {POLICY_LABELS[policy]}",
                font_size=11,
                color=C["text_light"],
                font="Times New Roman",
            )
            row = VGroup(dot, txt).arrange(RIGHT, buff=0.12)
            legend_items.add(row)
        legend_items.arrange(DOWN, aligned_edge=LEFT, buff=0.1)
        legend_box = SurroundingRectangle(
            legend_items, color=C["steel"], stroke_width=1, fill_color=C["bg_panel"], fill_opacity=0.8, buff=0.15
        )
        legend = VGroup(legend_box, legend_items).to_corner(UL, buff=0.3)
        self.play(FadeIn(legend), run_time=0.5)
        self.wait(1.5)

        # Final message
        final_msg = Text(
            "Same landscape, different basins",
            font_size=26,
            color=C["highlight"],
            font="Times New Roman",
            weight=BOLD,
        ).to_edge(DOWN, buff=0.5)
        self.play(Write(final_msg), run_time=1.0)
        self.wait(2.0)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 2: PolicyComparisonScene
# ─────────────────────────────────────────────────────────────────────
class PolicyComparisonScene(BaseSearchnetScene):
    """
    Four side-by-side panels showing how each policy instrument
    reshapes the fitness landscape through different mechanism bundles.
    """

    def construct(self):
        title = self.add_persistent_title(
            "Four Policy Instruments, Four Search Corridors"
        )
        self.add_source("Downing (2026) — AMR Industrial Policy")

        # Panel configuration
        panels_data = [
            {
                "policy": "P1",
                "label": "Res-Suppressing",
                "arrows": [
                    ("Scope", "down", -0.6),
                    ("Synergy", "down", -0.3),
                    ("Herding", "up", 0.2),
                ],
                "summary": "Constrained breadth",
            },
            {
                "policy": "P2",
                "label": "Res-Enriching",
                "arrows": [
                    ("Scope", "up", 0.5),
                    ("Herding", "up", 0.4),
                ],
                "summary": "Broad + herding",
            },
            {
                "policy": "P3",
                "label": "Res-Retaining",
                "arrows": [
                    ("Synergy", "up", 0.6),
                    ("Herding", "down", -0.3),
                ],
                "summary": "Deep + niche",
            },
            {
                "policy": "P4",
                "label": "Res-Freeing",
                "arrows": [
                    ("Scope", "up", 0.6),
                    ("Synergy", "up", 0.5),
                ],
                "summary": "Balanced breadth + depth",
            },
        ]

        # Create 4 panels
        panel_groups = VGroup()
        panel_width = 2.8
        panel_height = 4.0

        for i, pd in enumerate(panels_data):
            policy = pd["policy"]
            col = POLICY_COLORS[policy]

            # Panel background
            bg = RoundedRectangle(
                width=panel_width,
                height=panel_height,
                corner_radius=0.1,
                fill_color=C["bg_panel"],
                fill_opacity=0.9,
                stroke_color=col,
                stroke_width=2,
            )

            # Policy label header
            header = Text(
                f"{policy}: {pd['label']}",
                font_size=14,
                color=col,
                font="Times New Roman",
                weight=BOLD,
            )
            header.move_to(bg.get_top() + DOWN * 0.35)

            # Fitness curve inside panel
            curve_ax = Axes(
                x_range=[0, 4, 1],
                y_range=[0, 1.2, 0.3],
                x_length=2.0,
                y_length=1.4,
                tips=False,
                axis_config={"color": C["steel"], "stroke_width": 0.8},
            )
            curve_ax.move_to(bg.get_center() + UP * 0.2)

            # Baseline fitness curve
            baseline_curve = curve_ax.plot(
                lambda x: 0.8 * np.exp(-((x - 2) ** 2) / 1.5),
                x_range=[0.1, 3.9],
                color=C["steel"],
                stroke_width=1.5,
                stroke_opacity=0.5,
            )

            # Modified curve reflecting policy
            if policy == "P1":
                # Narrower, shifted left (constrained scope)
                mod_curve = curve_ax.plot(
                    lambda x: 0.9 * np.exp(-((x - 1.5) ** 2) / 0.6),
                    x_range=[0.1, 3.9],
                    color=col,
                    stroke_width=2.5,
                )
            elif policy == "P2":
                # Broader, slightly lower (wide but shallow)
                mod_curve = curve_ax.plot(
                    lambda x: 0.55 * np.exp(-((x - 2.2) ** 2) / 3.0),
                    x_range=[0.1, 3.9],
                    color=col,
                    stroke_width=2.5,
                )
            elif policy == "P3":
                # Deeper, same width (synergy-boosted)
                mod_curve = curve_ax.plot(
                    lambda x: 1.1 * np.exp(-((x - 2) ** 2) / 1.0),
                    x_range=[0.1, 3.9],
                    color=col,
                    stroke_width=2.5,
                )
            else:  # P4
                # Both broader AND deeper
                mod_curve = curve_ax.plot(
                    lambda x: 1.0 * np.exp(-((x - 2.3) ** 2) / 2.0),
                    x_range=[0.1, 3.9],
                    color=col,
                    stroke_width=2.5,
                )

            # Mechanism arrows
            arrow_group = VGroup()
            arrow_start_y = bg.get_center()[1] - 0.8
            for j, (mech_name, direction, delta) in enumerate(pd["arrows"]):
                y_pos = arrow_start_y - j * 0.45
                x_pos = bg.get_center()[0]

                mech_label = Text(
                    mech_name,
                    font_size=11,
                    color=C["text_light"],
                    font="Times New Roman",
                )
                mech_label.move_to([x_pos - 0.6, y_pos, 0])

                # Arrow showing direction of effect
                arrow_col = C["positive"] if direction == "up" else C["warning"]
                arrow_start = np.array([x_pos + 0.3, y_pos, 0])
                if direction == "up":
                    arrow_end = arrow_start + np.array([0.0, 0.25, 0])
                    arrow_symbol = Text(
                        f"+{abs(delta):.1f}",
                        font_size=10,
                        color=arrow_col,
                        font="Times New Roman",
                    )
                else:
                    arrow_end = arrow_start + np.array([0.0, -0.25, 0])
                    arrow_symbol = Text(
                        f"{delta:.1f}",
                        font_size=10,
                        color=arrow_col,
                        font="Times New Roman",
                    )
                arr = Arrow(
                    start=arrow_start,
                    end=arrow_end,
                    color=arrow_col,
                    stroke_width=3,
                    max_tip_length_to_length_ratio=0.4,
                    buff=0,
                )
                arrow_symbol.next_to(arr, RIGHT, buff=0.08)
                arrow_group.add(VGroup(mech_label, arr, arrow_symbol))

            # Summary text
            summary = Text(
                pd["summary"],
                font_size=12,
                color=col,
                font="Times New Roman",
                slant=ITALIC,
            )
            summary.move_to(bg.get_bottom() + UP * 0.3)

            panel = VGroup(bg, header, curve_ax, baseline_curve, mod_curve, arrow_group, summary)
            panel_groups.add(panel)

        # Arrange panels in a row
        panel_groups.arrange(RIGHT, buff=0.2)
        panel_groups.scale_to_fit_width(13.0)
        panel_groups.next_to(title, DOWN, buff=0.35)

        # Animate
        for i, panel in enumerate(panel_groups):
            self.play(FadeIn(panel), run_time=0.6)
            self.wait(0.4)

        self.wait(3.0)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 3: ComplementarityTrapScene
# ─────────────────────────────────────────────────────────────────────
class ComplementarityTrapScene(BaseSearchnetScene):
    """
    Illustrates the Complementarity Trap:
    P3 (Res-Retaining) boosts synergy payoffs, deepening the fitness
    basin and reducing firms' strategic flexibility.
    """

    def construct(self):
        title = self.add_persistent_title("The Complementarity Trap")
        self.add_source("Downing (2026) — AMR Industrial Policy")

        # ── Axes for the morphing fitness curve ──
        ax = Axes(
            x_range=[0, 6, 1],
            y_range=[0, 1.5, 0.3],
            x_length=7,
            y_length=3.5,
            tips=False,
            axis_config={"color": C["steel"], "stroke_width": 1.2},
        ).shift(UP * 0.3)

        x_label = Text(
            "Strategy Space",
            font_size=14,
            color=C["text_dim"],
            font="Times New Roman",
        ).next_to(ax, DOWN, buff=0.2)
        y_label = Text(
            "Fitness",
            font_size=14,
            color=C["text_dim"],
            font="Times New Roman",
        ).next_to(ax, LEFT, buff=0.15).rotate(PI / 2)

        self.play(FadeIn(ax), FadeIn(x_label), FadeIn(y_label), run_time=0.6)

        # ── Phase 1: flat landscape ──
        phase_label = Text(
            "Phase 1: Flat landscape (no policy)",
            font_size=16,
            color=C["text_light"],
            font="Times New Roman",
        ).next_to(ax, UP, buff=0.1).shift(DOWN * 0.2)
        self.play(FadeIn(phase_label), run_time=0.3)

        # ValueTracker for basin depth (0 = flat, 1 = deep)
        depth = ValueTracker(0.0)

        def curve_func(x, d):
            """Fitness curve that deepens as d increases from 0 to 1."""
            base = 0.3 + 0.1 * np.sin(0.8 * x)  # flat baseline
            basin = (0.3 + 0.9 * d) * np.exp(-((x - 3) ** 2) / (1.5 - 0.7 * d + 0.01))
            return base + basin

        curve = always_redraw(
            lambda: ax.plot(
                lambda x: curve_func(x, depth.get_value()),
                x_range=[0.2, 5.8],
                color=interpolate_color(
                    ManimColor(C["steel"]),
                    ManimColor(C["teal"]),
                    depth.get_value(),
                ),
                stroke_width=3,
            )
        )
        self.play(Create(curve), run_time=0.8)
        self.wait(1.0)

        # ── Phase 2: Grant policy deepens basin ──
        phase2_label = Text(
            "Phase 2: P3 boosts synergy \u2192 basin deepens",
            font_size=16,
            color=C["teal"],
            font="Times New Roman",
        ).move_to(phase_label.get_center())

        self.play(
            FadeOut(phase_label),
            FadeIn(phase2_label),
            run_time=0.4,
        )

        # Animate depth increasing
        self.play(depth.animate.set_value(1.0), run_time=3.0, rate_func=smooth)
        self.wait(0.5)

        # ── Flexibility bars ──
        bar_label = Text(
            "Strategic Flexibility",
            font_size=14,
            color=C["text_light"],
            font="Times New Roman",
        ).to_edge(DOWN, buff=1.4)
        self.play(FadeIn(bar_label), run_time=0.3)

        n_bars = 8
        bar_width = 0.5
        bar_group = VGroup()
        max_height = 1.2
        bar_start_x = -2.5

        for i in range(n_bars):
            bar = Rectangle(
                width=bar_width,
                height=max_height,
                fill_color=C["sage"],
                fill_opacity=0.8,
                stroke_color=C["sage"],
                stroke_width=1,
            )
            bar.move_to([bar_start_x + i * (bar_width + 0.15), -2.5, 0])
            bar.align_to([0, -3.1, 0], DOWN)
            bar_group.add(bar)

        self.play(FadeIn(bar_group), run_time=0.5)
        self.wait(0.5)

        # Shrink bars as depth increases (already at 1.0, now animate shrinking)
        phase3_label = Text(
            "More synergy = less flexibility",
            font_size=16,
            color=C["warning"],
            font="Times New Roman",
            weight=BOLD,
        ).move_to(phase2_label.get_center())

        shrink_anims = []
        rng = np.random.default_rng(99)
        for i, bar in enumerate(bar_group):
            target_height = max_height * (0.15 + 0.1 * rng.random())
            new_bar = bar.copy()
            new_bar.stretch_to_fit_height(target_height)
            new_bar.align_to([0, -3.1, 0], DOWN)
            new_bar.set_fill(C["warning"], opacity=0.8)
            new_bar.set_stroke(C["warning"])
            shrink_anims.append(Transform(bar, new_bar))

        self.play(
            FadeOut(phase2_label),
            FadeIn(phase3_label),
            *shrink_anims,
            run_time=2.5,
            rate_func=smooth,
        )

        # Trap label
        trap_text = Text(
            "The Complementarity Trap: More synergy = less flexibility",
            font_size=20,
            color=C["highlight"],
            font="Times New Roman",
            weight=BOLD,
        ).to_edge(DOWN, buff=0.3)
        self.play(Write(trap_text), run_time=1.0)
        self.wait(2.5)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 4: KCrossoverScene
# ─────────────────────────────────────────────────────────────────────
class KCrossoverScene(BaseSearchnetScene):
    """
    Shows how the optimal policy switches as landscape complexity K
    increases: P4 (Res-Freeing) dominates at low K,
    P3 (Res-Retaining) dominates at high K, with a crossover at K*.
    """

    def construct(self):
        title = self.add_persistent_title("K-Crossover: No Universal Best Policy")
        self.add_source("Downing (2026) — AMR Industrial Policy")

        # ── Axes ──
        ax = Axes(
            x_range=[0, 14, 2],
            y_range=[0, 1.1, 0.2],
            x_length=8,
            y_length=4.5,
            tips=False,
            axis_config={"color": C["steel"], "stroke_width": 1.2},
            x_axis_config={
                "numbers_to_include": [2, 4, 6, 8, 10, 12],
                "font_size": 20,
            },
            y_axis_config={
                "numbers_to_include": [0.2, 0.4, 0.6, 0.8, 1.0],
                "font_size": 20,
            },
        ).shift(DOWN * 0.2)

        x_lab = Text(
            "K (Landscape Complexity)",
            font_size=16,
            color=C["text_dim"],
            font="Times New Roman",
        ).next_to(ax, DOWN, buff=0.35)
        y_lab = Text(
            "Policy Advantage",
            font_size=16,
            color=C["text_dim"],
            font="Times New Roman",
        ).next_to(ax, LEFT, buff=0.25).rotate(PI / 2)

        self.play(FadeIn(ax), FadeIn(x_lab), FadeIn(y_lab), run_time=0.6)

        # ── P4 curve: dominates at low K, declines ──
        # P4 advantage = sigmoid-decay: high at low K, drops at high K
        def p4_advantage(k):
            return 0.95 / (1 + np.exp(0.7 * (k - 7))) + 0.05

        # ── P3 curve: weak at low K, rises ──
        def p3_advantage(k):
            return 0.90 / (1 + np.exp(-0.7 * (k - 7))) + 0.05

        p4_curve = ax.plot(
            p4_advantage,
            x_range=[0.5, 13.5],
            color=C["plum"],
            stroke_width=3,
        )
        p3_curve = ax.plot(
            p3_advantage,
            x_range=[0.5, 13.5],
            color=C["teal"],
            stroke_width=3,
        )

        # Labels for curves
        p4_label = Text(
            "P4: Res-Freeing",
            font_size=14,
            color=C["plum"],
            font="Times New Roman",
            weight=BOLD,
        )
        p4_label.next_to(ax.c2p(2, p4_advantage(2)), UP, buff=0.15)

        p3_label = Text(
            "P3: Res-Retaining",
            font_size=14,
            color=C["teal"],
            font="Times New Roman",
            weight=BOLD,
        )
        p3_label.next_to(ax.c2p(11, p3_advantage(11)), UP, buff=0.15)

        # Animate curves drawing
        self.play(Create(p4_curve), FadeIn(p4_label), run_time=1.5)
        self.play(Create(p3_curve), FadeIn(p3_label), run_time=1.5)
        self.wait(0.5)

        # ── Crossover point K* ──
        # Solve for crossover: p4_advantage(k) = p3_advantage(k) => k ~ 7
        k_star = 7.0
        y_star = p4_advantage(k_star)

        crossover_dot = Dot(
            point=ax.c2p(k_star, y_star),
            radius=0.1,
            color=C["highlight"],
        )
        crossover_label = MathTex(
            "K^*",
            font_size=30,
            color=C["highlight"],
        ).next_to(crossover_dot, UR, buff=0.12)

        # Dashed vertical line at K*
        k_star_line = DashedLine(
            start=ax.c2p(k_star, 0),
            end=ax.c2p(k_star, y_star),
            color=C["highlight"],
            stroke_width=1.5,
            dash_length=0.1,
        )

        self.play(
            FadeIn(crossover_dot),
            FadeIn(crossover_label),
            Create(k_star_line),
            run_time=1.0,
        )
        self.wait(0.5)

        # ── Shaded regions showing dominance ──
        # Left region: P4 dominates (low K)
        left_region = ax.get_area(
            p4_curve,
            x_range=[0.5, k_star],
            bounded_graph=p3_curve,
            color=C["plum"],
            opacity=0.2,
        )
        left_text = Text(
            "P4 dominates",
            font_size=13,
            color=C["plum"],
            font="Times New Roman",
            slant=ITALIC,
        ).move_to(ax.c2p(3.5, 0.5))

        # Right region: P3 dominates (high K)
        right_region = ax.get_area(
            p3_curve,
            x_range=[k_star, 13.5],
            bounded_graph=p4_curve,
            color=C["teal"],
            opacity=0.2,
        )
        right_text = Text(
            "P3 dominates",
            font_size=13,
            color=C["teal"],
            font="Times New Roman",
            slant=ITALIC,
        ).move_to(ax.c2p(10.5, 0.5))

        self.play(
            FadeIn(left_region),
            FadeIn(left_text),
            FadeIn(right_region),
            FadeIn(right_text),
            run_time=1.5,
        )
        self.wait(1.0)

        # ── Animated K sweep ──
        k_tracker = ValueTracker(1.0)
        sweep_line = always_redraw(
            lambda: DashedLine(
                start=ax.c2p(k_tracker.get_value(), 0),
                end=ax.c2p(k_tracker.get_value(), 1.05),
                color=C["text_light"],
                stroke_width=1,
                dash_length=0.08,
                stroke_opacity=0.6,
            )
        )
        sweep_label = always_redraw(
            lambda: Text(
                f"K = {k_tracker.get_value():.0f}",
                font_size=14,
                color=C["text_light"],
                font="Times New Roman",
            ).next_to(ax.c2p(k_tracker.get_value(), 1.05), UP, buff=0.1)
        )

        # Winner indicator
        def _get_winner_text():
            k = k_tracker.get_value()
            if p4_advantage(k) > p3_advantage(k) + 0.02:
                return Text("Winner: P4", font_size=15, color=C["plum"], font="Times New Roman", weight=BOLD)
            elif p3_advantage(k) > p4_advantage(k) + 0.02:
                return Text("Winner: P3", font_size=15, color=C["teal"], font="Times New Roman", weight=BOLD)
            else:
                return Text("Crossover!", font_size=15, color=C["highlight"], font="Times New Roman", weight=BOLD)

        winner_text = always_redraw(
            lambda: _get_winner_text().to_edge(DOWN, buff=0.8)
        )

        self.play(FadeIn(sweep_line), FadeIn(sweep_label), FadeIn(winner_text), run_time=0.3)
        self.play(k_tracker.animate.set_value(13.0), run_time=4.0, rate_func=linear)
        self.wait(1.0)

        # Final message
        final = Text(
            "K-Crossover: No universal best policy",
            font_size=22,
            color=C["highlight"],
            font="Times New Roman",
            weight=BOLD,
        ).to_edge(DOWN, buff=0.3)
        self.play(FadeOut(winner_text), Write(final), run_time=1.0)
        self.wait(2.0)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)
