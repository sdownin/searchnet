"""
AMR Paper — Utility Function & SCP Dynamics Animations
========================================================
Four scenes for the Academy of Management Review paper on industrial policy
and the 10-component utility function within the SaoMNK framework.

Scenes:
    1. TenComponentUtilityScene   — decompose utility into 10 mechanism bars
    2. SCPCouplingScene            — SCP linear chain → coupled 3-body system
    3. PolicyShockTimelineScene    — theta_shock natural experiments in K-4 space
    4. InstitutionalErosionScene   — W-matrix erosion raises landscape ruggedness

Requires: manim CE >= 0.18, numpy
Usage:
    manim -pql scene_amr_utility.py TenComponentUtilityScene
    manim -pql scene_amr_utility.py SCPCouplingScene
    manim -pql scene_amr_utility.py PolicyShockTimelineScene
    manim -pql scene_amr_utility.py InstitutionalErosionScene
"""

import numpy as np
from manim import *

# Import shared palette
try:
    from searchnet_viz import SEARCHNET_COLORS as C, BaseSearchnetScene
except ImportError:
    # Fallback palette if run standalone
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
        "K_AC":       "#2a9d8f",
        "K_CA":       "#e9c46a",
        "K_AA":       "#1b3a5c",
        "K_CC":       "#e76f51",
        "actor":      "#4fc3f7",
        "component":  "#e9c46a",
    }

    class BaseSearchnetScene(Scene):
        scene_title = "SearchNet Visualization"
        scene_subtitle = ""

        def setup(self):
            self.camera.background_color = C["bg_dark"]

        def add_persistent_title(self, title_text=None, font="Times New Roman"):
            title_text = title_text or self.scene_title
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


# ═══════════════════════════════════════════════════════════════════════
# COMPONENT DEFINITIONS — the 10-component utility function
# ═══════════════════════════════════════════════════════════════════════

COMPONENTS = [
    # (name,             short_label,  color,     beta_value, sign)
    ("NK Fitness",       "NK",         C["teal"],        1.00, +1),
    ("Scope Cost",       "Scope",      C["coral"],       0.50, -1),
    ("Synergy (XWX)",    "XWX",        C["amber"],       0.60, +1),
    ("Herding",          "Herd",       C["actor"],       0.30, +1),
    ("Congestion",       "Cong",       "#e05555",        0.35, -1),
    ("Displacement",     "Disp",       "#8b1a1a",        0.25, -1),
    ("Complementarity",  "Comp",       C["positive"],    0.30, +1),
    ("Closure",          "Clos",       C["plum"],        0.15, +1),
    ("Rivalry",          "Riv",        C["highlight"],   0.10, -1),
    ("Legitimacy",       "Leg",        "#ffd700",        0.20, +1),
]


# ═══════════════════════════════════════════════════════════════════════
# Scene 1: TenComponentUtilityScene
# ═══════════════════════════════════════════════════════════════════════

class TenComponentUtilityScene(BaseSearchnetScene):
    """
    Decompose a single firm's utility bar into 10 colored components.
    Each component appears one at a time, growing from the baseline.
    Final frame: all 10 stacked with a total-utility reference line.
    """

    def construct(self):
        # --- Title ---
        title = self.add_persistent_title("The 10-Component Utility Function")
        self.add_source("Downing (2026) — AMR Mathematical Appendix")

        # --- Phase 0: single aggregate bar ---
        total_height = 3.5
        bar_width = 1.0
        origin_y = -2.0
        origin_x = -4.5

        # Aggregate bar
        agg_bar = Rectangle(
            height=total_height, width=bar_width,
            fill_color=C["steel"], fill_opacity=0.7,
            stroke_color=C["text_light"], stroke_width=1,
        )
        agg_bar.move_to(
            np.array([origin_x, origin_y + total_height / 2, 0])
        )
        agg_label = Text(
            "Total\nUtility",
            font_size=16, color=C["text_light"],
        ).next_to(agg_bar, DOWN, buff=0.15)

        self.play(GrowFromEdge(agg_bar, DOWN), FadeIn(agg_label), run_time=1.0)
        self.wait(0.5)

        # --- Phase 1: decompose into 10 components ---
        # Compute component heights (absolute values, scaled)
        raw_heights = [beta for (_, _, _, beta, _) in COMPONENTS]
        scale = total_height / sum(raw_heights)
        comp_heights = [h * scale for h in raw_heights]

        # Build stacked bars from bottom up
        bar_x = origin_x + 2.5
        stacked_bars = VGroup()
        stacked_labels = VGroup()
        current_y = origin_y

        for i, (name, short, color, beta, sign) in enumerate(COMPONENTS):
            h = comp_heights[i]
            bar = Rectangle(
                height=h, width=bar_width * 0.9,
                fill_color=color, fill_opacity=0.85,
                stroke_color=WHITE, stroke_width=0.5,
            )
            bar.move_to(np.array([bar_x, current_y + h / 2, 0]))

            # Short label inside the bar (if tall enough) or to the right
            sign_str = "+" if sign > 0 else "−"
            lbl_text = f"{short} ({sign_str})"
            lbl = Text(lbl_text, font_size=11, color=WHITE, weight=BOLD)

            if h > 0.25:
                lbl.move_to(bar.get_center())
            else:
                lbl.next_to(bar, RIGHT, buff=0.1)

            stacked_bars.add(bar)
            stacked_labels.add(lbl)
            current_y += h

        # Detailed name labels on the right side
        detail_labels = VGroup()
        for i, (name, short, color, beta, sign) in enumerate(COMPONENTS):
            sign_str = "+" if sign > 0 else "−"
            dl = Text(
                f"{name}  β = {sign_str}{beta:.2f}",
                font_size=13, color=color,
            )
            detail_labels.add(dl)

        detail_labels.arrange(DOWN, buff=0.12, aligned_edge=LEFT)
        detail_labels.move_to(np.array([3.0, 0.0, 0]))

        # Animate: fade out aggregate, grow components one by one
        self.play(FadeOut(agg_bar), FadeOut(agg_label), run_time=0.5)

        for i in range(10):
            anims = [
                GrowFromEdge(stacked_bars[i], DOWN),
                FadeIn(stacked_labels[i]),
                FadeIn(detail_labels[i]),
            ]
            self.play(*anims, run_time=0.6)

        # --- Phase 2: total utility reference line ---
        total_line = DashedLine(
            start=np.array([bar_x - 0.8, current_y, 0]),
            end=np.array([bar_x + 0.8, current_y, 0]),
            color=C["text_light"],
            stroke_width=2,
        )
        total_lbl = Text(
            "U_i(B_i)", font_size=16, color=C["text_light"],
        ).next_to(total_line, RIGHT, buff=0.1)

        # Baseline reference
        base_line = DashedLine(
            start=np.array([bar_x - 0.8, origin_y, 0]),
            end=np.array([bar_x + 0.8, origin_y, 0]),
            color=C["text_dim"],
            stroke_width=1,
        )
        base_lbl = Text(
            "0", font_size=14, color=C["text_dim"],
        ).next_to(base_line, LEFT, buff=0.1)

        self.play(
            Create(total_line), FadeIn(total_lbl),
            Create(base_line), FadeIn(base_lbl),
            run_time=0.8,
        )

        # --- Phase 3: bracket grouping ---
        bracket_info = Text(
            "Positive terms: NK + XWX + Herd + Comp + Clos + Leg\n"
            "Negative terms: Scope + Cong + Disp + Riv",
            font_size=12, color=C["text_dim"],
        ).to_edge(DOWN, buff=0.3)
        self.play(FadeIn(bracket_info), run_time=0.5)
        self.wait(2.5)


# ═══════════════════════════════════════════════════════════════════════
# Scene 2: SCPCouplingScene
# ═══════════════════════════════════════════════════════════════════════

class SCPCouplingScene(BaseSearchnetScene):
    """
    Three circles S, C, P transition from linear chain to bidirectional
    coupling to a stochastic 3-body orbital metaphor.
    """

    def construct(self):
        title = self.add_persistent_title("SCP as a Stochastic 3-Body Problem")
        self.add_source("cf. Bain (1956), Porter (1981), Downing (2026)")

        # --- Node setup ---
        radius = 0.55
        s_pos = LEFT * 3.5
        c_pos = ORIGIN
        p_pos = RIGHT * 3.5

        def make_node(label_text, color, pos):
            circ = Circle(radius=radius, fill_color=color, fill_opacity=0.8,
                          stroke_color=WHITE, stroke_width=2)
            circ.move_to(pos)
            label = Text(label_text, font_size=28, color=WHITE, weight=BOLD)
            label.move_to(pos)
            return VGroup(circ, label)

        s_node = make_node("S", C["teal"], s_pos)
        c_node = make_node("C", C["amber"], c_pos)
        p_node = make_node("P", C["coral"], p_pos)

        nodes = VGroup(s_node, c_node, p_node)

        # Sublabels
        s_sub = Text("Structure", font_size=14, color=C["text_dim"]).next_to(s_node, DOWN, buff=0.2)
        c_sub = Text("Conduct", font_size=14, color=C["text_dim"]).next_to(c_node, DOWN, buff=0.2)
        p_sub = Text("Performance", font_size=14, color=C["text_dim"]).next_to(p_node, DOWN, buff=0.2)

        self.play(
            *[FadeIn(n) for n in [s_node, c_node, p_node]],
            *[FadeIn(s) for s in [s_sub, c_sub, p_sub]],
            run_time=0.8,
        )

        # ── Phase 1: Linear chain S → C → P ──
        phase_label = Text(
            "Linear chain (Bain, 1956)",
            font_size=18, color=C["text_light"],
        ).to_edge(DOWN, buff=0.5)

        def make_arrow(start, end, color=WHITE, buff=0.6):
            return Arrow(
                start=start, end=end,
                color=color, stroke_width=3,
                buff=buff, max_tip_length_to_length_ratio=0.15,
            )

        arrow_sc = make_arrow(s_pos, c_pos)
        arrow_cp = make_arrow(c_pos, p_pos)

        self.play(
            GrowArrow(arrow_sc), GrowArrow(arrow_cp),
            FadeIn(phase_label),
            run_time=1.0,
        )
        self.wait(1.5)

        # ── Phase 2: Bidirectional — all 6 feedback channels ──
        phase_label_2 = Text(
            "Coupled system: 6 feedback channels",
            font_size=18, color=C["text_light"],
        ).to_edge(DOWN, buff=0.5)

        # Reverse arrows (offset slightly to avoid overlap)
        offset = UP * 0.15
        arrow_cs = make_arrow(c_pos + offset, s_pos + offset, color=C["teal"])
        arrow_pc = make_arrow(p_pos + offset, c_pos + offset, color=C["amber"])

        # Diagonal arrows S<->P (arced via offset)
        diag_up = UP * 1.2
        arrow_sp = Arrow(
            start=s_pos + UP * 0.6, end=p_pos + UP * 0.6,
            color=C["highlight"], stroke_width=2, buff=0.6,
            max_tip_length_to_length_ratio=0.1,
        )
        arrow_ps = Arrow(
            start=p_pos + UP * 0.9, end=s_pos + UP * 0.9,
            color=C["coral"], stroke_width=2, buff=0.6,
            max_tip_length_to_length_ratio=0.1,
        )

        # Move existing arrows down slightly
        arrow_sc_low = make_arrow(s_pos - offset, c_pos - offset)
        arrow_cp_low = make_arrow(c_pos - offset, p_pos - offset)

        self.play(
            ReplacementTransform(arrow_sc, arrow_sc_low),
            ReplacementTransform(arrow_cp, arrow_cp_low),
            GrowArrow(arrow_cs), GrowArrow(arrow_pc),
            GrowArrow(arrow_sp), GrowArrow(arrow_ps),
            ReplacementTransform(phase_label, phase_label_2),
            run_time=1.2,
        )
        self.wait(1.5)

        # ── Phase 3: Orbital 3-body metaphor ──
        phase_label_3 = Text(
            "Stochastic 3-body problem: S, C, P co-evolve",
            font_size=18, color=C["text_light"],
        ).to_edge(DOWN, buff=0.5)

        # Fade out all arrows
        all_arrows = VGroup(
            arrow_sc_low, arrow_cp_low, arrow_cs, arrow_pc, arrow_sp, arrow_ps,
        )

        self.play(
            FadeOut(all_arrows),
            FadeOut(s_sub), FadeOut(c_sub), FadeOut(p_sub),
            ReplacementTransform(phase_label_2, phase_label_3),
            run_time=0.8,
        )

        # Move nodes to triangle arrangement for orbit
        orbit_center = DOWN * 0.3
        orbit_radius = 1.8
        angles_init = [np.pi / 2, np.pi / 2 + 2 * np.pi / 3, np.pi / 2 + 4 * np.pi / 3]

        target_positions = [
            orbit_center + orbit_radius * np.array([np.cos(a), np.sin(a), 0])
            for a in angles_init
        ]

        self.play(
            s_node.animate.move_to(target_positions[0]),
            c_node.animate.move_to(target_positions[1]),
            p_node.animate.move_to(target_positions[2]),
            run_time=1.0,
        )

        # Draw faint orbit path
        orbit_path = Circle(radius=orbit_radius, color=C["text_dim"],
                            stroke_width=1, stroke_opacity=0.4)
        orbit_path.move_to(orbit_center)
        self.play(Create(orbit_path), run_time=0.5)

        # Orbital rotation — rotate all three nodes around center
        orbit_group = VGroup(s_node, c_node, p_node)

        # Animate 1.5 full rotations
        self.play(
            Rotate(orbit_group, angle=3 * PI, about_point=orbit_center),
            run_time=6.0,
            rate_func=linear,
        )

        # Final label
        final_label = Text(
            "SCP(t): Structure, Conduct, and Performance co-evolve",
            font_size=16, color=C["text_light"], slant=ITALIC,
        ).next_to(phase_label_3, UP, buff=0.15)
        self.play(FadeIn(final_label), run_time=0.5)
        self.wait(1.5)


# ═══════════════════════════════════════════════════════════════════════
# Scene 3: PolicyShockTimelineScene
# ═══════════════════════════════════════════════════════════════════════

class PolicyShockTimelineScene(BaseSearchnetScene):
    """
    Timeline showing K-4 trajectories across Normal → Policy Active → Post-Policy
    phases, with vertical transition markers.
    """

    def construct(self):
        title = self.add_persistent_title("Policy Shocks as Natural Experiments")
        self.add_source("θ-shocks create natural experiments within simulations")

        # --- Axes ---
        ax = Axes(
            x_range=[0, 500, 100],
            y_range=[0, 1.0, 0.2],
            x_length=10,
            y_length=4.5,
            axis_config={
                "color": C["text_dim"],
                "stroke_width": 1.5,
                "include_ticks": True,
                "tick_size": 0.05,
                "font_size": 18,
            },
            tips=False,
        ).shift(DOWN * 0.3)

        x_label = Text("Simulation Steps", font_size=14, color=C["text_dim"])
        x_label.next_to(ax.x_axis, DOWN, buff=0.2)
        y_label = Text("K value", font_size=14, color=C["text_dim"])
        y_label.next_to(ax.y_axis, LEFT, buff=0.15).rotate(PI / 2)

        self.play(Create(ax), FadeIn(x_label), FadeIn(y_label), run_time=1.0)

        # --- Phase regions ---
        phase_colors = [C["positive"], C["highlight"], C["teal"]]
        phase_names = ["Normal", "Policy Active", "Post-Policy"]
        phase_ranges = [(0, 100), (100, 300), (300, 500)]

        for (x0, x1), name, color in zip(phase_ranges, phase_names, phase_colors):
            # Shaded region
            x0_scene = ax.c2p(x0, 0)[0]
            x1_scene = ax.c2p(x1, 0)[0]
            y_bot = ax.c2p(0, 0)[1]
            y_top = ax.c2p(0, 1.0)[1]

            rect = Rectangle(
                width=x1_scene - x0_scene,
                height=y_top - y_bot,
                fill_color=color,
                fill_opacity=0.08,
                stroke_width=0,
            )
            rect.move_to(np.array([(x0_scene + x1_scene) / 2, (y_bot + y_top) / 2, 0]))
            self.add(rect)

            lbl = Text(name, font_size=12, color=color)
            lbl.move_to(np.array([(x0_scene + x1_scene) / 2, y_top + 0.15, 0]))
            self.play(FadeIn(lbl), run_time=0.3)

        # --- Vertical transition lines ---
        for x_val in [100, 300]:
            vline = DashedLine(
                start=ax.c2p(x_val, 0),
                end=ax.c2p(x_val, 1.0),
                color=WHITE,
                stroke_width=1.5,
                dash_length=0.08,
            )
            self.play(Create(vline), run_time=0.3)

            # Flash effect: expanding circle at transition
            flash_center = ax.c2p(x_val, 0.5)
            flash_circle = Circle(
                radius=0.05, color=WHITE, fill_opacity=0.8, stroke_width=0,
            ).move_to(flash_center)
            self.play(
                flash_circle.animate.scale(8).set_opacity(0),
                run_time=0.4,
            )
            self.remove(flash_circle)

        # --- K-4 trajectory data (synthetic) ---
        np.random.seed(42)
        steps = np.arange(0, 501, 1)

        def smooth_trajectory(base_phase1, slope_phase2, base_phase3, noise=0.01):
            """Generate a 3-phase trajectory with smooth transitions."""
            y = np.zeros_like(steps, dtype=float)
            for i, t in enumerate(steps):
                if t <= 100:
                    y[i] = base_phase1 + 0.0005 * t
                elif t <= 300:
                    dt = t - 100
                    y[i] = base_phase1 + 0.05 + slope_phase2 * dt / 200
                else:
                    dt = t - 300
                    y[i] = base_phase3 + 0.0003 * dt
            # Smooth
            kernel = np.ones(10) / 10
            y = np.convolve(y, kernel, mode="same")
            y += np.random.normal(0, noise, len(y))
            return np.clip(y, 0.02, 0.98)

        k_data = {
            "K_AC": smooth_trajectory(0.35, 0.20, 0.58, 0.008),
            "K_CA": smooth_trajectory(0.50, -0.15, 0.38, 0.010),
            "K_AA": smooth_trajectory(0.20, 0.10, 0.32, 0.006),
            "K_CC": smooth_trajectory(0.60, -0.25, 0.40, 0.012),
        }

        k_colors = {
            "K_AC": C["K_AC"],
            "K_CA": C["K_CA"],
            "K_AA": C["K_AA"],
            "K_CC": C["K_CC"],
        }

        # --- Animate K lines one at a time ---
        legend_items = VGroup()
        for idx, (k_name, y_vals) in enumerate(k_data.items()):
            points = [ax.c2p(steps[j], y_vals[j]) for j in range(0, len(steps), 3)]
            line = VMobject(color=k_colors[k_name], stroke_width=2.5)
            line.set_points_smoothly(points)

            # Legend entry
            leg_dot = Dot(color=k_colors[k_name], radius=0.06)
            leg_text = Text(k_name, font_size=13, color=k_colors[k_name])
            leg_entry = VGroup(leg_dot, leg_text).arrange(RIGHT, buff=0.1)
            legend_items.add(leg_entry)

            self.play(Create(line), run_time=1.2)

        legend_items.arrange(DOWN, buff=0.1, aligned_edge=LEFT)
        legend_items.to_corner(UR, buff=0.6).shift(DOWN * 0.5)
        legend_bg = SurroundingRectangle(
            legend_items, color=C["text_dim"], fill_color=C["bg_panel"],
            fill_opacity=0.8, buff=0.12, corner_radius=0.05,
        )
        self.play(FadeIn(legend_bg), FadeIn(legend_items), run_time=0.5)

        # --- Bottom annotation ---
        annotation = Text(
            "θ-shocks create natural experiments within simulations",
            font_size=14, color=C["text_dim"], slant=ITALIC,
        ).to_edge(DOWN, buff=0.25)
        self.play(FadeIn(annotation), run_time=0.5)
        self.wait(2.0)


# ═══════════════════════════════════════════════════════════════════════
# Scene 4: InstitutionalErosionScene
# ═══════════════════════════════════════════════════════════════════════

class InstitutionalErosionScene(BaseSearchnetScene):
    """
    Three W-matrix heatmaps morph Normal → Eroding → Eroded.
    A 'Landscape Ruggedness' meter rises as institutional structure degrades.
    """

    def construct(self):
        title = self.add_persistent_title("Institutional Erosion Increases Landscape Complexity")
        self.add_source("W-matrix degradation → NK ruggedness rise")

        # --- Generate synthetic W matrices ---
        np.random.seed(123)
        n = 12  # matrix size

        # Normal: clean block-diagonal structure
        w_normal = np.zeros((n, n))
        blocks = [(0, 4), (4, 8), (8, 12)]
        for s, e in blocks:
            w_normal[s:e, s:e] = np.random.uniform(0.6, 1.0, (e - s, e - s))
        np.fill_diagonal(w_normal, 0)
        # Small off-block noise
        w_normal += np.random.uniform(0, 0.08, (n, n))
        np.fill_diagonal(w_normal, 0)
        w_normal = np.clip(w_normal, 0, 1)

        # Eroding: blocks becoming fuzzy
        w_eroding = w_normal.copy()
        cross_noise = np.random.uniform(0, 0.35, (n, n))
        w_eroding = 0.6 * w_eroding + 0.4 * cross_noise
        np.fill_diagonal(w_eroding, 0)
        w_eroding = np.clip(w_eroding, 0, 1)

        # Eroded: nearly random
        w_eroded = np.random.uniform(0.15, 0.65, (n, n))
        np.fill_diagonal(w_eroded, 0)

        # --- Build heatmap mobjects ---
        cell_size = 0.28
        heatmap_center = LEFT * 1.5 + DOWN * 0.3

        def matrix_to_heatmap(mat, center):
            """Create a VGroup of colored squares representing a matrix."""
            group = VGroup()
            half = n * cell_size / 2
            for i in range(n):
                for j in range(n):
                    val = mat[i, j]
                    # Interpolate: low=dark blue, high=bright amber
                    r = int(42 + val * (233 - 42))
                    g = int(26 + val * (196 - 26))
                    b = int(92 + val * (106 - 92))
                    color = rgb_to_color([r / 255, g / 255, b / 255])

                    sq = Square(
                        side_length=cell_size,
                        fill_color=color,
                        fill_opacity=0.9,
                        stroke_width=0.3,
                        stroke_color=C["bg_dark"],
                    )
                    x = center[0] - half + j * cell_size + cell_size / 2
                    y = center[1] + half - i * cell_size - cell_size / 2
                    sq.move_to(np.array([x, y, 0]))
                    group.add(sq)
            return group

        hm_normal = matrix_to_heatmap(w_normal, heatmap_center)

        # Stage labels
        stage_labels = ["Normal", "Eroding", "Eroded"]
        stage_label = Text(
            stage_labels[0], font_size=20, color=C["text_light"],
        ).next_to(hm_normal, DOWN, buff=0.25)

        # --- Ruggedness meter ---
        meter_x = 3.5
        meter_bottom = -2.5
        meter_height = 4.0
        meter_width = 0.6

        meter_bg = Rectangle(
            height=meter_height, width=meter_width,
            fill_color=C["bg_panel"], fill_opacity=0.8,
            stroke_color=C["text_dim"], stroke_width=1,
        ).move_to(np.array([meter_x, meter_bottom + meter_height / 2, 0]))

        meter_label = Text(
            "Landscape\nRuggedness",
            font_size=13, color=C["text_light"],
        ).next_to(meter_bg, RIGHT, buff=0.15)

        # Meter fill bar (starts low)
        ruggedness_levels = [0.15, 0.50, 0.90]
        fill_height = ruggedness_levels[0] * meter_height
        meter_fill = Rectangle(
            height=fill_height, width=meter_width * 0.8,
            fill_color=C["teal"], fill_opacity=0.9,
            stroke_width=0,
        )
        meter_fill.move_to(
            np.array([meter_x, meter_bottom + fill_height / 2, 0])
        )

        # Low / High labels
        low_lbl = Text("Low", font_size=11, color=C["text_dim"]).next_to(meter_bg, DOWN, buff=0.08)
        high_lbl = Text("High", font_size=11, color=C["text_dim"]).next_to(meter_bg, UP, buff=0.08)

        # --- Animate ---
        self.play(
            FadeIn(hm_normal), FadeIn(stage_label),
            FadeIn(meter_bg), FadeIn(meter_label),
            FadeIn(meter_fill), FadeIn(low_lbl), FadeIn(high_lbl),
            run_time=1.0,
        )
        self.wait(1.0)

        # Transition 1: Normal → Eroding
        hm_eroding = matrix_to_heatmap(w_eroding, heatmap_center)
        stage_label_2 = Text(
            stage_labels[1], font_size=20, color=C["highlight"],
        ).next_to(hm_normal, DOWN, buff=0.25)

        fill_height_2 = ruggedness_levels[1] * meter_height
        meter_fill_2 = Rectangle(
            height=fill_height_2, width=meter_width * 0.8,
            fill_color=C["highlight"], fill_opacity=0.9,
            stroke_width=0,
        ).move_to(np.array([meter_x, meter_bottom + fill_height_2 / 2, 0]))

        self.play(
            *[
                Transform(hm_normal[i], hm_eroding[i])
                for i in range(len(hm_normal))
            ],
            ReplacementTransform(stage_label, stage_label_2),
            ReplacementTransform(meter_fill, meter_fill_2),
            run_time=2.0,
        )
        self.wait(1.0)

        # Transition 2: Eroding → Eroded
        hm_eroded = matrix_to_heatmap(w_eroded, heatmap_center)
        stage_label_3 = Text(
            stage_labels[2], font_size=20, color=C["coral"],
        ).next_to(hm_normal, DOWN, buff=0.25)

        fill_height_3 = ruggedness_levels[2] * meter_height
        meter_fill_3 = Rectangle(
            height=fill_height_3, width=meter_width * 0.8,
            fill_color=C["coral"], fill_opacity=0.9,
            stroke_width=0,
        ).move_to(np.array([meter_x, meter_bottom + fill_height_3 / 2, 0]))

        self.play(
            *[
                Transform(hm_normal[i], hm_eroded[i])
                for i in range(len(hm_normal))
            ],
            ReplacementTransform(stage_label_2, stage_label_3),
            ReplacementTransform(meter_fill_2, meter_fill_3),
            run_time=2.0,
        )
        self.wait(1.0)

        # Final annotation
        final_note = Text(
            "Institutional erosion increases landscape complexity",
            font_size=14, color=C["text_dim"], slant=ITALIC,
        ).to_edge(DOWN, buff=0.2)
        self.play(FadeIn(final_note), run_time=0.5)
        self.wait(2.0)
