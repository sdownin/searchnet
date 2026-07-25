"""
AMR Mathematical Appendix — Dynamic Model Animations
=====================================================
Five scenes illustrating the Ising-model correspondence, hysteresis,
ELO tournament ranking, replicator dynamics on the simplex, and the
Fitness Parity Paradox for the AMR industrial-policy paper.

Render examples (manim CE 0.20):
    manim -pql scene_amr_dynamics.py IsingCorrespondenceScene
    manim -pql scene_amr_dynamics.py HysteresisScene
    manim -pql scene_amr_dynamics.py EloTournamentScene
    manim -pql scene_amr_dynamics.py ReplicatorDynamicsScene
    manim -pql scene_amr_dynamics.py FitnessParityParadoxScene
"""

import numpy as np
from manim import *

# ── Import shared palette and base class ──────────────────────────────
try:
    from searchnet_viz import SEARCHNET_COLORS as C, BaseSearchnetScene
except ImportError:
    # Fallback: inline the palette so the file is self-contained
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
        "actor":      "#4fc3f7",
        "component":  "#e9c46a",
    }

    class BaseSearchnetScene(Scene):
        scene_title = "SearchNet Visualization"
        scene_subtitle = ""

        def setup(self):
            self.camera.background_color = C["bg_dark"]

        def add_title(self, title_text=None, subtitle_text=None, font="Times New Roman"):
            title_text = title_text or self.scene_title
            subtitle_text = subtitle_text or self.scene_subtitle
            title = Text(title_text, font_size=36, color=C["text_light"],
                         font=font, weight=BOLD)
            sub = None
            if subtitle_text:
                sub = Text(subtitle_text, font_size=18, color=C["text_dim"],
                           font=font, slant=ITALIC)
                sub.next_to(title, DOWN, buff=0.15)
                group = VGroup(title, sub).move_to(ORIGIN)
                self.play(Write(title), run_time=0.8)
                self.play(FadeIn(sub), run_time=0.4)
                self.wait(1)
                self.play(FadeOut(group), run_time=0.5)
            else:
                title.move_to(ORIGIN)
                self.play(Write(title), run_time=0.8)
                self.wait(1)
                self.play(FadeOut(title), run_time=0.5)
            return title, sub

        def add_persistent_title(self, title_text=None, font="Times New Roman"):
            title_text = title_text or self.scene_title
            title = Text(title_text, font_size=28, color=C["text_light"],
                         font=font, weight=BOLD).to_edge(UP, buff=0.3)
            self.play(FadeIn(title), run_time=0.5)
            return title

        def add_source(self, source_text, font_size=11):
            source = Text(source_text, font_size=font_size,
                          color=C["text_dim"], slant=ITALIC).to_corner(DR, buff=0.25)
            self.add(source)
            return source


# ── Policy colors used across scenes ──────────────────────────────────
POLICY_COLORS = [C["teal"], C["amber"], C["coral"], C["plum"]]
POLICY_LABELS = ["P1: Subsidy", "P2: R&D Grant", "P3: Tax Conc.", "P4: Guarantee"]


# =====================================================================
# Scene 1 — Ising Correspondence
# =====================================================================
class IsingCorrespondenceScene(BaseSearchnetScene):
    """
    Left: 8x8 Ising spin grid (red up / blue down).
    Right: bipartite activity matrix (firms x activities).
    Animate the mapping, then build a correspondence table row by row.
    """
    scene_title = "Ising-Model Correspondence"
    scene_subtitle = "Mapping spins to firm-activity ties"

    def construct(self):
        self.add_title()

        rng = np.random.default_rng(7)
        N = 8
        spins = rng.choice([-1, 1], size=(N, N))

        # ── Left panel: spin grid ─────────────────────────────────────
        spin_squares = VGroup()
        for i in range(N):
            for j in range(N):
                color = C["coral"] if spins[i, j] == 1 else C["navy"]
                sq = Square(side_length=0.35, fill_opacity=0.85,
                            fill_color=color, stroke_width=0.5,
                            stroke_color=C["steel"])
                sq.move_to(np.array([-4.0 + j * 0.4, 2.0 - i * 0.4, 0]))
                spin_squares.add(sq)

        spin_label = Text("Ising Spin Lattice", font_size=18,
                          color=C["text_dim"]).next_to(spin_squares, UP, buff=0.25)

        # ── Right panel: activity matrix ──────────────────────────────
        firms = 4
        activities = 4
        B = (spins[:firms, :activities] + 1) // 2  # 0/1

        mat_squares = VGroup()
        for i in range(firms):
            for j in range(activities):
                color = C["teal"] if B[i, j] == 1 else C["bg_panel"]
                sq = Square(side_length=0.6, fill_opacity=0.85,
                            fill_color=color, stroke_width=0.8,
                            stroke_color=C["steel"])
                sq.move_to(np.array([1.5 + j * 0.7, 2.0 - i * 0.7, 0]))
                mat_squares.add(sq)

        # Row / col labels
        firm_labels = VGroup(*[
            Text(f"F{i+1}", font_size=14, color=C["text_dim"]).move_to(
                np.array([0.7, 2.0 - i * 0.7, 0]))
            for i in range(firms)
        ])
        act_labels = VGroup(*[
            Text(f"A{j+1}", font_size=14, color=C["text_dim"]).move_to(
                np.array([1.5 + j * 0.7, 2.7, 0]))
            for j in range(activities)
        ])
        mat_label = Text("Firm-Activity Matrix  B", font_size=18,
                         color=C["text_dim"]).next_to(
            VGroup(mat_squares, act_labels), UP, buff=0.25)

        # Animate grids in
        self.play(FadeIn(spin_squares, shift=LEFT * 0.3),
                  FadeIn(spin_label), run_time=0.8)
        self.play(FadeIn(mat_squares, shift=RIGHT * 0.3),
                  FadeIn(firm_labels), FadeIn(act_labels),
                  FadeIn(mat_label), run_time=0.8)

        # ── Arrows connecting some spins to matrix cells ──────────────
        arrows = VGroup()
        for idx in range(4):
            i, j = idx, idx
            start = spin_squares[i * N + j].get_right()
            end = mat_squares[i * activities + j].get_left()
            arr = Arrow(start, end, buff=0.08, stroke_width=2,
                        color=C["highlight"], max_tip_length_to_length_ratio=0.15)
            arrows.add(arr)
        self.play(LaggedStart(*[GrowArrow(a) for a in arrows],
                              lag_ratio=0.25), run_time=1.2)
        self.wait(0.5)

        # ── Correspondence table ──────────────────────────────────────
        rows = [
            ("Spin  \u03c3",           "Activity  B\u1d62\u2c7c"),
            ("External field  h",  "Policy  \u0394\u03b2"),
            ("Coupling  J",        "W-matrix"),
            ("Temperature  T",     "Rationality  1/\u03b2"),
        ]

        table_group = VGroup()
        header_bg = Rectangle(width=5.6, height=0.4, fill_color=C["slate"],
                              fill_opacity=0.6, stroke_width=0)
        header_bg.move_to(np.array([0, -1.6, 0]))
        hdr_l = Text("Ising", font_size=16, color=C["amber"],
                      weight=BOLD).move_to(header_bg.get_left() + RIGHT * 1.2)
        hdr_r = Text("SaoMNK", font_size=16, color=C["teal"],
                      weight=BOLD).move_to(header_bg.get_right() + LEFT * 1.2)
        table_group.add(header_bg, hdr_l, hdr_r)

        self.play(FadeIn(table_group), run_time=0.4)

        for k, (left_txt, right_txt) in enumerate(rows):
            y = -2.1 - k * 0.4
            row_bg = Rectangle(width=5.6, height=0.35,
                               fill_color=C["bg_panel"],
                               fill_opacity=0.4 if k % 2 == 0 else 0.2,
                               stroke_width=0)
            row_bg.move_to(np.array([0, y, 0]))
            lt = Text(left_txt, font_size=14,
                      color=C["text_light"]).move_to(row_bg.get_left() + RIGHT * 1.2)
            rt = Text(right_txt, font_size=14,
                      color=C["text_light"]).move_to(row_bg.get_right() + LEFT * 1.2)
            self.play(FadeIn(row_bg), FadeIn(lt), FadeIn(rt), run_time=0.35)

        self.wait(1.5)


# =====================================================================
# Scene 2 — Hysteresis
# =====================================================================
class HysteresisScene(BaseSearchnetScene):
    """
    Magnetization-vs-temperature plot with forward and backward sweeps,
    shaded hysteresis area, and policy-dependence annotation.
    """
    scene_title = "Hysteresis in Policy Adoption"
    scene_subtitle = "Path dependence from Ising dynamics"

    def construct(self):
        self.add_title()
        ptitle = self.add_persistent_title("Hysteresis: Path Dependence of Policy")

        # ── Axes ──────────────────────────────────────────────────────
        ax = Axes(
            x_range=[0, 5, 1], y_range=[-1.1, 1.1, 0.5],
            x_length=8, y_length=4.5,
            axis_config={"color": C["steel"], "include_tip": True,
                         "tip_length": 0.15},
            x_axis_config={"include_numbers": False},
            y_axis_config={"include_numbers": False},
        ).shift(DOWN * 0.3)

        x_lab = Text("Temperature  T", font_size=16,
                      color=C["text_dim"]).next_to(ax.x_axis, DOWN, buff=0.25)
        y_lab = Text("Magnetization  m", font_size=16,
                      color=C["text_dim"]).next_to(ax.y_axis, LEFT, buff=0.25).rotate(PI / 2)

        self.play(Create(ax), FadeIn(x_lab), FadeIn(y_lab), run_time=0.8)

        # ── Synthetic magnetization curves ────────────────────────────
        T = np.linspace(0.01, 5, 200)

        # Forward sweep (cooling -> heating): stays high then drops
        Tc_fwd = 3.2
        m_fwd = np.tanh(1.5 * (Tc_fwd - T))
        m_fwd = np.clip(m_fwd, -1, 1)

        # Backward sweep (heating -> cooling): stays low then rises
        Tc_bwd = 1.8
        m_bwd = np.tanh(1.5 * (Tc_bwd - T))
        m_bwd = np.clip(m_bwd, -1, 1)

        fwd_curve = ax.plot_line_graph(
            T, m_fwd, add_vertex_dots=False,
            line_color=C["coral"], stroke_width=3,
        )
        bwd_curve = ax.plot_line_graph(
            T, m_bwd, add_vertex_dots=False,
            line_color=C["teal"], stroke_width=3,
        )

        fwd_label = Text("Forward (heating)", font_size=14,
                         color=C["coral"]).next_to(
            ax.c2p(4.2, 0.3), RIGHT, buff=0.1)
        bwd_label = Text("Backward (cooling)", font_size=14,
                         color=C["teal"]).next_to(
            ax.c2p(4.2, -0.3), RIGHT, buff=0.1)

        # Animate forward sweep
        self.play(Create(fwd_curve["line_graph"]), FadeIn(fwd_label), run_time=2.0)
        self.wait(0.3)

        # Animate backward sweep
        self.play(Create(bwd_curve["line_graph"]), FadeIn(bwd_label), run_time=2.0)
        self.wait(0.3)

        # ── Shade hysteresis area ─────────────────────────────────────
        # Build polygon from forward top + backward bottom
        n_pts = 100
        T_fill = np.linspace(0.01, 5, n_pts)
        m_fwd_fill = np.tanh(1.5 * (Tc_fwd - T_fill))
        m_bwd_fill = np.tanh(1.5 * (Tc_bwd - T_fill))

        poly_pts = []
        for i in range(n_pts):
            poly_pts.append(ax.c2p(T_fill[i], m_fwd_fill[i]))
        for i in range(n_pts - 1, -1, -1):
            poly_pts.append(ax.c2p(T_fill[i], m_bwd_fill[i]))

        hysteresis_poly = Polygon(
            *poly_pts, fill_color=C["amber"], fill_opacity=0.3,
            stroke_width=0,
        )
        self.play(FadeIn(hysteresis_poly), run_time=1.0)

        caption = Text("The history of policy matters", font_size=20,
                       color=C["amber"], weight=BOLD).to_edge(DOWN, buff=0.4)
        self.play(Write(caption), run_time=0.8)
        self.wait(2)


# =====================================================================
# Scene 3 — ELO Tournament
# =====================================================================
class EloTournamentScene(BaseSearchnetScene):
    """
    Four policy-type bars start at ELO 1500. Round-robin matches flash
    pairwise, winner rises, loser falls. After 10 rounds the ranking
    stabilises and a formula annotation appears.
    """
    scene_title = "ELO Tournament Ranking"
    scene_subtitle = "Policy survival through competition"

    def construct(self):
        self.add_title()
        ptitle = self.add_persistent_title("ELO Tournament: Policy Competition")

        rng = np.random.default_rng(42)

        # ── ELO parameters ────────────────────────────────────────────
        n_policies = 4
        K_elo = 32
        elo = np.full(n_policies, 1500.0)

        # True strengths (P3 is best, P4 close second)
        strengths = np.array([0.40, 0.45, 0.70, 0.60])

        # ── Bar chart setup ───────────────────────────────────────────
        bar_width = 1.0
        spacing = 1.6
        base_y = -2.5
        scale = 0.012  # elo -> scene units

        def bar_height(e):
            return (e - 1300) * scale

        bars = VGroup()
        bar_labels = VGroup()
        elo_texts = VGroup()
        for i in range(n_policies):
            h = bar_height(elo[i])
            bar = Rectangle(width=bar_width, height=h,
                            fill_color=POLICY_COLORS[i], fill_opacity=0.85,
                            stroke_color=WHITE, stroke_width=0.5)
            bar.move_to(np.array([-3 + i * spacing, base_y + h / 2, 0]))
            bars.add(bar)

            lbl = Text(f"P{i+1}", font_size=18, color=C["text_light"],
                       weight=BOLD).next_to(bar, DOWN, buff=0.15)
            bar_labels.add(lbl)

            etxt = Text(str(int(elo[i])), font_size=14,
                        color=C["text_light"]).next_to(bar, UP, buff=0.1)
            elo_texts.add(etxt)

        self.play(LaggedStart(*[GrowFromEdge(b, DOWN) for b in bars],
                              lag_ratio=0.15),
                  FadeIn(bar_labels), run_time=1.0)
        self.play(FadeIn(elo_texts), run_time=0.3)

        # ── Round-robin matches ───────────────────────────────────────
        round_label = Text("Round 1", font_size=20,
                           color=C["text_dim"]).to_edge(RIGHT, buff=0.6).shift(UP * 1.5)
        self.play(FadeIn(round_label), run_time=0.3)

        pairs = [(a, b) for a in range(n_policies)
                 for b in range(a + 1, n_policies)]

        for rd in range(1, 11):
            new_rl = Text(f"Round {rd}", font_size=20,
                          color=C["text_dim"]).move_to(round_label)
            self.play(Transform(round_label, new_rl), run_time=0.15)

            rng.shuffle(pairs)
            for a, b in pairs[:3]:  # 3 matches per round for pacing
                # Expected scores
                Ea = 1.0 / (1.0 + 10 ** ((elo[b] - elo[a]) / 400.0))
                Eb = 1.0 - Ea

                # Outcome based on true strength
                prob_a = strengths[a] / (strengths[a] + strengths[b])
                Sa = 1.0 if rng.random() < prob_a else 0.0
                Sb = 1.0 - Sa

                elo[a] += K_elo * (Sa - Ea)
                elo[b] += K_elo * (Sb - Eb)

                # Flash match highlight
                highlight_a = bars[a].copy().set_stroke(color=C["highlight"], width=3)
                highlight_b = bars[b].copy().set_stroke(color=C["highlight"], width=3)
                self.add(highlight_a, highlight_b)
                self.wait(0.08)
                self.remove(highlight_a, highlight_b)

            # Update bars
            anims = []
            for i in range(n_policies):
                h = max(bar_height(elo[i]), 0.15)
                new_bar = Rectangle(
                    width=bar_width, height=h,
                    fill_color=POLICY_COLORS[i], fill_opacity=0.85,
                    stroke_color=WHITE, stroke_width=0.5,
                )
                new_bar.move_to(np.array([-3 + i * spacing, base_y + h / 2, 0]))
                anims.append(Transform(bars[i], new_bar))

                new_etxt = Text(str(int(elo[i])), font_size=14,
                                color=C["text_light"]).next_to(new_bar, UP, buff=0.1)
                anims.append(Transform(elo_texts[i], new_etxt))

            self.play(*anims, run_time=0.35)

        # ── ELO formula ───────────────────────────────────────────────
        formula = MathTex(
            r"R_{\text{new}} = R + K\,(S - E)",
            font_size=28, color=C["text_light"],
        ).to_edge(RIGHT, buff=0.5).shift(DOWN * 0.5)
        formula_box = SurroundingRectangle(formula, color=C["slate"],
                                           buff=0.15, corner_radius=0.08,
                                           fill_color=C["bg_panel"],
                                           fill_opacity=0.7)
        self.play(FadeIn(formula_box), Write(formula), run_time=0.8)

        # ── Highlight winner ──────────────────────────────────────────
        winner = int(np.argmax(elo))
        crown = Text("\u2605", font_size=28, color=C["amber"]).next_to(
            bars[winner], UP, buff=0.35)
        caption = Text("Tournament reveals which policy survives competition",
                       font_size=18, color=C["highlight"]).to_edge(DOWN, buff=0.35)
        self.play(FadeIn(crown), Write(caption), run_time=0.8)
        self.wait(2)


# =====================================================================
# Scene 4 — Replicator Dynamics on the Simplex
# =====================================================================
class ReplicatorDynamicsScene(BaseSearchnetScene):
    """
    Ternary simplex plot showing trajectories of three policy-type population
    shares under LOW vs HIGH erosion.
    """
    scene_title = "Replicator Dynamics"
    scene_subtitle = "Erosion shifts the evolutionarily stable strategy"

    @staticmethod
    def bary_to_cart(p, scale=4.5):
        """Convert barycentric (p1, p2, p3) to 2D Cartesian."""
        # Vertices of equilateral triangle
        v0 = np.array([-scale / 2, -scale * np.sqrt(3) / 6, 0])
        v1 = np.array([scale / 2, -scale * np.sqrt(3) / 6, 0])
        v2 = np.array([0, scale * np.sqrt(3) / 3, 0])
        return p[0] * v0 + p[1] * v1 + p[2] * v2

    def construct(self):
        self.add_title()
        ptitle = self.add_persistent_title("Replicator Dynamics on the Simplex")

        scale = 4.5
        v0 = self.bary_to_cart(np.array([1, 0, 0]), scale)
        v1 = self.bary_to_cart(np.array([0, 1, 0]), scale)
        v2 = self.bary_to_cart(np.array([0, 0, 1]), scale)

        # ── Triangle ──────────────────────────────────────────────────
        triangle = Polygon(v0, v1, v2, stroke_color=C["steel"],
                           stroke_width=2, fill_opacity=0)
        triangle.shift(DOWN * 0.3)

        corner_labels = VGroup(
            Text("P1: Subsidy", font_size=14,
                 color=C["teal"]).next_to(v0 + DOWN * 0.3, DOWN, buff=0.1),
            Text("P2: R&D Grant", font_size=14,
                 color=C["amber"]).next_to(v1 + DOWN * 0.3, DOWN, buff=0.1),
            Text("P3: Tax Conc.", font_size=14,
                 color=C["coral"]).next_to(v2 + DOWN * 0.3 + UP * 0.6, UP, buff=0.1),
        )
        # Shift everything down a bit
        simplex_group = VGroup(triangle, corner_labels).shift(DOWN * 0.3)

        self.play(Create(triangle), FadeIn(corner_labels), run_time=0.8)

        # ── Replicator dynamics simulation ────────────────────────────
        def replicator_step(x, payoff_matrix, dt=0.02):
            """One Euler step of replicator equation: dx_i/dt = x_i(f_i - f_bar)."""
            f = payoff_matrix @ x
            f_bar = x @ f
            dx = x * (f - f_bar) * dt
            x_new = x + dx
            x_new = np.clip(x_new, 1e-6, 1.0)
            x_new /= x_new.sum()
            return x_new

        # LOW erosion: P3 (Tax Concession / vertex 2) wins
        A_low = np.array([
            [0.0,  -0.1, -0.2],
            [0.1,   0.0, -0.15],
            [0.2,   0.15, 0.0],
        ])

        # HIGH erosion: P1 (Subsidy / vertex 0) wins — guarantees shift
        A_high = np.array([
            [0.0,   0.15, 0.2],
            [-0.15, 0.0,  0.1],
            [-0.2, -0.1,  0.0],
        ])

        n_steps = 250
        x0 = np.array([0.34, 0.33, 0.33])

        # Low erosion trajectory
        traj_low = [x0.copy()]
        x = x0.copy()
        for _ in range(n_steps):
            x = replicator_step(x, A_low)
            traj_low.append(x.copy())

        # High erosion trajectory
        traj_high = [x0.copy()]
        x = x0.copy()
        for _ in range(n_steps):
            x = replicator_step(x, A_high)
            traj_high.append(x.copy())

        def traj_to_points(traj):
            pts = []
            for p in traj:
                cart = self.bary_to_cart(p, scale)
                pts.append(cart + DOWN * 0.6)  # match simplex shift
            return pts

        pts_low = traj_to_points(traj_low)
        pts_high = traj_to_points(traj_high)

        # ── Draw trajectories ─────────────────────────────────────────
        path_low = VMobject(stroke_color=C["coral"], stroke_width=3)
        path_low.set_points_smoothly(pts_low[::3])

        path_high = VMobject(stroke_color=C["teal"], stroke_width=3)
        path_high.set_points_smoothly(pts_high[::3])

        # Starting dot
        start_dot = Dot(pts_low[0], radius=0.08, color=C["highlight"])
        self.play(FadeIn(start_dot), run_time=0.3)

        low_label = Text("Low erosion", font_size=14,
                         color=C["coral"]).move_to(
            np.array([2.5, 1.0, 0]))
        high_label = Text("High erosion", font_size=14,
                          color=C["teal"]).move_to(
            np.array([-2.5, -2.5, 0]))

        self.play(Create(path_low), FadeIn(low_label), run_time=2.5)
        self.play(Create(path_high), FadeIn(high_label), run_time=2.5)

        # End dots
        end_low = Dot(pts_low[-1], radius=0.1, color=C["coral"])
        end_high = Dot(pts_high[-1], radius=0.1, color=C["teal"])
        self.play(FadeIn(end_low), FadeIn(end_high), run_time=0.4)

        caption = Text("Erosion shifts the evolutionarily stable strategy",
                       font_size=18, color=C["highlight"]).to_edge(DOWN, buff=0.3)
        self.play(Write(caption), run_time=0.8)
        self.wait(2)


# =====================================================================
# Scene 5 — Fitness Parity Paradox
# =====================================================================
class FitnessParityParadoxScene(BaseSearchnetScene):
    """
    Two panels: head-to-head bars show parity among 4 policy types,
    but a diversified portfolio bar exceeds any single policy.
    """
    scene_title = "Fitness Parity Paradox"
    scene_subtitle = "No single best, but diversification wins"

    def construct(self):
        self.add_title()
        ptitle = self.add_persistent_title("Fitness Parity Paradox")

        # ── Layout parameters ─────────────────────────────────────────
        panel_w = 5.0
        bar_w = 0.7
        max_h = 3.0
        base_y = -2.2

        # ── LEFT PANEL: Head-to-Head ──────────────────────────────────
        left_title = Text("Head-to-Head", font_size=20,
                          color=C["text_light"], weight=BOLD).move_to(
            np.array([-3.2, 1.8, 0]))
        left_border = Rectangle(width=panel_w, height=4.5,
                                stroke_color=C["slate"], stroke_width=1,
                                fill_opacity=0).move_to(np.array([-3.2, -0.2, 0]))

        self.play(FadeIn(left_border), Write(left_title), run_time=0.5)

        # Target fitness values (near parity)
        target_fitness = [0.78, 0.81, 0.83, 0.80]

        left_bars = VGroup()
        left_labels = VGroup()
        left_vals = VGroup()
        for i in range(4):
            h = target_fitness[i] * max_h
            bar = Rectangle(width=bar_w, height=0.05,
                            fill_color=POLICY_COLORS[i], fill_opacity=0.85,
                            stroke_width=0.5, stroke_color=WHITE)
            x = -4.5 + i * 0.95
            bar.move_to(np.array([x, base_y + 0.025, 0]))
            left_bars.add(bar)

            lbl = Text(f"P{i+1}", font_size=14,
                       color=C["text_dim"]).move_to(np.array([x, base_y - 0.3, 0]))
            left_labels.add(lbl)

        self.play(FadeIn(left_labels), run_time=0.3)

        # Animate bars growing to near-equal heights
        grow_anims = []
        for i in range(4):
            h = target_fitness[i] * max_h
            x = -4.5 + i * 0.95
            new_bar = Rectangle(width=bar_w, height=h,
                                fill_color=POLICY_COLORS[i], fill_opacity=0.85,
                                stroke_width=0.5, stroke_color=WHITE)
            new_bar.move_to(np.array([x, base_y + h / 2, 0]))
            grow_anims.append(Transform(left_bars[i], new_bar))

        self.play(*grow_anims, run_time=2.0)

        # Value labels
        for i in range(4):
            h = target_fitness[i] * max_h
            x = -4.5 + i * 0.95
            val = Text(f"{target_fitness[i]:.2f}", font_size=12,
                       color=C["text_light"])
            val.move_to(np.array([x, base_y + h + 0.2, 0]))
            left_vals.add(val)
        self.play(FadeIn(left_vals), run_time=0.3)

        parity_brace = BraceBetweenPoints(
            np.array([-4.85, base_y + min(target_fitness) * max_h, 0]),
            np.array([-4.85, base_y + max(target_fitness) * max_h, 0]),
            direction=LEFT,
            color=C["text_dim"],
        )
        parity_txt = Text("Parity", font_size=14,
                          color=C["amber"]).next_to(parity_brace, LEFT, buff=0.1)
        self.play(FadeIn(parity_brace), FadeIn(parity_txt), run_time=0.5)

        # ── RIGHT PANEL: Portfolio ────────────────────────────────────
        right_title = Text("Portfolio", font_size=20,
                           color=C["text_light"], weight=BOLD).move_to(
            np.array([3.2, 1.8, 0]))
        right_border = Rectangle(width=panel_w, height=4.5,
                                 stroke_color=C["slate"], stroke_width=1,
                                 fill_opacity=0).move_to(np.array([3.2, -0.2, 0]))

        self.play(FadeIn(right_border), Write(right_title), run_time=0.5)

        # Individual policy bars (smaller, dimmed)
        right_bars = VGroup()
        for i in range(4):
            h = target_fitness[i] * max_h
            x = 1.8 + i * 0.75
            bar = Rectangle(width=0.55, height=h,
                            fill_color=POLICY_COLORS[i], fill_opacity=0.4,
                            stroke_width=0.5, stroke_color=C["steel"])
            bar.move_to(np.array([x, base_y + h / 2, 0]))
            right_bars.add(bar)

        self.play(FadeIn(right_bars), run_time=0.5)

        # Portfolio bar (taller)
        portfolio_fitness = 0.94
        ph = portfolio_fitness * max_h
        portfolio_bar = Rectangle(width=0.85, height=0.05,
                                  fill_color=C["highlight"], fill_opacity=0.9,
                                  stroke_width=1, stroke_color=C["amber"])
        px = 4.5
        portfolio_bar.move_to(np.array([px, base_y + 0.025, 0]))

        plbl = Text("Mix", font_size=14, color=C["text_light"],
                     weight=BOLD).move_to(np.array([px, base_y - 0.3, 0]))
        self.play(FadeIn(portfolio_bar), FadeIn(plbl), run_time=0.3)

        # Grow portfolio bar
        new_pbar = Rectangle(width=0.85, height=ph,
                             fill_color=C["highlight"], fill_opacity=0.9,
                             stroke_width=1, stroke_color=C["amber"])
        new_pbar.move_to(np.array([px, base_y + ph / 2, 0]))
        self.play(Transform(portfolio_bar, new_pbar), run_time=1.5)

        pval = Text(f"{portfolio_fitness:.2f}", font_size=14,
                    color=C["amber"], weight=BOLD).next_to(new_pbar, UP, buff=0.1)
        self.play(FadeIn(pval), run_time=0.3)

        # ── Dashed line showing max individual ────────────────────────
        max_individual = max(target_fitness) * max_h + base_y
        dashed = DashedLine(
            start=np.array([1.3, max_individual, 0]),
            end=np.array([5.1, max_individual, 0]),
            color=C["text_dim"], stroke_width=1, dash_length=0.1,
        )
        dashed_label = Text("Best single policy", font_size=11,
                            color=C["text_dim"]).next_to(dashed, RIGHT, buff=0.1)
        self.play(Create(dashed), FadeIn(dashed_label), run_time=0.5)

        # ── Caption ───────────────────────────────────────────────────
        caption = Text(
            "No single best, but diversification wins",
            font_size=20, color=C["highlight"], weight=BOLD,
        ).to_edge(DOWN, buff=0.3)
        self.play(Write(caption), run_time=0.8)
        self.wait(2)
