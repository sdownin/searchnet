"""
Scene: Blume Tutorial — Part 2 (Intermediate)
================================================
Six scenes for the intermediate/expert portion of the Blume (1993) tutorial.
Covers logit choice mechanics, potential games, detailed balance,
the Gibbs measure, phase transitions, and the full chain of reasoning.

Scenes:
  1. LogitChoiceScene       — Logit formula with computed probability bars
  2. PotentialGameScene     — 2x2 game matrix with potential function surface
  3. DetailedBalanceScene   — Detailed balance equation, step-by-step build
  4. GibbsMeasureScene      — Bar chart of configurations under varying beta
  5. PhaseTransitionScene   — Order parameter vs. beta with critical point
  6. ChainOfReasoningScene  — Logical chain from logit to QRE convergence

Render (individual):
    manim -qh scene_blume_part2.py LogitChoiceScene
    manim -qh scene_blume_part2.py PotentialGameScene
    manim -qh scene_blume_part2.py DetailedBalanceScene
    manim -qh scene_blume_part2.py GibbsMeasureScene
    manim -qh scene_blume_part2.py PhaseTransitionScene
    manim -qh scene_blume_part2.py ChainOfReasoningScene

Render (all):
    manim -qh scene_blume_part2.py
"""

import numpy as np
from manim import *
from searchnet_viz import SEARCHNET_COLORS, BaseSearchnetScene

C = SEARCHNET_COLORS


# ─────────────────────────────────────────────────────────────────────
# Scene 1: Logit Choice Mechanics
# ─────────────────────────────────────────────────────────────────────
class LogitChoiceScene(BaseSearchnetScene):
    """
    Displays the logit best-response formula, computes probabilities
    from concrete utility differences, and shows them as bars.
    """

    scene_title = "The Logit Choice Rule"
    scene_subtitle = "How agents pick actions under bounded rationality"

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993), Games and Economic Behavior")

        # ── Logit formula ──────────────────────────────────────────────
        formula = MathTex(
            r"P(\text{flip } j) = \frac{ e^{\beta \cdot \Delta U_{ij}} }{ 1 + \sum_{k} e^{\beta \cdot \Delta U_{ik}} }",
            font_size=36,
            color=C["text_light"],
        ).to_edge(UP, buff=0.5)

        self.play(Write(formula), run_time=1.2)
        self.wait(0.8)

        # ── Highlight components ───────────────────────────────────────
        brace_num = Brace(formula, UP, color=C["teal"], buff=0.05)
        brace_num_label = Text(
            "Attractiveness of move j",
            font_size=14, color=C["teal"], font="Times New Roman",
        ).next_to(brace_num, UP, buff=0.05)

        brace_den = Brace(formula, DOWN, color=C["amber"], buff=0.05)
        brace_den_label = Text(
            "Sum over all moves + pass",
            font_size=14, color=C["amber"], font="Times New Roman",
        ).next_to(brace_den, DOWN, buff=0.05)

        self.play(
            GrowFromCenter(brace_num), FadeIn(brace_num_label),
            run_time=0.6,
        )
        self.play(
            GrowFromCenter(brace_den), FadeIn(brace_den_label),
            run_time=0.6,
        )
        self.wait(0.8)

        # ── Concrete example ───────────────────────────────────────────
        self.play(
            FadeOut(brace_num), FadeOut(brace_num_label),
            FadeOut(brace_den), FadeOut(brace_den_label),
            run_time=0.4,
        )

        beta_val = 2.0
        delta_u = np.array([0.3, -0.1, 0.5, 0.1])
        n_moves = len(delta_u)

        # Compute logit probabilities (including "pass" option = exp(0) = 1)
        exp_vals = np.exp(beta_val * delta_u)
        denom = 1.0 + exp_vals.sum()
        probs = exp_vals / denom
        prob_pass = 1.0 / denom

        example_tex = MathTex(
            r"\beta = 2.0, \quad \Delta U = [0.3,\, -0.1,\, 0.5,\, 0.1]",
            font_size=26, color=C["amber"],
        ).next_to(formula, DOWN, buff=0.4)
        self.play(FadeIn(example_tex), run_time=0.5)

        # ── Bar chart ──────────────────────────────────────────────────
        all_probs = list(probs) + [prob_pass]
        labels = [f"j={k+1}" for k in range(n_moves)] + ["pass"]
        bar_colors = [C["teal"]] * n_moves + [C["text_dim"]]

        ax = Axes(
            x_range=[0, len(all_probs) + 1, 1],
            y_range=[0, 0.45, 0.1],
            x_length=9, y_length=3,
            axis_config={"color": C["text_dim"], "stroke_width": 1.2},
            tips=False,
        ).shift(DOWN * 1.0)

        y_label = Text(
            "Probability", font_size=14, color=C["text_dim"],
            font="Times New Roman",
        ).rotate(PI / 2).next_to(ax, LEFT, buff=0.25)
        self.play(Create(ax), FadeIn(y_label), run_time=0.5)

        bar_width = 0.65
        bars = VGroup()
        bar_labels = VGroup()
        prob_labels = VGroup()

        for i, (p, lbl, col) in enumerate(zip(all_probs, labels, bar_colors)):
            height = p * (3 / 0.45)
            bar = Rectangle(
                width=bar_width * (9 / (len(all_probs) + 1)),
                height=max(height, 0.02),
                fill_color=col,
                fill_opacity=0.85,
                stroke_width=0.5,
                stroke_color=C["text_dim"],
            )
            x_coord = ax.c2p(i + 1, 0)[0]
            y_base = ax.c2p(0, 0)[1]
            bar.move_to([x_coord, y_base + bar.height / 2, 0])
            bars.add(bar)

            label_mob = Text(
                lbl, font_size=13, color=C["text_dim"], font="Times New Roman",
            ).next_to(bar, DOWN, buff=0.08)
            bar_labels.add(label_mob)

            prob_text = Text(
                f"{p:.2f}", font_size=12, color=C["text_light"],
                font="Times New Roman",
            ).next_to(bar, UP, buff=0.05)
            prob_labels.add(prob_text)

        # Animate bars growing from zero
        self.play(
            *[GrowFromEdge(bar, DOWN) for bar in bars],
            run_time=1.0,
        )
        self.play(
            FadeIn(bar_labels), FadeIn(prob_labels),
            run_time=0.5,
        )

        # ── Highlight pass bar ─────────────────────────────────────────
        pass_bar = bars[-1]
        pass_rect = SurroundingRectangle(
            pass_bar, color=C["highlight"], buff=0.06, stroke_width=1.5,
        )
        pass_note = Text(
            '"pass" = denominator\'s 1',
            font_size=13, color=C["highlight"], font="Times New Roman",
        ).next_to(pass_rect, RIGHT, buff=0.2)
        self.play(Create(pass_rect), FadeIn(pass_note), run_time=0.5)
        self.wait(0.5)

        # ── Key message ───────────────────────────────────────────────
        message = Text(
            "Better moves get higher probability, but ALL moves are possible",
            font_size=17, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.2)
        self.play(FadeIn(message), run_time=0.5)
        self.wait(2)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 2: Potential Game — Matrix and Surface
# ─────────────────────────────────────────────────────────────────────
class PotentialGameScene(BaseSearchnetScene):
    """
    2x2 game matrix showing that unilateral utility changes equal
    potential-function changes. The potential is visualized as a
    surface that rises and falls with player moves.
    """

    scene_title = "Potential Games"
    scene_subtitle = "Individual incentives align with a global score"

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Monderer & Shapley (1996)")

        # ── Definition ─────────────────────────────────────────────────
        defn = MathTex(
            r"u_i(s_i', s_{-i}) - u_i(s_i, s_{-i})",
            r"=",
            r"\Phi(s_i', s_{-i}) - \Phi(s_i, s_{-i})",
            font_size=28,
            color=C["text_light"],
        ).to_edge(UP, buff=0.4)

        brace_left = Brace(defn[0], DOWN, color=C["teal"], buff=0.08)
        bl_label = Text(
            "Player i's incentive", font_size=13,
            color=C["teal"], font="Times New Roman",
        ).next_to(brace_left, DOWN, buff=0.05)

        brace_right = Brace(defn[2], DOWN, color=C["coral"], buff=0.08)
        br_label = Text(
            "Change in global potential", font_size=13,
            color=C["coral"], font="Times New Roman",
        ).next_to(brace_right, DOWN, buff=0.05)

        self.play(Write(defn), run_time=1.0)
        self.play(
            GrowFromCenter(brace_left), FadeIn(bl_label),
            GrowFromCenter(brace_right), FadeIn(br_label),
            run_time=0.7,
        )
        self.wait(1.0)
        self.play(
            FadeOut(brace_left), FadeOut(bl_label),
            FadeOut(brace_right), FadeOut(br_label),
            run_time=0.4,
        )

        # ── 2x2 Game matrix ───────────────────────────────────────────
        # Payoffs: coordination game  u1, u2
        # (A,A) = (3,3)   (A,B) = (0,0)
        # (B,A) = (0,0)   (B,B) = (2,2)
        # Potential: Phi(A,A)=3, Phi(A,B)=0, Phi(B,A)=0, Phi(B,B)=2

        matrix_group = VGroup()

        # Column/row headers
        p1_label = Text("Player 1", font_size=16, color=C["teal"],
                         font="Times New Roman", weight=BOLD)
        p2_label = Text("Player 2", font_size=16, color=C["coral"],
                         font="Times New Roman", weight=BOLD)

        cell_size = 1.2
        entries = [
            ["3, 3", "0, 0"],
            ["0, 0", "2, 2"],
        ]
        phi_vals = [
            ["\\Phi=3", "\\Phi=0"],
            ["\\Phi=0", "\\Phi=2"],
        ]
        phi_colors = [
            [C["teal"], C["text_dim"]],
            [C["text_dim"], C["amber"]],
        ]

        table_cells = VGroup()
        phi_labels_group = VGroup()

        for r in range(2):
            for cc in range(2):
                rect = Square(
                    side_length=cell_size,
                    stroke_color=C["text_dim"], stroke_width=1.5,
                    fill_color=C["bg_panel"], fill_opacity=0.6,
                )
                rect.move_to([
                    cc * cell_size - 2.5,
                    -r * cell_size - 0.5,
                    0,
                ])
                entry = Text(
                    entries[r][cc], font_size=18, color=C["text_light"],
                    font="Times New Roman",
                ).move_to(rect.get_center() + UP * 0.15)

                phi_label = MathTex(
                    phi_vals[r][cc], font_size=18,
                    color=phi_colors[r][cc],
                ).move_to(rect.get_center() + DOWN * 0.25)

                table_cells.add(VGroup(rect, entry))
                phi_labels_group.add(phi_label)

        # Row labels (Player 1: A, B)
        row_a = Text("A", font_size=18, color=C["teal"],
                      font="Times New Roman", weight=BOLD)
        row_a.next_to(table_cells[0][0], LEFT, buff=0.25)
        row_b = Text("B", font_size=18, color=C["teal"],
                      font="Times New Roman", weight=BOLD)
        row_b.next_to(table_cells[2][0], LEFT, buff=0.25)

        # Column labels (Player 2: A, B)
        col_a = Text("A", font_size=18, color=C["coral"],
                      font="Times New Roman", weight=BOLD)
        col_a.next_to(table_cells[0][0], UP, buff=0.2)
        col_b = Text("B", font_size=18, color=C["coral"],
                      font="Times New Roman", weight=BOLD)
        col_b.next_to(table_cells[1][0], UP, buff=0.2)

        p1_label.next_to(row_a, LEFT, buff=0.4)
        p2_label.next_to(col_a, UP, buff=0.2)

        self.play(
            FadeIn(table_cells), FadeIn(p1_label), FadeIn(p2_label),
            FadeIn(row_a), FadeIn(row_b), FadeIn(col_a), FadeIn(col_b),
            run_time=0.8,
        )
        self.wait(0.5)

        # ── Highlight a player-1 deviation ─────────────────────────────
        # Player 1 switches A->B while Player 2 stays at A
        # Utility change: 0 - 3 = -3.  Potential change: 0 - 3 = -3.
        highlight_from = SurroundingRectangle(
            table_cells[0], color=C["highlight"], stroke_width=2.5, buff=0.05,
        )
        highlight_to = SurroundingRectangle(
            table_cells[2], color=C["highlight"], stroke_width=2.5, buff=0.05,
        )

        arrow_dev = Arrow(
            table_cells[0].get_center() + LEFT * 0.7,
            table_cells[2].get_center() + LEFT * 0.7,
            color=C["highlight"], stroke_width=2.5, buff=0.1,
        )
        dev_label = MathTex(
            r"\Delta u_1 = -3", font_size=20, color=C["highlight"],
        ).next_to(arrow_dev, LEFT, buff=0.15)

        self.play(Create(highlight_from), run_time=0.4)
        self.play(
            ReplacementTransform(highlight_from, highlight_to),
            GrowArrow(arrow_dev),
            run_time=0.7,
        )
        self.play(FadeIn(dev_label), run_time=0.4)

        # Show potential change equals utility change
        phi_change = MathTex(
            r"\Delta \Phi = 0 - 3 = -3",
            font_size=20, color=C["coral"],
        ).next_to(dev_label, DOWN, buff=0.2)
        eq_sign = MathTex(
            r"\checkmark\ \Delta u_1 = \Delta \Phi",
            font_size=20, color=C["positive"],
        ).next_to(phi_change, DOWN, buff=0.15)

        self.play(FadeIn(phi_labels_group), run_time=0.5)
        self.play(FadeIn(phi_change), run_time=0.5)
        self.play(FadeIn(eq_sign), run_time=0.4)
        self.wait(1.0)

        # ── Potential surface (right side) ─────────────────────────────
        # Simple bar representation of the 4 potential values
        phi_bar_data = [3, 0, 0, 2]
        phi_bar_labels_text = ["(A,A)", "(A,B)", "(B,A)", "(B,B)"]

        bar_group = VGroup()
        bar_x_start = 2.0
        bar_spacing = 1.2
        bar_max_h = 2.5

        for i, (val, lbl) in enumerate(zip(phi_bar_data, phi_bar_labels_text)):
            h = max(val / 3.0 * bar_max_h, 0.05)
            bar = Rectangle(
                width=0.7, height=h,
                fill_color=interpolate_color(
                    ManimColor(C["text_dim"]), ManimColor(C["teal"]), val / 3.0
                ),
                fill_opacity=0.85,
                stroke_width=0.5, stroke_color=C["text_dim"],
            )
            x_pos = bar_x_start + i * bar_spacing
            bar.move_to([x_pos, -2.0 + h / 2, 0])

            lbl_mob = Text(
                lbl, font_size=11, color=C["text_dim"], font="Times New Roman",
            ).next_to(bar, DOWN, buff=0.06)

            val_mob = MathTex(
                f"\\Phi={val}", font_size=14, color=C["text_light"],
            ).next_to(bar, UP, buff=0.05)

            bar_group.add(VGroup(bar, lbl_mob, val_mob))

        phi_title = MathTex(
            r"\Phi(s_1, s_2)",
            font_size=22, color=C["text_light"],
        ).move_to([bar_x_start + 1.8, 1.2, 0])
        self.play(FadeIn(phi_title), FadeIn(bar_group), run_time=0.8)

        # ── Key message ───────────────────────────────────────────────
        message = Text(
            "Individual incentives align with a global score",
            font_size=17, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.15)
        self.play(FadeIn(message), run_time=0.5)
        self.wait(2)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 3: Detailed Balance
# ─────────────────────────────────────────────────────────────────────
class DetailedBalanceScene(BaseSearchnetScene):
    """
    Step-by-step build of the detailed balance equation:
      q(x->x')/q(x'->x) = exp(beta[Phi(x')-Phi(x)]) = pi(x')/pi(x)
    With a balance-scale visual.
    """

    scene_title = "Detailed Balance"
    scene_subtitle = "Why the simulation converges"

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993), Games and Economic Behavior")

        # ── Two states ─────────────────────────────────────────────────
        state_x = Circle(
            radius=0.5, fill_color=C["teal"], fill_opacity=0.7,
            stroke_color=C["text_light"], stroke_width=2,
        ).shift(LEFT * 3 + UP * 1.0)
        label_x = MathTex("x", font_size=28, color=C["text_light"])
        label_x.move_to(state_x.get_center())

        state_xp = Circle(
            radius=0.5, fill_color=C["coral"], fill_opacity=0.7,
            stroke_color=C["text_light"], stroke_width=2,
        ).shift(RIGHT * 3 + UP * 1.0)
        label_xp = MathTex("x'", font_size=28, color=C["text_light"])
        label_xp.move_to(state_xp.get_center())

        self.play(
            FadeIn(state_x), FadeIn(label_x),
            FadeIn(state_xp), FadeIn(label_xp),
            run_time=0.6,
        )

        # ── Transition arrows (use Arrow instead of CurvedArrow for CE 0.20 compat)
        arrow_forward = Arrow(
            state_x.get_right() + UP * 0.2,
            state_xp.get_left() + UP * 0.2,
            color=C["amber"], stroke_width=2.5, buff=0.1,
        )
        arrow_backward = Arrow(
            state_xp.get_left() + DOWN * 0.2,
            state_x.get_right() + DOWN * 0.2,
            color=C["text_dim"], stroke_width=2.5, buff=0.1,
        )

        fwd_label = MathTex(
            r"q(x \to x')", font_size=20, color=C["amber"],
        ).next_to(arrow_forward, UP, buff=0.1)
        bwd_label = MathTex(
            r"q(x' \to x)", font_size=20, color=C["text_dim"],
        ).next_to(arrow_backward, DOWN, buff=0.1)

        self.play(
            GrowArrow(arrow_forward), FadeIn(fwd_label),
            run_time=0.6,
        )
        self.play(
            GrowArrow(arrow_backward), FadeIn(bwd_label),
            run_time=0.6,
        )
        self.wait(0.5)

        # ── Build the equation step by step ────────────────────────────
        # Step 1: rate ratio
        eq_step1 = MathTex(
            r"\frac{q(x \to x')}{q(x' \to x)}",
            font_size=30, color=C["text_light"],
        ).shift(DOWN * 0.5)
        self.play(Write(eq_step1), run_time=0.7)
        self.wait(0.5)

        # Step 2: = exp(beta * delta Phi)
        eq_step2 = MathTex(
            r"\frac{q(x \to x')}{q(x' \to x)}",
            r"=",
            r"e^{\,\beta\,[\Phi(x') - \Phi(x)]}",
            font_size=30, color=C["text_light"],
        ).shift(DOWN * 0.5)
        eq_step2[2].set_color(C["amber"])

        self.play(TransformMatchingTex(eq_step1, eq_step2), run_time=0.8)
        self.wait(0.5)

        # Step 3: = pi(x') / pi(x)
        eq_step3 = MathTex(
            r"\frac{q(x \to x')}{q(x' \to x)}",
            r"=",
            r"e^{\,\beta\,[\Phi(x') - \Phi(x)]}",
            r"=",
            r"\frac{\pi(x')}{\pi(x)}",
            font_size=30, color=C["text_light"],
        ).shift(DOWN * 0.5)
        eq_step3[2].set_color(C["amber"])
        eq_step3[4].set_color(C["highlight"])

        self.play(TransformMatchingTex(eq_step2, eq_step3), run_time=0.8)
        self.wait(0.8)

        # ── Balance scale visual ───────────────────────────────────────
        # Fulcrum
        fulcrum = Triangle(
            fill_color=C["text_dim"], fill_opacity=0.8, stroke_width=0,
        ).scale(0.25).move_to(DOWN * 2.5)

        beam_center = fulcrum.get_top() + UP * 0.05
        # Tilt to show Phi(x') > Phi(x)
        tilt_angle = -12 * DEGREES
        beam = Line(
            LEFT * 2.0, RIGHT * 2.0,
            color=C["text_light"], stroke_width=3,
        ).move_to(beam_center).rotate(tilt_angle, about_point=beam_center)

        left_pan = Square(
            side_length=0.6, fill_color=C["teal"], fill_opacity=0.5,
            stroke_color=C["teal"], stroke_width=1.5,
        ).move_to(beam.get_start() + DOWN * 0.4)
        left_label = MathTex(
            r"\Phi(x)", font_size=16, color=C["teal"],
        ).next_to(left_pan, DOWN, buff=0.08)

        right_pan = Square(
            side_length=0.6, fill_color=C["coral"], fill_opacity=0.5,
            stroke_color=C["coral"], stroke_width=1.5,
        ).move_to(beam.get_end() + DOWN * 0.4)
        right_label = MathTex(
            r"\Phi(x')", font_size=16, color=C["coral"],
        ).next_to(right_pan, DOWN, buff=0.08)

        # Weight indicators
        weight_left = Circle(
            radius=0.15, fill_color=C["teal"], fill_opacity=0.9,
            stroke_width=0,
        ).move_to(left_pan.get_center())
        weight_right = VGroup(
            Circle(radius=0.15, fill_color=C["coral"], fill_opacity=0.9,
                   stroke_width=0).shift(UP * 0.05),
            Circle(radius=0.15, fill_color=C["coral"], fill_opacity=0.7,
                   stroke_width=0).shift(DOWN * 0.12),
        ).move_to(right_pan.get_center())

        scale_group = VGroup(
            fulcrum, beam, left_pan, right_pan,
            left_label, right_label, weight_left, weight_right,
        )

        self.play(FadeIn(scale_group), run_time=0.8)
        self.wait(0.5)

        scale_note = Text(
            "Higher potential tips the balance: more flow toward x'",
            font_size=14, color=C["text_dim"], font="Times New Roman",
        ).next_to(scale_group, DOWN, buff=0.2)
        self.play(FadeIn(scale_note), run_time=0.4)

        # ── Key message ───────────────────────────────────────────────
        message = Text(
            "Forward and backward rates balance: this is why the simulation converges",
            font_size=16, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.12)
        self.play(FadeIn(message), run_time=0.5)
        self.wait(2.5)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 4: Gibbs Measure — Configuration Bar Chart
# ─────────────────────────────────────────────────────────────────────
class GibbsMeasureScene(BaseSearchnetScene):
    """
    Bar chart of all configurations for a tiny system.
    Animate beta sweeping from 0 to infinity to show concentration.
    """

    scene_title = "The Gibbs Measure"
    scene_subtitle = "How temperature shapes the equilibrium distribution"

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993), Games and Economic Behavior")

        # ── Gibbs formula ──────────────────────────────────────────────
        gibbs_label = MathTex(
            r"\pi_\beta(x) = \frac{e^{\beta \cdot \Phi(x)}}{Z(\beta)}",
            font_size=32, color=C["text_light"],
        ).to_edge(UP, buff=0.4)
        self.play(Write(gibbs_label), run_time=0.8)

        # ── 8-configuration toy system ─────────────────────────────────
        n_states = 8
        # Potential values for each configuration
        potentials = np.array([0.2, 0.5, 1.0, 0.3, 0.8, 1.5, 0.4, 0.7])
        state_labels = [f"x_{{{i+1}}}" for i in range(n_states)]

        # ── Axes ───────────────────────────────────────────────────────
        ax = Axes(
            x_range=[0, n_states + 1, 1],
            y_range=[0, 0.65, 0.1],
            x_length=9, y_length=3.5,
            axis_config={"color": C["text_dim"], "stroke_width": 1.2},
            tips=False,
        ).shift(DOWN * 0.6)

        y_label = Text(
            "Probability", font_size=14, color=C["text_dim"],
            font="Times New Roman",
        ).rotate(PI / 2).next_to(ax, LEFT, buff=0.25)
        self.play(Create(ax), FadeIn(y_label), run_time=0.5)

        # ── Beta tracker ───────────────────────────────────────────────
        beta_tracker = ValueTracker(0.01)

        beta_tex = always_redraw(lambda: MathTex(
            r"\beta = " + f"{beta_tracker.get_value():.1f}",
            font_size=28, color=C["amber"],
        ).next_to(gibbs_label, DOWN, buff=0.15))
        self.play(FadeIn(beta_tex), run_time=0.3)

        # ── Regime labels ──────────────────────────────────────────────
        regime_label = always_redraw(lambda: Text(
            "Uniform (all equal)" if beta_tracker.get_value() < 0.5
            else ("Concentrating" if beta_tracker.get_value() < 5.0
                  else "Near-deterministic"),
            font_size=16,
            color=(C["text_dim"] if beta_tracker.get_value() < 0.5
                   else (C["amber"] if beta_tracker.get_value() < 5.0
                         else C["coral"])),
            font="Times New Roman",
        ).next_to(beta_tex, RIGHT, buff=0.5))
        self.play(FadeIn(regime_label), run_time=0.3)

        # ── Animated bars ──────────────────────────────────────────────
        bar_width = 0.7

        def get_gibbs_probs(beta_val):
            weights = np.exp(beta_val * potentials)
            return weights / weights.sum()

        def create_bars(beta_val):
            probs = get_gibbs_probs(beta_val)
            bar_group = VGroup()
            for i in range(n_states):
                height = probs[i] * (3.5 / 0.65)
                bar = Rectangle(
                    width=bar_width * (9 / (n_states + 1)),
                    height=max(height, 0.02),
                    fill_color=interpolate_color(
                        ManimColor(C["text_dim"]),
                        ManimColor(C["teal"]),
                        potentials[i] / potentials.max(),
                    ),
                    fill_opacity=0.85,
                    stroke_width=0.5,
                    stroke_color=C["text_dim"],
                )
                x_coord = ax.c2p(i + 1, 0)[0]
                y_base = ax.c2p(0, 0)[1]
                bar.move_to([x_coord, y_base + bar.height / 2, 0])

                label = MathTex(
                    state_labels[i], font_size=13, color=C["text_dim"],
                ).next_to(bar, DOWN, buff=0.08)
                bar_group.add(VGroup(bar, label))
            return bar_group

        bars = always_redraw(lambda: create_bars(beta_tracker.get_value()))
        self.play(FadeIn(bars), run_time=0.5)
        self.wait(0.5)

        # ── beta = 0: uniform ──────────────────────────────────────────
        beta0_note = Text(
            "beta = 0: all configurations equally likely",
            font_size=15, color=C["text_dim"], font="Times New Roman",
        ).to_edge(DOWN, buff=0.15)
        self.play(FadeIn(beta0_note), run_time=0.4)
        self.play(beta_tracker.animate.set_value(0.01), run_time=0.5)
        self.wait(1.0)
        self.play(FadeOut(beta0_note), run_time=0.3)

        # ── Sweep beta up ──────────────────────────────────────────────
        self.play(beta_tracker.animate.set_value(2.0), run_time=2.0)
        self.wait(0.5)

        self.play(beta_tracker.animate.set_value(5.0), run_time=2.0)
        self.wait(0.5)

        # ── beta -> infinity: only max survives ────────────────────────
        self.play(beta_tracker.animate.set_value(15.0), run_time=2.5)

        inf_note = Text(
            "beta -> infinity: only the global maximum survives",
            font_size=15, color=C["coral"], font="Times New Roman",
        ).to_edge(DOWN, buff=0.15)
        self.play(FadeIn(inf_note), run_time=0.4)

        # Highlight the max-potential config
        best_idx = int(np.argmax(potentials))
        pointer = Triangle(
            fill_color=C["highlight"], fill_opacity=0.9, stroke_width=0,
        ).scale(0.12).rotate(PI)
        pointer.next_to(ax.c2p(best_idx + 1, 0.6), UP, buff=0.1)
        self.play(FadeIn(pointer), run_time=0.3)

        self.wait(2)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 5: Phase Transition
# ─────────────────────────────────────────────────────────────────────
class PhaseTransitionScene(BaseSearchnetScene):
    """
    Order parameter vs. beta showing a phase transition at critical beta*.
    Disordered (random search) vs. ordered (structured equilibrium).
    """

    scene_title = "Phase Transition"
    scene_subtitle = "From disorder to order at a critical threshold"

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993); cf. Ising model")

        # ── Title ──────────────────────────────────────────────────────
        title = Text(
            "Order Parameter vs. Rationality",
            font_size=26, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.35)
        self.play(FadeIn(title), run_time=0.4)

        # ── Axes ───────────────────────────────────────────────────────
        ax = Axes(
            x_range=[0, 6, 1],
            y_range=[0, 1.1, 0.2],
            x_length=8, y_length=4.5,
            axis_config={"color": C["text_dim"], "stroke_width": 1.5},
            tips=False,
        ).shift(DOWN * 0.3)

        x_label = MathTex(
            r"\beta", font_size=24, color=C["text_dim"],
        ).next_to(ax, DOWN, buff=0.2)
        y_label = Text(
            "Order parameter", font_size=15, color=C["text_dim"],
            font="Times New Roman",
        ).rotate(PI / 2).next_to(ax, LEFT, buff=0.3)

        self.play(Create(ax), FadeIn(x_label), FadeIn(y_label), run_time=0.6)

        # ── Sigmoid-like order parameter curve ─────────────────────────
        beta_star = 2.5

        def order_param(beta):
            """Smooth sigmoidal transition centered at beta_star."""
            return 1.0 / (1.0 + np.exp(-4.0 * (beta - beta_star)))

        curve = ax.plot(
            order_param,
            x_range=[0.01, 6, 0.05],
            color=C["highlight"],
            stroke_width=3,
        )
        self.play(Create(curve), run_time=1.5)

        # ── Critical point ─────────────────────────────────────────────
        critical_line = DashedLine(
            ax.c2p(beta_star, 0),
            ax.c2p(beta_star, 1.1),
            color=C["coral"], stroke_width=2, dash_length=0.1,
        )
        beta_star_label = MathTex(
            r"\beta^*",
            font_size=24, color=C["coral"],
        ).next_to(critical_line, DOWN, buff=0.1)

        self.play(Create(critical_line), FadeIn(beta_star_label), run_time=0.6)
        self.wait(0.5)

        # ── Region labels ──────────────────────────────────────────────
        disordered_box = VGroup(
            Rectangle(
                width=2.5, height=0.9,
                fill_color=C["bg_panel"], fill_opacity=0.8,
                stroke_color=C["text_dim"], stroke_width=1,
            ),
            Text(
                "Disordered\n(random search)",
                font_size=14, color=C["text_dim"],
                font="Times New Roman", line_spacing=1.2,
            ),
        )
        disordered_box[1].move_to(disordered_box[0].get_center())
        disordered_box.move_to(ax.c2p(1.0, 0.7))

        ordered_box = VGroup(
            Rectangle(
                width=2.8, height=0.9,
                fill_color=C["bg_panel"], fill_opacity=0.8,
                stroke_color=C["teal"], stroke_width=1,
            ),
            Text(
                "Ordered\n(structured equilibrium)",
                font_size=14, color=C["teal"],
                font="Times New Roman", line_spacing=1.2,
            ),
        )
        ordered_box[1].move_to(ordered_box[0].get_center())
        ordered_box.move_to(ax.c2p(4.5, 0.35))

        self.play(FadeIn(disordered_box), run_time=0.5)
        self.play(FadeIn(ordered_box), run_time=0.5)
        self.wait(0.5)

        # ── Moving dot along the curve ─────────────────────────────────
        dot = Dot(color=C["highlight"], radius=0.08)
        dot.move_to(ax.c2p(0.01, order_param(0.01)))

        beta_val_tracker = ValueTracker(0.01)
        dot.add_updater(
            lambda d: d.move_to(
                ax.c2p(
                    beta_val_tracker.get_value(),
                    order_param(beta_val_tracker.get_value()),
                )
            )
        )

        self.play(FadeIn(dot), run_time=0.3)
        self.play(beta_val_tracker.animate.set_value(6.0), run_time=3.0)
        self.wait(0.5)

        # ── Analogy label ──────────────────────────────────────────────
        analogy = Text(
            "Like water freezing at 0 degrees C",
            font_size=16, color=C["text_light"],
            font="Times New Roman", slant=ITALIC,
        ).to_edge(DOWN, buff=0.15)
        self.play(FadeIn(analogy), run_time=0.5)

        self.wait(2.5)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 6: Chain of Reasoning
# ─────────────────────────────────────────────────────────────────────
class ChainOfReasoningScene(BaseSearchnetScene):
    """
    The logical chain from the vignette summary, animated step by step:
    Logit BR + Potential -> Gibbs -> SAOM identity -> QRE convergence.
    """

    scene_title = "The Chain of Reasoning"
    scene_subtitle = "From logit best-response to equilibrium convergence"

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993); Snijders (2001)")

        # ── Build the chain ────────────────────────────────────────────
        box_width = 6.0
        box_height = 0.65
        vertical_gap = 0.35
        arrow_len = 0.3

        steps = [
            "Logit best response  +  Potential game",
            "Stationary distribution = Gibbs measure",
            "SAOM + additive utility = Potential game",
            "searchnet CTMC converges to QRE",
        ]
        step_colors = [C["teal"], C["amber"], C["coral"], C["highlight"]]

        boxes = []
        labels = []
        arrows = []

        start_y = 2.0

        for i, (text, col) in enumerate(zip(steps, step_colors)):
            y_pos = start_y - i * (box_height + vertical_gap + arrow_len)

            box = RoundedRectangle(
                width=box_width, height=box_height,
                corner_radius=0.12,
                fill_color=C["bg_panel"], fill_opacity=0.85,
                stroke_color=col, stroke_width=2,
            ).move_to([0, y_pos, 0])

            label = Text(
                text,
                font_size=18, color=C["text_light"],
                font="Times New Roman",
            ).move_to(box.get_center())

            boxes.append(box)
            labels.append(label)

            if i > 0:
                prev_y = start_y - (i - 1) * (box_height + vertical_gap + arrow_len)
                arrow = Arrow(
                    [0, prev_y - box_height / 2 - 0.05, 0],
                    [0, y_pos + box_height / 2 + 0.05, 0],
                    color=C["text_dim"], stroke_width=2,
                    buff=0,
                    max_tip_length_to_length_ratio=0.3,
                )
                arrows.append(arrow)

        # ── Animate step by step ───────────────────────────────────────
        # Step 1
        self.play(
            FadeIn(boxes[0]), Write(labels[0]),
            run_time=0.8,
        )
        self.wait(0.5)

        # Step 2
        self.play(GrowArrow(arrows[0]), run_time=0.4)
        self.play(
            FadeIn(boxes[1]), Write(labels[1]),
            run_time=0.8,
        )
        self.wait(0.5)

        # Step 3
        self.play(GrowArrow(arrows[1]), run_time=0.4)
        self.play(
            FadeIn(boxes[2]), Write(labels[2]),
            run_time=0.8,
        )
        self.wait(0.5)

        # Step 4 (conclusion)
        self.play(GrowArrow(arrows[2]), run_time=0.4)
        self.play(
            FadeIn(boxes[3]), Write(labels[3]),
            run_time=0.8,
        )
        self.wait(0.8)

        # ── Highlight conclusion ───────────────────────────────────────
        conclusion_highlight = SurroundingRectangle(
            VGroup(boxes[3], labels[3]),
            color=C["highlight"], stroke_width=3, buff=0.1,
            corner_radius=0.15,
        )
        self.play(Create(conclusion_highlight), run_time=0.6)

        # ── Implication arrow from step 3 to step 1 (the key insight)
        # Show that SAOM being a potential game is the bridge
        bridge_brace = Brace(
            VGroup(boxes[0], boxes[2]),
            direction=RIGHT, color=C["plum"], buff=0.15,
        )
        bridge_label = Text(
            "Blume's\nbridge",
            font_size=14, color=C["plum"],
            font="Times New Roman", line_spacing=1.2,
        ).next_to(bridge_brace, RIGHT, buff=0.1)
        self.play(
            GrowFromCenter(bridge_brace), FadeIn(bridge_label),
            run_time=0.6,
        )

        self.wait(3)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)
