"""
Scene: Blume Tutorial — Part 1 (Novice)
=========================================
Four accessible, non-technical animations introducing the intuition behind
Blume's (1993) logit dynamics and Gibbs convergence.

Scenes:
  1. SeatingAnalogyScene   — People clustering near friends in a room
  2. NoisyBestResponseScene — How noise (beta) shapes choice
  3. AnnealingScene         — Hot/cold annealing metaphor for exploration/exploitation
  4. ConvergenceDemoScene   — Two different starts converge to the same equilibrium

Render (individual):
    manim -ql scene_blume_part1.py SeatingAnalogyScene
    manim -ql scene_blume_part1.py NoisyBestResponseScene
    manim -ql scene_blume_part1.py AnnealingScene
    manim -ql scene_blume_part1.py ConvergenceDemoScene

Render (all):
    manim -ql scene_blume_part1.py
"""

import numpy as np
from manim import *
from searchnet_viz import SEARCHNET_COLORS, BaseSearchnetScene

C = SEARCHNET_COLORS

# Person colors for the seating analogy (3 friend groups)
PERSON_COLORS = [C["teal"], C["coral"], C["amber"], C["teal"], C["coral"], C["amber"]]


# ─────────────────────────────────────────────────────────────────────
# Scene 1: Seating Analogy
# ─────────────────────────────────────────────────────────────────────
class SeatingAnalogyScene(BaseSearchnetScene):
    """
    M=6 people (colored circles) choose among N=8 seats (squares)
    arranged in clusters. People enter one at a time, then reshuffle
    toward friends — clusters form organically.
    """

    scene_title = "The Seating Problem"
    scene_subtitle = "How people naturally cluster near friends"

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993), Games and Economic Behavior")

        # ── Seat positions: 8 seats in 3 loose clusters ────────────
        #   Cluster A (left):  seats 0, 1, 2
        #   Cluster B (right): seats 3, 4, 5
        #   Loner seats:       seats 6, 7
        seat_positions = [
            # Cluster A (left)
            np.array([-4.0, 0.5, 0]),
            np.array([-3.0, 0.5, 0]),
            np.array([-3.5, -0.5, 0]),
            # Cluster B (right)
            np.array([3.0, 0.5, 0]),
            np.array([4.0, 0.5, 0]),
            np.array([3.5, -0.5, 0]),
            # Loner seats
            np.array([-0.5, 1.2, 0]),
            np.array([0.5, -1.2, 0]),
        ]

        N_SEATS = len(seat_positions)
        M_PEOPLE = 6

        # ── Draw seats (squares) ──────────────────────────────────
        seats = VGroup()
        seat_labels = VGroup()
        for i, pos in enumerate(seat_positions):
            sq = Square(
                side_length=0.7,
                fill_color=C["bg_panel"],
                fill_opacity=0.5,
                stroke_color=C["steel"],
                stroke_width=1.5,
            ).move_to(pos)
            seats.add(sq)
            lbl = Text(str(i + 1), font_size=14, color=C["text_dim"],
                       font="Times New Roman").move_to(pos + DOWN * 0.55)
            seat_labels.add(lbl)

        room_label = Text(
            "Room with 8 seats",
            font_size=22, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.35)
        self.play(FadeIn(room_label), FadeIn(seats), FadeIn(seat_labels), run_time=0.8)
        self.wait(0.5)

        # ── People (colored circles) — friend groups by color ─────
        #   Group teal:  persons 0, 3
        #   Group coral: persons 1, 4
        #   Group amber: persons 2, 5
        person_colors = [C["teal"], C["coral"], C["amber"],
                         C["teal"], C["coral"], C["amber"]]

        # Random initial assignment (each person picks a random empty seat)
        rng = np.random.default_rng(42)
        assignment = rng.choice(N_SEATS, size=M_PEOPLE, replace=False).tolist()

        # Create person circles off-screen (left)
        people = VGroup()
        for i in range(M_PEOPLE):
            circ = Circle(
                radius=0.28,
                fill_color=person_colors[i],
                fill_opacity=0.9,
                stroke_color=WHITE,
                stroke_width=1.5,
            )
            circ.move_to(LEFT * 6.5 + UP * (1.5 - i * 0.6))
            plbl = Text(
                f"P{i + 1}", font_size=13, color=C["bg_dark"],
                font="Times New Roman", weight=BOLD,
            ).move_to(circ.get_center())
            pg = VGroup(circ, plbl)
            people.add(pg)

        # ── Phase 1: People enter and sit down one at a time ──────
        enter_label = Text(
            "People arrive and pick seats...",
            font_size=18, color=C["text_dim"],
            font="Times New Roman", slant=ITALIC,
        ).to_edge(DOWN, buff=0.4)
        self.play(FadeIn(enter_label), run_time=0.3)

        for i in range(M_PEOPLE):
            target = seat_positions[assignment[i]]
            self.play(
                people[i].animate.move_to(target),
                run_time=0.5,
            )

        self.wait(0.8)
        self.play(FadeOut(enter_label), run_time=0.3)

        # ── Phase 2: People reshuffle toward friends ──────────────
        # Friend pairs: (0,3), (1,4), (2,5) — same color
        # Strategy: move each friend pair toward the same cluster
        reshuffle_label = Text(
            "Now they reshuffle toward friends...",
            font_size=18, color=C["text_dim"],
            font="Times New Roman", slant=ITALIC,
        ).to_edge(DOWN, buff=0.4)
        self.play(FadeIn(reshuffle_label), run_time=0.3)

        # Target clustering: teal pair -> cluster A, coral -> cluster B,
        # amber -> one in each cluster (compromise)
        target_seats = {
            0: 0,  # teal -> cluster A, seat 0
            3: 1,  # teal -> cluster A, seat 1
            1: 3,  # coral -> cluster B, seat 3
            4: 4,  # coral -> cluster B, seat 4
            2: 2,  # amber -> cluster A, seat 2
            5: 5,  # amber -> cluster B, seat 5
        }

        # Animate in two waves for visual clarity
        # Wave 1: most dislocated people move
        wave1_anims = []
        for person_idx in [0, 1, 3, 4]:
            target_seat = target_seats[person_idx]
            wave1_anims.append(
                people[person_idx].animate.move_to(seat_positions[target_seat])
            )
        self.play(*wave1_anims, run_time=1.2)
        self.wait(0.3)

        # Wave 2: remaining people adjust
        wave2_anims = []
        for person_idx in [2, 5]:
            target_seat = target_seats[person_idx]
            wave2_anims.append(
                people[person_idx].animate.move_to(seat_positions[target_seat])
            )
        self.play(*wave2_anims, run_time=1.0)
        self.wait(0.5)

        self.play(FadeOut(reshuffle_label), run_time=0.3)

        # ── Highlight clusters ────────────────────────────────────
        cluster_a_rect = SurroundingRectangle(
            VGroup(seats[0], seats[1], seats[2]),
            color=C["teal"], buff=0.25, corner_radius=0.15,
            stroke_width=2,
        )
        cluster_b_rect = SurroundingRectangle(
            VGroup(seats[3], seats[4], seats[5]),
            color=C["coral"], buff=0.25, corner_radius=0.15,
            stroke_width=2,
        )
        self.play(Create(cluster_a_rect), Create(cluster_b_rect), run_time=0.8)

        punchline = Text(
            "People naturally cluster near friends",
            font_size=24, color=C["highlight"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.35)
        self.play(FadeIn(punchline), run_time=0.5)
        self.wait(2.5)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 2: Noisy Best Response
# ─────────────────────────────────────────────────────────────────────
class NoisyBestResponseScene(BaseSearchnetScene):
    """
    Four bars represent utility of 4 options. A pointer shows which
    option gets chosen. Beta slider goes from 0 (random) to high
    (greedy), visually demonstrating the noise-rationality tradeoff.
    """

    scene_title = "Noisy Best Response"
    scene_subtitle = "How rationality shapes decisions"

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993), Games and Economic Behavior")

        rng = np.random.default_rng(7)

        # ── Utility values for 4 options ──────────────────────────
        utilities = np.array([0.3, 0.7, 1.0, 0.5])
        option_labels = ["A", "B", "C", "D"]
        bar_colors = [C["slate"], C["sage"], C["teal"], C["steel"]]
        n_options = len(utilities)

        # ── Title ─────────────────────────────────────────────────
        title = Text(
            "Choosing Among Options",
            font_size=24, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.35)
        self.play(FadeIn(title), run_time=0.4)

        # ── Draw static utility bars ──────────────────────────────
        bar_width = 1.0
        bar_spacing = 1.6
        max_bar_height = 3.0
        bar_baseline_y = -1.5
        bar_start_x = -(n_options - 1) * bar_spacing / 2

        bars = VGroup()
        bar_labels = VGroup()
        util_labels = VGroup()

        for i in range(n_options):
            h = utilities[i] / utilities.max() * max_bar_height
            bar = Rectangle(
                width=bar_width,
                height=h,
                fill_color=bar_colors[i],
                fill_opacity=0.85,
                stroke_color=C["text_dim"],
                stroke_width=1,
            )
            x = bar_start_x + i * bar_spacing
            bar.move_to([x, bar_baseline_y + h / 2, 0])
            bars.add(bar)

            lbl = Text(option_labels[i], font_size=20, color=C["text_light"],
                       font="Times New Roman", weight=BOLD)
            lbl.next_to(bar, DOWN, buff=0.15)
            bar_labels.add(lbl)

            ulbl = Text(f"u={utilities[i]:.1f}", font_size=14, color=C["text_dim"],
                        font="Times New Roman")
            ulbl.next_to(bar, UP, buff=0.1)
            util_labels.add(ulbl)

        self.play(
            *[GrowFromEdge(b, DOWN) for b in bars],
            *[FadeIn(l) for l in bar_labels],
            *[FadeIn(u) for u in util_labels],
            run_time=0.8,
        )

        # ── Choice pointer (triangle) ────────────────────────────
        pointer = Triangle(
            fill_color=C["highlight"], fill_opacity=0.95,
            stroke_width=0,
        ).scale(0.18).rotate(PI)

        def pointer_above_bar(idx):
            bar = bars[idx]
            return bar.get_top() + UP * 0.55

        pointer.move_to(pointer_above_bar(2))  # start at best option
        self.play(FadeIn(pointer), run_time=0.3)

        # ── Beta tracker and label ────────────────────────────────
        beta_tracker = ValueTracker(0.0)

        beta_label = always_redraw(lambda: MathTex(
            r"\beta = " + (
                f"{beta_tracker.get_value():.1f}"
                if beta_tracker.get_value() < 20
                else r"\infty"
            ),
            font_size=34, color=C["amber"],
        ).move_to([0, 2.3, 0]))

        regime_label = always_redraw(lambda: Text(
            "Random" if beta_tracker.get_value() < 0.5
            else ("Noisy" if beta_tracker.get_value() < 5
                  else "Greedy"),
            font_size=22,
            color=(C["coral"] if beta_tracker.get_value() < 0.5
                   else (C["amber"] if beta_tracker.get_value() < 5
                         else C["teal"])),
            font="Times New Roman", weight=BOLD,
        ).move_to([0, 1.8, 0]))

        self.play(FadeIn(beta_label), FadeIn(regime_label), run_time=0.4)

        # ── Beta slider bar ───────────────────────────────────────
        slider_left = np.array([-4.5, -2.8, 0])
        slider_right = np.array([4.5, -2.8, 0])
        slider_line = Line(
            slider_left, slider_right,
            color=C["text_dim"], stroke_width=3,
        )
        slider_low = Text("0", font_size=14, color=C["text_dim"],
                          font="Times New Roman").next_to(slider_line, LEFT, buff=0.15)
        slider_high = MathTex(r"\infty", font_size=18, color=C["text_dim"]
                              ).next_to(slider_line, RIGHT, buff=0.15)
        slider_title = MathTex(r"\beta", font_size=22, color=C["text_dim"]
                               ).next_to(slider_line, DOWN, buff=0.15)

        slider_dot = always_redraw(lambda: Dot(
            point=slider_left + (slider_right - slider_left) * min(
                beta_tracker.get_value() / 25.0, 1.0
            ),
            color=C["highlight"], radius=0.12,
        ))

        self.play(
            Create(slider_line), FadeIn(slider_low), FadeIn(slider_high),
            FadeIn(slider_title), FadeIn(slider_dot),
            run_time=0.5,
        )

        # ── Helper: animate pointer bouncing based on beta ────────
        def bounce_pointer(beta_val, n_bounces=8, duration=2.0):
            """Simulate choices at given beta by bouncing the pointer."""
            if beta_val < 0.01:
                # Uniform random
                probs = np.ones(n_options) / n_options
            else:
                weights = np.exp(beta_val * utilities)
                probs = weights / weights.sum()

            choices = rng.choice(n_options, size=n_bounces, p=probs)
            per_bounce = duration / n_bounces
            for c in choices:
                self.play(
                    pointer.animate.move_to(pointer_above_bar(c)),
                    run_time=per_bounce,
                    rate_func=there_and_back_with_pause if per_bounce > 0.15 else linear,
                )

        # ── Phase 1: beta = 0 (random) ───────────────────────────
        self.play(beta_tracker.animate.set_value(0.1), run_time=0.5)
        bounce_pointer(0.1, n_bounces=10, duration=2.5)
        self.wait(0.3)

        # ── Phase 2: beta = 2 (noisy) ────────────────────────────
        self.play(beta_tracker.animate.set_value(2.0), run_time=1.0)
        bounce_pointer(2.0, n_bounces=10, duration=2.5)
        self.wait(0.3)

        # ── Phase 3: beta = 10 (nearly greedy) ───────────────────
        self.play(beta_tracker.animate.set_value(10.0), run_time=1.0)
        bounce_pointer(10.0, n_bounces=8, duration=2.0)
        self.wait(0.3)

        # ── Phase 4: beta -> infinity (greedy) ────────────────────
        self.play(beta_tracker.animate.set_value(25.0), run_time=1.5)
        # Pointer locks onto best option
        best_idx = int(np.argmax(utilities))
        self.play(pointer.animate.move_to(pointer_above_bar(best_idx)), run_time=0.5)

        lock_label = Text(
            "Always picks the best option",
            font_size=18, color=C["teal"],
            font="Times New Roman", weight=BOLD,
        ).next_to(pointer, UP, buff=0.25)
        self.play(FadeIn(lock_label), run_time=0.4)
        self.wait(2)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 3: Annealing Metaphor
# ─────────────────────────────────────────────────────────────────────
class AnnealingScene(BaseSearchnetScene):
    """
    Left: hot metal (particles bouncing). Right: cold metal (ordered lattice).
    Animate cooling with a temperature gauge. Map to search: exploration vs.
    exploitation.
    """

    scene_title = "Annealing: From Chaos to Order"
    scene_subtitle = "The metallurgy metaphor for search"

    N_PARTICLES = 30
    LATTICE_ROWS = 5
    LATTICE_COLS = 6

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993), Games and Economic Behavior")

        rng = np.random.default_rng(42)

        # ── Layout regions ────────────────────────────────────────
        block_width = 3.5
        block_height = 3.0
        left_center = np.array([-3.2, 0.0, 0])
        right_center = np.array([3.2, 0.0, 0])

        # ── Metal block outlines ──────────────────────────────────
        left_block = RoundedRectangle(
            width=block_width, height=block_height,
            corner_radius=0.15,
            stroke_color=C["coral"], stroke_width=2,
            fill_color=C["coral"], fill_opacity=0.08,
        ).move_to(left_center)

        right_block = RoundedRectangle(
            width=block_width, height=block_height,
            corner_radius=0.15,
            stroke_color=C["teal"], stroke_width=2,
            fill_color=C["teal"], fill_opacity=0.08,
        ).move_to(right_center)

        left_title = Text(
            "HOT", font_size=22, color=C["coral"],
            font="Times New Roman", weight=BOLD,
        ).next_to(left_block, UP, buff=0.15)

        right_title = Text(
            "COLD", font_size=22, color=C["teal"],
            font="Times New Roman", weight=BOLD,
        ).next_to(right_block, UP, buff=0.15)

        self.play(
            FadeIn(left_block), FadeIn(right_block),
            FadeIn(left_title), FadeIn(right_title),
            run_time=0.6,
        )

        # ── Reference images: hot = random particles, cold = lattice
        # Show the cold lattice (target) on the right immediately
        lattice_dots = VGroup()
        dx = (block_width - 0.8) / (self.LATTICE_COLS - 1)
        dy = (block_height - 0.8) / (self.LATTICE_ROWS - 1)
        for r in range(self.LATTICE_ROWS):
            for c_idx in range(self.LATTICE_COLS):
                x = right_center[0] - (block_width - 0.8) / 2 + c_idx * dx
                y = right_center[1] - (block_height - 0.8) / 2 + r * dy
                dot = Dot(
                    point=[x, y, 0], radius=0.08,
                    color=C["teal"], fill_opacity=0.85,
                )
                lattice_dots.add(dot)

        self.play(*[FadeIn(d) for d in lattice_dots], run_time=0.6)

        # ── Hot particles (random positions, will animate jiggling) ─
        particles = VGroup()
        particle_targets = []  # lattice positions for later convergence
        for i in range(self.N_PARTICLES):
            x = left_center[0] + rng.uniform(-block_width / 2 + 0.3, block_width / 2 - 0.3)
            y = left_center[1] + rng.uniform(-block_height / 2 + 0.3, block_height / 2 - 0.3)
            dot = Dot(
                point=[x, y, 0], radius=0.08,
                color=C["coral"], fill_opacity=0.85,
            )
            particles.add(dot)

        # Precompute lattice targets for these particles
        for i in range(self.N_PARTICLES):
            r = i // self.LATTICE_COLS
            c_idx = i % self.LATTICE_COLS
            if r < self.LATTICE_ROWS:
                x = left_center[0] - (block_width - 0.8) / 2 + c_idx * dx
                y = left_center[1] - (block_height - 0.8) / 2 + r * dy
                particle_targets.append(np.array([x, y, 0]))
            else:
                # Extra particles fade out
                particle_targets.append(None)

        self.play(*[FadeIn(p) for p in particles], run_time=0.5)

        # ── Temperature gauge (center) ────────────────────────────
        gauge_x = 0.0
        gauge_bottom = -1.5
        gauge_top = 1.5
        gauge_height = gauge_top - gauge_bottom

        gauge_outline = Rectangle(
            width=0.4, height=gauge_height,
            stroke_color=C["text_dim"], stroke_width=1.5,
            fill_opacity=0,
        ).move_to([gauge_x, (gauge_bottom + gauge_top) / 2, 0])

        gauge_label_hot = Text("HOT", font_size=12, color=C["coral"],
                               font="Times New Roman").next_to(gauge_outline, UP, buff=0.1)
        gauge_label_cold = Text("COLD", font_size=12, color=C["teal"],
                                font="Times New Roman").next_to(gauge_outline, DOWN, buff=0.1)

        temp_tracker = ValueTracker(1.0)  # 1.0 = hot, 0.0 = cold

        gauge_fill = always_redraw(lambda: Rectangle(
            width=0.35,
            height=max(temp_tracker.get_value() * gauge_height, 0.02),
            fill_color=interpolate_color(
                ManimColor(C["teal"]), ManimColor(C["coral"]),
                temp_tracker.get_value(),
            ),
            fill_opacity=0.8,
            stroke_width=0,
        ).move_to([
            gauge_x,
            gauge_bottom + max(temp_tracker.get_value() * gauge_height, 0.02) / 2,
            0,
        ]))

        self.play(
            FadeIn(gauge_outline), FadeIn(gauge_label_hot),
            FadeIn(gauge_label_cold), FadeIn(gauge_fill),
            run_time=0.5,
        )

        # ── Phase 1: Jiggle hot particles ─────────────────────────
        hot_label = Text(
            "Exploration — particles move freely",
            font_size=16, color=C["coral"],
            font="Times New Roman", slant=ITALIC,
        ).to_edge(DOWN, buff=0.3)
        self.play(FadeIn(hot_label), run_time=0.3)

        # Jiggle: several rounds of random displacement
        for _ in range(4):
            jiggle_anims = []
            for p in particles:
                dx_j = rng.uniform(-0.35, 0.35)
                dy_j = rng.uniform(-0.35, 0.35)
                new_pos = p.get_center() + np.array([dx_j, dy_j, 0])
                # Clamp to block bounds
                new_pos[0] = np.clip(new_pos[0],
                                     left_center[0] - block_width / 2 + 0.15,
                                     left_center[0] + block_width / 2 - 0.15)
                new_pos[1] = np.clip(new_pos[1],
                                     left_center[1] - block_height / 2 + 0.15,
                                     left_center[1] + block_height / 2 - 0.15)
                jiggle_anims.append(p.animate.move_to(new_pos))
            self.play(*jiggle_anims, run_time=0.4)

        self.wait(0.5)
        self.play(FadeOut(hot_label), run_time=0.3)

        # ── Phase 2: Cool down — particles slow and organize ──────
        cooling_label = Text(
            "Cooling down...",
            font_size=16, color=C["amber"],
            font="Times New Roman", slant=ITALIC,
        ).to_edge(DOWN, buff=0.3)
        self.play(FadeIn(cooling_label), run_time=0.3)

        # Gradual cooling in 5 stages
        temp_stages = [0.8, 0.6, 0.4, 0.2, 0.0]
        jiggle_scales = [0.25, 0.18, 0.10, 0.05, 0.0]

        for stage_idx, (temp_val, jiggle_scale) in enumerate(
            zip(temp_stages, jiggle_scales)
        ):
            move_anims = [temp_tracker.animate.set_value(temp_val)]

            for i, p in enumerate(particles):
                target = particle_targets[i]
                if target is None:
                    continue
                # Interpolate toward lattice position with jiggle
                current = p.get_center()
                blend = 1.0 - temp_val  # blend toward target as temp drops
                new_pos = current * (1 - blend * 0.4) + target * (blend * 0.4)
                if jiggle_scale > 0:
                    new_pos[:2] += rng.uniform(-jiggle_scale, jiggle_scale, size=2)
                # Final stage: snap to lattice
                if stage_idx == len(temp_stages) - 1:
                    new_pos = target.copy()
                new_pos[0] = np.clip(new_pos[0],
                                     left_center[0] - block_width / 2 + 0.15,
                                     left_center[0] + block_width / 2 - 0.15)
                new_pos[1] = np.clip(new_pos[1],
                                     left_center[1] - block_height / 2 + 0.15,
                                     left_center[1] + block_height / 2 - 0.15)
                move_anims.append(p.animate.move_to(new_pos))

            self.play(*move_anims, run_time=0.8)

        self.play(FadeOut(cooling_label), run_time=0.3)

        # ── Change left block color to match cold ─────────────────
        cooled_title = Text(
            "COOLED", font_size=22, color=C["teal"],
            font="Times New Roman", weight=BOLD,
        ).next_to(left_block, UP, buff=0.15)

        self.play(
            left_block.animate.set_stroke(color=C["teal"]).set_fill(
                color=C["teal"], opacity=0.08
            ),
            Transform(left_title, cooled_title),
            *[p.animate.set_color(C["teal"]) for p in particles],
            run_time=0.8,
        )

        # ── Parallel labels ───────────────────────────────────────
        search_label = Text(
            "Hot = Exploration     Cold = Exploitation",
            font_size=20, color=C["highlight"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.3)
        self.play(FadeIn(search_label), run_time=0.5)
        self.wait(2.5)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)


# ─────────────────────────────────────────────────────────────────────
# Scene 4: Convergence Demo
# ─────────────────────────────────────────────────────────────────────
class ConvergenceDemoScene(BaseSearchnetScene):
    """
    Two side-by-side grids: one starts SPARSE (few active cells),
    one starts DENSE (many active cells). Both evolve via logit
    best-response and converge to the same equilibrium pattern.
    """

    scene_title = "Convergence to Equilibrium"
    scene_subtitle = "Different starts, same destination"

    GRID_SIZE = 8
    CELL_SIZE = 0.42
    BETA = 2.5
    N_STEPS = 300

    def construct(self):
        self.setup()
        self.add_title()
        source = self.add_source("Blume (1993), Games and Economic Behavior")

        rng = np.random.default_rng(42)
        colors = [C["teal"], C["coral"]]

        title = Text(
            "Two Different Starting Points",
            font_size=24, color=C["text_light"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(UP, buff=0.35)
        self.play(FadeIn(title), run_time=0.4)

        # ── Create two grids with different initial conditions ────
        # Sparse: ~15% active (state 1)
        grid_sparse = (rng.random((self.GRID_SIZE, self.GRID_SIZE)) < 0.15).astype(int)
        # Dense: ~85% active (state 1)
        grid_dense = (rng.random((self.GRID_SIZE, self.GRID_SIZE)) < 0.85).astype(int)

        def build_grid_display(grid, x_offset, label_text):
            cells = {}
            group = VGroup()
            for r in range(self.GRID_SIZE):
                for cc in range(self.GRID_SIZE):
                    sq = Square(
                        side_length=self.CELL_SIZE,
                        fill_opacity=0.85,
                        stroke_width=0.4,
                        stroke_color=C["bg_panel"],
                        fill_color=colors[grid[r, cc]],
                    )
                    x = (cc - self.GRID_SIZE / 2 + 0.5) * self.CELL_SIZE + x_offset
                    y = (self.GRID_SIZE / 2 - 0.5 - r) * self.CELL_SIZE - 0.3
                    sq.move_to([x, y, 0])
                    cells[(r, cc)] = sq
                    group.add(sq)

            label = Text(
                label_text, font_size=16, color=C["text_dim"],
                font="Times New Roman",
            ).next_to(group, DOWN, buff=0.2)
            return cells, group, label

        cells_s, group_s, label_s = build_grid_display(
            grid_sparse, -3.3, "Sparse start (~15%)"
        )
        cells_d, group_d, label_d = build_grid_display(
            grid_dense, 3.3, "Dense start (~85%)"
        )

        # Divider
        divider = DashedLine(
            UP * 2.2, DOWN * 2.8,
            color=C["text_dim"], stroke_width=1, dash_length=0.1,
        )

        self.play(
            FadeIn(group_s), FadeIn(group_d),
            FadeIn(label_s), FadeIn(label_d),
            Create(divider),
            run_time=0.8,
        )
        self.wait(0.5)

        # ── Beta label ────────────────────────────────────────────
        beta_label = MathTex(
            r"\beta = " + f"{self.BETA:.1f}",
            font_size=22, color=C["amber"],
        ).move_to([0, -2.8, 0])
        self.play(FadeIn(beta_label), run_time=0.3)

        # ── Step counter ──────────────────────────────────────────
        step_tracker = ValueTracker(0)
        step_label = always_redraw(lambda: Text(
            f"Step {int(step_tracker.get_value())}",
            font_size=14, color=C["text_dim"],
            font="Times New Roman",
        ).move_to([0, 2.0, 0]))
        self.play(FadeIn(step_label), run_time=0.2)

        # ── Ising step helper (reuse from blume_gibbs) ────────────
        def ising_step_local(grid, beta, rng_local):
            rows, cols = grid.shape
            r = rng_local.integers(rows)
            cc = rng_local.integers(cols)
            current = grid[r, cc]
            neighbor_sum = 0
            for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                nr, nc = r + dr, cc + dc
                if 0 <= nr < rows and 0 <= nc < cols:
                    neighbor_sum += (1 if grid[nr, nc] == current else -1)
            delta_phi = -2.0 * neighbor_sum
            p_flip = np.exp(beta * delta_phi) / (1.0 + np.exp(beta * delta_phi))
            if rng_local.random() < p_flip:
                grid[r, cc] = 1 - current
            return grid, (r, cc)

        # ── Run dynamics in parallel on both grids ────────────────
        rng_s = np.random.default_rng(100)
        rng_d = np.random.default_rng(200)

        batch_size = 15
        n_batches = self.N_STEPS // batch_size

        for batch_idx in range(n_batches):
            anims = []
            for _ in range(batch_size):
                grid_sparse, (r, cc) = ising_step_local(grid_sparse, self.BETA, rng_s)
                anims.append(cells_s[(r, cc)].animate.set_fill(colors[grid_sparse[r, cc]]))

                grid_dense, (r2, cc2) = ising_step_local(grid_dense, self.BETA, rng_d)
                anims.append(cells_d[(r2, cc2)].animate.set_fill(colors[grid_dense[r2, cc2]]))

            step_val = (batch_idx + 1) * batch_size
            anims.append(step_tracker.animate.set_value(step_val))
            self.play(*anims, run_time=0.2)

        self.wait(0.5)

        # ── Compute similarity between the two final grids ────────
        agreement = np.mean(grid_sparse == grid_dense) * 100

        # ── Highlight convergence ─────────────────────────────────
        # Update labels
        new_label_s = Text(
            "Final state (sparse start)", font_size=14,
            color=C["teal"], font="Times New Roman",
        ).next_to(group_s, DOWN, buff=0.2)
        new_label_d = Text(
            "Final state (dense start)", font_size=14,
            color=C["teal"], font="Times New Roman",
        ).next_to(group_d, DOWN, buff=0.2)
        self.play(
            Transform(label_s, new_label_s),
            Transform(label_d, new_label_d),
            run_time=0.5,
        )

        # ── Punchline ────────────────────────────────────────────
        punchline = Text(
            "Same equilibrium regardless of starting point",
            font_size=22, color=C["highlight"],
            font="Times New Roman", weight=BOLD,
        ).to_edge(DOWN, buff=0.2)

        # Arrow connecting both grids
        arrow = DoubleArrow(
            group_s.get_right() + RIGHT * 0.2,
            group_d.get_left() + LEFT * 0.2,
            color=C["highlight"], stroke_width=3,
            buff=0.1,
        )
        converge_label = MathTex(
            r"\to \pi_\beta",
            font_size=28, color=C["highlight"],
        ).next_to(arrow, UP, buff=0.1)

        self.play(
            FadeIn(punchline),
            GrowFromCenter(arrow),
            FadeIn(converge_label),
            run_time=0.8,
        )
        self.wait(3)
        self.play(*[FadeOut(m) for m in self.mobjects], run_time=0.8)
