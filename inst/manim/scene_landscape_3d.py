"""
Scene: 3D Fitness Landscape
=============================
ThreeDScene showing a rugged NK fitness surface with colored spheres
(agents) climbing the landscape. Trails show search paths. Camera
rotation reveals landscape structure. Optionally shows how the
landscape deforms with endogenous effects.

Render:
    manim -qh scene_landscape_3d.py LandscapeScene
    manim -ql scene_landscape_3d.py LandscapeScene
"""

from manim import *
import numpy as np
from searchnet_viz import SEARCHNET_COLORS, nk_fitness_values, nk_surface_function

C = SEARCHNET_COLORS


class LandscapeScene(ThreeDScene):
    """3D NK fitness landscape with agents climbing and search trails."""

    # Configuration — override via subclass or before render
    N = 6
    K = 2
    seed = 42
    n_agents = 5
    n_climb_steps = 25
    show_endogenous_deformation = True

    def construct(self):
        self.camera.background_color = C["bg_dark"]

        # ── Title card ──────────────────────────────────────────────
        title = Text(
            "NK Fitness Landscape",
            font_size=36,
            color=C["text_light"],
            font="Times New Roman",
            weight=BOLD,
        )
        subtitle = Text(
            f"N={self.N}, K={self.K} — agents searching for high-fitness configurations",
            font_size=18,
            color=C["text_dim"],
            font="Times New Roman",
            slant=ITALIC,
        )
        subtitle.next_to(title, DOWN, buff=0.15)
        title_group = VGroup(title, subtitle).move_to(ORIGIN)

        self.add_fixed_in_frame_mobjects(title_group)
        self.play(Write(title), run_time=0.8)
        self.play(FadeIn(subtitle), run_time=0.4)
        self.wait(1)
        self.play(FadeOut(title_group), run_time=0.5)
        self.remove(title_group)

        # ── Generate NK landscape ───────────────────────────────────
        fitness = nk_fitness_values(N=self.N, K=self.K, seed=self.seed)
        resolution = 40

        def surface_func(u, v, deform=0.0):
            z = nk_surface_function(u, v, fitness, smoothing=0.25)
            # Endogenous deformation: convergence flattens some peaks
            if deform > 0:
                z += deform * 0.3 * np.sin(3 * u * np.pi) * np.cos(3 * v * np.pi)
            return np.array([
                (u - 0.5) * 6,
                (v - 0.5) * 6,
                (z - 0.4) * 4,
            ])

        # ── Build 3D surface ────────────────────────────────────────
        surface = Surface(
            lambda u, v: surface_func(u, v),
            u_range=[0, 1],
            v_range=[0, 1],
            resolution=(resolution, resolution),
            fill_opacity=0.7,
            stroke_width=0.3,
            stroke_color=C["text_dim"],
        )
        surface.set_color_by_gradient(C["navy"], C["teal"], C["amber"])

        # Camera setup
        self.set_camera_orientation(phi=65 * DEGREES, theta=-45 * DEGREES, zoom=0.7)

        self.play(Create(surface), run_time=2.0)

        # ── Axis labels ─────────────────────────────────────────────
        x_label = Text("Configuration Dim 1", font_size=13, color=C["text_dim"])
        x_label.rotate(PI / 2, axis=RIGHT).move_to(np.array([0, -3.5, -1.5]))

        y_label = Text("Configuration Dim 2", font_size=13, color=C["text_dim"])
        y_label.rotate(PI / 2, axis=RIGHT).rotate(PI / 2, axis=OUT)
        y_label.move_to(np.array([-3.5, 0, -1.0]))

        fitness_label = Text("Fitness", font_size=13, color=C["text_dim"])
        fitness_label.rotate(PI / 2, axis=RIGHT).rotate(PI / 2, axis=OUT)
        fitness_label.move_to(np.array([-3.5, 0, 0.5]))

        self.add_fixed_orientation_mobjects(x_label, y_label, fitness_label)
        self.play(FadeIn(x_label), FadeIn(y_label), FadeIn(fitness_label), run_time=0.5)

        # ── Create agents ───────────────────────────────────────────
        agent_colors = [C["coral"], C["teal"], C["amber"], C["slate"], C["sage"]]
        agent_labels_text = [f"Agent {i+1}" for i in range(self.n_agents)]

        rng = np.random.default_rng(123)
        starts = rng.uniform(0.1, 0.9, size=(self.n_agents, 2))

        agents = []
        trails = []

        for i in range(self.n_agents):
            u, v = starts[i]
            pos = surface_func(u, v)
            sphere = Sphere(radius=0.12, color=agent_colors[i % len(agent_colors)])
            sphere.set_opacity(0.9)
            sphere.move_to(pos + np.array([0, 0, 0.15]))

            label = Text(
                agent_labels_text[i],
                font_size=10,
                color=WHITE,
                weight=BOLD,
            )
            label.move_to(pos + np.array([0, 0, 0.4]))
            self.add_fixed_orientation_mobjects(label)

            agents.append({"sphere": sphere, "label": label, "u": u, "v": v})
            trails.append([pos.copy()])

        self.play(
            *[GrowFromCenter(a["sphere"]) for a in agents],
            *[FadeIn(a["label"]) for a in agents],
            run_time=0.8,
        )

        # ── Climbing animation ──────────────────────────────────────
        phase_label = Text("Phase 1: Local Search", font_size=18, color=C["highlight"])
        phase_label.to_edge(DOWN, buff=0.5)
        self.add_fixed_in_frame_mobjects(phase_label)
        self.play(FadeIn(phase_label), run_time=0.3)

        for step in range(self.n_climb_steps):
            anims = []
            for i, agent in enumerate(agents):
                u, v = agent["u"], agent["v"]

                # Stochastic hill-climbing with small perturbation
                du = rng.uniform(-0.04, 0.04)
                dv = rng.uniform(-0.04, 0.04)

                f_curr = nk_surface_function(u, v, fitness, 0.25)
                u_cand = np.clip(u + du, 0.02, 0.98)
                v_cand = np.clip(v + dv, 0.02, 0.98)
                f_cand = nk_surface_function(u_cand, v_cand, fitness, 0.25)

                # Accept uphill moves; occasionally accept downhill (exploration)
                if f_cand >= f_curr or rng.random() < 0.15:
                    u_new, v_new = u_cand, v_cand
                else:
                    u_new, v_new = u, v

                new_pos = surface_func(u_new, v_new)
                agent["u"], agent["v"] = u_new, v_new

                anims.append(agent["sphere"].animate.move_to(new_pos + np.array([0, 0, 0.15])))
                anims.append(agent["label"].animate.move_to(new_pos + np.array([0, 0, 0.4])))

                # Trail segment
                old_pos = trails[i][-1]
                trail_line = Line(
                    old_pos + np.array([0, 0, 0.15]),
                    new_pos + np.array([0, 0, 0.15]),
                    color=agent_colors[i % len(agent_colors)],
                    stroke_width=1.5,
                    stroke_opacity=0.5,
                )
                self.add(trail_line)
                trails[i].append(new_pos.copy())

            self.play(*anims, run_time=0.12)

        # ── Camera rotation ─────────────────────────────────────────
        self.play(FadeOut(phase_label), run_time=0.3)
        self.begin_ambient_camera_rotation(rate=0.15)
        self.wait(3)
        self.stop_ambient_camera_rotation()

        # ── Endogenous deformation ──────────────────────────────────
        if self.show_endogenous_deformation:
            deform_label = Text(
                "Endogenous effects reshape the landscape",
                font_size=18,
                color=C["warning"],
                font="Times New Roman",
                slant=ITALIC,
            )
            deform_label.to_edge(DOWN, buff=0.5)
            self.add_fixed_in_frame_mobjects(deform_label)
            self.play(FadeIn(deform_label), run_time=0.5)

            # Morph surface with increasing deformation
            for deform_level in [0.2, 0.5, 0.8]:
                new_surface = Surface(
                    lambda u, v, d=deform_level: surface_func(u, v, deform=d),
                    u_range=[0, 1],
                    v_range=[0, 1],
                    resolution=(resolution, resolution),
                    fill_opacity=0.7,
                    stroke_width=0.3,
                    stroke_color=C["text_dim"],
                )
                new_surface.set_color_by_gradient(C["navy"], C["teal"], C["coral"])
                self.play(Transform(surface, new_surface), run_time=1.5)

            self.wait(1)
            self.play(FadeOut(deform_label), run_time=0.3)

        # ── Final camera sweep ──────────────────────────────────────
        self.begin_ambient_camera_rotation(rate=0.2)
        self.wait(3)
        self.stop_ambient_camera_rotation()

        self.wait(2)
