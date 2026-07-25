"""
Scene: 3D Phase Space Trajectories
====================================
ThreeDScene showing actor trajectories through the
Network-Behavior-Fitness phase space. Reads CSV exported
by searchnet_export_phase_space() in R.

Render:
    manim -qh scene_phase_space.py PhaseSpaceScene
    manim -ql scene_phase_space.py PhaseSpaceScene

Input CSV expected columns:
    chain_step_id, actor_id, strategy, time_norm, phase,
    <x_var>, <y_var>, <z_var>

Override defaults via subclass attributes or before render:
    PhaseSpaceScene.csv_path = "my_data.csv"
    PhaseSpaceScene.x_col = "K_AC"
    PhaseSpaceScene.y_col = "exploration_rate"
    PhaseSpaceScene.z_col = "utility"
"""

from manim import *
import numpy as np
import csv
from pathlib import Path
from searchnet_viz import SEARCHNET_COLORS, BaseSearchnetScene

C = SEARCHNET_COLORS

# Strategy-to-color mapping (matches R palette order)
STRATEGY_COLORS = [
    C["navy"], C["teal"], C["coral"], C["amber"],
    C["slate"], C["sage"], C["plum"], C["steel"],
]


def _load_phase_csv(csv_path):
    """Load phase space CSV into structured dict keyed by actor_id."""
    path = Path(csv_path)
    if not path.exists():
        raise FileNotFoundError(f"Phase space CSV not found: {path}")
    with open(path, "r", newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows = list(reader)
    # Group by actor_id
    actors = {}
    for row in rows:
        aid = int(float(row["actor_id"]))
        if aid not in actors:
            actors[aid] = {
                "strategy": row.get("strategy", "1"),
                "steps": [],
            }
        actors[aid]["steps"].append(row)
    # Sort each actor's steps by chain_step_id
    for aid in actors:
        actors[aid]["steps"].sort(key=lambda r: float(r["chain_step_id"]))
    return actors


def _normalize(values, margin=0.05):
    """Normalize values to [-3, 3] range for manim coordinates."""
    arr = np.array(values, dtype=float)
    vmin, vmax = np.nanmin(arr), np.nanmax(arr)
    span = vmax - vmin if vmax > vmin else 1.0
    return (arr - vmin) / span * 6.0 - 3.0


class PhaseSpaceScene(ThreeDScene):
    """
    3D Phase Space with actor trajectories as colored spheres
    moving through Network-Behavior-Fitness space.

    Configuration attributes (override before render):
        csv_path    — path to CSV from searchnet_export_phase_space()
        x_col       — column name for X axis (Network dimension)
        y_col       — column name for Y axis (Behavior dimension)
        z_col       — column name for Z axis (Fitness dimension)
        x_label     — axis label override
        y_label     — axis label override
        z_label     — axis label override
        trail_fade  — number of past steps visible as trail (default 15)
        sphere_radius — radius of actor spheres (default 0.12)
        show_shadows — project shadows onto XY, XZ, YZ planes (default True)
        camera_rotation_rate — degrees per second of camera orbit
    """

    csv_path = "phase_space_data.csv"
    x_col = "K_AC"
    y_col = "exploration_rate"
    z_col = "utility"
    x_label = "Network"
    y_label = "Behavior"
    z_label = "Fitness"
    trail_fade = 15
    sphere_radius = 0.12
    show_shadows = True
    camera_rotation_rate = 8  # degrees per second

    def construct(self):
        self.camera.background_color = C["bg_dark"]

        # ── Title card ──────────────────────────────────────────────
        title = Text(
            "Phase Space Trajectories",
            font_size=36,
            color=C["text_light"],
            font="Times New Roman",
            weight=BOLD,
        )
        subtitle = Text(
            f"{self.x_label} / {self.y_label} / {self.z_label}",
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
        self.wait(1.0)
        self.play(FadeOut(title_group), run_time=0.5)
        self.remove(title_group)

        # ── Load data ──────────────────────────────────────────────
        actors = _load_phase_csv(self.csv_path)
        if not actors:
            raise ValueError("No actor data found in CSV.")

        # Collect all values for normalization
        all_x, all_y, all_z = [], [], []
        for aid, adata in actors.items():
            for step in adata["steps"]:
                try:
                    all_x.append(float(step[self.x_col]))
                    all_y.append(float(step[self.y_col]))
                    all_z.append(float(step[self.z_col]))
                except (ValueError, KeyError):
                    pass

        x_norm_all = _normalize(all_x)
        y_norm_all = _normalize(all_y)
        z_norm_all = _normalize(all_z)

        # Build per-actor normalized coordinate arrays
        actor_trajectories = {}
        idx = 0
        for aid in sorted(actors.keys()):
            coords = []
            for step in actors[aid]["steps"]:
                try:
                    float(step[self.x_col])
                    coords.append(np.array([
                        x_norm_all[idx],
                        y_norm_all[idx],
                        z_norm_all[idx],
                    ]))
                    idx += 1
                except (ValueError, KeyError):
                    idx += 1
                    continue
            if coords:
                actor_trajectories[aid] = {
                    "coords": coords,
                    "strategy": actors[aid]["strategy"],
                }

        # Map strategies to colors
        unique_strats = sorted(set(
            v["strategy"] for v in actor_trajectories.values()
        ))
        strat_color_map = {}
        for i, s in enumerate(unique_strats):
            strat_color_map[s] = STRATEGY_COLORS[i % len(STRATEGY_COLORS)]

        # ── Build 3D axes ──────────────────────────────────────────
        axes = ThreeDAxes(
            x_range=[-3.5, 3.5, 1],
            y_range=[-3.5, 3.5, 1],
            z_range=[-3.5, 3.5, 1],
            x_length=7,
            y_length=7,
            z_length=7,
            axis_config={
                "color": C["text_dim"],
                "stroke_width": 1.5,
                "include_tip": True,
                "tip_length": 0.15,
            },
        )

        # Axis labels
        x_lab = Text(self.x_label, font_size=14, color=C["text_dim"],
                     font="Times New Roman")
        x_lab.rotate(PI / 2, axis=RIGHT).move_to(np.array([3.8, 0, -3.8]))

        y_lab = Text(self.y_label, font_size=14, color=C["text_dim"],
                     font="Times New Roman")
        y_lab.rotate(PI / 2, axis=RIGHT).rotate(PI / 2, axis=OUT)
        y_lab.move_to(np.array([0, 3.8, -3.8]))

        z_lab = Text(self.z_label, font_size=14, color=C["text_dim"],
                     font="Times New Roman")
        z_lab.rotate(PI / 2, axis=RIGHT)
        z_lab.move_to(np.array([-3.8, 0, 0]))

        # Camera setup
        self.set_camera_orientation(
            phi=70 * DEGREES, theta=-50 * DEGREES, zoom=0.65
        )

        self.play(Create(axes), run_time=1.0)
        self.add(x_lab, y_lab, z_lab)
        self.wait(0.3)

        # ── Strategy legend (fixed in frame) ───────────────────────
        legend_items = []
        for s in unique_strats:
            dot = Dot(radius=0.06, color=strat_color_map[s])
            label = Text(f"Strategy {s}", font_size=11,
                         color=C["text_light"], font="Times New Roman")
            label.next_to(dot, RIGHT, buff=0.1)
            legend_items.append(VGroup(dot, label))

        if legend_items:
            legend = VGroup(*legend_items).arrange(DOWN, buff=0.1, aligned_edge=LEFT)
            legend.to_corner(UL, buff=0.4)
            self.add_fixed_in_frame_mobjects(legend)
            self.play(FadeIn(legend), run_time=0.5)

        # ── Determine animation timeline ───────────────────────────
        max_steps = max(len(v["coords"]) for v in actor_trajectories.values())

        # Create spheres at initial positions
        spheres = {}
        trails = {}  # actor_id -> list of Line mobjects
        shadow_dots = {}  # actor_id -> dict of plane -> Dot

        for aid, tdata in actor_trajectories.items():
            col = strat_color_map[tdata["strategy"]]
            pos = tdata["coords"][0]
            sphere = Sphere(
                radius=self.sphere_radius,
                resolution=(8, 8),
            ).set_color(col).set_opacity(0.9)
            sphere.move_to(pos)
            spheres[aid] = sphere
            trails[aid] = []

            # Shadow dots on three planes
            if self.show_shadows:
                shadow_dots[aid] = {
                    "xy": Dot3D(
                        point=np.array([pos[0], pos[1], -3.5]),
                        radius=0.04, color=col,
                    ).set_opacity(0.2),
                    "xz": Dot3D(
                        point=np.array([pos[0], -3.5, pos[2]]),
                        radius=0.04, color=col,
                    ).set_opacity(0.2),
                    "yz": Dot3D(
                        point=np.array([-3.5, pos[1], pos[2]]),
                        radius=0.04, color=col,
                    ).set_opacity(0.2),
                }

        # Add all initial mobjects
        for aid in spheres:
            self.add(spheres[aid])
            if self.show_shadows:
                for plane_dot in shadow_dots[aid].values():
                    self.add(plane_dot)

        self.wait(0.5)

        # ── Animate trajectory step by step ────────────────────────
        # Determine step duration based on total steps
        total_anim_time = min(30, max(5, max_steps * 0.15))
        step_duration = total_anim_time / max(1, max_steps - 1)

        for t in range(1, max_steps):
            anims = []

            for aid, tdata in actor_trajectories.items():
                if t >= len(tdata["coords"]):
                    continue

                col = strat_color_map[tdata["strategy"]]
                old_pos = tdata["coords"][t - 1]
                new_pos = tdata["coords"][t]

                # Move sphere
                anims.append(spheres[aid].animate.move_to(new_pos))

                # Add trail segment
                trail_line = Line3D(
                    start=old_pos,
                    end=new_pos,
                    thickness=0.02,
                    color=col,
                ).set_opacity(0.7)
                trails[aid].append(trail_line)
                self.add(trail_line)

                # Fade old trails beyond trail_length
                if len(trails[aid]) > self.trail_fade:
                    old_trail = trails[aid][-self.trail_fade - 1]
                    old_trail.set_opacity(0.0)

                # Update shadow projections
                if self.show_shadows:
                    shadow_dots[aid]["xy"].move_to(
                        np.array([new_pos[0], new_pos[1], -3.5])
                    )
                    shadow_dots[aid]["xz"].move_to(
                        np.array([new_pos[0], -3.5, new_pos[2]])
                    )
                    shadow_dots[aid]["yz"].move_to(
                        np.array([-3.5, new_pos[1], new_pos[2]])
                    )

            if anims:
                self.play(*anims, run_time=step_duration, rate_func=linear)

            # Slow camera rotation
            if self.camera_rotation_rate > 0:
                self.camera.increment_theta(
                    self.camera_rotation_rate * step_duration * DEGREES
                )

        # ── Final hold with full rotation ──────────────────────────
        self.wait(0.5)

        # Slow 360 rotation to reveal 3D structure
        self.begin_ambient_camera_rotation(rate=0.3)
        self.wait(6)
        self.stop_ambient_camera_rotation()

        # ── Source annotation ──────────────────────────────────────
        source = Text(
            "SearchNet (SaoMNK) Phase Space",
            font_size=11,
            color=C["text_dim"],
            slant=ITALIC,
        ).to_corner(DR, buff=0.25)
        self.add_fixed_in_frame_mobjects(source)
        self.play(FadeIn(source), run_time=0.3)
        self.wait(2)


class PhaseSpaceComparisonScene(ThreeDScene):
    """
    Side-by-side or overlay comparison of two phase space CSVs.

    Configuration:
        csv_path_a, csv_path_b — paths to the two CSV files
        label_a, label_b       — condition labels
        x_col, y_col, z_col    — column names
    """

    csv_path_a = "phase_space_baseline.csv"
    csv_path_b = "phase_space_shocked.csv"
    label_a = "Baseline"
    label_b = "Shocked"
    x_col = "K_AC"
    y_col = "exploration_rate"
    z_col = "utility"
    x_label = "Network"
    y_label = "Behavior"
    z_label = "Fitness"

    def construct(self):
        self.camera.background_color = C["bg_dark"]

        # Title
        title = Text(
            f"Phase Space: {self.label_a} vs {self.label_b}",
            font_size=32,
            color=C["text_light"],
            font="Times New Roman",
            weight=BOLD,
        ).move_to(ORIGIN)
        self.add_fixed_in_frame_mobjects(title)
        self.play(Write(title), run_time=0.8)
        self.wait(1)
        self.play(FadeOut(title), run_time=0.5)
        self.remove(title)

        # Load both datasets
        actors_a = _load_phase_csv(self.csv_path_a)
        actors_b = _load_phase_csv(self.csv_path_b)

        # Collect all values for shared normalization
        all_vals = {"x": [], "y": [], "z": []}
        for actors in [actors_a, actors_b]:
            for aid, adata in actors.items():
                for step in adata["steps"]:
                    try:
                        all_vals["x"].append(float(step[self.x_col]))
                        all_vals["y"].append(float(step[self.y_col]))
                        all_vals["z"].append(float(step[self.z_col]))
                    except (ValueError, KeyError):
                        pass

        x_norm = _normalize(all_vals["x"])
        y_norm = _normalize(all_vals["y"])
        z_norm = _normalize(all_vals["z"])

        # Build axes
        axes = ThreeDAxes(
            x_range=[-3.5, 3.5, 1],
            y_range=[-3.5, 3.5, 1],
            z_range=[-3.5, 3.5, 1],
            x_length=7, y_length=7, z_length=7,
            axis_config={"color": C["text_dim"], "stroke_width": 1.5},
        )

        self.set_camera_orientation(
            phi=70 * DEGREES, theta=-50 * DEGREES, zoom=0.65
        )
        self.play(Create(axes), run_time=1.0)

        # Plot condition A as teal point cloud
        idx = 0
        points_a = VGroup()
        for aid in sorted(actors_a.keys()):
            for step in actors_a[aid]["steps"]:
                try:
                    float(step[self.x_col])
                    pos = np.array([x_norm[idx], y_norm[idx], z_norm[idx]])
                    dot = Dot3D(point=pos, radius=0.04, color=C["teal"])
                    dot.set_opacity(0.3)
                    points_a.add(dot)
                    idx += 1
                except (ValueError, KeyError):
                    idx += 1

        # Plot condition B as coral point cloud
        points_b = VGroup()
        for aid in sorted(actors_b.keys()):
            for step in actors_b[aid]["steps"]:
                try:
                    float(step[self.x_col])
                    pos = np.array([x_norm[idx], y_norm[idx], z_norm[idx]])
                    dot = Dot3D(point=pos, radius=0.04, color=C["coral"])
                    dot.set_opacity(0.3)
                    points_b.add(dot)
                    idx += 1
                except (ValueError, KeyError):
                    idx += 1

        self.play(FadeIn(points_a), run_time=1.5)
        self.play(FadeIn(points_b), run_time=1.5)

        # Legend
        leg_a = VGroup(
            Dot(radius=0.06, color=C["teal"]),
            Text(self.label_a, font_size=12, color=C["text_light"],
                 font="Times New Roman"),
        ).arrange(RIGHT, buff=0.1)
        leg_b = VGroup(
            Dot(radius=0.06, color=C["coral"]),
            Text(self.label_b, font_size=12, color=C["text_light"],
                 font="Times New Roman"),
        ).arrange(RIGHT, buff=0.1)
        legend = VGroup(leg_a, leg_b).arrange(DOWN, buff=0.1, aligned_edge=LEFT)
        legend.to_corner(UL, buff=0.4)
        self.add_fixed_in_frame_mobjects(legend)
        self.play(FadeIn(legend), run_time=0.5)

        # Rotate to reveal structure
        self.begin_ambient_camera_rotation(rate=0.25)
        self.wait(10)
        self.stop_ambient_camera_rotation()
        self.wait(1)
