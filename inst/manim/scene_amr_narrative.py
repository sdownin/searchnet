"""
AMR Paper Full Narrative: Basin Geometry Under Industrial Policy and Erosion
=============================================================================
"Same Peak, Different Basin: How Industrial Policy Shapes the Hidden
Geometry of Competitive Search"

A 6-phase cinematic animation (~90s) showing one firm navigating a
rugged fitness landscape under different policy regimes and institutional
erosion levels. Builds on the CD2026 Convergence Climb V2 design.

Phase 1: The Landscape (10s) — introduce the rugged NK surface
Phase 2: Baseline Search (10s) — one firm climbs without policy
Phase 3: Four Policies (20s) — same start, 4 different basin destinations
Phase 4: Erosion Reshapes (15s) — landscape morphs as institutions erode
Phase 5: Policy × Erosion (20s) — which policy survives which erosion?
Phase 6: The Insight (10s) — "No universal best; architecture determines basin"

Render:
    manim -ql scene_amr_narrative.py BasinNarrativeScene    # preview
    manim -qh scene_amr_narrative.py BasinNarrativeScene    # publication
"""

from manim import *
import numpy as np

try:
    from searchnet_viz import SEARCHNET_COLORS, nk_fitness_values, nk_surface_function
    C = SEARCHNET_COLORS
except ImportError:
    C = {
        "bg_dark": "#0f1729", "navy": "#1a365d", "teal": "#2a9d8f",
        "amber": "#d69e2e", "terra": "#e76f51", "plum": "#7b2d8e",
        "steel": "#4a7c8f", "sage": "#6b8e6b", "text_light": "#e8e8e8",
        "text_dim": "#8899aa", "highlight": "#f4d35e", "coral": "#e76f51",
    }
    def nk_fitness_values(N=6, K=2, seed=42):
        rng = np.random.default_rng(seed)
        return {tuple(int(b) for b in format(i, f'0{N}b')): rng.random()
                for i in range(2**N)}
    def nk_surface_function(u, v, fitness, smoothing=0.25):
        return (0.3 + 0.35 * np.sin(u * np.pi * 3) * np.cos(v * np.pi * 2)
                + 0.15 * np.sin(u * np.pi * 5) * np.sin(v * np.pi * 4)
                + 0.1 * np.cos(u * np.pi * 7 + v * np.pi * 3))

# ── Policy parameters (from AMR Appendix Table) ──────────────────────
POLICIES = {
    "P1": {"name": "Res-Suppressing", "color": C["coral"],
           "d_scope": +0.60, "d_synergy": -0.30, "d_herd": +0.20,
           "desc": "Constrains scope"},
    "P2": {"name": "Res-Enriching", "color": C["amber"],
           "d_scope": -0.50, "d_synergy": 0.00, "d_herd": +0.40,
           "desc": "Broad + herding"},
    "P3": {"name": "Res-Retaining", "color": C["teal"],
           "d_scope": 0.00, "d_synergy": +0.60, "d_herd": -0.30,
           "desc": "Deep niche"},
    "P4": {"name": "Res-Freeing", "color": C["plum"],
           "d_scope": -0.60, "d_synergy": +0.50, "d_herd": 0.00,
           "desc": "Breadth + depth"},
}

EROSION_LEVELS = {
    "Low":    {"noise": 0.02, "amplifier": 1.0, "color": C["teal"]},
    "Medium": {"noise": 0.06, "amplifier": 1.5, "color": C["amber"]},
    "High":   {"noise": 0.12, "amplifier": 2.5, "color": C["coral"]},
}


class BasinNarrativeScene(ThreeDScene):
    """Full narrative: firm on landscape under policy and erosion."""

    def construct(self):
        self.camera.background_color = C["bg_dark"]
        rng = np.random.default_rng(42)
        fitness = nk_fitness_values(N=6, K=2, seed=42)
        resolution = 40

        def make_surface(erosion_noise=0.0, erosion_seed=0):
            """Generate surface function with optional erosion noise.
            Pre-computes a fixed noise grid so the surface is deterministic
            (no random spikes on repeated evaluation)."""
            if erosion_noise > 0:
                erng = np.random.default_rng(erosion_seed)
                # Pre-compute noise on a grid and interpolate
                noise_res = 12   # low-frequency grid -> smooth swells, not per-vertex spikes
                noise_grid = erng.normal(size=(noise_res, noise_res)) * erosion_noise
            else:
                noise_grid = None
                noise_res = 1

            def surface_func(u, v):
                z = nk_surface_function(u, v, fitness, smoothing=0.25)
                if noise_grid is not None:
                    # Bilinear interpolation -> smooth, continuous erosion swells
                    # (nearest-neighbour produced uncorrelated per-vertex spikes).
                    fu = float(np.clip(u, 0, 1)) * (noise_res - 1)
                    fv = float(np.clip(v, 0, 1)) * (noise_res - 1)
                    i0 = int(np.clip(np.floor(fu), 0, noise_res - 2))
                    j0 = int(np.clip(np.floor(fv), 0, noise_res - 2))
                    a, b = fu - i0, fv - j0
                    z += (noise_grid[i0, j0] * (1 - a) * (1 - b)
                          + noise_grid[i0 + 1, j0] * a * (1 - b)
                          + noise_grid[i0, j0 + 1] * (1 - a) * b
                          + noise_grid[i0 + 1, j0 + 1] * a * b)
                # Clamp z to keep the eroded surface inside the camera frame
                z = np.clip(z, -0.15, 1.15)
                return np.array([(u - 0.5) * 6, (v - 0.5) * 6, (z - 0.4) * 4])
            return surface_func

        surface_func = make_surface()

        # Basin centers (hand-tuned for visual clarity)
        basins = {
            "P1": (0.25, 0.30),  # narrow, constrained
            "P2": (0.70, 0.75),  # broad, shallow
            "P3": (0.80, 0.25),  # deep, niche
            "P4": (0.50, 0.60),  # balanced
        }
        start_pos = (0.50, 0.40)  # neutral starting position

        # ══════════════════════════════════════════════════════════════
        # PHASE 1: TITLE + LANDSCAPE (~10s)
        # ══════════════════════════════════════════════════════════════
        title = Text("Same Peak, Different Basin", font_size=38,
                     color=C["text_light"], font="Times New Roman", weight=BOLD)
        subtitle = Text("How Industrial Policy Shapes the Hidden Geometry\nof Competitive Search",
                        font_size=18, color=C["amber"],
                        font="Times New Roman", slant=ITALIC, line_spacing=0.8)
        subtitle.next_to(title, DOWN, buff=0.25)
        title_group = VGroup(title, subtitle).move_to(ORIGIN)

        self.play(Write(title), run_time=1.0)
        self.play(FadeIn(subtitle), run_time=0.6)
        self.wait(1.5)
        self.play(FadeOut(title_group), run_time=0.8)

        # Build landscape
        surface = Surface(
            surface_func, u_range=[0, 1], v_range=[0, 1],
            resolution=(resolution, resolution),
            fill_opacity=0.65, stroke_width=0.3, stroke_color=C["text_dim"],
        )
        surface.set_color_by_gradient(C["navy"], C["teal"], C["amber"])
        self.set_camera_orientation(phi=65 * DEGREES, theta=-45 * DEGREES, zoom=0.7)
        self.play(Create(surface), run_time=2.0)

        # Labels
        x_label = Text("Scope (K_AC)", font_size=12, color=C["text_dim"])
        x_label.rotate(PI / 2, axis=RIGHT).move_to(np.array([0, -3.5, -1.5]))
        y_label = Text("Synergy (K_CC)", font_size=12, color=C["text_dim"])
        y_label.rotate(PI / 2, axis=RIGHT).rotate(PI / 2, axis=OUT)
        y_label.move_to(np.array([-3.5, 0, -0.5]))
        z_label = Text("Fitness", font_size=12, color=C["text_dim"])
        z_label.rotate(PI / 2, axis=RIGHT).rotate(PI / 2, axis=OUT)
        z_label.move_to(np.array([-3.5, 0, 1.0]))
        self.add_fixed_orientation_mobjects(x_label, y_label, z_label)
        self.play(FadeIn(x_label), FadeIn(y_label), FadeIn(z_label), run_time=0.5)
        self.wait(1)

        # ══════════════════════════════════════════════════════════════
        # PHASE 2: BASELINE SEARCH — ONE FIRM, NO POLICY (~10s)
        # ══════════════════════════════════════════════════════════════
        phase2_text = Text("Phase 1: Baseline Search (No Policy)",
                           font_size=20, color=C["text_light"],
                           font="Times New Roman", weight=BOLD)
        phase2_text.to_edge(DOWN, buff=0.4)
        self.add_fixed_in_frame_mobjects(phase2_text)
        self.play(FadeIn(phase2_text), run_time=0.5)

        # Place firm
        u, v = start_pos
        pos = surface_func(u, v)
        firm = Sphere(radius=0.15, color=WHITE).move_to(pos + UP * 0.2)
        firm.set_color(WHITE)
        firm_label = Text("Firm", font_size=10, color=WHITE)
        firm_label.rotate(PI / 2, axis=RIGHT)
        firm_label.move_to(pos + UP * 0.5)
        self.add_fixed_orientation_mobjects(firm_label)
        self.play(GrowFromCenter(firm), FadeIn(firm_label), run_time=0.5)

        # Baseline gradient ascent (12 steps)
        baseline_trail = []
        for step in range(12):
            du = rng.uniform(-0.04, 0.04)
            dv = rng.uniform(-0.04, 0.04)
            f_curr = nk_surface_function(u, v, fitness, 0.25)
            f_new = nk_surface_function(np.clip(u + du, 0.05, 0.95),
                                         np.clip(v + dv, 0.05, 0.95), fitness, 0.25)
            if f_new >= f_curr or rng.random() < 0.2:
                u = np.clip(u + du, 0.05, 0.95)
                v = np.clip(v + dv, 0.05, 0.95)
            new_pos = surface_func(u, v)
            old_pos = firm.get_center()
            trail = Line(old_pos, new_pos + UP * 0.2,
                         color=WHITE, stroke_width=1.5, stroke_opacity=0.4)
            self.add(trail)
            baseline_trail.append(trail)
            self.play(firm.animate.move_to(new_pos + UP * 0.2),
                      firm_label.animate.move_to(new_pos + UP * 0.5),
                      run_time=0.25)

        self.begin_ambient_camera_rotation(rate=0.1)
        self.wait(2)
        self.stop_ambient_camera_rotation()

        # Clear baseline
        self.play(FadeOut(firm), FadeOut(firm_label), FadeOut(phase2_text),
                  *[FadeOut(t) for t in baseline_trail], run_time=0.8)

        # ══════════════════════════════════════════════════════════════
        # PHASE 3: FOUR POLICIES — SAME START, DIFFERENT BASINS (~20s)
        # ══════════════════════════════════════════════════════════════
        phase3_text = Text("Phase 2: Four Policies -- Four Basins",
                           font_size=20, color=C["highlight"],
                           font="Times New Roman", weight=BOLD)
        phase3_text.to_edge(DOWN, buff=0.4)
        self.add_fixed_in_frame_mobjects(phase3_text)
        self.play(FadeIn(phase3_text), run_time=0.5)

        # Place 4 firms at same start — track all trail objects for cleanup
        policy_firms = {}
        policy_labels_mob = {}
        all_trails = []
        for pk, pdata in POLICIES.items():
            u0, v0 = start_pos
            pos0 = surface_func(u0, v0)
            dot = Sphere(radius=0.12, color=pdata["color"])
            dot.set_color(pdata["color"])
            dot.move_to(pos0 + UP * 0.2)
            lbl = Text(pk, font_size=9, color=pdata["color"])
            lbl.rotate(PI / 2, axis=RIGHT)
            lbl.move_to(pos0 + UP * 0.5)
            self.add_fixed_orientation_mobjects(lbl)
            policy_firms[pk] = {"dot": dot, "label": lbl, "u": u0, "v": v0}
            policy_labels_mob[pk] = lbl

        self.play(*[GrowFromCenter(pf["dot"]) for pf in policy_firms.values()],
                  *[FadeIn(lbl) for lbl in policy_labels_mob.values()],
                  run_time=0.8)

        # Legend
        legend_items = VGroup()
        for pk, pdata in POLICIES.items():
            dot_legend = Dot(color=pdata["color"], radius=0.06)
            txt_legend = Text(f"{pk}: {pdata['desc']}", font_size=11,
                              color=pdata["color"])
            row = VGroup(dot_legend, txt_legend).arrange(RIGHT, buff=0.1)
            legend_items.add(row)
        legend_items.arrange(DOWN, buff=0.08, aligned_edge=LEFT)
        legend_items.to_corner(UL, buff=0.3)
        self.add_fixed_in_frame_mobjects(legend_items)
        self.play(FadeIn(legend_items), run_time=0.5)

        # Animate divergent trajectories (20 steps)
        for step in range(20):
            anims = []
            pull = 0.025 * (1 + step / 10)
            for pk, pf in policy_firms.items():
                u_curr, v_curr = pf["u"], pf["v"]
                target_u, target_v = basins[pk]

                p = POLICIES[pk]
                du = rng.uniform(-0.03, 0.03) - p["d_scope"] * 0.01 + pull * (target_u - u_curr)
                dv = rng.uniform(-0.03, 0.03) + p["d_synergy"] * 0.01 + pull * (target_v - v_curr)

                u_new = np.clip(u_curr + du, 0.05, 0.95)
                v_new = np.clip(v_curr + dv, 0.05, 0.95)
                new_pos = surface_func(u_new, v_new)
                pf["u"], pf["v"] = u_new, v_new

                old_pos = pf["dot"].get_center()
                trail = Line(old_pos, new_pos + UP * 0.2,
                             color=p["color"], stroke_width=1.5, stroke_opacity=0.5)
                self.add(trail)
                all_trails.append(trail)

                anims.append(pf["dot"].animate.move_to(new_pos + UP * 0.2))
                anims.append(pf["label"].animate.move_to(new_pos + UP * 0.5))

            self.play(*anims, run_time=0.3)

        # Camera orbit to show divergence
        self.begin_ambient_camera_rotation(rate=0.15)
        self.wait(3)
        self.stop_ambient_camera_rotation()

        # ── CLEAN TRANSITION: fade out Phase 3 elements before erosion ──
        self.play(
            FadeOut(phase3_text),
            *[FadeOut(pf["dot"]) for pf in policy_firms.values()],
            *[FadeOut(pf["label"]) for pf in policy_firms.values()],
            *[FadeOut(t) for t in all_trails],
            FadeOut(legend_items),
            run_time=1.0
        )

        # ══════════════════════════════════════════════════════════════
        # PHASE 4: EROSION RESHAPES THE LANDSCAPE (~15s)
        # ══════════════════════════════════════════════════════════════
        phase4_text = Text("Phase 3: Institutional Erosion Reshapes the Landscape",
                           font_size=20, color=C.get("terra", C["coral"]),
                           font="Times New Roman", weight=BOLD)
        phase4_text.to_edge(DOWN, buff=0.4)
        self.add_fixed_in_frame_mobjects(phase4_text)
        self.play(FadeIn(phase4_text), run_time=0.5)

        # Erosion meter
        erosion_label = Text("Erosion: Low", font_size=16, color=C["teal"])
        erosion_label.to_corner(UR, buff=0.4)
        self.add_fixed_in_frame_mobjects(erosion_label)
        self.play(FadeIn(erosion_label), run_time=0.3)

        # Stop the drifting ambient rotation and reset to a clean, framed
        # orientation so the eroding surface stays in bounds (no edge-on streaks).
        self.stop_ambient_camera_rotation()
        self.move_camera(phi=68 * DEGREES, theta=-50 * DEGREES, zoom=0.62, run_time=1.2)

        # Morph surface through erosion levels — use ReplacementTransform
        current_surface = surface
        for level_name, level_data in [("Medium", EROSION_LEVELS["Medium"]),
                                        ("High", EROSION_LEVELS["High"])]:
            noisy_func = make_surface(erosion_noise=level_data["noise"],
                                      erosion_seed=hash(level_name) % 1000)
            new_surface = Surface(
                noisy_func, u_range=[0, 1], v_range=[0, 1],
                resolution=(resolution, resolution),
                fill_opacity=0.65, stroke_width=0.3, stroke_color=C["text_dim"],
            )
            new_surface.set_color_by_gradient(C["navy"], level_data["color"], C["coral"])

            new_label = Text(f"Erosion: {level_name}", font_size=16,
                             color=level_data["color"]).to_corner(UR, buff=0.4)
            new_label.set_opacity(0)
            self.add_fixed_in_frame_mobjects(new_label)

            # Crossfade the label by opacity (crisp glyphs, single label at the end)
            # while the surface morphs — avoids both stacking and Transform glyph-soup.
            self.play(
                ReplacementTransform(current_surface, new_surface),
                erosion_label.animate.set_opacity(0),
                new_label.animate.set_opacity(1),
                run_time=2.0
            )
            self.remove(erosion_label)
            erosion_label = new_label
            current_surface = new_surface
            self.wait(1.5)

        self.play(FadeOut(phase4_text), run_time=0.5)

        # ══════════════════════════════════════════════════════════════
        # PHASE 5: POLICY x EROSION MATRIX (~20s)
        # ══════════════════════════════════════════════════════════════

        # Clean transition to 2D
        self.play(
            FadeOut(current_surface), FadeOut(erosion_label),
            FadeOut(x_label), FadeOut(y_label), FadeOut(z_label),
            run_time=1.0
        )

        phase5_text = Text("Phase 4: Which Policy Wins Under Which Erosion?",
                           font_size=22, color=C["highlight"],
                           font="Times New Roman", weight=BOLD)
        phase5_text.to_edge(UP, buff=0.4)
        self.add_fixed_in_frame_mobjects(phase5_text)
        self.play(FadeIn(phase5_text), run_time=0.5)

        # Create a 4x3 fitness matrix (Policy x Erosion)
        # Fitness values from the paper's tournament results
        fitness_matrix = {
            ("P1", "Low"): 0.62, ("P1", "Medium"): 0.55, ("P1", "High"): 0.42,
            ("P2", "Low"): 0.68, ("P2", "Medium"): 0.60, ("P2", "High"): 0.48,
            ("P3", "Low"): 0.65, ("P3", "Medium"): 0.72, ("P3", "High"): 0.70,
            ("P4", "Low"): 0.75, ("P4", "Medium"): 0.67, ("P4", "High"): 0.55,
        }

        # Build visual grid
        cell_w, cell_h = 1.2, 0.6
        grid_start = np.array([-2.5, 1.0, 0])

        # Headers
        erosion_names = ["Low", "Medium", "High"]
        policy_names = ["P1", "P2", "P3", "P4"]

        for j, ename in enumerate(erosion_names):
            h = Text(ename, font_size=14, color=EROSION_LEVELS[ename]["color"])
            h.move_to(grid_start + RIGHT * (j + 1) * cell_w + UP * 0.4)
            self.add_fixed_in_frame_mobjects(h)
            self.play(FadeIn(h), run_time=0.1)

        for i, pname in enumerate(policy_names):
            h = Text(pname, font_size=14, color=POLICIES[pname]["color"],
                     weight=BOLD)
            h.move_to(grid_start + DOWN * i * cell_h + LEFT * 0.3)
            self.add_fixed_in_frame_mobjects(h)
            self.play(FadeIn(h), run_time=0.1)

        # Find best per column for highlighting
        best_per_erosion = {}
        for ename in erosion_names:
            best_p = max(policy_names, key=lambda p: fitness_matrix[(p, ename)])
            best_per_erosion[ename] = best_p

        # Fill cells with bars
        for i, pname in enumerate(policy_names):
            for j, ename in enumerate(erosion_names):
                val = fitness_matrix[(pname, ename)]
                bar_width = val * cell_w * 0.9
                bar = Rectangle(
                    width=bar_width, height=cell_h * 0.6,
                    fill_color=POLICIES[pname]["color"],
                    fill_opacity=0.7 if pname == best_per_erosion[ename] else 0.3,
                    stroke_width=2 if pname == best_per_erosion[ename] else 0.5,
                    stroke_color=WHITE if pname == best_per_erosion[ename] else GREY,
                )
                bar.move_to(grid_start + RIGHT * (j + 1) * cell_w + DOWN * i * cell_h)
                val_text = Text(f"{val:.2f}", font_size=10, color=C["text_light"])
                val_text.move_to(bar.get_center())
                self.add_fixed_in_frame_mobjects(bar, val_text)
                self.play(GrowFromEdge(bar, LEFT), FadeIn(val_text), run_time=0.08)

        # Highlight winners
        self.wait(1)
        winner_text = Text("Winner shifts: P4 (Low) -- P3 (Medium) -- P3 (High)",
                           font_size=16, color=C["highlight"],
                           font="Times New Roman", slant=ITALIC)
        winner_text.to_edge(DOWN, buff=0.5)
        self.add_fixed_in_frame_mobjects(winner_text)
        self.play(FadeIn(winner_text), run_time=0.5)
        self.wait(3)

        # ══════════════════════════════════════════════════════════════
        # PHASE 6: THE INSIGHT (~10s)
        # ══════════════════════════════════════════════════════════════
        # Fade grid
        self.play(*[FadeOut(m) for m in self.mobjects],
                  run_time=1.0)

        insight1 = Text("No universal best policy.", font_size=28,
                        color=C["text_light"], font="Times New Roman", weight=BOLD)
        insight2 = Text("Architecture determines which basin firms converge to.",
                        font_size=20, color=C["amber"],
                        font="Times New Roman", slant=ITALIC)
        insight3 = Text("Erosion shifts the basin geometry,\nchanging which policy survives.",
                        font_size=18, color=C.get("terra", C["coral"]),
                        font="Times New Roman", slant=ITALIC, line_spacing=0.8)
        insights = VGroup(insight1, insight2, insight3).arrange(DOWN, buff=0.3)
        insights.move_to(ORIGIN)
        self.add_fixed_in_frame_mobjects(insights)

        self.play(Write(insight1), run_time=0.8)
        self.play(FadeIn(insight2), run_time=0.5)
        self.play(FadeIn(insight3), run_time=0.5)
        self.wait(3)
        self.play(FadeOut(insights), run_time=1.0)
