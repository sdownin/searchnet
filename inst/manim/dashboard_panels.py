"""
Dashboard Panel Scenes for SearchNet (SaoMNK)
===============================================
Reusable manim scene classes for composing multi-panel dashboard animations.
Each scene loads CSV data lazily and can be rendered individually or arranged
in a grid via DashboardComposer.

Scenes:
    1. KDegreeEvolutionScene   -- 4-panel K_AC/K_CC/K_CA/K_AA trajectories
    2. BipartiteNetworkScene   -- Animated firm x route bipartite network
    3. FitnessLandscapeScene   -- 3D NK surface with climbing agents
    4. InformationFunnelScene  -- Cross-layer observation waterfall
    5. ShockResponseScene      -- Pre/post shock split-screen
    6. GameTheoreticScene      -- Congestion game payoff + QRE annealing

Utility:
    DashboardComposer          -- Grid layout with synced timeline

Render single scene:
    manim -qh dashboard_panels.py KDegreeEvolutionScene

Requires: manim CE, numpy, csv (stdlib)
"""

from __future__ import annotations

import csv
import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
from manim import *

# -- Import searchnet_viz with graceful fallback --------------------------
try:
    from searchnet_viz import (
        BaseSearchnetScene as BaseResearchScene,
        SEARCHNET_COLORS as SAOMNK_COLORS,
        nk_fitness_values,
        nk_surface_function,
    )
    # Carrier colors not in searchnet_viz; define locally
    CARRIER_COLORS = {
        "AA": "#C0392B", "DL": "#2980B9", "UA": "#1B2A4A", "WN": "#E76F51",
        "AS": "#2A9D8F", "B6": "#7B2D8E", "NK": "#E9C46A", "G4": "#6B9080",
    }
except ImportError:
    # Inline fallback so the module works without searchnet_viz installed
    SAOMNK_COLORS = {
        "navy": "#1B2A4A", "steel": "#4A6FA5", "teal": "#2A9D8F",
        "amber": "#E9C46A", "terra": "#E76F51", "sage": "#6B9080",
        "plum": "#7B2D8E", "slate": "#5C6B73",
        "bg_dark": "#0F1729", "bg_panel": "#1A2340",
        "text_light": "#E8E8E8", "text_dim": "#8899AA",
        "positive": "#2A9D8F", "negative": "#E76F51",
        "neutral": "#4A6FA5", "highlight": "#E9C46A", "baseline": "#1B2A4A",
    }
    CARRIER_COLORS = {
        "AA": "#C0392B", "DL": "#2980B9", "UA": "#1B2A4A", "WN": "#E76F51",
        "AS": "#2A9D8F", "B6": "#7B2D8E", "NK": "#E9C46A", "G4": "#6B9080",
    }

    class BaseResearchScene(Scene):
        """Minimal fallback when searchnet_viz is not installed."""
        scene_title = ""
        scene_subtitle = ""
        source_label = "SAOM-NK Simulation"

        def setup(self):
            self.camera.background_color = SAOMNK_COLORS["bg_dark"]

        def add_title(self, title=None, subtitle=None, animate=True):
            t = title or self.scene_title
            s = subtitle or self.scene_subtitle
            title_text = Text(t, font_size=34, color=SAOMNK_COLORS["text_light"],
                              font="Times New Roman", weight=BOLD)
            title_text.to_edge(UP, buff=0.35)
            if animate:
                self.play(Write(title_text), run_time=0.8)
            else:
                self.add(title_text)
            sub_text = None
            if s:
                sub_text = Text(s, font_size=18, color=SAOMNK_COLORS["text_dim"],
                                font="Times New Roman", slant=ITALIC)
                sub_text.next_to(title_text, DOWN, buff=0.12)
                if animate:
                    self.play(FadeIn(sub_text), run_time=0.4)
                else:
                    self.add(sub_text)
            return title_text, sub_text

        def add_source(self, text=None):
            label = Text(text or self.source_label, font_size=11,
                         color=SAOMNK_COLORS["text_dim"])
            label.to_edge(DOWN, buff=0.15).to_edge(RIGHT, buff=0.3)
            self.add(label)
            return label

        @staticmethod
        def load_csv(filepath):
            with open(filepath, "r", encoding="utf-8") as f:
                return list(csv.DictReader(f))

        def make_node(self, label, color=WHITE, radius=0.15, font_size=10):
            circle = Circle(radius=radius, color=color, fill_opacity=0.8)
            text = Text(label[:12], font_size=font_size, color=SAOMNK_COLORS["bg_dark"])
            return VGroup(circle, text)

        def make_edge(self, start, end, color=None, width=1.5):
            return Line(start, end, color=color or SAOMNK_COLORS["text_dim"],
                        stroke_width=width, stroke_opacity=0.6)

        def fade_all_out(self, run_time=0.5):
            if self.mobjects:
                self.play(*[FadeOut(m) for m in self.mobjects], run_time=run_time)

    # Minimal NK functions for the 3D landscape fallback
    import itertools

    def nk_fitness_values(N=8, K=3, seed=42):
        rng = np.random.default_rng(seed)
        interactions = {}
        for i in range(N):
            others = list(range(N))
            others.remove(i)
            interactions[i] = sorted(rng.choice(others, size=K, replace=False).tolist())
        contribution_tables = {}
        for i in range(N):
            contribution_tables[i] = rng.uniform(0, 1, size=2 ** (K + 1))
        fitness = {}
        for config in itertools.product([0, 1], repeat=N):
            total = 0.0
            for i in range(N):
                bits = [config[i]] + [config[j] for j in interactions[i]]
                idx = int("".join(str(b) for b in bits), 2)
                total += contribution_tables[i][idx]
            fitness[config] = total / N
        return fitness

    def nk_surface_function(x, y, fitness_cache, smoothing=0.3):
        if not fitness_cache:
            return 0.5
        total_weight = 0.0
        total_fitness = 0.0
        for config, fit in fitness_cache.items():
            d0 = abs(x - config[0])
            d1 = abs(y - config[1])
            dist = np.sqrt(d0**2 + d1**2 + 1e-10)
            weight = np.exp(-dist / max(smoothing, 0.01))
            total_weight += weight
            total_fitness += weight * fit
        return total_fitness / total_weight if total_weight > 0 else 0.5


# -- Shared constants ----------------------------------------------------
C = SAOMNK_COLORS

# Default data directory: inst/manim/data/ within the searchnet package
DEFAULT_DATA_DIR = Path(__file__).resolve().parent / "data"

# Major industry shocks with simulation-round and calendar-year positions
SHOCKS = [
    {"label": "9/11",     "year": 2001, "color": C["terra"]},
    {"label": "Mergers",  "year": 2010, "color": C["plum"]},
    {"label": "COVID-19", "year": 2020, "color": C["terra"]},
]

K_METRICS = ["K_AC", "K_CC", "K_CA", "K_AA"]
K_METRIC_COLORS = {
    "K_AC": C["steel"],
    "K_CC": C["teal"],
    "K_CA": C["amber"],
    "K_AA": C["terra"],
}


# =========================================================================
# Helper utilities
# =========================================================================

def _load_csv(filepath: str | Path) -> List[Dict[str, str]]:
    """Load a CSV file as a list of dicts."""
    with open(filepath, "r", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def _safe_float(val: str, default: float = 0.0) -> float:
    try:
        return float(val)
    except (ValueError, TypeError):
        return default


def _make_timeline_bar(axes, year_range, shocks, y_pos=-3.5):
    """Create a scrub-bar with shock markers below the axes.

    Returns (bar_group, shock_markers).
    """
    bar_width = axes.x_length if hasattr(axes, "x_length") else 10
    bar = Rectangle(width=bar_width, height=0.08,
                    fill_color=C["text_dim"], fill_opacity=0.3,
                    stroke_width=0)
    bar.move_to(np.array([axes.get_center()[0], y_pos, 0]))

    markers = VGroup()
    y_min, y_max = year_range
    for shock in shocks:
        if y_min <= shock["year"] <= y_max:
            frac = (shock["year"] - y_min) / max(y_max - y_min, 1)
            x = bar.get_left()[0] + frac * bar_width
            dot = Dot(point=np.array([x, y_pos, 0]), radius=0.06,
                      color=shock["color"])
            lbl = Text(shock["label"], font_size=9, color=shock["color"])
            lbl.next_to(dot, DOWN, buff=0.08)
            markers.add(VGroup(dot, lbl))

    return VGroup(bar, markers)


def _add_shock_lines(axes, shocks, year_range, y_data_range):
    """Return a VGroup of vertical dashed shock marker lines with labels."""
    group = VGroup()
    y_min_yr, y_max_yr = year_range
    y_lo, y_hi = y_data_range
    for shock in shocks:
        if y_min_yr <= shock["year"] <= y_max_yr:
            x_val = shock["year"]
            line = DashedLine(
                axes.c2p(x_val, y_lo), axes.c2p(x_val, y_hi),
                color=shock["color"], stroke_width=1.2, dash_length=0.08,
            )
            lbl = Text(shock["label"], font_size=10, color=shock["color"],
                       slant=ITALIC)
            lbl.next_to(line, UP, buff=0.08)
            group.add(VGroup(line, lbl))
    return group


# =========================================================================
# Scene 1 -- K-Degree Evolution (4-panel)
# =========================================================================

class KDegreeEvolutionScene(BaseResearchScene):
    """Animated 4-panel display of K_AC, K_CC, K_CA, K_AA over time.

    Each sub-panel plots one K metric with carrier-level trajectories,
    vertical shock markers, and a shared timeline scrub bar.

    Usage:
        # Render directly
        manim -qh dashboard_panels.py KDegreeEvolutionScene

        # Programmatic with custom data
        class MyK(KDegreeEvolutionScene):
            carrier_panel_csv = "path/to/carrier_panel.csv"
            carrier_pairs_csv = "path/to/carrier_pairs.csv"
    """

    scene_title = "K-System Evolution"
    scene_subtitle = "Four dimensions of competitive interdependence"
    source_label = "DB1B + T-100 | 1993-2024"

    carrier_panel_csv: str = str(DEFAULT_DATA_DIR / "k4_trajectories.csv")
    carrier_pairs_csv: str = str(DEFAULT_DATA_DIR / "k4_trajectories.csv")
    year_range: Tuple[int, int] = (1993, 2024)

    def construct(self):
        title, sub = self.add_title()
        self.add_source()
        self.wait(0.3)

        # -- Load data ------------------------------------------------
        panel_data = _load_csv(self.carrier_panel_csv)

        # Organize: {carrier: {year: {metric: value}}}
        carrier_years: Dict[str, Dict[int, Dict[str, float]]] = {}
        for row in panel_data:
            carrier = row.get("carrier", row.get("carrier_code", ""))
            year = int(_safe_float(row.get("year", row.get("Year",
                       row.get("round", "0")))))
            if not carrier or year == 0:
                continue
            carrier_years.setdefault(carrier, {})[year] = {
                m: _safe_float(row.get(m, row.get(m.lower(), "0")))
                for m in K_METRICS
            }

        carriers = sorted(carrier_years.keys())
        years = sorted({y for c in carrier_years.values() for y in c})
        if not years:
            self._placeholder("No year data found in k4_trajectories.csv")
            return

        yr_lo, yr_hi = min(years), max(years)

        # -- Build 4 sub-panels in 2x2 grid ---------------------------
        panel_positions = [
            UP * 0.6 + LEFT * 3.2,   # top-left  = K_AC
            UP * 0.6 + RIGHT * 3.2,  # top-right = K_CC
            DOWN * 2.0 + LEFT * 3.2, # bot-left  = K_CA
            DOWN * 2.0 + RIGHT * 3.2,# bot-right = K_AA
        ]

        for idx, metric in enumerate(K_METRICS):
            pos = panel_positions[idx]
            self._draw_k_panel(metric, carrier_years, carriers, years,
                               yr_lo, yr_hi, pos)

        # -- Timeline scrub bar ----------------------------------------
        ref_axes = Axes(x_range=[yr_lo, yr_hi, 5], y_range=[0, 1],
                        x_length=10, y_length=0.01).shift(DOWN * 3.5)
        bar = _make_timeline_bar(ref_axes, (yr_lo, yr_hi), SHOCKS, y_pos=-3.5)
        self.play(FadeIn(bar), run_time=0.5)

        self.wait(3)

    # -- Internal helpers ----------------------------------------------

    def _draw_k_panel(self, metric, carrier_years, carriers, years,
                      yr_lo, yr_hi, center):
        """Draw a single K-metric sub-panel at *center*."""
        all_vals = []
        for c in carriers:
            for y in years:
                v = carrier_years.get(c, {}).get(y, {}).get(metric, None)
                if v is not None:
                    all_vals.append(v)

        if not all_vals:
            return

        v_lo = max(0, min(all_vals) * 0.9)
        v_hi = max(all_vals) * 1.1

        axes = Axes(
            x_range=[yr_lo, yr_hi, max(1, (yr_hi - yr_lo) // 4)],
            y_range=[v_lo, v_hi, (v_hi - v_lo) / 3],
            x_length=5.0, y_length=2.2,
            axis_config={"color": C["text_dim"], "stroke_width": 1.0,
                         "include_tip": False},
            x_axis_config={"decimal_number_config": {"num_decimal_places": 0}},
        ).move_to(center)

        panel_label = Text(metric, font_size=16,
                           color=K_METRIC_COLORS.get(metric, C["steel"]),
                           font="Times New Roman", weight=BOLD)
        panel_label.next_to(axes, UP, buff=0.08)

        self.play(Create(axes), FadeIn(panel_label), run_time=0.5)

        # Shock markers
        shock_markers = _add_shock_lines(axes, SHOCKS,
                                         (yr_lo, yr_hi), (v_lo, v_hi))
        if shock_markers:
            self.play(FadeIn(shock_markers), run_time=0.3)

        # Carrier trajectories
        for carrier in carriers:
            cdata = carrier_years.get(carrier, {})
            pts = [(y, cdata[y][metric]) for y in sorted(cdata) if metric in cdata[y]]
            if len(pts) < 2:
                continue

            color = CARRIER_COLORS.get(carrier, C["steel"])
            line_pts = [axes.c2p(y, v) for y, v in pts]
            line = VMobject(color=color, stroke_width=1.8, stroke_opacity=0.7)
            line.set_points_smoothly(line_pts)
            self.play(Create(line), run_time=0.3)

    def _placeholder(self, msg):
        t = Text(msg, font_size=18, color=C["text_dim"])
        self.play(FadeIn(t), run_time=0.5)
        self.wait(1)


# =========================================================================
# Scene 2 -- Bipartite Network (Firm x Route)
# =========================================================================

class BipartiteNetworkScene(BaseResearchScene):
    """Animated bipartite firm-route network that evolves across years.

    Firms appear as colored circles (top row), routes as smaller dots
    (bottom row). Edges appear/disappear as carriers enter/exit routes.

    Usage:
        manim -qh dashboard_panels.py BipartiteNetworkScene
    """

    scene_title = "Firm-Route Bipartite Network"
    scene_subtitle = "Competitive overlap through shared route presence"
    source_label = "DB1B Route Data | 1993-2024"

    bipartite_csv: str = str(DEFAULT_DATA_DIR / "bipartite_snapshots.csv")
    top_n_routes: int = 30
    year_step: int = 3

    def construct(self):
        title, sub = self.add_title()
        self.add_source()
        self.wait(0.3)

        # -- Load route-level data -------------------------------------
        try:
            route_data = _load_csv(self.bipartite_csv)
        except FileNotFoundError:
            self._show_fallback()
            return

        # Build year snapshots: {year: {carrier: set(routes)}}
        snapshots: Dict[int, Dict[str, set]] = {}
        route_counts: Dict[str, int] = {}
        for row in route_data:
            year = int(_safe_float(row.get("round", row.get("year",
                       row.get("Year", "0")))))
            carrier = row.get("carrier", row.get("carrier_code", ""))
            route = row.get("route", row.get("market",
                    row.get("route_id", "")))
            if not (year and carrier and route):
                continue
            snapshots.setdefault(year, {}).setdefault(carrier, set()).add(route)
            route_counts[route] = route_counts.get(route, 0) + 1

        if not snapshots:
            self._show_fallback()
            return

        # Select top routes by frequency
        top_routes = sorted(route_counts, key=route_counts.get, reverse=True
                            )[:self.top_n_routes]
        years = sorted(snapshots.keys())

        # -- Year counter ----------------------------------------------
        year_counter = Text(str(years[0]), font_size=28,
                            color=C["highlight"], font="Times New Roman",
                            weight=BOLD)
        year_counter.to_corner(UR, buff=0.5)
        self.play(FadeIn(year_counter), run_time=0.3)

        prev_elements = VGroup()

        for year in years[::self.year_step]:
            snap = snapshots.get(year, {})
            carriers_this_year = sorted(snap.keys())

            # -- Layout: carriers on top, routes on bottom -------------
            n_carriers = len(carriers_this_year)
            n_routes = len(top_routes)

            carrier_nodes = VGroup()
            route_nodes = VGroup()
            edges = VGroup()

            # Carrier positions (top row)
            for i, carrier in enumerate(carriers_this_year):
                x = (i - (n_carriers - 1) / 2) * 1.2
                color = CARRIER_COLORS.get(carrier, C["steel"])
                circle = Circle(radius=0.2, color=color, fill_opacity=0.85,
                                stroke_color=WHITE, stroke_width=1)
                lbl = Text(carrier, font_size=10, color=WHITE, weight=BOLD)
                node = VGroup(circle, lbl).move_to(np.array([x, 1.5, 0]))
                carrier_nodes.add(node)

            # Route positions (bottom row)
            route_positions = {}
            for j, route in enumerate(top_routes):
                x = (j - (n_routes - 1) / 2) * 0.4
                dot = Dot(point=np.array([x, -2.0, 0]), radius=0.04,
                          color=C["text_dim"])
                route_nodes.add(dot)
                route_positions[route] = np.array([x, -2.0, 0])

            # Edges: carrier -> routes it serves
            for i, carrier in enumerate(carriers_this_year):
                carrier_routes = snap.get(carrier, set())
                carrier_pos = np.array([
                    (i - (n_carriers - 1) / 2) * 1.2, 1.5, 0])
                color = CARRIER_COLORS.get(carrier, C["text_dim"])
                for route in carrier_routes:
                    if route in route_positions:
                        edge = Line(carrier_pos, route_positions[route],
                                    color=color, stroke_width=0.6,
                                    stroke_opacity=0.3)
                        edges.add(edge)

            # -- Animate transition ------------------------------------
            new_year = Text(str(year), font_size=28, color=C["highlight"],
                            font="Times New Roman", weight=BOLD)
            new_year.to_corner(UR, buff=0.5)

            new_elements = VGroup(carrier_nodes, route_nodes, edges)

            if prev_elements.submobjects:
                self.play(
                    FadeOut(prev_elements),
                    FadeIn(new_elements),
                    Transform(year_counter, new_year),
                    run_time=0.8,
                )
            else:
                self.play(
                    FadeIn(new_elements),
                    Transform(year_counter, new_year),
                    run_time=0.8,
                )

            prev_elements = new_elements
            self.wait(0.5)

        self.wait(2)

    def _show_fallback(self):
        """Show synthetic demo when real data is unavailable."""
        msg = Text("Route data not found -- showing synthetic demo",
                   font_size=14, color=C["text_dim"])
        self.play(FadeIn(msg), run_time=0.5)

        # Synthetic demo
        carriers = list(CARRIER_COLORS.keys())[:6]
        rng = np.random.default_rng(42)

        carrier_nodes = VGroup()
        for i, c in enumerate(carriers):
            x = (i - 2.5) * 1.5
            circle = Circle(radius=0.22, color=CARRIER_COLORS[c],
                            fill_opacity=0.85, stroke_color=WHITE,
                            stroke_width=1)
            lbl = Text(c, font_size=11, color=WHITE, weight=BOLD)
            carrier_nodes.add(VGroup(circle, lbl).move_to(
                np.array([x, 1.5, 0])))

        route_dots = VGroup()
        n_routes = 20
        for j in range(n_routes):
            x = (j - (n_routes - 1) / 2) * 0.5
            route_dots.add(Dot(np.array([x, -2.0, 0]), radius=0.04,
                               color=C["text_dim"]))

        edges = VGroup()
        for i, c in enumerate(carriers):
            cx = (i - 2.5) * 1.5
            n_served = rng.integers(5, 15)
            served = rng.choice(n_routes, size=n_served, replace=False)
            for j in served:
                rx = (j - (n_routes - 1) / 2) * 0.5
                edges.add(Line(
                    np.array([cx, 1.5, 0]), np.array([rx, -2.0, 0]),
                    color=CARRIER_COLORS[c], stroke_width=0.6,
                    stroke_opacity=0.25))

        self.play(FadeIn(edges), run_time=0.8)
        self.play(FadeIn(carrier_nodes), FadeIn(route_dots), run_time=0.6)
        self.wait(3)


# =========================================================================
# Scene 3 -- Fitness Landscape (3D)
# =========================================================================

class FitnessLandscapeScene(ThreeDScene):
    """3D rugged NK fitness landscape with multiple climbing agents.

    Agents (firms) are visible simultaneously, showing convergence or
    divergence as they ascend via noisy gradient search.  Camera rotates
    for dramatic reveal.

    Usage:
        manim -qh dashboard_panels.py FitnessLandscapeScene
    """

    calibration_csv: str = str(DEFAULT_DATA_DIR / "phase_space.csv")
    n_loci: int = 6
    k_epistasis: int = 2
    n_agents: int = 6
    n_climb_steps: int = 25
    seed: int = 42

    def construct(self):
        self.camera.background_color = C["bg_dark"]

        # -- Title card ------------------------------------------------
        title = Text("Rugged Fitness Landscape", font_size=34,
                     color=C["text_light"], font="Times New Roman", weight=BOLD)
        subtitle = Text("NK model  |  N={}, K={}".format(self.n_loci, self.k_epistasis),
                        font_size=18, color=C["text_dim"],
                        font="Times New Roman", slant=ITALIC)
        subtitle.next_to(title, DOWN, buff=0.12)
        tg = VGroup(title, subtitle).move_to(ORIGIN)
        self.play(Write(title), run_time=0.6)
        self.play(FadeIn(subtitle), run_time=0.3)
        self.wait(0.8)
        self.play(FadeOut(tg), run_time=0.4)

        # -- Generate landscape ----------------------------------------
        fitness = nk_fitness_values(N=self.n_loci, K=self.k_epistasis,
                                    seed=self.seed)
        resolution = 40

        def surface_func(u, v):
            z = nk_surface_function(u, v, fitness, smoothing=0.25)
            return np.array([(u - 0.5) * 6, (v - 0.5) * 6, (z - 0.4) * 4])

        surface = Surface(
            surface_func, u_range=[0, 1], v_range=[0, 1],
            resolution=(resolution, resolution),
            fill_opacity=0.7, stroke_width=0.3,
            stroke_color=C["text_dim"],
        )
        surface.set_color_by_gradient(C["navy"], C["teal"], C["amber"])

        self.set_camera_orientation(phi=65 * DEGREES, theta=-45 * DEGREES,
                                    zoom=0.7)
        self.play(Create(surface), run_time=1.5)

        # -- Axis labels -----------------------------------------------
        x_lbl = Text("Activity Space", font_size=14, color=C["text_dim"])
        x_lbl.rotate(PI / 2, axis=RIGHT).move_to(np.array([0, -3.5, -1.5]))
        z_lbl = Text("Fitness", font_size=14, color=C["text_dim"])
        z_lbl.rotate(PI / 2, axis=RIGHT).rotate(PI / 2, axis=OUT)
        z_lbl.move_to(np.array([-3.5, 0, 0.5]))
        self.add_fixed_orientation_mobjects(x_lbl, z_lbl)
        self.play(FadeIn(x_lbl), FadeIn(z_lbl), run_time=0.4)

        # -- Agents ----------------------------------------------------
        agent_colors = [C["terra"], C["teal"], C["plum"],
                        C["amber"], C["steel"], C["sage"]]
        carrier_labels = list(CARRIER_COLORS.keys())[:self.n_agents]
        rng = np.random.default_rng(self.seed + 1)

        starts = rng.uniform(0.1, 0.9, size=(self.n_agents, 2))
        best_config = max(fitness.keys(), key=lambda cfg: fitness[cfg])
        target_x = best_config[0] * 0.7 + 0.15
        target_y = best_config[1] * 0.7 + 0.15

        agents = []
        for i in range(self.n_agents):
            u, v = starts[i]
            pos = surface_func(u, v)
            dot = Sphere(radius=0.12, color=agent_colors[i % len(agent_colors)])
            dot.set_opacity(0.9)
            dot.move_to(pos + np.array([0, 0, 0.15]))
            lbl = Text(carrier_labels[i] if i < len(carrier_labels) else f"F{i}",
                       font_size=10, color=WHITE, weight=BOLD)
            lbl.move_to(pos + np.array([0, 0, 0.4]))
            self.add_fixed_orientation_mobjects(lbl)
            agents.append({"dot": dot, "label": lbl, "u": u, "v": v})

        self.play(*[GrowFromCenter(a["dot"]) for a in agents],
                  *[FadeIn(a["label"]) for a in agents], run_time=0.6)

        # -- Climb animation -------------------------------------------
        for step in range(self.n_climb_steps):
            anims = []
            for i, agent in enumerate(agents):
                u, v = agent["u"], agent["v"]
                du = rng.uniform(-0.05, 0.05)
                dv = rng.uniform(-0.05, 0.05)
                f_curr = nk_surface_function(u, v, fitness, 0.25)
                f_new = nk_surface_function(
                    np.clip(u + du, 0, 1), np.clip(v + dv, 0, 1),
                    fitness, 0.25)
                pull = 0.02 * (1 + step / self.n_climb_steps)
                if f_new >= f_curr or rng.random() < 0.2:
                    u_new = np.clip(u + du + pull * (target_x - u), 0.02, 0.98)
                    v_new = np.clip(v + dv + pull * (target_y - v), 0.02, 0.98)
                else:
                    u_new = np.clip(u + pull * (target_x - u), 0.02, 0.98)
                    v_new = np.clip(v + pull * (target_y - v), 0.02, 0.98)
                new_pos = surface_func(u_new, v_new)
                agent["u"], agent["v"] = u_new, v_new

                # Trail
                old_pos = surface_func(u, v)
                trail = Line(old_pos + np.array([0, 0, 0.15]),
                             new_pos + np.array([0, 0, 0.15]),
                             color=agent_colors[i % len(agent_colors)],
                             stroke_width=1.2, stroke_opacity=0.4)
                self.add(trail)

                anims.append(agent["dot"].animate.move_to(
                    new_pos + np.array([0, 0, 0.15])))
                anims.append(agent["label"].animate.move_to(
                    new_pos + np.array([0, 0, 0.4])))

            self.play(*anims, run_time=0.12)

        # -- Camera rotation -------------------------------------------
        self.begin_ambient_camera_rotation(rate=0.15)
        self.wait(2)
        self.stop_ambient_camera_rotation()

        # -- Convergence annotation ------------------------------------
        note = Text("Firms converge to similar architectures",
                    font_size=18, color=C["highlight"],
                    font="Times New Roman", slant=ITALIC)
        note.to_edge(DOWN, buff=0.5)
        self.add_fixed_in_frame_mobjects(note)
        self.play(FadeIn(note), run_time=0.4)
        self.wait(2)


# =========================================================================
# Scene 4 -- Information Funnel (8 observation layers)
# =========================================================================

class InformationFunnelScene(BaseResearchScene):
    """Animated waterfall showing competitive dynamics across 8 observation layers.

    Each layer is a horizontal bar that fills as data accumulates.
    Cross-layer correlations are shown as connecting lines.
    QAP r=0.78 result is highlighted.

    Usage:
        manim -qh dashboard_panels.py InformationFunnelScene
    """

    scene_title = "Information Funnel"
    scene_subtitle = "Competitive signals across eight observation layers"
    source_label = "Cross-layer QAP validation"

    triple_layer_csv: str = str(DEFAULT_DATA_DIR / "utility_trajectories.csv")

    # The eight observation layers (from most concrete to most abstract)
    LAYERS = [
        ("Route Overlap",       0.95, C["teal"]),
        ("Schedule Similarity", 0.88, C["teal"]),
        ("Fleet Composition",   0.82, C["steel"]),
        ("Hub Structure",       0.79, C["steel"]),
        ("Fare Distribution",   0.74, C["amber"]),
        ("Financial Profile",   0.68, C["amber"]),
        ("Strategic Posture",   0.62, C["terra"]),
        ("Media Framing",       0.55, C["terra"]),
    ]

    def construct(self):
        title, sub = self.add_title()
        self.add_source()
        self.wait(0.3)

        # -- Draw the funnel layers ------------------------------------
        max_width = 9.0
        bar_height = 0.35
        gap = 0.12
        start_y = 2.0

        bars = []
        labels = []

        for i, (name, fill_frac, color) in enumerate(self.LAYERS):
            y = start_y - i * (bar_height + gap)
            width = max_width * (1.0 - i * 0.06)  # slight funnel taper

            # Background bar
            bg = Rectangle(width=width, height=bar_height,
                           fill_color=C["bg_panel"], fill_opacity=0.6,
                           stroke_color=C["text_dim"], stroke_width=0.5)
            bg.move_to(np.array([0, y, 0]))

            # Fill bar (starts at zero width, animated)
            fill = Rectangle(width=0.01, height=bar_height - 0.04,
                             fill_color=color, fill_opacity=0.7,
                             stroke_width=0)
            fill.move_to(np.array([
                bg.get_left()[0] + 0.005, y, 0
            ]))
            fill.align_to(bg, LEFT)

            # Label
            lbl = Text(name, font_size=12, color=C["text_light"])
            lbl.next_to(bg, LEFT, buff=0.15)

            # Value label
            val = Text(f"{fill_frac:.0%}", font_size=11, color=color,
                       weight=BOLD)
            val.next_to(bg, RIGHT, buff=0.15)

            bars.append((bg, fill, fill_frac, width, y, color))
            labels.append((lbl, val))

        # Animate bars appearing
        for i, ((bg, fill, frac, w, y, color), (lbl, val)) in enumerate(
                zip(bars, labels)):
            target_fill = Rectangle(
                width=w * frac, height=bar_height - 0.04,
                fill_color=color, fill_opacity=0.7, stroke_width=0)
            target_fill.move_to(np.array([
                bg.get_left()[0] + w * frac / 2, y, 0
            ]))

            self.play(
                FadeIn(bg), FadeIn(lbl),
                Transform(fill, target_fill),
                FadeIn(val),
                run_time=0.35,
            )

        # -- Cross-layer correlation lines -----------------------------
        corr_lines = VGroup()
        for i in range(len(bars) - 1):
            _, _, frac_a, w_a, y_a, col_a = bars[i]
            _, _, frac_b, w_b, y_b, col_b = bars[i + 1]
            corr = min(frac_a, frac_b)
            line = Line(
                np.array([0, y_a - bar_height / 2, 0]),
                np.array([0, y_b + bar_height / 2, 0]),
                color=C["highlight"], stroke_width=1.5,
                stroke_opacity=corr * 0.8,
            )
            corr_lines.add(line)

        self.play(FadeIn(corr_lines), run_time=0.6)

        # -- QAP result highlight --------------------------------------
        qap_box = RoundedRectangle(
            width=4.5, height=0.6, corner_radius=0.1,
            fill_color=C["bg_panel"], fill_opacity=0.9,
            stroke_color=C["highlight"], stroke_width=2,
        )
        qap_box.to_edge(DOWN, buff=0.4)
        qap_text = Text("QAP correlation  r = 0.78  (p < 0.001)",
                        font_size=16, color=C["highlight"],
                        font="Times New Roman", weight=BOLD)
        qap_text.move_to(qap_box)

        self.play(FadeIn(qap_box), Write(qap_text), run_time=0.6)
        self.wait(3)


# =========================================================================
# Scene 5 -- Shock Response (split-screen)
# =========================================================================

class ShockResponseScene(BaseResearchScene):
    """Split-screen showing pre-shock vs post-shock competitive landscape.

    Animated transition at shock point: screen shatters then reassembles.
    K_AA trajectory with divergence window highlighted, plus recovery
    speed comparison (hub-spoke vs point-to-point).

    Usage:
        manim -qh dashboard_panels.py ShockResponseScene
    """

    scene_title = "Shock Response and Recovery"
    scene_subtitle = "Architectural resilience under exogenous disruption"
    source_label = "SAOM-NK Simulation"

    shock_trajectory_csv: str = str(DEFAULT_DATA_DIR / "shock_events.csv")
    shock_round: int = 40

    def construct(self):
        title, sub = self.add_title()
        self.add_source()
        self.wait(0.3)

        # -- Load trajectory data --------------------------------------
        try:
            raw = _load_csv(self.shock_trajectory_csv)
        except FileNotFoundError:
            raw = []

        # Parse into {condition: [(round, mean, sd), ...]}
        traj: Dict[str, List[Tuple[int, float, float]]] = {}
        for row in raw:
            cond = row.get("condition", "")
            if not cond:
                continue
            traj.setdefault(cond, []).append((
                int(_safe_float(row.get("round", "0"))),
                _safe_float(row.get("mean_kaa", "0")),
                _safe_float(row.get("sd_kaa", "0")),
            ))
        for k in traj:
            traj[k].sort()

        # -- Phase 1: Pre-shock trajectory -----------------------------
        axes = Axes(
            x_range=[0, 100, 20],
            y_range=[0, 0.10, 0.02],
            x_length=10, y_length=4.0,
            axis_config={"color": C["text_dim"], "stroke_width": 1.2,
                         "include_tip": False},
        ).shift(DOWN * 0.4)

        x_lab = Text("Simulation Round", font_size=13, color=C["text_dim"])
        x_lab.next_to(axes, DOWN, buff=0.25)
        y_lab = Text("Mean K_AA", font_size=13, color=C["text_dim"])
        y_lab.next_to(axes, LEFT, buff=0.25).rotate(PI / 2)
        self.play(Create(axes), FadeIn(x_lab), FadeIn(y_lab), run_time=0.6)

        # Pre-shock and post-shock zones
        shock_x = axes.c2p(self.shock_round, 0)[0]
        pre_zone = Rectangle(
            width=shock_x - axes.c2p(0, 0)[0],
            height=axes.c2p(0, 0.10)[1] - axes.c2p(0, 0)[1],
            fill_color=C["teal"], fill_opacity=0.05, stroke_width=0,
        )
        pre_zone.move_to(np.array([
            (axes.c2p(0, 0)[0] + shock_x) / 2,
            (axes.c2p(0, 0)[1] + axes.c2p(0, 0.10)[1]) / 2, 0
        ]))

        post_zone = Rectangle(
            width=axes.c2p(100, 0)[0] - shock_x,
            height=axes.c2p(0, 0.10)[1] - axes.c2p(0, 0)[1],
            fill_color=C["terra"], fill_opacity=0.05, stroke_width=0,
        )
        post_zone.move_to(np.array([
            (shock_x + axes.c2p(100, 0)[0]) / 2,
            (axes.c2p(0, 0)[1] + axes.c2p(0, 0.10)[1]) / 2, 0
        ]))

        pre_lbl = Text("Pre-Shock", font_size=14, color=C["teal"], slant=ITALIC)
        pre_lbl.move_to(pre_zone).shift(UP * 1.5)
        post_lbl = Text("Post-Shock", font_size=14, color=C["terra"], slant=ITALIC)
        post_lbl.move_to(post_zone).shift(UP * 1.5)

        self.play(FadeIn(pre_zone), FadeIn(post_zone),
                  FadeIn(pre_lbl), FadeIn(post_lbl), run_time=0.5)

        # -- Shock line ------------------------------------------------
        shock_line = DashedLine(
            np.array([shock_x, axes.c2p(0, 0)[1], 0]),
            np.array([shock_x, axes.c2p(0, 0.10)[1], 0]),
            color=C["terra"], stroke_width=1.5, dash_length=0.1,
        )
        shock_lbl = Text("W-matrix shock", font_size=11, color=C["terra"],
                         slant=ITALIC)
        shock_lbl.next_to(shock_line, UP, buff=0.1)

        # -- Draw trajectories -----------------------------------------
        condition_styles = [
            ("Baseline (no shock)", C["steel"], "Baseline"),
            ("W-shock at round 40", C["terra"], "Shocked"),
        ]

        drawn_lines = []
        for cond, color, display_name in condition_styles:
            if cond not in traj:
                continue
            pts = traj[cond]
            line_pts = [axes.c2p(r, m) for r, m, _ in pts]
            if len(line_pts) < 2:
                continue

            pre_pts = [p for p in line_pts if p[0] <= shock_x + 0.01]
            post_pts = [p for p in line_pts if p[0] >= shock_x - 0.01]

            if len(pre_pts) >= 2:
                pre_line = VMobject(color=color, stroke_width=2.5)
                pre_line.set_points_smoothly(pre_pts)
                self.play(Create(pre_line), run_time=0.6)

            if cond == condition_styles[-1][0]:
                self.play(Create(shock_line), FadeIn(shock_lbl), run_time=0.4)

                # -- Shatter effect ------------------------------------
                flash = Rectangle(width=14, height=8,
                                  fill_color=WHITE, fill_opacity=0.3,
                                  stroke_width=0)
                self.play(FadeIn(flash, run_time=0.1))
                self.play(FadeOut(flash, run_time=0.3))

            if len(post_pts) >= 2:
                post_line = VMobject(color=color, stroke_width=2.5)
                post_line.set_points_smoothly(post_pts)
                self.play(Create(post_line), run_time=0.6)

            end_lbl = Text(display_name, font_size=12, color=color)
            if line_pts:
                end_lbl.next_to(line_pts[-1], RIGHT, buff=0.1)
                self.play(FadeIn(end_lbl), run_time=0.3)

        # -- Divergence window highlight -------------------------------
        div_rect = Rectangle(
            width=axes.c2p(60, 0)[0] - axes.c2p(40, 0)[0],
            height=0.5,
            fill_color=C["highlight"], fill_opacity=0.1,
            stroke_color=C["highlight"], stroke_width=1,
        )
        div_rect.move_to(np.array([
            (axes.c2p(40, 0)[0] + axes.c2p(60, 0)[0]) / 2,
            axes.c2p(0, 0.03)[1], 0
        ]))
        div_lbl = Text("Divergence Window", font_size=11,
                       color=C["highlight"], slant=ITALIC)
        div_lbl.next_to(div_rect, DOWN, buff=0.08)
        self.play(FadeIn(div_rect), FadeIn(div_lbl), run_time=0.4)

        # -- Recovery comparison bar chart -----------------------------
        recovery_title = Text("Recovery Speed", font_size=14,
                              color=C["text_light"], weight=BOLD)
        recovery_title.to_edge(RIGHT, buff=0.3).shift(DOWN * 1.5)
        self.play(FadeIn(recovery_title), run_time=0.3)

        bar_data = [("Hub-Spoke", 0.85, C["steel"]),
                    ("Point-to-Point", 0.55, C["teal"])]

        for j, (name, val, color) in enumerate(bar_data):
            bar = Rectangle(width=val * 1.5, height=0.25,
                            fill_color=color, fill_opacity=0.8,
                            stroke_width=0)
            bar.next_to(recovery_title, DOWN, buff=0.2 + j * 0.4)
            bar.align_to(recovery_title, LEFT)
            bar_lbl = Text(f"{name}  {val:.0%}", font_size=10,
                           color=C["text_light"])
            bar_lbl.next_to(bar, RIGHT, buff=0.1)
            self.play(GrowFromEdge(bar, LEFT), FadeIn(bar_lbl), run_time=0.4)

        self.wait(3)


# =========================================================================
# Scene 6 -- Game-Theoretic (Congestion Game + QRE)
# =========================================================================

class GameTheoreticScene(BaseResearchScene):
    """Animated congestion game with QRE probability distribution.

    Shows firms making route-entry decisions, utility changes as a payoff
    matrix, and a heat map of QRE strategy probabilities.  Temperature
    annealing visualized as a cooling color gradient converging toward
    Nash equilibrium.

    Usage:
        manim -qh dashboard_panels.py GameTheoreticScene
    """

    scene_title = "Congestion Game Dynamics"
    scene_subtitle = "Quantal response equilibrium on route networks"
    source_label = "SAOM-NK Model | Congestion Module"

    n_routes: int = 5
    n_firms: int = 4
    n_annealing_steps: int = 12
    seed: int = 42

    def construct(self):
        title, sub = self.add_title()
        self.add_source()
        self.wait(0.3)

        rng = np.random.default_rng(self.seed)

        # -- Phase 1: Payoff matrix ------------------------------------
        phase1 = Text("Payoff Structure", font_size=18,
                      color=C["highlight"], font="Times New Roman")
        phase1.shift(UP * 2.0 + LEFT * 3.5)
        self.play(FadeIn(phase1), run_time=0.3)

        firms = [f"F{i+1}" for i in range(self.n_firms)]
        routes = [f"R{j+1}" for j in range(self.n_routes)]

        # Generate payoff matrix (firm x route), higher = more attractive
        base_payoff = rng.uniform(2, 8, (self.n_firms, self.n_routes))

        cell_w, cell_h = 0.8, 0.45
        matrix_group = VGroup()

        # Column headers (routes)
        for j, route in enumerate(routes):
            hdr = Text(route, font_size=11, color=C["teal"], weight=BOLD)
            hdr.move_to(np.array([
                -3.0 + (j + 1) * cell_w, 1.2, 0
            ]))
            matrix_group.add(hdr)

        # Row headers + cells
        for i, firm in enumerate(firms):
            row_hdr = Text(firm, font_size=11, color=C["steel"], weight=BOLD)
            row_hdr.move_to(np.array([-3.0, 1.2 - (i + 1) * cell_h, 0]))
            matrix_group.add(row_hdr)

            for j in range(self.n_routes):
                val = base_payoff[i, j]
                t = (val - 2) / 6  # normalize to [0,1]
                color = interpolate_color(
                    ManimColor(C["navy"]),
                    ManimColor(C["amber"]),
                    t
                )
                cell = Rectangle(width=cell_w - 0.04, height=cell_h - 0.04,
                                 fill_color=color, fill_opacity=0.6,
                                 stroke_color=C["text_dim"], stroke_width=0.5)
                cell.move_to(np.array([
                    -3.0 + (j + 1) * cell_w,
                    1.2 - (i + 1) * cell_h, 0
                ]))
                cell_text = Text(f"{val:.1f}", font_size=9,
                                 color=C["text_light"])
                cell_text.move_to(cell)
                matrix_group.add(cell, cell_text)

        self.play(FadeIn(matrix_group), run_time=0.8)
        self.wait(0.5)

        # -- Phase 2: QRE heat map with temperature annealing ----------
        phase2 = Text("QRE Annealing", font_size=18,
                      color=C["highlight"], font="Times New Roman")
        phase2.shift(UP * 2.0 + RIGHT * 3.0)
        self.play(FadeIn(phase2), run_time=0.3)

        temp_label = Text("Temperature: ", font_size=14, color=C["text_dim"])
        temp_value = DecimalNumber(10.0, num_decimal_places=1, font_size=14,
                                   color=C["terra"])
        temp_group = VGroup(temp_label, temp_value).arrange(RIGHT, buff=0.1)
        temp_group.next_to(phase2, DOWN, buff=0.2)
        self.play(FadeIn(temp_group), run_time=0.3)

        # QRE probability: P(route j | firm i) ~ exp(payoff_ij / T)
        heatmap_cells = {}
        hm_cell_w, hm_cell_h = 0.65, 0.50
        hm_origin = np.array([3.0, -0.5, 0])

        for i in range(self.n_firms):
            for j in range(self.n_routes):
                cell = Rectangle(width=hm_cell_w - 0.03, height=hm_cell_h - 0.03,
                                 fill_color=C["navy"], fill_opacity=0.3,
                                 stroke_color=C["text_dim"], stroke_width=0.4)
                cell.move_to(hm_origin + np.array([
                    (j - self.n_routes / 2) * hm_cell_w,
                    -(i - self.n_firms / 2) * hm_cell_h, 0
                ]))
                heatmap_cells[(i, j)] = cell
                self.add(cell)

        # Anneal: decrease temperature over steps
        temperatures = np.logspace(1, -1, self.n_annealing_steps)

        for step, T in enumerate(temperatures):
            anims = []
            for i in range(self.n_firms):
                logits = base_payoff[i] / T
                logits -= logits.max()  # numerical stability
                probs = np.exp(logits) / np.exp(logits).sum()

                for j in range(self.n_routes):
                    p = probs[j]
                    color = interpolate_color(
                        ManimColor(C["navy"]),
                        ManimColor(C["terra"]),
                        float(np.clip(p * self.n_routes, 0, 1))
                    )
                    cell = heatmap_cells[(i, j)]
                    anims.append(cell.animate.set_fill(color, opacity=0.4 + p * 0.6))

            anims.append(temp_value.animate.set_value(T))
            self.play(*anims, run_time=0.25)

        # -- Nash equilibrium annotation -------------------------------
        nash = Text("Converging to Nash equilibrium",
                    font_size=14, color=C["highlight"], slant=ITALIC)
        nash.to_edge(DOWN, buff=0.5)
        self.play(FadeIn(nash), run_time=0.4)
        self.wait(3)


# =========================================================================
# Dashboard Composer (utility class)
# =========================================================================

class DashboardComposer:
    """Compose multiple scenes into a grid-layout dashboard animation.

    This is a utility class (not a Scene itself) that orchestrates
    rendering of individual panel scenes and assembles them into a
    unified multi-panel video.

    Usage:
        composer = DashboardComposer()
        composer.add_panel(KDegreeEvolutionScene, row=0, col=0)
        composer.add_panel(BipartiteNetworkScene, row=0, col=1)
        composer.add_panel(ShockResponseScene, row=1, col=0)
        composer.add_panel(GameTheoreticScene, row=1, col=1)
        composer.render("dashboard.mp4", fps=30, quality="high")

    The composer renders each panel scene individually at the target
    resolution, then uses FFmpeg to assemble them into a grid layout.
    Manim's built-in quality settings control per-panel resolution.

    Note:
        Requires FFmpeg (bundled with manim CE) and subprocess access.
        For synchronized timelines, each panel scene should use the same
        total animation duration.
    """

    QUALITY_MAP = {
        "low":      ("-ql",  480,  854),
        "medium":   ("-qm",  720, 1280),
        "high":     ("-qh", 1080, 1920),
        "4k":       ("-qk", 2160, 3840),
    }

    def __init__(self):
        self._panels: List[Dict] = []
        self._grid_rows = 0
        self._grid_cols = 0

    def add_panel(self, scene_class, row: int, col: int, **scene_kwargs):
        """Register a scene class at grid position (row, col).

        Args:
            scene_class: A manim Scene subclass from this module.
            row: Grid row (0-indexed from top).
            col: Grid column (0-indexed from left).
            **scene_kwargs: Keyword args forwarded as class attributes
                            when instantiating the scene.
        """
        self._panels.append({
            "scene_class": scene_class,
            "row": row,
            "col": col,
            "kwargs": scene_kwargs,
        })
        self._grid_rows = max(self._grid_rows, row + 1)
        self._grid_cols = max(self._grid_cols, col + 1)

    def render(self, output_path: str = "dashboard.mp4",
               fps: int = 30, quality: str = "high",
               module_path: str = __file__):
        """Render all panels and composite into a grid video.

        Args:
            output_path: Final output file path.
            fps: Frames per second for final output.
            quality: One of 'low', 'medium', 'high', '4k'.
            module_path: Path to the Python file containing scene classes.

        Raises:
            RuntimeError: If any panel render or FFmpeg composite fails.
        """
        q_flag, panel_h, panel_w = self.QUALITY_MAP.get(
            quality, self.QUALITY_MAP["high"])

        # Per-panel resolution (divide total by grid)
        pw = panel_w // self._grid_cols
        ph = panel_h // self._grid_rows

        rendered_paths = []
        media_dir = Path("media/videos/dashboard_panels")
        media_dir.mkdir(parents=True, exist_ok=True)

        for panel in self._panels:
            scene_name = panel["scene_class"].__name__

            cmd = [
                sys.executable, "-m", "manim", "render",
                q_flag,
                "--fps", str(fps),
                "-r", f"{ph},{pw}",
                module_path,
                scene_name,
            ]

            print(f"[DashboardComposer] Rendering {scene_name} "
                  f"at {pw}x{ph} ...")
            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                print(f"[ERROR] {scene_name} render failed:\n"
                      f"{result.stderr}")
                raise RuntimeError(
                    f"Failed to render {scene_name}: {result.stderr[:500]}")

            # Locate the rendered file
            q_dirname = {
                "-ql": "480p15", "-qm": "720p30",
                "-qh": "1080p60", "-qk": "2160p60",
            }.get(q_flag, "1080p60")
            rendered = (Path("media/videos/dashboard_panels") / q_dirname
                        / f"{scene_name}.mp4")
            rendered_paths.append({
                "path": str(rendered),
                "row": panel["row"],
                "col": panel["col"],
            })

        # -- FFmpeg grid composition -----------------------------------
        if len(rendered_paths) <= 1:
            if rendered_paths:
                import shutil
                shutil.copy2(rendered_paths[0]["path"], output_path)
            return

        # Build FFmpeg filter_complex for grid layout
        inputs = []
        for i, rp in enumerate(rendered_paths):
            inputs.extend(["-i", rp["path"]])

        # Create xstack layout string
        layout_parts = []
        for rp in rendered_paths:
            r, c = rp["row"], rp["col"]
            x_str = f"w0*{c}" if c > 0 else "0"
            y_str = f"h0*{r}" if r > 0 else "0"
            layout_parts.append(f"{x_str}_{y_str}")

        n_inputs = len(rendered_paths)
        layout = "|".join(layout_parts)
        filter_complex = (
            f"xstack=inputs={n_inputs}:"
            f"layout={layout}:"
            f"fill=color={C['bg_dark'].replace('#', '0x')}"
        )

        ffmpeg_cmd = [
            "ffmpeg", "-y",
            *inputs,
            "-filter_complex", filter_complex,
            "-c:v", "libx264", "-preset", "medium",
            "-r", str(fps),
            output_path,
        ]

        print(f"[DashboardComposer] Compositing {n_inputs} panels -> "
              f"{output_path}")
        result = subprocess.run(ffmpeg_cmd, capture_output=True, text=True)

        if result.returncode != 0:
            print(f"[ERROR] FFmpeg composite failed:\n{result.stderr}")
            raise RuntimeError(
                f"FFmpeg composite failed: {result.stderr[:500]}")

        print(f"[DashboardComposer] Dashboard saved to {output_path}")


# =========================================================================
# Scene registry for CLI access
# =========================================================================

SCENE_REGISTRY = {
    "k4_evolution":       KDegreeEvolutionScene,
    "bipartite_network":  BipartiteNetworkScene,
    "fitness_landscape":  FitnessLandscapeScene,
    "information_funnel": InformationFunnelScene,
    "shock_response":     ShockResponseScene,
    "game_theoretic":     GameTheoreticScene,
}
