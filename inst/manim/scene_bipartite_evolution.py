"""
Scene: Bipartite Network Evolution
====================================
Bipartite network of actors (blue circles, top row) and components
(amber squares, bottom row) evolving over simulation steps.
Side panels show actor-actor and component-component projections.

Reads adjacency matrices from CSV files exported by R.

Render:
    manim -qh scene_bipartite_evolution.py BipartiteEvolutionScene
    manim -ql scene_bipartite_evolution.py BipartiteEvolutionScene
"""

from manim import *
import numpy as np
from pathlib import Path
from searchnet_viz import BaseSearchnetScene, SEARCHNET_COLORS

C = SEARCHNET_COLORS


class BipartiteEvolutionScene(BaseSearchnetScene):
    """Bipartite network evolving over simulation steps with projection panels."""

    scene_title = "Bipartite Network Evolution"
    scene_subtitle = "Actors and components co-evolving"

    # Override these before rendering to point at real data
    data_dir = None          # directory containing step_001.csv, step_002.csv, ...
    n_actors = 6
    n_components = 8
    actor_labels = None      # list of strings or None for A1, A2, ...
    component_labels = None  # list of strings or None for C1, C2, ...

    def construct(self):
        self.add_title()
        self.add_source("SearchNet Simulation")

        # ── Load snapshot data ──────────────────────────────────────
        snapshots = self._load_snapshots()
        n_snapshots = len(snapshots)

        # Labels
        a_labels = self.actor_labels or [f"A{i+1}" for i in range(self.n_actors)]
        c_labels = self.component_labels or [f"C{j+1}" for j in range(self.n_components)]

        # ── Layout constants ────────────────────────────────────────
        # Main bipartite panel (center)
        main_x_span = 7.0
        actor_y = 2.0
        comp_y = -1.5
        main_left = -3.5

        # Side projection panels
        proj_size = 2.2

        # ── Create actor nodes (blue circles, top row) ──────────────
        actors = VGroup()
        actor_texts = VGroup()
        for i in range(self.n_actors):
            x = main_left + i * (main_x_span / max(self.n_actors - 1, 1))
            circ = Circle(
                radius=0.22,
                fill_color=C["actor"],
                fill_opacity=0.85,
                stroke_color=WHITE,
                stroke_width=1.2,
            ).move_to(np.array([x, actor_y, 0]))
            actors.add(circ)

            lbl = Text(a_labels[i], font_size=11, color=WHITE).move_to(circ)
            actor_texts.add(lbl)

        # ── Create component nodes (amber squares, bottom row) ──────
        components = VGroup()
        comp_texts = VGroup()
        for j in range(self.n_components):
            x = main_left + j * (main_x_span / max(self.n_components - 1, 1))
            sq = Square(
                side_length=0.38,
                fill_color=C["component"],
                fill_opacity=0.85,
                stroke_color=WHITE,
                stroke_width=1.2,
            ).move_to(np.array([x, comp_y, 0]))
            components.add(sq)

            lbl = Text(c_labels[j], font_size=9, color=WHITE).move_to(sq)
            comp_texts.add(lbl)

        # ── Headers ─────────────────────────────────────────────────
        actor_header = Text("Actors", font_size=18, color=C["actor"])
        actor_header.next_to(actors, UP, buff=0.25)
        comp_header = Text("Components", font_size=18, color=C["component"])
        comp_header.next_to(components, DOWN, buff=0.25)

        # ── Show nodes ──────────────────────────────────────────────
        self.play(
            *[FadeIn(a) for a in actors],
            *[FadeIn(t) for t in actor_texts],
            *[FadeIn(c) for c in components],
            *[FadeIn(t) for t in comp_texts],
            FadeIn(actor_header), FadeIn(comp_header),
            run_time=1.2,
        )

        # ── Step counter ────────────────────────────────────────────
        step_label = Text("Step: 0", font_size=22, color=C["text_light"])
        step_label.to_corner(UL, buff=0.4)
        self.play(FadeIn(step_label), run_time=0.3)

        # ── Projection panel placeholders ───────────────────────────
        # Actor-actor projection (bottom-left)
        aa_title = Text("Actor-Actor\nProjection", font_size=12, color=C["K_AA"])
        aa_title.to_corner(DL, buff=0.3).shift(UP * 0.3)

        # Component-component projection (bottom-right)
        cc_title = Text("Component-Component\nProjection", font_size=12, color=C["K_CC"])
        cc_title.to_corner(DR, buff=0.3).shift(UP * 0.3)

        self.play(FadeIn(aa_title), FadeIn(cc_title), run_time=0.3)

        # ── Animate snapshots ───────────────────────────────────────
        active_edges = {}       # (i, j) -> Line mobject
        aa_proj_edges = {}      # (i1, i2) -> Line
        cc_proj_edges = {}      # (j1, j2) -> Line

        for snap_idx, snap in enumerate(snapshots):
            adj = snap["adjacency"]  # n_actors x n_components

            # Update step label
            new_step_label = Text(
                f"Step: {snap.get('step', snap_idx + 1)}",
                font_size=22,
                color=C["text_light"],
            )
            new_step_label.to_corner(UL, buff=0.4)

            # Determine edges to add and remove
            new_edges = set()
            for i in range(min(self.n_actors, adj.shape[0])):
                for j in range(min(self.n_components, adj.shape[1])):
                    if adj[i, j] > 0:
                        new_edges.add((i, j))

            edges_to_add = new_edges - set(active_edges.keys())
            edges_to_remove = set(active_edges.keys()) - new_edges

            anims = [Transform(step_label, new_step_label)]

            # Remove old edges
            for key in edges_to_remove:
                if key in active_edges:
                    anims.append(FadeOut(active_edges[key]))
                    del active_edges[key]

            # Add new edges
            for (i, j) in edges_to_add:
                line = Line(
                    actors[i].get_center(),
                    components[j].get_center(),
                    stroke_color=C["teal"],
                    stroke_width=1.5,
                    stroke_opacity=0.6,
                )
                active_edges[(i, j)] = line
                anims.append(Create(line))

            # ── Update projections ──────────────────────────────────
            # Actor-actor projection: two actors share edge if they share a component
            aa_adj = adj @ adj.T
            np.fill_diagonal(aa_adj, 0)
            aa_new = set()
            for i1 in range(self.n_actors):
                for i2 in range(i1 + 1, self.n_actors):
                    if i1 < aa_adj.shape[0] and i2 < aa_adj.shape[1] and aa_adj[i1, i2] > 0:
                        aa_new.add((i1, i2))

            # Remove old AA projection edges
            for key in set(aa_proj_edges.keys()) - aa_new:
                anims.append(FadeOut(aa_proj_edges[key]))
                del aa_proj_edges[key]

            # Add new AA projection edges
            proj_aa_center = np.array([-5.5, -2.8, 0])
            proj_aa_scale = 0.4
            for (i1, i2) in aa_new - set(aa_proj_edges.keys()):
                p1 = proj_aa_center + np.array([
                    proj_aa_scale * np.cos(2 * np.pi * i1 / self.n_actors),
                    proj_aa_scale * np.sin(2 * np.pi * i1 / self.n_actors), 0
                ])
                p2 = proj_aa_center + np.array([
                    proj_aa_scale * np.cos(2 * np.pi * i2 / self.n_actors),
                    proj_aa_scale * np.sin(2 * np.pi * i2 / self.n_actors), 0
                ])
                line = Line(p1, p2, stroke_color=C["K_AA"], stroke_width=1.2, stroke_opacity=0.7)
                aa_proj_edges[(i1, i2)] = line
                anims.append(Create(line))

            # Component-component projection
            cc_adj = adj.T @ adj
            np.fill_diagonal(cc_adj, 0)
            cc_new = set()
            for j1 in range(self.n_components):
                for j2 in range(j1 + 1, self.n_components):
                    if j1 < cc_adj.shape[0] and j2 < cc_adj.shape[1] and cc_adj[j1, j2] > 0:
                        cc_new.add((j1, j2))

            for key in set(cc_proj_edges.keys()) - cc_new:
                anims.append(FadeOut(cc_proj_edges[key]))
                del cc_proj_edges[key]

            proj_cc_center = np.array([5.5, -2.8, 0])
            proj_cc_scale = 0.4
            for (j1, j2) in cc_new - set(cc_proj_edges.keys()):
                p1 = proj_cc_center + np.array([
                    proj_cc_scale * np.cos(2 * np.pi * j1 / self.n_components),
                    proj_cc_scale * np.sin(2 * np.pi * j1 / self.n_components), 0
                ])
                p2 = proj_cc_center + np.array([
                    proj_cc_scale * np.cos(2 * np.pi * j2 / self.n_components),
                    proj_cc_scale * np.sin(2 * np.pi * j2 / self.n_components), 0
                ])
                line = Line(p1, p2, stroke_color=C["K_CC"], stroke_width=1.2, stroke_opacity=0.7)
                cc_proj_edges[(j1, j2)] = line
                anims.append(Create(line))

            self.play(*anims, run_time=1.2)
            self.wait(0.2)

        # ── Final hold ──────────────────────────────────────────────
        self.wait(3)

    def _load_snapshots(self):
        """
        Load network snapshots from CSV files or generate demo data.

        Expected file format: data_dir/step_001.csv, step_002.csv, ...
        Each CSV is an adjacency matrix (actors as rows, components as cols).
        """
        if self.data_dir and Path(self.data_dir).exists():
            data_path = Path(self.data_dir)
            files = sorted(data_path.glob("step_*.csv"))
            snapshots = []
            for f in files:
                step_num = int(f.stem.split("_")[1])
                _, _, matrix = self.load_adjacency_csv(str(f))
                snapshots.append({"step": step_num, "adjacency": matrix})
            if snapshots:
                return snapshots

        return self._generate_demo_snapshots()

    def _generate_demo_snapshots(self, n_snapshots=10, seed=42):
        """Generate synthetic evolving bipartite network snapshots."""
        rng = np.random.default_rng(seed)
        snapshots = []

        # Start sparse, gradually densify with preferential attachment
        adj = np.zeros((self.n_actors, self.n_components))

        # Seed a few initial ties
        for i in range(self.n_actors):
            j = rng.integers(0, self.n_components)
            adj[i, j] = 1

        for step in range(1, n_snapshots + 1):
            # Add 1-3 ties
            n_add = rng.integers(1, 4)
            for _ in range(n_add):
                i = rng.integers(0, self.n_actors)
                # Preferential attachment: favor components with more ties
                col_degrees = adj.sum(axis=0) + 0.5
                probs = col_degrees / col_degrees.sum()
                j = rng.choice(self.n_components, p=probs)
                adj[i, j] = 1

            # Remove 0-1 ties
            if rng.random() < 0.4:
                existing = list(zip(*np.where(adj > 0)))
                if existing:
                    idx = rng.integers(0, len(existing))
                    ri, ci = existing[idx]
                    adj[ri, ci] = 0

            snapshots.append({"step": step * 20, "adjacency": adj.copy()})

        return snapshots
