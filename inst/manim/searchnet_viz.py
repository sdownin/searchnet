"""
SearchNet Visualization Utilities
==================================
Base module for all SearchNet (SaoMNK) manim animations.
Provides color palettes, base scene class, NK landscape generators,
and CSV data loaders for R-exported simulation output.

Usage:
    from searchnet_viz import SEARCHNET_COLORS, BaseSearchnetScene, nk_fitness_values
"""

import csv
import itertools
from pathlib import Path

import numpy as np
from manim import *


# ─────────────────────────────────────────────────────────────────────
# Color Palette — consistent branding across all SearchNet scenes
# ─────────────────────────────────────────────────────────────────────
SEARCHNET_COLORS = {
    # Primary palette
    "navy":       "#1b3a5c",
    "teal":       "#2a9d8f",
    "amber":      "#e9c46a",
    "coral":      "#e76f51",

    # Extended palette
    "slate":      "#415a77",
    "sage":       "#6b9080",
    "plum":       "#7b2d8e",
    "steel":      "#778da9",

    # Background and text
    "bg_dark":    "#0f1729",
    "bg_panel":   "#1a2238",
    "text_light": "#e8e8e8",
    "text_dim":   "#8899aa",

    # Semantic colors
    "highlight":  "#f4a261",
    "warning":    "#e76f51",
    "positive":   "#2a9d8f",
    "neutral":    "#778da9",

    # Degree types (K-4)
    "K_AC":       "#2a9d8f",   # teal — actor->component
    "K_CA":       "#e9c46a",   # amber — component->actor
    "K_AA":       "#1b3a5c",   # navy — actor->actor
    "K_CC":       "#e76f51",   # coral — component->component

    # Node types
    "actor":      "#4fc3f7",   # light blue circles
    "component":  "#e9c46a",   # amber squares
}

# Shorthand alias
C = SEARCHNET_COLORS


# ─────────────────────────────────────────────────────────────────────
# Base Scene Class
# ─────────────────────────────────────────────────────────────────────
class BaseSearchnetScene(Scene):
    """
    Base class for all SearchNet manim scenes.
    Provides standardized title cards, source annotations, and data loading.
    """

    scene_title = "SearchNet Visualization"
    scene_subtitle = ""

    def setup(self):
        """Set dark background on scene setup."""
        self.camera.background_color = C["bg_dark"]

    def add_title(self, title_text=None, subtitle_text=None, font="Times New Roman"):
        """
        Display a centered title card, then fade it out.
        Returns the (title, subtitle) mobjects.
        """
        title_text = title_text or self.scene_title
        subtitle_text = subtitle_text or self.scene_subtitle

        title = Text(
            title_text,
            font_size=36,
            color=C["text_light"],
            font=font,
            weight=BOLD,
        )

        sub = None
        if subtitle_text:
            sub = Text(
                subtitle_text,
                font_size=18,
                color=C["text_dim"],
                font=font,
                slant=ITALIC,
            )
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
        """
        Add a title that remains at the top of the frame.
        Returns the title mobject.
        """
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
        """
        Add a small source annotation in the bottom-right corner.
        """
        source = Text(
            source_text,
            font_size=font_size,
            color=C["text_dim"],
            slant=ITALIC,
        ).to_corner(DR, buff=0.25)
        self.add(source)
        return source

    @staticmethod
    def load_csv(path):
        """
        Load a generic CSV file into a list of dicts.
        """
        path = Path(path)
        if not path.exists():
            raise FileNotFoundError(f"CSV not found: {path}")
        with open(path, "r", newline="", encoding="utf-8") as f:
            reader = csv.DictReader(f)
            return list(reader)

    @staticmethod
    def load_trajectory(csv_path):
        """
        Load R-exported trajectory CSV.

        Expected format — columns: step (or round), plus any number of
        numeric value columns. Optionally a 'condition' column for grouping.

        Returns:
            dict mapping condition_name -> list of (step, value, sd) tuples
            If no condition column, returns {"default": [(step, val, sd), ...]}.
        """
        rows = BaseSearchnetScene.load_csv(csv_path)
        if not rows:
            return {}

        headers = list(rows[0].keys())

        # Detect condition column
        has_condition = "condition" in headers

        # Detect step column
        step_col = "step" if "step" in headers else "round" if "round" in headers else headers[0]

        # Detect value and sd columns
        val_col = None
        sd_col = None
        for h in headers:
            if h.lower() in ("mean", "value", "y", "fitness"):
                val_col = h
            if h.lower() in ("sd", "se", "std", "stderr"):
                sd_col = h

        # If no explicit value column, use the second numeric column
        if val_col is None:
            numeric_cols = [h for h in headers if h not in (step_col, "condition")]
            val_col = numeric_cols[0] if numeric_cols else headers[1]

        result = {}
        for row in rows:
            cond = row.get("condition", "default") if has_condition else "default"
            step = float(row[step_col])
            val = float(row[val_col])
            sd = float(row[sd_col]) if sd_col and row.get(sd_col) else 0.0

            if cond not in result:
                result[cond] = []
            result[cond].append((step, val, sd))

        # Sort each condition by step
        for cond in result:
            result[cond].sort(key=lambda x: x[0])

        return result

    @staticmethod
    def load_adjacency_csv(csv_path):
        """
        Load an adjacency matrix from CSV.
        First column is row labels, remaining columns are targets.
        Returns (row_labels, col_labels, numpy_matrix).
        """
        rows = BaseSearchnetScene.load_csv(csv_path)
        if not rows:
            return [], [], np.array([])

        headers = list(rows[0].keys())
        label_col = headers[0]
        col_labels = headers[1:]
        row_labels = [r[label_col] for r in rows]

        matrix = np.zeros((len(rows), len(col_labels)))
        for i, row in enumerate(rows):
            for j, col in enumerate(col_labels):
                try:
                    matrix[i, j] = float(row[col])
                except (ValueError, KeyError):
                    matrix[i, j] = 0.0

        return row_labels, col_labels, matrix


# ─────────────────────────────────────────────────────────────────────
# NK Fitness Landscape Generators
# ─────────────────────────────────────────────────────────────────────
def nk_fitness_values(N=6, K=2, seed=42):
    """
    Generate a full NK fitness landscape.

    Parameters
    ----------
    N : int
        Number of loci (dimensions).
    K : int
        Epistatic interactions per locus.
    seed : int
        Random seed for reproducibility.

    Returns
    -------
    dict
        Mapping from binary configuration tuples to fitness values in [0, 1].
    """
    rng = np.random.default_rng(seed)

    # For each locus, randomly choose K other loci it interacts with
    interactions = {}
    for i in range(N):
        others = list(range(N))
        others.remove(i)
        interactions[i] = sorted(rng.choice(others, size=min(K, len(others)), replace=False).tolist())

    # Contribution tables: for each locus, fitness depends on own state + K interacting
    contribution_tables = {}
    for i in range(N):
        n_combos = 2 ** (min(K, N - 1) + 1)
        contribution_tables[i] = rng.uniform(0, 1, size=n_combos)

    # Compute fitness for every configuration
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
    """
    Map continuous (x, y) in [0,1]^2 to a fitness value by interpolating
    the NK landscape onto a smooth surface for 3D visualization.

    Parameters
    ----------
    x, y : float
        Coordinates in [0, 1].
    fitness_cache : dict
        Output of nk_fitness_values().
    smoothing : float
        Controls ruggedness amplitude.

    Returns
    -------
    float
        Interpolated fitness value.
    """
    val = 0.0
    keys = list(fitness_cache.keys())
    n_terms = min(len(keys), 20)

    for i in range(n_terms):
        f = fitness_cache[keys[i]]
        freq_x = 1 + (i % 5) * 1.5
        freq_y = 1 + ((i * 3) % 7) * 1.2
        phase_x = f * 2 * np.pi
        phase_y = (1 - f) * 2 * np.pi
        val += f * np.sin(freq_x * x * np.pi + phase_x) * np.cos(freq_y * y * np.pi + phase_y)

    val = val / n_terms
    # Add controlled ruggedness
    val += smoothing * np.sin(5 * x * np.pi) * np.sin(5 * y * np.pi)
    val += 0.15 * np.sin(8 * x * np.pi + 1) * np.cos(7 * y * np.pi + 2)
    return val


# ─────────────────────────────────────────────────────────────────────
# Block-Diagonal Matrix Helper
# ─────────────────────────────────────────────────────────────────────
def create_block_diagonal_matrix(N, blocks):
    """
    Create a block-diagonal adjacency/interaction matrix.
    Python equivalent of the R helper in SaoMNK.

    Parameters
    ----------
    N : int
        Total matrix dimension.
    blocks : list of tuples
        Each tuple is (start_row, end_row, start_col, end_col, value).
        Indices are 0-based.

    Returns
    -------
    np.ndarray
        N x N matrix with specified blocks filled.

    Example
    -------
    >>> # 2x2 block structure for 4 actors + 4 components
    >>> M = create_block_diagonal_matrix(8, [
    ...     (0, 4, 0, 4, 1),   # actor-actor block
    ...     (4, 8, 4, 8, 1),   # component-component block
    ...     (0, 4, 4, 8, 1),   # actor-component block
    ...     (4, 8, 0, 4, 1),   # component-actor block
    ... ])
    """
    M = np.zeros((N, N))
    for (r0, r1, c0, c1, val) in blocks:
        M[r0:r1, c0:c1] = val
    return M


# ─────────────────────────────────────────────────────────────────────
# Smoothing Utility
# ─────────────────────────────────────────────────────────────────────
def loess_smooth(x, y, frac=0.3, n_out=100):
    """
    Simple local-weighted regression smoothing (LOESS-like).
    Uses a Gaussian kernel for local weighting.

    Parameters
    ----------
    x, y : array-like
        Input data points.
    frac : float
        Bandwidth fraction (0 to 1). Larger = smoother.
    n_out : int
        Number of output points.

    Returns
    -------
    (x_smooth, y_smooth) : tuple of np.ndarray
    """
    x = np.asarray(x, dtype=float)
    y = np.asarray(y, dtype=float)

    x_smooth = np.linspace(x.min(), x.max(), n_out)
    y_smooth = np.zeros(n_out)

    bandwidth = frac * (x.max() - x.min())
    if bandwidth < 1e-10:
        bandwidth = 1.0

    for i, xi in enumerate(x_smooth):
        weights = np.exp(-0.5 * ((x - xi) / bandwidth) ** 2)
        weights /= weights.sum() + 1e-12
        y_smooth[i] = np.dot(weights, y)

    return x_smooth, y_smooth
