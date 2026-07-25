#!/usr/bin/env python
"""
CLI for rendering SearchNet (SaoMNK) dashboard panels
=======================================================
Renders individual scenes or a composed multi-panel dashboard.

Usage:
    # Single scene
    python render_dashboard.py --scene k4_evolution --quality medium
    python render_dashboard.py --scene fitness_landscape --quality high

    # Composed dashboard (all panels in 2x3 grid)
    python render_dashboard.py --dashboard all --quality high --output dashboard.mp4

    # Specific panels in dashboard
    python render_dashboard.py --dashboard k4_evolution,shock_response --quality medium

    # List available scenes
    python render_dashboard.py --list

Available scenes:
    k4_evolution        4-panel K_AC/K_CC/K_CA/K_AA trajectories
    bipartite_network   Animated firm x route bipartite network
    fitness_landscape   3D NK surface with climbing agents
    information_funnel  Cross-layer observation waterfall
    shock_response      Pre/post shock split-screen
    game_theoretic      Congestion game payoff + QRE annealing

Requires: manim CE, numpy
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

# This script lives alongside dashboard_panels.py inside
# SaoMNK/inst/manim/ within the searchnet package.
MODULE_DIR = Path(__file__).resolve().parent
MODULE_FILE = MODULE_DIR / "dashboard_panels.py"

SCENE_REGISTRY = {
    "k4_evolution":       "KDegreeEvolutionScene",
    "bipartite_network":  "BipartiteNetworkScene",
    "fitness_landscape":  "FitnessLandscapeScene",
    "information_funnel": "InformationFunnelScene",
    "shock_response":     "ShockResponseScene",
    "game_theoretic":     "GameTheoreticScene",
}

QUALITY_FLAGS = {
    "low":    "-ql",
    "medium": "-qm",
    "high":   "-qh",
    "4k":     "-qk",
}

# Default 2x3 grid layout for the full dashboard
DEFAULT_GRID = [
    ("k4_evolution",       0, 0),
    ("bipartite_network",  0, 1),
    ("fitness_landscape",  0, 2),
    ("information_funnel", 1, 0),
    ("shock_response",     1, 1),
    ("game_theoretic",     1, 2),
]


def render_single(scene_key: str, quality: str = "high",
                  output: str | None = None, fps: int = 30) -> int:
    """Render a single scene via manim CLI.

    Args:
        scene_key: Key from SCENE_REGISTRY (e.g. 'k4_evolution').
        quality: One of 'low', 'medium', 'high', '4k'.
        output: Optional output filename override.
        fps: Frames per second.

    Returns:
        Subprocess return code.
    """
    scene_name = SCENE_REGISTRY.get(scene_key)
    if not scene_name:
        print(f"[ERROR] Unknown scene: {scene_key}")
        print(f"  Available: {', '.join(sorted(SCENE_REGISTRY))}")
        return 1

    q_flag = QUALITY_FLAGS.get(quality, "-qh")

    cmd = [
        sys.executable, "-m", "manim", "render",
        q_flag,
        "--fps", str(fps),
        str(MODULE_FILE),
        scene_name,
    ]

    if output:
        cmd.extend(["-o", output])

    print(f"[render_dashboard] Rendering {scene_name} ({quality}) ...")
    print(f"  cmd: {' '.join(cmd)}")

    result = subprocess.run(cmd, cwd=str(MODULE_DIR))
    if result.returncode == 0:
        print(f"[render_dashboard] {scene_name} rendered successfully.")
    else:
        print(f"[render_dashboard] {scene_name} FAILED (code {result.returncode}).")

    return result.returncode


def render_dashboard(panel_keys: list[str] | None = None,
                     quality: str = "high",
                     output: str = "dashboard.mp4",
                     fps: int = 30) -> int:
    """Render multiple panels and composite into a grid.

    Args:
        panel_keys: List of scene keys, or None for all panels.
        quality: Render quality.
        output: Output file path.
        fps: Frames per second.

    Returns:
        0 on success, non-zero on failure.
    """
    # Determine which panels to include
    if panel_keys is None or panel_keys == ["all"]:
        grid = DEFAULT_GRID
    else:
        # Auto-arrange specified panels into a grid
        grid = []
        n = len(panel_keys)
        cols = min(n, 3)
        for i, key in enumerate(panel_keys):
            if key not in SCENE_REGISTRY:
                print(f"[ERROR] Unknown scene: {key}")
                return 1
            grid.append((key, i // cols, i % cols))

    # Import DashboardComposer
    sys.path.insert(0, str(MODULE_DIR))
    from dashboard_panels import DashboardComposer, SCENE_REGISTRY as SR

    composer = DashboardComposer()
    for key, row, col in grid:
        scene_cls = SR.get(key)
        if scene_cls is None:
            print(f"[WARN] Skipping unknown scene: {key}")
            continue
        composer.add_panel(scene_cls, row=row, col=col)

    try:
        composer.render(
            output_path=output,
            fps=fps,
            quality=quality,
            module_path=str(MODULE_FILE),
        )
        print(f"[render_dashboard] Dashboard saved to {output}")
        return 0
    except RuntimeError as e:
        print(f"[render_dashboard] Dashboard render FAILED: {e}")
        return 1


def main():
    parser = argparse.ArgumentParser(
        description="Render SearchNet (SaoMNK) dashboard panels",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )

    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--scene", type=str,
        help="Render a single scene by key (e.g. k4_evolution)")
    group.add_argument(
        "--dashboard", type=str, nargs="?", const="all",
        help="Render composed dashboard. 'all' or comma-separated scene keys")
    group.add_argument(
        "--list", action="store_true",
        help="List available scenes and exit")

    parser.add_argument(
        "--quality", type=str, default="high",
        choices=["low", "medium", "high", "4k"],
        help="Render quality (default: high)")
    parser.add_argument(
        "--output", "-o", type=str, default=None,
        help="Output filename (default: <scene>.mp4 or dashboard.mp4)")
    parser.add_argument(
        "--fps", type=int, default=30,
        help="Frames per second (default: 30)")

    args = parser.parse_args()

    if args.list:
        print("Available scenes:")
        print("-" * 55)
        for key, class_name in sorted(SCENE_REGISTRY.items()):
            print(f"  {key:<24s} {class_name}")
        print()
        print("Usage examples:")
        print("  python render_dashboard.py --scene k4_evolution --quality medium")
        print("  python render_dashboard.py --dashboard all --quality high")
        print("  python render_dashboard.py --dashboard k4_evolution,shock_response")
        return 0

    if args.scene:
        return render_single(
            args.scene, quality=args.quality,
            output=args.output, fps=args.fps)

    if args.dashboard is not None:
        panels = None if args.dashboard == "all" else args.dashboard.split(",")
        out = args.output or "dashboard.mp4"
        return render_dashboard(
            panel_keys=panels, quality=args.quality,
            output=out, fps=args.fps)

    return 0


if __name__ == "__main__":
    sys.exit(main())
