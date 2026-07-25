#!/usr/bin/env bash
# ============================================================
# SearchNet Manim — Batch Render Script
# ============================================================
# Usage:
#   bash render_all.sh          # high quality (1080p60)
#   bash render_all.sh -ql      # low quality preview (480p15)
#   bash render_all.sh -qk      # 4K render (2160p60)
# ============================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Quality flag: default to high quality
QUALITY="${1:--qh}"

echo "============================================"
echo "SearchNet Manim Batch Render"
echo "Quality: $QUALITY"
echo "Directory: $SCRIPT_DIR"
echo "============================================"
echo ""

# Track timing
START_TIME=$(date +%s)
SCENES_RENDERED=0
SCENES_FAILED=0

render_scene() {
    local file="$1"
    local scene="$2"
    echo "--- Rendering: $scene ($file) ---"
    if manim "$QUALITY" --config_file manim.cfg "$file" "$scene"; then
        echo "    [OK] $scene"
        SCENES_RENDERED=$((SCENES_RENDERED + 1))
    else
        echo "    [FAIL] $scene"
        SCENES_FAILED=$((SCENES_FAILED + 1))
    fi
    echo ""
}

# Scene 1: K-4 Degree Evolution
render_scene "scene_k4_evolution.py" "K4EvolutionScene"

# Scene 2: Bipartite Network Evolution
render_scene "scene_bipartite_evolution.py" "BipartiteEvolutionScene"

# Scene 3: 3D Fitness Landscape
render_scene "scene_landscape_3d.py" "LandscapeScene"

# Scene 4: Exogenous Shock
render_scene "scene_shock.py" "ShockScene"

# Summary
END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))

echo "============================================"
echo "Batch render complete"
echo "  Rendered: $SCENES_RENDERED"
echo "  Failed:   $SCENES_FAILED"
echo "  Time:     ${ELAPSED}s"
echo "  Output:   $SCRIPT_DIR/media/"
echo "============================================"
