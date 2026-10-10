"""
README animation: the network and the landscape, changing together
===================================================================
Renders man/figures/readme-landscape.gif (via tools/make_readme_gif.R, which
exports the data, calls this scene, and converts the MP4 to a GIF).

Left: the bipartite network of one seeded saomnk_run() (actors circles,
components squares; Okabe-Ito colors as in the README figures), the influence
matrix W, and the focal actor's objective over the run.
Right: the focal actor's fitness landscape -- the SAOM objective
f_i(x) = s_i(x)' theta over all 2^N portfolios x it could hold, given the other
actors' ties at that ministep -- laid out as a 16 x 16 grid (rows: which of
components A-D it holds; columns: which of E-H), each axis ordered by how
many of the four it holds, so the empty portfolio sits top left and a full
module at the bottom or right edge.

Every number drawn comes from the JSON written by tools/make_readme_gif.R; the
scene computes nothing about the model itself.

    SEARCHNET_README_JSON=/path/readme_landscape.json \
        manim render -r 880,520 --fps 10 scene_readme_landscape.py ReadmeLandscapeScene
"""

import json
import os

import numpy as np
from manim import (DOWN, LEFT, RIGHT, UP, Circle, Create, FadeOut, Line, Rectangle,
                   Scene, Square, Text, VGroup, VMobject, config, interpolate_color,
                   ManimColor)

# ── colors: Okabe-Ito, as in tools/make_readme_figures.R ──────────────────
ORANGE = "#E69F00"      # focal actor
SKY = "#56B4E9"         # other actors
BLUE = "#0072B2"        # components
VERMILLION = "#D55E00"  # a tie being dropped
INK = "#222222"
GREY = "#8c8c8c"
EDGE = "#b5b5b5"
# cividis anchors (colorblind-safe sequential) for the landscape
CIVIDIS = ["#00204D", "#414D6B", "#7C7B78", "#BCAF6F", "#FFEA46"]

config.background_color = "#FFFFFF"
config.frame_height = 8.0
config.frame_width = 8.0 * 880 / 520
# An explicit installed face: the generic "Sans" alias resolves inconsistently
# on Windows. Text is laid out at TEXT_UPSCALE x the target size and scaled
# down, because Pango's glyph positions are rounded to whole units at small
# font sizes, which runs words together ("isbetter") or splits them ("matr ix").
FONT = "Arial"
TEXT_UPSCALE = 8

LETTERS = "ABCDEFGH"


def cividis(u):
    u = float(np.clip(u, 0.0, 1.0)) * (len(CIVIDIS) - 1)
    k = min(int(u), len(CIVIDIS) - 2)
    return interpolate_color(ManimColor(CIVIDIS[k]), ManimColor(CIVIDIS[k + 1]), u - k)


# axis order: subsets of four components by size, then by bit pattern
ORDER = sorted(range(16), key=lambda b: (bin(b).count("1"), b))


def txt(s, size, color=INK, weight="NORMAL"):
    return Text(s, font=FONT, font_size=size * TEXT_UPSCALE, color=color,
                weight=weight, disable_ligatures=True).scale(1 / TEXT_UPSCALE)


class ReadmeLandscapeScene(Scene):
    def construct(self):
        path = os.environ.get("SEARCHNET_README_JSON", "readme_landscape.json")
        with open(path, encoding="utf-8") as fh:
            d = json.load(fh)
        M, N, focal = d["M"], d["N"], d["focal"]
        land = np.array(d["landscape"])            # (n_steps + 1) x 2^N
        steps = d["steps"]
        focal_row = d["focal_row"]                  # 1-based config row per state
        lo, hi = float(land.min()), float(land.max())
        norm = lambda v: (v - lo) / (hi - lo) if hi > lo else 0.5
        n_t = len(steps)
        assert N == 8, "the grid layout assumes N = 8 (two 4-component axes)"

        # ── left: the bipartite network ─────────────────────────────────
        hdr_l = txt("Actors hold components", 22, weight="BOLD").move_to([-3.65, 3.62, 0])
        sub_l = txt("circles: actors; squares: components (one simulated run)", 15, GREY).next_to(hdr_l, DOWN, buff=0.1)
        self.add(hdr_l, sub_l)

        ax = np.linspace(-5.85, -1.45, M)
        cx = np.linspace(-6.2, -1.1, N)
        y_a, y_c = 2.25, 0.25
        actors, comps = [], []
        for i in range(M):
            col = ORANGE if i + 1 == focal else SKY
            c = Circle(radius=0.27, color=col, fill_color=col, fill_opacity=1, stroke_width=0)
            c.move_to([ax[i], y_a, 0])
            lab = txt(str(i + 1), 17, "#FFFFFF", "BOLD").move_to(c)
            actors.append(VGroup(c, lab))
        for j in range(N):
            s = Square(side_length=0.46, color=BLUE, fill_color=BLUE, fill_opacity=1, stroke_width=0)
            s.move_to([cx[j], y_c, 0])
            lab = txt(LETTERS[j], 16, "#FFFFFF", "BOLD").move_to(s)
            comps.append(VGroup(s, lab))
        mod1 = txt("module 1", 13, GREY).move_to([(cx[0] + cx[3]) / 2, y_c - 0.48, 0])
        mod2 = txt("module 2", 13, GREY).move_to([(cx[4] + cx[7]) / 2, y_c - 0.48, 0])
        brk1 = Line([cx[0] - 0.23, y_c - 0.32, 0], [cx[3] + 0.23, y_c - 0.32, 0], color=GREY, stroke_width=2)
        brk2 = Line([cx[4] - 0.23, y_c - 0.32, 0], [cx[7] + 0.23, y_c - 0.32, 0], color=GREY, stroke_width=2)

        def edge(i, j):
            col = ORANGE if i + 1 == focal else EDGE
            w = 3.2 if i + 1 == focal else 2.0
            return Line([ax[i], y_a - 0.27, 0], [cx[j], y_c + 0.23, 0], color=col, stroke_width=w)

        edges = {}
        start = np.array(d["start"])
        for i in range(M):
            for j in range(N):
                if start[i][j] == 1:
                    edges[(i, j)] = edge(i, j)
        self.add(*edges.values(), *actors, *comps, mod1, mod2, brk1, brk2)

        # ── left, bottom: W and the focal actor's objective ───────────────
        W = np.array(d["W"])
        wc = 0.2
        w0 = np.array([-6.35, -1.55, 0])
        wcells = VGroup()
        for r in range(N):
            for c in range(N):
                fill = "#eeeeee" if r == c else (BLUE if W[r][c] > 0 else "#FFFFFF")
                sq = Square(side_length=wc, fill_color=fill, fill_opacity=1, color="#d0d0d0", stroke_width=0.6)
                sq.move_to(w0 + np.array([c * wc + wc / 2, -r * wc - wc / 2, 0]))
                wcells.add(sq)
        w_t = txt("influence matrix W", 13, INK, "BOLD").next_to(wcells, UP, buff=0.12).align_to(wcells, LEFT)
        w_s = txt("two modules", 12, GREY).next_to(wcells, DOWN, buff=0.1).align_to(wcells, LEFT)
        self.add(wcells, w_t, w_s)

        # trace axes
        tx0, tx1, ty0, ty1 = -4.2, -0.75, -3.35, -1.65
        f_focal = np.array([land[k][focal_row[k] - 1] for k in range(n_t + 1)])
        tlo, thi = lo, hi
        X = lambda k: tx0 + (tx1 - tx0) * k / n_t
        Y = lambda v: ty0 + (ty1 - ty0) * (v - tlo) / (thi - tlo)
        axis = VGroup(Line([tx0, ty0, 0], [tx1, ty0, 0], color=GREY, stroke_width=1.5),
                      Line([tx0, ty0, 0], [tx0, ty1, 0], color=GREY, stroke_width=1.5))
        t_t = txt(f"actor {focal}'s objective f", 13, INK, "BOLD").move_to([(tx0 + tx1) / 2, ty1 + 0.27, 0])
        t_x = txt("ministep", 12, GREY).move_to([(tx0 + tx1) / 2, ty0 - 0.22, 0])
        self.add(axis, t_t, t_x)
        trace = VMobject(color=ORANGE, stroke_width=3)
        trace.set_points_as_corners([[X(0), Y(f_focal[0]), 0], [X(0), Y(f_focal[0]), 0]])
        self.add(trace)

        # ── right: the landscape grid ─────────────────────────────────────
        hdr_r = txt(f"Fitness landscape of actor {focal}", 22, weight="BOLD").move_to([3.45, 3.66, 0])
        sub_r = txt("its objective for all 256 portfolios, given the others' ties",
                    15, GREY).next_to(hdr_r, DOWN, buff=0.1)
        self.add(hdr_r, sub_r)

        cs = 0.33
        g0 = np.array([1.1, 2.48, 0])               # top-left corner of the grid
        inv = {b: k for k, b in enumerate(ORDER)}
        def cell_xy(cfg):
            r, c = inv[cfg & 15], inv[cfg >> 4]
            return g0 + np.array([c * cs + cs / 2, -r * cs - cs / 2, 0])
        cells = []
        for cfg in range(256):
            sq = Square(side_length=cs, stroke_width=0, fill_opacity=1,
                        fill_color=cividis(norm(land[0][cfg])))
            sq.move_to(cell_xy(cfg))
            cells.append(sq)
        self.add(*cells)

        # axis glyphs: four mini squares per row (A-D) and per column (E-H)
        ms = 0.062
        def glyph(bits, horizontal):
            g = VGroup()
            for b in range(4):
                held = (bits >> b) & 1
                q = Square(side_length=ms, stroke_width=0.8, color=BLUE,
                           fill_color=BLUE if held else "#FFFFFF", fill_opacity=1)
                off = (b - 1.5) * (ms + 0.012)
                q.shift(np.array([off, 0, 0]) if horizontal else np.array([0, -off, 0]))
                g.add(q)
            return g
        for k in range(16):
            gr = glyph(ORDER[k], True).move_to(g0 + np.array([-0.2, -k * cs - cs / 2, 0]))
            gc = glyph(ORDER[k], False).move_to(g0 + np.array([k * cs + cs / 2, 0.2, 0]))
            self.add(gr, gc)
        ax_r = txt("holds which of A B C D", 13, GREY).rotate(np.pi / 2).move_to(g0 + np.array([-0.5, -8 * cs, 0]))
        ax_c = txt("holds which of E F G H", 13, GREY).move_to(g0 + np.array([8 * cs, 0.43, 0]))
        self.add(ax_r, ax_c)

        # colorbar + legend
        cb = VGroup()
        nseg = 40
        cb_x0, cb_x1, cb_y = 1.55, 4.05, -3.12
        for s in range(nseg):
            seg = Rectangle(width=(cb_x1 - cb_x0) / nseg + 0.005, height=0.14, stroke_width=0,
                            fill_color=cividis(s / (nseg - 1)), fill_opacity=1)
            seg.move_to([cb_x0 + (s + 0.5) * (cb_x1 - cb_x0) / nseg, cb_y, 0])
            cb.add(seg)
        cb_lo = txt("lower f", 12, GREY).next_to(cb, LEFT, buff=0.1)
        cb_hi = txt("higher f", 12, GREY).next_to(cb, RIGHT, buff=0.1)
        leg_dot = Circle(radius=0.1, color=INK, stroke_width=2, fill_color=ORANGE, fill_opacity=1).move_to([1.2, -3.55, 0])
        leg_dt = txt(f"actor {focal} now", 12, INK).next_to(leg_dot, RIGHT, buff=0.08)
        leg_pk = Circle(radius=0.075, color="#FFFFFF", stroke_width=2.5).move_to([3.35, -3.55, 0])
        leg_pk_bg = Square(side_length=0.26, stroke_width=0, fill_color=CIVIDIS[1], fill_opacity=1).move_to(leg_pk)
        leg_pt = txt("local peak (no single add/drop is better)", 12, INK).next_to(leg_pk_bg, RIGHT, buff=0.08)
        self.add(cb, cb_lo, cb_hi, leg_dot, leg_dt, leg_pk_bg, leg_pk, leg_pt)

        def peaks(f):
            return [cfg for cfg in range(256) if all(f[cfg] >= f[cfg ^ (1 << j)] for j in range(N))]
        def peak_marks(f):
            return VGroup(*[Circle(radius=0.075, color="#FFFFFF", stroke_width=2.5).move_to(cell_xy(p))
                            for p in peaks(f)])
        pk = peak_marks(land[0])
        marker = Circle(radius=0.12, color=INK, stroke_width=2.5, fill_color=ORANGE, fill_opacity=1)
        marker.move_to(cell_xy(focal_row[0] - 1))
        self.add(pk, marker)

        # caption
        def caption(k, s=None):
            if s is None:
                msg = f"ministep 0 of {n_t}: the start of the run"
            else:
                verb = {"add": "adds", "drop": "drops"}.get(s["change"])
                who = f"actor {s['actor']}"
                what = f"{verb} component {LETTERS[s['comp'] - 1]}" if verb else "keeps its portfolio"
                msg = f"ministep {k} of {n_t}: {who} {what}"
            col = ORANGE if (s is not None and s["actor"] == focal) else INK
            return txt(msg, 16, col, "BOLD").move_to([-3.65, -0.72, 0])
        cap = caption(0)
        self.add(cap)
        self.wait(0.8)

        # ── the run, ministep by ministep ─────────────────────────────────
        for k, s in enumerate(steps, start=1):
            is_focal = s["actor"] == focal
            changed = s["change"] != "none"
            rt = 0.45 if (is_focal and changed) else (0.1 if changed else 0.05)
            anims = []
            new_cap = caption(k, s)
            cap.become(new_cap)
            i, j = s["actor"] - 1, s["comp"] - 1
            dropped = None
            if s["change"] == "add":
                e = edge(i, j)
                edges[(i, j)] = e
                anims.append(Create(e))
            elif s["change"] == "drop":
                dropped = edges.pop((i, j))
                dropped.set_stroke(VERMILLION, width=3.5)
                anims.append(FadeOut(dropped))
            for cfg in range(256):
                new = cividis(norm(land[k][cfg]))
                if land[k][cfg] != land[k - 1][cfg]:
                    anims.append(cells[cfg].animate.set_fill(new))
            if focal_row[k] != focal_row[k - 1]:
                anims.append(marker.animate.move_to(cell_xy(focal_row[k] - 1)))
            new_pk = peak_marks(land[k])
            pk.become(new_pk)
            pts = [[X(m), Y(f_focal[m]), 0] for m in range(k + 1)]
            anims.append(trace.animate.set_points_as_corners(pts))
            self.play(*anims, run_time=rt)
            for e in actors + comps:
                self.bring_to_front(e)
            self.bring_to_front(pk, marker)

        last = focal_row[-1] - 1
        held = " ".join(LETTERS[b] for b in range(N) if (last >> b) & 1) or "nothing"
        where = ", a local peak" if last in peaks(land[-1]) else ""
        end = txt(f"end of the run: actor {focal} holds {held}{where}", 16, INK, "BOLD")
        end.move_to([-3.65, -0.72, 0])
        cap.become(end)
        self.wait(1.6)
