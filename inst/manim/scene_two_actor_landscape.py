"""
Two focal actors, two landscapes, one portfolio space
=====================================================
A two-actor companion to scene_readme_landscape.py, rendered by
tools/make_two_actor_gif.R (which exports the data, calls this scene, and
converts the lossless .mov to a GIF). It reuses that scene's text helper, color map and
axis order by import.

Left: the bipartite network of one seeded saomnk_run() with two focal actors
in their own colors (Okabe-Ito orange and blue; the other actors grey,
components dark grey), the influence matrix W, and both focal actors'
objectives over the run.
Right: the two actors do NOT share a landscape. Each one's objective
f_i(x) = s_i(x)' theta depends on the other actors' ties, the other focal
actor's included. Their portfolios do live in the same space, so the scene
draws two 16 x 16 grids on the same axes (rows: which of A-D held; columns:
which of E-H), each colored by ONE actor's own objective on a shared color
scale, and marks both actors' current portfolios on both grids: the grid's
own actor as a solid token, the other focal actor as a hollow ring.
Landscapes are never averaged.

When one focal actor's move changes whether a single add or drop would raise
the other's objective from where the other stands, the caption says so and
that neighboring portfolio is outlined on the other's grid.

Every number drawn comes from the JSON written by tools/make_two_actor_gif.R.

    SEARCHNET_TWO_ACTOR_JSON=/path/two_actor_landscape.json \
        manim render -r 1100,560 --fps 10 scene_two_actor_landscape.py TwoActorLandscapeScene
"""

import json
import os
import sys

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from manim import (DOWN, LEFT, RIGHT, UP, Circle, Create, FadeOut, Line, Rectangle,  # noqa: E402
                   Scene, Square, VGroup, VMobject, config)
from scene_readme_landscape import (CIVIDIS, GREY, INK, LETTERS, ORDER, cividis,  # noqa: E402
                                    txt)

# ── colors: Okabe-Ito focal pair; everything else neutral ────────────────
FOCAL_COLORS = ["#E69F00", "#0072B2"]   # orange, blue (fs_oi in tools/figures_shared.R)
OTHER = "#c4c4c4"                        # other actors
COMP = "#555555"                         # components, W, axis glyphs
EDGE = "#c9c9c9"
DROP = "#000000"                         # a tie being dropped

config.background_color = "#FFFFFF"
config.frame_height = 8.0
config.frame_width = 8.0 * 1100 / 560


class TwoActorLandscapeScene(Scene):
    def construct(self):
        path = os.environ.get("SEARCHNET_TWO_ACTOR_JSON", "two_actor_landscape.json")
        with open(path, encoding="utf-8") as fh:
            d = json.load(fh)
        M, N = d["M"], d["N"]
        F = d["focals"]                                  # two actor ids, 1-based
        lands = [np.array(L) for L in d["landscapes"]]   # each (n_steps + 1) x 2^N
        rows = [[r - 1 for r in rr] for rr in d["rows"]]  # 0-based config per state
        steps = d["steps"]
        n_t = len(steps)
        assert N == 8, "the grid layout assumes N = 8 (two 4-component axes)"
        # one color scale for both grids, so equal colors mean equal f
        lo = float(min(L.min() for L in lands))
        hi = float(max(L.max() for L in lands))
        norm = lambda v: (v - lo) / (hi - lo) if hi > lo else 0.5
        col_of = {F[0]: FOCAL_COLORS[0], F[1]: FOCAL_COLORS[1]}
        # an opaque white backdrop, so the lossless (--transparent, qtrle) render
        # the driver asks for is identical to an ordinary one on white
        self.add(Rectangle(width=config.frame_width + 0.2, height=config.frame_height + 0.2,
                           stroke_width=0, fill_color="#FFFFFF", fill_opacity=1))

        # ── left: the bipartite network ─────────────────────────────────
        LX = -5.15                                       # left-panel center
        hdr_l = txt("Actors hold components", 22, weight="BOLD").move_to([LX, 3.62, 0])
        sub_l = txt("circles: actors; squares: components (one simulated run)", 14, GREY)
        sub_l.next_to(hdr_l, DOWN, buff=0.1)
        self.add(hdr_l, sub_l)

        ax = np.linspace(-7.25, -3.05, M)
        cx = np.linspace(-7.55, -2.75, N)
        y_a, y_c = 2.4, 0.6
        actors, comps = [], []
        for i in range(M):
            focal = (i + 1) in col_of
            col = col_of.get(i + 1, OTHER)
            r = 0.29 if focal else 0.25
            c = Circle(radius=r, color=col, fill_color=col, fill_opacity=1, stroke_width=0)
            c.move_to([ax[i], y_a, 0])
            lab = txt(str(i + 1), 17, "#FFFFFF" if focal else INK, "BOLD").move_to(c)
            actors.append(VGroup(c, lab))
        for j in range(N):
            s = Square(side_length=0.44, color=COMP, fill_color=COMP, fill_opacity=1, stroke_width=0)
            s.move_to([cx[j], y_c, 0])
            lab = txt(LETTERS[j], 16, "#FFFFFF", "BOLD").move_to(s)
            comps.append(VGroup(s, lab))
        mod1 = txt("module 1", 13, GREY).move_to([(cx[0] + cx[3]) / 2, y_c - 0.47, 0])
        mod2 = txt("module 2", 13, GREY).move_to([(cx[4] + cx[7]) / 2, y_c - 0.47, 0])
        brk1 = Line([cx[0] - 0.22, y_c - 0.31, 0], [cx[3] + 0.22, y_c - 0.31, 0], color=GREY, stroke_width=2)
        brk2 = Line([cx[4] - 0.22, y_c - 0.31, 0], [cx[7] + 0.22, y_c - 0.31, 0], color=GREY, stroke_width=2)

        def edge(i, j):
            col = col_of.get(i + 1, EDGE)
            w = 3.4 if (i + 1) in col_of else 2.0
            r = 0.29 if (i + 1) in col_of else 0.25
            return Line([ax[i], y_a - r, 0], [cx[j], y_c + 0.22, 0], color=col, stroke_width=w)

        edges = {}
        start = np.array(d["start"])
        for i in range(M):
            for j in range(N):
                if start[i][j] == 1:
                    edges[(i, j)] = edge(i, j)
        # focal edges drawn above the grey ones
        order = sorted(edges, key=lambda k: (k[0] + 1) in col_of)
        self.add(*[edges[k] for k in order], *actors, *comps, mod1, mod2, brk1, brk2)

        # ── left, bottom: W and the two objectives ───────────────────────
        W = np.array(d["W"])
        wc = 0.19
        w0 = np.array([-7.7, -1.62, 0])
        wcells = VGroup()
        for r in range(N):
            for c in range(N):
                fill = "#eeeeee" if r == c else (COMP if W[r][c] > 0 else "#FFFFFF")
                sq = Square(side_length=wc, fill_color=fill, fill_opacity=1, color="#d0d0d0", stroke_width=0.6)
                sq.move_to(w0 + np.array([c * wc + wc / 2, -r * wc - wc / 2, 0]))
                wcells.add(sq)
        w_t = txt("influence matrix W", 13, INK, "BOLD").next_to(wcells, UP, buff=0.12).align_to(wcells, LEFT)
        w_s = txt("two modules", 12, GREY).next_to(wcells, DOWN, buff=0.1).align_to(wcells, LEFT)
        self.add(wcells, w_t, w_s)

        tx0, tx1, ty0, ty1 = -5.55, -2.65, -3.4, -1.75
        f_tr = [np.array([lands[a][k][rows[a][k]] for k in range(n_t + 1)]) for a in range(2)]
        X = lambda k: tx0 + (tx1 - tx0) * k / n_t
        Y = lambda v: ty0 + (ty1 - ty0) * (v - lo) / (hi - lo)
        axis = VGroup(Line([tx0, ty0, 0], [tx1, ty0, 0], color=GREY, stroke_width=1.5),
                      Line([tx0, ty0, 0], [tx0, ty1, 0], color=GREY, stroke_width=1.5))
        t_t = txt("each focal actor's objective f", 13, INK, "BOLD")
        t_t.move_to([(tx0 + tx1) / 2, ty1 + 0.42, 0])
        legs = VGroup()
        for a in range(2):
            ln = Line([0, 0, 0], [0.32, 0, 0], color=FOCAL_COLORS[a], stroke_width=4)
            lt = txt(f"actor {F[a]}", 12, INK).next_to(ln, RIGHT, buff=0.07)
            legs.add(VGroup(ln, lt))
        legs.arrange(RIGHT, buff=0.3).move_to([(tx0 + tx1) / 2, ty1 + 0.14, 0])
        t_x = txt("ministep", 12, GREY).move_to([(tx0 + tx1) / 2, ty0 - 0.22, 0])
        self.add(axis, t_t, legs, t_x)
        traces = []
        for a in range(2):
            # the first trace wider, so it shows around the second where they coincide
            tr = VMobject(color=FOCAL_COLORS[a], stroke_width=5.5 if a == 0 else 2.6)
            p0 = [X(0), Y(f_tr[a][0]), 0]
            tr.set_points_as_corners([p0, p0])
            traces.append(tr)
        self.add(*traces)

        # ── right: two grids on the same axes ─────────────────────────────
        cs = 0.27
        G0 = [np.array([-1.55, 2.42, 0]), np.array([3.35, 2.42, 0])]  # top-left corners
        inv = {b: k for k, b in enumerate(ORDER)}

        def cell_xy(g, cfg):
            r, c = inv[cfg & 15], inv[cfg >> 4]
            return G0[g] + np.array([c * cs + cs / 2, -r * cs - cs / 2, 0])

        gw = 16 * cs
        sub_r = txt("each grid: one actor's objective for all 256 portfolios, given everyone else's ties",
                    14, GREY).move_to([(G0[0][0] + G0[1][0] + gw) / 2, 3.3, 0])
        self.add(sub_r)
        cells = [[], []]
        ms = 0.055

        def glyph(bits, horizontal):
            g = VGroup()
            for b in range(4):
                held = (bits >> b) & 1
                q = Square(side_length=ms, stroke_width=0.8, color=COMP,
                           fill_color=COMP if held else "#FFFFFF", fill_opacity=1)
                off = (b - 1.5) * (ms + 0.011)
                q.shift(np.array([off, 0, 0]) if horizontal else np.array([0, -off, 0]))
                g.add(q)
            return g

        for g in range(2):
            hdr = txt(f"Landscape of actor {F[g]}", 21, FOCAL_COLORS[g], "BOLD")
            hdr.move_to([G0[g][0] + gw / 2, 3.66, 0])
            self.add(hdr)
            for cfg in range(256):
                sq = Square(side_length=cs, stroke_width=0, fill_opacity=1,
                            fill_color=cividis(norm(lands[g][0][cfg])))
                sq.move_to(cell_xy(g, cfg))
                cells[g].append(sq)
            self.add(*cells[g])
            for k in range(16):
                self.add(glyph(ORDER[k], True).move_to(G0[g] + np.array([-0.19, -k * cs - cs / 2, 0])))
                self.add(glyph(ORDER[k], False).move_to(G0[g] + np.array([k * cs + cs / 2, 0.19, 0])))
            ax_c = txt("holds which of E F G H", 12, GREY).move_to(G0[g] + np.array([gw / 2, 0.42, 0]))
            self.add(ax_c)
        ax_r = txt("holds which of A B C D", 12, GREY).rotate(np.pi / 2)
        ax_r.move_to(G0[0] + np.array([-0.45, -gw / 2, 0]))
        self.add(ax_r)

        # peaks: no single add or drop raises f (a white square outline)
        def peaks(f):
            return [cfg for cfg in range(256) if all(f[cfg] >= f[cfg ^ (1 << j)] for j in range(N))]

        def peak_marks(g, f):
            return VGroup(*[Square(side_length=cs - 0.035, color="#FFFFFF", stroke_width=2.2)
                            .move_to(cell_xy(g, p)) for p in peaks(f)])

        pks = [peak_marks(g, lands[g][0]) for g in range(2)]
        self.add(*pks)

        # tokens: on grid g, actor g solid, the other focal actor hollow
        def solid(a):
            return Circle(radius=0.095, color=INK, stroke_width=2, fill_color=FOCAL_COLORS[a], fill_opacity=1)

        def hollow(a):
            under = Circle(radius=0.122, color=INK, stroke_width=5.5)
            ring = Circle(radius=0.122, color=FOCAL_COLORS[a], stroke_width=3)
            return VGroup(under, ring)

        # tok[g][a]: actor a's token on grid g
        tok = [[None, None], [None, None]]
        for g in range(2):
            for a in range(2):
                m = solid(a) if a == g else hollow(a)
                m.move_to(cell_xy(g, rows[a][0]))
                tok[g][a] = m
            self.add(tok[g][1 - g], tok[g][g])           # own token on top

        # legend + color bar, under the grids
        RX = (G0[0][0] + G0[1][0] + gw) / 2
        cb = VGroup()
        nseg = 40
        cb_x0, cb_x1, cb_y = RX - 1.6, RX + 1.6, -2.28
        for s in range(nseg):
            seg = Rectangle(width=(cb_x1 - cb_x0) / nseg + 0.005, height=0.14, stroke_width=0,
                            fill_color=cividis(s / (nseg - 1)), fill_opacity=1)
            seg.move_to([cb_x0 + (s + 0.5) * (cb_x1 - cb_x0) / nseg, cb_y, 0])
            cb.add(seg)
        cb_lo = txt("lower f", 12, GREY).next_to(cb, LEFT, buff=0.1)
        cb_hi = txt("higher f  (one scale, both grids)", 12, GREY).next_to(cb, RIGHT, buff=0.1)
        self.add(cb, cb_lo, cb_hi)

        ly = -2.72
        l1 = solid(0).move_to([0, 0, 0])
        l1b = solid(1).next_to(l1, RIGHT, buff=0.05)
        l1t = txt("the grid's own actor", 12, INK).next_to(l1b, RIGHT, buff=0.08)
        l2 = hollow(1).next_to(l1t, RIGHT, buff=0.35)
        l2b = hollow(0).next_to(l2, RIGHT, buff=0.05)
        l2t = txt("the other focal actor", 12, INK).next_to(l2b, RIGHT, buff=0.08)
        pk_bg = Square(side_length=cs, stroke_width=0, fill_color=CIVIDIS[1], fill_opacity=1)
        pk_bg.next_to(l2t, RIGHT, buff=0.35)
        pk_sq = Square(side_length=cs - 0.035, color="#FFFFFF", stroke_width=2.2).move_to(pk_bg)
        pk_t = txt("local peak", 12, INK).next_to(pk_bg, RIGHT, buff=0.08)
        leg = VGroup(l1, l1b, l1t, l2, l2b, l2t, pk_bg, pk_sq, pk_t).move_to([RX, ly, 0])
        self.add(leg)
        hon = txt("The two actors do not share a landscape: each one's objective depends on the others' ties,",
                  12, GREY).move_to([RX, -3.17, 0])
        hon2 = txt("the other focal actor's included. Same portfolio axes, so both tokens are drawn on both grids.",
                   12, GREY).move_to([RX, -3.47, 0])
        self.add(hon, hon2)

        # ── captions (left, between the network and the traces) ──────────
        CAP_Y, NOTE_Y = -0.36, -0.7

        def caption(k, s=None):
            if s is None:
                msg, col = f"ministep 0 of {n_t}: the start of the run", INK
            else:
                verb = {"add": "adds", "drop": "drops"}.get(s["change"])
                what = f"{verb} component {LETTERS[s['comp'] - 1]}" if verb else "keeps its portfolio"
                msg = f"ministep {k} of {n_t}: actor {s['actor']} {what}"
                col = col_of.get(s["actor"], INK)
            return txt(msg, 16, col, "BOLD").move_to([LX, CAP_Y, 0])

        def flips(k, mover):
            """Single toggles whose sign of gain, for the OTHER focal actor at its
            current portfolio, changed with the mover's toggle at ministep k."""
            o = 1 - mover
            r0, r1 = rows[o][k - 1], rows[o][k]
            if r0 != r1:
                return []
            f0, f1 = lands[o][k - 1], lands[o][k]
            out = []
            for j in range(N):
                n = r1 ^ (1 << j)
                g0, g1 = f0[n] - f0[r1] > 1e-9, f1[n] - f1[r1] > 1e-9
                if g0 != g1:
                    out.append((j, n, g1, (r1 >> j) & 1))
            return out

        cap = caption(0)
        note = VGroup()
        hl = VGroup()
        self.add(cap, note, hl)
        self.wait(1.0)

        # ── the run, ministep by ministep ─────────────────────────────────
        for k, s in enumerate(steps, start=1):
            mover = F.index(s["actor"]) if s["actor"] in F else None
            changed = s["change"] != "none"
            rt = 0.45 if (mover is not None and changed) else (0.1 if changed else 0.05)
            anims = []
            cap.become(caption(k, s))
            i, j = s["actor"] - 1, s["comp"] - 1
            if s["change"] == "add":
                e = edge(i, j)
                edges[(i, j)] = e
                anims.append(Create(e))
            elif s["change"] == "drop":
                dropped = edges.pop((i, j))
                dropped.set_stroke(DROP, width=3.5)
                anims.append(FadeOut(dropped))
            # recolor in one frame rather than interpolating: the shifts are small
            # (one tie moves f by a fraction of the scale), and a one-frame change
            # keeps the GIF's per-frame difference to the moving tokens
            for g in range(2):
                for cfg in range(256):
                    if lands[g][k][cfg] != lands[g][k - 1][cfg]:
                        cells[g][cfg].set_fill(cividis(norm(lands[g][k][cfg])))
                pks[g].become(peak_marks(g, lands[g][k]))
            for a in range(2):
                if rows[a][k] != rows[a][k - 1]:
                    for g in range(2):
                        anims.append(tok[g][a].animate.move_to(cell_xy(g, rows[a][k])))
                pts = [[X(m), Y(f_tr[a][m]), 0] for m in range(k + 1)]
                anims.append(traces[a].animate.set_points_as_corners(pts))

            fl = flips(k, mover) if (mover is not None and changed) else []
            if fl:
                o = 1 - mover
                jj, n, now_up, held = fl[0]
                act = "dropping" if held else "adding"
                msg = (f"now {act} {LETTERS[jj]} would raise actor {F[o]}'s f" if now_up else
                       f"now {act} {LETTERS[jj]} would no longer raise actor {F[o]}'s f")
                note.become(txt(msg, 14, FOCAL_COLORS[o], "BOLD").move_to([LX, NOTE_Y, 0]))
                hl.become(VGroup(*[Square(side_length=cs + 0.02, color=FOCAL_COLORS[mover], stroke_width=4)
                                   .move_to(cell_xy(o, x[1])) for x in fl]))
            elif mover is not None and changed:
                note.become(VGroup())
                hl.become(VGroup())
            self.play(*anims, run_time=rt)
            for e in actors + comps:
                self.bring_to_front(e)
            for g in range(2):
                self.bring_to_front(pks[g], tok[g][1 - g], tok[g][g])
            self.bring_to_front(hl)
            if fl:
                self.wait(1.3)

        note.become(VGroup())
        hl.become(VGroup())
        held = [" ".join(LETTERS[b] for b in range(N) if (rows[a][-1] >> b) & 1) or "nothing"
                for a in range(2)]
        if held[0] == held[1]:
            msg = f"end of the run: actors {F[0]} and {F[1]} both hold {held[0]}"
        else:
            msg = f"end: actor {F[0]} holds {held[0]}; actor {F[1]} holds {held[1]}"
        cap.become(txt(msg, 16, INK, "BOLD").move_to([LX, CAP_Y, 0]))
        self.wait(2.0)
