"""
All six actors, six landscapes, one portfolio space
===================================================
The "full field" companion to scene_readme_landscape.py and
scene_two_actor_landscape.py, rendered by tools/make_all_actors_gif.R (which
exports the data, calls this scene, and converts the lossless .mov to a GIF).
It reuses the README scene's text helper, color map and axis order by import.

Left: the bipartite network of one seeded saomnk_run() with every actor in
its own Okabe-Ito color (yellow left out: it is illegible on white) and its
ties drawn in that color; components and the influence matrix W in neutral
grey; below, every actor's objective at its own portfolio over the run, with
the current ministep marked.
Right: six 16 x 16 grids on the same axes (rows: which of A-D held; columns:
which of E-H; each axis ordered by subset size, empty portfolio top left),
each colored by ONE actor's own objective on one shared cividis scale. On
every grid the grid's own actor is a solid token and the other five are
hollow rings in their colors; actors sharing a portfolio split one ring into
arcs. Local peaks have a white outline. When an actor moves, its token moves
on all six grids, its own grid is framed in its color, and on every other
actor's grid the cell that became that actor's best single add or drop is
outlined in the mover's color. Landscapes are never averaged.

Every number drawn comes from the JSON written by tools/make_all_actors_gif.R.

    SEARCHNET_ALL_ACTORS_JSON=/path/all_actors_landscape.json \
        manim render -r 1440,810 --fps 10 scene_all_actors_landscape.py AllActorsLandscapeScene
"""

import json
import os
import sys

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from manim import (DOWN, LEFT, RIGHT, UP, Arc, Circle, Create, DashedLine, FadeOut,  # noqa: E402
                   Line, Rectangle, Scene, Square, VGroup, VMobject, config)
from scene_readme_landscape import (CIVIDIS, GREY, INK, LETTERS, ORDER, cividis,  # noqa: E402
                                    txt)

# ── colors: Okabe-Ito, one per actor (yellow #F0E442 omitted: illegible on
# white); components, W and axes neutral grey ──────────────────────────────
ACTOR_COLORS = ["#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7"]
# label color inside each actor circle, for contrast
ACTOR_TEXT = [INK, INK, "#FFFFFF", "#FFFFFF", "#FFFFFF", INK]
COMP = "#6b6b6b"
DROP = "#000000"

W_PX, H_PX = 1440, 810
config.background_color = "#FFFFFF"
config.frame_height = 8.0
config.frame_width = 8.0 * W_PX / H_PX


class AllActorsLandscapeScene(Scene):
    def construct(self):
        path = os.environ.get("SEARCHNET_ALL_ACTORS_JSON", "all_actors_landscape.json")
        with open(path, encoding="utf-8") as fh:
            d = json.load(fh)
        M, N = d["M"], d["N"]
        lands = [np.array(L) for L in d["landscapes"]]      # each (n_t + 1) x 2^N
        rows = [[r - 1 for r in rr] for rr in d["rows"]]   # 0-based config per state
        steps = d["steps"]
        n_t = len(steps)
        assert N == 8, "the grid layout assumes N = 8 (two 4-component axes)"
        assert M == 6, "the layout assumes six actors (a 2 x 3 block of grids)"
        # one color scale for all six grids, so equal colors mean equal f
        lo = float(min(L.min() for L in lands))
        hi = float(max(L.max() for L in lands))
        norm = lambda v: (v - lo) / (hi - lo) if hi > lo else 0.5
        self.add(Rectangle(width=config.frame_width + 0.2, height=config.frame_height + 0.2,
                           stroke_width=0, fill_color="#FFFFFF", fill_opacity=1))

        # ── left: the bipartite network ─────────────────────────────────
        LX = -4.2
        hdr_l = txt("Six actors hold components", 22, weight="BOLD").move_to([LX, 3.66, 0])
        sub_l = txt("circles: actors; squares: components (one simulated run)", 14, GREY)
        sub_l.next_to(hdr_l, DOWN, buff=0.1)
        self.add(hdr_l, sub_l)

        ax = np.linspace(-6.45, -1.95, M)
        cx = np.linspace(-6.75, -1.65, N)
        y_a, y_c, ra = 2.5, 1.02, 0.25
        actors, comps = [], []
        for i in range(M):
            c = Circle(radius=ra, color=INK, stroke_width=1.2,
                       fill_color=ACTOR_COLORS[i], fill_opacity=1).move_to([ax[i], y_a, 0])
            lab = txt(str(i + 1), 16, ACTOR_TEXT[i], "BOLD").move_to(c)
            actors.append(VGroup(c, lab))
        for j in range(N):
            s = Square(side_length=0.4, color=COMP, fill_color=COMP, fill_opacity=1, stroke_width=0)
            s.move_to([cx[j], y_c, 0])
            lab = txt(LETTERS[j], 15, "#FFFFFF", "BOLD").move_to(s)
            comps.append(VGroup(s, lab))
        mod1 = txt("module 1", 12, GREY).move_to([(cx[0] + cx[3]) / 2, y_c - 0.44, 0])
        mod2 = txt("module 2", 12, GREY).move_to([(cx[4] + cx[7]) / 2, y_c - 0.44, 0])
        brk1 = Line([cx[0] - 0.2, y_c - 0.29, 0], [cx[3] + 0.2, y_c - 0.29, 0], color=GREY, stroke_width=2)
        brk2 = Line([cx[4] - 0.2, y_c - 0.29, 0], [cx[7] + 0.2, y_c - 0.29, 0], color=GREY, stroke_width=2)

        def edge(i, j):
            return Line([ax[i], y_a - ra, 0], [cx[j], y_c + 0.2, 0],
                        color=ACTOR_COLORS[i], stroke_width=2.6)

        edges = {}
        start = np.array(d["start"])
        for i in range(M):
            for j in range(N):
                if start[i][j] == 1:
                    edges[(i, j)] = edge(i, j)
        self.add(*edges.values(), *actors, *comps, mod1, mod2, brk1, brk2)

        # ── left, bottom: W and the six objectives ────────────────────────
        W = np.array(d["W"])
        wc = 0.12
        w0 = np.array([-6.95, -1.38, 0])
        wcells = VGroup()
        for r in range(N):
            for c in range(N):
                fill = "#eeeeee" if r == c else (COMP if W[r][c] > 0 else "#FFFFFF")
                sq = Square(side_length=wc, fill_color=fill, fill_opacity=1, color="#d0d0d0", stroke_width=0.5)
                sq.move_to(w0 + np.array([c * wc + wc / 2, -r * wc - wc / 2, 0]))
                wcells.add(sq)
        w_t = txt("influence W", 12, INK, "BOLD").next_to(wcells, UP, buff=0.1).align_to(wcells, LEFT)
        w_s = txt("two modules", 11, GREY).next_to(wcells, DOWN, buff=0.08).align_to(wcells, LEFT)
        self.add(wcells, w_t, w_s)

        f_tr = [np.array([lands[a][k][rows[a][k]] for k in range(n_t + 1)]) for a in range(M)]
        tlo = float(min(t.min() for t in f_tr))
        thi = float(max(t.max() for t in f_tr))
        pad = 0.06 * (thi - tlo)
        tlo, thi = tlo - pad, thi + pad
        tx0, tx1, ty0, ty1 = -5.6, -2.3, -3.62, -1.22
        X = lambda k: tx0 + (tx1 - tx0) * k / n_t
        Y = lambda v: ty0 + (ty1 - ty0) * (v - tlo) / (thi - tlo)
        axis = VGroup(Line([tx0, ty0, 0], [tx1, ty0, 0], color=GREY, stroke_width=1.5),
                      Line([tx0, ty0, 0], [tx0, ty1, 0], color=GREY, stroke_width=1.5))
        t_t = txt("each actor's objective f at its own portfolio", 13, INK, "BOLD")
        t_t.move_to([(tx0 + tx1) / 2 + 0.3, ty1 + 0.24, 0])
        t_x = txt("ministep", 12, GREY).move_to([(tx0 + tx1) / 2, ty0 - 0.2, 0])
        self.add(axis, t_t, t_x)
        traces = []
        for a in range(M):
            tr = VMobject(color=ACTOR_COLORS[a], stroke_width=3)
            p0 = [X(0), Y(f_tr[a][0]), 0]
            tr.set_points_as_corners([p0, p0])
            traces.append(tr)
        self.add(*traces)
        now_line = DashedLine([X(0), ty0, 0], [X(0), ty1, 0], color=GREY, stroke_width=1.5,
                              dash_length=0.06)
        self.add(now_line)

        def end_labels(k):
            """Direct labels at the line ends, pushed apart to a minimum gap."""
            ys = [Y(f_tr[a][k]) for a in range(M)]
            order = sorted(range(M), key=lambda a: ys[a])
            gap = 0.19
            pos = {}
            prev = -1e9
            for a in order:
                y = max(ys[a], prev + gap)
                pos[a] = y
                prev = y
            # shift down if the stack ran off the top of the plot
            over = max(pos.values()) - (ty1 + 0.1)
            if over > 0:
                for a in pos:
                    pos[a] -= over
            g = VGroup()
            for a in range(M):
                g.add(txt(f"actor {a + 1}", 11, ACTOR_COLORS[a], "BOLD")
                      .move_to([tx1 + 0.08, pos[a], 0], aligned_edge=LEFT))
            return g

        labs = end_labels(0)
        self.add(labs)

        # ── right: six grids on the same axes ─────────────────────────────
        cs = 0.145
        gw = 16 * cs
        gx = [-0.92, 1.82, 4.56]
        gy = [3.08, 0.3]
        G0 = [np.array([gx[g % 3], gy[g // 3], 0]) for g in range(M)]   # top-left corners
        inv = {b: k for k, b in enumerate(ORDER)}
        RX = (gx[0] + gx[2] + gw) / 2

        def cell_xy(g, cfg):
            r, c = inv[cfg & 15], inv[cfg >> 4]
            return G0[g] + np.array([c * cs + cs / 2, -r * cs - cs / 2, 0])

        hdr_r = txt("Six landscapes, one portfolio space", 22, weight="BOLD").move_to([RX, 3.66, 0])
        self.add(hdr_r)
        cells = [[] for _ in range(M)]
        for g in range(M):
            t = txt(f"actor {g + 1}", 16, ACTOR_COLORS[g], "BOLD")
            t.move_to(G0[g] + np.array([gw / 2, 0.24, 0]))
            self.add(t)
            for cfg in range(256):
                # a hairline stroke in the fill color hides antialiasing seams
                c0 = cividis(norm(lands[g][0][cfg]))
                sq = Square(side_length=cs, stroke_width=0.6, stroke_color=c0,
                            fill_opacity=1, fill_color=c0)
                sq.move_to(cell_xy(g, cfg))
                cells[g].append(sq)
            self.add(*cells[g])

        # peaks: no single add or drop raises f (a white square outline)
        def peaks(f):
            return [cfg for cfg in range(256) if all(f[cfg] >= f[cfg ^ (1 << j)] for j in range(N))]

        def peak_marks(g, f):
            return VGroup(*[Square(side_length=cs - 0.03, color="#FFFFFF", stroke_width=1.6)
                            .move_to(cell_xy(g, p)) for p in peaks(f)])

        pks = [peak_marks(g, lands[g][0]) for g in range(M)]
        self.add(*pks)

        # tokens. On grid g: actor g a solid disc; every other actor a hollow
        # ring; several actors on one cell share one ring split into arcs.
        R_RING, R_DISC = 0.088, 0.05

        def disc(a, r=R_DISC):
            return Circle(radius=r, color=INK, stroke_width=1.6,
                          fill_color=ACTOR_COLORS[a], fill_opacity=1)

        def ring(ids, r=R_RING, w=3.2):
            g = VGroup(Circle(radius=r, color=INK, stroke_width=w + 2.6))
            n = len(ids)
            for k, a in enumerate(ids):
                g.add(Arc(radius=r, start_angle=np.pi / 2 - 2 * np.pi * k / n,
                          angle=-2 * np.pi / n, color=ACTOR_COLORS[a], stroke_width=w))
            return g

        def layout(g, pos, skip=None):
            """pos[a] = config of actor a; returns the tokens on grid g."""
            out = VGroup()
            by_cell = {}
            for a in range(M):
                if a == g or a == skip:
                    continue
                by_cell.setdefault(pos[a], []).append(a)
            for cfg, ids in by_cell.items():
                out.add(ring(ids).move_to(cell_xy(g, cfg)))
            if g != skip:
                out.add(disc(g).move_to(cell_xy(g, pos[g])))
            return out

        pos = [rows[a][0] for a in range(M)]
        toks = [layout(g, pos) for g in range(M)]
        self.add(*toks)

        # color bar and legend, under the grids
        cb = VGroup()
        nseg = 40
        cb_x0, cb_x1, cb_y = RX - 1.9, RX + 1.3, -2.3
        for s in range(nseg):
            seg = Rectangle(width=(cb_x1 - cb_x0) / nseg + 0.005, height=0.13, stroke_width=0,
                            fill_color=cividis(s / (nseg - 1)), fill_opacity=1)
            seg.move_to([cb_x0 + (s + 0.5) * (cb_x1 - cb_x0) / nseg, cb_y, 0])
            cb.add(seg)
        cb_lo = txt(f"lower f ({lo:.1f})", 12, GREY).next_to(cb, LEFT, buff=0.1)
        cb_hi = txt(f"higher f ({hi:.1f}), one scale, all grids", 12, GREY).next_to(cb, RIGHT, buff=0.1)
        self.add(cb, cb_lo, cb_hi)

        l1 = disc(0, 0.07).move_to([0, 0, 0])
        l1t = txt("the grid's own actor", 12, INK).next_to(l1, RIGHT, buff=0.08)
        l2 = ring([1]).next_to(l1t, RIGHT, buff=0.3)
        l2t = txt("another actor", 12, INK).next_to(l2, RIGHT, buff=0.08)
        l3 = ring([2, 4, 5]).next_to(l2t, RIGHT, buff=0.3)
        l3t = txt("actors sharing a portfolio", 12, INK).next_to(l3, RIGHT, buff=0.08)
        pk_bg = Square(side_length=cs, stroke_width=0, fill_color=CIVIDIS[1], fill_opacity=1)
        pk_bg.next_to(l3t, RIGHT, buff=0.3)
        pk_sq = Square(side_length=cs - 0.03, color="#FFFFFF", stroke_width=1.6).move_to(pk_bg)
        pk_t = txt("local peak", 12, INK).next_to(pk_bg, RIGHT, buff=0.08)
        leg = VGroup(l1, l1t, l2, l2t, l3, l3t, pk_bg, pk_sq, pk_t).move_to([RX, -2.7, 0])
        self.add(leg)
        hon = [
            "The six landscapes differ because each actor's objective depends on the others' ties.",
            "Same portfolio axes on every grid: rows = which of A B C D held, columns = which of E F G H,",
            "each ordered by how many are held (empty portfolio top left, all eight bottom right).",
        ]
        for k, s in enumerate(hon):
            self.add(txt(s, 12, GREY).move_to([RX, -3.12 - 0.27 * k, 0]))

        # ── captions (left, between the network and the traces) ──────────
        CAP_Y, NOTE_Y = 0.12, -0.2

        def caption(k, s=None):
            if s is None:
                msg, col = f"ministep 0 of {n_t}: the start of the run", INK
            else:
                verb = {"add": "adds", "drop": "drops"}.get(s["change"])
                what = f"{verb} component {LETTERS[s['comp'] - 1]}" if verb else "keeps its portfolio"
                msg = f"ministep {k} of {n_t}: actor {s['actor']} {what}"
                col = ACTOR_COLORS[s["actor"] - 1]
            return txt(msg, 16, col, "BOLD").move_to([LX, CAP_Y, 0])

        def best(f, r):
            g = np.array([f[r ^ (1 << j)] for j in range(N)])
            return frozenset(int(j) for j in np.flatnonzero(g >= g.max() - 1e-9))

        def best_changes(k, mover):
            """Other actors whose best single add/drop (from where they stand)
            changed with the mover's toggle at ministep k: [(actor, new cell)]."""
            out = []
            for o in range(M):
                if o == mover or rows[o][k] != rows[o][k - 1]:
                    continue
                r = rows[o][k]
                b0, b1 = best(lands[o][k - 1], r), best(lands[o][k], r)
                if b0 != b1:
                    out.append((o, [r ^ (1 << j) for j in sorted(b1)]))
            return out

        cap = caption(0)
        self.add(cap)
        note = None
        hl = None
        frame = None
        self.wait(1.2)

        n_hl = 0
        # ── the run, ministep by ministep ─────────────────────────────────
        for k, s in enumerate(steps, start=1):
            mover = s["actor"] - 1
            changed = s["change"] != "none"
            self.remove(cap)
            cap = caption(k, s)
            self.add(cap)
            for m in (note, hl, frame):
                if m is not None:
                    self.remove(m)
            note = hl = frame = None
            # traces, labels and the current-ministep line
            for a in range(M):
                traces[a].set_points_as_corners([[X(m), Y(f_tr[a][m]), 0] for m in range(k + 1)])
            self.remove(labs)
            labs = end_labels(k)
            self.add(labs)
            now_line.put_start_and_end_on([X(k), ty0, 0], [X(k), ty1, 0])
            if not changed:
                self.wait(0.1)
                continue

            i, j = mover, s["comp"] - 1
            anims = []
            if s["change"] == "add":
                e = edge(i, j)
                edges[(i, j)] = e
                anims.append(Create(e))
            else:
                dropped = edges.pop((i, j))
                dropped.set_stroke(DROP, width=3.5)
                anims.append(FadeOut(dropped))
            # recolor in one frame rather than interpolating (see the two-actor scene)
            for g in range(M):
                for cfg in range(256):
                    if lands[g][k][cfg] != lands[g][k - 1][cfg]:
                        c1 = cividis(norm(lands[g][k][cfg]))
                        cells[g][cfg].set_fill(c1).set_stroke(c1)
                self.remove(pks[g])
                pks[g] = peak_marks(g, lands[g][k])
                self.add(pks[g])
            # the mover: its own grid framed, its token moving on all six grids
            frame = Rectangle(width=gw + 0.08, height=gw + 0.08, color=ACTOR_COLORS[mover],
                              stroke_width=5).move_to(G0[mover] + np.array([gw / 2, -gw / 2, 0]))
            self.add(frame)
            old, new = rows[mover][k - 1], rows[mover][k]
            movers = []
            for g in range(M):
                self.remove(toks[g])
                toks[g] = layout(g, pos, skip=mover)
                self.add(toks[g])
                m = (disc(mover, R_DISC * 1.35) if g == mover else ring([mover], R_RING * 1.2, 4))
                m.move_to(cell_xy(g, old))
                self.add(m)
                movers.append(m)
                anims.append(m.animate.move_to(cell_xy(g, new)))
            pos[mover] = new
            # whose best single add/drop the move changed
            bc = best_changes(k, mover)
            if bc:
                n_hl += 1
                hl = VGroup(*[Square(side_length=cs + 0.035, color=ACTOR_COLORS[mover], stroke_width=3.4)
                              .move_to(cell_xy(o, c)) for o, cl in bc for c in cl])
                who = [str(o + 1) for o, _ in bc]
                whos = who[0] if len(who) == 1 else ", ".join(who[:-1]) + " and " + who[-1]
                act = "actor" if len(who) == 1 else "actors"
                note = txt(f"this changes the best single add or drop for {act} {whos} (outlined)",
                           13, INK).move_to([LX, NOTE_Y, 0])
                self.add(note)
            self.play(*anims, run_time=0.3)
            for g in range(M):
                self.remove(movers[g], toks[g])
                toks[g] = layout(g, pos)
                self.add(toks[g])
            for e in actors + comps:
                self.bring_to_front(e)
            if hl is not None:
                self.add(hl)
                self.wait(0.4)
            else:
                self.wait(0.1)

        for m in (note, hl, frame):
            if m is not None:
                self.remove(m)
        self.remove(cap)
        self.add(txt(f"end of the run ({n_t} ministeps)", 16, INK, "BOLD").move_to([LX, CAP_Y, 0]))
        self.wait(2.2)
        print(f"[scene] ministeps with a best-move outline: {n_hl}")
