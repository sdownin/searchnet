"""
Landscape views that do not depend on W having two blocks
=========================================================
Review drafts. Three right-hand panels for the README animation that work for
any influence matrix W; the left panel (network, W, objective trace) and the
ministep loop are those of ReadmeLandscapeScene. The data are the JSON files
written by tools/make_architecture_gifs.R, one per architecture (modular,
nested modules, local ring, random; N = 8).

    FittedGridScene  the 16 x 16 grid of all 256 portfolios, with the
                     row/column split of the eight components chosen from W
                     (the balanced split with the least |w| across it, over
                     all 35) and each axis in Gray-code order over a W-based
                     seriation of its four components
    CompassScene     the eight single moves open to the focal actor (add a
                     component it lacks, drop one it holds), each colored by
                     the change in f that move would produce
    WOverlayScene    W, with the focal actor's held components lit and each
                     lit cell (h and j both held) colored by its contribution
                     theta_XWX * w_hj to f; the parts of f below

Layout choices (the split, the orders) are computed here from W; every value
of f comes from the JSON.

    SEARCHNET_README_JSON=/path/ring.json \\
        manim render -r 880,520 --fps 10 scene_architecture_views.py CompassScene

or render all twelve with  Rscript tools/make_architecture_gifs.R DIR
"""

import itertools
import os
import sys

import numpy as np
from manim import (DOWN, LEFT, RIGHT, UP, Circle, Line, Rectangle, Square, VGroup,
                   interpolate_color, ManimColor)

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from scene_readme_landscape import (BLUE, GREY, INK, LETTERS, ORANGE,  # noqa: E402
                                    ReadmeLandscapeScene, cividis, colorbar, glyph,
                                    local_peaks, txt)
from scene_landscape_styles import GRAY, MID, NEG, POS, diverging  # noqa: E402

PALE_ORANGE = "#FCE9C4"


def w_split(W):
    """The balanced split (S, T) of the N = 8 components with the least total
    |w| across it, over all 35 splits (component A fixed in S; ties: first in
    lexicographic order). Returns S, T and the cross-split share of |W|."""
    A = np.abs(np.asarray(W, dtype=float))
    np.fill_diagonal(A, 0.0)
    n = A.shape[0]
    best = None
    for S in itertools.combinations(range(n), n // 2):
        if 0 not in S:
            continue
        T = tuple(j for j in range(n) if j not in S)
        cut = A[np.ix_(S, T)].sum() + A[np.ix_(T, S)].sum()
        if best is None or cut < best[0] - 1e-12:
            best = (cut, S, T)
    total = A.sum()
    return list(best[1]), list(best[2]), (best[0] / total if total > 0 else 0.0)


def seriate(comps, W):
    """Order of a few components maximizing the |w| between neighbors in the
    order (exhaustive; ties: the first permutation in lexicographic order)."""
    A = np.abs(np.asarray(W, dtype=float))
    A = A + A.T
    best = None
    for p in itertools.permutations(comps):
        s = sum(A[p[k], p[k + 1]] for k in range(len(p) - 1))
        if best is None or s > best[0] + 1e-12:
            best = (s, list(p))
    return best[1]


def held_str(c, n=8):
    return " ".join(LETTERS[b] for b in range(n) if (c >> b) & 1) or "nothing"


class _ArchLeft(ReadmeLandscapeScene):
    """Left panel relabeled for the architecture: W shaded by |w|, its name
    under W, and the module brackets kept only where W has two blocks."""

    def build_left(self):
        super().build_left()
        d = self.d
        W = np.abs(np.array(d["W"], dtype=float))
        off = W.copy()
        np.fill_diagonal(off, 0.0)
        wmax = off.max() or 1.0
        for k, sq in enumerate(self.wcells):
            r, c = divmod(k, self.N)
            if r != c:
                sq.set_fill(interpolate_color(ManimColor("#FFFFFF"), ManimColor(BLUE),
                                              float(W[r][c] / wmax)))
        lab = txt(d.get("arch_label", "influence matrix"), 12, GREY)
        self.w_sub.become(lab.move_to(self.w_sub, aligned_edge=LEFT))
        if d.get("arch") not in ("modular", "nested"):
            self.remove(self.module_marks)

    def walk_moves(self):
        """(ministep, component index, 'add'/'drop') for each focal move."""
        out = []
        for k in range(1, self.n_t + 1):
            a, b = self.focal_row[k - 1] - 1, self.focal_row[k] - 1
            if a != b:
                j = (a ^ b).bit_length() - 1
                out.append((k, j, "add" if (b >> j) & 1 else "drop"))
        return out


# ════════════════════════════════════════════════════════════════════════
# 1. W-fitted grid
# ════════════════════════════════════════════════════════════════════════
class FittedGridScene(_ArchLeft):
    CS = 0.31
    G0 = np.array([1.2, 2.5, 0])

    def build_right(self):
        focal, land, norm = self.focal, self.land, self.norm
        W = np.array(self.d["W"], dtype=float)
        S, T, share = w_split(W)
        self.rows, self.cols = seriate(S, W), seriate(T, W)
        self.right_header(f"Fitness landscape of actor {focal}",
                          "all 256 portfolios; axes split where W is weakest")
        cs, g0 = self.CS, self.G0
        ginv = {b: k for k, b in enumerate(GRAY)}

        def sub_bits(cfg, comps):
            return sum(((cfg >> j) & 1) << m for m, j in enumerate(comps))

        def cell_xy(cfg):
            r, c = ginv[sub_bits(cfg, self.rows)], ginv[sub_bits(cfg, self.cols)]
            return g0 + np.array([c * cs + cs / 2, -r * cs - cs / 2, 0])
        self.cell_xy = cell_xy

        self.cells = []
        for cfg in range(256):
            sq = Square(side_length=cs, stroke_width=0, fill_opacity=1,
                        fill_color=cividis(norm(land[0][cfg]))).move_to(cell_xy(cfg))
            self.cells.append(sq)
        self.add(*self.cells)
        for k in range(16):
            self.add(glyph(GRAY[k], True).move_to(g0 + np.array([-0.2, -k * cs - cs / 2, 0])),
                     glyph(GRAY[k], False).move_to(g0 + np.array([k * cs + cs / 2, 0.2, 0])))
        rl = " ".join(LETTERS[j] for j in self.rows)
        cl = " ".join(LETTERS[j] for j in self.cols)
        self.add(txt(f"holds which of {rl}", 13, GREY).rotate(np.pi / 2)
                 .move_to(g0 + np.array([-0.5, -8 * cs, 0])),
                 txt(f"holds which of {cl}", 13, GREY).move_to(g0 + np.array([8 * cs, 0.43, 0])))
        pct = f"{100 * share:.0f}%"
        self.add(txt(f"split from W: rows {rl.replace(' ', '')}, columns {cl.replace(' ', '')}; "
                     f"{pct} of |W| crosses it", 12, INK, "BOLD")
                 .move_to([g0[0] + 8 * cs, g0[1] - 16 * cs - 0.25, 0]),
                 txt("Gray-code axes: adjacent rows or columns differ by one component",
                     11, GREY).move_to([g0[0] + 8 * cs, g0[1] - 16 * cs - 0.53, 0]))

        self.add(*colorbar(1.55, 4.05, -3.2))
        leg_dot = Circle(radius=0.1, color=INK, stroke_width=2, fill_color=ORANGE,
                         fill_opacity=1).move_to([1.2, -3.62, 0])
        leg_dt = txt(f"actor {focal} now", 12, INK).next_to(leg_dot, RIGHT, buff=0.08)
        leg_pk = Circle(radius=0.075, color="#FFFFFF", stroke_width=2.5).move_to([3.35, -3.62, 0])
        leg_bg = Square(side_length=0.26, stroke_width=0, fill_color="#414D6B",
                        fill_opacity=1).move_to(leg_pk)
        leg_pt = txt("local peak (no single add/drop is better)", 12, INK).next_to(leg_bg, RIGHT, buff=0.08)
        self.add(leg_dot, leg_dt, leg_bg, leg_pk, leg_pt)

        self.pk = self.peak_marks(land[0])
        self.marker = Circle(radius=0.115, color=INK, stroke_width=2.5, fill_color=ORANGE,
                             fill_opacity=1).move_to(cell_xy(self.focal_row[0] - 1))
        self.add(self.pk, self.marker)


# ════════════════════════════════════════════════════════════════════════
# 2. Move compass
# ════════════════════════════════════════════════════════════════════════
class CompassScene(_ArchLeft):
    C = np.array([3.75, 0.3, 0])
    R0, R = 0.84, 1.86        # spoke from the center disk to the move node

    def deltas(self, k):
        c = self.focal_row[k] - 1
        f = self.land[k]
        return np.array([f[c ^ (1 << j)] - f[c] for j in range(self.N)])

    def angle(self, j):
        return np.pi / 2 - j * 2 * np.pi / self.N

    def unit(self, j):
        a = self.angle(j)
        return np.array([np.cos(a), np.sin(a), 0])

    def build_right(self):
        focal = self.focal
        self.dmax = max(float(np.abs(self.deltas(k)).max()) for k in range(self.n_t + 1)) or 1.0
        self.right_header(f"The moves open to actor {focal}",
                          "spokes: add or drop one component; color: change in f")
        best = int(np.argmax(self.land[-1]))
        self.moves = {k: (j, ch) for k, j, ch in self.walk_moves()}
        self.compass = self.make_compass(0)
        self.status = self.make_status(0)
        self.trail = self.make_trail(0)
        self.add(self.compass, self.status, self.trail)

        dm = self.dmax
        cb = colorbar(1.75, 3.65, -3.68, f"{-dm:.1f}", f"+{dm:.1f}",
                      color_fn=lambda u: diverging(2 * u - 1))
        self.add(*cb, txt("change in f", 11, GREY).next_to(cb[2], RIGHT, buff=0.15))
        ring = Circle(radius=0.1, color=ORANGE, stroke_width=4).move_to([5.75, -3.68, 0])
        self.add(ring, txt("move taken", 11, INK).next_to(ring, RIGHT, buff=0.07))
        self._best = best

    def make_compass(self, k, chosen=None):
        """chosen: component index of the move being taken, or -1 for 'stays'."""
        C, N = self.C, self.N
        c = self.focal_row[k] - 1
        dl = self.deltas(k)
        g = VGroup()
        for j in range(N):
            u = self.unit(j)
            col = diverging(dl[j] / self.dmax)
            mag = abs(dl[j]) / self.dmax
            g.add(Line(C + u * self.R0, C + u * (self.R - 0.2), color=col if mag > 0.05 else "#cfcfcf",
                       stroke_width=2 + 7 * mag))
        for j in range(N):
            u = self.unit(j)
            held = (c >> j) & 1
            col = diverging(dl[j] / self.dmax)
            node = Circle(radius=0.27, stroke_width=1.2, stroke_color=INK, fill_color=col,
                          fill_opacity=1).move_to(C + u * self.R)
            light = abs(dl[j]) / self.dmax < 0.55
            lab = txt(("-" if held else "+") + LETTERS[j], 13, INK if light else "#FFFFFF", "BOLD")
            val = txt(f"{dl[j]:+.1f}", 12, INK).move_to(C + u * (self.R + 0.6))
            g.add(node, lab.move_to(node), val)
            if chosen == j:
                g.add(Circle(radius=0.34, color=ORANGE, stroke_width=5).move_to(node))
                g.add(Line(C + u * self.R0, C + u * (self.R - 0.27), color=ORANGE, stroke_width=6))
        disk = Circle(radius=self.R0 - 0.04, stroke_width=3 if chosen == -1 else 2,
                      stroke_color=ORANGE, fill_color="#FFFFFF", fill_opacity=1).move_to(C)
        g.add(disk)
        if chosen == -1:
            g.add(Circle(radius=self.R0 + 0.06, color=ORANGE, stroke_width=5).move_to(C))
        ms = 0.175
        strip = VGroup()
        for j in range(N):
            h = (c >> j) & 1
            xy = C + np.array([(j - 3.5) * (ms + 0.012), -0.14, 0])
            q = Square(side_length=ms, stroke_width=1, color=BLUE,
                       fill_color=BLUE if h else "#FFFFFF", fill_opacity=1).move_to(xy)
            strip.add(q, txt(LETTERS[j], 10, "#FFFFFF" if h else GREY, "BOLD" if h else "NORMAL").move_to(xy))
        g.add(strip, txt(f"f = {self.land[k][c]:.1f}", 14, INK, "BOLD").move_to(C + np.array([0, 0.2, 0])))
        return g

    def make_status(self, k):
        dl = self.deltas(k)
        up = int((dl > 1e-9).sum())
        if up == 0:
            s = txt("local peak: no single move raises f", 14, INK, "BOLD")
        else:
            s = txt(f"{up} of {self.N} single moves raise f", 14, INK, "BOLD")
        c = self.focal_row[k] - 1
        best = int(np.argmax(self.land[k]))
        gap = self.land[k][best] - self.land[k][c]
        sub = txt(f"best of all 256 now: f = {self.land[k][best]:.1f} ({held_str(best)})"
                  + ("" if gap < 1e-9 else f", {gap:.1f} above"), 11, GREY)
        return VGroup(s, sub).arrange(DOWN, buff=0.08).move_to([3.75, -2.6, 0])

    def make_trail(self, k):
        steps = [(kk, j, ch) for kk, j, ch in self.walk_moves() if kk <= k]
        if not steps:
            body = f"path so far: starts holding {held_str(self.focal_row[0] - 1)}"
        else:
            tail = steps[-6:]
            parts = [("+" if ch == "add" else "-") + LETTERS[j] for _, j, ch in tail]
            body = "path so far: " + ("... " if len(steps) > 6 else "") + "  ".join(parts)
        return txt(body, 11, INK).move_to([3.75, -3.2, 0])

    def right_step(self, k, s):
        if s["actor"] == self.focal:
            mv = self.moves.get(k)
            self.compass.become(self.make_compass(k - 1, chosen=mv[0] if mv else -1))
            self._pending = k
        else:
            self.compass.become(self.make_compass(k))
            self.status.become(self.make_status(k))
        return []

    def step_run_time(self, s):
        if s["actor"] == self.focal:
            return 0.6
        return 0.12 if s["change"] != "none" else 0.05

    def right_front(self):
        k = getattr(self, "_pending", None)
        if k is not None:
            self.wait(0.3)
            self.compass.become(self.make_compass(k))
            self.status.become(self.make_status(k))
            self.trail.become(self.make_trail(k))
            self._pending = None
        self.bring_to_front(self.compass)

    def peak_marks(self, f):
        return VGroup()


# ════════════════════════════════════════════════════════════════════════
# 3. W overlay
# ════════════════════════════════════════════════════════════════════════
class WOverlayScene(_ArchLeft):
    CS = 0.44
    G0 = np.array([1.45, 2.78, 0])

    def build_right(self):
        focal = self.focal
        self.W = np.array(self.d["W"], dtype=float)
        self.th = self.d["theta"]
        self.parts = np.array(self.d["parts"], dtype=float)
        self.pnames = self.d["part_names"]
        off = np.abs(self.W.copy())
        np.fill_diagonal(off, 0.0)
        self.wmax = off.max() or 1.0
        self.cmax = abs(self.th["XWX"]) * self.wmax
        self.pmax = float(np.abs(self.parts).max()) or 1.0
        self.right_header(f"What makes up actor {focal}'s f",
                          "W, with the cells its held components switch on")
        self.mat = self.make_matrix(0)
        self.dec = self.make_parts(0)
        self.add(self.mat, self.dec)
        cb = colorbar(1.65, 3.4, -3.3, "0", f"+{self.cmax:.1f}",
                      color_fn=lambda u: diverging(u))
        self.add(*cb, txt("cell's part of f", 11, GREY).next_to(cb[2], RIGHT, buff=0.15))
        self.add(txt("No peaks here: this view shows the one portfolio held, not all 256.",
                     11, GREY).move_to([3.85, -3.72, 0]))

    def make_matrix(self, k):
        cs, g0, N, W = self.CS, self.G0, self.N, self.W
        c = self.focal_row[k] - 1
        held = [(c >> j) & 1 for j in range(N)]
        th = self.th["XWX"]
        g = VGroup()
        total = 0.0
        for r in range(N):
            for q in range(N):
                xy = g0 + np.array([q * cs + cs / 2, -r * cs - cs / 2, 0])
                if r == q:
                    fill = "#eeeeee"
                elif held[r] and held[q]:
                    v = th * W[r][q]
                    total += v
                    fill = diverging(v / self.cmax) if W[r][q] != 0 else "#FFFFFF"
                else:
                    base = interpolate_color(ManimColor("#FFFFFF"), ManimColor("#9ECAE1"),
                                             float(abs(W[r][q]) / self.wmax))
                    fill = interpolate_color(base, ManimColor(PALE_ORANGE), 0.55) \
                        if (held[r] or held[q]) else base
                sq = Square(side_length=cs, fill_color=fill, fill_opacity=1, color="#d0d0d0",
                            stroke_width=0.6).move_to(xy)
                g.add(sq)
                if r != q and held[r] and held[q] and W[r][q] != 0:
                    v = th * W[r][q]
                    g.add(txt(f"{v:.1f}", 10, "#FFFFFF" if abs(v) / self.cmax > 0.6 else INK, "BOLD")
                          .move_to(xy))
        for j in range(N):
            col, wt = (ORANGE, "BOLD") if held[j] else (GREY, "NORMAL")
            g.add(txt(LETTERS[j], 13, col, wt).move_to(g0 + np.array([-0.2, -j * cs - cs / 2, 0])),
                  txt(LETTERS[j], 13, col, wt).move_to(g0 + np.array([j * cs + cs / 2, 0.2, 0])))
        # outline the held block (rows and columns of held components)
        for j in range(N):
            if held[j]:
                g.add(Rectangle(width=N * cs, height=cs, stroke_color=ORANGE, stroke_width=1.6,
                                fill_opacity=0).move_to(g0 + np.array([N * cs / 2, -j * cs - cs / 2, 0])),
                      Rectangle(width=cs, height=N * cs, stroke_color=ORANGE, stroke_width=1.6,
                                fill_opacity=0).move_to(g0 + np.array([j * cs + cs / 2, -N * cs / 2, 0])))
        xi = self.pnames.index("XWX")
        assert abs(total - self.parts[k][xi]) < 1e-6, "lit cells must sum to the XWX part of f"
        # side legend
        x1 = g0[0] + N * cs + 0.25
        leg = VGroup(
            txt("cell (h, j) is lit", 11, INK, "BOLD"),
            txt(f"when actor {self.focal}", 11, INK), txt("holds both h and j", 11, INK),
            txt("number: its part", 11, INK), txt("of f, theta x w_hj", 11, INK),
            txt("pale blue: w_hj", 11, GREY), txt("not switched on", 11, GREY),
        ).arrange(DOWN, buff=0.07, aligned_edge=LEFT)
        g.add(leg.move_to([x1, g0[1] - 0.15, 0], aligned_edge=UP + LEFT))
        return g

    def make_parts(self, k):
        p = self.parts[k]
        names = {"density": "ties (density)", "inPop": "popularity", "XWX": "complementarity (lit cells)"}
        rows = VGroup()
        x0, xv, xz = 1.3, 4.35, 5.65
        scale = 1.05 / self.pmax
        y0 = y = self.G0[1] - self.N * self.CS - 0.35
        for nm, v in zip(self.pnames, p):
            lab = txt(names.get(nm, nm), 12, INK).move_to([x0, y, 0], aligned_edge=LEFT)
            val = txt(f"{v:+.1f}", 12, INK, "BOLD").move_to([xv, y, 0], aligned_edge=RIGHT)
            w = abs(v) * scale
            bar = Rectangle(width=max(w, 0.01), height=0.17, stroke_width=0,
                            fill_color=POS if v >= 0 else NEG, fill_opacity=0.85)
            bar.move_to([xz + (w / 2 if v >= 0 else -w / 2), y, 0])
            rows.add(lab, val, bar)
            y -= 0.38
        rows.add(Line([x0, y + 0.17, 0], [xv, y + 0.17, 0], color=GREY, stroke_width=1.2))
        rows.add(txt("f", 13, INK, "BOLD").move_to([x0, y - 0.04, 0], aligned_edge=LEFT),
                 txt(f"{p.sum():+.1f}", 13, ORANGE, "BOLD").move_to([xv, y - 0.04, 0], aligned_edge=RIGHT))
        rows.add(Line([xz, y0 + 0.2, 0], [xz, y + 0.2, 0], color=GREY, stroke_width=1))
        return rows

    def right_step(self, k, s):
        self.mat.become(self.make_matrix(k))
        self.dec.become(self.make_parts(k))
        return []

    def step_run_time(self, s):
        if s["actor"] == self.focal and s["change"] != "none":
            return 0.7
        return 0.12 if s["change"] != "none" else 0.05

    def peak_marks(self, f):
        return VGroup()

    def right_front(self):
        pass


