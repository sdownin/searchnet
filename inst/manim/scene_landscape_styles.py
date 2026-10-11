"""
Alternative representations of the README landscape panel
=========================================================
Four scenes that keep the left panel of the README animation (network, W,
objective trace) and the ministep loop of ReadmeLandscapeScene, and replace
only the right-hand panel, the focal actor's objective f over all 256
portfolios. They read the same JSON (tools/make_readme_gif.R); local peaks and
the grid orders below are derived from that landscape, nothing is recomputed
from the model.

    HypercubeScene   the 8-cube: one node per portfolio, one edge per add/drop
    TerrainScene     Gray-code grid as bars, height and color = f, slow orbit
    ContourScene     contour map (Gray-code grid) with the actor's walk; at the
                     end it switches to the change in f since the start
    DifferenceScene  color = change in f since the previous ministep, with an
                     inset of the current landscape

A 2-D layout of an 8-bit portfolio space cannot keep every single-move pair
adjacent; each scene's caption states what adjacency means in its layout.

    SEARCHNET_README_JSON=/path/readme_landscape.json \\
        manim render -r 880,520 --fps 10 scene_landscape_styles.py HypercubeScene

or render all four GIFs with  Rscript tools/make_readme_gif.R --styles=DIR
"""

import os
import sys

import numpy as np
from manim import (DOWN, LEFT, RIGHT, UP, Circle, Create, Dot, FadeIn, FadeOut,
                   Line, Rectangle, Square, ValueTracker, VGroup, VMobject, always_redraw,
                   interpolate_color, linear, ManimColor)

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from scene_readme_landscape import (BLUE, CIVIDIS, GREY, INK, LETTERS, ORANGE, ORDER,  # noqa: E402
                                    ReadmeLandscapeScene, cividis, colorbar, glyph,
                                    local_peaks, txt)

# 4-bit reflected Gray code: consecutive entries differ in exactly one component
GRAY = [0, 1, 3, 2, 6, 7, 5, 4, 12, 13, 15, 14, 10, 11, 9, 8]
GRAY_INV = {b: k for k, b in enumerate(GRAY)}
# diverging palette for changes in f (ColorBrewer RdBu ends, neutral grey middle)
NEG, MID, POS = "#2166AC", "#F2F2F2", "#B2182B"
PEAK_NAMES = {0: "none", 15: "A-D", 240: "E-H", 255: "all"}


def diverging(u):
    """u in [-1, 1] -> blue / grey / red."""
    u = float(np.clip(u, -1.0, 1.0))
    if u < 0:
        return interpolate_color(ManimColor(MID), ManimColor(NEG), -u)
    return interpolate_color(ManimColor(MID), ManimColor(POS), u)


def overlap(a, b):
    """do the bounding boxes of two mobjects intersect?"""
    return not (a.get_right()[0] < b.get_left()[0] or b.get_right()[0] < a.get_left()[0] or
                a.get_top()[1] < b.get_bottom()[1] or b.get_top()[1] < a.get_bottom()[1])


def popcount(x):
    return bin(x).count("1")


def note(lines, y, size=12):
    """A grey multi-line note centered under the right panel."""
    g = VGroup(*[txt(s, size, GREY) for s in lines]).arrange(DOWN, buff=0.07)
    return g.move_to([3.65, y, 0])


class _StyleBase(ReadmeLandscapeScene):
    """Shared bits for the styles: the focal actor's walk."""

    def walk(self):
        """(ministep, portfolio) at the start and at every focal move."""
        out = [(0, self.focal_row[0] - 1)]
        for k in range(1, self.n_t + 1):
            if self.focal_row[k] != self.focal_row[k - 1]:
                out.append((k, self.focal_row[k] - 1))
        return out


# ════════════════════════════════════════════════════════════════════════
# 1. Hypercube graph
# ════════════════════════════════════════════════════════════════════════
class HypercubeScene(_StyleBase):
    def build_right(self):
        focal, land = self.focal, self.land
        self.right_header(f"Portfolio space of actor {focal}",
                          "one node per portfolio; one line per single add or drop")
        dx, sp, x0, yc = 0.66, 0.08, 1.05, -0.12
        layers = {n: sorted([c for c in range(256) if popcount(c) == n],
                            key=lambda c: (-popcount(c & 15), GRAY_INV[c & 15], GRAY_INV[c >> 4]))
                  for n in range(9)}
        pos = self.pos = {}
        for n, members in layers.items():
            m = len(members)
            for idx, c in enumerate(members):
                pos[c] = np.array([x0 + n * dx, yc + ((m - 1) / 2 - idx) * sp, 0])

        edges = VGroup()
        for c in range(256):
            for j in range(8):
                if not (c >> j) & 1:
                    edges.add(Line(pos[c], pos[c | (1 << j)], color="#d4d4d4", stroke_width=0.5))
        self.add(edges)
        self.nbr = self.neighbor_edges(self.focal_row[0] - 1)
        self.trail = VGroup()
        self.add(self.nbr, self.trail)

        self.nodes = []
        for c in range(256):
            dot = Circle(radius=0.038, stroke_width=0, fill_opacity=1,
                         fill_color=cividis(self.norm(land[0][c]))).move_to(pos[c])
            self.nodes.append(dot)
        self.add(*self.nodes)

        # anchors: the four corners of the two-module structure
        offs = {0: LEFT, 255: RIGHT, 15: LEFT, 240: LEFT}
        for c, name in PEAK_NAMES.items():
            lab = txt(name + (" held" if c in (15, 240) else ""), 11, INK)
            lab.next_to(pos[c], offs[c], buff=0.13)
            self.add(lab)
        for n in range(9):
            self.add(txt(str(n), 12, GREY).move_to([x0 + n * dx, -3.08, 0]))
        self.add(txt("components held", 12, GREY).move_to([x0 + 4 * dx, -3.33, 0]))
        side = txt("within a column: more of A-D on top, more of E-H below", 11, GREY)
        side.move_to([x0 + 4 * dx, 2.95, 0])
        self.add(side)

        self.add(*colorbar(1.55, 2.95, -3.72))
        leg_dot = Circle(radius=0.08, color=INK, stroke_width=2, fill_color=ORANGE,
                         fill_opacity=1).move_to([4.42, -3.72, 0])
        leg_dt = txt(f"actor {focal}", 11, INK).next_to(leg_dot, RIGHT, buff=0.06)
        leg_pk = Circle(radius=0.07, color=INK, stroke_width=2).move_to([5.55, -3.72, 0])
        leg_pt = txt("local peak", 11, INK).next_to(leg_pk, RIGHT, buff=0.06)
        self.add(leg_dot, leg_dt, leg_pk, leg_pt)

        self.pk = self.peak_marks(land[0])
        self.marker = Circle(radius=0.085, color=INK, stroke_width=2.2, fill_color=ORANGE,
                             fill_opacity=1).move_to(pos[self.focal_row[0] - 1])
        self.add(self.pk, self.marker)

    def neighbor_edges(self, c):
        return VGroup(*[Line(self.pos[c], self.pos[c ^ (1 << j)], color=ORANGE, stroke_width=1.6,
                             stroke_opacity=0.75) for j in range(8)])

    def peak_marks(self, f):
        return VGroup(*[Circle(radius=0.075, color=INK, stroke_width=2).move_to(self.pos[p])
                        for p in local_peaks(f, self.N)])

    def right_step(self, k, s):
        land, anims = self.land, []
        for c in range(256):
            if land[k][c] != land[k - 1][c]:
                anims.append(self.nodes[c].animate.set_fill(cividis(self.norm(land[k][c]))))
        a, b = self.focal_row[k - 1] - 1, self.focal_row[k] - 1
        if a != b:
            for age, seg in enumerate(reversed(self.trail.submobjects)):
                seg.set_stroke(opacity=max(0.15, 0.8 * 0.7 ** (age + 1)))
            seg = Line(self.pos[a], self.pos[b], color=ORANGE, stroke_width=5, stroke_opacity=0.9)
            self.trail.add(seg)
            anims.append(Create(seg))
            anims.append(self.marker.animate.move_to(self.pos[b]))
            self.nbr.become(self.neighbor_edges(b))
        self.pk.become(self.peak_marks(land[k]))
        return anims


# ════════════════════════════════════════════════════════════════════════
# 2. Terrain: Gray-code grid as bars, slow orbit
# ════════════════════════════════════════════════════════════════════════
class TerrainScene(_StyleBase):
    U = 0.25             # cell size on the ground
    HMAX = 3.0           # height of the highest bar
    ELEV = np.radians(40)
    AZ_IN, AZ0, AZ1 = np.radians(8), np.radians(30), np.radians(75)
    CENTER = np.array([3.65, -0.8, 0])

    def proj(self, X, Y, z, az):
        """ground (X, Y) and height z -> screen point, plus depth (larger = farther)."""
        ca, sa = np.cos(az), np.sin(az)
        xr, yr = X * ca - Y * sa, X * sa + Y * ca
        return (self.CENTER + np.array([xr, yr * np.sin(self.ELEV) + z * np.cos(self.ELEV), 0]), yr)

    def cur_f(self):
        t = self.tk.get_value()
        k0 = int(np.floor(t)); k1 = min(k0 + 1, self.n_t); a = t - k0
        return (1 - a) * self.land[k0] + a * self.land[k1]

    def ground(self, c):
        """grid position of portfolio c: rows A-D, columns E-H, both Gray-ordered."""
        r, q = GRAY_INV[c & 15], GRAY_INV[c >> 4]
        return (q - 7.5) * self.U, (7.5 - r) * self.U

    def terrain(self):
        f, az, u, h2 = self.cur_f(), self.az.get_value(), self.U / 2, None
        peaks = set(local_peaks(f, self.N))
        ca, sa = np.cos(az), np.sin(az)
        # faces whose outward normal points toward the viewer
        normals = [((1, 0), sa), ((-1, 0), -sa), ((0, 1), ca), ((0, -1), -ca)]
        vis = [n for n, ny in normals if ny < 0]
        order = sorted(range(256), key=lambda c: -(self.ground(c)[0] * sa + self.ground(c)[1] * ca))
        g = VGroup()
        for c in order:
            X, Y = self.ground(c)
            h = 0.03 + self.HMAX * self.norm(f[c])
            top = cividis(self.norm(f[c]))
            for (nx, ny) in vis:
                if nx:
                    a, b = (X + nx * u, Y - u), (X + nx * u, Y + u)
                else:
                    a, b = (X - u, Y + ny * u), (X + u, Y + ny * u)
                pts = [self.proj(*a, 0, az)[0], self.proj(*b, 0, az)[0],
                       self.proj(*b, h, az)[0], self.proj(*a, h, az)[0]]
                shade = 0.35 if nx else 0.55
                side = interpolate_color(top, ManimColor("#000000"), shade)
                g.add(VMobject(stroke_width=0.5, stroke_color=side, fill_opacity=1, fill_color=side)
                      .set_points_as_corners(pts + [pts[0]]))
            pts = [self.proj(X + sx * u, Y + sy * u, h, az)[0]
                   for sx, sy in ((-1, -1), (1, -1), (1, 1), (-1, 1))]
            g.add(VMobject(stroke_width=0.3, stroke_color="#00000040", fill_opacity=1,
                           fill_color=top).set_points_as_corners(pts + [pts[0]]))
            if c in peaks:
                ring = Circle(radius=0.075, color="#FFFFFF", stroke_width=2.2)
                ring.stretch(np.sin(self.ELEV), 1).move_to(self.proj(X, Y, h, az)[0])
                g.add(ring)
        return g

    def bar_top(self, c):
        X, Y = self.ground(c)
        f = self.cur_f()
        return self.proj(X, Y, 0.03 + self.HMAX * self.norm(f[c]), self.az.get_value())[0]

    def build_right(self):
        focal = self.focal
        self.right_header(f"Fitness landscape of actor {focal}",
                          "height and color: its objective f, all 256 portfolios")
        self.tk = ValueTracker(0.0)
        self.az = ValueTracker(self.AZ_IN)
        self.add(self.tk, self.az)
        self.add(always_redraw(self.terrain))

        # floating labels: the four corners, and the two axes
        for c, name in PEAK_NAMES.items():
            word = txt(name, 11, INK, "BOLD")
            lab = VGroup(Rectangle(width=word.width + 0.08, height=word.height + 0.07, stroke_width=0,
                                   fill_color="#FFFFFF", fill_opacity=0.85).move_to(word), word)
            lab.add_updater(lambda m, c=c: m.move_to(self.bar_top(c) + np.array([-0.3, 0.14, 0])))
            self.add(lab)
        u8 = 8.5 * self.U
        ax_r = txt("rows: A-D", 11, GREY)
        ax_c = txt("columns: E-H", 11, GREY)
        ax_r.add_updater(lambda m: m.move_to(self.proj(-u8 - 0.35, 0, 0, self.az.get_value())[0]))
        ax_c.add_updater(lambda m: m.move_to(self.proj(0, -u8 - 0.3, 0, self.az.get_value())[0]))
        self.add(ax_r, ax_c)

        stem = always_redraw(lambda: Line(self.bar_top(self.cur_c()), self.bar_top(self.cur_c()) + np.array([0, 0.42, 0]),
                                          color=INK, stroke_width=2))
        self.marker = always_redraw(lambda: Circle(radius=0.1, color=INK, stroke_width=2.2, fill_color=ORANGE,
                                                   fill_opacity=1).move_to(self.bar_top(self.cur_c()) + np.array([0, 0.48, 0])))
        self.add(stem, self.marker)
        self.add(note(["Gray-code rows and columns: neighboring bars differ by one add",
                       "or drop, but not every single move is a neighboring bar"], -3.3, 11))
        self.add(*colorbar(1.75, 3.55, -3.76))
        leg_dot = Circle(radius=0.08, color=INK, stroke_width=2, fill_color=ORANGE,
                         fill_opacity=1).move_to([4.6, -3.76, 0])
        self.add(leg_dot, txt(f"actor {focal} now", 11, INK).next_to(leg_dot, RIGHT, buff=0.06))
        self.pk = VGroup()

    def cur_c(self):
        return self.focal_row[int(round(self.tk.get_value()))] - 1

    def right_step(self, k, s):
        # the camera holds still during the run (a moving camera changes every
        # pixel of every frame and triples the GIF); it orbits in the intro and
        # the finale instead
        return [self.tk.animate(rate_func=linear).set_value(k)]

    def intro(self):
        self.play(self.az.animate.set_value(self.AZ0), run_time=1.2)

    def right_front(self):
        pass

    def finale(self):
        self.play(self.az.animate.set_value(self.AZ1), run_time=2.0)
        self.wait(0.4)


# ════════════════════════════════════════════════════════════════════════
# 3. Terraced contour map with the walk, then the change since the start
# ════════════════════════════════════════════════════════════════════════
class ContourScene(_StyleBase):
    """Contours drawn on cell edges (no interpolation between portfolios):
    each cell is colored by its band of f, and a line separates two
    neighboring cells in different bands."""
    CS = 0.33
    G0 = np.array([1.1, 2.48, 0])
    NB = 10                          # bands of f

    def rc(self, c):
        return GRAY_INV[c & 15], GRAY_INV[c >> 4]

    def cell_xy(self, c):
        r, q = self.rc(c)
        return self.G0 + np.array([q * self.CS + self.CS / 2, -r * self.CS - self.CS / 2, 0])

    def bands(self, v, lo, hi, nb):
        return np.clip(np.floor((np.asarray(v) - lo) / (hi - lo) * nb), 0, nb - 1).astype(int)

    def contour_lines(self, band, width=1.4, color="#FFFFFF"):
        """segments on the cell edges between neighbors in different bands."""
        z = np.empty((16, 16), dtype=int)
        for c in range(256):
            z[self.rc(c)] = band[c]
        cs, g = self.CS, self.G0
        segs = VGroup()
        for r in range(16):
            for q in range(16):
                if q < 15 and z[r, q] != z[r, q + 1]:
                    x = g[0] + (q + 1) * cs
                    segs.add(Line([x, g[1] - r * cs, 0], [x, g[1] - (r + 1) * cs, 0],
                                  color=color, stroke_width=width))
                if r < 15 and z[r, q] != z[r + 1, q]:
                    y = g[1] - (r + 1) * cs
                    segs.add(Line([g[0] + q * cs, y, 0], [g[0] + (q + 1) * cs, y, 0],
                                  color=color, stroke_width=width))
        return segs

    def f_colors(self, k):
        b = self.bands(self.land[k], self.lo, self.hi, self.NB)
        return b, [cividis((x + 0.5) / self.NB) for x in b]

    def build_right(self):
        focal = self.focal
        self.hdr = self.right_header(f"Fitness landscape of actor {focal}",
                                     "contour map of its objective f over the 256 portfolios")
        band, cols = self.f_colors(0)
        self.cells = []
        for c in range(256):
            sq = Square(side_length=self.CS, stroke_width=0.8, stroke_color=cols[c], fill_opacity=1,
                        fill_color=cols[c])
            self.cells.append(sq.move_to(self.cell_xy(c)))
        self.add(*self.cells)
        self.lines = self.contour_lines(band)
        self.add(self.lines)
        for k in range(16):
            self.add(glyph(GRAY[k], True).move_to(self.G0 + np.array([-0.2, -k * self.CS - self.CS / 2, 0])),
                     glyph(GRAY[k], False).move_to(self.G0 + np.array([k * self.CS + self.CS / 2, 0.2, 0])))
        self.add(txt("holds which of A B C D (Gray-code order)", 12, GREY).rotate(np.pi / 2)
                 .move_to(self.G0 + np.array([-0.5, -8 * self.CS, 0])),
                 txt("holds which of E F G H (Gray-code order)", 12, GREY)
                 .move_to(self.G0 + np.array([8 * self.CS, 0.43, 0])))
        self.legend = VGroup(*colorbar(1.55, 4.05, -3.12,
                                       color_fn=lambda u: cividis((np.floor(u * self.NB * 0.999) + 0.5) / self.NB)))
        self.add(self.legend)
        self.add(note(["each cell is a portfolio; lines separate bands of f",
                       "neighbors differ by one add or drop; not every move is a neighbor",
                       "orange: the actor's walk, numbered by ministep of arrival"], -3.62, 11))
        p0 = self.cell_xy(self.focal_row[0] - 1)
        self.trail = VMobject(color=ORANGE, stroke_width=4)
        self.trail.set_points_as_corners([p0, p0])
        self.ticks = VGroup()
        self.tick_text, self.tick_lab = {}, {}
        self.add(self.trail, self.ticks)
        self.add_tick(0, self.focal_row[0] - 1)
        self.pk = self.peak_marks(self.land[0])
        self.marker = Circle(radius=0.12, color=INK, stroke_width=2.5, fill_color=ORANGE,
                             fill_opacity=1).move_to(p0)
        self.add(self.pk, self.marker)
        self.walk_pts = [p0]

    def add_tick(self, k, c):
        """a dot at each portfolio the walk reaches, labeled with the ministep(s) it arrived."""
        self.tick_text.setdefault(c, []).append(str(k))
        p = self.cell_xy(c)
        if c in self.tick_lab:
            self.ticks.remove(self.tick_lab[c])
        others = [v for kk, v in self.tick_lab.items() if kk != c]
        for direction in (UP + RIGHT, DOWN + RIGHT, UP + LEFT, DOWN + LEFT):
            lab = txt(", ".join(self.tick_text[c]), 10, INK, "BOLD")
            lab.next_to(p, direction, buff=0.1)
            bg = Rectangle(width=lab.width + 0.07, height=lab.height + 0.06, stroke_width=0,
                           fill_color="#FFFFFF", fill_opacity=0.85).move_to(lab)
            if not any(overlap(bg, o) for o in others):
                break
        self.tick_lab[c] = VGroup(bg, lab)
        self.ticks.add(Dot(p, radius=0.045, color=INK), self.tick_lab[c])

    def peak_marks(self, f):
        return VGroup(*[Circle(radius=0.075, color="#FFFFFF", stroke_width=2.5).move_to(self.cell_xy(p))
                        for p in local_peaks(f, self.N)])

    def right_step(self, k, s):
        anims = []
        if np.any(self.land[k] != self.land[k - 1]):
            band, cols = self.f_colors(k)
            for c in range(256):
                self.cells[c].set_fill(cols[c]).set_stroke(cols[c])
            self.lines.become(self.contour_lines(band))
        a, b = self.focal_row[k - 1] - 1, self.focal_row[k] - 1
        if a != b:
            self.walk_pts.append(self.cell_xy(b))
            anims.append(self.trail.animate.set_points_as_corners(self.walk_pts))
            anims.append(self.marker.animate.move_to(self.cell_xy(b)))
            self.add_tick(k, b)
        self.pk.become(self.peak_marks(self.land[k]))
        return anims

    def right_front(self):
        self.bring_to_front(self.trail, self.ticks, self.pk, self.marker)

    def finale(self):
        """hold the final map, then switch to the change in f since ministep 0."""
        self.wait(1.0)
        d = self.land[-1] - self.land[0]
        m = float(np.abs(d).max()) or 1.0
        nb = 9                                           # odd: a band centered on zero
        band = self.bands(d, -m, m, nb)
        cols = [diverging((2 * (x + 0.5) / nb) - 1) for x in band]
        hdr = txt("Change since the start", 22, weight="BOLD").move_to(self.hdr[0])
        sub = txt("f at the end minus f at ministep 0, same map", 15, GREY).move_to(self.hdr[1])
        leg = VGroup(*colorbar(1.55, 4.05, -3.12, f"{-m:+.1f}", f"{m:+.1f}",
                               color_fn=lambda u: diverging(2 * (np.floor(u * nb * 0.999) + 0.5) / nb - 1)))
        anims = [self.cells[c].animate.set_fill(cols[c]).set_stroke(cols[c]) for c in range(256)]
        self.play(*anims, self.lines.animate.become(self.contour_lines(band, color=INK)),
                  FadeOut(self.hdr[0]), FadeOut(self.hdr[1]), FadeIn(hdr), FadeIn(sub),
                  FadeOut(self.legend), FadeIn(leg), FadeOut(self.pk), run_time=0.6)
        self.bring_to_front(self.trail, self.ticks, self.marker)
        self.wait(2.4)


# ════════════════════════════════════════════════════════════════════════
# 4. Difference overlay: change since the previous ministep, plus an inset
# ════════════════════════════════════════════════════════════════════════
class DifferenceScene(_StyleBase):
    CS = 0.27
    G0 = np.array([1.12, 2.45, 0])
    ICS = 0.09
    IG0 = np.array([5.12, -2.2, 0])

    def xy(self, c, g0, cs):
        inv = {b: k for k, b in enumerate(ORDER)}
        r, q = inv[c & 15], inv[c >> 4]
        return g0 + np.array([q * cs + cs / 2, -r * cs - cs / 2, 0])

    def delta(self, k):
        return np.zeros(256) if k == 0 else self.land[k] - self.land[k - 1]

    def what_changed(self, k):
        """a data-derived summary of which portfolios moved at ministep k."""
        d = self.delta(k)
        nz = set(np.nonzero(d)[0].tolist())
        if not nz:
            return txt("no portfolio changed", 12, GREY, "BOLD")
        for b in range(self.N):
            if nz == {c for c in range(256) if (c >> b) & 1} and len(set(np.round(d[list(nz)], 9))) == 1:
                v = d[next(iter(nz))]
                return txt(f"every portfolio holding {LETTERS[b]}: {v:+.1f}", 12,
                           POS if v > 0 else NEG, "BOLD")
        return txt(f"{len(nz)} portfolios changed", 12, INK, "BOLD")

    def build_right(self):
        focal, land = self.focal, self.land
        self.dmax = float(np.abs(np.diff(land, axis=0)).max()) or 1.0
        self.right_header(f"Change in actor {focal}'s landscape",
                          "f now minus f one ministep earlier, for all 256 portfolios")
        self.cells = []
        for c in range(256):
            sq = Square(side_length=self.CS, stroke_width=0.6, stroke_color=MID, fill_opacity=1,
                        fill_color=MID).move_to(self.xy(c, self.G0, self.CS))
            self.cells.append(sq)
        self.add(*self.cells)
        for k in range(16):
            self.add(glyph(ORDER[k], True).move_to(self.G0 + np.array([-0.2, -k * self.CS - self.CS / 2, 0])),
                     glyph(ORDER[k], False).move_to(self.G0 + np.array([k * self.CS + self.CS / 2, 0.2, 0])))
        self.add(txt("holds which of A B C D", 12, GREY).rotate(np.pi / 2)
                 .move_to(self.G0 + np.array([-0.5, -8 * self.CS, 0])),
                 txt("holds which of E F G H", 12, GREY).move_to(self.G0 + np.array([8 * self.CS, 0.5, 0])))

        # inset: the current landscape on the same grid
        self.icells = []
        for c in range(256):
            sq = Square(side_length=self.ICS, stroke_width=0, fill_opacity=1,
                        fill_color=cividis(self.norm(land[0][c]))).move_to(self.xy(c, self.IG0, self.ICS))
            self.icells.append(sq)
        self.add(*self.icells)
        self.add(txt("f now", 11, INK, "BOLD").next_to(self.icells[0], UP, buff=0.08)
                 .align_to(self.icells[0], LEFT))
        self.imarker = Circle(radius=0.055, color=INK, stroke_width=1.5, fill_color=ORANGE,
                              fill_opacity=1).move_to(self.xy(self.focal_row[0] - 1, self.IG0, self.ICS))
        self.add(self.imarker)

        x0 = 1.0
        self.changed = self.what_changed(0).move_to([0, -2.15, 0], aligned_edge=LEFT).set_x(x0, LEFT)
        self.add(self.changed)
        dm = self.dmax
        cb = colorbar(1.5, 3.4, -2.58, f"{-dm:+.1f}", f"{dm:+.1f}", color_fn=lambda u: diverging(2 * u - 1))
        self.add(*cb, txt("change in f", 11, GREY).next_to(cb[2], RIGHT, buff=0.15))
        leg_dot = Circle(radius=0.08, color=INK, stroke_width=2, fill_color=ORANGE,
                         fill_opacity=1).move_to([x0 + 0.08, -2.98, 0])
        self.add(leg_dot, txt(f"actor {focal}'s portfolio", 11, INK).next_to(leg_dot, RIGHT, buff=0.06))
        self.add(VGroup(*[txt(s, 11, GREY) for s in
                          ["grey: no change. Actor %d's own moves never" % focal,
                           "change its landscape; the others' moves do.",
                           "Same grid order as the README panel."]])
                 .arrange(DOWN, buff=0.06, aligned_edge=LEFT).move_to([0, -3.55, 0]).set_x(x0, LEFT))
        self.marker = Circle(radius=0.1, color=INK, stroke_width=2.2, fill_color=ORANGE,
                             fill_opacity=1).move_to(self.xy(self.focal_row[0] - 1, self.G0, self.CS))
        self.add(self.marker)
        self.pk = VGroup()

    def step_run_time(self, s):
        if s["change"] == "none":
            return 0.05
        return 0.45 if s["actor"] == self.focal else 0.2

    def right_step(self, k, s):
        """the change map snaps to the new ministep, so it is legible for the whole step."""
        anims = []
        d1 = self.delta(k)
        for c in range(256):
            col = diverging(d1[c] / self.dmax)
            self.cells[c].set_fill(col).set_stroke(col)
            self.icells[c].set_fill(cividis(self.norm(self.land[k][c])))
        self.changed.become(self.what_changed(k).move_to(self.changed, aligned_edge=LEFT))
        if self.focal_row[k] != self.focal_row[k - 1]:
            b = self.focal_row[k] - 1
            anims.append(self.marker.animate.move_to(self.xy(b, self.G0, self.CS)))
            anims.append(self.imarker.animate.move_to(self.xy(b, self.IG0, self.ICS)))
        return anims

    def right_front(self):
        self.bring_to_front(self.marker, self.imarker)
