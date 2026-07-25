const pptxgen = require("pptxgenjs");

const pres = new pptxgen();
pres.layout = "LAYOUT_16x9";
pres.author = "Stephen Downing";
pres.title = "searchnet: Simulating Strategic Search on Fitness Landscapes";

// ============================================================
// COLOR PALETTE & DESIGN TOKENS
// ============================================================
const C = {
  navy:      "1A365D",
  navyDark:  "0F1F3A",
  teal:      "2B6CB0",
  tealLight: "3B82C4",
  amber:     "D69E2E",
  amberLight:"ECC94B",
  white:     "FFFFFF",
  offWhite:  "F7FAFC",
  gray100:   "F7FAFC",
  gray200:   "EDF2F7",
  gray300:   "E2E8F0",
  gray400:   "A0AEC0",
  gray500:   "718096",
  gray600:   "4A5568",
  gray700:   "2D3748",
  gray800:   "1A202C",
  black:     "000000",
  codeGray:  "EDF2F7",
  codeBorder:"CBD5E0",
  green:     "38A169",
  red:       "E53E3E",
  purple:    "805AD5",
};

const FONTS = {
  heading: "Georgia",
  body:    "Calibri",
  code:    "Consolas",
};

// Factory functions to avoid pptxgenjs option-mutation pitfall
const makeShadow = () => ({ type: "outer", blur: 6, offset: 2, angle: 135, color: "000000", opacity: 0.12 });
const makeCardShadow = () => ({ type: "outer", blur: 4, offset: 1, angle: 135, color: "000000", opacity: 0.10 });

// ============================================================
// SLIDE MASTERS
// ============================================================
pres.defineSlideMaster({
  title: "DARK_FULL",
  background: { color: C.navyDark },
  objects: [
    { shape: pres.shapes.RECTANGLE, options: { x: 0, y: 5.1, w: 10, h: 0.525, fill: { color: C.teal }, transparency: 60 } },
  ],
});

pres.defineSlideMaster({
  title: "CONTENT",
  background: { color: C.offWhite },
  objects: [
    { shape: pres.shapes.RECTANGLE, options: { x: 0, y: 0, w: 10, h: 0.85, fill: { color: C.navy } } },
    { shape: pres.shapes.RECTANGLE, options: { x: 0, y: 5.2, w: 10, h: 0.425, fill: { color: C.gray200 } } },
  ],
});

pres.defineSlideMaster({
  title: "SECTION",
  background: { color: C.navy },
  objects: [
    { shape: pres.shapes.RECTANGLE, options: { x: 0.5, y: 2.4, w: 1.5, h: 0.06, fill: { color: C.amber } } },
  ],
});

// ============================================================
// HELPER FUNCTIONS
// ============================================================
function addContentTitle(slide, title) {
  slide.addText(title, {
    x: 0.5, y: 0.12, w: 9, h: 0.65,
    fontFace: FONTS.heading, fontSize: 22, color: C.white,
    bold: true, valign: "middle", margin: 0,
  });
}

function addFooter(slide, sectionLabel, slideNum) {
  slide.addText(sectionLabel, {
    x: 0.5, y: 5.25, w: 5, h: 0.35,
    fontFace: FONTS.body, fontSize: 9, color: C.gray500,
    valign: "middle", margin: 0,
  });
  slide.addText(String(slideNum), {
    x: 8.5, y: 5.25, w: 1, h: 0.35,
    fontFace: FONTS.body, fontSize: 9, color: C.gray500,
    align: "right", valign: "middle", margin: 0,
  });
}

function addPlainEnglishBox(slide, text, x, y, w, h) {
  slide.addShape(pres.shapes.RECTANGLE, {
    x: x, y: y, w: w, h: h || 0.7,
    fill: { color: "FFFBEB" },
    line: { color: C.amber, width: 1.5 },
    shadow: makeCardShadow(),
  });
  slide.addText([
    { text: "Plain English: ", options: { bold: true, color: C.amber, fontSize: 11, fontFace: FONTS.body } },
    { text: text, options: { color: C.gray700, fontSize: 11, fontFace: FONTS.body } },
  ], {
    x: x + 0.15, y: y + 0.05, w: w - 0.3, h: (h || 0.7) - 0.1,
    valign: "middle", margin: 0,
  });
}

function addCodeBlock(slide, code, x, y, w, h) {
  slide.addShape(pres.shapes.RECTANGLE, {
    x: x, y: y, w: w, h: h,
    fill: { color: C.gray800 },
    shadow: makeCardShadow(),
  });
  slide.addText(code, {
    x: x + 0.15, y: y + 0.1, w: w - 0.3, h: h - 0.2,
    fontFace: FONTS.code, fontSize: 11, color: C.amberLight,
    valign: "top", margin: 0, paraSpaceAfter: 4,
  });
}

function addCard(slide, x, y, w, h, fillColor) {
  slide.addShape(pres.shapes.RECTANGLE, {
    x: x, y: y, w: w, h: h,
    fill: { color: fillColor || C.white },
    shadow: makeCardShadow(),
  });
}

function addKBadge(slide, label, x, y, color) {
  slide.addShape(pres.shapes.RECTANGLE, {
    x: x, y: y, w: 0.6, h: 0.35,
    fill: { color: color || C.teal },
  });
  slide.addText(label, {
    x: x, y: y, w: 0.6, h: 0.35,
    fontFace: FONTS.code, fontSize: 10, color: C.white,
    align: "center", valign: "middle", bold: true, margin: 0,
  });
}

// ============================================================
// SECTION 1: WELCOME & MOTIVATION (Slides 1-5)
// ============================================================

// --- SLIDE 1: Title ---
let s1 = pres.addSlide({ masterName: "DARK_FULL" });
s1.addText("searchnet", {
  x: 0.8, y: 0.8, w: 8.4, h: 1.2,
  fontFace: FONTS.heading, fontSize: 52, color: C.white,
  bold: true, margin: 0,
});
s1.addText("Simulating Strategic Search on Fitness Landscapes", {
  x: 0.8, y: 1.9, w: 8.4, h: 0.8,
  fontFace: FONTS.body, fontSize: 22, color: C.amberLight,
  margin: 0,
});
s1.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 2.9, w: 2.0, h: 0.05, fill: { color: C.amber },
});
s1.addText("Stephen Downing", {
  x: 0.8, y: 3.15, w: 8.4, h: 0.5,
  fontFace: FONTS.body, fontSize: 18, color: C.white, margin: 0,
});
s1.addText("University of Missouri", {
  x: 0.8, y: 3.6, w: 8.4, h: 0.4,
  fontFace: FONTS.body, fontSize: 14, color: C.gray400, margin: 0,
});
s1.addText("Professional Development Workshop  |  2026", {
  x: 0.8, y: 4.1, w: 8.4, h: 0.4,
  fontFace: FONTS.body, fontSize: 13, color: C.gray400, margin: 0,
});

// --- SLIDE 2: The Problem ---
let s2 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s2, "The Problem");
addFooter(s2, "SECTION 1: Welcome & Motivation", 2);

addCard(s2, 0.5, 1.1, 9.0, 1.5, C.white);
s2.addText("NK models assume a single agent on a fixed landscape.", {
  x: 0.8, y: 1.2, w: 8.4, h: 0.5,
  fontFace: FONTS.heading, fontSize: 20, color: C.navy, bold: true, margin: 0,
});
s2.addText("But firms search together, and the landscape changes as they search.", {
  x: 0.8, y: 1.75, w: 8.4, h: 0.5,
  fontFace: FONTS.heading, fontSize: 20, color: C.red, bold: true, margin: 0,
});

// Three problem cards
const problems = [
  { title: "Single Agent", desc: "Classic NK has one searcher on one landscape. No rivals." },
  { title: "Fixed Landscape", desc: "Fitness is exogenous. Others' choices don't change your payoffs." },
  { title: "No Network", desc: "No structure connecting who does what, or who competes with whom." },
];
problems.forEach((p, i) => {
  const cx = 0.5 + i * 3.1;
  addCard(s2, cx, 2.9, 2.8, 1.8, C.white);
  s2.addShape(pres.shapes.RECTANGLE, {
    x: cx, y: 2.9, w: 2.8, h: 0.06, fill: { color: C.red },
  });
  s2.addText(p.title, {
    x: cx + 0.2, y: 3.1, w: 2.4, h: 0.4,
    fontFace: FONTS.heading, fontSize: 15, color: C.navy, bold: true, margin: 0,
  });
  s2.addText(p.desc, {
    x: cx + 0.2, y: 3.5, w: 2.4, h: 0.9,
    fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0,
  });
});

// --- SLIDE 3: The Solution ---
let s3 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s3, "The Solution");
addFooter(s3, "SECTION 1: Welcome & Motivation", 3);

addCard(s3, 0.5, 1.1, 9.0, 1.2, C.white);
s3.addText("searchnet endogenizes the fitness landscape by reformulating NK as a bipartite SAOM.", {
  x: 0.8, y: 1.2, w: 8.4, h: 0.9,
  fontFace: FONTS.heading, fontSize: 20, color: C.navy, bold: true, margin: 0,
});

// Two-column explanation
addCard(s3, 0.5, 2.6, 4.2, 2.3, C.white);
s3.addText("Traditional NK", {
  x: 0.7, y: 2.75, w: 3.8, h: 0.35,
  fontFace: FONTS.heading, fontSize: 14, color: C.gray500, bold: true, margin: 0,
});
s3.addText([
  { text: "One agent, N bits, K interactions\n", options: { breakLine: true, fontSize: 12, color: C.gray600 } },
  { text: "Fixed fitness function\n", options: { breakLine: true, fontSize: 12, color: C.gray600 } },
  { text: "Greedy local search\n", options: { breakLine: true, fontSize: 12, color: C.gray600 } },
  { text: "No strategic interaction", options: { fontSize: 12, color: C.gray600 } },
], {
  x: 0.7, y: 3.15, w: 3.8, h: 1.5,
  fontFace: FONTS.body, bullet: true, margin: 0, paraSpaceAfter: 4,
});

addCard(s3, 5.3, 2.6, 4.2, 2.3, "EBF8FF");
s3.addText("SaoMNK (searchnet)", {
  x: 5.5, y: 2.75, w: 3.8, h: 0.35,
  fontFace: FONTS.heading, fontSize: 14, color: C.teal, bold: true, margin: 0,
});
s3.addText([
  { text: "M agents, N activities, {K} networks\n", options: { breakLine: true, fontSize: 12, color: C.gray700 } },
  { text: "Endogenous fitness landscape\n", options: { breakLine: true, fontSize: 12, color: C.gray700 } },
  { text: "Bounded rationality (QRE)\n", options: { breakLine: true, fontSize: 12, color: C.gray700 } },
  { text: "Full strategic interdependence", options: { fontSize: 12, color: C.gray700 } },
], {
  x: 5.5, y: 3.15, w: 3.8, h: 1.5,
  fontFace: FONTS.body, bullet: true, margin: 0, paraSpaceAfter: 4,
});

// Arrow between columns
s3.addText("\u2192", {
  x: 4.5, y: 3.2, w: 1.0, h: 0.8,
  fontFace: FONTS.body, fontSize: 36, color: C.amber,
  align: "center", valign: "middle", margin: 0,
});

// --- SLIDE 4: What You'll Learn Today ---
let s4 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s4, "What You'll Learn Today");
addFooter(s4, "SECTION 1: Welcome & Motivation", 4);

const learns = [
  { num: "1", title: "Build a Simulation World", desc: "Create actors, activities, and the bipartite network that connects them" },
  { num: "2", title: "Run Experiments", desc: "Execute simulations, apply shocks, and sweep parameters" },
  { num: "3", title: "Read the {K} Dashboard", desc: "Interpret the four coupled network trajectories" },
  { num: "4", title: "Connect to Your Research", desc: "Map your own RQs to searchnet constructs" },
];
learns.forEach((l, i) => {
  const ly = 1.15 + i * 1.05;
  addCard(s4, 0.5, ly, 9.0, 0.9, C.white);
  // Number circle
  s4.addShape(pres.shapes.OVAL, {
    x: 0.7, y: ly + 0.15, w: 0.55, h: 0.55,
    fill: { color: C.teal },
  });
  s4.addText(l.num, {
    x: 0.7, y: ly + 0.15, w: 0.55, h: 0.55,
    fontFace: FONTS.heading, fontSize: 18, color: C.white,
    align: "center", valign: "middle", bold: true, margin: 0,
  });
  s4.addText(l.title, {
    x: 1.5, y: ly + 0.1, w: 7.5, h: 0.4,
    fontFace: FONTS.heading, fontSize: 16, color: C.navy, bold: true, margin: 0,
  });
  s4.addText(l.desc, {
    x: 1.5, y: ly + 0.5, w: 7.5, h: 0.3,
    fontFace: FONTS.body, fontSize: 12, color: C.gray500, margin: 0,
  });
});

// --- SLIDE 5: The One-Slide Summary ---
let s5 = pres.addSlide({ masterName: "DARK_FULL" });
s5.addText("The One-Slide Summary", {
  x: 0.5, y: 0.3, w: 9, h: 0.7,
  fontFace: FONTS.heading, fontSize: 28, color: C.white, bold: true, margin: 0,
});
s5.addShape(pres.shapes.RECTANGLE, {
  x: 0.5, y: 1.0, w: 1.5, h: 0.05, fill: { color: C.amber },
});

// The chain
const chain = [
  { label: "NK", desc: "Single agent\nfixed landscape", color: C.gray600 },
  { label: "\u2282", desc: "", color: C.amber },
  { label: "SaoMNK", desc: "M agents\n{K} networks", color: C.teal },
  { label: "\u2261", desc: "", color: C.amber },
  { label: "Cond. Logit", desc: "McFadden choice\non bipartite DGP", color: C.tealLight },
  { label: "\u2192", desc: "", color: C.amber },
  { label: "QRE", desc: "Bounded rational\nequilibrium", color: C.green },
];

let chainX = 0.3;
chain.forEach((c, i) => {
  if (c.desc === "") {
    // Connector symbol
    s5.addText(c.label, {
      x: chainX, y: 1.6, w: 0.6, h: 1.0,
      fontFace: FONTS.body, fontSize: 28, color: c.color,
      align: "center", valign: "middle", margin: 0,
    });
    chainX += 0.6;
  } else {
    const bw = i === 0 ? 1.5 : 2.2;
    addCard(s5, chainX, 1.6, bw, 1.0, C.gray800);
    s5.addShape(pres.shapes.RECTANGLE, {
      x: chainX, y: 1.6, w: bw, h: 0.06, fill: { color: c.color },
    });
    s5.addText(c.label, {
      x: chainX + 0.1, y: 1.7, w: bw - 0.2, h: 0.35,
      fontFace: FONTS.code, fontSize: 13, color: c.color, bold: true, margin: 0,
    });
    s5.addText(c.desc, {
      x: chainX + 0.1, y: 2.05, w: bw - 0.2, h: 0.45,
      fontFace: FONTS.body, fontSize: 9, color: C.gray400, margin: 0,
    });
    chainX += bw + 0.1;
  }
});

addPlainEnglishBox(s5, "Firms choosing activities on a shared landscape, with bounded rationality, produce a well-defined equilibrium we can simulate and estimate.", 0.5, 3.0, 9.0, 0.7);

s5.addText("This is the theoretical backbone of searchnet.", {
  x: 0.5, y: 3.9, w: 9.0, h: 0.5,
  fontFace: FONTS.body, fontSize: 14, color: C.gray400, italic: true, margin: 0,
});

// ============================================================
// SECTION 2: THE SIMULATION WORLD (Slides 6-17)
// ============================================================

// --- Section Divider ---
let s6div = pres.addSlide({ masterName: "SECTION" });
s6div.addText("SECTION 2", {
  x: 0.5, y: 1.6, w: 9, h: 0.6,
  fontFace: FONTS.body, fontSize: 14, color: C.amber, bold: true, charSpacing: 4, margin: 0,
});
s6div.addText("The Simulation World", {
  x: 0.5, y: 2.6, w: 9, h: 0.9,
  fontFace: FONTS.heading, fontSize: 36, color: C.white, bold: true, margin: 0,
});
s6div.addText("Building a miniature economy from scratch", {
  x: 0.5, y: 3.5, w: 9, h: 0.5,
  fontFace: FONTS.body, fontSize: 16, color: C.gray400, margin: 0,
});

// --- SLIDE 6: Building a Miniature Economy ---
let s6 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s6, "Building a Miniature Economy");
addFooter(s6, "SECTION 2: The Simulation World", 6);

addCard(s6, 0.5, 1.1, 9.0, 1.0, C.white);
s6.addText("Think of it like building a factory: you choose the machines (N), the workers (M), and the wiring between them ({K}).", {
  x: 0.7, y: 1.2, w: 8.6, h: 0.7,
  fontFace: FONTS.body, fontSize: 15, color: C.gray700, margin: 0,
});

// Three-column metaphor
const metaphors = [
  { title: "Workers (M)", desc: "Actors: firms, people, teams.\nThey choose what to do.", col: C.teal },
  { title: "Machines (N)", desc: "Activities: technologies, markets, strategies.\nThey get chosen.", col: C.amber },
  { title: "Wiring ({K})", desc: "The hidden networks.\nHow everything connects.", col: C.purple },
];
metaphors.forEach((m, i) => {
  const mx = 0.5 + i * 3.1;
  addCard(s6, mx, 2.4, 2.8, 2.0, C.white);
  s6.addShape(pres.shapes.RECTANGLE, {
    x: mx, y: 2.4, w: 2.8, h: 0.06, fill: { color: m.col },
  });
  s6.addText(m.title, {
    x: mx + 0.2, y: 2.6, w: 2.4, h: 0.4,
    fontFace: FONTS.heading, fontSize: 15, color: C.navy, bold: true, margin: 0,
  });
  s6.addText(m.desc, {
    x: mx + 0.2, y: 3.05, w: 2.4, h: 1.0,
    fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0,
  });
});

addPlainEnglishBox(s6, "A searchnet simulation is an artificial economy: agents choose activities, and the four hidden networks emerge from their collective choices.", 0.5, 4.6, 9.0, 0.55);

// --- SLIDE 7: Actors (M) ---
let s7 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s7, "Actors (M)");
addFooter(s7, "SECTION 2: The Simulation World", 7);

addCard(s7, 0.5, 1.1, 5.5, 2.5, C.white);
s7.addText("Who is searching?", {
  x: 0.7, y: 1.2, w: 5.1, h: 0.4,
  fontFace: FONTS.heading, fontSize: 16, color: C.navy, bold: true, margin: 0,
});
s7.addText([
  { text: "Firms in an industry\n", options: { breakLine: true } },
  { text: "Investors in a market\n", options: { breakLine: true } },
  { text: "Scientists in a field\n", options: { breakLine: true } },
  { text: "Teams in an organization", options: {} },
], {
  x: 0.7, y: 1.7, w: 5.1, h: 1.6,
  fontFace: FONTS.body, fontSize: 13, color: C.gray600, bullet: true, margin: 0, paraSpaceAfter: 6,
});

// Visual: row of actor boxes
addCard(s7, 6.3, 1.1, 3.2, 2.5, C.gray200);
s7.addText("Rows of B", {
  x: 6.5, y: 1.2, w: 2.8, h: 0.3,
  fontFace: FONTS.code, fontSize: 11, color: C.gray500, align: "center", margin: 0,
});
for (let i = 0; i < 6; i++) {
  s7.addShape(pres.shapes.RECTANGLE, {
    x: 6.6, y: 1.65 + i * 0.3, w: 2.6, h: 0.22,
    fill: { color: i < 3 ? C.teal : C.tealLight },
    transparency: 20,
  });
  s7.addText("Actor " + (i + 1), {
    x: 6.7, y: 1.65 + i * 0.3, w: 2.4, h: 0.22,
    fontFace: FONTS.code, fontSize: 9, color: C.white, valign: "middle", margin: 0,
  });
}

s7.addText("M = number of actors (rows of the bipartite matrix B)", {
  x: 0.5, y: 3.8, w: 9.0, h: 0.4,
  fontFace: FONTS.code, fontSize: 13, color: C.navy, margin: 0,
});

addPlainEnglishBox(s7, "Actors are the row-players. Each actor has a portfolio of chosen activities.", 0.5, 4.3, 9.0, 0.55);

// --- SLIDE 8: Components (N) ---
let s8 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s8, "Components (N)");
addFooter(s8, "SECTION 2: The Simulation World", 8);

addCard(s8, 0.5, 1.1, 5.5, 2.5, C.white);
s8.addText("What can be chosen?", {
  x: 0.7, y: 1.2, w: 5.1, h: 0.4,
  fontFace: FONTS.heading, fontSize: 16, color: C.navy, bold: true, margin: 0,
});
s8.addText([
  { text: "Routes an airline can fly\n", options: { breakLine: true } },
  { text: "Technologies a firm can adopt\n", options: { breakLine: true } },
  { text: "Research topics to pursue\n", options: { breakLine: true } },
  { text: "Markets to enter", options: {} },
], {
  x: 0.7, y: 1.7, w: 5.1, h: 1.6,
  fontFace: FONTS.body, fontSize: 13, color: C.gray600, bullet: true, margin: 0, paraSpaceAfter: 6,
});

// Visual: column blocks
addCard(s8, 6.3, 1.1, 3.2, 2.5, C.gray200);
s8.addText("Columns of B", {
  x: 6.5, y: 1.2, w: 2.8, h: 0.3,
  fontFace: FONTS.code, fontSize: 11, color: C.gray500, align: "center", margin: 0,
});
for (let i = 0; i < 8; i++) {
  s8.addShape(pres.shapes.RECTANGLE, {
    x: 6.55 + i * 0.32, y: 1.6, w: 0.26, h: 1.8,
    fill: { color: C.amber },
    transparency: 20 + i * 5,
  });
}
s8.addText("N1  N2  N3  N4  N5  N6  N7  N8", {
  x: 6.5, y: 3.45, w: 2.8, h: 0.2,
  fontFace: FONTS.code, fontSize: 7, color: C.gray500, align: "center", margin: 0,
});

s8.addText("N = number of components (columns of the bipartite matrix B)", {
  x: 0.5, y: 3.8, w: 9.0, h: 0.4,
  fontFace: FONTS.code, fontSize: 13, color: C.navy, margin: 0,
});

addPlainEnglishBox(s8, "Components are the column-resources. Each is either adopted (1) or not (0) by each actor.", 0.5, 4.3, 9.0, 0.55);

// --- SLIDE 9: The Bipartite Matrix ---
let s9 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s9, "The Bipartite Matrix");
addFooter(s9, "SECTION 2: The Simulation World", 9);

s9.addText([
  { text: "B \u2208 {0,1}", options: { fontFace: FONTS.code, fontSize: 14, color: C.navy, bold: true } },
  { text: "M\u00D7N", options: { fontFace: FONTS.code, fontSize: 10, color: C.navy, bold: true, superscript: true } },
], {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4, margin: 0,
});

// Draw matrix grid
const matX = 1.5, matY = 1.7;
const cellW = 0.55, cellH = 0.35;
const matRows = 5, matCols = 8;
const matData = [
  [1,0,1,0,0,1,0,0],
  [0,1,0,1,1,0,0,1],
  [1,1,0,0,0,0,1,0],
  [0,0,1,1,0,1,0,1],
  [1,0,0,0,1,0,1,0],
];

// Column headers
for (let j = 0; j < matCols; j++) {
  s9.addText("n" + (j+1), {
    x: matX + j * cellW, y: matY - 0.3, w: cellW, h: 0.25,
    fontFace: FONTS.code, fontSize: 9, color: C.amber, align: "center", valign: "middle", margin: 0,
  });
}
// Row headers
for (let i = 0; i < matRows; i++) {
  s9.addText("m" + (i+1), {
    x: matX - 0.5, y: matY + i * cellH, w: 0.45, h: cellH,
    fontFace: FONTS.code, fontSize: 9, color: C.teal, align: "right", valign: "middle", margin: 0,
  });
}
// Cells
for (let i = 0; i < matRows; i++) {
  for (let j = 0; j < matCols; j++) {
    const val = matData[i][j];
    s9.addShape(pres.shapes.RECTANGLE, {
      x: matX + j * cellW, y: matY + i * cellH, w: cellW, h: cellH,
      fill: { color: val ? C.teal : C.gray200 },
      line: { color: C.gray300, width: 0.5 },
    });
    s9.addText(String(val), {
      x: matX + j * cellW, y: matY + i * cellH, w: cellW, h: cellH,
      fontFace: FONTS.code, fontSize: 11, color: val ? C.white : C.gray400,
      align: "center", valign: "middle", margin: 0,
    });
  }
}

// Annotations
s9.addText("Actors (M)", {
  x: 0.3, y: 2.2, w: 1.0, h: 0.4,
  fontFace: FONTS.body, fontSize: 10, color: C.teal, bold: true, rotate: 270, margin: 0,
});
s9.addText("Components (N)", {
  x: 2.5, y: matY + matRows * cellH + 0.1, w: 2.5, h: 0.3,
  fontFace: FONTS.body, fontSize: 10, color: C.amber, bold: true, align: "center", margin: 0,
});

addPlainEnglishBox(s9, "The matrix B is the state of the world. 1 = actor i does activity j. Everything else derives from B.", 0.5, 4.3, 9.0, 0.55);

// --- SLIDE 10: Code ---
let s10 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s10, "Your First Simulation World");
addFooter(s10, "SECTION 2: The Simulation World", 10);

addCodeBlock(s10,
  'library(searchnet)\n\n' +
  '# Create the environment\n' +
  'env <- saomnk_env(\n' +
  '  M       = 6,      # 6 actors (firms)\n' +
  '  N       = 10,     # 10 components (activities)\n' +
  '  density = 0.3,    # 30% of cells start as 1\n' +
  '  seed    = 42      # reproducible\n' +
  ')',
  0.5, 1.1, 9.0, 2.8
);

addPlainEnglishBox(s10, "One line of code creates an entire economy: 6 firms, 10 activities, and a random initial assignment.", 0.5, 4.1, 9.0, 0.55);

s10.addText("Try it: Change M, N, or density and see what happens to the initial matrix.", {
  x: 0.5, y: 4.8, w: 9.0, h: 0.3,
  fontFace: FONTS.body, fontSize: 12, color: C.teal, italic: true, margin: 0,
});

// --- SLIDE 11: The Four Hidden Networks ---
let s11 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s11, "The Four Hidden Networks: {K}");
addFooter(s11, "SECTION 2: The Simulation World", 11);

s11.addText("One bipartite matrix B generates four coupled network layers.", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4,
  fontFace: FONTS.body, fontSize: 14, color: C.gray600, margin: 0,
});

const kNets = [
  { k: "K_AC", label: "Scope", desc: "Actor\u2192Component\n(How much is each firm doing?)", col: C.teal, proj: "B" },
  { k: "K_CA", label: "Popularity", desc: "Component\u2192Actor\n(How crowded is each activity?)", col: C.amber, proj: "B\u1D40" },
  { k: "K_AA", label: "Sociality", desc: "Actor\u2192Actor\n(Who overlaps with whom?)", col: C.green, proj: "BB\u1D40" },
  { k: "K_CC", label: "Epistasis", desc: "Component\u2192Component\n(Which activities go together?)", col: C.purple, proj: "B\u1D40B" },
];
kNets.forEach((kn, i) => {
  const kx = 0.5 + (i % 2) * 4.7;
  const ky = 1.7 + Math.floor(i / 2) * 1.7;
  addCard(s11, kx, ky, 4.3, 1.4, C.white);
  addKBadge(s11, kn.k, kx + 0.15, ky + 0.15, kn.col);
  s11.addText(kn.label, {
    x: kx + 0.9, y: ky + 0.1, w: 3.2, h: 0.35,
    fontFace: FONTS.heading, fontSize: 14, color: C.navy, bold: true, margin: 0,
  });
  s11.addText(kn.desc, {
    x: kx + 0.15, y: ky + 0.55, w: 3.0, h: 0.7,
    fontFace: FONTS.body, fontSize: 11, color: C.gray600, margin: 0,
  });
  s11.addText("Projection: " + kn.proj, {
    x: kx + 3.0, y: ky + 0.6, w: 1.1, h: 0.5,
    fontFace: FONTS.code, fontSize: 9, color: kn.col, align: "center", margin: 0,
  });
});

// --- SLIDE 12: K_AC (Scope) ---
let s12 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s12, "K_AC: Scope");
addFooter(s12, "SECTION 2: The Simulation World", 12);

addKBadge(s12, "K_AC", 0.5, 1.15, C.teal);
s12.addText("How much is each firm doing?", {
  x: 1.3, y: 1.1, w: 8.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

addCard(s12, 0.5, 1.7, 5.0, 1.8, C.white);
s12.addText("Definition", {
  x: 0.7, y: 1.8, w: 4.6, h: 0.3,
  fontFace: FONTS.body, fontSize: 11, color: C.gray500, bold: true, margin: 0,
});
s12.addText("K_AC(i) = \u2211_j B(i,j)", {
  x: 0.7, y: 2.15, w: 4.6, h: 0.35,
  fontFace: FONTS.code, fontSize: 14, color: C.navy, margin: 0,
});
s12.addText("The row-sum of B: count of activities adopted by actor i.", {
  x: 0.7, y: 2.55, w: 4.6, h: 0.5,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0,
});

// Visual: bar chart metaphor
addCard(s12, 5.8, 1.7, 3.7, 1.8, C.gray200);
s12.addText("Scope per Actor", {
  x: 6.0, y: 1.8, w: 3.3, h: 0.3,
  fontFace: FONTS.body, fontSize: 10, color: C.gray500, align: "center", margin: 0,
});
const scopeVals = [3, 5, 2, 7, 4, 1];
scopeVals.forEach((v, i) => {
  const bw = v * 0.38;
  s12.addShape(pres.shapes.RECTANGLE, {
    x: 6.2, y: 2.15 + i * 0.2, w: bw, h: 0.15,
    fill: { color: C.teal },
  });
  s12.addText("m" + (i+1), {
    x: 5.9, y: 2.15 + i * 0.2, w: 0.3, h: 0.15,
    fontFace: FONTS.code, fontSize: 7, color: C.gray500, align: "right", valign: "middle", margin: 0,
  });
});

addPlainEnglishBox(s12, "Scope measures how diversified each actor is. High scope = broad portfolio, low scope = specialist.", 0.5, 3.7, 9.0, 0.55);

s12.addText("In the airline example: scope = number of routes an airline flies.", {
  x: 0.5, y: 4.4, w: 9.0, h: 0.4,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, italic: true, margin: 0,
});

// --- SLIDE 13: K_CA (Popularity) ---
let s13 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s13, "K_CA: Popularity");
addFooter(s13, "SECTION 2: The Simulation World", 13);

addKBadge(s13, "K_CA", 0.5, 1.15, C.amber);
s13.addText("How crowded is each activity?", {
  x: 1.3, y: 1.1, w: 8.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

addCard(s13, 0.5, 1.7, 5.0, 1.8, C.white);
s13.addText("Definition", {
  x: 0.7, y: 1.8, w: 4.6, h: 0.3,
  fontFace: FONTS.body, fontSize: 11, color: C.gray500, bold: true, margin: 0,
});
s13.addText("K_CA(j) = \u2211_i B(i,j)", {
  x: 0.7, y: 2.15, w: 4.6, h: 0.35,
  fontFace: FONTS.code, fontSize: 14, color: C.navy, margin: 0,
});
s13.addText("The column-sum of B: count of actors adopting activity j.", {
  x: 0.7, y: 2.55, w: 4.6, h: 0.5,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0,
});

// Visual
addCard(s13, 5.8, 1.7, 3.7, 1.8, C.gray200);
s13.addText("Popularity per Activity", {
  x: 6.0, y: 1.8, w: 3.3, h: 0.3,
  fontFace: FONTS.body, fontSize: 10, color: C.gray500, align: "center", margin: 0,
});
const popVals = [2, 4, 1, 3, 5, 2, 3, 1];
popVals.forEach((v, i) => {
  const bw = v * 0.32;
  s13.addShape(pres.shapes.RECTANGLE, {
    x: 6.2, y: 2.2 + i * 0.15, w: bw, h: 0.11,
    fill: { color: C.amber },
  });
});

addPlainEnglishBox(s13, "Popularity measures competition. High popularity = red ocean; low popularity = blue ocean.", 0.5, 3.7, 9.0, 0.55);

// --- SLIDE 14: K_AA (Sociality) ---
let s14 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s14, "K_AA: Sociality");
addFooter(s14, "SECTION 2: The Simulation World", 14);

addKBadge(s14, "K_AA", 0.5, 1.15, C.green);
s14.addText("Who overlaps with whom?", {
  x: 1.3, y: 1.1, w: 8.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

addCard(s14, 0.5, 1.7, 4.5, 2.0, C.white);
s14.addText("Projection: BB\u1D40", {
  x: 0.7, y: 1.8, w: 4.1, h: 0.3,
  fontFace: FONTS.code, fontSize: 12, color: C.green, bold: true, margin: 0,
});
s14.addText("K_AA(i,j) = number of shared activities between actors i and j.", {
  x: 0.7, y: 2.15, w: 4.1, h: 0.5,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0,
});
s14.addText("Diagonal = scope. Off-diagonal = rivalry overlap.", {
  x: 0.7, y: 2.7, w: 4.1, h: 0.5,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0,
});

// Mini adjacency matrix
addCard(s14, 5.5, 1.7, 4.0, 2.0, C.gray200);
const aaSize = 4;
for (let i = 0; i < aaSize; i++) {
  for (let j = 0; j < aaSize; j++) {
    const intensity = i === j ? 0 : Math.floor(Math.random() * 40 + 20);
    s14.addShape(pres.shapes.RECTANGLE, {
      x: 6.0 + j * 0.6, y: 2.0 + i * 0.35, w: 0.55, h: 0.3,
      fill: { color: i === j ? C.green : C.green },
      transparency: i === j ? 10 : 50 + intensity,
    });
  }
}
s14.addText("Actor-by-Actor overlap", {
  x: 5.7, y: 3.5, w: 3.6, h: 0.2,
  fontFace: FONTS.body, fontSize: 9, color: C.gray500, align: "center", margin: 0,
});

addPlainEnglishBox(s14, "Sociality captures competitive overlap. Two airlines flying the same routes have high K_AA.", 0.5, 3.9, 9.0, 0.55);

// --- SLIDE 15: K_CC (Epistasis) ---
let s15 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s15, "K_CC: Epistasis");
addFooter(s15, "SECTION 2: The Simulation World", 15);

addKBadge(s15, "K_CC", 0.5, 1.15, C.purple);
s15.addText("Which activities go together?", {
  x: 1.3, y: 1.1, w: 8.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

addCard(s15, 0.5, 1.7, 4.5, 2.0, C.white);
s15.addText("Projection: B\u1D40B", {
  x: 0.7, y: 1.8, w: 4.1, h: 0.3,
  fontFace: FONTS.code, fontSize: 12, color: C.purple, bold: true, margin: 0,
});
s15.addText("K_CC(j,k) = number of actors doing both activity j and k.", {
  x: 0.7, y: 2.15, w: 4.1, h: 0.5,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0,
});
s15.addText("Diagonal = popularity. Off-diagonal = complementarity.", {
  x: 0.7, y: 2.7, w: 4.1, h: 0.5,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0,
});

// Mini component adjacency
addCard(s15, 5.5, 1.7, 4.0, 2.0, C.gray200);
for (let i = 0; i < 5; i++) {
  for (let j = 0; j < 5; j++) {
    s15.addShape(pres.shapes.RECTANGLE, {
      x: 6.0 + j * 0.55, y: 1.95 + i * 0.32, w: 0.5, h: 0.28,
      fill: { color: C.purple },
      transparency: i === j ? 10 : 40 + ((i + j) % 4) * 10,
    });
  }
}
s15.addText("Activity-by-Activity co-adoption", {
  x: 5.7, y: 3.5, w: 3.6, h: 0.2,
  fontFace: FONTS.body, fontSize: 9, color: C.gray500, align: "center", margin: 0,
});

addPlainEnglishBox(s15, "Epistasis is the traditional NK 'K' -- but now it emerges from collective behavior instead of being hard-coded.", 0.5, 3.9, 9.0, 0.55);

// --- SLIDE 16: The Coupling ---
let s16 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s16, "The Coupling: Change One, Change All");
addFooter(s16, "SECTION 2: The Simulation World", 16);

s16.addText("When actor i adds activity j, all four K networks update simultaneously.", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.5,
  fontFace: FONTS.body, fontSize: 14, color: C.gray700, margin: 0,
});

// Circular dependency visual
const centerX = 5.0, centerY = 3.0, radius = 1.3;
const kLabels = [
  { k: "K_AC", angle: -90, col: C.teal },
  { k: "K_CA", angle: 0, col: C.amber },
  { k: "K_AA", angle: 90, col: C.green },
  { k: "K_CC", angle: 180, col: C.purple },
];
kLabels.forEach((kl) => {
  const rad = kl.angle * Math.PI / 180;
  const kx = centerX + radius * Math.cos(rad) - 0.4;
  const ky = centerY + radius * Math.sin(rad) - 0.2;
  s16.addShape(pres.shapes.RECTANGLE, {
    x: kx, y: ky, w: 0.8, h: 0.4,
    fill: { color: kl.col },
  });
  s16.addText(kl.k, {
    x: kx, y: ky, w: 0.8, h: 0.4,
    fontFace: FONTS.code, fontSize: 10, color: C.white,
    align: "center", valign: "middle", bold: true, margin: 0,
  });
});

// Central B node
s16.addShape(pres.shapes.OVAL, {
  x: centerX - 0.35, y: centerY - 0.35, w: 0.7, h: 0.7,
  fill: { color: C.navy },
});
s16.addText("B", {
  x: centerX - 0.35, y: centerY - 0.35, w: 0.7, h: 0.7,
  fontFace: FONTS.code, fontSize: 18, color: C.white,
  align: "center", valign: "middle", bold: true, margin: 0,
});

addPlainEnglishBox(s16, "This is why we need all four K's. They aren't independent--they are four views of the same bipartite state B.", 0.5, 4.5, 9.0, 0.55);

// --- SLIDE 17: The {K} Table ---
let s17 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s17, "The {K} Table: Formal Summary");
addFooter(s17, "SECTION 2: The Simulation World", 17);

const kTableData = [
  [
    { text: "Symbol", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 11, fontFace: FONTS.body } },
    { text: "Name", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 11, fontFace: FONTS.body } },
    { text: "Projection", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 11, fontFace: FONTS.body } },
    { text: "Dimension", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 11, fontFace: FONTS.body } },
    { text: "Interpretation", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 11, fontFace: FONTS.body } },
  ],
  [
    { text: "K_AC", options: { fontFace: FONTS.code, fontSize: 10, color: C.teal, bold: true } },
    { text: "Scope", options: { fontSize: 10, fontFace: FONTS.body } },
    { text: "rowSums(B)", options: { fontFace: FONTS.code, fontSize: 10 } },
    { text: "M \u00D7 1", options: { fontFace: FONTS.code, fontSize: 10 } },
    { text: "Actor diversification", options: { fontSize: 10, fontFace: FONTS.body } },
  ],
  [
    { text: "K_CA", options: { fontFace: FONTS.code, fontSize: 10, color: C.amber, bold: true } },
    { text: "Popularity", options: { fontSize: 10, fontFace: FONTS.body } },
    { text: "colSums(B)", options: { fontFace: FONTS.code, fontSize: 10 } },
    { text: "1 \u00D7 N", options: { fontFace: FONTS.code, fontSize: 10 } },
    { text: "Activity crowding", options: { fontSize: 10, fontFace: FONTS.body } },
  ],
  [
    { text: "K_AA", options: { fontFace: FONTS.code, fontSize: 10, color: C.green, bold: true } },
    { text: "Sociality", options: { fontSize: 10, fontFace: FONTS.body } },
    { text: "B %*% t(B)", options: { fontFace: FONTS.code, fontSize: 10 } },
    { text: "M \u00D7 M", options: { fontFace: FONTS.code, fontSize: 10 } },
    { text: "Competitive overlap", options: { fontSize: 10, fontFace: FONTS.body } },
  ],
  [
    { text: "K_CC", options: { fontFace: FONTS.code, fontSize: 10, color: C.purple, bold: true } },
    { text: "Epistasis", options: { fontSize: 10, fontFace: FONTS.body } },
    { text: "t(B) %*% B", options: { fontFace: FONTS.code, fontSize: 10 } },
    { text: "N \u00D7 N", options: { fontFace: FONTS.code, fontSize: 10 } },
    { text: "Activity complementarity", options: { fontSize: 10, fontFace: FONTS.body } },
  ],
];

s17.addTable(kTableData, {
  x: 0.5, y: 1.2, w: 9.0,
  colW: [1.0, 1.3, 1.6, 1.2, 3.9],
  border: { pt: 0.5, color: C.gray300 },
  rowH: [0.4, 0.4, 0.4, 0.4, 0.4],
  autoPage: false,
});

addPlainEnglishBox(s17, "All four K's come from the same matrix B. Change B and you change all four simultaneously. This is the coupling that makes SaoMNK different from standard NK.", 0.5, 3.7, 9.0, 0.75);


// ============================================================
// SECTION 3: HOW TIME WORKS (Slides 18-25)
// ============================================================

// --- Section Divider ---
let s18div = pres.addSlide({ masterName: "SECTION" });
s18div.addText("SECTION 3", {
  x: 0.5, y: 1.6, w: 9, h: 0.6,
  fontFace: FONTS.body, fontSize: 14, color: C.amber, bold: true, charSpacing: 4, margin: 0,
});
s18div.addText("How Time Works", {
  x: 0.5, y: 2.6, w: 9, h: 0.9,
  fontFace: FONTS.heading, fontSize: 36, color: C.white, bold: true, margin: 0,
});
s18div.addText("The ministep clock, choice functions, and bounded rationality", {
  x: 0.5, y: 3.5, w: 9, h: 0.5,
  fontFace: FONTS.body, fontSize: 16, color: C.gray400, margin: 0,
});

// --- SLIDE 18: The Ministep Clock ---
let s18 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s18, "The Ministep Clock");
addFooter(s18, "SECTION 3: How Time Works", 18);

s18.addText("One actor at a time, random order", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

// Timeline visual
const tlY = 2.0;
s18.addShape(pres.shapes.LINE, {
  x: 0.8, y: tlY + 0.25, w: 8.4, h: 0,
  line: { color: C.gray400, width: 2 },
});

const steps = ["m3", "m1", "m5", "m2", "m3", "m4", "m1", "m6"];
const stepColors = [C.green, C.teal, C.purple, C.amber, C.green, C.red, C.teal, C.gray600];
steps.forEach((st, i) => {
  const sx = 0.8 + i * 1.05;
  s18.addShape(pres.shapes.OVAL, {
    x: sx, y: tlY, w: 0.5, h: 0.5,
    fill: { color: stepColors[i] },
  });
  s18.addText(st, {
    x: sx, y: tlY, w: 0.5, h: 0.5,
    fontFace: FONTS.code, fontSize: 9, color: C.white,
    align: "center", valign: "middle", bold: true, margin: 0,
  });
  s18.addText("t=" + (i + 1), {
    x: sx - 0.05, y: tlY + 0.55, w: 0.6, h: 0.25,
    fontFace: FONTS.code, fontSize: 8, color: C.gray500, align: "center", margin: 0,
  });
});

addCard(s18, 0.5, 3.0, 9.0, 1.2, C.white);
s18.addText([
  { text: "At each ministep, exactly one actor gets a chance to move.\n", options: { breakLine: true, bullet: true } },
  { text: "The actor can ADD an activity, DROP an activity, or PASS.\n", options: { breakLine: true, bullet: true } },
  { text: "After the choice, the clock advances and another actor is drawn.", options: { bullet: true } },
], {
  x: 0.7, y: 3.1, w: 8.6, h: 1.0,
  fontFace: FONTS.body, fontSize: 13, color: C.gray600, margin: 0, paraSpaceAfter: 4,
});

addPlainEnglishBox(s18, "Time is not simultaneous. Actors take turns, like a board game. This is the SAOM (Stochastic Actor-Oriented Model) clock.", 0.5, 4.4, 9.0, 0.65);

// --- SLIDE 19: The Rate Function ---
let s19 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s19, "The Rate Function");
addFooter(s19, "SECTION 3: How Time Works", 19);

s19.addText("Who acts next?", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

addCard(s19, 0.5, 1.7, 9.0, 1.3, C.white);
s19.addText("\u03BB_i = rate parameter for actor i", {
  x: 0.7, y: 1.8, w: 8.6, h: 0.35,
  fontFace: FONTS.code, fontSize: 14, color: C.navy, margin: 0,
});
s19.addText("P(actor i acts next) = \u03BB_i / \u2211_k \u03BB_k", {
  x: 0.7, y: 2.2, w: 8.6, h: 0.35,
  fontFace: FONTS.code, fontSize: 14, color: C.teal, margin: 0,
});
s19.addText("Waiting time between ministeps ~ Exponential(\u2211 \u03BB_i)", {
  x: 0.7, y: 2.6, w: 8.6, h: 0.3,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0,
});

addPlainEnglishBox(s19, "Higher rate = more opportunities to act. Think of it as 'clock speed' for each player.", 0.5, 3.2, 9.0, 0.55);

s19.addText("Default: equal rates (\u03BB_i = \u03BB for all i). Can be made heterogeneous.", {
  x: 0.5, y: 4.0, w: 9.0, h: 0.4,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, italic: true, margin: 0,
});

// --- SLIDE 20: The Choice Set ---
let s20 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s20, "The Choice Set");
addFooter(s20, "SECTION 3: How Time Works", 20);

s20.addText("When it's your turn, what can you do?", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

const choices = [
  { label: "ADD", desc: "Adopt a new activity\nB(i,j): 0 \u2192 1", col: C.green, icon: "+" },
  { label: "DROP", desc: "Abandon an activity\nB(i,j): 1 \u2192 0", col: C.red, icon: "\u2013" },
  { label: "PASS", desc: "Do nothing\nB unchanged", col: C.gray500, icon: "\u2205" },
];
choices.forEach((ch, i) => {
  const cx = 0.5 + i * 3.2;
  addCard(s20, cx, 1.8, 2.9, 2.2, C.white);
  s20.addShape(pres.shapes.OVAL, {
    x: cx + 1.05, y: 1.95, w: 0.7, h: 0.7,
    fill: { color: ch.col },
  });
  s20.addText(ch.icon, {
    x: cx + 1.05, y: 1.95, w: 0.7, h: 0.7,
    fontFace: FONTS.heading, fontSize: 24, color: C.white,
    align: "center", valign: "middle", bold: true, margin: 0,
  });
  s20.addText(ch.label, {
    x: cx + 0.2, y: 2.75, w: 2.5, h: 0.35,
    fontFace: FONTS.heading, fontSize: 16, color: ch.col, bold: true, align: "center", margin: 0,
  });
  s20.addText(ch.desc, {
    x: cx + 0.2, y: 3.15, w: 2.5, h: 0.6,
    fontFace: FONTS.body, fontSize: 11, color: C.gray600, align: "center", margin: 0,
  });
});

addPlainEnglishBox(s20, "Each ministep is a binary choice about one cell of B. Flip it on, flip it off, or leave it alone.", 0.5, 4.2, 9.0, 0.55);

// --- SLIDE 21: The Choice Function ---
let s21 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s21, "The Choice Function");
addFooter(s21, "SECTION 3: How Time Works", 21);

s21.addText("McFadden Conditional Logit", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

addCard(s21, 0.5, 1.7, 9.0, 1.5, C.white);
s21.addText("P(flip j | actor i) = exp(\u03B2 \u00B7 \u0394U_ij) / \u2211_k exp(\u03B2 \u00B7 \u0394U_ik)", {
  x: 0.7, y: 1.85, w: 8.6, h: 0.5,
  fontFace: FONTS.code, fontSize: 15, color: C.navy, margin: 0,
});
s21.addText([
  { text: "\u0394U_ij = change in utility if actor i flips cell j\n", options: { breakLine: true } },
  { text: "\u03B2 = bounded rationality parameter (inverse temperature)\n", options: { breakLine: true } },
  { text: "Sum runs over all possible flips + the pass option", options: {} },
], {
  x: 0.7, y: 2.4, w: 8.6, h: 0.7,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, bullet: true, margin: 0, paraSpaceAfter: 4,
});

addPlainEnglishBox(s21, "The actor picks the option with the highest utility gain, but with noise. Better options are more likely, not guaranteed.", 0.5, 3.4, 9.0, 0.55);

s21.addText("This is exactly the discrete choice model from industrial organization and transportation economics.", {
  x: 0.5, y: 4.2, w: 9.0, h: 0.4,
  fontFace: FONTS.body, fontSize: 12, color: C.gray500, italic: true, margin: 0,
});

// --- SLIDE 22: WORKED EXAMPLE (Choice) ---
let s22 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s22, "Worked Example: Choice Probabilities");
addFooter(s22, "SECTION 3: How Time Works", 22);

s22.addText("Actor m1 currently does {n2, n5, n7}. It's her turn. \u03B2 = 2.", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4,
  fontFace: FONTS.body, fontSize: 13, color: C.gray700, margin: 0,
});

const exTable = [
  [
    { text: "Option", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 10, fontFace: FONTS.body } },
    { text: "Action", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 10, fontFace: FONTS.body } },
    { text: "\u0394U", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 10, fontFace: FONTS.body } },
    { text: "exp(\u03B2\u00B7\u0394U)", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 10, fontFace: FONTS.body } },
    { text: "P(choose)", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 10, fontFace: FONTS.body } },
  ],
  ["Add n1", "+n1 to portfolio", "+0.8", "4.95", "0.31"],
  ["Add n3", "+n3 to portfolio", "+0.3", "1.82", "0.11"],
  ["Drop n2", "-n2 from portfolio", "-0.1", "0.82", "0.05"],
  ["Drop n5", "-n5 from portfolio", "+0.5", "2.72", "0.17"],
  ["Drop n7", "-n7 from portfolio", "-0.4", "0.45", "0.03"],
  ["Pass", "Do nothing", "0.0", "1.00", "0.06"],
  [
    { text: "Add n4", options: { bold: true, color: C.teal, fontSize: 10 } },
    { text: "+n4 to portfolio", options: { bold: true, color: C.teal, fontSize: 10 } },
    { text: "+1.2", options: { bold: true, color: C.teal, fontSize: 10, fontFace: FONTS.code } },
    { text: "11.02", options: { bold: true, color: C.teal, fontSize: 10, fontFace: FONTS.code } },
    { text: "0.27 \u2190 best!", options: { bold: true, color: C.teal, fontSize: 10 } },
  ],
];

s22.addTable(exTable, {
  x: 0.5, y: 1.7, w: 9.0,
  colW: [1.2, 2.0, 1.2, 1.8, 2.8],
  border: { pt: 0.5, color: C.gray300 },
  rowH: [0.32, 0.28, 0.28, 0.28, 0.28, 0.28, 0.28, 0.32],
  autoPage: false,
  fontSize: 10,
  fontFace: FONTS.body,
});

addPlainEnglishBox(s22, "Adding n4 has the highest utility gain (+1.2), so it gets 27% probability. But adding n1 also has a 31% chance. Bounded rationality means the best option usually wins, but not always.", 0.5, 4.3, 9.0, 0.75);

// --- SLIDE 23: The Utility Function ---
let s23 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s23, "The Utility Function: 10 Components");
addFooter(s23, "SECTION 3: How Time Works", 23);

s23.addText("What makes an activity attractive (or costly)?", {
  x: 0.5, y: 1.0, w: 9.0, h: 0.35,
  fontFace: FONTS.heading, fontSize: 16, color: C.navy, bold: true, margin: 0,
});

const utilTable = [
  [
    { text: "#", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 9, fontFace: FONTS.body } },
    { text: "Effect", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 9, fontFace: FONTS.body } },
    { text: "Plain English", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 9, fontFace: FONTS.body } },
    { text: "K Layer", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 9, fontFace: FONTS.body } },
  ],
  ["1", "outdegree (density)", "Baseline cost of doing anything", "K_AC"],
  ["2", "outdegree\u00B2 (scope cost)", "Diminishing returns to breadth", "K_AC"],
  ["3", "indegree popularity", "Benefit of choosing popular activities", "K_CA"],
  ["4", "indegree activity", "Intrinsic attractiveness of each activity", "K_CA"],
  ["5", "reciprocity", "Mutual adoption benefit", "K_AA"],
  ["6", "transitive triplets", "Do what your rival's rival does", "K_AA"],
  ["7", "nbrDist2 (indirect ties)", "Two-step network effects", "K_AA"],
  ["8", "egoX (actor covariate)", "Actor-specific strategy", "K_AC"],
  ["9", "altX (alter covariate)", "Activity-specific value", "K_CA"],
  ["10", "sameX (homophily)", "Birds of a feather", "K_AA"],
];

s23.addTable(utilTable, {
  x: 0.3, y: 1.45, w: 9.4,
  colW: [0.4, 2.2, 3.8, 1.0],
  border: { pt: 0.5, color: C.gray300 },
  rowH: [0.3, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26],
  autoPage: false,
  fontSize: 9,
  fontFace: FONTS.body,
});

addPlainEnglishBox(s23, "Each theta parameter is a dial you can turn. Together they define 'what does it mean to search well?' in your simulated economy.", 0.5, 4.5, 9.0, 0.65);

// --- SLIDE 24: WORKED EXAMPLE (Utility) ---
let s24 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s24, "Worked Example: Utility Decomposition");
addFooter(s24, "SECTION 3: How Time Works", 24);

s24.addText("Actor m1 considers adding activity n4. What happens to her utility?", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.35,
  fontFace: FONTS.body, fontSize: 13, color: C.gray700, margin: 0,
});

addCard(s24, 0.5, 1.6, 9.0, 2.5, C.white);
s24.addText([
  { text: "\u0394U(m1, +n4) =\n", options: { breakLine: true, fontFace: FONTS.code, fontSize: 12, color: C.navy, bold: true } },
  { text: "  \u03B81 \u00D7 (+1)          # scope increases by 1\n", options: { breakLine: true, fontFace: FONTS.code, fontSize: 11, color: C.gray600 } },
  { text: "+ \u03B82 \u00D7 (+7)          # scope\u00B2: 3\u00B2\u21924\u00B2 = +7\n", options: { breakLine: true, fontFace: FONTS.code, fontSize: 11, color: C.gray600 } },
  { text: "+ \u03B83 \u00D7 (3)           # n4 has 3 adopters (popularity)\n", options: { breakLine: true, fontFace: FONTS.code, fontSize: 11, color: C.gray600 } },
  { text: "+ \u03B85 \u00D7 (1)           # m2 also does n4 (reciprocity)\n", options: { breakLine: true, fontFace: FONTS.code, fontSize: 11, color: C.gray600 } },
  { text: "+ \u03B86 \u00D7 (2)           # 2 transitive triplets closed\n", options: { breakLine: true, fontFace: FONTS.code, fontSize: 11, color: C.gray600 } },
  { text: "= -2.0 + (-1.4) + 0.9 + 0.5 + 1.6  =  -0.4 + 3.0 = +1.2", options: { fontFace: FONTS.code, fontSize: 12, color: C.teal, bold: true } },
], {
  x: 0.7, y: 1.7, w: 8.6, h: 2.3, margin: 0, paraSpaceAfter: 2,
});

addPlainEnglishBox(s24, "Adding n4 is costly (scope goes up) but the synergies and network effects more than compensate. Net utility change = +1.2.", 0.5, 4.3, 9.0, 0.65);

// --- SLIDE 25: The Bounded Rationality Dial ---
let s25 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s25, "The Bounded Rationality Dial");
addFooter(s25, "SECTION 3: How Time Works", 25);

s25.addText("\u03B2 controls how 'smart' the actors are", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

// Three regime cards
const regimes = [
  { beta: "\u03B2 = 0", label: "Random", desc: "Pure noise. Actors choose uniformly at random. No optimization.", col: C.gray500 },
  { beta: "\u03B2 \u2248 2", label: "Bounded Rational", desc: "QRE regime. Better options more likely, but mistakes happen.", col: C.teal },
  { beta: "\u03B2 \u2192 \u221E", label: "Greedy (= NK)", desc: "Always pick best option. Recovers classic NK local search.", col: C.amber },
];
regimes.forEach((r, i) => {
  const rx = 0.5 + i * 3.15;
  addCard(s25, rx, 1.7, 2.85, 2.2, C.white);
  s25.addShape(pres.shapes.RECTANGLE, {
    x: rx, y: 1.7, w: 2.85, h: 0.06, fill: { color: r.col },
  });
  s25.addText(r.beta, {
    x: rx + 0.2, y: 1.85, w: 2.45, h: 0.35,
    fontFace: FONTS.code, fontSize: 16, color: r.col, bold: true, margin: 0,
  });
  s25.addText(r.label, {
    x: rx + 0.2, y: 2.25, w: 2.45, h: 0.35,
    fontFace: FONTS.heading, fontSize: 14, color: C.navy, bold: true, margin: 0,
  });
  s25.addText(r.desc, {
    x: rx + 0.2, y: 2.65, w: 2.45, h: 0.9,
    fontFace: FONTS.body, fontSize: 11, color: C.gray600, margin: 0,
  });
});

// Arrow underneath
s25.addShape(pres.shapes.LINE, {
  x: 0.8, y: 4.15, w: 8.4, h: 0,
  line: { color: C.gray400, width: 2 },
});
s25.addText("Random", {
  x: 0.5, y: 4.25, w: 2, h: 0.3,
  fontFace: FONTS.body, fontSize: 10, color: C.gray500, margin: 0,
});
s25.addText("Greedy", {
  x: 7.5, y: 4.25, w: 2, h: 0.3,
  fontFace: FONTS.body, fontSize: 10, color: C.gray500, align: "right", margin: 0,
});

addPlainEnglishBox(s25, "NK is the special case where beta = infinity. SaoMNK nests NK by letting beta be finite.", 0.5, 4.6, 9.0, 0.55);

// ============================================================
// SECTION 4: RUNNING SIMULATIONS (Slides 26-33)
// ============================================================

// --- Section Divider ---
let s26div = pres.addSlide({ masterName: "SECTION" });
s26div.addText("SECTION 4", {
  x: 0.5, y: 1.6, w: 9, h: 0.6,
  fontFace: FONTS.body, fontSize: 14, color: C.amber, bold: true, charSpacing: 4, margin: 0,
});
s26div.addText("Running Simulations", {
  x: 0.5, y: 2.6, w: 9, h: 0.9,
  fontFace: FONTS.heading, fontSize: 36, color: C.white, bold: true, margin: 0,
});
s26div.addText("From code to insight in three steps", {
  x: 0.5, y: 3.5, w: 9, h: 0.5,
  fontFace: FONTS.body, fontSize: 16, color: C.gray400, margin: 0,
});

// --- SLIDE 26: Three Steps ---
let s26 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s26, "Three Steps to a Simulation");
addFooter(s26, "SECTION 4: Running Simulations", 26);

const steps3 = [
  { num: "1", title: "ENV", desc: "Build the world\n(M, N, density, epistasis)", col: C.teal, fn: "saomnk_env()" },
  { num: "2", title: "MODEL", desc: "Set the rules\n(theta parameters, beta)", col: C.amber, fn: "saomnk_model()" },
  { num: "3", title: "RUN", desc: "Let it evolve\n(ministeps, trajectories)", col: C.green, fn: "saomnk_run()" },
];
steps3.forEach((st, i) => {
  const sx = 0.5 + i * 3.2;
  addCard(s26, sx, 1.3, 2.8, 2.8, C.white);
  s26.addShape(pres.shapes.OVAL, {
    x: sx + 1.0, y: 1.5, w: 0.7, h: 0.7,
    fill: { color: st.col },
  });
  s26.addText(st.num, {
    x: sx + 1.0, y: 1.5, w: 0.7, h: 0.7,
    fontFace: FONTS.heading, fontSize: 22, color: C.white,
    align: "center", valign: "middle", bold: true, margin: 0,
  });
  s26.addText(st.title, {
    x: sx + 0.2, y: 2.3, w: 2.4, h: 0.4,
    fontFace: FONTS.heading, fontSize: 16, color: C.navy, bold: true, align: "center", margin: 0,
  });
  s26.addText(st.desc, {
    x: sx + 0.2, y: 2.75, w: 2.4, h: 0.7,
    fontFace: FONTS.body, fontSize: 11, color: C.gray600, align: "center", margin: 0,
  });
  s26.addText(st.fn, {
    x: sx + 0.2, y: 3.5, w: 2.4, h: 0.35,
    fontFace: FONTS.code, fontSize: 11, color: st.col, align: "center", bold: true, margin: 0,
  });
  // Arrow after first two
  if (i < 2) {
    s26.addText("\u2192", {
      x: sx + 2.7, y: 2.2, w: 0.6, h: 0.6,
      fontFace: FONTS.body, fontSize: 28, color: C.gray400,
      align: "center", valign: "middle", margin: 0,
    });
  }
});

addPlainEnglishBox(s26, "Every searchnet simulation follows this pattern: create the world, define the rules, press play.", 0.5, 4.4, 9.0, 0.55);

// --- SLIDE 27: Code Walkthrough ---
let s27 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s27, "Code Walkthrough");
addFooter(s27, "SECTION 4: Running Simulations", 27);

addCodeBlock(s27,
  '# Step 1: Environment\n' +
  'env <- saomnk_env(M = 6, N = 10, density = 0.3,\n' +
  '                  seed = 42)\n\n' +
  '# Step 2: Model (set the theta parameters)\n' +
  'mod <- saomnk_model(env,\n' +
  '  theta_density  = -2.0,   # cost of breadth\n' +
  '  theta_scope2   = -0.2,   # quadratic scope cost\n' +
  '  theta_popular  =  0.3,   # popularity benefit\n' +
  '  theta_recip    =  0.5,   # reciprocity\n' +
  '  theta_trans    =  0.8,   # transitivity\n' +
  '  beta           =  2.0)   # bounded rationality\n\n' +
  '# Step 3: Run\n' +
  'res <- saomnk_run(mod, n_steps = 500)',
  0.5, 1.1, 9.0, 3.8
);

s27.addText("Each theta controls one term in the utility function. beta controls noise.", {
  x: 0.5, y: 5.0, w: 9.0, h: 0.3,
  fontFace: FONTS.body, fontSize: 11, color: C.gray500, italic: true, margin: 0,
});

// --- SLIDE 28: Reading the K-4 Panel ---
let s28 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s28, "Reading the {K}-4 Panel");
addFooter(s28, "SECTION 4: Running Simulations", 28);

s28.addText("The primary diagnostic: four time-series, one for each K layer.", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.35,
  fontFace: FONTS.body, fontSize: 13, color: C.gray700, margin: 0,
});

// Four mini panels
const panels = [
  { k: "K_AC (Scope)", desc: "Are firms expanding or contracting?", col: C.teal },
  { k: "K_CA (Popularity)", desc: "Is competition concentrating?", col: C.amber },
  { k: "K_AA (Sociality)", desc: "Are firms converging or diverging?", col: C.green },
  { k: "K_CC (Epistasis)", desc: "Are activities coupling or decoupling?", col: C.purple },
];
panels.forEach((p, i) => {
  const px = 0.5 + (i % 2) * 4.7;
  const py = 1.65 + Math.floor(i / 2) * 1.55;
  addCard(s28, px, py, 4.3, 1.3, C.white);
  s28.addShape(pres.shapes.RECTANGLE, {
    x: px, y: py, w: 0.08, h: 1.3, fill: { color: p.col },
  });
  s28.addText(p.k, {
    x: px + 0.25, y: py + 0.1, w: 3.8, h: 0.35,
    fontFace: FONTS.code, fontSize: 12, color: p.col, bold: true, margin: 0,
  });
  s28.addText(p.desc, {
    x: px + 0.25, y: py + 0.5, w: 3.8, h: 0.35,
    fontFace: FONTS.body, fontSize: 11, color: C.gray600, margin: 0,
  });
  // Mini trajectory line hint
  s28.addShape(pres.shapes.LINE, {
    x: px + 0.25, y: py + 1.0, w: 3.5, h: 0,
    line: { color: p.col, width: 1.5, dashType: "dash" },
  });
});

addPlainEnglishBox(s28, "The K-4 panel is your MRI scan of the simulated economy. Watch how the four layers co-evolve over time.", 0.5, 4.8, 9.0, 0.55);

// --- SLIDE 29: Network Snapshots ---
let s29 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s29, "Network Snapshots");
addFooter(s29, "SECTION 4: Running Simulations", 29);

s29.addText("Watch the bipartite network evolve over time.", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.35,
  fontFace: FONTS.body, fontSize: 14, color: C.gray700, margin: 0,
});

// Three snapshot frames
["t = 0", "t = 100", "t = 500"].forEach((t, i) => {
  const fx = 0.5 + i * 3.2;
  addCard(s29, fx, 1.7, 2.8, 2.5, C.white);
  s29.addText(t, {
    x: fx, y: 1.75, w: 2.8, h: 0.3,
    fontFace: FONTS.code, fontSize: 12, color: C.teal, bold: true, align: "center", margin: 0,
  });
  // Mini matrix dots
  for (let r = 0; r < 4; r++) {
    for (let c = 0; c < 6; c++) {
      const density = i === 0 ? 0.3 : i === 1 ? 0.4 : 0.5;
      const filled = Math.sin((r * 7 + c * 13 + i * 23) % 17) > (1 - density * 2) ? 1 : 0;
      s29.addShape(pres.shapes.RECTANGLE, {
        x: fx + 0.35 + c * 0.35, y: 2.2 + r * 0.35, w: 0.28, h: 0.28,
        fill: { color: filled ? C.teal : C.gray200 },
      });
    }
  }
  if (i < 2) {
    s29.addText("\u2192", {
      x: fx + 2.7, y: 2.4, w: 0.6, h: 0.6,
      fontFace: FONTS.body, fontSize: 24, color: C.gray400, align: "center", valign: "middle", margin: 0,
    });
  }
});

addPlainEnglishBox(s29, "Structure emerges from individual choices. The initial random matrix self-organizes into patterns.", 0.5, 4.4, 9.0, 0.55);

// --- SLIDE 30: Utility Decomposition ---
let s30 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s30, "Utility Decomposition: Who's Winning and Why");
addFooter(s30, "SECTION 4: Running Simulations", 30);

s30.addText("Break each actor's total utility into its component effects.", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.35,
  fontFace: FONTS.body, fontSize: 14, color: C.gray700, margin: 0,
});

// Stacked bar concept
addCard(s30, 0.5, 1.7, 9.0, 2.6, C.white);
const actors = ["m1", "m2", "m3", "m4", "m5", "m6"];
const effectColors = [C.teal, C.amber, C.green, C.purple, C.red];
const effectLabels = ["Scope cost", "Popularity", "Reciprocity", "Transitivity", "Covariates"];
actors.forEach((a, i) => {
  s30.addText(a, {
    x: 0.7, y: 1.95 + i * 0.35, w: 0.5, h: 0.3,
    fontFace: FONTS.code, fontSize: 10, color: C.gray600, valign: "middle", margin: 0,
  });
  let barX = 1.3;
  const widths = [0.8 + Math.random() * 0.5, 1.0 + Math.random() * 0.8, 0.6 + Math.random() * 0.4, 0.5 + Math.random() * 0.6, 0.3 + Math.random() * 0.3];
  widths.forEach((w, j) => {
    s30.addShape(pres.shapes.RECTANGLE, {
      x: barX, y: 1.95 + i * 0.35, w: w, h: 0.28,
      fill: { color: effectColors[j] },
    });
    barX += w;
  });
});

// Legend
effectLabels.forEach((el, i) => {
  const lx = 0.7 + i * 1.8;
  s30.addShape(pres.shapes.RECTANGLE, {
    x: lx, y: 4.1, w: 0.2, h: 0.15,
    fill: { color: effectColors[i] },
  });
  s30.addText(el, {
    x: lx + 0.25, y: 4.06, w: 1.5, h: 0.22,
    fontFace: FONTS.body, fontSize: 9, color: C.gray600, valign: "middle", margin: 0,
  });
});

addPlainEnglishBox(s30, "Decomposition reveals not just who wins, but which forces drive their advantage.", 0.5, 4.5, 9.0, 0.55);

// --- SLIDE 31: Adding Strategies ---
let s31 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s31, "Adding Strategies: Heterogeneous Actors");
addFooter(s31, "SECTION 4: Running Simulations", 31);

addCodeBlock(s31,
  '# Give actors different strategies via covariates\n' +
  'env <- saomnk_env(M = 6, N = 10, density = 0.3)\n\n' +
  '# Actor covariate: "aggressiveness"\n' +
  'env$egoX <- c(1, 1, 0, 0, -1, -1)\n' +
  '# 1 = aggressive, 0 = neutral, -1 = conservative\n\n' +
  'mod <- saomnk_model(env,\n' +
  '  theta_egoX = 0.5,  # aggressive actors expand\n' +
  '  ...)',
  0.5, 1.1, 9.0, 2.4
);

addCard(s31, 0.5, 3.7, 9.0, 1.0, C.white);
s31.addText([
  { text: "egoX: actor-level covariates (size, type, strategy)\n", options: { breakLine: true, bullet: true } },
  { text: "altX: activity-level covariates (attractiveness, cost)\n", options: { breakLine: true, bullet: true } },
  { text: "sameX: homophily effects (do actors of the same type cluster?)", options: { bullet: true } },
], {
  x: 0.7, y: 3.8, w: 8.6, h: 0.8,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0, paraSpaceAfter: 4,
});

addPlainEnglishBox(s31, "Covariates let you model heterogeneous strategies. Not all firms search the same way.", 0.5, 4.9, 9.0, 0.45);

// --- SLIDE 32: The Epistasis Matrix ---
let s32 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s32, "The Epistasis Matrix");
addFooter(s32, "SECTION 4: Running Simulations", 32);

// Two visual cards
addCard(s32, 0.5, 1.2, 4.2, 2.8, C.white);
s32.addText("Block-Diagonal = Modular", {
  x: 0.7, y: 1.3, w: 3.8, h: 0.35,
  fontFace: FONTS.heading, fontSize: 14, color: C.green, bold: true, margin: 0,
});
// Block diagonal visual
for (let i = 0; i < 6; i++) {
  for (let j = 0; j < 6; j++) {
    const inBlock = (i < 3 && j < 3) || (i >= 3 && j >= 3);
    s32.addShape(pres.shapes.RECTANGLE, {
      x: 1.0 + j * 0.45, y: 1.8 + i * 0.3, w: 0.4, h: 0.25,
      fill: { color: inBlock ? C.green : C.gray200 },
      transparency: inBlock ? 20 : 0,
    });
  }
}
s32.addText("Activities cluster into independent modules. Easy to optimize.", {
  x: 0.7, y: 3.65, w: 3.8, h: 0.3,
  fontFace: FONTS.body, fontSize: 10, color: C.gray600, margin: 0,
});

addCard(s32, 5.3, 1.2, 4.2, 2.8, C.white);
s32.addText("Full = Rugged", {
  x: 5.5, y: 1.3, w: 3.8, h: 0.35,
  fontFace: FONTS.heading, fontSize: 14, color: C.red, bold: true, margin: 0,
});
// Full matrix visual
for (let i = 0; i < 6; i++) {
  for (let j = 0; j < 6; j++) {
    s32.addShape(pres.shapes.RECTANGLE, {
      x: 5.8 + j * 0.45, y: 1.8 + i * 0.3, w: 0.4, h: 0.25,
      fill: { color: C.red },
      transparency: 10 + ((i * 3 + j * 7) % 40),
    });
  }
}
s32.addText("Everything depends on everything. Many local optima. Hard to optimize.", {
  x: 5.5, y: 3.65, w: 3.8, h: 0.3,
  fontFace: FONTS.body, fontSize: 10, color: C.gray600, margin: 0,
});

addPlainEnglishBox(s32, "The epistasis matrix controls landscape ruggedness -- just like in classic NK, but here it co-evolves with the actors.", 0.5, 4.2, 9.0, 0.55);

// --- SLIDE 33: Key Insight ---
let s33 = pres.addSlide({ masterName: "DARK_FULL" });
s33.addText("Key Insight", {
  x: 0.5, y: 0.5, w: 9.0, h: 0.5,
  fontFace: FONTS.body, fontSize: 14, color: C.amber, bold: true, charSpacing: 3, margin: 0,
});
s33.addText("The landscape is not fixed.", {
  x: 0.5, y: 1.3, w: 9.0, h: 0.8,
  fontFace: FONTS.heading, fontSize: 36, color: C.white, bold: true, margin: 0,
});
s33.addText("It evolves with the actors.", {
  x: 0.5, y: 2.1, w: 9.0, h: 0.8,
  fontFace: FONTS.heading, fontSize: 36, color: C.amberLight, bold: true, margin: 0,
});
s33.addShape(pres.shapes.RECTANGLE, {
  x: 0.5, y: 3.1, w: 2.0, h: 0.05, fill: { color: C.amber },
});
s33.addText("Every time an actor adds or drops an activity, the fitness landscape reshapes for everyone. This is what 'endogenous landscape' means.", {
  x: 0.5, y: 3.4, w: 9.0, h: 0.8,
  fontFace: FONTS.body, fontSize: 15, color: C.gray400, margin: 0,
});

// ============================================================
// SECTION 5: EXPERIMENTS & SHOCKS (Slides 34-38)
// ============================================================

// --- Section Divider ---
let s34div = pres.addSlide({ masterName: "SECTION" });
s34div.addText("SECTION 5", {
  x: 0.5, y: 1.6, w: 9, h: 0.6,
  fontFace: FONTS.body, fontSize: 14, color: C.amber, bold: true, charSpacing: 4, margin: 0,
});
s34div.addText("Experiments & Shocks", {
  x: 0.5, y: 2.6, w: 9, h: 0.9,
  fontFace: FONTS.heading, fontSize: 36, color: C.white, bold: true, margin: 0,
});
s34div.addText("Alternate realities, causal identification, and parameter sweeps", {
  x: 0.5, y: 3.5, w: 9, h: 0.5,
  fontFace: FONTS.body, fontSize: 16, color: C.gray400, margin: 0,
});

// --- SLIDE 34: The Simulation Factory ---
let s34 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s34, "The Simulation Factory");
addFooter(s34, "SECTION 5: Experiments & Shocks", 34);

s34.addText("Each run = one alternate reality", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

// Multiple parallel universes visual
for (let i = 0; i < 5; i++) {
  const uy = 1.8 + i * 0.55;
  const alpha = 15 + i * 5;
  addCard(s34, 0.5 + i * 0.1, uy, 8.5 - i * 0.2, 0.45, C.white);
  s34.addText("Run " + (i + 1) + ":  seed=" + (42 + i) + "  \u03B8_trans=" + (0.5 + i * 0.1).toFixed(1), {
    x: 0.8 + i * 0.1, y: uy + 0.05, w: 7.5, h: 0.35,
    fontFace: FONTS.code, fontSize: 10, color: C.gray600, valign: "middle", margin: 0,
  });
}

addCard(s34, 0.5, 4.0, 9.0, 0.8, "EBF8FF");
s34.addText([
  { text: "Same structure, different seeds \u2192 statistical distribution of outcomes\n", options: { breakLine: true, bullet: true } },
  { text: "Same structure, different thetas \u2192 comparative statics / counterfactuals", options: { bullet: true } },
], {
  x: 0.7, y: 4.05, w: 8.6, h: 0.7,
  fontFace: FONTS.body, fontSize: 12, color: C.gray700, margin: 0, paraSpaceAfter: 4,
});

// --- SLIDE 35: Exogenous Shocks ---
let s35 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s35, "Exogenous Shocks");
addFooter(s35, "SECTION 5: Experiments & Shocks", 35);

s35.addText('theta_shocks for "what if?" questions', {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

addCodeBlock(s35,
  '# Run 250 steps, then shock transitivity\n' +
  'res <- saomnk_run(mod, n_steps = 500,\n' +
  '  theta_shocks = list(\n' +
  '    list(step = 250,\n' +
  '         theta_trans = 0.0)  # kill clustering\n' +
  '  )\n' +
  ')',
  0.5, 1.7, 9.0, 1.8
);

addCard(s35, 0.5, 3.7, 9.0, 1.2, C.white);
s35.addText([
  { text: "Shocks change parameters mid-simulation (like a policy intervention)\n", options: { breakLine: true, bullet: true } },
  { text: "Multiple shocks can be sequenced at different time steps\n", options: { breakLine: true, bullet: true } },
  { text: "Pre-shock = baseline, post-shock = treatment (natural experiment logic)", options: { bullet: true } },
], {
  x: 0.7, y: 3.8, w: 8.6, h: 1.0,
  fontFace: FONTS.body, fontSize: 12, color: C.gray600, margin: 0, paraSpaceAfter: 4,
});

// --- SLIDE 36: Before/After Comparison ---
let s36 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s36, "Before / After: The Structural Break");
addFooter(s36, "SECTION 5: Experiments & Shocks", 36);

// Two K-4 panels side by side
["Before Shock (t < 250)", "After Shock (t > 250)"].forEach((title, idx) => {
  const bx = 0.5 + idx * 4.7;
  addCard(s36, bx, 1.2, 4.3, 3.0, C.white);
  s36.addText(title, {
    x: bx + 0.15, y: 1.3, w: 4.0, h: 0.3,
    fontFace: FONTS.heading, fontSize: 12, color: idx === 0 ? C.teal : C.red, bold: true, margin: 0,
  });
  // Mini trajectory lines
  const kCols = [C.teal, C.amber, C.green, C.purple];
  const kNames = ["K_AC", "K_CA", "K_AA", "K_CC"];
  kCols.forEach((kc, ki) => {
    const ly = 1.8 + ki * 0.55;
    s36.addText(kNames[ki], {
      x: bx + 0.1, y: ly, w: 0.6, h: 0.2,
      fontFace: FONTS.code, fontSize: 7, color: kc, margin: 0,
    });
    s36.addShape(pres.shapes.LINE, {
      x: bx + 0.7, y: ly + 0.1, w: 3.3, h: 0,
      line: { color: kc, width: 1.5, dashType: idx === 1 ? "dash" : "solid" },
    });
  });
});

// Vertical shock line annotation
s36.addShape(pres.shapes.LINE, {
  x: 5.0, y: 1.2, w: 0, h: 3.0,
  line: { color: C.red, width: 2, dashType: "dash" },
});
s36.addText("SHOCK", {
  x: 4.6, y: 4.25, w: 0.8, h: 0.25,
  fontFace: FONTS.code, fontSize: 9, color: C.red, bold: true, align: "center", margin: 0,
});

addPlainEnglishBox(s36, "The K-4 panel shows the structural break visually. Compare trajectories before and after the shock to identify causal effects.", 0.5, 4.6, 9.0, 0.55);

// --- SLIDE 37: From Simulation to Causal Inference ---
let s37 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s37, "From Simulation to Causal Inference");
addFooter(s37, "SECTION 5: Experiments & Shocks", 37);

s37.addText("DID / Synthetic Control / RD Pipeline", {
  x: 0.5, y: 1.1, w: 9.0, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.navy, bold: true, margin: 0,
});

const ciSteps = [
  { num: "1", title: "Generate Treatment", desc: "Run with shock at t*" },
  { num: "2", title: "Generate Control", desc: "Run without shock (same seed)" },
  { num: "3", title: "Apply DID/Synth/RD", desc: "Estimate causal effect of the shock" },
  { num: "4", title: "Repeat N times", desc: "Build distribution of treatment effects" },
];
ciSteps.forEach((ci, i) => {
  const cy = 1.7 + i * 0.75;
  addCard(s37, 0.5, cy, 9.0, 0.65, C.white);
  s37.addShape(pres.shapes.OVAL, {
    x: 0.7, y: cy + 0.1, w: 0.45, h: 0.45,
    fill: { color: C.teal },
  });
  s37.addText(ci.num, {
    x: 0.7, y: cy + 0.1, w: 0.45, h: 0.45,
    fontFace: FONTS.heading, fontSize: 16, color: C.white,
    align: "center", valign: "middle", bold: true, margin: 0,
  });
  s37.addText(ci.title, {
    x: 1.4, y: cy + 0.05, w: 3.0, h: 0.3,
    fontFace: FONTS.heading, fontSize: 13, color: C.navy, bold: true, margin: 0,
  });
  s37.addText(ci.desc, {
    x: 1.4, y: cy + 0.35, w: 7.5, h: 0.25,
    fontFace: FONTS.body, fontSize: 11, color: C.gray600, margin: 0,
  });
});

addPlainEnglishBox(s37, "The simulation gives you perfect experimental control. You know the true DGP, so you can validate your causal inference method.", 0.5, 4.75, 9.0, 0.55);

// --- SLIDE 38: Batch Experiments ---
let s38 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s38, "Batch Experiments: Parameter Sweeps");
addFooter(s38, "SECTION 5: Experiments & Shocks", 38);

addCodeBlock(s38,
  '# Define parameter grid\n' +
  'grid <- expand.grid(\n' +
  '  theta_trans = seq(0, 1, 0.2),\n' +
  '  theta_popular = seq(0, 0.5, 0.1),\n' +
  '  beta = c(1, 2, 5, 10)\n' +
  ')\n\n' +
  '# Run all combinations\n' +
  'results <- saomnk_experiment(\n' +
  '  env, grid,\n' +
  '  n_steps = 500,\n' +
  '  n_reps = 50,      # 50 replications each\n' +
  '  parallel = TRUE    # use all cores\n' +
  ')',
  0.5, 1.1, 9.0, 3.0
);

addPlainEnglishBox(s38, "Parameter sweeps let you map the entire response surface. Which combinations of theta produce concentration? Fragmentation? Collapse?", 0.5, 4.3, 9.0, 0.65);

// ============================================================
// SECTION 6: CONNECTING TO YOUR RESEARCH (Slides 39-43)
// ============================================================

// --- Section Divider ---
let s39div = pres.addSlide({ masterName: "SECTION" });
s39div.addText("SECTION 6", {
  x: 0.5, y: 1.6, w: 9, h: 0.6,
  fontFace: FONTS.body, fontSize: 14, color: C.amber, bold: true, charSpacing: 4, margin: 0,
});
s39div.addText("Connecting to Your Research", {
  x: 0.5, y: 2.6, w: 9, h: 0.9,
  fontFace: FONTS.heading, fontSize: 36, color: C.white, bold: true, margin: 0,
});
s39div.addText("From your research question to a searchnet simulation", {
  x: 0.5, y: 3.5, w: 9, h: 0.5,
  fontFace: FONTS.body, fontSize: 16, color: C.gray400, margin: 0,
});

// --- SLIDE 39: The Translation Table ---
let s39 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s39, "The Translation Table");
addFooter(s39, "SECTION 6: Connecting to Your Research", 39);

const ttData = [
  [
    { text: "Your Context", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 10, fontFace: FONTS.body } },
    { text: "Actors (M)", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 10, fontFace: FONTS.body } },
    { text: "Components (N)", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 10, fontFace: FONTS.body } },
    { text: "Key K", options: { bold: true, color: C.white, fill: { color: C.navy }, fontSize: 10, fontFace: FONTS.body } },
  ],
  ["Airlines", "Carriers", "Routes", "K_AA = route overlap"],
  ["Venture Capital", "VC firms", "Portfolio co's", "K_CC = syndication"],
  ["Academia", "Researchers", "Topics", "K_CC = field coupling"],
  ["Platforms", "Apps", "Features", "K_CA = feature crowding"],
  ["Pharma", "Firms", "Drug targets", "K_CC = target epistasis"],
  ["Strategy", "BUs", "Capabilities", "K_AC = scope"],
];

s39.addTable(ttData, {
  x: 0.5, y: 1.2, w: 9.0,
  colW: [1.8, 1.8, 2.0, 3.4],
  border: { pt: 0.5, color: C.gray300 },
  rowH: [0.35, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3],
  autoPage: false,
  fontSize: 10,
  fontFace: FONTS.body,
});

addPlainEnglishBox(s39, "Any context where agents choose from a shared set of options can be modeled as a bipartite SAOM.", 0.5, 3.8, 9.0, 0.55);

// --- SLIDE 40: Exercise ---
let s40 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s40, "Exercise: Map YOUR Research Question");
addFooter(s40, "SECTION 6: Connecting to Your Research", 40);

addCard(s40, 0.5, 1.2, 9.0, 3.5, "FFFBEB");
s40.addShape(pres.shapes.RECTANGLE, {
  x: 0.5, y: 1.2, w: 9.0, h: 0.06, fill: { color: C.amber },
});

s40.addText("5-Minute Individual Exercise", {
  x: 0.7, y: 1.35, w: 8.6, h: 0.4,
  fontFace: FONTS.heading, fontSize: 18, color: C.amber, bold: true, margin: 0,
});

const exercises = [
  "What is your research question?",
  "Who are the actors (M)? What are the components (N)?",
  "Which K layer matters most for your theory?",
  "What theta parameters capture your key mechanism?",
  "What shock would test your hypothesis?",
];
s40.addText(exercises.map((e, i) => ({
  text: (i + 1) + ". " + e + (i < exercises.length - 1 ? "\n" : ""),
  options: { breakLine: i < exercises.length - 1, fontSize: 14, color: C.gray700, fontFace: FONTS.body },
})), {
  x: 0.7, y: 1.9, w: 8.6, h: 2.5, margin: 0, paraSpaceAfter: 10,
});

s40.addText("Share with your neighbor. We'll discuss as a group.", {
  x: 0.5, y: 4.9, w: 9.0, h: 0.3,
  fontFace: FONTS.body, fontSize: 13, color: C.teal, italic: true, margin: 0,
});

// --- SLIDE 41: Where to Go Next ---
let s41 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s41, "Where to Go Next");
addFooter(s41, "SECTION 6: Connecting to Your Research", 41);

const resources = [
  { title: "Package Vignettes", desc: "Step-by-step tutorials for common use cases", icon: "1" },
  { title: "JSS Paper (forthcoming)", desc: "Formal treatment of SaoMNK theory and the searchnet implementation", icon: "2" },
  { title: "GitHub Repository", desc: "github.com/sdownin/searchnet -- issues, PRs, and development roadmap", icon: "3" },
  { title: "Online Appendix", desc: "Full proofs of Theorems 1-4 and additional simulation results", icon: "4" },
];
resources.forEach((r, i) => {
  const ry = 1.15 + i * 0.95;
  addCard(s41, 0.5, ry, 9.0, 0.8, C.white);
  s41.addShape(pres.shapes.OVAL, {
    x: 0.7, y: ry + 0.15, w: 0.5, h: 0.5,
    fill: { color: C.teal },
  });
  s41.addText(r.icon, {
    x: 0.7, y: ry + 0.15, w: 0.5, h: 0.5,
    fontFace: FONTS.heading, fontSize: 16, color: C.white,
    align: "center", valign: "middle", bold: true, margin: 0,
  });
  s41.addText(r.title, {
    x: 1.5, y: ry + 0.1, w: 7.5, h: 0.35,
    fontFace: FONTS.heading, fontSize: 14, color: C.navy, bold: true, margin: 0,
  });
  s41.addText(r.desc, {
    x: 1.5, y: ry + 0.45, w: 7.5, h: 0.3,
    fontFace: FONTS.body, fontSize: 11, color: C.gray500, margin: 0,
  });
});

// --- SLIDE 42: The Formal Foundation ---
let s42 = pres.addSlide({ masterName: "CONTENT" });
addContentTitle(s42, "The Formal Foundation: Theorems 1\u20134");
addFooter(s42, "SECTION 6: Connecting to Your Research", 42);

const theorems = [
  { num: "Theorem 1", title: "Nesting", desc: "NK is a special case of SaoMNK when M = 1 and \u03B2 \u2192 \u221E." },
  { num: "Theorem 2", title: "Coupling", desc: "The four K networks are algebraically determined by B. Changing one entry of B changes all four." },
  { num: "Theorem 3", title: "Equilibrium", desc: "The ministep process converges to a Quantal Response Equilibrium (QRE) for finite \u03B2." },
  { num: "Theorem 4", title: "Identification", desc: "The theta parameters are identified from panel data on B via conditional maximum likelihood." },
];
theorems.forEach((th, i) => {
  const ty = 1.15 + i * 0.95;
  addCard(s42, 0.5, ty, 9.0, 0.8, C.white);
  s42.addShape(pres.shapes.RECTANGLE, {
    x: 0.5, y: ty, w: 0.08, h: 0.8, fill: { color: C.amber },
  });
  s42.addText(th.num, {
    x: 0.75, y: ty + 0.1, w: 1.5, h: 0.3,
    fontFace: FONTS.code, fontSize: 12, color: C.amber, bold: true, margin: 0,
  });
  s42.addText(th.title, {
    x: 2.0, y: ty + 0.1, w: 2.0, h: 0.3,
    fontFace: FONTS.heading, fontSize: 13, color: C.navy, bold: true, margin: 0,
  });
  s42.addText(th.desc, {
    x: 0.75, y: ty + 0.4, w: 8.5, h: 0.3,
    fontFace: FONTS.body, fontSize: 11, color: C.gray600, margin: 0,
  });
});

addPlainEnglishBox(s42, "These four theorems establish that searchnet is not ad hoc. It has a rigorous foundation in game theory and econometrics.", 0.5, 4.95, 9.0, 0.55);

// --- SLIDE 43: Thank You ---
let s43 = pres.addSlide({ masterName: "DARK_FULL" });
s43.addText("Thank You", {
  x: 0.5, y: 0.8, w: 9.0, h: 0.9,
  fontFace: FONTS.heading, fontSize: 44, color: C.white, bold: true, margin: 0,
});
s43.addShape(pres.shapes.RECTANGLE, {
  x: 0.5, y: 1.8, w: 2.0, h: 0.05, fill: { color: C.amber },
});

s43.addText("Stephen Downing", {
  x: 0.5, y: 2.2, w: 9.0, h: 0.5,
  fontFace: FONTS.body, fontSize: 20, color: C.white, margin: 0,
});
s43.addText("University of Missouri  |  Trulaske College of Business", {
  x: 0.5, y: 2.7, w: 9.0, h: 0.4,
  fontFace: FONTS.body, fontSize: 14, color: C.gray400, margin: 0,
});

s43.addText([
  { text: "downings@missouri.edu\n", options: { breakLine: true, color: C.amberLight } },
  { text: "github.com/sdownin/searchnet\n", options: { breakLine: true, color: C.amberLight } },
  { text: "\nCite as: Downing, S. (2026). searchnet: Simulating Strategic\nSearch on Fitness Landscapes. R package.", options: { color: C.gray400, fontSize: 11 } },
], {
  x: 0.5, y: 3.3, w: 9.0, h: 1.5,
  fontFace: FONTS.body, fontSize: 14, margin: 0, paraSpaceAfter: 4,
});


// ============================================================
// WRITE FILE
// ============================================================
const outPath = "D:/Search_networks/SaoMNK/docs/searchnet_PDW_slides.pptx";
pres.writeFile({ fileName: outPath }).then(() => {
  console.log("Presentation saved to: " + outPath);
}).catch(err => {
  console.error("Error:", err);
});
