#!/usr/bin/env Rscript
###############################################################################
## build_submission.R
##
## Builds the Journal of Statistical Software submission for the searchnet
## paper, from the sources in this directory, and nothing else:
##
##   1. paper/searchnet-jss.pdf                  JSS-format manuscript
##                                               (rticles::jss_article)
##   2. paper/searchnet-jss-online-appendix.html online appendix
##   3. docs/paper/index.html                    public web version (--web)
##   4. paper/jss_submission/                    the complete bundle,
##                                               regenerated from scratch,
##                                               with MANIFEST.txt (sha256)
##      including code.R and code.html           the manuscript's R code
##                                               (knitr::purl) and its
##                                               knitr::spin() rendering with
##                                               sessionInfo(), as JSS asks;
##                                               see build_code_html.R
##
## Usage (from anywhere; paths resolve relative to this script):
##
##   Rscript paper/build_submission.R            full build: every chunk is
##                                               evaluated (slow; this is the
##                                               only build fit to submit)
##   Rscript paper/build_submission.R --fast     format check: chunks are not
##                                               evaluated, no package
##                                               tarball, no code.html;
##                                               MANIFEST says so
##   Rscript paper/build_submission.R --preprint  preprint (e.g. arXiv): the
##                                               same manuscript with jss.cls
##                                               `nojss` (no JSS masthead) into
##                                               paper/preprint/; combine with
##                                               --fast for a format check
##   options:  --no-appendix  --no-web  --no-tarball  --no-code
##
## The package is taken from the repository root with pkgload::load_all(), so
## the paper always documents the code it ships with, not whatever version
## happens to be installed. (The pre-2026-10-07 script sourced
## R/saomnk-loader.R, which no longer exists, and setwd() to a fixed D:/ path.)
##
## CONTENT GATE. Every text file written into the bundle, the web page, and
## the contents of the package tarball are scanned against
## tools/embargo-content-patterns.txt, the same list the release gate
## (tools/check_public_snapshot.R, section 3) uses. A hit stops the build with
## a non-zero exit status and the offending file:line list. The scan cannot
## read PDFs or images; it reads the .tex the PDF is compiled from instead.
## This guard blocks by design: it prevents an outbound action, it does not
## measure the author's work.
###############################################################################

## ---------------------------------------------------------------- locate -- #
.script_path <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) return(normalizePath(f[1], winslash = "/", mustWork = TRUE))
  o <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(o)) return(normalizePath(o, winslash = "/", mustWork = TRUE))
  stop("Run with Rscript or source(); cannot locate build_submission.R.")
})
PAPER  <- dirname(.script_path)
ROOT   <- dirname(PAPER)
BUNDLE <- file.path(PAPER, "jss_submission")
WEBDIR <- file.path(ROOT, "docs", "paper")

if (!file.exists(file.path(ROOT, "DESCRIPTION")))
  stop("No DESCRIPTION at ", ROOT, "; build_submission.R must live in paper/.")

args <- commandArgs(trailingOnly = TRUE)
FAST        <- "--fast" %in% args
DO_APPENDIX <- !"--no-appendix" %in% args
DO_WEB      <- !"--no-web" %in% args
DO_TARBALL  <- !FAST && !"--no-tarball" %in% args
DO_CODE     <- !FAST && !"--no-code" %in% args
PREPRINT    <- "--preprint" %in% args

say <- function(...) cat(sprintf("[build] %s\n", paste0(...)))
need <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("Missing packages: ", paste(miss, collapse = ", "),
                         call. = FALSE)
}
need(c("rmarkdown", "knitr", "rticles", "digest", "tinytex"))

Sys.setenv(SEARCHNET_JSS_FAST = if (FAST) "1" else "")
say("mode: ", if (FAST) "FAST (chunks not evaluated; not for submission)"
              else "FULL (all chunks evaluated)")

## ------------------------------------------------------------ the gate --- #
EMBARGO_FILE <- file.path(ROOT, "tools", "embargo-content-patterns.txt")
.read_patterns <- function(path) {
  if (!file.exists(path)) return(character())
  p <- trimws(readLines(path, warn = FALSE))
  p[nzchar(p) & !startsWith(p, "#")]
}
EMBARGO <- .read_patterns(EMBARGO_FILE)
if (!length(EMBARGO))
  stop("Cannot read ", EMBARGO_FILE, ": refusing to build a submission ",
       "without the content scan.", call. = FALSE)

.binary_ext <- paste0("[.](png|jpe?g|gif|pdf|mp4|webm|rds|rda|RData|pptx|",
                      "docx|xlsx|zip|gz|tgz|ico|woff2?|ttf|otf)$")

## Returns a character vector of "file:line  pattern" hits; empty when clean.
## `label` maps each file to the name reported (relative path).
scan_embargo <- function(files, label = files) {
  hits <- character()
  for (k in seq_along(files)) {
    f <- files[k]
    for (p in EMBARGO)
      if (grepl(p, label[k], perl = TRUE))
        hits <- c(hits, sprintf("%s (path)  %s", label[k], p))
    if (grepl(.binary_ext, f, ignore.case = TRUE)) next
    txt <- tryCatch(suppressWarnings(readLines(f, warn = FALSE,
                                               encoding = "UTF-8")),
                    error = function(e) character())
    if (!length(txt)) next
    ## Self-contained HTML embeds fonts and images as base64 data URIs, whose
    ## random letters match short word patterns (three-letter ones hit
    ## base64-encoded fonts).
    ## Base64 is not readable text; images cannot be scanned here anyway.
    txt <- gsub("data:[a-zA-Z0-9.+/-]+;base64,[A-Za-z0-9+/=]+", "data:",
                txt, perl = TRUE, useBytes = TRUE)
    for (p in EMBARGO) {
      ln <- which(grepl(p, txt, perl = TRUE, useBytes = TRUE))
      if (length(ln))
        hits <- c(hits, sprintf("%s:%d  %s", label[k], ln, p))
    }
  }
  hits
}
gate <- function(files, label, what) {
  hits <- scan_embargo(files, label)
  if (length(hits)) {
    cat("\n[FAIL] content gate: ", length(hits), " embargoed match(es) in ",
        what, ":\n", sep = "")
    cat(paste0("    ", hits), sep = "\n")
    cat("\nNothing may be submitted or published until these are removed",
        "from the SOURCES and the build is rerun.\n")
    quit(save = "no", status = 1)
  }
  say("content gate: ", what, " clean (", length(files), " files)")
}

## Gate the sources first: a hit here would only be copied downstream.
src_rmd <- file.path(PAPER, c("searchnet-jss.Rmd",
                              "searchnet-jss-online-appendix.Rmd",
                              "searchnet.bib"))
gate(src_rmd, basename(src_rmd), "paper sources")

## ----------------------------------------------------------- the package -- #
if (!FAST) {
  need("pkgload")
  say("loading searchnet from ", ROOT, " with pkgload::load_all()")
  pkgload::load_all(ROOT, quiet = TRUE, export_all = FALSE)
}
sn_version <- unname(read.dcf(file.path(ROOT, "DESCRIPTION"),
                              fields = "Version")[1, 1])
say("searchnet ", sn_version, ", RSiena ",
    as.character(utils::packageVersion("RSiena")), ", ", R.version.string)

render_in <- function(input, ...) {
  ## A fresh environment per document, so chunks cannot see each other.
  rmarkdown::render(input, envir = new.env(parent = globalenv()),
                    quiet = TRUE, ...)
}

## ------------------------------------------------------- 1. manuscript --- #
## Path of `x` relative to directory `base` (no regex: Windows paths).
relpath <- function(x, base) substring(x, nchar(base) + 2L)
TMP <- normalizePath(tempdir(), winslash = "/")

## The official JSS class and bibliography style, as shipped with rticles.
## Copied fresh each build so the bundle carries the files the .tex needs.
jss_res <- system.file("rmarkdown", "templates", "jss", "skeleton",
                       package = "rticles")
for (f in c("jss.cls", "jss.bst", "jsslogo.jpg"))
  if (!file.copy(file.path(jss_res, f), file.path(PAPER, f), overwrite = TRUE))
    stop("cannot copy ", f, " from rticles", call. = FALSE)

## Stale auxiliary files from an interrupted run break the next one.
unlink(file.path(PAPER, paste0("searchnet-jss.",
                               c("aux", "out", "log", "bbl", "blg", "toc"))))
## knitr caches as well, on a full build. `api-run` modifies the R6 `env` in
## place under cache = TRUE, so a cache hit skips the run and leaves `env`
## unsimulated; the next chunk then fails (seen 2026-10-07). A cache also
## survives a package change it cannot see.
if (!FAST)
  unlink(file.path(PAPER, c("searchnet-jss_cache",
                            "searchnet-jss-online-appendix_cache")),
         recursive = TRUE)

## --------------------------------------------- preprint (--preprint) --- #
## A preprint (e.g. arXiv) must not carry the JSS masthead: jss.cls's `nojss`
## option keeps the layout and drops the journal header, volume/issue
## placeholders and DOI. Built into paper/preprint/ (gitignored) as the PDF
## plus a self-contained source set (.tex, .bib, jss.cls/.bst, figures), and
## nothing else: the submission bundle, web page and tarball are untouched.
if (PREPRINT) {
  say("rendering the PREPRINT (jss.cls nojss) -> paper/preprint/")
  old_wd <- setwd(PAPER)
  on.exit(setwd(old_wd), add = TRUE)
  unlink(file.path(PAPER, paste0("searchnet-jss-preprint.",
                                 c("aux", "out", "log", "bbl", "blg", "toc"))))
  pp_pdf <- normalizePath(
    render_in("searchnet-jss.Rmd",
              output_file = "searchnet-jss-preprint.pdf",
              output_format = rticles::jss_article(
                keep_tex = TRUE,
                pandoc_args = c("--variable", "classoption=nojss"))),
    winslash = "/")
  pp_tex <- file.path(PAPER, "searchnet-jss-preprint.tex")
  if (!file.exists(pp_tex) || !any(grepl("nojss", readLines(pp_tex, n = 20))))
    stop("preprint .tex missing or not built with the nojss option", call. = FALSE)
  if (any(grepl(PAPER, readLines(pp_tex, warn = FALSE), fixed = TRUE)))
    stop("the preprint .tex contains absolute paths under ", PAPER, call. = FALSE)
  PP <- file.path(PAPER, "preprint")
  unlink(PP, recursive = TRUE); dir.create(PP)
  ## Every \includegraphics target, parsed with balanced braces (the optional
  ## argument carries alt text with its own braces), then resolved to the file
  ## on disk: LaTeX drops the extension, so try the graphic extensions.
  .tex <- paste(readLines(pp_tex, warn = FALSE), collapse = "\n")
  .targets <- character()
  for (st in gregexpr("\\\\includegraphics", .tex, perl = TRUE)[[1]]) {
    if (st < 0) break
    k <- st + nchar("\\includegraphics")
    ch <- function(i) substr(.tex, i, i)
    if (ch(k) == "[") {                       # skip [ ... ] with nested braces
      depth <- 0L
      repeat {
        c1 <- ch(k)
        if (c1 == "{") depth <- depth + 1L
        if (c1 == "}") depth <- depth - 1L
        if (c1 == "]" && depth == 0L) break
        k <- k + 1L
      }
      k <- k + 1L
    }
    if (ch(k) != "{") next
    e <- regexpr("}", substr(.tex, k + 1L, nchar(.tex)), fixed = TRUE)
    .targets <- c(.targets, substr(.tex, k + 1L, k + e - 1L))
  }
  figs <- character()
  for (t in unique(.targets)) {
    cand <- c(t, paste0(t, c(".pdf", ".png", ".jpg", ".jpeg")))
    hit <- cand[file.exists(file.path(PAPER, cand))]
    if (!length(hit)) stop("preprint figure not found on disk: ", t, call. = FALSE)
    figs <- c(figs, hit[1])
  }
  for (f in c(basename(pp_pdf), basename(pp_tex), "searchnet.bib",
              "jss.cls", "jss.bst", "jsslogo.jpg", figs)) {
    src <- file.path(PAPER, f)
    if (!file.exists(src)) next
    dir.create(dirname(file.path(PP, f)), recursive = TRUE, showWarnings = FALSE)
    file.copy(src, file.path(PP, f), overwrite = TRUE)
  }
  pf <- list.files(PP, recursive = TRUE, full.names = TRUE)
  gate(pf, relpath(pf, PP), "the preprint")
  say("  -> ", file.path(PP, basename(pp_pdf)), " (", length(pf), " files)")
  say("done -- PREPRINT build (no JSS masthead); not the submission bundle")
  quit(save = "no", status = 0)
}

say("rendering the JSS manuscript (rticles::jss_article)")
old_wd <- setwd(PAPER)               # knitr resolves figures/ relative to the
on.exit(setwd(old_wd), add = TRUE)   # .Rmd; restored on exit
## Rendered by relative name from inside paper/, with no output_dir: given
## absolute paths, rmarkdown copies every image into searchnet-jss_files/ and
## writes absolute D:/... paths into the .tex, which then compiles nowhere
## else and leaks the local directory layout.
pdf_out <- normalizePath(
  render_in("searchnet-jss.Rmd",
            output_format = rticles::jss_article(keep_tex = TRUE)),
  winslash = "/")
tex_out <- file.path(PAPER, "searchnet-jss.tex")
if (any(grepl(PAPER, readLines(tex_out, warn = FALSE), fixed = TRUE)))
  stop("the .tex contains absolute paths under ", PAPER, call. = FALSE)
if (!file.exists(pdf_out) || !file.exists(tex_out))
  stop("manuscript render produced no PDF/TeX", call. = FALSE)
say("  -> ", pdf_out)

## -------------------------------------------------- 2. online appendix --- #
app_out <- NULL
if (DO_APPENDIX) {
  say("rendering the online appendix")
  app_out <- normalizePath(
    render_in("searchnet-jss-online-appendix.Rmd"), winslash = "/")
  say("  -> ", app_out)
}

## ------------------------------------------------------- 3. web version -- #
## The public web page is built from the same .tex the PDF is compiled from,
## so it cannot drift from the manuscript. pandoc reads LaTeX tables, math,
## \label/\ref and natbib citations directly; the JSS front-matter macros are
## replaced by plain definitions it understands.
if (DO_WEB) {
  say("rendering the web version into docs/paper/")
  dir.create(WEBDIR, recursive = TRUE, showWarnings = FALSE)
  tex <- readLines(tex_out, encoding = "UTF-8", warn = FALSE)
  b <- grep("^\\\\begin\\{document\\}", tex); e <- grep("^\\\\end\\{document\\}", tex)
  body <- tex[(b + 1):(e - 1)]
  body <- body[!grepl("^\\\\bibliography\\{", body)]
  meta_yaml <- rmarkdown::yaml_front_matter(file.path(PAPER, "searchnet-jss.Rmd"))
  macros <- c(
    "\\newcommand{\\pkg}[1]{\\textbf{#1}}",
    "\\newcommand{\\proglang}[1]{#1}",
    "\\newcommand{\\code}[1]{\\texttt{#1}}",
    "\\newcommand{\\K}{\\{K\\}}",
    "\\newcommand{\\fct}[1]{\\texttt{#1()}}",
    "\\newcommand{\\email}[1]{#1}",
    "\\newcommand{\\pandocbounded}[1]{#1}")
  web_tex <- file.path(TMP, "searchnet-jss-web.tex")
  writeLines(c("\\documentclass{article}", macros, "\\begin{document}", body,
               "\\end{document}"), web_tex, useBytes = TRUE)
  abstract <- gsub("\\\\pkg\\{([^}]*)\\}", "**\\1**", meta_yaml$abstract)
  abstract <- gsub("\\\\proglang\\{([^}]*)\\}", "\\1", abstract)
  abstract <- gsub("\\\\code\\{([^}]*)\\}", "`\\1`", abstract)
  abstract <- gsub("\\\\_", "_", abstract)
  web_meta <- file.path(TMP, "searchnet-jss-web-meta.yaml")
  writeLines(c("---",
               paste0("title: \"", meta_yaml$title$plain, "\""),
               "author: \"Stephen Downing, University of Missouri\"",
               paste0("subtitle: \"Manuscript prepared for the Journal of ",
                      "Statistical Software. searchnet ", sn_version, ".\""),
               "abstract: |",
               paste0("  ", strwrap(abstract, 76)),
               "---"), web_meta)
  web_html <- file.path(WEBDIR, "index.html")
  css <- file.path(TMP, "searchnet-web.css")
  writeLines(c(
    "body{max-width:52em;margin:2em auto;padding:0 1em;line-height:1.5;",
    "font-family:Georgia,serif;color:#222;background:#fff}",
    "img{max-width:100%;height:auto}table{border-collapse:collapse;margin:1em auto}",
    "td,th{padding:.2em .6em;border-bottom:1px solid #ccc}",
    "pre{background:#f6f6f6;padding:.6em;overflow-x:auto}",
    "figcaption,caption{font-size:.9em;color:#444}"), css)
  ## Figures are embedded, so the page has no dependency on paper/.
  rmarkdown::pandoc_convert(
    web_tex, to = "html5", from = "latex", output = web_html,
    wd = PAPER,
    options = c("--standalone", "--embed-resources", "--mathjax",
                "--citeproc", paste0("--bibliography=",
                                     file.path(PAPER, "searchnet.bib")),
                "--metadata-file", web_meta, "--css", css,
                "--number-sections", "--toc", "--toc-depth=2",
                "--resource-path", PAPER,
                "--metadata", "link-citations=true",
                "--metadata", "pagetitle=searchnet (JSS manuscript)"))
  if (!file.exists(web_html)) stop("web render failed", call. = FALSE)
  say("  -> ", web_html)
  gate(web_html, "docs/paper/index.html", "the web version")
}

## ------------------------------------------------------------- 4. bundle -- #
say("regenerating the bundle in ", BUNDLE)
old <- list.files(BUNDLE, recursive = TRUE, all.files = TRUE,
                  full.names = TRUE, no.. = TRUE)
unlink(old, recursive = TRUE, force = TRUE)
unlink(list.dirs(BUNDLE, recursive = TRUE)[-1], recursive = TRUE)
dir.create(BUNDLE, showWarnings = FALSE)

cp <- function(from, to_rel) {
  to <- file.path(BUNDLE, to_rel)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(from, to, overwrite = TRUE, copy.date = TRUE))
    stop("copy failed: ", from, call. = FALSE)
}

## Manuscript sources and outputs, plus the JSS class files the .tex needs.
cp(file.path(PAPER, "searchnet-jss.Rmd"), "searchnet-jss.Rmd")
cp(tex_out,                               "searchnet-jss.tex")
cp(pdf_out,                               "searchnet-jss.pdf")
cp(file.path(PAPER, "searchnet.bib"),     "searchnet.bib")
for (f in c("jss.cls", "jss.bst", "jsslogo.jpg"))
  if (file.exists(file.path(PAPER, f))) cp(file.path(PAPER, f), f)

## Figures actually used: static images referenced by the .tex, and the
## knitted chunk figures (the .tex points into searchnet-jss_files/).
tex_txt <- paste(readLines(tex_out, warn = FALSE), collapse = "\n")
inc <- regmatches(tex_txt, gregexpr("\\\\includegraphics(\\[[^]]*\\])?\\{[^}]+\\}",
                                    tex_txt))[[1]]
inc <- unique(sub(".*\\{([^}]+)\\}$", "\\1", inc))
for (f in inc) {
  src <- file.path(PAPER, f)
  if (!file.exists(src)) {
    alt <- Sys.glob(paste0(src, ".*"))
    if (!length(alt)) stop("figure referenced by the .tex not found: ", f,
                           call. = FALSE)
    src <- alt[1]; f <- paste0(f, sub(".*(\\.[^.]+)$", "\\1", src))
  }
  cp(src, f)
}
say("  figures copied: ", length(inc))

if (!is.null(app_out)) {
  cp(file.path(PAPER, "searchnet-jss-online-appendix.Rmd"),
     "searchnet-jss-online-appendix.Rmd")
  cp(app_out, "searchnet-jss-online-appendix.html")
}

## Replication materials: reproduce_all.R and every script it sources
## (recursively), plus the remaining replication scripts and the README.
REPL <- file.path(PAPER, "replication")
sourced <- function(f) {
  x <- readLines(f, warn = FALSE)
  s <- regmatches(x, regexpr("source\\([^)]*\"[^\"]+\\.R\"", x))
  basename(sub(".*\"([^\"]+\\.R)\"$", "\\1", s))
}
todo <- "reproduce_all.R"; got <- character()
while (length(todo)) {
  f <- todo[1]; todo <- todo[-1]
  if (f %in% got) next
  p <- file.path(REPL, f)
  if (!file.exists(p)) stop("replication script sourced but missing: ", f,
                            call. = FALSE)
  got <- c(got, f); todo <- c(todo, setdiff(sourced(p), got))
}
repl_files <- union(got, list.files(REPL, pattern = "\\.(R|md)$"))
for (f in repl_files) cp(file.path(REPL, f), file.path("replication", f))
say("  replication files: ", paste(repl_files, collapse = ", "))

## JSS replication code: code.R (every manuscript chunk, via knitr::purl) and
## code.html (its knitr::spin() rendering, ending with sessionInfo()). This
## re-runs every chunk, so it is part of the full build only. Rendered in TMP;
## only the two files enter the bundle, and they are gated here and again
## with the whole bundle below.
if (DO_CODE) {
  say("writing code.R and code.html (knitr::purl, knitr::spin)")
  source(file.path(PAPER, "build_code_html.R"), local = TRUE)
  code_files <- make_code_html(file.path(PAPER, "searchnet-jss.Rmd"),
                               out_dir = BUNDLE,
                               work_dir = file.path(TMP, "code-html"))
  gate(code_files, basename(code_files), "code.R and code.html")
  say("  -> ", paste(basename(code_files), collapse = ", "))
}

## The package source, as JSS requires. Built without vignettes (their
## sources are in the tarball); the tarball's contents are gated too.
if (DO_TARBALL) {
  need("pkgbuild")
  say("building the package source tarball")
  ## Build from the PUBLIC snapshot of HEAD, not the dev working tree: the
  ## submission ships what the public release ships, so every path listed in
  ## .public-exclude (entries ending in "/" are directories) is dropped first.
  snap <- file.path(TMP, "public-snapshot")
  unlink(snap, recursive = TRUE)
  system2("git", c("-C", shQuote(ROOT), "worktree", "add", "--detach",
                   shQuote(snap), "HEAD"), stdout = FALSE, stderr = FALSE)
  on.exit(system2("git", c("-C", shQuote(ROOT), "worktree", "remove",
                           "--force", shQuote(snap)),
                  stdout = FALSE, stderr = FALSE), add = TRUE)
  if (!dir.exists(snap)) stop("could not create the public snapshot worktree")
  excl <- trimws(readLines(file.path(ROOT, ".public-exclude"), warn = FALSE))
  excl <- excl[nzchar(excl) & !startsWith(excl, "#")]
  unlink(file.path(snap, excl), recursive = TRUE)
  tb <- pkgbuild::build(snap, dest_path = TMP, vignettes = FALSE,
                        manual = FALSE, quiet = TRUE)
  xdir <- file.path(TMP, "tarball-scan")
  unlink(xdir, recursive = TRUE); dir.create(xdir)
  utils::untar(tb, exdir = xdir)
  tf <- list.files(xdir, recursive = TRUE, full.names = TRUE)
  gate(tf, relpath(tf, xdir), basename(tb))
  cp(tb, basename(tb))
}

## Gate everything in the bundle before writing the manifest.
bf <- list.files(BUNDLE, recursive = TRUE, full.names = TRUE)
gate(bf, relpath(bf, BUNDLE), "the submission bundle")

## ----------------------------------------------------------- MANIFEST ---- #
rel <- sort(relpath(bf, BUNDLE))
sha <- vapply(file.path(BUNDLE, rel),
              function(f) digest::digest(file = f, algo = "sha256"),
              character(1))
commit <- tryCatch(system2("git", c("-C", shQuote(ROOT), "rev-parse",
                                    "--short", "HEAD"), stdout = TRUE,
                           stderr = FALSE), error = function(e) "unknown")
dirty <- tryCatch(length(system2("git", c("-C", shQuote(ROOT), "status",
                                          "--porcelain"), stdout = TRUE,
                                 stderr = FALSE)) > 0, error = function(e) NA)
writeLines(c(
  "searchnet JSS submission bundle -- MANIFEST",
  paste0("build mode : ", if (FAST)
    "FAST (chunks NOT evaluated; format check only -- NOT FOR SUBMISSION)"
    else "FULL (all chunks evaluated)"),
  paste0("built      : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("git commit : ", paste(commit, collapse = ""),
         if (isTRUE(dirty)) " (working tree had uncommitted changes)" else ""),
  paste0("searchnet  : ", sn_version),
  paste0("RSiena     : ", as.character(utils::packageVersion("RSiena"))),
  paste0("R          : ", R.version.string),
  paste0("content gate: passed (", basename(EMBARGO_FILE), ", ",
         length(EMBARGO), " patterns)"),
  paste0("code.html  : ", if (DO_CODE)
    "included (code.R via knitr::purl, rendered with knitr::spin)"
    else "NOT included (--fast or --no-code)"),
  "",
  "sha256                                                            file",
  paste(sha, rel, sep = "  ")),
  file.path(BUNDLE, "MANIFEST.txt"))
say("MANIFEST.txt written (", length(rel), " files)")
say("done", if (FAST) " -- FAST build: rerun without --fast before submitting" else "")
