#!/usr/bin/env Rscript
###############################################################################
## check_public_snapshot.R
##
## Release gate. Fails if a snapshot about to be published carries any path
## listed in .public-exclude, or any path matching the internal-artifact
## patterns that reached the public repo before 2026-08-24.
##
## Usage, from inside the snapshot worktree (checked out on public-release,
## tree already loaded via `git read-tree --reset -u <tag>`):
##
##     Rscript tools/check_public_snapshot.R [<worktree-path>] [<exclude-file>]
##
## Exit status 0 = safe to publish. Exit status 1 = do not publish.
##
## THIS CHECK BLOCKS, AND THAT IS DELIBERATE.
## It is a prevention guard on outbound publication, not a measurement of the
## author's own work: it owns no work product, it evaluates an action in
## flight rather than something already delivered, and it proposes undoing
## nothing. It has one determinate fix -- remove the file from the snapshot.
###############################################################################

args    <- commandArgs(trailingOnly = TRUE)
wt      <- if (length(args) >= 1) args[[1]] else "."
exclude <- if (length(args) >= 2) args[[2]] else file.path(wt, ".public-exclude")
## .public-exclude lists itself (it names what it withholds), so a snapshot
## worktree may not carry it. Fall back to the dev checkout running the gate.
if (!file.exists(exclude) && length(args) < 2 && file.exists(".public-exclude"))
  exclude <- normalizePath(".public-exclude")

## ---------------------------------------------------------------------------
## Patterns for content that must never ship, independent of the list file.
## The list file covers files we chose to keep private; these patterns catch
## files nobody remembered to list. Both failed once, so both are checked.
##
## Read from tools/internal-file-patterns.txt, which the pre-commit hook reads
## too. One list, deliberately: two copies of a pattern set drift, and a guard
## that has drifted still reports success.
## ---------------------------------------------------------------------------
.read_patterns <- function(path) {
  if (!file.exists(path)) return(NULL)
  p <- trimws(readLines(path, warn = FALSE))
  p[nzchar(p) & !startsWith(p, "#")]
}

## Look next to this script first, then in the worktree being checked.
.self_dir <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f[1])) else NA_character_
}, error = function(e) NA_character_)

PATTERN_FILE <- NULL
for (cand in c(if (!is.na(.self_dir)) file.path(.self_dir, "internal-file-patterns.txt"),
               file.path(wt, "tools", "internal-file-patterns.txt"),
               "tools/internal-file-patterns.txt")) {
  if (!is.null(cand) && file.exists(cand)) { PATTERN_FILE <- cand; break }
}

ARTIFACT_PATTERNS <- .read_patterns(PATTERN_FILE)
if (is.null(ARTIFACT_PATTERNS) || !length(ARTIFACT_PATTERNS))
  stop("cannot read internal-file-patterns.txt; refusing to certify a ",
       "snapshot with no pattern list. Looked next to this script and under ",
       wt, "/tools/.", call. = FALSE)

fail <- function(...) { cat("\n[FAIL] ", ..., "\n", sep = ""); quit(status = 1) }

if (!dir.exists(wt)) fail("worktree not found: ", wt)

tracked <- system2("git", c("-C", shQuote(wt), "ls-files"), stdout = TRUE)
if (!length(tracked)) fail("no tracked files found in ", wt, " -- wrong path?")

problems <- list()

## -- 1. explicit exclusion list --------------------------------------------- #
if (file.exists(exclude)) {
  lines <- readLines(exclude, warn = FALSE)
  lines <- trimws(lines)
  listed <- lines[nzchar(lines) & !startsWith(lines, "#")]
  ## An entry ending in "/" withholds everything under that directory.
  dirs <- listed[endsWith(listed, "/")]
  hit <- union(intersect(listed, tracked),
               tracked[vapply(tracked, function(t) any(startsWith(t, dirs)), logical(1))])
  if (length(hit))
    problems[["listed in .public-exclude"]] <- hit
} else {
  cat("[warn] no .public-exclude found at ", exclude,
      " -- pattern checks still apply\n", sep = "")
}

## -- 2. artifact patterns ---------------------------------------------------- #
for (p in ARTIFACT_PATTERNS) {
  hit <- grep(p, tracked, value = TRUE)
  if (length(hit))
    problems[[paste0("matches internal pattern ", p)]] <- hit
}

## -- 3. embargoed CONTENT ----------------------------------------------------- #
## Sections 1-2 judge paths. They cannot see a file that is meant to be public
## but carries text that must not be: that is how unpublished results reached
## every public tag from v0.2.0 to v0.10.0. This section reads
## the files. The pattern list is private (it names what it guards), so it is
## looked up next to this script, i.e. in the dev checkout running the gate,
## and the gate refuses to certify anything when it cannot find it.
EMBARGO_FILE <- NULL
for (cand in c(if (!is.na(.self_dir)) file.path(.self_dir, "embargo-content-patterns.txt"),
               "tools/embargo-content-patterns.txt")) {
  if (!is.null(cand) && file.exists(cand)) { EMBARGO_FILE <- cand; break }
}
EMBARGO <- .read_patterns(EMBARGO_FILE)
if (is.null(EMBARGO) || !length(EMBARGO))
  fail("cannot read embargo-content-patterns.txt; refusing to certify a ",
       "snapshot without the content scan. Run this script from the dev ",
       "checkout (Rscript tools/check_public_snapshot.R <worktree>).")
## Public-only patterns: material that may live in the private manuscript and
## dev tree (and so must not be in the list the paper build also scans) but
## never in a public snapshot, such as AI-drafting notes and revision checklists
## (author rule, 2026-10-10). Same lookup and the same refusal when missing.
PUBLIC_ONLY_FILE <- NULL
for (cand in c(if (!is.na(.self_dir)) file.path(.self_dir, "public-only-content-patterns.txt"),
               "tools/public-only-content-patterns.txt")) {
  if (!is.null(cand) && file.exists(cand)) { PUBLIC_ONLY_FILE <- cand; break }
}
PUBLIC_ONLY <- .read_patterns(PUBLIC_ONLY_FILE)
if (is.null(PUBLIC_ONLY) || !length(PUBLIC_ONLY))
  fail("cannot read public-only-content-patterns.txt; refusing to certify a ",
       "snapshot without it. Run this script from the dev checkout.")
EMBARGO <- c(EMBARGO, PUBLIC_ONLY)

.binary_ext <- "[.](png|jpe?g|gif|pdf|mp4|webm|rds|rda|RData|pptx|docx|xlsx|zip|gz|ico|woff2?|ttf|otf)$"
shipped <- if (file.exists(exclude)) {
  .d <- listed[endsWith(listed, "/")]
  tracked[!(tracked %in% listed) &
          !vapply(tracked, function(t) any(startsWith(t, .d)), logical(1))]
} else tracked
for (p in EMBARGO) {
  hit <- shipped[grepl(p, shipped, perl = TRUE)]
  if (length(hit)) problems[[paste0("path matches embargo pattern ", p)]] <- hit
}
for (f in shipped[!grepl(.binary_ext, shipped, ignore.case = TRUE)]) {
  path <- file.path(wt, f)
  if (!file.exists(path)) next
  txt <- tryCatch(suppressWarnings(readLines(path, warn = FALSE, encoding = "UTF-8")),
                  error = function(e) character())
  if (!length(txt)) next
  for (p in EMBARGO) {
    ln <- which(grepl(p, txt, perl = TRUE, useBytes = TRUE))
    if (length(ln))
      problems[[paste0("content matches embargo pattern ", p)]] <-
        c(problems[[paste0("content matches embargo pattern ", p)]],
          paste0(f, ":", ln))
  }
}

## Binary documents can carry the same text. Office files are zipped XML and
## are read here; a PDF is read through pdftools or pdftotext, and when neither
## is available a shipped PDF is refused rather than waved through. Four
## tracked PDFs carried embargoed text past the first version of this section.
.scan_text <- function(label, txt) {
  for (p in EMBARGO) {
    if (any(grepl(p, txt, perl = TRUE, useBytes = TRUE)))
      problems[[paste0("content matches embargo pattern ", p)]] <<-
        c(problems[[paste0("content matches embargo pattern ", p)]], label)
  }
}
for (f in shipped[grepl("[.](docx|pptx|xlsx)$", shipped, ignore.case = TRUE)]) {
  path <- file.path(wt, f); if (!file.exists(path)) next
  td <- tempfile(); dir.create(td)
  xml <- tryCatch(utils::unzip(path, exdir = td), error = function(e) character())
  xml <- xml[grepl("[.]xml$", xml)]
  txt <- unlist(lapply(xml, function(x) gsub("<[^>]+>", " ",
           paste(readLines(x, warn = FALSE, encoding = "UTF-8"), collapse = " "))))
  .scan_text(paste0(f, " (office text)"), txt)
  unlink(td, recursive = TRUE)
}
pdfs <- shipped[grepl("[.]pdf$", shipped, ignore.case = TRUE)]
if (length(pdfs)) {
  have_pdftools <- requireNamespace("pdftools", quietly = TRUE)
  pdftotext <- Sys.which("pdftotext")
  for (f in pdfs) {
    path <- file.path(wt, f); if (!file.exists(path)) next
    txt <- if (have_pdftools) {
      tryCatch(pdftools::pdf_text(path), error = function(e) NULL)
    } else if (nzchar(pdftotext)) {
      out <- tempfile(fileext = ".txt")
      system2(pdftotext, c(shQuote(path), shQuote(out)), stdout = FALSE, stderr = FALSE)
      if (file.exists(out)) readLines(out, warn = FALSE) else NULL
    } else NULL
    if (is.null(txt)) {
      problems[["PDF that cannot be read (install pdftools or pdftotext)"]] <-
        c(problems[["PDF that cannot be read (install pdftools or pdftotext)"]], f)
    } else .scan_text(paste0(f, " (pdf text)"), txt)
  }
}

## -- 4. images must be reviewed ------------------------------------------------ #
## The content scan reads text; it cannot read pixels. On 2026-10-08 the JSS
## paper's Figure 1 turned out to be another paper's conceptual figure, with
## that paper's title and empirical results drawn into the PNG; it had shipped
## since v0.2.0 under a neutral file name. Every shipped image must therefore
## carry a recorded human/agent review: its SHA-256 listed in the PRIVATE file
## tools/public-image-review.txt ("<sha256>  <path>  <date>  <note>"). A new
## or changed image fails here until someone has looked at it.
digest_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE))
    return(digest::digest(file = path, algo = "sha256"))
  fail("the image review needs the 'digest' package to hash files")
}
REVIEW_FILE <- NULL
for (cand in c(if (!is.na(.self_dir)) file.path(.self_dir, "public-image-review.txt"),
               "tools/public-image-review.txt")) {
  if (!is.null(cand) && file.exists(cand)) { REVIEW_FILE <- cand; break }
}
.img_ext <- "[.](png|jpe?g|gif|svg|webp|bmp|tiff?)$"
imgs <- shipped[grepl(.img_ext, shipped, ignore.case = TRUE)]
if (length(imgs)) {
  if (is.null(REVIEW_FILE))
    fail("cannot read public-image-review.txt; refusing to certify ",
         length(imgs), " shipped image(s) nobody has reviewed.")
  rv <- trimws(readLines(REVIEW_FILE, warn = FALSE))
  rv <- rv[nzchar(rv) & !startsWith(rv, "#")]
  reviewed <- tolower(sub("\\s.*$", "", rv))
  for (f in imgs) {
    path <- file.path(wt, f)
    if (!file.exists(path)) next
    h <- tolower(digest_sha256(path))
    if (!h %in% reviewed)
      problems[["image not reviewed (add its sha256 to tools/public-image-review.txt after looking at it)"]] <-
        c(problems[["image not reviewed (add its sha256 to tools/public-image-review.txt after looking at it)"]], f)
  }
}

## -- report ------------------------------------------------------------------ #
if (length(problems)) {
  cat("\nRefusing to publish: ", sum(lengths(problems)),
      " path(s) must not reach the public repository.\n\n", sep = "")
  for (why in names(problems)) {
    cat("  ", why, ":\n", sep = "")
    for (f in problems[[why]]) cat("      ", f, "\n", sep = "")
  }
  cat("\nFix: remove them from the snapshot worktree before committing, e.g.\n")
  cat("     git -C <worktree> rm --cached -- <path>...\n")
  cat("Then re-run this check. Do not push until it exits 0.\n")
  quit(status = 1)
}

cat("[ok] snapshot is clean: ", length(tracked),
    " tracked paths, none excluded or internal.\n", sep = "")
quit(status = 0)
