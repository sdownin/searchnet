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
  hit <- intersect(listed, tracked)
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
