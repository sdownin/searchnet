###############################################################################
## test-terminology.R
## Terminology gate: W is the INFLUENCE MATRIX. It is not epistasis, and no
## degree measures epistasis.
##
## Standard (author decision 2026-08-21, carried by Paper T / CD2026 707f0b2):
## the N x N matrix a user passes in is the influence matrix (the NK
## interaction matrix with real-valued entries; Rivkin and Siggelkow's name).
## Epistasis names three separate things and W is only the first. K_CC is the
## realized structure -- the component-projection degree colSums((B'B) > 0),
## which does not read W. Epistatic fitness is the outcome -- the XWX effect
## weighted by influence_weight. A degree cannot measure a fitness
## consequence: K_CC and epistatic fitness co-evolve, and neither measures the
## other or W. K_CC keeps the display label "Epistasis"; "epistasis parameter
## K" (NK's K) and the adjective "epistatic" are untouched by this rule.
##
## The rule lives in a test that fails, not in a note that decays: the same
## discipline as CD2026's borrowed-name registry. The 2026-08-21 audit found 67
## stale uses across R, man, vignettes and proofs AFTER the 0.4.0 API rename had
## supposedly settled the question; a note did not hold, a failing test will.
###############################################################################

pkg_root <- normalizePath(file.path(dirname(dirname(getwd()))), winslash = "/")
if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
  pkg_root <- normalizePath("D:/Search_networks/SaoMNK", winslash = "/")
}

## The ONLY places the old terms may appear, and only in these phrasings.
## Each is the concordance ("it is also called the interaction matrix"), stated
## once so a reader arriving from the NK literature can find the object.
ALLOW <- list(
  ## saomnk_model() roxygen for `influence_matrix`, and the Rd it generates
  list(file = "R/saomnk-api.R",
       phrase = "the NK interaction matrix with"),
  list(file = "man/saomnk_model.Rd",
       phrase = "the NK interaction matrix with"),
  ## this test file quotes the terms in order to ban them
  list(file = "tests/testthat/test-terminology.R", phrase = NULL)
)

.scan <- function(dirs, pattern) {
  files <- unlist(lapply(dirs, function(d)
    list.files(file.path(pkg_root, d), pattern = "\\.(R|Rd|Rmd|md|tex)$",
               recursive = TRUE, full.names = TRUE)))
  ## R/_dev* subdirectories are not package code
  files <- files[!grepl("/R/_dev", files)]

  ## Nor is anything git ignores. Added 2026-08-23: this gate was failing on 330
  ## occurrences across 112 `R/*.Rmd` illustration notebooks, which `.gitignore`
  ## line 139 excludes from the repository entirely. They are local scratch --
  ## absent from a fresh clone, absent from every release snapshot, and never
  ## read by anyone but their author. A terminology rule for PACKAGE CONTENT
  ## should not be enforced against files that are not package content.
  ##
  ## This matters beyond tidiness. A check that fails on work it does not own,
  ## every run, teaches the reader to ignore it -- and this one exists precisely
  ## because a note about the rename decayed and a failing test was supposed to
  ## be the thing that held. A gate nobody trusts is worse than the note it
  ## replaced.
  ##
  ## `git ls-files` is the authority on what is package content. If git is
  ## unavailable the scan falls back to the full file list, which fails loudly
  ## rather than passing vacuously.
  tracked <- tryCatch({
    out <- suppressWarnings(system2("git", c("-C", shQuote(pkg_root), "ls-files"),
                                    stdout = TRUE, stderr = FALSE))
    if (length(out) && !is.null(attr(out, "status"))) character(0) else out
  }, error = function(e) character(0))

  if (length(tracked)) {
    rel_all <- sub(paste0("^", pkg_root, "/"), "", files)
    files <- files[rel_all %in% tracked]
  }

  hits <- list()
  for (f in files) {
    l <- readLines(f, warn = FALSE, encoding = "UTF-8")
    idx <- grep(pattern, l, ignore.case = TRUE, perl = TRUE)
    if (!length(idx)) next
    rel <- sub(paste0("^", pkg_root, "/"), "", f)
    for (i in idx) {
      allowed <- FALSE
      for (a in ALLOW) {
        if (identical(rel, a$file) &&
            (is.null(a$phrase) || grepl(a$phrase, l[i], fixed = TRUE))) {
          allowed <- TRUE; break
        }
      }
      if (!allowed) hits[[length(hits) + 1L]] <- sprintf("%s:%d: %s", rel, i, trimws(l[i]))
    }
  }
  unlist(hits)
}

test_that("no 'epistasis matrix' or 'interaction matrix' outside the concordance", {
  hits <- .scan(c("R", "man", "vignettes", "inst/proofs"),
                "epistasis matri|interaction matri")
  expect_length(hits, 0)
  if (length(hits)) {
    cat("\nTerminology gate: the influence matrix is 'influence matrix'.\n",
        "Old terms found outside the allowlist:\n  ",
        paste(hits, collapse = "\n  "), "\n")
  }
})

test_that("no roxygen title or heading puts 'epistasis' immediately before 'matrix'", {
  ## Titles are the line right after a roxygen block opens, and Rd \title{}
  ## lines; a heading in a vignette is a '#' line. Cheap superset check: any
  ## roxygen (#') or Rd title or markdown heading line with the adjacency.
  hits <- .scan(c("R", "man", "vignettes", "inst/proofs"),
                "^\\s*(#'|\\\\title\\{|#+\\s).*epistasis\\s+matri")
  expect_length(hits, 0)
})

test_that("the things that must NOT be renamed are still there", {
  ## K_CC keeps its 'Epistasis' label; NK's K is still the epistasis parameter.
  api <- readLines(file.path(pkg_root, "R", "saomnk-api.R"), warn = FALSE)
  expect_true(any(grepl("K_\\{CC\\}|K_CC", api)))
  pt <- readLines(file.path(pkg_root, "inst", "proofs", "PROOF_TABLE.md"), warn = FALSE)
  expect_true(any(grepl("epistasis parameter", pt)))
})
