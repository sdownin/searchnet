#!/usr/bin/env Rscript
# Fail if any roxygen @export tag lacks a NAMESPACE entry.
#
# searchnet's NAMESPACE is MANUALLY maintained (see its header), so an @export
# tag in R/*.R does nothing on its own. Before v0.6.0 that silently stranded 17
# exported functions -- reachable only via `searchnet:::` -- including the three
# counterfactual bridge functions that downstream project notes document as the
# supported path. This guard makes any future divergence loud instead of silent.
#
# Character classes are used instead of backslash escapes throughout, so the
# file survives being written by heredocs and code generators.
#
# Usage: Rscript tools/check_namespace_sync.R [package_root]
# Exit status 1 if out of sync.

root <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "."

fn_re     <- "^`?([A-Za-z._][A-Za-z0-9._]*)`?[[:space:]]*<-[[:space:]]*function"
export_re <- "^#'[[:space:]]*@export"
roxy_re   <- "^#'"

files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
tagged <- character(0)
for (f in files) {
  l <- readLines(f, warn = FALSE)
  for (i in grep(export_re, l)) {
    # walk forward to the first function assignment, skipping further roxygen
    # lines and blanks; stop at any other code so we do not attribute a tag to
    # an unrelated definition further down the file.
    for (j in seq(i + 1, min(i + 25, length(l)))) {
      m <- regmatches(l[j], regexec(fn_re, l[j]))[[1]]
      if (length(m) == 2) { tagged <- c(tagged, m[2]); break }
      if (grepl(roxy_re, l[j]) || !nzchar(trimws(l[j]))) next
      break
    }
  }
}
tagged <- unique(tagged)

ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
grab <- function(pat) {
  hits <- grep(pat, ns, value = TRUE)
  inner <- sub("^[A-Za-z3]+[(]", "", hits)
  trimws(gsub("[)]", "", inner))
}
exported <- grab("^export[(]")
# S3method(print, foo) registers print.foo
s3 <- vapply(strsplit(grab("^S3method[(]"), ","), function(p)
  paste(trimws(p), collapse = "."), character(1))

missing <- setdiff(tagged, c(exported, s3))

if (length(missing)) {
  cat("NAMESPACE OUT OF SYNC:", length(missing),
      "function(s) carry @export but have no NAMESPACE entry\n")
  cat(paste0("  ", sort(missing), collapse = "\n"), "\n\n")
  cat("Add export(<name>) -- or S3method(<generic>, <class>) for methods -- to NAMESPACE.\n")
  quit(status = 1)
}
cat("NAMESPACE in sync:", length(tagged), "@export-tagged functions,",
    length(exported), "export() entries,", length(s3), "S3 methods.\n")
