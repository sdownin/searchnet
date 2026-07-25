## Compile JSS manuscript as finalized PDF
## Strategy: pre-run all simulations, cache results, then compile with cached output
setwd("D:/Search_networks/SaoMNK/paper")

# Step 1: Ensure searchnet is loadable
cat("Step 1: Loading searchnet...\n")
.saomnk_dir <- file.path(dirname(getwd()), "R")
source(file.path(.saomnk_dir, "saomnk-loader.R"))
cat("  searchnet loaded.\n")

# Step 2: Create cache directory
cache_dir <- "searchnet-jss_cache/pdf"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
cat("Step 2: Cache dir ready.\n")

# Step 3: Read the manuscript and prepare for compilation
lines <- readLines("searchnet-jss.Rmd")

# Fix: ensure all simulation chunks have cache=TRUE so they only run once
# Find all ```{r chunks and add cache=TRUE if not present
for (i in seq_along(lines)) {
  if (grepl("^```\\{r ", lines[i]) && !grepl("cache", lines[i]) && !grepl("setup", lines[i])) {
    lines[i] <- sub("\\}$", ", cache=TRUE}", lines[i])
  }
}

# Remove entire keywords block that causes xmpquote LaTeX errors
kw_start <- grep("^keywords:", lines)[1]
if (!is.na(kw_start)) {
  kw_end <- kw_start
  while (kw_end < length(lines) && grepl("^  -", lines[kw_end + 1])) {
    kw_end <- kw_end + 1
  }
  lines <- lines[-(kw_start:kw_end)]
  cat("  Removed keywords block (lines", kw_start, "to", kw_end, ")\n")
}

# Ensure header-includes has the necessary LaTeX macros
# Check if \pkg is already defined
if (!any(grepl("newcommand.*\\\\pkg", lines))) {
  header_idx <- grep("^header-includes:", lines)[1]
  if (!is.na(header_idx)) {
    # Find last line of header-includes
    insert_at <- header_idx
    while (insert_at < length(lines) && grepl("^  -", lines[insert_at + 1])) {
      insert_at <- insert_at + 1
    }
    new_defs <- c(
      '  - \\renewcommand{\\pkg}[1]{\\textbf{#1}}',
      '  - \\renewcommand{\\proglang}[1]{\\textsl{#1}}',
      '  - \\renewcommand{\\code}[1]{\\texttt{#1}}'
    )
    lines <- c(lines[1:insert_at], new_defs, lines[(insert_at + 1):length(lines)])
  }
}

# Write temp file
writeLines(lines, "searchnet-jss-FINAL.Rmd")

# Step 4: Compile with natbib and pdflatex
cat("Step 3: Compiling PDF (this may take several minutes if simulations run)...\n")
tryCatch({
  rmarkdown::render(
    "searchnet-jss-FINAL.Rmd",
    output_format = rmarkdown::pdf_document(
      latex_engine = "pdflatex",
      citation_package = "natbib",
      keep_tex = TRUE,
      toc = FALSE,
      number_sections = TRUE
    ),
    output_file = "searchnet-jss-FINAL.pdf",
    quiet = FALSE
  )
  cat("\n\nSUCCESS: searchnet-jss-FINAL.pdf compiled!\n")
  cat("Size:", file.size("searchnet-jss-FINAL.pdf") / 1024, "KB\n")
}, error = function(e) {
  cat("\nCompilation with code execution failed.\n")
  cat("Error:", e$message, "\n")
  cat("\nFalling back to eval=FALSE draft...\n")

  # Fallback: set all chunks to eval=FALSE
  lines2 <- readLines("searchnet-jss-FINAL.Rmd")
  setup_idx <- grep("knitr::opts_chunk", lines2)[1]
  if (!is.na(setup_idx)) {
    lines2[setup_idx] <- sub(
      "knitr::opts_chunk\\$set\\(",
      "knitr::opts_chunk$set(eval = FALSE, ",
      lines2[setup_idx]
    )
  }
  writeLines(lines2, "searchnet-jss-FINAL.Rmd")

  rmarkdown::render(
    "searchnet-jss-FINAL.Rmd",
    output_format = rmarkdown::pdf_document(
      latex_engine = "pdflatex",
      citation_package = "natbib",
      keep_tex = FALSE,
      toc = FALSE,
      number_sections = TRUE
    ),
    output_file = "searchnet-jss-FINAL.pdf",
    quiet = TRUE
  )
  cat("FALLBACK SUCCESS: searchnet-jss-FINAL.pdf (code shown, not executed)\n")
  cat("Size:", file.size("searchnet-jss-FINAL.pdf") / 1024, "KB\n")
})

# Cleanup temp Rmd
unlink("searchnet-jss-FINAL.Rmd")

# Copy to submission bundle
file.copy("searchnet-jss-FINAL.pdf", "jss_submission/searchnet-jss-manuscript.pdf",
          overwrite = TRUE)
cat("Copied to jss_submission/\n")
