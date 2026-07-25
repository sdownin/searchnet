## Compile JSS manuscript as DRAFT PDF (code shown but not executed)
setwd("D:/Search_networks/SaoMNK/paper")

# Create a temporary copy with eval=FALSE globally
lines <- readLines("searchnet-jss.Rmd")

# Find the setup chunk and inject eval=FALSE
setup_idx <- grep("knitr::opts_chunk", lines)[1]
if (!is.na(setup_idx)) {
  lines[setup_idx] <- sub(
    "knitr::opts_chunk\\$set\\(",
    "knitr::opts_chunk$set(eval = FALSE, ",
    lines[setup_idx]
  )
}
# Remove keywords line that causes xmpquote LaTeX error
lines <- lines[!grepl("^keywords:", lines)]

# Add LaTeX macro definitions if not already present
header_idx <- grep("^header-includes:", lines)[1]
if (is.na(header_idx)) {
  # Add header-includes before the first ---
  yaml_end <- grep("^---$", lines)[2]
  new_header <- c(
    "header-includes:",
    "  - \\newcommand{\\pkg}[1]{\\textbf{#1}}",
    "  - \\newcommand{\\proglang}[1]{\\textsl{#1}}",
    "  - \\newcommand{\\code}[1]{\\texttt{#1}}",
    "  - \\newcommand{\\K}{\\{K\\}}"
  )
  lines <- c(lines[1:(yaml_end - 1)], new_header, lines[yaml_end:length(lines)])
} else {
  # Append to existing header-includes
  insert_after <- header_idx
  while (insert_after < length(lines) && grepl("^  -", lines[insert_after + 1])) {
    insert_after <- insert_after + 1
  }
  new_defs <- c(
    "  - \\newcommand{\\pkg}[1]{\\textbf{#1}}",
    "  - \\newcommand{\\proglang}[1]{\\textsl{#1}}",
    "  - \\newcommand{\\code}[1]{\\texttt{#1}}"
  )
  # Only add if not already defined
  if (!any(grepl("newcommand.*pkg", lines))) {
    lines <- c(lines[1:insert_after], new_defs, lines[(insert_after + 1):length(lines)])
  }
}

# Write temporary file
writeLines(lines, "searchnet-jss-DRAFT.Rmd")

# Render
cat("Compiling DRAFT PDF (code not executed)...\n")
tryCatch({
  rmarkdown::render("searchnet-jss-DRAFT.Rmd",
                     output_format = rmarkdown::pdf_document(
                       latex_engine = "pdflatex",
                       citation_package = "natbib",
                       keep_tex = FALSE
                     ),
                     quiet = TRUE)
  cat("SUCCESS: searchnet-jss-DRAFT.pdf\n")
}, error = function(e) {
  cat("FAILED:", e$message, "\n")
})

# Clean up temp file
unlink("searchnet-jss-DRAFT.Rmd")
