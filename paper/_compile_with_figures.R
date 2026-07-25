## Compile JSS manuscript PDF with pre-generated figures
setwd("D:/Search_networks/SaoMNK/paper")

lines <- readLines("searchnet-jss.Rmd")

# 1. Set all chunks to eval=FALSE (use pre-generated figures instead)
setup_idx <- grep("knitr::opts_chunk", lines)[1]
if (!is.na(setup_idx)) {
  lines[setup_idx] <- sub(
    "knitr::opts_chunk\\$set\\(",
    "knitr::opts_chunk$set(eval = FALSE, ",
    lines[setup_idx]
  )
}

# 2. Remove keywords block
kw_start <- grep("^keywords:", lines)[1]
if (!is.na(kw_start)) {
  kw_end <- kw_start
  while (kw_end < length(lines) && grepl("^  -", lines[kw_end + 1])) kw_end <- kw_end + 1
  lines <- lines[-(kw_start:kw_end)]
}

# 3. Insert figure includes after key code chunks
# Find the API walkthrough section and add K-4 panel figure
insert_figure <- function(lines, after_pattern, fig_path, caption, label) {
  idx <- grep(after_pattern, lines, fixed = TRUE)[1]
  if (!is.na(idx)) {
    # Find the end of the code chunk (```)
    end_idx <- idx
    while (end_idx < length(lines) && !grepl("^```$", lines[end_idx])) end_idx <- end_idx + 1
    fig_block <- c(
      "",
      sprintf("![%s](figures/%s){#fig:%s width=90%%}", caption, fig_path, label),
      ""
    )
    lines <- c(lines[1:end_idx], fig_block, lines[(end_idx + 1):length(lines)])
  }
  lines
}

# Insert figures at strategic locations
lines <- insert_figure(lines, "saomnk_plot_k4(env)",
  "fig_k4_panel.png",
  "The K-4 coupled degree panel showing K_AC (scope), K_CA (popularity), K_AA (sociality), and K_CC (epistasis) evolving over simulation time.",
  "k4panel")

lines <- insert_figure(lines, "compute_formal_utility",
  "fig_10component.png",
  "10-component utility decomposition for each actor: NK fitness, scope cost, synergy, herding, congestion, displacement, complementarity, closure, rivalry, legitimacy.",
  "utility10")

# Also add standalone figure sections
# Find the Illustrations section and add key figures
illust_idx <- grep("Illustrations", lines)[1]
if (!is.na(illust_idx)) {
  fig_section <- c(
    "",
    "### The Bipartite Matrix and {K} Framework",
    "",
    "Figure \\ref{fig:bipartite} shows the bipartite matrix $\\mathbf{B}$ for a simulated environment with $M=6$ actors and $N=8$ components. Figure \\ref{fig:k4bars} decomposes the four coupled degree measures directly from this matrix.",
    "",
    "![The bipartite matrix B: teal cells indicate actor-component affiliations.](figures/fig_bipartite_heatmap.png){#fig:bipartite width=70%}",
    "",
    "![The four K dimensions computed from one bipartite matrix: scope (K_AC), popularity (K_CA), sociality (K_AA), and epistasis (K_CC).](figures/fig_k4_from_matrix.png){#fig:k4bars width=85%}",
    "",
    "### Convergence from Different Starting Points",
    "",
    "Figure \\ref{fig:convergence} demonstrates Blume's convergence theorem: five simulations starting at densities ranging from 0.05 to 0.95 all converge toward the same equilibrium density and scope, confirming that the stationary distribution is independent of initial conditions.",
    "",
    "![Convergence: five simulations from different starting densities converge to the same equilibrium. The Gibbs measure is unique regardless of initial conditions.](figures/fig_convergence.png){#fig:convergence width=85%}",
    ""
  )
  lines <- c(lines[1:illust_idx], fig_section, lines[(illust_idx + 1):length(lines)])
}

writeLines(lines, "searchnet-jss-FINAL.Rmd")

cat("Compiling PDF with figures...\n")
tryCatch({
  rmarkdown::render("searchnet-jss-FINAL.Rmd",
    output_format = rmarkdown::pdf_document(
      latex_engine = "pdflatex",
      citation_package = "natbib",
      keep_tex = FALSE, toc = FALSE, number_sections = TRUE
    ),
    output_file = "searchnet-jss-FINAL.pdf",
    quiet = TRUE)
  cat("SUCCESS:", file.size("searchnet-jss-FINAL.pdf") / 1024, "KB\n")
}, error = function(e) cat("FAILED:", e$message, "\n"))

unlink("searchnet-jss-FINAL.Rmd")
file.copy("searchnet-jss-FINAL.pdf", "jss_submission/searchnet-jss-manuscript.pdf", overwrite = TRUE)
cat("Copied to jss_submission/\n")
