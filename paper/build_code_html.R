###############################################################################
## build_code_html.R
##
## JSS replication code for the searchnet manuscript: code.R and code.html.
## Sourced by build_submission.R (full build only); defines one function and
## runs nothing on its own.
##
## JSS asks for the R code that reproduces all results in the paper, plus a
## code.html produced from it by knitr::spin() that shows the output,
## including sessionInfo(). code.R here is the code of every chunk of
## searchnet-jss.Rmd, in order, extracted with knitr::purl(): it is the code
## that produced the manuscript's numbers and figures, so it cannot drift from
## them the way a separately maintained script can. Chunks the manuscript
## shows without evaluating (eval = FALSE) arrive commented out, as purl()
## writes them. Chunk options are dropped (cache, include, fig.cap); only the
## labels are kept, as spin() chunk headers.
##
## Test on its own (needs the package loaded, as the build does):
##   pkgload::load_all("."); source("paper/build_code_html.R")
##   make_code_html("paper/searchnet-jss.Rmd", tempdir())
###############################################################################

## Writes <out_dir>/code.R and <out_dir>/code.html; returns both paths.
## `work_dir` is where spin() and render() run (their figure and cache files
## stay there, outside the bundle).
make_code_html <- function(rmd, out_dir,
                           work_dir = file.path(tempdir(), "code-html")) {
  for (p in c("knitr", "rmarkdown"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("make_code_html() needs ", p, call. = FALSE)
  unlink(work_dir, recursive = TRUE)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ## 1. purl: chunk code in order, chunk headers as "## ----label, opts----".
  purled <- file.path(work_dir, "purled.R")
  knitr::purl(rmd, output = purled, documentation = 1L, quiet = TRUE)
  x <- readLines(purled, warn = FALSE, encoding = "UTF-8")

  ## A line beginning "#'" would become spin() text; none is expected.
  if (any(grepl("^#'", x)))
    stop("purled code contains lines starting with #' (spin text markers)",
         call. = FALSE)

  ## 2. purl's chunk headers -> spin chunk headers, keeping only the label.
  hdr <- grepl("^## -{4,}", x)
  lab <- sub("-+$", "", sub("^## -{4,}", "", x[hdr]))
  lab <- trimws(sub(",.*$", "", lab))
  x[hdr] <- ifelse(nzchar(lab), paste0("#+ ", lab), "#+")

  src <- basename(rmd)
  head <- c(
    "#' ---",
    "#' title: \"searchnet: replication code for the JSS manuscript\"",
    "#' output:",
    "#'   html_document:",
    "#'     toc: true",
    "#' ---",
    "#'",
    paste0("#' The R code of every chunk in `", src, "`, in manuscript order,"),
    "#' extracted with `knitr::purl()` by `paper/build_submission.R`.",
    "#' Running this script reproduces the results reported in the paper.",
    "#' Code the manuscript shows without evaluating (`eval = FALSE`) is",
    "#' commented out. `code.html` is this script rendered with",
    "#' `knitr::spin()`. The two simulated figures included as image files",
    "#' are produced by `replication/make_k_system_figures.R`; the other",
    "#' image files are diagrams.",
    "#'",
    "#' Generated file: edit the manuscript, not this script.",
    "")
  tail <- c("",
            "#' ## Session information",
            "#+ session-info",
            "sessionInfo()")
  code_r <- file.path(work_dir, "code.R")
  writeLines(c(head, x, tail), code_r, useBytes = TRUE)

  ## 3. spin -> .Rmd, then render to a self-contained HTML page. A fresh
  ## environment, so nothing from the build session leaks into the chunks.
  spun <- knitr::spin(code_r, knit = FALSE, format = "Rmd")
  html <- rmarkdown::render(spun, output_dir = work_dir, quiet = TRUE,
                            envir = new.env(parent = globalenv()))
  if (!file.exists(html)) stop("code.html was not produced", call. = FALSE)

  out_r    <- file.path(out_dir, "code.R")
  out_html <- file.path(out_dir, "code.html")
  if (!file.copy(code_r, out_r, overwrite = TRUE) ||
      !file.copy(html, out_html, overwrite = TRUE))
    stop("cannot copy code.R / code.html to ", out_dir, call. = FALSE)
  c(code_r = out_r, code_html = out_html)
}
