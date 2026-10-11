#!/usr/bin/env Rscript
###############################################################################
## api_inventory.R
##
## Regenerates inst/API_CLASSIFICATION.md: every name exported by NAMESPACE,
## sorted into one class, with its title from the Rd page that documents it.
## The package is not loaded; NAMESPACE, R/ and man/ are parsed as files.
##
## Usage, from the package root:
##     Rscript tools/api_inventory.R            # rewrite the file
##     Rscript tools/api_inventory.R --check    # exit 1 if the file is stale
##
## Classes, first matching rule wins:
##   legacy        the function body calls a deprecation helper (.Deprecated
##                 or a helper whose name contains "deprecat"), or its Rd
##                 title starts with "Deprecated"
##   advanced      no Rd page, an Rd page marked \keyword{internal}, or a
##                 dot-prefixed Rd page: candidates to make internal
##   teaching      classroom and strategy-game workflows, teaching presets
##   visualization plotting, plot themes and palettes, and exporters that
##                 feed animation and dashboards
##   core          everything else, with a coarse area label
##
## The rules are deliberately mechanical so the file can be regenerated after
## any NAMESPACE change. Move a function by changing its documentation or the
## rules below, not by editing the generated file.
###############################################################################

args  <- commandArgs(trailingOnly = TRUE)
check <- "--check" %in% args
root  <- if (length(setdiff(args, "--check"))) setdiff(args, "--check")[1] else "."
out   <- file.path(root, "inst", "API_CLASSIFICATION.md")

## ---- NAMESPACE --------------------------------------------------------------
nsd <- file.path(tempfile("ns"), "pkg"); dir.create(nsd, recursive = TRUE)
invisible(file.copy(file.path(root, "NAMESPACE"), file.path(nsd, "NAMESPACE")))
ns <- parseNamespaceFile("pkg", dirname(nsd), mustExist = TRUE)
unlink(dirname(nsd), recursive = TRUE)
exports <- sort(unique(ns$exports))
s3 <- ns$S3methods

## ---- R/ top-level definitions ---------------------------------------------
deprecated_fun <- character()
for (f in list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE)) {
  for (e in parse(f, keep.source = FALSE)) {
    if (!is.call(e) || !is.name(e[[1]]) ||
        !as.character(e[[1]]) %in% c("<-", "=")) next
    lhs <- e[[2]]; rhs <- e[[3]]
    if (!is.name(lhs) || !is.call(rhs) || !identical(rhs[[1]], as.name("function"))) next
    calls <- all.names(rhs[[3]])
    if (any(calls == ".Deprecated" | grepl("deprecat", calls, ignore.case = TRUE) &
            calls != ".searchnet_deprecation_flags"))
      deprecated_fun <- c(deprecated_fun, as.character(lhs))
  }
}

## ---- man/ ------------------------------------------------------------------
rd_text <- function(x) {
  s <- paste(unlist(lapply(x, function(y) if (is.list(y)) rd_text(y) else as.character(y))),
             collapse = "")
  gsub("[[:space:]]+", " ", trimws(s))
}
rd <- list()
for (f in list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE)) {
  p <- tools::parse_Rd(f)
  tags <- vapply(p, function(x) attr(x, "Rd_tag"), character(1))
  get <- function(tag) vapply(p[tags == tag], rd_text, character(1))
  name <- get("\\name")[1]
  ent <- list(name = name, title = get("\\title")[1],
              internal = "internal" %in% get("\\keyword"))
  for (a in unique(c(name, get("\\alias")))) if (is.null(rd[[a]])) rd[[a]] <- ent
}

## ---- classify ---------------------------------------------------------------
re_teach <- "classroom|^searchnet_game_|^searchnet_check_setup$|^searchnet_validate_preset$"
re_viz   <- "plot|^theme_|palette|^scale_(colou?r|fill)_|_for_manim$|^searchnet_export_|_geo_arc$|compass"
area_of <- function(n) {
  if (grepl("causal|_did$|_synth$|_rd$|_rd_cross$|placebo|shock_support", n)) return("causal")
  if (grepl("saom_to_|empirical_to_|counterfactual|extract_estimates|_from_fit$|coevolve|from_long", n))
    return("estimation bridge")
  if (grepl("(^|_)k_|_k4|coholder|imitation|empirical_(influence|epistasis)|sim_ego|effect_dimensions|classify_effect|readings", n, ignore.case = TRUE))
    return("{K}")
  if (grepl("^lean_|_lean", n)) return("formal checks")
  if (grepl("^rosetta_", n)) return("translation registry")
  if (grepl("gof|screen|ladder|check|verify|proof|_sai$|confirm|assent|chain_|repertoire|ergodicity|recovery|moment_gate|provenance|readback|degree_bounds|infeasible|opportunity", n))
    return("diagnostics")
  if (grepl("^nk_|^bd_|mean_field|self_consistency|multiplier|basin|tradeoff|cross_partial|replicator|_ess$|elo", n))
    return("analytics")
  "simulation"
}
rows <- lapply(exports, function(n) {
  r <- rd[[n]]
  title <- if (is.null(r)) "" else r$title
  topic <- if (is.null(r)) "" else r$name
  cls <- if (n %in% deprecated_fun || grepl("^Deprecated", title)) "legacy"
    else if (is.null(r) || r$internal || startsWith(topic, "dot-")) "advanced"
    else if (grepl(re_teach, n)) "teaching"
    else if (grepl(re_viz, n, ignore.case = TRUE) || grepl("^(Plot|Animate|Visuali)", title)) "visualization"
    else "core"
  data.frame(name = n, class = cls, area = if (cls == "core") area_of(n) else "",
             topic = topic, title = title, stringsAsFactors = FALSE)
})
tab <- do.call(rbind, rows)

## ---- write -------------------------------------------------------------------
md_esc <- function(s) gsub("|", "\\|", s, fixed = TRUE)
cls_order <- c(core = "Core", visualization = "Visualization", teaching = "Teaching",
               legacy = "Legacy (deprecated wrappers)",
               advanced = "Advanced / internal candidates")
cls_def <- c(
  core = "Simulation, the estimation bridge, the {K} framework, diagnostics and causal tools.",
  visualization = "Plots, plot themes and palettes, and exporters that feed animation or dashboards.",
  teaching = "Classroom and strategy-game workflows and teaching presets.",
  legacy = "Superseded names kept as wrappers; each warns once per session and forwards to its replacement (see `?searchnet-naming`).",
  advanced = "Exported but undocumented or documented as internal; candidates to stop exporting in a future minor release.")

L <- c("# searchnet exported API, by class",
       "",
       "Generated by `tools/api_inventory.R` from `NAMESPACE`, `R/` and `man/`.",
       "Do not edit by hand; change the documentation or the rules in the script",
       "and regenerate with `Rscript tools/api_inventory.R`.",
       "",
       "| Class | Exports | Meaning |", "|---|---:|---|")
for (k in names(cls_order))
  L <- c(L, sprintf("| %s | %d | %s |", cls_order[[k]], sum(tab$class == k), cls_def[[k]]))
L <- c(L, sprintf("| **Total** | **%d** | |", nrow(tab)), "")
for (k in names(cls_order)) {
  sub <- tab[tab$class == k, , drop = FALSE]
  if (k == "core") sub <- sub[order(sub$area, sub$name), , drop = FALSE]
  L <- c(L, paste0("## ", cls_order[[k]], " (", nrow(sub), ")"), "")
  if (!nrow(sub)) { L <- c(L, "None.", ""); next }
  if (k == "core") {
    L <- c(L, "| Function | Area | Title | Help topic |", "|---|---|---|---|")
    L <- c(L, sprintf("| `%s` | %s | %s | %s |", sub$name, sub$area,
                      md_esc(sub$title), ifelse(nzchar(sub$topic), paste0("`", sub$topic, "`"), "")))
  } else {
    L <- c(L, "| Function | Title | Help topic |", "|---|---|---|")
    L <- c(L, sprintf("| `%s` | %s | %s |", sub$name, md_esc(sub$title),
                      ifelse(nzchar(sub$topic), paste0("`", sub$topic, "`"), "(none)")))
  }
  L <- c(L, "")
}
if (length(s3) && nrow(s3)) {
  L <- c(L, paste0("## Registered S3 methods (", nrow(s3), ")"), "",
         "Methods for the classes the functions above return; not counted above.", "",
         paste0("- `", s3[, 1], "(<", s3[, 2], ">)`"), "")
}
L <- L[-length(L)]

if (check) {
  cur <- if (file.exists(out)) readLines(out, warn = FALSE) else character()
  if (!identical(cur, L)) {
    cat("[stale] ", out, " does not match NAMESPACE/man; run Rscript tools/api_inventory.R\n", sep = "")
    quit(status = 1)
  }
  cat("[ok] ", out, " is current.\n", sep = "")
  quit(status = 0)
}
writeLines(L, out, useBytes = TRUE)
cat("wrote ", out, ": ", paste(sprintf("%s %d", names(cls_order),
    vapply(names(cls_order), function(k) sum(tab$class == k), 1L)), collapse = ", "),
    " (total ", nrow(tab), ")\n", sep = "")
