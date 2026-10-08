###############################################################################
## rosetta-translate.R
##
##   rosetta_translate()    a model, class by class, and the registry entries
##                          it contains as special cases or already satisfies
##   rosetta_compare()      classes x models table
##   rosetta_new_entry()    template entry for a researcher's own model
##   rosetta_lean()         Lean statements behind an entry or a model
##   rosetta_export_json()  the registry as JSON for front ends
###############################################################################

## Class-by-class view of a spec: one row per registered class.
.rosetta_terms_of_spec <- function(s, classes = rosetta_classes()) {
  rows <- lapply(seq_len(nrow(classes)), function(i) {
    cid <- classes$id[i]
    effs <- Filter(function(e) identical(e$class, cid), s$effects)
    on <- Filter(.rosetta_effect_on, effs)
    status <- if (length(on)) "active" else if (length(effs)) "zero" else "absent"
    expr <- if (length(effs)) paste(vapply(effs, function(e)
      sprintf("theta_%s = %s", e$effect, format(signif(e$parameter, 4))), ""),
      collapse = "; ") else "0"
    if (cid == "complementarity" && length(on) && !is.null(s$influence_matrix))
      expr <- paste0(expr, sprintf(" (W: %d x %d)", nrow(s$influence_matrix), ncol(s$influence_matrix)))
    data.frame(class = cid, label = classes$label[i], k_channel = classes$k_channel[i],
               reads = classes$reads[i],
               moves = classes$moves[i],
               status = status,
               effects = paste(vapply(effs, function(e) e$effect, ""), collapse = ", "),
               expression = expr, construct = classes$construct[i], stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  other <- Filter(function(e) !e$class %in% classes$id, s$effects)
  if (length(other))
    out <- rbind(out, data.frame(class = "other", label = "other", k_channel = "other",
                                 reads = "other", moves = "other",
                                 status = if (any(vapply(other, .rosetta_effect_on, TRUE))) "active" else "zero",
                                 effects = paste(vapply(other, function(e) e$effect, ""), collapse = ", "),
                                 expression = paste(vapply(other, function(e) sprintf("theta_%s = %s", e$effect,
                                   format(signif(e$parameter, 4))), ""), collapse = "; "),
                                 construct = "", stringsAsFactors = FALSE))
  out
}

## Class-by-class view of an entry.
.rosetta_terms_of_entry <- function(e, classes = rosetta_classes()) {
  tl <- e$terms
  rows <- lapply(seq_len(nrow(classes)), function(i) {
    cid <- classes$id[i]
    t <- Filter(function(x) identical(x$class, cid), tl)
    t <- if (length(t)) t[[1]] else list(status = "absent", expression = "0")
    data.frame(class = cid, label = classes$label[i], k_channel = classes$k_channel[i],
               reads = classes$reads[i],
               moves = classes$moves[i],
               status = t$status, effects = .rs_or(t$effect, ""),
               expression = .rs_or(t$expression, if (t$status %in% .ROSETTA_ON) "" else "0"),
               construct = .rs_or(t$construct, classes$construct[i]),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

.rosetta_on <- function(terms) terms$class[terms$status %in% .ROSETTA_ON]

## Does spec s satisfy entry e's structural restrictions?
.rosetta_meets <- function(s, e) {
  r <- .rs_or(e$restrictions, list())
  why <- character(0)
  if (is.numeric(r$M) && !is.na(s$M) && s$M != r$M) why <- c(why, sprintf("M = %s, needs %s", s$M, r$M))
  if (is.numeric(r$N) && !is.na(s$N) && s$N != r$N) why <- c(why, sprintf("N = %s, needs %s", s$N, r$N))
  W <- s$influence_matrix
  if (identical(r$W, "binary-k-regular") && !is.null(W)) {
    E <- (W != 0) * 1
    if (!all(W %in% c(0, 1))) why <- c(why, "W is real-valued, needs binary")
    diag(E) <- 0  # XWX sums over j != h, so the diagonal is not part of the pattern
    if (length(unique(rowSums(E))) != 1L) why <- c(why, "W is not K-regular")
  }
  if (identical(r$W, "zero") && !is.null(W) && any(W != 0)) why <- c(why, "W must be zero")
  if (.rosetta_is_inf(r$beta) && !(is.numeric(s$beta) && is.infinite(s$beta)))
    why <- c(why, "needs beta -> infinity")
  rr <- .rs_or(r$revision_rule, "any")
  sr <- .rs_or(s$revision_rule, "multinomial")
  if (rr %in% c("single_flip", "multinomial") && !identical(rr, sr))
    why <- c(why, sprintf("revision rule %s, needs %s", sr, rr))
  why
}

## The nesting rule, shared with front ends that read the JSON contract.
##
## A class c is INCLUDED in a model iff the model has at least one effect of
## class c that is not held fixed at zero (fix && parameter == 0).
## A model U nests an entry E (E is reached by restricting U's parameters) iff
##   (1) every class whose term in E is active, linearized or fixed is
##       included in U (E's zero or absent classes impose nothing: U reaches
##       them by setting their coefficients to 0), and
##   (2) if E restricts the revision rule, U uses the same rule.
## Verdicts: does_not_nest when (1) fails; conditional when (1) holds and (2)
## fails; nests_in_limit when both hold and E has M = Inf or beta = Inf;
## nests otherwise. M = 1, a binary W and zero coefficients are restrictions
## of parameters U already has, so they never block nesting. Entries whose
## relation is not a nesting relation (translation, estimation-check,
## comparison, not-carried) are never nested: their verdict is
## does_not_nest, with the relation given as the reason.
.rosetta_effect_on <- function(e) {
  fix <- if (is.null(e$fix)) TRUE else isTRUE(as.logical(e$fix))
  !(fix && isTRUE(e$parameter == 0))
}

.rosetta_nesting <- function(s, e, classes) {
  te <- .rosetta_terms_of_entry(e, classes)
  need <- te$class[te$status %in% .ROSETTA_ON]
  incl <- unique(vapply(Filter(.rosetta_effect_on, s$effects), function(x) x$class, ""))
  missing <- setdiff(need, incl)
  r <- .rs_or(e$restrictions, list())
  conditions <- character(0)
  rr <- r$revision_rule
  if (!is.null(rr) && rr %in% c("single_flip", "multinomial") &&
      !identical(rr, .rs_or(s$revision_rule, "multinomial")))
    conditions <- paste("revision rule", sub("_", "-", rr))
  limits <- c(if (.rosetta_is_inf(r$M)) "M to infinity",
              if (.rosetta_is_inf(r$beta)) "beta to infinity")
  if (!e$relation %in% .ROSETTA_NESTING) {
    verdict <- "does_not_nest"
    conditions <- c(conditions, paste0("relation is ", e$relation, ": related, not nested"))
  } else verdict <- if (length(missing)) "does_not_nest" else if (length(conditions)) "conditional"
                    else if (length(limits)) "nests_in_limit" else "nests"
  data.frame(id = e$id, title = e$title, relation = e$relation, verdict = verdict,
             missing = paste(missing, collapse = ", "),
             conditions = paste(conditions, collapse = "; "),
             limits = paste(limits, collapse = "; "), stringsAsFactors = FALSE)
}

#' Translate a model into the classes of the SAOM-NK objective
#'
#' Decomposes a model class by class (status, effects, expression, K dimension
#' and construct) and gives a nesting verdict against every registry entry.
#'
#' A class is \emph{included} in the model when the model has an effect of
#' that class not held fixed at zero. The model nests an entry when (1) it
#' includes every class the entry has active, linearized or fixed, and (2) it
#' uses the entry's revision rule if the entry sets one. Verdicts:
#' `"does_not_nest"` when (1) fails (`missing` names the classes);
#' `"conditional"` when (1) holds and (2) fails (`conditions` says what to
#' change); `"nests_in_limit"` when both hold and the entry is reached as
#' M or beta tends to infinity (`limits`); `"nests"` otherwise. Restrictions
#' to M = 1, a binary W, or zero coefficients are parameter values the model
#' already has, so they never block nesting. Entries whose relation is not a
#' special case, linearized special case or equilibrium characterization are
#' related but never nested.
#'
#' @param x A `rosetta_spec`, entry id, `saomnk_model`, `SaomNkRSienaBiEnv`,
#'   or JSON specification (path, string or list).
#' @param include_private Logical; also relate to `entries-private/`.
#' @param path Registry directory.
#' @return A list of class `rosetta_translation` with `spec`, `terms`
#'   (data.frame, one row per class), `nesting` (one row per entry: `id`,
#'   `title`, `relation`, `verdict`, `missing`, `conditions`, `limits`),
#'   `contains` (the nested entries, verdict other than `"does_not_nest"`,
#'   with `differences`: the classes to switch off and the restrictions to
#'   impose), and `within` (entries whose restrictions the model already
#'   satisfies, switching on no class the entry leaves off).
#' @seealso [rosetta_compare()], [rosetta_plot()]
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) {
#'   mod <- saomnk_model(density = -1, popularity = -0.2,
#'                       influence_matrix = saomnk_block_diagonal(6, 2))
#'   rosetta_translate(mod)
#' }
rosetta_translate <- function(x, include_private = FALSE,
                              path = getOption("searchnet.rosetta_path")) {
  classes <- rosetta_classes(path)
  s <- .rosetta_as_spec(x, include_private, path)
  terms <- .rosetta_terms_of_spec(s, classes)
  on_u <- .rosetta_on(terms)
  es <- .rosetta_load(include_private, path)
  nest <- list(); cont <- list(); within <- list()
  for (e in es) {
    v <- .rosetta_nesting(s, e, classes)
    nest[[length(nest) + 1L]] <- v
    te <- .rosetta_terms_of_entry(e, classes)
    on_e <- .rosetta_on(te)
    if (v$verdict != "does_not_nest") {
      off <- setdiff(on_u, on_e)
      d <- c(if (length(off)) paste0(off, ": active -> ", te$status[match(off, te$class)]),
             paste0("restriction: ", .rosetta_restriction_text(e)))
      cont[[length(cont) + 1L]] <- data.frame(id = e$id, title = e$title, relation = e$relation,
                                              verdict = v$verdict,
                                              differences = paste(d, collapse = "; "),
                                              stringsAsFactors = FALSE)
    }
    if (e$relation %in% .ROSETTA_NESTING && all(on_u %in% on_e) && !length(.rosetta_meets(s, e))) {
      extra <- setdiff(on_e, on_u)
      within[[length(within) + 1L]] <- data.frame(id = e$id, title = e$title, relation = e$relation,
        differences = if (length(extra)) paste0(extra, ": ", te$status[match(extra, te$class)],
                                                " in entry, absent here", collapse = "; ") else "",
        stringsAsFactors = FALSE)
    }
  }
  empty <- data.frame(id = character(0), title = character(0), relation = character(0),
                      differences = character(0), stringsAsFactors = FALSE)
  structure(list(spec = s, terms = terms,
                 nesting = if (length(nest)) do.call(rbind, nest) else
                   cbind(empty[, 1:3], verdict = character(0), missing = character(0),
                         conditions = character(0), limits = character(0)),
                 contains = if (length(cont)) do.call(rbind, cont) else
                   cbind(empty[, 1:3], verdict = character(0), differences = character(0)),
                 within = if (length(within)) do.call(rbind, within) else empty),
            class = c("rosetta_translation", "list"))
}

.rosetta_restriction_text <- function(e) {
  r <- .rs_or(e$restrictions, list())
  keep <- r[intersect(names(r), c("M", "N", "K", "W", "beta", "revision_rule", "rates"))]
  keep <- Filter(function(v) !is.null(v) && length(v), keep)
  if (!length(keep)) return("none beyond the terms")
  paste(paste0(names(keep), " = ", vapply(keep, function(v) paste(unlist(v), collapse = "/"), "")),
        collapse = ", ")
}

print.rosetta_translation <- function(x, ...) {
  cat("<rosetta_translation>\n")
  t <- x$terms
  for (i in seq_len(nrow(t)))
    cat(sprintf("  %-11s %-5s %-7s %s\n", t$class[i], t$k_channel[i], t$status[i], t$expression[i]))
  n <- x$nesting
  if (!any(n$verdict != "does_not_nest")) cat("Nests no registry entry\n")
  for (v in c("nests", "nests_in_limit", "conditional")) {
    ids <- n$id[n$verdict == v]
    if (length(ids)) cat(sprintf("%-15s %s\n", paste0(v, ":"), paste(ids, collapse = ", ")))
  }
  cat(sprintf("Satisfies the restriction of (%d): %s\n", nrow(x$within),
              if (nrow(x$within)) paste(x$within$id, collapse = ", ") else "none"))
  invisible(x)
}

#' Compare models class by class
#'
#' @param x A vector of entry ids, or a list mixing entry ids and models
#'   (anything [rosetta_translate()] accepts). List names become column
#'   names.
#' @param ... Further models or entry ids, appended to `x`; so
#'   `rosetta_compare(spec, "nk-adaptive-walk")` compares a model with one
#'   entry.
#' @param include_private Logical; also read `entries-private/`.
#' @param path Registry directory.
#' @return A data.frame with one row per class: `class`, `k_channel`, then
#'   one column per model holding `"status: expression"`. Attribute `long`
#'   holds the long form (model, class, k_channel, status, expression,
#'   construct); attribute `verdicts` gives, for every model column that is
#'   not an entry and every entry column, the nesting verdict of
#'   [rosetta_translate()] (`model`, `entry`, `verdict`, `missing`,
#'   `conditions`, `limits`).
#' @seealso [rosetta_translate()], [rosetta_plot()]
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE))
#'   rosetta_compare(c("nk-adaptive-walk", "plain-saom-two-mode"))
rosetta_compare <- function(x, ..., include_private = FALSE,
                            path = getOption("searchnet.rosetta_path")) {
  classes <- rosetta_classes(path)
  as_items <- function(v) {
    if (is.character(v)) return(as.list(v))
    if (inherits(v, c("rosetta_spec", "saomnk_model", "R6")) ||
        (is.list(v) && !is.null(v$effects))) return(list(v))
    v
  }
  x <- as_items(x)
  more <- list(...)
  for (i in seq_along(more)) {
    it <- as_items(more[[i]])
    nmi <- names(more)[i]
    if (!is.null(nmi) && nzchar(nmi) && length(it) == 1L) names(it) <- nmi
    x <- c(x, it)
  }
  nm <- names(x)
  if (is.null(nm)) nm <- rep("", length(x))
  ids <- rosetta_entries(include_private, path)$id
  long <- list(); specs <- list(); ents <- list()
  for (k in seq_along(x)) {
    xi <- x[[k]]
    if (is.character(xi) && length(xi) == 1L && xi %in% ids) {
      en <- rosetta_entry(xi, include_private, path)
      te <- .rosetta_terms_of_entry(en, classes)
      label <- if (nzchar(nm[k])) nm[k] else xi
      ents[[label]] <- en
    } else {
      sp <- .rosetta_as_spec(xi, include_private, path)
      te <- .rosetta_terms_of_spec(sp, classes)
      label <- if (nzchar(nm[k])) nm[k] else paste0("model_", k)
      specs[[label]] <- sp
    }
    te$model <- label
    long[[k]] <- te
  }
  vd <- list()
  for (mn in names(specs)) for (en in names(ents)) {
    v <- .rosetta_nesting(specs[[mn]], ents[[en]], classes)
    vd[[length(vd) + 1L]] <- data.frame(model = mn, entry = en, verdict = v$verdict,
                                        missing = v$missing, conditions = v$conditions,
                                        limits = v$limits, stringsAsFactors = FALSE)
  }
  long <- do.call(rbind, long)
  models <- unique(long$model)
  cls <- unique(long$class)
  out <- data.frame(class = cls, k_channel = long$k_channel[match(cls, long$class)],
                    stringsAsFactors = FALSE)
  for (m in models) {
    sub <- long[long$model == m, ]
    v <- paste0(sub$status, ": ", sub$expression)[match(cls, sub$class)]
    v[is.na(v)] <- "absent: 0"
    out[[m]] <- v
  }
  attr(out, "long") <- long[, c("model", "class", "k_channel", "status", "expression", "construct")]
  attr(out, "verdicts") <- if (length(vd)) do.call(rbind, vd) else NULL
  out
}

#' Write a registry entry template from a model
#'
#' Starts an entry for a researcher's own theory: the terms are filled from
#' the model's classes, the relation and statement are placeholders, and the
#' provenance marks the file as a draft to be reviewed. Edit the file, put it
#' in `entries/` (or a registry of your own, see `searchnet.rosetta_path`),
#' and run [rosetta_validate()].
#'
#' @param id Kebab-case id; also the file name.
#' @param from A model (anything [rosetta_translate()] accepts).
#' @param dir Output directory (default `tempdir()`).
#' @param title,model Text fields of the entry.
#' @param relation One of the relation vocabulary (default `"translation"`).
#' @param constructs Optional named list, construct label to class ids.
#' @return Invisibly, the path of the written file.
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) {
#'   mod <- saomnk_model(density = -1, popularity = -0.3)
#'   f <- rosetta_new_entry("my-crowding-theory", from = mod)
#'   cat(readLines(f), sep = "\n")
#' }
rosetta_new_entry <- function(id, from, dir = tempdir(), title = id, model = title,
                              relation = "translation", constructs = NULL) {
  .rosetta_need_yaml("rosetta_new_entry()")
  stopifnot(grepl("^[a-z0-9]+(-[a-z0-9]+)*$", id), relation %in% .ROSETTA_RELATION)
  tr <- rosetta_translate(from)
  t <- tr$terms[tr$terms$class != "other", ]
  s <- tr$spec
  terms <- lapply(seq_len(nrow(t)), function(i) {
    x <- list(class = t$class[i], status = t$status[i], expression = t$expression[i],
              construct = t$construct[i])
    eff <- Filter(function(e) identical(e$class, t$class[i]), s$effects)
    if (length(eff)) { x$effect <- eff[[1]]$effect; x$value <- eff[[1]]$parameter }
    x
  })
  if (is.null(constructs)) {
    on <- t[t$status %in% .ROSETTA_ON, ]
    constructs <- stats::setNames(as.list(on$class), on$construct)
  }
  restr <- list(M = if (is.na(s$M)) NULL else s$M, W = if (is.null(s$influence_matrix)) "zero" else "real",
                revision_rule = s$revision_rule)
  e <- list(schema_version = "1", id = id, title = title, model = model,
            citations = list(list(key = "author-year", text = "Full reference.", doi = NULL)),
            relation = relation,
            statement = "State the relation to the SAOM-NK objective in one or two sentences.",
            restrictions = Filter(Negate(is.null), restr), terms = terms,
            constructs = constructs, lean = list(), checks = list(),
            does_not_cover = list("State what the relation does not establish."),
            provenance = list(drafted = paste("rosetta_new_entry(),", format(Sys.Date())),
                              ai_drafted = FALSE, reviewed = FALSE))
  f <- file.path(dir, paste0(id, ".yaml"))
  writeLines(c("# Registry entry template written by searchnet::rosetta_new_entry().",
               "# Edit every placeholder, then run rosetta_validate().",
               yaml::as.yaml(e)), f, useBytes = TRUE)
  invisible(f)
}

#' Lean statements behind an entry or a model
#'
#' For an entry, lists the Lean declarations it names with their statements
#' from the library registry. Where the library supports the model, also
#' writes an instance file with [lean_export_model()]: for the one-actor NK
#' restriction a classical NK landscape (whose export proves that the NK
#' term is NK fitness and that greedy search is the zero-noise limit), and
#' otherwise the model's own statistics (`density`, `outAct`, `inPop`,
#' `cycle4`, `XWX`). Check the file with [lean_check()] when Lean is
#' installed.
#'
#' @param x Entry id, `rosetta_spec`, or any model [rosetta_translate()]
#'   accepts.
#' @param dir Output directory for the instance file.
#' @param N_max Largest `N` exported (the landscape table has `2^N` rows).
#' @param include_private Logical; also read `entries-private/`.
#' @return A list of class `rosetta_lean` with `declarations` (data.frame
#'   `decl`, `statement`, `in_registry`), `instance` (path or `NULL`), and
#'   `notes`.
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) {
#'   l <- rosetta_lean("nk-adaptive-walk")
#'   l$declarations
#' }
rosetta_lean <- function(x, dir = tempdir(), N_max = 6L, include_private = TRUE) {
  notes <- character(0)
  reg <- tryCatch(lean_registry(), error = function(e) NULL)
  decls <- character(0)
  if (is.character(x) && length(x) == 1L && x %in% rosetta_entries(include_private)$id) {
    e <- rosetta_entry(x, include_private)
    decls <- unlist(e$lean)
    s <- rosetta_model(e, N = min(4L, N_max), M = if (is.numeric(e$restrictions$M)) NULL else 2L)
  } else s <- .rosetta_as_spec(x, include_private)
  stmt <- if (!is.null(reg)) reg$statement[match(decls, reg$decl)] else rep(NA_character_, length(decls))
  dd <- data.frame(decl = decls, statement = stmt, in_registry = !is.na(stmt),
                   stringsAsFactors = FALSE)
  inst <- NULL
  N <- s$N
  if (!is.na(N) && N > N_max) {
    notes <- c(notes, sprintf("N = %d exceeds N_max = %d; no instance written", N, N_max))
  } else if (!is.na(s$M) && s$M == 1L) {
    K <- .rs_or(s$K, 0L)
    nk <- nk_landscape(N, K, model = "random", seed = .rs_or(s$seed, 1L))
    inst <- lean_export_model(nk, dir = dir)
    notes <- c(notes, "one-actor restriction exported as a classical NK landscape")
  } else if (!is.na(s$M) && !is.na(N)) {
    th <- list(); W <- NULL
    for (ef in s$effects) {
      nm <- if (ef$effect == "density") "outAct" else ef$effect
      if (nm %in% c("outAct", "inPop", "cycle4")) th[[nm]] <- .rs_or(th[[nm]], 0) + ef$parameter
      else if (nm == "XWX" && !is.null(s$influence_matrix)) W <- ef$parameter * s$influence_matrix
      else notes <- c(notes, paste(ef$effect, "has no Lean statistic; left out of the instance"))
    }
    if (!is.null(s$beta) && is.finite(s$beta)) th <- lapply(th, function(v) v * s$beta)
    if (s$M * N > 8) notes <- c(notes, "M * N > 8: the local-optima count is skipped")
    inst <- suppressWarnings(lean_export_model(list(M = s$M, N = N, E = diag(N), W = W,
                                                    theta = th), dir = dir))
  }
  structure(list(declarations = dd, instance = if (is.null(inst)) NULL else as.character(inst),
                 facts = if (is.null(inst)) NULL else attr(inst, "facts"), notes = notes),
            class = c("rosetta_lean", "list"))
}

print.rosetta_lean <- function(x, ...) {
  cat("<rosetta_lean>\n")
  if (nrow(x$declarations)) for (i in seq_len(nrow(x$declarations)))
    cat(sprintf("  %s\n    %s\n", x$declarations$decl[i],
                if (is.na(x$declarations$statement[i])) "(not in the registry)" else x$declarations$statement[i]))
  if (!is.null(x$instance)) cat("  instance:", x$instance, "\n")
  for (n in x$notes) cat("  note:", n, "\n")
  invisible(x)
}

#' Export the registry as JSON
#'
#' Writes the class registry and the entries in the JSON contract front ends
#' read: `{"schema_version": "1", "searchnet_version", "classes": [{"id",
#' "label", "effects", "k_channel", "reads", "moves",
#' "color", "lean_stat", "description"}],
#' "entries": [{"id", "title", "model", "citations": [{"key", "text",
#' "doi"}], "relation", "statement", "restrictions", "terms": [{"class",
#' "status", "expression", "construct"}], "constructs", "lean",
#' "does_not_cover", "provenance"}]}`. A class's `k_channel` is a deprecated
#' alias of its `moves`.
#'
#' @param path Output file (`NULL` returns the string only).
#' @param include_private Logical; include `entries-private/` (default
#'   `FALSE`).
#' @param pretty Logical; indent.
#' @return The JSON string, invisibly when `path` is given.
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) {
#'   js <- rosetta_export_json()
#'   substr(js, 1, 80)
#' }
rosetta_export_json <- function(path = NULL, include_private = FALSE, pretty = TRUE) {
  cl <- rosetta_classes()
  classes <- lapply(seq_len(nrow(cl)), function(i) list(
    id = cl$id[i], label = cl$label[i],
    effects = I(trimws(strsplit(cl$effects[i], ",")[[1]])),
    k_channel = cl$k_channel[i], reads = cl$reads[i],
    moves = cl$moves[i], color = cl$color[i],
    lean_stat = if (is.na(cl$lean_stat[i])) NULL else cl$lean_stat[i],
    description = cl$description[i]))
  es <- .rosetta_load(include_private)
  entries <- lapply(unname(es), function(e) list(
    id = e$id, title = e$title, model = e$model,
    citations = lapply(e$citations, function(c) list(key = c$key, text = c$text,
                                                      doi = .rs_or(c$doi, NULL))),
    relation = e$relation, statement = e$statement,
    restrictions = .rs_or(e$restrictions, structure(list(), names = character(0))),
    terms = lapply(e$terms, function(t) list(class = t$class, status = t$status,
                                              expression = .rs_or(t$expression, ""),
                                              construct = .rs_or(t$construct, ""))),
    constructs = lapply(.rs_or(e$constructs, structure(list(), names = character(0))),
                        function(v) I(unlist(v))),
    lean = I(as.character(unlist(e$lean))),
    does_not_cover = I(as.character(unlist(e$does_not_cover))),
    provenance = e$provenance))
  ver <- tryCatch(as.character(utils::packageVersion("searchnet")), error = function(e) NA_character_)
  out <- list(schema_version = "1", searchnet_version = ver, classes = classes, entries = entries)
  js <- as.character(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", pretty = pretty,
                                      digits = NA, na = "null"))
  if (!is.null(path)) { writeLines(js, path, useBytes = TRUE); return(invisible(js)) }
  js
}
