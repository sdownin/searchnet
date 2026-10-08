###############################################################################
## rosetta-registry.R
##
## The translation registry: effect classes and entries that state foundational
## search models as restrictions of the SAOM-NK objective, read from
## inst/rosetta (classes.yaml, entries/*.yaml, entries-private/*.yaml).
##
##   rosetta_classes()         the effect-class registry (shipped + registered)
##   rosetta_register_class()  add a class for the current session
##   rosetta_entries()         one row per entry
##   rosetta_entry()           one entry as a list
##   rosetta_validate()        errors (schema, ids, Lean names) and advisory
##                             measurements, kept apart
###############################################################################

.rosetta_state <- new.env(parent = emptyenv())
.rosetta_state$user_classes <- list()
.rosetta_state$k_labels <- list()

.rs_or <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

## "Inf" (any case), "infinity" or an infinite number: the limit restrictions.
.rosetta_is_inf <- function(v) {
  if (is.null(v) || length(v) != 1L) return(FALSE)
  if (is.numeric(v)) return(is.infinite(v))
  tolower(as.character(v)) %in% c("inf", "infinity")
}

.rosetta_need_yaml <- function(what) {
  if (!requireNamespace("yaml", quietly = TRUE))
    stop(what, " needs the 'yaml' package (install.packages(\"yaml\")).",
         call. = FALSE)
}

## Read a registry YAML file without YAML 1.1 boolean coercion of keys.
##
## The yaml package follows YAML 1.1, where y, Y, n, N, yes, no, on and off are
## booleans. A restriction key `N:` was read as FALSE, so the entry's N never
## reached rosetta_model() and the export carried a key "FALSE". Here every
## plain scalar the parser resolves as a boolean is first kept as its source
## text, marked; mapping keys keep the text, so `N:` stays "N". Values are
## then resolved: true/false/yes/no/on/off (any case) become logicals, so
## `ai_drafted: yes` still reads TRUE, while the single letters y/n stay
## strings (YAML 1.2 does not read them as booleans either).
.rosetta_yaml_bool_mark <- function(x) structure(x, class = "rosetta_yaml_bool")

.rosetta_yaml_resolve <- function(v) {
  if (inherits(v, "rosetta_yaml_bool")) {
    s <- tolower(unclass(v))
    if (s %in% c("true", "yes", "on")) return(TRUE)
    if (s %in% c("false", "no", "off")) return(FALSE)
    return(as.character(unclass(v)))
  }
  if (is.list(v)) {
    a <- attributes(v)
    v <- lapply(v, .rosetta_yaml_resolve)
    attributes(v) <- a
  }
  v
}

.rosetta_read_yaml <- function(file) {
  h <- list("bool#yes" = .rosetta_yaml_bool_mark, "bool#no" = .rosetta_yaml_bool_mark)
  .rosetta_yaml_resolve(yaml::read_yaml(file, handlers = h))
}

## Mapping keys that a plain YAML 1.1 reader (yaml::read_yaml without
## handlers, PyYAML) coerces to booleans: they arrive as "TRUE"/"FALSE".
.rosetta_bool_keys <- function(x, where = "") {
  if (!is.list(x)) return(character(0))
  nm <- names(x)
  if (is.null(nm)) nm <- rep("", length(x))
  pre <- if (nzchar(where)) paste0(where, "/") else ""
  hit <- nm[nm %in% c("TRUE", "FALSE")]
  out <- if (length(hit)) paste0(pre, hit) else character(0)
  for (i in seq_along(x))
    out <- c(out, .rosetta_bool_keys(x[[i]], if (nzchar(nm[i])) paste0(pre, nm[i]) else where))
  out
}

## Locate inst/rosetta: option, installed package, then the source tree.
.rosetta_home <- function(path = getOption("searchnet.rosetta_path")) {
  ok <- function(d) !is.null(d) && length(d) == 1L && !is.na(d) && nzchar(d) &&
    file.exists(file.path(d, "classes.yaml"))
  cand <- list(path, Sys.getenv("SEARCHNET_ROSETTA_PATH", ""),
               tryCatch(system.file("rosetta", package = "searchnet"),
                        error = function(e) ""))
  for (d in cand) if (ok(d)) return(normalizePath(d, winslash = "/"))
  d <- normalizePath(getwd(), winslash = "/")
  for (k in 0:6) {
    if (ok(file.path(d, "inst", "rosetta")))
      return(normalizePath(file.path(d, "inst", "rosetta"), winslash = "/"))
    if (ok(d)) return(d)
    up <- dirname(d)
    if (identical(up, d)) break
    d <- up
  }
  stop("translation registry not found: set options(searchnet.rosetta_path = ",
       "<dir containing classes.yaml>).", call. = FALSE)
}

.ROSETTA_K <- c("K_CC", "K_AC", "K_AA", "K_CA", "other")
## A registered class may open a dimension of its own, named K_<letters/digits>.
.ROSETTA_K_PATTERN <- "^(K_[A-Za-z0-9]+|other)$"
.ROSETTA_STATUS <- c("active", "fixed", "zero", "absent", "linearized")
.ROSETTA_RELATION <- c("special-case", "special-case-linearized",
                       "equilibrium-characterization", "translation",
                       "estimation-check", "comparison", "not-carried")
.ROSETTA_ON <- c("active", "fixed", "linearized")
.ROSETTA_NESTING <- c("special-case", "special-case-linearized", "equilibrium-characterization")

.rosetta_moves_of <- function(c) {
  v <- .rs_or(c$moves, .rs_or(c$k_channel, "other"))
  as.character(v)[1]
}

.rosetta_class_df <- function(cls) {
  col <- function(x, f = "") if (is.null(x) || !length(x)) f else x
  data.frame(
    id = vapply(cls, function(c) c$id, ""),
    label = vapply(cls, function(c) col(c$label, c$id), ""),
    effects = vapply(cls, function(c) paste(unlist(c$effects), collapse = ", "), ""),
    simulable = vapply(cls, function(c) paste(unlist(c$simulable), collapse = ", "), ""),
    ## k_channel is a deprecated alias of moves (the v0.12.0 field). A class
    ## read without the two fields falls back on it here so that the figures
    ## still draw; rosetta_validate() reports the omission (E9).
    k_channel = vapply(cls, .rosetta_moves_of, ""),
    reads = vapply(cls, function(c) col(c$reads, .rosetta_moves_of(c)), ""),
    moves = vapply(cls, .rosetta_moves_of, ""),
    color = vapply(cls, function(c) col(c$color, "#7A7A7A"), ""),
    lean_stat = vapply(cls, function(c) if (is.null(c$lean_stat)) NA_character_
                       else as.character(c$lean_stat), ""),
    construct = vapply(cls, function(c) col(c$construct, ""), ""),
    description = vapply(cls, function(c) col(c$description, ""), ""),
    source = vapply(cls, function(c) col(c$source, "shipped"), ""),
    stringsAsFactors = FALSE)
}

.rosetta_class_list <- function(path = getOption("searchnet.rosetta_path")) {
  .rosetta_need_yaml("rosetta_classes()")
  y <- .rosetta_read_yaml(file.path(.rosetta_home(path), "classes.yaml"))
  cls <- y$classes
  user <- .rosetta_state$user_classes
  for (u in user) {
    hit <- which(vapply(cls, function(c) identical(c$id, u$id), TRUE))
    if (length(hit)) cls[[hit]] <- u else cls[[length(cls) + 1L]] <- u
  }
  cls
}

#' Effect classes of the SAOM-NK objective
#'
#' The class registry the translation functions use: each class groups
#' searchnet effect names into one summand of the objective, names two
#' \{K\} dimensions, a color token, and the statistic of the Lean library
#' that carries it. The two fields are those of
#' [searchnet_effect_dimensions()]: `reads`, the dimension the change
#' statistic depends on (what the deciding actor reads), and `moves`, the
#' dimension of the target statistic (the moment estimation matches and the
#' coefficient moves first). `k_channel` is a deprecated alias of `moves`,
#' kept for the v0.12.0 contract. Shipped classes are read from
#' `inst/rosetta/classes.yaml`; classes added with
#' [rosetta_register_class()] are appended (or replace a shipped class with
#' the same id) for the current session.
#'
#' @param path Directory holding `classes.yaml` (default: the option
#'   `searchnet.rosetta_path`, else the installed package).
#' @return A data.frame with columns `id`, `label`, `effects`, `simulable`,
#'   `k_channel` (deprecated alias of `moves`), `reads`, `moves`,
#'   `color`, `lean_stat`, `construct`, `description`, `source`.
#' @seealso [rosetta_register_class()], [rosetta_translate()]
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) rosetta_classes()[, 1:5]
rosetta_classes <- function(path = getOption("searchnet.rosetta_path")) {
  .rosetta_class_df(.rosetta_class_list(path))
}

## Display names of the {K} dimensions (K_CC = "Epistasis", ...): the
## `k_channel_label` map of classes.yaml, then labels given at registration.
.rosetta_k_labels <- function(path = getOption("searchnet.rosetta_path")) {
  .rosetta_need_yaml("the {K} dimension labels")
  y <- .rosetta_read_yaml(file.path(.rosetta_home(path), "classes.yaml"))
  lab <- unlist(.rs_or(y$k_channel_label, list()))
  if (is.null(lab)) lab <- character(0)
  user <- unlist(.rosetta_state$k_labels)
  if (length(user)) lab[names(user)] <- user
  lab
}

#' Register an effect class for the current session
#'
#' Adds a class to the registry (or replaces a class with the same `id`), so
#' that a researcher's own effects are decomposed, compared, plotted and
#' exported like the shipped ones. Registration lasts for the R session; to
#' make a class permanent add it to `inst/rosetta/classes.yaml`.
#'
#' @param id Stable key (lowercase letters, digits, underscores).
#' @param label Chip text (default `id`).
#' @param effects Character vector of searchnet effect names in the class.
#' @param k_channel Deprecated alias of `moves` (the v0.12.0 argument): one
#'   of `"K_CC"`, `"K_AC"`, `"K_AA"`, `"K_CA"`, `"other"`, or a new dimension
#'   named `"K_"` followed by letters or digits (for example `"K_AL"`).
#'   Classes that share a dimension are grouped under one badge by
#'   [rosetta_plot()].
#' @param color Hex color token.
#' @param lean_stat Lean statistic carrying the class, or `NULL`.
#' @param construct Default construct label.
#' @param description One line.
#' @param simulable Effects [saomnk_run()] can simulate (default: none).
#' @param k_label Display name of the dimension shown on its badge (for
#'   example `"Legitimacy"`), or `NULL` to keep the shipped name, if any.
#' @param reads The dimension the class's change statistic reads (same
#'   vocabulary as `k_channel`); default: `moves`.
#' @param moves The dimension its target statistic moves; default
#'   `k_channel`. When given, `k_channel` is set to it.
#' @return Invisibly, the registered class as a list.
#' @seealso [rosetta_classes()], [rosetta_reset_classes()]
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) {
#'   rosetta_register_class("prestige", effects = "altX", k_channel = "K_CA",
#'                          construct = "status of components")
#'   rosetta_classes()$id
#'   rosetta_reset_classes()
#' }
rosetta_register_class <- function(id, label = id, effects, k_channel = "other",
                                   color = "#7A7A7A", lean_stat = NULL,
                                   construct = "", description = "",
                                   simulable = character(0), k_label = NULL,
                                   reads = NULL, moves = NULL) {
  stopifnot(is.character(id), length(id) == 1L, grepl("^[a-z][a-z0-9_]*$", id))
  stopifnot(is.character(effects), length(effects) >= 1L)
  ok_k <- function(v) is.character(v) && length(v) == 1L && grepl(.ROSETTA_K_PATTERN, v)
  if (!ok_k(k_channel))
    stop("k_channel must be one of ", paste(.ROSETTA_K, collapse = ", "),
         ", or a new dimension named K_ followed by letters or digits", call. = FALSE)
  if (!is.null(moves)) {
    if (!ok_k(moves)) stop("moves: same vocabulary as k_channel", call. = FALSE)
    if (!missing(k_channel) && !identical(k_channel, moves))
      stop("k_channel is a deprecated alias of moves; give one value or two equal ones", call. = FALSE)
    k_channel <- moves
  }
  if (is.null(reads)) reads <- k_channel
  if (!ok_k(reads)) stop("reads: same vocabulary as k_channel", call. = FALSE)
  if (!is.null(k_label)) {
    stopifnot(is.character(k_label), length(k_label) == 1L)
    .rosetta_state$k_labels[[k_channel]] <- k_label
  }
  if (!grepl("^#[0-9A-Fa-f]{6}$", color)) stop("color must be a hex code like #4B4FA6", call. = FALSE)
  cl <- list(id = id, label = label, effects = as.list(effects),
             simulable = as.list(intersect(simulable, effects)),
             k_channel = k_channel, reads = reads,
             moves = k_channel, color = color, lean_stat = lean_stat,
             construct = construct, description = description, source = "registered")
  .rosetta_state$user_classes[[id]] <- cl
  invisible(cl)
}

#' @rdname rosetta_register_class
#' @export
rosetta_reset_classes <- function() {
  .rosetta_state$user_classes <- list()
  .rosetta_state$k_labels <- list()
  invisible(NULL)
}

## effect name -> class id, from the registry
.rosetta_effect_class <- function(effect, classes = rosetta_classes()) {
  out <- rep("other", length(effect))
  for (i in seq_len(nrow(classes))) {
    eff <- trimws(strsplit(classes$effects[i], ",")[[1]])
    out[effect %in% eff & out == "other"] <- classes$id[i]
  }
  out
}

## --------------------------------------------------------------------------- #
## Entries                                                                       #
## --------------------------------------------------------------------------- #

.rosetta_entry_files <- function(include_private = FALSE,
                                 path = getOption("searchnet.rosetta_path")) {
  home <- .rosetta_home(path)
  dirs <- c(public = file.path(home, "entries"))
  if (isTRUE(include_private)) dirs <- c(dirs, private = file.path(home, "entries-private"))
  out <- character(0)
  for (nm in names(dirs)) {
    d <- dirs[[nm]]
    if (!dir.exists(d)) next
    f <- sort(list.files(d, pattern = "[.]ya?ml$", full.names = TRUE))
    names(f) <- rep(nm, length(f))
    out <- c(out, f)
  }
  out
}

.rosetta_read_entry <- function(file, visibility = "public") {
  e <- .rosetta_read_yaml(file)
  attr(e, "file") <- normalizePath(file, winslash = "/")
  attr(e, "visibility") <- visibility
  e
}

.rosetta_load <- function(include_private = FALSE,
                          path = getOption("searchnet.rosetta_path")) {
  .rosetta_need_yaml("The translation registry")
  f <- .rosetta_entry_files(include_private, path)
  out <- lapply(seq_along(f), function(i) .rosetta_read_entry(f[[i]], names(f)[i]))
  names(out) <- vapply(out, function(e) .rs_or(e$id, NA_character_), "")
  out
}

#' Entries of the translation registry
#'
#' One row per foundational model stated as a restriction (or other relation)
#' of the SAOM-NK objective. Shipped entries live in `inst/rosetta/entries`;
#' entries kept out of public releases live in `inst/rosetta/entries-private`
#' and are read only with `include_private = TRUE` (silently skipped when the
#' directory is absent).
#'
#' @param include_private Logical; also read `entries-private/`.
#' @param path Registry directory (default: option `searchnet.rosetta_path`,
#'   else the installed package).
#' @return A data.frame with columns `id`, `title`, `model`, `relation`,
#'   `active` (classes switched on), `off` (classes zero or absent),
#'   `citations`, `lean` (number of Lean declarations), `visibility`.
#' @seealso [rosetta_entry()], [rosetta_model()], [rosetta_validate()]
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) rosetta_entries()[, 1:4]
rosetta_entries <- function(include_private = FALSE,
                            path = getOption("searchnet.rosetta_path")) {
  es <- .rosetta_load(include_private, path)
  if (!length(es))
    return(data.frame(id = character(0), title = character(0), model = character(0),
                      relation = character(0), active = character(0), off = character(0),
                      citations = character(0), lean = integer(0),
                      visibility = character(0), stringsAsFactors = FALSE))
  st <- function(e, on) {
    t <- e$terms
    if (!length(t)) return("")
    s <- vapply(t, function(x) .rs_or(x$status, "absent"), "")
    c <- vapply(t, function(x) .rs_or(x$class, "other"), "")
    paste(c[if (on) s %in% .ROSETTA_ON else !s %in% .ROSETTA_ON], collapse = ", ")
  }
  data.frame(
    id = vapply(es, function(e) .rs_or(e$id, NA_character_), ""),
    title = vapply(es, function(e) .rs_or(e$title, ""), ""),
    model = vapply(es, function(e) .rs_or(e$model, ""), ""),
    relation = vapply(es, function(e) .rs_or(e$relation, ""), ""),
    active = vapply(es, st, "", on = TRUE),
    off = vapply(es, st, "", on = FALSE),
    citations = vapply(es, function(e) paste(vapply(e$citations, function(c)
      .rs_or(c$key, ""), ""), collapse = ", "), ""),
    lean = vapply(es, function(e) length(e$lean), 1L),
    visibility = vapply(es, function(e) attr(e, "visibility"), ""),
    row.names = NULL, stringsAsFactors = FALSE)
}

#' One entry of the translation registry
#'
#' @param id Entry id (kebab-case, the file name without `.yaml`).
#' @param include_private Logical; also search `entries-private/`.
#' @param path Registry directory.
#' @return The entry as a list of class `rosetta_entry`.
#' @seealso [rosetta_entries()]
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) rosetta_entry("nk-adaptive-walk")$title
rosetta_entry <- function(id, include_private = TRUE,
                          path = getOption("searchnet.rosetta_path")) {
  stopifnot(is.character(id), length(id) == 1L)
  es <- .rosetta_load(include_private, path)
  if (!id %in% names(es))
    stop("no registry entry '", id, "'. Known: ",
         paste(names(es), collapse = ", "), call. = FALSE)
  structure(es[[id]], class = c("rosetta_entry", "list"))
}

print.rosetta_entry <- function(x, ...) {
  cat(sprintf("<rosetta_entry> %s\n  %s (%s)\n", x$id, x$title, x$relation))
  cat("  ", strwrap(x$statement, 76, prefix = "  "), sep = "\n")
  for (t in x$terms)
    cat(sprintf("  %-11s %-10s %s\n", t$class, t$status, .rs_or(t$expression, "")))
  if (length(x$lean)) cat("  Lean:", paste(unlist(x$lean), collapse = ", "), "\n")
  invisible(x)
}

## --------------------------------------------------------------------------- #
## Validation                                                                     #
## --------------------------------------------------------------------------- #

.ROSETTA_BRITISH <- c("organis", "recognis", "characteris", "summaris", "generalis",
                      "normalis", "standardis", "operationalis", "parameteris",
                      "theoris", "conceptualis", "emphasis(e|ing)", "analys(e|ing)\\b",
                      "labell", "modell", "behaviour", "colour", "favour", "defence",
                      "centre", "whilst", "amongst", "towards", "cancell", "travell")

.rosetta_lean_known <- function() {
  home <- tryCatch(lean_home(), error = function(e) NA_character_)
  if (is.na(home)) return(NULL)
  d <- tryCatch(lean_declarations(home, kinds = c("theorem", "lemma", "def")),
                error = function(e) NULL)
  r <- tryCatch(lean_registry()$decl, error = function(e) character(0))
  unique(c(if (!is.null(d)) d$decl, r))
}

.rosetta_entry_strings <- function(x) {
  if (is.list(x)) unlist(lapply(x, .rosetta_entry_strings), use.names = FALSE)
  else if (is.character(x)) x else character(0)
}

#' Validate the translation registry
#'
#' Checks every entry and separates two kinds of finding, following the rule
#' that a check measuring someone's work reports and never demands:
#'
#' \describe{
#'   \item{errors}{a violation with a determinate fix. E1 the entry fails the
#'     schema (required fields, types, the relation, status and K-dimension
#'     vocabularies); E2 the id differs from its file name or repeats; E3 a
#'     Lean declaration named in `lean` is not in the package's Lean library;
#'     E4 a term names a class that is not registered; E5 an em-dash or a
#'     section symbol; E6 British spelling; E7 a check names an R function
#'     that does not exist or a test file that is missing; E8 a mapping key
#'     that a YAML 1.1 reader coerces to a boolean (an unquoted `N`, `Y`,
#'     `yes`, `no`, `on` or `off`); E9 a class in `classes.yaml` (or a
#'     registered class) lacks `reads` or `moves`, names a dimension outside
#'     the vocabulary, or carries a `k_channel` (deprecated alias) that
#'     differs from its `moves`. E9 rows carry the id `class:<id>`.}
#'   \item{advisory}{measurements, reported only. A1 AI-drafted and not yet
#'     reviewed; A2 a citation without a DOI; A3 a special case with an empty
#'     `does_not_cover`; A4 a registered class the entry does not mention;
#'     A5 the Lean library could not be located, so E3 was not run; A6 a
#'     test file is named but no test directory is present (an installed
#'     package), so it was not checked.}
#' }
#'
#' @param path Registry directory.
#' @param include_private Logical; also validate `entries-private/`.
#' @return A data.frame of class `rosetta_validation` with columns `id`,
#'   `level` (`"error"` or `"advisory"`), `code`, `message`; attribute `ok`
#'   is `TRUE` when there are no errors.
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) {
#'   v <- rosetta_validate()
#'   attr(v, "ok")
#' }
rosetta_validate <- function(path = getOption("searchnet.rosetta_path"),
                             include_private = TRUE) {
  .rosetta_need_yaml("rosetta_validate()")
  files <- .rosetta_entry_files(include_private, path)
  classes <- rosetta_classes(path)
  lean_known <- .rosetta_lean_known()
  ## A registry of one's own may omit schema.yaml; the package's then applies.
  sf <- file.path(.rosetta_home(path), "schema.yaml")
  if (!file.exists(sf)) sf <- file.path(.rosetta_home(), "schema.yaml")
  sch <- .rosetta_read_yaml(sf)
  req <- unlist(sch$required)
  rel_ok <- names(sch$relation); st_ok <- names(sch$status)
  w_ok <- unlist(sch$W); rev_ok <- unlist(sch$revision_rule)
  rows <- list()
  add <- function(id, level, code, msg)
    rows[[length(rows) + 1L]] <<- data.frame(id = id, level = level, code = code,
                                             message = msg, stringsAsFactors = FALSE)
  if (is.null(lean_known))
    add("(registry)", "advisory", "A5", "Lean library not found; Lean names were not checked")
  ## E9: every class carries both {K} fields, reads and moves.
  for (cl in .rosetta_class_list(path)) {
    cid <- paste0("class:", .rs_or(cl$id, "?"))
    for (fld in c("reads", "moves")) {
      v <- cl[[fld]]
      if (is.null(v) || !length(v) || !nzchar(as.character(v)[1]))
        add(cid, "error", "E9", paste("missing field", fld))
      else if (!grepl(.ROSETTA_K_PATTERN, as.character(v)[1]))
        add(cid, "error", "E9", paste0(fld, " outside the vocabulary: ", v))
    }
    if (!is.null(cl$k_channel) && !is.null(cl$moves) &&
        !identical(as.character(cl$k_channel), as.character(cl$moves)))
      add(cid, "error", "E9", "k_channel differs from moves (k_channel is a deprecated alias of moves)")
  }
  seen <- character(0)
  ## Test files named in `checks` resolve against the source tree holding this
  ## registry, or the package's own source tree when the registry is a copy.
  pkg_root <- unique(c(dirname(dirname(.rosetta_home(path))),
                       tryCatch(dirname(dirname(.rosetta_home())), error = function(e) NULL)))
  for (k in seq_along(files)) {
    f <- files[[k]]
    e <- tryCatch(.rosetta_read_yaml(f), error = function(err) err)
    fid <- sub("[.]ya?ml$", "", basename(f))
    if (inherits(e, "error")) { add(fid, "error", "E1", paste("YAML does not parse:", conditionMessage(e))); next }
    id <- .rs_or(e$id, fid)
    ## E8: a key a plain YAML 1.1 reader coerces to a boolean (N, Y, yes, no,
    ## on, off unquoted). The package's own loader keeps it, but any other
    ## reader of the file (and the export of an older searchnet) would not.
    bk <- .rosetta_bool_keys(tryCatch(yaml::read_yaml(f), error = function(err) NULL))
    if (length(bk))
      add(id, "error", "E8", paste0("key(s) read as boolean by a YAML 1.1 reader, quote them ",
                                    "(for example \"N\": null): ", paste(bk, collapse = ", ")))
    miss <- setdiff(req, names(e))
    if (length(miss)) add(id, "error", "E1", paste("missing field(s):", paste(miss, collapse = ", ")))
    if (!is.null(e$schema_version) && !identical(as.character(e$schema_version), "1"))
      add(id, "error", "E1", "schema_version must be \"1\"")
    if (!is.null(e$id) && !grepl("^[a-z0-9]+(-[a-z0-9]+)*$", e$id))
      add(id, "error", "E1", "id must be kebab-case")
    if (!is.null(e$relation) && !e$relation %in% rel_ok)
      add(id, "error", "E1", paste("unknown relation", e$relation))
    for (c in e$citations) {
      if (is.null(c$key) || is.null(c$text))
        add(id, "error", "E1", "each citation needs key and text")
      else if (is.null(c$doi) || !nzchar(.rs_or(c$doi, "")))
        add(id, "advisory", "A2", paste("citation without DOI:", c$key))
    }
    r <- e$restrictions
    if (!is.null(r)) {
      if (!is.null(r$revision_rule) && !r$revision_rule %in% rev_ok)
        add(id, "error", "E1", paste("unknown revision_rule", r$revision_rule))
      if (!is.null(r$W) && !r$W %in% w_ok)
        add(id, "error", "E1", paste("unknown W form", r$W))
    }
    if (!is.list(e$terms) || !length(e$terms)) add(id, "error", "E1", "terms must be a non-empty list")
    tcls <- character(0)
    for (t in e$terms) {
      if (is.null(t$class) || is.null(t$status)) { add(id, "error", "E1", "each term needs class and status"); next }
      tcls <- c(tcls, t$class)
      if (!t$status %in% st_ok) add(id, "error", "E1", paste("unknown status", t$status, "for", t$class))
      if (!t$class %in% classes$id) add(id, "error", "E4", paste("unregistered class", t$class))
    }
    if (any(duplicated(tcls))) add(id, "error", "E1", "a class appears twice in terms")
    absent <- setdiff(classes$id, tcls)
    if (length(absent)) add(id, "advisory", "A4", paste("classes not mentioned (read as absent):",
                                                       paste(absent, collapse = ", ")))
    if (!is.null(e$constructs)) for (cn in names(e$constructs))
      if (!all(unlist(e$constructs[[cn]]) %in% tcls))
        add(id, "error", "E1", paste("construct", shQuote(cn), "names a class not in terms"))
    if (!identical(id, fid)) add(id, "error", "E2", paste("id differs from file name", basename(f)))
    if (id %in% seen) add(id, "error", "E2", "id used twice")
    seen <- c(seen, id)
    if (!is.null(lean_known)) for (d in unlist(e$lean)) {
      if (!(d %in% lean_known || any(endsWith(lean_known, paste0(".", d)))))
        add(id, "error", "E3", paste("Lean declaration not in the package library:", d))
    }
    for (ch in unlist(e$checks)) {
      if (grepl("[.]R$", ch)) {
        roots <- pkg_root[dir.exists(file.path(pkg_root, dirname(ch)))]
        if (!length(roots))
          add(id, "advisory", "A6", paste("test directory not present here; not checked:", ch))
        else if (!any(file.exists(file.path(roots, ch))))
          add(id, "error", "E7", paste("check file not found:", ch))
      } else if (!exists(ch, mode = "function"))
        add(id, "error", "E7", paste("check function not found:", ch))
    }
    ## Citation text keeps its source's spelling, so it is not scanned for E6.
    txt <- .rosetta_entry_strings(e[setdiff(names(e), "citations")])
    if (any(grepl("\u2014|\u00a7", txt))) add(id, "error", "E5", "em-dash or section symbol")
    for (b in .ROSETTA_BRITISH) if (any(grepl(b, txt, ignore.case = TRUE, perl = TRUE)))
      add(id, "error", "E6", paste("British spelling matching", b))
    pv <- e$provenance
    if (isTRUE(pv$ai_drafted) && !isTRUE(pv$reviewed))
      add(id, "advisory", "A1", "AI-drafted, not yet reviewed")
    if (identical(e$relation, "special-case") && !length(e$does_not_cover))
      add(id, "advisory", "A3", "special case with an empty does_not_cover")
  }
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(id = character(0), level = character(0), code = character(0),
               message = character(0), stringsAsFactors = FALSE)
  structure(out, class = c("rosetta_validation", "data.frame"),
            ok = !any(out$level == "error"), n_entries = length(files))
}

print.rosetta_validation <- function(x, ...) {
  err <- x[x$level == "error", , drop = FALSE]
  adv <- x[x$level == "advisory", , drop = FALSE]
  cat(sprintf("Registry check: %d entries, %d error(s), %d advisory measurement(s)\n",
              attr(x, "n_entries"), nrow(err), nrow(adv)))
  if (nrow(err)) {
    cat("Errors (each has a determinate fix):\n")
    for (i in seq_len(nrow(err))) cat(sprintf("  [%s] %s: %s\n", err$code[i], err$id[i], err$message[i]))
  }
  if (nrow(adv)) {
    cat("Advisory (measurements, for information):\n")
    tab <- table(adv$code)
    for (cd in names(tab)) cat(sprintf("  %s: %d\n", cd, tab[[cd]]))
  }
  invisible(x)
}
