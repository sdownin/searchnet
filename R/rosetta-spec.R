###############################################################################
## rosetta-spec.R
##
## The model specification shared by the translation functions, the JSON
## interface and the simulation engine:
##
##   list(M, N, K, influence_matrix, effects = list(list(class, effect,
##        parameter, fix)), revision_rule, beta, seed)
##
##   rosetta_model()            an entry's restriction as a runnable spec
##   rosetta_run()              simulate a spec (saomnk_run, or nk_walk at M = 1)
##   rosetta_saomnk_model()     spec -> saomnk_model()
##   rosetta_model_to_json()    spec / saomnk_model / env -> JSON
##   rosetta_model_from_json()  JSON (path, string or list) -> spec
##   rosetta_r_code()           spec -> equivalent searchnet R code
###############################################################################

.rosetta_spec <- function(M, N, K = NULL, influence_matrix = NULL, effects = list(),
                          revision_rule = "multinomial", beta = NULL, seed = NULL,
                          source = NULL) {
  if (!is.null(influence_matrix)) {
    influence_matrix <- as.matrix(influence_matrix)
    dimnames(influence_matrix) <- NULL
  }
  structure(list(M = if (is.null(M)) NA_integer_ else as.integer(M),
                 N = if (is.null(N)) NA_integer_ else as.integer(N),
                 K = if (is.null(K)) NULL else as.integer(K),
                 influence_matrix = influence_matrix, effects = effects,
                 revision_rule = revision_rule, beta = beta, seed = seed,
                 source = source),
            class = c("rosetta_spec", "list"))
}

print.rosetta_spec <- function(x, ...) {
  cat(sprintf("<rosetta_spec> M = %s, N = %s%s, revision = %s, beta = %s%s\n",
              x$M, x$N, if (!is.null(x$K)) paste0(", K = ", x$K) else "",
              x$revision_rule, if (is.null(x$beta)) "1 (engine scale)" else format(x$beta),
              if (!is.null(x$source)) paste0("  [from ", x$source, "]") else ""))
  if (!is.null(x$influence_matrix))
    cat(sprintf("  W: %d x %d, %d nonzero off-diagonal entries\n", nrow(x$influence_matrix),
                ncol(x$influence_matrix),
                sum(x$influence_matrix != 0) - sum(diag(x$influence_matrix) != 0)))
  for (e in x$effects)
    cat(sprintf("  %-11s %-20s %s\n", e$class, e$effect, format(e$parameter)))
  invisible(x)
}

## binary K-regular W, the NK influence pattern off the diagonal. The diagonal
## is 0: the engine's XWX statistic sums over j != h (R/saomnk-base.R), so a
## 1 on the diagonal never entered the objective, and K counts each row's
## other components (rowSums(W) == K). nk_landscape()'s influence_matrix
## carries the diagonal (a component's own contribution), so it equals this W
## plus the identity.
.rosetta_binary_W <- function(N, K, seed = NULL, model = "random") {
  if (!is.null(seed)) set.seed(seed)
  deps <- nk_dependencies(N, K, model)
  W <- matrix(0, N, N)
  for (i in seq_len(N)) W[i, deps[[i]]] <- 1
  diag(W) <- 0
  W
}

## K of an influence matrix: off-diagonal nonzeros per row (the maximum).
.rosetta_W_K <- function(W) {
  E <- (W != 0) * 1
  diag(E) <- 0
  as.integer(max(rowSums(E)))
}

## real-valued W: modular blocks with signed, heterogeneous magnitudes
.rosetta_real_W <- function(N, K = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  K <- if (is.null(K)) max(1L, floor(N / 3)) else K
  blocks <- max(1L, round(N / (K + 1)))
  B <- saomnk_block_diagonal(N, blocks)
  extra <- matrix(stats::rbinom(N * N, 1, 0.1), N, N)
  pat <- ((B + extra) > 0) * 1
  mag <- matrix(stats::rlnorm(N * N, 0, 0.6), N, N)
  sgn <- matrix(sample(c(1, -1), N * N, replace = TRUE, prob = c(0.75, 0.25)), N, N)
  W <- pat * mag * sgn
  diag(W) <- 0
  round(W, 3)
}

.rosetta_default_value <- function(class) {
  switch(class, complementarity = 1, scope = -0.5, crowding = -0.2, contact = 0.1,
         imitation = 0.2, 0.1)
}

#' Generate an entry's restriction as a runnable model specification
#'
#' Turns a registry entry into a specification that searchnet can simulate:
#' every class the entry switches on gets its default (or supplied) parameter,
#' every class it sets to zero or leaves absent is omitted, and the entry's
#' restrictions fix the number of actors, the influence matrix and the
#' revision rule. Run it with [rosetta_run()], convert it with
#' [rosetta_saomnk_model()], or print the equivalent code with
#' [rosetta_r_code()].
#'
#' Restrictions are applied as follows. `M`: a number in the entry fixes it,
#' `"Inf"` (the many-actor limit) uses the `M` argument (default 20). `W`: `"binary-k-regular"`
#' draws the NK influence pattern with `K` partners per component (the
#' pattern [nk_landscape()] draws with the same seed, with the diagonal set
#' to 0 because the XWX statistic sums over \eqn{j \neq h}{j != h}, so each
#' row sums to `K`), `"real"` or `"any"`
#' draws a modular real-valued matrix, `"zero"` omits it. `beta`:
#' `"Inf"` is kept as `Inf` in the specification; at `M = 1`
#' [rosetta_run()] then runs the steepest-ascent adaptive walk, and at
#' `M >= 2` it scales the parameters by `beta_large`.
#'
#' @param id Entry id (see [rosetta_entries()]).
#' @param M,N,K Sizes; `NULL` takes the entry's restriction or a default
#'   (`M = 3`, `N = 6`, `K = 2`).
#' @param parameters Optional named numeric vector, one value per class id,
#'   overriding the entry's values for classes it switches on.
#' @param seed Integer seed for the influence matrix and for runs.
#' @param include_private Logical; also look in `entries-private/`.
#' @param path Registry directory.
#' @return A `rosetta_spec` list with elements `M`, `N`, `K`,
#'   `influence_matrix`, `effects`, `revision_rule`, `beta`, `seed`.
#' @seealso [rosetta_run()], [rosetta_translate()], [rosetta_r_code()]
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) {
#'   s <- rosetta_model("nk-adaptive-walk", N = 6, K = 2)
#'   s
#' }
rosetta_model <- function(id, M = NULL, N = NULL, K = NULL, parameters = NULL,
                          seed = 1L, include_private = TRUE,
                          path = getOption("searchnet.rosetta_path")) {
  e <- if (inherits(id, "rosetta_entry")) id else rosetta_entry(id, include_private, path)
  r <- .rs_or(e$restrictions, list())
  num <- function(v) !is.null(v) && is.numeric(v) && length(v) == 1L && is.finite(v)
  if (is.null(M)) {
    M <- if (num(r$M)) r$M else if (.rosetta_is_inf(r$M)) 20L else 3L
  } else if (num(r$M) && M != r$M) {
    warning("entry ", e$id, " restricts M to ", r$M, "; using M = ", M,
            " leaves the restriction", call. = FALSE)
  }
  if (is.null(N)) N <- if (num(r$N)) r$N else 6L
  if (is.null(K)) K <- if (num(r$K)) r$K else min(2L, N - 1L)
  stopifnot(N >= 1, K >= 0, K <= N - 1)
  terms <- e$terms
  cls <- rosetta_classes(path)
  effects <- list()
  W <- NULL
  for (t in terms) {
    if (!t$status %in% .ROSETTA_ON) next
    if (!t$class %in% cls$id) next
    val <- if (!is.null(parameters) && t$class %in% names(parameters)) parameters[[t$class]]
           else if (num(t$value)) t$value else .rosetta_default_value(t$class)
    eff <- .rs_or(t$effect, NULL)
    if (is.null(eff)) {
      row <- cls[cls$id == t$class, ]
      sim <- trimws(strsplit(row$simulable, ",")[[1]])
      all <- trimws(strsplit(row$effects, ",")[[1]])
      eff <- if (length(sim) && nzchar(sim[1])) sim[1] else all[1]
    }
    effects[[length(effects) + 1L]] <- list(class = t$class, effect = eff,
                                            parameter = as.numeric(val), fix = TRUE)
    if (t$class == "complementarity") {
      wform <- .rs_or(r$W, "real")
      W <- switch(wform,
                  "binary-k-regular" = .rosetta_binary_W(N, K, seed),
                  "zero" = NULL,
                  .rosetta_real_W(N, K, seed))
    }
  }
  beta <- r$beta
  beta <- if (.rosetta_is_inf(beta)) Inf else if (is.numeric(beta)) beta else NULL
  rev <- .rs_or(r$revision_rule, if (identical(beta, Inf)) "single_flip" else "multinomial")
  Kout <- if (!is.null(W) && identical(r$W, "binary-k-regular")) K else NULL
  .rosetta_spec(M = M, N = N, K = Kout, influence_matrix = W, effects = effects,
                revision_rule = rev, beta = beta, seed = seed, source = e$id)
}

## --------------------------------------------------------------------------- #
## Coercion: anything model-like -> rosetta_spec                                  #
## --------------------------------------------------------------------------- #

.rosetta_effects_from_sm <- function(sm, classes) {
  dv <- sm$dv_bipartite
  if (is.null(dv)) return(list(effects = list(), W = NULL))
  out <- list(); W <- NULL
  for (e in c(dv$effects, dv$coCovars)) {
    out[[length(out) + 1L]] <- list(class = .rosetta_effect_class(e$effect, classes),
                                    effect = e$effect, parameter = as.numeric(e$parameter),
                                    fix = isTRUE(.rs_or(e$fix, TRUE)))
  }
  for (e in c(dv$coDyadCovars, dv$varDyadCovars)) {
    out[[length(out) + 1L]] <- list(class = .rosetta_effect_class(e$effect, classes),
                                    effect = e$effect, parameter = as.numeric(e$parameter),
                                    fix = isTRUE(.rs_or(e$fix, TRUE)))
    if (identical(e$effect, "XWX") && is.null(W) && !is.null(e$x)) {
      x <- e$x
      W <- if (length(dim(x)) == 3L) x[, , 1] else as.matrix(x)
    }
  }
  list(effects = out, W = W)
}

.rosetta_as_spec <- function(x, include_private = TRUE,
                             path = getOption("searchnet.rosetta_path")) {
  if (inherits(x, "rosetta_spec")) return(x)
  classes <- rosetta_classes(path)
  if (inherits(x, "rosetta_entry")) return(rosetta_model(x, path = path))
  if (is.character(x) && length(x) == 1L) {
    ids <- rosetta_entries(include_private, path)$id
    if (x %in% ids) return(rosetta_model(x, include_private = include_private, path = path))
    return(rosetta_model_from_json(x))
  }
  if (inherits(x, "saomnk_model")) {
    p <- .rosetta_effects_from_sm(x, classes)
    N <- if (!is.null(p$W)) nrow(p$W) else NA_integer_
    return(.rosetta_spec(M = NA_integer_, N = N, influence_matrix = p$W,
                         effects = p$effects, source = "saomnk_model"))
  }
  if (inherits(x, "SaomNkRSienaBiEnv")) {
    p <- .rosetta_effects_from_sm(x$config_structure_model, classes)
    return(.rosetta_spec(M = x$M, N = x$N, influence_matrix = p$W, effects = p$effects,
                         seed = x$rand_seed, source = "SaomNkRSienaBiEnv"))
  }
  if (is.list(x) && !is.null(x$effects)) return(rosetta_model_from_json(x))
  stop("cannot read a model from an object of class ", class(x)[1],
       "; supply an entry id, a rosetta_spec, a saomnk_model, a SaomNkRSienaBiEnv, ",
       "or a JSON model specification.", call. = FALSE)
}

## --------------------------------------------------------------------------- #
## JSON                                                                            #
## --------------------------------------------------------------------------- #

#' Model specifications as JSON
#'
#' The JSON form of a model, shared with front ends:
#' `{"M", "N", "K", "influence_matrix": [[...]] or null, "effects": [{"class",
#' "effect", "parameter", "fix"}], "revision_rule": "single_flip" |
#' "multinomial", "beta": number or null, "seed"}`. An infinite `beta` is
#' written as the string `"Inf"`.
#'
#' `rosetta_model_from_json()` accepts a file path, a JSON string, or the
#' parsed list. Effects without a `class` are classified from the registry.
#'
#' @param x For `rosetta_model_from_json()`: path, JSON string or list. For
#'   `rosetta_model_to_json()`: a `rosetta_spec`, entry id, `saomnk_model` or
#'   `SaomNkRSienaBiEnv`.
#' @param path Optional file to write.
#' @param pretty Logical; indent the JSON.
#' @return `rosetta_model_from_json()`: a `rosetta_spec`.
#'   `rosetta_model_to_json()`: the JSON string (invisibly when `path` is
#'   given).
#' @seealso [rosetta_model()], [rosetta_export_json()]
#' @export
#' @examples
#' js <- '{"M": 3, "N": 4, "effects": [{"effect": "inPop", "parameter": -0.2}]}'
#' if (requireNamespace("yaml", quietly = TRUE)) rosetta_model_from_json(js)
rosetta_model_from_json <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    x <- if (file.exists(x)) jsonlite::fromJSON(x, simplifyVector = FALSE)
         else jsonlite::fromJSON(x, simplifyVector = FALSE)
  }
  if (!is.list(x)) stop("rosetta_model_from_json(): expected a path, JSON string or list", call. = FALSE)
  classes <- rosetta_classes()
  W <- x$influence_matrix
  if (!is.null(W) && length(W)) {
    W <- if (is.matrix(W)) W else do.call(rbind, lapply(W, function(r) as.numeric(unlist(r))))
  } else W <- NULL
  effs <- x$effects
  if (is.data.frame(effs)) effs <- lapply(seq_len(nrow(effs)), function(i) as.list(effs[i, ]))
  effs <- lapply(effs, function(e) {
    eff <- as.character(e$effect)
    list(class = .rs_or(as.character(e$class), .rosetta_effect_class(eff, classes)),
         effect = eff, parameter = as.numeric(.rs_or(e$parameter, 0)),
         fix = if (is.null(e$fix)) TRUE else isTRUE(as.logical(e$fix)))
  })
  beta <- x$beta
  if (!is.null(beta)) beta <- if (.rosetta_is_inf(beta)) Inf else as.numeric(beta)
  N <- .rs_or(x$N, if (!is.null(W)) nrow(W) else NA_integer_)
  .rosetta_spec(M = .rs_or(x$M, NA_integer_), N = N, K = x$K, influence_matrix = W,
                effects = effs, revision_rule = .rs_or(x$revision_rule, "multinomial"),
                beta = beta, seed = x$seed, source = .rs_or(x$source, "json"))
}

#' @rdname rosetta_model_from_json
#' @export
rosetta_model_to_json <- function(x, path = NULL, pretty = TRUE) {
  s <- .rosetta_as_spec(x)
  out <- list(
    M = if (is.na(s$M)) NULL else s$M, N = if (is.na(s$N)) NULL else s$N,
    K = s$K,
    influence_matrix = if (is.null(s$influence_matrix)) NULL
                       else lapply(seq_len(nrow(s$influence_matrix)), function(i) s$influence_matrix[i, ]),
    effects = lapply(s$effects, function(e) list(class = e$class, effect = e$effect,
                                                  parameter = e$parameter, fix = isTRUE(e$fix))),
    revision_rule = s$revision_rule,
    beta = if (is.null(s$beta)) NULL else if (is.infinite(s$beta)) "Inf" else s$beta,
    seed = s$seed)
  js <- jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", pretty = pretty, digits = NA)
  if (!is.null(path)) { writeLines(js, path, useBytes = TRUE); return(invisible(as.character(js))) }
  as.character(js)
}

## --------------------------------------------------------------------------- #
## To the engine                                                                  #
## --------------------------------------------------------------------------- #

.rosetta_engine_args <- function(s, beta_large = 25) {
  b <- if (is.null(s$beta)) 1 else if (is.infinite(s$beta)) beta_large else s$beta
  args <- list(density = 0)
  extra <- list(); dropped <- character(0); has_scope <- FALSE
  for (e in s$effects) {
    p <- b * e$parameter
    switch(e$effect,
      density = { args$density <- p },
      inPop = { args$popularity <- p },
      outAct = { args$scope <- p },
      XWX = {
        if (is.null(s$influence_matrix)) dropped <- c(dropped, "XWX (no influence matrix)")
        else { args$influence_matrix <- s$influence_matrix; args$influence_weight <- p }
      },
      cycle4 = , outActSqrt = , inPopSqrt = {
        extra[[e$effect]] <- list(effect = e$effect, parameter = p)
      },
      dropped <- c(dropped, e$effect))
  }
  list(args = args, extra = extra, dropped = dropped, beta = b)
}

#' Convert a model specification to a searchnet structure model
#'
#' @param x A `rosetta_spec`, entry id, or JSON specification.
#' @param beta_large Finite scale used for an infinite `beta` (default 25).
#' @return A `saomnk_model`. Effects the engine cannot simulate (the
#'   imitation statistic, covariates without data) are left out with a
#'   warning.
#' @seealso [rosetta_run()], [saomnk_model()]
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE)) {
#'   mod <- rosetta_saomnk_model("plain-saom-two-mode")
#'   names(mod)
#' }
#' @export
rosetta_saomnk_model <- function(x, beta_large = 25) {
  s <- .rosetta_as_spec(x)
  a <- .rosetta_engine_args(s, beta_large)
  if (length(a$dropped))
    warning("not simulable by saomnk_run(), left out: ", paste(a$dropped, collapse = ", "),
            call. = FALSE)
  do.call(saomnk_model, c(a$args, a$extra))
}

#' Simulate a model specification
#'
#' Runs a specification on searchnet's engine. With `M >= 2` it builds
#' [saomnk_env()] and [saomnk_model()] and calls [saomnk_run()]. With
#' `M = 1` (the engine's RSiena path needs two actors) it runs the classical
#' adaptive walk of [nk_walk()] on [nk_landscape()] with the same `N`, `K`
#' and seed, which is the reduction the specification states.
#'
#' @param x A `rosetta_spec`, entry id, or JSON specification.
#' @param steps_per_actor Expected decision opportunities per actor.
#' @param density Initial bipartite density.
#' @param seed Seed (default: the specification's, else 1).
#' @param beta_large Finite scale used for an infinite `beta`.
#' @return A list of class `rosetta_run` with `engine` (`"saomnk"` or
#'   `"nk_walk"`), `B` (final actor-by-component matrix), `K_AC`, `K_CA`,
#'   `K_AA`, `K_CC` (the mean of each \{K\} degree in the final state: actor
#'   scope, component popularity, actor co-membership degree, component
#'   co-occurrence degree), `spec`, and `env` or `walk`.
#' @seealso [rosetta_model()]
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("yaml", quietly = TRUE)) {
#'   r <- rosetta_run(rosetta_model("plain-saom-two-mode", M = 3, N = 5),
#'                    steps_per_actor = 3)
#'   r$B
#' }
#' }
rosetta_run <- function(x, steps_per_actor = 5, density = 0.3, seed = NULL,
                        beta_large = 25) {
  s <- .rosetta_as_spec(x)
  seed <- as.integer(.rs_or(seed, .rs_or(s$seed, 1L)))
  if (is.na(s$M) || is.na(s$N)) stop("the specification needs M and N to run", call. = FALSE)
  if (s$M == 1L) {
    K <- .rs_or(s$K, if (!is.null(s$influence_matrix))
      .rosetta_W_K(s$influence_matrix) else 0L)
    nk <- nk_landscape(s$N, K, model = "random", seed = seed)
    w <- nk_walk(nk, type = "steepest")
    B <- matrix(nk_bits(w$terminal, s$N), nrow = 1)
    return(structure(c(list(engine = "nk_walk", B = B), .rosetta_k_summary(B),
                       list(spec = s, walk = w, landscape = nk)),
                     class = c("rosetta_run", "list")))
  }
  env <- saomnk_env(M = s$M, N = s$N, density = density, seed = seed)
  mod <- rosetta_saomnk_model(s, beta_large)
  utils::capture.output(saomnk_run(env, mod, steps_per_actor = steps_per_actor, seed = seed))
  B <- as.matrix(env$bipartite_matrix)
  structure(c(list(engine = "saomnk", B = B), .rosetta_k_summary(B), list(spec = s, env = env)),
            class = c("rosetta_run", "list"))
}

## Mean {K} degrees of a bipartite state (the definitions of saomnk_get_degrees():
## row and column sums, and degrees in the two one-mode projections).
.rosetta_k_summary <- function(B) {
  B <- (as.matrix(B) != 0) * 1
  S <- tcrossprod(B); diag(S) <- 0
  P <- crossprod(B); diag(P) <- 0
  list(K_AC = mean(rowSums(B)), K_CA = mean(colSums(B)),
       K_AA = mean(rowSums(S > 0)), K_CC = mean(rowSums(P > 0)))
}

#' Equivalent searchnet code for a model specification
#'
#' Writes the R code that builds and runs the specification with the
#' ordinary searchnet API, so a model designed through a front end can be
#' reproduced without it.
#'
#' @param x A `rosetta_spec`, entry id, `saomnk_model`, environment, or JSON
#'   specification.
#' @param steps_per_actor Value written into the `saomnk_run()` call.
#' @return A character string of R code.
#' @export
#' @examples
#' if (requireNamespace("yaml", quietly = TRUE))
#'   cat(rosetta_r_code("plain-saom-two-mode"))
rosetta_r_code <- function(x, steps_per_actor = 5) {
  s <- .rosetta_as_spec(x)
  seed <- .rs_or(s$seed, 1L)
  num <- function(v) format(signif(v, 6), scientific = FALSE, trim = TRUE)
  L <- c("library(searchnet)", "")
  if (!is.na(s$M) && s$M == 1L) {
    K <- .rs_or(s$K, 0L)
    L <- c(L, "# One actor and greedy choice: the classical NK adaptive walk.",
           sprintf("nk <- nk_landscape(N = %d, K = %d, model = \"random\", seed = %d)", s$N, K, seed),
           "walk <- nk_walk(nk, type = \"steepest\")",
           sprintf("nk_bits(walk$terminal, %d)", s$N))
    return(paste(L, collapse = "\n"))
  }
  a <- .rosetta_engine_args(s)
  if (!is.null(a$args$influence_matrix)) {
    W <- a$args$influence_matrix
    L <- c(L, sprintf("W <- matrix(c(%s),\n            nrow = %d, byrow = TRUE)",
                      paste(apply(W, 1, function(r) paste(num(r), collapse = ", ")),
                            collapse = ",\n              "), nrow(W)))
  }
  M <- if (is.na(s$M)) 3L else s$M
  N <- if (is.na(s$N)) (if (!is.null(s$influence_matrix)) nrow(s$influence_matrix) else 6L) else s$N
  L <- c(L, sprintf("env <- saomnk_env(M = %d, N = %d, density = 0.3, seed = %d)", M, N, seed))
  parts <- sprintf("density = %s", num(a$args$density))
  if (!is.null(a$args$popularity)) parts <- c(parts, sprintf("popularity = %s", num(a$args$popularity)))
  if (!is.null(a$args$scope)) parts <- c(parts, sprintf("scope = %s", num(a$args$scope)))
  if (!is.null(a$args$influence_matrix))
    parts <- c(parts, "influence_matrix = W", sprintf("influence_weight = %s", num(a$args$influence_weight)))
  for (nm in names(a$extra))
    parts <- c(parts, sprintf("%s = list(effect = \"%s\", parameter = %s)", nm, nm,
                              num(a$extra[[nm]]$parameter)))
  L <- c(L, paste0("mod <- saomnk_model(", paste(parts, collapse = ",\n                    "), ")"))
  if (length(a$dropped))
    L <- c(L, paste0("# Not simulable by saomnk_run(), left out: ", paste(a$dropped, collapse = ", ")))
  if (!is.null(s$beta) && is.infinite(s$beta))
    L <- c(L, "# beta = Inf is approximated by scaling every parameter by 25.")
  if (identical(s$revision_rule, "single_flip"))
    L <- c(L, "# The engine's ministep is RSiena's multinomial choice; single-flip results hold for its own protocol.")
  L <- c(L, sprintf("saomnk_run(env, mod, steps_per_actor = %d, seed = %d)", as.integer(steps_per_actor), seed),
         "saomnk_get_bipartite(env)")
  paste(L, collapse = "\n")
}
