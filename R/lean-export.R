###############################################################################
## lean-export.R
##
## From a SAOM-NK model definition to a Lean file that instantiates the general
## theorems of the SaomNK library for that model and proves numeric facts about
## it by kernel evaluation.
##
##   lean_spec()          model (R6 environment, nk_landscape, or list) ->
##                        normalized specification, with an explicit map from
##                        RSiena effects to Lean statistics
##   lean_export_model()  specification -> Instance_<hash>.lean
##   lean_check()         (lean-tools.R) build and axiom audit
###############################################################################

#' How RSiena two-mode effects map to the Lean library
#'
#' One row per evaluation effect the exporter understands. `slot` is where the
#' effect lands in the Lean normal form `Spec` (an actor-indexed own-row term,
#' activity-weighted crowding, or a symmetric pair kernel); `potential` says
#' whether the effect keeps the exact potential (`"exact"`) and so the Gibbs
#' stationary law, or breaks it (`"none"`), with the Lean declaration that
#' proves the claim.
#'
#' @return A data.frame with columns `effect`, `slot`, `lean`, `potential`,
#'   `proof`, `note`.
#' @examples
#' lean_effect_map()[, c("effect", "slot", "potential")]
#' @export
lean_effect_map <- function() {
  data.frame(
    effect = c("density", "outAct", "inPop", "cycle4", "XWX", "egoX", "altX", "nk",
               "outActSqrt", "RateX", "outRate", "creation/endowment"),
    slot = c("own", "own", "congestion", "pair", "own", "own (actor-indexed)", "own", "own",
             "own (not exported)", "rate", "rate", "directional"),
    lean = c("density", "outAct", "inPop", "cycle4", "xwx", "egoStat", "altStat",
             "nkFitness (table)", "-", "-", "-", "DirSpec"),
    potential = c("exact", "exact", "exact", "exact", "exact", "exact", "exact", "exact",
                  "exact", "exact", "not covered", "only if creation = endowment"),
    proof = c("isExactPotential_own", "isExactPotential_own", "isExactPotential_inPop",
              "isExactPotential_cycle4", "isExactPotential_own", "isExactPotential_own",
              "isExactPotential_own", "isExactPotential_own", "isExactPotential_own",
              "detailed_balance_of_potential", "-", "dir_integrability_necessary"),
    note = c(
      "sum of the row",
      "squared outdegree; the engine routes a bipartite 'density' to RSiena's outAct",
      "Rosenthal potential sum_d n_d (n_d + 1) / 2",
      "potential = number of four-cycles; a root ('#' parameter) form is not exported",
      "b' W b with the coDyadCovar W, scaled by its coefficient",
      "two-mode layer: actor-specific own term, potential exact. On a one-mode relational layer an ego covariate admits NO common potential (TwoLayer.egoCovariate_not_ownAPotential)",
      "activity covariate on held activities",
      "mean of the enumerated contribution table",
      "own-row, so the potential exists, but irrational; omitted from numeric export with a warning",
      "state-independent actor rates keep Gibbs stationarity",
      "state-dependent rates are outside the proved protocol",
      "a creation/endowment split keeps a potential only when the two change statistics agree"),
    stringsAsFactors = FALSE)
}

.lean_known_eval <- c("density", "outAct", "inPop", "cycle4", "XWX", "egoX", "altX")

## Resolve a covariate given by name ("strat_1_coCovar", "self$strat_1_coCovar")
## against an R6 environment, or return the entry's own `x`.
.lean_covariate <- function(model, eff) {
  if (!is.null(eff$x)) return(eff$x)
  nm <- eff$interaction1
  if (is.null(nm) || !nzchar(nm)) return(NULL)
  nm <- sub("^self\\$", "", as.character(nm))
  val <- tryCatch(model[[nm]], error = function(e) NULL)
  if (is.null(val)) NULL else val
}

## LSB-first row order: row r (1-based) is the configuration with
## sum_j b_j 2^(j-1) = r - 1. Reorder a (2^N x N) table given its configs.
.lean_reorder_lsb <- function(tab, configs) {
  N <- ncol(configs)
  code <- as.vector(configs %*% 2^(0:(N - 1)))
  out <- matrix(NA_real_, 2^N, ncol(tab))
  out[code + 1L, ] <- tab
  out
}

#' Normalize a SAOM-NK model for formal export
#'
#' Extracts what the Lean exporter needs from a model definition and maps each
#' evaluation effect onto the statistics of the Lean library (see
#' [lean_effect_map()]). Effects with no Lean counterpart are listed, with a
#' warning, and are left out of the exported specification: theorems about the
#' export are then theorems about the model without them.
#'
#' `model` may be:
#' \itemize{
#'   \item a `SaomNkRSienaBiEnv` object: actors, components, the influence
#'     pattern (`component_1_coDyadCovar`, else `search_matrix`, else the
#'     identity, binarized), the enumerated landscape (`fitness_landscape`, if
#'     fully enumerated), the current bipartite matrix, and the evaluation
#'     effects of the bipartite dependent variable (from the RSiena effects
#'     table when present, else from the structure model);
#'   \item an `nk_landscape` object: a single actor on that landscape with NK
#'     weight 1 (the classical NK model);
#'   \item a plain list with elements `M`, `N`, `E` (N x N 0/1), optionally
#'     `landscape` (a `2^N x N` contribution table in LSB-first row order, or an
#'     `nk_landscape`), `theta` (named: any of `nk`, `density`, `outAct`,
#'     `inPop`, `cycle4`, `XWX`, `egoX`, `altX`), `W`, `ego` (actor covariate),
#'     `alt` (activity covariate), `config` (M x N 0/1), `relational`.
#' }
#'
#' Covariates are centered by default, as RSiena centers them; centering adds a
#' multiple of the density to the own-row term and changes no potential claim.
#'
#' `relational = list(ego = z, theta_AB = a, theta_BA = b)` declares a
#' symmetric one-mode relational layer coupled to the two-mode layer. The
#' exporter then also states the two-layer no-potential results that apply: an
#' ego covariate on the relational degree with non-constant `z` admits no
#' common potential, and `theta_AB != theta_BA` admits no joint potential.
#'
#' @param model A model definition (see Details).
#' @param theta Optional named numeric overriding or adding coefficients.
#' @param landscape_id Which landscape replicate of an R6 model to use.
#' @param config Optional M x N 0/1 matrix, the configuration about which
#'   numeric facts are stated. Defaults to the model's current matrix.
#' @param revision Ministep acceptance rule: `"glauber"` (binary logit),
#'   `"metropolis"`, or `"multinomial"` (RSiena's default choice among all
#'   flips and no change). Gibbs stationarity is proved for the first two.
#' @param center_covariates Logical; center `ego`/`alt` covariates.
#' @param relational Optional relational-layer declaration (see Details).
#' @return An object of class `searchnet_lean_spec`.
#' @seealso [lean_export_model()], [lean_effect_map()]
#' @examples
#' mod <- list(M = 2, N = 3, E = diag(3),
#'             theta = c(density = -0.5, inPop = 0.2))
#' lean_spec(mod)
#' @export
lean_spec <- function(model, theta = NULL, landscape_id = 1, config = NULL,
                      revision = c("glauber", "metropolis", "multinomial"),
                      center_covariates = TRUE, relational = NULL) {
  revision <- match.arg(revision)
  notes <- character(0); unmapped <- character(0)
  th <- c(nk = 0, density = 0, outAct = 0, inPop = 0, cycle4 = 0)
  W <- NULL; ego <- NULL; alt <- NULL; contrib <- NULL; E <- NULL

  if (inherits(model, "nk_landscape")) {
    M <- 1L; N <- as.integer(model$N)
    E <- model$influence_matrix
    contrib <- .lean_reorder_lsb(model$contributions, model$configs)
    th["nk"] <- 1
    if (is.null(config)) config <- matrix(0L, 1, N)
  } else if (inherits(model, "R6") && !is.null(model$M) && !is.null(model$N)) {
    M <- as.integer(model$M); N <- as.integer(model$N)
    E <- if (!is.null(model$component_1_coDyadCovar) &&
             all(dim(as.matrix(model$component_1_coDyadCovar)) == N)) {
      as.matrix(model$component_1_coDyadCovar)
    } else if (!is.null(model$search_matrix) && all(dim(as.matrix(model$search_matrix)) == N)) {
      as.matrix(model$search_matrix)
    } else diag(N)
    if (!is.null(model$fitness_landscape)) {
      fl <- model$fitness_landscape
      if (dim(fl)[2] == 2^N) {
        contrib <- .lean_reorder_lsb(fl[landscape_id, , (N + 1):(2 * N)],
                                     fl[landscape_id, , seq_len(N)])
      } else notes <- c(notes, "sampled landscape: NK term not exported")
    }
    if (is.null(config) && !is.null(model$bipartite_matrix)) config <- model$bipartite_matrix
    effs <- .lean_effects_from_r6(model)
    for (e in effs) {
      nm <- e$effect; val <- as.numeric(e$parameter %||% 0)
      if (identical(nm, "density")) {
        nm <- "outAct"
        notes <- c(notes, "'density' on the bipartite DV is RSiena's outAct (as the engine routes it)")
      }
      if (!is.null(e$internal_parameter) && nm %in% c("cycle4")) {
        unmapped <- c(unmapped, paste0(nm, " (internal parameter ", e$internal_parameter, ")"))
        next
      }
      if (nm %in% c("outAct", "inPop", "cycle4")) th[nm] <- th[nm] + val
      else if (nm == "XWX") {
        Wm <- .lean_covariate(model, e)
        if (is.null(Wm)) { unmapped <- c(unmapped, "XWX (covariate not found)"); next }
        W <- (if (is.null(W)) 0 else W) + val * as.matrix(Wm)
      } else if (nm == "egoX") {
        z <- .lean_covariate(model, e)
        if (is.null(z)) { unmapped <- c(unmapped, "egoX (covariate not found)"); next }
        z <- as.numeric(z); if (center_covariates) z <- z - mean(z)
        ego <- (if (is.null(ego)) 0 else ego) + val * z
      } else if (nm == "altX") {
        v <- .lean_covariate(model, e)
        if (is.null(v)) { unmapped <- c(unmapped, "altX (covariate not found)"); next }
        v <- as.numeric(v); if (center_covariates) v <- v - mean(v)
        alt <- (if (is.null(alt)) 0 else alt) + val * v
      } else unmapped <- c(unmapped, nm)
    }
  } else if (is.list(model)) {
    M <- as.integer(model$M); N <- as.integer(model$N)
    E <- model$E %||% diag(N)
    ls <- model$landscape
    if (inherits(ls, "nk_landscape")) {
      contrib <- .lean_reorder_lsb(ls$contributions, ls$configs)
      if (is.null(model$E)) E <- ls$influence_matrix
    } else if (!is.null(ls)) contrib <- as.matrix(ls)
    if (is.null(config)) config <- model$config
    W <- model$W; ego <- model$ego; alt <- model$alt
    if (center_covariates) {
      if (!is.null(ego)) ego <- ego - mean(ego)
      if (!is.null(alt)) alt <- alt - mean(alt)
    }
    mt <- model$theta
    if (!is.null(mt)) {
      mt <- unlist(mt)
      for (nm in names(mt)) {
        if (nm %in% names(th)) th[nm] <- mt[[nm]]
        else if (nm == "XWX" && !is.null(W)) W <- mt[[nm]] * W
        else if (nm == "egoX" && !is.null(ego)) ego <- mt[[nm]] * ego
        else if (nm == "altX" && !is.null(alt)) alt <- mt[[nm]] * alt
        else unmapped <- c(unmapped, nm)
      }
    }
    if (is.null(relational)) relational <- model$relational
  } else stop("lean_spec(): unsupported model of class ", class(model)[1], call. = FALSE)

  if (!is.null(theta)) for (nm in names(theta)) {
    if (nm %in% names(th)) th[nm] <- theta[[nm]] else unmapped <- c(unmapped, nm)
  }
  if (th["nk"] != 0 && is.null(contrib)) {
    notes <- c(notes, "NK weight given but no enumerated landscape: NK term dropped")
    th["nk"] <- 0
  }
  E <- (as.matrix(E) != 0) * 1L
  rs <- rowSums(E)
  K <- if (all(diag(E) == 1) && length(unique(rs)) == 1L) as.integer(rs[1] - 1L) else NA_integer_
  if (is.na(K)) notes <- c(notes, "E is not K-regular (or lacks its diagonal): no regularity fact")
  if (is.null(W)) W <- matrix(0, N, N)
  if (is.null(ego)) ego <- rep(0, M)
  if (is.null(alt)) alt <- rep(0, N)
  if (!is.null(config)) config <- (as.matrix(config) != 0) * 1L
  if (length(unmapped)) warning("lean_spec(): effects with no Lean statistic, left out of the ",
                                "export: ", paste(unique(unmapped), collapse = ", "), call. = FALSE)
  structure(list(M = M, N = N, K = K, E = E, contrib = contrib, theta = th,
                 W = as.matrix(W), ego = as.numeric(ego), alt = as.numeric(alt),
                 config = config, revision = revision, relational = relational,
                 unmapped = unique(unmapped), notes = unique(notes)),
            class = "searchnet_lean_spec")
}

## Evaluation effects of the bipartite DV of an R6 model, as a list of
## list(effect, parameter, interaction1, internal_parameter, x).
.lean_effects_from_r6 <- function(model) {
  sm <- model$config_structure_model
  dv <- if (!is.null(sm$dv_bipartite)) sm$dv_bipartite else NULL
  if (is.null(dv)) return(list())
  c(dv$effects %||% list(), dv$coCovars %||% list(), dv$coDyadCovars %||% list())
}

#' @export
print.searchnet_lean_spec <- function(x, ...) {
  cat(sprintf("<searchnet_lean_spec> M = %d, N = %d, K = %s, revision = %s\n",
              x$M, x$N, if (is.na(x$K)) "NA" else x$K, x$revision))
  nz <- x$theta[x$theta != 0]
  cat("  coefficients: ", if (length(nz)) paste(names(nz), signif(nz, 4), sep = " = ",
                                                 collapse = ", ") else "none", "\n", sep = "")
  if (any(x$W != 0)) cat("  XWX covariate: yes\n")
  if (any(x$ego != 0)) cat("  ego covariate: yes\n")
  if (any(x$alt != 0)) cat("  activity covariate: yes\n")
  if (length(x$unmapped)) cat("  UNMAPPED (left out): ", paste(x$unmapped, collapse = ", "), "\n", sep = "")
  for (n in x$notes) cat("  note: ", n, "\n", sep = "")
  invisible(x)
}

## ---------------------------------------------------------------------------
## Exact rationals
## ---------------------------------------------------------------------------

## A double as an exact binary fraction num / 2^k (num an integer string), or
## rounded to `digits` decimal digits.
.lean_frac <- function(x, digits = NULL) {
  if (!is.finite(x)) stop("non-finite value cannot be exported", call. = FALSE)
  if (!is.null(digits)) {
    num <- sprintf("%.0f", round(x * 10^digits))
    return(list(num = num, base = 10L, k = as.integer(digits)))
  }
  if (x == 0) return(list(num = "0", base = 2L, k = 0L))
  k <- 0L; y <- x
  while (y != round(y)) { y <- y * 2; k <- k + 1L }
  ## strip common factors of two
  while (k > 0L && (y / 2) == round(y / 2)) { y <- y / 2; k <- k - 1L }
  list(num = sprintf("%.0f", y), base = 2L, k = k)
}

.lean_frac_lit <- function(f) {
  if (f$k == 0L) return(if (startsWith(f$num, "-")) paste0("(", f$num, ")") else f$num)
  sprintf("(%s / %d^%d)", f$num, f$base, f$k)
}

.lean_frac_value <- function(f, exact) {
  if (exact) gmp::as.bigq(gmp::as.bigz(f$num), gmp::pow.bigz(f$base, f$k))
  else as.numeric(f$num) / f$base^f$k
}

## ---------------------------------------------------------------------------
## The R mirror of RatSpec.utilityQ / potentialQ (double, or exact with gmp)
## ---------------------------------------------------------------------------

.lean_num_spec <- function(spec, digits, exact) {
  cv <- function(x) {
    fs <- lapply(as.vector(x), .lean_frac, digits = digits)
    vals <- lapply(fs, .lean_frac_value, exact = exact)
    if (exact) do.call(c, vals) else unlist(vals)
  }
  list(theta = lapply(spec$theta, function(v) cv(v)),
       contrib = if (!is.null(spec$contrib)) cv(t(spec$contrib)) else NULL,  # row-major
       W = cv(t(spec$W)), ego = cv(spec$ego), alt = cv(spec$alt),
       M = spec$M, N = spec$N, has_nk = !is.null(spec$contrib))
}

.lean_own <- function(ns, i, b) {
  N <- ns$N; dens <- sum(b)
  zero <- ns$theta$density * 0
  nk <- zero
  if (ns$has_nk) {
    code <- sum(b * 2^(0:(N - 1)))
    nk <- ns$theta$nk * sum(ns$contrib[code * N + seq_len(N)]) / N
  }
  xw <- zero
  for (j in which(b == 1L)) for (h in which(b == 1L)) xw <- xw + ns$W[(j - 1L) * N + h]
  al <- zero
  for (j in which(b == 1L)) al <- al + ns$alt[j]
  nk + ns$theta$density * dens + ns$theta$outAct * dens^2 + xw + ns$ego[i] * dens + al
}

.lean_utility <- function(ns, B, i) {
  b <- B[i, ]; n <- colSums(B)
  cyc <- 0
  for (k in setdiff(seq_len(ns$M), i)) { o <- sum(b * B[k, ]); cyc <- cyc + o * (o - 1) / 2 }
  .lean_own(ns, i, b) + ns$theta$inPop * sum(b * n) + ns$theta$cycle4 * cyc
}

.lean_potential <- function(ns, B) {
  M <- ns$M; n <- colSums(B)
  own <- ns$theta$density * 0
  for (m in seq_len(M)) own <- own + .lean_own(ns, m, B[m, ])
  cyc <- 0
  for (p in seq_len(M)) for (k in setdiff(seq_len(M), p)) {
    o <- sum(B[p, ] * B[k, ]); cyc <- cyc + o * (o - 1) / 2
  }
  own + ns$theta$inPop * sum(n * (n + 1) / 2) + ns$theta$cycle4 * (cyc / 2)
}

.lean_cfg <- function(code, M, N) {
  matrix(as.integer(intToBits(code))[seq_len(M * N)], nrow = M, ncol = N, byrow = TRUE)
}

## Count single-flip-stable configurations via local maxima of the potential
## (equivalent to Lean's countLocalOpt by RatSpec.isLocalOptB_iff_localMax).
.lean_count_local_opt <- function(ns, exact) {
  M <- ns$M; N <- ns$N; nc <- 2^(M * N)
  P <- vector("list", nc)
  for (c in 0:(nc - 1)) P[[c + 1]] <- .lean_potential(ns, .lean_cfg(c, M, N))
  near_tie <- FALSE; count <- 0L
  for (c in 0:(nc - 1)) {
    ok <- TRUE
    for (t in 0:(M * N - 1)) {
      d <- P[[bitwXor(c, bitwShiftL(1L, t)) + 1]] - P[[c + 1]]
      if (!exact && d != 0 && abs(as.numeric(d)) < 1e-9) near_tie <- TRUE
      if (d > 0) { ok <- FALSE; break }
    }
    if (ok) count <- count + 1L
  }
  list(count = count, near_tie = near_tie)
}

.lean_q_lit <- function(q) {
  num <- as.character(gmp::numerator(q)); den <- as.character(gmp::denominator(q))
  if (den == "1") return(if (startsWith(num, "-")) paste0("(", num, ")") else num)
  sprintf("(%s / %s)", num, den)
}

.lean_bracket <- function(x) {
  s <- 1e9
  lo <- floor(x * s) - 1; hi <- ceiling(x * s) + 1
  c(sprintf("(%.0f / 1000000000)", lo), sprintf("(%.0f / 1000000000)", hi))
}

## ---------------------------------------------------------------------------
## The exporter
## ---------------------------------------------------------------------------

#' Export a SAOM-NK model as a Lean instance file
#'
#' Writes `Instance_<hash>.lean`, a Lean file that defines the model as a
#' `RatSpec` (exact rationals), instantiates the general theorems of the
#' `SaomNK` library for it, and proves numeric facts about it by kernel
#' evaluation. Check it with [lean_check()].
#'
#' What the file proves, when the model allows:
#' \itemize{
#'   \item `E_regular`: the influence matrix is K-regular (`decide`);
#'   \item `C_consistent`: the enumerated landscape respects the influence
#'     matrix, hence (`nk_is_fitness`) the exported NK term is NK fitness;
#'   \item `index_convention`: the LSB code of each actor's row equals the row
#'     index R uses for the landscape table, minus one;
#'   \item `exact_potential`, `exists_nash`, `gibbs_stationary` (for the
#'     binary-logit or Metropolis protocol), `greedy_limit` (zero-noise limit);
#'   \item `utility_B0_i`, `potential_B0`: utilities and potential at the
#'     given configuration, exact when the 'gmp' package is installed, else
#'     bracketed to 1e-9;
#'   \item `count_local_opt`: the number of single-flip-stable configurations
#'     (for `M * N <= max_cells`), and `count_local_opt_card`, what it counts;
#'   \item with a declared relational layer, the two-layer no-potential
#'     statements that apply.
#' }
#'
#' Numbers: with `exact = TRUE` every double is written as the exact binary
#' fraction it is; otherwise rounded to `digits` decimal digits. The choice is
#' recorded in the file header. The file name is a hash of its content, so the
#' same model exports to the same file.
#'
#' Size: the landscape table has `2^N` rows and the local-optima count
#' enumerates `2^(M N)` configurations. Above `max_N` the export stops, and
#' above `max_cells` the count is skipped, unless `native = TRUE`, which proves
#' the large facts with `native_decide`. That trusts the Lean compiler
#' (axiom `Lean.ofReduceBool`), and [lean_check()] reports it as `"native"`.
#'
#' @param model A model definition or a `searchnet_lean_spec` (see
#'   [lean_spec()]).
#' @param dir Output directory (default `tempdir()`).
#' @param exact Logical; exact binary fractions (default) or decimal rounding.
#' @param digits Decimal digits when `exact = FALSE`.
#' @param native Logical; allow `native_decide` for large instances (flagged).
#' @param max_N Largest N exported without `native` (default 6).
#' @param max_cells Largest `M * N` for the local-optima count (default 8).
#' @param ... Passed to [lean_spec()].
#' @return Invisibly, the path of the written file, with attributes `spec`,
#'   `hash` and `facts` (a data.frame of the numeric facts stated).
#' @examples
#' mod <- list(M = 2, N = 3, E = diag(3),
#'             theta = c(density = -0.5, inPop = 0.2))
#' f <- lean_export_model(mod, dir = tempdir())
#' basename(f)
#' attr(f, "facts")
#' @export
lean_export_model <- function(model, dir = tempdir(), exact = TRUE, digits = 6,
                              native = FALSE, max_N = 6, max_cells = 8, ...) {
  spec <- if (inherits(model, "searchnet_lean_spec")) model else lean_spec(model, ...)
  M <- spec$M; N <- spec$N
  if (N > max_N && !native)
    stop(sprintf("N = %d exceeds max_N = %d (the table has 2^N rows). Use native = TRUE to ",
                 N, max_N), "prove large facts with native_decide (flagged), or raise max_N.",
         call. = FALSE)
  dig <- if (exact) NULL else digits
  have_gmp <- requireNamespace("gmp", quietly = TRUE)
  ns <- .lean_num_spec(spec, dig, exact = have_gmp)
  lit <- function(x) .lean_frac_lit(.lean_frac(x, dig))
  bool <- function(v) paste0("![", paste(ifelse(v != 0, "true", "false"), collapse = ", "), "]")
  mat_bool <- function(m) paste0("![", paste(apply(m, 1, bool), collapse = ",\n    "), "]")
  vec_q <- function(v) paste0("![", paste(vapply(v, lit, ""), collapse = ", "), "]")
  mat_q <- function(m) paste0("![", paste(apply(m, 1, vec_q), collapse = ",\n    "), "]")
  dec_big <- if (native) "native_decide" else "decide +kernel"

  L <- character(0); add <- function(...) L <<- c(L, ...)
  facts <- data.frame(decl = character(0), value = character(0), stringsAsFactors = FALSE)
  decls <- character(0); thm <- function(name) decls <<- c(decls, name)

  add("import SaomNK", "", "namespace @NS@", "", "open SaomNK Filter Topology", "")
  add(sprintf("def E : Influence %d := %s", N, mat_bool(spec$E)), "")
  if (!is.null(spec$contrib)) {
    add(sprintf("def C : Fin (2 ^ %d) \u2192 Fin %d \u2192 \u211A := %s", N, N, mat_q(spec$contrib)), "")
  } else add(sprintf("def C : Fin (2 ^ %d) \u2192 Fin %d \u2192 \u211A := fun _ _ => 0", N, N), "")
  add(sprintf("def W : Fin %d \u2192 Fin %d \u2192 \u211A := %s", N, N, mat_q(spec$W)), "")
  add(sprintf("/-- The model. -/\ndef S : RatSpec %d %d where", M, N),
      "  E := E", "  C := C",
      sprintf("  \u03B8nk := %s", lit(spec$theta[["nk"]])),
      sprintf("  \u03B8density := %s", lit(spec$theta[["density"]])),
      sprintf("  \u03B8outAct := %s", lit(spec$theta[["outAct"]])),
      sprintf("  \u03B8inPop := %s", lit(spec$theta[["inPop"]])),
      sprintf("  \u03B8cycle4 := %s", lit(spec$theta[["cycle4"]])),
      "  W := W",
      sprintf("  ego := %s", vec_q(spec$ego)),
      sprintf("  alt := %s", vec_q(spec$alt)), "")

  if (!is.na(spec$K)) {
    add(sprintf("/-- The influence matrix is K-regular with K = %d. -/", spec$K),
        sprintf("theorem E_regular : IsKRegular E %d := by decide +kernel", spec$K), "")
    thm("E_regular")
  }
  if (!is.null(spec$contrib)) {
    consistent <- .lean_table_consistent(spec$E, spec$contrib)
    if (consistent) {
      add("/-- The enumerated landscape respects the influence matrix. -/",
          sprintf("theorem C_consistent : S.TableConsistentQ := by %s", dec_big), "",
          "/-- Hence the exported NK term is NK fitness of the general theory. -/",
          sprintf("theorem nk_is_fitness (b : Fin %d \u2192 Bool) :", N),
          "    ((nkQ S.C b : \u211A) : \u211D) = nkFitness S.E (tablePayoff S.CReal) b :=",
          "  S.nkQ_eq_nkFitness C_consistent b", "")
      thm(c("C_consistent", "nk_is_fitness"))
    } else {
      spec$notes <- c(spec$notes, "landscape table not consistent with E: no NK-fitness link")
      add("-- The landscape table is NOT consistent with E (checked in R); the NK term is a",
          "-- table lookup without the link to NK fitness.", "")
    }
  }
  add(sprintf("/-- Exact potential (any unilateral deviation). -/"),
      sprintf("theorem exact_potential {B B' : Config %d %d} {i : Fin %d} (h : Unilateral B B' i) :", M, N, M),
      "    S.utilityQ i B' - S.utilityQ i B = S.potentialQ B' - S.potentialQ B :=",
      "  S.exact_potentialQ h", "",
      "/-- A pure-strategy Nash equilibrium exists. -/",
      "theorem exists_nash : \u2203 B, IsNash S.toSpec.utility B := S.exists_nash", "")
  thm(c("exact_potential", "exists_nash"))
  if (spec$revision %in% c("glauber", "metropolis")) {
    acc <- spec$revision
    add(sprintf("/-- Gibbs stationarity of the single-flip chain with %s acceptance and any", acc),
        "state-independent proposal rates `c`. -/",
        sprintf("theorem gibbs_stationary (c : Fin %d \u2192 Fin %d \u2192 \u211D) (\u03B2 : \u211D) (B' : Config %d %d) :", M, N, M, N),
        sprintf("    (\u2211 i, \u2211 j, gibbsW \u03B2 S.toSpec.potential (flip B' i j) * flipRate %s c \u03B2 S.toSpec.utility (flip B' i j) i j)", acc),
        sprintf("      = gibbsW \u03B2 S.toSpec.potential B' * \u2211 i, \u2211 j, flipRate %s c \u03B2 S.toSpec.utility B' i j :=", acc),
        sprintf("  global_balance_of_potential %s_reversible S.isFlipPotential c \u03B2 B'", acc), "")
    thm("gibbs_stationary")
  } else {
    add("-- Multinomial (RSiena default) ministep: Gibbs stationarity is NOT claimed; the",
        "-- library proves it for binary-logit and Metropolis acceptance only.", "")
  }
  add("/-- Zero-noise limit: a unique improving flip is taken with probability \u2192 1. -/",
      sprintf("theorem greedy_limit (B : Config %d %d) (i : Fin %d) (j : Fin %d)", M, N, M, N),
      "    (hpos : 0 < S.toSpec.utility i (flip B i j) - S.toSpec.utility i B)",
      "    (hmax : \u2200 k, k \u2260 j \u2192 S.toSpec.utility i (flip B i k) - S.toSpec.utility i B",
      "      < S.toSpec.utility i (flip B i j) - S.toSpec.utility i B) :",
      "    Tendsto (fun \u03B2 => logitProb (fun k => S.toSpec.utility i (flip B i k) - S.toSpec.utility i B) \u03B2 j)",
      "      atTop (\U{1D4DD} 1) :=",
      "  logitProb_tendsto_one _ j hpos hmax", "")
  thm("greedy_limit")

  if (!is.null(spec$config)) {
    B0 <- spec$config
    add(sprintf("def B0 : Config %d %d := %s", M, N, mat_bool(B0)), "")
    codes <- as.vector(B0 %*% 2^(0:(N - 1)))
    add("/-- Index convention: R's landscape row for actor i's portfolio is 1 + codeLSB. -/",
        sprintf("theorem index_convention : %s := by decide +kernel",
                paste(sprintf("codeLSB (B0 %d) = %d", seq_len(M) - 1L, codes), collapse = " \u2227 ")), "")
    thm("index_convention")
    for (i in seq_len(M)) {
      u <- .lean_utility(ns, B0, i)
      nm <- sprintf("utility_B0_%d", i - 1L)
      if (have_gmp) {
        add(sprintf("theorem %s : S.utilityQ %d B0 = %s := by decide +kernel", nm, i - 1L, .lean_q_lit(u)))
        facts <- rbind(facts, data.frame(decl = nm, value = as.character(u)))
      } else {
        br <- .lean_bracket(as.numeric(u))
        add(sprintf("theorem %s : %s \u2264 S.utilityQ %d B0 \u2227 S.utilityQ %d B0 \u2264 %s := by decide +kernel",
                    nm, br[1], i - 1L, i - 1L, br[2]))
        facts <- rbind(facts, data.frame(decl = nm, value = format(as.numeric(u), digits = 15)))
      }
      thm(nm)
    }
    p <- .lean_potential(ns, B0)
    if (have_gmp) {
      add(sprintf("theorem potential_B0 : S.potentialQ B0 = %s := by decide +kernel", .lean_q_lit(p)))
      facts <- rbind(facts, data.frame(decl = "potential_B0", value = as.character(p)))
    } else {
      br <- .lean_bracket(as.numeric(p))
      add(sprintf("theorem potential_B0 : %s \u2264 S.potentialQ B0 \u2227 S.potentialQ B0 \u2264 %s := by decide +kernel",
                  br[1], br[2]))
      facts <- rbind(facts, data.frame(decl = "potential_B0", value = format(as.numeric(p), digits = 15)))
    }
    thm("potential_B0"); add("")
  }

  if (M * N <= max_cells || native) {
    cl <- .lean_count_local_opt(ns, exact = have_gmp)
    if (cl$near_tie) {
      add("-- Local-optima count skipped: near-ties in double precision (install 'gmp').", "")
    } else {
      tac <- if (M * N <= max_cells) "decide +kernel" else "native_decide"
      add(sprintf("/-- Number of single-flip-stable configurations (of 2^%d). -/", M * N),
          sprintf("theorem count_local_opt : S.countLocalOpt = %d := by %s", cl$count, tac), "",
          "/-- ... which is the number of flip-stable configurations of the general model. -/",
          sprintf("theorem count_local_opt_card :"),
          sprintf("    Nat.card {B : Config %d %d // IsFlipStable S.toSpec.utility B} = %d := by",
                  M, N, cl$count),
          "  rw [\u2190 S.countLocalOpt_eq_card]; exact count_local_opt", "")
      thm(c("count_local_opt", "count_local_opt_card"))
      facts <- rbind(facts, data.frame(decl = "count_local_opt", value = as.character(cl$count)))
    }
  } else {
    add(sprintf("-- Local-optima count skipped: M * N = %d exceeds max_cells = %d.", M * N, max_cells), "")
  }

  rel <- spec$relational
  if (!is.null(rel)) {
    z <- as.numeric(rel$ego %||% rep(0, M))
    if (length(unique(z)) > 1L && M >= 2L) {
      i0 <- 1L; k0 <- which(z != z[1])[1]
      add(sprintf("def z : Fin %d \u2192 \u211A := %s", M, vec_q(z)), "",
          "/-- On the relational layer an ego covariate on the degree admits no common potential,",
          "whatever the other terms. -/",
          sprintf("theorem ego_no_relational_potential (ownB : (Fin %d \u2192 Bool) \u2192 \u211D) (\u03B8 : \u211D)", N),
          sprintf("    (\u03A8A : TwoLayer.Rel %d \u2192 \u211D) (a b : \u211D) :", M),
          sprintf("    \u00AC (TwoLayer.TwoLayerSpec.mk (M := %d) ownB \u03B8", M),
          "        (fun i A => ((z i : \u211A) : \u211D) * TwoLayer.relDegree A i) \u03A8A a b).OwnAPotential :=",
          sprintf("  TwoLayer.egoCovariate_not_ownAPotential _ (fun i => ((z i : \u211A) : \u211D)) (fun _ _ => rfl)"),
          sprintf("    (i := %d) (k := %d) (by decide) (by norm_num [z])", i0 - 1L, k0 - 1L), "")
      thm("ego_no_relational_potential")
    }
    a <- rel$theta_AB; b <- rel$theta_BA
    if (!is.null(a) && !is.null(b) && a != b && M >= 2L && N >= 1L) {
      add("/-- Asymmetric cross-layer coupling admits no joint exact potential, whatever the",
          "own terms. -/",
          sprintf("theorem no_joint_potential (ownB : (Fin %d \u2192 Bool) \u2192 \u211D) (\u03B8 : \u211D)", N),
          sprintf("    (ownA : Fin %d \u2192 TwoLayer.Rel %d \u2192 \u211D) (\u03A8A : TwoLayer.Rel %d \u2192 \u211D) :", M, M, M),
          sprintf("    \u00AC \u2203 \u03A6, (TwoLayer.TwoLayerSpec.mk (M := %d) (N := %d) ownB \u03B8 ownA \u03A8A", M, N),
          sprintf("        ((%s : \u211A) : \u211D) ((%s : \u211A) : \u211D)).IsExactPotential \u03A6 := by", lit(a), lit(b)),
          "  rintro \u27E8\u03A6, h\u03A6\u27E9",
          "  have h := TwoLayer.integrability_necessary _ (by norm_num) (by norm_num) \u03A6 h\u03A6",
          "  norm_num at h", "")
      thm("no_joint_potential")
    }
  }

  ## header, hash, namespace, audit
  body <- paste(L, collapse = "\n")
  hash <- substr(.lean_md5(body), 1, 10)
  nsname <- paste0("SaomNKInstance.h", hash)
  body <- gsub("@NS@", nsname, body, fixed = TRUE)
  header <- c(
    "/-",
    "  Generated by searchnet::lean_export_model(). Do not edit; re-export instead.",
    sprintf("  Model: M = %d actors, N = %d components, K = %s; revision rule: %s.", M, N,
            if (is.na(spec$K)) "not regular" else spec$K, spec$revision),
    sprintf("  Numbers: %s.", if (exact) "exact binary fractions of the R doubles"
            else sprintf("rounded to %d decimal digits", digits)),
    sprintf("  Numeric facts: %s.", if (have_gmp) "exact (gmp)" else "bracketed to 1e-9"),
    if (native) "  WARNING: native = TRUE; facts proved by native_decide trust the compiler." else NULL,
    if (length(spec$unmapped)) sprintf("  Effects left out (no Lean statistic): %s.",
                                       paste(spec$unmapped, collapse = ", ")) else NULL,
    if (length(spec$notes)) paste0("  Note: ", spec$notes) else NULL,
    "  Check: searchnet::lean_check(dir), or `lake env lean <this file>` in the SaomNK project.",
    "-/")
  audit <- paste0("#print axioms ", nsname, ".", decls)
  txt <- c(header, body, sprintf("end %s", nsname), "", "-- Axiom audit", audit)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, sprintf("Instance_%s.lean", hash))
  con <- file(path, open = "wb"); on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(paste(txt, collapse = "\n"), "\n"))), con)
  invisible(structure(normalizePath(path, winslash = "/"), spec = spec, hash = hash,
                      facts = facts, decls = paste0(nsname, ".", decls)))
}

.lean_md5 <- function(text) {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  con <- file(f, open = "wb"); writeBin(charToRaw(enc2utf8(text)), con); close(con)
  unname(tools::md5sum(f))
}

## R-side check of the table consistency the Lean file asserts.
.lean_table_consistent <- function(E, contrib) {
  N <- ncol(E)
  for (c in 0:(2^N - 1)) {
    b <- as.integer(intToBits(c))[seq_len(N)]
    for (d in seq_len(N)) {
      cm <- sum((b * E[d, ]) * 2^(0:(N - 1)))
      if (!isTRUE(all.equal(contrib[cm + 1, d], contrib[c + 1, d], tolerance = 0)))
        return(FALSE)
    }
  }
  TRUE
}
