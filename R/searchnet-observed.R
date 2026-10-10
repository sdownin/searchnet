###############################################################################
## searchnet-observed.R
##
## The observed-data layer: from a user's own long-format records to the
## package's actor-by-component arrays and to {K} readings of them.
##
## searchnet_bipartite_from_long()  long data frame -> M x N (x W) 0/1 array
## searchnet_k_readings()           array -> per-node {K} degrees per period
## searchnet_k_covariate()          readings -> M x W / N x W covariate matrix
##
## The {K} definitions are those of inst/rosetta/K_DIMENSIONS.md, the single
## statement of the four dimensions. The readings here are computed exactly as
## the simulation engine computes its degree frames (saomnk_get_degrees()):
## K_AC and K_CA are the margins of B; K_AA and K_CC are distinct-partner
## counts in the off-diagonal projections B B^T and B^T B, so they EXCLUDE the
## node itself. tests/testthat/test-observed-data.R checks the equality on a
## simulated chain, step by step.
###############################################################################


# --------------------------------------------------------------------------- #
#  internal helpers
# --------------------------------------------------------------------------- #

## Default universe for an id or period column: factor levels when the column
## is a factor, otherwise the sorted unique values (numeric sort for numbers).
.obs_default_universe <- function(v) {
  if (is.factor(v)) return(levels(droplevels(v)))
  sort(unique(v))
}

## Validate an explicit universe and confirm it covers every observed value.
.obs_check_universe <- function(universe, observed, arg, colname) {
  if (anyNA(universe))
    stop("`", arg, "` contains NA; a universe lists real ids only.", call. = FALSE)
  u <- as.character(universe)
  if (anyDuplicated(u))
    stop("`", arg, "` contains duplicated values: ",
         paste(utils::head(unique(u[duplicated(u)]), 5), collapse = ", "),
         ".", call. = FALSE)
  miss <- setdiff(unique(as.character(observed)), u)
  if (length(miss))
    stop(length(miss), " value(s) of column '", colname, "' are not in `", arg,
         "`: ", paste(utils::head(miss, 5), collapse = ", "),
         if (length(miss) > 5) ", ..." else "",
         ". Add them to `", arg, "` or drop those rows first.", call. = FALSE)
  invisible(u)
}


# --------------------------------------------------------------------------- #
#  searchnet_bipartite_from_long
# --------------------------------------------------------------------------- #

#' Build the Actor-by-Component Array from Long-Format Data
#'
#' Converts observed records, one row per actor-component (and period)
#' pair, into the \eqn{M \times N}{M x N} matrix or
#' \eqn{M \times N \times W}{M x N x W} array \code{B} that SAOM-NK works
#' with: actors by components, one slice per period. This is the observed-data
#' entry point; its \code{B} goes directly to \code{\link{searchnet_k_readings}}
#' and to \code{\link{searchnet_coevolve_data}}.
#'
#' A tie is present when the (summed) weight exceeds \code{threshold}, or, when
#' \code{weight} is \code{NULL}, whenever the pair appears in the data.
#' Duplicated rows for the same actor, component and period are collapsed:
#' their weights are summed (with a message).
#'
#' Supplying \code{actors}, \code{components} or \code{periods} fixes the
#' universe and its order. An actor listed there but absent from the data, or
#' absent in some period, gets a row of zeros, so composition is stable across
#' waves. Without them the universe is the observed ids (factor levels for a
#' factor column, otherwise sorted unique values).
#'
#' @param data A data frame in long format.
#' @param actor,component Names of the actor-id and component-id columns.
#' @param period Optional name of the period (wave) column. \code{NULL} gives a
#'   single \eqn{M \times N}{M x N} matrix.
#' @param weight Optional name of a numeric weight column. \code{NULL} means
#'   presence of a row is a tie.
#' @param threshold Numeric scalar. With a weight, a tie is
#'   \code{weight > threshold}. Ignored when \code{weight} is \code{NULL}.
#' @param actors,components,periods Optional vectors fixing the universe and
#'   order of actors, components and periods.
#'
#' @return An object of class \code{"searchnet_bipartite"}: a list with
#'   \describe{
#'     \item{\code{B}}{an integer 0/1 array, \eqn{M \times N \times W}{M x N x W}
#'       with dimnames (or an \eqn{M \times N}{M x N} matrix when
#'       \code{period} is \code{NULL});}
#'     \item{\code{actors}, \code{components}}{the universes as character
#'       vectors, in row and column order;}
#'     \item{\code{periods}}{the periods in slice order, in their original type
#'       (\code{NULL} without a period column);}
#'     \item{\code{weighted}, \code{threshold}}{how ties were defined.}
#'   }
#'
#' @seealso \code{\link{searchnet_k_readings}},
#'   \code{\link{searchnet_coevolve_data}}
#' @export
#' @examples
#' long <- data.frame(
#'   actor     = c("a1", "a1", "a2", "a3", "a1", "a2", "a2"),
#'   component = c("c1", "c2", "c2", "c3", "c1", "c1", "c3"),
#'   period    = c(1, 1, 1, 1, 2, 2, 2)
#' )
#' obs <- searchnet_bipartite_from_long(long, "actor", "component", "period",
#'                                      actors = c("a1", "a2", "a3", "a4"))
#' obs
#' obs$B[, , "2"]
searchnet_bipartite_from_long <- function(data, actor, component, period = NULL,
                                          weight = NULL, threshold = 0,
                                          actors = NULL, components = NULL,
                                          periods = NULL) {
  fn <- "searchnet_bipartite_from_long()"
  if (!is.data.frame(data))
    stop("`data` must be a data frame in long format; got ", class(data)[1L],
         ".", call. = FALSE)
  .scalar_name <- function(x, arg) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
      stop("`", arg, "` must be a single column name (a string).", call. = FALSE)
  }
  .scalar_name(actor, "actor"); .scalar_name(component, "component")
  if (!is.null(period)) .scalar_name(period, "period")
  if (!is.null(weight)) .scalar_name(weight, "weight")
  cols <- c(actor = actor, component = component, period = period, weight = weight)
  if (anyDuplicated(cols))
    stop("`actor`, `component`, `period` and `weight` must name different ",
         "columns; got ", paste(sprintf("%s = '%s'", names(cols), cols),
                                collapse = ", "), ".", call. = FALSE)
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols))
    stop("Column(s) not found in `data`: ",
         paste0("'", missing_cols, "'", collapse = ", "), ". Available: ",
         paste(utils::head(names(data), 10), collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold))
    stop("`threshold` must be a single number.", call. = FALSE)

  for (k in c("actor", "component", "period")) {
    if (is.null(cols[k]) || is.na(cols[k])) next
    v <- data[[cols[[k]]]]
    if (anyNA(v))
      stop(sum(is.na(v)), " row(s) have NA in the ", k, " column '", cols[[k]],
           "'. Drop or recode them before building B.", call. = FALSE)
  }
  w <- NULL
  if (!is.null(weight)) {
    w <- data[[weight]]
    if (is.logical(w)) w <- as.numeric(w)
    if (!is.numeric(w))
      stop("Weight column '", weight, "' must be numeric; got ", class(w)[1L],
           ".", call. = FALSE)
    if (anyNA(w))
      stop(sum(is.na(w)), " row(s) have NA in the weight column '", weight,
           "'. A missing weight is not a zero; drop or impute them first.",
           call. = FALSE)
  }

  a_vals <- data[[actor]]; c_vals <- data[[component]]
  p_vals <- if (is.null(period)) NULL else data[[period]]

  if (is.null(actors)) {
    if (!nrow(data)) stop("`data` has no rows; supply `actors` and ",
                          "`components` to build an all-zero B.", call. = FALSE)
    actors <- .obs_default_universe(a_vals)
  }
  if (is.null(components)) {
    if (!nrow(data)) stop("`data` has no rows; supply `actors` and ",
                          "`components` to build an all-zero B.", call. = FALSE)
    components <- .obs_default_universe(c_vals)
  }
  act_u <- .obs_check_universe(actors, a_vals, "actors", actor)
  cmp_u <- .obs_check_universe(components, c_vals, "components", component)
  if (!is.null(period)) {
    if (is.null(periods)) {
      if (!nrow(data)) stop("`data` has no rows; supply `periods`.", call. = FALSE)
      periods <- .obs_default_universe(p_vals)
    }
    per_u <- .obs_check_universe(periods, p_vals, "periods", period)
  } else {
    if (!is.null(periods))
      stop("`periods` was supplied but `period` (the column) was not.",
           call. = FALSE)
    per_u <- "1"
  }
  M <- length(act_u); N <- length(cmp_u); W <- length(per_u)
  if (!M || !N)
    stop("The actor and component universes must each be non-empty.", call. = FALSE)

  ai <- match(as.character(a_vals), act_u)
  ci <- match(as.character(c_vals), cmp_u)
  wi <- if (is.null(period)) rep(1L, length(ai)) else match(as.character(p_vals), per_u)
  cell <- ai + (ci - 1L) * M + (wi - 1L) * M * N

  dup <- duplicated(cell)
  if (any(dup)) {
    message(fn, ": ", sum(dup), " duplicated actor-component",
            if (!is.null(period)) "-period" else "", " row(s) collapsed; ",
            if (is.null(weight)) "presence is a tie." else "their weights were summed.")
  }
  B <- array(0L, dim = c(M, N, W))
  if (length(cell)) {
    if (is.null(weight)) {
      B[unique(cell)] <- 1L
    } else {
      tot <- rowsum(w, cell, reorder = FALSE)
      B[as.integer(rownames(tot))] <- as.integer(tot[, 1L] > threshold)
    }
  }
  dimnames(B) <- list(act_u, cmp_u, per_u)
  if (is.null(period)) {
    B <- B[, , 1L, drop = TRUE]
    if (!is.matrix(B)) B <- matrix(B, M, N, dimnames = list(act_u, cmp_u))
    periods <- NULL
  }
  structure(list(B = B, actors = act_u, components = cmp_u, periods = periods,
                 weighted = !is.null(weight), threshold = threshold),
            class = "searchnet_bipartite")
}

#' @export
print.searchnet_bipartite <- function(x, ...) {
  B <- x$B
  W <- if (length(dim(B)) == 3L) dim(B)[3L] else 1L
  cat(sprintf("<searchnet_bipartite> %d actors x %d components x %d period%s\n",
              dim(B)[1L], dim(B)[2L], W, if (W == 1L) "" else "s"))
  cat("Ties: ", if (x$weighted) sprintf("summed weight > %s", format(x$threshold))
                else "presence of a row", "\n", sep = "")
  ties <- if (W == 1L) sum(B) else apply(B, 3L, sum)
  dens <- ties / (dim(B)[1L] * dim(B)[2L])
  lab <- if (is.null(x$periods)) "" else as.character(x$periods)
  tab <- data.frame(period = lab, ties = as.integer(ties),
                    density = round(dens, 3))
  print(tab, row.names = FALSE)
  invisible(x)
}


# --------------------------------------------------------------------------- #
#  searchnet_k_readings
# --------------------------------------------------------------------------- #

## Coerce the accepted inputs to a 3-d 0/1 integer array with ids and periods.
.obs_as_k_input <- function(B) {
  periods <- NULL
  if (inherits(B, "searchnet_bipartite")) {
    periods <- B$periods
    B <- B$B
  } else if (is.data.frame(B)) {
    stop("`B` is a data frame. Build the array from long data first with ",
         "searchnet_bipartite_from_long().", call. = FALSE)
  } else if (is.list(B)) {
    periods <- names(B)
    B <- .searchnet_as_wave_array(B, "B")
  }
  if (is.matrix(B)) {
    dn <- dimnames(B)
    B <- array(B, dim = c(dim(B), 1L), dimnames = c(if (is.null(dn)) list(NULL, NULL) else dn, list(NULL)))
    single <- TRUE
  } else if (is.array(B) && length(dim(B)) == 3L) {
    single <- FALSE
  } else {
    stop("`B` must be an M x N matrix, an M x N x W array, a list of M x N ",
         "matrices, or the result of searchnet_bipartite_from_long().",
         call. = FALSE)
  }
  if (is.logical(B)) storage.mode(B) <- "integer"
  if (!is.numeric(B))
    stop("`B` must be numeric 0/1.", call. = FALSE)
  if (anyNA(B))
    stop("`B` contains NA. {K} readings need a complete matrix; resolve ",
         "missing ties before reading the dimensions.", call. = FALSE)
  if (any(B != 0 & B != 1))
    stop("`B` must be 0/1. For valued data, threshold it with ",
         "searchnet_bipartite_from_long(weight = , threshold = ).", call. = FALSE)
  M <- dim(B)[1L]; N <- dim(B)[2L]; W <- dim(B)[3L]
  dn <- dimnames(B)
  act <- if (!is.null(dn[[1L]])) dn[[1L]] else as.character(seq_len(M))
  cmp <- if (!is.null(dn[[2L]])) dn[[2L]] else as.character(seq_len(N))
  if (is.null(periods)) {
    periods <- if (single) NA_integer_
               else if (!is.null(dn[[3L]])) dn[[3L]] else seq_len(W)
  }
  if (length(periods) != W) periods <- seq_len(W)
  storage.mode(B) <- "integer"
  list(B = B, actors = act, components = cmp, periods = periods)
}

## Jaccard overlap of row portfolios, averaged over pairs with a non-empty
## union. `S` is the full (diagonal-included) row projection.
.obs_mean_jaccard <- function(S) {
  d <- diag(S)
  if (length(d) < 2L) return(NA_real_)
  U <- outer(d, d, "+") - S
  ut <- upper.tri(S)
  ok <- ut & U > 0
  if (!any(ok)) return(NA_real_)
  mean(S[ok] / U[ok])
}

#' \{K\} Readings of an Observed Actor-by-Component Network
#'
#' Reads the four \{K\} dimensions off an actor-by-component matrix \code{B},
#' per node and per period, using the definitions in
#' \code{system.file("rosetta", "K_DIMENSIONS.md", package = "searchnet")}:
#' \describe{
#'   \item{Expansiveness \eqn{K_{AC}(i)}}{components actor \eqn{i} holds (row sum);}
#'   \item{Popularity \eqn{K_{CA}(j)}}{actors holding component \eqn{j} (column sum);}
#'   \item{Sociality \eqn{K_{AA}(i)}}{OTHER actors sharing at least one
#'     component with \eqn{i}, the off-diagonal degree of \eqn{BB^T};}
#'   \item{Epistasis \eqn{K_{CC}(j)}}{OTHER components co-held with \eqn{j},
#'     the off-diagonal degree of \eqn{B^TB}.}
#' }
#' These are the same quantities, computed the same way, as the simulation
#' engine's degree frames (\code{\link{saomnk_get_degrees}}, the \{K\}-4 panel),
#' so an observed panel and a simulated chain can be compared directly.
#'
#' The weighted forms are reported alongside: Sociality strength
#' \eqn{\sum_{h \ne i} (BB^T)_{ih}} (overlap counted by shared components) and
#' Epistasis strength \eqn{\sum_{k \ne j} (B^TB)_{jk}} (co-holding counted by
#' actors). The accounting identities of K_DIMENSIONS.md section 3 are
#' asserted for every period, and the function errors if any fails:
#' (I1) \eqn{\sum_i K_{AC} = \sum_j K_{CA} = |B|};
#' (I2) \eqn{\sum_j K_{CA}(K_{CA}-1)} = total Sociality strength;
#' (I3) \eqn{\sum_i K_{AC}(K_{AC}-1)} = total Epistasis strength.
#' The identities hold for strengths, not for the partner-count degrees.
#'
#' @param B An \eqn{M \times N}{M x N} 0/1 matrix, an
#'   \eqn{M \times N \times W}{M x N x W} array, a list of \eqn{W} matrices, or
#'   the result of \code{\link{searchnet_bipartite_from_long}}.
#'
#' @return A data frame with one row per node and period and columns
#'   \code{period}, \code{level} (\code{"actor"} or \code{"component"}),
#'   \code{id}, \code{K_AC}, \code{K_AA}, \code{sociality_strength} (actor rows)
#'   and \code{K_CA}, \code{K_CC}, \code{epistasis_strength} (component rows);
#'   the columns that do not apply to a level are \code{NA}. \code{period} is
#'   \code{NA} for a single matrix. Attribute \code{"summary"} is a per-period
#'   data frame: \code{n_actors}, \code{n_components}, \code{ties},
#'   \code{density}, the four mean degrees, the total Sociality and Epistasis
#'   strengths, \code{actor_jaccard} and \code{component_jaccard} (mean
#'   pairwise Jaccard overlap of portfolios and of holder sets, over pairs with
#'   a non-empty union) and \code{tie_jaccard} (Jaccard of the tie sets of a
#'   period and the one before it, the stability index SAOM users check before
#'   estimation; \code{NA} for the first period).
#'
#' @seealso \code{\link{searchnet_bipartite_from_long}},
#'   \code{\link{searchnet_k_covariate}}, \code{\link{saomnk_get_degrees}}
#' @export
#' @examples
#' set.seed(1)
#' B <- array(rbinom(6 * 5 * 3, 1, 0.3), dim = c(6, 5, 3))
#' k <- searchnet_k_readings(B)
#' head(k)
#' attr(k, "summary")
searchnet_k_readings <- function(B) {
  inp <- .obs_as_k_input(B)
  arr <- inp$B
  M <- dim(arr)[1L]; N <- dim(arr)[2L]; W <- dim(arr)[3L]
  rows <- vector("list", W); summ <- vector("list", W)
  for (w in seq_len(W)) {
    b <- matrix(arr[, , w], M, N)
    S <- tcrossprod(b); P <- crossprod(b)
    k_ac <- as.integer(diag(S)); k_ca <- as.integer(diag(P))
    S0 <- S; diag(S0) <- 0L; P0 <- P; diag(P0) <- 0L
    k_aa <- as.integer(rowSums(S0 > 0)); k_cc <- as.integer(rowSums(P0 > 0))
    s_str <- as.integer(rowSums(S0)); e_str <- as.integer(rowSums(P0))
    ties <- sum(b)
    ## ---- accounting identities (K_DIMENSIONS.md section 3) ----------------
    if (sum(k_ac) != ties || sum(k_ca) != ties)
      stop("Identity I1 failed in period ", w, ": sum K_AC = ", sum(k_ac),
           ", sum K_CA = ", sum(k_ca), ", |B| = ", ties, ".", call. = FALSE)
    if (sum(as.numeric(k_ca) * (k_ca - 1)) != sum(as.numeric(s_str)))
      stop("Identity I2 failed in period ", w, ".", call. = FALSE)
    if (sum(as.numeric(k_ac) * (k_ac - 1)) != sum(as.numeric(e_str)))
      stop("Identity I3 failed in period ", w, ".", call. = FALSE)
    if (any(k_aa > M - 1L) || any(k_cc > N - 1L))
      stop("A projection degree exceeds the number of other nodes in period ",
           w, ".", call. = FALSE)

    per <- inp$periods[w]
    rows[[w]] <- data.frame(
      period = rep(per, M + N),
      level  = rep(c("actor", "component"), c(M, N)),
      id     = c(inp$actors, inp$components),
      K_AC   = c(k_ac, rep(NA_integer_, N)),
      K_AA   = c(k_aa, rep(NA_integer_, N)),
      sociality_strength = c(s_str, rep(NA_integer_, N)),
      K_CA   = c(rep(NA_integer_, M), k_ca),
      K_CC   = c(rep(NA_integer_, M), k_cc),
      epistasis_strength = c(rep(NA_integer_, M), e_str),
      stringsAsFactors = FALSE)
    tj <- NA_real_
    if (w > 1L) {
      prev <- arr[, , w - 1L]; cur <- arr[, , w]
      u <- sum(prev | cur)
      tj <- if (u > 0) sum(prev & cur) / u else NA_real_
    }
    summ[[w]] <- data.frame(
      period = per, n_actors = M, n_components = N, ties = ties,
      density = ties / (M * N),
      mean_K_AC = mean(k_ac), mean_K_CA = mean(k_ca),
      mean_K_AA = mean(k_aa), mean_K_CC = mean(k_cc),
      sociality_strength = sum(s_str), epistasis_strength = sum(e_str),
      actor_jaccard = .obs_mean_jaccard(S),
      component_jaccard = .obs_mean_jaccard(P),
      tie_jaccard = tj,
      stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  s <- do.call(rbind, summ); rownames(s) <- NULL
  attr(out, "summary") <- s
  out
}


# --------------------------------------------------------------------------- #
#  searchnet_k_covariate
# --------------------------------------------------------------------------- #

#' A \{K\} Reading as a Node-by-Period Covariate Matrix
#'
#' Reshapes one column of \code{\link{searchnet_k_readings}} into an
#' \eqn{M \times W}{M x W} (actor) or \eqn{N \times W}{N x W} (component)
#' matrix, the shape \code{\link{searchnet_coevolve_data}} takes for a varying
#' covariate in \code{actor_covars} or \code{component_covars}.
#'
#' A degree of the dependent network entered as a covariate is a lagged
#' reading of the network itself, a different estimand from the endogenous
#' effects (\code{outAct}, \code{inPop}) that model the same degree as a
#' mechanism. Use it for description or as a lagged control, and say which.
#'
#' @param readings The data frame returned by \code{\link{searchnet_k_readings}}.
#' @param dimension One of \code{"K_AC"}, \code{"K_AA"},
#'   \code{"sociality_strength"} (actor level) or \code{"K_CA"}, \code{"K_CC"},
#'   \code{"epistasis_strength"} (component level).
#' @return A numeric matrix, nodes by periods, with dimnames.
#' @export
#' @examples
#' set.seed(2)
#' B <- array(rbinom(5 * 4 * 3, 1, 0.4), dim = c(5, 4, 3))
#' searchnet_k_covariate(searchnet_k_readings(B), "K_AA")
searchnet_k_covariate <- function(readings, dimension = c("K_AC", "K_AA",
                                  "sociality_strength", "K_CA", "K_CC",
                                  "epistasis_strength")) {
  dimension <- match.arg(dimension)
  need <- c("period", "level", "id", dimension)
  if (!is.data.frame(readings) || !all(need %in% names(readings)))
    stop("`readings` must be the data frame returned by searchnet_k_readings().",
         call. = FALSE)
  lvl <- if (dimension %in% c("K_AC", "K_AA", "sociality_strength")) "actor"
         else "component"
  r <- readings[readings$level == lvl, , drop = FALSE]
  ids <- unique(r$id); pers <- unique(r$period)
  out <- matrix(NA_real_, length(ids), length(pers),
                dimnames = list(ids, as.character(pers)))
  out[cbind(match(r$id, ids), match(r$period, pers))] <- r[[dimension]]
  out
}
