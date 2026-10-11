################################################################################
# CLASSIC NK LANDSCAPES  (Kauffman & Levin 1987; Kauffman 1993; Levinthal 1997)
#
# A self-contained implementation of the standard NK model, independent of
# RSiena and of the SAOM-NK engine.  Its purpose is twofold:
#
#   1. Provide the conventional NK model as first-class functionality, so
#      users can run classical experiments (adaptive walks, local-optima
#      counts, ruggedness sweeps) directly in searchnet.
#
#   2. Serve as an EXTERNAL reference landscape for the SAOM-NK reduction
#      (Property 1).  `nk_verify_reduction()` checks this module's encoding
#      (fitness recomputed from the stored per-locus tables with an
#      independently coded bit order) and, with `engine = TRUE`, evaluates the
#      same landscape through the engine's utility methods for one actor.  It
#      does not exercise the engine's own landscape generator
#      (compute_fitness_landscape(), which draws its own tables) or the RSiena
#      ministep path (which refuses M = 1).  The environment method
#      `verify_nk_equivalence()` is a consistency check of the engine's
#      stored landscape array.
#
# Encoding convention (consistent throughout, and with the engine):
#   A configuration is a binary vector b = (b_1, ..., b_N).
#   Its integer code is  sum(b * 2^((N-1):0))  in 0..2^N-1, i.e. b_1 is the
#   most significant bit.  Landscapes store fitness indexed by code + 1.
#
# Adapted from the reference script _star_poc/standalone_nk.R, whose
# vectorized bit-code construction is retained here.
################################################################################


# ==============================================================================
# Epistatic neighborhood construction
# ==============================================================================

#' Epistatic dependencies for an NK landscape
#'
#' Builds the list of loci on which each locus's fitness contribution depends.
#' Locus \code{i} always depends on itself plus \code{K} others.
#'
#' @param N Integer. Number of loci (components).
#' @param K Integer. Number of epistatic partners per locus, \code{0 <= K <= N-1}.
#' @param model Character. Neighborhood topology:
#'   \describe{
#'     \item{\code{"adjacent"}}{Ring topology: partners are the nearest loci on
#'       a circle (Kauffman's "neighborhood" model). Deterministic.}
#'     \item{\code{"random"}}{Partners drawn uniformly without replacement from
#'       the other loci (Kauffman's "random" model).}
#'     \item{\code{"block"}}{Near-decomposable: loci are partitioned into blocks
#'       of size \code{K + 1} and depend only on their own block (Simon 1962;
#'       Baldwin & Clark 2000). \code{N} need not divide evenly; the final
#'       block absorbs the remainder.}
#'   }
#'
#' @return A list of length \code{N}; element \code{i} is an integer vector of
#'   the loci (including \code{i}) that determine contribution \code{f_i}.
#'
#' @examples
#' nk_dependencies(6, K = 2, model = "adjacent")
#' nk_dependencies(6, K = 2, model = "block")
#'
#' @seealso \code{\link{nk_landscape}}
#' @export
nk_dependencies <- function(N, K, model = c("adjacent", "random", "block")) {
  model <- match.arg(model)
  stopifnot(N >= 1, K >= 0, K <= N - 1)

  if (model == "adjacent") {
    lapply(seq_len(N), function(i) {
      if (K <= 0) return(i)
      if (K >= N - 1) return(seq_len(N))
      half <- K %/% 2
      nb <- integer(0)
      for (d in seq_len(half)) {
        nb <- c(nb, ((i - 1 + d) %% N) + 1, ((i - 1 - d) %% N) + 1)
      }
      if (K %% 2 == 1) nb <- c(nb, ((i - 1 + N %/% 2) %% N) + 1)
      sort(unique(c(i, nb)))
    })
  } else if (model == "random") {
    lapply(seq_len(N), function(i) {
      if (K <= 0) return(i)
      others <- setdiff(seq_len(N), i)
      sort(c(i, sample(others, size = min(K, length(others)))))
    })
  } else {  # block
    block_size <- K + 1
    lapply(seq_len(N), function(i) {
      b <- (i - 1) %/% block_size
      idx <- seq(b * block_size + 1, min((b + 1) * block_size, N))
      # final short block: extend backwards so every locus keeps K partners
      if (length(idx) < block_size && N >= block_size) {
        idx <- seq(N - block_size + 1, N)
      }
      sort(unique(idx))
    })
  }
}


# ==============================================================================
# Landscape construction
# ==============================================================================

#' Generate a classic NK fitness landscape
#'
#' Constructs a Kauffman NK landscape by exhaustive enumeration of all
#' \eqn{2^N} configurations.  Each locus \eqn{i} contributes
#' \eqn{f_i(b_i, b_{\Omega(i)})}, drawn i.i.d. \eqn{U(0,1)} for every
#' distinct pattern of its epistatic neighborhood; total fitness is the
#' mean of the \eqn{N} contributions.
#'
#' @param N Integer. Number of loci. Exhaustive enumeration limits practical
#'   use to \code{N <= 20} (\eqn{2^{20} \approx} 1M configurations).
#' @param K Integer. Epistatic partners per locus, \code{0 <= K <= N-1}.
#'   \code{K = 0} gives a smooth, single-peaked landscape; \code{K = N-1}
#'   gives a maximally rugged (effectively random) landscape.
#' @param model Character. Neighborhood topology; see
#'   \code{\link{nk_dependencies}}.
#' @param seed Integer or \code{NULL}. Random seed for reproducibility.
#'
#' @return An object of class \code{"nk_landscape"}: a list with
#'   \describe{
#'     \item{\code{N}, \code{K}, \code{model}, \code{seed}}{Parameters as supplied.}
#'     \item{\code{fitness}}{Numeric vector of length \eqn{2^N}. Element
#'       \code{code + 1} is the fitness of the configuration with integer
#'       code \code{code}.}
#'     \item{\code{contributions}}{Matrix \eqn{2^N \times N} of per-locus
#'       contributions, rows in code order.}
#'     \item{\code{configs}}{Matrix \eqn{2^N \times N} of configuration bits,
#'       rows in code order.}
#'     \item{\code{dependencies}}{List of epistatic neighborhoods.}
#'     \item{\code{tables}}{List of per-locus payoff tables. Element \code{i}
#'       has length \eqn{2^{K_i + 1}}, where \eqn{K_i + 1} is the size of
#'       \code{dependencies[[i]]}; entry \code{p + 1} is \eqn{f_i} for the
#'       neighborhood pattern with key \eqn{p = \sum_k b_{d_k} 2^{k-1}},
#'       \eqn{d_k} the \eqn{k}-th element of \code{dependencies[[i]]}.}
#'     \item{\code{influence_matrix}}{\eqn{N \times N} binary matrix;
#'       \code{[i, j] = 1} iff locus \code{j} influences contribution
#'       \code{f_i}. Diagonal is 1.}
#'   }
#'
#' @references
#' Kauffman, S. A., & Levin, S. (1987). Towards a general theory of adaptive
#' walks on rugged landscapes. \emph{Journal of Theoretical Biology},
#' \bold{128}(1), 11--45.
#'
#' Levinthal, D. A. (1997). Adaptation on rugged landscapes.
#' \emph{Management Science}, \bold{43}(7), 934--950.
#'
#' @examples
#' nk <- nk_landscape(N = 8, K = 2, seed = 42)
#' nk
#' summary(nk_local_optima(nk))
#'
#' @seealso \code{\link{nk_walk}}, \code{\link{nk_local_optima}},
#'   \code{\link{nk_to_saomnk}}
#' @export
nk_landscape <- function(N, K,
                         model = c("adjacent", "random", "block"),
                         seed = NULL) {
  model <- match.arg(model)
  stopifnot(N >= 1, N <= 22, K >= 0, K <= N - 1)
  if (N > 20) {
    warning("N > 20: exhaustive enumeration requires ", 2^N,
            " rows and may exhaust memory.")
  }
  if (!is.null(seed)) set.seed(seed)

  deps <- nk_dependencies(N, K, model)

  # All configurations, in integer-code order (b_1 = most significant bit).
  n_conf <- 2L^N
  configs <- matrix(0L, nrow = n_conf, ncol = N)
  for (j in seq_len(N)) {
    period <- 2L^(N - j)
    configs[, j] <- rep(rep(c(0L, 1L), each = period),
                        length.out = n_conf)
  }

  # Per-locus contributions: f_i depends only on the bits in deps[[i]].
  contributions <- matrix(0, nrow = n_conf, ncol = N)
  tables <- vector("list", N)
  for (i in seq_len(N)) {
    di <- deps[[i]]
    bw <- 2^(seq_along(di) - 1)
    pat <- as.integer(configs[, di, drop = FALSE] %*% bw)  # 0 .. 2^|di| - 1
    tab <- stats::runif(2^length(di))
    tables[[i]] <- tab
    contributions[, i] <- tab[pat + 1L]
  }

  fitness <- rowMeans(contributions)

  E <- matrix(0L, N, N, dimnames = list(paste0("c", seq_len(N)),
                                        paste0("c", seq_len(N))))
  for (i in seq_len(N)) E[i, deps[[i]]] <- 1L

  structure(list(
    N = N, K = K, model = model, seed = seed,
    fitness = fitness,
    contributions = contributions,
    configs = configs,
    dependencies = deps,
    tables = tables,
    influence_matrix = E
  ), class = "nk_landscape")
}


#' @export
print.nk_landscape <- function(x, ...) {
  cat(sprintf("<nk_landscape>  N = %d, K = %d, model = \"%s\"\n",
              x$N, x$K, x$model))
  cat(sprintf("  configurations : %s\n", format(length(x$fitness), big.mark = ",")))
  cat(sprintf("  fitness range  : [%.4f, %.4f]  mean %.4f\n",
              min(x$fitness), max(x$fitness), mean(x$fitness)))
  if (x$N <= 16) {
    n_opt <- nrow(nk_local_optima(x))
    if (x$K == x$N - 1L) {
      cat(sprintf("  local optima   : %d  (fully random limit 2^N/(N+1) = %.1f)\n",
                  n_opt, 2^x$N / (x$N + 1)))
    } else {
      cat(sprintf("  local optima   : %d\n", n_opt))
    }
  }
  invisible(x)
}


# ==============================================================================
# Configuration helpers
# ==============================================================================

#' Convert between configuration bits and integer codes
#'
#' @param bits Integer vector or matrix of 0/1 configuration bits (rows are
#'   configurations; \code{b_1} is the most significant bit).
#' @param code Integer vector of configuration codes in \code{0:(2^N - 1)}.
#' @param N Integer. Number of loci.
#'
#' @return \code{nk_code()} returns integer codes; \code{nk_bits()} returns a
#'   matrix of bits with \code{N} columns.
#'
#' @examples
#' nk_code(c(1, 0, 1))       # 5
#' nk_bits(5, N = 3)         # 1 0 1
#'
#' @name nk_encoding
#' @export
nk_code <- function(bits) {
  if (is.null(dim(bits))) bits <- matrix(bits, nrow = 1)
  N <- ncol(bits)
  as.integer(bits %*% 2^((N - 1):0))
}

#' @rdname nk_encoding
#' @export
nk_bits <- function(code, N) {
  # intToBits() is little-endian (LSB first). The package convention is
  # b_1 = most significant bit, so take the low N bits and reverse them.
  # (Writing this explicitly: a rev/head idiom here silently selected the
  # HIGH bits of the 32-bit word and returned all zeros.)
  mat <- vapply(code, function(cc) {
    lo <- as.integer(intToBits(as.integer(cc)))[seq_len(N)]  # LSB..MSB
    rev(lo)                                                  # MSB..LSB
  }, integer(N))
  # vapply returns N x length(code); for N == 1 it drops to a plain vector.
  matrix(as.integer(mat), nrow = length(code), ncol = N, byrow = TRUE)
}


#' Fitness of specified configurations
#'
#' @param landscape An \code{"nk_landscape"} object.
#' @param config Either an integer vector of configuration codes, or a binary
#'   vector/matrix of configuration bits. If \code{NULL}, all fitness values
#'   are returned.
#'
#' @return Numeric vector of fitness values.
#'
#' @examples
#' nk <- nk_landscape(N = 6, K = 2, seed = 1)
#' nk_fitness(nk, c(1, 0, 1, 1, 0, 0))
#'
#' @export
nk_fitness <- function(landscape, config = NULL) {
  stopifnot(inherits(landscape, "nk_landscape"))
  if (is.null(config)) return(landscape$fitness)
  code <- if (is.null(dim(config)) && length(config) == landscape$N &&
               all(config %in% c(0, 1))) {
    nk_code(config)
  } else if (!is.null(dim(config))) {
    nk_code(config)
  } else {
    as.integer(config)
  }
  landscape$fitness[code + 1L]
}


# ==============================================================================
# Local optima
# ==============================================================================

#' Enumerate local optima of an NK landscape
#'
#' A configuration is a local optimum if its fitness is greater than or equal
#' to that of all \eqn{N} single-bit-flip (Hamming-1) neighbors.  The count
#' grows with \eqn{K}; in the fully random case \eqn{K = N - 1} its
#' expectation is \eqn{2^N / (N + 1)}.
#'
#' @param landscape An \code{"nk_landscape"} object.
#' @param strict Logical. If \code{TRUE}, require strictly greater fitness than
#'   every neighbor. Default \code{FALSE} (>=), matching the standard
#'   convention.
#'
#' @return A \code{data.frame} with columns \code{code}, \code{fitness}, and
#'   \code{rank} (1 = global optimum), ordered by decreasing fitness.
#'
#' @examples
#' nk0 <- nk_landscape(N = 10, K = 0, seed = 1)
#' nrow(nk_local_optima(nk0))   # 1: K = 0 is single-peaked
#'
#' nk5 <- nk_landscape(N = 10, K = 5, seed = 1)
#' nrow(nk_local_optima(nk5))   # many
#'
#' @export
nk_local_optima <- function(landscape, strict = FALSE) {
  stopifnot(inherits(landscape, "nk_landscape"))
  N <- landscape$N
  fit <- landscape$fitness
  codes <- seq_along(fit) - 1L

  is_opt <- rep(TRUE, length(fit))
  for (j in seq_len(N)) {
    nbr <- bitwXor(codes, as.integer(2^(N - j)))
    is_opt <- if (strict) is_opt & (fit > fit[nbr + 1L]) else
                          is_opt & (fit >= fit[nbr + 1L])
  }

  out <- data.frame(code = codes[is_opt], fitness = fit[is_opt])
  out <- out[order(-out$fitness), , drop = FALSE]
  out$rank <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}


# ==============================================================================
# Adaptive walks
# ==============================================================================

#' Adaptive walk on an NK landscape
#'
#' Performs a classical local search from a starting configuration, moving
#' among Hamming-1 neighbors until a local optimum is reached.
#'
#' @param landscape An \code{"nk_landscape"} object.
#' @param start Integer code, binary vector, or \code{NULL} for a random start.
#' @param type Character. Search rule:
#'   \describe{
#'     \item{\code{"steepest"}}{Move to the best improving neighbor
#'       (steepest-ascent hill climbing). The standard NK adaptive walk.}
#'     \item{\code{"greedy"}}{Move to the first improving neighbor found,
#'       scanning loci in random order (random-ascent).}
#'     \item{\code{"random"}}{Move to a randomly chosen improving neighbor.}
#'   }
#' @param max_steps Integer. Safety cap on the number of moves.
#'
#' @return An object of class \code{"nk_walk"}: a list with \code{path}
#'   (integer codes visited, start and terminal included), \code{fitness}
#'   (fitness at each step), \code{terminal} (final code), \code{steps}
#'   (moves made), and \code{is_local_optimum} (\code{TRUE} when the terminal
#'   configuration has no strictly improving neighbor; \code{FALSE} when the
#'   walk stopped at \code{max_steps} first).
#'
#' @examples
#' nk <- nk_landscape(N = 10, K = 3, seed = 7)
#' w  <- nk_walk(nk, start = 0, type = "steepest")
#' w$steps
#' tail(w$fitness, 1)
#'
#' @export
nk_walk <- function(landscape, start = NULL,
                    type = c("steepest", "greedy", "random"),
                    max_steps = 1000L) {
  stopifnot(inherits(landscape, "nk_landscape"))
  type <- match.arg(type)
  N <- landscape$N
  fit <- landscape$fitness

  cur <- if (is.null(start)) {
    sample.int(length(fit), 1L) - 1L
  } else if (length(start) == N && all(start %in% c(0, 1))) {
    nk_code(start)
  } else {
    as.integer(start)
  }

  improving_nbrs <- function(code) {
    nbrs <- vapply(seq_len(N), function(j) bitwXor(code, as.integer(2^(N - j))),
                   integer(1))
    nf <- fit[nbrs + 1L]
    improving <- which(nf > fit[code + 1L])
    list(nbrs = nbrs, nf = nf, improving = improving)
  }

  path <- cur
  fpath <- fit[cur + 1L]
  for (s in seq_len(max_steps)) {
    nb <- improving_nbrs(cur)
    if (length(nb$improving) == 0L) break
    improving <- nb$improving
    nxt <- switch(type,
      steepest = nb$nbrs[improving[which.max(nb$nf[improving])]],
      greedy   = nb$nbrs[improving[sample.int(length(improving), 1L)][1L]],
      random   = nb$nbrs[improving[sample.int(length(improving), 1L)]]
    )
    cur <- nxt
    path <- c(path, cur)
    fpath <- c(fpath, fit[cur + 1L])
  }

  ## Computed, not assumed: a walk cut off by max_steps can end on a
  ## configuration that still has an improving neighbor.
  structure(list(
    path = path,
    fitness = fpath,
    terminal = cur,
    steps = length(path) - 1L,
    is_local_optimum = length(improving_nbrs(cur)$improving) == 0L,
    type = type
  ), class = "nk_walk")
}


#' @export
print.nk_walk <- function(x, ...) {
  cat(sprintf("<nk_walk>  type = \"%s\",  steps = %d\n", x$type, x$steps))
  cat(sprintf("  start fitness : %.4f\n", x$fitness[1]))
  cat(sprintf("  final fitness : %.4f  (code %d)\n",
              x$fitness[length(x$fitness)], x$terminal))
  invisible(x)
}


# ==============================================================================
# Ruggedness sweep
# ==============================================================================

#' Sweep landscape statistics across K
#'
#' Replicates the canonical NK ruggedness experiment: as \eqn{K} rises, the
#' number of local optima grows (to about \eqn{2^N / (N + 1)} at \eqn{K = N - 1})
#' and adaptive walks terminate sooner.
#'
#' @param N Integer. Number of loci.
#' @param K_values Integer vector of \eqn{K} levels to sweep.
#' @param n_landscapes Integer. Replicate landscapes per \eqn{K}.
#' @param n_walks Integer. Adaptive walks per landscape (random starts).
#' @param model Character. Neighborhood topology.
#' @param seed Integer or \code{NULL}.
#'
#' @return A \code{data.frame} with one row per \eqn{K} and columns \code{N},
#'   \code{K}, \code{n_optima} (mean local-optimum count over the
#'   landscapes), \code{expected_optima} (the fully random expectation
#'   \eqn{2^N / (N + 1)}, given only in the row with \eqn{K = N - 1} and
#'   \code{NA} otherwise, because that expectation holds only in the fully
#'   random case),
#'   \code{global_fitness} (mean global-optimum fitness),
#'   \code{mean_walk_steps}, and \code{mean_terminal_fitness} (steepest-ascent
#'   walks from random starts).
#'
#' @examples
#' \donttest{
#' nk_sweep_K(N = 10, K_values = c(0, 2, 4, 8), n_landscapes = 3)
#' }
#'
#' @export
nk_sweep_K <- function(N = 12, K_values = 0:(N - 1),
                       n_landscapes = 10, n_walks = 20,
                       model = c("adjacent", "random", "block"),
                       seed = NULL) {
  model <- match.arg(model)
  if (!is.null(seed)) set.seed(seed)

  rows <- lapply(K_values, function(k) {
    stats_l <- lapply(seq_len(n_landscapes), function(l) {
      nk <- nk_landscape(N, k, model = model)
      opt <- nk_local_optima(nk)
      walks <- lapply(seq_len(n_walks), function(w) nk_walk(nk, type = "steepest"))
      data.frame(
        n_optima = nrow(opt),
        global_fitness = max(nk$fitness),
        mean_walk_steps = mean(vapply(walks, function(w) w$steps, numeric(1))),
        mean_terminal_fitness = mean(vapply(walks,
          function(w) w$fitness[length(w$fitness)], numeric(1)))
      )
    })
    d <- do.call(rbind, stats_l)
    data.frame(
      N = N, K = k,
      n_optima = mean(d$n_optima),
      expected_optima = if (k == N - 1) 2^N / (N + 1) else NA_real_,
      global_fitness = mean(d$global_fitness),
      mean_walk_steps = mean(d$mean_walk_steps),
      mean_terminal_fitness = mean(d$mean_terminal_fitness)
    )
  })
  do.call(rbind, rows)
}


# ==============================================================================
# Bridge to SAOM-NK
# ==============================================================================

#' Convert a classic NK landscape to a SAOM-NK configuration
#'
#' Emits environment and structure-model settings matching the supplied NK
#' landscape's size and influence pattern (Property 1: NK is the
#' \eqn{M = 1}, \eqn{\beta \to \infty} special case of SAOM-NK).  The payoff
#' tables are not transferred: an environment built from these settings draws
#' its own tables in \code{compute_fitness_landscape()}.  To evaluate this
#' landscape itself through the engine, see
#' \code{nk_verify_reduction(engine = TRUE)}.
#'
#' @param landscape An \code{"nk_landscape"} object.
#' @param M Integer. Number of actors. \code{M = 1} is the reduction setting;
#'   \code{M > 1} uses the same influence pattern in a multi-actor SAOM-NK
#'   search (the generalization direction).
#'
#' @return A list with components \code{env_params} (arguments for
#'   \code{\link{saomnk_env}}), \code{model_params} (arguments for
#'   \code{\link{saomnk_model}}), \code{influence_matrix}, and
#'   \code{landscape} (the source object).
#'
#' @examples
#' nk <- nk_landscape(N = 8, K = 2, seed = 42)
#' spec <- nk_to_saomnk(nk)
#' str(spec$env_params)
#'
#' @seealso \code{\link{nk_verify_reduction}}
#' @export
nk_to_saomnk <- function(landscape, M = 1L) {
  stopifnot(inherits(landscape, "nk_landscape"))
  list(
    env_params = list(
      M = M,
      N = landscape$N,
      BI_PROB = 0.5,
      name = sprintf("_nk_N%d_K%d_", landscape$N, landscape$K)
    ),
    model_params = list(
      density = 0,
      influence_matrix = landscape$influence_matrix,
      influence_weight = 1
    ),
    influence_matrix = landscape$influence_matrix,
    landscape = landscape
  )
}


#' Check an NK landscape's encoding and, optionally, its reading by the engine
#'
#' Two checks with separately reported results.
#'
#' \strong{Encoding check (always run).} Recomputes the fitness of every
#' configuration from the per-locus payoff tables (\code{landscape$tables})
#' with a separately coded bit order: the bits of each configuration are read
#' from its integer code with \code{bitwAnd()} and \code{bitwShiftR()}
#' (\eqn{b_1} the most significant bit), not taken from
#' \code{landscape$configs}, and each locus's table key is assembled with
#' \code{bitwShiftL()} and \code{bitwOr()} rather than by a matrix product.
#' The result is compared with \code{landscape$fitness}, and
#' \code{landscape$configs} is compared bit by bit with the decoded bits. An
#' endianness error, a permuted bit order, or a table key built in a
#' different order shows up as a non-zero difference. This is a
#' self-consistency check of \code{nk_landscape()} and the package's encoding
#' convention; it does not involve the SAOM-NK engine.
#'
#' \strong{Engine check (\code{engine = TRUE}).} Places the same landscape in
#' a single-actor \code{SaomNkRSienaBiEnv}: the configurations, per-locus
#' contributions and fitness are written into the environment's
#' \code{fitness_landscape} array in the engine's row order (the
#' \code{expand.grid()} order produced by \code{compute_fitness_landscape()},
#' first bit least significant), with the landscape's influence matrix as
#' \code{component_1_coDyadCovar}. For every configuration the engine's
#' \code{compute_formal_utility()}, with only \eqn{\beta_F = 1} (all network
#' and cost weights 0, as in the reduction conditions), is compared with the
#' NK fitness. A steepest-ascent walk driven by the engine's
#' \code{compute_choice_probabilities()} utility changes is then compared
#' step by step with \code{nk_walk(type = "steepest")} from the same start.
#' This exercises the engine's reading of a stored landscape and its choice
#' utilities. It does not exercise \code{compute_fitness_landscape()}, which
#' draws its own payoff tables and cannot take these, and it does not run the
#' RSiena ministep simulation, which refuses \eqn{M = 1}.
#'
#' Other checks of Property 1: the environment method
#' \code{verify_nk_equivalence()} checks that the engine's stored landscape
#' array is internally consistent (configurations sharing a masked code carry
#' the same contribution, and fitness is their mean); Lemmas 1 and 2 (proof
#' table C6 and C8) prove payoff and dynamic equivalence analytically.
#'
#' @param N Integer. Number of loci (ignored when \code{landscape} is given).
#' @param K Integer. Epistatic partners per locus (ignored when
#'   \code{landscape} is given).
#' @param model Character. Neighborhood topology; see
#'   \code{\link{nk_dependencies}}. Default \code{"adjacent"}, as in
#'   \code{\link{nk_landscape}}.
#' @param seed Integer or \code{NULL}. Seed for the landscape.
#' @param tolerance Numeric. Maximum permitted absolute difference.
#' @param engine Logical. Also run the engine check (default \code{FALSE}).
#'   Requires \code{N <= 12}.
#' @param walk_start Integer code at which the engine-driven walk starts
#'   (default 0).
#' @param landscape An \code{"nk_landscape"} object to check instead of
#'   building one from \code{N}, \code{K}, \code{model} and \code{seed}.
#' @param verbose Logical. Print a summary (default \code{TRUE}).
#'
#' @return Invisibly, a list with \code{encoding_max_difference} (maximum
#'   absolute difference between stored fitness and fitness recomputed from
#'   the tables), \code{config_mismatches} (bits of \code{landscape$configs}
#'   that disagree with the decoded codes), \code{n_configs}, \code{model},
#'   and \code{passed}. With \code{engine = TRUE} also
#'   \code{engine_max_difference} (maximum absolute difference between the
#'   engine's formal utility and the NK fitness over all configurations),
#'   \code{engine_walk_identical} (whether the engine-driven and
#'   \code{nk_walk()} paths coincide), and \code{engine_walk_steps}; then
#'   \code{passed} requires both checks to pass.
#'
#' @examples
#' nk_verify_reduction(N = 10, K = 3, model = "random", seed = 42)
#' \donttest{
#' nk_verify_reduction(N = 8, K = 3, model = "random", seed = 42, engine = TRUE)
#' }
#'
#' @seealso \code{\link{nk_landscape}}, \code{\link{nk_to_saomnk}}
#' @export
nk_verify_reduction <- function(N = 10, K = 3,
                                model = c("adjacent", "random", "block"),
                                seed = NULL, tolerance = 1e-12,
                                engine = FALSE, walk_start = 0L,
                                landscape = NULL, verbose = TRUE) {
  if (is.null(landscape)) {
    model <- match.arg(model)
    nk <- nk_landscape(N, K, model = model, seed = seed)
  } else {
    stopifnot(inherits(landscape, "nk_landscape"))
    nk <- landscape
  }
  if (is.null(nk$tables)) {
    stop("landscape has no per-locus payoff tables ($tables); ",
         "rebuild it with the current nk_landscape().")
  }
  N <- nk$N
  n_conf <- 2L^N
  codes <- seq_len(n_conf) - 1L

  ## --- Encoding check -------------------------------------------------------
  ## Bit j of a code, b_1 most significant, read with integer bit operations.
  bit_of <- function(j) bitwAnd(bitwShiftR(codes, N - j), 1L)
  bits <- vapply(seq_len(N), bit_of, integer(n_conf))
  if (n_conf == 1L) bits <- matrix(bits, nrow = 1L)
  config_mismatches <- sum(bits != nk$configs)

  recon <- numeric(n_conf)
  for (i in seq_len(N)) {
    di <- nk$dependencies[[i]]
    key <- integer(n_conf)
    for (k in seq_along(di)) {
      key <- bitwOr(key, bitwShiftL(bit_of(di[k]), k - 1L))
    }
    recon <- recon + nk$tables[[i]][key + 1L]
  }
  recon <- recon / N
  enc_diff <- max(abs(recon - nk$fitness))
  enc_pass <- enc_diff <= tolerance && config_mismatches == 0L

  out <- list(encoding_max_difference = enc_diff,
              config_mismatches = config_mismatches,
              n_configs = n_conf,
              model = nk$model,
              passed = enc_pass)

  ## --- Engine check ---------------------------------------------------------
  if (isTRUE(engine)) {
    if (N > 12) stop("engine = TRUE requires N <= 12 (exhaustive evaluation).")
    env <- SaomNkRSienaBiEnv$new(list(
      M = 1L, N = N, BI_PROB = 0.5,
      rand_seed = if (is.null(nk$seed)) 1L else nk$seed,
      name = "_nk_verify_", dir_output = tempdir()))
    E <- unname(nk$influence_matrix)
    env$component_1_coDyadCovar <- E

    ## Engine row order: row r holds the configuration whose bits satisfy
    ## sum(b * 2^(0:(N-1))) == r - 1 (expand.grid, first bit fastest).
    eng_bits <- vapply(seq_len(N), function(j)
      bitwAnd(bitwShiftR(codes, j - 1L), 1L), integer(n_conf))
    if (n_conf == 1L) eng_bits <- matrix(eng_bits, nrow = 1L)
    grid <- as.matrix(expand.grid(rep(list(0:1), N)))
    if (!all(grid == eng_bits)) {
      stop("internal: engine row order does not match expand.grid().")
    }
    nk_code_of_row <- as.integer(eng_bits %*% 2^((N - 1):0))
    is_peak <- as.numeric(nk_code_of_row %in% nk_local_optima(nk)$code)
    arr <- array(0, dim = c(1L, n_conf, 2L * N + 2L))
    arr[1, , seq_len(N)] <- eng_bits
    arr[1, , N + seq_len(N)] <- nk$contributions[nk_code_of_row + 1L, ]
    arr[1, , 2L * N + 1L] <- nk$fitness[nk_code_of_row + 1L]
    arr[1, , 2L * N + 2L] <- is_peak
    env$fitness_landscape <- arr

    only_F <- list(beta_F = 1, beta_s = 0, beta_w = 0, beta_h = 0,
                   beta_cong = 0, beta_disp = 0, beta_comp = 0,
                   beta_clos = 0, beta_riv = 0, beta_leg = 0)
    eng_u <- vapply(codes, function(cc) {
      env$bipartite_matrix[1, ] <- nk$configs[cc + 1L, ]
      do.call(env$compute_formal_utility, c(list(actor_id = 1L), only_F))$nk_fitness
    }, numeric(1))
    eng_diff <- max(abs(eng_u - nk$fitness))

    ## Engine-driven steepest ascent: move to the flip with the largest
    ## positive utility change, stop when none is positive.
    cur <- as.integer(walk_start)
    eng_path <- cur
    for (s in seq_len(n_conf)) {
      env$bipartite_matrix[1, ] <- nk$configs[cur + 1L, ]
      du <- do.call(env$compute_choice_probabilities,
                    c(list(beta = 1), only_F))[[1]]$delta_u
      if (!any(du > 0)) break
      j <- which.max(du)
      cur <- bitwXor(cur, as.integer(2^(N - j)))
      eng_path <- c(eng_path, cur)
    }
    nk_path <- nk_walk(nk, start = as.integer(walk_start), type = "steepest")$path
    walk_same <- identical(as.integer(eng_path), as.integer(nk_path))

    out$engine_max_difference <- eng_diff
    out$engine_walk_identical <- walk_same
    out$engine_walk_steps <- length(eng_path) - 1L
    out$passed <- enc_pass && eng_diff <= tolerance && walk_same
  }

  if (verbose) {
    cat(sprintf("NK landscape check: N=%d, K=%d, model=\"%s\", %s configurations\n",
                N, nk$K, nk$model, format(n_conf, big.mark = ",")))
    cat(sprintf("  encoding: max |fitness - table recomputation| = %.2e, config bit mismatches = %d\n",
                enc_diff, config_mismatches))
    if (isTRUE(engine)) {
      cat(sprintf("  engine  : max |formal utility (beta_F only, M = 1) - NK fitness| = %.2e\n",
                  out$engine_max_difference))
      cat(sprintf("  engine  : steepest walk from code %d, %d steps, path %s nk_walk()\n",
                  as.integer(walk_start), out$engine_walk_steps,
                  if (out$engine_walk_identical) "identical to" else "DIFFERS from"))
    }
    cat(sprintf("  result  : %s\n", if (out$passed) "PASS" else "FAIL"))
  }
  invisible(out)
}
