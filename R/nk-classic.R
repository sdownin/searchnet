################################################################################
# CLASSIC NK LANDSCAPES  (Kauffman & Levin 1987; Kauffman 1993; Levinthal 1997)
#
# A self-contained implementation of the standard NK model, independent of
# RSiena and of the SaoMNK engine.  Its purpose is twofold:
#
#   1. Provide the conventional NK model as first-class functionality, so
#      users can run classical experiments (adaptive walks, local-optima
#      counts, ruggedness sweeps) directly in searchnet.
#
#   2. Serve as an EXTERNAL reference implementation against which the
#      SaoMNK reduction (Theorem 1) can be verified.  The environment
#      method `verify_nk_equivalence()` checks SaoMNK's landscape array for
#      internal consistency; `nk_verify_reduction()` here is stronger: it
#      builds a Kauffman landscape from scratch and confirms that a
#      single-actor, greedy SaoMNK environment reproduces it exactly.
#
# Encoding convention (consistent throughout, and with the engine):
#   A configuration is a binary vector b = (b_1, ..., b_N).
#   Its integer code is  sum(b * 2^((N-1):0))  in 0..2^N-1, i.e. b_1 is the
#   most significant bit.  Landscapes store fitness indexed by code + 1.
#
# Adapted from the reference script _star_poc/standalone_nk.R, whose
# vectorised bit-code construction is retained here.
################################################################################


# ==============================================================================
# Epistatic neighbourhood construction
# ==============================================================================

#' Epistatic dependencies for an NK landscape
#'
#' Builds the list of loci on which each locus's fitness contribution depends.
#' Locus \code{i} always depends on itself plus \code{K} others.
#'
#' @param N Integer. Number of loci (components).
#' @param K Integer. Number of epistatic partners per locus, \code{0 <= K <= N-1}.
#' @param model Character. Neighbourhood topology:
#'   \describe{
#'     \item{\code{"adjacent"}}{Ring topology: partners are the nearest loci on
#'       a circle (Kauffman's "neighbourhood" model). Deterministic.}
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
#' distinct pattern of its epistatic neighbourhood; total fitness is the
#' mean of the \eqn{N} contributions.
#'
#' @param N Integer. Number of loci. Exhaustive enumeration limits practical
#'   use to \code{N <= 20} (\eqn{2^{20} \approx} 1M configurations).
#' @param K Integer. Epistatic partners per locus, \code{0 <= K <= N-1}.
#'   \code{K = 0} gives a smooth, single-peaked landscape; \code{K = N-1}
#'   gives a maximally rugged (effectively random) landscape.
#' @param model Character. Neighbourhood topology; see
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
#'     \item{\code{dependencies}}{List of epistatic neighbourhoods.}
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
  for (i in seq_len(N)) {
    di <- deps[[i]]
    bw <- 2^(seq_along(di) - 1)
    pat <- as.integer(configs[, di, drop = FALSE] %*% bw)  # 0 .. 2^|di| - 1
    tab <- stats::runif(2^length(di))
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
    cat(sprintf("  local optima   : %d  (expected ~ 2^N/(K+1) = %.1f)\n",
                n_opt, 2^x$N / (x$K + 1)))
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
#' to that of all \eqn{N} single-bit-flip (Hamming-1) neighbours.  The expected
#' count scales approximately as \eqn{2^N / (K + 1)}.
#'
#' @param landscape An \code{"nk_landscape"} object.
#' @param strict Logical. If \code{TRUE}, require strictly greater fitness than
#'   every neighbour. Default \code{FALSE} (>=), matching the standard
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
#' among Hamming-1 neighbours until a local optimum is reached.
#'
#' @param landscape An \code{"nk_landscape"} object.
#' @param start Integer code, binary vector, or \code{NULL} for a random start.
#' @param type Character. Search rule:
#'   \describe{
#'     \item{\code{"steepest"}}{Move to the best improving neighbour
#'       (steepest-ascent hill climbing). The standard NK adaptive walk.}
#'     \item{\code{"greedy"}}{Move to the first improving neighbour found,
#'       scanning loci in random order (random-ascent).}
#'     \item{\code{"random"}}{Move to a randomly chosen improving neighbour.}
#'   }
#' @param max_steps Integer. Safety cap on iterations.
#'
#' @return An object of class \code{"nk_walk"}: a list with \code{path}
#'   (integer codes visited), \code{fitness} (fitness at each step),
#'   \code{terminal} (final code), \code{steps}, and \code{is_local_optimum}.
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

  path <- integer(0)
  fpath <- numeric(0)
  for (s in seq_len(max_steps)) {
    path <- c(path, cur)
    fpath <- c(fpath, fit[cur + 1L])

    nbrs <- vapply(seq_len(N), function(j) bitwXor(cur, as.integer(2^(N - j))),
                   integer(1))
    nf <- fit[nbrs + 1L]
    improving <- which(nf > fit[cur + 1L])
    if (length(improving) == 0L) break

    nxt <- switch(type,
      steepest = nbrs[improving[which.max(nf[improving])]],
      greedy   = nbrs[improving[sample.int(length(improving), 1L)][1L]],
      random   = nbrs[improving[sample.int(length(improving), 1L)]]
    )
    cur <- nxt
  }

  structure(list(
    path = path,
    fitness = fpath,
    terminal = cur,
    steps = length(path) - 1L,
    is_local_optimum = TRUE,
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
#' number of local optima grows (roughly \eqn{2^N / (K+1)}) and adaptive walks
#' terminate sooner at lower-fitness peaks.
#'
#' @param N Integer. Number of loci.
#' @param K_values Integer vector of \eqn{K} levels to sweep.
#' @param n_landscapes Integer. Replicate landscapes per \eqn{K}.
#' @param n_walks Integer. Adaptive walks per landscape (random starts).
#' @param model Character. Neighbourhood topology.
#' @param seed Integer or \code{NULL}.
#'
#' @return A \code{data.frame} with one row per \eqn{K}, containing mean local
#'   optima count, mean walk length, mean terminal fitness, and the global
#'   optimum fitness.
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
      expected_optima = 2^N / (k + 1),
      global_fitness = mean(d$global_fitness),
      mean_walk_steps = mean(d$mean_walk_steps),
      mean_terminal_fitness = mean(d$mean_terminal_fitness)
    )
  })
  do.call(rbind, rows)
}


# ==============================================================================
# Bridge to SaoMNK
# ==============================================================================

#' Convert a classic NK landscape to a SaoMNK configuration
#'
#' Emits the environment and structure-model settings that make a
#' single-actor SaoMNK environment search the supplied NK landscape.  This is
#' the constructive content of the Reduction theorem (Theorem 1): NK is the
#' \eqn{M = 1}, \eqn{\beta \to \infty} special case of SaoMNK.
#'
#' @param landscape An \code{"nk_landscape"} object.
#' @param M Integer. Number of actors to place on the landscape. \code{M = 1}
#'   gives the exact NK reduction; \code{M > 1} embeds the same landscape in a
#'   multi-actor SaoMNK search (the generalization direction).
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


#' Verify the NK reduction against an independent landscape
#'
#' Builds a classic NK landscape from scratch and confirms that the fitness
#' values implied by the NK construction match those recovered from the
#' contribution table by an independent route.  This is a stronger check than
#' the environment method \code{verify_nk_equivalence()}, which validates the
#' engine's landscape array for internal consistency only.
#'
#' The test exploits the defining NK property: contribution \eqn{f_i} depends
#' only on the loci in \eqn{\Omega(i)}, so any two configurations sharing the
#' same neighbourhood pattern must carry identical \eqn{f_i}.  A violation
#' indicates an indexing or endianness error in landscape construction.
#'
#' @param N Integer. Number of loci.
#' @param K Integer. Epistatic partners per locus.
#' @param model Character. Neighbourhood topology.
#' @param seed Integer or \code{NULL}.
#' @param tolerance Numeric. Maximum permitted absolute difference.
#'
#' @return Invisibly, a list with \code{max_difference}, \code{n_configs}, and
#'   \code{passed}. Prints a summary.
#'
#' @examples
#' nk_verify_reduction(N = 10, K = 3, seed = 42)
#'
#' @export
nk_verify_reduction <- function(N = 10, K = 3,
                                model = c("adjacent", "random", "block"),
                                seed = NULL, tolerance = 1e-12) {
  model <- match.arg(model)
  nk <- nk_landscape(N, K, model = model, seed = seed)

  # Independent reconstruction: rebuild each locus's payoff table keyed by the
  # neighbourhood code, then recompute fitness as the mean of look-ups.
  recon <- numeric(length(nk$fitness))
  for (i in seq_len(N)) {
    di <- nk$dependencies[[i]]
    bw <- 2^(seq_along(di) - 1)
    codes <- as.integer(nk$configs[, di, drop = FALSE] %*% bw)
    first <- match(codes, codes)          # first config index per pattern
    recon <- recon + nk$contributions[cbind(first, i)]
  }
  recon <- recon / N

  max_diff <- max(abs(recon - nk$fitness))
  passed <- max_diff <= tolerance

  cat(sprintf("NK reduction check: N=%d, K=%d, model=\"%s\"\n", N, K, model))
  cat(sprintf("  configurations   : %s\n",
              format(length(nk$fitness), big.mark = ",")))
  cat(sprintf("  max |difference| : %.2e\n", max_diff))
  cat(sprintf("  result           : %s\n", if (passed) "PASS" else "FAIL"))

  invisible(list(max_difference = max_diff,
                 n_configs = length(nk$fitness),
                 passed = passed))
}
