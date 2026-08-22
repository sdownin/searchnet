#' @title SaoMNK Utility Functions
#' @description Standalone helper functions used throughout the SaoMNK package.
#' @name saomnk-utils
NULL


#' Compute Jaccard Index Between Two Binary Matrices
#'
#' Measures the similarity between two binary matrices as the ratio of
#' maintained ties to total ties (maintained + changed).
#'
#' @param m0 A binary matrix (the baseline / earlier state).
#' @param m1 A binary matrix of the same dimensions (the comparison / later state).
#' @return A numeric scalar in \eqn{[0, 1]}. Returns 1 when the matrices are
#'   identical and 0 when they share no ties in common.
#' @export
get_jaccard_index <- function(m0, m1) {
  diffvec      <- c(m1 - m0)
  cnt_maintain <- sum(m1 * m0)
  cnt_change   <- sum(diffvec != 0)
  cnt_maintain / (cnt_maintain + cnt_change)
}


#' Toggle a Tie in a One-Mode Network Matrix
#'
#' Flips a tie from 0 to 1 or 1 to 0 at position \code{(i, j)} in a
#' square one-mode adjacency matrix.
#' Self-ties (\code{i == j}) are silently ignored.
#'
#' @param m A square numeric matrix (one-mode adjacency matrix).
#' @param i Row index of the dyad.
#' @param j Column index of the dyad.
#' @return The modified matrix with the \code{(i, j)} entry toggled,
#'   or the original matrix if \code{i == j}.
#' @export
saomnk_toggle <- function(m, i, j) {
  if (i != j) {
    m[i, j] <- 1 - m[i, j]
  }
  m
}


#' Toggle a Tie in a Bipartite Network Matrix
#'
#' Flips a tie from 0 to 1 or 1 to 0 at position \code{(i, j)} in a
#' rectangular bipartite (two-mode) adjacency matrix.
#' Indices that fall outside the matrix dimensions are silently ignored.
#'
#' @param m A numeric matrix (rows = actors, columns = components).
#' @param i Row index (actor).
#' @param j Column index (component).
#' @return The modified matrix with the \code{(i, j)} entry toggled,
#'   or the original matrix if the indices are out of bounds.
#' @export
saomnk_toggle_bipartite <- function(m, i, j) {
  if (i >= 1 && i <= dim(m)[1] && j >= 1 && j <= dim(m)[2]) {
    m[i, j] <- 1 - m[i, j]
  }
  m
}


#' Check That a Value Exists (Non-NULL, Non-NA, Non-NaN)
#'
#' A safe existence check that avoids masking \code{\link[base]{exists}}.
#' Returns \code{TRUE} when the value is not \code{NULL}, not \code{NA},
#' and not \code{NaN}.
#'
#' @param x Any R object.
#' @return Logical scalar.
#' @export
saomnk_exists <- function(x) {
  !is.null(x) && !is.na(x) && !is.nan(x)
}


#' Create a Block-Diagonal Influence Matrix
#'
#' Builds a symmetric block-diagonal binary matrix of dimension
#' \eqn{N \times N} with \code{B} approximately equal-sized blocks.
#' Within each block every pair of components interacts (entry = 1);
#' across blocks there is no interaction (entry = 0).
#' This is used to define modular component interaction structures
#' for the NK fitness landscape.
#'
#' @param N Integer. Total number of components (matrix dimension).
#' @param B Integer. Number of blocks (modules).
#' @return A numeric \eqn{N \times N} matrix with 1s on the block
#'   diagonals and 0s elsewhere.
#' @examples
#' # 12 components split into 3 modules of size 4
#' create_block_diag(12, 3)
#' @export
create_block_diag <- function(N, B) {
  block_sizes <- rep(N %/% B, B)
  remainder <- N %% B
  if (remainder > 0) {
    block_sizes[1:remainder] <- block_sizes[1:remainder] + 1
  }
  blocks <- lapply(block_sizes, function(s) matrix(1, nrow = s, ncol = s))
  as.matrix(Matrix::bdiag(blocks))[1:N, 1:N]
}


#' Estimate an Influence Matrix from Observed Bipartite Data
#'
#' Given a bipartite matrix B (actors x components), computes the component
#' co-occurrence matrix as an estimate of the influence matrix. Components that
#' frequently co-occur in the same actors' portfolios are inferred to influence
#' one another more strongly.
#'
#' Three methods available:
#' \itemize{
#'   \item \code{"jaccard"}: Jaccard similarity between component adoption vectors
#'   \item \code{"cosine"}: Cosine similarity between columns of B
#'   \item \code{"cooccurrence"}: Raw co-occurrence count normalized by max
#' }
#'
#' @param B A binary bipartite matrix (M x N) or a SaomNkRSienaBiEnv object
#' @param method One of \code{"jaccard"}, \code{"cosine"}, \code{"cooccurrence"}
#' @param threshold Minimum similarity to retain (set to 0 below threshold)
#' @param diagonal Value for diagonal entries (default 0, matching block_diag convention)
#' @return An N x N symmetric influence-matrix estimate built from realized
#'   co-holding, suitable as the \code{influence_matrix} argument of
#'   \code{\link{saomnk_model}}
#' @export
#' @examples
#' # From a simulated environment
#' env <- saomnk_env(M = 10, N = 8, density = 0.4, seed = 42)
#' W <- saomnk_empirical_influence(env$bipartite_matrix)
#'
#' # Use in a model
#' model <- saomnk_model(density = -0.5, influence_matrix = W, influence_weight = 0.3)
saomnk_empirical_influence <- function(B, method = "jaccard", threshold = 0, diagonal = 0) {
  # If B is an env object, extract the matrix
  if (is.environment(B) || inherits(B, "R6")) {
    B <- B$bipartite_matrix
  }

  stopifnot(is.matrix(B), all(B %in% c(0, 1)))
  N <- ncol(B)

  W <- matrix(0, N, N)

  if (method == "jaccard") {
    for (j in 1:(N - 1)) {
      for (k in (j + 1):N) {
        both   <- sum(B[, j] & B[, k])
        either <- sum(B[, j] | B[, k])
        sim <- if (either > 0) both / either else 0
        W[j, k] <- W[k, j] <- sim
      }
    }
  } else if (method == "cosine") {
    norms <- sqrt(colSums(B^2))
    for (j in 1:(N - 1)) {
      for (k in (j + 1):N) {
        sim <- if (norms[j] > 0 && norms[k] > 0) {
          sum(B[, j] * B[, k]) / (norms[j] * norms[k])
        } else 0
        W[j, k] <- W[k, j] <- sim
      }
    }
  } else if (method == "cooccurrence") {
    co <- t(B) %*% B
    diag(co) <- 0
    mx <- max(co)
    W <- if (mx > 0) co / mx else co
  } else {
    stop("method must be 'jaccard', 'cosine', or 'cooccurrence'")
  }

  # Apply threshold
  W[W < threshold] <- 0

  # Set diagonal
  diag(W) <- diagonal

  W
}


## Once-per-session deprecation flags (internal). The alias below warns the
## first time it is called in a session, not on every call, so an old script
## that loops over it is not buried in repeats.
.searchnet_deprecation_flags <- new.env(parent = emptyenv())
.searchnet_reset_deprecations <- function() {
  rm(list = ls(.searchnet_deprecation_flags), envir = .searchnet_deprecation_flags)
  invisible(TRUE)
}

#' @rdname saomnk_empirical_influence
#' @section Deprecated alias:
#' \code{saomnk_empirical_epistasis()} is the pre-0.8.2 name and is kept as a
#' deprecated alias: same arguments, same return value, plus a one-time
#' deprecation warning per session. The function returns an influence-matrix
#' estimate, not epistasis, which is why the name changed.
#' @export
saomnk_empirical_epistasis <- function(B, method = "jaccard", threshold = 0, diagonal = 0) {
  if (!isTRUE(.searchnet_deprecation_flags$empirical_epistasis)) {
    .Deprecated("saomnk_empirical_influence", package = "searchnet",
                msg = paste0("saomnk_empirical_epistasis() is deprecated as of searchnet ",
                             "0.8.2; use saomnk_empirical_influence(). It returns an ",
                             "influence-matrix estimate, not epistasis."))
    .searchnet_deprecation_flags$empirical_epistasis <- TRUE
  }
  saomnk_empirical_influence(B, method = method, threshold = threshold, diagonal = diagonal)
}

#' Get path to searchnet proof files
#'
#' Returns the file path(s) to the formal proof documents shipped with the
#' package in \code{inst/proofs/}.
#'
#' @param file Name of the proof file (default: lists all available).
#' @return Character vector of file path(s).
#' @export
searchnet_proof <- function(file = NULL) {
  proof_dir <- system.file("proofs", package = "searchnet")
  if (proof_dir == "") {
    proof_dir <- file.path(.saomnk_dir, "..", "inst", "proofs")
  }
  if (is.null(file)) {
    return(list.files(proof_dir, full.names = TRUE))
  }
  file.path(proof_dir, file)
}
