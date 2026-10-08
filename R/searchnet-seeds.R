## ---------------------------------------------------------------------------
## Seed streams and run provenance (internal helpers)
## ---------------------------------------------------------------------------
##
## Seed streams. Several sweeps used to derive their per-run seeds by adding
## offsets to a base seed, e.g. `seed + arm * 10000 + rep * 100 + iters` for
## the initial draw and `seed + arm * 20000 + rep * 100 + iters` for the
## dynamics. Additive offsets collide: arm 2's initial-draw seed equals arm
## 1's dynamics seed, so two streams that the design treats as independent
## were the same stream. Other sites fed one seed to both the landscape/initial
## draw and the simulation run.
##
## `.searchnet_seed()` replaces those offsets with a hash of (base, purpose,
## indices). The purpose string namespaces the stream ("init", "run", ...), so
## two purposes never share a seed by construction of the inputs, and the
## 32-bit avalanche mix spreads neighboring indices across the whole range.
## It is pure base-R arithmetic on doubles (every intermediate stays below
## 2^53, so it is exact), deterministic across platforms, and needs no
## package.

## 32-bit unsigned helpers on doubles in [0, 2^32).
.sn_u32  <- function(x) x %% 4294967296
.sn_xor32 <- function(a, b) {
  hi <- bitwXor(as.integer(a %/% 65536), as.integer(b %/% 65536))
  lo <- bitwXor(as.integer(a %% 65536),  as.integer(b %% 65536))
  hi * 65536 + lo
}
.sn_mul32 <- function(a, b) {
  a_lo <- a %% 65536; a_hi <- a %/% 65536
  b_lo <- b %% 65536; b_hi <- b %/% 65536
  .sn_u32(a_lo * b_lo + ((a_hi * b_lo + a_lo * b_hi) %% 65536) * 65536)
}
## MurmurHash3 finalizer: a bijection on 32-bit words with full avalanche.
.sn_fmix32 <- function(h) {
  h <- .sn_xor32(h, h %/% 65536)
  h <- .sn_mul32(h, 2246822507)   # 0x85ebca6b
  h <- .sn_xor32(h, h %/% 8192)
  h <- .sn_mul32(h, 3266489909)   # 0xc2b2ae35
  .sn_xor32(h, h %/% 65536)
}

#' Purpose-namespaced seed derivation (internal)
#'
#' Derives a seed for one random stream from a base seed, a purpose label and
#' any number of integer indices. Different purposes, or different indices
#' under the same purpose, give unrelated seeds; the same inputs always give
#' the same seed.
#'
#' @param base Numeric scalar. The user-facing base seed.
#' @param purpose Character scalar naming the stream, e.g.
#'   \code{"ergodicity:init"}.
#' @param ... Integer-valued scalars (arm, replicate, step, ...).
#' @return An integer in \code{[1, .Machine$integer.max]}.
#' @keywords internal
#' @noRd
.searchnet_seed <- function(base, purpose, ...) {
  if (!is.numeric(base) || length(base) != 1L || !is.finite(base))
    stop("`base` seed must be a single finite number.", call. = FALSE)
  if (!is.character(purpose) || length(purpose) != 1L || !nzchar(purpose))
    stop("`purpose` must be a non-empty character scalar.", call. = FALSE)
  idx <- unlist(list(...), use.names = FALSE)
  if (length(idx) && (!is.numeric(idx) || any(!is.finite(idx))))
    stop("seed indices must be finite numbers.", call. = FALSE)

  h <- 2538058380                       # 0x9747b28c, arbitrary nonzero start
  absorb <- function(h, t) .sn_fmix32(.sn_u32(.sn_xor32(h, .sn_u32(t)) + 2654435769))
  bytes <- utf8ToInt(enc2utf8(purpose))
  for (b in bytes) h <- absorb(h, b)
  h <- absorb(h, length(bytes))         # terminate the label
  h <- absorb(h, round(base))
  h <- absorb(h, floor(round(base) / 4294967296))  # high word for large seeds
  for (t in idx) h <- absorb(h, round(t))
  h <- absorb(h, length(idx))           # (1, 2) and (1, 2, 0) differ
  as.integer(h %% 2147483647) + 1L
}


## ---------------------------------------------------------------------------
## Run provenance
## ---------------------------------------------------------------------------

## The searchnet version that is actually running. When the code was sourced
## from files (the test harness, inst/saomnk-loader.R) rather than loaded from
## an installed namespace, the installed version may be stale, so say so.
.searchnet_running_version <- function() {
  in_ns <- identical(environmentName(topenv(environment(.searchnet_running_version))),
                     "searchnet")
  inst <- tryCatch(as.character(utils::packageVersion("searchnet")),
                   error = function(e) NA_character_)
  if (in_ns) return(inst)
  if (is.na(inst)) "sourced (not installed)"
  else sprintf("sourced (installed version %s)", inst)
}

#' Build a provenance record for a simulation (internal)
#'
#' @param seed The seed the run used (after any defaulting).
#' @param call The matched call, or NULL.
#' @param ... Further named fields to record.
#' @return A list of class \code{"searchnet_provenance"}.
#' @keywords internal
#' @noRd
.searchnet_provenance <- function(seed = NULL, call = NULL, ...) {
  rsiena_v <- tryCatch(as.character(utils::packageVersion("RSiena")),
                       error = function(e) NA_character_)
  out <- list(
    searchnet_version = .searchnet_running_version(),
    RSiena_version    = rsiena_v,
    R_version         = R.version.string,
    rng_kind          = RNGkind(),
    seed              = seed,
    call              = call,
    call_text         = if (is.null(call)) NA_character_
                        else paste(deparse(call, width.cutoff = 500L), collapse = " "),
    timestamp         = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
  extra <- list(...)
  if (length(extra)) out[names(extra)] <- extra
  class(out) <- c("searchnet_provenance", "list")
  out
}

#' @export
print.searchnet_provenance <- function(x, ...) {
  cat("searchnet run provenance\n")
  cat(sprintf("  searchnet : %s\n", x$searchnet_version))
  cat(sprintf("  RSiena    : %s\n", x$RSiena_version))
  cat(sprintf("  R         : %s\n", x$R_version))
  cat(sprintf("  RNG kind  : %s\n", paste(x$rng_kind, collapse = " / ")))
  cat(sprintf("  seed      : %s\n",
              if (is.null(x$seed)) "NULL" else paste(x$seed, collapse = ", ")))
  if (!is.na(x$call_text)) cat(sprintf("  call      : %s\n", x$call_text))
  cat(sprintf("  recorded  : %s\n", x$timestamp))
  invisible(x)
}

#' Run provenance of a searchnet simulation
#'
#' Returns the provenance record that \code{\link{saomnk_run}},
#' \code{\link{saomnk_monte_carlo}} and the sweep functions store with their
#' results: the searchnet, RSiena and R versions, the RNG kind, the seed the
#' run actually used, and the call. Reproducing a stochastic result needs all
#' of these, and a seed on its own is not enough when the RNG kind or an
#' engine version differs.
#'
#' @param x A \code{SaomNkRSienaBiEnv} after a run, or a result object
#'   (data frame or list) returned by a sweep function.
#' @return A \code{searchnet_provenance} list, or \code{NULL} if \code{x}
#'   carries none (for example, an environment whose \code{search_rsiena()}
#'   method was called directly rather than through \code{saomnk_run()}).
#' @examples
#' \donttest{
#' env <- saomnk_env(M = 4, N = 6, seed = 42)
#' saomnk_run(env, saomnk_model(density = -0.5), steps_per_actor = 3, seed = 7)
#' searchnet_provenance(env)
#' }
#' @export
searchnet_provenance <- function(x) {
  if (inherits(x, "R6") && "provenance" %in% names(x)) return(x$provenance)
  attr(x, "provenance", exact = TRUE)
}
