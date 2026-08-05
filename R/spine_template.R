# Spine template cache (Rust-backed).
#
# Generating the full spine from scratch is expensive at large N:
# 12-35 min at 25m, dominated by per-person Rust work and the
# R->Arrow string conversion in `arrow::write_parquet`. Since the
# spine is deterministic per seed, we pay that cost once at a modest
# template size (default 100k) and sample with replacement on every
# subsequent call.
#
# The entire read/sample/take/write pipeline runs in Rust via
# `generate_spine_from_template_parquet__`, bypassing the R->Arrow
# path completely. At 25m this is expected to take tens of seconds
# rather than tens of minutes.
#
# AEUIDs are generated fresh in Rust per output row, so every output
# has unique per-agency AEUIDs regardless of how many times a
# template row was sampled.

# v3: added core-scope vitals and physical-presence seed columns.
.SPINE_TEMPLATE_VERSION    <- "v3"
.SPINE_TEMPLATE_DEFAULT_N  <- 100000L


#' Resolve the spine template cache path for a given seed
#'
#' Template is keyed on seed + schema version. Different seeds or
#' different versions live in different files, so the cache is safe
#' across changes.
#'
#' @param seed Integer.
#' @param cache_dir Character or NULL. Defaults to
#'   `{get_data_path()}/_spine_cache` or a tempdir if no data path
#'   is configured.
#' @return Character path to the template parquet file.
#' @keywords internal
spine_template_path <- function(seed, cache_dir = NULL) {
  if (is.null(cache_dir)) {
    base <- tryCatch(get_data_path(), error = function(e) NULL)
    if (is.null(base) || !nzchar(base)) {
      base <- file.path(tempdir(), "fplida")
    }
    cache_dir <- file.path(base, "_spine_cache")
  }
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  file.path(cache_dir,
            sprintf("spine-template-seed%d-%s.parquet",
                    as.integer(seed), .SPINE_TEMPLATE_VERSION))
}


#' Build (or rebuild) the spine template for a given seed
#'
#' Generates `n_template` persons in Rust and writes them as a single
#' parquet file at the cache path. Called automatically by
#' `generate_spine()` when the template for the requested seed does
#' not exist. Safe to call explicitly to pre-populate a cache.
#'
#' @param seed Integer. Must match the seed passed to `generate_spine()`.
#' @param n_template Integer. Template size. Default 100000.
#' @param cache_dir Character or NULL.
#' @param force Logical. If TRUE, rebuild even if a template exists.
#' @return Invisibly, the template path.
#' @export
build_spine_template <- function(seed = 42L,
                                 n_template = .SPINE_TEMPLATE_DEFAULT_N,
                                 cache_dir = NULL,
                                 force = FALSE) {
  seed <- as.integer(seed)
  n_template <- as.integer(n_template)
  path <- spine_template_path(seed, cache_dir)

  if (file.exists(path) && !force) {
    return(invisible(path))
  }

  message(sprintf("Building spine template (n=%s, seed=%d)...",
                  format(n_template, big.mark = ","), seed))
  t0 <- proc.time()
  # Process-unique, because the cache directory is shared. Two builds with the
  # same seed would otherwise write the same temp file and corrupt each other.
  tmp_path <- paste0(path, ".tmp-", Sys.getpid())
  build_spine_template_parquet__(n_template, seed, tmp_path)
  tryCatch(
    move_file(tmp_path, path),
    error = function(e) {
      # Another process published an identical template first. That is fine:
      # the template is a pure function of n_template and seed.
      if (file.exists(path)) unlink(tmp_path) else stop(e)
    }
  )
  elapsed <- (proc.time() - t0)[["elapsed"]]
  message(sprintf("Spine template built in %.1fs: %s",
                  elapsed, path))
  invisible(path)
}
