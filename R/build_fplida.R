# Slice-based build orchestrator.
#
# Flow:
#   1. Generate canonical spine (single-process, full N).
#   2. Generate central products that need the full population.
#   3. Materialise K slice spines into ephemeral slice run_dirs.
#   4. Spawn K PSOCK workers; each runs build_fplida_slice_worker().
#   5. Merge slice outputs into canonical run_dir as part files.
#   6. Clean up slice run_dirs.
#   7. If export_format = "csv", convert the canonical run_dir to CSV
#      via DuckDB (see convert_parquet_dir_to_csv()).
#
# Reuses existing per-product generators unchanged.


#' Build a complete fplida dataset using parallel slice workers
#'
#' Splits the spine into \code{k_slices} disjoint person ranges and
#' runs the full product pipeline in parallel across fresh R worker
#' processes. For any non-trivial N this is substantially faster than
#' a sequential build because it breaks the per-generator chunk-level
#' serialisation and reduces per-process allocator pressure.
#'
#' The canonical output run directory has one directory per product:
#' per-product outputs are stored as part files
#' (\code{part-NNN.parquet}), naturally readable via
#' \code{arrow::open_dataset()}.
#'
#' Cross-person- or household-dependent generators (\code{spine},
#' \code{core}, \code{blade}, \code{lfs}) run once centrally on the full
#' population before workers start. All other products run per slice.
#'
#' CSV output: when \code{export_format = "csv"}, the build runs to
#' parquet internally (all Rust fast paths are parquet-only), then the
#' canonical run directory is converted to CSV via DuckDB. The CSV run
#' directory is written to \code{<run_dir>_csv/}. STP is preserved as
#' parquet rather than converted to CSV. With \code{messy_files = TRUE},
#' preserved STP product directories are written under
#' \code{ato-stp/stp-standard/} or \code{ato-stp/stp-extended/} in the CSV
#' run directory. With \code{keep_parquet = FALSE} the source parquet run
#' directory is deleted after conversion.
#'
#' @param n Integer. Total number of persons (default 1,000,000).
#' @param seed Integer. Base random seed.
#' @param years Integer vector. Panel years for time-varying datasets.
#' @param k_slices Integer. Number of parallel slice workers. Defaults
#'   to \code{detectCores()} at \code{n < 15M} and \code{cores/2} at
#'   larger N (memory headroom per worker).
#' @param rayon_threads Integer or NULL. Rayon threads per worker.
#'   NULL auto-computes from \code{cores / k_slices}.
#' @param products Character. Either \code{"all"} or a character vector
#'   of product names.
#' @param exclude_products Character or NULL. Products to exclude when
#'   \code{products = "all"}. Cannot include \code{"spine"}.
#' @param export_format \code{"parquet"} (default) or \code{"csv"}.
#' @param output_dir Character or NULL. Base output directory.
#' @param suffix Character or NULL. Optional canonical run_dir suffix.
#' @param slice_parent_dir Character or NULL. Parent directory for the
#'   ephemeral slice run_dirs. NULL uses \code{tempdir()}.
#' @param keep_slice_dirs Logical. If TRUE, slice run_dirs are not
#'   removed after merge (useful for debugging). Default FALSE.
#' @param keep_parquet Logical. When \code{export_format = "csv"},
#'   should the source parquet files be kept alongside the converted
#'   CSVs? Defaults to \code{TRUE}. If \code{FALSE}, the parquet run
#'   directory is deleted after a successful CSV conversion. Ignored
#'   when \code{export_format = "parquet"}.
#' @param export_base_file Logical. If TRUE, include the internal
#'   base-spine file in the final output. A Parquet build retains
#'   \code{_system/base-spine.parquet}. A CSV build also exports
#'   \code{base-spine-v6/base-spine-v6.csv}. The build always creates the
#'   Parquet file for internal processing. The default is FALSE.
#' @param complete_dil_schema Logical. If TRUE, write one canonical table for
#'   every product-table structure in the bundled PLIDA Data Item List for the
#'   selected build targets. The default is TRUE for \code{products = "all"}
#'   and FALSE for a partial build. Survey structures are included, but their
#'   unvalidated values remain typed missing where a bespoke generator does
#'   not supply them.
#' @param complete_dil_rows Positive integer. Maximum rows in each canonical
#'   DIL schema companion. The default is 100. Richer bespoke product outputs
#'   retain their normal row counts.
#' @param messy_files Logical. When \code{export_format = "csv"}, write
#'   each PLIDA data product as a top-level folder, keep BLADE product folders
#'   under \code{abs-blade/}, keep STP parquet product folders under
#'   \code{ato-stp/stp-standard/} or \code{ato-stp/stp-extended/}, omit other
#'   agency grouping folders, and write agency spines as top-level
#'   \code{<agency>-spine-v6/} folders. Defaults to TRUE.
#' @param messy_names Logical. When \code{export_format = "csv"}, vary a
#'   small subset of variables across related PIT/PAYG, MBS, and PBS
#'   year products. Defaults to TRUE.
#'
#' @return Invisibly, a list with build metadata and per-slice stats.
#'
#' @examples
#' \dontrun{
#' res <- build_fplida(n = 1e6)
#' res <- build_fplida(n = 25e6, k_slices = 5, rayon_threads = 2)
#' res <- build_fplida(n = 1e6, export_format = "csv")
#' }
#'
#' @export
build_fplida <- function(n = 1000000L,
                         seed = 42L,
                         years = 2015:2025,
                         k_slices = NULL,
                         rayon_threads = NULL,
                         products = "all",
                         exclude_products = NULL,
                         export_format = c("parquet", "csv"),
                         output_dir = NULL,
                         suffix = NULL,
                         slice_parent_dir = NULL,
                         keep_slice_dirs = FALSE,
                         keep_parquet = TRUE,
                         export_base_file = FALSE,
                         complete_dil_schema = identical(products, "all") &&
                           is.null(exclude_products),
                         complete_dil_rows = 100L,
                         messy_files = TRUE,
                         messy_names = TRUE) {

  # ---- Validate inputs ------------------------------------------------
  n <- as.integer(n)
  seed <- as.integer(seed)
  years <- as.integer(years)
  export_format <- match.arg(export_format)
  messy_files <- isTRUE(messy_files)
  messy_names <- isTRUE(messy_names)
  export_base_file <- isTRUE(export_base_file)
  complete_dil_schema <- isTRUE(complete_dil_schema)
  complete_dil_rows <- as.integer(complete_dil_rows)
  stopifnot(n > 0L, !is.na(seed), length(years) > 0L,
            length(complete_dil_rows) == 1L, !is.na(complete_dil_rows),
            complete_dil_rows > 0L)

  # Internal build format: all Rust fast paths are parquet-only. When
  # the caller asks for CSV, we build to parquet then convert at the
  # end via DuckDB. See convert_parquet_dir_to_csv().
  want_csv      <- export_format == "csv"
  build_format  <- "parquet"
  if (want_csv) {
    if (!requireNamespace("duckdb", quietly = TRUE) ||
        !requireNamespace("DBI", quietly = TRUE)) {
      stop("CSV export requires the `duckdb` and `DBI` packages. ",
           "Install with: install.packages(c('duckdb', 'DBI'))",
           call. = FALSE)
    }
  }

  cores <- tryCatch(parallel::detectCores(logical = FALSE),
                    error = function(e) 1L)
  if (is.na(cores) || cores < 1L) cores <- 1L

  # K / thread-per-worker policy by scale.
  #
  # - At ≤10M, K = detectCores() × T=1 maximises process-level
  #   parallelism and runs comfortably in RAM.
  # - At >10M, 10 concurrent workers each touching 2.5M+ persons blows
  #   the 32GB memory budget: every generator's per-slice working set
  #   (spine columns, MBS/PBS chunk buffers, arrow write staging) adds
  #   up across workers. Drop K to ~cores/2 and give each worker 2
  #   Rayon threads to preserve total core utilisation. Fewer workers
  #   = more memory headroom per worker, avoids swap thrash.
  if (is.null(k_slices)) {
    if (n >= 15000000L) {
      k_slices <- max(2L, cores %/% 2L)
    } else {
      k_slices <- max(1L, cores)
    }
  }
  k_slices <- as.integer(k_slices)
  stopifnot(k_slices >= 1L, k_slices <= n)

  if (is.null(rayon_threads)) {
    # Saturate cores: if K < cores, give each worker cores/K threads.
    # If K >= cores, use 1 thread per worker.
    rayon_threads <- if (k_slices >= cores) 1L
                     else rayon_threads_per_worker(k_slices, cores)
  }
  rayon_threads <- as.integer(rayon_threads)

  # ---- Resolve product set (reuse build_fplida logic) ----------------
  all_products <- c("spine", "census", "pit_ps", "pit_itr", "core", "blade",
                    "he", "domino", "mbs", "pbs", "tva",
                    "combined", "births", "deaths", "mcd", "ato_cr",
                    "visa", "mt_demogs", "sdb", "travellers",
                    "pit_ie", "busown", "sae", "cgt", "rps", "stp",
                    "ndis", "apprentice", "dex", "air", "amep", "nacdc",
                    "aedc", "acld", "sdac", "ers", "jk", "jm", "nhs",
                    "nsmhw", "pex", "apsed", "ato_mcs", "lfs", "smsf")

  if (identical(products, "all")) {
    to_build <- all_products
    if (!is.null(exclude_products)) {
      exclude_products <- tolower(exclude_products)
      if ("spine" %in% exclude_products) {
        stop("Cannot exclude 'spine'.", call. = FALSE)
      }
      if (!"blade" %in% exclude_products && "core" %in% exclude_products) {
        stop("Cannot exclude 'core' when building 'blade'. ",
             "Exclude 'blade' too or keep 'core'.", call. = FALSE)
      }
      bad <- setdiff(exclude_products, all_products)
      if (length(bad) > 0L) {
        stop("Unknown exclude_products: ", paste(bad, collapse = ", "),
             call. = FALSE)
      }
      to_build <- setdiff(to_build, exclude_products)
    }
  } else {
    products <- tolower(products)
    bad <- setdiff(products, all_products)
    if (length(bad) > 0L) {
      stop("Unknown products: ", paste(bad, collapse = ", "),
           call. = FALSE)
    }
    to_build <- union("spine", products)
    if ("pit_itr" %in% to_build && !"pit_ps" %in% to_build) {
      to_build <- union(to_build, "pit_ps")
    }
    if ("blade" %in% to_build && !"core" %in% to_build) {
      to_build <- union(to_build, "core")
    }
  }

  # Keep the canonical order
  build_order <- intersect(all_products, to_build)
  worker_products <- setdiff(build_order, c("spine", "core", "blade", "lfs"))

  message("\n=== Building fplida dataset ===")
  message("  N: ", format(n, big.mark = ","))
  message("  Seed: ", seed)
  message("  Years: ", min(years), "-", max(years))
  message("  K slices: ", k_slices)
  message("  Rayon threads per worker: ", rayon_threads)
  message("  Products: ", paste(build_order, collapse = ", "))
  message("  Format: ", export_format)
  message("  Complete DIL schema: ", complete_dil_schema)
  if (complete_dil_schema) {
    message("  Canonical DIL rows per table: up to ", complete_dil_rows)
  }
  message("")

  total_start <- proc.time()
  stage_timings <- list()
  # ---- Stage 1: Canonical spine --------------------------------------
  message("--- STAGE 1: SPINE (central) ---")
  t0 <- proc.time()
  generate_spine(n = n, seed = seed, output_dir = output_dir,
                 format = build_format, suffix = suffix,
                 return_data = FALSE)
  canonical_run_dir <- getOption("fplida.run_dir")
  if (is.null(canonical_run_dir) || !dir.exists(canonical_run_dir)) {
    stop("Canonical run_dir not set after generate_spine().",
         call. = FALSE)
  }
  canonical_spine_path <- file.path(canonical_run_dir, "_system",
                                    "base-spine.parquet")
  if (!file.exists(canonical_spine_path)) {
    stop("Canonical spine file missing at ", canonical_spine_path,
         call. = FALSE)
  }
  remove_internal_base_file <- function() {
    if (!export_base_file && file.exists(canonical_spine_path)) {
      unlink(canonical_spine_path)
    }
  }
  stage_timings$spine <- (proc.time() - t0)[["elapsed"]]
  message(sprintf("  SPINE done in %.1fs", stage_timings$spine))

  # ---- Stage 2: CORE (central, cross-person) -------------------------
  if ("core" %in% build_order) {
    message("\n--- STAGE 2: CORE (central, cross-person) ---")
    t0 <- proc.time()
    generate_core(seed = seed, output_dir = output_dir, years = years,
                  format = build_format, return_data = FALSE)
    stage_timings$core <- (proc.time() - t0)[["elapsed"]]
    message(sprintf("  CORE done in %.1fs", stage_timings$core))
  }

  # ---- Stage 2b: BLADE business source of truth + DIL tables ---------
  if ("blade" %in% build_order) {
    message("\n--- STAGE 2b: BLADE (central, business-level) ---")
    t0 <- proc.time()
    generate_blade(seed = seed, output_dir = output_dir,
                   format = build_format, return_data = FALSE)
    stage_timings$blade <- (proc.time() - t0)[["elapsed"]]
    message(sprintf("  BLADE done in %.1fs", stage_timings$blade))
  }

  # ---- Stage 2c: LLFS household panel -----------------------------------
  if ("lfs" %in% build_order) {
    message("\n--- STAGE 2c: LFS (central, household panel) ---")
    t0 <- proc.time()
    generate_lfs(seed = seed, output_dir = output_dir,
                 format = build_format, return_data = FALSE)
    stage_timings$lfs <- (proc.time() - t0)[["elapsed"]]
    message(sprintf("  LFS done in %.1fs", stage_timings$lfs))
  }

  # ---- Stage 3: Materialise slice spines -----------------------------
  if (length(worker_products) > 0L) {
    message("\n--- STAGE 3: slice spines (materialising) ---")
    t0 <- proc.time()
    ranges <- compute_slice_ranges(n, k_slices)
    if (is.null(slice_parent_dir)) {
      slice_parent_dir <- file.path(tempdir(), "fplida_slices")
    }
    if (!dir.exists(slice_parent_dir)) {
      dir.create(slice_parent_dir, recursive = TRUE)
    }

    slice_run_dirs <- materialise_all_slice_spines(
      canonical_spine_path = canonical_spine_path,
      ranges = ranges,
      parent_dir = slice_parent_dir,
      n_total = n,
      seed = seed
    )
    stage_timings$slice_spines <- (proc.time() - t0)[["elapsed"]]
    message(sprintf("  %d slice spines materialised in %.1fs",
                    k_slices, stage_timings$slice_spines))
  } else {
    message("\n--- STAGE 3: slice spines (skipped) ---")
    message("  no per-slice products requested")
    slice_run_dirs <- character(0)
    stage_timings$slice_spines <- 0
  }

  # ---- Stage 4: Launch workers in parallel ---------------------------
  message("\n--- STAGE 4: parallel slice workers ---")
  t0 <- proc.time()
  if (length(worker_products) == 0L) {
    message("  no per-slice products requested; skipping workers")
    stage_timings$workers <- 0
    stage_timings$merge <- 0
    if (!keep_slice_dirs) cleanup_slice_dirs(slice_run_dirs)
    dil_schema_result <- NULL
    if (complete_dil_schema) {
      message("\n--- STAGE 5: PLIDA DIL product-table structures ---")
      dil_t0 <- proc.time()
      dil_schema_result <- .complete_plida_dil_structures(
        run_dir = canonical_run_dir,
        build_order = build_order,
        seed = seed,
        max_rows = complete_dil_rows
      )
      stage_timings$dil_schema <- (proc.time() - dil_t0)[["elapsed"]]
    }
    remove_internal_base_file()
    total_elapsed <- (proc.time() - total_start)[["elapsed"]]
    message(sprintf("\n=== Build done in %.1fs ===", total_elapsed))

    # CSV conversion for early-return path (spine/core only).
    csv_result <- NULL
    result_run_dir <- canonical_run_dir
    if (want_csv) {
      csv_dir <- paste0(canonical_run_dir, "_csv")
      csv_result <- convert_parquet_dir_to_csv(
        src_dir = canonical_run_dir,
        dst_dir = csv_dir,
        verbose = FALSE,
        preserve_parquet_datasets = "ato-stp",
        messy_files = messy_files,
        messy_names = messy_names
      )
      if (!isTRUE(keep_parquet)) {
        unlink(canonical_run_dir, recursive = TRUE)
        options(fplida.run_dir = csv_dir)
        result_run_dir <- csv_dir
        if (!is.null(dil_schema_result)) {
          dil_schema_result$files <- character()
          dil_schema_result$parquet_files_removed <- TRUE
        }
      }
    }

    return(invisible(list(
      n                     = n,
      seed                  = seed,
      k_slices              = k_slices,
      rayon_threads         = rayon_threads,
      years                 = years,
      products              = build_order,
      format                = export_format,
      build_format          = build_format,
      messy_files           = messy_files,
      messy_names           = messy_names,
      export_base_file      = export_base_file,
      complete_dil_schema   = complete_dil_schema,
      complete_dil_rows     = complete_dil_rows,
      canonical_run_dir     = result_run_dir,
      total_elapsed_seconds = total_elapsed,
      stage_timings         = stage_timings,
      merge_stats           = list(),
      dil_schema            = dil_schema_result,
      worker_results        = list(),
      csv_conversion        = csv_result
    )))
  }

  # Build worker invocation arguments
  # Pick MBS/PBS chunk_size so that K × per-chunk peak memory and
  # per-chunk string buffer stays inside two constraints:
  #   (a) total RAM: K × chunk_peak_bytes <= ~60% of machine RAM
  #   (b) Arrow i32-offset StringArray: per-chunk per-column string
  #       bytes must stay well under 2^31 = 2.1 GB (Arrow byte array
  #       offset hard limit). Exceeding this panics with
  #       "offset overflow" in arrow-array byte_array.rs and takes
  #       the whole R worker process down.
  #
  # At 25M K=5 the per-worker slice is 5M persons. An MBS/PBS chunk
  # of 400k persons × ~150 claims/person × ~30 bytes for the biggest
  # string column × 10 years ≈ 1.8 GB — already close to the 2 GB
  # limit at a single point in time. 150k stays safely under.
  #
  # Do NOT raise the floor based on K alone. Earlier versions had a
  # "K<=5 means more RAM per worker, use larger chunks" override,
  # but that reasoning is wrong at very large N: each K=5 worker
  # carries a 5× bigger slice, so the per-chunk string buffer
  # constraint is the dominant one, not the total RAM budget.
  per_worker_n <- n %/% k_slices
  mbs_pbs_chunk <- if (per_worker_n >= 2000000L) {
    150000L
  } else if (per_worker_n >= 1000000L) {
    200000L
  } else {
    300000L
  }

  message(sprintf("  per-worker N: %s, MBS/PBS chunk_size: %s",
                  format(per_worker_n, big.mark = ","),
                  format(mbs_pbs_chunk, big.mark = ",")))

  worker_args <- lapply(seq_len(k_slices), function(i) {
    list(
      slice_run_dir = slice_run_dirs[i],
      slice_id      = i - 1L,
      slice_seed    = as.integer(seed + (i - 1L) * 100000L),
      years         = years,
      products      = worker_products,
      export_format = build_format,
      mbs_pbs_chunk = mbs_pbs_chunk
    )
  })

  # Spawn a PSOCK cluster of K workers. PSOCK spawns fresh R processes
  # (safe with extendr / Rust native code) and supports parLapply.
  cl <- parallel::makePSOCKcluster(k_slices)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  # On each worker: set RAYON_NUM_THREADS before loading fplida, then
  # load the package.
  parallel::clusterExport(cl, varlist = c("rayon_threads"),
                          envir = environment())
  parallel::clusterEvalQ(cl, {
    Sys.setenv(RAYON_NUM_THREADS = as.character(rayon_threads))
    suppressPackageStartupMessages(library(fplida))
    NULL
  })

  worker_results <- parallel::parLapply(cl, worker_args, function(args) {
    do.call(fplida::build_fplida_slice_worker, args)
  })

  parallel::stopCluster(cl)
  on.exit()  # clear the cluster cleanup since we've stopped it

  worker_errors <- unlist(lapply(worker_results, function(worker) {
    failures <- vapply(worker$product_results, function(product_result) {
      err <- product_result$metadata$error
      if (is.null(err)) "" else err
    }, character(1))
    failures <- failures[nzchar(failures)]
    if (!length(failures)) return(character(0))
    paste0("slice ", worker$slice_id, " / ", names(failures), ": ", failures)
  }), use.names = FALSE)
  if (length(worker_errors)) {
    stop("Slice worker product failures:\n",
         paste(worker_errors, collapse = "\n"),
         call. = FALSE)
  }

  stage_timings$workers <- (proc.time() - t0)[["elapsed"]]
  message(sprintf("  all workers done in %.1fs (wall clock)",
                  stage_timings$workers))

  # ---- Stage 5: Merge slice outputs ----------------------------------
  message("\n--- STAGE 5: merge slice outputs ---")
  t0 <- proc.time()
  merge_stats <- merge_slice_outputs(
    slice_run_dirs = slice_run_dirs,
    canonical_run_dir = canonical_run_dir,
    format = build_format
  )
  stage_timings$merge <- (proc.time() - t0)[["elapsed"]]
  merge_msg <- sprintf("  merged %d files, %d agency spines in %.1fs",
                       merge_stats$moved_files,
                       merge_stats$merged_agency_spines,
                       stage_timings$merge)
  if (!is.null(merge_stats$fast_merged_agency_spines) &&
      merge_stats$fast_merged_agency_spines > 0L) {
    merge_msg <- sprintf(
      "%s (%d agency spines via DuckDB)",
      merge_msg,
      merge_stats$fast_merged_agency_spines
    )
  }
  message(merge_msg)

  # ---- Stage 6: Complete PLIDA DIL product-table structures ----------
  dil_schema_result <- NULL
  if (complete_dil_schema) {
    message("\n--- STAGE 6: PLIDA DIL product-table structures ---")
    t0 <- proc.time()
    dil_schema_result <- .complete_plida_dil_structures(
      run_dir = canonical_run_dir,
      build_order = build_order,
      seed = seed,
      max_rows = complete_dil_rows
    )
    stage_timings$dil_schema <- (proc.time() - t0)[["elapsed"]]
    message(sprintf("  DIL schema done in %.1fs", stage_timings$dil_schema))
  }

  # ---- Stage 7: Cleanup ----------------------------------------------
  if (!keep_slice_dirs) {
    cleanup_slice_dirs(slice_run_dirs)
  }

  remove_internal_base_file()

  total_elapsed <- (proc.time() - total_start)[["elapsed"]]
  message(sprintf("\n=== Build done in %.1fs (%.1f min) ===",
                  total_elapsed, total_elapsed / 60))

  # ---- Stage 9: Post-build CSV conversion ----------------------------
  # When CSV was requested, convert the canonical parquet run directory
  # to CSV via DuckDB. Output goes to `<run_dir>_csv/`. When
  # `keep_parquet = FALSE`, delete the parquet run directory after
  # conversion and redirect the fplida.run_dir option to the CSV side.
  csv_result <- NULL
  result_run_dir <- canonical_run_dir
  if (want_csv) {
    message("\n--- STAGE 9: parquet -> CSV conversion (DuckDB) ---")
    csv_dir <- paste0(canonical_run_dir, "_csv")
    csv_t0 <- proc.time()
    csv_result <- convert_parquet_dir_to_csv(
      src_dir = canonical_run_dir,
      dst_dir = csv_dir,
      verbose = FALSE,
      preserve_parquet_datasets = "ato-stp",
      messy_files = messy_files,
      messy_names = messy_names
    )
    csv_elapsed <- (proc.time() - csv_t0)[["elapsed"]]
    message(sprintf(
      "  CSV: %d files, %s rows, %.2f GB, %.1fs",
      csv_result$total_files,
      format(csv_result$total_rows, big.mark = ","),
      csv_result$total_bytes / (1024 * 1024 * 1024),
      csv_elapsed
    ))
    stage_timings$csv_convert <- csv_elapsed
    if (!isTRUE(keep_parquet)) {
      message("  Deleting parquet run dir: ", canonical_run_dir)
      unlink(canonical_run_dir, recursive = TRUE)
      options(fplida.run_dir = csv_dir)
      result_run_dir <- csv_dir
      if (!is.null(dil_schema_result)) {
        dil_schema_result$files <- character()
        dil_schema_result$parquet_files_removed <- TRUE
      }
    }
  }

  invisible(list(
    n                     = n,
    seed                  = seed,
    k_slices              = k_slices,
    rayon_threads         = rayon_threads,
    years                 = years,
    products              = build_order,
    format                = export_format,
    build_format          = build_format,
    messy_files           = messy_files,
    messy_names           = messy_names,
    export_base_file      = export_base_file,
    complete_dil_schema   = complete_dil_schema,
    complete_dil_rows     = complete_dil_rows,
    canonical_run_dir     = result_run_dir,
    total_elapsed_seconds = total_elapsed,
    stage_timings         = stage_timings,
    merge_stats           = merge_stats,
    dil_schema            = dil_schema_result,
    worker_results        = worker_results,
    csv_conversion        = csv_result
  ))
}
