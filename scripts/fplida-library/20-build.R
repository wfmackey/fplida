source_provenance <- function(paths) {
  git <- Sys.getenv("FPLIDA_GIT", Sys.which("git"))
  git_read <- function(arguments) {
    output <- system2(git, c("-C", shQuote(paths$source), arguments),
                       stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(output, "status"))) stop(paste(output, collapse = "\n"))
    output
  }
  if (length(git_read(c("diff", "--name-only", "--diff-filter=U")))) {
    stop("Source has unresolved merge conflicts")
  }
  if (length(git_read(c("status", "--porcelain", "--untracked-files=no")))) {
    stop("Commit tracked source changes before a reproducible build")
  }
  sha <- git_read(c("rev-parse", "HEAD"))[[1L]]
  package <- find.package("fplida")
  stamp <- file.path(package, "SOURCE_GIT_SHA")
  if (!file.exists(stamp) || !identical(trimws(readLines(stamp, warn = FALSE)[1L]), sha)) {
    stop("Installed fplida SOURCE_GIT_SHA does not match source HEAD")
  }
  binary <- list.files(file.path(package, "libs"), pattern = "\\.(so|dll|dylib)$",
                        full.names = TRUE, recursive = TRUE)
  list(version = as.character(utils::packageVersion("fplida")), git_sha = sha,
       source_repo = paths$source, installed_package = package,
       binary_md5 = as.list(tools::md5sum(binary)),
       r_version = R.version.string,
       built_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
}

build_library_profile <- function(config, paths, run_id) {
  provenance <- source_provenance(paths)
  run <- library_run_paths(paths, config$name, run_id)
  if (dir.exists(run$work) || dir.exists(run$records)) stop("Run ID already exists: ", run_id)
  purrr::walk(c(run$work, run$slices, run$records), function(path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  })
  log_connection <- file(file.path(run$records, "build.log"), open = "wt")
  sink(log_connection, split = TRUE)
  sink(log_connection, type = "message")
  on.exit({ sink(type = "message"); sink(); close(log_connection) }, add = TRUE)
  config$run_id <- run_id
  if (!"years_by_product" %in% names(formals(fplida::build_fplida))) {
    stop("Installed build_fplida lacks years_by_product")
  }
  if (!"stp_zstd_level" %in% names(formals(fplida::build_fplida))) {
    stop("Installed build_fplida lacks stp_zstd_level")
  }
  workers <- Sys.getenv("FPLIDA_BUILD_SLICES", "")
  processes <- Sys.getenv("FPLIDA_BUILD_WORKERS", "")
  threads <- Sys.getenv("FPLIDA_BUILD_THREADS", "")
  config$k_slices <- if (nzchar(workers)) as.integer(workers) else config$k_slices
  config$n_workers <- if (nzchar(processes)) as.integer(processes) else config$n_workers
  config$rayon_threads <- if (nzchar(threads)) as.integer(threads) else config$rayon_threads
  json_write_atomic(list(config = config, provenance = provenance, paths = run),
                    file.path(run$records, "request.json"))
  # A fresh cache prevents the old offline tree from supplying a base spine.
  old_data_path <- getOption("fplida.data_path")
  options(fplida.data_path = file.path(run$work, "fresh-cache"))
  on.exit(options(fplida.data_path = old_data_path), add = TRUE)
  started <- Sys.time()
  result <- fplida::build_fplida(
    n = config$n, seed = config$seed, years = config$years,
    years_by_product = config$years_by_product, products = config$products,
    k_slices = config$k_slices, n_workers = config$n_workers,
    rayon_threads = config$rayon_threads,
    stp_zstd_level = config$stp_zstd_level,
    output_dir = run$work, slice_parent_dir = run$slices,
    export_format = config$format, keep_parquet = FALSE, keep_slice_dirs = FALSE,
    export_base_file = FALSE, complete_dil_schema = config$complete_dil_schema,
    complete_dil_rows = config$complete_dil_rows,
    messy_files = TRUE, messy_names = TRUE
  )
  saveRDS(result, file.path(run$records, "build-result.rds"))
  # The API already returns the CSV directory when source Parquet is removed.
  output <- normalizePath(result$canonical_run_dir, mustWork = TRUE)
  state <- list(config = config, provenance = provenance, paths = run,
                output = output, elapsed_seconds = as.numeric(difftime(
                  Sys.time(), started, units = "secs")), status = "built")
  json_write_atomic(state, file.path(run$records, "state.json"))
  cat("Built staged data: ", output, "\nRun ID: ", run_id, "\n", sep = "")
  invisible(state)
}

verify_library_profile <- function(paths, run_id) {
  record_dir <- file.path(paths$records, run_id)
  state <- jsonlite::read_json(file.path(record_dir, "state.json"), simplifyVector = TRUE)
  result <- readRDS(file.path(record_dir, "build-result.rds"))
  audit_temp <- file.path(record_dir, "duckdb-temp")
  # Each audit helper closes its connections before this cleanup runs.
  on.exit(unlink(audit_temp, recursive = TRUE, force = TRUE), add = TRUE)
  inventory <- inventory_library(state$output, result,
                                temp_dir = audit_temp)
  qa <- check_library(state$output, state$config, inventory,
                       temp_dir = audit_temp, result = result)
  json_write_atomic(qa, file.path(record_dir, "qa.json"))
  write_library_manifest(state$output, state$config, state$provenance, inventory, qa)
  state$status <- if (qa$passed) "verified" else "failed_qa"
  state$verified_file_signature <- as.list(tools::md5sum(
    file.path(state$output, c("file-manifest.csv", "asset-manifest.csv", "manifest.json"))
  ))
  json_write_atomic(state, file.path(record_dir, "state.json"))
  if (!qa$passed) stop("Data failed QA; see ", file.path(record_dir, "qa.json"))
  cat("Verified staged data: ", state$output, "\n", sep = "")
  invisible(state)
}

publish_library_profile <- function(paths, run_id) {
  record_dir <- file.path(paths$records, run_id)
  state <- jsonlite::read_json(file.path(record_dir, "state.json"), simplifyVector = TRUE)
  if (!identical(state$status, "verified")) stop("Run is not verified")
  expected <- utils::read.csv(file.path(state$output, "file-manifest.csv"),
                              stringsAsFactors = FALSE)
  actual <- file.info(file.path(state$output, expected$path))
  actual_files <- list.files(state$output, pattern = "\\.(csv|parquet)$", recursive = TRUE)
  actual_files <- actual_files[!actual_files %in% c("file-manifest.csv", "asset-manifest.csv")]
  if (!setequal(actual_files, expected$path) || any(is.na(actual$size)) ||
      any(actual$size != expected$bytes) ||
      any(abs(as.numeric(actual$mtime) - expected$modified_unix) > 0.0001)) {
    stop("Staged files changed after QA")
  }
  manifest_paths <- file.path(state$output, c("file-manifest.csv", "asset-manifest.csv", "manifest.json"))
  if (!identical(unname(as.character(tools::md5sum(manifest_paths))),
                 unname(as.character(unlist(state$verified_file_signature))))) {
    stop("Staged manifests changed after QA")
  }
  run <- state$paths
  dir.create(dirname(run$final), recursive = TRUE, showWarnings = FALSE)
  backed_up <- FALSE
  if (file.exists(run$final)) {
    dir.create(dirname(run$backup), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(run$backup)) stop("Backup already exists: ", run$backup)
    if (!file.rename(run$final, run$backup)) stop("Could not preserve previous folder")
    backed_up <- TRUE
  }
  if (!file.rename(state$output, run$final)) {
    if (backed_up) file.rename(run$backup, run$final)
    stop("Could not publish staged folder; previous data restored where possible")
  }
  state$output <- run$final
  state$status <- "published"
  state$published_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  json_write_atomic(state, file.path(record_dir, "state.json"))
  cat("Published: ", run$final, "\n", sep = "")
  invisible(state)
}

archive_obsolete_library_entries <- function(paths, run_id) {
  profiles <- c("1k", "100k", "1m", "30m")
  purrr::walk(profiles, function(profile) {
    manifest <- file.path(paths$final, profile, "manifest.json")
    if (!file.exists(manifest)) stop("Final profile is incomplete: ", profile)
    content <- jsonlite::read_json(manifest)
    if (!isTRUE(content$qa$passed)) stop("Final profile failed QA: ", profile)
  })
  entries <- list.files(paths$final, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  entries <- entries[!basename(entries) %in% profiles]
  destination <- file.path(paths$backup, run_id, "previous-library")
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  purrr::walk(entries, function(entry) {
    target <- file.path(destination, basename(entry))
    if (file.exists(target)) stop("Backup already exists: ", target)
    if (!file.rename(entry, target)) stop("Cannot archive ", entry)
    cat("Preserved previous entry: ", target, "\n", sep = "")
  })
  stopifnot(setequal(list.files(paths$final, all.files = TRUE, no.. = TRUE), profiles))
  invisible(destination)
}
