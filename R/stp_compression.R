# Optional STP compression runs after generation, before slice consolidation.
.validate_stp_zstd_level <- function(level) {
  if (is.null(level)) return(NULL)
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      !is.finite(level) || level != floor(level) || level < 1L || level > 22L) {
    stop("stp_zstd_level must be NULL or an integer from 1 to 22.", call. = FALSE)
  }
  if (!requireNamespace("arrow", quietly = TRUE) ||
      !arrow::codec_is_available("zstd")) {
    stop("STP ZSTD compression requires Arrow with ZSTD support.", call. = FALSE)
  }
  as.integer(level)
}

.write_stp_zstd <- function(table, path, level) {
  arrow::write_parquet(table, path, compression = "zstd",
                       compression_level = level, chunk_size = 131072L,
                       use_dictionary = TRUE, write_statistics = TRUE)
}

.replace_stp_parquet <- function(temporary, path) {
  # A sibling file keeps the rename on the same filesystem. Fail closed if
  # the platform cannot replace an existing file; never delete it first.
  if (!file.rename(temporary, path)) {
    stop("Cannot replace STP Parquet file: ", path, call. = FALSE)
  }
}

.reencode_stp_parquet_file <- function(path, level) {
  temporary <- tempfile(paste0(".", basename(path), ".zstd-"),
                        tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  input_bytes <- unname(file.size(path))
  original <- arrow::read_parquet(path, as_data_frame = FALSE, mmap = FALSE)
  .write_stp_zstd(original, temporary, level)
  encoded <- arrow::read_parquet(temporary, as_data_frame = FALSE, mmap = FALSE)
  schema_equal <- original$schema$Equals(encoded$schema, check_metadata = TRUE)
  values_equal <- schema_equal && original$Equals(encoded, check_metadata = TRUE)
  # Arrow treats NaN as unequal to itself. Use an exact R comparison only
  # when Arrow cannot confirm equality, including exceptional float values.
  if (schema_equal && !values_equal) {
    values_equal <- identical(as.data.frame(original), as.data.frame(encoded),
                              num.eq = FALSE)
  }
  if (!schema_equal || !values_equal || original$num_rows != encoded$num_rows) {
    stop("STP Parquet verification failed: ", path, call. = FALSE)
  }
  result <- list(input_bytes = input_bytes,
                 output_bytes = unname(file.size(temporary)),
                 rows = original$num_rows)
  # Release both readers before replacement, including on Windows.
  rm(original, encoded)
  gc(verbose = FALSE)
  .replace_stp_parquet(temporary, path)
  result
}

.compress_slice_stp <- function(slice_run_dir, level) {
  level <- .validate_stp_zstd_level(level)
  if (is.null(level)) return(NULL)
  started <- proc.time()[["elapsed"]]
  files <- list.files(file.path(slice_run_dir, "ato-stp"),
                       pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE)
  files <- files[grepl("^stp_(standard|extended)_", basename(dirname(files)))]
  previous_threads <- arrow::cpu_count()
  arrow::set_cpu_count(1L)
  on.exit(arrow::set_cpu_count(previous_threads), add = TRUE)
  # Generation has finished; reclaim its R objects before reading a file.
  gc(verbose = FALSE)
  stats <- lapply(files, .reencode_stp_parquet_file, level = level)
  total <- function(field) sum(vapply(stats, `[[`, numeric(1), field))
  list(codec = "zstd", level = level, files = length(files),
       input_bytes = total("input_bytes"), output_bytes = total("output_bytes"),
       rows = total("rows"), elapsed_seconds = proc.time()[["elapsed"]] - started)
}
