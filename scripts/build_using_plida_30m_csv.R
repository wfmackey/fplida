#!/usr/bin/env Rscript

# Build the exact fplida source corpus expected by using-plida:
#   $FPLIDA_OUTPUT_ROOT/fplida_30m_csv
#
# The package generators build internally to parquet. This script keeps STP
# parquet, converts the non-STP source files used by using-plida to CSV, and
# deletes unused parquet products to keep disk use bounded.

options(warn = 1)

script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE),
                                       value = TRUE)[1])
if (is.na(script_path)) {
  script_path <- file.path(getwd(), "scripts", "build_using_plida_30m_csv.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           mustWork = TRUE)

output_root <- path.expand(Sys.getenv(
  "FPLIDA_OUTPUT_ROOT", file.path("~", "offline", "fplida-data")
))
target_dir <- file.path(output_root, "fplida_30m_csv")
mbs_tmp_dir <- file.path(output_root, "fplida_30m_mbs2015_tmp")
log_dir <- file.path(output_root, "_build_logs")
convert_only <- identical(Sys.getenv("FPLIDA_CONVERT_ONLY"), "1")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
say <- function(...) {
  cat(sprintf("[%s] %s\n", stamp(), paste0(..., collapse = "")))
  flush.console()
}

run_cmd <- function(cmd, args, ...) {
  say("Running: ", paste(c(cmd, args), collapse = " "))
  status <- system2(cmd, args, stdout = "", stderr = "", ...)
  if (!identical(status, 0L)) {
    stop("Command failed with status ", status, ": ",
         paste(c(cmd, args), collapse = " "), call. = FALSE)
  }
  invisible(status)
}

dir_size_gib <- function(path) {
  if (!dir.exists(path)) return(NA_real_)
  out <- suppressWarnings(system2("du", c("-sk", path),
                                  stdout = TRUE, stderr = TRUE))
  kb <- suppressWarnings(as.numeric(strsplit(out[1], "[[:space:]]+")[[1]][1]))
  kb / 1024^2
}

free_gib <- function(path) {
  probe <- if (dir.exists(path)) path else dirname(path)
  out <- suppressWarnings(system2("df", c("-k", probe),
                                  stdout = TRUE, stderr = TRUE))
  fields <- strsplit(out[length(out)], "[[:space:]]+")[[1]]
  fields <- fields[nzchar(fields)]
  as.numeric(fields[4]) / 1024^2
}

write_table <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "")
}

stage_timings <- function(result) {
  st <- result$stage_timings
  if (is.null(st) || !length(st)) return(data.frame())
  data.frame(
    stage = names(st),
    elapsed_seconds = round(as.numeric(st), 3),
    stringsAsFactors = FALSE
  )
}

product_timings <- function(result) {
  wr <- result$worker_results
  if (is.null(wr) || !length(wr)) return(data.frame())
  rows <- do.call(rbind, lapply(wr, function(worker) {
    pr <- worker$product_results
    if (is.null(pr) || !length(pr)) return(NULL)
    data.frame(
      slice_id = worker$slice_id,
      product = names(pr),
      elapsed_seconds = vapply(pr, function(x) x$elapsed_seconds, numeric(1)),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(rows) || !nrow(rows)) return(data.frame())

  products <- sort(unique(rows$product))
  out <- data.frame(
    product = products,
    max_worker_seconds = NA_real_,
    sum_worker_seconds = NA_real_,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(products)) {
    x <- rows$elapsed_seconds[rows$product == products[i]]
    out$max_worker_seconds[i] <- max(x)
    out$sum_worker_seconds[i] <- sum(x)
  }
  out$max_worker_seconds <- round(out$max_worker_seconds, 3)
  out$sum_worker_seconds <- round(out$sum_worker_seconds, 3)
  out
}

csv_converter <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) {
    stop("CSV conversion requires duckdb and DBI.", call. = FALSE)
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  DBI::dbExecute(con, "PRAGMA threads = 8")
  DBI::dbExecute(con, "PRAGMA memory_limit = '20GB'")
  DBI::dbExecute(con, "PRAGMA preserve_insertion_order = false")

  escape_sql <- function(path) gsub("'", "''", path, fixed = TRUE)

  convert_one <- function(src_glob, csv_out, label) {
    dir.create(dirname(csv_out), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(csv_out)) unlink(csv_out)
    t0 <- proc.time()
    DBI::dbExecute(con, sprintf(
      "COPY (SELECT * FROM read_parquet('%s')) TO '%s' (FORMAT CSV, HEADER)",
      escape_sql(src_glob), escape_sql(csv_out)
    ))
    elapsed <- (proc.time() - t0)[["elapsed"]]
    size_gib <- file.size(csv_out) / 1024^3
    say(sprintf("Converted %-42s %8.2f GiB in %.1fs",
                label, size_gib, elapsed))
    data.frame(
      label = label,
      src = src_glob,
      dest = csv_out,
      elapsed_seconds = round(elapsed, 3),
      size_gib = round(size_gib, 3),
      stringsAsFactors = FALSE
    )
  }

  convert_one
}

convert_flat_parquet <- function(convert_one, rel_path, label) {
  pq <- file.path(target_dir, rel_path)
  csv <- sub("\\.parquet$", ".csv", pq)
  res <- convert_one(pq, csv, label)
  unlink(pq)
  res
}

convert_product_dir <- function(convert_one, rel_dir, csv_name = NULL,
                                label = rel_dir) {
  src_dir <- file.path(target_dir, rel_dir)
  if (is.null(csv_name)) csv_name <- paste0(basename(src_dir), ".csv")
  csv <- file.path(dirname(src_dir), csv_name)
  res <- convert_one(file.path(src_dir, "part-*.parquet"), csv, label)
  unlink(src_dir, recursive = TRUE)
  res
}

convert_and_prune <- function() {
  say("Starting in-place CSV conversion/pruning for using-plida subset")
  convert_one <- csv_converter()
  rows <- list()
  add <- function(x) rows[[length(rows) + 1L]] <<- x

  # CORE: using-plida consumes the 2021 demographic table and ABS spine.
  add(convert_flat_parquet(
    convert_one,
    "abs-core/plidage-core-demog-cb-c21-2006-latest.parquet",
    "core-demog"
  ))
  add(convert_flat_parquet(convert_one, "abs-core/abs-spine.parquet",
                           "abs-spine"))
  unlink(file.path(target_dir, "abs-core", "*.parquet"))
  unlink(file.path(target_dir, "abs-core", "plidage-core-locat-cb-2006-latest"),
         recursive = TRUE)
  unlink(file.path(target_dir, "abs-core", "plidage-core-relat-cb-c21-2006-latest"),
         recursive = TRUE)

  # PIT_ITR: using-plida converts every ITR CSV and its ATO spine.
  pit_itr_dir <- file.path(target_dir, "ato-pit_itr")
  pit_itr_products <- list.dirs(pit_itr_dir, recursive = FALSE,
                                full.names = FALSE)
  for (product in sort(pit_itr_products)) {
    add(convert_product_dir(convert_one, file.path("ato-pit_itr", product),
                            csv_name = paste0(product, ".csv"),
                            label = paste0("pit_itr/", product)))
  }
  add(convert_flat_parquet(convert_one, "ato-pit_itr/ato-spine.parquet",
                           "ato-spine"))

  # MBS: using-plida consumes the 2015 claims file and DHDA spine.
  mbs_dir <- file.path(target_dir, "dhda-mbs")
  mbs_products <- list.dirs(mbs_dir, recursive = FALSE, full.names = FALSE)
  for (product in mbs_products) {
    rel <- file.path("dhda-mbs", product)
    if (identical(product, "madipge-mbs-d-claims-2015")) {
      add(convert_product_dir(convert_one, rel,
                              csv_name = paste0(product, ".csv"),
                              label = "mbs-2015"))
    } else {
      unlink(file.path(target_dir, rel), recursive = TRUE)
    }
  }
  add(convert_flat_parquet(convert_one, "dhda-mbs/dhda-spine.parquet",
                           "dhda-spine"))

  # PIT_PS is only a build-time dependency of PIT_ITR and is not consumed by
  # using-plida's raw-source conversion scripts.
  unlink(file.path(target_dir, "ato-pit_ps"), recursive = TRUE)
  unlink(file.path(target_dir, "_system"), recursive = TRUE)

  out <- if (length(rows)) do.call(rbind, rows) else data.frame()
  write_table(out, file.path(log_dir, "using_plida_30m_csv_conversion.csv"))
  say("CSV conversion/pruning complete. Final size: ",
      sprintf("%.2f GiB", dir_size_gib(target_dir)))
  out
}

say("Repo root: ", repo_root)
say("Output root: ", output_root)
say("Target directory: ", target_dir)
say("Free space before build: ", sprintf("%.1f GiB", free_gib(output_root)))

if (!convert_only && dir.exists(target_dir)) {
  say("Removing existing target directory before clean rebuild: ", target_dir)
  unlink(target_dir, recursive = TRUE, force = TRUE)
}
if (!convert_only && dir.exists(mbs_tmp_dir)) {
  say("Removing existing MBS temp directory before clean rebuild: ", mbs_tmp_dir)
  unlink(mbs_tmp_dir, recursive = TRUE, force = TRUE)
}

if (!identical(Sys.getenv("FPLIDA_SKIP_INSTALL"), "1")) {
  run_cmd(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", repo_root))
} else {
  say("Skipping package install because FPLIDA_SKIP_INSTALL=1")
}

suppressPackageStartupMessages(library(fplida))
say("Loaded fplida version: ", as.character(utils::packageVersion("fplida")))

if (convert_only) {
  if (!dir.exists(target_dir)) {
    stop("FPLIDA_CONVERT_ONLY=1 but target directory does not exist: ",
         target_dir, call. = FALSE)
  }
  say("FPLIDA_CONVERT_ONLY=1; converting/pruning existing target directory")
  conversion_start <- Sys.time()
  conversion <- convert_and_prune()
  summary <- data.frame(
    label = "using_plida_30m_csv",
    n = 30000000L,
    output_dir = target_dir,
    started_at = format(conversion_start, "%Y-%m-%d %H:%M:%S %Z"),
    finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    build_elapsed_seconds = NA_real_,
    final_size_gib = round(dir_size_gib(target_dir), 3),
    free_gib_after = round(free_gib(output_root), 3),
    stringsAsFactors = FALSE
  )
  write_table(summary, file.path(log_dir, "summary_using_plida_30m_csv.csv"))
  say("All requested using-plida 30m source files completed.")
  print(summary, row.names = FALSE)
  quit(save = "no", status = 0)
}

build_start <- Sys.time()
say("Building main using-plida subset without MBS: core, pit_ps, pit_itr, stp")
result_main <- fplida::build_fplida(
  n = 30000000L,
  seed = 42L,
  years = 2015:2025,
  products = c("core", "pit_ps", "pit_itr", "stp"),
  output_dir = output_root,
  suffix = "csv",
  export_format = "parquet",
  keep_slice_dirs = FALSE
)

say("Building MBS separately for 2015 only")
result_mbs <- fplida::build_fplida(
  n = 30000000L,
  seed = 42L,
  years = 2015L,
  products = "mbs",
  output_dir = output_root,
  suffix = "mbs2015_tmp",
  export_format = "parquet",
  keep_slice_dirs = FALSE
)
build_end <- Sys.time()

actual_dir <- normalizePath(result_main$canonical_run_dir, mustWork = TRUE)
expected_dir <- normalizePath(target_dir, mustWork = FALSE)
if (!identical(actual_dir, expected_dir)) {
  stop("Unexpected output directory: expected ", expected_dir, " but got ",
       actual_dir, call. = FALSE)
}

actual_mbs_dir <- normalizePath(result_mbs$canonical_run_dir, mustWork = TRUE)
expected_mbs_dir <- normalizePath(mbs_tmp_dir, mustWork = FALSE)
if (!identical(actual_mbs_dir, expected_mbs_dir)) {
  stop("Unexpected MBS output directory: expected ", expected_mbs_dir,
       " but got ", actual_mbs_dir, call. = FALSE)
}

say("Moving 2015-only MBS output into final target directory")
target_mbs_dir <- file.path(target_dir, "dhda-mbs")
if (dir.exists(target_mbs_dir)) unlink(target_mbs_dir, recursive = TRUE)
ok <- file.rename(file.path(mbs_tmp_dir, "dhda-mbs"), target_mbs_dir)
if (!isTRUE(ok)) {
  stop("Could not move MBS output into final target directory.", call. = FALSE)
}
unlink(mbs_tmp_dir, recursive = TRUE, force = TRUE)

saveRDS(list(main = result_main, mbs2015 = result_mbs),
        file.path(log_dir, "result_using_plida_30m_csv.rds"))
st_main <- stage_timings(result_main)
if (nrow(st_main)) st_main$build <- "main"
st_mbs <- stage_timings(result_mbs)
if (nrow(st_mbs)) st_mbs$build <- "mbs2015"
write_table(rbind(st_main, st_mbs),
            file.path(log_dir, "stage_timings_using_plida_30m_csv.csv"))
pt_main <- product_timings(result_main)
if (nrow(pt_main)) pt_main$build <- "main"
pt_mbs <- product_timings(result_mbs)
if (nrow(pt_mbs)) pt_mbs$build <- "mbs2015"
pt <- rbind(pt_main, pt_mbs)
if (nrow(pt)) {
  pt <- pt[order(pt$max_worker_seconds, decreasing = TRUE), ]
  write_table(pt, file.path(log_dir,
                            "product_timings_using_plida_30m_csv.csv"))
}

say("Parquet subset build complete in ",
    sprintf("%.1f min", as.numeric(difftime(build_end, build_start,
                                            units = "secs")) / 60))
say("Parquet subset size before conversion: ",
    sprintf("%.2f GiB", dir_size_gib(target_dir)))
if (nrow(pt)) {
  say("Top product bottlenecks by max slice-worker seconds:")
  print(head(pt, 12L), row.names = FALSE)
}

conversion <- convert_and_prune()

summary <- data.frame(
  label = "using_plida_30m_csv",
  n = 30000000L,
  output_dir = target_dir,
  started_at = format(build_start, "%Y-%m-%d %H:%M:%S %Z"),
  finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  build_elapsed_seconds = round(as.numeric(difftime(build_end, build_start,
                                                    units = "secs")), 3),
  final_size_gib = round(dir_size_gib(target_dir), 3),
  free_gib_after = round(free_gib(output_root), 3),
  stringsAsFactors = FALSE
)
write_table(summary, file.path(log_dir, "summary_using_plida_30m_csv.csv"))

say("All requested using-plida 30m source files completed.")
print(summary, row.names = FALSE)
