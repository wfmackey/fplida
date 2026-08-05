#!/usr/bin/env Rscript

# Build a 1M-person fplida dataset as MESSY CSV, mirroring the layout and
# quirks of real PLIDA in the ABS DataLab (messy_files + messy_names).
#
# Same generation parameters as the reference 1m build (n = 1M, seed = 42,
# years 2006-2025, health + labour modules on), but exported as CSV with
# the messy DataLab-style folder and variable-name conventions, written to
# ~/offline/datalab1m/. The CORE product includes the core_vitals and
# core_residence core-scope outputs.

options(warn = 1)

`%||%` <- function(a, b) if (is.null(a)) b else a

target_dir <- path.expand(Sys.getenv("FPLIDA_DATALAB_TARGET",
                                      "~/offline/datalab1m"))
work_root <- path.expand(Sys.getenv("FPLIDA_DATALAB_WORK_ROOT",
                                    "~/offline/.datalab1m-build"))

products <- c(
  "census",
  "core",
  "blade",
  "pit_ps",
  "pit_itr",
  "domino",
  "ndis",
  "mbs",
  "pbs",
  "he",
  "tva",
  "stp",
  "travellers",
  "busown",
  "deaths",
  "sdac",
  "lfs"
)
years <- 2006L:2025L
seed <- as.integer(Sys.getenv("FPLIDA_DATALAB_SEED", "42"))
n <- as.integer(Sys.getenv("FPLIDA_DATALAB_N", "1000000"))
k_slices_env <- Sys.getenv("FPLIDA_DATALAB_K_SLICES", "")
k_slices <- if (nzchar(k_slices_env)) as.integer(k_slices_env) else NULL

# Fresh build: clear target and work dirs.
if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE, force = TRUE)
if (dir.exists(work_root)) unlink(work_root, recursive = TRUE, force = TRUE)
dir.create(work_root, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(target_dir), recursive = TRUE, showWarnings = FALSE)

library(fplida)


cat("Building datalab fplida subset (messy CSV)\n")
cat("  target: ", target_dir, "\n", sep = "")
cat("  work_root: ", work_root, "\n", sep = "")
cat("  n: ", format(n, big.mark = ","), "\n", sep = "")
cat("  seed: ", seed, "\n", sep = "")
cat("  years: ", min(years), "-", max(years), "\n", sep = "")
cat("  products: ", paste(products, collapse = ", "), "\n", sep = "")
cat("  modules: health, labour\n")
cat("  export: csv, messy_files = TRUE, messy_names = TRUE\n\n", sep = "")

wall_start <- Sys.time()
result <- build_fplida(
  n = n,
  seed = seed,
  years = years,
  k_slices = k_slices,
  products = products,
  output_dir = work_root,
  export_format = "csv",
  messy_files = TRUE,
  messy_names = TRUE,
  keep_parquet = FALSE,
  keep_slice_dirs = FALSE
)
wall_end <- Sys.time()
elapsed <- as.numeric(difftime(wall_end, wall_start, units = "secs"))

# CSV output lives at "<canonical_run_dir>_csv"; the parquet source was
# deleted (keep_parquet = FALSE), but canonical_run_dir still names its path.
csv_dir <- paste0(result$canonical_run_dir, "_csv")
if (!dir.exists(csv_dir)) {
  stop("Expected CSV run directory not found: ", csv_dir, call. = FALSE)
}
if (!file.rename(csv_dir, target_dir)) {
  stop("Could not move CSV run directory from ", csv_dir,
       " to ", target_dir, call. = FALSE)
}
options(fplida.run_dir = target_dir)

# Drop the (now empty) work root.
unlink(work_root, recursive = TRUE, force = TRUE)

log_dir <- file.path(target_dir, "_build_logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

build_summary <- data.frame(
  metric = c(
    "n", "seed", "year_min", "year_max", "k_slices", "rayon_threads",
    "export_format", "messy_files", "messy_names",
    "wall_start", "wall_end", "wall_elapsed_seconds",
    "build_reported_seconds", "csv_total_files", "csv_total_bytes",
    "target_dir"
  ),
  value = c(
    as.character(n), as.character(seed),
    as.character(min(years)), as.character(max(years)),
    as.character(result$k_slices), as.character(result$rayon_threads),
    "csv", "TRUE", "TRUE",
    format(wall_start, "%Y-%m-%d %H:%M:%S %Z"),
    format(wall_end, "%Y-%m-%d %H:%M:%S %Z"),
    sprintf("%.3f", elapsed),
    sprintf("%.3f", result$total_elapsed_seconds),
    as.character(result$csv_conversion$total_files %||% NA),
    as.character(result$csv_conversion$total_bytes %||% NA),
    target_dir
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(build_summary, file.path(log_dir, "build-summary.csv"),
                 row.names = FALSE)
saveRDS(result, file.path(log_dir, "build-result.rds"))

cat("\nBuild moved to: ", target_dir, "\n", sep = "")
cat("Wall elapsed seconds: ", sprintf("%.1f", elapsed), "\n", sep = "")
cat("Logs: ", log_dir, "\n", sep = "")
