#!/usr/bin/env Rscript

# Build full fplida parquet exports for offline use.
# Outputs:
#   $FPLIDA_OUTPUT_ROOT/fplida_10m

options(warn = 1)

script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE),
                                       value = TRUE)[1])
if (is.na(script_path)) {
  script_path <- file.path(getwd(), "scripts", "build_offline_fplida_exports.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           mustWork = TRUE)

output_root <- path.expand(Sys.getenv(
  "FPLIDA_OUTPUT_ROOT", file.path("~", "offline", "fplida-data")
))
log_dir <- file.path(output_root, "_build_logs")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

targets <- data.frame(
  label = c("10m"),
  n = c(10000000L),
  stringsAsFactors = FALSE
)
targets$dir <- file.path(output_root, paste0("fplida_", targets$label))

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

product_timings <- function(result) {
  wr <- result$worker_results
  if (is.null(wr) || !length(wr)) {
    return(data.frame())
  }
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

stage_timings <- function(result) {
  st <- result$stage_timings
  if (is.null(st) || !length(st)) return(data.frame())
  data.frame(
    stage = names(st),
    elapsed_seconds = round(as.numeric(st), 3),
    stringsAsFactors = FALSE
  )
}

build_one <- function(label, n, target_dir) {
  say("============================================================")
  say("Starting full fplida build: ", label, " (n = ",
      format(n, big.mark = ","), ")")
  say("Output directory: ", target_dir)
  say("Free space before build: ", sprintf("%.1f GiB", free_gib(output_root)))

  if (dir.exists(target_dir)) {
    say("Removing existing target directory before clean rebuild: ", target_dir)
    unlink(target_dir, recursive = TRUE, force = TRUE)
  }

  options(fplida.run_dir = NULL)

  start <- Sys.time()
  result <- fplida::build_fplida(
    n = n,
    seed = 42L,
    products = "all",
    output_dir = output_root,
    export_format = "parquet",
    keep_slice_dirs = FALSE
  )
  end <- Sys.time()

  actual_dir <- normalizePath(result$canonical_run_dir, mustWork = TRUE)
  expected_dir <- normalizePath(target_dir, mustWork = FALSE)
  if (!identical(actual_dir, expected_dir)) {
    stop("Unexpected output directory for ", label, ": expected ",
         expected_dir, " but got ", actual_dir, call. = FALSE)
  }

  elapsed <- as.numeric(difftime(end, start, units = "secs"))
  size_gib <- dir_size_gib(target_dir)
  free_after <- free_gib(output_root)

  saveRDS(result, file.path(log_dir, paste0("result_", label, ".rds")))
  write_table(stage_timings(result),
              file.path(log_dir, paste0("stage_timings_", label, ".csv")))

  pt <- product_timings(result)
  if (nrow(pt)) {
    pt <- pt[order(pt$max_worker_seconds, decreasing = TRUE), ]
    write_table(pt, file.path(log_dir, paste0("product_timings_", label, ".csv")))
  }

  top <- if (nrow(pt)) head(pt, 12L) else data.frame()
  say("Completed ", label, " in ", sprintf("%.1f min", elapsed / 60))
  say("Output size: ", sprintf("%.2f GiB", size_gib),
      "; free space after build: ", sprintf("%.1f GiB", free_after))
  if (nrow(top)) {
    say("Top product bottlenecks by max slice-worker seconds:")
    print(top, row.names = FALSE)
  }

  data.frame(
    label = label,
    n = n,
    output_dir = target_dir,
    started_at = format(start, "%Y-%m-%d %H:%M:%S %Z"),
    finished_at = format(end, "%Y-%m-%d %H:%M:%S %Z"),
    elapsed_seconds = round(elapsed, 3),
    elapsed_minutes = round(elapsed / 60, 3),
    size_gib = round(size_gib, 3),
    free_gib_after = round(free_after, 3),
    stringsAsFactors = FALSE
  )
}

say("Repo root: ", repo_root)
say("Output root: ", output_root)
say("Build log directory: ", log_dir)

if (!identical(Sys.getenv("FPLIDA_SKIP_INSTALL"), "1")) {
  run_cmd(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", repo_root))
} else {
  say("Skipping package install because FPLIDA_SKIP_INSTALL=1")
}

suppressPackageStartupMessages(library(fplida))
say("Loaded fplida version: ", as.character(utils::packageVersion("fplida")))

summary_path <- file.path(log_dir, "build_summary.csv")
summary_rows <- list()

for (i in seq_len(nrow(targets))) {
  row <- targets[i, ]
  res <- build_one(row$label, row$n, row$dir)
  summary_rows[[length(summary_rows) + 1L]] <- res
  write_table(do.call(rbind, summary_rows), summary_path)
  say("Updated summary: ", summary_path)
}

say("All requested fplida builds completed.")
