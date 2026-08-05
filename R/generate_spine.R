#' Generate the synthetic person spine
#'
#' Creates a cross-sectional person spine with demographics, education,
#' occupation (real 6-digit ANZSCO codes with 6D task scores), and income
#' parameters. All datasets (Census, ITR, STP, etc.) are later projected
#' from this spine.
#'
#' @param n Integer. Number of persons to generate.
#' @param seed Integer. Random seed for reproducibility.
#' @param output_dir Character or NULL. Base output directory. If NULL, uses
#'   \code{get_data_path()}. One of the two must be set.
#' @param format Character. Output format: "parquet" (default) or "csv".
#' @param suffix Character or NULL. Optional suffix appended to the run
#'   directory name to avoid overwriting (e.g. "seed3" creates
#'   \code{fplida_5m_seed3/}). By default, runs with the same N (rounded)
#'   overwrite each other.
#' @return A data.frame with one row per person. Includes core attributes
#'   (41 variables from Rust, including baseline_income, disability
#'   attributes, country_of_birth_sacc, ASGS sa2/sa3/sa4_code and
#'   household_id) plus 13 agency-specific SYNTHETIC_AEUID variables.
#' @param return_data Logical. If TRUE (default), the function returns
#'   an R data.frame of the spine for the caller's use. At very large N
#'   this conversion can take many minutes; orchestration paths that
#'   discard the return value should pass `return_data = FALSE`.
#' @param use_template Logical. If TRUE (default), use the spine
#'   template cache: at the first call for a given seed, a ~100k
#'   template is built once; subsequent calls sample from it with
#'   replacement. This is dramatically faster at large N. Set to
#'   FALSE to build each person directly without template sampling.
#' @param template_n Integer. Template size when building (default
#'   100000).
#' @export
generate_spine <- function(n = 200000L, seed = 42L, output_dir = NULL,
                           format = c("parquet", "csv"), suffix = NULL,
                           return_data = TRUE,
                           use_template = TRUE,
                           template_n = .SPINE_TEMPLATE_DEFAULT_N) {
  n <- as.integer(n)
  seed <- as.integer(seed)
  format <- match.arg(format)
  stopifnot(n > 0L, !is.na(seed))

  # Resolve output + run dir up front — needed by both paths.
  base <- resolve_output_dir(output_dir)
  run_dir <- file.path(base, run_dir_name(n, suffix))
  if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
  sys_dir <- file.path(run_dir, "_system")
  if (!dir.exists(sys_dir)) dir.create(sys_dir, recursive = TRUE)
  spine_path <- file.path(sys_dir, "base-spine.parquet")

  # --- Template fast path (parquet only) -----------------------------------
  # Read template from cache, sample n rows, write output parquet,
  # all in Rust. No R-side arrow conversion, no R data.frame.
  if (use_template && format == "parquet") {
    tpath <- spine_template_path(seed)
    if (!file.exists(tpath)) {
      build_spine_template(seed = seed, n_template = template_n)
    }
    generate_spine_from_template_parquet__(
      template_path = tpath,
      n             = n,
      seed          = seed,
      out_path      = spine_path,
      agency_codes  = .AGENCIES
    )
    message("Spine saved to ", spine_path, " (sampled from template)")
    .write_run_metadata(sys_dir, n, seed)
    options(fplida.run_dir = run_dir)

    if (return_data) {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Package 'arrow' is required to return data.frame at ",
             "return_data = TRUE.", call. = FALSE)
      }
      return(as.data.frame(read_parquet_safely(spine_path),
                            stringsAsFactors = FALSE))
    }
    return(invisible(NULL))
  }

  # --- Legacy full-generation path -----------------------------------------
  # Fresh per-person generation, R-side arrow_table + write_parquet.
  # Used when use_template = FALSE or format = "csv".
  raw <- generate_spine__(n, seed)
  cols <- as.list(raw)

  for (agency in .AGENCIES) {
    cols[[paste0("aeuid_", tolower(agency))]] <-
      generate_aeuids(n, agency, seed)
  }

  if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for parquet output. ",
           "Install it with: install.packages('arrow')", call. = FALSE)
    }
    tbl <- arrow::arrow_table(!!!cols)
    arrow::write_parquet(tbl, spine_path)
    message("Spine saved to ", spine_path)
    rm(tbl)
  } else {
    df_tmp <- as.data.frame(cols, stringsAsFactors = FALSE)
    .write_spine_file(df_tmp, sys_dir, "base-spine", format)
    rm(df_tmp)
  }

  .write_run_metadata(sys_dir, n, seed)
  options(fplida.run_dir = run_dir)

  if (return_data) {
    as.data.frame(cols, stringsAsFactors = FALSE)
  } else {
    invisible(NULL)
  }
}


#' Write run metadata to _system/metadata.md
#' @noRd
.write_run_metadata <- function(sys_dir, n, seed) {
  path <- file.path(sys_dir, "metadata.md")
  lines <- c(
    "# fplida run metadata",
    "",
    sprintf("- **N**: %d", n),
    sprintf("- **Seed**: %d", seed),
    sprintf("- **Created**: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  )
  writeLines(lines, path)
}


#' Write a spine data.frame to parquet or csv
#' @noRd
.write_spine_file <- function(df, dir, stem, format) {
  ext <- if (format == "parquet") ".parquet" else ".csv"
  path <- file.path(dir, paste0(stem, ext))
  if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for parquet output. ",
           "Install it with: install.packages('arrow')", call. = FALSE)
    }
    arrow::write_parquet(df, path)
  } else {
    write.csv(df, path, row.names = FALSE)
  }
  message("Spine saved to ", path)
  invisible(path)
}
