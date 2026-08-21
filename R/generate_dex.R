#' Generate DEX dataset (Data Exchange)
#'
#' @section Dataset and variable information:
#' The [DSS Data Exchange policy](https://dex.dss.gov.au/policy) website gives
#' information about this dataset. Use `dataset_info("DEX")` for dataset
#' information. Use `variable_info("DEX")` for variables, sources, value
#' support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @export
generate_dex <- function(spine = NULL, seed = 42L, output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = TRUE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  if (format != "parquet") stop("dex writes parquet only.", call. = FALSE)

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "DEX")

  dex_cols <- c("spine_id", "aeuid_dss", "birth_year", "sex", "state",
                "indigenous", "country_of_birth_sacc", "education", "baseline_employed",
                "baseline_income", "sa2_code", "disability_severity")
  spine_loaded <- is.null(spine)
  if (spine_loaded) spine <- load_spine_select(run_dir, dex_cols)
  stopifnot(is.data.frame(spine))

  mini_spine <- data.frame(spine_id = spine$spine_id, aeuid_dss = spine$aeuid_dss,
                           stringsAsFactors = FALSE)

  out_client     <- file.path(ds_dir, "madipge-dex-d-extended-15-current-special_client.parquet")
  out_attendance <- file.path(ds_dir, "madipge-dex-d-extended-15-current-special_attendance.parquet")
  out_assessment <- file.path(ds_dir, "madipge-dex-d-extended-15-current-special_client_assessment.parquet")
  n_rows <- project_dex_to_parquet__(
    aeuid             = as.character(spine$aeuid_dss),
    birth_year        = as.integer(spine$birth_year),
    sex               = as.integer(spine$sex),
    state             = as.integer(spine$state),
    indigenous        = as.integer(spine$indigenous),
    # SACC country code (consistent across datasets) under BIRTHCOUNTRYCODE.
    country_of_birth  = as.integer(spine$country_of_birth_sacc),
    education         = as.integer(spine$education),
    baseline_employed = as.integer(spine$baseline_employed),
    baseline_income   = as.numeric(spine$baseline_income),
    sa2               = as.integer(spine$sa2_code),
    disability_severity = as.integer(spine$disability_severity),
    seed              = as.integer(seed + 2900L),
    out_client        = out_client,
    out_attendance    = out_attendance,
    out_assessment    = out_assessment
  )

  # DEX publishes fifteen tables and the bespoke generator writes three. The
  # rest are the activity, session and reference tables a service-delivery
  # pipeline joins to; without them a join on `special_organisation` cannot
  # be prototyped at all.
  clients <- if (file.exists(out_client)) {
    as.data.frame(read_parquet_safely(out_client), stringsAsFactors = FALSE)
  } else {
    NULL
  }
  if (!is.null(clients) && nrow(clients)) {
    index <- match(clients$SYNTHETIC_AEUID, as.character(spine$aeuid_dss))
    client_rows <- spine[index, , drop = FALSE]
    period <- list(start_year = 2015L, end_year = 2025L)
    bespoke <- c("special_client", "special_attendance",
                 "special_client_assessment")
    product <- "madipge-dex-d-extended-15-current"

    # The three bespoke tables emit a subset of the columns the data item
    # list gives them, so a consumer joining on OUTLETID or ACTIVITYID finds
    # the column absent rather than empty. Top them up from the registry
    # rather than leaving the gap.
    for (table in bespoke) {
      path <- file.path(ds_dir, sprintf("%s-%s.parquet", product, table))
      if (!file.exists(path)) next
      frame <- as.data.frame(read_parquet_safely(path),
                             stringsAsFactors = FALSE)
      if (!nrow(frame)) next
      missing <- setdiff(.registry_table_variables("DEX", table),
                         names(frame))
      if (!length(missing)) next
      index <- match(frame$SYNTHETIC_AEUID, as.character(spine$aeuid_dss))
      filled <- .project_registry_table("DEX", product, table,
                                        spine[index, , drop = FALSE],
                                        frame$SYNTHETIC_AEUID, seed, period,
                                        source_frame = frame)
      if (is.null(filled)) next
      for (name in missing) frame[[name]] <- filled[[name]]
      arrow::write_parquet(frame, path)
    }

    for (table in setdiff(.registry_product_tables("DEX", product), bespoke)) {
      frame <- .project_registry_table("DEX", product, table, client_rows,
                                       clients$SYNTHETIC_AEUID, seed, period,
                                       source_frame = clients)
      if (is.null(frame)) next
      # The reference and lookup tables describe a service, an outlet or an
      # organisation, not a client. Carrying one row per person would make
      # them a per-client record under a catalogue's name, and a join on
      # `special_organisation` would return one organisation per client.
      catalogue_size <- .DEX_CATALOGUE_ROWS[[table]]
      if (!is.null(catalogue_size)) {
        key <- setdiff(names(frame), "SYNTHETIC_AEUID")
        frame <- frame[seq_len(min(catalogue_size, nrow(frame))), key,
                       drop = FALSE]
        rownames(frame) <- NULL
      }
      arrow::write_parquet(
        frame, file.path(ds_dir, sprintf("%s-%s.parquet", product, table)))
    }
  }

  write_agency_spine(mini_spine, "DSS", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  if (return_data && file.exists(out_client)) {
    return(as.data.frame(read_parquet_safely(out_client)))
  }
  invisible(list(n_rows = as.integer(n_rows)))
}
