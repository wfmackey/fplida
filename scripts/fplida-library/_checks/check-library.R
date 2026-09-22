# Checks read completed data. The runner saves the returned report.
check_library <- function(root, config, inventory, temp_dir, result = NULL) {
  con <- open_audit_connection(temp_dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  checks <- list()
  record <- function(name, passed, detail) {
    checks[[name]] <<- list(passed = isTRUE(passed), detail = detail)
    cat(sprintf("%s: %s\n", name, if (isTRUE(passed)) "PASS" else "FAIL"))
  }
  paths <- file.path(root, inventory$path)
  record("nonempty_files", all(inventory$bytes > 0),
         list(files = nrow(inventory), zero_bytes = sum(inventory$bytes == 0)))
  record("known_row_counts", all(!is.na(inventory$rows)),
         list(missing_counts = sum(is.na(inventory$rows))))

  core <- paths[grepl("plidage-core-demog", inventory$asset) &
                  !grepl("--", inventory$asset)]
  record("core_present", length(core) > 0, list(files = length(core)))
  if (!length(core)) return(list(passed = FALSE, checks = checks))
  population <- read_asset(con, core) |>
    dplyr::select(spine_id) |>
    dplyr::mutate(spine_id = as.character(spine_id))
  population_counts <- population |>
    dplyr::summarise(rows = dplyr::n(), people = dplyr::n_distinct(spine_id),
                      missing = sum(is.na(spine_id) | spine_id == "")) |>
    dplyr::collect()
  record("core_population_grain",
         population_counts$rows == config$n &&
           population_counts$people == config$n && population_counts$missing == 0,
         as.list(population_counts))
  population <- population |> dplyr::distinct(spine_id) |>
    dplyr::compute(name = "qa_population", temporary = TRUE)

  if (identical(config$products, "all") || !is.null(result)) {
    coverage <- build_product_coverage(result, inventory)
    record("requested_build_products", coverage$passed, coverage)
    if (identical(config$products, "all")) {
      registered <- c("spine", "blade", names(fplida:::.dil_build_target_datasets))
      record("all_registered_build_products", setequal(result$products, registered),
             list(missing = setdiff(registered, result$products),
                  unexpected = setdiff(result$products, registered)))
    }
  } else {
    missing_families <- setdiff(config$products, unique(inventory$family))
    record("requested_asset_families", !length(missing_families),
           list(missing = missing_families))
  }

  spine_indices <- which(grepl("^[a-z]+-spine(?:-v6)?$", inventory$asset))
  agency_names <- sub("-spine(?:-v6)?$", "", inventory$asset[spine_indices], perl = TRUE)
  spine_groups <- split(paths[spine_indices], agency_names)
  required_spines <- c("abs", "ato", "dss", "dhda", "de", "ncver", "ha", "ndia", "rbdm")
  record("required_agency_spines", all(required_spines %in% names(spine_groups)),
         list(required = required_spines, found = names(spine_groups)))
  lookups <- list()
  linked_lookups <- list()
  purrr::iwalk(spine_groups, function(files, agency) {
    if (agency == "business") return(invisible(NULL))
    raw <- read_asset(con, files)
    if (!all(c("spine_id", "synthetic_aeuid") %in% colnames(raw))) {
      record(paste0("spine_columns_", agency), FALSE, list(files = files))
      return(invisible(NULL))
    }
    # Repeated copies of an agency lookup can exist in Parquet domain folders.
    lookup <- raw |>
      dplyr::select(spine_id, synthetic_aeuid) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::filter(!is.na(spine_id), spine_id != "",
                    !is.na(synthetic_aeuid), synthetic_aeuid != "") |>
      dplyr::distinct() |>
      dplyr::compute(name = paste0("qa_spine_", agency), temporary = TRUE)
    counts <- lookup |>
      dplyr::summarise(rows = dplyr::n(), people = dplyr::n_distinct(spine_id),
                        agency_ids = dplyr::n_distinct(synthetic_aeuid)) |>
      dplyr::collect()
    foreign <- lookup |>
      dplyr::anti_join(population, by = "spine_id") |>
      dplyr::summarise(rows = dplyr::n()) |> dplyr::collect()
    record(paste0("spine_grain_", agency),
           counts$rows == counts$people && counts$rows == counts$agency_ids &&
             foreign$rows == 0,
           c(as.list(counts), list(ids_outside_core = as.numeric(foreign$rows))))
    linked_lookups[[agency]] <<- lookup |> dplyr::select(synthetic_aeuid)
    lookups[[agency]] <<- raw |>
      dplyr::select(synthetic_aeuid) |>
      dplyr::mutate(synthetic_aeuid = as.character(synthetic_aeuid)) |>
      dplyr::filter(!is.na(synthetic_aeuid), synthetic_aeuid != "") |>
      dplyr::distinct() |>
      dplyr::compute(name = paste0("qa_agency_ids_", agency), temporary = TRUE)
  })

  family_agency <- c(census = "abs", pit_ps = "ato", pit_itr = "ato",
                      stp = "ato", busown = "ato", domino = "dss", mbs = "dhda",
                      pbs = "dhda", he = "de", tva = "ncver", visa = "ha",
                      ndis = "ndia", deaths = "rbdm", lfs = "abs")
  asset_groups <- split(seq_len(nrow(inventory)), inventory$asset)
  linkage <- list()
  purrr::iwalk(asset_groups, function(indices, asset) {
    family <- unique(inventory$family[indices])
    agency <- unname(family_agency[family])
    if (length(agency) != 1L || is.na(agency)) return(invisible(NULL))
    data <- read_asset(con, paths[indices])
    if (!"synthetic_aeuid" %in% colnames(data)) return(invisible(NULL))
    if (is.null(lookups[[agency]])) return(invisible(NULL))
    data <- data |> dplyr::select(synthetic_aeuid) |>
      dplyr::mutate(synthetic_aeuid = as.character(synthetic_aeuid))
    if (!qa_links_full) data <- utils::head(data, qa_link_limit)
    present <- data |>
      dplyr::filter(!is.na(synthetic_aeuid), synthetic_aeuid != "")
    counts <- present |>
      dplyr::summarise(records = dplyr::n(), ids = dplyr::n_distinct(synthetic_aeuid)) |>
      dplyr::collect()
    foreign <- present |>
      dplyr::anti_join(lookups[[agency]], by = "synthetic_aeuid") |>
      dplyr::summarise(records = dplyr::n()) |> dplyr::collect()
    linked <- present |>
      dplyr::semi_join(linked_lookups[[agency]], by = "synthetic_aeuid") |>
      dplyr::summarise(records = dplyr::n()) |> dplyr::collect()
    linkage[[asset]] <<- list(agency = agency, records_checked = as.numeric(counts$records),
                             ids_checked = as.numeric(counts$ids),
                             records_linked_to_core = as.numeric(linked$records),
                             unmatched_records = as.numeric(foreign$records))
  })
  unmatched <- sum(vapply(linkage, function(x) x$unmatched_records, numeric(1)))
  record("record_agency_links", unmatched == 0 && length(linkage) > 0L,
         list(assets_checked = length(linkage), unmatched_records = unmatched,
              mode = if (qa_links_full) "all_rows" else "first_rows_per_asset",
              row_limit_per_asset = if (qa_links_full) NULL else qa_link_limit))

  periods <- audit_reporting_periods(root, inventory, temp_dir)
  unparsed_periods <- periods |> dplyr::filter(nonmissing_unparsed_years > 0)
  record("reporting_fields_parse", nrow(unparsed_periods) == 0L,
         list(assets = unparsed_periods))
  business <- NULL
  if (identical(config$products, "all") || "blade" %in% config$products) {
    business <- check_business_links(root, config, inventory, temp_dir)
    record("business_links", business$passed, business)
  }
  if (config$name == "30m") {
    health <- unique(inventory$asset[inventory$family %in% c("mbs", "pbs")])
    primary_health <- health[!grepl("--", health)]
    record("health_2024_only", length(primary_health) >= 2L &&
             all(grepl("2024$", primary_health)), list(assets = primary_health))
    out_of_window <- periods |>
      dplyr::filter(!is.na(observed_year_min), observed_year_min < 2010L)
    record("reporting_years_2010_onwards", nrow(out_of_window) == 0L,
           list(assets_outside_window = out_of_window,
                assets_with_observed_years = sum(!is.na(periods$observed_year_min)),
                basis = "Calendar reporting years or financial ending years; historical attributes excluded"))
    health_periods <- periods |> dplyr::filter(asset %in% primary_health)
    record("health_observed_year_2024", nrow(health_periods) >= 2L &&
             all(health_periods$observed_year_min == 2024L &
                   health_periods$observed_year_max == 2024L), health_periods)
  }
  list(passed = all(vapply(checks, function(x) x$passed, logical(1))),
       checks = checks, agency_links = linkage, business_links = business,
       reporting_periods = periods,
       checked_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
}
