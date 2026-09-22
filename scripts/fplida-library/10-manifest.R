# The file inventory comes from finished output, not requested products.
asset_name <- function(path) {
  stem <- tools::file_path_sans_ext(basename(path))
  if (grepl("^part[-_]", stem)) stem <- basename(dirname(path))
  if (identical(stem, "business-spine-v6")) "business-spine" else stem
}

asset_family <- function(asset) {
  dplyr::case_when(
    grepl("spine|plida-blade-link|blade-key", asset) ~ "linking",
    grepl("^blade-", asset) ~ "blade",
    grepl("stp[_-]", asset) ~ "stp",
    grepl("mbs", asset) ~ "mbs",
    grepl("pbs", asset) ~ "pbs",
    grepl("cen[0-9]|census", asset) ~ "census",
    grepl("^(plidage-core|madipge-core|core_)", asset) ~ "core",
    grepl("dom[-_]|domino", asset) ~ "domino",
    grepl("hied|student|higher-education", asset) ~ "he",
    grepl("tva|trn-actvty|prog-comp", asset) ~ "tva",
    grepl("ndis", asset) ~ "ndis",
    grepl("death|cause-of-death", asset) ~ "deaths",
    grepl("mig-d-visa|visa", asset) ~ "visa",
    grepl("pmp-(llfs|coes)|^llfs|^coes", asset) ~ "lfs",
    grepl("partnership|sole.trader|business.owner", asset) ~ "busown",
    grepl("pay.sum|atopaysum|ps_geo", asset) ~ "pit_ps",
    grepl("inc.loss|context|ded.exp.off|whld.debt", asset) ~ "pit_itr",
    TRUE ~ "other"
  )
}

filename_period <- function(asset) {
  fy <- regmatches(asset, regexpr("fy[0-9]{4}", asset))
  if (length(fy) && nzchar(fy)) {
    start <- as.integer(substr(fy, 3L, 4L))
    start <- if (start >= 70L) start + 1900L else start + 2000L
    return(paste0("FY", start, "-", start + 1L))
  }
  monthly <- regmatches(asset, regexpr("20[0-9]{2}_m[0-9]{2}", asset))
  if (length(monthly) && nzchar(monthly)) return(sub("_m", "-", monthly))
  fiscal <- regmatches(asset, regexpr("20[0-9]{2}_[0-9]{2}$", asset))
  if (length(fiscal) && nzchar(fiscal)) return(paste0("FY", sub("_", "-", fiscal)))
  period <- regmatches(asset, regexpr("(?:19|20)[0-9]{2}(?:-(?:latest|current|20[0-9]{2}))?$",
                                      asset, perl = TRUE))
  if (length(period) && nzchar(period)) period else NA_character_
}

inventory_library <- function(root, result = NULL, temp_dir) {
  root <- normalizePath(root, mustWork = TRUE)
  files <- sort(list.files(root, pattern = "\\.(csv|parquet)$", recursive = TRUE,
                            full.names = TRUE, ignore.case = TRUE))
  files <- files[!grepl("/(file-manifest|asset-manifest)\\.csv$", files)]
  if (!length(files)) stop("No data files in ", root)
  symlinks <- files[nzchar(Sys.readlink(files))]
  if (length(symlinks)) stop("Output contains external symlinks: ", symlinks[1L])
  info <- file.info(files)
  inventory <- tibble::tibble(
    path = substring(files, nchar(root) + 2L), asset = vapply(files, asset_name, ""),
    format = tolower(tools::file_ext(files)), bytes = as.numeric(info$size),
    rows = NA_real_, modified_unix = as.numeric(info$mtime),
    modified_utc = format(info$mtime, tz = "UTC", usetz = TRUE)
  )
  con <- open_audit_connection(temp_dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  parquet <- which(inventory$format == "parquet")
  if (length(parquet)) {
    # Row counts from Parquet footers avoid reading event columns.
    metadata <- DBI::dbGetQuery(con, paste0(
      "SELECT file_name, num_rows FROM parquet_file_metadata(",
      sql_files(files[parquet]), ")"
    ))
    inventory$rows[parquet] <- as.numeric(metadata$num_rows[
      match(files[parquet], metadata$file_name)
    ])
  }
  conversions <- result$csv_conversion$per_file
  if (is.data.frame(conversions) && nrow(conversions)) {
    matches <- match(inventory$path, as.character(conversions$path))
    known <- which(!is.na(matches) & inventory$format == "csv")
    inventory$rows[known] <- conversions$n_rows[matches[known]]
  }
  unknown <- which(is.na(inventory$rows))
  purrr::walk(unknown, function(i) {
    count <- read_asset(con, files[i]) |> dplyr::summarise(rows = dplyr::n()) |>
      dplyr::collect()
    inventory$rows[i] <<- as.numeric(count$rows)
  })
  inventory |>
    dplyr::mutate(family = asset_family(asset),
                  filename_period = vapply(asset, filename_period, ""))
}

readme_year_ranges <- function(observed_years, fallback) {
  if (is.na(observed_years) || !nzchar(observed_years)) return(fallback)
  years <- sort(unique(as.integer(strsplit(observed_years, ",\\s*")[[1L]])))
  groups <- split(years, cumsum(c(TRUE, diff(years) != 1L)))
  paste(vapply(groups, function(group) {
    if (length(group) == 1L) as.character(group) else paste(range(group), collapse = "-")
  }, character(1)), collapse = ", ")
}

write_library_manifest <- function(root, config, provenance, inventory, qa) {
  assets <- inventory |>
    dplyr::group_by(family, asset, format, filename_period) |>
    dplyr::summarise(files = dplyr::n(), rows = sum(rows), bytes = sum(bytes),
                      .groups = "drop") |>
    dplyr::left_join(qa$reporting_periods, by = "asset") |>
    dplyr::arrange(family, asset)
  inventory <- inventory |> dplyr::left_join(qa$reporting_periods, by = "asset")
  utils::write.csv(inventory, file.path(root, "file-manifest.csv"), row.names = FALSE)
  utils::write.csv(assets, file.path(root, "asset-manifest.csv"), row.names = FALSE)
  manifest <- list(
    schema_version = 1L, profile = config, provenance = provenance,
    data_files = nrow(inventory), assets = nrow(assets),
    data_bytes = sum(inventory$bytes), data_rows = sum(inventory$rows),
    formats = as.list(table(inventory$format)), qa = qa,
    period_basis = "Observed reporting fields where present; otherwise labelled file periods. Financial years use the ending year. Historical attributes can predate reporting years.",
    asset_manifest = "asset-manifest.csv", file_manifest = "file-manifest.csv"
  )
  json_write_atomic(manifest, file.path(root, "manifest.json"))
  asset_lines <- purrr::pmap_chr(assets, function(asset, format, rows, bytes,
                                                reporting_period, reporting_fields,
                                                observed_years, ...) {
    sprintf("| `%s` | %s | %s | %s | %s |", asset,
            readme_year_ranges(observed_years, reporting_period),
            format, base::format(rows, scientific = FALSE, big.mark = ","),
            base::format(bytes, scientific = FALSE, big.mark = ","))
  })
  overview <- c(
    paste0("# fplida synthetic data: ", config$name), "",
    "These files contain synthetic data. They contain no confidential PLIDA or BLADE records.", "",
    paste0("- fplida version: `", provenance$version, "`."),
    paste0("- Source commit: `", provenance$git_sha, "`."),
    paste0("- Build date: `", provenance$built_at_utc, "`."),
    paste0("- Person population: ", format(config$n, big.mark = ","), "."),
    paste0("- Seed: `", config$seed, "`."),
    paste0("- Requested years: `", min(config$years), "-", max(config$years), "`."),
    paste0("- Data files: ", nrow(inventory), "."),
    paste0("- Data size: ", format(sum(inventory$bytes), scientific = FALSE,
                                    big.mark = ","), " bytes."), "",
    "## Files and years", "",
    "Each row below identifies one asset. Reporting years come from the data where a reporting field exists.",
    "The asset manifest names each checked field. File labels identify periods when no reporting field exists.",
    "Financial years use the ending year. For example, 2010 means FY2009-2010 for financial-year fields.",
    "",
    "Birth dates, arrival dates and historical spell dates can precede the reporting period.",
    "CORE residence uses `pp_period`. CORE location spell start dates can precede the residence reporting period.",
    if (config$name == "30m") "MBS and PBS contain 2024 only. Other reporting periods start in 2010 or later." else NULL,
    if (config$name == "1k") paste0(
      "The build includes all products. Schema companions contain at most ",
      config$complete_dil_rows, " rows per table."
    ) else NULL,
    if (config$name == "1k") "Some schema companions contain typed missing values. They are not validated statistical models." else NULL,
    "", "| Asset | Reporting years or file period | Format | Rows | Bytes |",
    "|---|---|---|---:|---:|", asset_lines, "",
    "## Links and checks", "",
    "Use the agency spine to link an agency identifier to `spine_id`.",
    "Exclude missing identifiers before a join. Check the grain before each join.",
    "",
    "The manifest records the coverage and results of the checks.",
    "The build-product report identifies generated products, empty tables and products with no eligible output.",
    "Parquet row counts come from file metadata. CSV row counts come from conversion records or a full count.",
    paste0("Record-to-spine checks: ", if (qa_links_full) "all rows." else
      paste0("up to ", format(qa_link_limit, big.mark = ","), " rows per asset.")),
    "", "## Reproduce the build", "",
    "Use the source commit and package version above.",
    "Run this command from the package source folder:", "",
    "```sh", paste0("Rscript scripts/build_fplida_library.R --profile=", config$name,
                      " --action=build"), "```", "",
    "The build first writes to a separate work folder. Publication requires completed checks.",
    "Build logs and previous data folders remain outside this data folder.", ""
  )
  writeLines(overview, file.path(root, "README.md"))
  invisible(manifest)
}
