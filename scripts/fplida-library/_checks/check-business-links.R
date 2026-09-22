# Business keys are lookups; job, owner and pay-event records can repeat them.
check_business_links <- function(root, config, inventory, temp_dir) {
  con <- open_audit_connection(temp_dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  checks <- links <- list()
  record <- function(name, passed, detail) {
    checks[[name]] <<- list(passed = isTRUE(passed), detail = detail)
  }
  groups <- split(seq_len(nrow(inventory)), inventory$asset)
  read_group <- function(indices) read_asset(con, file.path(root, inventory$path[indices]))
  count_rows <- function(data) {
    data |> dplyr::summarise(rows = dplyr::n()) |> dplyr::collect() |>
      dplyr::pull(rows) |> as.numeric()
  }
  required <- c("business-spine", "plida-blade-link", "blade-key-abn-hash-trunc-to-bn-key")
  missing <- setdiff(required, names(groups))
  record("business_link_assets", !length(missing), list(missing = missing))
  if (length(missing)) return(list(passed = FALSE, checks = checks,
                                   business_links = links, coverage = list()))

  business <- read_group(groups[["business-spine"]]) |>
    dplyr::transmute(bn = as.character(bn))
  business_counts <- business |> dplyr::summarise(
    rows = dplyr::n(), businesses = dplyr::n_distinct(bn),
    missing = sum(is.na(bn) | bn == "")) |> dplyr::collect()
  record("business_spine_grain", business_counts$rows > 0 &
           business_counts$rows == business_counts$businesses & business_counts$missing == 0,
         c(as.list(business_counts), list(mode = "all_rows")))
  business <- business |> dplyr::filter(!is.na(bn), bn != "") |>
    dplyr::distinct(bn) |> dplyr::compute(name = "qa_businesses", temporary = TRUE)

  mapping <- read_group(groups[[required[3L]]]) |>
    dplyr::transmute(abn_hash_trunc = as.character(abn_hash_trunc), bn = as.character(bn))
  mapping_counts <- mapping |> dplyr::summarise(
    rows = dplyr::n(), hashes = dplyr::n_distinct(abn_hash_trunc),
    businesses = dplyr::n_distinct(bn),
    missing = sum(is.na(bn) | bn == "" | is.na(abn_hash_trunc) | abn_hash_trunc == "")) |>
    dplyr::collect()
  unknown <- count_rows(dplyr::anti_join(mapping, business, by = "bn"))
  unmapped <- count_rows(dplyr::anti_join(business, mapping, by = "bn"))
  record("business_hash_mapping", mapping_counts$missing == 0 &
           mapping_counts$rows == mapping_counts$hashes &
           mapping_counts$rows == mapping_counts$businesses & unknown == 0 & unmapped == 0,
         c(as.list(mapping_counts), list(unknown_businesses = unknown,
           unmapped_businesses = unmapped, mode = "all_rows")))
  mapping <- mapping |> dplyr::filter(!is.na(abn_hash_trunc), abn_hash_trunc != "") |>
    dplyr::distinct(abn_hash_trunc, bn) |>
    dplyr::compute(name = "qa_business_hashes", temporary = TRUE)
  core <- which(grepl("plidage-core-demog", inventory$asset) & !grepl("--", inventory$asset))
  record("business_link_population_present", length(core) > 0L, list(files = length(core)))
  if (!length(core)) return(list(passed = FALSE, checks = checks,
                                 business_links = links, coverage = list()))
  population <- read_group(core) |> dplyr::transmute(spine_id = as.character(spine_id)) |>
    dplyr::filter(!is.na(spine_id), spine_id != "") |> dplyr::distinct(spine_id) |>
    dplyr::compute(name = "qa_business_people", temporary = TRUE)
  lookup <- list(bn = business |> dplyr::transmute(key = bn),
    abn_hash_trunc = mapping |> dplyr::transmute(key = abn_hash_trunc) |> dplyr::distinct(),
    spine_id = population |> dplyr::transmute(key = spine_id))
  lookup$cntrctr_bn <- lookup$bn
  mode <- if (qa_links_full) "all_rows" else "limited_rows_per_asset"

  purrr::iwalk(groups, function(indices, asset) {
    if (asset %in% c("business-spine", required[3L])) return(invisible(NULL))
    data <- read_group(indices)
    person_link <- identical(asset, "plida-blade-link")
    columns <- intersect(c("bn", "cntrctr_bn", "abn_hash_trunc",
                            if (person_link) "spine_id"), colnames(data))
    if (person_link) record("person_business_link_columns",
      all(c("spine_id", "bn") %in% columns), list(found = columns))
    if (!length(columns)) return(invisible(NULL))
    grain <- if (person_link) c("spine_id", "bn", "relationship_type", "job_number") else character()
    data <- data |> dplyr::select(dplyr::all_of(unique(c(columns, intersect(grain, colnames(data)))))) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    if (!qa_links_full) data <- utils::head(data, qa_link_limit) |> dplyr::compute(temporary = TRUE)
    detail <- list(asset_rows = sum(inventory$rows[indices]), mode = mode,
      row_limit = if (qa_links_full) NULL else qa_link_limit, keys = list())
    purrr::walk(columns, function(column) {
      summary <- data |> dplyr::transmute(key = .data[[column]]) |>
        dplyr::left_join(dplyr::mutate(lookup[[column]], matched = 1L), by = "key") |>
        dplyr::summarise(records_checked = dplyr::n(), ids_checked = dplyr::n_distinct(key),
          missing_records = sum(is.na(key) | key == ""),
          unmatched_records = sum(!is.na(key) & key != "" & is.na(matched))) |>
        dplyr::collect() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::coalesce(.x, 0)))
      detail$keys[[column]] <<- as.list(summary)
    })
    if (person_link && all(grain %in% colnames(data))) {
      rows <- count_rows(data)
      unique_jobs <- count_rows(data |> dplyr::distinct(!!!rlang::syms(grain)))
      record("person_business_link_grain", rows == unique_jobs,
        list(records_checked = rows, unique_relationship_jobs = unique_jobs,
             grain = grain, mode = mode))
    }
    # A row carrying both eras' identifiers must name the same business.
    if (all(c("bn", "abn_hash_trunc") %in% columns)) {
      mismatched <- data |> dplyr::filter(!is.na(bn), bn != "",
        !is.na(abn_hash_trunc), abn_hash_trunc != "") |>
        dplyr::anti_join(mapping, by = c("abn_hash_trunc", "bn")) |> count_rows()
      detail$mismatched_identifier_pairs <- mismatched
    }
    links[[asset]] <<- detail
  })
  unmatched <- sum(vapply(links, function(x) sum(vapply(x$keys,
    function(key) as.numeric(key$unmatched_records), numeric(1))), numeric(1)))
  mismatched <- sum(vapply(links, function(x) {
    if (is.null(x$mismatched_identifier_pairs)) 0 else x$mismatched_identifier_pairs
  }, numeric(1)))
  person_missing <- sum(vapply(links[["plida-blade-link"]]$keys,
    function(key) as.numeric(key$missing_records), numeric(1)))
  coverage <- list(mode = mode, row_limit_per_asset = if (qa_links_full) NULL else qa_link_limit,
    lookup_mode = "all_rows", assets_inspected = length(groups),
    assets_with_business_keys = length(links),
    families_checked = unique(inventory$family[inventory$asset %in% names(links)]),
    note = "Repeated businesses and people are allowed in jobs, ownership and events. Missing optional record keys are counted separately. Limited mode is not a random sample.")
  record("record_business_links", unmatched == 0 & mismatched == 0 & person_missing == 0,
    list(unmatched_records = unmatched, mismatched_identifier_pairs = mismatched,
         missing_person_business_keys = person_missing, coverage = coverage))
  list(passed = all(vapply(checks, function(x) x$passed, logical(1))),
       checks = checks, business_links = links, coverage = coverage)
}
