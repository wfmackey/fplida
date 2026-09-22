.dil_agency_id_columns <- function(dataset, columns) {
  keys <- "SYNTHETIC_AEUID"
  if (identical(dataset, "BIRTHS")) keys <- c(keys, "SYTHETIC_AEUID")
  if (identical(dataset, "CENSUS")) keys <- c(keys, "C11_PERSON_ID")
  columns[toupper(columns) %in% keys]
}

# Schema companions can include an agency record without a tax lodgement.
# Complete their lookups from the identities actually written, without changing
# the primary generators or treating an existing unlinked record as linked.
.dil_reconcile_agency_lookups <- function(run_dir, datasets) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("duckdb", quietly = TRUE)) {
    stop("Complete DIL linkage checks require packages 'DBI' and 'duckdb'.",
         call. = FALSE)
  }
  base_path <- file.path(run_dir, "_system", "base-spine.parquet")
  stage <- tempfile(".dil-links-", tmpdir = dirname(base_path))
  dir.create(stage)
  con <- NULL
  on.exit({
    if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(stage, recursive = TRUE)
  }, add = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, "SET memory_limit = '1GB'")
  DBI::dbExecute(con, "SET threads = 1")
  DBI::dbExecute(con, paste0("SET temp_directory = ",
    DBI::dbQuoteString(con, file.path(stage, "spill"))))
  literal <- function(x) as.character(DBI::dbQuoteString(con, x))
  identifier <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  scan <- function(paths) paste0("read_parquet([",
    paste(literal(paths), collapse = ","), "], union_by_name = true)")
  execute <- function(sql) DBI::dbExecute(con, sql)
  query <- function(sql) DBI::dbGetQuery(con, sql)
  assert_empty <- function(sql, message) {
    bad <- query(paste0(sql, " LIMIT 1"))
    if (nrow(bad)) stop(message, ": ", as.character(bad[[1L]][[1L]]),
                         call. = FALSE)
  }
  id <- '"SYNTHETIC_AEUID"'
  spine <- 'NULLIF(CAST("spine_id" AS VARCHAR), \'\')'
  datasets <- unique(datasets)
  agencies <- vapply(datasets, dataset_to_agency, character(1))
  all_lookup_paths <- list.files(run_dir, recursive = TRUE, full.names = TRUE,
    pattern = "^[a-z]+-spine\\.parquet$")
  staged <- list()
  report <- list()

  for (agency in unique(agencies)) {
    selected <- datasets[agencies == agency]
    execute("CREATE OR REPLACE TEMP TABLE emitted (dataset VARCHAR, aeuid VARCHAR)")
    for (dataset in selected) {
      files <- list.files(dataset_dir(run_dir, dataset), recursive = TRUE,
        full.names = TRUE, pattern = "\\.parquet$")
      files <- files[!grepl("^[a-z]+-spine\\.parquet$", basename(files))]
      key_names <- lapply(files, function(path) {
        names <- arrow::ParquetFileReader$create(path, mmap = FALSE)$GetSchema()$names
        keys <- .dil_agency_id_columns(dataset, names)
        if (anyDuplicated(toupper(keys))) {
          stop("Ambiguous agency ID column: ", path, call. = FALSE)
        }
        keys
      })
      for (key in unique(unlist(key_names, use.names = FALSE))) {
        key_files <- files[vapply(key_names, function(keys) key %in% keys, logical(1))]
        execute(paste0("INSERT INTO emitted SELECT DISTINCT ", literal(dataset),
          ", CAST(", identifier(key), " AS VARCHAR) FROM ",
          scan(key_files), " WHERE ", identifier(key),
          " IS NOT NULL AND CAST(", identifier(key), " AS VARCHAR) <> ''"))
      }
    }
    if (!query("SELECT count(*) AS n FROM emitted")$n) next

    aeuid_column <- paste0("aeuid_", tolower(agency))
    base_columns <- arrow::ParquetFileReader$create(base_path, mmap = FALSE)$GetSchema()$names
    if (!all(c("spine_id", aeuid_column) %in% base_columns)) {
      stop("Canonical spine lacks identity columns for ", agency, call. = FALSE)
    }
    execute(paste0("CREATE OR REPLACE TEMP TABLE identities AS SELECT ",
      "row_number() OVER () AS position, ", spine, " AS spine_id, CAST(",
      identifier(aeuid_column), " AS VARCHAR) AS aeuid FROM ", scan(base_path)))
    assert_empty("SELECT aeuid FROM identities WHERE aeuid IS NOT NULL AND aeuid <> '' GROUP BY aeuid HAVING count(*) > 1",
      paste0("Duplicate canonical ", agency, " identity"))
    assert_empty("SELECT e.aeuid FROM emitted e LEFT JOIN identities b ON e.aeuid = b.aeuid WHERE b.aeuid IS NULL OR b.spine_id IS NULL",
      paste0("Unresolvable emitted ", agency, " identity"))

    sources <- all_lookup_paths[basename(all_lookup_paths) ==
      paste0(tolower(agency), "-spine.parquet")]
    existing <- if (length(sources)) {
      paste0("SELECT CAST(", id, " AS VARCHAR) AS aeuid, ", spine,
        " AS spine_id FROM ", scan(sources))
    } else "SELECT NULL::VARCHAR AS aeuid, NULL::VARCHAR AS spine_id WHERE false"
    execute(paste0("CREATE OR REPLACE TEMP TABLE existing AS ", existing))
    assert_empty("SELECT aeuid FROM existing GROUP BY aeuid HAVING count(DISTINCT spine_id) > 1",
      paste0("Conflicting existing ", agency, " links"))
    assert_empty("SELECT e.aeuid FROM existing e LEFT JOIN identities b ON e.aeuid = b.aeuid WHERE e.spine_id IS NOT NULL AND (b.spine_id IS NULL OR e.spine_id <> b.spine_id)",
      paste0("Existing ", agency, " link conflicts with canonical spine"))
    execute("CREATE OR REPLACE TEMP TABLE known AS SELECT aeuid, max(spine_id) AS spine_id FROM existing GROUP BY aeuid")
    n <- as.integer(query("SELECT count(*) AS n FROM identities")$n)
    DBI::dbWriteTable(con, "unlinked_positions", data.frame(
      position = which(!.agency_linkage_mask(n, agency))), overwrite = TRUE)

    for (dataset in selected) {
      path <- file.path(dataset_dir(run_dir, dataset),
        paste0(tolower(agency), "-spine.parquet"))
      current <- if (file.exists(path)) {
        paste0("SELECT CAST(", id, " AS VARCHAR) AS aeuid, ", spine,
          " AS spine_id FROM ", scan(path))
      } else "SELECT NULL::VARCHAR AS aeuid, NULL::VARCHAR AS spine_id WHERE false"
      execute(paste0("CREATE OR REPLACE TEMP TABLE current_lookup AS ", current))
      assert_empty("SELECT aeuid FROM current_lookup GROUP BY aeuid HAVING count(*) > 1",
        paste0("Duplicate ", dataset, " agency lookup identity"))
      execute(paste0("CREATE OR REPLACE TEMP TABLE additions AS SELECT DISTINCT e.aeuid, ",
        "CASE WHEN k.aeuid IS NOT NULL THEN k.spine_id ",
        "WHEN u.position IS NOT NULL THEN NULL ELSE b.spine_id END AS spine_id ",
        "FROM emitted e JOIN identities b ON e.aeuid = b.aeuid ",
        "LEFT JOIN known k ON e.aeuid = k.aeuid ",
        "LEFT JOIN unlinked_positions u ON b.position = u.position ",
        "LEFT JOIN current_lookup c ON e.aeuid = c.aeuid ",
        "WHERE e.dataset = ", literal(dataset), " AND c.aeuid IS NULL"))
      added <- as.numeric(query("SELECT count(*) AS n FROM additions")$n)
      report[[dataset]] <- list(agency = agency, added = added, path = path)
      if (!added) next
      temporary <- file.path(stage, paste0(dataset, ".parquet"))
      execute(paste0("COPY (SELECT spine_id, aeuid AS ", id,
        " FROM current_lookup UNION ALL SELECT spine_id, aeuid AS ", id,
        " FROM additions) TO ", literal(temporary),
        " (FORMAT PARQUET, COMPRESSION SNAPPY)"))
      staged[[path]] <- temporary
    }
  }
  # Validate and stage every lookup before replacing any existing file. Closing
  # DuckDB also releases input handles before replacement on Windows.
  DBI::dbDisconnect(con, shutdown = TRUE)
  con <- NULL
  for (path in names(staged)) {
    if (!file.rename(staged[[path]], path)) {
      stop("Could not publish completed agency lookup: ", path, call. = FALSE)
    }
  }
  report
}
