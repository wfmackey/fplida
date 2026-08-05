#!/usr/bin/env Rscript

# Reproducible schema and value audit for PLIDA and BLADE data.
#
# The script audits an existing fplida run directory by default. It can also
# create a small fixed-seed build before auditing it. Every generated product
# remains in the schema audit. Value fidelity is deferred for LFS, NHS, NSMHW,
# PEX, SDAC and the BLADE survey tables. Census, ACLD and AEDC remain in the
# administrative value audit.

.audit_survey_datasets <- c("LFS", "NHS", "NSMHW", "PEX", "SDAC")
.audit_default_seed <- 20260803L

.audit_empty <- function(columns) {
  out <- as.data.frame(setNames(rep(list(character()), length(columns)),
                                columns),
                       stringsAsFactors = FALSE, check.names = FALSE)
  out
}

.audit_bind_rows <- function(rows, columns = NULL) {
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    if (is.null(columns)) return(data.frame())
    return(.audit_empty(columns))
  }
  all_columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    missing <- setdiff(all_columns, names(x))
    for (name in missing) x[[name]] <- rep(NA, nrow(x))
    x[all_columns]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.audit_normalise_name <- function(x) {
  tolower(gsub("[^[:alnum:]]+", "-", trimws(as.character(x))))
}

.audit_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    script <- normalizePath(sub("^--file=", "", file_arg[[1L]]),
                            mustWork = FALSE)
    return(dirname(dirname(script)))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

.audit_read_metadata <- function(repo_root = .audit_repo_root()) {
  plida_dir <- file.path(repo_root, "inst", "plida_metadata")
  blade_dir <- file.path(repo_root, "inst", "blade_metadata")
  required <- c(
    file.path(plida_dir, "datasets.csv"),
    file.path(plida_dir, "products.csv"),
    file.path(plida_dir, "variables.csv"),
    file.path(blade_dir, "tables.csv"),
    file.path(blade_dir, "variables.csv")
  )
  missing <- required[!file.exists(required)]
  if (length(missing)) {
    stop("Metadata files not found: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  read_meta <- function(path) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                    na.strings = character())
  }
  datasets <- read_meta(required[[1L]])
  products <- read_meta(required[[2L]])
  variables <- read_meta(required[[3L]])
  blade_tables <- read_meta(required[[4L]])
  blade_variables <- read_meta(required[[5L]])

  schema_datasets <- sort(unique(datasets[["Dataset Acronym"]]))
  admin_datasets <- sort(setdiff(schema_datasets, .audit_survey_datasets))
  survey_products <- unique(products[["Product Name"]][
    products[["Dataset"]] %in% .audit_survey_datasets
  ])

  blade_tables[["Source.Type"]] <- .audit_blade_source_type(
    blade_tables[["Table.Name"]]
  )

  list(
    datasets = datasets,
    schema_datasets = schema_datasets,
    admin_datasets = admin_datasets,
    survey_products = survey_products,
    products = products,
    variables = variables,
    blade_tables = blade_tables,
    blade_variables = blade_variables
  )
}

.audit_blade_source_type <- function(table_name) {
  # These terms identify the ABS survey collections in the published table
  # names. They reproduce the package's source classification (tables 8--23
  # and 38--46) without excluding tables merely because a value check fails.
  survey_name <- grepl(
    paste(
      "survey",
      "expenditure on research and development",
      "employee earning and hours",
      "capital expenditure",
      "business conditions and sentiments",
      "higher education research.*development",
      "agricultural commodities",
      sep = "|"
    ),
    table_name, ignore.case = TRUE, perl = TRUE
  )
  ifelse(survey_name, "survey", "administrative")
}

.audit_discover_files <- function(run_dir) {
  run_dir <- normalizePath(run_dir, mustWork = TRUE)
  files <- list.files(run_dir, recursive = TRUE, full.names = TRUE,
                      pattern = "\\.(parquet|csv)$", ignore.case = TRUE)
  if (!length(files)) return(character())
  relative <- substring(normalizePath(files, mustWork = TRUE),
                        nchar(run_dir) + 2L)
  components <- strsplit(relative, .Platform$file.sep, fixed = TRUE)
  keep <- !vapply(components, function(x) {
    any(x %in% c("_system", "L-drive")) ||
      grepl("-spine\\.(parquet|csv)$", x[[length(x)]], ignore.case = TRUE) ||
      grepl("^blade-key-", x[[length(x)]], ignore.case = TRUE)
  }, logical(1))
  sort(files[keep])
}

.audit_file_index <- function(run_dir, files = .audit_discover_files(run_dir)) {
  if (!length(files)) {
    return(.audit_empty(c(
      "source_file", "relative_file", "dataset_folder", "partition",
      "logical_output"
    )))
  }
  run_dir <- normalizePath(run_dir, mustWork = TRUE)
  relative <- substring(normalizePath(files, mustWork = TRUE),
                        nchar(run_dir) + 2L)
  components <- strsplit(relative, .Platform$file.sep, fixed = TRUE)
  stem <- tools::file_path_sans_ext(basename(files))
  # A partition can be part-000, part-000-001, or a period partition such as
  # part-202501. In each case, its parent directory is the output name.
  is_part <- grepl("^part(?:-[0-9]+)+$", stem, ignore.case = TRUE,
                   perl = TRUE)
  logical_output <- ifelse(is_part, basename(dirname(files)), stem)
  dataset_folder <- vapply(components, `[[`, character(1), 1L)
  data.frame(
    source_file = normalizePath(files, mustWork = TRUE),
    relative_file = relative,
    dataset_folder = dataset_folder,
    partition = ifelse(is_part, stem, NA_character_),
    logical_output = logical_output,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.audit_read_column_names <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to audit parquet files.",
           call. = FALSE)
    }
    return(arrow::ParquetFileReader$create(path)$GetSchema()$names)
  }
  names(utils::read.csv(path, nrows = 0L, check.names = FALSE))
}

.audit_infer_dataset <- function(dataset_folder, metadata) {
  folder_suffix <- sub("^[^-]+-", "", dataset_folder)
  folder_key <- .audit_normalise_name(folder_suffix)
  dataset <- unique(metadata$datasets[["Dataset Acronym"]])
  hits <- dataset[.audit_normalise_name(dataset) == folder_key]
  if (length(hits)) hits[[1L]] else NA_character_
}

.audit_year_tokens <- function(text) {
  text <- paste(text, collapse = " ")
  years <- regmatches(text, gregexpr("(?:19|20)[0-9]{2}", text,
                                     perl = TRUE))[[1L]]
  if (identical(years, "")) years <- character()

  long_fy <- regmatches(
    text,
    gregexpr("((?:19|20)[0-9]{2})[-_/]([0-9]{2})(?![0-9])", text,
             perl = TRUE)
  )[[1L]]
  if (!identical(long_fy, "")) {
    for (value in long_fy) {
      start_year <- as.integer(substr(value, 1L, 4L))
      end_short <- as.integer(sub(".*[-_/]", "", value))
      end_year <- 100L * (start_year %/% 100L) + end_short
      if (end_year < start_year) end_year <- end_year + 100L
      years <- c(years, end_year)
    }
  }
  sort(unique(as.integer(years)))
}

.audit_financial_periods <- function(text) {
  text <- paste(text, collapse = " ")
  periods <- character()
  add_period <- function(start_year, end_short) {
    end_year <- 100L * (start_year %/% 100L) + end_short
    if (end_year < start_year) end_year <- end_year + 100L
    if (end_year == start_year + 1L) {
      periods <<- c(
        periods,
        sprintf("%04d-%02d", start_year, end_year %% 100L)
      )
    }
  }

  long <- regmatches(
    text,
    gregexpr("(?<![0-9])(?:19|20)[0-9]{2}[-_/][0-9]{2}(?![0-9])",
             text, perl = TRUE)
  )[[1L]]
  if (!identical(long, "")) {
    for (value in long) {
      add_period(
        as.integer(substr(value, 1L, 4L)),
        as.integer(sub(".*[-_/]", "", value))
      )
    }
  }

  short <- regmatches(
    text,
    gregexpr("(?<![0-9])[0-9]{2}(?:[-_/]?[0-9]{2})(?![0-9])",
             text, perl = TRUE)
  )[[1L]]
  if (!identical(short, "")) {
    for (value in short) {
      digits <- gsub("[^0-9]", "", value)
      start_short <- as.integer(substr(digits, 1L, 2L))
      start_year <- if (start_short >= 80L) {
        1900L + start_short
      } else {
        2000L + start_short
      }
      add_period(start_year, as.integer(substr(digits, 3L, 4L)))
    }
  }
  unique(periods)
}

.audit_period_score <- function(output_periods, candidate_periods) {
  if (!length(output_periods) || !length(candidate_periods)) return(0)
  if (length(intersect(output_periods, candidate_periods))) return(100000)
  output_start <- as.integer(substr(output_periods, 1L, 4L))
  candidate_start <- as.integer(substr(candidate_periods, 1L, 4L))
  distance <- min(abs(outer(output_start, candidate_start, `-`)))
  max(0, 10000 - 100 * distance)
}

.audit_year_score <- function(output_years, candidate_years) {
  if (!length(output_years) || !length(candidate_years)) return(0)
  distance <- min(abs(outer(output_years, candidate_years, `-`)))
  if (distance == 0L) return(100000)
  max(0, 10000 - 100 * distance)
}

.audit_name_tokens <- function(text) {
  tokens <- unlist(strsplit(.audit_normalise_name(paste(text, collapse = " ")),
                            "-", fixed = TRUE), use.names = FALSE)
  stop_tokens <- c(
    "madip", "madipge", "plida", "plidage", "pmp", "ato", "abs", "d",
    "ge", "monthly", "current", "latest", "fy", "v7", "v8", "part"
  )
  unique(tokens[nzchar(tokens) & !tokens %in% stop_tokens &
                  !grepl("^[0-9]+$", tokens)])
}

.audit_table_match <- function(product_rows, logical_output, relative_file,
                               generated_variables, dataset,
                               force_table = FALSE) {
  tables <- unique(product_rows[["Table Name"]])
  tables <- tables[!is.na(tables) & nzchar(tables)]
  if (!length(tables)) return(NA_character_)
  if (length(tables) == 1L) return(tables[[1L]])

  generated_key <- unique(toupper(generated_variables))
  match_text <- paste(logical_output, relative_file)
  output_tokens <- .audit_name_tokens(match_text)
  output_years <- .audit_year_tokens(match_text)
  financial_dataset <- dataset %in% c(
    "BUSOWN", "CGT", "PIT_IE", "PIT_ITR", "PIT_PS", "RPS", "SAE", "SMSF"
  )
  output_periods <- if (financial_dataset) {
    .audit_financial_periods(match_text)
  } else {
    character()
  }
  scores <- vapply(tables, function(table_name) {
    table_rows <- product_rows[product_rows[["Table Name"]] == table_name,
                               , drop = FALSE]
    official_key <- unique(toupper(table_rows[["Variable Name"]]))
    overlap <- length(intersect(generated_key, official_key))
    precision <- overlap / max(1L, length(generated_key))
    table_tokens <- .audit_name_tokens(table_name)
    token_score <- length(intersect(output_tokens, table_tokens)) /
      max(1L, length(union(output_tokens, table_tokens)))
    table_years <- .audit_year_tokens(table_name)
    table_periods <- if (financial_dataset) {
      .audit_financial_periods(table_name)
    } else {
      character()
    }
    version_score <- if (grepl(
      "(?:^|[_-])v8(?:$|[_-])", table_name,
      ignore.case = TRUE, perl = TRUE
    )) 1 else 0
    1000 * precision + 5 * overlap + 100 * token_score +
      .audit_period_score(output_periods, table_periods) +
      .audit_year_score(output_years, table_years) + version_score
  }, numeric(1))
  best_indices <- which(scores == max(scores))
  if (length(best_indices) > 1L) return(NA_character_)
  best <- tables[[best_indices[[1L]]]]

  if (!force_table) {
    union_key <- unique(toupper(product_rows[["Variable Name"]]))
    union_overlap <- length(intersect(generated_key, union_key))
    best_key <- unique(toupper(product_rows[["Variable Name"]][
      product_rows[["Table Name"]] == best
    ]))
    best_overlap <- length(intersect(generated_key, best_key))
    # An exact product file can intentionally combine several official tables.
    if (union_overlap > best_overlap &&
        union_overlap / max(1L, length(generated_key)) >= 0.8) {
      return(NA_character_)
    }
  }
  best
}

.audit_table_suffix_match <- function(product, logical_output, tables) {
  prefix <- paste0(product, "--")
  if (!startsWith(logical_output, prefix)) return(NA_character_)

  suffix <- substring(logical_output, nchar(prefix) + 1L)
  exact <- tables[tables == suffix]
  if (length(exact) == 1L) return(exact[[1L]])

  # Most existing logical outputs replace table-name underscores with hyphens.
  # Use that compatibility form only when it identifies one official table.
  # Exact matching remains necessary for the STP table pairs that differ only
  # by a hyphen versus an underscore.
  normalised <- tables[
    .audit_normalise_name(tables) == .audit_normalise_name(suffix)
  ]
  if (length(normalised) == 1L) normalised[[1L]] else NA_character_
}

.audit_map_one_output <- function(logical_output, metadata,
                                  relative_file = logical_output,
                                  dataset_folder = NA_character_,
                                  generated_variables = character()) {
  blade_products <- unique(metadata$blade_tables[["Product.Name"]])
  if (logical_output %in% blade_products) {
    table_row <- metadata$blade_tables[
      metadata$blade_tables[["Product.Name"]] == logical_output,
      , drop = FALSE
    ][1L, , drop = FALSE]
    return(data.frame(
      asset = "BLADE",
      dataset = "BLADE",
      official_product = logical_output,
      official_table = as.character(table_row[["Table.Name"]]),
      table_number = as.character(table_row[["Table.Number"]]),
      source_type = as.character(table_row[["Source.Type"]]),
      value_scope = if (table_row[["Source.Type"]] == "survey") {
        "deferred_survey"
      } else {
        "audited"
      },
      mapping_status = "exact_product",
      schema_role = "canonical_structure",
      stringsAsFactors = FALSE
    ))
  }

  plida_products <- unique(metadata$products[["Product Name"]])
  product <- if (logical_output %in% plida_products) {
    logical_output
  } else {
    candidates <- plida_products[
      startsWith(logical_output, paste0(plida_products, "--"))
    ]
    if (length(candidates)) candidates[[which.max(nchar(candidates))]] else NA_character_
  }
  inferred_dataset <- .audit_infer_dataset(dataset_folder, metadata)
  if (is.na(product) && !is.na(inferred_dataset)) {
    candidate_products <- unique(metadata$products[["Product Name"]][
      metadata$products[["Dataset"]] == inferred_dataset
    ])
    generated_key <- unique(toupper(generated_variables))
    match_text <- paste(logical_output, relative_file)
    output_tokens <- .audit_name_tokens(match_text)
    output_years <- .audit_year_tokens(match_text)
    financial_dataset <- inferred_dataset %in% c(
      "BUSOWN", "CGT", "PIT_IE", "PIT_ITR", "PIT_PS", "RPS", "SAE", "SMSF"
    )
    output_periods <- if (financial_dataset) {
      .audit_financial_periods(match_text)
    } else {
      character()
    }
    product_scores <- vapply(candidate_products, function(candidate) {
      rows <- metadata$variables[
        metadata$variables[["Dataset"]] == inferred_dataset &
          metadata$variables[["Product Name"]] == candidate,
        , drop = FALSE
      ]
      official_key <- unique(toupper(rows[["Variable Name"]]))
      overlap <- length(intersect(generated_key, official_key))
      precision <- overlap / max(1L, length(generated_key))
      candidate_rows <- metadata$products[
        metadata$products[["Dataset"]] == inferred_dataset &
          metadata$products[["Product Name"]] == candidate,
        , drop = FALSE
      ]
      name_text <- c(candidate, candidate_rows[["Module Name"]],
                     unique(rows[["Table Name"]]))
      candidate_tokens <- .audit_name_tokens(name_text)
      token_score <- length(intersect(output_tokens, candidate_tokens)) /
        max(1L, length(union(output_tokens, candidate_tokens)))
      candidate_years <- .audit_year_tokens(name_text)
      candidate_periods <- if (financial_dataset) {
        .audit_financial_periods(name_text)
      } else {
        character()
      }
      1000 * precision + 5 * overlap + 100 * token_score +
        .audit_period_score(output_periods, candidate_periods) +
        .audit_year_score(output_years, candidate_years)
    }, numeric(1))
    if (length(product_scores)) {
      best_index <- which.max(product_scores)
      best_product <- candidate_products[[best_index]]
      best_rows <- metadata$variables[
        metadata$variables[["Dataset"]] == inferred_dataset &
          metadata$variables[["Product Name"]] == best_product,
        , drop = FALSE
      ]
      overlap <- length(intersect(generated_key,
                                  unique(toupper(best_rows[["Variable Name"]]))))
      name_overlap <- length(intersect(
        output_tokens,
        .audit_name_tokens(c(best_product, unique(best_rows[["Table Name"]])))
      ))
      if (overlap > 0L || name_overlap >= 2L) product <- best_product
    }
  }
  if (is.na(product)) {
    value_scope <- if (!is.na(inferred_dataset) &&
                       inferred_dataset %in% .audit_survey_datasets) {
      "deferred_survey"
    } else {
      "audited"
    }
    return(data.frame(
      asset = "unmapped", dataset = inferred_dataset,
      official_product = NA_character_, official_table = NA_character_,
      table_number = NA_character_,
      source_type = if (value_scope == "deferred_survey") {
        "survey"
      } else {
        NA_character_
      },
      value_scope = value_scope, mapping_status = "unmapped_output",
      schema_role = "unmapped",
      stringsAsFactors = FALSE
    ))
  }

  product_rows <- metadata$products[
    metadata$products[["Product Name"]] == product, , drop = FALSE
  ]
  dataset <- unique(product_rows[["Dataset"]])
  dataset <- if (length(dataset)) dataset[[1L]] else NA_character_
  variable_rows <- metadata$variables[
    metadata$variables[["Dataset"]] == dataset &
      metadata$variables[["Product Name"]] == product,
    , drop = FALSE
  ]
  tables <- unique(variable_rows[["Table Name"]])
  tables <- tables[!is.na(tables) & nzchar(tables)]
  exact_product <- identical(product, logical_output)
  table_suffix <- startsWith(logical_output, paste0(product, "--"))
  if (exact_product) {
    official_table <- if (length(tables) == 1L) tables[[1L]] else NA_character_
    mapping_status <- "exact_product"
    schema_role <- if (length(tables) == 1L) {
      "auxiliary_exact_product"
    } else {
      "auxiliary_product"
    }
  } else if (table_suffix) {
    official_table <- .audit_table_suffix_match(
      product, logical_output, tables
    )
    suffix <- substring(logical_output, nchar(product) + 3L)
    exact_table_suffix <- !is.na(official_table) && any(tables == suffix)
    mapping_status <- if (exact_table_suffix) {
      "product_with_table_suffix"
    } else if (!is.na(official_table)) {
      "normalised_table_suffix"
    } else {
      "unmatched_table_suffix"
    }
    schema_role <- if (exact_table_suffix) {
      "canonical_structure"
    } else if (!is.na(official_table)) {
      "auxiliary_table_alias"
    } else {
      "unmatched_table_suffix"
    }
  } else {
    official_table <- .audit_table_match(
      variable_rows, logical_output, relative_file, generated_variables,
      dataset = dataset, force_table = TRUE
    )
    mapping_status <- "metadata_schema_match"
    schema_role <- "auxiliary_metadata_match"
  }
  value_scope <- if (dataset %in% .audit_survey_datasets) {
    "deferred_survey"
  } else {
    "audited"
  }

  data.frame(
    asset = "PLIDA", dataset = dataset, official_product = product,
    official_table = official_table, table_number = NA_character_,
    source_type = if (value_scope == "deferred_survey") "survey" else "administrative",
    value_scope = value_scope,
    mapping_status = mapping_status, schema_role = schema_role,
    stringsAsFactors = FALSE
  )
}

.audit_map_outputs <- function(file_index, metadata) {
  rows <- lapply(seq_len(nrow(file_index)), function(i) {
    file_row <- file_index[i, , drop = FALSE]
    generated_variables <- .audit_read_column_names(file_row[["source_file"]])
    cbind(file_row,
          .audit_map_one_output(
            logical_output = file_row[["logical_output"]],
            metadata = metadata,
            relative_file = file_row[["relative_file"]],
            dataset_folder = file_row[["dataset_folder"]],
            generated_variables = generated_variables
          ),
          stringsAsFactors = FALSE)
  })
  .audit_bind_rows(rows, c("source_file", "relative_file", "dataset_folder",
                          "partition", "logical_output", "asset", "dataset",
                          "official_product", "official_table",
                          "table_number", "source_type", "value_scope",
                          "mapping_status", "schema_role"))
}

.audit_expected_variables <- function(mapping, metadata) {
  if (mapping[["asset"]] == "BLADE") {
    rows <- metadata$blade_variables[
      metadata$blade_variables[["Product.Name"]] ==
        mapping[["official_product"]], , drop = FALSE
    ]
    return(data.frame(
      variable = rows[["Variable.Name"]],
      description = rows[["Item"]],
      valid_response = rows[["Valid.Response"]],
      available_periods = rows[["Available.Periods"]],
      stringsAsFactors = FALSE
    ))
  }
  if (mapping[["asset"]] != "PLIDA") {
    return(.audit_empty(c("variable", "description", "valid_response",
                          "available_periods")))
  }
  rows <- metadata$variables[
    metadata$variables[["Dataset"]] == mapping[["dataset"]] &
      metadata$variables[["Product Name"]] == mapping[["official_product"]],
    , drop = FALSE
  ]
  table_name <- mapping[["official_table"]]
  if (!is.na(table_name) && nzchar(table_name)) {
    rows <- rows[rows[["Table Name"]] == table_name, , drop = FALSE]
  }
  data.frame(
    variable = rows[["Variable Name"]],
    description = rows[["Variable Description"]],
    valid_response = NA_character_,
    available_periods = NA_character_,
    stringsAsFactors = FALSE
  )
}

.audit_read_frame <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to audit parquet files.",
           call. = FALSE)
    }
    return(as.data.frame(arrow::read_parquet(path),
                         stringsAsFactors = FALSE, check.names = FALSE))
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = character())
}

.audit_collapse <- function(x, limit = 12L) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)
  paste(utils::head(sort(x), limit), collapse = " | ")
}

.audit_domain_summary <- function(x) {
  if (!length(x)) return(NA_character_)
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    values <- x[!is.na(x)]
    if (!length(values)) return("all missing")
    return(paste0(format(min(values)), " to ", format(max(values))))
  }
  if (is.numeric(x) || is.integer(x)) {
    values <- x[!is.na(x)]
    if (!length(values)) return("all missing")
    q <- stats::quantile(values, probs = c(0, .25, .5, .75, 1),
                         na.rm = TRUE, names = FALSE, type = 7)
    return(paste(format(q, trim = TRUE, scientific = FALSE),
                 collapse = " | "))
  }
  values <- as.character(x)
  values <- values[!is.na(values)]
  if (!length(values)) return("all missing")
  counts <- table(values, useNA = "no")
  order_index <- order(-as.integer(counts), names(counts))
  counts <- counts[order_index]
  shown <- utils::head(counts, 12L)
  paste0(names(shown), "=", as.integer(shown), collapse = " | ")
}

.audit_parse_valid_codes <- function(text) {
  text <- paste(unique(text[!is.na(text) & nzchar(text)]), collapse = " ")
  if (!nzchar(text)) return(character())
  # Equality text is not always a complete code frame. For example,
  # "4 digit numeric; 0 = unknown" defines a sentinel inside a much larger
  # domain. Quarter, postcode, year, date, amount and free-text descriptions
  # have the same problem. Only a self-contained categorical enumeration is
  # safe to enforce as an exhaustive domain.
  structural_domain <- grepl(
    paste(
      "\\b(?:numeric|number|character|alphanumeric|text|date|postcode|",
      "quarter|financial year|calendar year|year|dollar|amount|count|",
      "percentage|percent|decimal|digit|range)\\b|\\bup to\\b|",
      "\\bexample\\b|\\be\\.g\\.",
      sep = ""
    ),
    text, ignore.case = TRUE, perl = TRUE
  )
  if (structural_domain) return(character())
  # Only equality-delimited enumerations are safe to parse automatically.
  # Hyphens also occur in dates, financial years and prose ranges, so using
  # them as code delimiters creates false domain failures.
  pattern <- "(?:^|[[:space:];,/])['\"]?([[:alnum:].]{1,10})['\"]?[[:space:]]*="
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]]
  if (!length(matches) || identical(matches, "")) return(character())
  codes <- sub(pattern, "\\1", matches, perl = TRUE)
  # Some BLADE cells put a quoted sentinel after the final labelled code,
  # for example: '1' = Yes '0' = No 'N/A'. Preserve those explicit terminal
  # domain members without treating unquoted prose as a code.
  sentinel_matches <- regmatches(
    text,
    gregexpr("['\"](?:N/A|UNKNOWN)['\"]", text,
             ignore.case = TRUE, perl = TRUE)
  )[[1L]]
  if (length(sentinel_matches) && !identical(sentinel_matches, "")) {
    sentinels <- gsub("['\"]", "", sentinel_matches)
    codes <- c(codes, sentinels)
  }
  excluded <- c("and", "or", "blank", "example", "missing",
                "digit", "numeric", "character", "alphanumeric")
  codes <- unique(codes[!tolower(codes) %in% excluded])
  if (length(codes) < 2L || any(grepl("Y{2,}|X{2,}", codes))) {
    return(character())
  }
  codes
}

.audit_value_findings <- function(x, column, dataset, source_file,
                                  logical_output, valid_response = NA_character_) {
  rows <- list()
  add <- function(type, severity, matched, example, detail) {
    if (matched <= 0L) return(NULL)
    rows[[length(rows) + 1L]] <<- data.frame(
      source_file = source_file,
      logical_output = logical_output,
      dataset = dataset,
      variable = column,
      finding_type = type,
      severity = severity,
      matched_values = as.numeric(matched),
      example = as.character(example),
      detail = detail,
      stringsAsFactors = FALSE
    )
  }

  values <- as.character(x)
  nonmissing <- values[!is.na(values)]
  if (!length(nonmissing)) {
    return(.audit_empty(c("source_file", "logical_output", "dataset",
                          "variable", "finding_type", "severity",
                          "matched_values", "example", "detail")))
  }
  codes <- .audit_parse_valid_codes(valid_response)

  if (is.character(x) || is.factor(x)) {
    checks <- list(
      explicit_placeholder = grepl(
        "(^|[_ -])(placeholder|dummy|fake|lorem|todo|tbd)([_ -]|$)",
        nonmissing, ignore.case = TRUE, perl = TRUE
      ),
      dataset_counter_placeholder = grepl(
        "^[[:alpha:]][[:alnum:]&]*_[0-9]{6,}$", nonmissing,
        ignore.case = TRUE, perl = TRUE
      ),
      template_marker = grepl("\\{\\{|\\$\\{|<placeholder", nonmissing,
                              ignore.case = TRUE, perl = TRUE)
    )
    for (type in names(checks)) {
      hit <- checks[[type]]
      severity <- "error"
      add(type, severity, sum(hit), if (any(hit)) nonmissing[which(hit)[1L]] else NA,
          "Explicit placeholder-like value.")
    }
    generic_code <- grepl("^C[0-9]{2,4}$", nonmissing,
                          ignore.case = TRUE, perl = TRUE)
    documented_code <- toupper(nonmissing) %in% toupper(codes)
    deaths_icd_column <- identical(toupper(dataset), "DEATHS") && grepl(
      "^(?:UCOD|ENTITY[0-9]+|RACS[0-9]+)$",
      toupper(column), perl = TRUE
    )
    undocumented_generic <- generic_code & !documented_code &
      !deaths_icd_column
    add(
      "generic_c_code", "error", sum(undocumented_generic),
      if (any(undocumented_generic)) {
        nonmissing[which(undocumented_generic)[1L]]
      } else {
        NA_character_
      },
      "Generic Cnn/Cnnn code is not documented in an exhaustive official domain."
    )
    if (any(generic_code & documented_code)) {
      add(
        "documented_c_code", "warning", sum(generic_code & documented_code),
        nonmissing[which(generic_code & documented_code)[1L]],
        "Cnn/Cnnn code is documented in the exhaustive official domain."
      )
    }
  }

  upper_column <- toupper(column)
  if (grepl("YEAR|(^|_)YR($|_)", upper_column, perl = TRUE) &&
      all(nonmissing == "2024")) {
    add("generic_fixed_year", "warning", length(nonmissing), "2024",
        "All non-missing year values use the lightweight fallback year.")
  }
  if (grepl("FIN_YEAR|FNCL_YR|FINANCIAL_YEAR|PYRL_FNCL_YR", upper_column) &&
      all(nonmissing == "2023-24")) {
    add("generic_fixed_financial_year", "warning", length(nonmissing),
        "2023-24",
        "All non-missing financial-year values use the lightweight fallback.")
  }
  if (grepl("DATE|_DT$|START|END|CMNCMT|CESTN", upper_column, perl = TRUE)) {
    date_text <- substr(nonmissing, 1L, 10L)
    iso_date <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date_text)
    parsed <- if (all(iso_date)) as.Date(date_text) else as.Date(character())
    if (length(parsed) && all(!is.na(parsed)) &&
        all(format(parsed, "%Y") == "2024")) {
      add("generic_2024_date_range", "warning", length(nonmissing),
          format(min(parsed)),
          "All parseable dates fall in 2024; verify the source reference period.")
    }
  }
  dataset_token <- gsub("[^[:alnum:]]", "", toupper(dataset))
  if (!is.na(dataset_token) && nzchar(dataset_token) &&
      grepl("KEY|ID|IDENT|NUMBER|NMBR|SEQ", upper_column) &&
      all(grepl(paste0("^", dataset_token, "[0-9]{8}$"),
                toupper(nonmissing)))) {
    add("dataset_counter_identifier", "warning", length(nonmissing),
        nonmissing[[1L]],
        "All identifiers are dataset-name counters from generic fallback logic.")
  }

  if (length(codes) >= 2L) {
    observed <- unique(nonmissing[nzchar(nonmissing)])
    outside <- setdiff(observed, codes)
    if (length(outside)) {
      add("official_domain_mismatch", "error", length(outside), outside[[1L]],
          paste0("Observed values outside conservatively parsed BLADE codes: ",
                 paste(utils::head(codes, 20L), collapse = " | ")))
    }
  }

  .audit_bind_rows(rows, c("source_file", "logical_output", "dataset",
                           "variable", "finding_type", "severity",
                           "matched_values", "example", "detail"))
}

.audit_structure_key <- function(asset, dataset, product, table) {
  paste(asset, dataset, product, table, sep = "::")
}

.audit_required_structures <- function(metadata) {
  variables <- metadata$variables
  plida <- unique(variables[
    c("Dataset", "Module Name", "Product Name", "Table Name")
  ])
  product_key <- paste(plida[["Dataset"]], plida[["Product Name"]],
                       sep = "\r")
  table_counts <- table(product_key)
  product_table_count <- as.integer(table_counts[product_key])
  plida_rows <- data.frame(
    asset = "PLIDA",
    dataset = plida[["Dataset"]],
    module = plida[["Module Name"]],
    official_product = plida[["Product Name"]],
    official_table = plida[["Table Name"]],
    table_number = NA_character_,
    source_type = ifelse(
      plida[["Dataset"]] %in% .audit_survey_datasets,
      "survey", "administrative"
    ),
    value_scope = ifelse(
      plida[["Dataset"]] %in% .audit_survey_datasets,
      "deferred_survey", "audited"
    ),
    product_table_count = product_table_count,
    required_logical_output = paste0(
      plida[["Product Name"]], "--", plida[["Table Name"]]
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  blade <- metadata$blade_tables
  blade_rows <- data.frame(
    asset = "BLADE",
    dataset = "BLADE",
    module = NA_character_,
    official_product = blade[["Product.Name"]],
    official_table = blade[["Table.Name"]],
    table_number = as.character(blade[["Table.Number"]]),
    source_type = blade[["Source.Type"]],
    value_scope = ifelse(
      blade[["Source.Type"]] == "survey", "deferred_survey", "audited"
    ),
    product_table_count = 1L,
    required_logical_output = blade[["Product.Name"]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  structures <- .audit_bind_rows(list(plida_rows, blade_rows))
  structures[["structure_key"]] <- .audit_structure_key(
    structures[["asset"]], structures[["dataset"]],
    structures[["official_product"]], structures[["official_table"]]
  )
  structures
}

.audit_product_coverage <- function(output_map, metadata) {
  structures <- .audit_required_structures(metadata)
  canonical <- output_map[
    output_map[["schema_role"]] == "canonical_structure",
    , drop = FALSE
  ]
  canonical_key <- .audit_structure_key(
    canonical[["asset"]], canonical[["dataset"]],
    canonical[["official_product"]], canonical[["official_table"]]
  )
  observed <- lapply(structures[["structure_key"]], function(key) {
    unique(canonical[["logical_output"]][canonical_key == key])
  })
  structures[["coverage_status"]] <- ifelse(
    lengths(observed) > 0L, "observed", "missing"
  )
  structures[["observed_outputs"]] <- vapply(
    observed, .audit_collapse, character(1), limit = 1000L
  )
  structures
}

.audit_one_file <- function(file_row, mapping, metadata) {
  frame <- .audit_read_frame(file_row[["source_file"]])
  expected <- if (mapping[["schema_role"]] %in% c(
    "canonical_structure", "auxiliary_exact_product",
    "auxiliary_table_alias"
  )) {
    .audit_expected_variables(mapping, metadata)
  } else {
    .audit_empty(c("variable", "description", "valid_response",
                   "available_periods"))
  }
  expected_key <- toupper(expected[["variable"]])
  generated <- names(frame)
  summary_rows <- list()
  finding_rows <- list()
  for (i in seq_along(generated)) {
    variable <- generated[[i]]
    hits <- which(expected_key == toupper(variable))
    valid_response <- if (length(hits)) {
      .audit_collapse(expected[["valid_response"]][hits], limit = 1000L)
    } else {
      NA_character_
    }
    x <- frame[[i]]
    missing <- sum(is.na(x))
    summary_rows[[i]] <- data.frame(
      source_file = file_row[["relative_file"]],
      logical_output = file_row[["logical_output"]],
      asset = mapping[["asset"]], dataset = mapping[["dataset"]],
      official_product = mapping[["official_product"]],
      official_table = mapping[["official_table"]],
      source_type = mapping[["source_type"]],
      value_scope = mapping[["value_scope"]],
      schema_role = mapping[["schema_role"]],
      variable = variable,
      type = paste(class(x), collapse = "/"),
      n_rows = nrow(frame), missing = missing,
      missing_pct = if (length(x)) round(100 * missing / length(x), 6) else NA_real_,
      distinct = length(unique(x[!is.na(x)])),
      domain_or_distribution = .audit_domain_summary(x),
      official_variable = if (length(hits)) expected[["variable"]][hits[[1L]]] else NA_character_,
      official_description = if (length(hits)) .audit_collapse(expected[["description"]][hits], 1000L) else NA_character_,
      official_valid_response = valid_response,
      schema_status = if (length(hits)) "generated_and_official" else "generated_only",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (mapping[["value_scope"]] == "audited") {
      finding_rows[[i]] <- .audit_value_findings(
        x = x, column = variable, dataset = mapping[["dataset"]],
        source_file = file_row[["relative_file"]],
        logical_output = file_row[["logical_output"]],
        valid_response = valid_response
      )
    }
  }
  list(
    variable_summary = .audit_bind_rows(summary_rows),
    findings = .audit_bind_rows(finding_rows)
  )
}

.audit_column_coverage <- function(output_map, variable_summary, metadata) {
  coverage_columns <- c(
    "structure_key", "logical_output", "asset", "dataset",
    "official_product", "official_table", "source_type", "value_scope",
    "mapped_files", "generated_variable", "official_variable",
    "coverage_status"
  )
  rows <- list()
  canonical_map <- output_map[
    output_map[["schema_role"]] == "canonical_structure",
    , drop = FALSE
  ]
  unit_columns <- c("asset", "dataset", "official_product",
                    "official_table", "source_type", "value_scope",
                    "schema_role")
  mapping_units <- unique(canonical_map[unit_columns])
  for (i in seq_len(nrow(mapping_units))) {
    mapping <- mapping_units[i, , drop = FALSE]
    expected <- .audit_expected_variables(mapping, metadata)
    same_mapping <- rep(TRUE, nrow(canonical_map))
    for (name in unit_columns) {
      same_mapping <- same_mapping &
        ((is.na(canonical_map[[name]]) & is.na(mapping[[name]])) |
           (!is.na(canonical_map[[name]]) & !is.na(mapping[[name]]) &
              canonical_map[[name]] == mapping[[name]]))
    }
    mapped_files <- canonical_map[["relative_file"]][same_mapping]
    mapped_outputs <- unique(canonical_map[["logical_output"]][same_mapping])
    generated <- unique(variable_summary[["variable"]][
      variable_summary[["source_file"]] %in% mapped_files
    ])
    official <- unique(expected[["variable"]])
    official <- official[!is.na(official) & nzchar(official)]
    all_keys <- union(toupper(generated), toupper(official))
    if (!length(all_keys)) next
    rows[[length(rows) + 1L]] <- .audit_bind_rows(lapply(all_keys, function(key) {
      generated_name <- generated[toupper(generated) == key]
      official_name <- official[toupper(official) == key]
      data.frame(
        structure_key = .audit_structure_key(
          mapping[["asset"]], mapping[["dataset"]],
          mapping[["official_product"]], mapping[["official_table"]]
        ),
        logical_output = .audit_collapse(mapped_outputs, limit = 1000L),
        asset = mapping[["asset"]], dataset = mapping[["dataset"]],
        official_product = mapping[["official_product"]],
        official_table = mapping[["official_table"]],
        source_type = mapping[["source_type"]],
        value_scope = mapping[["value_scope"]],
        mapped_files = .audit_collapse(mapped_files, limit = 1000L),
        generated_variable = if (length(generated_name)) generated_name[[1L]] else NA_character_,
        official_variable = if (length(official_name)) official_name[[1L]] else NA_character_,
        coverage_status = if (length(generated_name) && length(official_name)) {
          "generated_and_official"
        } else if (length(generated_name)) {
          "generated_only"
        } else {
          "official_only"
        },
        stringsAsFactors = FALSE
      )
    }))
  }
  .audit_bind_rows(rows, coverage_columns)
}

.audit_write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

.audit_read_allowlist <- function(path = NULL) {
  columns <- c("issue_type", "key", "reason")
  if (is.null(path)) return(.audit_empty(columns))
  if (!file.exists(path)) stop("Allowlist not found: ", path, call. = FALSE)
  allowlist <- utils::read.csv(path, stringsAsFactors = FALSE,
                               check.names = FALSE, na.strings = character())
  missing <- setdiff(columns, names(allowlist))
  if (length(missing)) {
    stop("Allowlist must contain columns: ", paste(columns, collapse = ", "),
         call. = FALSE)
  }
  allowlist <- allowlist[columns]
  undocumented <- is.na(allowlist[["reason"]]) |
    !nzchar(trimws(allowlist[["reason"]]))
  if (any(undocumented)) {
    stop("Every allowlist row must include a non-empty reason.", call. = FALSE)
  }
  unique(allowlist)
}

.audit_all_missing_admin_variables <- function(variable_summary) {
  columns <- c("key", "detail")
  required <- c(
    "logical_output", "asset", "dataset", "official_product",
    "official_table", "value_scope", "schema_role", "variable",
    "n_rows", "missing"
  )
  if (!nrow(variable_summary) ||
      !all(required %in% names(variable_summary))) {
    return(.audit_empty(columns))
  }

  in_scope <-
    !is.na(variable_summary[["schema_role"]]) &
    variable_summary[["schema_role"]] == "canonical_structure" &
    !is.na(variable_summary[["value_scope"]]) &
    variable_summary[["value_scope"]] == "audited" &
    !is.na(variable_summary[["n_rows"]]) &
    !is.na(variable_summary[["missing"]])
  scoped <- variable_summary[in_scope, , drop = FALSE]
  if (!nrow(scoped)) return(.audit_empty(columns))

  # Aggregate physical parts before applying the gate. A variable is all
  # missing only when it has no value anywhere in its canonical logical output.
  group_key <- paste(
    scoped[["asset"]], scoped[["dataset"]],
    scoped[["official_product"]], scoped[["official_table"]],
    scoped[["logical_output"]], toupper(scoped[["variable"]]),
    sep = "\r"
  )
  groups <- split(seq_len(nrow(scoped)), group_key)
  rows <- lapply(groups, function(index) {
    n_rows <- sum(as.numeric(scoped[["n_rows"]][index]))
    missing <- sum(as.numeric(scoped[["missing"]][index]))
    if (n_rows <= 0 || missing != n_rows) return(NULL)

    first <- index[[1L]]
    structure_key <- .audit_structure_key(
      scoped[["asset"]][first], scoped[["dataset"]][first],
      scoped[["official_product"]][first],
      scoped[["official_table"]][first]
    )
    data.frame(
      key = paste(structure_key, scoped[["logical_output"]][first],
                  scoped[["variable"]][first], sep = "::"),
      detail = paste(
        scoped[["logical_output"]][first], scoped[["dataset"]][first],
        scoped[["official_product"]][first],
        scoped[["official_table"]][first], scoped[["variable"]][first],
        paste0(missing, "/", n_rows, " values missing"),
        sep = " | "
      ),
      stringsAsFactors = FALSE
    )
  })
  .audit_bind_rows(rows, columns)
}

.audit_unresolved_admin_variables <- function(variable_summary) {
  columns <- c(
    "asset", "dataset", "official_product", "official_table",
    "logical_output", "variable", "type", "n_rows",
    "official_description", "official_valid_response"
  )
  required <- c(columns, "value_scope", "schema_role", "missing")
  if (!nrow(variable_summary) ||
      !all(required %in% names(variable_summary))) {
    return(.audit_empty(columns))
  }

  in_scope <-
    !is.na(variable_summary[["schema_role"]]) &
    variable_summary[["schema_role"]] == "canonical_structure" &
    !is.na(variable_summary[["value_scope"]]) &
    variable_summary[["value_scope"]] == "audited" &
    !is.na(variable_summary[["n_rows"]]) &
    variable_summary[["n_rows"]] > 0 &
    !is.na(variable_summary[["missing"]]) &
    variable_summary[["missing"]] == variable_summary[["n_rows"]]
  unresolved <- variable_summary[in_scope, columns, drop = FALSE]
  if (!nrow(unresolved)) return(.audit_empty(columns))

  text_columns <- intersect(
    c("official_description", "official_valid_response"),
    names(unresolved)
  )
  for (column in text_columns) {
    unresolved[[column]] <- trimws(gsub(
      "[\\r\\n]+", " ", unresolved[[column]], perl = TRUE
    ))
  }

  unresolved <- unresolved[order(
    unresolved[["asset"]], unresolved[["dataset"]],
    unresolved[["official_product"]], unresolved[["official_table"]],
    unresolved[["logical_output"]], toupper(unresolved[["variable"]]),
    na.last = TRUE
  ), , drop = FALSE]
  rownames(unresolved) <- NULL
  unresolved
}

.audit_gate_failures <- function(product_coverage, column_coverage,
                                 output_map, value_findings,
                                 variable_summary = data.frame(),
                                 allowlist = .audit_read_allowlist()) {
  rows <- list()
  add_rows <- function(issue_type, key, detail) {
    if (!length(key)) return(NULL)
    rows[[length(rows) + 1L]] <<- data.frame(
      issue_type = issue_type, key = as.character(key),
      detail = as.character(detail), stringsAsFactors = FALSE
    )
  }

  missing_products <- product_coverage[
    product_coverage[["coverage_status"]] == "missing", , drop = FALSE
  ]
  add_rows(
    "missing_product_or_table", missing_products[["structure_key"]],
    paste(missing_products[["asset"]], missing_products[["dataset"]],
          missing_products[["official_table"]], sep = " | ")
  )
  missing_variables <- column_coverage[
    column_coverage[["coverage_status"]] == "official_only", , drop = FALSE
  ]
  add_rows(
    "missing_official_variable",
    paste(missing_variables[["structure_key"]],
          missing_variables[["official_variable"]], sep = "::"),
    paste(missing_variables[["logical_output"]],
          missing_variables[["dataset"]],
          missing_variables[["official_product"]], sep = " | ")
  )
  unmapped <- output_map[
    output_map[["mapping_status"]] %in%
      c("unmapped_output", "unmatched_table_suffix"),
    , drop = FALSE
  ]
  unmapped <- unique(unmapped[c("logical_output")])
  add_rows("unmapped_output", unmapped[["logical_output"]],
           rep("Generated output has no official administrative metadata match.",
               nrow(unmapped)))
  errors <- value_findings[value_findings[["severity"]] == "error",
                           , drop = FALSE]
  add_rows(
    "invalid_or_placeholder_value",
    paste(errors[["logical_output"]], errors[["variable"]],
          errors[["finding_type"]], sep = "::"),
    paste(errors[["example"]], errors[["detail"]], sep = " | ")
  )
  all_missing <- .audit_all_missing_admin_variables(variable_summary)
  add_rows(
    "all_missing_admin_variable", all_missing[["key"]],
    all_missing[["detail"]]
  )

  failures <- unique(.audit_bind_rows(rows,
    c("issue_type", "key", "detail")))
  if (!nrow(failures)) {
    failures[["allowed"]] <- logical()
    failures[["allowlist_reason"]] <- character()
    return(failures)
  }
  allow_key <- paste(allowlist[["issue_type"]], allowlist[["key"]],
                     sep = "\r")
  failure_key <- paste(failures[["issue_type"]], failures[["key"]],
                       sep = "\r")
  match_index <- match(failure_key, allow_key)
  failures[["allowed"]] <- !is.na(match_index)
  failures[["allowlist_reason"]] <- NA_character_
  failures[["allowlist_reason"]][!is.na(match_index)] <-
    allowlist[["reason"]][match_index[!is.na(match_index)]]
  failures
}

run_admin_schema_value_audit <- function(run_dir,
                                         report_dir = file.path(
                                           .audit_repo_root(), "audit",
                                           "admin-schema-values"),
                                         repo_root = .audit_repo_root(),
                                         seed = .audit_default_seed,
                                         allowlist_path = NULL,
                                         fail_on_error = TRUE) {
  metadata <- .audit_read_metadata(repo_root)
  file_index <- .audit_file_index(run_dir)
  if (!nrow(file_index)) {
    stop("No auditable parquet or CSV product files found in ", run_dir,
         call. = FALSE)
  }
  output_map <- .audit_map_outputs(file_index, metadata)
  file_map <- output_map

  variable_rows <- list()
  finding_rows <- list()
  for (i in seq_len(nrow(file_map))) {
    result <- .audit_one_file(file_map[i, , drop = FALSE],
                              file_map[i, c("logical_output", "asset",
                                            "dataset", "official_product",
                                            "official_table", "table_number",
                                            "source_type", "value_scope",
                                            "mapping_status", "schema_role"),
                                       drop = FALSE],
                              metadata)
    variable_rows[[i]] <- result$variable_summary
    finding_rows[[i]] <- result$findings
  }
  variable_summary <- .audit_bind_rows(variable_rows)
  unresolved_admin_variables <- .audit_unresolved_admin_variables(
    variable_summary
  )
  value_findings <- .audit_bind_rows(finding_rows,
    c("source_file", "logical_output", "dataset", "variable",
      "finding_type", "severity", "matched_values", "example", "detail"))
  product_coverage <- .audit_product_coverage(output_map, metadata)
  column_coverage <- .audit_column_coverage(output_map, variable_summary,
                                            metadata)
  allowlist <- .audit_read_allowlist(allowlist_path)
  gate_failures <- .audit_gate_failures(
    product_coverage = product_coverage,
    column_coverage = column_coverage,
    output_map = output_map,
    value_findings = value_findings,
    variable_summary = variable_summary,
    allowlist = allowlist
  )
  unallowed_gate_failures <- sum(!gate_failures[["allowed"]])

  plida_structure <- product_coverage[["asset"]] == "PLIDA"
  blade_structure <- product_coverage[["asset"]] == "BLADE"
  observed_structure <- product_coverage[["coverage_status"]] == "observed"
  missing_variable_structures <- unique(column_coverage[["structure_key"]][
    column_coverage[["coverage_status"]] == "official_only"
  ])
  schema_complete <- observed_structure &
    !product_coverage[["structure_key"]] %in% missing_variable_structures
  observed_plida_products <- unique(product_coverage[["official_product"]][
    plida_structure & observed_structure
  ])
  unmapped <- output_map[["mapping_status"]] %in%
    c("unmapped_output", "unmatched_table_suffix")
  deferred_files <- output_map[["value_scope"]] == "deferred_survey"
  canonical_file <- output_map[["schema_role"]] == "canonical_structure"
  auxiliary_file <- startsWith(output_map[["schema_role"]], "auxiliary_")
  summary <- data.frame(
    audit_seed = as.integer(seed),
    run_dir = normalizePath(run_dir, mustWork = TRUE),
    survey_value_scope = paste(.audit_survey_datasets, collapse = ";"),
    plida_schema_datasets_expected = length(metadata$schema_datasets),
    admin_plida_datasets_expected = length(metadata$admin_datasets),
    plida_products_expected = length(unique(
      product_coverage[["official_product"]][plida_structure]
    )),
    plida_products_observed = length(observed_plida_products),
    plida_structures_expected = sum(plida_structure),
    plida_structures_observed = sum(plida_structure & observed_structure),
    plida_structures_missing = sum(plida_structure & !observed_structure),
    plida_structures_schema_complete = sum(plida_structure & schema_complete),
    blade_tables_expected = sum(blade_structure),
    blade_tables_observed = sum(blade_structure & observed_structure),
    blade_tables_missing = sum(blade_structure & !observed_structure),
    blade_tables_schema_complete = sum(blade_structure & schema_complete),
    physical_files_scanned = nrow(file_index),
    survey_value_files_deferred = sum(deferred_files),
    blade_survey_tables_deferred = sum(
      metadata$blade_tables[["Source.Type"]] == "survey"
    ),
    logical_outputs_scanned = length(unique(output_map[["logical_output"]])),
    canonical_structure_outputs_scanned = length(unique(
      output_map[["logical_output"]][canonical_file]
    )),
    auxiliary_value_outputs_scanned = length(unique(
      output_map[["logical_output"]][auxiliary_file]
    )),
    unmapped_outputs = length(unique(output_map[["logical_output"]][unmapped])),
    generated_variables_scanned = nrow(variable_summary),
    official_variables_missing = sum(column_coverage[["coverage_status"]] ==
                                       "official_only"),
    generated_variables_unmatched = sum(column_coverage[["coverage_status"]] ==
                                          "generated_only"),
    value_errors = sum(value_findings[["severity"]] == "error"),
    explicit_placeholder_errors = sum(
      value_findings[["finding_type"]] %in%
        c("explicit_placeholder", "dataset_counter_placeholder",
          "template_marker")
    ),
    generic_c_code_errors = sum(
      value_findings[["finding_type"]] == "generic_c_code" &
        value_findings[["severity"]] == "error"
    ),
    official_domain_errors = sum(
      value_findings[["finding_type"]] == "official_domain_mismatch"
    ),
    all_missing_admin_variables = sum(
      gate_failures[["issue_type"]] == "all_missing_admin_variable"
    ),
    suspicious_value_warnings = sum(value_findings[["severity"]] == "warning"),
    gate_failures = nrow(gate_failures),
    gate_failures_allowed = sum(gate_failures[["allowed"]]),
    gate_failures_unallowed = unallowed_gate_failures,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)
  .audit_write_csv(summary, file.path(report_dir, "summary.csv"))
  .audit_write_csv(output_map, file.path(report_dir, "output-map.csv"))
  .audit_write_csv(product_coverage,
                   file.path(report_dir, "product-coverage.csv"))
  .audit_write_csv(product_coverage,
                   file.path(report_dir, "structure-coverage.csv"))
  .audit_write_csv(column_coverage,
                   file.path(report_dir, "column-coverage.csv"))
  .audit_write_csv(variable_summary,
                   file.path(report_dir, "variable-summary.csv"))
  .audit_write_csv(
    unresolved_admin_variables,
    file.path(report_dir, "unresolved-admin-variables.csv")
  )
  .audit_write_csv(value_findings,
                   file.path(report_dir, "value-findings.csv"))
  .audit_write_csv(gate_failures,
                   file.path(report_dir, "gate-failures.csv"))

  if (isTRUE(fail_on_error) && unallowed_gate_failures > 0L) {
    stop("Administrative schema/value audit gate failed with ",
         unallowed_gate_failures, " unallowed issue(s). Reports: ",
         normalizePath(report_dir, mustWork = TRUE), call. = FALSE)
  }

  invisible(list(
    summary = summary,
    output_map = output_map,
    product_coverage = product_coverage,
    structure_coverage = product_coverage,
    column_coverage = column_coverage,
    variable_summary = variable_summary,
    unresolved_admin_variables = unresolved_admin_variables,
    value_findings = value_findings,
    gate_failures = gate_failures,
    report_dir = normalizePath(report_dir, mustWork = TRUE)
  ))
}

.audit_build_admin <- function(build_root, n, seed, years) {
  if (!requireNamespace("fplida", quietly = TRUE)) {
    stop("The optional build mode requires an installed fplida package. ",
         "Install the current checkout before using --build.", call. = FALSE)
  }
  dir.create(build_root, recursive = TRUE, showWarnings = FALSE)
  result <- fplida::build_fplida(
    n = as.integer(n), seed = as.integer(seed), years = as.integer(years),
    k_slices = 1L, products = "all",
    output_dir = build_root, suffix = paste0("admin_audit_seed", seed),
    export_format = "parquet"
  )
  result$canonical_run_dir
}

.audit_usage <- function() {
  paste(
    "Usage:",
    "  Rscript scripts/audit_admin_schema_values.R --run-dir PATH [--report-dir PATH] [--allowlist PATH]",
    "  Rscript scripts/audit_admin_schema_values.R --build --build-root PATH [--n 500] [--seed 20260803] [--years 2006:2025] [--report-dir PATH] [--allowlist PATH]",
    "",
    "The command exits non-zero for unallowed placeholders, reliable official-domain",
    "mismatches, unmapped outputs, missing products/tables, missing variables,",
    "or entirely missing administrative/Census canonical variables.",
    "Use --report-only to write findings without enforcing the gate.",
    "An allowlist CSV requires issue_type,key,reason columns.",
    "",
    "The audit defers values for LFS, NHS, NSMHW, PEX and SDAC, plus BLADE",
    "survey tables identified from their published table names. Census, ACLD",
    "and AEDC values remain in scope. Every PLIDA and BLADE schema is audited.",
    sep = "\n"
  )
}

.audit_parse_cli <- function(args) {
  out <- list(build = FALSE, run_dir = NULL, report_dir = NULL,
              build_root = NULL, n = 500L, seed = .audit_default_seed,
              years = 2006:2025, allowlist = NULL, report_only = FALSE,
              help = FALSE)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--build") {
      out$build <- TRUE
    } else if (arg == "--report-only") {
      out$report_only <- TRUE
    } else if (arg %in% c("--help", "-h")) {
      out$help <- TRUE
    } else if (arg %in% c("--run-dir", "--report-dir", "--build-root",
                          "--allowlist",
                          "--n", "--seed", "--years")) {
      if (i == length(args)) stop("Missing value after ", arg, call. = FALSE)
      value <- args[[i + 1L]]
      key <- gsub("-", "_", sub("^--", "", arg))
      out[[key]] <- value
      i <- i + 1L
    } else {
      stop("Unknown argument: ", arg, "\n", .audit_usage(), call. = FALSE)
    }
    i <- i + 1L
  }
  out$n <- as.integer(out$n)
  out$seed <- as.integer(out$seed)
  if (is.character(out$years)) {
    if (grepl("^[0-9]{4}:[0-9]{4}$", out$years)) {
      ends <- as.integer(strsplit(out$years, ":", fixed = TRUE)[[1L]])
      out$years <- seq.int(ends[[1L]], ends[[2L]])
    } else {
      out$years <- as.integer(strsplit(out$years, ",", fixed = TRUE)[[1L]])
    }
  }
  out
}

.audit_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  cli <- .audit_parse_cli(args)
  if (cli$help) {
    cat(.audit_usage(), "\n")
    return(invisible(NULL))
  }
  if (cli$build) {
    if (is.null(cli$build_root)) {
      stop("--build requires --build-root PATH.", call. = FALSE)
    }
    cli$run_dir <- .audit_build_admin(cli$build_root, cli$n, cli$seed,
                                      cli$years)
  }
  if (is.null(cli$run_dir)) {
    stop("Provide --run-dir PATH or use --build --build-root PATH.\n",
         .audit_usage(), call. = FALSE)
  }
  if (is.null(cli$report_dir)) {
    cli$report_dir <- file.path(.audit_repo_root(), "audit",
                                "admin-schema-values")
  }
  result <- run_admin_schema_value_audit(
    run_dir = cli$run_dir, report_dir = cli$report_dir,
    repo_root = .audit_repo_root(), seed = cli$seed,
    allowlist_path = cli$allowlist,
    fail_on_error = !cli$report_only
  )
  print(result$summary, row.names = FALSE)
  cat("Reports: ", result$report_dir, "\n", sep = "")
  invisible(result)
}

if (sys.nframe() == 0L) {
  .audit_main()
}
