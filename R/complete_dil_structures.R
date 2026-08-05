# Complete PLIDA DIL product-table structures.
#
# Bespoke generators remain the source of the package's richer behavioural
# data. A complete build also writes a small, linked, metadata-shaped table for
# every structure in the bundled PLIDA DIL. These canonical structure files
# make the full DIL surface available without replacing the richer outputs.

.dil_deferred_survey_datasets <- c("LFS", "NHS", "NSMHW", "PEX", "SDAC")

.dil_build_target_datasets <- c(
  census = "CENSUS", pit_ps = "PIT_PS", pit_itr = "PIT_ITR",
  core = "CORE", he = "HE", domino = "DOMINO", mbs = "MBS",
  pbs = "PBS", tva = "TVA", combined = "COMBINED", births = "BIRTHS",
  deaths = "DEATHS", mcd = "MCD", ato_cr = "ATO_CR", visa = "VISA",
  mt_demogs = "MT_DEMOGS", sdb = "SDB", travellers = "TRAVELLERS",
  pit_ie = "PIT_IE", busown = "BUSOWN", sae = "SAE", cgt = "CGT",
  rps = "RPS", stp = "STP", ndis = "NDIS", apprentice = "A&T",
  dex = "DEX", air = "AIR", amep = "AMEP", nacdc = "NACDC",
  aedc = "AEDC", acld = "ACLD", sdac = "SDAC", ers = "ERS",
  jk = "JK", jm = "JM", nhs = "NHS", nsmhw = "NSMHW", pex = "PEX",
  apsed = "APSED", ato_mcs = "ATO_MCS", lfs = "LFS", smsf = "SMSF"
)

.dil_structure_inventory <- function(datasets = NULL) {
  variables <- utils::read.csv(
    .dil_metadata_path("variables.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!is.null(datasets)) {
    variables <- variables[variables$Dataset %in% datasets, , drop = FALSE]
  }
  variables <- variables[
    nzchar(variables[["Product Name"]]) & nzchar(variables[["Table Name"]]),
    ,
    drop = FALSE
  ]
  structures <- unique(variables[c(
    "Dataset", "Module Name", "Product Name", "Table Name"
  )])
  structures <- structures[order(
    structures$Dataset,
    structures[["Product Name"]],
    structures[["Table Name"]]
  ), , drop = FALSE]
  rownames(structures) <- NULL
  list(structures = structures, variables = variables)
}

.dil_structure_stem <- function(product_name, table_name) {
  # DIL table names contain only filesystem-safe ASCII letters, numbers,
  # underscores, hyphens, and periods. Preserve the exact spelling because
  # 21 STP table pairs differ only by underscore versus hyphen.
  if (grepl("[^A-Za-z0-9_.-]", table_name)) {
    stop("Unsafe DIL table name: ", table_name, call. = FALSE)
  }
  paste0(product_name, "--", table_name)
}

.dil_structure_tokens <- function(x) {
  x <- tolower(gsub("[^[:alnum:]]+", " ", paste(x, collapse = " ")))
  tokens <- unlist(strsplit(x, " +"), use.names = FALSE)
  unique(tokens[nzchar(tokens)])
}

.dil_parquet_logical_output <- function(path) {
  stem <- tools::file_path_sans_ext(basename(path))
  if (grepl("^part(?:-[0-9]+)+$", stem, perl = TRUE)) {
    basename(dirname(path))
  } else {
    stem
  }
}

.dil_structure_candidate_index <- function(dataset_directory,
                                           canonical_outputs = NULL) {
  files <- list.files(
    dataset_directory,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.parquet$",
    ignore.case = TRUE
  )
  files <- files[!grepl("-spine\\.parquet$", files, ignore.case = TRUE)]
  if (!length(files)) {
    return(data.frame(
      path = character(), logical_output = character(),
      stringsAsFactors = FALSE
    ))
  }
  logical_output <- vapply(files, .dil_parquet_logical_output, character(1))
  # Never reuse a canonical DIL file from a prior build. Some bespoke
  # generators also use a product--table stem, so callers with the DIL
  # inventory identify exact canonical outputs instead of excluding every
  # double-hyphen output.
  fresh_source <- if (is.null(canonical_outputs)) {
    !grepl("--", logical_output, fixed = TRUE)
  } else {
    !logical_output %in% canonical_outputs
  }
  files <- files[fresh_source]
  logical_output <- logical_output[fresh_source]
  if (!length(files)) {
    return(data.frame(
      path = character(), logical_output = character(),
      stringsAsFactors = FALSE
    ))
  }
  # One physical part is sufficient as a value/template source. Schemas are
  # invariant across parts produced by merge_slice_outputs().
  keep <- !duplicated(logical_output)
  files <- files[keep]
  logical_output <- logical_output[keep]
  columns <- lapply(files, function(path) {
    arrow::ParquetFileReader$create(path)$GetSchema()$names
  })
  out <- data.frame(
    path = files,
    logical_output = logical_output,
    stringsAsFactors = FALSE
  )
  out$columns <- I(columns)
  out
}

.dil_best_structure_source <- function(candidates, product_name, table_name,
                                       variable_names,
                                       allow_product_source = TRUE,
                                       source_aliases = character()) {
  if (!nrow(candidates)) return(NULL)
  # Reuse only one exact table output, or one exact product output when a
  # table-specific output is absent. Fuzzy matching can copy a different year
  # or population merely because common columns overlap.
  table_matches <- candidates$logical_output == table_name
  alias_matches <- candidates$logical_output %in% source_aliases
  product_matches <- candidates$logical_output == product_name
  candidates <- if (sum(table_matches) == 1L) {
    candidates[table_matches, , drop = FALSE]
  } else if (sum(alias_matches) == 1L) {
    candidates[alias_matches, , drop = FALSE]
  } else if (isTRUE(allow_product_source)) {
    candidates[product_matches, , drop = FALSE]
  } else {
    candidates[0L, , drop = FALSE]
  }
  if (nrow(candidates) != 1L) return(NULL)
  expected <- unique(toupper(variable_names))
  generated <- unique(toupper(candidates$columns[[1L]]))
  key_columns <- c("SYNTHETIC_AEUID", "SPINE_ID")
  overlap <- setdiff(intersect(expected, generated), key_columns)
  expected_nonkey <- setdiff(expected, key_columns)
  expects_person_key <- length(intersect(expected, key_columns)) > 0L
  has_person_key <- length(intersect(generated, key_columns)) > 0L
  if ((expects_person_key && !has_person_key) ||
      !length(expected_nonkey) || !length(overlap)) {
    return(NULL)
  }
  candidates$path[[1L]]
}

.dil_allow_product_source <- function(dataset, product_table_count) {
  # MBS and PBS generators write one annual product. Their DIL structures split
  # the same schema into monthly tables. The period adapter rewrites each
  # canonical table's dates to its exact month after it copies the annual data.
  dataset %in% c("MBS", "PBS") ||
    .dil_has_multi_table_product_source(dataset) ||
    identical(as.integer(product_table_count), 1L)
}

.dil_source_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  value <- as.character(x)
  parsed <- suppressWarnings(as.Date(value, format = "%d%b%y"))
  unresolved <- is.na(parsed) & !is.na(value) & nzchar(value)
  if (any(unresolved)) {
    parsed[unresolved] <- suppressWarnings(as.Date(
      value[unresolved], format = "%Y-%m-%d"
    ))
  }
  parsed
}

.dil_filter_product_source <- function(source_frame, dataset, product_name,
                                       table_name, module_name, max_rows) {
  if (!nrow(source_frame)) return(source_frame)
  max_rows <- as.integer(max_rows)
  if (identical(dataset, "CORE") &&
      "COMBINED_CATEGORY" %in% names(source_frame)) {
    lower <- tolower(table_name)
    parent <- grepl("par[_-]?chi|parent[_-]?child", lower, perl = TRUE)
    partner <- grepl("partner|spouse", lower, perl = TRUE)
    category <- tolower(as.character(source_frame$COMBINED_CATEGORY))
    if (parent && any(category == "parent-child")) {
      source_frame <- source_frame[category == "parent-child", , drop = FALSE]
    } else if (partner && any(category == "partner")) {
      source_frame <- source_frame[category == "partner", , drop = FALSE]
    } else if (any(category == "partner") &&
               any(category == "parent-child")) {
      # The native relationship output is ordered by relationship type.
      # Interleave both types for combined relationship-map tables.
      partner_rows <- source_frame[category == "partner", , drop = FALSE]
      parent_rows <- source_frame[category == "parent-child", , drop = FALSE]
      take_partner <- ceiling(max_rows / 2)
      take_parent <- floor(max_rows / 2)
      source_frame <- rbind(
        utils::head(partner_rows, take_partner),
        utils::head(parent_rows, take_parent)
      )
    }
    return(utils::head(source_frame, max_rows))
  }
  if (!dataset %in% c("MBS", "PBS")) {
    return(utils::head(source_frame, max_rows))
  }
  period <- .dil_table_period(dataset, product_name, table_name, module_name)
  date_name <- if (dataset == "MBS") "DOS" else "SPPLY_DT"
  if (identical(period$granularity, "month") &&
      date_name %in% names(source_frame)) {
    date <- .dil_source_date(source_frame[[date_name]])
    in_period <- !is.na(date) & date >= period$start & date <= period$end
    if (any(in_period)) {
      source_frame <- source_frame[in_period, , drop = FALSE]
    }
  }
  utils::head(source_frame, max_rows)
}

.dil_read_head <- function(path, n) {
  dataset <- arrow::open_dataset(path, format = "parquet")
  as.data.frame(
    dplyr::collect(utils::head(dataset, n)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.dil_financial_compact_datasets <- c(
  "ATO_MCS", "BUSOWN", "PIT_IE", "PIT_ITR", "PIT_PS", "RPS", "SAE"
)

.dil_compact_financial_context <- function(dataset, text) {
  dataset %in% .dil_financial_compact_datasets ||
    (identical(dataset, "CORE") && grepl(
      "(?:^|[-_])(?:income|employment)(?:[-_]|$)",
      tolower(paste(text, collapse = " ")), perl = TRUE
    ))
}

.dil_month_table_period <- function(dataset, table_name) {
  lower <- tolower(paste(table_name, collapse = " "))
  pattern <- if (dataset %in% c("MBS", "PBS")) {
    "_(?:((?:19|20)[0-9]{2}))_([0-9]{1,2})$"
  } else if (identical(dataset, "STP")) {
    "_(?:((?:19|20)[0-9]{2}))_m([0-9]{2})$"
  } else {
    return(NULL)
  }
  parts <- regmatches(lower, regexec(pattern, lower, perl = TRUE))[[1L]]
  if (!length(parts)) return(NULL)
  year <- as.integer(parts[[2L]])
  month <- as.integer(parts[[3L]])
  if (is.na(month) || month < 1L || month > 12L) return(NULL)
  start <- as.Date(sprintf("%04d-%02d-01", year, month))
  list(
    start_year = year,
    end_year = year,
    start = start,
    end = seq(start, by = "1 month", length.out = 2L)[[2L]] - 1L
  )
}

.dil_coverage_span_datasets <- c(
  "AMEP", "ATO_CR", "MT_DEMOGS", "SDB", "VISA"
)

.dil_coverage_table_period <- function(dataset, table_name) {
  if (!dataset %in% .dil_coverage_span_datasets) return(NULL)
  lower <- tolower(paste(table_name, collapse = " "))
  parts <- regmatches(
    lower,
    regexec(
      "_(?:((?:19|20)[0-9]{2}))_([0-9]{2})(?:_|$)",
      lower, perl = TRUE
    )
  )[[1L]]
  if (!length(parts)) return(NULL)
  start_year <- as.integer(parts[[2L]])
  end_short <- as.integer(parts[[3L]])
  end_year <- 100L * (start_year %/% 100L) + end_short
  if (end_year < start_year) end_year <- end_year + 100L
  if (end_year <= start_year) return(NULL)
  list(
    start_year = start_year,
    end_year = end_year,
    start = as.Date(sprintf("%04d-01-01", start_year)),
    end = as.Date(sprintf("%04d-12-31", end_year))
  )
}

.dil_period <- function(text, default_year = 2024L, dataset = NULL,
                        table_name = NULL) {
  text <- paste(text, collapse = " ")
  month_period <- .dil_month_table_period(dataset, table_name)
  if (!is.null(month_period)) return(month_period)
  coverage_period <- .dil_coverage_table_period(dataset, table_name)
  if (!is.null(coverage_period)) return(coverage_period)
  periods <- regmatches(
    text,
    gregexpr("(?<![0-9])(?:19|20)[0-9]{2}[-_/][0-9]{2}(?![0-9])",
             text, perl = TRUE)
  )[[1L]]
  if (length(periods) && !identical(periods, "")) {
    # An underscore also separates a calendar year from a month or duration
    # in DIL table names (for example, 2021_12m and 2021_46m). Accept a
    # candidate as a financial year only when its second component is the
    # immediately following year.
    valid_period <- vapply(periods, function(value) {
      start_year <- as.integer(substr(value, 1L, 4L))
      end_short <- as.integer(sub(".*[-_/]", "", value))
      identical(end_short, as.integer((start_year + 1L) %% 100L))
    }, logical(1))
    periods <- periods[valid_period]
  }
  if (length(periods) && !identical(periods, "")) {
    value <- periods[[length(periods)]]
    start_year <- as.integer(substr(value, 1L, 4L))
    end_short <- as.integer(sub(".*[-_/]", "", value))
    end_year <- 100L * (start_year %/% 100L) + end_short
    if (end_year < start_year) end_year <- end_year + 100L
    return(list(
      start_year = start_year,
      end_year = end_year,
      start = as.Date(sprintf("%04d-07-01", start_year)),
      end = as.Date(sprintf("%04d-06-30", end_year))
    ))
  }

  compact_fy <- regmatches(
    tolower(text),
    gregexpr("fy[0-9]{4}", tolower(text), perl = TRUE)
  )[[1L]]
  if (length(compact_fy) && !identical(compact_fy, "")) {
    digits <- sub("^fy", "", compact_fy[[length(compact_fy)]])
    start_short <- as.integer(substr(digits, 1L, 2L))
    start_year <- if (start_short >= 80L) 1900L + start_short else 2000L + start_short
    end_short <- as.integer(substr(digits, 3L, 4L))
    end_year <- 100L * (start_year %/% 100L) + end_short
    if (end_year < start_year) end_year <- end_year + 100L
    return(list(
      start_year = start_year,
      end_year = end_year,
      start = as.Date(sprintf("%04d-07-01", start_year)),
      end = as.Date(sprintf("%04d-06-30", end_year))
    ))
  }

  table_lower <- tolower(paste(table_name, collapse = " "))
  compact_financial <- .dil_compact_financial_context(
    dataset, c(text, table_name)
  )
  compact <- regmatches(
    table_lower,
    gregexpr("(?<![0-9])[0-9]{4}(?![0-9])", table_lower, perl = TRUE)
  )[[1L]]
  compact <- compact[compact != ""]
  if (!compact_financial) {
    compact <- compact[!grepl("^(?:19|20)[0-9]{2}$", compact)]
  }
  if (length(compact)) {
    value <- compact[[length(compact)]]
    if (identical(dataset, "MCD")) {
      month <- as.integer(substr(value, 1L, 2L))
      year <- as.integer(substr(value, 3L, 4L)) + 2000L
      if (month >= 1L && month <= 12L) {
        start <- as.Date(sprintf("%04d-%02d-01", year, month))
        end <- seq(start, by = "1 month", length.out = 2L)[[2L]] - 1L
        return(list(
          start_year = year,
          end_year = year,
          start = start,
          end = end
        ))
      }
    }
    if (identical(dataset, "ACLD")) {
      start_year <- 2000L + as.integer(substr(value, 1L, 2L))
      end_year <- 2000L + as.integer(substr(value, 3L, 4L))
      return(list(
        start_year = start_year,
        end_year = end_year,
        start = as.Date(sprintf("%04d-01-01", start_year)),
        end = as.Date(sprintf("%04d-12-31", end_year))
      ))
    }
    if (compact_financial) {
      start_short <- as.integer(substr(value, 1L, 2L))
      end_short <- as.integer(substr(value, 3L, 4L))
      start_year <- if (start_short >= 80L) {
        1900L + start_short
      } else {
        2000L + start_short
      }
      end_year <- 100L * (start_year %/% 100L) + end_short
      if (end_year < start_year) end_year <- end_year + 100L
      return(list(
        start_year = start_year,
        end_year = end_year,
        start = as.Date(sprintf("%04d-07-01", start_year)),
        end = as.Date(sprintf("%04d-06-30", end_year))
      ))
    }
  }

  years <- regmatches(
    text,
    gregexpr("(?:19|20)[0-9]{2}", text, perl = TRUE)
  )[[1L]]
  years <- suppressWarnings(as.integer(years[years != ""]))
  years <- years[is.finite(years)]
  if (!length(years)) years <- default_year
  start_year <- min(years)
  end_year <- max(years)
  list(
    start_year = start_year,
    end_year = end_year,
    start = as.Date(sprintf("%04d-01-01", start_year)),
    end = as.Date(sprintf("%04d-12-31", end_year))
  )
}

.dil_table_period <- function(dataset, product_name, table_name,
                              module_name) {
  coverage_period <- .dil_coverage_table_period(dataset, table_name)
  if (!is.null(coverage_period)) {
    return(c(
      coverage_period,
      list(
        year = coverage_period$end_year,
        month = NA_integer_,
        quarter = NA_integer_,
        granularity = "range",
        precise = FALSE
      )
    ))
  }
  period <- .dil_period(
    c(product_name, table_name, module_name),
    dataset = dataset,
    table_name = table_name
  )
  lower <- tolower(table_name)
  compact_financial <- .dil_compact_financial_context(
    dataset, c(product_name, table_name, module_name)
  )
  year_matches <- regmatches(
    lower, gregexpr("(?:19|20)[0-9]{2}", lower, perl = TRUE)
  )[[1L]]
  years <- suppressWarnings(as.integer(year_matches[year_matches != ""]))
  year <- if (length(years)) years[[1L]] else NA_integer_
  if (!length(year) || is.na(year)) year <- period$end_year

  month <- NA_integer_
  if (dataset %in% c("MBS", "PBS")) {
    match <- regexec("_(?:19|20)[0-9]{2}_([0-9]{1,2})$", lower,
                     perl = TRUE)
    parts <- regmatches(lower, match)[[1L]]
    if (length(parts)) month <- as.integer(parts[[2L]])
  }
  if (dataset == "MCD") {
    match <- regexec("_([01][0-9])([0-9]{2})(?:_|$)", lower, perl = TRUE)
    parts <- regmatches(lower, match)[[1L]]
    if (length(parts)) {
      month <- as.integer(parts[[2L]])
      year <- 2000L + as.integer(parts[[3L]])
    }
  }
  stp_match <- regexec("_(?:19|20)[0-9]{2}_m([0-9]{2})$", lower,
                       perl = TRUE)
  stp_parts <- regmatches(lower, stp_match)[[1L]]
  if (length(stp_parts)) month <- as.integer(stp_parts[[2L]])
  month_words <- c(
    jan = 1L, feb = 2L, mar = 3L, apr = 4L, may = 5L, jun = 6L,
    jul = 7L, aug = 8L, sep = 9L, oct = 10L, nov = 11L, dec = 12L
  )
  word_hit <- names(month_words)[vapply(
    names(month_words),
    function(word) grepl(paste0("(?:^|_)", word, "(?:[0-9]{4}|_|$)"),
                         lower, perl = TRUE),
    logical(1)
  )]
  if (length(word_hit)) month <- month_words[[word_hit[[1L]]]]

  quarter <- NA_integer_
  if (dataset %in% c("NDIS", "TRAVELLERS")) {
    q_match <- regexec("(?:19|20)[0-9]{2}(?:q|_)([1-4])$", lower,
                       perl = TRUE)
    q_parts <- regmatches(lower, q_match)[[1L]]
    if (length(q_parts)) quarter <- as.integer(q_parts[[2L]])
  }
  if (!is.na(quarter)) {
    month <- 1L + (quarter - 1L) * 3L
    start <- as.Date(sprintf("%04d-%02d-01", year, month))
    end <- seq(start, by = "3 months", length.out = 2L)[[2L]] - 1L
    return(list(
      start_year = year,
      end_year = year,
      start = start,
      end = end,
      year = year,
      month = NA_integer_,
      quarter = quarter,
      granularity = "quarter",
      precise = TRUE
    ))
  }
  if (!is.na(month) && month >= 1L && month <= 12L) {
    start <- as.Date(sprintf("%04d-%02d-01", year, month))
    end <- seq(start, by = "1 month", length.out = 2L)[[2L]] - 1L
    return(list(
      start_year = year,
      end_year = year,
      start = start,
      end = end,
      year = year,
      month = month,
      quarter = NA_integer_,
      granularity = "month",
      precise = TRUE
    ))
  }
  compact_table <- regmatches(
    lower,
    gregexpr("(?<![0-9])[0-9]{4}(?![0-9])", lower, perl = TRUE)
  )[[1L]]
  compact_table <- compact_table[compact_table != ""]
  if (!compact_financial) {
    compact_table <- compact_table[
      !grepl("^(?:19|20)[0-9]{2}$", compact_table)
    ]
  }
  long_fy <- regmatches(
    lower,
    gregexpr("(?<![0-9])(?:19|20)[0-9]{2}[-_][0-9]{2}(?![0-9])",
             lower, perl = TRUE)
  )[[1L]]
  valid_long_fy <- length(long_fy) && !identical(long_fy, "") && any(
    vapply(long_fy, function(value) {
      start_year <- as.integer(substr(value, 1L, 4L))
      end_short <- as.integer(sub(".*[-_]", "", value))
      identical(end_short, as.integer((start_year + 1L) %% 100L))
    }, logical(1))
  )
  period_is_financial <-
    format(period$start, "%m-%d") == "07-01" &&
    format(period$end, "%m-%d") == "06-30" &&
    period$end_year == period$start_year + 1L
  financial_year <- valid_long_fy || period_is_financial ||
    (compact_financial && length(compact_table) > 0L)
  annual <- length(unique(years)) == 1L && !grepl("current", lower)
  if (financial_year) {
    return(list(
      start_year = period$start_year,
      end_year = period$end_year,
      start = period$start,
      end = period$end,
      year = period$end_year,
      month = NA_integer_,
      quarter = NA_integer_,
      granularity = "financial_year",
      precise = TRUE
    ))
  }
  if (annual) {
    return(list(
      start_year = year,
      end_year = year,
      start = as.Date(sprintf("%04d-01-01", year)),
      end = as.Date(sprintf("%04d-12-31", year)),
      year = year,
      month = NA_integer_,
      quarter = NA_integer_,
      granularity = "year",
      precise = TRUE
    ))
  }
  list(
    start_year = period$start_year,
    end_year = period$end_year,
    start = period$start,
    end = period$end,
    year = year,
    month = NA_integer_,
    quarter = NA_integer_,
    granularity = "range",
    precise = FALSE
  )
}

.dil_apply_table_period <- function(frame, dataset, product_name, table_name,
                                    module_name, seed) {
  if (!nrow(frame)) return(frame)
  period <- .dil_table_period(dataset, product_name, table_name, module_name)
  if (!isTRUE(period$precise)) return(frame)
  n <- nrow(frame)
  draw_date <- function(name, start = period$start, end = period$end) {
    salt <- .stable_name_seed(paste(table_name, name, sep = "|"))
    u <- .admin_unit_interval(n, seed, salt)
    date_span <- pmax(0L, as.integer(end - start))
    start + floor(u * (date_span + 1L))
  }
  draw_offset <- function(name, maximum) {
    salt <- .stable_name_seed(paste(table_name, name, sep = "|"))
    floor(.admin_unit_interval(n, seed, salt) * (maximum + 1L))
  }
  set_if_present <- function(name, value) {
    if (name %in% names(frame)) frame[[name]] <<- value
  }

  # Rewrite only the field that defines a dated partition. Preserve historical
  # fields, such as birth, diagnosis, relationship, and first-contact dates.
  if (dataset == "MBS") {
    dos <- draw_date("DOS")
    set_if_present("DOS", dos)
    set_if_present("DOP", dos + draw_offset("DOP", 28L))
    set_if_present("RPDATE", dos - draw_offset("RPDATE", 180L))
  } else if (dataset == "PBS") {
    supply <- draw_date("SPPLY_DT")
    set_if_present("SPPLY_DT", supply)
    set_if_present("PRSCRB_DT", supply - draw_offset("PRSCRB_DT", 30L))
    set_if_present("EXTRCT_DT", supply + 7L + draw_offset("EXTRCT_DT", 53L))
  } else if (dataset == "STP" && period$granularity == "month") {
    payment <- draw_date("PMT_DT")
    pay_end <- payment - draw_offset("DRVPAYENDDATE", 7L)
    pay_start <- pay_end - 7L - draw_offset("DRVPAYSTARTDATE", 7L)
    set_if_present("PMT_DT", payment)
    set_if_present("DRVPAYENDDATE", pay_end)
    set_if_present("DRVPAYSTARTDATE", pay_start)
  } else if (dataset == "NDIS" && period$granularity == "quarter") {
    set_if_present("SFOF_DT", draw_date("SFOF_DT"))
    if ("PYMTRQSTCRTDDT" %in% names(frame)) {
      lodged <- draw_date("PYMTRQSTCRTDDT")
      cleared <- pmin(
        lodged + draw_offset("RBAPYMTCLRDDT", 14L), period$end
      )
      sent <- pmin(
        cleared + draw_offset("RBAPYMTSENTDT", 7L), period$end
      )
      support_end <- lodged - draw_offset("SUPPENDDTADJ", 30L)
      support_start <- support_end - draw_offset("SUPPSTRTDTADJ", 90L)
      set_if_present("PYMTRQSTCRTDDT", lodged)
      set_if_present("RBAPYMTCLRDDT", cleared)
      set_if_present("RBAPYMTSENTDT", sent)
      set_if_present("SUPPSTRTDTADJ", support_start)
      set_if_present("SUPPENDDTADJ", support_end)
    }
    # FY_CLAIM uses Australian financial-year end-year coding. Calendar Q1
    # and Q2 end in the same year; Q3 and Q4 end in the following year.
    if ("FY_CLAIM" %in% names(frame)) {
      frame$FY_CLAIM <- rep(
        as.integer(period$year + as.integer(period$quarter >= 3L)), n
      )
    }
    set_if_present("SRCSYSTMDT", rep(period$end, n))
    set_if_present("SNAPDT", rep(period$end, n))
  } else if (dataset == "TRAVELLERS" &&
             period$granularity %in% c("year", "quarter")) {
    set_if_present(
      "DURATION_MOVEMENT_DATE", draw_date("DURATION_MOVEMENT_DATE")
    )
  } else if (dataset == "NACDC" && period$granularity == "month") {
    set_if_present("REFERENCE_DATE", rep(period$end, n))
  }

  if (dataset == "DEATHS" && period$granularity == "year") {
    death_date <- draw_date("DEATH_DATE")
    set_if_present("DEATH_DATE", death_date)
    for (name in intersect(c("YEAR_OF_DEATH", "DEATH_YEAR"), names(frame))) {
      frame[[name]] <- as.integer(format(death_date, "%Y"))
    }
    for (name in intersect(c("MONTH_OF_DEATH", "DEATH_MONTH"), names(frame))) {
      frame[[name]] <- as.integer(format(death_date, "%m"))
    }
    set_if_present("DEATH_DAY", as.integer(format(death_date, "%d")))
  }

  for (name in intersect(
    c("REFERENCE_DATE", "REF_DATE", "SNAPSHOT_DATE", "RUN_DATE", "RUN_DT",
      "EXTRACT_DATE", "EXTRACT_DT", "REPORTING_DATE"),
    names(frame)
  )) {
    frame[[name]] <- rep(period$end, n)
  }
  for (name in intersect(
    c("REFERENCE_YEAR", "REF_YEAR", "REPORTING_YEAR", "EXTRACT_YEAR"),
    names(frame)
  )) {
    frame[[name]] <- rep(as.integer(period$year), n)
  }
  if (!is.na(period$month)) {
    for (name in intersect(
      c("REFERENCE_MONTH", "REF_MONTH", "REPORTING_MONTH", "EXTRACT_MONTH"),
      names(frame)
    )) {
      frame[[name]] <- rep(as.integer(period$month), n)
    }
  }
  if (!is.na(period$quarter)) {
    for (name in intersect(
      c("REFERENCE_QUARTER", "REF_QUARTER", "REPORTING_QUARTER"),
      names(frame)
    )) {
      frame[[name]] <- rep(as.integer(period$quarter), n)
    }
  }
  period_value <- switch(
    period$granularity,
    month = sprintf("%04d-%02d", period$year, period$month),
    quarter = sprintf("%04dQ%d", period$year, period$quarter),
    financial_year = sprintf("%04d-%02d", period$start_year,
                             period$end_year %% 100L),
    year = as.character(period$year),
    as.character(period$year)
  )
  for (name in intersect(
    c("REFERENCE_PERIOD", "REPORTING_PERIOD", "THE_REPORTING_PERIOD"),
    names(frame)
  )) {
    frame[[name]] <- rep(period_value, n)
  }
  frame
}

.dil_recycle_spine <- function(spine_pool, dataset, n, seed, salt) {
  if (!nrow(spine_pool) || n <= 0L) return(spine_pool[0L, , drop = FALSE])
  agency <- dataset_to_agency(dataset)
  aeuid_name <- paste0("aeuid_", tolower(agency))
  eligible <- seq_len(nrow(spine_pool))
  if (aeuid_name %in% names(spine_pool)) {
    linked <- !is.na(spine_pool[[aeuid_name]]) & nzchar(spine_pool[[aeuid_name]])
    if (any(linked)) eligible <- which(linked)
  }
  start <- as.integer((as.numeric(seed) * 1009 + as.numeric(salt) * 9176) %%
                        length(eligible)) + 1L
  selected <- eligible[((start - 1L + seq_len(n) - 1L) %% length(eligible)) + 1L]
  spine_pool[selected, , drop = FALSE]
}

.dil_align_source_to_spine <- function(source_frame, spine_pool, dataset,
                                        min_rows = 25L) {
  if (!nrow(source_frame)) return(NULL)
  source_names <- toupper(names(source_frame))
  agency_name <- paste0("aeuid_", tolower(dataset_to_agency(dataset)))
  source_key <- if ("SYNTHETIC_AEUID" %in% source_names &&
                    agency_name %in% names(spine_pool)) {
    names(source_frame)[match("SYNTHETIC_AEUID", source_names)]
  } else if ("SPINE_ID" %in% source_names &&
             "spine_id" %in% names(spine_pool)) {
    names(source_frame)[match("SPINE_ID", source_names)]
  } else {
    return(NULL)
  }
  pool_key <- if (toupper(source_key) == "SYNTHETIC_AEUID") {
    agency_name
  } else {
    "spine_id"
  }
  source_values <- as.character(source_frame[[source_key]])
  pool_values <- as.character(spine_pool[[pool_key]])
  if (anyDuplicated(pool_values)) return(NULL)
  matched <- match(source_values, pool_values)
  keep <- !is.na(source_values) & nzchar(source_values) & !is.na(matched)
  required <- min(as.integer(min_rows), nrow(source_frame))
  if (sum(keep) < required) return(NULL)
  list(
    source = source_frame[keep, , drop = FALSE],
    spine = spine_pool[matched[keep], , drop = FALSE]
  )
}

.dil_read_key_matches <- function(path, key, values) {
  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values) || !file.exists(path)) return(NULL)
  dataset <- arrow::open_dataset(path, format = "parquet")
  dataset_names <- names(dataset$schema)
  actual_key <- dataset_names[match(toupper(key), toupper(dataset_names))]
  if (!length(actual_key) || is.na(actual_key)) return(NULL)
  filter_call <- substitute(
    dplyr::filter(dataset, key_column %in% value_set),
    list(key_column = as.name(actual_key), value_set = values)
  )
  tryCatch(
    as.data.frame(
      dplyr::collect(eval(filter_call)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )
}

.dil_align_source_to_full_spine <- function(source_frame, base_path,
                                             agency_spine_path, dataset,
                                             min_rows = 25L) {
  if (!nrow(source_frame)) return(NULL)
  source_names <- toupper(names(source_frame))
  source_key <- if ("SYNTHETIC_AEUID" %in% source_names) {
    names(source_frame)[match("SYNTHETIC_AEUID", source_names)]
  } else if ("SPINE_ID" %in% source_names) {
    names(source_frame)[match("SPINE_ID", source_names)]
  } else {
    return(NULL)
  }
  source_values <- as.character(source_frame[[source_key]])

  if (toupper(source_key) == "SYNTHETIC_AEUID") {
    agency_map <- .dil_read_key_matches(
      agency_spine_path, "SYNTHETIC_AEUID", source_values
    )
    if (is.null(agency_map) || !nrow(agency_map)) return(NULL)
    agency_names <- toupper(names(agency_map))
    aeuid_key <- names(agency_map)[
      match("SYNTHETIC_AEUID", agency_names)
    ]
    map_spine_key <- names(agency_map)[match("SPINE_ID", agency_names)]
    if (is.na(aeuid_key) || is.na(map_spine_key) ||
        anyDuplicated(as.character(agency_map[[aeuid_key]]))) {
      return(NULL)
    }
    map_index <- match(
      source_values, as.character(agency_map[[aeuid_key]])
    )
    source_spine_ids <- as.character(agency_map[[map_spine_key]][map_index])
  } else {
    source_spine_ids <- source_values
  }

  full_spine <- .dil_read_key_matches(
    base_path, "spine_id", source_spine_ids
  )
  if (is.null(full_spine) || !nrow(full_spine)) return(NULL)
  full_names <- toupper(names(full_spine))
  full_spine_key <- names(full_spine)[match("SPINE_ID", full_names)]
  if (is.na(full_spine_key) ||
      anyDuplicated(as.character(full_spine[[full_spine_key]]))) {
    return(NULL)
  }
  matched <- match(
    source_spine_ids, as.character(full_spine[[full_spine_key]])
  )
  keep <- !is.na(source_spine_ids) & nzchar(source_spine_ids) &
    !is.na(matched)
  required <- min(as.integer(min_rows), nrow(source_frame))
  if (sum(keep) < required) return(NULL)
  list(
    source = source_frame[keep, , drop = FALSE],
    spine = full_spine[matched[keep], , drop = FALSE],
    source_index = which(keep)
  )
}

.dil_numeric_key <- function(spine_rows, seed, salt = 0L) {
  n <- nrow(spine_rows)
  base <- if ("id" %in% names(spine_rows)) {
    suppressWarnings(as.numeric(spine_rows$id))
  } else {
    seq_len(n)
  }
  base[!is.finite(base)] <- seq_len(n)[!is.finite(base)]
  (base * 1000003 + as.numeric(seed) * 9176 + as.numeric(salt) * 104729) %%
    999999999999
}

.dil_character_id <- function(prefix, spine_rows, seed, salt = 0L,
                              width = 12L) {
  value <- .dil_numeric_key(spine_rows, seed, salt)
  paste0(prefix, sprintf(paste0("%0", width, ".0f"), value))
}

.dil_link_family <- function(name) {
  upper <- toupper(name)
  families <- c(
    plan = "PLAN", provider = "PROVIDER|PRVDR", case = "CASE",
    employer = "EMPLOYER|PYR", participant = "PARTICIPANT",
    client = "CLIENT", household = "HOUSEHOLD|DWELLING|FAMILY"
  )
  hit <- names(families)[vapply(
    families, function(pattern) grepl(pattern, upper, perl = TRUE),
    logical(1)
  )]
  if (length(hit) && grepl(
    "ID|IDNTFR|IDENTIFIER|GUID|UUID|NMBR|NUMBER|KEY|SEQ", upper,
    perl = TRUE
  )) {
    return(hit[[1L]])
  }
  name
}

.dil_sample_values <- function(values, n, seed, salt = 0L,
                               weights = NULL) {
  keep <- !is.na(values) & nzchar(values)
  values <- values[keep]
  if (!is.null(weights)) weights <- as.numeric(weights)[keep]
  distinct <- !duplicated(values)
  values <- values[distinct]
  if (!is.null(weights)) weights <- weights[distinct]
  if (!length(values)) return(rep(NA_character_, n))
  u <- .admin_unit_interval(n, seed, salt)
  if (!is.null(weights) && length(weights) == length(values) &&
      all(is.finite(weights)) && all(weights >= 0) && sum(weights) > 0) {
    cumulative <- cumsum(weights) / sum(weights)
    index <- findInterval(u, c(0, cumulative), rightmost.closed = TRUE)
    return(values[pmin(pmax(index, 1L), length(values))])
  }
  index <- 1L + floor(u * length(values))
  values[pmin(index, length(values))]
}

.dil_evidence_weights <- function(values, distribution) {
  if (is.na(distribution) || !nzchar(distribution)) return(NULL)
  entries <- trimws(strsplit(distribution, ";", fixed = TRUE)[[1L]])
  pattern <- "^(.+?)=([0-9]+(?:\\.[0-9]+)?)$"
  valid <- grepl(pattern, entries, perl = TRUE)
  if (!all(valid)) return(NULL)
  labels <- trimws(sub(pattern, "\\1", entries, perl = TRUE))
  counts <- suppressWarnings(as.numeric(sub(pattern, "\\2", entries,
                                             perl = TRUE)))
  matched <- match(values, labels)
  if (anyNA(matched)) return(NULL)
  weights <- counts[matched]
  if (any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0) {
    return(NULL)
  }
  weights
}

.dil_sample_evidence_values <- function(values, distribution, n, seed,
                                        salt = 0L) {
  values <- unique(values[!is.na(values) & nzchar(values)])
  if (!length(values)) return(rep(NA_character_, n))
  weights <- .dil_evidence_weights(values, distribution)
  # A documented domain does not imply a uniform distribution. Leave a
  # multi-value field unresolved unless the evidence also supplies weights.
  if (length(values) > 1L && is.null(weights)) return(NULL)
  .dil_sample_values(values, n, seed, salt, weights)
}

.dil_amep_hours_value <- function(name, description, n, seed, salt) {
  upper <- toupper(name)
  supported_name <- grepl(
    paste0(
      "^(?:AMEP|AMEPDL|SLPT|SPP|SPPDL)_[0-9]{4}",
      "(?:_[0-9]{2}|_Q[1-4])?$|",
      "^(?:CHILDCARE|HTS|EXTEND)_[0-9]{4}_Q[1-4]$"
    ),
    upper, perl = TRUE
  )
  supported_description <- grepl(
    "number of hours attended by client", tolower(description), fixed = TRUE
  )
  if (!supported_name || !supported_description) return(NULL)

  quarterly <- grepl("_Q[1-4]$", upper, perl = TRUE)
  entitlement <- grepl("^(?:AMEP|AMEPDL)_", upper, perl = TRUE)
  maximum <- if (quarterly) {
    160
  } else if (entitlement) {
    510
  } else {
    600
  }
  u <- .admin_unit_interval(n, seed, salt)
  attended <- u >= 0.35
  hours <- numeric(n)
  hours[attended] <- round(
    maximum * ((u[attended] - 0.35) / 0.65)^1.6 * 4
  ) / 4
  pmin(hours, maximum)
}

.dil_anzsic_codeframe <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    path <- system.file(
      "extdata", "codeframes", "anzsic2006.tsv", package = "fplida"
    )
    if (!nzchar(path)) {
      path <- file.path("inst", "extdata", "codeframes", "anzsic2006.tsv")
    }
    cache <<- if (file.exists(path)) {
      utils::read.delim(
        path, stringsAsFactors = FALSE, check.names = FALSE,
        colClasses = "character"
      )
    } else {
      data.frame()
    }
    cache
  }
})

.dil_industry_value <- function(name, description, spine_rows, key) {
  if (!"industry" %in% names(spine_rows)) return(NULL)
  upper <- toupper(name)
  if (grepl("FLAG|INDICATOR", upper) ||
      grepl("\\b(?:flag|indicator)\\b", description,
            ignore.case = TRUE, perl = TRUE)) {
    return(NULL)
  }
  division_number <- pmin(pmax(as.integer(spine_rows$industry), 1L), 19L)
  division_code <- LETTERS[division_number]
  codeframe <- .dil_anzsic_codeframe()
  wants_name <- grepl("NAME|DESC|LABEL", upper, perl = TRUE)
  wants_class <- grepl("ANZSIC.*(?:CLASS|4)|CLASS.*ANZSIC", upper,
                       perl = TRUE)
  if (!nrow(codeframe)) {
    if (wants_name) return(NULL)
    return(division_code)
  }
  division_name <- stats::setNames(
    codeframe$anzsic_division, codeframe$anzsic_division_code
  )
  if (!wants_class) {
    if (wants_name) return(unname(division_name[division_code]))
    return(division_code)
  }
  out_code <- character(length(division_code))
  out_name <- character(length(division_code))
  for (division in unique(division_code)) {
    rows <- which(division_code == division)
    choices <- codeframe[
      codeframe$anzsic_division_code == division,
      , drop = FALSE
    ]
    if (!nrow(choices)) next
    chosen <- 1L + as.integer(key[rows] %% nrow(choices))
    out_code[rows] <- choices$anzsic_class_code[chosen]
    out_name[rows] <- choices$anzsic_class[chosen]
  }
  if (wants_name) out_name else out_code
}

.dil_admin_evidence <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    filenames <- c(
      "dhda-health-variable-code-evidence.csv",
      "dss-variable-code-evidence.csv",
      "education-variable-code-evidence.csv",
      "home-affairs-variable-code-evidence.csv",
      "ndis-variable-code-evidence.csv",
      "vet-apprentice-variable-code-evidence.csv"
    )
    paths <- vapply(filenames, function(filename) {
      path <- system.file("internal-docs", filename, package = "fplida")
      if (!nzchar(path)) path <- file.path("inst", "internal-docs", filename)
      path
    }, character(1))
    paths <- paths[file.exists(paths)]
    if (!length(paths)) {
      cache <<- data.frame()
      return(cache)
    }
    rows <- lapply(paths, function(path) {
      frame <- utils::read.csv(
        path, stringsAsFactors = FALSE, check.names = FALSE,
        na.strings = character()
      )
      required <- c(
        "dataset", "variable", "type", "observed_small_domain_values",
        "observed_generated_domain_or_distribution", "evidence_status"
      )
      frame[required]
    })
    cache <<- unique(do.call(rbind, rows))
    rownames(cache) <<- NULL
    cache
  }
})

.dil_admin_domain_statuses <- c(
  "direct_plida_metadata_code_labels_public_collection_context",
  "official_mbs_schedule_anchor_local_lookup_distribution",
  "official_mbs_classification_structure_local_lookup_values",
  "official_dex_score_range_distribution_local",
  "official_tcsi_mode_of_attendance_generated_subset_distribution_local",
  "official_tcsi_attendance_type_generated_subset_distribution_local"
)

.dil_admin_evidence_value <- function(dataset, name, description, n, seed,
                                      salt) {
  evidence <- .dil_admin_evidence()
  if (!nrow(evidence)) return(NULL)
  hits <- which(
    evidence$dataset == dataset &
      toupper(evidence$variable) == toupper(name) &
      nzchar(evidence$observed_small_domain_values)
  )
  if (!length(hits)) return(NULL)
  row <- evidence[hits[[1L]], , drop = FALSE]
  status <- tolower(row$evidence_status[[1L]])
  # Reuse only domains that the evidence audit positively identified as an
  # official/public value set or range. Never infer approval from the absence
  # of a warning word in the status label.
  if (!status %in% .dil_admin_domain_statuses) {
    return(NULL)
  }
  identifier_text <- toupper(paste(name, description))
  unique_identifier <- grepl(
    "AEUID|SCRAM|HASH|DE.?IDENT|UNIQUE|IDENTIFIER|IDENTIFICATION",
    identifier_text, perl = TRUE
  ) && !grepl("CLASSIFICATION|CATEGORY CODE|STATUS CODE", identifier_text)
  if (unique_identifier) {
    return(NULL)
  }
  values <- strsplit(
    row$observed_small_domain_values[[1L]], ";", fixed = TRUE
  )[[1L]]
  values <- unique(trimws(values))
  values <- values[
    nzchar(values) & !tolower(values) %in% c("na", "<na>", "all missing")
  ]
  if (!length(values) || length(values) > 60L) return(NULL)
  if (any(grepl(
    "^[[:alpha:]][[:alnum:]&]*_[0-9]{6,}$|^C[0-9]{2,4}$|placeholder|<placeholder",
    values, ignore.case = TRUE, perl = TRUE
  ))) {
    return(NULL)
  }
  type <- tolower(row$type[[1L]])
  sampled <- .dil_sample_evidence_values(
    values, row$observed_generated_domain_or_distribution[[1L]],
    n, seed, salt
  )
  if (is.null(sampled)) {
    return(switch(
      type,
      logical = rep(NA, n),
      integer = rep(NA_integer_, n),
      numeric = rep(NA_real_, n),
      rep(NA_character_, n)
    ))
  }
  if (type == "logical") {
    parsed <- tolower(sampled)
    if (all(parsed %in% c("true", "false"))) return(parsed == "true")
  }
  if (type %in% c("integer", "numeric") &&
      all(grepl("^-?[0-9]+(?:\\.[0-9]+)?$", sampled, perl = TRUE))) {
    number <- as.numeric(sampled)
    if (type == "integer" && all(number == floor(number))) {
      return(as.integer(number))
    }
    return(number)
  }
  sampled
}

.dil_census_evidence <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    path <- system.file(
      "internal-docs", "census-variable-code-evidence.csv",
      package = "fplida"
    )
    if (!nzchar(path)) {
      path <- file.path(
        "inst", "internal-docs", "census-variable-code-evidence.csv"
      )
    }
    cache <<- if (file.exists(path)) {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      data.frame()
    }
    cache
  }
})

.dil_census_domain_statuses <- c(
  "official_abs_dictionary_small_code_set_current_tested",
  "official_abs_dictionary_dollar_code_current_tested",
  "official_abs_dictionary_numeric_or_year_current_tested"
)

.dil_acld_census_mapping <- local({
  census_variables <- NULL
  function(name) {
    upper <- toupper(name)
    normalised <- sub("_(11_16|16_21)$", "", upper, perl = TRUE)
    normalised <- sub("_(FP|MP|SP|P)_(11|16|21)$", "", normalised,
                      perl = TRUE)
    normalised <- sub("_(FP|MP|SP|P)(11|16|21)$", "", normalised,
                      perl = TRUE)
    normalised <- sub("_(11|16|21)$", "", normalised, perl = TRUE)
    if (identical(normalised, upper)) return(NULL)

    if (is.null(census_variables)) {
      inventory <- .dil_structure_inventory("CENSUS")$variables
      census_variables <<- unique(toupper(inventory[["Variable Name"]]))
    }
    if (!normalised %in% census_variables) return(NULL)

    census_evidence <- .dil_census_evidence()
    evidence_hit <- which(
      toupper(census_evidence$variable) == normalised &
        tolower(census_evidence$evidence_status) %in%
          .dil_census_domain_statuses
    )
    spine_backed <- grepl(
      "^(?:AGEP|DOBYP|DOBMP|SEXP|BPLP|BPFP|BPMP|BPPP|STE[A-Z0-9]*[DP])$",
      normalised, perl = TRUE
    )
    if (!length(evidence_hit) && !spine_backed) return(NULL)

    suffix <- regmatches(
      upper,
      regexec("(?:_|^)(11|16|21)$", upper, perl = TRUE)
    )[[1L]]
    if (!length(suffix)) return(NULL)
    year <- switch(suffix[[2L]], `11` = 2011L, `16` = 2016L, `21` = 2021L)
    role <- regmatches(
      upper,
      regexec("_(FP|MP|SP|P)_?(?:11|16|21)$", upper, perl = TRUE)
    )[[1L]]
    list(
      variable = normalised,
      year = year,
      role = if (length(role)) role[[2L]] else "P"
    )
  }
})

.dil_acld_census_value <- function(name, spine_rows, seed, salt) {
  mapping <- .dil_acld_census_mapping(name)
  if (is.null(mapping)) return(NULL)

  role_spine <- spine_rows
  n <- nrow(role_spine)
  key <- .dil_numeric_key(role_spine, seed, salt)
  if (mapping$role %in% c("FP", "MP")) {
    role_spine$birth_year <- as.integer(role_spine$birth_year) -
      20L - as.integer(key %% 21L)
    role_spine$sex <- if (mapping$role == "FP") 2L else 1L
  } else if (mapping$role == "SP") {
    role_spine$birth_year <- as.integer(role_spine$birth_year) -
      4L + as.integer(key %% 9L)
    role_spine$sex <- ifelse(as.integer(role_spine$sex) == 1L, 2L, 1L)
  }
  .dil_census_value(
    mapping$variable,
    role_spine,
    seed,
    salt,
    paste0("acld_census_", mapping$year)
  )
}

.dil_asgs_2021_value <- function(spine_rows, key, level) {
  if (!"sa2_code" %in% names(spine_rows)) {
    return(rep(NA_character_, nrow(spine_rows)))
  }
  lookup <- .load_mb_lookup()
  if (!nrow(lookup)) return(rep(NA_character_, nrow(spine_rows)))
  out <- rep(NA_character_, nrow(spine_rows))
  spine_sa2 <- suppressWarnings(as.integer(spine_rows$sa2_code))
  for (sa2 in unique(spine_sa2[!is.na(spine_sa2)])) {
    rows <- which(spine_sa2 == sa2)
    choices <- lookup[lookup$sa2_code == sa2, , drop = FALSE]
    if (!nrow(choices)) next
    chosen <- 1L + as.integer(key[rows] %% nrow(choices))
    value <- if (identical(level, "SA1")) {
      choices$sa1_code[chosen]
    } else {
      choices$mb_code[chosen]
    }
    out[rows] <- sprintf("%011.0f", as.numeric(value))
  }
  out
}

.dil_census_value <- function(name, spine_rows, seed, salt, table_name) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_numeric_key(spine_rows, seed, salt)

  if (upper %in% c("SYNTHETIC_AEUID", "C11_PERSON_ID")) {
    if ("aeuid_abs" %in% names(spine_rows)) {
      return(as.character(spine_rows$aeuid_abs))
    }
    return(.dil_character_id("P", spine_rows, seed, salt, 10L))
  }
  if (upper == "DWELLING_ID") {
    household <- if ("household_id" %in% names(spine_rows)) {
      suppressWarnings(as.numeric(spine_rows$household_id))
    } else {
      key
    }
    household[!is.finite(household)] <- key[!is.finite(household)]
    return(sprintf("D%010.0f", household %% 1e10))
  }
  if (upper == "FAMILY_ID") {
    household <- if ("household_id" %in% names(spine_rows)) {
      suppressWarnings(as.numeric(spine_rows$household_id))
    } else {
      key
    }
    household[!is.finite(household)] <- key[!is.finite(household)]
    return(sprintf("F%010.0f", household %% 1e10))
  }

  year <- if (grepl("2011", table_name)) 2011L else if (grepl("2016", table_name)) 2016L else 2021L
  age <- pmin(pmax(year - as.integer(spine_rows$birth_year), 0L), 115L)
  if (upper == "AGEP") return(age)
  if (upper == "DOBYP") return(as.integer(spine_rows$birth_year))
  if (upper == "DOBMP") {
    if ("month_of_birth" %in% names(spine_rows)) return(as.integer(spine_rows$month_of_birth))
    return(1L + as.integer(key %% 12L))
  }
  if (upper == "SEXP") return(as.integer(spine_rows$sex))
  if (grepl("^STE", upper)) return(as.integer(spine_rows$state))
  current_usual_geography <- year == 2021L && upper %in% c(
    "SA1UCD", "SA1UCP", "SA2UCD", "SA2UCP", "SA3UCD", "SA3UCP",
    "SA4UCD", "SA4UCP", "MBUCD", "MBUCP"
  )
  if (current_usual_geography && startsWith(upper, "SA2")) {
    return(as.integer(spine_rows$sa2_code))
  }
  if (current_usual_geography && startsWith(upper, "SA3")) {
    return(as.integer(spine_rows$sa3_code))
  }
  if (current_usual_geography && startsWith(upper, "SA4")) {
    return(as.integer(spine_rows$sa4_code))
  }
  if (current_usual_geography && startsWith(upper, "SA1")) {
    return(.dil_asgs_2021_value(spine_rows, key, "SA1"))
  }
  if (current_usual_geography && startsWith(upper, "MB")) {
    return(.dil_asgs_2021_value(spine_rows, key, "MB"))
  }
  if (upper %in% c("BPLP", "BPFP", "BPMP", "BPPP")) {
    if ("country_of_birth_sacc" %in% names(spine_rows)) {
      return(sprintf("%04d", as.integer(spine_rows$country_of_birth_sacc)))
    }
  }
  # Preserve the reviewed current-generator distributions for fields already
  # covered by the Census evidence register. The source-specific completion
  # rules below are for canonical fields that those generators did not fill.
  evidence <- .dil_census_evidence()
  if (nrow(evidence) && year == 2021L) {
    allowed <- tolower(evidence$evidence_status) %in%
      .dil_census_domain_statuses
    hit <- match(upper, toupper(evidence$variable[allowed]))
    if (!is.na(hit)) {
      evidence <- evidence[allowed, , drop = FALSE]
      values <- strsplit(
        as.character(evidence$observed_small_domain_values[[hit]]),
        ";", fixed = TRUE
      )[[1L]]
      values <- trimws(values)
      # Identifier domains in the evidence register are observed examples,
      # not reusable classifications.
      if (!grepl("_ID$|AEUID", upper) && length(values) <= 40L) {
        sampled <- .dil_sample_evidence_values(
          values,
          evidence$observed_generated_domain_or_distribution[[hit]],
          n, seed, salt
        )
        if (!is.null(sampled) && any(!is.na(sampled))) return(sampled)
      }
    }
  }
  source_value <- .dil_census_source_value(
    name, spine_rows, seed, salt, table_name
  )
  if (!is.null(source_value)) return(source_value)
  if (grepl("^(?:SA[1-4]|MB|LGA|POA|POW)", upper, perl = TRUE)) {
    return(rep(NA_character_, n))
  }
  if (upper %in% c("ANC1P", "ANC2P")) return(rep(NA_character_, n))
  if (grepl("^(IF|IMP)", upper)) {
    return(rep(NA_integer_, n))
  }

  if (grepl("^(RNT|MRE)[A-Z0-9]*D$", upper)) {
    return(sprintf("%04d", 100L + as.integer(key %% 1900L)))
  }
  # The Census dictionary has many field-specific code frames. Do not imply
  # that an unresolved field uses a generic binary classification.
  rep(NA_character_, n)
}

.dil_traveller_wide_spec <- function(name) {
  upper <- toupper(name)
  patterns <- list(
    list(
      pattern = "^PP_WT_([0-9]{4})M([1-9]|1[0-2])$",
      kind = 1L, storage = "double"
    ),
    list(
      pattern = "^PP_STATUS_([0-9]{4})M([1-9]|1[0-2])$",
      kind = 2L, storage = "integer"
    ),
    list(
      pattern = "^ERP_STATUS_([0-9]{4})Q([1-4])$",
      kind = 3L, storage = "integer"
    ),
    list(
      pattern = "^CENSUS_([0-9]{4})$",
      kind = 4L, storage = "integer", period = 3L
    ),
    list(
      pattern = "^PP_CENSUS_([0-9]{4})$",
      kind = 5L, storage = "integer", period = 3L
    )
  )
  for (spec in patterns) {
    matched <- regmatches(upper, regexec(spec$pattern, upper, perl = TRUE))[[1L]]
    if (!length(matched)) next
    return(list(
      kind = spec$kind,
      year = as.integer(matched[[2L]]),
      period = if (!is.null(spec$period)) {
        spec$period
      } else {
        as.integer(matched[[3L]])
      },
      storage = spec$storage
    ))
  }
  NULL
}

.dil_traveller_residence_seed <- function(spine_rows, seed) {
  n <- nrow(spine_rows)
  identity <- if ("id" %in% names(spine_rows)) {
    suppressWarnings(as.numeric(spine_rows$id))
  } else {
    seq_len(n)
  }
  identity[!is.finite(identity)] <- seq_len(n)[!is.finite(identity)]
  fallback <- as.integer(
    (identity * 1103515245 + as.double(seed) + 3600) %% 2147483646 + 1
  )
  if (!"residence_seed" %in% names(spine_rows)) return(fallback)
  out <- as.integer(spine_rows$residence_seed)
  out[is.na(out)] <- fallback[is.na(out)]
  out
}

.dil_traveller_wide_value <- function(name, spine_rows, seed) {
  spec <- .dil_traveller_wide_spec(name)
  if (is.null(spec)) return(NULL)
  n <- nrow(spine_rows)
  optional_integer <- function(field) {
    if (field %in% names(spine_rows)) {
      return(as.integer(spine_rows[[field]]))
    }
    rep(NA_integer_, n)
  }
  country_of_birth <- if ("country_of_birth" %in% names(spine_rows)) {
    as.integer(spine_rows$country_of_birth)
  } else if ("country_of_birth_sacc" %in% names(spine_rows)) {
    sacc <- as.integer(spine_rows$country_of_birth_sacc)
    ifelse(is.na(sacc), NA_integer_, as.integer(sacc != 1101L))
  } else {
    rep(NA_integer_, n)
  }
  values <- .project_traveller_dil_values__(
    kind = spec$kind,
    year = spec$year,
    period = spec$period,
    birth_year = as.integer(spine_rows$birth_year),
    month_of_birth = if ("month_of_birth" %in% names(spine_rows)) {
      as.integer(spine_rows$month_of_birth)
    } else {
      rep(7L, n)
    },
    country_of_birth = country_of_birth,
    year_of_arrival = optional_integer("year_of_arrival"),
    year_of_death = optional_integer("year_of_death"),
    month_of_death = optional_integer("month_of_death"),
    day_of_death = optional_integer("day_of_death"),
    residence_seed = .dil_traveller_residence_seed(spine_rows, seed)
  )[["values"]]
  if (identical(spec$storage, "integer")) as.integer(values) else as.numeric(values)
}

.dil_traveller_reference_value <- function(name, spine_rows, seed, period) {
  upper <- toupper(name)
  fields <- c(
    "COUNTRY_OF_CITIZENSHIP", "VISA_STREAM_CODE", "VISA_SUBCLASS",
    "NOM_DIRECTION", "VISA_APPLICANT_TYPE"
  )
  if (!upper %in% fields) return(NULL)
  n <- nrow(spine_rows)
  if (upper == "VISA_STREAM_CODE") {
    # The DIL publishes only the 0000 no-valid-value fallback. It does not
    # publish the stream codeframe needed to translate a valid subclass.
    return(rep(NA_character_, n))
  }
  optional_integer <- function(field) {
    if (field %in% names(spine_rows)) {
      return(as.integer(spine_rows[[field]]))
    }
    rep(NA_integer_, n)
  }
  country_of_birth_sacc <- if (
    "country_of_birth_sacc" %in% names(spine_rows)
  ) {
    as.integer(spine_rows$country_of_birth_sacc)
  } else if ("country_of_birth" %in% names(spine_rows)) {
    ifelse(
      as.integer(spine_rows$country_of_birth) == 0L,
      1101L, NA_integer_
    )
  } else {
    rep(NA_integer_, n)
  }
  citizenship <- if ("citizenship" %in% names(spine_rows)) {
    as.integer(spine_rows$citizenship)
  } else {
    ifelse(country_of_birth_sacc == 1101L, 1L, 2L)
  }
  reference_year <- as.integer(period$end_year)
  if (upper == "NOM_DIRECTION") {
    # No public PLIDA code labels are available. A and D are an explicit
    # local movement map for arrival and departure, linked to arrival year.
    key <- .dil_traveller_residence_seed(spine_rows, seed)
    arrived <- optional_integer("year_of_arrival") == reference_year
    arrived[is.na(arrived)] <- FALSE
    return(ifelse(arrived, "A", ifelse((key + reference_year) %% 2L == 0L,
                                       "A", "D")))
  }
  values <- .project_traveller_reference_values__(
    reference_year = reference_year,
    citizenship = citizenship,
    country_of_birth_sacc = country_of_birth_sacc,
    birth_year = optional_integer("birth_year"),
    year_of_arrival = optional_integer("year_of_arrival"),
    year_of_death = optional_integer("year_of_death"),
    residence_seed = .dil_traveller_residence_seed(spine_rows, seed)
  )
  if (upper == "VISA_APPLICANT_TYPE") {
    subclass <- as.character(values[["VISA_SUBCLASS"]])
    key <- .dil_traveller_residence_seed(spine_rows, seed)
    out <- ifelse(key %% 100L < 65L, "Primary", "Secondary")
    # Australian citizens do not have a visa applicant role for the
    # reference period.
    out[subclass == "001"] <- NA_character_
    return(out)
  }
  as.character(values[[upper]])
}

.dil_general_value <- function(name, description, dataset, product_name,
                               table_name, module_name, spine_rows, seed,
                               survey_values_deferred = FALSE,
                               period = NULL) {
  upper <- toupper(name)
  text <- toupper(paste(name, description))
  n <- nrow(spine_rows)
  salt <- .stable_name_seed(paste(product_name, table_name, name, sep = "|"))
  link_salt <- .stable_name_seed(
    paste(dataset, .dil_link_family(name), sep = "|")
  )
  if (is.null(period)) {
    period <- .dil_period(
      c(product_name, table_name, module_name),
      dataset = dataset,
      table_name = table_name
    )
  }
  key <- .dil_numeric_key(spine_rows, seed, salt)
  link_key <- .dil_numeric_key(spine_rows, seed, link_salt)
  u <- .admin_unit_interval(n, seed, salt)
  amount_suffix <- grepl(
    "(?:^|_)(?:AMT|AMOUNT)(?:$|_[0-9]+$)|(?:AMT|AMOUNT)[0-9_]*$",
    upper, perl = TRUE
  )
  amount_name <- amount_suffix || grepl(
    "(?:^|_)(?:INCM|INCOME|GROSS|GRS|WAGE|WAGES|SALARY|TAX|WHELD|SUPER|CNTRBTN|CONTRIBUTION|BALANCE|VALUE|COST|EXPENSE|LOSS|DEBT|RENT)(?:$|_)",
    upper, perl = TRUE
  ) || grepl(
    "\\b(?:amount|income|payment|payments|dollar|dollars|wage|wages|salary|tax|withheld|superannuation|contribution|balance|value|cost|expense|loss|debt|rent|premium|premiums)\\b",
    description, ignore.case = TRUE, perl = TRUE
  )
  categorical_name <- grepl(
    "STATUS|TYPE|CLASS|CATEGORY|CODE|_CD$|_CDE$|FLAG|DUMMY|(?:^|_)(?:IND|INDICATOR)(?:$|_)",
    upper, perl = TRUE
  ) || grepl(
    "^\\s*whether|\\b(?:flag|indicator|status code|category code|classification code)\\b",
    description, ignore.case = TRUE, perl = TRUE
  )
  count_name <- grepl(
    "(?:^|_)COUNT(?:$|_)|COUNT$|^COUNT_|(?:^|_)(?:CNT|NUM|QTY|QUANTITY)(?:$|_)|NUMBER_?OF",
    upper, perl = TRUE
  ) || grepl(
    "\\b(?:count|number of|quantity)\\b", description,
    ignore.case = TRUE, perl = TRUE
  )
  description_date <- !amount_suffix && !count_name &&
    grepl("\\bdate\\b", description, ignore.case = TRUE, perl = TRUE) &&
    !grepl("\\bto date\\b", description, ignore.case = TRUE, perl = TRUE)
  date_name <- grepl(
    "(?:^|_)(?:DATE|DT)(?:$|_)|(?:DATE|DT)$|^DATE|CMNCMT|CESTN|VALIDFROM|VALIDTO|EFFECTIVEFROM|EFFECTIVETO",
    upper, perl = TRUE
  ) || (
    grepl("(?:^|_)(?:START|END|ENDING)(?:$|_)", upper, perl = TRUE) &&
      grepl("\\b(?:date|week ending|period ending)\\b", description,
            ignore.case = TRUE, perl = TRUE)
  ) || description_date
  year_name <- !amount_suffix && !categorical_name && !count_name && grepl(
    "(?:^|_)(?:YEAR|YR)(?:$|_)|YEAR$|^YEAR",
    upper, perl = TRUE
  )
  age_name <- !amount_suffix && !amount_name && !categorical_name && (
    grepl(
      "^AGE(?:$|[0-9_])|(?:^|_)AGE(?:$|_)", upper, perl = TRUE
    ) || grepl("\\bage\\b", description, ignore.case = TRUE, perl = TRUE)
  )

  agency <- dataset_to_agency(dataset)
  aeuid_name <- paste0("aeuid_", tolower(agency))
  if (upper == "SYNTHETIC_AEUID") {
    if (aeuid_name %in% names(spine_rows)) {
      return(as.character(spine_rows[[aeuid_name]]))
    }
    return(.dil_character_id("P", spine_rows, seed, link_salt))
  }
  if (upper == "SPINE_ID" && "spine_id" %in% names(spine_rows)) {
    return(as.character(spine_rows$spine_id))
  }
  if (!survey_values_deferred) {
    geography_value <- .dil_admin_geography_value(
      name, spine_rows, seed, period
    )
    if (!is.null(geography_value)) return(geography_value)
  }
  if (dataset == "CENSUS") {
    return(.dil_census_value(name, spine_rows, seed, salt, table_name))
  }
  if (dataset == "ACLD") {
    acld_value <- .dil_acld_census_value(
      name, spine_rows, seed, link_salt
    )
    if (!is.null(acld_value)) return(acld_value)
  }
  if (dataset == "TRAVELLERS") {
    traveller_value <- .dil_traveller_reference_value(
      name, spine_rows, seed, period
    )
    if (!is.null(traveller_value)) return(traveller_value)
    traveller_value <- .dil_traveller_wide_value(name, spine_rows, seed)
    if (!is.null(traveller_value)) return(traveller_value)
  }

  if (survey_values_deferred) {
    if (grepl("DATE|_DT$", upper)) return(rep(as.Date(NA), n))
    if (grepl("AMT|COUNT|_CNT$|YEAR|_YR$|MONTH|AGE|SCORE|WEIGHT|RATE", upper)) {
      return(rep(NA_real_, n))
    }
    return(rep(NA_character_, n))
  }

  evidence_value <- .dil_admin_evidence_value(
    dataset, name, description, n, seed, salt
  )
  if (!is.null(evidence_value)) return(evidence_value)

  if (upper %in% c("BN", "ABN")) return(.admin_abn(n, seed, name))
  if (grepl("ABN_HASH|ARID_HASH", upper)) {
    return(paste0("H", sprintf("%015.0f", link_key %% 1e15)))
  }
  if (grepl("ABSRID|ABSPID|PERSON_ID|CLIENT_ID|PARTICIPANT_ID", upper)) {
    if (aeuid_name %in% names(spine_rows)) {
      return(as.character(spine_rows[[aeuid_name]]))
    }
  }
  if (grepl("ANZSCO|OCCUP", upper)) {
    occupation_label <- grepl("NAME|DESC|LABEL", upper, perl = TRUE)
    occupation_flag <- grepl("FLAG|INDICATOR", upper, perl = TRUE) ||
      grepl("\\b(?:flag|indicator)\\b", description,
            ignore.case = TRUE, perl = TRUE)
    if (!occupation_label && !occupation_flag &&
        "anzsco_code" %in% names(spine_rows)) {
      return(sprintf("%06d", as.integer(spine_rows$anzsco_code)))
    }
  }
  if (grepl("ANZSIC|INDUSTRY", upper)) {
    industry_value <- .dil_industry_value(
      name, description, spine_rows, link_key
    )
    if (!is.null(industry_value)) return(industry_value)
  }
  if (grepl("DWELLING_ID|HOUSEHOLD_ID|FAMILY_ID", upper)) {
    prefix <- if (grepl("DWELL", upper)) "D" else if (grepl("FAMILY", upper)) "F" else "H"
    return(.dil_character_id(prefix, spine_rows, seed, link_salt, 10L))
  }
  if (grepl("SCRAM|HASH|GUID|UUID|SCOREID|IDENTIFIER|IDNTFR|(^|_)(KEY|SID)$|_SEQ$", upper) ||
      grepl(
        "(?:PLAN|PROVIDER|PRVDR|CASE|EMPLOYER|PARTICIPANT|CLIENT).*(?:ID|NMBR|NUMBER|KEY|SEQ)$",
        upper, perl = TRUE
      ) ||
      (grepl("(^|_)ID$|NMBR$|NUMBER$", upper) &&
         grepl("UNIQUE|IDENTIF|REFERENCE NUMBER|RECORD NUMBER", text))) {
    return(sprintf("%012.0f", link_key %% 1e12))
  }

  if (grepl("BIRTH|BRTH|DOB", upper) && grepl("YEAR|_YR$", upper)) {
    return(as.integer(spine_rows$birth_year))
  }
  if (grepl("BIRTH|BRTH|DOB", upper) && grepl("MONTH|_MNTH$", upper)) {
    if ("month_of_birth" %in% names(spine_rows)) return(as.integer(spine_rows$month_of_birth))
    return(1L + as.integer(key %% 12L))
  }
  if (grepl("DEATH|DTH", upper) && grepl("YEAR|_YR$", upper)) {
    if ("year_of_death" %in% names(spine_rows)) return(as.integer(spine_rows$year_of_death))
    return(rep(NA_integer_, n))
  }
  if (grepl("DEATH|DTH", upper) && grepl("MONTH|_MNTH$", upper)) {
    if ("month_of_death" %in% names(spine_rows)) return(as.integer(spine_rows$month_of_death))
    return(rep(NA_integer_, n))
  }
  if (grepl("BIRTH|BRTH|DOB", upper) && grepl("DATE|_DT$", upper)) {
    return(as.Date(sprintf("%04d-07-01", as.integer(spine_rows$birth_year))))
  }
  if (grepl("DEATH|DTH", upper) && grepl("DATE|_DT$", upper)) {
    if (all(c("year_of_death", "month_of_death", "day_of_death") %in% names(spine_rows))) {
      text_date <- sprintf(
        "%04d-%02d-%02d", as.integer(spine_rows$year_of_death),
        as.integer(spine_rows$month_of_death), as.integer(spine_rows$day_of_death)
      )
      text_date[is.na(spine_rows$year_of_death)] <- NA_character_
      return(as.Date(text_date))
    }
    return(rep(as.Date(NA), n))
  }
  if (upper %in% c("FIN_YEAR_ENDING", "FINANCIAL_YEAR_ENDING")) {
    return(rep(period$end, n))
  }
  if (upper == "FIN_YR") return(rep(as.integer(period$start_year), n))
  if (grepl("FIN_YEAR|FINANCIAL_YEAR|FNCL_YR|INCOME_YEAR", upper)) {
    return(rep(sprintf("%04d-%02d", period$start_year,
                       period$end_year %% 100L), n))
  }
  if (grepl("SEX|GENDER", upper)) {
    return(ifelse(as.integer(spine_rows$sex) == 1L, "M", "F"))
  }
  if (grepl("INDIGENOUS|INDIG_STAT|ATSI", upper)) {
    if ("indigenous" %in% names(spine_rows)) return(as.integer(spine_rows$indigenous))
  }
  if (grepl(
    "COUNTRY.*BIRTH|BIRTH.*COUNTRY|(?:^|_)COB(?:$|_)", upper,
    perl = TRUE
  )) {
    if ("country_of_birth_sacc" %in% names(spine_rows)) {
      return(sprintf("%04d", as.integer(spine_rows$country_of_birth_sacc)))
    }
  }
  if (grepl("ARRIVAL.*YEAR|YEAR.*ARRIVAL", upper) &&
      "year_of_arrival" %in% names(spine_rows)) {
    return(as.integer(spine_rows$year_of_arrival))
  }
  state_indicator <- grepl(
    "STATE_MOVE|IMP_FLAG|ELECTORATE", upper, perl = TRUE
  ) || grepl("\\b(?:flag|indicator)\\b", description,
             ignore.case = TRUE, perl = TRUE)
  state_geography <- grepl(
    "(?:^|_)STATE(?:$|_)|^STATE(?:CODE|CD|CDE)$|(?:^|_)STE(?:$|_)",
    upper, perl = TRUE
  ) || (grepl("STATE", upper, fixed = TRUE) &&
          grepl("\\bstate\\b|state or territory|state/territory",
                description, ignore.case = TRUE, perl = TRUE))
  if (state_geography && !state_indicator) {
    return(as.integer(spine_rows$state))
  }
  if (grepl("STATE_MOVE", upper, fixed = TRUE)) {
    return(as.integer(u < 0.5))
  }
  geography_name <- grepl("NAME|LABEL|DESCRIPTION|DESC", upper, perl = TRUE)
  if (grepl("SA2", upper) && !geography_name &&
      "sa2_code" %in% names(spine_rows)) {
    return(as.integer(spine_rows$sa2_code))
  }
  if (grepl("SA3", upper) && !geography_name &&
      "sa3_code" %in% names(spine_rows)) {
    return(as.integer(spine_rows$sa3_code))
  }
  if (grepl("SA4", upper) && !geography_name &&
      "sa4_code" %in% names(spine_rows)) {
    return(as.integer(spine_rows$sa4_code))
  }
  if (grepl("POSTCODE|POST_CODE", upper)) {
    return(.admin_postcode(spine_rows$state, seed, salt))
  }
  if (grepl("SA1|MESH|(^|_)MB(_|$)", upper, perl = TRUE)) {
    return(.dil_asgs_2021_value(spine_rows, key, "MB"))
  }
  if (grepl("LGA", upper) && !geography_name) {
    return(rep(NA_character_, n))
  }

  age_group <- age_name && grepl(
    "CATEGORY|GROUP|RANGE|BAND|CLASS", text, perl = TRUE
  )
  if (age_group) return(rep(NA_character_, n))
  if (age_name && "birth_year" %in% names(spine_rows)) {
    return(pmin(pmax(period$end_year - as.integer(spine_rows$birth_year), 0L), 115L))
  }
  if (dataset == "AMEP") {
    amep_hours <- .dil_amep_hours_value(
      name, description, n, seed, salt
    )
    if (!is.null(amep_hours)) return(amep_hours)
  }
  if (grepl("HOUR", upper)) {
    if ("baseline_hours" %in% names(spine_rows)) {
      return(pmax(as.numeric(spine_rows$baseline_hours), 0))
    }
    return(round(5 + 45 * u, 1))
  }

  if (date_name) {
    span <- max(0L, as.integer(period$end - period$start))
    fraction <- if (grepl(
      "(?:^|_)(?:END|ENDING|CEAS|CESTN|LATEST|EXIT)(?:$|_)|ENDDATE$",
      upper, perl = TRUE
    )) {
      0.55 + 0.45 * u
    } else if (grepl(
      "(?:^|_)(?:START|CMNC|CMNCMT|COMMENC|EARLIEST|ENTRY)(?:$|_)|STARTDATE$",
      upper, perl = TRUE
    )) {
      0.55 * u
    } else {
      u
    }
    return(period$start + floor(fraction * span))
  }
  if (year_name) return(rep(as.integer(period$end_year), n))
  if (grepl("(?:^|_)(?:MONTH|MNTH)(?:$|_)|MONTH$|MNTH$", upper,
            perl = TRUE)) {
    return(1L + as.integer(key %% 12L))
  }
  if (grepl("(?:^|_)(?:QUARTER|QTR)(?:$|_)|QUARTER$|QTR$", upper,
            perl = TRUE)) {
    return(1L + as.integer(key %% 4L))
  }

  if (grepl(
    "FLAG|DUMMY|(?:^|_)(?:IND|INDICATOR)(?:$|_)", upper, perl = TRUE
  ) || grepl(
    "^\\s*whether|\\b(?:flag|indicator)\\b", description,
    ignore.case = TRUE, perl = TRUE
  )) {
    return(as.integer(u < 0.5))
  }
  if (grepl("YES[ /-]*NO|\\byes or no\\b", text, perl = TRUE)) {
    return(ifelse(u < 0.5, "Y", "N"))
  }
  if (dataset == "DEATHS" &&
      grepl("^UCOD$|^ENTITY[0-9]+$|^RACS[0-9]+$", upper)) {
    return(.dil_sample_values(
      c("I21", "C34", "C50", "E11", "F03", "G30", "J44", "N18", "U07.1"),
      n, seed, salt
    ))
  }
  if (categorical_name) {
    # A typed missing value keeps an unresolved code frame visible to the
    # release audit. Generic 1/2/9 values would conceal the evidence gap.
    return(rep(NA_character_, n))
  }

  if (amount_name) {
    income <- if ("baseline_income" %in% names(spine_rows)) {
      pmax(as.numeric(spine_rows$baseline_income), 10000)
    } else {
      rep(50000, n)
    }
    multiplier <- if (grepl("BAL|ASSET|VALUE", text)) {
      0.5 + 4.5 * u
    } else if (grepl("TAX|WHELD|OFFSET|LEVY", text)) {
      0.02 + 0.28 * u
    } else if (grepl("COST|EXP|DEDUCT|FEE", text)) {
      0.002 + 0.12 * u
    } else {
      0.05 + 1.10 * u
    }
    value <- round(income * multiplier, 2)
    if (grepl("LOSS|RFNDBL|REFUND", text)) {
      value[(key %% 5L) == 0L] <- -value[(key %% 5L) == 0L]
    }
    return(value)
  }
  if (count_name) {
    return(as.integer(key %% 8L))
  }
  if (grepl("PERCENT|PCT|PERC", upper) ||
      grepl("%|\\bpercent(?:age)?\\b", description,
            ignore.case = TRUE, perl = TRUE)) {
    return(round(100 * u, 2))
  }
  if (grepl("(?:^|_)(?:RATE|RATIO|PROPORTION)(?:$|_)", upper,
            perl = TRUE) ||
      grepl("\\b(?:rate|ratio|proportion)\\b", description,
            ignore.case = TRUE, perl = TRUE)) {
    return(round(u, 4))
  }
  if (grepl("SCORE|INDEX", upper)) {
    limit <- if (dataset == "AEDC") 10 else 100
    return(round(limit * u, 2))
  }
  if (geography_name && grepl(
    "SA1|SA2|SA3|SA4|LGA|STATE|ELECTORATE|POSTCODE",
    upper, perl = TRUE
  )) {
    return(rep(NA_character_, n))
  }
  if (geography_name && grepl(
    "ANZSIC|INDUSTRY|ANZSCO|OCCUP", upper, perl = TRUE
  )) {
    return(rep(NA_character_, n))
  }
  if (grepl("NAME|TITLE|ORGANISATION|ORGANIZATION|EMPLOYER", upper)) {
    return(rep(NA_character_, n))
  }
  if (grepl("DESC|DESCRIPTION|TEXT|LABEL", upper)) {
    return(rep(NA_character_, n))
  }
  # Last resort before typed missing: draw from the codes the registry
  # documents, whether sourced or guessed. This sits at the END deliberately.
  # Everything above encodes real semantics the registry cannot know — that the
  # sex of a female parent is 2, that a change flag is 0/1 — and a documented
  # domain must never preempt it. Here, though, the alternative is an empty
  # column, and a labelled value of the right shape beats that.
  registry_column <- .registry_value_column(dataset, name, n, seed, salt)
  if (!is.null(registry_column)) return(registry_column)

  # Preserve unsupported fields as typed missing. The strict administrative
  # audit reports these fields as release blockers.
  rep(NA_character_, n)
}

.dil_reconcile_structure_frame <- function(frame, dataset) {
  order_pair <- function(start_name, end_name) {
    if (!all(c(start_name, end_name) %in% names(frame))) return()
    start <- frame[[start_name]]
    end <- frame[[end_name]]
    if (!inherits(start, "Date") || !inherits(end, "Date")) return()
    comparable <- !is.na(start) & !is.na(end)
    low <- pmin(start[comparable], end[comparable])
    high <- pmax(start[comparable], end[comparable])
    frame[[start_name]][comparable] <<- low
    frame[[end_name]][comparable] <<- high
  }
  order_three <- function(first_name, second_name, third_name) {
    fields <- c(first_name, second_name, third_name)
    if (!all(fields %in% names(frame))) return()
    values <- frame[fields]
    if (!all(vapply(values, inherits, logical(1), what = "Date"))) return()
    complete <- stats::complete.cases(values)
    if (!any(complete)) return()
    ordered <- t(apply(
      as.data.frame(lapply(values, as.numeric))[complete, , drop = FALSE],
      1L, sort
    ))
    for (i in seq_along(fields)) {
      frame[[fields[[i]]]][complete] <<- as.Date(ordered[, i], origin = "1970-01-01")
    }
  }

  order_pair("VALIDFROM", "VALIDTO")
  order_pair("EFFECTIVEFROM", "EFFECTIVETO")
  if (dataset == "NDIS") {
    order_pair("SUPPSTRTDTADJ", "SUPPENDDTADJ")
    order_pair("STARTDATE", "ENDDATE")
    order_pair("FIRSTPAYMENT", "LATESTPAYMENT")
    order_pair("SUPPCATSTRTDT", "SUPPCATENDDT")
    order_pair("PLANEFCTVDT", "PLANEXPRYDT")
    order_three("PYMTRQSTCRTDDT", "RBAPYMTCLRDDT", "RBAPYMTSENTDT")
    order_three("PLANEFCTVDT", "PLANACTVTYSTRTDTM", "PLANEXPRYDT")
    order_pair("RQSTDDT", "CRTDDT")
    order_pair("RQSTDDT", "DCSNDT")
    order_pair("RQSTDDT", "WTHDRWLDT")
  }
  frame
}

.dil_make_structure_frame <- function(variable_rows, source_frame,
                                      spine_rows, dataset, product_name,
                                      table_name, module_name, seed) {
  variables <- unique(variable_rows[["Variable Name"]])
  variables <- variables[!is.na(variables) & nzchar(variables)]
  source_names <- toupper(names(source_frame))
  descriptions <- variable_rows[["Variable Description"]]
  names(descriptions) <- variable_rows[["Variable Name"]]
  deferred <- dataset %in% .dil_deferred_survey_datasets
  period <- .dil_period(
    c(product_name, table_name, module_name),
    dataset = dataset,
    table_name = table_name
  )

  out <- lapply(variables, function(variable) {
    hit <- match(toupper(variable), source_names)
    if (!is.na(hit) && length(source_frame[[hit]]) &&
        (deferred || !all(is.na(source_frame[[hit]])))) {
      return(source_frame[[hit]])
    }
    description <- descriptions[[variable]]
    if (is.null(description) || is.na(description)) description <- ""
    source_value <- .dil_dataset_source_value(
      name = variable,
      description = description,
      dataset = dataset,
      source_frame = source_frame,
      spine_rows = spine_rows,
      seed = seed,
      period = period,
      product_name = product_name,
      table_name = table_name,
      module_name = module_name
    )
    if (!is.null(source_value)) return(source_value)
    .dil_general_value(
      name = variable,
      description = description,
      dataset = dataset,
      product_name = product_name,
      table_name = table_name,
      module_name = module_name,
      spine_rows = spine_rows,
      seed = seed,
      survey_values_deferred = deferred,
      period = period
    )
  })
  names(out) <- variables
  frame <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  frame <- .dil_apply_table_period(
    frame, dataset, product_name, table_name, module_name, seed
  )
  .dil_reconcile_structure_frame(frame, dataset)
}

.complete_plida_dil_structures <- function(run_dir, build_order, seed,
                                           max_rows = 100L,
                                           verbose = TRUE) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Complete DIL structures require the `arrow` package.", call. = FALSE)
  }
  selected <- unname(.dil_build_target_datasets[
    intersect(names(.dil_build_target_datasets), build_order)
  ])
  selected <- unique(selected[!is.na(selected)])
  if (!length(selected)) {
    return(invisible(list(structures_written = 0L, files = character())))
  }

  metadata <- .dil_structure_inventory(selected)
  structures <- metadata$structures
  variables <- metadata$variables
  if (!nrow(structures)) {
    return(invisible(list(structures_written = 0L, files = character())))
  }

  base_path <- file.path(run_dir, "_system", "base-spine.parquet")
  if (!file.exists(base_path)) {
    stop("Base spine is required before DIL structure completion: ",
         base_path, call. = FALSE)
  }
  pool_n <- max(1000L, min(5000L, as.integer(max_rows) * 4L))
  spine_pool <- .dil_read_head(base_path, pool_n)
  if (!nrow(spine_pool)) stop("Base spine is empty.", call. = FALSE)

  source_cache <- new.env(parent = emptyenv())
  alignment_cache <- new.env(parent = emptyenv())
  product_keys <- paste(
    structures$Dataset, structures[["Product Name"]], sep = "\r"
  )
  product_table_counts <- table(product_keys)
  dataset_indices <- stats::setNames(
    vector("list", length(selected)), selected
  )
  files_written <- character(nrow(structures))
  rows_written <- integer(nrow(structures))

  for (dataset in selected) {
    ds_dir <- dataset_dir(run_dir, dataset)
    dataset_structures <- structures[
      structures$Dataset == dataset, , drop = FALSE
    ]
    dataset_indices[[dataset]] <- .dil_structure_candidate_index(
      ds_dir,
      canonical_outputs = .dil_canonical_structure_outputs(
        dataset_structures
      )
    )
  }

  for (i in seq_len(nrow(structures))) {
    dataset <- structures$Dataset[[i]]
    product_name <- structures[["Product Name"]][[i]]
    table_name <- structures[["Table Name"]][[i]]
    module_name <- structures[["Module Name"]][[i]]
    variable_rows <- variables[
      variables$Dataset == dataset &
        variables[["Product Name"]] == product_name &
        variables[["Table Name"]] == table_name,
      ,
      drop = FALSE
    ]
    variable_names <- variable_rows[["Variable Name"]]
    source_path <- .dil_best_structure_source(
      dataset_indices[[dataset]], product_name, table_name, variable_names,
      allow_product_source = .dil_allow_product_source(
        dataset,
        product_table_counts[[paste(dataset, product_name, sep = "\r")]]
      ),
      source_aliases = .dil_structure_source_aliases(
        dataset, product_name, table_name
      )
    )
    source_frame <- NULL
    aligned <- NULL
    if (!is.null(source_path)) {
      if (exists(source_path, envir = source_cache, inherits = FALSE)) {
        source_frame <- get(source_path, envir = source_cache, inherits = FALSE)
      } else {
        source_rows <- if (dataset %in% c("MBS", "PBS")) {
          as.integer(min(as.numeric(max_rows) * 24, .Machine$integer.max))
        } else if (identical(dataset, "CORE")) {
          # CORE relationships are written partner-first, followed by
          # parent-child rows. Read enough to sample either group.
          as.integer(min(as.numeric(max_rows) * 4, .Machine$integer.max))
        } else {
          as.integer(max_rows)
        }
        source_frame <- .dil_read_head(source_path, source_rows)
        assign(source_path, source_frame, envir = source_cache)
      }
      source_frame <- .dil_filter_product_source(
        source_frame, dataset, product_name, table_name, module_name,
        max_rows
      )
      aligned <- .dil_align_source_to_spine(
        source_frame, spine_pool, dataset
      )
      source_has_person_key <- any(
        toupper(names(source_frame)) %in% c("SYNTHETIC_AEUID", "SPINE_ID")
      )
      if (is.null(aligned) && source_has_person_key) {
        source_names <- toupper(names(source_frame))
        source_key <- if ("SYNTHETIC_AEUID" %in% source_names) {
          names(source_frame)[match("SYNTHETIC_AEUID", source_names)]
        } else {
          names(source_frame)[match("SPINE_ID", source_names)]
        }
        alignment_key <- paste(
          source_path, dataset,
          paste(as.character(source_frame[[source_key]]), collapse = "\r"),
          sep = "\n"
        )
        if (exists(alignment_key, envir = alignment_cache,
                   inherits = FALSE)) {
          cached_alignment <- get(
            alignment_key, envir = alignment_cache, inherits = FALSE
          )
          if (!identical(cached_alignment, FALSE)) {
            aligned <- cached_alignment
            aligned$source <- source_frame[
              cached_alignment$source_index, , drop = FALSE
            ]
          }
        } else {
          agency <- tolower(dataset_to_agency(dataset))
          agency_spine_path <- file.path(
            dataset_dir(run_dir, dataset), paste0(agency, "-spine.parquet")
          )
          aligned <- .dil_align_source_to_full_spine(
            source_frame, base_path, agency_spine_path, dataset
          )
          assign(
            alignment_key,
            if (is.null(aligned)) FALSE else aligned,
            envir = alignment_cache
          )
        }
      }
      if (is.null(aligned)) {
        # A person-level source must match the canonical spine. Family,
        # dwelling, and other aggregate sources can be reused without a
        # person key because their native identifiers define the record.
        if (source_has_person_key) source_frame <- NULL
      } else {
        source_frame <- aligned$source
      }
    }
    if (is.null(source_frame) || !nrow(source_frame)) {
      target_n <- min(nrow(spine_pool), as.integer(max_rows))
      source_frame <- data.frame(.row = seq_len(target_n))
    }
    n_rows <- nrow(source_frame)
    structure_salt <- .stable_name_seed(paste(
      dataset, product_name, table_name, sep = "|"
    ))
    spine_rows <- if (is.null(aligned)) {
      .dil_recycle_spine(
        spine_pool, dataset, n_rows, seed, structure_salt
      )
    } else {
      aligned$spine
    }
    frame <- .dil_make_structure_frame(
      variable_rows = variable_rows,
      source_frame = source_frame,
      spine_rows = spine_rows,
      dataset = dataset,
      product_name = product_name,
      table_name = table_name,
      module_name = module_name,
      seed = as.integer(seed)
    )

    stem <- .dil_structure_stem(product_name, table_name)
    path <- file.path(dataset_dir(run_dir, dataset), paste0(stem, ".parquet"))
    arrow::write_parquet(frame, path)
    files_written[[i]] <- path
    rows_written[[i]] <- nrow(frame)
  }

  if (verbose) {
    message(
      "  DIL structures: ", length(files_written),
      " files across ", length(unique(structures$Dataset)), " datasets"
    )
  }
  invisible(list(
    structures_written = length(files_written),
    datasets = sort(unique(structures$Dataset)),
    files = files_written,
    rows = rows_written
  ))
}
