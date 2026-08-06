# Build the package dataset and variable information files from source metadata.

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]),
                         mustWork = TRUE))
  }
  normalizePath(
    file.path("data-raw", "update_variable_info.R"),
    mustWork = TRUE
  )
}

.repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."),
  winslash = "/",
  mustWork = TRUE
)

source(file.path(.repo_root, "data-raw", "variable_info_topics.R"))

.read_csv <- function(path, na.strings = "") {
  if (!file.exists(path)) {
    stop("Required input not found: ", path, call. = FALSE)
  }
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = na.strings,
    comment.char = "",
    fileEncoding = "UTF-8"
  )
}

.text <- function(x) {
  x <- enc2utf8(as.character(x))
  x[is.na(x)] <- ""
  x <- gsub("_x000D_", " ", x, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
}

.collapse <- function(x, separator = " | ") {
  x <- unique(.text(x))
  x <- x[nzchar(x)]
  paste(x, collapse = separator)
}

.first <- function(x) {
  x <- .text(x)
  x <- x[nzchar(x)]
  if (length(x)) x[[1L]] else ""
}

.json_array <- function(x) {
  x <- unique(.text(x))
  x <- x[nzchar(x)]
  if (!length(x)) return("[]")
  x <- gsub("\\\\", "\\\\\\\\", x, perl = TRUE)
  x <- gsub('"', '\\\\"', x, fixed = TRUE)
  paste0('["', paste(x, collapse = '\",\"'), '"]')
}

.urls <- function(x) {
  x <- paste(.text(x), collapse = " | ")
  hit <- regmatches(
    x,
    gregexpr("https?://[^|;[:space:]]+", x, perl = TRUE)
  )[[1L]]
  hit <- unique(gsub("[),.]$", "", hit, perl = TRUE))
  paste(hit[nzchar(hit)], collapse = " | ")
}

.context_year <- function(product, table) {
  context <- paste(product, table)
  hits <- regmatches(
    context,
    gregexpr("20[0-9]{2}", context, perl = TRUE)
  )[[1L]]
  if (length(hits)) as.integer(tail(hits, 1L)) else NA_integer_
}

.nearest_codeframe_year <- function(year, available_years) {
  available_years <- sort(unique(as.integer(available_years)))
  earlier <- available_years[available_years <= year]
  if (length(earlier)) max(earlier) else min(available_years)
}

.pair_key <- function(dataset, variable) {
  paste(.text(dataset), toupper(.text(variable)), sep = "\r")
}

.group_counts <- function(dataset, variable, product, table) {
  key <- .pair_key(dataset, variable)
  split_rows <- split(seq_along(key), key)
  data.frame(
    key = names(split_rows),
    occurrence_count = vapply(split_rows, length, integer(1)),
    product_count = vapply(
      split_rows,
      function(i) length(unique(.text(product[i]))),
      integer(1)
    ),
    table_count = vapply(
      split_rows,
      function(i) length(unique(.text(table[i]))),
      integer(1)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.match_counts <- function(dataset, variable, product, table) {
  counts <- .group_counts(dataset, variable, product, table)
  counts[match(.pair_key(dataset, variable), counts$key), -1L, drop = FALSE]
}

.make_occurrence_id <- function(data) {
  base <- paste(
    data$asset,
    data$record_type,
    data$dataset,
    data$module,
    data$product,
    data$table,
    data$table_number,
    data$source_sheet,
    data$source_item,
    data$variable,
    data$variable_level,
    sep = "::"
  )
  ordinal <- ave(seq_along(base), base, FUN = seq_along)
  ifelse(ordinal == 1L, base, paste0(base, "::", ordinal))
}

.survey_datasets <- c("LFS", "NHS", "NSMHW", "PEX", "SDAC")
.blade_survey_tables <- c(8L:23L, 38L:46L)

.plida_dir <- file.path(.repo_root, "inst", "plida_metadata")
.blade_dir <- file.path(.repo_root, "inst", "blade_metadata")
.docs_dir <- file.path(.repo_root, "inst", "internal-docs")
.codeframe_dir <- file.path(.repo_root, "inst", "extdata", "codeframes")
.plida_dil_url <- paste0(
  "https://www.abs.gov.au/statistics/microdata-tablebuilder/",
  "available-microdata-tablebuilder/",
  "person-level-integrated-data-asset-plida"
)

plida_datasets <- .read_csv(file.path(.plida_dir, "datasets.csv"))
plida_variables <- .read_csv(file.path(.plida_dir, "variables.csv"))
plida_products <- .read_csv(file.path(.plida_dir, "products.csv"))
blade_tables <- .read_csv(file.path(.blade_dir, "tables.csv"))
blade_variables <- .read_csv(file.path(.blade_dir, "variables.csv"))
blade_keys <- .read_csv(file.path(.blade_dir, "keys.csv"))
blade_domains <- .read_csv(file.path(.blade_dir, "domains.csv"))
websites <- .read_csv(file.path(.repo_root, "data-raw", "dataset-websites.csv"))

if (!setequal(websites$dataset,
              c(unique(plida_datasets[["Dataset Acronym"]]), "BLADE"))) {
  stop("dataset-websites.csv does not cover the full dataset inventory.",
       call. = FALSE)
}

# Dataset information contains source metadata only. It contains no generation
# rules or implementation fields. Some source rows describe separate modules
# or products under one dataset acronym. Collapse those rows without discarding
# their distinct source descriptions.
plida_dataset_groups <- split(
  seq_len(nrow(plida_datasets)),
  .text(plida_datasets[["Dataset Acronym"]])
)
dataset_info <- do.call(rbind, lapply(
  plida_dataset_groups,
  function(rows) {
    dataset <- .first(plida_datasets[["Dataset Acronym"]][rows])
    data.frame(
      asset = "PLIDA",
      collection_type = ifelse(
        dataset %in% .survey_datasets,
        "survey",
        "administrative"
      ),
      dataset = dataset,
      dataset_name = .collapse(plida_datasets[["Dataset Name"]][rows]),
      supplier = .collapse(plida_datasets[["Supplier"]][rows]),
      custodian = .collapse(plida_datasets[["Custodian"]][rows]),
      dataset_description = .collapse(
        plida_datasets[["Dataset Description"]][rows]
      ),
      reference_period = .collapse(
        plida_datasets[["Reference Period"]][rows]
      ),
      update_frequency = .collapse(
        plida_datasets[["Update Frequency"]][rows]
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
))

dataset_info <- rbind(
  dataset_info,
  data.frame(
    asset = "BLADE",
    collection_type = "administrative_and_survey",
    dataset = "BLADE",
    dataset_name = "Business Longitudinal Analysis Data Environment",
    supplier = "ABS",
    custodian = "ABS",
    dataset_description = paste(
      "Business-level longitudinal data from administrative and survey",
      "sources."
    ),
    reference_period = paste(
      "Table-specific; periods are recorded by variable_info()."
    ),
    update_frequency = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
)

website_hit <- match(dataset_info$dataset, websites$dataset)
dataset_info$information_source <- websites$source_label[website_hit]
dataset_info$information_url <- websites$url[website_hit]
dataset_info$information_summary <- websites$summary[website_hit]
dataset_info$metadata_source <- ifelse(
  dataset_info$asset == "PLIDA",
  "PLIDA Data Item List",
  "BLADE Data Item List"
)
dataset_info$metadata_vintage <- ifelse(
  dataset_info$asset == "PLIDA",
  "19 March 2026",
  "April 2026"
)
dataset_info <- dataset_info[order(dataset_info$asset, dataset_info$dataset), ]
row.names(dataset_info) <- NULL

# Spell out the supplying agency. `supplier` is an acronym ("DHDA"), which is
# not much use on its own. COMBINED and CORE are derived from several agencies
# and carry the sentinel "Multiple" rather than an acronym.
local({
  agencies <- .read_csv(file.path(.plida_dir, "agencies.csv"))
  lookup <- stats::setNames(agencies[["Agency Description"]], agencies[["Agency"]])
  nm <- unname(lookup[dataset_info$supplier])
  nm[dataset_info$supplier == "Multiple"] <- "Multiple agencies"
  if (anyNA(nm)) {
    stop("No agency description for: ",
         paste(unique(dataset_info$supplier[is.na(nm)]), collapse = ", "),
         call. = FALSE)
  }
  dataset_info$supplier_name <<- nm
})
dataset_info <- dataset_info[, c(
  "asset", "collection_type", "dataset", "dataset_name", "supplier",
  "supplier_name", "custodian", "dataset_description", "reference_period",
  "update_frequency", "information_source", "information_url",
  "information_summary", "metadata_source", "metadata_vintage"
)]

dataset_info_path <- file.path(.repo_root, "inst", "dataset-info.csv")
utils::write.csv(
  dataset_info,
  dataset_info_path,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

.dataset_column <- function(dataset, column) {
  dataset_info[[column]][match(dataset, dataset_info$dataset)]
}

# ---- per-product reference periods ----------------------------------------
#
# A variable's reference period is the period of the products it appears in,
# not the whole dataset's. CENSUS runs 2011, 2016 and 2021, but a variable that
# appears only in the 2021 person product is a 2021 variable.
#
# Products encode their period in the name suffix and/or the description:
#   fy0910 / fy2009-10   financial year
#   YYYY                 a single calendar year
#   YY-YY                a 2-digit span, e.g. 03-19 -> 2003 to 2019
#   YY-YY-YY             discrete years, e.g. 11-16-21 (the Census points)
#   YYYY-current         open-ended
#
# Three traps, all present in the source data:
#   * "fy2009-10" must match before "fy[0-9]{4}", or it reads as fy(2009) and
#     the leading "20" becomes the 2-digit year 2020.
#   * "(updated Apr 2026)" is a publication date and "(Census 2021 version)" is
#     a vintage marker. Neither is a reference period, and both sit next to a
#     real one ("2006-latest (Census 2011 version)"), so both are stripped.
#   * A consecutive year pair is one cycle ("2014-2015"), not two years. The
#     curated dataset periods use that form, so cycles are written in full.

.two_digit_year <- function(x) {
  n <- as.integer(x)
  ifelse(n <= 30L, 2000L + n, 1900L + n)
}

.cycle_label <- function(start_year) sprintf("%d-%d", start_year, start_year + 1L)

.parse_product_period <- function(name, desc) {
  name <- as.character(name)
  desc <- gsub("[(](updated|published)[^)]*[)]", "", as.character(desc),
               ignore.case = TRUE)
  desc <- gsub("[(]Census [0-9]{4} version[)]", "", desc, ignore.case = TRUE)
  blob <- paste(name, desc)
  open <- grepl("current|ongoing|latest", blob, ignore.case = TRUE)

  fy4 <- regmatches(blob, gregexpr("fy(19|20)[0-9]{2}-[0-9]{2}", blob,
                                   ignore.case = TRUE))[[1]]
  if (length(fy4)) {
    return(list(years = integer(0),
                fys = sort(unique(.cycle_label(as.integer(substr(fy4, 3, 6))))),
                open = open))
  }
  fy2 <- regmatches(blob, gregexpr("fy[0-9]{4}(?![0-9-])", blob,
                                   ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(fy2)) {
    starts <- .two_digit_year(substr(fy2, 3, 4))
    return(list(years = integer(0), fys = sort(unique(.cycle_label(starts))),
                open = open))
  }

  m <- regmatches(name, regexpr("(-[0-9]{2}){2,}$", name))
  if (length(m)) {
    yrs <- sort(unique(.two_digit_year(as.integer(
      strsplit(sub("^-", "", m), "-")[[1]]))))
    if (length(yrs) == 2L && yrs[2] == yrs[1] + 1L) {
      return(list(years = integer(0), fys = .cycle_label(yrs[1]), open = open))
    }
    if (length(yrs) == 2L) yrs <- seq(yrs[1], yrs[2])
    return(list(years = yrs, fys = character(0), open = open))
  }

  if (open) {
    m2 <- regmatches(name, regexpr("-([0-9]{2})-current$", name))
    if (length(m2)) {
      return(list(years = .two_digit_year(sub("^-([0-9]{2})-current$", "\\1", m2)),
                  fys = character(0), open = TRUE))
    }
  }

  # Union name with the description's period. The description is often richer:
  # `madipge-nhs-d-survey-2022` is the 2022-2023 NHS cycle and only its
  # description says so.
  #
  # Only the FINAL comma-separated field of the description is a period. The
  # earlier fields carry product codes that look like years — "MADIP GE,
  # 190101d, PBS, Prescription data, 2010" would otherwise yield 1901 — and
  # prose such as "includes 2011 Census".
  desc_tail <- trimws(sub("^.*,", "", desc))
  y <- c(regmatches(name, gregexpr("(19|20)[0-9]{2}", name))[[1]],
         regmatches(desc_tail, gregexpr("(19|20)[0-9]{2}", desc_tail))[[1]])
  if (length(y)) {
    yrs <- sort(unique(as.integer(y)))
    if (length(yrs) == 2L && yrs[2] == yrs[1] + 1L && !open) {
      return(list(years = integer(0), fys = .cycle_label(yrs[1]), open = FALSE))
    }
    return(list(years = yrs, fys = character(0), open = open))
  }

  list(years = integer(0), fys = character(0), open = open)
}

# Format the way the curated dataset registry does: a comma list for a few
# values, "A to B" once a contiguous run gets long.
.format_period <- function(years, fys, open = FALSE) {
  if (length(fys)) {
    fys <- sort(unique(fys))
    if (open) return(paste(fys[1], "to current"))
    if (length(fys) <= 3L) return(paste(fys, collapse = ", "))
    return(paste(fys[1], "to", fys[length(fys)]))
  }
  if (!length(years)) return("")
  years <- sort(unique(years))
  if (open) return(paste(years[1], "to current"))
  if (length(years) <= 3L) return(paste(years, collapse = ", "))
  if (identical(years, seq(years[1], years[length(years)]))) {
    return(paste(years[1], "to", years[length(years)]))
  }
  paste(years, collapse = ", ")
}

.product_period <- local({
  parsed <- lapply(seq_len(nrow(plida_products)), function(i) {
    .parse_product_period(plida_products[["Product Name"]][i],
                          plida_products[["Product Description"]][i])
  })
  stats::setNames(
    vapply(parsed, function(r) .format_period(r$years, r$fys, r$open), character(1)),
    plida_products[["Product Name"]]
  )
})

# PLIDA occurrence spine.
plida <- data.frame(
  asset = "PLIDA",
  record_type = "variable",
  collection_type = ifelse(
    plida_variables$Dataset %in% .survey_datasets,
    "survey",
    "administrative"
  ),
  dataset = .text(plida_variables$Dataset),
  dataset_name = .dataset_column(plida_variables$Dataset, "dataset_name"),
  module = .text(plida_variables[["Module Name"]]),
  product = .text(plida_variables[["Product Name"]]),
  table = .text(plida_variables[["Table Name"]]),
  table_number = "",
  source_sheet = "",
  table_scope = "",
  source_item = "",
  variable = .text(plida_variables[["Variable Name"]]),
  variable_level = "",
  official_description = .text(plida_variables[["Variable Description"]]),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# The DIL contains a small number of blank descriptions. These labels use an
# official related field or an unambiguous classification title.
description_overrides <- c(
  AGE2122_END = "Age at the end of the 2021-22 financial year",
  AGE2122_START = "Age at the start of the 2021-22 financial year",
  AGE2223_END = "Age at the end of the 2022-23 financial year",
  AGE2223_START = "Age at the start of the 2022-23 financial year",
  AGE2324_END = "Age at the end of the 2023-24 financial year",
  AGE2324_START = "Age at the start of the 2023-24 financial year",
  LGA_CODE_2024 = "2024 Local Government Area code",
  SA1_CODE_2021 = "2021 Statistical Area Level 1 code",
  SA2_CODE_2021 = "2021 Statistical Area Level 2 code",
  SA3_CODE_2021 = "2021 Statistical Area Level 3 code",
  SA4_CODE_2021 = "2021 Statistical Area Level 4 code",
  STATE_CODE_2021 = "2021 state or territory code",
  NNELGBTXEXPTN_FBT_TTLRPRTBAMT = paste(
    "Total reportable fringe benefits amount from employers not exempt",
    "from FBT under section 57A of the FBTAA 1986"
  ),
  NNELGBTXEXMPTN_FBT_TTLRPRTBAMT = paste(
    "Total reportable fringe benefits amount from employers not exempt",
    "from FBT under section 57A of the FBTAA 1986"
  )
)

plida$variable_description <- plida$official_description
blank_description <- !nzchar(plida$variable_description)
override_hit <- match(plida$variable, names(description_overrides))
has_override <- blank_description & !is.na(override_hit)
plida$variable_description[has_override] <- unname(
  description_overrides[override_hit[has_override]]
)
description_by_variable <- tapply(
  plida$official_description,
  .pair_key(plida$dataset, plida$variable),
  .first
)
related_description <- unname(description_by_variable[
  .pair_key(plida$dataset, plida$variable)
])
has_related_description <- blank_description & !has_override &
  nzchar(.text(related_description))
plida$variable_description[has_related_description] <-
  .text(related_description[has_related_description])
plida$variable_description[
  blank_description & !has_override & !has_related_description
] <-
  "The PLIDA DIL does not supply a variable description."
plida$description_source <- ifelse(
  has_override,
  "Official classification title or related PLIDA DIL description",
  ifelse(
    has_related_description,
    "Related occurrence in the PLIDA DIL",
    ifelse(
      blank_description,
      "PLIDA DIL: description not supplied",
      "PLIDA DIL"
    )
  )
)
plida$description_source_url <- .plida_dil_url
description_curation_path <- file.path(
  .repo_root,
  "data-raw",
  "variable-info-description-curation.csv"
)
if (file.exists(description_curation_path)) {
  description_curation <- .read_csv(description_curation_path)
  required_curation_columns <- c(
    "dataset", "variable", "variable_description", "description_source",
    "description_source_url"
  )
  if (!all(required_curation_columns %in% names(description_curation))) {
    stop("Description curation file has an unexpected schema.", call. = FALSE)
  }
  curation_key <- .pair_key(
    description_curation$dataset,
    description_curation$variable
  )
  if (anyDuplicated(curation_key)) {
    stop("Description curation keys are not unique.", call. = FALSE)
  }
  curation_hit <- match(
    .pair_key(plida$dataset, plida$variable),
    curation_key
  )
  if (!all(curation_key %in% .pair_key(plida$dataset, plida$variable))) {
    stop("Description curation contains an unknown variable.", call. = FALSE)
  }
  curated <- !is.na(curation_hit)
  plida$variable_description[curated] <- .text(
    description_curation$variable_description[curation_hit[curated]]
  )
  plida$description_source[curated] <- .text(
    description_curation$description_source[curation_hit[curated]]
  )
  plida$description_source_url[curated] <- .text(
    description_curation$description_source_url[curation_hit[curated]]
  )
}
plida$variable_type <- ""
plida$variable_type_source <- "Not supplied by the PLIDA DIL"
# Per-product where the product metadata carries a period; the dataset period
# is the fallback for the products whose metadata records no year at all.
plida$reference_period <- unname(.product_period[plida$product])
.no_product_period <- is.na(plida$reference_period) |
  !nzchar(plida$reference_period)
plida$reference_period[.no_product_period] <-
  .dataset_column(plida$dataset[.no_product_period], "reference_period")

# A product named "...-2006-latest" claims an open end, but the curated dataset
# period often names the real one ("2006 to 2023"). Prefer the curated end so a
# variable does not claim more coverage than the dataset has.
local({
  ds_period <- .dataset_column(plida$dataset, "reference_period")
  ds_closed <- !grepl("current|latest", ds_period, ignore.case = TRUE)
  open_var <- grepl("to current$", plida$reference_period)
  fix <- open_var & ds_closed & !is.na(ds_period)
  if (any(fix)) {
    ds_end <- vapply(ds_period[fix], function(s) {
      y <- unlist(regmatches(s, gregexpr("[0-9]{4}(-[0-9]{4})?", s)))
      if (!length(y)) NA_character_ else y[length(y)]
    }, character(1))
    start <- sub(" to current$", "", plida$reference_period[fix])
    plida$reference_period[fix] <<- ifelse(
      is.na(ds_end), plida$reference_period[fix], paste(start, "to", ds_end)
    )
  }
})
plida$available_periods <- ""
plida$official_valid_response <- ""

# BLADE variable occurrences.
table_hit <- match(blade_variables[["Table.Number"]],
                   blade_tables[["Table.Number"]])
blade <- data.frame(
  asset = "BLADE",
  record_type = "variable",
  collection_type = ifelse(
    blade_variables[["Table.Number"]] %in% .blade_survey_tables,
    "survey",
    "administrative"
  ),
  dataset = "BLADE",
  dataset_name = "Business Longitudinal Analysis Data Environment",
  module = "",
  product = .text(blade_variables[["Product.Name"]]),
  table = .text(blade_variables[["Table.Name"]]),
  table_number = .text(blade_variables[["Table.Number"]]),
  source_sheet = .text(blade_variables$Sheet),
  table_scope = .text(blade_tables$Scope[table_hit]),
  source_item = .text(blade_variables$Item),
  variable = .text(blade_variables[["Variable.Name"]]),
  variable_level = .text(blade_variables[["Variable.Level"]]),
  official_description = .text(blade_variables$Item),
  variable_description = .text(blade_variables$Item),
  description_source = "BLADE DIL",
  description_source_url = .dataset_column("BLADE", "information_url"),
  variable_type = "",
  variable_type_source = "Not supplied by the BLADE DIL",
  reference_period = .text(blade_tables[["Reference.Period"]][table_hit]),
  available_periods = .text(blade_variables[["Available.Periods"]]),
  official_valid_response = .text(blade_variables[["Valid.Response"]]),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
blade$variable_description[!nzchar(blade$variable_description)] <-
  "The BLADE DIL does not supply a variable description."
blade$description_source[!nzchar(blade$official_description)] <-
  "BLADE DIL: description not supplied"

# BLADE linking-key occurrences use the same public variable register.
blade_key_rows <- data.frame(
  asset = "BLADE",
  record_type = "linking_key",
  collection_type = "administrative",
  dataset = "BLADE",
  dataset_name = "Business Longitudinal Analysis Data Environment",
  module = "",
  product = .text(blade_keys[["Product.Name"]]),
  table = .text(blade_keys[["Key.Name"]]),
  table_number = "",
  source_sheet = .text(blade_keys$Appendix),
  table_scope = "linking key",
  source_item = .text(blade_keys$Item),
  variable = .text(blade_keys[["Variable.Name"]]),
  variable_level = "linking key",
  official_description = .text(blade_keys$Item),
  variable_description = .text(blade_keys$Item),
  description_source = "BLADE DIL linking-key appendix",
  description_source_url = .dataset_column("BLADE", "information_url"),
  variable_type = "",
  variable_type_source = "Not supplied by the BLADE DIL",
  reference_period = "",
  available_periods = .text(blade_keys[["Available.Periods"]]),
  official_valid_response = .text(blade_keys[["Valid.Response"]]),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
blade_key_rows$variable_description[
  !nzchar(blade_key_rows$variable_description)
] <- "The BLADE DIL does not supply a variable description."

info <- rbind(plida, blade, blade_key_rows)
info[] <- lapply(info, .text)

counts <- .match_counts(
  info$dataset,
  info$variable,
  info$product,
  info$table
)
info$occurrence_count <- counts$occurrence_count
info$product_count <- counts$product_count
info$table_count <- counts$table_count

.value_kind <- function(valid_response) {
  response <- tolower(.text(valid_response))
  if (!nzchar(response)) return("not_specified")
  if (grepl(
    "date|yyyy|financial year|income year|calendar year|month|quarter|week|day",
    response,
    perl = TRUE
  )) return("date_or_period")
  if (grepl("percent|percentage|ratio|rate|^%$", response, perl = TRUE)) {
    return("rate_or_percentage")
  }
  if (grepl("amount|\\$|dollar", response, perl = TRUE)) return("amount")
  if (grepl("count|number of", response, perl = TRUE)) return("count")
  if (grepl(
    "numeric|number|integer|decimal|^ml$|hectare|\\(ha\\)|\\(t\\)|kwh|kl|litre|hours|minutes",
    response,
    perl = TRUE
  )) return("numeric")
  if (grepl("identifier|alphanumeric", response, perl = TRUE)) {
    return("identifier")
  }
  "code_or_category"
}

info$value_kind <- vapply(
  info$official_valid_response,
  .value_kind,
  character(1)
)
info$value_domain <- ifelse(
  info$value_kind == "identifier",
  "identifier",
  ifelse(
    info$value_kind == "date_or_period",
    "open date or period",
    ifelse(
      info$value_kind %in% c(
        "amount", "count", "numeric", "rate_or_percentage"
      ),
      "open numeric domain",
      ifelse(
        nzchar(info$official_valid_response),
        "source-defined response domain",
        "not specified"
      )
    )
  )
)
blade_typed <- info$asset == "BLADE" & info$value_kind != "not_specified"
info$variable_type[blade_typed] <- ifelse(
  info$value_kind[blade_typed] %in% c(
    "amount", "count", "numeric", "rate_or_percentage"
  ),
  "numeric",
  ifelse(
    info$value_kind[blade_typed] == "date_or_period",
    "date or period",
    ifelse(
      info$value_kind[blade_typed] == "identifier",
      "identifier",
      "categorical"
    )
  )
)
info$variable_type_source[blade_typed] <- "BLADE DIL valid response"
info$valid_values <- "[]"
info$metadata_source <- ifelse(
  info$asset == "PLIDA",
  "PLIDA Data Item List",
  "BLADE Data Item List"
)
info$metadata_vintage <- ifelse(
  info$asset == "PLIDA",
  "19 March 2026",
  "April 2026"
)
info$value_source <- .dataset_column(info$dataset, "information_source")
info$value_source_url <- .dataset_column(info$dataset, "information_url")

# BLADE publishes complete value lists for selected variables in domains.csv.
# Exclude local plausible domains because they are not source-defined values.
blade_domain_rows <- nzchar(.text(blade_domains[["Variable.Name"]])) &
  .text(blade_domains[["Source.Sheet"]]) !=
    "LOCAL_PLAUSIBLE_NOT_OFFICIAL"
blade_domain_split <- split(
  blade_domains$Value[blade_domain_rows],
  tolower(.text(blade_domains[["Variable.Name"]][blade_domain_rows]))
)
blade_info_rows <- info$asset == "BLADE" &
  info$collection_type == "administrative"
for (i in which(blade_info_rows)) {
  values <- blade_domain_split[[tolower(info$variable[[i]])]]
  if (length(values)) {
    info$valid_values[[i]] <- .json_array(values)
    info$value_source[[i]] <- "BLADE Data Item List"
    info$value_source_url[[i]] <- .dataset_column(
      "BLADE", "information_url"
    )
  }
}

# The BLADE appendices publish additional domains by appendix rather than by
# variable. Map only variables whose valid-response field identifies the exact
# appendix domain.
blade_appendix_rows <- !nzchar(.text(blade_domains[["Variable.Name"]])) &
  .text(blade_domains[["Source.Sheet"]]) !=
    "LOCAL_PLAUSIBLE_NOT_OFFICIAL"
blade_appendix_values <- split(
  blade_domains$Value[blade_appendix_rows],
  blade_domains$Domain[blade_appendix_rows]
)
# Appendix 9 includes explanatory footnotes in the value column. The finite
# domain contains only the published one- or two-character unit codes.
if ("trade_unit" %in% names(blade_appendix_values)) {
  blade_appendix_values[["trade_unit"]] <- blade_appendix_values[[
    "trade_unit"
  ]][grepl("^[A-Z]{1,2}$", blade_appendix_values[["trade_unit"]])]
}
blade_appendix_domain <- c(
  setNames(
    rep("iplord_technology", 24L),
    as.vector(outer(
      c("p", "p_chemistry", "p_instrmts", "p_elec_eng", "p_mech_eng",
        "p_other"),
      c("filed_modal_class", "granted_modal_class", "retired_modal_class",
        "le_modal_class"),
      paste,
      sep = "_"
    ))
  ),
  f_country_of_earliest_filing = "iso_country_alpha2",
  f_earliest_country_of_grant = "iso_country_alpha2",
  classifying_country_code = "iso_country_alpha2",
  linked_application_country = "iso_country_alpha2",
  country_code = "iso_country_alpha2",
  ip_right_sub_type = "ip_right_sub_type",
  status = "ip_status",
  event_category = "ip_event_category",
  event_type = "ip_event_type",
  link_type = "ip_link_type",
  mktselected = "max_market",
  country_of_final_dest_ex = "trade_country_code",
  country_of_origin_im = "trade_country_code",
  port_of_discharge_ex = "trade_foreign_port",
  port_of_loading_im = "trade_foreign_port",
  port_of_loading_ex = "trade_australian_port",
  port_of_discharge_im = "trade_australian_port",
  invoice_currency_ex = "trade_currency",
  invoice_currency_im = "trade_currency",
  unit_of_quantity_ex = "trade_unit",
  unit_of_quantity_im = "trade_unit"
)
for (variable in names(blade_appendix_domain)) {
  rows <- info$asset == "BLADE" &
    info$collection_type == "administrative" &
    tolower(info$variable) == tolower(variable) &
    grepl("Appendix|Appendices", info$official_valid_response,
          ignore.case = TRUE)
  values <- blade_appendix_values[[blade_appendix_domain[[variable]]]]
  if (!any(rows) || !length(values)) next
  info$valid_values[rows] <- .json_array(values)
  info$value_source[rows] <- "BLADE Data Item List appendix"
  info$value_source_url[rows] <- .dataset_column("BLADE", "information_url")
}

.blade_hyphen_values <- function(response) {
  response <- .text(response)
  has_invalid_suffix <- grepl(
    "[[:space:]]+everything else - invalid response or no response given$",
    response,
    ignore.case = TRUE,
    perl = TRUE
  )
  response <- sub(
    "[[:space:]]+everything else - invalid response or no response given$",
    "",
    response,
    ignore.case = TRUE,
    perl = TRUE
  )
  response <- sub("^Numeric[[:space:]]+", "", response, perl = TRUE)
  if (grepl(
    "(?:^|[[:space:]])[[:alnum:]]+-[[:space:]]|[[:space:]]Missing$",
    response,
    perl = TRUE
  )) return(character())

  marker <- "(?<!\\S)([0-9]+|[A-Z][A-Z0-9_]*|n|L) - "
  locations <- gregexpr(marker, response, perl = TRUE)[[1L]]
  if (locations[[1L]] != 1L ||
      (length(locations) < 2L && !has_invalid_suffix)) {
    return(character())
  }
  marker_lengths <- attr(locations, "match.length")
  codes <- regmatches(response, gregexpr(marker, response, perl = TRUE))[[1L]]
  codes <- trimws(sub(" - $", "", codes))
  labels <- vapply(seq_along(locations), function(index) {
    start <- locations[[index]] + marker_lengths[[index]]
    end <- if (index < length(locations)) {
      locations[[index + 1L]] - 1L
    } else {
      nchar(response)
    }
    trimws(substr(response, start, end))
  }, character(1))
  if (any(!nzchar(labels)) || any(grepl("^[0-9]+$", labels)) || any(grepl(
    "(?<!\\S)[[:alnum:]_.-]+[[:space:]]*=",
    labels,
    perl = TRUE
  ))) {
    return(character())
  }
  paste0(codes, ": ", labels)
}

.blade_response_values <- function(response) {
  response <- .text(response)

  # These complete binary lists use type labels or hyphen delimiters instead
  # of the usual code = label form.
  binary_responses <- list(
    "Numeric (0 = FALSE; 1 = TRUE)" = c("0: FALSE", "1: TRUE"),
    "Numeric (0 = No; 1 = Yes)" = c("0: No", "1: Yes"),
    "Numeric - Binary 0 - No 1 - Yes" = c("0: No", "1: Yes"),
    paste0(
      "Y - Yes N - No everything else - invalid response or no response ",
      "given"
    )
  )
  binary_responses[[4L]] <- c("Y: Yes", "N: No")
  names(binary_responses)[[4L]] <- paste(
    "Y - Yes N - No everything else - invalid response or no response",
    "given"
  )
  if (response %in% names(binary_responses)) {
    return(binary_responses[[response]])
  }
  if (identical(
    response,
    "n - does not exist 0 - cancelled 1 = active L - Long term non-remitter"
  )) {
    return(c(
      "n: does not exist", "0: cancelled", "1: active",
      "L: Long term non-remitter"
    ))
  }
  bit_indicator <- regmatches(
    response,
    regexec("^1 - (BIT (?:Company|Individual|Partnership|Trust) data exists)$",
            response, perl = TRUE)
  )[[1L]]
  if (length(bit_indicator) == 2L) {
    return(paste0("1: ", bit_indicator[[2L]]))
  }
  if (identical(
    response,
    paste(
      "1 digit numeric 1 = coded address passed acceptance criteria at",
      "the ARID level 2 = coded address passed acceptance criteria at the",
      "MB level 3 = coded address passed acceptance criteria at the SA2 level"
    )
  )) {
    return(c(
      "1: coded address passed acceptance criteria at the ARID level",
      "2: coded address passed acceptance criteria at the MB level",
      "3: coded address passed acceptance criteria at the SA2 level"
    ))
  }

  categorical_response <- sub(
    "^Categorical[[:space:]]+", "", response, perl = TRUE
  )
  categorical_marker <- "'([^']+)'[[:space:]]*=[[:space:]]*"
  categorical_locations <- gregexpr(
    categorical_marker, categorical_response, perl = TRUE
  )[[1L]]
  if (categorical_locations[[1L]] == 1L &&
      length(categorical_locations) >= 2L) {
    marker_lengths <- attr(categorical_locations, "match.length")
    markers <- regmatches(
      categorical_response,
      gregexpr(categorical_marker, categorical_response, perl = TRUE)
    )[[1L]]
    codes <- sub("^'([^']+)'.*$", "\\1", markers, perl = TRUE)
    labels <- vapply(seq_along(categorical_locations), function(index) {
      start <- categorical_locations[[index]] + marker_lengths[[index]]
      end <- if (index < length(categorical_locations)) {
        categorical_locations[[index + 1L]] - 1L
      } else {
        nchar(categorical_response)
      }
      trimws(substr(categorical_response, start, end))
    }, character(1))
    trailing_value <- regmatches(
      tail(labels, 1L),
      regexec("^(.*?)[[:space:]]+'([^']+)'$", tail(labels, 1L), perl = TRUE)
    )[[1L]]
    if (length(trailing_value) == 3L) {
      labels[[length(labels)]] <- trimws(trailing_value[[2L]])
      standalone <- trailing_value[[3L]]
    } else {
      standalone <- character()
    }
    if (all(nzchar(labels))) {
      return(c(paste0(codes, ": ", labels), standalone))
    }
  }

  bracket_response <- sub(
    "^Categorical[[:space:]]+", "", response, perl = TRUE
  )
  bracket_marker <- "\\[([^]]*)\\][[:space:]]*=[[:space:]]*"
  bracket_locations <- gregexpr(
    bracket_marker, bracket_response, perl = TRUE
  )[[1L]]
  if (bracket_locations[[1L]] == 1L && length(bracket_locations) >= 2L) {
    marker_lengths <- attr(bracket_locations, "match.length")
    markers <- regmatches(
      bracket_response,
      gregexpr(bracket_marker, bracket_response, perl = TRUE)
    )[[1L]]
    codes <- sub("^(\\[[^]]*\\]).*$", "\\1", markers, perl = TRUE)
    labels <- vapply(seq_along(bracket_locations), function(index) {
      start <- bracket_locations[[index]] + marker_lengths[[index]]
      end <- if (index < length(bracket_locations)) {
        bracket_locations[[index + 1L]] - 1L
      } else {
        nchar(bracket_response)
      }
      trimws(substr(bracket_response, start, end))
    }, character(1))
    if (all(nzchar(labels))) return(paste0(codes, ": ", labels))
  }

  parenthetical <- regmatches(
    response,
    regexec("^(?:Character|Numeric) \\(([^()]*)\\)$", response, perl = TRUE)
  )[[1L]]
  if (length(parenthetical) == 2L) {
    values <- trimws(strsplit(parenthetical[[2L]], ";", fixed = TRUE)[[1L]])
    if (length(values) >= 2L && all(grepl(
      "^[[:alnum:]_.-]+$", values, perl = TRUE
    ))) return(values)
  }

  quoted_list_pattern <- paste0(
    "^(?:Character )?(?:\"[^\"]+\"[[:space:]]*){2,}$"
  )
  if (grepl(quoted_list_pattern, response, perl = TRUE)) {
    values <- regmatches(response, gregexpr('"[^"]+"', response))[[1L]]
    return(substring(values, 2L, nchar(values) - 1L))
  }

  quoted_hyphen_response <- sub(
    "^Character[[:space:]]+", "", response, perl = TRUE
  )
  quoted_hyphen_marker <- '"([^"]*)" - '
  quoted_hyphen_locations <- gregexpr(
    quoted_hyphen_marker, quoted_hyphen_response, perl = TRUE
  )[[1L]]
  if (quoted_hyphen_locations[[1L]] == 1L &&
      length(quoted_hyphen_locations) >= 2L) {
    marker_lengths <- attr(quoted_hyphen_locations, "match.length")
    markers <- regmatches(
      quoted_hyphen_response,
      gregexpr(quoted_hyphen_marker, quoted_hyphen_response, perl = TRUE)
    )[[1L]]
    codes <- sub('^"([^"]*)".*$', "\\1", markers, perl = TRUE)
    codes[!nzchar(trimws(codes))] <- "Blank"
    labels <- vapply(seq_along(quoted_hyphen_locations), function(index) {
      start <- quoted_hyphen_locations[[index]] + marker_lengths[[index]]
      end <- if (index < length(quoted_hyphen_locations)) {
        quoted_hyphen_locations[[index + 1L]] - 1L
      } else {
        nchar(quoted_hyphen_response)
      }
      trimws(substr(quoted_hyphen_response, start, end))
    }, character(1))
    if (all(nzchar(labels))) return(paste0(codes, ": ", labels))
  }

  hyphen_values <- .blade_hyphen_values(response)
  if (length(hyphen_values)) return(hyphen_values)

  quoted_marker <- '"([^"]+)"\\s*=\\s*'
  quoted_locations <- gregexpr(quoted_marker, response, perl = TRUE)[[1L]]
  if (quoted_locations[[1L]] == 1L && length(quoted_locations) >= 2L) {
    marker_lengths <- attr(quoted_locations, "match.length")
    markers <- regmatches(
      response, gregexpr(quoted_marker, response, perl = TRUE)
    )[[1L]]
    codes <- sub('^"([^"]+)".*$', "\\1", markers, perl = TRUE)
    labels <- vapply(seq_along(quoted_locations), function(index) {
      start <- quoted_locations[[index]] + marker_lengths[[index]]
      end <- if (index < length(quoted_locations)) {
        quoted_locations[[index + 1L]] - 1L
      } else {
        nchar(response)
      }
      trimws(substr(response, start, end))
    }, character(1))
    if (all(nzchar(labels))) return(paste0(codes, ": ", labels))
  }

  typed_enumeration <- grepl(
    "^(?:Numeric|Character)[[:space:]]+", response, perl = TRUE
  )
  if (typed_enumeration) {
    response <- sub(
      "^(?:Numeric|Character)[[:space:]]+", "", response, perl = TRUE
    )
    if (grepl(
      "varies or unknown quantity|no limit",
      response,
      ignore.case = TRUE,
      perl = TRUE
    )) return(character())
  }

  if (!nzchar(response) || grepl(
    paste0(
      "numeric|number|amount|\\$|identifier|alphanumeric|date|yyyy|",
      "percent|percentage|ratio|rate"
    ),
    if (typed_enumeration) "" else response,
    ignore.case = TRUE,
    perl = TRUE
  )) return(character())

  pattern <- paste0(
    "(?<![[:alnum:]_.-])([[:alnum:]_.-]+)\\s*=\\s*([^=]+?)",
    "(?=(?:\\s+[[:alnum:]_.-]+\\s*=)|$)"
  )
  match_locations <- gregexpr(pattern, response, perl = TRUE)[[1L]]
  matches <- regmatches(response, gregexpr(pattern, response, perl = TRUE))[[1L]]
  if (match_locations[[1L]] != 1L || length(matches) < 2L) {
    return(character())
  }

  values <- vapply(matches, function(value) {
    parts <- regmatches(value, regexec(pattern, value, perl = TRUE))[[1L]]
    paste0(parts[[2L]], ": ", trimws(parts[[3L]]))
  }, character(1))

  # Reject strings in which an alternative code was absorbed into a label,
  # for example "'0' or Blank = Missing". The official response remains in
  # official_valid_response when it cannot be normalised without loss.
  labels <- sub("^[^:]+: ", "", values)
  if (any(grepl(
    "(?:^|[[:space:]])['\"]?[[:alnum:]._-]+['\"]?[[:space:]]+or$",
    labels,
    ignore.case = TRUE,
    perl = TRUE
  ))) return(character())

  values
}

# Normalise only complete code-label enumerations in the official BLADE
# response field. Open numeric, date, identifier and free-text responses stay
# in official_valid_response.
blade_response_rows <- which(
  info$asset == "BLADE" &
    info$collection_type == "administrative" &
    info$valid_values == "[]" &
    nzchar(info$official_valid_response)
)
for (i in blade_response_rows) {
  values <- .blade_response_values(info$official_valid_response[[i]])
  if (!length(values)) next
  info$valid_values[[i]] <- .json_array(values)
  info$value_source[[i]] <- "BLADE Data Item List valid response"
  info$value_source_url[[i]] <- .dataset_column("BLADE", "information_url")
}

# Census public codeframes are year-specific. Use the occurrence year when the
# product or table identifies it; otherwise include the documented values from
# all available Census years.
census_values_path <- file.path(
  .codeframe_dir,
  "census-codeframe-values.csv"
)
if (file.exists(census_values_path)) {
  census_values <- .read_csv(census_values_path)
  census_values$entry <- ifelse(
    nzchar(.text(census_values$label)),
    paste0(.text(census_values$code), ": ", .text(census_values$label)),
    .text(census_values$code)
  )
  census_by_variable <- split(
    census_values$entry,
    toupper(.text(census_values$variable))
  )
  census_url_by_variable <- tapply(
    census_values$source_url,
    toupper(.text(census_values$variable)),
    .collapse
  )
  census_by_year <- split(
    census_values$entry,
    paste(census_values$year, toupper(.text(census_values$variable)),
          sep = "\r")
  )
  census_url_by_year <- tapply(
    census_values$source_url,
    paste(census_values$year, toupper(.text(census_values$variable)),
          sep = "\r"),
    .collapse
  )
  for (i in which(info$dataset == "CENSUS")) {
    context <- paste(info$product[[i]], info$table[[i]])
    year_hit <- regmatches(
      context,
      gregexpr("20(?:11|16|21)", context, perl = TRUE)
    )[[1L]]
    source_key <- if (length(year_hit)) {
      paste(tail(year_hit, 1L), toupper(info$variable[[i]]), sep = "\r")
    } else {
      toupper(info$variable[[i]])
    }
    values <- if (length(year_hit)) {
      census_by_year[[source_key]]
    } else {
      census_by_variable[[source_key]]
    }
    source_url <- if (length(year_hit)) {
      unname(census_url_by_year[source_key])
    } else {
      unname(census_url_by_variable[source_key])
    }
    if (length(values)) {
      info$valid_values[[i]] <- .json_array(values)
      info$value_source[[i]] <- "ABS Census data item list"
      if (length(source_url) && nzchar(.text(source_url))) {
        info$value_source_url[[i]] <- .text(source_url)
      }
    }
  }
}

# ACLD workbook frame identifiers map source-defined codes to each occurrence.
acld_mapping_path <- file.path(
  .codeframe_dir,
  "acld-variable-codeframes.csv"
)
acld_values_path <- file.path(
  .codeframe_dir,
  "acld-codeframe-values.csv"
)
if (file.exists(acld_mapping_path) && file.exists(acld_values_path)) {
  acld_mapping <- .read_csv(acld_mapping_path)
  acld_values <- .read_csv(acld_values_path)
  acld_values$entry <- ifelse(
    nzchar(.text(acld_values$label)),
    paste0(.text(acld_values$code), ": ", .text(acld_values$label)),
    .text(acld_values$code)
  )
  acld_value_split <- split(acld_values$entry, acld_values$frame_id)
  mapping_key <- paste(
    .text(acld_mapping$product),
    toupper(.text(acld_mapping$variable)),
    sep = "\r"
  )
  for (i in which(info$dataset == "ACLD")) {
    hit <- match(
      paste(info$product[[i]], toupper(info$variable[[i]]), sep = "\r"),
      mapping_key
    )
    if (!is.na(hit) && isTRUE(acld_mapping$supported[[hit]])) {
      values <- acld_value_split[[acld_mapping$frame_id[[hit]]]]
      if (length(values)) {
        info$valid_values[[i]] <- .json_array(values)
        info$value_source[[i]] <- "ABS ACLD detailed microdata workbook"
        source_url <- .text(acld_mapping$source_url[[hit]])
        if (nzchar(source_url)) info$value_source_url[[i]] <- source_url
      }
    }
  }
}

# Public tax codeframes with a direct PLIDA mapping.
lodgement_values <- c(
  "AGNT_ELS", "AGNT_PPR", "ETAX", "SP_MYTAX", "SP_PPR", "TELE",
  "TXPK_EXP"
)
info$valid_values[
  info$dataset == "PIT_ITR" & info$variable == "LODGMENT_SOURCE"
] <- .json_array(lodgement_values)

health_fund_path <- file.path(.codeframe_dir, "ato-health-insurer-ids.tsv")
if (file.exists(health_fund_path)) {
  health_funds <- utils::read.delim(
    health_fund_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = "",
    fileEncoding = "UTF-8"
  )
  health_fund_values <- paste0(
    .text(health_funds$code),
    ": ",
    .text(health_funds$insurer)
  )
  health_rows <- info$dataset == "PIT_ITR" &
    grepl("^HLTH_FND_CD(?:_[1-4])?$", info$variable, perl = TRUE)
  info$valid_values[health_rows] <- .json_array(health_fund_values)
}

asset_opt_out_rows <- info$dataset == "PIT_ITR" & info$variable %in% c(
  "BBI_ELGBL_AST_BBI_CD", "TFE_ELGBL_AST_OPT_OUT_CD"
)
info$valid_values[asset_opt_out_rows] <- .json_array(c(
  "5: some eligible assets",
  "10: all eligible assets",
  "15: no opt-out"
))
business_segment_rows <- info$dataset == "PIT_ITR" &
  info$variable == "BUSINESS_MARKET_SEGMENT"
info$valid_values[business_segment_rows] <- .json_array(c(
  "GOV: government",
  "LGE: large group or enterprise",
  "SME: small or medium enterprise",
  "INB: individual not in business",
  "MIC: micro business",
  "NFP: not-for-profit"
))

.aedc_ledger_values <- function(variable, text) {
  variable <- toupper(.text(variable))
  text <- tolower(.text(text))

  if (variable %in% c("A1AZ", "A1BZ", "A1CZ", "A1DZ")) {
    return(as.character(0:9))
  }
  if (variable == "AGECUT") return(as.character(0:2))
  if (variable == "AGEGROUP") return(c("<5", "5", "6", ">6"))
  if (variable == "AGEGROUP3TO7") return(as.character(3:7))
  if (variable == "SCHOOLTYPE") return(c("C", "G", "I"))
  if (variable == "REMOTENESS") {
    return(c(
      "Major Cities of Australia", "Inner Regional Australia",
      "Outer Regional Australia", "Remote Australia",
      "Very Remote Australia"
    ))
  }
  if (variable == "REMOTENESSCODE") return(as.character(1:5))
  if (variable == "CYCLE") {
    return(paste0(1:6, ": ", c(2009, 2012, 2015, 2018, 2021, 2024)))
  }
  if (variable == "MOC") return(as.character(4:9))
  if (variable %in% c("IREGPUBLIC", "LGAPUBLIC")) {
    return(as.character(c(1, 5, 9, 21, 22, 23)))
  }
  if (variable == "VALIDDOMAINS") return(as.character(0:5))
  if (grepl(
    "^(?:COMGEN|EMOT|LANGCOG|PHYS|SOC)VALID$",
    variable,
    perl = TRUE
  )) return(c("0", "1"))
  if (grepl("^D[1-9]$", variable, perl = TRUE)) {
    return(c("0", "1", "2", "3", "88"))
  }
  if (variable %in% c("D10", "D10A", "D10B", "D10C", "D11")) {
    return(c("0", "1", "88"))
  }
  if (grepl("^D10(?:[A-Z]+Y|FY[A-Z])$", variable, perl = TRUE)) {
    return(c("0", "1"))
  }
  if (variable %in% c(
    "DEVDIFF", "DV1", "DV2", "SEIFAEXCLUDED", "SPECIALNEEDS"
  ) || grepl("^(?:ONTRACK[0-5]|OT[1-5])$", variable, perl = TRUE)) {
    return(c("0", "1"))
  }
  if (variable == "E2Y") {
    return(c("1: Yes", "2: No", "88: Don't know"))
  }
  if (variable %in% c("DAYCARE", "DAYCARENO", "PSDC")) {
    return(c("0", "1", "88"))
  }
  if (variable == "PRESCHOOL") return(c("0", "1"))

  if (grepl("vintage-specific", text, fixed = TRUE)) return(character())
  if (grepl("1 to 3 and 88", text, fixed = TRUE)) {
    return(c(as.character(1:3), "88"))
  }
  if (grepl("1, 2 and 88", text, fixed = TRUE)) {
    return(c("1", "2", "88"))
  }
  if (grepl("1 to 4 and 88", text, fixed = TRUE)) {
    return(c(as.character(1:4), "88"))
  }
  if (grepl("0 to 15", text, fixed = TRUE)) return(as.character(0:15))
  if (grepl("deciles 1 to 10", text, fixed = TRUE)) {
    return(as.character(1:10))
  }
  if (grepl("(?:codes|categories|bands|quintiles) 1 to 5", text,
            perl = TRUE)) return(as.character(1:5))
  if (grepl("(?:codes|categories|bands|role codes) 1 to 4", text,
            perl = TRUE)) return(as.character(1:4))
  if (grepl("(?:codes|values) 0 to 4", text, perl = TRUE)) {
    return(as.character(0:4))
  }
  if (grepl("(?:codes|values) 0 to 2", text, perl = TRUE)) {
    return(as.character(0:2))
  }
  if (grepl("(?:codes|values) 1 to 3", text, perl = TRUE)) {
    return(as.character(1:3))
  }
  if (grepl("0 unselected, 1 no and 2 yes", text, fixed = TRUE)) {
    return(c("0: Unselected", "1: No", "2: Yes"))
  }
  if (grepl("0, 1 and 88|0, 1 or 88|0, 1, 88|0, 1 or 88",
            text, perl = TRUE)) return(c("0", "1", "88"))
  if (grepl(
    "0/1|0 no or 1 yes|0 no and 1 yes|0 or 1|0 zero-days and 1 one-or-more-days|binary",
    text,
    perl = TRUE
  )) return(c("0", "1"))
  if (grepl("publishable code 1", text, fixed = TRUE)) return("1")
  character()
}

aedc_ledger_path <- file.path(
  .docs_dir,
  "admin-value-decision-ledger-aedc.csv"
)
if (file.exists(aedc_ledger_path)) {
  aedc_ledger <- .read_csv(aedc_ledger_path)
  for (j in seq_len(nrow(aedc_ledger))) {
    values <- .aedc_ledger_values(
      aedc_ledger$variable[[j]],
      aedc_ledger$implemented_domain_or_derivation[[j]]
    )
    if (!aedc_ledger$status[[j]] %in% c("populated", "structural") ||
        !length(values)) next
    rows <- info$dataset == "AEDC" &
      toupper(info$variable) == toupper(aedc_ledger$variable[[j]])
    info$valid_values[rows] <- .json_array(values)
    info$value_source[rows] <- "AEDC 2025 Data Dictionary"
    source_url <- .urls(aedc_ledger$evidence_source[[j]])
    if (!nzchar(source_url)) {
      source_url <- .dataset_column("AEDC", "information_url")
    }
    info$value_source_url[rows] <- source_url
  }
}

# AEDC uses a different Not applicable code in the 2009-2018 and 2021-2024
# collections. Keep each occurrence's value list specific to its collection.
for (i in which(
  info$dataset == "AEDC" & toupper(info$variable) %in%
    c("A4A", "B1", "B1A", "B1B", "B1C", "B1D")
)) {
  year <- .context_year(info$product[[i]], info$table[[i]])
  not_applicable <- if (!is.na(year) && year <= 2018L) "999" else "99"
  values <- if (toupper(info$variable[[i]]) == "A4A") {
    c("0", "1", "88", not_applicable)
  } else {
    c("1", "2", "3", "88", not_applicable)
  }
  info$valid_values[[i]] <- .json_array(values)
  info$value_source[[i]] <- "AEDC data dictionary"
  info$value_source_url[[i]] <- paste(
    "https://www.aedc.gov.au/resources/detail/aedc-data-dictionary-2019",
    paste0(
      "https://www.aedc.gov.au/resources/detail/",
      "aedc-2025-data-dictionary"
    ),
    sep = " | "
  )
}

# The DEX Protocols publish these finite labels and codes.
dex_protocol_url <- paste0(
  "https://dex.dss.gov.au/sites/default/files/documents/2026-02/",
  "3016-dex-protocols-update-march.pdf"
)
dex_values <- list(
  ACCOMMODATIONTYPECODE = c(
    "Boarding house", "Crisis, emergency or transition",
    "Independent living unit", "Indigenous community/settlement",
    "Institutional setting (i.e. residential aged care, hospital)",
    "Private residence—client or family owned/purchasing",
    "Private residence—private rental", "Private residence—public rental",
    "Public shelter", "Supported accommodation", "Other", "Not stated"
  ),
  HOUSEHOLDCOMPOSITIONCODE = c(
    "Single (person living alone)", "Sole parent with dependant(s)",
    "Couple", "Couple with dependant(s)", "Group (related adults)",
    "Group (unrelated adults)", "Homeless/No household",
    "Not stated or inadequately described"
  ),
  EDUCATIONLEVELCODE = c(
    "Pre-primary education", "Primary education", "Secondary education",
    "Certificate level", "Advanced diploma and diploma level",
    "Bachelor’s degree level",
    "Graduate diploma and graduate certificate level",
    "Postgraduate degree level", "Other education"
  ),
  EMPLOYMENTSTATUSCODE = c(
    "Paid work full-time", "Paid work part-time",
    "Unpaid work (includes volunteering)",
    "Not working and not looking for work",
    "Unemployed (not working but looking for work)",
    "Studying full-time", "Studying part-time", "Caring", "Parenting"
  ),
  INCOMESOURCECODE = c(
    "Nil income", "Employee salary/wages",
    "Other income including superannuation and investments",
    "Self-employed (unincorporated business income)",
    "Government payments/pensions/allowances",
    "Not stated/Inadequately described"
  ),
  INCOMEFREQUENCYCODE = c("Weekly", "Fortnightly", "Monthly", "Annually"),
  HOMELESSCODE = c("No", "At Risk", "Yes"),
  DISABILITYCODE = c(
    "Intellectual/learning", "Psychiatric", "Sensory/speech",
    "Physical/diverse", "None (no disability)",
    "Not stated/inadequately described"
  ),
  DVACARDSTATUSCODE = c(
    "DVA Gold Card", "DVA White Card", "DVA Orange Card or other",
    "No DVA entitlement"
  ),
  EXITREASONCODE = c(
    "Client no longer requires assistance",
    "Service unable to provide assistance",
    "Client now requires higher level of care",
    "Client has moved out of area", "Client terminated the service",
    "Client died", "Client no longer eligible", "Client needs have been met",
    "None of the above"
  ),
  SERVICESETTINGCODE = c(
    "Organisation outlet/office", "Clients’ residence", "Community venue",
    "Partner organisation", "Telephone", "Video", "Online service",
    "Healthcare facility", "Education facility", "Justice facility"
  ),
  ATTENDANCEPROFILECODE = c(
    "Family", "Community event", "Peer support group", "Couple",
    "Cohabitants"
  ),
  PARTICIPATIONTYPE = c("Client", "Support person", "Unidentified client"),
  REFERRALFROMSOURCE = c(
    "Health agency", "Community services agency", "Educational agency",
    "Internal", "Legal agency", "Employment/job placement agency",
    "Lender/financial agency", "Accounting agency",
    "Centrelink/Department of Human Services (DHS)", "Other Agency",
    "Self", "Family", "Friends", "General Medical Practitioner",
    "My Aged Care Gateway", "NDIS referral", "Linkages Package",
    "Continuity of Support (CoS) Programme",
    "Humanitarian Settlement Program", "LAC Referral", "Other party",
    "Not stated/inadequately described"
  ),
  EXTERNREFERDETINCODE = c(
    "Health professional", "Financial institution", "Legal aid/solicitor",
    "Accountant/financial advisor", "Real estate agent", "Agronomist",
    "Succession planner", "Social support group", "Training organisation",
    "Other government agency", "Asset agent", "Other"
  ),
  MONEYWORKSHOPCODE = c(
    "Workshop 1 - Making Money Last Until Payday",
    "Workshop 2 - Planning For the Future",
    "Workshop 3 - How Can Banks Help",
    "Workshop 4 - Internet and Phone Banking",
    "Workshop 5 - Credit Can Be a Hazard",
    "Workshop 6 - Money Loans Sharks and Traps",
    "Workshop 7 - A Roof Overhead - Home Ownership",
    "Workshop 8 - A Roof Overhead Tenancy",
    "Workshop 9 - Managing Paperwork", "Other workshop"
  ),
  TOPICCODE = c(
    "Abuse/Neglect/Violence", "Access to non-NDIS service",
    "Child Protection", "Community Inclusion—Social/Family",
    "Disability services complaints", "Discrimination/rights",
    "Education", "Employment", "Equipment/aids", "Finances",
    "Government payments", "Health/Mental Health",
    "Housing/Homelessness", "Legal/Access to Justice",
    "NDIS—Internal Review", "NDIS—Access/Planning",
    "NDIS—Support implementing plan/Accessing services", "Other",
    "Physical access", "Transport", "Vulnerable/isolated"
  ),
  NDISELIGIBILITYCODE = c(
    "NDIS in-progress access request", "NDIS eligible", "NDIS ineligible"
  ),
  ASSESSEDBY = c(
    "SCORE directly – client", "SCORE directly – practitioner",
    "SCORE directly – joint", "SCORE directly – support person",
    "Validated outcomes tool – client",
    "Validated outcomes tool – practitioner",
    "Validated outcomes tool – joint",
    "Validated outcomes tool – support person"
  ),
  ASSESSMENTTYPE = c("Pre", "Post"),
  ASSESSMENTTYPECODE = c("PRE", "POST"),
  OUTCOMEDOMAINSCORE = as.character(1:5)
)
for (variable in names(dex_values)) {
  rows <- info$dataset == "DEX" & toupper(info$variable) == variable
  if (!any(rows)) next
  info$valid_values[rows] <- .json_array(dex_values[[variable]])
  info$value_source[rows] <- "DSS Data Exchange Protocols"
  info$value_source_url[rows] <- dex_protocol_url
}

dex_client_domains <- c(
  "Physical health", "Mental health, wellbeing and self-care",
  "Personal and family safety", "Age-appropriate development",
  "Community participation and networks", "Family functioning",
  "Financial resilience", "Employment", "Education and skills training",
  "Material wellbeing and basic necessities", "Housing",
  "Changed knowledge and access to information", "Changed skills",
  "Changed behaviours", "Empowerment, choice and control to make own decisions",
  "Engagement with relevant support services",
  "Changed impact of immediate crisis",
  "I am satisfied with the services I have received",
  "The service listened to me and understood my issues",
  "I am better able to deal with issues that I sought help with"
)
dex_community_domains <- c(
  "Community infrastructure and networks",
  "Organisational knowledge, skills and practices",
  "Group/community knowledge, skills, attitudes and behaviours",
  "Social cohesion"
)
for (table_name in c(
  "special_client_assessment", "special_community_assessment"
)) {
  rows <- info$dataset == "DEX" & info$table == table_name
  community <- table_name == "special_community_assessment"
  domain_rows <- rows & toupper(info$variable) == "OUTCOMEDOMAIN"
  type_rows <- rows & toupper(info$variable) == "OUTCOMETYPE"
  info$valid_values[domain_rows] <- .json_array(if (community) {
    dex_community_domains
  } else {
    dex_client_domains
  })
  info$valid_values[type_rows] <- .json_array(if (community) {
    "Community"
  } else {
    c("Circumstances", "Goals", "Satisfaction")
  })
  outcome_rows <- domain_rows | type_rows
  info$value_source[outcome_rows] <- "DSS Data Exchange Protocols"
  info$value_source_url[outcome_rows] <- dex_protocol_url
}

.read_codeframe_tsv <- function(filename) {
  path <- file.path(.codeframe_dir, filename)
  if (!file.exists(path)) return(NULL)
  utils::read.delim(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE,
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
}

.codeframe_display <- function(codeframe, labels_only = FALSE) {
  if (is.null(codeframe) || !nrow(codeframe)) return(character())
  label_column <- if ("name" %in% names(codeframe)) "name" else "label"
  labels <- .text(codeframe[[label_column]])
  if (labels_only) return(unique(labels[nzchar(labels)]))
  codes <- .text(codeframe$code)

  # Deduplicate on the CODE, not on the rendered "code: label" string.
  #
  # A codeframe spanning several vintages carries one row per code per year,
  # and the name drifts between them: LGA 11500 appears as "Campbelltown (C)",
  # "Campbelltown (C) (NSW)" and "Campbelltown (NSW)". Those render as three
  # different strings, so a `unique()` over the rendered value keeps all three
  # — and since generation strips the label and samples uniformly, that area
  # came up three times as often as a single-vintage one.
  #
  # The last row wins because the file is ordered oldest vintage first, so the
  # surviving label is the most current name for the code.
  keep <- !duplicated(codes, fromLast = TRUE)
  codes <- codes[keep]
  labels <- labels[keep]
  ifelse(nzchar(labels), paste0(codes, ": ", labels), codes)
}

.apply_source_values <- function(rows, values, source_label, source_url) {
  if (!any(rows) || !length(values)) return(invisible(NULL))
  info$valid_values[rows] <<- .json_array(values)
  info$value_source[rows] <<- source_label
  info$value_source_url[rows] <<- source_url
  invisible(NULL)
}

lga_codeframe <- .read_codeframe_tsv("lga.tsv")
ireg_codeframe <- .read_codeframe_tsv("ireg.tsv")
phn_codeframe <- .read_codeframe_tsv("phn.tsv")
sa2_codeframe <- .read_codeframe_tsv("sa2_2021.tsv")
language_codeframe <- .read_codeframe_tsv("ascl_language.tsv")
country_codeframe <- .read_codeframe_tsv("sacc_country.tsv")
state_codeframe <- .read_codeframe_tsv("state.tsv")

# AEDC geography variables use the codeframe that applies to each collection
# year. Code variables use code-and-label display entries; name variables use
# the published labels.
aedc_geography <- c(
  "IREGCODE", "IREGNAME", "LCLGACODE", "LCLGANAME", "LGACODE",
  "LGANAME", "PHNCODE", "PHNNAME"
)
for (i in which(
  info$dataset == "AEDC" & toupper(info$variable) %in% aedc_geography
)) {
  variable <- toupper(info$variable[[i]])
  if (startsWith(variable, "PHN")) {
    codeframe <- phn_codeframe
    source_label <- "AIHW Primary Health Network codeframe"
  } else {
    codeframe <- if (startsWith(variable, "IREG")) {
      ireg_codeframe
    } else {
      lga_codeframe
    }
    year <- .context_year(info$product[[i]], info$table[[i]])
    if (!is.na(year) && !is.null(codeframe) && "year" %in% names(codeframe)) {
      reference_year <- .nearest_codeframe_year(year, codeframe$year)
      codeframe <- codeframe[
        as.integer(codeframe$year) == reference_year, , drop = FALSE
      ]
    }
    source_label <- "ABS Australian Statistical Geography Standard codeframe"
  }
  if (is.null(codeframe) || !nrow(codeframe)) next
  .apply_source_values(
    seq_len(nrow(info)) == i,
    .codeframe_display(codeframe, grepl("NAME$", variable)),
    source_label,
    .collapse(codeframe$source_url)
  )
}

# DEX, DOMINO and RPS use public classification files bundled with the
# package. Values remain source definitions, not generated frequencies.
for (specification in list(
  list("DEX", "MAINLANGUAGECODE", language_codeframe,
       "ABS Australian Standard Classification of Languages",
       paste0(
         "https://www.abs.gov.au/statistics/classifications/",
         "australian-standard-classification-languages-ascl/2016"
       )),
  list("DEX", "BIRTHCOUNTRYCODE", country_codeframe,
       "ABS Standard Australian Classification of Countries",
       paste0(
         "https://www.abs.gov.au/statistics/classifications/",
         "standard-australian-classification-countries-sacc/",
         "latest-release"
       )),
  list("DOMINO", "LANG_CODE", language_codeframe,
       "ABS Australian Standard Classification of Languages",
       paste0(
         "https://www.abs.gov.au/statistics/classifications/",
         "australian-standard-classification-languages-ascl/2016"
       )),
  list("DOMINO", "CNTRY_CDE", country_codeframe,
       "ABS Standard Australian Classification of Countries",
       paste0(
         "https://www.abs.gov.au/statistics/classifications/",
         "standard-australian-classification-countries-sacc/",
         "latest-release"
       ))
)) {
  codeframe <- specification[[3L]]
  if (is.null(codeframe) || !nrow(codeframe)) next
  rows <- info$dataset == specification[[1L]] &
    toupper(info$variable) == specification[[2L]]
  .apply_source_values(
    rows,
    .codeframe_display(codeframe),
    specification[[4L]],
    specification[[5L]]
  )
}

if (!is.null(state_codeframe) && nrow(state_codeframe)) {
  dex_state_variables <- c(
    "CLIENTSTATE", "OUTLETSTATE", "STATE_ASGS_2021",
    "STE2016BOUNDARYCODE", "STE2021BOUNDARYCODE"
  )
  .apply_source_values(
    info$dataset == "DEX" & toupper(info$variable) %in% dex_state_variables,
    .codeframe_display(state_codeframe),
    "ABS Australian Statistical Geography Standard state codeframe",
    paste0(
      "https://www.abs.gov.au/statistics/standards/",
      "australian-statistical-geography-standard-asgs-edition-3/",
      "jul2021-jun2026/main-structure-and-greater-capital-city-",
      "statistical-areas"
    )
  )
}

if (!is.null(sa2_codeframe) && nrow(sa2_codeframe)) {
  .apply_source_values(
    info$dataset == "DEX" & toupper(info$variable) %in%
      c("SA2_ASGS_2021", "SA22021BOUNDARYCODE"),
    .codeframe_display(sa2_codeframe),
    "ABS ASGS 2021 Statistical Area Level 2 codeframe",
    .collapse(sa2_codeframe$source_url)
  )
}

if (!is.null(lga_codeframe) && nrow(lga_codeframe)) {
  for (year in c(2016L, 2021L, 2023L, 2024L)) {
    codeframe <- lga_codeframe[as.integer(lga_codeframe$year) == year, ]
    variables <- switch(
      as.character(year),
      `2016` = "LGA2016BOUNDARYCODE",
      `2021` = "LGA2021BOUNDARYCODE",
      `2023` = "LGA_CODE_2023",
      `2024` = "LGA_CODE_2024"
    )
    datasets <- if (year <= 2021L) "DEX" else "RPS"
    .apply_source_values(
      info$dataset == datasets & toupper(info$variable) == variables,
      .codeframe_display(codeframe),
      paste("ABS Local Government Area", year, "codeframe"),
      .collapse(codeframe$source_url)
    )
  }
  .apply_source_values(
    info$dataset == "DOMINO" & toupper(info$variable) == "LGA",
    .codeframe_display(lga_codeframe),
    "ABS Local Government Area codeframes",
    .collapse(lga_codeframe$source_url)
  )
}

has_values <- info$valid_values != "[]"
info$value_kind[has_values] <- "code_or_category"
info$value_domain[has_values] <- "finite source-defined values"
info$variable_type[has_values] <- "categorical"
info$variable_type_source[has_values] <- info$value_source[has_values]
info$value_definition <- ifelse(
  has_values,
  "The valid_values field contains the source-defined values.",
  ifelse(
    nzchar(info$official_valid_response),
    info$official_valid_response,
    ifelse(
      info$value_domain == "identifier",
      "The variable is an identifier; no finite value list applies.",
      ifelse(
        info$value_domain == "open date or period",
        "The variable has an open date or period domain.",
        ifelse(
          info$value_domain == "open numeric domain",
          "The variable has an open numeric domain.",
          "The source metadata does not publish a finite value list."
        )
      )
    )
  )
)

info$value_support_status <- ifelse(
  info$collection_type == "survey",
  "not_applicable",
  "sourced"
)
info$limitation <- ifelse(
  info$collection_type == "survey",
  paste(
    "Value support is not applicable because survey variable values are",
    "outside the registry scope."
  ),
  ifelse(
    has_values | nzchar(info$official_valid_response),
    "The source defines the value domain. Frequency information is not included.",
    ifelse(
      info$value_domain %in% c(
        "identifier", "open date or period", "open numeric domain"
      ),
      "No finite code list applies to this variable.",
      "The source metadata does not publish a finite value list."
    )
  )
)

blade_unrepresented_appendix <- info$asset == "BLADE" &
  info$collection_type == "administrative" &
  info$valid_values == "[]" &
  grepl("Appendix|Appendices", info$official_valid_response,
        ignore.case = TRUE)
info$value_support_status[blade_unrepresented_appendix] <- "unsupported"
info$limitation[blade_unrepresented_appendix] <- paste(
  "The BLADE DIL references an external appendix code list that is not",
  "represented in valid_values."
)

.evidence_label <- function(text, dataset, url = "") {
  text <- tolower(paste(.text(text), collapse = " "))
  url <- tolower(paste(.text(url), collapse = " "))
  if (grepl("ncver.edu.au|training.gov.au", url)) {
    return("NCVER or National Training Register documentation")
  }
  if (grepl("dewr.gov.au", url)) {
    return("Department of Employment and Workplace Relations documentation")
  }
  if (grepl("ato.gov.au|alife-research.app", url)) {
    return("Australian Taxation Office guidance")
  }
  if (grepl("ndis.gov.au", url)) return("NDIA dataset documentation")
  if (grepl("dex.dss.gov.au|dss.gov.au", url)) {
    return("DSS dataset documentation")
  }
  if (grepl("aedc.gov.au|education.gov.au", url)) {
    return("AEDC data dictionary or program documentation")
  }
  if (grepl("abs.gov.au|geo.abs.gov.au", url)) {
    return("Australian Bureau of Statistics documentation")
  }
  if (grepl("alife", text)) return("ATO ALife variable manual")
  if (grepl("privatehealth", text)) {
    return("Australian Government private health insurer register")
  }
  if (grepl("avetmiss|ncver", text)) {
    return("NCVER AVETMISS data element definitions")
  }
  if (grepl("acld", text)) return("ABS ACLD detailed microdata workbook")
  if (grepl("aedc", text)) return("AEDC data dictionary")
  if (grepl("census|asgs|seifa", text)) {
    return("ABS Census Dictionary or ASGS classification")
  }
  if (grepl("blade", text)) return("BLADE Data Item List")
  if (grepl("ato|tax", text)) return("Australian Taxation Office guidance")
  if (grepl("ndis|ndia", text)) return("NDIA dataset documentation")
  if (grepl("dss|domino|data exchange|dex", text)) {
    return("DSS dataset documentation")
  }
  if (grepl("air|immunisation", text)) {
    return("Australian Immunisation Register documentation")
  }
  .dataset_column(dataset, "information_source")
}

.exception_limitation <- function(determination) {
  determination <- tolower(.text(determination))
  if (grepl("structurally_inapplicable|structural_source_blank",
            determination)) {
    return("The variable is structurally blank when it does not apply.")
  }
  if (grepl("proprietary.score", determination)) {
    return(paste(
      "The public documentation does not supply the required score",
      "definition or cut-offs."
    ))
  }
  if (grepl("public.lookup", determination)) {
    return("The source metadata does not supply the required public lookup.")
  }
  if (grepl("aggregate", determination)) {
    return(paste(
      "The public documentation does not supply the aggregate rule required",
      "for this variable."
    ))
  }
  if (grepl("codeframe|code.map|legacy.code", determination)) {
    return("No exact public codeframe is documented for this variable.")
  }
  "No exact public value mapping is documented for this variable."
}

# Apply explicit present-state exceptions at their documented occurrence
# scope. Structural blanks remain supported.
exceptions_path <- file.path(
  .docs_dir,
  "admin-value-exception-determinations.csv"
)
if (file.exists(exceptions_path)) {
  exceptions <- .read_csv(exceptions_path)
  scope_columns <- c(
    official_product = "product",
    official_table = "table"
  )
  for (j in seq_len(nrow(exceptions))) {
    rows <- info$asset == "PLIDA" &
      info$dataset == exceptions$dataset[[j]] &
      toupper(info$variable) == toupper(exceptions$variable[[j]])
    for (source_column in names(scope_columns)) {
      target_column <- scope_columns[[source_column]]
      scope_value <- .text(exceptions[[source_column]][[j]])
      if (nzchar(scope_value)) {
        rows <- rows & info[[target_column]] == scope_value
      }
    }
    if (!any(rows)) next
    structural <- grepl(
      "structurally_inapplicable|structural_source_blank",
      exceptions$determination[[j]],
      ignore.case = TRUE,
      perl = TRUE
    )
    if (!structural) info$value_support_status[rows] <- "unsupported"
    source_rows <- rows & info$valid_values == "[]"
    evidence_url <- .urls(exceptions$evidence_source[[j]])
    if (nzchar(evidence_url)) {
      info$value_source[source_rows] <- .evidence_label(
        exceptions$evidence_source[[j]],
        exceptions$dataset[[j]],
        evidence_url
      )
      info$value_source_url[source_rows] <- evidence_url
    }
    info$limitation[rows] <- .exception_limitation(
      exceptions$determination[[j]]
    )
  }
}

.apply_evidence <- function(data, dataset_column, variable_column,
                            source_column, url_column = NULL) {
  if (!nrow(data)) return(invisible(NULL))
  for (j in seq_len(nrow(data))) {
    dataset <- .text(data[[dataset_column]][[j]])
    variable <- .text(data[[variable_column]][[j]])
    rows <- info$asset == "PLIDA" & info$dataset == dataset &
      toupper(info$variable) == toupper(variable) &
      info$valid_values == "[]"
    if (!any(rows)) next
    source_text <- data[[source_column]][[j]]
    url_text <- if (!is.null(url_column)) data[[url_column]][[j]] else source_text
    evidence_url <- .urls(url_text)
    if (nzchar(evidence_url)) {
      info$value_source[rows] <<- .evidence_label(
        source_text, dataset, evidence_url
      )
      info$value_source_url[rows] <<- evidence_url
    }
  }
  invisible(NULL)
}

# Use source-backed decision records only for evidence provenance. The
# user-facing register does not import implementation fields or generation
# assessments from these records.
tax_path <- file.path(.docs_dir, "tax-admin-value-decisions.csv")
if (file.exists(tax_path)) {
  tax <- .read_csv(tax_path)
  implemented_candidate <- grepl(
    "^implemented_",
    .text(tax$candidate_status),
    perl = TRUE
  )
  tax$public_source <- ifelse(
    implemented_candidate,
    .text(tax$candidate_evidence_source),
    .text(tax$evidence_source)
  )
  tax$public_url <- ifelse(
    implemented_candidate & nzchar(.text(tax$candidate_evidence_url)),
    .text(tax$candidate_evidence_url),
    .text(tax$evidence_url)
  )
  .apply_evidence(
    tax,
    "dataset",
    "variable",
    "public_source",
    "public_url"
  )
}

acld_decisions_path <- file.path(
  .docs_dir,
  "acld-admin-value-decisions.csv"
)
if (file.exists(acld_decisions_path)) {
  acld_decisions <- .read_csv(acld_decisions_path)
  .apply_evidence(
    acld_decisions,
    "dataset",
    "variable",
    "evidence_url",
    "evidence_url"
  )
}

census_decisions_path <- file.path(
  .docs_dir,
  "admin-value-decision-ledger-census.csv"
)
if (file.exists(census_decisions_path)) {
  census_decisions <- .read_csv(census_decisions_path)
  .apply_evidence(
    census_decisions,
    "dataset",
    "variable",
    "evidence_source"
  )
}

aedc_decisions_path <- file.path(
  .docs_dir,
  "admin-value-decision-ledger-aedc.csv"
)
if (file.exists(aedc_decisions_path)) {
  aedc_decisions <- .read_csv(aedc_decisions_path)
  .apply_evidence(
    aedc_decisions,
    "dataset",
    "variable",
    "evidence_source"
  )
}

social_decisions_path <- file.path(
  .docs_dir,
  "admin-value-decision-ledger-social-employment.csv"
)
if (file.exists(social_decisions_path)) {
  social_decisions <- .read_csv(social_decisions_path)
  .apply_evidence(
    social_decisions,
    "dataset",
    "variable",
    "evidence_source"
  )
}

# Field-specific source limitations are stated without implementation details.
lodgement_rows <- info$dataset == "PIT_ITR" &
  info$variable == "LODGMENT_SOURCE"
info$value_source[lodgement_rows] <-
  "ATO ALife variable manual and ATO Taxation Statistics"
info$value_source_url[lodgement_rows] <- paste(
  "https://alife-research.app/assets/manual/itr/c_lodgement_type.md",
  paste0(
    "https://data.gov.au/data/dataset/faea4485-f407-457d-97f8-",
    "3f0822ccd654/resource/e19017b8-3894-44e2-995a-1c253c96977e"
  ),
  sep = " | "
)
info$limitation[lodgement_rows] <- paste(
  "Public annual counts do not distinguish agent paper, telephone and",
  "TaxPack Express returns."
)

if (exists("health_rows")) {
  info$value_source[health_rows] <-
    "Australian Government private health insurer register"
  info$value_source_url[health_rows] <-
    "https://www.privatehealth.gov.au/dynamic/Insurer/Index/"
  info$limitation[health_rows] <- paste(
    "The source is a current register. It does not provide year-specific",
    "insurer codes for 2009-10 to 2023-24."
  )
}

info$value_source[asset_opt_out_rows] <- "BLADE Data Item List"
info$value_source_url[asset_opt_out_rows] <-
  "https://www.abs.gov.au/statistics/data-integration/integrated-data/business-longitudinal-analysis-data-environment-blade"
info$limitation[asset_opt_out_rows] <- paste(
  "The BLADE item uses an i_ prefix and numeric storage. The PLIDA field",
  "uses character storage."
)
info$value_source[business_segment_rows] <- "BLADE Data Item List"
info$value_source_url[business_segment_rows] <-
  "https://www.abs.gov.au/statistics/data-integration/integrated-data/business-longitudinal-analysis-data-environment-blade"
info$limitation[business_segment_rows] <- paste(
  "The PLIDA 2023-24 occurrence extends one year beyond the BLADE",
  "availability list."
)

# The DEX locality variables are suburb text, not SA2 classifications. The
# public metadata does not publish complete codeframes for three other fields.
dex_locality_rows <- info$dataset == "DEX" & toupper(info$variable) %in%
  c("CLIENTLOCALITY", "OUTLETLOCALITY")
info$value_kind[dex_locality_rows] <- "text"
info$value_domain[dex_locality_rows] <- "open text domain"
info$variable_type[dex_locality_rows] <- "character"
info$variable_type_source[dex_locality_rows] <-
  "DSS Data Exchange bulk-upload specification"
info$value_source[dex_locality_rows] <-
  "DSS Data Exchange bulk-upload specification"
info$value_source_url[dex_locality_rows] <-
  "https://dex.dss.gov.au/document/131"
info$value_definition[dex_locality_rows] <-
  "The variable accepts suburb text with a maximum length of 50 characters."
info$limitation[dex_locality_rows] <-
  "The variable is an open suburb text field; no finite value list applies."

dex_unsupported_rows <- info$dataset == "DEX" & toupper(info$variable) %in%
  c("ASSESSEDBYCODE", "SOURCESYSTEMCODE", "THEDAYOFWEEK")
info$value_support_status[dex_unsupported_rows] <- "unsupported"
info$value_source[
  info$dataset == "DEX" & toupper(info$variable) == "ASSESSEDBYCODE"
] <- "DSS Data Exchange bulk-upload specification"
info$value_source_url[
  info$dataset == "DEX" & toupper(info$variable) == "ASSESSEDBYCODE"
] <- "https://dex.dss.gov.au/document/131"
info$value_source[
  info$dataset == "DEX" & toupper(info$variable) %in%
    c("SOURCESYSTEMCODE", "THEDAYOFWEEK")
] <- "PLIDA Data Item List"
info$value_source_url[
  info$dataset == "DEX" & toupper(info$variable) %in%
    c("SOURCESYSTEMCODE", "THEDAYOFWEEK")
] <- .plida_dil_url
info$limitation[
  info$dataset == "DEX" & toupper(info$variable) == "ASSESSEDBYCODE"
] <- paste(
  "The public specification confirms SDJOINT as an example, but the complete",
  "codeframe is available only through DEX reference data."
)
info$limitation[
  info$dataset == "DEX" & toupper(info$variable) == "SOURCESYSTEMCODE"
] <- paste(
  "The PLIDA DIL gives FOFMS and EXTNL as examples, not as a complete",
  "codeframe."
)
info$limitation[
  info$dataset == "DEX" & toupper(info$variable) == "THEDAYOFWEEK"
] <- "The public metadata does not define the weekday numbering convention."
info$variable_type_source[has_values] <- info$value_source[has_values]

info$topic_tags <- variable_info_topic_tags(
  asset = info$asset,
  dataset = info$dataset,
  variable = info$variable,
  description = info$variable_description,
  module = info$module,
  product = info$product,
  table = info$table
)

info <- info[order(
  info$asset,
  info$dataset,
  info$product,
  info$table,
  suppressWarnings(as.integer(info$table_number)),
  info$source_sheet,
  info$source_item,
  info$variable,
  info$variable_level,
  na.last = TRUE
), , drop = FALSE]
row.names(info) <- NULL
info$occurrence_id <- .make_occurrence_id(info)

# Where each half of a variable's page came from. Filled at the end of this
# script, once the sources have settled; declared here because `column_order`
# drops anything it does not name.
info$description_provenance <- ""
info$value_provenance <- ""

column_order <- c(
  "occurrence_id", "asset", "record_type", "collection_type", "dataset",
  "dataset_name", "module", "product", "table", "table_number",
  "source_sheet", "table_scope", "source_item", "variable",
  "variable_level", "official_description", "variable_description",
  "description_source", "description_source_url", "description_provenance",
  "variable_type",
  "variable_type_source", "reference_period", "available_periods",
  "official_valid_response", "value_kind", "value_domain", "valid_values",
  "value_definition", "value_source", "value_source_url", "value_provenance",
  "value_support_status", "limitation", "occurrence_count",
  "product_count", "table_count", "topic_tags", "metadata_source",
  "metadata_vintage"
)
missing_columns <- setdiff(column_order, names(info))
if (length(missing_columns)) {
  stop(
    "Missing variable-info column(s): ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}
info <- info[column_order]

expected_rows <- nrow(plida_variables) + nrow(blade_variables) +
  nrow(blade_keys)
if (nrow(info) != expected_rows) {
  stop(
    "Variable occurrence count mismatch: expected ", expected_rows,
    ", found ", nrow(info), ".",
    call. = FALSE
  )
}
if (anyDuplicated(info$occurrence_id)) {
  stop("variable-info occurrence_id values are not unique.", call. = FALSE)
}

required_text <- c(
  "occurrence_id", "asset", "record_type", "collection_type", "dataset",
  "dataset_name", "variable", "variable_description",
  "description_source", "description_source_url", "value_kind",
  "value_domain", "valid_values", "value_definition", "value_source",
  "value_source_url", "value_support_status", "limitation", "topic_tags",
  "metadata_source", "metadata_vintage", "description_provenance",
  "value_provenance"
)
# ---- apply researched value domains -----------------------------------------
#
# `admin-value-exception-determinations.csv` above recorded an earlier review
# that could only answer one question: is there an exact published codeframe?
# Everything it answered "no" to became `unsupported`. Re-researching those
# variables against the `guessed` bar resolved most of them, and found real
# published codeframes the first pass had missed. Those results arrive here.
#
# This runs AFTER the determinations deliberately: it is what supersedes them.
# It runs BEFORE the generic name-based guesses, so a researched domain always
# beats one inferred from the variable name alone.
resolved_path <- file.path(.docs_dir, "resolved-value-domains.csv")

# Variables research looked at and could not resolve. The generic guesses below
# must leave these alone: a name-shaped guess is a weaker claim than a specific
# finding that the domain is unknowable, and it would quietly overturn one.
researched_unsupported <- character(0)

if (file.exists(resolved_path)) local({
  resolved <- .read_csv(resolved_path)
  applied <- 0L
  occurrences <- 0L
  described <- 0L
  for (column in c("variable_description", "description_source",
                   "description_source_url", "value_definition",
                   "limitation", "description_provenance",
                   "value_provenance")) {
    if (is.null(resolved[[column]])) resolved[[column]] <- NA_character_
  }

  for (j in seq_len(nrow(resolved))) {
    rows <- info$dataset == resolved$dataset[[j]] &
      toupper(info$variable) == toupper(resolved$variable[[j]])
    if (!any(rows)) next

    status <- .text(resolved$status[[j]])
    if (status == "unsupported") {
      researched_unsupported <<- c(
        researched_unsupported,
        paste(resolved$dataset[[j]], toupper(resolved$variable[[j]]))
      )
      next
    }

    values <- tryCatch(
      as.character(jsonlite::fromJSON(resolved$values[[j]])),
      error = function(e) character(0)
    )
    values <- values[!is.na(values) & nzchar(values)]
    enumerated <- isTRUE(as.logical(resolved$enumerated[[j]])) && length(values)

    # Research adds; it does not subtract. A pass looking for value domains
    # that concludes "this is an opaque identifier" has learned nothing about
    # provenance, so it must not demote a variable the evidence process already
    # established as `sourced`.
    #
    # A resolution that carries values has earned the right to set the status,
    # because it is then making a claim about the codes themselves.
    demotes <- status == "guessed" && !length(values) &&
      all(info$value_support_status[rows] == "sourced")

    # When the demotion is declined, the domain text is declined with it. The
    # research wrote that text to describe a guess, and it often says so — a
    # `sourced` row carrying a domain marked "(inferred)" contradicts itself.
    if (!demotes) {
      info$value_support_status[rows] <<- status
      info$value_domain[rows] <<- resolved$value_domain[[j]]
    } else {
      # The demotion is declined, but a name for the domain is not a claim
      # about provenance and "not specified" is worse than any of them. Where
      # the row has no domain at all, take the researched one and leave the
      # `sourced` status alone.
      # A domain the research itself marks "(inferred)" is the one thing that
      # cannot come across: the row keeps its `sourced` status, and a sourced
      # row carrying an inferred domain contradicts itself.
      offered <- .text(resolved$value_domain[[j]])
      unnamed <- rows & (!nzchar(.text(info$value_domain)) |
                           info$value_domain == "not specified")
      if (any(unnamed) && nzchar(offered) &&
          !grepl("(inferred)", offered, fixed = TRUE)) {
        info$value_domain[unnamed] <<- offered
      }
    }

    # A guessed resolution often has no source, because there is nothing to
    # cite: the domain came from the name and the description. Where research
    # did find a page that supports the shape without confirming the mapping,
    # that citation is kept. Otherwise the existing source stays, and the
    # `guessed` status is what tells the reader not to trust the mapping.
    new_source <- .text(resolved$value_source[[j]])
    if (nzchar(new_source)) {
      info$value_source[rows] <<- new_source
    } else if (status == "guessed") {
      info$value_source[rows] <<-
        "Inferred from the variable name and description"
    }
    new_url <- .text(resolved$value_source_url[[j]])
    if (nzchar(new_url)) info$value_source_url[rows] <<- new_url

    if (enumerated) {
      info$valid_values[rows] <<- .json_array(values)
      info$value_definition[rows] <<- if (status == "sourced") {
        "The source publishes this value domain."
      } else {
        paste(
          "The values are inferred from the variable name and description,",
          "not published by the source."
        )
      }
    } else {
      size <- suppressWarnings(as.integer(resolved$full_list_size[[j]]))
      oversized <- !is.na(size) && size > length(values)
      info$value_definition[rows] <<- if (oversized && status == "sourced") {
        # Documented, but too large to carry. Naming the size is the useful
        # part: it tells a reader the column holds one of 358,010 mesh blocks
        # rather than leaving them to wonder whether anything is known.
        paste0(
          "The source publishes this value domain of ",
          format(size, big.mark = ","),
          " values. It is too large to list here; see the value source."
        )
      } else if (oversized) {
        # Same shape, but a guess must never say the source publishes it. The
        # size is the inferred domain's size, not a published count.
        paste0(
          "The value domain is inferred to hold about ",
          format(size, big.mark = ","),
          " values, which is too many to list here. The source does not",
          " confirm the mapping."
        )
      } else {
        # Not every domain is a list. A timestamp, a provider number and a
        # sequence counter each have a definite shape and no finite set of
        # values.
        "The domain is open, so no finite value list applies."
      }
    }

    info$limitation[rows] <<- if (status == "sourced") {
      if (enumerated) {
        "The source defines the value domain. Frequency information is not included."
      } else {
        paste(
          "The source defines the value domain but it is not listed here.",
          "Generated values follow the published format rather than the",
          "published list."
        )
      }
    } else {
      paste(
        "The value domain is inferred from the variable name and description,",
        "not published by the source. The values are of the right shape but",
        "are not a confirmed mapping."
      )
    }

    # Written prose beats the generic sentences above wherever research has
    # any. The generic text answers "is there a code list?", which for an
    # opaque identifier is both true and useless: the reader wants to know
    # what the thing identifies, and that the column is worth joining on.
    # Where a finding supplies nothing, the fallbacks stand.
    curated_description <- .text(resolved$variable_description[[j]])
    if (nzchar(curated_description)) {
      info$variable_description[rows] <<- curated_description
      info$description_source[rows] <<-
        .text(resolved$description_source[[j]])
      info$description_source_url[rows] <<-
        .text(resolved$description_source_url[[j]])
      described <- described + 1L
    }
    curated_definition <- .text(resolved$value_definition[[j]])
    if (nzchar(curated_definition)) {
      info$value_definition[rows] <<- curated_definition
    }
    curated_limitation <- .text(resolved$limitation[[j]])
    if (nzchar(curated_limitation)) {
      info$limitation[rows] <<- curated_limitation
    }

    # `official` is the one label research has to claim for itself: only the
    # researcher knows whether the text is quoted from the source or written
    # around it. The rest is derived at the end of this script.
    for (column in c("description_provenance", "value_provenance")) {
      declared <- .text(resolved[[column]][[j]])
      if (nzchar(declared)) info[[column]][rows] <<- declared
    }

    applied <- applied + 1L
    occurrences <- occurrences + sum(rows)
  }

  message("Wrote a researched description for ", described, " variables.")
  message("Applied researched value domains for ", applied,
          " variables across ", format(occurrences, big.mark = ","),
          " occurrences.")
})

# ---- infer value domains from variable names -------------------------------
#
# Anything still without a value domain gets one guessed from its name, and is
# labelled `guessed` so a reader knows the codes are inferred rather than
# published. This runs LAST, so it can never overwrite a sourced domain: it
# only fills genuine blanks.
local({
  source(file.path(.repo_root, "data-raw", "value_guess_rules.R"), local = TRUE)

  blank_values <- trimws(info$valid_values) %in% c("", "[]")
  no_domain <- !nzchar(.text(info$value_domain)) |
    info$value_domain %in% c("not specified", "")
  # `unsupported` is eligible now, and that is the whole point of the status.
  #
  # It used to be excluded, which quietly froze 1,160 occurrences: a variable
  # the old review had marked `unsupported` was never offered a rule, so ACLD's
  # remoteness-area fields stayed empty while the very same rule labelled 125
  # remoteness-area occurrences elsewhere in the registry.
  #
  # Nothing is forced. A rule fires only when it recognises the name, so a
  # variable that nothing recognises — an undescribed field like NDIS
  # POSTRACHSGSUPP — stays `unsupported`, and the status keeps meaning
  # something. Structural blanks never reach here: a variable the source
  # documents as empty is left `sourced` by the determinations pass above, so
  # it can never be given invented values.
  eligible <- blank_values & no_domain &
    info$value_support_status %in% c("sourced", "not_applicable", "unsupported")
  eligible <- eligible &
    !(paste(info$dataset, toupper(info$variable)) %in% researched_unsupported)

  upper <- toupper(info$variable)
  matched_rule <- rep(NA_character_, nrow(info))
  for (rule in value_guess_rules) {
    open <- eligible & is.na(matched_rule)
    if (!any(open)) break
    hit <- open & grepl(rule$pattern, upper, perl = TRUE)
    if (!is.null(rule$desc_pattern)) {
      hit <- hit & grepl(rule$desc_pattern, info$official_description,
                         ignore.case = TRUE, perl = TRUE)
    }
    if (!any(hit)) next
    matched_rule[hit] <- rule$id
    info$value_support_status[hit] <<- "guessed"
    info$value_domain[hit] <<- rule$domain
    if (!nzchar(rule$type)) rule$type <- ""
    blank_type <- hit & !nzchar(.text(info$variable_type))
    info$variable_type[blank_type] <<- rule$type
    info$variable_type_source[blank_type] <<- "Inferred from the variable name"
    if (length(rule$values)) {
      info$valid_values[hit] <<- .json_array(rule$values)
      info$value_definition[hit] <<- paste(
        "The values are inferred from the variable name, not published by the",
        "source."
      )
    } else {
      info$value_definition[hit] <<- paste(
        "The domain is inferred from the variable name. It is an open domain,",
        "so no finite value list applies."
      )
    }
    info$value_source[hit] <<- "Inferred from the variable name"
    info$limitation[hit] <<- paste0(
      "The value domain is inferred from the variable name, not published by ",
      "the source. ",
      if (!is.null(rule$note)) paste0(rule$note, " ") else "",
      "The values are of the right shape but are not a confirmed mapping."
    )
  }
  n <- sum(!is.na(matched_rule))
  message("Guessed a value domain for ", format(n, big.mark = ","),
          " occurrences across ", length(unique(stats::na.omit(matched_rule))),
          " rules.")
  message("Still without a domain: ",
          format(sum(eligible & is.na(matched_rule)), big.mark = ","))
})

# ---- provenance ------------------------------------------------------------
#
# A reader deserves to know which of three things they are reading: the
# custodian's own wording out of the data item list, a published classification
# quoted and cited, or prose written from several sources and general knowledge.
# The three are not equally strong and should not look alike on the page.
#
# Research declares `official` when it quotes a source directly; everything
# else is derived here, so a variable nobody has researched still gets an
# honest label.
local({
  from_dil <- function(source) {
    nzchar(source) &
      grepl("(PLIDA|BLADE) DIL|Data Item List", source) &
      !grepl(" and ", source, fixed = TRUE)
  }
  # `official` claims the text on the page is the source's own. Deriving that
  # from a citation alone would overclaim: naming the ABS Address Register
  # Information Guide does not make a paragraph written around it a quote.
  #
  # A transcribed code list is the exception, and the evidence is in the data.
  # Where `valid_values` holds codes taken from a cited external publisher,
  # those codes and their labels ARE the source's own text — the METEOR value
  # domains and the DSS Aristotle code lists are carried across verbatim.
  quoted_values <- function(source, url, values) {
    nzchar(source) & !grepl("^Inferred from", source) &
      grepl("^https?://", url) & trimws(values) != "[]"
  }

  description <- .text(info$description_source)
  blank <- !nzchar(.text(info$description_provenance))
  # Prose, unless research says it quoted the source. Never derived `official`.
  info$description_provenance[blank] <<- ifelse(
    from_dil(description[blank]), "metadata", "ai"
  )

  value <- .text(info$value_source)
  url <- .text(info$value_source_url)
  blank <- !nzchar(.text(info$value_provenance))
  info$value_provenance[blank] <<- ifelse(
    from_dil(value[blank]), "metadata",
    ifelse(
      quoted_values(value[blank], url[blank], info$valid_values[blank]),
      "official", "ai"
    )
  )

  message("Description provenance:")
  print(table(info$description_provenance), quote = FALSE)
  message("Value provenance:")
  print(table(info$value_provenance), quote = FALSE)
})

allowed_provenance <- c("metadata", "official", "ai")
for (column in c("description_provenance", "value_provenance")) {
  if (!all(info[[column]] %in% allowed_provenance)) {
    stop("Unexpected ", column, ".", call. = FALSE)
  }
}

for (column in required_text) {
  if (any(!nzchar(.text(info[[column]])))) {
    stop("Blank required variable-info field: ", column, call. = FALSE)
  }
}

allowed_status <- c("sourced", "guessed", "unsupported", "not_applicable")
if (!all(info$value_support_status %in% allowed_status)) {
  stop("Unexpected value_support_status.", call. = FALSE)
}
if (any(
  info$value_support_status == "not_applicable" &
    info$collection_type != "survey"
)) {
  stop("not_applicable is restricted to survey occurrences.", call. = FALSE)
}
# A survey occurrence may now be `guessed`: survey instruments are among the
# most regular things to infer from, because a whole module shares one answer
# scale. It stays `not_applicable` when no rule matches.
if (any(
  info$collection_type == "survey" &
    !info$value_support_status %in% c("not_applicable", "guessed")
)) {
  stop("A survey occurrence must be not_applicable or guessed.", call. = FALSE)
}

valid_json_shape <- startsWith(info$valid_values, "[") &
  endsWith(info$valid_values, "]")
if (!all(valid_json_shape)) {
  stop("A valid_values field is not a JSON array.", call. = FALSE)
}

topic_parts <- strsplit(info$topic_tags, ",", fixed = TRUE)
if (any(lengths(topic_parts) == 0L) ||
    any(!unlist(topic_parts, use.names = FALSE) %in%
          VARIABLE_INFO_TOPIC_VOCABULARY)) {
  stop("A topic_tags field contains an unknown topic.", call. = FALSE)
}
canonical_topics <- vapply(topic_parts, function(tags) {
  paste(
    VARIABLE_INFO_TOPIC_VOCABULARY[
      VARIABLE_INFO_TOPIC_VOCABULARY %in% unique(tags)
    ],
    collapse = ","
  )
}, character(1))
if (!identical(info$topic_tags, canonical_topics)) {
  stop("topic_tags values are not in canonical order.", call. = FALSE)
}

prohibited_columns <- c(
  "output_state", "implementation_class", "value_rule", "generation_rule",
  "observed_generated_domain", "required_next_action"
)
if (length(intersect(names(info), prohibited_columns))) {
  stop("variable-info contains a prohibited implementation field.",
       call. = FALSE)
}

public_text <- do.call(paste, c(info[c(
  "variable_description", "description_source", "value_definition",
  "value_source", "limitation"
)], sep = " "))
if (any(grepl(
  paste0(
    "remediat|before this|after this|previous version|initially|",
    "required next action|implementation class|generation rule|",
    "(^|[ ;])(R/|src/|tests/|inst/)"
  ),
  public_text,
  ignore.case = TRUE,
  perl = TRUE
))) {
  stop("variable-info contains development-history or implementation text.",
       call. = FALSE)
}

variable_info_path <- file.path(
  .repo_root, "inst", "variable-info.csv.gz"
)
variable_info_connection <- gzfile(
  variable_info_path, open = "wt", encoding = "UTF-8"
)
utils::write.csv(
  info,
  variable_info_connection,
  row.names = FALSE,
  na = ""
)
close(variable_info_connection)
legacy_variable_info_path <- file.path(
  .repo_root, "inst", "variable-info.csv"
)
if (file.exists(legacy_variable_info_path)) {
  unlink(legacy_variable_info_path)
}

cat("Wrote ", nrow(dataset_info), " dataset rows to ",
    normalizePath(dataset_info_path, winslash = "/"), ".\n", sep = "")
cat("Wrote ", nrow(info), " variable occurrences to ",
    normalizePath(variable_info_path, winslash = "/"), ".\n", sep = "")
print(with(info, table(asset, collection_type, value_support_status)))
