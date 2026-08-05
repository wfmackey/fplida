#!/usr/bin/env Rscript

# Build one conservative remediation row for each administrative
# dataset-variable pair that is entirely missing in the current audit output.
# This script uses only checked-in registers and base R. It does not infer or
# invent a value domain when exact source evidence is absent.

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
  }
  normalizePath(
    file.path("scripts", "build_admin_value-remediation-register.R"),
    mustWork = TRUE
  )
}

.repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."),
  winslash = "/",
  mustWork = TRUE
)

.read_csv <- function(path) {
  if (!file.exists(path)) stop("Required input not found: ", path, call. = FALSE)
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = "",
    comment.char = "",
    fileEncoding = "UTF-8"
  )
}

.require_columns <- function(x, columns, path) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    stop(
      "Missing required column(s) in ", path, ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(x)
}

.normalise_text <- function(x) {
  x <- enc2utf8(as.character(x))
  present <- !is.na(x)
  x[present] <- trimws(gsub("[[:space:]]+", " ", x[present], perl = TRUE))
  x[present & !nzchar(x)] <- NA_character_
  x
}

.collapse_unique <- function(x) {
  x <- unique(.normalise_text(x))
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  paste(x, collapse = " | ")
}

.count_unique <- function(x) {
  x <- unique(.normalise_text(x))
  sum(!is.na(x))
}

.pair_key <- function(dataset, variable) {
  paste(dataset, variable, sep = "\r")
}

.column_or_na <- function(x, candidates) {
  hit <- intersect(candidates, names(x))
  if (!length(hit)) return(rep(NA_character_, nrow(x)))
  x[[hit[[1L]]]]
}

.gap_path <- file.path(
  .repo_root, "inst", "internal-docs", "admin-value-gap-register.csv"
)
.crosswalk_path <- file.path(
  .repo_root, "inst", "internal-docs", "generated-variable-crosswalk.csv"
)
.output_path <- file.path(
  .repo_root, "inst", "internal-docs",
  "admin-value-remediation-register.csv"
)

gap <- .read_csv(.gap_path)
.require_columns(
  gap,
  c(
    "asset", "dataset", "official_product", "official_table",
    "logical_output", "variable", "type", "n_rows",
    "official_description", "official_valid_response"
  ),
  .gap_path
)

text_columns <- setdiff(names(gap), "n_rows")
gap[text_columns] <- lapply(gap[text_columns], .normalise_text)
if (any(is.na(gap$dataset)) || any(is.na(gap$variable))) {
  stop("The gap register contains a missing dataset or variable key.", call. = FALSE)
}

gap_key <- .pair_key(gap$dataset, gap$variable)
gap_groups <- split(seq_len(nrow(gap)), gap_key)

remediation_rows <- lapply(gap_groups, function(index) {
  part <- gap[index, , drop = FALSE]
  example_order <- order(
    part$official_product,
    part$official_table,
    part$logical_output,
    na.last = TRUE
  )
  example <- part[example_order[[1L]], , drop = FALSE]

  table_present <- !is.na(part$official_table)
  table_keys <- paste(
    ifelse(is.na(part$official_product), "", part$official_product),
    part$official_table,
    sep = "\r"
  )

  data.frame(
    asset = .collapse_unique(part$asset),
    dataset = part$dataset[[1L]],
    variable = part$variable[[1L]],
    occurrence_count = length(index),
    product_count = .count_unique(part$official_product),
    table_count = length(unique(table_keys[table_present])),
    type = .collapse_unique(part$type),
    official_descriptions = .collapse_unique(part$official_description),
    official_valid_responses = .collapse_unique(part$official_valid_response),
    example_product = example$official_product,
    example_table = example$official_table,
    example_logical_output = example$logical_output,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})

register <- do.call(rbind, remediation_rows)
register <- register[order(register$dataset, register$variable), , drop = FALSE]
rownames(register) <- NULL
register_key <- .pair_key(register$dataset, register$variable)

crosswalk <- .read_csv(.crosswalk_path)
.require_columns(
  crosswalk,
  c(
    "dataset", "table", "variable", "type", "crosswalk_status",
    "candidate_variable", "candidate_product", "basis", "notes"
  ),
  .crosswalk_path
)
crosswalk[] <- lapply(crosswalk, .normalise_text)
crosswalk_key <- .pair_key(crosswalk$dataset, crosswalk$variable)
crosswalk_groups <- split(seq_len(nrow(crosswalk)), crosswalk_key)

register$existing_generator_exact_match <- register_key %in% names(crosswalk_groups)
register$existing_generator_match_count <- integer(nrow(register))
register$existing_generator_tables <- NA_character_
register$existing_generator_types <- NA_character_
register$existing_generator_crosswalk_statuses <- NA_character_
register$existing_generator_candidate_variables <- NA_character_
register$existing_generator_candidate_products <- NA_character_
register$existing_generator_basis <- NA_character_
register$existing_generator_notes <- NA_character_

for (i in which(register$existing_generator_exact_match)) {
  part <- crosswalk[crosswalk_groups[[register_key[[i]]]], , drop = FALSE]
  register$existing_generator_match_count[[i]] <- nrow(part)
  register$existing_generator_tables[[i]] <- .collapse_unique(part$table)
  register$existing_generator_types[[i]] <- .collapse_unique(part$type)
  register$existing_generator_crosswalk_statuses[[i]] <-
    .collapse_unique(part$crosswalk_status)
  register$existing_generator_candidate_variables[[i]] <-
    .collapse_unique(part$candidate_variable)
  register$existing_generator_candidate_products[[i]] <-
    .collapse_unique(part$candidate_product)
  register$existing_generator_basis[[i]] <- .collapse_unique(part$basis)
  register$existing_generator_notes[[i]] <- .collapse_unique(part$notes)
}

evidence_paths <- sort(Sys.glob(file.path(
  .repo_root, "inst", "internal-docs", "*-variable-code-evidence.csv"
)))
if (!length(evidence_paths)) {
  stop("No variable-code evidence registers were found.", call. = FALSE)
}

evidence_rows <- lapply(evidence_paths, function(path) {
  evidence <- .read_csv(path)
  .require_columns(evidence, c("variable", "evidence_status"), path)
  if (!"dataset" %in% names(evidence)) {
    if (basename(path) == "census-variable-code-evidence.csv") {
      evidence$dataset <- "CENSUS"
    } else {
      stop(
        "Cannot infer dataset for evidence register: ", path,
        call. = FALSE
      )
    }
  }

  data.frame(
    dataset = .normalise_text(evidence$dataset),
    variable = .normalise_text(evidence$variable),
    evidence_file = basename(path),
    evidence_status = .normalise_text(evidence$evidence_status),
    evidence_source_url = .normalise_text(.column_or_na(
      evidence, c("source_url", "official_source_url")
    )),
    evidence_source_summary = .normalise_text(.column_or_na(
      evidence, c("source_summary", "official_code_or_source_summary")
    )),
    evidence_generated_assessment = .normalise_text(.column_or_na(
      evidence, "current_generated_assessment"
    )),
    evidence_required_next_action = .normalise_text(.column_or_na(
      evidence, "required_next_action"
    )),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})
evidence <- do.call(rbind, evidence_rows)
if (any(is.na(evidence$dataset)) || any(is.na(evidence$variable))) {
  stop("An evidence register contains a missing dataset or variable key.",
       call. = FALSE)
}
evidence_key <- .pair_key(evidence$dataset, evidence$variable)
evidence_groups <- split(seq_len(nrow(evidence)), evidence_key)

register$evidence_exact_match <- register_key %in% names(evidence_groups)
register$evidence_file <- NA_character_
register$evidence_status <- "no_exact_variable_evidence"
register$evidence_source_url <- NA_character_
register$evidence_source_summary <- NA_character_
register$evidence_generated_assessment <- NA_character_
register$evidence_required_next_action <- NA_character_

for (i in which(register$evidence_exact_match)) {
  part <- evidence[evidence_groups[[register_key[[i]]]], , drop = FALSE]
  register$evidence_file[[i]] <- .collapse_unique(part$evidence_file)
  register$evidence_status[[i]] <- .collapse_unique(part$evidence_status)
  register$evidence_source_url[[i]] <-
    .collapse_unique(part$evidence_source_url)
  register$evidence_source_summary[[i]] <-
    .collapse_unique(part$evidence_source_summary)
  register$evidence_generated_assessment[[i]] <-
    .collapse_unique(part$evidence_generated_assessment)
  register$evidence_required_next_action[[i]] <-
    .collapse_unique(part$evidence_required_next_action)
}

# Only statuses that explicitly say the current codes were tested against an
# official dictionary are treated as source-backed. All other matches remain
# partial/local even when they provide useful collection context or a URL.
.source_backed_statuses <- c(
  "official_abs_dictionary_dollar_code_current_tested",
  "official_abs_dictionary_numeric_or_year_current_tested",
  "official_abs_dictionary_small_code_set_current_tested"
)

register$source_coverage <- "no_exact_variable_evidence"
matched <- register$evidence_exact_match
missing_url <- matched & (
  is.na(register$evidence_source_url) |
    !nzchar(register$evidence_source_url)
)
register$source_coverage[missing_url] <- "exact_evidence_missing_source_url"

matched_with_url <- matched & !missing_url
status_sets <- strsplit(
  ifelse(is.na(register$evidence_status), "", register$evidence_status),
  " | ",
  fixed = TRUE
)
fully_source_backed <- vapply(
  status_sets,
  function(status) length(status) > 0L && all(status %in% .source_backed_statuses),
  logical(1)
)
register$source_coverage[matched_with_url & fully_source_backed] <-
  "exact_source_backed_variable_evidence"
register$source_coverage[matched_with_url & !fully_source_backed] <-
  "exact_partial_or_local_variable_evidence"

register$initial_action <- "source_required_before_implementation"
has_generator <- register$existing_generator_exact_match

register$initial_action[
  has_generator &
    register$source_coverage == "exact_source_backed_variable_evidence"
] <- "validate_then_route_existing_generator_field"

register$initial_action[
  has_generator &
    register$source_coverage == "exact_partial_or_local_variable_evidence"
] <- "resolve_evidence_limit_then_route_existing_generator_field"

register$initial_action[
  has_generator & register$source_coverage %in% c(
    "no_exact_variable_evidence", "exact_evidence_missing_source_url"
  )
] <- "source_then_validate_existing_generator_field"

register$initial_action[
  !has_generator &
    register$source_coverage == "exact_source_backed_variable_evidence"
] <- "implement_from_cited_source"

register$initial_action[
  !has_generator &
    register$source_coverage == "exact_partial_or_local_variable_evidence"
] <- "complete_source_mapping_before_implementation"

.value_semantics <- function(name, description) {
  upper <- toupper(name)
  text <- toupper(paste(name, description))
  if (grepl(
    "AEUID|SPINE|SCRAM|HASH|GUID|UUID|IDENTIFIER|IDNTFR|(^|_)(ID|KEY)$",
    upper, perl = TRUE
  ) || grepl("UNIQUE (?:ID|IDENTIFIER|NUMBER)", text, perl = TRUE)) {
    return("stable_identifier")
  }
  if (grepl(
    "DATE|TIMESTAMP|(^|_)(DT|STS|ETS)$|EARLIEST|LATEST",
    text, perl = TRUE
  )) {
    return("date_or_timestamp")
  }
  if (grepl(
    "AMOUNT|(^|_)(AMT|INCM|INCOME|TAX|WAGE|SALARY|BALANCE)(_|$)",
    text, perl = TRUE
  ) || grepl(
    "CAPITAL GAIN|CAPITAL LOSS|CASH AND OTHER CONSIDERATIONS",
    text, perl = TRUE
  )) {
    return("financial_amount")
  }
  if (grepl(
    "SA1|SA2|SA3|SA4|MESH|LGA|PHN|POSTCODE|REMOTENESS|SEIFA",
    text, perl = TRUE
  )) {
    return("geography_or_area_classification")
  }
  if (grepl("SCORE|INDEX|DECILE|QUINTILE|PERCENTILE", text, perl = TRUE)) {
    return("score_or_index")
  }
  if (grepl(
    "FLAG|INDICATOR|(^|_)(IND|FL)$|WHETHER|YES.?NO|ELIGIB",
    text, perl = TRUE
  )) {
    return("indicator_or_flag")
  }
  if (grepl(
    "COUNT|NUMBER OF|DURATION|PERIOD OF RESIDENCE|(^|_)(CNT|NUM|DAYS|MONTHS|YEARS)(_|$)",
    text, perl = TRUE
  )) {
    return("count_or_duration")
  }
  if (grepl("NAME|DESCRIPTION|LABEL|TITLE|FREE.?TEXT", text, perl = TRUE)) {
    return("label_or_text")
  }
  if (grepl("CODE|TYPE|STATUS|CATEGORY|CLASS|REASON|LEVEL", text,
            perl = TRUE)) {
    return("categorical_codeframe")
  }
  "dataset_specific_value"
}

.target_rule <- c(
  stable_identifier = paste(
    "Generate a stable synthetic identifier.",
    "Reuse it for the same entity in every linked table."
  ),
  date_or_timestamp = paste(
    "Generate an event date within the official reference period.",
    "Enforce the relevant chronological order."
  ),
  financial_amount = paste(
    "Generate a plausible monetary value from linked economic state.",
    "Enforce accounting identities and valid signs."
  ),
  geography_or_area_classification = paste(
    "Map the linked residence or service location through the correct",
    "vintage-specific reference geography."
  ),
  score_or_index = paste(
    "Use the official scale and missing-value rules.",
    "Derive the value from correlated source items when required."
  ),
  indicator_or_flag = paste(
    "Use the official indicator domain.",
    "Derive the value from the linked event or person state."
  ),
  count_or_duration = paste(
    "Generate a non-negative count or duration from linked events.",
    "Apply the official upper bound when one exists."
  ),
  label_or_text = paste(
    "Look up the official label from its generated code.",
    "Do not generate arbitrary prose."
  ),
  categorical_codeframe = paste(
    "Use the official vintage-specific codeframe.",
    "Preserve structural and item non-response as missing."
  ),
  dataset_specific_value = paste(
    "Define a dataset-specific rule from the official variable definition.",
    "Link the rule to related generated fields."
  )
)

register$value_semantics <- mapply(
  .value_semantics,
  register$variable,
  ifelse(is.na(register$official_descriptions), "",
         register$official_descriptions),
  USE.NAMES = FALSE
)
register$target_value_rule <- unname(.target_rule[register$value_semantics])

# Cardinality assertions intentionally pin this register to the current gap
# audit. A changed audit or evidence inventory must be reviewed, not silently
# accepted as an equivalent remediation denominator.
.expected_register_rows <- 2593L
.expected_distinct_variable_names <- 2538L
.expected_gap_occurrences <- 16462L
.expected_exact_generator_matches <- 212L
.expected_exact_evidence_matches <- 173L
.expected_source_backed_matches <- 24L

stopifnot(
  nrow(register) == .expected_register_rows,
  length(unique(register$variable)) == .expected_distinct_variable_names,
  !anyDuplicated(register_key),
  sum(register$occurrence_count) == .expected_gap_occurrences,
  all(register$occurrence_count >= register$table_count),
  all(register$table_count >= 1L),
  all(register$product_count >= 1L),
  sum(register$existing_generator_exact_match) ==
    .expected_exact_generator_matches,
  sum(register$evidence_exact_match) == .expected_exact_evidence_matches,
  sum(register$source_coverage ==
        "exact_source_backed_variable_evidence") ==
    .expected_source_backed_matches,
  all(!register$evidence_exact_match |
        (!is.na(register$evidence_file) &
           nzchar(register$evidence_file) &
           register$evidence_status != "no_exact_variable_evidence")),
  all(register$source_coverage != "exact_source_backed_variable_evidence" |
        (!is.na(register$evidence_source_url) &
           nzchar(register$evidence_source_url))),
  all(!register$evidence_exact_match |
        register$existing_generator_exact_match),
  all(!is.na(register$source_coverage) & nzchar(register$source_coverage)),
  all(!is.na(register$initial_action) & nzchar(register$initial_action)),
  all(!is.na(register$value_semantics) & nzchar(register$value_semantics)),
  all(!is.na(register$target_value_rule) &
        nzchar(register$target_value_rule)),
  all(register$initial_action !=
        "validate_then_route_existing_generator_field" |
        (register$existing_generator_exact_match &
           register$source_coverage ==
             "exact_source_backed_variable_evidence")),
  all(register$initial_action != "implement_from_cited_source" |
        (!register$existing_generator_exact_match &
           register$source_coverage ==
             "exact_source_backed_variable_evidence"))
)

utils::write.csv(
  register,
  .output_path,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

round_trip <- .read_csv(.output_path)
stopifnot(
  nrow(round_trip) == .expected_register_rows,
  identical(names(round_trip), names(register)),
  length(unique(round_trip$variable)) == .expected_distinct_variable_names
)

cat("Wrote ", normalizePath(.output_path, winslash = "/"), "\n", sep = "")
cat("Rows: ", nrow(register), "\n", sep = "")
cat("Distinct variable names: ", length(unique(register$variable)), "\n",
    sep = "")
cat("Source coverage:\n")
print(table(register$source_coverage), quote = FALSE)
cat("Initial actions:\n")
print(table(register$initial_action), quote = FALSE)
