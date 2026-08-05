#!/usr/bin/env Rscript

# Build the final decision records for the original administrative value gaps.
#
# The occurrence ledger keeps one row for each original all-missing canonical
# variable occurrence. The pair ledger aggregates those rows by dataset and
# variable. A reviewed determination is mandatory for every variable that is
# still entirely missing in the final fixed-seed audit.

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]),
                         mustWork = TRUE))
  }
  normalizePath(
    file.path("scripts", "build_admin_value_decision_ledger.R"),
    mustWork = TRUE
  )
}

.repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."),
  winslash = "/",
  mustWork = TRUE
)

.parse_args <- function(args) {
  out <- list(
    audit_dir = NULL,
    output_dir = file.path(.repo_root, "inst", "internal-docs"),
    exceptions = file.path(
      .repo_root, "inst", "internal-docs",
      "admin-value-exception-determinations.csv"
    )
  )
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (!arg %in% c("--audit-dir", "--output-dir", "--exceptions")) {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    if (i == length(args)) stop("Missing value after ", arg, call. = FALSE)
    key <- gsub("-", "_", sub("^--", "", arg))
    out[[key]] <- args[[i + 1L]]
    i <- i + 2L
  }
  if (is.null(out$audit_dir)) {
    stop("Provide --audit-dir PATH.", call. = FALSE)
  }
  out
}

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

.require_complete_text <- function(x, columns, path) {
  for (column in columns) {
    value <- trimws(as.character(x[[column]]))
    invalid <- is.na(value) | !nzchar(value)
    if (any(invalid)) {
      stop(
        "Missing or empty ", column, " in ", path,
        " at row ", which(invalid)[[1L]], ".",
        call. = FALSE
      )
    }
  }
  invisible(x)
}

.validated_count <- function(x, column, path) {
  value <- suppressWarnings(as.numeric(x))
  invalid <- is.na(value) | !is.finite(value) | value < 0 | value != floor(value)
  if (any(invalid)) {
    stop(
      "Invalid non-negative integer ", column, " in ", path,
      " at row ", which(invalid)[[1L]], ".",
      call. = FALSE
    )
  }
  value
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

.key_part <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "<NA>"
  x
}

.occurrence_key <- function(asset, dataset, product, table, output, variable) {
  paste(
    .key_part(asset), .key_part(dataset), .key_part(product),
    .key_part(table), .key_part(output), toupper(.key_part(variable)),
    sep = "\r"
  )
}

.pair_key <- function(dataset, variable) {
  paste(.key_part(dataset), toupper(.key_part(variable)), sep = "\r")
}

.implementation_path <- function(dataset) {
  if (dataset %in% c("MBS", "PBS")) {
    return(paste0("R/generate_", tolower(dataset), ".R"))
  }
  if (dataset == "CENSUS") {
    return("R/dil_census_values.R; R/dil_geography_values.R; src/rust/src/census_2021.rs")
  }
  if (dataset == "ACLD") {
    return("R/acld_codeframes.R; R/generate_acld.R; src/rust/src/acld.rs")
  }
  if (dataset == "AEDC") {
    return("R/generate_aedc.R; src/rust/src/aedc.rs")
  }
  if (dataset == "AIR") {
    return("R/generate_air.R; src/rust/src/air.rs")
  }
  if (dataset == "NACDC") {
    return("R/generate_nacdc.R; src/rust/src/nacdc.rs")
  }
  if (dataset == "NDIS") return("R/dil_ndis_values.R; src/rust/src/ndis.rs")
  if (dataset == "TRAVELLERS") {
    return("R/complete_dil_structures.R; src/rust/src/travellers.rs")
  }
  if (dataset %in% c("TVA", "A&T")) return("R/dil_vet_apprentice_values.R")
  if (dataset == "HE") return("R/dil_higher_education_values.R")
  if (dataset %in% c("AMEP", "APSED", "MT_DEMOGS", "SDB", "VISA")) {
    return("R/dil_home_affairs_values.R")
  }
  if (dataset %in% c("DEX", "DOMINO", "SAE", "RPS")) {
    return("R/dil_social_employment_values.R")
  }
  if (dataset %in% c(
    "CGT", "SMSF", "ATO_MCS", "ATO_CR", "PIT_ITR", "PIT_PS",
    "PIT_IE", "BUSOWN"
  )) {
    return("R/dil_tax_values.R; R/dil_admin_value_rules.R")
  }
  if (dataset %in% c("CORE", "COMBINED")) {
    return("R/dil_core_values.R; R/generate_core.R; R/generate_combined.R")
  }
  if (dataset == "STP") return("R/dil_stp_values.R")
  if (dataset == "ERS") return("R/dil_misc_admin_values.R")
  if (dataset %in% c("BIRTHS", "DEATHS", "MCD", "JK")) {
    return("R/dil_admin_value_rules.R; R/generate_dil_lightweight.R")
  }
  "R/complete_dil_structures.R"
}

.scope_match <- function(exception, occurrence, column) {
  value <- exception[[column]]
  is.na(value) ||
    (!is.na(occurrence[[column]]) && value == occurrence[[column]])
}

.match_exception <- function(exceptions, occurrence) {
  hits <- which(
    exceptions$dataset == occurrence$dataset &
      toupper(exceptions$variable) == toupper(occurrence$variable)
  )
  if (!length(hits)) return(NA_integer_)
  scope_columns <- c("official_product", "official_table", "logical_output")
  keep <- vapply(hits, function(i) {
    all(vapply(scope_columns, function(column) {
      .scope_match(exceptions[i, , drop = FALSE], occurrence, column)
    }, logical(1)))
  }, logical(1))
  hits <- hits[keep]
  if (!length(hits)) return(NA_integer_)
  specificity <- vapply(hits, function(i) {
    sum(!is.na(exceptions[i, scope_columns, drop = TRUE]))
  }, integer(1))
  best <- hits[specificity == max(specificity)]
  if (length(best) > 1L) {
    stop(
      "Ambiguous reviewed exceptions for ", occurrence$dataset, " | ",
      occurrence$official_product, " | ", occurrence$official_table, " | ",
      occurrence$logical_output, " | ", occurrence$variable,
      ". Equally specific exception rows: ", paste(best, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  best[[1L]]
}

.args <- .parse_args(commandArgs(trailingOnly = TRUE))
.gap_path <- file.path(
  .repo_root, "inst", "internal-docs", "admin-value-gap-register.csv"
)
.register_path <- file.path(
  .repo_root, "inst", "internal-docs", "admin-value-remediation-register.csv"
)
.summary_path <- file.path(.args$audit_dir, "variable-summary.csv")

gap <- .read_csv(.gap_path)
register <- .read_csv(.register_path)
runtime <- .read_csv(.summary_path)
exceptions <- .read_csv(.args$exceptions)

.require_columns(
  gap,
  c(
    "asset", "dataset", "official_product", "official_table",
    "logical_output", "variable", "type", "n_rows",
    "official_description", "official_valid_response"
  ),
  .gap_path
)
.require_columns(
  register,
  c(
    "asset", "dataset", "variable", "occurrence_count", "type",
    "official_descriptions", "official_valid_responses", "value_semantics",
    "target_value_rule", "evidence_source_url", "source_coverage"
  ),
  .register_path
)
.require_columns(
  runtime,
  c(
    "asset", "dataset", "official_product", "official_table",
    "logical_output", "variable", "type", "n_rows", "missing",
    "domain_or_distribution", "value_scope", "schema_role"
  ),
  .summary_path
)
.require_columns(
  exceptions,
  c(
    "dataset", "variable", "official_product", "official_table",
    "logical_output", "determination", "exact_rationale",
    "evidence_source", "caveat"
  ),
  .args$exceptions
)

gap_key_columns <- c(
  "asset", "dataset", "official_product", "official_table",
  "logical_output", "variable"
)
.require_complete_text(gap, c(gap_key_columns, "type"), .gap_path)
.require_complete_text(
  register,
  c(
    "asset", "dataset", "variable", "type", "value_semantics",
    "target_value_rule", "source_coverage"
  ),
  .register_path
)
gap$n_rows <- .validated_count(gap$n_rows, "n_rows", .gap_path)
if (any(gap$n_rows <= 0)) {
  stop("Every original gap occurrence must have a positive n_rows.",
       call. = FALSE)
}
register$occurrence_count <- .validated_count(
  register$occurrence_count, "occurrence_count", .register_path
)
if (any(register$occurrence_count <= 0)) {
  stop("Every remediation-register pair must have a positive occurrence_count.",
       call. = FALSE)
}

gap_key <- .occurrence_key(
  gap$asset, gap$dataset, gap$official_product, gap$official_table,
  gap$logical_output, gap$variable
)
if (anyDuplicated(gap_key)) {
  stop("The original gap register contains a duplicate occurrence key.",
       call. = FALSE)
}
register_key <- .pair_key(register$dataset, register$variable)
if (anyDuplicated(register_key)) {
  stop("The remediation register contains a duplicate dataset-variable key.",
       call. = FALSE)
}

text_columns <- setdiff(names(exceptions), character())
exceptions[text_columns] <- lapply(exceptions[text_columns], .normalise_text)
if (any(is.na(exceptions$dataset)) || any(is.na(exceptions$variable)) ||
    any(is.na(exceptions$determination)) ||
    any(is.na(exceptions$exact_rationale)) ||
    any(is.na(exceptions$evidence_source))) {
  stop(
    "Every exception needs a dataset, variable, determination, rationale, and evidence source.",
    call. = FALSE
  )
}
exception_scope_key <- .occurrence_key(
  rep("*", nrow(exceptions)), exceptions$dataset,
  exceptions$official_product, exceptions$official_table,
  exceptions$logical_output, exceptions$variable
)
if (anyDuplicated(exception_scope_key)) {
  stop("The exception determinations contain a duplicate scope key.",
       call. = FALSE)
}

runtime <- runtime[
  !is.na(runtime$value_scope) & runtime$value_scope == "audited" &
    !is.na(runtime$schema_role) &
    runtime$schema_role == "canonical_structure",
  ,
  drop = FALSE
]
if (!nrow(runtime)) {
  stop("The final audit has no audited canonical variable rows.",
       call. = FALSE)
}
.require_complete_text(runtime, c(gap_key_columns, "type"), .summary_path)
runtime$n_rows <- .validated_count(runtime$n_rows, "n_rows", .summary_path)
runtime$missing <- .validated_count(runtime$missing, "missing", .summary_path)
invalid_missing <- runtime$missing > runtime$n_rows
if (any(invalid_missing)) {
  stop(
    "The final audit has missing greater than n_rows at row ",
    which(invalid_missing)[[1L]], ".",
    call. = FALSE
  )
}
runtime_key <- .occurrence_key(
  runtime$asset, runtime$dataset, runtime$official_product,
  runtime$official_table, runtime$logical_output, runtime$variable
)
runtime_groups <- split(seq_len(nrow(runtime)), runtime_key)
runtime_occurrences <- do.call(rbind, lapply(runtime_groups, function(index) {
  part <- runtime[index, , drop = FALSE]
  data.frame(
    asset = part$asset[[1L]],
    dataset = part$dataset[[1L]],
    official_product = part$official_product[[1L]],
    official_table = part$official_table[[1L]],
    logical_output = part$logical_output[[1L]],
    variable = part$variable[[1L]],
    final_type = .collapse_unique(part$type),
    final_n_rows = sum(as.numeric(part$n_rows)),
    final_missing = sum(as.numeric(part$missing)),
    generated_domain_or_distribution = .collapse_unique(
      part$domain_or_distribution
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}))
rownames(runtime_occurrences) <- NULL

final_key <- .occurrence_key(
  runtime_occurrences$asset, runtime_occurrences$dataset,
  runtime_occurrences$official_product, runtime_occurrences$official_table,
  runtime_occurrences$logical_output, runtime_occurrences$variable
)
runtime_hit <- match(gap_key, final_key)
if (anyNA(runtime_hit)) {
  missing <- gap[is.na(runtime_hit), , drop = FALSE]
  stop(
    nrow(missing),
    " original administrative gap occurrence(s) were not observed in the final audit. First: ",
    paste(missing[1L, c("dataset", "logical_output", "variable")],
          collapse = " | "),
    call. = FALSE
  )
}

final <- runtime_occurrences[runtime_hit, , drop = FALSE]
if (any(final$final_n_rows <= 0)) {
  stop("A matched final audit occurrence has zero rows.", call. = FALSE)
}
type_mismatch <- .normalise_text(gap$type) != .normalise_text(final$final_type)
if (any(is.na(type_mismatch) | type_mismatch)) {
  first <- which(is.na(type_mismatch) | type_mismatch)[[1L]]
  stop(
    "A matched occurrence changed type: ", gap$dataset[[first]], " | ",
    gap$logical_output[[first]], " | ", gap$variable[[first]], " (",
    gap$type[[first]], " -> ", final$final_type[[first]], ").",
    call. = FALSE
  )
}
occurrences <- gap
occurrences$final_type <- final$final_type
occurrences$final_n_rows <- final$final_n_rows
occurrences$final_missing <- final$final_missing
occurrences$final_missing_pct <- round(
  100 * occurrences$final_missing / occurrences$final_n_rows,
  6
)
occurrences$generated_domain_or_distribution <-
  final$generated_domain_or_distribution
occurrences$final_status <- ifelse(
  occurrences$final_missing == occurrences$final_n_rows,
  "typed_missing",
  ifelse(
    occurrences$final_missing > 0,
    "populated_with_item_missingness",
    "populated"
  )
)
populated_occurrence <- occurrences$final_status != "typed_missing"
missing_distribution <- populated_occurrence & (
  is.na(occurrences$generated_domain_or_distribution) |
    !nzchar(trimws(occurrences$generated_domain_or_distribution))
)
if (any(missing_distribution)) {
  first <- which(missing_distribution)[[1L]]
  stop(
    "A populated occurrence has no generated domain or distribution: ",
    occurrences$dataset[[first]], " | ",
    occurrences$logical_output[[first]], " | ",
    occurrences$variable[[first]], ".",
    call. = FALSE
  )
}

occurrence_pair_key <- .pair_key(occurrences$dataset, occurrences$variable)
register_hit <- match(occurrence_pair_key, register_key)
if (anyNA(register_hit)) {
  stop("An original occurrence has no remediation-register pair.", call. = FALSE)
}
asset_mismatch <- occurrences$asset != register$asset[register_hit]
if (any(is.na(asset_mismatch) | asset_mismatch)) {
  stop("An original occurrence disagrees with its remediation-register asset.",
       call. = FALSE)
}
occurrences$value_semantics <- register$value_semantics[register_hit]
occurrences$target_value_rule <- register$target_value_rule[register_hit]
occurrences$source_coverage <- register$source_coverage[register_hit]
occurrences$implementation <- vapply(
  occurrences$dataset, .implementation_path, character(1)
)

occurrences$determination <- "populated_synthetic_value"
occurrences$exact_rationale <- paste0(
  occurrences$target_value_rule,
  " Final fixed-seed distribution: ",
  occurrences$generated_domain_or_distribution,
  "."
)
occurrences$evidence_source <- ifelse(
  !is.na(register$evidence_source_url[register_hit]),
  register$evidence_source_url[register_hit],
  paste0("Official PLIDA or BLADE metadata; ", occurrences$implementation)
)
occurrences$caveat <- paste(
  "The values are synthetic.",
  "The audit checks the implemented domain, internal coherence, and placeholder absence;",
  "it does not certify population estimates."
)

has_item_missingness <- occurrences$final_status ==
  "populated_with_item_missingness"
occurrences$determination[has_item_missingness] <-
  "populated_with_structural_or_item_missingness"
occurrences$caveat[has_item_missingness] <- paste(
  occurrences$caveat[has_item_missingness],
  "Some rows are missing because the field is conditional or permits item non-response."
)

typed_missing <- which(occurrences$final_status == "typed_missing")
exception_hit <- rep(NA_integer_, nrow(occurrences))
for (i in typed_missing) {
  exception_hit[[i]] <- .match_exception(
    exceptions, occurrences[i, , drop = FALSE]
  )
}
unclassified <- typed_missing[is.na(exception_hit[typed_missing])]
if (length(unclassified)) {
  missing <- occurrences[unclassified, , drop = FALSE]
  stop(
    length(unclassified),
    " typed-missing occurrence(s) lack a reviewed exception. First: ",
    paste(missing[1L, c("dataset", "logical_output", "variable")],
          collapse = " | "),
    call. = FALSE
  )
}
used_exceptions <- unique(exception_hit[typed_missing])
unused_exceptions <- setdiff(seq_len(nrow(exceptions)), used_exceptions)
if (length(unused_exceptions)) {
  first <- unused_exceptions[[1L]]
  stop(
    length(unused_exceptions),
    " reviewed exception row(s) do not match a final typed-missing occurrence. First: ",
    paste(
      exceptions[first, c(
        "dataset", "official_product", "official_table",
        "logical_output", "variable"
      )],
      collapse = " | "
    ),
    call. = FALSE
  )
}
if (length(typed_missing)) {
  decision <- exceptions[exception_hit[typed_missing], , drop = FALSE]
  occurrences$determination[typed_missing] <- decision$determination
  occurrences$exact_rationale[typed_missing] <- decision$exact_rationale
  occurrences$evidence_source[typed_missing] <- decision$evidence_source
  occurrences$caveat[typed_missing] <- decision$caveat
}

occurrences$allowlist_key <- NA_character_
if (length(typed_missing)) {
  occurrences$allowlist_key[typed_missing] <- paste(
    final$asset[typed_missing],
    final$dataset[typed_missing],
    final$official_product[typed_missing],
    final$official_table[typed_missing],
    final$logical_output[typed_missing],
    final$variable[typed_missing],
    sep = "::"
  )
}

pair_groups <- split(seq_len(nrow(occurrences)), occurrence_pair_key)
pairs <- do.call(rbind, lapply(pair_groups, function(index) {
  part <- occurrences[index, , drop = FALSE]
  status_count <- table(factor(
    part$final_status,
    levels = c(
      "populated", "populated_with_item_missingness", "typed_missing"
    )
  ))
  pair_status <- if (status_count[["typed_missing"]] == 0L) {
    if (status_count[["populated_with_item_missingness"]] > 0L) {
      "populated_with_item_missingness"
    } else {
      "populated"
    }
  } else if (status_count[["typed_missing"]] == nrow(part)) {
    "typed_missing"
  } else {
    "mixed_populated_and_typed_missing"
  }
  data.frame(
    asset = .collapse_unique(part$asset),
    dataset = part$dataset[[1L]],
    variable = part$variable[[1L]],
    original_occurrence_count = nrow(part),
    populated_occurrence_count = unname(status_count[["populated"]]),
    populated_with_item_missingness_count =
      unname(status_count[["populated_with_item_missingness"]]),
    typed_missing_occurrence_count = unname(status_count[["typed_missing"]]),
    final_status = pair_status,
    original_type = .collapse_unique(part$type),
    final_type = .collapse_unique(part$final_type),
    official_descriptions = .collapse_unique(part$official_description),
    official_valid_responses = .collapse_unique(
      part$official_valid_response
    ),
    value_semantics = .collapse_unique(part$value_semantics),
    generated_domain_or_distribution = .collapse_unique(
      part$generated_domain_or_distribution
    ),
    determination = .collapse_unique(part$determination),
    exact_rationale = .collapse_unique(part$exact_rationale),
    evidence_source = .collapse_unique(part$evidence_source),
    implementation = .collapse_unique(part$implementation),
    caveat = .collapse_unique(part$caveat),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}))
pairs <- pairs[order(pairs$dataset, toupper(pairs$variable)), , drop = FALSE]
rownames(pairs) <- NULL

pair_count_hit <- match(.pair_key(pairs$dataset, pairs$variable), register_key)
stopifnot(
  !anyNA(pair_count_hit),
  all(pairs$original_occurrence_count == register$occurrence_count[pair_count_hit])
)

allowlist <- unique(data.frame(
  issue_type = rep("all_missing_admin_variable", length(typed_missing)),
  key = occurrences$allowlist_key[typed_missing],
  reason = occurrences$exact_rationale[typed_missing],
  stringsAsFactors = FALSE,
  check.names = FALSE
))

stopifnot(
  nrow(occurrences) == 16462L,
  nrow(pairs) == 2593L,
  length(unique(pairs$variable)) == 2538L,
  sum(pairs$original_occurrence_count) == 16462L,
  all(!is.na(occurrences$determination) & nzchar(occurrences$determination)),
  all(!is.na(occurrences$exact_rationale) & nzchar(occurrences$exact_rationale)),
  all(!is.na(occurrences$evidence_source) & nzchar(occurrences$evidence_source)),
  all(occurrences$final_status %in% c(
    "populated", "populated_with_item_missingness", "typed_missing"
  )),
  nrow(allowlist) == length(typed_missing)
)

dir.create(.args$output_dir, recursive = TRUE, showWarnings = FALSE)
occurrence_output <- file.path(
  .args$output_dir, "admin-value-occurrence-decisions.csv"
)
pair_output <- file.path(
  .args$output_dir, "admin-value-decision-ledger.csv"
)
allowlist_output <- file.path(
  .args$output_dir, "admin-value-audit-allowlist.csv"
)

utils::write.csv(
  occurrences, occurrence_output, row.names = FALSE, na = "",
  fileEncoding = "UTF-8"
)
utils::write.csv(
  pairs, pair_output, row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
utils::write.csv(
  allowlist, allowlist_output, row.names = FALSE, na = "",
  fileEncoding = "UTF-8"
)

cat("Occurrence decisions: ", nrow(occurrences), "\n", sep = "")
cat("Dataset-variable pairs: ", nrow(pairs), "\n", sep = "")
cat("Distinct variable names: ", length(unique(pairs$variable)), "\n", sep = "")
cat("Final pair status:\n")
print(table(pairs$final_status), quote = FALSE)
cat("Audit allowlist rows: ", nrow(allowlist), "\n", sep = "")
cat("Wrote ", normalizePath(pair_output, winslash = "/"), "\n", sep = "")
