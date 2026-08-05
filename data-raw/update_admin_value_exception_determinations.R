# Build the reviewed determinations for administrative variables that remain
# entirely missing after the final value implementation.

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]),
                         mustWork = TRUE))
  }
  normalizePath(
    file.path("data-raw", "update_admin_value_exception_determinations.R"),
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

.rows <- list()
.add <- function(x) {
  columns <- c(
    "dataset", "variable", "official_product", "official_table",
    "logical_output", "determination", "exact_rationale",
    "evidence_source", "caveat"
  )
  missing <- setdiff(columns, names(x))
  for (name in missing) x[[name]] <- NA_character_
  x <- x[columns]
  x[] <- lapply(x, .normalise_text)
  .rows[[length(.rows) + 1L]] <<- x
  invisible(x)
}

.make <- function(dataset, variable, determination, exact_rationale,
                  evidence_source, caveat = NA_character_,
                  official_product = NA_character_,
                  official_table = NA_character_,
                  logical_output = NA_character_) {
  data.frame(
    dataset = dataset,
    variable = variable,
    official_product = official_product,
    official_table = official_table,
    logical_output = logical_output,
    determination = determination,
    exact_rationale = exact_rationale,
    evidence_source = evidence_source,
    caveat = caveat,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.docs_dir <- file.path(.repo_root, "inst", "internal-docs")
.gap_path <- file.path(.docs_dir, "admin-value-gap-register.csv")
gap <- .read_csv(.gap_path)
.require_columns(
  gap,
  c(
    "asset", "dataset", "official_product", "official_table",
    "logical_output", "variable"
  ),
  .gap_path
)

acld_path <- file.path(.docs_dir, "acld-admin-value-decisions.csv")
acld <- .read_csv(acld_path)
.require_columns(
  acld,
  c(
    "dataset", "variable", "remaining_unresolved_gap_occurrence_count",
    "determination", "exact_rationale", "evidence_url"
  ),
  acld_path
)
acld <- acld[acld$remaining_unresolved_gap_occurrence_count > 0L, , drop = FALSE]
.acld_mapping_path <- file.path(
  .repo_root, "inst", "extdata", "codeframes",
  "acld-variable-codeframes.csv"
)
acld_mapping <- .read_csv(.acld_mapping_path)
.require_columns(
  acld_mapping, c("product", "variable", "supported"), .acld_mapping_path
)
acld_gap <- gap[
  gap$dataset == "ACLD" &
    toupper(gap$variable) %in% toupper(acld$variable),
  ,
  drop = FALSE
]
acld_mapping_key <- paste(
  acld_mapping$product, toupper(acld_mapping$variable), sep = "\r"
)
acld_gap_key <- paste(
  acld_gap$official_product, toupper(acld_gap$variable), sep = "\r"
)
acld_mapping_hit <- match(acld_gap_key, acld_mapping_key)
if (anyNA(acld_mapping_hit)) {
  stop("An unresolved ACLD occurrence has no codeframe mapping.",
       call. = FALSE)
}
acld_gap <- acld_gap[!acld_mapping$supported[acld_mapping_hit], , drop = FALSE]
acld_decision_hit <- match(
  toupper(acld_gap$variable), toupper(acld$variable)
)
observed_acld_count <- table(factor(
  toupper(acld_gap$variable), levels = toupper(acld$variable)
))
if (!identical(
  as.integer(observed_acld_count),
  as.integer(acld$remaining_unresolved_gap_occurrence_count)
)) {
  stop(
    "The ACLD occurrence scopes do not match the unresolved counts in ",
    acld_path, ".",
    call. = FALSE
  )
}
.add(data.frame(
  dataset = acld_gap$dataset,
  variable = acld_gap$variable,
  official_product = acld_gap$official_product,
  official_table = acld_gap$official_table,
  logical_output = acld_gap$logical_output,
  determination = paste0(
    "typed_missing_", acld$determination[acld_decision_hit]
  ),
  exact_rationale = acld$exact_rationale[acld_decision_hit],
  evidence_source = acld$evidence_url[acld_decision_hit],
  caveat = paste(
    "The required historical geography or SEIFA crosswalk is not available",
    "in a checked public source. No code is invented."
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
))

ndis_path <- file.path(.docs_dir, "ndis-admin-structural-determinations.csv")
ndis <- .read_csv(ndis_path)
.require_columns(
  ndis,
  c(
    "variable", "determination", "exact_rationale", "evidence_url"
  ),
  ndis_path
)
.add(data.frame(
  dataset = "NDIS",
  variable = ndis$variable,
  determination = ndis$determination,
  exact_rationale = ndis$exact_rationale,
  evidence_source = ndis$evidence_url,
  caveat = paste(
    "The field stays typed missing so synthetic output does not imply source",
    "coverage or a codeframe that the public documentation does not support."
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
))

social_path <- file.path(
  .docs_dir, "admin-value-decision-ledger-social-employment.csv"
)
social <- .read_csv(social_path)
.require_columns(
  social,
  c("dataset", "variable", "status", "evidence_source", "caveat"),
  social_path
)
social <- social[social$status == "unsupported-codeframe", , drop = FALSE]
.add(data.frame(
  dataset = social$dataset,
  variable = social$variable,
  determination = "typed_missing_unpublished_internal_codeframe",
  exact_rationale = social$caveat,
  evidence_source = social$evidence_source,
  caveat = paste(
    "An exact source column is retained when present. The fallback remains",
    "typed missing instead of using a locally invented domain."
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
))

tax_path <- file.path(.docs_dir, "tax-admin-value-decisions.csv")
tax <- .read_csv(tax_path)
.require_columns(
  tax,
  c(
    "dataset", "variable", "status", "implementation_class",
    "determination_rule", "evidence_source", "evidence_url", "caveat"
  ),
  tax_path
)
tax <- tax[
  tax$status == "unsupported_private_codeframe" |
    tax$implementation_class == "typed_structural_missing",
  ,
  drop = FALSE
]
.add(data.frame(
  dataset = tax$dataset,
  variable = tax$variable,
  determination = ifelse(
    tax$implementation_class == "typed_structural_missing",
    "typed_missing_structurally_inapplicable",
    "typed_missing_private_codeframe"
  ),
  exact_rationale = tax$determination_rule,
  evidence_source = ifelse(
    is.na(tax$evidence_url),
    tax$evidence_source,
    paste(tax$evidence_source, tax$evidence_url, sep = "; ")
  ),
  caveat = tax$caveat,
  stringsAsFactors = FALSE,
  check.names = FALSE
))

census_path <- file.path(
  .docs_dir, "admin-value-decision-ledger-census.csv"
)
census <- .read_csv(census_path)
.require_columns(
  census,
  c(
    "dataset", "variable", "status", "determination", "rule",
    "evidence_source", "caveat"
  ),
  census_path
)
census <- census[census$status == "typed_missing", , drop = FALSE]
.add(data.frame(
  dataset = census$dataset,
  variable = census$variable,
  determination = census$determination,
  exact_rationale = census$rule,
  evidence_source = census$evidence_source,
  caveat = census$caveat,
  stringsAsFactors = FALSE,
  check.names = FALSE
))

aedc_path <- file.path(.docs_dir, "admin-value-decision-ledger-aedc.csv")
aedc <- .read_csv(aedc_path)
.require_columns(
  aedc,
  c(
    "dataset", "variable", "status", "determination", "rule",
    "evidence_source", "caveat"
  ),
  aedc_path
)
aedc_typed_missing <- grepl(
  "^typed[-_]missing", aedc$determination, ignore.case = TRUE, perl = TRUE
)
if (any(aedc_typed_missing & aedc$status != "unsupported-codeframe")) {
  stop(
    "An AEDC typed-missing determination has an incompatible ledger status.",
    call. = FALSE
  )
}
aedc <- aedc[aedc_typed_missing, , drop = FALSE]
.add(data.frame(
  dataset = aedc$dataset,
  variable = aedc$variable,
  determination = aedc$determination,
  exact_rationale = aedc$rule,
  evidence_source = aedc$evidence_source,
  caveat = aedc$caveat,
  stringsAsFactors = FALSE,
  check.names = FALSE
))

air_variables <- c(
  "CCNTRPRV", "CNTRNDCTN_TSP", "COBJNPRV", "CONTRNUM", "ENDREAS",
  "EPSREAS", "LCTN_TYP_CD", "PRVDR_PRCTC_TYP_GRP", "PRVTYPE", "RECTYP",
  "SCHOOLID", "VACCINE"
)
.add(.make(
  "AIR",
  air_variables,
  "typed_missing_unpublished_air_codeframe",
  paste(
    "The public PLIDA data item list names the field but does not publish the",
    "AIR provider, practice, event, record, school, or vaccine-product",
    "codeframe required to create a valid value."
  ),
  paste(
    "PLIDA data item list; R/generate_air.R; src/rust/src/air.rs;",
    "tests/testthat/test-generate-air-canonical-values.R"
  ),
  "Antigen and event fields are populated separately. No private AIR code is inferred."
))

.add(.make(
  "NACDC",
  "SERVICE_TYPE_CODE",
  "typed_missing_unpublished_service_type_codeframe",
  paste(
    "The source-backed NACDC service descriptors do not establish the exact",
    "internal SERVICE_TYPE_CODE values used by these PLIDA tables."
  ),
  paste(
    "PLIDA data item list; R/generate_nacdc.R; src/rust/src/nacdc.rs;",
    "tests/testthat/test-generate-nacdc-canonical-values.R"
  ),
  "Service descriptions remain usable; the unpublished internal code remains missing."
))

.add(.make(
  "TRAVELLERS",
  "VISA_STREAM_CODE",
  "typed_missing_unpublished_visa_stream_codeframe",
  paste(
    "The PLIDA data item list publishes 0000 only as the fallback when a valid",
    "visa stream is unavailable. It does not publish the stream codeframe",
    "needed to translate the generated visa subclasses."
  ),
  "PLIDA data item list; R/complete_dil_structures.R; src/rust/src/travellers.rs",
  "The field remains typed missing instead of assigning the 0000 fallback to every traveller."
))

vet_path <- file.path(.repo_root, "R", "dil_vet_apprentice_values.R")
vet_text <- paste(readLines(vet_path, warn = FALSE), collapse = "\n")
.extract_vector <- function(name) {
  pattern <- paste0("(?s)", name, "\\s*<-\\s*c\\((.*?)\\)\n")
  hit <- regmatches(vet_text, regexec(pattern, vet_text, perl = TRUE))[[1L]]
  if (!length(hit)) stop("Could not extract ", name, " from ", vet_path,
                         call. = FALSE)
  value <- strsplit(hit[[2L]], ",", fixed = TRUE)[[1L]]
  gsub("[\\\"'[:space:]]", "", value, perl = TRUE)
}
tva_variables <- .extract_vector("\\.dil_tva_unsupported_variables")
at_variables <- .extract_vector("\\.dil_at_unsupported_variables")
.add(.make(
  "TVA",
  tva_variables,
  "typed_missing_no_exact_avetmiss_mapping",
  paste(
    "The public AVETMISS definitions do not establish an exact mapping for",
    "this derived PLIDA identifier or status field."
  ),
  paste(
    "NCVER AVETMISS Data element definitions 2.2 and 2.3;",
    "R/dil_vet_apprentice_values.R"
  ),
  "The field remains typed missing instead of receiving a plausible but unverified code."
))
.add(.make(
  "A&T",
  at_variables,
  "typed_missing_no_exact_apprentice_codeframe",
  paste(
    "The public training and AVETMISS sources do not establish the internal",
    "apprentice-system codeframe or reference-table value for this field."
  ),
  paste(
    "NCVER AVETMISS Data element definitions; training.gov.au;",
    "R/dil_vet_apprentice_values.R"
  ),
  "The field remains typed missing instead of receiving a locally invented code."
))

.core_ctpp <- gap[
  gap$dataset == "CORE" & toupper(gap$variable) == "CTPP" &
    grepl("^partner_", gap$official_table),
  ,
  drop = FALSE
]
if (nrow(.core_ctpp) != 6L) {
  stop("Expected six structurally inapplicable CORE CTPP occurrences.",
       call. = FALSE)
}
.add(.make(
  .core_ctpp$dataset,
  .core_ctpp$variable,
  "typed_missing_structurally_inapplicable",
  paste(
    "CTPP is a child-type field. It is populated for parent-child records and",
    "is structurally inapplicable in the six partner-only canonical tables."
  ),
  "PLIDA data item list; R/dil_core_values.R; tests/testthat/test-dil-core-values.R",
  "Only the partner-table occurrences are allowed to remain missing.",
  official_product = .core_ctpp$official_product,
  official_table = .core_ctpp$official_table,
  logical_output = .core_ctpp$logical_output
))

exceptions <- do.call(rbind, .rows)
exceptions <- exceptions[order(
  exceptions$dataset,
  toupper(exceptions$variable),
  exceptions$official_product,
  exceptions$official_table,
  exceptions$logical_output,
  na.last = TRUE
), , drop = FALSE]
rownames(exceptions) <- NULL

required <- c(
  "dataset", "variable", "determination", "exact_rationale",
  "evidence_source"
)
for (name in required) {
  if (any(is.na(exceptions[[name]]) | !nzchar(exceptions[[name]]))) {
    stop("A required exception field is empty: ", name, call. = FALSE)
  }
}

scope_key <- paste(
  exceptions$dataset,
  toupper(exceptions$variable),
  ifelse(is.na(exceptions$official_product), "", exceptions$official_product),
  ifelse(is.na(exceptions$official_table), "", exceptions$official_table),
  ifelse(is.na(exceptions$logical_output), "", exceptions$logical_output),
  sep = "\r"
)
if (anyDuplicated(scope_key)) {
  stop("The exception determinations contain a duplicate scope key.",
       call. = FALSE)
}

register_path <- file.path(.docs_dir, "admin-value-remediation-register.csv")
register <- .read_csv(register_path)
.require_columns(register, c("asset", "dataset", "variable"), register_path)
if (any(is.na(register$asset)) || any(is.na(register$dataset)) ||
    any(is.na(register$variable))) {
  stop("The remediation register has an incomplete pair key.", call. = FALSE)
}
register_key <- paste(register$dataset, toupper(register$variable), sep = "\r")
if (anyDuplicated(register_key)) {
  stop("The remediation register has a duplicate dataset-variable key.",
       call. = FALSE)
}
exception_key <- paste(exceptions$dataset, toupper(exceptions$variable), sep = "\r")
unknown <- setdiff(exception_key, register_key)
if (length(unknown)) {
  stop("An exception is outside the original remediation register: ",
       unknown[[1L]], call. = FALSE)
}

scope_columns <- c("official_product", "official_table", "logical_output")
for (i in seq_len(nrow(exceptions))) {
  candidate <- gap$dataset == exceptions$dataset[[i]] &
    toupper(gap$variable) == toupper(exceptions$variable[[i]])
  for (column in scope_columns) {
    if (!is.na(exceptions[[column]][[i]])) {
      candidate <- candidate &
        !is.na(gap[[column]]) &
        gap[[column]] == exceptions[[column]][[i]]
    }
  }
  if (!any(candidate)) {
    stop(
      "Exception row ", i,
      " does not match an original administrative gap occurrence.",
      call. = FALSE
    )
  }
  pair_hit <- match(exception_key[[i]], register_key)
  if (!all(gap$asset[candidate] == register$asset[[pair_hit]])) {
    stop("An exception scope disagrees with the remediation-register asset.",
         call. = FALSE)
  }
}

output_path <- file.path(
  .docs_dir, "admin-value-exception-determinations.csv"
)
utils::write.csv(
  exceptions,
  output_path,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

cat("Exception determinations: ", nrow(exceptions), "\n", sep = "")
cat("Datasets:\n")
print(table(exceptions$dataset), quote = FALSE)
cat("Wrote ", normalizePath(output_path, winslash = "/"), "\n", sep = "")
