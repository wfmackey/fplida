# Build the resolved value domains for administrative variables that earlier
# review left entirely missing.
#
# WHY THIS EXISTS
#
# `admin-value-exception-determinations.csv` records an earlier review of every
# administrative variable that generated no values. That review had two
# outcomes available to it: an exact published codeframe, or typed missing. Its
# rationales say so plainly — "no exact public codeframe", "no defensible
# crosswalk", "would conceal an unresolved mapping".
#
# `guessed` did not exist then. It does now, and it changes the question. A
# field whose exact mapping is unpublished can still have a defensible shape,
# and saying so honestly beats an empty column. Re-researching the determined
# variables against that lower bar resolved most of them, and turned up a
# number of genuinely published codeframes the first pass simply missed —
# the DSS Aristotle metadata registry for DOMINO, the ABS geo service for the
# 2011 and 2016 ASGS vintages, and the ABS trade appendices the BLADE Data Item
# List had been pointing at all along.
#
# This script reads the research findings in `data-raw/value-research/`, checks
# them, and writes the flat table that `update_variable_info.R` applies. The
# determinations file is left untouched: it remains the record of what was
# decided under the old bar, and this file records what supersedes it.

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
  }
  normalizePath(
    file.path("data-raw", "update_resolved_value_domains.R"),
    mustWork = TRUE
  )
}

.repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."),
  winslash = "/",
  mustWork = TRUE
)

.research_dir <- file.path(.repo_root, "data-raw", "value-research")
.docs_dir <- file.path(.repo_root, "inst", "internal-docs")
.codeframe_dir <- file.path(.repo_root, "inst", "extdata", "codeframes")

# Above this, a code list stops being documentation and starts being data. The
# largest list the registry already carries is SA2 at 2,473 values, a 78 KB
# JSON blob; four megabytes of mesh block codes on every occurrence is a
# different thing entirely. Oversized domains are recorded by reference: the
# domain, the source and the published size, with generation left to the
# structural rules that already know an 11-digit mesh block when they see one.
.enumeration_cap <- 2500L

.normalise_text <- function(x) {
  x <- enc2utf8(as.character(x))
  present <- !is.na(x)
  x[present] <- trimws(gsub("[[:space:]]+", " ", x[present], perl = TRUE))
  x[present & !nzchar(x)] <- NA_character_
  x
}

.chr <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  value <- as.character(x)[[1L]]
  if (is.na(value) || !nzchar(trimws(value))) return(NA_character_)
  trimws(value)
}

.int <- function(x) {
  if (is.null(x) || !length(x)) return(NA_integer_)
  suppressWarnings(as.integer(x)[[1L]])
}

files <- list.files(.research_dir, pattern = "\\.json$", full.names = TRUE)
if (!length(files)) {
  stop("No research findings found in ", .research_dir, call. = FALSE)
}

rows <- list()
for (path in files) {
  findings <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  for (f in findings) {
    values <- if (is.null(f$values)) {
      character(0)
    } else {
      as.character(unlist(f$values, use.names = FALSE))
    }
    values <- values[!is.na(values) & nzchar(trimws(values))]
    rows[[length(rows) + 1L]] <- data.frame(
      dataset = .chr(f$dataset),
      variable = .chr(f$variable),
      status = .chr(f$status),
      value_domain = .chr(f$value_domain),
      values = jsonlite::toJSON(values, auto_unbox = FALSE),
      n_values = length(values),
      values_are_sample = isTRUE(f$values_are_sample),
      full_list_size = .int(f$full_list_size),
      value_source = .chr(f$value_source),
      value_source_url = .chr(f$value_source_url),
      confidence = .chr(f$confidence),
      rationale = .chr(f$rationale),
      evidence_quote = .chr(f$evidence_quote),
      supersedes_rationale = .chr(f$supersedes_rationale),
      research_file = basename(path),
      stringsAsFactors = FALSE
    )
  }
}
resolved <- do.call(rbind, rows)

# ---- checks ---------------------------------------------------------------

allowed <- c("sourced", "guessed", "unsupported")
if (!all(resolved$status %in% allowed)) {
  stop(
    "Unexpected research status: ",
    paste(unique(setdiff(resolved$status, allowed)), collapse = ", "),
    call. = FALSE
  )
}

key <- paste(resolved$dataset, toupper(resolved$variable))
if (anyDuplicated(key)) {
  stop(
    "Duplicate dataset/variable in the research findings: ",
    paste(unique(key[duplicated(key)]), collapse = ", "),
    call. = FALSE
  )
}

# A `sourced` claim without a fetchable URL is a guess wearing a source's
# clothes. That is the one failure mode worth a hard stop: it launders an
# inference into a fact, which is exactly what `guessed` exists to prevent.
bad_source <- resolved$status == "sourced" &
  (is.na(resolved$value_source_url) |
     !grepl("^https?://", resolved$value_source_url) |
     is.na(resolved$value_source))
if (any(bad_source)) {
  stop(
    "A sourced resolution carries no usable source URL: ",
    paste(key[bad_source], collapse = ", "),
    call. = FALSE
  )
}

# A resolution with no value list is not a failure: plenty of domains are open
# rather than enumerable. A contraindication timestamp, a Medicare provider
# number and a sequence counter all have a well-defined shape and no finite
# list, and saying "this is a timestamp" is still worth far more than typed
# missing. What such a resolution must have is a named domain.
open_domain <- resolved$status %in% c("sourced", "guessed") &
  resolved$n_values == 0
if (any(open_domain & is.na(resolved$value_domain))) {
  stop(
    "A resolved variable carries neither values nor a value domain: ",
    paste(key[open_domain & is.na(resolved$value_domain)], collapse = ", "),
    call. = FALSE
  )
}

# `value_source` is user-facing. It names who published the classification, not
# where this package keeps its copy: a reader wants to know the ABS defines
# remoteness areas, not which file in `extdata` holds the transcription. The
# registry build rejects implementation paths in public text anyway, so catch
# it here where the message can say which finding is at fault.
implementation_path <- grepl(
  "(^|[ ;(])(R/|src/|tests/|inst/|data-raw/)",
  resolved$value_source,
  perl = TRUE
)
if (any(implementation_path)) {
  stop(
    "A value source cites a path inside the package rather than its ",
    "publisher: ", paste(key[implementation_path], collapse = ", "),
    call. = FALSE
  )
}

# `unsupported` must stay clean: no source, no values. It is the honest
# outcome, and it should look like one.
untidy <- resolved$status == "unsupported" &
  (resolved$n_values > 0 | !is.na(resolved$value_source_url))
if (any(untidy)) {
  stop(
    "An unsupported resolution carries values or a source: ",
    paste(key[untidy], collapse = ", "),
    call. = FALSE
  )
}

# ---- oversized domains ----------------------------------------------------

# A sample is not a domain, and size is not what makes it one. A researcher who
# listed eight New South Wales electorates out of several hundred nationally has
# not given us the domain; generating from those eight would put every child in
# NSW. So a list is carried only when it is complete: either it is not flagged
# as a sample, or the count matches the published total.
sampled <- resolved$values_are_sample |
  (!is.na(resolved$full_list_size) &
     resolved$full_list_size > resolved$n_values)
oversized <- sampled | (!is.na(resolved$full_list_size) &
                          resolved$full_list_size > .enumeration_cap)
resolved$enumerated <- !oversized & resolved$status != "unsupported"

# Drop the partial list rather than let generation draw from an arbitrary two
# dozen codes: the structural rules produce better-distributed values than that,
# and the domain and its published size are still recorded.
resolved$values[oversized] <- jsonlite::toJSON(character(0), auto_unbox = FALSE)
resolved$n_values[oversized] <- 0L

resolved <- resolved[order(resolved$dataset, resolved$variable), ]
row.names(resolved) <- NULL

character_columns <- setdiff(names(resolved), c(
  "n_values", "full_list_size", "values_are_sample", "enumerated", "values"
))
resolved[character_columns] <- lapply(resolved[character_columns], .normalise_text)

output_path <- file.path(.docs_dir, "resolved-value-domains.csv")
utils::write.csv(
  resolved, output_path, row.names = FALSE, na = "", fileEncoding = "UTF-8"
)

cat("Resolved value domains: ", nrow(resolved), "\n", sep = "")
cat("By status:\n")
print(table(resolved$status), quote = FALSE)
cat("Enumerated in the registry: ", sum(resolved$enumerated), "\n", sep = "")
cat("Documented by reference (over ", .enumeration_cap, " values): ",
    sum(oversized), "\n", sep = "")
cat("By dataset:\n")
print(table(resolved$dataset), quote = FALSE)
cat("Wrote ", normalizePath(output_path, winslash = "/"), "\n", sep = "")
