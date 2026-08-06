#!/usr/bin/env Rscript
# Audit the written descriptions in the shipped registry.
#
# Every check here is a defect an earlier round actually produced, so each one
# earned its place. Run it after rebuilding the registry, and before claiming a
# dataset is done:
#
#   Rscript data-raw/audit_variable_descriptions.R            # everything
#   Rscript data-raw/audit_variable_descriptions.R MBS,DOMINO # one slice
#
# Exits non-zero when a check fails, so it can gate a rebuild.

.script_path <- function() {
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[startsWith(file_arg, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
  }
  normalizePath(file.path("data-raw", "audit_variable_descriptions.R"),
                mustWork = TRUE)
}
repo <- normalizePath(file.path(dirname(.script_path()), ".."),
                      winslash = "/", mustWork = TRUE)
args <- commandArgs(trailingOnly = TRUE)
only <- if (length(args)) strsplit(args[[1]], ",")[[1]] else NULL

info <- read.csv(gzfile(file.path(repo, "inst", "variable-info.csv.gz")),
                 stringsAsFactors = FALSE)
admin <- info[info$collection_type == "administrative", ]
if (!is.null(only)) {
  admin <- admin[toupper(admin$dataset) %in% toupper(only), , drop = FALSE]
  cat("scoped to:", paste(only, collapse = ", "), "\n")
}
# A pair counts as described when the text was written rather than lifted from
# the data item list. Comparing against the official description instead makes
# every variable with a blank official description look curated.
described <- admin[admin$description_provenance %in% c("official", "ai"), ]
described <- described[!duplicated(paste(described$dataset,
                                         toupper(described$variable))), ]

fail <- 0L
check <- function(label, bad, show = 4L) {
  n <- sum(bad)
  cat(sprintf("%-58s %s\n", label, if (n) paste("FAIL", n) else "ok"))
  if (n) {
    fail <<- fail + 1L
    rows <- described[bad, , drop = FALSE]
    for (i in seq_len(min(show, nrow(rows)))) {
      cat("    ", rows$dataset[i], rows$variable[i], "\n")
    }
  }
  invisible(NULL)
}

cat("described pairs:", nrow(described), "\n\n")

check("definition never repeats the limitation",
      described$value_definition == described$limitation)

check("description is not the official description reworded",
      tolower(gsub("[^a-z]", "", tolower(described$variable_description))) ==
        tolower(gsub("[^a-z]", "", tolower(described$official_description))))

check("description says more than one sentence",
      nchar(described$variable_description) < 80L)

first <- sub("[.](?=\\s|$).*$", ".", described$variable_description,
             perl = TRUE)
# The table cell shows this sentence alone. Beyond about 320 characters it is
# a paragraph pretending to be a summary; below 10 it is a fragment.
check("first sentence is a summary, not a fragment or a paragraph",
      nchar(first) < 10L | nchar(first) > 320L)
cat(sprintf("%-58s %d over 200 chars (median %d)\n",
            "  (soft) first sentences are short enough to scan",
            sum(nchar(first) > 200L), stats::median(nchar(first))))

check("description does not open by naming the variable",
      mapply(function(d, v) {
        startsWith(tolower(d), tolower(v))
      }, described$variable_description, described$variable))

check("no boilerplate left in the definition",
      grepl("does not publish a finite value list", described$value_definition,
            fixed = TRUE))

check("value domain is named",
      described$value_domain %in% c("", "not specified"))

check("a sourced description cites a fetchable-looking URL",
      described$value_support_status == "sourced" &
        !grepl("^https?://", described$value_source_url))

check("an official tag carries a quotation",
      described$description_provenance == "official" &
        !grepl('"', described$variable_description, fixed = TRUE))

# One description reused across variables is the failure mode that makes a
# whole dataset worthless, so it is counted rather than sampled.
by_desc <- table(described$variable_description)
reused <- names(by_desc)[by_desc > 1L]
cat(sprintf("%-58s %s\n", "no description is reused across variables",
            if (length(reused)) paste("FAIL", length(reused)) else "ok"))
if (length(reused)) {
  fail <- fail + 1L
  for (d in utils::head(reused, 3)) {
    who <- described[described$variable_description == d, ]
    cat("    ", paste(unique(paste(who$dataset, who$variable)),
                      collapse = ", "), "\n")
  }
}

cat("\nprovenance of described pairs:\n")
print(table(described$description_provenance))

cat("\nby dataset:\n")
print(sort(table(described$dataset), decreasing = TRUE))

cat("\n", if (fail) paste("AUDIT FAILED:", fail, "checks") else "AUDIT PASSED",
    "\n", sep = "")
quit(status = if (fail) 1L else 0L)
