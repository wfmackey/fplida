#!/usr/bin/env Rscript

# Refresh the current ATO health-insurer identifier list from the Australian
# Government PrivateHealth website. This is a current register, not a
# historical financial-year codeframe.

if (!requireNamespace("rvest", quietly = TRUE)) {
  stop("Package 'rvest' is required.", call. = FALSE)
}

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]),
                         mustWork = TRUE))
  }
  normalizePath(
    file.path("data-raw", "update_ato_health_insurer_ids.R"),
    mustWork = TRUE
  )
}

repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."),
  winslash = "/",
  mustWork = TRUE
)
source_url <- "https://www.privatehealth.gov.au/dynamic/Insurer/Index/"

tables <- rvest::html_table(rvest::read_html(source_url), fill = TRUE)
matches <- which(vapply(
  tables,
  function(x) all(c("Insurer", "ATO ID", "Type") %in% names(x)),
  logical(1)
))
if (length(matches) != 1L) {
  stop("Expected one insurer table at the source URL.", call. = FALSE)
}

source <- as.data.frame(tables[[matches]], stringsAsFactors = FALSE)
insurers <- data.frame(
  code = trimws(as.character(source[["ATO ID"]])),
  insurer = trimws(as.character(source[["Insurer"]])),
  membership_type = trimws(as.character(source[["Type"]])),
  source_url = source_url,
  source_retrieved_date = as.character(Sys.Date()),
  scope_note = paste0(
    "Current Australian Government insurer register; not a verified ",
    "historical financial-year snapshot."
  ),
  stringsAsFactors = FALSE
)
insurers <- insurers[order(insurers$code), , drop = FALSE]
rownames(insurers) <- NULL

stopifnot(
  nrow(insurers) >= 30L,
  all(grepl("^[A-Z]{3}$", insurers$code)),
  !anyDuplicated(insurers$code),
  all(nzchar(insurers$insurer)),
  all(insurers$membership_type %in% c("Open", "Restricted"))
)

output <- file.path(
  repo_root, "inst", "extdata", "codeframes", "ato-health-insurer-ids.tsv"
)
utils::write.table(
  insurers,
  output,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = ""
)
cat("Wrote ", normalizePath(output, winslash = "/"), "\n", sep = "")
cat("Rows: ", nrow(insurers), "\n", sep = "")
