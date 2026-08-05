# Build the compact ASGS 2021 Mesh Block lookup used by generated location
# tables. Source: ABS ASGS Edition 3 allocation file MB_2021_AUST.xlsx.

url <- paste0(
  "https://www.abs.gov.au/statistics/standards/",
  "australian-statistical-geography-standard-asgs/",
  "edition-3-july-2021-june-2026/access-and-downloads/",
  "allocation-files/MB_2021_AUST.xlsx"
)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required to parse the ABS workbook.",
       call. = FALSE)
}
if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required to write the lookup.",
       call. = FALSE)
}

tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb", quiet = FALSE)

raw <- readxl::read_excel(tmp, sheet = "MB_2021_AUST",
                          col_types = "text")

wanted <- c(
  "MB_CODE_2021",
  "SA1_CODE_2021",
  "SA2_CODE_2021",
  "SA4_CODE_2021",
  "STATE_CODE_2021"
)
missing <- setdiff(wanted, names(raw))
if (length(missing)) {
  stop("ABS workbook missing expected columns: ",
       paste(missing, collapse = ", "), call. = FALSE)
}

lookup <- data.table::as.data.table(raw[, wanted])
data.table::setnames(
  lookup,
  c("mb_code", "sa1_code", "sa2_code", "sa4_code", "state")
)
lookup[, state := suppressWarnings(as.integer(state))]
lookup <- lookup[!is.na(state) & state %in% 1:8]

out <- file.path("inst", "extdata", "mb_lookup.csv.gz")
data.table::fwrite(lookup, out)

message("Wrote ", out, " with ", nrow(lookup), " Mesh Block rows.")
