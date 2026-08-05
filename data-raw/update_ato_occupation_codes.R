# Vendor the ATO salary and wage occupation codes, and build an ANZSCO crosswalk.
#
# The ATO field on an individual income tax return is NOT ANZSCO. Taxpayers pick
# a six-digit code from the ATO's own published list at question 1. The two
# classifications overlap heavily but not completely: of the 1,167 ATO codes for
# 2025-26, 951 are also valid ANZSCO 2019 codes, and 216 are not.
#
# fplida gives each person an ANZSCO occupation on the shared spine. To emit a
# real ATO code for that person we map ANZSCO -> ATO down the code hierarchy:
#   exact 6-digit match, else 4-digit unit group, else 3-digit minor group,
#   else 1-digit major group, else 999000 ("Occupation not listed").
# Every ANZSCO 2019 code resolves at or above the major-group level, so 999000
# is never actually needed — it is kept as a defensive last resort.
#
# Source: ATO Salary and Wage Occupation Codes, data.gov.au.
#   https://www.data.gov.au/data/dataset/ato-salary-and-wage-occupation-codes
#   Licence: Creative Commons Attribution 2.5 Australia
#   https://creativecommons.org/licenses/by/2.5/au/
#
# Regenerate with:
#   Rscript data-raw/update_ato_occupation_codes.R

for (pkg in c("readxl")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required to regenerate this file.", call. = FALSE)
  }
}

URL <- paste0(
  "https://data.gov.au/data/dataset/0063be5d-2e80-477e-94cb-e865e6749b4b/",
  "resource/78426e3f-87d5-4ea4-ae66-2ce12cbe0324/download/",
  "ato-salary-and-wage-occupation-codes.xlsx"
)

tmp <- tempfile(fileext = ".xlsx")
utils::download.file(URL, tmp, mode = "wb", quiet = TRUE)

sheets <- readxl::excel_sheets(tmp)
latest <- sheets[length(sheets)]
message("Using ATO sheet: ", latest, " (of ", length(sheets), " income years)")

raw <- readxl::read_excel(tmp, sheet = latest)
names(raw) <- c("code", "description")
raw$code <- suppressWarnings(as.integer(raw$code))
raw <- raw[!is.na(raw$code) & nzchar(trimws(raw$description)), , drop = FALSE]

# One representative description per code: the shortest, which is reliably the
# plain occupation name rather than a "Manager - regional, retail" style synonym.
raw <- raw[order(raw$code, nchar(raw$description), raw$description), , drop = FALSE]
codes <- raw[!duplicated(raw$code), , drop = FALSE]

stopifnot(
  "expected roughly 1,200 ATO occupation codes" =
    nrow(codes) > 800L && nrow(codes) < 2000L,
  "codes must be six digits" =
    all(codes$code >= 100000L & codes$code <= 999999L)
)

out_codes <- file.path("inst", "extdata", "codeframes", "ato-occupation-codes.tsv")
dir.create(dirname(out_codes), recursive = TRUE, showWarnings = FALSE)
utils::write.table(
  data.frame(ato_code = codes$code, ato_description = codes$description,
             stringsAsFactors = FALSE),
  out_codes, sep = "\t", row.names = FALSE, quote = TRUE, na = ""
)
message("Wrote ", nrow(codes), " ATO codes to ", out_codes)

# ---- ANZSCO -> ATO crosswalk ----------------------------------------------

anzsco_path <- file.path("inst", "extdata", "codeframes",
                         "anzsco2019-occupation-codes.txt")
anzsco <- readLines(anzsco_path, warn = FALSE)
anzsco <- as.integer(anzsco[!startsWith(anzsco, "#") & nzchar(trimws(anzsco))])

ato <- sort(unique(codes$code))
NOT_LISTED <- 999000L

# Deterministic: at each fallback level take the lowest matching ATO code.
pick <- function(candidates) if (length(candidates)) min(candidates) else NA_integer_

map_one <- function(a) {
  if (a %in% ato) return(c(a, "exact"))
  for (lvl in list(c(100L, "unit"), c(1000L, "minor"), c(100000L, "major"))) {
    div <- as.integer(lvl[[1]])
    hit <- pick(ato[ato %/% div == a %/% div])
    if (!is.na(hit)) return(c(hit, lvl[[2]]))
  }
  c(NOT_LISTED, "not_listed")
}

mapped <- vapply(anzsco, map_one, character(2))
crosswalk <- data.frame(
  anzsco_code = anzsco,
  ato_code    = as.integer(mapped[1, ]),
  match_level = mapped[2, ],
  stringsAsFactors = FALSE
)

stopifnot(
  "every ANZSCO code must map to a valid ATO code" =
    all(crosswalk$ato_code %in% c(ato, NOT_LISTED)),
  "crosswalk must cover every ANZSCO code" =
    nrow(crosswalk) == length(anzsco)
)

out_xwalk <- file.path("inst", "extdata", "codeframes",
                       "anzsco-to-ato-occupation.tsv")
utils::write.table(crosswalk, out_xwalk, sep = "\t", row.names = FALSE,
                   quote = FALSE, na = "")
message("Wrote ", nrow(crosswalk), " crosswalk rows to ", out_xwalk)
print(table(crosswalk$match_level))
