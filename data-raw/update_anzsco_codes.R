# Vendor the valid 6-digit ANZSCO 2019 occupation codes.
#
# Census OCCP is validated against this list. It used to be read from the
# `strayr` package at run time, which made Census output depend on whether an
# optional package happened to be installed: without it the validity check was
# skipped, the per-person RNG stream shifted, and almost every Census row
# changed. Vendoring the list makes the behaviour unconditional.
#
# Source: strayr::anzsco2019, which packages the ABS classification
#   ANZSCO — Australian and New Zealand Standard Classification of
#   Occupations, 2019 (cat. 1220.0).
#   https://www.abs.gov.au/statistics/classification/anzsco-australian-and-new-zealand-standard-classification-occupations/latest-release
#
# Regenerate with:
#   Rscript data-raw/update_anzsco_codes.R
# `strayr` is needed only to run this script, not to use the package:
#   remotes::install_github("runapp-aus/strayr")

if (!requireNamespace("strayr", quietly = TRUE)) {
  stop("Package 'strayr' is required to regenerate this file.\n",
       "  remotes::install_github(\"runapp-aus/strayr\")", call. = FALSE)
}

env <- new.env(parent = emptyenv())
utils::data("anzsco2019", package = "strayr", envir = env)

codes <- env$anzsco2019$anzsco_occupation_code
codes <- codes[grepl("^[0-9]{6}$", codes)]
codes <- sort(unique(as.integer(codes)))

stopifnot(
  "expected roughly 1,200 six-digit ANZSCO codes" =
    length(codes) > 1000L && length(codes) < 1500L,
  "codes must all be six digits" =
    all(codes >= 100000L & codes <= 999999L)
)

out <- file.path("inst", "extdata", "codeframes", "anzsco2019-occupation-codes.txt")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

writeLines(
  c(
    "# Valid 6-digit ANZSCO 2019 occupation codes, one per line.",
    "# Source: ABS ANZSCO 2019 (cat. 1220.0), via strayr::anzsco2019.",
    "# https://www.abs.gov.au/statistics/classification/anzsco-australian-and-new-zealand-standard-classification-occupations/latest-release",
    "# Regenerate with data-raw/update_anzsco_codes.R",
    format(codes)
  ),
  out
)

message("Wrote ", length(codes), " codes to ", out)
