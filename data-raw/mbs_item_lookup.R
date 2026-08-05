# data-raw/mbs_item_lookup.R
#
# Build a compact MBS item reference table for the fplida package.
# Source: a CSV extract of the official MBS XML, published at
#   https://www.mbsonline.gov.au/ (the downloads carry a version and date,
#   e.g. MBS-XML-20250701 Version 3).
#
# Output: inst/extdata/mbs_item_lookup.csv
#
# Point FPLIDA_MBS_ITEMS_CSV at your extracted CSV before running this.
#
# Following the data-raw convention from R Packages 2e, Ch 7:
# https://r-pkgs.org/data.html

# -- Source path ---------------------------------------------------------------
src_path <- Sys.getenv("FPLIDA_MBS_ITEMS_CSV", unset = NA_character_)

if (is.na(src_path) || !nzchar(src_path)) {
  stop("Set FPLIDA_MBS_ITEMS_CSV to the path of the MBS items CSV extract.",
       call. = FALSE)
}
if (!file.exists(src_path)) {
  stop("MBS items source not found at:\n  ", src_path, call. = FALSE)
}

items <- read.csv(src_path, stringsAsFactors = FALSE)
cat("Source rows:", nrow(items), "\n")

# -- Filter to current items (no end date) ------------------------------------
items <- items[is.na(items$item_end_date) | items$item_end_date == "", ]
cat("After filtering ended items:", nrow(items), "\n")

# -- Keep only standard fee items (fee_type == "N") ---------------------------
# Derived fee items (fee_type == "D") have complex formulas; exclude them
# as they can't be sampled with a simple schedule_fee lookup.
items <- items[!is.na(items$fee_type) & items$fee_type == "N", ]
cat("After filtering to standard fee items:", nrow(items), "\n")

# -- Drop items with missing schedule_fee -------------------------------------
items <- items[!is.na(items$schedule_fee) & items$schedule_fee > 0, ]
cat("After dropping zero/missing fees:", nrow(items), "\n")

# -- Select columns -----------------------------------------------------------
lookup <- data.frame(
  item_num      = as.integer(items$item_num),
  category      = as.integer(items$category),
  group         = as.character(items$group),
  sub_heading   = as.integer(items$sub_heading),
  benefit_type  = as.character(items$benefit_type),
  schedule_fee  = round(as.numeric(items$schedule_fee), 2),
  benefit_75    = round(as.numeric(ifelse(is.na(items$benefit_75), 0,
                                          items$benefit_75)), 2),
  benefit_85    = round(as.numeric(ifelse(is.na(items$benefit_85), 0,
                                          items$benefit_85)), 2),
  benefit_100   = round(as.numeric(ifelse(is.na(items$benefit_100), 0,
                                          items$benefit_100)), 2),
  anaes         = ifelse(!is.na(items$anaes) & items$anaes == "Y", "Y", "N"),
  stringsAsFactors = FALSE
)

# -- Add sampling weight column ------------------------------------------------
# Approximate real MBS utilisation shares by category.
# Source: AIHW Medicare Statistics, Annual Report 2022-23.
#
# These are *claim-level* shares (not item-level), so high-volume items
# (GP attendances, pathology) dominate.
cat_weights <- c(
  "1"  = 0.38,   # Professional Attendances (GP, specialist)
  "2"  = 0.03,   # Diagnostic Procedures
  "3"  = 0.08,   # Therapeutic Procedures
  "4"  = 0.02,   # Oral/Dental
  "5"  = 0.08,   # Diagnostic Imaging
  "6"  = 0.30,   # Pathology
  "7"  = 0.05,   # Allied Health
  "8"  = 0.02,   # ATSI Health
  "9"  = 0.01,   # Unattached Miscellaneous
  "10" = 0.03    # Dentistry
)

# Within each category, distribute weight equally across items
lookup$weight <- 0
for (cat_str in names(cat_weights)) {
  cat_int <- as.integer(cat_str)
  mask <- lookup$category == cat_int
  n_items <- sum(mask)
  if (n_items > 0L) {
    lookup$weight[mask] <- cat_weights[[cat_str]] / n_items
  }
}

# Normalise weights to sum to 1
lookup$weight <- lookup$weight / sum(lookup$weight)

# -- Sort by category, group, item_num ----------------------------------------
lookup <- lookup[order(lookup$category, lookup$group, lookup$item_num), ]
rownames(lookup) <- NULL

cat("Final lookup rows:", nrow(lookup), "\n")
cat("Category distribution:\n")
print(table(lookup$category))

# -- Write output --------------------------------------------------------------
out_path <- file.path(
  dirname(dirname(normalizePath(".", mustWork = TRUE))),
  "inst", "data", "mbs_item_lookup.csv"
)
# Handle running from project root vs data-raw/
out_path <- file.path(
  rprojroot::find_package_root_file(),
  "inst", "data", "mbs_item_lookup.csv"
)

# Fallback if rprojroot not available
tryCatch({
  out_path <- file.path(
    rprojroot::find_package_root_file(),
    "inst", "data", "mbs_item_lookup.csv"
  )
}, error = function(e) {
  # Assume we're running from the package root
  out_path <<- file.path("inst", "data", "mbs_item_lookup.csv")
})

if (!dir.exists(dirname(out_path))) {
  dir.create(dirname(out_path), recursive = TRUE)
}

write.csv(lookup, out_path, row.names = FALSE)
cat("Wrote", nrow(lookup), "rows to", out_path, "\n")
