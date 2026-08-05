# data-raw/pbs_item_lookup.R
#
# Build a compact PBS item reference table for the fplida package.
# Source: PBS Public Production Data API CSV files (2026-01-01 schedule).
#   Downloaded from https://data.pbs.gov.au/
#
# Output: inst/extdata/pbs_item_lookup.csv
#
# Point FPLIDA_PBS_CSV_DIR at the unpacked `tables_as_csv` directory before
# running this.
#
# Following the data-raw convention from R Packages 2e, Ch 7:
# https://r-pkgs.org/data.html

# -- Source path ---------------------------------------------------------------
src_dir <- Sys.getenv("FPLIDA_PBS_CSV_DIR", unset = NA_character_)

if (is.na(src_dir) || !nzchar(src_dir)) {
  stop("Set FPLIDA_PBS_CSV_DIR to the unpacked PBS API `tables_as_csv` ",
       "directory. Download the CSV files from https://data.pbs.gov.au/ first.",
       call. = FALSE)
}
if (!dir.exists(src_dir)) {
  stop("PBS API CSV files not found at:\n  ", src_dir, call. = FALSE)
}

# -- Read source tables --------------------------------------------------------
items <- read.csv(file.path(src_dir, "ITEMS.csv"),
                  stringsAsFactors = FALSE, na.strings = c("", "null"))
cat("Items source rows:", nrow(items), "\n")

atc_rels <- read.csv(file.path(src_dir, "ITEM-ATC-RELATIONSHIPS.csv"),
                     stringsAsFactors = FALSE, na.strings = c("", "null"))
cat("ATC relationships:", nrow(atc_rels), "\n")

# -- Filter to General Schedule, current items ---------------------------------
# Keep only GE (General Schedule) — this is the main PBS programme
items <- items[!is.na(items$program_code) & items$program_code == "GE", ]
cat("After filtering to GE programme:", nrow(items), "\n")

# Keep current items (no non_effective_date)
items <- items[is.na(items$non_effective_date), ]
cat("After filtering ended items:", nrow(items), "\n")

# Exclude supply-only items (not subsidised)
items <- items[is.na(items$supply_only_indicator) |
               items$supply_only_indicator != "Y", ]
cat("After excluding supply-only:", nrow(items), "\n")

# -- Deduplicate by pbs_code --------------------------------------------------
# Multiple li_item_ids per pbs_code (different brands/packs).
# Keep the first row per pbs_code (sorted by originator_brand first).
items <- items[order(items$pbs_code,
                     ifelse(!is.na(items$originator_brand_indicator) &
                            items$originator_brand_indicator == "Y", 0L, 1L),
                     items$li_item_id), ]
items <- items[!duplicated(items$pbs_code), ]
cat("After deduplication by pbs_code:", nrow(items), "\n")

# -- Join ATC codes ------------------------------------------------------------
# Keep primary ATC mapping (atc_priority_pct == 100 or highest)
atc_rels <- atc_rels[order(atc_rels$pbs_code,
                           -as.numeric(atc_rels$atc_priority_pct)), ]
atc_rels <- atc_rels[!duplicated(atc_rels$pbs_code), ]
items <- merge(items, atc_rels[, c("pbs_code", "atc_code")],
               by = "pbs_code", all.x = TRUE)

# Extract ATC level 1 (first character)
items$atc_level1 <- substr(items$atc_code, 1L, 1L)

# -- Clean price fields --------------------------------------------------------
items$claimed_price <- as.numeric(items$claimed_price)

# Drop items with missing/zero price
items <- items[!is.na(items$claimed_price) & items$claimed_price > 0, ]
cat("After dropping zero/missing prices:", nrow(items), "\n")

# -- Select columns for lookup -------------------------------------------------
lookup <- data.frame(
  pbs_code         = as.character(items$pbs_code),
  atc_code         = as.character(items$atc_code),
  atc_level1       = as.character(items$atc_level1),
  benefit_type     = as.character(items$benefit_type_code),
  claimed_price    = round(items$claimed_price, 2),
  pack_size        = as.integer(items$pack_size),
  number_of_repeats = as.integer(items$number_of_repeats),
  manner_of_admin  = as.character(items$manner_of_administration),
  stringsAsFactors = FALSE
)

# -- Add sampling weight column ------------------------------------------------
# Approximate real PBS utilisation shares by ATC level 1.
# Source: AIHW Pharmaceutical Benefits Statistics 2022-23, Table 1.
atc_weights <- c(
  "A" = 0.13,   # Alimentary Tract & Metabolism (PPIs, diabetes)
  "B" = 0.05,   # Blood & Blood Forming Organs (anticoagulants)
  "C" = 0.21,   # Cardiovascular (statins, ACE inhibitors)
  "D" = 0.02,   # Dermatologicals
  "G" = 0.04,   # Genito-Urinary (contraceptives, BPH)
  "H" = 0.03,   # Systemic Hormonal (thyroid, corticosteroids)
  "J" = 0.07,   # Anti-infectives (antibiotics)
  "L" = 0.04,   # Antineoplastic & Immunomodulating
  "M" = 0.05,   # Musculo-Skeletal (NSAIDs, gout)
  "N" = 0.20,   # Nervous System (antidepressants, opioids)
  "P" = 0.01,   # Antiparasitic
  "R" = 0.08,   # Respiratory (asthma, COPD)
  "S" = 0.03,   # Sensory Organs (eye drops)
  "V" = 0.01    # Various
)

# Within each ATC level 1 group, distribute weight equally across items
lookup$weight <- 0
for (atc1 in names(atc_weights)) {
  mask <- !is.na(lookup$atc_level1) & lookup$atc_level1 == atc1
  n_items <- sum(mask)
  if (n_items > 0L) {
    lookup$weight[mask] <- atc_weights[[atc1]] / n_items
  }
}

# Handle items with no ATC level 1 — give them minimal weight
no_atc <- is.na(lookup$atc_level1)
if (any(no_atc)) {
  lookup$weight[no_atc] <- 0.01 / sum(no_atc)
}

# Normalise weights to sum to 1
lookup$weight <- lookup$weight / sum(lookup$weight)

# -- Sort by ATC level 1, then pbs_code ---------------------------------------
lookup <- lookup[order(lookup$atc_level1, lookup$pbs_code), ]
rownames(lookup) <- NULL

cat("Final lookup rows:", nrow(lookup), "\n")
cat("ATC level 1 distribution:\n")
print(table(lookup$atc_level1, useNA = "ifany"))
cat("\nBenefit type distribution:\n")
print(table(lookup$benefit_type))

# -- Write output --------------------------------------------------------------
tryCatch({
  out_path <- file.path(
    rprojroot::find_package_root_file(),
    "inst", "data", "pbs_item_lookup.csv"
  )
}, error = function(e) {
  out_path <<- file.path("inst", "data", "pbs_item_lookup.csv")
})

if (!dir.exists(dirname(out_path))) {
  dir.create(dirname(out_path), recursive = TRUE)
}

write.csv(lookup, out_path, row.names = FALSE)
cat("Wrote", nrow(lookup), "rows to", out_path, "\n")
