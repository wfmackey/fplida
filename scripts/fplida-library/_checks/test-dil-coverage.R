#!/usr/bin/env Rscript
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
library_script_root <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
source(file.path(library_script_root, "00-setup.R"))

registry <- dil_asset_agencies()
structures <- fplida:::.dil_structure_inventory()$structures
stems <- paste0(structures[["Product Name"]], "--", structures[["Table Name"]])
expected <- vapply(structures$Dataset, function(dataset) {
  tolower(fplida:::dataset_to_agency(dataset))
}, "")
stopifnot(identical(unname(registry[stems]), unname(expected)))
for (dataset in c("ATO_MCS", "SMSF", "APSED", "DEX", "STP")) {
  index <- which(structures$Dataset == dataset)[1L]
  stopifnot(identical(asset_agency(stems[index], "unclassified", registry), unname(expected[index])))
}
stopifnot(identical(asset_agency("native-stp-example", "stp", registry), "ato"),
          is.na(asset_agency("unknown-product", "unclassified", registry)))
all_assets <- data.frame(asset = stems)
stopifnot(dil_structure_coverage(all_assets)$passed)
missing <- dil_structure_coverage(all_assets[-1L, , drop = FALSE])
stopifnot(!missing$passed, identical(missing$missing, stems[1L]))
# Product-level presence alone must not hide an absent canonical table.
stopifnot(!dil_structure_coverage(data.frame(asset = structures[["Product Name"]]))$passed)
conflict <- structures[1:2, , drop = FALSE]
conflict$Dataset <- c("PIT_ITR", "MBS")
conflict[["Product Name"]] <- "shared-product"
conflict[["Table Name"]] <- c("one", "two")
error <- tryCatch(dil_asset_agencies(conflict), error = identity)
stopifnot(inherits(error, "error"), grepl("Ambiguous", conditionMessage(error)))
cat(sprintf("DIL agency mapping and coverage: PASS (%d structures, %d agencies).\n",
            length(stems), length(unique(registry))))
