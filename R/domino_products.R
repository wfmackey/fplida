# DOMINO subtable coverage.
#
# The income support record is published as 35 products across 64 tables. The
# bespoke generator writes nine of them: the base record, indigenous status,
# the principal carer beta table and the five payment-history summaries. The
# rest are the income supplement families (`inc-*`), the entitlement history
# families (`een-*`) and the per-payment history tables (`pyh-*`).
#
# A pipeline that follows a payment from the base record into its history
# finds those tables absent rather than empty, which is the failure that
# looks like a broken path rather than a missing product.

#' Products DOMINO already writes
#'
#' @param ds_dir Character. DOMINO dataset directory.
#' @return Character vector of product names.
#' @keywords internal
.domino_written_products <- function(ds_dir) {
  files <- list.files(ds_dir, pattern = "\\.parquet$")
  files <- files[grepl("--", files, fixed = TRUE)]
  unique(sub("--.*$", "", files))
}


#' Write the DOMINO products the bespoke generator does not
#'
#' @param ds_dir Character. DOMINO dataset directory.
#' @param spine data.frame. Spine rows.
#' @param seed Integer. Random seed.
#' @return Invisibly, the number of files written.
#' @keywords internal
.domino_write_missing_products <- function(ds_dir, spine, seed) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(invisible(0L))

  # The people DOMINO already describes, so a subtable covers the same
  # recipients rather than the whole population.
  base_path <- file.path(ds_dir,
                         "madipge-dom-monthly-d-base--static-demogs.parquet")
  if (!file.exists(base_path)) return(invisible(0L))
  base <- as.data.frame(read_parquet_safely(base_path),
                        stringsAsFactors = FALSE)
  if (!nrow(base)) return(invisible(0L))

  index <- match(base$SYNTHETIC_AEUID, as.character(spine$aeuid_dss))
  recipients <- spine[index, , drop = FALSE]
  period <- list(start_year = 2006L, end_year = 2025L)

  variables <- utils::read.csv(
    .dil_metadata_path("variables.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  variables <- variables[variables$Dataset == "DOMINO", , drop = FALSE]
  structures <- unique(variables[, c("Product Name", "Table Name")])
  structures <- structures[nzchar(structures[["Product Name"]]) &
                             nzchar(structures[["Table Name"]]), ,
                           drop = FALSE]

  written_products <- .domino_written_products(ds_dir)
  written <- 0L
  for (i in seq_len(nrow(structures))) {
    product <- structures[["Product Name"]][i]
    table <- structures[["Table Name"]][i]
    if (product %in% written_products) next

    frame <- .project_registry_table("DOMINO", product, table, recipients,
                                     base$SYNTHETIC_AEUID, seed, period,
                                     source_frame = base)
    if (is.null(frame)) next
    arrow::write_parquet(
      frame,
      file.path(ds_dir, sprintf("%s--%s.parquet", product,
                                gsub("_", "-", table))))
    written <- written + 1L
  }
  invisible(written)
}
