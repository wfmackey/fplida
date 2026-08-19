# Project a table from the registry's own variable list.
#
# Several datasets deliver a handful of their published products and leave the
# rest absent: NDIS ships participants, payments and plan supports but not
# carers, providers or outcomes; DEX ships three of its fifteen tables. A
# consumer who reads the data item list, finds `ndis_carerdemo`, and goes
# looking for it finds nothing, and cannot tell an unimplemented product from
# an empty one.
#
# This builds a table from the variable list the registry gives it, valuing
# each column through the same dataset value rules the canonical DIL path
# uses. It is the honest floor: every published column exists, carries a value
# of the right kind, and agrees with the bespoke product where they share a
# person.

#' Variables the registry gives a table
#'
#' @param dataset Character. Dataset acronym.
#' @param table Character. Table name from the data item list.
#' @return Character vector of variable names.
#' @keywords internal
.registry_table_variables <- function(dataset, table) {
  variables <- utils::read.csv(
    .dil_metadata_path("variables.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  variables <- variables[variables$Dataset == dataset &
                           variables[["Table Name"]] == table, , drop = FALSE]
  unique(variables[["Variable Name"]])
}


#' Tables the registry gives a product
#'
#' @param dataset Character. Dataset acronym.
#' @param product Character. Product name from the data item list.
#' @return Character vector of table names.
#' @keywords internal
.registry_product_tables <- function(dataset, product) {
  variables <- utils::read.csv(
    .dil_metadata_path("variables.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  variables <- variables[variables$Dataset == dataset &
                           variables[["Product Name"]] == product, ,
                         drop = FALSE]
  unique(variables[["Table Name"]])
}


#' Build a registry table over a set of people
#'
#' @param dataset Character. Dataset acronym.
#' @param product Character. Product name.
#' @param table Character. Table name.
#' @param spine_rows data.frame. The people the table describes.
#' @param aeuid Character vector. Their identifier in this dataset.
#' @param seed Integer. Random seed.
#' @param period List with `start_year` and `end_year`.
#' @param source_frame data.frame or NULL. A bespoke product for the same
#'   people, so shared columns agree with it rather than being drawn again.
#' @return A data.frame with the registry's variables for that table.
#' @keywords internal
.project_registry_table <- function(dataset, product, table, spine_rows,
                                    aeuid, seed, period,
                                    source_frame = NULL) {
  variables <- .registry_table_variables(dataset, table)
  if (!length(variables)) return(NULL)
  n <- nrow(spine_rows)
  if (!n) return(NULL)
  if (is.null(source_frame)) source_frame <- spine_rows

  out <- vector("list", length(variables))
  names(out) <- variables
  for (name in variables) {
    value <- NULL
    if (toupper(name) %in% c("SYNTHETIC_AEUID")) {
      value <- as.character(aeuid)
    } else if (name %in% names(source_frame)) {
      # A column the bespoke product already answers for keeps its answer, so
      # the two products describe the same person the same way.
      value <- source_frame[[name]]
    } else {
      value <- tryCatch(
        .dil_dataset_source_value(name, "", dataset, source_frame, spine_rows,
                                  seed, period, product, table, ""),
        error = function(e) NULL
      )
    }
    if (is.null(value)) {
      value <- tryCatch(
        .dil_value_for(name, spine_rows, as.character(aeuid), dataset,
                       product, seed),
        error = function(e) NULL
      )
    }
    if (is.null(value)) value <- rep(NA_character_, n)
    if (length(value) == 1L) value <- rep(value, n)
    if (length(value) != n) value <- rep_len(value, n)
    out[[name]] <- value
  }
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}


#' Write every registry table of a product that is not already there
#'
#' @param ds_dir Character. Dataset directory.
#' @param dataset Character. Dataset acronym.
#' @param product Character. Product name.
#' @param spine_rows data.frame. The people the product describes.
#' @param aeuid Character vector. Their identifier in this dataset.
#' @param seed Integer. Random seed.
#' @param period List with `start_year` and `end_year`.
#' @param source_frame data.frame or NULL. A bespoke product for the same
#'   people.
#' @param one_file Logical. Write one file for the product rather than one
#'   per table. Used where the delivered convention is a single file.
#' @param max_tables Integer. Cap on tables written, for products the
#'   registry splits into dozens of period-specific tables.
#' @return Invisibly, the number of files written.
#' @keywords internal
.write_registry_product <- function(ds_dir, dataset, product, spine_rows,
                                    aeuid, seed, period,
                                    source_frame = NULL, one_file = FALSE,
                                    max_tables = Inf) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(invisible(0L))
  tables <- .registry_product_tables(dataset, product)
  if (!length(tables)) return(invisible(0L))
  tables <- utils::head(sort(tables), max_tables)

  if (one_file) {
    # The delivered convention for this product is one file, so the tables
    # are unioned on their shared columns.
    frames <- lapply(tables, function(table) {
      .project_registry_table(dataset, product, table, spine_rows, aeuid,
                              seed, period, source_frame)
    })
    frames <- Filter(Negate(is.null), frames)
    if (!length(frames)) return(invisible(0L))
    shared <- Reduce(intersect, lapply(frames, names))
    frame <- do.call(rbind, lapply(frames, function(x) x[, shared,
                                                         drop = FALSE]))
    arrow::write_parquet(frame,
                         file.path(ds_dir, paste0(product, ".parquet")))
    return(invisible(1L))
  }

  written <- 0L
  for (table in tables) {
    frame <- .project_registry_table(dataset, product, table, spine_rows,
                                     aeuid, seed, period, source_frame)
    if (is.null(frame)) next
    arrow::write_parquet(
      frame, file.path(ds_dir, sprintf("%s-%s.parquet", product, table)))
    written <- written + 1L
  }
  invisible(written)
}


# DEX reference and lookup tables are catalogues, not per-client records.
# These are the number of distinct entries each holds, in the order of the
# real collection: a few hundred funded organisations, more outlets than
# organisations, and short code lists for the reference tables.
.DEX_CATALOGUE_ROWS <- list(
  special_organisation = 60L,
  special_outlet = 140L,
  special_program = 25L,
  special_service_type = 40L,
  special_ref_assistance_needed = 20L,
  special_ref_calendar = 48L,
  special_ref_referral_to = 18L,
  special_ttl_priority_group = 12L
)


#' Complete a dataset against its own data item list
#'
#' Writes every table the registry gives the dataset that is not already
#' there, and tops up the ones that are with the columns they are missing.
#' The bespoke generator keeps every value it already produces; this only
#' fills what it left out.
#'
#' @param ds_dir Character. Dataset directory.
#' @param dataset Character. Dataset acronym.
#' @param spine data.frame. Spine rows.
#' @param aeuid_column Character. The spine column holding this dataset's
#'   person identifier.
#' @param seed Integer. Random seed.
#' @param period List with `start_year` and `end_year`.
#' @param sample_rate Numeric. Share of the spine the dataset covers, for
#'   tables it has to build a population for.
#' @return Invisibly, the number of files written or topped up.
#' @keywords internal
.complete_dataset_products <- function(ds_dir, dataset, spine, aeuid_column,
                                       seed, period, sample_rate = 1) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(invisible(0L))
  if (!dir.exists(ds_dir)) return(invisible(0L))
  if (!aeuid_column %in% names(spine)) return(invisible(0L))

  variables <- utils::read.csv(
    .dil_metadata_path("variables.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  variables <- variables[variables$Dataset == dataset, , drop = FALSE]
  structures <- unique(variables[, c("Product Name", "Table Name")])
  structures <- structures[nzchar(structures[["Product Name"]]) &
                             nzchar(structures[["Table Name"]]), ,
                           drop = FALSE]
  if (!nrow(structures)) return(invisible(0L))

  # The people the dataset covers. A dataset that already wrote something
  # takes its population from there so the new tables describe the same
  # people; otherwise it takes a share of the spine.
  existing <- list.files(ds_dir, pattern = "\\.parquet$", full.names = TRUE)
  existing <- existing[!grepl("-spine\\.parquet$", existing)]
  covered <- NULL
  for (path in existing) {
    frame <- tryCatch(as.data.frame(read_parquet_safely(path),
                                    stringsAsFactors = FALSE),
                      error = function(e) NULL)
    if (!is.null(frame) && "SYNTHETIC_AEUID" %in% names(frame) &&
        nrow(frame)) {
      covered <- unique(as.character(frame$SYNTHETIC_AEUID))
      break
    }
  }
  if (is.null(covered) || !length(covered)) {
    keep <- .select_lightweight_rows(spine, dataset, seed, sample_rate,
                                     nrow(spine))
    covered <- unique(as.character(spine[[aeuid_column]][keep]))
  }
  index <- match(covered, as.character(spine[[aeuid_column]]))
  people <- spine[index[!is.na(index)], , drop = FALSE]
  covered <- covered[!is.na(index)]
  if (!nrow(people)) return(invisible(0L))

  touched <- 0L
  for (i in seq_len(nrow(structures))) {
    product <- structures[["Product Name"]][i]
    table <- structures[["Table Name"]][i]
    expected <- .registry_table_variables(dataset, table)
    if (!length(expected)) next

    path <- file.path(ds_dir, sprintf("%s--%s.parquet", product, table))
    plain <- file.path(ds_dir, sprintf("%s.parquet", product))
    if (!file.exists(path) && file.exists(plain)) path <- plain

    if (file.exists(path)) {
      frame <- tryCatch(as.data.frame(read_parquet_safely(path),
                                      stringsAsFactors = FALSE),
                        error = function(e) NULL)
      if (is.null(frame) || !nrow(frame)) next
      missing <- setdiff(expected, names(frame))
      if (!length(missing)) next
      rows <- if ("SYNTHETIC_AEUID" %in% names(frame)) {
        spine[match(as.character(frame$SYNTHETIC_AEUID),
                    as.character(spine[[aeuid_column]])), , drop = FALSE]
      } else {
        people[rep_len(seq_len(nrow(people)), nrow(frame)), , drop = FALSE]
      }
      ids <- if ("SYNTHETIC_AEUID" %in% names(frame)) {
        as.character(frame$SYNTHETIC_AEUID)
      } else {
        rep_len(covered, nrow(frame))
      }
      filled <- .project_registry_table(dataset, product, table, rows, ids,
                                        seed, period, source_frame = frame)
      if (is.null(filled)) next
      for (name in missing) frame[[name]] <- filled[[name]]
      arrow::write_parquet(frame, path)
      touched <- touched + 1L
      next
    }

    frame <- .project_registry_table(dataset, product, table, people, covered,
                                     seed, period)
    if (is.null(frame)) next
    arrow::write_parquet(frame, path)
    touched <- touched + 1L
  }
  invisible(touched)
}
