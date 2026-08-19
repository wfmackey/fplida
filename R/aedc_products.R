# AEDC sibling products.
#
# The census is published as five products a cycle: the core record, the
# domain instrument, and three siblings -- indigenous, language and special
# needs -- each with its own short variable list. The generator wrote the core
# table and copied it to the four sibling paths, so `aedc_2021_lan`, which the
# data item list gives fourteen variables, arrived with the core record's two
# hundred, and a consumer reading it got the whole census under a name that
# says it is the language instrument.
#
# Each sibling is now projected to its own variable list, over the same
# children the core table holds, through the same AEDC value rules the
# canonical DIL path uses. The core and domain products keep their Rust
# writer.

.AEDC_SIBLING_FAMILIES <- c("indigenous", "language", "specialneeds")

# The table each product family maps to, given a cycle. The data item list
# names them by an abbreviation of the family.
.AEDC_FAMILY_TABLE_SUFFIX <- c(
  indigenous = "ind", language = "lan", specialneeds = "spec"
)


#' Variables the registry gives an AEDC sibling table
#'
#' @param family Character. "indigenous", "language" or "specialneeds".
#' @param cycle Integer. Cycle year.
#' @return Character vector of variable names, in registry order.
#' @keywords internal
.aedc_table_variables <- function(family, cycle) {
  table_name <- sprintf("aedc_%d_%s", cycle,
                        .AEDC_FAMILY_TABLE_SUFFIX[[family]])
  variables <- utils::read.csv(
    .dil_metadata_path("variables.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  variables <- variables[variables$Dataset == "AEDC" &
                           variables[["Table Name"]] == table_name, ,
                         drop = FALSE]
  unique(variables[["Variable Name"]])
}


#' Build one AEDC sibling product for one cycle
#'
#' @param core data.frame. The cycle's core AEDC table.
#' @param spine_rows data.frame. Spine rows for the same children.
#' @param family Character. Sibling family.
#' @param cycle Integer. Cycle year.
#' @param seed Integer. Random seed.
#' @return A data.frame with the registry's variables for that table.
#' @keywords internal
.aedc_sibling_frame <- function(core, spine_rows, family, cycle, seed) {
  variables <- .aedc_table_variables(family, cycle)
  if (!length(variables)) return(NULL)

  product_name <- sprintf("madipge-aedc-d-%s-%d", family, cycle)
  table_name <- sprintf("aedc_%d_%s", cycle,
                        .AEDC_FAMILY_TABLE_SUFFIX[[family]])
  period <- list(start_year = cycle, end_year = cycle)

  out <- vector("list", length(variables))
  names(out) <- variables
  for (name in variables) {
    value <- if (name %in% names(core)) {
      # The core record already answers for the identifiers and the year, and
      # a sibling that disagreed with it would not be the same child.
      core[[name]]
    } else {
      .dil_aedc_source_value(name, "", core, spine_rows, seed, period,
                             product_name, table_name)
    }
    if (is.null(value)) value <- rep(NA_character_, nrow(core))
    if (length(value) == 1L) value <- rep(value, nrow(core))
    out[[name]] <- value
  }
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}


#' An empty AEDC sibling with the right columns
#'
#' @param family Character. Sibling family.
#' @param cycle Integer. Cycle year.
#' @return A zero-row data.frame, or NULL when the registry has no such table.
#' @keywords internal
.aedc_empty_sibling <- function(family, cycle) {
  variables <- .aedc_table_variables(family, cycle)
  if (!length(variables)) return(NULL)
  frame <- as.data.frame(
    stats::setNames(replicate(length(variables), character(0),
                              simplify = FALSE), variables),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  frame
}

#' Write the AEDC sibling products for a cycle
#'
#' @param ds_dir Character. AEDC dataset directory.
#' @param cycle Integer. Cycle year.
#' @param spine data.frame. Spine rows.
#' @param seed Integer. Random seed.
#' @return Invisibly, the number of products written.
#' @keywords internal
.aedc_write_siblings <- function(ds_dir, cycle, spine, seed) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(invisible(0L))
  core_path <- file.path(ds_dir,
                         sprintf("madipge-aedc-d-core-%d.parquet", cycle))
  if (!file.exists(core_path)) return(invisible(0L))
  core <- as.data.frame(read_parquet_safely(core_path),
                        stringsAsFactors = FALSE)

  # The children in the cycle, in the order the core table holds them.
  index <- match(core$SYNTHETIC_AEUID, as.character(spine$aeuid_de))
  spine_rows <- spine[index, , drop = FALSE]

  written <- 0L
  for (family in .AEDC_SIBLING_FAMILIES) {
    frame <- if (nrow(core)) {
      .aedc_sibling_frame(core, spine_rows, family, cycle, seed)
    } else {
      # A cycle with no children still has its products, empty. Dropping the
      # file instead would make an unimplemented product and an empty one
      # look the same.
      .aedc_empty_sibling(family, cycle)
    }
    if (is.null(frame)) next
    arrow::write_parquet(
      frame,
      file.path(ds_dir, sprintf("madipge-aedc-d-%s-%d.parquet", family, cycle)))
    written <- written + 1L
  }
  invisible(written)
}


# The AEDC cut scores. A child is developmentally vulnerable when their
# domain score falls below the 10th percentile of the 2009 national baseline,
# and developmentally at risk between the 10th and 25th. The cuts are fixed at
# the 2009 distribution and applied unchanged to every later cycle, which is
# what lets vulnerability rise or fall between them: recomputing the
# percentile each cycle would hold every cycle at 10% by construction and make
# the headline measure impossible to move.
.AEDC_VULNERABLE_PERCENTILE <- 0.10
.AEDC_AT_RISK_PERCENTILE <- 0.25

# The four domains and the baseline cycle their cuts come from.
.AEDC_DOMAINS <- c("PHYS", "SOC", "EMOT", "LANGCOG")
.AEDC_BASELINE_CYCLE <- 2009L


#' Domain cut scores from the baseline cycle
#'
#' @param ds_dir Character. AEDC dataset directory.
#' @return A named list of two-element numeric vectors, or NULL when the
#'   baseline cycle is absent.
#' @keywords internal
.aedc_cut_scores <- function(ds_dir) {
  path <- file.path(ds_dir, sprintf("madipge-aedc-d-core-%d.parquet",
                                    .AEDC_BASELINE_CYCLE))
  if (!file.exists(path)) return(NULL)
  baseline <- as.data.frame(read_parquet_safely(path),
                            stringsAsFactors = FALSE)
  if (!nrow(baseline)) return(NULL)

  cuts <- list()
  for (domain in .AEDC_DOMAINS) {
    scores <- suppressWarnings(as.numeric(baseline[[domain]]))
    scores <- scores[is.finite(scores)]
    if (!length(scores)) next
    cuts[[domain]] <- stats::quantile(
      scores, c(.AEDC_VULNERABLE_PERCENTILE, .AEDC_AT_RISK_PERCENTILE),
      names = FALSE, type = 7)
  }
  if (!length(cuts)) NULL else cuts
}


#' Recut a cycle's domain categories against the baseline cut scores
#'
#' @param ds_dir Character. AEDC dataset directory.
#' @param cycle Integer. Cycle year.
#' @param cuts List. Cut scores from `.aedc_cut_scores()`.
#' @return Invisibly, TRUE when the cycle was rewritten.
#' @keywords internal
.aedc_apply_cut_scores <- function(ds_dir, cycle, cuts) {
  if (is.null(cuts)) return(invisible(FALSE))
  path <- file.path(ds_dir, sprintf("madipge-aedc-d-core-%d.parquet", cycle))
  if (!file.exists(path)) return(invisible(FALSE))
  frame <- as.data.frame(read_parquet_safely(path), stringsAsFactors = FALSE)
  if (!nrow(frame)) return(invisible(FALSE))

  changed <- FALSE
  for (domain in names(cuts)) {
    column <- paste0(domain, "CATEGORY")
    if (!domain %in% names(frame) || !column %in% names(frame)) next
    scores <- suppressWarnings(as.numeric(frame[[domain]]))
    # 1 developmentally vulnerable, 2 developmentally at risk, 3 on track.
    # The fourth category the generator used is not in the AEDC measure.
    category <- ifelse(scores < cuts[[domain]][[1L]], 1L,
                       ifelse(scores < cuts[[domain]][[2L]], 2L, 3L))
    category[is.na(scores)] <- NA_integer_
    frame[[column]] <- category
    changed <- TRUE
  }
  if (!changed) return(invisible(FALSE))

  arrow::write_parquet(frame, path)
  domain_path <- file.path(ds_dir,
                           sprintf("madipge-aedc-d-domain-%d.parquet", cycle))
  if (file.exists(domain_path)) arrow::write_parquet(frame, domain_path)
  invisible(TRUE)
}
