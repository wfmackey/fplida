# MCD product splitting.
#
# Medicare consumer directory is published as three families per extract
# vintage -- demographics, address and entitlements -- and the generator wrote
# one flat `enrolments` table with a handful of columns from each. A consumer
# looking for `mcd_0623_address` found nothing, and one reading the flat table
# got a record whose grain matched none of the three: the address family has a
# start and end date per address spell, and the entitlement family a
# concession programme code with its own status dates.
#
# The extract vintages are named in the data item list. Only the June 2022 one
# carries two address tables, because it spans the ASGS reissue and gives the
# 2016 and 2021 boundaries separately.

# Extract vintages the data item list names, and whether each splits its
# address table by ASGS edition.
.MCD_VINTAGES <- list(
  list(code = "0622", asgs = c("2016", "2021")),
  list(code = "0623", asgs = "2021"),
  list(code = "0624", asgs = "2021"),
  list(code = "0625", asgs = "2021"),
  list(code = "1225", asgs = "2021")
)

.MCD_DEMOGS_VARIABLES <- c(
  "SYNTHETIC_AEUID", "YEAR_OF_BIRTH", "MONTH_OF_BIRTH", "SEX",
  "YEAR_OF_DEATH", "MONTH_OF_DEATH", "CNSMR_STS", "CNSMR_ETS",
  "CNSMR_CHRTC_STS", "CNSMR_CHRTC_ETS"
)

.MCD_ENTITLEMENT_VARIABLES <- c(
  "SYNTHETIC_AEUID", "CNPGM_ETM_CDE", "CNPGM_ETM_STS", "CNPGM_ETM_ETS",
  "CON_CNPGM_ETM_STS", "CON_CNPGM_ETM_ETS", "REL_CNTRY_CDE"
)


#' File name for an MCD product
#'
#' @param vintage Character. Extract vintage, e.g. "0623".
#' @param family Character. "demogs", "address" or "entitlements".
#' @param asgs Character or NULL. ASGS edition, for the vintage that splits
#'   its address table.
#' @return Character file name.
#' @keywords internal
.mcd_product_file <- function(vintage, family, asgs = NULL) {
  stem <- if (is.null(asgs)) {
    sprintf("mcd_%s_%s", vintage, family)
  } else {
    sprintf("mcd_%s_%s_asgs_%s", vintage, family, asgs)
  }
  sprintf("madipge-mcd-d-%s.parquet", gsub("_", "-", stem))
}


#' A status date pair for an MCD spell
#'
#' Every MCD family dates its records with a start and an end, and an open
#' spell has no end. The dates are derived from the person rather than drawn
#' per family, so a person's demographics, address and entitlement spells sit
#' inside one another rather than crossing.
#'
#' @param n Integer. Number of rows.
#' @param key Numeric vector. A stable per-person key.
#' @param first_year Integer. Earliest a spell can start.
#' @param last_year Integer. Latest a spell can end.
#' @param open_share Numeric. Share of spells still open.
#' @return A list with `start` and `end` Date vectors.
#' @keywords internal
.mcd_spell_dates <- function(n, key, first_year, last_year,
                             open_share = 0.75) {
  if (!n) return(list(start = as.Date(character(0)),
                      end = as.Date(character(0))))
  span <- max(1L, last_year - first_year)
  start_year <- first_year + (key %% span)
  start <- as.Date(sprintf("%04d-%02d-01", start_year,
                           1L + as.integer((key %/% 7) %% 12)))
  open <- ((key %/% 13) %% 100) < (open_share * 100)
  end_year <- pmin(last_year, start_year + 1L + as.integer((key %/% 17) %% 8))
  end <- as.Date(sprintf("%04d-%02d-28", end_year,
                         1L + as.integer((key %/% 23) %% 12)))
  end[open] <- as.Date(NA)
  list(start = start, end = end)
}


#' Split the MCD enrolments table into its published products
#'
#' @param ds_dir Character. MCD dataset directory.
#' @param spine data.frame or NULL. Spine rows, for the address geography.
#' @param seed Integer. Random seed.
#' @param first_year Integer. Earliest spell year.
#' @param last_year Integer. Latest spell year.
#' @return Invisibly, the number of files written.
#' @keywords internal
.mcd_split_products <- function(ds_dir, spine = NULL, seed = 42L,
                                first_year = 2006L, last_year = 2025L) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(invisible(0L))
  source_path <- file.path(ds_dir,
                           "madipge-mcd-d-enrolments-06-current.parquet")
  if (!file.exists(source_path)) return(invisible(0L))
  frame <- as.data.frame(read_parquet_safely(source_path),
                         stringsAsFactors = FALSE)
  if (!nrow(frame)) return(invisible(0L))

  n <- nrow(frame)
  key <- suppressWarnings(as.numeric(gsub("[^0-9]", "",
                                          frame$SYNTHETIC_AEUID)))
  unusable <- !is.finite(key)
  key[unusable] <- seq_len(n)[unusable]
  key <- (key * 1000003 + as.numeric(seed) * 9176) %% 1e9

  # The address is the person's own, so it comes from the same lookup every
  # other product reads rather than being drawn again here.
  location <- if (!is.null(spine) && "aeuid_sa" %in% names(spine)) {
    index <- match(frame$SYNTHETIC_AEUID, as.character(spine$aeuid_sa))
    rows <- .spine_address_lookup_rows(spine[index, , drop = FALSE],
                                       agency = "MCD", seed = seed)
    rows
  } else {
    NULL
  }

  written <- 0L
  for (vintage in .MCD_VINTAGES) {
    demogs <- frame[, intersect(.MCD_DEMOGS_VARIABLES, names(frame)),
                    drop = FALSE]
    consumer <- .mcd_spell_dates(n, key, first_year, last_year)
    demogs$CNSMR_STS <- consumer$start
    demogs$CNSMR_ETS <- consumer$end
    characteristic <- .mcd_spell_dates(n, key + 101, first_year, last_year)
    demogs$CNSMR_CHRTC_STS <- characteristic$start
    demogs$CNSMR_CHRTC_ETS <- characteristic$end
    demogs <- demogs[, .MCD_DEMOGS_VARIABLES, drop = FALSE]
    arrow::write_parquet(
      demogs, file.path(ds_dir, .mcd_product_file(vintage$code, "demogs")))
    written <- written + 1L

    entitlements <- frame[, intersect(.MCD_ENTITLEMENT_VARIABLES,
                                      names(frame)), drop = FALSE]
    programme <- .mcd_spell_dates(n, key + 211, first_year, last_year,
                                  open_share = 0.6)
    entitlements$CNPGM_ETM_STS <- programme$start
    entitlements$CNPGM_ETM_ETS <- programme$end
    concession <- .mcd_spell_dates(n, key + 307, first_year, last_year,
                                   open_share = 0.5)
    entitlements$CON_CNPGM_ETM_STS <- concession$start
    entitlements$CON_CNPGM_ETM_ETS <- concession$end
    # Country of residence for a reciprocal health care agreement. 1101 is
    # Australia, which is where all but a few people are.
    entitlements$REL_CNTRY_CDE <- ifelse((key %% 1000) < 8, "8104", "1101")
    entitlements <- entitlements[, .MCD_ENTITLEMENT_VARIABLES, drop = FALSE]
    arrow::write_parquet(
      entitlements,
      file.path(ds_dir, .mcd_product_file(vintage$code, "entitlements")))
    written <- written + 1L

    address_spell <- .mcd_spell_dates(n, key + 401, first_year, last_year)
    for (asgs in vintage$asgs) {
      address <- data.frame(
        SYNTHETIC_AEUID = frame$SYNTHETIC_AEUID,
        ADR_TYP = if ("ADR_TYP" %in% names(frame)) frame$ADR_TYP else "R",
        START_DATE = address_spell$start,
        END_DATE = address_spell$end,
        stringsAsFactors = FALSE
      )
      address[[paste0("SA1_ASGS_", asgs)]] <-
        if (!is.null(location)) location$sa1_code else NA_character_
      address[[paste0("SA2_ASGS_", asgs)]] <-
        if (!is.null(location)) location$sa2_code else NA_character_
      address[[paste0("SA4_ASGS_", asgs)]] <-
        if (!is.null(location)) as.integer(location$sa4_code) else NA_integer_
      address[[paste0("STATE_ASGS_", asgs)]] <-
        if (!is.null(location)) as.integer(location$state) else NA_integer_

      # Only the June 2022 vintage names its ASGS edition in the table, as it
      # is the only one that carries both.
      suffix <- if (length(vintage$asgs) > 1L) asgs else NULL
      arrow::write_parquet(
        address,
        file.path(ds_dir, .mcd_product_file(vintage$code, "address", suffix)))
      written <- written + 1L
    }
  }

  invisible(written)
}
