# Agency codes and dataset mappings for PLIDA linkage infrastructure.

.AGENCIES <- c(
  "ABS", "AIHW", "ATO", "APSC", "DE", "DEWR", "DHDA",
  "DSS", "HA", "NCVER", "NDIA", "RBDM", "SA"
)

.AGENCY_DATASETS <- list(
  ABS   = c("BLADE", "CENSUS", "ACLD", "COMBINED", "CORE", "LFS", "NHS",
            "NSMHW", "PEX", "SDAC"),
  AIHW  = c("NACDC"),
  ATO   = c("ATO_CR", "ATO_MCS", "BUSOWN", "CGT", "ERS", "JK", "JM",
            "PIT_IE", "PIT_ITR", "PIT_PS", "RPS", "SAE", "SMSF", "STP"),
  APSC  = c("APSED"),
  DE    = c("AEDC", "HE"),
  DEWR  = c("A&T"),
  DHDA  = c("AIR", "MBS", "PBS"),
  DSS   = c("DOMINO", "DEX"),
  HA    = c("AMEP", "MT_DEMOGS", "SDB", "TRAVELLERS", "VISA"),
  NCVER = c("TVA"),
  NDIA  = c("NDIS"),
  RBDM  = c("BIRTHS", "DEATHS"),
  SA    = c("MCD")
)

.AGENCY_LINKAGE_DEFAULT_RATE <- 0.90
.AGENCY_LINKAGE_RATES <- c(
  ATO = 0.98
)

.AEUID_SEQ_LIMIT <- 0x10000000
.ATO_TAX_FREE_THRESHOLD <- 18200
.ATO_REFERENCE_YEAR <- 2024L


.agency_linkage_rate <- function(agency) {
  rate <- unname(.AGENCY_LINKAGE_RATES[agency])
  if (length(rate) == 0L || is.na(rate)) .AGENCY_LINKAGE_DEFAULT_RATE else rate
}


.agency_linkage_mask <- function(n, agency) {
  n <- as.integer(n)
  if (n <= 0L) return(logical(0L))

  rate <- .agency_linkage_rate(agency)
  n_linked <- max(0L, min(n, as.integer(round(n * rate))))
  linked <- rep(FALSE, n)
  if (n_linked == 0L) return(linked)
  if (n_linked == n) {
    linked[] <- TRUE
    return(linked)
  }

  agency_salt <- sum(utf8ToInt(agency))
  has_seed <- exists(".Random.seed", envir = globalenv())
  old_seed <- if (has_seed) .Random.seed else NULL
  on.exit({
    if (is.null(old_seed)) {
      rm(".Random.seed", envir = globalenv(), inherits = FALSE)
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)

  set.seed(910000L + agency_salt * 1009L)
  linked[sample.int(n, n_linked, replace = FALSE)] <- TRUE
  linked
}


.agency_aeuid_prefix <- function(agency, seed) {
  agency <- toupper(agency)
  codes <- utf8ToInt(agency)
  agency_hash <- sum(as.numeric(codes) * seq_along(codes) * 4099)
  as.integer((as.numeric(seed) * 1009 + agency_hash + 0xA53C7) %% 0x100000)
}


.aeuid_uniform <- function(aeuid) {
  aeuid <- as.character(aeuid)
  prefix <- suppressWarnings(strtoi(substr(aeuid, 1L, 5L), base = 16L))
  suffix <- suppressWarnings(strtoi(substr(aeuid, 6L, 12L), base = 16L))
  bad <- is.na(suffix)
  suffix[bad] <- seq_along(suffix)[bad]
  prefix[is.na(prefix)] <- 0L
  ((as.numeric(suffix) * 104729 + as.numeric(prefix) * 13007 + 8191) %%
     1000003) / 1000003
}


.ato_record_probability <- function(spine_df,
                                    reference_year = .ATO_REFERENCE_YEAR,
                                    tax_free_threshold = .ATO_TAX_FREE_THRESHOLD) {
  n <- nrow(spine_df)
  if (n == 0L) return(numeric(0))

  age <- if ("birth_year" %in% names(spine_df)) {
    as.integer(reference_year) - as.integer(spine_df$birth_year)
  } else {
    rep(45L, n)
  }
  income <- if ("baseline_income" %in% names(spine_df)) {
    pmax(as.numeric(spine_df$baseline_income), 0)
  } else {
    rep(tax_free_threshold * 2, n)
  }
  employed <- if ("baseline_employed" %in% names(spine_df)) {
    as.integer(spine_df$baseline_employed) == 1L
  } else {
    income > 0
  }

  p <- rep(0.94, n)
  p[age < 16L] <- 0.08

  working_age <- age >= 16L & age < 65L
  below_threshold <- income < tax_free_threshold
  p[working_age & below_threshold & employed & income > 0] <- 0.55
  p[working_age & below_threshold & (!employed | income <= 0)] <- 0.30
  p[working_age & !below_threshold] <- 0.98

  retired <- age >= 65L
  p[retired & income < tax_free_threshold] <- 0.42
  p[retired & income >= tax_free_threshold & income < 45000] <- 0.70
  p[retired & income >= 45000] <- 0.88
  p[age >= 75L & income < tax_free_threshold] <- 0.32

  p[is.na(age) | is.na(income)] <- 0.94
  pmin(pmax(p, 0), 1)
}


.ato_record_mask <- function(spine_df,
                             reference_year = .ATO_REFERENCE_YEAR,
                             tax_free_threshold = .ATO_TAX_FREE_THRESHOLD) {
  n <- nrow(spine_df)
  if (n == 0L) return(logical(0))
  if (!"aeuid_ato" %in% names(spine_df)) return(rep(TRUE, n))

  .aeuid_uniform(spine_df$aeuid_ato) <
    .ato_record_probability(
      spine_df,
      reference_year = reference_year,
      tax_free_threshold = tax_free_threshold
    )
}


filter_ato_records <- function(spine_df,
                               reference_year = .ATO_REFERENCE_YEAR,
                               tax_free_threshold = .ATO_TAX_FREE_THRESHOLD) {
  spine_df[
    .ato_record_mask(
      spine_df,
      reference_year = reference_year,
      tax_free_threshold = tax_free_threshold
    ),
    ,
    drop = FALSE
  ]
}


#' Generate deterministic SYNTHETIC_AEUID values for an agency
#'
#' Produces unique, deterministic hex-formatted identifiers for each person
#' within an agency. The same (n, agency, seed) always gives the same output.
#'
#' @param n Integer. Number of persons.
#' @param agency Character. Agency code (e.g. "ABS", "ATO").
#' @param seed Integer. Base seed (same as the spine seed).
#' @return Character vector of length n with hex-formatted AEUIDs.
#' @keywords internal
generate_aeuids <- function(n, agency, seed) {
  n <- as.integer(n)
  if (n <= 0L) return(character(0))
  if (n >= .AEUID_SEQ_LIMIT) {
    stop("AEUID generator supports n < ", .AEUID_SEQ_LIMIT, call. = FALSE)
  }

  sprintf("%05X%07X", .agency_aeuid_prefix(agency, seed), seq_len(n))
}


#' Look up the agency for a dataset
#'
#' @param dataset Character. Dataset name (e.g. "CENSUS", "PIT_ITR").
#' @return Agency code, or NULL if not found.
#' @keywords internal
dataset_to_agency <- function(dataset) {
  for (agency in names(.AGENCY_DATASETS)) {
    if (dataset %in% .AGENCY_DATASETS[[agency]]) return(agency)
  }
  NULL
}


#' Write an agency spine file
#'
#' Creates a \code{[agency]_spine} file (parquet or csv) with \code{spine_id}
#' and \code{SYNTHETIC_AEUID} columns, matching the real PLIDA structure.
#' If the file already exists it is not overwritten (idempotent).
#'
#' @param spine_df Data.frame. The full fplida-base spine (must contain
#'   \code{spine_id} and \code{aeuid_[agency]} columns).
#' @param agency Character. Agency code (e.g. "ABS").
#' @param output_dir Character. Directory to write the file.
#' @param format Character. Output format: "parquet" (default) or "csv".
#' @param reference_year Integer. Year used for ATO non-lodgement
#'   modelling. Ignored for non-ATO agency spines.
#' @return The file path (invisibly).
#' @export
write_agency_spine <- function(spine_df, agency, output_dir,
                               format = c("parquet", "csv"),
                               reference_year = .ATO_REFERENCE_YEAR) {
  agency <- toupper(agency)
  format <- match.arg(format)
  stopifnot(
    "`agency` must be a known agency code" = agency %in% .AGENCIES,
    "`output_dir` must be provided" = !is.null(output_dir)
  )

  ext <- if (format == "parquet") ".parquet" else ".csv"
  path <- file.path(output_dir, paste0(tolower(agency), "-spine", ext))

  if (file.exists(path)) {
    message(agency, " spine already exists at ", path)
    return(invisible(path))
  }

  aeuid_col <- paste0("aeuid_", tolower(agency))
  stopifnot(
    "`spine_df` must contain spine_id column" = "spine_id" %in% names(spine_df),
    "`spine_df` must contain AEUID column for agency" = aeuid_col %in% names(spine_df)
  )

  if (agency == "ATO") {
    spine_df <- filter_ato_records(spine_df, reference_year = reference_year)
  }

  spine_id <- as.character(spine_df$spine_id)
  missing_spine_id <- is.na(spine_id) | !nzchar(spine_id)
  if (any(missing_spine_id)) {
    spine_id[missing_spine_id] <- sprintf("SP%010d", which(missing_spine_id))
  }
  spine_id[!.agency_linkage_mask(length(spine_id), agency)] <- NA_character_

  out_df <- data.frame(
    spine_id = spine_id,
    SYNTHETIC_AEUID = spine_df[[aeuid_col]],
    stringsAsFactors = FALSE
  )

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for parquet output. ",
           "Install it with: install.packages('arrow')", call. = FALSE)
    }
    arrow::write_parquet(out_df, path)
  } else {
    write.csv(out_df, path, row.names = FALSE)
  }

  message(agency, " spine saved to ", path)
  invisible(path)
}
