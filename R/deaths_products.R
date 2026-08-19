# DEATHS product splitting and year vintaging.
#
# The real asset publishes deaths as two product families. `cause_of_death_*`
# carries the medical record -- the underlying cause, the entity and RACS axes,
# the certifier, and the geography of usual residence. `death_registrations_*`
# is the registration itself: fourteen demographic variables, for 2007 to
# 2012 only.
#
# The generator wrote one flat table with both in it, so a consumer looking
# for the registration table found nothing, and one reading the combined
# table saw a medical record attached to every registration, which the real
# data does not have for those years.
#
# The geography also carries a vintage. ASGS and SEIFA were reissued after the
# 2021 Census, and the data item list names the reissued variables separately:
# `SEIFA_IRSD_DEC` runs to the 2023 preliminary release and
# `SEIFA_IRSD_DEC_2021` starts at the 2021 final one. Emitting the 2021 names
# in every year told a consumer the 2021 boundaries applied to a 2013 death.


# The registration table exists for these years only.
.DEATHS_REGISTRATION_YEARS <- 2007L:2012L

# The fourteen variables the data item list gives `death_registrations_*`.
.DEATHS_REGISTRATION_VARIABLES <- c(
  "SYNTHETIC_AEUID", "REFERENCE_YEAR", "YEAR_OF_DEATH", "MONTH_OF_DEATH",
  "DEATH_DATE", "DEATH_AGE", "SEX", "BIRTH_PLACE", "MARITAL_STATUS",
  "INDIGENOUS_STATUS", "PERIOD_RESIDENCE", "REG_STATE", "URES5_SA2",
  "URES9_SA2"
)

# Geography and socio-economic variables reissued after the 2021 Census. The
# earlier name applies up to and including the last year the list gives it;
# the reissued one applies from the year it starts.
.DEATHS_VINTAGED <- list(
  list(current = "SEIFA_IRSD_DEC_2021", earlier = "SEIFA_IRSD_DEC",
       from = 2021L),
  list(current = "SEIFA_IRSAD_DEC_2021", earlier = "SEIFA_IRSAD_DEC",
       from = 2021L),
  list(current = "REMOTENESS_AREA_2021", earlier = "REMOTENESS_AREA",
       from = 2021L),
  list(current = "URES9_SA2_2021", earlier = "URES9_SA2", from = 2021L),
  list(current = "LGA_CODE_2023", earlier = "LGA_CODE", from = 2021L)
)

# PLACE_OF_DEATH appears in the 2019 tables and nowhere else.
.DEATHS_PLACE_OF_DEATH_YEARS <- 2019L


#' File name for a DEATHS product
#'
#' @param family Character. "cause-of-death" or "death-registrations".
#' @param year Integer. Reference year.
#' @return Character file name.
#' @keywords internal
.deaths_product_file <- function(family, year) {
  sprintf("madipge-death-d-%s-%d.parquet", family, year)
}


#' Period of residence at the address of usual residence
#'
#' @param n Integer. Number of rows.
#' @param key Numeric vector. A stable per-person key.
#' @return Character vector of period codes.
#' @keywords internal
.deaths_period_residence <- function(n, key) {
  if (!n) return(character(0))
  # 1 under a year, 2 one to under five, 3 five to under ten, 4 ten or more,
  # 9 not stated. Most people who die have lived at the address for years.
  codes <- c("1", "2", "3", "4", "9")
  weights <- c(0.08, 0.19, 0.20, 0.48, 0.05)
  cuts <- cumsum(weights)
  draw <- (key %% 10000) / 10000
  codes[findInterval(draw, cuts) + 1L]
}


#' Split a year's deaths into its published products and apply its vintage
#'
#' Reads the combined table the generator wrote for one year, writes the
#' `death_registrations` product where the year has one, and rewrites the
#' `cause_of_death` product with the variable names that year uses.
#'
#' @param ds_dir Character. DEATHS dataset directory.
#' @param year Integer. Reference year.
#' @param spine data.frame or NULL. Spine rows, for the SA2 of usual
#'   residence.
#' @param seed Integer. Random seed.
#' @return Invisibly, TRUE when a file was rewritten.
#' @keywords internal
.deaths_split_year <- function(ds_dir, year, spine = NULL, seed = 42L) {
  path <- file.path(ds_dir, .deaths_product_file("cause-of-death", year))
  if (!file.exists(path)) return(invisible(FALSE))
  frame <- as.data.frame(read_parquet_safely(path), stringsAsFactors = FALSE)
  if (!nrow(frame)) return(invisible(FALSE))

  # The SA2 of usual residence is the person's, so it comes from the spine
  # rather than being drawn again here.
  if (!is.null(spine) && "aeuid_rbdm" %in% names(spine) &&
      "sa2_code" %in% names(spine)) {
    index <- match(frame$SYNTHETIC_AEUID, as.character(spine$aeuid_rbdm))
    sa2 <- as.character(spine$sa2_code)[index]
  } else {
    sa2 <- rep(NA_character_, nrow(frame))
  }
  frame$URES9_SA2 <- sa2
  # URES5_SA2 is the 2011-boundary SA2 the earlier tables carry. It is the
  # same place, so it agrees with URES9_SA2 above the SA2 level.
  frame$URES5_SA2 <- sa2

  key <- suppressWarnings(as.numeric(gsub("[^0-9]", "",
                                          frame$SYNTHETIC_AEUID)))
  unusable <- !is.finite(key)
  key[unusable] <- seq_len(nrow(frame))[unusable]
  key <- (key * 1000003 + as.numeric(seed) * 9176) %% 1e9
  frame$PERIOD_RESIDENCE <- .deaths_period_residence(nrow(frame), key)

  if (!"DEATH_DATE" %in% names(frame)) {
    frame$DEATH_DATE <- as.Date(sprintf(
      "%04d-%02d-%02d", frame$YEAR_OF_DEATH,
      pmin(pmax(frame$MONTH_OF_DEATH, 1L), 12L),
      pmin(pmax(frame$DEATH_DAY, 1L), 28L)))
  }

  # The registration table, where the year has one.
  if (year %in% .DEATHS_REGISTRATION_YEARS) {
    registration <- frame[, intersect(.DEATHS_REGISTRATION_VARIABLES,
                                      names(frame)), drop = FALSE]
    arrow::write_parquet(
      registration,
      file.path(ds_dir, .deaths_product_file("death-registrations", year)))
  }

  # And the cause-of-death table, under the names that year uses.
  cause <- frame
  cause$URES5_SA2 <- NULL
  cause$PERIOD_RESIDENCE <- NULL
  cause$DEATH_DATE <- NULL
  for (spec in .DEATHS_VINTAGED) {
    if (year >= spec$from) next
    if (spec$current %in% names(cause)) {
      cause[[spec$earlier]] <- cause[[spec$current]]
      cause[[spec$current]] <- NULL
    }
  }
  if (!year %in% .DEATHS_PLACE_OF_DEATH_YEARS) {
    cause$PLACE_OF_DEATH <- NULL
  }
  arrow::write_parquet(cause, path)

  invisible(TRUE)
}


#' Split every year's deaths into its published products
#'
#' @param ds_dir Character. DEATHS dataset directory.
#' @param years Integer vector. Reference years.
#' @param spine data.frame or NULL. Spine rows.
#' @param seed Integer. Random seed.
#' @return Invisibly, the number of years rewritten.
#' @keywords internal
.deaths_split_products <- function(ds_dir, years, spine = NULL, seed = 42L) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(invisible(0L))
  written <- 0L
  for (year in as.integer(years)) {
    if (isTRUE(.deaths_split_year(ds_dir, year, spine, seed))) {
      written <- written + 1L
    }
  }
  invisible(written)
}
