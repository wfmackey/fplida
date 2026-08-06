# Nominal price and income indices -----------------------------------------
#
# The generated data is nominal, so a dollar in one reference year is not a
# dollar in another. These accessors expose the same published series the
# generators use, so a user can deflate fplida output with the exact index that
# inflated it, and so the R fallback implementations stay in step with Rust.
#
# The table is built by data-raw/update_nominal_indices.R from ABS and ATO
# publications. src/rust/src/nominal_series.rs holds the same numbers as
# compiled-in constants for the generation path.

.nominal_registry <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) return(cache)
    path <- system.file("extdata", "nominal-indices.csv", package = "fplida")
    if (!nzchar(path)) path <- file.path("inst", "extdata", "nominal-indices.csv")
    tbl <- utils::read.csv(path, stringsAsFactors = FALSE)
    tbl$year <- as.integer(tbl$year)
    tbl$index <- as.numeric(tbl$index)
    tbl$projected <- as.logical(tbl$projected)
    cache <<- tbl
    cache
  }
})

.nominal_basis <- function(basis) {
  basis <- match.arg(basis, c("financial", "calendar"))
  if (identical(basis, "financial")) "financial_year" else "calendar_year"
}

#' Published nominal indices used to generate fplida dollar amounts
#'
#' Every dollar figure fplida generates is nominal: it carries the price and
#' wage level of its reference year. These are the published series that put it
#' there. Divide a generated amount by the index for its year to express it in
#' anchor-year dollars.
#'
#' @param series Character. One of \code{"wage"} (ATO average salary or wages
#'   per individual), \code{"price"} (ABS Consumer Price Index),
#'   \code{"business"} (ATO average total income per company),
#'   \code{"transfer"} (Male Total Average Weekly Earnings, the pension
#'   benchmark) or \code{"health"} (the Medicare Benefits Schedule indexation
#'   factor). Reference series not used in generation are available through
#'   \code{nominal_indices()}.
#' @param year Integer vector. A financial year is named by the calendar year
#'   it starts in, so 2011 is the year ended 30 June 2012.
#' @param basis Character. \code{"financial"} or \code{"calendar"}. Tax, BLADE
#'   and payment-summary products are on a financial-year basis; Census,
#'   residence and the monthly service products are on a calendar-year basis.
#'
#' @return A numeric vector the same length as \code{year}, equal to 1 in the
#'   anchor year (calendar 2021). \code{NA} for years outside the table.
#'
#' @examples
#' # How much did prices rise between 2015-16 and 2023-24?
#' nominal_index("price", 2023) / nominal_index("price", 2015)
#'
#' # Deflate a 2023-24 wage to anchor-year dollars.
#' 95000 / nominal_index("wage", 2023)
#'
#' @seealso \code{\link{nominal_indices}} for the full table, including the
#'   reference series and the flag marking projected years.
#' @export
nominal_index <- function(series = c("wage", "price", "business", "transfer",
                                     "health"),
                          year,
                          basis = c("financial", "calendar")) {
  series <- match.arg(series)
  basis <- .nominal_basis(basis)
  year <- as.integer(year)

  tbl <- .nominal_registry()
  tbl <- tbl[tbl$series == series & tbl$basis == basis, , drop = FALSE]
  tbl$index[match(year, tbl$year)]
}

#' The full table of nominal indices, with provenance
#'
#' The headline series fplida generates against, plus the reference series that
#' were considered alongside them: the Wage Price Index, Average Weekly
#' Earnings, nominal GDP and several ATO aggregates. Every row carries a
#' \code{projected} flag marking years the published data does not reach, which
#' this package filled by compounding the trailing ten-year average growth.
#'
#' @return A data frame with columns \code{series}, \code{role} (headline or
#'   reference), \code{basis}, \code{year}, \code{index} and \code{projected}.
#'
#' @examples
#' idx <- nominal_indices()
#' head(idx[idx$role == "headline" & idx$basis == "financial_year", ])
#'
#' @seealso \code{\link{nominal_index}} to look up a single series.
#' @export
nominal_indices <- function() {
  .nominal_registry()
}

# Headline log growth between two years, for the R fallback generators. Keeps
# the pure-R implementations on the same series as the Rust ones.
.nominal_log_step <- function(series, from, to, basis = "calendar") {
  from_index <- nominal_index(series, from, basis)
  to_index <- nominal_index(series, to, basis)
  log(to_index / from_index)
}
