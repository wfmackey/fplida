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
#'   anchor year (calendar 2021). A year outside the table is held at the
#'   nearest end rather than extrapolated, which is what the generators do, so
#'   asking for 1850 returns the earliest level in the table.
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
  # Held at the nearest end outside the table, exactly as `nominal::row()` does
  # in src/rust/src/nominal.rs. Returning NA instead would be more honest in
  # isolation but is worse in practice: an NA index turns a whole money column
  # into NA on the Rust side, and on the R side an NA mean makes `rnorm()`
  # return a single NaN without consuming its draws, which silently shifts every
  # later value in the stream. Both paths hold flat, so they agree.
  clamped <- pmin(pmax(year, min(tbl$year)), max(tbl$year))
  tbl$index[match(clamped, tbl$year)]
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

# Per-unit departure from the headline, R side --------------------------------
#
# The headline index alone moves every unit by the same multiplier, so the ratio
# between one reference year and another would be identical for every business
# in the file. Real firms do not grow in lockstep, and a file where they do is
# obvious the moment anyone plots it. Each unit therefore carries its own
# departure from the headline, drawn deterministically from its identifier.
#
# What follows is a second implementation of the model in
# src/rust/src/nominal.rs. It does not reproduce the Rust numbers for the same
# unit and does not need to. The two cannot share a stream: the Rust draws come
# from a SplitMix64 mix over unsigned 64-bit words, and R has no unsigned 64-bit
# arithmetic, so an R port of that hash would silently be a different function.
# What has to agree is the statistical model, not the digits.
#
# Nothing reconciles a figure from one against a figure from the other. BLADE
# business financials are assembled here in R and have no Rust counterpart at
# all, and the other callers are the pure-R fallbacks that only run when the
# compiled generator is absent, so the two implementations never both produce a
# number for the same row.
#
# The structure mirrors the Rust module: a per-unit growth profile that
# compounds with distance from the anchor, a permanent random walk, and a
# transitory shock in the year itself, all in logs, then mean-corrected by half
# the variance so the population mean still tracks the published series.

# The calendar year every index equals 1, and the year anchor amounts are drawn
# at. Distance from it is what the deviation grows with, on either basis, which
# is how the Rust module measures it too.
.NOMINAL_ANCHOR <- 2021L

# Dispersion presets in log points, mirroring nominal::BUSINESS and
# nominal::PERSON. Business income is much the wider of the two: a firm can lose
# a third of its turnover in a year and get it back the next, and a wage earner
# cannot.
.NOMINAL_BUSINESS_DISPERSION <- c(profile = 0.020, permanent = 0.150,
                                  transitory = 0.100)
.NOMINAL_PERSON_DISPERSION <- c(profile = 0.010, permanent = 0.080,
                                transitory = 0.050)

# Distinct salts keep the three components independent of each other and of
# every other hashed draw in the package.
.NOMINAL_SALT_PROFILE <- 40771L
.NOMINAL_SALT_PERMANENT <- 55621L
.NOMINAL_SALT_TRANSITORY <- 71317L

# A standard normal from a unit identifier, deterministic and vectorised over
# `unit`. The mixing follows the integer-hash idiom used throughout this package
# rather than any RNG, so nothing here consumes a random stream and no generator
# output shifts because a deviation was added.
.nominal_hash_normal <- function(unit, seed, salt, year) {
  modulus <- 2147483647
  # Everything the draw depends on goes in at once. Folding the year in later,
  # after the last non-linear step, would leave two years differing by a constant
  # shift through a linear map: consecutive years then come out correlated near
  # 0.5, the permanent walk grows about 40 per cent faster than its own standard
  # deviation says, and the mean correction no longer holds the population mean
  # on the headline.
  x <- ((as.numeric(unit) %% modulus) * 104729 +
          (as.numeric(seed) %% modulus) * 8191 +
          as.numeric(salt) * 65537 +
          (as.numeric(year) + 4096) * 2654435) %% modulus
  # Each round is a multiply then a xor-shift. The xor-shifts are what break the
  # lattice a bare linear congruence leaves behind, and they earn their place
  # here because unit keys arrive in near-arithmetic sequences, which is the one
  # input a linear congruence maps onto a visible pattern. Every product stays
  # under 2^53, so the double arithmetic is exact.
  xi <- as.integer(x)
  x <- as.numeric(bitwXor(xi, bitwShiftR(xi, 15)))
  x <- (x * 48271 + as.numeric(salt) * 8191 + 907633385) %% modulus
  xi <- as.integer(x)
  x <- as.numeric(bitwXor(xi, bitwShiftR(xi, 13)))
  x <- (x * 69621 + 12345) %% modulus
  xi <- as.integer(x)
  x <- as.numeric(bitwXor(xi, bitwShiftR(xi, 16)))
  # Held off both tails, so the normal is always finite.
  stats::qnorm((x + 0.5) / (modulus + 1))
}

# A unit's log departure from the headline in one year. `year` is a single year
# on whichever basis the caller is working in; `unit` is a vector of integer
# keys, one per unit.
.nominal_unit_deviation <- function(unit, seed, year,
                                    dispersion = .NOMINAL_BUSINESS_DISPERSION) {
  n <- length(unit)
  year <- as.integer(year)[[1L]]
  if (n == 0L) return(numeric(0))
  if (is.na(year) || all(dispersion == 0)) return(rep(0, n))
  k <- year - .NOMINAL_ANCHOR
  # The anchor amount already carries the unit's own level, so the departure
  # from it starts at nothing.
  if (k == 0L) return(rep(0, n))

  profile <- dispersion[["profile"]] * k *
    .nominal_hash_normal(unit, seed, .NOMINAL_SALT_PROFILE, 0L)

  # A shock belongs to the later of the two years it separates, so the walk
  # gives the same path whichever direction it is built from.
  walk <- rep(0, n)
  if (k > 0L) {
    for (y in (.NOMINAL_ANCHOR + 1L):year) {
      walk <- walk +
        .nominal_hash_normal(unit, seed, .NOMINAL_SALT_PERMANENT, y)
    }
  } else {
    for (y in (year + 1L):.NOMINAL_ANCHOR) {
      walk <- walk -
        .nominal_hash_normal(unit, seed, .NOMINAL_SALT_PERMANENT, y)
    }
  }
  permanent <- dispersion[["permanent"]] * walk

  transitory <- dispersion[["transitory"]] *
    .nominal_hash_normal(unit, seed, .NOMINAL_SALT_TRANSITORY, year)

  # Without the half-variance correction the mean amount would drift above the
  # published series by the Jensen gap, and the gap would widen with every year
  # away from the anchor, so synthetic aggregates would stop reconciling.
  variance <- dispersion[["profile"]]^2 * k^2 +
    dispersion[["permanent"]]^2 * abs(k) +
    dispersion[["transitory"]]^2
  profile + permanent + transitory - 0.5 * variance
}

# A stable integer key from whatever identifier a product carries, mirroring
# `nominal::unit_key` on the Rust side. The digits of an id are enough: what the
# key has to be is the same number every time the same unit is generated, and
# every id in this package is either numeric or a fixed-width code.
.nominal_unit_key <- function(id) {
  digits <- gsub("[^0-9]", "", as.character(id))
  out <- suppressWarnings(as.numeric(digits))
  # An id with no digits at all still needs a key, and any stable one will do.
  missing <- is.na(out)
  if (any(missing)) out[missing] <- which(missing)
  out
}

# The multiplier that turns an anchor-year dollar amount into a `year` amount
# for each unit: the headline index times that unit's own departure from it.
#
# `year` is either one year for every unit or one year per unit. The second form
# is what a product needs when each row carries its own reference year, as a
# benefit spell or a study year does.
.nominal_unit_factor <- function(series, year, unit, seed,
                                 dispersion = .NOMINAL_BUSINESS_DISPERSION,
                                 basis = "financial") {
  n <- length(unit)
  if (n == 0L) return(numeric(0))
  year <- as.integer(year)
  if (length(year) == 1L) {
    if (is.na(year)) return(rep(1, n))
    return(nominal_index(series, year, basis) *
             exp(.nominal_unit_deviation(unit, seed, year, dispersion)))
  }
  stopifnot(length(year) == n)

  # One year at a time, because the deviation walks from the anchor to a single
  # year. There are only ever a handful of distinct years in a column, so this
  # is a few passes rather than a loop over rows.
  out <- rep(1, n)
  for (y in unique(year[!is.na(year)])) {
    rows <- which(year == y)
    out[rows] <- nominal_index(series, y, basis) *
      exp(.nominal_unit_deviation(unit[rows], seed, y, dispersion))
  }
  out
}
