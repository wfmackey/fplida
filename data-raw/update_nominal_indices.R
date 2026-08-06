# Build the nominal price, wage and business indices that give every generated
# dollar amount a year.
#
# WHY THIS EXISTS
#
# Every dollar figure in PLIDA and BLADE is a nominal figure. A wage in the
# 2016-17 STP file and a wage in the 2023-24 STP file are not the same unit,
# because prices and wages moved 30 per cent between them. Until this file
# existed, fplida drew a person's income once at a 2021 anchor and reused it in
# every reference year, so a synthetic PLIDA had no inflation and no nominal
# wage growth at all. Anyone using it to test a deflation step, a real-terms
# conversion, a bracket-creep calculation or a nominal-versus-real regression
# got an answer that could not be wrong, because there was nothing there to get
# wrong.
#
# This script builds the headline series that fix that, from published sources
# only. The generation side adds person-level and business-level deviation
# around these headlines; see `src/rust/src/nominal.rs`.
#
# WHICH SERIES AND WHY
#
#   price     ABS Consumer Price Index, All groups, Australia, original.
#             The general price level. Drives benefit rates, co-payments,
#             fee schedules and anything legislated as CPI-indexed.
#
#   wage      ATO Taxation statistics, Individuals Table 1A: total salary or
#             wages divided by the number of individuals reporting salary or
#             wages. This is the closest published analogue of the thing the
#             synthetic data actually contains -- the average wage on an
#             Australian income tax return -- so PLIDA's own wage aggregates
#             reproduce it. It runs about half a point a year above the Wage
#             Price Index because it carries compositional change and job
#             mobility, which the synthetic panel also has.
#
#   business  ATO Taxation statistics, Company Table 1A: total income divided
#             by the number of companies. The BLADE analogue of the wage
#             series, and much more volatile, as company income is.
#
#   transfer  The income-support indexation path. Pensions take the higher of
#             CPI and the Pensioner and Beneficiary Living Cost Index, then are
#             benchmarked to Male Total Average Weekly Earnings; the MTAWE
#             benchmark binds in most years, so MTAWE is used for the pension
#             path. Allowances are CPI-indexed and use `price`.
#
#   health    The Medicare Benefits Schedule indexation factor. MBS fees move
#             on the Wage Cost Index, not CPI, and were frozen outright between
#             2013-14 and 2019-20, which is why a Medicare benefit in the
#             synthetic data must not simply track prices.
#
# All five are published on a financial-year basis and are also given on a
# calendar-year basis, because PLIDA mixes the two: tax and BLADE products are
# financial-year, Census and the residence products are calendar-year.
#
# OUTPUTS
#
#   inst/extdata/nominal-indices.csv   the tidy series, with provenance
#   src/rust/src/nominal_series.rs     the same numbers as Rust constants
#
# The Rust table is generated rather than read at run time because it is small,
# it is needed inside every slice worker, and compiling it in removes a file
# read from the hot path. `scripts/generate_anzsco_table.py` does the same for
# the ANZSCO table.
#
# REGENERATE WITH
#
#   Rscript data-raw/update_nominal_indices.R
#
# Needs the internet, and `readxl` to read the ATO spreadsheets.

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
  }
  normalizePath(
    file.path("data-raw", "update_nominal_indices.R"),
    mustWork = TRUE
  )
}

.repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."),
  winslash = "/",
  mustWork = TRUE
)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required to read the ATO taxation statistics ",
       "spreadsheets.", call. = FALSE)
}

# The first and last financial year the generated table covers. The published
# data does not reach either end, so the tails are projected; every projected
# row is flagged as such in the CSV and in the Rust table.
.first_fy <- 1994L
.last_fy <- 2029L

# Everything is expressed relative to calendar 2021, because the fplida spine
# draws each person's baseline income at a calendar-2021 anchor.
.anchor_cy <- 2021L

# Number of trailing years averaged to extrapolate beyond the published data.
.tail_window <- 10L

.cache_dir <- file.path(tempdir(), "fplida-nominal-cache")
dir.create(.cache_dir, recursive = TRUE, showWarnings = FALSE)

.download <- function(url, dest_name) {
  dest <- file.path(.cache_dir, dest_name)
  if (!file.exists(dest)) {
    message("  fetching ", dest_name)
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}

# ---------------------------------------------------------------------------
# ABS, via the SDMX data API at data.api.abs.gov.au
# ---------------------------------------------------------------------------

# The key is positional: every dimension of the dataflow in order, dots
# between. `.abs_keys` records what each position means, because the codes on
# their own are unreadable and the ABS reuses numbers across dataflows.
.abs_series <- list(
  cpi = list(
    flow = "ABS,CPI,2.0.0",
    key = "1.10001.10.50.Q",
    dims = paste(
      "MEASURE=1 index numbers; INDEX=10001 All groups CPI;",
      "TSEST=10 original; REGION=50 Australia; FREQ=Q"
    ),
    freq = "Q",
    label = "ABS Consumer Price Index, All groups, Australia, original",
    catalogue = "ABS 6401.0"
  ),
  wpi = list(
    flow = "ABS,WPI,1.2.0",
    key = "1.THRPEB.7.TOT.10.AUS.Q",
    dims = paste(
      "MEASURE=1 index; INDEX=THRPEB total hourly rates of pay excluding",
      "bonuses; SECTOR=7 private and public; INDUSTRY=TOT all industries;",
      "TSEST=10 original; REGION=AUS; FREQ=Q"
    ),
    freq = "Q",
    label = paste(
      "ABS Wage Price Index, total hourly rates of pay excluding bonuses,",
      "all sectors, Australia, original"
    ),
    catalogue = "ABS 6345.0"
  ),
  awote = list(
    flow = "ABS,AWE,1.0.0",
    key = "3.1.3.7.TOT.10.AUS.S",
    dims = paste(
      "MEASURE=3 full-time adult average weekly ordinary time earnings;",
      "ESTIMATE_TYPE=1 earnings; SEX=3 persons; SECTOR=7 private and public;",
      "INDUSTRY=TOT; TSEST=10 original; REGION=AUS; FREQ=S"
    ),
    freq = "S",
    label = paste(
      "ABS Average Weekly Earnings, full-time adult ordinary time earnings,",
      "persons, Australia, original"
    ),
    catalogue = "ABS 6302.0"
  ),
  mtawe = list(
    flow = "ABS,AWE,1.0.0",
    key = "2.1.1.7.TOT.10.AUS.S",
    dims = paste(
      "MEASURE=2 full-time adult average weekly total earnings;",
      "ESTIMATE_TYPE=1 earnings; SEX=1 males; SECTOR=7 private and public;",
      "INDUSTRY=TOT; TSEST=10 original; REGION=AUS; FREQ=S"
    ),
    freq = "S",
    label = paste(
      "ABS Average Weekly Earnings, full-time adult total earnings, males,",
      "Australia, original (MTAWE, the pension benchmark)"
    ),
    catalogue = "ABS 6302.0"
  ),
  gdp = list(
    flow = "ABS,ANA_AGG,1.0.0",
    key = "M3.GPM.10.AUS.Q",
    dims = paste(
      "MEASURE=M3 current prices; DATA_ITEM=GPM gross domestic product;",
      "TSEST=10 original; REGION=AUS; FREQ=Q"
    ),
    freq = "Q",
    label = "ABS Australian National Accounts, GDP, current prices, original",
    catalogue = "ABS 5206.0"
  )
)

.abs_url <- function(spec) {
  sprintf(
    "https://data.api.abs.gov.au/rest/data/%s/%s?startPeriod=1990-Q1&format=csv",
    spec$flow, spec$key
  )
}

.fetch_abs <- function(name, spec) {
  path <- .download(.abs_url(spec), paste0("abs-", name, ".csv"))
  raw <- utils::read.csv(path, stringsAsFactors = FALSE)
  stopifnot(
    "ABS response has no observations" = nrow(raw) > 0L,
    "ABS response is missing TIME_PERIOD/OBS_VALUE" =
      all(c("TIME_PERIOD", "OBS_VALUE") %in% names(raw))
  )
  out <- data.frame(
    period = as.character(raw$TIME_PERIOD),
    value = as.numeric(raw$OBS_VALUE),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$value), , drop = FALSE]
  out[order(out$period), , drop = FALSE]
}

# Quarterly periods read 2011-Q3. Half-yearly AWE periods read 2011-S2, where
# S1 is the May reference period and S2 is November.
.split_period <- function(period) {
  list(
    year = as.integer(substr(period, 1L, 4L)),
    sub = as.integer(substr(period, 7L, 7L))
  )
}

# A financial year runs 1 July to 30 June, so 2011-12 is quarters 2011-Q3,
# 2011-Q4, 2012-Q1 and 2012-Q2, and AWE periods 2011-S2 (November) and 2012-S1
# (May). Incomplete years are dropped rather than part-averaged.
.to_financial_year <- function(obs, freq, aggregate = c("mean", "sum")) {
  aggregate <- match.arg(aggregate)
  parts <- .split_period(obs$period)
  n_sub <- if (identical(freq, "Q")) 4L else 2L
  first_sub <- if (identical(freq, "Q")) 3L else 2L
  fy <- ifelse(parts$sub >= first_sub, parts$year, parts$year - 1L)
  by_fy <- split(obs$value, fy)
  complete <- vapply(by_fy, length, integer(1)) == n_sub
  by_fy <- by_fy[complete]
  if (!length(by_fy)) return(data.frame(fy = integer(0), value = numeric(0)))
  data.frame(
    fy = as.integer(names(by_fy)),
    value = vapply(by_fy, if (aggregate == "sum") sum else mean, numeric(1)),
    row.names = NULL
  )
}

.to_calendar_year <- function(obs, freq, aggregate = c("mean", "sum")) {
  aggregate <- match.arg(aggregate)
  parts <- .split_period(obs$period)
  n_sub <- if (identical(freq, "Q")) 4L else 2L
  by_cy <- split(obs$value, parts$year)
  complete <- vapply(by_cy, length, integer(1)) == n_sub
  by_cy <- by_cy[complete]
  if (!length(by_cy)) return(data.frame(cy = integer(0), value = numeric(0)))
  data.frame(
    cy = as.integer(names(by_cy)),
    value = vapply(by_cy, if (aggregate == "sum") sum else mean, numeric(1)),
    row.names = NULL
  )
}

# ---------------------------------------------------------------------------
# ATO taxation statistics, via data.gov.au
# ---------------------------------------------------------------------------

# Table 1A of each is a single long time series with items down the rows and
# income years across the columns, which is why these two files carry the whole
# history and nothing has to be stitched across editions.
.ato_individuals_url <- paste0(
  "https://data.gov.au/data/dataset/faea4485-f407-457d-97f8-3f0822ccd654/",
  "resource/61e0832a-3e39-43b6-893c-5ef43796651e/download/",
  "ts24individual01byyear.xlsx"
)
.ato_company_url <- paste0(
  "https://data.gov.au/data/dataset/faea4485-f407-457d-97f8-3f0822ccd654/",
  "resource/0f4179ef-3ea3-4b68-a1ad-c5aaac9d28be/download/",
  "ts24company01selecteditemsbyyear.xlsx"
)

.read_ato_table_1a <- function(path) {
  readxl::read_excel(
    path,
    sheet = "Table 1A",
    col_names = FALSE,
    .name_repair = "minimal"
  )
}

# Row labels repeat: an item appears twice, once as a count in `no.` and once
# as an amount in `$`. The unit column disambiguates them.
.ato_row <- function(tbl, label, unit) {
  labels <- as.character(tbl[[1L]])
  units <- as.character(tbl[[2L]])
  hit <- which(labels == label & units == unit)
  if (length(hit) != 1L) {
    stop("expected exactly one ATO row for '", label, "' in ", unit,
         ", found ", length(hit), call. = FALSE)
  }
  suppressWarnings(as.numeric(as.character(unlist(tbl[hit, ]))))
}

# Income years read 1978-79 with an en dash, except the one that straddles the
# century, which the ATO writes as 1999-2000. The first two columns are the
# item label and its unit and carry no year.
.ato_years <- function(tbl) {
  header <- as.character(unlist(tbl[2L, ]))
  fy <- rep(NA_integer_, length(header))
  looks_like_year <- !is.na(header) & grepl("^[0-9]{4}.[0-9]{2}([0-9]{2})?$", header)
  fy[looks_like_year] <- as.integer(substr(header[looks_like_year], 1L, 4L))
  fy
}

.ato_ratio <- function(tbl, label) {
  fy <- .ato_years(tbl)
  amount <- .ato_row(tbl, label, "$")
  count <- .ato_row(tbl, label, "no.")
  keep <- !is.na(fy) & !is.na(amount) & !is.na(count) & count > 0
  data.frame(fy = fy[keep], value = amount[keep] / count[keep])
}

# ---------------------------------------------------------------------------
# Series that are not published as a machine-readable time series
# ---------------------------------------------------------------------------
#
# MBS indexation and the transfer-payment rules are published as legislative
# instruments and departmental notes, one year at a time. They are recorded in
# a checked-in CSV rather than scraped, with the source against each row, and
# read here.

.manual_path <- file.path(.repo_root, "data-raw", "nominal-sources",
                          "administered-indexation.csv")

.read_manual <- function(series_name) {
  stopifnot("administered indexation table is missing" =
              file.exists(.manual_path))
  tbl <- utils::read.csv(.manual_path, stringsAsFactors = FALSE)
  tbl <- tbl[tbl$series == series_name, , drop = FALSE]
  stopifnot("no rows for requested administered series" = nrow(tbl) > 0L)
  tbl <- tbl[order(tbl$fy_start), , drop = FALSE]
  stopifnot(
    "administered indexation years must be consecutive" =
      identical(tbl$fy_start, seq(min(tbl$fy_start), max(tbl$fy_start))),
    "every administered row needs a source" = all(nzchar(tbl$source))
  )
  # A row the Department publishes a factor for is a fact. A row this repo has
  # filled in with a long-run assumption is not, and is carried through to the
  # `projected` flag so it is visible in the generated table.
  data.frame(fy = as.integer(tbl$fy_start),
             pct = as.numeric(tbl$annual_pct_change),
             assumed = tbl$status != "published")
}

# ---------------------------------------------------------------------------
# Index construction
# ---------------------------------------------------------------------------

# Compound an annual percentage change series into a level index. The level is
# arbitrary at this point; every series is rebased to the anchor afterwards.
.pct_to_level <- function(pct_tbl) {
  level <- cumprod(c(1, 1 + pct_tbl$pct[-1] / 100))
  data.frame(fy = pct_tbl$fy, value = level, assumed = pct_tbl$assumed)
}

# Extend a level series to the full window. Before the published data, and
# after it, the series compounds at its own trailing (or leading) geometric
# mean growth over `.tail_window` years. Every filled year is flagged, so a
# user can see which figures are published and which are the script's own
# extrapolation.
.extend <- function(level, first, last, window = .tail_window) {
  level <- level[order(level$fy), , drop = FALSE]
  observed <- level$fy
  stopifnot("series is too short to extrapolate" = nrow(level) > window)

  geo <- function(v) (utils::tail(v, 1L) / utils::head(v, 1L))^(1 / (length(v) - 1L))
  lead_rate <- geo(utils::head(level$value, window + 1L))
  tail_rate <- geo(utils::tail(level$value, window + 1L))

  # The ATO series reaches back to 1978-79, well before the window starts, so
  # drop what falls outside it. The extrapolation rates above are taken from the
  # ends of the published data, not the ends of the window.
  in_window <- level$fy >= first & level$fy <= last
  level <- level[in_window, , drop = FALSE]

  out <- data.frame(fy = first:last, value = NA_real_)
  out$value[match(level$fy, out$fy)] <- level$value

  earliest <- max(min(observed), first)
  latest <- min(max(observed), last)
  for (y in seq_len(max(0L, earliest - first))) {
    yr <- earliest - y
    out$value[out$fy == yr] <- out$value[out$fy == yr + 1L] / lead_rate
  }
  for (y in seq_len(max(0L, last - latest))) {
    yr <- latest + y
    out$value[out$fy == yr] <- out$value[out$fy == yr - 1L] * tail_rate
  }
  # A year is `projected` when it sits outside the published data, and also
  # when the source table itself flagged it as an assumption rather than a
  # published figure.
  assumed <- rep(FALSE, nrow(out))
  if (!is.null(level$assumed)) {
    assumed <- level$assumed[match(out$fy, level$fy)]
    assumed[is.na(assumed)] <- TRUE
  }
  out$projected <- !(out$fy %in% observed) | assumed
  out[!is.na(out$value), , drop = FALSE]
}

.rebase <- function(level, at, value_at = 1) {
  base <- level$value[level$fy == at]
  stopifnot("rebase year is outside the series" = length(base) == 1L)
  level$value <- level$value / base * value_at
  level
}

# The calendar-year value of a financial-year series is the mean of the two
# financial years that straddle it: calendar 2021 is half of 2020-21 and half
# of 2021-22. Series with quarterly source data could be averaged directly, but
# doing it the same way for every series keeps the financial-year and
# calendar-year tables mutually consistent, which matters because a person's
# Census income band and their tax return have to agree.
.calendar_from_financial <- function(level) {
  years <- level$fy
  out <- data.frame(fy = years, value = NA_real_, projected = level$projected)
  prev <- level$value[match(years - 1L, years)]
  out$value <- ifelse(is.na(prev), level$value, (prev + level$value) / 2)
  out$projected <- level$projected |
    ifelse(is.na(match(years - 1L, years)), TRUE,
           level$projected[match(years - 1L, years)])
  out
}

message("Fetching ABS series")
abs_raw <- lapply(names(.abs_series), function(nm) .fetch_abs(nm, .abs_series[[nm]]))
names(abs_raw) <- names(.abs_series)

message("Fetching ATO taxation statistics")
ato_ind <- .read_ato_table_1a(.download(.ato_individuals_url, "ato-individuals-t1.xlsx"))
ato_co <- .read_ato_table_1a(.download(.ato_company_url, "ato-company-t1.xlsx"))

# ---- the five headline levels, on a financial-year basis -------------------

fy_levels <- list(
  price = .to_financial_year(abs_raw$cpi, "Q"),
  wage = .ato_ratio(ato_ind, "Salary or wages"),
  business = .ato_ratio(ato_co, "Total income3"),
  transfer = .to_financial_year(abs_raw$mtawe, "S"),
  health = .pct_to_level(.read_manual("mbs"))
)

# Reference series that are reported alongside the five but not used directly
# by the generators. They are in the CSV so the choice of headline can be
# checked, and so a user can index to something else if they want to.
fy_reference <- list(
  cpi = .to_financial_year(abs_raw$cpi, "Q"),
  wpi = .to_financial_year(abs_raw$wpi, "Q"),
  awote = .to_financial_year(abs_raw$awote, "S"),
  mtawe = .to_financial_year(abs_raw$mtawe, "S"),
  gdp_nominal = .to_financial_year(abs_raw$gdp, "Q", aggregate = "sum"),
  ato_avg_salary_wages = .ato_ratio(ato_ind, "Salary or wages"),
  ato_avg_taxable_income = .ato_ratio(ato_ind, "Taxable income (not loss)"),
  ato_company_avg_income = .ato_ratio(ato_co, "Total income3"),
  ato_company_avg_wages = .ato_ratio(ato_co, "Total salary and wage expenses")
)

# The wage series must be extended past the last ATO income year, which lags by
# about two years. It is extended on average growth like the others, but the
# projected flag makes clear that the recent tail is not an ATO figure.
build_index <- function(level) {
  extended <- .extend(level, .first_fy, .last_fy)
  cy <- .calendar_from_financial(extended)
  cy_base <- cy$value[cy$fy == .anchor_cy]
  stopifnot("anchor year is missing" = length(cy_base) == 1L)
  list(
    fy = transform(extended, value = value / cy_base),
    cy = transform(cy, value = value / cy_base)
  )
}

indices <- lapply(fy_levels, build_index)
reference <- lapply(fy_reference, build_index)

# ---- checks ---------------------------------------------------------------

for (nm in names(indices)) {
  idx <- indices[[nm]]$fy
  missing_years <- setdiff(.first_fy:.last_fy, idx$fy)
  if (length(missing_years)) {
    stop("the ", nm, " index does not cover ",
         paste(missing_years, collapse = ", "), call. = FALSE)
  }
  stopifnot("index must be strictly positive" = all(idx$value > 0))
  growth <- diff(idx$value) / utils::head(idx$value, -1L)
  years <- utils::tail(idx$fy, -1L)
  # Prices, wages and the pension benchmark did fall in a couple of years --
  # the CPI in 1997-98, and again in 2019-20 when child care was made free --
  # but never by much, and a large fall means the wrong row was read. Company
  # income genuinely drops in a downturn, and the MBS indexation factor is zero
  # through the freeze, so those two are only checked for not collapsing.
  floor_growth <- if (nm %in% c("business", "health")) -0.10 else -0.02
  bad <- which(growth < floor_growth | growth > 0.25)
  if (length(bad)) {
    stop("implausible annual growth in the ", nm, " index: ",
         paste(sprintf("%d %+.1f%%", years[bad], 100 * growth[bad]),
               collapse = ", "),
         call. = FALSE)
  }
}

# The generated data must not accidentally claim more or less inflation than
# Australia had. Over 2005-06 to 2023-24 the CPI roughly rose 60 per cent and
# average wages on tax returns roughly doubled; a build that breaks either of
# these has broken the series, not just the formatting.
.span <- function(idx, from, to) {
  idx$value[idx$fy == to] / idx$value[idx$fy == from]
}
stopifnot(
  "CPI over 2005-06 to 2023-24 is outside the expected range" =
    abs(.span(indices$price$fy, 2005L, 2023L) - 1.62) < 0.10,
  "average wage over 2005-06 to 2023-24 is outside the expected range" =
    abs(.span(indices$wage$fy, 2005L, 2023L) - 1.94) < 0.12
)

# ---- write the CSV --------------------------------------------------------

tidy <- function(named, role) {
  do.call(rbind, lapply(names(named), function(nm) {
    rbind(
      data.frame(series = nm, role = role, basis = "financial_year",
                 year = named[[nm]]$fy$fy, index = named[[nm]]$fy$value,
                 projected = named[[nm]]$fy$projected,
                 stringsAsFactors = FALSE),
      data.frame(series = nm, role = role, basis = "calendar_year",
                 year = named[[nm]]$cy$fy, index = named[[nm]]$cy$value,
                 projected = named[[nm]]$cy$projected,
                 stringsAsFactors = FALSE)
    )
  }))
}

out_csv <- rbind(tidy(indices, "headline"), tidy(reference, "reference"))
out_csv$index <- round(out_csv$index, 6L)
out_csv <- out_csv[order(out_csv$role, out_csv$series, out_csv$basis,
                         out_csv$year), , drop = FALSE]

csv_path <- file.path(.repo_root, "inst", "extdata", "nominal-indices.csv")
dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out_csv, csv_path, row.names = FALSE, na = "")
message("Wrote ", nrow(out_csv), " rows to ", csv_path)

# ---- write the Rust table -------------------------------------------------

.rust_rows <- function(basis) {
  years <- .first_fy:.last_fy
  cols <- c("wage", "price", "business", "transfer", "health")
  vapply(years, function(y) {
    vals <- vapply(cols, function(nm) {
      tbl <- indices[[nm]][[if (basis == "financial_year") "fy" else "cy"]]
      tbl$value[tbl$fy == y]
    }, numeric(1))
    projected <- any(vapply(cols, function(nm) {
      tbl <- indices[[nm]][[if (basis == "financial_year") "fy" else "cy"]]
      tbl$projected[tbl$fy == y]
    }, logical(1)))
    sprintf(
      "    NominalRow { year: %d, wage: %.8f, price: %.8f, business: %.8f, transfer: %.8f, health: %.8f, projected: %s },",
      y, vals[["wage"]], vals[["price"]], vals[["business"]],
      vals[["transfer"]], vals[["health"]], if (projected) "true" else "false"
    )
  }, character(1))
}

n_years <- length(.first_fy:.last_fy)
rs_path <- file.path(.repo_root, "src", "rust", "src", "nominal_series.rs")
writeLines(
  c(
    "// AUTO-GENERATED by data-raw/update_nominal_indices.R -- do not edit by hand.",
    "//",
    "// Headline nominal indices, expressed relative to calendar 2021 = 1.0, which",
    "// is the year the fplida spine draws each person's baseline income at.",
    "//",
    "// Sources, per column:",
    "//   wage      ATO Taxation statistics, Individuals Table 1A: total salary or",
    "//             wages / individuals reporting salary or wages.",
    "//   price     ABS Consumer Price Index, All groups, Australia, original.",
    "//   business  ATO Taxation statistics, Company Table 1A: total income /",
    "//             number of companies.",
    "//   transfer  ABS Average Weekly Earnings, full-time adult total earnings,",
    "//             males (MTAWE), the Age Pension benchmark.",
    "//   health    Medicare Benefits Schedule annual indexation factor,",
    "//             data-raw/nominal-sources/administered-indexation.csv.",
    "//",
    "// `projected` marks a year outside the published data, filled by compounding",
    "// the trailing ten-year average growth of the series.",
    "",
    "#[derive(Debug, Clone, Copy)]",
    "pub struct NominalRow {",
    "    pub year: i32,",
    "    pub wage: f64,",
    "    pub price: f64,",
    "    pub business: f64,",
    "    pub transfer: f64,",
    "    pub health: f64,",
    "    pub projected: bool,",
    "}",
    "",
    sprintf("/// First year in both tables (a financial year is named by the July it starts in)."),
    sprintf("pub const FIRST_YEAR: i32 = %d;", .first_fy),
    sprintf("/// Last year in both tables."),
    sprintf("pub const LAST_YEAR: i32 = %d;", .last_fy),
    sprintf("/// The calendar year every index is 1.0 in."),
    sprintf("pub const ANCHOR_YEAR: i32 = %d;", .anchor_cy),
    "",
    "/// Financial-year basis: 2011 means the year ended 30 June 2012.",
    sprintf("pub const FINANCIAL_YEAR: [NominalRow; %d] = [", n_years),
    .rust_rows("financial_year"),
    "];",
    "",
    "/// Calendar-year basis.",
    sprintf("pub const CALENDAR_YEAR: [NominalRow; %d] = [", n_years),
    .rust_rows("calendar_year"),
    "];",
    ""
  ),
  rs_path
)
message("Wrote ", n_years, " years to ", rs_path)

# ---- a readable summary for the commit message ----------------------------

summary_tbl <- data.frame(
  fy = indices$price$fy$fy,
  price = round(100 * c(NA, diff(indices$price$fy$value) /
                          utils::head(indices$price$fy$value, -1L)), 2),
  wage = round(100 * c(NA, diff(indices$wage$fy$value) /
                         utils::head(indices$wage$fy$value, -1L)), 2),
  business = round(100 * c(NA, diff(indices$business$fy$value) /
                             utils::head(indices$business$fy$value, -1L)), 2),
  transfer = round(100 * c(NA, diff(indices$transfer$fy$value) /
                             utils::head(indices$transfer$fy$value, -1L)), 2),
  health = round(100 * c(NA, diff(indices$health$fy$value) /
                           utils::head(indices$health$fy$value, -1L)), 2)
)
message("\nAnnual growth, per cent, financial year:")
print(summary_tbl[summary_tbl$fy >= 2005L & summary_tbl$fy <= 2025L, ],
      row.names = FALSE)
