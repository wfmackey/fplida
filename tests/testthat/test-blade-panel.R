# The time dimension of the BLADE tables: no table may emit a `tsid` outside
# its own declared periods, and the tables whose grain is one row per business
# per period must be panels rather than single-period snapshots.

.blade_panel_declared_tsids <- function(table_number) {
  fplida:::.blade_declared_tsids(table_number)
}

test_that("every table's declared periods come from its own metadata", {
  tables <- fplida:::.blade_tables()

  for (table_number in tables[["Table.Number"]]) {
    declared <- fplida:::.blade_declared_periods(table_number)
    expect_gt(length(declared), 0L)
    # Either the variables name the periods, or the table's reference range is
    # expanded year by year; both give a period every end year can be read from.
    years <- vapply(declared, fplida:::.blade_period_end_year, integer(1),
                    USE.NAMES = FALSE)
    expect_false(any(is.na(years)),
                 info = paste("table", table_number,
                              "has an unreadable period"))
  }

  # Table 5's own range, as published: 2001-02 to 2024-25.
  expect_equal(length(fplida:::.blade_declared_periods(5L)), 24L)
  expect_equal(min(fplida:::.blade_declared_periods(5L)), "2001-02")
  expect_equal(max(fplida:::.blade_declared_periods(5L)), "2024-25")

  # Table 29 leaves Available.Periods empty, so the reference range stands in.
  expect_equal(length(fplida:::.blade_declared_periods(29L)), 121L)
  expect_true("1904-05" %in% fplida:::.blade_declared_periods(29L))
  expect_true("2024-25" %in% fplida:::.blade_declared_periods(29L))
})

test_that("no table resolves a tsid outside its own declared periods", {
  tables <- fplida:::.blade_tables()
  offenders <- character(0)

  for (table_number in tables[["Table.Number"]]) {
    got <- fplida:::.blade_tsid(table_number)
    if (!got %in% .blade_panel_declared_tsids(table_number)) {
      offenders <- c(offenders, paste0("table ", table_number, " -> ", got))
    }
    # A pin the table does not declare must be refused, not trusted.
    forced <- fplida:::.blade_tsid(table_number, "1066-67")
    expect_true(forced %in% .blade_panel_declared_tsids(table_number),
                info = paste("table", table_number, "accepted a foreign pin"))
  }

  expect_equal(offenders, character(0))

  # The specific leak this invariant closes: table 5 used to borrow table 1's
  # period, which put 2025-26 into a table whose range stops at 2024-25.
  expect_equal(fplida:::.blade_tsid(5L), "25")
  expect_equal(fplida:::.blade_tsid(1L), "26")
})

test_that("panel tables declare a grain judgement and a bounded expansion", {
  # Every table carrying a tsid is either expanded or deliberately not; the
  # panel set is the record of that judgement and must stay inside the tables
  # BLADE actually publishes.
  tables <- fplida:::.blade_tables()
  expect_true(all(fplida:::.BLADE_PANEL_TABLES %in% tables[["Table.Number"]]))
  expect_equal(fplida:::.BLADE_PANEL_TABLES, c(1L, 2L, 3L, 5L))

  for (table_number in fplida:::.BLADE_PANEL_TABLES) {
    periods <- fplida:::.blade_panel_periods(table_number)
    expect_gt(length(periods), 1L)
    expect_setequal(periods, fplida:::.blade_declared_periods(table_number))
    # Oldest first, so a stacked frame reads forwards in time.
    years <- vapply(periods, fplida:::.blade_period_end_year, integer(1),
                    USE.NAMES = FALSE)
    expect_false(is.unsorted(years))
  }

  # A table not in the set is generated once, whatever its metadata declares.
  expect_equal(fplida:::.blade_panel_periods(4L), character(0))
  expect_equal(fplida:::.blade_panel_periods(29L), character(0))
})

test_that("a panel drops a business outside its operating window", {
  rows <- data.frame(
    bn = sprintf("BN%011d", 1:4),
    business_birth_year = c(2010L, 2010L, NA_integer_, 1995L),
    business_exit_year = c(NA_integer_, 2015L, 2008L, NA_integer_),
    stringsAsFactors = FALSE
  )

  expect_equal(fplida:::.blade_panel_active_rows(rows, 2008L),
               c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(fplida:::.blade_panel_active_rows(rows, 2011L),
               c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(fplida:::.blade_panel_active_rows(rows, 2016L),
               c(TRUE, FALSE, FALSE, TRUE))
})

test_that("PAYG is a panel over its own periods with a moving headcount", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_blade_panel_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 600L, seed = 31L, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  business_spine <- generate_blade_business_spine(
    spine = spine, seed = 31L, n_businesses = 180L,
    output_dir = tmp, return_data = TRUE
  )
  frames <- generate_blade(
    business_spine = business_spine, spine = spine, seed = 31L,
    output_dir = tmp, tables = c(1L, 4L, 5L), return_data = TRUE
  )

  payg <- frames[["blade-table-05-pay-as-you-go-payg"]]
  table1 <- frames[["blade-table-01-cross-sectional-indicative-data-items"]]
  bas <- frames[["blade-table-04-business-activity-statement-bas"]]

  # The panel shape: many periods, one row per business per period, and every
  # period one the table itself declares.
  declared <- .blade_panel_declared_tsids(5L)
  expect_equal(length(unique(payg$tsid)), 24L)
  expect_setequal(unique(payg$tsid), declared)
  expect_false("26" %in% payg$tsid)
  expect_equal(min(payg$tsid), "02")
  expect_equal(max(payg$tsid), "25")
  expect_equal(sum(duplicated(payg[c("bn", "tsid")])), 0L)
  expect_gt(nrow(payg), nrow(business_spine))
  expect_true(all(payg$bn %in% business_spine$bn))

  # Table 4 keeps its business-quarter grain and stays a single-period table,
  # so a table carrying a tsid is not on its own expanded.
  expect_equal(length(unique(bas$tsid)), 1L)
  expect_equal(nrow(bas), nrow(business_spine))

  # No business-period precedes the business or follows it.
  end_year <- function(ts) {
    y <- as.integer(ts)
    ifelse(y > 50L, 1900L + y, 2000L + y)
  }
  window <- merge(
    payg,
    business_spine[c("bn", "business_birth_year", "business_exit_year")],
    by = "bn"
  )
  window$year <- end_year(window$tsid)
  expect_equal(sum(!is.na(window$business_birth_year) &
                     window$year < window$business_birth_year + 1L), 0L)
  expect_equal(sum(!is.na(window$business_exit_year) &
                     window$year > window$business_exit_year), 0L)

  # `. = No PAYG data` is documented for both employment items, so the empty
  # case has to occur, on both items together and on a small share of rows.
  expect_gt(sum(is.na(payg$fte)), 0L)
  expect_identical(is.na(payg$fte), is.na(payg$hcnt))
  expect_lt(mean(is.na(payg$fte)), 0.1)

  # Employment moves with the business rather than being redrawn: headcounts
  # vary within a business, and the full-time ratio does not.
  present <- payg[!is.na(payg$hcnt), ]
  by_business <- split(present$hcnt, present$bn)
  by_business <- by_business[lengths(by_business) >= 10L]
  expect_gt(length(by_business), 0L)
  varies <- vapply(by_business, function(x) length(unique(x)) > 1L, logical(1))
  expect_gt(mean(varies), 0.5)
  ratio <- present$fte[present$hcnt > 0] / present$hcnt[present$hcnt > 0]
  expect_true(all(ratio >= 0.6 & ratio <= 0.95))

  # Table 1 is a panel over its own twenty-five periods, so PAYG still joins to
  # the business register on the pair a user would reach for.
  expect_equal(length(unique(table1$tsid)), 25L)
  expect_setequal(unique(table1$tsid), .blade_panel_declared_tsids(1L))
  expect_gt(nrow(merge(payg, table1, by = c("bn", "tsid"))), 0L)
})

test_that("a whole build keeps every tsid inside its table's own periods", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_blade_tsid_range_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 300L, seed = 33L, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  frames <- generate_blade(
    spine = spine, seed = 33L, output_dir = tmp, tables = "all",
    max_rows = 60L, return_data = TRUE
  )

  tables <- fplida:::.blade_tables()
  checked <- 0L
  offenders <- character(0)

  for (i in seq_len(nrow(tables))) {
    product_name <- tables[["Product.Name"]][i]
    frame <- frames[[product_name]]
    if (is.null(frame) || !"tsid" %in% names(frame)) next
    checked <- checked + 1L
    outside <- setdiff(unique(as.character(frame$tsid)),
                       .blade_panel_declared_tsids(tables[["Table.Number"]][i]))
    if (length(outside)) {
      offenders <- c(offenders, paste0(product_name, " -> ",
                                       paste(outside, collapse = ",")))
    }
  }

  # Fifty-four of the sixty-two tables carry a tsid.
  expect_equal(checked, 54L)
  expect_equal(offenders, character(0))
})
