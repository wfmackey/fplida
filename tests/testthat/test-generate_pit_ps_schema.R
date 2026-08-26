# The payment-summary schema changes by financial year, from four variables
# in 2001-02 to thirty-six in 2022-23, and the generator once wrote the same
# fourteen invented column names for every year. These hold the emitted
# tables to the bundled data item list, table by table, so the two cannot
# drift apart again in silence.

pit_ps_schema_fixture <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    out <- file.path(tempdir(), "pit_ps_schema")
    if (dir.exists(out)) unlink(out, recursive = TRUE)
    dir.create(out, recursive = TRUE)
    suppressMessages({
      generate_spine(n = 1500L, seed = 33L, output_dir = out)
      # BLADE first, so the employer identifiers are real business numbers
      # and the correspondence key exists to check them against.
      generate_core(seed = 33L, output_dir = out, years = 2015:2023)
      generate_blade(seed = 33L, output_dir = out)
      result <- generate_pit_ps(seed = 33L, output_dir = out)
    })
    run <- getOption("fplida.run_dir")
    cache <<- list(out = out, run = run,
                   ps = file.path(run, "ato-pit_ps"),
                   blade = file.path(run, "abs-blade"),
                   result = result)
    cache
  }
})

pit_ps_table_path <- function(fixture, stem) {
  file.path(fixture$ps, paste0(stem, ".parquet"))
}


test_that("the data item list gives PIT_PS 22 products over 31 tables", {
  plan <- fplida:::.pit_ps_structures()
  expect_identical(length(unique(plan$product)), 22L)
  expect_identical(nrow(plan), 31L)
  expect_identical(sort(unique(plan$fy)), 2002L:2023L)
})


test_that("every product name is the one the registry publishes", {
  products <- utils::read.csv(
    fplida:::.dil_metadata_path("products.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  published <- products[["Product Name"]][products$Dataset == "PIT_PS"]

  # The module name is the bare string "Payment Summaries" for every year, so
  # a lookup keyed on a year-bearing module string matches nothing. This is
  # the check that caught that.
  expect_identical(
    unique(products[["Module Name"]][products$Dataset == "PIT_PS"]),
    "Payment Summaries"
  )

  resolved <- vapply(2002:2023, fplida:::pit_ps_product_name, character(1))
  expect_false(anyNA(resolved))
  expect_identical(sort(resolved), sort(published))
  expect_identical(length(unique(resolved)), 22L)

  # The nine products the ATO delivered individually keep their own spelling
  # rather than the collection's.
  expect_identical(
    fplida:::pit_ps_product_name(2011L),
    "madip-ge-020101d-ato-atopaysum1011-fy2010-11"
  )
  expect_identical(
    fplida:::pit_ps_product_name(2023L),
    "madipge-ato-d-pay-sum-fy2223"
  )
})


test_that("a financial year outside the delivery writes nothing", {
  skip_if_not_installed("arrow")
  skip_on_cran()
  fixture <- pit_ps_schema_fixture()

  before <- list.files(fixture$ps)
  for (year in c(2001L, 2024L)) {
    result <- suppressMessages(
      generate_pit_ps(seed = 33L, years = year, output_dir = fixture$out)
    )
    expect_identical(result$n_records, 0L)
    expect_length(result$years, 0L)
  }
  expect_identical(sort(list.files(fixture$ps)), sort(before))

  # A request mixing covered and uncovered years keeps the covered ones.
  result <- suppressMessages(
    generate_pit_ps(seed = 33L, years = c(2001L, 2011L, 2024L),
                    output_dir = fixture$out)
  )
  expect_identical(result$years, 2011L)
})


test_that("every declared table is written with exactly its declared variables", {
  skip_if_not_installed("arrow")
  skip_on_cran()
  fixture <- pit_ps_schema_fixture()
  plan <- fplida:::.pit_ps_structures()

  expect_identical(fixture$result$years, 2002L:2023L)

  for (i in seq_len(nrow(plan))) {
    path <- pit_ps_table_path(fixture, plan$stem[i])
    expect_true(file.exists(path), info = plan$stem[i])
    emitted <- names(arrow::open_dataset(path, format = "parquet"))
    declared <- plan$variables[[i]]
    expect_identical(setdiff(emitted, declared), character(0),
                     info = paste(plan$table[i], "has undeclared columns"))
    expect_identical(setdiff(declared, emitted), character(0),
                     info = paste(plan$table[i], "is missing columns"))
  }
})


test_that("the schema changes by year the way the registry says it does", {
  skip_if_not_installed("arrow")
  skip_on_cran()
  fixture <- pit_ps_schema_fixture()

  widths <- c(
    "madipge-ato-d-pay-sum-fy0102--ato_pay_sum_0102_16m" = 4L,
    "madipge-ato-d-pay-sum-fy0405--ato_pay_sum_0405_16m" = 5L,
    "madip-ge-020101d-ato-atopaysum1011-fy2010-11--ato_pay_sum_1011" = 21L,
    "madipge-ato-d-pay-sum-fy2223--ato_pay_sum_2223_12m" = 36L
  )
  for (stem in names(widths)) {
    emitted <- names(arrow::open_dataset(
      pit_ps_table_path(fixture, stem), format = "parquet"
    ))
    expect_identical(length(emitted), widths[[stem]], info = stem)
  }

  # None of the invented names the generator used to write survive anywhere.
  invented <- c("FINANCIAL_YEAR", "EMPLOYER_ABN", "GROSS_PAYMENTS",
                "TAX_WITHHELD", "REPORTABLE_FBT", "SUPER_GUARANTEE",
                "ALLOWANCES", "LUMP_SUM_A", "LUMP_SUM_B", "LUMP_SUM_D",
                "LUMP_SUM_E", "UNION_FEES", "WORKPLACE_GIVING")
  plan <- fplida:::.pit_ps_structures()
  emitted <- unlist(lapply(plan$stem, function(stem) {
    names(arrow::open_dataset(pit_ps_table_path(fixture, stem),
                              format = "parquet"))
  }))
  expect_identical(intersect(unique(emitted), invented), character(0))
})


test_that("the employer key follows the registry, per table and not per year", {
  skip_if_not_installed("arrow")
  skip_on_cran()
  fixture <- pit_ps_schema_fixture()
  plan <- fplida:::.pit_ps_structures()

  # 2021-22 is the mixed year: the six-month extract still keys on the hash
  # while the sixteen-month re-extract keys on the business number.
  mixed <- plan[plan$fy == 2022L, ]
  expect_identical(
    mixed$key_var[mixed$table == "ato_pay_sum_2122_6m"], "ABN_HASH_TRUNC"
  )
  expect_identical(
    mixed$key_var[mixed$table == "ato_pay_sum_2122_16m"], "BN"
  )

  for (i in seq_len(nrow(plan))) {
    emitted <- names(arrow::open_dataset(
      pit_ps_table_path(fixture, plan$stem[i]), format = "parquet"
    ))
    keys <- intersect(c("BN", "ABN_HASH_TRUNC"), emitted)
    expect_identical(keys, if (nzchar(plan$key_var[i])) plan$key_var[i]
                     else character(0),
                     info = plan$table[i])
  }
})


test_that("both employer identifiers bridge to a BLADE business", {
  skip_if_not_installed("arrow")
  skip_on_cran()
  fixture <- pit_ps_schema_fixture()
  plan <- fplida:::.pit_ps_structures()

  key <- as.data.frame(arrow::read_parquet(
    file.path(fixture$blade, "blade-key-abn-hash-trunc-to-bn-key.parquet")
  ))
  expect_gt(nrow(key), 0L)

  read_key_column <- function(column) {
    rows <- plan[plan$key_var == column, ]
    unlist(lapply(rows$stem, function(stem) {
      as.data.frame(arrow::read_parquet(
        pit_ps_table_path(fixture, stem), col_select = dplyr::all_of(column)
      ))[[column]]
    }))
  }

  hashed <- read_key_column("ABN_HASH_TRUNC")
  expect_gt(length(hashed), 0L)
  expect_true(all(hashed %in% key$abn_hash_trunc))
  # The unprefixed 12-character uppercase hex the ATO products to 2021-22
  # publish, and never confusable with the BN-prefixed value beside it.
  expect_true(all(grepl("^[0-9A-F]{12}$", hashed)))

  business_number <- read_key_column("BN")
  expect_gt(length(business_number), 0L)
  expect_true(all(business_number %in% key$bn))
  expect_true(all(startsWith(business_number, "BN")))
})


test_that("the ages, the geography and the components agree with the spine", {
  skip_if_not_installed("arrow")
  skip_on_cran()
  fixture <- pit_ps_schema_fixture()

  frame <- as.data.frame(arrow::read_parquet(pit_ps_table_path(
    fixture, "madipge-ato-d-pay-sum-fy2223--ato_pay_sum_2223_12m"
  )))
  spine <- as.data.frame(arrow::read_parquet(
    file.path(fixture$run, "_system", "base-spine.parquet")
  ))
  spine <- spine[match(frame$SYNTHETIC_AEUID, spine$aeuid_ato), , drop = FALSE]

  # Age at 30 June is the age at the 1 July that opened the year, plus one.
  expect_identical(frame$AGE2223_END, frame$AGE2223_START + 1L)
  expected_age <- ifelse(spine$month_of_birth <= 6L,
                         2022L - spine$birth_year,
                         2021L - spine$birth_year)
  expect_identical(frame$AGE2223_START,
                   pmin(pmax(expected_age, 0L), 115L))

  # ASGS codes nest. Some people are coded to an area but not to an address,
  # which is what an SA1 and a mesh block of NA mean here, so the nesting is
  # checked where there is an address.
  addressed <- !is.na(frame$SA1)
  expect_gt(mean(addressed), 0.8)
  expect_identical(substr(frame$SA1[addressed], 1L, 9L),
                   sprintf("%09d", frame$SA2[addressed]))
  expect_identical(frame$SA3, frame$SA2 %/% 10000L)
  expect_identical(sprintf("%03d", frame$SA4),
                   substr(sprintf("%09d", frame$SA2), 1L, 3L))
  expect_identical(as.character(frame$STE),
                   substr(sprintf("%09d", frame$SA2), 1L, 1L))

  # The address is the person's own, carried through the shared spine
  # resolution rather than drawn here, so it agrees with the spine except
  # where the ATO has not caught up with a move. LGA follows the state of the
  # address the ATO holds, not the state the spine has moved them to.
  expect_gt(mean(frame$STE == as.integer(spine$state)), 0.9)
  expect_identical(substr(frame$LGA, 1L, 1L), as.character(frame$STE))

  # The taxable and tax-free components sum to the gross rather than being
  # drawn beside it.
  expect_equal(frame$TOTL_TXBL_AMT + frame$TAX_FREE_AMT, frame$GRS_AMT,
               tolerance = 1e-6)

  # The reported period sits inside the income year.
  expect_true(all(frame$PERD_STRT_DT >= as.Date("2022-07-01")))
  expect_true(all(frame$PERD_END_DT <= as.Date("2023-06-30")))
  expect_true(all(frame$PERD_END_DT >= frame$PERD_STRT_DT))
  expect_identical(unique(frame$INCM_YR), 2023L)
  expect_identical(unique(frame$FIN_YEAR), "2022-23")
  expect_identical(unique(frame$EXTRACT_REF), "FY2022-23")
})


test_that("a six-month extract sees fewer summaries than the full year", {
  skip_if_not_installed("arrow")
  skip_on_cran()
  fixture <- pit_ps_schema_fixture()

  count_rows <- function(stem) {
    nrow(as.data.frame(arrow::read_parquet(
      pit_ps_table_path(fixture, stem),
      col_select = dplyr::all_of("SYNTHETIC_AEUID")
    )))
  }
  full <- count_rows("madipge-ato-d-pay-sum-fy2223--ato_pay_sum_2223_12m")
  early <- count_rows("madipge-ato-d-pay-sum-fy2223--ato_pay_sum_2223_6m")
  expect_lt(early, full)
  expect_gt(early, 0.7 * full)

  # A geography table names each person once, not once per payment summary.
  people <- count_rows("madip-ge-020126d-ato-atopaysum1516-fy2015-16--ps_geo2021_1516")
  summaries <- count_rows("madip-ge-020126d-ato-atopaysum1516-fy2015-16--ato_pay_sum_1516")
  expect_lt(people, summaries)
})
