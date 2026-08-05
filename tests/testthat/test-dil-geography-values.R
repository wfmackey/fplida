test_that("official LGA codeframe includes the required vintages", {
  values <- fplida:::.dil_load_lga_codeframe()
  expect_setequal(
    unique(values$year),
    c(2011L, 2016L, 2021L, 2022L, 2023L, 2024L)
  )
  expect_true(all(table(values$year) > 500L))
  expect_true(all(grepl("^[1-8][0-9]{4}$", values$code)))
})

test_that("LGA values are valid, linked, and vintage-specific", {
  spine <- data.frame(
    id = 1:80,
    state = rep(1:8, each = 10),
    stringsAsFactors = FALSE
  )
  period <- list(end_year = 2024L)
  codes_2021 <- fplida:::.dil_lga_value(
    "LGA_2021", spine, 20260803L, period
  )
  names_2021 <- fplida:::.dil_lga_value(
    "LGA_NAME_2021", spine, 20260803L, period
  )
  values <- fplida:::.dil_load_lga_codeframe()
  frame_2021 <- values[values$year == 2021L, , drop = FALSE]
  matched <- match(codes_2021, frame_2021$code)

  expect_false(anyNA(matched))
  expect_identical(names_2021, frame_2021$name[matched])
  expect_identical(substr(codes_2021, 1L, 1L), as.character(spine$state))
  expect_identical(
    codes_2021,
    fplida:::.dil_lga_value("LGA_2021", spine, 20260803L, period)
  )
  expect_true(all(fplida:::.dil_lga_value(
    "LGAPUBLIC", spine, 20260803L, period
  ) == 1L))
})

test_that("LGA names select the stated DIL vintage", {
  period <- list(end_year = 2024L)
  expect_identical(
    fplida:::.dil_lga_reference_year("LGA_UR_11", period), 2011L
  )
  expect_identical(
    fplida:::.dil_lga_reference_year("LGA22D", period), 2022L
  )
  expect_identical(
    fplida:::.dil_lga_reference_year("LGA_CODE_2023", period), 2023L
  )
  expect_identical(
    fplida:::.dil_lga_reference_year("LGA_CODE_2024", period), 2024L
  )
})

test_that("Indigenous Region and PHN codes use official linked domains", {
  spine <- data.frame(
    id = 1:80,
    state = rep(1:8, each = 10),
    stringsAsFactors = FALSE
  )
  ireg <- fplida:::.dil_ireg_value(
    "IREGCODE", spine, 20260803L
  )
  ireg_name <- fplida:::.dil_ireg_value(
    "IREGNAME", spine, 20260803L
  )
  ireg_values <- fplida:::.dil_area_codeframe("ireg.tsv", 100L)
  ireg_values <- ireg_values[ireg_values$year == 2021L, , drop = FALSE]
  expect_false(anyNA(match(ireg, ireg_values$code)))
  expect_identical(ireg_name, ireg_values$name[match(ireg, ireg_values$code)])

  phn <- fplida:::.dil_phn_value("PHNCODE", spine, 20260803L)
  phn_name <- fplida:::.dil_phn_value("PHNNAME", spine, 20260803L)
  phn_values <- fplida:::.dil_area_codeframe("phn.tsv", 31L)
  expect_false(anyNA(match(phn, phn_values$code)))
  expect_identical(phn_name, phn_values$name[match(phn, phn_values$code)])
  expect_identical(substr(phn, 4L, 4L), as.character(spine$state))
})
