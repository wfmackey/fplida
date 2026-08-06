test_that("DIL inventory retains all exact product-table structures", {
  inventory <- fplida:::.dil_structure_inventory()
  structures <- inventory$structures

  expect_equal(nrow(structures), 2140L)
  expect_equal(length(unique(structures[["Product Name"]])), 559L)
  expect_equal(length(unique(structures$Dataset)), 43L)
  expect_equal(sum(structures$Dataset == "CENSUS"), 9L)
  expect_equal(sum(structures$Dataset == "BLADE"), 0L)

  stp <- structures[structures$Dataset == "STP", , drop = FALSE]
  hyphen <- fplida:::.dil_structure_stem(
    "pmp-stp-extended", "stp_extended_etp_2019-20"
  )
  underscore <- fplida:::.dil_structure_stem(
    "pmp-stp-extended", "stp_extended_etp_2019_20"
  )
  expect_false(identical(hyphen, underscore))
  expect_equal(nrow(stp), 182L)

  ndis <- fplida:::.dil_structure_inventory("NDIS")
  expect_equal(nrow(ndis$structures), 134L)
  expect_equal(nrow(ndis$variables), 2647L)
  travellers <- fplida:::.dil_structure_inventory("TRAVELLERS")
  expect_equal(nrow(travellers$structures), 35L)
  expect_equal(nrow(travellers$variables), 1319L)
  traveller_names <- unique(travellers$variables[["Variable Name"]])
  expect_equal(sum(grepl(
    "^ERP_STATUS_[0-9]{4}Q[1-4]$", traveller_names, perl = TRUE
  )), 74L)
  expect_equal(sum(grepl(
    "^PP_STATUS_[0-9]{4}M(?:[1-9]|1[0-2])$",
    traveller_names, perl = TRUE
  )), 240L)
  expect_equal(sum(grepl(
    "^PP_WT_[0-9]{4}M(?:[1-9]|1[0-2])$",
    traveller_names, perl = TRUE
  )), 240L)
})

test_that("TRAVELLERS wide residence fields use the shared monthly model", {
  spine <- data.frame(
    id = 1:6,
    birth_year = c(1980L, 2018L, 1990L, 1990L, 1985L, 1985L),
    month_of_birth = c(1L, 7L, 5L, 6L, 1L, 1L),
    country_of_birth = c(0L, 0L, 1L, 1L, 0L, 0L),
    country_of_birth_sacc = c(1101L, 1101L, 2100L, 2100L, 1101L, 1101L),
    year_of_arrival = c(NA, NA, 2015L, 2010L, NA, NA),
    year_of_death = rep(NA_integer_, 6L),
    month_of_death = rep(NA_integer_, 6L),
    day_of_death = rep(NA_integer_, 6L),
    residence_seed = 101L:106L
  )
  traveller_value <- function(name, description = name) {
    fplida:::.dil_general_value(
      name = name,
      description = description,
      dataset = "TRAVELLERS",
      product_name = "madipge-mig-d-pp-2006-current",
      table_name = "madipge_mig_d_pp_2006_current",
      module_name = "Travellers 2006 - latest",
      spine_rows = spine,
      seed = 20260803L
    )
  }

  weight <- traveller_value("PP_WT_2014M6")
  pp_status <- traveller_value("PP_STATUS_2014M6")
  erp_status <- traveller_value("ERP_STATUS_2014Q2")

  expect_type(weight, "double")
  expect_type(pp_status, "integer")
  expect_type(erp_status, "integer")
  expect_true(all(weight >= 0 & weight <= 1))
  expect_true(all(pp_status %in% c(1L, 3L)))
  expect_true(all(erp_status %in% 1:3))
  expect_identical(pp_status, ifelse(weight > 0, 1L, 3L))
  expect_true(any(weight > 0))

  # The second person is not born and the third has not arrived by 2014.
  expect_identical(weight[c(2L, 3L)], c(0, 0))
  expect_identical(pp_status[c(2L, 3L)], c(3L, 3L))
  expect_identical(erp_status[c(2L, 3L)], c(3L, 3L))
  expect_identical(weight, traveller_value("PP_WT_2014M6"))

  census <- traveller_value("CENSUS_2016")
  erp_q3 <- traveller_value("ERP_STATUS_2016Q3")
  pp_census <- traveller_value("PP_CENSUS_2016")
  pp_q3_weights <- vapply(
    7:9,
    function(month) traveller_value(paste0("PP_WT_2016M", month)),
    numeric(nrow(spine))
  )
  expect_identical(census, erp_q3)
  expect_identical(
    pp_census,
    ifelse(rowSums(pp_q3_weights > 0) > 0, 1L, 3L)
  )

  expect_true(all(traveller_value(
    "NOM_DIRECTION", "Direction of travel in the reference quarter"
  ) %in% c("A", "D")))
  expect_true(all(stats::na.omit(traveller_value(
    "VISA_APPLICANT_TYPE", "Visa applicant type"
  )) %in% c("Primary", "Secondary")))
})

test_that("TRAVELLERS reference movement fields use coherent HA domains", {
  spine <- data.frame(
    id = 1:7,
    birth_year = c(1980L, 1980L, 1980L, 1980L, 1980L, 1980L, 1980L),
    country_of_birth_sacc = c(
      1101L, 1201L, 7103L, 2102L, 7103L, 1101L, 2102L
    ),
    citizenship = c(1L, 2L, 2L, 1L, 2L, 2L, 2L),
    year_of_arrival = c(NA, 2000L, 2010L, 2005L, 2020L, NA, 2000L),
    year_of_death = c(NA, NA, NA, NA, NA, NA, 2010L),
    residence_seed = 201L:207L
  )
  traveller_value <- function(name, table_name = "madip_ge_mig_travellers_2014") {
    fplida:::.dil_general_value(
      name = name,
      description = name,
      dataset = "TRAVELLERS",
      product_name = "madipge-mig-d-travellers-2006-current",
      table_name = table_name,
      module_name = "Travellers 2006 - latest",
      spine_rows = spine,
      seed = 20260803L
    )
  }
  subclass_domain <- c(
    "000", "001", "444", "189", "190", "491", "186", "482",
    "820", "801", "309", "300", "100", "500", "485", "476",
    "417", "462", "200", "201", "202", "204", "866"
  )

  country <- traveller_value("COUNTRY_OF_CITIZENSHIP")
  stream <- traveller_value("VISA_STREAM_CODE")
  subclass <- traveller_value("VISA_SUBCLASS")
  direction <- traveller_value("NOM_DIRECTION")
  applicant_type <- traveller_value("VISA_APPLICANT_TYPE")

  expect_type(country, "character")
  expect_type(stream, "character")
  expect_type(subclass, "character")
  expect_identical(country[1:5], c("1101", "1201", "7103", "1101", "7103"))
  expect_false(identical(country[[6L]], "1101"))
  expect_true(all(grepl("^[0-9]{4}$", country)))
  expect_true(all(is.na(stream)))
  expect_identical(subclass[c(1L, 2L, 4L, 5L, 7L)],
                   c("000", "444", "000", "001", "001"))
  expect_true(all(subclass %in% subclass_domain))
  expect_true(all(direction %in% c("A", "D")))
  expect_true(all(stats::na.omit(applicant_type) %in%
                    c("Primary", "Secondary")))
  expect_identical(is.na(applicant_type), subclass == "001")
  expect_identical(subclass, traveller_value("VISA_SUBCLASS"))

  sacc_path <- system.file(
    "extdata", "codeframes", "sacc_country.tsv", package = "fplida"
  )
  sacc <- read.delim(sacc_path, sep = "\t", quote = "", stringsAsFactors = FALSE)
  expect_true(all(country %in% sprintf("%04d", sacc$code)))

  subclass_2021 <- traveller_value(
    "VISA_SUBCLASS", "madip_ge_mig_travellers_2021q1"
  )
  expect_true(subclass_2021[[5L]] %in% setdiff(subclass_domain, c("000", "001", "444")))
  expect_identical(subclass_2021[[7L]], "001")

  inventory <- fplida:::.dil_structure_inventory("TRAVELLERS")$variables
  occurrences <- table(inventory[["Variable Name"]])
  expect_identical(
    as.integer(occurrences[c(
      "COUNTRY_OF_CITIZENSHIP", "VISA_STREAM_CODE", "VISA_SUBCLASS",
      "NOM_DIRECTION", "VISA_APPLICANT_TYPE"
    )]),
    rep(32L, 5L)
  )
})

test_that("canonical TRAVELLERS tables materialise all derived fields", {
  skip_if_not_installed("arrow")
  tmp <- tempfile("fplida_dil_travellers_")
  dir.create(file.path(tmp, "_system"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  n <- 8L
  spine <- data.frame(
    id = seq_len(n),
    spine_id = sprintf("S%010d", seq_len(n)),
    aeuid_ha = sprintf("H%011d", seq_len(n)),
    birth_year = c(1980L, 2018L, 1990L, 1990L, 1985L, 1985L, 1970L, 2000L),
    month_of_birth = rep(1:8, length.out = n),
    sex = rep(1:2, length.out = n),
    state = rep(1:8, length.out = n),
    indigenous = rep(c(1L, 4L), length.out = n),
    country_of_birth = c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 0L),
    country_of_birth_sacc = c(
      1101L, 1101L, 7103L, 2102L, 1101L, 1101L, 3204L, 1101L
    ),
    citizenship = c(1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L),
    year_of_arrival = c(NA, NA, 2015L, 2010L, NA, NA, 2000L, NA),
    year_of_death = rep(NA_integer_, n),
    month_of_death = rep(NA_integer_, n),
    day_of_death = rep(NA_integer_, n),
    residence_seed = 101L:(100L + n),
    sa2_code = 101021001L + seq_len(n),
    sa3_code = 10102L,
    sa4_code = 101L,
    anzsco_code = 261313L,
    industry = 1L,
    baseline_employed = 1L,
    baseline_hours = 38,
    baseline_income = 80000,
    household_id = rep(seq_len(n / 2L), each = 2L)
  )
  arrow::write_parquet(
    spine, file.path(tmp, "_system", "base-spine.parquet")
  )
  result <- fplida:::.complete_plida_dil_structures(
    run_dir = tmp,
    build_order = "travellers",
    seed = 20260803L,
    max_rows = n,
    verbose = FALSE
  )
  expect_equal(result$structures_written, 35L)

  read_structure <- function(product, table) {
    path <- file.path(
      tmp, "ha-travellers",
      paste0(fplida:::.dil_structure_stem(product, table), ".parquet")
    )
    as.data.frame(arrow::read_parquet(path))
  }
  erp <- read_structure(
    "madipge-mig-d-erp-2006-current",
    "madipge_mig_d_erp_2006_current"
  )
  pp <- read_structure(
    "madipge-mig-d-pp-2006-current",
    "madipge_mig_d_pp_2006_current"
  )
  ppw <- read_structure(
    "madipge-mig-d-pp-2006-current",
    "madipge_mig_d_ppw_2006_current"
  )
  erp_names <- grep("^(ERP_STATUS_|CENSUS_)", names(erp), value = TRUE)
  pp_names <- grep("^(PP_STATUS_|PP_CENSUS_)", names(pp), value = TRUE)
  ppw_names <- grep("^PP_WT_", names(ppw), value = TRUE)
  expect_equal(length(erp_names), 78L)
  expect_equal(length(pp_names), 244L)
  expect_equal(length(ppw_names), 240L)
  expect_false(anyNA(erp[erp_names]))
  expect_false(anyNA(pp[pp_names]))
  expect_false(anyNA(ppw[ppw_names]))
  expect_true(all(unlist(ppw[ppw_names], use.names = FALSE) >= 0))
  expect_true(all(unlist(ppw[ppw_names], use.names = FALSE) <= 1))

  movement_paths <- list.files(
    file.path(tmp, "ha-travellers"),
    pattern = paste0(
      "^madipge-mig-d-travellers-2006-current--",
      "madip_ge_mig_travellers_.*\\.parquet$"
    ),
    full.names = TRUE
  )
  expect_length(movement_paths, 32L)
  movement <- do.call(rbind, lapply(movement_paths, function(path) {
    frame <- as.data.frame(arrow::read_parquet(path))
    frame[c(
      "COUNTRY_OF_CITIZENSHIP", "VISA_STREAM_CODE", "VISA_SUBCLASS",
      "NOM_DIRECTION", "VISA_APPLICANT_TYPE"
    )]
  }))
  expect_false(anyNA(movement$COUNTRY_OF_CITIZENSHIP))
  expect_true(all(is.na(movement$VISA_STREAM_CODE)))
  expect_false(anyNA(movement$VISA_SUBCLASS))
  expect_true(all(grepl("^[0-9]{4}$", movement$COUNTRY_OF_CITIZENSHIP)))
  expect_true(all(movement$VISA_SUBCLASS %in% c(
    "000", "001", "444", "189", "190", "491", "186", "482",
    "820", "801", "309", "300", "100", "500", "485", "476",
    "417", "462", "200", "201", "202", "204", "866"
  )))
  expect_true(all(movement$NOM_DIRECTION %in% c("A", "D")))
  expect_true(all(stats::na.omit(movement$VISA_APPLICANT_TYPE) %in%
                    c("Primary", "Secondary")))
  expect_identical(
    is.na(movement$VISA_APPLICANT_TYPE), movement$VISA_SUBCLASS == "001"
  )
})

test_that("table periods preserve monthly, quarterly, and snapshot timing", {
  mbs <- fplida:::.dil_table_period(
    "MBS", "madipge-mbs-d-claims-2006", "mbs_dos_2006_1",
    "Medicare Benefits Schedule"
  )
  expect_identical(mbs$start, as.Date("2006-01-01"))
  expect_identical(mbs$end, as.Date("2006-01-31"))
  expect_identical(mbs$month, 1L)
  expect_identical(mbs$granularity, "month")

  mbs_october <- fplida:::.dil_table_period(
    "MBS", "madipge-mbs-d-claims-2009", "mbs_dos_2009_10",
    "Medicare Benefits Schedule"
  )
  expect_identical(mbs_october$start, as.Date("2009-10-01"))
  expect_identical(mbs_october$end, as.Date("2009-10-31"))

  ndis <- fplida:::.dil_table_period(
    "NDIS", "madipge-ndis-exp-d-outcomes-13-current",
    "ndis_familyoutcomes_2016_3",
    "National Disability Insurance Scheme Expanded Outcomes 2013-current"
  )
  expect_identical(ndis$start, as.Date("2016-07-01"))
  expect_identical(ndis$end, as.Date("2016-09-30"))
  expect_identical(ndis$quarter, 3L)
  expect_identical(ndis$granularity, "quarter")

  nacdc <- fplida:::.dil_table_period(
    "NACDC", "madipge-aged-care-d-agedrecipient-15-current",
    "aged_recipient_apr2025", "Aged Care Data 2015-current"
  )
  expect_identical(nacdc$start, as.Date("2025-04-01"))
  expect_identical(nacdc$end, as.Date("2025-04-30"))
  expect_identical(nacdc$month, 4L)

  core <- fplida:::.dil_table_period(
    "CORE", "plidage-core-income-cb-annual",
    "core_employment_annual_0607_v8", "Core employment"
  )
  expect_identical(core$start, as.Date("2006-07-01"))
  expect_identical(core$end, as.Date("2007-06-30"))
  expect_identical(core$granularity, "financial_year")

  mcd <- fplida:::.dil_table_period(
    "MCD", "madipge-mcd-d-current", "mcd_0622_demogs",
    "Medicare Consumer Directory"
  )
  expect_identical(mcd$start, as.Date("2022-06-01"))
  expect_identical(mcd$end, as.Date("2022-06-30"))
  expect_identical(mcd$month, 6L)
})

test_that("table periods do not confuse durations with financial years", {
  pit_12m <- fplida:::.dil_table_period(
    "PIT_ITR", "madipge-ato-d-context-fy2021",
    "ato_itr_context_2021_12m", "ATO individual income tax"
  )
  expect_identical(pit_12m$start, as.Date("2020-07-01"))
  expect_identical(pit_12m$end, as.Date("2021-06-30"))

  sae_46m <- fplida:::.dil_table_period(
    "SAE", "madipmp-ato-maasmats-2020-21",
    "plida_maasmats_2021_46m", "Member account attributes"
  )
  expect_identical(sae_46m$start, as.Date("2020-07-01"))
  expect_identical(sae_46m$end, as.Date("2021-06-30"))

  pit_1920 <- fplida:::.dil_table_period(
    "PIT_ITR", "madipge-ato-d-context-fy1920",
    "ato_itr_context_1920", "ATO individual income tax"
  )
  expect_identical(pit_1920$start, as.Date("2019-07-01"))
  expect_identical(pit_1920$end, as.Date("2020-06-30"))

  core_census <- fplida:::.dil_table_period(
    "CORE", "plidage-core-demog-cb-c11-2006-latest",
    "core_demog_2011_v8", "Core demographics"
  )
  expect_identical(core_census$start, as.Date("2011-01-01"))
  expect_identical(core_census$end, as.Date("2011-12-31"))

  ato_cr <- fplida:::.dil_table_period(
    "ATO_CR", "madipge-ato-d-client-register-1999-25",
    "ato_cr_1999_25", "ATO client register"
  )
  expect_identical(ato_cr$start, as.Date("1999-01-01"))
  expect_identical(ato_cr$end, as.Date("2025-12-31"))
  expect_false(ato_cr$precise)

  visa <- fplida:::.dil_table_period(
    "VISA", "madipge-visa-d-1990-25", "visa_1990_25",
    "Home Affairs visa"
  )
  expect_identical(visa$start, as.Date("1990-01-01"))
  expect_identical(visa$end, as.Date("2025-12-31"))

  amep <- fplida:::.dil_table_period(
    "AMEP", "madipge-amep-d-2003-19", "amep_2003_19",
    "Adult Migrant English Program"
  )
  expect_identical(amep$start, as.Date("2003-01-01"))
  expect_identical(amep$end, as.Date("2019-12-31"))
})

test_that("all DIL table periods stay within the published time horizon", {
  structures <- fplida:::.dil_structure_inventory()$structures
  periods <- lapply(seq_len(nrow(structures)), function(i) {
    fplida:::.dil_table_period(
      structures$Dataset[[i]], structures[["Product Name"]][[i]],
      structures[["Table Name"]][[i]], structures[["Module Name"]][[i]]
    )
  })
  start_year <- vapply(
    periods, function(period) as.integer(format(period$start, "%Y")),
    integer(1)
  )
  end_year <- vapply(
    periods, function(period) as.integer(format(period$end, "%Y")),
    integer(1)
  )
  expect_true(all(start_year >= 1990L))
  expect_true(all(end_year <= 2026L))
})

test_that("table periods rewrite copied source dates and period fields", {
  frame <- data.frame(
    DOS = as.Date(c("2024-08-10", "2024-09-12")),
    DOP = as.Date(c("2024-08-11", "2024-09-13")),
    RPDATE = as.Date(c("2024-07-01", "2024-07-02")),
    stringsAsFactors = FALSE
  )
  actual <- fplida:::.dil_apply_table_period(
    frame,
    dataset = "MBS",
    product_name = "madipge-mbs-d-claims-2006",
    table_name = "mbs_dos_2006_1",
    module_name = "Medicare Benefits Schedule",
    seed = 20260803L
  )
  expect_true(all(actual$DOS >= as.Date("2006-01-01")))
  expect_true(all(actual$DOS <= as.Date("2006-01-31")))
  expect_true(all(actual$DOP >= actual$DOS))
  expect_true(all(actual$RPDATE <= actual$DOS))

  ndis_frame <- data.frame(
    FY_CLAIM = 2024L,
    PYMTRQSTCRTDDT = as.Date(c("2024-08-10", "2024-09-12")),
    RBAPYMTCLRDDT = as.Date(c("2024-08-09", "2024-09-11")),
    RBAPYMTSENTDT = as.Date(c("2024-08-08", "2024-09-10")),
    SUPPSTRTDTADJ = as.Date(c("2024-07-01", "2024-08-01")),
    SUPPENDDTADJ = as.Date(c("2024-07-31", "2024-08-31")),
    SRCSYSTMDT = as.Date("2024-12-31")
  )
  ndis_actual <- fplida:::.dil_apply_table_period(
    ndis_frame,
    dataset = "NDIS",
    product_name = "madipge-ndis-exp-d-payments-13-current",
    table_name = "ndis_payments_2013_3",
    module_name = "NDIS expanded payments",
    seed = 20260803L
  )
  ndis_actual <- fplida:::.dil_reconcile_structure_frame(
    ndis_actual, "NDIS"
  )
  expect_true(all(ndis_actual$PYMTRQSTCRTDDT >= as.Date("2013-07-01")))
  expect_true(all(ndis_actual$PYMTRQSTCRTDDT <= as.Date("2013-09-30")))
  expect_true(all(ndis_actual$SUPPSTRTDTADJ <= ndis_actual$SUPPENDDTADJ))
  expect_true(all(ndis_actual$PYMTRQSTCRTDDT <= ndis_actual$RBAPYMTCLRDDT))
  expect_true(all(ndis_actual$RBAPYMTCLRDDT <= ndis_actual$RBAPYMTSENTDT))
  expect_identical(unique(ndis_actual$FY_CLAIM), 2014L)
  expect_identical(unique(ndis_actual$SRCSYSTMDT), as.Date("2013-09-30"))

  traveller_frame <- data.frame(
    DURATION_MOVEMENT_DATE = as.Date(c("2024-01-01", "2024-02-01")),
    REFERENCE_PERIOD = "2024Q1"
  )
  traveller_actual <- fplida:::.dil_apply_table_period(
    traveller_frame,
    dataset = "TRAVELLERS",
    product_name = "madipge-mig-d-travellers-2006-current",
    table_name = "madip_ge_mig_travellers_2020q1",
    module_name = "Travellers",
    seed = 20260803L
  )
  expect_true(all(
    traveller_actual$DURATION_MOVEMENT_DATE >= as.Date("2020-01-01") &
      traveller_actual$DURATION_MOVEMENT_DATE <= as.Date("2020-03-31")
  ))
  expect_identical(unique(traveller_actual$REFERENCE_PERIOD), "2020Q1")
})

test_that("snapshot periods preserve historical dates", {
  frame <- data.frame(
    REFERENCE_DATE = as.Date("2024-10-31"),
    HCP_DEMENTIA_DIAGNOSIS_DATE = as.Date("2018-06-15"),
    stringsAsFactors = FALSE
  )
  actual <- fplida:::.dil_apply_table_period(
    frame,
    dataset = "NACDC",
    product_name = "madipge-aged-care-d-agedrecipient-15-current",
    table_name = "aged_recipient_apr2025",
    module_name = "Aged Care Data 2015-current",
    seed = 20260803L
  )
  expect_identical(actual$REFERENCE_DATE, as.Date("2025-04-30"))
  expect_identical(
    actual$HCP_DEMENTIA_DIAGNOSIS_DATE, as.Date("2018-06-15")
  )
})

test_that("DIL completion writes exact schemas without adding linkage fields", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_dil_structures_")
  dir.create(file.path(tmp, "_system"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  n <- 100L
  spine <- data.frame(
    id = seq_len(n),
    spine_id = sprintf("S%010d", seq_len(n)),
    birth_year = rep(1960:2009, length.out = n),
    month_of_birth = rep(1:12, length.out = n),
    sex = rep(1:2, length.out = n),
    state = rep(1:8, length.out = n),
    indigenous = rep(c(1L, 4L), length.out = n),
    country_of_birth_sacc = rep(c(1101L, 2100L), length.out = n),
    year_of_arrival = rep(c(NA_integer_, 2005L), length.out = n),
    year_of_death = NA_integer_,
    month_of_death = NA_integer_,
    day_of_death = NA_integer_,
    sa2_code = 101021001L + seq_len(n),
    sa3_code = 10102L,
    sa4_code = 101L,
    anzsco_code = 261313L,
    industry = 1L,
    baseline_employed = 1L,
    baseline_hours = 38,
    baseline_income = 80000,
    household_id = rep(seq_len(n / 2L), each = 2L),
    aeuid_ato = sprintf("A%011d", seq_len(n)),
    aeuid_apsc = sprintf("P%011d", seq_len(n)),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(
    spine, file.path(tmp, "_system", "base-spine.parquet")
  )

  result <- fplida:::.complete_plida_dil_structures(
    run_dir = tmp,
    build_order = c("apsed", "ers"),
    seed = 20260803L,
    max_rows = 25L,
    verbose = FALSE
  )
  expect_equal(result$structures_written, 10L)

  inventory <- fplida:::.dil_structure_inventory(c("APSED", "ERS"))
  for (i in seq_len(nrow(inventory$structures))) {
    structure <- inventory$structures[i, , drop = FALSE]
    expected <- inventory$variables[["Variable Name"]][
      inventory$variables$Dataset == structure$Dataset &
        inventory$variables[["Product Name"]] == structure[["Product Name"]] &
        inventory$variables[["Table Name"]] == structure[["Table Name"]]
    ]
    stem <- fplida:::.dil_structure_stem(
      structure[["Product Name"]], structure[["Table Name"]]
    )
    path <- file.path(
      tmp,
      paste0(tolower(fplida:::dataset_to_agency(structure$Dataset)), "-",
             tolower(structure$Dataset)),
      paste0(stem, ".parquet")
    )
    expect_true(file.exists(path), info = stem)
    frame <- as.data.frame(arrow::read_parquet(path))
    expect_identical(names(frame), unique(expected), info = stem)
    expect_equal(nrow(frame), 25L, info = stem)
  }

  apsed_agency <- inventory$structures[
    inventory$structures$Dataset == "APSED" &
      inventory$structures[["Table Name"]] == "apsed_agency",
    ,
    drop = FALSE
  ]
  agency_path <- file.path(
    tmp, "apsc-apsed",
    paste0(fplida:::.dil_structure_stem(
      apsed_agency[["Product Name"]], apsed_agency[["Table Name"]]
    ), ".parquet")
  )
  agency_frame <- as.data.frame(arrow::read_parquet(agency_path))
  expect_false("SYNTHETIC_AEUID" %in% names(agency_frame))
  expect_true(all(agency_frame$AGENCY_SIZE %in% c(
    "Micro", "Extra Small", "Small", "Medium", "Large", "Extra Large"
  )))
})

test_that("administrative evidence domains replace generic values", {
  spine <- data.frame(
    id = 1:40,
    birth_year = rep(1980L, 40L),
    sex = rep(1:2, length.out = 40L),
    state = rep(1:8, length.out = 40L),
    baseline_income = rep(70000, 40L),
    baseline_employed = rep(1L, 40L),
    aeuid_dhda = sprintf("H%011d", 1:40),
    stringsAsFactors = FALSE
  )
  actual <- fplida:::.dil_general_value(
    name = "BILLTYPECD",
    description = "Medicare billing type code",
    dataset = "MBS",
    product_name = "madipge-mbs-d-claims-2006",
    table_name = "mbs_dos_2006_1",
    module_name = "Medicare Benefits Schedule",
    spine_rows = spine,
    seed = 20260803L
  )
  expect_true(all(actual %in% c("D", "P")))
  expect_gt(length(unique(actual)), 1L)
  expect_false(any(grepl("^C[0-9]+$", actual)))

  attendance_mode <- fplida:::.dil_general_value(
    name = "ATTENDANCE_MODE",
    description = paste(
      "A code which identifies the mode of attendance by which the student",
      "undertook in a calendar year"
    ),
    dataset = "HE",
    product_name = "pmp-he-enrol",
    table_name = "he_enrol",
    module_name = "Higher education",
    spine_rows = spine,
    seed = 20260803L
  )
  expect_type(attendance_mode, "integer")
  expect_true(all(is.na(attendance_mode)))
})

test_that("evidence domains retain observed local frequencies", {
  actual <- fplida:::.dil_sample_values(
    c("common", "rare"), 1000L, 20260803L, 17L,
    weights = c(99, 1)
  )
  expect_gt(mean(actual == "common"), 0.95)
  expect_lt(mean(actual == "rare"), 0.05)
  expect_identical(
    fplida:::.dil_evidence_weights(
      c("042", "041"), "042=2490; 041=10"
    ),
    c(2490, 10)
  )
  expect_null(fplida:::.dil_sample_evidence_values(
    c("A", "B"), "A / A / B", 100L, 20260803L, 18L
  ))
})

test_that("source reuse is exact, aligned, and ignores canonical outputs", {
  candidates <- data.frame(
    path = c("table.parquet", "product.parquet", "other.parquet"),
    logical_output = c("target_table", "target_product", "other_table"),
    stringsAsFactors = FALSE
  )
  candidates$columns <- I(list(
    c("FAMILY_ID", "FMCF", "FNOF"),
    c("FAMILY_ID", "FMCF", "FNOF"),
    c("FAMILY_ID", "FMCF", "FNOF")
  ))
  expect_identical(
    fplida:::.dil_best_structure_source(
      candidates, "target_product", "target_table",
      c("FAMILY_ID", "FMCF", "FNOF")
    ),
    "table.parquet"
  )
  expect_null(fplida:::.dil_best_structure_source(
    candidates[2L, , drop = FALSE],
    "target_product", "target_table", c("FAMILY_ID", "FMCF", "FNOF"),
    allow_product_source = FALSE
  ))
  expect_identical(
    fplida:::.dil_best_structure_source(
      candidates[2L, , drop = FALSE],
      "target_product", "target_table", c("FAMILY_ID", "FMCF", "FNOF"),
      allow_product_source = TRUE
    ),
    "product.parquet"
  )
  sparse_product <- candidates[2L, , drop = FALSE]
  sparse_product$columns <- I(list(c("SYNTHETIC_AEUID", "ONE_FIELD")))
  expect_identical(
    fplida:::.dil_best_structure_source(
      sparse_product, "target_product", "target_table",
      c("SYNTHETIC_AEUID", "ONE_FIELD", "UNRESOLVED_A", "UNRESOLVED_B")
    ),
    "product.parquet"
  )
  alias <- candidates[3L, , drop = FALSE]
  alias$columns <- I(list(c("SYNTHETIC_AEUID", "ONE_FIELD")))
  expect_identical(
    fplida:::.dil_best_structure_source(
      alias, "target_product", "target_table",
      c("SYNTHETIC_AEUID", "ONE_FIELD"),
      allow_product_source = FALSE,
      source_aliases = "other_table"
    ),
    "other.parquet"
  )

  source <- data.frame(
    SYNTHETIC_AEUID = c("A1", "A3", "not-in-spine"),
    VALUE = 1:3,
    stringsAsFactors = FALSE
  )
  spine <- data.frame(
    spine_id = c("S1", "S2", "S3"),
    aeuid_abs = c("A1", "A2", "A3"),
    stringsAsFactors = FALSE
  )
  aligned <- fplida:::.dil_align_source_to_spine(
    source, spine, "CENSUS", min_rows = 2L
  )
  expect_identical(aligned$source$SYNTHETIC_AEUID, c("A1", "A3"))
  expect_identical(aligned$spine$spine_id, c("S1", "S3"))

  skip_if_not_installed("arrow")
  full_spine_dir <- tempfile("fplida_full_spine_")
  dir.create(full_spine_dir)
  on.exit(unlink(full_spine_dir, recursive = TRUE), add = TRUE)
  full_spine_path <- file.path(full_spine_dir, "base-spine.parquet")
  agency_spine_path <- file.path(full_spine_dir, "abs-spine.parquet")
  arrow::write_parquet(
    data.frame(
      spine_id = sprintf("S%02d", 1:40),
      birth_year = 1980L + (1:40 %% 20L)
    ),
    full_spine_path
  )
  arrow::write_parquet(
    data.frame(
      spine_id = c("S31", "S35", "S40"),
      SYNTHETIC_AEUID = c("A1", "A2", "A3")
    ),
    agency_spine_path
  )
  full_aligned <- fplida:::.dil_align_source_to_full_spine(
    source[1:2, , drop = FALSE], full_spine_path, agency_spine_path,
    "CENSUS", min_rows = 2L
  )
  expect_identical(full_aligned$source$SYNTHETIC_AEUID, c("A1", "A3"))
  expect_identical(full_aligned$spine$spine_id, c("S31", "S40"))

  tmp <- tempfile("fplida_dil_candidates_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  arrow::write_parquet(source, file.path(tmp, "native.parquet"))
  arrow::write_parquet(source, file.path(tmp, "product--table.parquet"))
  arrow::write_parquet(source, file.path(tmp, "product--native-table.parquet"))
  indexed <- fplida:::.dil_structure_candidate_index(tmp)
  expect_identical(indexed$logical_output, "native")
  indexed_with_inventory <- fplida:::.dil_structure_candidate_index(
    tmp, canonical_outputs = "product--table"
  )
  expect_setequal(
    indexed_with_inventory$logical_output,
    c("native", "product--native-table")
  )
})

test_that("annual MBS and PBS products can populate monthly DIL tables", {
  expect_true(fplida:::.dil_allow_product_source("MBS", 13L))
  expect_true(fplida:::.dil_allow_product_source("PBS", 12L))
  expect_true(fplida:::.dil_allow_product_source("CORE", 1L))
  expect_false(fplida:::.dil_allow_product_source("CORE", 2L))
  expect_true(fplida:::.dil_allow_product_source("AIR", 7L))
  expect_true(fplida:::.dil_allow_product_source("JK", 5L))
  expect_true(fplida:::.dil_allow_product_source("TVA", 2L))
  expect_true(fplida:::.dil_allow_product_source("NACDC", 6L))
  expect_true(fplida:::.dil_allow_product_source("NDIS", 134L))
  expect_false(fplida:::.dil_allow_product_source("STP", 98L))
})

test_that("explicit source aliases cover CGT, DEATHS, HE, DOMINO, and CORE", {
  expect_identical(
    fplida:::.dil_structure_source_aliases(
      "CGT", "pmp-cgt-2000-01", "cgt_2000_01"
    ),
    "pmp-cgt-0001"
  )
  expect_identical(
    fplida:::.dil_structure_source_aliases(
      "CORE", "plidage-core-relat-cb-c21-2006-latest",
      "core_partner_2021_v8"
    ),
    "plidage-core-relat-cb-c21-2006-latest"
  )
  expect_identical(
    fplida:::.dil_structure_source_aliases(
      "DEATHS",
      "madip-ge-050101d-deaths-deathregistrations2007-2007",
      "death_registrations_2007"
    ),
    "madipge-death-d-cause-of-death-2007"
  )
  expect_identical(
    fplida:::.dil_structure_source_aliases(
      "HE", "madipge-hied", "hes_madip_student_enrol"
    ),
    "madipge-hied-student-enrol"
  )
  expect_identical(
    fplida:::.dil_structure_source_aliases(
      "DOMINO", "madipge-dom-monthly-d-base", "static_demogs"
    ),
    "madipge-dom-monthly-d-base--static-demogs"
  )
})

test_that("monthly DIL tables select values from their annual product", {
  source <- data.frame(
    DOS = c("15Jan20", "20Feb20", "22Feb20", "08Mar20"),
    ITEM = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )
  february <- fplida:::.dil_filter_product_source(
    source,
    dataset = "MBS",
    product_name = "madipge-mbs-d-claims-2020",
    table_name = "mbs_dos_2020_2",
    module_name = "Medicare Benefits Schedule",
    max_rows = 1L
  )
  expect_identical(february$ITEM, "2")

  no_matching_month <- fplida:::.dil_filter_product_source(
    source,
    dataset = "MBS",
    product_name = "madipge-mbs-d-claims-2020",
    table_name = "mbs_dos_2020_4",
    module_name = "Medicare Benefits Schedule",
    max_rows = 2L
  )
  expect_identical(no_matching_month$ITEM, c("1", "2"))
})

test_that("persistent administrative identifiers are stable across tables", {
  spine <- data.frame(
    id = 1:40,
    birth_year = 1980L,
    sex = rep(1:2, length.out = 40L),
    state = 1L,
    baseline_income = 70000,
    stringsAsFactors = FALSE
  )
  identifier <- function(product, table) {
    fplida:::.dil_general_value(
      "EMPLOYERGUID", "Unique employer identifier", "STP", product, table,
      "Single Touch Payroll", spine, 20260803L
    )
  }
  expect_identical(
    identifier("pmp-stp-2019", "stp_2019_m01"),
    identifier("pmp-stp-2020", "stp_2020_m12")
  )
})

test_that("administrative fallback respects variable semantics", {
  spine <- data.frame(
    id = 1:40,
    birth_year = rep(1965:2004, length.out = 40L),
    sex = rep(1:2, length.out = 40L),
    state = rep(1:8, length.out = 40L),
    baseline_income = seq(40000, 118000, length.out = 40L),
    baseline_employed = rep(1L, 40L),
    stringsAsFactors = FALSE
  )
  value <- function(name, description, dataset = "A&T") {
    fplida:::.dil_general_value(
      name = name,
      description = description,
      dataset = dataset,
      product_name = if (dataset == "A&T") {
        "plidage-apprentice"
      } else {
        "madipge-ato-d-ded-exp-off-fy2021"
      },
      table_name = if (dataset == "A&T") {
        "activity_ledger"
      } else {
        "ato_itr_ded_exp_off_2021_12m"
      },
      module_name = "Administrative data",
      spine_rows = spine,
      seed = 20260803L
    )
  }

  gender <- value("PARENT1GENDER", "First parent or carer gender")
  expect_type(gender, "character")
  expect_setequal(unique(gender), c("M", "F"))

  # Research reached this field but could only establish that it is a status
  # code, not what the codes are, so it stays typed missing.
  payment_status <- value("PAYMENTSTATUSCODE", "Payment Status Code")
  expect_type(payment_status, "character")
  expect_true(all(is.na(payment_status)))

  # Where research DID establish a domain, the fallback draws from it rather
  # than writing typed missing. This is the whole point of the registry hook,
  # and CONTACTMETHOD reaches it through the `categorical_name` branch that
  # used to return NA before consulting anything.
  contact <- value("CONTACTMETHOD", "The method used to contact the apprentice")
  documented <- .registry_values_for("A&T", "CONTACTMETHOD")
  expect_gt(length(documented), 0L)
  expect_true(all(contact %in% documented))

  # A variable the registry documents nothing for still writes typed missing:
  # the fallback fills documented domains, it does not invent them.
  undocumented <- value("ZZ_NOT_A_REAL_FIELD_CD", "Not a real code")
  expect_true(all(is.na(undocumented)))

  custodial <- value(
    "CUSTODIALAUSTRALIANAPPRENTICE",
    "Whether the Apprentice is in prison"
  )
  expect_type(custodial, "integer")
  expect_setequal(unique(custodial), c(0L, 1L))

  indicator <- value(
    "SPS_DIE_DRG_YR_IND", "Did your spouse die during the year?",
    dataset = "PIT_ITR"
  )
  expect_type(indicator, "integer")
  expect_setequal(unique(indicator), c(0L, 1L))

  amount <- value(
    "DFRD_NCL_BUSLSSESPRR_YR_PP_AMT",
    "Deferred non-commercial business losses from a prior year",
    dataset = "PIT_ITR"
  )
  expect_type(amount, "double")
  expect_gt(length(unique(amount)), 10L)
  expect_true(max(abs(amount)) > 10000)

  wages <- value(
    "SALARY_WAGES_AMT", "Salary and wages amount", dataset = "PIT_ITR"
  )
  expect_type(wages, "double")
  expect_gt(length(unique(wages)), 10L)

  age <- value("AGE2324_END", "", dataset = "PIT_ITR")
  expect_false(inherits(age, "Date"))
  expect_true(all(age >= 16L & age <= 115L))

  estate <- value(
    "ESTATE_VALUE_AMT", "Estate value amount", dataset = "PIT_ITR"
  )
  expect_type(estate, "double")
  expect_gt(length(unique(estate)), 10L)

  age_pension <- value(
    "AGE_PNSN_AMT", "Age pension amount", dataset = "PIT_ITR"
  )
  expect_type(age_pension, "double")
  expect_gt(length(unique(age_pension)), 10L)

  prior_year_count <- value(
    "PRPTY_RNTD_WKS_THIS_YR_CNT", "Count of weeks rented this year",
    dataset = "PIT_ITR"
  )
  expect_type(prior_year_count, "integer")
  expect_true(all(prior_year_count %in% 0:7))

  state_move <- value(
    "STATE_MOVE_1YR_21", "Usual Address One Year Ago Indicator for State",
    dataset = "ACLD"
  )
  expect_type(state_move, "integer")
  expect_setequal(unique(state_move), c(0L, 1L))

  valid_from <- value(
    "VALIDFROM", "The date from which the value is valid"
  )
  expect_s3_class(valid_from, "Date")

  citizenship <- value(
    "COUNTRY_OF_CITIZENSHIP", "Country of citizenship"
  )
  expect_type(citizenship, "character")
  expect_true(all(is.na(citizenship)))

  score_id <- value("SCOREID", "Unique score identifier", dataset = "DEX")
  expect_type(score_id, "character")
  expect_gt(length(unique(score_id)), 10L)

  employer_id <- value(
    "EMPLOYERGUID", "Unique employer identifier", dataset = "STP"
  )
  expect_type(employer_id, "character")
  expect_gt(length(unique(employer_id)), 10L)

  employer_count <- value(
    "NUMBEROFEMPLOYERRELATIONSHIPS",
    "Number of employer relationships", dataset = "STP"
  )
  expect_type(employer_count, "integer")
  expect_true(all(employer_count %in% 0:7))

  geography_name <- value(
    "SA2_NAME", "Statistical Area Level 2 name", dataset = "CORE"
  )
  expect_type(geography_name, "character")
  expect_true(all(is.na(geography_name)))
})

test_that("ANZSIC fields use the bundled code frame by level", {
  spine <- data.frame(
    id = 1:40,
    birth_year = 1980L,
    sex = rep(1:2, length.out = 40L),
    state = 1L,
    industry = rep(1:19, length.out = 40L),
    baseline_income = 70000,
    stringsAsFactors = FALSE
  )
  value <- function(name, description) {
    fplida:::.dil_general_value(
      name, description, "PIT_ITR", "madipge-ato-d-context-fy2021",
      "ato_itr_context_2021_12m", "ATO individual income tax", spine,
      20260803L
    )
  }

  class_code <- value("ANZSIC_CLASS_CODE", "ANZSIC 2006 class code")
  expect_true(all(grepl("^[0-9]{4}$", class_code)))
  class_name <- value(
    "ANZSIC_CLASS_DESCRIPTION", "ANZSIC 2006 class description"
  )
  expect_true(all(!is.na(class_name) & nzchar(class_name)))
  flag <- value("ANZSIC_IMPUTATION_FLAG", "ANZSIC imputation flag")
  expect_type(flag, "integer")
  expect_setequal(unique(flag), c(0L, 1L))
})

test_that("AMEP attendance fields use period-specific hour supports", {
  variables <- fplida:::.dil_structure_inventory("AMEP")$variables
  hour_rows <- unique(variables[grepl(
    "number of hours attended by client",
    tolower(variables[["Variable Description"]]), fixed = TRUE
  ), c("Variable Name", "Variable Description")])
  expect_equal(nrow(hour_rows), 317L)

  quarterly <- fplida:::.dil_amep_hours_value(
    "AMEP_2019_Q4",
    "AMEP (510 hour entitlement) - number of hours attended by client in 2019 Quarter 4",
    100L, 20260803L, 1L
  )
  expect_type(quarterly, "double")
  expect_true(all(quarterly >= 0 & quarterly <= 160))
  expect_gt(length(unique(quarterly)), 10L)

  annual <- fplida:::.dil_amep_hours_value(
    "AMEPDL_2010_11",
    "AMEPDL (510 hour entitlement) - number of hours attended by client in 2010-11 financial year",
    100L, 20260803L, 2L
  )
  expect_true(all(annual >= 0 & annual <= 510))
})

test_that("ACLD Census aliases preserve roles and reject change flags", {
  spine <- data.frame(
    id = 1:40,
    birth_year = 1980L,
    sex = rep(1:2, length.out = 40L),
    state = 1L,
    country_of_birth_sacc = 1101L,
    baseline_income = 70000,
    stringsAsFactors = FALSE
  )
  value <- function(name, description) {
    fplida:::.dil_general_value(
      name, description, "ACLD", "madipge-acld16-d-person-16-21",
      "acld_1621", "Australian Census Longitudinal Dataset", spine,
      20260803L
    )
  }

  parent_age <- value("AGEP_FP_16", "Age of Female Parent")
  expect_true(all(parent_age >= 56L & parent_age <= 76L))
  expect_identical(
    unique(value("SEXP_FP_16", "Sex of Female Parent")), 2L
  )
  expect_identical(
    unique(value("BPLP_FP_21", "Country of Birth of Female Parent")),
    "1101"
  )
  consistency <- value("CFAGEP_11_16", "Age Consistency Flag")
  expect_type(consistency, "integer")
  expect_setequal(unique(consistency), c(0L, 1L))
  occupation <- value("OCCP_21", "Occupation code")
  expect_type(occupation, "character")
  expect_true(all(is.na(occupation)))
  language <- value("LANP_21", "Language used at home")
  expect_type(language, "character")
  expect_true(all(is.na(language)))
})

test_that("Census geography uses official current and historical codeframes", {
  lookup <- fplida:::.load_mb_lookup()
  spine <- data.frame(
    id = 1:40,
    birth_year = 1980L,
    sex = rep(1:2, length.out = 40L),
    state = lookup$state[1:40],
    sa2_code = lookup$sa2_code[1:40],
    sa3_code = as.integer(substr(lookup$sa2_code[1:40], 1L, 5L)),
    sa4_code = lookup$sa4_code[1:40],
    stringsAsFactors = FALSE
  )
  current <- function(name) {
    fplida:::.dil_census_value(
      name, spine, 20260803L, 3L, "census_person_2021"
    )
  }
  historical <- function(name) {
    fplida:::.dil_census_value(
      name, spine, 20260803L, 3L, "census_person_2016"
    )
  }
  current_general <- function(name, description) {
    fplida:::.dil_general_value(
      name = name,
      description = description,
      dataset = "CENSUS",
      product_name = "madipge-cen21-d-person-2021",
      table_name = "census_2021_person",
      module_name = "",
      spine_rows = spine,
      seed = 20260803L
    )
  }

  expect_true(all(current("SA2UCP") %in% lookup$sa2_code))
  expect_true(all(current("SA1UCP") %in% as.character(lookup$sa1_code)))
  expect_true(all(current("MBUCP") %in% as.character(lookup$mb_code)))
  expect_true(all(grepl(
    "^LGA[1-8][0-9]{4}$",
    current_general("POWLGA", "Local government area of workplace")
  )))
  expect_true(all(grepl("^[1-8][0-9]{8}$", historical("SA2UCP"))))
  expect_true(all(grepl("^[1-8][0-9]{6}$", historical("SA1UCP"))))
  expect_true(all(is.na(historical("HDEMP"))))
  expect_gt(mean(current("HDEMP") == "042"), 0.95)
  expect_true(all(grepl("^[0-9]{4}$", current("ANC1P"))))
  expect_true(all(current("IFAGEP") %in% c("1", "2")))
})

test_that("deferred surveys receive schemas without invented fallback codes", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_dil_survey_")
  dir.create(file.path(tmp, "_system"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  n <- 30L
  spine <- data.frame(
    id = seq_len(n),
    spine_id = sprintf("S%010d", seq_len(n)),
    birth_year = rep(1970L, n),
    sex = rep(1:2, length.out = n),
    state = rep(1:8, length.out = n),
    baseline_income = 60000,
    household_id = rep(seq_len(15L), each = 2L),
    aeuid_abs = sprintf("B%011d", seq_len(n)),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(
    spine, file.path(tmp, "_system", "base-spine.parquet")
  )

  result <- fplida:::.complete_plida_dil_structures(
    run_dir = tmp,
    build_order = "pex",
    seed = 20260803L,
    max_rows = 25L,
    verbose = FALSE
  )
  expect_equal(result$structures_written, 1L)

  frame <- as.data.frame(arrow::read_parquet(result$files[[1L]]))
  text_values <- unlist(frame[vapply(frame, is.character, logical(1))],
                        use.names = FALSE)
  text_values <- text_values[!is.na(text_values)]
  expect_false(any(grepl("^C[0-9]{2,4}$", text_values)))
})

test_that("DIL completion honours row caps below 25", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_dil_small_cap_")
  dir.create(file.path(tmp, "_system"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  n <- 30L
  spine <- data.frame(
    id = seq_len(n),
    spine_id = sprintf("S%010d", seq_len(n)),
    birth_year = rep(1970L, n),
    sex = rep(1:2, length.out = n),
    state = rep(1:8, length.out = n),
    baseline_income = 60000,
    household_id = rep(seq_len(15L), each = 2L),
    aeuid_abs = sprintf("B%011d", seq_len(n)),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(
    spine, file.path(tmp, "_system", "base-spine.parquet")
  )

  result <- fplida:::.complete_plida_dil_structures(
    run_dir = tmp,
    build_order = "pex",
    seed = 20260803L,
    max_rows = 7L,
    verbose = FALSE
  )

  expect_true(length(result$rows) > 0L)
  expect_true(all(result$rows <= 7L))
})
