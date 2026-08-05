social_employment_fixture <- function(n = 320L) {
  codeframe_path <- system.file(
    "extdata", "codeframes", "sa2_2021.tsv", package = "fplida"
  )
  if (!nzchar(codeframe_path)) {
    codeframe_path <- testthat::test_path(
      "../../inst/extdata/codeframes/sa2_2021.tsv"
    )
  }
  sa2 <- utils::read.delim(
    codeframe_path, stringsAsFactors = FALSE, colClasses = "character"
  )
  state <- rep(1:8, length.out = n)
  state_code <- vapply(
    as.character(1:8),
    function(value) sa2$code[sa2$state == value][[1L]],
    character(1)
  )
  spine <- data.frame(
    id = seq_len(n),
    spine_id = sprintf("SP%08d", seq_len(n)),
    aeuid_dss = sprintf("DSS%08d", seq_len(n)),
    aeuid_ato = sprintf("ATO%08d", seq_len(n)),
    birth_year = rep(1935:2010, length.out = n),
    month_of_birth = rep(1:12, length.out = n),
    sex = rep(1:2, length.out = n),
    state = state,
    indigenous = rep(c(rep(1L, 17L), 2L, 3L, 4L), length.out = n),
    country_of_birth_sacc = rep(c(1101L, 1201L, 2304L), length.out = n),
    education = rep(0:6, length.out = n),
    baseline_employed = rep(0:1, length.out = n),
    baseline_income = seq(12000, 120000, length.out = n),
    baseline_hours = rep(c(0, 15, 25, 38, 45), length.out = n),
    disability_severity = rep(c(NA, 1:4), length.out = n),
    sa2_code = as.integer(unname(state_code[as.character(state)]))
  )
  list(
    spine = spine,
    source = data.frame(row.names = seq_len(n)),
    period = list(
      start_year = 2023L, end_year = 2024L,
      start = as.Date("2023-07-01"), end = as.Date("2024-06-30")
    )
  )
}

social_employment_register <- function() {
  path <- system.file(
    "internal-docs", "admin-value-remediation-register.csv",
    package = "fplida"
  )
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "../../inst/internal-docs/admin-value-remediation-register.csv"
    )
  }
  values <- utils::read.csv(path, stringsAsFactors = FALSE)
  values[values$dataset %in% c("DEX", "DOMINO", "SAE", "RPS"), ]
}

social_employment_value <- function(dataset, name, fixture,
                                    description = "", product_name = "",
                                    table_name = "", source = fixture$source) {
  helper <- switch(
    dataset,
    DEX = fplida:::.dil_dex_source_value,
    DOMINO = fplida:::.dil_domino_source_value,
    SAE = fplida:::.dil_sae_source_value,
    RPS = fplida:::.dil_rps_source_value
  )
  helper(
    name, description, source, fixture$spine, 20260803L, fixture$period,
    product_name, table_name, dataset
  )
}

test_that("social and employment rules cover the complete target register", {
  fixture <- social_employment_fixture()
  register <- social_employment_register()

  expect_identical(
    setNames(as.integer(table(register$dataset)), names(table(register$dataset))),
    c(DEX = 81L, DOMINO = 80L, RPS = 5L, SAE = 10L)
  )
  expect_identical(sum(register$occurrence_count), 534L)

  for (row in seq_len(nrow(register))) {
    value <- social_employment_value(
      register$dataset[[row]], register$variable[[row]], fixture,
      register$official_descriptions[[row]],
      register$example_product[[row]], register$example_table[[row]]
    )
    label <- paste(register$dataset[[row]], register$variable[[row]])
    expect_false(is.null(value), info = label)
    expect_length(value, nrow(fixture$spine))
    unsupported <- fplida:::.dil_social_is_unsupported_codeframe(
      register$dataset[[row]], register$variable[[row]]
    )
    if (unsupported) {
      expect_identical(
        value, rep(NA_character_, nrow(fixture$spine)), info = label
      )
    } else {
      expect_true(any(!is.na(value)), info = label)
    }
    if (is.character(value) && !unsupported) {
      expect_true(any(nzchar(value[!is.na(value)])), info = label)
    }
  }
})

test_that("unsupported codeframes preserve exact source values only", {
  fixture <- social_employment_fixture()
  register <- social_employment_register()
  unsupported <- register[vapply(
    seq_len(nrow(register)),
    function(row) fplida:::.dil_social_is_unsupported_codeframe(
      register$dataset[[row]], register$variable[[row]]
    ),
    logical(1)
  ), ]

  expect_equal(nrow(unsupported), 76L)
  for (row in seq_len(nrow(unsupported))) {
    source_value <- sprintf(
      "SOURCE_%03d", seq_len(nrow(fixture$spine))
    )
    source <- setNames(
      data.frame(source_value, check.names = FALSE),
      unsupported$variable[[row]]
    )
    observed <- social_employment_value(
      unsupported$dataset[[row]], unsupported$variable[[row]], fixture,
      unsupported$official_descriptions[[row]],
      unsupported$example_product[[row]], unsupported$example_table[[row]],
      source
    )
    expect_identical(observed, source_value)
  }
})

test_that("runtime evidence gates match the decision ledger", {
  path <- system.file(
    "internal-docs",
    "admin-value-decision-ledger-social-employment.csv",
    package = "fplida"
  )
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "../../inst/internal-docs/",
      "admin-value-decision-ledger-social-employment.csv"
    )
  }
  ledger <- utils::read.csv(path, stringsAsFactors = FALSE)

  for (dataset in names(fplida:::.dil_social_unsupported_codeframes)) {
    documented <- ledger$variable[
      ledger$dataset == dataset &
        ledger$status == "unsupported-codeframe"
    ]
    expect_setequal(
      fplida:::.dil_social_unsupported_codeframes[[dataset]],
      documented
    )
  }
})

test_that("DEX values use linked geography, calendar, and SCORE domains", {
  fixture <- social_employment_fixture()
  value <- function(name, table_name = "special_client_assessment") {
    social_employment_value("DEX", name, fixture, table_name = table_name)
  }

  expect_identical(
    value("ANCESTRYCODE"),
    ifelse(fixture$spine$indigenous %in% 2:4, "1102", "1101")
  )
  expect_true(all(grepl("^[1-8][0-9]{2}$", value("FED2022BOUNDARYCODE"))))
  expect_true(all(grepl("^[1-8][0-9]{2}$", value("ACPR2018BOUNDARYCODE"))))
  expect_identical(value("ASSESSEDBY"), rep(
    "SCORE directly – joint", nrow(fixture$spine)
  ))
  expect_identical(value("ASSESSEDBYCODE"), rep(
    "SDJOINT", nrow(fixture$spine)
  ))
  expect_true(all(grepl(
    "^[A-Z]+[1-5]$", value("OUTCOMEDOMAINSCORECODE")
  )))
  expect_true(all(value("SOURCESYSTEMCODE") %in% c("FOFMS", "EXTNL")))
  expect_identical(value("THEDAYNAME"), weekdays(value("THEDAY")))
  expect_identical(
    value("THEDAYOFWEEK"), as.integer(format(value("THEDAY"), "%u"))
  )
  expect_true(all(value("DISABILITYCODE") %in% c(
    "Intellectual/learning", "Psychiatric", "Sensory/speech",
    "Physical/diverse", "None (no disability)",
    "Not stated/inadequately described"
  )))

  source <- data.frame(ASSESSEDBYCODE = rep("SOURCE_CODE", nrow(fixture$spine)))
  expect_identical(
    social_employment_value(
      "DEX", "ASSESSEDBYCODE", fixture, source = source
    ),
    source$ASSESSEDBYCODE
  )
})

test_that("DOMINO public and structural derivations remain coherent", {
  fixture <- social_employment_fixture()
  value <- function(name, table_name = "") {
    social_employment_value("DOMINO", name, fixture, table_name = table_name)
  }

  expect_identical(
    value("INDIG_CODE"),
    ifelse(fixture$spine$indigenous %in% 2:4, "Y", "N")
  )
  expect_identical(value("CNTRY_CDE"), rep("1101", nrow(fixture$spine)))
  expect_identical(value("CTRY_CODE"), rep("1101", nrow(fixture$spine)))
  expect_identical(value("OBJECT_TYPE_CODE"), rep(
    "PER", nrow(fixture$spine)
  ))
  expect_identical(
    value("BIRTH_CTRY_CODE"),
    sprintf("%04d", fixture$spine$country_of_birth_sacc)
  )
  expect_true(all(grepl("^[1-8][0-9]{2}$", value("CED"))))
  continuous_hours <- value("EMP_INC_CONT_HRS")
  incapacity_hours <- value("INCAP_WK_WRK_HRS")
  expect_true(all(continuous_hours >= 0L & continuous_hours <= 80L))
  expect_true(all(incapacity_hours >= 0L & incapacity_hours <= 30L))
  expect_true(all(grepl("^E[0-9]+$", value("EMPLYR_ID"))))
})

test_that("SAE retains only its supported reporting-period fallback", {
  fixture <- social_employment_fixture()
  value <- function(name, source = fixture$source) {
    social_employment_value("SAE", name, fixture, source = source)
  }

  expect_identical(value("EXTRACT_REF"), rep(
    "FY2023-24", nrow(fixture$spine)
  ))
})

test_that("RPS values use official LGA codes", {
  fixture <- social_employment_fixture()
  value <- function(name, table_name = "") {
    social_employment_value("RPS", name, fixture, table_name = table_name)
  }

  lga <- fplida:::.dil_load_lga_codeframe()
  for (year in c(2023L, 2024L)) {
    observed <- value(paste0("LGA_CODE_", year))
    expect_true(all(observed %in% lga$code[lga$year == year]))
  }
})
