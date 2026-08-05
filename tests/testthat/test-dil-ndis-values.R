ndis_value_test_spine <- function(n = 100L) {
  data.frame(
    id = seq_len(n),
    birth_year = rep(c(1955L, 1980L, 2002L, 2010L, 2018L), length.out = n),
    sex = rep(c(1L, 2L), length.out = n),
    state = rep(1:8, length.out = n),
    indigenous = rep(c(1L, 1L, 1L, 2L), length.out = n),
    country_of_birth_sacc = rep(
      c(1101L, 1201L, 4102L, 6101L, 7103L), length.out = n
    ),
    person_type = rep(1:27, length.out = n),
    disability_severity = rep(1:4, length.out = n),
    stringsAsFactors = FALSE
  )
}

ndis_value <- function(name, table = "ndis_participantdemographics",
                       product = "madipge-ndis-exp-d-participants-13-current",
                       n = 100L, source = NULL) {
  spine <- ndis_value_test_spine(n)
  if (is.null(source)) source <- data.frame(.row = seq_len(n))
  fplida:::.dil_ndis_source_value(
    name = name,
    description = "",
    source_frame = source,
    spine_rows = spine,
    seed = 20260803L,
    period = list(
      start_year = 2024L, end_year = 2024L,
      start = as.Date("2024-01-01"), end = as.Date("2024-12-31")
    ),
    product_name = product,
    table_name = table,
    module_name = "NDIS expanded participants"
  )
}

test_that("NDIS remediation classifies every original gap entry", {
  gap_path <- system.file(
    "internal-docs", "admin-value-gap-register.csv", package = "fplida"
  )
  if (!nzchar(gap_path)) {
    gap_path <- file.path(
      "inst", "internal-docs", "admin-value-gap-register.csv"
    )
  }
  gap <- utils::read.csv(gap_path, check.names = FALSE)
  gap <- gap[gap$dataset == "NDIS", , drop = FALSE]

  expect_equal(nrow(gap), 1099L)
  expect_equal(length(unique(gap$variable)), 104L)
  expect_setequal(
    unique(gap$variable),
    c(
      fplida:::.dil_ndis_supported_gap_variables,
      fplida:::.dil_ndis_structural_variables
    )
  )
  expect_equal(
    sum(gap$variable %in% fplida:::.dil_ndis_supported_gap_variables),
    1093L
  )
  expect_equal(
    sum(gap$variable %in% fplida:::.dil_ndis_structural_variables),
    6L
  )
})

test_that("NDIS participant values use documented domains", {
  expect_true(all(ndis_value("CALDSTS") %in% c("Y", "N")))
  expect_true(all(ndis_value("ACTVPRTCPNTIND") %in% c("Y", "N")))
  expect_true(all(ndis_value("EVERELIGBLIND") == "Y"))
  expect_true(all(ndis_value("GNDRTYP") %in% c("M", "F")))
  expect_true(all(ndis_value("NDISMMMSCD") %in% as.character(1:7)))
  expect_true(all(ndis_value("REMOTENESS_DESCRIPTION_MMM") %in%
                    c("City", "Rural", "Remote", "Very remote")))
  expect_true(all(ndis_value("RACD2011") %in%
                    as.character(as.vector(outer(1:8 * 10L, 0:4, `+`)))))
  expect_true(all(ndis_value("SVRTYSCR") >= 0 & ndis_value("SVRTYSCR") <= 1))
  expect_true(all(nchar(ndis_value("POSTCD")) == 4L))

  expected_groups <- c(
    "Autism", "Hearing Impairment", "Intellectual Disability",
    "Multiple Sclerosis", "Other", "Other Neurological", "Other Physical",
    "Other Sensory/Speech", "Psychosocial disability", "Spinal Cord Injury",
    "Stroke", "Visual Impairment"
  )
  expect_true(all(ndis_value("NDISDSBLTYGRPNM") %in% expected_groups))
})

test_that("NDIS goal, plan support, and payment values are coherent", {
  category <- ndis_value(
    "SUPPCATNM", table = "ndis_supportcatalogue",
    product = "madipge-ndis-exp-d-plansupports-13-current"
  )
  item <- ndis_value(
    "SUPPITEMDESC", table = "ndis_supportcatalogue",
    product = "madipge-ndis-exp-d-plansupports-13-current"
  )
  price <- as.numeric(ndis_value(
    "UNITPRICE", table = "ndis_supportcatalogue",
    product = "madipge-ndis-exp-d-plansupports-13-current"
  ))
  expect_false(anyNA(category))
  expect_false(anyNA(item))
  expect_true(all(nzchar(item)))
  expect_true(all(is.finite(price) & price > 0))

  management <- ndis_value(
    "PLANMGTMTHDDESC", table = "ndis_plansupport",
    product = "madipge-ndis-exp-d-plansupports-13-current"
  )
  expect_true(all(management %in% c(
    "Agency Managed", "Plan Managed", "Self Managed Fully",
    "Self Managed Partly", "Not recorded"
  )))

  core <- ndis_value("CORE", table = "ndis_goals")
  capacity <- ndis_value("CAPACITY_BUILDING", table = "ndis_goals")
  capital <- ndis_value("CAPITAL", table = "ndis_goals")
  expect_equal((core == "Y") + (capacity == "Y") + (capital == "Y"),
               rep(1L, length(core)))

  expect_equal(
    unique(ndis_value("FY_CLAIM", table = "ndis_payments_2024_2")),
    2024L
  )
  expect_equal(
    unique(ndis_value("FY_CLAIM", table = "ndis_payments_2024_3")),
    2025L
  )
})

test_that("NDIS outcome fields agree within each synthetic response", {
  table <- "ndis_participantoutcomes_2024_4"
  product <- "madipge-ndis-exp-d-outcomes-13-current"
  form <- ndis_value("FORM", table, product)
  question <- as.integer(ndis_value("QUESTION", table, product))
  answer <- ndis_value("ANSWER", table, product)
  timing <- ndis_value("PLANTIMING", table, product)
  timing_n <- as.integer(ndis_value("PLANTIMING_N", table, product))
  entry <- ndis_value("ENTRYQRTR", table, product)

  expect_true(all(form %in% c(
    "Participant 0 to before school", "Participant starting school to 14",
    "Participant 15 to 24", "Participant 25 and over"
  )))
  expect_true(all(question >= 1L & question <= 15L))
  expect_true(all(answer %in% c("1", "2", "3")))
  expect_equal(
    timing,
    c("Baseline", "First review", "Second review", "Third review")[
      timing_n + 1L
    ]
  )
  expect_match(entry, "^20[0-9]{2}Q[1-4]$")
})

test_that("NDIS unsupported fields remain visibly structural", {
  for (name in fplida:::.dil_ndis_structural_variables) {
    expect_true(
      all(is.na(ndis_value(name))),
      info = paste(name, "must remain typed missing")
    )
  }
})

test_that("NDIS fallback domains contain no placeholder labels", {
  inspect <- setdiff(
    fplida:::.dil_ndis_supported_gap_variables,
    c("LGANM2021", "PMTRQSTPRVDR_BN", "PRVDR_BN")
  )
  placeholders <- "^(unknown|tbd|todo|placeholder|dummy|not supplied)$"
  for (name in inspect) {
    value <- ndis_value(name)
    expect_false(is.null(value), info = paste("No rule for", name))
    observed <- tolower(trimws(as.character(value[!is.na(value)])))
    expect_true(length(observed) > 0L, info = paste("All missing:", name))
    expect_false(any(grepl(placeholders, observed)), info = name)
  }
})
