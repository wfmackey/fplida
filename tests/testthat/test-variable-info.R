variable_info_columns <- c(
  "occurrence_id", "asset", "record_type", "collection_type", "dataset",
  "dataset_name", "module", "product", "table", "table_number",
  "source_sheet", "table_scope", "source_item", "variable",
  "variable_level", "official_description", "variable_description",
  "description_source", "description_source_url", "description_provenance",
  "variable_type",
  "variable_type_source", "reference_period", "available_periods",
  "official_valid_response", "value_kind", "value_domain", "valid_values",
  "value_definition", "value_source", "value_source_url", "value_provenance",
  "value_support_status", "limitation", "occurrence_count", "product_count",
  "table_count", "topic_tags", "metadata_source", "metadata_vintage"
)

variable_info_topics <- c(
  "id", "date_time", "demographic", "family_household", "geography",
  "housing", "income", "taxation", "superannuation", "employment",
  "payroll", "education_training", "health", "disability_caring",
  "social_security", "aged_care", "migration_citizenship", "travel",
  "births_deaths", "business", "industry", "finance_accounting",
  "innovation_research", "digital_technology", "trade",
  "intellectual_property", "agriculture", "energy_environment",
  "legal_insolvency", "program_service_delivery", "survey_design",
  "data_quality"
)

variable_info_test_cache <- new.env(parent = emptyenv())

variable_info_test_data <- function() {
  if (!exists("variables", envir = variable_info_test_cache, inherits = FALSE)) {
    assign("variables", variable_info(), envir = variable_info_test_cache)
  }
  get("variables", envir = variable_info_test_cache, inherits = FALSE)
}

dataset_info_test_data <- function() {
  if (!exists("datasets", envir = variable_info_test_cache, inherits = FALSE)) {
    assign("datasets", dataset_info(), envir = variable_info_test_cache)
  }
  get("datasets", envir = variable_info_test_cache, inherits = FALSE)
}

registry_values <- function(info, dataset, variable, table = NULL) {
  rows <- info$dataset == dataset &
    toupper(info$variable) == toupper(variable)
  if (!is.null(table)) rows <- rows & info$table == table
  values <- unique(info$valid_values[rows])
  expect_length(values, 1L)
  as.character(unlist(jsonlite::fromJSON(values[[1L]]), use.names = FALSE))
}

test_that("the variable registry has the official occurrence surface", {
  info <- variable_info_test_data()

  expect_identical(names(info), variable_info_columns)
  expect_equal(nrow(info), 72656L)
  expect_equal(sum(info$asset == "PLIDA"), 67398L)
  expect_equal(
    sum(info$asset == "BLADE" & info$record_type == "variable"),
    5246L
  )
  expect_equal(
    sum(info$asset == "BLADE" & info$record_type == "linking_key"),
    12L
  )
  expect_true(all(nzchar(info$occurrence_id)))
  expect_equal(anyDuplicated(info$occurrence_id), 0L)
})

test_that("support status follows collection type", {
  info <- variable_info_test_data()

  expect_setequal(
    unique(info$value_support_status),
    c("sourced", "guessed", "unsupported", "not_applicable")
  )
  # not_applicable remains survey-only, but the converse no longer holds: a
  # survey occurrence whose value domain can be inferred from its name is
  # `guessed` instead.
  expect_true(all(
    info$collection_type[info$value_support_status == "not_applicable"] ==
      "survey"
  ))
  expect_true(all(
    info$value_support_status[info$collection_type == "survey"] %in%
      c("not_applicable", "guessed")
  ))
  expect_true(all(info$occurrence_count > 0L))
  expect_true(all(info$product_count > 0L))
  expect_true(all(info$table_count > 0L))
  # A survey occurrence carries a value list only where its domain was guessed
  # from the variable name; the registry still publishes none of its own.
  survey <- info$collection_type == "survey"
  expect_true(all(info$valid_values[survey & info$value_support_status ==
                                      "not_applicable"] == "[]"))
})

test_that("valid values are JSON arrays", {
  skip_if_not_installed("jsonlite")
  info <- variable_info_test_data()

  parsed <- lapply(unique(info$valid_values), jsonlite::fromJSON)
  expect_true(all(vapply(
    parsed,
    function(values) !length(values) || is.character(values),
    logical(1)
  )))
})

test_that("topic tags use the controlled vocabulary and canonical order", {
  info <- variable_info_test_data()
  row_topics <- strsplit(info$topic_tags, ",", fixed = TRUE)

  expect_true(all(nzchar(info$topic_tags)))
  expect_setequal(unique(unlist(row_topics)), variable_info_topics)
  expect_true(all(vapply(
    row_topics,
    function(tags) {
      positions <- match(tags, variable_info_topics)
      all(!is.na(positions)) && !anyDuplicated(tags) &&
        (length(positions) < 2L || all(diff(positions) > 0L))
    },
    logical(1)
  )))
})

test_that("PIT support counts apply to distinct variables", {
  info <- variable_info_test_data()
  pit <- info[info$dataset %in% c("PIT_ITR", "PIT_PS", "PIT_IE"), ]
  pair_key <- paste(pit$dataset, pit$variable, sep = "\r")

  status_count <- tapply(
    pit$value_support_status,
    pair_key,
    function(status) length(unique(status))
  )
  expect_true(all(status_count == 1L))

  pairs <- pit[!duplicated(pair_key), , drop = FALSE]
  expect_equal(
    as.integer(table(factor(
      pairs$dataset,
      levels = c("PIT_ITR", "PIT_PS", "PIT_IE")
    ))),
    c(601L, 41L, 18L)
  )

  sourced <- pairs$value_support_status == "sourced"
  guessed <- pairs$value_support_status == "guessed"
  unsupported <- pairs$value_support_status == "unsupported"
  expect_equal(
    as.integer(tapply(sourced, pairs$dataset, sum)[
      c("PIT_ITR", "PIT_PS", "PIT_IE")
    ]),
    c(59L, 10L, 16L)
  )
  expect_equal(
    as.integer(tapply(guessed, pairs$dataset, sum)[
      c("PIT_ITR", "PIT_PS", "PIT_IE")
    ]),
    c(541L, 31L, 2L)
  )
  # sourced + guessed + unsupported must account for every distinct variable.
  expect_equal(
    as.integer(tapply(sourced | guessed | unsupported, pairs$dataset, sum)[
      c("PIT_ITR", "PIT_PS", "PIT_IE")
    ]),
    c(601L, 41L, 18L)
  )
  expect_equal(
    as.integer(tapply(unsupported, pairs$dataset, sum)[
      c("PIT_ITR", "PIT_PS", "PIT_IE")
    ]),
    c(1L, 0L, 0L)
  )
})

test_that("dataset information covers all registry datasets and websites", {
  variables <- variable_info_test_data()
  datasets <- dataset_info_test_data()

  expect_identical(
    names(datasets),
    c(
      "asset", "collection_type", "dataset", "dataset_name", "supplier",
      "supplier_name", "custodian", "dataset_description", "reference_period",
      "update_frequency", "information_source", "information_url",
      "information_summary", "metadata_source", "metadata_vintage"
    )
  )
  expect_equal(nrow(datasets), 44L)
  expect_equal(anyDuplicated(datasets$dataset), 0L)
  expect_setequal(datasets$dataset, variables$dataset)
  expect_true(all(nzchar(datasets$information_source)))
  expect_true(all(grepl("^https://", datasets$information_url)))
  expect_false(any(grepl("[[:space:]]", datasets$information_url)))
})

test_that("registry accessors use exact case-insensitive filters", {
  datasets <- dataset_info(dataset = "pit_itr")
  expect_identical(datasets$dataset, "PIT_ITR")
  expect_equal(nrow(dataset_info(asset = "plida")), 43L)

  pit_income <- variable_info(dataset = "pit_itr", topic = "INCOME")
  expect_true(nrow(pit_income) > 0L)
  expect_true(all(pit_income$dataset == "PIT_ITR"))
  expect_true(all(vapply(
    strsplit(pit_income$topic_tags, ",", fixed = TRUE),
    function(tags) "income" %in% tags,
    logical(1)
  )))

  blade_survey <- variable_info(
    asset = "blade", collection_type = "SURVEY", record_type = "Variable"
  )
  expect_equal(nrow(blade_survey), 2794L)
  expect_true(all(blade_survey$asset == "BLADE"))
  expect_true(all(blade_survey$collection_type == "survey"))
  expect_true(all(blade_survey$record_type == "variable"))

  unsupported <- variable_info(value_support_status = "UNSUPPORTED")
  expect_true(nrow(unsupported) > 0L)
  expect_true(all(unsupported$value_support_status == "unsupported"))

  expect_error(dataset_info(dataset = "NOT_A_DATASET"), "Unknown `dataset`")
  expect_error(dataset_info(asset = "NOT_AN_ASSET"), "Unknown `asset`")
  expect_error(variable_info(topic = "incom"), "complete topic tag")
  expect_error(
    variable_info(collection_type = "sample"),
    "Unknown `collection_type`"
  )
  expect_error(variable_info(record_type = "key"), "Unknown `record_type`")
  expect_error(
    variable_info(value_support_status = "partial"),
    "Unknown `value_support_status`"
  )
})

test_that("administrative descriptions do not use a missing-description label", {
  info <- variable_info_test_data()
  administrative <- info$collection_type == "administrative"

  expect_false(any(grepl(
    "does not supply a variable description",
    info$variable_description[administrative],
    fixed = TRUE
  )))
})

test_that("AEDC finite values use source-defined domains", {
  skip_if_not_installed("jsonlite")
  info <- variable_info_test_data()

  expect_identical(registry_values(info, "AEDC", "A1"), as.character(1:4))
  expect_identical(
    registry_values(info, "AEDC", "A10"),
    c("1", "2", "3", "88")
  )
  expect_identical(registry_values(info, "AEDC", "A1A"), c("0", "1"))
  expect_identical(
    registry_values(info, "AEDC", "REMOTENESS"),
    c(
      "Major Cities of Australia", "Inner Regional Australia",
      "Outer Regional Australia", "Remote Australia",
      "Very Remote Australia"
    )
  )
  expect_identical(
    registry_values(info, "AEDC", "IREGPUBLIC"),
    c("1", "5", "9", "21", "22", "23")
  )
  expect_identical(
    registry_values(info, "AEDC", "CYCLE"),
    c(
      "1: 2009", "2: 2012", "3: 2015", "4: 2018", "5: 2021",
      "6: 2024"
    )
  )
  expect_identical(registry_values(info, "AEDC", "MOC"), as.character(4:9))
  expect_identical(
    registry_values(info, "AEDC", "SCHOOLTYPE"),
    c("C", "G", "I")
  )
})

test_that("DEX finite values exclude implementation encodings", {
  skip_if_not_installed("jsonlite")
  info <- variable_info_test_data()

  # ASSESSEDBYCODE holds the code, and only one code mnemonic (SDJOINT) is
  # public — the rest sit behind a login-gated reference file. Its label twin
  # ASSESSEDBY carries the eight published categories, and putting those
  # strings here would write label text into a code column.
  expect_identical(registry_values(info, "DEX", "ASSESSEDBYCODE"), character())
  expect_identical(registry_values(info, "DEX", "PHYSICAL"), character())
  expect_identical(
    registry_values(info, "DEX", "THEDAYOFWEEK"), as.character(1:7)
  )
  expect_identical(
    registry_values(info, "DEX", "NDISELIGIBILITYCODE"),
    c("NDIS in-progress access request", "NDIS eligible", "NDIS ineligible")
  )
  expect_identical(
    registry_values(info, "DEX", "ASSESSEDBY"),
    c(
      "SCORE directly – client", "SCORE directly – practitioner",
      "SCORE directly – joint", "SCORE directly – support person",
      "Validated outcomes tool – client",
      "Validated outcomes tool – practitioner",
      "Validated outcomes tool – joint",
      "Validated outcomes tool – support person"
    )
  )
  expect_identical(registry_values(info, "DEX", "CLIENTLOCALITY"), character())
  expect_identical(registry_values(info, "DEX", "OUTLETLOCALITY"), character())
  localities <- info$dataset == "DEX" & toupper(info$variable) %in%
    c("CLIENTLOCALITY", "OUTLETLOCALITY")
  # What matters is that a suburb name is never given a finite code list, not
  # the wording of the domain: research renamed these from "open text domain"
  # to say whose locality each one is.
  expect_true(all(trimws(info$valid_values[localities]) == "[]"))
  expect_true(all(grepl("locality", info$value_domain[localities],
                        ignore.case = TRUE)))
  expect_true(all(info$value_support_status[localities] == "sourced"))
  # None of these three has a published codeframe, so none may claim `sourced`.
  # They are `guessed` now rather than `unsupported`: a day-of-week column
  # taking 1-7 is useful and honestly labelled, and the status carries the
  # warning that the mapping is inferred.
  incomplete_codeframes <- info$dataset == "DEX" &
    toupper(info$variable) %in%
      c("ASSESSEDBYCODE", "SOURCESYSTEMCODE", "THEDAYOFWEEK")
  expect_true(all(
    info$value_support_status[incomplete_codeframes] == "guessed"
  ))
  expect_length(registry_values(info, "DEX", "REFERRALFROMSOURCE"), 22L)
  expect_identical(
    registry_values(
      info, "DEX", "OUTCOMETYPE", "special_client_assessment"
    ),
    c("Circumstances", "Goals", "Satisfaction")
  )
  expect_identical(
    registry_values(
      info, "DEX", "OUTCOMETYPE", "special_community_assessment"
    ),
    "Community"
  )
  state_values <- registry_values(info, "DEX", "STE2021BOUNDARYCODE")
  expect_true("9: Other Territories" %in% state_values)
})

test_that("BLADE normalisation retains complete response domains", {
  skip_if_not_installed("jsonlite")
  info <- variable_info_test_data()
  blade <- info[
    info$asset == "BLADE" & info$collection_type == "administrative",
  ]

  binary <- blade$official_valid_response ==
    "Numeric - Binary 0 - No 1 - Yes"
  expect_true(any(binary))
  expect_true(all(vapply(
    blade$valid_values[binary],
    function(values) identical(
      jsonlite::fromJSON(values), c("0: No", "1: Yes")
    ),
    logical(1)
  )))

  # The BLADE Data Item List points these at external appendices. Those
  # appendices were tracked down — they are the ABS international merchandise
  # trade workbook and the IP Australia data dictionary — so the domains are
  # published and the variables are `sourced`.
  #
  # Five still carry no list. Four are the trade classifications, whose
  # published lists run to thousands of commodity lines and are recorded by
  # reference. The fifth is the IP `classification` column, a union across
  # seven classification systems whose combined size nobody measured, so only
  # a sample was ever available and a sample is not a domain.
  #
  # An empty list here therefore means "documented, not carried", which is why
  # the status must still be `sourced` rather than `unsupported`.
  missing_appendix <- grepl(
    "Appendix|Appendices", blade$official_valid_response, ignore.case = TRUE
  ) & blade$valid_values == "[]"
  expect_equal(sum(missing_appendix), 5L)
  expect_true(all(
    blade$value_support_status[missing_appendix] == "sourced"
  ))
  expect_true(all(grepl(
    "too large to list here|not listed here",
    blade$value_definition[missing_appendix]
  ) | grepl(
    "not listed here", blade$limitation[missing_appendix]
  )))

  trade_units <- c(
    "BC", "CM", "CT", "CU", "G", "IU", "KG", "L", "LA", "M", "MC",
    "NO", "NR", "PR", "SM", "SR", "T", "TH"
  )
  expect_identical(
    registry_values(info, "BLADE", "unit_of_quantity_ex"),
    trade_units
  )
  expect_identical(
    registry_values(info, "BLADE", "unit_of_quantity_im"),
    trade_units
  )
  expect_identical(
    registry_values(info, "BLADE", "geocode_precision"),
    c(
      "1: coded address passed acceptance criteria at the ARID level",
      "2: coded address passed acceptance criteria at the MB level",
      "3: coded address passed acceptance criteria at the SA2 level"
    )
  )
  expect_identical(
    registry_values(info, "BLADE", "unions"),
    c(
      "1: Unions covered by the agreement",
      "0: No unions covered by the agreement"
    )
  )

  table_49_flags <- blade$table_number == "49" &
    blade$official_valid_response == "Numeric 1 = Yes 0 = No"
  expect_true(any(table_49_flags))
  expect_true(all(vapply(
    blade$valid_values[table_49_flags],
    function(values) identical(
      jsonlite::fromJSON(values), c("1: Yes", "0: No")
    ),
    logical(1)
  )))
})

test_that("public registries exclude generation and development fields", {
  variables <- variable_info_test_data()
  datasets <- dataset_info_test_data()
  prohibited_fields <- c(
    "output_state", "implementation_class", "value_rule",
    "generation_rule", "generation_method", "generated_domain",
    "generated_frequency", "generated_weight", "next_action"
  )

  expect_length(intersect(names(variables), prohibited_fields), 0L)
  expect_length(intersect(names(datasets), prohibited_fields), 0L)

  variable_text <- do.call(
    paste,
    c(variables[c(
      "variable_description", "description_source", "value_definition",
      "value_source", "limitation"
    )], sep = " ")
  )
  dataset_text <- do.call(
    paste,
    c(datasets[c(
      "dataset_description", "information_source", "information_summary"
    )], sep = " ")
  )
  prohibited_wording <- paste(
    c(
      "placeholder", "previous version", "prior version", "remediat",
      "release[- ]audit", "generation rule", "implementation class"
    ),
    collapse = "|"
  )
  expect_false(any(grepl(
    prohibited_wording, variable_text, ignore.case = TRUE
  )))
  expect_false(any(grepl(
    prohibited_wording, dataset_text, ignore.case = TRUE
  )))
})
