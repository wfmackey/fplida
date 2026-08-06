aedc_value_fixture <- function(n = 800L, year = 2024L) {
  spine <- data.frame(
    spine_id = seq_len(n),
    state = rep(1:8, length.out = n),
    birth_year = rep(year - 5L, n),
    sex = rep(1:2, length.out = n),
    stringsAsFactors = FALSE
  )
  source <- data.frame(
    YEAR = rep(year, n),
    AGEINMONTHS = rep(40:99, length.out = n),
    STATE = rep(
      c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"),
      length.out = n
    ),
    ATSITYPE = rep(c(1L, 2L, 3L, 4L), length.out = n),
    LANG = rep(c(1L, 0L), length.out = n),
    CANCOM = rep(c(1L, 0L), length.out = n),
    SPECIALNEEDS = rep(c(1L, rep(0L, 9L)), length.out = n),
    A1 = rep(1:4, length.out = n),
    A1A = rep(0:1, length.out = n),
    A1B = rep(1:0, length.out = n),
    A1C = 0L,
    A1D = 0L,
    E2Y = rep(c(1L, 2L, 88L), length.out = n),
    E3AY = rep(c(1L, 4L, 88L), length.out = n),
    SEIFADECILE = rep(1:10, length.out = n),
    PHYSCATEGORY = rep(1:4, length.out = n),
    SOCCATEGORY = rep(1:4, length.out = n),
    EMOTCATEGORY = rep(1:4, length.out = n),
    LANGCOGCATEGORY = rep(1:4, length.out = n),
    COMGENCATEGORY = rep(1:4, length.out = n),
    stringsAsFactors = FALSE
  )
  list(
    spine = spine,
    source = source,
    period = list(end_year = year)
  )
}

aedc_test_value <- function(fixture, name, seed = 811L) {
  .dil_aedc_source_value(
    name = name,
    description = "",
    source_frame = fixture$source,
    spine_rows = fixture$spine,
    seed = seed,
    period = fixture$period
  )
}

test_that("AEDC structural and direct values use public domains", {
  fixture <- aedc_value_fixture()

  expect_setequal(unique(aedc_test_value(fixture, "MOC")), 4:9)
  for (name in c(
    "AGCAMPUSID", "COMMUNITYID", "JCAMPUSID", "LOCALCOMMUNITYID",
    "LOCATIONAGEID", "REGIONID", "SCHOOLAGEID", "SCHOOLID",
    "TEACHERID"
  )) {
    value <- aedc_test_value(fixture, name)
    expect_false(anyNA(value), info = name)
    expect_gt(anyDuplicated(value), 0L)
    expect_identical(value, aedc_test_value(fixture, name), info = name)
  }

  expect_setequal(unique(aedc_test_value(fixture, "AGECAT")), 0:15)
  expect_setequal(
    unique(aedc_test_value(fixture, "AGEGROUP")),
    c("<5", "5", "6", ">6")
  )
  expect_true(all(
    aedc_test_value(fixture, "DAYCARE") %in% c(0L, 1L, 88L)
  ))
  expect_true(all(
    aedc_test_value(fixture, "DAYCARENO") %in% c(0L, 1L, 88L)
  ))
  expect_true(all(
    aedc_test_value(fixture, "PSDC") %in% c(0L, 1L, 88L)
  ))
  expect_true(all(
    aedc_test_value(fixture, "PRESCHOOL") %in% c(0L, 1L, NA)
  ))
  expect_true(all(aedc_test_value(fixture, "SEIFAEXCLUDED") == 0L))

  categories <- as.matrix(fixture$source[c(
    "PHYSCATEGORY", "SOCCATEGORY", "EMOTCATEGORY",
    "LANGCOGCATEGORY", "COMGENCATEGORY"
  )])
  on_track <- rowSums(categories >= 3L)
  for (threshold in 1:4) {
    expect_identical(
      aedc_test_value(fixture, paste0("OT", threshold)),
      as.integer(on_track >= threshold)
    )
  }
  expect_identical(
    aedc_test_value(fixture, "ONTRACK0"),
    as.integer(on_track == 0L)
  )
  for (threshold in 1:5) {
    expect_identical(
      aedc_test_value(fixture, paste0("ONTRACK", threshold)),
      as.integer(on_track >= threshold)
    )
  }
})

test_that("AEDC public instrument values preserve filters and cycle codes", {
  current <- aedc_value_fixture(year = 2024L)
  historical <- aedc_value_fixture(year = 2018L)

  for (name in c("A1Z", "A1AZ", "A1BZ", "A1CZ", "A1DZ")) {
    expect_true(all(
      aedc_test_value(current, name)[!is.na(aedc_test_value(current, name))] %in%
        0:9
    ), info = name)
  }
  for (name in paste0("B1", LETTERS[1:4])) {
    current_value <- aedc_test_value(current, name)
    historical_value <- aedc_test_value(historical, name)
    expect_true(all(current_value[!is.na(current_value)] %in%
                      c(1L, 2L, 3L, 88L, 99L)), info = name)
    expect_true(all(historical_value[!is.na(historical_value)] %in%
                      c(1L, 2L, 3L, 88L, 999L)), info = name)
    expect_false(any(historical_value == 99L, na.rm = TRUE), info = name)
  }
  expect_true(all(is.na(
    aedc_test_value(current, "B1A")[current$source$ATSITYPE == 4L]
  )))

  consult <- aedc_test_value(current, "CONSULT")
  role <- aedc_test_value(current, "CONSULTROLE")
  expect_true(all(consult %in% c(0L, 1L)))
  expect_true(all(role[!is.na(role)] %in% 1:4))
  expect_true(all(is.na(role[consult == 0L])))

  for (name in paste0("D", 1:9)) {
    expect_true(all(aedc_test_value(current, name) %in%
                      c(0L, 1L, 2L, 88L)), info = name)
  }
  early <- aedc_value_fixture(year = 2012L)
  for (name in paste0("D", 1:9)) {
    expect_true(all(aedc_test_value(early, name) %in%
                      c(0L, 3L, 88L)), info = name)
  }
  expect_true(all(aedc_test_value(current, "D10") %in% c(0L, 1L, 88L)))
  expect_true(all(aedc_test_value(current, "D11") %in% c(0L, 1L, 88L)))
  expect_true(all(aedc_test_value(current, "DEVDIFF") %in% 0:1))
  expect_true(all(aedc_test_value(current, "E1") %in% c(0L, 1L, 88L)))

  diagnoses <- c(
    paste0("DIAGNOSIS", c(1L, 3L, 4L, 7:21)),
    "DIAGNOSIS6A", paste0("DIAGNOSIS", 22:32)
  )
  diagnosis_values <- vapply(
    diagnoses,
    function(name) aedc_test_value(current, name),
    integer(nrow(current$spine))
  )
  expect_true(all(diagnosis_values %in% 0:1))
  non_special <- current$source$SPECIALNEEDS == 0L
  expect_true(all(diagnosis_values[non_special, , drop = FALSE] == 0L))
  expect_true(all(rowSums(
    diagnosis_values[!non_special, , drop = FALSE]
  ) >= 1L))

  for (name in paste0("LANGSOURCE", 0:5)) {
    value <- aedc_test_value(current, name)
    expect_true(all(value[!is.na(value)] %in% 0:2), info = name)
  }
  for (name in c(
    "PARENT1SCHOOL", "PARENT1POSTSCHOOL",
    "PARENT2SCHOOL", "PARENT2POSTSCHOOL"
  )) {
    expect_true(all(aedc_test_value(current, name) %in%
                      c(1L, 2L, 3L, 4L, 88L)), info = name)
  }
})

test_that("AEDC fields the registry cannot document stay typed missing", {
  fixture <- aedc_value_fixture()

  # The blocklist is the list of AEDC fields the first value review could not
  # resolve. Research against the AEDC data dictionary resolved most of them,
  # so the blocklist now defers: a name the registry documents falls through to
  # the generic fallback, which draws from that domain.
  #
  # A name the registry documents nothing for must still end at typed missing.
  # That is what stops the fallback inventing codes for a field nobody has
  # been able to pin down.
  documented <- vapply(
    .dil_aedc_blocked_names,
    function(name) !is.null(.registry_values_for("AEDC", name)),
    logical(1)
  )
  expect_true(any(documented))
  expect_true(any(!documented))

  for (name in .dil_aedc_blocked_names[!documented]) {
    value <- .dil_dataset_source_value(
      name = name,
      description = "Indicator score category",
      dataset = "AEDC",
      source_frame = fixture$source,
      spine_rows = fixture$spine,
      seed = 811L,
      period = fixture$period,
      product_name = "madipge-aedc-d-core-2024",
      table_name = "aedc_2024_core"
    )
    expect_true(all(is.na(value)), info = name)
    expect_type(value, "character")
  }
})

test_that("AEDC canonical frame combines exact, derived and blocked values", {
  fixture <- aedc_value_fixture(n = 200L)
  fixture$source$CYCLE <- 6L
  fixture$source$REMOTENESS <- rep(
    c(
      "Major Cities of Australia", "Inner Regional Australia",
      "Outer Regional Australia", "Remote Australia",
      "Very Remote Australia"
    ),
    length.out = nrow(fixture$source)
  )
  fixture$source$REMOTENESSCODE <- rep(1:5, length.out = nrow(fixture$source))
  variables <- c(
    "CYCLE", "REMOTENESS", "REMOTENESSCODE", "MOC", "SCHOOLID",
    "AGECAT", "D1", "DIAGNOSIS7", "SEIFAEXCLUDED", "MSI"
  )
  rows <- data.frame(
    `Variable Name` = variables,
    `Variable Description` = variables,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  frame <- .dil_make_structure_frame(
    variable_rows = rows,
    source_frame = fixture$source,
    spine_rows = fixture$spine,
    dataset = "AEDC",
    product_name = "madipge-aedc-d-core-2024",
    table_name = "aedc_2024_core",
    module_name = "Australian Early Development Census 2024",
    seed = 811L
  )

  expect_identical(names(frame), variables)
  expect_true(all(frame$CYCLE == 6L))
  expect_true(all(frame$REMOTENESSCODE %in% 1:5))
  expect_true(all(frame$MOC %in% 4:9))
  expect_false(anyNA(frame$SCHOOLID))
  expect_true(all(frame$AGECAT %in% 0:15))
  expect_true(all(frame$D1 %in% c(0L, 1L, 2L, 88L)))
  expect_true(all(frame$DIAGNOSIS7 %in% 0:1))
  expect_true(all(frame$SEIFAEXCLUDED == 0L))
  expect_true(all(is.na(frame$MSI)))
})
