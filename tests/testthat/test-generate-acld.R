acld_expect_registered_domains <- function(data, product) {
  registry <- fplida:::.acld_codeframe_registry()
  mapping <- registry$mapping[
    registry$mapping$product == product & registry$mapping$supported,
    ,
    drop = FALSE
  ]
  expect_true(all(mapping$variable %in% names(data)))

  for (i in seq_len(nrow(mapping))) {
    variable <- mapping$variable[[i]]
    frame <- registry$values[
      registry$values$frame_id == mapping$frame_id[[i]],
      ,
      drop = FALSE
    ]
    observed <- as.character(data[[variable]])
    ranges <- frame$code[frame$value_kind == "range"]
    if (length(ranges)) {
      bounds <- as.integer(strsplit(ranges[[1L]], "-", fixed = TRUE)[[1L]])
      number <- suppressWarnings(as.integer(observed))
      valid <- is.na(observed) |
        (!is.na(number) & number >= bounds[[1L]] & number <= bounds[[2L]]) |
        observed %in% frame$code[frame$value_kind != "range"]
    } else {
      valid <- is.na(observed) | observed %in% frame$code
    }
    expect_true(all(valid), info = variable)
  }
}

test_that("official ACLD workbooks cover every DIL variable explicitly", {
  registry <- fplida:::.acld_codeframe_registry()
  mapping <- registry$mapping
  values <- registry$values

  expect_equal(nrow(mapping), 1401L)
  expect_equal(
    as.integer(table(mapping$product)),
    c(840L, 561L)
  )
  expect_equal(sum(mapping$supported), 1193L)
  expect_equal(sum(!mapping$supported), 208L)
  expect_equal(anyDuplicated(paste(values$frame_id, values$code)), 0L)
  expect_true(all(grepl("^https://www\\.abs\\.gov\\.au/", mapping$source_url)))
  expect_true(all(mapping$source_release_date == "2023-12-19"))
  expect_true(all(nchar(mapping$workbook_sha256) == 64L))

  supported_frames <- unique(mapping$frame_id[mapping$supported])
  has_domain <- vapply(supported_frames, function(frame_id) {
    any(values$value_kind[values$frame_id == frame_id] %in%
          c("substantive", "range"))
  }, logical(1))
  expect_true(all(has_domain))

  unsupported <- mapping[!mapping$supported, , drop = FALSE]
  decisions <- vapply(
    unsupported$variable,
    fplida:::.acld_unsupported_decision,
    character(1)
  )
  expect_equal(
    as.integer(table(factor(decisions, levels = c(
      "existing_manual", "public_asgs_geography",
      "public_census_classification", "unresolved"
    )))),
    c(7L, 25L, 137L, 39L)
  )
})

test_that("ACLD remediation resolves 750 original missing occurrences", {
  gap_path <- system.file(
    "internal-docs", "admin-value-gap-register.csv", package = "fplida"
  )
  if (!nzchar(gap_path)) {
    gap_path <- file.path("inst", "internal-docs", "admin-value-gap-register.csv")
  }
  gap <- utils::read.csv(gap_path, stringsAsFactors = FALSE, check.names = FALSE)
  gap <- gap[gap$dataset == "ACLD", , drop = FALSE]
  mapping <- fplida:::.acld_codeframe_registry()$mapping
  matched <- match(
    paste(gap$official_product, toupper(gap$variable)),
    paste(mapping$product, toupper(mapping$variable))
  )
  manual <- gap$variable %in% c(
    "LEVEL", "WEIGHT4_11_16", "WEIGHT4_11_16_21", "WEIGHT4_16_21"
  )
  decision <- vapply(
    mapping$variable[matched],
    fplida:::.acld_unsupported_decision,
    character(1)
  )
  public <- decision %in% c(
    "public_census_classification", "public_asgs_geography"
  )
  resolved <- mapping$supported[matched] | manual | public

  expect_equal(sum(resolved), 750L)
  expect_equal(sum(!resolved), 16L)
  expect_equal(length(unique(gap$variable[resolved])), 458L)
  expect_equal(length(unique(gap$variable[!resolved])), 11L)
  expect_setequal(unique(gap$variable[!resolved]), c(
    "IAREA_UR_11", "IAREA_UR_16", "IEO_UR_21",
    "MBUCD_11", "MBUCD_16", "MBUCP_11", "MBUCP_16",
    "POWP_11", "POWP_16", "RA_UR_11", "RA_UR_16"
  ))
})

test_that("ACLD decision ledger accounts for every original occurrence", {
  path <- system.file(
    "internal-docs", "acld-admin-value-decisions.csv", package = "fplida"
  )
  if (!nzchar(path)) {
    path <- file.path(
      "inst", "internal-docs", "acld-admin-value-decisions.csv"
    )
  }
  ledger <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  expect_equal(nrow(ledger), 507L)
  expect_equal(anyDuplicated(paste(ledger$dataset, ledger$variable)), 0L)
  expect_true(all(ledger$dataset == "ACLD"))
  expect_equal(sum(ledger$original_gap_occurrence_count), 766L)
  expect_equal(sum(ledger$workbook_unsupported_occurrence_count), 208L)
  expect_equal(sum(ledger$existing_resolved_gap_occurrence_count), 625L)
  expect_equal(sum(ledger$new_public_resolved_gap_occurrence_count), 125L)
  expect_equal(sum(ledger$remaining_unresolved_gap_occurrence_count), 16L)
  expect_true(all(nzchar(ledger$exact_rationale)))
  expect_true(all(grepl(
    "^https://(?:www|geo)\\.abs\\.gov\\.au/",
    ledger$evidence_url,
    perl = TRUE
  )))
})

test_that("Rust ACLD projection uses official unlinked codes", {
  n <- 20000L
  raw <- fplida:::project_acld__(
    aeuid = sprintf("A%010d", seq_len(n)),
    birth_year = rep(1980L, n),
    sex = rep(1L, n),
    state = rep(1L, n),
    indigenous = rep(1L, n),
    country_of_birth = rep(0L, n),
    education = rep(5L, n),
    baseline_employed = rep(1L, n),
    baseline_income = rep(80000, n),
    seed = 3401L
  )
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  unlinked_16 <- raw$LINKFLAG_16 == 2L
  expect_true(any(unlinked_16))
  expect_true(all(raw$AGEP_16[unlinked_16] == 999L))
  expect_true(all(raw$SEXP_16[unlinked_16] == 99L))
  expect_true(all(raw$INGP_16[unlinked_16] == 99L))
  expect_true(all(raw$MSTP_16[unlinked_16] == 99L))
  expect_true(all(raw$LFSP_16[unlinked_16] == 99L))
  expect_true(all(raw$HSCP_16[unlinked_16] == 99L))
  expect_true(all(raw$INCP_16[unlinked_16] == 999L))
  expect_true(all(raw$ASSNP_16[unlinked_16] == 99L))
  expect_false(any(vapply(
    raw,
    function(value) any(as.character(value) == "9999", na.rm = TRUE),
    logical(1)
  )))
})

test_that("generate_acld writes independent, source-backed panel products", {
  skip_if_not_installed("arrow")
  tmp <- file.path(tempdir(), paste0("acld_codeframes_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 8000L, seed = 81L, output_dir = tmp)
  acld_1121 <- generate_acld(
    spine = spine,
    seed = 81L,
    output_dir = tmp,
    return_data = TRUE
  )
  run_dir <- getOption("fplida.run_dir")
  product_1121 <- "madipge-acld11-d-persons-11-16-21"
  product_1621 <- "madipge-acld16-d-person-16-21"
  acld_1621 <- as.data.frame(arrow::read_parquet(file.path(
    run_dir, "abs-acld", paste0(product_1621, ".parquet")
  )))

  expect_gt(nrow(acld_1121), 200L)
  expect_gt(nrow(acld_1621), 200L)
  expect_true(all(spine$birth_year[
    match(acld_1121$SYNTHETIC_AEUID, spine$aeuid_abs)
  ] <= 2011L))
  expect_true(all(spine$birth_year[
    match(acld_1621$SYNTHETIC_AEUID, spine$aeuid_abs)
  ] <= 2016L))
  expect_lt(
    mean(acld_1121$SYNTHETIC_AEUID %in% acld_1621$SYNTHETIC_AEUID),
    0.20
  )

  acld_expect_registered_domains(acld_1121, product_1121)
  acld_expect_registered_domains(acld_1621, product_1621)

  for (product in c(product_1121, product_1621)) {
    data <- if (product == product_1121) acld_1121 else acld_1621
    public <- fplida:::.acld_codeframe_registry()$mapping
    public <- public[
      public$product == product & !public$supported &
        vapply(
          public$variable,
          function(variable) fplida:::.acld_unsupported_decision(variable) %in%
            c("public_census_classification", "public_asgs_geography"),
          logical(1)
        ),
      , drop = FALSE
    ]
    expect_true(all(public$variable %in% names(data)))
    expect_true(all(vapply(
      public$variable,
      function(variable) any(!is.na(data[[variable]]) &
        nzchar(as.character(data[[variable]]))),
      logical(1)
    )))
  }

  public_registry <- fplida:::.acld_public_registry()
  acld_status <- c("99997", "99998", "99999")
  expect_true(all(acld_1121$BPLP_21 %in% c(
    public_registry$sacc$code, acld_status
  )))
  expect_true(all(acld_1121$LANP_21 %in% c(
    public_registry$language$code, acld_status
  )))
  expect_true(all(acld_1121$INDP_21 %in% c(
    public_registry$anzsic$anzsic_class_code, acld_status
  )))
  expect_true(all(grepl(
    "^(?:[0-9]{6}|999999[789])$", acld_1121$OCCP_21, perl = TRUE
  )))
  expect_true(all(grepl(
    "^(?:[0-9]{6}|999999[789])$", acld_1121$QALFP_21, perl = TRUE
  )))
  expect_true(all(acld_1121$LGA_UR_11 %in% c(
    public_registry$lga$code[public_registry$lga$year == 2011L], "999999"
  )))
  expect_true(all(grepl(
    "^(?:[0-9]{11}|999999999999)$", acld_1121$MBUCP_21, perl = TRUE
  )))
  expect_true(all(grepl(
    "^(?:[0-9]{9}|9999999999)$", acld_1121$SA2UCP_21, perl = TRUE
  )))
  expect_true(all(acld_1121$LEVEL == "Person"))
  expect_true(all(acld_1621$LEVEL == "Person"))
  expect_true(all(acld_1121$WEIGHT4_11_16 > 0))
  expect_true(all(acld_1121$WEIGHT4_11_16_21 > 0))
  expect_true(all(acld_1621$WEIGHT4_16_21 > 0))

  spine_1121 <- spine[match(acld_1121$SYNTHETIC_AEUID, spine$aeuid_abs), ]
  expect_equal(
    as.integer(acld_1121$AGEP_11),
    pmin(2011L - as.integer(spine_1121$birth_year), 85L)
  )
  linked_16 <- acld_1121$LINKFLAG_16 == "1"
  expect_equal(
    as.integer(acld_1121$AGEP_16[linked_16]),
    pmin(2016L - as.integer(spine_1121$birth_year[linked_16]), 85L)
  )
  expect_true(all(acld_1121$AGEP_16[!linked_16] == "999"))
  expect_true(all(acld_1121$SEXP_16[!linked_16] == "99"))
  expect_true(all(acld_1121$INCP_16[!linked_16] == "999"))

  both <- acld_1121$LINKFLAG_16 == "1" & acld_1121$LINKFLAG_21 == "1"
  income_11 <- as.integer(acld_1121$INCP_11[both])
  income_16 <- as.integer(acld_1121$INCP_16[both])
  expect_gt(stats::cor(income_11, income_16), 0.75)
})

test_that("ACLD unresolved public-workbook rows stay typed missing", {
  unresolved <- fplida:::.acld_unresolved_variables()
  expect_equal(length(unresolved), 27L)
  expect_true(all(c(
    "IAREA_UR_11", "IAREA_UR_16", "IEO_UR_21",
    "MBUCD_11", "MBUCD_16", "MBUCP_11", "MBUCP_16",
    "POWP_11", "POWP_16", "RA_UR_11", "RA_UR_16"
  ) %in% unresolved))

  spine <- data.frame(
    id = 1:4, birth_year = 1980L, sex = 1L, state = 1L,
    indigenous = 1L, country_of_birth_sacc = 1101L,
    year_of_arrival = 2000L, citizenship = 1L,
    baseline_income = 60000, baseline_hours = 38L,
    baseline_employed = TRUE, education = 3L, anzsco_code = 261313L,
    anzsco_major = 2L, skill_level = 1L, industry = 10L,
    disability_onset_year = NA_integer_, disability_severity = 9L,
    residence_seed = 1:4, sa2_code = 101021007L,
    household_id = 1:4, stringsAsFactors = FALSE
  )
  for (variable in unresolved) {
    value <- fplida:::.dil_acld_source_value(
      variable, data.frame(.row = 1:4), spine, 81L,
      product_name = "madipge-acld11-d-persons-11-16-21",
      table_name = "acld_1121", module_name = "ACLD"
    )
    expect_true(all(is.na(value)), info = variable)
  }
})
