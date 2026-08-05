test_that("NACDC product sources use coherent official value domains", {
  skip_if_not_installed("arrow")

  tmp <- file.path(tempdir(), paste0("nacdc_values_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  old_run_dir <- getOption("fplida.run_dir")
  on.exit(options(fplida.run_dir = old_run_dir), add = TRUE)

  spine <- generate_spine(n = 12000L, seed = 219L, output_dir = tmp)
  recipient <- generate_nacdc(
    spine = spine, seed = 219L, output_dir = tmp, return_data = TRUE
  )
  run_dir <- getOption("fplida.run_dir")
  ds_dir <- fplida:::dataset_dir(run_dir, "NACDC")
  products <- c(
    aged_recipient = "madipge-aged-care-d-agedrecipient-15-current",
    aged_service = "madipge-aged-care-d-agedservice-15-current",
    chsp = "madipge-aged-care-d-chsp-16-current",
    hcp = "madipge-aged-care-d-hcp-15-current",
    rac = "madipge-aged-care-d-rac-15-current",
    tcp = "madipge-aged-care-d-tcp-15-current"
  )
  paths <- stats::setNames(
    file.path(ds_dir, paste0(unname(products), ".parquet")),
    names(products)
  )
  expect_true(all(file.exists(paths)))

  source <- lapply(paths, function(path) {
    as.data.frame(arrow::read_parquet(path), stringsAsFactors = FALSE)
  })
  expect_true(all(vapply(source, nrow, integer(1)) > 0L))

  marital <- c(
    D = "Divorced",
    M = "Married (registered or de facto)",
    P = "Separated",
    S = "Never Married",
    W = "Widowed"
  )
  expect_true(all(recipient$MARITAL_STATUS_CODE %in% names(marital)))
  expect_identical(
    unname(marital[recipient$MARITAL_STATUS_CODE]),
    recipient$MARITAL_STATUS_DESC
  )
  expect_identical(recipient$MARITAL_STATUS, recipient$MARITAL_STATUS_DESC)
  expect_true(all(recipient$INDIGENOUS_STATUS %in% as.character(1:4)))
  expect_true(all(recipient$STATE %in% c(
    "NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"
  )))
  expect_true(all(recipient$CARE_TYPE %in% c(
    "Commonwealth Home Support Program", "Home Care Packages Program",
    "Residential care", "Transition Care Program"
  )))
  expect_true(all(
    is.na(recipient$HCP_LEVEL) |
      recipient$HCP_LEVEL %in% paste("LEVEL", 1:4)
  ))
  expect_identical(
    is.na(recipient$HCP_LEVEL),
    recipient$CARE_TYPE != "Home Care Packages Program"
  )
  expect_true(all(
    is.na(recipient$EXIT_DATE) | recipient$EXIT_DATE >= recipient$ENTRY_DATE
  ))

  legacy_languages <- c(
    `02` = "English", `72` = "Mandarin", `38` = "Arabic",
    `65` = "Vietnamese", `67` = "Cantonese", `48` = "Punjabi",
    `12` = "Greek", `13` = "Italian", `46` = "Hindi",
    `16` = "Spanish", `96` = "Not stated/Inadequately described"
  )
  expect_identical(
    unname(legacy_languages[source$aged_recipient$PREFERRED_LANGUAGE_CODE]),
    source$aged_recipient$PREFERRED_LANGUAGE_DESC
  )

  service <- source$aged_service
  expect_false(anyDuplicated(service$SERVICE_ID_ENCRYPTED) > 0L)
  expect_true(all(grepl("^PROV[0-9A-F]{16}$", service$PROVIDER_ID_ENCRYPTED)))
  expect_true(all(grepl("^SERV[0-9A-F]{16}$", service$SERVICE_ID_ENCRYPTED)))
  expect_true(all(service$PROVIDER_ORGANISATION_TYPE %in% c(
    "Charitable", "Community Based", "Local Government",
    "Private Incorporated Body", "Publicly Listed Company", "Religious",
    "Religious/Charitable", "State Government", "Territory Government",
    "Unknown"
  )))
  expect_true(all(service$PROVIDER_PURPOSE %in% c(
    "For profit", "Government", "Not for profit"
  )))
  expect_true(all(service$PROVIDER_INCORPORATED_BODY %in% c("Y", "N", "U")))
  expect_true(all(service$SERVICE_APPROVAL_CARE_TYPE %in% c(
    "Commonwealth Home Support Program (CHSP)", "Home Care",
    "Residential", "Flexible"
  )))
  expect_true(all(service$SERVICE_STATUS %in% c(
    "Operational", "Inactive", "Suspended", "Offline"
  )))
  expect_true(all(service$SERVICE_TYPE %in% c(
    "Commonwealth Home Support Program", "Home Care",
    "Residential", "Transition Care"
  )))
  expect_true(all(grepl("^[1-9][0-9]{8}$", service$SERVICE_SA2_CODE)))
  expect_true("SERVICE_SA2_NAME" %in% names(service))
  sa2 <- utils::read.delim(
    system.file(
      "extdata", "codeframes", "sa2_2021.tsv", package = "fplida"
    ),
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE
  )
  expected_sa2_name <- sa2$name[match(service$SERVICE_SA2_CODE, sa2$code)]
  expect_false(anyNA(expected_sa2_name))
  expect_identical(service$SERVICE_SA2_NAME, expected_sa2_name)

  current_languages <- c(
    `1201` = "English", `7104` = "Mandarin", `4202` = "Arabic",
    `6302` = "Vietnamese", `7101` = "Cantonese", `5207` = "Punjabi",
    `2201` = "Greek", `2401` = "Italian", `5203` = "Hindi",
    `2303` = "Spanish", `&&&&` = "Not stated/Inadequately described"
  )
  chsp <- source$chsp
  expect_identical(
    unname(current_languages[chsp$PREFERRED_LANGUAGE_CODE]),
    chsp$PREFERRED_LANGUAGE_DESC
  )
  expect_true(all(chsp$CARER_EXISTENCE %in% c("Y", "N")))
  expect_true(all(as.integer(chsp$ASSISTANCE_MINUTES_NBR) %in%
                    c(15L, 30L, 45L, 60L, 90L, 120L, 180L, 240L)))
  chsp_subtypes <- c(
    "Allied Health and Therapy Services" = "Physiotherapy",
    "Assistance with Care and Housing" = "Client Advocacy",
    "Centre-based Respite" = "At Centre",
    "Cottage Respite" = "Overnight Community Respite",
    "Domestic Assistance" = "General House Cleaning",
    "Flexible Respite" = "In Home/Community",
    "Goods, Equipment and Assistive Technology" = "Support and mobility aids",
    "Home Maintenance" = "Minor Home Maintenance and Repairs",
    "Home Modifications" = "Car Modification",
    "Meals" = "Food Preparation in the Home",
    "Nursing" = "Continence Advisory Services",
    "Other Food Services" = "Food Advice, Lessons, Training, Food Safety",
    "Personal Care" = "Assistance with Self-Care",
    "Social Support Group" = "Community Access - Group",
    "Social Support Individual" = "Accompanied Activities e.g Shopping",
    "Specialised Support Services" = "Dementia Advisory Services",
    "Transport" = "Direct (driver is volunteer or worker)"
  )
  expect_identical(
    unname(chsp_subtypes[chsp$SERVICE_TYPE_DESC]),
    chsp$SERVICE_SUBTYPE_DESC
  )
  expect_false("SERVICE_TYPE_CODE" %in% names(chsp))

  hcp <- source$hcp
  expect_true(all(hcp$CARE_APPROVAL_LEVEL %in% paste("LEVEL", 1:4)))
  expect_identical(hcp$CARE_APPROVAL_LEVEL, hcp$ENTRY_CARE_LEVEL)
  expect_identical(hcp$CARE_APPROVAL_LEVEL, hcp$HCP_LEVEL)
  expect_true(all(hcp$CARE_APPROVAL_TYPE == "Home Care"))
  expect_true(all(hcp$ENTRY_TYPE == "HOME CARE"))
  expect_true(all(hcp$FIRST_CONTACT_SETTING %in% c(
    "Hospital (acute care)", "Other inpatient setting",
    "Residential aged care service", "Other",
    "Not stated/inadequately described"
  )))
  expect_true(all(hcp$LEAVE_REASON %in% c(
    "Hospital", "Respite", "Social", "Transition care"
  )))
  expect_identical(is.na(hcp$EXIT_REASON), is.na(hcp$EXIT_DATE))
  expect_true(all(stats::na.omit(hcp$EXIT_REASON) %in% c(
    "Death", "Other", "Return to community", "To hospital",
    "To residential aged care"
  )))

  rac <- source$rac
  expect_identical(is.na(rac$EXIT_REASON), is.na(rac$EXIT_DATE))
  expect_true(all(rac$LEAVE_REASON %in% c(
    "Emergency", "Transition care", "Social", "Pre-entry", "Hospital"
  )))
  expect_true(all(stats::na.omit(rac$EXIT_REASON) %in% c(
    "To other residential aged care", "To hospital", "Return to community",
    "Other", "Death"
  )))

  tcp <- source$tcp
  entry_score <- as.integer(tcp$FUNCTIONAL_CAPACITY_ENTRY)
  exit_score <- as.integer(tcp$FUNCTIONAL_CAPACITY_EXIT)
  expect_true(all(entry_score >= 0L & entry_score <= 100L))
  expect_identical(is.na(tcp$FUNCTIONAL_CAPACITY_EXIT), is.na(tcp$EXIT_DATE))
  expect_true(all(exit_score[!is.na(exit_score)] >= entry_score[!is.na(exit_score)]))
  expect_true(all(exit_score[!is.na(exit_score)] <= 100L))

  for (product in c("hcp", "rac", "tcp")) {
    expect_true(all(source[[product]]$SERVICE_ID_ENCRYPTED %in%
                      service$SERVICE_ID_ENCRYPTED))
  }
  expect_true(all(chsp$PROVIDER_ID_ENCRYPTED %in% service$PROVIDER_ID_ENCRYPTED))
})

test_that("NACDC canonical tables receive every source-supported gap value", {
  skip_if_not_installed("arrow")

  tmp <- file.path(tempdir(), paste0("nacdc_canonical_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  old_run_dir <- getOption("fplida.run_dir")
  on.exit(options(fplida.run_dir = old_run_dir), add = TRUE)

  spine <- generate_spine(n = 5000L, seed = 421L, output_dir = tmp)
  generate_nacdc(
    spine = spine, seed = 421L, output_dir = tmp, return_data = FALSE
  )
  run_dir <- getOption("fplida.run_dir")
  completed <- fplida:::.complete_plida_dil_structures(
    run_dir = run_dir,
    build_order = "nacdc",
    seed = 421L,
    max_rows = 1250L,
    verbose = FALSE
  )
  expect_equal(completed$structures_written, 57L)

  gaps <- utils::read.csv(
    system.file(
      "internal-docs", "admin-value-gap-register.csv", package = "fplida"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  gaps <- gaps[gaps$dataset == "NACDC", , drop = FALSE]
  unresolved <- "SERVICE_TYPE_CODE"
  resolved <- gaps[!gaps$variable %in% unresolved, , drop = FALSE]
  expect_equal(nrow(gaps), 275L)
  expect_equal(length(unique(gaps$variable)), 29L)
  expect_equal(nrow(resolved), 260L)
  expect_equal(length(unique(resolved$variable)), 28L)
  expect_equal(sum(gaps$variable %in% unresolved), 15L)

  ds_dir <- fplida:::dataset_dir(run_dir, "NACDC")
  for (i in seq_len(nrow(resolved))) {
    path <- file.path(ds_dir, paste0(resolved$logical_output[[i]], ".parquet"))
    expect_true(file.exists(path), info = resolved$logical_output[[i]])
    frame <- as.data.frame(arrow::read_parquet(path))
    variable <- resolved$variable[[i]]
    expect_true(variable %in% names(frame), info = resolved$logical_output[[i]])
    value <- as.character(frame[[variable]])
    expect_true(
      any(!is.na(value) & nzchar(value)),
      info = paste(resolved$logical_output[[i]], variable)
    )
  }
})
