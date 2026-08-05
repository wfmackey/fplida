home_affairs_value_fixture <- function(n = 400L) {
  spine <- generate_spine(n = n, seed = 20260803L)
  subclass <- rep(c(500L, 190L, 820L, 200L, 482L), length.out = n)
  source <- data.frame(
    ENROLLED_FY = rep(2016:2019, length.out = n),
    INITIAL_ASLPR_SPEAK = rep(0:2, length.out = n),
    INITIAL_ASLPR_LISTEN = rep(0:2, length.out = n),
    INITIAL_ASLPR_READ = rep(0:2, length.out = n),
    INITIAL_ASLPR_WRITE = rep(0:2, length.out = n),
    LATEST_ASLPR_SPEAK = rep(1:3, length.out = n),
    LATEST_ASLPR_LISTEN = rep(1:3, length.out = n),
    LATEST_ASLPR_READ = rep(1:3, length.out = n),
    LATEST_ASLPR_WRITE = rep(1:3, length.out = n),
    AGENCYID = sprintf("%012d", rep(1:20, length.out = n)),
    AGENCY_FUNCTION = rep(
      c("Policy", "Larger operational", "Regulatory", "Specialist"),
      length.out = n
    ),
    DISABILITY = rep(c(FALSE, FALSE, TRUE), length.out = n),
    EMPLOYMENT_STATUS = rep(
      c("Ongoing", "Non-ongoing - specified term"), length.out = n
    ),
    FATHERS_LANGUAGE = rep(c("1201", "7104", "4202"), length.out = n),
    FIRST_LANGUAGE = rep(c("1201", "7104", "4202"), length.out = n),
    MOTHERS_LANGUAGE = rep(c("1201", "7104", "4202"), length.out = n),
    HIGHEST_EDUCATION_COUNTRY = rep(c("1101", "7103"), length.out = n),
    PREVIOUS_EMPLOYMENT = rep(
      c("Private sector", "State government", "Student"), length.out = n
    ),
    ADR_TYP = rep(c("R", "P"), length.out = n),
    BIRTH_CNTRY_CDE = as.integer(spine$country_of_birth_sacc),
    DOB_M = as.integer(spine$month_of_birth),
    DOB_Y = as.integer(spine$birth_year),
    MARITAL_STS = rep(c("Single", "Married", "De facto"), length.out = n),
    SOURCE = rep(c("VISA", "SETTLE"), length.out = n),
    CITIZENSHIP = as.integer(spine$country_of_birth_sacc),
    PRINCIPAL_FLAG = rep(c("P", "S", "S"), length.out = n),
    MONTH_OF_BIRTH = as.integer(spine$month_of_birth),
    YEAR_OF_BIRTH = as.integer(spine$birth_year),
    ENG_FLUENCY_SPUK = rep(as.character(1:4), length.out = n),
    MARITAL_STATUS = rep(c("Single", "Married"), length.out = n),
    VISA_SUB_CLASS = subclass,
    VISA_SUBCLASS_CD = subclass,
    VISA_SUBCLASS_DS = rep(
      c("Student", "Skilled Nominated", "Partner", "Refugee",
        "Skills in Demand"), length.out = n
    ),
    VISA_PRGRM_DS = rep(
      c("Temporary Visa Program", "Migration Program", "Migration Program",
        "Humanitarian Program", "Temporary Visa Program"), length.out = n
    ),
    VISA_REPORT_GROUP_DS = rep(
      c("Student", "Skilled", "Family", "Humanitarian", "Skilled"),
      length.out = n
    ),
    VA_PRIMARY_FL = rep(c("Y", "N", "Y"), length.out = n),
    VA_CLNT_LOCN_CD = rep(c(3L, 4L), length.out = n),
    TR_CITZ_CNTRY_CD = as.integer(spine$country_of_birth_sacc),
    TR_STAY_PERIOD_DD = rep(c(1460L, 0L, 0L, 0L, 1095L), length.out = n),
    NM_OCPTN_CD = sprintf("%06d", as.integer(spine$anzsco_code)),
    IELTS_OVRL = rep(c(6.5, 7, 0, 0, 6), length.out = n),
    stringsAsFactors = FALSE
  )
  list(
    spine = spine,
    source = source,
    period = list(
      start_year = 2003L, end_year = 2019L,
      start = as.Date("2003-01-01"), end = as.Date("2019-12-31")
    )
  )
}

test_that("AMEP assessments, streams, awards, and client fields are coherent", {
  fixture <- home_affairs_value_fixture()
  value <- function(name) {
    fplida:::.dil_amep_source_value(
      name, fixture$source, fixture$spine, 20260803L, fixture$period
    )
  }

  indicators <- c(
    "LEARNING_01", "LEARNING_02", "READING_03", "READING_04",
    "WRITING_05", "WRITING_06", "ORAL_COMMUNICATION_07",
    "ORAL_COMMUNICATION_08"
  )
  for (indicator in indicators) {
    initial <- value(paste0("INITIAL_", indicator))
    latest <- value(paste0("LATEST_", indicator))
    expect_true(all(initial %in% 1:3), info = indicator)
    expect_true(all(latest %in% 1:5), info = indicator)
    expect_true(all(latest >= initial), info = indicator)
  }

  expect_true(all(value("AWARD_CODE") %in%
                    c("10725NAT", "10727NAT", "10728NAT", "10729NAT", "10730NAT")))
  expect_true(all(grepl("Spoken and Written English", value("AWARD_NAME"))))
  expect_identical(value("ID"), paste0("ARMS-CSWE-", value("AWARD_CODE")))
  expect_true(all(value("CSWE_LEVEL") %in%
                    c("Preliminary", "Certificate I", "Certificate II",
                      "Certificate III", "Certificate IV")))
  expect_true(all(value("INITIAL_STREAM") %in%
                    c("Pre-Employment English", "Social English")))
  expect_true(all(value("CURRENT_STREAM") %in%
                    c("Pre-Employment English", "Social English")))
  expect_true(all(grepl("^[0-9]{4}$", value("LANGUAGE"))))
  expect_false(any(value("LANGUAGE") == "1201"))
  expect_true(all(grepl("^AMEP-[A-Z]{2,3}-[0-9]{3}$",
                        value("REGISTRATION_CENTRE"))))
  expect_true(all(value("VISA_SUBCLASS") %in%
                    fplida:::.dil_ha_codeframe("visa_subclass.tsv")$code))
})

test_that("APSED public classifications and local operational codes are usable", {
  fixture <- home_affairs_value_fixture()
  value <- function(name) {
    fplida:::.dil_apsed_source_value(
      name, fixture$source, fixture$spine, 20260803L, fixture$period
    )
  }

  substantive <- value("SUBSTANTIVE_CLASSIFICATION")
  acting <- value("ACTING_CLASSIFICATION")
  classifications <- c(
    "Trainee", "Graduate", paste("APS", 1:6), paste("EL", 1:2),
    paste("SES", 1:3)
  )
  expect_true(all(substantive %in% classifications))
  expect_true(anyNA(acting))
  expect_true(any(!is.na(acting)))
  expect_true(all(stats::na.omit(acting) %in% classifications))
  expect_true(all(value("AGENCY_SIZE") %in%
                    c("Micro", "Extra Small", "Small", "Medium", "Large",
                      "Extra Large")))

  expect_true(all(value("JOBFAMILY_CODE") %in%
                    c("SD", "CR", "AD", "PP", "PO", "ICT", "DR", "AF",
                      "HR", "LP")))
  expect_false(anyNA(value("JOBFAMILY")))
  expect_false(anyNA(value("JOBFAMILY_FUNCTION")))
  expect_false(anyNA(value("JOBFAMILY_ROLE")))
  expect_true(all(value("HIGHEST_EDUCATION_QUALIFICATION") %in%
                    c("998", "621", "611", "421", "310", "120")))
  expect_match(value("FIELD_OF_STUDY_MAIN"), "^[0-9]{4}$")
  expect_true(any(!is.na(value("FIELD_OF_STUDY_OTHER"))))
  expect_true(all(grepl(
    "^[0-9]{4}$", stats::na.omit(value("FIELD_OF_STUDY_OTHER"))
  )))
  expect_true(all(value("LEAVE_CODE") %in% c("A", "M", "L", "I", "O")))
  expect_true(all(value("MOVEMENT_CODE") %in%
                    c("CLS", "AGY", "LOC", "HRS", "SEP", "ENG")))
})

test_that("APSED education values retain one value per row without a spine education field", {
  fixture <- home_affairs_value_fixture(25L)
  fixture$spine$education <- NULL

  qualification <- fplida:::.dil_apsed_education_value(
    "HIGHEST_EDUCATION_QUALIFICATION",
    fixture$spine,
    20260803L
  )
  field <- fplida:::.dil_apsed_education_value(
    "FIELD_OF_STUDY_MAIN",
    fixture$spine,
    20260803L
  )

  expect_length(qualification, nrow(fixture$spine))
  expect_length(field, nrow(fixture$spine))
  expect_identical(qualification, rep("611", nrow(fixture$spine)))
  expect_match(field, "^[0-9]{4}$", all = TRUE)
})

test_that("settlement and migrant-demographic additions preserve semantics", {
  fixture <- home_affairs_value_fixture()
  sdb <- function(name) {
    fplida:::.dil_sdb_source_value(
      name, fixture$source, fixture$spine, 20260803L
    )
  }
  mt <- function(name) {
    fplida:::.dil_mt_demogs_source_value(
      name, fixture$source, fixture$spine, 20260803L
    )
  }

  expect_match(sdb("CSM_SERV_REQ_ID"), "^CSR[0-9]{12}$")
  expect_match(sdb("VISA_GRANT_NO"), "^VG[0-9]{12}$")
  expect_match(sdb("DOB_MMMYYYY"), "^[A-Z]{3}[0-9]{4}$")
  expect_true(all(sdb("LOCATION") %in% c("3", "4")))
  dependent <- sdb("DEPENDENT_CODE")
  expect_true(anyNA(dependent))
  expect_true(any(!is.na(dependent)))
  expect_true(all(stats::na.omit(dependent) %in% c("S", "C", "O")))
  expect_match(sdb("PREF_LANG"), "^[0-9]{4}$")
  expect_true(all(sdb("RELIGION") %in%
                    fplida:::.dil_ha_codeframe("ascrg_religion.tsv")$code))
  expect_false(anyNA(sdb("ETHNICITY")))

  expect_identical(mt("CNTRY_CDE"), rep("1101", nrow(fixture$spine)))
  expect_identical(mt("MARITAL_STATUS"), fixture$source$MARITAL_STS)
  expect_identical(mt("SRC_CD"), ifelse(fixture$source$SOURCE == "VISA", "V", "S"))
})

test_that("visa application additions are conditional and mutually coherent", {
  fixture <- home_affairs_value_fixture()
  value <- function(name) {
    fplida:::.dil_visa_source_value(
      name, fixture$source, fixture$spine, 20260803L
    )
  }
  context <- fplida:::.dil_ha_visa_context(
    fixture$source, fixture$spine, 20260803L
  )

  coe1 <- value("COE1_COURSE_CD")
  expect_true(all(!is.na(coe1[context$student & context$primary])))
  expect_true(all(is.na(coe1[!context$student | !context$primary])))
  expect_match(stats::na.omit(coe1), "^[0-9]{6}[A-Z]$")
  expect_match(stats::na.omit(value("COE1_PRVDR_CD")), "^[0-9]{5}[A-Z]$")
  expect_match(stats::na.omit(value("COE1_PRVDR_ABN_TX")), "^H[0-9]{15}$")
  expect_true(all(stats::na.omit(value("COE1_COURSE_LEVEL_DS")) %in% c(
    "Schools Sector",
    "English Language Intensive Courses for Overseas Students",
    "Vocational Education and Training Sector", "Higher Education Sector",
    "Postgraduate Research Sector"
  )))

  expect_identical(value("TR_VISA_GRANT_NUMBER"), value("VA_VISA_GRANT_NR"))
  expect_match(value("TR_APLCTN_ID"), "^APP[0-9]{12}$")
  expect_match(value("TR_VISA_GRANT_NUMBER"), "^VG[0-9]{12}$")
  expect_identical(value("TR_VISA_SUBCLASS_CD"), context$subclass)

  nominated <- context$skilled & context$primary &
    as.integer(fixture$spine$anzsco_code) != 0L
  expect_true(all(!is.na(value("NM_OCPTN_FULL_DS")[nominated])))
  expect_true(all(value("NM_OCPTN_TYPE_CD")[nominated] == "ANZSCO"))
  expect_true(all(value("VA_OCPTN_TYPE_CD") %in% c("ANZSCO", NA_character_)))

  proficiency <- value("IELTS_ENGLISH_PROF")
  expect_true(all(stats::na.omit(proficiency) %in%
                    c("Limited", "Modest", "Competent", "Proficient",
                      "Very high")))
  expect_true(all(value("VA_SOURCE_SYSTEM_CD") %in%
                    c("TRIPS", "ICSE", "IRIS", "MPMS")))
  expect_identical(value("VA_TYPE_L2_LONG_DS"), context$description)
})

test_that("native settlement dates and migrant address types are realistic", {
  skip_if_not_installed("arrow")
  output_dir <- file.path(
    tempdir(), paste0("fplida_home_affairs_native_", Sys.getpid())
  )
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  spine <- generate_spine(
    n = 5000L, seed = 20260803L, output_dir = output_dir
  )
  sdb <- generate_sdb(
    spine = spine, seed = 20260803L, output_dir = output_dir,
    return_data = TRUE
  )
  mt <- generate_mt_demogs(
    spine = spine, seed = 20260803L, output_dir = output_dir,
    return_data = TRUE
  )

  expect_gt(nrow(sdb), 0L)
  expect_true(all(sdb$DATE_OF_GRANT >= sdb$ARRIVAL_DATE))
  expect_true(all(as.integer(sdb$DATE_OF_GRANT - sdb$ARRIVAL_DATE) <= 730L))
  expect_setequal(unique(mt$ADR_TYP), c("R", "P"))
  expect_gt(mean(mt$ADR_TYP == "R"), 0.80)
  expect_lt(mean(mt$ADR_TYP == "R"), 0.98)
})

test_that("every original Home Affairs and APSED gap has a source or rule", {
  fixture <- home_affairs_value_fixture()
  register <- utils::read.csv(
    fplida_test_inst_path(
      "internal-docs", "admin-value-remediation-register.csv"
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  datasets <- c("AMEP", "APSED", "MT_DEMOGS", "SDB", "VISA")
  register <- register[register$dataset %in% datasets, , drop = FALSE]

  covered <- vapply(seq_len(nrow(register)), function(i) {
    name <- register$variable[[i]]
    hit <- match(toupper(name), toupper(names(fixture$source)))
    source_value <- if (is.na(hit)) NULL else fixture$source[[hit]]
    source_covered <- !is.null(source_value) && any(!is.na(source_value))
    rule <- fplida:::.dil_home_affairs_source_value(
      name, register$official_descriptions[[i]], register$dataset[[i]],
      fixture$source, fixture$spine, 20260803L, fixture$period,
      register$example_product[[i]], register$example_table[[i]]
    )
    source_covered || !is.null(rule)
  }, logical(1))

  expect_true(
    all(covered),
    info = paste(register$dataset[!covered], register$variable[!covered],
                 collapse = ", ")
  )
  expect_equal(nrow(register), 127L)
  expect_equal(sum(register$occurrence_count), 262L)
})
