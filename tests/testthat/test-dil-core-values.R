test_that("CORE public classifications use documented value domains", {
  spine <- generate_spine(n = 240L, seed = 20260803L)
  source <- data.frame(.row = seq_len(nrow(spine)))
  period <- fplida:::.dil_table_period(
    "CORE", "plidage-core-relat-cb-c21-2006-latest",
    "relationship_map_v2025_2021c", "Core Relationships"
  )
  value <- function(name, table = "relationship_map_v2025_2021c") {
    fplida:::.dil_core_source_value(
      name, name, source, spine, 20260803L, period,
      "plidage-core-relat-cb-c21-2006-latest", table,
      "Core Relationships"
    )
  }

  expect_true(all(value("CTPP", "par_chi_census_2021_v8") %in% c(1L, 2L)))
  expect_true(all(value("MDCP", "partner_census_2021_v8") %in% 1:2))
  expect_true(all(value("MSTP", "partner_census_2021_v8") %in% c(1L, 5L)))
  expect_true(all(value("RLCP", "partner_census_2021_v8") %in% 1:4))
  expect_true(all(value("RLHP") %in% c(12L, 15L, 31L, 41L, 51L)))
  expect_true(all(value("RLGP") %in% c(12L, 15L, 31L, 41L, 51L)))
  expect_true(all(value("SPIP", "partner_census_2021_v8") == 2L))
  expect_true(all(value("FMCF") %in% c(1L, 2L, 3L, 9L)))
  expect_true(all(value("HCFMF") %in% 1:3))
  expect_true(all(value("FRLF") %in% 1:7))
})

test_that("CORE relationship fields are internally coherent", {
  spine <- generate_spine(n = 180L, seed = 11L)
  source <- data.frame(.row = seq_len(nrow(spine)))
  period <- fplida:::.dil_period("2021", dataset = "CORE")
  value <- function(name, table) {
    fplida:::.dil_core_source_value(
      name, name, source, spine, 44L, period, "", table, ""
    )
  }

  partner_category <- value("COMBINED_CATEGORY", "partner_ato_v8")
  partner_status <- value("COMBINED_STATUS", "partner_ato_v8")
  expect_true(all(partner_category == "Partner"))
  expect_true(all(partner_status %in% c("Married", "De facto")))
  expect_true(all(value("REL_CODE", "partner_ato_v8") %in%
                    c("PT_MARRIED", "PT_DEFACTO")))

  child_category <- value("COMBINED_CATEGORY", "par_chi_births_v8")
  child_status <- value("COMBINED_STATUS", "par_chi_births_v8")
  expect_true(all(child_category == "Parent-Child"))
  expect_true(all(child_status %in% c("Biological", "Step")))
  expect_true(all(value("REL_CODE", "par_chi_births_v8") %in%
                    c("PC_BIO", "PC_STEP")))

  original <- value("SPINE_ID_ORIGINAL", "relationship_map_v2025_2021c")
  related <- value("SPINE_ID_MAIN_REL", "relationship_map_v2025_2021c")
  expect_false(anyNA(original))
  expect_false(anyNA(related))
  expect_false(any(original == related))
  expect_false(anyNA(value("PAIRID", "relationship_map_v2025_2021c")))
})

test_that("CORE residence fields share one lifecycle projection", {
  spine <- generate_spine(n = 300L, seed = 29L)
  source <- data.frame(.row = seq_len(nrow(spine)))
  period <- fplida:::.dil_table_period(
    "CORE", "plidage-core-scope-resi-pp-2006-latest",
    "pp_weight_2018_v8", "Core Residence"
  )
  value <- function(name) {
    fplida:::.dil_core_source_value(
      name, name, source, spine, 29L, period,
      "plidage-core-scope-resi-pp-2006-latest",
      "pp_weight_2018_v8", "Core Residence"
    )
  }

  weight <- value("PP_WEIGHT")
  status <- value("PP_STATUS")
  expect_true(all(weight >= 0 & weight <= 1))
  expect_identical(status, ifelse(weight > 0, 1L, 3L))
  expect_match(value("PP_PERIOD"), "^2018-[0-9]{2}-[0-9]{2}$")
  expect_true(all(value("ERP_STATUS") %in% 1:3))
  expect_true(all(value("ERP_PERIOD") == "2018"))
})

test_that("CORE address and demographic fields preserve structural values", {
  spine <- generate_spine(n = 400L, seed = 73L)
  source <- data.frame(.row = seq_len(nrow(spine)))
  period <- fplida:::.dil_period("2021", dataset = "CORE")
  value <- function(name) {
    fplida:::.dil_core_source_value(
      name, name, source, spine, 73L, period, "", "ar_addr_v8", ""
    )
  }

  expect_true(all(value("ADDRESS_USE") %in%
                    c("RESIDENTIAL", "NON_RESIDENTIAL", "BOTH")))
  expect_true(all(value("ADR_TYP") %in% c("R", "P")))
  private <- value("PRIVATE_DWELLING_STRUCT")
  special <- value("SD_DWELLING_TYPE_ID")
  expect_true(all(private[!is.na(private)] %in% c(1L, 2L, 3L, 9L)))
  expect_true(all(special[!is.na(special)] %in% 1:8))
  expect_false(any(!is.na(private) & !is.na(special)))
  expect_true(any(!is.na(special)))

  expect_true(all(grepl("^[0-9]{4}$", value("BPLP"))))
  expect_identical(value("DOB_M"), as.integer(spine$month_of_birth))
  expect_identical(value("DOB_Y"), as.integer(spine$birth_year))
  expect_false(anyNA(value("BIRTH_ID")))
})

test_that("CORE activity periods use explicit local representations", {
  spine <- generate_spine(n = 120L, seed = 97L)
  source <- data.frame(.row = seq_len(nrow(spine)))
  period <- fplida:::.dil_period("2006 latest", dataset = "CORE")
  value <- function(name) {
    fplida:::.dil_core_source_value(
      name, name, source, spine, 97L, period, "", "itr_activity_v8", ""
    )
  }

  expect_match(value("ACTIVITY_FY"), "^[0-9]{2}$")
  expect_match(value("T_PERIOD"), "^[0-9]{2}$")
  expect_match(value("C_PERIOD"), "^20[0-9]{2}-[0-9]{2}$")
  expect_match(value("D_PERIOD"), "^20[0-9]{2}$")
  expect_match(value("M_PERIOD"), "^20[0-9]{2}$")
})
