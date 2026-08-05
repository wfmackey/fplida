audit_script <- testthat::test_path("..", "..", "scripts",
                                   "audit_admin_schema_values.R")
if (!file.exists(audit_script)) {
  testthat::skip("Repository audit script is excluded from the R source package.")
}
source(audit_script, local = TRUE)

test_that("all schemas remain in scope while survey values are deferred", {
  metadata <- .audit_read_metadata(testthat::test_path("..", ".."))

  expect_setequal(.audit_survey_datasets,
                  c("LFS", "NHS", "NSMHW", "PEX", "SDAC"))
  expect_false(any(.audit_survey_datasets %in% metadata$admin_datasets))
  expect_true(all(c("CENSUS", "ACLD", "AEDC", "PIT_ITR", "MBS") %in%
                    metadata$admin_datasets))
  expect_equal(length(metadata$admin_datasets), 38L)
  expect_equal(length(metadata$schema_datasets), 43L)
  structures <- .audit_required_structures(metadata)
  expect_equal(sum(structures$asset == "PLIDA"), 2140L)
  expect_equal(sum(structures$asset == "BLADE"), 62L)
  expect_equal(length(unique(structures$official_product[
    structures$asset == "PLIDA"
  ])), 559L)
  expect_equal(nrow(metadata$blade_tables), 62L)
  expect_setequal(
    metadata$blade_tables[["Table.Number"]][
      metadata$blade_tables[["Source.Type"]] == "survey"
    ],
    c(8:23, 38:46)
  )
})

test_that("output mapping resolves exact products and table suffixes", {
  metadata <- .audit_read_metadata(testthat::test_path("..", ".."))

  census <- .audit_map_one_output("madipge-cen21-d-person-2021", metadata)
  expect_equal(census$asset, "PLIDA")
  expect_equal(census$dataset, "CENSUS")
  expect_equal(census$official_table, "census_2021_person")
  expect_equal(census$schema_role, "auxiliary_exact_product")

  apsed <- .audit_map_one_output("pmp-apsed--apsed_agency", metadata)
  expect_equal(apsed$dataset, "APSED")
  expect_equal(apsed$official_product, "pmp-apsed")
  expect_equal(apsed$official_table, "apsed_agency")
  expect_equal(apsed$schema_role, "canonical_structure")

  blade <- .audit_map_one_output(
    "blade-table-01-cross-sectional-indicative-data-items", metadata
  )
  expect_equal(blade$asset, "BLADE")
  expect_equal(blade$table_number, "1")
  expect_equal(blade$schema_role, "canonical_structure")

  blade_survey <- .audit_map_one_output(
    "blade-table-08-business-characteristics-survey-bcs", metadata
  )
  expect_equal(blade_survey$source_type, "survey")
  expect_equal(blade_survey$value_scope, "deferred_survey")
})

test_that("multi-table products require one canonical output per table", {
  metadata <- .audit_read_metadata(testthat::test_path("..", ".."))
  apsed_rows <- metadata$variables[
    metadata$variables$Dataset == "APSED" &
      metadata$variables[["Product Name"]] == "pmp-apsed",
    , drop = FALSE
  ]

  exact_product <- .audit_map_one_output(
    "pmp-apsed", metadata,
    generated_variables = unique(apsed_rows[["Variable Name"]])
  )
  expect_equal(exact_product$mapping_status, "exact_product")
  expect_equal(exact_product$schema_role, "auxiliary_product")
  expect_true(is.na(exact_product$official_table))

  agency <- .audit_map_one_output(
    "pmp-apsed--apsed_agency", metadata,
    generated_variables = unique(apsed_rows[["Variable Name"]][
      apsed_rows[["Table Name"]] == "apsed_agency"
    ])
  )
  disability <- .audit_map_one_output(
    "pmp-apsed--apsed_disability", metadata,
    generated_variables = unique(apsed_rows[["Variable Name"]][
      apsed_rows[["Table Name"]] == "apsed_disability"
    ])
  )
  expect_equal(agency$official_table, "apsed_agency")
  expect_equal(disability$official_table, "apsed_disability")
  expect_equal(agency$schema_role, "canonical_structure")
  expect_equal(disability$schema_role, "canonical_structure")
})

test_that("table suffix matching preserves distinct official table names", {
  metadata <- .audit_read_metadata(testthat::test_path("..", ".."))

  hyphen_table <- .audit_map_one_output(
    "pmp-stp-extended--stp_extended_etp_2019-20", metadata
  )
  underscore_table <- .audit_map_one_output(
    "pmp-stp-extended--stp_extended_etp_2019_20", metadata
  )
  ambiguous_alias <- .audit_map_one_output(
    "pmp-stp-extended--stp-extended-etp-2019-20", metadata
  )

  expect_equal(hyphen_table$official_table, "stp_extended_etp_2019-20")
  expect_equal(underscore_table$official_table, "stp_extended_etp_2019_20")
  expect_equal(hyphen_table$schema_role, "canonical_structure")
  expect_equal(underscore_table$schema_role, "canonical_structure")
  expect_true(is.na(ambiguous_alias$official_table))
  expect_equal(ambiguous_alias$mapping_status, "unmatched_table_suffix")
})

test_that("normalised table aliases remain auxiliary outputs", {
  metadata <- .audit_read_metadata(testthat::test_path("..", ".."))

  alias <- .audit_map_one_output(
    "madipge-dom-monthly-d-base--static-demogs", metadata
  )
  case_alias <- .audit_map_one_output(
    "pmp-apsed--APSED_AGENCY", metadata
  )

  expect_equal(alias$official_table, "static_demogs")
  expect_equal(alias$mapping_status, "normalised_table_suffix")
  expect_equal(alias$schema_role, "auxiliary_table_alias")
  expect_equal(case_alias$official_table, "apsed_agency")
  expect_equal(case_alias$mapping_status, "normalised_table_suffix")
  expect_equal(case_alias$schema_role, "auxiliary_table_alias")
})

test_that("value scan distinguishes placeholders from generic fallbacks", {
  explicit <- .audit_value_findings(
    x = c("apsed_000001", "ordinary"), column = "VALUE",
    dataset = "APSED", source_file = "x.csv", logical_output = "x"
  )
  expect_true(any(explicit$finding_type == "dataset_counter_placeholder" &
                    explicit$severity == "error"))

  generic <- .audit_value_findings(
    x = rep(2024L, 3L), column = "REFERENCE_YEAR",
    dataset = "APSED", source_file = "x.csv", logical_output = "x"
  )
  expect_true(any(generic$finding_type == "generic_fixed_year" &
                    generic$severity == "warning"))
  expect_false(any(generic$severity == "error"))

  undocumented_c <- .audit_value_findings(
    x = c("C01", "C02"), column = "CATEGORY",
    dataset = "APSED", source_file = "x.csv", logical_output = "x"
  )
  expect_true(any(undocumented_c$finding_type == "generic_c_code" &
                    undocumented_c$severity == "error"))

  documented_c <- .audit_value_findings(
    x = c("C01", "C02"), column = "CATEGORY",
    dataset = "BLADE", source_file = "x.csv", logical_output = "x",
    valid_response = "C01 = First category C02 = Second category"
  )
  expect_true(any(documented_c$finding_type == "documented_c_code" &
                    documented_c$severity == "warning"))
  expect_false(any(documented_c$finding_type == "generic_c_code"))
})

test_that("DEATHS cause-of-death fields accept valid ICD-10 C codes", {
  for (column in c("UCOD", "ENTITY1", "ENTITY20", "RACS1", "RACS20")) {
    findings <- .audit_value_findings(
      x = c("C34", "C50", "C61"), column = column,
      dataset = "DEATHS", source_file = "deaths.csv",
      logical_output = "deaths"
    )
    expect_false(any(findings$finding_type == "generic_c_code"),
                 info = column)
  }

  wrong_column <- .audit_value_findings(
    x = "C34", column = "CATEGORY", dataset = "DEATHS",
    source_file = "deaths.csv", logical_output = "deaths"
  )
  wrong_dataset <- .audit_value_findings(
    x = "C34", column = "UCOD", dataset = "APSED",
    source_file = "apsed.csv", logical_output = "apsed"
  )
  expect_true(any(wrong_column$finding_type == "generic_c_code"))
  expect_true(any(wrong_dataset$finding_type == "generic_c_code"))
})

test_that("only complete categorical metadata is an exhaustive domain", {
  expect_setequal(
    .audit_parse_valid_codes(
      "0 = No 1 = Yes 7777777 = Multiple 999999999 = Missing"
    ),
    c("0", "1", "7777777", "999999999")
  )
  expect_length(
    .audit_parse_valid_codes(
      "4 digit numeric; 0 = Unknown; 9999 = Missing"
    ),
    0L
  )
  expect_length(
    .audit_parse_valid_codes(
      "Postcode; 0 = Unknown; 9999 = Missing"
    ),
    0L
  )
  expect_setequal(
    .audit_parse_valid_codes(
      "Categorical '1' = Yes '0' = No 'N/A'"
    ),
    c("1", "0", "N/A")
  )
  expect_setequal(
    .audit_parse_valid_codes(
      "Categorical '1' = Yes '0' = No 'UNKNOWN'"
    ),
    c("1", "0", "UNKNOWN")
  )
})

test_that("metadata aliases map to the matching period", {
  metadata <- .audit_read_metadata(testthat::test_path("..", ".."))
  official_variables <- function(dataset, product) {
    unique(metadata$variables[["Variable Name"]][
      metadata$variables[["Dataset"]] == dataset &
        metadata$variables[["Product Name"]] == product
    ])
  }

  cgt <- .audit_map_one_output(
    "pmp-cgt-0102", metadata,
    relative_file = "ato-cgt/pmp-cgt-0102/part-000.parquet",
    dataset_folder = "ato-cgt",
    generated_variables = official_variables("CGT", "pmp-cgt-2001-02")
  )
  expect_equal(cgt$official_product, "pmp-cgt-2001-02")

  sae <- .audit_map_one_output(
    "madipmp-ato-maasmats-1819", metadata,
    relative_file = paste0(
      "ato-sae/madipmp-ato-maasmats-1819/part-000.parquet"
    ),
    dataset_folder = "ato-sae",
    generated_variables = official_variables(
      "SAE", "madipmp-ato-maasmats-2018-19"
    )
  )
  expect_equal(sae$official_product, "madipmp-ato-maasmats-2018-19")

  itr_product <- "madip-ge-020102d-ato-atoitrcontext1011-fy2010-11"
  itr <- .audit_map_one_output(
    "madipge-ato-d-context-fy1011", metadata,
    relative_file = paste0(
      "ato-pit_itr/madipge-ato-d-context-fy1011/part-000.parquet"
    ),
    dataset_folder = "ato-pit_itr",
    generated_variables = official_variables("PIT_ITR", itr_product)
  )
  expect_equal(itr$official_product, itr_product)

  core <- .audit_map_one_output(
    "core_residence", metadata,
    relative_file = "abs-core/core_residence/part-202001.parquet",
    dataset_folder = "abs-core",
    generated_variables = c("synthetic_aeuid", "month", "pp_weight")
  )
  expect_equal(
    core$official_product,
    "plidage-core-scope-resi-pp-2006-latest"
  )
  expect_equal(core$official_table, "pp_weight_2020_v8")
})

test_that("only canonical outputs participate in official column checks", {
  metadata <- .audit_read_metadata(testthat::test_path("..", ".."))
  file_mapping <- function(relative_file, dataset_folder, logical_output,
                           mapping) {
    cbind(
      data.frame(
        source_file = relative_file,
        relative_file = relative_file,
        dataset_folder = dataset_folder,
        partition = NA_character_,
        logical_output = logical_output,
        stringsAsFactors = FALSE
      ),
      mapping,
      stringsAsFactors = FALSE
    )
  }

  apsed_output <- "pmp-apsed--apsed_agency"
  apsed_file <- file.path("apsc-apsed", paste0(apsed_output, ".csv"))
  apsed <- file_mapping(
    apsed_file, "apsc-apsed", apsed_output,
    .audit_map_one_output(apsed_output, metadata,
                          generated_variables = "AGENCYID")
  )
  core_output <- "core_residence"
  core_file <- file.path("abs-core", paste0(core_output, ".csv"))
  core <- file_mapping(
    core_file, "abs-core", core_output,
    .audit_map_one_output(
      core_output, metadata, relative_file = core_file,
      dataset_folder = "abs-core",
      generated_variables = c("synthetic_aeuid", "month", "pp_weight")
    )
  )
  output_map <- rbind(apsed, core)
  variable_summary <- data.frame(
    source_file = c(apsed_file, core_file),
    variable = c("AGENCYID", "synthetic_aeuid"),
    stringsAsFactors = FALSE
  )

  expect_equal(core$mapping_status, "metadata_schema_match")
  expect_equal(core$schema_role, "auxiliary_metadata_match")
  expect_equal(core$value_scope, "audited")

  column_coverage <- .audit_column_coverage(
    output_map, variable_summary, metadata
  )
  expect_equal(nrow(column_coverage), 3L)
  expect_equal(sum(column_coverage$coverage_status == "official_only"), 2L)
  expect_setequal(column_coverage$official_variable,
                  c("AGENCYID", "AGENCY_FUNCTION", "AGENCY_SIZE"))
  expect_true(all(column_coverage$logical_output == apsed_output))
  expect_false(any(grepl("CORE", column_coverage$structure_key,
                         fixed = TRUE)))

  structure_coverage <- .audit_product_coverage(output_map, metadata)
  expect_equal(sum(structure_coverage$asset == "PLIDA" &
                     structure_coverage$coverage_status == "observed"), 1L)
  expect_equal(structure_coverage$coverage_status[
    structure_coverage$asset == "PLIDA" &
      structure_coverage$dataset == "APSED" &
      structure_coverage$official_table == "apsed_agency"
  ], "observed")
})

test_that("partitioned parquet files share their parent product name", {
  skip_if_not_installed("arrow")
  run_dir <- tempfile("fplida_partitioned_audit_")
  product_dir <- file.path(run_dir, "abs-census",
                           "madipge-cen21-d-person-2021")
  dir.create(product_dir, recursive = TRUE)
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)
  arrow::write_parquet(data.frame(AGEP = 20L),
                       file.path(product_dir, "part-001.parquet"))
  arrow::write_parquet(data.frame(AGEP = 30L),
                       file.path(product_dir, "part-002.parquet"))
  arrow::write_parquet(data.frame(AGEP = 40L),
                       file.path(product_dir, "part-000-001.parquet"))

  index <- .audit_file_index(run_dir)
  expect_equal(nrow(index), 3L)
  expect_equal(unique(index$logical_output),
               "madipge-cen21-d-person-2021")
})

test_that("gate exceptions require an exact key and documented reason", {
  allow_path <- tempfile(fileext = ".csv")
  on.exit(unlink(allow_path), add = TRUE)
  utils::write.csv(
    data.frame(
      issue_type = "unmapped_output", key = "known-local-output",
      reason = "Temporary local compatibility output; remove before release."
    ),
    allow_path, row.names = FALSE
  )
  allowlist <- .audit_read_allowlist(allow_path)
  failures <- .audit_gate_failures(
    product_coverage = data.frame(coverage_status = character(),
                                  official_product = character(),
                                  asset = character(), dataset = character(),
                                  official_table = character()),
    column_coverage = data.frame(coverage_status = character(),
                                 logical_output = character(),
                                 official_variable = character(),
                                 dataset = character(),
                                 official_product = character()),
    output_map = data.frame(mapping_status = "unmapped_output",
                            logical_output = "known-local-output"),
    value_findings = data.frame(severity = character(),
                                logical_output = character(),
                                variable = character(),
                                finding_type = character(),
                                example = character(), detail = character()),
    variable_summary = data.frame(),
    allowlist = allowlist
  )
  expect_true(failures$allowed)
  expect_match(failures$allowlist_reason, "Temporary local")

  utils::write.csv(
    data.frame(issue_type = "unmapped_output", key = "x", reason = ""),
    allow_path, row.names = FALSE
  )
  expect_error(.audit_read_allowlist(allow_path), "non-empty reason")
})

test_that("all-missing gate is limited to canonical audited variables", {
  empty_product_coverage <- data.frame(
    coverage_status = character(), official_product = character(),
    asset = character(), dataset = character(), official_table = character()
  )
  empty_column_coverage <- data.frame(
    coverage_status = character(), logical_output = character(),
    official_variable = character(), dataset = character(),
    official_product = character()
  )
  empty_output_map <- data.frame(
    mapping_status = character(), logical_output = character()
  )
  empty_findings <- data.frame(
    severity = character(), logical_output = character(),
    variable = character(), finding_type = character(),
    example = character(), detail = character()
  )
  variable_summary <- data.frame(
    logical_output = c("admin-all", "survey", "auxiliary", "empty",
                       "admin-part", "admin-part"),
    asset = "PLIDA", dataset = "APSED",
    official_product = "pmp-apsed",
    official_table = c("apsed-agency", "apsed-disability", "apsed-language",
                       "apsed-location", "apsed-agency", "apsed-agency"),
    value_scope = c("audited", "deferred_survey", "audited", "audited",
                    "audited", "audited"),
    schema_role = c("canonical_structure", "canonical_structure",
                    "auxiliary_product", "canonical_structure",
                    "canonical_structure", "canonical_structure"),
    variable = c("AGENCYID", "DISABILITY", "LANGUAGE", "STATE",
                 "AGENCY_SIZE", "AGENCY_SIZE"),
    n_rows = c(2L, 2L, 2L, 0L, 2L, 2L),
    missing = c(2L, 2L, 2L, 0L, 2L, 0L),
    stringsAsFactors = FALSE
  )

  failures <- .audit_gate_failures(
    product_coverage = empty_product_coverage,
    column_coverage = empty_column_coverage,
    output_map = empty_output_map,
    value_findings = empty_findings,
    variable_summary = variable_summary
  )

  all_missing <- failures[
    failures$issue_type == "all_missing_admin_variable", , drop = FALSE
  ]
  expect_equal(nrow(all_missing), 1L)
  expect_match(all_missing$key, "admin-all::AGENCYID", fixed = TRUE)
  expect_match(all_missing$detail, "2/2 values missing", fixed = TRUE)
})

test_that("unresolved admin report contains only all-missing audited canonical rows", {
  variable_summary <- data.frame(
    asset = c("PLIDA", "BLADE", "PLIDA", "PLIDA", "PLIDA", "PLIDA"),
    dataset = c("APSED", "BLADE", "APSED", "NHS", "APSED", "APSED"),
    official_product = paste0("product-", 1:6),
    official_table = paste0("table-", 1:6),
    logical_output = paste0("output-", 1:6),
    variable = c("B", "A", "C", "D", "E", "F"),
    type = rep("character", 6L),
    n_rows = c(2L, 3L, 2L, 2L, 0L, 2L),
    missing = c(2L, 3L, 1L, 2L, 0L, 2L),
    official_description = c(
      "Description 1\ncontinued", paste0("Description ", 2:6)
    ),
    official_valid_response = c(
      "Response 1\r\ncontinued", paste0("Response ", 2:6)
    ),
    value_scope = c("audited", "audited", "audited", "deferred_survey",
                    "audited", "audited"),
    schema_role = c(rep("canonical_structure", 5L), "auxiliary_product"),
    stringsAsFactors = FALSE
  )

  unresolved <- .audit_unresolved_admin_variables(variable_summary)

  expect_named(unresolved, c(
    "asset", "dataset", "official_product", "official_table",
    "logical_output", "variable", "type", "n_rows",
    "official_description", "official_valid_response"
  ))
  expect_equal(nrow(unresolved), 2L)
  expect_setequal(unresolved$variable, c("A", "B"))
  expect_true(all(unresolved$n_rows > 0L))
  has_line_break <- function(x) {
    grepl("\n", x, fixed = TRUE) | grepl("\r", x, fixed = TRUE)
  }
  expect_false(any(has_line_break(unresolved$official_description)))
  expect_false(any(has_line_break(unresolved$official_valid_response)))

  empty <- .audit_unresolved_admin_variables(variable_summary[0, ])
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(unresolved))
})

test_that("audit writes deterministic machine-readable report surfaces", {
  run_dir <- tempfile("fplida_admin_audit_run_")
  report_dir <- tempfile("fplida_admin_audit_report_")
  dir.create(file.path(run_dir, "apsc-apsed"), recursive = TRUE)
  dir.create(file.path(run_dir, "abs-blade"), recursive = TRUE)
  dir.create(file.path(run_dir, "abs-nhs"), recursive = TRUE)
  on.exit(unlink(c(run_dir, report_dir), recursive = TRUE), add = TRUE)

  utils::write.csv(
    data.frame(
      AGENCYID = c("apsed_000001", "apsed_000002"),
      AGENCY_FUNCTION = c("C01", "C02"),
      check.names = FALSE
    ),
    file.path(run_dir, "apsc-apsed", "pmp-apsed--apsed_agency.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(SURVEY_PLACEHOLDER = "nhs_000001"),
    file.path(run_dir, "abs-nhs", "madipge-nhs-d-survey-2022.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(bn = c("BN00000000001", "BN00000000002"),
               x_al_st = c(1L, 7L), check.names = FALSE),
    file.path(run_dir, "abs-blade",
              "blade-table-01-cross-sectional-indicative-data-items.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(id = "blade_000001", bcs_version = "C01",
               check.names = FALSE),
    file.path(
      run_dir, "abs-blade",
      "blade-table-08-business-characteristics-survey-bcs.csv"
    ),
    row.names = FALSE
  )

  result <- run_admin_schema_value_audit(
    run_dir = run_dir, report_dir = report_dir,
    repo_root = testthat::test_path("..", ".."), seed = 123L,
    fail_on_error = FALSE
  )

  expect_equal(result$summary$audit_seed, 123L)
  expect_equal(result$summary$physical_files_scanned, 4L)
  expect_equal(result$summary$survey_value_files_deferred, 2L)
  expect_equal(result$summary$blade_survey_tables_deferred, 25L)
  expect_equal(result$summary$plida_products_expected, 559L)
  expect_equal(result$summary$plida_structures_expected, 2140L)
  expect_equal(result$summary$plida_structures_observed, 1L)
  expect_equal(result$summary$plida_structures_missing, 2139L)
  expect_equal(result$summary$blade_tables_expected, 62L)
  expect_equal(result$summary$blade_tables_observed, 2L)
  expect_equal(result$summary$canonical_structure_outputs_scanned, 3L)
  expect_equal(result$summary$auxiliary_value_outputs_scanned, 1L)
  expect_gt(result$summary$explicit_placeholder_errors, 0L)
  expect_gt(result$summary$generic_c_code_errors, 0L)
  expect_gt(result$summary$official_domain_errors, 0L)
  expect_equal(result$summary$all_missing_admin_variables, 0L)
  expect_true(any(result$column_coverage$coverage_status == "official_only"))
  expect_true(any(result$value_findings$finding_type == "generic_c_code"))
  expect_true(any(result$value_findings$finding_type ==
                    "official_domain_mismatch"))
  deferred_outputs <- c(
    "madipge-nhs-d-survey-2022",
    "blade-table-08-business-characteristics-survey-bcs"
  )
  expect_false(any(result$value_findings$logical_output %in% deferred_outputs))
  expect_true(all(
    result$output_map$value_scope[
      result$output_map$logical_output %in% deferred_outputs
    ] == "deferred_survey"
  ))
  expect_gt(result$summary$gate_failures_unallowed, 0L)
  expect_error(
    run_admin_schema_value_audit(
      run_dir = run_dir, report_dir = tempfile("fplida_gate_failure_"),
      repo_root = testthat::test_path("..", ".."), seed = 123L
    ),
    "audit gate failed"
  )

  expected_files <- c(
    "summary.csv", "output-map.csv", "product-coverage.csv",
    "structure-coverage.csv",
    "column-coverage.csv", "variable-summary.csv", "value-findings.csv",
    "gate-failures.csv", "unresolved-admin-variables.csv"
  )
  expect_true(all(file.exists(file.path(report_dir, expected_files))))
  expect_identical(
    names(result$unresolved_admin_variables),
    c("asset", "dataset", "official_product", "official_table",
      "logical_output", "variable", "type", "n_rows",
      "official_description", "official_valid_response")
  )
})
