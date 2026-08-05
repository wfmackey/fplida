#!/usr/bin/env Rscript

target_guides <- c("home-affairs", "dss", "education", "vet-apprentice", "ndis")

read_csv <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

schema_path <- function(guide) {
  file.path("inst/internal-docs/schema-registers",
            paste0("schema-register-", guide, ".csv"))
}

source_urls <- list(
  `home-affairs` = paste(
    c(
      "https://www.abs.gov.au/statistics/data-integration/integrated-data/person-level-integrated-data-asset-plida/plida-data-and-legislation",
      "https://immi.homeaffairs.gov.au/settling-in-australia/settlement-reports",
      "https://immi.homeaffairs.gov.au/settling-in-australia/settlement-reports/settlement-reporting-caveats",
      "https://immi.homeaffairs.gov.au/settlement-services-subsite/files/settlement-database-data-dictionary.pdf",
      "https://immi.homeaffairs.gov.au/amep-subsite/Files/paper-a-profile-amep-clients.pdf"
    ),
    collapse = " | "
  ),
  dss = paste(
    c(
      "https://www.abs.gov.au/statistics/data-integration/integrated-data/person-level-integrated-data-asset-plida/plida-data-and-legislation",
      "https://www.dss.gov.au/doing-business-us/corporate-policies/metadata-research-datasets",
      "https://dss.aristotlecloud.io/item/1942/dataset/domino-external-analytical-version",
      "https://dex.dss.gov.au/policy",
      "https://dex.dss.gov.au/sites/default/files/documents/2023-03/1931-data-exchange-protocols.pdf",
      "https://dex.dss.gov.au/sites/default/files/documents/2025-09/2646-2536-chsp-dex-protocols.pdf"
    ),
    collapse = " | "
  ),
  education = paste(
    c(
      "https://www.abs.gov.au/statistics/data-integration/integrated-data/person-level-integrated-data-asset-plida/plida-data-and-legislation",
      "https://www.education.gov.au/higher-education-statistics/student-data",
      "https://www.tcsisupport.gov.au/element/330",
      "https://www.tcsisupport.gov.au/element/329/7.00",
      "https://www.education.gov.au/early-childhood/about/data-and-reports/australian-early-development-census",
      "https://www.aedc.gov.au/resources/detail/aedc-2025-data-dictionary"
    ),
    collapse = " | "
  ),
  `vet-apprentice` = paste(
    c(
      "https://www.abs.gov.au/statistics/data-integration/integrated-data/person-level-integrated-data-asset-plida/plida-data-and-legislation",
      "https://www.ncver.edu.au/research-and-statistics/collections/students-and-courses-collection/total-vet-students-and-courses",
      "https://www.ncver.edu.au/rto-hub/what-is-avetmiss",
      "https://www.ncver.edu.au/__data/assets/pdf_file/0022/62383/AVETMISS-Data-element-definitions-2_3-Nov-2022.pdf",
      "https://www.dewr.gov.au/australian-apprenticeships"
    ),
    collapse = " | "
  ),
  ndis = paste(
    c(
      "https://www.abs.gov.au/statistics/data-integration/integrated-data/person-level-integrated-data-asset-plida/plida-data-and-legislation",
      "https://dataresearch.ndis.gov.au/datasets",
      "https://dataresearch.ndis.gov.au/datasets/participant-datasets",
      "https://www.ndis.gov.au/participants/using-your-funding/plan-implementation-meeting/guide-your-management-options"
    ),
    collapse = " | "
  )
)

identifier_variables <- c("SYNTHETIC_AEUID", "TVA_ROW_ID")
date_like_variables <- c(
  "ARRIVAL_DATE", "DATE_OF_GRANT", "LAST_ON_DAY", "VA_LODGED_DT",
  "TR_VISA_GRANT_DT", "PERIOD_START_DATE", "PERIOD_END_DATE",
  "SESSION_DATE", "START_DATE", "END_DATE", "DATE_PROGRAM_COMPLETED",
  "FIRST_PLAN_APPROVAL_DATE", "ACTIVITY_START_DATE", "ACTIVITY_END_DATE",
  "UNIT_STUDY_CENSUS"
)

make_empty <- function(schema, crosswalk) {
  merged <- merge(
    schema,
    crosswalk[, c("guide", "dataset", "table", "variable",
                  "crosswalk_status", "candidate_variable",
                  "candidate_product", "candidate_description")],
    by = c("guide", "dataset", "table", "variable"),
    all.x = TRUE,
    sort = FALSE
  )
  merged[order(match(paste(schema$guide, schema$dataset, schema$table, schema$variable),
                     paste(merged$guide, merged$dataset, merged$table, merged$variable))), ]
}

set_row <- function(row, evidence_status, source_summary,
                    current_generated_assessment, required_next_action) {
  row$evidence_status <- evidence_status
  row$source_summary <- source_summary
  row$current_generated_assessment <- current_generated_assessment
  row$required_next_action <- required_next_action
  row
}

base_assessment <- function(row) {
  if (row$variable %in% identifier_variables) {
    return("Generated identifier shape is observed local output; it is not an official public code distribution.")
  }
  small_values <- row$observed_small_domain_values
  if (is.na(small_values)) {
    small_values <- ""
  }
  if (row$type %in% c("numeric", "integer") && small_values == "") {
    return("Observed quantiles/ranges are synthetic generated-output evidence, not an official distribution.")
  }
  if (row$type == "Date" || row$variable %in% date_like_variables) {
    return("Observed date range is synthetic generated-output evidence unless an official source is attached separately.")
  }
  "Observed values are the current fplida generated domain; source equivalence depends on the evidence status."
}

next_action <- function(row) {
  if (grepl("local_identifier", row$evidence_status)) {
    return("Keep this as a generated linkage identifier check; do not cite it as a public source variable.")
  }
  if (grepl("not_found|not_source_equivalent|needs_tightening|needs_format_tightening|unverified|not_exhausted|need_mapping|labels_need_mapping|cutoffs_local",
            row$evidence_status)) {
    return("Find a public variable/code-table source or keep this row labelled local/synthetic in future builds.")
  }
  if (grepl("distribution_local|generated_subset", row$evidence_status)) {
    return("Keep generated frequencies/distributions labelled synthetic unless calibrated against official aggregate or source-data evidence.")
  }
  "Keep this row under source-backed regression tests and retain the cited source."
}

classify_home_affairs <- function(row) {
  status <- "public_home_affairs_variable_codebook_not_found_local_metadata_only"
  summary <- "Local PLIDA metadata or generator output describes this generated field. No public Home Affairs variable-level code table proving the generated value domain was found in this pass."

  if (row$variable %in% identifier_variables) {
    status <- "local_identifier_not_public_source_variable"
    summary <- "This is a generated package linkage identifier, not a public Home Affairs source-code variable."
  } else if (row$dataset == "SDB") {
    sdb_concepts <- c("ARRIVAL_DATE", "COUNTRY_OF_BIRTH", "MARITAL_STATUS",
                      "ENG_FLUENCY_SPUK", "VISA_SUB_CLASS", "SEX",
                      "MONTH_OF_BIRTH", "YEAR_OF_BIRTH")
    if (row$variable %in% sdb_concepts) {
      status <- "official_sdb_dictionary_concept_generated_domain_local"
      summary <- "Home Affairs SDB pages and the SDB Data Dictionary support the settlement-record context and this broad data-item concept; generated frequencies and compact code values remain local until mapped item-by-item to the dictionary tables."
    } else {
      status <- "sdb_public_dictionary_exact_variable_or_code_not_found"
      summary <- "The SDB public sources support settlement reporting, but I did not find an exact public source proving this generated variable name and generated value domain."
    }
  } else if (row$dataset == "AMEP") {
    if (grepl("ASLPR|HOURS|ENROLMENT", row$variable)) {
      status <- "amep_public_profile_context_distribution_local"
      summary <- "The AMEP profile paper supports AMEP client, tuition-hours, enrolment-period, and English-proficiency context. It does not provide a variable-level ASLPR score codebook or source distribution for the generated compact fields."
    }
  } else if (row$dataset == "TRAVELLERS") {
    status <- "traveller_public_codebook_not_found_generated_compact_local"
    summary <- "Local PLIDA metadata contains wide ERP/PP status concepts, but no public Home Affairs codebook was found for the generated compact 1/2/3 quarterly status strings."
  } else if (row$dataset == "VISA") {
    status <- "visa_public_codebook_not_found_local_metadata_only"
    summary <- "Local PLIDA metadata describes the visa fields. No public Home Affairs codebook was found that proves generated subclass/reporting-group/status labels or their generated distribution."
  }

  set_row(row, status, summary, base_assessment(row), "")
}

classify_dss <- function(row) {
  status <- "dss_public_variable_codebook_not_found_local_metadata_only"
  summary <- "Local PLIDA metadata or generator output describes this generated DSS field; no public variable-specific code table proving the generated value domain was found in this pass."

  if (row$variable %in% identifier_variables) {
    status <- "local_identifier_not_public_source_variable"
    summary <- "This is a generated package linkage identifier, not an official public DSS code distribution."
  } else if (row$dataset == "DOMINO") {
    status <- "domino_aristotle_dataset_context_public_code_table_not_found"
    summary <- "DSS confirms DOMINO as longitudinal social-security administrative data and points users to Aristotle metadata; the public dataset entry supports dataset context, but this pass did not locate public variable-level pages proving generated DOMINO code values."
  } else if (row$dataset == "DEX") {
    status <- "dex_protocol_context_generated_domain_local"
    summary <- "DSS Data Exchange policy/protocols support DEX service, client, SCORE, extended-data, and exit-reason concepts. The exact generated labels remain local unless separately matched to a protocol field-value table."
    if (row$variable %in% c("SCORE_PRE", "SCORE_POST")) {
      status <- "official_dex_score_range_distribution_local"
      summary <- "DEX policy/protocols support SCORE outcomes; local PLIDA metadata describes outcome-domain score as 1-5. The generated pre/post distribution and monotonic rule are synthetic local behaviour."
    } else if (row$variable == "EXIT_REASON") {
      status <- "official_dex_exit_reason_concept_generated_labels_not_source_equivalent"
      summary <- "DEX protocols list official client-exit-reason categories, but the generated labels do not match the protocol wording exactly. Treat this as concept-only, not source-equivalent."
    } else if (row$variable %in% c("ASSISTANCE_TYPE", "GENDER_CODE", "ATSI_CODE",
                                  "EDUCATION_LEVEL", "EMPLOYMENT_STATUS",
                                  "COUNTRY_OF_BIRTH")) {
      status <- "dex_protocol_or_metadata_concept_generated_labels_unverified"
      summary <- "DEX protocols and local PLIDA metadata support the broad field concept. This pass did not prove that the generated compact labels/codes are the official DEX field values."
    }
  }

  set_row(row, status, summary, base_assessment(row), "")
}

classify_education <- function(row) {
  status <- "education_public_collection_context_distribution_local"
  summary <- "Department of Education public sources support the collection context and local PLIDA metadata provides the field description; generated values/distributions remain local unless exact data-element/code evidence is attached."

  if (row$variable %in% identifier_variables) {
    status <- "local_identifier_not_public_source_variable"
    summary <- "This is a generated package linkage identifier, not an official public Education code distribution."
  } else if (row$dataset == "HE") {
    status <- "he_collection_context_tcsi_mapping_not_exhausted"
    summary <- "The Department states HESSC/HESDC includes enrolment, EFTSL/load, course and completion data and points to TCSI for data elements. Exact TCSI element mapping was not attached for this generated field in this pass."
    if (row$variable == "ATTENDANCE_TYPE") {
      status <- "official_tcsi_attendance_type_generated_subset_distribution_local"
      summary <- "TCSI Element E330 defines type of attendance codes 1 full-time and 2 part-time; generated frequencies are synthetic."
    } else if (row$variable %in% c("ATTENDANCE_MODE", "MODE_ATTENDANCE")) {
      status <- "official_tcsi_mode_of_attendance_generated_subset_distribution_local"
      summary <- "TCSI Element E329 defines mode-of-attendance codes including 1 internal, 2 external, and 3 multi-modal; the current generated domain is a subset and generated frequencies are synthetic."
    } else if (row$variable %in% c("HELP_DEBT", "LOAN_FEE", "TOTAL_AMOUNT_CHARGED",
                                  "AMOUNT_PAID_UPFRONT", "EQUIVALENT_FT_STUDENT_LOAD",
                                  "COURSE_LOAD")) {
      status <- "he_official_concept_continuous_distribution_local"
      summary <- "The Department HE student-data page supports HELP/load/charge concepts in the collection. Observed generated amount/load distributions are synthetic and not official calibration evidence."
    }
  } else if (row$dataset == "AEDC") {
    status <- "official_aedc_dictionary_variable_generated_distribution_local"
    summary <- "The AEDC 2025 Data Dictionary is the official variable reference and covers variable descriptions, data collection, domain-score calculation and vulnerability measures. Generated scores/frequencies are local unless mapped to cycle-specific scoring tables."
    if (grepl("CATEGORY$|^DV[12]$", row$variable)) {
      status <- "official_aedc_vulnerability_concept_generated_cutoffs_local"
      summary <- "The AEDC dictionary covers domain-score calculation and vulnerability measures, but this register did not verify the generator's 4.5/6.0 cutoffs against official cycle-specific scoring rules."
    }
  }

  set_row(row, status, summary, base_assessment(row), "")
}

classify_vet <- function(row) {
  status <- "vet_public_codebook_not_found_local_metadata_only"
  summary <- "Local metadata or generator output describes this generated VET/apprentice field; no exact public code table was attached in this pass."

  if (row$variable %in% identifier_variables) {
    status <- "local_identifier_not_public_source_variable"
    summary <- "This is a generated package linkage identifier, not an official public VET code distribution."
  } else if (row$dataset == "TVA") {
    status <- "avetmiss_data_element_context_generated_distribution_local"
    summary <- "NCVER states Total VET is compiled under AVETMISS and the AVETMISS definitions document provides element definitions, formats and code lists. The generated value frequencies remain synthetic."
    exact_or_subset <- c(
      "AT_SCHOOL_FG", "PRIOR_ED_ACHIEVE_FG", "PROGRAM_VET_FG", "SUBJECT_VET_FG",
      "SUBJECT_FG", "APPRENTICESHIP_FG", "VET_IN_SCHOOLS_FG",
      "STUDENT_COMMENCING_FG", "PROGRAM_LOE_ID", "PROGRAM_ID",
      "ANZSCO_ID", "DELIVERY_MODE_ID", "NATIONAL_FUNDING_SOURCE_ID",
      "NATIONAL_OUTCOME_ID", "GENDER_ID", "STATE_OF_FUNDING_GF",
      "CLIENT_STATE_RESIDENCE_DERIVED", "HEAD_OFFICE_STATE",
      "PROGRAM_FOE_ID", "SUBJECT_FOE_ID", "PROGRAM_RECOGNITION_ID",
      "TRAIN_ORG_TYPE_ID"
    )
    if (row$variable %in% exact_or_subset) {
      status <- "official_avetmiss_element_generated_subset_distribution_local"
      summary <- "The AVETMISS data element definitions include the matching concept/code-list family for this field. The current generated values should be treated as a generated subset until each value is regression-tested against the official table."
    }
    if (grepl("STATE", row$variable)) {
      status <- "official_avetmiss_state_code_needs_format_tightening"
      summary <- "AVETMISS state identifier values are two-character codes such as 01-08, plus 09 and 99. Current generated state values are numeric-style 1-8, so the concept is sourced but formatting needs tightening."
    }
  } else if (row$dataset == "A&T") {
    status <- "apprentice_public_codebook_not_found_local_metadata_only"
    summary <- "DEWR sources support the Australian Apprenticeships program context and AVETMISS covers apprentice/trainee collections. This pass did not find a public A&T variable-level codebook proving the generated compact labels."
    if (row$variable %in% c("QUALIFICATION_LEVEL", "INDIGENOUS_STATUS", "SCHOOL_BASED")) {
      status <- "apprentice_generated_compact_not_source_equivalent"
      summary <- "The generated compact values are local implementation labels/codes. Public AVETMISS concepts exist, but this generated domain has not been shown to be source-equivalent."
    }
  }

  set_row(row, status, summary, base_assessment(row), "")
}

classify_ndis <- function(row) {
  status <- "ndis_public_dataset_context_distribution_local"
  summary <- "NDIS public dataset pages support participant, budget, utilisation, diagnosis, plan-management and demographic concepts. Generated values/distributions are local unless exact public data-rule mapping is attached."

  if (row$variable %in% identifier_variables) {
    status <- "local_identifier_not_public_source_variable"
    summary <- "This is a generated package linkage identifier, not an official public NDIS code distribution."
  } else if (row$variable == "MANAGEMENT_TYPE") {
    status <- "official_ndis_management_options_generated_labels_need_mapping"
    summary <- "NDIS guidance confirms self-management, plan management and NDIA-management options. Generated labels Plan/Agency/Self are concept-compatible but need exact DIL/public variable mapping before being treated as official labels."
  } else if (row$variable %in% c("PLAN_BUDGET_AMT", "SUPPORT_CATEGORY",
                                  "DISABILITY_GROUP", "PARTICIPANT_STATUS")) {
    status <- "ndis_public_dataset_concept_generated_domain_unverified"
    summary <- "NDIS participant dataset pages support this concept family, but this pass did not prove the generated categories or budget distribution against the public data rules or source variable codebook."
  } else if (row$variable %in% c("ABORIGINAL_TSI", "GENDER", "STATE_ASGS_2021",
                                  "YEAR_OF_BIRTH", "FIRST_PLAN_APPROVAL_DATE")) {
    status <- "ndis_metadata_concept_distribution_local"
    summary <- "Local PLIDA metadata or public NDIS dataset context supports the broad concept. Generated value labels/ranges remain local implementation evidence unless mapped to source data rules."
  }

  set_row(row, status, summary, base_assessment(row), "")
}

classifiers <- list(
  `home-affairs` = classify_home_affairs,
  dss = classify_dss,
  education = classify_education,
  `vet-apprentice` = classify_vet,
  ndis = classify_ndis
)

crosswalk <- read_csv("inst/internal-docs/generated-variable-crosswalk.csv")

for (guide in target_guides) {
  schema <- read_csv(schema_path(guide))
  merged <- make_empty(schema, crosswalk)
  out <- merged[, c("dataset", "table", "variable", "type",
                    "domain_or_distribution", "values_if_small_domain",
                    "candidate_variable", "candidate_description",
                    "crosswalk_status")]
  names(out) <- c("dataset", "table", "variable", "type",
                  "observed_generated_domain_or_distribution",
                  "observed_small_domain_values",
                  "plida_or_crosswalk_variable",
                  "plida_or_crosswalk_description",
                  "crosswalk_status")
  out$evidence_status <- ""
  out$source_url <- source_urls[[guide]]
  out$source_summary <- ""
  out$current_generated_assessment <- ""
  out$required_next_action <- ""
  out[is.na(out)] <- ""

  classified <- lapply(seq_len(nrow(out)), function(i) {
    row <- out[i, , drop = FALSE]
    row <- classifiers[[guide]](row)
    row$required_next_action <- next_action(row)
    row
  })
  out <- do.call(rbind, classified)

  output_path <- file.path("inst/internal-docs",
                           paste0(guide, "-variable-code-evidence.csv"))
  write.csv(out, output_path, row.names = FALSE, na = "")
}
