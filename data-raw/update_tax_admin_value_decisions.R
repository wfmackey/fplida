#!/usr/bin/env Rscript

# Build the exact decision ledger for the original ATO administrative-value
# gaps. This script audits the live value dispatcher. It does not change any
# tax generator or shared dispatch rule.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The pkgload package is required to audit the live tax rules.",
       call. = FALSE)
}
pkgload::load_all(".", compile = FALSE, quiet = TRUE)

tax_datasets <- c(
  "CGT", "SMSF", "ATO_MCS", "ATO_CR",
  "PIT_ITR", "PIT_PS", "PIT_IE", "BUSOWN"
)

read_csv <- function(path) {
  utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = character()
  )
}

gap <- read_csv("inst/internal-docs/admin-value-gap-register.csv")
gap <- gap[gap$dataset %in% tax_datasets, , drop = FALSE]
register <- read_csv(
  "inst/internal-docs/admin-value-remediation-register.csv"
)
register <- register[register$dataset %in% tax_datasets, , drop = FALSE]

pair_key <- function(dataset, variable) paste(dataset, variable, sep = "\r")
gap_key <- pair_key(gap$dataset, gap$variable)
register_key <- pair_key(register$dataset, register$variable)

stopifnot(
  nrow(gap) == 2222L,
  nrow(register) == 184L,
  anyDuplicated(register_key) == 0L,
  setequal(unique(gap_key), register_key),
  identical(
    as.integer(table(factor(gap$dataset, levels = tax_datasets))),
    c(1062L, 144L, 87L, 33L, 825L, 51L, 5L, 15L)
  )
)

# The fixture exposes every current conditional rule to a non-zero source
# amount while retaining inapplicable rows. A large deterministic sample makes
# rare but supported branches observable without treating sampled frequencies
# as source evidence.
make_tax_fixture <- function(n = 2000L) {
  spine <- data.frame(
    id = seq_len(n),
    spine_id = sprintf("S%010d", seq_len(n)),
    aeuid_ato = sprintf("T%010d", seq_len(n)),
    birth_year = rep(1940:2019, length.out = n),
    month_of_birth = rep(1:12, length.out = n),
    sex = rep(1:2, length.out = n),
    state = rep(1:8, length.out = n),
    indigenous = rep(c(1L, 4L), length.out = n),
    country_of_birth_sacc = rep(c(1101L, 2102L), length.out = n),
    year_of_arrival = rep(c(NA, 2000L), length.out = n),
    year_of_death = rep(c(NA, NA, NA, 2020L), length.out = n),
    month_of_death = rep(c(NA, NA, NA, 6L), length.out = n),
    day_of_death = rep(c(NA, NA, NA, 15L), length.out = n),
    residence_seed = seq_len(n) + 100L,
    sa2_code = rep(101021007L, n),
    sa3_code = rep(10102L, n),
    sa4_code = rep(101L, n),
    anzsco_code = rep(
      c(111111L, 254411L, 351311L, 621111L), length.out = n
    ),
    industry = rep(1:19, length.out = n),
    baseline_employed = rep(c(TRUE, TRUE, FALSE), length.out = n),
    baseline_hours = rep(38, n),
    baseline_income = seq(20000, 100000, length.out = n),
    household_id = seq_len(n),
    stringsAsFactors = FALSE
  )
  alternating <- rep(c(0, 1000), length.out = n)
  source <- data.frame(
    MBR_IDV_DCSD_IND = rep(
      c(FALSE, FALSE, FALSE, TRUE), length.out = n
    ),
    CG_CY_TOTL_AMT = seq(10000, 100000, length.out = n),
    CG_TOTL_REAL_EST_AMT = seq(5000, 50000, length.out = n),
    CG_TOTL_SHARES_UNITS_AMT = seq(3000, 30000, length.out = n),
    CG_TOTL_OTH_AMT = seq(2000, 20000, length.out = n),
    CL_CY_TOTL_AMT = seq(0, 10000, length.out = n),
    CGT_TOTL_DSCNT_APLD_AMT = seq(1000, 10000, length.out = n),
    SB_CNCSNS_APLD_TOTL_AMT = alternating,
    CRNT_PNSN_EXMT_INCM_AMT = alternating,
    GRS_TRST_DSTRBTNS_AMT = alternating,
    INCM_OTHR_AMT = alternating,
    OD_AMT = alternating,
    NON_DDCTBL_EXPNS_OTHR_AMT = alternating,
    LSP_AMT = alternating,
    INCM_STRM_PMT_AMT = alternating,
    WRK_RLTD_CAR_EXPNSS_AMT = alternating,
    WRK_RLTD_CLTHG_EXPNSS_AMT = alternating,
    WRK_RLTD_SELF_EDUCN_EXPNSS_AMT = alternating,
    LUMP_SUM_A = alternating,
    CLOSG_STK_TOTL_AMT = alternating,
    DPRCTN_EXPNS_AMT = alternating,
    MTR_VHCL_BUS_EXPNSS_TOTL_AMT = alternating,
    SLRY_AND_WG_EXPNSS_TOTL_AMT = alternating,
    ASSBL_GOVT_INDY_PMTS_NPP_AMT = alternating,
    ASSBL_GOVT_INDY_PMTS_PP_AMT = alternating,
    BUS_ACTY_CNT = rep(c(0, 1, 0, 1), length.out = n),
    BBI_ELGBL_AST_OPT_OUT_NUM = rep(c(NA, 0, NA, 2), length.out = n),
    TFE_ELGBL_AST_OPT_OUT_NUM = rep(c(NA, 0, NA, 2), length.out = n),
    BUS_LSS_ACTY1_NET_LSS_AMT = -alternating,
    BUS_LSS_ACTY2_NET_LSS_AMT = -alternating,
    BUS_LSS_ACTY3_NET_LSS_AMT = -alternating,
    OTHR_BUS_INCM_NPP_AMT = rep(c(1000, -1000), length.out = n),
    OTHR_BUS_INCM_PP_AMT = rep(c(1000, -1000), length.out = n),
    NONPP_OTHR_DDCTNS_DSTBN_AMT = alternating,
    PP_OTHR_DDCTNS_DSTBN_AMT = alternating,
    AVLBL_OTHR_RFNDBL_TOS_AMT = alternating,
    ETPS_OTHRTHN_EXCSVCMPNT_AMT = alternating,
    stringsAsFactors = FALSE
  )
  list(spine = spine, source = source)
}

fixture <- make_tax_fixture()
has_value <- function(value) {
  !is.null(value) && any(!is.na(value) & nzchar(as.character(value)))
}

audit_occurrence <- function(i) {
  row <- gap[i, , drop = FALSE]
  period <- fplida:::.dil_period(
    c(row$official_product, row$official_table, ""),
    dataset = row$dataset,
    table_name = row$official_table
  )
  tax_value <- fplida:::.dil_tax_source_value(
    row$variable, row$official_description, row$dataset,
    fixture$source, fixture$spine, 20260803L, period
  )
  dataset_value <- fplida:::.dil_dataset_source_value(
    row$variable, row$official_description, row$dataset,
    fixture$source, fixture$spine, 20260803L, period,
    row$official_product, row$official_table, ""
  )
  general_value <- if (is.null(dataset_value)) {
    fplida:::.dil_general_value(
      row$variable, row$official_description, row$dataset,
      row$official_product, row$official_table, "",
      fixture$spine, 20260803L, FALSE, period
    )
  } else {
    NULL
  }
  final_value <- if (!is.null(dataset_value)) dataset_value else general_value

  tax_present <- has_value(tax_value)
  dataset_present <- has_value(dataset_value)
  general_present <- has_value(general_value)
  implementation_class <- if (!is.null(tax_value) && tax_present) {
    "tax_specific_rule"
  } else if (!is.null(tax_value)) {
    "typed_structural_missing"
  } else if (!is.null(dataset_value) && dataset_present) {
    "legacy_cgt_derivation"
  } else if (is.null(dataset_value) && general_present) {
    "generic_structural_geography"
  } else {
    "unsupported_private_codeframe"
  }
  status <- if (has_value(final_value)) {
    "populated"
  } else if (implementation_class == "typed_structural_missing") {
    "synthetic_structural"
  } else {
    "unsupported_private_codeframe"
  }
  data.frame(
    dataset = row$dataset,
    variable = row$variable,
    status = status,
    implementation_class = implementation_class,
    stringsAsFactors = FALSE
  )
}

occurrence_audit <- do.call(
  rbind, lapply(seq_len(nrow(gap)), audit_occurrence)
)
occurrence_audit$key <- pair_key(
  occurrence_audit$dataset, occurrence_audit$variable
)

status_count <- function(key, status) {
  sum(occurrence_audit$key == key & occurrence_audit$status == status)
}

implementation_rule <- function(dataset, variable, implementation_class) {
  if (implementation_class == "legacy_cgt_derivation") {
    return(paste0(
      "R/dil_admin_value_rules.R:.dil_cgt_source_value() decomposes the ",
      "generated CGT total, asset, loss, discount and concession amounts ",
      "into the named schedule component while preserving accounting bounds."
    ))
  }
  if (implementation_class == "generic_structural_geography") {
    return(paste0(
      "The shared administrative geography rule samples an official LGA ",
      "code for the product year and keeps the code consistent with the ",
      "person's synthetic state."
    ))
  }
  if (implementation_class == "typed_structural_missing") {
    return(paste0(
      "R/dil_tax_values.R returns NA_character_. The fields apply to ",
      "company schedules, while project_cgt() emits individual schedules."
    ))
  }
  if (implementation_class == "unsupported_private_codeframe") {
    return(paste0(
      "R/dil_tax_values.R has no field rule and returns NULL. The shared ",
      "categorical fallback returns NA_character_ so an unverified private ",
      "ATO codeframe is not concealed by invented values."
    ))
  }

  if (dataset == "CGT") {
    return(switch(
      variable,
      ENTITY_TYPE = "Populate I because project_cgt() emits individual schedules.",
      CG_ERNT_ARNGMTS_PRTY_CD = paste0(
        "Populate a deterministic Y/N earnout-arrangement indicator."
      ),
      CG_EXMPTN_15_YR_FR_SB_CD = paste0(
        "When small-business concessions apply, sample the public schedule ",
        "codes S, U, R, G and O; otherwise retain missing."
      ),
      paste0(
        "Populate the coherent scrip-rollover Y/N field from one shared ",
        "synthetic rollover context."
      )
    ))
  }
  if (dataset == "SMSF") {
    return(paste0(
      "R/dil_tax_values.R:.dil_tax_smsf_source_value() uses the named SMSF ",
      "annual-return domain. It conditions optional codes on the related ",
      "amount and uses age for lump-sum and income-stream categories."
    ))
  }
  if (dataset == "ATO_MCS") {
    return(paste0(
      "R/dil_tax_values.R:.dil_tax_mcs_source_value() applies the public ",
      "MCS reporting-specification domain and its reporting-vintage rule, ",
      "with a shared account phase, status and deceased-member context."
    ))
  }
  if (dataset == "ATO_CR") {
    return(paste0(
      "R/dil_tax_values.R:.dil_tax_ato_cr_source_value() derives the named ",
      "client/address status, address role, month-year of birth, or May-scope ",
      "indicator from the person spine and deterministic keys."
    ))
  }
  paste0(
    "R/dil_tax_values.R:.dil_tax_pit_source_value() applies the named ",
    "PIT structural or public-form rule. It derives the value from the ",
    "financial year, spine occupation/age/income, related amount, or a ",
    "shared spouse/business/tax context, and retains missing when inapplicable."
  )
}

evidence_source <- function(dataset, implementation_class) {
  if (implementation_class == "legacy_cgt_derivation") {
    return(paste(
      "R/dil_admin_value_rules.R; R/generate_cgt.R;",
      "tests/testthat/test-dil-admin-value-rules.R; PLIDA DIL"
    ))
  }
  if (implementation_class == "generic_structural_geography") {
    return(paste(
      "R/dil_geography_values.R; inst/extdata/codeframes/lga.tsv;",
      "PLIDA DIL"
    ))
  }
  if (implementation_class == "typed_structural_missing") {
    return("R/dil_tax_values.R; R/generate_cgt.R; PLIDA DIL")
  }
  if (implementation_class == "unsupported_private_codeframe") {
    return(paste(
      "inst/plida_metadata/variables.csv; R/dil_tax_values.R;",
      "R/complete_dil_structures.R"
    ))
  }
  paste0(
    "R/dil_tax_values.R; tests/testthat/test-dil-tax-values.R; ",
    "inst/plida_metadata/variables.csv"
  )
}

evidence_url <- function(dataset, implementation_class) {
  if (implementation_class == "generic_structural_geography") {
    return("https://geo.abs.gov.au/arcgis/rest/services/ASGS2024/LGA/MapServer")
  }
  if (dataset == "CGT") {
    return(paste0(
      "https://www.ato.gov.au/individuals-and-families/your-tax-return/",
      "instructions-to-complete-your-tax-return/mytax-instructions/2024/",
      "income/australian-income-or-losses-from-investments-or-property/",
      "capital-gains-tax-schedule"
    ))
  }
  if (dataset == "SMSF") {
    return("https://www.ato.gov.au/api/public/content/0-7986dcf2-8213-49ee-8403-b54dccb52ad8")
  }
  if (dataset == "ATO_MCS") {
    return("https://softwaredevelopers.ato.gov.au/MCSspecification")
  }
  if (dataset %in% c("PIT_ITR", "PIT_PS", "PIT_IE", "BUSOWN")) {
    return("https://www.ato.gov.au/api/public/content/a9a181ef-2223-491e-9e44-2de03b272a91_Individual_tax_return_instructions_2022_pdf")
  }
  "https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/person-level-integrated-data-asset-plida"
}

implementation_caveat <- function(status, implementation_class) {
  if (status == "unsupported_private_codeframe") {
    return(paste0(
      "The DIL proves the field exists but does not enumerate its values. ",
      "Keep typed missing until an exact source or defensible rule is recorded."
    ))
  }
  if (implementation_class == "typed_structural_missing") {
    return(paste0(
      "This determination is correct only for the current person-schedule ",
      "generator; a future company/trust CGT source must populate the fields."
    ))
  }
  if (implementation_class == "legacy_cgt_derivation") {
    return(paste0(
      "The identities are coherent, but the decomposition shares are local ",
      "synthetic assumptions rather than observed ATO distributions."
    ))
  }
  if (implementation_class == "generic_structural_geography") {
    return(paste0(
      "The codeframe and state relationship are official; the person's exact ",
      "LGA and its within-state distribution are synthetic."
    ))
  }
  paste0(
    "The code domain or field semantics are source-backed, but sampled ",
    "frequencies and record-level assignments remain synthetic."
  )
}

candidate <- data.frame(
  dataset = c(
    rep("PIT_ITR", 3L),
    rep("PIT_ITR", 5L),
    "PIT_ITR", "PIT_ITR"
  ),
  variable = c(
    "BBI_ELGBL_AST_BBI_CD", "TFE_ELGBL_AST_OPT_OUT_CD",
    "BUSINESS_MARKET_SEGMENT",
    "HLTH_FND_CD", "HLTH_FND_CD_1", "HLTH_FND_CD_2",
    "HLTH_FND_CD_3", "HLTH_FND_CD_4",
    "INCM_TYP_CD", "XMEMPTYPENFP"
  ),
  candidate_status = c(
    rep("implemented_official_cross_asset_codeframe", 3L),
    rep("implemented_current_public_codeframe", 5L),
    "public_related_domain_requires_source_equivalence",
    "clear_binary_semantics_requires_encoding_confirmation"
  ),
  candidate_rule = c(
    paste0(
      "Use character codes 5, 10 and 15 for some eligible assets, all ",
      "eligible assets and no opt-out; preserve invalid/no response as missing."
    ),
    paste0(
      "Use character codes 5, 10 and 15 for some eligible assets, all ",
      "eligible assets and no opt-out; preserve invalid/no response as missing."
    ),
    paste0(
      "Use GOV, LGE, SME, INB, MIC and NFP. Derive INB for non-business ",
      "individuals and use linked business/employer state when available."
    ),
    rep(paste0(
      "Sample a three-letter ATO health-insurer ID from a public ",
      "current register, consistently across a person's policy fields."
    ), 5L),
    paste0(
      "Use the ATO income-type domain only after confirming that PIT wages ",
      "INCM_TYP_CD is source-equivalent to the STP/payment-summary field."
    ),
    paste0(
      "Derive a binary not-for-profit-employer indicator from linked employer ",
      "market segment; retain missing when there is no observed main employer."
    )
  ),
  candidate_evidence_source = c(
    rep(paste0(
      "inst/blade_metadata/variables.csv, Table 6 Business Income Tax ",
      "valid responses (official April 2026 BLADE DIL snapshot)"
    ), 3L),
    rep(paste0(
      "Australian Government privatehealth.gov.au insurer register; ",
      "ATO 2020 pre-filling report specification"
    ), 5L),
    "ATO STP Phase 2 income-types guidance",
    "PLIDA DIL field description; linked BLADE business market segment"
  ),
  candidate_evidence_url = c(
    rep(paste0(
      "https://www.abs.gov.au/statistics/microdata-tablebuilder/",
      "available-microdata-tablebuilder/business-longitudinal-analysis-",
      "data-environment-blade"
    ), 3L),
    rep("https://www.privatehealth.gov.au/dynamic/Insurer/Index/", 5L),
    "https://www.ato.gov.au/api/public/content/0-2f417730-27cf-4825-8b51-ee53bfe00358",
    "https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/person-level-integrated-data-asset-plida"
  ),
  candidate_caveat = c(
    paste0(
      "The BLADE item has an i_ prefix and numeric storage while PLIDA stores ",
      "character. The implementation stores the code numbers as character."
    ),
    paste0(
      "The BLADE item has an i_ prefix and numeric storage while PLIDA stores ",
      "character. The implementation stores the code numbers as character."
    ),
    paste0(
      "The exact concept matches, but the PLIDA 2023-24 occurrence extends ",
      "one year beyond the checked-in BLADE availability list."
    ),
    rep(paste0(
      "The package uses the current public register for all DIL years. The ",
      "source does not provide year-specific insurer codes for 2009-10 to ",
      "2023-24."
    ), 5L),
    paste0(
      "The public STP domain is related but not proven identical to this ITR ",
      "wages-table field, especially before STP Phase 2."
    ),
    paste0(
      "The semantics are binary, but the DIL does not publish the stored ",
      "Y/N, 1/0 or other encoding. Confirm encoding before implementation."
    )
  ),
  stringsAsFactors = FALSE
)
candidate <- rbind(
  candidate,
  data.frame(
    dataset = "PIT_ITR",
    variable = "LODGMENT_SOURCE",
    candidate_status = "implemented_alife_manual_codeframe",
    candidate_rule = paste0(
      "Use the ALife c_lodgement_type codes. Sample AGNT_ELS, ETAX, ",
      "SP_MYTAX and SP_PPR with the ATO annual broad-channel counts."
    ),
    candidate_evidence_source = paste0(
      "inst/plida_metadata/alife_variable_manual.csv; ",
      "inst/extdata/codeframes/ato-pit-lodgement-channel-weights.tsv; ",
      "ATO Taxation Statistics Snapshot Table 5"
    ),
    candidate_evidence_url = paste(
      "https://alife-research.app/assets/manual/itr/c_lodgement_type.md",
      paste0(
        "https://data.gov.au/data/dataset/faea4485-f407-457d-97f8-",
        "3f0822ccd654/resource/e19017b8-3894-44e2-995a-1c253c96977e"
      ),
      sep = " | "
    ),
    candidate_caveat = paste0(
      "The DIL-to-ALife link is a semantic crosswalk, not an identifier ",
      "match. Public counts do not separate AGNT_PPR, TELE or TXPK_EXP, so ",
      "the model gives those valid ALife codes zero weight."
    ),
    stringsAsFactors = FALSE
  )
)
candidate$key <- pair_key(candidate$dataset, candidate$variable)
stopifnot(anyDuplicated(candidate$key) == 0L)

ledger_rows <- lapply(seq_len(nrow(register)), function(i) {
  row <- register[i, , drop = FALSE]
  key <- register_key[[i]]
  occurrence <- occurrence_audit[occurrence_audit$key == key, , drop = FALSE]
  implementation_classes <- unique(occurrence$implementation_class)
  statuses <- unique(occurrence$status)
  stopifnot(length(implementation_classes) == 1L, length(statuses) == 1L)
  implementation_class <- implementation_classes[[1L]]
  status <- statuses[[1L]]
  candidate_hit <- match(key, candidate$key)
  candidate_values <- if (is.na(candidate_hit)) {
    list(
      candidate_status = "no_defensible_rule_identified",
      candidate_rule = "Keep typed missing pending an exact source.",
      candidate_evidence_source = "No exact public codeframe found in this audit.",
      candidate_evidence_url = "",
      candidate_caveat = paste0(
        "A broad concept or similarly named field is not enough to infer the ",
        "private ATO value encoding."
      )
    )
  } else {
    as.list(candidate[candidate_hit, setdiff(names(candidate), "key")])
  }

  data.frame(
    dataset = row$dataset,
    variable = row$variable,
    occurrence_count = as.integer(row$occurrence_count),
    product_count = as.integer(row$product_count),
    table_count = as.integer(row$table_count),
    type = row$type,
    official_descriptions = row$official_descriptions,
    status = status,
    populated_occurrence_count = status_count(key, "populated"),
    synthetic_structural_occurrence_count = status_count(
      key, "synthetic_structural"
    ),
    unsupported_private_codeframe_occurrence_count = status_count(
      key, "unsupported_private_codeframe"
    ),
    implementation_class = implementation_class,
    determination_rule = implementation_rule(
      row$dataset, row$variable, implementation_class
    ),
    evidence_source = evidence_source(row$dataset, implementation_class),
    evidence_url = evidence_url(row$dataset, implementation_class),
    caveat = implementation_caveat(status, implementation_class),
    candidate_status = candidate_values$candidate_status,
    candidate_rule = candidate_values$candidate_rule,
    candidate_evidence_source = candidate_values$candidate_evidence_source,
    candidate_evidence_url = candidate_values$candidate_evidence_url,
    candidate_caveat = candidate_values$candidate_caveat,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})

ledger <- do.call(rbind, ledger_rows)
ledger <- ledger[order(match(ledger$dataset, tax_datasets), ledger$variable), ]
rownames(ledger) <- NULL

stopifnot(
  nrow(ledger) == 184L,
  anyDuplicated(pair_key(ledger$dataset, ledger$variable)) == 0L,
  sum(ledger$occurrence_count) == 2222L,
  sum(ledger$populated_occurrence_count) == 2042L,
  sum(ledger$synthetic_structural_occurrence_count) == 26L,
  sum(ledger$unsupported_private_codeframe_occurrence_count) == 154L,
  identical(
    as.integer(table(factor(
      ledger$status,
      levels = c(
        "populated", "synthetic_structural",
        "unsupported_private_codeframe"
      )
    ))),
    c(161L, 2L, 21L)
  ),
  identical(
    as.integer(table(factor(
      ledger$implementation_class,
      levels = c(
        "tax_specific_rule", "legacy_cgt_derivation",
        "generic_structural_geography", "typed_structural_missing",
        "unsupported_private_codeframe"
      )
    ))),
    c(89L, 68L, 4L, 2L, 21L)
  ),
  identical(
    as.integer(tapply(
      ledger$occurrence_count,
      factor(
        ledger$implementation_class,
        levels = c(
          "tax_specific_rule", "legacy_cgt_derivation",
          "generic_structural_geography", "typed_structural_missing",
          "unsupported_private_codeframe"
        )
      ),
      sum
    )),
    c(1078L, 926L, 38L, 26L, 154L)
  ),
  all(nzchar(ledger$determination_rule)),
  all(nzchar(ledger$evidence_source)),
  all(nzchar(ledger$caveat))
)

output <- "inst/internal-docs/tax-admin-value-decisions.csv"
utils::write.csv(ledger, output, row.names = FALSE, na = "")
message(
  "Wrote ", nrow(ledger), " tax decisions covering ",
  sum(ledger$occurrence_count), " occurrences: ",
  sum(ledger$populated_occurrence_count), " populated, ",
  sum(ledger$synthetic_structural_occurrence_count), " structural, ",
  sum(ledger$unsupported_private_codeframe_occurrence_count),
  " unsupported."
)
