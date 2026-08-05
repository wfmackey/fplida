.blade_test_business_rows <- function() {
  data.frame(
    state = rep(seq_len(8L), each = 4L),
    bn = sprintf("BN%011d", seq_len(32L)),
    id = sprintf("ID%010d", seq_len(32L)),
    cn = sprintf("CN%009d", seq_len(32L)),
    fn = sprintf("FN%010d", seq_len(32L)),
    bg_id = "",
    legal_form = rep(c("Company", "Sole trader", "Partnership", "Trust"),
                     8L),
    anzsic06 = rep(c("0111", "2399", "4511", "7000"), 8L),
    representative_anzsco_code = rep("261313", 32L),
    x_pcode = sprintf("%04d", 2000L + seq_len(32L)),
    turnover = seq(250000, 8000000, length.out = 32L),
    annual_wages = seq(90000, 2200000, length.out = 32L),
    d_total_payees = rep(4:11, 4L),
    stringsAsFactors = FALSE
  )
}

.blade_test_fields <- function(table_number, fields,
                               business_rows = .blade_test_business_rows(),
                               seed = 20260803L) {
  metadata <- fplida:::.blade_variables(table_number)
  metadata <- metadata[metadata[["Variable.Name"]] %in% fields, , drop = FALSE]
  fplida:::.make_blade_frame(
    variable_names = fields,
    business_rows = business_rows,
    table_number = table_number,
    product_name = unique(metadata[["Product.Name"]])[[1L]],
    seed = seed,
    variables = metadata
  )
}

test_that("BLADE metadata includes April 2026 numbered tables", {
  tables_path <- system.file("blade_metadata", "tables.csv",
                             package = "fplida")
  variables_path <- system.file("blade_metadata", "variables.csv",
                                package = "fplida")
  keys_path <- system.file("blade_metadata", "keys.csv",
                           package = "fplida")
  domains_path <- system.file("blade_metadata", "domains.csv",
                              package = "fplida")

  tables <- read.csv(tables_path, stringsAsFactors = FALSE,
                     check.names = FALSE)
  variables <- read.csv(variables_path, stringsAsFactors = FALSE,
                        check.names = FALSE)
  keys <- read.csv(keys_path, stringsAsFactors = FALSE,
                   check.names = FALSE)
  domains <- read.csv(domains_path, stringsAsFactors = FALSE,
                      check.names = FALSE)

  expect_equal(nrow(tables), 62L)
  expect_true(all(c(1L, 4L, 6L, 7L) %in% tables[["Table.Number"]]))
  expect_true(all(c("bn", "id", "tsid") %in% variables[["Variable.Name"]]))
  expect_true(all(c("ID to BN Key", "CN to BN Key") %in%
                    keys[["Key.Name"]]))
  expect_true(all(c("bn", "id", "bg_id", "cn") %in%
                    keys[["Variable.Name"]]))
  expect_gt(nrow(domains), 2500L)
  expect_true(all(c("A3", "A6", "A10", "A11", "A12", "A21") %in%
                    domains[["Source.Sheet"]]))
  expect_equal(fplida:::dataset_to_agency("BLADE"), "ABS")
})

test_that("BLADE table source classification separates surveys from admin data", {
  survey_tables <- c(8L:23L, 38L:46L)
  admin_tables <- setdiff(seq_len(62L), survey_tables)

  expect_true(all(fplida:::.blade_is_survey_table(survey_tables)))
  expect_false(any(fplida:::.blade_is_survey_table(admin_tables)))
  expect_equal(
    fplida:::.blade_table_source_type(c(1L, 7L, 8L, 17L, 38L, 47L)),
    c("administrative", "administrative", "survey", "survey", "survey",
      "administrative")
  )
})

test_that("BLADE metadata classifier uses published domains, not generic codes", {
  business_rows <- .blade_test_business_rows()
  variables <- fplida:::.blade_variables()

  make_fields <- function(table_number, fields) {
    metadata <- variables[
      variables[["Table.Number"]] == table_number &
        variables[["Variable.Name"]] %in% fields,
      , drop = FALSE
    ]
    fplida:::.make_blade_frame(
      variable_names = fields,
      business_rows = business_rows,
      table_number = table_number,
      product_name = unique(metadata[["Product.Name"]])[[1L]],
      seed = 20260803L,
      variables = metadata
    )
  }

  longitudinal <- make_fields(2L, "impute")
  legacy_classifications <- make_fields(
    2L, c("cast_anzsic93", "cast_sisca06", "cast_sisca93")
  )
  dealroom <- make_fields(59L, "revenue_me")
  expect_true(all(longitudinal$impute %in% 0L:3L))
  expect_true(all(legacy_classifications$cast_anzsic93 == 9999L))
  expect_true(all(legacy_classifications$cast_sisca06 == 9999L))
  expect_true(all(legacy_classifications$cast_sisca93 == 9999L))
  expect_true(all(dealroom$revenue_me %in% 0L:1L))

  agriculture <- make_fields(3L, "d_ag_anzsic06_group")
  expect_equal(agriculture$d_ag_anzsic06_group,
               as.integer(substr(business_rows$anzsic06, 1L, 3L)))

  bit <- make_fields(6L, c(
    "t_cr_tw_abn_not_qtd_nobnfcry_amt", "p_tw_abn_not_qtd_amt",
    "t_tw_abn_not_qtd_amt", "p_tw_abn_not_qtd_cr_share_amt",
    "t_tw_abn_not_qtd_share_amt", "i_psi_abn_not_qtd_amt",
    "c_inettrad", "c_frgnshre", "c_pcnonmem", "bit_comp_yyyy",
    "c_erlyst_yr_net_exmt_incm_amt"
  ))
  expect_true(all(vapply(bit[1:6], is.numeric, logical(1))))
  expect_false(any(vapply(bit[1:6], function(x) {
    any(grepl("^BN", as.character(x)))
  }, logical(1))))
  expect_true(all(stats::na.omit(bit$c_inettrad) %in% c("Y", "N")))
  expect_true(all(stats::na.omit(bit$c_frgnshre) >= 0 &
                    stats::na.omit(bit$c_frgnshre) <= 100))
  expect_gt(length(unique(stats::na.omit(bit$c_frgnshre))), 2L)
  expect_true(all(stats::na.omit(bit$bit_comp_yyyy) == 1L))
  expect_true(any(stats::na.omit(bit$c_erlyst_yr_net_exmt_incm_amt) > 2025))

  stp <- make_fields(7L, c(
    "month_actioned", "ed_forgn_incm_exmt",
    "ed_incm_cmnty_dev_emplt_prjct", "ed_incm_othr",
    "ed_pyr_spntn_cntrbtn_rprtbl", "ed_rfb_exmt", "ed_rfb_txbl",
    "ed_wheld"
  ))
  expect_true(all(stp$month_actioned %in% month.abb))
  expect_true(all(vapply(stp[-1L], is.numeric, logical(1))))
  expect_true(all(vapply(stp[-1L], function(x) any(x > 1), logical(1))))
  stp_branch <- make_fields(7L, "branch_number")
  expect_true(is.numeric(stp_branch$branch_number))
  expect_lt(max(stp_branch$branch_number, na.rm = TRUE), 100L)

  ip_counts <- make_fields(28L, c(
    "financial_year", "p_filed_count", "p_filed_modal_class",
    "p_granted_class_scope"
  ))
  expect_true(all(ip_counts$financial_year >= 2000L))
  expect_false(all(ip_counts$p_filed_count == ip_counts$financial_year))
  expect_true(all(stats::na.omit(ip_counts$p_filed_modal_class) %in%
                    fplida:::.blade_domain_values("iplord_technology")))
  expect_gt(length(unique(stats::na.omit(ip_counts$p_filed_modal_class))), 2L)
  expect_true(is.numeric(ip_counts$p_granted_class_scope))
  expect_lt(max(ip_counts$p_granted_class_scope, na.rm = TRUE), 100L)

  trade <- make_fields(
    48L, c("nature_of_tariff_code_im", "port_of_discharge_im")
  )
  expect_true(all(trade$nature_of_tariff_code_im %in%
                    c("C", "G", "N", "Q", "R")))
  expect_true(all(trade$port_of_discharge_im %in% c(
    "101", "102", "103", "104", "106", "107", "112", "118",
    "201", "202", "203", "204", "212", "298",
    "301", "303", "304", "305", "306", "307", "309", "311",
    "401", "403", "408", "409", "411", "413", "417", "418",
    "501", "502", "504", "505", "508", "510", "512", "513",
    "601", "602", "603", "605", "610", "611", "698",
    "701", "703", "704", "780", "798", "801", "898"
  )))

  patents <- make_fields(29L, "cii_group")
  patent_dates <- make_fields(31L, "classification_removal_date")
  cgt <- make_fields(35L, "k_scrpt_rlovr_cd_a")
  agreements <- make_fields(49L, "performancepayappliestogroup")
  dealroom_industry <- make_fields(59L, "ind_enterprise")
  expect_true(all(patents$cii_group %in%
                    c("CII-Likely-MoM", "CII-Unlikely-MoM", "Non-CII")))
  expect_s3_class(patent_dates$classification_removal_date, "Date")
  expect_true(all(cgt$k_scrpt_rlovr_cd_a %in% c("Y", "N")))
  expect_true(all(stats::na.omit(agreements$performancepayappliestogroup) ==
                    "Y"))
  expect_true(all(dealroom_industry$ind_enterprise %in% 0L:1L))

  cgt_temporal_words <- make_fields(
    35L, c("cl_cy_totl_amt", "cg_exmptn_15_yr_fr_sb_cd")
  )
  expect_true(any(cgt_temporal_words$cl_cy_totl_amt != 2023))
  expect_true(all(cgt_temporal_words$cg_exmptn_15_yr_fr_sb_cd %in%
                    c(5L, 10L, 15L, 20L, 25L)))

  imports <- make_fields(48L, "fob_value_im")
  agreements_numeric <- make_fields(
    49L, c("awards", "nominalduration", "unpaidfamilycarersleave")
  )
  flad <- make_fields(56L, c("z_ll_count", "z_ll_filter", "z_year"))
  dealroom_context <- make_fields(
    59L, c("investors_swf_n", "s_ind_legal_documents", "tech_iot",
           "income_subscription")
  )
  family_violence <- make_fields(60L, "entity_organisation_location")
  max_panel <- make_fields(
    61L, c("abei", "csexportguidance", "emdgamountapproved")
  )
  expect_true(is.numeric(imports$fob_value_im))
  expect_true(all(vapply(agreements_numeric, is.numeric, logical(1))))
  expect_true(all(vapply(flad, is.numeric, logical(1))))
  expect_false(all(flad$z_ll_count == flad$z_year))
  expect_true(all(dealroom_context$s_ind_legal_documents %in% 0L:1L))
  expect_true(all(dealroom_context$tech_iot %in% 0L:1L))
  expect_true(all(dealroom_context$income_subscription %in% 0L:1L))
  expect_lt(max(dealroom_context$investors_swf_n, na.rm = TRUE), 100L)
  expect_true(all(family_violence$entity_organisation_location %in% 1L:8L))
  expect_true(all(max_panel$abei %in% 0L:1L))
  expect_lt(max(max_panel$csexportguidance, na.rm = TRUE), 100L)
  expect_true(any(max_panel$emdgamountapproved > 100L))

  wad <- make_fields(51L, "typeofvariation")
  emerging <- make_fields(52L, "type")
  rdti <- make_fields(
    53L, c("companyhasultimateholdingcompany",
           "applicationforheadorsubsidiary")
  )
  capex <- make_fields(18L, "impute_capex")
  expect_true(all(wad$typeofvariation %in%
                    c("wages", "conditions", "wages/conditions")))
  expect_identical(unique(emerging$type), "ADOPTER")
  expect_true(all(rdti$companyhasultimateholdingcompany %in%
                    c("Yes", "No", "N/A")))
  expect_true(all(rdti$applicationforheadorsubsidiary %in% c(
    "The head company", "Subsidiary members",
    "The head company and Subsidiary members"
  )))
  expect_true(all(capex$impute_capex %in%
                    c("Reported", "Impute partial", "Impute full",
                      "Winsorised")))

  survey_text <- c(
    unlist(make_fields(9L, c("periodc", "digtecoth_s")),
           use.names = FALSE),
    unlist(make_fields(10L, "sklegal_im"), use.names = FALSE)
  )
  expect_false(any(grepl("^C[0-9]{2,3}$", as.character(survey_text))))
})

test_that("BLADE admin fallbacks vary by variable and respect applicability", {
  rows <- .blade_test_business_rows()
  bas <- .blade_test_fields(4L, "month_actioned", rows)
  expect_gt(length(unique(bas$month_actioned)), 6L)
  expect_true(all(bas$month_actioned %in% month.abb))

  bit <- .blade_test_fields(6L, c(
    "c_busmarin", "c_fcrncy", "c_tfe_elgbl_ast_opt_out_num",
    "i_totlwage", "c_grosintr", "c_grosdivd", "c_totlinc",
    "p_costsale", "p_superann", "p_totlexps"
  ), rows)
  individual <- rows$legal_form == "Sole trader"
  expect_true(all(is.na(bit$i_totlwage[!individual])))
  expect_true(all(bit$i_totlwage[individual] > 0))
  expect_true(all(stats::na.omit(bit$c_busmarin) %in%
                    c("GOV", "LGE", "SME", "INB", "MIC", "NFP")))
  expect_true(all(stats::na.omit(bit$c_fcrncy) %in%
                    as.integer(fplida:::.blade_domain_values(
                      variable_name = "c_fcrncy"
                    ))))
  expect_true(all(grepl("^[0-9]{6}$",
                        stats::na.omit(bit$c_tfe_elgbl_ast_opt_out_num))))
  expect_false(identical(bit$c_grosintr, bit$c_grosdivd))
  expect_false(identical(bit$c_grosintr, bit$c_totlinc))
  expect_false(identical(bit$p_costsale, bit$p_superann))
  expect_false(identical(bit$p_costsale, bit$p_totlexps))

  ip_counts <- .blade_test_fields(
    28L, c("p_filed_count", "p_granted_count", "p_retired_count"), rows
  )
  value_patterns <- vapply(ip_counts, paste, character(1), collapse = "|")
  expect_equal(length(unique(value_patterns)), 3L)

  dealroom <- .blade_test_fields(
    59L, c("round_amount", "total_funding", "valuation_min",
           "valuation_max", "s_ind_energy_oilgas", "s_ind_security_data"),
    rows
  )
  expect_false(identical(dealroom$round_amount, dealroom$total_funding))
  expect_false(identical(dealroom$s_ind_energy_oilgas,
                         dealroom$s_ind_security_data))
  expect_true(all(dealroom$valuation_min <= dealroom$valuation_max))
})

test_that("BLADE admin categorical values use published workbook domains", {
  rows <- .blade_test_business_rows()
  appointment <- .blade_test_fields(26L, "appointment_type", rows)
  expect_gt(length(unique(appointment$appointment_type)), 4L)
  expect_false(any(is.na(appointment$appointment_type)))

  applications <- .blade_test_fields(
    29L, c("ip_right_type", "auexamsection", "f_country_of_earliest_filing",
           "ip_right_sub_type", "status"), rows
  )
  expect_true(all(applications$ip_right_type %in%
                    c("design", "patent", "trade_mark")))
  expect_true(all(applications$f_country_of_earliest_filing %in%
                    fplida:::.blade_domain_values("iso_country_alpha2")))
  expect_true(all(stats::na.omit(applications$ip_right_sub_type) %in%
                    fplida:::.blade_domain_values("ip_right_sub_type")))
  expect_true(all(applications$status %in%
                    fplida:::.blade_domain_values("ip_status")))

  events <- .blade_test_fields(32L, c("event_category", "event_type"), rows)
  expect_true(all(events$event_category %in%
                    fplida:::.blade_domain_values("ip_event_category")))
  expect_true(all(events$event_type %in%
                    fplida:::.blade_domain_values("ip_event_type")))

  trade <- .blade_test_fields(
    47L, c("country_of_final_dest_ex", "port_of_discharge_ex",
           "port_of_loading_ex", "invoice_currency_ex", "unit_of_quantity_ex"),
    rows
  )
  expect_true(all(trade$country_of_final_dest_ex %in%
                    fplida:::.blade_domain_values("trade_country_code")))
  expect_true(all(trade$port_of_discharge_ex %in%
                    fplida:::.blade_domain_values("trade_foreign_port")))
  expect_true(all(trade$invoice_currency_ex %in%
                    fplida:::.blade_domain_values("trade_currency")))

  dealroom <- .blade_test_fields(
    59L, c("round_type", "hq_country", "client_focus"), rows
  )
  expect_true(all(dealroom$round_type %in%
                    fplida:::.blade_domain_values(variable_name = "round_type")))
  expect_true(all(dealroom$hq_country %in%
                    fplida:::.blade_domain_values(variable_name = "hq_country")))
  expect_true(all(dealroom$client_focus %in% c("business", "consumer", "both")))

  max_values <- .blade_test_fields(
    62L, c("mktdescription", "mktevent", "mktselected"), rows
  )
  expect_true(all(max_values$mktselected %in%
                    fplida:::.blade_domain_values("max_market")))
  expect_gt(length(unique(max_values$mktdescription)), 4L)
  expect_gt(length(unique(max_values$mktevent)), 3L)
})

test_that("external CSM gaps use explicitly non-official local domains", {
  rows <- .blade_test_business_rows()
  fields <- list(
    `47` = c("commodity_code_ex", "sitc_item_code_ex"),
    `48` = c(
      "bec_group_code_im", "commodity_code_im", "preference_code_im",
      "sitc_item_code_im", "treatment_code_im"
    )
  )
  domains <- fplida:::.blade_domains()
  local <- domains[
    domains[["Variable.Name"]] %in% unlist(fields, use.names = FALSE),
    , drop = FALSE
  ]

  expect_true(nrow(local) > 0L)
  expect_true(all(local[["Domain"]] == "local_plausible_trade_code"))
  expect_true(all(local[["Source.Sheet"]] ==
                    "LOCAL_PLAUSIBLE_NOT_OFFICIAL"))

  generated <- lapply(names(fields), function(table_number) {
    .blade_test_fields(as.integer(table_number), fields[[table_number]], rows)
  })
  names(generated) <- names(fields)
  for (table_number in names(fields)) {
    for (field in fields[[table_number]]) {
      values <- generated[[table_number]][[field]]
      local_values <- fplida:::.blade_domain_values(variable_name = field)
      expect_false(anyNA(values), info = field)
      expect_true(all(values %in% local_values), info = field)
      expect_gt(length(unique(values)), 1L)
    }
  }
  expect_true(all(nchar(generated[["47"]]$commodity_code_ex) == 8L))
  expect_true(all(nchar(generated[["47"]]$sitc_item_code_ex) == 5L))
  expect_true(all(nchar(generated[["48"]]$bec_group_code_im) == 3L))
  expect_true(all(nchar(generated[["48"]]$commodity_code_im) == 10L))
  expect_true(all(nchar(generated[["48"]]$sitc_item_code_im) == 5L))
  expect_true(all(nchar(generated[["48"]]$treatment_code_im) == 3L))
})

test_that("BLADE admin date sequences are chronologically coherent", {
  rows <- .blade_test_business_rows()
  applications <- .blade_test_fields(29L, c(
    "priority_date", "earliest_filed_date", "f_intl_earliest_filing_date",
    "application_date", "f_earliest_grant_date",
    "gained_registration_status_date", "gained_enforceable_status_date",
    "deemed_retired_date"
  ), rows)
  expect_true(all(applications$priority_date <= applications$application_date))
  expect_true(all(applications$earliest_filed_date <= applications$application_date))
  expect_true(all(applications$f_intl_earliest_filing_date <=
                    applications$f_earliest_grant_date))
  expect_true(all(applications$application_date < applications$deemed_retired_date))
  expect_true(all(applications$f_earliest_grant_date <=
                    applications$gained_registration_status_date))
  expect_true(all(applications$gained_registration_status_date <=
                    applications$gained_enforceable_status_date))

  agreements <- .blade_test_fields(49L, c(
    "certificationdt", "commencementdt", "expirydt", "terminationdt",
    paste0("wageincreasedate", 1:18)
  ), rows)
  expect_true(all(agreements$certificationdt <= agreements$commencementdt))
  expect_true(all(agreements$commencementdt < agreements$expirydt))
  expect_true(all(agreements$commencementdt < agreements$terminationdt))
  for (name in paste0("wageincreasedate", 1:18)) {
    expect_true(all(stats::na.omit(agreements[[name]] -
                                     agreements$commencementdt) > 0))
  }

  rdti <- .blade_test_fields(53L, c(
    "rdti_incomeperiodstarton", "rdti_incomeperiodstendon",
    "rdti_projectstartdate", "rdti_projectenddate",
    "rdti_totalnumberofemployees", "numberofemployeesengagedinrd"
  ), rows)
  expect_true(all(rdti$rdti_incomeperiodstarton < rdti$rdti_incomeperiodstendon))
  expect_true(all(rdti$rdti_projectstartdate < rdti$rdti_projectenddate))
  expect_true(all(rdti$numberofemployeesengagedinrd <=
                    rdti$rdti_totalnumberofemployees))
})

test_that("Rust blade stable_name_seed matches the R helper (port stage 0)", {
  vals <- c("bn", "id", "x_itip", "x_gst_bn", "c_agrgtd_tnovr_rng_cd",
            "ed_anl_lng_srvc_unsd_ls_a", "payfreq_eeh", "representative_anzsco_code",
            "blade-table-07-single-touch-payroll-stp", "")
  for (v in vals) {
    expect_equal(blade_stable_name_seed__(v),
                 fplida:::.stable_name_seed(v), info = v)
  }
})

test_that("generate_blade_business_spine is consistent with person employment", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_blade_spine_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 300L, seed = 21L, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  business_spine <- generate_blade_business_spine(
    spine = spine,
    seed = 21L,
    output_dir = tmp,
    return_data = TRUE
  )

  run_dir <- getOption("fplida.run_dir")
  expect_true(file.exists(file.path(run_dir, "_system",
                                    "business-spine.parquet")))
  expect_true(file.exists(file.path(run_dir, "_system",
                                    "plida-blade-link.parquet")))
  expect_true(all(c("bn", "id", "state", "anzsic06", "employment_count",
                    "annual_wages", "turnover") %in% names(business_spine)))
  expect_true(all(c("x_anzsic06", "x_sisca08", "x_sisca06", "x_sector", "hcnt",
                    "linked_payg_rows", "linked_distinct_persons",
                    "payg_headcount_mismatch", "health_industry_flag") %in%
                    names(business_spine)))
  expect_true(all(grepl("^BN[0-9]{11}$", business_spine$bn)))
  expect_true(all(nchar(business_spine$id) == 10L))
  expect_true(all(nchar(business_spine$cn) == 10L))
  expect_true(all(nchar(business_spine$bg_id[business_spine$bg_id != ""]) ==
                    12L))
  expect_true(all(grepl("^BG[0-9]{10}$",
                        business_spine$bg_id[business_spine$bg_id != ""])))
  bg_links <- table(business_spine$bg_id[business_spine$bg_id != ""])
  expect_true(length(bg_links) == 0L || min(bg_links) > 1L)
  expect_true(any(business_spine$health_industry_flag == 1L))
  expect_true(any(business_spine$payg_headcount_mismatch == 1L))

  employed <- spine$baseline_employed == 1L &
    !is.na(spine$baseline_income) &
    spine$baseline_income > 0
  expect_equal(sum(business_spine$employment_count), sum(employed))
  expect_equal(round(sum(business_spine$annual_wages), 2),
               round(sum(spine$baseline_income[employed]), 2))

  link <- as.data.frame(arrow::read_parquet(file.path(
    run_dir, "_system", "plida-blade-link.parquet"
  )))
  expect_true(all(c("spine_id", "SYNTHETIC_AEUID", "bn", "BN",
                    "ABN_HASH_TRUNC", "id", "bg_id",
                    "SYNTHETIC_AEUID_ATO", "synthetic_aeuid_abs",
                    "SYNTHETIC_AEUID_ABS", "SYNTHETIC_AEUID_DHDA",
                    "ANZSCO_CODE", "occupation_health_flag",
                    "PAYG_GROSS_WAGES",
                    "relationship_type", "source_dataset") %in%
                    names(link)))
  expect_true(all(link$bn %in% business_spine$bn))
  expect_identical(link$BN, link$bn)
  expect_identical(link$ABN_HASH_TRUNC, link$bn)
  expect_true("employee" %in% link$relationship_type)
  expect_true(any(duplicated(link$spine_id)))
  expect_true(any(link$occupation_health_flag == 1L, na.rm = TRUE))

  employee_link <- link[link$relationship_type %in%
                          c("employee", "employee_secondary_job"), ]
  employee_state <- spine$state[match(employee_link$spine_id,
                                      spine$spine_id)]
  business_state <- business_spine$state[match(employee_link$BN,
                                               business_spine$bn)]
  expect_gt(mean(employee_state == business_state, na.rm = TRUE), 0.85)
})

test_that("generate_blade keeps admin records complete while sampling surveys", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_blade_source_type_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 500L, seed = 27L, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  business_spine <- generate_blade_business_spine(
    spine = spine,
    seed = 27L,
    n_businesses = 160L,
    output_dir = tmp,
    return_data = TRUE
  )
  frames <- generate_blade(
    business_spine = business_spine,
    spine = spine,
    seed = 27L,
    output_dir = tmp,
    tables = c(1L, 8L),
    sample_rate = 0.5,
    max_rows = 25L,
    return_data = TRUE
  )

  admin <- frames[["blade-table-01-cross-sectional-indicative-data-items"]]
  survey <- frames[["blade-table-08-business-characteristics-survey-bcs"]]

  expect_equal(nrow(admin), nrow(business_spine))
  expect_setequal(admin$bn, business_spine$bn)
  expect_equal(nrow(survey), 25L)
  expect_true(all(survey$id %in% business_spine$id))
  expect_lt(length(unique(survey$id)), length(unique(business_spine$id)))
})

test_that("BUSOWN business IDs resolve to the BLADE business spine", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_busown_blade_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 1000L, seed = 28L, output_dir = tmp,
                 return_data = FALSE, use_template = FALSE)
  business_spine <- generate_blade_business_spine(
    seed = 28L,
    n_businesses = 220L,
    output_dir = tmp,
    return_data = TRUE
  )
  generate_busown(seed = 28L, years = 2022L:2023L, output_dir = tmp,
                  return_data = FALSE)

  run_dir <- getOption("fplida.run_dir")
  busown_files <- list.files(file.path(run_dir, "ato-busown"),
                             pattern = "^madipge-ato-d-business-owners-.*\\.parquet$",
                             full.names = TRUE)
  expect_gt(length(busown_files), 0L)

  busown <- do.call(rbind, lapply(busown_files, function(path) {
    as.data.frame(arrow::read_parquet(path))
  }))
  expect_gt(nrow(busown), 0L)
  expect_true(all(grepl("^BN[0-9]{11}$", busown$ABN_HASH_TRUNC)))
  expect_true(all(busown$ABN_HASH_TRUNC %in% business_spine$bn))
})

test_that("STP employer and contractor BNs resolve to the BLADE business spine", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_stp_blade_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 3000L, seed = 29L, output_dir = tmp,
                 return_data = FALSE, use_template = FALSE)
  business_spine <- generate_blade_business_spine(
    seed = 29L,
    n_businesses = 700L,
    output_dir = tmp,
    return_data = TRUE
  )
  generate_stp(seed = 29L, years = 2020L, output_dir = tmp,
               return_data = FALSE, chunk_size = 1000L)

  run_dir <- getOption("fplida.run_dir")
  stp_files <- list.files(file.path(run_dir, "ato-stp"),
                          pattern = "\\.parquet$",
                          recursive = TRUE, full.names = TRUE)
  stp_files <- stp_files[grepl("/stp_standard_pay_events_", stp_files)]
  expect_gt(length(stp_files), 0L)

  pay <- as.data.frame(arrow::open_dataset(stp_files, format = "parquet"))
  contractor_bn <- pay$CNTRCTR_BN[
    !is.na(pay$CNTRCTR_BN) & nzchar(pay$CNTRCTR_BN)
  ]

  expect_gt(length(contractor_bn), 0L)
  expect_true(all(pay$BN %in% business_spine$bn))
  expect_true(all(contractor_bn %in% business_spine$bn))
})

test_that("generate_blade_business_spine handles secondary jobs without integer overflow", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_blade_overflow_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 5000L, seed = 24L, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  business_spine <- expect_no_warning(
    generate_blade_business_spine(
      spine = spine,
      seed = 24L,
      output_dir = tmp,
      return_data = TRUE
    )
  )

  run_dir <- getOption("fplida.run_dir")
  link <- as.data.frame(arrow::read_parquet(file.path(
    run_dir, "_system", "plida-blade-link.parquet"
  )))

  expect_false(anyNA(link$BN))
  expect_true(any(link$relationship_type == "employee_secondary_job"))
  expect_true(all(link$BN %in% business_spine$bn))
})

test_that("BLADE business locations use ABS MB allocation-compatible codes", {
  business_rows <- data.frame(
    bn = fplida:::.blade_numeric_id("BN", seq_len(8), 11L),
    state = seq_len(8),
    stringsAsFactors = FALSE
  )

  location <- fplida:::.blade_business_location_frame(
    variable_names = c("bn", "hashed_arid", "busloc_version", "tsid",
                       "quarter", "mesh_block_21", "sa2_code_21",
                       "geocode_precision", "address_type"),
    business_rows = business_rows,
    table_number = 25L,
    product_name = "blade-table-25-business-locations",
    seed = 31L
  )

  mb_lookup <- fplida:::.load_mb_lookup()
  mb_match <- match(location$mesh_block_21, mb_lookup$mb_code)

  expect_false(anyNA(mb_match))
  expect_identical(location$sa2_code_21, mb_lookup$sa2_code[mb_match])
  expect_identical(substr(location$sa2_code_21, 1L, 1L),
                   as.character(seq_len(8)))
  expect_true(all(nchar(location$hashed_arid) == 24L))
  expect_true(all(location$geocode_precision %in% 1L:3L))
  expect_true(all(location$address_type %in%
                    c("", "POSTAL", "ACCOUNTANT", "POSTAL, ACCOUNTANT")))

  historic <- fplida:::.blade_business_location_frame(
    variable_names = c("bn", "locations_version", "tsid", "quarter",
                       "mesh_block_21"),
    business_rows = business_rows,
    table_number = 24L,
    product_name = "blade-table-24-business-locations-historic",
    seed = 31L
  )

  expect_true(all(historic$mesh_block_21 %in% mb_lookup$mb_code))
  expect_false("sa2_code_21" %in% names(historic))

  ag_vars <- fplida:::.blade_variables(table_number = 3L)
  ag_geo <- fplida:::.make_blade_frame(
    variable_names = c("sa1_code_2021", "sa2_code_2021",
                       "sa2_name_2021"),
    business_rows = business_rows,
    table_number = 3L,
    product_name = "blade-table-03-agricultural-indicative-data-items",
    seed = 31L,
    variables = ag_vars
  )
  sa1_to_sa2 <- unique(mb_lookup[c("sa1_code", "sa2_code")])
  ag_match <- match(ag_geo$sa1_code_2021, sa1_to_sa2$sa1_code)

  expect_false(anyNA(ag_match))
  expect_identical(ag_geo$sa2_code_2021, sa1_to_sa2$sa2_code[ag_match])
  expect_identical(ag_geo$sa2_name_2021, paste("SA2", ag_geo$sa2_code_2021))

  flad_vars <- fplida:::.blade_variables(table_number = 56L)
  flad_geo <- fplida:::.make_blade_frame(
    variable_names = "sa2",
    business_rows = business_rows,
    table_number = 56L,
    product_name = "blade-table-56-farm-level-analytical-dataset-flad-control",
    seed = 31L,
    variables = flad_vars
  )
  expect_true(all(flad_geo$sa2 %in% mb_lookup$sa2_code))
})

test_that("generate_blade writes selected DIL-complete tables", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_blade_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 250L, seed = 22L, output_dir = tmp,
                 return_data = FALSE)
  generate_core(seed = 22L, output_dir = tmp, return_data = FALSE)
  generate_blade(seed = 22L, output_dir = tmp,
                 tables = c(1L, 4L, 5L, 6L, 7L, 8L, 17L, 27L),
                 max_rows = 1000L, return_data = FALSE)

  run_dir <- getOption("fplida.run_dir")
  blade_dir <- file.path(run_dir, "abs-blade")
  expect_true(dir.exists(blade_dir))

  variables_path <- system.file("blade_metadata", "variables.csv",
                                package = "fplida")
  variables <- read.csv(variables_path, stringsAsFactors = FALSE,
                        check.names = FALSE)

  expect_blade_cols <- function(table_number) {
    table_vars <- variables[variables[["Table.Number"]] == table_number, ,
                            drop = FALSE]
    product_name <- unique(table_vars[["Product.Name"]])
    expect_length(product_name, 1L)
    path <- file.path(blade_dir, paste0(product_name, ".parquet"))
    expect_true(file.exists(path))
    got <- names(as.data.frame(arrow::read_parquet(path)))
    expected <- fplida:::.blade_table_variable_names(table_vars, table_number)
    expect_true(all(expected %in% got))
  }

  expect_blade_cols(1L)
  expect_blade_cols(4L)
  expect_blade_cols(5L)
  expect_blade_cols(6L)
  expect_blade_cols(7L)
  expect_blade_cols(8L)
  expect_blade_cols(17L)
  expect_blade_cols(27L)

  key_vars_path <- system.file("blade_metadata", "keys.csv",
                               package = "fplida")
  key_vars <- read.csv(key_vars_path, stringsAsFactors = FALSE,
                       check.names = FALSE)
  for (product_name in unique(key_vars[["Product.Name"]])) {
    path <- file.path(blade_dir, paste0(product_name, ".parquet"))
    expect_true(file.exists(path))
    got <- names(as.data.frame(arrow::read_parquet(path)))
    expected <- unique(key_vars[key_vars[["Product.Name"]] == product_name,
                                "Variable.Name"])
    expect_true(all(expected %in% got))
    key_frame <- as.data.frame(arrow::read_parquet(path))
    if (product_name == "blade-key-id-to-bn-key") {
      expect_true(all(c("25", fplida:::.blade_tsid(8L)) %in%
                        unique(as.character(key_frame$tsid))))
    } else {
      expect_equal(unique(key_frame$tsid), "25")
    }
  }

  expect_equal(
    length(list.files(blade_dir, pattern = "^blade-training-")),
    0L
  )

  table1 <- as.data.frame(arrow::read_parquet(file.path(
    blade_dir, "blade-table-01-cross-sectional-indicative-data-items.parquet"
  )))
  bas <- as.data.frame(arrow::read_parquet(file.path(
    blade_dir, "blade-table-04-business-activity-statement-bas.parquet"
  )))
  payg <- as.data.frame(arrow::read_parquet(file.path(
    blade_dir, "blade-table-05-pay-as-you-go-payg.parquet"
  )))
  stp <- as.data.frame(arrow::read_parquet(file.path(
    blade_dir, "blade-table-07-single-touch-payroll-stp.parquet"
  )))
  bit <- as.data.frame(arrow::read_parquet(file.path(
    blade_dir, "blade-table-06-business-income-tax-bit.parquet"
  )))
  bcs <- as.data.frame(arrow::read_parquet(file.path(
    blade_dir, "blade-table-08-business-characteristics-survey-bcs.parquet"
  )))
  birthdate <- as.data.frame(arrow::read_parquet(file.path(
    blade_dir, "blade-table-27-business-birthdate.parquet"
  )))
  eeh <- as.data.frame(arrow::read_parquet(file.path(
    blade_dir, "blade-table-17-employee-earning-and-hours-eeh.parquet"
  )))
  id_key <- as.data.frame(arrow::read_parquet(file.path(
    blade_dir, "blade-key-id-to-bn-key.parquet"
  )))
  link <- as.data.frame(arrow::read_parquet(file.path(
    run_dir, "_system", "plida-blade-link.parquet"
  )))
  business_spine <- as.data.frame(arrow::read_parquet(file.path(
    run_dir, "_system", "business-spine.parquet"
  )))

  expect_true(all(c("bn", "x_anzsic06", "d_div06", "x_sisca08",
                    "x_sector", "id", "x_sisca06", "x_anzsic93") %in%
                    names(table1)))
  expect_true(all(grepl("^[0-9]{4}$", table1$x_anzsic93)))
  expect_false(all(table1$x_anzsic93 == "9999"))
  expect_true(all(as.character(table1$x_sisca06) %in%
                    c("1001", "1009", "3000", "4000")))
  expect_false(all(as.character(table1$x_sisca06) == "0"))
  expect_true(all(grepl("^BN[0-9]{11}$", table1$bn)))
  expect_true(all(table1$bg_id == "" | grepl("^BG[0-9]{10}$", table1$bg_id)))
  health_private <- table1$d_div06 == "Q" & table1$x_sector == 1L
  expect_true(any(health_private))
  expect_true(all(table1$x_sisca08[health_private] %in% c("1001", "1009")))
  expect_true(mean(table1$x_sisca08[health_private] == "1009") > 0.7)
  role_codes <- c("n", "0", "1", "L")
  expect_true(all(table1$x_itip %in% role_codes))
  expect_true(all(table1$x_itw %in% role_codes))
  expect_true(all(table1$x_gstp %in% role_codes))
  expect_true(any(table1$x_gst_bn == ""))
  expect_true(all(table1$x_gst_bn == "" |
                    grepl("^GST[0-9]{7}$", table1$x_gst_bn)))
  bg_links <- table(table1$bg_id[table1$bg_id != ""])
  expect_true(length(bg_links) == 0L || min(bg_links) > 1L)
  health_bg <- table(table1$bg_id[table1$d_div06 == "Q" &
                                    table1$bg_id != ""])
  expect_true(length(health_bg) == 0L || min(health_bg) > 1L)
  expect_equal(unique(table1$tsid), "26")
  expect_true(any(table1$d_div06 == "Q"))
  expect_true(all(c("spine_id", "SYNTHETIC_AEUID_ATO",
                    "synthetic_aeuid_abs", "SYNTHETIC_AEUID_DHDA",
                    "BN", "ANZSCO_CODE", "occupation_health_flag") %in%
                    names(link)))
  expect_true(any(duplicated(link$spine_id)))

  employee_link <- link[link$relationship_type %in%
                          c("employee", "employee_secondary_job"), ]
  linked_persons <- tapply(employee_link$SYNTHETIC_AEUID, employee_link$BN,
                           function(x) length(unique(x)))
  payg_linked <- linked_persons[payg$bn]
  payg_linked[is.na(payg_linked)] <- 0L
  expect_equal(unique(payg$tsid), unique(table1$tsid))
  expect_equal(unique(stp$tsid), "26")
  expect_setequal(table1$bn, business_spine$bn)
  expect_setequal(bas$bn, business_spine$bn)
  expect_setequal(payg$bn, business_spine$bn)
  expect_setequal(bit$bn, business_spine$bn)
  expect_setequal(stp$bn, business_spine$bn)
  expect_setequal(birthdate$bn, business_spine$bn)
  expect_true(all(id_key$bn %in% business_spine$bn))
  expect_gt(nrow(merge(payg, table1, by = c("bn", "tsid"))), 0L)
  expect_true(any(payg$hcnt != as.integer(payg_linked)))
  expect_true(any(stp$d_total_payees != payg$hcnt[match(stp$bn, payg$bn)]))

  expect_true("month_actioned" %in% names(bas))
  expect_true(all(bas$exports_amt <= bas$turnover))
  expect_true(all(bas$turnover >= bas$oexp))
  bas_wages <- bas$wages[match(stp$bn, bas$bn)]
  payroll_rows <- bas_wages > 0 | stp$ed_pmt_sumry_totl_grs_pmt > 0
  expect_false(any(bas_wages[payroll_rows] ==
                     stp$ed_pmt_sumry_totl_grs_pmt[payroll_rows],
                   na.rm = TRUE))

  expect_true(all(c("cn", "fn") %in% names(bit)))
  expect_true(all(c(
    "bit_comp_yyyy", "bit_ind_yyyy", "bit_part_yyyy", "bit_trust_yyyy"
  ) %in% names(bit)))
  expect_true(all(vapply(
    bit[c("bit_comp_yyyy", "bit_ind_yyyy", "bit_part_yyyy", "bit_trust_yyyy")],
    function(x) any(!is.na(x)), logical(1)
  )))
  expect_false(any(vapply(bit, function(x) {
    is.character(x) && any(grepl("_[0-9]{6}$", x))
  }, logical(1))))
  expect_true(any(bit$c_totlwage > 0, na.rm = TRUE))
  company <- bit$c_totlwage > 0
  expect_true(all(is.na(bit$i_totlwage[company])))
  expect_true(all(is.na(bit$p_totlwage[company])))
  expect_true(all(is.na(bit$t_totlwage[company])))
  bit_numeric_checks <- intersect(
    c("t_ausn_fcrs_frm_nzc_nobnfcry_amt",
      "c_div_frnkg_acnt_opng_bal_amt",
      "c_div_frnkg_acnt_closg_bal_amt"),
    names(bit)
  )
  expect_true(all(vapply(bit[bit_numeric_checks], is.numeric, logical(1))))

  expect_type(stp$ed_pmt_sumry_totl_grs_pmt, "double")
  expect_type(stp$ed_sg_emplr_cntrbtn, "double")
  expect_equal(stp$ed_sg_emplr_cntrbtn,
               round(stp$ed_pmt_sumry_totl_grs_pmt * 0.115, 2))
  expect_false(any(vapply(stp, function(x) {
    is.character(x) && any(grepl("_[0-9]{6}$", x))
  }, logical(1))))

  expect_true(all(bcs$d_gsnewy %in% c(0L, 1L, 7777777L, 88888888L,
                                      999999999L)))
  expect_true(any(bcs$d_gsnewy == 0L))
  expect_true(any(bcs$d_gsnewy == 1L))
  expect_true(any(bcs$d_gsnewy %in% c(7777777L, 88888888L, 999999999L)))
  expect_type(bcs$incothin_bcs, "double")
  expect_type(bcs$inctotal_bcs, "double")
  bcs_count_checks <- intersect(
    c("locs_bcs", "empprop_bcs", "empsaldr_bcs", "empoth_bcs",
      "emptotal_bcs", "empft_bcs", "casuals_bcs", "perscomm_bcs",
      "persceas_bcs", "busownyr_bcs", "busopyr_bcs"),
    names(bcs)
  )
  expect_true(all(vapply(bcs[bcs_count_checks], is.numeric, logical(1))))
  expect_false(any(vapply(bcs[bcs_count_checks], function(x) {
    any(x %in% c(88888888L, 999999999L), na.rm = TRUE)
  }, logical(1))))
  expect_true(all(bcs$locs_bcs >= 1L, na.rm = TRUE))
  expect_false(any(vapply(bcs, function(x) {
    is.character(x) && any(grepl("_[0-9]{6}$", x))
  }, logical(1))))
  expect_gt(nrow(merge(bcs, id_key, by = c("id", "tsid"))), 0L)

  expect_true(any(is.na(birthdate$birth_date)))
  expect_true(any(grepl("^1993", birthdate$birth_date)))
  expect_true(any(grepl("^2001", birthdate$birth_date)))

  expect_true(all(c("id", "eid_eeh", "anzsco22_4_eeh",
                    "state_eeh") %in% names(eeh)))
  expect_equal(unique(eeh$tsid), "23")
  expect_true(all(nchar(eeh$eid_eeh) == 15L))
  expect_false(any(eeh$eid_eeh %in% link$spine_id))
  health_bns <- unique(table1$bn[table1$d_div06 == "Q"])
  health_ids <- unique(id_key$id[id_key$bn %in% health_bns])
  expect_true(any(eeh$id %in% health_ids &
                    substr(eeh$anzsco22_4_eeh, 1L, 2L) %in%
                    c("25", "41", "42")))
})

test_that("build_fplida treats blade as a central post-core product", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_build_blade_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- build_fplida(n = 120L, seed = 23L, products = "blade",
                         output_dir = tmp, k_slices = 2L,
                         years = 2023L:2024L)

  expect_true(all(c("spine", "core", "blade") %in% result$products))
  expect_true(!is.null(result$stage_timings$blade))
  expect_equal(length(result$worker_results), 0L)
  expect_true(file.exists(file.path(result$canonical_run_dir, "_system",
                                    "business-spine.parquet")))
  expect_true(file.exists(file.path(result$canonical_run_dir, "_system",
                                    "plida-blade-link.parquet")))
  expect_true(dir.exists(file.path(result$canonical_run_dir, "abs-blade")))
  blade_files <- list.files(file.path(result$canonical_run_dir, "abs-blade"),
                            pattern = "\\.parquet$", full.names = TRUE)
  placeholder_counts <- vapply(blade_files, function(path) {
    frame <- as.data.frame(arrow::read_parquet(path))
    sum(vapply(frame, function(x) {
      is.character(x) && any(grepl("_[0-9]{6}$", x))
    }, logical(1)))
  }, integer(1))
  expect_equal(sum(placeholder_counts), 0L)
})
