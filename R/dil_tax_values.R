# Source-backed categorical values for ATO administrative products.
#
# The code frames in this file come from public ATO forms and software
# specifications. Internal ATO processing fields return NULL so that the
# release audit continues to report them as unresolved.

.dil_tax_key <- function(spine_rows, seed, name) {
  .dil_numeric_key(
    spine_rows, seed,
    .stable_name_seed(paste("ATO tax value", toupper(name), sep = "|"))
  )
}

.dil_tax_pick <- function(key, values, weights = rep(1, length(values))) {
  stopifnot(length(values) > 0L, length(values) == length(weights))
  weights <- as.numeric(weights)
  weights[!is.finite(weights) | weights < 0] <- 0
  if (!sum(weights)) weights[] <- 1
  cumulative <- cumsum(weights) / sum(weights)
  draw <- (as.numeric(key) %% 10000) / 10000
  index <- vapply(
    draw, function(value) which(value < cumulative)[[1L]], integer(1)
  )
  values[index]
}

.dil_tax_pit_lodgement_weights <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    path <- system.file(
      "extdata", "codeframes", "ato-pit-lodgement-channel-weights.tsv",
      package = "fplida"
    )
    if (!nzchar(path)) {
      path <- file.path(
        "inst", "extdata", "codeframes",
        "ato-pit-lodgement-channel-weights.tsv"
      )
    }
    values <- utils::read.delim(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    count_columns <- c("AGNT_ELS", "ETAX", "SP_MYTAX", "SP_PPR")
    stopifnot(
      identical(values$end_year, 2010L:2024L),
      all(c(
        "financial_year", "end_year", count_columns, "source_url"
      ) %in% names(values)),
      all(vapply(values[count_columns], is.numeric, logical(1))),
      all(as.matrix(values[count_columns]) >= 0),
      all(rowSums(values[count_columns]) > 0),
      all(nzchar(values$source_url))
    )
    cache <<- values
    cache
  }
})

.dil_tax_pit_lodgement_source <- function(spine_rows, seed, period) {
  weights <- .dil_tax_pit_lodgement_weights()
  end_year <- suppressWarnings(as.integer(period$end_year))
  if (!length(end_year) || is.na(end_year)) end_year <- max(weights$end_year)
  end_year <- min(max(end_year, min(weights$end_year)), max(weights$end_year))
  year_row <- weights[match(end_year, weights$end_year), , drop = FALSE]
  channels <- c("AGNT_ELS", "ETAX", "SP_MYTAX", "SP_PPR")
  counts <- as.numeric(year_row[1L, channels])
  cumulative <- cumsum(counts) / sum(counts)

  # Use one stable person key for every year. The fine-resolution modulus keeps
  # the very small 2015-16 e-tax share representable.
  key <- .dil_tax_key(spine_rows, seed, "PIT lodgement channel")
  modulus <- 2147483647
  mixed <- ((as.numeric(key) %% modulus) * 48271 + 1) %% modulus
  draw <- (mixed + 0.5) / modulus
  index <- vapply(
    draw, function(value) which(value < cumulative)[[1L]], integer(1)
  )
  channels[index]
}

.dil_tax_age <- function(spine_rows, period) {
  n <- nrow(spine_rows)
  birth_year <- if ("birth_year" %in% names(spine_rows)) {
    suppressWarnings(as.integer(spine_rows$birth_year))
  } else if ("birth_date" %in% names(spine_rows)) {
    suppressWarnings(as.integer(format(as.Date(spine_rows$birth_date), "%Y")))
  } else {
    rep(NA_integer_, n)
  }
  as.integer(period$end_year) - birth_year
}

.dil_tax_amount <- function(source_frame, aliases) {
  value <- .dil_source_alias(source_frame, aliases)
  if (is.null(value)) return(NULL)
  suppressWarnings(as.numeric(as.character(value)))
}

.dil_tax_applicable <- function(source_frame, aliases, key,
                                fallback_percent = 10L) {
  amount <- .dil_tax_amount(source_frame, aliases)
  if (!is.null(amount)) return(is.finite(amount) & amount != 0)
  key %% 100L < as.integer(fallback_percent)
}

.dil_tax_extract_reference <- function(period, n) {
  start_year <- suppressWarnings(as.integer(period$start_year))
  end_year <- suppressWarnings(as.integer(period$end_year))
  if (!length(start_year) || is.na(start_year)) start_year <- end_year - 1L
  if (!length(end_year) || is.na(end_year)) end_year <- start_year + 1L
  rep(sprintf("FY%04d-%02d", start_year, end_year %% 100L), n)
}

# Capital gains tax schedule 2024 and capital gains tax schedule guide:
# https://www.ato.gov.au/individuals-and-families/your-tax-return/instructions-to-complete-your-tax-return/mytax-instructions/2024/income/australian-income-or-losses-from-investments-or-property/capital-gains-tax-schedule
.dil_tax_cgt_source_value <- function(name, source_frame, spine_rows, seed) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_tax_key(spine_rows, seed, paste("CGT", upper, sep = "|"))

  if (upper == "CG_ERNT_ARNGMTS_PRTY_CD") {
    return(ifelse(key %% 20L == 0L, "Y", "N"))
  }
  if (upper == "ENTITY_TYPE") {
    # project_cgt() emits person records only. I is the individual entity code
    # used by the ATO capital-gains schedule.
    return(rep("I", n))
  }
  if (upper == "CG_EXMPTN_15_YR_FR_SB_CD") {
    applies <- .dil_tax_applicable(
      source_frame,
      c("SB_CNCSNS_APLD_TOTL_AMT", "SMALL_BUSINESS_CONCESSIONS_AMOUNT"),
      key, 8L
    )
    value <- rep(NA_character_, n)
    value[applies] <- .dil_tax_pick(
      key[applies], c("S", "U", "R", "G", "O"), c(25, 10, 40, 15, 10)
    )
    return(value)
  }
  if (upper == "K_SCRPT_RLOVR_CD_A") {
    context_key <- .dil_tax_key(spine_rows, seed, "CGT scrip rollover")
    return(ifelse(context_key %% 25L == 0L, "Y", "N"))
  }
  if (upper == "L_SCRPT_ACQ_ENT_CD_E") {
    context_key <- .dil_tax_key(spine_rows, seed, "CGT scrip rollover")
    return(ifelse(context_key %% 25L == 0L, "Y", "N"))
  }
  if (upper == "L_SCRPT_ACQ_ENT_JNT_RLOVR_L") {
    context_key <- .dil_tax_key(spine_rows, seed, "CGT scrip rollover")
    return(ifelse(context_key %% 50L == 0L, "Y", "N"))
  }
  if (upper %in% c("M_EMPLYEE_SHARE_SCHM_N", "N_DIV149_O")) {
    # These fields are for company schedules. The synthetic CGT source is a
    # person schedule, so missing is the structurally correct value.
    return(rep(NA_character_, n))
  }
  NULL
}

# Self-managed superannuation fund annual return 2023 instructions:
# https://www.ato.gov.au/api/public/content/0-7986dcf2-8213-49ee-8403-b54dccb52ad8
.dil_tax_smsf_source_value <- function(name, source_frame, spine_rows, seed,
                                       period) {
  upper <- toupper(name)
  key <- .dil_tax_key(spine_rows, seed, paste("SMSF", upper, sep = "|"))
  age <- .dil_tax_age(spine_rows, period)

  if (upper == "FND_BNFT_STRCTR_CD") {
    # The ATO instructions state that most SMSFs use accumulation code A.
    return(.dil_tax_pick(key, c("A", "D", "E"), c(92, 5, 3)))
  }
  if (upper == "CRNT_PNSN_INC_EXMT_CAL_MTHD_CD") {
    applies <- .dil_tax_applicable(
      source_frame,
      c("CRNT_PNSN_EXMT_INCM_AMT", "CRNT_PNSN_INCM_EXMT_AMT"),
      key, 35L
    )
    value <- rep(NA_character_, length(key))
    value[applies] <- .dil_tax_pick(key[applies], c("B", "C"), c(35, 65))
    return(value)
  }
  if (upper == "GRS_TRST_DSTRBTNS_CD") {
    applies <- .dil_tax_applicable(
      source_frame, "GRS_TRST_DSTRBTNS_AMT", key, 25L
    )
    value <- rep(NA_character_, length(key))
    value[applies] <- .dil_tax_pick(
      key[applies],
      c("D", "E", "F", "H", "S", "T", "I", "M", "U", "P", "Q"),
      c(7, 4, 6, 8, 18, 8, 13, 10, 12, 8, 6)
    )
    return(value)
  }
  if (upper == "OI_TYP_CD") {
    applies <- .dil_tax_applicable(source_frame, "INCM_OTHR_AMT", key, 25L)
    value <- rep(NA_character_, length(key))
    value[applies] <- .dil_tax_pick(
      key[applies], c("B", "C", "F", "R", "T", "W", "O"),
      c(20, 12, 10, 14, 12, 7, 25)
    )
    return(value)
  }
  if (upper %in% c(
    "DDCTNS_OTHR_DDCTN_TYP_CD", "NON_DDCTBL_EXPNS_OTHR_CD"
  )) {
    aliases <- if (upper == "DDCTNS_OTHR_DDCTN_TYP_CD") {
      "OD_AMT"
    } else {
      "NON_DDCTBL_EXPNS_OTHR_AMT"
    }
    applies <- .dil_tax_applicable(source_frame, aliases, key, 25L)
    value <- rep(NA_character_, length(key))
    value[applies] <- .dil_tax_pick(
      key[applies], c("A", "B", "C", "E", "F", "I", "N", "R", "T", "O"),
      c(7, 9, 8, 6, 9, 8, 7, 12, 9, 25)
    )
    return(value)
  }
  if (upper == "LSP_CD") {
    applies <- .dil_tax_applicable(source_frame, "LSP_AMT", key, 12L)
    value <- rep(NA_character_, length(key))
    ordinary <- applies & key %% 10L < 8L
    value[ordinary] <- ifelse(
      !is.na(age[ordinary]) & age[ordinary] >= 60L, "A", "B"
    )
    exceptional <- applies & !ordinary
    value[exceptional] <- .dil_tax_pick(
      key[exceptional], c("C", "D", "E", "F", "G"), c(25, 20, 10, 35, 10)
    )
    return(value)
  }
  if (upper == "INCM_STRM_PMT_CD") {
    applies <- .dil_tax_applicable(
      source_frame, "INCM_STRM_PMT_AMT", key, 20L
    )
    value <- rep(NA_character_, length(key))
    ordinary <- applies & key %% 10L < 7L
    value[ordinary] <- ifelse(
      !is.na(age[ordinary]) & age[ordinary] >= 60L, "M", "N"
    )
    exceptional <- applies & !ordinary
    value[exceptional] <- .dil_tax_pick(
      key[exceptional], c("O", "P", "Q", "R"), c(45, 20, 10, 25)
    )
    return(value)
  }
  if (upper == "MBR_RCRD_CD") {
    # The annual-return form labels section F as current members and section G
    # as former members. The single-letter values retain that form structure.
    return(ifelse(key %% 20L == 0L, "G", "F"))
  }
  NULL
}

# Member contributions statement electronic reporting specification 10.1.3:
# https://softwaredevelopers.ato.gov.au/MCSspecification
.dil_tax_mcs_context <- function(source_frame, spine_rows, seed, period) {
  key <- .dil_tax_key(spine_rows, seed, "MCS account context")
  age <- .dil_tax_age(spine_rows, period)
  pension <- !is.na(age) & age >= 60L & key %% 100L < 45L
  defined_benefit <- !is.na(age) & age >= 55L & key %% 100L >= 45L &
    key %% 100L < 50L
  phase <- ifelse(defined_benefit, "B", ifelse(pension, "P", "A"))
  deceased <- .dil_source_alias(source_frame, "MBR_IDV_DCSD_IND")
  if (is.null(deceased)) {
    deceased <- if ("year_of_death" %in% names(spine_rows)) {
      !is.na(spine_rows$year_of_death) &
        as.integer(spine_rows$year_of_death) <= as.integer(period$end_year)
    } else {
      rep(FALSE, nrow(spine_rows))
    }
  }
  deceased <- if (is.logical(deceased)) {
    deceased
  } else {
    toupper(as.character(deceased)) %in% c("Y", "1", "TRUE", "T")
  }
  deceased[is.na(deceased)] <- FALSE
  closed <- deceased | phase %in% c("P", "B") | key %% 100L >= 96L
  modern <- as.integer(period$end_year) >= 2013L
  status <- if (modern) {
    ifelse(closed, "C", ifelse(phase == "A" & key %% 20L == 0L, "L", "O"))
  } else {
    ifelse(closed, "C", "A")
  }
  list(key = key, age = age, phase = phase, status = status)
}

.dil_tax_mcs_source_value <- function(name, source_frame, spine_rows, seed,
                                      period) {
  upper <- toupper(name)
  context <- .dil_tax_mcs_context(
    source_frame, spine_rows, seed, period
  )
  key <- .dil_tax_key(spine_rows, seed, paste("MCS", upper, sep = "|"))

  if (upper == "ACNT_PHS_CD") {
    if (as.integer(period$end_year) < 2013L) {
      return(rep(NA_character_, nrow(spine_rows)))
    }
    return(context$phase)
  }
  if (upper == "SF_MBRSHP_ACNT_STS_CD") return(context$status)
  if (upper == "PRVDR_TYP_CD") {
    if (as.integer(period$end_year) < 2013L) {
      return(rep(NA_character_, nrow(spine_rows)))
    }
    return(.dil_tax_pick(
      key, c("P", "N", "S", "X", "D", "E", "A", "C", "R"),
      c(38, 20, 25, 2, 3, 2, 4, 3, 3)
    ))
  }
  if (upper == "SUPLR_PRVDR_RLTNSHP") {
    return(.dil_tax_pick(
      key,
      c("A", "C", "F", "I", "L", "R", "S", "T", "U", "W", "X", NA),
      c(29, 7, 5, 3, 4, 3, 3, 25, 4, 4, 5, 8)
    ))
  }
  if (upper == "LDGMT_TYP_CD") {
    if (as.integer(period$end_year) < 2008L) {
      return(rep(NA_character_, nrow(spine_rows)))
    }
    return(ifelse(key %% 20L == 0L, "A", "O"))
  }
  NULL
}

# The address-status letters match the existing ATO client-register generator.
# Public ABS documentation confirms residential and postal address use but does
# not publish the internal POSTALTYPE code frame; that field remains unresolved.
.dil_tax_ato_cr_source_value <- function(name, source_frame, spine_rows,
                                         seed) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_tax_key(spine_rows, seed, paste("ATO_CR", upper, sep = "|"))

  if (upper == "ADDR_RELIABLE_STS") {
    return(ifelse(key %% 20L == 0L, "U", "R"))
  }
  if (upper == "ADDR_STS_CD") {
    return(ifelse(key %% 25L == 0L, "I", "A"))
  }
  if (upper %in% c("ADDR_TYP", "ADR_TYP")) {
    type_key <- .dil_tax_key(spine_rows, seed, "ATO_CR address type")
    return(ifelse(type_key %% 10L == 0L, "P", "R"))
  }
  if (upper == "CLNT_STS_CD") {
    return(ifelse(key %% 20L == 0L, "I", "A"))
  }
  if (upper == "DOB_MONYYYY") {
    birth_year <- if ("birth_year" %in% names(spine_rows)) {
      suppressWarnings(as.integer(spine_rows$birth_year))
    } else {
      rep(NA_integer_, n)
    }
    month <- 1L + as.integer(key %% 12L)
    label <- month.abb[month]
    return(ifelse(is.na(birth_year), NA_character_, paste0(label, birth_year)))
  }
  if (grepl("^IN_MAY_[0-9]{2}$", upper)) {
    return(ifelse(key %% 100L < 95L, 1L, 0L))
  }
  # POSTALTYPE is an internal address-repair classification. Its code frame
  # is not published and is not equivalent to residential/postal address type.
  NULL
}

# Individual tax return instructions 2022 and business and professional items
# schedule 2022. Field-level rules below retain missing for inapplicable items.
# https://www.ato.gov.au/api/public/content/a9a181ef-2223-491e-9e44-2de03b272a91_Individual_tax_return_instructions_2022_pdf
# https://www.ato.gov.au/forms-and-instructions/business-and-professional-items-schedule-2022-and-instructions
.dil_tax_health_insurer_ids <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    path <- system.file(
      "extdata", "codeframes", "ato-health-insurer-ids.tsv",
      package = "fplida"
    )
    if (!nzchar(path)) {
      path <- file.path(
        "inst", "extdata", "codeframes", "ato-health-insurer-ids.tsv"
      )
    }
    values <- utils::read.delim(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    )
    stopifnot(
      nrow(values) >= 30L,
      all(c(
        "code", "insurer", "membership_type", "source_url",
        "source_retrieved_date", "scope_note"
      ) %in% names(values)),
      all(grepl("^[A-Z]{3}$", values$code)),
      !anyDuplicated(values$code)
    )
    cache <<- values
    cache
  }
})

.dil_tax_business_context <- function(source_frame, spine_rows, seed) {
  n <- nrow(spine_rows)
  key <- .dil_tax_key(spine_rows, seed, "PIT individual business context")
  activity_count <- .dil_tax_amount(
    source_frame, c("BUS_ACTY_CNT", "BUSINESS_ACTIVITY_COUNT")
  )
  if (!is.null(activity_count)) {
    observed <- is.finite(activity_count)
    business <- observed & activity_count > 0
    business[!observed] <- key[!observed] %% 101L < 12L
    return(business)
  }

  amount_names <- c(
    "OTHR_BUS_INCM_NPP_AMT", "OTHR_BUS_INCM_PP_AMT",
    "BUS_LSS_ACTY1_NET_LSS_AMT", "BUS_LSS_ACTY2_NET_LSS_AMT",
    "BUS_LSS_ACTY3_NET_LSS_AMT", "CLOSG_STK_TOTL_AMT",
    "DPRCTN_EXPNS_AMT", "MTR_VHCL_BUS_EXPNSS_TOTL_AMT",
    "SLRY_AND_WG_EXPNSS_TOTL_AMT"
  )
  observed <- business <- rep(FALSE, n)
  for (amount_name in amount_names) {
    amount <- .dil_source_column(source_frame, amount_name)
    if (is.null(amount)) next
    amount <- suppressWarnings(as.numeric(as.character(amount)))
    finite <- is.finite(amount)
    observed[finite] <- TRUE
    business[finite] <- business[finite] | amount[finite] != 0
  }
  business[!observed] <- key[!observed] %% 101L < 12L
  business
}

.dil_tax_asset_opt_out_code <- function(
    upper, source_frame, spine_rows, seed) {
  n <- nrow(spine_rows)
  key <- .dil_tax_key(spine_rows, seed, paste("PIT", upper, sep = "|"))
  business <- .dil_tax_business_context(source_frame, spine_rows, seed)
  aliases <- if (upper == "BBI_ELGBL_AST_BBI_CD") {
    c("BBI_ELGBL_AST_OPT_OUT_NUM", "BBI_ELGBL_AST_OPT_OUT_VAL_AMT")
  } else {
    c("TFE_ELGBL_AST_OPT_OUT_NUM", "TFE_ELGBL_AST_OPT_OUT_VAL_AMT")
  }
  opt_out <- .dil_tax_amount(source_frame, aliases)
  if (is.null(opt_out)) opt_out <- rep(NA_real_, n)
  known_opt_out <- is.finite(opt_out)
  observed_opt_out <- known_opt_out & opt_out > 0
  eligible <- observed_opt_out | (business & key %% 103L < 35L)
  synthetic_opt_out <- eligible & !known_opt_out & key %% 107L < 12L
  value <- rep(NA_character_, n)
  choosing <- observed_opt_out | synthetic_opt_out
  value[eligible & !choosing] <- "15"
  value[choosing] <- ifelse(key[choosing] %% 8L == 0L, "10", "5")
  value
}

.dil_tax_health_fund_value <- function(
    upper, spine_rows, seed, period) {
  slot <- if (upper == "HLTH_FND_CD") {
    1L
  } else {
    suppressWarnings(as.integer(sub("^HLTH_FND_CD_", "", upper)))
  }
  if (is.na(slot) || !slot %in% 1:4) return(NULL)

  n <- nrow(spine_rows)
  age <- .dil_tax_age(spine_rows, period)
  income <- if ("baseline_income" %in% names(spine_rows)) {
    suppressWarnings(as.numeric(spine_rows$baseline_income))
  } else {
    rep(NA_real_, n)
  }
  probability <- rep(0.46, n)
  probability[!is.na(age) & age < 18L] <- 0.12
  probability[!is.na(age) & age >= 18L & age < 30L] <- 0.38
  probability[!is.na(age) & age >= 30L & age < 65L] <- 0.52
  probability[!is.na(age) & age >= 65L] <- 0.48
  probability[is.finite(income) & income < 30000] <-
    probability[is.finite(income) & income < 30000] - 0.12
  probability[is.finite(income) & income >= 60000] <-
    probability[is.finite(income) & income >= 60000] + 0.10
  probability[is.finite(income) & income >= 100000] <-
    probability[is.finite(income) & income >= 100000] + 0.08
  probability <- pmin(pmax(probability, 0.08), 0.78)

  coverage_key <- .dil_tax_key(
    spine_rows, seed, "PIT private health coverage"
  )
  covered <- ((coverage_key %% 10009) + 0.5) / 10009 < probability
  count_key <- .dil_tax_key(
    spine_rows, seed, "PIT private health policy count"
  )
  # Use a prime modulus distinct from the coverage draw. The package's base
  # key is linear, so reusing one modulus would correlate coverage and count.
  count_draw <- ((count_key %% 10007) + 0.5) / 10007
  policy_count <- rep(0L, n)
  policy_count[covered] <- ifelse(
    count_draw[covered] < 0.925, 1L,
    ifelse(
      count_draw[covered] < 0.985, 2L,
      ifelse(count_draw[covered] < 0.997, 3L, 4L)
    )
  )

  insurers <- .dil_tax_health_insurer_ids()
  weights <- ifelse(insurers$membership_type == "Open", 5, 1)
  primary_key <- .dil_tax_key(
    spine_rows, seed, "PIT private health primary fund"
  )
  primary <- .dil_tax_pick(primary_key %% 10037, insurers$code, weights)
  primary_index <- match(primary, insurers$code)
  fund_index <- 1L + (
    primary_index - 1L + (slot - 1L) * 7L
  ) %% nrow(insurers)
  value <- insurers$code[fund_index]
  value[policy_count < slot] <- NA_character_
  value
}

.dil_tax_spouse_context <- function(spine_rows, seed, period) {
  key <- .dil_tax_key(spine_rows, seed, "PIT spouse context")
  age <- .dil_tax_age(spine_rows, period)
  eligible <- !is.na(age) & age >= 18L
  draw <- key %% 100L
  full_year <- eligible & draw < 48L
  part_year <- eligible & draw >= 48L & draw < 58L
  days_in_year <- as.integer(
    as.Date(sprintf("%04d-06-30", as.integer(period$end_year))) -
      as.Date(sprintf("%04d-07-01", as.integer(period$end_year) - 1L)) + 1L
  )
  days <- rep(NA_integer_, nrow(spine_rows))
  days[part_year] <- 1L + as.integer(key[part_year] %% (days_in_year - 1L))
  list(
    key = key, age = age, full_year = full_year, part_year = part_year,
    indicator = ifelse(full_year, "Y", ifelse(part_year, "N", NA_character_)),
    days = days
  )
}

.dil_tax_pit_source_value <- function(name, source_frame, spine_rows, seed,
                                      period) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_tax_key(spine_rows, seed, paste("PIT", upper, sep = "|"))
  age <- .dil_tax_age(spine_rows, period)

  if (upper == "EXTRACT_REF") {
    return(.dil_tax_extract_reference(period, n))
  }
  if (upper == "RECORD_ID") {
    return(.dil_character_id(
      "ITR", spine_rows, seed, .stable_name_seed("PIT record id"), 12L
    ))
  }
  if (upper == "AMDT_CD") {
    return(ifelse(key %% 20L == 0L, "A", "O"))
  }
  if (upper == "LODGMENT_SOURCE") {
    return(.dil_tax_pit_lodgement_source(spine_rows, seed, period))
  }
  if (upper %in% c(
    "BBI_ELGBL_AST_BBI_CD", "TFE_ELGBL_AST_OPT_OUT_CD"
  )) {
    return(.dil_tax_asset_opt_out_code(
      upper, source_frame, spine_rows, seed
    ))
  }
  if (upper == "BUSINESS_MARKET_SEGMENT") {
    business <- .dil_tax_business_context(source_frame, spine_rows, seed)
    value <- rep("INB", n)
    value[business] <- .dil_tax_pick(
      key[business], c("MIC", "SME"), c(95, 5)
    )
    return(value)
  }
  if (grepl("^HLTH_FND_CD(?:_[1-4])?$", upper)) {
    return(.dil_tax_health_fund_value(upper, spine_rows, seed, period))
  }
  if (upper %in% c("LSPA_TYP", "LSPS_AMTA_CD")) {
    key <- .dil_tax_key(spine_rows, seed, "PIT lump sum A type")
    applies <- .dil_tax_applicable(
      source_frame,
      c("LSPA_AMT", "LUMP_SUM_A", "LSPS_AMT_A_TOTL_CALCD_AMT"),
      key, 5L
    )
    value <- rep(NA_character_, n)
    value[applies] <- ifelse(key[applies] %% 5L == 0L, "T", "R")
    return(value)
  }
  if (upper %in% c("IDV_OCPTN_CD", "SUB_OCPTN_GRP_CD")) {
    occupation <- .dil_source_alias(
      source_frame, c("SUB_OCPTN_GRP_CD", "IDV_OCPTN_CD")
    )
    if (is.null(occupation) && "anzsco_code" %in% names(spine_rows)) {
      occupation <- spine_rows$anzsco_code
    }
    if (is.null(occupation)) return(NULL)
    occupation <- suppressWarnings(as.integer(as.character(occupation)))
    return(ifelse(is.na(occupation), NA_character_, sprintf("%06d", occupation)))
  }
  if (upper == "OCPTN_GRP_CD") {
    occupation <- .dil_source_alias(
      source_frame, c("SUB_OCPTN_GRP_CD", "IDV_OCPTN_CD", "OCPTN_GRP_CD")
    )
    if (is.null(occupation) && "anzsco_code" %in% names(spine_rows)) {
      occupation <- spine_rows$anzsco_code
    }
    if (is.null(occupation)) return(NULL)
    occupation <- suppressWarnings(as.integer(
      gsub("[^0-9]", "", as.character(occupation))
    ))
    six_digit <- ifelse(
      is.na(occupation), NA_character_, sprintf("%06d", occupation)
    )
    return(substr(six_digit, 1L, 4L))
  }
  if (upper %in% c(
    "HAD_SPS_CRNT_FY_DAYS", "HAD_SPS_CRNT_FY_FULL_PERD_CD"
  )) {
    spouse <- .dil_tax_spouse_context(spine_rows, seed, period)
    if (upper == "HAD_SPS_CRNT_FY_DAYS") return(spouse$days)
    return(spouse$indicator)
  }
  if (upper == "MRTL_STS_CD") {
    spouse <- .dil_tax_spouse_context(spine_rows, seed, period)
    return(ifelse(
      is.na(age) | age < 18L, "S",
      ifelse(spouse$full_year | spouse$part_year, "M",
             ifelse(key %% 10L == 0L, "F", "S"))
    ))
  }
  if (upper == "WRK_RLTD_CAR_EXPNSS_CLM_TYP_CD") {
    applies <- .dil_tax_applicable(
      source_frame, "WRK_RLTD_CAR_EXPNSS_AMT", key, 35L
    )
    value <- rep(NA_character_, n)
    value[applies] <- ifelse(key[applies] %% 4L == 0L, "B", "S")
    return(value)
  }
  if (upper == "WRK_RLTD_CLTHG_EXPNSS_TYP_CD") {
    applies <- .dil_tax_applicable(
      source_frame, "WRK_RLTD_CLTHG_EXPNSS_AMT", key, 30L
    )
    value <- rep(NA_character_, n)
    value[applies] <- .dil_tax_pick(
      key[applies], c("C", "N", "S", "P"), c(30, 35, 25, 10)
    )
    return(value)
  }
  if (upper == "WRK_RLTD_SELF_EDUCN_TYP_CD") {
    applies <- .dil_tax_applicable(
      source_frame, "WRK_RLTD_SELF_EDUCN_EXPNSS_AMT", key, 8L
    )
    value <- rep(NA_character_, n)
    value[applies] <- ifelse(key[applies] %% 5L == 0L, "I", "K")
    return(value)
  }
  if (upper == "ETP_TYP_CD") {
    amount <- .dil_tax_amount(
      source_frame, c("ETPS_OTHRTHN_EXCSVCMPNT_AMT", "ETPS_EXCSVCMPNT_AMT")
    )
    applies <- if (is.null(amount)) key %% 100L < 4L else {
      is.finite(amount) & amount != 0
    }
    value <- rep(NA_character_, n)
    value[applies] <- .dil_tax_pick(
      key[applies], c("R", "O", "S", "P", "D", "B", "N"),
      c(25, 55, 4, 4, 4, 4, 4)
    )
    return(value)
  }
  if (upper %in% c(
    "ASSBL_GOVT_INDY_PMTS_NPP_CD", "ASSBL_GOVT_INDY_PMTS_PP_CD"
  )) {
    suffix <- if (grepl("NPP", upper, fixed = TRUE)) "NPP" else "PP"
    applies <- .dil_tax_applicable(
      source_frame, paste0("ASSBL_GOVT_INDY_PMTS_", suffix, "_AMT"),
      key, 3L
    )
    value <- rep(NA_character_, n)
    value[applies & key %% 4L == 0L] <- "D"
    return(value)
  }
  if (grepl("^BUS_LSS_ACTY[1-3]_TYP_CD$", upper)) {
    activity <- sub("^BUS_LSS_ACTY([1-3])_TYP_CD$", "\\1", upper)
    applies <- .dil_tax_applicable(
      source_frame, paste0("BUS_LSS_ACTY", activity, "_NET_LSS_AMT"),
      key, 8L
    )
    value <- rep(NA_character_, n)
    value[applies] <- as.character(.dil_tax_pick(
      key[applies], 1:8, c(8, 6, 7, 8, 8, 8, 10, 45)
    ))
    return(value)
  }
  if (grepl("^BUS_LSSACTY[1-3]_PSHPORSOLETRDR_CD$", upper)) {
    activity <- sub(
      "^BUS_LSSACTY([1-3])_PSHPORSOLETRDR_CD$", "\\1", upper
    )
    applies <- .dil_tax_applicable(
      source_frame, paste0("BUS_LSS_ACTY", activity, "_NET_LSS_AMT"),
      key, 8L
    )
    value <- rep(NA_character_, n)
    value[applies] <- ifelse(key[applies] %% 4L == 0L, "P", "S")
    return(value)
  }
  if (upper == "CESD_OR_CMNCD_BUS_STS_CD") {
    value <- rep(NA_character_, n)
    value[key %% 100L < 3L] <- "C1"
    value[key %% 100L >= 3L & key %% 100L < 6L] <- "C2"
    return(value)
  }
  if (upper == "CLOSG_STK_ACTN_CD") {
    applies <- .dil_tax_applicable(
      source_frame, "CLOSG_STK_TOTL_AMT", key, 10L
    )
    value <- rep(NA_character_, n)
    value[applies] <- .dil_tax_pick(
      key[applies], c("C", "M", "R"), c(75, 20, 5)
    )
    return(value)
  }
  if (upper == "DPRCTN_EXPNS_CD") {
    applies <- .dil_tax_applicable(
      source_frame, c("DPRCTN_EXPNS_AMT", "DEPRECIATION_EXPENSES_AMOUNT"),
      key, 12L
    )
    value <- rep(NA_character_, n)
    value[applies] <- ifelse(key[applies] %% 5L == 0L, "M", "S")
    return(value)
  }
  if (upper == "MTR_VHCL_BUS_EXPNSS_TYP_CD") {
    applies <- .dil_tax_applicable(
      source_frame, "MTR_VHCL_BUS_EXPNSS_TOTL_AMT", key, 15L
    )
    value <- rep(NA_character_, n)
    value[applies] <- .dil_tax_pick(
      key[applies], c("S", "B", "N"), c(55, 35, 10)
    )
    return(value)
  }
  if (upper == "SLRY_AND_WG_EXPNSS_TOTL_CD") {
    applies <- .dil_tax_applicable(
      source_frame, "SLRY_AND_WG_EXPNSS_TOTL_AMT", key, 12L
    )
    value <- rep(NA_character_, n)
    value[applies] <- .dil_tax_pick(
      key[applies], c("C", "A", "B", "O"), c(55, 18, 12, 15)
    )
    return(value)
  }
  if (upper %in% c("OTHR_BUS_INCM_NPP_CD", "OTHR_BUS_INCM_PP_CD")) {
    suffix <- if (grepl("NPP", upper, fixed = TRUE)) "NPP" else "PP"
    amount <- .dil_tax_amount(
      source_frame, paste0("OTHR_BUS_INCM_", suffix, "_AMT")
    )
    if (is.null(amount)) return(rep(NA_character_, n))
    return(ifelse(is.finite(amount) & amount < 0, "L", NA_character_))
  }
  if (upper %in% c(
    "NPP_OTHR_DDCTNS_BUS_LSS_CD", "PP_OTHR_DDCTNS_DSTBN_BUSLSS_CD"
  )) {
    prefix <- if (startsWith(upper, "NPP")) "NONPP" else "PP"
    applies <- .dil_tax_applicable(
      source_frame, paste0(prefix, "_OTHR_DDCTNS_DSTBN_AMT"), key, 3L
    )
    value <- rep(NA_character_, n)
    value[applies] <- "D"
    return(value)
  }
  if (upper == "OTHR_RFNDBL_TOS_CD") {
    applies <- .dil_tax_applicable(
      source_frame, c("AVLBL_OTHR_RFNDBL_TOS_AMT", "OTHR_RFNDBL_TOS_AMT"),
      key, 3L
    )
    value <- rep(NA_character_, n)
    value[applies] <- .dil_tax_pick(
      key[applies], c("S", "E", "M"), c(65, 30, 5)
    )
    return(value)
  }
  if (upper == "MDCRE_FULLLVY_EXMTN_CLM_TYP_CD") {
    return(ifelse(key %% 100L == 0L, "C", NA_character_))
  }
  if (upper == "SLS_PMT_TYP_CD") {
    return(ifelse(key %% 100L < 2L, "N", NA_character_))
  }
  if (upper == "UNDR_18_INCM_TYP_CD") {
    value <- rep(NA_character_, n)
    under_18 <- !is.na(age) & age < 18L
    value[under_18] <- ifelse(key[under_18] %% 5L == 0L, "M", "A")
    return(value)
  }
  if (upper == "PRT_YR_TFT_ELGBL_MTHS_CD") {
    value <- rep(NA_integer_, n)
    part_year <- key %% 20L == 0L
    value[part_year] <- 1L + as.integer(key[part_year] %% 12L)
    return(value)
  }
  if (upper == "HVYOUAPLDANEXMPTNORRLVRIND") {
    context_key <- .dil_tax_key(
      spine_rows, seed, "PIT exemption or rollover"
    )
    return(ifelse(context_key %% 20L == 0L, "Y", "N"))
  }
  if (upper == "HVYOUAPLDANEXMPTNORRLVRCD") {
    key <- .dil_tax_key(spine_rows, seed, "PIT exemption or rollover")
    applies <- key %% 20L == 0L
    value <- rep(NA_character_, n)
    value[applies] <- .dil_tax_pick(
      key[applies], c("A", "B", "C", "D", "F", "J", "R", "S", "X"),
      c(12, 10, 8, 8, 10, 8, 14, 15, 15)
    )
    return(value)
  }
  if (upper == "PNSNR_TAX_OFST_CD") {
    eligible <- !is.na(age) & age >= 60L & key %% 100L < 8L
    value <- rep(NA_character_, n)
    value[eligible] <- .dil_tax_pick(
      key[eligible], c("S", "P", "I", "D", "L", "M", "A", "J", "Q", "K", "R")
    )
    return(value)
  }
  if (upper == "PNSNR_VETERAN_CD") {
    eligible <- !is.na(age) & age >= 60L & key %% 100L < 3L
    value <- rep(NA_character_, n)
    value[eligible] <- .dil_tax_pick(key[eligible], c("V", "W", "X"))
    return(value)
  }
  if (upper == "SATO_CD") {
    eligible <- !is.na(age) & age >= 60L & key %% 100L < 18L
    value <- rep(NA_character_, n)
    value[eligible] <- .dil_tax_pick(
      key[eligible], c("A", "B", "C", "D", "E"), c(45, 15, 20, 10, 10)
    )
    return(value)
  }
  if (upper == "SNR_AUSNS_VETERAN_CD") {
    eligible <- !is.na(age) & age >= 60L & key %% 100L < 3L
    value <- rep(NA_character_, n)
    value[eligible] <- .dil_tax_pick(key[eligible], c("V", "W", "X"))
    return(value)
  }
  if (upper == "SPS_OR_HSKPR_TAX_OFST_CLM_CD") {
    eligible <- !is.na(age) & age >= 18L & key %% 100L < 4L
    value <- rep(NA_character_, n)
    value[eligible] <- .dil_tax_pick(
      key[eligible], c("S", "W", "H", "C"), c(55, 15, 20, 10)
    )
    return(value)
  }
  if (upper == "NONELGBLTAXEXMPTNSPSFBTTOTL") {
    spouse <- .dil_tax_spouse_context(spine_rows, seed, period)
    income <- if ("baseline_income" %in% names(spine_rows)) {
      pmax(suppressWarnings(as.numeric(spine_rows$baseline_income)), 0)
    } else {
      rep(50000, n)
    }
    value <- rep(NA_real_, n)
    has_amount <- (spouse$full_year | spouse$part_year) & key %% 20L == 0L
    value[has_amount] <- round(
      income[has_amount] * (0.02 + (key[has_amount] %% 800L) / 10000)
    )
    return(value)
  }
  NULL
}

.dil_tax_source_value <- function(name, description, dataset, source_frame,
                                  spine_rows, seed, period) {
  if (identical(dataset, "CGT")) {
    return(.dil_tax_cgt_source_value(
      name, source_frame, spine_rows, seed
    ))
  }
  if (identical(dataset, "SMSF")) {
    return(.dil_tax_smsf_source_value(
      name, source_frame, spine_rows, seed, period
    ))
  }
  if (identical(dataset, "ATO_MCS")) {
    return(.dil_tax_mcs_source_value(
      name, source_frame, spine_rows, seed, period
    ))
  }
  if (identical(dataset, "ATO_CR")) {
    return(.dil_tax_ato_cr_source_value(
      name, source_frame, spine_rows, seed
    ))
  }
  if (dataset %in% c("PIT_ITR", "PIT_PS", "PIT_IE", "BUSOWN")) {
    return(.dil_tax_pit_source_value(
      name, source_frame, spine_rows, seed, period
    ))
  }
  NULL
}
