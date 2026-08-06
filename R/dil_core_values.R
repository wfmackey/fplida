# Canonical values for PLIDA CORE tables.
#
# Public ABS classifications are used where they exist. Several CORE fields
# are internal PLIDA integration constructs without a public code frame. Those
# fields use the small, explicit local domains below instead of generic
# placeholder values.

.dil_core_column_or_spine <- function(source_frame, source_names,
                                      spine_rows, spine_name = NULL) {
  value <- .dil_source_alias(source_frame, source_names)
  if (!is.null(value)) return(value)
  if (!is.null(spine_name) && spine_name %in% names(spine_rows)) {
    return(spine_rows[[spine_name]])
  }
  NULL
}

.dil_core_country_of_birth <- function(source_frame, spine_rows) {
  value <- .dil_source_alias(
    source_frame, c("BIRTH_CTRY_CODE", "BIRTHPLACE", "BPLP")
  )
  if (!is.null(value)) {
    numeric <- suppressWarnings(as.integer(as.character(value)))
    return(ifelse(is.na(numeric), NA_character_, sprintf("%04d", numeric)))
  }
  if ("country_of_birth_sacc" %in% names(spine_rows)) {
    value <- suppressWarnings(as.integer(spine_rows$country_of_birth_sacc))
    return(ifelse(is.na(value), NA_character_, sprintf("%04d", value)))
  }
  if ("country_of_birth" %in% names(spine_rows)) {
    return(ifelse(
      as.integer(spine_rows$country_of_birth) == 0L, "1101", "9999"
    ))
  }
  rep(NA_character_, nrow(spine_rows))
}

.dil_core_census_year <- function(table_name, period) {
  lower <- tolower(paste(table_name, collapse = " "))
  if (grepl("(?:^|[_-])c?11(?:[_-]|$)|2011", lower, perl = TRUE)) {
    return(2011L)
  }
  if (grepl("(?:^|[_-])c?16(?:[_-]|$)|2016", lower, perl = TRUE)) {
    return(2016L)
  }
  if (grepl("(?:^|[_-])c?21(?:[_-]|$)|2021", lower, perl = TRUE)) {
    return(2021L)
  }
  as.integer(period$end_year)
}

.dil_core_relationship_context <- function(source_frame, spine_rows, seed,
                                           table_name, period) {
  n <- nrow(spine_rows)
  lower <- tolower(paste(table_name, collapse = " "))
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("CORE relationship context")
  )
  parent_table <- grepl(
    "par[_-]?chi|parent[_-]?child|child[_-]?type", lower, perl = TRUE
  )
  partner_table <- grepl("partner|spouse", lower, perl = TRUE)

  category <- .dil_source_alias(source_frame, "COMBINED_CATEGORY")
  if (is.null(category)) {
    category <- if (parent_table) {
      rep("Parent-Child", n)
    } else if (partner_table) {
      rep("Partner", n)
    } else {
      ifelse(key %% 100L < 55L, "Partner", "Parent-Child")
    }
  }
  category <- as.character(category)
  is_parent <- grepl("parent|child", category, ignore.case = TRUE)

  status <- .dil_source_alias(source_frame, "COMBINED_STATUS")
  if (is.null(status)) {
    status <- ifelse(
      is_parent,
      ifelse(key %% 10L < 9L, "Biological", "Step"),
      ifelse(key %% 10L < 8L, "Married", "De facto")
    )
  }
  status <- as.character(status)

  original <- .dil_source_alias(source_frame, "SPINE_ID_ORIGINAL")
  if (is.null(original)) {
    original <- if ("spine_id" %in% names(spine_rows)) {
      as.character(spine_rows$spine_id)
    } else {
      .dil_character_id("P", spine_rows, seed, 7121L, 12L)
    }
  }
  main <- .dil_source_alias(source_frame, "SPINE_ID_MAIN_REL")
  if (is.null(main)) {
    ids <- if ("spine_id" %in% names(spine_rows)) {
      as.character(spine_rows$spine_id)
    } else {
      .dil_character_id("P", spine_rows, seed, 9151L, 12L)
    }
    main <- if (length(ids) > 1L) c(ids[-1L], ids[[1L]]) else {
      paste0(ids, "R")
    }
  }

  census_year <- .dil_core_census_year(table_name, period)
  age <- pmax(census_year - as.integer(spine_rows$birth_year), 0L)
  same_sex <- key %% 20L == 0L
  married <- grepl("married", status, ignore.case = TRUE)
  step <- grepl("step", status, ignore.case = TRUE)

  list(
    key = key,
    category = category,
    status = status,
    is_parent = is_parent,
    married = married,
    step = step,
    age = age,
    original = as.character(original),
    main = as.character(main),
    pair_id = .dil_character_id(
      "REL", spine_rows, seed,
      .stable_name_seed("CORE relationship pair"), 12L
    ),
    rel_code = ifelse(
      is_parent,
      ifelse(step, "PC_STEP", "PC_BIO"),
      ifelse(married, "PT_MARRIED", "PT_DEFACTO")
    ),
    ctpp = ifelse(is_parent, ifelse(step, 2L, 1L), NA_integer_),
    mdcp = ifelse(is_parent, 3L, ifelse(married, 1L, 2L)),
    mstp = ifelse(is_parent, 1L, ifelse(married, 5L, 1L)),
    rlcp = ifelse(
      is_parent, NA_integer_,
      ifelse(married, ifelse(same_sex, 3L, 1L),
             ifelse(same_sex, 4L, 2L))
    ),
    rlhp = ifelse(
      is_parent,
      ifelse(age < 15L, 31L, ifelse(age < 25L, 41L, 51L)),
      ifelse(married, 12L, 15L)
    )
  )
}

.dil_core_address_context <- function(spine_rows, seed) {
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("CORE address context")
  )
  draw <- key %% 100L
  structure_draw <- (key %/% 100L) %% 100L
  special <- draw >= 95L
  private_structure <- ifelse(
    special, NA_integer_,
    ifelse(structure_draw < 70L, 1L,
           ifelse(structure_draw < 88L, 2L,
                  ifelse(structure_draw < 98L, 3L, 9L)))
  )
  list(
    key = key,
    address_use = ifelse(
      draw < 92L, "RESIDENTIAL",
      ifelse(draw < 97L, "NON_RESIDENTIAL", "BOTH")
    ),
    address_type = ifelse(draw < 90L, "R", "P"),
    private_structure = private_structure,
    # Local one-digit map for special-dwelling building groups. Missing means
    # the address is a private dwelling and the field is not applicable.
    special_type = ifelse(special, 1L + as.integer(key %% 8L), NA_integer_)
  )
}

.dil_core_residence_context <- function(spine_rows, seed, period) {
  year <- if (length(period$year)) as.integer(period$year) else NA_integer_
  if (is.na(year)) year <- as.integer(period$end_year)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("CORE physical-presence period")
  )
  month <- 1L + as.integer(key %% 12L)
  weight <- numeric(nrow(spine_rows))
  for (value in sort(unique(month))) {
    projected <- .dil_traveller_wide_value(
      sprintf("PP_WT_%04dM%d", year, value), spine_rows, seed
    )
    rows <- month == value
    weight[rows] <- projected[rows]
  }
  next_month_year <- ifelse(month == 12L, year + 1L, year)
  next_month <- ifelse(month == 12L, 1L, month + 1L)
  month_end <- as.Date(sprintf(
    "%04d-%02d-01", next_month_year, next_month
  )) - 1L
  list(
    pp_period = as.character(month_end),
    pp_weight = as.numeric(weight),
    pp_status = ifelse(weight > 0, 1L, 3L),
    erp_period = rep(as.character(year), nrow(spine_rows)),
    erp_status = .dil_traveller_wide_value(
      sprintf("ERP_STATUS_%04dQ2", year), spine_rows, seed
    )
  )
}

.dil_core_activity_period <- function(name, spine_rows, seed, period) {
  upper <- toupper(name)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("CORE source activity period")
  )
  last_year <- min(max(as.integer(period$end_year), 2006L), 2025L)
  first_year <- 2006L
  activity_year <- first_year + as.integer(
    key %% (last_year - first_year + 1L)
  )
  if (upper %in% c("ACTIVITY_FY", "T_PERIOD")) {
    return(sprintf("%02d", activity_year %% 100L))
  }
  if (upper == "C_PERIOD") {
    return(sprintf("%04d-%02d", activity_year, (activity_year + 1L) %% 100L))
  }
  as.character(activity_year)
}

.dil_core_source_label <- function(table_name) {
  lower <- tolower(paste(table_name, collapse = " "))
  if (grepl("birth", lower)) return("BIRTHS")
  if (grepl("domino", lower)) return("DOMINO")
  if (grepl("ato|itr|tax", lower)) return("ATO")
  if (grepl("census|2011|2016|2021", lower)) return("CENSUS")
  "COMBINED"
}

.dil_core_source_value <- function(name, description, source_frame,
                                   spine_rows, seed, period,
                                   product_name = "", table_name = "",
                                   module_name = "") {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("CORE canonical fields")
  )

  if (upper %in% c("BIRTH_CTRY_CODE", "BIRTHPLACE", "BPLP")) {
    return(.dil_core_country_of_birth(source_frame, spine_rows))
  }
  if (upper == "DOB_M") {
    value <- .dil_core_column_or_spine(
      source_frame, "MONTH_OF_BIRTH", spine_rows, "month_of_birth"
    )
    if (!is.null(value)) return(as.integer(value))
  }
  if (upper == "DOB_Y") {
    value <- .dil_core_column_or_spine(
      source_frame, "YEAR_OF_BIRTH", spine_rows, "birth_year"
    )
    if (!is.null(value)) return(as.integer(value))
  }
  if (upper == "DAY_OF_DEATH") {
    value <- .dil_core_column_or_spine(
      source_frame, "DAY_OF_DEATH", spine_rows, "day_of_death"
    )
    if (!is.null(value)) return(as.integer(value))
  }
  if (upper == "BIRTH_ID") {
    return(.dil_character_id(
      "B", spine_rows, seed, .stable_name_seed("CORE birth event"), 12L
    ))
  }
  if (upper == "DEATH_AMENDED") {
    death_year <- if ("year_of_death" %in% names(spine_rows)) {
      as.integer(spine_rows$year_of_death)
    } else {
      rep(NA_integer_, n)
    }
    return(as.integer(!is.na(death_year)))
  }
  if (upper == "MARITAL_STATUS") {
    age <- pmax(.dil_core_census_year(table_name, period) -
                  as.integer(spine_rows$birth_year), 0L)
    adult <- age >= 18L
    return(ifelse(
      !adult, 1L,
      ifelse(key %% 100L < 58L, 2L,
             ifelse(age >= 60L & key %% 100L < 68L, 3L, 4L))
    ))
  }

  if (upper == "ARID") {
    value <- .dil_source_alias(source_frame, "ARID")
    if (!is.null(value)) return(as.character(value))
    # One address key, shared with Core Locations and with every agency
    # product. This used to mint its own AR-prefixed identifier, which left a
    # person's address in ar_addr unjoinable to their address in core_loc.
    return(.dil_address_key("CORE", spine_rows, seed))
  }
  if (upper %in% c(
    "ADDRESS_USE", "ADR_TYP", "CR_ADDRESS_SOURCE",
    "DOM_ADDRESS_SOURCE", "MCD_ADDRESS_SOURCE",
    "PRIVATE_DWELLING_STRUCT", "SD_DWELLING_TYPE_ID"
  )) {
    address <- .dil_core_address_context(spine_rows, seed)
    if (upper == "ADDRESS_USE") return(address$address_use)
    if (upper %in% c(
      "ADR_TYP", "CR_ADDRESS_SOURCE", "DOM_ADDRESS_SOURCE",
      "MCD_ADDRESS_SOURCE"
    )) return(address$address_type)
    if (upper == "PRIVATE_DWELLING_STRUCT") {
      return(address$private_structure)
    }
    return(address$special_type)
  }

  relationship_names <- c(
    "COMBINED_CATEGORY", "COMBINED_STATUS", "CTPP", "FMCF", "FRLF",
    "HAD_SPS_CRNT_FY_DAYS", "HAD_SPS_CRNT_FY_FULL_PERD_CD", "HCFMF",
    "MDCP", "MSTP", "PAIRID", "REL_CODE", "RLCP", "RLGP", "RLHP",
    "SOURCES", "SPINE_ID_MAIN_REL", "SPINE_ID_ORIGINAL", "SPIP"
  )
  if (upper %in% relationship_names) {
    relationship <- .dil_core_relationship_context(
      source_frame, spine_rows, seed, table_name, period
    )
    if (upper == "COMBINED_CATEGORY") return(relationship$category)
    if (upper == "COMBINED_STATUS") return(relationship$status)
    if (upper == "PAIRID") return(relationship$pair_id)
    if (upper == "SPINE_ID_ORIGINAL") return(relationship$original)
    if (upper == "SPINE_ID_MAIN_REL") return(relationship$main)
    if (upper == "REL_CODE") return(relationship$rel_code)
    if (upper == "SOURCES") {
      return(rep(.dil_core_source_label(table_name), n))
    }
    if (upper == "CTPP") return(relationship$ctpp)
    if (upper == "MDCP") return(relationship$mdcp)
    if (upper == "MSTP") return(relationship$mstp)
    if (upper == "RLCP") return(relationship$rlcp)
    if (upper %in% c("RLHP", "RLGP")) return(relationship$rlhp)
    if (upper == "SPIP") {
      return(ifelse(relationship$is_parent, NA_integer_, 2L))
    }
    if (upper == "FMCF") {
      return(c(1L, 2L, 3L, 9L)[1L + as.integer(relationship$key %% 4L)])
    }
    if (upper == "HCFMF") {
      return(c(1L, 2L, 3L)[1L + as.integer(relationship$key %% 3L)])
    }
    if (upper == "FRLF") {
      return(1L + as.integer(relationship$key %% 7L))
    }
    full_year_spouse <- relationship$married & relationship$key %% 10L < 8L
    year_days <- ifelse(period$end_year %% 4L == 0L, 366L, 365L)
    spouse_days <- ifelse(
      full_year_spouse, NA_integer_,
      1L + as.integer(relationship$key %% (year_days - 1L))
    )
    if (upper == "HAD_SPS_CRNT_FY_DAYS") return(spouse_days)
    if (upper == "HAD_SPS_CRNT_FY_FULL_PERD_CD") {
      return(ifelse(full_year_spouse, "Y", "N"))
    }
  }

  if (upper %in% c("PP_PERIOD", "PP_STATUS", "PP_WEIGHT",
                   "ERP_PERIOD", "ERP_STATUS")) {
    residence <- .dil_core_residence_context(spine_rows, seed, period)
    return(switch(
      upper,
      PP_PERIOD = residence$pp_period,
      PP_STATUS = residence$pp_status,
      PP_WEIGHT = residence$pp_weight,
      ERP_PERIOD = residence$erp_period,
      ERP_STATUS = residence$erp_status
    ))
  }

  if (upper %in% c(
    "ACTIVITY_FY", "C_PERIOD", "D_PERIOD", "M_PERIOD", "T_PERIOD"
  )) {
    return(.dil_core_activity_period(name, spine_rows, seed, period))
  }

  NULL
}
