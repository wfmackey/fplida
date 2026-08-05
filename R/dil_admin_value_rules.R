# Dataset-specific values for canonical administrative tables.
#
# These rules supplement exact generated columns. They are limited to fields
# that can be derived from an existing generated record. Unknown code frames
# continue to return NULL and remain visible to the release audit.

.dil_source_column <- function(source_frame, name) {
  hit <- match(toupper(name), toupper(names(source_frame)))
  if (is.na(hit)) return(NULL)
  source_frame[[hit]]
}

.dil_source_alias <- function(source_frame, names) {
  for (name in names) {
    value <- .dil_source_column(source_frame, name)
    if (!is.null(value) && any(!is.na(value))) return(value)
  }
  NULL
}

.dil_births_source_value <- function(name, source_frame, spine_rows, seed) {
  upper <- toupper(name)
  if (upper == "SYTHETIC_AEUID") {
    return(.dil_source_alias(source_frame, "SYNTHETIC_AEUID"))
  }
  if (upper == "CNTRY_CDE") {
    return(rep("1101", nrow(spine_rows)))
  }
  if (upper == "POSTALTYPE") {
    key <- .dil_numeric_key(
      spine_rows, seed, .stable_name_seed("BIRTHS address type")
    )
    return(ifelse(key %% 100L < 88L, "R", "P"))
  }
  if (upper == "IN_MAY_24") return(rep(1L, nrow(spine_rows)))
  if (upper == "DUP_CLUSTER_ID") {
    row <- seq_len(nrow(spine_rows))
    member <- (row - 1L) %% 50L < 2L
    cluster <- sprintf(
      "DUP%08d",
      ((row - 1L) %/% 50L) +
        .stable_name_seed(paste("BIRTHS", seed, sep = "|"))
    )
    cluster[!member] <- NA_character_
    return(cluster)
  }
  if (upper == "QUALITY") {
    key <- .dil_numeric_key(
      spine_rows, seed, .stable_name_seed("BIRTHS linkage quality")
    )
    draw <- key %% 100L
    return(ifelse(draw < 85L, 1L, ifelse(draw < 97L, 2L, 3L)))
  }
  NULL
}

.dil_deaths_source_value <- function(name, source_frame, spine_rows, seed,
                                     period) {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed(paste("DEATHS", upper, sep = "|"))
  )
  aliases <- list(
    BIRTHPLACE = "BIRTH_PLACE",
    REMOTENESS_AREA = "REMOTENESS_AREA_2021",
    SEIFA_IRSAD_DEC = "SEIFA_IRSAD_DEC_2021",
    SEIFA_IRSD_DEC = "SEIFA_IRSD_DEC_2021"
  )
  if (upper %in% names(aliases)) {
    value <- .dil_source_alias(source_frame, aliases[[upper]])
    if (!is.null(value)) return(value)
  }
  if (upper %in% c("BIRTH_PLACE", "BIRTHPLACE")) {
    if ("country_of_birth_sacc" %in% names(spine_rows)) {
      return(as.integer(spine_rows$country_of_birth_sacc))
    }
    if ("country_of_birth" %in% names(spine_rows)) {
      return(ifelse(
        as.integer(spine_rows$country_of_birth) == 0L, 1101L, 9999L
      ))
    }
  }
  if (upper == "MARITAL_STATUS") {
    age <- pmax(period$end_year - as.integer(spine_rows$birth_year), 0L)
    adult <- age >= 18L
    value <- rep(1L, n)
    value[adult & key %% 10L < 6L] <- 2L
    value[adult & age >= 60L & key %% 10L == 6L] <- 3L
    value[adult & key %% 10L >= 7L] <- 4L
    return(value)
  }
  if (upper == "CERTIFIER") {
    return(ifelse(key %% 5L == 0L, "C", "D"))
  }
  if (upper == "PLACE_OF_DEATH") {
    draw <- key %% 100L
    return(ifelse(
      draw < 50L, 1L,
      ifelse(draw < 75L, 2L, ifelse(draw < 90L, 3L, 4L))
    ))
  }
  if (upper %in% c("REMOTENESS_AREA", "REMOTENESS_AREA_2021")) {
    draw <- key %% 100L
    return(ifelse(
      draw < 65L, 1L,
      ifelse(draw < 85L, 2L,
             ifelse(draw < 95L, 3L, ifelse(draw < 98L, 4L, 5L)))
    ))
  }
  if (upper %in% c(
    "SEIFA_IRSAD_DEC", "SEIFA_IRSAD_DEC_2021",
    "SEIFA_IRSD_DEC", "SEIFA_IRSD_DEC_2021"
  )) {
    return(1L + as.integer(key %% 10L))
  }
  if (upper == "PERIOD_RESIDENCE") {
    death_year <- .dil_source_alias(source_frame, "YEAR_OF_DEATH")
    if (is.null(death_year)) {
      death_year <- rep(period$end_year, nrow(spine_rows))
    }
    arrival_year <- if ("year_of_arrival" %in% names(spine_rows)) {
      as.integer(spine_rows$year_of_arrival)
    } else {
      rep(NA_integer_, nrow(spine_rows))
    }
    start_year <- ifelse(
      is.na(arrival_year), as.integer(spine_rows$birth_year), arrival_year
    )
    return(pmax(as.integer(death_year) - start_year, 0L))
  }
  NULL
}

.dil_mcd_source_value <- function(name, source_frame, spine_rows, seed,
                                  period) {
  upper <- toupper(name)
  if (upper == "CNSMR_CHRTC_STS") {
    return(.dil_source_alias(source_frame, "CNSMR_STS"))
  }
  if (upper %in% c("CNSMR_CHRTC_ETS", "CNSMR_ETS")) {
    start <- .dil_source_alias(source_frame, "CNSMR_STS")
    if (is.null(start)) return(NULL)
    key <- .dil_numeric_key(
      spine_rows, seed, .stable_name_seed("MCD consumer end timestamp")
    )
    ended <- key %% 20 == 0
    if (length(ended) && !any(ended)) ended[[1L]] <- TRUE
    value <- rep(as.Date(NA), length(start))
    value[ended] <- pmax(
      as.Date(start[ended]) + 30L,
      period$end - as.integer(key[ended] %% 365L)
    )
    return(value)
  }
  if (upper %in% c("CNTRY_CDE", "REL_CNTRY_CDE")) {
    if ("country_of_birth_sacc" %in% names(spine_rows)) {
      return(sprintf(
        "%04d", as.integer(spine_rows$country_of_birth_sacc)
      ))
    }
    if ("country_of_birth" %in% names(spine_rows)) {
      return(ifelse(as.integer(spine_rows$country_of_birth) == 0L,
                    "1101", NA_character_))
    }
  }
  NULL
}

.dil_cgt_method <- function(name) {
  upper <- toupper(name)
  if (grepl("(?:^|_)(?:DISC|DM)(?:_|$)|DSCNT", upper, perl = TRUE)) {
    return("discount")
  }
  if (grepl("(?:^|_)(?:INDX|IM)(?:_|$)|IDXTN", upper, perl = TRUE)) {
    return("indexation")
  }
  if (grepl("(?:^|_)(?:OTH|OM)(?:_|$)|OTHER", upper, perl = TRUE)) {
    return("other")
  }
  NULL
}

.dil_cgt_components <- function(source_frame, spine_rows, seed, period) {
  total <- as.numeric(.dil_source_column(source_frame, "CG_CY_TOTL_AMT"))
  real_estate <- as.numeric(.dil_source_column(
    source_frame, "CG_TOTL_REAL_EST_AMT"
  ))
  shares <- as.numeric(.dil_source_column(
    source_frame, "CG_TOTL_SHARES_UNITS_AMT"
  ))
  other <- as.numeric(.dil_source_column(source_frame, "CG_TOTL_OTH_AMT"))
  losses <- as.numeric(.dil_source_column(source_frame, "CL_CY_TOTL_AMT"))
  discount_applied <- as.numeric(.dil_source_column(
    source_frame, "CGT_TOTL_DSCNT_APLD_AMT"
  ))
  concessions <- as.numeric(.dil_source_column(
    source_frame, "SB_CNCSNS_APLD_TOTL_AMT"
  ))
  n <- length(total)
  required <- list(
    total, real_estate, shares, other, losses, discount_applied, concessions
  )
  if (!n || any(vapply(required, length, integer(1)) != n) ||
      nrow(spine_rows) != n) {
    return(NULL)
  }
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("CGT canonical decomposition")
  )
  active_share <- 0.10 + 0.15 * ((key %% 1000) / 999)
  index_share <- if (period$end_year <= 2010L) 0.08 else 0.02
  method_share <- c(
    discount = 0.72,
    indexation = index_share,
    other = 0.28 - index_share
  )

  asset <- list(
    active_real_estate = real_estate * active_share,
    inactive_real_estate = real_estate * (1 - active_share),
    active_shares = shares * active_share,
    inactive_shares = shares * (1 - active_share),
    active_fmis = other * 0.01,
    active_other = other * 0.09,
    inactive_collectables = other * 0.04,
    inactive_fmis = other * 0.01,
    inactive_hedging = other * 0.05,
    inactive_other = other * 0.80
  )
  active_total <- asset$active_real_estate + asset$active_shares +
    asset$active_fmis + asset$active_other
  inactive_total <- pmax(total - active_total, 0)

  loss_share <- c(
    real_estate = 0.40, shares = 0.35, fmis = 0.05, hedging = 0.20
  )
  method_loss <- lapply(method_share, function(share) {
    pmin(losses * share, total * share)
  })
  post_loss <- lapply(names(method_share), function(method) {
    pmax(total * method_share[[method]] - method_loss[[method]], 0)
  })
  names(post_loss) <- names(method_share)

  discount_denominator <- pmax(total * method_share[["discount"]], 1)
  discount_active <- pmin(
    discount_applied * active_total * method_share[["discount"]] /
      discount_denominator,
    active_total * method_share[["discount"]]
  )
  discount_inactive <- pmax(discount_applied - discount_active, 0)

  list(
    total = total,
    losses = losses,
    concessions = concessions,
    method_share = method_share,
    asset = asset,
    active_total = active_total,
    inactive_total = inactive_total,
    loss_share = loss_share,
    method_loss = method_loss,
    post_loss = post_loss,
    discount_active = discount_active,
    discount_inactive = discount_inactive,
    key = key
  )
}

.dil_cgt_asset_component <- function(name, components) {
  upper <- toupper(name)
  active <- startsWith(upper, "ACTV_")
  if (active && grepl("FMIS", upper, fixed = TRUE)) {
    return(components$asset$active_fmis)
  }
  if (active && grepl("REAL_EST", upper, fixed = TRUE)) {
    return(components$asset$active_real_estate)
  }
  if (active && grepl("SHARES_UNITS", upper, fixed = TRUE)) {
    return(components$asset$active_shares)
  }
  if (active && grepl("OTH_ASSTS", upper, fixed = TRUE)) {
    return(components$asset$active_other)
  }
  if (startsWith(upper, "N_ACTV_") &&
      grepl("COLLECT", upper, fixed = TRUE)) {
    return(components$asset$inactive_collectables)
  }
  if (startsWith(upper, "N_ACTV_") &&
      grepl("FMIS", upper, fixed = TRUE)) {
    return(components$asset$inactive_fmis)
  }
  if (startsWith(upper, "N_ACTV_") &&
      grepl("HEDG_FINC", upper, fixed = TRUE)) {
    return(components$asset$inactive_hedging)
  }
  if (startsWith(upper, "N_ACTV_") &&
      grepl("REAL_EST", upper, fixed = TRUE)) {
    return(components$asset$inactive_real_estate)
  }
  if (startsWith(upper, "N_ACTV_") &&
      grepl("SHARES_UNITS", upper, fixed = TRUE)) {
    return(components$asset$inactive_shares)
  }
  if (startsWith(upper, "N_ACTV_") &&
      grepl("OTH_ASSTS", upper, fixed = TRUE)) {
    return(components$asset$inactive_other)
  }
  NULL
}

.dil_cgt_source_value <- function(name, description, source_frame,
                                  spine_rows, seed, period) {
  upper <- toupper(name)
  components <- .dil_cgt_components(
    source_frame, spine_rows, seed, period
  )
  if (is.null(components)) return(NULL)

  if (upper == "CGERNTARNGMNTSYRSRUNFRNUM") {
    has_earnout <- components$key %% 20 == 0
    return(ifelse(has_earnout, 1L + components$key %% 5L, 0L))
  }
  if (upper == "CGERNTARNGCPTLGAINLSSTHSYRAMT") {
    has_earnout <- components$key %% 20 == 0
    direction <- ifelse(components$key %% 2 == 0, 1, -1)
    return(ifelse(
      has_earnout,
      round(direction * components$total * (0.05 +
        0.15 * ((components$key %% 100) / 99))),
      0
    ))
  }

  amount_description <- grepl(
    paste0(
      "capital gain|capital loss|cash and other considerations|",
      "total current year|amount"
    ),
    description, ignore.case = TRUE, perl = TRUE
  )
  if (!amount_description) return(NULL)

  method <- .dil_cgt_method(upper)
  asset <- .dil_cgt_asset_component(upper, components)
  if (!is.null(asset) && !is.null(method)) {
    return(round(asset * components$method_share[[method]]))
  }

  if (startsWith(upper, "CYCL_")) {
    loss_type <- if (grepl("REAL_EST", upper, fixed = TRUE)) {
      "real_estate"
    } else if (grepl("SHARES_UNITS", upper, fixed = TRUE)) {
      "shares"
    } else if (grepl("FMIS", upper, fixed = TRUE)) {
      "fmis"
    } else {
      "hedging"
    }
    return(round(components$losses * components$loss_share[[loss_type]]))
  }
  if (startsWith(upper, "D_TOTL_CAP_LSS_APLD_") && !is.null(method)) {
    return(round(components$method_loss[[method]]))
  }
  if (startsWith(upper, "E_CYCG_ACTV_") && !is.null(method)) {
    return(round(components$active_total * components$method_share[[method]]))
  }
  if (startsWith(upper, "E_CYCG_N_ACTV_") && !is.null(method)) {
    return(round(components$inactive_total *
      components$method_share[[method]]))
  }
  if (startsWith(upper, "E_TOTL_CYCG_LSS_APLD_") && !is.null(method)) {
    return(round(components$post_loss[[method]]))
  }
  if (upper == "F_CGT_DISC_ACTV_K") {
    return(round(components$discount_active))
  }
  if (upper == "F_CGT_DISC_N_ACTV_J") {
    return(round(components$discount_inactive))
  }
  if (startsWith(upper, "G_SB_CONC") && !is.null(method)) {
    concession_share <- if (grepl("ACT", upper, fixed = TRUE)) {
      0.50
    } else if (grepl("RETIRE", upper, fixed = TRUE) ||
               grepl("RTIRE", upper, fixed = TRUE)) {
      0.30
    } else {
      0.20
    }
    return(round(components$concessions * concession_share *
      components$method_share[[method]]))
  }
  if (startsWith(upper, "H_TOTALSB_CONCS_") && !is.null(method)) {
    return(round(components$concessions * components$method_share[[method]]))
  }
  if (startsWith(upper, "H_CG_") && !is.null(method)) {
    discount <- if (method == "discount") {
      components$discount_active + components$discount_inactive
    } else {
      0
    }
    return(round(pmax(
      components$post_loss[[method]] - discount -
        components$concessions * components$method_share[[method]],
      0
    )))
  }
  if (upper == "K_SCRPT_ORIGINAL_CASH_D") {
    return(round(components$total *
      (1.10 + 0.40 * ((components$key %% 100) / 99))))
  }
  if (upper == "TOTL_CY_CAPTL_GAIN_DSCNT") {
    return(round(components$total * components$method_share[["discount"]]))
  }
  if (upper == "TOTL_CY_CAPTL_GAIN_IDXTN") {
    return(round(components$total * components$method_share[["indexation"]]))
  }
  if (upper == "OTH_TOTL_CY_CAPTL_GAIN") {
    return(round(components$total * components$method_share[["other"]]))
  }
  NULL
}

.dil_dataset_source_value <- function(name, description, dataset,
                                      source_frame, spine_rows, seed,
                                      period, product_name = "",
                                      table_name = "", module_name = "") {
  if (dataset %in% c("AMEP", "APSED", "MT_DEMOGS", "SDB", "VISA")) {
    home_affairs_value <- .dil_home_affairs_source_value(
      name, description, dataset, source_frame, spine_rows, seed, period,
      product_name, table_name
    )
    if (!is.null(home_affairs_value)) return(home_affairs_value)
  }
  if (identical(dataset, "BIRTHS")) {
    return(.dil_births_source_value(
      name, source_frame, spine_rows, seed
    ))
  }
  if (identical(dataset, "DEATHS")) {
    return(.dil_deaths_source_value(
      name, source_frame, spine_rows, seed, period
    ))
  }
  if (identical(dataset, "MCD")) {
    return(.dil_mcd_source_value(
      name, source_frame, spine_rows, seed, period
    ))
  }
  if (dataset %in% c(
    "CGT", "SMSF", "ATO_MCS", "ATO_CR", "PIT_ITR", "PIT_PS",
    "PIT_IE", "BUSOWN"
  )) {
    tax_value <- .dil_tax_source_value(
      name, description, dataset, source_frame, spine_rows, seed, period
    )
    if (!is.null(tax_value)) return(tax_value)
  }
  if (identical(dataset, "CGT")) {
    return(.dil_cgt_source_value(
      name, description, source_frame, spine_rows, seed, period
    ))
  }
  if (identical(dataset, "CORE")) {
    return(.dil_core_source_value(
      name, description, source_frame, spine_rows, seed, period,
      product_name, table_name, module_name
    ))
  }
  if (identical(dataset, "STP")) {
    return(.dil_stp_source_value(
      name, description, source_frame, spine_rows, seed, period, table_name
    ))
  }
  if (identical(dataset, "ERS")) {
    return(.dil_ers_source_value(
      name, description, source_frame, spine_rows, seed, period
    ))
  }
  if (identical(dataset, "HE")) {
    return(.dil_he_source_value(
      name, source_frame, spine_rows, seed, period
    ))
  }
  if (identical(dataset, "TVA")) {
    return(.dil_tva_source_value(
      name, description, source_frame, spine_rows, seed, period
    ))
  }
  if (identical(dataset, "A&T")) {
    return(.dil_apprentice_source_value(
      name, description, source_frame, spine_rows, seed, period
    ))
  }
  if (identical(dataset, "NDIS")) {
    return(.dil_ndis_source_value(
      name, description, source_frame, spine_rows, seed, period,
      product_name, table_name, module_name
    ))
  }
  if (identical(dataset, "ACLD")) {
    return(.dil_acld_source_value(
      name, source_frame, spine_rows, seed,
      product_name, table_name, module_name
    ))
  }
  if (identical(dataset, "AEDC")) {
    return(.dil_aedc_source_value(
      name, description, source_frame, spine_rows, seed, period,
      product_name, table_name
    ))
  }
  if (identical(dataset, "DEX")) {
    return(.dil_dex_source_value(
      name, description, source_frame, spine_rows, seed, period,
      product_name, table_name, module_name
    ))
  }
  if (identical(dataset, "DOMINO")) {
    return(.dil_domino_source_value(
      name, description, source_frame, spine_rows, seed, period,
      product_name, table_name, module_name
    ))
  }
  if (identical(dataset, "SAE")) {
    return(.dil_sae_source_value(
      name, description, source_frame, spine_rows, seed, period,
      product_name, table_name, module_name
    ))
  }
  if (identical(dataset, "RPS")) {
    return(.dil_rps_source_value(
      name, description, source_frame, spine_rows, seed, period,
      product_name, table_name, module_name
    ))
  }
  NULL
}
