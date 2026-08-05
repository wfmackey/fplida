# Canonical values for Single Touch Payroll fields that are conditional in
# the native event products.

.dil_stp_source_value <- function(name, description, source_frame,
                                  spine_rows, seed, period,
                                  table_name = "") {
  upper <- toupper(name)
  n <- nrow(spine_rows)
  key <- .dil_numeric_key(
    spine_rows, seed, .stable_name_seed("STP conditional fields")
  )

  if (upper == "ANL_LNG_SRVC_UNSD_LS_A_CD") {
    amount <- .dil_source_column(
      source_frame, "ANL_LNG_SRVC_UNSD_LS_A_AMT"
    )
    if (!is.null(amount) && length(amount) == n) {
      amount <- suppressWarnings(as.numeric(amount))
      out <- rep(NA_character_, n)
      paid <- is.finite(amount) & amount > 0
      out[paid] <- ifelse(key[paid] %% 5L == 0L, "T", "R")
      return(out)
    }
    # Canonical pay-event tables contain only a small linked sample. When the
    # native amount is unavailable, treat the sampled rows as leave-payment
    # records and use the two ATO-permitted Lump sum A codes.
    return(ifelse(key %% 5L == 0L, "T", "R"))
  }

  if (upper == "CNTRCTR_BN") {
    existing <- .dil_source_alias(source_frame, "CNTRCTR_BN")
    if (!is.null(existing)) return(as.character(existing))
    contractor <- key %% 29L == 0L
    # The value is an ABS-confidentialised business identifier, not an ABN.
    business_key <- .dil_numeric_key(
      spine_rows, seed,
      .stable_name_seed("STP contractor client business")
    )
    value <- paste0("BN", sprintf("%011.0f", business_key %% 1e11))
    value[!contractor] <- NA_character_
    return(value)
  }

  if (upper == "ETP_PMT_TYP_CD") {
    # ATO ETP codes. O and R dominate life-benefit ETPs; D, N, and T are
    # death-benefit cases. The compact native generator currently emits R.
    draw <- key %% 100L
    return(ifelse(
      draw < 55L, "O",
      ifelse(draw < 90L, "R",
             ifelse(draw < 94L, "D",
                    ifelse(draw < 99L, "N", "T")))
    ))
  }

  NULL
}
