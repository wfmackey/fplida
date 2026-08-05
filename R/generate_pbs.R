#' Generate PBS dataset (Pharmaceutical Benefits Scheme Dispensings)
#'
#' Consolidated single-Rust-call entry point. All heavy work lives
#' inside `generate_pbs_full__`, which runs year-level `par_iter`
#' with streaming parquet writes per year. The R wrapper just loads
#' the item lookup and spine columns, then calls Rust once.
#'
#' @section Dataset and variable information:
#' The [PBS overview](https://www.pbs.gov.au/about) gives information about
#' this dataset. Use `dataset_info("PBS")` for dataset information. Use
#' `variable_info("PBS")` for variables, sources, value support, and topic tags.
#'
#' @param spine Data.frame or NULL.
#' @param seed Integer.
#' @param years Integer vector of calendar years to generate.
#' @param output_dir Character or NULL.
#' @param format "parquet" only.
#' @param return_data Logical. If TRUE, read back from disk.
#' @param chunk_size Integer. Persons per streaming write batch.
#'   Default 100000. Peak memory per year is bounded by this.
#' @export
generate_pbs <- function(spine = NULL, seed = 42L, years = 2006L:2025L,
                         output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = FALSE,
                         chunk_size = 100000L) {
  seed  <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  chunk_size <- as.integer(chunk_size)
  stopifnot(
    "`seed` must be integer"       = !is.na(seed),
    "`years` must be integer"      = all(!is.na(years)),
    "`chunk_size` must be positive" = chunk_size > 0L
  )
  if (format != "parquet") {
    stop("generate_pbs() now writes parquet only.", call. = FALSE)
  }

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "PBS")

  pbs_cols <- c("spine_id", "aeuid_dhda", "birth_year", "month_of_birth",
                "sex", "state",
                "year_of_death", "month_of_death", "day_of_death",
                "baseline_income", "baseline_employed",
                "disability_onset_year", "disability_severity", "is_dc")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, pbs_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))
  corr <- .healthcare_correlation_vectors(spine)

  mini_spine <- data.frame(
    spine_id   = spine$spine_id,
    aeuid_dhda = spine$aeuid_dhda,
    stringsAsFactors = FALSE
  )

  items <- .load_pbs_items()
  pack_size_clean <- as.integer(
    ifelse(is.na(items$pack_size), 30L, items$pack_size)
  )
  repeats_clean <- as.integer(
    ifelse(is.na(items$number_of_repeats), 0L, items$number_of_repeats)
  )

  yr_start <- min(as.integer(years))
  yr_end   <- max(as.integer(years))

  res <- generate_pbs_full__(
    aeuid_dhda        = as.character(spine$aeuid_dhda),
    birth_year        = as.integer(spine$birth_year),
    month_of_birth    = .spine_month_of_birth(spine),
    sex               = as.integer(spine$sex),
    state             = as.integer(spine$state),
    year_of_death     = corr$year_of_death,
    month_of_death    = corr$month_of_death,
    day_of_death      = corr$day_of_death,
    baseline_income   = corr$baseline_income,
    baseline_employed = corr$baseline_employed,
    disability_onset_year = corr$disability_onset_year,
    disability_severity   = corr$disability_severity,
    disability_is_dc      = corr$disability_is_dc,
    pbs_code          = as.character(items$pbs_code),
    atc_level1        = as.character(items$atc_level1),
    benefit_type      = as.character(items$benefit_type),
    claimed_price     = as.numeric(items$claimed_price),
    pack_size         = pack_size_clean,
    number_of_repeats = repeats_clean,
    item_weight       = as.numeric(items$weight),
    out_dir           = ds_dir,
    year_start        = yr_start,
    year_end          = yr_end,
    seed              = as.integer(seed),
    chunk_persons     = chunk_size
  )

  # Rename to canonical product names
  for (yr in seq(yr_start, yr_end)) {
    src <- file.path(ds_dir, sprintf("madipge-pbs-d-prescriptions-%d.parquet", yr))
    dst <- file.path(ds_dir, paste0(pbs_product_name(yr), ".parquet"))
    if (src != dst && file.exists(src)) {
      move_file(src, dst)
    }
  }

  write_agency_spine(mini_spine, "DHDA", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  n_disp <- as.integer(res$n_dispensings)
  names(n_disp) <- as.character(res$year)

  if (return_data) {
    result <- list()
    for (yr in years) {
      path <- file.path(ds_dir, paste0(pbs_product_name(yr), ".parquet"))
      if (file.exists(path)) {
        result[[as.character(yr)]] <- as.data.frame(
          read_parquet_safely(path), stringsAsFactors = FALSE
        )
      } else {
        result[[as.character(yr)]] <- .empty_pbs_dispensings()
      }
    }
    return(result)
  }

  invisible(list(n_dispensings = n_disp, years = years))
}


# ===========================================================================
# Item lookup (lazy-loaded, cached per session)
# ===========================================================================
.pbs_item_env <- new.env(parent = emptyenv())

.load_pbs_items <- function() {
  if (!is.null(.pbs_item_env$data)) return(.pbs_item_env$data)
  csv_path <- system.file("extdata", "pbs_item_lookup.csv", package = "fplida")
  if (!nzchar(csv_path)) {
    stop("PBS item lookup not found. Reinstall fplida.", call. = FALSE)
  }
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  .pbs_item_env$data <- df
  df
}


.empty_pbs_dispensings <- function() {
  data.frame(
    SYNTHETIC_AEUID = character(0), PTNT_BRTH_MTH = integer(0),
    PTNT_BRTH_YR = integer(0), PTNT_SEX_CD = character(0),
    PTNT_PSTCD = character(0), PTNT_CTGRY_DRVD_CD = character(0),
    ITM_CD = character(0), DRG_TYP_CD = character(0),
    PRSCRB_DT = as.Date(character(0)), SPPLY_DT = as.Date(character(0)),
    EXTRCT_DT = as.Date(character(0)),
    PRSCRPTN_CNT = integer(0), RPT_ORDR_NMBR = integer(0),
    PRVS_SPPLY_NMBR = integer(0), PBS_RGLTN24_ADJST_QTY = integer(0),
    SRT_RPT_IND = character(0),
    PTNT_CNTRBTN_AMT = numeric(0), BNFT_AMT = numeric(0),
    CTG_BNFT_AMT = numeric(0), CTG_CD = character(0),
    PRSCRBR_ID_SCRAM = character(0), PRSCRBR_TYP_CD = character(0),
    PRSCRBR_MJR_PSTCD = character(0), MJR_SPCLTY_GRP_CD = character(0),
    PHRMCY_ID_SCRAM = character(0), PHRMCY_PSTCD = character(0),
    PHRMCY_APPRVL_TYP_CD = character(0), RGLTN24_IND = character(0),
    STRMLND_ATHRTY_CD = character(0), UNDR_CPRSCRPTN_TYP_CD = character(0),
    stringsAsFactors = FALSE
  )
}
