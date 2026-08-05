#' Generate MBS dataset (Medicare Benefits Schedule Claims)
#'
#' Consolidated single-Rust-call entry point. All heavy work — spine
#' column access, participant selection, provider/practice pool
#' generation, year loop, per-year chunked claim generation, and
#' parquet writes — lives inside `generate_mbs_full__`. The R
#' wrapper is a thin facade that loads the item lookup (cached CSV)
#' and the spine column vectors, then makes a single Rust call.
#'
#' Year-level parallelism: `generate_mbs_full__` uses `rayon::par_iter`
#' across years, so multiple years are generated concurrently. Each
#' year uses a streaming ArrowWriter that flushes per-chunk
#' RecordBatches, bounding memory to `chunk_size` persons' worth.
#'
#' @section Dataset and variable information:
#' [MBS Online](https://www.mbsonline.gov.au/) gives information about this
#' dataset. Use `dataset_info("MBS")` for dataset information. Use
#' `variable_info("MBS")` for variables, sources, value support, and topic tags.
#'
#' @param spine Data.frame from `generate_spine()` or NULL to load
#'   from the run directory.
#' @param seed Integer random seed for MBS generation.
#' @param years Integer vector of calendar years to generate.
#' @param output_dir Character or NULL. Base output directory.
#' @param format Character. "parquet" (default) or "csv".
#' @param return_data Logical. If TRUE, reads back the written
#'   parquet files and returns a named list keyed by year. Default
#'   FALSE (stream to disk only, return metadata).
#' @param chunk_size Integer. Persons per streaming write batch.
#'   Default 100000. Peak memory per year is bounded by this.
#'
#' @return If `return_data = TRUE`: named list keyed by year, each
#'   a data.frame of claims. If FALSE: metadata with per-year counts.
#' @export
generate_mbs <- function(spine = NULL, seed = 42L, years = 2006L:2025L,
                         output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = FALSE,
                         chunk_size = 100000L) {
  seed  <- as.integer(seed)
  years <- as.integer(years)
  format <- match.arg(format)
  chunk_size <- as.integer(chunk_size)
  stopifnot(
    "`seed` must be an integer"     = !is.na(seed),
    "`years` must be integer"       = all(!is.na(years)),
    "`chunk_size` must be positive" = chunk_size > 0L
  )
  if (format != "parquet") {
    stop("generate_mbs() now writes parquet only. CSV path removed ",
         "with the full__ consolidation.", call. = FALSE)
  }

  run_dir <- resolve_run_dir(output_dir)
  ds_dir  <- dataset_dir(run_dir, "MBS")

  # ---- Load spine columns ------------------------------------------------
  mbs_cols <- c("spine_id", "aeuid_dhda", "birth_year", "month_of_birth",
                "sex", "state",
                "year_of_death", "month_of_death", "day_of_death",
                "baseline_income", "baseline_employed",
                "disability_onset_year", "disability_severity", "is_dc")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, mbs_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))
  corr <- .healthcare_correlation_vectors(spine)

  mini_spine <- data.frame(
    spine_id   = spine$spine_id,
    aeuid_dhda = spine$aeuid_dhda,
    stringsAsFactors = FALSE
  )

  # ---- Load item lookup (R-side CSV cache, tiny cost) --------------------
  items <- .load_mbs_items()
  sub_heading_clean <- as.character(
    ifelse(is.na(items$sub_heading), "", items$sub_heading)
  )

  # ---- Single Rust call: participants, pools, year-par_iter, writes ------
  yr_start <- min(as.integer(years))
  yr_end   <- max(as.integer(years))

  res <- generate_mbs_full__(
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
    item_num          = as.integer(items$item_num),
    item_category     = as.integer(items$category),
    item_group        = as.character(items$group),
    item_sub_heading  = sub_heading_clean,
    item_benefit_type = as.character(items$benefit_type),
    item_schedule_fee = as.numeric(items$schedule_fee),
    item_benefit_75   = as.numeric(items$benefit_75),
    item_benefit_85   = as.numeric(items$benefit_85),
    item_benefit_100  = as.numeric(items$benefit_100),
    item_weight       = as.numeric(items$weight),
    out_dir           = ds_dir,
    year_start        = yr_start,
    year_end          = yr_end,
    seed              = as.integer(seed),
    chunk_persons     = chunk_size
  )

  # ---- Rename output files to canonical product names -------------------
  # Rust writes as "madipge-mbs-d-claims-YYYY.parquet" — convert to the
  # PLIDA canonical product name per year (currently identical but
  # parametrized for future lookup).
  for (yr in seq(yr_start, yr_end)) {
    src <- file.path(ds_dir, sprintf("madipge-mbs-d-claims-%d.parquet", yr))
    dst <- file.path(ds_dir, paste0(mbs_product_name(yr), ".parquet"))
    if (src != dst && file.exists(src)) {
      move_file(src, dst)
    }
  }

  # ---- Agency spine -----------------------------------------------------
  write_agency_spine(mini_spine, "DHDA", ds_dir, format = format)
  if (spine_loaded) { rm(spine); gc() }

  n_claims <- as.integer(res$n_claims)
  names(n_claims) <- as.character(res$year)

  if (return_data) {
    result <- list()
    for (yr in years) {
      path <- file.path(ds_dir, paste0(mbs_product_name(yr), ".parquet"))
      if (file.exists(path)) {
        result[[as.character(yr)]] <- as.data.frame(
          read_parquet_safely(path), stringsAsFactors = FALSE
        )
      } else {
        result[[as.character(yr)]] <- .empty_mbs_claims()
      }
    }
    return(result)
  }

  invisible(list(n_claims = n_claims, years = years))
}

# Spine month of birth, used to hold claims and dispensings on or after the
# person's birth. A spine without the column falls back to January, which
# reproduces the old whole-birth-year window.
.spine_month_of_birth <- function(spine) {
  if ("month_of_birth" %in% names(spine)) {
    mob <- as.integer(spine$month_of_birth)
    mob[is.na(mob) | mob < 1L | mob > 12L] <- 1L
    mob
  } else {
    rep(1L, nrow(spine))
  }
}

.healthcare_correlation_vectors <- function(spine) {
  n <- nrow(spine)
  list(
    baseline_income = if ("baseline_income" %in% names(spine)) {
      as.numeric(spine$baseline_income)
    } else {
      rep(NA_real_, n)
    },
    baseline_employed = if ("baseline_employed" %in% names(spine)) {
      as.integer(spine$baseline_employed)
    } else {
      rep(NA_integer_, n)
    },
    disability_onset_year = if ("disability_onset_year" %in% names(spine)) {
      as.integer(spine$disability_onset_year)
    } else {
      rep(NA_integer_, n)
    },
    disability_severity = if ("disability_severity" %in% names(spine)) {
      as.integer(spine$disability_severity)
    } else {
      rep(NA_integer_, n)
    },
    disability_is_dc = if ("is_dc" %in% names(spine)) {
      as.integer(spine$is_dc)
    } else {
      rep(NA_integer_, n)
    },
    year_of_death = if ("year_of_death" %in% names(spine)) {
      as.integer(spine$year_of_death)
    } else {
      rep(NA_integer_, n)
    },
    month_of_death = if ("month_of_death" %in% names(spine)) {
      as.integer(spine$month_of_death)
    } else {
      rep(NA_integer_, n)
    },
    day_of_death = if ("day_of_death" %in% names(spine)) {
      as.integer(spine$day_of_death)
    } else {
      rep(NA_integer_, n)
    }
  )
}


# ===========================================================================
# Item lookup (lazy-loaded, cached per session)
# ===========================================================================
.mbs_item_env <- new.env(parent = emptyenv())

#' Load MBS item lookup table (cached)
#' @noRd
.load_mbs_items <- function() {
  if (!is.null(.mbs_item_env$data)) return(.mbs_item_env$data)
  csv_path <- system.file("extdata", "mbs_item_lookup.csv", package = "fplida")
  if (!nzchar(csv_path)) {
    stop("MBS item lookup not found. Reinstall fplida.", call. = FALSE)
  }
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  df$item_num <- as.integer(df$item_num)
  df$category <- as.integer(df$category)
  df$group    <- as.character(df$group)
  .mbs_item_env$data <- df
  df
}


# ===========================================================================
# Empty data.frame helper (for zero-participant edge case)
# ===========================================================================
.empty_mbs_claims <- function() {
  data.frame(
    SYNTHETIC_AEUID = character(0),
    DOS = as.Date(character(0)), DOP = as.Date(character(0)),
    ITEM = character(0), AGGRITEM = character(0),
    MBSCAT = integer(0), MBSGROUP = character(0),
    MBSSUBGROUP = character(0), BTOS = character(0),
    SPR_RSP = character(0),
    FEECHARGED = numeric(0), BENPAID = numeric(0), SCHEDFEE = numeric(0),
    NUMSERV = integer(0), INHOSPITAL = character(0),
    BILLTYPECD = character(0), ENRPC = character(0), SPRPC = character(0),
    SCRAM_SPR = character(0), SCRAM_RPR = character(0),
    SPRPRAC = character(0), RPRPRAC = character(0),
    RPDATE = as.Date(character(0)), RPRPC = character(0),
    stringsAsFactors = FALSE
  )
}
