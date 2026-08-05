#' Generate Longitudinal Labour Force Survey data
#'
#' Projects a household-based, person-month LLFS sample from the fplida
#' spine using the April 2026 ABS 6602.0 Longitudinal Labour Force data item
#' list. Selected households are followed for up to eight
#' monthly responses, with household attrition and workbook-driven
#' monthly/quarterly collection cadence. The generator also writes the
#' Characteristics of Employment supplementary person file (`pmp-coes`) for
#' August respondents, using the ABS COE DataLab item list.
#'
#' @section Dataset and variable information:
#' The [ABS Longitudinal Labour Force microdata](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/longitudinal-labour-force-australia)
#' website gives information about this dataset. Use `dataset_info("LFS")` for
#' dataset information. Use `variable_info("LFS")` for variables, sources,
#' value support, and topic tags.
#'
#' @inheritParams generate_apsed
#' @param sample_household_rate Numeric. Fraction of eligible households
#'   selected into the LLFS panel before applying \code{max_households}.
#' @param max_households Integer. Maximum selected households. Keeps full
#'   builds bounded while preserving household-level sampling.
#' @param survey_year Integer. Calendar year for household entry cohorts.
#'   Each selected household contributes up to eight consecutive monthly
#'   responses from its first selection month, so output can extend into
#'   the following year.
#'
#' @export
generate_lfs <- function(spine = NULL, seed = 42L, output_dir = NULL,
                         format = c("parquet", "csv"),
                         return_data = TRUE,
                         sample_household_rate = 0.01,
                         max_households = 30000L,
                         survey_year = 2025L) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  return_data <- isTRUE(return_data)
  sample_household_rate <- as.numeric(sample_household_rate)
  max_households <- as.integer(max_households)
  survey_year <- as.integer(survey_year)
  stopifnot(
    "`seed` must be an integer" = !is.na(seed),
    "`sample_household_rate` must be in (0, 1]" =
      is.finite(sample_household_rate) &&
      sample_household_rate > 0 && sample_household_rate <= 1,
    "`max_households` must be positive" =
      !is.na(max_households) && max_households > 0L,
    "`survey_year` must be a valid year" =
      !is.na(survey_year) && survey_year >= 1900L
  )

  run_dir <- resolve_run_dir(output_dir)
  ds_dir <- dataset_dir(run_dir, "LFS")

  lfs_cols <- c("spine_id", "aeuid_abs", "household_id",
                "birth_year", "month_of_birth", "sex", "state",
                "sa2_code", "sa4_code", "country_of_birth",
                "country_of_birth_sacc", "year_of_arrival",
                "education", "baseline_employed", "baseline_hours",
                "baseline_income", "industry", "anzsco_code",
                "anzsco_major", "skill_level", "person_type")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, lfs_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))

  specs <- .llfs_variables()
  values <- .llfs_values()
  coes_specs <- .coes_variables()
  coes_values <- .coes_values()
  frame <- .project_llfs(
    spine = spine,
    specs = specs,
    values = values,
    seed = seed,
    sample_household_rate = sample_household_rate,
    max_households = max_households,
    survey_year = survey_year
  )
  coes_frame <- .project_coes(
    lfs = frame,
    specs = coes_specs,
    values = coes_values,
    seed = seed,
    survey_year = survey_year
  )

  path <- write_product(frame, .llfs_product_name(), "LFS", run_dir,
                        format = format)
  coes_path <- write_product(coes_frame, .coes_product_name(), "LFS", run_dir,
                             format = format)
  mini_spine <- data.frame(
    spine_id = spine$spine_id,
    aeuid_abs = spine$aeuid_abs,
    stringsAsFactors = FALSE
  )
  write_agency_spine(mini_spine, "ABS", ds_dir, format = format)
  if (spine_loaded) {
    rm(spine)
    gc()
  }

  n_rows <- nrow(frame)
  n_coes_rows <- nrow(coes_frame)
  if (return_data) {
    frame
  } else {
    rm(frame)
    rm(coes_frame)
    gc()
    invisible(list(
      dataset = "LFS",
      product = .llfs_product_name(),
      additional_products = .coes_product_name(),
      n_rows = n_rows,
      n_coes_rows = n_coes_rows,
      path = path,
      coes_path = coes_path
    ))
  }
}

.llfs_product_name <- function() {
  "pmp-llfs"
}

.coes_product_name <- function() {
  "pmp-coes"
}

.llfs_extdata_path <- function(filename) {
  path <- system.file("extdata", "llfs", filename, package = "fplida")
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "llfs", filename)
  }
  path
}

.llfs_variables <- function() {
  utils::read.csv(.llfs_extdata_path("llfs_variables.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE)
}

.llfs_values <- function() {
  utils::read.csv(.llfs_extdata_path("llfs_values.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE,
                  colClasses = c(
                    identifier = "character",
                    code = "character",
                    value_label = "character",
                    frequency_note = "character",
                    sheet = "character",
                    source_row = "integer",
                    value_order = "integer",
                    is_placeholder = "character",
                    source_workbook = "character"
                  ))
}

.coes_variables <- function() {
  utils::read.csv(.llfs_extdata_path("coes_variables.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE)
}

.coes_values <- function() {
  utils::read.csv(.llfs_extdata_path("coes_values.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE,
                  colClasses = c(
                    identifier = "character",
                    code = "character",
                    value_label = "character",
                    frequency_note = "character",
                    sheet = "character",
                    source_row = "integer",
                    value_order = "integer",
                    is_placeholder = "character",
                    source_workbook = "character"
                  ))
}

.project_llfs <- function(spine, specs, values, seed,
                          sample_household_rate, max_households,
                          survey_year) {
  if (!nrow(spine)) {
    return(.empty_llfs_frame(specs))
  }

  age_ref <- as.integer(survey_year - spine$birth_year)
  eligible_idx <- which(!is.na(age_ref) & age_ref >= 15L)
  if (!length(eligible_idx)) {
    return(.empty_llfs_frame(specs))
  }

  eligible <- spine[eligible_idx, , drop = FALSE]
  hh_ids <- sort(unique(as.character(eligible$household_id)))
  n_households <- length(hh_ids)
  target_households <- min(
    max_households,
    n_households,
    max(1L, as.integer(ceiling(n_households * sample_household_rate)))
  )

  set.seed(seed + 66020L)
  selected_hh <- sort(sample(hh_ids, target_households, replace = FALSE))

  selected <- eligible[as.character(eligible$household_id) %in% selected_hh,
                       , drop = FALSE]
  selected <- selected[order(as.character(selected$household_id),
                             selected$spine_id), , drop = FALSE]
  person_idx <- seq_len(nrow(selected))
  hh_match <- match(as.character(selected$household_id), selected_hh)
  person_seq <- ave(person_idx, as.character(selected$household_id),
                    FUN = seq_along)

  hh_meta <- .llfs_household_metadata(
    spine = spine,
    selected_hh = selected_hh,
    survey_year = survey_year,
    seed = seed
  )

  max_wave <- hh_meta$max_wave[hh_match]
  row_person_idx <- rep(person_idx, times = max_wave)
  response_num <- unlist(lapply(max_wave, seq_len), use.names = FALSE)
  row_hh_match <- hh_match[row_person_idx]

  rows <- selected[row_person_idx, , drop = FALSE]
  first_month <- hh_meta$first_month[row_hh_match]
  month_index <- first_month + response_num - 1L
  survey_year_row <- survey_year + (month_index - 1L) %/% 12L
  survey_month <- ((month_index - 1L) %% 12L) + 1L
  age <- as.integer(survey_year_row - rows$birth_year)
  first_ym <- survey_year * 100L + hh_meta$first_month
  abs_hid <- sprintf("LLFS%02d%02d%05d",
                     survey_year %% 100L, first_month,
                     hh_meta$household_seq[row_hh_match])
  abs_pid <- sprintf("%02d", person_seq[row_person_idx])
  abs_rid <- paste0(abs_hid, abs_pid)
  employed <- as.logical(rows$baseline_employed) & age >= 15L
  full_time <- employed & as.numeric(rows$baseline_hours) >= 35
  unemployed <- !employed & age < 65L &
    ((.llfs_row_key(rows$spine_id, seed, response_num) %% 100L) < 18L)
  lfstatus <- ifelse(employed, "1", ifelse(unemployed, "2", "3"))
  quarterly_month <- survey_month %in% c(2L, 5L, 8L, 11L)

  ctx <- list(
    rows = rows,
    specs = specs,
    values = values,
    age = age,
    age_ref = age_ref,
    selected_hh = selected_hh,
    hh_meta = hh_meta,
    hh_match = row_hh_match,
    person_seq = person_seq[row_person_idx],
    response_num = response_num,
    survey_year = survey_year_row,
    survey_month = survey_month,
    month_index = month_index,
    first_month = first_month,
    first_ym = first_ym[row_hh_match],
    abs_hid = abs_hid,
    abs_pid = abs_pid,
    abs_rid = abs_rid,
    employed = employed,
    full_time = full_time,
    unemployed = unemployed,
    lfstatus = lfstatus,
    quarterly_month = quarterly_month,
    seed = seed,
    n = length(row_person_idx)
  )

  value_map <- split(values[values$is_placeholder != "TRUE", , drop = FALSE],
                     values$identifier[values$is_placeholder != "TRUE"])
  out <- vector("list", nrow(specs))
  names(out) <- specs$identifier
  for (identifier in specs$identifier) {
    out[[identifier]] <- .llfs_generate_column(identifier, ctx, value_map)
  }

  quarterly_vars <- specs$identifier[specs$collection_cadence == "quarterly"]
  not_quarterly <- !quarterly_month
  for (identifier in quarterly_vars) {
    out[[identifier]][not_quarterly] <- NA
  }

  df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  df[specs$identifier]
}

.project_coes <- function(lfs, specs, values, seed, survey_year) {
  if (!nrow(lfs)) {
    return(.empty_llfs_frame(specs))
  }

  august_mid <- as.integer(survey_year * 100L + 8L)
  august_idx <- which(as.integer(lfs$ABSMID) == august_mid)
  if (!length(august_idx)) {
    return(.empty_llfs_frame(specs))
  }

  rows <- lfs[august_idx, , drop = FALSE]
  key <- .llfs_row_key(rows$SYNTHETIC_AEUID, seed + 6333L, rows$RESPNUM)

  # COE is collected from private dwellings in 7/8 of the August LFS sample.
  keep <- key %% 8L != 0L
  rows <- rows[keep, , drop = FALSE]
  key <- key[keep]
  if (!nrow(rows)) {
    return(.empty_llfs_frame(specs))
  }

  ctx <- list(
    rows = rows,
    key = key,
    seed = seed,
    survey_year = survey_year,
    n = nrow(rows),
    employed = as.character(rows$LFSTATUS) == "1",
    unemployed = as.character(rows$LFSTATUS) == "2",
    full_time = as.character(rows$FTPTEMP) == "1",
    multiple_jobs = as.character(rows$MULTJOB) == "21"
  )
  ctx$employee_main <- ctx$employed & .llfs_row_key(
    rows$SYNTHETIC_AEUID, seed + 7100L, rows$RESPNUM
  ) %% 100L < 78L
  ctx$employee_second <- ctx$multiple_jobs & .llfs_row_key(
    rows$SYNTHETIC_AEUID, seed + 7101L, rows$RESPNUM
  ) %% 100L < 65L

  value_map <- split(values[values$is_placeholder != "TRUE", , drop = FALSE],
                     values$identifier[values$is_placeholder != "TRUE"])
  out <- vector("list", nrow(specs))
  names(out) <- specs$identifier
  for (idx in seq_len(nrow(specs))) {
    identifier <- specs$identifier[idx]
    out[[identifier]] <- .coes_generate_column(
      identifier = identifier,
      spec = specs[idx, , drop = FALSE],
      ctx = ctx,
      value_map = value_map
    )
  }

  df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  df[specs$identifier]
}

.empty_llfs_frame <- function(specs) {
  out <- replicate(nrow(specs), logical(0), simplify = FALSE)
  names(out) <- specs$identifier
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

.llfs_household_metadata <- function(spine, selected_hh, survey_year, seed) {
  hh_all <- as.character(spine$household_id)
  age_ref <- as.integer(survey_year - spine$birth_year)
  n_15 <- as.integer(tabulate(match(hh_all[age_ref >= 15L], selected_hh),
                              nbins = length(selected_hh)))
  n_kid14 <- as.integer(tabulate(match(hh_all[age_ref < 15L], selected_hh),
                                 nbins = length(selected_hh)))
  n_kid04 <- as.integer(tabulate(match(hh_all[age_ref < 5L], selected_hh),
                                 nbins = length(selected_hh)))
  youngest <- rep(NA_integer_, length(selected_hh))
  child_rows <- which(age_ref < 15L & hh_all %in% selected_hh)
  if (length(child_rows)) {
    child_age <- age_ref[child_rows]
    child_hh <- match(hh_all[child_rows], selected_hh)
    youngest <- as.integer(tapply(child_age, child_hh, min)[
      as.character(seq_along(selected_hh))
    ])
  }

  row_key <- as.double(seq_along(selected_hh)) + as.double(seed) * 97
  first_month <- as.integer(((row_key * 7 + as.double(seed)) %% 12) + 1)
  set.seed(seed + 66120L)
  max_wave <- rep(8L, length(selected_hh))
  attrit <- stats::runif(length(selected_hh)) < 0.18
  if (any(attrit)) {
    max_wave[attrit] <- sample.int(7L, sum(attrit), replace = TRUE,
                                   prob = c(1, 1, 2, 2, 3, 3, 4))
  }

  data.frame(
    household_id = selected_hh,
    household_seq = seq_along(selected_hh),
    first_month = first_month,
    max_wave = max_wave,
    n_15 = pmax(n_15, 1L),
    n_kid14 = n_kid14,
    n_kid04 = n_kid04,
    youngest_child = youngest,
    stringsAsFactors = FALSE
  )
}

.llfs_generate_column <- function(identifier, ctx, value_map) {
  n <- ctx$n
  rows <- ctx$rows
  key <- .llfs_row_key(rows$spine_id, ctx$seed, ctx$response_num)

  switch(identifier,
    SYNTHETIC_AEUID = as.character(rows$aeuid_abs),
    ABSRID = ctx$abs_rid,
    ABSHID = ctx$abs_hid,
    ABSPID = ctx$abs_pid,
    ABSMID = as.integer(ctx$survey_year * 100L + ctx$survey_month),
    SURVYEAR = as.integer(ctx$survey_year),
    STRTDATE = as.Date(sprintf("%04d-%02d-01", ctx$survey_year,
                               ctx$survey_month)) + as.integer(key %% 21L),
    FRSTMTH = as.integer(ctx$first_ym),
    MNTHSEL = as.integer(ctx$response_num),
    NUMRESP = as.integer(ctx$hh_meta$max_wave[ctx$hh_match]),
    RESPNUM = as.integer(ctx$response_num),
    SAMPFRME = .llfs_from_codes(identifier, n, value_map,
                                default = "2021"),
    ROTGRP = as.integer(((ctx$first_month - 1L) %% 8L) + 1L),
    RESPTYPE = .llfs_binary_code(key, "01", "02", p = 0.72),
    URSTATUS = .llfs_from_codes(identifier, n, value_map, default = "1"),
    SUBPSRCE = .llfs_from_codes(identifier, n, value_map, default = "-1"),
    STATESEL = as.integer(rows$state),
    STATEUR = as.integer(rows$state),
    AREAUR = ifelse(as.integer(rows$state) %in% c(1L, 2L, 3L, 5L), "1", "2"),
    GCCSA = .llfs_capital_rest_code(rows$state, ctx$person_seq, nt_split = TRUE),
    REGNASGS = sprintf("%03d", as.integer(rows$sa4_code)),
    STATASGC = as.integer(rows$state),
    AREAASGC = ifelse(as.integer(rows$state) %in% c(1L, 2L, 3L, 5L), "1", "2"),
    CCBSASGC = .llfs_capital_rest_code(rows$state, ctx$person_seq,
                                        nt_split = FALSE),
    RG06ASGC = sprintf("%03d", as.integer(rows$sa4_code)),
    RG96ASGC = sprintf("%03d", as.integer(rows$sa4_code)),
    RG21ASGC = sprintf("%03d", as.integer(rows$sa4_code)),
    RGV9ASGC = sprintf("%03d", as.integer(rows$sa4_code)),
    RGV1ASGC = sprintf("%03d", as.integer(rows$sa4_code)),
    SEX = as.integer(rows$sex),
    AGE = as.integer(ctx$age),
    YRBIRTH = as.integer(rows$birth_year),
    COBMCG = .llfs_country_group(rows$country_of_birth_sacc),
    COBRC = ifelse(is.na(rows$country_of_birth_sacc),
                   NA_character_,
                   as.character(rows$country_of_birth_sacc)),
    DECARR = .llfs_decade(rows$year_of_arrival,
                          rows$country_of_birth_sacc),
    YRARR = as.integer(rows$year_of_arrival),
    ELAPYRAR = ifelse(is.na(rows$year_of_arrival), NA_integer_,
                      pmax(0L, ctx$survey_year - as.integer(rows$year_of_arrival))),
    SOCMAR = .llfs_binary_code(key, "1", "2", p = 0.55),
    LFSFREL = .llfs_household_relationship(ctx),
    NUMFAMH = .llfs_count_code(pmax(1L, as.integer(ceiling(
      ctx$hh_meta$n_15[ctx$hh_match] / 3)))),
    NPER15H = .llfs_count_code(ctx$hh_meta$n_15[ctx$hh_match]),
    NKID14H = .llfs_count_code(ctx$hh_meta$n_kid14[ctx$hh_match]),
    NKID04H = .llfs_count_code(ctx$hh_meta$n_kid04[ctx$hh_match]),
    AGYNG14H = .llfs_child_age_code(ctx$hh_meta$youngest_child[ctx$hh_match]),
    FAMNUM = .llfs_count_code(pmax(1L, as.integer(ceiling(
      ctx$person_seq / 3)))),
    NPER15F = .llfs_count_code(pmax(1L, pmin(
      ctx$hh_meta$n_15[ctx$hh_match], 5L))),
    NKID14F = .llfs_count_code(ctx$hh_meta$n_kid14[ctx$hh_match]),
    NKID04F = .llfs_count_code(ctx$hh_meta$n_kid04[ctx$hh_match]),
    AGYNG14F = .llfs_child_age_code(ctx$hh_meta$youngest_child[ctx$hh_match]),
    EDUCSTAT = .llfs_educstat(ctx),
    YRSCHOOL = .llfs_year_school(ctx),
    HIGHYEAR = .llfs_highyear(ctx),
    EDUCATT = .llfs_educatt(ctx),
    HIQ01LVL = .llfs_education_level(rows$education),
    HIQ93LVL = .llfs_education_level_1993(rows$education),
    HIQXXLVL = .llfs_education_level(rows$education),
    NSQ01LVL = .llfs_education_level(rows$education),
    NSQ01FLD = .llfs_from_codes(identifier, n, value_map, key = key),
    HIQ93FLD = .llfs_from_codes(identifier, n, value_map, key = key),
    YRCOMNSQ = as.integer(pmin(ctx$survey_year - 1L,
                               pmax(rows$birth_year + 18L,
                                    ctx$survey_year - (key %% 35L)))),
    LFSTATUS = ctx$lfstatus,
    FTPTEMP = ifelse(ctx$employed, ifelse(ctx$full_time, "1", "2"), "0"),
    STATEMP = .llfs_status_employment(ctx, key),
    MULTJOB = ifelse(ctx$employed,
                     ifelse(key %% 100L < 8L, "21", "11"), "00"),
    WORKMON = .llfs_workday(ctx, 1L),
    WORKTUE = .llfs_workday(ctx, 2L),
    WORKWED = .llfs_workday(ctx, 3L),
    WORKTHU = .llfs_workday(ctx, 4L),
    WORKFRI = .llfs_workday(ctx, 5L),
    WORKSAT = .llfs_workday(ctx, 6L),
    WORKSUN = .llfs_workday(ctx, 7L),
    TENURMTH = .llfs_tenure_months(ctx),
    TENUREYR = .llfs_tenure_years(ctx),
    AWAYWORK = ifelse(ctx$employed & key %% 100L < 6L, "21", "-1"),
    EXPECT = ifelse(ctx$employed,
                    .llfs_from_codes(identifier, n, value_map, key), "00"),
    ACTFTPT = ifelse(ctx$employed, ifelse(.llfs_hours(ctx) >= 35, "1", "2"), "0"),
    HRSWORK = ifelse(ctx$employed, .llfs_hours(ctx), 0),
    USLFTPT = ifelse(ctx$employed, ifelse(as.numeric(rows$baseline_hours) >= 35, "1", "2"), "0"),
    HRUWAJ = ifelse(ctx$employed, pmax(1, as.integer(rows$baseline_hours)), 0),
    HRAWMJ = ifelse(ctx$employed, pmax(0, .llfs_hours(ctx) - ifelse(key %% 100L < 12L, 2L, 0L)), 0),
    HRUWMJ = ifelse(ctx$employed, pmax(1, as.integer(rows$baseline_hours)), 0),
    UNDREMP = .llfs_underemployed(ctx, expanded = FALSE),
    UNDREMEX = .llfs_underemployed(ctx, expanded = TRUE),
    REASLTFT = ifelse(ctx$employed & ctx$full_time & .llfs_hours(ctx) < 35,
                      .llfs_from_codes(identifier, n, value_map, key), "0"),
    MOREHRPT = ifelse(ctx$employed & !ctx$full_time,
                      .llfs_binary_code(key, "1", "2", p = 0.32), "0"),
    PREFFTPT = ifelse(ctx$employed & !ctx$full_time,
                      .llfs_binary_code(key, "1", "2", p = 0.35), "0"),
    PREFHRPT = ifelse(ctx$employed & !ctx$full_time,
                      pmax(as.integer(rows$baseline_hours) + 5L, 20L), 0),
    REASLTUH = ifelse(ctx$employed & .llfs_hours(ctx) < pmax(1, rows$baseline_hours),
                      .llfs_from_codes(identifier, n, value_map, key), "00"),
    MOREHREX = ifelse(ctx$employed,
                      .llfs_binary_code(key, "1", "2", p = 0.25), "0"),
    PRFFTPTX = ifelse(ctx$employed,
                      .llfs_binary_code(key, "1", "2", p = 0.30), "0"),
    PREFHREX = ifelse(ctx$employed, pmax(.llfs_hours(ctx) + 4L, 20L), 0),
    PREFAVAI = ifelse(ctx$employed,
                      .llfs_from_codes(identifier, n, value_map, key), "00"),
    IND06DIV = .llfs_industry_div(rows$industry, 19L),
    IND06GRP = .llfs_from_codes(identifier, n, value_map, key),
    IND93DIV = .llfs_industry_div(rows$industry, 17L),
    IND93GRP = .llfs_from_codes(identifier, n, value_map, key),
    IND83DIV = .llfs_industry_div(rows$industry, 12L),
    IND83GRP = .llfs_from_codes(identifier, n, value_map, key),
    IND78DIV = .llfs_industry_div(rows$industry, 12L),
    IND78GRP = .llfs_from_codes(identifier, n, value_map, key),
    OCC13MGR = sprintf("%02d", pmax(1L, as.integer(rows$anzsco_major))),
    OCC13SKL = sprintf("%02d", pmax(1L, as.integer(rows$skill_level))),
    OCC13OCC = sprintf("%06d", pmax(0L, as.integer(rows$anzsco_code))),
    OCCSEMGR = sprintf("%02d", pmax(1L, as.integer(rows$anzsco_major))),
    OCCSESKL = sprintf("%02d", pmax(1L, as.integer(rows$skill_level))),
    OCCSEUGP = .llfs_from_codes(identifier, n, value_map, key),
    OCCFEMGR = sprintf("%02d", pmax(1L, as.integer(rows$anzsco_major))),
    OCCFEUGP = .llfs_from_codes(identifier, n, value_map, key),
    OCC76MGR = sprintf("%02d", pmax(1L, as.integer(rows$anzsco_major))),
    OCC76OCC = .llfs_from_codes(identifier, n, value_map, key),
    FTPTUN = ifelse(ctx$unemployed,
                    .llfs_binary_code(key, "2", "3", p = 0.72), "0"),
    LJOBSTAT = ifelse(ctx$unemployed,
                      .llfs_from_codes(identifier, n, value_map, key), "00"),
    REASLLJ = ifelse(ctx$unemployed,
                     .llfs_from_codes(identifier, n, value_map, key), "0000"),
    DURJSCH = ifelse(ctx$unemployed, as.integer((key %% 80L) + 1L), 0L),
    DURUNFT = ifelse(ctx$unemployed, as.integer((key %% 156L) + 1L), 0L),
    NWKSLJOB = ifelse(ctx$unemployed | !ctx$employed,
                      as.integer((key %% 260L) + 1L), 0L),
    NWKSLFTJ = ifelse(ctx$unemployed | !ctx$employed,
                      as.integer((key %% 520L) + 1L), 0L),
    NILFSTAT = ifelse(ctx$lfstatus == "3",
                      .llfs_from_codes(identifier, n, value_map, key), "00"),
    NUMLLJ3M = ifelse(key %% 100L < 8L, "01", "00"),
    RTRNCH3M = ifelse(key %% 100L < 5L, "11", "22"),
    RSLLJ3MA = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3MB = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3MC = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3MD = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3ME = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3MF = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3MG = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3MH = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3MI = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3MJ = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3MK = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3ML = .llfs_retrench_reason(ctx, identifier, value_map),
    RSLLJ3MM = .llfs_retrench_reason(ctx, identifier, value_map),
    POPSEX = as.integer(rows$sex),
    POPAGEGR = .llfs_age_group(ctx$age),
    POPREGN = .llfs_capital_rest_code(rows$state, ctx$person_seq,
                                       nt_split = FALSE),
    POPCOUNT = as.integer(round(.llfs_weight(ctx) * 1000)),
    PREVWT = round(.llfs_weight(ctx) * (0.98 + (key %% 40L) / 1000), 8),
    WEIGHT = .llfs_weight(ctx),
    REPWTG01 = .llfs_replicate_weight(ctx, 1L),
    REPWTG02 = .llfs_replicate_weight(ctx, 2L),
    REPWTG03 = .llfs_replicate_weight(ctx, 3L),
    REPWTG04 = .llfs_replicate_weight(ctx, 4L),
    REPWTG05 = .llfs_replicate_weight(ctx, 5L),
    REPWTG06 = .llfs_replicate_weight(ctx, 6L),
    REPWTG07 = .llfs_replicate_weight(ctx, 7L),
    REPWTG08 = .llfs_replicate_weight(ctx, 8L),
    REPWTG09 = .llfs_replicate_weight(ctx, 9L),
    REPWTG10 = .llfs_replicate_weight(ctx, 10L),
    REPWTG11 = .llfs_replicate_weight(ctx, 11L),
    REPWTG12 = .llfs_replicate_weight(ctx, 12L),
    REPWTG13 = .llfs_replicate_weight(ctx, 13L),
    REPWTG14 = .llfs_replicate_weight(ctx, 14L),
    REPWTG15 = .llfs_replicate_weight(ctx, 15L),
    REPWTG16 = .llfs_replicate_weight(ctx, 16L),
    REPWTG17 = .llfs_replicate_weight(ctx, 17L),
    REPWTG18 = .llfs_replicate_weight(ctx, 18L),
    REPWTG19 = .llfs_replicate_weight(ctx, 19L),
    REPWTG20 = .llfs_replicate_weight(ctx, 20L),
    REPWTG21 = .llfs_replicate_weight(ctx, 21L),
    REPWTG22 = .llfs_replicate_weight(ctx, 22L),
    REPWTG23 = .llfs_replicate_weight(ctx, 23L),
    REPWTG24 = .llfs_replicate_weight(ctx, 24L),
    REPWTG25 = .llfs_replicate_weight(ctx, 25L),
    REPWTG26 = .llfs_replicate_weight(ctx, 26L),
    REPWTG27 = .llfs_replicate_weight(ctx, 27L),
    REPWTG28 = .llfs_replicate_weight(ctx, 28L),
    REPWTG29 = .llfs_replicate_weight(ctx, 29L),
    REPWTG30 = .llfs_replicate_weight(ctx, 30L),
    .llfs_from_codes(identifier, n, value_map, key)
  )
}

.coes_generate_column <- function(identifier, spec, ctx, value_map) {
  n <- ctx$n
  rows <- ctx$rows

  if (.coes_should_not_collect(identifier, spec$frequency, ctx$survey_year,
                               value_map)) {
    return(.coes_not_collected_value(identifier, n, value_map))
  }

  if (grepl("^WPX02[0-9]{2}$", identifier)) {
    return(.coes_replicate_weight(ctx, as.integer(sub("^WPX02", "", identifier))))
  }

  switch(identifier,
    SYNTHETIC_AEUID = as.character(rows$SYNTHETIC_AEUID),
    ABSRID = as.character(rows$ABSRID),
    ABSMID = as.integer(rows$ABSMID),
    AGES = sprintf("%03d", pmin(120L, pmax(15L, as.integer(rows$AGE)))),
    ELAPSEDB = as.integer(pmax(0, .coes_num(rows$ELAPYRAR))),
    ELAPSEDA = ifelse(is.na(.coes_num(rows$ELAPYRAR)), "-9", "1"),
    ASGSSA4S = as.character(rows$REGNASGS),
    SEXS = as.integer(rows$SEX),
    YRBIRTH3 = as.integer(rows$YRBIRTH),
    AGEYNGFB = as.character(rows$AGYNG14F),
    AGEYNGFA = ifelse(as.character(rows$AGYNG14F) == "-9", "-1", "1"),
    AGEYNGHB = as.character(rows$AGYNG14H),
    AGEYNGHA = ifelse(as.character(rows$AGYNG14H) == "-9", "-1", "1"),
    NUMKD14F = as.character(rows$NKID14F),
    NUMKD14H = as.character(rows$NKID14H),
    NUMDP24F = .llfs_count_code(.coes_num(rows$NKID14F) +
                                  pmax(0L, 24L - as.integer(rows$AGE)) %/% 10L),
    NUMDP24H = .llfs_count_code(.coes_num(rows$NKID14H) +
                                  pmax(0L, 24L - as.integer(rows$AGE)) %/% 10L),
    MARSTATS = as.character(rows$SOCMAR),
    HIGHYR = as.character(rows$HIGHYEAR),
    ASCEDLVL = as.character(rows$HIQ01LVL),
    ASCEDLVS = as.character(rows$NSQ01LVL),
    MFHNSQ = as.character(rows$NSQ01FLD),
    FTPTEMPS = as.character(rows$FTPTEMP),
    HRUWAJB = as.integer(.coes_num(rows$HRUWAJ)),
    LFSTATUS = as.character(rows$LFSTATUS),
    HRAWMJB = as.integer(.coes_num(rows$HRAWMJ)),
    HRAWMJA = .coes_param_flag(.coes_num(rows$HRAWMJ), ctx$employed),
    DUREMPYB = as.integer(.coes_num(rows$TENUREYR)),
    DUREMPYA = .coes_param_flag(.coes_num(rows$TENUREYR), ctx$employed),
    DUREMPMB = as.integer(.coes_num(rows$TENURMTH)),
    DUREMPMA = .coes_param_flag(.coes_num(rows$TENURMTH), ctx$employed),
    FTPTMAIN = as.character(rows$FTPTEMP),
    SKILLMJ = as.character(rows$OCC13SKL),
    MUSLHRSB = as.integer(.coes_num(rows$HRUWMJ)),
    MUSLHRSA = .coes_param_flag(.coes_num(rows$HRUWMJ), ctx$employed),
    WKDHRSA = .coes_param_flag(.coes_num(rows$HRSWORK), ctx$employed),
    WKDHRSB = as.integer(.coes_num(rows$HRSWORK)),
    WKPYAJBR = .coes_earnings_range(ctx, narrow = FALSE),
    WKPYAJNR = .coes_earnings_range(ctx, narrow = TRUE),
    HRLYRMJB = .coes_hourly_earnings(ctx, second = FALSE),
    HRLYRMJA = .coes_param_flag(.coes_hourly_earnings(ctx, second = FALSE),
                                ctx$employee_main),
    HRLYRSJB = .coes_hourly_earnings(ctx, second = TRUE),
    HRLYRSJA = .coes_param_flag(.coes_hourly_earnings(ctx, second = TRUE),
                                ctx$employee_second),
    WKPAYMJB = .coes_weekly_earnings(ctx, second = FALSE),
    WKPAYMJA = .coes_param_flag(.coes_weekly_earnings(ctx, second = FALSE),
                                ctx$employee_main),
    WKPAYSJB = .coes_weekly_earnings(ctx, second = TRUE),
    WKPAYSJA = .coes_param_flag(.coes_weekly_earnings(ctx, second = TRUE),
                                ctx$employee_second),
    WKPAYAJB = .coes_weekly_earnings(ctx, both = TRUE),
    WKPAYAJA = .coes_param_flag(.coes_weekly_earnings(ctx, both = TRUE),
                                ctx$employee_main | ctx$employee_second),
    COEPOP1 = .coes_population_code(identifier, ctx),
    COEPOP2 = .coes_population_code(identifier, ctx),
    COEPOP3 = .coes_population_code(identifier, ctx),
    COEPOP4 = .coes_population_code(identifier, ctx),
    COEPOP5 = .coes_population_code(identifier, ctx),
    COEPOP6 = .coes_population_code(identifier, ctx),
    COEPOP7 = .coes_population_code(identifier, ctx),
    COEPOP8 = .coes_population_code(identifier, ctx),
    COEPOP9 = .coes_population_code(identifier, ctx),
    COEPOP10 = .coes_population_code(identifier, ctx),
    COEPOP11 = .coes_population_code(identifier, ctx),
    COEPOP12 = .coes_population_code(identifier, ctx),
    COEPOP13 = .coes_population_code(identifier, ctx),
    COEPOP14 = .coes_population_code(identifier, ctx),
    FINPRSWT = round(.coes_num(rows$WEIGHT) * 1.07, 8),
    .coes_from_codes(identifier, ctx, value_map)
  )
}

.coes_from_codes <- function(identifier, ctx, value_map,
                             default = NA_character_) {
  .llfs_from_codes(identifier, ctx$n, value_map, ctx$key, default = default)
}

.coes_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

.coes_param_flag <- function(value, in_scope) {
  ifelse(in_scope, ifelse(is.na(value), "-9", "1"), "0")
}

.coes_hourly_earnings <- function(ctx, second = FALSE) {
  base <- 24 + (ctx$key %% 7000L) / 100
  if (second) base <- base * 0.85
  round(ifelse(if (second) ctx$employee_second else ctx$employee_main,
               base, 0), 2)
}

.coes_weekly_earnings <- function(ctx, second = FALSE, both = FALSE) {
  main_hours <- pmax(1, .coes_num(ctx$rows$HRAWMJ))
  main <- round(main_hours * .coes_hourly_earnings(ctx, second = FALSE))
  second_hours <- 4 + (ctx$key %% 18L)
  second_pay <- round(second_hours * .coes_hourly_earnings(ctx, second = TRUE))
  if (both) return(main + second_pay)
  if (second) return(second_pay)
  main
}

.coes_earnings_range <- function(ctx, narrow = FALSE) {
  weekly <- .coes_weekly_earnings(ctx, both = TRUE)
  breaks <- if (narrow) {
    c(0, 200, 400, 600, 800, 1000, 1200, 1500, 2000, Inf)
  } else {
    c(0, 500, 1000, 1500, 2000, Inf)
  }
  code <- findInterval(weekly, breaks, rightmost.closed = TRUE)
  sprintf("%02d", pmax(1L, code))
}

.coes_population_code <- function(identifier, ctx) {
  in_population <- switch(identifier,
    COEPOP1 = ctx$employed,
    COEPOP2 = ctx$employee_main,
    COEPOP3 = ctx$employee_main | (ctx$employed & !ctx$employee_main),
    COEPOP4 = ctx$employee_main | (ctx$employed & !ctx$employee_main),
    COEPOP5 = ctx$employed & !ctx$employee_main,
    COEPOP6 = ctx$employed & as.character(ctx$rows$UNDREMP) %in% c("02", "23"),
    COEPOP7 = ctx$employed & ctx$key %% 100L < 9L,
    COEPOP8 = ctx$multiple_jobs,
    COEPOP9 = ctx$employee_second,
    COEPOP10 = ctx$multiple_jobs,
    COEPOP11 = ctx$employee_main & ctx$key %% 100L < 13L,
    COEPOP12 = ctx$employed & ctx$key %% 100L < 14L,
    COEPOP13 = ctx$employed & ctx$key %% 100L < 6L,
    COEPOP14 = ctx$employed & ctx$key %% 100L < 5L,
    rep(FALSE, ctx$n)
  )
  ifelse(in_population, "1", ifelse(ctx$employed, "0", "-1"))
}

.coes_replicate_weight <- function(ctx, idx) {
  key <- .llfs_row_key(ctx$rows$SYNTHETIC_AEUID, ctx$seed + 8000L + idx,
                       ctx$rows$RESPNUM)
  multiplier <- 0.935 + (key %% 130L) / 1000
  round(.coes_num(ctx$rows$WEIGHT) * 1.07 * multiplier, 8)
}

.coes_should_not_collect <- function(identifier, frequency, survey_year,
                                     value_map) {
  if (!nzchar(frequency)) return(FALSE)
  lower <- tolower(frequency)
  if (grepl("2014-2024", lower, fixed = TRUE) && survey_year > 2024L) {
    return(TRUE)
  }
  if (grepl("2024-", lower, fixed = TRUE) && survey_year < 2024L) {
    return(TRUE)
  }
  if (!grepl("biennial", lower, fixed = TRUE)) {
    return(FALSE)
  }

  vals <- value_map[[identifier]]
  if (is.null(vals) || !nrow(vals)) return(FALSE)
  text <- tolower(paste(vals$value_label, vals$frequency_note, collapse = " "))
  even_only <- grepl("even years only", text, fixed = TRUE)
  odd_only <- grepl("odd years only", text, fixed = TRUE)
  if (even_only && !odd_only && survey_year %% 2L == 1L) return(TRUE)
  if (odd_only && !even_only && survey_year %% 2L == 0L) return(TRUE)
  FALSE
}

.coes_not_collected_value <- function(identifier, n, value_map) {
  vals <- value_map[[identifier]]
  if (!is.null(vals) && nrow(vals)) {
    text <- paste(vals$value_label, vals$frequency_note)
    idx <- grepl("not collected", text, ignore.case = TRUE)
    if (any(idx)) return(rep(vals$code[which(idx)[1]], n))
    if ("-9" %in% vals$code) return(rep("-9", n))
  }
  rep(NA_character_, n)
}

.llfs_row_key <- function(spine_id, seed, response_num = 1L) {
  raw <- gsub("\\D", "", as.character(spine_id))
  num <- suppressWarnings(as.numeric(substr(raw, pmax(1, nchar(raw) - 8L),
                                           nchar(raw))))
  num[is.na(num)] <- seq_along(num)[is.na(num)]
  as.integer(abs((num * 1103515245 + seed * 1009 + response_num * 9176) %%
                   2147483647))
}

.llfs_codes <- function(identifier, value_map) {
  vals <- value_map[[identifier]]
  if (is.null(vals) || !nrow(vals)) return(character(0))
  codes <- vals$code
  codes <- codes[nzchar(codes) & codes != "..."]
  unique(codes)
}

.llfs_from_codes <- function(identifier, n, value_map, key = NULL,
                             default = NA_character_) {
  codes <- .llfs_codes(identifier, value_map)
  if (!length(codes)) return(rep(default, n))
  if (is.null(key)) key <- seq_len(n)
  codes[as.integer(key %% length(codes)) + 1L]
}

.llfs_binary_code <- function(key, yes, no, p = 0.5) {
  ifelse((key %% 10000L) < as.integer(p * 10000), yes, no)
}

.llfs_capital_rest_code <- function(state, person_seq, nt_split = TRUE) {
  state <- as.integer(state)
  capital <- as.integer(person_seq) %% 2L == 1L
  out <- ifelse(capital, sprintf("%d1", state), sprintf("%d9", state))
  out[state == 7L] <- if (nt_split) ifelse(capital[state == 7L], "71", "79") else "70"
  out[state == 8L] <- if (nt_split) "81" else "80"
  out[is.na(state)] <- "00"
  out
}

.llfs_country_group <- function(sacc) {
  sacc <- suppressWarnings(as.integer(sacc))
  out <- ifelse(is.na(sacc), "0003",
                ifelse(sacc == 1101L, "1100",
                       paste0(substr(sprintf("%04d", sacc), 1L, 1L), "000")))
  out[!out %in% c("0000", "0003", paste0(1:9, "000"), "1100")] <- "0000"
  out
}

.llfs_decade <- function(year, sacc = NULL) {
  year <- suppressWarnings(as.integer(year))
  out <- rep("00", length(year))
  if (!is.null(sacc)) {
    aus <- suppressWarnings(as.integer(sacc)) == 1101L
    out[aus & !is.na(aus)] <- "01"
  }
  overseas <- out != "01" & !is.na(year)
  out[overseas & year < 1950L] <- "02"
  out[overseas & year >= 1950L & year < 1960L] <- "03"
  out[overseas & year >= 1960L & year < 1970L] <- "04"
  out[overseas & year >= 1970L & year < 1980L] <- "05"
  out[overseas & year >= 1980L & year < 1990L] <- "06"
  out[overseas & year >= 1990L & year < 2000L] <- "07"
  out[overseas & year >= 2000L & year < 2010L] <- "08"
  out[overseas & year >= 2010L & year < 2020L] <- "09"
  out[overseas & year >= 2020L] <- "10"
  out
}

.llfs_household_relationship <- function(ctx) {
  rel <- rep("010", ctx$n)
  rel[ctx$person_seq == 2L] <- "020"
  rel[ctx$person_seq > 2L & ctx$age < 25L] <- "030"
  rel[ctx$person_seq > 2L & ctx$age >= 25L] <- "090"
  rel
}

.llfs_count_code <- function(x, max_value = 99L) {
  x <- pmax(0L, pmin(max_value, suppressWarnings(as.integer(x))))
  sprintf("%02d", x)
}

.llfs_child_age_code <- function(age) {
  out <- .llfs_count_code(age, max_value = 14L)
  out[is.na(age)] <- "-9"
  out
}

.llfs_educstat <- function(ctx) {
  out <- rep("00", ctx$n)
  young <- ctx$age >= 15L & ctx$age <= 24L
  out[young & ctx$age <= 19L & ctx$rows$education < 3L] <- "03"
  out[young & ctx$age <= 19L & ctx$rows$education >= 3L] <- "02"
  out[young & ctx$age > 19L & ctx$rows$education >= 4L] <- "05"
  out[young & ctx$age > 19L & ctx$rows$education < 4L] <- "04"
  out
}

.llfs_year_school <- function(ctx) {
  ifelse(ctx$age >= 15L & ctx$age <= 19L,
         pmax(7L, pmin(12L, ctx$age - 5L)),
         NA_integer_)
}

.llfs_highyear <- function(ctx) {
  codes <- c("623", "622", "621", "613", "611", "611", "611")
  idx <- pmax(1L, pmin(length(codes), as.integer(ctx$rows$education) + 1L))
  codes[idx]
}

.llfs_educatt <- function(ctx) {
  out <- rep("00", ctx$n)
  out[ctx$age >= 15L & ctx$age <= 19L & ctx$rows$education < 3L] <- "05"
  out[ctx$age >= 15L & ctx$age <= 24L & ctx$rows$education >= 3L] <- "02"
  out
}

.llfs_education_level <- function(education) {
  codes <- c("623", "621", "611", "514", "421", "312", "121")
  idx <- pmax(1L, pmin(length(codes), as.integer(education) + 1L))
  codes[idx]
}

.llfs_education_level_1993 <- function(education) {
  codes <- c("05", "04", "03", "02", "01", "01", "01")
  idx <- pmax(1L, pmin(length(codes), as.integer(education) + 1L))
  codes[idx]
}

.llfs_workday <- function(ctx, day) {
  key <- .llfs_row_key(ctx$rows$spine_id, ctx$seed + day, ctx$response_num)
  ifelse(ctx$employed,
         ifelse(key %% 100L < ifelse(day <= 5L, 86L, 18L), "01", "02"),
         "00")
}

.llfs_hours <- function(ctx) {
  base <- pmax(0, as.integer(ctx$rows$baseline_hours))
  delta <- as.integer((.llfs_row_key(ctx$rows$spine_id, ctx$seed + 10L,
                                     ctx$response_num) %% 9L) - 4L)
  pmax(0L, pmin(80L, base + delta))
}

.llfs_tenure_months <- function(ctx) {
  key <- .llfs_row_key(ctx$rows$spine_id, ctx$seed + 20L, ctx$response_num)
  ifelse(ctx$employed, sprintf("%02d", as.integer(key %% 12L) + 1L), "00")
}

.llfs_tenure_years <- function(ctx) {
  key <- .llfs_row_key(ctx$rows$spine_id, ctx$seed + 21L, ctx$response_num)
  years <- pmin(20L, as.integer(key %% 20L) + 1L)
  ifelse(ctx$employed, sprintf("%04d", years), "0000")
}

.llfs_underemployed <- function(ctx, expanded = FALSE) {
  key <- .llfs_row_key(ctx$rows$spine_id, ctx$seed + if (expanded) 31L else 30L,
                       ctx$response_num)
  under <- ctx$employed & !ctx$full_time & key %% 100L < 24L
  out <- rep("00", ctx$n)
  out[under] <- if (expanded) "23" else "02"
  out
}

.llfs_status_employment <- function(ctx, key) {
  out <- rep("00", ctx$n)
  out[ctx$employed & key %% 100L < 65L] <- "11"
  out[ctx$employed & key %% 100L >= 65L & key %% 100L < 85L] <- "12"
  out[ctx$employed & key %% 100L >= 85L & key %% 100L < 94L] <- "21"
  out[ctx$employed & key %% 100L >= 94L] <- "32"
  out
}

.llfs_industry_div <- function(industry, max_div = 19L) {
  sprintf("%02d", pmax(1L, pmin(max_div, as.integer(industry))))
}

.llfs_retrench_reason <- function(ctx, identifier, value_map) {
  key <- .llfs_row_key(ctx$rows$spine_id, ctx$seed + .stable_name_seed(identifier),
                       ctx$response_num)
  ifelse(key %% 100L < 4L,
         .llfs_from_codes(identifier, ctx$n, value_map, key),
         "00")
}

.llfs_age_group <- function(age) {
  age <- as.integer(age)
  out <- rep("6599", length(age))
  out[age >= 15L & age <= 19L] <- "1519"
  out[age >= 20L & age <= 24L] <- "2024"
  out[age >= 25L & age <= 34L] <- "2534"
  out[age >= 35L & age <= 44L] <- "3544"
  out[age >= 45L & age <= 54L] <- "4554"
  out[age >= 55L & age <= 59L] <- "5559"
  out[age >= 60L & age <= 64L] <- "6064"
  out
}

.llfs_weight <- function(ctx) {
  key <- .llfs_row_key(ctx$rows$spine_id, ctx$seed + 40L, ctx$response_num)
  round(80 + (key %% 2600L) / 1.1, 8)
}

.llfs_replicate_weight <- function(ctx, idx) {
  key <- .llfs_row_key(ctx$rows$spine_id, ctx$seed + 100L + idx,
                       ctx$response_num)
  multiplier <- 0.94 + (key %% 120L) / 1000
  round(.llfs_weight(ctx) * multiplier, 8)
}
