#' Generate simulated 2021 Census data
#'
#' Projects Census 2021 person, family, and dwelling tables from the fplida
#' spine. Person-level records are derived from the spine's demographics,
#' education, occupation, and income attributes; family and dwelling records
#' are derived from the linked people; and Census-specific columns (marital
#' status, language, health conditions, etc.) are generated with age- and
#' occupation-conditioned distributions.
#'
#' @section Dataset and variable information:
#' The [ABS Census microdata](https://www.abs.gov.au/statistics/microdata-tablebuilder/available-microdata-tablebuilder/census-population-and-housing)
#' website gives information about this dataset. Use `dataset_info("CENSUS")`
#' for dataset information. Use `variable_info("CENSUS")` for variables,
#' sources, value support, and topic tags.
#'
#' @param spine Data.frame (from \code{generate_spine()}) or NULL. If NULL,
#'   the most recent spine RDS is loaded from \code{get_data_path()}.
#' @param seed Integer. Random seed for Census-specific column generation.
#' @param output_dir Character or NULL. Base output directory. If NULL, uses
#'   \code{get_data_path()}. One of the two must be set.
#' @param format Character. Output format: "parquet" (default) or "csv".
#' @param return_data Logical. If TRUE (default), return data.frames in memory;
#'   if FALSE, write to disk and return metadata only.
#'
#' @return A named list with three data.frames:
#'   \describe{
#'     \item{person}{Person-level records (census_2021_person)}
#'     \item{family}{Family-level records (census_2021_family)}
#'     \item{dwelling}{Dwelling-level records (census_2021_dwelling)}
#'   }
#'
#' @examples
#' \dontrun{
#' spine <- generate_spine(n = 1000L, seed = 1L)
#' census <- generate_census(spine = spine, seed = 1L)
#' str(census$person)
#' }
#'
#' @export
generate_census <- function(spine = NULL, seed = 42L, output_dir = NULL,
                            format = c("parquet", "csv"),
                            return_data = TRUE) {
  seed <- as.integer(seed)
  format <- match.arg(format)
  return_data <- as.logical(return_data)
  stopifnot("`seed` must be an integer" = !is.na(seed))

  # Resolve run directory
  run_dir <- resolve_run_dir(output_dir)

  # ---- Selective spine loading (memory-efficient) ----
  census_cols <- c("spine_id", "aeuid_abs", "birth_year", "sex", "state",
                   "indigenous", "country_of_birth_sacc", "year_of_arrival",
                   "citizenship", "education", "baseline_employed",
                   "baseline_hours", "anzsco_code", "anzsco_major", "industry",
                   "task_physical", "sa2_code", "household_id",
                   "year_of_death", "month_of_death", "day_of_death",
                   "disability_onset_year", "disability_severity")
  spine_loaded <- is.null(spine)
  if (spine_loaded) {
    spine <- load_spine_select(run_dir, census_cols)
  }
  stopifnot("`spine` must be a data.frame" = is.data.frame(spine))

  missing_cols <- setdiff(census_cols, names(spine))
  if (length(missing_cols) > 0L) {
    stop("`spine` is missing required Census columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # The 2021 Census reference night was 10 August. People known to have died
  # before that date cannot appear in the person, family, or dwelling tables.
  spine <- spine[.census_present_on_night(spine), , drop = FALSE]

  n <- nrow(spine)

  # Mini spine for agency spine writing (spine_id + aeuid_abs only)
  mini_spine <- data.frame(
    spine_id  = spine$spine_id,
    aeuid_abs = spine$aeuid_abs,
    stringsAsFactors = FALSE
  )

  # Project person table from spine
  person <- project_census_person(spine, seed)

  # A spine household can contain people whose independently generated state
  # differs. Split those households by state so a Census dwelling never spans
  # states, while retaining each person's cross-product spine geography.
  links <- .census_household_links(spine)
  n_dwellings <- length(links$dwelling_id)
  n_families <- length(links$family_id)

  dwelling_raw <- generate_census_2021_dwelling__(as.integer(n_dwellings),
                                                   seed + 1L)
  family_raw <- generate_census_2021_family__(as.integer(n_families),
                                               seed + 2L)

  dwelling <- as.data.frame(dwelling_raw, stringsAsFactors = FALSE)
  family <- as.data.frame(family_raw, stringsAsFactors = FALSE)

  # Person links are the source of truth for every family/dwelling aggregate.
  dwelling$DWELLING_ID <- links$dwelling_id
  family$FAMILY_ID <- links$family_id
  person$DWELLING_ID <- links$person_dwelling_id
  person$FAMILY_ID <- links$person_family_id

  family <- .census_add_family_composition(family, person)
  dwelling <- .census_add_dwelling_values(dwelling, person)

  if (spine_loaded && !return_data) { rm(spine); gc() }

  # Write products
  write_product(person, census_product_name("person"), "CENSUS", run_dir, format)
  write_product(family, census_product_name("family"), "CENSUS", run_dir, format)
  write_product(dwelling, census_product_name("dwelling"), "CENSUS", run_dir, format)

  # Write ABS agency spine
  ds_dir <- dataset_dir(run_dir, "CENSUS")
  write_agency_spine(mini_spine, "ABS", ds_dir, format = format)

  if (return_data) {
    list(
      person   = person,
      family   = family,
      dwelling = dwelling
    )
  } else {
    rm(person, family, dwelling); gc()
    invisible(list(
      n_person   = n,
      n_dwelling = n_dwellings,
      n_family   = n_families,
      path       = "abs-census"
    ))
  }
}


#' Identify people alive on the 2021 Census reference night
#'
#' A fully known death date before 10 August 2021 excludes a person. Partial
#' death dates exclude a person only when the known year or month is already
#' sufficient to establish that the death preceded Census night.
#'
#' @param spine Spine data.frame with death-date columns.
#' @return Logical vector, one value per spine row.
#' @noRd
.census_present_on_night <- function(spine) {
  death_year <- as.integer(spine$year_of_death)
  death_month <- as.integer(spine$month_of_death)
  death_day <- as.integer(spine$day_of_death)

  died_before <- !is.na(death_year) & (
    death_year < 2021L |
      (death_year == 2021L & !is.na(death_month) & death_month < 8L) |
      (death_year == 2021L & !is.na(death_month) & death_month == 8L &
         !is.na(death_day) & death_day < 10L)
  )
  !died_before
}


#' Build coherent Census household links from the spine
#'
#' @param spine Census-night spine rows.
#' @return Person-level and table-level dwelling and family identifiers.
#' @noRd
.census_household_links <- function(spine) {
  n <- nrow(spine)
  if (n == 0L) {
    return(list(
      person_dwelling_id = character(),
      person_family_id = character(),
      dwelling_id = character(),
      family_id = character()
    ))
  }

  state <- as.integer(spine$state)
  if (anyNA(state) || any(!state %in% 1:9)) {
    stop("Census household links require a valid state code for every person.",
         call. = FALSE)
  }

  household <- spine$household_id
  if (is.numeric(household)) {
    # Numeric keys avoid allocating one large character string per person in
    # full-population builds. Multiplication by 10 leaves room for state 1:9.
    household_state <- as.double(household) * 10 + state
    missing_household <- is.na(household_state)
    household_state[missing_household] <- -which(missing_household)
  } else {
    household <- as.character(household)
    missing_household <- is.na(household) | !nzchar(household)
    household[missing_household] <- paste0("missing-", which(missing_household))
    household_state <- paste(household, state, sep = "\r")
  }
  household_levels <- unique(household_state)
  dwelling_index <- match(household_state, household_levels)
  dwelling_size <- tabulate(dwelling_index, nbins = length(household_levels))

  dwelling_id <- sprintf("D%010d", seq_along(household_levels))
  person_dwelling_id <- dwelling_id[dwelling_index]

  # A one-person household is not a Census family. Only multi-person
  # dwellings receive a family identifier and a row in the family table.
  family_dwelling_index <- which(dwelling_size >= 2L)
  person_family_index <- match(dwelling_index, family_dwelling_index)
  family_id <- sprintf("F%010d", seq_along(family_dwelling_index))
  person_family_id <- family_id[person_family_index]

  list(
    person_dwelling_id = person_dwelling_id,
    person_family_id = person_family_id,
    dwelling_id = dwelling_id,
    family_id = family_id
  )
}


#' Derive Census family composition from linked people
#'
#' @param family Census family data.frame.
#' @param person Census person data.frame after FAMILY_ID assignment.
#' @return Family data.frame with coherent FMCF, CDCF, CPRF, and SSCF values.
#' @noRd
.census_add_family_composition <- function(family, person) {
  n_families <- nrow(family)
  if (n_families == 0L) {
    return(family)
  }

  family_index <- match(person$FAMILY_ID, family$FAMILY_ID)
  linked <- !is.na(family_index)
  if (any(!is.na(person$FAMILY_ID) & !linked)) {
    stop("Census person FAMILY_ID does not match the family table.",
         call. = FALSE)
  }

  child <- person$AGEP < 18L
  adult <- !child
  persons <- tabulate(family_index[linked], nbins = n_families)
  children <- tabulate(family_index[linked & child], nbins = n_families)
  adults <- tabulate(family_index[linked & adult], nbins = n_families)

  fmcf <- rep("9", n_families)
  fmcf[adults >= 2L & children == 0L] <- "1"
  fmcf[adults >= 2L & children > 0L] <- "2"
  fmcf[adults == 1L & children > 0L] <- "3"

  cdcf <- rep("@@", n_families)
  cdcf[fmcf == "1"] <- "00"
  cdcf[fmcf == "2"] <- sprintf("%02d", pmin(children[fmcf == "2"], 6L))
  cdcf[fmcf == "3"] <- sprintf(
    "%02d", pmin(children[fmcf == "3"], 6L) + 7L
  )

  male_adults <- tabulate(
    family_index[linked & adult & person$SEXP == 1L],
    nbins = n_families
  )
  female_adults <- tabulate(
    family_index[linked & adult & person$SEXP == 2L],
    nbins = n_families
  )
  couple <- fmcf %in% c("1", "2")
  sscf <- rep("@", n_families)
  sscf[couple & male_adults >= 2L & female_adults == 0L] <- "1"
  sscf[couple & female_adults >= 2L & male_adults == 0L] <- "2"
  sscf[couple & male_adults > 0L & female_adults > 0L] <- "3"

  family$FMCF <- fmcf
  family$CDCF <- cdcf
  family$CPRF <- as.character(pmin(persons, 6L))
  family$SSCF <- sscf
  family
}


#' Derive Census dwelling geography and composition from linked people
#'
#' @param dwelling Census dwelling data.frame.
#' @param person Census person data.frame after DWELLING_ID assignment.
#' @return Dwelling data.frame with coherent geography and counts.
#' @noRd
.census_add_dwelling_values <- function(dwelling, person) {
  if (nrow(dwelling) == 0L) {
    return(.census_add_dwelling_counts(dwelling, person))
  }

  first_resident <- match(dwelling$DWELLING_ID, person$DWELLING_ID)
  if (anyNA(first_resident)) {
    stop("Census dwelling table contains a dwelling with no linked resident.",
         call. = FALSE)
  }
  dwelling$STEUCD <- as.integer(person$STEUCP[first_resident])

  dwelling_index <- match(person$DWELLING_ID, dwelling$DWELLING_ID)
  state_agrees <- person$STEUCP == dwelling$STEUCD[dwelling_index]
  if (anyNA(dwelling_index) || anyNA(state_agrees) || any(!state_agrees)) {
    stop("Census dwelling state does not agree with linked residents.",
         call. = FALSE)
  }

  .census_add_dwelling_counts(dwelling, person)
}


#' Add linked-person and selected-health counts to Census dwellings
#'
#' @param dwelling Census dwelling data.frame.
#' @param person Census person data.frame after DWELLING_ID assignment.
#' @return The dwelling data.frame with coherent NPRD and 2021 health counts.
#' @noRd
.census_add_dwelling_counts <- function(dwelling, person) {
  dwelling_index <- match(person$DWELLING_ID, dwelling$DWELLING_ID)
  n_dwellings <- nrow(dwelling)

  count_rows <- function(include) {
    tabulate(dwelling_index[include], nbins = n_dwellings)
  }

  occupied <- count_rows(rep(TRUE, nrow(person)))
  adult <- person$AGEP >= 15L
  child <- !adult
  health_missing <- person$CLTHP == "&"
  selected_condition <- person$CLTHP %in% c("1", "2", "3")

  adult_n <- count_rows(adult)
  adult_missing <- count_rows(adult & health_missing)
  adult_selected <- count_rows(adult & selected_condition)
  child_n <- count_rows(child)
  child_missing <- count_rows(child & health_missing)
  child_selected <- count_rows(child & selected_condition)
  all_missing <- count_rows(health_missing)
  all_selected <- count_rows(selected_condition)

  dwelling$NPRD <- ifelse(
    occupied == 0L,
    "@",
    as.character(pmin(occupied, 8L))
  )

  dwelling$CALTHD <- as.character(pmin(adult_selected, 5L))
  dwelling$CALTHD[adult_missing > 0L] <- "6"
  dwelling$CALTHD[adult_n > 0L & adult_missing == adult_n] <- "&"
  dwelling$CALTHD[occupied == 0L] <- "@"

  dwelling$CCLTHD <- as.character(pmin(child_selected, 5L))
  dwelling$CCLTHD[child_missing > 0L] <- "6"
  dwelling$CCLTHD[child_n > 0L & child_missing == child_n] <- "&"
  dwelling$CCLTHD[child_n == 0L] <- "@"

  dwelling$CPLTHD <- sprintf("%02d", pmin(all_selected, 10L))
  dwelling$CPLTHD[all_missing > 0L] <- "11"
  dwelling$CPLTHD[occupied > 0L & all_missing == occupied] <- "&&"
  dwelling$CPLTHD[occupied == 0L] <- "@@"

  dwelling$CPLTHRD <- as.character(pmin(all_selected, 5L))
  dwelling$CPLTHRD[all_missing > 0L] <- "6"
  dwelling$CPLTHRD[occupied > 0L & all_missing == occupied] <- "&"
  dwelling$CPLTHRD[occupied == 0L] <- "@"

  dwelling
}


#' Load spine from the _system/ subfolder of a run directory
#'
#' Looks for base-spine.parquet first, then base-spine.csv.
#'
#' @param run_dir Character. Run directory (e.g. fplida_500000_42/).
#' @return Data.frame.
#' @noRd
load_spine <- function(run_dir) {
  sys_dir <- file.path(run_dir, "_system")
  if (!dir.exists(sys_dir)) {
    stop("No _system/ subfolder found in ", run_dir,
         ". Run generate_spine() first.", call. = FALSE)
  }

  pq <- file.path(sys_dir, "base-spine.parquet")
  csv <- file.path(sys_dir, "base-spine.csv")

  if (file.exists(pq)) {
    path <- pq
  } else if (file.exists(csv)) {
    path <- csv
  } else {
    stop("No base-spine file found in ", sys_dir,
         ". Run generate_spine() first.", call. = FALSE)
  }

  ext <- tools::file_ext(path)
  if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read parquet spine. ",
           "Install it with: install.packages('arrow')", call. = FALSE)
    }
    spine <- as.data.frame(read_parquet_safely(path))
  } else {
    spine <- read.csv(path, stringsAsFactors = FALSE)
  }

  message("Loaded spine from ", path)
  spine
}
