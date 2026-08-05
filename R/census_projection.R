# Census 2021 person table projection from the fplida spine.
#
# Thin R wrapper over the Rust generator `project_census_person__`
# (src/rust/src/census_2021.rs), which is the single live projection path.
# All Census-specific column logic and code frames live in Rust; this file
# only marshals the required spine columns and the valid-ANZSCO lookup.
#
# Code frames (BPLP/LANP/RELP/HEAP/INDP/MTWP/OCCP) and the cross-dataset
# country-of-birth (SACC) and ASGS geography are handled in Rust, sourced
# from inst/foundations/census_2021.toml and inst/extdata/codeframes/.


# -- Valid ANZSCO 6-digit occupation codes (bundled) ------------------------

#' Cached vector of valid 6-digit ANZSCO 2019 occupation codes used to
#' validate and clean the spine's occupation codes when emitting OCCP.
#'
#' The list is bundled in `inst/extdata/codeframes/`. It was previously read
#' from the optional `strayr` package, which meant Census output depended on
#' whether that package happened to be installed: without it the validity
#' check was skipped, the per-person RNG stream shifted, and almost every
#' Census row changed. Regenerate with `data-raw/update_anzsco_codes.R`.
#' @keywords internal
.census_valid_anzsco_codes <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    path <- system.file("extdata", "codeframes",
                        "anzsco2019-occupation-codes.txt", package = "fplida")
    if (!nzchar(path)) {
      path <- file.path("inst", "extdata", "codeframes",
                        "anzsco2019-occupation-codes.txt")
    }
    if (!file.exists(path)) {
      stop("The bundled ANZSCO 2019 occupation code list is missing. ",
           "Reinstall fplida.", call. = FALSE)
    }

    lines <- readLines(path, warn = FALSE)
    lines <- lines[!startsWith(lines, "#") & nzchar(trimws(lines))]
    cache <<- sort(unique(as.integer(lines)))
    cache
  }
})


# -- Main projection wrapper ------------------------------------------------

#' Project Census 2021 person table from the fplida spine.
#'
#' @param spine_df data.frame from generate_spine(); must include
#'   `country_of_birth_sacc` and `sa2_code`.
#' @param seed Integer seed for Census-specific column generation.
#' @return data.frame with Census 2021 person columns.
#' @keywords internal
project_census_person <- function(spine_df, seed) {
  if (!exists("project_census_person__", mode = "function")) {
    stop("project_census_person__ (Rust) is unavailable; reinstall fplida.",
         call. = FALSE)
  }
  raw <- project_census_person__(
    aeuid_abs             = as.character(spine_df$aeuid_abs),
    birth_year            = as.integer(spine_df$birth_year),
    sex                   = as.integer(spine_df$sex),
    state                 = as.integer(spine_df$state),
    indigenous            = as.integer(spine_df$indigenous),
    country_of_birth_sacc = as.integer(spine_df$country_of_birth_sacc),
    year_of_arrival       = as.integer(spine_df$year_of_arrival),
    citizenship           = as.integer(spine_df$citizenship),
    education             = as.integer(spine_df$education),
    baseline_employed     = as.integer(spine_df$baseline_employed),
    baseline_hours        = as.integer(spine_df$baseline_hours),
    anzsco_code           = as.integer(spine_df$anzsco_code),
    anzsco_major          = as.integer(spine_df$anzsco_major),
    industry              = as.integer(spine_df$industry),
    valid_anzsco_codes    = .census_valid_anzsco_codes(),
    disability_onset_year = as.integer(spine_df$disability_onset_year),
    disability_severity   = as.integer(spine_df$disability_severity),
    task_physical         = as.double(spine_df$task_physical),
    sa2                   = as.integer(spine_df$sa2_code),
    seed                  = as.integer(seed)
  )
  as.data.frame(raw, stringsAsFactors = FALSE)
}
