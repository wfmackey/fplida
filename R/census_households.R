# Census household assembly.
#
# A dwelling and a family are properties of a household, and a household spans
# build slices: the spine's row order scatters them, so a contiguous slice cuts
# through most of the multi-person ones. Deriving the identifiers per slice
# therefore did two things wrong at once. It numbered dwellings from one inside
# each slice, so `D0000000001` meant a different household in every slice and a
# join on it merged unrelated people -- on a 20,000-person build, 5,467 of
# 5,514 dwelling identifiers held more than one household. And it split the
# households that straddled a boundary, so 2,295 of 8,686 came out as two
# dwellings.
#
# The identifiers now come from the spine's own `dwelling_id`, so every slice
# agrees without being told. The composition a person's role depends on --
# who else lives there, how old they are -- still needs the whole household,
# so the roles are computed once centrally and read back by each slice.

# RLHP, relationship in household, from the 2021 Census category list.
.CENSUS_RLHP <- c(
  registered_marriage = "11",
  de_facto_opposite   = "15",
  de_facto_male       = "17",
  de_facto_female     = "18",
  lone_parent         = "21",
  child_under_15      = "31",
  dependent_student   = "41",
  non_dependent_child = "51",
  sibling             = "61",
  parent              = "62",
  other_related       = "69",
  unrelated_child     = "36",
  group_member        = "72",
  lone_person         = "73",
  not_applicable      = "@@"
)

# A couple is two adults close in age. Beyond this gap they read as a parent
# and an adult child, which is the other thing a two-adult household can be.
.CENSUS_COUPLE_MAX_AGE_GAP <- 18L

# Below this gap an extra adult is a housemate or a sibling; at or above it
# they are the reference person's adult child.
.CENSUS_GENERATION_GAP <- 20L

# Share of couples in a registered marriage rather than a de facto one. The
# 2021 Census counted about 4.9 million registered-marriage couples against
# 1.2 million de facto.
.CENSUS_REGISTERED_SHARE <- 0.80


#' Dwelling and family identifiers for a spine
#'
#' Both are derived from the spine's `dwelling_id`, so a household carries the
#' same identifiers in every build slice and in a single-process build.
#'
#' @param spine data.frame. Spine rows, needing `dwelling_id`.
#' @return A list with `dwelling` and `family` character vectors, one per
#'   person. `family` is `NA` for a lone-person household, which is not a
#'   Census family.
#' @keywords internal
.census_household_identifiers <- function(spine) {
  n <- nrow(spine)
  if (!n) return(list(dwelling = character(), family = character()))

  # A dwelling is in one state. The spine guarantees that now, but a
  # hand-built one need not, and a Census dwelling that spanned states would
  # be a place in two places, so the state joins the key.
  state <- suppressWarnings(as.integer(spine$state))
  state[is.na(state)] <- 0L
  key <- (.dil_dwelling_key(spine) * 10 + state) %% 1e10

  dwelling <- sprintf("D%010.0f", key)
  size <- as.integer(table(key)[as.character(key)])
  family <- ifelse(size >= 2L, sprintf("F%010.0f", key), NA_character_)
  list(dwelling = dwelling, family = family)
}


#' Each person's relationship to their household
#'
#' Needs the whole household, so it is computed once over the full spine
#' rather than per slice: a slice that holds two of a household's four people
#' cannot tell a lone parent from a partnered one.
#'
#' @param spine data.frame. The full spine.
#' @param seed Integer. Random seed.
#' @param reference_year Integer. Census year.
#' @return A data.frame with `spine_id`, `DWELLING_ID`, `FAMILY_ID`, `RLHP`,
#'   `FPIP` and `SPIP`.
#' @export
census_household_roles <- function(spine, seed = 42L, reference_year = 2021L) {
  n <- nrow(spine)
  ids <- .census_household_identifiers(spine)
  if (!n) {
    return(data.frame(
      spine_id = character(), DWELLING_ID = character(),
      FAMILY_ID = character(), RLHP = character(),
      FPIP = character(), SPIP = character(),
      stringsAsFactors = FALSE
    ))
  }

  age <- as.integer(reference_year) - as.integer(spine$birth_year)
  age[is.na(age)] <- 40L
  sex <- as.integer(spine$sex)
  members <- split(seq_len(n), ids$dwelling)

  rlhp <- rep(.CENSUS_RLHP[["not_applicable"]], n)
  fpip <- rep("@", n)
  spip <- rep("@", n)

  # One draw per dwelling decides whether its couple is registered, and it has
  # to be stable, so it comes from the dwelling rather than from an RNG stream
  # whose order depends on how the households were split.
  marriage_draw <- .mobility_dwelling_draw(spine, seed, "registered marriage")

  for (rows in members) {
    ages <- age[rows]
    adults <- rows[ages >= 18L]
    children <- rows[ages < 18L]

    if (!length(adults)) {
      # A household with no adult in it. The spine makes these when a child's
      # state has no parenting-age household to attach them to, and the Census
      # still needs a reference person and a relationship for each of them.
      oldest <- rows[which.max(age[rows])]
      spip[oldest] <- "1"
      if (length(rows) == 1L) {
        rlhp[oldest] <- .CENSUS_RLHP[["lone_person"]]
      } else {
        # All of them are children, so none is a group household member in
        # the sense the code means; they are unrelated children under 15.
        rlhp[rows] <- .CENSUS_RLHP[["unrelated_child"]]
      }
      next
    }

    # The oldest adult is the family reference person.
    reference <- adults[which.max(age[adults])]
    others <- setdiff(adults, reference)
    spip[reference] <- "1"

    partner <- NA_integer_
    if (length(others)) {
      closest <- others[which.min(abs(age[others] - age[reference]))]
      if (abs(age[closest] - age[reference]) <= .CENSUS_COUPLE_MAX_AGE_GAP) {
        partner <- closest
      }
    }

    if (!is.na(partner)) {
      registered <- marriage_draw[reference] < .CENSUS_REGISTERED_SHARE
      code <- if (registered) {
        .CENSUS_RLHP[["registered_marriage"]]
      } else if (isTRUE(sex[reference] == sex[partner])) {
        if (isTRUE(sex[reference] == 1L)) {
          .CENSUS_RLHP[["de_facto_male"]]
        } else {
          .CENSUS_RLHP[["de_facto_female"]]
        }
      } else {
        .CENSUS_RLHP[["de_facto_opposite"]]
      }
      rlhp[c(reference, partner)] <- code
      spip[partner] <- "2"
      others <- setdiff(others, partner)
    } else if (length(children)) {
      rlhp[reference] <- .CENSUS_RLHP[["lone_parent"]]
    } else if (!length(others)) {
      rlhp[reference] <- .CENSUS_RLHP[["lone_person"]]
    } else {
      rlhp[reference] <- .CENSUS_RLHP[["group_member"]]
    }

    # A parent indicator belongs to the people with children in the dwelling.
    if (length(children)) {
      parents <- c(reference, if (!is.na(partner)) partner)
      fpip[parents] <- ifelse(sex[parents] == 1L, "1", "2")
    }

    # Children under 15 are children; older ones are dependent students while
    # they are studying and non-dependent children after that.
    if (length(children)) {
      rlhp[children] <- .CENSUS_RLHP[["child_under_15"]]
    }

    # The adults left over are the ones a two-adult model could not describe.
    for (person in others) {
      gap <- age[reference] - age[person]
      rlhp[person] <- if (gap >= .CENSUS_GENERATION_GAP) {
        if (age[person] < 25L) {
          .CENSUS_RLHP[["dependent_student"]]
        } else {
          .CENSUS_RLHP[["non_dependent_child"]]
        }
      } else if (gap <= -.CENSUS_GENERATION_GAP) {
        .CENSUS_RLHP[["parent"]]
      } else if (!length(children) && is.na(partner)) {
        .CENSUS_RLHP[["group_member"]]
      } else {
        .CENSUS_RLHP[["sibling"]]
      }
    }
  }

  data.frame(
    spine_id = as.character(spine$spine_id),
    DWELLING_ID = ids$dwelling,
    FAMILY_ID = ids$family,
    RLHP = unname(rlhp),
    FPIP = fpip,
    SPIP = spip,
    stringsAsFactors = FALSE
  )
}


#' Path to the cached household roles
#'
#' @param run_dir Character. Run directory.
#' @return Character path.
#' @keywords internal
.census_household_roles_path <- function(run_dir) {
  file.path(run_dir, "_system", "census-households.parquet")
}


#' Compute the household roles once for a whole build
#'
#' Called from the central stage of `build_fplida()`, before the slice workers
#' start. Each slice reads the result rather than recomputing it from the part
#' of the household it happens to hold.
#'
#' @param spine data.frame or NULL. The full spine.
#' @param seed Integer. Random seed.
#' @param output_dir Character or NULL. Base output directory.
#' @param reference_year Integer. Census year.
#' @return Invisibly, the path written.
#' @export
write_census_household_roles <- function(spine = NULL, seed = 42L,
                                         output_dir = NULL,
                                         reference_year = 2021L) {
  run_dir <- resolve_run_dir(output_dir)
  if (is.null(spine)) {
    spine <- load_spine_select(run_dir, c("spine_id", "birth_year", "sex",
                                          "state", "household_id",
                                          "dwelling_id", "year_of_death",
                                          "month_of_death", "day_of_death"))
  }
  # The roles have to describe the household the Census sees. Counting people
  # who died before Census night leaves a household whose reference person is
  # dead with no reference person at all, and makes a widowed parent look
  # partnered.
  spine <- spine[.census_present_on_night(spine), , drop = FALSE]
  roles <- census_household_roles(spine, seed, reference_year)
  sys_dir <- file.path(run_dir, "_system")
  if (!dir.exists(sys_dir)) dir.create(sys_dir, recursive = TRUE)
  path <- .census_household_roles_path(run_dir)
  arrow::write_parquet(roles, path)
  invisible(path)
}


#' Read the household roles for a set of spine rows
#'
#' Falls back to computing them from `spine` when the central stage has not
#' run, which is the standalone `generate_census()` path.
#'
#' @param spine data.frame. Spine rows, possibly a slice.
#' @param run_dir Character. Run directory.
#' @param seed Integer. Random seed.
#' @param reference_year Integer. Census year.
#' @return A data.frame of roles, in the order of `spine`.
#' @keywords internal
.census_read_household_roles <- function(spine, run_dir, seed = 42L,
                                         reference_year = 2021L) {
  path <- .census_household_roles_path(run_dir)
  if (file.exists(path) && requireNamespace("arrow", quietly = TRUE)) {
    roles <- as.data.frame(read_parquet_safely(path), stringsAsFactors = FALSE)
    index <- match(as.character(spine$spine_id), roles$spine_id)
    if (!anyNA(index)) return(roles[index, , drop = FALSE])
  }
  census_household_roles(spine, seed, reference_year)
}


#' Collapse the merged dwelling and family tables to one row per household
#'
#' Both are one row per household, and a household that straddles a slice
#' boundary is written by every slice that holds part of it. The identifiers
#' agree, so the duplicates are exact and the first row of each is kept.
#'
#' @param run_dir Character. Canonical run directory.
#' @return Invisibly, the number of rows removed.
#' @keywords internal
.census_dedupe_household_tables <- function(run_dir) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(invisible(0L))
  removed <- 0L
  for (spec in list(list(table = "dwelling", key = "DWELLING_ID"),
                    list(table = "family", key = "FAMILY_ID"))) {
    prod_dir <- file.path(dataset_dir(run_dir, "CENSUS"),
                          census_product_name(spec$table))
    if (!dir.exists(prod_dir)) next
    parts <- list.files(prod_dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(parts) < 2L) next

    combined <- as.data.frame(
      arrow::open_dataset(parts, unify_schemas = TRUE),
      stringsAsFactors = FALSE
    )
    if (!spec$key %in% names(combined)) next
    before <- nrow(combined)
    combined <- combined[!duplicated(combined[[spec$key]]), , drop = FALSE]
    rownames(combined) <- NULL
    removed <- removed + (before - nrow(combined))

    unlink(parts)
    arrow::write_parquet(combined, file.path(prod_dir, "part-000.parquet"))
  }
  invisible(removed)
}
