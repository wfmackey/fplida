# The code lists that are too large for the variable registry.
#
# `variable_info()` carries a variable's codes in `valid_values`, which works
# until the list is a classification rather than a handful of categories. A
# registry row cannot hold 368,286 mesh blocks, so those variables say how many
# values the source publishes and point at the source instead.
#
# That is honest and it is a dead end, because the package already ships many of
# those tables: a reader is sent to an external website for something sitting in
# the library they have loaded. These functions hand the table over.
#
# The catalogue below is the single place a table is declared. Each entry names
# the shipped file, the rows inside it that make up the domain, and the registry
# value domains it serves. `update_variable_info.R` reads the same declaration
# to decide which value definitions get to print a call, so a domain can never
# advertise a table the package does not have.

# `filter` is a list of column = value pairs, all of which must hold. `domains`
# names the registry value domains an entry serves, and earns those variables a
# printed call in their value definition.
#
# An entry is only listed where the shipped table holds the WHOLE domain. Several
# near misses were left out on purpose, because a reader told to call
# `get_values()` and handed nine tenths of a classification is worse off than one
# sent to the publisher: the ASGS 2021 SA3 layer ships 340 of 359 codes, because
# the Census geography file carries the spatial areas and not the special ones;
# LGA ships 566 of 567; the ASCL language table ships 172 of 173. Those domains
# still name their source and nothing else.
.value_table_catalogue <- function() {
  list(
    `mbs-items` = list(
      title = "Medicare Benefits Schedule item numbers",
      values = 5911L,
      path = c("extdata", "mbs_item_lookup.csv"),
      code = "item_num",
      label = NULL,
      extra = c("category", "group", "schedule_fee"),
      filter = NULL,
      domains = "mbs_item_number",
      source = "Department of Health and Aged Care, MBS Online"
    ),
    `pbs-items` = list(
      title = "Pharmaceutical Benefits Scheme item codes",
      values = 4274L,
      path = c("extdata", "pbs_item_lookup.csv"),
      code = "pbs_code",
      label = NULL,
      extra = c("atc_code", "benefit_type", "pack_size"),
      filter = NULL,
      domains = "pbs_item_code",
      source = "Department of Health and Aged Care, PBS schedule"
    ),
    `mesh-blocks` = list(
      title = "ASGS 2021 mesh blocks",
      values = 368286L,
      path = c("extdata", "mb_lookup.csv.gz"),
      code = "mb_code",
      label = NULL,
      extra = c("sa1_code", "sa2_code", "sa4_code", "state"),
      filter = NULL,
      domains = c("asgs_2021_mesh_block", "asgs_mesh_block_2021"),
      source = "ABS Australian Statistical Geography Standard 2021"
    ),
    # The mesh block table is the source for SA1 and SA4 as well. It is the only
    # shipped file that carries the whole of either: sa1_lookup.csv holds 61,822
    # of the 61,845 SA1s, and the SA2 lookup's 107 SA4s are the spatial ones.
    `sa1` = list(
      title = "ASGS 2021 Statistical Areas Level 1",
      values = 61845L,
      path = c("extdata", "mb_lookup.csv.gz"),
      code = "sa1_code",
      label = NULL,
      extra = c("sa2_code", "sa4_code", "state"),
      filter = NULL,
      domains = c("asgs_sa1_2021", "asgs_2021_statistical_area_level_1"),
      source = "ABS Australian Statistical Geography Standard 2021"
    ),
    `sa2` = list(
      title = "ASGS 2021 Statistical Areas Level 2",
      values = 2473L,
      path = c("extdata", "codeframes", "sa2_2021.tsv"),
      code = "code",
      label = "name",
      extra = "state",
      filter = list(year = "2021"),
      domains = c("asgs_sa2", "asgs_sa2_2021",
                  "asgs_2021_statistical_area_level_2",
                  "avetmiss_statistical_area_level_2_identifier (ABS ASGS SA2)"),
      source = "ABS Australian Statistical Geography Standard 2021"
    ),
    `sa4` = list(
      title = "ASGS 2021 Statistical Areas Level 4",
      values = 108L,
      path = c("extdata", "mb_lookup.csv.gz"),
      code = "sa4_code",
      label = NULL,
      extra = "state",
      filter = NULL,
      domains = "asgs_sa4_2021",
      source = "ABS Australian Statistical Geography Standard 2021"
    ),
    `state-electorates` = list(
      title = "ASGS 2021 state electoral divisions",
      values = 433L,
      path = c("extdata", "codeframes", "census-geography-values.tsv"),
      code = "code",
      label = "name",
      extra = "state",
      filter = list(year = "2021", layer = "SED"),
      domains = c("asgs2021_sed_code", "asgs2021_sed_name"),
      source = "ABS Australian Statistical Geography Standard 2021"
    ),
    `countries` = list(
      title = "Standard Australian Classification of Countries",
      values = 255L,
      path = c("extdata", "codeframes", "sacc_country.tsv"),
      code = "code",
      label = "label",
      extra = character(0),
      filter = NULL,
      domains = "sacc_country",
      source = "ABS Standard Australian Classification of Countries"
    ),
    `census-birthplace` = list(
      title = "Census country of birth of person",
      values = 344L,
      path = c("extdata", "codeframes", "census-codeframe-values.csv"),
      code = "code",
      label = "label",
      extra = character(0),
      filter = list(year = "2021", variable = "BPLP"),
      domains = "census_bplp_country_of_birth_of_person",
      source = "ABS Census of Population and Housing 2021 data item list"
    ),
    `census-relationship` = list(
      title = "Census relationship in household",
      values = 34L,
      path = c("extdata", "codeframes", "census-codeframe-values.csv"),
      code = "code",
      label = "label",
      extra = character(0),
      filter = list(year = "2021", variable = "RLHP"),
      domains = "census_rlhp_relationship_in_household",
      source = "ABS Census of Population and Housing 2021 data item list"
    ),
    `census-relationship-grandchildren` = list(
      title = "Census relationship in household, grandchildren separated",
      values = 32L,
      path = c("extdata", "codeframes", "census-codeframe-values.csv"),
      code = "code",
      label = "label",
      extra = character(0),
      filter = list(year = "2021", variable = "RLGP"),
      domains = "census_rlgp_relationship_in_household_incl_grandchildren",
      source = "ABS Census of Population and Housing 2021 data item list"
    ),
    `census-family-composition` = list(
      title = "Census family composition",
      values = 37L,
      path = c("extdata", "codeframes", "census-codeframe-values.csv"),
      code = "code",
      label = "label",
      extra = character(0),
      filter = list(year = "2021", variable = "FMCF"),
      domains = "census_fmcf_family_composition",
      source = "ABS Census of Population and Housing 2021 data item list"
    )
  )
}

# `system.file()` takes the path one component at a time, and handing it a
# character vector returns one path per element rather than one joined path.
.value_table_path <- function(entry) {
  path <- do.call(system.file,
                  c(as.list(entry$path), list(package = "fplida")))
  if (length(path) != 1L || !nzchar(path) || !file.exists(path)) {
    return(NA_character_)
  }
  path
}

.read_value_table <- function(path) {
  sep <- if (grepl("\\.tsv$", path)) "\t" else ","
  connection <- if (grepl("\\.gz$", path)) gzfile(path) else path
  utils::read.csv(connection, sep = sep, stringsAsFactors = FALSE,
                  colClasses = "character", check.names = FALSE)
}

#' Get a code list too large to print in the variable registry
#'
#' A classification with thousands of codes cannot sit in a `variable_info()`
#' row, so those variables record how many values the source publishes and name
#' the source. `get_values()` returns the list itself for the classifications
#' the package ships.
#'
#' Call it with no argument for the catalogue: one row per table, with the key
#' to ask for, how many codes it holds, and where the codes came from.
#'
#' @param key Character. A table key, as listed by `get_values()`.
#'
#' @return A data frame of codes, with `code` first and a `label` column where
#'   the source publishes names. Called with no argument, the catalogue.
#'
#' @seealso [variable_values()] for the code lists small enough to sit in the
#'   registry itself, and [variable_info()] for the registry.
#'
#' @examples
#' # What can be fetched.
#' get_values()
#'
#' # One classification.
#' head(get_values("sa2"))
#' @export
get_values <- function(key = NULL) {
  catalogue <- .value_table_catalogue()

  if (is.null(key)) {
    # The declared count, not a measured one: listing the catalogue should not
    # read 368,286 mesh blocks off disk to find out how many there are. The
    # declaration is held to the file, and to the size the source publishes, by
    # tests/testthat/test-value-tables.R.
    result <- data.frame(
      key = names(catalogue),
      title = vapply(catalogue, `[[`, character(1), "title"),
      values = vapply(catalogue, `[[`, integer(1), "values"),
      source = vapply(catalogue, `[[`, character(1), "source"),
      stringsAsFactors = FALSE
    )
    rownames(result) <- NULL
    return(.as_output_frame(result))
  }

  if (!is.character(key) || length(key) != 1L || is.na(key)) {
    stop("`key` must be a single table key. Call get_values() to list them.",
         call. = FALSE)
  }
  if (!key %in% names(catalogue)) {
    stop(sprintf(
      "No shipped table for \"%s\". Call get_values() for the %d keys there are.",
      key, length(catalogue)
    ), call. = FALSE)
  }
  .as_output_frame(.get_values_table(key))
}

# Reading a 368,286-row table on every call is wasteful when the same session
# asks twice, and the tables never change within an install.
.value_table_cache <- new.env(parent = emptyenv())

.get_values_table <- function(key) {
  cached <- get0(key, envir = .value_table_cache, inherits = FALSE)
  if (!is.null(cached)) return(cached)

  entry <- .value_table_catalogue()[[key]]
  path <- .value_table_path(entry)
  # Loud rather than empty. A caller who gets zero rows back reads it as "this
  # classification has no codes", which is a worse answer than an error.
  if (is.na(path)) {
    stop(sprintf(
      "The table for \"%s\" is not installed with this build of fplida (%s).",
      key, paste(entry$path, collapse = "/")
    ), call. = FALSE)
  }

  raw <- .read_value_table(path)
  for (column in names(entry$filter)) {
    if (!column %in% names(raw)) {
      stop(sprintf("Column \"%s\" is missing from %s.", column, path),
           call. = FALSE)
    }
    raw <- raw[raw[[column]] == entry$filter[[column]], , drop = FALSE]
  }

  wanted <- c(entry$code, entry$label, entry$extra)
  missing <- setdiff(wanted, names(raw))
  if (length(missing)) {
    stop(sprintf("Columns %s are missing from %s.",
                 paste(sQuote(missing), collapse = ", "), path), call. = FALSE)
  }

  out <- raw[, wanted, drop = FALSE]
  names(out)[[1L]] <- "code"
  if (!is.null(entry$label)) names(out)[[2L]] <- "label"
  out <- out[!duplicated(out$code), , drop = FALSE]
  rownames(out) <- NULL

  assign(key, out, envir = .value_table_cache)
  out
}

# A classification is thousands of rows long, and printing it as a data frame
# floods the console with exactly the thing the caller wanted to look at.
.as_output_frame <- function(x) {
  if (requireNamespace("tibble", quietly = TRUE)) return(tibble::as_tibble(x))
  x
}

#' @rdname get_values
#' @export
get_mbs_item_numbers <- function() get_values("mbs-items")

#' @rdname get_values
#' @export
get_pbs_item_codes <- function() get_values("pbs-items")

#' @rdname get_values
#' @export
get_mesh_blocks <- function() get_values("mesh-blocks")

#' @rdname get_values
#' @export
get_sa1_codes <- function() get_values("sa1")

#' @rdname get_values
#' @export
get_sa2_codes <- function() get_values("sa2")

#' @rdname get_values
#' @export
get_country_codes <- function() get_values("countries")
