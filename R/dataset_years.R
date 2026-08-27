# What years a dataset covers, and how a generator is held to them.
#
# build_fplida() takes one `years` vector and hands it to every year-aware
# generator, but PLIDA datasets do not share a period. TVA runs 2015 to 2023,
# HE stops in 2021, PIT_PS stops at the 2022-23 financial year. Handing the
# same span to all of them writes products that do not exist in PLIDA, and a
# user who trusts the output then builds a pipeline on a year that will never
# arrive in the real asset.
#
# The contract is the bundled dataset registry -- `plida_metadata/datasets.csv`,
# column `Reference Period` -- so the answer moves when the metadata moves
# rather than when someone remembers to update a constant in a generator.


# The registry writes an open period as "... to current". "Current" has to
# resolve to a fixed year: the system clock would make a build depend on the
# day it ran and would keep inventing later years forever. The newest period
# any bundled metadata declares is 2025-26 -- STP in datasets.csv, and several
# BLADE tables in blade_metadata/tables.csv -- so the end of that financial
# year is as far as "current" can honestly reach.
PLIDA_CURRENT_PERIOD_YEAR <- 2026L

# A build product name is the registry acronym lower-cased, apart from these.
# `spine` and `blade` have no row in datasets.csv: the spine is ours, and
# BLADE's periods are per table in blade_metadata/tables.csv. Both are
# therefore unrestricted here.
PRODUCT_DATASET_ALIASES <- list(apprentice = "A&T")

# The two personal income tax products are built back to 2010 whatever window
# the build asks for. This is not a formatting choice: the employment panel
# behind them is a counterfactual trajectory anchored at 2021 and walked
# backwards and forwards from there (src/rust/src/employment.rs), so the span
# of `years` decides the values inside every year, not just which years get
# written. Shortening it would change the years the caller did ask for. Held
# here rather than in the slice worker so a build can plan, report and generate
# the same years.
PRODUCT_YEAR_FLOOR <- list(pit_ps = 2010L, pit_itr = 2010L)

# Products a build steers with its `years`. The rest write the periods they
# were built with -- Census writes 2011, 2016 and 2021 whatever the build asks
# for, and CGT, RPS, SAE and PIT_IE each generate their whole published span --
# so the build's `years` is not theirs to narrow. Those generators still gate
# the `years` they are given when called directly.
YEAR_AWARE_PRODUCTS <- c("core", "pit_ps", "pit_itr", "he", "domino", "mbs",
                         "pbs", "tva", "deaths", "travellers", "busown",
                         "stp", "air")


#' Years a PLIDA dataset covers
#'
#' Reports the reference period the bundled PLIDA registry declares for a
#' dataset, as a vector of years. Every year-aware generator narrows its
#' `years` argument to this before generating, so a build never writes a
#' product year that does not exist in PLIDA.
#'
#' Years are calendar years for calendar-year datasets and financial-year END
#' years for financial-year datasets, matching what the generators' `years`
#' arguments already mean. `PIT_PS` covering the 2001-02 to 2022-23 financial
#' years therefore returns `2002:2023`.
#'
#' A period written as "... to current" is closed at 2026, the end of the
#' newest financial year any bundled metadata declares. The value is fixed, not
#' read from the system clock, so the same build gives the same answer next
#' year.
#'
#' @param dataset Character. One dataset, named either as the registry writes
#'   it (`"TVA"`, `"PIT_PS"`, `"A&T"`) or as [build_fplida()] names the product
#'   (`"tva"`, `"pit_ps"`, `"apprentice"`). Case does not matter.
#'
#' @return An integer vector of years, sorted. `NULL` when the dataset declares
#'   no period -- an unknown name, the spine, or BLADE, whose periods are per
#'   table. `NULL` means unrestricted, never "no years".
#'
#' @seealso [plida_dataset_periods()] for every dataset at once.
#'
#' @examples
#' plida_dataset_years("TVA")
#' plida_dataset_years("pit_ps")
#' plida_dataset_years("CENSUS")
#'
#' @export
plida_dataset_years <- function(dataset) {
  stopifnot("`dataset` must be one dataset name" =
              is.character(dataset) && length(dataset) == 1L && !is.na(dataset))
  registry <- dataset_year_registry()
  years <- registry[[dataset_acronym(dataset)]]
  if (is.null(years) || length(years) == 0L) return(NULL)
  years
}


#' Reference periods of every PLIDA dataset
#'
#' The companion to [plida_dataset_years()]: one row per dataset acronym in the
#' bundled registry, with the period as the metadata writes it and as the
#' generators read it. Use it to see why a build dropped a year.
#'
#' @param dataset Character, or `NULL` for every dataset. Named as the registry
#'   writes it (`"TVA"`) or as [build_fplida()] names the product (`"tva"`), the
#'   same as [plida_dataset_years()]. A name the registry does not carry gives
#'   no rows, which is the table's way of saying what `plida_dataset_years()`
#'   says with `NULL`: no declared period, so nothing restricts it.
#'
#' @return A data frame with one row per dataset: `dataset`, the
#'   `reference_period` text from the registry, the parsed `years` in compact
#'   form, `first_year`, `last_year` and `n_years`.
#'
#' @seealso [plida_dataset_years()] for one dataset's years.
#'
#' @examples
#' plida_dataset_periods("TVA")
#' periods <- plida_dataset_periods()
#' periods[periods$dataset %in% c("TVA", "HE", "PIT_PS"), ]
#'
#' @export
plida_dataset_periods <- function(dataset = NULL) {
  registry <- dataset_year_registry()
  texts <- attr(registry, "period_text")
  datasets <- sort(names(registry))
  if (!is.null(dataset)) {
    stopifnot("`dataset` must be one dataset name" =
                is.character(dataset) && length(dataset) == 1L &&
                !is.na(dataset))
    datasets <- intersect(datasets, dataset_acronym(dataset))
  }
  if (length(datasets) == 0L) {
    return(data.frame(dataset = character(), reference_period = character(),
                      years = character(), first_year = integer(),
                      last_year = integer(), n_years = integer(),
                      stringsAsFactors = FALSE))
  }

  row_for <- function(ds) {
    years <- registry[[ds]]
    data.frame(
      dataset          = ds,
      reference_period = texts[[ds]],
      years            = format_year_span(years),
      first_year       = if (length(years)) min(years) else NA_integer_,
      last_year        = if (length(years)) max(years) else NA_integer_,
      n_years          = length(years),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, lapply(datasets, row_for))
  rownames(out) <- NULL
  out
}


#' Narrow a requested year span to what a dataset covers
#'
#' The gate every year-aware generator runs its `years` argument through.
#' Reports what it dropped, because a silent drop is as unhelpful as the silent
#' invention it replaces.
#'
#' @param dataset Character. Dataset acronym or build product name.
#' @param years Integer vector. Years the caller asked for.
#' @param quiet Logical. Suppress the message when years are dropped.
#' @return The requested years that the dataset covers, sorted and unique.
#'   Possibly empty, in which case the caller must write nothing.
#' @keywords internal
gate_dataset_years <- function(dataset, years, quiet = FALSE) {
  requested <- sort(unique(as.integer(years)))
  covered <- plida_dataset_years(dataset)
  if (is.null(covered)) return(requested)

  kept <- intersect(requested, covered)
  dropped <- setdiff(requested, covered)
  if (length(dropped) == 0L || isTRUE(quiet)) return(kept)

  acronym <- dataset_acronym(dataset)
  if (length(kept) == 0L) {
    message(sprintf(
      "%s covers %s, which excludes every requested year (%s). %s",
      acronym, format_year_span(covered), format_year_span(requested),
      paste0("Writing nothing for ", acronym, ".")
    ))
  } else {
    message(sprintf(
      "%s covers %s: requested %s, generating %s (dropped %s).",
      acronym, format_year_span(covered), format_year_span(requested),
      format_year_span(kept), format_year_span(dropped)
    ))
  }
  kept
}


#' Years a product asks for before its dataset's period is applied
#'
#' Only the two personal income tax products differ from the build's `years`,
#' and only by reaching further back. Applied here so [build_fplida()] can plan
#' and report exactly the years the slice workers will generate.
#'
#' @param product Character. One build product name.
#' @param years Integer vector. The build's `years`.
#' @return An integer vector, sorted and unique.
#' @keywords internal
product_requested_years <- function(product, years) {
  years <- sort(unique(as.integer(years)))
  floor_year <- PRODUCT_YEAR_FLOOR[[product]]
  if (is.null(floor_year)) return(years)
  sort(unique(c(seq.int(floor_year, max(floor_year, max(years))), years)))
}


#' Plan the years each year-aware product in a build will generate
#'
#' @param products Character vector of build product names.
#' @param years Integer vector. The build's `years`.
#' @param quiet Logical. Suppress the per-product drop messages.
#' @return A named list, one integer vector per year-aware product in
#'   `products`. An empty entry means the dataset covers none of the requested
#'   years, so nothing should be generated for it.
#' @keywords internal
plan_product_years <- function(products, years, quiet = TRUE) {
  planned <- intersect(products, YEAR_AWARE_PRODUCTS)
  plan_one <- function(product) {
    gate_dataset_years(product, product_requested_years(product, years),
                       quiet = quiet)
  }
  stats::setNames(lapply(planned, plan_one), planned)
}


#' Say what a build's year plan does to each product
#'
#' The gate inside a generator messages when it drops a year, but a sliced
#' build runs its generators in worker processes whose messages nobody sees.
#' The orchestrator reports the plan here instead, so a dropped year is visible
#' where the build is.
#'
#' @param plan Named list from [plan_product_years()].
#' @param years Integer vector. The build's `years`.
#' @return `plan`, invisibly.
#' @keywords internal
report_product_year_plan <- function(plan, years) {
  requested <- sort(unique(as.integer(years)))
  differs <- vapply(plan, function(p) !identical(as.integer(p), requested),
                    logical(1))
  if (!any(differs)) {
    message("  Years by product: every product covers ",
            format_year_span(requested))
    return(invisible(plan))
  }

  message("  Years by product, where they differ from the request:")
  report_one <- function(product) {
    covered <- plida_dataset_years(product)
    planned <- as.integer(plan[[product]])
    note <- if (length(planned) == 0L) {
      "not built"
    } else {
      c(if (length(setdiff(requested, planned))) {
          paste("dropped", format_year_span(setdiff(requested, planned)))
        },
        if (length(setdiff(planned, requested))) {
          paste("added", format_year_span(setdiff(planned, requested)))
        })
    }
    message(sprintf("    %-11s %-22s (%s covers %s; %s)",
                    product,
                    if (length(planned)) format_year_span(planned) else "none",
                    dataset_acronym(product), format_year_span(covered),
                    paste(note, collapse = ", ")))
  }
  invisible(lapply(names(plan)[differs], report_one))

  # An added year is a year the caller did not ask for, so say why one exists
  # rather than leaving it to be discovered in the output directory.
  added_any <- vapply(plan, function(p) {
    length(setdiff(as.integer(p), requested)) > 0L
  }, logical(1))
  if (any(added_any)) {
    message("    Added years are tax history: ",
            paste(names(plan)[added_any], collapse = " and "),
            " are always built back to ", min(unlist(PRODUCT_YEAR_FLOOR)),
            ", because the employment panel behind them is a walk and a ",
            "shorter span would change the years you did ask for.")
  }
  invisible(plan)
}


# ---- Registry reading and period parsing --------------------------------

#' Dataset acronym for a dataset or product name
#' @keywords internal
#' @noRd
dataset_acronym <- function(dataset) {
  alias <- PRODUCT_DATASET_ALIASES[[tolower(dataset)]]
  if (!is.null(alias)) return(alias)
  toupper(dataset)
}


#' Years covered by every acronym in the bundled dataset registry
#'
#' Read once and cached: a build asks for it in every generator, in every
#' slice worker.
#'
#' @keywords internal
#' @noRd
dataset_year_registry <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    cached <<- read_dataset_year_registry()
    cached
  }
})


#' @keywords internal
#' @noRd
read_dataset_year_registry <- function() {
  path <- registry_file("plida_metadata", "datasets.csv")
  if (!nzchar(path) || !file.exists(path)) {
    # Better an unrestricted build than a build that refuses to run because a
    # metadata file moved. Every dataset then behaves as it did before the gate.
    warning("Dataset registry not found; dataset periods are unrestricted.",
            call. = FALSE)
    empty <- structure(list(), period_text = list())
    return(empty)
  }

  registry <- utils::read.csv(path, check.names = TRUE,
                              stringsAsFactors = FALSE)
  acronyms <- sort(unique(trimws(registry$Dataset.Acronym)))

  # An acronym appears on several rows -- CORE five times, STP twice -- one per
  # module. Their periods are the same today, but a union is the honest reading
  # of "the dataset covers this" if a module ever runs longer than its siblings.
  rows_for <- function(acronym) {
    is_row <- trimws(registry$Dataset.Acronym) == acronym
    trimws(registry$Reference.Period[is_row])
  }
  years <- lapply(acronyms, function(a) parse_reference_period(rows_for(a)))
  texts <- lapply(acronyms,
                  function(a) paste(unique(rows_for(a)), collapse = "; "))

  structure(stats::setNames(years, acronyms),
            period_text = stats::setNames(texts, acronyms))
}


#' Years named by one or more `Reference Period` strings
#'
#' One rule covers every form the registry uses. Split on commas; a token
#' reading `A to B` expands from the last year named in A to the last year
#' named in B, and any other token is the last year it names. The last year is
#' what a financial-year token means to a generator, so `2005-2006 to
#' 2023-2024` gives `2006:2024` and `2020 to 2025-2026` gives `2020:2026`.
#'
#' @param period Character vector of period strings.
#' @return An integer vector, sorted and unique. Empty when nothing parses,
#'   which callers read as "no declared period".
#' @keywords internal
#' @noRd
parse_reference_period <- function(period) {
  period <- period[!is.na(period) & nzchar(trimws(period))]
  if (length(period) == 0L) return(integer(0))

  expand_token <- function(token) {
    sides <- trimws(strsplit(token, "[[:space:]]+to[[:space:]]+")[[1]])
    ends <- vapply(sides[nzchar(sides)], last_named_year, integer(1),
                   USE.NAMES = FALSE)
    ends <- ends[!is.na(ends)]
    if (length(ends) == 0L) return(integer(0))
    seq.int(min(ends), max(ends))
  }

  tokens <- trimws(unlist(strsplit(period, ",", fixed = TRUE),
                          use.names = FALSE))
  tokens <- tokens[nzchar(tokens)]
  sort(unique(unlist(lapply(tokens, expand_token), use.names = FALSE)))
}


#' Last year named in a period token, resolving "current"
#' @keywords internal
#' @noRd
last_named_year <- function(text) {
  found <- regmatches(text, gregexpr("(19|20)[0-9]{2}", text))[[1]]
  if (length(found) > 0L) return(as.integer(found[length(found)]))
  if (grepl("current", text, ignore.case = TRUE)) {
    return(PLIDA_CURRENT_PERIOD_YEAR)
  }
  NA_integer_
}


#' Write a year vector compactly, collapsing consecutive runs
#'
#' `2015:2023` reads as "2015-2023" and the Census years as
#' "2011, 2016, 2021", which is how the registry writes them and how a message
#' about them stays readable.
#'
#' @keywords internal
#' @noRd
format_year_span <- function(years) {
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0L) return("no years")
  run <- cumsum(c(TRUE, diff(years) != 1L))
  runs <- split(years, run)
  as_text <- vapply(runs, function(y) {
    if (length(y) == 1L) as.character(y) else paste0(min(y), "-", max(y))
  }, character(1))
  paste(as_text, collapse = ", ")
}
