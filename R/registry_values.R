# Emit the codes the registry documents.
#
# The canonical-structure generator infers a column's values from its name
# (see `.dil_general_value()`), which it has always done independently of the
# variable registry. That means a column the registry documents precisely — the
# nine Australian state codes, say — was still filled by a generic categorical
# heuristic.
#
# These helpers close that gap. Where the registry carries a value list for a
# dataset-and-variable pair, generation draws from THAT list. The list may be
# `sourced` (a published classification) or `guessed` (inferred from the
# variable name); both beat a generic heuristic, and a guessed domain is
# labelled as such in the registry so the user knows which they have.

.registry_value_cache <- new.env(parent = emptyenv())

# (dataset, UPPERCASED variable) -> character vector of bare codes.
#
# `valid_values` entries are either "code: label" or a bare code. Only the code
# is emitted: the label is documentation, not data.
.registry_value_lookup <- function() {
  cached <- get0("lookup", envir = .registry_value_cache, inherits = FALSE)
  if (!is.null(cached)) return(cached)

  info <- tryCatch(variable_info(), error = function(e) NULL)
  if (is.null(info) || !nrow(info)) {
    assign("lookup", list(), envir = .registry_value_cache)
    return(list())
  }

  keep <- nzchar(info$valid_values) & trimws(info$valid_values) != "[]"
  info <- info[keep, c("dataset", "variable", "valid_values"), drop = FALSE]
  if (!nrow(info)) {
    assign("lookup", list(), envir = .registry_value_cache)
    return(list())
  }

  key <- paste(info$dataset, toupper(info$variable), sep = "\r")
  info <- info[!duplicated(key), , drop = FALSE]
  key <- key[!duplicated(key)]

  # Both halves are kept. The code is the data, but the label is not merely
  # documentation: a column named `..._NAME` beside a `..._CODE` holds the
  # label, and emitting the code into it would be wrong data rather than
  # imprecise data.
  parsed <- lapply(info$valid_values, function(raw) {
    values <- tryCatch(
      as.character(unlist(jsonlite::fromJSON(raw), use.names = FALSE)),
      error = function(e) character(0)
    )
    # "1: New South Wales" -> code "1", label "New South Wales".
    # A bare code is left alone and its label is the code.
    code <- trimws(sub("^\\s*([^:]+?)\\s*:.*$", "\\1", values))
    label <- trimws(sub("^\\s*[^:]+?\\s*:\\s*", "", values))
    keep <- nzchar(code)
    data.frame(
      code = code[keep],
      label = ifelse(nzchar(label[keep]), label[keep], code[keep]),
      stringsAsFactors = FALSE
    )
  })

  names(parsed) <- key
  parsed <- parsed[vapply(parsed, nrow, integer(1)) > 0L]
  assign("lookup", parsed, envir = .registry_value_cache)
  parsed
}

#' Registry codes and labels for one dataset-and-variable pair
#'
#' @return A data frame with `code` and `label`, or `NULL`.
#' @keywords internal
#' @noRd
.registry_labelled_for <- function(dataset, name) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  lookup <- .registry_value_lookup()
  if (!length(lookup)) return(NULL)
  lookup[[paste(dataset, toupper(name), sep = "\r")]]
}

#' Registry codes for one dataset-and-variable pair
#'
#' @return A character vector of codes, or `NULL` when the registry documents
#'   no value list for the pair.
#' @keywords internal
#' @noRd
.registry_values_for <- function(dataset, name) {
  values <- .registry_labelled_for(dataset, name)
  if (is.null(values)) return(NULL)
  values$code
}

#' Draw an area column from the registry, anchored to the person's state
#'
#' ASGS codes carry their state in the leading digit, so drawing uniformly from
#' a national list hands a Queenslander a Victorian mesh block. The package
#' promises the opposite — that a person's geography agrees across products —
#' and `.dil_pick_area_value()` is what keeps that promise everywhere else.
#' This routes documented area codes through it.
#'
#' Returns `NULL` when the registry list is not state-decodable, which is the
#' signal to fall back rather than guess: a caller that cannot honour the
#' guarantee should write typed missing instead of breaking it.
#'
#' @keywords internal
#' @noRd
.registry_area_column <- function(dataset, name, spine_rows, seed, salt = 0L) {
  values <- .registry_labelled_for(dataset, name)
  if (is.null(values) || !nrow(values)) return(NULL)
  n <- nrow(spine_rows)
  if (!n) return(NULL)

  state <- substr(values$code, 1L, 1L)
  usable <- state %in% as.character(1:8)
  # Codes like 999 "not linked" have no state and must not become one. They are
  # dropped from the draw rather than mapped somewhere arbitrary.
  if (sum(usable) < 2L) return(NULL)
  values <- values[usable, , drop = FALSE]
  values$state <- state[usable]
  # A single state's worth of codes is not a national classification; treating
  # it as one would anchor every person to that state.
  if (length(unique(values$state)) < 2L) return(NULL)

  # `.dil_pick_area_value()` reads the label from `name`, and returns it
  # instead of the code when the column is itself a `..._NAME`.
  values$name <- values$label
  .dil_pick_area_value(values, toupper(name), spine_rows, seed, salt)
}

#' Draw a column from the registry's documented codes
#'
#' Deterministic in `(seed, salt)`, matching the rest of the generator.
#'
#' @return A character vector of length `n`, or `NULL` when the registry
#'   documents no value list for the pair.
#' @keywords internal
#' @noRd
.registry_value_column <- function(dataset, name, n, seed, salt = 0L) {
  values <- .registry_values_for(dataset, name)
  if (is.null(values) || !length(values) || n <= 0L) return(NULL)
  .dil_sample_values(values, n, seed, salt)
}
