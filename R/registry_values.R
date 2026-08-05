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

  parsed <- lapply(info$valid_values, function(raw) {
    values <- tryCatch(
      as.character(unlist(jsonlite::fromJSON(raw), use.names = FALSE)),
      error = function(e) character(0)
    )
    # "1: New South Wales" -> "1". A bare code is left alone.
    values <- sub("^\\s*([^:]+?)\\s*:.*$", "\\1", values)
    values <- trimws(values)
    values[nzchar(values)]
  })

  names(parsed) <- key
  parsed <- parsed[lengths(parsed) > 0L]
  assign("lookup", parsed, envir = .registry_value_cache)
  parsed
}

#' Registry codes for one dataset-and-variable pair
#'
#' @return A character vector of codes, or `NULL` when the registry documents
#'   no value list for the pair.
#' @keywords internal
#' @noRd
.registry_values_for <- function(dataset, name) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  lookup <- .registry_value_lookup()
  if (!length(lookup)) return(NULL)
  lookup[[paste(dataset, toupper(name), sep = "\r")]]
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
