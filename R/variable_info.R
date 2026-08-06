#' Get dataset information
#'
#' `dataset_info()` returns the central dataset registry for PLIDA and BLADE.
#' Each row describes one dataset. The registry gives the dataset description,
#' source agency, reference period, metadata source and an information website.
#'
#' @param dataset Optional character vector of dataset codes. The match is not
#'   case-sensitive.
#' @param asset Optional character vector of asset codes. The match is not
#'   case-sensitive.
#'
#' @return A data frame with one row for each selected dataset.
#'
#' @seealso [variable_info()]
#' @export
dataset_info <- function(dataset = NULL, asset = NULL) {
  info <- .read_fplida_registry("dataset-info.csv")

  filters <- list(
    dataset = .registry_filter(info, "dataset", dataset, "dataset"),
    asset = .registry_filter(info, "asset", asset, "asset")
  )

  .filter_fplida_registry(info, filters)
}

#' Get variable information
#'
#' `variable_info()` returns the central variable registry for PLIDA and
#' BLADE. Each row records one occurrence of a variable or BLADE linking key
#' in the official metadata. A variable can occur in more than one product or
#' table.
#'
#' The registry gives descriptions, source details, periods, value information,
#' support status and topic tags. The `valid_values` variable contains a JSON
#' array of display entries for normalised value sets that are available in
#' the bundled public source material. Entries are raw values or
#' `value: label` strings. Use the linked source before treating an entry as a
#' machine code. The raw BLADE response text remains in
#' `official_valid_response` when no normalised array is available.
#' `variable_type` and `variable_level` are blank when the source metadata does
#' not supply them.
#'
#' @section Value support:
#' `value_support_status` records where a variable's value domain came from.
#'
#' `sourced` means published metadata gives the value domain: the codes in
#' `valid_values` come from a classification or code list, and `value_source`
#' names it.
#'
#' `guessed` means the value domain is inferred from the variable's name and
#' description, not from a published list. A variable whose name ends `_STATE`
#' is given the Australian state codes on that basis alone. The generated
#' column therefore holds plausible values of the right shape, but the source
#' does not confirm them. Treat the codes as a placeholder, not a mapping.
#'
#' `unsupported` means the value domain is neither published nor inferable, so
#' the column is written as typed missing rather than given invented codes. The
#' `limitation` variable gives the reason.
#'
#' Survey variables use `not_applicable` because the registry does not assess
#' survey values.
#'
#' @param dataset Optional character vector of dataset codes. The match is not
#'   case-sensitive.
#' @param asset Optional character vector of asset codes. The match is not
#'   case-sensitive.
#' @param topic Optional character vector of topic tags. Each value must match
#'   one complete comma-separated tag. The function does not use partial
#'   matches.
#' @param collection_type Optional character vector of collection types.
#'   The match is not case-sensitive.
#' @param record_type Optional character vector of record types. The match is
#'   not case-sensitive.
#' @param value_support_status Optional character vector of value-support
#'   statuses. The match is not case-sensitive.
#'
#' @return A data frame with one row for each selected variable occurrence.
#'
#' @seealso [dataset_info()]
#' @export
variable_info <- function(dataset = NULL, asset = NULL, topic = NULL,
                          collection_type = NULL, record_type = NULL,
                          value_support_status = NULL) {
  info <- .read_fplida_registry("variable-info.csv.gz")

  filters <- list(
    dataset = .registry_filter(info, "dataset", dataset, "dataset"),
    asset = .registry_filter(info, "asset", asset, "asset"),
    collection_type = .registry_filter(
      info, "collection_type", collection_type, "collection_type"
    ),
    record_type = .registry_filter(
      info, "record_type", record_type, "record_type"
    ),
    value_support_status = .registry_filter(
      info,
      "value_support_status",
      value_support_status,
      "value_support_status"
    )
  )
  topic_filter <- .registry_topic_filter(info, topic)

  result <- .filter_fplida_registry(info, filters)
  if (!is.null(topic_filter)) {
    row_topics <- strsplit(result$topic_tags, ",", fixed = TRUE)
    keep <- vapply(
      row_topics,
      function(tags) any(tolower(trimws(tags)) %in% topic_filter),
      logical(1)
    )
    result <- result[keep, , drop = FALSE]
    rownames(result) <- NULL
  }

  result
}

.fplida_registry_cache <- new.env(parent = emptyenv())

.fplida_registry_path <- function(filename) {
  installed <- system.file(filename, package = "fplida")
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }

  roots <- unique(c(
    getwd(),
    dirname(getwd()),
    dirname(dirname(getwd()))
  ))
  candidates <- file.path(roots, "inst", filename)
  found <- candidates[file.exists(candidates)]
  if (length(found)) {
    return(found[[1L]])
  }

  stop(
    sprintf(
      "Cannot find `%s`. Reinstall fplida or run this function from the package source tree.",
      filename
    ),
    call. = FALSE
  )
}

.read_fplida_registry <- function(filename) {
  path <- normalizePath(
    .fplida_registry_path(filename), winslash = "/", mustWork = TRUE
  )
  details <- file.info(path)
  signature <- paste(
    path, details$size, as.numeric(details$mtime), sep = "|"
  )
  cached <- get0(filename, envir = .fplida_registry_cache, inherits = FALSE)

  if (!is.null(cached) && identical(cached$signature, signature)) {
    return(cached$data)
  }

  data <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  assign(
    filename,
    list(signature = signature, data = data),
    envir = .fplida_registry_cache
  )
  data
}

.normalise_registry_values <- function(values, argument) {
  if (is.null(values)) {
    return(NULL)
  }
  if (!is.character(values) || !length(values) || anyNA(values)) {
    stop(
      sprintf("`%s` must be NULL or a character vector.", argument),
      call. = FALSE
    )
  }

  values <- trimws(values)
  if (any(!nzchar(values))) {
    stop(
      sprintf("`%s` cannot contain an empty value.", argument),
      call. = FALSE
    )
  }
  unique(tolower(values))
}

.registry_filter <- function(info, variable, values, argument) {
  values <- .normalise_registry_values(values, argument)
  if (is.null(values)) {
    return(NULL)
  }

  available <- unique(tolower(info[[variable]]))
  unknown <- values[!values %in% available]
  if (length(unknown)) {
    stop(
      sprintf(
        "Unknown `%s` value%s: %s.",
        argument,
        if (length(unknown) == 1L) "" else "s",
        paste(sprintf('"%s"', unknown), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  values
}

.registry_topic_filter <- function(info, topic) {
  topic <- .normalise_registry_values(topic, "topic")
  if (is.null(topic)) {
    return(NULL)
  }

  available <- unique(tolower(trimws(unlist(
    strsplit(info$topic_tags, ",", fixed = TRUE),
    use.names = FALSE
  ))))
  unknown <- topic[!topic %in% available]
  if (length(unknown)) {
    stop(
      sprintf(
        "Unknown `topic` value%s: %s. Use a complete topic tag.",
        if (length(unknown) == 1L) "" else "s",
        paste(sprintf('"%s"', unknown), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  topic
}

.filter_fplida_registry <- function(info, filters) {
  keep <- rep(TRUE, nrow(info))
  for (variable in names(filters)) {
    values <- filters[[variable]]
    if (!is.null(values)) {
      keep <- keep & tolower(info[[variable]]) %in% values
    }
  }

  result <- info[keep, , drop = FALSE]
  rownames(result) <- NULL
  result
}
