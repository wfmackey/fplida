# Rebuild the ACLD codeframe registry from the official ABS detailed
# microdata data-item workbooks.

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required to rebuild the ACLD codeframes.")
}

sources <- data.frame(
  product = c(
    "madipge-acld11-d-persons-11-16-21",
    "madipge-acld16-d-person-16-21"
  ),
  file = c("acld_1121.xlsx", "acld_1621.xlsx"),
  url = c(
    paste0(
      "https://www.abs.gov.au/statistics/microdata-tablebuilder/",
      "available-microdata-tablebuilder/australian-census-longitudinal-",
      "dataset/ACLD%202011_16_21%20detailed%20microdata.xlsx"
    ),
    paste0(
      "https://www.abs.gov.au/statistics/microdata-tablebuilder/",
      "available-microdata-tablebuilder/australian-census-longitudinal-",
      "dataset/ACLD%202016_21%20detailed%20microdata.xlsx"
    )
  ),
  sha256 = c(
    "bd0945f639fa8e8dafd59ca0b63df0276a590616063e26619a0eef9852cbecad",
    "f8378ca88da84ca470ea25b7eb7de43a8de8e6f26a6dc2525596df855abfb1c6"
  ),
  source_release_date = c("2023-12-19", "2023-12-19"),
  stringsAsFactors = FALSE
)

source_dir <- tempfile("fplida-acld-codeframes-")
dir.create(source_dir)
on.exit(unlink(source_dir, recursive = TRUE), add = TRUE)

sha256_file <- function(path) {
  output <- system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE)
  sub("[[:space:]].*$", "", output[[1L]])
}

for (i in seq_len(nrow(sources))) {
  path <- file.path(source_dir, sources$file[[i]])
  utils::download.file(sources$url[[i]], path, mode = "wb", quiet = TRUE)
  observed <- sha256_file(path)
  if (!identical(observed, sources$sha256[[i]])) {
    stop(
      "The ABS ACLD workbook changed: ", sources$file[[i]],
      ". Review the new workbook before updating the expected checksum."
    )
  }
}

metadata <- utils::read.csv(
  file.path("inst", "plida_metadata", "variables.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
metadata <- metadata[metadata$Dataset == "ACLD", , drop = FALSE]

classify_value <- function(code, label) {
  if (grepl("^Not stated(?:$|[[:space:]-])", label, ignore.case = TRUE)) {
    return("not_stated")
  }
  if (grepl("^Not applicable(?:$|[[:space:]-])", label, ignore.case = TRUE)) {
    return("not_applicable")
  }
  if (grepl("^Unlinked record(?:$|[[:space:]-])", label, ignore.case = TRUE)) {
    return("unlinked")
  }
  if (grepl("^[0-9]+-[0-9]+$", code)) return("range")
  if (
    grepl("Census Dictionary|abs\\.gov", label, ignore.case = TRUE) ||
      grepl("digit", code, ignore.case = TRUE)
  ) {
    return("external_reference")
  }
  "substantive"
}

extract_candidates <- function(path, product, inventory) {
  ignored <- c("Cover", "Classifications by Topic ", "Classification Index")
  candidates <- list()
  candidate_n <- 0L
  inventory_upper <- toupper(inventory)

  for (sheet in setdiff(readxl::excel_sheets(path), ignored)) {
    cells <- suppressMessages(readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    ))
    cells <- as.matrix(cells)

    for (column in seq_len(ncol(cells))) {
      column_values <- trimws(cells[, column])
      rows <- which(
        !is.na(column_values) &
          toupper(column_values) %in% inventory_upper
      )
      for (row in rows) {
        variable <- inventory[match(toupper(column_values[[row]]), inventory_upper)]
        codes <- labels <- character()
        if (row < nrow(cells)) {
          for (value_row in seq.int(row + 1L, nrow(cells))) {
            code <- trimws(cells[value_row, column])
            if (
              is.na(code) || !nzchar(code) ||
                toupper(code) %in% inventory_upper
            ) {
              break
            }
            label <- if (column < ncol(cells)) {
              trimws(cells[value_row, column + 1L])
            } else {
              NA_character_
            }
            codes <- c(codes, code)
            labels <- c(labels, label)
          }
        }
        if (!length(codes)) next

        # Some workbook cells are visually merged and repeat the code in the
        # underlying sheet XML. Keep one row per official code and prefer the
        # row that carries its visible label.
        chosen <- vapply(split(seq_along(codes), codes), function(index) {
          labelled <- index[!is.na(labels[index]) & nzchar(labels[index])]
          if (length(labelled)) labelled[[1L]] else index[[1L]]
        }, integer(1))
        chosen <- sort(chosen)
        codes <- codes[chosen]
        labels <- labels[chosen]

        candidate_n <- candidate_n + 1L
        value_kind <- mapply(
          classify_value,
          codes,
          ifelse(is.na(labels), "", labels),
          USE.NAMES = FALSE
        )
        values <- data.frame(
          code = codes,
          label = labels,
          value_kind = value_kind,
          stringsAsFactors = FALSE
        )
        candidates[[candidate_n]] <- list(
          product = product,
          variable = variable,
          source_sheet = sheet,
          values = values,
          score = 1000L * sum(value_kind %in% c("substantive", "range")) +
            nrow(values)
        )
      }
    }
  }
  candidates
}

all_candidates <- list()
for (i in seq_len(nrow(sources))) {
  product <- sources$product[[i]]
  inventory <- unique(metadata$`Variable Name`[
    metadata$`Product Name` == product
  ])
  all_candidates <- c(
    all_candidates,
    extract_candidates(
      file.path(source_dir, sources$file[[i]]),
      product,
      inventory
    )
  )
}

candidate_key <- vapply(
  all_candidates,
  function(x) paste(x$product, toupper(x$variable), sep = "\r"),
  character(1)
)
best_candidates <- lapply(split(seq_along(all_candidates), candidate_key), function(index) {
  scores <- vapply(all_candidates[index], `[[`, integer(1), "score")
  all_candidates[[index[[which.max(scores)]]]]
})

candidate_signature <- vapply(best_candidates, function(candidate) {
  values <- candidate$values
  paste(
    values$code,
    ifelse(is.na(values$label), "", values$label),
    values$value_kind,
    sep = "\u241f",
    collapse = "\u241e"
  )
}, character(1))

best_order <- order(
  vapply(best_candidates, `[[`, character(1), "product"),
  vapply(best_candidates, `[[`, character(1), "variable")
)
signature_levels <- unique(candidate_signature[best_order])
frame_id <- sprintf("ACLD%03d", match(candidate_signature, signature_levels))

mapped <- do.call(rbind, lapply(seq_along(best_candidates), function(i) {
  candidate <- best_candidates[[i]]
  values <- candidate$values
  source_row <- sources[sources$product == candidate$product, , drop = FALSE]
  supported <- any(values$value_kind %in% c("substantive", "range"))
  data.frame(
    product = candidate$product,
    variable = candidate$variable,
    frame_id = frame_id[[i]],
    source_sheet = candidate$source_sheet,
    source_url = source_row$url[[1L]],
    source_release_date = source_row$source_release_date[[1L]],
    workbook_sha256 = source_row$sha256[[1L]],
    supported = supported,
    unsupported_reason = if (supported) "" else "external_classification_reference_only",
    stringsAsFactors = FALSE
  )
}))

inventory <- unique(metadata[, c("Product Name", "Variable Name")])
names(inventory) <- c("product", "variable")
inventory_key <- paste(inventory$product, toupper(inventory$variable), sep = "\r")
mapped_key <- paste(mapped$product, toupper(mapped$variable), sep = "\r")
missing <- inventory[!inventory_key %in% mapped_key, , drop = FALSE]
if (nrow(missing)) {
  missing$frame_id <- ""
  missing$source_sheet <- ""
  missing$source_url <- sources$url[match(missing$product, sources$product)]
  missing$source_release_date <- sources$source_release_date[
    match(missing$product, sources$product)
  ]
  missing$workbook_sha256 <- sources$sha256[
    match(missing$product, sources$product)
  ]
  missing$supported <- FALSE
  missing$unsupported_reason <- "not_enumerated_in_workbook"
  mapped <- rbind(mapped, missing)
}
mapped <- mapped[order(mapped$product, mapped$variable), , drop = FALSE]

first_frame <- match(signature_levels, candidate_signature)
values <- do.call(rbind, lapply(seq_along(first_frame), function(i) {
  candidate_values <- best_candidates[[first_frame[[i]]]]$values
  data.frame(
    frame_id = sprintf("ACLD%03d", i),
    display_order = seq_len(nrow(candidate_values)),
    candidate_values,
    stringsAsFactors = FALSE
  )
}))

output_dir <- file.path("inst", "extdata", "codeframes")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  mapped,
  file.path(output_dir, "acld-variable-codeframes.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  values,
  file.path(output_dir, "acld-codeframe-values.csv"),
  row.names = FALSE,
  na = ""
)

message(
  "Wrote ", nrow(mapped), " ACLD variable mappings and ",
  nrow(values), " unique codeframe values."
)
