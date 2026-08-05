#!/usr/bin/env Rscript

# Rebuild Census codeframes from official ABS detailed-microdata files and
# the 2021 Census Dictionary. The generated registry is read at run time; a
# released fplida package therefore does not need internet access.

required_packages <- c("readxl", "rvest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install the following packages before rebuilding Census codeframes: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

.script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[startsWith(args, "--file=")]
  if (length(file_arg)) {
    return(normalizePath(
      sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE
    ))
  }
  normalizePath(
    file.path("data-raw", "update_census_codeframes.R"), mustWork = TRUE
  )
}

repo_root <- normalizePath(
  file.path(dirname(.script_path()), ".."), winslash = "/", mustWork = TRUE
)

sources <- data.frame(
  year = c(2011L, 2016L),
  file = c("census-2011-expanded.xls", "census-2016-detailed.xls"),
  url = c(
    paste0(
      "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&",
      "Expanded%20CSF%20Data%20item%20list.xls&2037.0.30.001&",
      "Data%20Cubes&4D86558DCFA7A018CA257C3E000EE68E&0&2011&",
      "13.12.2013&Latest"
    ),
    paste0(
      "https://www.abs.gov.au/statistics/microdata-tablebuilder/",
      "available-microdata-tablebuilder/census-population-and-housing/",
      "data%20item%20list%20-%205%20percent%20detailed%20microdata.xls"
    )
  ),
  sha256 = c(
    "ad3f24ef778a8c15bebbe35cb5628456a8ccb8ea0fe291c1b83d965ce08f9647",
    "75e8d4467c23cb4a43200a65e1d377228eb31b5d10437a65a15ec3c4118e84de"
  ),
  source_release_date = c("2013-12-12", "2017-01-10"),
  stringsAsFactors = FALSE
)

source_2021 <- list(
  file = "census-2021-detailed.xlsx",
  url = paste0(
    "https://www.abs.gov.au/statistics/microdata-tablebuilder/",
    "available-microdata-tablebuilder/census-population-and-housing/",
    "Data%20item%20list%20-%202021%20Census%20detailed%20microdata_",
    "18052023.xlsx"
  ),
  sha256 = "c120b7ad9678f47980ddb90427fda9fb42565be91c335da5bec83617e5244d25",
  source_release_date = "2023-05-18"
)

source_dir <- tempfile("fplida-census-codeframes-")
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
      "The official ABS Census file changed: ", sources$file[[i]],
      ". Review it before changing the expected checksum.",
      call. = FALSE
    )
  }
}
source_2021_path <- file.path(source_dir, source_2021$file)
utils::download.file(
  source_2021$url, source_2021_path, mode = "wb", quiet = TRUE
)
observed_2021 <- sha256_file(source_2021_path)
if (!identical(observed_2021, source_2021$sha256)) {
  stop(
    "The official 2021 Census detailed-microdata file changed. Review it ",
    "before changing the expected checksum.",
    call. = FALSE
  )
}

classify_value <- function(code, label) {
  label_lower <- tolower(trimws(ifelse(is.na(label), "", label)))
  if (grepl("not applicable|does not apply", label_lower)) {
    return("not_applicable")
  }
  if (grepl("not stated|not known", label_lower)) return("not_stated")
  if (grepl("overseas visitor", label_lower)) return("overseas_visitor")
  if (grepl(
    "inadequately described|undefined|supplementary code", label_lower
  )) {
    return("other_status")
  }
  if (grepl("^[0-9]+[[:space:]]*-[[:space:]]*[0-9]+$", code)) {
    return("range")
  }
  "substantive"
}

clean_variable <- function(value) {
  value <- trimws(as.character(value))
  sub("[[:space:]*]+$", "", value)
}

extract_workbook_sheet <- function(path, year, sheet, source_url) {
  cells <- suppressMessages(readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  ))
  cells <- as.matrix(cells)
  if (year == 2011L) {
    variable_column <- 1L
    # Column 3 is the released Expanded CSF classification value. Column 4
    # shows the source Census codes grouped into that released value.
    code_column <- 3L
    label_column <- 5L
  } else {
    variable_column <- 1L
    # Column 4 is the value in the detailed DataLab test file. Column 5 shows
    # the corresponding dictionary-format code (for example, INGP &).
    code_column <- 4L
    label_column <- 6L
  }
  current <- NA_character_
  rows <- list()
  row_n <- 0L
  for (i in seq_len(nrow(cells))) {
    candidate <- clean_variable(cells[i, variable_column])
    if (!is.na(candidate) && grepl("^[A-Z][A-Z0-9_]*$", candidate)) {
      current <- candidate
    }
    code <- trimws(cells[i, code_column])
    label <- trimws(cells[i, label_column])
    if (
      is.na(current) || is.na(code) || !nzchar(code) ||
        tolower(code) %in% c("numeric", "n/a", "not applicable") ||
        is.na(label) || !nzchar(label)
    ) {
      next
    }
    row_n <- row_n + 1L
    rows[[row_n]] <- data.frame(
      year = year,
      variable = current,
      code = code,
      label = label,
      value_kind = classify_value(code, label),
      source_sheet = sheet,
      source_url = source_url,
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

workbook_values <- list()
workbook_n <- 0L
for (source_i in seq_len(nrow(sources))) {
  source <- sources[source_i, , drop = FALSE]
  path <- file.path(source_dir, source$file[[1L]])
  sheets <- readxl::excel_sheets(path)
  classification_sheets <- sheets[grepl(
    "Dwelling|Family|Person", sheets, ignore.case = TRUE
  )]
  for (sheet in classification_sheets) {
    workbook_n <- workbook_n + 1L
    workbook_values[[workbook_n]] <- extract_workbook_sheet(
      path, source$year[[1L]], sheet, source$url[[1L]]
    )
  }
}
workbook_values <- do.call(rbind, workbook_values)

# The detailed-microdata workbooks enumerate only the classifications carried
# on those particular sample files. The integrated Census products also expose
# standard Census variables which are not on those samples. The archived ABS
# Census Dictionaries provide the authoritative full classifications for 2011
# and 2016.
legacy_dictionary_indexes <- data.frame(
  year = c(2011L, 2016L),
  url = c(
    paste0(
      "https://www.abs.gov.au/ausstats/abs%40.nsf/Lookup/",
      "2901.0Main%20Features302011"
    ),
    paste0(
      "https://www.abs.gov.au/ausstats/abs%40.nsf/Lookup/",
      "2901.0Chapter99882016"
    )
  ),
  stringsAsFactors = FALSE
)

legacy_dictionary_links <- function(year, index_url) {
  page <- rvest::read_html(index_url)
  anchors <- rvest::html_elements(page, "a")
  links <- data.frame(
    variable = trimws(rvest::html_text2(anchors)),
    href = rvest::html_attr(anchors, "href"),
    stringsAsFactors = FALSE
  )
  links <- links[
    grepl("^[A-Z][A-Z0-9]+$", links$variable) &
      grepl(paste0("2901\\.0Chapter[0-9]+", year), links$href),
    ,
    drop = FALSE
  ]
  links <- links[!duplicated(links$variable), , drop = FALSE]
  links$year <- as.integer(year)
  links$index_url <- index_url
  links
}

legacy_links <- do.call(rbind, lapply(
  seq_len(nrow(legacy_dictionary_indexes)),
  function(i) legacy_dictionary_links(
    legacy_dictionary_indexes$year[[i]],
    legacy_dictionary_indexes$url[[i]]
  )
))

split_html_lines <- function(value) {
  value <- strsplit(as.character(value), "\n", fixed = TRUE)[[1L]]
  value <- trimws(value)
  value[nzchar(value)]
}

extract_legacy_dictionary_page <- function(i) {
  link <- legacy_links[i, , drop = FALSE]
  source_url <- if (grepl("^https?://", link$href[[1L]])) {
    link$href[[1L]]
  } else {
    paste0("https://www.abs.gov.au", link$href[[1L]])
  }
  page <- tryCatch(rvest::read_html(source_url), error = function(error) NULL)
  if (is.null(page)) return(NULL)
  content <- rvest::html_element(page, "#printcontent")
  if (inherits(content, "xml_missing")) return(NULL)
  rows <- rvest::html_elements(content, "table tr")
  direct_cells <- lapply(
    rows, rvest::html_elements, xpath = "./td"
  )
  first_cell <- vapply(
    direct_cells,
    function(cells) {
      if (!length(cells)) return("")
      trimws(rvest::html_text2(cells[[1L]]))
    },
    character(1)
  )
  start <- which(grepl("^Categories:", first_cell))
  if (!length(start)) return(NULL)
  start <- start[[1L]]

  parsed <- list()
  parsed_n <- 0L
  for (row_i in seq.int(start, length(rows))) {
    if (row_i > start && grepl("^Number of categories:", first_cell[[row_i]])) {
      break
    }
    cells <- direct_cells[[row_i]]
    if (!length(cells)) next
    cell_text <- trimws(vapply(cells, rvest::html_text2, character(1)))
    cell_text <- cell_text[nzchar(cell_text)]
    cell_text <- cell_text[cell_text != "Categories:"]
    if (length(cell_text) < 2L) next

    code_cell <- NA_integer_
    code_lines <- character()
    for (cell_i in seq_len(length(cell_text) - 1L)) {
      candidate <- split_html_lines(cell_text[[cell_i]])
      candidate <- gsub(
        "[[:space:]]*-[[:space:]]*", "-", candidate, perl = TRUE
      )
      if (length(candidate) && all(grepl(
        "^[0-9A-Za-z@&]+(?:-[0-9A-Za-z@&]+)?$",
        candidate,
        perl = TRUE
      ))) {
        code_cell <- cell_i
        code_lines <- candidate
        break
      }
    }
    if (is.na(code_cell) || code_cell == length(cell_text)) next
    label_lines <- split_html_lines(cell_text[[code_cell + 1L]])
    if (!length(label_lines)) next
    if (length(code_lines) != length(label_lines)) {
      if (length(code_lines) == 1L) {
        label_lines <- paste(label_lines, collapse = " ")
      } else {
        next
      }
    }
    for (value_i in seq_along(code_lines)) {
      parsed_n <- parsed_n + 1L
      parsed[[parsed_n]] <- data.frame(
        year = link$year[[1L]],
        variable = link$variable[[1L]],
        code = code_lines[[value_i]],
        label = label_lines[[value_i]],
        value_kind = classify_value(
          code_lines[[value_i]], label_lines[[value_i]]
        ),
        source_sheet = paste0(
          link$year[[1L]], " Census Dictionary category table"
        ),
        source_url = source_url,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(parsed)) return(NULL)
  do.call(rbind, parsed)
}

legacy_dictionary_values <- if (.Platform$OS.type == "unix") {
  parallel::mclapply(
    seq_len(nrow(legacy_links)),
    extract_legacy_dictionary_page,
    mc.cores = 4L
  )
} else {
  lapply(seq_len(nrow(legacy_links)), extract_legacy_dictionary_page)
}
legacy_dictionary_values <- legacy_dictionary_values[
  !vapply(legacy_dictionary_values, is.null, logical(1))
]
legacy_dictionary_values <- do.call(rbind, legacy_dictionary_values)

dictionary_base <- paste0(
  "https://www.abs.gov.au/census/guide-census-data/",
  "census-dictionary/2021"
)
index_url <- paste0(dictionary_base, "/variables-index")
index <- rvest::read_html(index_url)
anchors <- rvest::html_elements(index, "a")
links <- unique(data.frame(
  variable = trimws(rvest::html_text2(anchors)),
  href = rvest::html_attr(anchors, "href"),
  stringsAsFactors = FALSE
))
links <- links[
  grepl("^[A-Z][A-Z0-9]+$", links$variable) &
    grepl("/variables-topic/", links$href),
  ,
  drop = FALSE
]

extract_dictionary_page <- function(i) {
  source_url <- paste0("https://www.abs.gov.au", links$href[[i]])
  page <- rvest::read_html(source_url)
  tables <- rvest::html_table(
    rvest::html_elements(page, "table"),
    fill = TRUE,
    convert = FALSE
  )
  category_table <- which(vapply(
    tables,
    function(x) identical(names(x), c("Code", "Category")),
    logical(1)
  ))
  if (length(category_table)) {
    values <- as.data.frame(
      tables[[category_table[[1L]]]], stringsAsFactors = FALSE
    )
    values <- data.frame(
      Code = trimws(as.character(values$Code)),
      Category = trimws(as.character(values$Category)),
      stringsAsFactors = FALSE
    )
  } else {
    # Large standard classifications are displayed as indented hierarchies.
    # In each row the code is followed by its label in the next populated
    # column. Capture every level here; the detailed-microdata item length is
    # applied at run time to retain the correct level.
    hierarchical <- list()
    hierarchical_n <- 0L
    for (table in tables) {
      cells <- as.matrix(table)
      for (row in seq_len(nrow(cells))) {
        row_values <- trimws(as.character(cells[row, ]))
        for (column in seq_len(max(ncol(cells) - 1L, 0L))) {
          code <- row_values[[column]]
          combined <- regmatches(
            code,
            regexec(
              "^([0-9A-Z@&]+)[[:space:]]+[-–][[:space:]]+(.+)$",
              code,
              perl = TRUE
            )
          )[[1L]]
          if (length(combined)) {
            hierarchical_n <- hierarchical_n + 1L
            hierarchical[[hierarchical_n]] <- data.frame(
              Code = combined[[2L]],
              Category = combined[[3L]],
              stringsAsFactors = FALSE
            )
            next
          }
          valid_code <- !is.na(code) && grepl(
            "^(?:[0-9@&V]+|[A-Z]{1,4})$", code, perl = TRUE
          )
          if (!valid_code) next
          label_candidates <- row_values[seq.int(column + 1L, ncol(cells))]
          label_candidates <- label_candidates[
            !is.na(label_candidates) & nzchar(label_candidates)
          ]
          if (!length(label_candidates)) next
          hierarchical_n <- hierarchical_n + 1L
          hierarchical[[hierarchical_n]] <- data.frame(
            Code = code,
            Category = label_candidates[[length(label_candidates)]],
            stringsAsFactors = FALSE
          )
        }
      }
    }
    if (!length(hierarchical)) return(NULL)
    values <- do.call(rbind, hierarchical)
  }
  numeric_range <- grepl(
    "^[0-9]+[[:space:]]*-[[:space:]]*[0-9]+$", values$Code
  )
  values$Code[numeric_range] <- gsub(
    "[[:space:]]", "", values$Code[numeric_range]
  )
  # Several dictionary tables use visually merged heading rows in the Code
  # column. They describe groups but are not values that occur in microdata.
  valid_code <- grepl(
    "^[0-9A-Za-z@&]+(?:-[0-9A-Za-z@&]+)?$",
    values$Code,
    perl = TRUE
  )
  values <- values[
    !is.na(values$Code) & nzchar(values$Code) & valid_code &
      !is.na(values$Category) & nzchar(values$Category),
    ,
    drop = FALSE
  ]
  data.frame(
    year = 2021L,
    variable = links$variable[[i]],
    code = values$Code,
    label = values$Category,
    value_kind = mapply(
      classify_value, values$Code, values$Category, USE.NAMES = FALSE
    ),
    source_sheet = "2021 Census Dictionary category table",
    source_url = source_url,
    stringsAsFactors = FALSE
  )
}

dictionary_values <- if (.Platform$OS.type == "unix") {
  parallel::mclapply(seq_len(nrow(links)), extract_dictionary_page, mc.cores = 4L)
} else {
  lapply(seq_len(nrow(links)), extract_dictionary_page)
}
dictionary_values <- dictionary_values[
  !vapply(dictionary_values, is.null, logical(1))
]
dictionary_values <- do.call(rbind, dictionary_values)

length_rows <- lapply(
  readxl::excel_sheets(source_2021_path)[-1L],
  function(sheet) {
    cells <- suppressMessages(readxl::read_excel(
      source_2021_path,
      sheet = sheet,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    ))
    cells <- as.matrix(cells)
    data.frame(
      year = 2021L,
      variable = trimws(cells[, 1L]),
      data_item_length = suppressWarnings(as.integer(cells[, 3L])),
      stringsAsFactors = FALSE
    )
  }
)
data_item_lengths <- do.call(rbind, length_rows)
data_item_lengths <- data_item_lengths[
  grepl("^[A-Z][A-Z0-9_]*$", data_item_lengths$variable) &
    !is.na(data_item_lengths$data_item_length),
  ,
  drop = FALSE
]
data_item_lengths <- unique(data_item_lengths)

values <- rbind(
  workbook_values,
  legacy_dictionary_values,
  dictionary_values
)
values <- values[
  !duplicated(paste(values$year, values$variable, values$code, sep = "\r")),
  ,
  drop = FALSE
]
values <- values[order(values$year, values$variable), , drop = FALSE]
rownames(values) <- NULL

infer_data_item_length <- function(frame) {
  status <- frame$code[frame$value_kind %in% c(
    "not_applicable", "not_stated", "overseas_visitor"
  )]
  if (length(status)) return(max(nchar(status)))
  code <- sub("-.*$", "", frame$code)
  max(nchar(code))
}

inferred_lengths <- do.call(rbind, lapply(
  split(
    values,
    paste(
      values$year,
      values$variable,
      values$source_sheet,
      values$source_url,
      sep = "\r"
    )
  ),
  function(frame) data.frame(
    year = frame$year[[1L]],
    variable = frame$variable[[1L]],
    source_sheet = frame$source_sheet[[1L]],
    source_url = frame$source_url[[1L]],
    inferred_data_item_length = infer_data_item_length(frame),
    stringsAsFactors = FALSE
  )
))

metadata <- utils::read.csv(
  file.path(repo_root, "inst", "plida_metadata", "variables.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
metadata <- metadata[metadata$Dataset == "CENSUS", , drop = FALSE]
metadata$year <- as.integer(sub(
  ".*_([0-9]{4})_.*", "\\1", metadata$`Table Name`, perl = TRUE
))
inventory <- unique(metadata[, c("year", "Variable Name")])
names(inventory)[[2L]] <- "variable"

source_mapping <- unique(values[, c(
  "year", "variable", "source_sheet", "source_url"
)])
# Prefer the detailed-microdata workbook when it enumerates the field. The
# Census Dictionary then supplies fields absent from that product-specific
# workbook.
source_mapping$priority <- ifelse(
  grepl("Census Dictionary", source_mapping$source_sheet), 2L, 1L
)
source_mapping <- source_mapping[order(
  source_mapping$year,
  source_mapping$variable,
  source_mapping$priority
), , drop = FALSE]
source_mapping <- source_mapping[
  !duplicated(paste(source_mapping$year, source_mapping$variable, sep = "\r")),
  c("year", "variable", "source_sheet", "source_url"),
  drop = FALSE
]

mapping <- merge(
  inventory,
  source_mapping,
  by = c("year", "variable"),
  all.x = TRUE,
  sort = TRUE
)
mapping <- merge(
  mapping,
  data_item_lengths,
  by = c("year", "variable"),
  all.x = TRUE,
  sort = TRUE
)
mapping <- merge(
  mapping,
  inferred_lengths,
  by = c("year", "variable", "source_sheet", "source_url"),
  all.x = TRUE,
  sort = TRUE
)
mapping$data_item_length[is.na(mapping$data_item_length)] <-
  mapping$inferred_data_item_length[is.na(mapping$data_item_length)]
mapping$inferred_data_item_length <- NULL
mapping$supported <- !is.na(mapping$source_url) & nzchar(mapping$source_url)
mapping$unsupported_reason <- ifelse(
  mapping$supported, "", "not_enumerated_in_official_source"
)
mapping$source_sheet[is.na(mapping$source_sheet)] <- ""
mapping$source_url[is.na(mapping$source_url)] <- ifelse(
  mapping$year[is.na(mapping$source_url)] == 2021L,
  index_url,
  sources$url[match(
    mapping$year[is.na(mapping$source_url)], sources$year
  )]
)

stopifnot(
  !anyDuplicated(paste(mapping$year, mapping$variable, sep = "\r")),
  !anyDuplicated(paste(values$year, values$variable, values$code, sep = "\r")),
  all(values$value_kind %in% c(
    "substantive", "range", "not_applicable", "not_stated",
    "overseas_visitor", "other_status"
  )),
  nrow(values) > 5000L
)

output_dir <- file.path(repo_root, "inst", "extdata", "codeframes")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  mapping,
  file.path(output_dir, "census-variable-codeframes.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  values,
  file.path(output_dir, "census-codeframe-values.csv"),
  row.names = FALSE,
  na = ""
)

cat(
  "Wrote ", nrow(mapping), " Census variable-year mappings and ",
  nrow(values), " official code values.\n",
  sep = ""
)
