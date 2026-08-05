#!/usr/bin/env Rscript

# Update packaged BLADE metadata from the current ABS DIL workbook.
#
# The April 2026 BLADE workbook is not a tidy multi-sheet DIL like the
# PLIDA workbook. It has one contents sheet, numbered table sheets, and
# appendices. This script normalises the numbered table sheets into two
# package CSVs:
#   - inst/blade_metadata/tables.csv
#   - inst/blade_metadata/variables.csv
#   - inst/blade_metadata/keys.csv
#   - inst/blade_metadata/domains.csv

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default) {
  hit <- which(args == flag)
  if (!length(hit) || hit == length(args)) return(default)
  args[[hit + 1L]]
}

input <- arg_value(
  "--input",
  file.path("data-raw", "blade_data_item_list_2026-04.xlsx")
)
metadata_dir <- arg_value("--metadata-dir", file.path("inst", "blade_metadata"))
audit_path <- arg_value(
  "--audit",
  file.path("data-raw", "blade_coverage_2026-04.csv")
)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("The readxl package is required to parse the BLADE workbook.",
       call. = FALSE)
}

clean_cell <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  trimws(gsub("[[:space:]]+", " ", x))
}

slugify <- function(x) {
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("(^-|-$)", "", x)
  substr(x, 1L, 70L)
}

table_name_from_title <- function(title) {
  out <- sub("^Table[[:space:]]+[0-9]+:[[:space:]]*", "", title)
  if (grepl(",", out, fixed = TRUE)) {
    return(trimws(sub(",[[:space:]]*[^,]*$", "", out)))
  }
  trimws(sub(
    "[[:space:]]+[0-9]{4}-[0-9]{2}[[:space:]]+to[[:space:]]+[0-9]{4}-[0-9]{2}$",
    "",
    out
  ))
}

reference_period_from_title <- function(title) {
  if (grepl(",", title, fixed = TRUE)) {
    return(trimws(sub("^.*,", "", title)))
  }
  out <- sub("^Table[[:space:]]+[0-9]+:[[:space:]]*", "", title)
  hit <- regmatches(
    out,
    regexpr(
      "[0-9]{4}-[0-9]{2}[[:space:]]+to[[:space:]]+[0-9]{4}-[0-9]{2}$",
      out
    )
  )
  if (length(hit) && nzchar(hit)) trimws(hit) else ""
}

table_scope <- function(table_number) {
  if (table_number <= 34L) "standard" else "requestable"
}

read_sheet <- function(path, sheet) {
  raw <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  raw[] <- lapply(raw, clean_cell)
  raw
}

find_header_row <- function(raw, sheet) {
  first <- clean_cell(raw[[1L]])
  candidates <- which(
    grepl("^(Item|Item Description|Description)$", first,
          ignore.case = TRUE)
  )
  if (!length(candidates)) {
    stop("Could not locate BLADE table header row in sheet ", sheet,
         call. = FALSE)
  }
  candidates[[1L]]
}

variable_level <- function(header, parent, direct_variable_header) {
  level <- header
  if (direct_variable_header) {
    level <- sub("^Variable[[:space:]]+[Nn]ame", "", level)
    level <- gsub("^\\s*\\((.*)\\)\\s*$", "\\1", level)
  } else if (grepl("variable[[:space:]]*name", parent, ignore.case = TRUE)) {
    level <- header
  }
  if (grepl("variable[[:space:]]*name", level, ignore.case = TRUE)) {
    level <- ""
  }
  trimws(level)
}

parse_table_sheet <- function(path, sheet) {
  raw <- read_sheet(path, sheet)
  table_number <- as.integer(sheet)
  title <- raw[4L, 1L]
  name <- table_name_from_title(title)
  reference_period <- reference_period_from_title(title)
  product_name <- sprintf("blade-table-%02d-%s", table_number, slugify(name))

  header_row <- find_header_row(raw, sheet)
  header <- clean_cell(unlist(raw[header_row, ], use.names = FALSE))
  parent <- if (header_row > 1L) {
    clean_cell(unlist(raw[header_row - 1L, ], use.names = FALSE))
  } else {
    rep("", length(header))
  }

  item_col <- which(
    grepl("^(Item|Item Description|Description)$", header,
          ignore.case = TRUE)
  )[[1L]]
  valid_col <- which(
    grepl("valid[[:space:]]*response", header, ignore.case = TRUE) |
      grepl("valid[[:space:]]*response", parent, ignore.case = TRUE)
  )[[1L]]

  direct_variable_cols <- which(
    grepl("variable[[:space:]]*name", header, ignore.case = TRUE)
  )
  if (length(direct_variable_cols)) {
    variable_cols <- direct_variable_cols
  } else {
    parent_variable_col <- which(
      grepl("variable[[:space:]]*name", parent, ignore.case = TRUE)
    )[[1L]]
    variable_cols <- parent_variable_col:(valid_col - 1L)
  }
  variable_cols <- variable_cols[
    variable_cols > item_col & variable_cols < valid_col
  ]

  period_cols <- if (valid_col < length(header)) {
    (valid_col + 1L):length(header)
  } else {
    integer(0)
  }
  period_cols <- period_cols[nzchar(header[period_cols])]

  table <- data.frame(
    Table.Number = table_number,
    Sheet = sheet,
    Table.Name = name,
    Table.Title = title,
    Product.Name = product_name,
    Reference.Period = reference_period,
    Scope = table_scope(table_number),
    Header.Row = header_row,
    stringsAsFactors = FALSE
  )

  rows <- list()
  k <- 0L
  for (row in seq.int(header_row + 1L, nrow(raw))) {
    if (!nzchar(paste(raw[row, ], collapse = ""))) next
    for (variable_col in variable_cols) {
      variable_name <- raw[row, variable_col]
      if (!nzchar(variable_name)) next

      available_periods <- character(0)
      for (period_col in period_cols) {
        if (nzchar(raw[row, period_col])) {
          available_periods <- c(available_periods, header[[period_col]])
        }
      }

      k <- k + 1L
      rows[[k]] <- data.frame(
        Table.Number = table_number,
        Sheet = sheet,
        Table.Name = name,
        Product.Name = product_name,
        Item = raw[row, item_col],
        Variable.Name = variable_name,
        Variable.Level = variable_level(
          header[[variable_col]],
          parent[[variable_col]],
          variable_col %in% direct_variable_cols
        ),
        Valid.Response = raw[row, valid_col],
        Available.Periods = paste(available_periods, collapse = ";"),
        stringsAsFactors = FALSE
      )
    }
  }

  list(table = table, variables = do.call(rbind, rows))
}

parse_key_sheet <- function(path, sheet) {
  raw <- read_sheet(path, sheet)
  title <- raw[4L, 1L]
  key_name <- sub("^Appendix[[:space:]]+[0-9]+:[[:space:]]*", "", title)
  key_name <- trimws(sub(",[[:space:]]*[^,]*$", "", key_name))
  product_name <- paste0("blade-key-", slugify(key_name))

  header_row <- find_header_row(raw, sheet)
  header <- clean_cell(unlist(raw[header_row, ], use.names = FALSE))
  next_header <- if (header_row < nrow(raw)) {
    clean_cell(unlist(raw[header_row + 1L, ], use.names = FALSE))
  } else {
    rep("", length(header))
  }

  valid_col <- which(
    grepl("valid[[:space:]]*response", header, ignore.case = TRUE)
  )[[1L]]
  variable_col <- which(
    grepl("variable[[:space:]]*name", header, ignore.case = TRUE)
  )[[1L]]
  period_cols <- if (valid_col < length(header)) {
    (valid_col + 1L):length(header)
  } else {
    integer(0)
  }
  period_labels <- header[period_cols]
  blank_periods <- !nzchar(period_labels) & nzchar(next_header[period_cols])
  period_labels[blank_periods] <- next_header[period_cols[blank_periods]]
  period_cols <- period_cols[nzchar(period_labels)]
  period_labels <- period_labels[nzchar(period_labels)]

  data_start <- header_row + 1L
  if (sheet == "A2") {
    # Row 7 only describes ABN/EUM levels; the actual variables begin
    # on row 8.
    data_start <- data_start + 1L
  }

  rows <- list()
  k <- 0L
  for (row in seq.int(data_start, nrow(raw))) {
    variable_name <- raw[row, variable_col]
    if (!nzchar(variable_name)) next
    available_periods <- character(0)
    for (j in seq_along(period_cols)) {
      if (nzchar(raw[row, period_cols[[j]]])) {
        available_periods <- c(available_periods, period_labels[[j]])
      }
    }
    k <- k + 1L
    rows[[k]] <- data.frame(
      Appendix = sheet,
      Key.Name = key_name,
      Product.Name = product_name,
      Item = raw[row, 1L],
      Variable.Name = variable_name,
      Valid.Response = raw[row, valid_col],
      Available.Periods = paste(available_periods, collapse = ";"),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

fill_down <- function(x) {
  x <- clean_cell(x)
  current <- ""
  for (i in seq_along(x)) {
    if (nzchar(x[[i]])) current <- x[[i]]
    x[[i]] <- current
  }
  x
}

pad_numeric_code <- function(x, width) {
  x <- clean_cell(x)
  numeric <- grepl("^[0-9]+$", x)
  x[numeric] <- sprintf(paste0("%0", width, "d"), as.integer(x[numeric]))
  x
}

domain_rows <- function(domain, values, source_sheet,
                        variable_name = "") {
  values <- clean_cell(values)
  variable_name <- rep_len(clean_cell(variable_name), length(values))
  rows <- nzchar(values)
  unique(data.frame(
    Domain = rep(domain, sum(rows)),
    Variable.Name = variable_name[rows],
    Value = values[rows],
    Source.Sheet = rep(source_sheet, sum(rows)),
    stringsAsFactors = FALSE
  ))
}

variable_domain_rows <- function(path, sheet, header_row,
                                 variable_col, value_col) {
  raw <- read_sheet(path, sheet)
  rows <- seq.int(header_row + 1L, nrow(raw))
  variable_name <- fill_down(raw[rows, variable_col])
  domain_rows(
    domain = "variable",
    values = raw[rows, value_col],
    source_sheet = sheet,
    variable_name = variable_name
  )
}

parse_appendix_domains <- function(path) {
  a6 <- read_sheet(path, "A6")
  a7 <- read_sheet(path, "A7")
  a8 <- read_sheet(path, "A8")
  a9 <- read_sheet(path, "A9")
  a10 <- read_sheet(path, "A10")
  a12 <- read_sheet(path, "A12")
  a13 <- read_sheet(path, "A13")
  a14 <- read_sheet(path, "A14")
  a17 <- read_sheet(path, "A17")
  a18 <- read_sheet(path, "A18")
  a19 <- read_sheet(path, "A19")
  a21 <- read_sheet(path, "A21")

  domains <- list(
    variable_domain_rows(path, "A3", 6L, 2L, 3L),
    domain_rows("trade_country_code",
                pad_numeric_code(a6[9:nrow(a6), 5L], 3L), "A6"),
    domain_rows("trade_foreign_port",
                pad_numeric_code(a6[9:nrow(a6), 7L], 6L), "A6"),
    domain_rows("trade_australian_port",
                pad_numeric_code(a7[8:nrow(a7), 2L], 3L), "A7"),
    domain_rows("trade_currency", a8[7:nrow(a8), 3L], "A8"),
    domain_rows("trade_unit", a9[6:nrow(a9), 1L], "A9"),
    domain_rows("iplord_technology", a10[6:nrow(a10), 1L], "A10"),
    variable_domain_rows(path, "A11", 7L, 2L, 3L),
    domain_rows("iso_country_alpha2", a12[8:nrow(a12), 1L], "A12"),
    domain_rows("ip_right_sub_type", a13[6:nrow(a13), 1L], "A13"),
    domain_rows("ip_status", a14[6:nrow(a14), 1L], "A14"),
    domain_rows("ip_event_category", a17[6:nrow(a17), 1L], "A17"),
    domain_rows("ip_event_type", a18[6:nrow(a18), 1L], "A18"),
    domain_rows("ip_link_type", a19[6:nrow(a19), 1L], "A19"),
    domain_rows("max_market", a21[6:nrow(a21), 1L], "A21")
  )
  out <- unique(do.call(rbind, domains))
  out[order(out[["Source.Sheet"]], out[["Domain"]],
            out[["Variable.Name"]], out[["Value"]]), , drop = FALSE]
}

local_plausible_domains <- function() {
  # The April 2026 DIL points these seven trade fields to external CSM
  # appendices that are not bundled with the workbook. These small domains
  # preserve the documented widths and field meanings for usable synthetic
  # data. They are deliberately labelled as local and are not substitutes for
  # the official CSM codeframes.
  values <- list(
    commodity_code_ex = c(
      "02013000", "04022100", "10019900", "22042100", "26011100",
      "27090000", "71081200", "76011000", "84713000", "87032300"
    ),
    sitc_item_code_ex = c(
      "01111", "04120", "07110", "28150", "33300", "68410", "78120",
      "84140"
    ),
    bec_group_code_im = c(
      "111", "121", "210", "310", "410", "521", "610", "730"
    ),
    commodity_code_im = c(
      "0201300010", "0901110001", "3004900010", "3926900090",
      "6403999010", "8471300010", "8517130010", "8703230010"
    ),
    preference_code_im = c("GEN", "FTA", "DCS", "LDC", "NZ", "US"),
    sitc_item_code_im = c(
      "01111", "07110", "54170", "58390", "76410", "78120", "89390",
      "93100"
    ),
    treatment_code_im = c("000", "100", "200", "300", "400", "500")
  )
  do.call(rbind, lapply(names(values), function(variable_name) {
    domain_rows(
      domain = "local_plausible_trade_code",
      values = values[[variable_name]],
      source_sheet = "LOCAL_PLAUSIBLE_NOT_OFFICIAL",
      variable_name = variable_name
    )
  }))
}

workbook_sheets <- readxl::excel_sheets(input)
table_sheets <- workbook_sheets[grepl("^[0-9]+$", workbook_sheets)]
parsed <- lapply(table_sheets, parse_table_sheet, path = input)

tables <- do.call(rbind, lapply(parsed, `[[`, "table"))
variables <- do.call(rbind, lapply(parsed, `[[`, "variables"))
keys <- do.call(rbind, lapply(c("A1", "A2"), parse_key_sheet, path = input))
domains <- unique(rbind(parse_appendix_domains(input),
                        local_plausible_domains()))
domains <- domains[order(domains[["Source.Sheet"]], domains[["Domain"]],
                         domains[["Variable.Name"]], domains[["Value"]]),
                   , drop = FALSE]

dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(tables, file.path(metadata_dir, "tables.csv"),
                 row.names = FALSE)
utils::write.csv(variables, file.path(metadata_dir, "variables.csv"),
                 row.names = FALSE)
utils::write.csv(keys, file.path(metadata_dir, "keys.csv"),
                 row.names = FALSE)
utils::write.csv(domains, file.path(metadata_dir, "domains.csv"),
                 row.names = FALSE)

audit <- merge(
  tables,
  aggregate(
    Variable.Name ~ Table.Number,
    variables,
    function(x) length(unique(x))
  ),
  by = "Table.Number",
  all.x = TRUE
)
names(audit)[names(audit) == "Variable.Name"] <- "N.Unique.Variables"
utils::write.csv(audit, audit_path, row.names = FALSE)

message(
  "Updated BLADE metadata: ",
  nrow(tables), " tables, ",
  nrow(variables), " variable rows, ",
  length(unique(variables$Variable.Name)), " unique variable names, ",
  nrow(keys), " key variable rows, ",
  nrow(domains), " appendix domain rows."
)
