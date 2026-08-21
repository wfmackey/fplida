# Rebuild the generated schema register.
#
# The register is the headline coverage metric: one row per generated column,
# recording what the current generators actually emit -- type, missingness,
# distinct count, observed domain -- joined to the PLIDA and BLADE metadata
# for the same variable. It is implementation evidence, not calibration
# evidence: the official codebooks still say what a variable means.
#
# It existed as a committed CSV with nothing to rebuild it, so no coverage
# claim could be checked and no regression in coverage could be seen. This
# script builds a small fixed-seed dataset, reads every parquet it writes,
# and regenerates the register and its per-guide splits, printing the change
# in coverage against the committed version.
#
# Run from the repository root:
#   Rscript data-raw/build_generated_schema_register.R
#
# Options:
#   FPLIDA_REGISTER_N     people in the sample build (default 500)
#   FPLIDA_REGISTER_SEED  seed (default 20260101)

suppressPackageStartupMessages({
  library(fplida)
  library(fplida.info)
  library(arrow)
})

n_people <- as.integer(Sys.getenv("FPLIDA_REGISTER_N", "500"))
seed <- as.integer(Sys.getenv("FPLIDA_REGISTER_SEED", "20260101"))
register_path <- file.path("fplida.info", "inst", "internal-docs",
                           "generated-schema-register.csv")
split_dir <- file.path("fplida.info", "inst", "internal-docs",
                       "schema-registers")

# Which internal guide a dataset belongs to. A dataset with no entry is
# reported under its own name so a new product cannot vanish from the count.
GUIDE_OF_DATASET <- c(
  BLADE = "blade", CENSUS = "census", CORE = "core-combined",
  COMBINED = "core-combined", MBS = "dhda-health", PBS = "dhda-health",
  AIR = "dhda-health", NACDC = "dhda-health", MCD = "dhda-health",
  DOMINO = "dss", DEX = "dss", NDIS = "ndis", HE = "education",
  AEDC = "education", ACLD = "core-combined", TVA = "vet-apprentice",
  APPRENTICE = "vet-apprentice", AMEP = "home-affairs",
  VISA = "home-affairs", SDB = "home-affairs", TRAVELLERS = "home-affairs",
  MT_DEMOGS = "home-affairs", PIT_PS = "pit", PIT_ITR = "pit",
  PIT_IE = "pit", STP = "stp", SAE = "pit", CGT = "pit", RPS = "pit",
  BUSOWN = "pit", ERS = "pit", JK = "dss", JM = "dss", ATO_CR = "pit",
  ATO_MCS = "pit", BIRTHS = "vital-events", DEATHS = "vital-events",
  SDAC = "core-combined", APSED = "dss", NHS = "core-combined",
  NSMHW = "core-combined", PEX = "core-combined", SMSF = "pit"
)

# Directory name to dataset, the inverse of dataset_dir()'s agency-dataset
# folder convention.
dataset_of_directory <- function(directory) {
  toupper(sub("^[^-]+-", "", directory))
}

describe_column <- function(x) {
  n <- length(x)
  missing <- sum(is.na(x))
  present <- x[!is.na(x)]
  distinct <- length(unique(present))

  domain <- if (!length(present)) {
    ""
  } else if (is.numeric(present)) {
    q <- stats::quantile(present, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
    paste(format(q, trim = TRUE, digits = 6), collapse = " / ")
  } else if (inherits(present, "Date")) {
    paste(format(range(present)), collapse = " to ")
  } else {
    counts <- sort(table(as.character(present)), decreasing = TRUE)
    keep <- seq_len(min(8L, length(counts)))
    paste(sprintf("%s=%d", names(counts)[keep], as.integer(counts)[keep]),
          collapse = "; ")
  }

  values <- if (distinct > 0L && distinct <= 25L) {
    paste(sort(unique(as.character(present))), collapse = "; ")
  } else {
    ""
  }

  list(type = class(x)[[1L]], n_rows = n, missing = missing,
       missing_pct = if (n) round(100 * missing / n, 4) else 0,
       distinct = distinct, domain_or_distribution = domain,
       values_if_small_domain = values)
}

# Reuse a build when one is offered, so the register can be reassembled
# without paying for the generation again.
run_dir <- Sys.getenv("FPLIDA_REGISTER_RUN_DIR", "")
if (nzchar(run_dir)) {
  message("Reusing the build at ", run_dir)
} else {
  message("Building a ", format(n_people, big.mark = ","),
          "-person sample at seed ", seed, " ...")
  out <- file.path(tempdir(), "fplida_schema_register")
  unlink(out, recursive = TRUE)
  # The DIL schema companions are 100-row stubs whose columns come from the
  # registry by construction, so including them would report a coverage of
  # almost 100% that says nothing about the generators. The register records
  # what the bespoke generators emit.
  result <- build_fplida(n = n_people, seed = seed, output_dir = out,
                         products = "all", k_slices = 2L,
                         complete_dil_schema = FALSE)
  run_dir <- result$canonical_run_dir
}

tables <- list.dirs(run_dir, recursive = TRUE)
tables <- tables[vapply(tables, function(x)
  length(list.files(x, pattern = "\\.parquet$")) > 0, logical(1))]
tables <- tables[!grepl("/_system", tables)]
message("Reading ", length(tables), " generated tables ...")

rows <- list()
for (path in tables) {
  relative <- sub(paste0("^", run_dir, "/"), "", path)
  parts <- strsplit(relative, "/", fixed = TRUE)[[1L]]
  dataset <- dataset_of_directory(parts[[1L]])
  table_name <- parts[[length(parts)]]

  frame <- tryCatch(
    as.data.frame(arrow::open_dataset(
      list.files(path, pattern = "\\.parquet$", full.names = TRUE),
      unify_schemas = TRUE)),
    error = function(e) NULL
  )
  if (is.null(frame) || !ncol(frame)) next

  for (column in names(frame)) {
    described <- describe_column(frame[[column]])
    rows[[length(rows) + 1L]] <- data.frame(
      guide = unname(GUIDE_OF_DATASET[dataset] %||% tolower(dataset)),
      dataset = dataset,
      table = table_name,
      variable = column,
      type = described$type,
      n_rows = described$n_rows,
      missing = described$missing,
      missing_pct = described$missing_pct,
      distinct = described$distinct,
      domain_or_distribution = described$domain_or_distribution,
      values_if_small_domain = described$values_if_small_domain,
      stringsAsFactors = FALSE
    )
  }
}
register <- do.call(rbind, rows)
message("Described ", nrow(register), " generated columns.")

# Join the registry: a column that matches its dataset and variable name has
# metadata behind it, and one that does not is what the coverage number is
# counting.
info <- as.data.frame(variable_info())
info$key <- paste(toupper(info$dataset), toupper(info$variable))
register$key <- paste(toupper(register$dataset), toupper(register$variable))

collapse_unique <- function(x) {
  x <- unique(x[nzchar(x) & !is.na(x)])
  if (!length(x)) return("")
  paste(x, collapse = " | ")
}
by_key <- split(info, info$key)
lookup <- function(key, column) {
  hit <- by_key[[key]]
  if (is.null(hit)) "" else collapse_unique(as.character(hit[[column]]))
}

# An excerpt, not the whole description. The description lives in the
# registry, one join away, and carrying all of it here would repeat a
# 330-character field once per generated column and put the register past
# 20 MB.
excerpt <- function(x, width = 140L) {
  ifelse(nchar(x) > width, paste0(substr(x, 1L, width - 1L), "\u2026"), x)
}
register$plida_variable_descriptions <- excerpt(vapply(
  register$key, lookup, character(1), column = "variable_description"))
# A variable can appear in dozens of products, and listing every one repeats
# up to 4,000 characters per row. The count plus the first few is what a
# reviewer needs; variable_info() has the rest.
product_summary <- function(key) {
  hit <- by_key[[key]]
  if (is.null(hit)) return("")
  products <- sort(unique(as.character(hit$product)))
  products <- products[nzchar(products)]
  if (!length(products)) return("")
  if (length(products) <= 3L) return(paste(products, collapse = " | "))
  sprintf("%s | ... (%d products)", paste(products[1:3], collapse = " | "),
          length(products))
}
register$plida_products <- vapply(register$key, product_summary, character(1))
register$metadata_match <- ifelse(
  nzchar(register$plida_variable_descriptions) |
    register$key %in% names(by_key),
  "matched PLIDA variable metadata",
  "no PLIDA metadata match by dataset+variable")

blade <- info[info$asset == "BLADE", , drop = FALSE]
blade_by_variable <- split(blade, toupper(blade$variable))
blade_lookup <- function(variable, column) {
  hit <- blade_by_variable[[toupper(variable)]]
  if (is.null(hit)) "" else collapse_unique(as.character(hit[[column]]))
}
register$blade_item <- excerpt(vapply(register$variable, blade_lookup,
                                      character(1),
                                      column = "official_description"))
register$blade_valid_response <- excerpt(vapply(
  register$variable, blade_lookup, character(1),
  column = "official_valid_response"))
register$blade_available_periods <- excerpt(vapply(
  register$variable, blade_lookup, character(1),
  column = "available_periods"), width = 60L)
register$key <- NULL

column_order <- c("guide", "dataset", "table", "variable", "type", "n_rows",
                  "missing", "missing_pct", "distinct",
                  "domain_or_distribution", "values_if_small_domain",
                  "metadata_match", "plida_variable_descriptions",
                  "plida_products", "blade_item", "blade_valid_response",
                  "blade_available_periods")
register <- register[order(register$guide, register$dataset, register$table,
                           register$variable), column_order]
rownames(register) <- NULL

# Coverage must not fall. Report the change rather than asserting it, because
# a deliberate product removal is a legitimate reason for it to.
matched <- function(x) mean(x$metadata_match == "matched PLIDA variable metadata")
if (file.exists(register_path)) {
  previous <- utils::read.csv(register_path, stringsAsFactors = FALSE)
  message(sprintf(
    "Columns: %d -> %d (%+d). Matched to metadata: %.1f%% -> %.1f%% (%+.1f pp).",
    nrow(previous), nrow(register), nrow(register) - nrow(previous),
    100 * matched(previous), 100 * matched(register),
    100 * (matched(register) - matched(previous))))
  lost <- setdiff(paste(previous$dataset, previous$table, previous$variable),
                  paste(register$dataset, register$table, register$variable))
  if (length(lost)) {
    message("Columns present before and absent now: ", length(lost))
    message("  ", paste(utils::head(lost, 10), collapse = "\n  "))
  }
} else {
  message(sprintf("Columns: %d. Matched to metadata: %.1f%%.",
                  nrow(register), 100 * matched(register)))
}

utils::write.csv(register, register_path, row.names = FALSE)
message("Wrote ", register_path)

if (!dir.exists(split_dir)) dir.create(split_dir, recursive = TRUE)
for (guide in sort(unique(register$guide))) {
  utils::write.csv(register[register$guide == guide, , drop = FALSE],
                   file.path(split_dir, sprintf("schema-register-%s.csv", guide)),
                   row.names = FALSE)
}
message("Wrote ", length(unique(register$guide)), " per-guide splits to ",
        split_dir)
