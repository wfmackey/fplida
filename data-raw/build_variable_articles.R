# Generate the pkgdown articles that document every PLIDA and BLADE variable.
#
# Writes to vignettes/articles/, which pkgdown builds into the website but
# R CMD build does not ship in the package tarball. Run this whenever
# inst/variable-info.csv.gz or inst/dataset-info.csv changes, then rebuild the
# site with pkgdown::build_site().
#
# Usage:
#   Rscript data-raw/build_variable_articles.R

suppressMessages({
  library(fplida)
})

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. install.packages('jsonlite')",
       call. = FALSE)
}

out_dir <- file.path("vignettes", "articles")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

vi <- variable_info()
di <- dataset_info()

message("Registry: ", nrow(vi), " occurrences across ", length(unique(vi$dataset)),
        " datasets")

# ---- helpers ---------------------------------------------------------------

blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

nz <- function(x, fallback = NULL) {
  if (length(x) != 1L || blank(x)) return(fallback)
  trimws(as.character(x))
}

# The site is generated, so any text from the registry is escaped before it
# reaches the page.
esc <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

# A YAML scalar that survives colons, quotes and stray dashes in dataset names.
yaml_string <- function(x) {
  paste0('"', gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", x)), '"')
}

comma <- function(n) formatC(n, format = "d", big.mark = ",")

slug <- function(x) tolower(gsub("[^A-Za-z0-9]+", "-", x))

# "1: Yes" -> c("1", "Yes"). Values without a separator become a bare label.
split_value <- function(s) {
  m <- regexpr(": ", s, fixed = TRUE)
  if (m == -1L) return(c(s, ""))
  c(substr(s, 1L, m - 1L), substr(s, m + 2L, nchar(s)))
}

parse_domain <- function(json) {
  if (blank(json) || identical(trimws(json), "[]")) return(list())
  parsed <- tryCatch(jsonlite::fromJSON(json), error = function(e) NULL)
  if (is.null(parsed) || !length(parsed)) return(list())
  lapply(as.character(parsed), split_value)
}

# ---- shared value-domain dictionary ---------------------------------------
# Only 677 distinct value lists back 72,656 occurrences, so store each once and
# have every variable reference it by index.

domain_keys <- unique(vi$valid_values)
domain_keys <- domain_keys[!blank(domain_keys) & trimws(domain_keys) != "[]"]
domain_list <- lapply(domain_keys, parse_domain)
names(domain_list) <- domain_keys
domain_index <- stats::setNames(seq_along(domain_keys) - 1L, domain_keys)

message("Distinct value domains: ", length(domain_keys))

# ---- one payload row per unique dataset + variable -------------------------

vi$.key <- paste(vi$dataset, vi$variable, sep = "\r")
first_of <- !duplicated(vi$.key)

# Where each variable appears, capped so a variable in 200 tables does not
# dominate the page.
where_all <- split(paste0(vi$product, " / ", vi$table), vi$.key)

# Reference periods are per-occurrence, so a variable appearing in several
# products has several. Show the union, not whichever row happened to sort
# first. Distinct values are comma-joined in the order they occur.
period_all <- vapply(
  split(vi$reference_period, vi$.key),
  function(x) {
    x <- unique(x[nzchar(x) & !is.na(x)])
    if (!length(x)) return("")
    # Chronological, not occurrence order, so a variable present in 2011, 2016
    # and 2021 does not read "2016, 2011, 2021".
    lead <- suppressWarnings(as.integer(sub("^.*?((19|20)[0-9]{2}).*$", "\\1", x)))
    lead[is.na(lead)] <- .Machine$integer.max
    x <- x[order(lead, x)]
    lead <- sort(lead)
    # A long consecutive run reads better as a span: a variable in all 23 CGT
    # annual products should say "2000-2001 to 2022-2023", the way the curated
    # dataset period does, not list every cycle.
    simple <- all(grepl("^(19|20)[0-9]{2}(-(19|20)[0-9]{2})?$", x))
    if (length(x) > 3L && simple &&
        identical(lead, seq(lead[1], lead[length(lead)]))) {
      return(paste(x[1], "to", x[length(x)]))
    }
    paste(x, collapse = ", ")
  },
  character(1)
)

build_payload <- function(rows) {
  used <- character(0)
  vars <- lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    dom <- NULL
    vv <- r$valid_values
    if (!blank(vv) && trimws(vv) != "[]" && !is.na(domain_index[[vv]])) {
      dom <- unname(domain_index[[vv]])
      used <<- c(used, vv)
    }
    w <- unique(where_all[[r$.key]])
    n_w <- length(w)
    if (n_w > 12L) {
      w <- c(w[1:12], paste0("and ", comma(n_w - 12L), " more"))
    }
    # as.list keeps this a JSON array. auto_unbox would turn a single entry
    # into a bare string, and the page expects an array.
    w <- as.list(w)
    rec <- list(
      n = nz(r$variable, ""),
      d = nz(r$variable_description, nz(r$official_description, "")),
      od = nz(r$official_description),
      t = nz(r$variable_type),
      k = nz(r$value_domain),
      def = nz(r$value_definition),
      p = nz(unname(period_all[r$.key]), r$reference_period),
      s = nz(r$value_support_status, "not_applicable"),
      src = nz(r$value_source),
      url = nz(r$value_source_url),
      lim = nz(r$limitation),
      where = w
    )
    if (!is.null(dom)) rec$dom <- dom
    # Drop empty fields; across thousands of rows this is a large saving.
    rec[!vapply(rec, is.null, logical(1))]
  })
  # Only the domains this page actually uses, keeping the index stable.
  used <- unique(used)
  local_idx <- stats::setNames(seq_along(used) - 1L, used)
  doms <- unname(lapply(used, function(k) {
    lapply(domain_list[[k]], function(p) unname(p))
  }))
  vars <- lapply(vars, function(v) {
    if (!is.null(v$dom)) {
      key <- domain_keys[v$dom + 1L]
      v$dom <- unname(local_idx[[key]])
    }
    v
  })
  list(domains = doms, vars = vars)
}

payload_script <- function(payload) {
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null",
                           na = "null", digits = NA)
  # </script> cannot appear inside an inline script element.
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  paste0('<script type="application/json">', json, "</script>")
}

# ---- per-dataset articles --------------------------------------------------

datasets <- di[order(di$asset != "PLIDA", di$dataset), ]
index_rows <- character(0)
index_agency <- character(0)
index_agency_code <- character(0)

for (i in seq_len(nrow(datasets))) {
  d <- datasets[i, ]
  code <- d$dataset
  rows_all <- vi[vi$dataset == code, , drop = FALSE]
  if (!nrow(rows_all)) {
    message("  skipping ", code, " (no registry rows)")
    next
  }
  rows <- rows_all[first_of[vi$dataset == code], , drop = FALSE]
  rows <- rows[order(rows$variable), , drop = FALSE]

  n_products <- length(unique(rows_all$product))
  n_tables <- nrow(unique(rows_all[c("product", "table")]))
  file <- file.path(out_dir, paste0("dataset-", slug(code), ".Rmd"))

  title <- paste0(code, ": ", nz(d$dataset_name, code))

  summary_items <- list(
    c("Asset", esc(d$asset)),
    c("Collection", esc(d$collection_type)),
    c("Supplier", esc(nz(d$supplier, "Not recorded"))),
    c("Custodian", esc(nz(d$custodian, "Not recorded"))),
    c("Reference period", esc(nz(d$reference_period, "Not recorded"))),
    c("Update frequency", esc(nz(d$update_frequency, "Not recorded"))),
    c("Products", comma(n_products)),
    c("Tables", comma(n_tables)),
    c("Unique variables", comma(nrow(rows))),
    c("Variable occurrences", comma(nrow(rows_all))),
    c("Metadata source", paste0(esc(d$metadata_source), ", ",
                                esc(d$metadata_vintage)))
  )
  if (!blank(d$information_url)) {
    summary_items <- c(summary_items, list(c(
      "Official information",
      paste0('<a href="', esc(d$information_url), '" rel="nofollow noopener">',
             esc(nz(d$information_source, d$information_url)), "</a>")
    )))
  }
  summary_html <- paste0(
    '<dl class="fp-summary">',
    paste0(vapply(summary_items, function(kv) {
      paste0("<dt>", kv[1], "</dt><dd>", kv[2], "</dd>")
    }, character(1)), collapse = ""),
    "</dl>"
  )

  # Products and their tables.
  pt <- unique(rows_all[c("product", "table")])
  pt_counts <- as.data.frame(table(pt$product), stringsAsFactors = FALSE)
  names(pt_counts) <- c("product", "tables")
  var_counts <- tapply(rows_all$variable, rows_all$product,
                       function(x) length(unique(x)))
  pt_counts$variables <- as.integer(var_counts[pt_counts$product])
  pt_counts <- pt_counts[order(pt_counts$product), , drop = FALSE]

  product_html <- paste0(
    '<table class="fp-index"><thead><tr><th>Product</th>',
    '<th class="fp-num">Tables</th><th class="fp-num">Variables</th>',
    "</tr></thead><tbody>",
    paste0(vapply(seq_len(nrow(pt_counts)), function(j) {
      paste0("<tr><td><code>", esc(pt_counts$product[j]), "</code></td>",
             '<td class="fp-num">', comma(pt_counts$tables[j]), "</td>",
             '<td class="fp-num">', comma(pt_counts$variables[j]), "</td></tr>")
    }, character(1)), collapse = ""),
    "</tbody></table>"
  )

  payload <- build_payload(rows)

  desc <- nz(d$dataset_description, "")
  info <- nz(d$information_summary, "")

  lines <- c(
    "---",
    paste0("title: ", yaml_string(title)),
    paste0("description: ", yaml_string(
      substr(paste0(desc, ""), 1L, 300L))),
    "---",
    "",
    if (nzchar(desc)) c(desc, "") else NULL,
    if (nzchar(info)) c(paste0("*", info, "*"), "") else NULL,
    "## Dataset summary",
    "",
    summary_html,
    "",
    "## Products and tables",
    "",
    paste0("`", code, "` has ", comma(n_products), " product",
           if (n_products == 1L) "" else "s", " covering ", comma(n_tables),
           " table", if (n_tables == 1L) "" else "s", "."),
    "",
    product_html,
    "",
    "## Variables",
    "",
    paste0("Select a row for the full detail: the official description, the ",
           "value definition, every valid value, the source, and the tables ",
           "the variable appears in."),
    "",
    '<div class="fplida-vartable">',
    payload_script(payload),
    "</div>",
    ""
  )

  writeLines(lines, file, useBytes = TRUE)

  index_rows <- c(index_rows, paste0(
    "<tr><td><a href=\"dataset-", slug(code), ".html\"><code>", esc(code),
    "</code></a></td><td>", esc(nz(d$dataset_name, "")), "</td>",
    "<td>", esc(nz(d$supplier, "")), "</td>",
    "<td>", esc(d$collection_type), "</td>",
    '<td class="fp-num">', comma(n_products), "</td>",
    '<td class="fp-num">', comma(n_tables), "</td>",
    '<td class="fp-num">', comma(nrow(rows)), "</td></tr>"
  ))
  index_agency <- c(index_agency, nz(d$supplier_name, "Unknown agency"))
  index_agency_code <- c(index_agency_code, nz(d$supplier, ""))

  message("  wrote ", file, " (", comma(nrow(rows)), " variables)")
}

# ---- dataset index ---------------------------------------------------------

# Group the index by supplying agency. Agencies are ordered by how many
# datasets they supply, so the substantial ones lead, with ties alphabetical.
# "Multiple agencies" sorts last whatever its count, since it is a residual
# rather than an agency.
local({
  agency_n <- table(index_agency)
  multiple <- names(agency_n) == "Multiple agencies"
  ord <- order(multiple, -as.integer(agency_n), names(agency_n))
  out <- character(0)
  for (ag in names(agency_n)[ord]) {
    hit <- index_agency == ag
    code <- unique(index_agency_code[hit])
    label <- if (length(code) == 1L && nzchar(code) && code != "Multiple") {
      paste0(esc(ag), " <code>", esc(code), "</code>")
    } else {
      esc(ag)
    }
    out <- c(
      out,
      paste0('<tr class="fp-group"><th colspan="7">', label,
             ' <span class="fp-group-n">', comma(sum(hit)),
             if (sum(hit) == 1L) " dataset" else " datasets", "</span></th></tr>"),
      index_rows[hit]
    )
  }
  grouped_index_rows <<- out
})

n_ds <- nrow(datasets)
n_occ <- nrow(vi)
n_uniq <- sum(first_of)

index <- c(
  "---",
  'title: "Datasets"',
  'description: "Every PLIDA dataset and the BLADE module, with the products, tables and variables in each."',
  "---",
  "",
  paste0("`fplida` carries the published structure of ", comma(n_ds),
         " datasets: ", comma(nrow(datasets) - 1L),
         " PLIDA datasets and the BLADE module. Together they hold ",
         comma(n_occ), " variable occurrences, which reduce to ",
         comma(n_uniq), " distinct dataset-and-variable pairs."),
  "",
  paste0("The same variable often appears in many tables of the same dataset. ",
         "Each dataset page lists the distinct variables once and records ",
         "where each one appears."),
  "",
  "## All datasets",
  "",
  paste0("Grouped by the agency that supplies the data. ",
         "COMBINED and CORE are derived from several agencies."),
  "",
  '<table class="fp-index"><thead><tr><th>Code</th><th>Name</th><th>Agency</th>',
  '<th>Collection</th>',
  '<th class="fp-num">Products</th><th class="fp-num">Tables</th>',
  '<th class="fp-num">Variables</th></tr></thead><tbody>',
  grouped_index_rows,
  "</tbody></table>",
  "",
  "## Reading the value support status",
  "",
  "Each variable carries one of three statuses.",
  "",
  paste0("- `supported` — the registry records a value domain for the ",
         "variable, drawn from a published classification or code list."),
  paste0("- `unsupported` — the source does not publish a finite value list, ",
         "so the synthetic column is written as typed missing rather than ",
         "given invented codes."),
  paste0("- `not_applicable` — the variable belongs to a survey, which is ",
         "outside the value-assessment scope. The structure is present; the ",
         "values are not assessed."),
  "",
  paste0("A `supported` status describes the registry, not the fidelity of ",
         "the generated column. It does not guarantee that a canonical ",
         "companion file populates that column."),
  ""
)

writeLines(index, file.path(out_dir, "datasets.Rmd"), useBytes = TRUE)
message("  wrote ", file.path(out_dir, "datasets.Rmd"))
message("Done. Build the site with pkgdown::build_site().")
