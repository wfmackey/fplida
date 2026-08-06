# The registry documents a value domain. The generator decides whether it can
# honour it. These pin the cases where those two come apart, each of which was
# a live defect caught by adversarial verification.

test_that("researched domains resolve every variable the first review could not", {
  info <- variable_info()
  resolved <- read.csv(
    system.file("internal-docs", "resolved-value-domains.csv",
                package = "fplida"),
    stringsAsFactors = FALSE
  )

  expect_gt(nrow(resolved), 270L)
  expect_setequal(
    unique(resolved$status), c("sourced", "guessed", "unsupported")
  )

  # Every resolution names a variable that exists.
  registry_key <- unique(paste(info$dataset, toupper(info$variable)))
  expect_true(all(
    paste(resolved$dataset, toupper(resolved$variable)) %in% registry_key
  ))

  # A sourced resolution must cite a fetchable source. This is the one claim
  # that would launder an inference into a fact.
  sourced <- resolved[resolved$status == "sourced", , drop = FALSE]
  expect_gt(nrow(sourced), 100L)
  expect_true(all(grepl("^https?://", sourced$value_source_url)))
  expect_true(all(nzchar(sourced$value_source)))

  # A value source names the publisher, never where this package keeps a copy.
  expect_false(any(grepl(
    "(^|[ ;(])(R/|src/|tests/|inst/|data-raw/)", resolved$value_source,
    perl = TRUE
  )))

  # `unsupported` stays clean: no values, no source. It is the honest outcome
  # and it should look like one.
  unsupported <- resolved[resolved$status == "unsupported", , drop = FALSE]
  expect_true(all(unsupported$n_values == 0L))
  expect_true(all(!nzchar(unsupported$value_source_url) |
                    is.na(unsupported$value_source_url)))
})

test_that("a partial list is never carried as a domain", {
  resolved <- read.csv(
    system.file("internal-docs", "resolved-value-domains.csv",
                package = "fplida"),
    stringsAsFactors = FALSE
  )

  # Eight New South Wales electorates out of several hundred nationally is not
  # the domain. Generating from that sample would put every child in NSW, so a
  # list is carried only when it is complete.
  sampled <- as.logical(resolved$values_are_sample) |
    (!is.na(resolved$full_list_size) &
       resolved$full_list_size > resolved$n_values)
  expect_true(any(sampled))
  expect_true(all(resolved$n_values[sampled] == 0L))
  expect_true(all(!as.logical(resolved$enumerated[sampled])))

  # The domain and its size survive even when the list does not, so a reader
  # can tell "documented, too big to carry" from "nobody knows".
  carried <- resolved[sampled & resolved$status != "unsupported", ]
  expect_true(all(nzchar(carried$value_domain)))
})

test_that("documented area codes stay inside the person's own state", {
  skip_if_not_installed("jsonlite")

  # ASGS codes carry their state in the leading digit. A flat draw from a
  # national list hands a Queenslander a Victorian mesh block, which
  # contradicts the guarantee that a person's geography agrees across
  # products.
  spine <- data.frame(
    person_id = 1:8,
    state = c(1L, 1L, 2L, 3L, 4L, 5L, 6L, 8L)
  )

  # Only the area fields whose full list is carried can be anchored. The
  # others were sampled, and a sample is not a domain.
  for (pair in list(
    c("ACLD", "RA_UR_11"), c("ACLD", "RA_UR_16"), c("AEDC", "GCCSACODE")
  )) {
    drawn <- fplida:::.registry_area_column(
      pair[[1]], pair[[2]], spine, seed = 3L
    )
    expect_false(is.null(drawn), info = paste(pair, collapse = " "))
    present <- !is.na(drawn)
    expect_equal(
      substr(as.character(drawn[present]), 1L, 1L),
      as.character(spine$state[present]),
      info = paste(pair, collapse = " ")
    )
  }
})

test_that("a name column holds the name, not the code", {
  skip_if_not_installed("jsonlite")

  # `.registry_value_lookup()` strips everything after the first colon, so a
  # `..._NAME` column given the same `code: label` list as its `..._CODE`
  # sibling would emit the code into both.
  spine <- data.frame(person_id = 1:6, state = c(1L, 1L, 2L, 3L, 4L, 5L))

  drawn <- fplida:::.registry_area_column("AEDC", "GCCSANAME", spine, seed = 5L)
  expect_false(is.null(drawn))
  present <- drawn[!is.na(drawn)]
  expect_gt(length(present), 0L)
  # A name is not a bare code: it must not be a run of digits.
  expect_false(any(grepl("^[0-9]+$", present)))

  # The code sibling holds codes drawn from the same list, so the two must
  # differ in shape.
  codes <- fplida:::.registry_area_column("AEDC", "GCCSACODE", spine, seed = 5L)
  expect_false(identical(drawn, codes))
})

test_that("an unanchorable area list writes typed missing rather than guessing", {
  skip_if_not_installed("jsonlite")
  spine <- data.frame(person_id = 1:6, state = c(1L, 2L, 3L, 4L, 5L, 6L))

  # The researched STATEELECTORATE list covers New South Wales only. Anchoring
  # is impossible, so the helper refuses rather than putting everyone in NSW.
  expect_null(
    fplida:::.registry_area_column("AEDC", "STATEELECTORATE", spine, seed = 2L)
  )
})

test_that("AEDC fields defer to the registry but still refuse to invent", {
  documented <- vapply(
    fplida:::.dil_aedc_blocked_names,
    function(name) !is.null(fplida:::.registry_values_for("AEDC", name)),
    logical(1)
  )
  # The blocklist was exactly the set the first review could not resolve.
  # Research resolved a large part of it; the rest must stay typed missing.
  expect_gt(sum(documented), 30L)
  expect_gt(sum(!documented), 0L)
})

codes_of <- function(raw) {
  parsed <- tryCatch(
    as.character(unlist(jsonlite::fromJSON(raw), use.names = FALSE)),
    error = function(e) character(0)
  )
  trimws(sub("^\\s*([^:]+?)\\s*:.*$", "\\1", parsed))
}

test_that("every documented code appears exactly once", {
  skip_if_not_installed("jsonlite")
  info <- variable_info()
  has <- nzchar(info$valid_values) & trimws(info$valid_values) != "[]"
  pairs <- info[has, c("dataset", "variable", "valid_values")]
  pairs <- pairs[!duplicated(paste(pairs$dataset, toupper(pairs$variable))), ]
  expect_gt(nrow(pairs), 3000L)

  # Generation strips the label and samples uniformly, so a code listed twice
  # is drawn twice as often. Two ways in: "N: No" beside "N: Default", and a
  # multi-vintage codeframe carrying one row per code per year with the name
  # drifting between them.
  duplicated_codes <- vapply(
    pairs$valid_values,
    function(raw) anyDuplicated(codes_of(raw)) > 0L,
    logical(1)
  )
  offenders <- paste(
    pairs$dataset[duplicated_codes], pairs$variable[duplicated_codes]
  )
  expect_identical(offenders, character(0))
})

test_that("a researched value domain slug names one code set", {
  skip_if_not_installed("jsonlite")
  resolved <- read.csv(
    system.file("internal-docs", "resolved-value-domains.csv",
                package = "fplida"),
    stringsAsFactors = FALSE
  )
  carried <- resolved[resolved$n_values > 0L & nzchar(resolved$value_domain), ]

  # A reader treats value_domain as an identifier, so one slug must not name
  # two incompatible code sets — `yes_no_flag` meaning both 0/1 and Y/N.
  #
  # Labels may differ freely. Four superannuation indicators all take
  # true/false while describing different things, and spelling out what each
  # one means is more useful than forcing one shared wording.
  by_domain <- split(carried$values, carried$value_domain)
  collisions <- names(by_domain)[vapply(by_domain, function(v) {
    length(unique(lapply(v, function(raw) sort(codes_of(raw))))) > 1L
  }, logical(1))]

  expect_identical(collisions, character(0))
})

test_that("the researched codes reach the Rust generators", {
  # The registry documents a domain; the Rust generators emit the data and
  # cannot read the registry. Without a bridge each keeps its own hardcoded
  # array, and those drift: DOMINO emitted FTN for a fortnightly frequency the
  # custodian codes as 2WE, and a 1-3 impairment code for what is published as
  # a 0-95 rating.
  path <- system.file(
    "extdata", "codeframes", "researched-value-codes.tsv", package = "fplida"
  )
  expect_true(nzchar(path))
  codes <- utils::read.delim(path, stringsAsFactors = FALSE, quote = "")
  expect_identical(names(codes), c("dataset", "variable", "code", "label"))
  expect_gt(nrow(codes), 500L)

  # The table is embedded by `include_str!` and split on tabs, so a tab or a
  # newline inside a field would silently shift every later column.
  expect_false(any(grepl("[\t\r\n]", codes$label)))
  expect_true(all(nzchar(codes$code)))
  expect_true(all(codes$variable == toupper(codes$variable)))
  expect_equal(anyDuplicated(paste(codes$dataset, codes$variable, codes$code)), 0L)

  # It must agree with the registry it was built from.
  info <- variable_info()
  registry <- unique(paste(info$dataset, toupper(info$variable)))
  expect_true(all(paste(codes$dataset, codes$variable) %in% registry))
})

test_that("generated columns stay inside their documented domain", {
  skip_if_not_installed("arrow")
  skip_on_cran()

  # Every defect this test guards against was invisible because nothing
  # compared what the registry claims against what the generator produces.
  # The two were maintained in different places and drifted apart.
  resolved <- read.csv(
    system.file("internal-docs", "resolved-value-domains.csv",
                package = "fplida"),
    stringsAsFactors = FALSE
  )
  resolved <- resolved[resolved$n_values > 0L, , drop = FALSE]

  out <- file.path(tempdir(), "value_research_domain_check")
  if (dir.exists(out)) unlink(out, recursive = TRUE)
  dir.create(out, recursive = TRUE)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  spine <- generate_spine(n = 300, seed = 41L, output_dir = out)
  generate_domino(spine = spine, seed = 41L, output_dir = out)
  generate_sae(spine = spine, seed = 41L, output_dir = out)

  files <- list.files(out, pattern = "parquet$", recursive = TRUE,
                      full.names = TRUE)
  expect_gt(length(files), 0L)

  offenders <- character(0)
  for (f in files) {
    frame <- tryCatch(as.data.frame(arrow::read_parquet(f)),
                      error = function(e) NULL)
    if (is.null(frame) || !nrow(frame)) next
    for (nm in names(frame)) {
      hit <- toupper(resolved$variable) == toupper(nm)
      if (!any(hit)) next
      documented <- codes_of(resolved$values[hit][1])
      got <- unique(as.character(frame[[nm]]))
      got <- got[!is.na(got)]
      outside <- setdiff(got, documented)
      if (length(outside)) {
        offenders <- c(offenders, paste0(
          nm, " emits ", paste(utils::head(outside, 3), collapse = ",")
        ))
      }
    }
  }
  expect_identical(unique(offenders), character(0))
})
