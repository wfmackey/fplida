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

test_that("every researched code appears exactly once", {
  skip_if_not_installed("jsonlite")
  resolved <- read.csv(
    system.file("internal-docs", "resolved-value-domains.csv",
                package = "fplida"),
    stringsAsFactors = FALSE
  )
  carried <- resolved[resolved$n_values > 0L, , drop = FALSE]
  expect_gt(nrow(carried), 100L)

  # "N: No" beside "N: Default" makes N twice as likely as Y once the labels
  # are stripped for generation.
  duplicated_codes <- vapply(
    carried$values, function(raw) anyDuplicated(codes_of(raw)) > 0L, logical(1)
  )
  expect_false(any(duplicated_codes))
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
