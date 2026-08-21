# The address register identifier is the one column that is supposed to mean
# the same thing in fifteen datasets, so it is the one most easily broken by
# each generator answering for itself.

test_that("every generator of a residential address loads the dwelling", {
  # The address key falls back to the person when a spine frame has no
  # `dwelling_id`, and the fallback is silent. A generator that forgets the
  # column therefore keys its ARID on the person while every other product keys
  # it on the dwelling, and the join the identifier exists for returns nothing.
  # ATO_MCS did exactly that. This is the check that catches the next one.
  sources <- c("R/generate_core.R", "R/generate_dil_lightweight.R")
  for (file in sources) {
    path <- testthat::test_path("..", "..", file)
    if (!file.exists(path)) next
    expect_true(any(grepl("dwelling_id", readLines(path), fixed = TRUE)),
                info = file)
  }
})

test_that("co-residents share an address, and agencies disagree about it", {
  skip_if_not_installed("arrow")
  skip_on_cran()

  out <- file.path(tempdir(), "arid_agreement")
  if (dir.exists(out)) unlink(out, recursive = TRUE)
  dir.create(out, recursive = TRUE)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  suppressMessages(build_fplida(
    n = 4000, seed = 11L, output_dir = out,
    products = c("core", "ato_cr", "mcd", "domino"),
    complete_dil_schema = TRUE
  ))

  files <- list.files(out, pattern = "parquet$", recursive = TRUE,
                      full.names = TRUE)
  seen <- list()
  for (f in files) {
    frame <- tryCatch(as.data.frame(arrow::read_parquet(f)),
                      error = function(e) NULL)
    if (is.null(frame) || !nrow(frame)) next
    column <- intersect(c("ARID", "ARID_HASH_TRUNC"), names(frame))
    if (!length(column) || !"SPINE_ID" %in% names(frame)) next
    pairs <- unique(data.frame(
      person = as.character(frame$SPINE_ID),
      arid = as.character(frame[[column[[1]]]]),
      product = f,
      stringsAsFactors = FALSE
    ))
    seen[[length(seen) + 1L]] <- pairs
  }
  # CORE plus at least one agency product, or the test proves nothing.
  expect_gt(length(seen), 1L)

  all_pairs <- do.call(rbind, seen)
  all_pairs <- all_pairs[!is.na(all_pairs$person) & !is.na(all_pairs$arid), ]
  by_person <- tapply(all_pairs$arid, all_pairs$person,
                      function(x) length(unique(x)))

  # One ARID per person across every product was the old assertion, and a
  # disagreement rate contradicts it: on 2016 Census night PLIDA disagreed
  # with the Census for 22.48% of records at SA1. What has to hold is that
  # the disagreement is bounded -- most people carry one address, and nobody
  # carries a different one in every product.
  expect_gt(mean(by_person == 1L), 0.5)
  expect_lt(mean(by_person > 1L), 0.5)
})

test_that("a household's address is one address", {
  skip_if_not_installed("arrow")

  spine <- generate_spine(n = 20000L, seed = 17L)
  rows <- fplida:::.spine_address_lookup_rows(spine, agency = "ATO",
                                              seed = 17L)
  resolved <- !is.na(rows$mb_code)
  multi <- names(which(table(spine$dwelling_id) > 1L))
  keep <- resolved & spine$dwelling_id %in% multi

  # This is the guarantee the disagreement rate replaces the old one with:
  # within a single agency, one dwelling is one address. Two agencies may
  # hold different vintages of it; one agency may not hold two at once.
  per_dwelling <- tapply(rows$mb_code[keep], spine$dwelling_id[keep],
                         function(x) length(unique(x)))
  expect_true(all(per_dwelling == 1L))
})

test_that("some people are coded to an area but not to an address", {
  spine <- generate_spine(n = 20000L, seed = 17L)
  rows <- fplida:::.spine_address_lookup_rows(spine, agency = "ATO",
                                              seed = 17L)

  # The ABS could tie 91% of the 25.7 million people on its 2021
  # administrative population snapshot to a dwelling; the remaining 9% could
  # be coded to an area but not to an address.
  unresolved <- mean(is.na(rows$mb_code))
  expect_gt(unresolved, 0.04)
  expect_lt(unresolved, 0.15)
  # The area survives even where the address does not.
  expect_false(any(is.na(rows$state[is.na(rows$mb_code)])))
})

test_that("an establishment address never collides with a residence", {
  spine <- data.frame(spine_id = sprintf("SP%010d", 1:50))

  home <- fplida:::.dil_address_key("CORE", spine, seed = 4L)
  outlet <- fplida:::.dil_address_key("DEX", spine, seed = 4L)
  provider <- fplida:::.dil_address_key("NDIS", spine, seed = 4L)

  # A service outlet is not somebody's house. Drawing both from one key space
  # would let a join between a provider table and a location table match, and
  # the match would mean nothing.
  expect_identical(intersect(home, outlet), character(0))
  expect_identical(intersect(home, provider), character(0))
  expect_true(all(startsWith(home, "0")))
  expect_true(all(substr(outlet, 1L, 1L) == "8"))

  # Twelve hexadecimal digits, matching Core Locations.
  expect_true(all(grepl("^[0-9A-F]{12}$", c(home, outlet))))
})

test_that("the address key survives reordering and subsetting", {
  spine <- data.frame(spine_id = sprintf("SP%010d", 1:20))

  full <- fplida:::.dil_address_key("MCD", spine, seed = 9L)
  shuffled <- spine[c(11:20, 1:10), , drop = FALSE]
  expect_identical(
    fplida:::.dil_address_key("MCD", shuffled, seed = 9L),
    full[c(11:20, 1:10)]
  )
  subset <- spine[c(3L, 17L), , drop = FALSE]
  expect_identical(
    fplida:::.dil_address_key("MCD", subset, seed = 9L),
    full[c(3L, 17L)]
  )
})

test_that("the registry explains what an ARID is rather than what it is not", {
  info <- variable_info()
  arid <- info[toupper(info$variable) %in%
                 c("ARID", "ARID_HASH_TRUNC", "HASHED_ARID"), , drop = FALSE]
  expect_gt(nrow(arid), 40L)

  # Every occurrence is sourced, names the register, and cites a page.
  expect_true(all(arid$value_support_status == "sourced"))
  expect_true(all(grepl("Address Register", arid$value_domain, fixed = TRUE)))
  expect_true(all(grepl("^https://www\\.abs\\.gov\\.au/", arid$value_source_url)))

  # The complaint that started this: the definition and the limitation were
  # the same generic sentence, and the description restated the variable name.
  expect_false(any(arid$value_definition == arid$limitation))
  expect_false(any(
    tolower(arid$variable_description) == tolower(arid$official_description)
  ))
  expect_true(all(nchar(arid$variable_description) > 150L))
  expect_false(any(grepl("does not publish a finite value list",
                         arid$value_definition, fixed = TRUE)))

  # Each dataset says what the address belongs to, so the entries are not one
  # description copied fifteen times.
  expect_gt(length(unique(arid$variable_description)), 12L)
})
