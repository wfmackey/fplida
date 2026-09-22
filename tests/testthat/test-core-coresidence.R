# Related people have to be co-resident on purpose, not by accident.
#
# The generator keys the address on the dwelling, so people in one dwelling
# share an ARID. If the relationship pairs ignore the dwelling, a household or
# family construction keyed on co-residence runs green and produces nothing --
# every person alone, every recorded pair separated. That is the failure mode
# that passes locally and breaks in the lab, so it is held here.
#
# The relationship and location frames are built through their own internals
# rather than through `generate_core()`, which would also build the 240-month
# residence product for no gain here.

.coresidence_cache <- new.env(parent = emptyenv())

.coresidence_fixture <- function(n = 20000L, seed = 7L) {
  key <- paste(n, seed)
  if (!is.null(.coresidence_cache[[key]])) return(.coresidence_cache[[key]])
  fixture <- .coresidence_build(n, seed)
  .coresidence_cache[[key]] <- fixture
  fixture
}

.coresidence_build <- function(n, seed) {
  spine <- generate_spine(n = n, seed = seed)
  events <- fplida:::.core_household_events(spine, seed)
  locations <- fplida:::project_core_locations(spine, seed, events$moves)
  open <- locations[is.na(locations$END_DATE), , drop = FALSE]
  list(
    spine = spine,
    relationships = events$relationships,
    locations = locations,
    open = open,
    dwelling = stats::setNames(as.character(spine$dwelling_id),
                               spine$spine_id),
    arid = stats::setNames(open$ARID, open$SPINE_ID),
    start = stats::setNames(open$START_DATE, open$SPINE_ID)
  )
}


test_that("the amendment flags exist and carry the registry's domain", {
  fx <- .coresidence_fixture()
  rel <- fx$relationships
  child <- rel[rel$COMBINED_CATEGORY == "Parent-Child", , drop = FALSE]

  expect_true(all(c("SINGLE_AMENDED", "DEATH_AMENDED") %in% names(rel)))
  expect_true(all(stats::na.omit(rel$SINGLE_AMENDED) %in% c(0L, 1L)))
  expect_true(all(stats::na.omit(rel$DEATH_AMENDED) %in% c(0L, 1L)))
  expect_true(all(rel$SOURCES %in% c("ATO", "BIRTHS", "CENSUS", "DOMINO")))
  # The registry declares the flags on `core_partner_*` alone.
  expect_true(all(is.na(child$SINGLE_AMENDED)))
  expect_true(all(is.na(child$DEATH_AMENDED)))
})


test_that("most couples are co-resident and a minority live apart", {
  fx <- .coresidence_fixture()
  partner <- fx$relationships[
    fx$relationships$COMBINED_CATEGORY == "Partner", , drop = FALSE]
  together <- fx$dwelling[partner$SPINE_ID_ORIGINAL] ==
    fx$dwelling[partner$SPINE_ID_MAIN_REL]

  expect_gt(mean(together), 0.80)
  # A population in which every couple shares an address lets a pipeline that
  # reads co-residence off the address run clean here and find nothing in the
  # lab.
  expect_gt(mean(!together), 0)
  expect_lt(mean(!together), 0.20)
})


test_that("children live with a linked parent, and some do not", {
  fx <- .coresidence_fixture()
  child <- fx$relationships[
    fx$relationships$COMBINED_CATEGORY == "Parent-Child", , drop = FALSE]
  co <- fx$dwelling[child$SPINE_ID_ORIGINAL] ==
    fx$dwelling[child$SPINE_ID_MAIN_REL]

  expect_gt(mean(co), 0.70)
  expect_gt(mean(!co), 0)
})


test_that("a child can have two parent links", {
  fx <- .coresidence_fixture()
  child <- fx$relationships[
    fx$relationships$COMBINED_CATEGORY == "Parent-Child", , drop = FALSE]
  links <- unique(child[, c("SPINE_ID_ORIGINAL", "SPINE_ID_MAIN_REL")])
  per_child <- table(links$SPINE_ID_ORIGINAL)

  expect_gt(mean(per_child >= 2L), 0)
  expect_true(all(per_child <= 2L))
})


test_that("relationships end, with and without a flag", {
  fx <- .coresidence_fixture()
  partner <- fx$relationships[
    fx$relationships$COMBINED_CATEGORY == "Partner", , drop = FALSE]

  expect_gt(mean(!is.na(partner$RECORD_END)), 0)
  expect_gt(sum(partner$SINGLE_AMENDED %in% 1L), 0)
  expect_gt(sum(partner$DEATH_AMENDED %in% 1L), 0)
  # The unobserved separation: an end date the record cannot account for.
  silent <- !is.na(partner$RECORD_END) &
    partner$RECORD_END != partner$RECORD_START &
    partner$SINGLE_AMENDED == 0L & partner$DEATH_AMENDED == 0L
  expect_gt(sum(silent), 0)

  ended <- !is.na(partner$RECORD_END)
  expect_true(all(as.Date(partner$RECORD_END[ended]) >=
                    as.Date(partner$RECORD_START[ended])))
})


test_that("CORE and Census name the same couple", {
  fx <- .coresidence_fixture()
  partner <- fx$relationships[
    fx$relationships$COMBINED_CATEGORY == "Partner", , drop = FALSE]
  roles <- census_household_roles(fx$spine, 7L)
  couple <- roles$SPIP %in% c("1", "2") &
    roles$RLHP %in% c("11", "15", "17", "18")
  named <- unique(c(partner$SPINE_ID_ORIGINAL, partner$SPINE_ID_MAIN_REL))

  # CORE sees the whole population and the Census only Census night, so the
  # two can name different reference people in a household whose oldest adult
  # died first. Everything else has to agree.
  expect_gt(mean(roles$spine_id[couple] %in% named), 0.90)

  # And they agree about whether the couple is married.
  status <- partner$COMBINED_STATUS[
    match(roles$spine_id[couple], partner$SPINE_ID_ORIGINAL)]
  registered <- roles$RLHP[couple] == "11"
  ok <- !is.na(status)
  expect_true(all((status[ok] == "Married") == registered[ok]))
})


test_that("a pair can be recorded twice, from two sources", {
  fx <- .coresidence_fixture()
  partner <- fx$relationships[
    fx$relationships$COMBINED_CATEGORY == "Partner", , drop = FALSE]
  key <- paste(pmin(partner$SPINE_ID_ORIGINAL, partner$SPINE_ID_MAIN_REL),
               pmax(partner$SPINE_ID_ORIGINAL, partner$SPINE_ID_MAIN_REL))

  sources <- tapply(partner$SOURCES, key, function(x) length(unique(x)))
  expect_gt(sum(sources > 1L), 0)

  # The Census sees the couple on one night, so its record is a point.
  point <- !is.na(partner$RECORD_END) &
    partner$RECORD_START == partner$RECORD_END
  expect_gt(sum(point), 0)
  expect_true(all(partner$RECORD_START[point] == "2021-08-10"))

  # PAIRID identifies the pair, so the two source rows carry one value.
  expect_true(all(tapply(key, partner$PAIRID,
                         function(x) length(unique(x))) == 1L))
})


test_that("two members of a couple change address records months apart", {
  fx <- .coresidence_fixture()
  partner <- fx$relationships[
    fx$relationships$COMBINED_CATEGORY == "Partner", , drop = FALSE]
  together <- fx$dwelling[partner$SPINE_ID_ORIGINAL] ==
    fx$dwelling[partner$SPINE_ID_MAIN_REL]
  live <- partner[together & is.na(partner$RECORD_END), , drop = FALSE]

  same_now <- fx$arid[live$SPINE_ID_ORIGINAL] ==
    fx$arid[live$SPINE_ID_MAIN_REL]
  lagged <- fx$start[live$SPINE_ID_ORIGINAL] !=
    fx$start[live$SPINE_ID_MAIN_REL]
  # Same address, different day it was reported on.
  expect_gt(mean(lagged & same_now, na.rm = TRUE), 0)
})


test_that("a separated couple ends up at two addresses", {
  fx <- .coresidence_fixture()
  partner <- fx$relationships[
    fx$relationships$COMBINED_CATEGORY == "Partner", , drop = FALSE]
  together <- fx$dwelling[partner$SPINE_ID_ORIGINAL] ==
    fx$dwelling[partner$SPINE_ID_MAIN_REL]
  # A point record is one night's observation, not an ending, and a death
  # moves nobody.
  separated <- partner[together & !is.na(partner$RECORD_END) &
                         partner$RECORD_END != partner$RECORD_START &
                         partner$DEATH_AMENDED %in% 0L, , drop = FALSE]
  skip_if(!nrow(separated), "no separations")

  apart <- fx$arid[separated$SPINE_ID_ORIGINAL] !=
    fx$arid[separated$SPINE_ID_MAIN_REL]
  expect_gt(mean(apart, na.rm = TRUE), 0.80)
})


test_that("co-residents who stayed still share one address", {
  fx <- .coresidence_fixture()
  # Never moved and never left, so the open spell is the dwelling's own.
  keep <- fx$open$SPINE_ID[!is.na(fx$open$ARID) &
                             fx$open$START_DATE == "2006-01-01"]
  skip_if(!length(keep), "nobody stayed put")

  per_dwelling <- tapply(fx$arid[keep], fx$dwelling[keep],
                         function(x) length(unique(x)))
  expect_true(all(per_dwelling == 1L))
  # And distinct between dwellings, or the key would join two addresses.
  expect_equal(length(unique(fx$arid[keep])),
               length(unique(fx$dwelling[keep])))
})


test_that("a leaver closes the shared address and opens another", {
  fx <- .coresidence_fixture()
  events_moves <- fx$locations[
    fx$locations$SPINE_ID %in% names(which(table(fx$locations$SPINE_ID) == 2L)),
    , drop = FALSE]
  skip_if(!nrow(events_moves), "no multi-spell people")

  # Whatever put a person on two spells, the two abut and the address changes.
  history <- events_moves[order(events_moves$SPINE_ID,
                                events_moves$START_DATE), , drop = FALSE]
  first <- history[c(TRUE, FALSE), , drop = FALSE]
  second <- history[c(FALSE, TRUE), , drop = FALSE]
  expect_identical(first$SPINE_ID, second$SPINE_ID)
  expect_true(all(as.Date(first$END_DATE) + 1L == as.Date(second$START_DATE)))
  expect_true(all(is.na(second$END_DATE)))
  resolved <- !is.na(first$ARID) & !is.na(second$ARID)
  expect_true(all(first$ARID[resolved] != second$ARID[resolved]))
})
