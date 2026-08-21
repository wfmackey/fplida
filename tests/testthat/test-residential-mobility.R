# The dwelling model is only useful if it is imperfect in the ways the real
# one is. Each figure below is published; the test is that the generator hits
# it, not that it is exact.

.mobility_test_spine <- function(n = 40000L, seed = 13L) {
  generate_spine(n = n, seed = seed)
}

test_that("draws for different purposes are independent", {
  spine <- .mobility_test_spine(n = 20000L)

  a <- fplida:::.mobility_dwelling_draw(spine, 42L, "residential move")
  b <- fplida:::.mobility_dwelling_draw(spine, 42L, "move distance")

  # Adding a salt to the result only rotates the sequence, leaving two
  # purposes perfectly rank-correlated: a move distance drawn that way is not
  # independent of whether the household moved at all.
  expect_lt(abs(stats::cor(a, b)), 0.05)
  expect_lt(abs(mean(a) - 0.5), 0.02)
  expect_lt(abs(mean(b) - 0.5), 0.02)
})

test_that("move rates match the published figures", {
  spine <- .mobility_test_spine()
  history <- fplida:::.spine_move_history(spine, 42L)

  # 15.0% of people changed address in the year before the 2021 Census and
  # 40.7% over five years.
  expect_lt(abs(mean(history$moved_1yr) - 0.150), 0.02)
  expect_lt(abs(mean(history$moved_5yr) - 0.407), 0.03)
  # A one-year mover is a five-year mover.
  expect_true(all(history$moved_5yr[history$moved_1yr]))
})

test_that("a household moves as one", {
  spine <- .mobility_test_spine()
  history <- fplida:::.spine_move_history(spine, 42L)

  per_dwelling <- tapply(history$moved_5yr, spine$dwelling_id,
                         function(x) length(unique(x)))
  expect_true(all(per_dwelling == 1L))
  per_dwelling <- tapply(history$previous_sa2, spine$dwelling_id,
                         function(x) length(unique(x)))
  expect_true(all(per_dwelling == 1L))
})

test_that("87% of movers stay inside their own state", {
  spine <- .mobility_test_spine()
  history <- fplida:::.spine_move_history(spine, 42L)
  lookup <- fplida:::.load_mb_lookup()

  previous_state <- lookup$state[
    match(as.integer(history$previous_sa2), lookup$sa2_code)]
  movers <- history$moved_5yr & !is.na(previous_state)
  skip_if(!any(movers), "no movers generated")

  same <- mean(previous_state[movers] == as.integer(spine$state)[movers])
  expect_lt(abs(same - 0.870), 0.06)
})

test_that("two agencies disagree about an address at the published rates", {
  spine <- .mobility_test_spine()
  a <- fplida:::.spine_address_lookup_rows(spine, agency = "ATO", seed = 42L)
  b <- fplida:::.spine_address_lookup_rows(spine, agency = "SA", seed = 42L)
  ok <- !is.na(a$sa1_code) & !is.na(b$sa1_code)
  skip_if(!any(ok), "no resolved addresses")

  # On 2016 Census night PLIDA disagreed with the Census for 22.48% of
  # records at SA1, 18.07% at SA2, 9.50% at SA4 and 2.18% at state level.
  # Between two agencies the rate is a little lower, because disagreement
  # needs one to have caught up with a move and the other not.
  sa1 <- mean(a$sa1_code[ok] != b$sa1_code[ok])
  sa2 <- mean(a$sa2_code[ok] != b$sa2_code[ok])
  sa4 <- mean(a$sa4_code[ok] != b$sa4_code[ok])
  state <- mean(a$state[ok] != b$state[ok])

  expect_gt(sa1, 0.10)
  expect_lt(sa1, 0.30)
  # Disagreement has to fall as the geography coarsens, or the moves behind
  # it are not moves between real places.
  expect_gte(sa1, sa2)
  expect_gte(sa2, sa4)
  expect_gte(sa4, state)
  expect_lt(state, 0.06)
  expect_gt(state, 0)
})

test_that("one agency holds one address per dwelling", {
  spine <- .mobility_test_spine()
  rows <- fplida:::.spine_address_lookup_rows(spine, agency = "ATO",
                                              seed = 42L)
  multi <- names(which(table(spine$dwelling_id) > 1L))
  keep <- !is.na(rows$mb_code) & spine$dwelling_id %in% multi
  skip_if(!any(keep), "no multi-person dwellings with an address")

  per_dwelling <- tapply(rows$mb_code[keep], spine$dwelling_id[keep],
                         function(x) length(unique(x)))
  expect_true(all(per_dwelling == 1L))
})

test_that("some people are coded to an area but not to an address", {
  spine <- .mobility_test_spine()
  rows <- fplida:::.spine_address_lookup_rows(spine, agency = "ATO",
                                              seed = 42L)

  # The ABS could tie 91% of the 25.7 million people on its 2021
  # administrative population snapshot to a dwelling.
  unresolved <- mean(is.na(rows$mb_code))
  expect_gt(unresolved, 0.04)
  expect_lt(unresolved, 0.15)
  expect_false(any(is.na(rows$state[is.na(rows$mb_code)])))

  # 4.3% to 11.2% of children with an address history were missing an ARID,
  # so children are harder to place than adults.
  age <- 2021L - spine$birth_year
  expect_gt(mean(is.na(rows$mb_code[age < 18L])),
            mean(is.na(rows$mb_code[age >= 18L])))
})

test_that("Core Locations leaves unresolved addresses missing", {
  spine <- .mobility_test_spine(n = 20000L)
  locations <- fplida:::project_core_locations(spine, 42L)

  missing <- mean(is.na(locations$ARID))
  expect_gt(missing, 0.04)
  expect_lt(missing, 0.15)
  # The area survives where the address does not.
  expect_false(any(is.na(locations$STATE[is.na(locations$ARID)])))
  expect_true(all(is.na(locations$MB_ASGS_2021[is.na(locations$ARID)])))
})

test_that("place of usual residence a year and five years ago carries moves", {
  spine <- .mobility_test_spine(n = 30000L)

  pur1 <- fplida:::.dil_census_value("PUR1P", spine, 42L, 0L,
                                     "census_2021_person")
  pur5 <- fplida:::.dil_census_value("PUR5P", spine, 42L, 0L,
                                     "census_2021_person")
  current <- as.character(spine$sa2_code)
  real <- grepl("^[1-9][0-9]{8}$", pur1) & grepl("^[1-9][0-9]{8}$", pur5)
  skip_if(!any(real), "no substantive PUR codes")

  # Setting these to the person's current SA2 gave a population in which
  # nobody had moved for five years.
  moved_1 <- mean(pur1[real] != current[real])
  moved_5 <- mean(pur5[real] != current[real])
  expect_gt(moved_1, 0.05)
  expect_gt(moved_5, moved_1)
  # A move inside the SA2 changes the SA1 and not the SA2, so the share
  # showing a different SA2 sits below the headline move rate.
  expect_lt(moved_1, 0.150)
  expect_lt(moved_5, 0.407)
})


test_that("Core Locations holds an address history, not one address", {
  spine <- .mobility_test_spine(n = 20000L)
  locations <- fplida:::project_core_locations(spine, 42L)

  spells <- table(locations$SPINE_ID)
  # A person who moved has a closed spell where they used to live and an open
  # one where they live now. One open spell each, never closing, makes every
  # person look as if they had one address for the whole window.
  expect_gt(mean(spells > 1L), 0.2)
  expect_lt(mean(spells > 1L), 0.6)
  expect_equal(max(as.integer(spells)), 2L)

  # Exactly one open spell per person: you live at one address now.
  open <- tapply(is.na(locations$END_DATE), locations$SPINE_ID, sum)
  expect_true(all(open == 1L))
})

test_that("an address spell abuts the next and changes the address", {
  spine <- .mobility_test_spine(n = 20000L)
  locations <- fplida:::project_core_locations(spine, 42L)

  movers <- names(which(table(locations$SPINE_ID) == 2L))
  skip_if(!length(movers), "no movers")
  history <- locations[locations$SPINE_ID %in% utils::head(movers, 200L), ,
                       drop = FALSE]
  history <- history[order(history$SPINE_ID, history$START_DATE), ]

  by_person <- split(history, history$SPINE_ID)
  for (person in by_person) {
    if (nrow(person) != 2L) next
    # The earlier spell closes the day before the later one opens.
    expect_equal(as.Date(person$END_DATE[1L]) + 1L,
                 as.Date(person$START_DATE[2L]))
    expect_true(is.na(person$END_DATE[2L]))
    # And an ARID stands for an address, so moving changes it.
    if (!any(is.na(person$ARID))) {
      expect_false(person$ARID[1L] == person$ARID[2L])
    }
  }
})
