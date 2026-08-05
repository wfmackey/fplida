test_that("Census INGP preserves the shared-spine Indigenous status meaning", {
  n <- 2000L
  spine <- generate_spine(n = n, seed = 20260803L)
  spine$indigenous <- rep(1:4, length.out = n)
  person <- generate_census(spine = spine, seed = 20260804L)$person
  eligible_spine <- spine[
    fplida:::.census_present_on_night(spine), , drop = FALSE
  ]

  expected <- c(`1` = "4", `2` = "1", `3` = "2", `4` = "3")
  stated <- person$INGP != "&"

  expect_true(all(vapply(1:4, function(code) {
    any(stated & eligible_spine$indigenous == code)
  }, logical(1))))
  expect_identical(
    person$INGP[stated],
    unname(expected[as.character(eligible_spine$indigenous[stated])])
  )
})

test_that("standalone Census INGP weights use code 4 for non-Indigenous", {
  person <- as.data.frame(
    fplida:::generate_census_2021_person__(50000L, 20260805L),
    stringsAsFactors = FALSE
  )
  share <- prop.table(table(factor(
    person$INGP,
    levels = c("1", "2", "3", "4", "&")
  )))

  expect_gt(unname(share[["4"]]), 0.90)
  expect_lt(unname(share[["4"]]), 0.95)
  expect_gt(unname(share[["1"]]), 0.02)
  expect_lt(unname(share[["1"]]), 0.04)
  expect_lt(unname(share[["2"]]), 0.003)
  expect_lt(unname(share[["3"]]), 0.003)
})
