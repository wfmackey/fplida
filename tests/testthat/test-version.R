test_that("fplida_version returns a non-empty string", {
  v <- fplida_version()

  expect_type(v, "character")
  expect_length(v, 1)
  expect_true(nchar(v) > 0)
})
