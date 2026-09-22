# pkgdown builds its reference index from `_pkgdown.yml`, and it aborts rather
# than warns when an exported topic has no place in that file. Nothing else in
# the suite reads `_pkgdown.yml`, so until now an export added without an index
# entry stayed invisible: the pkgdown workflow runs on a push to `main`, which
# is after review, and it has taken the site down three times that way.
#
# The check only runs from a checkout. `_pkgdown.yml` is in `.Rbuildignore`, so
# a tarball does not carry it and `R CMD check` skips this file.

test_that("every exported topic has a place in the pkgdown index", {
  skip_if_not_installed("pkgdown")

  root <- testthat::test_path("..", "..")
  skip_if(!file.exists(file.path(root, "_pkgdown.yml")),
          "not running from the source tree")

  # check_pkgdown() is silent about success only in the sense that it returns
  # invisibly; it still prints a tick. The failure is an error carrying the
  # names of the missing topics.
  expect_no_error(suppressMessages(pkgdown::check_pkgdown(root)))
})
