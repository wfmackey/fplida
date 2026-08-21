test_that("every catalogue entry ships and returns its published domain", {
  catalogue <- fplida.info:::.value_table_catalogue()
  expect_gt(length(catalogue), 0L)

  resolved <- utils::read.csv(
    fplida_test_inst_path("internal-docs", "resolved-value-domains.csv"),
    stringsAsFactors = FALSE
  )
  published <- stats::setNames(
    suppressWarnings(as.integer(resolved$full_list_size)),
    resolved$value_domain
  )

  for (key in names(catalogue)) {
    entry <- catalogue[[key]]
    values <- get_values(key)

    # The point of the accessor is the whole list. A table that holds most of a
    # classification is worse than none, because the caller cannot tell. The
    # declared count has to match both the file and the size the source
    # publishes, so the catalogue cannot drift from either.
    sizes <- unique(stats::na.omit(published[entry$domains]))
    expect_length(sizes, 1L)
    expect_identical(nrow(values), as.integer(sizes),
                     info = paste(key, "against", paste(entry$domains,
                                                        collapse = ", ")))
    expect_identical(entry$values, as.integer(sizes), info = key)

    expect_identical(names(values)[[1L]], "code")
    expect_false(any(duplicated(values$code)))
    expect_false(any(is.na(values$code) | !nzchar(values$code)))
    if (!is.null(entry$label)) expect_identical(names(values)[[2L]], "label")
  }
})

test_that("the catalogue lists itself and refuses a key it does not have", {
  catalogue <- get_values()
  expect_setequal(catalogue$key, names(fplida.info:::.value_table_catalogue()))
  expect_true(all(catalogue$values > 0L))
  expect_true(all(nzchar(catalogue$source)))

  # Loud rather than empty: a caller handed zero rows reads it as "this
  # classification has no codes", which is the wrong answer.
  expect_error(get_values("mbs items"), "No shipped table")
  expect_error(get_values(c("sa2", "sa1")), "single table key")
})

test_that("the named wrappers agree with the keys they stand for", {
  expect_identical(get_mbs_item_numbers(), get_values("mbs-items"))
  expect_identical(get_sa2_codes(), get_values("sa2"))
  expect_identical(get_country_codes(), get_values("countries"))
})

test_that("a variable prints the call only where the table ships", {
  info <- variable_info()
  catalogue <- fplida.info:::.value_table_catalogue()
  served <- unlist(lapply(catalogue, `[[`, "domains"), use.names = FALSE)

  prints_call <- grepl("get_values(", info$value_definition, fixed = TRUE)
  expect_gt(sum(prints_call), 0L)

  # Never advertise a table the package does not have.
  expect_true(all(info$value_domain[prints_call] %in% served))

  # Every key named in a value definition is a key the accessor answers to.
  keys <- unique(gsub('^.*get_values\\("([^"]+)"\\).*$', "\\1",
                      info$value_definition[prints_call]))
  expect_true(all(keys %in% names(catalogue)))

  # And a variable whose codes the registry already carries is not sent away
  # to fetch them.
  carried <- !trimws(info$valid_values) %in% c("", "[]")
  expect_false(any(prints_call & carried))
})
