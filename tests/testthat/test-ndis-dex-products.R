# NDIS publishes six products and DEX fifteen tables. Each generator wrote
# three, so a consumer who read the data item list, found `ndis_carerdemo` or
# `special_organisation`, and went looking for it found nothing.

.ndis_dex_run <- function(n = 6000L, seed = 8L) {
  tmp <- tempfile("fplida_ndis_dex_")
  dir.create(tmp)
  spine <- generate_spine(n = n, seed = seed, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  generate_ndis(spine = spine, seed = seed, output_dir = tmp,
                return_data = FALSE)
  generate_dex(spine = spine, seed = seed, output_dir = tmp,
               return_data = FALSE)
  run_dir <- getOption("fplida.run_dir")
  list(tmp = tmp, ndis = file.path(run_dir, "ndia-ndis"),
       dex = file.path(run_dir, "dss-dex"))
}

.ndis_dex_read <- function(dir, file) {
  path <- file.path(dir, file)
  if (!file.exists(path)) return(NULL)
  as.data.frame(arrow::read_parquet(path))
}


test_that("every published NDIS product is written", {
  skip_if_not_installed("arrow")
  td <- .ndis_dex_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  for (family in c("participants", "payments", "plansupports", "carers",
                   "providers", "outcomes")) {
    frame <- .ndis_dex_read(
      td$ndis, sprintf("madipge-ndis-exp-d-%s-13-current.parquet", family))
    expect_false(is.null(frame), info = family)
    expect_gt(nrow(frame), 0L)
  }
})

test_that("the NDIS carers table carries its registry variables", {
  skip_if_not_installed("arrow")
  td <- .ndis_dex_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  carers <- .ndis_dex_read(td$ndis,
                           "madipge-ndis-exp-d-carers-13-current.parquet")
  info <- as.data.frame(variable_info("NDIS"))
  expected <- sort(unique(info$variable[info$table == "ndis_carerdemo"]))
  expect_setequal(names(carers), expected)
})

test_that("every published DEX table is written", {
  skip_if_not_installed("arrow")
  td <- .ndis_dex_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  info <- as.data.frame(variable_info("DEX"))
  for (table in unique(info$table)) {
    frame <- .ndis_dex_read(
      td$dex, sprintf("madipge-dex-d-extended-15-current-%s.parquet", table))
    expect_false(is.null(frame), info = table)
    expected <- unique(info$variable[info$table == table])
    # Every variable the data item list gives the table is present. A
    # reference table drops the client identifier, which is the point of it
    # being a reference table.
    if (!is.null(fplida:::.DEX_CATALOGUE_ROWS[[table]])) {
      expected <- setdiff(expected, "SYNTHETIC_AEUID")
    }
    expect_length(setdiff(expected, names(frame)), 0L)
  }
})

test_that("a DEX reference table is a catalogue, not a per-client record", {
  skip_if_not_installed("arrow")
  td <- .ndis_dex_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  clients <- .ndis_dex_read(
    td$dex, "madipge-dex-d-extended-15-current-special_client.parquet")
  skip_if(is.null(clients) || nrow(clients) < 50L, "too few clients")

  for (table in c("special_organisation", "special_outlet",
                  "special_program", "special_ref_calendar")) {
    frame <- .ndis_dex_read(
      td$dex, sprintf("madipge-dex-d-extended-15-current-%s.parquet", table))
    # One organisation per client would make a join on it return one
    # organisation per client, which is not what the table is for.
    expect_lt(nrow(frame), nrow(clients))
    expect_false("SYNTHETIC_AEUID" %in% names(frame))
  }
})
