# The Home Affairs generators each wrote a few of their published tables and a
# subset of their columns, so a consumer reading the data item list found most
# of the dataset absent: AMEP was missing 24 of the 25 variables its address
# table publishes, VISA 37 of 48 on its application table.

.home_affairs_run <- function(n = 5000L, seed = 8L) {
  tmp <- tempfile("fplida_ha_")
  dir.create(tmp)
  spine <- generate_spine(n = n, seed = seed, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  for (generator in c(generate_amep, generate_visa, generate_mt_demogs,
                      generate_travellers)) {
    generator(spine = spine, seed = seed, output_dir = tmp,
              return_data = FALSE)
  }
  list(tmp = tmp, run_dir = getOption("fplida.run_dir"))
}

.home_affairs_columns <- function(run_dir, dataset) {
  dirs <- list.dirs(run_dir, recursive = FALSE)
  dirs <- dirs[grepl(paste0("-", tolower(dataset), "$"), dirs)]
  if (!length(dirs)) return(character(0))
  files <- list.files(dirs[[1L]], pattern = "\\.parquet$", full.names = TRUE)
  files <- files[!grepl("-spine\\.parquet$", files)]
  unique(unlist(lapply(files, function(path) {
    names(as.data.frame(arrow::read_parquet(path)))
  })))
}


test_that("every Home Affairs variable the registry publishes is emitted", {
  skip_if_not_installed("arrow")
  td <- .home_affairs_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  for (dataset in c("AMEP", "VISA", "MT_DEMOGS", "TRAVELLERS")) {
    info <- as.data.frame(variable_info(dataset))
    expected <- unique(info$variable)
    present <- .home_affairs_columns(td$run_dir, dataset)
    expect_length(setdiff(expected, present), 0L)
  }
})

test_that("completing a dataset does not disturb what it already wrote", {
  skip_if_not_installed("arrow")
  td <- .home_affairs_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  dirs <- list.dirs(td$run_dir, recursive = FALSE)
  visa_dir <- dirs[grepl("-visa$", dirs)][[1L]]
  visa <- as.data.frame(arrow::read_parquet(file.path(
    visa_dir, "madipge-mig-d-visa-2000-current.parquet")))

  # The bespoke columns keep their values; only the gaps are filled.
  expect_true("SYNTHETIC_AEUID" %in% names(visa))
  expect_gt(nrow(visa), 0L)
  expect_false(any(duplicated(names(visa))))
})
