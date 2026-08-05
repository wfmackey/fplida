# Shared test infrastructure.
# This file is sourced before any test file runs.

# Set a shared temp directory as the fplida data path for all tests.
# Each test file gets a clean subdirectory under this.
.test_data_dir <- file.path(tempdir(), "fplida_dp_test")
if (!dir.exists(.test_data_dir)) dir.create(.test_data_dir, recursive = TRUE)
options(fplida.data_path = .test_data_dir)
fplida_test_inst_path <- function(...) {
  components <- c(...)
  path <- do.call(
    system.file,
    c(as.list(components), list(package = "fplida"))
  )
  if (nzchar(path)) return(path)
  do.call(
    testthat::test_path,
    c(list("..", "..", "inst"), as.list(components))
  )
}
