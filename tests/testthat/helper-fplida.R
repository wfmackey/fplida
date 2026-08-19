# Shared test infrastructure.
# This file is sourced before any test file runs.

# Set a shared temp directory as the fplida data path for all tests.
# Each test file gets a clean subdirectory under this.
.test_data_dir <- file.path(tempdir(), "fplida_dp_test")
if (!dir.exists(.test_data_dir)) dir.create(.test_data_dir, recursive = TRUE)
options(fplida.data_path = .test_data_dir)
fplida_test_inst_path <- function(...) {
  components <- c(...)
  # The registry moved to fplida.info, so look there first: code frames and
  # the internal documentation live with the data package now.
  for (pkg in c("fplida.info", "fplida")) {
    path <- do.call(system.file, c(as.list(components), list(package = pkg)))
    if (nzchar(path)) return(path)
  }
  for (root in list(c("..", "..", "inst"),
                    c("..", "..", "fplida.info", "inst"))) {
    path <- do.call(testthat::test_path, c(as.list(root), as.list(components)))
    if (file.exists(path)) return(path)
  }
  do.call(testthat::test_path, c(list("..", "..", "inst"),
                                 as.list(components)))
}
