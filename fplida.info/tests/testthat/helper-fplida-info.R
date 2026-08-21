# Registry files ship with this package; fall back to the source tree so the
# tests run before an install.
fplida_test_inst_path <- function(...) {
  components <- c(...)
  path <- do.call(system.file,
                  c(as.list(components), list(package = "fplida.info")))
  if (nzchar(path)) return(path)
  do.call(testthat::test_path,
          c(list("..", "..", "inst"), as.list(components)))
}
