test_that("set_data_path and get_data_path work", {
  old_opt <- getOption("fplida.data_path")
  on.exit(options(fplida.data_path = old_opt), add = TRUE)

  renviron <- file.path(Sys.getenv("HOME"), ".Renviron")
  old_renviron <- if (file.exists(renviron)) {
    readLines(renviron, warn = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_renviron)) {
      if (file.exists(renviron)) file.remove(renviron)
    } else {
      writeLines(old_renviron, renviron)
    }
  }, add = TRUE)

  tmp <- file.path(tempdir(), "fplida_dp_test")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  set_data_path(tmp)

  # Both paths should resolve to the same location (macOS /var → /private/var)
  expect_equal(normalizePath(get_data_path()), normalizePath(tmp))
  expect_true(dir.exists(tmp))
})

test_that("get_data_path returns NULL when nothing is set", {
  old_opt <- getOption("fplida.data_path")
  old_env <- Sys.getenv("FPLIDA_DATA_PATH", "")
  on.exit({
    options(fplida.data_path = old_opt)
    if (nzchar(old_env)) {
      Sys.setenv(FPLIDA_DATA_PATH = old_env)
    } else {
      Sys.unsetenv("FPLIDA_DATA_PATH")
    }
  }, add = TRUE)

  options(fplida.data_path = NULL)
  Sys.unsetenv("FPLIDA_DATA_PATH")

  expect_null(get_data_path())
})

test_that("get_data_path falls back to env var", {
  old_opt <- getOption("fplida.data_path")
  old_env <- Sys.getenv("FPLIDA_DATA_PATH", "")
  on.exit({
    options(fplida.data_path = old_opt)
    if (nzchar(old_env)) {
      Sys.setenv(FPLIDA_DATA_PATH = old_env)
    } else {
      Sys.unsetenv("FPLIDA_DATA_PATH")
    }
  }, add = TRUE)

  options(fplida.data_path = NULL)
  Sys.setenv(FPLIDA_DATA_PATH = "/tmp/test_env_path")

  expect_equal(get_data_path(), "/tmp/test_env_path")
})
