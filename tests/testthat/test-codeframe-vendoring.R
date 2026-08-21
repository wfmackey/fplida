# The code frames this package's Rust crate embeds with `include_str!` live in
# `inst/extdata/codeframes/`, not in `fplida.info` with the rest of the
# registry. They have to: a tarball built by `R CMD build` contains only the
# package's own tree, so an include reaching into a sibling package fails to
# compile the moment the package is built the standard way rather than from a
# checkout with both trees side by side.
#
# That leaves two copies of the same file, and two copies drift. These tests
# are what stops them: if the registry's copy is regenerated and this one is
# not, the R side and the compiled side disagree about a classification and
# nothing else would say so.

vendored_codeframes <- function() {
  dir <- fplida_test_inst_path("extdata", "codeframes")
  if (!nzchar(dir) || !dir.exists(dir)) return(character(0))
  list.files(dir, pattern = "\\.tsv$")
}

test_that("the vendored code frames are the ones the crate embeds", {
  crate <- testthat::test_path("..", "..", "src", "rust", "src")
  skip_if(!dir.exists(crate), "not running from the source tree")

  sources <- list.files(crate, pattern = "\\.rs$", full.names = TRUE)
  includes <- unlist(lapply(sources, function(path) {
    lines <- readLines(path, warn = FALSE)
    hits <- grep("include_str!\\(\"[^\"]*codeframes/", lines, value = TRUE)
    sub('^.*codeframes/([^"]*)".*$', "\\1", hits)
  }))
  includes <- unique(includes)
  skip_if(!length(includes), "the crate embeds no code frames")

  # Every embedded frame reaches inside this package, never into a sibling.
  embedded_paths <- unlist(lapply(sources, function(path) {
    lines <- readLines(path, warn = FALSE)
    grep("include_str!\\(\"[^\"]*codeframes/", lines, value = TRUE)
  }))
  expect_false(any(grepl("fplida\\.info", embedded_paths)))

  # And every one of them is present where the build will look for it.
  package_dir <- testthat::test_path("..", "..", "inst", "extdata",
                                     "codeframes")
  skip_if(!dir.exists(package_dir), "package code frames not in the tree")
  expect_length(setdiff(includes, list.files(package_dir)), 0L)
})

test_that("a vendored code frame matches the registry's copy", {
  package_dir <- testthat::test_path("..", "..", "inst", "extdata",
                                     "codeframes")
  registry_dir <- testthat::test_path("..", "..", "fplida.info", "inst",
                                      "extdata", "codeframes")
  skip_if(!dir.exists(package_dir) || !dir.exists(registry_dir),
          "not running from the source tree")

  shared <- intersect(list.files(package_dir), list.files(registry_dir))
  skip_if(!length(shared), "no shared code frames")

  for (file in shared) {
    # Byte-identical, not merely similar: the compiled side and the R side
    # have to agree on a classification exactly.
    expect_identical(
      tools::md5sum(file.path(package_dir, file))[[1L]],
      tools::md5sum(file.path(registry_dir, file))[[1L]],
      info = file
    )
  }
})
