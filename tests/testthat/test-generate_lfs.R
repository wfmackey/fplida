test_that("LLFS metadata is built from the April 2026 workbook", {
  vars <- read.csv(
    system.file("extdata", "llfs", "llfs_variables.csv", package = "fplida"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  values <- read.csv(
    system.file("extdata", "llfs", "llfs_values.csv", package = "fplida"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  coes_vars <- read.csv(
    system.file("extdata", "llfs", "coes_variables.csv", package = "fplida"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  coes_values <- read.csv(
    system.file("extdata", "llfs", "coes_values.csv", package = "fplida"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_equal(nrow(vars), 168L)
  expect_equal(nrow(values), 6810L)
  expect_true(all(c("SYNTHETIC_AEUID", "ABSRID", "ABSHID", "ABSPID",
                    "ABSMID", "LFSTATUS", "REPWTG01", "REPWTG30") %in%
                    vars$identifier))
  expect_equal(sum(vars$collection_cadence == "monthly"), 109L)
  expect_equal(sum(vars$collection_cadence == "quarterly"), 37L)
  expect_equal(sum(vars$collection_cadence == "static"), 22L)

  expect_equal(nrow(coes_vars), 220L)
  expect_equal(nrow(coes_values), 2211L)
  expect_true(all(c("SYNTHETIC_AEUID", "ABSRID", "ABSMID", "FINPRSWT",
                    "WPX0201", "WPX0230", "COEPOP1") %in%
                    coes_vars$identifier))
  expect_equal(sum(coes_vars$collection_cadence == "annual"), 171L)
  expect_equal(sum(coes_vars$collection_cadence == "biennial"), 38L)
  expect_equal(sum(coes_vars$collection_cadence == "static"), 11L)
})

test_that("LLFS household months remain valid for large integer seeds", {
  spine <- data.frame(
    household_id = rep(sprintf("H%03d", 1:20), each = 2L),
    birth_year = rep(c(1980L, 2015L), 20L),
    stringsAsFactors = FALSE
  )
  metadata <- fplida:::.llfs_household_metadata(
    spine = spine,
    selected_hh = unique(spine$household_id),
    survey_year = 2025L,
    seed = 20260803L
  )

  expect_false(anyNA(metadata$first_month))
  expect_true(all(metadata$first_month %in% 1:12))
})

test_that("generate_lfs writes a linked household panel with workbook cadence", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_lfs_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 5000L, seed = 21L, output_dir = tmp,
                 return_data = FALSE)
  lfs <- generate_lfs(seed = 21L, output_dir = tmp, return_data = TRUE,
                      sample_household_rate = 0.25, max_households = 500L)

  run_dir <- getOption("fplida.run_dir")
  product_path <- file.path(run_dir, "abs-lfs", "pmp-llfs.parquet")
  coes_path <- file.path(run_dir, "abs-lfs", "pmp-coes.parquet")
  spine_path <- file.path(run_dir, "abs-lfs", "abs-spine.parquet")
  expect_true(file.exists(product_path))
  expect_true(file.exists(coes_path))
  expect_true(file.exists(spine_path))

  vars <- read.csv(
    system.file("extdata", "llfs", "llfs_variables.csv", package = "fplida"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_setequal(names(lfs), vars$identifier)

  abs_spine <- as.data.frame(arrow::read_parquet(spine_path))
  expect_true(all(c("spine_id", "SYNTHETIC_AEUID") %in% names(abs_spine)))
  expect_true(all(unique(lfs$SYNTHETIC_AEUID) %in%
                    abs_spine$SYNTHETIC_AEUID))

  waves_by_household <- tapply(lfs$RESPNUM, lfs$ABSHID, max)
  expect_lte(max(waves_by_household), 8L)
  expect_true(any(waves_by_household < 8L))
  expect_true(all(lfs$MNTHSEL %in% 1:8))
  expect_equal(lfs$MNTHSEL, lfs$RESPNUM)

  person_waves <- split(lfs$RESPNUM, lfs$ABSRID)
  expect_true(all(vapply(
    person_waves,
    function(x) identical(sort(unique(x)), seq_len(max(x))),
    logical(1)
  )))

  month <- as.integer(lfs$ABSMID %% 100L)
  quarter_month <- month %in% c(2L, 5L, 8L, 11L)
  quarterly <- vars$identifier[vars$collection_cadence == "quarterly"]
  expect_true(all(vapply(
    quarterly,
    function(identifier) all(is.na(lfs[[identifier]][!quarter_month])),
    logical(1)
  )))
  expect_true(any(!is.na(lfs$EXPECT[quarter_month])))
  expect_true(all(!is.na(lfs$LFSTATUS)))
  expect_true(all(!is.na(lfs$WEIGHT)))

  coes_vars <- read.csv(
    system.file("extdata", "llfs", "coes_variables.csv", package = "fplida"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  coes <- as.data.frame(arrow::read_parquet(coes_path))
  expect_setequal(names(coes), coes_vars$identifier)
  expect_equal(unique(coes$ABSMID), 202508L)
  expect_true(all(coes$ABSRID %in% lfs$ABSRID))
  coes_link <- merge(
    coes[, c("ABSRID", "SYNTHETIC_AEUID")],
    lfs[lfs$ABSMID == 202508L, c("ABSRID", "SYNTHETIC_AEUID")],
    by = "ABSRID",
    suffixes = c("_coes", "_llfs"),
    all.x = TRUE
  )
  expect_true(all(!is.na(coes_link$SYNTHETIC_AEUID_llfs)))
  expect_equal(coes_link$SYNTHETIC_AEUID_coes,
               coes_link$SYNTHETIC_AEUID_llfs)
  expect_true(nrow(coes) > 0L)
  expect_lt(nrow(coes), sum(lfs$ABSMID == 202508L))
  expect_true(all(coes$FINPRSWT > 0))
  expect_true(all(coes$WPX0201 > 0))
})

test_that("LLFS bounded categorical outputs use workbook code values", {
  skip_if_not_installed("arrow")

  tmp <- tempfile("fplida_lfs_codes_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  generate_spine(n = 3000L, seed = 22L, output_dir = tmp,
                 return_data = FALSE)
  lfs <- generate_lfs(seed = 22L, output_dir = tmp, return_data = TRUE,
                      sample_household_rate = 0.30, max_households = 400L)

  values <- read.csv(
    system.file("extdata", "llfs", "llfs_values.csv", package = "fplida"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  values <- values[values$is_placeholder != "TRUE" & values$code != "...",
                   , drop = FALSE]
  bounded <- c(
    "LFSTATUS", "SEX", "FTPTEMP", "RESPTYPE", "STATEMP", "MULTJOB",
    "WORKMON", "WORKTUE", "TENURMTH", "EXPECT", "PREFAVAI", "RTRNCH3M",
    "LFSFREL", "COBMCG", "DECARR", "GCCSA", "CCBSASGC", "POPAGEGR",
    "POPREGN"
  )

  for (identifier in bounded) {
    allowed <- unique(values$code[values$identifier == identifier])
    got <- unique(as.character(lfs[[identifier]][!is.na(lfs[[identifier]])]))
    expect_true(
      all(got %in% allowed),
      info = paste(identifier, "has values outside workbook code list")
    )
  }

  coes_path <- file.path(getOption("fplida.run_dir"), "abs-lfs",
                         "pmp-coes.parquet")
  coes <- as.data.frame(arrow::read_parquet(coes_path))
  coes_values <- read.csv(
    system.file("extdata", "llfs", "coes_values.csv", package = "fplida"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = c(
      identifier = "character",
      code = "character",
      value_label = "character",
      frequency_note = "character",
      sheet = "character",
      source_row = "integer",
      value_order = "integer",
      is_placeholder = "character",
      source_workbook = "character"
    )
  )
  coes_values <- coes_values[
    coes_values$is_placeholder != "TRUE" & coes_values$code != "...",
    , drop = FALSE
  ]
  coes_bounded <- c("LFSTATUS", "SEXS", "FTPTEMPS", "COEPOP1", "COEPOP8",
                    "COEPOP14", "WKDHRSA", "HRAWMJA", "DUREMPYA")
  for (identifier in coes_bounded) {
    allowed <- unique(coes_values$code[coes_values$identifier == identifier])
    got <- unique(as.character(coes[[identifier]][!is.na(coes[[identifier]])]))
    expect_true(
      all(got %in% allowed),
      info = paste(identifier, "has values outside COES workbook code list")
    )
  }
})


# -- COES earnings ranges ----------------------------------------------------

.coes_test_frame <- function(n = 200000L, seed = 42L, survey_year = 2020L) {
  tmp <- tempfile("fplida_coes_")
  dir.create(tmp)
  spine <- generate_spine(n = n, seed = seed, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  generate_lfs(spine = spine, seed = seed, survey_year = survey_year,
               output_dir = tmp, return_data = FALSE)
  run_dir <- getOption("fplida.run_dir")
  list(
    tmp = tmp,
    coes = as.data.frame(arrow::read_parquet(
      file.path(run_dir, "abs-lfs", "pmp-coes.parquet")))
  )
}

# Edges from the code frames in inst/extdata/llfs/coes_values.csv.
.COES_TEST_BROAD <- c(seq(0, 1800, by = 200), 2000, 2500, 3000)
.COES_TEST_NARROW <- c(seq(0, 1960, by = 40), seq(2000, 2950, by = 50), 3000)
.COES_TEST_HOURLY <- c(0, seq(10, 75, by = 5), 80)

test_that("COES pay ranges band the amount beside them", {
  skip_if_not_installed("arrow")
  td <- .coes_test_frame()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)
  coes <- td$coes
  skip_if(nrow(coes) == 0L, "no COES rows generated")

  # A range drawn independently of the amount lets a respondent report
  # $1,850 in WKPAYMJB and "Under $200" in WKPYMJBR on the same row.
  banded <- function(amount_col, code_col, breaks) {
    amount <- as.numeric(coes[[amount_col]])
    code <- coes[[code_col]]
    in_scope <- code %in% sprintf("%02d", seq_along(breaks))
    skip_if(!any(in_scope), paste("no rows in scope for", code_col))
    expect_equal(code[in_scope],
                 sprintf("%02d", findInterval(amount, breaks))[in_scope])
  }

  banded("WKPAYMJB", "WKPYMJBR", .COES_TEST_BROAD)
  banded("WKPAYMJB", "WKPYMJNR", .COES_TEST_NARROW)
  banded("WKPAYSJB", "WKPYSJBR", .COES_TEST_BROAD)
  banded("WKPAYSJB", "WKPYSJNR", .COES_TEST_NARROW)
  banded("WKPAYAJB", "WKPYAJBR", .COES_TEST_BROAD)
  banded("WKPAYAJB", "WKPYAJNR", .COES_TEST_NARROW)
  banded("HRLYRMJB", "HOURLYMJ", .COES_TEST_HOURLY)
  banded("HRLYRSJB", "HOURLYSJ", .COES_TEST_HOURLY)
})

test_that("COES pay ranges reach the top of the published distribution", {
  skip_if_not_installed("arrow")
  td <- .coes_test_frame()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)
  coes <- td$coes
  skip_if(nrow(coes) == 0L, "no COES rows generated")

  # Invented breaks -- five broad codes and nine narrow, where the frames
  # carry 13 and 71 -- leave the top two-thirds empty by construction.
  expect_true(any(coes$WKPYAJBR %in% sprintf("%02d", 6:13)))
  expect_true(any(coes$WKPYAJNR %in% sprintf("%02d", 10:71)))
  expect_gt(length(unique(coes$WKPYAJNR)), 20L)
})

test_that("COES pay ranges emit only codes the frames publish", {
  skip_if_not_installed("arrow")
  td <- .coes_test_frame()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)
  coes <- td$coes
  skip_if(nrow(coes) == 0L, "no COES rows generated")

  frames <- utils::read.csv(
    fplida_test_inst_path("extdata", "llfs", "coes_values.csv"),
    stringsAsFactors = FALSE)
  published <- function(id) frames$code[frames$identifier == id]

  # The broad and hourly frames are enumerated in full.
  expect_length(setdiff(unique(coes$WKPYAJBR), published("WKPYAJBR")), 0L)
  expect_length(setdiff(unique(coes$HOURLYMJ), published("HOURLYMJ")), 0L)

  # The narrow frames elide their middle with a placeholder row, so only the
  # ends are there to check against: the span is what can be asserted.
  non_amount <- c("-1", "-2", "-3", "-9", "00")
  expect_length(
    setdiff(unique(coes$WKPYAJNR), c(sprintf("%02d", 1:71), non_amount)), 0L)
})
