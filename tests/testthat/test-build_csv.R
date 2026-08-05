# Tests for build_fplida(export_format = "csv") and convert_parquet_dir_to_csv.
#
# These exercise the DuckDB-based post-build parquet→CSV conversion path.
# They use small N so they run in seconds; the production path was
# benchmarked at 30m (707 GB in 14.3 min).

skip_if_no_csv_deps <- function() {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("arrow")
}

# ---------------------------------------------------------------------
# convert_parquet_dir_to_csv() unit tests
# ---------------------------------------------------------------------

test_that("convert_parquet_dir_to_csv handles flat parquet files", {
  skip_if_no_csv_deps()

  src <- tempfile("conv_src_")
  dst <- tempfile("conv_dst_")
  dir.create(file.path(src, "abs-demo"), recursive = TRUE)

  df <- data.frame(
    id     = sprintf("P%07d", 1:100),
    value  = rnorm(100),
    group  = sample(c("A", "B", "C"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(df, file.path(src, "abs-demo", "demo.parquet"))

  res <- convert_parquet_dir_to_csv(src, dst, verbose = FALSE)

  csv_path <- file.path(dst, "demo", "demo.csv")
  expect_true(file.exists(csv_path))
  expect_false(dir.exists(file.path(dst, "abs-demo")))
  back <- read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(back), 100L)
  expect_equal(back$id, df$id)
  expect_equal(sort(unique(back$group)), sort(unique(df$group)))

  expect_equal(res$total_files, 1L)
  expect_equal(res$total_rows, 100)
  expect_true(res$total_elapsed_sec >= 0)

  unlink(c(src, dst), recursive = TRUE)
})

test_that("convert_parquet_dir_to_csv handles partitioned datasets", {
  skip_if_no_csv_deps()

  src <- tempfile("conv_part_src_")
  dst <- tempfile("conv_part_dst_")
  part_dir <- file.path(src, "de-he", "madipge-hied-student-load")
  dir.create(part_dir, recursive = TRUE)

  for (i in 1:3) {
    df <- data.frame(
      aeuid = sprintf("P%07d", ((i - 1) * 10 + 1):(i * 10)),
      year  = 2020L + i,
      load  = runif(10),
      stringsAsFactors = FALSE
    )
    arrow::write_parquet(
      df,
      file.path(part_dir, sprintf("part-%03d.parquet", i))
    )
  }

  res <- convert_parquet_dir_to_csv(src, dst, verbose = FALSE)

  csv_path <- file.path(dst, "madipge-hied-student-load",
                        "madipge-hied-student-load.csv")
  expect_true(file.exists(csv_path))
  expect_false(dir.exists(file.path(dst, "de-he")))

  back <- read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(back), 30L)
  expect_equal(sort(unique(back$year)), 2021:2023)
  expect_equal(res$total_rows, 30)

  unlink(c(src, dst), recursive = TRUE)
})

test_that("convert_parquet_dir_to_csv defaults dst_dir to <src>_csv", {
  skip_if_no_csv_deps()

  parent <- tempfile("conv_default_")
  dir.create(parent)
  src    <- file.path(parent, "run")
  dir.create(file.path(src, "ato-tiny"), recursive = TRUE)
  df <- data.frame(x = 1:5)
  arrow::write_parquet(df, file.path(src, "ato-tiny", "t.parquet"))

  convert_parquet_dir_to_csv(src, verbose = FALSE)

  expect_true(file.exists(file.path(paste0(src, "_csv"), "t", "t.csv")))
  expect_false(dir.exists(file.path(paste0(src, "_csv"), "ato-tiny")))
  unlink(parent, recursive = TRUE)
})

test_that("convert_parquet_dir_to_csv writes top-level v6 spine folders", {
  skip_if_no_csv_deps()

  src <- tempfile("conv_spine_src_")
  dst <- tempfile("conv_spine_dst_")
  dir.create(file.path(src, "_system"), recursive = TRUE)
  dir.create(file.path(src, "abs-blade"), recursive = TRUE)
  dir.create(file.path(src, "ato-pit_ps"), recursive = TRUE)
  stp_part_dir <- file.path(src, "ato-stp", "stp_standard_pay_events_2020_m01")
  stp_ext_part_dir <- file.path(src, "ato-stp", "stp_extended_pay_events_2020_m01")
  dir.create(stp_part_dir, recursive = TRUE)
  dir.create(stp_ext_part_dir, recursive = TRUE)

  spine <- data.frame(
    spine_id = c("P0000001", "P0000002"),
    SYNTHETIC_AEUID = c("ATO000001", "ATO000002"),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(
    data.frame(spine_id = spine$spine_id, sex = c(1L, 2L)),
    file.path(src, "_system", "base-spine.parquet")
  )
  arrow::write_parquet(
    data.frame(spine_id = spine$spine_id, bn = c("BN000001", "BN000002")),
    file.path(src, "_system", "plida-blade-link.parquet")
  )
  arrow::write_parquet(
    data.frame(bn = c("BN000001", "BN000002"), value = c(1, 2)),
    file.path(src, "abs-blade", "blade-key-id-to-bn-key.parquet")
  )
  arrow::write_parquet(spine, file.path(src, "ato-pit_ps", "ato-spine.parquet"))
  arrow::write_parquet(spine, file.path(src, "ato-stp", "ato-spine.parquet"))
  arrow::write_parquet(
    data.frame(SYNTHETIC_AEUID = spine$SYNTHETIC_AEUID, GRS_AMT = c(10, 20)),
    file.path(src, "ato-pit_ps", "madipge-ato-d-pay-sum-fy1920.parquet")
  )
  arrow::write_parquet(
    data.frame(SYNTHETIC_AEUID = spine$SYNTHETIC_AEUID, PMT_DT = as.Date("2020-01-01")),
    file.path(stp_part_dir, "part-000.parquet")
  )
  arrow::write_parquet(
    data.frame(SYNTHETIC_AEUID = spine$SYNTHETIC_AEUID, PMT_DT = as.Date("2020-01-01")),
    file.path(stp_ext_part_dir, "part-000.parquet")
  )

  convert_parquet_dir_to_csv(
    src, dst,
    preserve_parquet_datasets = "ato-stp",
    messy_names = FALSE,
    verbose = FALSE
  )

  spine_csv <- file.path(dst, "ato-spine-v6", "ato-spine-v6.csv")
  expect_true(file.exists(spine_csv))
  expect_equal(nrow(read.csv(spine_csv, stringsAsFactors = FALSE)), 2L)
  expect_true(file.exists(file.path(dst, "base-spine-v6",
                                    "base-spine-v6.csv")))
  expect_false(dir.exists(file.path(dst, "_system")))
  expect_false(dir.exists(file.path(dst, "plida-blade-link")))
  expect_false(dir.exists(file.path(dst, "ato-pit_ps")))
  expect_true(dir.exists(file.path(dst, "ato-stp")))
  expect_false(dir.exists(file.path(dst, "stp_standard_pay_events_2020_m01")))
  expect_false(dir.exists(file.path(dst, "stp_extended_pay_events_2020_m01")))
  expect_false(dir.exists(file.path(dst, "blade-key-id-to-bn-key")))
  expect_false(file.exists(file.path(dst, "madipge-ato-d-pay-sum-fy1920",
                                     "ato-spine.csv")))
  expect_false(any(grepl("ato-spine", list.files(
    file.path(dst, "madipge-ato-d-pay-sum-fy1920"),
    recursive = TRUE
  ))))
  expect_false(any(grepl("ato-spine", list.files(
    file.path(dst, "ato-stp", "stp-standard",
              "stp_standard_pay_events_2020_m01"),
    recursive = TRUE
  ))))
  expect_true(file.exists(file.path(
    dst, "madipge-ato-d-pay-sum-fy1920",
    "madipge-ato-d-pay-sum-fy1920.csv"
  )))
  expect_true(file.exists(file.path(
    dst, "abs-blade", "blade-key-id-to-bn-key",
    "blade-key-id-to-bn-key.csv"
  )))
  expect_true(file.exists(file.path(
    dst, "ato-stp", "stp-standard",
    "stp_standard_pay_events_2020_m01", "part-000.parquet"
  )))
  expect_true(file.exists(file.path(
    dst, "ato-stp", "stp-extended",
    "stp_extended_pay_events_2020_m01", "part-000.parquet"
  )))

  unlink(c(src, dst), recursive = TRUE)
})

test_that("convert_parquet_dir_to_csv can keep legacy dataset spine files", {
  skip_if_no_csv_deps()

  src <- tempfile("conv_clean_spine_src_")
  dst <- tempfile("conv_clean_spine_dst_")
  dir.create(file.path(src, "ato-pit_ps"), recursive = TRUE)

  arrow::write_parquet(
    data.frame(spine_id = "P0000001", SYNTHETIC_AEUID = "ATO000001"),
    file.path(src, "ato-pit_ps", "ato-spine.parquet")
  )

  convert_parquet_dir_to_csv(src, dst, messy_files = FALSE, verbose = FALSE)

  expect_true(file.exists(file.path(dst, "ato-pit_ps", "ato-spine.csv")))
  expect_false(dir.exists(file.path(dst, "ato-spine-v6")))

  unlink(c(src, dst), recursive = TRUE)
})

test_that("convert_parquet_dir_to_csv can vary related product column names", {
  skip_if_no_csv_deps()

  src <- tempfile("conv_names_src_")
  dst <- tempfile("conv_names_dst_")
  dir.create(file.path(src, "dhda-mbs"), recursive = TRUE)
  dir.create(file.path(src, "dhda-pbs"), recursive = TRUE)

  arrow::write_parquet(
    data.frame(
      SYNTHETIC_AEUID = "D001",
      ITEM = "23",
      FEECHARGED = 45.5,
      BENPAID = 38.7,
      stringsAsFactors = FALSE
    ),
    file.path(src, "dhda-mbs", "madipge-mbs-d-claims-2019.parquet")
  )
  arrow::write_parquet(
    data.frame(
      SYNTHETIC_AEUID = "D001",
      ITM_CD = "1234A",
      BNFT_AMT = 18.2,
      PRSCRPTN_CNT = 1L,
      stringsAsFactors = FALSE
    ),
    file.path(src, "dhda-pbs", "madipge-pbs-d-prescriptions-2019.parquet")
  )

  convert_parquet_dir_to_csv(src, dst, verbose = FALSE)

  mbs <- read.csv(file.path(dst, "madipge-mbs-d-claims-2019",
                            "madipge-mbs-d-claims-2019.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE)
  pbs <- read.csv(file.path(dst, "madipge-pbs-d-prescriptions-2019",
                            "madipge-pbs-d-prescriptions-2019.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE)
  expect_false(dir.exists(file.path(dst, "dhda-mbs")))
  expect_false(dir.exists(file.path(dst, "dhda-pbs")))
  expect_true(all(c("ITEMNUM", "FEE_CHARGED", "BEN_PAID") %in% names(mbs)))
  expect_false(any(c("ITEM", "FEECHARGED", "BENPAID") %in% names(mbs)))
  expect_true(all(c("ITEM_CD", "BENEFIT_AMT", "PRESCRIPTION_CNT") %in% names(pbs)))
  expect_false(any(c("ITM_CD", "BNFT_AMT", "PRSCRPTN_CNT") %in% names(pbs)))

  unlink(c(src, dst), recursive = TRUE)
})

test_that("CSV conversion preserves canonical DIL column names", {
  skip_if_no_csv_deps()

  src <- tempfile("conv_canonical_names_src_")
  dst <- tempfile("conv_canonical_names_dst_")
  dir.create(file.path(src, "ato-pit_itr"), recursive = TRUE)
  stem <- paste0(
    "madipge-ato-d-context-fy1920--",
    "ato_itr_context_1920_12m"
  )
  arrow::write_parquet(
    data.frame(
      SYNTHETIC_AEUID = "A001",
      INCM_YR = 2020L,
      AGE_FY_START = 40L,
      TAXABLE_STATUS = "Y",
      stringsAsFactors = FALSE
    ),
    file.path(src, "ato-pit_itr", paste0(stem, ".parquet"))
  )

  convert_parquet_dir_to_csv(src, dst, verbose = FALSE)

  actual <- read.csv(
    file.path(dst, stem, paste0(stem, ".csv")),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_identical(
    names(actual),
    c("SYNTHETIC_AEUID", "INCM_YR", "AGE_FY_START", "TAXABLE_STATUS")
  )

  unlink(c(src, dst), recursive = TRUE)
})

# ---------------------------------------------------------------------
# build_fplida(export_format = "csv") integration tests
# ---------------------------------------------------------------------

test_that("build_fplida(export_format='csv') converts a small build", {
  skip_if_no_csv_deps()

  tmp <- tempfile("fplida_csv_build_")
  dir.create(tmp, recursive = TRUE)

  res <- build_fplida(
    n             = 1000L,
    seed          = 123L,
    years         = 2020:2021,
    products      = c("census", "core", "pit_ps"),
    k_slices      = 1L,
    export_format = "csv",
    output_dir    = tmp,
    suffix        = "csvbuild"
  )

  expect_equal(res$format, "csv")
  expect_equal(res$build_format, "parquet")
  expect_true(res$messy_files)
  expect_true(res$messy_names)
  expect_false(is.null(res$csv_conversion))
  expect_true(res$csv_conversion$total_files > 0L)
  expect_true(res$csv_conversion$total_rows > 0L)

  # Parquet run dir still exists (keep_parquet default TRUE)
  run_pq <- res$canonical_run_dir
  expect_true(dir.exists(run_pq))

  # CSV sibling dir exists with <run>_csv suffix
  run_csv <- paste0(run_pq, "_csv")
  expect_true(dir.exists(run_csv))
  expect_false(file.exists(file.path(
    run_pq, "_system", "base-spine.parquet"
  )))
  expect_false(dir.exists(file.path(run_csv, "base-spine-v6")))

  # Spot-check: census person product exists as a partition directory
  # on the parquet side (post slice merge) and as a single CSV on the
  # CSV side (convert_parquet_dir_to_csv flattens partitioned datasets).
  pq_person_dir <- file.path(run_pq, "abs-census",
                             "madipge-cen21-d-person-2021")
  expect_true(dir.exists(pq_person_dir))
  pq_parts <- list.files(pq_person_dir, pattern = "^part-.*\\.parquet$",
                         full.names = TRUE)
  expect_true(length(pq_parts) >= 1L)

  csv_person <- file.path(run_csv, "madipge-cen21-d-person-2021",
                          "madipge-cen21-d-person-2021.csv")
  expect_true(file.exists(csv_person))
  expect_false(dir.exists(file.path(run_csv, "abs-census")))

  # Row counts must match between parquet and CSV sides. The Census person
  # table excludes people known to have died before Census night.
  ds <- arrow::open_dataset(pq_person_dir, format = "parquet")
  pq_rows <- ds$num_rows
  # DuckDB-produced CSVs always have a header row.
  csv_rows <- length(readLines(csv_person)) - 1L
  expect_gt(pq_rows, 0L)
  expect_lte(pq_rows, 1000L)
  expect_equal(csv_rows, pq_rows)

  # PIT_PS: multiple FY sub-tables (as partition dirs on the parquet
  # side), each must have a top-level product folder on the CSV side.
  ps_dir_pq  <- file.path(run_pq, "ato-pit_ps")
  ps_subdirs <- list.dirs(ps_dir_pq, recursive = FALSE, full.names = FALSE)
  expect_true(length(ps_subdirs) > 0L)
  ps_csv <- file.path(run_csv, ps_subdirs, paste0(ps_subdirs, ".csv"))
  expect_true(all(file.exists(ps_csv)))
  expect_false(dir.exists(file.path(run_csv, "ato-pit_ps")))
  expect_true(file.exists(file.path(run_csv, "ato-spine-v6",
                                    "ato-spine-v6.csv")))
  expect_true(file.exists(file.path(run_csv, "abs-spine-v6",
                                    "abs-spine-v6.csv")))

  unlink(tmp, recursive = TRUE)
})

test_that("CSV builds export the base file only when requested", {
  skip_if_no_csv_deps()

  tmp <- tempfile("fplida_csv_base_")
  dir.create(tmp, recursive = TRUE)

  res <- build_fplida(
    n = 100L,
    seed = 124L,
    years = 2020:2021,
    products = "core",
    k_slices = 1L,
    export_format = "csv",
    output_dir = tmp,
    suffix = "csvbase",
    export_base_file = TRUE
  )

  run_pq <- res$canonical_run_dir
  run_csv <- paste0(run_pq, "_csv")
  expect_true(res$export_base_file)
  expect_true(file.exists(file.path(
    run_pq, "_system", "base-spine.parquet"
  )))
  expect_true(file.exists(file.path(
    run_csv, "base-spine-v6", "base-spine-v6.csv"
  )))

  unlink(tmp, recursive = TRUE)
})

test_that("build_fplida(export_format='csv', keep_parquet=FALSE) removes parquet", {
  skip_if_no_csv_deps()

  tmp <- tempfile("fplida_csv_only_")
  dir.create(tmp, recursive = TRUE)

  res <- build_fplida(
    n             = 500L,
    seed          = 7L,
    years         = 2020:2021,
    products      = c("census"),
    k_slices      = 1L,
    export_format = "csv",
    output_dir    = tmp,
    suffix        = "csvonly",
    keep_parquet  = FALSE
  )

  # canonical_run_dir points to the retained CSV side.
  run_csv <- res$canonical_run_dir
  run_pq <- sub("_csv$", "", run_csv)
  expect_false(dir.exists(run_pq))
  expect_true(dir.exists(run_csv))

  # CSV output present
  expect_true(file.exists(file.path(
    run_csv, "madipge-cen21-d-person-2021",
    "madipge-cen21-d-person-2021.csv"
  )))
  expect_true(file.exists(file.path(run_csv, "abs-spine-v6",
                                    "abs-spine-v6.csv")))
  expect_false(dir.exists(file.path(run_csv, "abs-census")))

  # fplida.run_dir option now points at the CSV side
  expect_equal(getOption("fplida.run_dir"), run_csv)

  unlink(tmp, recursive = TRUE)
})

test_that("build_fplida(export_format='csv') preserves STP as parquet", {
  skip_if_no_csv_deps()

  tmp <- tempfile("fplida_csv_stp_")
  dir.create(tmp, recursive = TRUE)

  res <- build_fplida(
    n             = 1000L,
    seed          = 19L,
    years         = 2020L,
    products      = c("census", "stp"),
    k_slices      = 1L,
    export_format = "csv",
    output_dir    = tmp,
    suffix        = "stpcsv",
    keep_parquet  = FALSE
  )

  run_csv <- res$canonical_run_dir
  stp_root <- file.path(run_csv, "ato-stp")
  stp_standard_dirs <- list.files(file.path(stp_root, "stp-standard"),
                                  pattern = "^stp_standard_",
                                  full.names = TRUE)
  stp_extended_dirs <- list.files(file.path(stp_root, "stp-extended"),
                                  pattern = "^stp_extended_",
                                  full.names = TRUE)
  expect_true(dir.exists(stp_root))
  expect_gt(length(stp_standard_dirs), 0L)
  expect_gt(length(stp_extended_dirs), 0L)
  expect_false(length(list.files(run_csv, pattern = "^stp_",
                                 full.names = TRUE)) > 0L)
  expect_gt(length(list.files(stp_root, pattern = "\\.parquet$",
                              recursive = TRUE)), 0L)
  expect_equal(length(list.files(stp_root, pattern = "\\.csv$",
                                 recursive = TRUE)), 0L)
  expect_false(any(grepl("ato-spine", list.files(stp_root, recursive = TRUE))))
  expect_true(file.exists(file.path(run_csv, "ato-spine-v6",
                                    "ato-spine-v6.csv")))
  expect_true(file.exists(file.path(
    run_csv, "madipge-cen21-d-person-2021",
    "madipge-cen21-d-person-2021.csv"
  )))

  unlink(tmp, recursive = TRUE)
})
