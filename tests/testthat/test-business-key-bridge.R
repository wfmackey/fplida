# Every business a product names has to be a BLADE business.
#
# The delivery changed how it identifies a business partway through 2021-22.
# Files up to the change carry `ABN_HASH_TRUNC` and files after it carry `BN`,
# and `blade-key-abn-hash-trunc-to-bn-key` is the only thing that joins the two
# eras. A value rule that invents its own identifier instead of drawing one
# from the BLADE pool puts a value in neither column of that key, and the
# bridge silently matches nothing.

test_that("every value rule that names a business draws one from the pool", {
  pool <- sprintf("BN%011d", seq_len(64) * 7L)
  fplida:::.set_business_pool_r(pool)
  on.exit(fplida:::.set_business_pool_r(character(0)), add = TRUE)
  bridge <- fplida:::.abn_hash_trunc(pool)

  spine_rows <- data.frame(
    id = sprintf("P%010d", 1:12),
    spine_id = sprintf("SP%010d", 1:12),
    aeuid_ato = sprintf("%05X%07X", 0x93F7C, 1:12),
    birth_year = rep(1980L, 12),
    baseline_income = rep(60000, 12),
    stringsAsFactors = FALSE
  )
  aeuid <- spine_rows$aeuid_ato

  canonical <- function(name) {
    fplida:::.dil_general_value(
      name = name, description = "", dataset = "BUSOWN",
      product_name = "madipge-ato-d-business-owners-fy1516",
      table_name = "ato_sole_traders_fy1516",
      module_name = "Business Owners", spine_rows = spine_rows, seed = 11L
    )
  }
  admin <- function(name) {
    fplida:::.admin_value_for(
      name = name, description = "", spine_rows = spine_rows, aeuid = aeuid,
      dataset = "BUSOWN", product_name = "madipmp-ato-jobkeeper", seed = 11L,
      location_rows = NULL
    )
  }
  registry <- function(name) {
    fplida:::.dil_value_for(
      name = name, spine_rows = spine_rows, aeuid = aeuid, dataset = "BUSOWN",
      product_name = "madipge-ato-d-business-owners-fy1516", seed = 11L
    )
  }

  for_each_rule <- list(canonical = canonical, admin = admin,
                        registry = registry)
  checked <- vapply(for_each_rule, function(rule) {
    hashed <- rule("ABN_HASH_TRUNC")
    plain <- rule("BN")
    expect_true(all(plain %in% pool))
    expect_true(all(hashed %in% bridge))
    expect_true(all(grepl("^[0-9A-F]{12}$", hashed)))
    # The two names identify the same business, so one is the hash of the
    # other rather than an independent draw.
    expect_identical(hashed, fplida:::.abn_hash_trunc(plain))
    length(plain)
  }, numeric(1))
  expect_true(all(checked == nrow(spine_rows)))
})

test_that("a person keeps one business across tables, years and both eras", {
  pool <- sprintf("BN%011d", seq_len(64) * 7L)
  fplida:::.set_business_pool_r(pool)
  on.exit(fplida:::.set_business_pool_r(character(0)), add = TRUE)

  spine_rows <- data.frame(
    id = sprintf("P%010d", 1:9),
    spine_id = sprintf("SP%010d", 1:9),
    aeuid_ato = sprintf("%05X%07X", 0x93F7C, 1:9),
    birth_year = rep(1975L, 9),
    stringsAsFactors = FALSE
  )
  value <- function(name, product_name, table_name) {
    fplida:::.dil_general_value(
      name = name, description = "", dataset = "BUSOWN",
      product_name = product_name, table_name = table_name,
      module_name = "Business Owners", spine_rows = spine_rows, seed = 11L
    )
  }

  early <- value("ABN_HASH_TRUNC", "madipge-ato-d-business-owners-fy1011",
                 "ato_sole_traders_fy1011")
  late <- value("ABN_HASH_TRUNC", "madipge-ato-d-business-owners-fy1819",
                "ato_partnerships_fy1819")
  after <- value("BN", "madipge-ato-d-business-owners-fy2223",
                 "ato_sole_trader_fy2223_16m")

  expect_identical(early, late)
  expect_identical(early, fplida:::.abn_hash_trunc(after))

  # Keyed on the person, not on where the row happens to sit: reorder the
  # people and each keeps the business they had.
  shuffled <- spine_rows[rev(seq_len(nrow(spine_rows))), , drop = FALSE]
  reordered <- fplida:::.dil_general_value(
    name = "ABN_HASH_TRUNC", description = "", dataset = "BUSOWN",
    product_name = "madipge-ato-d-business-owners-fy1011",
    table_name = "ato_sole_traders_fy1011", module_name = "Business Owners",
    spine_rows = shuffled, seed = 11L
  )
  expect_identical(reordered, rev(early))
})

test_that("the canonical pass leaves a bespoke product where it finds one", {
  skip_if_not_installed("arrow")
  tmp <- tempfile("fplida_bespoke_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  stem <- fplida:::.dil_structure_stem(
    "madipge-ato-d-business-owners-fy1516", "ato_sole_traders_fy1516"
  )
  # BUSOWN writes one file; PIT_PS writes a directory of merged slice parts.
  file_path <- file.path(tmp, paste0(stem, ".parquet"))
  bespoke <- data.frame(
    SYNTHETIC_AEUID = sprintf("%05X%07X", 0x93F7C, 1:4),
    FIN_YEAR = rep("2015-16", 4L),
    ABN_HASH_TRUNC = fplida:::.abn_hash_trunc(sprintf("BN%011d", 1:4)),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(bespoke, file_path)
  expect_identical(fplida:::.dil_bespoke_structure_path(tmp, stem), file_path)

  parts_stem <- fplida:::.dil_structure_stem(
    "madipge-ato-d-pay-sum-fy2223", "ato_pay_sum_2223_12m"
  )
  parts_dir <- file.path(tmp, parts_stem)
  dir.create(parts_dir)
  arrow::write_parquet(bespoke, file.path(parts_dir, "part-000.parquet"))
  expect_identical(fplida:::.dil_bespoke_structure_path(tmp, parts_stem),
                   parts_dir)
  expect_null(fplida:::.dil_bespoke_structure_path(tmp, "not-a-product"))

  variable_rows <- data.frame(
    "Variable Name" = names(bespoke),
    "Variable Description" = rep("", ncol(bespoke)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  spine_pool <- data.frame(
    id = sprintf("P%010d", 1:4),
    spine_id = sprintf("SP%010d", 1:4),
    aeuid_ato = bespoke$SYNTHETIC_AEUID,
    birth_year = rep(1980L, 4L),
    stringsAsFactors = FALSE
  )
  rows <- fplida:::.dil_top_up_bespoke_structure(
    file_path, variable_rows, spine_pool, "BUSOWN",
    "madipge-ato-d-business-owners-fy1516", "ato_sole_traders_fy1516",
    "Business Owners", 11L
  )
  expect_identical(rows, 4L)
  expect_equal(as.data.frame(arrow::read_parquet(file_path)), bespoke)

  # A column the registry declares and the generator left out is filled, and
  # only that column: the rest of the product is untouched.
  variable_rows <- rbind(variable_rows, data.frame(
    "Variable Name" = "PYR_ARID",
    "Variable Description" = "Employer address identifier",
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  rows <- fplida:::.dil_top_up_bespoke_structure(
    file_path, variable_rows, spine_pool, "BUSOWN",
    "madipge-ato-d-business-owners-fy1516", "ato_sole_traders_fy1516",
    "Business Owners", 11L
  )
  topped <- as.data.frame(arrow::read_parquet(file_path))
  expect_identical(rows, 4L)
  expect_true("PYR_ARID" %in% names(topped))
  expect_equal(topped[names(bespoke)], bespoke)
})

test_that("a completed build's business identifiers all bridge to BLADE", {
  skip_if_not_installed("arrow")
  skip_on_cran()

  tmp <- tempfile("fplida_bridge_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- build_fplida(
    n = 3000L, seed = 31L, years = 2015L:2023L,
    products = c("spine", "blade", "busown"), k_slices = 1L,
    output_dir = tmp, complete_dil_schema = TRUE, complete_dil_rows = 60L,
    messy_files = FALSE, messy_names = FALSE
  )
  run_dir <- result$canonical_run_dir

  key <- as.data.frame(arrow::read_parquet(file.path(
    run_dir, "abs-blade", "blade-key-abn-hash-trunc-to-bn-key.parquet"
  )))
  expect_gt(nrow(key), 0L)

  files <- list.files(run_dir, pattern = "\\.parquet$", recursive = TRUE,
                      full.names = TRUE)
  files <- files[!grepl("blade-key-abn-hash-trunc-to-bn-key", files)]

  read_key_column <- function(paths, column, valid) {
    vapply(paths, function(path) {
      value <- as.data.frame(arrow::read_parquet(path))[[column]]
      value <- as.character(value[!is.na(value)])
      if (!length(value)) return(NA_real_)
      mean(value %in% valid)
    }, numeric(1), USE.NAMES = FALSE)
  }
  carries <- function(column) {
    files[vapply(files, function(path) {
      column %in% arrow::ParquetFileReader$create(path)$GetSchema()$names
    }, logical(1))]
  }

  hashed_files <- carries("ABN_HASH_TRUNC")
  plain_files <- carries("BN")
  expect_gt(length(hashed_files), 0L)
  expect_gt(length(plain_files), 0L)

  hashed_rate <- read_key_column(hashed_files, "ABN_HASH_TRUNC",
                                 key$abn_hash_trunc)
  plain_rate <- read_key_column(plain_files, "BN", key$bn)
  expect_equal(sum(hashed_rate < 1, na.rm = TRUE), 0L)
  expect_equal(sum(plain_rate < 1, na.rm = TRUE), 0L)

  # The bespoke concordance survives the completion pass: BUSOWN keeps more
  # rows than the canonical row cap, which a wholesale rewrite would not.
  busown <- list.files(file.path(run_dir, "ato-busown"),
                       pattern = "--.*\\.parquet$", full.names = TRUE)
  spans <- vapply(busown, function(path) {
    arrow::ParquetFileReader$create(path)$num_rows
  }, numeric(1), USE.NAMES = FALSE)
  expect_true(any(spans != 60L))
})
