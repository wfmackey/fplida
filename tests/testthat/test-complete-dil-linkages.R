.complete_linkage_fixture <- function(root) {
  dir.create(file.path(root, "_system"), recursive = TRUE)
  ids <- generate_aeuids(1000L, "ATO", 42L)
  ids <- c(ids[c(1L, 2L, 3L)], ids[!.ato_record_mask(data.frame(aeuid_ato = ids))][1:2], ids[4L])
  base <- data.frame(spine_id = paste0("P", 1:6), aeuid_ato = ids,
                     aeuid_dss = paste0("D", 1:6))
  arrow::write_parquet(base, file.path(root, "_system", "base-spine.parquet"))
  stp <- dataset_dir(root, "STP")
  mcs <- dataset_dir(root, "ATO_MCS")
  lookup <- data.frame(spine_id = c("P1", NA_character_),
                       SYNTHETIC_AEUID = ids[1:2])
  arrow::write_parquet(lookup, file.path(stp, "ato-spine.parquet"))
  arrow::write_parquet(data.frame(spine_id = NA_character_, SYNTHETIC_AEUID = ids[3]),
                       file.path(mcs, "ato-spine.parquet"))
  arrow::write_parquet(data.frame(SYNTHETIC_AEUID = ids[1:4], amount = 11:14),
    file.path(stp, "pmp-stp-extended--stp_extended_etp_2019-20.parquet"))
  dir.create(file.path(mcs, "primary"))
  arrow::write_parquet(data.frame(SYNTHETIC_AEUID = ids[3], amount = 20),
                       file.path(mcs, "primary", "part-001.parquet"))
  arrow::write_parquet(data.frame(synthetic_aeuid = ids[5], amount = 30),
                       file.path(mcs, "primary", "part-002.parquet"))
  list(base = base, stp = stp, mcs = mcs, original = lookup)
}

test_that("DIL completion covers primary and companion IDs without dropping non-lodgers", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  tmp <- tempfile("dil-linkage-")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  fixture <- .complete_linkage_fixture(tmp)
  files <- list.files(tmp, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE)
  records <- files[!grepl("-spine\\.parquet$", files)]
  before <- tools::md5sum(records)
  expect_false(any(.ato_record_mask(fixture$base[4:5, , drop = FALSE])))
  result <- .dil_reconcile_agency_lookups(tmp, c("STP", "ATO_MCS"))
  stp <- as.data.frame(read_parquet_safely(file.path(fixture$stp, "ato-spine.parquet")))
  mcs <- as.data.frame(read_parquet_safely(file.path(fixture$mcs, "ato-spine.parquet")))
  expect_equal(result$STP$added, 2)
  expect_equal(result$ATO_MCS$added, 1)
  expect_equal(stp[1:2, ], fixture$original)
  expect_true(is.na(stp$spine_id[match(fixture$base$aeuid_ato[3], stp$SYNTHETIC_AEUID)]))
  expect_setequal(stp$SYNTHETIC_AEUID, fixture$base$aeuid_ato[1:4])
  expect_setequal(mcs$SYNTHETIC_AEUID, fixture$base$aeuid_ato[c(3, 5)])
  expect_false(fixture$base$aeuid_ato[6] %in% c(stp$SYNTHETIC_AEUID, mcs$SYNTHETIC_AEUID))
  expect_identical(tools::md5sum(records), before)
  lookup_files <- c(file.path(fixture$stp, "ato-spine.parquet"),
                    file.path(fixture$mcs, "ato-spine.parquet"))
  checksum <- tools::md5sum(lookup_files)
  again <- .dil_reconcile_agency_lookups(tmp, c("STP", "ATO_MCS"))
  expect_equal(sum(vapply(again, `[[`, numeric(1), "added")), 0)
  expect_identical(tools::md5sum(lookup_files), checksum)
})

test_that("DIL linkage errors leave previously published lookups intact", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  tmp <- tempfile("dil-linkage-invalid-")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  fixture <- .complete_linkage_fixture(tmp)
  path <- file.path(fixture$stp, "ato-spine.parquet")
  before <- tools::md5sum(path)
  domino <- dataset_dir(tmp, "DOMINO")
  invalid <- file.path(domino, "unknown.parquet")
  arrow::write_parquet(data.frame(SYNTHETIC_AEUID = "NOT_IN_CANONICAL_SPINE"), invalid)
  expect_error(.dil_reconcile_agency_lookups(tmp, c("STP", "DOMINO")),
                "Unresolvable emitted DSS identity")
  expect_identical(tools::md5sum(path), before)
  expect_equal(list.files(file.path(tmp, "_system"), all.files = TRUE,
                           pattern = "^\\.dil-links-"), character())
  unlink(invalid)
  conflict <- fixture$original
  conflict$spine_id[1] <- "P2"
  arrow::write_parquet(conflict, file.path(fixture$mcs, "ato-spine.parquet"))
  expect_error(.dil_reconcile_agency_lookups(tmp, "STP"),
                "Conflicting existing ATO links")
  expect_identical(tools::md5sum(path), before)
  arrow::write_parquet(conflict, path)
  unlink(file.path(fixture$mcs, "ato-spine.parquet"))
  expect_error(.dil_reconcile_agency_lookups(tmp, "STP"),
                "conflicts with canonical spine")
})

test_that("new DIL lookup IDs retain deterministic unlinked records", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  tmp <- tempfile("dil-linkage-unlinked-")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp, "_system"), recursive = TRUE)
  n <- 100L
  base <- data.frame(spine_id = paste0("P", seq_len(n)),
                     aeuid_dss = paste0("D", seq_len(n)))
  arrow::write_parquet(base, file.path(tmp, "_system", "base-spine.parquet"))
  path <- dataset_dir(tmp, "DEX")
  arrow::write_parquet(data.frame(SYNTHETIC_AEUID = base$aeuid_dss),
                       file.path(path, "primary.parquet"))
  .dil_reconcile_agency_lookups(tmp, "DEX")
  lookup <- as.data.frame(read_parquet_safely(file.path(path, "dss-spine.parquet")))
  lookup <- lookup[match(base$aeuid_dss, lookup$SYNTHETIC_AEUID), ]
  expect_identical(!is.na(lookup$spine_id), .agency_linkage_mask(n, "DSS"))
  expect_equal(lookup$spine_id[!is.na(lookup$spine_id)],
               base$spine_id[.agency_linkage_mask(n, "DSS")])
})

test_that("the canonical completion step publishes its emitted agency IDs", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  tmp <- tempfile("dil-linkage-completion-")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  generate_spine(n = 40L, seed = 42L, output_dir = tmp, use_template = FALSE)
  run_dir <- resolve_run_dir(tmp)
  result <- .complete_plida_dil_structures(run_dir, "ers", seed = 42L,
    max_rows = 40L, verbose = FALSE)
  expect_equal(result$structures_written, 1L)
  frame <- as.data.frame(read_parquet_safely(result$files[[1L]]))
  lookup <- as.data.frame(read_parquet_safely(
    file.path(dataset_dir(run_dir, "ERS"), "ato-spine.parquet")))
  expect_equal(nrow(frame), 40L)
  expect_setequal(frame$SYNTHETIC_AEUID, lookup$SYNTHETIC_AEUID)
  expect_true(any(!.ato_record_mask(data.frame(aeuid_ato = frame$SYNTHETIC_AEUID))))
})
