test_that("tax decision ledger preserves every original remediation pair", {
  path <- fplida_test_inst_path(
    "internal-docs", "tax-admin-value-decisions.csv"
  )
  ledger <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  )

  expect_equal(nrow(ledger), 184L)
  expect_equal(
    anyDuplicated(paste(ledger$dataset, ledger$variable)),
    0L
  )
  expect_setequal(
    unique(ledger$dataset),
    c(
      "CGT", "SMSF", "ATO_MCS", "ATO_CR",
      "PIT_ITR", "PIT_PS", "PIT_IE", "BUSOWN"
    )
  )
  expect_equal(sum(ledger$occurrence_count), 2222L)

  expected_occurrences <- c(
    ATO_CR = 33L, ATO_MCS = 87L, BUSOWN = 15L, CGT = 1062L,
    PIT_IE = 5L, PIT_ITR = 825L, PIT_PS = 51L, SMSF = 144L
  )
  observed_occurrences <- tapply(
    ledger$occurrence_count, ledger$dataset, sum
  )
  expect_equal(
    as.integer(observed_occurrences[names(expected_occurrences)]),
    unname(expected_occurrences)
  )
})

test_that("tax decision ledger has exact outcome and implementation counts", {
  path <- fplida_test_inst_path(
    "internal-docs", "tax-admin-value-decisions.csv"
  )
  ledger <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  )

  expect_equal(sum(ledger$populated_occurrence_count), 2042L)
  expect_equal(sum(ledger$synthetic_structural_occurrence_count), 26L)
  expect_equal(
    sum(ledger$unsupported_private_codeframe_occurrence_count),
    154L
  )
  expect_equal(
    as.integer(table(factor(
      ledger$status,
      levels = c(
        "populated", "synthetic_structural",
        "unsupported_private_codeframe"
      )
    ))),
    c(161L, 2L, 21L)
  )
  expect_equal(
    as.integer(tapply(
      ledger$occurrence_count, ledger$implementation_class, sum
    )[c(
      "tax_specific_rule", "legacy_cgt_derivation",
      "generic_structural_geography", "typed_structural_missing",
      "unsupported_private_codeframe"
    )]),
    c(1078L, 926L, 38L, 26L, 154L)
  )

  structural <- ledger[ledger$status == "synthetic_structural", ]
  expect_setequal(
    structural$variable,
    c("M_EMPLYEE_SHARE_SCHM_N", "N_DIV149_O")
  )
  expect_true(all(structural$dataset == "CGT"))
})

test_that("tax decision ledger records evidence, caveats and next sources", {
  path <- fplida_test_inst_path(
    "internal-docs", "tax-admin-value-decisions.csv"
  )
  ledger <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  )

  expect_true(all(nzchar(ledger$determination_rule)))
  expect_true(all(nzchar(ledger$evidence_source)))
  expect_true(all(nzchar(ledger$evidence_url)))
  expect_true(all(nzchar(ledger$caveat)))
  expect_true(all(nzchar(ledger$candidate_status)))
  expect_true(all(nzchar(ledger$candidate_rule)))
  expect_true(all(nzchar(ledger$candidate_evidence_source)))
  expect_true(all(nzchar(ledger$candidate_caveat)))

  implemented <- ledger[
    ledger$candidate_status ==
      "implemented_official_cross_asset_codeframe", , drop = FALSE
  ]
  expect_setequal(
    implemented$variable,
    c(
      "BBI_ELGBL_AST_BBI_CD", "TFE_ELGBL_AST_OPT_OUT_CD",
      "BUSINESS_MARKET_SEGMENT"
    )
  )
  expect_equal(sum(implemented$occurrence_count), 13L)
  expect_true(all(implemented$status == "populated"))

  public_health <- ledger[
    ledger$candidate_status ==
      "implemented_current_public_codeframe", , drop = FALSE
  ]
  expect_setequal(
    public_health$variable,
    c(
      "HLTH_FND_CD", "HLTH_FND_CD_1", "HLTH_FND_CD_2",
      "HLTH_FND_CD_3", "HLTH_FND_CD_4"
    )
  )
  expect_equal(sum(public_health$occurrence_count), 63L)
  expect_true(all(public_health$status == "populated"))

  lodgement <- ledger[
    ledger$candidate_status == "implemented_alife_manual_codeframe",
    , drop = FALSE
  ]
  expect_identical(lodgement$dataset, "PIT_ITR")
  expect_identical(lodgement$variable, "LODGMENT_SOURCE")
  expect_identical(lodgement$occurrence_count, 16L)
  expect_identical(lodgement$status, "populated")
})
