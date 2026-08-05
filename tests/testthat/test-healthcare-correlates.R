test_that("DOMINO benefit status correlates with healthcare usage", {
  tmp <- tempfile("fplida-healthcare-correlates-")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  spine <- suppressMessages(
    generate_spine(n = 12000L, seed = 731L, output_dir = tmp, format = "parquet")
  )
  domino <- suppressMessages(
    generate_domino(
      spine = spine, seed = 731L, years = 2015L:2025L,
      output_dir = tmp, return_data = TRUE
    )
  )
  mbs <- suppressMessages(
    generate_mbs(
      spine = spine, seed = 731L, years = 2015L:2025L,
      output_dir = tmp, return_data = TRUE, chunk_size = 3000L
    )
  )
  pbs <- suppressMessages(
    generate_pbs(
      spine = spine, seed = 731L, years = 2015L:2025L,
      output_dir = tmp, return_data = TRUE, chunk_size = 3000L
    )
  )

  dsp_ids <- unique(domino$det_ben$SYNTHETIC_AEUID[
    domino$det_ben$BEN_TYPE_CODE == "DSP"
  ])
  jobseeker_ids <- setdiff(
    unique(domino$det_ben$SYNTHETIC_AEUID[
      domino$det_ben$BEN_TYPE_CODE %in% c("NSA", "JSP")
    ]),
    dsp_ids
  )

  group <- rep("Other", nrow(spine))
  group[spine$aeuid_dss %in% jobseeker_ids] <- "JobSeeker"
  group[spine$aeuid_dss %in% dsp_ids] <- "DSP"

  count_usage <- function(tables) {
    rows <- do.call(rbind, unname(tables))
    counts <- table(rows$SYNTHETIC_AEUID)
    out <- as.integer(counts[spine$aeuid_dhda])
    out[is.na(out)] <- 0L
    out
  }

  group <- factor(group, levels = c("Other", "JobSeeker", "DSP"))
  age_mid <- 2020L - as.integer(spine$birth_year)
  healthcare_usage <- count_usage(mbs) + count_usage(pbs)
  fit <- lm(healthcare_usage ~ group + factor(age_mid))
  adjusted_means <- vapply(levels(group), function(level) {
    newdata <- data.frame(
      group = factor(level, levels = levels(group)),
      age_mid = age_mid
    )
    mean(predict(fit, newdata = newdata))
  }, numeric(1))

  expect_gt(sum(group == "DSP"), 20)
  expect_gt(sum(group == "JobSeeker"), 100)
  expect_gt(unname(adjusted_means["DSP"]), unname(adjusted_means["JobSeeker"]) * 1.35)
  expect_gt(unname(adjusted_means["JobSeeker"]), unname(adjusted_means["Other"]) * 1.03)
  expect_lt(unname(adjusted_means["JobSeeker"]), unname(adjusted_means["Other"]) * 1.20)
})
