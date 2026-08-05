#!/usr/bin/env Rscript

# check_income_reconciliation.R
# ------------------------------------------------------------------------------
# Regression check that STP, PAYG and PIT derive salary/wages from the same
# shared employment panel and reconcile.
#
#   PAYG payment summaries (ato-pit_ps)  : GROSS_PAYMENTS, TAX_WITHHELD per employer-FY
#   PIT income-edited     (ato-pit_ie)   : WANDS (wages & salary) per person-FY
#   STP pay events        (ato-stp)      : PMT_SUMRY_TOTL_GRS_PMT_AMT per person-FY
#
# Reconciliation guarantees after the shared-panel refactor:
#   * EXACT  : sum(PAYG GROSS_PAYMENTS over employers) == PIT_IE WANDS, per person-FY.
#   * EXACT  : PIT_ITR aggregates the PAYG summaries, so its wage line ties out too.
#   * ANCHOR : STP income is scaled to the same per-person-FY panel total, so STP
#              gross tracks PAYG gross (median ratio ~ 1); per-cent / per-employer
#              tie-out is a further refinement (STP iterating the panel entries).
#
# Usage:  Rscript scripts/check_income_reconciliation.R   (from the package root)

suppressWarnings(suppressMessages({
  library(dplyr); library(arrow)
  if (requireNamespace("pkgload", quietly = TRUE)) pkgload::load_all(".", quiet = TRUE) else library(fplida)
}))

n    <- as.integer(Sys.getenv("FPLIDA_RECON_N", "6000"))
seed <- 7L
tmp  <- file.path(tempdir(), "income_recon"); unlink(tmp, recursive = TRUE); dir.create(tmp)

message(sprintf("== Building spine + PAYG + PIT_IE + STP (n=%d, seed=%d) ==", n, seed))
sp <- suppressMessages(generate_spine(n = n, seed = seed, output_dir = tmp, format = "parquet"))
invisible(suppressMessages(generate_pit_ps(spine = sp, seed = seed, years = 2020:2024, output_dir = tmp)))
invisible(suppressMessages(generate_pit_ie(spine = sp, seed = seed, years = 2020:2024, output_dir = tmp)))
invisible(suppressMessages(generate_stp(spine = sp, seed = seed, years = 2020:2024, output_dir = tmp)))

rd   <- getOption("fplida.run_dir", tmp)
allf <- list.files(rd, recursive = TRUE, full.names = TRUE, pattern = "\\.parquet$")
rdp  <- function(pat) {
  f <- allf[grepl(pat, allf)]
  if (!length(f)) return(NULL)
  as_tibble(collect(open_dataset(f)))
}

payg <- rdp("pay-sum") |>
  group_by(SYNTHETIC_AEUID, fy = as.integer(FINANCIAL_YEAR)) |>
  summarise(payg_gross = sum(GROSS_PAYMENTS), payg_whld = sum(TAX_WITHHELD), .groups = "drop")
pit  <- rdp("income-edited") |>
  mutate(fy = as.integer(substr(FIN_YEAR, 1, 4)) + 1L) |>
  group_by(SYNTHETIC_AEUID, fy) |>
  summarise(wands = sum(WANDS), .groups = "drop")
stp  <- rdp("stp_standard_pay_events") |>
  mutate(fy = as.integer(substr(PYRL_FNCL_YR, 1, 4)) + 1L) |>
  group_by(SYNTHETIC_AEUID, fy) |>
  summarise(stp_gross = sum(PMT_SUMRY_TOTL_GRS_PMT_AMT), .groups = "drop")

cat("\n=========== INCOME RECONCILIATION ===========\n")

# 1. PAYG <-> PIT_IE : exact per person-FY
jp <- inner_join(payg, pit, by = c("SYNTHETIC_AEUID", "fy")) |> mutate(d = abs(payg_gross - wands))
cat(sprintf("PAYG gross == PIT_IE WANDS : %d/%d exact (%.2f%%)  max diff $%.2f\n",
            sum(jp$d < 0.01), nrow(jp), 100 * mean(jp$d < 0.01), max(jp$d)))

# 2. STP <-> PAYG : anchored (same panel total)
js <- inner_join(payg, stp, by = c("SYNTHETIC_AEUID", "fy")) |> mutate(r = stp_gross / pmax(payg_gross, 1))
cat(sprintf("STP gross  ~  PAYG gross   : median ratio %.3f  mean %.3f  cor %.3f  (n=%d)\n",
            median(js$r), mean(js$r), cor(js$stp_gross, js$payg_gross), nrow(js)))

ok <- mean(jp$d < 0.01) > 0.999 && abs(median(js$r) - 1) < 0.05
cat(if (ok) "\nRECONCILIATION: PASS (PAYG/PIT exact; STP anchored)\n" else
           "\nRECONCILIATION: REVIEW (see above)\n")
