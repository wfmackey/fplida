test_that("ATO occupation is noisier than Census occupation", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  old_path <- getOption("fplida.data_path")
  old_run <- getOption("fplida.run_dir")
  on.exit({
    options(fplida.data_path = old_path)
    options(fplida.run_dir = old_run)
  }, add = TRUE)

  data_path <- file.path(tempdir(), "ato_census_occ_test")
  unlink(data_path, recursive = TRUE)
  options(fplida.data_path = data_path)

  spine <- generate_spine(n = 12000L, seed = 202L)
  generate_census(spine = spine, seed = 203L, return_data = FALSE)
  generate_pit_ps(spine = spine, seed = 204L, years = 2021L)
  generate_pit_itr(spine = spine, seed = 205L, years = 2021L)

  run_dir <- getOption("fplida.run_dir")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  q <- sprintf("
with spine as (
  select aeuid_abs, aeuid_ato
  from read_parquet('%s/_system/base-spine.parquet')
),
census as (
  select SYNTHETIC_AEUID as aeuid_abs, OCCP
  from read_parquet('%s/abs-census/madipge-cen21-d-person-2021.parquet')
),
ato as (
  select SYNTHETIC_AEUID as aeuid_ato, SUB_OCPTN_GRP_CD
  from read_parquet('%s/ato-pit_itr/madipge-ato-d-context-fy2021.parquet')
),
joined as (
  select c.OCCP, a.SUB_OCPTN_GRP_CD
  from ato a
  join spine s using (aeuid_ato)
  join census c using (aeuid_abs)
  where c.OCCP not in ('@@@@@@', '&&&&&&', 'VVVVVV')
    and a.SUB_OCPTN_GRP_CD not in ('000000', '@@@@@@', '&&&&&&', 'VVVVVV')
)
select avg((OCCP = SUB_OCPTN_GRP_CD)::int) as agreement
     , avg(regexp_matches(SUB_OCPTN_GRP_CD, '^[1-8][0-9]{3}99$')::int) as ato_nfd_rate
     , avg(regexp_matches(SUB_OCPTN_GRP_CD, '^9[0-9]{5}$')::int) as ato_special_rate
     , avg(case when substr(SUB_OCPTN_GRP_CD, 1, 1) = '1'
                then regexp_matches(SUB_OCPTN_GRP_CD, '^[1-8][0-9]{3}99$')::int
                else null end) as ato_manager_nfd_rate
     , avg(case when substr(SUB_OCPTN_GRP_CD, 1, 1) <> '1'
                then regexp_matches(SUB_OCPTN_GRP_CD, '^[1-8][0-9]{3}99$')::int
                else null end) as ato_other_nfd_rate
from joined
", run_dir, run_dir, run_dir)

  audit <- DBI::dbGetQuery(con, q)
  expect_gt(audit$agreement, 0.75)
  expect_lt(audit$agreement, 0.85)
  expect_gt(audit$ato_nfd_rate, 0.02)
  expect_gt(audit$ato_special_rate, 0.005)
  expect_gt(audit$ato_manager_nfd_rate, audit$ato_other_nfd_rate)
})
