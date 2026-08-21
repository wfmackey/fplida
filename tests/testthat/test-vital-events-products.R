# DEATHS and MCD are each published as several products, and each generator
# wrote one flat table with a few columns from each. These tests guard the
# split and the vintaging.

.vital_events_run <- function(n = 20000L, seed = 8L, years = 2010L:2022L) {
  tmp <- tempfile("fplida_vital_")
  dir.create(tmp)
  spine <- generate_spine(n = n, seed = seed, output_dir = tmp,
                          return_data = TRUE, use_template = FALSE)
  generate_deaths(spine = spine, seed = seed, years = years,
                  output_dir = tmp, return_data = FALSE)
  generate_mcd(spine = spine, seed = seed, output_dir = tmp,
               return_data = FALSE)
  run_dir <- getOption("fplida.run_dir")
  list(tmp = tmp, run_dir = run_dir,
       deaths_dir = file.path(run_dir, "rbdm-deaths"),
       mcd_dir = file.path(run_dir, "sa-mcd"))
}

.vital_read <- function(dir, file) {
  path <- file.path(dir, file)
  if (!file.exists(path)) return(NULL)
  as.data.frame(arrow::read_parquet(path))
}


test_that("DEATHS publishes a registration table for the years that have one", {
  skip_if_not_installed("arrow")
  td <- .vital_events_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  # The data item list gives death_registrations_2007 to _2012 and no later.
  for (year in 2010:2012) {
    registration <- .vital_read(
      td$deaths_dir, sprintf("madipge-death-d-death-registrations-%d.parquet",
                             year))
    expect_false(is.null(registration))
    expect_setequal(names(registration), fplida:::.DEATHS_REGISTRATION_VARIABLES)
  }
  expect_null(.vital_read(
    td$deaths_dir, "madipge-death-d-death-registrations-2015.parquet"))
})

test_that("a death registration carries no medical record", {
  skip_if_not_installed("arrow")
  td <- .vital_events_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  registration <- .vital_read(
    td$deaths_dir, "madipge-death-d-death-registrations-2011.parquet")
  skip_if(is.null(registration), "no 2011 registrations")
  # The underlying cause and the entity and RACS axes belong to the
  # cause-of-death table, which the real registration table does not have.
  expect_false(any(c("UCOD", "ENTITY1", "RACS1", "CERTIFIER") %in%
                     names(registration)))
  expect_true(all(c("DEATH_DATE", "PERIOD_RESIDENCE", "URES5_SA2",
                    "URES9_SA2") %in% names(registration)))
})

test_that("DEATHS geography is named by its vintage", {
  skip_if_not_installed("arrow")
  td <- .vital_events_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  early <- .vital_read(td$deaths_dir,
                       "madipge-death-d-cause-of-death-2013.parquet")
  late <- .vital_read(td$deaths_dir,
                      "madipge-death-d-cause-of-death-2022.parquet")
  skip_if(is.null(early) || is.null(late), "years not generated")

  # ASGS and SEIFA were reissued after the 2021 Census, and the list names
  # the reissued variables separately. Emitting the 2021 names in every year
  # said the 2021 boundaries applied to a 2013 death.
  expect_true("SEIFA_IRSD_DEC" %in% names(early))
  expect_false("SEIFA_IRSD_DEC_2021" %in% names(early))
  expect_true("SEIFA_IRSD_DEC_2021" %in% names(late))
  expect_true("REMOTENESS_AREA" %in% names(early))
  expect_true("REMOTENESS_AREA_2021" %in% names(late))

  # PLACE_OF_DEATH appears in the 2019 tables and nowhere else.
  expect_false("PLACE_OF_DEATH" %in% names(early))
  place <- .vital_read(td$deaths_dir,
                       "madipge-death-d-cause-of-death-2019.parquet")
  expect_true("PLACE_OF_DEATH" %in% names(place))
})

test_that("MCD publishes demographics, address and entitlements per vintage", {
  skip_if_not_installed("arrow")
  td <- .vital_events_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  for (vintage in c("0623", "0624", "0625", "1225")) {
    for (family in c("demogs", "address", "entitlements")) {
      frame <- .vital_read(td$mcd_dir, sprintf(
        "madipge-mcd-d-mcd-%s-%s.parquet", vintage, family))
      expect_false(is.null(frame), info = paste(vintage, family))
      expect_gt(nrow(frame), 0L)
    }
  }
  # Only the June 2022 vintage spans the ASGS reissue, so only it carries
  # both editions of its address table.
  expect_false(is.null(.vital_read(
    td$mcd_dir, "madipge-mcd-d-mcd-0622-address-asgs-2016.parquet")))
  expect_false(is.null(.vital_read(
    td$mcd_dir, "madipge-mcd-d-mcd-0622-address-asgs-2021.parquet")))
})

test_that("MCD families carry their published variables and spell dates", {
  skip_if_not_installed("arrow")
  td <- .vital_events_run()
  on.exit(unlink(td$tmp, recursive = TRUE), add = TRUE)

  demogs <- .vital_read(td$mcd_dir, "madipge-mcd-d-mcd-0623-demogs.parquet")
  expect_setequal(names(demogs), fplida:::.MCD_DEMOGS_VARIABLES)

  entitlements <- .vital_read(td$mcd_dir,
                              "madipge-mcd-d-mcd-0623-entitlements.parquet")
  expect_setequal(names(entitlements), fplida:::.MCD_ENTITLEMENT_VARIABLES)

  address <- .vital_read(td$mcd_dir, "madipge-mcd-d-mcd-0623-address.parquet")
  expect_setequal(names(address),
                  c("SYNTHETIC_AEUID", "ADR_TYP", "START_DATE", "END_DATE",
                    "SA1_ASGS_2021", "SA2_ASGS_2021", "SA4_ASGS_2021",
                    "STATE_ASGS_2021"))

  # A spell has a start, and an open one has no end. Every record ending on
  # the same day would make a spell model untestable.
  expect_false(any(is.na(address$START_DATE)))
  expect_gt(mean(is.na(address$END_DATE)), 0.3)
  expect_lt(mean(is.na(address$END_DATE)), 0.95)
  closed <- !is.na(address$END_DATE)
  expect_true(all(address$END_DATE[closed] >= address$START_DATE[closed]))
})
