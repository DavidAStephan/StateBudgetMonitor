test_that("jurisdiction_from_series detects all eight codes", {
  series <- c(
    "Net debt ; New South Wales ;",
    "Estimated Resident Population ; Persons ; Victoria ;",
    "Gross state product, Current prices ; Queensland ;",
    "GFS ; Western Australia ;",
    "GFS ; South Australia ;",
    "GFS ; Tasmania ;",
    "GFS ; Australian Capital Territory ;",
    "GFS ; Northern Territory ;",
    "Aggregate ; Australia ;"
  )
  result <- jurisdiction_from_series(series)
  expect_equal(result,
               c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT", NA))
})

test_that("ABS empty-schema helpers produce the right columns", {
  expect_setequal(
    names(empty_abs_gfs_schema()),
    c("jurisdiction", "series_id", "series", "table_no", "period",
      "value", "unit", "release_date")
  )
  expect_setequal(
    names(empty_abs_population_schema()),
    c("jurisdiction", "fiscal_year", "population", "release_date")
  )
  expect_setequal(
    names(empty_abs_state_accounts_schema()),
    c("jurisdiction", "fiscal_year", "gsp_aud_mil", "release_date")
  )
})
