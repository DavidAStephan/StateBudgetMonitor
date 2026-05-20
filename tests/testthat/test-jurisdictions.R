test_that("sbm_jurisdictions covers all eight states/territories", {
  j <- sbm_jurisdictions()
  expect_setequal(
    j$code,
    c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")
  )
  expect_true(all(!is.na(j$name)))
  expect_true(all(j$fy_end_month == 6L))
  expect_true(all(j$fy_end_day   == 30L))
})
