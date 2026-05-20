test_that("sbm_fy_label maps July-Dec dates to start year FY", {
  expect_equal(sbm_fy_label("2024-07-01"), "2024-25")
  expect_equal(sbm_fy_label("2024-12-31"), "2024-25")
})

test_that("sbm_fy_label maps Jan-June dates to prior-year FY", {
  expect_equal(sbm_fy_label("2025-01-01"), "2024-25")
  expect_equal(sbm_fy_label("2025-06-30"), "2024-25")
  expect_equal(sbm_fy_label("2025-07-01"), "2025-26")
})

test_that("sbm_fy_to_dates round-trips against sbm_fy_label", {
  d <- sbm_fy_to_dates(c("2014-15", "2024-25", "2025-26"))
  expect_equal(d$start_date, as.Date(c("2014-07-01", "2024-07-01", "2025-07-01")))
  expect_equal(d$end_date,   as.Date(c("2015-06-30", "2025-06-30", "2026-06-30")))
  expect_equal(sbm_fy_label(d$start_date), d$fy)
  expect_equal(sbm_fy_label(d$end_date),   d$fy)
})

test_that("sbm_asof honours config or falls back to today", {
  expect_equal(sbm_asof(list(run = list(asof_date = "2024-03-15"))),
               as.Date("2024-03-15"))
  expect_equal(sbm_asof(list(run = list(asof_date = NULL))), Sys.Date())
})
