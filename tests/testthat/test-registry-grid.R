test_that("document registry covers full grid expected for Phase 1", {
  reg <- sbm_load_document_registry(
    testthat::test_path("..", "..", "inst", "document_registry.csv")
  )

  ## All eight jurisdictions present
  expect_setequal(unique(reg$jurisdiction), sbm_jurisdictions()$code)

  ## All three canonical doc_types present
  expect_setequal(unique(reg$doc_type), c("budget", "myefo", "outcome"))

  ## Coverage window: at minimum, last 5 FYs
  fy_starts <- as.integer(substr(reg$fiscal_year, 1L, 4L))
  expect_true(max(fy_starts) >= 2024L)
  expect_true(min(fy_starts) <= 2020L)

  ## No release dates in the future from the asof anchor
  expect_true(all(reg$release_date <= as.Date("2026-05-20")))

  ## document_id is unique
  expect_equal(length(unique(reg$document_id)), nrow(reg))
})

test_that("registry pdf_path follows the data/raw/<juris>/ convention", {
  reg <- sbm_load_document_registry(
    testthat::test_path("..", "..", "inst", "document_registry.csv")
  )
  expected_prefix <- sprintf("data/raw/%s/", tolower(reg$jurisdiction))
  expect_true(all(startsWith(reg$pdf_path, expected_prefix)))
  expect_true(all(grepl("\\.pdf$", reg$pdf_path)))
})

test_that("each (jurisdiction, doc_type) has at least 5 fiscal years", {
  reg <- sbm_load_document_registry(
    testthat::test_path("..", "..", "inst", "document_registry.csv")
  )
  coverage <- reg |>
    dplyr::count(jurisdiction, doc_type, name = "n_years")
  expect_true(all(coverage$n_years >= 5L),
              info = paste(capture.output(print(coverage)), collapse = "\n"))
})
