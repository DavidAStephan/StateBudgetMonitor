test_that("sbm_download_pdfs skips rows with missing source_url", {
  withr::local_tempdir()

  registry <- tibble::tibble(
    document_id    = c("NSW_2024-25_budget", "NSW_2024-25_myefo"),
    jurisdiction   = "NSW",
    doc_type       = c("budget", "myefo"),
    fiscal_year    = "2024-25",
    release_date   = as.Date(c("2024-06-18", "2024-12-17")),
    source_url     = c(NA_character_, NA_character_),
    pdf_path       = c("data/raw/nsw/2024-25_budget.pdf",
                       "data/raw/nsw/2024-25_myefo.pdf"),
    parser_version = "v0",
    notes          = NA_character_
  )

  cfg <- list(paths = list(raw = "data/raw"),
              run   = list(http_retries = 1L, user_agent = "test"))

  out <- sbm_download_pdfs(registry, cfg)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$download_status == "skipped_no_url"))
  expect_true(all(is.na(out$local_path)))
})

test_that("sbm_download_pdfs treats an existing non-empty cache as cached", {
  withr::local_tempdir()
  fs::dir_create("data/raw/nsw")
  writeBin(charToRaw("not really a pdf"),
           "data/raw/nsw/2024-25_budget.pdf")

  registry <- tibble::tibble(
    document_id    = "NSW_2024-25_budget",
    jurisdiction   = "NSW",
    doc_type       = "budget",
    fiscal_year    = "2024-25",
    release_date   = as.Date("2024-06-18"),
    source_url     = "https://example.invalid/never-fetched",
    pdf_path       = "data/raw/nsw/2024-25_budget.pdf",
    parser_version = "v0",
    notes          = NA_character_
  )

  cfg <- list(paths = list(raw = "data/raw"),
              run   = list(http_retries = 1L, user_agent = "test"))

  out <- sbm_download_pdfs(registry, cfg)
  expect_equal(out$download_status, "cached")
  expect_true(file.exists(out$local_path))
})
