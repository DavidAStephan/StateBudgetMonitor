test_that("sbm_load_document_registry validates schema and codes", {
  reg <- sbm_load_document_registry(testthat::test_path("..", "..", "inst", "document_registry.csv"))
  expect_true(all(
    c("document_id", "jurisdiction", "doc_type", "fiscal_year",
      "release_date", "source_url", "pdf_path", "parser_version", "notes")
    %in% names(reg)
  ))
  expect_true(all(reg$jurisdiction %in% sbm_jurisdictions()$code))
  expect_true(all(reg$doc_type %in% c("budget", "myefo", "outcome")))
  expect_s3_class(reg$release_date, "Date")
})

test_that("sbm_load_document_registry rejects unknown jurisdictions", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "document_id,jurisdiction,doc_type,fiscal_year,release_date,source_url,pdf_path,parser_version,notes",
    "FAKE_2024-25_budget,XX,budget,2024-25,2024-06-18,,,v0,"
  ), tmp)
  expect_error(sbm_load_document_registry(tmp), "Unknown jurisdiction")
})

test_that("sbm_load_document_registry rejects unknown doc_type", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "document_id,jurisdiction,doc_type,fiscal_year,release_date,source_url,pdf_path,parser_version,notes",
    "NSW_2024-25_estimate,NSW,estimate,2024-25,2024-06-18,,,v0,"
  ), tmp)
  expect_error(sbm_load_document_registry(tmp), "Unknown doc_type")
})
