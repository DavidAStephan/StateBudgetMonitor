test_that("sbm_load_variable_dictionary loads with required columns", {
  dict <- sbm_load_variable_dictionary(testthat::test_path("..", "..", "inst", "variable_dictionary.csv"))
  expect_true(all(
    c("canonical_variable_id", "canonical_label", "gfs_code", "category",
      "sub_category", "unit", "is_flow", "jurisdiction",
      "source_line_item", "mapping_notes", "parser_version")
    %in% names(dict)
  ))
  expect_type(dict$is_flow, "logical")
})

test_that("(canonical_variable_id, jurisdiction) pairs are unique", {
  dict <- sbm_load_variable_dictionary(testthat::test_path("..", "..", "inst", "variable_dictionary.csv"))
  dup_count <- dict |>
    dplyr::count(canonical_variable_id, jurisdiction) |>
    dplyr::filter(n > 1L) |>
    nrow()
  expect_equal(dup_count, 0L)
})

test_that("sbm_load_variable_dictionary rejects duplicate keys", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "canonical_variable_id,canonical_label,gfs_code,category,sub_category,unit,is_flow,jurisdiction,source_line_item,mapping_notes,parser_version",
    "rev_total,Total revenue,GFS_REV,revenue,total,AUD_mil,TRUE,NSW,Total revenue,,v0",
    "rev_total,Total revenue,GFS_REV,revenue,total,AUD_mil,TRUE,NSW,Total revenue,,v0"
  ), tmp)
  expect_error(sbm_load_variable_dictionary(tmp), "duplicate")
})
