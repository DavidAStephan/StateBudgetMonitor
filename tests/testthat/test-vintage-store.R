test_that("sbm_facts_asof returns the latest estimate by release_date", {
  skip_if_not_installed("duckdb")
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(c(tmp, paste0(tmp, ".wal"))))

  cfg <- list(paths = list(warehouse = tmp))
  sbm_warehouse_init(cfg)

  ## Hand-build a tiny world: one variable, three vintages.
  registry <- tibble::tibble(
    document_id    = c("NSW_2024-25_budget", "NSW_2024-25_myefo",
                       "NSW_2023-24_outcome"),
    jurisdiction   = "NSW",
    doc_type       = c("budget", "myefo", "outcome"),
    fiscal_year    = c("2024-25", "2024-25", "2023-24"),
    release_date   = as.Date(c("2024-06-18", "2024-12-17", "2024-10-25")),
    source_url     = NA_character_,
    pdf_path       = NA_character_,
    parser_version = "v0",
    notes          = NA_character_
  )

  facts <- tibble::tibble(
    jurisdiction         = "NSW",
    variable_id          = "net_debt",
    canonical_label      = "Net debt",
    category             = "balance_sheet",
    sub_category         = "net_debt",
    unit                 = "AUD_mil",
    is_flow              = FALSE,
    gfs_code             = "GFS_NETDEBT",
    fiscal_year          = c("2025-26", "2025-26"),
    value_aud_mil        = c(159700, 162300),
    is_forward_estimate  = TRUE,
    estimate_type        = c("budget", "myefo"),
    document_id          = c("NSW_2024-25_budget", "NSW_2024-25_myefo"),
    extraction_method    = "manual_override",
    extraction_timestamp = Sys.time(),
    notes                = NA_character_
  )

  sbm_write_facts(tmp, facts, registry)

  ## As-of after both --- should pick MYEFO (most recent).
  late <- sbm_facts_asof(tmp, as.Date("2025-01-01"))
  expect_equal(nrow(late), 1L)
  expect_equal(late$value_aud_mil, 162300)
  expect_equal(late$document_id,    "NSW_2024-25_myefo")

  ## As-of between the two --- should pick budget only.
  mid <- sbm_facts_asof(tmp, as.Date("2024-10-01"))
  expect_equal(nrow(mid), 1L)
  expect_equal(mid$value_aud_mil, 159700)
  expect_equal(mid$document_id,    "NSW_2024-25_budget")

  ## As-of before either --- no rows.
  early <- sbm_facts_asof(tmp, as.Date("2024-01-01"))
  expect_equal(nrow(early), 0L)
})
