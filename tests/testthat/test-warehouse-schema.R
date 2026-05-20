test_that("sbm_warehouse_init creates all expected tables", {
  skip_if_not_installed("duckdb")
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(c(tmp, paste0(tmp, ".wal"))))

  cfg <- list(paths = list(warehouse = tmp))
  sbm_warehouse_init(cfg)

  con <- sbm_warehouse_connect(tmp, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tables <- DBI::dbListTables(con)
  expect_true(all(
    c("fiscal_facts", "dim_variables", "dim_jurisdictions",
      "dim_documents", "wage_policy") %in% tables
  ))

  ## dim_jurisdictions seeded
  jur <- DBI::dbGetQuery(con, "SELECT * FROM dim_jurisdictions")
  expect_equal(nrow(jur), 8L)
})

test_that("sbm_warehouse_init is idempotent", {
  skip_if_not_installed("duckdb")
  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(c(tmp, paste0(tmp, ".wal"))))

  cfg <- list(paths = list(warehouse = tmp))
  sbm_warehouse_init(cfg)
  expect_no_error(sbm_warehouse_init(cfg))
})
