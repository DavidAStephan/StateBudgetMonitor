#' Wage policy panel (Phase 0 stub)
#'
#' Reads the `wage_policy` table from the warehouse and returns it as
#' a long panel for the wage-policy page. Phase 0 simply returns an
#' empty tibble of the right shape.
#'
#' @param db_path Path to the warehouse file.
#' @return Tibble of wage policy rows.
#' @keywords internal
sbm_wage_policy_panel <- function(db_path) {
  con <- sbm_warehouse_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tibble::as_tibble(DBI::dbGetQuery(con, "SELECT * FROM wage_policy"))
}
