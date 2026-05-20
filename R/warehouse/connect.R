#' Open a DuckDB connection to the warehouse
#'
#' Callers are responsible for cleanup; pair every call with
#' `on.exit(DBI::dbDisconnect(con, shutdown = TRUE))`.
#'
#' @param db_path Path to the DuckDB file.
#' @param read_only Open the database read-only (`TRUE`, default) or
#'   read-write (`FALSE`).
#' @return A `DBIConnection`.
#' @export
sbm_warehouse_connect <- function(db_path, read_only = TRUE) {
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}

#' Resolve the warehouse path from config
#'
#' @param cfg Project config.
#' @return Absolute or project-relative path to the warehouse file.
#' @export
sbm_warehouse_path <- function(cfg) {
  cfg$paths$warehouse
}
