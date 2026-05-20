#' Build the revision history panel
#'
#' Returns one row per `(jurisdiction, variable_id, fiscal_year,
#' document_id)`, with the document's release date attached. This is
#' the panel the revisions page filters down to a single variable and
#' forecast year, then plots as a vintage trail.
#'
#' @param db_path Path to the warehouse file.
#' @return Tibble of all observed estimates with release dates.
#' @keywords internal
sbm_revision_history <- function(db_path) {
  con <- sbm_warehouse_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  sql <- "
    SELECT
      f.jurisdiction, f.variable_id, f.fiscal_year, f.value_aud_mil,
      f.is_forward_estimate, f.estimate_type, f.document_id,
      d.release_date, d.doc_type,
      v.canonical_label, v.category, v.sub_category, v.unit, v.is_flow
    FROM fiscal_facts f
    LEFT JOIN dim_documents  d ON d.document_id = f.document_id
    LEFT JOIN dim_variables  v ON v.variable_id = f.variable_id
    ORDER BY f.jurisdiction, f.variable_id, f.fiscal_year, d.release_date
  "
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}
