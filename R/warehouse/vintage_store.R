#' Read latest-as-of facts from the warehouse
#'
#' For each `(jurisdiction, variable_id, fiscal_year)`, returns the
#' most recent estimate whose source document was released on or
#' before `asof_date`. This is what powers the "what was knowable as
#' at any past date" view.
#'
#' Rows whose `document_id` is missing from `dim_documents` (or whose
#' `release_date` is NULL) are still returned — vintage filtering only
#' applies where a `release_date` is available.
#'
#' @param db_path Path to the warehouse file.
#' @param asof_date A `Date`.
#' @return Tibble of facts with `canonical_label`, `category`, etc.
#'   joined in.
#' @export
sbm_facts_asof <- function(db_path, asof_date) {
  con <- sbm_warehouse_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  sql <- "
    WITH facts_with_release AS (
      SELECT
        f.*,
        d.release_date,
        d.doc_type
      FROM fiscal_facts f
      LEFT JOIN dim_documents d USING (document_id)
      WHERE d.release_date IS NULL OR d.release_date <= ?
    ),
    ranked AS (
      SELECT
        *,
        ROW_NUMBER() OVER (
          PARTITION BY jurisdiction, variable_id, fiscal_year
          ORDER BY release_date DESC NULLS LAST, document_id DESC
        ) AS rn
      FROM facts_with_release
    )
    SELECT
      r.jurisdiction, r.variable_id, r.fiscal_year, r.value_aud_mil,
      r.is_forward_estimate, r.estimate_type, r.document_id,
      r.release_date, r.doc_type,
      v.canonical_label, v.category, v.sub_category, v.unit, v.is_flow,
      v.gfs_code
    FROM ranked r
    LEFT JOIN dim_variables v ON v.variable_id = r.variable_id
    WHERE rn = 1
  "

  res <- DBI::dbGetQuery(con, sql, params = list(as.character(asof_date)))
  tibble::as_tibble(res)
}
