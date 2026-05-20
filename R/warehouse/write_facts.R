#' Write harmonised facts into the warehouse
#'
#' Upserts `fiscal_facts` on its primary key, and refreshes
#' `dim_variables` and `dim_documents` from the data on every run.
#'
#' @param db_path Path to the warehouse file.
#' @param facts Tibble of harmonised facts (output of
#'   `sbm_apply_chart_of_accounts()`).
#' @param registry Document registry tibble.
#' @return The warehouse file path, so downstream targets can depend
#'   on the populated database (not just the post-init empty one).
#' @export
sbm_write_facts <- function(db_path, facts, registry) {
  con <- sbm_warehouse_connect(db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  ## --- dim_variables -----------------------------------------------------
  dim_vars <- facts |>
    dplyr::distinct(variable_id, canonical_label, category, sub_category,
                    unit, is_flow, gfs_code) |>
    dplyr::filter(!is.na(variable_id))

  DBI::dbExecute(con, "DELETE FROM dim_variables")
  if (nrow(dim_vars) > 0L) DBI::dbAppendTable(con, "dim_variables", dim_vars)

  ## --- dim_documents -----------------------------------------------------
  ## Use the registry as the canonical document catalogue, supplemented
  ## by any document_ids that appear in facts but aren't in registry yet
  ## (e.g. illustrative Phase 0 data).
  dim_docs <- registry |>
    dplyr::transmute(
      document_id,
      jurisdiction,
      doc_type,
      fiscal_year,
      release_date,
      source_url,
      pdf_path,
      parser_version,
      ingested_at = Sys.time(),
      notes
    )

  extra_docs <- facts |>
    dplyr::distinct(document_id, jurisdiction, estimate_type, fiscal_year) |>
    dplyr::filter(!document_id %in% dim_docs$document_id) |>
    dplyr::transmute(
      document_id,
      jurisdiction,
      doc_type      = estimate_type,
      fiscal_year,
      release_date  = as.Date(NA),
      source_url    = NA_character_,
      pdf_path      = NA_character_,
      parser_version = NA_character_,
      ingested_at   = Sys.time(),
      notes         = "Synthesised --- not in registry."
    )

  dim_docs_all <- dplyr::bind_rows(dim_docs, extra_docs)

  DBI::dbExecute(con, "DELETE FROM dim_documents")
  if (nrow(dim_docs_all) > 0L) {
    DBI::dbAppendTable(con, "dim_documents", dim_docs_all)
  }

  ## --- fiscal_facts ------------------------------------------------------
  fact_rows <- facts |>
    dplyr::transmute(
      jurisdiction,
      variable_id,
      fiscal_year,
      value_aud_mil,
      is_forward_estimate,
      estimate_type,
      document_id,
      extraction_method,
      extraction_timestamp,
      notes
    )

  ## Defensive dedup: drop any duplicate (jurisdiction, variable_id,
  ## fiscal_year, document_id) rows --- the primary key. LLM-assisted
  ## extraction occasionally emits the same (variable, year) row
  ## twice; we keep the first occurrence and log the count.
  n_before <- nrow(fact_rows)
  fact_rows <- fact_rows |>
    dplyr::distinct(jurisdiction, variable_id, fiscal_year, document_id,
                    .keep_all = TRUE)
  n_dropped <- n_before - nrow(fact_rows)
  if (n_dropped > 0L) {
    sbm_warn(sprintf(
      "write_facts: dropped %d duplicate fact row(s) on primary key",
      n_dropped
    ))
  }

  ## DuckDB doesn't support ON CONFLICT in dbAppendTable, so we wipe
  ## and reload. The warehouse is small (<< 1M rows projected) and
  ## fully rebuildable from data/extracted/, so this is fine.
  DBI::dbExecute(con, "DELETE FROM fiscal_facts")
  if (nrow(fact_rows) > 0L) DBI::dbAppendTable(con, "fiscal_facts", fact_rows)

  db_path
}
