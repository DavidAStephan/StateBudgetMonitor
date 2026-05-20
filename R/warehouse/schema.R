#' Initialise the DuckDB warehouse schema
#'
#' Creates the warehouse file and all tables/indexes if they do not
#' already exist. Idempotent — safe to call on every pipeline run.
#'
#' Schema:
#'
#' * `fiscal_facts`       — every fiscal observation, vintage-aware.
#' * `dim_variables`      — canonical variable catalogue.
#' * `dim_jurisdictions`  — eight states/territories with metadata.
#' * `dim_documents`      — every ingested Budget/MYEFO/Outcome.
#' * `wage_policy`        — extracted wage policy statements.
#'
#' Vintage discipline: every fact carries `document_id`, which joins to
#' `dim_documents.release_date`. Filtering on that date reconstructs
#' "what was knowable at any past as-of date".
#'
#' @param cfg Project config.
#' @return The warehouse file path (for `targets` `format = "file"`).
#' @export
sbm_warehouse_init <- function(cfg) {
  db_path <- sbm_warehouse_path(cfg)
  fs::dir_create(fs::path_dir(db_path))

  con <- sbm_warehouse_connect(db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS fiscal_facts (
      jurisdiction          TEXT    NOT NULL,
      variable_id           TEXT    NOT NULL,
      fiscal_year           TEXT    NOT NULL,
      value_aud_mil         DOUBLE  NOT NULL,
      is_forward_estimate   BOOLEAN NOT NULL,
      estimate_type         TEXT    NOT NULL,
      document_id           TEXT    NOT NULL,
      extraction_method     TEXT,
      extraction_timestamp  TIMESTAMP,
      notes                 TEXT,
      PRIMARY KEY (jurisdiction, variable_id, fiscal_year, document_id)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dim_variables (
      variable_id      TEXT PRIMARY KEY,
      canonical_label  TEXT NOT NULL,
      category         TEXT,
      sub_category     TEXT,
      unit             TEXT,
      is_flow          BOOLEAN,
      gfs_code         TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dim_jurisdictions (
      code                  TEXT PRIMARY KEY,
      name                  TEXT NOT NULL,
      fy_end_month          INTEGER NOT NULL,
      fy_end_day            INTEGER NOT NULL,
      budget_release_month  INTEGER
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dim_documents (
      document_id     TEXT PRIMARY KEY,
      jurisdiction    TEXT NOT NULL,
      doc_type        TEXT NOT NULL,
      fiscal_year     TEXT NOT NULL,
      release_date    DATE NOT NULL,
      source_url      TEXT,
      pdf_path        TEXT,
      parser_version  TEXT,
      ingested_at     TIMESTAMP,
      notes           TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS wage_policy (
      jurisdiction         TEXT NOT NULL,
      document_id          TEXT NOT NULL,
      effective_from       DATE,
      effective_to         DATE,
      annual_rise_pct      DOUBLE,
      coverage             TEXT,
      productivity_offset  TEXT,
      sign_on_bonus        TEXT,
      policy_text          TEXT,
      notes                TEXT,
      PRIMARY KEY (jurisdiction, document_id, effective_from)
    )
  ")

  ## Helpful indexes for the dashboards.
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_facts_juris_var
       ON fiscal_facts(jurisdiction, variable_id)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_facts_doc
       ON fiscal_facts(document_id)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_docs_release
       ON dim_documents(release_date)")

  ## Seed the static jurisdiction dimension on every init.
  DBI::dbExecute(con, "DELETE FROM dim_jurisdictions")
  DBI::dbAppendTable(con, "dim_jurisdictions", sbm_jurisdictions())

  db_path
}
