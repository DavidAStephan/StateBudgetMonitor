## statebudgetmonitor --- pipeline definition
##
## Each section corresponds to a stage in the dependency graph:
##   config -> ingest -> extract -> harmonise -> warehouse -> analytics
##
## Targets that hit the network are intentionally cached aggressively:
## rebuilds only fire when the upstream file hash or the as-of date changes.
##
## The Quarto site is rendered SEPARATELY (via `quarto render` in CI or
## interactively) so that iterating on the pipeline does not force a full
## site rebuild on every change.

library(targets)
library(tarchetypes)

## Load all package functions for the duration of the pipeline run.
tar_source("R")

tar_option_set(
  packages = c(
    "dplyr", "tidyr", "tibble", "purrr", "stringr", "lubridate",
    "readr", "fs", "glue", "cli", "rlang", "yaml",
    "DBI", "duckdb"
  ),
  format             = "rds",
  memory             = "transient",
  garbage_collection = TRUE,
  error              = "continue",
  workspace_on_error = TRUE
)

list(

  ## ---- Config & setup -----------------------------------------------------

  tar_target(config_file, "config.yml", format = "file"),
  tar_target(cfg,         config::get(file = config_file)),
  tar_target(asof_date,   sbm_asof(cfg)),

  ## ---- Document registry --------------------------------------------------

  tar_target(registry_file, cfg$document_registry$path, format = "file"),
  tar_target(registry,      sbm_load_document_registry(registry_file)),

  ## ---- Variable dictionary ------------------------------------------------

  tar_target(dictionary_file, cfg$variable_dictionary$path, format = "file"),
  tar_target(dictionary,      sbm_load_variable_dictionary(dictionary_file)),

  ## ---- Ingest (stub in Phase 0) -------------------------------------------
  ##
  ## In Phase 1 these become real readabs / httr2 fetches with on-disk
  ## caching. For now they emit empty tibbles so the DAG flows end-to-end.

  tar_target(abs_gfs,            sbm_ingest_abs_gfs(cfg)),
  tar_target(abs_population,     sbm_ingest_abs_population(cfg)),
  tar_target(abs_state_accounts, sbm_ingest_abs_state_accounts(cfg)),
  tar_target(downloaded_registry, sbm_download_pdfs(registry, cfg)),

  ## ---- Extract (stub in Phase 0) ------------------------------------------
  ##
  ## In Phases 2-3 these become per-jurisdiction parsers. Phase 0 emits a
  ## synthetic fiscal_facts tibble so downstream stages have something to
  ## work with.

  tar_target(extracted_facts,
             sbm_extract_all(downloaded_registry, dictionary, cfg)),

  ## ---- Manual overrides ---------------------------------------------------

  tar_target(overrides_file, cfg$paths$overrides, format = "file"),
  tar_target(overrides,      sbm_load_overrides(overrides_file)),

  ## ---- Harmonise ----------------------------------------------------------

  tar_target(
    harmonised_facts,
    sbm_apply_chart_of_accounts(extracted_facts, dictionary, overrides)
  ),

  ## ---- Warehouse ----------------------------------------------------------

  tar_target(warehouse_path, sbm_warehouse_init(cfg), format = "file"),
  tar_target(
    warehouse_populated,
    sbm_write_facts(warehouse_path, harmonised_facts, registry),
    format = "file"
  ),

  ## ---- Wage policy --------------------------------------------------------

  tar_target(wage_policy_rows, sbm_load_wage_policy(cfg)),
  tar_target(
    warehouse_wage_policy_written,
    sbm_write_wage_policy(warehouse_populated, wage_policy_rows),
    format = "file"
  ),

  ## ---- Analytics ----------------------------------------------------------

  tar_target(
    facts_latest,
    sbm_facts_asof(warehouse_populated, asof_date)
  ),
  tar_target(facts_aggregate, sbm_aggregate_states(facts_latest)),
  tar_target(facts_per_capita, sbm_per_capita(facts_latest, abs_population)),
  tar_target(facts_per_gsp,    sbm_per_gsp(facts_latest, abs_state_accounts)),
  tar_target(revisions,        sbm_revision_history(warehouse_populated)),
  tar_target(wage_panel,
             sbm_wage_policy_panel(warehouse_wage_policy_written)),

  ## ---- Validation report --------------------------------------------------

  tar_target(
    gfs_reconciliation,
    sbm_reconcile_gfs(facts_latest, abs_gfs, cfg)
  )

)
