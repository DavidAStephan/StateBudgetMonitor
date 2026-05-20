#' Load committed wage-policy CSVs into a long panel
#'
#' Wage-policy statements live in Budget Papers as free-text plus a
#' headline annual-rise percentage. Extraction is LLM-assisted (free
#' text) plus rule-based (percentages), runs on a developer machine,
#' and is committed to `data/extracted/wage_policy/<juris>.csv`.
#'
#' Schema of each per-jurisdiction CSV:
#'
#'   * `document_id`         --- source Budget Paper / MYEFO id.
#'   * `effective_from`      --- ISO date (yyyy-mm-dd).
#'   * `effective_to`        --- ISO date or empty.
#'   * `annual_rise_pct`     --- numeric.
#'   * `coverage`            --- string ("all public sector", "nurses", etc.).
#'   * `productivity_offset` --- string (or empty).
#'   * `sign_on_bonus`       --- string (or empty).
#'   * `policy_text`         --- verbatim policy statement.
#'   * `notes`               --- free-text caveats.
#'
#' @param cfg Project config.
#' @return Long tibble of wage-policy statements.
#' @keywords internal
sbm_load_wage_policy <- function(cfg) {
  root <- file.path(cfg$paths$extracted %||% "data/extracted", "wage_policy")
  if (!fs::dir_exists(root)) return(empty_wage_policy())

  files <- fs::dir_ls(root, glob = "*.csv")
  if (length(files) == 0L) return(empty_wage_policy())

  required <- c("document_id", "effective_from", "annual_rise_pct",
                "coverage", "policy_text")

  rows <- purrr::map_dfr(files, function(f) {
    csv <- tryCatch(readr::read_csv(f, show_col_types = FALSE),
                    error = function(e) NULL)
    if (is.null(csv) || nrow(csv) == 0L) return(NULL)
    missing <- setdiff(required, names(csv))
    if (length(missing) > 0L) {
      sbm_warn(sprintf("wage_policy/%s missing required columns: %s --- skipping",
                       basename(f), paste(missing, collapse = ", ")))
      return(NULL)
    }
    juris <- toupper(tools::file_path_sans_ext(basename(f)))
    csv |>
      dplyr::mutate(
        jurisdiction         = juris,
        effective_from       = suppressWarnings(as.Date(effective_from)),
        effective_to         = if ("effective_to" %in% names(csv))
                                 suppressWarnings(as.Date(effective_to))
                               else as.Date(NA),
        annual_rise_pct      = as.numeric(annual_rise_pct),
        productivity_offset  = if ("productivity_offset" %in% names(csv))
                                 productivity_offset else NA_character_,
        sign_on_bonus        = if ("sign_on_bonus" %in% names(csv))
                                 sign_on_bonus else NA_character_,
        notes                = if ("notes" %in% names(csv))
                                 notes else NA_character_
      ) |>
      dplyr::select(jurisdiction, document_id, effective_from, effective_to,
                    annual_rise_pct, coverage, productivity_offset,
                    sign_on_bonus, policy_text, notes)
  })

  if (is.null(rows) || nrow(rows) == 0L) return(empty_wage_policy())
  rows
}

empty_wage_policy <- function() {
  tibble::tibble(
    jurisdiction        = character(),
    document_id         = character(),
    effective_from      = as.Date(character()),
    effective_to        = as.Date(character()),
    annual_rise_pct     = numeric(),
    coverage            = character(),
    productivity_offset = character(),
    sign_on_bonus       = character(),
    policy_text         = character(),
    notes               = character()
  )
}

#' Write wage-policy rows into the warehouse
#'
#' Mirrors `sbm_write_facts()` --- wipe + reload pattern (DuckDB has
#' no `ON CONFLICT`). Returns the warehouse path so downstream
#' targets depend on the populated table.
#'
#' @param db_path Path to the warehouse file.
#' @param wage_rows Tibble from `sbm_load_wage_policy()`.
#' @return The warehouse path.
#' @keywords internal
sbm_write_wage_policy <- function(db_path, wage_rows) {
  con <- sbm_warehouse_connect(db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "DELETE FROM wage_policy")
  if (nrow(wage_rows) > 0L) {
    DBI::dbAppendTable(con, "wage_policy", wage_rows)
  }
  sbm_info(sprintf("wage_policy: %d rows written to warehouse", nrow(wage_rows)))
  db_path
}
