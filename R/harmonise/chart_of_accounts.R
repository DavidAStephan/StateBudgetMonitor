#' Apply harmonised chart of accounts to extracted facts (Phase 0 stub)
#'
#' In Phase 4 this joins extracted facts against the variable
#' dictionary, attaches canonical labels and GFS categories, and
#' overlays any rows from `data/manual_overrides/overrides.csv` which
#' take precedence.
#'
#' Phase 0 just attaches canonical labels by left-joining on
#' `(jurisdiction, canonical_variable_id)`, applies overrides if any,
#' and passes facts through unchanged otherwise.
#'
#' @param facts Extracted facts tibble (output of an extractor).
#' @param dictionary Variable dictionary tibble.
#' @param overrides Manual overrides tibble (may be zero-row).
#' @return Facts with canonical labels attached and overrides applied.
#' @export
sbm_apply_chart_of_accounts <- function(facts, dictionary, overrides) {

  labelled <- facts |>
    dplyr::left_join(
      dictionary |>
        dplyr::select(
          jurisdiction,
          variable_id = canonical_variable_id,
          canonical_label, category, sub_category, unit, is_flow, gfs_code
        ),
      by = c("jurisdiction", "variable_id")
    )

  ## Overrides take precedence: replace value for any matching key.
  if (nrow(overrides) > 0L) {
    labelled <- labelled |>
      dplyr::rows_update(
        overrides |>
          dplyr::select(jurisdiction, variable_id, fiscal_year, document_id,
                        value_aud_mil, notes),
        by = c("jurisdiction", "variable_id", "fiscal_year", "document_id"),
        unmatched = "ignore"
      )
  }

  labelled
}

#' Load manual overrides
#'
#' Edits the warehouse build to correct known extraction errors
#' without modifying the extracted CSVs themselves. Manual overrides
#' should be exceptional and individually documented in the `notes`
#' column.
#'
#' @param path Path to overrides CSV.
#' @return Tibble with override rows; zero rows if file is header-only.
#' @keywords internal
sbm_load_overrides <- function(path) {
  required <- c(
    "jurisdiction", "variable_id", "fiscal_year",
    "document_id", "value_aud_mil", "notes"
  )

  if (!file.exists(path)) {
    sbm_warn("overrides file not found at {.path {path}}")
    return(
      tibble::tibble(
        jurisdiction  = character(),
        variable_id   = character(),
        fiscal_year   = character(),
        document_id   = character(),
        value_aud_mil = numeric(),
        notes         = character()
      )
    )
  }

  ov <- readr::read_csv(path, show_col_types = FALSE)
  missing <- setdiff(required, names(ov))
  if (length(missing) > 0L) {
    stop(
      "overrides.csv missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  ov
}
