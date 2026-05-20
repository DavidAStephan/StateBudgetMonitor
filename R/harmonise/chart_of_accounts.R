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

  ## Defensively drop rows whose variable_id isn't in the dictionary
  ## --- those would later fail the NOT NULL constraint on
  ## dim_variables.canonical_label. The LLM occasionally invents
  ## variable_ids that aren't canonical (e.g. "rev_grants" instead of
  ## "rev_grants_tied" / "rev_grants_untied"). Log the count so the
  ## user can decide whether to extend the dictionary.
  n_orphan <- sum(is.na(labelled$canonical_label))
  if (n_orphan > 0L) {
    orphans <- labelled |>
      dplyr::filter(is.na(canonical_label)) |>
      dplyr::count(variable_id, sort = TRUE)
    sbm_warn(sprintf(
      "chart_of_accounts: dropping %d row(s) for variable_id(s) not in dictionary: %s",
      n_orphan,
      paste(sprintf("%s (%d)", orphans$variable_id, orphans$n), collapse = ", ")
    ))
    labelled <- labelled |> dplyr::filter(!is.na(canonical_label))
  }

  ## Defensive drop on NA primary-key / NOT-NULL fields the warehouse
  ## insists on. LLM responses occasionally leave is_forward_estimate
  ## blank or omit value_aud_mil for one of N years.
  n_bad <- sum(is.na(labelled$is_forward_estimate) | is.na(labelled$value_aud_mil) |
               is.na(labelled$fiscal_year) | is.na(labelled$variable_id))
  if (n_bad > 0L) {
    sbm_warn(sprintf(
      "chart_of_accounts: dropping %d row(s) with NA in required field(s)", n_bad
    ))
    labelled <- labelled |>
      dplyr::filter(!is.na(is_forward_estimate),
                    !is.na(value_aud_mil),
                    !is.na(fiscal_year),
                    !is.na(variable_id))
  }

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
