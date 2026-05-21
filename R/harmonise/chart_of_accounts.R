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

  ## Normalise fiscal_year before joining --- the LLM occasionally
  ## extracts balance-sheet column headers like "30 June 2019" or
  ## just "2019" as fiscal_year, when they should be the canonical
  ## "YYYY-YY" FY label.
  facts <- facts |>
    dplyr::mutate(fiscal_year = normalise_fiscal_year(fiscal_year))

  ## Recompute is_forward_estimate from the document's own FY rather
  ## than trusting the LLM's flag --- a row whose fiscal_year starts
  ## strictly AFTER the document's fiscal_year is a forward estimate.
  ## The LLM gets the budget-year row right but flips the flag on
  ## actuals reported in the same budget paper for prior years.
  facts <- facts |>
    dplyr::mutate(
      .doc_fy_start = as.integer(stringr::str_sub(estimate_type_fy(document_id), 1L, 4L)),
      .row_fy_start = as.integer(stringr::str_sub(fiscal_year, 1L, 4L)),
      is_forward_estimate = .row_fy_start > .doc_fy_start
    ) |>
    dplyr::select(-.doc_fy_start, -.row_fy_start)

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

#' Normalise miscellaneous fiscal-year encodings to "YYYY-YY"
#'
#' The LLM occasionally emits one of several non-canonical forms:
#'
#'   * `"YYYY-YY"` --- already canonical, pass through.
#'   * `"YYYY-MM-DD"` --- balance-sheet date. End-of-FY date (30 June)
#'     belongs to FY (YYYY-1, YYYY); other dates belong to whichever
#'     FY contains them.
#'   * `"YYYY"` --- typically a calendar-year column header from a
#'     balance sheet (i.e. "as at 30 June 2018"). Map to the FY that
#'     ENDS in that year: `"2018"` -> `"2017-18"`.
#'   * anything else --- return NA, which downstream filter drops.
#'
#' @param x Character vector.
#' @return Character vector of FY labels (or NA where unparseable).
#' @keywords internal
normalise_fiscal_year <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  ok_fy <- grepl("^[12][0-9]{3}-[0-9]{2}$", x)
  out[ok_fy] <- x[ok_fy]

  is_date <- grepl("^[12][0-9]{3}-[0-9]{2}-[0-9]{2}$", x)
  if (any(is_date)) {
    d <- suppressWarnings(as.Date(x[is_date]))
    y <- as.integer(format(d, "%Y"))
    m <- as.integer(format(d, "%m"))
    fy_start <- ifelse(m >= 7L, y, y - 1L)
    out[is_date] <- sprintf("%d-%02d", fy_start, (fy_start + 1L) %% 100L)
  }

  is_year <- grepl("^[12][0-9]{3}$", x)
  if (any(is_year)) {
    y <- as.integer(x[is_year])
    ## Treat bare "YYYY" as the FY that ENDS in that calendar year ---
    ## the LLM typically extracts these from "as at 30 June YYYY"
    ## headers on balance sheets.
    out[is_year] <- sprintf("%d-%02d", y - 1L, y %% 100L)
  }

  out
}

#' Extract the fiscal year from a canonical document_id
#'
#' Document ids follow `<JURIS>_<YYYY-YY>_<doctype>`. Returns the
#' `YYYY-YY` token; NA if it can't be parsed.
#'
#' @keywords internal
estimate_type_fy <- function(document_id) {
  stringr::str_extract(document_id, "[12][0-9]{3}-[0-9]{2}")
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
