#' Load and validate the variable dictionary
#'
#' The dictionary lives at `inst/variable_dictionary.csv` in long
#' format: one row per `(canonical_variable_id, jurisdiction)` pairing
#' the canonical variable to the state's source line-item text. This
#' is the harmonisation contract — every line item parsed out of a
#' Budget Paper must map back to a canonical variable defined here.
#'
#' @param path Path to the dictionary CSV.
#' @return Tibble with required columns; throws on schema violations.
#' @export
sbm_load_variable_dictionary <- function(path) {
  required <- c(
    "canonical_variable_id", "canonical_label", "gfs_code",
    "category", "sub_category", "unit", "is_flow",
    "jurisdiction", "source_line_item", "mapping_notes", "parser_version"
  )

  dict <- readr::read_csv(path, show_col_types = FALSE)

  missing <- setdiff(required, names(dict))
  if (length(missing) > 0L) {
    stop(
      "variable_dictionary.csv missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  ## is_flow must coerce cleanly to logical
  dict <- dict |>
    dplyr::mutate(
      is_flow = as.logical(is_flow)
    )

  ## (canonical_variable_id, jurisdiction) must be unique
  dups <- dict |>
    dplyr::count(canonical_variable_id, jurisdiction) |>
    dplyr::filter(n > 1L)
  if (nrow(dups) > 0L) {
    stop(
      "variable_dictionary.csv has duplicate (canonical_variable_id, ",
      "jurisdiction) pairs: ",
      paste(dups$canonical_variable_id, dups$jurisdiction,
            sep = "/", collapse = "; "),
      call. = FALSE
    )
  }

  ## Jurisdictions must be known
  valid_codes <- sbm_jurisdictions()$code
  bad <- setdiff(unique(dict$jurisdiction), valid_codes)
  if (length(bad) > 0L) {
    stop(
      "Unknown jurisdiction code(s) in variable_dictionary: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  dict
}
