#' Load and validate the budget document registry
#'
#' The registry is a hand-curated CSV at `inst/document_registry.csv`
#' listing every known Budget Paper, MYEFO, and Final Budget Outcome
#' across all eight jurisdictions from FY2014-15 onwards. URLs change
#' over time, so the registry is the source of truth for where each
#' document currently lives.
#'
#' @param path Path to the registry CSV.
#' @return A tibble with one row per document; throws if required
#'   columns are missing or codes are unknown.
#' @export
sbm_load_document_registry <- function(path) {
  required <- c(
    "document_id", "jurisdiction", "doc_type", "fiscal_year",
    "release_date", "source_url", "pdf_path", "parser_version", "notes"
  )

  reg <- readr::read_csv(path, show_col_types = FALSE)

  missing <- setdiff(required, names(reg))
  if (length(missing) > 0L) {
    stop(
      "document_registry.csv missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  ## Type coercions
  reg <- reg |>
    dplyr::mutate(
      release_date  = suppressWarnings(as.Date(release_date)),
      fiscal_year   = as.character(fiscal_year),
      parser_version = as.character(parser_version)
    )

  ## Jurisdiction sanity
  valid_codes <- sbm_jurisdictions()$code
  bad <- setdiff(unique(reg$jurisdiction), valid_codes)
  if (length(bad) > 0L) {
    stop(
      "Unknown jurisdiction code(s) in registry: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  ## doc_type sanity
  valid_types <- c("budget", "myefo", "outcome")
  bad_types <- setdiff(unique(reg$doc_type), valid_types)
  if (length(bad_types) > 0L) {
    stop(
      "Unknown doc_type(s) in registry: ",
      paste(bad_types, collapse = ", "),
      ". Expected one of: ", paste(valid_types, collapse = ", "),
      call. = FALSE
    )
  }

  reg
}
