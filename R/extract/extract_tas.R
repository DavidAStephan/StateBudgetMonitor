#' Extract TAS Budget Paper / MYEFO / Final Budget Outcome facts
#'
#' Tasmania publishes Budget Paper No. 1 (The Budget) in May or June.
#' General Government Sector forward estimates appear in chapter 4
#' (Fiscal Strategy and Outlook) and the financial statements
#' appendix.
#'
#' Phase 3 scaffold --- see [`sbm_extract_nsw()`] for the contract.
#'
#' @inheritParams sbm_extract_nsw
#' @keywords internal
sbm_extract_tas <- function(pdf_path, document_id, doc_type, fiscal_year, cfg) {
  anchors <- c("General Government",
               "(forward estimates|operating statement|fiscal aggregates)")

  tables <- sbm_extract_table_from_pdf(pdf_path, anchors = anchors)
  if (is.null(tables) || length(tables) == 0L) return(NULL)

  sbm_warn(sprintf(
    "TAS rule-based parser not yet implemented for %s --- found %d candidate table(s)",
    document_id, length(tables)
  ))
  NULL
}
