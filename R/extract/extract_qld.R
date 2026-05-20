#' Extract QLD Budget Paper / MYEFO / Final Budget Outcome facts
#'
#' Queensland publishes Budget Paper No. 2 (Budget Strategy and
#' Outlook) in June. The General Government Sector forward estimates
#' table lives in BP2 Appendix B.
#'
#' Phase 2 scaffold --- see [`sbm_extract_nsw()`] for the contract.
#'
#' @inheritParams sbm_extract_nsw
#' @keywords internal
sbm_extract_qld <- function(pdf_path, document_id, doc_type, fiscal_year, cfg) {
  anchors <- c("General Government Sector",
               "(Operating statement|estimated financial)")

  tables <- sbm_extract_table_from_pdf(pdf_path, anchors = anchors)
  if (is.null(tables) || length(tables) == 0L) return(NULL)

  sbm_warn(sprintf(
    "QLD rule-based parser not yet implemented for %s --- found %d candidate table(s)",
    document_id, length(tables)
  ))
  NULL
}
