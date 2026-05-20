#' Extract WA Budget Paper / MYEFO / Final Budget Outcome facts
#'
#' Western Australia publishes its Budget Paper in May (Budget Paper
#' No. 3, Economic and Fiscal Outlook). General Government Sector
#' forward estimates appear in Appendix 1.
#'
#' Phase 3 scaffold --- see [`sbm_extract_nsw()`] for the contract.
#'
#' @inheritParams sbm_extract_nsw
#' @keywords internal
sbm_extract_wa <- function(pdf_path, document_id, doc_type, fiscal_year, cfg) {
  anchors <- c("General Government", "(forward estimates|operating statement)")

  tables <- sbm_extract_table_from_pdf(pdf_path, anchors = anchors)
  if (is.null(tables) || length(tables) == 0L) return(NULL)

  sbm_warn(sprintf(
    "WA rule-based parser not yet implemented for %s --- found %d candidate table(s)",
    document_id, length(tables)
  ))
  NULL
}
