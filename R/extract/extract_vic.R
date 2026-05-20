#' Extract VIC Budget Paper / MYEFO / Final Budget Outcome facts
#'
#' Victoria publishes Budget Paper No. 5 (Statement of Finances) in
#' May. The General Government Sector forward estimates table lives
#' in BP5 Chapter 1; line items follow the AAS/GFS convention.
#'
#' Phase 2 scaffold --- see [`sbm_extract_nsw()`] for the contract.
#'
#' @inheritParams sbm_extract_nsw
#' @keywords internal
sbm_extract_vic <- function(pdf_path, document_id, doc_type, fiscal_year, cfg) {
  anchors <- c("General Government Sector",
               "(Operating statement|forward estimates|estimated financial)")

  tables <- sbm_extract_table_from_pdf(pdf_path, anchors = anchors)
  if (is.null(tables) || length(tables) == 0L) return(NULL)

  sbm_warn(sprintf(
    "VIC rule-based parser not yet implemented for %s --- found %d candidate table(s)",
    document_id, length(tables)
  ))
  NULL
}
