#' Extract SA Budget Paper / MYEFO / Final Budget Outcome facts
#'
#' South Australia publishes Budget Paper No. 3 (Budget Statement) in
#' June. General Government Sector forward estimates appear in
#' Chapter 1.
#'
#' Phase 3 scaffold --- see [`sbm_extract_nsw()`] for the contract.
#'
#' @inheritParams sbm_extract_nsw
#' @keywords internal
sbm_extract_sa <- function(pdf_path, document_id, doc_type, fiscal_year, cfg) {
  anchors <- c("General Government Sector",
               "(forward estimates|operating statement)")

  tables <- sbm_extract_table_from_pdf(pdf_path, anchors = anchors)
  if (is.null(tables) || length(tables) == 0L) return(NULL)

  sbm_warn(sprintf(
    "SA rule-based parser not yet implemented for %s --- found %d candidate table(s)",
    document_id, length(tables)
  ))
  NULL
}
