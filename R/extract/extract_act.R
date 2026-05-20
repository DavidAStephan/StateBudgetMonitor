#' Extract ACT Budget Paper / MYEFO / Final Budget Outcome facts
#'
#' Australian Capital Territory publishes Budget Paper No. 3 (Budget
#' Statements) in March. General Government Sector forward estimates
#' appear in the Treasury chapter.
#'
#' Phase 3 scaffold --- see [`sbm_extract_nsw()`] for the contract.
#'
#' @inheritParams sbm_extract_nsw
#' @keywords internal
sbm_extract_act <- function(pdf_path, document_id, doc_type, fiscal_year, cfg) {
  anchors <- c("General Government",
               "(forward estimates|operating statement|estimated financial)")

  tables <- sbm_extract_table_from_pdf(pdf_path, anchors = anchors)
  if (is.null(tables) || length(tables) == 0L) return(NULL)

  sbm_warn(sprintf(
    "ACT rule-based parser not yet implemented for %s --- found %d candidate table(s)",
    document_id, length(tables)
  ))
  NULL
}
