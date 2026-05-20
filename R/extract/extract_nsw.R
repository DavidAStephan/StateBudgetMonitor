#' Extract NSW Budget Paper / MYEFO / Final Budget Outcome facts
#'
#' New South Wales publishes Budget Paper No. 1 (Budget Statement) in
#' June. The General Government Sector forward estimates table
#' typically lives in Appendix A of BP1, formatted consistently
#' year-over-year since FY2017-18.
#'
#' This parser is a Phase 2 scaffold: it knows where to look but does
#' not yet perform line-item mapping. Once committed CSVs land under
#' `data/extracted/nsw/`, they take precedence and this parser is not
#' invoked. Until then, this returns NULL so the framework moves on.
#'
#' @param pdf_path Path to the local PDF.
#' @param document_id Canonical document id.
#' @param doc_type One of "budget", "myefo", "outcome".
#' @param fiscal_year FY label (e.g. "2024-25").
#' @param cfg Project config.
#' @return Tibble in `fiscal_facts` shape, or NULL if extraction not
#'   yet wired up.
#' @keywords internal
sbm_extract_nsw <- function(pdf_path, document_id, doc_type, fiscal_year, cfg) {
  anchors <- switch(doc_type,
    budget  = c("General Government Sector", "Forward [Ee]stimates"),
    myefo   = c("General Government Sector", "(Revised|Updated) estimates"),
    outcome = c("General Government Sector", "(Actual|Outcome)")
  )

  tables <- sbm_extract_table_from_pdf(pdf_path, anchors = anchors)
  if (is.null(tables) || length(tables) == 0L) return(NULL)

  ## TODO Phase 2: implement line-item mapping. Until then, leave the
  ## raw tabulapdf output to surface in logs and rely on committed
  ## CSVs under data/extracted/nsw/ for production data.
  sbm_warn(sprintf(
    "NSW rule-based parser not yet implemented for %s --- found %d candidate table(s)",
    document_id, length(tables)
  ))
  NULL
}
