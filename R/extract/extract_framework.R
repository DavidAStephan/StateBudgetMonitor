#' Run the per-jurisdiction extraction layer
#'
#' Dispatches to the appropriate `sbm_extract_<juris>()` function for
#' each row in the registry, then concatenates the results into a
#' single long fact tibble.
#'
#' Each per-state extractor returns a tibble in the canonical
#' `fiscal_facts` shape; rows whose document hasn't been downloaded
#' (or whose parser hasn't been implemented yet) are skipped with a
#' warning rather than aborting the pipeline.
#'
#' Extraction order of precedence per document:
#'
#'   1. **committed CSV** under `data/extracted/<juris>/<document_id>.csv`
#'      --- always wins. This is where LLM-assisted output lives and
#'      where the production pipeline reads from.
#'   2. **rule-based parse** of the cached PDF via `tabulapdf`. Runs
#'      only if (a) the CSV is missing and (b) the parser exists for
#'      this jurisdiction and (c) the PDF is on disk.
#'   3. **no data** --- skip the row and log it. Surfaces in STATUS.md
#'      via the build log.
#'
#' The production CI run never invokes an LLM; the LLM-assisted
#' backfill is a separate dev-machine script (see
#' `R/extract/extract_llm.R`).
#'
#' @param downloaded_registry Registry with `download_status` /
#'   `local_path` columns attached.
#' @param dictionary Variable dictionary tibble.
#' @param cfg Project config.
#' @return Long tibble in `fiscal_facts` shape.
#' @export
sbm_extract_all <- function(downloaded_registry, dictionary, cfg) {
  extracted_root <- cfg$paths$extracted %||% "data/extracted"
  fs::dir_create(extracted_root)

  per_doc <- purrr::pmap(
    downloaded_registry |>
      dplyr::select(document_id, jurisdiction, doc_type, fiscal_year,
                    release_date, source_url, local_path,
                    download_status, parser_version),
    function(document_id, jurisdiction, doc_type, fiscal_year,
             release_date, source_url, local_path,
             download_status, parser_version) {

      csv_path <- file.path(extracted_root, tolower(jurisdiction),
                            paste0(document_id, ".csv"))

      ## 1. Committed CSV wins.
      if (file.exists(csv_path) && file.size(csv_path) > 0L) {
        rows <- sbm_read_extracted_csv(csv_path)
        if (nrow(rows) == 0L) return(NULL)
        return(
          rows |>
            dplyr::mutate(
              jurisdiction         = jurisdiction,
              fiscal_year          = fiscal_year,
              document_id          = document_id,
              estimate_type        = doc_type,
              extraction_method    = "csv_committed",
              extraction_timestamp = Sys.time()
            )
        )
      }

      ## 2. Rule-based parse, if we have a PDF and a parser.
      parser <- sbm_resolve_parser(jurisdiction)
      if (!is.null(parser) && !is.na(local_path) && file.exists(local_path)) {
        rows <- tryCatch(
          parser(pdf_path = local_path,
                 document_id = document_id,
                 doc_type    = doc_type,
                 fiscal_year = fiscal_year,
                 cfg         = cfg),
          error = function(e) {
            sbm_warn(sprintf("rule-based parse failed for %s: %s",
                             document_id, conditionMessage(e)))
            NULL
          }
        )
        if (!is.null(rows) && nrow(rows) > 0L) {
          return(
            rows |>
              dplyr::mutate(
                extraction_method    = "rule_based",
                extraction_timestamp = Sys.time()
              )
          )
        }
      }

      ## 3. No data --- skip.
      NULL
    }
  )

  out <- dplyr::bind_rows(per_doc)
  sbm_info(sprintf(
    "extract: %d facts across %d documents (%d via committed CSV, %d via rule-based)",
    nrow(out),
    dplyr::n_distinct(out$document_id),
    sum(out$extraction_method == "csv_committed", na.rm = TRUE),
    sum(out$extraction_method == "rule_based",    na.rm = TRUE)
  ))
  out
}

#' Resolve the rule-based parser function for a jurisdiction
#'
#' Looks up `sbm_extract_<lowercase code>` in the package namespace,
#' returning NULL if no parser is registered yet. Each per-state
#' parser is its own file under `R/extract/`; this just dispatches.
#'
#' @keywords internal
sbm_resolve_parser <- function(jurisdiction) {
  fn_name <- paste0("sbm_extract_", tolower(jurisdiction))
  if (!exists(fn_name, mode = "function")) return(NULL)
  get(fn_name, mode = "function")
}

#' Read a committed extracted CSV and validate its schema
#'
#' The CSV schema is the contract between extraction (rule-based or
#' LLM-assisted, both committing CSVs under `data/extracted/`) and
#' the rest of the pipeline. Required columns:
#'
#'   * `variable_id`         --- canonical id from the variable dictionary.
#'   * `value_aud_mil`       --- the value in AUD millions.
#'   * `is_forward_estimate` --- TRUE for forward years, FALSE for actuals.
#'
#' Anything else in the CSV is preserved as additional columns.
#'
#' @keywords internal
sbm_read_extracted_csv <- function(path) {
  rows <- readr::read_csv(path, show_col_types = FALSE)

  required <- c("variable_id", "value_aud_mil", "is_forward_estimate")
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0L) {
    sbm_warn(sprintf(
      "extracted CSV %s missing required columns: %s --- skipping",
      path, paste(missing, collapse = ", ")
    ))
    return(tibble::tibble(
      variable_id         = character(),
      value_aud_mil       = numeric(),
      is_forward_estimate = logical()
    ))
  }

  rows |>
    dplyr::mutate(
      value_aud_mil       = as.numeric(value_aud_mil),
      is_forward_estimate = as.logical(is_forward_estimate)
    )
}

#' Generic tabulapdf-based table extractor
#'
#' Locates a target table in a Budget Paper PDF and returns it as a
#' tibble. The target is identified by a page-number hint plus a
#' set of regex anchors that should appear on the page (e.g.
#' `"General Government Sector"` and `"Forward estimates"`).
#'
#' This is intended as a building block for the per-state parsers,
#' not a complete extractor: each state still needs to know which
#' columns to keep and how to map line items to canonical variables.
#'
#' Requires `tabulapdf` (Suggests, JDK-backed). Returns NULL if
#' `tabulapdf` is not installed or if no candidate page is found.
#'
#' @param pdf_path Path to a local PDF.
#' @param anchors Character vector of regex patterns; pages matching
#'   ALL anchors are candidates.
#' @param page_hint Optional integer; if provided, restrict the search
#'   to a window around this page (+/- 3 pages).
#' @return A list of tibbles (one per candidate page), or NULL if
#'   none found or `tabulapdf` is missing.
#' @keywords internal
sbm_extract_table_from_pdf <- function(pdf_path, anchors, page_hint = NULL) {
  if (!requireNamespace("tabulapdf", quietly = TRUE)) {
    sbm_warn("tabulapdf not installed --- skipping rule-based extraction")
    return(NULL)
  }
  if (!file.exists(pdf_path)) {
    sbm_warn(sprintf("PDF not found: %s", pdf_path))
    return(NULL)
  }

  n_pages  <- tabulapdf::get_n_pages(pdf_path)
  page_seq <- if (is.null(page_hint)) {
    seq_len(n_pages)
  } else {
    seq(max(1L, page_hint - 3L), min(n_pages, page_hint + 3L))
  }

  candidate_pages <- purrr::keep(page_seq, function(p) {
    text <- tryCatch(tabulapdf::extract_text(pdf_path, pages = p),
                     error = function(e) "")
    all(purrr::map_lgl(anchors, ~ grepl(.x, text, ignore.case = TRUE)))
  })

  if (length(candidate_pages) == 0L) return(NULL)

  purrr::map(candidate_pages, function(p) {
    tabulapdf::extract_tables(pdf_path, pages = p, output = "tibble") |>
      purrr::pluck(1L, .default = NULL)
  })
}
