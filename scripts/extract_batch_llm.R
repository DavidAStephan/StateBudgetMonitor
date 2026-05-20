## Batch LLM extraction across a registry filter.
##
## For each registry row matching the filter, the script:
##   1. Skips if data/extracted/<juris>/<document_id>.csv already exists.
##   2. Confirms the local PDF exists.
##   3. Auto-discovers the General Government Sector forward-estimates
##      page range by text search (`Table A.1 ... General government
##      sector operating statement` as the primary anchor, with
##      fallbacks for older NSW formats).
##   4. Calls `sbm_extract_llm()` with that page range.
##   5. Logs per-doc outcome.
##
## Usage:
##   Rscript scripts/extract_batch_llm.R [JURIS] [DOC_TYPE]
##
##   defaults to all NSW budget + myefo rows.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(targets)
})

args <- commandArgs(trailingOnly = TRUE)
filter_juris <- if (length(args) >= 1L) args[[1L]] else "NSW"
filter_type  <- if (length(args) >= 2L) args[[2L]] else NULL

if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
  stop("ANTHROPIC_API_KEY not set --- paste it into .Renviron and restart R.",
       call. = FALSE)
}

tar_source("R")
cfg <- config::get(file = "config.yml")
cfg$llm$enabled_locally <- TRUE
registry   <- sbm_load_document_registry("inst/document_registry.csv")
dictionary <- sbm_load_variable_dictionary("inst/variable_dictionary.csv")

candidates <- registry |>
  filter(jurisdiction == filter_juris) |>
  { \(d) if (!is.null(filter_type)) filter(d, doc_type == filter_type) else d }()

cat(sprintf("Batch: %d candidate documents (jurisdiction = %s, doc_type = %s)\n",
            nrow(candidates), filter_juris,
            if (is.null(filter_type)) "any" else filter_type))

## --- Auto page-range discovery ------------------------------------------

find_ggs_pages <- function(pdf_path) {
  text <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) character())
  if (length(text) == 0L) return(NULL)
  n_pages <- length(text)

  ## A page is a strong "GGS operating statement" hit if it contains:
  ##   - "General government sector operating statement" (or "Total expenses"
  ##     near "Total revenue"), AND
  ##   - a numeric forward-FY header (e.g. "2024-25  2025-26").
  ## We strongly prefer pages whose contents include ACTUAL numbers in
  ## a table layout, not just the table title in a TOC.
  has_table_title <- grepl(
    "[Gg]eneral [Gg]overnment [Ss]ector [Oo]perating [Ss]tatement|General government sector\\s+operating statement",
    text
  )
  has_revenue_row <- grepl("[Tt]otal [Rr]evenue", text) &
                     grepl("[Tt]otal [Ee]xpenses", text)
  has_fy_cols     <- grepl("\\d{4}-\\d{2}\\D+\\d{4}-\\d{2}", text)
  has_numbers     <- grepl("\\b[1-9][0-9],[0-9]{3}\\b", text)  # e.g. 12,345

  ## Score per page: anchor strength × in-table content
  table_pages <- which(has_table_title & has_revenue_row & has_fy_cols & has_numbers)
  if (length(table_pages) == 0L) {
    ## Fallback: pages with revenue+expense rows + FY headers + numbers,
    ## even without explicit table title.
    table_pages <- which(has_revenue_row & has_fy_cols & has_numbers)
  }
  if (length(table_pages) == 0L) {
    ## Last fallback: any page with GGS + FY cols (probably a TOC or
    ## summary, but better than nothing for the LLM to reason about).
    table_pages <- which(has_table_title & has_fy_cols)
  }
  if (length(table_pages) == 0L) return(NULL)

  ## Prefer the LAST cluster of table pages --- appendices live at the
  ## end. Find the largest contiguous run of table pages.
  gaps <- which(diff(table_pages) > 3L)
  if (length(gaps) > 0L) {
    last_cluster_start <- gaps[length(gaps)] + 1L
    table_pages <- table_pages[last_cluster_start:length(table_pages)]
  }
  start <- table_pages[[1L]]
  end   <- min(table_pages[length(table_pages)] + 5L, n_pages, start + 9L)
  seq(start, end)
}

## --- Loop ----------------------------------------------------------------

extracted_root <- cfg$paths$extracted %||% "data/extracted"

results <- vector("list", nrow(candidates))
for (i in seq_len(nrow(candidates))) {
  row <- candidates[i, ]
  csv_path <- file.path(extracted_root, tolower(row$jurisdiction),
                        paste0(row$document_id, ".csv"))

  cat(sprintf("\n[%d/%d] %s\n", i, nrow(candidates), row$document_id))

  if (file.exists(csv_path) && file.size(csv_path) > 0L) {
    cat("  skipping --- CSV already exists\n")
    results[[i]] <- list(document_id = row$document_id, status = "already_have")
    next
  }

  if (is.na(row$pdf_path) || !file.exists(row$pdf_path)) {
    cat("  skipping --- PDF not on disk at", row$pdf_path, "\n")
    results[[i]] <- list(document_id = row$document_id, status = "no_pdf")
    next
  }

  pages <- find_ggs_pages(row$pdf_path)
  if (is.null(pages)) {
    cat("  skipping --- could not locate GGS table pages\n")
    results[[i]] <- list(document_id = row$document_id, status = "anchor_not_found")
    next
  }
  cat(sprintf("  GGS pages: %d-%d (n=%d)\n",
              min(pages), max(pages), length(pages)))

  out <- tryCatch(
    sbm_extract_llm(row$document_id, registry, dictionary, cfg, pages = pages),
    error = function(e) {
      cat("  ERROR:", conditionMessage(e), "\n")
      NULL
    }
  )
  results[[i]] <- list(
    document_id = row$document_id,
    status      = if (is.null(out)) "llm_error" else "extracted"
  )

  ## brief pause to be polite to the API
  Sys.sleep(1L)
}

## --- Summary -------------------------------------------------------------

cat("\n=== Batch summary ===\n")
summary_df <- dplyr::bind_rows(results)
print(summary_df |> dplyr::count(status, sort = TRUE))

cat("\nDone. Run `targets::tar_make()` to flow the new CSVs into the warehouse.\n")
