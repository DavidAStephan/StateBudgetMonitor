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
## When the anchor finder picks unhelpful pages (e.g. narrative
## mentioning "Net debt" but not the operating-statement table),
## passing `--whole-pdf` as any later arg forces chunking of the
## entire document instead of consulting the finder.
force_chunk  <- any(args == "--whole-pdf")
positional   <- args[!grepl("^--", args)]
filter_juris <- if (length(positional) >= 1L) positional[[1L]] else "NSW"
filter_type  <- if (length(positional) >= 2L) positional[[2L]] else NULL

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

  ## Anchors --- any state's terminology. NSW uses "General government
  ## sector operating statement"; VIC uses "Estimated comprehensive
  ## operating statement" inside "Estimated financial statements for
  ## the general government sector"; QLD uses "General Government
  ## operating statement" in BP2.
  ## Match the canonical table-title language. The structure varies:
  ##   - NSW: "General government sector operating statement"
  ##   - VIC: "Estimated general government sector comprehensive
  ##           operating statement"
  ##   - QLD: "General Government operating statement"
  ## Catch all of these with a flexible "general government" +
  ## "operating statement" pattern that tolerates intervening words.
  has_table_title <- grepl(
    paste0(
      "[Gg]eneral [Gg]overnment[ \\w]{0,40}[Oo]perating [Ss]tatement|",
      "[Ee]stimated [Ff]inancial [Ss]tatements"
    ),
    text
  )
  has_revenue_row <- grepl("[Tt]otal [Rr]evenue", text) &
                     grepl("[Tt]otal [Ee]xpenses", text)
  ## FY token uses either ASCII hyphen-minus or unicode en-dash;
  ## different states publish inconsistently.
  fy_token   <- "\\d{4}[-–]\\d{2}"
  has_fy_cols <- grepl(paste0(fy_token, "\\D+", fy_token), text)
  ## Match either comma-separated (12,345 / NSW + QLD) or
  ## space-separated thousands (12 345 / VIC).
  has_numbers     <- grepl("\\b[1-9][0-9](?:,|\\s)[0-9]{3}\\b", text)

  ## 4-of-4 hit: explicit op-statement language + Revenue/Expenses row
  ## + FY column header + actual numbers.
  table_pages <- which(has_table_title & has_revenue_row & has_fy_cols & has_numbers)

  ## 3-of-4 fallback: drop the explicit-title requirement.
  if (length(table_pages) == 0L) {
    table_pages <- which(has_revenue_row & has_fy_cols & has_numbers)
  }
  ## 2-of-4 last-ditch: title + FY columns (catches TOCs near tables).
  if (length(table_pages) == 0L) {
    table_pages <- which(has_table_title & has_fy_cols)
  }
  if (length(table_pages) == 0L) return(NULL)

  ## Prefer the FIRST contiguous cluster of table pages. In Statement
  ## of Finances documents, chapter 1 / appendix A is the General
  ## Government Sector; later clusters are typically Public
  ## Non-Financial Corporations (PNFC) or Non-financial Public Sector
  ## --- out of scope for this project. NSW Budget Papers put the GGS
  ## appendix near the end, but there's still only one GGS cluster.
  gaps <- which(diff(table_pages) > 3L)
  if (length(gaps) > 0L) {
    first_cluster_end <- gaps[[1L]]
    table_pages <- table_pages[seq_len(first_cluster_end)]
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

  pages <- if (force_chunk) NULL else find_ggs_pages(row$pdf_path)
  if (is.null(pages)) {
    ## Anchor finder failed --- fall back to chunking the whole PDF.
    ## Anthropic caps PDF input at 100 pages, so we slice into 90-page
    ## chunks and let the LLM find the GGS tables wherever they live.
    ## `pdf_length()` uses qpdf which fails on some encrypted PDFs;
    ## `pdf_text()` uses poppler and is more forgiving.
    n <- tryCatch(length(pdftools::pdf_text(row$pdf_path)),
                  error = function(e) 0L)
    if (n == 0L) {
      cat("  skipping --- unreadable PDF\n")
      results[[i]] <- list(document_id = row$document_id, status = "anchor_not_found")
      next
    }
    ## Text mode (via pdftools::pdf_text) uses ~500-1500 tokens per
    ## page, so 60-page slices stay comfortably under Sonnet's 200K
    ## context window even for the most text-dense Budget Papers.
    chunk_size <- 60L
    starts <- seq.int(1L, n, by = chunk_size)
    pages  <- lapply(starts, function(s) seq.int(s, min(s + chunk_size - 1L, n)))
    cat(sprintf("  anchor not found --- chunking whole PDF: %d pages in %d slice(s)\n",
                n, length(pages)))
  } else {
    cat(sprintf("  GGS pages: %d-%d (n=%d)\n",
                min(pages), max(pages), length(pages)))
  }

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
