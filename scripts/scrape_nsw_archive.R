## Scrape https://www.budget.nsw.gov.au/budget-archives for Budget
## Paper No 1 (Budget Statement) and Half-Yearly Review PDFs covering
## FY2014-15 onwards. Updates `inst/document_registry.csv` source_url
## column in-place. Run via:
##
##   Rscript scripts/scrape_nsw_archive.R
##
## After it runs, `targets::tar_make()` will download whatever PDFs
## aren't already on disk via the standard sbm_download_pdfs() path.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
})

archive_url <- "https://www.budget.nsw.gov.au/budget-archives"
html <- httr2::request(archive_url) |>
  httr2::req_user_agent("statebudgetmonitor (+https://github.com/DavidAStephan/StateBudgetMonitor)") |>
  httr2::req_perform() |>
  httr2::resp_body_string()

## Find every href ending in .pdf
hrefs <- unique(stringr::str_match_all(html, 'href="([^"]+\\.pdf)"')[[1]][, 2])
abs_urls <- ifelse(startsWith(hrefs, "/"),
                   paste0("https://www.budget.nsw.gov.au", hrefs),
                   hrefs)

## Extract fiscal_year from the FILENAME (not the storage-path
## prefix, which has its own YYYY-MM token). NSW filenames embed
## the FY in the form e.g. "2024-25" or "Budget-2024-25". We also
## verify the second pair is consistent with `start_year + 1`
## modulo 100.
fy_candidates <- function(filename) {
  hits <- stringr::str_match_all(filename, "(19|20)(\\d{2})-(\\d{2})")[[1]]
  if (nrow(hits) == 0L) return(NA_character_)
  ## keep only candidates where the second pair equals
  ## (start_year + 1) %% 100
  starts <- as.integer(paste0(hits[, 2L], hits[, 3L]))
  ends   <- as.integer(hits[, 4L])
  ok     <- (starts + 1L) %% 100L == ends
  if (!any(ok)) return(NA_character_)
  sprintf("%d-%02d", starts[ok][1L], ends[ok][1L])
}

fy <- vapply(basename(abs_urls), fy_candidates, character(1), USE.NAMES = FALSE)

## Classify each URL. Keep only canonical Budget-Statement and
## Half-Yearly-Review files; skip factsheets, speeches, gender /
## regional / western-sydney sidecar volumes.
classify <- function(url) {
  url_lower <- tolower(url)
  if (grepl("fact.?sheet", url_lower)) return(NA_character_)
  if (grepl("speech", url_lower))      return(NA_character_)
  if (grepl("gender|western-sydney|regional|social-justice|appropriation|infrastructure|agency", url_lower)) return(NA_character_)
  ## Budget Statement patterns
  if (grepl("budget.paper.no.?1.?budget.statement|budget.paper.1.?budget.statement|budget.paper.no\\.1.?budget.statement|budget.paper.no.?2.?budget.statement", url_lower)) {
    return("budget")
  }
  ## Half-Yearly Review (NSW name for MYEFO)
  if (grepl("half.yearly", url_lower)) return("myefo")
  NA_character_
}

scraped <- tibble(url = abs_urls, fiscal_year = fy) |>
  mutate(doc_type = vapply(url, classify, character(1))) |>
  filter(!is.na(doc_type), !is.na(fiscal_year)) |>
  ## Scope: FY2014-15 onwards
  mutate(fy_start = as.integer(substr(fiscal_year, 1L, 4L))) |>
  filter(fy_start >= 2014L) |>
  ## Dedupe (some pages link the same URL twice)
  distinct(fiscal_year, doc_type, .keep_all = TRUE) |>
  mutate(document_id = sprintf("NSW_%s_%s", fiscal_year, doc_type)) |>
  select(document_id, jurisdiction = doc_type, fiscal_year, doc_type, url)

cat("Scraped", nrow(scraped), "in-scope PDFs:\n")
print(scraped |> select(document_id, url), n = Inf)

## --- Update the registry -----------------------------------------------
reg <- read_csv("inst/document_registry.csv", show_col_types = FALSE)
before_urls <- sum(!is.na(reg$source_url))

reg <- reg |>
  rows_update(
    scraped |> transmute(document_id, source_url = url),
    by = "document_id",
    unmatched = "ignore"
  ) |>
  ## Also overwrite the "URL TBD" / placeholder notes for rows we
  ## just populated.
  mutate(
    notes = if_else(
      document_id %in% scraped$document_id,
      "Source URL scraped from budget.nsw.gov.au/budget-archives.",
      notes
    )
  )

after_urls <- sum(!is.na(reg$source_url))
write_csv(reg, "inst/document_registry.csv", na = "")
cat(sprintf("\nRegistry source_url count: %d -> %d (+%d)\n",
            before_urls, after_urls, after_urls - before_urls))
