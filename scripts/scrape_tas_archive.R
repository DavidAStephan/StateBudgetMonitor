## Scrape TAS State Budget archives.
##
## The Treasury Tasmania SharePoint site indexes past budgets at
## /budget-and-financial-management/2026-27-tasmanian-budget/budget-papers-archive
## with one sub-page per fiscal year. Each year's sub-page lists
## Budget Paper No 1 (The Budget --- the canonical document) and
## supporting volumes.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
})

ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

fetch <- function(url) {
  tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent(ua) |>
      httr2::req_retry(max_tries = 3L) |>
      httr2::req_perform() |>
      httr2::resp_body_string(),
    error = function(e) NULL
  )
}

base <- "https://www.treasury.tas.gov.au"
arc_url <- paste0(base, "/budget-and-financial-management/2026-27-tasmanian-budget/budget-papers-archive")
arc_html <- fetch(arc_url)
if (is.null(arc_html)) stop("Could not fetch TAS archive page", call. = FALSE)

fy_pages <- unique(str_match_all(
  arc_html,
  "/budget-papers-archive/([0-9]{4}-[0-9]{2,4})-tasmanian-budget"
)[[1]][, 2])

## Filter to FY2014-15+ scope; also normalise "1999-2000" -> "1999-00"
normalise_fy <- function(fy) {
  if (nchar(fy) == 7L) sub("-(\\d{2})\\d{2}$", "-\\1", fy) else fy
}
fy_pages <- vapply(fy_pages, normalise_fy, character(1), USE.NAMES = FALSE)
fy_pages <- fy_pages[as.integer(substr(fy_pages, 1L, 4L)) >= 2014L]
fy_pages <- unique(c(fy_pages, "2025-26", "2026-27"))  # ensure current years included
cat("TAS fiscal years to crawl:", paste(fy_pages, collapse = ", "), "\n")

scraped <- purrr::map_dfr(fy_pages, function(fy) {
  Sys.sleep(0.3)
  cat("  ", fy, "...\n", sep = "")
  url <- sprintf("%s/budget-and-financial-management/2026-27-tasmanian-budget/budget-papers-archive/%s-tasmanian-budget",
                 base, fy)
  html <- fetch(url)
  if (is.null(html)) return(NULL)
  pdfs <- unique(str_match_all(html, 'href="(/Documents/[^"]+\\.pdf)"')[[1]][, 2])

  bp1 <- pdfs[grepl("Budget-Paper-No-1", pdfs, ignore.case = TRUE) &
              !grepl("Speech|Bill|Gender|Overview-Booklet|Vol", pdfs, ignore.case = TRUE)]

  rev <- pdfs[grepl("Revised-Estimates|Revised Estimates", pdfs, ignore.case = TRUE)]

  rows <- list()
  if (length(bp1) > 0L) {
    rows <- c(rows, list(tibble(
      document_id = paste0("TAS_", fy, "_budget"),
      fiscal_year = fy,
      doc_type    = "budget",
      source_url  = paste0(base, bp1[[1L]])
    )))
  }
  if (length(rev) > 0L) {
    rows <- c(rows, list(tibble(
      document_id = paste0("TAS_", fy, "_myefo"),
      fiscal_year = fy,
      doc_type    = "myefo",
      source_url  = paste0(base, rev[[1L]])
    )))
  }
  if (length(rows) == 0L) return(NULL)
  dplyr::bind_rows(rows)
})

cat(sprintf("\nScraped %d TAS PDFs:\n", nrow(scraped)))
print(scraped |> select(document_id, source_url), n = Inf)

reg <- read_csv("inst/document_registry.csv", show_col_types = FALSE)
before <- sum(!is.na(reg$source_url))
reg <- reg |>
  rows_update(
    scraped |> transmute(document_id, source_url),
    by = "document_id",
    unmatched = "ignore"
  ) |>
  mutate(
    notes = if_else(
      document_id %in% scraped$document_id,
      "Source URL scraped from treasury.tas.gov.au budget-papers-archive.",
      notes
    )
  )
after <- sum(!is.na(reg$source_url))
write_csv(reg, "inst/document_registry.csv", na = "")
cat(sprintf("\nRegistry source_url count: %d -> %d (+%d)\n",
            before, after, after - before))
