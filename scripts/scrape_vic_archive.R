## Scrape https://www.dtf.vic.gov.au/previous-budgets for VIC State
## Budget and Budget Update PDFs covering FY2014-15 onwards. Each
## fiscal year has its own sub-page; the Statement of Finances PDF
## is linked from `/<fy>-statement-finances` (Budget Paper No 5),
## the mid-year update from `/<fy>-budget-update`.
##
## Updates inst/document_registry.csv source_url in-place.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
})

ua <- "statebudgetmonitor (+https://github.com/DavidAStephan/StateBudgetMonitor)"

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

## ---- 1. Get list of fiscal years from previous-budgets -----------------

prev_html <- fetch("https://www.dtf.vic.gov.au/previous-budgets")
if (is.null(prev_html)) stop("Could not fetch previous-budgets page", call. = FALSE)

fy_pages <- unique(
  str_match_all(prev_html,
                '/([0-9]{4}-[0-9]{2})-state-budget"')[[1]][, 2]
)

## Filter to FY2014-15+ scope
fy_pages <- fy_pages[as.integer(substr(fy_pages, 1L, 4L)) >= 2014L]
cat("Fiscal years to crawl:", paste(fy_pages, collapse = ", "), "\n")

## ---- 2. Crawl each year's statement-finances + budget-update pages -----

find_pdf <- function(html, must_match = NULL, exclude = NULL) {
  if (is.null(html)) return(NA_character_)
  abs_pdfs  <- unique(str_match_all(html, 'href="(https?://[^"]+\\.pdf)"')[[1]][, 2])
  rel_pdfs  <- unique(str_match_all(html, 'href="(/[^"]+\\.pdf)"')[[1]][, 2])
  pdfs <- c(abs_pdfs, paste0("https://www.dtf.vic.gov.au", rel_pdfs))
  pdfs <- unique(pdfs)
  if (length(pdfs) == 0L) return(NA_character_)
  if (!is.null(must_match)) {
    keep <- grepl(must_match, pdfs, ignore.case = TRUE)
    pdfs <- pdfs[keep]
  }
  if (!is.null(exclude)) {
    drop <- grepl(exclude, pdfs, ignore.case = TRUE)
    pdfs <- pdfs[!drop]
  }
  if (length(pdfs) == 0L) return(NA_character_)
  pdfs[[1L]]
}

scraped <- purrr::map_dfr(fy_pages, function(fy) {
  Sys.sleep(0.4)
  cat("  ", fy, "...\n", sep = "")
  ## Main FY page hosts the canonical Statement of Finances link
  ## for older years; modern years also have a dedicated sub-page.
  main_html <- fetch(sprintf("https://www.dtf.vic.gov.au/%s-state-budget", fy))
  sof_html  <- fetch(sprintf("https://www.dtf.vic.gov.au/%s-statement-finances", fy))
  bup_html  <- fetch(sprintf("https://www.dtf.vic.gov.au/%s-budget-update",      fy))

  ## Statement of Finances --- prefer dedicated sub-page if it has
  ## a PDF; else look on the main FY page for "Statement of Finances".
  sof_url <- find_pdf(sof_html, must_match = "statement.of.finances|paper.no.5")
  if (is.na(sof_url)) {
    sof_url <- find_pdf(main_html,
                        must_match = "statement.of.finances|paper.no.5",
                        exclude    = "gender|overview|appropriation|infrastructure|regional|rural")
  }

  ## Budget Update / Mid-Year Update
  bup_url <- find_pdf(bup_html, must_match = "budget.update|mid.year")
  if (is.na(bup_url)) {
    bup_url <- find_pdf(main_html,
                        must_match = "budget.update|mid.year",
                        exclude    = "factsheet")
  }

  tibble(
    document_id = c(paste0("VIC_", fy, "_budget"),
                    paste0("VIC_", fy, "_myefo")),
    fiscal_year = fy,
    doc_type    = c("budget", "myefo"),
    source_url  = c(sof_url, bup_url)
  )
}) |>
  filter(!is.na(source_url),
         ## In-scope FYs only
         as.integer(substr(fiscal_year, 1L, 4L)) >= 2014L,
         as.integer(substr(fiscal_year, 1L, 4L)) <= 2025L)

cat(sprintf("\nScraped %d VIC PDFs:\n", nrow(scraped)))
print(scraped |> select(document_id, source_url), n = Inf)

## ---- 3. Update the registry --------------------------------------------

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
      "Source URL scraped from dtf.vic.gov.au.",
      notes
    )
  )

after <- sum(!is.na(reg$source_url))
write_csv(reg, "inst/document_registry.csv", na = "")
cat(sprintf("\nRegistry source_url count: %d -> %d (+%d)\n",
            before, after, after - before))
