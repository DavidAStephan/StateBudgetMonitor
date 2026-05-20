## Scrape WA State Budget archives:
##   - ourstatebudget.wa.gov.au/<FY>/index.html sub-pages list
##     Budget Paper No 3 (Economic and Fiscal Outlook) PDFs.
##   - BP3 is the canonical doc for our scope (General Government
##     Sector forward estimates).
##   - Mid-Year Review URLs aren't on the canonical archive page; can
##     be hunted separately if needed.

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

base <- "https://www.ourstatebudget.wa.gov.au"

## ---- 1. Get list of fiscal years ----------------------------------------

prev_html <- fetch(paste0(base, "/previous-budgets.html"))
if (is.null(prev_html)) stop("Could not fetch WA previous-budgets.html", call. = FALSE)

fy_pages <- unique(str_match_all(prev_html, '/([0-9]{4}-[0-9]{2})/index.html')[[1]][, 2])
fy_pages <- fy_pages[as.integer(substr(fy_pages, 1L, 4L)) >= 2014L]
fy_pages <- c(fy_pages, "2025-26")  # current year (not on archive page)
fy_pages <- unique(fy_pages)
cat("WA fiscal years to crawl:", paste(fy_pages, collapse = ", "), "\n")

## ---- 2. For each year find BP3 PDF --------------------------------------

scraped <- purrr::map_dfr(fy_pages, function(fy) {
  Sys.sleep(0.3)
  cat("  ", fy, "...\n", sep = "")
  html <- fetch(sprintf("%s/%s/index.html", base, fy))
  if (is.null(html)) return(NULL)
  hrefs <- unique(str_match_all(html, 'href="(/[^"]+\\.pdf)"')[[1]][, 2])
  bp3 <- hrefs[grepl("bp3", hrefs, ignore.case = TRUE) &
               !grepl("snapshot|overview|reader|guide", hrefs, ignore.case = TRUE)]
  if (length(bp3) == 0L) return(NULL)
  tibble(
    document_id = paste0("WA_", fy, "_budget"),
    fiscal_year = fy,
    doc_type    = "budget",
    source_url  = paste0(base, bp3[[1L]])
  )
})

cat(sprintf("\nScraped %d WA PDFs:\n", nrow(scraped)))
print(scraped |> select(document_id, source_url), n = Inf)

## ---- 3. Update registry -------------------------------------------------

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
      "Source URL scraped from ourstatebudget.wa.gov.au.",
      notes
    )
  )
after <- sum(!is.na(reg$source_url))
write_csv(reg, "inst/document_registry.csv", na = "")
cat(sprintf("\nRegistry source_url count: %d -> %d (+%d)\n",
            before, after, after - before))
