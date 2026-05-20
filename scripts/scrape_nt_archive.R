## Scrape NT State Budget archives:
##   - Current year on budget.nt.gov.au/papers (BP2 = fiscal strategy)
##   - Past budgets on treasury.nt.gov.au/dtf/financial-management-group/previous-budget-papers,
##     which links to per-year directories at
##     treasury.nt.gov.au/pms/financial-management/previous-budgets/<FY>/Budget-Paper-2-<FY>.pdf

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

past_html <- fetch("https://treasury.nt.gov.au/dtf/financial-management-group/previous-budget-papers")
if (is.null(past_html)) stop("Could not fetch NT past budgets page", call. = FALSE)

## All BP2 PDFs on the past page. Pattern varies year-to-year:
##   - older years: "Budget-Paper-2-2016-17.pdf"
##   - some years: "bp2-2018-19.pdf" or similar
##   - newer years (on budget.nt.gov.au): "<FY>-budget-bp2.pdf"
all_pdfs <- unique(str_match_all(past_html, 'href="(https?://[^"]+\\.pdf)"')[[1]][, 2])

fy_from_url <- function(url) {
  hits <- stringr::str_match_all(basename(url), "(19|20)(\\d{2})-(\\d{2})")[[1]]
  if (nrow(hits) == 0L) return(NA_character_)
  starts <- as.integer(paste0(hits[, 2L], hits[, 3L]))
  ends   <- as.integer(hits[, 4L])
  ok     <- (starts + 1L) %% 100L == ends
  if (!any(ok)) return(NA_character_)
  sprintf("%d-%02d", starts[ok][1L], ends[ok][1L])
}

scraped <- tibble(url = all_pdfs) |>
  mutate(
    fname    = basename(url),
    fy       = vapply(url, fy_from_url, character(1)),
    is_bp2   = grepl("(?i)budget.paper.2|bp2", fname),
    is_other_paper = grepl("(?i)budget.paper.[134]|bp[134]|economy|regional|overview|industry|guide|appropriation|speech|highlights",
                           fname)
  ) |>
  filter(is_bp2, !is_other_paper, !is.na(fy)) |>
  group_by(fy) |>
  slice(1) |>
  ungroup() |>
  filter(as.integer(substr(fy, 1L, 4L)) >= 2014L) |>
  transmute(
    document_id = paste0("NT_", fy, "_budget"),
    fiscal_year = fy,
    doc_type    = "budget",
    source_url  = url
  )

cat(sprintf("Scraped %d NT BP2 PDFs:\n", nrow(scraped)))
print(scraped |> select(document_id, source_url), n = Inf)

## ---- Update registry ----------------------------------------------------

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
      "Source URL scraped from treasury.nt.gov.au previous budget papers.",
      notes
    )
  )
after <- sum(!is.na(reg$source_url))
write_csv(reg, "inst/document_registry.csv", na = "")
cat(sprintf("\nRegistry source_url count: %d -> %d (+%d)\n",
            before, after, after - before))
