## Scrape QLD State Budget archives:
##   - Past budgets: treasury.qld.gov.au/budget/queensland-budget/previous-budgets/<FY>/
##     -> Budget_<FY>_Strategy_Outlook.pdf (BP2, Budget Strategy and Outlook)
##   - Mid-Year Fiscal and Economic Review (MYFER) --- usually linked
##     from the same FY page, naming varies.
##   - Current year: budget.qld.gov.au/budget-papers/ for the LATEST budget.

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

abs_url <- function(rel, base) {
  if (is.na(rel)) return(NA_character_)
  if (startsWith(rel, "http")) return(rel)
  if (startsWith(rel, "/"))    return(paste0(base, rel))
  paste0(base, "/", rel)
}

find_pdf <- function(html, base, must_match, exclude = NULL) {
  if (is.null(html)) return(NA_character_)
  pdfs <- unique(c(
    str_match_all(html, 'href="(https?://[^"]+\\.pdf)"')[[1]][, 2],
    vapply(str_match_all(html, 'href="(/[^"]+\\.pdf)"')[[1]][, 2],
           abs_url, character(1), base = base)
  ))
  if (length(pdfs) == 0L) return(NA_character_)
  if (!is.null(must_match)) pdfs <- pdfs[grepl(must_match, pdfs, ignore.case = TRUE)]
  if (!is.null(exclude))    pdfs <- pdfs[!grepl(exclude, pdfs, ignore.case = TRUE)]
  if (length(pdfs) == 0L) return(NA_character_)
  pdfs[[1L]]
}

## ---- 1. Past budgets ---------------------------------------------------

archive_base <- "https://www.treasury.qld.gov.au"
archive_index <- fetch(paste0(archive_base, "/budget/queensland-budget/previous-budgets/"))
if (is.null(archive_index)) stop("Could not fetch QLD previous-budgets index",
                                 call. = FALSE)

fy_paths <- unique(str_match_all(
  archive_index,
  "/budget/queensland-budget/previous-budgets/([0-9]{4}-[0-9]{2})/"
)[[1]][, 2])
fy_paths <- fy_paths[as.integer(substr(fy_paths, 1L, 4L)) >= 2014L]
cat("QLD fiscal years to crawl:", paste(fy_paths, collapse = ", "), "\n")

scraped <- purrr::map_dfr(fy_paths, function(fy) {
  Sys.sleep(0.3)
  cat("  ", fy, "...\n", sep = "")
  url <- sprintf("%s/budget/queensland-budget/previous-budgets/%s/", archive_base, fy)
  html <- fetch(url)

  ## QLD BP2 = Strategy and Outlook (the canonical fiscal-statement doc)
  sof_url <- find_pdf(html, archive_base,
                      must_match = "Strategy.Outlook|BP2|BudgetPaper.2|Budget.Paper.2",
                      exclude    = "SDS_|RAP_|Capital|Speech|BP3|BP4")
  ## Mid-Year update --- search broadly; QLD has had MYFER, MYFR, etc.
  myefo_url <- find_pdf(html, archive_base,
                        must_match = "Mid.Year|MYFER|MYFR|MidYear",
                        exclude    = "SDS_|RAP_")

  tibble(
    document_id = c(paste0("QLD_", fy, "_budget"),
                    paste0("QLD_", fy, "_myefo")),
    fiscal_year = fy,
    doc_type    = c("budget", "myefo"),
    source_url  = c(sof_url, myefo_url)
  )
}) |>
  filter(!is.na(source_url))

## ---- 2. Current year on budget.qld.gov.au ------------------------------

current_base <- "https://budget.qld.gov.au"
curr <- fetch(paste0(current_base, "/budget-papers/"))
if (!is.null(curr)) {
  ## Find the FY tag on this page
  fy_curr <- str_extract(curr, "Budget.([0-9]{4}-[0-9]{2})")
  fy_curr <- if (!is.na(fy_curr)) str_extract(fy_curr, "[0-9]{4}-[0-9]{2}") else NA_character_
  if (!is.na(fy_curr) && !fy_curr %in% scraped$fiscal_year) {
    bp2 <- find_pdf(curr, current_base,
                    must_match = "BP2.*Budget.Strategy.Outlook|BP2-Budget-Strategy",
                    exclude    = "Contents|Overview|Snapshot|Glance|Speech")
    if (!is.na(bp2)) {
      scraped <- bind_rows(scraped,
        tibble(document_id = paste0("QLD_", fy_curr, "_budget"),
               fiscal_year = fy_curr, doc_type = "budget",
               source_url  = bp2))
    }
  }
}

cat(sprintf("\nScraped %d QLD PDFs:\n", nrow(scraped)))
print(scraped |> select(document_id, source_url), n = Inf)

## ---- 3. Update registry ------------------------------------------------

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
      "Source URL scraped from treasury.qld.gov.au / budget.qld.gov.au.",
      notes
    )
  )

after <- sum(!is.na(reg$source_url))
write_csv(reg, "inst/document_registry.csv", na = "")
cat(sprintf("\nRegistry source_url count: %d -> %d (+%d)\n",
            before, after, after - before))
