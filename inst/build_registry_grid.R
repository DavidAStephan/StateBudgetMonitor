## Build the document_registry.csv grid programmatically.
##
## This is a one-shot generator: run it whenever the FY range needs
## extending, or to refresh the deterministic shape of the registry.
## URLs in the resulting CSV are best-effort placeholders ---
## hand-edit `inst/document_registry.csv` directly to refine them as
## the budget calendar evolves.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(readr)
  library(stringr)
})

## ---------------------------------------------------------------------------
## Jurisdiction metadata --- duplicated from R/utils/jurisdictions.R because
## this script is run standalone (not as part of `tar_make()`).
## ---------------------------------------------------------------------------

jurisdictions <- tribble(
  ~code, ~name,                          ~budget_month, ~myefo_month, ~outcome_month,
  "NSW", "New South Wales",               6L,            12L,          10L,
  "VIC", "Victoria",                      5L,            12L,           9L,
  "QLD", "Queensland",                    6L,            12L,           9L,
  "WA",  "Western Australia",             5L,            12L,           9L,
  "SA",  "South Australia",               6L,            12L,           9L,
  "TAS", "Tasmania",                      5L,            12L,          10L,
  "ACT", "Australian Capital Territory",  3L,            12L,          10L,
  "NT",  "Northern Territory",            5L,            12L,           9L
)

landing_url <- c(
  NSW = "https://www.budget.nsw.gov.au/",
  VIC = "https://www.dtf.vic.gov.au/state-budget",
  QLD = "https://budget.qld.gov.au/",
  WA  = "https://www.ourstatebudget.wa.gov.au/",
  SA  = "https://www.statebudget.sa.gov.au/",
  TAS = "https://www.treasury.tas.gov.au/budget-and-financial-management/state-budget",
  ACT = "https://www.treasury.act.gov.au/budget",
  NT  = "https://budget.nt.gov.au/"
)

## ---------------------------------------------------------------------------
## Coverage window
## ---------------------------------------------------------------------------

start_fy_year <- 2014L  # FY2014-15
end_fy_year   <- 2025L  # FY2025-26
asof_date     <- as.Date("2026-05-20")

fy_years <- start_fy_year:end_fy_year
fy_label <- function(y) sprintf("%d-%02d", y, (y + 1L) %% 100L)

## ---------------------------------------------------------------------------
## Build grid: 8 jurisdictions x 3 doc_types x FYs (where the document
## would have been released on or before asof_date).
## ---------------------------------------------------------------------------

build_rows <- function(juris, doc_type) {
  meta <- jurisdictions |> filter(code == juris)

  month <- switch(doc_type,
                  budget  = meta$budget_month,
                  myefo   = meta$myefo_month,
                  outcome = meta$outcome_month)

  ## Budget for FY <y>-<y+1>: released in (calendar) year y (Mar-Jun).
  ## MYEFO for FY <y>-<y+1>: released in Dec of year y.
  ## Outcome for FY <y>-<y+1>: released in Sep-Oct of year y+1.
  rows <- purrr::map_dfr(fy_years, function(yr) {
    cal_year <- switch(doc_type,
                       budget  = yr,
                       myefo   = yr,
                       outcome = yr + 1L)
    rel <- as.Date(sprintf("%d-%02d-15", cal_year, month))
    if (rel > asof_date) return(NULL)

    state_specific <- switch(doc_type,
      budget  = "Budget Papers (BP1 + BP2 + BP3 + supporting volumes)",
      myefo   = switch(juris,
        NSW = "Half-Yearly Review",
        VIC = "Budget Update",
        QLD = "Mid-Year Fiscal and Economic Review",
        WA  = "Mid-Year Review",
        SA  = "Mid-Year Budget Review",
        TAS = "Revised Estimates Report",
        ACT = "Budget Review",
        NT  = "Mid-Year Report"
      ),
      outcome = switch(juris,
        NSW = "Final Budget Outcome / Report on State Finances",
        VIC = "Financial Report for the State",
        QLD = "Report on State Finances",
        WA  = "Annual Report on State Finances",
        SA  = "Final Budget Outcome",
        TAS = "Treasurer's Annual Financial Report",
        ACT = "Consolidated Annual Financial Statements",
        NT  = "Treasurer's Annual Financial Report"
      )
    )

    tibble(
      document_id    = sprintf("%s_%s_%s", juris, fy_label(yr), doc_type),
      jurisdiction   = juris,
      doc_type       = doc_type,
      fiscal_year    = fy_label(yr),
      release_date   = rel,
      source_url     = NA_character_,
      pdf_path       = sprintf("data/raw/%s/%s_%s.pdf",
                               tolower(juris), fy_label(yr), doc_type),
      parser_version = "v0",
      notes          = sprintf(
        "State-specific name: %s. release_date is an estimate from typical release month --- replace with the actual gazetted date when known. URL TBD --- starting point: %s",
        state_specific, landing_url[[juris]]
      )
    )
  })

  rows
}

grid <- expand_grid(
  jurisdiction = jurisdictions$code,
  doc_type     = c("budget", "myefo", "outcome")
) |>
  purrr::pmap_dfr(\(jurisdiction, doc_type) build_rows(jurisdiction, doc_type)) |>
  arrange(jurisdiction, fiscal_year, doc_type)

## ---------------------------------------------------------------------------
## Confirmed rows: hand-curated entries with known URLs / dates.
## Listed here, then overlaid on top of the grid so we keep the
## benefit of the deterministic grid while pinning known specifics.
## ---------------------------------------------------------------------------

confirmed <- tribble(
  ~document_id,            ~release_date,    ~source_url,                                                          ~notes,
  "NSW_2023-24_outcome",   "2024-10-25",     "https://www.budget.nsw.gov.au/2023-24/final-budget-outcome",         "Confirmed seed row.",
  "NSW_2024-25_budget",    "2024-06-18",     "https://www.budget.nsw.gov.au/2024-25",                              "Confirmed seed row.",
  "NSW_2024-25_myefo",     "2024-12-17",     "https://www.budget.nsw.gov.au/2024-25/half-yearly-review",           "Confirmed seed row (NSW Half-Yearly Review).",
  "VIC_2023-24_outcome",   "2024-09-27",     "https://www.dtf.vic.gov.au/state-budget/2023-24-financial-report",   "Confirmed seed row (VIC Financial Report).",
  "VIC_2024-25_budget",    "2024-05-07",     "https://www.dtf.vic.gov.au/state-budget/2024-25-state-budget",       "Confirmed seed row."
) |>
  mutate(release_date = as.Date(release_date))

grid <- grid |>
  rows_update(confirmed, by = "document_id", unmatched = "ignore")

## ---------------------------------------------------------------------------
## Sanity checks
## ---------------------------------------------------------------------------

stopifnot(length(unique(grid$document_id)) == nrow(grid))
stopifnot(all(grid$release_date <= asof_date))

cat(sprintf("Registry grid: %d rows across %d jurisdictions and %d doc_types.\n",
            nrow(grid), dplyr::n_distinct(grid$jurisdiction),
            dplyr::n_distinct(grid$doc_type)))
cat("Coverage by doc_type:\n")
print(grid |> count(doc_type))
cat("Coverage by jurisdiction:\n")
print(grid |> count(jurisdiction))
cat("Confirmed URLs: ", sum(!is.na(grid$source_url)), "\n", sep = "")

write_csv(grid, "inst/document_registry.csv", na = "")
cat("Wrote inst/document_registry.csv\n")
