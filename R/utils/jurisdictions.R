#' Canonical jurisdiction metadata
#'
#' All eight Australian states and territories with their full names,
#' financial-year end date (all 30 June), and typical Budget Paper
#' release month. Released-month is descriptive; the authoritative
#' release date comes from `dim_documents` once a document is ingested.
#'
#' @return A tibble with columns `code`, `name`, `fy_end_month`,
#'   `fy_end_day`, `budget_release_month`.
#' @export
sbm_jurisdictions <- function() {
  tibble::tribble(
    ~code, ~name,                          ~fy_end_month, ~fy_end_day, ~budget_release_month,
    "NSW", "New South Wales",              6L,            30L,         6L,
    "VIC", "Victoria",                     6L,            30L,         5L,
    "QLD", "Queensland",                   6L,            30L,         6L,
    "WA",  "Western Australia",            6L,            30L,         5L,
    "SA",  "South Australia",              6L,            30L,         6L,
    "TAS", "Tasmania",                     6L,            30L,         5L,
    "ACT", "Australian Capital Territory", 6L,            30L,         3L,
    "NT",  "Northern Territory",           6L,            30L,         5L
  )
}
