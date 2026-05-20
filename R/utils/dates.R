#' Resolve the pipeline as-of date
#'
#' Vintage replay is driven by an as-of date. If the config explicitly
#' sets `run$asof_date`, that wins. Otherwise we fall back to
#' `Sys.Date()`. Returned as a `Date`.
#'
#' @param cfg Project config (from `config::get()`).
#' @return A length-1 `Date`.
#' @export
sbm_asof <- function(cfg) {
  raw <- cfg$run$asof_date
  if (is.null(raw) || identical(raw, "") || is.na(raw)) {
    return(Sys.Date())
  }
  as.Date(raw)
}

#' Convert a calendar date to an Australian financial-year label
#'
#' Australian financial years run 1 July to 30 June. A date of
#' `2024-09-01` belongs to FY `2024-25`.
#'
#' @param x A `Date` or character coercible to `Date`.
#' @return A character vector of FY labels, e.g. `"2024-25"`.
#' @export
sbm_fy_label <- function(x) {
  x <- as.Date(x)
  year <- as.integer(format(x, "%Y"))
  month <- as.integer(format(x, "%m"))
  fy_start <- ifelse(month >= 7L, year, year - 1L)
  fy_end_two_digit <- sprintf("%02d", (fy_start + 1L) %% 100L)
  paste0(fy_start, "-", fy_end_two_digit)
}

#' Convert an Australian financial-year label to its start and end dates
#'
#' @param fy A character vector of FY labels, e.g. `"2024-25"`.
#' @return A tibble with columns `fy`, `start_date`, `end_date`.
#' @export
sbm_fy_to_dates <- function(fy) {
  stopifnot(is.character(fy))
  start_year <- as.integer(substr(fy, 1L, 4L))
  tibble::tibble(
    fy         = fy,
    start_date = as.Date(paste0(start_year,        "-07-01")),
    end_date   = as.Date(paste0(start_year + 1L,   "-06-30"))
  )
}
