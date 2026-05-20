#' Fetch ABS population by jurisdiction
#'
#' Wraps `readabs::read_abs()` for catalogue 3101.0 (National, State
#' and Territory Population). Selects the Estimated Resident
#' Population series per state / territory, takes the end-of-FY
#' (30 June) observation as the FY's population, and returns the
#' panel used for per-capita normalisation.
#'
#' Caches the underlying ABS time-series spreadsheet to
#' `data/raw/abs/3101.0/`. On network failure with no cached file,
#' returns an empty tibble.
#'
#' @param cfg Project config.
#' @return Tibble with columns `jurisdiction`, `fiscal_year`,
#'   `population`, `release_date`.
#' @export
sbm_ingest_abs_population <- function(cfg) {
  cat_no  <- cfg$abs$population_cat %||% "3101.0"
  out_dir <- file.path(cfg$paths$raw %||% "data/raw", "abs", cat_no)
  fs::dir_create(out_dir)

  abs_raw <- tryCatch(
    suppressMessages(
      readabs::read_abs(cat_no = cat_no, path = out_dir, show_progress_bars = FALSE)
    ),
    error = function(e) {
      sbm_warn(sprintf("readabs failed for cat %s: %s", cat_no, conditionMessage(e)))
      NULL
    }
  )

  if (is.null(abs_raw) || nrow(abs_raw) == 0L) {
    sbm_warn(sprintf("abs_population: no data for cat %s --- returning empty schema",
                     cat_no))
    return(empty_abs_population_schema())
  }

  pop <- abs_raw |>
    dplyr::mutate(jurisdiction = jurisdiction_from_series(series)) |>
    dplyr::filter(
      grepl("Estimated Resident Population", series, ignore.case = TRUE),
      grepl("Persons",                       series, ignore.case = TRUE),
      !is.na(jurisdiction)
    ) |>
    dplyr::transmute(
      jurisdiction,
      period       = as.Date(date),
      population   = as.numeric(value),
      release_date = as.Date(NA)
    ) |>
    ## ABS quarterly series are date-stamped as the first day of the
    ## quarter month; the June quarter ERP (period "06-01" in the series)
    ## is the end-of-FY value.
    dplyr::filter(format(period, "%m-%d") == "06-01") |>
    dplyr::mutate(fiscal_year = sbm_fy_label(period - 1L)) |>
    dplyr::select(jurisdiction, fiscal_year, population, release_date) |>
    dplyr::arrange(jurisdiction, fiscal_year)

  sbm_info(sprintf("abs_population (cat %s): %d rows across %d jurisdictions",
                   cat_no, nrow(pop), dplyr::n_distinct(pop$jurisdiction)))
  pop
}

empty_abs_population_schema <- function() {
  tibble::tibble(
    jurisdiction = character(),
    fiscal_year  = character(),
    population   = numeric(),
    release_date = as.Date(character())
  )
}
