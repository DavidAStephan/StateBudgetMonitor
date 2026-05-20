#' Fetch ABS state accounts (Gross State Product)
#'
#' Wraps `readabs::read_abs()` for catalogue 5220.0 (Australian
#' National Accounts: State Accounts). Selects the nominal (current
#' price) GSP series per jurisdiction; this is what we divide into
#' fiscal aggregates for the share-of-GSP normalisation.
#'
#' Caches the underlying ABS spreadsheet to `data/raw/abs/5220.0/`.
#'
#' @param cfg Project config.
#' @return Tibble with columns `jurisdiction`, `fiscal_year`,
#'   `gsp_aud_mil`, `release_date`.
#' @export
sbm_ingest_abs_state_accounts <- function(cfg) {
  cat_no  <- cfg$abs$state_accounts_cat %||% "5220.0"
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
    sbm_warn(sprintf("abs_state_accounts: no data for cat %s --- returning empty schema",
                     cat_no))
    return(empty_abs_state_accounts_schema())
  }

  ## Select nominal (current prices) GSP, annual frequency, per jurisdiction.
  gsp <- abs_raw |>
    dplyr::mutate(jurisdiction = jurisdiction_from_series(series)) |>
    dplyr::filter(
      grepl("Gross state product",                  series, ignore.case = TRUE),
      grepl("Current prices",                       series, ignore.case = TRUE),
      !grepl("per capita",                          series, ignore.case = TRUE),
      !is.na(jurisdiction)
    ) |>
    dplyr::transmute(
      jurisdiction,
      period       = as.Date(date),
      gsp_aud_mil  = as.numeric(value),
      release_date = as.Date(NA)
    ) |>
    dplyr::filter(format(period, "%m-%d") == "06-30") |>
    dplyr::mutate(fiscal_year = sbm_fy_label(period - 1L)) |>
    dplyr::select(jurisdiction, fiscal_year, gsp_aud_mil, release_date) |>
    dplyr::arrange(jurisdiction, fiscal_year)

  sbm_info(sprintf("abs_state_accounts (cat %s): %d rows across %d jurisdictions",
                   cat_no, nrow(gsp), dplyr::n_distinct(gsp$jurisdiction)))
  gsp
}

empty_abs_state_accounts_schema <- function() {
  tibble::tibble(
    jurisdiction = character(),
    fiscal_year  = character(),
    gsp_aud_mil  = numeric(),
    release_date = as.Date(character())
  )
}
