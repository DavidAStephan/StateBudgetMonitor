#' Fetch ABS Government Finance Statistics (annual)
#'
#' ABS catalogue 5512.0 (Government Finance Statistics, Annual)
#' migrated from time-series spreadsheets to Excel data cubes around
#' the same time NOM_Nowcast's OAD did. `readabs::read_abs()` no
#' longer accepts it; we therefore try `download_abs_data_cube()`
#' first and fall back to `read_abs()` for older / unmigrated
#' catalogues.
#'
#' Caches the underlying XLSX to `data/raw/abs/<cat_no>/`. On hard
#' failure with no cached file, returns an empty schema rather than
#' aborting the pipeline.
#'
#' Returned as a tidy long tibble. Jurisdiction mapping is attached
#' where the series description embeds a state name. Harmonisation
#' to our canonical `variable_id` taxonomy via `gfs_code` is done by
#' [`sbm_reconcile_gfs()`].
#'
#' @param cfg Project config.
#' @return Tibble with columns `jurisdiction`, `series_id`, `series`,
#'   `table_no`, `period`, `value`, `unit`, `release_date`.
#' @export
sbm_ingest_abs_gfs <- function(cfg) {
  cat_no  <- cfg$abs$gfs_annual_cat %||% "5512.0"
  out_dir <- file.path(cfg$paths$raw %||% "data/raw", "abs", cat_no)
  fs::dir_create(out_dir)

  ## Path 1: data cube (post-2021ish ABS pattern).
  cube <- tryCatch(
    sbm_ingest_abs_gfs_cube(cat_no, out_dir),
    error = function(e) {
      sbm_warn(sprintf("download_abs_data_cube failed for %s: %s",
                       cat_no, conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(cube) && nrow(cube) > 0L) return(cube)

  ## Path 2: legacy time-series spreadsheet via read_abs.
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
    sbm_warn(sprintf("abs_gfs: no data for cat %s --- returning empty schema",
                     cat_no))
    return(empty_abs_gfs_schema())
  }

  tidy <- abs_raw |>
    dplyr::transmute(
      series_id    = series_id,
      series       = series,
      table_no     = table_no,
      period       = as.Date(date),
      value        = as.numeric(value),
      unit         = unit,
      release_date = as.Date(NA)
    ) |>
    dplyr::mutate(jurisdiction = jurisdiction_from_series(series))

  sbm_info(sprintf("abs_gfs (cat %s, time-series): %d rows, %d distinct series",
                   cat_no, nrow(tidy), dplyr::n_distinct(tidy$series_id)))
  tidy
}

#' Try to fetch a GFS data cube and tidy it
#'
#' ABS data cubes are multi-sheet Excel files keyed by jurisdiction
#' and table. We download via `readabs::download_abs_data_cube()`,
#' then parse the sheets we recognise into our long tibble.
#'
#' This is best-effort: ABS cube structure varies across releases,
#' so the parser is permissive (returns NULL on any failure rather
#' than throwing). The caller falls back to the legacy time-series
#' path on NULL.
#'
#' @keywords internal
sbm_ingest_abs_gfs_cube <- function(cat_no, out_dir) {
  if (!exists("download_abs_data_cube",
              where = asNamespace("readabs"),
              mode = "function")) {
    return(NULL)
  }

  xlsx <- tryCatch(
    suppressMessages(
      readabs::download_abs_data_cube(
        catalogue_string = "government-finance-statistics-annual",
        cube             = "Table 1",
        path             = out_dir
      )
    ),
    error = function(e) NULL
  )

  if (is.null(xlsx) || !file.exists(xlsx)) return(NULL)

  ## ABS publishes the GFS cube with one sheet per fiscal aggregate
  ## block; we read every sheet and unify. Each sheet has a banner
  ## row + a "Series" column on the left. The parser is intentionally
  ## permissive --- production users should refine for their FY scope.
  sheet_names <- tryCatch(readxl::excel_sheets(xlsx), error = function(e) character())
  if (length(sheet_names) == 0L) return(NULL)

  tidy <- purrr::map_dfr(sheet_names, function(s) {
    raw <- tryCatch(
      suppressMessages(readxl::read_excel(xlsx, sheet = s, skip = 5L)),
      error = function(e) NULL
    )
    if (is.null(raw) || ncol(raw) < 2L) return(NULL)
    raw |>
      dplyr::rename(series = 1L) |>
      tidyr::pivot_longer(-series, names_to = "period_str",
                          values_to = "value", values_drop_na = TRUE) |>
      dplyr::mutate(
        period = suppressWarnings(as.Date(paste0("30-06-", period_str), "%d-%m-%Y")),
        table_no = s
      ) |>
      dplyr::filter(!is.na(period), is.numeric(value)) |>
      dplyr::transmute(
        series_id    = paste(s, series, sep = "::"),
        series,
        table_no,
        period,
        value        = as.numeric(value),
        unit         = "AUD_mil",
        release_date = as.Date(NA)
      )
  })

  if (nrow(tidy) == 0L) return(NULL)

  tidy <- tidy |> dplyr::mutate(jurisdiction = jurisdiction_from_series(series))
  sbm_info(sprintf("abs_gfs (cat %s, data cube): %d rows, %d distinct series",
                   cat_no, nrow(tidy), dplyr::n_distinct(tidy$series_id)))
  tidy
}

empty_abs_gfs_schema <- function() {
  tibble::tibble(
    jurisdiction = character(),
    series_id    = character(),
    series       = character(),
    table_no     = character(),
    period       = as.Date(character()),
    value        = numeric(),
    unit         = character(),
    release_date = as.Date(character())
  )
}

#' Derive jurisdiction code from an ABS series description
#'
#' ABS series names typically embed the jurisdiction name verbatim,
#' e.g. `"Net debt ; New South Wales ;"`. This regex-matches each of
#' the eight Australian states and territories against the series
#' string and returns the canonical 2-3 letter code, or NA if no
#' jurisdiction is detected.
#'
#' @keywords internal
jurisdiction_from_series <- function(series) {
  patterns <- c(
    "NSW" = "New South Wales",
    "VIC" = "Victoria",
    "QLD" = "Queensland",
    "WA"  = "Western Australia",
    "SA"  = "South Australia",
    "TAS" = "Tasmania",
    "ACT" = "Australian Capital Territory",
    "NT"  = "Northern Territory"
  )
  out <- rep(NA_character_, length(series))
  for (code in names(patterns)) {
    hit <- grepl(patterns[[code]], series, ignore.case = TRUE)
    out[hit & is.na(out)] <- code
  }
  out
}
