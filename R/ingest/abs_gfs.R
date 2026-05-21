#' Fetch ABS Government Finance Statistics (annual, GG sector by state)
#'
#' Downloads the per-state General Government sector data files directly
#' from the ABS catalogue 5512.0 publication page. ABS migrated this
#' catalogue from time-series spreadsheets to data cubes, and
#' `readabs::read_abs()` no longer handles it; we bypass readabs and
#' fetch the .xlsx files directly via the documented URL pattern:
#'
#'   `https://www.abs.gov.au/statistics/economy/government/`
#'   `government-finance-statistics-annual/<FY>/55120DO00N_<FY_compressed>.xlsx`
#'
#' where N is the table number (003-010 for the eight states).
#'
#' Each .xlsx has four sheets:
#'   * `Table_1` --- operating statement (revenue, expenses, net
#'     operating balance, net acquisition of non-financial assets)
#'   * `Table_2` --- cash flow statement
#'   * `Table_3` --- balance sheet (net debt, net worth, etc.)
#'   * `Table_4` --- expenses by purpose / COFOG function
#'
#' This fetcher reads Tables 1 + 3 and produces a long tibble keyed
#' on `(jurisdiction, abs_line_item, fiscal_year)`.  The mapping
#' from `abs_line_item` to our canonical `variable_id` lives in the
#' inline `abs_to_canonical` table.
#'
#' Caches downloaded xlsx files under `data/raw/abs/5512.0/`.
#'
#' @param cfg Project config (uses `cfg$run$user_agent` and
#'   `cfg$abs$gfs_annual_publication_fy`).
#' @return Tibble with columns `jurisdiction`, `variable_id`,
#'   `abs_line_item`, `fiscal_year`, `value_aud_mil`, `release_date`.
#' @export
sbm_ingest_abs_gfs <- function(cfg) {
  publication_fy <- cfg$abs$gfs_annual_publication_fy %||% "2024-25"
  fy_compressed  <- gsub("-", "", publication_fy)  # "202425"

  out_dir <- file.path(cfg$paths$raw %||% "data/raw", "abs", "5512.0")
  fs::dir_create(out_dir)

  ## DO00n -> jurisdiction mapping confirmed from the ABS catalogue
  ## page; stable across releases.
  state_tables <- tibble::tribble(
    ~do_num, ~jurisdiction,
    "003",   "NSW",
    "004",   "VIC",
    "005",   "QLD",
    "006",   "SA",
    "007",   "WA",
    "008",   "TAS",
    "009",   "NT",
    "010",   "ACT"
  )

  base_url <- sprintf(
    "https://www.abs.gov.au/statistics/economy/government/government-finance-statistics-annual/%s/",
    publication_fy
  )

  ## --- Download each xlsx (cache-aware) ---------------------------------
  for (i in seq_len(nrow(state_tables))) {
    dest <- file.path(out_dir,
                      sprintf("%s_GG.xlsx", state_tables$jurisdiction[i]))
    if (!file.exists(dest) || file.size(dest) < 10000L) {
      url <- sprintf("%s55120DO%s_%s.xlsx",
                     base_url, state_tables$do_num[i], fy_compressed)
      ok <- tryCatch({
        resp <- sbm_http_get(url, cfg)
        if (httr2::resp_status(resp) < 400L) {
          writeBin(httr2::resp_body_raw(resp), dest)
          TRUE
        } else FALSE
      }, error = function(e) FALSE)
      if (!ok) {
        sbm_warn(sprintf("abs_gfs: could not download %s", url))
        next
      }
    }
  }

  ## --- Parse each file --------------------------------------------------
  panels <- purrr::map_dfr(seq_len(nrow(state_tables)), function(i) {
    juris <- state_tables$jurisdiction[i]
    f <- file.path(out_dir, sprintf("%s_GG.xlsx", juris))
    if (!file.exists(f)) return(NULL)
    bind_rows(
      parse_abs_gfs_sheet(f, "Table_1", juris),
      parse_abs_gfs_sheet(f, "Table_3", juris)
    )
  })

  if (is.null(panels) || nrow(panels) == 0L) {
    sbm_warn("abs_gfs: nothing extracted --- returning empty schema")
    return(empty_abs_gfs_schema())
  }

  ## --- Map ABS line items to our canonical variable_ids -----------------
  abs_to_canonical <- tibble::tribble(
    ~abs_line_item,                                    ~variable_id,
    "Taxation revenue",                                 "rev_taxation",
    "Sales of goods and services",                      "rev_sales_goods_svcs",
    "Interest income",                                  "rev_interest",
    "Dividend income",                                  "rev_dividend",
    "Other revenue",                                    "rev_other",
    "Total GFS revenue",                                "rev_total",
    "Depreciation",                                     "exp_depreciation",
    "Total GFS expenses",                               "exp_total",
    "GFS Net operating balance",                        "net_op_balance",
    "GFS NET LENDING(+)/BORROWING(-)",                  "net_lending",
    "Total net acquisition of non-financial assets",    "capex_pnfa",
    "Net debt(a)",                                      "net_debt",
    "Net debt",                                         "net_debt",
    "Net worth",                                        "net_worth",
    "Net financial worth",                              "net_fin_worth",
    "Net financial liabilities",                        "net_fin_liabilities"
  )

  out <- panels |>
    dplyr::inner_join(abs_to_canonical, by = "abs_line_item") |>
    dplyr::transmute(
      jurisdiction,
      variable_id,
      abs_line_item,
      fiscal_year,
      value_aud_mil,
      release_date = as.Date(NA)
    ) |>
    dplyr::distinct()

  sbm_info(sprintf("abs_gfs: %d rows across %d jurisdictions, %d variables",
                   nrow(out), dplyr::n_distinct(out$jurisdiction),
                   dplyr::n_distinct(out$variable_id)))
  out
}

#' Parse a single Table_n sheet from an ABS GFS xlsx
#'
#' The ABS structure: row 5 has FY column headers, row 7+ has data.
#' Column 1 is the line-item label.
#'
#' @keywords internal
parse_abs_gfs_sheet <- function(path, sheet, juris) {
  d <- tryCatch(
    suppressWarnings(suppressMessages(
      readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                         .name_repair = "minimal")
    )),
    error = function(e) NULL
  )
  if (is.null(d) || nrow(d) < 8L) return(NULL)

  ## Row 5 has fiscal year headers; row 6 has the "$m" unit row.
  fy_row <- as.character(unlist(d[5, , drop = TRUE]))
  fy_cols <- which(grepl("^[12][0-9]{3}-[0-9]{2}$", fy_row))
  if (length(fy_cols) == 0L) return(NULL)

  fy_labels <- fy_row[fy_cols]
  labels    <- d[[1L]]

  rows <- purrr::map_dfr(seq_len(nrow(d)), function(i) {
    lab <- labels[[i]]
    if (is.na(lab) || nchar(as.character(lab)) < 3L) return(NULL)
    vals <- suppressWarnings(as.numeric(unlist(d[i, fy_cols, drop = TRUE])))
    if (all(is.na(vals))) return(NULL)
    tibble::tibble(
      jurisdiction  = juris,
      abs_line_item = as.character(lab),
      fiscal_year   = fy_labels,
      value_aud_mil = vals
    )
  })

  rows |> dplyr::filter(!is.na(value_aud_mil))
}

empty_abs_gfs_schema <- function() {
  tibble::tibble(
    jurisdiction  = character(),
    variable_id   = character(),
    abs_line_item = character(),
    fiscal_year   = character(),
    value_aud_mil = numeric(),
    release_date  = as.Date(character())
  )
}

#' Derive jurisdiction code from an ABS series description (legacy helper)
#'
#' Retained for any callers that still use the old time-series series
#' string format. New cube-based fetcher labels jurisdictions directly.
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
