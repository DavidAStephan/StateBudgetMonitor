## Build the canonical variable dictionary as a long-format CSV.
##
## One row per (canonical_variable_id, jurisdiction). The canonical
## variables follow the taxonomy in the project brief; per-state
## source-line-item text starts as the canonical label and is refined
## as parsers consume Budget Papers.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(readr)
})

jurisdictions <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")

## ---------------------------------------------------------------------------
## Canonical variable definitions
## ---------------------------------------------------------------------------

canonical <- tribble(
  ~canonical_variable_id, ~canonical_label,                        ~gfs_code,    ~category,         ~sub_category,         ~unit,    ~is_flow,
  ## --- Revenue (operating statement) ----------------------------------------
  "rev_total",            "Total revenue",                          "GFS_REV",    "revenue",         "total",               "AUD_mil", TRUE,
  "rev_taxation",         "Taxation revenue",                       "GFS_TAX",    "revenue",         "taxation_total",      "AUD_mil", TRUE,
  "rev_payroll",          "Payroll tax",                            "GFS_PAYROLL","revenue",         "taxation_payroll",    "AUD_mil", TRUE,
  "rev_stamp_duty",       "Stamp duties",                           "GFS_DUTY",   "revenue",         "taxation_stamp",      "AUD_mil", TRUE,
  "rev_land_tax",         "Land tax",                               "GFS_LAND",   "revenue",         "taxation_land",       "AUD_mil", TRUE,
  "rev_gambling",         "Gambling taxes",                         "GFS_GAMBLE", "revenue",         "taxation_gambling",   "AUD_mil", TRUE,
  "rev_motor_vehicle",    "Motor vehicle taxes",                    "GFS_MV",     "revenue",         "taxation_motor",      "AUD_mil", TRUE,
  "rev_other_tax",        "Other taxation revenue",                 "GFS_OTHTAX", "revenue",         "taxation_other",      "AUD_mil", TRUE,
  "rev_gst",              "GST revenue",                            "GFS_GST",    "revenue",         "grants_gst",          "AUD_mil", TRUE,
  "rev_grants_tied",      "Commonwealth tied grants",               "GFS_TIED",   "revenue",         "grants_tied",         "AUD_mil", TRUE,
  "rev_grants_untied",    "Commonwealth untied grants (ex-GST)",    "GFS_UNTIED", "revenue",         "grants_untied",       "AUD_mil", TRUE,
  "rev_sales_goods_svcs", "Sales of goods and services",            "GFS_SALES",  "revenue",         "sales",               "AUD_mil", TRUE,
  "rev_interest",         "Interest income",                        "GFS_INT_IN", "revenue",         "interest_income",     "AUD_mil", TRUE,
  "rev_dividend",         "Dividend and tax-equivalent income",     "GFS_DIVID",  "revenue",         "dividend",            "AUD_mil", TRUE,
  "rev_other",            "Other revenue",                          "GFS_OTHREV", "revenue",         "other",               "AUD_mil", TRUE,

  ## --- Expenses by economic type --------------------------------------------
  "exp_total",            "Total expenses",                         "GFS_EXP",    "expenses",        "total",               "AUD_mil", TRUE,
  "exp_employee",         "Employee expenses",                      "GFS_EMP",    "expenses",        "economic_employee",   "AUD_mil", TRUE,
  "exp_super",            "Superannuation expenses",                "GFS_SUPER",  "expenses",        "economic_super",      "AUD_mil", TRUE,
  "exp_depreciation",     "Depreciation",                           "GFS_DEPR",   "expenses",        "economic_depr",       "AUD_mil", TRUE,
  "exp_interest",         "Interest expenses",                      "GFS_INT_EX", "expenses",        "economic_interest",   "AUD_mil", TRUE,
  "exp_other_operating",  "Other operating expenses",               "GFS_OTHOP",  "expenses",        "economic_other_op",   "AUD_mil", TRUE,
  "exp_current_grants",   "Current grants and subsidies",           "GFS_CURGR",  "expenses",        "economic_curr_grant", "AUD_mil", TRUE,
  "exp_capital_grants",   "Capital grants",                         "GFS_CAPGR",  "expenses",        "economic_cap_grant",  "AUD_mil", TRUE,

  ## --- Expenses by function (COFOG-aligned) ---------------------------------
  "exp_health",           "Expenses on health",                     "COFOG_07",   "expenses",        "function_health",     "AUD_mil", TRUE,
  "exp_education",        "Expenses on education",                  "COFOG_09",   "expenses",        "function_education",  "AUD_mil", TRUE,
  "exp_public_order",     "Expenses on public order and safety",    "COFOG_03",   "expenses",        "function_public_ord", "AUD_mil", TRUE,
  "exp_transport_comms",  "Expenses on transport and communications","COFOG_04T", "expenses",        "function_transport",  "AUD_mil", TRUE,
  "exp_social_security",  "Expenses on social security and welfare","COFOG_10",   "expenses",        "function_social",     "AUD_mil", TRUE,
  "exp_general_public",   "Expenses on general public services",    "COFOG_01",   "expenses",        "function_gen_pub",    "AUD_mil", TRUE,
  "exp_recreation",       "Expenses on recreation, culture, religion","COFOG_08", "expenses",        "function_recreation", "AUD_mil", TRUE,
  "exp_other_function",   "Expenses on other functions",            "COFOG_OTH",  "expenses",        "function_other",      "AUD_mil", TRUE,

  ## --- Balance sheet, capex, key fiscal aggregates --------------------------
  "net_op_balance",       "Net operating balance",                  "GFS_NOB",    "balance",         "operating",           "AUD_mil", TRUE,
  "net_cap_investment",   "Net capital investment",                 "GFS_NCI",    "balance",         "capex",               "AUD_mil", TRUE,
  "net_lending",          "Net lending / (borrowing)",              "GFS_NETLEND","balance",         "fiscal",              "AUD_mil", TRUE,
  "cash_surplus",         "Cash surplus / (deficit)",               "GFS_CASH",   "balance",         "cash",                "AUD_mil", TRUE,
  "net_debt",             "Net debt",                               "GFS_NETDEBT","balance_sheet",   "net_debt",            "AUD_mil", FALSE,
  "net_fin_liabilities",  "Net financial liabilities",              "GFS_NFL",    "balance_sheet",   "net_fin_liab",        "AUD_mil", FALSE,
  "net_worth",            "Net worth",                              "GFS_NW",     "balance_sheet",   "net_worth",           "AUD_mil", FALSE,
  "capex_pnfa",           "Purchases of non-financial assets",      "GFS_PNFA",   "capex",           "pnfa",                "AUD_mil", TRUE,
  "capex_infra_program",  "Total infrastructure investment program","GFS_INFRA",  "capex",           "infra_program",       "AUD_mil", TRUE,

  ## --- Economic parameters --------------------------------------------------
  "econ_gsp_growth",      "GSP growth (real, %)",                   "ECON_GSP",   "economic",        "gsp_growth",          "pct",     TRUE,
  "econ_employment",      "Employment growth (%)",                  "ECON_EMP",   "economic",        "employment",          "pct",     TRUE,
  "econ_unemployment",    "Unemployment rate (%)",                  "ECON_UNEMP", "economic",        "unemployment",        "pct",     FALSE,
  "econ_cpi",             "Consumer Price Index growth (%)",        "ECON_CPI",   "economic",        "cpi",                 "pct",     TRUE,
  "econ_wpi",             "Wage Price Index growth (%)",            "ECON_WPI",   "economic",        "wpi",                 "pct",     TRUE,
  "econ_population",      "Population growth (%)",                  "ECON_POP",   "economic",        "population",          "pct",     TRUE
)

## ---------------------------------------------------------------------------
## Cross-join to every jurisdiction. Source-line-item text defaults to
## the canonical label --- per-state refinements happen as parsers
## consume Budget Papers and as the user hand-curates the dictionary.
## ---------------------------------------------------------------------------

dict <- tidyr::expand_grid(
  canonical |> dplyr::mutate(rid = dplyr::row_number()),
  jurisdiction = jurisdictions
) |>
  dplyr::arrange(rid, jurisdiction) |>
  dplyr::mutate(
    source_line_item = canonical_label,
    mapping_notes    = "Seeded from canonical label --- refine per BP1 line-item text",
    parser_version   = "v0"
  ) |>
  dplyr::select(canonical_variable_id, canonical_label, gfs_code, category,
                sub_category, unit, is_flow, jurisdiction, source_line_item,
                mapping_notes, parser_version)

stopifnot(nrow(dict) == nrow(canonical) * length(jurisdictions))
cat(sprintf("Variable dictionary: %d rows (%d canonical x %d jurisdictions)\n",
            nrow(dict), nrow(canonical), length(jurisdictions)))

readr::write_csv(dict, "inst/variable_dictionary.csv", na = "")
cat("Wrote inst/variable_dictionary.csv\n")
