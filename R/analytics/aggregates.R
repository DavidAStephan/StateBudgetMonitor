#' Aggregate facts across jurisdictions
#'
#' Combines all jurisdictions into an `"AUS_STATES"` aggregate. The
#' aggregation rule is the same shape for stocks and flows (sum), but
#' the interpretation differs:
#'
#'   * **Flow variables** (`is_flow = TRUE`) --- the sum is itself a
#'     flow: e.g. total revenue across states for FY2024-25.
#'   * **Stock variables** (`is_flow = FALSE`) --- the sum is a
#'     point-in-time aggregate balance: e.g. combined net debt at
#'     30 June 2024.
#'
#' Percentage / rate variables (unit `"pct"`) cannot be aggregated by
#' simple sum --- they are dropped from the aggregate panel. Real
#' multi-jurisdiction comparison happens on the comparison page,
#' not via aggregation.
#'
#' Coverage is exposed via `n_states` so downstream consumers can
#' flag partial-coverage rows.
#'
#' @param facts Latest-as-of facts tibble.
#' @return Aggregate tibble with `jurisdiction = "AUS_STATES"`.
#' @keywords internal
sbm_aggregate_states <- function(facts) {
  if (nrow(facts) == 0L) {
    return(
      facts |>
        dplyr::mutate(n_states = integer()) |>
        dplyr::slice(0)
    )
  }

  ## Drop rate-style variables that can't be summed.
  summable <- facts |>
    dplyr::filter(is.na(unit) | unit != "pct")

  if (nrow(summable) == 0L) {
    return(summable |> dplyr::mutate(n_states = integer()) |> dplyr::slice(0))
  }

  ## Group by (variable, fiscal_year) only --- the aggregate is the
  ## sum of each jurisdiction's latest-as-of value for that
  ## (jurisdiction, variable, fiscal_year), regardless of whether
  ## that latest was a Budget, MYEFO, or Outcome.
  summable |>
    dplyr::group_by(variable_id, canonical_label, category, sub_category,
                    unit, is_flow, gfs_code, fiscal_year) |>
    dplyr::summarise(
      value_aud_mil       = sum(value_aud_mil, na.rm = TRUE),
      n_states            = dplyr::n(),
      is_forward_estimate = any(is_forward_estimate),
      .groups             = "drop"
    ) |>
    dplyr::mutate(jurisdiction = "AUS_STATES", .before = 1L)
}
