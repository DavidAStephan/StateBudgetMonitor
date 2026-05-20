#' Reconcile harmonised facts against ABS Government Finance Statistics
#'
#' Joins our extracted facts to the ABS GFS panel on
#' `(jurisdiction, gfs_code, fiscal_year)` and flags any discrepancy
#' above `cfg$validation$gfs_tolerance_pct` (default 2.0%). Flagged
#' rows surface on the methodology page and (when the build log is
#' wired to STATUS.md) in the project status.
#'
#' The join requires that ABS GFS data has been harmonised to our
#' canonical schema; for now we project the raw ABS panel into the
#' canonical shape via `gfs_code`. Series whose `gfs_code` doesn't
#' appear on the ABS side are silently dropped from the comparison.
#'
#' Returns zero rows if either side is empty --- the report
#' degrades gracefully while ABS catalogue access is being worked
#' through (see STATUS.md).
#'
#' @param facts_latest Latest harmonised facts tibble.
#' @param abs_gfs ABS GFS panel tibble.
#' @param cfg Project config.
#' @return Tibble of reconciliation rows with `diff_pct` and
#'   `flagged` columns.
#' @export
sbm_reconcile_gfs <- function(facts_latest, abs_gfs, cfg) {
  tol <- cfg$validation$gfs_tolerance_pct %||% 2.0

  empty <- tibble::tibble(
    jurisdiction       = character(),
    variable_id        = character(),
    gfs_code           = character(),
    fiscal_year        = character(),
    sbm_value_aud_mil  = numeric(),
    gfs_value_aud_mil  = numeric(),
    diff_pct           = numeric(),
    flagged            = logical()
  )

  if (nrow(facts_latest) == 0L || nrow(abs_gfs) == 0L) {
    sbm_info("reconcile_gfs: at least one input panel is empty --- skipping")
    return(empty)
  }

  ## Aggregate ABS GFS series to (jurisdiction, gfs_code, fiscal_year).
  abs_agg <- abs_gfs |>
    dplyr::filter(!is.na(jurisdiction)) |>
    dplyr::mutate(fiscal_year = sbm_fy_label(period)) |>
    dplyr::group_by(jurisdiction, gfs_code = series_id, fiscal_year) |>
    dplyr::summarise(gfs_value_aud_mil = sum(value, na.rm = TRUE),
                     .groups = "drop")

  recon <- facts_latest |>
    dplyr::filter(!is.na(gfs_code)) |>
    dplyr::inner_join(abs_agg,
                      by = c("jurisdiction", "gfs_code", "fiscal_year")) |>
    dplyr::transmute(
      jurisdiction,
      variable_id,
      gfs_code,
      fiscal_year,
      sbm_value_aud_mil = value_aud_mil,
      gfs_value_aud_mil,
      diff_pct = 100 * (value_aud_mil - gfs_value_aud_mil) /
                       pmax(abs(gfs_value_aud_mil), 1)
    ) |>
    dplyr::mutate(flagged = abs(diff_pct) > tol)

  n_flag <- sum(recon$flagged, na.rm = TRUE)
  sbm_info(sprintf(
    "reconcile_gfs: %d rows compared, %d flagged (> %.1f%% diff)",
    nrow(recon), n_flag, tol
  ))
  recon
}
