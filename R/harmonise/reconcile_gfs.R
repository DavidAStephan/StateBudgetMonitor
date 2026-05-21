#' Reconcile harmonised facts against ABS Government Finance Statistics
#'
#' Joins our extracted facts to the ABS GFS panel on
#' `(jurisdiction, variable_id, fiscal_year)`, computes a percentage
#' difference, and flags any row where the absolute discrepancy
#' exceeds `cfg$validation$gfs_tolerance_pct` (default 2.0%).
#'
#' Both sides use the same canonical `variable_id` taxonomy ---
#' [`sbm_ingest_abs_gfs()`] maps the ABS line items to our codes
#' before returning. Discrepancies above tolerance are the brief's
#' validation backstop: if a row passes, the LLM extracted the same
#' figure ABS publishes; if it fails, either our extraction is
#' wrong, the source Budget Paper differs from final GFS audit, or
#' there's a definitional mismatch (e.g. our `exp_total` includes
#' capital grants that ABS records elsewhere).
#'
#' @param facts_latest Latest harmonised facts tibble.
#' @param abs_gfs ABS GFS panel tibble.
#' @param cfg Project config.
#' @return Tibble of reconciliation rows with `diff_aud_mil`,
#'   `diff_pct`, and `flagged` columns. Sorted by absolute
#'   discrepancy descending so the worst offenders show first.
#' @export
sbm_reconcile_gfs <- function(facts_latest, abs_gfs, cfg) {
  tol <- cfg$validation$gfs_tolerance_pct %||% 2.0

  empty <- tibble::tibble(
    jurisdiction        = character(),
    variable_id         = character(),
    canonical_label     = character(),
    fiscal_year         = character(),
    sbm_value_aud_mil   = numeric(),
    abs_value_aud_mil   = numeric(),
    diff_aud_mil        = numeric(),
    diff_pct            = numeric(),
    flagged             = logical(),
    sbm_document_id     = character(),
    abs_line_item       = character()
  )

  if (nrow(facts_latest) == 0L || nrow(abs_gfs) == 0L) {
    sbm_info("reconcile_gfs: at least one input panel is empty --- skipping")
    return(empty)
  }

  recon <- facts_latest |>
    dplyr::select(jurisdiction, variable_id, canonical_label, fiscal_year,
                  sbm_value_aud_mil = value_aud_mil,
                  sbm_document_id   = document_id) |>
    dplyr::inner_join(
      abs_gfs |>
        dplyr::select(jurisdiction, variable_id, fiscal_year,
                      abs_value_aud_mil = value_aud_mil,
                      abs_line_item),
      by = c("jurisdiction", "variable_id", "fiscal_year")
    ) |>
    dplyr::mutate(
      diff_aud_mil = sbm_value_aud_mil - abs_value_aud_mil,
      diff_pct = ifelse(
        abs(abs_value_aud_mil) < 1,
        ifelse(abs(diff_aud_mil) < 1, 0, sign(diff_aud_mil) * 999),
        100 * diff_aud_mil / abs(abs_value_aud_mil)
      ),
      flagged = abs(diff_pct) > tol
    ) |>
    dplyr::arrange(dplyr::desc(abs(diff_pct)))

  n_total <- nrow(recon)
  n_flag  <- sum(recon$flagged, na.rm = TRUE)
  sbm_info(sprintf(
    "reconcile_gfs: %d rows compared, %d flagged (> %.1f%%) --- %.1f%% pass rate",
    n_total, n_flag, tol, 100 * (n_total - n_flag) / max(n_total, 1L)
  ))
  recon
}
