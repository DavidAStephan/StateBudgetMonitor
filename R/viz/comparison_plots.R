#' Cross-state comparison plot
#'
#' One trace per jurisdiction for a single variable. Designed to be
#' wrapped in `crosstalk::bscols()` so a `SharedData` filter widget
#' can drive selection.
#'
#' @param facts Tibble of facts (multiple jurisdictions, one variable).
#' @param title Plot title.
#' @param yaxis_title Y-axis label.
#' @return A `plotly` object.
#' @export
sbm_plot_comparison <- function(facts, title = NULL, yaxis_title = "AUD millions") {
  if (nrow(facts) == 0L) {
    return(
      plotly::plot_ly() |>
        sbm_theme_fiscal(title = title %||% "No data", yaxis_title = yaxis_title)
    )
  }

  plotly::plot_ly(
    data = facts,
    x = ~fiscal_year, y = ~value_aud_mil,
    color = ~jurisdiction,
    type  = "scatter", mode = "lines+markers",
    line  = list(width = 2)
  ) |>
    sbm_theme_fiscal(title = title, yaxis_title = yaxis_title)
}
