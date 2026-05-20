#' Time-series plot for a single jurisdiction
#'
#' Renders a single-variable, single-jurisdiction time series with
#' historical actuals (solid) and the most recent forward estimates
#' (dashed). Returns a `plotly` htmlwidget suitable for embedding in
#' a Quarto page.
#'
#' @param facts Tibble of facts already filtered to one
#'   `(jurisdiction, variable_id)`.
#' @param title Plot title.
#' @param yaxis_title Y-axis label.
#' @return A `plotly` object.
#' @export
sbm_plot_timeseries <- function(facts, title = NULL, yaxis_title = "AUD millions") {
  if (nrow(facts) == 0L) {
    return(
      plotly::plot_ly() |>
        sbm_theme_fiscal(title = title %||% "No data", yaxis_title = yaxis_title) |>
        plotly::add_annotations(
          text = "No data available for this selection.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    )
  }

  hist <- dplyr::filter(facts, !is_forward_estimate)
  fcst <- dplyr::filter(facts, is_forward_estimate)

  p <- plotly::plot_ly()

  if (nrow(hist) > 0L) {
    p <- plotly::add_trace(
      p,
      data = hist,
      x = ~fiscal_year, y = ~value_aud_mil,
      type = "scatter", mode = "lines+markers",
      name = "Actual",
      line = list(width = 2.5)
    )
  }

  if (nrow(fcst) > 0L) {
    p <- plotly::add_trace(
      p,
      data = fcst,
      x = ~fiscal_year, y = ~value_aud_mil,
      type = "scatter", mode = "lines+markers",
      name = "Forward estimate",
      line = list(width = 2.5, dash = "dash")
    )
  }

  sbm_theme_fiscal(p, title = title, yaxis_title = yaxis_title)
}
