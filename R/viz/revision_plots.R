#' Vintage-trail plot
#'
#' Shows how the estimate for a single
#' `(jurisdiction, variable_id, fiscal_year)` has moved across
#' successive Budget Papers and MYEFOs. X axis is the document's
#' release date; Y axis is the AUD million value.
#'
#' @param revisions Tibble from `sbm_revision_history()` filtered down
#'   to one `(jurisdiction, variable_id, fiscal_year)`.
#' @param title Plot title.
#' @param yaxis_title Y-axis label.
#' @return A `plotly` object.
#' @export
sbm_plot_revisions <- function(revisions, title = NULL,
                               yaxis_title = "AUD millions") {
  if (nrow(revisions) == 0L) {
    return(
      plotly::plot_ly() |>
        sbm_theme_fiscal(title = title %||% "No data", yaxis_title = yaxis_title)
    )
  }

  plotly::plot_ly(
    data = revisions,
    x = ~release_date, y = ~value_aud_mil,
    color = ~doc_type,
    type = "scatter", mode = "lines+markers",
    text = ~document_id,
    hovertemplate = paste(
      "<b>%{text}</b><br>",
      "Released: %{x}<br>",
      "Value: %{y:,.0f} AUD mil<extra></extra>"
    )
  ) |>
    sbm_theme_fiscal(title = title, yaxis_title = yaxis_title) |>
    plotly::layout(xaxis = list(title = "Document release date"))
}
