#' Plotly layout defaults for fiscal charts
#'
#' Applied via `plotly::layout()` so every chart on the site shares a
#' consistent look without each `.qmd` repeating the same spec.
#'
#' @param p A `plotly` object.
#' @param title Plot title (character or NULL).
#' @param yaxis_title Y-axis title (character or NULL).
#' @return The `plotly` object with layout applied.
#' @export
sbm_theme_fiscal <- function(p, title = NULL, yaxis_title = NULL) {
  plotly::layout(
    p,
    title = title,
    font  = list(family = "Inter, system-ui, -apple-system, sans-serif",
                 size = 13L),
    margin = list(l = 60L, r = 20L, t = 50L, b = 60L),
    hovermode = "closest",
    xaxis = list(
      title       = "Fiscal year",
      showgrid    = TRUE,
      gridcolor   = "rgba(0,0,0,0.06)",
      zeroline    = FALSE
    ),
    yaxis = list(
      title       = yaxis_title,
      showgrid    = TRUE,
      gridcolor   = "rgba(0,0,0,0.06)",
      zerolinecolor = "rgba(0,0,0,0.2)",
      tickformat  = ","
    ),
    legend = list(orientation = "h", x = 0, y = -0.2)
  )
}
