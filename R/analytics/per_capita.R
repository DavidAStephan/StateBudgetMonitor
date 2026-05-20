#' Per-capita normalisation
#'
#' Divides each fact's `value_aud_mil` by the relevant population (in
#' millions) to produce AUD per person. Jurisdictions and fiscal
#' years without a matching population row are dropped from the
#' normalised output.
#'
#' @param facts Latest-as-of facts tibble.
#' @param population ABS population panel.
#' @return Facts with an additional column `value_per_capita_aud`.
#' @keywords internal
sbm_per_capita <- function(facts, population) {
  if (nrow(facts) == 0L) {
    return(facts |> dplyr::mutate(value_per_capita_aud = numeric()))
  }
  facts |>
    dplyr::inner_join(population, by = c("jurisdiction", "fiscal_year")) |>
    dplyr::mutate(value_per_capita_aud = value_aud_mil * 1e6 / population) |>
    dplyr::select(-population)
}
