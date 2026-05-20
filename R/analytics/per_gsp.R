#' Per-GSP normalisation
#'
#' Divides each fact's `value_aud_mil` by Gross State Product in the
#' same units (AUD millions) to produce a share of GSP.
#'
#' @param facts Latest-as-of facts tibble.
#' @param state_accounts ABS state accounts tibble with `gsp_aud_mil`.
#' @return Facts with `value_share_gsp` added.
#' @keywords internal
sbm_per_gsp <- function(facts, state_accounts) {
  if (nrow(facts) == 0L) {
    return(facts |> dplyr::mutate(value_share_gsp = numeric()))
  }
  facts |>
    dplyr::inner_join(state_accounts, by = c("jurisdiction", "fiscal_year")) |>
    dplyr::mutate(value_share_gsp = value_aud_mil / gsp_aud_mil) |>
    dplyr::select(-gsp_aud_mil)
}
