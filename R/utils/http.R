#' HTTP GET with retry/backoff
#'
#' Thin wrapper around `httr2::req_perform()` that honours the project's
#' configured retry policy and user-agent. Returns the response object;
#' callers are responsible for decoding the body.
#'
#' @param url Absolute URL to fetch.
#' @param cfg Project config (for retry / backoff / user-agent).
#' @param ... Additional `httr2::req_*` modifiers (e.g. `httr2::req_headers()`).
#' @return An `httr2_response` object.
#' @export
sbm_http_get <- function(url, cfg, ...) {
  retries <- cfg$run$http_retries  %||% 4L
  backoff <- cfg$run$http_backoff_seconds %||% c(1, 4, 16, 64)
  ua      <- cfg$run$user_agent     %||% "statebudgetmonitor"

  req <- httr2::request(url) |>
    httr2::req_user_agent(ua) |>
    httr2::req_retry(
      max_tries = retries + 1L,
      backoff   = function(i) backoff[min(i, length(backoff))]
    )

  dots <- list(...)
  for (modifier in dots) req <- modifier(req)

  httr2::req_perform(req)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
