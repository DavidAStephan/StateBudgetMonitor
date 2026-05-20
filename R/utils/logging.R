#' Console logging helpers
#'
#' Thin wrappers over `cli` so that pipeline functions can log uniformly
#' without each caller importing `cli` directly. Mirrors the `nn_*`
#' family in NOM_Nowcast.
#'
#' @param msg Message string (may contain `{glue}`/`{cli}` placeholders).
#' @name sbm_logging
NULL

#' @rdname sbm_logging
#' @export
sbm_info    <- function(msg) cli::cli_alert_info(msg)

#' @rdname sbm_logging
#' @export
sbm_warn    <- function(msg) cli::cli_alert_warning(msg)

#' @rdname sbm_logging
#' @export
sbm_success <- function(msg) cli::cli_alert_success(msg)

#' @rdname sbm_logging
#' @export
sbm_danger  <- function(msg) cli::cli_alert_danger(msg)

#' Time an expression and log how long it took
#'
#' @param msg Label printed alongside the elapsed time.
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#' @export
sbm_time <- function(msg, expr) {
  t0 <- Sys.time()
  res <- force(expr)
  dt <- difftime(Sys.time(), t0, units = "secs")
  cli::cli_alert_info("{msg} ({format(round(as.numeric(dt), 2), nsmall = 2)}s)")
  res
}
