#' Download Budget Paper PDFs
#'
#' For each row in the registry, downloads `source_url` to `pdf_path`
#' under the project root (typically `data/raw/<juris>/<fy>_<type>.pdf`).
#' Rows where `source_url` is missing are skipped --- the registry is
#' a living document; URLs that the maintainer has not yet pinned
#' don't block the pipeline.
#'
#' Caching: if the destination file already exists and is non-empty,
#' it is left untouched. To force a re-download, delete the file or
#' bump a row's `source_url` (changes invalidate the targets hash via
#' the registry file).
#'
#' Retries: HTTP failures honour the project's configured retry
#' policy via `sbm_http_get()`. Hard failures (after retries) log a
#' warning and leave any pre-existing cached file in place.
#'
#' @param registry The document registry tibble.
#' @param cfg Project config.
#' @return The registry with two additional columns:
#'   `download_status` (one of "cached", "downloaded", "skipped_no_url",
#'   "failed") and `local_path` (the absolute path on disk, or NA).
#' @export
sbm_download_pdfs <- function(registry, cfg) {
  raw_root <- cfg$paths$raw %||% "data/raw"
  fs::dir_create(raw_root)

  results <- purrr::pmap_dfr(
    registry |> dplyr::select(document_id, source_url, pdf_path),
    function(document_id, source_url, pdf_path) {
      dest <- pdf_path
      fs::dir_create(fs::path_dir(dest))

      if (is.na(source_url) || !nzchar(source_url)) {
        return(tibble::tibble(
          document_id     = document_id,
          download_status = "skipped_no_url",
          local_path      = NA_character_
        ))
      }

      if (file.exists(dest) && file.size(dest) > 0L) {
        return(tibble::tibble(
          document_id     = document_id,
          download_status = "cached",
          local_path      = fs::path_abs(dest)
        ))
      }

      resp <- tryCatch(
        sbm_http_get(source_url, cfg),
        error = function(e) e
      )

      if (inherits(resp, "error") ||
          httr2::resp_status(resp) >= 400L) {
        sbm_warn(sprintf(
          "download failed for %s (%s) --- leaving cache untouched",
          document_id, source_url
        ))
        return(tibble::tibble(
          document_id     = document_id,
          download_status = "failed",
          local_path      = if (file.exists(dest)) fs::path_abs(dest)
                            else NA_character_
        ))
      }

      writeBin(httr2::resp_body_raw(resp), dest)
      tibble::tibble(
        document_id     = document_id,
        download_status = "downloaded",
        local_path      = fs::path_abs(dest)
      )
    }
  )

  out <- registry |> dplyr::left_join(results, by = "document_id")

  sbm_info(sprintf(
    "downloads: %d cached / %d downloaded / %d skipped / %d failed",
    sum(out$download_status == "cached",         na.rm = TRUE),
    sum(out$download_status == "downloaded",     na.rm = TRUE),
    sum(out$download_status == "skipped_no_url", na.rm = TRUE),
    sum(out$download_status == "failed",         na.rm = TRUE)
  ))

  out
}
