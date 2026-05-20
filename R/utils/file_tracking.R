#' Track a set of files for `targets` invalidation
#'
#' Returns a tibble of `(path, mtime, size)` for every file under
#' `dir` matching the pattern. `targets` hashes the result, so any
#' file added, removed, renamed, or modified invalidates the target.
#'
#' Use this instead of `tarchetypes::tar_files()` when the file list
#' may be empty --- `tar_files()` errors on a zero-length input,
#' which is hostile during scaffolding when no CSVs have been
#' committed yet.
#'
#' @param dir Directory to scan.
#' @param pattern Filename regex (default: `"\\.csv$"`).
#' @param recursive Whether to recurse into subdirectories.
#' @return Tibble with columns `path`, `mtime`, `size`. Zero rows
#'   when the directory is missing or empty.
#' @keywords internal
sbm_list_extracted_csvs <- function(dir, pattern = "\\.csv$", recursive = TRUE) {
  if (!dir.exists(dir)) {
    return(tibble::tibble(
      path  = character(),
      mtime = as.POSIXct(character()),
      size  = numeric()
    ))
  }
  files <- list.files(dir, pattern = pattern, recursive = recursive,
                      full.names = TRUE)
  if (length(files) == 0L) {
    return(tibble::tibble(
      path  = character(),
      mtime = as.POSIXct(character()),
      size  = numeric()
    ))
  }
  info <- file.info(files)
  tibble::tibble(
    path  = normalizePath(files, mustWork = FALSE),
    mtime = info$mtime,
    size  = info$size
  )
}
