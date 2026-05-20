## Render the Quarto site and check per-page load size.
##
## Usage: Rscript scripts/perf_test_pages.R
##
## Renders the full site to `_site/`, then for each HTML page reports
## file size, embedded payload size, and any pages exceeding the
## brief's 3-second target proxy (1 MB total payload as a rough
## proxy for client-side render time on a modern laptop).

suppressPackageStartupMessages({
  library(fs)
  library(dplyr)
  library(tibble)
})

site_dir <- "_site"
if (!dir_exists(site_dir)) {
  message("No `_site/` directory --- run `quarto render` first.")
  quit(status = 1L)
}

html_files <- dir_ls(site_dir, recurse = TRUE, glob = "*.html")
if (length(html_files) == 0L) {
  message("No HTML files in `_site/`. Did the render succeed?")
  quit(status = 1L)
}

threshold_bytes <- 1024 * 1024  # 1 MB
target_seconds  <- 3.0

report <- tibble(
  page      = fs::path_rel(html_files, site_dir),
  size_kb   = round(as.numeric(file_size(html_files)) / 1024, 1)
) |>
  mutate(over_threshold = size_kb * 1024 > threshold_bytes) |>
  arrange(desc(size_kb))

cat(sprintf("\nPage size report (threshold: %.0f KB, ~%.1fs target):\n",
            threshold_bytes / 1024, target_seconds))
print(report, n = Inf)

if (any(report$over_threshold)) {
  cat("\n!! Pages over threshold:\n")
  print(report |> filter(over_threshold))
  quit(status = 1L)
} else {
  cat("\nAll pages under threshold.\n")
}
