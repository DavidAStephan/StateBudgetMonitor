#' LLM-assisted Budget-Paper extractor (dev-machine only)
#'
#' Uses the `ellmer` package to drive a large language model
#' (Anthropic Claude by default) over a Budget-Paper PDF and produce
#' a structured `data/extracted/<juris>/<document_id>.csv` of fiscal
#' facts. The CSV schema matches the contract documented in
#' [`sbm_read_extracted_csv()`].
#'
#' **This function must never run in CI.** It honours
#' `cfg$llm$enabled_locally`, which defaults to FALSE, and aborts
#' immediately if it detects a GitHub Actions environment. The
#' workflow is:
#'
#'   1. Developer downloads the PDF (the registry already covers this).
#'   2. Developer runs `sbm_extract_llm(document_id)` on their machine.
#'   3. Function writes the structured CSV to `data/extracted/`.
#'   4. Developer commits the CSV.
#'   5. CI reads the committed CSV via the production extraction
#'      framework --- no LLM is ever invoked in CI.
#'
#' Requires the `ellmer` package and an `ANTHROPIC_API_KEY` env var.
#'
#' @param document_id Canonical document id to extract.
#' @param registry The document registry tibble.
#' @param dictionary The variable dictionary tibble (used to seed the
#'   LLM with the canonical variable taxonomy).
#' @param cfg Project config.
#' @param pages Optional integer vector of page numbers to send to the
#'   LLM (e.g. `100:140`). Anthropic caps PDF input at 100 pages, so
#'   for Budget Papers larger than that the caller must specify a
#'   subset covering the General Government Sector tables.
#' @return Path to the written CSV (invisibly), or NULL on failure /
#'   gated abort.
#' @export
sbm_extract_llm <- function(document_id, registry, dictionary, cfg,
                            pages = NULL) {

  ## --- production safety rails ---------------------------------------------
  if (!isTRUE(cfg$llm$enabled_locally)) {
    sbm_warn(
      "sbm_extract_llm: cfg$llm$enabled_locally is FALSE --- refusing to run. ",
      "Set llm.enabled_locally: true in config.yml on your dev machine ",
      "to enable LLM-assisted extraction."
    )
    return(invisible(NULL))
  }
  if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) {
    stop("sbm_extract_llm: refusing to run inside GitHub Actions. ",
         "LLM-assisted extraction is dev-machine only.",
         call. = FALSE)
  }
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("sbm_extract_llm: `ellmer` is not installed. ",
         "`install.packages('ellmer')` and retry.", call. = FALSE)
  }

  api_key_env <- cfg$llm$api_key_env %||% "ANTHROPIC_API_KEY"
  if (!nzchar(Sys.getenv(api_key_env))) {
    stop(sprintf("sbm_extract_llm: %s not set in env.", api_key_env),
         call. = FALSE)
  }

  ## --- look up the registry row -------------------------------------------
  row <- registry |> dplyr::filter(document_id == !!document_id)
  if (nrow(row) != 1L) {
    stop(sprintf("sbm_extract_llm: document_id %s not found in registry",
                 document_id), call. = FALSE)
  }
  pdf_path <- row$pdf_path
  if (!file.exists(pdf_path)) {
    stop(sprintf("sbm_extract_llm: PDF not on disk at %s --- run downloader first",
                 pdf_path), call. = FALSE)
  }

  ## Carve out the requested page range if needed. Anthropic caps PDF
  ## input at 100 pages; Budget Papers are often 150-400 pages, of
  ## which only ~20-40 contain the General Government Sector tables.
  if (!is.null(pages)) {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("sbm_extract_llm: `pdftools` is needed for page subsetting. ",
           "`install.packages('pdftools')` and retry.", call. = FALSE)
    }
    subset_dir <- file.path(tempdir(), "sbm_pdf_subsets")
    fs::dir_create(subset_dir)
    subset_path <- file.path(subset_dir, paste0(document_id, "_subset.pdf"))
    pdftools::pdf_subset(pdf_path, pages = pages, output = subset_path)
    sbm_info(sprintf("Subset PDF: %d pages -> %s",
                     length(pages), basename(subset_path)))
    pdf_path <- subset_path
  }

  ## --- build prompt with the canonical variable taxonomy ------------------
  juris_dict <- dictionary |>
    dplyr::filter(jurisdiction == row$jurisdiction) |>
    dplyr::transmute(
      variable_id = canonical_variable_id,
      label       = canonical_label,
      source_line_item,
      mapping_notes
    )

  taxonomy_text <- paste0(
    "Canonical variable taxonomy for ", row$jurisdiction, ":\n",
    paste(
      sprintf("  - %s: \"%s\" (typical source line: \"%s\")",
              juris_dict$variable_id,
              juris_dict$label,
              juris_dict$source_line_item),
      collapse = "\n"
    )
  )

  system_prompt <- glue::glue("
    You are extracting fiscal estimates from an Australian state \\
    Budget Paper for {row$jurisdiction}, fiscal year {row$fiscal_year}, \\
    document type {row$doc_type}.

    Find the General Government Sector forward estimates table. For \\
    each fiscal year present in the table, extract the value (in AUD \\
    millions) for as many of the canonical variables listed below as \\
    you can. Mark any year strictly after {row$fiscal_year} as a \\
    forward estimate (is_forward_estimate = TRUE); the budget year \\
    and any prior years are actuals or revised estimates \\
    (is_forward_estimate = FALSE).

    {taxonomy_text}

    Output a CSV with header:
    variable_id,value_aud_mil,fiscal_year,is_forward_estimate,source_line_item,notes

    One row per (variable_id x fiscal_year) you found. If a value is \\
    missing or unclear in the source, omit the row rather than \\
    guessing. Use the exact canonical variable_id values from the \\
    taxonomy. Notes column can be empty.
  ")

  model <- cfg$llm$model    %||% "claude-opus-4-7"
  provider <- cfg$llm$provider %||% "anthropic"

  ## ellmer reads ANTHROPIC_API_KEY from env automatically; we only
  ## verified above that it's set. Avoid the deprecated `api_key` arg.
  chat <- switch(
    provider,
    anthropic = ellmer::chat_anthropic(
      model         = model,
      system_prompt = system_prompt
    ),
    stop(sprintf("Unsupported LLM provider: %s", provider), call. = FALSE)
  )

  sbm_info(sprintf("LLM extraction: %s (model = %s)", document_id, model))
  csv_text <- chat$chat(
    ellmer::content_pdf_file(pdf_path)
  )

  ## --- write to disk and verify schema ------------------------------------
  out_dir  <- file.path(
    cfg$paths$extracted %||% "data/extracted",
    tolower(row$jurisdiction)
  )
  fs::dir_create(out_dir)
  out_path <- file.path(out_dir, paste0(document_id, ".csv"))

  ## Strip any markdown code-fence wrapping the LLM may emit.
  csv_text <- gsub("^```[a-z]*\\n|\\n```$", "", csv_text)
  writeLines(csv_text, out_path)

  ## Round-trip validate.
  check <- sbm_read_extracted_csv(out_path)
  if (nrow(check) == 0L) {
    sbm_danger(sprintf("LLM extraction wrote %s but it parses to 0 valid rows",
                       out_path))
  } else {
    sbm_success(sprintf("LLM extraction: %d rows -> %s", nrow(check), out_path))
  }

  invisible(out_path)
}
