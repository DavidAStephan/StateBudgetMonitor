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
  ## input at 100 pages; Budget Papers are often 150-400 pages.
  ##
  ## If `pages` is a numeric vector of <=100 pages: send that slice.
  ## If it's a list of slices: send each slice as a separate LLM call
  ## and concatenate the resulting CSVs (used for whole-PDF chunking).
  if (!is.null(pages) && !is.list(pages)) {
    if (length(pages) > 100L) {
      stop("sbm_extract_llm: page slice exceeds Anthropic's 100-page cap. ",
           "Pass `pages` as a list of slices to auto-chunk.", call. = FALSE)
    }
    pages <- list(pages)
  }
  if (is.null(pages) || length(pages) == 0L) {
    pages <- list(NULL)  # send the whole PDF (must be <=100 pages)
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
  ## A fresh chat per slice is essential --- `chat_anthropic()` keeps
  ## conversation history, so reusing one instance accumulates prior
  ## slice contents into every subsequent request and blows past the
  ## 200K context window on the later slices of large docs.
  new_chat <- function() {
    switch(
      provider,
      anthropic = ellmer::chat_anthropic(
        model         = model,
        system_prompt = system_prompt
      ),
      stop(sprintf("Unsupported LLM provider: %s", provider), call. = FALSE)
    )
  }

  sbm_info(sprintf("LLM extraction: %s (model = %s, %d slice%s)",
                   document_id, model, length(pages),
                   if (length(pages) == 1L) "" else "s"))

  ## Extract text per slice and send as plain text. Sending PDF binary
  ## directly used Anthropic's vision tokens (each page rendered as an
  ## image, ~5K tokens), blowing past the 200K context on text-heavy
  ## Budget Papers. Plain text averages ~500-1500 tokens per page,
  ## fitting comfortably within the window.
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("sbm_extract_llm: `pdftools` is required.", call. = FALSE)
  }

  full_text <- tryCatch(pdftools::pdf_text(pdf_path),
                        error = function(e) NULL)
  if (is.null(full_text) || length(full_text) == 0L) {
    sbm_warn(sprintf("sbm_extract_llm: unable to read text from %s", pdf_path))
    return(invisible(NULL))
  }

  csv_chunks <- vapply(seq_along(pages), function(i) {
    slice <- pages[[i]]
    if (is.null(slice)) slice <- seq_along(full_text)
    slice_text <- paste(full_text[slice], collapse = "\n\n")
    if (length(pages) > 1L) {
      sbm_info(sprintf("  slice %d/%d (%d pages, %d chars)",
                       i, length(pages), length(slice), nchar(slice_text)))
    }
    ## Fresh chat per slice (see new_chat() above).
    ## If a single slice errors, log and continue with the others ---
    ## partial results are better than nothing for the rest of the doc.
    tryCatch(
      new_chat()$chat(slice_text),
      error = function(e) {
        sbm_warn(sprintf("  slice %d/%d failed: %s",
                         i, length(pages), conditionMessage(e)))
        ""
      }
    )
  }, character(1))
  csv_text <- paste(csv_chunks, collapse = "\n")

  ## --- write to disk and verify schema ------------------------------------
  out_dir  <- file.path(
    cfg$paths$extracted %||% "data/extracted",
    tolower(row$jurisdiction)
  )
  fs::dir_create(out_dir)
  out_path <- file.path(out_dir, paste0(document_id, ".csv"))

  ## Sanitise the response: the LLM may wrap the CSV in markdown
  ## code-fences, prepend a chatty preamble, or append a postscript.
  ## We keep only:
  ##   (a) exactly one header line matching the contract
  ##   (b) data lines that start with a snake_case variable_id
  ##       followed by a comma and a numeric value
  csv_lines <- unlist(strsplit(csv_text, "\n", fixed = TRUE))
  header_idx <- grep("^variable_id\\s*,\\s*value_aud_mil", csv_lines)
  data_rx    <- "^[a-z][a-z_0-9]*\\s*,\\s*-?[0-9.]+"
  data_lines <- csv_lines[grepl(data_rx, csv_lines)]
  if (length(header_idx) == 0L || length(data_lines) == 0L) {
    sbm_warn(sprintf(
      "sbm_extract_llm: no parseable CSV found in LLM response for %s",
      document_id
    ))
    return(invisible(NULL))
  }
  header_line <- csv_lines[header_idx[[1L]]]
  writeLines(c(header_line, data_lines), out_path)

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
