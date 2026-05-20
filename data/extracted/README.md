# `data/extracted/` --- extracted fact CSVs

This directory holds structured fact CSVs extracted from Budget
Papers. Files here are **committed to the repository** and are what
the production pipeline reads to populate `fiscal_facts`.

The production CI pipeline never calls an LLM. Two paths populate
this directory:

1. **Rule-based extraction** --- per-jurisdiction parsers under
   `R/extract/extract_<juris>.R` that use `tabulapdf` to parse a
   downloaded PDF. Run as part of `targets::tar_make()`, but only on
   developer machines that have a working JDK (for `rJava`).
2. **LLM-assisted extraction** --- the dev-only helper
   `sbm_extract_llm()` in `R/extract/extract_llm.R`. Requires an
   `ANTHROPIC_API_KEY` and `cfg$llm$enabled_locally = TRUE`. Runs on
   a developer machine; commit the resulting CSVs.

Either path produces a CSV per Budget document, named
`<document_id>.csv` and stored under `data/extracted/<lower(juris)>/`.

## File naming

```
data/extracted/nsw/NSW_2024-25_budget.csv
data/extracted/nsw/NSW_2024-25_myefo.csv
data/extracted/vic/VIC_2024-25_budget.csv
...
```

`<document_id>` matches the `document_id` column in
`inst/document_registry.csv`.

## CSV schema (contract)

| Column                | Type    | Required | Meaning                                                                  |
|-----------------------|---------|----------|--------------------------------------------------------------------------|
| `variable_id`         | string  | yes      | Canonical variable id from `inst/variable_dictionary.csv`.               |
| `value_aud_mil`       | numeric | yes      | Value in AUD millions.                                                   |
| `fiscal_year`         | string  | no       | FY label (e.g. `"2024-25"`). Defaults to the document's `fiscal_year`.   |
| `is_forward_estimate` | boolean | yes      | TRUE if FY > document's FY (forward estimate), FALSE otherwise.          |
| `source_line_item`    | string  | no       | The source line-item text from the PDF, for traceability.                |
| `notes`               | string  | no       | Free-text caveats (e.g. "value imputed from prior year due to redaction").|

Anything else in the CSV is preserved as additional columns but
ignored by the framework.

## Manual overrides vs. extracted CSVs

If you need to correct a specific fact after extraction, **do not edit
the extracted CSV** (which may be regenerated). Add a row to
`data/manual_overrides/overrides.csv` instead --- those take
precedence in the warehouse build and are individually documented.

## When extraction returns nothing

If `data/extracted/<juris>/<document_id>.csv` does not exist or is
empty, the pipeline skips that document silently and logs it in the
build output. Coverage gaps surface in `STATUS.md`.
