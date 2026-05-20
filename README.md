# StateBudgetMonitor

Interactive view of fiscal positions across all eight Australian states and
territories, built by a reproducible R pipeline from Budget Papers, MYEFOs,
and Final Budget Outcomes.

* **Live site:** <https://DavidAStephan.github.io/StateBudgetMonitor/>
* **Updated:** weekly, Mondays 06:00 UTC, via GitHub Actions.
* **Sibling project:** [NOM_Nowcast](https://github.com/DavidAStephan/NOM_Nowcast)
  (same engineering conventions).

## What it does

1. Maintains a registry of every Australian state and territory Budget Paper,
   MYEFO, and Final Budget Outcome from FY2014-15 onwards (~265 documents).
2. Downloads each document and extracts the General Government Sector
   forward-estimates table, rule-based first via `tabulapdf` with an
   LLM-assisted fallback that runs only on a developer machine.
3. Harmonises line items across jurisdictions against the ABS Government
   Finance Statistics chart of accounts.
4. Stores every fact in a DuckDB warehouse keyed on its source document, so
   that any past "as-at" view of the data can be reconstructed.
5. Renders the dashboard with Quarto, fully static, deployed to GitHub Pages.

## Quick start

```r
# 1. install pinned dependencies
install.packages("renv")
renv::restore()

# 2. run the pipeline end-to-end
targets::tar_make()

# 3. render the site
quarto::quarto_render()

# 4. open it
browseURL("_site/index.html")
```

## Pipeline architecture

```
                 +-----------------+
                 | document        |
                 | registry (CSV)  |
                 +--------+--------+
                          |
       +------------------+-------------------+
       |                                      |
       v                                      v
 +-----------+                          +-----------+
 | downloads | ----> data/raw/         | ABS GFS / |
 | (httr2)   |       (gitignored)      | pop / GSP |
 +-----+-----+                          +-----+-----+
       |                                      |
       v                                      |
 +-----------+   per-state    +----------+    |
 | extract/  | -------------> | extracted|    |
 | (tabulapdf|                | CSV      |    |
 |  + ellmer)|                |(committed|    |
 +-----------+                +-----+----+    |
                                    |         |
                                    v         v
                              +-----------+   |
                              | harmonise +<--+
                              | (variable_|
                              | dict)     |
                              +-----+-----+
                                    |
                                    v
                              +-----------+   manual_overrides/
                              | warehouse | <-- overrides.csv
                              | (DuckDB)  |
                              +-----+-----+
                                    |
                       +------------+-----------+
                       v            v           v
                  +---------+ +---------+ +---------+
                  | analytics| | reports/| | tests/  |
                  | (per cap,| | (Quarto)| | testthat|
                  | revisions|+---------+ +---------+
                  | etc.)    |
                  +---------+
```

The LLM-assisted extraction step **only runs on a developer machine**; its
structured outputs are committed to `data/extracted/`, and the production
CI pipeline reads from those CSVs. CI never calls an LLM.

## Project structure

```
StateBudgetMonitor/
+-- DESCRIPTION                     # R package metadata
+-- NAMESPACE                       # generated from roxygen
+-- _targets.R                      # pipeline DAG
+-- config.yml                      # all parameters
+-- _quarto.yml                     # site configuration
+-- custom.scss                     # site theme overrides
+-- R/
|   +-- utils/                      # dates, logging, http, jurisdictions
|   +-- ingest/                     # registry, downloads, ABS fetchers
|   +-- extract/                    # per-state PDF extractors (Phases 2-3)
|   +-- harmonise/                  # variable dictionary, chart of accounts
|   +-- warehouse/                  # DuckDB schema, vintage store
|   +-- analytics/                  # aggregates, per-capita, revisions
|   +-- viz/                        # plotly theme, time-series, comparison
+-- inst/
|   +-- document_registry.csv       # all known Budget/MYEFO/Outcome docs
|   +-- variable_dictionary.csv     # canonical_id x jurisdiction mapping
+-- data/
|   +-- raw/                        # downloaded PDFs (gitignored)
|   +-- extracted/                  # LLM-assisted CSV outputs (committed)
|   +-- manual_overrides/           # exceptions (committed)
+-- reports/                        # Quarto .qmd files
|   +-- state_pages/                # one .qmd per jurisdiction
+-- tests/testthat/                 # unit tests
+-- .github/workflows/render.yml    # CI: pipeline + render + deploy
+-- renv.lock                       # pinned dependency manifest
+-- .Rprofile                       # renv activation + R options
+-- README.md
+-- STATUS.md
```

## Deploy your own fork

1. Fork the repository on GitHub.
2. **Settings -> Pages**: set Source to "GitHub Actions".
3. **Settings -> Actions -> General -> Workflow permissions**: enable
   "Read and write permissions".
4. Update `site-url` and `repo-url` in `_quarto.yml` and the URLs in
   `.github/workflows/render.yml` if you change the repo name.
5. Push to `main`. The first run takes ~10-15 minutes; subsequent runs
   complete in ~5 minutes thanks to the package cache.

R packages install from the Posit Public Package Manager Linux binaries, so
no source compilation is required in CI.

## LLM-assisted extraction (developer-only)

For documents where rule-based extraction fails, set the
`ANTHROPIC_API_KEY` environment variable locally and run the relevant
extractor manually. The outputs are committed under `data/extracted/`.
The production CI pipeline reads those committed CSVs and **never** calls
an LLM itself --- this keeps production purely R plus structured public
data, which matters for institutional deployment.

## Contributing

* Follow the tidyverse style guide: `styler::style_pkg()` and
  `lintr::lint_package()` before commits.
* Use roxygen2 with `Roxygen: list(markdown = TRUE)` for all exported
  functions; regenerate `NAMESPACE` with `devtools::document()`.
* Add tests under `tests/testthat/` for every non-trivial function.
* Run `targets::tar_make()` locally before pushing to confirm the
  pipeline still flows end-to-end.

## License

MIT --- see [LICENSE.md](LICENSE.md).
