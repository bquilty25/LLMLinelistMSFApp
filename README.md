# LLMLineListMSFApp

LLMLineListMSFApp is an installable R package for extracting outbreak line lists from free-text narratives with large language models. It packages the interactive Shiny workflow, provider-facing runtime helpers, prompt utilities, bundled demo assets, and the comparison and metrics functions used to evaluate extraction quality.

## What it includes

- A packaged Shiny app launched with `LLMLineListMSFApp::run_llm_linelist_app()`.
- Extraction helpers for document ingestion, prompt construction, provider calls, structured parsing, and optional deduplication.
- Evaluation helpers for line list comparison and validation metrics.
- Bundled demo and prompt assets exposed through helper functions rather than repo-relative paths.

## Install

Quick local install from the repository root:

```sh
R CMD INSTALL .
```

If you want installed vignette access via `vignette("getting-started", package = "LLMLineListMSFApp")`, build and install the source tarball instead:

```sh
R CMD build .
R CMD INSTALL LLMLineListMSFApp_0.0.0.9000.tar.gz
```

For development workflows, load the package without installing it:

```r
pkgload::load_all(".")
```

## Run the app

Installed package entrypoint:

```r
LLMLineListMSFApp::run_llm_linelist_app()
```

Repository compatibility launcher:

```r
shiny::runApp("scripts/shiny_apps")
```

The compatibility launcher delegates to the packaged app. It exists so older repo-local workflows do not keep depending on sourced scripts under `scripts/core`.

## Use from R

The package does not require the Shiny app. You can run the extraction workflow directly from an R session.

Example using bundled demo text:

```r
library(LLMLineListMSFApp)
library(readr)

document_text <- read_file(llmlinelist_demo_path())

result <- process_document(
	document_content = document_text,
	provider = "azure",
	model = "gpt-5"
)

linelist <- result$linelist
```

Example starting from a `.docx` file:

```r
library(LLMLineListMSFApp)

document_text <- extract_docx_markdown("report.docx")

result <- process_document(
	document_content = document_text,
	content_type = "markdown",
	provider = "ollama",
	model = "llama3.3:latest"
)

linelist <- consolidate_duplicates_llm(result$linelist)
```

For a fuller walkthrough, see `vignette("getting-started", package = "LLMLineListMSFApp")` after installing the built package tarball.

## Core package surface

Extraction workflow:

- `extract_docx_markdown()`
- `create_system_prompt()`
- `llm_call_tidy()`
- `extract_structured_data()`
- `process_document()`
- `generate_extraction_prompt()`
- `consolidate_duplicates_llm()`
- `call_mlx_llm()`

Comparison and metrics:

- `compare_linelists()`
- `compute_case_metrics()`
- `compute_variable_metrics()`
- `compute_per_case_list_metrics()`
- `create_metrics_row()`
- `aggregate_replicate_metrics()`

Bundled assets:

- `llmlinelist_demo_path()`
- `llmlinelist_system_prompt_path()`

## Provider setup

The packaged app currently exposes Azure OpenAI, Claude, MLX, and Ollama. The lower-level runtime helper `llm_call_tidy()` also supports OpenAI and Gemini.

- Azure OpenAI: set `AZURE_OPENAI_ENDPOINT` and `AZURE_OPENAI_API_KEY`. Optionally set `AZURE_OPENAI_API_VERSION`.
- Claude: set `CLAUDE_API_KEY`.
- OpenAI: set `OPENAI_API_KEY` if using the runtime helper directly.
- Gemini: set `GEMINI_API_KEY` if using the runtime helper directly.
- MLX: ensure the local Python environment and MLX dependencies are available. The packaged helper script lives under `inst/scripts/mlx_generate.py`.
- Ollama: ensure the local Ollama server is running and the selected model is available.

## Repository layout

```text
.
├── R/                         # package functions
├── inst/
│   ├── apps/shiny_app/        # packaged Shiny app
│   ├── extdata/               # bundled demo and prompt assets
│   └── scripts/               # packaged MLX helper
├── man/                       # generated documentation
├── tests/testthat/            # package tests
└── scripts/shiny_apps/        # compatibility launchers for older repo workflows
```

## Validation

The package is validated through `testthat`, `R CMD build`, and `R CMD check --no-manual`.

## License

MIT. See `LICENSE` and `LICENSE.md`.
