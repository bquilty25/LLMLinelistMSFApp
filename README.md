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

## Provider setup

The app and runtime helpers read API credentials from environment variables. The easiest way to set these is with a project-level `.Renviron` file, which R loads automatically on startup.

1. Copy the example template and open it in a text editor:

```sh
cp .Renviron.example .Renviron
```

2. Fill in the credentials for the provider(s) you want to use (you only need the variables for the providers you are actually using):

| Provider | Required variables |
|---|---|
| Azure OpenAI | `AZURE_OPENAI_ENDPOINT`, `AZURE_OPENAI_API_KEY` |
| Claude | `CLAUDE_API_KEY` |
| OpenAI | `OPENAI_API_KEY` |
| Gemini | `GEMINI_API_KEY` |
| MLX / Ollama (local) | See [Local model setup](#local-model-setup) below |

3. Restart R so the new variables are picked up:

```r
# In RStudio: Session > Restart R
# Or from the terminal:
# Rscript -e "Sys.getenv('AZURE_OPENAI_ENDPOINT')"  # verify it loaded
```

> `.Renviron` is listed in `.gitignore` — never commit real credentials.

## Run the app

```r
LLMLineListMSFApp::run_llm_linelist_app()
```

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
├── vignettes/                 # getting-started vignette
└── .Renviron.example          # credentials template
```

## Validation

The package is validated through `testthat`, `R CMD build`, and `R CMD check --no-manual`.

## Local model setup

### MLX (Apple Silicon)

MLX runs models locally on Apple Silicon. It requires a Python environment with `mlx-lm` installed:

```sh
python3 -m venv ~/mlx-env
~/mlx-env/bin/pip install mlx-lm
```

Optionally set `LLMLINELIST_MLX_PYTHON_BIN` in your `.Renviron` if your environment is elsewhere (defaults to `~/mlx-env/bin/python3`).

### Ollama

Install Ollama from [ollama.com](https://ollama.com), start the server, and pull a model before use:

```sh
ollama serve &
ollama pull qwen3.6:latest
```

No credentials are needed — the app connects to `localhost:11434` automatically.

## License

MIT. See `LICENSE` and `LICENSE.md`.
