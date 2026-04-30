# LLMLineListMSFApp

LLMLineListMSFApp is a standalone Shiny application for turning outbreak narratives into structured line lists with large language models. This repository is the app-focused extraction from a larger research and validation codebase; the benchmarking, manuscript, and other project-specific components are intentionally out of scope here.

The app is designed for interactive use. You can paste a narrative directly into the interface or upload a document, choose a provider and model, select how the system prompt should be handled, and review the extracted line list, metadata, and relationship network in the browser.

## What the app does

- Accepts pasted text or uploaded `.txt`, `.md`, `.docx`, and `.pdf` outbreak narratives.
- Uses the shared extraction pipeline from the original project rather than a separate demo-only code path.
- Supports multiple providers from the app UI, including Azure OpenAI, Claude, MLX, and Ollama.
- Lets you use the built-in validated prompt, auto-generate a prompt from the current document, or upload your own prompt file.
- Includes optional post-extraction deduplication.
- Ships with bundled demo narratives and line lists in `data/demos`.

By default, the app opens with the 12-case demo narrative preloaded.

## Repository layout

```text
.
├── data/
│   ├── demos/                  # bundled demo narratives, line lists, and metadata
│   └── system_prompts/         # built-in prompt files
├── scripts/
│   ├── core/                   # shared extraction and utility functions
│   ├── shiny_apps/
│   │   ├── app.R               # current Shiny app entry point
│   │   └── shiny_app.R         # legacy app kept for reference
│   └── mlx_generate.py         # local MLX helper script
├── outputs/                    # generated demo artefacts and app outputs
├── LLMLineList.Rproj
└── README.md
```

## Run locally

Start the app from the repository root:

```r
shiny::runApp("scripts/shiny_apps")
```

If you prefer to launch it from an R session that already uses `here`, this also works:

```r
shiny::runApp(here::here("scripts", "shiny_apps"))
```

## R packages

The app expects the following R packages to be available:

- `shiny`
- `shinyjs`
- `shinycssloaders`
- `shinythemes`
- `DT`
- `dplyr`
- `tidyr`
- `purrr`
- `stringr`
- `readr`
- `glue`
- `tibble`
- `visNetwork`
- `here`
- `jsonlite`

## Provider setup

The app can call different back ends depending on the provider you select in the UI. The required credentials or local runtime need to be available before processing narratives.

- `Azure OpenAI`: configure the relevant Azure credentials in your environment.
- `Claude`: configure the relevant Anthropic credentials in your environment.
- `MLX`: use a macOS environment with the required local MLX setup.
- `Ollama`: make sure Ollama is installed and the chosen model is available locally.

## Demo data

Bundled demo files live in `data/demos`.

- `synthetic_msf_demo_12_cases_v4_*` is the current 12-case demo set and is the app default.
- `synthetic_msf_demo_84_cases_v1_*` is the larger retained demo set.

These files are included to make it easy to launch the app and test the full extraction flow without needing to source your own narrative first.

## Scope of this repo

This repository is intentionally narrower than the original parent project. It is for the interactive app and the minimum supporting extraction code needed to run it. If you are looking for the historical validation pipeline, benchmarking artefacts, or manuscript workflow, those belong in the older research repo rather than here.

## License

MIT. See `LICENSE`.
