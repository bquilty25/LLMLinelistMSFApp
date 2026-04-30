# AGENTS: Guiding Principles for Data Analysis

This document outlines the agreed-upon guiding principles and tools (the "agents") for this data analysis project. Adhering to these standards ensures our work is **reproducible**, **readable**, **efficient**, and **maintainable**.

The core philosophy is to produce dynamic, report-driven analysis where the narrative, code, and results are seamlessly integrated. Functions are kept separate from the analysis scripts to promote code reuse and clarity.

-----

## Core Principles

  * **Reproducibility First**: Our analysis must be fully reproducible. We will use the **`here`** package to manage file paths, ensuring that the project can be run on any machine without modification. We avoid absolute paths and `setwd()`.
  * **Vectorised Operations**: We favour **vectorised operations** over loops (`for`, `while`) wherever possible. This approach is more idiomatic to R, significantly faster, and often results in cleaner, more readable code. The `tidyverse` suite of packages is our primary tool for achieving this.
  * **Modularity**: Analysis scripts should focus on the "what" and "why". The "how" is encapsulated in functions. All custom functions will be stored in separate scripts within the `scripts/` directory and sourced into analysis files as needed. This prevents code duplication and makes the analysis easier to follow.
  * **Clarity and Style**: We use the `tidyverse` style guide for code formatting. This includes using `snake_case` for variable and function names and leveraging the native R pipe `|>` for sequential operations.
  * **Efficient Data Storage**: Intermediate or processed data objects will be saved using the **`qs`** package. Its high-speed serialisation and compression make it superior to `.Rds` for managing large datasets during the analysis workflow.

-----

## The Agents: Key Packages & Tools

Our analysis relies on a small, powerful set of tools.

### `tidyverse`

The `tidyverse` is a collection of R packages designed for data science that share an underlying design philosophy, grammar, and data structures. It's our primary tool for day-to-day data manipulation and visualisation.

  * **`dplyr`**: For data manipulation (e.g., `mutate()`, `filter()`, `summarise()`).
  * **`ggplot2`**: For declarative and powerful data visualisation.
  * **`readr`**: For fast and reliable reading of flat files (e.g., `.csv`).
  * **`purrr`**: For functional programming and iteration, providing a vectorised alternative to many loop-based problems.

### `here`

The `here` package solves the problem of file path management. It creates paths relative to the project's root directory (identified by the `.Rproj` file), making our code portable and robust.

  * **Usage**: Instead of `read_csv("data/my_data.csv")`, we use `read_csv(here::here("data", "my_data.csv"))`.

### `qs`

The `qs` package provides a mechanism for quickly saving and reading R objects. It is significantly faster than the built-in `saveRDS()` and is our standard for saving cleaned data or computationally expensive intermediate results.

  * **Usage**:
    ```r
    # Save an object
    qs::qsave(my_large_dataframe, here::here("data", "processed", "cleaned_data.qs"))

    # Load an object
    my_large_dataframe <- qs::qread(here::here("data", "processed", "cleaned_data.qs"))
    ```

### `quarto`

Quarto is our tool for creating dynamic reports and documents. It allows us to weave narrative text (written in Markdown) with live R code chunks and their outputs (plots, tables, etc.). This ensures our final report is directly tied to the analysis that produced it.

  * **Inline Code**: We make extensive use of inline code to dynamically insert results directly into the text. For example, to report the number of observations, we would write:
      * **Markdown Text**: The dataset contains `` `r prettyNum(nrow(my_data), big.mark = ",")` `` observations.
      * **Rendered Output**: The dataset contains 1,234 observations.

When writing Quarto documents, use concise, scientific paragraphs and prose rather than bullet points and lists, like a scientific journal article that would be published in a prestigious journal like Science or Nature with a a structure of Abstract, Introduction, Methods, Results, Discussion (+ conclusion paragraph). Use UK English. Write with a sharp, analytical voice that combines intellectual depth with conversational directness. Use a confident first-person perspective that fearlessly dissects cultural phenomena. Blend academic-level insights with more casual language, creating a style that's both intellectually rigorous and immediately accessible. When writing, the results section should simply state the results; any more complex interpretation should be left for the Discussion.


-----

## Project Structure

To maintain consistency, we will use the following directory structure:

```
.
├── my_project.Rproj
├── AGENTS.md
├── data/
│   ├── raw/
│   │   └── source_data.csv
│   └── processed/
│       └── cleaned_data.qs
├── scripts/
│   ├── 00_run_pipeline.R
│   ├── 01_load_data.R
│   └── functions/
│       └── custom_functions.R
└── outputs/
    ├── validation_pipeline_run/
    │   └── run_<YYYYMMDD_HHMMSS>/
    │       ├── <ConfigName>/
    │       │   └── llm_results_rep<NN>.qs
    │       ├── logs/
    │       │   └── <provider>_<model>_raw_output_<timestamp>.txt
    │       ├── all_variable_metrics.csv
    │       ├── aggregated_variable_metrics.csv
    │       ├── per_case_list_metrics.csv
    │       ├── full_replicate_metrics.csv
    │       ├── aggregated_validation_summary.csv
    │       └── model_comparison_side_by_side.xlsx
    ├── plots/
    │   └── distribution_plot.png
    ├── archive/
    │   └── (legacy files from old runs)
    └── tables/
        └── summary_table.csv
```

**Output conventions:**

- All validation pipeline outputs land inside a timestamped run folder (`outputs/validation_pipeline_run/run_<timestamp>/`). Raw LLM text responses go into the `logs/` subfolder within that run folder — never directly in `outputs/`.
- `full_replicate_metrics.csv` is the authoritative per-replicate metrics file; there is no separate "legacy" copy.
- The convenience file `outputs/msf_llm_linelist_latest.csv` is the only file written directly to `outputs/` during normal pipeline runs.
- Intermediate document artefacts (XML, Markdown) extracted from DOCX files are cached in `data/msf_data/modified/`.
- All paths in scripts must use `here::here()` — never bare relative strings or `setwd()`.

-----
 
 ## File Naming and Orchestration
 
 To ensure our analysis pipeline is clear and executable in the correct order, we adhere to the following conventions:
 
   * **Numbered Scripts**: Analysis and processing scripts should be prefixed with a number (e.g., `01_load_data.R`, `02_clean_data.R`, `03_run_model.R`) to indicate their execution order.
   * **Orchestrator Script**: For complex pipelines involving multiple steps, create a master "orchestrator" script (often named `00_run_pipeline.R` or similar) that sources the individual numbered scripts in sequence. This provides a single entry point to run the entire analysis from start to finish.
 
 -----

## Workflow Example

A typical analysis workflow will follow these steps:

1.  **Setup**: In a `.qmd` file, the first code chunk loads necessary packages and sources our custom functions from the `scripts/` directory.

    ```r
    # Load packages
    library(tidyverse)
    library(here)

    # Source custom functions
    source(here::here("scripts", "functions", "01_data_cleaning_functions.R"))
    ```

2.  **Data Import**: Load the raw data using `here` to define the path.

3.  **Processing**: Use functions from our sourced scripts and `dplyr` verbs to clean and prepare the data. Save the processed data using `qs::qsave()`.

4.  **Analysis**: Perform the analysis using vectorised `dplyr` and `purrr` functions.

5.  **Reporting**: Present the results within the Quarto document, using `ggplot2` for plots and inline code `` `r ...` `` to embed key figures directly into the explanatory text.