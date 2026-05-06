suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinycssloaders)
  library(shinythemes)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(glue)
  library(tibble)
  library(visNetwork)
  library(jsonlite)
  library(rlang)
  library(LLMLineListMSFApp)
})

default_system_prompt_path <- llmlinelist_system_prompt_path()
default_demo_narrative_path <- llmlinelist_demo_path()

provider_model_catalog <- list(
  azure = c("gpt-5", "gpt-5-nano"),
  claude = c("claude-sonnet-4-6", "claude-haiku-4-5"),
  mlx = c(
    "mlx-community/Qwen3.5-9B-MLX-4bit",
    "mlx-community/Qwen3.6-27B-4bit",
    "mlx-community/Qwen3.6-35B-A3B-4bit",
    "mlx-community/gemma-4-e4b-it-4bit",
    "mlx-community/gemma-4-26b-a4b-it-4bit",
    "mlx-community/gemma-4-31b-it-4bit"
  ),
  ollama = c("llama3.3:latest")
)

provider_defaults <- vapply(provider_model_catalog, `[[`, character(1), 1)
app_max_response_tokens <- 32768
app_temperature <- 0.1

example_text <- if (file.exists(default_demo_narrative_path)) {
  readr::read_file(default_demo_narrative_path)
} else {
  paste(
    "Case EPI-001 concerns a 34-year-old female market trader from North District who developed fever, vomiting and weakness on 14 March 2026.",
    "She was admitted to Central Hospital on 16 March and was later classified as a probable case.",
    "Her husband EPI-002 had cared for her at home and developed symptoms on 18 March.",
    "A neighbour, EPI-003, attended the same burial ceremony and was listed as a contact.",
    "A nurse, EPI-004, reported unprotected exposure during triage on 15 March but remained asymptomatic at the time of reporting.",
    sep = "\n\n"
  )
}

trim_or_null <- function(value) {
  value <- trimws(value %||% "")
  if (!nzchar(value)) {
    return(NULL)
  }
  value
}

parse_reference_ids <- function(value) {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(character(0))
  }

  if (is.list(value)) {
    parsed_values <- unlist(value, use.names = FALSE)
    parsed_values <- as.character(parsed_values)
    parsed_values <- trimws(parsed_values)
    parsed_values <- parsed_values[nzchar(parsed_values)]
    parsed_values <- parsed_values[!tolower(parsed_values) %in% c("null", "na", "none")]
    return(unique(parsed_values))
  }

  value_chr <- trimws(as.character(value[[1]]))

  if (!nzchar(value_chr) || tolower(value_chr) %in% c("null", "na", "none", "[]")) {
    return(character(0))
  }

  if (str_detect(value_chr, "^\\s*\\[.*\\]\\s*$")) {
    parsed_json <- tryCatch(
      jsonlite::fromJSON(value_chr),
      error = function(e) NULL
    )

    if (!is.null(parsed_json)) {
      return(parse_reference_ids(parsed_json))
    }
  }

  parsed_values <- str_split(value_chr, "\\s*[,;\\n|]+\\s*")[[1]]
  parsed_values <- str_replace_all(parsed_values, '^"|"$', "")
  parsed_values <- trimws(parsed_values)
  parsed_values <- parsed_values[nzchar(parsed_values)]
  parsed_values <- parsed_values[!tolower(parsed_values) %in% c("null", "na", "none")]

  unique(parsed_values)
}

build_relationship_table <- function(linelist) {
  if (is.null(linelist) || nrow(linelist) == 0 || !"case_id" %in% names(linelist)) {
    return(tibble(
      source_case_id = character(),
      target_case_id = character(),
      relationship_type = character()
    ))
  }

  append_relationships <- function(column_name, relationship_type, reverse = FALSE) {
    if (!column_name %in% names(linelist)) {
      return(tibble())
    }

    purrr::map2_dfr(linelist$case_id, linelist[[column_name]], function(case_id, raw_value) {
      parsed_ids <- parse_reference_ids(raw_value)

      if (length(parsed_ids) == 0) {
        return(tibble())
      }

      if (reverse) {
        tibble(
          source_case_id = parsed_ids,
          target_case_id = case_id,
          relationship_type = relationship_type
        )
      } else {
        tibble(
          source_case_id = case_id,
          target_case_id = parsed_ids,
          relationship_type = relationship_type
        )
      }
    })
  }

  bind_rows(
    append_relationships("contacts", "contact"),
    append_relationships("secondary_cases", "secondary_case"),
    append_relationships("potential_infector", "potential_infector", reverse = TRUE),
    append_relationships("most_probable_infector", "most_probable_infector", reverse = TRUE)
  ) |>
    filter(!is.na(.data$source_case_id), !is.na(.data$target_case_id)) |>
    mutate(
      source_case_id = as.character(.data$source_case_id),
      target_case_id = as.character(.data$target_case_id)
    ) |>
    filter(.data$source_case_id != "", .data$target_case_id != "") |>
    distinct() |>
    arrange(.data$relationship_type, .data$source_case_id, .data$target_case_id)
}

build_node_table <- function(linelist, relationships) {
  linelist_tbl <- if (is.null(linelist) || nrow(linelist) == 0) tibble() else as_tibble(linelist)
  relationship_ids <- unique(c(relationships$source_case_id, relationships$target_case_id))
  node_ids <- unique(c(linelist_tbl$case_id, relationship_ids))

  if (length(node_ids) == 0) {
    return(tibble(id = character(), label = character()))
  }

  tibble(id = node_ids) |>
    left_join(linelist_tbl, by = c("id" = "case_id")) |>
    mutate(
      label = coalesce(.data$name, .data$id),
      sex_group = case_when(
        is.na(.data$sex) ~ "Unknown",
        str_to_lower(.data$sex) %in% c("f", "female") ~ "Female",
        str_to_lower(.data$sex) %in% c("m", "male") ~ "Male",
        TRUE ~ "Unknown"
      ),
      referenced_only = is.na(.data$name) & is.na(.data$sex) & is.na(.data$outcome),
      group = case_when(
        .data$referenced_only ~ "Referenced only",
        TRUE ~ .data$sex_group
      ),
      color.background = case_when(
        .data$group == "Female" ~ "#d95f5f",
        .data$group == "Male" ~ "#2c7fb8",
        .data$group == "Referenced only" ~ "#bdbdbd",
        TRUE ~ "#756bb1"
      ),
      color.border = "#ffffff",
      title = glue(
        "<b>{.data$id}</b><br>",
        "Name: {coalesce(.data$name, 'Unknown')}<br>",
        "Sex: {coalesce(.data$sex, 'Unknown')}<br>",
        "Age: {coalesce(as.character(.data$age), 'Unknown')}<br>",
        "Outcome: {coalesce(.data$outcome, 'Unknown')}"
      ) |> as.character()
    ) |>
    select("id", "label", "group", "title", "color.background", "color.border")
}

build_edge_table <- function(relationships) {
  required_columns <- c("source_case_id", "target_case_id", "relationship_type")

  if (is.null(relationships) || nrow(relationships) == 0 || !all(required_columns %in% names(relationships))) {
    return(tibble(
      from = character(),
      to = character(),
      label = character(),
      title = character(),
      dashes = logical(),
      arrows = character(),
      color = character()
    ))
  }

  relationships |>
    transmute(
      from = .data$source_case_id,
      to = .data$target_case_id,
      label = .data$relationship_type,
      title = .data$relationship_type,
      dashes = .data$relationship_type == "contact",
      arrows = case_when(
        .data$relationship_type == "contact" ~ "",
        TRUE ~ "to"
      ),
      color = case_when(
        .data$relationship_type == "secondary_case" ~ "#1b9e77",
        .data$relationship_type == "most_probable_infector" ~ "#d95f02",
        .data$relationship_type == "potential_infector" ~ "#7570b3",
        TRUE ~ "#636363"
      )
    )
}

with_external_provider_approval <- function(provider, expr) {
  external_provider <- provider %in% c("claude", "openai", "gemini")
  original_value <- Sys.getenv("LLMLINELIST_ALLOW_EXTERNAL_PROVIDER", unset = "")

  if (external_provider) {
    Sys.setenv(LLMLINELIST_ALLOW_EXTERNAL_PROVIDER = "true")
  }

  on.exit(
    {
      if (external_provider) {
        if (nzchar(original_value)) {
          Sys.setenv(LLMLINELIST_ALLOW_EXTERNAL_PROVIDER = original_value)
        } else {
          Sys.unsetenv("LLMLINELIST_ALLOW_EXTERNAL_PROVIDER")
        }
      }
    },
    add = TRUE
  )

  force(expr)
}

read_document_content <- function(uploaded_file) {
  ext <- tolower(tools::file_ext(uploaded_file$name))

  content <- switch(ext,
    txt = readr::read_file(uploaded_file$datapath),
    md = readr::read_file(uploaded_file$datapath),
    docx = extract_docx_markdown(uploaded_file$datapath),
    pdf = {
      if (!requireNamespace("pdftools", quietly = TRUE)) {
        rlang::abort("PDF processing requires the pdftools package.")
      }
      paste(pdftools::pdf_text(uploaded_file$datapath), collapse = "\n")
    },
    rlang::abort("Unsupported file type. Use .txt, .md, .docx or .pdf.")
  )

  list(
    content = content,
    content_type = if (ext %in% c("md", "docx")) "markdown" else "text",
    document_label = uploaded_file$name,
    document_path = uploaded_file$datapath
  )
}

prepare_document_payload <- function(input_mode, input_text, uploaded_file) {
  if (identical(input_mode, "upload")) {
    if (is.null(uploaded_file)) {
      rlang::abort("Upload a document before processing.")
    }
    return(read_document_content(uploaded_file))
  }

  input_text <- trimws(input_text %||% "")
  if (!nzchar(input_text)) {
    rlang::abort("Paste an outbreak narrative before processing.")
  }

  temp_document_path <- tempfile(pattern = "shiny_outbreak_", fileext = ".txt")
  readr::write_file(input_text, temp_document_path)

  list(
    content = input_text,
    content_type = "text",
    document_label = "Pasted text",
    document_path = temp_document_path
  )
}

resolve_system_prompt <- function(prompt_mode, prompt_file, document_payload,
                                  provider, model, max_sample_words) {
  if (identical(prompt_mode, "default")) {
    if (!file.exists(default_system_prompt_path)) {
      rlang::abort(glue("Built-in system prompt not found: {default_system_prompt_path}"))
    }
    return(default_system_prompt_path)
  }

  if (identical(prompt_mode, "upload")) {
    if (is.null(prompt_file)) {
      rlang::abort("Upload a system prompt file before processing.")
    }
    return(prompt_file$datapath)
  }

  auto_prompt_path <- tempfile(pattern = "shiny_system_prompt_", fileext = ".txt")
  with_external_provider_approval(
    provider,
    generate_extraction_prompt(
      document_path = document_payload$document_path,
      output_path = auto_prompt_path,
      provider = provider,
      model = model,
      max_sample_words = max_sample_words,
      force_regeneration = TRUE
    )
  )
}

run_shiny_extraction <- function(input_mode,
                                 input_text,
                                 uploaded_file,
                                 prompt_mode,
                                 prompt_file,
                                 provider,
                                 model,
                                 deduplicate,
                                 max_sample_words,
                                 prompt_context) {
  document_payload <- prepare_document_payload(input_mode, input_text, uploaded_file)
  resolved_model <- model
  system_prompt_path <- resolve_system_prompt(
    prompt_mode = prompt_mode,
    prompt_file = prompt_file,
    document_payload = document_payload,
    provider = provider,
    model = resolved_model,
    max_sample_words = max_sample_words
  )

  started_at <- Sys.time()

  extraction_result <- with_external_provider_approval(
    provider,
    process_document(
      document_content = document_payload$content,
      content_type = document_payload$content_type,
      max_tokens = app_max_response_tokens,
      provider = provider,
      model = resolved_model,
      temperature = app_temperature,
      few_shot_examples = trim_or_null(prompt_context),
      system_prompt_path = system_prompt_path
    )
  )

  linelist <- extraction_result$linelist |>
    as_tibble()

  if (deduplicate && nrow(linelist) > 0) {
    linelist <- with_external_provider_approval(
      provider,
      consolidate_duplicates_llm(
        linelist_data = linelist,
        provider = provider,
        model = resolved_model
      )
    ) |>
      as_tibble()
  }

  relationships <- build_relationship_table(linelist)
  processing_time <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  prompt_text <- readr::read_file(system_prompt_path)

  metadata <- tibble(
    metric = c(
      "document",
      "content_type",
      "provider",
      "model",
      "system_prompt",
      "deduplication",
      "cases_extracted",
      "relationships_extracted",
      "processing_time_sec"
    ),
    value = c(
      document_payload$document_label,
      document_payload$content_type,
      provider,
      resolved_model %||% provider_defaults[[provider]],
      basename(system_prompt_path),
      ifelse(deduplicate, "enabled", "disabled"),
      nrow(linelist),
      nrow(relationships),
      sprintf("%.1f", processing_time)
    )
  )

  list(
    linelist = linelist,
    relationships = relationships,
    raw_response = extraction_result$raw_response,
    metadata = metadata,
    prompt_text = prompt_text,
    prompt_path = system_prompt_path,
    processing_time = processing_time
  )
}

ui <- fluidPage(
  useShinyjs(),
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$style(HTML(
      "
      body {
        margin-left: 4%;
        margin-right: 4%;
        background-color: #f6f8fb;
      }
      .header-section {
        background: white;
        border-radius: 16px;
        padding: 28px;
        margin-bottom: 24px;
        box-shadow: 0 8px 24px rgba(22, 32, 72, 0.08);
      }
      .section-card {
        background: white;
        border-radius: 16px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 8px 24px rgba(22, 32, 72, 0.08);
      }
      .well {
        background: white !important;
        border: none;
        border-radius: 16px;
        box-shadow: 0 8px 24px rgba(22, 32, 72, 0.08);
      }
      .btn-primary {
        border: none;
        border-radius: 999px;
        padding: 10px 18px;
        background-color: #1f6feb;
      }
      .nav-pills > li > a {
        border-radius: 999px;
      }
      .metric-card {
        background: linear-gradient(135deg, #f8fbff 0%, #eef4ff 100%);
        border-radius: 14px;
        padding: 16px 18px;
        min-height: 108px;
        margin-bottom: 16px;
        border: 1px solid #d9e4ff;
      }
      .metric-label {
        color: #526075;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
      }
      .metric-value {
        color: #102a43;
        font-size: 28px;
        font-weight: 700;
        margin-top: 8px;
      }
      .metric-subtext {
        color: #526075;
        font-size: 13px;
        margin-top: 8px;
      }
      .help-copy {
        color: #526075;
        font-size: 13px;
      }
      .download-actions {
        display: flex;
        gap: 16px;
        align-items: stretch;
        justify-content: center;
        flex-wrap: wrap;
      }
      .download-action {
        flex: 0 1 280px;
        width: 280px;
        max-width: 100%;
      }
      .download-action .btn,
      .download-action .download-link {
        width: 100%;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        min-height: 46px;
        gap: 8px;
      }
      #network_plot {
        background: white;
        border-radius: 16px;
      }
      pre.shiny-text-output {
        white-space: pre-wrap;
        word-break: break-word;
      }
      "
    ))
  ),
  div(
    class = "header-section",
    fluidRow(
      column(
        8,
        h1("LLMLinelist"),
        h4("Validated extraction workflow for general outbreak narratives")
      ),
      column(
        4,
        div(
          class = "section-card",
          style = "margin-bottom: 0; padding: 14px 18px;",
          strong("\u26a0\ufe0f Important disclaimer"),
          p(
            class = "help-copy",
            "LLM extraction is not infallible. Always review the output carefully before use. Case counts, dates, identifiers, and epidemiological links should be verified against the source document. This tool is intended to assist — not replace — human review."
          )
        )
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(
        class = "section-card",
        h4("Document"),
        radioButtons(
          "input_mode",
          NULL,
          choices = c("Paste text" = "paste", "Upload file" = "upload"),
          selected = "paste",
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.input_mode === 'paste'",
          textAreaInput(
            "input_text",
            label = NULL,
            rows = 12,
            width = "100%",
            value = example_text,
            placeholder = "Paste an outbreak narrative here..."
          )
        ),
        conditionalPanel(
          condition = "input.input_mode === 'upload'",
          fileInput(
            "uploaded_document",
            "Upload narrative document",
            accept = c(".txt", ".md", ".docx", ".pdf")
          ),
          p(
            class = "help-copy",
            "DOCX files are converted to structured markdown before extraction."
          )
        )
      ),
      div(
        class = "section-card",
        h4("Model"),
        selectInput(
          "provider",
          "Provider",
          choices = c(
            "Azure OpenAI" = "azure",
            "Claude" = "claude",
            "MLX (local)" = "mlx",
            "Ollama (local)" = "ollama"
          ),
          selected = "azure"
        ),
        selectInput(
          "model",
          "Model",
          choices = provider_model_catalog[["azure"]],
          selected = unname(provider_defaults[["azure"]])
        )
      ),
      div(
        class = "section-card",
        h4("Prompting"),
        selectInput(
          "prompt_mode",
          "System prompt",
          choices = c(
            "Built-in validated prompt" = "default",
            "Auto-generate from current document" = "auto",
            "Upload prompt file" = "upload"
          ),
          selected = "default"
        ),
        conditionalPanel(
          condition = "input.prompt_mode === 'upload'",
          fileInput(
            "uploaded_prompt",
            "Upload system prompt",
            accept = c(".txt", ".md")
          )
        ),
        conditionalPanel(
          condition = "input.prompt_mode === 'auto'",
          numericInput(
            "max_sample_words",
            "Prompt-generation sample size",
            value = 2000,
            min = 250,
            step = 250
          )
        ),
        textAreaInput(
          "prompt_context",
          "Optional extra extraction guidance or examples",
          rows = 5,
          width = "100%",
          placeholder = "Paste extra instructions, terminology hints, or few-shot examples here if needed."
        ),
        checkboxInput(
          "deduplicate",
          "Run post-extraction deduplication",
          value = FALSE
        )
      ),
      actionButton(
        "process_btn",
        label = "Process narrative",
        width = "100%",
        class = "btn-primary btn-lg"
      )
    ),
    mainPanel(
      width = 8,
      div(
        class = "section-card",
        tabsetPanel(
          id = "results_tabs",
          type = "pills",
          tabPanel(
            "Overview",
            br(),
            uiOutput("summary_cards"),
            DTOutput("metadata_table") %>% withSpinner(type = 8, color = "#1f6feb")
          ),
          tabPanel(
            "Linelist",
            br(),
            DTOutput("linelist_table") %>% withSpinner(type = 8, color = "#1f6feb")
          ),
          tabPanel(
            "Relationships",
            br(),
            DTOutput("relationships_table") %>% withSpinner(type = 8, color = "#1f6feb")
          ),
          tabPanel(
            "Network",
            br(),
            visNetworkOutput("network_plot", height = "720px") %>% withSpinner(type = 8, color = "#1f6feb")
          ),
          tabPanel(
            "Prompt",
            br(),
            verbatimTextOutput("prompt_text")
          ),
          tabPanel(
            "Raw response",
            br(),
            verbatimTextOutput("raw_response")
          )
        )
      ),
      div(
        class = "section-card",
        div(
          class = "download-actions",
          div(
            class = "download-action",
            downloadButton("download_linelist", "Download linelist CSV")
          ),
          div(
            class = "download-action",
            downloadButton("download_relationships", "Download relationships CSV")
          ),
          div(
            class = "download-action",
            downloadButton("download_raw", "Download raw response")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  active_default_model <- reactiveVal(unname(provider_defaults[["azure"]]))

  observeEvent(input$provider,
    {
      next_default <- unname(provider_defaults[[input$provider]])
      updateSelectInput(
        session,
        "model",
        choices = provider_model_catalog[[input$provider]],
        selected = next_default
      )

      active_default_model(next_default)
    },
    ignoreInit = TRUE
  )

  results <- reactiveVal(NULL)

  observeEvent(input$process_btn, {
    shinyjs::disable("process_btn")
    updateActionButton(session, "process_btn", label = "Processing...")

    withProgress(message = "Running extraction...", value = 0.1, {
      outcome <- tryCatch(
        {
          incProgress(0.2, detail = "Preparing document")

          extraction_result <- run_shiny_extraction(
            input_mode = input$input_mode,
            input_text = input$input_text,
            uploaded_file = input$uploaded_document,
            prompt_mode = input$prompt_mode,
            prompt_file = input$uploaded_prompt,
            provider = input$provider,
            model = input$model,
            deduplicate = isTRUE(input$deduplicate),
            max_sample_words = input$max_sample_words %||% 2000,
            prompt_context = input$prompt_context
          )

          incProgress(0.8, detail = "Building outputs")
          results(extraction_result)

          showNotification(
            glue(
              "Extraction complete: {nrow(extraction_result$linelist)} cases, {nrow(extraction_result$relationships)} relationships"
            ),
            type = "message"
          )

          TRUE
        },
        error = function(e) {
          showNotification(e$message, type = "error", duration = NULL)
          FALSE
        },
        finally = {
          shinyjs::enable("process_btn")
          updateActionButton(session, "process_btn", label = "Process narrative")
        }
      )

      if (isTRUE(outcome)) {
        updateTabsetPanel(session, "results_tabs", selected = "Overview")
      }
    })
  })

  output$summary_cards <- renderUI({
    req(results())
    result <- results()
    linelist <- result$linelist
    relationships <- result$relationships

    fluidRow(
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-label", "Cases extracted"),
          div(class = "metric-value", nrow(linelist)),
          div(class = "metric-subtext", "Rows in the final linelist")
        )
      ),
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-label", "Relationships"),
          div(class = "metric-value", nrow(relationships)),
          div(class = "metric-subtext", "Contacts and transmission links")
        )
      ),
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-label", "Processing time"),
          div(class = "metric-value", sprintf("%.1fs", result$processing_time)),
          div(class = "metric-subtext", basename(result$prompt_path))
        )
      )
    )
  })

  output$metadata_table <- renderDT({
    req(results())
    datatable(
      results()$metadata,
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(results()$metadata), ordering = FALSE)
    )
  })

  output$linelist_table <- renderDT({
    req(results())

    datatable(
      results()$linelist,
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )
  })

  output$relationships_table <- renderDT({
    req(results())

    datatable(
      results()$relationships,
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )
  })

  output$network_plot <- renderVisNetwork({
    req(results())
    result <- results()
    nodes <- build_node_table(result$linelist, result$relationships)
    edges <- build_edge_table(result$relationships)

    shiny::validate(
      shiny::need(nrow(nodes) > 0, "Run an extraction to build the network.")
    )

    visNetwork(nodes, edges, width = "100%", height = "720px") |>
      visNodes(shadow = FALSE, size = 22) |>
      visEdges(smooth = FALSE) |>
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = TRUE) |>
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -60,
          centralGravity = 0.02,
          springLength = 130,
          springConstant = 0.08
        )
      ) |>
      visLegend(useGroups = TRUE, position = "right")
  })

  output$prompt_text <- renderText({
    req(results())
    results()$prompt_text
  })

  output$raw_response <- renderText({
    req(results())
    results()$raw_response
  })

  output$download_linelist <- downloadHandler(
    filename = function() {
      paste0("linelist_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(results())
      readr::write_csv(results()$linelist, file)
    }
  )

  output$download_relationships <- downloadHandler(
    filename = function() {
      paste0("relationships_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(results())
      readr::write_csv(results()$relationships, file)
    }
  )

  output$download_raw <- downloadHandler(
    filename = function() {
      paste0("raw_response_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      req(results())
      readr::write_file(results()$raw_response, file)
    }
  )
}

shinyApp(ui, server)
