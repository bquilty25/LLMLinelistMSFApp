llmlinelist_default_model <- function(provider) {
    defaults <- c(
        azure = "gpt-5",
        claude = "claude-sonnet-4-6",
        openai = "gpt-4o",
        gemini = "gemini-1.5-pro",
        ollama = "llama3.3:latest",
        mlx = "mlx-community/Qwen3.5-9B-MLX-4bit"
    )

    defaults[[provider]]
}

llmlinelist_require_env <- function(vars) {
    missing_vars <- vars[!nzchar(Sys.getenv(vars, unset = ""))]

    if (length(missing_vars) > 0) {
        rlang::abort(sprintf("Missing required environment variables: %s", paste(missing_vars, collapse = ", ")))
    }
}

llmlinelist_null_coalesce <- function(x, y) {
    if (is.null(x)) y else x
}

llmlinelist_estimate_cost <- function(model, prompt_tokens, completion_tokens) {
    model <- llmlinelist_null_coalesce(model, "unknown")

    pricing <- list(
        "gpt-5" = c(input = 1.25 / 1000000, output = 10 / 1000000),
        "gpt-5-nano" = c(input = 0.05 / 1000000, output = 0.4 / 1000000),
        "gpt-4o" = c(input = 2.5 / 1000000, output = 10 / 1000000),
        "claude-sonnet-4-6" = c(input = 3 / 1000000, output = 15 / 1000000),
        "claude-haiku-4-5" = c(input = 1 / 1000000, output = 5 / 1000000)
    )

    price <- pricing[[model]]
    if (is.null(price)) {
        return(NA_real_)
    }

    (prompt_tokens * price[["input"]]) + (completion_tokens * price[["output"]])
}

llmlinelist_append_examples <- function(prompt_template, few_shot_examples) {
    if (is.null(few_shot_examples) || !nzchar(few_shot_examples)) {
        return(prompt_template)
    }

    insert_position <- stringr::str_locate(prompt_template, "<outbreak_narrative>")[1, "start"] - 1
    if (is.na(insert_position)) {
        insert_position <- stringr::str_locate(prompt_template, "<outbreak_report>")[1, "start"] - 1
    }

    if (is.na(insert_position)) {
        return(paste0(prompt_template, "\n\n", few_shot_examples))
    }

    paste0(
        stringr::str_sub(prompt_template, 1, insert_position),
        few_shot_examples,
        "\n",
        stringr::str_sub(prompt_template, insert_position + 1, nchar(prompt_template))
    )
}

llmlinelist_prepare_prompt <- function(template, content) {
    output <- stringr::str_replace(template, "\\{\\{content\\}\\}", content)
    stringr::str_replace(output, "\\{content\\}", content)
}

llmlinelist_call_http_provider <- function(provider, prompt, model, temperature, max_tokens) {
    provider <- match.arg(provider, c("azure", "claude", "openai", "gemini", "ollama"))

    request_info <- switch(provider,
        azure = {
            llmlinelist_require_env(c("AZURE_OPENAI_ENDPOINT", "AZURE_OPENAI_API_KEY"))
            base_url <- sprintf(
                "%s/openai/deployments/%s/chat/completions?api-version=%s",
                Sys.getenv("AZURE_OPENAI_ENDPOINT"),
                model,
                Sys.getenv("AZURE_OPENAI_API_VERSION", unset = "2025-01-01-preview")
            )

            list(
                request = httr2::request(base_url) |>
                    httr2::req_headers(`Content-Type` = "application/json", `api-key` = Sys.getenv("AZURE_OPENAI_API_KEY")) |>
                    httr2::req_body_json(list(
                        messages = list(list(role = "user", content = prompt)),
                        max_completion_tokens = as.integer(max_tokens),
                        reasoning_effort = "low"
                    )),
                parse = function(resp) {
                    content <- httr2::resp_body_json(resp)
                    list(
                        text = content$choices[[1]]$message$content,
                        usage = list(
                            prompt_tokens = content$usage$prompt_tokens,
                            completion_tokens = content$usage$completion_tokens,
                            total_tokens = content$usage$total_tokens
                        )
                    )
                }
            )
        },
        claude = {
            llmlinelist_require_env("CLAUDE_API_KEY")
            list(
                request = httr2::request("https://api.anthropic.com/v1/messages") |>
                    httr2::req_headers(
                        `Content-Type` = "application/json",
                        `x-api-key` = Sys.getenv("CLAUDE_API_KEY"),
                        `anthropic-version` = "2023-06-01"
                    ) |>
                    httr2::req_body_json(list(
                        model = model,
                        messages = list(list(role = "user", content = prompt)),
                        temperature = temperature,
                        max_tokens = as.integer(max_tokens)
                    )),
                parse = function(resp) {
                    content <- httr2::resp_body_json(resp)
                    list(
                        text = content$content[[1]]$text,
                        usage = list(
                            prompt_tokens = content$usage$input_tokens,
                            completion_tokens = content$usage$output_tokens,
                            total_tokens = content$usage$input_tokens + content$usage$output_tokens
                        )
                    )
                }
            )
        },
        openai = {
            llmlinelist_require_env("OPENAI_API_KEY")
            list(
                request = httr2::request("https://api.openai.com/v1/chat/completions") |>
                    httr2::req_headers(
                        `Content-Type` = "application/json",
                        Authorization = sprintf("Bearer %s", Sys.getenv("OPENAI_API_KEY"))
                    ) |>
                    httr2::req_body_json(list(
                        model = model,
                        messages = list(list(role = "user", content = prompt)),
                        temperature = temperature,
                        max_tokens = as.integer(max_tokens)
                    )),
                parse = function(resp) {
                    content <- httr2::resp_body_json(resp)
                    list(
                        text = content$choices[[1]]$message$content,
                        usage = list(
                            prompt_tokens = content$usage$prompt_tokens,
                            completion_tokens = content$usage$completion_tokens,
                            total_tokens = content$usage$total_tokens
                        )
                    )
                }
            )
        },
        gemini = {
            llmlinelist_require_env("GEMINI_API_KEY")
            list(
                request = httr2::request(sprintf("https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent", model)) |>
                    httr2::req_url_query(key = Sys.getenv("GEMINI_API_KEY")) |>
                    httr2::req_headers(`Content-Type` = "application/json") |>
                    httr2::req_body_json(list(
                        contents = list(list(parts = list(list(text = prompt)))),
                        generationConfig = list(temperature = temperature, maxOutputTokens = as.integer(max_tokens))
                    )),
                parse = function(resp) {
                    content <- httr2::resp_body_json(resp)
                    text <- content$candidates[[1]]$content$parts[[1]]$text
                    usage <- if (!is.null(content$usageMetadata)) {
                        list(
                            prompt_tokens = content$usageMetadata$promptTokenCount,
                            completion_tokens = content$usageMetadata$candidatesTokenCount,
                            total_tokens = content$usageMetadata$totalTokenCount
                        )
                    } else {
                        list(
                            prompt_tokens = ceiling(nchar(prompt) / 4),
                            completion_tokens = ceiling(nchar(text) / 4),
                            total_tokens = ceiling((nchar(prompt) + nchar(text)) / 4)
                        )
                    }
                    list(text = text, usage = usage)
                }
            )
        },
        ollama = {
            list(
                request = httr2::request("http://localhost:11434/api/generate") |>
                    httr2::req_headers(`Content-Type` = "application/json") |>
                    httr2::req_body_json(list(
                        model = model,
                        prompt = prompt,
                        temperature = temperature,
                        stream = FALSE
                    )),
                parse = function(resp) {
                    content <- httr2::resp_body_json(resp)
                    list(
                        text = content$response,
                        usage = list(
                            prompt_tokens = ceiling(nchar(prompt) / 4),
                            completion_tokens = ceiling(nchar(content$response) / 4),
                            total_tokens = ceiling((nchar(prompt) + nchar(content$response)) / 4)
                        )
                    )
                }
            )
        }
    )

    response <- request_info$request |>
        httr2::req_retry(max_tries = 4) |>
        httr2::req_perform()

    if (httr2::resp_status(response) >= 300) {
        rlang::abort(sprintf("Provider call failed: HTTP %s", httr2::resp_status(response)))
    }

    request_info$parse(response)
}

#' Extract Markdown from DOCX
#'
#' Extract content from a DOCX file into a plain markdown-like text representation.
#'
#' @param docx_path Path to the DOCX file.
#'
#' @return A single character string.
#' @export
extract_docx_markdown <- function(docx_path) {
    if (!requireNamespace("officer", quietly = TRUE)) {
        rlang::abort("DOCX processing requires the officer package.")
    }

    if (!file.exists(docx_path)) {
        rlang::abort(sprintf("DOCX file not found: %s", docx_path))
    }

    doc <- officer::read_docx(docx_path)
    doc_content <- officer::docx_summary(doc)

    if (!"text" %in% names(doc_content)) {
        return("")
    }

    text <- doc_content$text
    text <- text[!is.na(text)]
    text <- trimws(text)
    text <- text[nzchar(text)]
    paste(text, collapse = "\n\n")
}

#' Build System Prompt From Packaged Template
#'
#' Load the bundled system prompt template and substitute document content.
#'
#' @param content Outbreak narrative content.
#' @param prompt_type Prompt template type.
#'
#' @return A single character string.
#' @export
create_system_prompt <- function(content, prompt_type = c("ebola_narrative", "msf", "generic")) {
    prompt_type <- match.arg(prompt_type)
    prompt_name <- switch(prompt_type,
        ebola_narrative = "system_prompt_ebola_narrative.txt",
        msf = "system_prompt_ebola_narrative.txt",
        generic = "system_prompt_ebola_narrative.txt"
    )

    prompt_template <- readr::read_file(llmlinelist_system_prompt_path(prompt_name))
    llmlinelist_prepare_prompt(prompt_template, content)
}

#' Call an LLM Provider
#'
#' Send a prompt to the configured provider and return the raw text response.
#'
#' @param prompt Prompt text.
#' @param provider Provider name.
#' @param model Optional model override.
#' @param temperature Sampling temperature.
#' @param max_tokens Maximum response tokens.
#' @param raw_prompt If `TRUE`, use the prompt as-is.
#' @param prompt_type Prompt template type when `raw_prompt = FALSE`.
#' @param kv_bits Optional MLX kv cache bits.
#' @param kv_quant_scheme Optional MLX kv quant scheme.
#'
#' @return A character string with provider metadata attached as attributes.
#' @export
llm_call_tidy <- function(prompt,
                          provider = c("azure", "claude", "openai", "gemini", "ollama", "mlx"),
                          model = NULL,
                          temperature = 0.1,
                          max_tokens = 25000,
                          raw_prompt = FALSE,
                          prompt_type = "ebola_narrative",
                          kv_bits = NULL,
                          kv_quant_scheme = NULL) {
    provider <- match.arg(provider)

    if (!is.character(prompt) || length(prompt) != 1 || !nzchar(prompt)) {
        rlang::abort("Prompt must be a non-empty character string.")
    }

    model <- llmlinelist_null_coalesce(model, llmlinelist_default_model(provider))
    full_prompt <- if (isTRUE(raw_prompt)) prompt else create_system_prompt(prompt, prompt_type = prompt_type)

    result <- if (provider == "mlx") {
        response <- call_mlx_llm(
            prompt = full_prompt,
            model = model,
            temperature = temperature,
            max_tokens = max_tokens,
            kv_bits = kv_bits,
            kv_quant_scheme = kv_quant_scheme
        )

        list(
            text = response,
            usage = list(
                prompt_tokens = ceiling(nchar(full_prompt) / 4),
                completion_tokens = ceiling(nchar(response) / 4),
                total_tokens = ceiling((nchar(full_prompt) + nchar(response)) / 4)
            ),
            mlx_timing = attr(response, "mlx_timing")
        )
    } else {
        llmlinelist_call_http_provider(provider, full_prompt, model, temperature, max_tokens)
    }

    response <- result$text
    attr(response, "usage") <- result$usage
    attr(response, "cost") <- llmlinelist_estimate_cost(model, result$usage$prompt_tokens, result$usage$completion_tokens)
    attr(response, "model") <- model
    attr(response, "provider") <- provider

    if (!is.null(result$mlx_timing)) {
        attr(response, "mlx_timing") <- result$mlx_timing
    }

    response
}

#' Parse Structured Data From LLM Response
#'
#' Extract a line list from tagged, fenced, or raw JSON in the response.
#'
#' @param response Raw LLM response text.
#'
#' @return A list with `linelist` and `raw_response`.
#' @export
extract_structured_data <- function(response) {
    try_parse_json <- function(text) {
        if (is.null(text) || !nzchar(stringr::str_trim(text))) {
            return(NULL)
        }

        text <- stringr::str_trim(text)

        if (grepl("^\\{", text) && grepl("\\]$", text)) {
            text <- paste0("[", text)
        } else if (grepl("^\\[", text) && grepl("\\}$", text)) {
            text <- paste0(text, "]")
        }

        tryCatch(
            jsonlite::fromJSON(text, simplifyDataFrame = TRUE),
            error = function(e) {
                if (!grepl("^\\[", text)) {
                    tryCatch(jsonlite::fromJSON(paste0("[", text, "]"), simplifyDataFrame = TRUE), error = function(e2) NULL)
                } else {
                    NULL
                }
            }
        )
    }

    extract_between_tags <- function(tag) {
        pattern <- sprintf("(?s)<%s>\\s*(.*?)\\s*</%s>", tag, tag)
        matches <- stringr::str_match_all(response, pattern)[[1]]
        if (nrow(matches) > 0) stringr::str_trim(matches[nrow(matches), 2]) else NULL
    }

    extract_fenced_json <- function() {
        pattern_json <- "(?s)```json\\s*(\\{.*?\\}|\\[.*?\\])\\s*```"
        matches <- stringr::str_match_all(response, pattern_json)[[1]]
        if (nrow(matches) > 0) {
            return(stringr::str_trim(matches[nrow(matches), 2]))
        }

        pattern_any <- "(?s)```\\s*(\\{.*?\\}|\\[.*?\\])\\s*```"
        matches <- stringr::str_match_all(response, pattern_any)[[1]]
        if (nrow(matches) > 0) stringr::str_trim(matches[nrow(matches), 2]) else NULL
    }

    extract_raw_json <- function() {
        matches <- stringr::str_match_all(response, "(?s)(\\{.*?\\}|\\[.*?\\])")[[1]]
        if (nrow(matches) > 0) stringr::str_trim(matches[nrow(matches), 2]) else NULL
    }

    json_text <- extract_between_tags("linelist")
    if (is.null(json_text)) json_text <- extract_fenced_json()
    if (is.null(json_text)) json_text <- extract_raw_json()

    linelist <- tibble::tibble()
    if (!is.null(json_text)) {
        parsed <- try_parse_json(json_text)
        if (!is.null(parsed) && is.data.frame(parsed)) {
            linelist <- tibble::as_tibble(parsed)
        } else if (is.list(parsed) && length(parsed) > 0 && all(vapply(parsed, is.list, logical(1)))) {
            linelist <- purrr::map_dfr(parsed, tibble::as_tibble)
        }
    }

    if (nrow(linelist) > 0) {
        if ("case_id" %in% names(linelist)) {
            linelist$case_id <- toupper(linelist$case_id)
        }

        normalize_list_column <- function(data, column_name) {
            if (!column_name %in% names(data)) {
                return(data)
            }

            data[[column_name]] <- purrr::map_chr(data[[column_name]], function(x) {
                if (is.null(x) || length(x) == 0 || all(is.na(x))) {
                    return(NA_character_)
                }
                ids <- toupper(as.character(unlist(x)))
                ids <- ids[!is.na(ids) & ids != "" & ids != "NULL"]
                if (length(ids) == 0) NA_character_ else paste(unique(ids), collapse = ", ")
            })

            data
        }

        linelist <- normalize_list_column(linelist, "contacts")
        linelist <- normalize_list_column(linelist, "secondary_cases")
        linelist <- normalize_list_column(linelist, "potential_infector")
        linelist <- normalize_list_column(linelist, "most_probable_infector")
    }

    list(linelist = linelist, raw_response = response)
}

#' Process a Document in One LLM Call
#'
#' Apply the packaged system prompt and extract a structured line list from a
#' document narrative.
#'
#' @param document_content Document content as a single string.
#' @param content_type Content type label, retained for interface compatibility.
#' @param max_tokens Maximum response tokens.
#' @param provider Provider name.
#' @param model Optional model override.
#' @param temperature Sampling temperature.
#' @param few_shot_examples Optional few-shot prompt block.
#' @param system_prompt_path Optional path to a prompt template file.
#' @param kv_bits Optional MLX kv cache bits.
#' @param kv_quant_scheme Optional MLX kv quant scheme.
#'
#' @return A list with `linelist`, `contacts`, and `raw_response`.
#' @export
process_document <- function(document_content, content_type = "text", max_tokens = 20000,
                             provider = "azure", model = NULL, temperature = 0.1,
                             few_shot_examples = NULL,
                             system_prompt_path = NULL,
                             kv_bits = NULL,
                             kv_quant_scheme = NULL) {
    if (is.null(system_prompt_path)) {
        system_prompt_path <- llmlinelist_system_prompt_path()
    }

    prompt_template <- readr::read_file(system_prompt_path)
    prompt_template <- llmlinelist_append_examples(prompt_template, few_shot_examples)
    full_prompt <- llmlinelist_prepare_prompt(prompt_template, document_content)

    llm_response <- llm_call_tidy(
        full_prompt,
        provider = provider,
        model = model,
        max_tokens = max_tokens,
        temperature = temperature,
        raw_prompt = TRUE,
        kv_bits = kv_bits,
        kv_quant_scheme = kv_quant_scheme
    )

    structured_result <- extract_structured_data(llm_response)
    if (is.null(structured_result$linelist) || nrow(structured_result$linelist) == 0) {
        return(list(linelist = tibble::tibble(), contacts = tibble::tibble(), raw_response = llm_response))
    }

    list(linelist = structured_result$linelist, contacts = tibble::tibble(), raw_response = llm_response)
}

# Deprecated compatibility wrapper retained for transition.
process_full_document_xml <- function(document_content, content_type = "xml", max_tokens = 20000,
                                      provider = "azure", model = NULL, temperature = 0.1,
                                      few_shot_examples = NULL,
                                      system_prompt_path = NULL,
                                      kv_bits = NULL,
                                      kv_quant_scheme = NULL) {
    .Deprecated("process_document")
    process_document(
        document_content = document_content,
        content_type = content_type,
        max_tokens = max_tokens,
        provider = provider,
        model = model,
        temperature = temperature,
        few_shot_examples = few_shot_examples,
        system_prompt_path = system_prompt_path,
        kv_bits = kv_bits,
        kv_quant_scheme = kv_quant_scheme
    )
}

#' Generate a Prompt File for App Auto Mode
#'
#' Create a package-safe prompt file from the bundled template and a sample of the document.
#'
#' @param document_path Path to the current document.
#' @param output_path Destination path for the generated prompt file.
#' @param provider Provider name.
#' @param model Model name.
#' @param max_sample_words Maximum document words to include in the generation note.
#' @param force_regeneration If `TRUE`, overwrite any existing file.
#'
#' @return The output path, invisibly.
#' @export
generate_extraction_prompt <- function(document_path,
                                       output_path,
                                       provider = "azure",
                                       model = NULL,
                                       max_sample_words = 400,
                                       force_regeneration = FALSE) {
    if (file.exists(output_path) && !isTRUE(force_regeneration)) {
        return(invisible(output_path))
    }

    document_text <- if (grepl("\\.docx$", document_path, ignore.case = TRUE)) {
        extract_docx_markdown(document_path)
    } else {
        readr::read_file(document_path)
    }

    words <- stringr::str_split(stringr::str_squish(document_text), "\\s+")[[1]]
    sample_text <- paste(utils::head(words, max_sample_words), collapse = " ")
    prompt_template <- readr::read_file(llmlinelist_system_prompt_path())
    generated_prompt <- paste(
        prompt_template,
        "",
        sprintf("Auto-generated context note for provider %s and model %s.", provider, llmlinelist_null_coalesce(model, llmlinelist_default_model(provider))),
        "Retain the extraction schema and rules from the template below. The following document sample highlights the narrative style and terminology in the current input:",
        sample_text,
        sep = "\n"
    )

    readr::write_file(generated_prompt, output_path)
    invisible(output_path)
}

llmlinelist_first_non_missing <- function(x) {
    values <- x[!is.na(x)]
    if (length(values) == 0) {
        return(x[[1]])
    }

    if (is.character(values)) {
        values <- values[trimws(values) != "" & tolower(values) != "null"]
        if (length(values) == 0) {
            return(x[[1]])
        }
    }

    values[[1]]
}

#' Consolidate Duplicate Rows in a Line List
#'
#' Deduplicate extracted rows using stable case-id and name/date heuristics.
#'
#' @param linelist_data Extracted line list data.
#' @param provider Provider name, accepted for interface compatibility.
#' @param model Model name, accepted for interface compatibility.
#'
#' @return A deduplicated tibble.
#' @export
consolidate_duplicates_llm <- function(linelist_data, provider = NULL, model = NULL) {
    if (is.null(linelist_data) || nrow(linelist_data) == 0) {
        return(tibble::as_tibble(linelist_data))
    }

    linelist_tbl <- tibble::as_tibble(linelist_data)
    linelist_tbl <- standardize_columns(linelist_tbl)

    if (!"case_id" %in% names(linelist_tbl)) {
        linelist_tbl$case_id <- NA_character_
    }

    if (!"name" %in% names(linelist_tbl)) {
        linelist_tbl$name <- NA_character_
    }

    if (!"onset_date" %in% names(linelist_tbl)) {
        linelist_tbl$onset_date <- NA_character_
    }

    linelist_tbl$case_id <- dplyr::na_if(toupper(trimws(as.character(linelist_tbl$case_id))), "")
    linelist_tbl$dedupe_name <- vapply(linelist_tbl$name, llmlinelist_normalize_name, character(1))
    linelist_tbl$dedupe_key <- dplyr::coalesce(
        ifelse(
            !is.na(linelist_tbl$dedupe_name),
            paste(linelist_tbl$dedupe_name, llmlinelist_null_coalesce(linelist_tbl$onset_date, "")),
            NA_character_
        ),
        linelist_tbl$case_id,
        paste0("row_", seq_len(nrow(linelist_tbl)))
    )

    split(linelist_tbl, linelist_tbl$dedupe_key, drop = TRUE) |>
        purrr::map_dfr(\(group) {
            stats::setNames(
                lapply(group, llmlinelist_first_non_missing),
                names(group)
            ) |>
                tibble::as_tibble()
        }) |>
        dplyr::select(-dplyr::any_of(c("dedupe_key", "dedupe_name")))
}

#' Call MLX Through the Packaged Helper Script
#'
#' @param prompt Prompt text.
#' @param model MLX model name.
#' @param temperature Sampling temperature.
#' @param max_tokens Maximum response tokens.
#' @param system_prompt Optional system prompt.
#' @param kv_bits Optional kv cache bits.
#' @param kv_quant_scheme Optional kv quant scheme.
#' @param debug If `TRUE`, print stderr.
#'
#' @return A character string.
#' @export
call_mlx_llm <- function(prompt, model, temperature = 0.1, max_tokens = 16384, system_prompt = NULL,
                         kv_bits = NULL, kv_quant_scheme = NULL, debug = FALSE) {
    full_prompt <- if (!is.null(system_prompt)) glue::glue("{system_prompt}\n\n{prompt}") else prompt
    temp_file <- tempfile(fileext = ".txt")
    on.exit(unlink(temp_file), add = TRUE)
    readr::write_file(full_prompt, temp_file)

    python_bin <- Sys.getenv("LLMLINELIST_MLX_PYTHON_BIN", unset = path.expand("~/mlx-env/bin/python3"))
    wrapper_script <- system.file("scripts", "mlx_generate.py", package = "LLMLineListMSFApp")

    if (!nzchar(wrapper_script)) {
        rlang::abort("Packaged MLX helper script not found.")
    }

    args <- c(
        wrapper_script,
        "--model", model,
        "--temperature", as.character(temperature),
        "--prompt", sprintf("@%s", temp_file)
    )

    if (!is.null(max_tokens)) args <- c(args, "--max-tokens", as.character(max_tokens))
    if (!is.null(system_prompt)) args <- c(args, "--system", system_prompt)
    if (!is.null(kv_bits)) args <- c(args, "--kv-bits", as.character(kv_bits))
    if (!is.null(kv_quant_scheme)) args <- c(args, "--kv-quant-scheme", kv_quant_scheme)
    if (isTRUE(debug)) args <- c(args, "--verbose")

    output <- system2(python_bin, args = args, stdout = TRUE, stderr = TRUE)
    status <- llmlinelist_null_coalesce(attr(output, "status"), 0L)

    if (!identical(status, 0L)) {
        rlang::abort(sprintf("MLX execution failed with status %s", status))
    }

    paste(output, collapse = "\n")
}
