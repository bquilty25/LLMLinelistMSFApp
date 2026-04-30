# Tidyverse LLM Functions for Outbreak Report Analysis
# Using elmer and modern tidyverse packages with httr2 fallback
# Supports Azure OpenAI (default for data security), Claude, OpenAI, and Gemini providers
#
# FILE FORMAT SUPPORT:
# - Text files (.txt)
# - PDF files (.pdf) - requires pdftools package
# - Microsoft Word files (.docx) - requires officer package, plain text extraction (no format conversion needed)
#
# SECURITY FEATURES FOR MSF CONFIDENTIAL DATA:
# - Azure OpenAI is the default provider for all functions
# - Interactive confirmation required before using non-Azure providers
# - Non-interactive mode blocks non-Azure providers to prevent accidental data leaks
# - All functions default to secure Azure endpoint

# Load required packages
library(httr2)
library(purrr)
library(dplyr)
library(tibble)
library(readr)
library(stringr)
library(glue)
library(rlang)
library(tidyr)

# Load here package for robust path resolution
if (requireNamespace("here", quietly = TRUE)) {
    library(here)
} else {
    message("here package not available. Using basic path resolution.")
}

# Check pdftools availability
.pdf_available <- function() {
    requireNamespace("pdftools", quietly = TRUE)
}

# Load pdftools only if available (needed for PDF processing)
if (.pdf_available()) {
    library(pdftools)
    message("pdftools loaded - PDF processing available.")
} else {
    message("pdftools package not available. PDF processing functionality will be disabled.")
}

# Check officer availability for DOCX processing
.officer_available <- function() {
    requireNamespace("officer", quietly = TRUE)
}

# Load officer if available
if (.officer_available()) {
    library(officer)
    message("officer loaded - DOCX processing available.")
} else {
    message("officer package not available. DOCX processing functionality will be disabled.")
}

# Check ellmer availability
.ellmer_available <- function() {
    requireNamespace("ellmer", quietly = TRUE)
}

# Load ellmer if available
if (.ellmer_available()) {
    library(ellmer)
    message("Using ellmer package for LLM interactions.")
} else {
    message("ellmer package not available. Using httr2 fallback for API calls.")
}

# Load examples for prompt context - check multiple possible paths
examples_paths <- c(
    "data/examples2.txt",
    "examples2.txt",
    file.path(getwd(), "data", "examples2.txt")
)

examples_file <- NULL
for (path in examples_paths) {
    if (file.exists(path)) {
        examples_file <- path
        break
    }
}

if (!is.null(examples_file)) {
    examples <- read_file(examples_file)
} else {
    warn("Examples file not found. Continuing without examples.")
    examples <- ""
}

#' Create System Prompt Using Template
#'
#' Load a system prompt template from file and substitute content
#'
#' @param content Character string containing the outbreak report content
#' @param prompt_type Character string specifying which prompt to use:
#'   "ebola_narrative" (default) for detailed French EVD narratives,
#'   "msf" for the original MSF prompt,
#'   "generic" for basic outbreak reports
#' @return Character string with complete system prompt
create_system_prompt <- function(content, prompt_type = c("ebola_narrative", "msf", "generic")) {
    prompt_type <- match.arg(prompt_type)

    # Map prompt type to filename
    prompt_filename <- switch(prompt_type,
        "ebola_narrative" = "system_prompt_ebola_narrative.txt",
        "msf" = "system_prompt_msf.txt",
        "generic" = "system_prompt.txt"
    )

    # Load the system prompt template - check multiple possible paths
    possible_paths <- c(
        file.path("data", prompt_filename),
        file.path(getwd(), "data", prompt_filename)
    )

    # Add here::here() path if here package is available
    if (requireNamespace("here", quietly = TRUE)) {
        possible_paths <- c(possible_paths, file.path(here::here(), "data", prompt_filename))
    }

    prompt_file <- NULL
    for (path in possible_paths) {
        if (file.exists(path)) {
            prompt_file <- path
            break
        }
    }

    if (is.null(prompt_file)) {
        abort(glue("System prompt file not found. Tried: {str_c(possible_paths, collapse = ', ')}"))
    }

    # Read the system prompt template
    prompt_template <- read_file(prompt_file)

    # Replace the {{content}} placeholder with the actual content
    # Use double-brace delimiters to avoid conflicts with JSON examples in prompt
    glue(prompt_template, content = content, .open = "{{", .close = "}}")
}

#' Configure LLM Chat Session
#'
#' This function sets up a chat session with specified LLM provider using ellmer
#'
#' @param provider Character string specifying the provider ("azure", "claude", "openai", "gemini", "ollama", "mlx", "transformers") - Azure is default for data security
#' @param model Character string specifying the model name
#' @param temperature Numeric value between 0 and 1 for response randomness
#' @param max_tokens Integer for maximum tokens in response
#' @return An ellmer chat object
#' @export
configure_llm_chat <- function(provider = c("azure", "claude", "openai", "gemini", "ollama", "mlx", "transformers", "cluster"),
                               model = NULL,
                               temperature = 0.1,
                               max_tokens = 25000,
                               kv_bits = NULL,
                               kv_quant_scheme = NULL) {
    # Validate inputs
    provider <- match.arg(provider)

    # Security warning for confidential data - require confirmation for non-Azure/non-local providers
    if (provider != "azure" && provider != "ollama" && provider != "mlx" && provider != "transformers" && provider != "cluster") {
        warning("WARNING: You are attempting to use a non-Azure provider (", provider, ") with potentially confidential MSF data.")
        cat("This may violate data security policies.\n")
        cat("Azure is the approved secure endpoint for confidential outbreak data.\n")
        cat("Ollama, MLX, Transformers, and Cluster are acceptable as they run locally/on-premise without sending data externally.\n")

        allow_noninteractive_external <- identical(
            tolower(Sys.getenv("LLMLINELIST_ALLOW_EXTERNAL_PROVIDER", unset = "false")),
            "true"
        )

        # Check if running in interactive mode
        if (!interactive() && !allow_noninteractive_external) {
            abort("Non-Azure/non-local provider requested in non-interactive mode. Use Azure, Ollama, or MLX provider for security.")
        }

        if (!interactive() && allow_noninteractive_external) {
            cat("Proceeding with", provider, "provider because LLMLINELIST_ALLOW_EXTERNAL_PROVIDER=true was set explicitly.\n")
        } else {
            cat("Do you want to continue with", provider, "? (y/n): ")
            response <- readline()
            if (!tolower(response) %in% c("y", "yes")) {
                abort("Operation cancelled. Please use Azure, Ollama, or MLX provider for confidential data.")
            }
            cat("Proceeding with", provider, "provider as requested.\n")
        }
    }

    if (!is.numeric(temperature) || length(temperature) != 1 ||
        temperature < 0 || temperature > 1) {
        abort("Temperature must be a single numeric value between 0 and 1")
    }

    if (!is.null(max_tokens) && (!is.numeric(max_tokens) || max_tokens <= 0)) {
        abort("max_tokens must be a positive integer")
    }

    # Set default models if not specified
    default_models <- list(
        "claude" = "claude-sonnet-4-6",
        "openai" = "gpt-4",
        "gemini" = "gemini-1.5-pro",
        "azure" = "gpt-4o",
        "ollama" = "llama3.3:latest",
        "mlx" = "mlx-community/Llama-3.2-3B-Instruct-4bit",
        "transformers" = "/sc-resources/llms/Qwen/Qwen3-32B",
        "cluster" = "/sc-resources/llms/Qwen/Qwen3-32B"
    )

    if (is.null(model)) {
        model <- default_models[[provider]]
    }

    # Check for required API keys (except for Ollama/MLX/Transformers/Cluster which are local)
    if (provider != "ollama" && provider != "mlx" && provider != "transformers" && provider != "cluster") {
        api_key_vars <- list(
            "claude" = "CLAUDE_API_KEY",
            "openai" = "OPENAI_API_KEY",
            "gemini" = "GEMINI_API_KEY",
            "azure" = "AZURE_OPENAI_API_KEY"
        )

        required_key <- api_key_vars[[provider]]
        if (Sys.getenv(required_key) == "") {
            abort(glue("{required_key} environment variable not set"))
        }
    }

    # Additional check for Azure endpoint
    if (provider == "azure" && Sys.getenv("AZURE_OPENAI_ENDPOINT") == "") {
        abort("AZURE_OPENAI_ENDPOINT environment variable not set")
    }

    # Configure chat session using ellmer if available, otherwise return config for httr2/cli
    # Note: Azure is excluded from ellmer path due to a bug in ellmer 0.4.0 where Azure's
    # content filter response fields (e.g. filtered: logical) cause a kwargs merge error.
    # Azure uses the httr2 fallback which makes direct REST calls without this issue.
    if (.ellmer_available() && provider != "ollama" && provider != "mlx" && provider != "transformers" && provider != "cluster" && provider != "azure") {
        chat <- switch(provider,
            "claude" = ellmer::chat_claude(
                model = model,
                system_prompt = "You are an expert in analyzing outbreak reports and extracting structured data.",
                temperature = temperature,
                max_tokens = max_tokens
            ),
            "openai" = ellmer::chat_openai(
                model = model,
                system_prompt = "You are an expert in analyzing outbreak reports and extracting structured data.",
                params = list(
                    temperature = temperature,
                    max_tokens = max_tokens
                )
            ),
            "azure" = ellmer::chat_azure_openai(
                model = model,
                system_prompt = "You are an expert in analyzing outbreak reports and extracting structured data.",
                api_version = Sys.getenv("AZURE_OPENAI_API_VERSION", "2024-10-21"),
                api_args = if (grepl("gpt-5|o1|o3|o4", model)) {
                    list()
                } else {
                    list(
                        temperature = temperature,
                        max_tokens = max_tokens
                    )
                }
            ),
            "gemini" = ellmer::chat_gemini(
                model = model,
                system_prompt = "You are an expert in analyzing outbreak reports and extracting structured data.",
                temperature = temperature,
                max_tokens = max_tokens
            )
        )
        return(chat)
    } else {
        # Return configuration for httr2 fallback (used for Ollama/MLX and when ellmer unavailable)
        return(list(
            provider = provider,
            model = model,
            temperature = temperature,
            max_tokens = max_tokens,
            system_prompt = "You are an expert in analyzing outbreak reports and extracting structured data.",
            kv_bits = kv_bits,
            kv_quant_scheme = kv_quant_scheme,
            use_fallback = TRUE
        ))
    }
}

#' Call LLM with Tidyverse Approach
#'
#' Modern tidyverse-based function for calling LLMs using ellmer
#'
#' @param prompt Character string containing the prompt
#' @param provider Character string specifying the provider
#' @param model Character string specifying the model (optional)
#' @param temperature Numeric value for response randomness
#' @param max_tokens Integer for maximum response tokens
#' @param raw_prompt Logical, if TRUE use prompt as-is without template
#' @param prompt_type Character string specifying which prompt template to use
#' @return Character string with LLM response
#' @export
llm_call_tidy <- function(prompt,
                          provider = c("azure", "claude", "openai", "gemini", "ollama", "mlx", "cluster"),
                          model = NULL,
                          temperature = 0.1,
                          max_tokens = 25000,
                          raw_prompt = FALSE,
                          prompt_type = "ebola_narrative",
                          kv_bits = NULL,
                          kv_quant_scheme = NULL) {
    # Input validation
    if (!is.character(prompt) || length(prompt) != 1 || nchar(prompt) == 0) {
        abort("Prompt must be a non-empty character string")
    }

    # Configure chat session
    chat_config <- configure_llm_chat(
        provider = provider,
        model = model,
        temperature = temperature,
        max_tokens = max_tokens,
        kv_bits = kv_bits,
        kv_quant_scheme = kv_quant_scheme
    )

    # Prepare prompt
    if (raw_prompt) {
        full_prompt <- prompt
    } else {
        # Prepare full prompt with examples and system instructions
        system_prompt <- create_system_prompt(prompt, prompt_type = prompt_type)
        full_prompt <- glue("
{examples}

{system_prompt}")
    }

    # Make API call with error handling
    if (.ellmer_available() && !isTRUE(chat_config$use_fallback)) {
        # Use ellmer
        response_text <- tryCatch(
            {
                chat_config$chat(full_prompt)
            },
            error = function(e) {
                abort(glue("LLM API call failed: {e$message}"))
            }
        )

        # Estimate usage for ellmer (since we can't easily get it from the simple chat method yet)
        # Note: In future ellmer versions, we might be able to extract this from the object
        usage <- list(
            prompt_tokens = ceiling(nchar(full_prompt) / 4),
            completion_tokens = ceiling(nchar(response_text) / 4)
        )
        usage$total_tokens <- usage$prompt_tokens + usage$completion_tokens

        response <- response_text
    } else if (provider == "mlx") {
        # Use MLX CLI directly
        response_text <- tryCatch(
            {
                call_mlx_llm(full_prompt, model = chat_config$model, temperature = chat_config$temperature, max_tokens = chat_config$max_tokens, system_prompt = chat_config$system_prompt, kv_bits = chat_config$kv_bits, kv_quant_scheme = chat_config$kv_quant_scheme, debug = FALSE)
            },
            error = function(e) {
                abort(glue("MLX call failed: {e$message}"))
            }
        )

        # Use actual token counts from MLX timing if available, else estimate
        mlx_timing <- attr(response_text, "mlx_timing")
        if (!is.null(mlx_timing) && !is.na(mlx_timing$prompt_tokens) && mlx_timing$prompt_tokens > 0) {
            usage <- list(
                prompt_tokens = mlx_timing$prompt_tokens,
                completion_tokens = mlx_timing$output_tokens %||% ceiling(nchar(response_text) / 4)
            )
        } else {
            usage <- list(
                prompt_tokens = ceiling(nchar(full_prompt) / 4),
                completion_tokens = ceiling(nchar(response_text) / 4)
            )
        }
        usage$total_tokens <- usage$prompt_tokens + usage$completion_tokens

        response <- response_text

        # Propagate MLX timing to response attributes
        if (!is.null(mlx_timing)) {
            attr(response, "mlx_timing") <- mlx_timing
        }
    } else if (provider == "transformers") {
        # Use Transformers via Python bridge
        response_text <- tryCatch(
            {
                call_transformers_llm(full_prompt, model = chat_config$model, temperature = chat_config$temperature, max_tokens = chat_config$max_tokens, device = "cuda", debug = FALSE)
            },
            error = function(e) {
                abort(glue("Transformers call failed: {e$message}"))
            }
        )

        # Estimate usage for Transformers
        usage <- list(
            prompt_tokens = ceiling(nchar(full_prompt) / 4),
            completion_tokens = ceiling(nchar(response_text) / 4)
        )
        usage$total_tokens <- usage$prompt_tokens + usage$completion_tokens

        response <- response_text
    } else if (provider == "cluster") {
        # Use HPC cluster via SSH
        cluster_result <- tryCatch(
            {
                call_cluster_llm(full_prompt, model = chat_config$model, temperature = chat_config$temperature, max_tokens = chat_config$max_tokens, debug = FALSE)
            },
            error = function(e) {
                abort(glue("Cluster call failed: {e$message}"))
            }
        )

        # call_cluster_llm now returns list with response and timing
        response_text <- cluster_result$response

        # Estimate usage for cluster
        usage <- list(
            prompt_tokens = ceiling(nchar(full_prompt) / 4),
            completion_tokens = ceiling(nchar(response_text) / 4)
        )
        usage$total_tokens <- usage$prompt_tokens + usage$completion_tokens

        response <- response_text

        # Attach SLURM timing metadata if available
        if (!is.null(cluster_result$slurm_execution_time) && !is.na(cluster_result$slurm_execution_time)) {
            attr(response, "slurm_execution_time") <- cluster_result$slurm_execution_time
            attr(response, "slurm_job_id") <- cluster_result$slurm_job_id
        }
    } else {
        # Use httr2 fallback
        result <- tryCatch(
            {
                llm_call_httr2_fallback(full_prompt, chat_config)
            },
            error = function(e) {
                abort(glue("LLM API call failed: {e$message}"))
            }
        )

        response <- result$text
        usage <- result$usage
    }

    # Calculate cost
    cost <- estimate_cost(chat_config$model, usage$prompt_tokens, usage$completion_tokens)

    # Attach metadata as attributes
    attr(response, "usage") <- usage
    attr(response, "cost") <- cost
    attr(response, "model") <- chat_config$model
    attr(response, "provider") <- provider
    # mlx_timing already attached above for mlx provider; remains NULL for others

    return(response)
}

#' HTR2 Fallback for LLM API Calls
#'
#' Fallback function using httr2 when elmer is not available
#'
#' @param prompt Character string containing the prompt
#' @param config List containing API configuration
#' @return Character string with LLM response
llm_call_httr2_fallback <- function(prompt, config) {
    provider <- config$provider
    model <- config$model
    temperature <- config$temperature
    max_tokens <- config$max_tokens

    is_reasoning_model <- function(model_name) {
        is.character(model_name) && length(model_name) == 1 && grepl("^gpt-5|^o1|^o3|^o4", model_name)
    }

    azure_response_needs_retry <- function(content) {
        if (is.null(content$choices) || length(content$choices) == 0) {
            return(FALSE)
        }

        choice <- content$choices[[1]]
        message_content <- choice$message$content
        finish_reason <- choice$finish_reason
        completion_tokens <- content$usage$completion_tokens
        reasoning_tokens <- content$usage$completion_tokens_details$reasoning_tokens

        is_empty_content <- is.null(message_content) ||
            (is.character(message_content) && length(message_content) == 1 && !nzchar(trimws(message_content)))

        isTRUE(is_empty_content) &&
            identical(finish_reason, "length") &&
            !is.null(completion_tokens) &&
            !is.null(reasoning_tokens) &&
            is.numeric(completion_tokens) &&
            is.numeric(reasoning_tokens) &&
            reasoning_tokens >= completion_tokens
    }

    build_request <- function(body) {
        req <- request(api_config$base_url) |>
            req_headers(!!!api_config$headers)

        if (provider == "gemini") {
            req <- req |> req_url_query(key = Sys.getenv("GEMINI_API_KEY"))
        }

        req |>
            req_body_json(body) |>
            req_retry(
                max_tries = 6,
                is_transient = \(resp) resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L),
                backoff = \(attempt) min(60 * 2^(attempt - 1), 300),
                after = \(resp) {
                    ra <- suppressWarnings(as.numeric(resp_header(resp, "Retry-After")))
                    if (!is.na(ra) && ra > 0) ra else NA_real_
                }
            )
    }

    # API configuration
    api_configs <- list(
        "ollama" = list(
            base_url = "http://localhost:11434/api/generate",
            headers = list(
                "Content-Type" = "application/json"
            ),
            body_fn = function(prompt, model, temperature, max_tokens) {
                list(
                    model = model,
                    prompt = prompt,
                    temperature = temperature,
                    stream = FALSE
                )
            },
            response_parser = function(resp) {
                content <- resp_body_json(resp)
                # Estimate usage for Ollama if not provided
                usage <- list(
                    prompt_tokens = ceiling(nchar(prompt) / 4),
                    completion_tokens = ceiling(nchar(content$response) / 4)
                )
                usage$total_tokens <- usage$prompt_tokens + usage$completion_tokens

                list(text = content$response, usage = usage)
            }
        ),
        "claude" = list(
            base_url = "https://api.anthropic.com/v1/messages",
            headers = list(
                "Content-Type" = "application/json",
                "x-api-key" = Sys.getenv("CLAUDE_API_KEY"),
                "anthropic-version" = "2023-06-01"
            ),
            body_fn = function(prompt, model, temperature, max_tokens) {
                body <- list(
                    model = model,
                    messages = list(list(role = "user", content = prompt)),
                    temperature = temperature
                )
                if (!is.null(max_tokens)) body$max_tokens <- max_tokens
                body
            },
            response_parser = function(resp) {
                content <- resp_body_json(resp)
                usage <- list(
                    prompt_tokens = content$usage$input_tokens,
                    completion_tokens = content$usage$output_tokens,
                    total_tokens = content$usage$input_tokens + content$usage$output_tokens
                )
                list(text = content$content[[1]]$text, usage = usage)
            }
        ),
        "openai" = list(
            base_url = "https://api.openai.com/v1/chat/completions",
            headers = list(
                "Content-Type" = "application/json",
                "Authorization" = glue("Bearer {Sys.getenv('OPENAI_API_KEY')}")
            ),
            body_fn = function(prompt, model, temperature, max_tokens) {
                body <- list(
                    model = model,
                    messages = list(list(role = "user", content = prompt)),
                    temperature = temperature
                )
                if (!is.null(max_tokens)) body$max_tokens <- max_tokens
                body
            },
            response_parser = function(resp) {
                content <- resp_body_json(resp)
                usage <- list(
                    prompt_tokens = content$usage$prompt_tokens,
                    completion_tokens = content$usage$completion_tokens,
                    total_tokens = content$usage$total_tokens
                )
                list(text = content$choices[[1]]$message$content, usage = usage)
            }
        ),
        "azure" = list(
            base_url = glue("{Sys.getenv('AZURE_OPENAI_ENDPOINT')}/openai/deployments/{model}/chat/completions?api-version={Sys.getenv('AZURE_OPENAI_API_VERSION', '2025-01-01-preview')}"),
            headers = list(
                "Content-Type" = "application/json",
                "api-key" = Sys.getenv("AZURE_OPENAI_API_KEY")
            ),
            body_fn = function(prompt, model, temperature, max_tokens) {
                # GPT-5 / o-series models do not support 'temperature' or 'max_tokens';
                # they require 'max_completion_tokens' and use reasoning tokens heavily,
                # so we set reasoning_effort="low" to avoid exhausting the token budget
                # on reasoning before any output is produced.
                is_new_model <- grepl("^gpt-5|^o1|^o3|^o4", model)
                body <- list(
                    messages = list(list(role = "user", content = prompt))
                )
                if (!is_new_model) body$temperature <- temperature
                if (!is.null(max_tokens)) {
                    if (is_new_model) {
                        body$max_completion_tokens <- as.integer(max_tokens)
                        body$reasoning_effort <- "low"
                    } else {
                        body$max_tokens <- as.integer(max_tokens)
                    }
                }
                body
            },
            response_parser = function(resp) {
                content <- resp_body_json(resp)
                usage <- list(
                    prompt_tokens = content$usage$prompt_tokens,
                    completion_tokens = content$usage$completion_tokens,
                    total_tokens = content$usage$total_tokens
                )
                list(text = content$choices[[1]]$message$content, usage = usage)
            }
        ),
        "gemini" = list(
            base_url = glue("https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent"),
            headers = list(
                "Content-Type" = "application/json"
            ),
            body_fn = function(prompt, model, temperature, max_tokens) {
                config <- list(temperature = temperature)
                if (!is.null(max_tokens)) config$maxOutputTokens <- max_tokens
                list(
                    contents = list(list(parts = list(list(text = prompt)))),
                    generationConfig = config
                )
            },
            response_parser = function(resp) {
                content <- resp_body_json(resp)
                # Gemini usage metadata might be different or missing in some versions
                # Attempt to extract if available, otherwise estimate
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
    )

    api_config <- api_configs[[provider]]

    # Add body
    body <- api_config$body_fn(prompt, model, temperature, max_tokens)
    req <- build_request(body)
    resp <- req |> req_perform()

    # Check for errors
    if (resp_status(resp) != 200) {
        abort(glue("LLM API call failed: HTTP {resp_status(resp)} {resp_status_desc(resp)}.\n{resp_body_string(resp)}"))
    }

    if (provider == "azure" && is_reasoning_model(model) && !is.null(max_tokens)) {
        content <- resp_body_json(resp)

        if (azure_response_needs_retry(content)) {
            retry_tokens <- min(max(as.integer(max_tokens) * 2L, 32768L), 64000L)

            if (retry_tokens > as.integer(max_tokens)) {
                cat(
                    "Azure reasoning model exhausted completion budget without producing content; retrying with",
                    retry_tokens,
                    "max completion tokens...\n"
                )

                retry_body <- api_config$body_fn(prompt, model, temperature, retry_tokens)
                retry_req <- build_request(retry_body)
                retry_resp <- retry_req |> req_perform()

                if (resp_status(retry_resp) == 200) {
                    resp <- retry_resp
                }
            }
        }
    }

    # Parse response
    api_config$response_parser(resp)
}

#' Validate JSON Output from LLM Response
#'
#' Check if the LLM response contains valid, complete JSON that can be parsed.
#' Detects truncated output, malformed JSON, and missing closing tags.
#'
#' @param response Character string containing LLM response
#' @param min_cases Integer minimum number of cases expected (default 10)
#' @return Named list with: valid (logical), n_cases (integer), error (character or NULL)
#' @export
validate_llm_json_output <- function(response, min_cases = 10) {
    result <- list(valid = FALSE, n_cases = 0, error = NULL)

    if (is.null(response) || nchar(trimws(response)) == 0) {
        result$error <- "Empty response"
        return(result)
    }

    # Check for closing linelist tag
    if (!grepl("</linelist>", response, fixed = TRUE)) {
        result$error <- "Missing </linelist> closing tag - output likely truncated"
        return(result)
    }

    # Extract JSON between linelist tags
    pattern <- "(?s)<linelist>\\s*(.*?)\\s*</linelist>"
    m <- stringr::str_match(response, pattern)

    if (is.na(m[1, 2])) {
        result$error <- "Could not extract JSON from linelist tags"
        return(result)
    }

    json_text <- stringr::str_trim(m[1, 2])

    # Check for truncated JSON (ends mid-string or mid-object)
    last_char <- substr(json_text, nchar(json_text), nchar(json_text))
    if (!last_char %in% c("]", "}")) {
        result$error <- glue("JSON appears truncated - ends with '{last_char}' instead of ] or }}")
        return(result)
    }

    # Try to parse JSON
    parsed <- tryCatch(
        {
            jsonlite::fromJSON(json_text, simplifyDataFrame = TRUE)
        },
        error = function(e) {
            result$error <<- glue("JSON parse error: {e$message}")
            NULL
        }
    )

    if (is.null(parsed)) {
        return(result)
    }

    # Check number of cases
    n_cases <- if (is.data.frame(parsed)) nrow(parsed) else length(parsed)
    result$n_cases <- n_cases

    if (n_cases < min_cases) {
        result$error <- glue("Only {n_cases} cases extracted (minimum expected: {min_cases})")
        result$valid <- FALSE # Still return the count but flag as potentially incomplete
        return(result)
    }

    result$valid <- TRUE
    return(result)
}

#' Extract Structured Data from LLM Response
#'
#' Parse XML-tagged linelist from LLM response into structured tibble
#'
#' @param response Character string containing LLM response with XML tags
#' @return Named list containing linelist tibble and raw response
#' @export
extract_structured_data <- function(response) {
    try_parse_json <- function(text) {
        if (is.null(text) || nchar(str_trim(text)) == 0) {
            return(NULL)
        }
        text <- str_trim(text)

        # Auto-fix missing opening or closing brackets for JSON arrays
        if (str_starts(text, "\\{") && str_ends(text, "\\]")) {
            text <- paste0("[", text)
        } else if (str_starts(text, "\\[") && str_ends(text, "\\}")) {
            text <- paste0(text, "]")
        }

        parsed <- tryCatch(
            {
                jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
            },
            error = function(e) {
                # Fallback: if it still fails, try wrapping the whole thing in []
                if (!str_starts(text, "\\[")) {
                    tryCatch(
                        {
                            jsonlite::fromJSON(paste0("[", text, "]"), simplifyDataFrame = TRUE)
                        },
                        error = function(e2) NULL
                    )
                } else {
                    NULL
                }
            }
        )
        parsed
    }

    # Attempt extraction strategies in order of preference
    # 1) XML-like tag: <linelist>...</linelist>
    extract_between_tags <- function(tag) {
        pattern <- glue::glue("(?s)<{tag}>\\s*(.*?)\\s*</{tag}>")
        matches <- stringr::str_match_all(response, pattern)[[1]]
        if (nrow(matches) > 0) {
            # Take the LAST match (most likely the generated one, not the example in prompt)
            return(stringr::str_trim(matches[nrow(matches), 2]))
        }
        NULL
    }

    # 2) Markdown fenced code block: ```json ... ``` or ``` ... ```
    extract_fenced_json <- function() {
        # Try explicit json fence first
        pattern_json <- "(?s)```json\\s*(\\{.*?\\}|\\[.*?\\])\\s*```"
        m1 <- stringr::str_match_all(response, pattern_json)[[1]]
        if (nrow(m1) > 0) {
            return(stringr::str_trim(m1[nrow(m1), 2]))
        }

        # Try generic fenced block that contains JSON
        pattern_any <- "(?s)```\\s*(\\{.*?\\}|\\[.*?\\])\\s*```"
        m2 <- stringr::str_match_all(response, pattern_any)[[1]]
        if (nrow(m2) > 0) {
            return(stringr::str_trim(m2[nrow(m2), 2]))
        }

        NULL
    }

    # 3) Raw JSON anywhere in the text (last JSON object/array)
    extract_raw_json <- function() {
        # Non-greedy match for individual objects/arrays
        pattern <- "(?s)(\\{.*?\\}|\\[.*?\\])"
        matches <- stringr::str_match_all(response, pattern)[[1]]
        if (nrow(matches) > 0) {
            # Take the LAST match
            return(stringr::str_trim(matches[nrow(matches), 2]))
        }
        NULL
    }

    json_text <- NULL

    # Prefer explicit linelist tag if present
    json_text <- extract_between_tags("linelist")

    # Fallback: try fenced JSON block
    if (is.null(json_text)) json_text <- extract_fenced_json()

    # Fallback: try raw JSON
    if (is.null(json_text)) json_text <- extract_raw_json()

    linelist <- tibble::tibble()

    if (!is.null(json_text)) {
        parsed <- try_parse_json(json_text)
        if (!is.null(parsed) && is.data.frame(parsed)) {
            linelist <- as_tibble(parsed)
        } else if (!is.null(parsed) && (is.list(parsed) || is.vector(parsed))) {
            # If parsed is a list of records convert to tibble if possible
            if (is.list(parsed) && length(parsed) > 0 && all(sapply(parsed, is.list))) {
                # list of objects -> bind_rows
                linelist <- purrr::map_dfr(parsed, ~ as_tibble(.x))
            } else if (is.data.frame(parsed)) {
                linelist <- as_tibble(parsed)
            }
        }
    }

    # If linelist has nested arrays for contacts/secondary_cases, normalize them to string format
    if (nrow(linelist) > 0) {
        # Normalize case_id to uppercase
        if ("case_id" %in% names(linelist)) {
            linelist <- linelist %>%
                dplyr::mutate(case_id = toupper(case_id))
        }

        # Normalize contacts to comma-separated string (uppercase)
        if ("contacts" %in% names(linelist)) {
            linelist <- linelist %>%
                dplyr::mutate(contacts = purrr::map_chr(contacts, function(x) {
                    if (is.null(x) || length(x) == 0 || all(is.na(x))) {
                        return(NA_character_)
                    }
                    # Flatten, uppercase, and collapse
                    ids <- toupper(as.character(unlist(x)))
                    ids <- ids[!is.na(ids) & ids != "" & toupper(ids) != "NULL"]
                    if (length(ids) == 0) {
                        return(NA_character_)
                    }
                    paste(unique(ids), collapse = ", ")
                }))
        }

        # Normalize secondary_cases to comma-separated string (uppercase)
        if ("secondary_cases" %in% names(linelist)) {
            linelist <- linelist %>%
                dplyr::mutate(secondary_cases = purrr::map_chr(secondary_cases, function(x) {
                    if (is.null(x) || length(x) == 0 || all(is.na(x))) {
                        return(NA_character_)
                    }
                    # Flatten, uppercase, and collapse
                    ids <- toupper(as.character(unlist(x)))
                    ids <- ids[!is.na(ids) & ids != "" & toupper(ids) != "NULL"]
                    if (length(ids) == 0) {
                        return(NA_character_)
                    }
                    paste(unique(ids), collapse = ", ")
                }))
        }

        # Normalize potential_infector to uppercase
        if ("potential_infector" %in% names(linelist)) {
            linelist <- linelist %>%
                dplyr::mutate(potential_infector = purrr::map_chr(potential_infector, function(x) {
                    if (is.null(x) || length(x) == 0 || all(is.na(x))) {
                        return(NA_character_)
                    }
                    ids <- toupper(as.character(unlist(x)))
                    ids <- ids[!is.na(ids) & ids != "" & toupper(ids) != "NULL"]
                    if (length(ids) == 0) {
                        return(NA_character_)
                    }
                    paste(unique(ids), collapse = ", ")
                }))
        }

        # Normalize most_probable_infector to uppercase
        if ("most_probable_infector" %in% names(linelist)) {
            linelist <- linelist %>%
                dplyr::mutate(most_probable_infector = ifelse(
                    is.na(most_probable_infector) | most_probable_infector == "null",
                    NA_character_,
                    toupper(most_probable_infector)
                ))
        }
    }

    # Return results
    list(
        linelist = linelist,
        raw_response = response
    )
}

#' Process PDF with Tidyverse Approach
#'
#' Modern version of PDF processing using tidyverse principles
#'
#' @param pdf_path Character string path to PDF file
#' @param provider Character string specifying LLM provider
#' @param model Character string specifying model (optional)
#' @param temperature Numeric value for response randomness
#' @return Named list with structured data and raw response
#' @export
process_pdf_tidy <- function(pdf_path,
                             provider = c("azure", "claude", "openai", "gemini", "ollama"),
                             model = NULL,
                             temperature = 0.1) {
    # Check if PDF processing is available
    if (!.pdf_available()) {
        abort("PDF processing requires pdftools package. Please install with: install.packages('pdftools')")
    }

    # Validate file exists
    if (!file.exists(pdf_path)) {
        abort(glue("PDF file not found: {pdf_path}"))
    }

    # Extract text from PDF using functional approach
    pdf_text <- pdf_path |>
        pdf_text() |>
        str_c(collapse = " ") |>
        str_squish()

    # Create specialized prompt for PDF analysis using MSF system prompt
    pdf_prompt <- create_system_prompt(pdf_text)

    # Call LLM
    response <- llm_call_tidy(
        prompt = pdf_prompt,
        provider = provider,
        model = model,
        temperature = temperature
    )

    # Extract structured data
    structured_data <- extract_structured_data(response)

    return(structured_data)
}

#' Batch Process Multiple Reports
#'
#' Process multiple outbreak reports efficiently using purrr
#' Supports text files (.txt), PDF files (.pdf), and DOCX files (.docx)
#' DOCX files use simple plain text extraction (no format conversion needed)
#'
#' @param report_paths Character vector of file paths to process
#' @param provider Character string specifying LLM provider
#' @param parallel Logical whether to use parallel processing
#' @return Named list of results for each report
#' @export
batch_process_reports <- function(report_paths,
                                  provider = c("azure", "claude", "openai", "gemini", "ollama"),
                                  parallel = FALSE) {
    provider <- match.arg(provider)

    # Validate all files exist
    missing_files <- report_paths[!file.exists(report_paths)]
    if (length(missing_files) > 0) {
        abort(glue("Files not found: {str_c(missing_files, collapse = ', ')}"))
    }

    # Process function
    process_single <- function(path) {
        message(glue("Processing: {basename(path)}"))

        if (str_detect(path, "\\.pdf$")) {
            process_pdf_tidy(path, provider = provider)
        } else if (str_detect(path, "\\.docx$")) {
            process_docx_tidy(path, provider = provider)
        } else {
            # Handle text files
            text_content <- read_file(path)
            response <- llm_call_tidy(text_content, provider = provider)
            extract_structured_data(response)
        }
    }

    # Choose processing method
    if (parallel) {
        # Use future/furrr for parallel processing if available
        if (requireNamespace("furrr", quietly = TRUE)) {
            future::plan(future::multisession)
            results <- furrr::future_map(report_paths, process_single, .progress = TRUE)
        } else {
            warn("furrr package not available, falling back to sequential processing")
            results <- map(report_paths, process_single, .progress = TRUE)
        }
    } else {
        results <- map(report_paths, process_single, .progress = TRUE)
    }

    # Name results by filename
    names(results) <- basename(report_paths)

    return(results)
}

#' Validate Structured Output
#'
#' Validate that extracted linelist meets expected structure and quality standards
#'
#' @param structured_data List containing linelist tibble
#' @return Logical indicating whether data passes validation
#' @export
validate_structured_output <- function(structured_data) {
    # Required columns for linelist
    required_columns <- c("case_id", "name", "sex", "age", "onset_date", "outcome")

    # Check linelist
    linelist <- structured_data$linelist

    if (nrow(linelist) == 0) {
        warn("Linelist is empty")
        return(FALSE)
    }

    missing_cols <- setdiff(required_columns, names(linelist))
    if (length(missing_cols) > 0) {
        warn(glue("Missing required columns: {str_c(missing_cols, collapse = ', ')}"))
        return(FALSE)
    }

    # Check for duplicate case_ids
    if (any(duplicated(linelist$case_id))) {
        warn("Duplicate case_ids found in linelist")
        return(FALSE)
    }

    return(TRUE)
}

#' Convert DOCX to Markdown Format
#'
#' Convert Microsoft Word DOCX files to Markdown format to preserve document structure
#'
#' @param docx_path Character string path to the DOCX file
#' @param md_path Character string path for the output Markdown file (optional)
#' @param return_text Logical whether to return the extracted text content
#' @return Character string with Markdown content if return_text is TRUE, otherwise file path
#' @export
convert_docx_to_markdown <- function(docx_path, md_path = NULL, return_text = TRUE) {
    # Check if DOCX processing is available
    if (!.officer_available()) {
        abort("DOCX processing requires officer package. Please install with: install.packages('officer')")
    }

    # Validate input file exists
    if (!file.exists(docx_path)) {
        abort(glue("DOCX file not found: {docx_path}"))
    }

    # Check file extension
    if (!str_detect(tolower(docx_path), "\\.docx$")) {
        abort("Input file must have .docx extension")
    }

    # Extract structured content from DOCX
    tryCatch(
        {
            # Read DOCX file
            doc <- officer::read_docx(docx_path)

            # Extract all content with structure
            doc_content <- officer::docx_summary(doc)

            # Convert to markdown
            markdown_content <- convert_to_markdown(doc_content)

            # Handle output
            if (!is.null(md_path)) {
                # Write to file
                write_file(markdown_content, md_path)
                message(glue("Markdown file created: {md_path}"))

                if (return_text) {
                    return(list(markdown_content = markdown_content, md_path = md_path))
                } else {
                    return(md_path)
                }
            } else {
                # Return content only
                return(markdown_content)
            }
        },
        error = function(e) {
            abort(glue("Failed to process DOCX file: {e$message}"))
        }
    )
}

#' Convert DOCX to RTF Format
#'
#' Convert Microsoft Word DOCX files to RTF format for better text processing
#'
#' @param docx_path Character string path to the DOCX file
#' @param rtf_path Character string path for the output RTF file (optional)
#' @param return_text Logical whether to return the extracted text content
#' @return Character string with RTF content if return_text is TRUE, otherwise file path
#' @export
convert_docx_to_rtf <- function(docx_path, rtf_path = NULL, return_text = TRUE) {
    # Check if DOCX processing is available
    if (!.officer_available()) {
        abort("DOCX processing requires officer package. Please install with: install.packages('officer')")
    }

    # Validate input file exists
    if (!file.exists(docx_path)) {
        abort(glue("DOCX file not found: {docx_path}"))
    }

    # Check file extension
    if (!str_detect(tolower(docx_path), "\\.docx$")) {
        abort("Input file must have .docx extension")
    }
    # Extract text content from DOCX
    tryCatch(
        {
            # Read DOCX file
            doc <- officer::read_docx(docx_path)

            # Extract all text content
            doc_content <- officer::docx_summary(doc)

            # Filter for text content and combine
            text_content <- doc_content |>
                filter(.data$content_type == "paragraph") |>
                pull(.data$text) |>
                str_c(collapse = "\n") |>
                str_squish()

            # Create RTF content
            rtf_content <- create_rtf_content(text_content)

            # Handle output
            if (!is.null(rtf_path)) {
                # Write to file
                write_file(rtf_content, rtf_path)
                message(glue("RTF file created: {rtf_path}"))

                if (return_text) {
                    return(list(rtf_content = rtf_content, rtf_path = rtf_path))
                } else {
                    return(rtf_path)
                }
            } else {
                # Return content only
                return(rtf_content)
            }
        },
        error = function(e) {
            abort(glue("Failed to process DOCX file: {e$message}"))
        }
    )
}

#' Convert DOCX Content to Markdown
#'
#' Helper function to convert DOCX summary content to Markdown format
#'
#' @param doc_content Tibble from officer::docx_summary()
#' @return Character string with Markdown formatting
convert_to_markdown <- function(doc_content) {
    if (nrow(doc_content) == 0) {
        return("")
    }

    # Separate paragraphs and table content
    paragraphs <- doc_content |>
        filter(.data$content_type == "paragraph") |>
        mutate(
            markdown_text = case_when(
                # Handle different heading styles
                !is.na(.data$style_name) & str_detect(.data$style_name, "Title") ~ paste0("# ", .data$text),
                !is.na(.data$style_name) & str_detect(.data$style_name, "heading 1") ~ paste0("# ", .data$text),
                !is.na(.data$style_name) & str_detect(.data$style_name, "heading 2") ~ paste0("## ", .data$text),
                !is.na(.data$style_name) & str_detect(.data$style_name, "heading 3") ~ paste0("### ", .data$text),
                !is.na(.data$style_name) & str_detect(.data$style_name, "heading 4") ~ paste0("#### ", .data$text),
                !is.na(.data$style_name) & str_detect(.data$style_name, "heading 5") ~ paste0("##### ", .data$text),
                !is.na(.data$style_name) & str_detect(.data$style_name, "heading 6") ~ paste0("###### ", .data$text),
                # Handle list paragraphs
                !is.na(.data$style_name) & str_detect(.data$style_name, "List") ~ paste0("- ", .data$text),
                # Handle quotes
                !is.na(.data$style_name) & str_detect(.data$style_name, "Quote") ~ paste0("> ", .data$text),
                # Regular paragraphs
                TRUE ~ .data$text
            ),
            doc_order = .data$doc_index
        )

    # Process table content
    tables <- doc_content |>
        filter(.data$content_type == "table cell") |>
        group_by(.data$row_id) |>
        summarise(
            table_row = paste(.data$text, collapse = " | "),
            doc_order = min(.data$doc_index),
            is_header = any(.data$is_header %in% TRUE, na.rm = TRUE),
            .groups = "drop"
        ) |>
        arrange(.data$doc_order) |>
        mutate(
            markdown_text = paste0("| ", .data$table_row, " |")
        )

    # Add table separators for headers
    if (nrow(tables) > 0) {
        # Find header rows and add separators after them
        table_with_separators <- tables |>
            mutate(
                separator = if_else(
                    .data$is_header | row_number() == 1,
                    paste0("|", str_replace_all(.data$table_row, "[^|]", "-"), "|"),
                    NA_character_
                )
            ) |>
            # Create final table text with separators
            mutate(
                final_text = if_else(
                    !is.na(.data$separator),
                    paste(.data$markdown_text, .data$separator, sep = "\n"),
                    .data$markdown_text
                )
            )

        tables <- table_with_separators |>
            select(.data$doc_order, markdown_text = .data$final_text)
    }

    # Combine all content in document order
    all_content <- bind_rows(
        paragraphs |> select(.data$doc_order, .data$markdown_text),
        tables |> select(.data$doc_order, .data$markdown_text)
    ) |>
        arrange(.data$doc_order) |>
        pull(.data$markdown_text)

    # Clean up and format
    markdown_content <- all_content |>
        str_c(collapse = "\n\n") |>
        # Clean up extra whitespace
        str_replace_all("\\n{3,}", "\n\n") |>
        # Remove empty lines
        str_replace_all("\\n\\s*\\n", "\n\n") |>
        str_trim()

    return(markdown_content)
}

#' Create RTF Content
#'
#' Helper function to wrap text content in basic RTF format
#'
#' @param text_content Character string with plain text content
#' @return Character string with RTF formatting
create_rtf_content <- function(text_content) {
    # Basic RTF header and formatting
    rtf_header <- "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Times New Roman;}}\\f0\\fs24"
    rtf_footer <- "}"

    # Clean and format text for RTF
    formatted_text <- text_content |>
        str_replace_all("\\n", "\\\\par ") |> # Convert newlines to RTF paragraph breaks
        str_replace_all("\\{", "\\\\{") |> # Escape RTF special characters
        str_replace_all("\\}", "\\\\}") |>
        str_replace_all("\\\\(?!par |\\{|\\})", "\\\\\\\\") # Escape backslashes

    # Combine header, content, and footer
    glue("{rtf_header} {formatted_text}{rtf_footer}")
}

#' Extract Structured XML from DOCX
#'
#' Extract structured XML representation preserving document hierarchy and table structure
#' This provides much richer context for LLM analysis than plain text
#'
#' @param docx_path Character string path to DOCX file
#' @return Character string with structured XML representation
extract_docx_structured_xml <- function(docx_path) {
    # Check if DOCX processing is available
    if (!.officer_available()) {
        abort("DOCX processing requires officer package. Please install with: install.packages('officer')")
    }

    # Validate input file exists
    if (!file.exists(docx_path)) {
        abort(glue("DOCX file not found: {docx_path}"))
    }

    # Extract structured content
    tryCatch(
        {
            # Read DOCX file
            doc <- officer::read_docx(docx_path)

            # Extract all content with structure
            doc_content <- officer::docx_summary(doc)

            # Convert to structured XML format
            xml_elements <- doc_content |>
                filter(.data$content_type %in% c("paragraph", "table cell")) |>
                arrange(.data$doc_index) |>
                mutate(
                    # Clean text content and escape for XML
                    clean_text = str_trim(.data$text),
                    # Escape special XML characters
                    clean_text = stringr::str_replace_all(.data$clean_text, "&", "&amp;"),
                    clean_text = stringr::str_replace_all(.data$clean_text, "<", "&lt;"),
                    clean_text = stringr::str_replace_all(.data$clean_text, ">", "&gt;"),
                    clean_text = stringr::str_replace_all(.data$clean_text, "\"", "&quot;"),
                    clean_text = stringr::str_replace_all(.data$clean_text, "'", "&apos;"),

                    # Create XML representation
                    xml_element = case_when(
                        # Handle paragraphs with style information
                        .data$content_type == "paragraph" & !is.na(.data$style_name) ~ {
                            glue('<paragraph style="{.data$style_name}" index="{.data$doc_index}">{.data$clean_text}</paragraph>')
                        },
                        .data$content_type == "paragraph" ~ {
                            glue('<paragraph index="{.data$doc_index}">{.data$clean_text}</paragraph>')
                        },
                        # Handle table cells with full structure information
                        .data$content_type == "table cell" ~ {
                            header_attr <- if_else(!is.na(.data$is_header) & .data$is_header, ' header="true"', "")
                            glue('<table_cell row="{.data$row_id}" col="{.data$cell_id}" index="{.data$doc_index}"{header_attr}>{.data$clean_text}</table_cell>')
                        },
                        TRUE ~ glue('<element index="{.data$doc_index}">{.data$clean_text}</element>')
                    )
                ) |>
                filter(nchar(.data$clean_text) > 0) |> # Remove empty elements
                pull(.data$xml_element)

            # Wrap in document structure
            structured_xml <- c(
                '<?xml version="1.0" encoding="UTF-8"?>',
                "<document>",
                "  <metadata>",
                "    <source>MSF Outbreak Report</source>",
                "    <extraction_method>officer_structured</extraction_method>",
                glue("    <total_elements>{length(xml_elements)}</total_elements>"),
                "  </metadata>",
                "  <content>",
                paste0("    ", xml_elements),
                "  </content>",
                "</document>"
            )

            return(str_c(structured_xml, collapse = "\n"))
        },
        error = function(e) {
            abort(glue("Failed to extract structured XML from DOCX file: {e$message}"))
        }
    )
}

#' Extract Plain Text from DOCX
#'
#' Simple extraction of all text content from DOCX without format conversion
#' Legacy function - consider using extract_docx_structured_xml for better results
#'
#' @param docx_path Character string path to DOCX file
#' Extract Markdown from DOCX
#'
#' Extract content from DOCX file and format as clean Markdown with proper table formatting
#' This preserves table structure while being more readable than XML and more structured than plain text
#'
#' @param docx_path Character string path to DOCX file
#' @return Character string with Markdown formatting
extract_docx_markdown <- function(docx_path) {
    # Check if DOCX processing is available
    if (!.officer_available()) {
        abort("DOCX processing requires officer package. Please install with: install.packages('officer')")
    }

    # Validate input file exists
    if (!file.exists(docx_path)) {
        abort(glue("DOCX file not found: {docx_path}"))
    }

    # Extract structured content and convert to markdown
    tryCatch(
        {
            # Read DOCX file
            doc <- officer::read_docx(docx_path)

            # Extract all content with structure
            doc_content <- officer::docx_summary(doc)

            # Convert to clean markdown format
            markdown_content <- convert_to_markdown(doc_content)

            return(markdown_content)
        },
        error = function(e) {
            abort(glue("Failed to extract markdown from DOCX file: {e$message}"))
        }
    )
}

#' Extract Plain Text from DOCX
#'
#' Extract all text content from DOCX file (paragraphs and table cells)
#' This provides a simple text representation without formatting
#'
#' @param docx_path Character string path to DOCX file
#' @return Character string with all text content
extract_docx_text <- function(docx_path) {
    # Check if DOCX processing is available
    if (!.officer_available()) {
        abort("DOCX processing requires officer package. Please install with: install.packages('officer')")
    }

    # Validate input file exists
    if (!file.exists(docx_path)) {
        abort(glue("DOCX file not found: {docx_path}"))
    }

    # Extract all text content directly
    tryCatch(
        {
            # Read DOCX file
            doc <- officer::read_docx(docx_path)

            # Extract all content
            doc_content <- officer::docx_summary(doc)

            # Extract all text from paragraphs and table cells
            text_vector <- doc_content |>
                filter(.data$content_type %in% c("paragraph", "table cell")) |>
                arrange(.data$doc_index) |>
                pull(.data$text)

            # Remove empty or whitespace-only entries and combine
            all_text <- text_vector[nchar(str_trim(text_vector)) > 0] |>
                str_c(collapse = "\n") |>
                str_squish()

            return(all_text)
        },
        error = function(e) {
            abort(glue("Failed to extract text from DOCX file: {e$message}"))
        }
    )
}

#' Process DOCX with Structured XML Approach
#'
#' Extract structured XML from DOCX and process with LLM analysis (preserves document structure)
#' This provides much better context to the LLM than plain text extraction
#'
#' @param docx_path Character string path to DOCX file
#' @param provider Character string specifying LLM provider
#' @param model Character string specifying model (optional)
#' @param temperature Numeric value for response randomness
#' @param save_xml Logical whether to save the extracted structured XML
#' @return Named list with structured data and raw response
#' @export
process_docx_structured <- function(docx_path,
                                    provider = c("azure", "claude", "openai", "gemini", "ollama"),
                                    model = NULL,
                                    temperature = 0.1,
                                    save_xml = FALSE) {
    # Validate file exists
    if (!file.exists(docx_path)) {
        abort(glue("DOCX file not found: {docx_path}"))
    }

    # Extract structured XML directly
    message(glue("Extracting structured XML from DOCX: {basename(docx_path)}"))

    xml_content <- extract_docx_structured_xml(docx_path)

    # Optionally save the extracted XML for review
    if (save_xml) {
        xml_path <- str_replace(docx_path, "\\.docx$", "_structured.xml")
        write_file(xml_content, xml_path)
        message(glue("Structured XML saved to: {xml_path}"))
    }

    # Create prompt using MSF system prompt
    prompt <- create_system_prompt(xml_content)

    # Call LLM
    response <- llm_call_tidy(
        prompt = prompt,
        provider = provider,
        model = model,
        temperature = temperature
    )

    # Extract structured data
    structured_data <- extract_structured_data(response)

    return(structured_data)
}

#' Process DOCX with Tidyverse Approach
#'
#' Extract text from DOCX and process with LLM analysis (no format conversion needed)
#' Legacy function - consider using process_docx_structured for better results
#'
#' @param docx_path Character string path to DOCX file
#' @param provider Character string specifying LLM provider
#' @param model Character string specifying model (optional)
#' @param temperature Numeric value for response randomness
#' @param save_text Logical whether to save the extracted plain text
#' @return Named list with structured data and raw response
#' @export
process_docx_tidy <- function(docx_path,
                              provider = c("azure", "claude", "openai", "gemini", "ollama"),
                              model = NULL,
                              temperature = 0.1,
                              save_text = FALSE) {
    # Validate file exists
    if (!file.exists(docx_path)) {
        abort(glue("DOCX file not found: {docx_path}"))
    }

    # Extract plain text directly
    message(glue("Extracting text from DOCX: {basename(docx_path)}"))

    text_content <- extract_docx_text(docx_path)

    # Optionally save the extracted text for review
    if (save_text) {
        text_path <- str_replace(docx_path, "\\.docx$", "_extracted.txt")
        write_file(text_content, text_path)
        message(glue("Extracted text saved to: {text_path}"))
    }

    # Create prompt using MSF system prompt
    prompt <- create_system_prompt(text_content)

    # Call LLM
    response <- llm_call_tidy(
        prompt = prompt,
        provider = provider,
        model = model,
        temperature = temperature
    )

    # Extract structured data
    structured_data <- extract_structured_data(response)

    return(structured_data)
}

# Example usage and testing functions
if (FALSE) {
    # Example: Process a single text report using Azure (default secure provider)
    test_report <- "On July 3, 2023, health officials reported measles cases..."
    result <- llm_call_tidy(test_report) # Uses Azure by default
    structured <- extract_structured_data(result)

    # Example: Explicitly using Azure OpenAI with specific model
    result_azure <- llm_call_tidy(test_report, provider = "azure", model = "gpt-4o")
    structured_azure <- extract_structured_data(result_azure)

    # Example: Using other providers (will require security confirmation)
    # result_claude <- llm_call_tidy(test_report, provider = "claude")  # Will prompt for confirmation

    # Example: Process a PDF (uses Azure by default)
    # pdf_result <- process_pdf_tidy("outbreak_report.pdf")

    # Example: Extract plain text from DOCX (recommended approach - no conversion needed)
    # text_content <- extract_docx_text("outbreak_report.docx")

    # Example: Process DOCX file directly (uses Azure by default, plain text extraction)
    # docx_result <- process_docx_tidy("outbreak_report.docx")
    # docx_result_with_saved_text <- process_docx_tidy("outbreak_report.docx", save_text = TRUE)

    # Example: Legacy format conversion options (if needed for other purposes)
    # md_content <- convert_docx_to_markdown("outbreak_report.docx")  # Markdown conversion
    # rtf_content <- convert_docx_to_rtf("outbreak_report.docx")      # RTF conversion

    # Example: Batch process multiple files including DOCX (uses Azure by default)
    # batch_results <- batch_process_reports(
    #   c("report1.txt", "report2.pdf", "report3.docx")
    # )
}

#' Check if MLX CLI is Available
#'
#' Verify that the llm CLI tool with mlx plugin is installed
#'
#' @return Logical indicating if MLX is available
#' @export
check_mlx_available <- function() {
    # Check if llm command exists
    result <- suppressWarnings(
        system2("which", args = "llm", stdout = TRUE, stderr = TRUE)
    )

    status <- attr(result, "status")
    if (length(result) == 0 || (!is.null(status) && status == 1)) {
        return(FALSE)
    }

    # Check if llm-mlx plugin is installed
    plugin_check <- suppressWarnings(
        system2("llm", args = "plugins", stdout = TRUE, stderr = TRUE)
    )

    if (length(plugin_check) == 0) {
        return(FALSE)
    }

    # Check if llm-mlx is in the plugin list
    return(any(str_detect(plugin_check, "llm-mlx")))
}

#' Check if MLX Model is Available
#'
#' Verify that a specific MLX model has been downloaded
#'
#' @param model Character string with model name (e.g., "mlx-community/Llama-3.2-3B-Instruct-4bit")
#' @return Logical indicating if model is available
#' @export
check_mlx_model <- function(model) {
    # List available MLX models
    result <- suppressWarnings(
        system2("llm", args = "models", stdout = TRUE, stderr = TRUE)
    )

    if (length(result) == 0) {
        return(FALSE)
    }

    # Check if our model is in the list
    return(any(str_detect(result, fixed(model))))
}

#' Call MLX VLM via CLI
#'
#' Execute python mlx-vlm module to call an MLX vision-language model
#'
#' @param prompt Character string containing the prompt
#' @param model Character string with model name
#' @param temperature Numeric value for response randomness
#' @param debug Logical whether to print debug info
#' @return Character string with LLM response
#' @export
call_mlx_llm <- function(prompt, model, temperature = 0.1, max_tokens = 16384, system_prompt = NULL,
                         kv_bits = NULL, kv_quant_scheme = NULL, debug = FALSE) {
    # Validate inputs
    if (!is.character(prompt) || length(prompt) != 1 || nchar(prompt) == 0) {
        abort("Prompt must be a non-empty character string")
    }

    # Pre-pend system prompt if provided
    full_prompt <- prompt
    if (!is.null(system_prompt)) {
        full_prompt <- glue::glue("{system_prompt}\n\n{prompt}")
    }

    # Write prompt to temporary file to avoid argument length limits
    temp_file <- tempfile(fileext = ".txt")
    on.exit(unlink(temp_file), add = TRUE)
    write_file(full_prompt, temp_file)

    # Use our custom wrapper script that disables Qwen3.5 thinking mode
    venv_python <- "~/mlx-env/bin/python3"
    wrapper_script <- here::here("scripts", "mlx_generate.py")

    # Build command arguments
    args <- c(
        wrapper_script,
        "--model", model,
        "--temperature", as.character(temperature),
        "--prompt", glue("@{temp_file}")
    )

    if (!is.null(max_tokens)) {
        args <- c(args, "--max-tokens", as.character(max_tokens))
    }

    if (!is.null(system_prompt)) {
        args <- c(args, "--system", shQuote(system_prompt))
    }

    if (!is.null(kv_bits)) {
        args <- c(args, "--kv-bits", as.character(kv_bits))
    }

    if (!is.null(kv_quant_scheme)) {
        args <- c(args, "--kv-quant-scheme", kv_quant_scheme)
    }

    if (debug) {
        message("=== MLX VLM COMMAND ===")
        args_with_verbose <- c(args, "--verbose")
        message("Starting system call...")
    }

    # Capture stdout (JSON response) and stderr (timing + debug) separately
    output_file <- tempfile(fileext = ".txt")
    stderr_file <- tempfile(fileext = ".txt")
    on.exit(unlink(output_file), add = TRUE)
    on.exit(unlink(stderr_file), add = TRUE)

    # Tee stderr: streamed tokens appear live in the R process log AND are saved
    # to stderr_file for MLX_TIMING parsing. Requires bash process substitution.
    command_str <- glue("PYTHONUNBUFFERED=1 {venv_python} {str_c(args, collapse = ' ')} > {output_file} 2> >(tee {stderr_file} >&2)")

    message("Running MLX inference...")
    tryCatch(
        {
            # Wrap in bash so process substitution (2> >(tee ...)) works
            status <- system(paste("bash -c", shQuote(command_str)), intern = FALSE)
        },
        error = function(e) {
            message("System call failed: ", e$message)
            stop(e)
        }
    )

    if (debug) {
        message("Finished system call. Status: ", status)
    }

    # Check for errors
    if (status != 0) {
        if (file.exists(stderr_file)) {
            err_output <- read_file(stderr_file)
            message("MLX stderr: ", str_trunc(err_output, 1000))
        }
        abort(glue("MLX VLM execution failed with status {status}. Check output above for details."))
    }

    # Read response from stdout
    response <- read_file(output_file)

    # Parse timing from stderr
    mlx_timing <- list(
        generation_secs = NA_real_, prompt_tokens = NA_integer_,
        output_tokens = NA_integer_, prompt_tps = NA_real_,
        generation_tps = NA_real_, peak_memory_gb = NA_real_
    )
    if (file.exists(stderr_file)) {
        stderr_lines <- readLines(stderr_file)
        timing_line <- stderr_lines[stringr::str_detect(stderr_lines, "^MLX_TIMING:")]
        if (length(timing_line) > 0) {
            timing_line <- timing_line[1]
            parse_num <- function(key) {
                m <- stringr::str_match(timing_line, glue::glue("{key}=([0-9.eE+-]+|NA)"))
                if (!is.na(m[1, 2]) && m[1, 2] != "NA") as.numeric(m[1, 2]) else NA_real_
            }
            mlx_timing$generation_secs <- parse_num("generation_secs")
            mlx_timing$prompt_tokens <- as.integer(parse_num("prompt_tokens"))
            mlx_timing$output_tokens <- as.integer(parse_num("output_tokens"))
            mlx_timing$prompt_tps <- parse_num("prompt_tps")
            mlx_timing$generation_tps <- parse_num("generation_tps")
            mlx_timing$peak_memory_gb <- parse_num("peak_memory_gb")
            message(glue::glue(
                "MLX: {round(mlx_timing$generation_secs, 1)}s | ",
                "{mlx_timing$output_tokens} out-tokens | ",
                "prefill {round(mlx_timing$prompt_tps, 0)} tok/s | ",
                "gen {round(mlx_timing$generation_tps, 1)} tok/s",
                if (!is.na(mlx_timing$peak_memory_gb)) " | {round(mlx_timing$peak_memory_gb, 1)} GB" else ""
            ))
        }
        if (debug) {
            message("=== MLX STDERR ===")
            message(paste(stderr_lines, collapse = "\n"))
        }
    }

    # Attach timing as attributes so callers can propagate it
    attr(response, "mlx_timing") <- mlx_timing

    if (debug) {
        message("=== MLX RESPONSE (first 500 chars) ===")
        message(str_trunc(response, 500))
        message("=== END ===")
    }

    return(response)
}

#' Call Transformers LLM via Python Bridge
#'
#' Execute Python script to call a Hugging Face Transformers model
#'
#' @param prompt Character string containing the prompt
#' @param model Character string with model path (e.g., /sc-resources/llms/qwen/Qwen3-32B)
#' @param temperature Numeric value for response randomness
#' @param max_tokens Integer for maximum tokens to generate
#' @param device Character string "cuda" or "cpu"
#' @param debug Logical whether to print debug info
#' @return Character string with LLM response
#' @export
call_transformers_llm <- function(prompt, model, temperature = 0.1, max_tokens = 16000, device = "cuda", debug = FALSE) {
    # Validate inputs
    if (!is.character(prompt) || length(prompt) != 1 || nchar(prompt) == 0) {
        abort("Prompt must be a non-empty character string")
    }

    # Check Python script exists
    python_script <- here::here("scripts", "core", "transformers_inference.py")
    if (!file.exists(python_script)) {
        abort(glue("Transformers inference script not found: {python_script}"))
    }

    # Write prompt to temporary file to avoid argument length limits
    temp_file <- tempfile(fileext = ".txt")
    on.exit(unlink(temp_file), add = TRUE)
    write_file(prompt, temp_file)

    # Build command
    cmd_args <- c(
        python_script,
        "--model", model,
        "--temperature", as.character(temperature),
        "--max-tokens", as.character(max_tokens),
        "--device", device
    )

    if (debug) {
        message("=== TRANSFORMERS PYTHON COMMAND ===")
        message(glue("python {str_c(cmd_args, collapse = ' ')} < {temp_file}"))
        message("=== PROMPT (first 500 chars) ===")
        message(str_trunc(prompt, 500))
        message("Starting Python inference...")
    }

    # Execute command with prompt from stdin
    output_file <- tempfile(fileext = ".txt")
    on.exit(unlink(output_file), add = TRUE)

    # Build full command with input/output redirection
    full_cmd <- glue("python {str_c(cmd_args, collapse = ' ')} < {temp_file} > {output_file} 2>&1")

    if (debug) {
        message("Full command: ", full_cmd)
    }

    # Execute
    status <- system(full_cmd, intern = FALSE)

    if (status != 0) {
        # Read error output
        if (file.exists(output_file)) {
            error_msg <- read_file(output_file)
            abort(glue("Transformers call failed with status {status}:\n{error_msg}"))
        } else {
            abort(glue("Transformers call failed with status {status}"))
        }
    }

    # Read response from output file
    if (!file.exists(output_file)) {
        abort("Output file not created by Python script")
    }

    response <- read_file(output_file)

    if (debug) {
        message("=== RESPONSE (first 500 chars) ===")
        message(str_trunc(response, 500))
    }

    return(response)
}

#' Call LLM on HPC Cluster via SSH
#'
#' Submit an LLM inference job to the Charité HPC cluster and wait for results.
#' Requires SSH key authentication to be set up for the cluster.
#'
#' @param prompt Character string containing the prompt
#' @param model Character string specifying the model path on the cluster
#' @param temperature Numeric sampling temperature
#' @param max_tokens Integer maximum tokens to generate
#' @param debug Logical whether to print debug info
#' @param cluster_user Character string SSH username (default: biqu10)
#' @param cluster_host Character string SSH hostname (default: s-sc-frontend2.charite.de)
#' @param cluster_dir Character string working directory on cluster
#' @param wait_timeout Integer seconds to wait for job completion (default: 1800 = 30 mins)
#' @param poll_interval Integer seconds between status checks (default: 10)
#' @return Character string with LLM response
#' @export
call_cluster_llm <- function(prompt, model = "/sc-resources/llms/Qwen/Qwen3-32B",
                             temperature = 0.1, max_tokens = 16000,
                             debug = FALSE,
                             cluster_user = "biqu10",
                             cluster_host = "s-sc-frontend2.charite.de",
                             cluster_dir = "/home/biqu10/cluster_inference",
                             wait_timeout = 1800,
                             poll_interval = 10) {
    # Validate inputs
    if (!is.character(prompt) || length(prompt) != 1 || nchar(prompt) == 0) {
        abort("Prompt must be a non-empty character string")
    }

    # Generate unique job ID based on timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    job_id <- glue("msf_validation_{timestamp}")

    # Local temp files
    local_prompt_file <- tempfile(pattern = "cluster_prompt_", fileext = ".txt")
    local_output_file <- tempfile(pattern = "cluster_output_", fileext = ".txt")
    on.exit(
        {
            unlink(local_prompt_file)
            unlink(local_output_file)
        },
        add = TRUE
    )

    # Remote file paths
    remote_prompt_file <- glue("{cluster_dir}/prompts/prompt_{timestamp}.txt")
    remote_output_file <- glue("{cluster_dir}/outputs/output_{timestamp}.txt")

    # Write prompt to local temp file
    write_file(prompt, local_prompt_file)

    if (debug) {
        message("=== CLUSTER JOB SUBMISSION ===")
        message(glue("Job ID: {job_id}"))
        message(glue("Model: {model}"))
        message(glue("Max tokens: {max_tokens}"))
        message(glue("Prompt length: {nchar(prompt)} characters"))
    }

    # Step 1: Copy prompt file to cluster
    message("Uploading prompt to cluster...")
    scp_cmd <- glue("scp {local_prompt_file} {cluster_user}@{cluster_host}:{remote_prompt_file}")
    scp_result <- system(scp_cmd, intern = FALSE)
    if (scp_result != 0) {
        abort(glue("Failed to upload prompt to cluster. Ensure SSH keys are set up for {cluster_user}@{cluster_host}"))
    }

    # Step 2: Submit SLURM job
    message("Submitting job to SLURM queue...")
    sbatch_cmd <- glue(
        "ssh {cluster_user}@{cluster_host} 'sbatch --parsable {cluster_dir}/run_single_inference.sh ",
        "\"{model}\" {temperature} {max_tokens} \"{remote_prompt_file}\" \"{remote_output_file}\"'"
    )

    if (debug) {
        message(glue("SBATCH command: {sbatch_cmd}"))
    }

    slurm_job_id <- system(sbatch_cmd, intern = TRUE)

    if (length(slurm_job_id) == 0 || !grepl("^[0-9]+$", slurm_job_id[1])) {
        abort(glue("Failed to submit SLURM job. Response: {paste(slurm_job_id, collapse = '\\n')}"))
    }

    slurm_job_id <- slurm_job_id[1]
    message(glue("Job submitted with SLURM ID: {slurm_job_id}"))

    # Step 3: Wait for job completion
    message("Waiting for job to complete...")
    start_time <- Sys.time()
    job_complete <- FALSE

    while (!job_complete) {
        # Check elapsed time
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        if (elapsed > wait_timeout) {
            # Try to cancel the job before giving up
            cancel_cmd <- glue("ssh {cluster_user}@{cluster_host} 'scancel {slurm_job_id}'")
            system(cancel_cmd, intern = FALSE)
            abort(glue("Job timeout after {round(elapsed/60, 1)} minutes. Job {slurm_job_id} cancelled."))
        }

        # Check job status
        status_cmd <- glue("ssh {cluster_user}@{cluster_host} 'squeue -j {slurm_job_id} -h -o %T 2>/dev/null || echo COMPLETED'")
        job_status <- system(status_cmd, intern = TRUE)

        if (length(job_status) == 0 || job_status[1] == "" || job_status[1] == "COMPLETED") {
            # Job no longer in queue - either completed or failed
            job_complete <- TRUE
        } else {
            if (debug) {
                message(glue("Job status: {job_status[1]} (elapsed: {round(elapsed/60, 1)} min)"))
            } else {
                cat(".")
            }
            Sys.sleep(poll_interval)
        }
    }
    cat("\n")
    message("Job completed. Downloading results...")

    # Step 4: Download output file
    scp_download_cmd <- glue("scp {cluster_user}@{cluster_host}:{remote_output_file} {local_output_file}")
    download_result <- system(scp_download_cmd, intern = FALSE)

    if (download_result != 0) {
        # Check if output file exists on cluster
        check_cmd <- glue("ssh {cluster_user}@{cluster_host} 'ls -la {remote_output_file} 2>&1'")
        check_result <- system(check_cmd, intern = TRUE)
        abort(glue("Failed to download output file. Remote file check: {paste(check_result, collapse = '\\n')}"))
    }

    # Step 5: Read and return response
    if (!file.exists(local_output_file)) {
        abort("Output file was not downloaded successfully")
    }

    response <- read_file(local_output_file)

    if (nchar(response) == 0) {
        # Check SLURM logs for errors
        log_cmd <- glue("ssh {cluster_user}@{cluster_host} 'cat ~/slurm-{slurm_job_id}.out 2>/dev/null || echo No log found'")
        log_output <- system(log_cmd, intern = TRUE)
        abort(glue("Empty response from cluster. SLURM log: {paste(log_output, collapse = '\\n')}"))
    }

    message(glue("Successfully received {nchar(response)} characters from cluster"))

    # Step 6: Query SLURM for actual execution time (excludes queue wait)
    message("Querying SLURM for execution time...")
    slurm_exec_time <- get_slurm_execution_time(
        slurm_job_id = slurm_job_id,
        cluster_user = cluster_user,
        cluster_host = cluster_host
    )

    if (!is.na(slurm_exec_time)) {
        message(glue("Job executed in {round(slurm_exec_time/60, 1)} minutes (GPU time)"))
    }

    if (debug) {
        message("=== RESPONSE (first 500 chars) ===")
        message(str_trunc(response, 500))
    }

    # Return list with response and timing metadata
    return(list(
        response = response,
        slurm_job_id = slurm_job_id,
        slurm_execution_time = slurm_exec_time
    ))
}

#' Submit Cluster Job Without Waiting (Async)
#'
#' Submit an LLM inference job to the HPC cluster and return immediately.
#' Use collect_cluster_result() to retrieve the output later.
#'
#' @param prompt Character string containing the prompt
#' @param job_id Unique identifier for this job (used for file naming)
#' @param model Character string specifying the model path on the cluster
#' @param temperature Numeric sampling temperature
#' @param max_tokens Integer maximum tokens to generate
#' @param debug Logical whether to print debug info
#' @param cluster_user Character string SSH username
#' @param cluster_host Character string SSH hostname
#' @param cluster_dir Character string working directory on cluster
#' @param run_folder Character string subfolder name for this run (optional, created if provided)
#' @return List with slurm_job_id, remote_output_file, and job metadata
#' @export
submit_cluster_job_async <- function(prompt, job_id,
                                     model = "/sc-resources/llms/Qwen/Qwen3-32B",
                                     temperature = 0.1, max_tokens = 16000,
                                     debug = FALSE,
                                     cluster_user = "biqu10",
                                     cluster_host = "s-sc-frontend2.charite.de",
                                     cluster_dir = "/home/biqu10/cluster_inference",
                                     run_folder = NULL) {
    # Validate inputs
    if (!is.character(prompt) || length(prompt) != 1 || nchar(prompt) == 0) {
        abort("Prompt must be a non-empty character string")
    }

    # Generate timestamp for unique file naming
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    # Add random suffix to avoid collisions when submitting rapidly
    random_suffix <- sprintf("%04d", sample(1000:9999, 1))
    file_id <- glue("{job_id}_{random_suffix}")

    # Local temp file for prompt
    local_prompt_file <- tempfile(pattern = "cluster_prompt_", fileext = ".txt")

    # Determine output paths - use run_folder if provided
    if (!is.null(run_folder) && nchar(run_folder) > 0) {
        remote_prompt_dir <- glue("{cluster_dir}/prompts/{run_folder}")
        remote_output_dir <- glue("{cluster_dir}/outputs/{run_folder}")
    } else {
        remote_prompt_dir <- glue("{cluster_dir}/prompts")
        remote_output_dir <- glue("{cluster_dir}/outputs")
    }

    remote_prompt_file <- glue("{remote_prompt_dir}/prompt_{file_id}.txt")
    remote_output_file <- glue("{remote_output_dir}/output_{file_id}.txt")

    # Write prompt to local temp file
    write_file(prompt, local_prompt_file)

    if (debug) {
        message("=== ASYNC CLUSTER JOB SUBMISSION ===")
        message(glue("Job ID: {job_id}"))
        message(glue("File ID: {file_id}"))
        message(glue("Run folder: {run_folder %||% '(none)'}"))
        message(glue("Model: {model}"))
        message(glue("Prompt length: {nchar(prompt)} characters"))
    }

    # Step 1: Create directories and copy prompt file to cluster
    if (!is.null(run_folder) && nchar(run_folder) > 0) {
        mkdir_cmd <- glue("ssh {cluster_user}@{cluster_host} 'mkdir -p {remote_prompt_dir} {remote_output_dir}'")
        system(mkdir_cmd, intern = FALSE)
    }

    message(glue("Uploading prompt for job {job_id}..."))
    scp_cmd <- glue("scp {local_prompt_file} {cluster_user}@{cluster_host}:{remote_prompt_file}")
    scp_result <- system(scp_cmd, intern = FALSE)

    # Clean up local temp file
    unlink(local_prompt_file)

    if (scp_result != 0) {
        abort(glue("Failed to upload prompt to cluster for job {job_id}"))
    }

    # Step 2: Submit SLURM job (don't wait)
    sbatch_cmd <- glue(
        "ssh {cluster_user}@{cluster_host} 'sbatch --parsable {cluster_dir}/run_single_inference.sh ",
        "\"{model}\" {temperature} {max_tokens} \"{remote_prompt_file}\" \"{remote_output_file}\"'"
    )

    if (debug) {
        message(glue("SBATCH command: {sbatch_cmd}"))
    }

    slurm_job_id <- system(sbatch_cmd, intern = TRUE)

    if (length(slurm_job_id) == 0 || !grepl("^[0-9]+$", slurm_job_id[1])) {
        abort(glue("Failed to submit SLURM job {job_id}. Response: {paste(slurm_job_id, collapse = '\\n')}"))
    }

    slurm_job_id <- slurm_job_id[1]
    message(glue("Job {job_id} submitted with SLURM ID: {slurm_job_id}"))

    # Return job metadata for later collection
    list(
        job_id = job_id,
        slurm_job_id = slurm_job_id,
        remote_output_file = remote_output_file,
        cluster_user = cluster_user,
        cluster_host = cluster_host,
        submit_time = Sys.time()
    )
}

#' Collect Results from Async Cluster Job
#'
#' Wait for a previously submitted cluster job to complete and download results.
#'
#' @param job_info List returned by submit_cluster_job_async()
#' @param wait_timeout Integer seconds to wait for job completion
#' @param poll_interval Integer seconds between status checks
#' @param debug Logical whether to print debug info
#' @return Character string with LLM response
#' @export
collect_cluster_result <- function(job_info, wait_timeout = 1800, poll_interval = 10, debug = FALSE) {
    slurm_job_id <- job_info$slurm_job_id
    remote_output_file <- job_info$remote_output_file
    cluster_user <- job_info$cluster_user
    cluster_host <- job_info$cluster_host
    job_id <- job_info$job_id

    message(glue("Waiting for job {job_id} (SLURM {slurm_job_id})..."))

    # Wait for job completion
    start_time <- Sys.time()
    job_complete <- FALSE

    while (!job_complete) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        if (elapsed > wait_timeout) {
            cancel_cmd <- glue("ssh {cluster_user}@{cluster_host} 'scancel {slurm_job_id}'")
            system(cancel_cmd, intern = FALSE)
            abort(glue("Job {job_id} timeout after {round(elapsed/60, 1)} minutes. Job cancelled."))
        }

        status_cmd <- glue("ssh {cluster_user}@{cluster_host} 'squeue -j {slurm_job_id} -h -o %T 2>/dev/null || echo COMPLETED'")
        job_status <- system(status_cmd, intern = TRUE)

        if (length(job_status) == 0 || job_status[1] == "" || job_status[1] == "COMPLETED") {
            job_complete <- TRUE
        } else {
            if (debug) {
                message(glue("Job {job_id} status: {job_status[1]} (elapsed: {round(elapsed/60, 1)} min)"))
            }
            Sys.sleep(poll_interval)
        }
    }

    message(glue("Job {job_id} completed. Downloading results..."))

    # Download output file
    local_output_file <- tempfile(pattern = "cluster_output_", fileext = ".txt")
    scp_download_cmd <- glue("scp {cluster_user}@{cluster_host}:{remote_output_file} {local_output_file}")
    download_result <- system(scp_download_cmd, intern = FALSE)

    if (download_result != 0) {
        check_cmd <- glue("ssh {cluster_user}@{cluster_host} 'ls -la {remote_output_file} 2>&1'")
        check_result <- system(check_cmd, intern = TRUE)
        abort(glue("Failed to download output for job {job_id}. Check: {paste(check_result, collapse = '\\n')}"))
    }

    response <- read_file(local_output_file)
    unlink(local_output_file)

    if (nchar(response) == 0) {
        log_cmd <- glue("ssh {cluster_user}@{cluster_host} 'cat ~/slurm-{slurm_job_id}.out 2>/dev/null || echo No log found'")
        log_output <- system(log_cmd, intern = TRUE)
        abort(glue("Empty response for job {job_id}. SLURM log: {paste(log_output, collapse = '\\n')}"))
    }

    message(glue("Job {job_id}: received {nchar(response)} characters"))

    # Query SLURM for execution time immediately after download
    slurm_exec_time <- get_slurm_execution_time(
        slurm_job_id = slurm_job_id,
        cluster_user = cluster_user,
        cluster_host = cluster_host
    )

    if (!is.na(slurm_exec_time) && debug) {
        message(glue("Job {job_id}: executed in {round(slurm_exec_time/60, 1)} minutes"))
    }

    return(list(
        response = response,
        slurm_execution_time = slurm_exec_time
    ))
}

#' Submit Batch of Cluster Jobs
#'
#' Submit multiple LLM inference jobs to the cluster queue at once.
#' Returns immediately after all jobs are queued.
#' Creates a timestamped run folder to organize outputs.
#'
#' @param prompts Named list of prompts (names become job IDs)
#' @param model Character string specifying the model path
#' @param temperature Numeric sampling temperature
#' @param max_tokens Integer maximum tokens to generate
#' @param run_name Optional name for this run (used in folder name, default: auto-generated timestamp)
#' @param debug Logical whether to print debug info
#' @return List of job_info objects for use with collect_cluster_batch()
#' @export
submit_cluster_batch <- function(prompts, model = "/sc-resources/llms/Qwen/Qwen3-32B",
                                 temperature = 0.1, max_tokens = 16000,
                                 run_name = NULL, debug = FALSE) {
    # Generate run folder name with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    if (!is.null(run_name) && nchar(run_name) > 0) {
        run_folder <- glue("run_{run_name}_{timestamp}")
    } else {
        run_folder <- glue("run_{timestamp}")
    }

    message(glue("Submitting batch of {length(prompts)} jobs to cluster..."))
    message(glue("Run folder: {run_folder}"))

    job_infos <- list()
    # Store run folder in the batch metadata
    attr(job_infos, "run_folder") <- run_folder

    for (job_id in names(prompts)) {
        prompt <- prompts[[job_id]]
        tryCatch(
            {
                job_info <- submit_cluster_job_async(
                    prompt = prompt,
                    job_id = job_id,
                    model = model,
                    temperature = temperature,
                    max_tokens = max_tokens,
                    run_folder = run_folder,
                    debug = debug
                )
                job_info$run_folder <- run_folder
                job_infos[[job_id]] <- job_info
                # Small delay to avoid overwhelming the scheduler
                Sys.sleep(0.5)
            },
            error = function(e) {
                warning(glue("Failed to submit job {job_id}: {e$message}"))
                job_infos[[job_id]] <<- list(
                    job_id = job_id,
                    error = e$message,
                    failed = TRUE,
                    run_folder = run_folder
                )
            }
        )
    }

    message(glue("Submitted {sum(sapply(job_infos, function(x) !isTRUE(x$failed)))} jobs successfully"))
    message(glue("Outputs will be in: ~/cluster_inference/outputs/{run_folder}/"))
    return(job_infos)
}

#' Get SLURM Job Execution Time via sacct
#'
#' Query SLURM accounting database to get actual job execution time
#' (excludes queue wait time).
#'
#' @param slurm_job_id Character or numeric SLURM job ID
#' @param cluster_user Character username for SSH
#' @param cluster_host Character hostname for SSH
#' @return Numeric execution time in seconds, or NA if unavailable
#' @export
get_slurm_execution_time <- function(slurm_job_id, cluster_user = "biqu10", cluster_host = "charite-cluster") {
    tryCatch(
        {
            # Use sacct to get elapsed time for the .batch step (actual execution)
            # Support both SSH config aliases (e.g., "charite-cluster") and full hostnames
            ssh_target <- if (grepl("@", cluster_host) || grepl("\\.", cluster_host)) {
                # Full hostname or already includes user
                cluster_host
            } else {
                # SSH config alias - don't prepend user
                cluster_host
            }

            sacct_cmd <- if (ssh_target == cluster_host && !grepl("@", ssh_target)) {
                # SSH config alias
                glue::glue("ssh {cluster_host} 'sacct -j {slurm_job_id} --format=Elapsed -P -n 2>/dev/null | head -1'")
            } else {
                # Full hostname
                glue::glue("ssh {cluster_user}@{cluster_host} 'sacct -j {slurm_job_id} --format=Elapsed -P -n 2>/dev/null | head -1'")
            }

            elapsed_str <- system(sacct_cmd, intern = TRUE)

            if (length(elapsed_str) == 0 || elapsed_str[1] == "" || is.na(elapsed_str[1])) {
                return(NA_real_)
            }

            # Parse HH:MM:SS or D-HH:MM:SS format
            elapsed_str <- trimws(elapsed_str[1])

            # Handle day format (D-HH:MM:SS)
            if (grepl("-", elapsed_str)) {
                parts <- strsplit(elapsed_str, "-")[[1]]
                days <- as.numeric(parts[1])
                time_part <- parts[2]
            } else {
                days <- 0
                time_part <- elapsed_str
            }

            # Parse HH:MM:SS
            time_parts <- as.numeric(strsplit(time_part, ":")[[1]])
            if (length(time_parts) == 3) {
                hours <- time_parts[1]
                mins <- time_parts[2]
                secs <- time_parts[3]
            } else if (length(time_parts) == 2) {
                hours <- 0
                mins <- time_parts[1]
                secs <- time_parts[2]
            } else {
                return(NA_real_)
            }

            total_secs <- (days * 86400) + (hours * 3600) + (mins * 60) + secs
            return(total_secs)
        },
        error = function(e) {
            warning(glue::glue("Failed to get SLURM timing for job {slurm_job_id}: {e$message}"))
            return(NA_real_)
        }
    )
}

#' Collect Results from Batch of Cluster Jobs
#'
#' Wait for all submitted cluster jobs to complete and collect results.
#'
#' @param job_infos List of job_info objects from submit_cluster_batch()
#' @param wait_timeout Integer seconds to wait per job
#' @param poll_interval Integer seconds between status checks
#' @param debug Logical whether to print debug info
#' @return Named list of responses (same names as input prompts)
#' @export
collect_cluster_batch <- function(job_infos, wait_timeout = 1800, poll_interval = 10, debug = FALSE) {
    message(glue("Collecting results from {length(job_infos)} jobs..."))

    results <- list()

    for (job_id in names(job_infos)) {
        job_info <- job_infos[[job_id]]

        if (isTRUE(job_info$failed)) {
            warning(glue("Skipping failed job {job_id}"))
            results[[job_id]] <- list(error = job_info$error, response = NULL)
            next
        }

        tryCatch(
            {
                result <- collect_cluster_result(
                    job_info = job_info,
                    wait_timeout = wait_timeout,
                    poll_interval = poll_interval,
                    debug = debug
                )

                # collect_cluster_result now returns list with response and timing
                results[[job_id]] <- list(
                    response = result$response,
                    error = NULL,
                    slurm_job_id = job_info$slurm_job_id,
                    slurm_execution_time = result$slurm_execution_time
                )
            },
            error = function(e) {
                warning(glue("Failed to collect job {job_id}: {e$message}"))
                results[[job_id]] <<- list(error = e$message, response = NULL)
            }
        )
    }

    successful <- sum(sapply(results, function(x) !is.null(x$response)))
    message(glue("Collected {successful}/{length(results)} jobs successfully"))

    return(results)
}

#' Estimate Cost of LLM Call
#'
#' Estimate the cost of an LLM call based on token usage and provider rates
#'
#' @param model Character string specifying the model
#' @param prompt_tokens Integer number of input tokens
#' @param completion_tokens Integer number of output tokens
#' @return Numeric value with estimated cost in USD
#' @export
estimate_cost <- function(model, prompt_tokens, completion_tokens) {
    # Rates per 1M tokens (approximate; update when provider pricing changes)
    rates <- list(
        "gpt-4o" = list(input = 2.50, output = 10.00),
        "gpt-4-turbo" = list(input = 10.00, output = 30.00),
        "gpt-4" = list(input = 30.00, output = 60.00),
        "gpt-3.5-turbo" = list(input = 0.50, output = 1.50),
        "claude-sonnet-4-6" = list(input = 3.00, output = 15.00),
        "claude-haiku-4-5" = list(input = 1.00, output = 5.00),
        "gemini-1.5-pro" = list(input = 3.50, output = 10.50),
        "gemini-1.5-flash" = list(input = 0.35, output = 1.05)
    )

    # Default to 0 if model not found (e.g. local models)
    if (is.null(model) || !model %in% names(rates)) {
        return(0)
    }

    rate <- rates[[model]]
    cost <- (prompt_tokens * rate$input / 1000000) + (completion_tokens * rate$output / 1000000)
    return(cost)
}
