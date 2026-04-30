# =============================================================================
# LLM EXTRACTION FUNCTIONS
# Functions for LLM interaction, prompt construction, and data extraction
# =============================================================================

#' Generate Few-Shot Examples from Ground Truth
#'
#' Select a fixed number (or fraction) of ground truth cases as few-shot examples.
#'
#' @param ground_truth_file Path to Excel file with ground truth data
#' @param ground_truth_sheet Sheet number or name in Excel file
#' @param sample_fraction Fraction of cases to use when n_examples is NULL (default 0.1 = 10%)
#' @param seed Random seed for reproducibility (default 42)
#' @param n_examples Exact number of examples to use. Overrides sample_fraction when set.
#'   If 0, returns NULL immediately (no few-shot injection). Nested design is guaranteed
#'   because set.seed() ensures sample() produces the same ordered draw every time, so
#'   the 1-shot example is always the first element of the 3-shot draw, etc.
#' @return Character string with formatted few-shot examples, or NULL when n_examples == 0
generate_few_shot_examples <- function(ground_truth_file = "data/msf_data/raw/narrative_linelist_updated.xlsx",
                                       ground_truth_sheet = 1,
                                       sample_fraction = 0.1,
                                       seed = 42,
                                       n_examples = NULL) {
    # Return NULL immediately for zero-shot configurations
    if (!is.null(n_examples) && n_examples == 0) {
        cat("n_examples = 0: skipping few-shot injection (zero-shot configuration)\n")
        return(NULL)
    }

    cat("Generating few-shot examples from ground truth...\n")

    # Load ground truth
    gt_data <- readxl::read_excel(ground_truth_file, sheet = ground_truth_sheet)
    cat("Loaded", nrow(gt_data), "ground truth cases\n")

    # Standardize column names
    gt_data <- standardize_columns(gt_data)

    # Determine sample size: exact count takes priority over fraction
    set.seed(seed)
    n_samples <- if (!is.null(n_examples)) {
        min(n_examples, nrow(gt_data))
    } else {
        max(1, round(nrow(gt_data) * sample_fraction))
    }
    sample_indices <- sample(1:nrow(gt_data), n_samples)
    sample_cases <- gt_data[sample_indices, ]

    cat("Selected", n_samples, "cases as few-shot examples\n")

    # Convert to normalized format for proper contact/secondary_cases handling
    normalized_sample <- convert_to_normalized_contacts(sample_cases)

    # Build JSON examples
    examples_list <- list()

    for (i in 1:nrow(normalized_sample$main_linelist)) {
        case_row <- normalized_sample$main_linelist[i, ]
        case_id <- case_row$case_id

        # Get contacts and secondary cases directly from columns
        case_contacts <- if (!is.null(case_row$contacts) && !is.na(case_row$contacts) && nzchar(case_row$contacts)) {
            stringr::str_extract_all(toupper(as.character(case_row$contacts)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]
        } else {
            character(0)
        }

        case_secondary <- if (!is.null(case_row$secondary_cases) && !is.na(case_row$secondary_cases) && nzchar(case_row$secondary_cases)) {
            stringr::str_extract_all(toupper(as.character(case_row$secondary_cases)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]
        } else {
            character(0)
        }

        # Build example object (matching expected output format)
        example <- list(
            case_id = case_row$case_id,
            name = ifelse(is.na(case_row$name), "null", tolower(as.character(case_row$name))),
            sex = ifelse(is.na(case_row$sex), "null", tolower(as.character(case_row$sex))),
            age = ifelse(is.na(case_row$age), "null", as.character(case_row$age)),
            age_unit = ifelse(is.na(case_row$age_unit), "null", tolower(as.character(case_row$age_unit))),
            province = ifelse(is.na(case_row$province), "null", tolower(as.character(case_row$province))),
            health_zone = ifelse(is.na(case_row$health_zone), "null", tolower(as.character(case_row$health_zone))),
            health_area = ifelse(is.na(case_row$health_area), "null", tolower(as.character(case_row$health_area))),
            residence_village = ifelse(is.na(case_row$residence_village), "null", tolower(as.character(case_row$residence_village))),
            profession = ifelse(is.na(case_row$profession), "null", tolower(as.character(case_row$profession))),
            onset_date = ifelse(is.na(case_row$onset_date), "null", as.character(case_row$onset_date)),
            outcome_date = ifelse(is.na(case_row$outcome_date), "null", as.character(case_row$outcome_date)),
            outcome = ifelse(is.na(case_row$outcome), "null", tolower(as.character(case_row$outcome))),
            classification = ifelse(is.na(case_row$classification), "null", tolower(as.character(case_row$classification))),
            potential_infector = ifelse(is.na(case_row$potential_infector), "null", as.character(case_row$potential_infector)),
            infection_route = ifelse(is.na(case_row$infection_route), "null", tolower(as.character(case_row$infection_route))),
            most_probable_infector = ifelse(is.na(case_row$most_probable_infector), "null", as.character(case_row$most_probable_infector)),
            contacts = I(if (length(case_contacts) > 0) case_contacts else character(0)),
            secondary_cases = I(if (length(case_secondary) > 0) case_secondary else character(0))
        )

        examples_list[[i]] <- example
    }

    # Convert to pretty JSON (auto_unbox but protect arrays with I())
    examples_json <- jsonlite::toJSON(examples_list, pretty = TRUE, auto_unbox = TRUE)

    # Create formatted few-shot section
    few_shot_section <- glue::glue("

## FEW-SHOT EXAMPLES FOR FORMAT REFERENCE

Below are {n_samples} example cases to demonstrate the **OUTPUT FORMAT AND STRUCTURE ONLY**. These are sample cases from a different part of the dataset used to illustrate formatting conventions.

**IMPORTANT**: These examples show HOW to format the output, NOT which cases to extract. You MUST extract ALL cases from the narrative document below, not just cases similar to these examples.

<examples>
{examples_json}
</examples>

**Key formatting points from the examples above:**
- `contacts`: Array of case_ids this person had contact WITH (people they were exposed to)
- `secondary_cases`: Array of case_ids this person infected (people they exposed)
- Use arrays `[]` even for single items or empty values
- Missing data should be \"null\" as a string
- Names, sex, outcomes should be lowercase
- Dates in DD/MM/YYYY format

**Now extract ALL cases from the outbreak narrative below:**
")

    cat("Few-shot examples generated successfully\n\n")
    return(few_shot_section)
}

#' Process Full Document as Single Unit
#'
#' Process entire document (XML or text) in one LLM call for maximum context
#'
#' @param document_content Character string with full document content
#' @param content_type Character, "xml" or "text" to specify document type
#' @param max_tokens Maximum tokens for LLM response
#' @param few_shot_examples Optional character string with few-shot examples to inject
#' @return Tibble with extracted linelist data
process_full_document_xml <- function(document_content, content_type = "xml", max_tokens = 20000,
                                      provider = "azure", model = NULL, temperature = 0.1,
                                      few_shot_examples = NULL,
                                      system_prompt_path = NULL,
                                      kv_bits = NULL,
                                      kv_quant_scheme = NULL) {
    cat("Processing full", content_type, "document (", nchar(document_content), " characters) as single unit...\n")

    # Load system prompt from specified path or fall back to default
    if (is.null(system_prompt_path)) {
        system_prompt_path <- here::here("data", "system_prompts", "system_prompt_ebola_narrative.txt")
    }
    system_prompt <- readr::read_file(system_prompt_path)
    cat("Using system prompt:", basename(system_prompt_path), "\n")

    # Inject few-shot examples if provided (before the {content} placeholder)
    if (!is.null(few_shot_examples) && nchar(few_shot_examples) > 0) {
        cat("Injecting few-shot examples into prompt...\n")
        # Find the position to insert (right before the outbreak_narrative section)
        insert_position <- stringr::str_locate(system_prompt, "<outbreak_narrative>")[1, "start"] - 1
        if (is.na(insert_position)) {
            # Fallback to old tag name for backwards compatibility
            insert_position <- stringr::str_locate(system_prompt, "<outbreak_report>")[1, "start"] - 1
        }
        system_prompt <- paste0(
            stringr::str_sub(system_prompt, 1, insert_position),
            few_shot_examples,
            "\n",
            stringr::str_sub(system_prompt, insert_position + 1, nchar(system_prompt))
        )
    }

    # Replace placeholder with document content
    # Use {{content}} for new format, {content} for backwards compatibility
    full_doc_prompt <- stringr::str_replace(system_prompt, "\\{\\{content\\}\\}", document_content)
    full_doc_prompt <- stringr::str_replace(full_doc_prompt, "\\{content\\}", document_content)

    # Call LLM with configured provider and model
    # Use raw_prompt=TRUE since system_prompt_ebola_narrative.txt already contains complete instructions
    cat("Sending full", content_type, "document to", provider, "with", max_tokens, "token limit...\n")
    llm_response <- llm_call_tidy(
        full_doc_prompt,
        provider = provider,
        model = model,
        max_tokens = max_tokens,
        temperature = temperature,
        raw_prompt = TRUE, # Prompt already contains system instructions
        kv_bits = kv_bits,
        kv_quant_scheme = kv_quant_scheme
    )

    # Extract structured data from response
    structured_result <- extract_structured_data(llm_response)

    if (is.null(structured_result$linelist) || nrow(structured_result$linelist) == 0) {
        warning("No structured data extracted from full document.")
        return(list(linelist = tibble::tibble(), contacts = tibble::tibble(), raw_response = llm_response))
    } else {
        cat("Full", content_type, "document processing successful:", nrow(structured_result$linelist), "rows extracted\n")

        # Return linelist (contacts are now embedded)
        return(list(
            linelist = structured_result$linelist,
            contacts = tibble::tibble(), # Deprecated
            raw_response = llm_response
        ))
    }
}

#' Generate LLM Results with Multiple Extraction Methods
#'
#' Process document using either XML structured extraction or plain text extraction
#' with optional chunking and compression for optimal token efficiency
#'
#' @param xml_path Path to XML file (if NULL, extracts from docx_path)
#' @param docx_path Path to DOCX file (used if xml_path is NULL or extraction_method is "text")
#' @param extraction_method Character, "xml" for structured XML or "text" for plain text extraction
#' @param use_compressed_xml Logical, whether to use compressed XML for better token efficiency
#' @param use_chunking Logical, whether to process in chunks (TRUE) or as full document (FALSE)
#' @param max_tokens Integer, maximum tokens for Azure OpenAI (increase for full document processing)
#' @param few_shot_examples Optional character string with few-shot examples
#' @return List with linelist and metadata
generate_llm_results <- function(xml_path = NULL,
                                 docx_path = NULL,
                                 extraction_method = "xml",
                                 use_compressed_xml = TRUE,
                                 use_chunking = TRUE,
                                 max_tokens = 20000,
                                 provider = "azure",
                                 model = NULL,
                                 temperature = 0.1,
                                 few_shot_examples = NULL,
                                 case_limit = NULL,
                                 system_prompt_path = NULL,
                                 log_dir = NULL,
                                 deduplicate = TRUE,
                                 kv_bits = NULL,
                                 kv_quant_scheme = NULL) {
    processing_method <- ifelse(use_chunking, "case-based chunking", "full document processing")
    extraction_type <- if (extraction_method == "xml") {
        ifelse(use_compressed_xml, "compressed XML", "standard XML")
    } else {
        "plain text"
    }

    cat("Generating LLM results using", processing_method, "with", extraction_type, "extraction\n")
    if (!is.null(case_limit)) {
        cat("Limit set to:", case_limit, "cases\n")
    }

    # Step 1: Get XML content (either from provided XML file or extract from DOCX)
    if (!is.null(xml_path) && file.exists(xml_path)) {
        cat("Loading XML from:", xml_path, "\n")
        document_content <- readr::read_file(xml_path)

        # If using compressed XML, decompress it first for processing
        if (use_compressed_xml && stringr::str_detect(xml_path, "compressed")) {
            cat("Decompressing XML for processing...\n")
            source("scripts/core/utils.R") # Load compression functions
            document_content <- decompress_xml(document_content, input_type = "string")
        }
    } else {
        # Fallback to DOCX extraction
        if (is.null(docx_path) || !file.exists(docx_path)) {
            stop("No valid document path provided. Supply either xml_path or docx_path.")
        }

        if (extraction_method == "xml") {
            cat("Extracting structured XML from DOCX:", docx_path, "\n")
            document_content <- extract_docx_structured_xml(docx_path)

            # Save extracted XML for review
            xml_filename <- stringr::str_replace(basename(docx_path), "\\.docx$", "_structured.xml")
            xml_save_path <- here::here("data", "msf_data", "modified", xml_filename)

            # Ensure directory exists
            if (!dir.exists(dirname(xml_save_path))) {
                dir.create(dirname(xml_save_path), recursive = TRUE)
            }

            readr::write_file(document_content, xml_save_path)
            cat("Structured XML saved to:", xml_save_path, "\n")
        } else if (extraction_method == "markdown") {
            # Define expected markdown path in modified directory
            md_filename <- stringr::str_replace(basename(docx_path), "\\.docx$", "_extracted.md")
            md_save_path <- here::here("data", "msf_data", "modified", md_filename)

            # Ensure directory exists
            if (!dir.exists(dirname(md_save_path))) {
                dir.create(dirname(md_save_path), recursive = TRUE)
            }

            if (file.exists(md_save_path)) {
                cat("Loading existing Markdown from:", md_save_path, "\n")
                document_content <- readr::read_file(md_save_path)
            } else {
                cat("Generating structured Markdown from DOCX...\n")

                # We need the XML first to do the "good" conversion
                # Check if XML file exists
                xml_filename <- stringr::str_replace(basename(docx_path), "\\.docx$", "_structured.xml")
                xml_save_path <- here::here("data", "msf_data", "modified", xml_filename)

                if (file.exists(xml_save_path)) {
                    cat("Loading intermediate XML from:", xml_save_path, "\n")
                    xml_content <- readr::read_file(xml_save_path)
                } else {
                    cat("Extracting structured XML from DOCX:", docx_path, "\n")
                    xml_content <- extract_docx_structured_xml(docx_path)
                    readr::write_file(xml_content, xml_save_path)
                    cat("Structured XML saved to:", xml_save_path, "\n")
                }

                # Convert XML to Markdown using the improved function
                document_content <- convert_structured_xml_to_markdown(xml_content)
                readr::write_file(document_content, md_save_path)
                cat("Markdown saved to:", md_save_path, "\n")
            }
        } else {
            cat("Extracting plain text from DOCX:", docx_path, "\n")
            document_content <- extract_docx_text(docx_path)

            # Save extracted text for review
            text_save_path <- stringr::str_replace(docx_path, "\\.docx$", "_extracted.txt")
            readr::write_file(document_content, text_save_path)
            cat("Plain text saved to:", text_save_path, "\n")
        }

        # Optionally compress the XML for future use
        if (use_compressed_xml) {
            # Only attempt compression if an XML save path was actually created
            if (exists("xml_save_path") && !is.null(xml_save_path) && stringr::str_detect(xml_save_path, "\\.xml$")) {
                tryCatch(
                    {
                        source("scripts/core/utils.R") # Load compression functions
                        compressed_path <- stringr::str_replace(xml_save_path, "\\.xml$", "_compressed.xml")
                        compress_xml(document_content, input_type = "string", output_file = compressed_path)
                    },
                    error = function(e) {
                        cat("Warning: Could not compress XML:", e$message, "\n")
                    }
                )
            } else {
                cat("Skipping XML compression: no XML save path available\n")
            }
        }
    }

    cat("Document size:", nchar(document_content), "characters\n")

    # Step 2: Process XML content based on chunking preference
    start_time <- Sys.time()

    if (use_chunking) {
        # Traditional chunking approach
        cat("Splitting document into individual case chunks...\n")
        case_chunks <- split_cases_from_content(document_content)

        if (length(case_chunks) == 0) {
            stop("No cases found in document")
        }

        # Apply case limit if set
        if (!is.null(case_limit) && case_limit > 0) {
            if (case_limit < length(case_chunks)) {
                cat("Limiting execution to first", case_limit, "cases (out of", length(case_chunks), ")\n")
                case_chunks <- case_chunks[1:case_limit]
            }
        }

        # Process each case individually with configured provider and model
        cat("Processing", length(case_chunks), "cases individually with", provider, "...\n")

        chunk_results <- purrr::map(case_chunks, function(chunk) {
            process_case_chunk(chunk, provider = provider, model = model, max_tokens = max_tokens)
        })

        # Separate linelist and contact results
        all_results <- purrr::map_dfr(chunk_results, ~ .x$linelist)
        all_contacts <- purrr::map_dfr(chunk_results, ~ .x$contacts)

        # Combine raw responses
        all_raw_responses <- purrr::map_chr(chunk_results, ~ .x$raw_response)
        combined_raw_response <- paste(all_raw_responses, collapse = "\n\n--- NEXT CHUNK ---\n\n")

        if (nrow(all_contacts) > 0) {
            cat("Total contact relationships extracted:", nrow(all_contacts), "\n")
        } else {
            cat("No contact relationships extracted from chunks\n")
        }
    } else {
        # Full document processing - single LLM call with configured provider
        cat("Processing full document as single unit with", provider, "and", max_tokens, "token limit...\n")
        full_doc_result <- process_full_document_xml(document_content,
            content_type = extraction_method,
            max_tokens = max_tokens, provider = provider,
            model = model, temperature = temperature,
            few_shot_examples = few_shot_examples,
            system_prompt_path = system_prompt_path,
            kv_bits = kv_bits,
            kv_quant_scheme = kv_quant_scheme
        )

        # Extract linelist from the result
        all_results <- full_doc_result$linelist
        all_contacts <- tibble::tibble() # Deprecated
        combined_raw_response <- full_doc_result$raw_response

        # Extract SLURM timing if available (from cluster provider)
        slurm_execution_time <- attr(full_doc_result$raw_response, "slurm_execution_time")
        slurm_job_id <- attr(full_doc_result$raw_response, "slurm_job_id")
        # Extract MLX timing if available (from mlx provider)
        mlx_timing <- attr(full_doc_result$raw_response, "mlx_timing")
    }

    # Save raw output
    timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
    # Clean model name for filename
    safe_model_name <- stringr::str_replace_all(ifelse(is.null(model), "default", model), "[^a-zA-Z0-9]", "_")
    effective_log_dir <- if (!is.null(log_dir)) log_dir else here::here("outputs")
    dir.create(effective_log_dir, recursive = TRUE, showWarnings = FALSE)
    raw_output_filename <- file.path(effective_log_dir, glue::glue("{provider}_{safe_model_name}_raw_output_{timestamp_str}.txt"))
    readr::write_file(combined_raw_response, raw_output_filename)
    cat("Raw output saved to:", raw_output_filename, "\n")

    end_time <- Sys.time()
    processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

    cat("LLM analysis completed in", round(processing_time, 2), "seconds\n")

    # For cluster jobs, prefer SLURM execution time (actual GPU time)
    if (exists("slurm_execution_time") && !is.null(slurm_execution_time) && !is.na(slurm_execution_time)) {
        cat(
            "SLURM execution time:", round(slurm_execution_time, 2), "seconds (",
            round(slurm_execution_time / 60, 1), "minutes)\n"
        )
        # Use SLURM time as the authoritative processing time for cluster jobs
        processing_time <- slurm_execution_time
    }
    cat("Total extracted linelist:", nrow(all_results), "rows\n")

    # Step 4: Deduplicate results
    if (!deduplicate) {
        cat("Deduplication skipped (deduplicate = FALSE).\n")
        deduplicated_results <- all_results
    } else if (use_chunking) {
        cat("Skipping deduplication for chunked strategy (enforced 1-to-1 mapping)\n")
        deduplicated_results <- all_results
    } else {
        cat("Standardizing columns before deduplication...\n")
        all_results <- standardize_columns(all_results)
        cat("Deduplicating results with", provider, "...\n")
        deduplicated_results <- consolidate_duplicates_llm(all_results, provider = provider, model = model)
        cat("After deduplication:", nrow(deduplicated_results), "rows\n")
    }

    # Save deduplicated results as CSV
    readr::write_csv(deduplicated_results, here::here("outputs", "msf_llm_linelist_latest.csv"))
    cat("Saved final results to: outputs/msf_llm_linelist_latest.csv\n")

    # Return results with metadata
    result <- list(
        linelist = deduplicated_results,
        raw_response = combined_raw_response, # Return actual LLM response for validation
        processing_time = processing_time,
        timestamp = Sys.time(),
        chunks_processed = ifelse(use_chunking, length(case_chunks), 1),
        pre_dedup_count = nrow(all_results),
        post_dedup_count = nrow(deduplicated_results)
    )

    # Add SLURM metadata if available (cluster jobs)
    if (exists("slurm_job_id") && !is.null(slurm_job_id) && !is.na(slurm_job_id)) {
        result$slurm_job_id <- slurm_job_id
        result$slurm_execution_time <- slurm_execution_time
    }

    # Add MLX timing if available (local mlx jobs)
    if (exists("mlx_timing") && !is.null(mlx_timing)) {
        result$mlx_timing <- mlx_timing
    }

    return(result)
}

#' Extract Few-Shot Case IDs from Generated Examples
#'
#' Parse the JSON block in few-shot examples to extract case IDs for exclusion
#'
#' @param few_shot_examples Character string containing the few-shot examples section
#' @return Character vector of case IDs used in few-shot examples
#' @export
extract_few_shot_case_ids <- function(few_shot_examples) {
    if (is.null(few_shot_examples) || !nzchar(few_shot_examples)) {
        return(character(0))
    }

    examples_json <- stringr::str_match(few_shot_examples, "(?s)<examples>(.*?)</examples>")[, 2]

    if (is.na(examples_json) || !nzchar(trimws(examples_json))) {
        return(character(0))
    }

    tryCatch(
        {
            examples_list <- jsonlite::fromJSON(examples_json)
            if (!is.null(examples_list$case_id)) {
                return(as.character(examples_list$case_id))
            }
            return(character(0))
        },
        error = function(e) {
            warning(glue::glue("Failed to parse few-shot examples JSON: {e$message}"))
            return(character(0))
        }
    )
}

#' Build Cluster Prompt
#'
#' Construct the full prompt for a cluster job
#'
#' @param docx_path Path to source DOCX file
#' @param system_prompt_path Path to system prompt file
#' @param few_shot_examples Character string with few-shot examples (optional)
#' @return Character string with complete prompt
#' @export
build_cluster_prompt <- function(docx_path, system_prompt_path, few_shot_examples = NULL) {
    # Derive expected markdown cache path from the docx filename
    md_filename <- stringr::str_replace(basename(docx_path), "\\.docx$", "_extracted.md")
    md_path <- here::here("data/msf_data/modified", md_filename)
    if (file.exists(md_path)) {
        markdown_content <- readr::read_file(md_path)
    } else {
        markdown_content <- extract_docx_markdown(docx_path)
    }

    # Load system prompt
    system_prompt <- readr::read_file(system_prompt_path)

    # Add few-shot examples if provided
    if (!is.null(few_shot_examples) && nzchar(few_shot_examples)) {
        system_prompt <- paste0(system_prompt, "\n\n", few_shot_examples)
    }

    glue::glue("{system_prompt}\n\n---\n\n# DOCUMENT TO ANALYZE:\n\n{markdown_content}")
}
