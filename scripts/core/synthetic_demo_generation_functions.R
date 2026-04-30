# =============================================================================
# SYNTHETIC DEMO GENERATION FUNCTIONS
# Helpers for generating a synthetic, schema-compatible demo linelist and
# matching narrative via Azure GPT-5.
# =============================================================================

library(tidyverse)
library(readxl)
library(readr)
library(writexl)
library(qs)
library(glue)
library(here)
library(jsonlite)

utils::globalVariables(".data")

call_loaded_function <- function(name, ...) {
    get(name, mode = "function")(...)
}

demo_case_id_regex <- function() {
    "\\b[A-Z]{3}[0-9]{3}\\b"
}

default_synthetic_gazetteer_path <- function() {
    here::here("data", "msf_data", "modified", "synthetic_demo_gazetteer.json")
}

load_synthetic_demo_gazetteer <- function(gazetteer_path = default_synthetic_gazetteer_path()) {
    if (!file.exists(gazetteer_path)) {
        stop(glue("Synthetic gazetteer not found: {gazetteer_path}"))
    }

    raw_gazetteer <- jsonlite::read_json(gazetteer_path, simplifyVector = FALSE)

    if (!is.list(raw_gazetteer) || length(raw_gazetteer) == 0) {
        stop(glue("Synthetic gazetteer is empty or malformed: {gazetteer_path}"))
    }

    required_fields <- c("province", "health_zone", "health_area", "villages")
    missing_fields <- purrr::map(raw_gazetteer, function(entry) setdiff(required_fields, names(entry)))

    if (any(lengths(missing_fields) > 0)) {
        stop(glue("Synthetic gazetteer entries are missing required fields in {gazetteer_path}"))
    }

    combinations <- purrr::map_dfr(raw_gazetteer, function(entry) {
        villages <- normalize_missing_strings(unlist(entry$villages, use.names = FALSE))
        villages <- villages[!is.na(villages)]

        if (length(villages) == 0) {
            stop(glue("Synthetic gazetteer entry has no villages: {entry$province} / {entry$health_zone} / {entry$health_area}"))
        }

        tibble::tibble(
            province = normalize_missing_strings(entry$province),
            health_zone = normalize_missing_strings(entry$health_zone),
            health_area = normalize_missing_strings(entry$health_area),
            residence_village = villages
        )
    }) |>
        dplyr::distinct()

    combinations <- combinations[
        order(combinations$province, combinations$health_zone, combinations$health_area, combinations$residence_village),
        ,
        drop = FALSE
    ]

    prompt_table <- combinations[c("province", "health_zone", "health_area")] |>
        unique()

    prompt_table$villages <- purrr::pmap(
        prompt_table,
        function(province, health_zone, health_area) {
            combinations$residence_village[
                combinations$province == province &
                    combinations$health_zone == health_zone &
                    combinations$health_area == health_area
            ] |>
                unique() |>
                sort()
        }
    )

    list(
        path = gazetteer_path,
        entries = raw_gazetteer,
        combinations = combinations,
        prompt_table = prompt_table
    )
}

validate_synthetic_demo_geography <- function(linelist, gazetteer) {
    geography_fields <- c("province", "health_zone", "health_area", "residence_village")
    allowed_combinations <- gazetteer$combinations
    errors <- character(0)

    for (field in geography_fields) {
        provided_values <- unique(stats::na.omit(normalize_missing_strings(linelist[[field]])))
        bad_values <- setdiff(provided_values, unique(allowed_combinations[[field]]))

        if (length(bad_values) > 0) {
            errors <- c(
                errors,
                glue("Field {field} includes values outside the synthetic gazetteer: {paste(head(bad_values, 10), collapse = ', ')}")
            )
        }
    }

    row_errors <- purrr::map_chr(seq_len(nrow(linelist)), function(index) {
        row <- linelist[index, geography_fields]
        candidates <- allowed_combinations

        for (field in geography_fields) {
            value <- normalize_missing_strings(row[[field]])
            if (!is.na(value)) {
                candidates <- candidates[candidates[[field]] == value, , drop = FALSE]
            }
        }

        if (nrow(candidates) == 0) {
            row_values <- purrr::imap_chr(row, function(value, field) {
                cleaned <- normalize_missing_strings(value)
                glue("{field}={ifelse(is.na(cleaned), 'NA', cleaned)}")
            })
            return(glue("Case {linelist$case_id[index]} has a geography combination not permitted by the synthetic gazetteer: {paste(row_values, collapse = ', ')}"))
        }

        NA_character_
    })

    c(errors, stats::na.omit(row_errors)) |>
        unique()
}

#' Expected Demo Linelist Columns
#'
#' @return Character vector of expected columns in output order
expected_demo_columns <- function() {
    c(
        "case_id",
        "name",
        "sex",
        "age",
        "age_unit",
        "province",
        "health_zone",
        "health_area",
        "residence_village",
        "profession",
        "onset_date",
        "outcome_date",
        "outcome",
        "classification",
        "potential_infector",
        "infection_route",
        "most_probable_infector",
        "contacts",
        "secondary_cases"
    )
}

#' Assert Required Functions Are Loaded
#'
#' @return Invisible TRUE when all dependencies are available
assert_synthetic_demo_dependencies <- function() {
    required_functions <- c(
        "llm_call_tidy",
        "extract_structured_data",
        "standardize_columns",
        "convert_to_normalized_contacts",
        "validate_contact_relationships",
        "extract_document_sample"
    )

    missing_functions <- required_functions[!vapply(required_functions, exists, logical(1), mode = "function")]

    if (length(missing_functions) > 0) {
        stop(glue("Missing required functions: {paste(missing_functions, collapse = ', ')}"))
    }

    invisible(TRUE)
}

#' Parse Case IDs from Free Text
#'
#' @param text Character vector
#' @return Character vector of unique case IDs
parse_demo_case_ids <- function(text) {
    if (length(text) == 0 || all(is.na(text))) {
        return(character(0))
    }

    ids <- stringr::str_extract_all(toupper(paste(text, collapse = ", ")), demo_case_id_regex())[[1]]
    unique(ids[!is.na(ids) & nzchar(ids)])
}

#' Normalize Missing-Like Strings
#'
#' @param x Vector to normalize
#' @return Character vector with blanks converted to NA
normalize_missing_strings <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[tolower(x) %in% c("", "na", "n/a", "null", "none", "unknown")] <- NA_character_
    x
}

#' Coerce a Data Frame to the Demo Linelist Schema
#'
#' @param df Data frame
#' @return Tibble with expected columns and normalized values
coerce_demo_linelist_schema <- function(df) {
    df <- tibble::as_tibble(df)
    df <- call_loaded_function("standardize_columns", df)

    expected_cols <- expected_demo_columns()
    missing_cols <- setdiff(expected_cols, names(df))

    for (col in missing_cols) {
        df[[col]] <- NA_character_
    }

    df <- df |> dplyr::select(dplyr::all_of(expected_cols), dplyr::everything())

    character_cols <- intersect(expected_cols, names(df))
    for (col in character_cols) {
        df[[col]] <- normalize_missing_strings(df[[col]])
    }

    if ("case_id" %in% names(df)) {
        df$case_id <- toupper(df$case_id)
    }

    id_list_cols <- c("contacts", "secondary_cases", "potential_infector", "most_probable_infector")
    for (col in intersect(id_list_cols, names(df))) {
        df[[col]] <- vapply(df[[col]], function(value) {
            ids <- parse_demo_case_ids(value)
            if (length(ids) == 0) {
                return(NA_character_)
            }
            paste(ids, collapse = ", ")
        }, character(1))
    }

    lowercase_cols <- c(
        "name", "sex", "age_unit", "province", "health_zone", "health_area",
        "residence_village", "profession", "outcome", "classification", "infection_route"
    )

    for (col in intersect(lowercase_cols, names(df))) {
        df[[col]] <- ifelse(is.na(df[[col]]), NA_character_, tolower(df[[col]]))
    }

    if ("age" %in% names(df)) {
        df$age <- ifelse(is.na(df$age), NA_character_, as.character(df$age))
    }

    df |> dplyr::select(dplyr::all_of(expected_cols))
}

#' Read and Standardize Source Ground Truth
#'
#' @param ground_truth_path Path to source workbook
#' @return Tibble with standardized schema
read_demo_source_ground_truth <- function(ground_truth_path) {
    gt <- readxl::read_excel(ground_truth_path, sheet = 1)
    coerce_demo_linelist_schema(gt)
}

#' Summarize Ground Truth for Synthetic Generation
#'
#' @param ground_truth Standardized source ground truth
#' @return List with compact structural summary for prompt construction
summarize_demo_source_ground_truth <- function(ground_truth) {
    contact_counts <- vapply(ground_truth$contacts, function(value) length(parse_demo_case_ids(value)), integer(1))
    secondary_counts <- vapply(ground_truth$secondary_cases, function(value) length(parse_demo_case_ids(value)), integer(1))

    count_non_missing_values <- function(x, field_name) {
        values <- normalize_missing_strings(x)
        values <- values[!is.na(values)]

        if (length(values) == 0) {
            out <- tibble::tibble(value = character(), n = integer())
            names(out)[1] <- field_name
            return(out)
        }

        counts <- sort(table(values), decreasing = TRUE)
        out <- tibble::tibble(value = names(counts), n = as.integer(counts))
        names(out)[1] <- field_name
        out
    }

    age_numeric <- suppressWarnings(as.numeric(ground_truth$age))

    banned_values <- list(
        names = unique(stats::na.omit(ground_truth$name)),
        provinces = unique(stats::na.omit(ground_truth$province)),
        health_zones = unique(stats::na.omit(ground_truth$health_zone)),
        health_areas = unique(stats::na.omit(ground_truth$health_area)),
        villages = unique(stats::na.omit(ground_truth$residence_village))
    )

    list(
        n_cases = nrow(ground_truth),
        column_names = names(ground_truth),
        sex_distribution = count_non_missing_values(ground_truth$sex, "sex"),
        classification_distribution = count_non_missing_values(ground_truth$classification, "classification"),
        outcome_distribution = count_non_missing_values(ground_truth$outcome, "outcome"),
        age_summary = tibble::tibble(
            min_age = min(age_numeric, na.rm = TRUE),
            median_age = median(age_numeric, na.rm = TRUE),
            max_age = max(age_numeric, na.rm = TRUE),
            n_missing_age = sum(is.na(age_numeric))
        ),
        missingness = tibble::tibble(
            field = names(ground_truth),
            missing_fraction = vapply(ground_truth, function(col) mean(is.na(normalize_missing_strings(col))), numeric(1))
        ) |>
            (
                function(df) df[order(df$missing_fraction, decreasing = TRUE), , drop = FALSE]
            )(),
        contact_summary = tibble::tibble(
            mean_contacts = mean(contact_counts),
            max_contacts = max(contact_counts),
            mean_secondary_cases = mean(secondary_counts),
            max_secondary_cases = max(secondary_counts)
        ),
        banned_values = banned_values
    )
}

#' Build a Synthetic Linelist Prompt
#'
#' @param source_summary Summary list from summarize_demo_source_ground_truth()
#' @param narrative_sample Document sample for style reference
#' @param target_case_count Number of synthetic cases to generate
#' @param case_id_prefix Prefix for generated case IDs
#' @return Character prompt for llm_call_tidy()
build_synthetic_linelist_prompt <- function(source_summary,
                                            narrative_sample,
                                            target_case_count = 84,
                                            case_id_prefix = "SYN",
                                            gazetteer) {
    expected_case_ids <- sprintf("%s%03d", case_id_prefix, seq_len(target_case_count))
    min_unique_villages <- max(3, min(12, floor(target_case_count / 3)))
    min_unique_routes <- max(3, min(8, floor(target_case_count / 4)))
    min_named_professions <- max(3, min(10, floor(target_case_count / 5)))
    min_multi_case_clusters <- if (target_case_count >= 12) 2 else 1
    min_uncertain_cases <- max(2, ceiling(target_case_count * 0.18))
    min_non_confirmed_cases <- max(2, ceiling(target_case_count * 0.15))

    summary_json <- jsonlite::toJSON(
        list(
            source_case_count = source_summary$n_cases,
            sex_distribution = source_summary$sex_distribution,
            classification_distribution = source_summary$classification_distribution,
            outcome_distribution = source_summary$outcome_distribution,
            age_summary = source_summary$age_summary,
            missingness = source_summary$missingness,
            contact_summary = source_summary$contact_summary
        ),
        pretty = TRUE,
        auto_unbox = TRUE,
        null = "null"
    )

    banned_values_json <- jsonlite::toJSON(source_summary$banned_values, pretty = TRUE, auto_unbox = TRUE)
    gazetteer_json <- jsonlite::toJSON(gazetteer$prompt_table, pretty = TRUE, auto_unbox = TRUE, null = "null")

    glue::glue_data(
        .x = list(
            target_case_count = target_case_count,
            expected_case_ids = paste(expected_case_ids, collapse = ", "),
            min_unique_villages = min_unique_villages,
            min_unique_routes = min_unique_routes,
            min_named_professions = min_named_professions,
            min_multi_case_clusters = min_multi_case_clusters,
            min_uncertain_cases = min_uncertain_cases,
            min_non_confirmed_cases = min_non_confirmed_cases,
            summary_json = summary_json,
            banned_values_json = banned_values_json,
            gazetteer_json = gazetteer_json,
            narrative_sample = narrative_sample
        ),
        .open = "<<",
        .close = ">>",
        "You are creating a fully synthetic outbreak linelist for a demo environment.\n\n",
        "Task: Generate exactly <<target_case_count>> fake cases that preserve the schema and broad epidemiological structure of the source material, while inventing all personal and place identifiers.\n\n",
        "Constraints:\n",
        "- Output ONLY a JSON array wrapped in <linelist> tags.\n",
        "- Generate exactly these case IDs and no others: <<expected_case_ids>>.\n",
        "- Never reuse any banned names or locations from the source material.\n",
        "- Use geography only from the approved synthetic gazetteer below. Do not invent any additional provinces, health zones, health areas, villages, aliases, abbreviations, or spelling variants.\n",
        "- Every non-missing province, health_zone, health_area, and residence_village value must exactly match the gazetteer and remain internally consistent as one approved hierarchy.\n",
        "- Keep all names, professions, places, outcomes, classifications, and infection_route values in lowercase.\n",
        "- Use DD/MM/YYYY dates.\n",
        "- Contacts, secondary_cases, and potential_infector must reference only IDs from the generated set.\n",
        "- secondary_cases must point to downstream cases; most_probable_infector and potential_infector must point to upstream cases.\n",
        "- contacts should be the union of epidemiological links for that case.\n",
        "- Use null for missing scalar values and [] for empty relationship arrays.\n",
        "- Preserve realistic sparsity: some fields should remain missing when plausible.\n\n",
        "Diversity requirements:\n",
        "- Do not collapse the outbreak into a single uniform chain or one dominant superspreader.\n",
        "- Create at least <<min_multi_case_clusters>> distinct transmission clusters when the case count permits, with a mix of household, funeral, healthcare, caregiving, and community exposures.\n",
        "- Use at least <<min_unique_villages>> distinct residence_village values and at least <<min_unique_routes>> distinct infection_route values.\n",
        "- Provide named professions for at least <<min_named_professions>> cases and vary them across healthcare, domestic, market, transport, education, agriculture, and informal work where plausible.\n",
        "- Vary ages, sexes, outcomes, and case roles; include children, working-age adults, and older adults when plausible.\n",
        "- Populate geography more richly than the minimum schema when possible: use varied health_zone, health_area, and province values instead of leaving most rows blank.\n\n",
        "Uncertainty requirements:\n",
        "- A realistic minority of cases must remain uncertain or partially unresolved. Include at least <<min_uncertain_cases>> cases with uncertainty in one or more of: source attribution, route, classification, or outcome timing.\n",
        "- Include at least <<min_non_confirmed_cases>> non-confirmed cases across probable and suspected classifications.\n",
        "- Leave most_probable_infector blank for some cases where the source is unresolved, and allow multiple IDs in potential_infector for a minority when several exposures are plausible.\n",
        "- Some narratives should include phrases corresponding to uncertainty such as possible, probable, suspected, reported exposure, not clearly established, or source uncertain.\n",
        "- Do not make every transmission route or case classification definitive.\n\n",
        "Schema (all fields required in each object):\n",
        paste(sprintf("- %s", expected_demo_columns()), collapse = "\n"),
        "\n\n",
        "Structural summary of the source dataset:\n<source_summary>\n<<summary_json>>\n</source_summary>\n\n",
        "Banned exact identifiers from the source dataset:\n<banned_values>\n<<banned_values_json>>\n</banned_values>\n\n",
        "Approved synthetic gazetteer. This is the only allowed geography universe:\n<synthetic_gazetteer>\n<<gazetteer_json>>\n</synthetic_gazetteer>\n\n",
        "Narrative style reference. Use this only for tone and outbreak-writing style, not for names or places:\n<style_reference>\n<<narrative_sample>>\n</style_reference>\n\n",
        "Output format example:\n",
        "<linelist>\n",
        "[\n",
        "  {\n",
        "    \"case_id\": \"SYN001\",\n",
        "    \"name\": \"amadou kelu\",\n",
        "    \"sex\": \"male\",\n",
        "    \"age\": \"28\",\n",
        "    \"age_unit\": \"years\",\n",
        "    \"province\": \"nord-ouest\",\n",
        "    \"health_zone\": \"mokala\",\n",
        "    \"health_area\": \"bikenga-centre\",\n",
        "    \"residence_village\": \"kalima\",\n",
        "    \"profession\": \"cultivator\",\n",
        "    \"onset_date\": \"04/03/2026\",\n",
        "    \"outcome_date\": null,\n",
        "    \"outcome\": \"under treatment\",\n",
        "    \"classification\": \"probable\",\n",
        "    \"potential_infector\": [\"SYN002\", \"SYN004\"],\n",
        "    \"infection_route\": \"reported household contact\",\n",
        "    \"most_probable_infector\": null,\n",
        "    \"contacts\": [\"SYN002\", \"SYN004\"],\n",
        "    \"secondary_cases\": []\n",
        "  }\n",
        "]\n",
        "</linelist>"
    )
}

#' Build a Repair Prompt for Invalid Synthetic Linelist Output
#'
#' @param parsed_linelist Current parsed linelist
#' @param validation_errors Character vector of errors
#' @param target_case_count Required number of cases
#' @param gazetteer Loaded synthetic gazetteer
#' @return Character repair prompt
build_synthetic_repair_prompt <- function(parsed_linelist, validation_errors, target_case_count, gazetteer) {
    linelist_json <- jsonlite::toJSON(parsed_linelist, pretty = TRUE, auto_unbox = TRUE, null = "null")
    gazetteer_json <- jsonlite::toJSON(gazetteer$prompt_table, pretty = TRUE, auto_unbox = TRUE, null = "null")

    glue::glue_data(
        .x = list(
            target_case_count = target_case_count,
            validation_errors = paste(sprintf("- %s", validation_errors), collapse = "\n"),
            linelist_json = linelist_json,
            gazetteer_json = gazetteer_json
        ),
        .open = "<<",
        .close = ">>",
        "Repair the synthetic outbreak linelist below.\n\n",
        "Requirements:\n",
        "- Return ONLY a corrected JSON array wrapped in <linelist> tags.\n",
        "- Keep exactly <<target_case_count>> rows.\n",
        "- Preserve the same schema and case IDs unless a correction is needed for validity.\n",
        "- Ensure all relationship references point to existing case IDs.\n",
        "- Ensure every geography field uses only exact values from the approved synthetic gazetteer below and that province, health_zone, health_area, and residence_village stay hierarchy-consistent.\n",
        "- Keep all text fields lowercase where appropriate.\n\n",
        "Validation errors to fix:\n<<validation_errors>>\n\n",
        "Approved synthetic gazetteer:\n<synthetic_gazetteer>\n<<gazetteer_json>>\n</synthetic_gazetteer>",
        "\n\nCurrent linelist:\n<linelist>\n<<linelist_json>>\n</linelist>"
    )
}

#' Parse Flexible Day-Month-Year Dates
#'
#' @param x Character vector of dates
#' @return Date vector
parse_demo_dates <- function(x) {
    x <- normalize_missing_strings(x)
    parsed <- as.Date(x, format = "%d/%m/%Y")

    unresolved <- is.na(parsed) & !is.na(x)
    if (any(unresolved)) {
        parsed[unresolved] <- as.Date(x[unresolved], format = "%Y-%m-%d")
    }

    parsed
}

#' Assess Diversity of a Synthetic Demo Linelist
#'
#' @param linelist Candidate synthetic linelist
#' @param target_case_count Expected number of rows
#' @return List with diversity errors and warnings
assess_synthetic_variation <- function(linelist, target_case_count) {
    clean_unique <- function(x) {
        values <- unique(stats::na.omit(normalize_missing_strings(x)))
        values[nzchar(values)]
    }

    secondary_counts <- vapply(linelist$secondary_cases, function(value) length(parse_demo_case_ids(value)), integer(1))
    primary_roots <- sum(
        is.na(normalize_missing_strings(linelist$most_probable_infector)) &
            is.na(normalize_missing_strings(linelist$potential_infector))
    )

    unique_villages <- clean_unique(linelist$residence_village)
    unique_routes <- clean_unique(linelist$infection_route)
    named_professions <- clean_unique(linelist$profession)
    unique_outcomes <- clean_unique(linelist$outcome)
    non_missing_health_zones <- sum(!is.na(normalize_missing_strings(linelist$health_zone)))
    non_missing_provinces <- sum(!is.na(normalize_missing_strings(linelist$province)))

    min_unique_villages <- max(3, min(12, floor(target_case_count / 3)))
    min_unique_routes <- max(3, min(8, floor(target_case_count / 4)))
    min_named_professions <- max(3, min(10, floor(target_case_count / 5)))
    max_secondary_from_one_case <- max(2, ceiling(target_case_count * 0.30))

    errors <- character(0)
    warnings <- character(0)

    if (length(unique_villages) < min_unique_villages) {
        errors <- c(errors, glue("Too little village variation: found {length(unique_villages)}, expected at least {min_unique_villages}"))
    }

    if (length(unique_routes) < min_unique_routes) {
        errors <- c(errors, glue("Too little infection-route variation: found {length(unique_routes)}, expected at least {min_unique_routes}"))
    }

    if (length(named_professions) < min_named_professions) {
        errors <- c(errors, glue("Too few named professions: found {length(named_professions)}, expected at least {min_named_professions}"))
    }

    if (max(secondary_counts, 0) > max_secondary_from_one_case) {
        errors <- c(errors, glue("One case dominates transmission too strongly: max secondary_cases count is {max(secondary_counts, 0)}, limit is {max_secondary_from_one_case}"))
    }

    if (target_case_count >= 10 && primary_roots < 2) {
        warnings <- c(warnings, "Only one apparent root cluster detected; consider more than one transmission chain")
    }

    if (length(unique_outcomes) < 3) {
        warnings <- c(warnings, "Outcome variation is limited")
    }

    if (non_missing_health_zones < ceiling(target_case_count * 0.40)) {
        warnings <- c(warnings, "Health zone coverage is sparse relative to the requested demo richness")
    }

    if (non_missing_provinces < ceiling(target_case_count * 0.25)) {
        warnings <- c(warnings, "Province coverage is sparse relative to the requested demo richness")
    }

    list(errors = unique(errors), warnings = unique(warnings))
}

#' Assess Whether Synthetic Output Contains Meaningful Uncertainty
#'
#' @param linelist Candidate synthetic linelist
#' @param target_case_count Expected number of rows
#' @return List with uncertainty-related errors and warnings
assess_synthetic_uncertainty <- function(linelist, target_case_count) {
    non_missing <- function(x) normalize_missing_strings(x)

    potential_infector_counts <- vapply(linelist$potential_infector, function(value) length(parse_demo_case_ids(value)), integer(1))
    unresolved_source_cases <- sum(
        is.na(non_missing(linelist$most_probable_infector)) |
            potential_infector_counts > 1
    )

    non_confirmed_cases <- sum(non_missing(linelist$classification) %in% c("probable", "suspected"), na.rm = TRUE)
    missing_outcome_dates <- sum(is.na(non_missing(linelist$outcome_date)))
    uncertain_routes <- sum(stringr::str_detect(non_missing(linelist$infection_route), "possible|probable|reported|uncertain|unknown|not clearly"), na.rm = TRUE)

    min_uncertain_cases <- max(2, ceiling(target_case_count * 0.18))
    min_non_confirmed_cases <- max(2, ceiling(target_case_count * 0.15))

    errors <- character(0)
    warnings <- character(0)

    if (unresolved_source_cases < min_uncertain_cases) {
        errors <- c(errors, glue("Too little source uncertainty: found {unresolved_source_cases} cases, expected at least {min_uncertain_cases}"))
    }

    if (non_confirmed_cases < min_non_confirmed_cases) {
        errors <- c(errors, glue("Too few probable/suspected cases: found {non_confirmed_cases}, expected at least {min_non_confirmed_cases}"))
    }

    if (missing_outcome_dates < max(1, floor(target_case_count * 0.10))) {
        warnings <- c(warnings, "Very few cases have unresolved outcome timing")
    }

    if (uncertain_routes == 0) {
        warnings <- c(warnings, "No infection_route values explicitly signal uncertainty")
    }

    list(errors = unique(errors), warnings = unique(warnings))
}

#' Validate a Synthetic Demo Linelist
#'
#' @param linelist Candidate synthetic linelist
#' @param source_ground_truth Original source ground truth for overlap checks
#' @param target_case_count Expected number of rows
#' @return List with validation result and messages
validate_synthetic_demo_linelist <- function(linelist,
                                             source_ground_truth,
                                             target_case_count = 84,
                                             gazetteer = NULL) {
    linelist <- coerce_demo_linelist_schema(linelist)

    errors <- character(0)
    warnings <- character(0)

    if (nrow(linelist) != target_case_count) {
        errors <- c(errors, glue("Expected {target_case_count} rows, found {nrow(linelist)}"))
    }

    if (anyDuplicated(linelist$case_id) > 0) {
        dup_ids <- linelist$case_id[duplicated(linelist$case_id)] |> unique()
        errors <- c(errors, glue("Duplicate case IDs: {paste(dup_ids, collapse = ', ')}"))
    }

    valid_case_id_pattern <- stringr::str_detect(linelist$case_id, "^[A-Z]{3}[0-9]{3}$")
    if (any(!valid_case_id_pattern)) {
        errors <- c(errors, "One or more case IDs do not match the expected AAA### pattern")
    }

    contact_validation <- call_loaded_function(
        "validate_contact_relationships",
        main_linelist = linelist,
        contact_relationships = call_loaded_function("convert_to_normalized_contacts", linelist)$contact_relationships
    )

    if (!isTRUE(contact_validation$is_valid)) {
        errors <- c(errors, glue("Invalid contact references: {paste(contact_validation$missing_case_ids, collapse = ', ')}"))
    }

    onset_dates <- parse_demo_dates(linelist$onset_date)
    outcome_dates <- parse_demo_dates(linelist$outcome_date)

    bad_onset <- !is.na(normalize_missing_strings(linelist$onset_date)) & is.na(onset_dates)
    bad_outcome <- !is.na(normalize_missing_strings(linelist$outcome_date)) & is.na(outcome_dates)

    if (any(bad_onset)) {
        errors <- c(errors, glue("Unparseable onset_date values in {sum(bad_onset)} row(s)"))
    }

    if (any(bad_outcome)) {
        errors <- c(errors, glue("Unparseable outcome_date values in {sum(bad_outcome)} row(s)"))
    }

    overlap_fields <- c("name", "province", "health_zone", "health_area", "residence_village")
    overlap_messages <- purrr::map_chr(overlap_fields, function(field) {
        overlap_values <- intersect(
            unique(stats::na.omit(linelist[[field]])),
            unique(stats::na.omit(source_ground_truth[[field]]))
        )

        if (length(overlap_values) > 0) {
            return(glue("Field {field} reuses source values: {paste(head(overlap_values, 10), collapse = ', ')}"))
        }

        NA_character_
    })
    overlap_messages <- stats::na.omit(overlap_messages)

    if (length(overlap_messages) > 0) {
        errors <- c(errors, overlap_messages)
    }

    if (!is.null(gazetteer)) {
        errors <- c(errors, validate_synthetic_demo_geography(linelist, gazetteer))
    }

    relationship_to_unknown <- vapply(seq_len(nrow(linelist)), function(index) {
        row_ids <- c(
            parse_demo_case_ids(linelist$contacts[index]),
            parse_demo_case_ids(linelist$secondary_cases[index]),
            parse_demo_case_ids(linelist$potential_infector[index]),
            parse_demo_case_ids(linelist$most_probable_infector[index])
        )
        any(!row_ids %in% linelist$case_id)
    }, logical(1))

    if (any(relationship_to_unknown)) {
        errors <- c(errors, "One or more relationship fields reference non-existent case IDs")
    }

    if (mean(is.na(linelist$onset_date)) > 0.2) {
        warnings <- c(warnings, "More than 20% of synthetic cases are missing onset_date")
    }

    diversity_assessment <- assess_synthetic_variation(linelist, target_case_count)
    errors <- c(errors, diversity_assessment$errors)
    warnings <- c(warnings, diversity_assessment$warnings)

    uncertainty_assessment <- assess_synthetic_uncertainty(linelist, target_case_count)
    errors <- c(errors, uncertainty_assessment$errors)
    warnings <- c(warnings, uncertainty_assessment$warnings)

    list(
        is_valid = length(errors) == 0,
        errors = unique(errors),
        warnings = unique(warnings),
        linelist = linelist
    )
}

#' Generate a Synthetic Linelist via GPT
#'
#' @param ground_truth_path Path to source workbook
#' @param narrative_path Path to source narrative document
#' @param target_case_count Number of synthetic cases to generate
#' @param provider LLM provider
#' @param model Model name
#' @param temperature Sampling temperature
#' @param max_tokens Token budget
#' @param max_retries Number of repair attempts
#' @param sample_words Number of words to sample from source narrative
#' @return List with linelist, prompt, response, and validation results
generate_synthetic_linelist <- function(ground_truth_path,
                                        narrative_path,
                                        target_case_count = 84,
                                        provider = "azure",
                                        model = "gpt-5",
                                        case_id_prefix = "SYN",
                                        gazetteer_path = default_synthetic_gazetteer_path(),
                                        temperature = 0.4,
                                        max_tokens = 32000,
                                        max_retries = 2,
                                        sample_words = 2500) {
    assert_synthetic_demo_dependencies()

    source_ground_truth <- read_demo_source_ground_truth(ground_truth_path)
    source_summary <- summarize_demo_source_ground_truth(source_ground_truth)
    narrative_sample <- call_loaded_function("extract_document_sample", narrative_path, max_words = sample_words)
    gazetteer <- load_synthetic_demo_gazetteer(gazetteer_path)

    prompt <- build_synthetic_linelist_prompt(
        source_summary = source_summary,
        narrative_sample = narrative_sample,
        target_case_count = target_case_count,
        case_id_prefix = case_id_prefix,
        gazetteer = gazetteer
    )

    raw_response <- call_loaded_function(
        "llm_call_tidy",
        prompt = prompt,
        provider = provider,
        model = model,
        temperature = temperature,
        max_tokens = max_tokens,
        raw_prompt = TRUE
    )

    parsed <- call_loaded_function("extract_structured_data", raw_response)$linelist
    validation <- validate_synthetic_demo_linelist(
        linelist = parsed,
        source_ground_truth = source_ground_truth,
        target_case_count = target_case_count,
        gazetteer = gazetteer
    )

    attempt <- 0
    while (!validation$is_valid && attempt < max_retries) {
        attempt <- attempt + 1
        repair_prompt <- build_synthetic_repair_prompt(validation$linelist, validation$errors, target_case_count, gazetteer)
        raw_response <- call_loaded_function(
            "llm_call_tidy",
            prompt = repair_prompt,
            provider = provider,
            model = model,
            temperature = 0.2,
            max_tokens = max_tokens,
            raw_prompt = TRUE
        )
        parsed <- call_loaded_function("extract_structured_data", raw_response)$linelist
        validation <- validate_synthetic_demo_linelist(
            linelist = parsed,
            source_ground_truth = source_ground_truth,
            target_case_count = target_case_count,
            gazetteer = gazetteer
        )
    }

    if (!validation$is_valid) {
        stop(glue(
            "Synthetic linelist generation failed validation after {max_retries + 1} attempt(s): {paste(validation$errors, collapse = '; ')}"
        ))
    }

    list(
        linelist = validation$linelist,
        prompt = prompt,
        response = raw_response,
        validation = validation,
        source_ground_truth = source_ground_truth,
        source_summary = source_summary,
        narrative_sample = narrative_sample,
        gazetteer = gazetteer
    )
}

#' Build a Prompt for Narrative Generation
#'
#' @param linelist Synthetic linelist
#' @param narrative_sample Source narrative sample for style reference
#' @return Character prompt
build_synthetic_narrative_prompt <- function(linelist, narrative_sample) {
    linelist_json <- jsonlite::toJSON(linelist, pretty = TRUE, auto_unbox = TRUE, null = "null")

    glue::glue_data(
        .x = list(
            narrative_sample = narrative_sample,
            linelist_json = linelist_json
        ),
        .open = "<<",
        .close = ">>",
        "Write a synthetic outbreak narrative document that is fully consistent with the linelist below.\n\n",
        "Requirements:\n",
        "- The document is for demonstration only and must remain fully fictional.\n",
        "- Mention all case IDs at least once.\n",
        "- Keep the overall style similar to the reference sample but do not reuse source names or locations.\n",
        "- Preserve exact linelist relationships, dates, classifications, outcomes, and places from the synthetic linelist.\n",
        "- Do not introduce any extra locations that are not already present in the synthetic linelist.\n",
        "- Use markdown with varied structure: mix cluster summaries, investigator notes, short bullet lists, and individual case paragraphs instead of repeating one template for every case.\n",
        "- Vary sentence openings, paragraph length, and emphasis so the narrative does not read like copied case cards.\n",
        "- Surface variation in geography, professions, age groups, exposure settings, and outcomes so the text feels like a heterogeneous outbreak investigation.\n",
        "- Make uncertainty visible in a minority of cases: note when the source is uncertain, when exposure was only reported, or when a case remains probable/suspected rather than confirmed.\n",
        "- Do not rewrite every case as if investigators are fully certain about the chain of transmission.\n",
        "- Output only the synthetic narrative markdown text.\n\n",
        "Style reference:\n<style_reference>\n<<narrative_sample>>\n</style_reference>\n\n",
        "Synthetic linelist:\n<linelist>\n<<linelist_json>>\n</linelist>"
    )
}

#' Generate a Synthetic Narrative via GPT
#'
#' @param linelist Synthetic linelist
#' @param narrative_sample Style reference sample
#' @param provider LLM provider
#' @param model Model name
#' @param max_tokens Token budget
#' @return Character markdown narrative
generate_synthetic_narrative <- function(linelist,
                                         narrative_sample,
                                         provider = "azure",
                                         model = "gpt-5",
                                         max_tokens = 24000) {
    prompt <- build_synthetic_narrative_prompt(linelist, narrative_sample)

    call_loaded_function(
        "llm_call_tidy",
        prompt = prompt,
        provider = provider,
        model = model,
        temperature = 0.4,
        max_tokens = max_tokens,
        raw_prompt = TRUE
    )
}

#' Write Simple Markdown to DOCX
#'
#' @param markdown_text Markdown text
#' @param output_path Output DOCX path
#' @return Output path
write_markdown_docx <- function(markdown_text, output_path) {
    if (!requireNamespace("officer", quietly = TRUE)) {
        warning("officer is not installed; skipping DOCX output")
        return(NULL)
    }

    lines <- stringr::str_split(markdown_text, "\\n")[[1]]
    doc <- officer::read_docx()

    for (line in lines) {
        trimmed <- trimws(line)

        if (!nzchar(trimmed)) {
            doc <- officer::body_add_par(doc, "", style = "Normal")
        } else if (stringr::str_starts(trimmed, "### ")) {
            doc <- officer::body_add_par(doc, stringr::str_remove(trimmed, "^###\\s+"), style = "heading 3")
        } else if (stringr::str_starts(trimmed, "## ")) {
            doc <- officer::body_add_par(doc, stringr::str_remove(trimmed, "^##\\s+"), style = "heading 2")
        } else if (stringr::str_starts(trimmed, "# ")) {
            doc <- officer::body_add_par(doc, stringr::str_remove(trimmed, "^#\\s+"), style = "heading 1")
        } else {
            doc <- officer::body_add_par(doc, trimmed, style = "Normal")
        }
    }

    print(doc, target = output_path)
    output_path
}

#' Write Synthetic Demo Outputs to Disk
#'
#' @param output_dir Directory for outputs
#' @param file_prefix Prefix for file names
#' @param linelist Synthetic linelist
#' @param narrative_markdown Synthetic narrative
#' @param metadata Metadata list to serialize
#' @return Named list of written paths
write_synthetic_demo_outputs <- function(output_dir,
                                         file_prefix,
                                         linelist,
                                         narrative_markdown,
                                         metadata) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    output_paths <- list(
        linelist_csv = file.path(output_dir, glue("{file_prefix}_linelist.csv")),
        linelist_xlsx = file.path(output_dir, glue("{file_prefix}_linelist.xlsx")),
        linelist_qs = file.path(output_dir, glue("{file_prefix}_linelist.qs")),
        narrative_md = file.path(output_dir, glue("{file_prefix}_narrative.md")),
        narrative_txt = file.path(output_dir, glue("{file_prefix}_narrative.txt")),
        narrative_docx = file.path(output_dir, glue("{file_prefix}_narrative.docx")),
        metadata_json = file.path(output_dir, glue("{file_prefix}_metadata.json"))
    )

    readr::write_csv(linelist, output_paths$linelist_csv, na = "")
    writexl::write_xlsx(linelist, output_paths$linelist_xlsx)
    qs::qsave(linelist, output_paths$linelist_qs)
    readr::write_file(narrative_markdown, output_paths$narrative_md)
    readr::write_file(narrative_markdown, output_paths$narrative_txt)
    jsonlite::write_json(metadata, output_paths$metadata_json, pretty = TRUE, auto_unbox = TRUE, null = "null")
    write_markdown_docx(narrative_markdown, output_paths$narrative_docx)

    output_paths
}

#' Generate a Full Synthetic Demo Dataset
#'
#' @param ground_truth_path Path to source workbook
#' @param narrative_path Path to source narrative document
#' @param target_case_count Number of synthetic cases
#' @param provider LLM provider
#' @param model Model name
#' @param output_dir Directory for outputs
#' @param file_prefix Prefix for generated files
#' @param max_retries Repair attempts for linelist generation
#' @return List with linelist, narrative, metadata, and output paths
generate_synthetic_demo_dataset <- function(ground_truth_path = here::here("data", "msf_data", "raw", "narrative_linelist_gt_mpi_inferred.xlsx"),
                                            narrative_path = here::here("data", "msf_data", "raw", "Narratif_draft_18june_edit.docx"),
                                            target_case_count = 84,
                                            provider = "azure",
                                            model = "gpt-5",
                                            case_id_prefix = "SYN",
                                            gazetteer_path = default_synthetic_gazetteer_path(),
                                            output_dir = here::here("data", "msf_data", "raw"),
                                            file_prefix = glue("synthetic_msf_demo_{target_case_count}_cases"),
                                            max_retries = 2) {
    linelist_result <- generate_synthetic_linelist(
        ground_truth_path = ground_truth_path,
        narrative_path = narrative_path,
        target_case_count = target_case_count,
        provider = provider,
        model = model,
        case_id_prefix = case_id_prefix,
        gazetteer_path = gazetteer_path,
        max_retries = max_retries
    )

    narrative_markdown <- generate_synthetic_narrative(
        linelist = linelist_result$linelist,
        narrative_sample = linelist_result$narrative_sample,
        provider = provider,
        model = model
    )

    metadata <- list(
        generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
        provider = provider,
        model = model,
        case_id_prefix = case_id_prefix,
        target_case_count = target_case_count,
        ground_truth_path = ground_truth_path,
        narrative_path = narrative_path,
        gazetteer_path = linelist_result$gazetteer$path,
        validation = list(
            warnings = linelist_result$validation$warnings,
            errors = linelist_result$validation$errors
        )
    )

    output_paths <- write_synthetic_demo_outputs(
        output_dir = output_dir,
        file_prefix = file_prefix,
        linelist = linelist_result$linelist,
        narrative_markdown = narrative_markdown,
        metadata = metadata
    )

    list(
        linelist = linelist_result$linelist,
        narrative_markdown = narrative_markdown,
        metadata = metadata,
        output_paths = output_paths,
        linelist_prompt = linelist_result$prompt,
        linelist_response = linelist_result$response
    )
}