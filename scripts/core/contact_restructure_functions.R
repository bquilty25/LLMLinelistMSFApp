# Contact Restructure Functions
# Functions to convert between comma-separated contact lists and normalized contact relationship tables

library(tidyverse)
library(qs)
library(readr)

#' Convert Comma-Separated Contacts to Relationship Table
#'
#' Converts the old format (comma-separated contacts in main linelist) to
#' normalized relationship table format
#'
#' @param linelist_data Tibble with comma-separated contacts/secondary_cases columns
#' @return List with main_linelist and contact_relationships
convert_to_normalized_contacts <- function(linelist_data) {
    cat("Converting comma-separated contacts to normalized relationship format...\n")

    # Helper function to parse comma-separated case IDs
    parse_case_ids <- function(text, source_case_id) {
        if (is.na(text) || text == "" || text == "null") {
            return(tibble())
        }

        # Extract generic three-letter case IDs (e.g. BIK001, WGT001, SYN001)
        case_ids <- str_extract_all(text, "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]

        if (length(case_ids) == 0) {
            return(tibble())
        }

        tibble(
            case_id = source_case_id,
            contact_id = unique(toupper(trimws(case_ids)))
        )
    }

    # Extract contact relationships from contacts column
    contact_relationships <- linelist_data %>%
        select(case_id, contacts) %>%
        filter(!is.na(contacts), contacts != "", contacts != "null") %>%
        pmap_dfr(~ parse_case_ids(..2, ..1)) %>%
        mutate(relationship_type = "contact")

    # Extract secondary case relationships from secondary_cases column
    secondary_relationships <- linelist_data %>%
        select(case_id, secondary_cases) %>%
        filter(!is.na(secondary_cases), secondary_cases != "", secondary_cases != "null") %>%
        pmap_dfr(~ parse_case_ids(..2, ..1)) %>%
        mutate(
            relationship_type = "secondary_case",
            # For secondary cases, the direction is reversed - case_id infected contact_id
            temp_case_id = case_id,
            case_id = contact_id,
            contact_id = temp_case_id
        ) %>%
        select(-temp_case_id)

    # Combine all relationships
    all_relationships <- bind_rows(contact_relationships, secondary_relationships) %>%
        distinct() %>%
        arrange(case_id, contact_id)

    # Keep main linelist WITH comma-separated contact columns (for direct comparison)
    main_linelist <- linelist_data

    cat("Extracted", nrow(all_relationships), "contact relationships\n")
    cat("Contact relationships:", sum(all_relationships$relationship_type == "contact"), "\n")
    cat("Secondary case relationships:", sum(all_relationships$relationship_type == "secondary_case"), "\n")

    list(
        main_linelist = main_linelist,
        contact_relationships = all_relationships
    )
}

#' Convert Normalized Contacts Back to Comma-Separated Format
#'
#' Converts normalized relationship table back to comma-separated format for compatibility
#'
#' @param main_linelist Main linelist without contact columns
#' @param contact_relationships Relationship table with case_id, contact_id, relationship_type
#' @return Tibble with comma-separated contacts and secondary_cases columns restored
convert_from_normalized_contacts <- function(main_linelist, contact_relationships) {
    cat("Converting normalized relationships back to comma-separated format...\n")

    # Aggregate contacts by case_id
    contacts_aggregated <- contact_relationships %>%
        filter(relationship_type == "contact") %>%
        group_by(case_id) %>%
        summarise(contacts = paste(contact_id, collapse = ", "), .groups = "drop")

    # Aggregate secondary cases by contact_id (reversed relationship)
    secondary_cases_aggregated <- contact_relationships %>%
        filter(relationship_type == "secondary_case") %>%
        group_by(contact_id) %>%
        summarise(secondary_cases = paste(case_id, collapse = ", "), .groups = "drop") %>%
        rename(case_id = contact_id)

    # Join back to main linelist
    result <- main_linelist %>%
        left_join(contacts_aggregated, by = "case_id") %>%
        left_join(secondary_cases_aggregated, by = "case_id") %>%
        mutate(
            contacts = if_else(is.na(contacts), "null", contacts),
            secondary_cases = if_else(is.na(secondary_cases), "null", secondary_cases)
        )

    cat("Restored comma-separated format for", nrow(result), "cases\n")

    return(result)
}

#' Create Ground Truth in New Format
#'
#' Creates ground truth files in the new normalized format from existing data
#'
#' @param input_tsv_path Path to existing TSV with comma-separated contacts
#' @param output_dir Directory to save new ground truth files
create_new_ground_truth <- function(input_tsv_path, output_dir = "data/msf_data/ground_truth") {
    cat("Creating new ground truth format from:", input_tsv_path, "\n")

    # Ensure output directory exists
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Load existing data
    if (!file.exists(input_tsv_path)) {
        stop("Input TSV file not found: ", input_tsv_path)
    }

    existing_data <- read_csv(input_tsv_path, show_col_types = FALSE)
    cat("Loaded", nrow(existing_data), "cases from existing data\n")

    # Convert to normalized format
    normalized_data <- convert_to_normalized_contacts(existing_data)

    # Save main linelist (without contact columns)
    main_linelist_path <- file.path(output_dir, "main_linelist.csv")
    write_csv(normalized_data$main_linelist, main_linelist_path)
    cat("Saved main linelist to:", main_linelist_path, "\n")

    # Save contact relationships
    relationships_path <- file.path(output_dir, "contact_relationships.csv")
    write_csv(normalized_data$contact_relationships, relationships_path)
    cat("Saved contact relationships to:", relationships_path, "\n")

    # Save as QS for faster loading
    ground_truth_package <- list(
        main_linelist = normalized_data$main_linelist,
        contact_relationships = normalized_data$contact_relationships,
        creation_date = Sys.time(),
        source_file = input_tsv_path
    )

    qs_path <- file.path(output_dir, "ground_truth_normalized.qs")
    qsave(ground_truth_package, qs_path)
    cat("Saved ground truth package to:", qs_path, "\n")

    return(list(
        main_linelist_path = main_linelist_path,
        relationships_path = relationships_path,
        qs_path = qs_path,
        data = normalized_data
    ))
}

#' Load Normalized Ground Truth
#'
#' Load ground truth data in the new normalized format
#'
#' @param ground_truth_dir Directory containing ground truth files
#' @return List with main_linelist and contact_relationships
load_normalized_ground_truth <- function(ground_truth_dir = "data/msf_data/ground_truth") {
    qs_path <- file.path(ground_truth_dir, "ground_truth_normalized.qs")

    main_linelist <- NULL
    contact_relationships <- NULL

    if (file.exists(qs_path)) {
        cat("Loading ground truth from QS file:", qs_path, "\n")
        ground_truth_package <- qread(qs_path)
        main_linelist <- ground_truth_package$main_linelist
        contact_relationships <- ground_truth_package$contact_relationships
    } else {
        # Fall back to CSV files
        main_path <- file.path(ground_truth_dir, "main_linelist.csv")
        relationships_path <- file.path(ground_truth_dir, "contact_relationships.csv")

        if (file.exists(main_path) && file.exists(relationships_path)) {
            cat("Loading ground truth from CSV files\n")
            main_linelist <- read_csv(main_path, show_col_types = FALSE)
            contact_relationships <- read_csv(relationships_path, show_col_types = FALSE)
        } else {
            stop("Ground truth files not found in directory: ", ground_truth_dir)
        }
    }

    # Convert back to single linelist with comma-separated contacts
    # This aligns with the user's request for "only <linelist>"
    combined_linelist <- convert_from_normalized_contacts(main_linelist, contact_relationships)

    return(list(
        main_linelist = combined_linelist,
        contact_relationships = tibble(case_id = character(), contact_id = character(), relationship_type = character())
    ))
}

#' Validate Contact Relationships
#'
#' Validate that all case_ids in relationships exist in main linelist
#'
#' @param main_linelist Main linelist data
#' @param contact_relationships Contact relationships data
#' @return List with validation results
validate_contact_relationships <- function(main_linelist, contact_relationships) {
    cat("Validating contact relationships...\n")

    existing_case_ids <- unique(main_linelist$case_id)

    # Check case_ids in relationships
    relationship_case_ids <- unique(c(contact_relationships$case_id, contact_relationships$contact_id))
    missing_case_ids <- setdiff(relationship_case_ids, existing_case_ids)

    # Summary statistics
    n_relationships <- nrow(contact_relationships)
    n_contacts <- sum(contact_relationships$relationship_type == "contact")
    n_secondary <- sum(contact_relationships$relationship_type == "secondary_case")

    validation_result <- list(
        is_valid = length(missing_case_ids) == 0,
        missing_case_ids = missing_case_ids,
        n_missing = length(missing_case_ids),
        n_relationships = n_relationships,
        n_contact_relationships = n_contacts,
        n_secondary_case_relationships = n_secondary,
        summary = glue::glue(
            "Contact relationships validation:",
            "- Total relationships: {n_relationships}",
            "- Contact relationships: {n_contacts}",
            "- Secondary case relationships: {n_secondary}",
            "- Missing case IDs: {length(missing_case_ids)}",
            "- Status: {ifelse(length(missing_case_ids) == 0, 'VALID', 'INVALID')}",
            .sep = "\n"
        )
    )

    cat(validation_result$summary, "\n")

    if (length(missing_case_ids) > 0) {
        cat("Missing case IDs:", paste(missing_case_ids, collapse = ", "), "\n")
    }

    return(validation_result)
}
