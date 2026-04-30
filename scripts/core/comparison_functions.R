# =============================================================================
# COMPARISON FUNCTIONS
# Functions for comparing LLM output vs ground truth
# =============================================================================

#' Fuzzy Value Match
#' 
#' Centralized fuzzy string matching function for dataset comparisons.
#' Correctly handles standard text equality, strings with common African names/order inversions,
#' and list data like contacts and secondary cases.
#' 
#' @param val1 Value to compare
#' @param val2 Second value to compare
#' @param column_name The name of the column, used for field-specific matching logic
#' @return Boolean indicating match
#' @export
fuzzy_match <- function(val1, val2, column_name = "") {
    if (is.na(val1) && is.na(val2)) {
        return(TRUE)
    }
    if (is.na(val1) || is.na(val2)) {
        return(FALSE)
    }
    if (val1 == "null" && val2 == "null") {
        return(TRUE)
    }
    if (val1 == "null" || val2 == "null") {
        return(FALSE)
    }
    if (val1 == "" && val2 == "") {
        return(TRUE)
    }
    if (val1 == "" || val2 == "") {
        return(FALSE)
    }

    # Special handling for comma-separated list fields (contacts, secondary_cases, potential_infector)
    if (column_name %in% c("contacts", "secondary_cases", "potential_infector", "most_probable_infector")) {
        # Parse comma-separated lists and extract case IDs
        parse_case_ids <- function(text) {
            # Extract generic three-letter case IDs
            case_ids <- stringr::str_extract_all(text, "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]
            unique(toupper(case_ids))
        }

        ids1 <- parse_case_ids(val1)
        ids2 <- parse_case_ids(val2)

        if (length(ids1) == 0 && length(ids2) == 0) {
            return(TRUE)
        }
        if (length(ids1) == 0 || length(ids2) == 0) {
            return(FALSE)
        }

        # Use strict set equality (order-independent)
        # Both sets must contain exactly the same case IDs
        return(setequal(ids1, ids2))
    }

    # Special handling for name field to account for word order variations
    if (column_name == "name") {
        # Clean and tokenize names
        clean_name <- function(name) {
            if (is.na(name) || name == "" || name == "null") {
                return(character(0))
            }
            # Remove special chars except spaces, lowercase
            cleaned <- tolower(trimws(gsub("[^a-zA-Z ]", "", as.character(name))))
            # Normalize multiple spaces to single spaces and split into words
            words <- strsplit(gsub(" +", " ", cleaned), " ")[[1]]
            # Sort words to handle order differences, filter out short words
            sort(words[nchar(words) >= 2]) # Allow 2+ char words for African names
        }

        words1 <- clean_name(val1)
        words2 <- clean_name(val2)
        
        if (length(words1) == 0 || length(words2) == 0) return(FALSE)
        
        # Combine sorted words back into strings
        sorted1 <- paste(words1, collapse = " ")
        sorted2 <- paste(words2, collapse = " ")
        
        # Use character-level generalized string distance (Levenshtein)
        # max.distance = 0.2 means allowing ~20% character differences (typos, missing accents)
        return(agrepl(sorted1, sorted2, max.distance = 0.2, ignore.case = TRUE) | 
               agrepl(sorted2, sorted1, max.distance = 0.2, ignore.case = TRUE))
    }

    # Standard fuzzy matching for other fields
    # Clean values for comparison (handle accents, spacing, case)
    clean1 <- tolower(trimws(gsub("[-_\\s]+", " ", gsub("[àáâãäéèêëïîíìôöòóõùúûüç]", "e", val1))))
    clean2 <- tolower(trimws(gsub("[-_\\s]+", " ", gsub("[àáâãäéèêëïîíìôöòóõùúûüç]", "e", val2))))

    return(clean1 == clean2)
}
# Functions for comparing LLM output vs ground truth
# =============================================================================

#' Standardize Column Names
#'
#' Map different column naming conventions to standard names
#'
#' @param df Dataframe to standardize
#' @return Dataframe with standardized column names
standardize_columns <- function(df) {
    # Create mapping of common variations to standard names
    column_mapping <- c(
        # Case ID variations
        "case_id" = "case_id",
        "Case_ID" = "case_id",
        "id" = "case_id",
        "patient_id" = "case_id",

        # Name variations
        "name" = "name",
        "Name" = "name",
        "patient_name" = "name",

        # Demographics
        "sex" = "sex",
        "Sex" = "sex",
        "gender" = "sex",
        "age" = "age",
        "Age" = "age",
        "age_unit" = "age_unit",
        "age_units" = "age_unit",

        # Geography
        "province" = "province",
        "Province" = "province",
        "health_zone" = "health_zone",
        "health_area" = "health_area",
        "residence_village" = "residence_village",
        "village" = "residence_village",

        # Clinical data
        "profession" = "profession",
        "occupation" = "profession",
        "onset_date" = "onset_date",
        "date_onset" = "onset_date",
        "symptom_onset" = "onset_date",
        "outcome_date" = "outcome_date",
        "outcome" = "outcome",
        "status" = "outcome",
        "classification" = "classification",

        # Transmission data
        "potential_infector" = "potential_infector",
        "infector" = "potential_infector",
        "source_case" = "potential_infector",
        "infection_route" = "infection_route",
        "route" = "infection_route",
        "most_probable_infector" = "most_probable_infector",
        "secondary_cases" = "secondary_cases",
        "contacts" = "contacts"
    )

    # Apply mapping
    names(df) <- sapply(names(df), function(x) {
        mapped_name <- column_mapping[tolower(x)]
        if (is.na(mapped_name)) x else mapped_name
    })

    return(df)
}

#' Compare Linelists
#'
#' Calculate detailed comparison metrics between LLM and ground truth
#'
#' @param llm_data LLM-generated linelist
#' @param ground_truth Ground truth linelist
#' @return List with detailed comparison metrics
compare_linelists <- function(llm_data, ground_truth) {
    cat("Comparing linelists...\n")

    # Standardize column names
    llm_std <- standardize_columns(llm_data)
    gt_std <- standardize_columns(ground_truth)

    # Identify shared and different columns
    shared_cols <- intersect(names(llm_std), names(gt_std))
    llm_only_cols <- setdiff(names(llm_std), names(gt_std))
    gt_only_cols <- setdiff(names(gt_std), names(llm_std))

    cat("Shared columns:", length(shared_cols), "\n")
    cat("LLM-only columns:", length(llm_only_cols), "\n")
    cat("Ground truth-only columns:", length(gt_only_cols), "\n")

    # Name-based case detection and matching
    case_id_performance <- analyze_case_detection(llm_std, gt_std)
    match_results <- case_id_performance$match_results

    # Column-wise comparison for shared columns using name matching
    column_metrics <- purrr::map_dfr(shared_cols, ~ {
        col_comparison <- compare_column_values(llm_std, gt_std, .x, match_results)
        tibble::tibble(
            column = .x,
            precision = col_comparison$precision,
            recall = col_comparison$recall,
            f1_score = col_comparison$f1_score,
            accuracy = col_comparison$accuracy,
            completeness_llm = col_comparison$completeness_llm,
            completeness_gt = col_comparison$completeness_gt
        )
    })

    # Overall metrics
    overall_metrics <- calculate_overall_metrics(column_metrics)

    # Return comprehensive results
    list(
        structure = list(
            shared_cols = shared_cols,
            llm_only_cols = llm_only_cols,
            gt_only_cols = gt_only_cols,
            n_shared = length(shared_cols),
            n_llm_only = length(llm_only_cols),
            n_gt_only = length(gt_only_cols)
        ),
        case_detection = case_id_performance,
        column_metrics = column_metrics,
        overall_metrics = overall_metrics,
        data = list(
            llm_standardized = llm_std,
            gt_standardized = gt_std
        )
    )
}

#' Match Cases by Name Similarity
#'
#' Find matching cases between LLM and GT data using name similarity
#'
#' @param llm_data LLM data with name column
#' @param gt_data Ground truth data with name column
#' @return List with matched cases and unmatched indices
match_cases_by_name <- function(llm_data, gt_data) {
    # Function to clean and tokenize names for flexible matching
    clean_name <- function(name) {
        if (is.na(name) || name == "" || name == "null") {
            return(character(0))
        }
        # Remove special chars except spaces, lowercase
        cleaned <- tolower(trimws(gsub("[^a-zA-Z ]", "", as.character(name))))
        # Normalize multiple spaces to single spaces and split into words
        words <- strsplit(gsub(" +", " ", cleaned), " ")[[1]]
        # Sort words to handle order differences, filter out short words
        sort(words[nchar(words) >= 3])
    }

    # Function to calculate name similarity
    name_similarity <- function(name1_words, name2_words) {
        if (length(name1_words) == 0 || length(name2_words) == 0) {
            return(0)
        }

        # Calculate Jaccard similarity (intersection / union)
        intersection <- length(intersect(name1_words, name2_words))
        union <- length(union(name1_words, name2_words))

        if (union == 0) {
            return(0)
        }
        return(intersection / union)
    }

    # Clean and tokenize names
    llm_names_tokens <- purrr::map(llm_data$name, clean_name)
    gt_names_tokens <- purrr::map(gt_data$name, clean_name)

    # Find best matches using similarity threshold
    name_matches <- tibble::tibble(
        llm_idx = integer(),
        gt_idx = integer(),
        case_name = character(),
        similarity = numeric()
    )

    gt_used <- rep(FALSE, nrow(gt_data))
    threshold <- 0.5 # At least 50% word overlap

    # Find matches with similarity scoring
    for (i in seq_along(llm_names_tokens)) {
        if (length(llm_names_tokens[[i]]) > 0) {
            # Calculate similarity with all unused GT names
            similarities <- purrr::map_dbl(seq_along(gt_names_tokens), function(j) {
                if (gt_used[j]) {
                    return(0)
                }
                name_similarity(llm_names_tokens[[i]], gt_names_tokens[[j]])
            })

            # Find best match above threshold
            best_match <- which.max(similarities)
            if (length(best_match) > 0 && similarities[best_match] >= threshold) {
                name_matches <- dplyr::bind_rows(name_matches, tibble::tibble(
                    llm_idx = i,
                    gt_idx = best_match,
                    case_name = llm_data$name[i],
                    similarity = similarities[best_match]
                ))
                gt_used[best_match] <- TRUE
            }
        }
    }

    # Find unmatched cases
    unmatched_llm <- setdiff(1:nrow(llm_data), name_matches$llm_idx)
    unmatched_gt <- setdiff(1:nrow(gt_data), name_matches$gt_idx)

    cat("Name-based matching found:", nrow(name_matches), "matched cases\n")
    cat("LLM extra cases:", length(unmatched_llm), "\n")
    cat("GT missing cases:", length(unmatched_gt), "\n")

    return(list(
        matches = name_matches,
        unmatched_llm = unmatched_llm,
        unmatched_gt = unmatched_gt,
        threshold_used = threshold
    ))
}

#' Analyze Case Detection Performance
#'
#' Compare case identification between LLM and ground truth using name matching
#' Includes categorization of error types
#'
#' @param llm_data Standardized LLM data
#' @param gt_data Standardized ground truth data
#' @return List with case detection metrics and error analysis
analyze_case_detection <- function(llm_data, gt_data) {
    # Use name-based matching instead of case IDs
    match_results <- match_cases_by_name(llm_data, gt_data)

    true_positives <- nrow(match_results$matches)
    false_positives <- length(match_results$unmatched_llm)
    false_negatives <- length(match_results$unmatched_gt)

    # --- Error Categorization ---

    # Analyze False Positives (Extra LLM cases)
    fp_analysis <- tibble::tibble(
        llm_idx = match_results$unmatched_llm,
        name = llm_data$name[match_results$unmatched_llm],
        category = "Unknown"
    )

    if (nrow(fp_analysis) > 0) {
        # Categorize FPs
        fp_analysis <- fp_analysis %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
                category = dplyr::case_when(
                    # Check if name is empty or null
                    is.na(name) | name == "null" | name == "" ~ "Empty/Null Name",
                    # Check if it looks like a header or noise (short, non-name chars)
                    nchar(name) < 3 ~ "Noise/Fragment",
                    TRUE ~ "Hallucination / Extra Extraction"
                )
            ) %>%
            dplyr::ungroup()
    }

    # Analyze False Negatives (Missing GT cases)
    fn_analysis <- tibble::tibble(
        gt_idx = match_results$unmatched_gt,
        name = gt_data$name[match_results$unmatched_gt],
        category = "Unknown"
    )

    if (nrow(fn_analysis) > 0) {
        # Categorize FNs
        fn_analysis <- fn_analysis %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
                category = dplyr::case_when(
                    is.na(name) | name == "null" | name == "" ~ "Empty GT Name",
                    TRUE ~ "Missed Case"
                )
            ) %>%
            dplyr::ungroup()
    }

    # Calculate standard metrics
    precision <- ifelse(nrow(llm_data) == 0, 0, true_positives / nrow(llm_data))
    recall <- ifelse(nrow(gt_data) == 0, 0, true_positives / nrow(gt_data))
    f1_score <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))

    list(
        true_positives = true_positives,
        false_positives = false_positives,
        false_negatives = false_negatives,
        precision = precision,
        recall = recall,
        f1_score = f1_score,
        n_llm_cases = nrow(llm_data),
        n_gt_cases = nrow(gt_data),
        detected_cases = match_results$matches$case_name,
        missed_cases = if (length(match_results$unmatched_gt) > 0) gt_data$name[match_results$unmatched_gt] else character(0),
        extra_cases = if (length(match_results$unmatched_llm) > 0) llm_data$name[match_results$unmatched_llm] else character(0),
        match_results = match_results, # Include detailed matching results
        error_analysis = list(
            false_positives = fp_analysis,
            false_negatives = fn_analysis
        )
    )
}

#' Compare Column Values Using Name-Based Matching
#'
#' Compare values for a specific column between datasets using matched cases
#' Includes categorization of mismatch types
#'
#' @param llm_data LLM data
#' @param gt_data Ground truth data
#' @param column Column name to compare
#' @param match_results Results from match_cases_by_name
#' @return List with column comparison metrics and error breakdown
compare_column_values <- function(llm_data, gt_data, column, match_results = NULL) {
    if (!column %in% names(llm_data) || !column %in% names(gt_data)) {
        return(list(
            precision = NA, recall = NA, f1_score = NA, accuracy = NA,
            completeness_llm = NA, completeness_gt = NA,
            error_breakdown = NULL
        ))
    }

    # Get values (handle missing values)
    llm_vals <- llm_data[[column]]
    gt_vals <- gt_data[[column]]

    # Calculate completeness (non-missing rate)
    completeness_llm <- sum(!is.na(llm_vals) & llm_vals != "" & llm_vals != "null") / length(llm_vals)
    completeness_gt <- sum(!is.na(gt_vals) & gt_vals != "" & gt_vals != "null") / length(gt_vals)

    error_breakdown <- NULL

    # If no match results provided, fall back to original method
    if (is.null(match_results)) {
        # For other columns, calculate overlap-based metrics
        llm_unique <- unique(llm_vals[!is.na(llm_vals) & llm_vals != "" & llm_vals != "null"])
        gt_unique <- unique(gt_vals[!is.na(gt_vals) & gt_vals != "" & gt_vals != "null"])

        if (length(llm_unique) == 0 && length(gt_unique) == 0) {
            precision <- recall <- f1_score <- accuracy <- 1.0
        } else if (length(llm_unique) == 0) {
            precision <- recall <- f1_score <- accuracy <- 0.0
        } else if (length(gt_unique) == 0) {
            precision <- recall <- f1_score <- accuracy <- 0.0
        } else {
            overlap <- length(intersect(llm_unique, gt_unique))
            precision <- overlap / length(llm_unique)
            recall <- overlap / length(gt_unique)
            f1_score <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
            accuracy <- overlap / length(union(llm_unique, gt_unique))
        }
    } else {
        # Use name-based matching for case-by-case comparison
        matches <- match_results$matches

        if (nrow(matches) == 0) {
            precision <- recall <- f1_score <- accuracy <- 0.0
        } else {
            # Create comparison dataframe using tidyverse
            comparison_df <- matches %>%
                dplyr::mutate(
                    llm_val = llm_vals[llm_idx],
                    gt_val = gt_vals[gt_idx],
                    llm_has_value = !is.na(llm_val) & llm_val != "" & llm_val != "null",
                    gt_has_value = !is.na(gt_val) & gt_val != "" & gt_val != "null",
                    both_have_values = llm_has_value & gt_has_value
                ) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(
                    values_match = dplyr::if_else(both_have_values,
                        fuzzy_match(llm_val, gt_val, column),
                        FALSE
                    ),
                    # Categorize errors
                    error_type = dplyr::case_when(
                        values_match ~ "None",
                        !llm_has_value & gt_has_value ~ "False Negative (Missing Value)",
                        llm_has_value & !gt_has_value ~ "False Positive (Hallucinated Value)",
                        llm_has_value & gt_has_value & !values_match ~ "Content Mismatch",
                        TRUE ~ "Other"
                    )
                ) %>%
                dplyr::ungroup()

            # Count metrics using tidyverse
            true_positives <- sum(comparison_df$values_match)
            llm_non_null_matched <- sum(comparison_df$llm_has_value)
            gt_non_null_matched <- sum(comparison_df$gt_has_value)
            both_have_values <- sum(comparison_df$both_have_values)

            # Generate error breakdown
            error_breakdown <- comparison_df %>%
                dplyr::filter(error_type != "None") %>%
                dplyr::count(error_type) %>%
                dplyr::mutate(column = column)

            # Calculate metrics
            # Precision: of LLM's non-null values in matched cases, how many are correct?
            precision <- if (llm_non_null_matched > 0) true_positives / llm_non_null_matched else 0

            # Recall: of GT's non-null values in matched cases, how many did LLM get right?
            recall <- if (gt_non_null_matched > 0) true_positives / gt_non_null_matched else 0

            # F1-score: harmonic mean
            f1_score <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))

            # Accuracy: for cases where both have values, what proportion match?
            accuracy <- if (both_have_values > 0) true_positives / both_have_values else 0
        }
    }

    list(
        precision = precision,
        recall = recall,
        f1_score = f1_score,
        accuracy = accuracy,
        completeness_llm = completeness_llm,
        completeness_gt = completeness_gt,
        error_breakdown = error_breakdown
    )
}

#' Calculate Overall Metrics
#'
#' Aggregate column-level metrics to overall performance
#'
#' @param column_metrics Tibble of column-level metrics
#' @return List with overall metrics
calculate_overall_metrics <- function(column_metrics) {
    # Calculate means, handling NAs
    safe_mean <- function(x) mean(x, na.rm = TRUE)

    list(
        precision = safe_mean(column_metrics$precision),
        recall = safe_mean(column_metrics$recall),
        f1_score = safe_mean(column_metrics$f1_score),
        accuracy = safe_mean(column_metrics$accuracy),
        completeness_llm = safe_mean(column_metrics$completeness_llm),
        completeness_gt = safe_mean(column_metrics$completeness_gt),
        n_columns_compared = sum(!is.na(column_metrics$precision))
    )
}
