# =============================================================================
# METRICS FUNCTIONS
# Functions for computing and aggregating validation metrics
# =============================================================================

#' Compute Case-Level Metrics
#'
#' Calculate precision, recall, F1, and date accuracy for matched cases
#'
#' @param llm_normalized Normalized LLM results (list with main_linelist)
#' @param gt_normalized Normalized ground truth (list with main_linelist)
#' @param few_shot_ids Character vector of case IDs to exclude from metrics
#' @return List with case_f1, case_recall, case_precision, date_accuracy, and case counts
#' @export
compute_case_metrics <- function(llm_normalized, gt_normalized, few_shot_ids = character(0)) {
    gt_main <- gt_normalized$main_linelist
    llm_main <- llm_normalized$main_linelist

    gt_cases <- unique(gt_main$case_id)
    llm_cases <- unique(llm_main$case_id)

    # Exclude few-shot examples
    if (length(few_shot_ids) > 0) {
        gt_cases <- setdiff(gt_cases, few_shot_ids)
        llm_cases <- setdiff(llm_cases, few_shot_ids)
        gt_main <- gt_main |> dplyr::filter(!case_id %in% few_shot_ids)
        llm_main <- llm_main |> dplyr::filter(!case_id %in% few_shot_ids)
    }

    matched_cases <- intersect(gt_cases, llm_cases)

    # If no case_id matches, try matching by name as fallback
    # This handles MLX runs that generate generic IDs like "Case_1"
    used_name_matching <- FALSE
    if (length(matched_cases) == 0 && nrow(gt_main) > 0 && nrow(llm_main) > 0) {
        # Normalize names for matching - ORDER INDEPENDENT
        normalize_name <- function(x) {
            if (is.na(x) || x == "" || tolower(x) == "null") {
                return(NA_character_)
            }
            # Lowercase, remove accents, trim whitespace
            x <- tolower(trimws(x))
            x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
            # Split into words, sort alphabetically, rejoin
            words <- strsplit(x, "\\s+")[[1]]
            words <- words[nchar(words) >= 2] # Remove single-letter parts
            if (length(words) == 0) {
                return(NA_character_)
            }
            paste(sort(words), collapse = " ")
        }

        gt_main <- gt_main |> dplyr::mutate(name_normalized = sapply(name, normalize_name))
        llm_main <- llm_main |> dplyr::mutate(name_normalized = sapply(name, normalize_name))

        # Match by normalized name
        gt_names <- unique(gt_main$name_normalized[!is.na(gt_main$name_normalized)])
        llm_names <- unique(llm_main$name_normalized[!is.na(llm_main$name_normalized)])
        matched_names <- intersect(gt_names, llm_names)

        if (length(matched_names) > 0) {
            used_name_matching <- TRUE
            # Use name counts for metrics
            n_gt <- length(gt_names)
            n_llm <- length(llm_names)
            n_matched <- length(matched_names)
            matched_cases <- matched_names # For date accuracy calc, use matched names
        }
    }

    if (!used_name_matching) {
        n_gt <- length(gt_cases)
        n_llm <- length(llm_cases)
        n_matched <- length(matched_cases)
    }

    case_recall <- if (n_gt > 0) n_matched / n_gt else 0
    case_precision <- if (n_llm > 0) n_matched / n_llm else 0
    case_f1 <- if ((case_precision + case_recall) > 0) {
        2 * (case_precision * case_recall) / (case_precision + case_recall)
    } else {
        0
    }

    # Date accuracy for matched cases
    date_accuracy <- NA_real_
    if (n_matched > 0 && !used_name_matching) {
        gt_dates <- gt_normalized$main_linelist |>
            dplyr::filter(case_id %in% matched_cases) |>
            dplyr::select(case_id, gt_date = onset_date)

        llm_dates <- llm_normalized$main_linelist |>
            dplyr::filter(case_id %in% matched_cases) |>
            dplyr::select(case_id, llm_date = onset_date)

        date_comparison <- dplyr::inner_join(gt_dates, llm_dates, by = "case_id") |>
            dplyr::mutate(match = (gt_date == llm_date) | (is.na(gt_date) & is.na(llm_date)))

        date_accuracy <- mean(date_comparison$match, na.rm = TRUE)
    } else if (n_matched > 0 && used_name_matching) {
        # For name-based matching, compare dates by matched names
        gt_dates <- gt_main |>
            dplyr::filter(name_normalized %in% matched_cases) |>
            dplyr::select(name_normalized, gt_date = onset_date)

        llm_dates <- llm_main |>
            dplyr::filter(name_normalized %in% matched_cases) |>
            dplyr::select(name_normalized, llm_date = onset_date)

        date_comparison <- dplyr::inner_join(gt_dates, llm_dates, by = "name_normalized") |>
            dplyr::mutate(match = (gt_date == llm_date) | (is.na(gt_date) & is.na(llm_date)))

        date_accuracy <- mean(date_comparison$match, na.rm = TRUE)
    }

    list(
        case_f1 = case_f1,
        case_recall = case_recall,
        case_precision = case_precision,
        date_accuracy = date_accuracy,
        n_gt_cases = n_gt,
        n_llm_cases = n_llm,
        n_matched_cases = n_matched
    )
}

#' Compute Variable-Level Metrics
#'
#' Calculate per-variable accuracy metrics across ALL ground truth cases.
#' This provides detailed field-by-field analysis of extraction quality.
#'
#' IMPORTANT: Metrics are calculated on ALL ground truth cases, not just matched ones.
#' Cases that the LLM did not extract count as false negatives for recall calculation.
#' This ensures that models with poor case detection don't appear artificially good
#' on variable extraction.
#'
#' @param llm_normalized Normalized LLM results (list with main_linelist)
#' @param gt_normalized Normalized ground truth (list with main_linelist)
#' @param few_shot_ids Character vector of case IDs to exclude from metrics
#' @param variables Character vector of variable names to compare (default: key linelist fields)
#' @return Tibble with per-variable metrics: variable, n_compared, n_match, f1, precision, recall
#' @export
compute_variable_metrics <- function(llm_normalized, gt_normalized, few_shot_ids = character(0),
                                     variables = c(
                                         "name", "sex", "age", "age_unit",
                                         "residence_village",
                                         "onset_date", "outcome_date", "outcome",
                                         "potential_infector", "infection_route",
                                         "most_probable_infector", "contacts", "secondary_cases"
                                     )) {
    # Get main linelists directly - no longer using relationship table
    gt_main <- gt_normalized$main_linelist
    llm_main <- llm_normalized$main_linelist

    # Ensure contacts and secondary_cases columns exist (add as NA if missing)
    if (!"contacts" %in% names(gt_main)) gt_main$contacts <- NA_character_
    if (!"secondary_cases" %in% names(gt_main)) gt_main$secondary_cases <- NA_character_
    if (!"contacts" %in% names(llm_main)) llm_main$contacts <- NA_character_
    if (!"secondary_cases" %in% names(llm_main)) llm_main$secondary_cases <- NA_character_

    # Get all GT cases and LLM cases (excluding few-shot)
    gt_cases <- unique(gt_main$case_id)
    llm_cases <- unique(llm_main$case_id)

    if (length(few_shot_ids) > 0) {
        gt_cases <- setdiff(gt_cases, few_shot_ids)
        llm_cases <- setdiff(llm_cases, few_shot_ids)

        # Filter main data to exclude few-shot
        gt_main <- gt_main |> dplyr::filter(!case_id %in% few_shot_ids)
        llm_main <- llm_main |> dplyr::filter(!case_id %in% few_shot_ids)
    }

    matched_cases <- intersect(gt_cases, llm_cases)

    # If no case_id matches, try matching by name as fallback
    # This handles MLX runs that generate generic IDs like "Case_1"
    if (length(matched_cases) == 0 && nrow(gt_main) > 0 && nrow(llm_main) > 0) {
        message("  No case_id matches found, attempting name-based matching...")

        # Normalize names for matching - ORDER INDEPENDENT
        normalize_name <- function(x) {
            if (is.na(x) || x == "" || tolower(x) == "null") {
                return(NA_character_)
            }
            # Lowercase, remove accents, trim whitespace
            x <- tolower(trimws(x))
            x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
            # Split into words, sort alphabetically, rejoin
            words <- strsplit(x, "\\s+")[[1]]
            words <- words[nchar(words) >= 2] # Remove single-letter parts
            if (length(words) == 0) {
                return(NA_character_)
            }
            paste(sort(words), collapse = " ")
        }

        gt_main <- gt_main |> dplyr::mutate(name_normalized = sapply(name, normalize_name))
        llm_main <- llm_main |> dplyr::mutate(name_normalized = sapply(name, normalize_name))

        # Match by normalized name
        gt_names <- unique(gt_main$name_normalized[!is.na(gt_main$name_normalized)])
        llm_names <- unique(llm_main$name_normalized[!is.na(llm_main$name_normalized)])
        matched_names <- intersect(gt_names, llm_names)

        if (length(matched_names) > 0) {
            message(glue::glue("  Found {length(matched_names)} name matches out of {length(gt_names)} GT names"))

            # Create a mapping from name to case_id for both datasets
            # For matched records, we'll use the GT case_id as the canonical one
            gt_name_to_id <- gt_main |>
                dplyr::filter(!is.na(name_normalized)) |>
                dplyr::distinct(name_normalized, .keep_all = TRUE) |>
                dplyr::select(name_normalized, gt_case_id = case_id)

            llm_name_to_id <- llm_main |>
                dplyr::filter(!is.na(name_normalized)) |>
                dplyr::distinct(name_normalized, .keep_all = TRUE) |>
                dplyr::select(name_normalized, llm_case_id = case_id)

            # Update llm_main to use GT case_ids where names match
            llm_main <- llm_main |>
                dplyr::left_join(gt_name_to_id, by = "name_normalized") |>
                dplyr::mutate(case_id = dplyr::if_else(!is.na(gt_case_id), gt_case_id, case_id)) |>
                dplyr::select(-gt_case_id)

            # Recalculate matched cases using the remapped IDs
            llm_cases <- unique(llm_main$case_id)
            matched_cases <- intersect(gt_cases, llm_cases)
            message(glue::glue("  After name-based remapping: {length(matched_cases)} matched cases"))
        }
    }

    unmatched_gt_cases <- setdiff(gt_cases, llm_cases) # GT cases LLM missed
    unmatched_llm_cases <- setdiff(llm_cases, gt_cases) # LLM hallucinations

    n_gt_total <- length(gt_cases)
    n_matched <- length(matched_cases)
    n_unmatched_gt <- length(unmatched_gt_cases)

    if (n_gt_total == 0) {
        return(tibble::tibble(
            variable = variables,
            n_compared = 0L,
            n_match = 0L,
            f1 = NA_real_,
            precision = NA_real_,
            recall = NA_real_,
            completeness_llm = NA_real_,
            completeness_gt = NA_real_
        ))
    }

    # Deduplicate data
    gt_main <- gt_main |>
        dplyr::distinct(case_id, .keep_all = TRUE) |>
        dplyr::arrange(case_id)

    llm_main <- llm_main |>
        dplyr::distinct(case_id, .keep_all = TRUE) |>
        dplyr::arrange(case_id)

    # Get GT data for matched cases
    gt_matched <- gt_main |>
        dplyr::filter(case_id %in% matched_cases)

    # Get GT data for unmatched cases (cases the LLM missed entirely)
    gt_unmatched <- gt_main |>
        dplyr::filter(case_id %in% unmatched_gt_cases)

    llm_matched <- llm_main |>
        dplyr::filter(case_id %in% matched_cases)

    # Create comparison df for matched cases
    comparison_df <- dplyr::inner_join(
        gt_matched |> dplyr::select(case_id, dplyr::any_of(variables)),
        llm_matched |> dplyr::select(case_id, dplyr::any_of(variables)),
        by = "case_id",
        suffix = c("_gt", "_llm")
    )

    # Build GT name-to-case-ID mapping for contact field matching
    # This allows matching when LLM outputs names instead of case IDs
    normalize_name_for_lookup <- function(x) {
        if (is.na(x) || x == "" || tolower(x) == "null") {
            return(NA_character_)
        }
        x <- tolower(trimws(x))
        x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
        words <- strsplit(x, "\\s+")[[1]]
        words <- words[nchar(words) >= 2]
        if (length(words) == 0) {
            return(NA_character_)
        }
        paste(sort(words), collapse = " ")
    }

    gt_name_to_id <- gt_normalized$main_linelist |>
        dplyr::mutate(name_normalized = sapply(name, normalize_name_for_lookup)) |>
        dplyr::filter(!is.na(name_normalized)) |>
        dplyr::distinct(name_normalized, .keep_all = TRUE) |>
        dplyr::select(name_normalized, case_id)

    # Helper for fuzzy value matching
    fuzzy_match <- function(val1, val2, var_name = "") {
        # Handle NA/null equivalence
        is_null_like <- function(x) is.na(x) || x == "" || tolower(as.character(x)) == "null"

        if (is_null_like(val1) && is_null_like(val2)) {
            return(TRUE)
        }
        if (is_null_like(val1) || is_null_like(val2)) {
            return(FALSE)
        }

        # Normalize for comparison
        clean1 <- tolower(trimws(gsub("[^a-zA-Z0-9/ ]", "", as.character(val1))))
        clean2 <- tolower(trimws(gsub("[^a-zA-Z0-9/ ]", "", as.character(val2))))

        # For list-type variables (contacts, secondary_cases), extract case IDs
        if (var_name %in% c("contacts", "secondary_cases", "potential_infector", "most_probable_infector")) {
            # First try extracting generic three-letter case IDs
            ids1 <- stringr::str_extract_all(toupper(as.character(val1)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]
            ids2 <- stringr::str_extract_all(toupper(as.character(val2)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]

            # If val2 (LLM) has no case IDs, try to extract names and convert to IDs
            if (length(ids2) == 0 && !is_null_like(val2)) {
                # Split by comma and try to match each name to GT
                name_parts <- strsplit(as.character(val2), ",")[[1]]
                for (name_part in name_parts) {
                    normalized_name <- normalize_name_for_lookup(name_part)
                    if (!is.na(normalized_name)) {
                        matched_row <- gt_name_to_id |> dplyr::filter(name_normalized == normalized_name)
                        if (nrow(matched_row) > 0) {
                            ids2 <- c(ids2, matched_row$case_id[1])
                        }
                    }
                }
            }

            if (length(ids1) == 0 && length(ids2) == 0) {
                return(TRUE)
            }
            if (length(ids1) == 0 || length(ids2) == 0) {
                return(FALSE)
            }

            # Set equality for contacts/secondary_cases
            return(setequal(ids1, ids2))
        }

        # For names, compare word sets (order-independent) with distance
        if (var_name == "name") {
            clean_name <- function(name) {
                if (is.na(name) || name == "" || name == "null") {
                    return(character(0))
                }
                cleaned <- tolower(trimws(gsub("[^a-zA-Z ]", "", as.character(name))))
                words <- strsplit(gsub(" +", " ", cleaned), " ")[[1]]
                sort(words[nchar(words) >= 2])
            }

            words1 <- clean_name(val1)
            words2 <- clean_name(val2)

            if (length(words1) == 0 && length(words2) == 0) {
                return(TRUE)
            }
            if (length(words1) == 0 || length(words2) == 0) {
                return(FALSE)
            }

            sorted1 <- paste(words1, collapse = " ")
            sorted2 <- paste(words2, collapse = " ")

            return(agrepl(sorted1, sorted2, max.distance = 0.2, ignore.case = TRUE) |
                agrepl(sorted2, sorted1, max.distance = 0.2, ignore.case = TRUE))
        }

        # Direct string match for other variables
        return(clean1 == clean2)
    }

    # Helper to check if value is null-like
    is_null_like <- function(x) is.na(x) || x == "" || tolower(as.character(x)) == "null"

    # Helper to compute per-case metrics for list-type fields (contacts, secondary_cases, etc.)
    # Returns list with metrics AND actual IDs for per-case output
    compute_list_metrics <- function(gt_val, llm_val, gt_name_to_id_df = NULL) {
        # Handle NA/null equivalence
        gt_empty <- is_null_like(gt_val)
        llm_empty <- is_null_like(llm_val)

        # Both empty = perfect match
        if (gt_empty && llm_empty) {
            return(list(
                precision = 1, recall = 1, f1 = 1,
                n_gt = 0L, n_llm = 0L, n_matched = 0L,
                gt_ids = character(0), llm_ids = character(0),
                matched_ids = character(0), missed_ids = character(0), invented_ids = character(0)
            ))
        }

        # Extract case IDs from GT
        gt_ids <- if (!gt_empty) {
            stringr::str_extract_all(toupper(as.character(gt_val)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]
        } else {
            character(0)
        }

        # Extract case IDs from LLM
        llm_ids <- if (!llm_empty) {
            stringr::str_extract_all(toupper(as.character(llm_val)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]
        } else {
            character(0)
        }

        # If LLM has no case IDs but has text, try name-based lookup
        if (length(llm_ids) == 0 && !llm_empty && !is.null(gt_name_to_id_df)) {
            name_parts <- strsplit(as.character(llm_val), ",")[[1]]
            for (name_part in name_parts) {
                normalized_name <- normalize_name_for_lookup(name_part)
                if (!is.na(normalized_name)) {
                    matched_row <- gt_name_to_id_df |> dplyr::filter(name_normalized == normalized_name)
                    if (nrow(matched_row) > 0) {
                        llm_ids <- c(llm_ids, toupper(matched_row$case_id[1]))
                    }
                }
            }
        }

        # Make unique
        gt_ids <- unique(gt_ids)
        llm_ids <- unique(llm_ids)

        n_gt <- length(gt_ids)
        n_llm <- length(llm_ids)
        matched_ids <- intersect(gt_ids, llm_ids)
        n_matched <- length(matched_ids)
        missed_ids <- setdiff(gt_ids, llm_ids) # In GT but not in LLM
        invented_ids <- setdiff(llm_ids, gt_ids) # In LLM but not in GT

        # Calculate metrics
        # GT empty, LLM not empty = all LLM values are false positives
        if (n_gt == 0 && n_llm > 0) {
            return(list(
                precision = 0, recall = 1, f1 = 0,
                n_gt = n_gt, n_llm = n_llm, n_matched = n_matched,
                gt_ids = gt_ids, llm_ids = llm_ids,
                matched_ids = matched_ids, missed_ids = missed_ids, invented_ids = invented_ids
            ))
        }

        # GT not empty, LLM empty = all GT values are false negatives
        if (n_gt > 0 && n_llm == 0) {
            return(list(
                precision = 1, recall = 0, f1 = 0,
                n_gt = n_gt, n_llm = n_llm, n_matched = n_matched,
                gt_ids = gt_ids, llm_ids = llm_ids,
                matched_ids = matched_ids, missed_ids = missed_ids, invented_ids = invented_ids
            ))
        }

        # Both empty after extraction (text existed but no case IDs found) = treat as match
        if (n_gt == 0 && n_llm == 0) {
            return(list(
                precision = 1, recall = 1, f1 = 1,
                n_gt = 0L, n_llm = 0L, n_matched = 0L,
                gt_ids = character(0), llm_ids = character(0),
                matched_ids = character(0), missed_ids = character(0), invented_ids = character(0)
            ))
        }

        precision <- n_matched / n_llm
        recall <- n_matched / n_gt
        f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
            2 * precision * recall / (precision + recall)
        } else {
            0
        }

        list(
            precision = precision, recall = recall, f1 = f1,
            n_gt = n_gt, n_llm = n_llm, n_matched = n_matched,
            gt_ids = gt_ids, llm_ids = llm_ids,
            matched_ids = matched_ids, missed_ids = missed_ids, invented_ids = invented_ids
        )
    }

    # List of variables that should use per-case list metrics
    list_type_vars <- c("secondary_cases", "potential_infector")

    # Compute metrics for each variable
    purrr::map_dfr(variables, function(var) {
        gt_col <- paste0(var, "_gt")
        llm_col <- paste0(var, "_llm")

        # Check if columns exist in comparison_df
        has_comparison_cols <- gt_col %in% names(comparison_df) && llm_col %in% names(comparison_df)

        # Check if variable exists in GT for unmatched cases
        has_gt_var <- var %in% names(gt_unmatched)

        if (!has_comparison_cols && !has_gt_var) {
            return(tibble::tibble(
                variable = var,
                n_compared = 0L,
                n_match = 0L,
                f1 = NA_real_,
                precision = NA_real_,
                recall = NA_real_,
                completeness_llm = NA_real_,
                completeness_gt = NA_real_
            ))
        }

        # ============================================================
        # SPECIAL HANDLING FOR LIST-TYPE VARIABLES (contacts, secondary_cases, etc.)
        # Use per-case precision/recall/F1 instead of binary match
        # ============================================================
        if (var %in% list_type_vars) {
            matched_gt_vals <- if (has_comparison_cols) comparison_df[[gt_col]] else character(0)
            matched_llm_vals <- if (has_comparison_cols) comparison_df[[llm_col]] else character(0)

            # Compute per-case metrics for matched cases
            case_metrics <- if (length(matched_gt_vals) > 0) {
                purrr::map2(matched_gt_vals, matched_llm_vals, ~ compute_list_metrics(.x, .y, gt_name_to_id))
            } else {
                list()
            }

            # Handle unmatched GT cases - they contribute 0 recall
            unmatched_gt_vals <- if (has_gt_var && n_unmatched_gt > 0) gt_unmatched[[var]] else character(0)

            # For unmatched GT cases, LLM gets 0 for recall on any non-empty GT values
            unmatched_metrics <- if (length(unmatched_gt_vals) > 0) {
                purrr::map(unmatched_gt_vals, ~ compute_list_metrics(.x, NA_character_, NULL))
            } else {
                list()
            }

            # Combine all case metrics
            all_case_metrics <- c(case_metrics, unmatched_metrics)

            if (length(all_case_metrics) == 0) {
                return(tibble::tibble(
                    variable = var,
                    n_compared = 0L,
                    n_match = 0L,
                    f1 = NA_real_,
                    precision = NA_real_,
                    recall = NA_real_,
                    completeness_llm = NA_real_,
                    completeness_gt = NA_real_
                ))
            }

            # Extract per-case scores
            precisions <- sapply(all_case_metrics, function(m) m$precision)
            recalls <- sapply(all_case_metrics, function(m) m$recall)
            f1s <- sapply(all_case_metrics, function(m) m$f1)
            n_gt_items <- sapply(all_case_metrics, function(m) m$n_gt)
            n_llm_items <- sapply(all_case_metrics, function(m) m$n_llm)
            n_matched_items <- sapply(all_case_metrics, function(m) m$n_matched)

            # Aggregate: mean across cases (including empty-empty as 1)
            # For comparison count, count cases where at least one side has values
            n_compared <- sum(n_gt_items > 0 | n_llm_items > 0)

            # F1: mean F1 across ALL cases (including empty-empty matches which are correct)
            f1 <- mean(f1s, na.rm = TRUE)

            # Aggregate precision: mean of per-case precisions
            precision <- mean(precisions, na.rm = TRUE)

            # Aggregate recall: mean of per-case recalls
            recall <- mean(recalls, na.rm = TRUE)

            # Completeness
            matched_gt_has_val <- if (length(matched_gt_vals) > 0) !sapply(matched_gt_vals, is_null_like) else logical(0)
            matched_llm_has_val <- if (length(matched_llm_vals) > 0) !sapply(matched_llm_vals, is_null_like) else logical(0)
            unmatched_gt_has_val <- if (length(unmatched_gt_vals) > 0) !sapply(unmatched_gt_vals, is_null_like) else logical(0)

            completeness_llm <- if (n_matched > 0) mean(matched_llm_has_val) else NA_real_
            completeness_gt <- if (n_gt_total > 0) {
                (sum(matched_gt_has_val) + sum(unmatched_gt_has_val)) / n_gt_total
            } else {
                NA_real_
            }

            return(tibble::tibble(
                variable = var,
                n_compared = as.integer(n_compared),
                n_match = as.integer(sum(n_matched_items)),
                f1 = f1,
                precision = precision,
                recall = recall,
                completeness_llm = completeness_llm,
                completeness_gt = completeness_gt
            ))
        }

        # ============================================================
        # STANDARD HANDLING FOR NON-LIST VARIABLES
        # ============================================================
        matched_gt_vals <- if (has_comparison_cols) comparison_df[[gt_col]] else character(0)
        matched_llm_vals <- if (has_comparison_cols) comparison_df[[llm_col]] else character(0)

        matched_gt_has_val <- if (length(matched_gt_vals) > 0) !sapply(matched_gt_vals, is_null_like) else logical(0)
        matched_llm_has_val <- if (length(matched_llm_vals) > 0) !sapply(matched_llm_vals, is_null_like) else logical(0)

        # Matches in matched cases
        matches_in_matched <- if (length(matched_gt_vals) > 0) {
            purrr::map2_lgl(matched_gt_vals, matched_llm_vals, ~ fuzzy_match(.x, .y, var))
        } else {
            logical(0)
        }

        # ============================================================
        # UNMATCHED GT CASES: These are all false negatives
        # ============================================================
        unmatched_gt_vals <- if (has_gt_var && n_unmatched_gt > 0) gt_unmatched[[var]] else character(0)
        unmatched_gt_has_val <- if (length(unmatched_gt_vals) > 0) !sapply(unmatched_gt_vals, is_null_like) else logical(0)

        # ============================================================
        # AGGREGATE METRICS ACROSS ALL GT CASES
        # ============================================================

        # Total GT non-null values (from matched + unmatched cases)
        n_gt_nonnull_matched <- sum(matched_gt_has_val)
        n_gt_nonnull_unmatched <- sum(unmatched_gt_has_val)
        n_gt_nonnull_total <- n_gt_nonnull_matched + n_gt_nonnull_unmatched

        # Total LLM non-null values (only from matched cases - unmatched LLM cases are hallucinations)
        n_llm_nonnull <- sum(matched_llm_has_val)

        # True positives: correct matches where both have values
        both_have <- matched_gt_has_val & matched_llm_has_val
        n_true_positive <- sum(matches_in_matched & both_have)

        # For comparison count - cases where we can compare (both have values)
        n_compared <- sum(both_have)

        # Precision: Of values LLM extracted (non-null), how many are correct?
        # This is unaffected by unmatched GT cases - we only care about what LLM produced
        precision <- if (n_llm_nonnull > 0) n_true_positive / n_llm_nonnull else NA_real_

        # Recall: Of ALL GT non-null values (matched + unmatched), how many did LLM get right?
        # Unmatched GT cases have 0 correct extractions by definition
        # This is the KEY FIX: recall now penalizes missing cases
        recall <- if (n_gt_nonnull_total > 0) n_true_positive / n_gt_nonnull_total else NA_real_

        # F1: Harmonic mean of precision and recall
        f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
            2 * precision * recall / (precision + recall)
        } else {
            NA_real_
        }

        # Completeness: calculated per set
        completeness_llm <- if (n_matched > 0) mean(matched_llm_has_val) else NA_real_
        completeness_gt <- if (n_gt_total > 0) {
            # Include both matched and unmatched GT
            (n_gt_nonnull_matched + n_gt_nonnull_unmatched) / n_gt_total
        } else {
            NA_real_
        }

        tibble::tibble(
            variable = var,
            n_compared = as.integer(n_compared),
            n_match = as.integer(n_true_positive),
            f1 = f1,
            precision = precision,
            recall = recall,
            completeness_llm = completeness_llm,
            completeness_gt = completeness_gt
        )
    })
}

#' Create Metrics Tibble Row
#'
#' Build a standardised tibble row for pipeline metrics
#'
#' @param config List with model configuration
#' @param replicate Integer replicate number
#' @param status Character "Success" or "Failed"
#' @param error Character error message or NA
#' @param attempts Integer number of attempts
#' @param metrics List from compute_case_metrics (optional)
#' @param time_total Numeric total processing time in seconds (optional
#' @return Single-row tibble with all metric columns
#' @export
create_metrics_row <- function(config, replicate, status, error = NA_character_,
                               attempts = 1, metrics = NULL, time_total = NA_real_,
                               slurm_job_id = NA_character_, slurm_execution_time = NA_real_,
                               var_metrics = NULL) {
    if (is.null(metrics)) {
        # Failed run - return NA metrics
        tibble::tibble(
            model = config$model_name_display,
            provider = config$provider,
            strategy = ifelse(isTRUE(config$use_chunking), "Chunked", "Unchunked"),
            replicate = replicate,
            status = status,
            error = error,
            attempts = attempts,
            case_f1 = NA_real_,
            case_recall = NA_real_,
            case_precision = NA_real_,
            secondary_f1 = NA_real_,
            secondary_recall = NA_real_,
            secondary_precision = NA_real_,
            infector_f1 = NA_real_,
            infector_recall = NA_real_,
            infector_precision = NA_real_,
            date_accuracy = NA_real_,
            time_total_sec = NA_real_,
            time_per_case_sec = NA_real_,
            slurm_job_id = NA_character_,
            slurm_exec_time_sec = NA_real_
        )
    } else {
        # Successful run
        time_per_case <- if (!is.na(time_total) && metrics$n_llm_cases > 0) {
            time_total / metrics$n_llm_cases
        } else {
            NA_real_
        }

        # Extract variable-specific metrics if provided
        secondary_metrics <- if (!is.null(var_metrics)) dplyr::filter(var_metrics, variable == "secondary_cases") else NULL
        infector_metrics <- if (!is.null(var_metrics)) dplyr::filter(var_metrics, variable == "potential_infector") else NULL

        tibble::tibble(
            model = config$model_name_display,
            provider = config$provider,
            strategy = ifelse(isTRUE(config$use_chunking), "Chunked", "Unchunked"),
            replicate = replicate,
            status = status,
            error = error,
            attempts = attempts,
            case_f1 = metrics$case_f1,
            case_recall = metrics$case_recall,
            case_precision = metrics$case_precision,
            secondary_f1 = if (!is.null(secondary_metrics) && nrow(secondary_metrics) > 0) secondary_metrics$f1 else NA_real_,
            secondary_recall = if (!is.null(secondary_metrics) && nrow(secondary_metrics) > 0) secondary_metrics$recall else NA_real_,
            secondary_precision = if (!is.null(secondary_metrics) && nrow(secondary_metrics) > 0) secondary_metrics$precision else NA_real_,
            infector_f1 = if (!is.null(infector_metrics) && nrow(infector_metrics) > 0) infector_metrics$f1 else NA_real_,
            infector_recall = if (!is.null(infector_metrics) && nrow(infector_metrics) > 0) infector_metrics$recall else NA_real_,
            infector_precision = if (!is.null(infector_metrics) && nrow(infector_metrics) > 0) infector_metrics$precision else NA_real_,
            date_accuracy = metrics$date_accuracy,
            time_total_sec = time_total,
            time_per_case_sec = time_per_case,
            slurm_job_id = slurm_job_id,
            slurm_exec_time_sec = slurm_execution_time
        )
    }
}

#' Aggregate Replicate Metrics
#'
#' Calculate summary statistics (median, IQR, range) across replicates
#'
#' @param metrics_df Tibble with per-replicate metrics
#' @return Tibble with aggregated metrics per model/provider/strategy
#' @export
aggregate_replicate_metrics <- function(metrics_df) {
    metrics_df |>
        dplyr::filter(status == "Success") |>
        dplyr::group_by(model, provider, strategy) |>
        dplyr::summarise(
            n_replicates = dplyr::n(),
            # Case F1
            case_f1_median = median(case_f1, na.rm = TRUE),
            case_f1_q25 = quantile(case_f1, 0.25, na.rm = TRUE),
            case_f1_q75 = quantile(case_f1, 0.75, na.rm = TRUE),
            case_f1_iqr = IQR(case_f1, na.rm = TRUE),
            case_f1_min = min(case_f1, na.rm = TRUE),
            case_f1_max = max(case_f1, na.rm = TRUE),
            case_f1_sd = sd(case_f1, na.rm = TRUE),
            # Case Recall
            case_recall_median = median(case_recall, na.rm = TRUE),
            case_recall_q25 = quantile(case_recall, 0.25, na.rm = TRUE),
            case_recall_q75 = quantile(case_recall, 0.75, na.rm = TRUE),
            case_recall_iqr = IQR(case_recall, na.rm = TRUE),
            case_recall_min = min(case_recall, na.rm = TRUE),
            case_recall_max = max(case_recall, na.rm = TRUE),
            # Case Precision
            case_precision_median = median(case_precision, na.rm = TRUE),
            case_precision_q25 = quantile(case_precision, 0.25, na.rm = TRUE),
            case_precision_q75 = quantile(case_precision, 0.75, na.rm = TRUE),
            case_precision_iqr = IQR(case_precision, na.rm = TRUE),
            case_precision_min = min(case_precision, na.rm = TRUE),
            case_precision_max = max(case_precision, na.rm = TRUE),

            # Secondary Cases F1
            secondary_f1_median = median(secondary_f1, na.rm = TRUE),
            secondary_f1_q25 = quantile(secondary_f1, 0.25, na.rm = TRUE),
            secondary_f1_q75 = quantile(secondary_f1, 0.75, na.rm = TRUE),
            secondary_f1_min = min(secondary_f1, na.rm = TRUE),
            secondary_f1_max = max(secondary_f1, na.rm = TRUE),
            # Secondary Cases Recall
            secondary_recall_median = median(secondary_recall, na.rm = TRUE),
            secondary_recall_q25 = quantile(secondary_recall, 0.25, na.rm = TRUE),
            secondary_recall_q75 = quantile(secondary_recall, 0.75, na.rm = TRUE),
            secondary_recall_min = min(secondary_recall, na.rm = TRUE),
            secondary_recall_max = max(secondary_recall, na.rm = TRUE),
            # Secondary Cases Precision
            secondary_precision_median = median(secondary_precision, na.rm = TRUE),
            secondary_precision_q25 = quantile(secondary_precision, 0.25, na.rm = TRUE),
            secondary_precision_q75 = quantile(secondary_precision, 0.75, na.rm = TRUE),
            secondary_precision_min = min(secondary_precision, na.rm = TRUE),
            secondary_precision_max = max(secondary_precision, na.rm = TRUE),

            # Infector F1
            infector_f1_median = median(infector_f1, na.rm = TRUE),
            infector_f1_q25 = quantile(infector_f1, 0.25, na.rm = TRUE),
            infector_f1_q75 = quantile(infector_f1, 0.75, na.rm = TRUE),
            infector_f1_min = min(infector_f1, na.rm = TRUE),
            infector_f1_max = max(infector_f1, na.rm = TRUE),
            # Infector Recall
            infector_recall_median = median(infector_recall, na.rm = TRUE),
            infector_recall_q25 = quantile(infector_recall, 0.25, na.rm = TRUE),
            infector_recall_q75 = quantile(infector_recall, 0.75, na.rm = TRUE),
            infector_recall_min = min(infector_recall, na.rm = TRUE),
            infector_recall_max = max(infector_recall, na.rm = TRUE),
            # Infector Precision
            infector_precision_median = median(infector_precision, na.rm = TRUE),
            infector_precision_q25 = quantile(infector_precision, 0.25, na.rm = TRUE),
            infector_precision_q75 = quantile(infector_precision, 0.75, na.rm = TRUE),
            infector_precision_min = min(infector_precision, na.rm = TRUE),
            infector_precision_max = max(infector_precision, na.rm = TRUE),
            # Date Accuracy
            date_accuracy_median = median(date_accuracy, na.rm = TRUE),
            date_accuracy_q25 = quantile(date_accuracy, 0.25, na.rm = TRUE),
            date_accuracy_q75 = quantile(date_accuracy, 0.75, na.rm = TRUE),
            date_accuracy_iqr = IQR(date_accuracy, na.rm = TRUE),
            date_accuracy_min = min(date_accuracy, na.rm = TRUE),
            date_accuracy_max = max(date_accuracy, na.rm = TRUE),
            # Timing
            time_total_median = median(time_total_sec, na.rm = TRUE),
            time_total_min = min(time_total_sec, na.rm = TRUE),
            time_total_max = max(time_total_sec, na.rm = TRUE),
            time_per_case_median = median(time_per_case_sec, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            # Formatted range strings for presentation
            case_f1_range = glue::glue("{round(case_f1_median*100, 1)}% ({round(case_f1_min*100, 1)}-{round(case_f1_max*100, 1)})"),
            case_f1_iqr_str = glue::glue("{round(case_f1_median*100, 1)}% [IQR: {round(case_f1_q25*100, 1)}-{round(case_f1_q75*100, 1)}]"),
            case_recall_range = glue::glue("{round(case_recall_median*100, 1)}% ({round(case_recall_min*100, 1)}-{round(case_recall_max*100, 1)})"),
            case_recall_iqr_str = glue::glue("{round(case_recall_median*100, 1)}% [IQR: {round(case_recall_q25*100, 1)}-{round(case_recall_q75*100, 1)}]"),
            case_precision_range = glue::glue("{round(case_precision_median*100, 1)}% ({round(case_precision_min*100, 1)}-{round(case_precision_max*100, 1)})"),
            case_precision_iqr_str = glue::glue("{round(case_precision_median*100, 1)}% [IQR: {round(case_precision_q25*100, 1)}-{round(case_precision_q75*100, 1)}]"),
            date_accuracy_range = glue::glue("{round(date_accuracy_median*100, 1)}% ({round(date_accuracy_min*100, 1)}-{round(date_accuracy_max*100, 1)})"),
            date_accuracy_iqr_str = glue::glue("{round(date_accuracy_median*100, 1)}% [IQR: {round(date_accuracy_q25*100, 1)}-{round(date_accuracy_q75*100, 1)}]"),

            # Secondary Ranges
            secondary_f1_range = glue::glue("{round(secondary_f1_median*100, 1)}% ({round(secondary_f1_min*100, 1)}-{round(secondary_f1_max*100, 1)})"),
            secondary_recall_range = glue::glue("{round(secondary_recall_median*100, 1)}% ({round(secondary_recall_min*100, 1)}-{round(secondary_recall_max*100, 1)})"),
            secondary_precision_range = glue::glue("{round(secondary_precision_median*100, 1)}% ({round(secondary_precision_min*100, 1)}-{round(secondary_precision_max*100, 1)})"),

            # Infector Ranges
            infector_f1_range = glue::glue("{round(infector_f1_median*100, 1)}% ({round(infector_f1_min*100, 1)}-{round(infector_f1_max*100, 1)})"),
            infector_recall_range = glue::glue("{round(infector_recall_median*100, 1)}% ({round(infector_recall_min*100, 1)}-{round(infector_recall_max*100, 1)})"),
            infector_precision_range = glue::glue("{round(infector_precision_median*100, 1)}% ({round(infector_precision_min*100, 1)}-{round(infector_precision_max*100, 1)})")
        )
}

#' Compute Variable Metrics for All Configs
#'
#' Process saved .qs result files and compute per-variable accuracy metrics
#'
#' @param configs List of model configurations
#' @param output_dir Path to output directory containing results
#' @param gt_normalized Normalized ground truth data
#' @param few_shot_ids Character vector of case IDs to exclude
#' @return List with all_metrics tibble and aggregated_metrics tibble
#' @export
compute_all_variable_metrics <- function(configs, output_dir, gt_normalized,
                                         few_shot_ids = character(0),
                                         few_shot_ids_map = NULL) {
    all_variable_metrics <- tibble::tibble()

    for (config in configs) {
        # Per-config exclusion list takes priority over global list.
        # few_shot_ids_map is a named list: config$name -> character vector of IDs.
        config_few_shot_ids <- if (!is.null(few_shot_ids_map) && !is.null(few_shot_ids_map[[config$name]])) {
            few_shot_ids_map[[config$name]]
        } else {
            few_shot_ids
        }

        # Try new structure (model-specific subfolder) first
        model_output_dir <- file.path(output_dir, config$name)

        if (dir.exists(model_output_dir)) {
            result_files <- list.files(model_output_dir, pattern = "llm_results_rep.*[.]qs$", full.names = TRUE)
        } else {
            # Fall back to old structure (files in root with config name in filename)
            result_files <- list.files(
                output_dir,
                pattern = glue::glue("llm_results_{config$name}_rep.*[.]qs$"),
                full.names = TRUE
            )
        }

        if (length(result_files) == 0) {
            message(glue::glue("  Skipping {config$name}: no result files found"))
            next
        }

        message(glue::glue("  Processing {config$name}: {length(result_files)} files"))

        for (result_file in result_files) {
            rep_num <- as.integer(stringr::str_extract(basename(result_file), "(?<=rep)\\d+"))

            tryCatch(
                {
                    llm_normalized <- qs::qread(result_file)

                    var_metrics <- compute_variable_metrics(
                        llm_normalized = llm_normalized,
                        gt_normalized = gt_normalized,
                        few_shot_ids = config_few_shot_ids
                    ) |>
                        dplyr::mutate(
                            model = config$name,
                            model_display = config$model_name_display,
                            provider = config$provider,
                            replicate = rep_num
                        )

                    all_variable_metrics <- dplyr::bind_rows(all_variable_metrics, var_metrics)
                },
                error = function(e) {
                    message(glue::glue("  Error processing {basename(result_file)}: {e$message}"))
                }
            )
        }
    }

    # Aggregate if we have data
    aggregated_metrics <- tibble::tibble()

    if (nrow(all_variable_metrics) > 0) {
        aggregated_metrics <- all_variable_metrics |>
            dplyr::group_by(model, model_display, provider, variable) |>
            dplyr::summarise(
                n_replicates = dplyr::n(),
                f1_median = median(f1, na.rm = TRUE),
                f1_q25 = quantile(f1, 0.25, na.rm = TRUE),
                f1_q75 = quantile(f1, 0.75, na.rm = TRUE),
                f1_min = min(f1, na.rm = TRUE),
                f1_max = max(f1, na.rm = TRUE),
                precision_median = median(precision, na.rm = TRUE),
                precision_q25 = quantile(precision, 0.25, na.rm = TRUE),
                precision_q75 = quantile(precision, 0.75, na.rm = TRUE),
                precision_min = min(precision, na.rm = TRUE),
                precision_max = max(precision, na.rm = TRUE),
                recall_median = median(recall, na.rm = TRUE),
                recall_q25 = quantile(recall, 0.25, na.rm = TRUE),
                recall_q75 = quantile(recall, 0.75, na.rm = TRUE),
                recall_min = min(recall, na.rm = TRUE),
                recall_max = max(recall, na.rm = TRUE),
                completeness_llm_median = median(completeness_llm, na.rm = TRUE),
                completeness_gt_median = median(completeness_gt, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::mutate(
                f1_iqr_str = glue::glue("{round(f1_median*100, 1)}% [IQR: {round(f1_q25*100, 1)}-{round(f1_q75*100, 1)}]")
            )
    }

    list(
        all_metrics = all_variable_metrics,
        aggregated_metrics = aggregated_metrics
    )
}

#' Compute Per-Case List Metrics
#'
#' For list-type variables (contacts, secondary_cases, potential_infector),
#' compute detailed per-case metrics showing GT IDs, LLM IDs, matched/missed/invented IDs,
#' and per-case precision/recall/F1.
#'
#' @param llm_normalized Normalized LLM results (list with main_linelist)
#' @param gt_normalized Normalized ground truth (list with main_linelist)
#' @param few_shot_ids Character vector of case IDs to exclude
#' @return Tibble with per-case metrics for each list-type variable
#' @export
compute_per_case_list_metrics <- function(llm_normalized, gt_normalized, few_shot_ids = character(0)) {
    list_type_vars <- c("secondary_cases", "potential_infector")

    # Helper to check if value is null-like
    is_null_like <- function(x) is.na(x) || x == "" || tolower(as.character(x)) == "null"

    # Helper to compute per-case metrics for a single cell
    compute_single_cell_metrics <- function(gt_val, llm_val) {
        gt_empty <- is_null_like(gt_val)
        llm_empty <- is_null_like(llm_val)

        # Both empty = perfect match
        if (gt_empty && llm_empty) {
            return(list(
                gt_ids = "", llm_ids = "", matched_ids = "", missed_ids = "", invented_ids = "",
                n_gt = 0L, n_llm = 0L, n_matched = 0L, precision = 1, recall = 1, f1 = 1
            ))
        }

        # Extract case IDs from GT
        gt_ids <- if (!gt_empty) {
            unique(stringr::str_extract_all(toupper(as.character(gt_val)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]])
        } else {
            character(0)
        }

        # Extract case IDs from LLM
        llm_ids <- if (!llm_empty) {
            unique(stringr::str_extract_all(toupper(as.character(llm_val)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]])
        } else {
            character(0)
        }

        n_gt <- length(gt_ids)
        n_llm <- length(llm_ids)
        matched_ids <- intersect(gt_ids, llm_ids)
        n_matched <- length(matched_ids)
        missed_ids <- setdiff(gt_ids, llm_ids)
        invented_ids <- setdiff(llm_ids, gt_ids)

        # Calculate metrics
        if (n_gt == 0 && n_llm > 0) {
            precision <- 0
            recall <- 1
            f1 <- 0
        } else if (n_gt > 0 && n_llm == 0) {
            precision <- 1
            recall <- 0
            f1 <- 0
        } else if (n_gt == 0 && n_llm == 0) {
            precision <- 1
            recall <- 1
            f1 <- 1
        } else {
            precision <- n_matched / n_llm
            recall <- n_matched / n_gt
            f1 <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
        }

        list(
            gt_ids = paste(gt_ids, collapse = ", "),
            llm_ids = paste(llm_ids, collapse = ", "),
            matched_ids = paste(matched_ids, collapse = ", "),
            missed_ids = paste(missed_ids, collapse = ", "),
            invented_ids = paste(invented_ids, collapse = ", "),
            n_gt = n_gt, n_llm = n_llm, n_matched = n_matched,
            precision = precision, recall = recall, f1 = f1
        )
    }

    # Get main linelists
    gt_main <- gt_normalized$main_linelist
    llm_main <- llm_normalized$main_linelist

    # Exclude few-shot cases
    if (length(few_shot_ids) > 0) {
        gt_main <- gt_main |> dplyr::filter(!case_id %in% few_shot_ids)
    }

    # Match GT and LLM by case_id
    matched_cases <- dplyr::inner_join(
        gt_main |> dplyr::select(case_id, dplyr::any_of(list_type_vars)),
        llm_main |> dplyr::select(case_id, dplyr::any_of(list_type_vars)),
        by = "case_id",
        suffix = c("_gt", "_llm")
    )

    # Build per-case metrics table
    results <- purrr::map_dfr(seq_len(nrow(matched_cases)), function(i) {
        case_id <- matched_cases$case_id[i]

        purrr::map_dfr(list_type_vars, function(var) {
            gt_col <- paste0(var, "_gt")
            llm_col <- paste0(var, "_llm")

            gt_val <- if (gt_col %in% names(matched_cases)) matched_cases[[gt_col]][i] else NA_character_
            llm_val <- if (llm_col %in% names(matched_cases)) matched_cases[[llm_col]][i] else NA_character_

            metrics <- compute_single_cell_metrics(gt_val, llm_val)

            tibble::tibble(
                case_id = case_id,
                variable = var,
                gt_ids = metrics$gt_ids,
                llm_ids = metrics$llm_ids,
                matched_ids = metrics$matched_ids,
                missed_ids = metrics$missed_ids,
                invented_ids = metrics$invented_ids,
                n_gt = metrics$n_gt,
                n_llm = metrics$n_llm,
                n_matched = metrics$n_matched,
                precision = metrics$precision,
                recall = metrics$recall,
                f1 = metrics$f1
            )
        })
    })

    results
}
