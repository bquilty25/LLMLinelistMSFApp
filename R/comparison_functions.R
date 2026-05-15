# =============================================================================
# COMPARISON FUNCTIONS
# Functions for comparing LLM output vs ground truth
# =============================================================================

#' Standardize Column Names
#'
#' Map different column naming conventions to standard names.
#'
#' @param df Data frame to standardize.
#'
#' @return A data frame with standardized column names.
standardize_columns <- function(df) {
    column_mapping <- c(
        "case_id" = "case_id",
        "Case_ID" = "case_id",
        "id" = "case_id",
        "patient_id" = "case_id",
        "name" = "name",
        "Name" = "name",
        "patient_name" = "name",
        "sex" = "sex",
        "Sex" = "sex",
        "gender" = "sex",
        "age" = "age",
        "Age" = "age",
        "age_unit" = "age_unit",
        "age_units" = "age_unit",
        "province" = "province",
        "Province" = "province",
        "health_zone" = "health_zone",
        "health_area" = "health_area",
        "residence_village" = "residence_village",
        "village" = "residence_village",
        "profession" = "profession",
        "occupation" = "profession",
        "onset_date" = "onset_date",
        "date_onset" = "onset_date",
        "symptom_onset" = "onset_date",
        "outcome_date" = "outcome_date",
        "outcome" = "outcome",
        "status" = "outcome",
        "classification" = "classification",
        "potential_infector" = "potential_infector",
        "infector" = "potential_infector",
        "source_case" = "potential_infector",
        "infection_route" = "infection_route",
        "route" = "infection_route",
        "most_probable_infector" = "most_probable_infector",
        "secondary_cases" = "secondary_cases",
        "contacts" = "contacts"
    )

    names(df) <- vapply(names(df), function(x) {
        mapped_name <- column_mapping[tolower(x)]
        if (is.na(mapped_name)) x else mapped_name
    }, character(1))

    df
}

#' Compare Linelists
#'
#' Calculate detailed comparison metrics between LLM output and ground truth.
#'
#' @param llm_data LLM-generated line list.
#' @param ground_truth Ground truth line list.
#'
#' @return A list containing structure diagnostics, case detection metrics,
#'   column-level metrics, and overall summary metrics.
#' @export
compare_linelists <- function(llm_data, ground_truth) {
    cat("Comparing linelists...\n")

    llm_std <- standardize_columns(llm_data)
    gt_std <- standardize_columns(ground_truth)

    shared_cols <- intersect(names(llm_std), names(gt_std))
    llm_only_cols <- setdiff(names(llm_std), names(gt_std))
    gt_only_cols <- setdiff(names(gt_std), names(llm_std))

    cat("Shared columns:", length(shared_cols), "\n")
    cat("LLM-only columns:", length(llm_only_cols), "\n")
    cat("Ground truth-only columns:", length(gt_only_cols), "\n")

    case_id_performance <- analyze_case_detection(llm_std, gt_std)
    match_results <- case_id_performance$match_results

    column_metrics <- purrr::map_dfr(shared_cols, function(column_name) {
        col_comparison <- compare_column_values(llm_std, gt_std, column_name, match_results)

        tibble::tibble(
            column = column_name,
            precision = col_comparison$precision,
            recall = col_comparison$recall,
            f1_score = col_comparison$f1_score,
            accuracy = col_comparison$accuracy,
            completeness_llm = col_comparison$completeness_llm,
            completeness_gt = col_comparison$completeness_gt
        )
    })

    overall_metrics <- calculate_overall_metrics(column_metrics)

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
#' Find matching cases between LLM and ground truth data using name similarity.
#'
#' @param llm_data LLM data with a name column.
#' @param gt_data Ground truth data with a name column.
#'
#' @return A list with matched cases and unmatched indices.
match_cases_by_name <- function(llm_data, gt_data) {
    clean_name <- function(name) {
        if (is.na(name) || name == "" || name == "null") {
            return(character(0))
        }

        cleaned <- tolower(trimws(gsub("[^a-zA-Z ]", "", as.character(name))))
        words <- strsplit(gsub(" +", " ", cleaned), " ")[[1]]
        sort(words[nchar(words) >= 3])
    }

    name_similarity <- function(name1_words, name2_words) {
        if (length(name1_words) == 0 || length(name2_words) == 0) {
            return(0)
        }

        intersection <- length(intersect(name1_words, name2_words))
        union <- length(union(name1_words, name2_words))

        if (union == 0) {
            return(0)
        }

        intersection / union
    }

    llm_names_tokens <- purrr::map(llm_data$name, clean_name)
    gt_names_tokens <- purrr::map(gt_data$name, clean_name)

    name_matches <- tibble::tibble(
        llm_idx = integer(),
        gt_idx = integer(),
        case_name = character(),
        similarity = numeric()
    )

    gt_used <- rep(FALSE, nrow(gt_data))
    threshold <- 0.5

    for (i in seq_along(llm_names_tokens)) {
        if (length(llm_names_tokens[[i]]) > 0) {
            similarities <- purrr::map_dbl(seq_along(gt_names_tokens), function(j) {
                if (gt_used[j]) {
                    return(0)
                }

                name_similarity(llm_names_tokens[[i]], gt_names_tokens[[j]])
            })

            best_match <- which.max(similarities)

            if (length(best_match) > 0 && similarities[best_match] >= threshold) {
                name_matches <- dplyr::bind_rows(
                    name_matches,
                    tibble::tibble(
                        llm_idx = i,
                        gt_idx = best_match,
                        case_name = llm_data$name[i],
                        similarity = similarities[best_match]
                    )
                )
                gt_used[best_match] <- TRUE
            }
        }
    }

    unmatched_llm <- setdiff(seq_len(nrow(llm_data)), name_matches$llm_idx)
    unmatched_gt <- setdiff(seq_len(nrow(gt_data)), name_matches$gt_idx)

    cat("Name-based matching found:", nrow(name_matches), "matched cases\n")
    cat("LLM extra cases:", length(unmatched_llm), "\n")
    cat("GT missing cases:", length(unmatched_gt), "\n")

    list(
        matches = name_matches,
        unmatched_llm = unmatched_llm,
        unmatched_gt = unmatched_gt,
        threshold_used = threshold
    )
}

#' Analyze Case Detection Performance
#'
#' Compare case identification between LLM and ground truth using name matching.
#'
#' @param llm_data Standardized LLM data.
#' @param gt_data Standardized ground truth data.
#'
#' @return A list with case detection metrics and error analysis.
analyze_case_detection <- function(llm_data, gt_data) {
    match_results <- match_cases_by_name(llm_data, gt_data)

    true_positives <- nrow(match_results$matches)
    false_positives <- length(match_results$unmatched_llm)
    false_negatives <- length(match_results$unmatched_gt)

    fp_analysis <- tibble::tibble(
        llm_idx = match_results$unmatched_llm,
        name = llm_data$name[match_results$unmatched_llm],
        category = "Unknown"
    )

    if (nrow(fp_analysis) > 0) {
        fp_analysis <- fp_analysis |>
            dplyr::rowwise() |>
            dplyr::mutate(
                category = dplyr::case_when(
                    is.na(name) | name == "null" | name == "" ~ "Empty/Null Name",
                    nchar(name) < 3 ~ "Noise/Fragment",
                    TRUE ~ "Hallucination / Extra Extraction"
                )
            ) |>
            dplyr::ungroup()
    }

    fn_analysis <- tibble::tibble(
        gt_idx = match_results$unmatched_gt,
        name = gt_data$name[match_results$unmatched_gt],
        category = "Unknown"
    )

    if (nrow(fn_analysis) > 0) {
        fn_analysis <- fn_analysis |>
            dplyr::rowwise() |>
            dplyr::mutate(
                category = dplyr::case_when(
                    is.na(name) | name == "null" | name == "" ~ "Empty GT Name",
                    TRUE ~ "Missed Case"
                )
            ) |>
            dplyr::ungroup()
    }

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
        match_results = match_results,
        error_analysis = list(
            false_positives = fp_analysis,
            false_negatives = fn_analysis
        )
    )
}

#' Compare Column Values Using Name-Based Matching
#'
#' Compare values for a specific column between datasets using matched cases.
#'
#' @param llm_data LLM data.
#' @param gt_data Ground truth data.
#' @param column Column name to compare.
#' @param match_results Results from match_cases_by_name.
#'
#' @return A list with column comparison metrics and error breakdown.
compare_column_values <- function(llm_data, gt_data, column, match_results = NULL) {
    if (!column %in% names(llm_data) || !column %in% names(gt_data)) {
        return(list(
            precision = NA,
            recall = NA,
            f1_score = NA,
            accuracy = NA,
            completeness_llm = NA,
            completeness_gt = NA,
            error_breakdown = NULL
        ))
    }

    llm_vals <- llm_data[[column]]
    gt_vals <- gt_data[[column]]

    llm_has_value_all <- !vapply(llm_vals, llmlinelist_is_null_like, logical(1))
    gt_has_value_all <- !vapply(gt_vals, llmlinelist_is_null_like, logical(1))

    completeness_llm <- sum(llm_has_value_all) / length(llm_vals)
    completeness_gt <- sum(gt_has_value_all) / length(gt_vals)

    error_breakdown <- NULL

    if (is.null(match_results)) {
        llm_unique <- unique(llm_vals[llm_has_value_all])
        gt_unique <- unique(gt_vals[gt_has_value_all])

        if (length(llm_unique) == 0 && length(gt_unique) == 0) {
            precision <- 1.0
            recall <- 1.0
            f1_score <- 1.0
            accuracy <- 1.0
        } else if (length(llm_unique) == 0 || length(gt_unique) == 0) {
            precision <- 0.0
            recall <- 0.0
            f1_score <- 0.0
            accuracy <- 0.0
        } else {
            overlap <- length(intersect(llm_unique, gt_unique))
            precision <- overlap / length(llm_unique)
            recall <- overlap / length(gt_unique)
            f1_score <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
            accuracy <- overlap / length(union(llm_unique, gt_unique))
        }
    } else {
        matches <- match_results$matches

        if (nrow(matches) == 0) {
            precision <- 0.0
            recall <- 0.0
            f1_score <- 0.0
            accuracy <- 0.0
        } else {
            comparison_df <- matches |>
                dplyr::mutate(
                    llm_val = llm_vals[llm_idx],
                    gt_val = gt_vals[gt_idx],
                    llm_has_value = !vapply(llm_val, llmlinelist_is_null_like, logical(1)),
                    gt_has_value = !vapply(gt_val, llmlinelist_is_null_like, logical(1)),
                    both_have_values = llm_has_value & gt_has_value
                ) |>
                dplyr::rowwise() |>
                dplyr::mutate(
                    values_match = dplyr::if_else(
                        !llm_has_value & !gt_has_value,
                        TRUE,
                        dplyr::if_else(both_have_values, fuzzy_match(llm_val, gt_val, column), FALSE)
                    ),
                    error_type = dplyr::case_when(
                        values_match ~ "None",
                        !llm_has_value & gt_has_value ~ "False Negative (Missing Value)",
                        llm_has_value & !gt_has_value ~ "False Positive (Hallucinated Value)",
                        llm_has_value & gt_has_value & !values_match ~ "Content Mismatch",
                        TRUE ~ "Other"
                    )
                ) |>
                dplyr::ungroup()

            true_positives <- sum(comparison_df$values_match & comparison_df$both_have_values)
            llm_non_null_matched <- sum(comparison_df$llm_has_value)
            gt_non_null_matched <- sum(comparison_df$gt_has_value)
            both_have_values <- sum(comparison_df$both_have_values)

            error_breakdown <- comparison_df |>
                dplyr::filter(error_type != "None") |>
                dplyr::count(error_type) |>
                dplyr::mutate(column = column)

            if (llm_non_null_matched == 0 && gt_non_null_matched == 0) {
                precision <- 1.0
                recall <- 1.0
                f1_score <- 1.0
                accuracy <- 1.0
            } else {
                precision <- if (llm_non_null_matched > 0) true_positives / llm_non_null_matched else 0
                recall <- if (gt_non_null_matched > 0) true_positives / gt_non_null_matched else 0
                f1_score <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
                accuracy <- if (both_have_values > 0) true_positives / both_have_values else 0
            }
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
#' Aggregate column-level metrics to overall performance.
#'
#' @param column_metrics Tibble of column-level metrics.
#'
#' @return A list with overall metrics.
calculate_overall_metrics <- function(column_metrics) {
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
