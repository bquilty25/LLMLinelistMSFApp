# =============================================================================
# METRICS FUNCTIONS
# Functions for computing and aggregating validation metrics
# =============================================================================

llmlinelist_is_null_like <- function(x) {
    normalized <- trimws(tolower(as.character(x)))

    is.na(x) || normalized %in% c("", "null", "[]")
}

llmlinelist_normalize_name <- function(x, min_chars = 2) {
    if (is.na(x) || x == "" || tolower(x) == "null") {
        return(NA_character_)
    }

    x <- tolower(trimws(x))
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    words <- strsplit(x, "\\s+")[[1]]
    words <- words[nchar(words) >= min_chars]

    if (length(words) == 0) {
        return(NA_character_)
    }

    paste(sort(words), collapse = " ")
}

llmlinelist_compute_list_metrics <- function(gt_val, llm_val, gt_name_to_id_df = NULL) {
    gt_empty <- llmlinelist_is_null_like(gt_val)
    llm_empty <- llmlinelist_is_null_like(llm_val)

    if (gt_empty && llm_empty) {
        return(list(
            precision = 1,
            recall = 1,
            f1 = 1,
            n_gt = 0L,
            n_llm = 0L,
            n_matched = 0L,
            gt_ids = character(0),
            llm_ids = character(0),
            matched_ids = character(0),
            missed_ids = character(0),
            invented_ids = character(0)
        ))
    }

    gt_ids <- if (!gt_empty) {
        stringr::str_extract_all(toupper(as.character(gt_val)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]
    } else {
        character(0)
    }

    llm_ids <- if (!llm_empty) {
        stringr::str_extract_all(toupper(as.character(llm_val)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]
    } else {
        character(0)
    }

    if (length(llm_ids) == 0 && !llm_empty && !is.null(gt_name_to_id_df)) {
        name_parts <- strsplit(as.character(llm_val), ",")[[1]]

        for (name_part in name_parts) {
            normalized_name <- llmlinelist_normalize_name(name_part)

            if (!is.na(normalized_name)) {
                matched_row <- gt_name_to_id_df |>
                    dplyr::filter(name_normalized == normalized_name)

                if (nrow(matched_row) > 0) {
                    llm_ids <- c(llm_ids, toupper(matched_row$case_id[1]))
                }
            }
        }
    }

    gt_ids <- unique(gt_ids)
    llm_ids <- unique(llm_ids)

    n_gt <- length(gt_ids)
    n_llm <- length(llm_ids)
    matched_ids <- intersect(gt_ids, llm_ids)
    n_matched <- length(matched_ids)
    missed_ids <- setdiff(gt_ids, llm_ids)
    invented_ids <- setdiff(llm_ids, gt_ids)

    if (n_gt == 0 && n_llm > 0) {
        return(list(
            precision = 0,
            recall = 1,
            f1 = 0,
            n_gt = n_gt,
            n_llm = n_llm,
            n_matched = n_matched,
            gt_ids = gt_ids,
            llm_ids = llm_ids,
            matched_ids = matched_ids,
            missed_ids = missed_ids,
            invented_ids = invented_ids
        ))
    }

    if (n_gt > 0 && n_llm == 0) {
        return(list(
            precision = 1,
            recall = 0,
            f1 = 0,
            n_gt = n_gt,
            n_llm = n_llm,
            n_matched = n_matched,
            gt_ids = gt_ids,
            llm_ids = llm_ids,
            matched_ids = matched_ids,
            missed_ids = missed_ids,
            invented_ids = invented_ids
        ))
    }

    if (n_gt == 0 && n_llm == 0) {
        return(list(
            precision = 1,
            recall = 1,
            f1 = 1,
            n_gt = 0L,
            n_llm = 0L,
            n_matched = 0L,
            gt_ids = character(0),
            llm_ids = character(0),
            matched_ids = character(0),
            missed_ids = character(0),
            invented_ids = character(0)
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
        precision = precision,
        recall = recall,
        f1 = f1,
        n_gt = n_gt,
        n_llm = n_llm,
        n_matched = n_matched,
        gt_ids = gt_ids,
        llm_ids = llm_ids,
        matched_ids = matched_ids,
        missed_ids = missed_ids,
        invented_ids = invented_ids
    )
}

#' Compute Case-Level Metrics
#'
#' Calculate precision, recall, F1, and date accuracy for matched cases.
#'
#' @param llm_normalized Normalized LLM results with `main_linelist`.
#' @param gt_normalized Normalized ground truth with `main_linelist`.
#' @param few_shot_ids Character vector of case IDs to exclude from metrics.
#'
#' @return A named list containing case-level summary metrics.
#' @export
compute_case_metrics <- function(llm_normalized, gt_normalized, few_shot_ids = character(0)) {
    gt_main <- gt_normalized$main_linelist
    llm_main <- llm_normalized$main_linelist

    gt_cases <- unique(gt_main$case_id)
    llm_cases <- unique(llm_main$case_id)

    if (length(few_shot_ids) > 0) {
        gt_cases <- setdiff(gt_cases, few_shot_ids)
        llm_cases <- setdiff(llm_cases, few_shot_ids)
        gt_main <- gt_main |> dplyr::filter(!case_id %in% few_shot_ids)
        llm_main <- llm_main |> dplyr::filter(!case_id %in% few_shot_ids)
    }

    matched_cases <- intersect(gt_cases, llm_cases)
    used_name_matching <- FALSE

    if (length(matched_cases) == 0 && nrow(gt_main) > 0 && nrow(llm_main) > 0) {
        gt_main <- gt_main |>
            dplyr::mutate(name_normalized = vapply(name, llmlinelist_normalize_name, character(1)))
        llm_main <- llm_main |>
            dplyr::mutate(name_normalized = vapply(name, llmlinelist_normalize_name, character(1)))

        gt_names <- unique(gt_main$name_normalized[!is.na(gt_main$name_normalized)])
        llm_names <- unique(llm_main$name_normalized[!is.na(llm_main$name_normalized)])
        matched_names <- intersect(gt_names, llm_names)

        if (length(matched_names) > 0) {
            used_name_matching <- TRUE
            n_gt <- length(gt_names)
            n_llm <- length(llm_names)
            n_matched <- length(matched_names)
            matched_cases <- matched_names
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
#' Calculate per-variable accuracy metrics across all ground truth cases.
#'
#' @param llm_normalized Normalized LLM results with `main_linelist`.
#' @param gt_normalized Normalized ground truth with `main_linelist`.
#' @param few_shot_ids Character vector of case IDs to exclude from metrics.
#' @param variables Character vector of variables to compare.
#'
#' @return A tibble of variable-level metrics.
#' @export
compute_variable_metrics <- function(llm_normalized, gt_normalized, few_shot_ids = character(0),
                                     variables = c(
                                         "name", "sex", "age", "age_unit",
                                         "residence_village",
                                         "onset_date", "outcome_date", "outcome",
                                         "potential_infector", "infection_route",
                                         "most_probable_infector", "contacts", "secondary_cases"
                                     )) {
    gt_main <- gt_normalized$main_linelist
    llm_main <- llm_normalized$main_linelist

    if (!"contacts" %in% names(gt_main)) gt_main$contacts <- NA_character_
    if (!"secondary_cases" %in% names(gt_main)) gt_main$secondary_cases <- NA_character_
    if (!"contacts" %in% names(llm_main)) llm_main$contacts <- NA_character_
    if (!"secondary_cases" %in% names(llm_main)) llm_main$secondary_cases <- NA_character_

    gt_cases <- unique(gt_main$case_id)
    llm_cases <- unique(llm_main$case_id)

    if (length(few_shot_ids) > 0) {
        gt_cases <- setdiff(gt_cases, few_shot_ids)
        llm_cases <- setdiff(llm_cases, few_shot_ids)
        gt_main <- gt_main |> dplyr::filter(!case_id %in% few_shot_ids)
        llm_main <- llm_main |> dplyr::filter(!case_id %in% few_shot_ids)
    }

    matched_cases <- intersect(gt_cases, llm_cases)

    if (length(matched_cases) == 0 && nrow(gt_main) > 0 && nrow(llm_main) > 0) {
        message("  No case_id matches found, attempting name-based matching...")

        gt_main <- gt_main |>
            dplyr::mutate(name_normalized = vapply(name, llmlinelist_normalize_name, character(1)))
        llm_main <- llm_main |>
            dplyr::mutate(name_normalized = vapply(name, llmlinelist_normalize_name, character(1)))

        gt_names <- unique(gt_main$name_normalized[!is.na(gt_main$name_normalized)])
        llm_names <- unique(llm_main$name_normalized[!is.na(llm_main$name_normalized)])
        matched_names <- intersect(gt_names, llm_names)

        if (length(matched_names) > 0) {
            message(glue::glue("  Found {length(matched_names)} name matches out of {length(gt_names)} GT names"))

            gt_name_to_id <- gt_main |>
                dplyr::filter(!is.na(name_normalized)) |>
                dplyr::distinct(name_normalized, .keep_all = TRUE) |>
                dplyr::select(name_normalized, gt_case_id = case_id)

            llm_main <- llm_main |>
                dplyr::left_join(gt_name_to_id, by = "name_normalized") |>
                dplyr::mutate(case_id = dplyr::if_else(!is.na(gt_case_id), gt_case_id, case_id)) |>
                dplyr::select(-gt_case_id)

            llm_cases <- unique(llm_main$case_id)
            matched_cases <- intersect(gt_cases, llm_cases)
            message(glue::glue("  After name-based remapping: {length(matched_cases)} matched cases"))
        }
    }

    unmatched_gt_cases <- setdiff(gt_cases, llm_cases)
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

    gt_main <- gt_main |>
        dplyr::distinct(case_id, .keep_all = TRUE) |>
        dplyr::arrange(case_id)

    llm_main <- llm_main |>
        dplyr::distinct(case_id, .keep_all = TRUE) |>
        dplyr::arrange(case_id)

    gt_matched <- gt_main |>
        dplyr::filter(case_id %in% matched_cases)
    gt_unmatched <- gt_main |>
        dplyr::filter(case_id %in% unmatched_gt_cases)
    llm_matched <- llm_main |>
        dplyr::filter(case_id %in% matched_cases)

    comparison_df <- dplyr::inner_join(
        gt_matched |> dplyr::select(case_id, dplyr::any_of(variables)),
        llm_matched |> dplyr::select(case_id, dplyr::any_of(variables)),
        by = "case_id",
        suffix = c("_gt", "_llm")
    )

    gt_name_to_id <- gt_normalized$main_linelist |>
        dplyr::mutate(name_normalized = vapply(name, llmlinelist_normalize_name, character(1))) |>
        dplyr::filter(!is.na(name_normalized)) |>
        dplyr::distinct(name_normalized, .keep_all = TRUE) |>
        dplyr::select(name_normalized, case_id)

    fuzzy_metric_match <- function(val1, val2, var_name = "") {
        if (llmlinelist_is_null_like(val1) && llmlinelist_is_null_like(val2)) {
            return(TRUE)
        }

        if (llmlinelist_is_null_like(val1) || llmlinelist_is_null_like(val2)) {
            return(FALSE)
        }

        clean1 <- tolower(trimws(gsub("[^a-zA-Z0-9/ ]", "", as.character(val1))))
        clean2 <- tolower(trimws(gsub("[^a-zA-Z0-9/ ]", "", as.character(val2))))

        if (var_name %in% c("contacts", "secondary_cases", "potential_infector", "most_probable_infector")) {
            ids1 <- stringr::str_extract_all(toupper(as.character(val1)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]
            ids2 <- stringr::str_extract_all(toupper(as.character(val2)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]

            if (length(ids2) == 0 && !llmlinelist_is_null_like(val2)) {
                name_parts <- strsplit(as.character(val2), ",")[[1]]

                for (name_part in name_parts) {
                    normalized_name <- llmlinelist_normalize_name(name_part)

                    if (!is.na(normalized_name)) {
                        matched_row <- gt_name_to_id |>
                            dplyr::filter(name_normalized == normalized_name)

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

            return(setequal(ids1, ids2))
        }

        if (var_name == "name") {
            words1 <- strsplit(gsub(" +", " ", tolower(trimws(gsub("[^a-zA-Z ]", "", as.character(val1))))), " ")[[1]]
            words2 <- strsplit(gsub(" +", " ", tolower(trimws(gsub("[^a-zA-Z ]", "", as.character(val2))))), " ")[[1]]
            words1 <- sort(words1[nchar(words1) >= 2])
            words2 <- sort(words2[nchar(words2) >= 2])

            if (length(words1) == 0 && length(words2) == 0) {
                return(TRUE)
            }

            if (length(words1) == 0 || length(words2) == 0) {
                return(FALSE)
            }

            sorted1 <- paste(words1, collapse = " ")
            sorted2 <- paste(words2, collapse = " ")

            return(
                agrepl(sorted1, sorted2, max.distance = 0.2, ignore.case = TRUE) |
                    agrepl(sorted2, sorted1, max.distance = 0.2, ignore.case = TRUE)
            )
        }

        clean1 == clean2
    }

    list_type_vars <- c("secondary_cases", "potential_infector")

    purrr::map_dfr(variables, function(var) {
        gt_col <- paste0(var, "_gt")
        llm_col <- paste0(var, "_llm")

        has_comparison_cols <- gt_col %in% names(comparison_df) && llm_col %in% names(comparison_df)
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

        if (var %in% list_type_vars) {
            matched_gt_vals <- if (has_comparison_cols) comparison_df[[gt_col]] else character(0)
            matched_llm_vals <- if (has_comparison_cols) comparison_df[[llm_col]] else character(0)

            case_metrics <- if (length(matched_gt_vals) > 0) {
                purrr::map2(matched_gt_vals, matched_llm_vals, ~ llmlinelist_compute_list_metrics(.x, .y, gt_name_to_id))
            } else {
                list()
            }

            unmatched_gt_vals <- if (has_gt_var && n_unmatched_gt > 0) gt_unmatched[[var]] else character(0)
            unmatched_metrics <- if (length(unmatched_gt_vals) > 0) {
                purrr::map(unmatched_gt_vals, ~ llmlinelist_compute_list_metrics(.x, NA_character_, NULL))
            } else {
                list()
            }

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

            precisions <- vapply(all_case_metrics, function(metric) metric$precision, numeric(1))
            recalls <- vapply(all_case_metrics, function(metric) metric$recall, numeric(1))
            f1s <- vapply(all_case_metrics, function(metric) metric$f1, numeric(1))
            n_gt_items <- vapply(all_case_metrics, function(metric) metric$n_gt, integer(1))
            n_llm_items <- vapply(all_case_metrics, function(metric) metric$n_llm, integer(1))
            n_matched_items <- vapply(all_case_metrics, function(metric) metric$n_matched, integer(1))

            n_compared <- sum(n_gt_items > 0 | n_llm_items > 0)
            f1 <- mean(f1s, na.rm = TRUE)
            precision <- mean(precisions, na.rm = TRUE)
            recall <- mean(recalls, na.rm = TRUE)

            matched_gt_has_val <- if (length(matched_gt_vals) > 0) !vapply(matched_gt_vals, llmlinelist_is_null_like, logical(1)) else logical(0)
            matched_llm_has_val <- if (length(matched_llm_vals) > 0) !vapply(matched_llm_vals, llmlinelist_is_null_like, logical(1)) else logical(0)
            unmatched_gt_has_val <- if (length(unmatched_gt_vals) > 0) !vapply(unmatched_gt_vals, llmlinelist_is_null_like, logical(1)) else logical(0)

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

        matched_gt_vals <- if (has_comparison_cols) comparison_df[[gt_col]] else character(0)
        matched_llm_vals <- if (has_comparison_cols) comparison_df[[llm_col]] else character(0)

        matched_gt_has_val <- if (length(matched_gt_vals) > 0) !vapply(matched_gt_vals, llmlinelist_is_null_like, logical(1)) else logical(0)
        matched_llm_has_val <- if (length(matched_llm_vals) > 0) !vapply(matched_llm_vals, llmlinelist_is_null_like, logical(1)) else logical(0)

        matches_in_matched <- if (length(matched_gt_vals) > 0) {
            purrr::map2_lgl(matched_gt_vals, matched_llm_vals, ~ fuzzy_metric_match(.x, .y, var))
        } else {
            logical(0)
        }

        unmatched_gt_vals <- if (has_gt_var && n_unmatched_gt > 0) gt_unmatched[[var]] else character(0)
        unmatched_gt_has_val <- if (length(unmatched_gt_vals) > 0) !vapply(unmatched_gt_vals, llmlinelist_is_null_like, logical(1)) else logical(0)

        n_gt_nonnull_matched <- sum(matched_gt_has_val)
        n_gt_nonnull_unmatched <- sum(unmatched_gt_has_val)
        n_gt_nonnull_total <- n_gt_nonnull_matched + n_gt_nonnull_unmatched
        n_llm_nonnull <- sum(matched_llm_has_val)
        both_have <- matched_gt_has_val & matched_llm_has_val
        n_true_positive <- sum(matches_in_matched & both_have)
        n_compared <- sum(both_have)

        precision <- if (n_llm_nonnull > 0) n_true_positive / n_llm_nonnull else NA_real_
        recall <- if (n_gt_nonnull_total > 0) n_true_positive / n_gt_nonnull_total else NA_real_
        f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
            2 * precision * recall / (precision + recall)
        } else {
            NA_real_
        }

        completeness_llm <- if (n_matched > 0) mean(matched_llm_has_val) else NA_real_
        completeness_gt <- if (n_gt_total > 0) {
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
#' Build a standardised tibble row for pipeline metrics.
#'
#' @param config List with model configuration.
#' @param replicate Integer replicate number.
#' @param status Character success state.
#' @param error Character error message or `NA`.
#' @param attempts Integer number of attempts.
#' @param metrics List from [compute_case_metrics()] or `NULL`.
#' @param time_total Numeric total processing time in seconds.
#' @param slurm_job_id Optional cluster job id.
#' @param slurm_execution_time Optional cluster execution time in seconds.
#' @param var_metrics Optional variable metrics tibble.
#'
#' @return A single-row tibble.
#' @export
create_metrics_row <- function(config, replicate, status, error = NA_character_,
                               attempts = 1, metrics = NULL, time_total = NA_real_,
                               slurm_job_id = NA_character_, slurm_execution_time = NA_real_,
                               var_metrics = NULL) {
    if (is.null(metrics)) {
        return(tibble::tibble(
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
        ))
    }

    time_per_case <- if (!is.na(time_total) && metrics$n_llm_cases > 0) {
        time_total / metrics$n_llm_cases
    } else {
        NA_real_
    }

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

#' Aggregate Replicate Metrics
#'
#' Calculate summary statistics across replicate runs.
#'
#' @param metrics_df Tibble with per-replicate metrics.
#'
#' @return A tibble with aggregated metrics by model, provider, and strategy.
#' @export
aggregate_replicate_metrics <- function(metrics_df) {
    metrics_df |>
        dplyr::filter(status == "Success") |>
        dplyr::group_by(model, provider, strategy) |>
        dplyr::summarise(
            n_replicates = dplyr::n(),
            case_f1_median = median(case_f1, na.rm = TRUE),
            case_f1_q25 = quantile(case_f1, 0.25, na.rm = TRUE),
            case_f1_q75 = quantile(case_f1, 0.75, na.rm = TRUE),
            case_f1_iqr = IQR(case_f1, na.rm = TRUE),
            case_f1_min = min(case_f1, na.rm = TRUE),
            case_f1_max = max(case_f1, na.rm = TRUE),
            case_f1_sd = sd(case_f1, na.rm = TRUE),
            case_recall_median = median(case_recall, na.rm = TRUE),
            case_recall_q25 = quantile(case_recall, 0.25, na.rm = TRUE),
            case_recall_q75 = quantile(case_recall, 0.75, na.rm = TRUE),
            case_recall_iqr = IQR(case_recall, na.rm = TRUE),
            case_recall_min = min(case_recall, na.rm = TRUE),
            case_recall_max = max(case_recall, na.rm = TRUE),
            case_precision_median = median(case_precision, na.rm = TRUE),
            case_precision_q25 = quantile(case_precision, 0.25, na.rm = TRUE),
            case_precision_q75 = quantile(case_precision, 0.75, na.rm = TRUE),
            case_precision_iqr = IQR(case_precision, na.rm = TRUE),
            case_precision_min = min(case_precision, na.rm = TRUE),
            case_precision_max = max(case_precision, na.rm = TRUE),
            secondary_f1_median = median(secondary_f1, na.rm = TRUE),
            secondary_f1_q25 = quantile(secondary_f1, 0.25, na.rm = TRUE),
            secondary_f1_q75 = quantile(secondary_f1, 0.75, na.rm = TRUE),
            secondary_f1_min = min(secondary_f1, na.rm = TRUE),
            secondary_f1_max = max(secondary_f1, na.rm = TRUE),
            secondary_recall_median = median(secondary_recall, na.rm = TRUE),
            secondary_recall_q25 = quantile(secondary_recall, 0.25, na.rm = TRUE),
            secondary_recall_q75 = quantile(secondary_recall, 0.75, na.rm = TRUE),
            secondary_recall_min = min(secondary_recall, na.rm = TRUE),
            secondary_recall_max = max(secondary_recall, na.rm = TRUE),
            secondary_precision_median = median(secondary_precision, na.rm = TRUE),
            secondary_precision_q25 = quantile(secondary_precision, 0.25, na.rm = TRUE),
            secondary_precision_q75 = quantile(secondary_precision, 0.75, na.rm = TRUE),
            secondary_precision_min = min(secondary_precision, na.rm = TRUE),
            secondary_precision_max = max(secondary_precision, na.rm = TRUE),
            infector_f1_median = median(infector_f1, na.rm = TRUE),
            infector_f1_q25 = quantile(infector_f1, 0.25, na.rm = TRUE),
            infector_f1_q75 = quantile(infector_f1, 0.75, na.rm = TRUE),
            infector_f1_min = min(infector_f1, na.rm = TRUE),
            infector_f1_max = max(infector_f1, na.rm = TRUE),
            infector_recall_median = median(infector_recall, na.rm = TRUE),
            infector_recall_q25 = quantile(infector_recall, 0.25, na.rm = TRUE),
            infector_recall_q75 = quantile(infector_recall, 0.75, na.rm = TRUE),
            infector_recall_min = min(infector_recall, na.rm = TRUE),
            infector_recall_max = max(infector_recall, na.rm = TRUE),
            infector_precision_median = median(infector_precision, na.rm = TRUE),
            infector_precision_q25 = quantile(infector_precision, 0.25, na.rm = TRUE),
            infector_precision_q75 = quantile(infector_precision, 0.75, na.rm = TRUE),
            infector_precision_min = min(infector_precision, na.rm = TRUE),
            infector_precision_max = max(infector_precision, na.rm = TRUE),
            date_accuracy_median = median(date_accuracy, na.rm = TRUE),
            date_accuracy_q25 = quantile(date_accuracy, 0.25, na.rm = TRUE),
            date_accuracy_q75 = quantile(date_accuracy, 0.75, na.rm = TRUE),
            date_accuracy_iqr = IQR(date_accuracy, na.rm = TRUE),
            date_accuracy_min = min(date_accuracy, na.rm = TRUE),
            date_accuracy_max = max(date_accuracy, na.rm = TRUE),
            time_total_median = median(time_total_sec, na.rm = TRUE),
            time_total_min = min(time_total_sec, na.rm = TRUE),
            time_total_max = max(time_total_sec, na.rm = TRUE),
            time_per_case_median = median(time_per_case_sec, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            case_f1_range = glue::glue("{round(case_f1_median*100, 1)}% ({round(case_f1_min*100, 1)}-{round(case_f1_max*100, 1)})"),
            case_f1_iqr_str = glue::glue("{round(case_f1_median*100, 1)}% [IQR: {round(case_f1_q25*100, 1)}-{round(case_f1_q75*100, 1)}]"),
            case_recall_range = glue::glue("{round(case_recall_median*100, 1)}% ({round(case_recall_min*100, 1)}-{round(case_recall_max*100, 1)})"),
            case_recall_iqr_str = glue::glue("{round(case_recall_median*100, 1)}% [IQR: {round(case_recall_q25*100, 1)}-{round(case_recall_q75*100, 1)}]"),
            case_precision_range = glue::glue("{round(case_precision_median*100, 1)}% ({round(case_precision_min*100, 1)}-{round(case_precision_max*100, 1)})"),
            case_precision_iqr_str = glue::glue("{round(case_precision_median*100, 1)}% [IQR: {round(case_precision_q25*100, 1)}-{round(case_precision_q75*100, 1)}]"),
            date_accuracy_range = glue::glue("{round(date_accuracy_median*100, 1)}% ({round(date_accuracy_min*100, 1)}-{round(date_accuracy_max*100, 1)})"),
            date_accuracy_iqr_str = glue::glue("{round(date_accuracy_median*100, 1)}% [IQR: {round(date_accuracy_q25*100, 1)}-{round(date_accuracy_q75*100, 1)}]"),
            secondary_f1_range = glue::glue("{round(secondary_f1_median*100, 1)}% ({round(secondary_f1_min*100, 1)}-{round(secondary_f1_max*100, 1)})"),
            secondary_recall_range = glue::glue("{round(secondary_recall_median*100, 1)}% ({round(secondary_recall_min*100, 1)}-{round(secondary_recall_max*100, 1)})"),
            secondary_precision_range = glue::glue("{round(secondary_precision_median*100, 1)}% ({round(secondary_precision_min*100, 1)}-{round(secondary_precision_max*100, 1)})"),
            infector_f1_range = glue::glue("{round(infector_f1_median*100, 1)}% ({round(infector_f1_min*100, 1)}-{round(infector_f1_max*100, 1)})"),
            infector_recall_range = glue::glue("{round(infector_recall_median*100, 1)}% ({round(infector_recall_min*100, 1)}-{round(infector_recall_max*100, 1)})"),
            infector_precision_range = glue::glue("{round(infector_precision_median*100, 1)}% ({round(infector_precision_min*100, 1)}-{round(infector_precision_max*100, 1)})")
        )
}

#' Compute Per-Case List Metrics
#'
#' Compute per-case metrics for list-type variables.
#'
#' @param llm_normalized Normalized LLM results with `main_linelist`.
#' @param gt_normalized Normalized ground truth with `main_linelist`.
#' @param few_shot_ids Character vector of case IDs to exclude.
#'
#' @return A tibble with per-case list metrics.
#' @export
compute_per_case_list_metrics <- function(llm_normalized, gt_normalized, few_shot_ids = character(0)) {
    list_type_vars <- c("secondary_cases", "potential_infector")

    compute_single_cell_metrics <- function(gt_val, llm_val) {
        gt_empty <- llmlinelist_is_null_like(gt_val)
        llm_empty <- llmlinelist_is_null_like(llm_val)

        if (gt_empty && llm_empty) {
            return(list(
                gt_ids = "",
                llm_ids = "",
                matched_ids = "",
                missed_ids = "",
                invented_ids = "",
                n_gt = 0L,
                n_llm = 0L,
                n_matched = 0L,
                precision = 1,
                recall = 1,
                f1 = 1
            ))
        }

        gt_ids <- if (!gt_empty) {
            unique(stringr::str_extract_all(toupper(as.character(gt_val)), "\\b[A-Z]{3}[0-9]{3}\\b")[[1]])
        } else {
            character(0)
        }

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
            n_gt = n_gt,
            n_llm = n_llm,
            n_matched = n_matched,
            precision = precision,
            recall = recall,
            f1 = f1
        )
    }

    gt_main <- gt_normalized$main_linelist
    llm_main <- llm_normalized$main_linelist

    if (length(few_shot_ids) > 0) {
        gt_main <- gt_main |> dplyr::filter(!case_id %in% few_shot_ids)
    }

    matched_cases <- dplyr::inner_join(
        gt_main |> dplyr::select(case_id, dplyr::any_of(list_type_vars)),
        llm_main |> dplyr::select(case_id, dplyr::any_of(list_type_vars)),
        by = "case_id",
        suffix = c("_gt", "_llm")
    )

    purrr::map_dfr(seq_len(nrow(matched_cases)), function(i) {
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
}
