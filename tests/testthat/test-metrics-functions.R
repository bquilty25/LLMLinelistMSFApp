test_that("compute_case_metrics supports direct case-id matches", {
    gt <- list(main_linelist = data.frame(
        case_id = c("ABC001", "ABC002"),
        name = c("Jane Doe", "John Smith"),
        onset_date = c("2026-01-01", "2026-01-02"),
        stringsAsFactors = FALSE
    ))

    llm <- list(main_linelist = data.frame(
        case_id = c("ABC001", "ABC999"),
        name = c("Jane Doe", "John Smith"),
        onset_date = c("2026-01-01", "2026-01-02"),
        stringsAsFactors = FALSE
    ))

    metrics <- compute_case_metrics(llm, gt)

    expect_equal(metrics$n_matched_cases, 1)
    expect_equal(metrics$n_gt_cases, 2)
    expect_equal(metrics$n_llm_cases, 2)
})

test_that("compute_variable_metrics accounts for unmatched ground truth cases", {
    gt <- list(main_linelist = data.frame(
        case_id = c("ABC001", "ABC002"),
        name = c("Jane Doe", "John Smith"),
        residence_village = c("Village A", "Village B"),
        potential_infector = c(NA, "ABC001"),
        secondary_cases = c("ABC002", NA),
        stringsAsFactors = FALSE
    ))

    llm <- list(main_linelist = data.frame(
        case_id = c("ABC001"),
        name = c("Jane Doe"),
        residence_village = c("Village A"),
        potential_infector = c(NA),
        secondary_cases = c("ABC002"),
        stringsAsFactors = FALSE
    ))

    metrics <- compute_variable_metrics(
        llm,
        gt,
        variables = c("name", "residence_village", "potential_infector", "secondary_cases")
    )

    expect_equal(nrow(metrics), 4)
    expect_true(all(metrics$variable %in% c("name", "residence_village", "potential_infector", "secondary_cases")))
    expect_true(metrics$recall[metrics$variable == "name"] < 1)
})

test_that("create_metrics_row and aggregate_replicate_metrics produce summary tibbles", {
    metrics <- list(
        case_f1 = 0.5,
        case_recall = 0.5,
        case_precision = 0.5,
        date_accuracy = 1,
        n_gt_cases = 2,
        n_llm_cases = 2,
        n_matched_cases = 1
    )

    var_metrics <- data.frame(
        variable = c("secondary_cases", "potential_infector"),
        f1 = c(1, 0.5),
        recall = c(1, 0.5),
        precision = c(1, 0.5),
        stringsAsFactors = FALSE
    )

    row_one <- create_metrics_row(
        config = list(model_name_display = "demo", provider = "local", use_chunking = FALSE),
        replicate = 1,
        status = "Success",
        metrics = metrics,
        time_total = 10,
        var_metrics = var_metrics
    )

    row_two <- create_metrics_row(
        config = list(model_name_display = "demo", provider = "local", use_chunking = FALSE),
        replicate = 2,
        status = "Success",
        metrics = metrics,
        time_total = 12,
        var_metrics = var_metrics
    )

    aggregated <- aggregate_replicate_metrics(dplyr::bind_rows(row_one, row_two))

    expect_equal(nrow(row_one), 1)
    expect_equal(nrow(aggregated), 1)
    expect_equal(aggregated$n_replicates, 2)
})

test_that("compute_per_case_list_metrics returns per-case rows", {
    gt <- list(main_linelist = data.frame(
        case_id = c("ABC001", "ABC002"),
        potential_infector = c(NA, "ABC001"),
        secondary_cases = c("ABC002", NA),
        stringsAsFactors = FALSE
    ))

    llm <- list(main_linelist = data.frame(
        case_id = c("ABC001", "ABC002"),
        potential_infector = c(NA, "ABC001"),
        secondary_cases = c("ABC002", NA),
        stringsAsFactors = FALSE
    ))

    metrics <- compute_per_case_list_metrics(llm, gt)

    expect_equal(nrow(metrics), 4)
    expect_true(all(metrics$f1 == 1))
})
