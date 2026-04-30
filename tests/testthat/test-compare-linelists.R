test_that("compare_linelists standardizes names and matches cases by name", {
    llm <- data.frame(
        case_id = c("ABC001", "ABC002"),
        name = c("Jane Doe", "John Smith"),
        residence_village = c("Village A", "Village B"),
        stringsAsFactors = FALSE
    )

    ground_truth <- data.frame(
        Case_ID = c("GT001", "GT002"),
        patient_name = c("Doe Jane", "John Smith"),
        village = c("Village A", "Village C"),
        stringsAsFactors = FALSE
    )

    result <- compare_linelists(llm, ground_truth)

    expect_equal(result$structure$n_shared, 3)
    expect_equal(result$case_detection$true_positives, 2)
    expect_equal(result$case_detection$false_positives, 0)
    expect_equal(result$case_detection$false_negatives, 0)
})

test_that("compare_linelists reports unmatched extracted cases", {
    llm <- data.frame(
        case_id = c("ABC001", "ABC002"),
        name = c("Jane Doe", "Extra Person"),
        stringsAsFactors = FALSE
    )

    ground_truth <- data.frame(
        case_id = c("GT001"),
        name = c("Doe Jane"),
        stringsAsFactors = FALSE
    )

    result <- compare_linelists(llm, ground_truth)

    expect_equal(result$case_detection$true_positives, 1)
    expect_equal(result$case_detection$false_positives, 1)
    expect_equal(result$case_detection$false_negatives, 0)
})
