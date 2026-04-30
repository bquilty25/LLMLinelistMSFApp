test_that("extract_structured_data parses tagged JSON", {
    response <- '<linelist>[{"case_id":"abc001","name":"Jane Doe","contacts":["ABC002"]}]</linelist>'

    parsed <- extract_structured_data(response)

    expect_equal(nrow(parsed$linelist), 1)
    expect_equal(parsed$linelist$case_id[[1]], "ABC001")
    expect_equal(parsed$linelist$contacts[[1]], "ABC002")
})

test_that("generate_extraction_prompt writes an app-compatible prompt file", {
    output_path <- tempfile(fileext = ".txt")

    generate_extraction_prompt(
        document_path = llmlinelist_demo_path(),
        output_path = output_path,
        provider = "ollama",
        force_regeneration = TRUE
    )

    expect_true(file.exists(output_path))
    expect_match(readLines(output_path, n = 1, warn = FALSE), "expert epidemiologist")
})

test_that("consolidate_duplicates_llm merges heuristic duplicates", {
    linelist <- data.frame(
        case_id = c("ABC001", "ABC001", NA),
        name = c("Jane Doe", "Jane Doe", "Jane Doe"),
        onset_date = c("2026-01-01", "2026-01-01", "2026-01-01"),
        stringsAsFactors = FALSE
    )

    deduped <- consolidate_duplicates_llm(linelist)

    expect_equal(nrow(deduped), 1)
    expect_equal(deduped$case_id[[1]], "ABC001")
})

test_that("packaged shiny app is installed and loadable", {
    skip_if_not_installed("shiny")

    app_dir <- system.file("apps", "shiny_app", package = "LLMLineListMSFApp")
    expect_true(nzchar(app_dir))
    expect_true(file.exists(file.path(app_dir, "app.R")))

    app_obj <- suppressWarnings(shiny::shinyAppDir(app_dir))
    expect_s3_class(app_obj, "shiny.appobj")
})
