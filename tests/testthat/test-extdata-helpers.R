test_that("packaged demo and prompt assets resolve", {
    demo_path <- llmlinelist_demo_path()
    linelist_path <- llmlinelist_demo_path(
        "synthetic_msf_demo_84_cases_butembo_villages_linelist.csv"
    )
    prompt_path <- llmlinelist_system_prompt_path()

    expect_true(file.exists(demo_path))
    expect_true(file.exists(linelist_path))
    expect_true(file.exists(prompt_path))
    expect_identical(
        basename(demo_path),
        "synthetic_msf_demo_84_cases_butembo_villages_narrative.md"
    )
    expect_identical(
        basename(linelist_path),
        "synthetic_msf_demo_84_cases_butembo_villages_linelist.csv"
    )
    expect_match(readLines(demo_path, n = 1, warn = FALSE), "Outbreak narrative")
    expect_match(readLines(linelist_path, n = 1, warn = FALSE), "^case_id,")
    expect_match(readLines(prompt_path, n = 1, warn = FALSE), "expert epidemiologist")
})
