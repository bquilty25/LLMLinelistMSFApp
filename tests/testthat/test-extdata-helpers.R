test_that("packaged demo and prompt assets resolve", {
    demo_path <- llmlinelist_demo_path()
    prompt_path <- llmlinelist_system_prompt_path()

    expect_true(file.exists(demo_path))
    expect_true(file.exists(prompt_path))
    expect_match(readLines(demo_path, n = 1, warn = FALSE), "Outbreak narrative")
    expect_match(readLines(prompt_path, n = 1, warn = FALSE), "expert epidemiologist")
})
