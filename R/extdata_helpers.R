#' Get Installed Demo File Path
#'
#' Resolve the path to a bundled demo asset installed with the package.
#'
#' @param name File name under `inst/extdata/demos`.
#'
#' @return An absolute file path.
#' @export
llmlinelist_demo_path <- function(name = "synthetic_msf_demo_84_cases_butembo_villages_narrative.md") {
    path <- system.file("extdata", "demos", name, package = "LLMLineListMSFApp")

    if (!nzchar(path)) {
        stop(sprintf("Demo asset not found: %s", name), call. = FALSE)
    }

    path
}

#' Get Installed System Prompt Path
#'
#' Resolve the path to a bundled system prompt installed with the package.
#'
#' @param name File name under `inst/extdata/system_prompts`.
#'
#' @return An absolute file path.
#' @export
llmlinelist_system_prompt_path <- function(name = "system_prompt_ebola_narrative.txt") {
    path <- system.file("extdata", "system_prompts", name, package = "LLMLineListMSFApp")

    if (!nzchar(path)) {
        stop(sprintf("System prompt asset not found: %s", name), call. = FALSE)
    }

    path
}
