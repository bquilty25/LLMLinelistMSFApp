#' Run the Packaged Shiny App
#'
#' Launch the installed line list extraction Shiny application.
#'
#' @param display.mode Shiny display mode.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns the result of [shiny::runApp()].
#' @export
run_llm_linelist_app <- function(display.mode = c("normal", "showcase"), ...) {
    if (!requireNamespace("shiny", quietly = TRUE)) {
        rlang::abort("Running the app requires the shiny package.")
    }

    display.mode <- match.arg(display.mode)
    app_dir <- system.file("apps", "shiny_app", package = "LLMLineListMSFApp")

    if (!nzchar(app_dir)) {
        rlang::abort("Packaged Shiny app directory not found.")
    }

    shiny::runApp(appDir = app_dir, display.mode = display.mode, ...)
}
