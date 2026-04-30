suppressPackageStartupMessages({
    library(shiny)
})

if (!requireNamespace("LLMLineListMSFApp", quietly = TRUE)) {
    if (requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
    } else {
        stop(
            paste(
                "LLMLineListMSFApp is not installed.",
                "Install the package or install pkgload to launch this compatibility entrypoint from the repository root."
            ),
            call. = FALSE
        )
    }
}

repo_app_dir <- file.path(getwd(), "inst", "apps", "shiny_app")
installed_app_dir <- system.file("apps", "shiny_app", package = "LLMLineListMSFApp")
app_dir <- if (dir.exists(repo_app_dir)) repo_app_dir else installed_app_dir

if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Packaged Shiny app directory not found.", call. = FALSE)
}

shiny::shinyAppDir(app_dir)
