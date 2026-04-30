#' Fuzzy Value Match
#'
#' Centralized fuzzy string matching for line list comparisons.
#' The function handles standard text equality, order-insensitive name matching,
#' and case-id set comparisons for list-like transmission fields.
#'
#' @param val1 Value to compare.
#' @param val2 Second value to compare.
#' @param column_name The field name used for field-specific matching logic.
#'
#' @return A logical scalar indicating whether the values match.
fuzzy_match <- function(val1, val2, column_name = "") {
    if (is.na(val1) && is.na(val2)) {
        return(TRUE)
    }

    if (is.na(val1) || is.na(val2)) {
        return(FALSE)
    }

    if (val1 == "null" && val2 == "null") {
        return(TRUE)
    }

    if (val1 == "null" || val2 == "null") {
        return(FALSE)
    }

    if (val1 == "" && val2 == "") {
        return(TRUE)
    }

    if (val1 == "" || val2 == "") {
        return(FALSE)
    }

    if (column_name %in% c("contacts", "secondary_cases", "potential_infector", "most_probable_infector")) {
        parse_case_ids <- function(text) {
            case_ids <- stringr::str_extract_all(text, "\\b[A-Z]{3}[0-9]{3}\\b")[[1]]
            unique(toupper(case_ids))
        }

        ids1 <- parse_case_ids(val1)
        ids2 <- parse_case_ids(val2)

        if (length(ids1) == 0 && length(ids2) == 0) {
            return(TRUE)
        }

        if (length(ids1) == 0 || length(ids2) == 0) {
            return(FALSE)
        }

        return(setequal(ids1, ids2))
    }

    if (column_name == "name") {
        clean_name <- function(name) {
            if (is.na(name) || name == "" || name == "null") {
                return(character(0))
            }

            cleaned <- tolower(trimws(gsub("[^a-zA-Z ]", "", as.character(name))))
            words <- strsplit(gsub(" +", " ", cleaned), " ")[[1]]
            sort(words[nchar(words) >= 2])
        }

        words1 <- clean_name(val1)
        words2 <- clean_name(val2)

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

    normalize_text <- function(value) {
        transliterated <- iconv(as.character(value), from = "UTF-8", to = "ASCII//TRANSLIT")
        tolower(trimws(gsub("[-_\\s]+", " ", transliterated)))
    }

    clean1 <- normalize_text(val1)
    clean2 <- normalize_text(val2)

    clean1 == clean2
}
