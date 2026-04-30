#' Package Globals
#'
#' @name llmlinelist-globals
#' @keywords internal
#' @importFrom stats IQR median quantile sd
NULL

utils::globalVariables(c(
    ".data",
    "case_f1",
    "case_id",
    "case_precision",
    "case_recall",
    "date_accuracy",
    "dedupe_key",
    "dedupe_name",
    "error_type",
    "gt_case_id",
    "gt_date",
    "gt_has_value",
    "gt_idx",
    "gt_val",
    "infector_f1",
    "infector_precision",
    "infector_recall",
    "llm_date",
    "llm_has_value",
    "llm_idx",
    "llm_val",
    "model",
    "name",
    "name_normalized",
    "onset_date",
    "provider",
    "secondary_f1",
    "secondary_precision",
    "secondary_recall",
    "status",
    "strategy",
    "time_per_case_sec",
    "time_total_sec",
    "variable"
))
