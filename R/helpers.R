
#' @importFrom utils globalVariables

# Manage dplyr behavior

utils::globalVariables(
  c("answer", "run_time", "n", "question", "question_raw", "res", "n_error", "still_error",
    "tokens", "model", "requests_per_minute", "tokens_per_minute",
    "decision_gpt", "decision_binary", "detailed_description")
)
