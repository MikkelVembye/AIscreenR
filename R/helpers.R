
#' @importFrom utils globalVariables

# Manage dplyr behavior

utils::globalVariables(
  c("answer", "run_time", "n", "question", "question_raw", "res", "n_error",
    "still_error", "prompt_tokens", "completion_tokens", "model",
    "requests_per_minute", "tokens_per_minute",
    "input_price_dollar", "output_price_dollar", "price_total_dollar",
    "decision_gpt", "decision_binary", "detailed_description",
    "final_decision_gpt_num", "n_words_answer", "longest_answer", "final_decision_gpt")
)
