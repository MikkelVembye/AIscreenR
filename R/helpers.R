
#' @importFrom utils globalVariables

# Manage dplyr behavior

utils::globalVariables(
  c("answer", "run_time", "n", "question", "question_raw", "res", "n_error",
    "still_error", "prompt_tokens", "completion_tokens", "model",
    "requests_per_minute", "tokens_per_minute",
    "input_price_dollar", "output_price_dollar", "price_total_dollar",
    "decision_gpt", "decision_binary", "detailed_description",
    "final_decision_gpt_num", "n_words_answer", "longest_answer", "final_decision_gpt",
    "req_per_min", "topp", "iterations", "input_price", "output_price", "max_reps",
    "promptid", "SE_kappa", "accuracy", "cm1", "cm2", "human_code", "human_in_gpt_ex", "human_ex_gpt_in", "n_ref",
    "n_refs", "n_screened", "human_ex_gpt_ex", "human_in_gpt_in", "p_agreement", "pe", "reps", "rm1", "rm2",
    "studyid", "top_p", "n_mis_answers", "screen_errros", "max_tries", "max_seconds", "irr", "se_irr", "cl_irr", "cu_irr",
    "level_of_agreement", "precision", "recall", "npv", "specificity", "bacc", "F2", "mcc", "denominator", "nominator")
)
