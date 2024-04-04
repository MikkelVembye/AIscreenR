
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

# Function to extract updated models and price information from OpenAI's https://openai.com/pricing

extract_price_table <- function(selector){

  url <- "https://openai.com/pricing"
  webpage <- rvest::read_html(url)

  table_list <- webpage |> rvest::html_nodes(selector) |> rvest::html_table()

  table <- table_list[[1]]
  colnames(table) <- table[1,]
  table <- table[-1,]

  token_per_dollars_in <- dplyr::if_else(stringr::str_detect(table$Input, "M tok"), 1e6, NA_real_)
  token_per_dollars_out  <- dplyr::if_else(stringr::str_detect(table$Output, "M tok"), 1e6, NA_real_)

  price_per_token_in <- as.numeric(stringr::str_extract(table$Input, "\\d+\\.\\d+")) / token_per_dollars_in
  price_per_token_out <- as.numeric(stringr::str_extract(table$Output, "\\d+\\.\\d+")) / token_per_dollars_out

  tibble::tibble(model = table$Model, input = price_per_token_in, output = price_per_token_out)

}

price_table <- function(){

  selector1 <- "#gpt-4-turbo > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)"
  selector2 <- "#gpt-4 > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)"
  selector3 <- "#gpt-3-5-turbo > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)"
  selector4 <- "#older-models > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)"

  selectors <- c(selector1, selector2, selector3, selector4)

  price_tab <- purrr::map(selectors, ~ extract_price_table(.x)) |> purrr::list_rbind()
  price_tab

}


price_input <- function(x){

  models_and_prices_040424 |> dplyr::filter(model == x) |> pull(input)

}

price_output <- function(x){

  models_and_prices_040424 |> dplyr::filter(model == x) |> pull(output)

}

# Defining available models
models_func <- function() {
  models_and_prices_040424$model
}

current_models <- models_func()

