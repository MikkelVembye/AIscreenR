#Helper functions

# Prices per token for the model(s). Models without a price get NA.
.model_prices <- function(data){
  dplyr::left_join(
    data,
    model_prizes[, c("model", "price_in_per_token", "price_out_per_token")],
    by = "model"
  )
}

# Average number of completion tokens for the inclusion_decision_simple function.
.avg_completion_tokens <- 7.05

# Empirical multipliers used to approximate the reasoning tokens of the gpt models.
# The multiplier is applied to the average number of completion tokens (.avg_completion_tokens).
# Measured as (mean + 1 sd) of the completion tokens (reasoning included) / 7.05 across 50 references with verbosity = "low".
# The upper estimate is used so the approximation tends to overestimate rather than underestimate the price.
# Verbosity is not used since the default (low) is almost always applied.
.reasoning_multipliers <- function(){
  tibble::tribble(
    ~model_group,    ~reasoning_effort, ~multiplier,
    "gpt-5-nano",    "low",             52.12,
    "gpt-5-nano",    "medium",          180.38,
    "gpt-5-nano",    "high",            323.80,
    "gpt-5-mini",    "low",             24.65,
    "gpt-5-mini",    "medium",          48.52,
    "gpt-5-mini",    "high",            117.20,
    "gpt-5.1",       "low",             10.85,
    "gpt-5.1",       "medium",          39.74,
    "gpt-5.1",       "high",            68.12,
    "gpt-5.4-mini",  "low",             9.70,
    "gpt-5.4-mini",  "medium",          12.88,
    "gpt-5.4-mini",  "high",            25.64,
    "gpt-6-luna",    "low",             3.26,
    "gpt-6-luna",    "medium",          5.19,
    "gpt-6-luna",    "high",            18.72,
    "gpt-6-sol",     "low",             3.26,
    "gpt-6-sol",     "medium",          8.40,
    "gpt-6-sol",     "high",            8.10,
    # Average of the measured gpt-6 models. Used for gpt-6 models that have not been measured.
    "gpt-6",         "low",             3.26,
    "gpt-6",         "medium",          6.80,
    "gpt-6",         "high",            13.41
  )
}

# Maps a model to the measured model group. Dated versions (e.g., gpt-5-mini-2025-08-07) use the group of
# the base model. Models that have not been measured use the closest group.
# Non-gpt models (e.g., Claude, Gemini) get NA, i.e., no reasoning multiplier.
.model_group <- function(model){
  base <- sub("-\\d{4}-\\d{2}-\\d{2}$", "", model)
  dplyr::case_when(
    !stringr::str_detect(model, "^gpt-") ~ NA_character_,
    base %in% .reasoning_multipliers()$model_group ~ base,
    stringr::str_detect(base, "nano") ~ "gpt-5-nano",
    stringr::str_detect(base, "mini") ~ "gpt-5.4-mini",
    stringr::str_detect(base, "^gpt-6") ~ "gpt-6",
    TRUE ~ "gpt-5.1"
  )
}

.price_gpt <- function(data){

  if (is_gpt_tbl(data)){

    # Real screening data. Each row is one request with the token counts reported by the API.
    price_dat <- data |> dplyr::mutate(n_requests = 1)

  } else {

    # Approximation data. The prompt tokens are approximated from the question
    # and the completion tokens from the average answer size and the reasoning effort.
    if (!"reasoning_effort" %in% names(data)) data$reasoning_effort <- NA_character_

    price_dat <-
      data |>
      dplyr::mutate(model_group = .model_group(model)) |>
      dplyr::left_join(.reasoning_multipliers(), by = c("model_group", "reasoning_effort")) |>
      dplyr::mutate(
        prompt_tokens = round(stringr::str_count(question, '\\w+') * 1.6),
        completion_tokens = .avg_completion_tokens * dplyr::coalesce(multiplier, 1),
        n_requests = iterations
      )

  }

  price_dat |>
    dplyr::filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
    .model_prices() |>
    dplyr::mutate(
      input_price = round(prompt_tokens * price_in_per_token * n_requests, 4),
      output_price = completion_tokens * price_out_per_token * n_requests
    ) |>
    dplyr::summarise(
      prompt = dplyr::first(promptid),
      completion_tokens = sum(completion_tokens * n_requests, na.rm = TRUE),
      input_price_dollar = sum(input_price, na.rm = TRUE),
      output_price_dollar = sum(output_price, na.rm = TRUE),
      total_price_dollar = round(input_price_dollar + output_price_dollar, 4),
      .by = c(promptid, model, iterations)
    )

}
