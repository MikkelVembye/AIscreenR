#Helper function


.input_price <-
  function(x){

    model_prizes |> dplyr::filter(model == x) |> dplyr::pull(price_in_per_token)

  }

.output_price <-
  function(x){

    model_prizes |> dplyr::filter(model == x) |> dplyr::pull(price_out_per_token)

  }

.price_gpt <- function(data){

  if (!is_gpt_tbl(data)){

    dat <-
      data |>
      dplyr::mutate(
        prompt_tokens = round(stringr::str_count(question, '\\w+') * 1.6),
        completion_tokens = 7.05 # Average number of completion tokens for the inclusion_decision_simple function
      ) |>
      dplyr::filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
      dplyr::rowwise() |>
      dplyr::mutate(

        input_price = round(prompt_tokens * .input_price(model) * iterations, 4),
        output_price = completion_tokens * .output_price(model) * iterations

      ) |>
      dplyr::ungroup() |>
      dplyr::summarise(

        prompt = unique(promptid),
        iterations = unique(iterations),
        input_price_dollar = sum(input_price, na.rm = TRUE),
        output_price_dollar = sum(output_price, na.rm = TRUE),
        total_price_dollar = round(input_price_dollar + output_price_dollar, 4),

        .by = c(prompt, model, iterations)

      )

  } else {

    dat <-
      data |>
      dplyr::filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
      dplyr::rowwise() |>
      dplyr::mutate(

        input_price = round(prompt_tokens * .input_price(model), 4),
        output_price = completion_tokens * .output_price(model)

      ) |>
      dplyr::ungroup() |>
      dplyr::summarise(

        prompt = unique(promptid),
        iterations = unique(iterations),
        input_price_dollar = sum(input_price, na.rm = TRUE),
        output_price_dollar = sum(output_price, na.rm = TRUE),
        total_price_dollar = round(input_price_dollar + output_price_dollar, 4),

        .by = c(prompt, model, iterations)

      )

  }

  dat

}


