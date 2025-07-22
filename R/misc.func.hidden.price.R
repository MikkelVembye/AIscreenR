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

# Calculate price for full text screening
.price_ftscreen <- function(file_paths, prompts, models, reps, decision_description) {

  # Estimate completion tokens based on the decision description flag
  # These are averages from observing function call token counts.
  completion_tokens_estimate <- if (decision_description) 110 else 15

  # Create a data frame of all combinations to be processed
  price_data <-
    expand.grid(
      file_path = file_paths,
      prompt = prompts,
      model = models,
      reps = 1:reps,
      stringsAsFactors = FALSE
    )

  # Calculate tokens and price for each combination
  price_data <-
    price_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # Read file content and count words
      file_content = paste(readLines(file_path, warn = FALSE), collapse = "\n"),
      # Estimate prompt tokens: words in file + words in prompt, with a multiplier
      prompt_tokens = round((stringr::str_count(file_content, '\\w+') + stringr::str_count(prompt, '\\w+')) * 1.6),
      completion_tokens = completion_tokens_estimate
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-file_content) |> # Remove file content column
    dplyr::rowwise() |>
    dplyr::mutate(
      # Calculate input and output price for each run
      input_price = prompt_tokens * .input_price(model),
      output_price = completion_tokens * .output_price(model)
    ) |>
    dplyr::ungroup()

  # Summarize the total cost
  total_price <-
    price_data |>
    dplyr::summarise(
      total_price_dollar = sum(input_price, na.rm = TRUE) + sum(output_price, na.rm = TRUE)
    ) |>
    dplyr::pull(total_price_dollar)

  return(round(total_price, 4))
}