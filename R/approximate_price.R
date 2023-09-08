
#' @title Approximate price estimation for title and abstract screening using ChatGPT models
#'
#' @description
#' `r lifecycle::badge("experimental")`<br>
#' <br>
#' This function supports the approximation of the price of the total title and abstract
#' screening with ChatGPT. The function only provide approximately accurate pricing
#' estimates for screenings drawing on the function calling function `incl_function_simple` (Default)
#' since the detailed descriptions will increase the completion tokens with an unknown account
#' which further more is quite model sensitive. gpt-4 models will yields more extensive
#' detailed responses relative to gpt-3.5-turbo models.
#'
#'
#' @template common-arg
#' @param model Character string with the name of the completion model. Can take
#'   multiple models, including gpt-4 models. Default = `"gpt-3.5-turbo-0613"`.
#'   Find available model at
#'   \url{https://platform.openai.com/docs/models/model-endpoint-compatibility}.
#' @param reps Numerical value indicating the number of times the same
#'   question should be sent to ChatGPT. This can be useful to test consistency
#'   between answers. Default is `1` but when using 3.5 models, we recommend setting this
#'   value to `10`.
#' @param token_word_ratio The multiplier used to approximate the number of tokens per word.
#'   Default is `1.6` which we empirically have found to be the average number of tokens per word.
#'
#' @return An object of class \code{"gpt_price"}. The object is a list containing the following
#' components:
#' \item{price}{numerical value indicating the total approximate price (in USD) of the screening across all gpt-models expected to be used for the screening.}
#' \item{price_data}{dataset with prices across all gpt models expected to be used for screening.}
#'
#' @importFrom stats df
#' @import dplyr
#' @export
#'
#' @examples
#' prompt <- "This is a prompt"
#'
#' app_price <- approximate_price_gpt(
#'   data = FFT_dat[1:2,],
#'   prompt = prompt,
#'   studyid = studyid,
#'   title = title,
#'   abstract = abstract,
#'   model = c("gpt-3.5-turbo-0613", "gpt-4"),
#'   reps = 10
#' )
#'
#' app_price
#' app_price$price_dollar
#' app_price$price_data


approximate_price_gpt <-
  function(
    data,
    prompt,
    studyid,
    title,
    abstract,
    model = "gpt-3.5-turbo-0613",
    reps = 1,
    token_word_ratio = 1.6
  ){

    ###############################################
    # Data manipulation
    ###############################################


    if (missing(studyid)){

      dat <-
        data |>
        dplyr::mutate(
          studyid = 1:nrow(data)
        ) |>
        dplyr::relocate(studyid, .before = {{ title }})


    } else {

      dat <-
        data |>
        dplyr::mutate(
          studyid = {{ studyid }}
        ) |>
        dplyr::relocate(studyid, .before = {{ title }})

    }

    question_dat <-
      dat |>
      dplyr::mutate(
        dplyr::across(c({{ title }}, {{ abstract }}), ~ dplyr::if_else(
          is.na(.x) | .x == "" | .x == " ", "No information", .x, missing = "No information")
        )
      ) |>
      dplyr::slice(rep(1:nrow(dat), length(prompt))) |>
      dplyr::mutate(
        prompt = rep(prompt, each = dplyr::n_distinct(studyid))
      ) |>
      dplyr::slice(rep(1:dplyr::n(), each = length(model))) |>
      dplyr::mutate(
        model = rep(model, dplyr::n_distinct(studyid)*dplyr::n_distinct(prompt)),
        question_raw = paste0(
          prompt,
          " Now, evaluate the following title and abstract for",
          " Study ", studyid, ":",
          " -Title: ", {{ title }},
          " -Abstract: ", {{ abstract }}
        ),
        question = stringr::str_replace_all(question_raw, "\n\n", " "),
        question = stringr::str_remove_all(question, "\n"),

        prompt_tokens = round(stringr::str_count(question, '\\w+') * token_word_ratio),
        completion_tokens = 11 # Average number of completion tokens for the inclusion_decision_simple function

      ) |>
      dplyr::select(-question_raw)


    price_dat <-
      question_dat |>
      filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
      summarise(

        input_price_dollar = case_when(
          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.0015/1000), 4),
          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.003/1000), 4),
          any(c("gpt-4", "gpt-4-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.03/1000), 4),
          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.06/1000), 4),
          TRUE ~ NA_real_
        ),


        output_price_dollar = case_when(
          any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.002/1000),
          any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.004/1000),
          any(c("gpt-4", "gpt-4-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.06/1000),
          any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.12/1000),
          TRUE ~ NA_real_
        ),

        input_price_dollar = input_price_dollar * reps,
        output_price_dollar = output_price_dollar * reps,
        price_total_dollar = input_price_dollar + output_price_dollar,

        .by = model

      )


    price <- sum(price_dat$price_total_dollar, na.rm = TRUE)

    res <- list(price_data = price_dat, price_dollar = price)
    class(res) <- c("list", "gpt_price")

    res

}
